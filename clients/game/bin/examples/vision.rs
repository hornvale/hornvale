//! DIAGNOSTIC SKETCH — reference-vision chart viewer.
//!
//! This example renders a committed `vessel/session/v2` snapshot's spatial
//! band to stdout as ANSI truecolor glyphs under a switchable colour
//! mapping (`--vision natural|rainbow|gray`). It exists so a human can LOOK
//! at a chart under mappings other than the sim's own sight claim — e.g.
//! turn-0 seed 42 is night under torchlight through a dichromat eye, so the
//! honest wire colour is dark ([36,36,1]) and hard to read as a picture.
//!
//! **The claims it renders are NOT the sim's sight claim.** `natural` shows
//! what the sim actually emitted; `rainbow` and `gray` are purely
//! diagnostic overlays driven by catalogue indices and luma respectively.
//! This is a candidate for deletion, or for promotion to an idea-registry
//! row if it earns its keep.
//!
//! Run: `cargo run -p hornvale-game --example vision [-- <file>] [--vision <mode>]`
//!
//! NOTE (throwaway code): the projection below reimplements
//! `hornvale_game_core::chart`'s private `project` locally rather than
//! promoting it to `pub`. If this sketch survives, that promotion (or an
//! idea-registry row) is the honest next step.

use std::collections::BTreeMap;
use std::env;

use hornvale_game_core::schema::{Plan, Snapshot, Spatial};

const DEFAULT_FIXTURE: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../core/tests/fixtures/session-seed-42-turn-0.json"
);

/// The three vision modes.
#[derive(Clone, Copy)]
enum Vision {
    /// The wire colour as-is — the only mode that shows the sim's claim.
    Natural,
    /// Catalogue-index-driven hue wheel (golden-angle stepping), full
    /// saturation/value. Purely diagnostic.
    Rainbow,
    /// Rec.709 luma of the natural colour, as R=G=B.
    Gray,
}

fn parse_vision(name: &str) -> Vision {
    match name {
        "natural" => Vision::Natural,
        "rainbow" => Vision::Rainbow,
        "gray" => Vision::Gray,
        other => panic!("--vision expects natural|rainbow|gray, got {other:?}"),
    }
}

fn mode_name(v: Vision) -> &'static str {
    match v {
        Vision::Natural => "natural",
        Vision::Rainbow => "rainbow",
        Vision::Gray => "gray",
    }
}

fn main() {
    let mut path: Option<String> = None;
    let mut vision = Vision::Natural;
    let mut args = env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--vision" => {
                vision = parse_vision(
                    args.next()
                        .expect("--vision expects natural|rainbow|gray")
                        .as_str(),
                );
            }
            _ => path = Some(arg),
        }
    }
    let path = path.unwrap_or_else(|| DEFAULT_FIXTURE.to_string());
    let text = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("cannot read {path}: {e}"));
    let snap: Snapshot =
        serde_json::from_str(&text).unwrap_or_else(|e| panic!("bad snapshot {path}: {e}"));
    assert_eq!(snap.schema, "vessel/session/v2", "wrong schema tag");

    let mut rows: BTreeMap<i64, BTreeMap<i64, Tile>> = BTreeMap::new();
    let mut legend = format!("vision: {}", mode_name(vision));
    match &snap.spatial {
        Spatial::Walk { chart } => {
            draw_walk(chart, vision, &mut rows);
            legend += &format!(
                " | walk band | radius {} | {} cells",
                chart.radius,
                chart.cells.len()
            );
            if let Some(sight) = &chart.sight {
                legend += &format!(
                    " | sight: observer={} projection={}",
                    sight.observer, sight.projection
                );
            }
        }
        Spatial::Chamber { plan } => {
            draw_chamber(plan, vision, &mut rows);
            legend += &format!(
                " | chamber plan | {}x{} | {} palette entries",
                plan.extent.w,
                plan.extent.h,
                plan.palette.len()
            );
        }
        // The underground band (The Gallery, Task 9) has no colour channel
        // at all — `vessel/level/v1`'s palette interns on `(kind,
        // visibility)`, never a shade (spec §4.1) — so there is nothing for
        // a vision-mode sketch to remap here. Rather than fabricate a
        // colour this band never claims, this arm draws no tiles and says
        // so in the legend.
        Spatial::Underground { level } => {
            legend += &format!(
                " | underground level | rung {} | {}x{} | no colour channel (monochrome band)",
                level.rung, level.extent.w, level.extent.h
            );
        }
    }

    for (_, row) in rows {
        let mut line = String::new();
        for (_, tile) in row {
            line += &paint(&tile, vision);
        }
        println!("{line}");
    }
    println!("{legend}");
}

/// One glyph+colour pair ready to print.
struct Tile {
    glyph: char,
    color: Option<[u8; 3]>,
}

/// The ANSI truecolor escape shape `term.rs` emits (foreground only; this
/// sketch never touches the alternate screen or raw mode).
fn paint(tile: &Tile, vision: Vision) -> String {
    let rgb: Option<[u8; 3]> = match (vision, tile.color) {
        (Vision::Natural, c) => c,
        // Rainbow colouring is applied at tile-construction time (it keys
        // off catalogue indices the Tile does not carry), so by the time a
        // tile is painted its colour already IS the rainbow claim.
        (Vision::Rainbow, c) => c,
        (Vision::Gray, Some([r, g, b])) => {
            // Rec.709 luma, rounded once.
            let l = (0.2126 * f64::from(r) + 0.7152 * f64::from(g) + 0.0722 * f64::from(b)).round()
                as u8;
            Some([l, l, l])
        }
        (Vision::Gray, None) => None,
    };
    match rgb {
        Some([r, g, b]) => format!("\x1b[38;2;{r};{g};{b}m{}\x1b[39m", tile.glyph),
        None => tile.glyph.to_string(),
    }
}

// Trig-free HSV→RGB: hue in degrees, s=v=1. Only arithmetic — no
// transcendentals, so no platform-libm question arises at all.
fn hsv(hue_deg: f64) -> [u8; 3] {
    let h = hue_deg.rem_euclid(360.0) / 60.0;
    let i = h.floor();
    let f = h - i;
    let [r, g, b] = match i as i64 % 6 {
        0 => [1.0, f, 0.0],
        1 => [1.0 - f, 1.0, 0.0],
        2 => [0.0, 1.0, f],
        3 => [0.0, 1.0 - f, 1.0],
        4 => [f, 0.0, 1.0],
        _ => [1.0, 0.0, 1.0 - f],
    };
    [
        (r * 255.0).round() as u8,
        (g * 255.0).round() as u8,
        (b * 255.0).round() as u8,
    ]
}

/// The golden angle in degrees — the hue step for the rainbow wheel.
const GOLDEN_ANGLE: f64 = 137.5;

// Same exposure and same remedy as `core/src/chart.rs::project`: the lint
// is inherited by directory ancestry, the named remedy
// (`hornvale_kernel::math`) is unavailable in a client by design, and this
// projection paints a diagnostic picture to stdout, never a committed
// artifact — decision 0055 leaves clients unconstrained.
#[allow(clippy::disallowed_methods)]
fn draw_walk(
    chart: &hornvale_game_core::schema::Chart,
    vision: Vision,
    rows: &mut BTreeMap<i64, BTreeMap<i64, Tile>>,
) {
    // Local copy of chart.rs's private projection — see the header note.
    let farthest = chart
        .cells
        .iter()
        .map(|c| c.distance_rad)
        .fold(0.0f64, f64::max);
    let rings = i64::from(chart.radius.max(1));
    for cell in &chart.cells {
        let theta = cell.bearing_deg.to_radians();
        let r = if farthest.is_finite() && farthest > 0.0 {
            cell.distance_rad / farthest * rings as f64
        } else {
            0.0
        };
        // Half-away-from-zero rounding, matching chart.rs's `project`.
        let row = (-theta.cos() * r).round() as i64;
        let col = (theta.sin() * r * 2.0).round() as i64;
        let glyph = if cell.state == "here" { '@' } else { '+' };
        let color = match vision {
            // Biome index on the golden-angle hue wheel, full sat/val.
            Vision::Rainbow => Some(hsv(f64::from(cell.biome) * GOLDEN_ANGLE)),
            _ => cell.color,
        };
        rows.entry(row)
            .or_default()
            .insert(col, Tile { glyph, color });
    }
}

fn draw_chamber(plan: &Plan, vision: Vision, rows: &mut BTreeMap<i64, BTreeMap<i64, Tile>>) {
    // Rainbow keys off the palette ENTRY KIND, stepped on the same
    // golden-angle hue wheel in first-seen order.
    let mut kinds: Vec<&str> = Vec::new();
    for e in &plan.palette {
        if !kinds.contains(&e.kind.as_str()) {
            kinds.push(&e.kind);
        }
    }
    for gy in 0..plan.extent.h {
        for gx in 0..plan.extent.w {
            let i = (gy * plan.extent.w + gx) as usize;
            let entry = match plan.palette.get(plan.cells[i] as usize) {
                Some(e) => e,
                None => continue,
            };
            let glyph = match entry.kind.as_str() {
                "floor" => '.',
                "threshold" => '+',
                _ => '#',
            };
            let color = match vision {
                Vision::Rainbow => {
                    let idx = kinds.iter().position(|k| *k == entry.kind).unwrap_or(0);
                    Some(hsv(f64::from(idx as u32) * GOLDEN_ANGLE))
                }
                _ => entry.color,
            };
            rows.entry(i64::from(gy))
                .or_default()
                .insert(i64::from(gx), Tile { glyph, color });
        }
    }
    // The possession stands out regardless of vision mode.
    if let Some(row) = rows.get_mut(&i64::from(plan.you.y)) {
        row.insert(
            i64::from(plan.you.x),
            Tile {
                glyph: '@',
                color: Some([255, 255, 255]),
            },
        );
    }
}
