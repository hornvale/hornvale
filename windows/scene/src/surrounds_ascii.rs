//! The in-process ASCII render of a `scene/surrounds/v1` document — Ring 0/1
//! under decision 0022, the same class as the gallery's three ASCII maps. A
//! render is a registered LENS, never ground truth (RENDER-9), and the
//! caption — not the picture — carries the honesty: it names the lens, the
//! orientation, and everything the picture had to leave out.

use crate::SurroundsScene;
use std::collections::BTreeMap;

/// The registered lenses. v1 ships one; a second is purely additive.
/// type-audit: bare-ok(identifier-text)
pub const SURROUNDS_LENSES: [&str; 1] = ["terrain"];

/// The glyph a cell draws under the `terrain` lens, before fading.
fn terrain_glyph(scene: &SurroundsScene, cell: &crate::SurroundsCell) -> char {
    if cell.state == "here" {
        return '@';
    }
    if let Some(m) = cell
        .marks
        .iter()
        .min_by(|a, b| a.salience.cmp(&b.salience).then(a.noun.cmp(&b.noun)))
    {
        return if m.kind == "agent" { '&' } else { '#' };
    }
    let water = scene
        .water_legend
        .get(cell.water as usize)
        .map(String::as_str)
        .unwrap_or("dry-land");
    match water {
        "ocean" => '~',
        "salt-basin" => '=',
        "river" => '+',
        _ => match cell.relief {
            0 | 1 => '_',
            2 => '.',
            3 => ':',
            4 => '^',
            _ => 'A',
        },
    }
}

/// A glyph's memory twin — what a `remembered` cell draws instead.
fn faded(g: char) -> char {
    match g {
        '~' | '=' | '+' | '_' => '-',
        '.' => ',',
        ':' => ';',
        '^' => 'n',
        'A' => 'a',
        '#' => 'o',
        '&' => '%',
        other => other,
    }
}

/// Render `scene` through `lens`. `ways` are the compass names of the
/// observer's real exits — the chart is lattice-aligned rather than north-up,
/// so the exits are how a reader orients.
/// type-audit: bare-ok(identifier-text: lens), bare-ok(identifier-text: ways), bare-ok(prose: return)
pub fn render_surrounds_ascii(scene: &SurroundsScene, lens: &str, ways: &[String]) -> String {
    if !SURROUNDS_LENSES.contains(&lens) {
        return format!(
            "There is no lens '{lens}'. Registered lenses: {}.",
            SURROUNDS_LENSES.join(", ")
        );
    }

    // Place every non-seam cell. row = -w; col = 2v + (up ? 0 : 1) + w. The
    // `+ w` term cancels the lattice's row offset: an up-triangle's
    // horizontal-edge neighbour below it (row + 1, col + 1 in the un-sheared
    // formula) would otherwise land down-and-to-the-right instead of
    // directly below, drawing a breadth-first ball as a right-leaning
    // parallelogram rather than the symmetric hexagon it actually is.
    let mut placed: BTreeMap<(i64, i64), char> = BTreeMap::new();
    let mut seams = 0usize;
    for c in &scene.cells {
        let (Some(v), Some(w), Some(up)) = (c.v, c.w, c.up) else {
            seams += 1;
            continue;
        };
        let row = -w;
        let col = 2 * v + i64::from(!up) + w;
        let g = terrain_glyph(scene, c);
        let g = if c.state == "remembered" { faded(g) } else { g };
        placed.insert((row, col), g);
    }

    let mut out = String::new();
    // Interpolate `lens`, never a literal: the caption's whole job is to name
    // which lens you are wearing, so a hardcoded name would make every render
    // through a second lens caption a lie.
    out.push_str(&format!(
        "[lens: {lens} · depth {} · radius {} · lattice-aligned, not north-up]\n",
        scene.depth, scene.radius
    ));

    if placed.is_empty() {
        out.push_str("  (nothing placeable in view)\n");
    } else {
        let rows: Vec<i64> = placed.keys().map(|&(r, _)| r).collect();
        let cols: Vec<i64> = placed.keys().map(|&(_, c)| c).collect();
        let (r0, r1) = (*rows.iter().min().unwrap(), *rows.iter().max().unwrap());
        let (c0, c1) = (*cols.iter().min().unwrap(), *cols.iter().max().unwrap());
        for r in r0..=r1 {
            let mut line = String::new();
            for c in c0..=c1 {
                line.push(*placed.get(&(r, c)).unwrap_or(&' '));
            }
            out.push_str(line.trim_end());
            out.push('\n');
        }
    }

    if !ways.is_empty() {
        out.push_str(&format!("  ways on: {}\n", ways.join(", ")));
    }
    if seams > 0 {
        out.push_str(&format!(
            "  {seams} cell{} beyond a face seam: real ground, no honest place on this chart.\n",
            if seams == 1 { "" } else { "s" }
        ));
    }
    if !scene.legend.is_empty() {
        out.push_str("  legend: ");
        let nouns: Vec<&str> = scene.legend.iter().map(|e| e.noun.as_str()).collect();
        out.push_str(&nouns.join(", "));
        out.push('\n');
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{SurroundsCell, SurroundsObserver, SurroundsScene};

    fn cell(u: i64, v: i64, w: i64, up: bool, state: &str, relief: u32) -> SurroundsCell {
        SurroundsCell {
            room: (u * 1000 + v * 10 + w).unsigned_abs() + u64::from(up),
            u: Some(u),
            v: Some(v),
            w: Some(w),
            up: Some(up),
            seam: false,
            state: state.to_string(),
            biome: 0,
            water: 3, // dry-land
            relief,
            regime: None,
            temperature_c: None,
            moisture: None,
            elevation_m: None,
            marks: vec![],
        }
    }

    fn scene(cells: Vec<SurroundsCell>) -> SurroundsScene {
        SurroundsScene {
            schema: crate::SURROUNDS_SCHEMA.to_string(),
            seed: 42,
            day: 0.0,
            observer: SurroundsObserver {
                room: 1,
                face: 0,
                depth: 12,
                latitude: 0.0,
                longitude: 0.0,
            },
            radius: 1,
            depth: 12,
            orientation: "lattice".to_string(),
            biome_legend: vec!["tundra".to_string()],
            water_legend: ["ocean", "salt-basin", "river", "dry-land"]
                .iter()
                .map(|s| s.to_string())
                .collect(),
            relief_legend: crate::RELIEF_LEGEND.iter().map(|s| s.to_string()).collect(),
            cells,
            legend: vec![],
        }
    }

    #[test]
    fn the_observer_is_an_at_sign_and_its_row_reads_left_to_right() {
        // An up observer at the origin, its same-row neighbours either side.
        let s = scene(vec![
            cell(0, 0, 0, true, "here", 2),
            cell(-1, 0, 0, false, "sensed", 2), // col +1: to the right
            cell(0, -1, 0, false, "sensed", 2), // col -1: to the left
        ]);
        let out = render_surrounds_ascii(&s, "terrain", &[]);
        let grid: Vec<&str> = out
            .lines()
            .filter(|l| !l.starts_with('[') && !l.starts_with("  "))
            .collect();
        assert!(
            grid.iter().any(|l| l.contains(".@.")),
            "the observer sits between its two same-row neighbours: {out}"
        );
    }

    #[test]
    fn a_remembered_cell_fades() {
        let s = scene(vec![
            cell(0, 0, 0, true, "here", 2),
            cell(-1, 0, 0, false, "remembered", 2),
        ]);
        let out = render_surrounds_ascii(&s, "terrain", &[]);
        assert!(out.contains('@'), "the observer is drawn");
        assert!(
            out.contains(','),
            "a remembered lowland fades '.' -> ',': {out}"
        );
    }

    #[test]
    fn the_caption_names_the_lens_and_declares_the_orientation() {
        let s = scene(vec![cell(0, 0, 0, true, "here", 2)]);
        let out = render_surrounds_ascii(&s, "terrain", &["E".to_string(), "Nw".to_string()]);
        let caption = out.lines().next().unwrap();
        assert!(caption.contains("lens: terrain"), "{caption}");
        assert!(caption.contains("lattice-aligned"), "{caption}");
        assert!(
            out.contains("ways on: E, Nw"),
            "the exits are the orientation hint, since the chart is not north-up: {out}"
        );
    }

    #[test]
    fn the_caption_names_whichever_registered_lens_was_asked_for() {
        // Registry-driven rather than hardcoded, so this gains real teeth the
        // moment a second lens is registered: a caption that named a literal
        // would then lie about which lens produced the picture.
        let s = scene(vec![cell(0, 0, 0, true, "here", 2)]);
        for lens in SURROUNDS_LENSES {
            let caption = render_surrounds_ascii(&s, lens, &[])
                .lines()
                .next()
                .expect("a render always opens with its caption")
                .to_string();
            assert!(
                caption.contains(&format!("lens: {lens}")),
                "the caption must name the lens it was asked for, got: {caption}"
            );
        }
    }

    #[test]
    fn a_seam_is_disclosed_not_hidden() {
        let mut seam = cell(0, 0, 0, true, "sensed", 2);
        seam.seam = true;
        seam.u = None;
        seam.v = None;
        seam.w = None;
        seam.up = None;
        seam.room = 999;
        let s = scene(vec![cell(0, 0, 0, true, "here", 2), seam]);
        let out = render_surrounds_ascii(&s, "terrain", &[]);
        assert!(
            out.contains("1 cell beyond a face seam"),
            "an unplaceable cell must be stated, not dropped silently: {out}"
        );
    }

    #[test]
    fn an_unknown_lens_is_refused_loudly() {
        let s = scene(vec![cell(0, 0, 0, true, "here", 2)]);
        let out = render_surrounds_ascii(&s, "nonesuch", &[]);
        assert!(out.contains("no lens 'nonesuch'"), "{out}");
        assert!(
            out.contains("terrain"),
            "the refusal names the registry: {out}"
        );
    }

    #[test]
    fn the_render_is_deterministic() {
        let s = scene(vec![
            cell(0, 0, 0, true, "here", 2),
            cell(-1, 0, 0, false, "sensed", 4),
        ]);
        assert_eq!(
            render_surrounds_ascii(&s, "terrain", &[]),
            render_surrounds_ascii(&s, "terrain", &[])
        );
    }
}
