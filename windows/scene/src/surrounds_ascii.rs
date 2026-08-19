//! The in-process ASCII render of a `scene/surrounds/v1` document — Ring 0/1
//! under decision 0022, the same class as the gallery's three ASCII maps. A
//! render is a registered LENS, never ground truth (RENDER-9), and the
//! caption — not the picture — carries the honesty: it names the lens, the
//! orientation, and everything the picture had to leave out.
//!
//! ## The chart is north-up, and this module is the reference projection
//!
//! [`project_north_up`] is the **spatial channel** spec §2 names as the
//! fourth: it turns a cell's polar coordinate about the observer
//! (`bearing_deg`, `distance_rad`) into a box on the character grid. Two
//! client renderers — `clients/game/core/src/chart.rs` and
//! `clients/vessel/src/pane_chart.ts` — implement the same geometry
//! independently, because neither may depend on this crate; the agreement
//! test (`clients/game/core/tests/chart.rs`) compares the first of those
//! against a fixture generated from THIS renderer, so this file is the
//! authority and those two are replicas.
//!
//! This replaced a lattice-aligned projection (`row = -w`,
//! `col = 2v + up-parity + w`) that could place only the cells carrying
//! lattice offsets — seam cells, where the lattice bends across a base
//! face, had none and were counted in a footer instead of drawn. Bearing
//! and distance exist for every cell including a seam one, so the whole
//! band draws now.
//!
//! Two consequences the lattice projection did not have, and both are
//! handled explicitly rather than assumed away:
//!
//! - **It is not injective.** A lattice projection is bijective by
//!   construction; rounding a polar coordinate onto a character grid is
//!   not. [`box_rank`] decides who keeps a contested box, and the caption
//!   states how many cells lost one.
//! - **It has a scale.** See [`project_north_up`] for what sets it and why
//!   it is read off the band itself rather than from the sphere's geometry
//!   (which a thin client does not have).

use crate::SurroundsScene;
use std::collections::BTreeMap;

/// The registered lenses. `terrain` draws the chart; `colour` draws the same
/// chart and tints it. Adding one is purely additive: the three committed
/// gallery charts render through `terrain` and cannot move.
/// type-audit: bare-ok(identifier-text)
pub const SURROUNDS_LENSES: [&str; 2] = ["terrain", "colour"];

/// The glyph a cell draws under the `terrain` lens, before fading, paired
/// with whether the colour lens may tint it — the colour it withholds tint
/// from otherwise names something the reader cannot see (the observer, a
/// mark, or the water covering the ground). `bool` no longer means "draws
/// the bedrock": since colour was recomposed as a surface mixture (surface
/// cover over the mineral blend, not raw rock), it means "the colour
/// describes what this glyph draws" — true only on the land arm below,
/// where the glyph really is the ground the surface colour was mixed for.
fn terrain_glyph(scene: &SurroundsScene, cell: &crate::SurroundsCell) -> (char, bool) {
    if cell.state == "here" {
        return ('@', false);
    }
    if let Some(m) = cell
        .marks
        .iter()
        .min_by(|a, b| a.salience.cmp(&b.salience).then(a.noun.cmp(&b.noun)))
    {
        return (if m.kind == "agent" { '&' } else { '#' }, false);
    }
    let water = scene
        .water_legend
        .get(cell.water as usize)
        .map(String::as_str)
        .unwrap_or("dry-land");
    match water {
        "ocean" => ('~', false),
        "salt-basin" => ('=', false),
        "river" => ('+', false),
        _ => (impedance_glyph(cell), true),
    }
}

/// The **ordinal** a dry-land cell draws: impedance, not relief alone —
/// "how hard this ground is to cross," absorbing canopy and roughness into
/// the one ranked answer spec §2.2 calls for. Reuses the five glyphs the
/// pre-impedance relief ladder already spent (`_ . : ^ A`), because the
/// survey this ladder was designed against (Task 4 report) found every
/// relief band, from `shelf` to `alpine`, carrying a full [-1, 1] spread on
/// both `micro.openness` and `micro.relief` — five bands' worth of real
/// spread, no more, no fewer.
///
/// `cell.relief` (0..=5) is the base term: elevation is the coarsest, most
/// reliable difficulty signal a room carries. Two `Micro` terms perturb it,
/// each bounded to at most one band of movement, so impedance never crosses
/// two elevation bands at once from vegetation or terrain roughness alone:
///
///   - **canopy** — `micro.openness` (`-1` closed .. `+1` open) contributes
///     `(1 - openness) / 2` (`0` in the open, `1` under closed canopy):
///     thick cover is a real obstacle to a walker, open ground is not.
///   - **roughness** — `micro.relief` (`-1` hollow .. `+1` rise) contributes
///     `|micro.relief|` (`0` flat, `1` at either extreme): a hollow and a
///     rise are equally uneven underfoot, so only the magnitude counts, not
///     the sign. `micro.aspect` (sun exposure) is deliberately not used
///     here — it says which way a slope faces, not how hard the ground
///     itself is to cross, and folding it in would let shade alone raise a
///     cell's rung.
///
/// The two perturbations are weighted `0.5` each, so a cell at its very
/// worst (closed canopy AND maximal roughness) rounds up at most one band
/// above its bare relief, and the flattest, most open cell of a given
/// relief band renders identically to the pre-impedance ladder.
fn impedance_glyph(cell: &crate::SurroundsCell) -> char {
    let canopy = (1.0 - cell.micro.openness) / 2.0;
    let roughness = cell.micro.relief.abs();
    let impedance = f64::from(cell.relief) + 0.5 * canopy + 0.5 * roughness;
    match impedance.round() as i64 {
        0 | 1 => '_',
        2 => '.',
        3 => ':',
        4 => '^',
        _ => 'A',
    }
}

/// A cell's box on the character grid, from its polar coordinate about the
/// observer — the **spatial channel** (spec §2), and the one thing all three
/// of Hornvale's chart renderers must agree on.
///
/// `bearing_deg` is the great-circle initial azimuth clockwise from north
/// and `distance_rad` is the great-circle angle, so the cell's offset from
/// the observer is `(north, east) = distance * (cos θ, sin θ)`. North is up,
/// which is *negative* row; east is right, positive column.
///
/// **The scale.** `farthest` is the largest `distance_rad` in the band and
/// `rings` is the band's BFS radius, so the outermost cell lands `rings`
/// row-units from the observer — one row per BFS ring. Row-units, not rows:
/// a cell due east spends its whole radius on columns and reaches row 0, so
/// a band whose farthest cell happens to lie east draws shorter than one
/// whose farthest cell lies north. Both inputs are read off the
/// document rather than derived from the sphere, deliberately: a client that
/// could compute a room's angular size from `depth` would be doing the
/// spherical trigonometry spec §5.1 put on the wire precisely to avoid. A
/// band whose cells are all at distance zero (radius 0, the observer alone)
/// collapses to the origin rather than dividing by it.
///
/// **The column doubles.** A monospace character cell is about twice as
/// tall as it is wide, so an eastward degree needs two columns to cover the
/// screen distance a northward degree covers in one row. Without it the
/// chart is an ellipse claiming to be a circle.
///
/// **Rounding is half-away-from-zero**, which is Rust's `f64::round` and is
/// NOT JavaScript's `Math.round` (that one rounds half toward `+∞`, so it
/// disagrees on every negative half-integer). The TypeScript replica states
/// this and compensates; it is written here because this is the definition
/// the other two copy.
fn project_north_up(bearing_deg: f64, distance_rad: f64, farthest: f64, rings: i64) -> (i64, i64) {
    if !farthest.is_finite() || farthest <= 0.0 {
        return (0, 0);
    }
    let theta = bearing_deg.to_radians();
    let r = distance_rad / farthest * rings as f64;
    let row = (-hornvale_kernel::math::cos(theta) * r).round() as i64;
    let col = (hornvale_kernel::math::sin(theta) * r * 2.0).round() as i64;
    (row, col)
}

/// The ordering key [`box_rank`] returns; see its doc for the three clauses,
/// in the tuple's own order.
type BoxRank = (bool, bool, u32, usize);

/// The rank that decides which cell keeps a box when two of them land in the
/// same one. Smallest wins.
///
/// **The rule, stated once: salience ranks, weight inks, and neither becomes
/// the other.** Spec §2 leaves this open and §2.3 licenses the three
/// renderers to differ in *vocabulary* but never in the *rule*, so this is
/// the rule, and `clients/game/core/src/chart.rs` and
/// `clients/vessel/src/pane_chart.ts` implement the identical ordering:
///
/// 1. **The observer never loses their own box.** The chart is egocentric
///    (decision 0076); a band that drew over `@` would have lost the one
///    cell the reader is standing in.
/// 2. **A marked cell beats an unmarked one, and among marked cells the
///    numerically smallest `salience` wins** — `salience` is a RANK where
///    lower is more salient, never a magnitude.
/// 3. **Ties break on document order**, which `surrounds.rs` fixes as
///    ascending `room`. Deterministic, and available to a renderer that
///    does not mirror `room` at all.
///
/// What is deliberately absent is the epistemic state. A `remembered` cell
/// holding the flagship settlement **wins its box and draws dim**: it is the
/// most salient thing standing there, and the chart still says you are
/// remembering it rather than looking at it. Weight never promotes a cell
/// and salience never brightens one.
fn box_rank(cell: &crate::SurroundsCell, index: usize) -> BoxRank {
    let dominant = cell.marks.iter().map(|m| m.salience).min();
    (
        cell.state != "here",
        dominant.is_none(),
        dominant.unwrap_or(0),
        index,
    )
}

/// One placed glyph: what to draw, the colour its cell carries (if any), and
/// whether the glyph is drawing the ground that colour describes.
struct Placed {
    /// The character drawn at this position — the same character regardless
    /// of epistemic state; see [`dimmed`].
    glyph: char,
    /// The cell's `color`, straight from the document; `None` when the
    /// scene was built through an uncoloured path.
    color: Option<[u8; 3]>,
    /// Whether `glyph` draws the surface `color` describes — see
    /// [`terrain_glyph`].
    ground: bool,
    /// Whether the cell is `remembered` rather than currently sensed — the
    /// epistemic channel, which dims rather than substituting a glyph.
    remembered: bool,
}

/// Wrap `glyph` in a 24-bit foreground colour and a reset.
///
/// Truecolor rather than the 256-colour cube: a terminal that does not
/// understand it degrades to an uncoloured glyph rather than a wrong one,
/// and the sim has no business probing the terminal's capabilities.
fn colored(glyph: char, rgb: [u8; 3]) -> String {
    format!(
        "\u{1b}[38;2;{};{};{}m{glyph}\u{1b}[0m",
        rgb[0], rgb[1], rgb[2]
    )
}

/// Wrap `s` in the terminal's dim attribute and a reset.
///
/// This is the epistemic channel's encoding (spec §2) on a lens that HAS a
/// weight channel: a `remembered` cell dims — it does not substitute a
/// different glyph. Composed *around* [`colored`]'s output rather than
/// replacing it, so a remembered cell that also carries colour gets both:
/// `\x1b[2m\x1b[38;2;r;g;bm<glyph>\x1b[0m\x1b[0m`.
///
/// Only the `colour` lens calls this. The `terrain` lens is documented
/// elsewhere (`windows/vessel/src/session.rs`'s `Eyes::Off` fallback) as
/// emitting zero escape sequences — the posture a screen reader takes — so
/// per spec §2.3 it **loses the epistemic axis entirely** rather than
/// recovering it through an escape the surface promised not to emit; see
/// the `epistemic:` caption line `render_surrounds_ascii` adds for that
/// lens.
fn dimmed(s: &str) -> String {
    format!("\u{1b}[2m{s}\u{1b}[0m")
}

/// Render `scene` through `lens`. `ways` are the compass names of the
/// observer's real exits — a reader who wants to walk still needs those,
/// because the chart is oriented to north and the lattice underfoot is not.
/// type-audit: bare-ok(identifier-text: lens), bare-ok(identifier-text: ways), bare-ok(prose: return)
pub fn render_surrounds_ascii(scene: &SurroundsScene, lens: &str, ways: &[String]) -> String {
    if !SURROUNDS_LENSES.contains(&lens) {
        return format!(
            "There is no lens '{lens}'. Registered lenses: {}.",
            SURROUNDS_LENSES.join(", ")
        );
    }

    // Place EVERY cell, seam cells included — see [`project_north_up`] for
    // the geometry and [`box_rank`] for who keeps a contested box. The
    // winner is decided by rank rather than by insertion order, so the
    // picture does not depend on which cell the document happened to list
    // last.
    let farthest = scene
        .cells
        .iter()
        .map(|c| c.distance_rad)
        .fold(0.0f64, f64::max);
    let rings = i64::from(scene.radius.max(1));
    let mut placed: BTreeMap<(i64, i64), (BoxRank, Placed)> = BTreeMap::new();
    for (i, c) in scene.cells.iter().enumerate() {
        let at = project_north_up(c.bearing_deg, c.distance_rad, farthest, rings);
        let rank = box_rank(c, i);
        if placed.get(&at).is_some_and(|(held, _)| *held <= rank) {
            continue;
        }
        let (g, ground) = terrain_glyph(scene, c);
        placed.insert(
            at,
            (
                rank,
                Placed {
                    glyph: g,
                    color: c.color,
                    ground,
                    remembered: c.state == "remembered",
                },
            ),
        );
    }
    let occluded = scene.cells.len().saturating_sub(placed.len());
    let placed: BTreeMap<(i64, i64), Placed> =
        placed.into_iter().map(|(at, (_, p))| (at, p)).collect();

    let mut out = String::new();
    // Interpolate `lens`, never a literal: the caption's whole job is to name
    // which lens you are wearing, so a hardcoded name would make every render
    // through a second lens caption a lie.
    out.push_str(&format!(
        "[lens: {lens} · depth {} · radius {} · north-up]\n",
        scene.depth, scene.radius
    ));

    // The spatial channel's own disclosure, and the counterpart to the
    // `colour:`/`epistemic:` lines below: the projection is not injective,
    // so a cell can lose its box to a more salient one and vanish from the
    // picture entirely. The count is what makes that checkable against the
    // chart in front of the reader rather than trusted on the renderer's
    // word — and it sits ABOVE the `sight:` line on purpose, because
    // `scripts/regenerate-artifacts.sh`'s `gen_chart_reference` slices the
    // grid out of this render by the range between `sight:` and `ways on:`.
    out.push_str(&format!(
        "  placement: north-up, one row per ring, east doubled for the \
         character cell — {} of {} cells drawn, {occluded} occluded where two \
         fell in one box (the more salient kept it).\n",
        placed.len(),
        scene.cells.len()
    ));

    // The colour lens's own disclosure, and the reason it is a caption line
    // rather than a footnote: the tint is the cell's SURFACE cover
    // (vegetation, litter, snow, sand or silt over the mineral blend — see
    // `windows/locale/src/surface.rs`), and the chart draws plenty of glyphs
    // whose surface is not the one on screen. Rather than let the picture
    // claim a river is meadow-coloured and retract it underneath, the lens
    // withholds the tint from every non-ground glyph and says how many it
    // withheld. The three counts partition the placed cells, so a reader can
    // check the sentence against the picture instead of trusting it.
    if lens == "colour" {
        let tinted = placed
            .values()
            .filter(|p| p.ground && p.color.is_some())
            .count();
        let withheld = placed
            .values()
            .filter(|p| !p.ground && p.color.is_some())
            .count();
        let bare = placed.values().filter(|p| p.color.is_none()).count();
        out.push_str(&format!(
            "  colour: each cell's surface cover, tinted only where the glyph draws that \
             ground — {tinted} tinted, {withheld} withheld (water, a mark, or you), \
             {bare} carrying no colour.\n"
        ));
        // The sight declaration, read from the document rather than assumed:
        // a caption that hardcoded "the standard observer" would lie the
        // moment a non-standard eye coloured the chart. Absent only when the
        // scene predates the colour layer or was built through the
        // uncoloured path.
        if let Some(sight) = &scene.sight {
            out.push_str(&format!(
                "  sight: {} · {} channels ({} chromatic) · {} projection, preserving {}.\n",
                sight.observer, sight.channels, sight.chromatic, sight.projection, sight.preserves
            ));
        }
        // §2.3's own axis-loss disclosure for THIS lens, the counterpart to
        // the terrain lens's `epistemic:` sentence below. The counts line
        // above states a fact a reader must still do arithmetic on to
        // notice ("0 tinted, 0 withheld" reads as an unremarkable band
        // unless you already suspect the chromatic axis is gone); this line
        // states the conclusion outright, in the same voice, the moment the
        // picture carries literally no colour information at all — every
        // placed cell bare is the strongest, always-correct signal that the
        // chromatic axis contributed nothing to this render, regardless of
        // WHY (no observer at all, an observer with zero chromatic
        // channels, or an observer with channels but no projection — every
        // one of those is the same loss from the reader's side of the
        // screen). Never fires when even one cell carries a colour, so it
        // cannot misfire on a merely water-heavy band that still tints its
        // dry ground.
        if !placed.is_empty() && bare == placed.len() {
            out.push_str(&format!(
                "  colour: this chart carries no chromatic channel, so no cell is tinted \
                 regardless of its surface cover — {bare} of {} placed.\n",
                placed.len()
            ));
        }
    }

    // The terrain lens's own disclosure — the epistemic counterpart to the
    // colour lens's tint disclosure above. This lens carries no weight
    // channel (it is the escape-free surface `Eyes::Off` picks for exactly
    // that reason, per `windows/vessel/src/session.rs`), so per spec §2.3 it
    // loses the epistemic axis entirely rather than recovering it some other
    // way — the count is what makes the sentence checkable against the
    // picture instead of trusted on its word.
    if lens == "terrain" {
        let remembered = placed.values().filter(|p| p.remembered).count();
        out.push_str(&format!(
            "  epistemic: this lens carries no weight channel, so remembered cells draw \
             identically to sensed ones — {remembered} of {} placed.\n",
            placed.len()
        ));
    }

    if placed.is_empty() {
        out.push_str("  (nothing placeable in view)\n");
    } else {
        let rows: Vec<i64> = placed.keys().map(|&(r, _)| r).collect();
        let cols: Vec<i64> = placed.keys().map(|&(_, c)| c).collect();
        let (r0, r1) = (*rows.iter().min().unwrap(), *rows.iter().max().unwrap());
        let (c0, c1) = (*cols.iter().min().unwrap(), *cols.iter().max().unwrap());
        for r in r0..=r1 {
            // `trailing_blanks` replaces the old `line.trim_end()`: trimming
            // a string that holds escape sequences would cut inside one.
            // Buffering the gaps and flushing them only before a real glyph
            // produces the identical trimmed line for the terrain lens.
            let mut line = String::new();
            let mut trailing_blanks = String::new();
            for c in c0..=c1 {
                match placed.get(&(r, c)) {
                    None => trailing_blanks.push(' '),
                    Some(p) => {
                        line.push_str(&trailing_blanks);
                        trailing_blanks.clear();
                        let drawn = match (lens, p.color, p.ground) {
                            ("colour", Some(rgb), true) => colored(p.glyph, rgb),
                            _ => p.glyph.to_string(),
                        };
                        // Weight is gated on the LENS carrying a weight
                        // channel at all, not on whether this particular
                        // cell happens to be coloured — `terrain` never
                        // dims, `colour` always does for a remembered cell,
                        // coloured or not.
                        if lens == "colour" && p.remembered {
                            line.push_str(&dimmed(&drawn));
                        } else {
                            line.push_str(&drawn);
                        }
                    }
                }
            }
            out.push_str(&line);
            out.push('\n');
        }
    }

    if !ways.is_empty() {
        out.push_str(&format!("  ways on: {}\n", ways.join(", ")));
    }
    // No seam footer any more, and its absence is the change: a seam cell
    // used to be counted here ("real ground, no honest place on this
    // chart") because the lattice offsets it needs were `null`. Bearing and
    // distance are not, so it is drawn like every other cell and there is
    // nothing left to disclose. The `placement:` line above is what
    // replaced it — it discloses the loss the projection can actually
    // still cause, which is occlusion, not the seam.
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
    use crate::{Mark, Resolution, SurroundsCell, SurroundsObserver, SurroundsScene};
    use hornvale_locale::CoverClass;

    /// One test cell, addressed the way the renderer now reads a cell: by
    /// its POLAR coordinate about the observer, not by lattice offsets.
    /// `bearing_deg` is clockwise from north and `distance_rad` is in units
    /// of the band's own outermost ring (the fixtures below use `1.0` for
    /// "on the rim" and `0.0` for the observer), which is what
    /// [`project_north_up`] normalises against.
    ///
    /// The lattice offsets are still filled in, because a non-seam cell
    /// carries them on the wire — but nothing in this module reads them any
    /// more, so they are a constant rather than a parameter.
    fn cell(bearing_deg: f64, distance_rad: f64, state: &str, relief: u32) -> SurroundsCell {
        SurroundsCell {
            room: (bearing_deg * 1000.0 + distance_rad * 7.0) as u64,
            u: Some(0),
            v: Some(0),
            w: Some(0),
            up: Some(true),
            seam: false,
            state: state.to_string(),
            biome: 0,
            water: 3, // dry-land
            relief,
            regime: None,
            temperature_c: None,
            moisture: None,
            elevation_m: None,
            height_asl_m: None,
            color: None,
            micro: crate::Micro {
                relief: 0.0,
                aspect: 0.0,
                wetness: 0.0,
                openness: 0.0,
            },
            marks: vec![],
            signal: None,
            cover: None,
            bearing_deg,
            distance_rad,
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
            orientation: "north-up".to_string(),
            biome_legend: vec!["tundra".to_string()],
            water_legend: ["ocean", "salt-basin", "river", "dry-land"]
                .iter()
                .map(|s| s.to_string())
                .collect(),
            relief_legend: crate::RELIEF_LEGEND.iter().map(|s| s.to_string()).collect(),
            sea_level_m: 0.0,
            cells,
            legend: vec![],
            sight: None,
            resolution: Resolution {
                grid_level: 6,
                depth_below_grid: 6,
                grid_resolution_fields: ["biome", "water"].iter().map(|s| s.to_string()).collect(),
            },
            cover_legend: CoverClass::LEGEND.iter().map(|s| s.to_string()).collect(),
        }
    }

    /// The four cardinal cells of a rim, plus the observer at the centre.
    /// Every fixture below that needs a placeable chart is built from this
    /// shape or a subset of it, so "two cells collide" is always a
    /// deliberate act rather than an accident of the fixture.
    fn compass_rose(states: [&str; 4]) -> SurroundsScene {
        scene(vec![
            cell(0.0, 0.0, "here", 2),
            cell(0.0, 1.0, states[0], 2),
            cell(90.0, 1.0, states[1], 2),
            cell(180.0, 1.0, states[2], 2),
            cell(270.0, 1.0, states[3], 2),
        ])
    }

    /// The rendered picture alone, caption block and footers stripped.
    ///
    /// A north-up chart is sparse, so a grid row very often DOES begin with
    /// two spaces — the old "not indented two spaces" filter this replaces
    /// was safe only while the lattice projection kept the picture dense,
    /// and would silently have eaten the top and bottom rows of every
    /// hexagon here. What actually distinguishes a caption line is that it
    /// opens with a lowercase keyword and a colon (`colour:`, `sight:`,
    /// `epistemic:`, `placement:`, `ways on:`, `legend:`); the glyph
    /// alphabet contains no lowercase letter at all, so no grid row can.
    fn picture(out: &str) -> Vec<String> {
        out.lines()
            .filter(|l| {
                if l.is_empty() || l.starts_with('[') {
                    return false;
                }
                let Some(rest) = l.strip_prefix("  ") else {
                    return true;
                };
                let keyword: String = rest
                    .chars()
                    .take_while(|c| c.is_ascii_lowercase() || *c == ' ')
                    .collect();
                let is_caption = !keyword.trim().is_empty()
                    && rest[keyword.len()..].starts_with(':')
                    || rest.starts_with('(');
                !is_caption
            })
            .map(str::to_string)
            .collect()
    }

    /// `(row, col)` of the first `glyph` in a rendered picture.
    fn find(out: &str, glyph: char) -> Option<(usize, usize)> {
        picture(out)
            .iter()
            .enumerate()
            .find_map(|(r, line)| line.chars().position(|c| c == glyph).map(|c| (r, c)))
    }

    /// The whole point of the campaign's third stage, pinned on the one
    /// fixture where the answer is not a matter of taste: a cell due north
    /// of the observer draws ABOVE the `@`, due east draws to its RIGHT,
    /// and the two southern/western ones mirror them. The old projection
    /// could not make this claim at all — it was lattice-aligned, and
    /// `orientation` said so.
    #[test]
    fn north_is_up_and_east_is_right() {
        // Distinct reliefs so each arm is identifiable by its own glyph
        // rather than by position alone.
        let mut s = compass_rose(["sensed"; 4]);
        for (c, relief) in s.cells[1..].iter_mut().zip([0, 2, 3, 5]) {
            c.relief = relief;
        }
        let out = render_surrounds_ascii(&s, "terrain", &[]);
        let at = find(&out, '@').expect("the observer is drawn");
        let north = find(&out, '_').expect("the north cell is drawn");
        let east = find(&out, '.').expect("the east cell is drawn");
        let south = find(&out, ':').expect("the south cell is drawn");
        let west = find(&out, 'A').expect("the west cell is drawn");
        assert_eq!(north.1, at.1, "north must share the observer's column");
        assert!(north.0 < at.0, "north must be ABOVE the observer: {out}");
        assert_eq!(south.1, at.1, "south must share the observer's column");
        assert!(south.0 > at.0, "south must be BELOW the observer: {out}");
        assert_eq!(east.0, at.0, "east must share the observer's row");
        assert!(east.1 > at.1, "east must be RIGHT of the observer: {out}");
        assert_eq!(west.0, at.0, "west must share the observer's row");
        assert!(west.1 < at.1, "west must be LEFT of the observer: {out}");
    }

    /// The 2x column factor, pinned on its own rather than left implicit in
    /// the picture above: a monospace cell is about twice as tall as it is
    /// wide, so the same angular distance east must cover twice the columns
    /// it covers rows going north. Drop the factor and the chart is an
    /// ellipse claiming to be a circle — and every other assertion in this
    /// file still passes.
    #[test]
    fn an_eastward_degree_covers_twice_the_columns_a_northward_one_covers_rows() {
        let (north_row, north_col) = project_north_up(0.0, 1.0, 1.0, 4);
        let (east_row, east_col) = project_north_up(90.0, 1.0, 1.0, 4);
        assert_eq!((north_row, north_col), (-4, 0));
        assert_eq!((east_row, east_col), (0, 8));
    }

    /// A band with nothing but the observer in it (radius 0) has no
    /// farthest cell to scale against. The projection must collapse to the
    /// origin rather than divide by zero and place a NaN.
    #[test]
    fn a_band_with_no_distance_collapses_to_the_origin() {
        assert_eq!(project_north_up(137.0, 0.0, 0.0, 1), (0, 0));
        let out = render_surrounds_ascii(&scene(vec![cell(0.0, 0.0, "here", 2)]), "terrain", &[]);
        assert_eq!(picture(&out), vec!["@".to_string()], "{out}");
    }

    #[test]
    fn a_remembered_cell_keeps_its_glyph_and_changes_only_its_weight() {
        // Build one scene twice, identical but for a cell's `state`
        // ("sensed" vs "remembered"). §2's rule: epistemic is a modulator
        // (weight), not a peer of the ordinal (glyph) channel, so a
        // remembered cell must draw the same glyph as a sensed one, only
        // dimmer — it must not substitute a different mark.
        //
        // Rendered through `colour`, not `terrain`: §2.3 (a client that
        // lacks a channel loses that channel's ENTIRE axis) means the
        // escape-free `terrain` lens must not distinguish the two states at
        // all — see `the_terrain_lens_declares_it_carries_no_weight_channel`
        // and `the_terrain_lens_emits_no_escape_sequences` for that half.
        // `colour` is the lens that actually carries the weight channel.
        let sensed = scene(vec![
            cell(0.0, 0.0, "here", 2),
            cell(90.0, 1.0, "sensed", 2),
        ]);
        let remembered = scene(vec![
            cell(0.0, 0.0, "here", 2),
            cell(90.0, 1.0, "remembered", 2),
        ]);
        let out_sensed = render_surrounds_ascii(&sensed, "colour", &[]);
        let out_remembered = render_surrounds_ascii(&remembered, "colour", &[]);

        // Clause 1: the glyph CHARACTER at that position is identical once
        // any weight escapes are stripped away. This pins the rule.
        assert_eq!(
            strip_escapes(&out_sensed),
            strip_escapes(&out_remembered),
            "a remembered cell must draw the same glyph as a sensed one, only \
             dimmer — sensed={out_sensed:?} remembered={out_remembered:?}"
        );

        // Clause 2: the raw renders must still differ — the weight moved
        // onto the remembered cell as a dim escape. Without this clause the
        // test would pass vacuously if the renderer stopped distinguishing
        // the two states at all.
        assert_ne!(
            out_sensed, out_remembered,
            "a remembered cell must still render differently from a sensed \
             one (dimmed), not identically: {out_sensed:?}"
        );
    }

    #[test]
    fn the_caption_names_the_lens_and_declares_the_orientation() {
        let s = scene(vec![cell(0.0, 0.0, "here", 2)]);
        let out = render_surrounds_ascii(&s, "terrain", &["E".to_string(), "Nw".to_string()]);
        let caption = out.lines().next().unwrap();
        assert!(caption.contains("lens: terrain"), "{caption}");
        assert!(caption.contains("north-up"), "{caption}");
        assert!(
            !caption.contains("lattice-aligned"),
            "the chart is no longer lattice-aligned and the caption must not \
             say it is: {caption}"
        );
        assert!(
            out.contains("ways on: E, Nw"),
            "the chart is oriented to north but the lattice underfoot is not, \
             so a reader who wants to WALK still needs the exits: {out}"
        );
    }

    #[test]
    fn the_caption_names_whichever_registered_lens_was_asked_for() {
        // Registry-driven rather than hardcoded, so this gains real teeth the
        // moment a second lens is registered: a caption that named a literal
        // would then lie about which lens produced the picture.
        let s = scene(vec![cell(0.0, 0.0, "here", 2)]);
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

    /// A seam cell — lattice offsets all `None`, because the lattice bends
    /// across a base face there — is DRAWN now, at the box its bearing and
    /// distance put it in. It used to be counted in a footer and omitted
    /// from the picture, which is what the footer's own wording admitted
    /// ("real ground, no honest place on this chart").
    ///
    /// Both halves are asserted: the mark lands where the projection says
    /// it should, AND the retired disclosure is gone. Without the second
    /// clause a renderer that drew the cell and *also* went on claiming it
    /// had nowhere to put it would pass.
    #[test]
    fn a_seam_cell_is_drawn_under_north_up() {
        let mut seam = cell(90.0, 1.0, "sensed", 2);
        seam.seam = true;
        seam.u = None;
        seam.v = None;
        seam.w = None;
        seam.up = None;
        seam.room = 999;
        let s = scene(vec![cell(0.0, 0.0, "here", 2), seam]);
        let out = render_surrounds_ascii(&s, "terrain", &[]);
        let at = find(&out, '@').expect("the observer is drawn");
        let drawn = find(&out, '.').expect("the seam cell must be drawn, not dropped");
        assert_eq!(drawn.0, at.0, "the seam cell is due east: same row: {out}");
        assert!(drawn.1 > at.1, "the seam cell is due east: to the right");
        assert!(
            !out.contains("beyond a face seam"),
            "the seam footer is retired — a seam cell has an honest place now: {out}"
        );
        assert!(
            out.contains("2 of 2 cells drawn"),
            "the placement disclosure must count the seam cell as drawn: {out}"
        );
    }

    /// The collision rule (see [`box_rank`]), on a FORCED collision: two
    /// cells at the identical bearing and distance cannot help landing in
    /// one box. Measured across ten observers x seven radii (70 real bands,
    /// 2,380 cells) the shipped projection collided zero times, so this is
    /// the guard's only coverage and it must not depend on a real band ever
    /// producing one — spec §6.4's "never trust unreachability".
    #[test]
    fn the_more_salient_of_two_colliding_cells_keeps_the_box() {
        fn marked(salience: u32, kind: &str) -> SurroundsCell {
            let mut c = cell(90.0, 1.0, "sensed", 2);
            c.marks = vec![Mark {
                noun: "x".to_string(),
                kind: kind.to_string(),
                datum: "d".to_string(),
                salience,
            }];
            c
        }
        // A settlement (`#`) at salience 20 and an agent (`&`) at 5, in
        // both document orders. `salience` is a RANK — lower is more
        // salient — so the AGENT wins both times. Asserting both orders is
        // what separates "the rule ran" from "the later insert won".
        for cells in [
            vec![
                cell(0.0, 0.0, "here", 2),
                marked(20, "settlement"),
                marked(5, "agent"),
            ],
            vec![
                cell(0.0, 0.0, "here", 2),
                marked(5, "agent"),
                marked(20, "settlement"),
            ],
        ] {
            let out = render_surrounds_ascii(&scene(cells), "terrain", &[]);
            assert!(
                find(&out, '&').is_some(),
                "the lower salience number is the MORE salient mark and must \
                 keep the box: {out}"
            );
            assert!(
                find(&out, '#').is_none(),
                "the less salient mark must have lost the box: {out}"
            );
            assert!(
                out.contains("2 of 3 cells drawn, 1 occluded"),
                "the caption must count the cell that lost its box: {out}"
            );
        }
    }

    /// An unmarked cell never takes a box from a marked one, whichever way
    /// round the document lists them — the second clause of [`box_rank`].
    #[test]
    fn a_marked_cell_outranks_an_unmarked_one_in_either_document_order() {
        let mut marked = cell(90.0, 1.0, "sensed", 2);
        marked.marks = vec![Mark {
            noun: "Ka".to_string(),
            kind: "settlement".to_string(),
            datum: "d".to_string(),
            salience: 40,
        }];
        for cells in [
            vec![
                cell(0.0, 0.0, "here", 2),
                marked.clone(),
                cell(90.0, 1.0, "sensed", 2),
            ],
            vec![
                cell(0.0, 0.0, "here", 2),
                cell(90.0, 1.0, "sensed", 2),
                marked.clone(),
            ],
        ] {
            let out = render_surrounds_ascii(&scene(cells), "terrain", &[]);
            assert!(
                find(&out, '#').is_some(),
                "the mark must keep the box: {out}"
            );
        }
    }

    /// The first clause of [`box_rank`]: the chart is egocentric (decision
    /// 0076), so the observer keeps their own box even against the most
    /// salient mark in the band. A chart that drew over `@` would have lost
    /// the one cell the reader is standing in.
    #[test]
    fn the_observer_never_loses_its_own_box() {
        let mut rival = cell(0.0, 0.0, "sensed", 2);
        rival.marks = vec![Mark {
            noun: "Ka".to_string(),
            kind: "settlement".to_string(),
            datum: "d".to_string(),
            salience: 0,
        }];
        let out = render_surrounds_ascii(
            &scene(vec![cell(0.0, 0.0, "here", 2), rival]),
            "terrain",
            &[],
        );
        assert_eq!(picture(&out), vec!["@".to_string()], "{out}");
    }

    /// **Salience ranks; weight inks.** The interaction spec §2 leaves open
    /// and this campaign had to settle: a `remembered` cell holding the
    /// flagship mark is bold-because-salient and dim-because-remembered at
    /// once. It wins its box on salience AND draws dim on epistemic —
    /// neither channel is allowed to become the other.
    ///
    /// Both halves are load-bearing. Without the first, weight would be
    /// silently acting as a rank (a remembered cell demoted out of the
    /// picture); without the second, salience would be silently acting as a
    /// weight (a salient cell drawn bright though it is only remembered).
    #[test]
    fn a_remembered_cell_holding_a_mark_wins_its_box_and_still_draws_dim() {
        let mut flagship = cell(90.0, 1.0, "remembered", 2);
        flagship.marks = vec![Mark {
            noun: "Dooga".to_string(),
            kind: "settlement".to_string(),
            datum: "the flagship".to_string(),
            salience: 5,
        }];
        flagship.color = Some([200, 30, 30]);
        let mut rival = cell(90.0, 1.0, "sensed", 2);
        rival.color = Some([30, 200, 30]);
        let out = render_surrounds_ascii(
            &scene(vec![cell(0.0, 0.0, "here", 2), flagship, rival]),
            "colour",
            &[],
        );
        assert!(
            strip_escapes(&out).contains('#'),
            "the remembered flagship must WIN its box on salience: {out}"
        );
        assert!(
            out.contains("\u{1b}[2m#"),
            "and must still draw DIM, because it is remembered: {out:?}"
        );
    }

    #[test]
    fn the_impedance_ladder_is_monotone_in_cost() {
        // Assert the ORDER, not the characters: for cells whose impedance
        // inputs are strictly ordered, the rendered glyph's rank in the
        // ladder is non-decreasing. Pinning specific glyphs would make this
        // test a second copy of the implementation.
        //
        // Exercised one axis at a time (relief, then canopy, then
        // roughness), each with the other two held at a neutral baseline,
        // rather than pinning the formula's exact weights — this only
        // relies on each axis being individually non-decreasing in
        // difficulty, which is the actual contract `impedance_glyph` makes.
        fn rank(glyph: char) -> usize {
            ['_', '.', ':', '^', 'A']
                .iter()
                .position(|&g| g == glyph)
                .unwrap_or_else(|| panic!("{glyph} is not a land-ladder rung"))
        }
        fn land_cell(relief: u32, openness: f64, micro_relief: f64) -> SurroundsCell {
            let mut c = cell(0.0, 0.0, "sensed", relief); // water: 3 (dry-land)
            c.micro.openness = openness;
            c.micro.relief = micro_relief;
            c
        }
        fn assert_non_decreasing(label: &str, glyphs: &[char]) {
            assert!(
                glyphs.windows(2).all(|w| rank(w[0]) <= rank(w[1])),
                "{label} axis must be non-decreasing in impedance: {glyphs:?}"
            );
        }
        let s = scene(vec![]);

        // Axis 1: relief worsens (0..=5), canopy fully open and ground flat.
        let by_relief: Vec<char> = (0..=5)
            .map(|r| terrain_glyph(&s, &land_cell(r, 1.0, 0.0)).0)
            .collect();
        assert_non_decreasing("relief", &by_relief);

        // Axis 2: canopy closes (open -> closed), relief and roughness held.
        let by_canopy: Vec<char> = [1.0, 0.5, 0.0, -0.5, -1.0]
            .iter()
            .map(|&o| terrain_glyph(&s, &land_cell(2, o, 0.0)).0)
            .collect();
        assert_non_decreasing("canopy", &by_canopy);
        // A positive control: "non-decreasing" alone is satisfied trivially
        // by a CONSTANT sequence, which is exactly what the pre-impedance,
        // relief-only ladder would produce here (it never read `micro` at
        // all). Pin that closing the canopy actually moves the glyph.
        assert_ne!(
            by_canopy.first(),
            by_canopy.last(),
            "closing the canopy from fully open to fully closed never moved \
             the glyph — the ladder is not reading micro.openness: {by_canopy:?}"
        );

        // Axis 3: ground roughens (|micro.relief| 0 -> 1), relief and
        // canopy held. Sign must not matter, only magnitude: a hollow and
        // a rise of the same magnitude must draw the same rung.
        let by_roughness: Vec<char> = [0.0, 0.3, 0.6, 0.9, 1.0]
            .iter()
            .map(|&m| terrain_glyph(&s, &land_cell(2, 1.0, m)).0)
            .collect();
        assert_non_decreasing("roughness", &by_roughness);
        assert_ne!(
            by_roughness.first(),
            by_roughness.last(),
            "roughening from flat to maximal never moved the glyph — the \
             ladder is not reading micro.relief: {by_roughness:?}"
        );
        assert_eq!(
            terrain_glyph(&s, &land_cell(2, 1.0, 1.0)).0,
            terrain_glyph(&s, &land_cell(2, 1.0, -1.0)).0,
            "a hollow and a rise of equal magnitude must draw the same rung"
        );
    }

    #[test]
    fn an_unknown_lens_is_refused_loudly() {
        let s = scene(vec![cell(0.0, 0.0, "here", 2)]);
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
            cell(0.0, 0.0, "here", 2),
            cell(90.0, 1.0, "sensed", 4),
        ]);
        assert_eq!(
            render_surrounds_ascii(&s, "terrain", &[]),
            render_surrounds_ascii(&s, "terrain", &[])
        );
    }

    #[test]
    fn the_colour_lens_is_registered() {
        assert!(SURROUNDS_LENSES.contains(&"colour"));
    }

    #[test]
    fn the_terrain_lens_emits_no_escape_sequences() {
        // The three committed gallery charts render through this lens, and
        // `windows/vessel/src/session.rs`'s `Eyes::Off` picks it specifically
        // because it promises "no observer, no tint, no escape sequence —
        // the same posture a screen reader takes." A remembered cell is
        // included deliberately: Task 3's first pass dimmed unconditionally,
        // which would have put an escape on exactly this path while this
        // test's fixture (`colored_test_scene()` alone, no remembered cell)
        // stayed green throughout. See the fix-round report for the RED-then-
        // GREEN proof this guard now actually catches that.
        let mut scene = colored_test_scene();
        scene.cells[1].state = "remembered".to_string();
        let out = render_surrounds_ascii(&scene, "terrain", &[]);
        assert!(
            !out.contains('\u{1b}'),
            "the terrain lens emitted an escape: {out}"
        );
    }

    #[test]
    fn the_terrain_lens_declares_it_carries_no_weight_channel() {
        // §2.3: a client that lacks a channel loses that channel's ENTIRE
        // axis and says so — it does not recover the axis some other way.
        // Pin both halves: the PICTURE is byte-identical for a sensed vs a
        // remembered cell (no per-cell distinction at all, not even a
        // fainter glyph — chart_body strips only the caption block), and
        // the caption states the loss with a checkable count rather than
        // staying silent about it or lying with a constant one.
        let sensed = scene(vec![
            cell(0.0, 0.0, "here", 2),
            cell(90.0, 1.0, "sensed", 2),
        ]);
        let remembered = scene(vec![
            cell(0.0, 0.0, "here", 2),
            cell(90.0, 1.0, "remembered", 2),
        ]);
        let out_sensed = render_surrounds_ascii(&sensed, "terrain", &[]);
        let out_remembered = render_surrounds_ascii(&remembered, "terrain", &[]);
        assert_eq!(
            chart_body(&out_sensed),
            chart_body(&out_remembered),
            "the terrain lens has no weight channel, so a remembered cell's \
             PICTURE must be identical to a sensed one's: \
             sensed={out_sensed:?} remembered={out_remembered:?}"
        );
        assert!(
            out_sensed.contains("epistemic: this lens carries no weight channel")
                && out_sensed.contains("0 of 2 placed"),
            "the sensed render's disclosure must count zero remembered: {out_sensed}"
        );
        assert!(
            out_remembered.contains("epistemic: this lens carries no weight channel")
                && out_remembered.contains("1 of 2 placed"),
            "the disclosure's count must be checkable against the scene, not \
             a constant sentence: {out_remembered}"
        );
    }

    #[test]
    fn the_colour_lens_emits_escapes_and_resets_them() {
        let scene = colored_test_scene();
        let out = render_surrounds_ascii(&scene, "colour", &[]);
        assert!(out.contains('\u{1b}'), "the colour lens emitted no escape");
        assert!(out.ends_with('\n'));
        // Every colour set must be followed by a reset before the string
        // ends, or the user's terminal stays tinted after the chart.
        let sets = out.matches("\u{1b}[38;2;").count();
        let resets = out.matches("\u{1b}[0m").count();
        assert_eq!(sets, resets, "{sets} colour sets but {resets} resets");
    }

    #[test]
    fn the_colour_lens_declares_the_chromatic_axis_lost_when_no_cell_carries_any_colour() {
        // §2.3: a client that lacks a channel loses that channel's ENTIRE
        // axis and DECLARES the loss — the counterpart to
        // `the_terrain_lens_declares_it_carries_no_weight_channel` for the
        // chromatic axis instead of the epistemic one. The counts line alone
        // ("0 tinted, 0 withheld, N carrying no colour") is not itself a
        // declaration — a reader has to notice the zeroes mean something —
        // so this pins that a SEPARATE, explicit sentence exists and that it
        // is absent whenever even one cell carries real colour (the positive
        // control: a coloured render must not also claim the axis is lost).
        let uncoloured = render_surrounds_ascii(&uncolored_test_scene(), "colour", &[]);
        let coloured = render_surrounds_ascii(&colored_test_scene(), "colour", &[]);
        const DECLARATION: &str = "colour: this chart carries no chromatic channel, so no \
             cell is tinted regardless of its surface cover";
        assert!(
            uncoloured.contains(DECLARATION),
            "an uncoloured render must declare the lost chromatic axis: {uncoloured}"
        );
        assert!(
            !coloured.contains(DECLARATION),
            "a render with real colour must not also claim the axis is lost: {coloured}"
        );
    }

    #[test]
    fn the_colour_lens_degrades_to_plain_glyphs_when_no_cell_has_a_colour() {
        // An uncoloured scene rendered through the colour lens must still
        // be readable rather than blank or escaped.
        let scene = uncolored_test_scene();
        let out = render_surrounds_ascii(&scene, "colour", &[]);
        assert!(
            !out.contains('\u{1b}'),
            "escapes emitted for an uncoloured scene"
        );
        assert!(
            out.contains("[lens: colour"),
            "the caption must still name the lens"
        );
    }

    #[test]
    fn the_two_lenses_draw_the_same_glyphs() {
        // Colour is a second channel over the same chart, not a different
        // chart. Stripping the escapes must recover the terrain render,
        // caption block aside — the caption is the one part that MUST
        // differ, since it names the lens and declares what colour did.
        let scene = colored_test_scene();
        let plain = render_surrounds_ascii(&scene, "terrain", &[]);
        let colored = render_surrounds_ascii(&scene, "colour", &[]);
        assert_eq!(chart_body(&strip_escapes(&colored)), chart_body(&plain));
    }

    #[test]
    fn the_colour_lens_withholds_the_surface_tint_from_water_a_mark_and_you() {
        // The colour a cell carries is the reflectance of its SURFACE cover.
        // Tinting a river glyph with the colour of the meadow under it
        // would have the picture assert something the reader cannot see —
        // precisely what RENDER-9's caption rule exists to prevent — and
        // water colour is a deferred registry row, so the honest move is
        // to withhold rather than to invent. The same reasoning covers a
        // mark (the glyph names a settlement, not the ground it stands on)
        // and the observer's own '@'.
        let mut s = uncolored_test_scene();
        s.cells[1].water = 2; // river
        let mut marked = cell(270.0, 1.0, "sensed", 3);
        marked.marks = vec![Mark {
            noun: "Ka".to_string(),
            kind: "settlement".to_string(),
            datum: "A settlement of this world.".to_string(),
            salience: 20,
        }];
        s.cells.push(marked);
        for c in s.cells.iter_mut() {
            c.color = Some([200, 30, 30]);
        }
        let out = render_surrounds_ascii(&s, "colour", &[]);
        // Four placed cells; only the dry-land, unmarked, non-observer one
        // is drawing the ground its colour describes.
        assert_eq!(
            out.matches("\u{1b}[38;2;").count(),
            1,
            "only a ground glyph may be tinted: {out}"
        );
        assert!(
            out.contains("1 tinted, 3 withheld"),
            "the caption must state what it withheld: {out}"
        );
        // The withheld glyphs are still drawn, just untinted.
        assert!(
            out.contains('+') && out.contains('@') && out.contains('#'),
            "{out}"
        );
    }

    #[test]
    fn the_colour_captions_counts_account_for_every_placed_cell() {
        // The caption is checkable only if its numbers add up to the chart
        // in front of the reader.
        let mut s = colored_test_scene();
        s.cells[2].color = None;
        let out = render_surrounds_ascii(&s, "colour", &[]);
        assert!(
            out.contains("1 tinted, 1 withheld"),
            "one ground cell tinted, the observer withheld: {out}"
        );
        assert!(
            out.contains("1 carrying no colour"),
            "the cell with no colour is its own category: {out}"
        );
    }

    /// Remove every CSI sequence from `s`.
    fn strip_escapes(s: &str) -> String {
        let mut out = String::new();
        let mut chars = s.chars();
        while let Some(c) = chars.next() {
            if c == '\u{1b}' {
                for c in chars.by_ref() {
                    if c == 'm' {
                        break;
                    }
                }
            } else {
                out.push(c);
            }
        }
        out
    }

    /// Everything but the caption block: the grid and the footers. The
    /// caption line opens with '[', the colour disclosure with `colour:`,
    /// and the terrain lens's epistemic disclosure with `epistemic:`; no
    /// grid row can begin with any of these, since every glyph is drawn
    /// from the terrain alphabet.
    fn chart_body(s: &str) -> String {
        s.lines()
            .filter(|l| {
                !l.starts_with('[')
                    && !l.trim_start().starts_with("colour:")
                    && !l.trim_start().starts_with("epistemic:")
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// Built on this module's own fixtures: `cell(u, v, w, up, state,
    /// relief)` and `scene(cells)`. `cell` sets `color: None`, so the
    /// coloured fixture assigns afterwards rather than changing that
    /// helper's signature — every existing test keeps compiling untouched.
    fn uncolored_test_scene() -> SurroundsScene {
        scene(vec![
            cell(0.0, 0.0, "here", 2),
            cell(90.0, 1.0, "sensed", 3),
            cell(180.0, 1.0, "sensed", 4),
        ])
    }

    fn colored_test_scene() -> SurroundsScene {
        let mut s = uncolored_test_scene();
        let palette = [[180, 90, 60], [120, 130, 110], [200, 190, 150]];
        for (cell, rgb) in s.cells.iter_mut().zip(palette) {
            cell.color = Some(rgb);
        }
        s
    }
}
