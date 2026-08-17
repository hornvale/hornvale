//! The in-process ASCII render of a `scene/surrounds/v1` document — Ring 0/1
//! under decision 0022, the same class as the gallery's three ASCII maps. A
//! render is a registered LENS, never ground truth (RENDER-9), and the
//! caption — not the picture — carries the honesty: it names the lens, the
//! orientation, and everything the picture had to leave out.

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
    let mut placed: BTreeMap<(i64, i64), Placed> = BTreeMap::new();
    let mut seams = 0usize;
    for c in &scene.cells {
        let (Some(v), Some(w), Some(up)) = (c.v, c.w, c.up) else {
            seams += 1;
            continue;
        };
        let row = -w;
        let col = 2 * v + i64::from(!up) + w;
        let (g, ground) = terrain_glyph(scene, c);
        placed.insert(
            (row, col),
            Placed {
                glyph: g,
                color: c.color,
                ground,
                remembered: c.state == "remembered",
            },
        );
    }

    let mut out = String::new();
    // Interpolate `lens`, never a literal: the caption's whole job is to name
    // which lens you are wearing, so a hardcoded name would make every render
    // through a second lens caption a lie.
    out.push_str(&format!(
        "[lens: {lens} · depth {} · radius {} · lattice-aligned, not north-up]\n",
        scene.depth, scene.radius
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
    use crate::{Mark, Resolution, SurroundsCell, SurroundsObserver, SurroundsScene};

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
            height_asl_m: None,
            color: None,
            micro: crate::Micro {
                relief: 0.0,
                aspect: 0.0,
                wetness: 0.0,
                openness: 0.0,
            },
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
            sea_level_m: 0.0,
            cells,
            legend: vec![],
            sight: None,
            resolution: Resolution {
                grid_level: 6,
                depth_below_grid: 6,
                grid_resolution_fields: ["biome", "water"].iter().map(|s| s.to_string()).collect(),
            },
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
            cell(0, 0, 0, true, "here", 2),
            cell(-1, 0, 0, false, "sensed", 2),
        ]);
        let remembered = scene(vec![
            cell(0, 0, 0, true, "here", 2),
            cell(-1, 0, 0, false, "remembered", 2),
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
            let mut c = cell(0, 0, 0, true, "sensed", relief); // water: 3 (dry-land)
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
            cell(0, 0, 0, true, "here", 2),
            cell(-1, 0, 0, false, "sensed", 2),
        ]);
        let remembered = scene(vec![
            cell(0, 0, 0, true, "here", 2),
            cell(-1, 0, 0, false, "remembered", 2),
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
        let mut marked = cell(3, 2, 0, false, "sensed", 3);
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
            cell(0, 0, 0, true, "here", 2),
            cell(1, 0, 0, false, "sensed", 3),
            cell(0, 1, 0, false, "sensed", 4),
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
