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
/// with whether that glyph is drawing the **ground itself**.
///
/// The pairing is what keeps the `colour` lens honest. `SurroundsCell.color`
/// is the reflectance of the cell's *bedrock*, so it is a truthful claim
/// about the thing drawn only when the thing drawn is that ground. Where the
/// glyph has been overridden to name something else — the observer, a mark
/// standing on the cell, or the water covering it — the bedrock colour
/// describes something the reader cannot see, and the `colour` lens withholds
/// it rather than tinting a river with the colour of the rock beneath it.
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
        _ => (
            match cell.relief {
                0 | 1 => '_',
                2 => '.',
                3 => ':',
                4 => '^',
                _ => 'A',
            },
            true,
        ),
    }
}

/// One placed glyph: what to draw, the colour its cell carries (if any), and
/// whether the glyph is drawing the ground that colour describes.
struct Placed {
    /// The character drawn at this position, already faded if remembered.
    glyph: char,
    /// The cell's `color`, straight from the document; `None` when the
    /// scene was built through an uncoloured path.
    color: Option<[u8; 3]>,
    /// Whether `glyph` draws the bedrock `color` describes — see
    /// [`terrain_glyph`].
    ground: bool,
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
        let g = if c.state == "remembered" { faded(g) } else { g };
        placed.insert(
            (row, col),
            Placed {
                glyph: g,
                color: c.color,
                ground,
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
    // rather than a footnote: the tint is BEDROCK, and the chart draws plenty
    // of glyphs that are not bedrock. Rather than let the picture claim a
    // river is granite-coloured and retract it underneath, the lens withholds
    // the tint from every non-ground glyph and says how many it withheld. The
    // three counts partition the placed cells, so a reader can check the
    // sentence against the picture instead of trusting it.
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
            "  colour: each cell's bedrock, tinted only where the glyph draws that ground — \
             {tinted} tinted, {withheld} withheld (water, a mark, or you), \
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
                        match (lens, p.color, p.ground) {
                            ("colour", Some(rgb), true) => line.push_str(&colored(p.glyph, rgb)),
                            _ => line.push(p.glyph),
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
    use crate::{Mark, SurroundsCell, SurroundsObserver, SurroundsScene};

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

    #[test]
    fn the_colour_lens_is_registered() {
        assert!(SURROUNDS_LENSES.contains(&"colour"));
    }

    #[test]
    fn the_terrain_lens_emits_no_escape_sequences() {
        // The three committed gallery charts render through this lens.
        // An escape here moves all of them.
        let scene = colored_test_scene();
        let out = render_surrounds_ascii(&scene, "terrain", &[]);
        assert!(
            !out.contains('\u{1b}'),
            "the terrain lens emitted an escape"
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
    fn the_colour_lens_withholds_the_bedrock_tint_from_water_a_mark_and_you() {
        // The colour a cell carries is the reflectance of its BEDROCK.
        // Tinting a river glyph with the colour of the granite under it
        // would have the picture assert something the reader cannot see —
        // precisely what RENDER-9's caption rule exists to prevent — and
        // water colour is a deferred registry row, so the honest move is
        // to withhold rather than to invent. The same reasoning covers a
        // mark (the glyph names a settlement, not the rock it stands on)
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
    /// caption line opens with '[' and the colour disclosure with `colour:`;
    /// no grid row can begin with either, since every glyph is drawn from
    /// the terrain alphabet.
    fn chart_body(s: &str) -> String {
        s.lines()
            .filter(|l| !l.starts_with('[') && !l.trim_start().starts_with("colour:"))
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
