//! Stage-1 preregistered measurement (spec §7 of
//! `docs/superpowers/specs/2026-08-17-the-illumination-design.md`): H1 and
//! H3. H2 is contingent on §6.1's seasonal-observability finding and is not
//! measured here (Task 6's scope, per its brief, is H1 and H3 only).
//!
//! Step 0 guards the whole file: the H1 band is unrecoverable (it was
//! measured once, at `b0f20c71`, and can never be re-taken), so every other
//! test in this file is meaningless if the band's population silently
//! changed shape.
//!
//! **H3's scope, stated so nobody has to infer it from what is absent.**
//! Spec §7 preregisters H3 "for each of the three renderers." This file
//! measures exactly one of them: `render_surrounds_ascii`
//! (`windows/scene/src/surrounds_ascii.rs`), the sim's own ASCII lens.
//! `clients/vessel/src/pane_chart.ts` and `clients/game/core/src/chart.rs`
//! are **not** measured here — that is Task 12's territory (the client
//! surfaces), and measuring them now would duplicate that task's work. So:
//! **H3 is confirmed for the sim's ASCII renderer only.** Read every
//! "H3 CONFIRMED" in this campaign's reports with that qualifier attached;
//! it is not restated at every occurrence.

// The lexicon inventory's count for this file was RAISED by The Pavement's
// task-10 measurement (37 -> 57). The reason the guard asks for is recorded
// once, in `tests/common/mod.rs`'s module doc, which covers both files.
use crate::common;

use hornvale_scene::{
    Micro, RELIEF_LEGEND, Resolution, SURROUNDS_SCHEMA, SurroundsCell, SurroundsObserver,
    SurroundsScene, render_surrounds_ascii,
};
use std::collections::BTreeSet;

/// Step 0: the population guard. Must run — and be trusted — before any
/// colour count in this file is read, because both H1 arms are counted over
/// this band and a band that is not the whole purview counts the wrong
/// population on both sides at once.
///
/// **This guard was re-founded by The Pavement, and the number it used to
/// pin is gone on purpose.** It asserted `cells.len() == 31` with the
/// message "the H1 band changed shape … the bedrock baseline of 1 distinct
/// colour over 31 cells was measured at b0f20c71 and cannot be re-taken".
/// The band did change shape, and not silently: decisions
/// [0506] (the lattice is a cube-sphere) and [0511] (walk depth is
/// `globe_level + 7`) moved it from 31 triangles to 81 quads deliberately.
/// The 31-cell *value* is unrecoverable and stays unrecovered — H1's floor
/// is founded on a live arm now
/// ([`common::bedrock_colours`]), not on that literal.
///
/// **So this guard asserts a world-independent property instead of a
/// literal**, following the ruling The Pavement applied to the same shape of
/// failure in `clients/game`: stop stating the count and derive it from the
/// band's own radius. An 8-connected purview of radius `r` **is** a
/// `(2r+1)²` block, so a band that is one cell short has dropped a cell
/// somewhere and the two H1 arms are being counted over a population neither
/// of them describes.
///
/// **What it is blind to** (repo convention; decision [0491] is the record
/// that a stated blindness is stated in the harsh, accurate form rather than
/// implied away). It enforces one
/// direction — the band is the complete square purview its own `radius`
/// claims — and it is blind to three things. (1) **Position.** It says
/// nothing about *where* the band is; moving `common::baseline_band` to
/// another observer would keep this green while every colour count below
/// changed. (2) **The cube's eight corners.** A purview overlapping one of
/// them legitimately has fewer than `(2r+1)²` cells, because a corner cell
/// has seven neighbours and not eight (spec §2.2); this guard would call
/// that a defect. The flagship band is nowhere near a corner, so the case
/// does not arise here — but a future band that moves must re-read this,
/// not silence it. (3) **Content.** Cells could all be sea, all be ice, or
/// all carry `color: None`, and this stays green; the arms below are what
/// speak to that.
///
/// [0491]: `docs/decisions/0491-a-stated-blindness-gets-a-visible-ratchet-not-a-silent-fix.md`
/// [0506]: `docs/decisions/0506-the-occupancy-lattice-is-a-cube-sphere.md`
/// [0511]: `docs/decisions/0511-walk-depth-is-globe-level-plus-seven.md`
#[test]
fn the_h1_band_is_the_whole_eight_connected_purview_both_arms_are_counted_over() {
    let world = common::genesis();
    let scene = common::baseline_band(&world);
    let side = 2 * scene.radius as usize + 1;
    assert_eq!(
        scene.cells.len(),
        side * side,
        "the H1 band is not the complete 8-connected purview of its own \
         radius {}: {} cells against the {side}x{side} = {} an 8-connected \
         purview of that radius is. Both H1 arms below are counted over this \
         band, so a dropped cell miscounts both. This is NOT the old 31-cell \
         guard — that population is gone by decisions 0506/0511 and its \
         bedrock literal with it; see this test's doc comment.",
        scene.radius,
        scene.cells.len(),
        side * side,
    );
}

/// H1 (spec §7): the surface mixture increases the number of distinguishable
/// colours in the walk band, bounded above and below.
///
/// - Floor: strictly more distinct colours than the **bedrock arm** — the
///   same band, same world, same day, same observer and illuminant, with the
///   surface cover layer deleted. See [`common::bedrock_colours`].
/// - Ceiling: not every cell gets a unique colour. A bijection between cell
///   and colour would mean the mixture is tracking address noise, not
///   cover — a defect dressed as a success, not a win.
///
/// # The floor was re-founded, not re-pinned (The Pavement, task 10)
///
/// It used to read `distinct.len() > BEDROCK_BASELINE` with
/// `BEDROCK_BASELINE = 1`: a literal captured at commit `b0f20c71` over that
/// era's 31-cell band. The Pavement moved the band (decisions 0506, 0511),
/// and **measurement of the move settled that the new band is a different
/// region, not the old one sampled more densely** — 81 quads over ~121 km²
/// against 31 triangles over ~47 km², at per-cell areas within 2% of each
/// other. "The bedrock was uniform over the old patch" therefore carries
/// nothing to the new one.
///
/// **Leaving the literal in place would have been the worst option
/// available**, and it is worth naming why, because the test was GREEN with
/// it: the new region is 2.55x larger, so it is *more* likely to span a
/// second rock class, and `> 1` could then be satisfied by the band having
/// grown rather than by the surface mixture doing anything at all. A green
/// assertion meaning a claim nobody made.
///
/// What replaces it is stronger than the literal ever was, in the same sense
/// `windows/locale/tests/suite/wetness_reading.rs` records for its own
/// replaced witnesses: the bedrock arm is a **same-world differential over
/// the live function**, so it holds in every world and every band rather
/// than in the one a capture happened to freeze, and the two arms differ in
/// exactly one term. The historical figures are not restated as if current;
/// `b0f20c71`'s "1 distinct colour over 31 cells" remains a true statement
/// about that commit's band and lives in [`common::bedrock_colours`]'s doc
/// as history.
#[test]
fn h1_the_surface_mixture_increases_distinguishable_colours() {
    let world = common::genesis();
    let band = common::baseline_band(&world);
    let distinct: BTreeSet<Option<[u8; 3]>> = band.cells.iter().map(|c| c.color).collect();
    let bedrock = common::bedrock_colours(&world, &band);

    // Printed, not asserted: the two arms' cardinalities and how many cells
    // carry no colour at all. A reader of a green run should not have to
    // re-run the arms to learn how much margin the floor has, and the
    // withheld count is what says whether either arm is speaking about
    // colour at all (`to_srgb` returns `None` where the observer has no
    // truthful sRGB image for a cell).
    let withheld = band.cells.iter().filter(|c| c.color.is_none()).count();
    println!(
        "H1 (seed 42 flagship band, {} cells): mixture arm {} distinct, \
         bedrock arm {} distinct, {withheld} cells carrying no colour",
        band.cells.len(),
        distinct.len(),
        bedrock.len(),
    );

    assert!(
        distinct.len() > bedrock.len(),
        "H1 floor failed: the surface mixture yields {} distinct colours over \
         the {}-cell band and the bedrock arm — the same band with the cover \
         layer deleted — yields {}, so the mixture did not increase \
         distinguishable colours",
        distinct.len(),
        band.cells.len(),
        bedrock.len(),
    );
    assert_ne!(
        distinct.len(),
        band.cells.len(),
        "H1 ceiling failed: every one of the {} cells got its own colour ({} \
         distinct) — the mixture is tracking address noise, not cover",
        band.cells.len(),
        distinct.len()
    );
}

/// A rendered line that is caption or footer text, not a grid row — the
/// exclusion list `render_surrounds_ascii` documents for its own output
/// (`windows/scene/src/surrounds_ascii.rs`'s caption block and footers).
/// Mirrors that module's private `#[cfg(test)]` `chart_body` helper, which
/// an integration test cannot import; this is a minimal, deliberate
/// re-statement rather than a shared copy. Both of the `colour` lens's own
/// disclosure lines (the counts sentence and, when it fires, the explicit
/// axis-loss sentence) start with `"colour:"` once trimmed, so this one
/// prefix excludes both without needing a second entry.
fn is_meta_line(line: &str) -> bool {
    let trimmed = line.trim_start();
    line.starts_with('[')
        || trimmed.starts_with("colour:")
        || trimmed.starts_with("epistemic:")
        || trimmed.starts_with("sight:")
        || trimmed.starts_with("ways on:")
        || trimmed.starts_with("legend:")
        || trimmed.starts_with("placement:")
}

/// One rendered grid position's parsed content: the glyph drawn, and — if a
/// `\x1b[38;2;R;G;Bm` escape preceded it — that escape's raw code.
///
/// Handles a **stack** of leading escapes, not just one: a `remembered` cell
/// on the `colour` lens draws `dimmed(colored(glyph, rgb))`, which is
/// `\x1b[2m\x1b[38;2;r;g;bm<glyph>\x1b[0m\x1b[0m` — dim wrapped around a
/// colour-set, two escapes before the glyph and two resets after it. A
/// parser that consumes only one leading escape and then treats the next
/// character as the glyph reads the SECOND escape's own `\x1b` as if it were
/// the glyph, silently corrupting both the unit set and the glyph multiset
/// the moment a fixture includes a remembered, coloured cell (none in this
/// file do yet, but the next person to extend H3 to the epistemic axis on
/// this same lens will hit it without this).
struct Token {
    glyph: char,
    colour_code: Option<String>,
}

/// Parse one chart-grid line (already known not to be a caption/footer line)
/// into its placed [`Token`]s, in left-to-right order. Spaces (unplaced grid
/// positions) are skipped.
fn tokenize_grid_line(line: &str) -> Vec<Token> {
    let mut out = Vec::new();
    let mut chars = line.chars().peekable();
    while let Some(c) = chars.next() {
        if c == ' ' {
            continue;
        }
        if c != '\u{1b}' {
            out.push(Token {
                glyph: c,
                colour_code: None,
            });
            continue;
        }
        // One or more stacked leading escapes before the glyph.
        let mut colour_code: Option<String> = None;
        loop {
            let mut code = String::from("\u{1b}");
            for c2 in chars.by_ref() {
                code.push(c2);
                if c2 == 'm' {
                    break;
                }
            }
            if code.starts_with("\u{1b}[38;2;") {
                colour_code = Some(code);
            }
            if chars.peek() == Some(&'\u{1b}') {
                chars.next();
                continue;
            }
            break;
        }
        let Some(glyph) = chars.next() else { break };
        // Consume every trailing reset escape (one per leading escape).
        while chars.peek() == Some(&'\u{1b}') {
            chars.next();
            for c2 in chars.by_ref() {
                if c2 == 'm' {
                    break;
                }
            }
        }
        out.push(Token { glyph, colour_code });
    }
    out
}

/// The distinct visual units actually drawn in `rendered`'s chart grid: a
/// bare glyph is one unit, and an escape-tinted glyph is a *different* unit
/// per colour it carries — so two ground cells sharing a glyph but not a
/// tint count as two distinct renderings, and a `colour`-lens render of an
/// entirely uncoloured scene collapses to exactly its bare-glyph alphabet.
/// Operates on the rendered text itself, not on the builder's internal
/// `Placed` records — this is `render_surrounds_ascii`'s own published
/// surface, the same discipline this project's measurement notes call "use
/// the published predicate."
fn distinct_rendered_units(rendered: &str) -> BTreeSet<String> {
    let mut out = BTreeSet::new();
    for line in rendered.lines() {
        if is_meta_line(line) {
            continue;
        }
        for token in tokenize_grid_line(line) {
            let mut unit = token.colour_code.unwrap_or_default();
            unit.push(token.glyph);
            out.insert(unit);
        }
    }
    out
}

/// Every glyph character drawn in `rendered`'s chart grid, escapes and
/// spaces stripped, in reading order — the multiset the H3 tests pin as
/// unchanged between the coloured and monochrome renders (spec §2.3: a lost
/// channel is never recovered by reallocating a glyph).
fn glyph_sequence(rendered: &str) -> Vec<char> {
    let mut out = Vec::new();
    for line in rendered.lines() {
        if is_meta_line(line) {
            continue;
        }
        out.extend(tokenize_grid_line(line).into_iter().map(|t| t.glyph));
    }
    out
}

/// The declaration text `render_surrounds_ascii`'s `colour` lens emits when
/// (and only when) every placed cell in the chart carries no colour at all —
/// added in this fix round, spec §2.3's own axis-loss disclosure for the
/// chromatic axis, the counterpart to the terrain lens's `epistemic:`
/// sentence. Asserting against this literal string, rather than the
/// always-present counts line, is what makes clause 2 below able to fail: a
/// substring present in every `colour`-lens render (as the counts line is)
/// proves nothing about whether the loss was actually DECLARED.
const CHROMATIC_LOSS_DECLARATION: &str = "colour: this chart carries no chromatic channel, so no cell is tinted \
     regardless of its surface cover";

/// Assert all three H3 clauses hold between a coloured and a monochrome
/// render of "the same scene" (spec §7's own words): fewer distinct
/// renderings, the caption's explicit axis-loss declaration (present on the
/// monochrome side, ABSENT on the coloured side — the positive control that
/// keeps clause 2 from being satisfied by a sentence that is simply always
/// there), and an unchanged glyph multiset.
fn assert_h3_clauses(out_coloured: &str, out_mono: &str, mono_band_size: usize) {
    // Clause 1: strictly fewer distinct renderings once the chromatic axis
    // is gone.
    let coloured_units = distinct_rendered_units(out_coloured);
    let mono_units = distinct_rendered_units(out_mono);
    assert!(
        mono_units.len() < coloured_units.len(),
        "H3 clause 1 failed: the monochrome render had {} distinct rendered \
         units, not fewer than the coloured render's {} — \
         coloured={out_coloured:?}\nmono={out_mono:?}",
        mono_units.len(),
        coloured_units.len()
    );

    // Clause 2: the caption DECLARES the lost axis — not merely a counts
    // line a reader would have to interpret, and not a sentence that is
    // present regardless of whether anything was actually lost.
    assert!(
        out_mono.contains(CHROMATIC_LOSS_DECLARATION),
        "H3 clause 2 failed: the monochrome render did not carry the explicit \
         axis-loss declaration: {out_mono}"
    );
    assert!(
        !out_coloured.contains(CHROMATIC_LOSS_DECLARATION),
        "H3 clause 2's positive control failed: the COLOURED render also \
         claimed the chromatic axis was lost — the declaration is not \
         actually conditioned on anything: {out_coloured}"
    );
    // The counts line, checked against the population it is ACTUALLY about.
    //
    // **This compared against the wrong population until The Pavement, and
    // nothing could have caught it.** It read the whole radius-4 ball against
    // a caption number that `windows/scene/src/surrounds_ascii.rs` computes
    // from `placed.values()`, and whose own doc says the three counts
    // partition the PLACED ones. Two different quantities; one literal.
    //
    // They were indistinguishable because on the icosphere's triangular ball
    // **31 placed == 31 in the band**: nothing was ever occluded, so the two
    // candidate populations collapsed to a single value and no assertion
    // written over either could tell them apart. The epoch pulled them apart
    // (81 rooms in the ball, 67 placed, 14 occluded where two fell in one
    // character box) and the wrong one went red. This is the third instance
    // of that trap in this campaign — an input whose two possible meanings
    // agree at every value the test can reach — so it is worth naming rather
    // than just fixing.
    //
    // The band population stays load-bearing rather than being dropped: the
    // caption must still account for every room of the ball, `drawn +
    // occluded`, which is what would catch a room DROPPED for want of a
    // coordinate. Only the colour count is read against `drawn`.
    let number_before = |needle: &str| -> usize {
        let caption = out_mono
            .lines()
            .find(|l| l.contains("cells drawn")) // lexicon: the caption's own literal wording
            .unwrap_or_else(|| panic!("the chart must caption its own placement: {out_mono}"));
        let head = &caption[..caption
            .find(needle)
            .unwrap_or_else(|| panic!("caption has no `{needle}`: {caption}"))];
        head.trim_end()
            .rsplit(|c: char| !c.is_ascii_digit())
            .next()
            .and_then(|d| d.parse::<usize>().ok())
            .unwrap_or_else(|| panic!("no count before `{needle}`: {caption}"))
    };
    // lexicon: `cells drawn` is the caption's own literal wording, matched verbatim
    let drawn = number_before(&format!(" of {mono_band_size} cells drawn"));
    let occluded = number_before(" occluded");
    assert_eq!(
        drawn + occluded,
        mono_band_size,
        "H3 clause 2 failed: the caption does not account for every room of \
         the band ({drawn} drawn + {occluded} occluded against a population of \
         {mono_band_size}) — a room that is neither drawn nor occluded has \
         been dropped: {out_mono}"
    );
    let carrying_no_colour = format!("{drawn} carrying no colour");
    assert!(
        out_mono.contains(&carrying_no_colour),
        "H3 clause 2 failed: the caption's own colour count does not match \
         what it placed ({carrying_no_colour} expected, from {drawn} drawn of \
         {mono_band_size}): {out_mono}"
    );

    // Clause 3 — the load-bearing one: the glyph multiset is UNCHANGED
    // between the two renders. A lost axis must not be recovered by
    // reallocating a glyph.
    let mut coloured_glyphs = glyph_sequence(out_coloured);
    let mut mono_glyphs = glyph_sequence(out_mono);
    coloured_glyphs.sort_unstable();
    mono_glyphs.sort_unstable();
    assert_eq!(
        coloured_glyphs, mono_glyphs,
        "H3 clause 3 failed: the glyph multiset changed between the coloured \
         and monochrome renders — a lost channel was recovered by \
         reallocating a glyph, which spec §2.3 forbids"
    );
}

/// H3 (spec §7, §2.3), measured on a REAL seed-42 walk band — the primary
/// claim this campaign makes for H3, not the fixture below.
///
/// **Why not the flagship's own band, unlike H1 and Step 0.** That was tried
/// first and is a genuine null worth recording: per Task 1's own report, the
/// flagship band is "entirely within one canonical grid vertex (a tropical
/// rainforest river vertex)" — every one of its 31 cells resolves to
/// `water == "river"`, so `terrain_glyph` withholds tint from all of them
/// regardless of chromatic capability (RENDER-9's ground-only tinting rule).
/// The rendered picture is therefore identical coloured or not — an
/// excluded-mechanism scope error, not an H3 finding. A five-seed check (42,
/// 13, 7, 1, 100) found the SAME `0 tinted, 31 withheld` on every one — this
/// is a property of where this project sites its flagship settlement, not
/// of seed 42 specifically (task-6 report).
///
/// **Why a real band, not only the hand-built fixture below.** A 288-point
/// globe sweep on seed 42 (`illumination_probe.rs::h3_real_band_sweep`)
/// found dry-land bands are common (53 of 288 sampled) and the fixture's
/// defining configuration — two ground cells sharing a glyph rung but
/// carrying different colours — is the MODAL case among them, not a
/// contrivance: 53 of 53 seed-42 dry-land bands exhibit it (97 of 101 on
/// seed 13). So H3 rests on the world, not on a scene built for it.
/// `common::REAL_H3_BAND_LAT_LON` is that sweep's first qualifying point.
#[test]
fn h3_a_monochrome_observer_loses_the_nominal_axis_and_says_so() {
    let world = common::genesis();
    let (lat, lon) = common::REAL_H3_BAND_LAT_LON;
    let coloured_band = common::real_band(&world, lat, lon);
    let mono_band = common::real_band_uncolored(&world, lat, lon);

    let out_coloured = render_surrounds_ascii(&coloured_band, "colour", &[]);
    let out_mono = render_surrounds_ascii(&mono_band, "colour", &[]);
    assert_h3_clauses(&out_coloured, &out_mono, mono_band.cells.len());
}

/// One hand-built land or water cell for [`fixture_scene`]. `v`/`w`/`up`
/// place it (`render_surrounds_ascii`'s `row = -w; col = 2v + !up + w`); `u`
/// is unused by placement (same as the private fixtures in
/// `windows/scene/src/surrounds_ascii.rs`'s own `#[cfg(test)]` module) so it
/// is fixed at `0`. `openness`/`micro_relief` feed `impedance_glyph`'s
/// published formula (`surrounds_ascii.rs`'s own doc comment): canopy
/// `(1 - openness) / 2`, roughness `|micro_relief|`, so two cells with
/// identical `relief`/`openness`/`micro_relief` are guaranteed to draw the
/// identical glyph regardless of anything else about them.
#[allow(clippy::too_many_arguments)]
fn fixture_cell(
    room: u64,
    bearing_deg: f64,
    distance_rad: f64,
    state: &str,
    water: u32,
    relief: u32,
    openness: f64,
    micro_relief: f64,
    color: Option<[u8; 3]>,
) -> SurroundsCell {
    SurroundsCell {
        room,
        u: Some(0),
        v: Some(0),
        w: Some(0),
        up: Some(true),
        seam: false,
        state: state.to_string(),
        biome: 0,
        water,
        relief,
        regime: None,
        temperature_c: None,
        moisture: None,
        elevation_m: None,
        height_asl_m: None,
        color,
        micro: Micro {
            relief: micro_relief,
            aspect: 0.0,
            wetness: 0.0,
            openness,
        },
        marks: vec![],
        signal: None,
        cover: None,
        bearing_deg,
        distance_rad,
    }
}

/// A small hand-built band exercising every branch of the degradation rule
/// under test: an observer (`@`, always bare), two land cells with the same
/// glyph rung but (when `colored`) different colours, a third land cell at
/// a different rung, and a river cell (always bare — RENDER-9's ground-only
/// tinting rule withholds tint from it regardless of `colored`, the
/// positive control that pins the comparison to the CHROMATIC axis rather
/// than to "did any tint appear at all"). `colored` toggles every land
/// cell's `color` between `Some` and `None`; nothing else differs, so the
/// two scenes really are "the same scene" (spec §7's own words) minus the
/// chromatic channel.
fn fixture_scene(colored: bool) -> SurroundsScene {
    let tint = |rgb: [u8; 3]| colored.then_some(rgb);
    let cells = vec![
        // Observer: always bare regardless of colour or water.
        fixture_cell(1, 0.0, 0.0, "here", 3, 2, 1.0, 0.0, None),
        // Land, rung 2 ('.'), colour A.
        fixture_cell(2, 0.0, 1.0, "sensed", 3, 2, 1.0, 0.0, tint([200, 30, 30])),
        // Land, SAME rung 2 ('.'), colour B — the pair that makes colour add
        // distinguishability the glyph alone does not carry.
        fixture_cell(3, 90.0, 1.0, "sensed", 3, 2, 1.0, 0.0, tint([30, 200, 30])),
        // Land, rung 4 ('^'), colour A again (reused on purpose: distinctness
        // here must come from the glyph, not the colour).
        fixture_cell(4, 180.0, 1.0, "sensed", 3, 4, 1.0, 0.0, tint([200, 30, 30])),
        // River: never tinted, colour or not — the control.
        fixture_cell(5, 270.0, 1.0, "sensed", 2, 2, 1.0, 0.0, tint([10, 10, 10])),
    ];
    SurroundsScene {
        schema: SURROUNDS_SCHEMA.to_string(),
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
        relief_legend: RELIEF_LEGEND.iter().map(|s| s.to_string()).collect(),
        sea_level_m: 0.0,
        cells,
        legend: vec![],
        sight: None,
        resolution: Resolution {
            grid_level: 6,
            depth_below_grid: 6,
            grid_resolution_fields: ["biome", "water"].iter().map(|s| s.to_string()).collect(),
        },
        cover_legend: hornvale_locale::CoverClass::LEGEND
            .iter()
            .map(|s| s.to_string())
            .collect(),
    }
}

/// H3's CONTROLLED case, not its primary claim (see
/// [`h3_a_monochrome_observer_loses_the_nominal_axis_and_says_so`] for the
/// real-band measurement that is). This still earns its place after the
/// real-band sweep: it PINS the exact mechanism with every input fixed by
/// hand — a guaranteed same-glyph/different-colour pair, an off-rung glyph,
/// and a river control — where a real band's exact glyph/colour mix could
/// drift under a future terrain or colour-composition change without this
/// test's clauses ever telling you why. The real-band test is the evidence
/// that the mechanism this fixture exercises actually occurs in the world;
/// this test is the precise, minimal pin of what the mechanism IS.
#[test]
fn h3_control_fixture_pins_the_exact_degradation_mechanism() {
    let coloured_band = fixture_scene(true);
    let mono_band = fixture_scene(false);

    let out_coloured = render_surrounds_ascii(&coloured_band, "colour", &[]);
    let out_mono = render_surrounds_ascii(&mono_band, "colour", &[]);
    assert_h3_clauses(&out_coloured, &out_mono, mono_band.cells.len());
}
