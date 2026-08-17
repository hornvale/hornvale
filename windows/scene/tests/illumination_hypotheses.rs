//! Stage-1 preregistered measurement (spec §7 of
//! `docs/superpowers/specs/2026-08-17-the-illumination-design.md`): H1 and
//! H3. H2 is contingent on §6.1's seasonal-observability finding and is not
//! measured here (Task 6's scope, per its brief, is H1 and H3 only).
//!
//! Step 0 guards the whole file: the H1 band is unrecoverable (it was
//! measured once, at `b0f20c71`, and can never be re-taken), so every other
//! test in this file is meaningless if the band's population silently
//! changed shape.

mod common;

use hornvale_scene::{
    Micro, RELIEF_LEGEND, Resolution, SURROUNDS_SCHEMA, SurroundsCell, SurroundsObserver,
    SurroundsScene, render_surrounds_ascii,
};
use std::collections::BTreeSet;

/// Step 0: the population guard. Must run — and be trusted — before any
/// colour count in this file is compared against Task 1's bedrock baseline.
#[test]
fn the_h1_band_is_still_the_population_the_baseline_was_taken_over() {
    let world = common::genesis();
    let scene = common::baseline_band(&world);
    assert_eq!(
        scene.cells.len(),
        31,
        "the H1 band changed shape; the bedrock baseline of 1 distinct \
         colour over 31 cells was measured at b0f20c71 and cannot be \
         re-taken. Do not compare colour counts until this is explained."
    );
}

/// H1 (spec §7): the surface mixture increases the number of distinguishable
/// colours in the walk band, bounded above and below.
///
/// - Floor: strictly more distinct colours than the bedrock baseline.
///   `BEDROCK_BASELINE` is the number Task 1's probe printed
///   (`illumination_probe.rs`'s `§7/H1` block) at commit `b0f20c71`, over
///   this exact band (`common::baseline_band`) — 1 distinct colour across
///   31 cells.
/// - Ceiling: not every cell gets a unique colour. A bijection between cell
///   and colour would mean the mixture is tracking address noise, not
///   cover — a defect dressed as a success, not a win.
#[test]
fn h1_the_surface_mixture_increases_distinguishable_colours() {
    /// Task 1's probe, `b0f20c71`: "distinct color count = 1" over this
    /// same 31-cell band. Never re-measurable — see this module's Step 0.
    const BEDROCK_BASELINE: usize = 1;

    let world = common::genesis();
    let band = common::baseline_band(&world);
    let distinct: BTreeSet<Option<[u8; 3]>> = band.cells.iter().map(|c| c.color).collect();

    assert!(
        distinct.len() > BEDROCK_BASELINE,
        "H1 floor failed: distinct colour count {} did not exceed the bedrock \
         baseline of {BEDROCK_BASELINE} (b0f20c71) — the surface mixture did not \
         increase distinguishable colours over the H1 band",
        distinct.len()
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
/// re-statement rather than a shared copy.
fn is_meta_line(line: &str) -> bool {
    let trimmed = line.trim_start();
    line.starts_with('[')
        || trimmed.starts_with("colour:")
        || trimmed.starts_with("epistemic:")
        || trimmed.starts_with("sight:")
        || trimmed.starts_with("ways on:")
        || trimmed.starts_with("legend:")
        || line.contains("beyond a face seam")
}

/// The distinct visual units actually drawn in `rendered`'s chart grid: a
/// bare glyph is one unit, and an escape-tinted glyph is a *different* unit
/// per colour it carries — so two ground cells sharing a glyph but not a
/// tint count as two distinct renderings, and a `colour`-lens render of an
/// entirely uncoloured scene collapses to exactly its bare-glyph alphabet.
/// Spaces (unplaced grid positions) are excluded. Operates on the rendered
/// text itself, not on the builder's internal `Placed` records — this is
/// `render_surrounds_ascii`'s own published surface, the same discipline
/// this project's measurement notes call "use the published predicate."
fn distinct_rendered_units(rendered: &str) -> BTreeSet<String> {
    let mut out = BTreeSet::new();
    for line in rendered.lines() {
        if is_meta_line(line) {
            continue;
        }
        let mut chars = line.chars().peekable();
        while let Some(c) = chars.next() {
            if c == '\u{1b}' {
                // The colour-set escape: "\x1b[38;2;R;G;Bm".
                let mut code = String::from("\u{1b}");
                for c2 in chars.by_ref() {
                    code.push(c2);
                    if c2 == 'm' {
                        break;
                    }
                }
                if let Some(glyph) = chars.next() {
                    // Consume the trailing reset escape "\x1b[0m".
                    for c2 in chars.by_ref() {
                        if c2 == 'm' {
                            break;
                        }
                    }
                    let mut unit = code;
                    unit.push(glyph);
                    out.insert(unit);
                }
            } else if c != ' ' {
                out.insert(c.to_string());
            }
        }
    }
    out
}

/// Every glyph character drawn in `rendered`'s chart grid, escapes and
/// spaces stripped, in reading order — the multiset
/// [`h3_a_monochrome_observer_loses_the_nominal_axis_and_says_so`] pins as
/// unchanged between the coloured and monochrome renders (spec §2.3: a lost
/// channel is never recovered by reallocating a glyph).
fn glyph_sequence(rendered: &str) -> Vec<char> {
    let mut out = Vec::new();
    for line in rendered.lines() {
        if is_meta_line(line) {
            continue;
        }
        let mut chars = line.chars().peekable();
        while let Some(c) = chars.next() {
            if c == '\u{1b}' {
                for c2 in chars.by_ref() {
                    if c2 == 'm' {
                        break;
                    }
                }
                if let Some(glyph) = chars.next() {
                    out.push(glyph);
                    for c2 in chars.by_ref() {
                        if c2 == 'm' {
                            break;
                        }
                    }
                }
            } else if c != ' ' {
                out.push(c);
            }
        }
    }
    out
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
    v: i64,
    w: i64,
    up: bool,
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
        v: Some(v),
        w: Some(w),
        up: Some(up),
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
/// two scenes really are "the same scene" (this test's H3 pseudocode's own
/// words) minus the chromatic channel.
fn fixture_scene(colored: bool) -> SurroundsScene {
    let tint = |rgb: [u8; 3]| colored.then_some(rgb);
    let cells = vec![
        // Observer: always bare regardless of colour or water.
        fixture_cell(1, 0, 0, true, "here", 3, 2, 1.0, 0.0, None),
        // Land, rung 2 ('.'), colour A.
        fixture_cell(
            2,
            1,
            0,
            false,
            "sensed",
            3,
            2,
            1.0,
            0.0,
            tint([200, 30, 30]),
        ),
        // Land, SAME rung 2 ('.'), colour B — the pair that makes colour add
        // distinguishability the glyph alone does not carry.
        fixture_cell(
            3,
            -1,
            0,
            false,
            "sensed",
            3,
            2,
            1.0,
            0.0,
            tint([30, 200, 30]),
        ),
        // Land, rung 4 ('^'), colour A again (reused on purpose: distinctness
        // here must come from the glyph, not the colour).
        fixture_cell(
            4,
            0,
            1,
            false,
            "sensed",
            3,
            4,
            1.0,
            0.0,
            tint([200, 30, 30]),
        ),
        // River: never tinted, colour or not — the control.
        fixture_cell(
            5,
            0,
            -1,
            false,
            "sensed",
            2,
            2,
            1.0,
            0.0,
            tint([10, 10, 10]),
        ),
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
        orientation: "lattice".to_string(),
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
    }
}

/// H3 (spec §7, §2.3): a client that lacks a channel loses that channel's
/// entire axis and declares the loss in its caption — it does not recover
/// the axis by reallocating another one.
///
/// **Why this test does not reuse `common::baseline_band`, unlike H1 and
/// Step 0.** It was tried first, and it is a genuine null for a reason
/// worth recording rather than silently routing around: the H1 band is, per
/// Task 1's own report, "entirely within one canonical grid cell (a
/// tropical rainforest river cell)" — every one of its 31 cells resolves to
/// `water == "river"`, so `terrain_glyph` withholds tint from all of them
/// regardless of the observer's chromatic capability (RENDER-9's
/// ground-only tinting rule). The rendered PICTURE is therefore byte-for-
/// byte identical whether the band is coloured or not — H3's clause 1
/// (fewer distinct renderings) is unmeasurable on that band, an "excluded
/// mechanism" scope error, not a finding about the degradation rule. H3 is
/// about `render_surrounds_ascii`'s own contract, not about whether seed
/// 42's flagship happens to overlook dry ground, so this test measures it
/// directly: a hand-built scene with real land cells (the same
/// `SurroundsCell`/`SurroundsScene` public constructors
/// `windows/scene/src/surrounds_ascii.rs`'s own `#[cfg(test)]` fixtures use,
/// duplicated here for the same reason `common::baseline_band` duplicates
/// the probe — those fixtures are private to that module).
///
/// **Why colour is assigned directly rather than composed from a real
/// `Observer`.** H3 is the render's DEGRADATION rule, not the surface
/// mixture's composition (H1's claim) — `hornvale_kernel::color::Observer`
/// also has no constructor for a true zero-chromatic eye
/// (`Observer::with_roles` refuses a role set with no `Chromatic` channel),
/// so "an observer with no chromatic channel" is realized the same way it
/// is everywhere else in this codebase: no colour on the cell at all
/// (`color: None`), which is what `render_surrounds_ascii`'s `colour` lens
/// already treats as the no-chromatic-information case (see
/// `the_colour_lens_degrades_to_plain_glyphs_when_no_cell_has_a_colour` in
/// that module).
#[test]
fn h3_a_monochrome_observer_loses_the_nominal_axis_and_says_so() {
    // Two land cells that draw the SAME glyph (identical relief/micro
    // inputs to `impedance_glyph`'s formula) but carry DIFFERENT colours —
    // the pair that makes the coloured render strictly more distinguishable
    // than the monochrome one. A third land cell at a different relief band
    // draws a different glyph, and a river cell is never tinted regardless
    // of colour (the ground-only rule), so it is a control that must render
    // identically either way.
    let coloured_band = fixture_scene(true);
    let mono_band = fixture_scene(false);

    let out_coloured = render_surrounds_ascii(&coloured_band, "colour", &[]);
    let out_mono = render_surrounds_ascii(&mono_band, "colour", &[]);

    // Clause 1: strictly fewer distinct renderings once the chromatic axis
    // is gone.
    let coloured_units = distinct_rendered_units(&out_coloured);
    let mono_units = distinct_rendered_units(&out_mono);
    assert!(
        mono_units.len() < coloured_units.len(),
        "H3 clause 1 failed: the monochrome render had {} distinct rendered \
         units, not fewer than the coloured render's {} — \
         coloured={out_coloured:?}\nmono={out_mono:?}",
        mono_units.len(),
        coloured_units.len()
    );

    // Clause 2: the caption names the lost axis, and states a checkable
    // count against the picture (the same discipline the `colour` lens's
    // withheld-tint disclosure already carries for ground/water/marks).
    assert!(
        out_mono.contains("colour:") && out_mono.contains("0 tinted, 0 withheld"),
        "H3 clause 2 failed: the monochrome caption did not declare zero \
         tinted, zero withheld — it does not name the lost chromatic axis: \
         {out_mono}"
    );
    let carrying_no_colour = format!("{} carrying no colour", mono_band.cells.len());
    assert!(
        out_mono.contains(&carrying_no_colour),
        "H3 clause 2 failed: the caption's own count does not match the \
         band's population ({carrying_no_colour} expected): {out_mono}"
    );

    // Clause 3 — the load-bearing one: the glyph multiset is UNCHANGED
    // between the two renders. A lost axis must not be recovered by
    // reallocating a glyph.
    let mut coloured_glyphs = glyph_sequence(&out_coloured);
    let mut mono_glyphs = glyph_sequence(&out_mono);
    coloured_glyphs.sort_unstable();
    mono_glyphs.sort_unstable();
    assert_eq!(
        coloured_glyphs, mono_glyphs,
        "H3 clause 3 failed: the glyph multiset changed between the coloured \
         and monochrome renders — a lost channel was recovered by \
         reallocating a glyph, which spec §2.3 forbids"
    );
}
