use hornvale_game_core::{Cell, CommandLine, Focus, Grid, Source, Weight, render, render_with};

const FIXTURE: &str = include_str!("fixtures/session-seed-42-turn-0.json");

/// The world plate is composed here, not in `chart.rs`/`plan.rs`'s own
/// tests, because `compose` lives in `spread.rs` — this crate has no
/// `tests/suite.rs` (the workspace's test-binary consolidation does not
/// reach `clients/game`, which is outside the cargo workspace), so this
/// file is that home.
///
/// **Deviates from the task brief's own draft of this test at one point,
/// found by running it, not by reasoning about it.** The brief's version
/// checked cell `(0, 0)` for `Source::Chart` in the `without` render; for
/// this fixture that cell is `Unattributed` — `chart::draw`'s own module
/// doc states the observer's box is anchored to the plate's CENTRE, not
/// its corner ("lands on (0, 0) [relative to the observer] ... anchors it
/// to the grid centre directly"), and which relative boxes are non-blank
/// around it depends on the fixture's own chart cells, which the corner
/// is not guaranteed to be among. `(20, 10)` — the plate's own centre at
/// 40x20, where the observer's `@` always lands by construction — is used
/// instead, so this test does not depend on incidental fixture content.
///
/// **Fix round 1: this test did not discriminate the behaviour it
/// claimed.** The synthetic plate below has exactly one non-blank cell,
/// and `blit` skips blank source cells — so a hypothetical MERGE bug
/// (`compose` drawing the band's chart/plan first and then blitting the
/// world plate on top, rather than choosing one or the other) would leave
/// the chart drawn everywhere the synthetic plate is blank, and `(20,
/// 10)` alone would read `World` either way: the original two assertions
/// passed under both the correct either/or and the merge bug. The fix
/// adds a SECOND probe cell, `(20, 6)`, which the synthetic plate leaves
/// blank and which the real `without` render draws as `Source::Chart`
/// (found by probing the real render — not guessed): under the correct
/// either/or, `(20, 6)` is `Unattributed` in `with` (the chart was never
/// drawn at all, so there is nothing there for `blit` to skip past);
/// under the merge bug, `(20, 6)` stays `Chart` in `with` (drawn first,
/// then not overwritten because the synthetic plate is blank there). This
/// was verified to actually discriminate — see the task report's "fix
/// round 1" section for the red/green mutation proof.
///
/// **THE PROBE IS NOW ASKED FOR, NOT WRITTEN DOWN (The Pavement, 2026-09-01),
/// AND THE PARAGRAPH ABOVE IS WHY IT HAD TO BE.** `(20, 6)` was a real
/// coordinate found by probing a real render, and it stopped being one: the
/// epoch moved the walk band from `globe_level + 6` to `+ 7` and the
/// occupancy lattice onto a cube-sphere, so the chart this fixture draws is a
/// dense 9x9 block where it was a sparse 31-room ring, and `(20, 6)` reads
/// `Unattributed` in `without` — the test failed on its PRECONDITION, before
/// reaching the behaviour it exists to check.
///
/// A coordinate that satisfies a property is not the same thing as the
/// property, and this file now asks for the property: scan the plate region
/// in row-major order for the first box that is `Source::Chart` in `without`
/// and is not the one the synthetic plate fills, and use that. Same
/// discrimination, same two assertions, no fixture-dependent literal — and it
/// panics loudly if no such box exists at all, because "the chart drew
/// nothing outside the plate's one filled box" would make the check vacuous
/// rather than passing.
#[test]
fn a_supplied_world_plate_replaces_the_band_view_and_nothing_else() {
    let plate = {
        let mut g = Grid::new(40, 20);
        g.set(20, 10, Cell::glyph('#', Weight::Normal, Source::World));
        g
    };
    let (with, _) = render_with(
        FIXTURE,
        80,
        24,
        Focus::Map,
        None,
        CommandLine::default(),
        None,
        None,
        Some(&plate),
        0,
        None,
    )
    .unwrap();
    let (without, _) = render_with(
        FIXTURE,
        80,
        24,
        Focus::Map,
        None,
        CommandLine::default(),
        None,
        None,
        None,
        0,
        None,
    )
    .unwrap();
    assert_eq!(with.get(20, 10).unwrap().source, Source::World);
    assert_eq!(without.get(20, 10).unwrap().source, Source::Chart);
    // The discriminating probe, ASKED FOR rather than written down (see the
    // doc above): the first box of the plate region that the real `without`
    // render draws as `Chart` and that the synthetic plate leaves blank. A
    // merge bug (draw chart, then blit world on top) would leave such a box
    // `Chart` in `with` too, because `blit` skips blank sources. The correct
    // either/or leaves it `Unattributed`: the chart was never drawn into
    // `with`'s plate at all.
    let (px, py) = (0..20u16)
        .flat_map(|y| (0..40u16).map(move |x| (x, y)))
        .find(|&(x, y)| {
            (x, y) != (20, 10) && without.get(x, y).is_some_and(|c| c.source == Source::Chart)
        })
        .expect(
            "no box of the plate region is `Chart` in the chart-only render outside the one \
             the synthetic plate fills — the probe below would be vacuous, so this is a \
             finding about the fixture's chart, not a passing test",
        );
    assert_eq!(without.get(px, py).unwrap().source, Source::Chart);
    assert_eq!(
        with.get(px, py).unwrap().source,
        Source::Unattributed,
        "the band's chart must not be drawn at all when a world plate is \
         supplied — a box the synthetic plate leaves blank ({px}, {py}) must \
         stay blank in `with`, not fall through to the chart underneath"
    );
    // The entry pane is untouched by the lens.
    for y in 0..24 {
        for x in 40..80 {
            assert_eq!(
                with.get(x, y),
                without.get(x, y),
                "the lens must not reach past the plate at ({x},{y})"
            );
        }
    }
}

/// A solid `Source::World` plate, big enough that it is never the thing
/// limiting the pane's width — so a probe that stops finding `World` has
/// found the PAGE's own plate region ending, not the supplied grid's.
///
/// **Solid, not sparse, and that is load-bearing** — the same lesson two
/// tests in this file already record: `blit` copies only non-blank SOURCE
/// squares, so a plate with one marked square can never populate a whole
/// row and every column probe below it would read blank for the wrong
/// reason.
fn solid_world_plate(w: u16, h: u16) -> Grid {
    let mut g = Grid::new(w, h);
    for y in 0..g.height() {
        for x in 0..g.width() {
            g.set(x, y, Cell::glyph('#', Weight::Normal, Source::World));
        }
    }
    g
}

/// Compose a `w`-by-`h` spread in `focus` with a solid world plate
/// supplied, and MEASURE the plate region's real column count off the
/// composed page: the run of `Source::World` squares from column 0 along a
/// mid-content row. Measured, never recomputed from
/// `spread::world_plate_width` — a test that restated the formula would
/// agree with any formula, including a wrong one.
fn plate_column_count(w: u16, h: u16, focus: Focus) -> u16 {
    let plate = solid_world_plate(w, h);
    let (g, _) = render_with(
        FIXTURE,
        w,
        h,
        focus,
        None,
        CommandLine::default(),
        None,
        None,
        Some(&plate),
        0,
        None,
    )
    .unwrap();
    let row = hornvale_game_core::spread::content_height(h) / 2;
    let mut n = 0u16;
    while n < w && g.get(n, row).is_some_and(|c| c.source == Source::World) {
        n += 1;
    }
    n
}

/// THE QUADRAT, TASK 9 — bound one of two: the map is at least half the
/// terminal.
///
/// **The sweep is what makes this discriminate.** At 80x24 the RETIRED
/// fixed 40-column plate was already exactly half, so a single-size test
/// here would pass against the very code this task replaces. Every wider
/// size in the sweep fails under that fixed width, and 200x50 additionally
/// fails under the pre-Task-9 fit (`GLYPH_ASPECT * content_height(50)` is
/// 92 columns, 46%) — so the sweep separates all three rules, not just two.
#[test]
fn the_plate_claims_at_least_half_the_terminal() {
    for (w, h) in [(80u16, 24u16), (104, 56), (120, 40), (200, 50), (300, 80)] {
        let cols = plate_column_count(w, h, Focus::Walk);
        assert!(
            cols * 2 >= w,
            "at {w}x{h} the plate claimed {cols} of {w} columns, under half"
        );
    }
}

/// THE QUADRAT, TASK 9 — bound two of two: the entry pane stays legible.
///
/// The direction a bare "at least half" rule breaks. A fraction has only
/// one bound, so a rule stated as a fraction alone would go on taking half
/// of a shrinking terminal until the prose had nowhere to go. 104x56 is the
/// size in this sweep where the ceiling actually BINDS — the square-footprint
/// preference wants all 104 columns there and this rule holds it to 64 — so
/// the assertion is not merely satisfied by the floor's own arithmetic.
#[test]
fn the_entry_pane_keeps_a_legible_minimum() {
    for (w, h) in [(80u16, 24u16), (104, 56), (120, 40), (200, 50), (300, 80)] {
        let entry = w - plate_column_count(w, h, Focus::Walk);
        assert!(
            entry >= hornvale_game_core::spread::MIN_ENTRY_WIDTH,
            "at {w}x{h} the entry pane got {entry} columns, under the legible minimum"
        );
    }
}

/// THE QUADRAT, TASK 9, FIX ROUND 1 — the square-footprint PREFERENCE,
/// pinned by an equality, because the two bound tests above cannot see it.
///
/// **Two behaviours were one assertion away from each other.** Delete the
/// preference from `world_plate_width` and every other test in this file and
/// in `bin/tests/driver.rs` stays green, while 120x40 drops 72 -> 60 and
/// 104x56 drops 64 -> 52. Worse, and this is the part that makes it a real
/// hole rather than an unpinned nicety: with the preference gone the rule is
/// exactly `ceil(w/2)`, so `entry = floor(w/2) >= MIN_ENTRY_WIDTH` holds for
/// every `w >= 80` by arithmetic alone — which makes
/// `the_entry_pane_keeps_a_legible_minimum` satisfiable without the ceiling,
/// so the "ceiling dropped" mutation this task's own report records as
/// CAUGHT would stop being caught. An unpinned behaviour was quietly
/// propping up a pinned one.
///
/// 120x40 is the size that states it cleanly: the floor wants 60, the
/// ceiling permits 80, and the preference is the only thing that says 72.
/// Measured off the page like every other width assertion here.
#[test]
fn the_square_footprint_preference_decides_the_width_when_it_can_afford_to() {
    let (w, h) = (120u16, 40u16);
    assert_eq!(
        plate_column_count(w, h, Focus::Walk),
        72,
        "at {w}x{h} the plate must be the square screen footprint \
         (GLYPH_ASPECT * content_height({h}) = 72), not the {} the half-the-\
         terminal floor alone would give",
        w.div_ceil(2)
    );
}

/// THE QUADRAT, TASK 9: **a supplied plate widens the pane, in every
/// focus** — replacing `the_world_plate_uses_the_width_only_while_the_map_
/// is_focused`, whose subject this task deletes rather than moves.
///
/// That test pinned The Portolan part II's Task 3a rule: the pane widened
/// only under `Focus::Map`, and a wide plate supplied in any other focus
/// was clipped to the fixed 40 columns. It was a true statement about the
/// code and it was the campaign's third reported defect wearing a test's
/// clothes — the default focus is `Focus::Walk`, so the widening never
/// once happened in the view a player walks around in. Focus decides where
/// the keys go; it never decided how wide the picture should be.
///
/// So the property is retargeted, not dropped: the width now depends on
/// whether a plate was SUPPLIED, and on nothing else. Both halves are
/// asserted, because a rule with one half is how the old defect survived —
/// `None` must still leave the fixed pane alone, or the chamber band's
/// floor plan and the walk band's own chart would be handed a pane sized
/// for a raster that is not there.
#[test]
fn a_supplied_world_plate_widens_the_pane_in_every_focus() {
    let (w, h) = (210u16, 56u16);
    let fit = hornvale_game_core::spread::world_plate_width(w, h);
    assert!(
        fit > hornvale_game_core::spread::PLATE_WIDTH,
        "VACUOUS GUARD: {w}x{h} must fit a pane wider than the fixed          {}, or nothing below discriminates",
        hornvale_game_core::spread::PLATE_WIDTH
    );

    for focus in [Focus::Walk, Focus::Map, Focus::Cli] {
        assert_eq!(
            plate_column_count(w, h, focus),
            fit,
            "a supplied plate must widen the pane in {focus:?}"
        );
    }

    // The other half: with no plate supplied, the pane is the fixed width
    // in every focus, whatever the terminal's size.
    for focus in [Focus::Walk, Focus::Map, Focus::Cli] {
        let (g, _) = render_with(
            FIXTURE,
            w,
            h,
            focus,
            None,
            CommandLine::default(),
            None,
            None,
            None,
            0,
            None,
        )
        .unwrap();
        let past = hornvale_game_core::spread::PLATE_WIDTH;
        let row = hornvale_game_core::spread::content_height(h) / 2;
        assert!(
            (0..past).any(|x| g.get(x, row).is_some_and(|c| !c.is_blank())),
            "VACUOUS GUARD: the band's own plate must draw something on row              {row} in {focus:?}"
        );
        assert!(
            (past..w).all(|x| g
                .get(x, row)
                .is_some_and(|c| c.source != Source::Chart && c.source != Source::World)),
            "with no plate supplied the pane must stay {past} columns wide in {focus:?}"
        );
    }

    // The floor degrades, never refuses -- and lands on the SAME width the
    // fixed constant already gave it, which is why 80x24 alone could never
    // have caught this task's defect.
    let plate = solid_world_plate(80, 24);
    let (floor, _) = render_with(
        FIXTURE,
        80,
        24,
        Focus::Map,
        None,
        CommandLine::default(),
        None,
        None,
        Some(&plate),
        0,
        None,
    )
    .unwrap();
    assert_eq!(floor.width(), 80, "80x24 degrades, never refuses");
    assert_eq!(
        plate_column_count(80, 24, Focus::Walk),
        hornvale_game_core::spread::PLATE_WIDTH,
        "the 80x24 floor keeps the width it has always had"
    );
}

/// ACCEPTANCE TEST 1: the complete spread in monochrome characters at
/// 80x24, still usable.
#[test]
fn the_spread_fits_eighty_by_twenty_four() {
    let g = render(FIXTURE, 80, 24).unwrap();
    assert_eq!(g.width(), 80);
    assert_eq!(g.height(), 24);
    let text = g.to_plain_text();
    assert_eq!(text.lines().count(), 24);
    assert!(
        text.lines().all(|l| l.chars().count() == 80),
        "no line may overflow"
    );
}

/// The three regions are all present and non-empty: plate left, entry
/// right, endpaper below.
#[test]
fn all_three_regions_are_drawn() {
    let g = render(FIXTURE, 80, 24).unwrap();
    let text = g.to_plain_text();
    let lines: Vec<&str> = text.lines().collect();
    // Index by CHARACTER, not by byte: a plate glyph could one day be
    // multi-byte (the 22-biome glyph vocabulary is a planned follow-on),
    // and `&l[..40]` byte-slicing a `String` panics the instant a
    // multi-byte character crosses that boundary rather than failing with
    // a useful assertion message.
    let plate_has_ink = lines[..20]
        .iter()
        .any(|l| !l.chars().take(40).collect::<String>().trim().is_empty());
    let entry_has_ink = lines[..20]
        .iter()
        .any(|l| !l.chars().skip(40).collect::<String>().trim().is_empty());
    let endpaper_has_ink = !lines[22].trim().is_empty();
    assert!(plate_has_ink, "the plate must be drawn");
    assert!(entry_has_ink, "the entry must be drawn");
    assert!(endpaper_has_ink, "the endpaper must be drawn");
}

/// THE GUTTER STAYS BLANK. Row `h - 3` (index 21 at 80x24) is the blank
/// gutter between the plate/entry region and the endpaper. It carried a
/// ways-on line for exactly one task before The Quire (task 9d) deleted it,
/// and `spread.rs`'s module doc says "do not re-add a ways-on element here"
/// — but that was PROSE, and the two tests above skip row 21 entirely, so
/// re-claiming the gutter passed the whole suite. Given the element has now
/// been added and removed twice, this is the cheap regression pin.
///
/// Rules and gutters carry no ink of their own, so the assertion is simply
/// that the row is blank. Note it will NOT catch a non-ways-on element
/// drawn elsewhere; it pins the row, which is what was actually at risk.
#[test]
fn the_gutter_row_carries_no_ink() {
    for (w, h) in [(80u16, 24u16), (100, 30)] {
        let g = render(FIXTURE, w, h).unwrap();
        let text = g.to_plain_text();
        let lines: Vec<&str> = text.lines().collect();
        let gutter = lines[usize::from(h) - 3];
        assert!(
            gutter.trim().is_empty(),
            "the gutter row (h-3) must stay blank at {w}x{h}, found: {gutter:?}"
        );
    }
}

/// ACCEPTANCE TEST 9: the noun join. Every noun the plate's legend names is
/// examinable, i.e. present in the prose's own noun catalog namespace.
#[test]
fn the_noun_namespaces_join() {
    use hornvale_game_core::{Snapshot, Spatial};
    let s = Snapshot::parse(FIXTURE).unwrap();
    let prose_nouns: Vec<&str> = s.narration.nouns.iter().map(|n| n.noun.as_str()).collect();
    assert!(
        !prose_nouns.is_empty(),
        "VACUOUS GUARD: the fixture must carry nouns"
    );
    if let Spatial::Walk { chart } = &s.spatial {
        assert!(
            !chart.legend.is_empty(),
            "VACUOUS GUARD: the chart must carry a legend"
        );
        // The namespaces are deliberately one. At least one legend noun must
        // be examinable, or the join the two-page spread rests on is fiction.
        assert!(
            chart
                .legend
                .iter()
                .any(|l| prose_nouns.contains(&l.noun.as_str())),
            "map and prose must share a noun namespace"
        );
    }
}

/// Prose is the constitutional primary and is carried verbatim. The client
/// may wrap it; it may never rewrite it.
#[test]
fn the_prose_is_not_reworded() {
    use hornvale_game_core::Snapshot;
    let s = Snapshot::parse(FIXTURE).unwrap();
    let g = render(FIXTURE, 80, 24).unwrap();
    let flat: String = g.to_plain_text().replace('\n', " ");
    let first_word = s.narration.prose.split_whitespace().next().unwrap();
    assert!(
        flat.contains(first_word),
        "the prose must appear on the page"
    );
}
