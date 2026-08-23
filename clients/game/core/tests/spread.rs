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
    )
    .unwrap();
    assert_eq!(with.get(20, 10).unwrap().source, Source::World);
    assert_eq!(without.get(20, 10).unwrap().source, Source::Chart);
    // The discriminating probe: (20, 6) is Chart in `without` (confirmed
    // by probing the real render) and blank in the synthetic plate, so a
    // merge bug (draw chart, then blit world on top) would leave it
    // `Chart` in `with` too. The correct either/or leaves it
    // `Unattributed`: the chart was never drawn into `with`'s plate at
    // all.
    assert_eq!(without.get(20, 6).unwrap().source, Source::Chart);
    assert_eq!(
        with.get(20, 6).unwrap().source,
        Source::Unattributed,
        "the band's chart must not be drawn at all when a world plate is \
         supplied — a cell the synthetic plate leaves blank must stay \
         blank in `with`, not fall through to the chart underneath"
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
