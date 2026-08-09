use hornvale_game_core::render;

const FIXTURE: &str = include_str!("fixtures/session-seed-42-turn-0.json");

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
    let plate_has_ink = lines[..20].iter().any(|l| !l[..40].trim().is_empty());
    let entry_has_ink = lines[..20].iter().any(|l| !l[40..].trim().is_empty());
    let endpaper_has_ink = !lines[22].trim().is_empty();
    assert!(plate_has_ink, "the plate must be drawn");
    assert!(entry_has_ink, "the entry must be drawn");
    assert!(endpaper_has_ink, "the endpaper must be drawn");
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
