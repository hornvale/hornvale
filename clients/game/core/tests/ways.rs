//! Task 9b: the ways-on line is a first-class rendered element, sourced from
//! `sensed.room.exits`/`spatial` rather than parsed out of prose. See
//! `src/ways.rs`'s module doc for the two-band derivation this exercises.

use hornvale_game_core::{Snapshot, render, ways::ways_on};

const WALK_FIXTURE: &str = include_str!("fixtures/session-seed-42-turn-0.json");
const CHAMBER_FIXTURE: &str = include_str!("fixtures/session-seed-42-chamber.json");

/// Pull the `Ways on: ...` sentence's own list out of a prose passage — the
/// sim's own convention (`Session::describe_here`/`describe_chamber_here`):
/// the LAST line, prefixed `"Ways on: "` and suffixed `"."`. This is test-only
/// parsing of the prose to build an independent expectation; the crate under
/// test never does this (see `entry.rs`'s module doc for why).
fn prose_ways(prose: &str) -> Vec<String> {
    let last = prose.lines().next_back().expect("prose has a last line");
    let stripped = last
        .strip_prefix("Ways on: ")
        .and_then(|s| s.strip_suffix('.'))
        .unwrap_or_else(|| panic!("last prose line is not a Ways-on sentence: {last:?}"));
    stripped.split(", ").map(str::to_string).collect()
}

/// THE CHEAPEST AVAILABLE CORRECTNESS CHECK, per the task brief: the sim's
/// own prose already states the right answer in its trailing sentence, so if
/// this crate's filter/derivation is wrong, the two disagree. Checked on
/// BOTH committed fixtures — one exercises the walk-band compass filter, the
/// other the chamber-band `at`/`of` derivation.
#[test]
fn rendered_ways_on_agrees_with_the_prose_on_the_walk_fixture() {
    let s = Snapshot::parse(WALK_FIXTURE).unwrap();
    let want = prose_ways(&s.narration.prose);
    assert_eq!(
        want,
        vec!["NE", "NW", "S"],
        "VACUOUS GUARD: unexpected fixture prose"
    );
    assert_eq!(ways_on(&s.sensed, &s.spatial), want);
}

#[test]
fn rendered_ways_on_agrees_with_the_prose_on_the_chamber_fixture() {
    let s = Snapshot::parse(CHAMBER_FIXTURE).unwrap();
    let want = prose_ways(&s.narration.prose);
    assert_eq!(
        want,
        vec!["out", "further in"],
        "VACUOUS GUARD: unexpected fixture prose"
    );
    assert_eq!(ways_on(&s.sensed, &s.spatial), want);
}

/// THE TEST THAT MATTERS MOST. Prose long enough to overflow `entry.rs`'s
/// budget and trigger its truncation marker — the exact scenario where the
/// old, prose-embedded "Ways on:" sentence could be (and, per the task's
/// premise, sometimes was) the very thing cut. The substituted prose below
/// carries NO "Ways on:" sentence at all, so this test fails if the
/// ways-on element is ever drawn from prose text instead of
/// `sensed.room.exits`: there is nothing correct to extract from this
/// prose, and only the wire channel still has the right answer.
#[test]
fn ways_on_survives_prose_overflow_that_truncates_the_entry() {
    let mut s = Snapshot::parse(WALK_FIXTURE).unwrap();
    let words: Vec<String> = (0..400).map(|i| format!("word{i}")).collect();
    s.narration.prose = words.join(" "); // no "Ways on:" sentence anywhere
    assert!(
        !s.narration.prose.contains("Ways on:"),
        "PREMISE CHECK: the synthetic prose must not accidentally contain the \
         real sentence, or the two sources of truth cannot be told apart"
    );
    let json = serde_json::to_string(&s).unwrap();

    let g = render(&json, 80, 24).unwrap();
    let text = g.to_plain_text();

    assert!(
        text.contains("more, not shown"),
        "PREMISE CHECK: the substituted prose must actually overflow the entry \
         pane, or this test proves nothing about truncation at all"
    );
    assert!(
        text.contains("Ways on: NE, NW, S."),
        "the ways-on element must be fully present and readable regardless of \
         how the prose truncated — it is sourced from sensed.room.exits, which \
         this edit never touched"
    );
}

/// The chamber-band counterpart: overflow the entry pane on the CHAMBER
/// fixture too, and confirm the fixed `out, further in` pair still renders —
/// proving the overflow guarantee holds for the `at`/`of` derivation, not
/// only the compass-filter path.
#[test]
fn chamber_ways_on_survives_prose_overflow_that_truncates_the_entry() {
    let mut s = Snapshot::parse(CHAMBER_FIXTURE).unwrap();
    let words: Vec<String> = (0..400).map(|i| format!("word{i}")).collect();
    s.narration.prose = words.join(" ");
    let json = serde_json::to_string(&s).unwrap();

    let g = render(&json, 80, 24).unwrap();
    let text = g.to_plain_text();

    assert!(
        text.contains("more, not shown"),
        "PREMISE CHECK: the substituted prose must actually overflow"
    );
    assert!(
        text.contains("Ways on: out, further in."),
        "the chamber band's ways-on element must survive prose overflow too"
    );
}
