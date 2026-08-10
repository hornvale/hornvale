//! Acceptance test 3: "a trace listing: every visible datum on the composed
//! screen, and which channel from the inventory it came from." `Cell::source`
//! and `Grid::provenance()` make that executable rather than a document.
//!
//! Run against **both** committed fixtures deliberately — the walk band
//! (`chart.rs`) and the chamber band (`plan.rs`) exercise different draw
//! paths, and a provenance bug confined to one band would be invisible to a
//! suite that only ever rendered the other.

use hornvale_game_core::{Grid, Source, render};

const WALK_FIXTURE: &str = include_str!("fixtures/session-seed-42-turn-0.json");
const CHAMBER_FIXTURE: &str = include_str!("fixtures/session-seed-42-chamber.json");

/// ACCEPTANCE TEST 3: every visible datum traces to a named channel. A cell
/// whose source is Unattributed is a datum nobody can justify.
#[test]
fn every_drawn_cell_names_its_channel() {
    let g = render(WALK_FIXTURE, 80, 24).unwrap();
    let p = g.provenance();
    assert_eq!(
        p.get(&Source::Unattributed).copied().unwrap_or(0),
        0,
        "every drawn cell must name the channel it came from: {p:?}"
    );
    assert!(
        p.get(&Source::Chart).copied().unwrap_or(0) > 0,
        "the plate is drawn"
    );
    assert!(
        p.get(&Source::Prose).copied().unwrap_or(0) > 0,
        "the entry is drawn"
    );
    assert!(
        p.get(&Source::Identity).copied().unwrap_or(0) > 0,
        "the endpaper is drawn"
    );
}

/// The redaction, restated at the render layer: no cell may claim `social`
/// as its source, because the mirror cannot see it.
#[test]
fn no_cell_claims_the_social_channel() {
    let names = format!("{:?}", render(WALK_FIXTURE, 80, 24).unwrap().provenance());
    assert!(
        !names.contains("Social"),
        "social is not renderable in this client"
    );
}

/// The chamber band's equivalent of the acceptance test above, over the
/// OTHER draw path (`plan.rs`, not `chart.rs`). `Source::Plan` is the one
/// variant `every_drawn_cell_names_its_channel` above cannot exercise, since
/// the walk fixture never calls `plan::draw`.
#[test]
fn every_drawn_cell_names_its_channel_in_the_chamber_band() {
    let g = render(CHAMBER_FIXTURE, 80, 24).unwrap();
    let p = g.provenance();
    assert_eq!(
        p.get(&Source::Unattributed).copied().unwrap_or(0),
        0,
        "every drawn cell must name the channel it came from: {p:?}"
    );
    assert!(
        p.get(&Source::Plan).copied().unwrap_or(0) > 0,
        "the floor plan is drawn"
    );
    assert!(
        p.get(&Source::Prose).copied().unwrap_or(0) > 0,
        "the entry is drawn"
    );
    assert!(
        p.get(&Source::Identity).copied().unwrap_or(0) > 0,
        "the endpaper is drawn"
    );
    // The command line (the `>` prompt) is drawn every turn regardless of
    // band, attributed to `Source::Chrome` (see that variant's doc for the
    // provenance defect this replaced: a twice-deleted `Source::WaysOn`).
    // This is also the acceptance test above never checks: it is the only
    // assertion in this file that proves `Source::Chrome` is reachable from
    // a real render at all, so it must stay a positive count, not merely
    // ride along on the `Unattributed == 0` check above.
    assert!(
        p.get(&Source::Chrome).copied().unwrap_or(0) > 0,
        "the command line is drawn"
    );
}

/// The redaction, restated over the chamber band. `Chart` and `Chamber`
/// carry different producers on the wire (`scene/surrounds/v2` vs
/// `vessel/plan/v1`), but neither mirrors `social` — belt and braces against
/// a future mirror that adds the field to only one of the two schema types.
#[test]
fn no_cell_claims_the_social_channel_in_the_chamber_band() {
    let names = format!(
        "{:?}",
        render(CHAMBER_FIXTURE, 80, 24).unwrap().provenance()
    );
    assert!(
        !names.contains("Social"),
        "social is not renderable in this client"
    );
}

/// A page that draws nothing must not be able to satisfy the acceptance
/// test by accident. `provenance()` over an untouched `Grid` is an EMPTY
/// map — not `{Unattributed: 0}`, nothing at all — which is why
/// `every_drawn_cell_names_its_channel`'s "no Unattributed" assertion is
/// paired with three separate ">0" assertions: the "no Unattributed" half
/// alone is satisfied by a blank grid just as easily as by a fully-attributed
/// one, and only the positive assertions tell those two situations apart.
#[test]
fn provenance_of_a_blank_grid_is_empty_not_vacuously_clean() {
    let g = Grid::new(80, 24);
    let p = g.provenance();
    assert!(
        p.is_empty(),
        "an untouched grid must report no provenance at all: {p:?}"
    );
    assert_eq!(p.get(&Source::Unattributed).copied().unwrap_or(0), 0);
}
