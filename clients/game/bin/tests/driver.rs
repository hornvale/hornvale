//! `Driver`'s acceptance tests: the containment (only JSON crosses out) and
//! the loop (a verb advances the turn).

use hornvale_game::driver::Driver;
use hornvale_game::input::Action;

/// The driver's ONLY output is snapshot JSON. If this ever returns a typed
/// value, the containment in The Quire spec section 6 has been broken.
#[test]
fn the_driver_yields_json_the_core_can_render() {
    let d = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
    let json = d.snapshot();
    let g = hornvale_game_core::render(&json, 80, 24).expect("core must render the live snapshot");
    assert_eq!(g.width(), 80);
    assert_eq!(g.height(), 24);
}

/// A turn advances. This is the loop the whole client is.
#[test]
fn handling_a_verb_advances_the_turn() {
    let mut d = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
    let before = hornvale_game_core::Snapshot::parse(&d.snapshot())
        .unwrap()
        .turn;
    d.handle("look");
    let after = hornvale_game_core::Snapshot::parse(&d.snapshot())
        .unwrap()
        .turn;
    assert_eq!(after, before + 1);
}

/// `handle`'s own return value is not a second, possibly-stale channel: it
/// must be exactly what a subsequent `snapshot()` call would give back.
#[test]
fn handles_return_value_matches_a_following_snapshot_call() {
    let mut d = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
    let returned = d.handle("look");
    let read_back = d.snapshot();
    assert_eq!(returned, read_back);
}

/// `--target most-populous-settlement` must mint at a DIFFERENT settlement
/// than the flagship default — seed 42's flagship is Googo (pop 68), the
/// most-populous settlement is Toa (pop 84), so the two snapshots'
/// `self.settlement` fields must disagree. If they ever agreed, `target`
/// would be silently ignored by the driver. (Both agents are MINTED; the
/// target chooses the settlement, not an existing resident.)
#[test]
fn the_most_populous_target_mints_at_a_different_settlement_than_flagship() {
    let flagship = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
    let popular =
        Driver::start(42, hornvale_vessel::PossessTarget::MostPopulousSettlement).unwrap();
    let a = hornvale_game_core::Snapshot::parse(&flagship.snapshot()).unwrap();
    let b = hornvale_game_core::Snapshot::parse(&popular.snapshot()).unwrap();
    assert_ne!(a.me.settlement, b.me.settlement);
}

/// A released possession's parting line is not a separate channel either —
/// it lands in the snapshot `handle` returns, same as any other turn.
#[test]
fn releasing_still_returns_a_parseable_snapshot() {
    let mut d = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
    let json = d.handle("release");
    let snap = hornvale_game_core::Snapshot::parse(&json).expect("release still yields a snapshot");
    assert!(!snap.narration.prose.is_empty());
}

/// The cursor is a plate primitive. At a band whose resolver does not exist
/// yet, it still moves and still reports — it refuses honestly rather than
/// resolving against the wrong plate. Faking a resolution here would be
/// worse than refusing, because a wrong name is indistinguishable from a
/// right one.
///
/// `Driver` has no `for_test` seam (see the task report: adding one to
/// widen the public surface for a single test was rejected). The smallest
/// honest way to reach a chamber-band session is the same one
/// `scripts/possession-chamber.txt` and the committed
/// `session-seed-42-chamber.json` fixture already use — a single `enter`
/// from seed 42's flagship opening position — so this test drives a real
/// `Driver` through it rather than inventing a lighter-weight seam.
#[test]
fn look_mode_at_an_unresolved_band_refuses_rather_than_resolving() {
    let mut driver = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
    driver.handle("enter");
    let snap = hornvale_game_core::Snapshot::parse(&driver.snapshot())
        .expect("a live session always yields a parseable snapshot");
    assert!(
        matches!(snap.spatial, hornvale_game_core::Spatial::Chamber { .. }),
        "seed 42's flagship must land in the chamber band after one `enter`"
    );

    driver.apply(Action::EnterLook);
    driver.apply(Action::CursorBy(1, 0));
    assert!(
        driver.cursor().is_some(),
        "the cursor must exist at every band"
    );
    assert_eq!(driver.strip_text(), Some("nothing here yet"));
}

/// The walk band DOES have a resolver (the terrain-feature index, scoped to
/// the observer's own cell — see `driver.rs`'s module doc for why cursor
/// motion does not change which cell is queried this campaign). Entering
/// look mode at seed 42's flagship opening position (walk band) must report
/// a real name, not the unresolved-band refusal — this is what would catch
/// a regression that accidentally routed every band through the same
/// refusal.
#[test]
fn look_mode_at_the_walk_band_resolves_a_real_name() {
    let mut driver = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
    let snap = hornvale_game_core::Snapshot::parse(&driver.snapshot()).unwrap();
    assert!(matches!(
        snap.spatial,
        hornvale_game_core::Spatial::Walk { .. }
    ));

    driver.apply(Action::EnterLook);
    let strip = driver.strip_text();
    assert!(strip.is_some(), "the walk band must resolve to something");
    assert_ne!(
        strip,
        Some("nothing here yet"),
        "the walk band has a real resolver and must not report the unresolved-band refusal"
    );
}

/// FIX ROUND 1: the cursor must genuinely track. Moving it off the
/// observer's own box changes the resolved cell, and therefore the strip
/// text, rather than recomputing the same answer regardless of position.
///
/// **Why the covering assertion is "names something" -> "unnamed terrain"
/// rather than two different NAMED features**, checked and worth recording:
/// a swept probe over every reachable box within seed 42's flagship
/// walk-band view found every `Some` box resolves to the SAME nearest
/// terrain `CellId` as the observer (`CellId(22195)`, confirmed by
/// instrumenting `resolve_walk_band` directly) — the visible neighbourhood
/// spans `distance_rad` on the order of 1e-4 (tens of metres), while
/// `NearestCellIndex` snaps onto one of only 40,962 cells tiling the whole
/// globe (roughly hundreds of kilometres apart at seed 42's `GLOBE_LEVEL`).
/// So a walk-band session structurally cannot cross a terrain-feature
/// boundary within view except by standing within metres of one — a real
/// scale mismatch this fix exposed, not a limitation of the test. What IS
/// reachable, and does prove the cursor is read: a box the observer's own
/// chart has no cell in at all (`cell_at` returns `None`, screen offset
/// `(15, 10)`, five columns west of centre `(20, 10)`) — a different,
/// real, honestly-reported outcome from the observer's own box.
#[test]
fn moving_the_cursor_off_the_observers_box_changes_the_strip() {
    let mut driver = Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap();
    driver.apply(Action::EnterLook);
    let at_observer = driver.strip_text().map(str::to_string);
    assert_eq!(
        at_observer.as_deref(),
        Some("Vngashngatva"),
        "the observer's own box must resolve to seed 42's real landmass name"
    );

    // Five columns west: no chart cell projects onto this box (verified by
    // instrumenting `chart::cell_at` directly), so the resolver honestly
    // reports UNNAMED_TERRAIN rather than repeating the observer's name.
    driver.apply(Action::CursorBy(-5, 0));
    let five_west = driver.strip_text().map(str::to_string);
    assert_ne!(
        five_west, at_observer,
        "the strip must change when the cursor moves off the observer's box"
    );
    assert_eq!(five_west.as_deref(), Some("unnamed terrain"));

    // And moving back must restore the observer's own answer — proving the
    // dependency runs both ways, not just away from the start.
    driver.apply(Action::CursorBy(5, 0));
    assert_eq!(driver.strip_text(), at_observer.as_deref());
}
