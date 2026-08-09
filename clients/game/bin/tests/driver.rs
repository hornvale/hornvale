//! `Driver`'s acceptance tests: the containment (only JSON crosses out) and
//! the loop (a verb advances the turn).

use hornvale_game::driver::Driver;

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
