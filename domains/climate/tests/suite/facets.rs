//! Integration coverage for the underworld realm (The Deep Realm, Task 1):
//! `Medium::Rock`, the graduated `Access` ladder, and the five-band rock
//! column. Since Task 7 (decision 0517 clause (a)) `Horizon` is the
//! kernel's shared roster and `Stratum::Rock(Horizon)` embeds it directly,
//! so the roster correspondence this file asserts is now structural rather
//! than a hand-maintained name mirror — the ordering claim survives as an
//! assertion that `Realm::UNDERDARK`'s rock strata are `Horizon::all()`
//! wrapped in `Rock`, in order.

use hornvale_kernel::Horizon;

#[test]
fn the_underworld_is_a_realm_with_a_rock_column() {
    let r = hornvale_climate::Realm::UNDERDARK;
    assert_eq!(r.medium, hornvale_climate::Medium::Rock);
    // FIVE bands, mirroring hornvale_kernel::Horizon exactly. See ledger
    // #18A / rule 1a: a four-band ladder cannot absorb the open depth-weld
    // fix without relocating every ChamberAddr.
    assert_eq!(
        r.strata(),
        &[
            hornvale_climate::Stratum::Rock(Horizon::Regolith),
            hornvale_climate::Stratum::Rock(Horizon::Cover),
            hornvale_climate::Stratum::Rock(Horizon::Basement),
            hornvale_climate::Stratum::Rock(Horizon::Roots),
            hornvale_climate::Stratum::Rock(Horizon::Underneath),
        ]
    );
}

#[test]
fn the_rock_ladder_matches_horizons_roster_one_for_one_in_order() {
    // The mirror is now structural (Task 7): `Stratum::Rock` embeds
    // `Horizon` directly, so there is no name to drift out of sync. What
    // still needs asserting is the ORDERING claim the old name-mirror test
    // carried — that `Realm::UNDERDARK`'s rock strata are `Horizon::all()`
    // wrapped in `Rock`, in the same order — since nothing about embedding
    // a type guarantees a caller assembled the list in ladder order.
    let strata: Vec<hornvale_climate::Stratum> =
        hornvale_climate::Realm::UNDERDARK.strata().to_vec();
    let expected: Vec<hornvale_climate::Stratum> = Horizon::all()
        .iter()
        .map(|&h| hornvale_climate::Stratum::Rock(h))
        .collect();
    assert_eq!(
        strata, expected,
        "Realm::UNDERDARK's rock strata must be Horizon::all() wrapped in \
         Rock, in order"
    );
}

#[test]
fn the_aperture_is_ordered_from_sealed_to_merged() {
    use hornvale_climate::Access::*;
    let ladder = [Sealed, Crack, CaveMouth, WorkedWay, Gate, ShaftNet, Merged];
    for w in ladder.windows(2) {
        assert!(w[0] < w[1], "{:?} must sort below {:?}", w[0], w[1]);
    }
}
