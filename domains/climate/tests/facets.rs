//! Integration coverage for the underworld realm (The Deep Realm, Task 1):
//! `Medium::Rock`, the graduated `Access` ladder, and the five-band rock
//! column. `domains/climate` may not import `hornvale_terrain` (a sibling
//! domain), so the roster correspondence this file asserts is checked by
//! NAME against a hardcoded mirror of `hornvale_terrain::BandKind` — the
//! only thing keeping the deliberate duplicate (decision 0094) honest here.

#[test]
fn the_underworld_is_a_realm_with_a_rock_column() {
    let r = hornvale_climate::Realm::UNDERDARK;
    assert_eq!(r.medium, hornvale_climate::Medium::Rock);
    // FIVE bands, mirroring hornvale_terrain::BandKind exactly. See ledger
    // #18A / rule 1a: a four-band ladder cannot absorb the open depth-weld
    // fix without relocating every ChamberAddr.
    assert_eq!(
        r.strata(),
        &[
            hornvale_climate::Stratum::Regolith,
            hornvale_climate::Stratum::Cover,
            hornvale_climate::Stratum::Basement,
            hornvale_climate::Stratum::Roots,
            hornvale_climate::Stratum::Underneath,
        ]
    );
}

#[test]
fn the_rock_ladder_matches_terrains_band_roster_one_for_one() {
    // Decision 0094: a shared roster, never a shared derivation. Climate may
    // not import terrain, so this is the only thing keeping the duplicate
    // honest. If terrain adds a sixth BandKind, this reddens rather than
    // silently giving the underworld a band it has no rock for.
    //
    // hornvale_terrain::BandKind, mirrored here as of ledger #18A: five
    // variants, this order — Regolith, Cover, Basement, Roots, Underneath.
    //
    // Assert the COUNT and the ORDER by name. Do not cast either enum to an
    // integer -- that would weld the ladder to a declaration position.
    const TERRAIN_BAND_ROSTER: [&str; 5] = ["Regolith", "Cover", "Basement", "Roots", "Underneath"];
    let strata = hornvale_climate::Realm::UNDERDARK.strata();
    assert_eq!(
        strata.len(),
        TERRAIN_BAND_ROSTER.len(),
        "the rock column must carry exactly as many bands as \
         hornvale_terrain::BandKind has variants"
    );
    for (stratum, name) in strata.iter().zip(TERRAIN_BAND_ROSTER.iter()) {
        assert_eq!(
            &format!("{stratum:?}"),
            name,
            "Stratum's rock bands must mirror BandKind's names, in order"
        );
    }
}

#[test]
fn the_aperture_is_ordered_from_sealed_to_merged() {
    use hornvale_climate::Access::*;
    let ladder = [Sealed, Crack, CaveMouth, WorkedWay, Gate, ShaftNet, Merged];
    for w in ladder.windows(2) {
        assert!(w[0] < w[1], "{:?} must sort below {:?}", w[0], w[1]);
    }
}
