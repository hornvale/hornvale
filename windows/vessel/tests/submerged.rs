//! THE FATHOM: `submerged` is a question about the MEDIUM, not about which
//! rung of a ladder you are standing on.

use hornvale_climate::{Realm, Stratum};

/// The live defect. Today `submerged` is `stratum != Surface`, so any rock
/// rung reads as underwater — which is what the game client would be told the
/// moment campaign 2 walks a player into a chamber.
#[test]
fn a_rock_stratum_is_not_submerged() {
    for rung in Realm::UNDERDARK.strata() {
        assert!(
            !hornvale_vessel::submerged_in(Some(*rung)),
            "{rung:?} is rock, not water"
        );
    }
}

/// The other direction, so the fix cannot be "always false".
#[test]
fn a_water_stratum_is_submerged() {
    for rung in Realm::WATERWORLD.strata() {
        assert!(
            hornvale_vessel::submerged_in(Some(*rung)),
            "{rung:?} is water"
        );
    }
}

/// And the surface, and the absent case, keep their present answers — this
/// is the half that must not move.
#[test]
fn the_surface_and_the_absent_case_are_unchanged() {
    assert!(!hornvale_vessel::submerged_in(Some(Stratum::Surface)));
    assert!(!hornvale_vessel::submerged_in(None));
}
