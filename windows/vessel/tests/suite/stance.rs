//! The Reticence, Task 3: prior and fold combine WITHOUT being summed into one
//! number (spec 3.5). The disagreement between them is the readable output — a
//! host may keep the warm word for you and still refuse to answer.

use hornvale_vessel::doctrine::Openness;
use hornvale_vessel::stance::{Stance, stance_for};

#[test]
fn an_unoffended_host_is_forthcoming_whatever_its_prior() {
    for prior in [Openness::Guarded, Openness::Wary, Openness::Open] {
        assert_eq!(
            stance_for(prior, 0),
            Stance::Forthcoming,
            "prior {prior:?} at zero overrides"
        );
    }
}

#[test]
fn the_prior_decides_how_fast_conduct_costs_you() {
    // Same conduct, different priors -> different stances. If this passes with
    // every prior producing the same stance, the prior is decorative and H4's
    // null has fired -- report it, do not retune.
    let guarded = stance_for(Openness::Guarded, 4);
    let open = stance_for(Openness::Open, 4);
    assert_ne!(
        guarded, open,
        "the prior must move the stance at equal conduct"
    );
}

#[test]
fn stances_worsen_monotonically_with_conduct() {
    let mut seen = Vec::new();
    for overrides in [0u32, 2, 4, 8, 16] {
        seen.push(stance_for(Openness::Wary, overrides));
    }
    let mut sorted = seen.clone();
    sorted.sort();
    assert_eq!(
        seen, sorted,
        "more overriding must never make a host MORE forthcoming: {seen:?}"
    );
}
