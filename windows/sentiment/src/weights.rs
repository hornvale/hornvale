//! The weight-vector: a people's derived per-axis "prejudice personality"
//! (spec §5, Task 2). [`weight_vector`] is a pure function of the JUDGER's
//! own authored psychology — never of a target, and never of a named pair —
//! so it satisfies 0021 by construction: nothing here can single out which
//! *other* people gets weighted differently, only how strongly *this*
//! people's own axes register at all.
//!
//! **The weight law.** Every axis starts at a uniform base weight of `1.0`
//! (0021: no axis is authored as more salient than another before the
//! judger's own psychology is applied). Three independent, non-negative
//! multipliers then scale that base, each drawn from a different field of
//! the judger's own [`PeopleTraits`]:
//!
//! 1. **Global insularity multiplier**, from `society.in_group_radius`
//!    (`SocietyVector`, documented "insular 0 ↔ expansive 1" —
//!    `domains/species/src/lib.rs:223`). Applied to EVERY axis. A
//!    **decreasing** function of `in_group_radius`: the more insular
//!    (lower-radius) a judger, the more it up-weights every axis it judges
//!    by — an insular people notices and weighs *every* difference more
//!    sharply; a cosmopolitan (high-radius) one dampens toward the uniform
//!    base. `global(r) = 1.0 + INSULARITY_GAIN * (1.0 - r)`, so a fully
//!    insular judger (`r = 0`) weighs everything at
//!    `1.0 + INSULARITY_GAIN` and a fully cosmopolitan one (`r = 1`) weighs
//!    everything at exactly the uniform base `1.0`. **This is the inverse
//!    direction of the plan's literal Step 1 text** — the plan text
//!    describes "max `in_group_radius`" as insular, which is backwards per
//!    the field's own doc comment; the controller correction (D1) governs.
//!
//! 2. **Threat multiplier**, from `mind.threat_response` (`MindVector`,
//!    documented "flee 0 ↔ stand 1" — `domains/species/src/lib.rs:170`).
//!    Applied only to [`Axis::SizeThreat`] and [`Axis::DietPredation`]. An
//!    **increasing** function of `threat_response`:
//!    `threat(t) = 1.0 + THREAT_GAIN * t`. Chosen deliberately (D2) on the
//!    reading that a people that STANDS its ground treats bodily-threat
//!    cues as salient, worth-weighing information — a people that has
//!    learned to face danger down attends to danger signals precisely
//!    because it acts on them. The equally arguable alternative — that a
//!    fleeing/fearful people is instead hypervigilant about threat — is
//!    *not* adopted here; this doc records the choice so the sign is on the
//!    record rather than accidental.
//!
//! 3. **Order multiplier**, from `society.sociality` and
//!    `society.status_basis`. Applied only to [`Axis::Sociality`]. A judger
//!    whose own society is `Hierarchic` (authority is ranked, not communal)
//!    or whose own status is earned by `Rank` (dominance and position,
//!    rather than knowledge or generosity) is, by construction, one for
//!    whom social ORDER is itself the currency of standing — such a judger
//!    is modeled as registering a mismatch in *another* people's social
//!    order more heavily than one whose own status system does not turn on
//!    rank at all. The two conditions are additive, so a judger with both
//!    traits compounds:
//!    `order = 1.0 + (HIERARCHIC_GAIN if Hierarchic else 0.0)
//!                  + (RANK_GAIN if Rank else 0.0)`.
//!
//! Every multiplier is `>= 1.0` by construction (every gain constant is
//! non-negative and every input is bounded in `[0,1]` or boolean), so
//! `weight_vector` is non-negative everywhere, as the interface requires.
//! Every gain constant below is an AUTHORED modeling constant applied
//! uniformly to every judger by the same rule — never a per-pair or
//! per-named-people preference (0021), the same discipline `axes.rs`'s
//! `PREDATION_SHIFT` documents for its own constant.

use hornvale_species::{Sociality, StatusBasis};

use crate::{Axis, PeopleTraits};

/// How strongly full insularity (`in_group_radius = 0`) up-weights every
/// axis over full cosmopolitanism (`in_group_radius = 1`).
/// plumb: pending(wave-1)
const INSULARITY_GAIN: f64 = 1.0;

/// How strongly full threat-standing (`threat_response = 1`) up-weights the
/// two threat axes (`SizeThreat`, `DietPredation`) over full flight
/// (`threat_response = 0`).
/// plumb: pending(wave-1)
const THREAT_GAIN: f64 = 1.0;

/// How much a `Hierarchic` sociality adds to the Sociality axis's weight.
/// plumb: pending(wave-1)
const HIERARCHIC_GAIN: f64 = 0.5;

/// How much a `Rank` status basis adds to the Sociality axis's weight.
/// plumb: pending(wave-1)
const RANK_GAIN: f64 = 0.5;

/// A people's derived per-axis weights — its "prejudice personality." A
/// pure function of the JUDGER's own psychology (see the module doc for the
/// full weight law). Indexed by [`Axis::ALL`] order. Non-negative.
/// type-audit: bare-ok(ratio: return)
pub fn weight_vector(judger: &PeopleTraits) -> [f64; 8] {
    let global = 1.0 + INSULARITY_GAIN * (1.0 - judger.society.in_group_radius);
    let threat = 1.0 + THREAT_GAIN * judger.mind.threat_response;
    let order =
        1.0 + if judger.society.sociality == Sociality::Hierarchic {
            HIERARCHIC_GAIN
        } else {
            0.0
        } + if judger.society.status_basis == StatusBasis::Rank {
            RANK_GAIN
        } else {
            0.0
        };

    Axis::ALL.map(|axis| match axis {
        Axis::SizeThreat | Axis::DietPredation => global * threat,
        Axis::Sociality => global * order,
        _ => global,
    })
}

// Property tests live in `tests/suite/judgment.rs` (the consolidated
// integration-test binary), matching `axes.rs`'s split between an in-module
// smoke test and the fuller integration battery.
