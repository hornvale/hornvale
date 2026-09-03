//! The warmth×competence projection and [`snap_judgment`] (spec §5, Task
//! 2): folds the eight weighted axis distances into a 2-D valuation and
//! classifies it to one of four emotions — the Stereotype Content Model's
//! warmth×competence quadrants (Fiske, Cuddy & Glick). Relational and
//! asymmetric by construction: `snap_judgment(a, b)` need not equal
//! `snap_judgment(b, a)`, because both the weight vector (judger-relative,
//! see [`crate::weight_vector`]) and two of the eight axis distances
//! (`DietPredation`, `SizeThreat`) are directional.
//!
//! **The projection.** For every axis, `weight_vector(judger)[axis] *
//! axis_distance(axis, judger, target)` is that axis's *weighted distance*
//! — how much this particular judger, with this particular psychology,
//! registers this particular gap. Each axis then pushes that weighted
//! distance onto warmth and/or competence by a fixed **signature** (spec §5
//! table; controller correction D3), applied identically to every
//! judger/target pair — a per-AXIS constant, never a per-people one (0021):
//!
//! | axis | warmth | competence | rationale |
//! |---|---:|---:|---|
//! | Habitat | −1.0 | · | an alien environment reads as unrelatable, not incompetent |
//! | DietPredation | −1.0 | +1.0 | a predation edge chills warmth but reads the other as capable-dangerous |
//! | ConditionNiche | −0.5 | · | mild: shared vs. mismatched climate tolerance is a background cue |
//! | Sociality | · | −1.0 | an illegible social order reads as *disordered*, not unloved |
//! | ActivityCycle | −1.0 | · | a mismatched waking schedule reads as simply unfamiliar |
//! | Reproductive | −1.0 | · | a mismatched life-pace reads as unfamiliar, not incapable |
//! | Language | · | −1.0 | unintelligible speech reads as an intelligibility failure, not coldness |
//! | SizeThreat | −1.0 | +1.0 | a bodily threat chills warmth but reads the other as capable-dangerous |
//!
//! **Baselines.** `warmth` starts at [`WARMTH_BASELINE`] (`1.0`, maximum)
//! and every signature entry only ever pushes it DOWN (no axis has a
//! positive warmth term); `competence` starts at [`COMPETENCE_BASELINE`]
//! (`0.5`) and is pushed both up (a predation or size threat reads as
//! *capable*-dangerous) and down (an order or language mismatch reads as
//! disordered/unintelligible).
//!
//! **The believability floor (D3).** At zero distance on every axis — an
//! identical-traits target — every weighted-distance term is exactly `0.0`
//! regardless of the judger's weight vector, so both sums vanish exactly
//! and the pair lands at `(warmth, competence) = (1.0, 0.5)`: maximum
//! warmth, and competence sitting exactly on the high side of the
//! classification threshold below. That is `Admiration`, never `Contempt`
//! — self-similarity must read as liking. Verified in
//! `tests/suite/judgment.rs`.
//!
//! **Classification.** Both coordinates split at the same threshold,
//! [`CLASSIFICATION_THRESHOLD`] (`0.5`), using `>=` for "high" — so the
//! believability-floor point sits exactly on the high side of both:
//!
//! | | competence ≥ 0.5 | competence < 0.5 |
//! |---|---|---|
//! | **warmth ≥ 0.5** | Admiration | Pity |
//! | **warmth < 0.5** | Envy | Contempt |

use crate::{Axis, PeopleTraits, axis_distance, weight_vector};

/// Warmth's starting point before any axis pushes it down: maximum warmth,
/// so a target differing on nothing at all is read as maximally warm (see
/// the module doc's believability floor).
/// plumb: pending(wave-1)
const WARMTH_BASELINE: f64 = 1.0;

/// Competence's starting point before any axis pushes it up or down: chosen
/// so that an identical-traits target (every push zero) sits exactly on the
/// high side of [`CLASSIFICATION_THRESHOLD`] (see the module doc).
/// plumb: pending(wave-1)
const COMPETENCE_BASELINE: f64 = 0.5;

/// The threshold separating "high" from "low" on both warmth and
/// competence; `>=` counts as high on both.
/// plumb: pending(wave-1)
const CLASSIFICATION_THRESHOLD: f64 = 0.5;

/// One axis's signed push onto `(warmth, competence)`, per the module doc's
/// signature table, indexed by [`Axis::ALL`] order.
const SIGNATURE: [(f64, f64); 8] = [
    (-1.0, 0.0), // Habitat
    (-1.0, 1.0), // DietPredation
    (-0.5, 0.0), // ConditionNiche (mild)
    (0.0, -1.0), // Sociality
    (-1.0, 0.0), // ActivityCycle
    (-1.0, 0.0), // Reproductive
    (0.0, -1.0), // Language
    (-1.0, 1.0), // SizeThreat
];

/// The four warmth×competence quadrants (the Stereotype Content Model).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Emotion {
    /// High competence, high warmth.
    Admiration,
    /// High competence, low warmth.
    Envy,
    /// Low competence, high warmth.
    Pity,
    /// Low competence, low warmth.
    Contempt,
}

/// One judger's 2-D valuation of one target, and the emotion it classifies
/// to (see the module doc for the projection, baselines, and
/// classification).
/// type-audit: bare-ok(ratio: warmth), bare-ok(ratio: competence)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Judgment {
    /// The warmth coordinate (see the module doc's baseline/signature).
    pub warmth: f64,
    /// The competence coordinate (see the module doc's baseline/signature).
    pub competence: f64,
    /// The classified emotion.
    pub emotion: Emotion,
}

/// `v(judger → target)`: the weighted, axis-projected valuation between two
/// peoples (see the module doc). Relational and asymmetric by construction.
pub fn snap_judgment(judger: &PeopleTraits, target: &PeopleTraits) -> Judgment {
    let weights = weight_vector(judger);
    let mut warmth = WARMTH_BASELINE;
    let mut competence = COMPETENCE_BASELINE;

    for (index, axis) in Axis::ALL.into_iter().enumerate() {
        let weighted_distance = weights[index] * axis_distance(axis, judger, target);
        let (warmth_sign, competence_sign) = SIGNATURE[index];
        warmth += warmth_sign * weighted_distance;
        competence += competence_sign * weighted_distance;
    }

    Judgment {
        warmth,
        competence,
        emotion: classify(warmth, competence),
    }
}

/// Classify a `(warmth, competence)` point to its quadrant (see the module
/// doc's table). `>=` counts as "high" on both axes.
fn classify(warmth: f64, competence: f64) -> Emotion {
    match (
        warmth >= CLASSIFICATION_THRESHOLD,
        competence >= CLASSIFICATION_THRESHOLD,
    ) {
        (true, true) => Emotion::Admiration,
        (false, true) => Emotion::Envy,
        (true, false) => Emotion::Pity,
        (false, false) => Emotion::Contempt,
    }
}
