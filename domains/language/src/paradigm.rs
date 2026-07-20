//! LANG-43: paradigm slots (Number, Tense) whose cascade-native form can
//! diverge from a mechanically-regular one, purely as a byproduct of
//! `evolve`'s word-edge rules ([`RuleKind::FinalLoss`],
//! [`RuleKind::ClusterSimplify`]) being positional over whatever sequence
//! they are handed — the divergence is irregularity, derived, never
//! authored. See `docs/superpowers/specs/2026-07-20-the-residue-design.md`.
#![warn(missing_docs)]

use crate::morphology::{ClassPosition, MorphDepth};
use hornvale_kernel::Seed;

/// A tongue's drawn Number/Tense grammaticalization depths and attachment
/// sides — the LANG-43 sibling of [`crate::morphology::TongueMorphology`]'s
/// evidential/noun-class depths, kept as its own additive struct (not
/// folded into `TongueMorphology`) so this campaign touches no existing
/// call site: nothing in the shipped grammar renderer consumes these
/// fields yet (spec §6, no rendering surface in V1).
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ParadigmDepths {
    /// How deeply Number (Singular/Plural) grammaticalizes.
    pub number_depth: MorphDepth,
    /// How deeply Tense (Present/Past) grammaticalizes.
    pub tense_depth: MorphDepth,
    /// Which side of the marked word the Number affix binds.
    pub number_position: ClassPosition,
    /// Which side of the marked word the Tense affix binds.
    pub tense_position: ClassPosition,
}

/// Preregistered Number-depth weights over `[None, Particle, Affix]` —
/// most attested languages mark number morphologically at least
/// optionally, so this skews toward `Affix` more than the epistemic
/// evidential/noun-class weights ([`crate::morphology`]) do; a purely
/// typological prior, unrelated to their worldview-grammaticalization
/// story.
const NUMBER_DEPTH_WEIGHTS: [f64; 3] = [30.0, 20.0, 50.0];

/// Preregistered Tense-depth weights over `[None, Particle, Affix]`.
const TENSE_DEPTH_WEIGHTS: [f64; 3] = [25.0, 25.0, 50.0];

/// The percentage chance (out of 100) the Number affix binds as a suffix
/// rather than a prefix.
const NUMBER_POSITION_SUFFIX_CHANCE: u32 = 70;

/// The percentage chance (out of 100) the Tense affix binds as a suffix
/// rather than a prefix.
const TENSE_POSITION_SUFFIX_CHANCE: u32 = 65;

/// The `weighted_index` bucket order both depth axes share: 0 = `None`,
/// 1 = `Particle`, 2 = `Affix` (matching
/// [`crate::morphology`]'s own `depth_from_bucket` convention).
fn depth_from_bucket(bucket: usize) -> MorphDepth {
    match bucket {
        0 => MorphDepth::None,
        1 => MorphDepth::Particle,
        _ => MorphDepth::Affix,
    }
}

/// Draw `species`' Number/Tense grammaticalization depths and attachment
/// sides — four permanent streams:
/// `language/<species>/grammar/depth/number`,
/// `language/<species>/grammar/depth/tense`,
/// `language/<species>/grammar/number-position`,
/// `language/<species>/grammar/tense-position`. Drawn, independent of
/// evidentiality/noun-class (never shares a stream or a weight table with
/// [`crate::morphology::morph_depths`]).
/// type-audit: bare-ok(identifier-text)
pub fn paradigm_depths(seed: &Seed, species: &str) -> ParadigmDepths {
    let mut number_stream = seed
        .derive("language")
        .derive(species)
        .derive("grammar")
        .derive("depth")
        .derive("number")
        .stream();
    let number_depth = depth_from_bucket(
        number_stream
            .weighted_index(&NUMBER_DEPTH_WEIGHTS)
            .expect("NUMBER_DEPTH_WEIGHTS is fixed and positive"),
    );

    let mut tense_stream = seed
        .derive("language")
        .derive(species)
        .derive("grammar")
        .derive("depth")
        .derive("tense")
        .stream();
    let tense_depth = depth_from_bucket(
        tense_stream
            .weighted_index(&TENSE_DEPTH_WEIGHTS)
            .expect("TENSE_DEPTH_WEIGHTS is fixed and positive"),
    );

    let mut number_pos_stream = seed
        .derive("language")
        .derive(species)
        .derive("grammar")
        .derive("number-position")
        .stream();
    let number_position = if number_pos_stream.range_u32(1, 100) <= NUMBER_POSITION_SUFFIX_CHANCE {
        ClassPosition::Suffix
    } else {
        ClassPosition::Prefix
    };

    let mut tense_pos_stream = seed
        .derive("language")
        .derive(species)
        .derive("grammar")
        .derive("tense-position")
        .stream();
    let tense_position = if tense_pos_stream.range_u32(1, 100) <= TENSE_POSITION_SUFFIX_CHANCE {
        ClassPosition::Suffix
    } else {
        ClassPosition::Prefix
    };

    ParadigmDepths {
        number_depth,
        tense_depth,
        number_position,
        tense_position,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn paradigm_depths_is_pure() {
        let a = paradigm_depths(&Seed(1), "test");
        let b = paradigm_depths(&Seed(1), "test");
        assert_eq!(a, b);
    }

    #[test]
    fn paradigm_depths_covers_all_three_buckets_across_many_seeds() {
        // Not a single fixed outcome — confirm the weighted draw actually
        // reaches every MorphDepth bucket over enough seeds, the same
        // sanity check style morph_depths' own test suite uses.
        let mut saw_none = false;
        let mut saw_particle = false;
        let mut saw_affix = false;
        for i in 0..200u64 {
            let d = paradigm_depths(&Seed(i), "test");
            match d.number_depth {
                MorphDepth::None => saw_none = true,
                MorphDepth::Particle => saw_particle = true,
                MorphDepth::Affix => saw_affix = true,
            }
        }
        assert!(saw_none && saw_particle && saw_affix);
    }
}
