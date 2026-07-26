//! The production grammar: preconditions, a process, effects, and a
//! conservation witness.
//!
//! The authoring notation each entry in [`PRODUCTIONS`] transcribes is:
//!
//! ```text
//!   calcine:  [ volatility >= 0.4, fixity < 0.5 ]
//!             --fire-->
//!             [ volatility -= 0.4, fixity += 0.3 ]
//!             ! mass-balance
//!             ~ fume(acrid)
//! ```
//!
//! The `~` slot can only be filled by a [`Sign`], never by a quality, because
//! it is what an OBSERVER gets — the notation's required slots are what forced
//! the latent/manifest split in the first place.

use crate::quality::{Quality, QualityVector};
use crate::sign::Sign;

/// Tolerance for the mass-balance comparison. Fixed and tiny; the arithmetic
/// is a sum of authored constants, so this absorbs representation error only.
const BALANCE_EPSILON: f64 = 1e-9;

/// An authored operation a practitioner can perform.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Process {
    /// Reduce to powder.
    Grind,
    /// Drive with fire.
    Calcine,
    /// Take up in water.
    Dissolve,
    /// Separate by boiling and catching the vapour.
    Distil,
    /// Let living matter work on itself.
    Ferment,
    /// Combine two inputs into one body.
    Amalgamate,
}

impl Process {
    /// Every process, in declaration order.
    pub const ALL: [Process; 6] = [
        Process::Grind,
        Process::Calcine,
        Process::Dissolve,
        Process::Distil,
        Process::Ferment,
        Process::Amalgamate,
    ];
}

/// A precondition on one quality axis: `min <= value <= max`.
/// type-audit: bare-ok(ratio: min), bare-ok(ratio: max)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Requirement {
    /// The axis constrained.
    pub quality: Quality,
    /// Inclusive lower bound.
    pub min: f64,
    /// Inclusive upper bound.
    pub max: f64,
}

/// One product of a production.
/// type-audit: bare-ok(ratio: bulk), bare-ok(ratio: deltas)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Output {
    /// Share of the total input bulk this product carries. The `bulk` values
    /// of a production's outputs must sum to its input count.
    pub bulk: f64,
    /// Quality deltas applied to the input bundle to make this product.
    pub deltas: &'static [(Quality, f64)],
}

/// An authored production: the grammar's primitive.
/// type-audit: bare-ok(identifier-text: name), bare-ok(count: inputs)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Production {
    /// Stable identifier, used in tests and diagnostics.
    pub name: &'static str,
    /// The operation performed.
    pub process: Process,
    /// How many substance bodies go in (1 for most; 2 for `Amalgamate`).
    pub inputs: usize,
    /// Preconditions on the input bundle.
    pub requires: &'static [Requirement],
    /// What comes out.
    pub outputs: &'static [Output],
    /// The sign channel in which the reaction manifests to an observer.
    pub emits: Sign,
}

/// Admissibility: does this production balance mass?
///
/// The summed bulk of the outputs — fume and residue included — must equal the
/// input count. This is the mundane tier's single invariant; opening UNI-2
/// later relaxes exactly this predicate and nothing else in the architecture.
/// type-audit: bare-ok(flag: return)
pub fn permits(p: &Production) -> bool {
    let total: f64 = p.outputs.iter().map(|o| o.bulk).sum();
    (total - p.inputs as f64).abs() < BALANCE_EPSILON
}

/// Does a substance with these qualities satisfy the production's
/// preconditions?
/// type-audit: bare-ok(flag: return)
pub fn admits(p: &Production, q: &QualityVector) -> bool {
    p.requires.iter().all(|r| {
        let v = q.get(r.quality);
        v >= r.min && v <= r.max
    })
}

/// The authored production table. Universal — identical in every world. Per-
/// world difference arrives through which of these are REACHABLE, which
/// depends on what a world is made of.
pub const PRODUCTIONS: &[Production] = &[
    Production {
        name: "calcine-ore",
        process: Process::Calcine,
        inputs: 1,
        requires: &[Requirement {
            quality: Quality::Fixity,
            min: 0.4,
            max: 1.0,
        }],
        outputs: &[
            Output {
                bulk: 0.7,
                deltas: &[(Quality::Fixity, 0.2), (Quality::Malleability, 0.3)],
            },
            Output {
                bulk: 0.3,
                deltas: &[(Quality::Volatility, 0.5)],
            },
        ],
        emits: Sign::Odour,
    },
    Production {
        name: "dissolve-salt",
        process: Process::Dissolve,
        inputs: 1,
        requires: &[Requirement {
            quality: Quality::Solubility,
            min: 0.6,
            max: 1.0,
        }],
        outputs: &[Output {
            bulk: 1.0,
            deltas: &[(Quality::Solubility, 0.1)],
        }],
        emits: Sign::Hue,
    },
    Production {
        name: "grind-stone",
        process: Process::Grind,
        inputs: 1,
        requires: &[Requirement {
            quality: Quality::Malleability,
            min: 0.0,
            max: 0.3,
        }],
        outputs: &[Output {
            bulk: 1.0,
            deltas: &[],
        }],
        emits: Sign::Grain,
    },
    Production {
        name: "burn-fuel",
        process: Process::Calcine,
        inputs: 1,
        requires: &[Requirement {
            quality: Quality::Combustibility,
            min: 0.6,
            max: 1.0,
        }],
        outputs: &[
            Output {
                bulk: 0.2,
                deltas: &[(Quality::Combustibility, -0.6), (Quality::Fixity, 0.4)],
            },
            Output {
                bulk: 0.8,
                deltas: &[(Quality::Volatility, 0.6)],
            },
        ],
        emits: Sign::Odour,
    },
    Production {
        name: "distil-spirit",
        process: Process::Distil,
        inputs: 1,
        requires: &[Requirement {
            quality: Quality::Volatility,
            min: 0.5,
            max: 1.0,
        }],
        outputs: &[
            Output {
                bulk: 0.4,
                deltas: &[(Quality::Volatility, 0.3)],
            },
            Output {
                bulk: 0.6,
                deltas: &[(Quality::Volatility, -0.4), (Quality::Fixity, 0.2)],
            },
        ],
        emits: Sign::Odour,
    },
    Production {
        name: "ferment-must",
        process: Process::Ferment,
        inputs: 1,
        requires: &[Requirement {
            quality: Quality::Vitality,
            min: 0.5,
            max: 1.0,
        }],
        outputs: &[
            Output {
                bulk: 0.9,
                deltas: &[(Quality::Vitality, -0.2), (Quality::Volatility, 0.3)],
            },
            Output {
                bulk: 0.1,
                deltas: &[(Quality::Volatility, 0.7)],
            },
        ],
        emits: Sign::Odour,
    },
    Production {
        name: "amalgamate-alloy",
        process: Process::Amalgamate,
        inputs: 2,
        requires: &[Requirement {
            quality: Quality::Malleability,
            min: 0.5,
            max: 1.0,
        }],
        outputs: &[Output {
            bulk: 2.0,
            deltas: &[(Quality::Malleability, 0.1)],
        }],
        emits: Sign::Lustre,
    },
];

#[cfg(test)]
mod tests {
    use super::*;
    use crate::quality::QualityVector;

    /// Every authored production balances mass. This is the mundane tier's
    /// single invariant, and the one predicate UNI-2 would later relax.
    #[test]
    fn every_production_balances_mass() {
        for p in PRODUCTIONS {
            assert!(permits(p), "{} does not balance mass", p.name);
        }
    }

    /// Every process in the inventory is exercised by at least one
    /// production -- no dead vocabulary.
    #[test]
    fn every_process_is_used() {
        for process in Process::ALL {
            assert!(
                PRODUCTIONS.iter().any(|p| p.process == process),
                "{process:?} has no production"
            );
        }
    }

    /// A production admits a substance only when every requirement holds.
    #[test]
    fn admission_respects_requirements() {
        let burn = PRODUCTIONS
            .iter()
            .find(|p| p.name == "burn-fuel")
            .expect("burn-fuel exists");
        let fuel = QualityVector {
            combustibility: 0.8,
            ..QualityVector::default()
        };
        let stone = QualityVector {
            combustibility: 0.1,
            ..QualityVector::default()
        };
        assert!(admits(burn, &fuel));
        assert!(!admits(burn, &stone));
    }

    /// An unbalanced production is rejected -- proving `permits` can say no,
    /// rather than passing because it never fires.
    #[test]
    fn permits_rejects_an_unbalanced_production() {
        let bad = Production {
            name: "ex-nihilo",
            process: Process::Calcine,
            inputs: 1,
            requires: &[],
            outputs: &[Output {
                bulk: 1.5,
                deltas: &[],
            }],
            emits: Sign::Hue,
        };
        assert!(!permits(&bad), "1.5 out of 1.0 in must not balance");
    }
}
