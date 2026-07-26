//! The Assay's property battery: the invariants every later campaign in The
//! Crucible inherits.

use hornvale_alchemy::Substrate;
use hornvale_alchemy::production::{PRODUCTIONS, admits, apply, permits};
use hornvale_alchemy::quality::{Quality, QualityVector, qualities_of};
use hornvale_alchemy::sign::signs_of;

/// A coarse sweep of the substrate space, used by several properties below.
fn sweep() -> Vec<Substrate> {
    let mut out = Vec::new();
    for m in [0.0, 0.5, 1.0] {
        for o in [0.0, 0.5, 1.0] {
            for s in [0.0, 0.5, 1.0] {
                for r in [0.0, 0.5, 1.0] {
                    for p in [0.0, 0.5, 1.0] {
                        out.push(Substrate {
                            metallic: m,
                            organic: o,
                            saline: s,
                            refractory: r,
                            purity: p,
                        });
                    }
                }
            }
        }
    }
    out
}

#[test]
fn every_production_balances_mass() {
    for p in PRODUCTIONS {
        assert!(permits(p), "{} does not balance", p.name);
    }
}

#[test]
fn production_names_are_unique() {
    let mut names: Vec<&str> = PRODUCTIONS.iter().map(|p| p.name).collect();
    names.sort_unstable();
    let before = names.len();
    names.dedup();
    assert_eq!(before, names.len(), "duplicate production name");
}

#[test]
fn qualities_and_signs_stay_in_range_across_the_sweep() {
    for s in sweep() {
        let q = qualities_of(&s);
        for axis in Quality::ALL {
            let v = q.get(axis);
            assert!((0.0..=1.0).contains(&v), "{axis:?} out of range at {s:?}");
        }
        let sg = signs_of(&q);
        for channel in [sg.heft, sg.grain, sg.lustre, sg.odour, sg.hue] {
            assert!((0.0..=1.0).contains(&channel), "sign out of range at {s:?}");
        }
    }
}

/// Derivation is a pure function: the same substrate always yields the same
/// qualities. The Assay draws nothing, so this must hold trivially -- and is
/// asserted anyway, because it is the claim a later change is most likely to
/// break silently.
#[test]
fn derivation_is_pure() {
    for s in sweep() {
        assert_eq!(qualities_of(&s), qualities_of(&s));
    }
}

/// No production is dead: each one admits at least one reachable substance.
/// An unreachable production would be authored vocabulary nothing can ever
/// use.
#[test]
fn no_production_is_unreachable() {
    let space: Vec<QualityVector> = sweep().iter().map(qualities_of).collect();
    for p in PRODUCTIONS {
        assert!(
            space.iter().any(|q| admits(p, q)),
            "{} is unreachable from any substrate",
            p.name
        );
    }
}

/// No production admits EVERYTHING: a precondition that always holds is not a
/// precondition, and would make the material layer uniform.
#[test]
fn no_production_admits_everything() {
    let space: Vec<QualityVector> = sweep().iter().map(qualities_of).collect();
    for p in PRODUCTIONS {
        if p.requires.is_empty() {
            continue;
        }
        assert!(
            space.iter().any(|q| !admits(p, q)),
            "{} admits every substance -- its requirements are vacuous",
            p.name
        );
    }
}

/// The spec's §8 evidence item 1: no production produces a quality vector
/// outside [0,1]. Discharged by actually applying every production's
/// outputs to every substrate in the sweep -- previously untestable,
/// because nothing ever called `apply`.
#[test]
fn no_production_output_leaves_the_unit_interval() {
    for s in sweep() {
        let q = qualities_of(&s);
        for p in PRODUCTIONS {
            for output in p.outputs {
                let product = apply(output, &q);
                for axis in Quality::ALL {
                    let v = product.get(axis);
                    assert!(
                        (0.0..=1.0).contains(&v),
                        "{} output leaves [0,1] on {axis:?} at {s:?}: {v}",
                        p.name
                    );
                }
            }
        }
    }
}
