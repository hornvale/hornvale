//! The Culvert — the water belief's route memo, and the instruments that
//! measure it.
//!
//! Spec: `docs/superpowers/specs/2026-09-05-the-culvert-design.md`.

use hornvale_vessel::liveness;

/// **Every predicate the drive stack COMMITS is one the roster REGISTERS.**
///
/// This is the guard for a defect that has now happened twice: `SLEPT_ON`
/// (The Pallet, 2026-09-03) was added to the drive stack and to
/// `Session::start`, and the two benches that hand-copy the same list were
/// not updated, so both panicked with `UnknownPredicate` for two days across
/// two merged campaigns. Nothing caught it because `--all-targets` COMPILES
/// an example and no gate RUNS one.
///
/// The direction this enforces is `committed ⊆ registered`. It is blind to
/// over-registration — a roster entry no drive ever commits passes here — and
/// that is the safe direction: an extra registration is inert, a missing one
/// is a panic.
#[test]
fn every_drive_predicate_the_stack_commits_is_on_the_roster() {
    let roster: std::collections::BTreeSet<&str> =
        liveness::DRIVE_PREDICATES.iter().map(|(p, _)| *p).collect();
    for pred in [
        liveness::AGENT_AT,
        liveness::DRANK,
        liveness::RESTED,
        liveness::SLEPT,
        liveness::SLEPT_ON,
        liveness::EATEN,
    ] {
        assert!(
            roster.contains(pred),
            "the drive stack commits `{pred}` and DRIVE_PREDICATES does not \
             register it — this is the `slept-on` defect recurring. Add it to \
             DRIVE_PREDICATES in windows/vessel/src/liveness.rs."
        );
    }
}

/// **The roster carries a doc for every entry, and no duplicates.**
///
/// `register_predicate` takes a doc string, and a registry entry with an empty
/// one is a registry entry nobody can read. A duplicate name would register
/// twice — idempotent today, but it would mean the roster had stopped being a
/// list of distinct predicates and nothing else would say so.
#[test]
fn the_drive_predicate_roster_is_well_formed() {
    let mut seen = std::collections::BTreeSet::new();
    for (name, doc) in liveness::DRIVE_PREDICATES {
        assert!(
            !name.is_empty(),
            "a roster entry has an empty predicate name"
        );
        assert!(!doc.is_empty(), "roster entry `{name}` has an empty doc");
        assert!(seen.insert(*name), "roster entry `{name}` appears twice");
    }
    assert!(
        seen.len() >= 6,
        "the roster holds {} entries; the drive stack commits at least six",
        seen.len()
    );
}
