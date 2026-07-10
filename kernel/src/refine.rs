//! Refinement engine, tier 0: pick deterministically among candidates
//! that contradict nothing (spec §3.1). This function's sophistication
//! grows for years; its signature shape should not.

use crate::ledger::{Fact, Ledger};
use crate::registry::ConceptRegistry;
use crate::seed::Stream;

/// Deterministically choose the index of a candidate whose fact would be
/// accepted by the ledger. Starts at a seeded offset and scans in order,
/// so the choice is uniform when unconstrained and consistent when
/// constrained. Returns None if no candidate survives.
/// type-audit: bare-ok(index)
pub fn choose_consistent<T>(
    stream: &mut Stream,
    ledger: &Ledger,
    registry: &ConceptRegistry,
    candidates: &[T],
    to_fact: impl Fn(&T) -> Fact,
) -> Option<usize> {
    if candidates.is_empty() {
        return None;
    }
    let offset = (stream.next_u64() % candidates.len() as u64) as usize;
    (0..candidates.len())
        .map(|i| (offset + i) % candidates.len())
        .find(|&i| ledger.check(&to_fact(&candidates[i]), registry).is_ok())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ledger::{EntityId, Value};
    use crate::seed::Seed;

    fn setup() -> (ConceptRegistry, Ledger, EntityId) {
        let mut r = ConceptRegistry::default();
        r.register_predicate("name", true, "canonical name")
            .unwrap();
        let mut l = Ledger::default();
        let e = l.mint_entity();
        (r, l, e)
    }

    fn name_fact(subject: EntityId, name: &str) -> Fact {
        Fact {
            subject,
            predicate: "name".to_string(),
            object: Value::Text(name.to_string()),
            place: None,
            day: None,
            provenance: "test".to_string(),
        }
    }

    #[test]
    fn choice_is_deterministic() {
        let (r, l, e) = setup();
        let candidates = ["Zaggrak", "Bolnar", "Mokru"];
        let a = choose_consistent(&mut Seed(5).stream(), &l, &r, &candidates, |n| {
            name_fact(e, n)
        });
        let b = choose_consistent(&mut Seed(5).stream(), &l, &r, &candidates, |n| {
            name_fact(e, n)
        });
        assert_eq!(a, b);
        assert!(a.is_some());
    }

    #[test]
    fn choice_varies_with_seed() {
        let (r, l, e) = setup();
        let candidates = ["a", "b", "c", "d", "e", "f", "g", "h"];
        let picks: Vec<Option<usize>> = (0..8)
            .map(|s| {
                choose_consistent(&mut Seed(s).stream(), &l, &r, &candidates, |n| {
                    name_fact(e, n)
                })
            })
            .collect();
        assert!(picks.windows(2).any(|w| w[0] != w[1]));
    }

    #[test]
    fn contradicting_candidates_are_skipped() {
        let (r, mut l, e) = setup();
        l.commit(name_fact(e, "Zaggrak"), &r).unwrap();
        // Committing a *different* name would contradict; only the
        // already-true name survives.
        let candidates = ["Bolnar", "Zaggrak", "Mokru"];
        for seed in 0..16 {
            let pick = choose_consistent(&mut Seed(seed).stream(), &l, &r, &candidates, |n| {
                name_fact(e, n)
            });
            assert_eq!(pick, Some(1));
        }
    }

    #[test]
    fn returns_none_when_nothing_survives() {
        let (r, mut l, e) = setup();
        l.commit(name_fact(e, "Zaggrak"), &r).unwrap();
        let candidates = ["Bolnar", "Mokru"];
        let pick = choose_consistent(&mut Seed(1).stream(), &l, &r, &candidates, |n| {
            name_fact(e, n)
        });
        assert_eq!(pick, None);
    }

    #[test]
    fn empty_candidates_yield_none() {
        let (r, l, e) = setup();
        let candidates: [&str; 0] = [];
        let pick = choose_consistent(&mut Seed(1).stream(), &l, &r, &candidates, |n| {
            name_fact(e, n)
        });
        assert_eq!(pick, None);
    }
}
