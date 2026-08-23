//! The Penstock stage 1, Task 2: the two `liveness` folds that used to scan
//! every agent's whole history per agent per tick now use the SPO-indexed
//! `Ledger::facts_of`. This pins that the swap is behaviour-preserving —
//! the old expression and the new one select the same facts, in the same
//! order, on a ledger where the two could differ.

use hornvale_kernel::{ConceptRegistry, Fact, Ledger, Value, test_lineage};

/// The predicate both call sites read. Kept as a literal rather than
/// imported so this test still fails if the constant is repointed.
const AGENT_AT: &str = "agent-at";

#[test]
fn facts_of_selects_what_find_and_filter_selected() {
    let mut r = ConceptRegistry::default();
    r.register_predicate(AGENT_AT, false, "").unwrap();
    let mut l = Ledger::default();
    let a = l.mint_entity(test_lineage(0));
    let b = l.mint_entity(test_lineage(1));

    // Interleaved subjects with descending objects, so index-key order and
    // commit order differ and a same-contents-wrong-order bug is visible.
    for (subj, room) in [(a, "r3"), (b, "r9"), (a, "r2"), (b, "r8"), (a, "r1")] {
        l.commit(
            Fact {
                subject: subj,
                predicate: AGENT_AT.to_string(),
                object: Value::Text(room.to_string()),
                place: None,
                day: None,
                provenance: "t".to_string(),
            },
            &r,
        )
        .unwrap();
    }

    for subj in [a, b] {
        let old: Vec<&Fact> = l.find(AGENT_AT).filter(|f| f.subject == subj).collect();
        let new: Vec<&Fact> = l.facts_of(subj, AGENT_AT).collect();
        assert_eq!(
            old, new,
            "the swap must be behaviour-preserving for {subj:?}"
        );
    }
}
