//! The incremental-fold primitive: advancing over a ledger prefix gives the
//! same answer as folding it from scratch (FOLD equals SCAN), and the position
//! guard makes a skipped or repeated fact loud rather than silent.

use hornvale_kernel::fold::{Folded, LedgerFold};
use hornvale_kernel::ledger::{EntityId, Fact, Ledger, Value};
use hornvale_kernel::registry::ConceptRegistry;

/// An ORDER-SENSITIVE test fold. `count` alone would pass even if facts were
/// absorbed in the wrong order, which is precisely the bug class this
/// primitive can have — so `rolling` mixes each fact's predicate length in a
/// way that does not commute.
#[derive(Debug, PartialEq)]
struct Probe {
    count: u64,
    rolling: u64,
}

impl LedgerFold for Probe {
    fn empty() -> Self {
        Probe {
            count: 0,
            rolling: 1,
        }
    }
    fn absorb(&mut self, fact: &Fact) {
        self.count += 1;
        self.rolling = self
            .rolling
            .wrapping_mul(31)
            .wrapping_add(fact.predicate.len() as u64);
    }
}

/// Two predicates of DIFFERENT name length, so `rolling` distinguishes their
/// order. Equal lengths would make the fold commutative and every test below
/// vacuous.
const SHORT: &str = "a";
const LONGER: &str = "abcd";

fn registry() -> ConceptRegistry {
    let mut r = ConceptRegistry::default();
    r.register_predicate(SHORT, false, "test").unwrap();
    r.register_predicate(LONGER, false, "test").unwrap();
    r
}

fn fact(predicate: &str, n: u64) -> Fact {
    Fact {
        subject: EntityId::new(n).expect("n is non-zero"),
        predicate: predicate.to_string(),
        object: Value::Flag(true),
        place: None,
        day: None,
        provenance: "test".to_string(),
    }
}

/// A ledger whose predicate sequence is deliberately mixed, so an order bug
/// moves `rolling`.
fn ledger_of(len: u64) -> Ledger {
    let reg = registry();
    let mut l = Ledger::default();
    for i in 1..=len {
        let p = if i % 3 == 0 { LONGER } else { SHORT };
        l.commit(fact(p, i), &reg).expect("a test fact commits");
    }
    l
}

#[test]
fn the_probe_fold_is_order_sensitive() {
    // Guards every other test in this file from being vacuous: a commutative
    // probe would pass them all under an ordering bug.
    let mut forward = Probe::empty();
    forward.absorb(&fact(SHORT, 1));
    forward.absorb(&fact(LONGER, 2));

    let mut backward = Probe::empty();
    backward.absorb(&fact(LONGER, 2));
    backward.absorb(&fact(SHORT, 1));

    assert_ne!(forward, backward, "the probe fold must not commute");
}

/// A self-consistency check, NOT the FOLD-equals-SCAN oracle: `Folded::rebuild`
/// is implemented by calling `advance_to` (see `Folded::rebuild`'s body), so a
/// bug inside `advance_to` is applied identically to both the "incremental"
/// and the "scan" side here and can cancel. This test pins that advancing in
/// two bites equals advancing in one — a real property, worth keeping — but a
/// tenant copying this file for its own FOLD-equals-SCAN test should copy
/// `one_fact_at_a_time_equals_folding_from_scratch` below instead, which is
/// the one that reaches the state through a path independent of `advance_to`.
#[test]
fn advancing_in_two_bites_equals_advancing_in_one() {
    let l = ledger_of(20);
    let scan: Folded<Probe> = Folded::rebuild(&l);

    // Advance in two bites, the way a tick does.
    let mut incremental: Folded<Probe> = Folded::new();
    let half = ledger_of(9);
    incremental.advance_to(&half);
    incremental.advance_to(&l);

    assert_eq!(incremental.state(), scan.state());
    assert_eq!(incremental.position(), scan.position());
}

/// The actual FOLD-equals-SCAN oracle, and the test to copy for a tenant's own
/// fold: `absorb_at` is a path independent of `advance_to`, so this is the one
/// test in this file that cannot be fooled by a bug shared between the
/// "incremental" and "scan" sides (unlike
/// `advancing_in_two_bites_equals_advancing_in_one` above, whose "scan" side
/// calls `advance_to` internally). A mutation confined to `advance_to` — for
/// example, an off-by-one in the skip count — moves this test's `one_by_one`
/// side but not `Folded::rebuild`'s, so the two disagree and the test catches
/// it.
#[test]
fn one_fact_at_a_time_equals_folding_from_scratch() {
    let l = ledger_of(20);
    let scan: Folded<Probe> = Folded::rebuild(&l);

    let mut one_by_one: Folded<Probe> = Folded::new();
    for (i, f) in l.iter().enumerate() {
        one_by_one.absorb_at(i as u64, f);
    }

    assert_eq!(one_by_one.state(), scan.state());
    assert_eq!(one_by_one.position(), scan.position());
}

#[test]
fn rebuilding_upto_a_position_equals_the_prefix_it_names() {
    let l = ledger_of(20);
    let upto = Folded::<Probe>::rebuild_upto(&l, 7);

    // The same prefix, reached the long way round.
    let prefix = ledger_of(7);
    let scan: Folded<Probe> = Folded::rebuild(&prefix);

    assert_eq!(upto.state(), scan.state());
    assert_eq!(upto.position(), 7);
}

#[test]
fn resuming_from_a_checkpoint_reaches_the_same_place_as_folding_throughout() {
    let l = ledger_of(20);

    // The checkpoint: whatever the state was at position 7.
    let checkpoint = Folded::<Probe>::rebuild_upto(&l, 7);
    let carried = Probe {
        count: checkpoint.state().count,
        rolling: checkpoint.state().rolling,
    };

    let mut resumed: Folded<Probe> = Folded::resume(carried, checkpoint.position());
    resumed.advance_to(&l);

    let scan: Folded<Probe> = Folded::rebuild(&l);
    assert_eq!(resumed.state(), scan.state());
    assert_eq!(resumed.position(), scan.position());
}

#[test]
fn rebuilding_upto_beyond_the_ledger_stops_at_the_ledger() {
    let l = ledger_of(5);
    let over = Folded::<Probe>::rebuild_upto(&l, 500);
    let all: Folded<Probe> = Folded::rebuild(&l);
    assert_eq!(over.state(), all.state());
    assert_eq!(over.position(), all.position());
}

#[test]
#[should_panic(expected = "exactly once")]
fn absorbing_the_same_position_twice_panics() {
    let l = ledger_of(3);
    let mut f: Folded<Probe> = Folded::new();
    let first = l.iter().next().expect("the ledger has facts");
    f.absorb_at(0, first);
    f.absorb_at(0, first); // the repeat bug, made loud
}

#[test]
#[should_panic(expected = "exactly once")]
fn skipping_a_position_panics() {
    let l = ledger_of(3);
    let mut f: Folded<Probe> = Folded::new();
    let second = l.iter().nth(1).expect("the ledger has 3 facts");
    f.absorb_at(1, second); // position 0 was never absorbed
}

/// CHAOS-REBUILD: metaplan §7's chaos-eviction rung, for folds.
///
/// Walk the ledger one fact at a time and, at EVERY position, throw the
/// accumulated state away and rebuild it from the ledger prefix — then carry
/// on. If the fold is genuinely a fold, the end state is identical to never
/// having discarded anything, for every possible discard schedule. This is the
/// most aggressive schedule there is — but "most aggressive" is not "most
/// diagnostic": this schedule alone gives NO signal on `absorb_at`'s own
/// purity, because a bug confined to `absorb_at` can cancel out when the
/// rebuild happens immediately after every single absorb (see the sparser,
/// every-third-position sibling test below, and spec §11, for why).
#[test]
fn discarding_the_state_at_every_position_is_unobservable() {
    let l = ledger_of(24);
    let resident: Folded<Probe> = Folded::rebuild(&l);

    let mut chaotic: Folded<Probe> = Folded::new();
    for (i, f) in l.iter().enumerate() {
        chaotic.absorb_at(i as u64, f);
        // Throw it all away and come back from the ledger.
        chaotic = Folded::rebuild_upto(&l, chaotic.position());
    }

    assert_eq!(
        chaotic.state(),
        resident.state(),
        "a discard schedule must not be observable"
    );
    assert_eq!(chaotic.position(), resident.position());
}

/// The same property under a SPARSER schedule. Not redundant: a bug can cancel
/// out under the every-step schedule — where the rebuilt state is recomputed
/// immediately after every single absorb — and survive one that lets several
/// absorbs accumulate between discards.
#[test]
fn discarding_the_state_at_every_third_position_is_unobservable() {
    let l = ledger_of(24);
    let resident: Folded<Probe> = Folded::rebuild(&l);

    let mut chaotic: Folded<Probe> = Folded::new();
    for (i, f) in l.iter().enumerate() {
        chaotic.absorb_at(i as u64, f);
        if i % 3 == 0 {
            chaotic = Folded::rebuild_upto(&l, chaotic.position());
        }
    }

    assert_eq!(chaotic.state(), resident.state());
    assert_eq!(chaotic.position(), resident.position());
}
