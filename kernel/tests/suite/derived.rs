//! `Derived` store properties: the cache is invisible, and the key is complete.

use hornvale_kernel::derived::{DepKey, Derived, Validity};
use hornvale_kernel::ledger::EntityId;

/// A stand-in pure derivation with a COMPLETE key: the value is a function
/// of the key and nothing else.
fn square(k: u64) -> u64 {
    k * k
}

fn read_through(store: &mut Derived<u64, u64>, k: u64) -> u64 {
    if let Some(v) = store.get(&k) {
        return *v;
    }
    let computed = square(k);
    store.insert(k, computed);
    computed
}

#[test]
fn cache_equals_recompute_over_a_repeating_key_sequence() {
    let mut store: Derived<u64, u64> = Derived::new();
    // A deterministic pseudo-sequence with repeats, so hits and misses mix.
    // No rand crate exists in this workspace (decision 0004) and none is added.
    let keys: Vec<u64> = (0..400u64)
        .map(|i| i.wrapping_mul(2_654_435_761) % 37)
        .collect();

    for k in &keys {
        assert_eq!(
            read_through(&mut store, *k),
            square(*k),
            "cache diverged from recomputation at {k}"
        );
    }
    assert!(store.hits() > 0, "a repeating sequence must produce hits");
    assert!(store.misses() > 0, "a cold start must produce misses");
}

#[test]
fn eviction_at_every_opportunity_changes_nothing_observable() {
    // CHAOS EVICTION -- the adversarial rung. For a Pure entry this is also
    // the KEY-COMPLETENESS test: dropping the entry forces recomputation
    // through the declared key alone, so a derivation reading anything the key
    // does not carry would diverge here.
    let keys: Vec<u64> = (0..200u64)
        .map(|i| i.wrapping_mul(2_654_435_761) % 23)
        .collect();

    let mut resident: Derived<u64, u64> = Derived::new();
    let mut chaotic: Derived<u64, u64> = Derived::new();
    let mut resident_out = Vec::new();
    let mut chaotic_out = Vec::new();

    for k in &keys {
        resident_out.push(read_through(&mut resident, *k));
        chaotic_out.push(read_through(&mut chaotic, *k));
        chaotic.evict_all(); // evict at EVERY legal opportunity
    }
    assert_eq!(
        resident_out, chaotic_out,
        "an evicted run must be byte-identical to a resident one"
    );
}

#[test]
fn pure_is_never_stale_and_ledger_is_stale_only_when_a_watched_dep_is_touched() {
    // The property Validity exists to hold: a Pure entry is never stale at
    // any ledger position, and a Ledger entry becomes stale exactly when a
    // later fact touches a key in its deps -- neither earlier (no touch yet)
    // nor merely because the ledger grew (growth elsewhere is not a touch).
    let e1 = EntityId::new(1).expect("1 is a valid EntityId");
    let e2 = EntityId::new(2).expect("2 is a valid EntityId");

    let watched = DepKey {
        subject: e1,
        predicate: "likes".to_string(),
        place: None,
    };
    let unrelated = DepKey {
        subject: e2,
        predicate: "likes".to_string(),
        place: None,
    };

    let pure = Validity::Pure;
    assert!(!pure.is_stale(0, &[]), "Pure is never stale, cold");
    assert!(
        !pure.is_stale(1_000_000, std::slice::from_ref(&watched)),
        "Pure is never stale, however far the ledger has grown or whatever it touched"
    );

    let ledger = Validity::Ledger {
        position: 5,
        deps: vec![watched.clone()],
    };

    // No growth since the recorded position: not stale.
    assert!(!ledger.is_stale(5, &[]));

    // Growth, but the touched facts do not name a watched dependency: not
    // stale -- growth alone is not the trigger.
    assert!(!ledger.is_stale(6, std::slice::from_ref(&unrelated)));

    // Growth AND a touched fact names the watched dependency: stale.
    assert!(ledger.is_stale(6, std::slice::from_ref(&watched)));
}
