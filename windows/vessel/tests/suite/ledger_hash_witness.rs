//! The Pawl's byte-identity witness: the drift check cannot see a ticked
//! ledger (no committed artifact carries one), so this test IS the check.
//! A fold that changes any drive's value changes some creature's route,
//! which changes the committed `agent-at` trail, which changes this hash.
//!
//! POSITIVE CONTROL (recorded, Task 1): `integrate_thirst`'s segment
//! accumulation in `windows/vessel/src/liveness.rs` —
//! `total += rate * (e - s);` mutated to `total += rate * (e - s) * 1.5;`
//! (scaling every thirst-integral segment by 1.5x, so the drive crosses its
//! action thresholds at different simulated instants and the creature's
//! route — hence its committed `agent-at` trail — diverges). Green hash is
//! [`EXPECTED`] (`0x214d_b29c_f466_8067`, recorded from two agreeing runs);
//! under the mutation the same script produced `0x84833ba833d1e9b0` — a
//! different value, confirming the test reddens on a real behavioural
//! change and not merely on noise. Mutated with `scripts/mutate.py`,
//! restored with `git checkout -- windows/vessel/src/liveness.rs`, and the
//! green hash reconfirmed on a rebuilt binary before this constant was
//! committed (a restored source with a stale binary is a known trap).
//!
//! **Accessor decision**: no new accessor was added.
//! `Session::session_ledger_json` already serializes the whole committed
//! `Ledger` (every `Fact`'s `subject`, `predicate`, `object`, `place`, `day`
//! and `provenance`), so this test hashes that string directly rather than
//! reconstructing a per-fact pipe-joined line — the JSON already carries
//! every field the brief asked for, and hashing the session's own
//! determinism accessor means this witness rides the same surface T3's
//! determinism test already established.
//!
//! Hashing is hand-rolled FNV-1a (the workspace has no hashing crate;
//! `kernel/src/seed.rs` keeps the same two constants private, so they are
//! duplicated here rather than exposed).

use crate::common;
use hornvale_vessel::{PossessOpts, Session};

/// FNV-1a's standard 64-bit offset basis. Duplicated from (not re-exported
/// by) `kernel/src/seed.rs`'s private constant of the same name and value —
/// this test has no business reaching into the kernel's seed-derivation
/// internals, and the constant is public domain arithmetic, not a secret.
const FNV_OFFSET_BASIS: u64 = 0xcbf2_9ce4_8422_2325;
/// FNV-1a's standard 64-bit prime. See [`FNV_OFFSET_BASIS`].
const FNV_PRIME: u64 = 0x0000_0100_0000_01b3;

/// Hash `bytes` with plain FNV-1a: XOR each byte into the running hash, then
/// multiply by the prime. No relation to `Seed::derive`'s variant (which
/// seeds the offset with a parent seed) — this is the textbook algorithm.
fn fnv1a(bytes: &[u8]) -> u64 {
    let mut h = FNV_OFFSET_BASIS;
    for &b in bytes {
        h ^= u64::from(b);
        h = h.wrapping_mul(FNV_PRIME);
    }
    h
}

/// The fixed script: 30 `wait`s, then `look`, then 30 more `wait`s (60 ticks
/// total — enough for at least two NPCs' drives to reset at least once, which
/// this function itself verifies via `committed_fact_count_for` before
/// returning, rather than trusting the tick count alone).
fn run_fixed_script(session: &mut Session<'_>) {
    let entities: Vec<_> = session.bodies().iter().map(|b| b.entity).collect();
    assert!(
        entities.len() >= 2,
        "seed 42's default session derives at least two bodies to \
         demonstrate the drive-reset claim below"
    );
    let before: Vec<usize> = entities
        .iter()
        .map(|&e| session.committed_fact_count_for(e))
        .collect();

    for _ in 0..30 {
        session.handle("wait");
    }
    session.handle("look");
    for _ in 0..30 {
        session.handle("wait");
    }

    let after: Vec<usize> = entities
        .iter()
        .map(|&e| session.committed_fact_count_for(e))
        .collect();
    let grew = before
        .iter()
        .zip(after.iter())
        .filter(|(b, a)| a > b)
        .count();
    assert!(
        grew >= 2,
        "expected at least 2 of {} bodies to commit new facts over the \
         60-tick script (drives resetting at least once each) — only {grew} \
         did: before = {before:?}, after = {after:?}",
        entities.len()
    );
}

/// The byte-identity witness itself: the seed-42 `possess` script's final
/// ledger, hashed. `EXPECTED` was recorded from two consecutive runs of this
/// test agreeing (determinism first, per the module doc's positive control),
/// before being committed as a literal.
const EXPECTED: u64 = 0x214d_b29c_f466_8067;

#[test]
fn the_seed_42_walk_commits_the_same_ledger_bytes() {
    let world = common::build(42).expect("seed 42 always builds a world");
    let (mut session, _opening) =
        Session::start(&world, &PossessOpts::default()).expect("seed 42 always starts a session");

    run_fixed_script(&mut session);

    let json = session.session_ledger_json();
    let hash = fnv1a(json.as_bytes());
    assert_eq!(
        hash, EXPECTED,
        "ledger hash changed: got {hash:#018x}, expected {EXPECTED:#018x} — \
         some committed fact's bytes moved for the fixed seed-42 script"
    );
}
