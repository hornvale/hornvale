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
//!
//! # ALL THREE CONSTANTS WERE RE-RECORDED AT THE ABSORPTION OF `2c34f9e4c`
//!
//! The Wicket (fatigue is a stock, rest split from sleep, a bout graded by
//! the room it was taken in), The Roll (a session ticks the settlement's
//! whole roll) and The Avowal (kinship facts at genesis) all move what a
//! seeded walk commits, so every hash here moved. **The point of this section
//! is that the new numbers are MAIN'S, not this campaign's**, and that was
//! established main-first rather than inferred:
//!
//! A detached worktree of `origin/main` at `2c34f9e4c` — no campaign code in
//! it at all — was given this file's two scripts verbatim (the seed-42
//! 30-wait/`look`/30-wait script, and the eight-wait emitter script on the
//! seed the search lands on today), plus a throwaway port of
//! `Session::hazard_memories` minus its `folds` argument, and asked for the
//! same three hashes. Then the merged tree was asked. They agree exactly:
//!
//! ```text
//! witness              old constant          main @2c34f9e4c       merged
//! seed-42 ledger       0x214db29cf4668067    0xabc4731e5cf1ab21    0xabc4731e5cf1ab21
//! emitter ledger       0x64aeb2f93e38d328    0x9f9094408637b815    0x9f9094408637b815
//! emitter hazard       0x8cf62f13ee7098f5    0x0f999463ce4836a3    0x0f999463ce4836a3
//! ```
//!
//! The two seed-42 columns are the same seed and the same script, so that row
//! is a direct comparison. The emitter rows are NOT: the old pair was recorded
//! on seed 28 and the search lands on seed 6 now (see [`EMITTER_SEED`]), so
//! "old" there is a different world and the load-bearing comparison is the
//! main-vs-merged pair, which is one seed and one script. The hazard digest's
//! own denominators agree between the two trees as well — 127 bodies, 532
//! shunned rooms, 13 dreaded, and every dread magnitude bit-for-bit — which is
//! a stronger statement than the hash alone, since a hash that matched over
//! two empty digests would match for the wrong reason.
//!
//! **What this does and does not prove.** It proves that The Pawl's fold
//! store, threaded through the resolution of a 261-commit absorption,
//! reproduces main's own walk byte for byte on both scripts — which is the
//! only thing the resolution could have broken. It does not prove the
//! constants will survive main's next campaign; nothing can, and that is what
//! this test is for.

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
/// ledger, hashed. Re-recorded at the absorption of `2c34f9e4c`; main's own
/// hash on the same script was `0xabc4731e5cf1ab21`, and the merged tree
/// reproduces it. The value it replaces, `0x214db29cf4668067`, was recorded
/// before The Wicket, The Roll and The Avowal. See the module doc for the
/// procedure and the full table.
///
/// **The script's runtime was checked against the rule that would have
/// shortened it and does not trip it.** It costs 10.1 s on the merged tree
/// against 27.9 s for the slowest vessel test this campaign did not write
/// (`controller_swap::the_driven_bodys_suppressed_drive_is_retrievable_but_/// absent_from_what_it_says`), so the sixty waits stand unchanged even though
/// each one now advances sixty-eight bodies rather than seven.
const EXPECTED: u64 = 0xabc4_731e_5cf1_ab21;

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

// ---------------------------------------------------------------------------
// The SECOND byte-identity witness (The Pawl, Task 5): an emitter-bearing
// world, and the hazard memories the seed-42 hash cannot see.
// ---------------------------------------------------------------------------

/// How many `wait`s the emitter script takes. Kept small on purpose: the
/// search below pays a world build per candidate seed, and the property it
/// searches for appears within a handful of ticks or not at all.
const EMITTER_SCRIPT_WAITS: usize = 8;

/// The fixed script for the emitter-bearing witness.
fn run_emitter_script(session: &mut Session<'_>) {
    for _ in 0..EMITTER_SCRIPT_WAITS {
        session.handle("wait");
    }
}

/// The first seed whose hazard fold actually REPLAYS an emitter's affect at a
/// past visit day — the path this campaign's stage 2 rebuilt and the seed-42
/// hash is blind to.
///
/// Searched rather than pinned, for `common`'s own stated reason: a hardcoded
/// seed stops exercising the feature the first time the world moves under it,
/// silently. [`EMITTER_SEED`] records which seed the search lands on today, so
/// a move is loud rather than invisible, but the search — not the constant —
/// is what selects the world.
fn emitter_bearing_world() -> (u64, hornvale_kernel::World) {
    common::world_where(
        "the hazard fold replays an emitter's affect at a past visit day",
        |session| {
            run_emitter_script(session);
            session.resident_alarm_replays() > 0
        },
    )
}

/// The seed [`emitter_bearing_world`] lands on today.
///
/// **IT WAS 28 UNTIL THE ABSORPTION OF `2c34f9e4c` AND IT IS 6 NOW, AND THIS
/// CONSTANT EXISTS SO THAT MOVE IS LOUD.** The assertion beneath the search
/// fired exactly as its own message asks it to: "that is a finding about the
/// sim, not a broken test". The finding is that WHICH worlds replay an
/// emitter's affect at a past visit day is not what it was, because The Roll
/// changed who is on the roll at all — a session ticks the settlement's whole
/// roll now, so the emitter-scan population a walk builds is a different
/// population, and the halo pre-filter and terrain shortcut that used to keep
/// seed 42's replay count at zero now let a great many more rooms through on
/// the seeds that reach the path at all. The magnitude is worth stating rather
/// than the fact alone: the eight-wait script that reported **10** past-day
/// replays on seed 28 reports **2,125** on seed 6, over 127 bodies, from 2,565
/// emitter scans of which 8 find an emitter. This is a re-measurement of a
/// different world, not a like-for-like ratio, and the search — not the
/// constant — is still what selects it.
///
/// The paragraphs below are the pre-absorption record and are kept because the
/// reasoning is what makes the search a search. Their COUNTS are pre-Roll and
/// must not be read forward.
///
/// **Seed 42 is not it, and the reason corrects a claim this campaign carried
/// through four tasks.** The ledger and several doc comments said seed 42's
/// "emitter scan is empty". Measured over `common::SIGHT_SEEDS` with the
/// store's own counters, **on a ten-wait script**: seed 42 builds 70 emitter
/// scans and **20 of them find an emitter** — the scan is not empty at all.
/// What seed 42 has none of is a past-day REPLAY: every remembered room is
/// either already terrain-frightening (the terrain shortcut `continue`s) or
/// outside `alarm_source_rooms` (the halo pre-filter), so `emitter_arousal` is
/// never reached from inside the hazard fold. Of the 64 seeds swept, exactly
/// **two** reach it — seed 28 with 14 replays and seed 55 with 20 — which is
/// why this witness searches instead of assuming.
///
/// **Every replay count in this file carries its script length, because they
/// differ and a bare number invites the wrong comparison.** The sweep above
/// ran ten waits; the fixed script this witness hashes runs
/// [`EMITTER_SCRIPT_WAITS`] (8), and reports **10** replays on seed 28 rather
/// than that sweep's 14. Both are correct measurements of different scripts.
const EMITTER_SEED: u64 = 6;

/// The emitter-bearing world's final ledger hash, recorded from two agreeing
/// runs before being committed.
///
/// # POSITIVE CONTROL (recorded, Task 5 fix round 1)
///
/// The campaign's first attempt at a control for these two constants was spec
/// §3 rule 5's wrong-reset mutation (`Sustenance::last_reset` returning the
/// reset BEFORE the correct one). **It left this test green**, because no
/// creature on seed 28 drinks twice inside the fixed script, so the mutation
/// was a no-op for every entity the hazard path reads. A control that cannot
/// fire proves nothing about the instrument it is meant to validate, so a
/// second one was taken ON the hazard path itself:
///
/// `windows/vessel/src/liveness.rs`, in `hazard_memory_memo`'s transient loop
/// — the PAST-DAY replay, not `alarm_field_memo`'s present-day emitter probe:
///
/// ```text
///   alarm += emitter_arousal(afraid, ledger, folds, m, day, terrain);
/// → alarm += emitter_arousal(afraid, ledger, folds, m, day, terrain) * 1.5;
/// ```
///
/// Applied with `scripts/mutate.py`. Under it the same fixed script produced:
///
/// ```text
/// 94 emitter scans, 16 with an emitter, 10 PAST-DAY affect replays inside the hazard fold
/// ledger 0xeb203415776db502, hazard 0xde28e2f4f828e62d over 7 bodies (61 shunned rooms, 1 dreaded)
///   dread: entity 9630022852472602626 at Facet { .. } = 0.675 (0x3fe599999999999a)
///   LEDGER: got 0xeb203415776db502, expected 0x64aeb2f93e38d328
///   HAZARD: got 0xde28e2f4f828e62d, expected 0x8cf62f13ee7098f5
/// ```
///
/// against the green `ledger 0x64aeb2f93e38d328, hazard 0x8cf62f13ee7098f5
/// over 7 bodies (59 shunned rooms, 1 dreaded)` with `dread = 0.45
/// (0x3fdccccccccccccd)`. **Both hashes moved, and neither floor tripped** —
/// 61 shunned and 1 dreaded, so the red is a moved hash and not a witness
/// that stopped witnessing. The dread magnitude moved by exactly the mutated
/// factor (0.45 → 0.675), which is the evidence that the moved HAZARD hash is
/// the past-day replay's own value rather than a side effect of the changed
/// route. Restored with `scripts/mutate.py`, REBUILT, and both constants
/// reconfirmed green before this doc was written (a restored source with a
/// stale binary is a known trap).
///
/// # RE-RECORDED AT THE ABSORPTION OF `2c34f9e4c`
///
/// On seed 6, the world the search lands on now. Main's own hash on the same
/// eight-wait script on the same seed was `0x9f9094408637b815`, and the merged
/// tree reproduces it. The value it replaces, `0x64aeb2f93e38d328`, was
/// recorded on seed 28 and is a different world, so it is not a comparison —
/// see the module doc's table, which says so in the same words.
///
/// The positive control below was taken on seed 28 before the absorption and
/// has NOT been re-run on seed 6. It is kept because what it established is a
/// property of the instrument rather than of the world — that both hashes move
/// under a mutation to the past-day replay, and that the dread magnitude moves
/// by exactly the mutated factor — and because a control deleted for being old
/// leaves nothing at all. Its printed numbers are seed 28's.
const EMITTER_LEDGER_HASH: u64 = 0x9f90_9440_8637_b815;

/// The emitter-bearing world's final HAZARD hash — every derived body's
/// `shunned` set and `dread` map, rendered canonically (see
/// [`hazard_digest`]). This is the half [`EXPECTED`] cannot see: `dread` is
/// felt rather than committed, so a change to the past-day affect path that
/// did not happen to flip a route would move nothing in the ledger.
///
/// Re-recorded at the absorption of `2c34f9e4c`; main's own hash on the same
/// script was `0x0f999463ce4836a3`, and the merged tree reproduces it. **This
/// is the strongest single line of the absorption's evidence**, because the
/// two trees agree not merely on the digest's hash but on everything the
/// digest is built from — 127 bodies, 532 shunned rooms, 13 dread entries, and
/// each magnitude's exact bit pattern (twelve at `0x3ff0000000000000` and one
/// at `0x3fdababa2c43f7ee`). Dread is the half [`EXPECTED`] cannot see, and it
/// is produced only by the past-day affect replay, which is precisely the path
/// The Pawl rebuilt.
const EMITTER_HAZARD_HASH: u64 = 0x0f99_9463_ce48_36a3;

/// A canonical, order-fixed rendering of every body's hazard memory.
///
/// `f64` dread magnitudes go in as their exact bit patterns rather than as
/// formatted text: this is a byte-identity witness, and a decimal rendering
/// would quietly absorb a change in the last ulp — the one place a past-day
/// affect read is most likely to move.
fn hazard_digest(
    memories: &[(
        hornvale_kernel::EntityId,
        hornvale_vessel::liveness::HazardMemory,
    )],
) -> String {
    let mut out = String::new();
    for (entity, mem) in memories {
        out.push_str(&format!("e{}|", entity.0.get()));
        for room in &mem.shunned {
            out.push_str(&format!("s{room:?}|"));
        }
        for (room, magnitude) in &mem.dread {
            out.push_str(&format!("d{room:?}={:016x}|", magnitude.to_bits()));
        }
        out.push('\n');
    }
    out
}

#[test]
fn the_emitter_bearing_walk_commits_the_same_ledger_and_hazard_bytes() {
    let (seed, world) = emitter_bearing_world();
    println!("--- the emitter-bearing byte-identity witness: seed {seed} ---");
    assert_eq!(
        seed, EMITTER_SEED,
        "the search moved off the seed this witness's constants were recorded on. That is \
         a finding about the sim, not a broken test: re-record both hashes on the new \
         seed, and say in the campaign record what changed about which worlds replay an \
         emitter's affect at a past day"
    );

    let (mut session, _opening) =
        Session::start(&world, &PossessOpts::default()).expect("the found world starts a session");
    run_emitter_script(&mut session);

    // THE DENOMINATOR. Without this the two hashes below could be pinning a
    // world whose hazard fold never entered the transient path at all, which
    // is exactly the blindness this second witness exists to remove.
    let replays = session.resident_alarm_replays();
    let scans = session.resident_emitter_scans();
    let with_emitters = session.resident_emitter_scans_with_emitters();
    println!(
        "{scans} emitter scans, {with_emitters} with an emitter, {replays} PAST-DAY affect \
         replays inside the hazard fold"
    );
    assert!(
        replays > 0,
        "the script must reach the hazard fold's past-day affect replay on this world, or \
         both hashes below are witnessing the same terrain-only path seed 42 already \
         covers"
    );

    let ledger_hash = fnv1a(session.session_ledger_json().as_bytes());
    let memories = session.hazard_memories();
    let hazard_hash = fnv1a(hazard_digest(&memories).as_bytes());
    let shunned: usize = memories.iter().map(|(_, m)| m.shunned.len()).sum();
    let dread: usize = memories.iter().map(|(_, m)| m.dread.len()).sum();
    println!(
        "ledger {ledger_hash:#018x}, hazard {hazard_hash:#018x} over {} bodies \
         ({shunned} shunned rooms, {dread} dreaded)",
        memories.len()
    );
    // The dread magnitudes themselves, printed because they are the one
    // quantity here that ONLY the past-day affect replay can produce: a
    // remembered alarm's magnitude is `emitter_arousal`'s return value at the
    // room's remembered visit day. A hash that moved could have moved for a
    // route change; a moved magnitude cannot have.
    for (entity, mem) in &memories {
        for (room, magnitude) in &mem.dread {
            println!(
                "  dread: entity {} at {room:?} = {magnitude:?} ({:#018x})",
                entity.0.get(),
                magnitude.to_bits()
            );
        }
    }
    assert!(
        shunned > 0,
        "the hazard digest must have something in it, or its hash is the hash of a list of \
         empty sets and would not move for any change to this path"
    );
    assert!(
        dread > 0,
        "the digest must carry at least one DREAD entry, or the past-day affect replay \
         reached nothing that the hash could witness: dread is the only part of a hazard \
         memory that a remembered alarm — and so the replay's own reset semantics — can \
         put there"
    );

    // BOTH verdicts are computed before either can panic, and that is not
    // stylistic. An `assert_eq!` on the ledger followed by one on the hazard
    // digest reports only the first: this witness's own positive control (see
    // the module doc) moves BOTH, so the hazard verdict — the half seed 42
    // cannot give at all — would never have been printed under the very
    // mutation that was meant to validate it.
    let mut moved: Vec<String> = Vec::new();
    if ledger_hash != EMITTER_LEDGER_HASH {
        moved.push(format!(
            "LEDGER: got {ledger_hash:#018x}, expected {EMITTER_LEDGER_HASH:#018x}"
        ));
    }
    if hazard_hash != EMITTER_HAZARD_HASH {
        moved.push(format!(
            "HAZARD: got {hazard_hash:#018x}, expected {EMITTER_HAZARD_HASH:#018x} — some \
             creature's remembered-frightening ground or its dread magnitude moved"
        ));
    }
    assert!(
        moved.is_empty(),
        "the emitter-bearing witness moved for the fixed script on seed {seed}:\n  {}",
        moved.join("\n  ")
    );
}
