//! The Pawl's byte-identity witnesses: the drift check cannot see a ticked
//! ledger (no committed artifact carries one), so these tests are the only
//! thing that looks at one. A fold that changes any drive's value changes some
//! creature's route, which changes the committed `agent-at` trail, which
//! changes the hash they print.
//!
//! **WHAT THEY ASSERT, AS OF THE CAMPAIGN'S CLOSE (2026-09-02): DETERMINISM AND
//! NON-VACUITY, NOT A COMMITTED HASH.** Each witness runs its fixed script
//! TWICE, on two fresh sessions of one seed, and requires the two to agree —
//! on the committed ledger bytes, and (for the emitter witness) on the derived
//! hazard digest as well. Every denominator the old constants rode on is still
//! asserted, on both runs. The hashes are PRINTED on every run so a future
//! migration has the numbers without this file gating on them.
//!
//! # WHY THE CONSTANTS RETIRED
//!
//! A committed constant here equalled "the whole seed-42 walk's behaviour". It
//! therefore reddened on **any** behaviour change by **any** campaign — The
//! Roll alone moved all three — which made it a tax on campaigns that had
//! nothing to do with these folds, and an unwinnable race against a queue that
//! gates the merge of main+branch rather than a branch tip: main can move
//! between recording the number and landing it.
//!
//! The constants were not a mistake. They were the drift check's stand-in
//! DURING the migration, when the pre-migration code still existed to diverge
//! from, and that job is finished. A future migration of these folds should
//! mint its own — which is exactly what this campaign did.
//!
//! **What is lost, said plainly.** A constant-free witness cannot tell you that
//! today's walk is the same walk as last month's; only that today's walk is
//! reproducible and non-empty. The campaign-time constants and their positive
//! controls are recorded below as a dated record for anyone who needs the
//! former.
//!
//! # THE CAMPAIGN-TIME RECORD (constants, controls, and the absorption)
//!
//! Everything from here to the end of this doc is history, recorded 2026-09-01
//! and 2026-09-02. Its numbers were correct when taken and nothing re-checks
//! them; read them as a dated record, never as a current claim.
//!
//! POSITIVE CONTROL (recorded, Task 1): `integrate_thirst`'s segment
//! accumulation in `windows/vessel/src/liveness.rs` —
//! `total += rate * (e - s);` mutated to `total += rate * (e - s) * 1.5;`
//! (scaling every thirst-integral segment by 1.5x, so the drive crosses its
//! action thresholds at different simulated instants and the creature's
//! route — hence its committed `agent-at` trail — diverges). Green hash was
//! the retired `EXPECTED` (`0x214d_b29c_f466_8067`, itself recorded from two
//! agreeing runs — the determinism check this file asserts on today);
//! under the mutation the same script produced `0x84833ba833d1e9b0` — a
//! different value, confirming the test reddens on a real behavioural
//! change and not merely on noise. Mutated with `scripts/mutate.py`,
//! restored with `git checkout -- windows/vessel/src/liveness.rs`, and the
//! green hash reconfirmed on a rebuilt binary before that constant was
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
//! **What that proved and did not prove.** It proves that The Pawl's fold
//! store, threaded through the resolution of a 261-commit absorption,
//! reproduced main's own walk byte for byte on both scripts — which is the
//! only thing the resolution could have broken. It did not prove the constants
//! would survive main's next campaign; nothing can, which is the argument
//! under "# WHY THE CONSTANTS RETIRED" above.
//!
//! One of those three numbers can still be seen today, and it is the strongest
//! statement this record makes: the seed-42 witness PRINTS `0xabc4731e5cf1ab21`
//! on this tree, so main's own hash at `2c34f9e4c` and the closing tree's agree
//! across the campaign's whole absorption. It is printed, not asserted.
//!
//! # THE SECOND POSITIVE CONTROL, TAKEN ON SEED 28 (campaign time)
//!
//! The campaign's first attempt at a control for the emitter witness was spec
//! §3 rule 5's wrong-reset mutation (`Sustenance::last_reset` returning the
//! reset BEFORE the correct one). **It left the test green**, because no
//! creature on seed 28 drinks twice inside the fixed script, so the mutation
//! was a no-op for every entity the hazard path reads. A control that cannot
//! fire proves nothing about the instrument it is meant to validate, so a
//! second one was taken ON the hazard path itself —
//! `windows/vessel/src/liveness.rs`, in `hazard_memory_memo`'s transient loop,
//! the PAST-DAY replay rather than `alarm_field_memo`'s present-day emitter
//! probe:
//!
//! ```text
//!   alarm += emitter_arousal(afraid, ledger, folds, m, day, terrain);
//! → alarm += emitter_arousal(afraid, ledger, folds, m, day, terrain) * 1.5;
//! ```
//!
//! Applied with `scripts/mutate.py`. Under it the eight-wait script on seed 28
//! produced `ledger 0xeb203415776db502, hazard 0xde28e2f4f828e62d over 7 bodies
//! (61 shunned rooms, 1 dreaded)` with `dread = 0.675
//! (0x3fe599999999999a)`, against the green `ledger 0x64aeb2f93e38d328, hazard
//! 0x8cf62f13ee7098f5 over 7 bodies (59 shunned, 1 dreaded)` with `dread = 0.45
//! (0x3fdccccccccccccd)`. **Both hashes moved and neither floor tripped**, and
//! the dread magnitude moved by exactly the mutated factor — which is what
//! showed the moved HAZARD hash to be the past-day replay's own value rather
//! than a side effect of a changed route. Restored with `scripts/mutate.py`,
//! REBUILT, and both constants reconfirmed green before it was written down.
//!
//! **THAT CONTROL IS STALE AND IS KEPT ANYWAY, WHICH IS A CLAIM ABOUT WHAT IT
//! WAS FOR.** It was demonstrated on SEED 28, at campaign time, against
//! constants that no longer exist; the search lands on seed 6 today (see
//! [`EMITTER_SEED`]) and the control has never been re-run there. What it
//! established is a property of the INSTRUMENT — that the digest moves under a
//! mutation to the past-day replay, and that dread carries the replay's own
//! value — not a property of any particular world, and a control deleted for
//! being old leaves nothing at all in its place. But it is no longer evidence
//! about what this file asserts: a constant-free witness guarantees
//! DETERMINISM (two fresh sessions agree) plus its FLOORS (the replay is
//! entered, the digest is non-empty, at least one dread entry exists), and
//! nothing more. It cannot detect a behaviour change, by mutation or
//! otherwise, and no control can make it able to.

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

/// One fresh session on `world`, the fixed script, and the hash of what it
/// committed.
///
/// A FRESH session each time is the whole point of calling this twice: two
/// hashes taken off one session would agree because they are the same string.
/// This starts possession again from the world, so the two runs share nothing
/// but the seed and the script.
fn ledger_hash_of_a_fresh_walk(world: &hornvale_kernel::World) -> u64 {
    let (mut session, _opening) =
        Session::start(world, &PossessOpts::default()).expect("seed 42 always starts a session");
    run_fixed_script(&mut session);
    fnv1a(session.session_ledger_json().as_bytes())
}

/// The byte-identity witness itself: the seed-42 `possess` script's final
/// ledger, hashed — twice, from two fresh sessions, and the two must agree.
///
/// **There is no committed constant here any more, and its absence is the
/// campaign's ruling rather than an omission.** See the module doc's
/// "# WHY THE CONSTANTS RETIRED" for the argument; the values they held are
/// recorded there as a dated campaign-time record.
///
/// What survives is what a constant-free witness can still guarantee: the walk
/// is DETERMINISTIC (two independent sessions on one seed commit the same
/// bytes), and it is NON-VACUOUS ([`run_fixed_script`] refuses to return unless
/// at least two bodies committed new facts over the sixty ticks, so the hash is
/// never the hash of a ledger nothing happened in). The hash is printed on
/// every run, so a campaign that wants to compare two trees has the number
/// without this file asserting on it.
///
/// **Runtime.** 12.953 s for the single-run form on the merged tree; 22.500 s
/// and 22.333 s for the two-run form in two full parallel runs of this crate,
/// against the campaign's own 60 s ceiling for a witness. That measurement
/// replaces a mangled sentence this doc used
/// to carry, which folded a `/// ` into the middle of a backticked identifier
/// (`controller_swap::the_driven_bodys_suppressed_drive_is_retrievable_but_absent_from_what_it_says`,
/// 27.9 s, the slowest vessel test this campaign did not write).
#[test]
fn the_seed_42_walk_commits_the_same_ledger_bytes() {
    let world = common::build(42).expect("seed 42 always builds a world");

    let first = ledger_hash_of_a_fresh_walk(&world);
    let second = ledger_hash_of_a_fresh_walk(&world);
    println!("--- the seed-42 byte-identity witness ---");
    println!("ledger hash {first:#018x} (second fresh session: {second:#018x})");

    assert_eq!(
        first, second,
        "two fresh seed-42 sessions running the same fixed script committed DIFFERENT \
         ledger bytes ({first:#018x} against {second:#018x}) — the walk is not \
         deterministic, which is a constitutional failure and not a moved golden"
    );
}

// ---------------------------------------------------------------------------
// The SECOND byte-identity witness (The Pawl, Task 5): an emitter-bearing
// world, and the hazard memories the seed-42 hash cannot see.
// ---------------------------------------------------------------------------

/// How many `wait`s the emitter script takes. Kept small on purpose: the
/// search below pays a world build per candidate seed, and the property it
/// searches for appears within a handful of ticks or not at all.
///
/// **It was 8 until the campaign's close and it is 2 now**, for the 60 s
/// witness ceiling — this test cost 137.033 s in a full parallel run of this
/// crate and costs 46.269 s and 47.889 s in two such runs with the cut and the
/// second determinism run together (35.9 s alone). Two is not a guess: sweeping
/// `common::SIGHT_SEEDS` (0..64) at each wait count, **one wait finds no world
/// in the range at all**, and two, three, four, six and eight all land on seed
/// 6 — so two is the cheapest script that still selects the world the
/// eight-wait search did, which is why [`EMITTER_SEED`] did not move with it.
/// `resident_folds.rs`'s `EMITTER_SEARCH_WAITS` is the same number for the same
/// search, and each file fails loudly on its own if the landing seed moves —
/// each pins its own `EMITTER_SEED` and asserts on it. **That was written here
/// one commit before it was true**: only this file had the guard, and the
/// sibling merely PRINTED the seed its whole comment block's numbers were
/// measured on. Task 7a's first fix round added the missing assertion rather
/// than deleting the claim.
/// type-audit: bare-ok(count)
const EMITTER_SCRIPT_WAITS: usize = 2;

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
/// **The script is two waits now, not eight** (see [`EMITTER_SCRIPT_WAITS`]),
/// and the seed did not move with it — which is the one thing that had to be
/// checked before cutting it. On the two-wait script seed 6 reports **342**
/// past-day replays over 127 bodies, from 453 emitter scans of which 3 find an
/// emitter, with 186 shunned rooms and 6 dread entries. Read that against the
/// eight-wait line above as two measurements of different scripts, exactly as
/// the paragraph at the bottom of this doc insists.
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
/// ran ten waits; the fixed script this witness hashed then ran eight, and
/// reported **10** replays on seed 28 rather than that sweep's 14. It runs
/// [`EMITTER_SCRIPT_WAITS`] (2) today. All three are correct measurements of
/// different scripts.
const EMITTER_SEED: u64 = 6;

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

/// What one fresh run of the emitter script produced.
///
/// Every field is captured PER RUN. Two runs sharing a session would share
/// their counters as well, and the determinism claim below would then be a
/// claim about one number compared with itself.
struct EmitterRun {
    /// FNV-1a over the session's own committed-ledger JSON.
    ledger_hash: u64,
    /// FNV-1a over [`hazard_digest`] of every body's hazard memory.
    hazard_hash: u64,
    /// How many bodies the digest covered.
    bodies: usize,
    /// Shunned rooms summed over every body.
    shunned: usize,
    /// Dread entries summed over every body.
    dread: usize,
    /// Past-day affect replays the hazard fold performed.
    replays: u64,
    /// Emitter scans the walk built.
    scans: u64,
    /// How many of those scans found an emitter.
    with_emitters: u64,
    /// Every dread magnitude, as `entity/room = value (bits)`.
    dread_lines: Vec<String>,
}

/// One fresh session on `world`, the fixed emitter script, and everything the
/// witness reads off it.
fn run_emitter_witness(world: &hornvale_kernel::World) -> EmitterRun {
    let (mut session, _opening) =
        Session::start(world, &PossessOpts::default()).expect("the found world starts a session");
    run_emitter_script(&mut session);

    let ledger_hash = fnv1a(session.session_ledger_json().as_bytes());
    let memories = session.hazard_memories();
    let hazard_hash = fnv1a(hazard_digest(&memories).as_bytes());
    let mut dread_lines = Vec::new();
    for (entity, mem) in &memories {
        for (room, magnitude) in &mem.dread {
            dread_lines.push(format!(
                "  dread: entity {} at {room:?} = {magnitude:?} ({:#018x})",
                entity.0.get(),
                magnitude.to_bits()
            ));
        }
    }
    EmitterRun {
        ledger_hash,
        hazard_hash,
        bodies: memories.len(),
        shunned: memories.iter().map(|(_, m)| m.shunned.len()).sum(),
        dread: memories.iter().map(|(_, m)| m.dread.len()).sum(),
        replays: session.resident_alarm_replays(),
        scans: session.resident_emitter_scans(),
        with_emitters: session.resident_emitter_scans_with_emitters(),
        dread_lines,
    }
}

/// The second witness, run twice on two fresh sessions of the searched world.
///
/// **There are no committed hash constants here any more** — see the module
/// doc's "# WHY THE CONSTANTS RETIRED". What is left is the three things a
/// constant-free witness can still hold:
///
/// 1. DETERMINISM, on both halves: two independent sessions on one seed commit
///    the same ledger bytes AND derive the same hazard digest.
/// 2. NON-VACUITY, on every floor the constants ever rode on: the past-day
///    replay is entered (`replays > 0`), the digest is not a list of empty sets
///    (`shunned > 0`), and it carries at least one DREAD entry (`dread > 0`) —
///    dread being the only part of a hazard memory the replay itself can put
///    there. Each floor is checked on BOTH runs, so a witness that quietly
///    stopped witnessing on the second one cannot hide behind an agreeing hash.
/// 3. The seed the search lands on, which is [`EMITTER_SEED`]'s own job.
///
/// Both hashes and every dread magnitude are printed, so the numbers a future
/// migration would want are on the record without this test asserting on them.
#[test]
fn the_emitter_bearing_walk_commits_the_same_ledger_and_hazard_bytes() {
    let (seed, world) = emitter_bearing_world();
    println!("--- the emitter-bearing byte-identity witness: seed {seed} ---");
    assert_eq!(
        seed, EMITTER_SEED,
        "the search moved off the seed this witness was recorded on. That is a finding \
         about the sim, not a broken test: say in the campaign record what changed about \
         which worlds replay an emitter's affect at a past day"
    );

    let first = run_emitter_witness(&world);
    let second = run_emitter_witness(&world);

    for (label, run) in [("run 1", &first), ("run 2", &second)] {
        println!(
            "{label}: {} emitter scans, {} with an emitter, {} PAST-DAY affect replays \
             inside the hazard fold",
            run.scans, run.with_emitters, run.replays
        );
        println!(
            "{label}: ledger {:#018x}, hazard {:#018x} over {} bodies ({} shunned rooms, \
             {} dreaded)",
            run.ledger_hash, run.hazard_hash, run.bodies, run.shunned, run.dread
        );
        for line in &run.dread_lines {
            println!("{label}:{line}");
        }

        // THE DENOMINATORS, checked on each run rather than once. Without them
        // two agreeing hashes could be two hashes of the same empty digest.
        assert!(
            run.replays > 0,
            "{label} must reach the hazard fold's past-day affect replay on this world, or \
             both hashes are witnessing the same terrain-only path seed 42 already covers"
        );
        assert!(
            run.shunned > 0,
            "{label}'s hazard digest must have something in it, or its hash is the hash of \
             a list of empty sets and would not move for any change to this path"
        );
        assert!(
            run.dread > 0,
            "{label}'s digest must carry at least one DREAD entry, or the past-day affect \
             replay reached nothing the hash could witness: dread is the only part of a \
             hazard memory that a remembered alarm — and so the replay's own reset \
             semantics — can put there"
        );
    }

    // BOTH verdicts are computed before either can panic, and that is not
    // stylistic. An `assert_eq!` on the ledger followed by one on the hazard
    // digest reports only the first: this witness's own positive control (see
    // the module doc) moves BOTH, so the hazard verdict — the half the seed-42
    // witness cannot give at all — would never have been printed under the very
    // mutation that was meant to validate it.
    let mut moved: Vec<String> = Vec::new();
    if first.ledger_hash != second.ledger_hash {
        moved.push(format!(
            "LEDGER: run 1 {:#018x}, run 2 {:#018x}",
            first.ledger_hash, second.ledger_hash
        ));
    }
    if first.hazard_hash != second.hazard_hash {
        moved.push(format!(
            "HAZARD: run 1 {:#018x}, run 2 {:#018x} — two fresh sessions on one seed \
             derived different remembered-frightening ground or different dread magnitudes",
            first.hazard_hash, second.hazard_hash
        ));
    }
    assert!(
        moved.is_empty(),
        "two fresh sessions on seed {seed} running the same fixed script disagreed, so the \
         walk is not deterministic:\n  {}",
        moved.join("\n  ")
    );
}
