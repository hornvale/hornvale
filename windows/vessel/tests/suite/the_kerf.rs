//! The Kerf's retired campaign-time identity witness. It once pinned two
//! committed ledger-hash constants, minted from the merge base with their
//! positive control run FIRST, against the `KnownWater` tenant this campaign
//! deleted.
//!
//! # WHAT THESE ARE FOR, AND WHEN THEY DIE
//!
//! The campaign moves `water_at` off [`hornvale_vessel::resident::KnownWater`]
//! (per entity: each visited room → its FIRST visit instant) onto
//! `LatestVisit` (per entity: each visited room → its ascending list of visit
//! instants), and deletes `KnownWater`. The change is byte-identical BY
//! CONSTRUCTION — the spec's §5 gives the four-step argument — and no
//! committed artifact carries a ticked session ledger, so the generated-
//! artifact drift check is structurally blind to it. These constants are that
//! drift check's stand-in for the duration of the migration, which is exactly
//! the instrument decision 0541 sanctions.
//!
//! **They retired at this campaign's close, per decision 0541.** The values,
//! controls, and seed sweep below are dated history that nothing re-checks.
//! What survives is the property witness: two FRESH runs of each Kerf script
//! agree, and every floor is asserted on each run. That guarantees determinism
//! plus reach; it cannot detect a later behaviour change, by mutation or any
//! other means. The fold-equals-scan witnesses in `resident_folds.rs` retain
//! the independent semantic comparison.
//!
//! # CONTROL A (REACH): `KnownWater::absorb` MADE A NO-OP
//!
//! ```text
//! python3 scripts/mutate.py windows/vessel/src/resident.rs \
//!   '<KnownWater::absorb body>' '<same, prefixed by an immediate return>'
//! cargo nextest run -p hornvale-vessel --test suite -E 'test(/the_kerf_/)' --no-capture
//! ```
//!
//! With the fold emptied at every instant, every `believed_water` read is
//! ignorant, so any script whose committed trail depends on the belief at all
//! MUST move. Measured on this Mac, 2026-09-04, at merge base `f20fdbecb`:
//!
//! ```text
//! script                        green (minted)        control A             moved?
//! seed 17, 12 waits             0x739488239689ce2a    0xb214b6413e99986e    YES
//! seed 11, 12 waits             0xd4e4a793ed706478    0xa05a0e2dc0bc7748    YES (historical)
//! seed 42 fixed script          0xc566d07e4d76ffbd    0xc566d07e4d76ffbd    no
//! seed 6 emitter, 2 waits       0x5d3d768236e116c9    0x5d3d768236e116c9    no
//!   (its hazard digest)         0x92ddc47a37de3d9e    0x92ddc47a37de3d9e    no
//! ```
//!
//! **THE TWO SCRIPTS THIS CAMPAIGN INHERITED ARE BLIND, AND THAT IS WHY THIS
//! FILE PINS TWO NEW SEEDS INSTEAD.** The seed-42 fixed script and the seed-6
//! two-wait emitter script — the pair `ledger_hash_witness.rs` and
//! `the_detent.rs` both build on — produced byte-identical ledgers with the
//! whole tenant neutralised. That is the same shape of finding The Detent
//! recorded for its own seed-42 half (a green seed-42 witness was never
//! evidence about the fear path), reached here for a different path: seed 42's
//! flagship condenses onto fresh water and its residents commit no positional
//! fact at all, so `KnownWater` is empty on that walk whether or not `absorb`
//! runs. The emitter script is two waits long, which is not enough walking for
//! any resident's water belief to reach a route. Minting a constant on either
//! would have produced a number that could not fail for the reason this
//! campaign needs it to.
//!
//! # HOW THE TWO SEEDS WERE CHOSEN (the sweep, not a guess)
//!
//! Seeds 0..24 were walked for 12 `wait`s each, clean and under control A, and
//! the hash compared. Eight moved — 3, 6, 11, 12, 16, 17, 21, 23 — so the
//! campaign has an identity proof available and STOP was not reached. Of the
//! eight, five also record belief reads at an instant strictly before a
//! committed sighting (`resident_beliefs_in_the_past() > 0`): 6, 16, 17, 21,
//! 23. Cost decided between them, against the campaign's standing 60 s ceiling
//! for a witness, measured in a four-test parallel `nextest` run of this crate:
//!
//! ```text
//! seed  bodies  agent_at  beliefs  past    wall     verdict
//!   17      67       928    28601   1194   22.1 s   MINTED
//!   11      27       290     7973      0    7.3 s   MINTED (cheap, present-instant only)
//!   21     101       391   104376    103   49.7 s   passed over: 2.2x seed 17 for no more reach
//!   16      83      2452    52697  11716  248.2 s   REFUSED: 4.1x over the 60 s ceiling
//! ```
//!
//! Seed 6 at 12 waits also moves, but it carries 127 bodies and the emitter
//! search already owns that seed for a different property; seed 17 is the same
//! evidence for a fifth of the walk. Both minted witnesses together cost
//! ~29.4 s, which is under the ceiling for either one alone.
//!
//! **THE WALL TIMES ABOVE ARE QUIET-BOX NUMBERS, AND THE CEILING VERDICT IS
//! TAKEN ON THEM DELIBERATELY.** Later readings on the same Mac, same binary,
//! same tests, ranged 38.4–45.3 s (seed 11) and 92.8–125.4 s (seed 17) — a
//! 5.7x spread with nothing about the tests changed. The confound is the box:
//! `uptime` reported load averages of `78.99 49.74 29.59` at the start of that
//! sequence and `262.01 157.25 83.42` at the end, on ten cores, from other
//! sessions' work. Recording the slow figure as this witness's cost would
//! freeze another campaign's contention into this file, and shortening the
//! script to chase it would trade real reach for a number that was never the
//! test's. So both are recorded, with the loads that separate them, and the
//! 60 s comparison is made against the quiet readings. The durable per-host
//! figure is `docs/timings/test-baseline-<host>.tsv`'s, which is host-keyed
//! for exactly this reason; a first stage-gate run on lefford will produce it.
//!
//! # CONTROL B (SHARPNESS): THE MIN-KEEPING BRANCH FLIPPED — AND IT CANNOT FIRE
//!
//! `if day < *first` → `if day > *first` in `KnownWater::absorb` (keep the
//! LATEST visit rather than the first), which changes `water_at`'s admission
//! only at a past instant. **It moved nothing — not one of the four scripts,
//! including seed 17 with its 1,194 past-instant belief reads.** Per the plan
//! that is a result to record, not a failure; but a null with no mechanism is
//! just an absence, so the mechanism was measured rather than guessed. A third
//! mutation replaced the branch body with a `panic!`:
//!
//! ```text
//! if day < *first {
//!     panic!("KERF-DIAG: the min-keeping branch FIRED (descending arrival)");
//! }
//! ```
//!
//! **No script panicked.** The branch is never taken on a WALK-DERIVED
//! ledger, because `DriveMovements` commits at the tick's own day and ticks
//! advance monotonically: the first sighting absorbed for a room already IS
//! its minimum, so the `day < *first` condition is never true there.
//!
//! # WHAT THAT DOES NOT LICENSE — A CLAIM MADE HERE AND FALSIFIED AT TASK 3
//!
//! This paragraph used to continue: "so `and_modify`'s comparison is a
//! structural no-op there and flipping its sense is unobservable. Control B
//! is therefore not a weak control on these shapes, it is an impossible one,
//! and no longer or luckier *script* could rescue it." **Every clause after
//! the semicolon is false, and the error is worth more than the sentence
//! was.**
//!
//! A panic proves the branch it sits in never FIRES. It says nothing about
//! what happens when the branch's condition is REWRITTEN — because the
//! rewritten condition is a different condition. `day > *first` fires on
//! every revisit at a later day, which on a monotonic walk is the common
//! case, not the impossible one: control B turns `KnownWater` into a
//! LATEST-visit map, and a room is then admitted only from its last visit
//! onward instead of its first. That is a real behaviour change on a real
//! walk, and Task 3's FOLD-equals-SCAN witnesses redden on **all four**
//! real-shape sweeps under it, across 324,535 comparisons.
//!
//! What Task 1 actually measured is narrower and still true: control B moves
//! **neither ledger hash**, on any of four scripts, including seed 17 with
//! its 1,194 past-instant belief reads. The admitted set differs; it reaches
//! a committed fact only through past-instant read → different admitted set
//! → different chosen room → different committed route, and that chain never
//! completes. **So these constants are the WEAKER instrument of the two this
//! campaign holds** (ledger ruling 8a): a campaign that minted only hash
//! constants would have shipped control B's behaviour change unseen. Read
//! them as what they are.
//!
//! The mutation that IS invisible everywhere except the hand-built fixture is
//! a third one, control C — empty the min-keeping arm entirely, so
//! first-arrival wins. It reddens the descending-order fixture alone. The two
//! separate cleanly: **the real shapes hold the comparison's SENSE, the
//! fixture holds the branch's FIRING**, and neither substitutes for the
//! other.
//!
//! **The branch is NOT dead code, and the paragraph above would read as
//! saying so if it stopped there.** `agent-at` has a second writer:
//! `liveness::place_agent(entity, room, day)` is `pub`, takes an ARBITRARY
//! `WorldTime`, and constrains nothing — and `windows/lab/src/synthetic.rs`
//! authors whole scenario ledgers with it (`stranded_from_known_water`,
//! `stranded_in_a_hot_waste`, and their siblings each commit two placements
//! at days they choose by hand). Those happen to ascend; nothing makes them,
//! and `windows/lab`'s health calibration reads belief over exactly those
//! ledgers. So the min-vs-first equivalence at the heart of the spec's §5
//! step 2 governs a case a sibling window can CONSTRUCT today, not a
//! hypothetical one. That is the honest reason the descending-order fixture
//! is load-bearing: not "no walk can reach this branch, so we synthesise
//! one", but "the writer that can reach it is public, in use one window
//! over, and unchecked".
//!
//! **What that costs the campaign, said plainly.** These constants prove that
//! the migrated `water_at` admits the same ROOMS from the same key set; they
//! cannot prove anything about the min-vs-`days.first()` equivalence at the
//! heart of §5's step 2, because no real walk distinguishes those. That
//! equivalence is exactly what the spec's own descending-order fixture (§4,
//! Task 3) is for — a hand-built ledger that commits one room's sightings in
//! DESCENDING day order — and this measurement is the reason that fixture is
//! load-bearing rather than belt-and-braces. It is the only instrument that
//! can reach the branch at all.
//!
//! # RESTORATION DISCIPLINE
//!
//! Every mutation was applied with `scripts/mutate.py` (which refuses to
//! no-op) and restored with `git checkout -- windows/vessel/src/resident.rs`,
//! never by retyping. The green readings above were re-taken on a REBUILT
//! binary after the last restore and reproduced exactly — a restored source
//! with a stale binary is a known trap in this repo.

use crate::common;
use hornvale_vessel::{PossessOpts, Session};

/// The seed whose residents walk far enough for a water belief to reach a
/// route AND whose belief reads run at past instants — the sharper of the two
/// witnesses. Pinned rather than searched: a search over this property costs a
/// world build and a twelve-wait walk per candidate seed, which is minutes,
/// and the sweep that selected it is recorded in the module doc instead. Each
/// test takes two fresh walks and requires their ledger hashes to agree, while
/// asserting the reach floors on both; if an epoch leaves this seed unable to
/// exercise the path, those floors fail loudly.
/// type-audit: bare-ok(index)
const WATER_BELIEF_SEED: u64 = 17;

/// The cheap second world, for the reason one world is an anecdote. It moves
/// under control A on a quarter of seed 17's wall time; it records no
/// past-instant belief read, so it witnesses the present-instant admission
/// only. See [`WATER_BELIEF_SEED`] on why both are pinned rather than searched.
/// type-audit: bare-ok(index)
const CHEAP_WATER_BELIEF_SEED: u64 = 0;

/// How many `wait`s each witness's script takes. Twelve is
/// `resident_folds.rs`'s `WITNESS_WAITS` for the same reason — it is the
/// shortest script the sweep found that lets a resident's water belief reach a
/// committed route — and each file keeps its own copy, as every seed constant
/// in this crate does.
/// type-audit: bare-ok(count)
const WITNESS_WAITS: usize = 12;

/// What one fresh run of a witness script produced.
struct WalkRun {
    /// FNV-1a over the session's own committed-ledger JSON.
    ledger_hash: u64,
    /// How many bodies the session derived.
    bodies: usize,
    /// Committed `agent-at` facts — what the fold under test absorbs.
    agent_at: usize,
    /// `believed_water` calls the walk made.
    beliefs: u64,
    /// How many of those ran at an instant strictly before a committed
    /// sighting of the same entity.
    beliefs_in_the_past: u64,
}

/// One fresh session on `seed`, [`WITNESS_WAITS`] waits, and everything the
/// witness reads off it.
///
/// This helper performs one run; each witness test calls it twice, asserts the
/// non-vacuity floors on both results, and compares their ledger hashes for
/// fresh-run determinism.
fn walk(seed: u64) -> WalkRun {
    let world = common::build(seed).expect("the pinned witness seed builds a world");
    let (mut session, _) =
        Session::start(&world, &PossessOpts::default()).expect("the pinned seed starts a session");
    for _ in 0..WITNESS_WAITS {
        session.handle("wait");
    }
    WalkRun {
        ledger_hash: crate::ledger_hash_witness::fnv1a(session.session_ledger_json().as_bytes()),
        bodies: session.bodies().len(),
        agent_at: session.committed_agent_at_count(),
        beliefs: session.resident_belief_lookups(),
        beliefs_in_the_past: session.resident_beliefs_in_the_past(),
    }
}

/// The floors every witness here asserts rather than prints, so an agreeing
/// two-run agreement cannot hide a walk that never reached the fold.
///
/// A fresh-run agreement only witnesses the retired migration's reach if the
/// walk actually committed sightings for the fold to absorb AND asked it
/// questions.
/// Neither is implied by the other: seed 42 makes tens of thousands of belief
/// reads over an EMPTY trail, which is precisely the shape that made it blind
/// to control A.
fn assert_the_floors(label: &str, run: &WalkRun) {
    assert!(
        run.bodies >= 2,
        "{label}: the session must derive at least two bodies, saw {}",
        run.bodies
    );
    assert!(
        run.agent_at > 0,
        "{label}: the walk must commit `agent-at` sightings, or `KnownWater` is empty and \
         two-run agreement would be vacuous for any change to the fold — which is exactly why the \
         seed-42 script is not one of this file's witnesses (see the module doc)",
    );
    assert!(
        run.beliefs > 0,
        "{label}: the walk must actually READ the belief ({} lookups), or the fold's \
         output never reaches a route and the hash witnesses nothing about it",
        run.beliefs
    );
}

/// The sharper witness: [`WATER_BELIEF_SEED`]'s two fresh walks, whose belief
/// reads include past instants.
///
/// **Runtime.** 22.079 s and 23.189 s in two four-test parallel runs of this
/// crate on a quiet Mac, against the campaign's 60 s ceiling for a witness;
/// 92.816-125.359 s in three later runs of the same binary at load averages
/// from `78.99 49.74 29.59` to `262.01 157.25 83.42` on ten cores. See the
/// module doc on why the ceiling is judged against the first pair.
///
/// Its retired constant was `0x7394_8823_9689_ce2a`; control A moved it to
/// `0xb214_b641_3e99_986e`, while control B did not. The latter is a limit of
/// the retired instrument rather than a fact about the fold: Task 3's
/// FOLD-equals-SCAN witnesses catch control B on every real shape.
#[test]
fn the_kerf_seed_17_walk_is_deterministic_with_its_floors() {
    let first = walk(WATER_BELIEF_SEED);
    let second = walk(WATER_BELIEF_SEED);
    println!(
        "--- the-kerf seed-{WATER_BELIEF_SEED} walk ---\nledger {:#018x} over {} bodies, \
         {} agent-at facts, {} belief reads ({} at a past instant)",
        first.ledger_hash, first.bodies, first.agent_at, first.beliefs, first.beliefs_in_the_past
    );
    assert_the_floors("seed 17 first run", &first);
    assert_the_floors("seed 17 second run", &second);
    assert!(
        first.beliefs_in_the_past > 0 && second.beliefs_in_the_past > 0,
        "seed 17 is minted as the PAST-INSTANT witness: the first run made {} belief reads \
         before a committed sighting and the second made {} — if either is now zero the seed \
         no longer buys what it was chosen for, and that is a finding about the sim, not a \
         broken test",
        first.beliefs_in_the_past,
        second.beliefs_in_the_past
    );
    assert_eq!(
        first.ledger_hash, second.ledger_hash,
        "two fresh seed-{WATER_BELIEF_SEED} walks disagree: {:#018x} != {:#018x}",
        first.ledger_hash, second.ledger_hash
    );
}

/// The cheap second world: [`CHEAP_WATER_BELIEF_SEED`]'s two fresh walks,
/// present-instant belief reads only.
///
/// The current witness was re-searched after the Tidemark move: seed 0 now
/// commits 160 `agent-at` facts and makes 35,540 belief reads over 58 bodies,
/// with no past-instant reads. The older seed-11 timings above remain as
/// historical evidence for the retired witness.
///
/// It exists because one world is an anecdote: seed 17 and seed 0 derive
/// different rosters over different terrain, and control A moved both. It
/// asserts no past-instant floor, deliberately — it records zero such reads,
/// and asserting a floor a shape cannot meet is how a witness gets quietly
/// weakened to make it pass.
#[test]
fn the_kerf_seed_0_walk_is_deterministic_with_its_floors() {
    let first = walk(CHEAP_WATER_BELIEF_SEED);
    let second = walk(CHEAP_WATER_BELIEF_SEED);
    println!(
        "--- the-kerf seed-{CHEAP_WATER_BELIEF_SEED} walk ---\nledger {:#018x} over {} \
         bodies, {} agent-at facts, {} belief reads ({} at a past instant)",
        first.ledger_hash, first.bodies, first.agent_at, first.beliefs, first.beliefs_in_the_past
    );
    assert_the_floors("seed 0 first run", &first);
    assert_the_floors("seed 0 second run", &second);
    assert_eq!(
        first.ledger_hash, second.ledger_hash,
        "two fresh seed-{CHEAP_WATER_BELIEF_SEED} walks disagree: {:#018x} != {:#018x}",
        first.ledger_hash, second.ledger_hash
    );
}
