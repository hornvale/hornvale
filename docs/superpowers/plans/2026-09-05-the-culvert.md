# The Culvert Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Remove `believed_water`'s repeated route search — a shared,
session-lived memo of `(from, dest, budget) -> Option<hop count>` — without
moving a committed byte.

**Architecture:** `plan_to_room(from, dest, budget, ∅)` is pure over mesh
geometry: `NavSpace` holds only `dest` and `avoid`, and never reads `Terrain`,
the ledger or the tick. All three belief folds pass an *empty* avoid set, so a
memo keyed on `(from, dest, budget)` is byte-identical by construction. The
memo is shared across entities (every within-sweep duplicate is a
cross-entity duplicate) and stores `None` (59.5% of calls are budget-exhausted
failures, and they are 95.1% of all node expansions).

**Tech Stack:** Rust 2024, std only (`serde`/`serde_json`/`libm` are the
workspace's whole external allowlist). `BTreeMap`/`BTreeSet`/`Vec` only — no
`HashMap`/`HashSet`, enforced by `clippy.toml` `disallowed-types`. No
wall-clock in production code.

**Spec:** [`docs/superpowers/specs/2026-09-05-the-culvert-design.md`](../specs/2026-09-05-the-culvert-design.md)
**Ledger:** [`docs/superpowers/ledgers/2026-09-05-the-culvert.md`](../ledgers/2026-09-05-the-culvert.md)

## Global Constraints

- **Byte-identity, no epoch.** Every committed world, almanac, scene and
  fixture is identical before and after. Spec Rule 1 governs any artifact
  movement: `book/src/gallery/`, `cli/tests/fixtures/`,
  `clients/game/core/tests/fixtures/` or any world/almanac/scene output moved →
  **STOP and report**; only `docs/audits/*`, `docs/digest/` or a
  `docs/timings.md` row moved → regenerate and commit in the same commit;
  nothing moved → commit as is.
- **Every crate sets `#![warn(missing_docs)]`.** Every public item, field and
  variant gets a one-line doc comment.
- **`cargo fmt` is the final step before every commit.** Fmt-gate skips are the
  most common review finding in this repo.
- **`make gate-commit` before every commit** on a Rust-touching change. Pass an
  explicit Bash `timeout: 3600000`.
- **The lexicon guard counts the token `cell` per file** against
  `docs/audits/lexicon-inventory.tsv`. `std::cell::Cell` trips it. Use
  `AtomicU64`, or a `// lexicon: <reason>` waiver — a reasonless waiver is a
  failure, not a pass.
- **`plan_to_room` is `hornvale_vessel::action::plan_to_room`.** It is *not*
  re-exported from `liveness` (`liveness.rs:8` imports it privately).
- **`PLAN_BUDGET = 1_000`** is a private const in `liveness.rs`, mirrored by
  hand as `PROBE_BUDGET` / `BUDGET` in two examples with nothing enforcing
  agreement.
- **Absorb main and `make sluice-stage BRANCH=campaign/the-culvert
  REF=<full-sha>` at every stage boundary**, regenerating aggregates first.
- **All three load averages recorded before and after every timed run.** Any
  run whose 1-minute average exceeds 10 at either end is set aside and listed.
  Counts are exempt — they are deterministic.

---

## File Structure

| file | responsibility | tasks |
|---|---|---|
| `windows/vessel/src/liveness.rs` | the drive-predicate roster (new `pub` const); `believed_water`, `nearer_to_home`, `shared_believed_water` call sites; the memo type if it lives here | 1, 6, 7, 8 |
| `windows/vessel/src/session.rs` | `Session::start`'s `register_predicate` block becomes a consumer of the roster | 1 |
| `windows/vessel/examples/session_length_scaling.rs` | registration repaired; belief columns fixed | 1, 2 |
| `windows/vessel/examples/agent_scaling.rs` | registration repaired | 1 |
| `windows/vessel/tests/suite/the_culvert.rs` | **new** — the campaign's tests: roster agreement, memo equivalence (Rule 2), counting witnesses, hash constants | 1, 3, 4, 5, 6, 7 |
| `windows/vessel/tests/suite.rs` | `mod the_culvert;` | 1 |
| `kernel/src/astar.rs` | **campaign-time only** — the C1b expansion counter, applied for measurement and reverted before merge | 9 |

---

# Stage 1 — the instruments

Nothing in production behaviour moves in this stage. Two of the three
preregistration instruments panic on `main` and one probe is vacuous; the
campaign is unmeasurable until both are fixed.

## Task 1: One drive-predicate roster, three consumers

**Files:**
- Modify: `windows/vessel/src/liveness.rs` (add the roster const near `SLEPT_ON`, `:2801`)
- Modify: `windows/vessel/src/session.rs:1593-1680` (`Session::start`'s register block)
- Modify: `windows/vessel/examples/session_length_scaling.rs:1297` (the `for (pred, doc)` loop)
- Modify: `windows/vessel/examples/agent_scaling.rs:343-376` (the chained `register_predicate` calls)
- Create: `windows/vessel/tests/suite/the_culvert.rs`
- Modify: `windows/vessel/tests/suite.rs` (add `mod the_culvert;`)

**Interfaces:**
- Consumes: nothing.
- Produces: `pub const DRIVE_PREDICATES: &[(&str, &str)]` in
  `hornvale_vessel::liveness` — `(name, doc)` pairs, in registration order.

**The defect this repairs.** Both examples panic on `main`:

```
thread 'main' panicked at windows/vessel/examples/session_length_scaling.rs:1406:18:
a real drive-movements fact always commits: UnknownPredicate { predicate: "slept-on" }
```

`liveness::SLEPT_ON` (`:2801`) was added by The Pallet on 2026-09-03 and
neither example was updated. The list is currently written out **three times in
three different spellings** — `Session::start`'s chained `register_predicate`
block, `session_length_scaling`'s `for (pred, doc) in [...]` loop, and
`agent_scaling`'s own chain — and `session.rs:960` already records that this
roster "has already gone stale three predicates in a row."

**Design latitude, per spec §6 Task 1b.** The spec licenses either a shared
published list or a divergence test, and says the implementer chooses *after
reading the emission sites*. This plan writes the shared list because the
repository already publishes seed labels the same way (`stream_labels()`), and
because a test comparing three hand-written lists still leaves three lists. **If
reading the emission sites shows the shared list cannot serve all three
consumers — for example if `Session::start` must register in an order the
examples must not — implement the divergence test instead and say so in your
report.** Do not implement both.

- [ ] **Step 1: Write the failing test**

Create `windows/vessel/tests/suite/the_culvert.rs`:

```rust
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
        assert!(!name.is_empty(), "a roster entry has an empty predicate name");
        assert!(!doc.is_empty(), "roster entry `{name}` has an empty doc");
        assert!(seen.insert(*name), "roster entry `{name}` appears twice");
    }
    assert!(
        seen.len() >= 6,
        "the roster holds {} entries; the drive stack commits at least six",
        seen.len()
    );
}
```

Add to `windows/vessel/tests/suite.rs`, in the existing `mod` list:

```rust
mod the_culvert;
```

- [ ] **Step 2: Run it and verify it fails**

Run: `cargo test -p hornvale-vessel --test suite -- the_culvert`
Expected: FAIL to COMPILE, `cannot find value DRIVE_PREDICATES in module liveness`.

**A compile failure is not yet evidence.** Before proceeding, record the
behavioural red as well — the thing the guard exists to catch:

Run: `cargo run --release -p hornvale-vessel --example session_length_scaling 2>&1 | tail -3`
Expected: the `UnknownPredicate { predicate: "slept-on" }` panic, exit 101.
Paste both into your report.

- [ ] **Step 3: Publish the roster**

In `windows/vessel/src/liveness.rs`, immediately after `SLEPT_ON`'s definition
(`:2801`):

```rust
/// Every game-layer predicate the drive stack commits, with the doc string it
/// registers under, in registration order — the ONE list, published so that
/// `Session::start` and the benches consume it instead of each writing their
/// own copy.
///
/// **Why this exists.** The list used to be written out three times in three
/// spellings: `Session::start`'s chained `register_predicate` block, and a
/// hand-copied subset in each of two `examples/` benches. When The Pallet added
/// [`SLEPT_ON`] on 2026-09-03 it updated the session and neither bench, and both
/// benches panicked with `UnknownPredicate` for two days across two merged
/// campaigns — invisible to every gate, because `--all-targets` COMPILES an
/// example and nothing RUNS one. `session.rs`'s own field doc had already
/// recorded that this roster "has already gone stale three predicates in a
/// row"; this is that observation given a mechanism.
///
/// **Scope: the DRIVE predicates only.** `Session::start` also registers the
/// thing-layer predicates (`LOCATED_IN`, `OPENNESS`, `LOCKEDNESS`), which no
/// bench needs and which are not this roster's business. It registers those
/// beside this list, not from it.
///
/// Every predicate here is registered PER SESSION, never at genesis (spec §3).
pub const DRIVE_PREDICATES: &[(&str, &str)] = &[
    (AGENT_AT, "an agent's position on a day"),
    (DRANK, "an agent satisfied its sustenance goal"),
    (RESTED, "an agent rested on a day, for this many ticks"),
    (SLEPT, "an agent slept on a day, for this many ticks"),
    (SLEPT_ON, "the kind of anchor an agent slept on"),
    (EATEN, "an agent ate (eased its hunger) on a day"),
];
```

**Copy the doc strings verbatim from `Session::start`'s existing block**
(`session.rs:1593-1680`) rather than from this plan — the registry is
contradiction-checked, and a doc string that differs from the one a world was
built with is a difference this plan cannot see. If any differ from the
strings above, the session's are authoritative; fix the plan's text in your
report.

- [ ] **Step 4: Run the roster tests**

Run: `cargo test -p hornvale-vessel --test suite -- the_culvert`
Expected: PASS, both tests.

- [ ] **Step 5: Make `Session::start` consume the roster**

In `windows/vessel/src/session.rs`, replace the six chained
`register_predicate` calls for the drive predicates with a loop over
`DRIVE_PREDICATES`. Leave the thing-layer registrations (`LOCATED_IN`,
`OPENNESS`, `LOCKEDNESS`) exactly as they are — they are not on this roster.

Update the field doc at `session.rs:956-962` so it names
`liveness::DRIVE_PREDICATES` as where a reader should look, replacing the
sentence that sends them to `Session::start`'s own block.

- [ ] **Step 6: Repair both benches**

`session_length_scaling.rs:1297` — replace the inline 5-element array with
`liveness::DRIVE_PREDICATES`. `agent_scaling.rs:343-376` — replace the five
chained calls with the same loop.

- [ ] **Step 7: Run both benches to completion**

Run: `cargo run --release -p hornvale-vessel --example session_length_scaling > /tmp/culvert-sls-pre.txt 2>&1; echo "rc=$?"`
Run: `cargo run --release -p hornvale-vessel --example agent_scaling > /tmp/culvert-as-pre.txt 2>&1; echo "rc=$?"`

Expected: `rc=0` from both, and a full table from each.

**Spec Rule 5 governs the branch.** Both run to completion → Stage 1 continues.
Either still panics on a *different* missing predicate → enumerate the full set
of predicates the drive stack commits (grep the `commit(` sites in
`liveness.rs`), add them, and say in your report that the roster was larger
than six — that finding is the point of the task, not a detour from it.

Keep both output files. They are the pre-fix column that Task 2 reports beside
the fixed one, and the C1b "before" run of Task 9.

- [ ] **Step 8: Confirm byte-identity**

Run: `make rebaseline` then
`git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$' | cut -f1)`

Global Constraints' Rule 1 branch table governs the response. A registration
that runs per-session and commits no new fact should move nothing; if
`docs/reference/concept-registry-generated.md` or any manifest moves, **STOP** —
it would mean a predicate reached genesis, which spec §3 forbids.

- [ ] **Step 9: Gate and commit**

```bash
cargo fmt
make gate-commit
git add windows/vessel/src/liveness.rs windows/vessel/src/session.rs \
        windows/vessel/examples/session_length_scaling.rs \
        windows/vessel/examples/agent_scaling.rs \
        windows/vessel/tests/suite/the_culvert.rs windows/vessel/tests/suite.rs
git commit -m "fix(vessel): one drive-predicate roster, three consumers

Both benches panicked with UnknownPredicate slept-on, added by The Pallet on
2026-09-03 and copied into neither. The list was written three times in three
spellings; session.rs's own doc already recorded that it had gone stale three
predicates in a row. Now published once and consumed by all three, with a test
asserting committed is a subset of registered.

Claude-Session: https://claude.ai/code/session_01DBUCxzBX77poAG35KVLf7C"
```

---

## Task 2: The belief probe stops lying about its own subject

**Files:**
- Modify: `windows/vessel/examples/session_length_scaling.rs:460-530` (`probe_believed_water_us`, `probe_shared_believed_water_us`), `:1422-1429` (probe selection), `:688-847` (`Band`), the table headers at `:879-948`

**Interfaces:**
- Consumes: Task 1's repaired benches.
- Produces: four new `Band` fields —
  `belief_probe_set_len: usize`, `belief_roster_non_empty: usize`,
  `belief_roster_max_set: usize`, `belief_roster_total_pairs: usize` — and a
  belief-probe agent selected as the roster's max-known-water member.

**The defect.** The probe agent is the roster's **max-`agent-at`** member
(`:1422-1429`), chosen once at band 1 and fixed. The Kerf measured that agent
holding an **empty** water belief at 10 of 10 bands in 3 of 3 runs, while 11 of
50 members hold a real set, the largest holding **46** rooms. So the column The
Detent attributed 99.08% of the six timed folds to is, on that agent, timing
`water_at` and nothing else — and both probes announce this with a `println!`
(`:481-485`, `:512-516`) that fails nothing and has fired at every band since
The Detent.

- [ ] **Step 1: Write the failing assertion into the probe**

Replace the vacuity `println!` in `probe_believed_water_us` with a panic. The
existing block reads:

```rust
    if some_count == 0 {
        println!(
            "believed_water: probe agent has no known water across {FOLD_REPS} calls at this band"
        );
    }
```

Replace with:

```rust
    assert!(
        some_count > 0,
        "believed_water: the probe agent has NO known water across {FOLD_REPS} \
         calls at this band, so this column times `water_at` and nothing else. \
         The probe agent is selected as the roster's max-known-water member \
         (The Culvert, Task 2); a zero here means that selection is wrong, not \
         that the world has no water. The Kerf measured 11 of 50 members \
         holding a real set, the largest holding 46 rooms."
    );
```

Do the same in `probe_shared_believed_water_us`.

- [ ] **Step 2: Run it and verify it fails**

Run: `cargo run --release -p hornvale-vessel --example session_length_scaling 2>&1 | tail -5`
Expected: PANIC on the new assertion at band 1.

This is the required behavioural red: it proves the selection is genuinely
wrong today, rather than proving only that an assertion compiles. Paste it.

- [ ] **Step 3: Select the probe agent by known water, not by history**

The selection at `:1422-1429` picks `max_by_key` over `counts` (the
`agent-at` fact count). Change the belief probe — and *only* the belief probe —
to select the roster member with the largest
`latest_visit.water_at(entity, t, terrain).len()` at the band where selection
happens, ties broken by ascending `EntityId` so the choice is deterministic.

**Leave the existing probe agent alone for the other four folds.** They are
The Detent's and The Kerf's subject and their columns must stay comparable
across campaigns; introducing a second probe agent for the belief columns is
the change, and conflating the two would silently move four unrelated columns.
Name the second one `belief_probe` in the code so the distinction cannot be
misread.

- [ ] **Step 4: Add the four roster-wide columns**

The bench already solved exactly this shape for `DRANK`, by adding four
roster-wide columns in its own fix round 1 — follow that precedent. Per band,
over the whole roster at that band's instant:

```rust
/// **The belief columns' denominator.** How many roster members hold a
/// NON-EMPTY known-water set at this band's instant. The single-agent
/// `believed_water_us` column above is a point measurement, and this is the
/// population it is a point in: The Kerf found 11 of 50 at the final band,
/// so a reader who takes the probe column as a roster statement is wrong by
/// construction. Reported for the same reason the `DRANK` distribution is.
belief_roster_non_empty: usize,
/// The largest known-water set any roster member holds at this band — the
/// worst population the memo must serve, and the number the probe agent is
/// now selected to match.
belief_roster_max_set: usize,
/// The total `(home, water room)` pairs implied across the whole roster:
/// exactly how many `plan_to_room` calls one roster-wide `believed_water`
/// sweep makes. 83 at Shape A band 10 before this campaign.
belief_roster_total_pairs: usize,
/// The probe agent's own set size, so a reader can see the point against the
/// distribution without cross-referencing another table.
belief_probe_set_len: usize,
```

Print them in the belief section of the output, and print the pre-fix
`believed_water_us` reading from Task 1's `/tmp/culvert-sls-pre.txt` beside the
new one in your report — spec §6 Task 1c requires the change of instrument be
visible rather than silent.

- [ ] **Step 5: Run and verify**

Run: `cargo run --release -p hornvale-vessel --example session_length_scaling > /tmp/culvert-sls-fixed.txt 2>&1; echo "rc=$?"`
Expected: `rc=0`, no panic, and `belief_probe_set_len > 0` at every band.

**Spec Rule 4 governs the branch.** Still vacuous → the selection is wrong and
the task is not done; do not relax the assertion.

- [ ] **Step 6: Gate and commit**

```bash
cargo fmt
make gate-commit
git add windows/vessel/examples/session_length_scaling.rs
git commit -m "fix(vessel): the belief probe measures a creature that believes in water

The probe agent was the roster's max-agent-at member, which holds an EMPTY
water belief at 10 of 10 bands -- so the column carrying 99.08% of the six
timed folds was timing water_at and nothing else, and said so in a println
that failed nothing. Vacuity is now a panic, the belief probe selects the
max-known-water member, and four roster-wide columns give the point
measurement its distribution.

Claude-Session: https://claude.ai/code/session_01DBUCxzBX77poAG35KVLf7C"
```

---

## Task 3: Campaign-time hash constants and their positive control

**Files:**
- Modify: `windows/vessel/tests/suite/the_culvert.rs`

**Interfaces:**
- Consumes: nothing.
- Produces: `CULVERT_SEED_42_LEDGER`, and whatever second constant the control
  proves reaches this campaign's path.

**The discipline, and the reason it is not just a hash.** Constants are minted
from **two agreeing runs**, given a positive control that MUST move them before
they are believed, re-recorded main-first after every absorption, and retired at
close with the dated record kept in this module's own doc. **The Kerf's finding
governs the ordering: a hash is a WEAKER instrument than a direct set
comparison** — its control B moved none of four script hashes yet reddened all
four real-shape FOLD-equals-SCAN sweeps. So these constants are the *secondary*
witness; Task 6's equivalence test is the primary one.

- [ ] **Step 1: Mint from two agreeing runs**

Write a test that walks seed 42 through the possession shape
(`Session::start` + waits) and hashes the resulting ledger. Run it twice from
scratch; the two runs must agree before any constant is written down. Reuse the
existing helper if one fits — `windows/vessel/tests/suite/ledger_hash_witness.rs`
holds The Detent's (`EMITTER_SEED: u64 = 6`, `pub(crate)`).

- [ ] **Step 2: Find a control that MOVES the constant**

Use `scripts/mutate.py` (the repo's mutation tool). The mutation must be one
that (a) compiles and (b) plausibly changes the water belief — for example
perturbing the tie-break in `believed_water`'s `min_by`, or the `day <= t`
admission in `water_at`.

**Do not take a prescribed mutation from this plan.** Read
`believed_water`, `nearer_to_home` and `water_at`, and choose one whose effect
you can argue reaches a committed fact. A plan author does not know which
derivations share a stream; you will, after reading.

**Assert the mutation happened.** Before substituting, assert the target text
exists (`assert old in s, "TARGET NOT FOUND"`). A no-op mutation produces a
green that looks exactly like a robust implementation, and `cargo fmt` rewrapping
a line has silently defeated a single-line replacement in this repo before.

**Branch:** the control moves the constant → the constant witnesses this
campaign's path; record which mutation, and the before/after values. The
control moves **nothing** → the constant is blind to this campaign and keeping
it would be theatre. Find a control that does reach, or record explicitly (as
The Detent did for its seed-42 constant) that this constant witnesses only
walk byte-identity and is blind to the belief path — and then mint a second one
on a seed whose residents actually hold water beliefs. **Seed 17 with 12 waits
is the shape this campaign has already measured to be belief-rich** (52 of 67
residents non-empty, max 23 rooms); seed 42's lab shape has a median agent
making zero searches.

- [ ] **Step 3: Record the dated record in the module doc**

At the top of `the_culvert.rs`, a `# THE DATED RECORD` block: each constant,
its value, the SHA it was taken at, the date, and the control that moved it.

- [ ] **Step 4: Gate and commit**

```bash
cargo fmt
make gate-commit
git add windows/vessel/tests/suite/the_culvert.rs
git commit -m "test(vessel): campaign-time hash constants with a control that moves them

Minted from two agreeing runs. The control is recorded with them: a constant
whose control moves nothing witnesses nothing about this campaign's path, and
The Kerf's finding stands -- a hash is a weaker instrument than the direct set
comparison Task 6 builds.

Claude-Session: https://claude.ai/code/session_01DBUCxzBX77poAG35KVLf7C"
```

- [ ] **Step 5: STAGE BOUNDARY**

Absorb main, `make rebaseline`, then
`make sluice-stage BRANCH=campaign/the-culvert REF=<full-sha>`.
Push the branch **before** submitting: the queue tests the SHA you give it, and
a commit pushed after enqueue is stranded.

---

# Stage 2 — the witnesses

## Task 4: The counting witness, red on the pre-fix tree

**Files:**
- Modify: `windows/vessel/tests/suite/the_culvert.rs`

**Interfaces:**
- Consumes: `liveness::believed_water`, `resident::LatestVisit::water_at`.
- Produces, all in `the_culvert.rs` and used by Tasks 5, 6, 7 and 9:
  - `struct SweepCounts { calls: usize, distinct_pairs: usize, non_empty: usize, max_set: usize }`
  - `fn culvert_sweep_counts(ledger: &Ledger, folds: &OwnedFolds, npcs: &[Body], t: WorldTime, terrain: &dyn Terrain) -> SweepCounts`
  - `fn culvert_real_pairs(shape: Shape) -> Vec<(Facet, Facet)>` — the distinct
    `(home, water room)` pairs a roster-wide sweep implies, which **Task 6's
    equivalence test consumes**. `enum Shape { Possession, Lab }`: the
    possession shape (seed 17, 12 waits) had zero unreachable pairs and the lab
    shape (seed 42, band 10) had 55 of 83, so Task 6 needs both to exercise both
    arms. Return them in a deterministic order — ascending `(from, dest)` — so
    the test is reproducible.
  - `const PLAN_BUDGET_MIRROR: usize = 1_000;` — `liveness::PLAN_BUDGET` is a
    private const an integration test cannot import, so it is mirrored here the
    way `PROBE_BUDGET` and `BUDGET` already mirror it in two examples. Give it a
    doc comment saying so, and saying that the three mirrors and the original
    are kept in sync by hand with nothing enforcing it.

**What it counts, and why it must be red today.** One roster-wide
`believed_water` sweep makes exactly `Σ|water_at(entity, t)|` calls to
`plan_to_room`. This witness counts those calls and the distinct
`(home, dest)` pairs among them, so that after the memo lands the same test can
assert the search count collapsed. **It must fail on the pre-memo tree** — a
witness that is green before the fix cannot show the fix happened.

- [ ] **Step 1: Write it against the possession shape (the belief-rich one)**

Use `KERF_WATER_SEED = 17` and `KERF_WAITS = 12`
(`windows/vessel/tests/suite/resident_folds.rs:3594-3614`), the shape this
campaign measured at 52-of-67 non-empty and 83 distinct pairs. Assert, with the
denominator asserted beneath each ratio:

```rust
assert!(counts.non_empty > 0, "denominator: no resident holds a water belief on this shape");
assert!(counts.calls > 0, "denominator: the sweep made no plan_to_room calls");
assert!(
    counts.calls <= 100,
    "one roster-wide believed_water sweep makes {} plan_to_room calls over {} \
     distinct (home, dest) pairs. Before The Culvert this was 529 calls over 83 \
     pairs at wait 12; the memo is meant to collapse the calls onto the pairs.",
    counts.calls, counts.distinct_pairs
);
```

- [ ] **Step 2: Run it and verify it FAILS**

Run: `cargo test -p hornvale-vessel --test suite -- culvert_sweep`
Expected: FAIL, reporting roughly 529 calls over 83 pairs.

Paste the failure. **A test that passes here is measuring the wrong thing** —
check that the sweep really is roster-wide and really is at wait 12.

- [ ] **Step 3: Mark it `#[ignore]` with a reason that names the task**

It stays red until Task 7. `windows/lab/tests/preregistration_guard.rs`'s
discipline requires an `#[ignore]` reason that names a cost or cites a decision;
name Task 7 and this plan.

- [ ] **Step 4: Gate and commit**

```bash
cargo fmt
make gate-commit
git add windows/vessel/tests/suite/the_culvert.rs
git commit -m "test(vessel): the sweep-count witness, red on the pre-memo tree

529 plan_to_room calls over 83 distinct pairs at wait 12 of the possession
shape. Ignored with a reason until Task 7 makes it green.

Claude-Session: https://claude.ai/code/session_01DBUCxzBX77poAG35KVLf7C"
```

---

## Task 5: Measure the moving anchor, then let Rule 3 decide

**Files:**
- Modify: `windows/vessel/tests/suite/the_culvert.rs`

**Interfaces:**
- Consumes: Task 4's `culvert_real_pairs`, `Shape` and `PLAN_BUDGET_MIRROR`.
- Produces: a recorded number; **no production code**.

**The question.** `shared_believed_water` (`liveness.rs:1880`) anchors its
ranking at `here` — the *current position*, which moves — rather than at
`home`. Its key space is therefore `positions × water rooms`, not
`homes × water rooms`, and that is the one way this campaign's bound (83 pairs,
saturating) fails: an unbounded memo in a long session.

**This is not measured.** The probe measured the home-anchored fold only.

- [ ] **Step 1: Count the `(here, dest)` key population**

Over Shape B's 12 waits and Shape A's 200 ticks, count distinct `(here, dest)`
pairs cumulatively per band/wait, exactly as Task 4 counts `(home, dest)`.
Report the growth curve, not just the endpoint.

- [ ] **Step 2: Apply spec Rule 3**

Bounded and comparable to the home-anchored 83 → **include** the site in Task 8,
and say in your report what bounds it. Growing with tick count → **exclude** the
site, and Task 8 is not run. Either way the number goes in the report and the
ledger; the site is not included on the strength of an expectation.

- [ ] **Step 3: Commit the measurement**

```bash
cargo fmt
make gate-commit
git add windows/vessel/tests/suite/the_culvert.rs
git commit -m "test(vessel): the moving-anchor key population, measured not assumed

shared_believed_water anchors at the current position, so its key space is
positions x water rooms rather than homes x water rooms. Spec Rule 3 decides
inclusion from this number.

Claude-Session: https://claude.ai/code/session_01DBUCxzBX77poAG35KVLf7C"
```

- [ ] **Step 4: STAGE BOUNDARY** — absorb, rebaseline, `make sluice-stage`.

---

# Stage 3 — the memo

## Task 6: The memo type and its equivalence test

**Files:**
- Modify: `windows/vessel/src/liveness.rs`
- Modify: `windows/vessel/tests/suite/the_culvert.rs`

**Interfaces:**
- Consumes: `crate::action::plan_to_room`.
- Produces:
  ```rust
  pub struct RouteMemo { /* private */ }
  impl RouteMemo {
      pub fn new() -> Self;
      pub fn hops(&mut self, from: &Facet, dest: &Facet, budget: usize) -> Option<usize>;
      pub fn searches(&self) -> u64;
      pub fn len(&self) -> usize;
      pub fn is_empty(&self) -> bool;
  }
  ```
  Note `hops` takes **no `avoid` parameter** — that is the key-hardening rider,
  applied structurally: a caller holding a real hazard set cannot reach this
  memo, which is a compile error rather than a silently wrong answer.

- [ ] **Step 1: Write the equivalence test first (spec Rule 2)**

```rust
/// **THE PRIMARY WITNESS.** For every `(from, dest, budget)` the memo is
/// asked, its answer equals a fresh `plan_to_room(from, dest, budget, ∅)` —
/// **including when both are `None`**.
///
/// This is deliberately stronger than a ledger hash, on The Kerf's own
/// finding: its control B moved none of four script hashes yet reddened all
/// four real-shape FOLD-equals-SCAN sweeps, so a hash is the weaker
/// instrument. The campaign's byte-identity claim is that
/// `plan_to_room(from, dest, budget, ∅)` is pure over mesh geometry
/// (`NavSpace` holds only `dest` and `avoid` and never reads `Terrain`); this
/// test is what makes that claim falsifiable rather than merely argued.
///
/// The `None` case is not an afterthought: 59.5% of real calls are
/// budget-exhausted failures and they are 95.1% of all node expansions, so a
/// memo that stored only successes would re-pay the worst calls forever.
#[test]
fn the_memo_answers_exactly_what_a_fresh_search_answers() {
    // Build a real world's mesh and take a real (home, water room) population
    // from the possession shape — NOT synthetic facets. The property must hold
    // on the pairs production actually asks about, including the unreachable
    // ones.
    // Both shapes: the possession shape has no unreachable pair and the lab
    // shape had 55 of 83 at band 10, so neither alone exercises both arms.
    let mut pairs = culvert_real_pairs(Shape::Possession);
    pairs.extend(culvert_real_pairs(Shape::Lab));
    assert!(!pairs.is_empty(), "denominator: no pairs to compare");

    let mut memo = RouteMemo::new();
    let mut reached = 0usize;
    let mut unreachable = 0usize;
    for (from, dest) in &pairs {
        let fresh = plan_to_room(from, dest, PLAN_BUDGET_MIRROR, &BTreeSet::new())
            .map(|p| p.len());
        let memoed = memo.hops(from, dest, PLAN_BUDGET_MIRROR);
        assert_eq!(
            memoed, fresh,
            "memo disagreed with a fresh search for {from:?} -> {dest:?}"
        );
        // And again, to exercise the HIT path, not only the miss path.
        assert_eq!(memo.hops(from, dest, PLAN_BUDGET_MIRROR), fresh, "hit disagreed with miss");
        match fresh { Some(_) => reached += 1, None => unreachable += 1 }
    }
    // Both arms must be exercised or the test is half a test.
    assert!(reached > 0, "no reachable pair in the population — the Some arm is untested");
    assert!(unreachable > 0, "no unreachable pair — the None arm, which is 95.1% of the cost, is untested");
    assert_eq!(memo.searches() as usize, pairs.len(), "one search per distinct pair, no more");
    assert_eq!(memo.len(), pairs.len(), "entries == searches");
}
```

**Both arms must fire, which is why the test unions the two shapes.** The
possession shape had **zero** budget exhaustions and the lab shape had **55 of
83** unreachable at band 10, so the union is what makes the `None` arm — 95.1%
of the real cost — testable at all. Report both counts. If the union still
yields no unreachable pair, the population is wrong, not the assertion: check
that `culvert_real_pairs(Shape::Lab)` is reading band 10 and not band 1.

- [ ] **Step 2: Run it and verify it fails to compile**

Run: `cargo test -p hornvale-vessel --test suite -- the_memo_answers`
Expected: FAIL, `cannot find type RouteMemo`.

- [ ] **Step 3: Implement**

```rust
/// A memo of `plan_to_room(from, dest, budget, ∅)` — the hop count only, and
/// `None` when the goal is not reachable within `budget`.
///
/// **Byte-identical by construction, not by testing.** `NavSpace` holds
/// exactly `dest` and `avoid`, and `edges_from` computes `move_cost` over
/// `Facet::neighbors`: it never consults `Terrain`, the ledger, or the tick.
/// So with `avoid` fixed empty the memoized function is pure over mesh
/// geometry, and caching it is exactly caching that function. Nothing within a
/// process invalidates an entry — not a commit, not a terrain rebuild, not a
/// belief change.
///
/// **It takes no `avoid` parameter, and that is the point.** Every belief fold
/// passes a freshly-allocated empty set; only `HomeNavCache::home_nav` passes a
/// real hazard set. A future caller holding one cannot reach this memo, because
/// there is nowhere to pass it — a compile error instead of a stale answer.
/// `budget` IS in the key, for the reason `HomeNavState` puts it in its own:
/// today every caller passes `PLAN_BUDGET` and nothing enforces that.
///
/// **Shared across entities, deliberately — the opposite of
/// [`HomeNavCache`].** `water_at` returns each entity's DISTINCT rooms, so an
/// entity never duplicates its own pair within a sweep; every duplicate is a
/// duplicate ACROSS entities, and on the possession shape that is 6.4x-9.1x of
/// the calls. `HomeNavCache` is per-entity because `pos` and its avoid-epoch
/// are per-entity; neither is in this key, so that half of the precedent does
/// not transfer.
///
/// **It stores the hop count, not the plan**, because `p.len()` is all any
/// caller consumes — the refinement The Waymark reached for `HomeNavFeature`.
///
/// **This is NOT a resident index and decision 0756 does not govern it.** It is
/// derived from no fact, absorbs no fact, and has no parent to be
/// asymptotically cheaper than; it is the same category as `RoomMeshMemo` and
/// `PrimaryAfraidMemo`.
#[derive(Default)]
pub struct RouteMemo {
    /// `(from, dest, budget) → hops`, `None` = not reachable within budget.
    hops: std::collections::BTreeMap<(Facet, Facet, usize), Option<usize>>,
    /// How many real searches this memo has run, ever — the deterministic
    /// witness the campaign preregisters on (C1a), in the shape
    /// [`HomeNavCache::searches`] already has. Never a wall-clock proxy.
    searches: u64,
}
```

with `hops()` doing the lookup, running `plan_to_room` on a miss, incrementing
`searches`, and inserting.

- [ ] **Step 4: Run and verify it passes**

Run: `cargo test -p hornvale-vessel --test suite -- the_memo_answers`
Expected: PASS, with both arm counts non-zero.

- [ ] **Step 5: Gate and commit**

```bash
cargo fmt
make gate-commit
git add windows/vessel/src/liveness.rs windows/vessel/tests/suite/the_culvert.rs
git commit -m "feat(vessel): RouteMemo, a memo of the belief folds' pure route search

Keyed (from, dest, budget), shared across entities, storing the hop count and
the negative result. Byte-identical by construction: NavSpace never reads
Terrain, so with an empty avoid set the search is pure over mesh geometry. It
takes no avoid parameter, so a caller with a real hazard set cannot reach it.
Not wired to anything yet.

Claude-Session: https://claude.ai/code/session_01DBUCxzBX77poAG35KVLf7C"
```

---

## Task 7: Wire the two home-anchored sites

**Files:**
- Modify: `windows/vessel/src/liveness.rs:1199-1229` (`believed_water`), `:8496-8524` (`nearer_to_home`), and the call chain that owns the memo
- Modify: `windows/vessel/tests/suite/the_culvert.rs` (un-ignore Task 4's witness)

**Interfaces:**
- Consumes: `RouteMemo` from Task 6.
- Produces: `believed_water` and `nearer_to_home` taking a `&mut RouteMemo`.

**Where the memo lives.** Session-lived and caller-owned, in the same scope
`RoomMeshMemo` and `HomeNavCache` already occupy — a `Session` field and
`run_simulation`'s own local. **Read those two through their call chains before
choosing where to thread it**; this plan does not name the parameter list from
outside the code.

**Why `nearer_to_home` is in scope.** Its doc says its tie-break "MUST match
`believed_water`'s ... or a mid-walk incremental belief could disagree with the
same belief re-derived from the committed history." Sharing one memo makes that
agreement structural rather than maintained. It also calls `d()` on *both* the
current belief and the found room every time water is perceived — two searches
per perception, both memoable.

- [ ] **Step 1: Thread the memo into `believed_water`**

Replace the inline `plan_to_room(...)` at `:1224` with a `memo.hops(...)` call.
**Nothing else in the function moves** — the `day <= t` admission, the
`is_water` intersection, the `(hops, Facet)` tie-break and the ascending-`Facet`
candidate order are all unchanged.

Note the borrow: the store guard is already dropped before the ranking (`:1194`
explains why). The memo borrow must not reintroduce a conflict with it.

- [ ] **Step 2: Thread it into `nearer_to_home`**

- [ ] **Step 3: Un-ignore Task 4's witness and run it**

Run: `cargo test -p hornvale-vessel --test suite -- culvert_sweep`
Expected: PASS — calls collapsed onto distinct pairs.

- [ ] **Step 4: Run the full vessel suite and the equivalence test**

Run: `cargo test -p hornvale-vessel --test suite 2>&1 | tee /tmp/culvert-vessel.txt`
Run once, inspect many. Trust the exit code; grep the file.

- [ ] **Step 5: Byte-identity**

Run: `make rebaseline` then the drift check. **Global Constraints' Rule 1 branch
table governs.** Any world/almanac/scene/fixture movement → **STOP and report**:
that is the campaign's premise failing, not a rebaseline.

Also re-run Task 3's hash constants and confirm they are unmoved.

- [ ] **Step 6: Gate and commit**

```bash
cargo fmt
make gate-commit
git add windows/vessel/src/liveness.rs windows/vessel/tests/suite/the_culvert.rs
git commit -m "perf(vessel): the water belief plans each route once per session

believed_water and nearer_to_home read RouteMemo instead of re-running the
search per known water room per read. Byte-identical: the memoized function is
pure over mesh geometry and the equivalence test compares every asked pair
against a fresh search, both arms.

Claude-Session: https://claude.ai/code/session_01DBUCxzBX77poAG35KVLf7C"
```

---

## Task 8 (CONDITIONAL on Task 5): The moving-anchor site

**Run this task only if spec Rule 3 admitted the site in Task 5.** If Task 5
found the `(here, dest)` key population growing with tick count, skip this task,
add a comment at `liveness.rs:1880` saying the site is deliberately not memoed
and why, and record the growth curve in the ledger.

**Files:** `windows/vessel/src/liveness.rs:1847-1885`, `the_culvert.rs`

- [ ] **Step 1:** Thread the same `RouteMemo` into `shared_believed_water`'s
  `here`-anchored ranking, and extend Task 6's equivalence test to the
  `(here, dest)` population.
- [ ] **Step 2:** Assert a bound on `memo.len()` at the end of a full run, using
  the number Task 5 measured — not a round number chosen to pass.
- [ ] **Step 3:** `cargo fmt`, `make gate-commit`, commit.

- [ ] **Step 4: STAGE BOUNDARY** — absorb, rebaseline, `make sluice-stage`.

---

# Stage 4 — readout and close

## Task 9: The three instruments, and C1b's campaign-time counter

**Files:**
- Modify (campaign-time, REVERTED before merge): `kernel/src/astar.rs`
- Modify: `docs/superpowers/specs/2026-09-05-the-culvert-design.md` §11

**Preconditions.** A quiet box: all three load averages recorded before and
after every run, and **any run whose 1-minute average exceeds 10 at either end
is set aside and listed**. Counts (C1a) are exempt; timings (C2) are not.

- [ ] **Step 1: C1a — searches per sweep, from the committed witness**

No instrumentation needed. Report against spec §4.1's table: Shape A 679 → ≤100,
Shape B 4,060 → ≤100, `entries == searches`, memo entries ≤ 200.

- [ ] **Step 2: C1b — apply the campaign-time expansion counter**

Add a counter beside the existing `expansions += 1` in
`AStarSolver::solve` (`kernel/src/astar.rs:142`) plus a read/reset pair.

**Use `AtomicU64`, not `std::cell::Cell`** — the lexicon guard counts the token
`cell` per file against `docs/audits/lexicon-inventory.tsv` and a `Cell` here
reddens `lexicon_guard::no_vertex_sense_cell_comes_back`. This cost the
counting probe a red gate; it is written down so it costs nobody else one.

Run both shapes, record expansions against spec §4.1's C1b table (Shape A
425,042 → ≤60,000; Shape B 392,391 → ≤15,000), then **revert the counter** and
confirm `git diff --stat -- kernel/` is empty.

- [ ] **Step 3: C2 — the effect-size floor**

`k` in µs/call/fact for `believed_water` on the repaired instrument against the
max-known-water agent, before and after, both reported. **The criterion is a
10× fall in `k`. It is NOT gated on r²** — print r² as a diagnostic only. The
Detent's H4 was met on the quantity and failed its own `r² ≥ 0.5` filter, which
admitted 0 of 4 campaign runs, because a fold with no slope left cannot produce
a well-fitting line.

- [ ] **Step 4: C3 — the control**

Run `fold_depth_sweep` before and after. It sweeps `drive_at` only and
structurally cannot see this fold; report it as the no-regression control it
is, and say so.

- [ ] **Step 5: Write spec §11**

Every criterion reported separately. **A falsified prediction is a finding, not
a failure** — if C2 fails while C1a passes, say so and say what that means
(likely: the memo removed the searches and the residual O(|set|) lookup is the
remaining term, exactly as §4.2 preregistered).

- [ ] **Step 6: Commit the readout.**

---

## Task 10: Close

- [ ] **Step 1: Absorb main and regenerate** — a ratchet can land between your
  last absorption and your close, so re-absorb here even if you absorbed at the
  Stage 3 boundary.
- [ ] **Step 2: Chronicle** — `book/src/chronicle/the-culvert.md`. Technical and
  mathematical altitude, comprehensible without reading the code.
- [ ] **Step 3: Freshness sweep** of stale chapters; re-score any
  Confidence-Gradient bet this campaign moved (decision 0030), sweeping on the
  *invariant*, not the wording.
- [ ] **Step 4: Retrospective** — `docs/retrospectives/the-culvert.md`. Process
  lessons, not product.
- [ ] **Step 5: Registry** — the seven rows spec §9 lists, including correcting
  `TOOL-known-water-plan-per-water-room`'s "an A\*" (it is Dijkstra;
  `NavSpace::heuristic` returns `0`).
- [ ] **Step 6: Decisions** from block 0806–0815 — 0806 likely, 0807/0808
  conditional. Mint only what a second site would otherwise re-derive.
- [ ] **Step 7: Retire the campaign-time hash constants**, keeping the dated
  record in `the_culvert.rs`'s module doc.
- [ ] **Step 8: Update `docs/audits/campaign-reconciliation.tsv`** — the
  `spec-2026-09-05-the-culvert-design` row moves from `active` to `shipped`, and
  plan/ledger/chronicle/retrospective rows are added. This file is
  **hand-authored, never regenerated** (`docs/generated-paths.txt:200`).
- [ ] **Step 9: G6** — present the post-G3 ledger digest to Nathan and **stop**.
  Determinism entries lead it. Then `closing-a-campaign`, unchanged.
