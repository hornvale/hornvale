# The Tackle — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Prepare `windows/vessel` for one unified action system by moving the
action layer into its own module, freeing the word `affordance`, and giving
the possessed body a mass — changing no behaviour whatsoever.

**Architecture:** Three independent, behaviour-preserving changes to
`windows/vessel`. Nothing gains a caller, nothing changes a value. The
campaign's entire acceptance criterion is that **the world comes out
byte-identical**: `make rebaseline` regenerates every committed artifact and
`git diff` reports nothing.

**Tech Stack:** Rust 2024, `cargo nextest`, no new dependencies (decision
0004 — `serde`, `serde_json`, `libm` only).

**Spec:** `docs/superpowers/specs/2026-08-19-the-bridle-metaplan.md` (Arc
I.a; see §4's arc table and §6 risks 2, 3, 4).

## Global Constraints

- **No new crates.** The allowlist is `ALLOWED_EXTERNAL` in
  `cli/tests/architecture.rs`.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only, enforced by
  `clippy.toml` `disallowed-types`.
- **No wall-clock time.** Time is `WorldTime { day: f64 }`.
- Every crate sets `#![warn(missing_docs)]`; every public item, field and
  variant needs a one-line doc comment.
- **`cargo fmt` is the final step before every commit.** Skipped fmt is this
  project's most common review finding.
- Every commit must pass `make gate-commit`.
- **Never run the workspace suite twice to ask it two questions.** Capture
  once to a file, then grep the file freely.
- **Integration tests are consolidated into ONE binary per crate.** Every
  crate's integration tests live under `tests/suite/<name>.rs` and are
  declared in `tests/suite.rs` as `#[path = "suite/<name>.rs"] mod <name>;`.
  Cargo compiles each top-level `tests/*.rs` as its own binary but does not
  descend into subdirectories, which is what collapses vessel's 22 binaries
  into 1. **Never add a new top-level file under `tests/`** — it silently
  undoes that. Select a file with a name filter, not a binary name:
  `--test suite -- <name>` (commit `31616034` repointed every old
  `--test <binary>` selector this way).
- **This campaign changes no observable behaviour.** If any committed
  artifact drifts, stop — that is a defect in the change, not an expected
  rebaseline.

---

### Task 1: Prove the drift check can fail

The whole campaign rests on "`git diff` is empty after `make rebaseline`".
An empty diff is worthless as evidence until we have watched it go red — a
vacuous check reads exactly like a passing one. This task builds no product
code; its deliverable is the confidence that Tasks 2–4's acceptance test is
real.

**Files:**
- Modify (temporarily, reverted within this task):
  `windows/vessel/src/liveness.rs`

**Interfaces:**
- Consumes: nothing.
- Produces: nothing in code. Produces for later tasks the verified command
  pair below, which Tasks 2, 3 and 4 each re-run.

- [ ] **Step 1: Record the clean baseline**

```bash
cd "$(git rev-parse --show-toplevel)"
make rebaseline
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
echo "clean-exit=$?"
```

Expected: `clean-exit=0`. If it is not 0, the tree was already drifted
before this campaign began — **stop and report**, do not proceed, and do not
commit the drift.

- [ ] **Step 2: Perturb one value that must reach an artifact**

Change `REMEMBERED_PENALTY` in `windows/vessel/src/liveness.rs` from `5` to
`6`. Use `scripts/mutate.py` rather than `sed` — it asserts the target
occurs, and occurs exactly once, so a silent no-op mutation is impossible.
(A `cargo fmt` rewrap once made a one-line replacement match nothing; the
suite reported `ok`, and that `ok` was indistinguishable from a robust
implementation.)

**Take the restore copy first.** `mutate.py` deliberately does not restore,
and its own doc names the wrong way to do it:

```bash
cp windows/vessel/src/liveness.rs /tmp/hv-liveness.orig
python3 scripts/mutate.py windows/vessel/src/liveness.rs \
  'const REMEMBERED_PENALTY: u64 = 5;' \
  'const REMEMBERED_PENALTY: u64 = 6;'
```

- [ ] **Step 3: Confirm the drift check goes RED**

```bash
make rebaseline
git diff --stat -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
echo "dirty-exit=$?"
```

Expected: `dirty-exit=1`, with at least one file listed by `--stat`.

**Decision rule — do not treat "which files moved" as predicted:**
- **One or more artifacts moved** → the check is live. Record which ones in
  the board post at Step 5; those are the files Tasks 2–4 must leave
  untouched.
- **Nothing moved** → the check is VACUOUS for this campaign. **Stop and
  report.** Do not proceed to Task 2 on the strength of an acceptance test
  that cannot fail. A plausible cause is that no committed artifact exercises
  a remembered-danger path; the response is to find a value that *does* reach
  one and repeat, not to lower the bar.

- [ ] **Step 4: Revert the perturbation completely**

```bash
cp /tmp/hv-liveness.orig windows/vessel/src/liveness.rs
git diff --exit-code -- windows/vessel/src/liveness.rs
echo "source-restored-exit=$?"
make rebaseline
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
echo "restored-exit=$?"
```

Expected: both `0`.

**Restore from the copy, never with `git checkout -- <file>`.** This is not
style. `git checkout --` reverts *uncommitted work in the same file* along
with the mutation — so if the file also held a test you had just written,
that test vanishes, and its absence reads exactly like the test having
passed. The Axes lost a round to this (retrospective §4), and
`scripts/mutate.py`'s own module doc warns against it by name. `git checkout
-- .` is the same trap with a wider blast radius.

- [ ] **Step 5: Record the finding**

There is no code to commit, so record the evidence where the campaign can
read it later:

```bash
cd "$(git rev-parse --show-toplevel)"
make board-post KIND=technique BY=campaign/the-bridle \
  PATHS='windows/vessel/' \
  NOTE='The Tackle positive control: perturbing REMEMBERED_PENALTY 5->6 and running `make rebaseline` DOES redden the generated-paths drift check (name the files it moved). The byte-identity acceptance test for this campaign is therefore not vacuous. Reverted; tree clean.'
```

---

### Task 2: Extract the action layer into its own module

`liveness.rs` is 14,768 lines. The action and planning layer is a
self-contained block inside it, running from `Action`'s doc comment down to
just before `Occupancy`'s. Moving it is a pure relocation.

**Files:**
- Create: `windows/vessel/src/action.rs`
- Modify: `windows/vessel/src/liveness.rs` (remove the block; import from
  the new module)
- Modify: `windows/vessel/src/lib.rs` (declare the module)
- Modify: `windows/vessel/src/clock.rs` (import path)
- Modify: `cli/src/concepts.rs` (import path, 3 sites)
- Create: `windows/vessel/tests/suite/action_module.rs`
- Modify: `windows/vessel/tests/suite.rs` (declare the module)

**Interfaces:**
- Consumes: nothing from Task 1.
- Produces: the module `hornvale_vessel::action`, exporting exactly the
  names listed below with unchanged signatures. Task 3 renames a method on
  `Drive`, which lives in `liveness.rs` and is *not* moved by this task.

**The items to move** — named rather than given as a line range, because
line numbers shift and the compiler is the authoritative boundary:

```
  pub enum Action                              (+ its impl block:
                                                Action::all, Action::concept_name)
  pub fn is_movement
  pub fn precondition_reads_committed_state
  pub fn is_replayable_in_catch_up
  fn action_variants_must_all_be_rostered      (the compile-time tripwire)
  pub struct PlanState
  const REMEMBERED_PENALTY
  fn move_cost
  pub struct GoapSpace          + its impl SearchSpace
  pub fn plan_to_water
  struct NavSpace               + its impl NavSpace and impl SearchSpace
  pub(crate) fn plan_to_room_memo
  pub fn plan_to_room
```

`Occupancy` and everything after it **stays** in `liveness.rs`. `Drive`,
`Perceived` and the six drive impls **stay** in `liveness.rs`.

- [ ] **Step 1: Write the failing test**

Create `windows/vessel/tests/suite/action_module.rs` and declare it in
`windows/vessel/tests/suite.rs`, among the existing `#[path]` declarations:

```rust
#[path = "suite/action_module.rs"]
mod action_module;
```

The file itself:

```rust
//! The action layer is reachable as its own module, not through `liveness`.

use hornvale_kernel::room::RoomAddr;
use hornvale_vessel::action::{Action, is_movement, plan_to_water};

#[test]
fn the_action_layer_has_its_own_module() {
    let here = RoomAddr { face: 0, path: vec![0] };
    assert!(is_movement(&Action::MoveTo(here.clone())));
    assert!(!is_movement(&Action::Drink));
    // The planner is reachable here too: standing on the water, the plan is
    // the single `Drink` with no move before it.
    let plan = plan_to_water(&here, &here, 64, &std::collections::BTreeSet::new())
        .expect("standing on the water, a plan always exists");
    assert_eq!(plan, vec![Action::Drink]);
}

#[test]
fn every_action_variant_still_carries_a_concept_name() {
    // The Actants' contract survives the move: each variant answers to a
    // concept, and the roster stays exhaustive.
    for a in Action::all() {
        assert!(!a.concept_name().is_empty(), "{a:?} has no concept name");
    }
    assert!(Action::all().len() >= 5);
}
```

- [ ] **Step 2: Run it and watch it fail for the right reason**

```bash
cargo test -p hornvale-vessel --test suite -- action_module > /tmp/hv-t2-red.log 2>&1; echo "exit=$?"
tail -20 /tmp/hv-t2-red.log
```

Expected: a compile error naming the unresolved module — `unresolved import
hornvale_vessel::action` or `could not find action in hornvale_vessel`.

**This red is a compile error, and a compile error proves nothing about
behaviour.** It only proves the module does not exist yet. The behavioural
evidence for this task is Step 6's byte-identity check, not this failure.

- [ ] **Step 3: Create the module and move the block**

Create `windows/vessel/src/action.rs` with a module doc comment, then move
the listed items into it **verbatim** — including every existing doc
comment, every `type-audit:` tag, and the `action_variants_must_all_be_rostered`
tripwire. Change no logic, no constant, no signature.

```rust
//! The action layer: what a body can do, and the search spaces that plan
//! sequences of it.
//!
//! Extracted from `liveness.rs` by The Tackle (Arc I.a of The Bridle) with
//! no behavioural change, so that The Deed can unify the player's verbs
//! with these without doing surgery inside a 14,700-line file.
```

Add to `windows/vessel/src/lib.rs`, in alphabetical position among the
existing `pub mod` declarations:

```rust
pub mod action;
```

- [ ] **Step 4: Let the compiler enumerate the fallout**

```bash
cargo check --workspace --all-targets > /tmp/hv-t2-check.log 2>&1; echo "exit=$?"
tail -40 /tmp/hv-t2-check.log
```

Fix each unresolved path by importing from `crate::action` /
`hornvale_vessel::action`. The known sites are `liveness.rs`, `clock.rs`
(2), and `cli/src/concepts.rs` (3) — but **trust the compiler's list over
this one**: it is authoritative and this sentence is a snapshot.

Repeat until the check is clean.

- [ ] **Step 5: Run the vessel suite**

```bash
cargo nextest run -p hornvale-vessel > /tmp/hv-t2-tests.log 2>&1; echo "exit=$?"
grep -E "^ *Summary|FAIL|panicked" /tmp/hv-t2-tests.log
```

Expected: exit 0. The vessel suite is the behavioural check — every existing
assertion about planning, drives and possession must still hold against
relocated code, including the new `action_module` tests.

- [ ] **Step 6: Prove byte-identity**

```bash
cd "$(git rev-parse --show-toplevel)"
make rebaseline
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
echo "drift-exit=$?"
```

Expected: `drift-exit=0`.

**Decision rule:**
- **`drift-exit=0`** → the extraction was pure. Proceed.
- **Anything drifted** → this is a DEFECT, not a rebaseline. A pure move
  cannot change output. Do not commit the regenerated artifacts. Diff them
  to find what the move altered — the usual causes are a constant retyped
  while moving, a `use` that resolved to a different item, or an item
  accidentally left behind and duplicated. Fix the cause and re-run.

- [ ] **Step 7: Format, gate, and commit**

```bash
cargo fmt
make gate-commit
git add windows/vessel/src/action.rs windows/vessel/src/liveness.rs \
        windows/vessel/src/lib.rs windows/vessel/src/clock.rs \
        cli/src/concepts.rs windows/vessel/tests/suite/action_module.rs \
        windows/vessel/tests/suite.rs
git commit -F - <<'MSG'
refactor(vessel): extract the action layer into its own module

A pure relocation out of liveness.rs (14,768 lines) with no behavioural
change: Action and its concept roster, PlanState, GoapSpace, NavSpace, the
two planners and the remembered-danger cost move to
windows/vessel/src/action.rs. Occupancy, Drive and the drive impls stay.

The Deed (Arc I.b) unifies the player's verbs with these, and doing that
inside a 14.7k-line file is how the semantic work would get lost.

Byte-identical: make rebaseline moves nothing.

Claude-Session: https://claude.ai/code/session_01QP6ZQxUrPwcSJs4fYJDvDg
MSG
```

**Use `git add <paths>`, never bare `git commit -a`** — `git commit` takes
the whole index, and this worktree may carry unrelated staged work.

---

### Task 3: Rename `Drive::affordance` to `Drive::proposal`

The word is wanted for Gibson's sense in Arc IV — an object advertising what
can be done *with* it. `Drive::affordance` is the opposite arrow: a drive
proposing its own next step. One word cannot hold both, and renaming now is
far cheaper than renaming when both senses are live.

**All 22 call sites and all 6 implementations are inside `liveness.rs`** —
verified with `grep -rn '\.affordance(' windows/vessel/`. This is a wide
diff confined to one file, not cross-crate churn.

**Files:**
- Modify: `windows/vessel/src/liveness.rs` (trait declaration, 6 impls, 22
  call sites, and the doc comments that name the method)
- Modify: `docs/audits/type-audit-report.md` (regenerated; see Step 5)

**Interfaces:**
- Consumes: `windows/vessel/src/action.rs` from Task 2 (the `Action` type
  the method returns).
- Produces: `Drive::proposal(&self, view: &Perceived, budget: usize) ->
  Option<Action>`, replacing `Drive::affordance` with an identical
  signature. No other name changes.

- [ ] **Step 1: Count the implementors before touching anything**

```bash
cd "$(git rev-parse --show-toplevel)"
grep -c 'fn affordance' windows/vessel/src/liveness.rs
grep -c '\.affordance(' windows/vessel/src/liveness.rs
grep -rn '\.affordance(\|fn affordance' --include='*.rs' . | grep -v 'windows/vessel/src/liveness.rs' | grep -v '^\./target'
```

Expected: `7` (1 trait declaration + 6 impls), `22`, and **no output** from
the third command.

**Do not count implementors with `grep 'impl Drive for'`** — it returns 3,
not 6, because three drives are written `impl<'a> Drive for Thermal<'a>` and
the pattern requires a space after `impl`. The method signature is the
reliable observable; the impl header is not.

**Decision rule:** if the third command prints anything, the rename is NOT
confined to one file. Widen the change to those files and say so in the
commit message — do not assume this plan's scope is still correct.

- [ ] **Step 2: Rename, and record why in the trait's doc**

Rename the trait method and all 22 call sites. Then extend the method's own
doc comment so the reservation is discoverable from the definition:

```rust
    /// The next executable step that reduces this drive from the view's
    /// position, or `None` when it cannot currently be advanced (its target
    /// is unreachable within `budget`, or there is nowhere new to look).
    ///
    /// **Named `proposal`, not `affordance`, deliberately** (The Tackle,
    /// Arc I.a). This is the actor→action arrow: a drive proposing its own
    /// next step. Gibson's *affordance* is the object→actor arrow — a thing
    /// advertising what may be done with it — and Arc IV (The Offer) needs
    /// the word for exactly that. One term cannot carry both directions.
    /// type-audit: bare-ok(count: budget)
    fn proposal(&self, view: &Perceived, budget: usize) -> Option<Action>;
```

- [ ] **Step 3: Verify no identifier named `affordance` survives**

```bash
grep -rn 'affordance' --include='*.rs' . | grep -v '^\./target'
```

Expected: only the doc comment written in Step 2, which mentions the word in
order to explain the reservation. **No identifier** named `affordance`
should remain — read the output rather than counting it.

- [ ] **Step 4: Compile and test**

```bash
cargo nextest run -p hornvale-vessel > /tmp/hv-t3-tests.log 2>&1; echo "exit=$?"
grep -E "^ *Summary|FAIL|panicked" /tmp/hv-t3-tests.log
```

Expected: exit 0. A rename that compiles and leaves every drive assertion
green is behaviour-preserving by construction.

- [ ] **Step 5: Prove byte-identity**

```bash
cd "$(git rev-parse --show-toplevel)"
make rebaseline
git diff --stat -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
echo "drift-exit=$?"
```

**Decision rule:**
- **`drift-exit=0`** → proceed; drop `docs/audits/` from Step 6's `git add`.
- **Only `docs/audits/type-audit-report.md` moved** → expected; the report
  is keyed on public-boundary signatures and this changed one. **Commit it
  in this same commit** — a report that drifts in one commit and is
  committed in the next leaves main red in between.
- **Anything else moved** → a rename changed behaviour, which is a defect.
  Investigate rather than rebaseline.

- [ ] **Step 6: Format, gate, and commit**

```bash
cargo fmt
make gate-commit
git add windows/vessel/src/liveness.rs docs/audits/
git commit -F - <<'MSG'
refactor(vessel): rename Drive::affordance to Drive::proposal

Frees Gibson's word for Arc IV (The Offer), where objects advertise what may
be done with them. The two are opposite arrows -- Drive::affordance is
actor->action ("what do I want next"), Gibson's is object->actor ("what does
this permit") -- and one term cannot carry both. Renaming now costs 22 call
sites in one file; renaming once both senses are live costs much more.

1 trait declaration + 6 impls + 22 call sites, all in liveness.rs.
Behaviour-preserving.

Claude-Session: https://claude.ai/code/session_01QP6ZQxUrPwcSJs4fYJDvDg
MSG
```

---

### Task 4: Derive a body's mass through one shared function

The cost model (`clock::cost_ticks(action, mass_kg, terrain_factor)`) already
charges time as a function of the body, with no driver parameter. It has
never been reachable for the possessed body, because that body has no mass:
`Agent` carries `id`, `species`, `perception`, `position`, `village` and
nothing else. Meanwhile the creature layer derives mass **inline**.

So the task is not "add a field to `Agent`" — it is **extract the existing
derivation into one function and make it reachable for both**, so that in
Arc I.b the player provably pays the same tariff as a creature rather than a
parallel one.

**Files:**
- Modify: `windows/vessel/src/clock.rs` (the shared derivation belongs
  beside `REFERENCE_MASS_KG` and `tempo`)
- Modify: `windows/vessel/src/liveness.rs` (the creature derivation calls it)
- Create: `windows/vessel/tests/suite/body_mass.rs`
- Modify: `windows/vessel/tests/suite.rs` (declare the module)

**Interfaces:**
- Consumes: `clock::REFERENCE_MASS_KG` (existing).
- Produces:

```rust
pub fn mass_for_species(species: &str, biosphere: Option<&Biosphere>) -> f64
```

Returns the species' mass in kilograms, falling back to `REFERENCE_MASS_KG`
when the biosphere is absent **or** the species is not in it. Arc I.b calls
this for the possessed body.

**The exact type standing in for `Biosphere`** is whatever `liveness.rs`'s
existing derivation holds when it calls `.get_by_label(&species)`. Read that
call site and use its type; do not invent one, and do not add a dependency
to `clock.rs` that would make it require a world to test — its current
freedom from ledger access is what lets it be unit-tested, and that is worth
preserving.

- [ ] **Step 1: Write the failing test**

Create `windows/vessel/tests/suite/body_mass.rs` and declare it in
`windows/vessel/tests/suite.rs` alongside the others:

```rust
#[path = "suite/body_mass.rs"]
mod body_mass;
```

The file itself:

```rust
//! One derivation of a body's mass, shared by every body.

use hornvale_vessel::clock::{REFERENCE_MASS_KG, mass_for_species};

#[test]
fn an_absent_biosphere_falls_back_to_the_reference_mass() {
    // liveness.rs states this fallback in a comment -- "a defaulted creature
    // reads at exactly tempo 1.0" -- and nothing asserted it. Now something
    // does.
    assert_eq!(mass_for_species("anything", None), REFERENCE_MASS_KG);
}

#[test]
fn an_unknown_species_falls_back_to_the_reference_mass() {
    assert_eq!(
        mass_for_species("no-such-species-exists", None),
        REFERENCE_MASS_KG
    );
}
```

- [ ] **Step 2: Run it and watch it fail**

```bash
cargo test -p hornvale-vessel --test suite -- body_mass > /tmp/hv-t4-red.log 2>&1; echo "exit=$?"
tail -20 /tmp/hv-t4-red.log
```

Expected: a compile error — `cannot find function mass_for_species`.

- [ ] **Step 3: Extract the derivation**

Move the body of `liveness.rs`'s inline mass lookup into
`clock::mass_for_species`, preserving its fallback exactly, and change the
creature site to call it. The creature's computed value must be
**unchanged** — this is an extraction, not a redefinition.

- [ ] **Step 4: Add the equivalence assertion**

Append to `windows/vessel/tests/suite/body_mass.rs` a test proving the creature
layer and `mass_for_species` agree for the same species.

**Do not hardcode a species name.** A hardcoded species that later leaves
the roster silently turns this into a vacuous pass against the fallback,
which is indistinguishable from success. Obtain a species from the roster at
runtime and assert the two paths return the same value for it. If a
biosphere cannot be obtained without building a world, assert instead that
the creature site and `mass_for_species` are the *same call* — i.e. that the
inline derivation no longer exists — and say so in the test's doc comment,
naming what that does and does not prove.

- [ ] **Step 5: Test and prove byte-identity**

```bash
cargo nextest run -p hornvale-vessel > /tmp/hv-t4-tests.log 2>&1; echo "exit=$?"
grep -E "^ *Summary|FAIL|panicked" /tmp/hv-t4-tests.log
cd "$(git rev-parse --show-toplevel)"
make rebaseline
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
echo "drift-exit=$?"
```

Expected: tests exit 0, `drift-exit=0`.

**Decision rule:** any drift means the extraction changed a creature's mass,
which changes every action cost it pays and therefore its whole trajectory.
That is a defect. Diff the creature derivation against the original and find
the discrepancy — do not rebaseline it away.

- [ ] **Step 6: Format, gate, and commit**

```bash
cargo fmt
make gate-commit
git add windows/vessel/src/clock.rs windows/vessel/src/liveness.rs \
        windows/vessel/tests/suite/body_mass.rs windows/vessel/tests/suite.rs \
        docs/audits/
git commit -F - <<'MSG'
feat(vessel): derive a body's mass through one shared function

clock::cost_ticks(action, mass_kg, terrain_factor) already charges time as a
function of the body with no driver parameter -- it was built for The
Bridle's keystone and simply never had a player routed through it. The
blocker is that the possessed body has no mass, while the creature layer
derives one inline.

Extracts that derivation to clock::mass_for_species, shared by both, so that
in The Deed the player provably pays the same tariff as a creature rather
than a parallel one. Fallback to REFERENCE_MASS_KG on an absent biosphere or
an unknown species -- unchanged, and now asserted.

Byte-identical: creature masses are unmoved.

Claude-Session: https://claude.ai/code/session_01QP6ZQxUrPwcSJs4fYJDvDg
MSG
```

---

### Task 5: Campaign acceptance

**Files:**
- Create: `book/src/chronicle/the-tackle.md`
- Create: `docs/retrospectives/the-tackle.md`
- Modify: `docs/retrospectives/README.md` (the grouped one-line index)

**Interfaces:**
- Consumes: Tasks 1–4 complete and committed.
- Produces: the Definition of Done artifacts (decisions 0013, 0020), which
  are a precondition for submitting to the sluice, not cleanup afterwards.

- [ ] **Step 1: Full-workspace verification, captured once**

`cargo nextest` does not run doctests, so both runs are needed — but each is
captured once and grepped, never re-run to ask a second question.

```bash
cd "$(git rev-parse --show-toplevel)"
cargo nextest run --workspace > /tmp/hv-tackle-nextest.log 2>&1; echo "nextest-exit=$?"
grep -E "^ *Summary|FAIL|panicked" /tmp/hv-tackle-nextest.log
```

```bash
cargo test --workspace --doc > /tmp/hv-tackle-doc.log 2>&1; echo "doc-exit=$?"
grep -E "^test result|FAILED" /tmp/hv-tackle-doc.log
```

Expected: both exits 0.

- [ ] **Step 2: Final byte-identity proof**

```bash
make rebaseline
git status --short
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
echo "final-drift-exit=$?"
```

Expected: `final-drift-exit=0` and a clean `git status`.

**This is the campaign's whole acceptance criterion.** Task 1 established
that this check can go red, so a zero here means something.

- [ ] **Step 3: Write the chronicle entry**

`book/src/chronicle/the-tackle.md`, at the book's usual altitude —
technical and mathematical, comprehensible without reading the code it may
show. Cover: why a behaviour-free campaign earned its own merge (Arc I.b's
artifact drift would otherwise arrive tangled with a 22-site rename, and no
reviewer could tell which change moved which byte); what the action layer is
and why it wanted its own module; the two arrows that made `affordance`
ambiguous; and the finding that the cost model already took no driver
parameter — the architecture had agreed with the keystone before anyone
stated it.

- [ ] **Step 4: Write the retrospective**

`docs/retrospectives/the-tackle.md` — process lessons, not product. Carry
forward from `.superpowers/sdd/decision-ledger.md` entries #11, #12 and #15:

- a grep for `impl Drive for` that returned 3 where the truth was 6, because
  three impls are generic — count the method, not the impl header;
- `Session::ledger`'s doc comment ("Never written back") which was true of
  the input world and false of the question being asked, and which reached
  both a conversation and a spec before being caught;
- a fidelity question escalated to Nathan that `clock.rs` had already
  settled — before flagging anything as undefined, grep for the thing that
  would already implement it.

Add the one-line entry to `docs/retrospectives/README.md`, and promote the
open items from `.superpowers/sdd/followups.md` into the retrospective's
follow-up section — that scratch file is git-ignored and dies with the
worktree.

- [ ] **Step 5: Commit**

```bash
cargo fmt
make gate-commit
git add book/src/chronicle/the-tackle.md docs/retrospectives/the-tackle.md \
        docs/retrospectives/README.md
git commit -F - <<'MSG'
docs(the-tackle): chronicle and retrospective

Claude-Session: https://claude.ai/code/session_01QP6ZQxUrPwcSJs4fYJDvDg
MSG
```

- [ ] **Step 6: Submit through the sluice**

Never push to main directly — `scripts/hooks/pre-push` refuses any push not
holding the canonical box's live claim, and the sluice is the only holder.

```bash
git push -u origin campaign/the-bridle
make sluice BRANCH=campaign/the-bridle REF=$(git rev-parse HEAD)
```

The merge **refuses without an authored `Sluice-Headline:` trailer**, which
must sit in the same trailer block as `Claude-Session` with no blank line
between them.

This branch carries The Bridle's metaplan as well as The Tackle's code, so
the headline should name both: the program was specified and its first,
behaviour-free campaign landed.
