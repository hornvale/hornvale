# The Millrace Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the laboratory's read path over the river network cheap again
without moving a single committed world.

**Architecture:** Three independent levers on one primitive. **L0** removes a
duplicate all-lines scan in the transect sweep. **L2** gives
`ChannelNetwork::nearest_line` a vertex-keyed spherical index that narrows the
*candidate line set* only, leaving the tie-break arithmetic untouched. **L1**
memoises `channel-connectivity`'s walk over its shared trunk suffixes, which is
also the precondition for repairing its vacuous continuation predicate.

**Tech Stack:** Rust 2024, `std` only. Dependency allowlist is `serde`,
`serde_json`, `libm` (decisions 0004 / 0041) — **no spatial-index crate**.
`cargo nextest` is the gate runner.

**Spec:** `docs/superpowers/specs/2026-08-13-the-millrace-design.md` — read it
first. Every task argues from it.

## Global Constraints

- **Byte-identity is the acceptance criterion, not a nice-to-have.** A green
  test suite is not evidence: the columns at risk live in `#[ignore]`d probes
  the gate never runs. Every task that touches `domains/terrain` or
  `windows/lab` ends with `make rebaseline` and the full drift diff.
- No `HashMap` / `HashSet` — `BTreeMap` / `BTreeSet` / `Vec` only
  (`clippy.toml` `disallowed-types`).
- No wall-clock time anywhere, **including `std::time::Instant` in test code**.
- Float ordering via `total_cmp` with deterministic tie-breaks.
- Every crate is `#![warn(missing_docs)]`; every `pub` item, field and variant
  gets a one-line doc comment.
- **Any change to a `pub` boundary drifts `docs/audits/type-audit-report.md`,
  and it must be regenerated in the SAME commit.** `cargo run
  --manifest-path tools/type-audit/Cargo.toml -- report >
  docs/audits/type-audit-report.md`. Every new `pub` primitive needs a
  `type-audit:` verdict tag on the item.
- `cargo fmt` is the final step before every commit.
- **The workspace enforcement tests live in `cli/`** — a `-p hornvale-terrain`
  green is not a branch-green. `make gate` before every commit.
- **The box is quiet** (Nathan, at G3): this is the only campaign running. Take
  `uptime` either side of every measurement anyway and paste it; if it shows
  load, the number is a contention datum and must say so.
- **Never run a census locally.** `HV_CENSUS=1` is the controller's business
  and runs on `lefford` via `scripts/census-run.sh`. Do not set it.

---

## File Structure

| file | responsibility | tasks |
|---|---|---|
| `domains/terrain/src/channel.rs` | `BankReading::transverse()`; the vertex index; `nearest_line` + `nearest_line_reference` | 1, 4 |
| `domains/terrain/tests/channel_properties.rs` | index-vs-reference equality; the `nearest_line` tie-break contract | 3, 4 |
| `domains/terrain/tests/rill_properties.rs` | the `rill_reading` tie-break contract | 3 |
| `windows/lab/src/metrics.rs` | the transect sweep; `lab_channel_connectivity` and its memo | 1, 5 |
| `windows/lab/tests/millrace_probe.rs` | **new** — the `k`-distribution and vacuity probes | 2, 5 |
| `docs/audits/type-audit-report.md` | regenerated wherever a `pub` boundary moves | 1, 4 |

---

## Task 1: L0 — collapse the duplicate all-lines scan

The sweep runs the 3,606-line scan **twice per probe**: once at
`metrics.rs:7335` to ask which line owns the probe, once inside `transverse_at`
at `:7338` to classify the band. `bank_reading` already computes both. This is a
~2x on the largest lab term at zero determinism risk, and it ships **first** so
that Task 4's index is measured against a tree with no obvious 2x left in it.

**Files:**
- Modify: `domains/terrain/src/channel.rs:804-812` (`transverse_at`), and the
  `impl BankReading` block near `:489`
- Modify: `windows/lab/src/metrics.rs:7332-7352`
- Modify: `docs/audits/type-audit-report.md` (regenerated)

**Interfaces:**
- Produces: `pub fn BankReading::transverse(&self) -> Transverse` — the band
  classification of this reading. Task 5 does **not** use it; nothing else
  depends on this task.

- [ ] **Step 1: Add `BankReading::transverse()` and make `transverse_at` delegate to it**

`transverse_at` currently inlines the classification. Move that one expression
onto `BankReading` so there is exactly one implementation, and have
`transverse_at` call it. This is the same "there can only be one" argument
`bank_signed_distance` already makes for delegating to `nearest_line`
(`channel.rs:976-983`).

In `impl BankReading` (add the block if none exists, next to the struct at
`channel.rs:489`):

```rust
    /// The band this reading falls in — the classification
    /// [`ChannelNetwork::transverse_at`] reports, computed from the reading
    /// rather than from a second query.
    ///
    /// This exists so a caller that already holds a [`BankReading`] never has
    /// to re-run [`ChannelNetwork::nearest_line`] to classify it. That
    /// duplication was real: the lab's transect sweep ran the all-lines scan
    /// twice per probe, once for the owning line and once for the band.
    pub fn transverse(&self) -> Transverse {
        Transverse::from_band(band(self.signed_distance, &self.band_edges))
    }
```

**Amended after Task 1's review (`8e22fc23`).** This block originally carried a
`type-audit: bare-ok(enum: return)` line above `pub fn transverse`, and the
implementation copied it faithfully. There is no `enum` class: `BARE_OK_CLASSES`
(`tools/type-audit/src/tag.rs`) does not list one, so `parse_tag` would reject
the tag with "unknown bare-ok class" the moment `Transverse` acquired a tracked
primitive and the item became an `AuditItem`. It was inert by luck rather than
valid, and `type-audit check` cannot catch a tag on an item it never audits. The
line is deleted here so the plan no longer prescribes it; the prose doc comment
is what `#![warn(missing_docs)]` actually requires.

`band` is already imported in this file from `hornvale_kernel`
(`channel.rs:22`). Then rewrite `transverse_at`'s body:

```rust
    pub fn transverse_at(&self, position: [f64; 3]) -> (Transverse, f64) {
        let Some(reading) = self.bank_reading(position) else {
            return (Transverse::Dry, f64::INFINITY);
        };
        (reading.transverse(), reading.signed_distance)
    }
```

- [ ] **Step 2: Verify the delegation changed nothing**

Run: `cargo test -p hornvale-terrain 2>&1 | tee /tmp/hv-t1a.txt`
Expected: PASS. This is a pure expression move; if anything reddens, stop and
report — it means the two expressions were not the same and the premise of this
task is wrong.

- [ ] **Step 3: Collapse the two scans in the sweep**

Replace `metrics.rs:7334-7338`. The current code is:

```rust
                    let q = lab_offset(line, j, left, offset);
                    if net.nearest_line(q).map(|(k, _)| k) != Some(i) {
                        still_own = false;
                    }
                    let band = net.transverse_at(q).0.index();
```

Replace with:

```rust
                    let q = lab_offset(line, j, left, offset);
                    // ONE scan, not two. `bank_reading` already resolves the
                    // winning line and the geometry the band is read from, so
                    // asking `nearest_line` separately re-ran the whole
                    // all-lines scan to recover a field this reading carries.
                    let (owns, band) = match net.bank_reading(q) {
                        Some(reading) => (reading.line == i, reading.transverse().index()),
                        None => (false, hornvale_terrain::channel::Transverse::Dry.index()),
                    };
                    if !owns {
                        still_own = false;
                    }
```

**The `None` arm is exact, and it is the part to check rather than assume.**
The old code's `nearest_line(q).map(..) != Some(i)` is `true` when
`nearest_line` returns `None`, giving `still_own = false`; and `transverse_at`
returns `Transverse::Dry` on a `None` reading (`channel.rs:805-807`). The match
above reproduces both.

- [ ] **Step 4: Prove the change is observable, then prove it is neutral**

Two separate things, and the order matters.

*Observable:* the point of this task is a call-count reduction, and no
assertion in the tree counts calls. So demonstrate it by measurement, not by
test: take matched arms on the level-6 sweep and report wall and peak RSS.

```bash
cd .claude/worktrees/the-millrace
uptime
git stash            # before-arm: the tree as it was
/usr/bin/time -l cargo test --release -p hornvale-lab --test <the transect test> 2>&1 | tail -20
git stash pop        # after-arm
/usr/bin/time -l cargo test --release -p hornvale-lab --test <the transect test> 2>&1 | tail -20
uptime
```

Identify the owning test by reading `windows/lab/tests/` for the one that
exercises `lab_band_transects` — do not guess its name. If no test exercises it
at level 6, say so and measure through the smallest study that does.

*Neutral:* the values must not move.

Run: `make gate 2>&1 | tee /tmp/hv-t1-gate.txt`
Expected: PASS.

- [ ] **Step 5: Regenerate artifacts and diff**

```bash
make rebaseline
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
git diff --stat book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/ docs/digest/ book/src/domesday/ clients/game/core/tests/fixtures/
```

**Decision rule — do not predict, branch:**
- Only `docs/audits/type-audit-report.md` moved → expected (this task adds a
  `pub fn`). Commit it in this commit.
- `book/src/laboratory/` moved → **STOP.** A census column changed value. This
  task is byte-identical by construction, so a move means the construction
  argument is wrong. Report with the diff.
- Anything else moved → **STOP** and report before committing.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add domains/terrain/src/channel.rs windows/lab/src/metrics.rs docs/audits/type-audit-report.md
git commit -- domains/terrain/src/channel.rs windows/lab/src/metrics.rs docs/audits/type-audit-report.md
```

Message: `perf(lab): one bank_reading per transect probe, not two scans`.
State the measured before/after and the `uptime` either side in the body.

---

## Task 2: Measure `k` — the falsification hinge for Task 4

Task 4 is the campaign's most expensive task and rests entirely on an unmeasured
quantity: how much a cap of radius `D + L_max/2` actually shrinks the candidate
set. Measure it **before** building anything. If the median `k` is below 8, the
index is falsified and the campaign says so.

**Files:**
- Create: `windows/lab/tests/millrace_probe.rs`

**Interfaces:**
- Consumes: nothing from Task 1.
- Produces: **the probe harness itself**, `windows/lab/tests/millrace_probe.rs`,
  which Task 4 Step 7 re-runs as its measurement and Task 5 Step 1 extends with
  the vacuity count. Build it to be extended — a world-building helper and a
  percentile-printing helper that a second probe can call, not one monolithic
  `#[test]` body. Also produces the `k` distribution that gates Task 4.

- [ ] **Step 1: Write the probe as an `#[ignore]`d test**

It must be `#[ignore]`d with a reason naming its cost — `windows/lab`'s
`preregistration_guard` is default-deny on `#[ignore]` reasons that do not name
a cost or cite a decision. Use a reason of the form
`"probe: <what it costs>"`, matching the existing `rill_probe.rs` style — read
that file for the exact convention rather than copying this sentence.

The probe must, for a real seed-42 world at `GLOBE_LEVEL` (6):

1. Build the `ChannelNetwork` and compute `L_max` = the maximum over every
   consecutive vertex pair of `angle(v_j, v_{j+1})`.
2. **Assert the theoretical ceiling holds:** `L_max <= 1.5 * E_max`, where
   `E_max` is the mesh's longest cell-to-neighbour edge, computed in the probe
   by walking `geo.neighbors`. A red here means the spec's segment-length
   argument (§3.2) is wrong somewhere and Task 4 must not proceed.
3. Replay the **real** query population — the probe positions
   `lab_band_transects` generates, and the seven interpolated join probes
   `lab_channel_connectivity` generates. Do not invent a synthetic population;
   `k` is a property of the queries actually made.
4. For each query, compute the true answer distance `D` by the existing linear
   scan, then count `|candidates|` = the number of distinct polylines with a
   vertex within `D + L_max/2` of the query point.
5. Print `L_max`, `E_max`, the query count, and the **distribution** of
   `k = L / |candidates|` — minimum, 5th, 25th, median, 75th, 95th percentile,
   maximum. A mean is not sufficient and must not be the only figure reported.

- [ ] **Step 2: Run it under both continuation predicates**

The connectivity repair in Task 5 **changes the query population** — walks chain
to the sea instead of stopping at the first sub-threshold trunk, so more joins
are probed and in different places. `k` is a property of that population, so
measure it both ways: once with the shipped `WaterKind::River` test, once with
the repaired `owner[last_cell].is_some()` test. Report two distributions.

Run: `cargo test --release -p hornvale-lab --test millrace_probe -- --ignored --nocapture 2>&1 | tee /tmp/hv-t2.txt`

- [ ] **Step 3: Score against the preregistered prediction**

Spec §2.1, prediction P2: median `k >= 8`, 95th-percentile-worst `k >= 2`.

**Decision rule:**
- Both hold, under both predicates → Task 4 proceeds.
- Median `k >= 8` but the 95th percentile is below 2 → Task 4 proceeds, and the
  spec's §3.2 iteration step (re-gather at a larger radius) is load-bearing
  rather than a formality. Say so.
- Median `k < 8` → **P2 is falsified. STOP.** Report the distribution and the
  reason. The campaign ships L0 + L1 and records the null as a finding. Do not
  build the index to see if it helps anyway.

- [ ] **Step 4: Commit**

```bash
cargo fmt && make gate
git add windows/lab/tests/millrace_probe.rs
git commit -- windows/lab/tests/millrace_probe.rs
```

Message: `measure(lab): the candidate-set distribution a nearest-line index would see`.
Put the two distributions and the P2 verdict in the body.

---

## Task 3: The two tie-break contracts, held by assertions

Both tie-breaks are documented today and held by **nothing**. Task 4 is about to
change the search order of one of them.

**Files:**
- Modify: `domains/terrain/tests/channel_properties.rs`
- Modify: `domains/terrain/tests/rill_properties.rs`

**Interfaces:**
- Produces: two named tests Task 4's review will re-run. Name them for what they
  assert, not for this campaign.

- [ ] **Step 1: Assert `nearest_line`'s tie-break**

The contract (`channel.rs:869-888`): the winner is the lexicographic argmin over
`(|d|, index)` — minimise `|d|`, and on an **exact** `|d|` tie the **lowest
index** wins. That tie-break reaches the serialized *sign*, because the winner's
downstream direction is what `bank_signed_distance` reports.

Write a test that constructs a position **exactly equidistant from two lines**
and asserts the lower-indexed one wins and its sign is reported. Build the
network by hand (the in-crate `test_network()` helper shape) so the tie is exact
rather than hoped for — a tie manufactured on a real world is a tie you cannot
guarantee.

- [ ] **Step 2: Assert `rill_reading`'s tie-break**

The contract (`branch.rs:782-798`): candidates are `here` first, then
`geo.neighbors(here)` in the geosphere's yield order, and `best.is_none_or(|(d, _, _)| distance < d)`
is a **strict** `<`, so an exact tie keeps whichever cell was offered first.
Its serialized path is `rill_reading -> grounded_wetness -> micro.wetness`.

- [ ] **Step 3: Prove each assertion can fail**

**Do not take a mutation from this plan.** A plan author does not know which
perturbations are observable; the implementer does, after reading. Find a
mutation for each test that (a) type-checks, (b) inverts or relaxes the
tie-break specifically, and (c) reddens the new test. Report which mutation you
chose and why.

Two rules, both learned the hard way at The Rill's close:

- **A mutation must prove it mutated.** Assert the target text exists before
  substituting it. A `cargo fmt` rewrap once made a single-line replacement
  match nothing, and the resulting green looked exactly like a robust
  implementation.
- **Verify the revert by re-running after `touch`, never by grepping the
  source.** `mv file.bak file` can restore an mtime *older* than the compiled
  binary, so cargo skips the rebuild and re-runs the **mutated** binary against
  reverted source. One direction gives a false red; the other gives a false
  **green**, which silently invalidates the whole proof.

```bash
# after reverting:
touch domains/terrain/src/channel.rs domains/terrain/src/branch.rs
cargo test -p hornvale-terrain --test channel_properties --test rill_properties
```

- [ ] **Step 4: Commit**

```bash
cargo fmt && make gate
git commit -- domains/terrain/tests/channel_properties.rs domains/terrain/tests/rill_properties.rs
```

Message: `test(terrain): hold both nearest-search tie-breaks with an assertion`.
Paste the mutation-red output and the post-`touch` green in the body.

---

## Task 4: The index, and the oracle that outlives it

**Gated on Task 2's P2 verdict.** Do not start until it reads "proceed".

**Files:**
- Modify: `domains/terrain/src/channel.rs` (the `ChannelNetwork` struct, `build`,
  `nearest_line`)
- Modify: `domains/terrain/tests/channel_properties.rs`
- Modify: `docs/audits/type-audit-report.md`

**Interfaces:**
- Consumes: Task 3's tie-break test, which must stay green.
- Produces: `nearest_line` unchanged in signature and return value.
  `nearest_line_reference` is **private** — it exists for the test, and it is
  reached from the integration test via a `#[doc(hidden)] pub` accessor or an
  in-crate `#[cfg(test)]` module; choose by reading how the crate already
  exposes internals to `tests/` and follow that, rather than inventing a third
  way.

- [ ] **Step 1: Preserve the linear scan as the oracle**

Rename the existing body to `nearest_line_reference`, leaving it
**byte-for-byte the loop it is today**. It is never deleted. An index whose
reference implementation is gone is an index nobody can re-verify.

- [ ] **Step 2: Write the equality test FIRST, against the reference alone**

Before any index exists, write the property test that will hold the index, and
watch it pass with `nearest_line` still delegating to the reference. That green
is the positive control: it proves the test harness, the position sample and the
comparison are sound, on a tree where the answer cannot be wrong.

The sample must include, and the test must say why each is present:
- levels 4 through 7;
- positions **beyond every line's endpoint** (the sign is meaningless there and
  the distance is to the nearer endpoint — a region the index must still get
  right);
- **polar positions** — The Bearing's index shipped with a near-pole coverage
  hole, and a `cover/cos(lat)` longitude window is exactly how it returns;
- positions at a confluence, where the repair has placed two lines' vertices on
  the same point.

Assert the **full return value**, `Option<(usize, f64)>` — not just the index,
and not an approximate distance. The `f64` must be bit-equal.

- [ ] **Step 3: Build the index inside `build`**

Store it on `ChannelNetwork` (a private field — the struct's public fields are a
documented surface and this is not part of it). It must carry its own vertex
positions: `ChannelNetwork` holds no `Geosphere` and `nearest_line` takes no
index parameter, and changing that signature ripples through `provider.rs:500`,
five `windows/locale` call sites and `metrics.rs`.

Build it **after** the confluence repair pass (`channel.rs:731-747`), because
that pass moves mouth vertices.

Compute and store `L_max` in the same O(V) pass. It is the measured bound the
correctness argument rests on; do not hard-code the `1.5 * E_max` ceiling as the
bound — that ceiling is Task 2's tripwire on this measurement, not a substitute
for it.

- [ ] **Step 4: Implement the query**

Per spec §3.2. The parts that are non-negotiable:

- Gather candidates from the cap of radius `rho = D + L_max/2`, then **evaluate
  the unchanged `line.signed_distance(position)`** on each candidate in
  **ascending polyline index**, applying the unchanged `d.abs() < best.abs()`.
  Never re-derive the sign from a segment — that would duplicate the
  degenerate-segment side-borrowing and the intra-line tie-break.
- Iterate: if `D + L_max/2 > rho`, re-gather at the larger radius. `D` decreases
  monotonically, so this terminates.
- **If `rho >= pi`, fall back to the reference scan.** A non-empty fallback is
  mandatory: the original returns `Some` whenever `polylines` is non-empty.
- Dedup candidates with an epoch-stamped `Vec<u32>` over polylines, **not** a
  `BTreeSet` — this is the hot path.
- Recompute any latitude window **per query** from `rho`. A fixed constant is
  the near-pole bug.

- [ ] **Step 5: Run the equality test against the index**

Run: `cargo test -p hornvale-terrain --test channel_properties 2>&1 | tee /tmp/hv-t4.txt`
Expected: PASS, and Task 3's tie-break test still PASS.

- [ ] **Step 6: Prove the test can catch a wrong index**

The equality test passing proves nothing until it is shown capable of failing.
Perturb the *pruning bound* — the one thing the correctness argument rests on —
so that it can exclude a true winner, and watch the test go red with a named
position. Then revert, `touch`, and re-run. Report the red.

- [ ] **Step 7: Measure, then regenerate and diff**

```bash
uptime
/usr/bin/time -l cargo test --release -p hornvale-lab --test millrace_probe -- --ignored --nocapture
uptime
make gate
make rebaseline
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
git diff --stat book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/ docs/digest/ book/src/domesday/ clients/game/core/tests/fixtures/
```

**Decision rule:**
- Only `docs/audits/` moved → expected. Commit in this commit.
- `book/src/laboratory/` or `book/src/gallery/` moved → **STOP.** The index
  changed a world. Report the diff; do not rebaseline it away.

Report the effect as an **absolute CPU-s/world delta**, not a ratio (spec §2.1).

- [ ] **Step 8: Commit**

Message: `perf(terrain): index nearest_line without moving its tie-break`.

---

## Task 5: `channel-connectivity` — quantify, memoise, repair

The memo and the repair are **one commit**. The repair alone makes the metric
structurally worse: walks stop chaining at the first sub-threshold trunk today,
and after it they chain to the sea.

**Files:**
- Modify: `windows/lab/src/metrics.rs` (`lab_channel_connectivity` at `:7170`,
  its registered `doc:` literal at `:3473`)
- Modify: `windows/lab/tests/millrace_probe.rs`

- [ ] **Step 1: Quantify the vacuity (prediction P3)**

Extend the probe to count, seed 42 at level 6: total walks, and walks whose
**first** continuation test is already false. Report the fraction.

**Inherit no prior figure.** The Rill's review estimated ~3,500 of 3,606 and
correctly refused to assert it; that number is not evidence and must not appear
as a baseline.

- [ ] **Step 2: Memoise the walk over its shared suffixes**

Walks share suffixes — the loop ends each iteration with `line = trunk`
(`:7220`) — and the per-hop test depends only on the hop, never on the walk's
history. So intactness is a pure suffix property:

```
intact_from[line] = hop_ok(line) && intact_from[trunk_of(line)]
```

Memoise it. The downhill graph is acyclic (`:7183-7184`), so no visited set is
needed. This is exact by construction, not by tolerance.

**Prove it exact before changing the predicate:** with the memo in and the
shipped `WaterKind::River` test still in place, the metric's value must be
**bit-identical on every probe seed**. Assert equality against the unmemoised
result in the probe; a memo that silently re-walks or mis-shares would otherwise
be invisible.

- [ ] **Step 3: Repair the predicate**

Replace the `WaterKind::River` continuation test with one asking whether any run
**carries** the next cell. Two things to get right, and neither is optional:

- **`owner` here is a per-world local** from `lab_run_owner`
  (`metrics.rs:7075-7089`), while `ChannelNetwork`'s own inverse index is the
  **`trunk_vertex`** field (`channel.rs:458`, public at `:781`). They are **not
  identical**: `trunk_vertex` keeps the **first** claiming run (`:758`),
  `lab_run_owner` keeps the **last** (`:7084`). They agree only because the
  relation is asserted functional by R-4. If you substitute the published
  accessor for the per-world rebuild — which is worth doing — **say in the
  commit that it rests on R-4**, and add an assertion that the two agree on the
  probe seeds.
- **The drop-out arm goes structurally dead.** After the repair, `continues` and
  the `owner[last_cell]` lookup are the same expression, so
  `else { good = false }` (`:7197-7202`) is unreachable. **Re-express it, do not
  delete it** — it is the only signal that a walk fell out of the network.

- [ ] **Step 4: Resolve the column question by measurement**

Run both arms over `the-ford-probe`'s 64 seeds, same build.

**Decision rule (spec §6.3):**
- **Values differ on any seed** → ship the repaired metric as a **new,
  additively-named column**; leave `channel-connectivity`'s definition and
  values untouched so its `census_history` series stays interpretable. Precedent
  for the naming: `channel-band-monotonicity-untruncated` sits beside
  `channel-band-monotonicity` for exactly this reason.
- **Values identical on all 64** → repair **in place**. No column is added and
  no census value moves.

Report which branch fired, with the 64-seed evidence.

- [ ] **Step 5: Correct the published description**

Whichever branch fired, the `doc:` literal at `metrics.rs:3473-3520` still says
the measurement is about the joins. It flows into **three** committed artifacts:
`book/src/laboratory/generated/the-census/schema.json`,
`.../census-of-the-meeting/schema.json`, and `book/src/domesday/hydrology.md`.

**No test reads a doc literal.** After editing it the verification is
`git diff`, not a test run — and do not chain the edit with a test invocation in
one shell call, because the workspace's two-run guard rejects the entire command
and the edit silently does not happen.

- [ ] **Step 6: Measure, regenerate, diff, commit**

Same shape as Task 4 Step 7. Report the absolute CPU-s/world delta.

**Decision rule for the diff:** `book/src/laboratory/` moving is **expected**
here if and only if Step 4 took the differ branch, and then only in the new
column. Any other movement is a STOP.

---

## Task 6: Close

- [ ] **Step 1: Absorb main and re-gate**

`make preflight` from the branch. **It does not fetch** — `git fetch origin`
first, or the GO is only as fresh as your last fetch. On NO-GO, merge main into
the branch and re-run the gate there.

A conflict-free merge runs **no hook at all**, so generated artifacts can merge
wrong without conflicting. **Always `make rebaseline` after an absorption**;
never infer freshness from a clean merge.

- [ ] **Step 2: Census refresh — CONTROLLER ONLY, requires Nathan's authorization**

Push the branch, then dispatch with a **full SHA**, never a branch name:

```bash
bash scripts/census-run.sh status      # is the box already held?
ssh lefford 'cd ~/Projects/hornvale && HV_CENSUS_WORKTREE=canonical \
  HV_CENSUS_REF=<full-sha> scripts/census-run.sh'
```

Check `git worktree list` on lefford for a **prunable** worktree first — a
directory that is gone but still registered makes `census-run.sh` fail with a
`cd` error that does not name the cause.

Budget from `docs/timings.md`, not from memory. Commit the regenerated goldens
**on lefford**, then push and fast-forward locally.

- [ ] **Step 3: Prove the census diff**

```bash
make lab-diff STUDY=the-census
```

Additivity is proven by a **shared-column diff**, never by line counts: adding a
column rewrites all 1,000 rows, which looks alarming and means nothing. The
evidence is 0 diffs across the shared columns.

- [ ] **Step 4: Definition of Done**

- `book/src/chronicle/the-millrace.md` — the campaign's public account,
  including every falsified prediction as a finding.
- `docs/retrospectives/the-millrace.md` — process lessons. Promote the campaign
  ledger's survivors **before** the worktree is torn down; `.superpowers/sdd/`
  is git-ignored and dies with it.
- Book freshness sweep; re-score any Confidence Gradient bet this moved.
- Flip `TOOL-24`'s levers in `book/src/frontier/idea-registry.md` as they land,
  and add the three registry rows the spec's §8 names as non-goals.
- A `docs/timings.md` row for the refresh.
- Board `technique` posts for what was learned the hard way.

---

## Self-Review

**Spec coverage.** §3 keystone → Task 4. §3.3 P2 → Task 2. §3.4 oracle → Task 4
Steps 1-2. §4 L0 → Task 1. §5 contracts → Task 3. §6.1 P3 → Task 5 Step 1. §6.2
memo → Task 5 Step 2. §6.3 branch table → Task 5 Step 4. §6.4 both consequences
→ Task 5 Step 3. §7 acceptance 1/4/5 → Task 6. §2.1 absolute-deltas rule → Tasks
1, 4, 5 measurement steps. **Gap found and closed:** §7 acceptance 6 (report
P1/P2/P3 against their preregistered statements) had no owner; it is now Task 6
Step 4's chronicle requirement.

**Placeholders.** Two deliberate non-prescriptions, both required by
`campaign-autopilot`'s rule against prescribing mutations from outside the code:
Task 3 Step 3 and Task 4 Step 6 name the *property* a mutation must demonstrate
and leave the choice to the implementer. Task 1 Step 4 leaves the transect
test's name to be read rather than guessed. These are not gaps.

**Type consistency.** `BankReading::transverse()` is defined in Task 1 and used
only there. `nearest_line_reference` is introduced in Task 4 Step 1 and used in
Steps 2 and 4. `L_max` is computed in Task 2 (probe) and again in Task 4
(`build`) — deliberately, and Task 2's assertion is the tripwire on Task 4's.
`trunk_vertex` vs `lab_run_owner` is called out in Task 5 Step 3 precisely
because the brief conflated them.
