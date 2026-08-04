# The Tolerance Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give a species an interior. Author a **dispersion** beside each vector, draw a per-settlement disposition from it, and make warlikeness a derived, place-specific property instead of a species constant.

**Architecture:** A species row becomes a *location plus a spread*. One draw per settlement — not per individual — keyed on the settlement's committed `cell-id`, never its `EntityId`. Warlikeness stops being `threat_response > 0.6` and becomes `f(structural pressure, drawn disposition, grid/group quadrant)`.

**Tech Stack:** Rust 2024, `cargo nextest`, `serde`/`serde_json`/`libm` only.

## Global Constraints

- Dependencies: `serde`, `serde_json`, `libm` ONLY. No new crates.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only (`clippy.toml`). Float sorting via `total_cmp`.
- No wall-clock time. `#![warn(missing_docs)]` on every public item.
- **type-audit tags** exactly `bare-ok(<class>)` or `bare-ok(<class>: <field>)`.
- **Layering:** `domains/*` depends on `hornvale-kernel` and nothing else. Composition happens in `windows/worldgen`.
- `Seed` is a tuple struct `Seed(pub u64)` — `Seed::new` does not exist.
- Any `#[ignore]` reason containing `heavy:` must match verbatim:
  `"heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"`
- **`cargo fmt` as the final step before every commit.** Never `--no-verify`.
- Main fails the gate on a stale type-audit report.
- **Idea-registry cells are capped at 600 characters**, five columns, non-empty **Where**, slug IDs only.

## The three rules this campaign must not break

1. **Never key a draw on `EntityId`.** `Ledger::mint_entity` assigns sequentially and The Salt ratified that an id may be stored, compared and looked up but **never read for its value**. An id-keyed draw silently reshuffles every settlement's disposition when one earlier entity is inserted — deterministic, reproducible, catastrophic. **Also never key on position in `all_settlements()`**, which returns commit order and is the same trap wearing a different hat.
2. **A new stream label is a permanent save-format contract.** Declared as a `pub const` in the owning crate's `streams` module, published through `stream_labels()`, never renamed.
3. **H1/H2/H3 are reported, never asserted.** Encoding a preregistered prediction as a build failure creates pressure to retune until the suite goes green. Guard assertions only.

---

### Task 1: Measure the pre-dispersion baseline

H1 claims each people's *mean* behaviour survives. That is unfalsifiable without a before.

**Files:**
- Create: `windows/worldgen/tests/tolerance_baseline.rs`

**Interfaces:**
- Consumes: `hornvale_worldgen::{build_world, WorldComponents}`, `hornvale_settlement::all_settlements`.
- Produces: printed per-people raid rates and between-settlement variance, pasted into Task 6's comparison.

- [ ] **Step 1: Write the measurement harness**

Follow `windows/worldgen/tests/generalist_baseline.rs` — written in the previous campaign, it already has the correct world-building preamble (decision 0092's sanctioned fixture posture) and the same-population discipline. Read it first.

```rust
//! The Tolerance, Task 1: what raiding looks like BEFORE a species has an
//! interior. Every settlement of a people shares one disposition today, so
//! the between-settlement variance measured here should be ~0 by
//! construction — that zero is the baseline H2 must move.
#![allow(clippy::disallowed_methods)]

const SEEDS: std::ops::RangeInclusive<u64> = 1..=30;

#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn report_pre_dispersion_raid_rates() {
    // per people: raid rate, and variance of that rate BETWEEN settlements
}
```

Report, per people: the fraction of its settlements that raid, and the **between-settlement variance** of whatever the raid gate reads.

- [ ] **Step 2: Add the guard assertions**

These are required — a harness that measures nothing looks identical to one that works:

```rust
    assert!(!rows.is_empty(), "no settlements sampled");
    assert!(rows.iter().all(|r| r.is_finite()), "non-finite rate");
    assert_eq!(per_people.len(), 6, "all six peoples measured; got {:?}",
               per_people.keys().collect::<Vec<_>>());
```

- [ ] **Step 3: Run once, capture everything**

Run: `cargo nextest run -p hornvale-worldgen --test tolerance_baseline --run-ignored all --no-capture 2>&1 | tee /tmp/hv-tol-base.txt`

Grep the file afterwards. **Do not re-run to see a second line.**

**Expect between-settlement variance ≈ 0 for every people.** If it is not zero, something already varies per settlement and this campaign's premise needs re-examining — report that rather than proceeding.

- [ ] **Step 4: Commit**

```bash
cargo fmt
git add windows/worldgen/tests/tolerance_baseline.rs
git commit -m "test(tolerance): the pre-dispersion raid baseline"
```

---

### Task 2: Dispersion as an authored species trait

**Files:**
- Modify: `domains/species/src/lib.rs`
- Modify: `domains/species/tests/coverage.rs`

**Interfaces:**
- Produces: `pub struct Dispersion { pub mind: f64, pub society: f64, pub perception: f64 }` and `pub fn dispersion_registry() -> ComponentStore<KindId, Dispersion>`.

- [ ] **Step 1: Write the failing test**

In `domains/species/tests/coverage.rs`:

```rust
#[test]
fn every_kind_with_a_mind_carries_a_dispersion() {
    let disp = hornvale_species::dispersion_registry();
    for (k, _) in hornvale_species::psyche_registry().iter() {
        assert!(disp.contains(k), "minded kind {k:?} has no dispersion row");
    }
}

#[test]
fn dispersion_is_a_ratio_on_every_axis() {
    for (k, d) in hornvale_species::dispersion_registry().iter() {
        for (name, v) in [("mind", d.mind), ("society", d.society), ("perception", d.perception)] {
            assert!((0.0..=1.0).contains(&v), "{k:?}'s {name} dispersion {v} is not a ratio");
        }
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo nextest run -p hornvale-species --test coverage`
Expected: FAIL — `dispersion_registry` does not exist.

- [ ] **Step 3: Add the type, with D2's frame stated**

D2 requires the species value's meaning be stated rather than assumed — the bug The Manikin removed one level up. State it once, here:

```rust
/// How widely a species spreads around its authored vectors.
///
/// **The authored vector is the MEAN, and this is the standard deviation of a
/// population around it.** That choice is a fiat, not a discovery, and it is
/// stated because leaving it unstated is precisely the frame bug The Manikin
/// removed one level up: a datum whose frame is implicit drifts in meaning as
/// the model grows.
///
/// One dispersion per vector, not per dimension. A per-dimension spread is a
/// refinement that should be argued from a measured need (spec §8).
///
/// `0.0` means every member is identical — the model's behaviour before this
/// campaign, and the value that must collapse H2's variance to zero.
/// type-audit: bare-ok(ratio)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Dispersion {
    /// Spread around [`MindVector`].
    pub mind: f64,
    /// Spread around [`SocietyVector`].
    pub society: f64,
    /// Spread around [`PerceptionVector`].
    pub perception: f64,
}
```

- [ ] **Step 4: Author the rows, with the keystone argued**

The keystone is that **variability is itself a species trait** — a eusocial insect has near-zero behavioural variance where a generalist has high variance. Author accordingly and say why per row. Human is the widest; the goblinoids narrower; dragons (solitary, no society) narrow.

```rust
/// Per-kind dispersion. **Variability is itself a species trait** (spec §2's
/// keystone): a species is a distribution, and how wide that distribution is
/// says as much about the kind as where it is centred.
/// type-audit: bare-ok(identifier-text)
pub fn dispersion_registry() -> ComponentStore<KindId, Dispersion> {
```

Human takes the largest values — the campaign's own argument is that psychological breadth, not ecological breadth, is what "generalist" means.

- [ ] **Step 5: Run to verify it passes**

Run: `cargo nextest run -p hornvale-species && cargo run --manifest-path tools/type-audit/Cargo.toml -- check`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add domains/species/src/lib.rs domains/species/tests/coverage.rs
git commit -m "feat(species): dispersion — a species is a distribution, not a point"
```

---

### Task 3: The population draw, keyed on a stable semantic identity

**This is the campaign's highest-risk task.** Read rule 1 above before writing anything.

**Files:**
- Modify: `windows/worldgen/src/lib.rs`
- Modify: `windows/worldgen/src/streams.rs` (or the crate's streams module)
- Create: `windows/worldgen/tests/tolerance_draw.rs`

**Interfaces:**
- Consumes: `Dispersion`, `dispersion_registry()` from Task 2.
- Produces: `pub fn settlement_disposition(world: &World, settlement: EntityId) -> Option<MindVector>` — the people's mean, perturbed by a draw keyed on that settlement's committed `cell-id`.

- [ ] **Step 1: Write the id-independence test FIRST**

This test is the whole reason the task is risky. Write it before the implementation:

```rust
/// The Salt's rule, enforced: a settlement's disposition must not change when
/// an unrelated entity is minted before it. `Ledger::mint_entity` assigns
/// sequentially, so an `EntityId`-keyed draw would silently reshuffle every
/// settlement's psychology on any insertion — deterministic, reproducible,
/// and catastrophic.
#[test]
fn a_settlements_disposition_survives_an_earlier_entity_being_minted() {
    let w = build_world(Seed(42), /* ... */).unwrap();
    let s = hornvale_settlement::all_settlements(&w)[3].id;
    let before = settlement_disposition(&w, s).unwrap();

    let mut w2 = w.clone();
    let _unrelated = w2.ledger.mint_instance("owlbear", None, "test", &w2.registry).unwrap();
    let after = settlement_disposition(&w2, s).unwrap();

    assert_eq!(before, after,
        "the draw is keyed on entity identity, not on the settlement's own \
         cell — inserting one unrelated entity moved this settlement's mind");
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo nextest run -p hornvale-worldgen --test tolerance_draw`
Expected: FAIL — `settlement_disposition` does not exist.

- [ ] **Step 3: Declare the stream label**

A permanent save-format contract (rule 2):

```rust
/// The per-settlement disposition draw (The Tolerance). Keyed on the
/// settlement's committed `cell-id`, never its `EntityId`.
pub const SETTLEMENT_DISPOSITION: &str = "settlement/disposition/v1";
```

Add it to the crate's `stream_labels()` so it reaches the generated manifest, and **regenerate the manifest** — a `stream_labels()` addition without a manifest regen is a known recurring miss.

- [ ] **Step 4: Implement, keyed on `cell-id`**

`hornvale_settlement::CELL_ID` is a **committed fact** on the settlement entity. It is geometric, semantic, and stable under entity insertion — unlike the `EntityId` and unlike the settlement's index in `all_settlements()`, which is commit order.

```rust
/// A settlement's effective mind: its people's authored mean, perturbed by a
/// draw from that people's `Dispersion`.
///
/// **Keyed on the settlement's committed `cell-id`, never its `EntityId`.**
/// See `tolerance_draw.rs`'s id-independence test for why that distinction is
/// load-bearing rather than stylistic.
pub fn settlement_disposition(world: &World, settlement: EntityId) -> Option<MindVector> {
```

Read `cell-id` from the ledger, derive a `Stream` from `(world.seed, SETTLEMENT_DISPOSITION, cell_id)`, and perturb each dimension of the people's `MindVector` by the drawn offset scaled by `Dispersion::mind`, clamped to `[0, 1]`.

- [ ] **Step 5: Add the uniqueness guard**

Two settlements sharing a cell would draw identically. Assert they do not:

```rust
#[test]
fn no_two_settlements_share_a_cell_id() {
    // otherwise the draw key is not unique and two settlements are one
}
```

- [ ] **Step 6: Run to verify both pass**

Run: `cargo nextest run -p hornvale-worldgen --test tolerance_draw 2>&1 | tee /tmp/hv-tol-draw.txt`
Expected: PASS.

- [ ] **Step 7: Commit**

```bash
cargo fmt
git add windows/worldgen/src/ windows/worldgen/tests/tolerance_draw.rs book/src/reference/
git commit -m "feat(worldgen): the per-settlement disposition draw, keyed on cell-id"
```

---

### Task 4: Derived warlikeness, and grid/group

**Files:**
- Modify: `windows/worldgen/src/history_bake.rs`
- Modify: `domains/species/src/lib.rs` (the grid/group doc on `SocietyVector`)

**Interfaces:**
- Consumes: `settlement_disposition` from Task 3.
- Produces: warlikeness derived per settlement rather than read off a species constant.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn two_settlements_of_one_people_can_differ_in_raiding() {
    // the sorting phenomenon: with dispersion > 0, one people's settlements
    // must not all answer the same way
}
```

- [ ] **Step 2: Run to verify it fails**

Expected: FAIL — every settlement of a people currently answers identically.

- [ ] **Step 3: Replace the gate's input**

`RAID_DISPOSITION_MIN` currently gates on the species' `threat_response`. Per D5/D8, warlikeness becomes:

```
warlike = f(structural pressure, drawn disposition, grid/group quadrant)
```

The disposition comes from Task 3. **D8 dissolves the axis mismatch as a consequence**: `threat_response` is *defensive* (flee ↔ stand) and raiding is *proactive*, so the gate stops borrowing a defensive axis rather than being patched.

**Update `RAID_DISPOSITION_MIN`'s disclosure** — it currently says "exactly one of the four settling peoples declines to raid", which is stale twice over (it is six peoples, and raiding is no longer a per-species flag).

- [ ] **Step 4: Adopt grid/group explicitly (D6)**

In `SocietyVector`'s doc:

```rust
/// **This is a grid/group instrument** (Douglas), adopted deliberately at the
/// owner's direction: `sociality` is *grid* (how rule-bound a life is) and
/// `in_group_radius` is *group* (how bounded "us" is). The four biases —
/// hierarchy, egalitarian/sect, individualist, fatalist — each carry published
/// predictions about cosmology, risk, and stance toward outsiders, so those are
/// DERIVED from the quadrant rather than authored per people. Adding a people
/// means placing it on two axes, not inventing its culture.
```

- [ ] **Step 5: Run to verify it passes**

Run: `cargo nextest run -p hornvale-worldgen 2>&1 | tee /tmp/hv-tol-t4.txt`

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add -A
git commit -m "feat(worldgen): warlikeness is derived per settlement, not per species"
```

---

### Task 5: The mutation proof

The program's shared acceptance criterion. A green test proves the code ran; only the mutation proves the axis is visible.

**Files:**
- Create: `windows/worldgen/tests/tolerance_mutation.rs`

- [ ] **Step 1: Write the mutation test**

```rust
/// Setting a people's dispersion to zero MUST collapse its between-settlement
/// variance to zero. If it does not, the dispersion parameter is not being
/// read and every H2 result is an artifact of something else.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn zero_dispersion_collapses_between_settlement_variance() {
    let real = between_settlement_variance(KindId("human"), None);
    let zeroed = between_settlement_variance(KindId("human"), Some(Dispersion {
        mind: 0.0, society: 0.0, perception: 0.0,
    }));
    assert!(real > VARIANCE_FLOOR, "authored dispersion produces no variance: {real}");
    assert!(zeroed < 1e-12, "zeroing dispersion did NOT collapse variance ({zeroed}) — \
                             the parameter is not being read, so H2 proves nothing");
}
```

- [ ] **Step 2: Run and confirm BOTH assertions hold**

Run: `cargo nextest run -p hornvale-worldgen --test tolerance_mutation --run-ignored all --no-capture 2>&1 | tee /tmp/hv-tol-mut.txt`

**Report both measured numbers.** A green test whose two cases produce similar numbers is a false pass.

**Choose `VARIANCE_FLOOR` from the two measured cases and disclose that you did**, stating both values, the seed range and the population in its doc. A constant calibrated against a measurement must say so.

- [ ] **Step 3: Commit**

```bash
cargo fmt
git add windows/worldgen/tests/tolerance_mutation.rs
git commit -m "test(tolerance): the mutation proof — zero dispersion collapses variance"
```

---

### Task 6: The preregistered readout

**Files:**
- Modify: `windows/worldgen/tests/tolerance_baseline.rs`

- [ ] **Step 1: Add the H1/H2/H3 readout**

Over seeds 1..=30, reported and **never asserted**:

- **H1** — each people's *mean* behaviour matches Task 1's pre-dispersion baseline. A moved mean means the draw is biased.
- **H2** — between-settlement variance is high for high-dispersion peoples and near-zero for low-dispersion ones.
- **H3** — humans raid at a rate strictly between goblin's and hobgoblin's, rather than at 0 or 1.

Raise the guard's peoples count if it changed, and keep the guards.

- [ ] **Step 2: Run once, capture everything**

Run: `cargo nextest run -p hornvale-worldgen --test tolerance_baseline --run-ignored all --no-capture 2>&1 | tee /tmp/hv-tol-readout.txt`

- [ ] **Step 3: Record the result whichever way it came out**

If H1 fails, the draw is biased and that is a defect. **If H2 fails — no measured outcome moves once dispersion is authored — the layer is decorative and should not ship.** That is spec §6's own falsification and rung 2 of the ladder. Report it; do not retune to rescue it.

- [ ] **Step 4: Commit**

```bash
cargo fmt
git add windows/worldgen/tests/tolerance_baseline.rs
git commit -m "test(tolerance): the preregistered readout"
```

---

### Task 7: Artifacts, epoch, book, close

- [ ] **Step 1: Regenerate and inspect**

Run: `make rebaseline`, then diff `book/src/gallery/`, `book/src/reference/`, `book/src/laboratory/`, `docs/audits/`. The type-audit report is the commonly missed one, and main now fails the gate on a stale one.

- [ ] **Step 2: Declare the epoch on evidence**

Unlike The Generalist, this campaign **adds a stream label and consumes new draws**, so a derivation almost certainly did move — but decision 0084 still says declare only when the evidence shows it. Check `*/streams.rs`, `kernel/src/seed.rs`, `kernel/src/noise.rs` and the artifact diff. Stamp per 0089 if warranted.

- [ ] **Step 3: Census regen — CARVE-OUT**

Adding a stream label moves every world with settlements. **Ask Nathan for explicit authorization**, then run `bash scripts/census-run.sh` on lefford (0079/0086/0081) — never `HV_CENSUS=1 regenerate-artifacts.sh`. Refresh **both** fixtures (31 `the-census` + 3 `census-of-the-meeting`).

Expect calibration witnesses to drift. **Re-pin witnesses, never claims** — for each, state what the claim is, verify it survives, and say how. `tools/census/queries/calibrate/golden-pins.sql` is a fourth file a census re-pin touches and the pre-commit hook checks it.

- [ ] **Step 4: Book, chronicle, retrospective**

Book chapters naming the species vectors need the grid/group adoption. **The chronicle may not carry registry IDs** outside the Frontier part — name the concept.

Record the honest headline whichever way H2 came out.

- [ ] **Step 5: Registry bookkeeping**

Flip `PSY-dispersion` to shipped; note what `CUL-belief-hysteresis` still owes (the slow feedback edges — the doctrine ratchet is the payoff and is deliberately not in this campaign); confirm `SOC-war-variance` still names its blocker.

- [ ] **Step 6: The full gate**

Run: `make gate 2>&1 | tee /tmp/hv-tol-gate.txt; echo "EXIT=$?"`

**Check the exit code explicitly.** Then `make census-check` and `shellcheck` on any changed script — neither runs inside `make gate`.

---

## Self-review

**Spec coverage.** D1 → Task 2. D2 → Task 2 Step 3. D3 → Task 3 (the whole task). D4 → Task 3. D5 → Task 4. D6 → Task 4 Step 4. D7 (deferred edges) → Task 7 Step 5, recorded not built. D8 → Task 4 Step 3. H1/H2/H3 → Tasks 1 and 6. The mutation → Task 5. §9's flagged items → Task 7.

**H4 is absent by design** — withdrawn at G3 because supply has no time parameter and The Mire's trajectory is periodic. Filed against `SOC-war-variance`.

**Placeholders.** Task 2's authored values and Task 4's exact gate formula are deliberately left to the implementer with their arguments specified, because inventing numbers here without measuring is the failure D3 of the previous campaign existed to prevent. Every other step carries its content.

**Type consistency.** `Dispersion { mind, society, perception }`, `dispersion_registry()`, `settlement_disposition()`, `SETTLEMENT_DISPOSITION` are used with identical names throughout.

**One risk this plan cannot remove:** Task 3 is where the campaign can go silently, catastrophically wrong, and its id-independence test is written *before* the implementation for exactly that reason.
