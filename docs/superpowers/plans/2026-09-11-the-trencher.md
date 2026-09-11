# The Trencher Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the vocabulary creatures eat with as fine as the vocabulary the world grows with, then populate the underworld with things that use the difference.

**Architecture:** Four stages in a forced order. Stage 1 splits `TrophicMode` into its three real axes, which is what makes the fantasy tier a *value* rather than a new mechanism. Stage 2 replaces the mean-of-seven with per-metabolite sums and moves two non-foods out of the food vocabulary. Stage 3 builds the metaphysics gate that does not yet exist and puts `thaumic` behind it. Stage 4 authors the biota.

**Tech Stack:** Rust 2024, `cargo nextest`, no new dependencies.

**Spec:** `docs/superpowers/specs/2026-09-11-the-trencher-design.md`

**Ledger:** `docs/superpowers/ledgers/2026-09-11-the-trencher.md` — rulings go there **as they occur**, never to `.superpowers/sdd/`.

## Global Constraints

- **No new crates.** Allowlist is `serde`, `serde_json`, `libm` (`ALLOWED_EXTERNAL` in `cli/tests/architecture.rs`).
- **No `HashMap`/`HashSet`.** `BTreeMap`/`BTreeSet`/`Vec` only; float sort is `total_cmp`.
- **Every public item, field and variant gets a doc comment** (`#![warn(missing_docs)]`).
- **`cargo fmt` is the last step before every commit.** `make gate-commit` must pass.
- **Layering is constitutional:** `kernel/` → `domains/*` → `windows/*` → `cli/`. A domain may not depend on a window. This is why the food vocabulary lives in the kernel and `EnergySource` does not.
- **`ResourceAxis` ids are append-only.** ids 0-6 are taken. **Never renumber one** — the id is the key a `ResourceVector` stores a weight under.
- **Any file that builds a world needs a row in `cli/tests/fixtures/world-build-sites.tsv`** (decision 0606), or `world_build_sites::no_unrostered_world_build_appears` REDs. The count is a per-file ratchet on world-build CALL SITES, not invocations.
- **A falsified prediction is a finding, never a failure** (decision 0016). **Never retune a constant, a curve, or an authored value to rescue a prediction.** Record the measured value with today's date and assert on what was measured.
- **This campaign authors no marine kind, changes no `MARINE_FORAGE` behaviour, and gives no `Surface`-realm kind a metabolite weight** — The Tidemark's boundary.
- **`CHEMOSYNTHATE` (id 6) is not removed, renumbered, or repurposed** — a live peer campaign weights it (ledger #1).

---

## File Structure

| file | responsibility |
|---|---|
| `kernel/src/ecology.rs` | **Modify.** The metabolite `ResourceAxis` members. Append-only; ids from 7 up. |
| `domains/species/src/lib.rs` | **Modify.** The trichotomy types replacing `TrophicMode`; the 39 kinds' migration; Stage 4's new organisms. The single largest surface, and it holds both the type and its instances. |
| `domains/species/tests/suite/metabolic_pairs.rs` | **Modify.** The sanctioned-pair table becomes a sanctioned-*triple* table. |
| `domains/terrain/src/pins.rs` | **Modify.** The metaphysics pin, following `TerrainPins`' all-`Option` `Default` shape. |
| `domains/terrain/src/lithology.rs` | **Modify.** `thaumic`'s derivation, behind the pin. |
| `windows/worldgen/src/energy.rs` | **Modify.** Per-metabolite supply; the sum; `Geothermal` as modifier. |
| `windows/worldgen/src/lib.rs` | **Modify.** The per-metabolite fields threaded into `score_at`'s `per_axis`. |
| `windows/lot/src/slots.rs` | **Modify.** The one non-species production consumer of `TrophicMode`. |
| `windows/worldgen/tests/suite/trencher_probe.rs` | **Create.** T1, T2, T3, T4. |

---

## Stage 0 — A defect on `main`, ahead of the refactor

### Task 0: `xorn` is alive — move it off `Absent`

**Files:**
- Modify: `domains/species/src/lib.rs` (`xorn`'s `thermal_strategy`; `ThermalStrategy::Absent`'s and `Unmodelled`'s docs)
- Modify: `domains/species/tests/suite/coverage.rs` (the witness table)
- Modify: `domains/species/tests/suite/metabolic_pairs.rs` (if `SANCTIONED` names the pair)

**Governing decision: 0976, "Ametabolic life is a category error."** Read it
first; it is this task's authority and it was ruled today.

**Why this runs BEFORE Stage 1.** The split would otherwise copy an ambiguous
token onto three axes. Fixing meaning once is cheaper than fixing it three
times.

- [ ] **Step 1: Confirm the state for yourself**

```bash
grep -rn 'ThermalStrategy::Absent' --include='*.rs' domains/ windows/
sed -n '50,80p' domains/species/src/allometry.rs
```

Measured before this plan: `ThermalStrategy::Absent` has **one** carrier
(`xorn`); `allometry.rs` returns basal rate `0.0` for it and nulls the
biological traits, so a carrier has no lifespan; `coverage.rs` registers
`xorn` as its `Rung::Witnessed` witness. `xorn` is simultaneously
`TrophicMode::Chemotrophic` with a `CHEMOSYNTHATE` weight of `0.35`.

- [ ] **Step 2: Write the failing test — `Absent` is uninhabited**

```rust
/// Decision 0976: a living kind always has a metabolism, so
/// `ThermalStrategy::Absent` is for things that are not alive. Nothing in the
/// shipped roster is such a thing, so the variant is `Declared`, not
/// `Witnessed`.
///
/// **This going RED because someone added a carrier is the point.** The fix is
/// not to delete this test — it is to ask whether the new kind is alive. If it
/// is, it wants `Unmodelled` or a real thermal strategy; if it is not, it
/// wants its own treatment rather than a `BiosphereTraits` row with the life
/// nulled out.
#[test]
fn no_living_kind_is_ametabolic() {
    let carriers = thermal_witnesses(ThermalStrategy::Absent);
    assert!(
        carriers.is_empty(),
        "ThermalStrategy::Absent is carried by {carriers:?}, but decision 0976          reserves it for kinds that are not alive. A creature with a trophic          mode has a metabolism."
    );
}
```

- [ ] **Step 3: Run it and watch it fail naming `xorn`**

Run: `cargo nextest run -p hornvale-species --test suite -E 'test(no_living_kind_is_ametabolic)'`
Expected: FAIL, listing `["xorn"]`.

- [ ] **Step 4: Move `xorn` to `Unmodelled`, and say why at the row**

`Unmodelled` is "Has a metabolism; its thermal behaviour is not modelled" —
exactly `xorn`'s situation. **Do not assign `Ectothermic`**, however plausible
a rock-dweller at cave temperature sounds: that is a modelling call nobody has
made, and making it silently inside a vocabulary refactor is what 0976 §"Why
`Unmodelled` rather than `Ectothermic`" forbids.

- [ ] **Step 5: Update both docs**

`ThermalStrategy::Absent` — re-gloss for a **thermal** axis, citing 0976, and
say it is reserved-and-uninhabited rather than retired. `Unmodelled` — its doc
is written entirely about the autotroph physics problem and now holds **two**
debts; say so, or the next reader will think the chemolithotroph case was an
accident.

- [ ] **Step 6: Update the coverage witness table**

`coverage.rs:143` registers `(ThermalStrategy::Absent, Rung::Witnessed,
&["xorn"])`. It becomes `Rung::Declared` with no witnesses. **A demotion here
is a correct outcome, not a coverage regression** — read the file's own doc on
what the rungs mean before editing.

- [ ] **Step 7: Rebaseline, and READ the diff**

```bash
make rebaseline
git diff --stat -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$' | cut -f1)
```

**Worlds SHOULD move here** — `xorn` gains a basal metabolic rate where it had
`0.0` and a lifespan where it had none. Confirm what moved is
demography/capacity-dependent and consistent with one kind gaining a
metabolism. **If NOTHING moves, that is the finding**: it would mean `xorn`'s
thermal strategy reaches no world number, and the campaign should know that
before Stage 1 builds on the same machinery.

- [ ] **Step 8: `cargo fmt`, gate, commit**

```bash
cargo fmt && make gate-commit
git add domains/species/src/lib.rs domains/species/tests/suite/coverage.rs docs/superpowers/ledgers/2026-09-11-the-trencher.md $(grep -v '^#' docs/generated-paths.txt | grep -v '^$' | cut -f1)
git commit -m "fix(the-trencher): xorn is alive -- decision 0976"
```

---

### Task 0b: a drive that cannot rise must not be pursued

**Files:**
- Modify: `windows/vessel/src/liveness.rs` (the `active` predicate, ~line 6186)
- Test: `windows/vessel/src/liveness.rs`'s own test module
- Modify: `windows/lab/tests/fixtures/affect-trace-seed-42.txt` (rebaseline)

**Root cause, already traced — do not re-derive it.** `arbitrate`'s activation
predicate reads:

```rust
if d.seek_while_asleep() {
    !awake || normally        // urgency is NEVER consulted on this arm
} else if awake {
    normally
} else {
    d.survival_override(u)
}
```

Fatigue is the only drive with `seek_while_asleep() == true`, "because it is
the drive that carries a creature INTO sleep". The rule's **unstated
assumption** is that a creature entering the off-phase has accrued fatigue.
When urgency is `0.0`, the drive engages anyway, **nothing can reduce it
below zero**, so the blocked branch fires and labels the creature
`Frustrated` with `valence: -1.0`.

Measured: 18 of `xorn`'s 40 ticks in the committed affect golden.

- [ ] **Step 1: Write the GENERAL failing test FIRST — no xorn in it**

The hypothesis worth testing is that **this was never xorn-specific**. A
normal creature (rise rate `0.3`) that sleeps to *full* rest while the
off-phase is still running has fatigue `0.0` and should hit the identical
path.

Write that test before the xorn one: a fully-rested creature, in the
off-phase, must not be `Frustrated` about `Fatigue`. **If it goes red, the
defect was always there** and `xorn` merely made it permanent instead of
brief. Report which.

- [ ] **Step 2: Write the xorn test — the permanent case**

Rise rate `0.0`, off-phase. Same assertion. This is the witness that stays
live: Nathan ruled `xorn` remains genuinely sleepless, so this test keeps a
real carrier rather than a synthetic one.

- [ ] **Step 3: Run both and confirm they fail for the RIGHT reason**

Expected: `Frustrated` with `object == Some(DriveKind::Fatigue)`. **If a test
fails some other way, stop** — it is not reproducing the traced mechanism.

- [ ] **Step 4: Fix the predicate**

```rust
if d.seek_while_asleep() {
    (!awake && u > 0.0) || normally
}
```

The exact float predicate is yours — any nonzero fatigue means there is
something to reduce, so `> 0.0` is likely right, but say why in a comment.

**CHECK THIS BEFORE COMMITTING, it is the risk the fix carries:** does a
creature need the fatigue drive *active* to STAY asleep? If the drive going
inactive at full rest wakes it mid-off-phase, this fix trades one defect for
another. Read how sleep is sustained and say what you found. **If it does
wake them, STOP and report** — the fix then needs a different shape and that
is a ruling, not an implementation choice.

- [ ] **Step 5: Run both tests, then the vessel suite**

- [ ] **Step 6: Rebaseline and read the diff**

`xorn` should lose its `Fatigue`-objected ticks and keep `Danger`/`Social`.
Confirm no other creature's block moved — **unless Step 1 went red**, in
which case other creatures SHOULD move, and that is the finding.

- [ ] **Step 7: `cargo fmt`, gate, commit**

```bash
cargo fmt && make gate-commit
git add windows/vessel/src/liveness.rs windows/lab/tests/fixtures/affect-trace-seed-42.txt docs/superpowers/ledgers/2026-09-11-the-trencher.md
git commit -m "fix(the-trencher): a drive that cannot rise is not pursued"
```


## Stage 1 — The trichotomy

### Task 1: The three axes and their sanctioned combinations

**Files:**
- Modify: `domains/species/src/lib.rs` (the `TrophicMode` definition, ~line 2628)
- Modify: `domains/species/tests/suite/metabolic_pairs.rs`

**Interfaces:**
- Produces: three enums replacing `TrophicMode`, and a sanctioned-combination table. **Write the exact type and variant names you chose into the ledger** — Tasks 2, 4 and 9-12 all consume them and must not guess.

- [ ] **Step 1: Read the row and the code before designing anything**

Read `BIO-trophic-trichotomy` in `book/src/frontier/idea-registry.md` — it is the design authority for this task and it is Nathan's own. Then read `TrophicMode`'s definition and every one of its variants' doc comments in `domains/species/src/lib.rs`, and `metabolic_pairs.rs`'s `SANCTIONED` table in full.

The factorisation is **energy source × electron donor × carbon source**. Today's four flattened values each map to a point in that space. **Deriving which point is a reading of the existing variants' docs, not a decision this plan makes for you.**

**`Absent` is decided, and the rule is SPLIT THE TOKEN.** An ideonomy pass
(ledger #4) found that `Absent` currently carries three different claims, and
that every other domain which made this mistake fixed it the same way — SQL's
single `NULL` against Codd's two markers, HL7/FHIR's separate
`unknown`/`not-asked`/`not-applicable` codes, survey methodology's distinct
missing-data codes. **None of them fixed it by choosing which meaning wins.**

```
  kind of absence   what it claims              direction over time
  ---------------   -------------------------   -------------------
  ontological       the organism has none       steady - permanent
  inapplicable      the axis does not apply     steady - structural
  epistemic         nobody has decided yet      DECAYING - it is a debt
```

Three consequences, and they are requirements:

1. **`Absent` does NOT become a value on each of the three axes.** Copying it
   three times triples the ambiguity instead of resolving it.
2. **Ontological absence sits OUTSIDE the triple, not inside it.** A construct
   has no energy source, no electron donor and no carbon source — that is one
   claim about the organism, not three coincidences. `TrophicMode::Absent` has
   **zero carriers** today, so nothing is displaced by moving it out.
3. **`Unmodelled` already IS the epistemic case and is careful about it** —
   "Has a metabolism; its thermal behaviour is not modelled", written after
   The Gossan's split. Do not reinvent it on the new axes; its population is
   supposed to shrink, which is a different lifecycle from the other two.

- [ ] **Step 2: Write the failing test — every shipped kind occupies a sanctioned combination**

This replaces `every_kind_carries_a_sanctioned_pair`. Same shape, three axes:

```rust
#[test]
fn every_kind_carries_a_sanctioned_combination() {
    let bio = hornvale_species::biosphere_registry();
    let mut offenders = Vec::new();
    for (id, traits) in bio.iter() {
        let combo = (traits.energy_source, traits.electron_donor, traits.carbon_source);
        if !SANCTIONED.contains(&combo) {
            offenders.push(format!("  {id:?}: {combo:?}"));
        }
    }
    assert!(
        offenders.is_empty(),
        "kinds carrying an unsanctioned metabolic combination:\n{}\n\n\
         Adding a combination to SANCTIONED is a deliberate act — it asserts \
         the combination is biologically meaningful, not merely typeable.",
        offenders.join("\n")
    );
}
```

Field names above are illustrative of the SHAPE. Use whatever you named them in Step 1, and keep them consistent from here on.

- [ ] **Step 3: Run it and confirm it fails to compile**

Run: `cargo nextest run -p hornvale-species --test suite -E 'test(sanctioned_combination)'`
Expected: FAIL — the fields do not exist.

- [ ] **Step 4: Define the three enums, and keep the count assertion**

`metabolic_pairs.rs` currently asserts a *count* of sanctioned pairs, with a doc explaining that a silent widening is the hazard. **Preserve that guard in its three-axis form.** Its own doc records that adding `(Unmodelled, Chemotrophic)` and flipping `treant` was caught only by the count — that is the defect the guard exists for and it must survive the split.

- [ ] **Step 5: Run the test and confirm it passes; then run the whole species suite**

Run: `cargo nextest run -p hornvale-species --test suite`
Expected: the new test PASSES. Other tests referencing `TrophicMode` will fail to compile — that is Task 2's work, and **the compiler is your enumeration**. Do not hunt for sites by grep.

- [ ] **Step 6: Write the chosen names into the ledger, then `cargo fmt`, gate, commit**

```bash
cargo fmt && make gate-commit
git add domains/species/src/lib.rs domains/species/tests/suite/metabolic_pairs.rs docs/superpowers/ledgers/2026-09-11-the-trencher.md
git commit -m "feat(the-trencher): TrophicMode becomes three axes"
```

---

### Task 2: Migrate the kinds and consumers — SHARES TASK 1'S COMMIT

**RULING (ledger #13): Tasks 1 and 2 land in ONE commit.** They were never
separately committable and the plan was wrong to imply it: removing
`TrophicMode` breaks its consumers, `make gate-commit` is workspace-wide, and
so **no green commit exists between these two tasks.** A task is the smallest
unit that carries its own test cycle, and "define the type" cannot carry one
here.

Leaving `TrophicMode` behind as a deprecated alias was considered and rejected
— it buys a commit boundary at the price of a temporary alias, and a
deprecated alias is exactly the kind of thing that outlives its deprecation.

**The consumer surface is FOUR production files, not three.** An earlier
measurement in this plan said `windows/sentiment`'s mentions were all inside
`#[cfg(test)]`. That is true of `axes.rs` and **false of `lib.rs`**, which
carries `pub trophic_mode: TrophicMode` at line 71 — a production field on
`PeopleTraits`, copied in production at line 141, with `#[cfg(test)]` not
starting until 175. The measurement checked one file and inferred its sibling.

**Files:**
- Modify: `domains/species/src/lib.rs` (the 39 `trophic_mode:` sites)
- Modify: `windows/lot/src/slots.rs` (~line 1620)
- Modify: `windows/worldgen/src/lib.rs`
- Modify: `windows/sentiment/src/axes.rs`, `windows/sentiment/src/lib.rs` (test fixtures only)

**Interfaces:**
- Consumes: Task 1's three enums, by the names recorded in the ledger.

- [ ] **Step 1: Let the compiler enumerate**

```bash
cargo check --workspace --all-targets 2>&1 | tee /tmp/trencher-sites.txt
grep -c '^error' /tmp/trencher-sites.txt
```

**Measured before this plan was written:** 47 construction sites, 49 match arms, across 3 production files plus `sentiment`'s test fixtures. If your error count implies a materially different surface, **stop and report it** — the plan's stage sizing rests on that measurement.

- [ ] **Step 2: Migrate each kind to its combination**

Every kind's current `trophic_mode` maps to exactly one point under Task 1's mapping. **This is mechanical; it is not a re-authoring.** If any kind's correct combination is genuinely ambiguous, do not guess — record it in the ledger, pick the reading that preserves today's behaviour, and say so in a comment at that kind.

- [ ] **Step 3: Fix the three production consumers**

`windows/lot/src/slots.rs` matches on `TrophicMode::Chemotrophic`. Read what it does with it and preserve the behaviour — a `Chemotrophic` kind under the old enum is a specific combination under the new one, and the match must select the same kinds it selected before.

- [ ] **Step 4: Prove nothing moved**

```bash
make rebaseline
git diff --stat -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$' | cut -f1)
```

**Expected: an EMPTY diff.** Stage 1 is a pure retyping — the same kinds with the same properties under different field names. **If any artifact moves, the migration changed a kind's meaning**, which is a defect, not a rebaseline. Find it before committing.

- [ ] **Step 5: `cargo fmt`, gate, commit**

```bash
cargo fmt && make gate-commit
git add domains/species/src/lib.rs windows/lot/src/slots.rs windows/worldgen/src/lib.rs windows/sentiment/src/axes.rs windows/sentiment/src/lib.rs
git commit -m "refactor(the-trencher): migrate 39 kinds and three consumers to the trichotomy"
```

- [ ] **Step 6: Stage gate**

```bash
make sluice-stage BRANCH=campaign/the-trencher REF=$(git rev-parse HEAD)
```

---

## Stage 2 — The food vocabulary and the reduction

### Task 3: The metabolite axes

**Files:**
- Modify: `kernel/src/ecology.rs`

**Interfaces:**
- Produces: new `pub const` `ResourceAxis` members, ids from **7** upward. Record the names and ids in the ledger; Tasks 4 and 9-12 consume them.

- [ ] **Step 1: Read the existing seven and their `ResourceKind`**

`kernel/src/ecology.rs:48-125`. Note that `PHOTOSYNTHATE` and `CHEMOSYNTHATE` are `ResourceKind::Field` (ambient, undepletable) and the rest are `Stock` (depletable, drives the trophic cap). **Your new axes' `kind` is a real decision** — a chemical metabolite produced continuously by rock chemistry is arguably `Field`; a standing mat of it is `Stock`. Argue it in the doc comment.

- [ ] **Step 2: Write the failing test**

```rust
#[test]
fn the_metabolite_axes_are_registered_and_append_only() {
    use hornvale_kernel::*;
    let ids: Vec<u16> = [/* your new axes */].iter().map(|a| a.id).collect();
    assert!(ids.iter().all(|&i| i >= 7), "metabolite ids must not collide with 0-6: {ids:?}");
    let mut sorted = ids.clone();
    sorted.sort_unstable();
    sorted.dedup();
    assert_eq!(sorted.len(), ids.len(), "duplicate axis id among the metabolites: {ids:?}");
    assert_eq!(CHEMOSYNTHATE.id, 6, "CHEMOSYNTHATE must keep id 6 — a live peer campaign weights it");
}
```

- [ ] **Step 3: Run it, watch it fail, add the axes, run it again**

Run: `cargo nextest run -p hornvale-kernel -E 'test(metabolite_axes)'`

- [ ] **Step 4: `cargo fmt`, gate, commit**

```bash
cargo fmt && make gate-commit
git add kernel/src/ecology.rs docs/superpowers/ledgers/2026-09-11-the-trencher.md
git commit -m "feat(the-trencher): the metabolite resource axes"
```

---

### Task 4: Per-metabolite supply, as a sum

**Files:**
- Modify: `windows/worldgen/src/energy.rs`
- Modify: `windows/worldgen/src/lib.rs` (`score_at`'s `per_axis`, ~line 2390)

**Interfaces:**
- Consumes: Task 3's axes.
- Produces: a per-metabolite supply reading, consumed by `score_at`.

- [ ] **Step 1: Read `subterranean_energy`'s doc in full before changing it**

`windows/worldgen/src/energy.rs:430-461`. It records *why* the mean exists — a clamped sum pinned every rung's median to 1.0. **You are not undoing a mistake; you are removing the reason the mistake was necessary.** Summing *within a metabolite* is a much narrower sum than summing across all seven, which is why it need not saturate. Say that in the new doc.

- [ ] **Step 2: Author the reaction→metabolite mapping, with its argument**

Two reactions both yielding H₂ feed one axis. `DetritalImport` routes to the existing `DETRITUS` axis. `Geothermal` **leaves the food vocabulary** and becomes a modifier on the chemical supplies — its form is yours, argued in the code (spec §4.2, ledger #2).

- [ ] **Step 3: Thread the metabolites into `score_at`**

`per_axis` in `windows/worldgen/src/lib.rs` is a fixed array of `(ResourceAxis, f64)` pairs. Add the metabolites. **`CHEMOSYNTHATE` stays in the array**, fed by whatever aggregate rule you choose, and its rule is a decision to state in the ledger — a generalist must still be able to eat "chemical food" generically.

- [ ] **Step 4: Prove `xorn` still places**

`xorn` weights `CHEMOSYNTHATE` at 0.35 and is the only existing chemotroph. Run the world-build suite and confirm it still has capacity. **A change that silently zeroes the one existing chemotroph is the failure to watch for here.**

- [ ] **Step 5: `make rebaseline`, and READ the diff**

Worlds SHOULD move — the supply changed. Confirm what moved is placement-dependent and not something unrelated.

- [ ] **Step 6: `cargo fmt`, gate, commit**

---

### Task 5: T1 — does the sum lift the ceiling?

**Files:**
- Create: `windows/worldgen/tests/suite/trencher_probe.rs`
- Modify: `windows/worldgen/tests/suite.rs` (register with `#[path = "suite/trencher_probe.rs"] mod trencher_probe;` — see its line 224 for the pattern; **there is no `tests/suite/mod.rs`**)
- Modify: `cli/tests/fixtures/world-build-sites.tsv` (**required** — a new world-building file)

- [ ] **Step 1: Write T1 as the spec freezes it**

```
PREDICTION: at least one rung realizes `fed` at >= 25%, AND the realized
            maximum exceeds 0.5.
BASELINE:   max 0.424277; `rich` and `teeming` never realized at any rung
            in any of twelve seeds.
```

Report the full corpus-band occupancy table and the realized maximum per rung **before** asserting. Reuse `subterranean_energy_probe.rs`'s twelve-seed idiom (`Q6_SEEDS`, `world_at`) and cite it.

- [ ] **Step 2: Run it and record the measurement with today's date in the test's doc comment**

- [ ] **Step 3: If the prediction fails, do NOT retune**

The spec names the remaining candidate: the saturating `yield_at` arms themselves. Record that as the finding, flip the assertion to pin the measured result, ledger it, and **report it as the headline** — it redirects Stage 4's expectations and possibly the successor's whole subject.

- [ ] **Step 4: `cargo fmt`, gate, commit, then stage gate**

```bash
make sluice-stage BRANCH=campaign/the-trencher REF=$(git rev-parse HEAD)
```

---

## Stage 3 — `thaumic`

### Task 6: The metaphysics pin

**Files:**
- Modify: `domains/terrain/src/pins.rs`

**This gate does not exist today** — verified by grep across `kernel/`, `domains/`, `windows/`, `cli/`. You are building it.

- [ ] **Step 1: Follow `TerrainPins`' shape exactly**

`domains/terrain/src/pins.rs:8-27` is `#[derive(Debug, Clone, Copy, PartialEq, Default)]` with every field an `Option`. **Default `None` means inert**, which is what makes an unpinned world byte-identical.

- [ ] **Step 2: Write the failing test — default is inert**

```rust
#[test]
fn the_default_world_is_metaphysically_inert() {
    let pins = TerrainPins::default();
    assert!(pins.metaphysics.is_none(), "an unpinned world must be inert");
}
```

- [ ] **Step 3: Implement, run, `cargo fmt`, gate, commit**

---

### Task 7: `thaumic`'s derivation, behind the pin

**Files:**
- Modify: `domains/terrain/src/lithology.rs`
- Modify: the 10 sites that set `thaumic: 0.0`

- [ ] **Step 1: Find the ten sites**

```bash
grep -rn 'thaumic:' --include='*.rs' domains/ windows/
```

Measured: 10 sites, every one hardcoded `0.0`, none via `Default`.

- [ ] **Step 2: Derive it, and let it be zero when inert**

The derivation's inputs are yours to choose and argue — `MAP-40`/`MAP-53` suggest faults, hotspots and deep-time cataclysms as the physical hooks, and The Ground's §8 names "ley-lines from faults, mana-wells from hotspots, hallowed/cursed ground from deep-time cataclysms". **Under an inert pin it must return exactly `0.0`**, by the same path today's code takes.

- [ ] **Step 3: `cargo fmt`, gate, commit**

---

### Task 8: T4 — the two-way gate control

**Files:**
- Modify: `windows/worldgen/tests/suite/trencher_probe.rs`

**Both arms are required. The first alone is the vacuous half.**

- [ ] **Step 1: The inert arm — byte-identity**

Seed-42 artifacts under default pins must be **byte-identical** to the pre-`thaumic` baseline. This is The Ground's own condition on the reservation.

- [ ] **Step 2: The pinned arm — it must actually fire**

The same seed with metaphysics pinned **must differ**, and the readout names *which fields moved at how many vertices*. A pin that changes nothing is a gate wired to a derivation that never fires, and the inert arm cannot see that.

- [ ] **Step 3: `cargo fmt`, gate, commit, stage gate**

---

## Stage 4 — The biota

### Task 9: Biotic import — the third doorway

**Files:**
- Modify: `windows/worldgen/src/energy.rs`
- Modify: `kernel/src/ecology.rs` (if it earns its own axis — argue it)

Energy gathered **outside** and deposited **inside** by something that flies. Guano is the canonical form. It is the first supply in this model created by a creature's *behaviour* rather than by geology, and it gives entrance caves a character neither of the other doorways produces.

- [ ] **Step 1: Decide whether it is its own axis or a `DETRITUS` contribution — and argue it in the ledger.** Both are defensible; guano is organic matter, which is what `DETRITUS` is, but it is deposited on a completely different spatial law (near roosts, not near drainage).
- [ ] **Step 2: Write the failing test, implement, run, commit.**

---

### Task 10: Producers — a mat per metabolite

**Files:**
- Modify: `domains/species/src/lib.rs`

- [ ] **Step 1: Author chemolithotrophic mats, one per metabolite axis.** `SocialForm::Sessile` is "Rooted; placed on the map, never agentified (autotrophs)"; `treant`, `shrieker` and `twig-blight` are the terrestrial precedent for flora as kinds.
- [ ] **Step 2: Obey the distinctness rule.** **No two kinds may differ only by stratum** — The Delvers withdrew two peoples over exactly this. Chemistry is now a legitimate separator, which is the whole point; depth alone is not.
- [ ] **Step 3: Test, gate, commit.**

---

### Task 11: Decomposers and grazers

**Files:**
- Modify: `domains/species/src/lib.rs`

- [ ] **Step 1: Fungi, on detritus and guano, near entrances.** Fungi are **heterotrophs** — they eat dead matter, not rock chemistry. That is why they belong at the doorways rather than in the deep chemistry, and it is why the mushroom forest lands in the right place for the right reason.
- [ ] **Step 2: Grazers — the missing middle.** Nothing eats mats or fungi today; `rust-monster` is a predator guild with no base under it.
- [ ] **Step 3: Test, gate, commit.**

---

### Task 12: The weird

**Files:**
- Modify: `domains/species/src/lib.rs`

Kinds exploiting one chemistry — or thaumic flux — hard enough to live only where it dominates. **These are only expressible after Stages 1-3 and they are the point of the campaign.**

- [ ] **Step 1: Author them, using thaumic as a VALUE on Stage 1's axes**, never as a new mechanism. A chemo-thaumo-autotroph is a thing that eats ley-flux and builds its own body from it; the trichotomy makes that a combination rather than a special case.
- [ ] **Step 2: Test, gate, commit.**

---

### Task 13: T2 and T3 — is any of it load-bearing?

**Files:**
- Modify: `windows/worldgen/tests/suite/trencher_probe.rs`

- [ ] **Step 1: T2 — the ablation.** For each authored chemotroph: capacity and placement under (a) its authored niche and (b) the same niche with its metabolite weight removed. **`PREDICTION: placed(b) < placed(a)` for every one.** Equality is a **RED, not a finding** — it means the kind places on its other axes and the new vocabulary feeds nothing, which is the defect this campaign exists to close. Return to the authoring.

  *Prior, from another campaign's arms and not this one's measurement:* The Staple D5B reports 12/12 seeds changing at least one capacity statistic under niche ablation at `CHEMOSYNTHATE` weights 0.51/0.65/0.80, with ablated profiles collapsing to `Undercroft` exclusively.

- [ ] **Step 2: T3 — does chemistry sort the biota?** Count distinct producer kinds whose occupied vertices are dominated by different metabolites. **`PREDICTION: >= 3`.** A failure sends the roster back to authoring, not the vocabulary back to design.

- [ ] **Step 3: Record both with today's date; ledger; commit.**

---

### Task 14: Close

- [ ] **Step 1: Chronicle** — `book/src/chronicle/the-trencher.md`. **The Ceiling has no chronicle of its own** and folded into this campaign; its measurements are this campaign's §2 and the chronicle tells both halves as one story.
- [ ] **Step 2: Freshness sweep**, including any Confidence Gradient bet this moves (decision 0030).
- [ ] **Step 3: Registry rows.** `BIO-trophic-trichotomy` flips from `raw`. `BIO-chemotrophy`, `BIO-lithology-is-not-a-tolerance-axis`, `BIO-underground-light-is-unfed`, `BIO-underground-budget-is-intensive` and `BIO-underground-tolerance-is-one-axis` all move or gain pointers. `MAP-40`/`MAP-53` gain "the hook is live".
- [ ] **Step 4: Retrospective** — `docs/retrospectives/the-trencher.md` (decision 0020), covering both campaigns.
- [ ] **Step 5: Reconciliation rows** for `the-trencher` AND `the-ceiling`.
- [ ] **Step 6: Census** — `make sluice-census BRANCH=campaign/the-trencher REF=$(git rev-parse HEAD)`.
- [ ] **Step 7: G6 — HARD STOP.** Present the ledger digest to Nathan, save-format entries leading. Then `closing-a-campaign`.
- [ ] **Step 8: Merge** — `make sluice BRANCH=campaign/the-trencher REF=$(git rev-parse HEAD)`.

---

## Self-Review

**Spec coverage.** §2 → Tasks 1 and 4 (read before changing). §3 → Task 2 Step 1 (the compiler enumeration rests on it). §4.1 → Tasks 1-2. §4.2 → Tasks 3-4. §4.3 → Task 4. §4.4 → Tasks 6-8. §4.5 → Tasks 9-12. §4.6 → nothing, by construction (peoples are out). §5 T1 → Task 5, T2/T3 → Task 13, T4 → Task 8. §6 → the NOT list, no task.

**Placeholder scan.** Four steps deliberately name a *property* rather than prescribing code — Task 1's variant naming, Task 4's metabolite mapping and `Geothermal` modifier, Task 7's derivation inputs, Task 9's axis-or-detritus call. Each is a judgment requiring a reading the plan author has not done, and each says so. That is the rule, not an omission.

**Type consistency.** Task 1 produces three enums whose names go in the ledger; Tasks 2, 4 and 10-12 consume them from there rather than from this document, because the plan cannot know what Step 1's reading will name them. Task 3's axes follow the same discipline. `CHEMOSYNTHATE` keeps id 6 throughout, asserted in Task 3's test.

**One risk this plan carries deliberately.** Stage 1 Task 2's "expect an empty diff" is an assertion about generated artifacts, which this campaign's predecessor learned to distrust. It is stated as a **branch**, not a prediction: an empty diff means the retyping was pure; a non-empty one means a kind's meaning changed and is a defect to find before committing, not a rebaseline to accept.
