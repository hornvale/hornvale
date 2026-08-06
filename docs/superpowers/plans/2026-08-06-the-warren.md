# The Warren Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the placement layer ask which realm a creature lives in — a
subterranean kind is scored against `subterranean_substrate` and gated by
whether the cell actually holds a cave.

**Architecture:** A sparse `HabitatRealm` component (two rows: xorn,
rust-monster) rides `WorldComponents`. `per_species_suitability` builds a
second, hoisted substrate field and takes a parallel `&[HabitatRealm]` slice
aligned with its existing dense species index. For a `Surface` kind the
arithmetic is untouched; for a `Subterranean` kind the substrate is swapped and
the product is multiplied by a binary cave-availability factor.

**Tech Stack:** Rust 2024, no new dependencies. `cargo nextest`, `make gate`.

## Global Constraints

**Every task's requirements implicitly include this section.**

- **No new dependencies**; allowlist is `serde`, `serde_json`, `libm`.
- **No `HashMap`/`HashSet`**; no wall-clock time (banned in test code too).
- **No new seeded draw, no new stream label.** This campaign changes a
  *computation*, not a derivation. If you find yourself adding a stream label,
  stop — the plan is wrong.
- **Every `pub` item, field and variant gets a doc comment**; every bare
  primitive at a `pub` boundary gets a `type-audit:` tag of the exact form
  `bare-ok(<class>: <name>)`. Enum tags go on the **enum's own** doc using
  `Variant.field` naming.
- **`cargo fmt` is the final step before every commit.**
- **THE COMPILER IS THE ENUMERATION.** File lists here are orientation, not the
  sweep. Never silence a missing-field, arity, or exhaustiveness error with a
  wildcard, `..`, or a stub.
- **THE PRE-COMMIT HOOK IS WORKSPACE-WIDE.** `scripts/hooks/pre-commit` runs
  `make quick` (workspace clippy + type-audit check *and* report freshness)
  regardless of staged paths. No task commits while the workspace is broken, so
  a signature change drags its call sites into the same commit. Expect the
  type-audit report to move; that is the hook, not drift.
- **Run tests once, capture, grep the file:**
  `cargo nextest run -p <crate> 2>&1 | tee /tmp/hv-<task>.txt`
- **Gate timing.** ~6 min on a quiet Mac, up to 37 min contended. Budget
  `timeout: 3600000`. Other sessions share this box — **stagger gates**.
- **THE CENSUS DOES NOT RUN ON THIS BOX.** `scripts/census-canonical-host.txt`
  names `lefford`; `require_canonical_census_host` refuses the Mac. CLAUDE.md's
  "the sanctioned refresh is local" means *not AWS*. Task 5 only.

---

## File Structure

| File | Responsibility | Task |
|---|---|---|
| `domains/species/src/lib.rs` | `HabitatRealm`, `habitat_realm_registry` (2 rows) | 1 |
| `windows/worldgen/src/components.rs` | `WorldComponents.habitat_realm`; `from_stores` param | 1 |
| `windows/worldgen/src/lib.rs` | `per_species_suitability`; `demography_report_with_beta_from` | 2 |
| `windows/worldgen/tests/deep_realm_rehome.rs` | C2a's hand probe now checks the live path | 3 |
| `windows/worldgen/tests/warren_readout.rs` | the P1/P2 readout (new) | 4 |

---

### Task 1: `HabitatRealm`, and the store that carries it

**Files:**
- Modify: `domains/species/src/lib.rs` (type + registry + re-export)
- Modify: `windows/worldgen/src/components.rs` (field, `assemble`, `from_stores`)
- Test: `domains/species/tests/coverage.rs`

**Interfaces:**
- Produces: `hornvale_species::HabitatRealm` (`Copy+Clone+Debug+PartialEq+Eq`),
  `HabitatRealm::SURFACE` const, `habitat_realm_registry() ->
  ComponentStore<KindId, HabitatRealm>`,
  `WorldComponents::habitat_realm` field.
- Consumes: nothing.

- [ ] **Step 1: Write the failing test**

Add to `domains/species/tests/coverage.rs` (it already imports
`biosphere_registry`; add `HabitatRealm`, `habitat_realm_registry`):

```rust
#[test]
fn the_subterranean_roster_is_exactly_the_two_rehomed_kinds() {
    // THE WARREN: C2a re-authored these two for true darkness and
    // SUBTERRANEAN_MOISTURE and nothing scored them there. This store is the
    // consumer half. It ships with exactly these two; C2c's Mountain and
    // Duergar dwarves are the next rows, and adding one is a deliberate edit.
    let reg = hornvale_species::habitat_realm_registry();
    let sub: Vec<&str> = reg
        .iter()
        .filter(|(_, r)| **r == hornvale_species::HabitatRealm::Subterranean)
        .map(|(k, _)| k.0)
        .collect();
    assert_eq!(sub, vec!["rust-monster", "xorn"], "ascending by KindId");
    assert_eq!(reg.len(), 2, "the store is sparse: absence means Surface");
}

#[test]
fn every_kind_in_the_realm_store_has_a_biosphere_row() {
    // Referential integrity, mirroring the peopled-cluster checks in
    // windows/worldgen/src/components.rs: a realm for a kind that does not
    // exist is a typo that would otherwise be silent.
    let bio = hornvale_species::biosphere_registry();
    for (kind, _) in hornvale_species::habitat_realm_registry().iter() {
        assert!(bio.get(kind).is_some(), "{} has no biosphere row", kind.0);
    }
}
```

- [ ] **Step 2: Run it to verify it fails**

```bash
cargo test -p hornvale-species --test coverage subterranean_roster 2>&1 | tee /tmp/hv-w1-red.txt
```

Expected: FAIL to compile — `cannot find type HabitatRealm`.

- [ ] **Step 3: Add the type and the registry**

In `domains/species/src/lib.rs`, beside `LifeSchedule`:

```rust
/// Which environmental frame a kind's carrying capacity is scored in (The
/// Warren). `domains/climate` owns the richer `Realm { medium, access }`;
/// this is deliberately NOT that type — a domain crate may not depend on a
/// sibling domain, and what the placement layer needs is a two-valued
/// question, not a realm vocabulary.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HabitatRealm {
    /// Scored against the surface substrate — every kind not in the store.
    Surface,
    /// Scored against the subterranean substrate, and gated by whether the
    /// cell holds a cave at all. A void that does not exist is not habitat.
    Subterranean,
}

impl HabitatRealm {
    /// The realm a kind absent from [`habitat_realm_registry`] carries.
    pub const SURFACE: HabitatRealm = HabitatRealm::Surface;
}

impl Component for HabitatRealm {}

/// The sparse habitat-realm component: **only** kinds that are not
/// `Surface` appear. Two rows today, both re-homed by The Deep Realm, whose
/// niches have been authored for darkness and near-saturation since that
/// campaign and scored against sunlit surface cells until this one.
///
/// Sparse rather than a `BiosphereTraits` field because this has a single
/// consumer (`per_species_suitability`) which holds a slice, not a row —
/// the consumer-count rule The Long Age established, which gave the
/// opposite answer there because the life schedule had six consumers each
/// already holding the row.
pub fn habitat_realm_registry() -> ComponentStore<KindId, HabitatRealm> {
    [
        // A cave-dark, damp mineral-eater: C2a measured its subterranean fit
        // at ~2.5x its surface fit once the low-insolation proxy came out.
        (KindId("rust-monster"), HabitatRealm::Subterranean),
        // Climate-indifferent by potency rather than by curve — C2a measured
        // its ratio at 1.02, flat within noise. Listed because it LIVES
        // underground, not because scoring it there will move it.
        (KindId("xorn"), HabitatRealm::Subterranean),
    ]
    .into_iter()
    .collect()
}
```

- [ ] **Step 4: Carry it on `WorldComponents`**

Add `pub habitat_realm: ComponentStore<KindId, HabitatRealm>,` to the struct,
populate it in `assemble()` from `hornvale_species::habitat_realm_registry()`,
and add a parameter to `from_stores`. **Let the compiler name every
`from_stores` caller** — expect ten, across `cli/tests`,
`windows/worldgen/{src,tests}`, and `windows/lab/src/roster.rs`. Lab's
synthetic rosters pass `ComponentStore::new()` (their re-keyed kinds are all
surface).

Do **not** add a `check_integrity` clause for it; the coverage test above owns
that invariant and `check_integrity`'s existing arity is already at
`#[allow(clippy::too_many_arguments)]`.

- [ ] **Step 5: Run to verify green**

```bash
cargo nextest run -p hornvale-species -p hornvale-worldgen 2>&1 | tee /tmp/hv-w1-green.txt
```

- [ ] **Step 6: `cargo fmt` and commit**

```bash
cargo fmt && git add -A
git commit -m "feat(species,worldgen): a kind may declare which realm it lives in

A sparse two-row store, following dispersion_registry. Its occupants are
the two kinds The Deep Realm re-authored for darkness and damp and then
left being scored against sunlight.

Claude-Session: https://claude.ai/code/session_01H7tpnfEUvEu9wbedN1FiUc"
```

---

### Task 2: The placement layer asks the question (MUTATIONS M1 and M2)

**Files:**
- Modify: `windows/worldgen/src/lib.rs` — `per_species_suitability` (~1053–1120),
  `demography_report_with_beta_from` (~1168)
- Modify: the nine other call sites the compiler names

**Interfaces:**
- Consumes: `HabitatRealm`, `WorldComponents::habitat_realm` (Task 1).
- Produces: `per_species_suitability(..., species_realm: &[HabitatRealm])` —
  a **parallel slice**, same order and length as `species_biosphere`.

**Why a parallel slice rather than a richer element type:** the function's own
doc already establishes this contract — *"Callers that pair this index with
other per-call tuples (`species` below, `mass_map`, `.composition` tags) must
rebuild all of them together from the same `species_set` ordering."* A parallel
slice follows the existing pattern; changing the element type to
`(&KindId, &BiosphereTraits)` would churn all ten call sites harder for no gain.

- [ ] **Step 1: Write the failing tests**

New file `windows/worldgen/tests/warren_gate.rs`:

```rust
//! THE WARREN: the placement layer's realm question, and its cave gate.
use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_species::HabitatRealm;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to,
    climate_from, per_species_suitability, terrain_of,
};

/// Seed 42 at terrain depth, plus the pieces the suitability layer needs.
fn fixture() -> (/* … as in windows/worldgen/tests/waterline_probe.rs … */) {
    // Copy the setup block verbatim from `waterline_probe.rs`'s own fixture
    // (it already builds terrain + climate + stellar inputs for exactly this
    // call). Do not invent a new one — matching it keeps this test's numbers
    // comparable with that probe's.
    unimplemented!("copy waterline_probe.rs's fixture setup")
}

#[test]
fn a_subterranean_kind_scores_zero_where_there_is_no_cave() {
    // The keystone (spec 3.1): a declared realm is worth nothing without
    // per-cell availability. ~88% of land cells hold no cave; a subterranean
    // kind must draw no capacity from them.
    // Build the roster, find rust-monster's dense index, and assert:
    //   - every land cell WITHOUT a cave has k == 0.0
    //   - at least one land cell WITH a cave has k > 0.0
}

#[test]
fn a_surface_kind_is_bit_identical_to_the_pre_campaign_arithmetic() {
    // Spec 3.5. Any world movement must be attributable to the two re-homed
    // kinds and nothing else. Score goblin with the realm slice present and
    // compare, by `f64::to_bits`, against the same computation with a
    // single-substrate reference expression inlined in this test.
}
```

Fill both bodies with real code before running — the plan's skeleton is
orientation, not a licence to ship `unimplemented!()`.

- [ ] **Step 2: Run to verify red**

```bash
cargo test -p hornvale-worldgen --test warren_gate 2>&1 | tee /tmp/hv-w2-red.txt
```

- [ ] **Step 3: Implement**

Hoist the second field beside the existing ones (they are already hoisted out
of the per-species loop — keep it that way):

```rust
    // The Warren: the subterranean reading of every cell, hoisted exactly as
    // the surface `substrate` is. Built unconditionally and read only by a
    // `Subterranean` kind; `subterranean_substrate` is pure, so this costs one
    // map and no draws.
    let subterranean = hornvale_kernel::CellMap::from_fn(geo, |cell| {
        subterranean_substrate(*substrate.get(cell))
    });
```

In the per-species closure, resolve the realm once outside the cell loop:

```rust
            let realm = species_realm.get(tag).copied().unwrap_or(HabitatRealm::SURFACE);
```

and inside it:

```rust
                let (s, availability) = match realm {
                    // Untouched: the same field, the same reading, and the
                    // factor below is exactly 1.0, which is an IEEE-754 no-op.
                    HabitatRealm::Surface => (substrate.get(cell), 1.0),
                    HabitatRealm::Subterranean => (
                        subterranean.get(cell),
                        if terrain.cave_at(cell).is_some() { 1.0 } else { 0.0 },
                    ),
                };
```

then multiply the existing product by `availability` as its **last** factor,
leaving the four condition terms in their current order.

**The `× 1.0` is bit-identical** — verified during The Long Age over the
roster's thirty real masses, bit-difference `0`. Do not restructure the
expression to "avoid" the multiply; the uniform shape is what makes the
`Surface` path auditable.

At the caller (`demography_report_with_beta_from`), build the realm slice from
the same iteration that builds `species_biosphere`, so the dense index matches:

```rust
    let species_realm: Vec<hornvale_species::HabitatRealm> = wc
        .biosphere
        .iter()
        .map(|(kind, _)| {
            wc.habitat_realm
                .get(kind)
                .copied()
                .unwrap_or(hornvale_species::HabitatRealm::SURFACE)
        })
        .collect();
```

Add `debug_assert_eq!(species_realm.len(), species_biosphere.len())` inside
`per_species_suitability`, and say in its doc that the slice is parallel to the
dense index.

- [ ] **Step 4: Let the compiler finish, then run**

```bash
cargo build --workspace --all-targets 2>&1 | tee /tmp/hv-w2-build.txt
cargo nextest run --workspace 2>&1 | tee /tmp/hv-w2-green.txt
```

**Expect worldgen goldens and identity fixtures to go RED here.** That is P3
arriving, not a bug. Do **not** re-pin in this task — Task 4 measures the
movement first, and re-pinning before measuring destroys the measurement. Note
which tests fail and carry the list forward.

- [ ] **Step 5: RUN MUTATION M1**

Make `habitat_realm_registry()` return an empty store.

```bash
cargo test -p hornvale-worldgen --test warren_gate a_subterranean_kind 2>&1 | tee /tmp/hv-w2-m1.txt
```

Expected **RED** (rust-monster returns to surface scoring, so cave-free cells
are no longer zero). Paste the failure, revert, confirm green.

- [ ] **Step 6: RUN MUTATION M2**

Force the availability factor to `1.0` unconditionally in the `Subterranean`
arm.

```bash
cargo test -p hornvale-worldgen --test warren_gate a_subterranean_kind 2>&1 | tee /tmp/hv-w2-m2.txt
```

Expected **RED** (cave-free cells now score non-zero). Paste, revert, confirm.

**Both mutations are reachable here** — unlike The Long Age's M2, this campaign
ships two live occupants. If either stays green, the test is vacuous: fix the
test, not the mutation, and say so.

- [ ] **Step 7: `cargo fmt` and commit** (with both mutation outputs pasted in
the message; note the still-red goldens and that Task 4 measures before Task 5
re-pins).

---

### Task 3: C2a's hand probe now checks the live path

**Files:** Modify `windows/worldgen/tests/deep_realm_rehome.rs`

That test measured the rehoming by calling `subterranean_substrate` **by hand**
because no live path used it. It now has one.

- [ ] **Step 1:** Add a test asserting the live `per_species_suitability`
  result for rust-monster over cave-bearing land cells agrees in **direction**
  with the hand-computed ratio the file already establishes (subterranean fit
  exceeds surface fit). Assert direction and rough magnitude, not an exact
  float — this is a cross-check between two paths, not a golden.
- [ ] **Step 2:** Add the xorn counterpart asserting it stays **flat** (C2a
  measured 1.02). *This is the campaign's wiring check:* reproducing C2a's
  asymmetry through a different code path is what proves the right thing was
  connected.
- [ ] **Step 3:** Run, `cargo fmt`, commit.

---

### Task 4: Measure the blast radius before touching a single pin

**Files:** Create `windows/worldgen/tests/warren_readout.rs`; write findings to
`.superpowers/sdd/readout.md`

**This task exists because the spec refuses to predict the magnitude.** Re-pin
first and the measurement is gone.

- [ ] **Step 1 — P1, direction.** Over ≥ 20 seeds: rust-monster's mean
  suitability over cave-bearing land cells, before and after. Report the ratio.
  Xorn's the same. Expected: rust-monster up, xorn flat within noise.
- [ ] **Step 2 — P2, range collapse.** Count land cells with non-zero
  suitability for each, before and after. **Expected to fall** (~88 % of land
  is cave-free). If it rises, the gate is not working — stop and report.
- [ ] **Step 3 — P3, world movement.** For seed 42: does the committed world
  JSON change? Do settlement placements or populations move, and by how much?
  Which goldens went red in Task 2, and is each explicable as a consequence of
  the two re-homed kinds?
- [ ] **Step 4 — attribute anything surprising.** The Deep Realm's retro traced
  its drift through `niche → suitability → coexistence fit → shared
  predator/prey pressure fields → every other creature's affect`. If an
  unrelated creature moved, walk that path before writing it down. **Keep a
  subagent's numbers; re-derive its "because".**
- [ ] **Step 5** — write `.superpowers/sdd/readout.md` and commit it into the
  spec as a new §10 (the scratch dies with the worktree).

---

### Task 5: Re-pin, regenerate, and close

Authorized by Nathan at G3 — census regen and golden re-pins both.

- [ ] **Step 1: Re-pin the goldens Task 2 reddened**, one commit, message
  naming the cause and citing Task 4's readout. Re-pin a **witness**, never a
  claim.
- [ ] **Step 2: `make rebaseline`**, then review the diff **deliberately**:

```bash
make rebaseline 2>&1 | tee /tmp/hv-w5-rebaseline.txt
git diff --stat book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/
```

Read the almanac diff. A life-history or settlement line that moved should be
explicable from the readout.

- [ ] **Step 3: `make gate`** (`timeout: 3600000`, staggered).

- [ ] **Step 4: The census — ON LEFFORD, NOT HERE.**

```bash
git push -u origin the-delvers
git rev-parse HEAD                      # a SHA, never a branch name
ssh lefford 'cd ~/Projects/hornvale && HV_CENSUS_REF=<full-sha> bash scripts/census-run.sh'
# then commit + push the refreshed fixtures FROM lefford
bash scripts/census-run.sh status       # from here: confirm no heavy run is mid-flight first
```

Then `make lab-diff STUDY=the-census` to review which metrics moved, and
`make census-check`. **A census re-pin touches four files** — check the
`golden-pins.sql` tripwire; column order is `(live, pinned)`.

- [ ] **Step 5: Absorb main** (`git log HEAD..origin/main` — check at *task*
  boundaries, not session boundaries), **regenerate after the merge before
  reading the diff** (a conflict-free merge of a generated artifact is silently
  wrong), then re-gate on the merged result.

- [ ] **Step 6: Chronicle, retrospective, freshness sweep, Confidence
  Gradient.** Grep `open-questions.md` for placement/niche/habitat before
  concluding N/A. Add C2w's paragraph to the program spec and amend C2c's to
  say the realm is real.

- [ ] **Step 7: STOP — G6 is a hard stop.** Present the post-G3 digest, then
  `closing-a-campaign`.

---

## Self-Review

**Spec coverage.** §3.1/§3.3 → T1. §3.2 → T1 Step 3's doc. §3.4 → T2. §3.5 →
T2 Step 1's second test. §4 → T1–T2. §5 P1/P2/P3 → T4. §6 M1/M2 → T2 Steps 5–6.
§7 → T5. §8.1's carve-out → T5 Step 4.

**Type consistency.** `HabitatRealm::SURFACE` (const, for the absence default)
and `HabitatRealm::Surface` (variant, for `match` arms) are both used and
deliberately distinct — the same shape `LifeSchedule::ALLOMETRIC` has.
`per_species_suitability` takes `species_realm: &[HabitatRealm]` at all ten
sites after Task 2.

**Known gap, deliberate.** Task 2 leaves goldens red until Task 5. That is
sequenced, not forgotten: Task 4's measurement is destroyed by an earlier
re-pin, and this ordering is the whole reason the campaign can report a
magnitude instead of a guess.
