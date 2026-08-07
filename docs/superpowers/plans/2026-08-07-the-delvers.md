# The Delvers (C2c) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Author five dwarf kinds — Hill, Mountain, Duergar, Gully, Desert — as
the first people in the Deep Realm and the first occupant of
`LifeSchedule::Paced`, differentiated on the axes that actually reach a number.

**Architecture:** Pure authoring plus two measurement instruments. No change to
the tolerance model, the cave gate, terrain, or the chamber graph. The
instruments (bind audit, pairwise distinctness) are built **before** the kinds so
they cannot be tuned to the result.

**Tech Stack:** Rust 2024, `cargo nextest`, workspace deps limited to `serde`,
`serde_json`, `libm`.

**Spec:** `docs/superpowers/specs/2026-08-07-the-delvers-design.md`

## Global Constraints

- **No new dependencies.** The allowlist is `ALLOWED_EXTERNAL` in
  `cli/tests/architecture.rs`: `serde`, `serde_json`, `libm`.
- **No `HashMap`/`HashSet`, no `std::time::Instant`** — including in test code.
  `clippy.toml` `disallowed-types` is workspace-wide and `-D warnings` is on.
- **Every public item, field and variant gets a one-line doc comment**
  (`#![warn(missing_docs)]`).
- **Every primitive at a `pub` boundary carries a `type-audit:` verdict tag.**
  `bare-ok(<class>)` / `waiver(<reason>)` / `pending(wave-N)`. The audit is
  default-deny and runs in `make gate`. Tag format is
  `bare-ok(class: field)` — a malformed tag is a recurring plan-text defect.
- **`cargo fmt` as the final step before every commit.** fmt-gate skips are the
  most common review finding.
- **Run `git log --oneline HEAD..origin/main` at the start of every task.**
  A literal step, not a principle. Main moved three times in one session
  during The Warren and the third collided semantically. If it moved, stop and
  report before continuing.
- **`make gate` in a worktree measures 22–37 min.** Budget `timeout: 3600000`.
- **The pre-commit hook runs `make quick` workspace-wide** regardless of staged
  paths. A task that changes a shared signature drags its call sites into the
  same commit; this is expected, not a failure.
- **Do not touch these, at all:** `tolerance_liebig`, `tolerance_tiered`,
  `subterranean_substrate`, `sovereignty_floor`, cave prevalence or clustering
  in `domains/terrain`, and the two tripwires below.
- **Two committed tripwires assert `ratio == 1.000` on purpose:**
  `windows/worldgen/tests/warren_readout.rs:310` and
  `windows/worldgen/tests/deep_realm_rehome.rs:301`. If either reddens, **STOP
  and report** — the tolerance model changed and the spec's §10.3 and The
  Warren's chronicle need re-measuring. Do not nudge the assertion.
  `windows/worldgen/tests/warren_gate.rs:162` is a mirror of production code;
  this campaign should not move it either.

---

## File Structure

**Created:**
- `windows/worldgen/tests/delver_bind_audit.rs` — Task 1. Which condition axis is
  the Liebig minimum, per kind, over land, as a function of elevation `devotion`.
- `windows/worldgen/tests/delver_distinctness.rs` — Task 2. Pairwise capacity-field
  distinctness over a roster, with a discrimination self-check.
- `windows/worldgen/tests/delver_readout.rs` — Task 7. P1–P6 and M1–M3.
- `book/src/reference/proto-dwarf-generated.md` — Task 6, generated.
- `book/src/chronicle/the-delvers.md`, `docs/retrospectives/the-delvers.md` —
  Task 10.

**Modified:**
- `domains/species/src/lib.rs` — five condition-niche fns + rows in
  `biosphere_registry` (1913), `psyche_registry` (2321), `dispersion_registry`
  (2424), `society_registry` (2526), `perception_registry` (2620), `family_of`
  (2743), `KIND_CONCEPTS` (2820), `habitat_realm_registry` (1833). Plus in-crate
  count tests at 3060, 3078, 3168.
- `domains/language/src/lib.rs` — `articulation_registry` (275),
  `lexicon_registry` (427), `family_proto` (543).
- `domains/language/src/accession.rs` — one appended cohort at `EPOCH_COHORTS`.
- `domains/species/tests/coverage.rs` — five coverage tables + the two realm
  tests (439, 455) + `every_authored_kind_is_allometric_today` (419).
- `cli/src/proto.rs` — generalise beyond the hardcoded `FAMILY = "goblinoid"`.
- `windows/lab/src/metrics.rs` — `GOBLINOID_DAUGHTERS` (5920), `ALL_DAUGHTERS`
  (5925), and the `"goblinoid"` call sites at 5984, 6012.
- The pinned-count and roster-constant files enumerated in Task 4.

---

## Task 1: The bind audit — which axis actually decides

**Why first:** The spec's entire §3 rests on "climate is silent at dwarf mass and
potency." That is an inference from The Tilth's measurement on a neighbouring
mass class, not a measurement. It also has a **closed form** that must be
confirmed empirically before any trait value is authored.

`ConditionResponse::eval` (`kernel/src/ecology.rs:244`) is
`floor + (1 - floor) * devotion * exp(-z²/2)`, clamped. `tolerance_liebig`
passes `floor_buf` to temperature/moisture/insolation and **`0.0`** to elevation.
So:

- elevation's value is `devotion_elev * bump`, whose **maximum is
  `devotion_elev`**;
- temperature/moisture/insolation are **always ≥ `floor_buf`**.

Therefore **elevation is the minimum on every cell iff
`devotion_elev < sovereignty_floor(mass, potency)`.** For a 70 kg, potency-0
kind that floor is `0.4477`; human's `devotion_elev` is `0.30`, which is below
it — reproducing The Tilth's "elevation binds on 100% of land" exactly.

**The consequence this task exists to test:** the climate axes' silence is an
**authoring** consequence, not a model constraint. A dwarf authored with
`devotion_elev > ~0.45` should have its climate curves bind near its elevation
optimum — with the risk that it also becomes sharply excluded away from that
optimum, which `non_void_roster` would catch.

**Files:**
- Create: `windows/worldgen/tests/delver_bind_audit.rs`

**Interfaces:**
- Consumes: `hornvale_worldgen::{build_world, terrain_of, climate_of, sky_of, substrate_field, SkyChoice, SettlementPins}`, `hornvale_worldgen::components::WorldComponents`, `hornvale_kernel::sovereignty_floor`, `hornvale_species::{ConditionNiche, ConditionResponse, BiosphereTraits}`.
- Produces: `fn binding_axis(cn: &ConditionNiche, s: &Substrate, floor_buf: f64) -> &'static str` returning one of `"temperature" | "moisture" | "insolation" | "elevation"`. Task 7 reuses this exact function by copying it (test crates do not share code); keep the name and return type stable.

- [ ] **Step 1: Check main has not moved**

```bash
git log --oneline HEAD..origin/main
```
Expected: empty. If not, STOP and report.

- [ ] **Step 2: Write the closed-form test first — it needs no world build**

Create `windows/worldgen/tests/delver_bind_audit.rs`:

```rust
//! THE DELVERS — the condition-axis bind audit.
//!
//! `tolerance_liebig` floors temperature/moisture/insolation by
//! `sovereignty_floor(mass, potency)` and passes elevation a literal `0.0`.
//! `ConditionResponse::eval` is `floor + (1 - floor) * devotion * bump`, so
//! elevation's value never exceeds its `devotion` while the other three never
//! fall below `floor_buf`. Elevation therefore binds on EVERY cell whenever
//! `devotion_elev < floor_buf`, regardless of terrain.
//!
//! That is the mechanism behind The Tilth's measured "elevation binds on 100%
//! of land for goblin, gnoll and human", and it means the silence of the
//! climate axes is an AUTHORING consequence, not a model constraint.

#![allow(clippy::disallowed_methods)]

use hornvale_kernel::{Mass, sovereignty_floor};
use hornvale_species::{ConditionNiche, ConditionResponse};

/// The Liebig-binding axis, mirroring `tolerance_liebig`
/// (`windows/worldgen/src/lib.rs:1051`), which is private.
///
/// **This mirrors production code and is a standing maintenance obligation.**
/// If the tolerance model changes, this goes stale and must be updated with
/// it — the same contract `warren_gate.rs` carries.
fn binding_axis(cn: &ConditionNiche, s: &Substrate, floor_buf: f64) -> &'static str {
    let t = cn.temperature.eval(s.temperature_c, floor_buf);
    let m = cn.moisture.eval(s.moisture, floor_buf);
    let i = cn.insolation.eval(s.insolation, floor_buf);
    let e = cn.elevation.eval(s.height_asl_m.get(), 0.0);
    let mut best = ("temperature", t);
    for cand in [("moisture", m), ("insolation", i), ("elevation", e)] {
        if cand.1 < best.1 {
            best = cand;
        }
    }
    best.0
}

#[test]
fn elevation_binds_everywhere_when_its_devotion_is_below_the_sovereignty_floor() {
    let floor_buf = sovereignty_floor(Mass::new(70.0).unwrap(), 0.0);
    assert!(
        (floor_buf - 0.4477).abs() < 1e-3,
        "a 70 kg potency-0 kind's sovereignty floor is 0.4477; got {floor_buf:.4}"
    );

    // Human's authored elevation devotion, from `human_condition_niche()`.
    let low = ConditionResponse { optimum: 1500.0, width: 4000.0, devotion: 0.30 };
    assert!(
        low.devotion < floor_buf,
        "human's elevation devotion 0.30 sits below the floor 0.4477, which is \
         WHY The Tilth measured elevation binding on 100% of land"
    );
    // The bound is tight: elevation's value can never exceed its devotion.
    assert!(
        low.eval(1500.0, 0.0) <= low.devotion + 1e-12,
        "eval at the optimum equals devotion exactly"
    );

    // And a devotion ABOVE the floor breaks the guarantee: at the optimum,
    // elevation is no longer the smallest term.
    let high = ConditionResponse { optimum: 1500.0, width: 4000.0, devotion: 0.60 };
    assert!(
        high.eval(1500.0, 0.0) > floor_buf,
        "at devotion 0.60 the elevation term rises above the floor at its \
         optimum, so a climate axis can bind there"
    );
}
```

`Substrate` is `hornvale_worldgen::Substrate`; import it. If its fields differ
from `temperature_c` / `moisture` / `insolation` / `height_asl_m`, read
`windows/worldgen/src/lib.rs` around `substrate_field` and use the real names —
do not guess.

- [ ] **Step 3: Run it and watch it fail for the right reason**

```bash
cargo nextest run -p hornvale-worldgen --test delver_bind_audit 2>&1 | tee /tmp/hv-t1.txt
```
Expected: compile error (missing `Substrate` import) or an assertion naming the
real floor value. Fix imports until it passes on the arithmetic.

- [ ] **Step 4: Add the live sweep over real terrain**

Append to the same file. This is the empirical half — a `#[ignore]`d probe,
because it builds worlds. The ignore reason **must** carry the `heavy:` token
verbatim (`cli/tests/heavy_tier.rs` does an exact-string check, not a prefix
check; a bespoke reason reddens the gate).

```rust
/// The six settling peoples, in registry order — the population whose bind
/// behaviour The Tilth measured and this reproduces.
const SETTLERS: [&str; 6] = ["kobold", "goblin", "hobgoblin", "bugbear", "gnoll", "human"];

#[test]
#[ignore = "heavy: live worldgen over 3 seeds"]
fn the_shipped_roster_is_bound_by_elevation_on_essentially_all_land() {
    for seed in [42u64, 7, 1234] {
        let report = bind_shares(seed);
        for (kind, elev_share, n) in &report {
            assert!(
                *elev_share > 0.99,
                "seed {seed}: {kind} is elevation-bound on {:.4} of {n} land cells, \
                 expected > 0.99. If this dropped, the tolerance model or the \
                 authored devotions changed and the spec's §3.1 premise is void.",
                elev_share
            );
        }
    }
}
```

Write `bind_shares(seed) -> Vec<(&'static str, f64, usize)>` yourself: build the
world at full depth, get `WorldComponents::assemble()`, compute
`substrate_field`, and for each of `SETTLERS` iterate `geo.cells()` where
`!terrain.is_ocean(cell)`, calling `binding_axis` with
`sovereignty_floor(bio.mass, bio.potency)`. Follow
`windows/worldgen/tests/niche_breadth_probe.rs` for the exact build calls — it
does the same setup and is the pattern to copy.

- [ ] **Step 5: Run the probe and record the real numbers**

```bash
cargo nextest run -p hornvale-worldgen --test delver_bind_audit \
  --run-ignored all 2>&1 | tee /tmp/hv-t1-live.txt
```
Expected: PASS. Paste the actual per-kind shares into the module doc comment as
a recorded measurement with the date. **Do not write a number you did not run.**

- [ ] **Step 6: `cargo fmt` and commit**

```bash
cargo fmt
cargo clippy -p hornvale-worldgen --all-targets -- -D warnings
git add windows/worldgen/tests/delver_bind_audit.rs
git commit -m "test(delvers): the condition-axis bind audit, closed form and live

Elevation is the Liebig minimum on every cell iff its authored devotion is
below sovereignty_floor(mass, potency) — 0.4477 for a 70 kg potency-0 kind,
against human's authored 0.30. Reproduces The Tilth's 100%-of-land result
from the arithmetic, and confirms it live on three seeds.

The climate axes' silence is an AUTHORING consequence, not a model constraint.

Claude-Session: https://claude.ai/code/session_01H7tpnfEUvEu9wbedN1FiUc"
```

**Deliverable:** a confirmed answer to "does climate bind for a dwarf-mass kind,
and what would make it?" — which Task 3 authors against.

---

## Task 2: The distinctness instrument, built before the kinds

**Why before:** Preregistration discipline. P2 and P3 are nulls the campaign
predicts; an instrument built after seeing the kinds could be tuned to report
them. This also carries the campaign's most important guard — **the probe must
be shown able to report "different"**, or "identical" proves nothing. The
Benchmark shipped a guard that was vacuous and green because it sampled the one
cell where the bug was invisible.

**Files:**
- Create: `windows/worldgen/tests/delver_distinctness.rs`

**Interfaces:**
- Consumes: `hornvale_worldgen::per_species_suitability` (signature at `windows/worldgen/src/lib.rs:1202`), which returns `Vec<(u32, CellMap<f64>)>` where `u32` is a build-local species index, **not** a stable id — map it back through the order of the `species_biosphere` slice you passed in.
- Produces: `fn pairwise_correlations(seed: u64, kinds: &[&str]) -> Vec<((String, String), f64)>` — Pearson correlation of two kinds' per-cell suitability over land cells, ascending by pair. Task 7 copies this function.

- [ ] **Step 1: Check main has not moved**

```bash
git log --oneline HEAD..origin/main
```

- [ ] **Step 2: Write the discrimination self-check FIRST**

This test must exist and pass before the correlation function is trusted. It
runs against the **current** roster, which contains kinds known to differ.

```rust
/// **The instrument must be shown able to report DIFFERENCE.** A distinctness
/// probe whose only assertions are "these two are identical" cannot be
/// distinguished from one that computes nothing. Kobold is a deliberate
/// highland specialist and gnoll is documented for desert; if this probe
/// cannot separate them it is broken, and every null it later reports is
/// worthless.
#[test]
#[ignore = "heavy: live worldgen"]
fn the_probe_separates_two_kinds_known_to_differ() {
    let pairs = pairwise_correlations(42, &["kobold", "gnoll"]);
    let (_, r) = &pairs[0];
    assert!(
        *r < 0.95,
        "kobold and gnoll must correlate below 0.95 or this probe cannot \
         discriminate; got {r:.6}"
    );
}

/// And able to report IDENTITY, against a pair that is identical by
/// construction: a kind compared with itself.
#[test]
#[ignore = "heavy: live worldgen"]
fn the_probe_reports_unity_for_a_kind_against_itself() {
    let pairs = pairwise_correlations(42, &["goblin", "goblin"]);
    let (_, r) = &pairs[0];
    assert!((r - 1.0).abs() < 1e-12, "expected exactly 1.0, got {r:.12}");
}
```

- [ ] **Step 3: Implement `pairwise_correlations`**

Build the world at full depth. Assemble `WorldComponents`. Build the
`species_biosphere` slice and the matching `species_realm` slice exactly as
`windows/worldgen/src/lib.rs:1538-1557` does for the live path — read that code
and copy its construction; a mismatched realm slice silently scores a
subterranean kind on the surface. Call `per_species_suitability`, then for each
requested pair compute Pearson `r` over cells where `!terrain.is_ocean(cell)`.

Use `total_cmp` for any float sorting (determinism rule). Return pairs sorted
ascending by `(a, b)`.

- [ ] **Step 4: Run both self-checks**

```bash
cargo nextest run -p hornvale-worldgen --test delver_distinctness \
  --run-ignored all 2>&1 | tee /tmp/hv-t2.txt
```
Expected: both PASS. If the kobold/gnoll correlation is ≥ 0.95, **stop** — that
is itself a finding worth reporting, because it would mean the existing roster
is already degenerate and P4's control is unavailable.

- [ ] **Step 5: `cargo fmt` and commit**

```bash
cargo fmt && cargo clippy -p hornvale-worldgen --all-targets -- -D warnings
git add windows/worldgen/tests/delver_distinctness.rs
git commit -m "test(delvers): pairwise capacity distinctness, with both controls

Built before the dwarves so it cannot be tuned to the nulls it will report.
Carries a discrimination check in both directions: it must separate kobold
from gnoll, and must return exactly 1.0 for a kind against itself.

Claude-Session: https://claude.ai/code/session_01H7tpnfEUvEu9wbedN1FiUc"
```

---

## Task 3: Author the five kinds

**This is one commit and cannot be split.** Commit `6fef04fc` proves it: a
biosphere row without its peopled cluster makes `WorldComponents::assemble()`
hard-fail workspace-wide with "a Settled kind is missing a peopled component",
and `cli/tests/accession.rs` checks registered↔accessioned parity in **both**
directions, so the concepts and the cohort must land together too.

**Files:**
- Modify: `domains/species/src/lib.rs`, `domains/language/src/lib.rs`,
  `domains/language/src/accession.rs`

**Interfaces:**
- Produces: five `KindId`s — `"desert-dwarf"`, `"duergar"`, `"gully-dwarf"`, `"hill-dwarf"`, `"mountain-dwarf"`. **Every later task uses these exact strings**, and every registry is ordered ascending by `KindId`, so this is their insertion order.
- Produces: family label `"dwarf"` on all five in `family_of`, and one `family_proto` row keyed `KindId("dwarf")`.
- Produces: concept ids `"<kind>-kind"` — e.g. `"hill-dwarf-kind"`.

- [ ] **Step 1: Check main has not moved**

- [ ] **Step 2: Decide the trait values against Task 1's measurement**

Fill this table before writing code, and record the reasoning in each
condition-niche fn's doc comment the way `human_condition_niche` does — cited
justification is this file's convention, and The Generalist had its plan-text
values rejected at review in favour of measured ones (`f18d7b75`).

```
  kind            mass   realm         elevation optimum   resource vector
  hill-dwarf      70     Surface       mid                 PLANT_FORAGE + ANIMAL_PREY
  mountain-dwarf  72     Subterranean  high                MINERAL-dominant
  duergar         72     Subterranean  high                MINERAL-dominant
  gully-dwarf     62     Surface       low                 DETRITUS-dominant
  desert-dwarf    66     Surface       (see below)          low-supply mix
```

**Elevation `devotion` is the campaign's central authoring choice.** Task 1
establishes that a devotion below ~0.45 makes elevation bind everywhere and the
climate curves inert; above it, climate can bind near the optimum but the kind
becomes sharply excluded away from it. **Choose the value that keeps
`non_void_roster` green**, and state the choice and its consequence in the doc
comment. If you raise it above the floor for any kind, P1 and P3 in the spec
change and you must report that before continuing — they are preregistered.

`desert-dwarf` is the deliberate control (Nathan's call): it is differentiated
**only** on prepared axes — an arid temperature/moisture curve — and shares
Hill's elevation response. It is expected to be indistinguishable from Hill. Say
so in its doc comment.

- [ ] **Step 3: Write the five condition-niche fns and the eight registry rows**

Insert alphabetically in every registry. Row shapes, copied from the live file:

```rust
// biosphere_registry() — insert ascending by KindId
(
    KindId("hill-dwarf"),
    BiosphereTraits {
        mass: Mass::new(70.0).unwrap(),
        metabolic_class: MetabolicClass::Endotherm,
        niche: ResourceVector::new(&[(PLANT_FORAGE, 0.55), (ANIMAL_PREY, 0.45)]).unwrap(),
        condition_niche: hill_dwarf_condition_niche(),
        potency: 0.0,
        social_form: SocialForm::Settled,
        schedule: LifeSchedule::Allometric,
    },
),
// mountain-dwarf and duergar instead take:
//   schedule: LifeSchedule::paced(FACTOR).unwrap(),
// where FACTOR clears LIFESPAN_THRESHOLD_YEARS = 120.0 (worldgen lib.rs:5129).
// Compute the resulting lifespan with `hornvale_species::life_history` and put
// the real number in the doc comment.
```

```rust
// psyche_registry():      MindVector { threat_response, deliberation_latency, time_horizon }
// dispersion_registry():  Dispersion { mind, society, perception }   <- REQUIRED for minded kinds
// society_registry():     SocietyVector { sociality, status_basis, in_group_radius }
// perception_registry():  PerceptionVector { activity, night_vision, sky_attention }
// family_of():            (KindId("hill-dwarf"), "dwarf"),           <- all five
// KIND_CONCEPTS:          ("hill-dwarf-kind", "a hill dwarf"),
// habitat_realm_registry(): only mountain-dwarf and duergar, Subterranean
```

Subterranean kinds should carry high `night_vision` and low `sky_attention` —
that is honest authoring and it reaches perception consumers even though it does
not reach capacity.

- [ ] **Step 4: Add the language rows**

```rust
// articulation_registry(): ArticulationVector { labiality, vowel_space, voicing,
//   sibilance, voice_loudness, tonality, exotic: ExoticManner::_ }  x5 daughters
// lexicon_registry(): Lexicon { noun, worker_override, warrior, artisan, shaman, top } x5
// family_proto(): ONE row, (KindId("dwarf"), ArticulationVector { ... })
```

`family_proto` is keyed by **family label**, not kind. Without it,
`check_integrity` (`windows/worldgen/src/components.rs:322-331`) fails the
moment the second `"dwarf"` row lands in `family_of`. The daughters' vectors
should diverge from the proto — that divergence is what the sound-change cascade
consumes.

- [ ] **Step 5: Append the accession cohort**

In `domains/language/src/accession.rs`, `EPOCH_COHORTS`, **append** — never edit
an existing cohort:

```rust
// Epoch 9 — The Delvers (2026-08-07): the dwarf family, five kinds.
&[
    "desert-dwarf-kind",
    "duergar-kind",
    "gully-dwarf-kind",
    "hill-dwarf-kind",
    "mountain-dwarf-kind",
],
```

One cohort, not five: the campaign is one arrival event and one epoch. Five
cohorts would assert five successive language epochs, which is a stronger claim
about the world's history than the campaign makes.

- [ ] **Step 6: Compile and let the integrity check guide you**

```bash
cargo build -p hornvale-species -p hornvale-language -p hornvale-worldgen 2>&1 | tail -30
cargo nextest run -p hornvale-worldgen components 2>&1 | tail -30
```
Expected: `check_integrity` passes. If it names a missing component, add that
row — the message tells you exactly which invariant broke.

- [ ] **Step 7: Run the ghost check — this is the task's real gate**

```bash
cargo nextest run -p hornvale-worldgen --test non_void_roster \
  --run-ignored all 2>&1 | tee /tmp/hv-t3-ghost.txt
```
Expected: PASS with **no allowlist entry added**. If a dwarf fails to reach
`hornvale_demography::FLOOR` on any tested seed, the trait values are wrong —
re-author them. **Do not add an allowlist entry**; that is the `BIO-39` kobold
failure being authored fresh.

- [ ] **Step 8: `cargo fmt` and commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
git add domains/species/src/lib.rs domains/language/src/lib.rs domains/language/src/accession.rs
git commit -m "feat(delvers): author the dwarf family — five kinds, one epoch

Hill, Mountain, Duergar, Gully, Desert across eight registries plus the
accession cohort. Mountain and Duergar are Subterranean and Paced — the
first occupants of LifeSchedule::Paced, which The Long Age shipped empty.

Desert is the deliberate control: differentiated only on prepared axes.

Claude-Session: https://claude.ai/code/session_01H7tpnfEUvEu9wbedN1FiUc"
```

Expect the rest of the workspace to be red after this commit — Task 4 is the
repair, and the pre-commit hook's `make quick` (fmt/clippy/type-audit) is what
must pass here, not the full suite.

---

## Task 4: Re-pin every count and roster the roster moved

**Files (all Modify):** enumerated below, each with its known line.

- [ ] **Step 1: Check main has not moved**

- [ ] **Step 2: Get the full failure list in one pass**

```bash
cargo nextest run --workspace --no-fail-fast 2>&1 | tee /tmp/hv-t4.txt
grep -E "^\s+FAIL|panicked at" /tmp/hv-t4.txt | head -80
```
Run once, inspect many. Do not re-run the suite to grep a second line.

- [ ] **Step 3: Fix the hard counts**

```
domains/species/src/lib.rs:3060      bio.len()  30 -> 35
domains/species/src/lib.rs:3078      psy.len()   9 -> 14
domains/species/src/lib.rs:3080-3084 per.len()   9 -> 14
domains/species/src/lib.rs:3168-3211 the 30-name lexicographic roster -> 35 names
domains/species/tests/coverage.rs:435  reg.len() 30 -> 35
windows/worldgen/tests/dissolve_equivalence.rs:18,28,30,73-83
windows/worldgen/tests/demesne.rs:344  BASELINE_PEOPLED_KINDS_42  6 -> 11
```

Insert each dwarf at its **alphabetical** position, not at the end.

- [ ] **Step 4: Fix the roster constants**

```
domains/species/tests/social_form.rs:48,78
windows/worldgen/src/lib.rs:8784-8796        the `six_peoples` BTreeSet
windows/worldgen/tests/generalist_baseline.rs:121,127
windows/worldgen/tests/tolerance_baseline.rs:129
windows/worldgen/tests/tolerance_mutation.rs:186
windows/worldgen/tests/solitary_tongue.rs:438
```

Names like `PEOPLES_WITH_HUMAN: [&str; 6]` now mislead. Rename to `PEOPLES` with
the correct arity and update every use site — a stale name here is how the next
campaign inherits a wrong count.

- [ ] **Step 5: Re-measure, don't guess, the seed-42 pinned tables**

These are **measurements**, not counts. Run the test, read the actual value from
the failure message, and pin that:

```
windows/worldgen/tests/deep_grammar.rs:181-202   (seed x kind morphology depth)
windows/worldgen/tests/diachronic.rs             (six LADDER_TABLE blocks)
windows/worldgen/tests/descent_graph.rs:76       (genesis-root count)
windows/worldgen/tests/exposure.rs               (Root/Gap partition)
windows/worldgen/tests/history_emit.rs           (material-fourth-key ties)
windows/worldgen/src/lib.rs                      (pantheon size, name-gloss)
windows/lab/src/metrics.rs:7189,7218,7306        (name-syllable pins)
windows/lab/tests/the_dial.rs                    (kobold loss_fraction denom)
windows/book/src/lib.rs:4224,4576,4603,5043,5074 (five peoples-line literals)
windows/vessel/tests/*                           (GRIEVANCE_NPC, if it moved)
```

For each, **classify the break before fixing it** (The Warren's rule): a
prediction that stopped being true is a finding; the same claim in other words
elsewhere needs a sweep — ask "how many places state this claim?", since no grep
on the message will pair them; a mirror of production code is the test working.
Report anything in the first category rather than silently re-pinning.

- [ ] **Step 6: Full suite green**

```bash
cargo nextest run --workspace --no-fail-fast 2>&1 | tee /tmp/hv-t4b.txt
```
Expected: PASS, **except** the byte goldens, which Task 8 rebaselines.

Confirm explicitly that `warren_readout`, `deep_realm_rehome` and `warren_gate`
are green. If any is red, STOP and report.

- [ ] **Step 7: `cargo fmt` and commit**

```bash
cargo fmt && git add -A && git commit -m "test(delvers): re-pin the counts and rosters five kinds moved

Every break classified before repair: predictions that stopped being true
are reported, restatements swept, mirrors updated.

Claude-Session: https://claude.ai/code/session_01H7tpnfEUvEu9wbedN1FiUc"
```

---

## Task 5: The coverage tables — the deliberate edits

**Files:** Modify `domains/species/tests/coverage.rs`

These are the rows previous campaigns left as tripwires naming C2c by hand. Each
is a deliberate edit, not a failure.

- [ ] **Step 1: Check main has not moved**

- [ ] **Step 2: The life-schedule table (`coverage.rs:284-307`)**

```rust
let expected: &[(&str, Rung, &[&str])] = &[
    ("Allometric", Rung::Witnessed, &allometric_kinds),
    ("Paced", Rung::Witnessed, &["duergar", "mountain-dwarf"]),
];
```

**The non-obvious edit:** the `Allometric` row currently uses `&every_kind` —
the whole registry. Once any kind is `Paced`, `life_schedule_witnesses(false)`
no longer equals `every_kind`, so that row must become an explicit or filtered
list. It fails even though nothing about Allometric kinds changed.

- [ ] **Step 3: The realm roster (`coverage.rs:439`)**

```rust
assert_eq!(sub, vec!["duergar", "mountain-dwarf", "rust-monster", "xorn"], "ascending by KindId");
assert_eq!(reg.len(), 4, "the store is sparse: absence means Surface");
```

Update the comment: it currently says "C2c's Mountain and Duergar dwarves are
the next rows" — replace with a statement of what shipped.

- [ ] **Step 4: Widen `every_authored_kind_is_allometric_today` (`coverage.rs:419`)**

Its own doc names C2c as the campaign that must widen it. Rename it — the name
is now false. Something like
`only_the_paced_dwarves_depart_from_pure_allometry`, asserting the exception set
explicitly rather than deleting the check.

- [ ] **Step 5: The remaining four tables**

`metabolic_class` (79), `status_basis` (143), `activity_cycle` (178),
`social_form` (216, the `Settled` list). Add the five dwarves in alphabetical
position to each witness list they belong in.

- [ ] **Step 6: Run, fmt, commit**

```bash
cargo nextest run -p hornvale-species 2>&1 | tail -20
cargo fmt && git add domains/species/tests/coverage.rs
git commit -m "test(delvers): the coverage tables' deliberate edits

Paced moves Declared -> Witnessed with two witnesses; the realm store goes
2 -> 4; every_authored_kind_is_allometric_today is renamed rather than
deleted, since the claim it made is now false rather than unenforced.

Claude-Session: https://claude.ai/code/session_01H7tpnfEUvEu9wbedN1FiUc"
```

---

## Task 6: Generalise the proto seam beyond goblinoid

**Files:** Modify `cli/src/proto.rs`, `scripts/regenerate-artifacts.sh`,
`windows/lab/src/metrics.rs`, `windows/worldgen/tests/proto_goblinoid_golden.rs`.
Create `book/src/reference/proto-dwarf-generated.md`.

- [ ] **Step 1: Check main has not moved**

- [ ] **Step 2: Generalise `cli/src/proto.rs`**

`cli/src/proto.rs:25` is `pub(crate) const FAMILY: &str = "goblinoid";` and its
doc says a second family "would need either a second page or this function
generalized to take a family argument — deferred until that need is real."
Dwarf makes it real. Take the family as an argument; keep `goblinoid`'s output
**byte-identical** (`proto_goblinoid_golden.rs` pins it and must stay green
without a rebaseline).

- [ ] **Step 3: Emit the second page**

Add the dwarf invocation to `scripts/regenerate-artifacts.sh` next to line 100's
`proto-goblinoid-generated.md`, and add the page to `book/src/SUMMARY.md`
(hand-authored — it is the one book file that is never code-generated).

- [ ] **Step 4: Extend the lab's daughter constants**

`windows/lab/src/metrics.rs:5920,5925` hardcode `GOBLINOID_DAUGHTERS: [&str; 3]`
and `ALL_DAUGHTERS: [&str; 4]`; the call sites at 5984 and 6012 pass the literal
`"goblinoid"`. The family-monophyly, inventory-closure and homophony metrics
will **silently not measure dwarf** otherwise — silence is the failure mode this
programme exists to prevent.

**This changes census metric values**, so it must land before Task 9's regen.

- [ ] **Step 5: Verify goblinoid is untouched**

```bash
cargo nextest run -p hornvale-worldgen --test proto_goblinoid_golden 2>&1 | tail -10
```
Expected: PASS with no rebaseline. If it fails, the generalisation changed
goblinoid's output and that is a bug, not a rebaseline.

- [ ] **Step 6: fmt, commit**

---

## Task 7: The preregistered readout and the three mutations

**Files:** Create `windows/worldgen/tests/delver_readout.rs`

**Interfaces:**
- Consumes: `binding_axis` (Task 1) and `pairwise_correlations` (Task 2) — copy both in; Rust test crates do not share code.

- [ ] **Step 1: Check main has not moved**

- [ ] **Step 2: P1 — climate is (or is not) silent for a dwarf**

Assert the binding axis is `elevation` on ≥ 99% of land for all five dwarves,
seeds 42 + 25 more. **If Task 3 raised any elevation devotion above the
sovereignty floor, this prediction changes** — report the real number and amend
the spec's §5 rather than forcing the assertion.

- [ ] **Step 3: P2 — Mountain ≡ Duergar, the predicted null**

```rust
/// **A PREREGISTERED NULL.** Mountain and Duergar share realm, elevation
/// response and resource vector; they differ only in depth, which has no slot
/// in the model — a chamber inherits `height_asl_m` from the ground above it
/// (`subterranean_substrate`, worldgen lib.rs:2189). The spec predicts they
/// are numerically the same kind, and this pins it.
///
/// If this reddens, something differentiates them that the campaign did not
/// intend. That is a finding to chase, not an assertion to relax.
#[test]
#[ignore = "heavy: live worldgen over 25 seeds"]
fn mountain_and_duergar_are_one_rank() { /* assert max |diff| < 1e-12 per cell */ }
```

- [ ] **Step 4: P3 — Desert ≡ Hill, the control**

Correlation > 0.99 despite an authored arid niche. Cross-reference
`BIO-gnoll-desert` in the doc comment: this is that row's second witness, and
the first one that was predicted in advance.

- [ ] **Step 5: P4 — the discrimination control**

Gully / Hill / Mountain pairwise correlations **below** 0.95. Without this, P2
and P3 prove nothing.

- [ ] **Step 6: P5 — the paced pair is read**

`cascade_regime_of` returns the slow regime for Mountain and Duergar and the
settled regime for the other three; `generation_length_of("mountain-dwarf")`
exceeds what mass alone predicts. This is the consumer The Long Age could not
observe, closed for free.

- [ ] **Step 7: P6 — world identity moved, magnitude unpredicted**

Report seed 42's committed-world diff. **Do not predict the magnitude** — The
Warren's retrospective records that refusing to guess is what kept a falsified
prediction from acquiring a number to defend.

- [ ] **Step 8: M1, M2, M3 — the mutations**

Each is a temporary local edit, run, observe RED, revert. Record the observed
failure message in the test's doc comment.

- M1: flip Mountain's realm row to `Surface` → its capacity becomes non-zero on
  cave-free land.
- M2: revert Mountain's schedule to `Allometric` → `cascade_regime_of` returns
  the fast regime. **The mutation The Long Age could not run.**
- M3: give Duergar a materially different elevation curve → P2's identity
  assertion breaks. **Without M3, P2 could be reporting identity because it
  computes nothing.** This is the one that matters most and the easiest to skip.

- [ ] **Step 9: Run everything, record real numbers, fmt, commit**

```bash
cargo nextest run -p hornvale-worldgen --test delver_readout \
  --run-ignored all 2>&1 | tee /tmp/hv-t7.txt
```

---

## Task 8: Regenerate artifacts and rebaseline the byte goldens

- [ ] **Step 1: Check main has not moved** — and if it did, **regenerate before
  reading any diff**. A generated artifact has no merge; a conflict-free merge
  of one is silently wrong.

- [ ] **Step 2: Regenerate**

```bash
make rebaseline
make rebaseline-goldens
```

- [ ] **Step 3: Diff and read every generated page**

```bash
git diff --stat book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/
```

`docs/audits/type-audit-report.md` drifts on any pub-boundary change and omitting
it is a common miss. The dictionary should have gained a **Dwarf cognates
section** for free (`cli/src/dictionary.rs:104` loops `family_proto().ids()`) —
confirm it did; its absence means `family_proto` is wrong.

- [ ] **Step 4: Full gate**

```bash
make gate 2>&1 | tee /tmp/hv-gate.txt
```
Budget 3600000 ms. Expected: PASS.

- [ ] **Step 5: Commit**

---

## Task 9: The census regen — authorized, on lefford

**Authorization:** granted by Nathan 2026-08-06. One run, at the merged SHA.
This is a carve-out; do not run it early or twice.

- [ ] **Step 1: Push the branch**

The census runs from a SHA on the canonical box, not from local state.

```bash
git push -u origin the-delvers
git rev-parse HEAD
```

- [ ] **Step 2: Dispatch on lefford with the FULL SHA**

```bash
ssh lefford '... HV_CENSUS_REF=<full-sha> bash scripts/census-run.sh'
```

The Mac's guard (`require_canonical_census_host`) refuses this locally.
CLAUDE.md's "the sanctioned refresh is local" means **not AWS**, not this box.
Pass a SHA, never a branch name.

- [ ] **Step 3: Expect the corrected churn, not the spec's**

```
book/src/laboratory/generated/the-census/rows.csv            ~1000 / 1000 rows
book/src/laboratory/generated/census-of-the-meeting/rows.csv  ~0
```

If `the-census` moves far fewer than 1000 rows, something is wrong — a new
settling people re-decides settlement placement on every seed.

- [ ] **Step 4: Commit and push from lefford**, then pull down.

- [ ] **Step 5: `make lab-diff STUDY=the-census`** and read which metrics moved.
  This is the review surface for the chronicle's census section.

---

## Task 10: Close — book, chronicle, retrospective, bookkeeping

- [ ] **Step 1: Check main has not moved.** If it did, **re-run Task 7's readout
  after absorbing.** A confirmed prediction has a shelf life measured in merges:
  The Warren measured P1 at 2.557×, wrote it into a chronicle, a spec and a
  census, and it read 1.000 an hour later. "Never absorb mid-measurement" was
  honoured and was not enough.

- [ ] **Step 2: The chronicle** — `book/src/chronicle/the-delvers.md`. Technical
  and mathematical altitude, comprehensible without the code. **No registry IDs
  anywhere in `book/` outside `book/src/frontier/`** — `docs_consistency`
  enforces it. Name the concept instead.

- [ ] **Step 3: Freshness sweep.** `book/src/domains/species.md:38,45,267,271,412`
  all say "six settling peoples". Sweep the whole book for "five peoples" / "six
  peoples" prose. **Sweep on the invariant, not the wording** — the same claim
  appears in phrasings no grep on one form will pair.

- [ ] **Step 4: Confidence Gradient re-score** (`book/src/open-questions.md`).
  Run the grep even if no bet obviously moved — The Warren's retrospective
  records nearly filing a false N/A. **A bet can be moved by a campaign that
  shares none of its nouns.**

- [ ] **Step 5: Retrospective** — `docs/retrospectives/the-delvers.md`. Process
  lessons, not product.

- [ ] **Step 6: Registry and metaplan bookkeeping** (spec §9). Scan the registry
  before minting any slug; IDs collide by arithmetic and the frozen-ids fixture
  makes a taken id look free. Mint slugs, never numbers (decision 0026).
  Correct in `2026-08-03-the-peoples-program-design.md`: the census cost (§1.4),
  the eight-registry count (§3.5), and `chamber.rs:304`'s stale "C2c is the
  digging campaign" pointer.

- [ ] **Step 7: `make gate` one final time**, then hand to G6.

---

## Self-Review

**Spec coverage:** §1.1→T1; §1.2→T7/P2; §1.3→T10 pointer fix; §1.4→T9/T10;
§2→T10 registry rows; §3.1→T1+T3; §3.2→T3; §3.3→T3+T5+T7/P5; §3.4→T6;
§3.5→T3+T10; §4→T4+T8; §5 P1-P6→T7; §6 M1-M3→T7; §7→T8/T9/T10; §9→T10.

**Gap found and closed:** the spec's §7 requires `non_void_roster` green with no
allowlist; that is now Task 3 Step 7 as an explicit blocking gate rather than an
implicit part of the final suite.

**Type consistency:** `binding_axis` and `pairwise_correlations` keep the same
signatures in Tasks 1, 2 and 7. The five `KindId` strings are fixed in Task 3's
Interfaces block and used verbatim in Tasks 4, 5 and 7.
