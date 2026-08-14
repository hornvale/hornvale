# The Glasshouse, Stage B — re-centring the temperature baseline

> **For agentic workers:** REQUIRED SUB-SKILL: Use `superpowers:subagent-driven-development`
> (recommended) or `superpowers:executing-plans` to implement this plan
> task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Move the census population from a 65%-ice, −11.99 °C median to one
spanning glacial→temperate→warm, by fixing the three measured causes in the
order their interactions force.

**Architecture:** Three independent defects, landed in dependency order.
Terrain first (the craton rescale under-delivers its continental budget, so the
coastline is cut ~1.1 km below the shelf break and all land reads high);
then the greenhouse thermostat in astronomy+climate (the habitable zone is
denominated for a *variable* greenhouse while the temperature model has a fixed
one); then the latitude profile (whose area-mean is +10 K, not 0, and which is
currently the only thing keeping worlds as warm as they are). The classifier is
measurement-gated and may need nothing.

**Tech Stack:** Rust 2024, `serde`/`serde_json`/`libm` only. `cargo nextest`,
`windows/lab` studies, `tools/type-audit`, `tools/seam-guard`.

## Global Constraints

- **Spec §4.1's preregistered criteria are FROZEN FOR GOOD.** They were
  superseded once (baselines written against estimates); §4.0 records that
  supersession as **spent**. A second revision after any outcome is visible is
  the retuning decision 0016 forbids. Do not touch §4.
- **The before-arm is frozen and verified** at `a9c8dd18` against the
  post-absorption census: `r(insolation-rel,T)=+0.9227`,
  `r(zone-position,T)=-0.9243`, median −11.99 °C, spread **44.5907 K**
  (nearest-rank), `insolation-rel` median 0.7419, `zone-position` median
  0.5023. Criterion 2's floor is **≥31.2 K**. Report criterion 6 over the
  **spinning subset** as well as all 1000 — mixing those populations is what
  made the original criterion nearly vacuous.
- **`domains/terrain/` carries byte-identity discipline.** Float addition is
  not associative; preserve summation order exactly. Verify with the artifact
  drift check, not just tests (`domains/terrain/CLAUDE.md`).
- **Regen order after any world-output change:** `make rebaseline` **then**
  `make rebaseline-goldens` — `regenerate-artifacts.sh` does not cover
  `windows/vessel/tests/fixtures/*.json`.
- **Census regen is a carve-out** — explicit authorization from Nathan.
  Cost from the ledger, not from prose: latest run **949.579 s**
  (`grep '| census |' docs/timings.md | tail`).
- **Decision numbers:** next free is **0131**. After absorbing main, re-check
  with `ls docs/decisions/ | grep -oE '^[0-9]{4}' | sort | uniq -d` — preflight
  and `docs_consistency` are both blind to duplicates.
- **Two guards that cost Stage A a red.** `cli/tests/heavy_tier.rs` accepts
  exactly one verbatim `heavy:` reason:
  `"heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"`.
  `cli/tests/claim_shape.rs` (decision 0093) requires a `/// claim: readout(…)`
  tag on **any** test that loops over seeds.
- **Constants need a kind (decision 0106):** `physics` / `earth-biosphere` /
  `hornvale-choice` / `hornvale-gauge`, with a citation. A wrong citation is
  worse than none.
- **Earth data settles anchors; Hornvale's census settles only distributions**
  (spec §6). Tuning `k` until the census median hits +8.6 °C is the forbidden
  circular move.

---

## File Structure

| File | Responsibility | Task |
|---|---|---|
| `domains/terrain/src/land_elevation_attribution.rs` | Existing probe. Gains the grid-realised continental fraction, the conditional-mean-over-retained-set accumulator the audit says is owed, and the craton radius distribution. | 1 |
| `domains/terrain/src/crust.rs` | `draw_cratons_unrepelled`'s rescale: exact solve + clamp constant. | 2 |
| `domains/terrain/tests/tectonic_properties.rs` | Rescale property tests + pin isolation. | 2 |
| `domains/astronomy/src/streams.rs` | New `GREENHOUSE` leg label. | 3 |
| `domains/astronomy/src/anchor.rs` | `Anchor.greenhouse_residual`, drawn. | 3 |
| `domains/astronomy/src/facts.rs` | `GREENHOUSE_FORCING_K` predicate + commit. | 3 |
| `domains/astronomy/tests/genesis_properties.rs` | Pin isolation for the new draw. | 3 |
| `domains/climate/src/provider.rs` | `ClimateInputs.greenhouse_forcing_k`. | 4 |
| `domains/climate/src/temperature.rs` | Thermostat transfer function; latitude profile. | 4, 5 |
| `windows/worldgen/src/lib.rs` | Wire astronomy's residual into `ClimateInputs`. | 4 |
| `windows/lab/src/metrics.rs` | `greenhouse-forcing-k` census column. | 3 |
| `docs/decisions/0131-*.md` | The clamp re-decision. | 2 |

---

## Task 1: Measure the grid, before touching anything

The analytic model cannot see what decides the coastline. Ledger entry #10's
whole recommendation rests on a projection from a paper model that omits
terranes, microcontinents and the repulsion pass; this task replaces the
projection with a measurement, and becomes the instrument that judges Task 2.

It also discharges a debt the audit names explicitly (§3.5): *"Stage B must
measure the conditional mean over the retained set before quoting any budget at
all — it is one extra accumulator in this probe."*

**Files:**
- Modify: `domains/terrain/src/land_elevation_attribution.rs`

**Interfaces:**
- Consumes: the existing probe's `ElevationTerms` decomposition and its
  conservation assert (do not weaken either).
- Produces: a printed readout block later tasks diff against. No public API.

- [ ] **Step 1: Read the existing probe and its conservation assert**

Read `domains/terrain/src/land_elevation_attribution.rs` end to end, plus
§1.2 and §2 of `docs/audits/land-elevation-attribution.md`. The probe already
asserts, on every land cell of every seed, that the components re-add to the
pipeline's own elevation within `1e-9`. **That assert stays exactly as it is** —
it is what makes every share in the readout meaningful.

- [ ] **Step 2: Add the three genuinely missing accumulators**

**Controller's correction, verified against the code before dispatch — the
first draft of this step was wrong.** It asked for five accumulators; **two of
them already exist**. The probe's seed loop already maintains `supply_sum`,
`threshold_sum` and `land_sum` — analytic continental supply, the grid area
clearing `CONTINENTAL_THRESHOLD_KM`, and the land the percentile granted — and
already prints them as the audit's "WHY SEA LEVEL LANDS THERE" block (0.2592 /
0.2724 / 0.3731). **Do not re-add them.** Read the loop at
`land_elevation_attribution.rs:256-300` first.

Three are actually missing:

1. **conditional mean crust over the retained set** — mean crust thickness over
   cells that clear the threshold. This is the accumulator audit §3.5 says is
   owed, and it converts "sea level would rise to the shelf break" into a
   defensible elevation figure instead of the 1113 m cut depth everyone
   misreads. §3.5's own estimate is ~29.1 km; measure it rather than adopt it.
2. **craton radius distribution** — min / mean / max / coefficient of variation,
   and the count at the clamp. CV is the variety axis ledger #10 found the
   obvious fix destroys; without it, Task 2 cannot be judged.
3. **post-repulsion pair separation** — min and mean centre-to-centre angle over
   craton pairs, against `REPEL_SEPARATION_FACTOR * (r_i + r_j)`. Records what
   `repel_cratons` achieves *today*, so Task 2 can tell a saturating repulsion
   pass from a working one.

Two facts that will otherwise cost you a wrong measurement:

- **`TectonicGlobe` already retains the craton set** — `globe.cratons`, no need
  to re-call `draw_cratons`. But it holds **majors alone**: `microcontinents`
  and `terranes` are separate fields, and the field's own doc says craton-census
  metrics count majors only. Compute the CV over `globe.cratons` and say so in
  the readout label, or the variety number silently means something else.
- **`REPEL_SEPARATION_FACTOR` is private to the `crust` module** (`crust.rs:656`,
  a bare `const`), so the probe cannot read it today. Widen it to `pub(crate)`.
  **Do not re-type the literal `1.2`** — a duplicated constant drifts silently,
  which is the exact trap audit §1.1 documents for the per-term helpers.

- [ ] **Step 3: Keep the probe in the commit gate**

The probe currently runs in the commit gate (12 worlds, 3–6 s at ordinary
load; §6 of the audit). Adding accumulators must not change that. Re-measure
and update the audit's cost table if it moved.

If it *does* need deferring, the reason string must be verbatim:
`#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]`
— an invented wording goes red on `cli/tests/heavy_tier.rs`. The probe already
carries its `/// claim: readout(…)` tag; keep it.

- [ ] **Step 4: Run it and capture the readout**

```bash
cargo test -p hornvale-terrain --lib the_land_elevation_terms_attribute_their_variance -- --nocapture
```

Paste the full readout into the task report. This is Stage B's terrain
before-arm.

- [ ] **Step 5: Verify byte-identity — the probe must change nothing**

This task is read-side only, so the world must be bit-identical.

```bash
cargo nextest run -p hornvale-terrain
SKIP_CENSUS=1 bash scripts/regenerate-artifacts.sh
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/ docs/digest/ book/src/domesday/ clients/game/core/tests/fixtures/
```

**Decision rule on the drift check** — do not predict, route:

| what moved | response |
|---|---|
| nothing | correct; this task is read-only |
| `docs/audits/type-audit-report.md` only | you changed a `pub` boundary; regenerate and commit it in this task |
| anything under `book/src/gallery/` or `book/src/laboratory/` | **STOP** — a read-only probe moved world output; you perturbed the arithmetic |

**The known deliberate red is GONE — measured, not assumed.** Earlier drafts of
this plan (and Task 1's dispatch) said `regenerate-artifacts.sh` aborts at `lab
backfill-schema` because the committed census predated Stage A's three metrics.
Task 1 ran it and it **exited 0**, leaking no `schema.json.tmp`. Cause: Stage A
merged to `main`, and The Rill's census refresh on `main` regenerated with those
metrics present, so the absorbed census is a strict superset of the branch's own
(identical blob to `main`'s, `324b38cb`). **No task should budget for the abort
or run post-abort steps by hand.** If a regen *does* abort at that step, that is
new information — stop and report it.

- [ ] **Step 6: Commit**

```bash
git add domains/terrain/src/land_elevation_attribution.rs docs/audits/land-elevation-attribution.md
git commit -m "probe(terrain): measure the grid, the retained set, and what repulsion achieves"
```

---

## Task 2: The craton rescale — exact solve and a raised clamp

**Files:**
- Modify: `domains/terrain/src/crust.rs:634-640`
- Modify: `domains/terrain/tests/tectonic_properties.rs`
- Create: `docs/decisions/0132-the-craton-clamp-is-a-budget-not-a-limit.md`

**Interfaces:**
- Consumes: Task 1's readout as the before-arm.
- Produces: no signature change. `continental_supply` keeps its type and its
  grid-free/draw-free guarantee.

**Background the implementer must not have to rediscover.** The rescale targets
a budget of ~0.4123 and delivers ~0.2706. Ledger #10 measured, over 20,000
simulated draws plus a 4,000-point sphere overlap Monte Carlo:

```
  variant                   analytic     TRUE  true/quota  radius CV  %at clamp
  today (as shipped)          0.2681   0.2370       0.644      0.227      50.3%
  exact solve, clamp 0.6      0.3506   0.2984       0.814      0.001      99.5%
  exact solve, clamp 0.8      0.4082   0.3398       0.920      0.275      30.5%
  exact solve, no clamp       0.4082   0.3406       0.922      0.351       0.0%
```

The budget is **unreachable under the 0.6 clamp on 97.1% of worlds**, so an
exact solve alone cannot deliver it — it can only pin every craton at the clamp,
which is why its radius CV collapses to 0.001. Raising the clamp *improves*
variety and saturates by ~0.8.

- [ ] **Step 1: Write the failing property test**

**Controller's correction — the first draft put this test in the wrong file, and
its seed derivation was silently wrong.** Verified against the tree Task 1 left:

- **The test goes in `crust.rs`'s own `mod tests`**, beside
  `continental_supply_is_the_area_the_rescale_budgets` (now `crust.rs:1735`),
  **not** in `tectonic_properties.rs`. That file is an *integration* test and
  sees only `pub` items; `CRATON_RADIUS_MAX_RAD` is `pub(crate)` and
  `default_ocean_target` is a helper defined *inside* `mod tests`
  (`crust.rs:1178`). Do not widen either to `pub` just to place a test.
- **The terrain seed is `Seed(seed).derive(streams::ROOT)`, never `Seed(seed)`.**
  The sketch below originally used the bare world seed. That compiles, runs, and
  measures a *different craton population* — a silent wrong answer, not a red.
  Both existing sweeps use the `derive` form; copy it.
- The rescale site is now **`crust.rs:650-655`** (Task 1's constant shifted it
  from the 634-640 this plan first named).

In `domains/terrain/src/crust.rs`, inside `mod tests`:

```rust
/// claim: invariant(forall-seed) — the rescale must deliver the budget it
/// solves for, unless the clamp makes that budget unreachable
#[test]
fn the_rescale_delivers_its_own_budget() {
    for seed in 0..200u64 {
        let terrain_seed = Seed(seed).derive(streams::ROOT);
        let ocean_target = default_ocean_target(terrain_seed);
        let cratons = draw_cratons(
            terrain_seed,
            &TerrainPins::default(),
            ocean_target,
            &mut Vec::new(),
        );
        let supply = continental_supply(&cratons);
        let budget = budget_for(terrain_seed, ocean_target);
        let all_clamped = cratons
            .iter()
            .all(|c| c.radius_rad >= CRATON_RADIUS_MAX_RAD - 1e-9);
        // Either it hit the budget, or the clamp made the budget unreachable.
        if !all_clamped {
            assert!(
                (supply - budget).abs() / budget < 0.02,
                "seed {seed}: supply {supply:.4} vs budget {budget:.4}, \
                 nothing clamped — the solve did not converge",
            );
        }
    }
}
```

`budget_for` does not exist. **Do not re-type `(1.0 - ocean_target) * (1.0 + margin)`
in the test** — that is the duplicated-formula drift audit §1.1 documents.
Extract the two lines at `crust.rs:629-631` into a crate-internal helper that
both `draw_cratons_unrepelled` and the test call, so one edit moves both.
The `margin` draw is the first `f64` off the `CRATONS` stream, so the helper
must consume that stream identically — **it is a save-format contract; the
helper must not change how many draws are taken or in what order.**

- [ ] **Step 2: Run it and confirm it fails**

```bash
cargo test -p hornvale-terrain --test tectonic_properties the_rescale_delivers_its_own_budget
```

Expected: FAIL, with a shortfall around 34% on most seeds.

- [ ] **Step 3: Implement the exact solve and the named clamp**

Replace the closed-form `sqrt` with a deterministic bisection that includes the
clamp inside the objective.

**`CRATON_RADIUS_MAX_RAD` ALREADY EXISTS.** Task 1 introduced it at
`crust.rs:617` as `pub(crate) const … = 0.6` (byte-inert: it replaced a bare
literal at the `.min()` site and an internal bound assert). **You are CHANGING
its value and rewriting its doc, not adding it** — pasting the block below as a
new declaration is a duplicate definition. That fails loudly at compile time
rather than silently, but do not spend a cycle on it. Keep `pub(crate)`; nothing
outside the crate needs it.

```rust
/// The maximum angular radius of a craton, radians.
///
/// kind: **hornvale-choice** (decision 0106; re-decided in 0131). Not a
/// geometric limit — a bound on how much of one world a single craton may be.
/// Raised from 0.6 by The Glasshouse: at 0.6 the rescale's own budget is
/// unreachable on 97.1% of worlds, so an exact solve could only pin every
/// craton at the clamp, collapsing continent-size variety (radius CV 0.001
/// simulated, against 0.2428 measured on the grid today). Overlap-deducted
/// continental area rises with this constant and saturates by ~0.8; beyond
/// that, added radius lands on ground another craton already covers.
pub(crate) const CRATON_RADIUS_MAX_RAD: f64 = 0.8;

/// Solve for the radius scale that makes the craton set deliver `target`
/// steradians of continental area, with `CRATON_RADIUS_MAX_RAD` applied
/// inside the objective so clamped cratons do not silently discard the area
/// the solve is counting on.
///
/// Bisection rather than the closed form: cap area is `2π(1 − cos r)`, which
/// is sub-quadratic in `r`, so `sqrt(target / current)` — exact only if area
/// scaled as `r²` — systematically under-delivers even before the clamp
/// fires. Deterministic: a fixed iteration count, no early exit on a
/// tolerance that could differ across platforms.
fn solve_radius_scale(cratons: &[Craton], target_sr: f64) -> f64 {
    let total = |s: f64| -> f64 {
        cratons
            .iter()
            .map(|c| {
                let r = (c.radius_rad * s).min(CRATON_RADIUS_MAX_RAD);
                let peak = PEAK_MIN_KM + (PEAK_MAX_KM - PEAK_MIN_KM) * (1.0 - c.age);
                std::f64::consts::TAU * (1.0 - math::cos(r)) * continental_cap_fraction(peak)
            })
            .sum()
    };
    let (mut lo, mut hi) = (0.0_f64, 1.0_f64);
    while total(hi) < target_sr && hi < 1024.0 {
        hi *= 2.0;
    }
    for _ in 0..80 {
        let mid = 0.5 * (lo + hi);
        if total(mid) < target_sr { lo = mid; } else { hi = mid; }
    }
    0.5 * (lo + hi)
}
```

and at the call site:

```rust
    let continental_area: f64 = cratons.iter().map(craton_continental_steradians).sum();
    if continental_area > 0.0 {
        let scale = solve_radius_scale(&cratons, budget * 4.0 * std::f64::consts::PI);
        for c in cratons.iter_mut() {
            c.radius_rad = (c.radius_rad * scale).min(CRATON_RADIUS_MAX_RAD);
        }
    }
```

**Do not** reuse `craton_continental_steradians` inside the objective by
mutating radii — the objective must be pure, or the bisection reads its own
side effects.

- [ ] **Step 4: Run the test and the terrain suite**

```bash
cargo test -p hornvale-terrain --test tectonic_properties the_rescale_delivers_its_own_budget
cargo nextest run -p hornvale-terrain
```

Expected: the new test PASSES. **Other tests are expected to move** — this is a
deliberate epoch. Two that specifically encode the old behaviour:

- `continental_supply_is_the_area_the_rescale_budgets` (`crust.rs:1735`). Its
  single-pinned-craton arm asserts `supply < 0.037`, and that bound is *derived
  from the 0.6 clamp* in its own comment: `(1 − cos 0.6)/2 ≈ 8.73%` of the
  sphere × the best-case continental fraction `≈ 0.415` ≈ 3.63%. At 0.8 the same
  arithmetic gives `(1 − cos 0.8)/2 ≈ 15.16%` × 0.415 ≈ **6.29%**. Recompute the
  bound *and rewrite the comment* — a stale comment beside a corrected number is
  worse than either alone. Its default-draw arm (`0.15..=0.60`) should still
  hold as supply rises from ~0.27 to ~0.41; if it does not, report rather than
  widen it.
- `default_worlds_never_trip_the_supply_fallback` (`tectonic_properties.rs:457`)

The second is load-bearing and **must still pass**: decision 0053 chose
`SUPPLY_SHORTFALL_FACTOR = 0.5` so default worlds provably keep the
exact-percentile path. Analytic supply/quota rises from ~0.69 to ~1.10 under
this change, moving *away* from the 0.5 trigger, so the property should hold
more comfortably than before. **If it fails, stop and report** — that would
mean the change reached a regime 0053 did not anticipate.

- [ ] **Step 5: Re-run Task 1's probe and route on what it says**

```bash
cargo test -p hornvale-terrain --lib the_land_elevation_terms_attribute_their_variance -- --nocapture
```

Decision rule — this is the task's real deliverable:

| grid-realised continental fraction vs land quota | response |
|---|---|
| **≥ quota** | the coastline is at or above the shelf break. Route 3 is sufficient; Routes 1 and 2 are unnecessary. Record it and stop. |
| **0.9–1.0 × quota** | close. Record the residual mean land elevation; the greenhouse carries the rest (spec §4.1's bound already says it must). Do not chase it with `SUPPLY_SHORTFALL_FACTOR`. |
| **< 0.9 × quota** | Route 3 under-delivered on the grid. Report the number and **stop for a design call** — Route 2 is a separate re-decision of a 0053 constant, not a nudge. |
| radius CV **< 0.10** | **STOP** — variety collapsed anyway. The clamp is still binding; report before proceeding. |
| min pair separation collapses vs Task 1's baseline | repulsion is saturating at the new radii. Report the numbers; do not "fix" `REPEL_SEPARATION_FACTOR` inside this task. |

- [ ] **Step 6: Write decision 0132**

`docs/decisions/0132-the-craton-clamp-is-a-budget-not-a-limit.md`, in the form
`docs/decisions/README.md` prescribes. It supersedes nothing (0053 is about
where sea level may land, not how big a craton may be) but must cite 0053 and
0057, and must record: the 97.1% infeasibility, the CV-0.001 collapse the
obvious fix produces, and the overlap saturation at ~0.8. Re-check the number
is still free before writing.

- [ ] **Step 7: Regenerate, in the documented order**

```bash
make rebaseline
make rebaseline-goldens
git diff --stat book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/ docs/digest/ book/src/domesday/ clients/game/core/tests/fixtures/ windows/vessel/tests/fixtures/
```

| what moved | response |
|---|---|
| `book/src/gallery/` (elevation map, biome maps, almanacs) | expected — commit |
| `book/src/laboratory/` (live study rows) | expected — commit |
| `book/src/reference/` — **name the FILE, not the directory** | The directory holds two different kinds of page and the STOP applies to only one. **Contract-bearing:** `stream-manifest-generated.md`, `concept-registry-generated.md`, `concept-manifest-generated.md` — if any of these moves and you added no stream label or predicate, **STOP**. **World-derived:** `dictionary-generated.md`, `locale-seed-42.json` and the other seed-42 renders — these move on any world-output change and are expected. Task 2 tripped this: the directory moved, the three manifests were byte-unchanged, and a directory-level STOP would have halted a correct run. Task 3 is the inverse case — it *does* add a label and a predicate, so the manifests moving is expected there and the manifests **not** moving is the STOP. |
| `windows/vessel/tests/fixtures/` | expected — this is why `rebaseline-goldens` is a separate step |
| nothing at all | **STOP** — the change did not take effect, or a path has no index entry and the check is vacuous |

- [ ] **Step 8: Commit**

```bash
git add -A
git commit -m "feat(terrain)!: the craton rescale delivers its budget — exact solve, clamp 0.8"
```

---

## Task 3: The greenhouse residual — the astronomy half

Draw the residual in astronomy, commit it as a fact, expose it as a census
column. Nothing consumes it yet, so this task is provably inert on temperature.

**A correction to spec §7 the implementer should not re-derive.** §7 says the
new quantity means "a new position in stream consumption order — both
save-format contracts." **It does not.** `Seed::derive(label)` (`kernel/src/seed.rs:70`)
FNV-hashes the label string into an independent seed, and every astronomy
quantity draws from its own leg (`streams::ORBIT`, `streams::OBLIQUITY`, …).
The `stream_labels!` macro assigns no indices. So a new leg perturbs **no
existing draw**, exactly as `STAR_AGE`, `MOON_NODES` and `MOON_DENSITY` did
before it. This is additive, and the pin-isolation property holds by
construction — which is a reason to *verify* it cheaply, not to skip it.

**Files:**
- Modify: `domains/astronomy/src/streams.rs`, `src/anchor.rs`, `src/facts.rs`,
  `src/lib.rs`
- Modify: `domains/astronomy/tests/genesis_properties.rs`
- Modify: `windows/lab/src/metrics.rs`

**Interfaces:**
- Produces: `Anchor.greenhouse_residual: f64` (dimensionless, mean 0);
  fact predicate `greenhouse-forcing-k`; lab metric `greenhouse-forcing-k`.
  Task 4 consumes all three.

- [ ] **Step 1: Write the failing determinism + isolation test**

In `domains/astronomy/tests/genesis_properties.rs`:

```rust
/// claim: readout(over 100 seeds — the greenhouse residual is drawn, in
/// range, and perturbs no existing astronomy quantity)
#[test]
fn the_greenhouse_residual_is_drawn_and_isolated() {
    for seed in 0..100u64 {
        let outcome = generate(Seed(seed), &SkyPins::default()).unwrap();
        let a = &outcome.system.anchor;
        assert_eq!(
            *a,
            generate(Seed(seed), &SkyPins::default()).unwrap().system.anchor
        );
        assert!(a.greenhouse_residual.is_finite());
        assert!((-1.0..=1.0).contains(&a.greenhouse_residual));
    }
}
```

**CORRECTED 2026-08-14 — the original sketch was silent-and-green.** It read
`let s = Seed(seed); generate_star(s); generate_anchor(s, ...)`, passing the
**bare world seed**. Production does not: `system.rs:44-46` is

```rust
let astronomy_seed = world_seed.derive(streams::ROOT);
let star = generate_star(astronomy_seed);
let anchor = generate_anchor(astronomy_seed, &star, pins)?;
```

so the sketch would have drawn the residual from a different leg than any
world ever uses, and every one of its assertions — finite, in range,
deterministic — is satisfiable on that wrong world. It would have passed while
testing nothing. Going through `generate` also matches what every other test in
this file already does (`outcome.system.anchor`), and needs no new imports;
`generate_star`/`generate_anchor` are exported but are NOT currently imported
here. This is the astronomy instance of the standing rule that a domain seed is
`Seed(seed).derive(streams::ROOT)`, never bare.

Isolation is the load-bearing half, and the plan does **not** prescribe the
mutation: find a discriminating one by reading the draw order — the property to
demonstrate is that `orbit`, `obliquity`, `mass` and `rotation` are
byte-unchanged from `main` for the same seeds. The cheapest honest proof is the
artifact drift check in Step 5, not a hand-written oracle.

- [ ] **Step 2: Run it and confirm it fails to compile**

```bash
cargo test -p hornvale-astronomy --test genesis_properties the_greenhouse_residual_is_drawn_and_isolated
```

Expected: FAIL — no field `greenhouse_residual`. **A compile failure is not
evidence an assertion would fire**; it only tells you the surface is absent.

- [ ] **Step 3: Add the label, the field, and the draw**

`streams.rs`, appended to `legs` (order within the macro is irrelevant to
derivation; append for readability):

```rust
        /// Atmospheric greenhouse residual draw (The Glasshouse).
        GREENHOUSE = "greenhouse" => "atmospheric greenhouse residual draw";
```

`anchor.rs`, on `Anchor`:

```rust
    /// Dimensionless greenhouse residual, −1..1, mean 0: how much thicker or
    /// thinner this world's atmosphere is than the carbonate–silicate
    /// thermostat alone would give it.
    ///
    /// kind: **hornvale-choice** (decision 0106). The *thermostat* is physics
    /// and is cited where it is applied (`domains/climate/src/temperature.rs`);
    /// this residual is the spread around it, and no Earth datum fixes its
    /// width. It exists because a purely derived thermostat leaves temperature
    /// a deterministic function of insolation, which is what spec §4.1
    /// criterion 6 exists to falsify.
    pub greenhouse_residual: f64,
```

and in `generate_anchor`, alongside the other draws:

```rust
    let greenhouse_residual = 2.0
        * astronomy_seed
            .derive(streams::GREENHOUSE)
            .stream()
            .next_f64()
        - 1.0;
```

- [ ] **Step 4: Commit the fact and add the census column**

Register the predicate in `facts.rs` beside `INSOLATION_REL` (follow that
constant's exact shape — `PredicateDef.name` duplicates its registry key,
decision 0015), and add the `greenhouse-forcing-k` metric in
`windows/lab/src/metrics.rs` next to `insolation-rel` (metrics.rs:1266 as of
the origin/main absorption — grep `"insolation-rel"`, do not trust the line).

**Check its build rung.** `TOOL-rung-tag-unchecked` is a live defect with three
known instances, one of which *panics*. Verify the new metric in a narrow study
before trusting it:

```bash
cargo run -p hornvale -- lab run <a two-metric study selecting only greenhouse-forcing-k>
```

- [ ] **Step 5: Prove the astronomy change is otherwise inert**

```bash
cargo nextest run -p hornvale-astronomy
make rebaseline
git diff --stat book/src/gallery/ book/src/reference/ book/src/laboratory/
```

| what moved | response |
|---|---|
| `book/src/reference/` stream manifest **and** concept registry | expected — you added a label and a predicate |
| `book/src/gallery/` almanacs gain a greenhouse line | expected — the fact is committed |
| `book/src/gallery/` **elevation or biome images** | **STOP** — nothing consumes the residual yet; world geometry must not move |

- [ ] **Step 6: Commit**

```bash
git add -A
git commit -m "feat(astronomy): draw and commit the greenhouse residual (consumed by nobody yet)"
```

---

## Task 4: The thermostat — the climate half

**Files:**
- Modify: `domains/climate/src/provider.rs:33-58`, `src/temperature.rs:50-79`
- Modify: `windows/worldgen/src/lib.rs` (the composition root wires it)

**Interfaces:**
- Consumes: `Anchor.greenhouse_residual` (Task 3), post-Task-2 terrain.
- Produces: `ClimateInputs.greenhouse_forcing_k: f64`.

- [ ] **Step 1: Evaluate the curve before writing the constant**

Do **not** characterise the model from its parameters. Write a throwaway
`--nocapture` test (or a `/tmp` script) that prints `T_land` at the p5 / p25 /
median / p75 / p95 of the *actual* insolation draw, for `k` ∈ {0.4, 0.5, 0.6,
0.7} against post-Task-2 hypsometry, and paste the table into the report.
Spec §3.1's table was fitted against *pre*-Task-2 terrain and its constants are
explicitly provisional.

**Do not run a `lab run` against a study written to `/tmp`** — `lab run`
publishes into `book/src/laboratory/generated/<name>/` regardless, under a
drift-checked path where `git diff --exit-code` is vacuous
(`TOOL-lab-run-publishes-ad-hoc-studies`).

- [ ] **Step 2: Pick `k` and `Tc` from Earth, not from the census**

The anchor is Earth's land mean **+8.6 °C at S = 1**. Spec §6's circularity
constraint: Earth data may settle the anchor; the census may settle only
spread, dominance and percentile placement. Record which datum fixed each
constant in its doc comment.

- [ ] **Step 3: Implement**

`provider.rs`, on `ClimateInputs`:

```rust
    /// Greenhouse forcing in kelvin, drawn in astronomy and passed through
    /// here so climate stays a derived read (`domains/climate/src/streams.rs`
    /// records that temperature, moisture and biome are seed-free).
    /// type-audit: bare-ok(quantity: kelvin offset)
    pub greenhouse_forcing_k: f64,
```

`temperature.rs`, replacing the fixed `288.0 * scale` in the `Spinning` branch
with the thermostat. Keep the `Locked` branch's structure — the 48 locked
worlds are `CLIM-locked-regime`'s, not this campaign's — but note in the
report whether it now disagrees with the spinning branch at the same
insolation.

- [ ] **Step 4: Run the climate and worldgen suites**

```bash
cargo nextest run -p hornvale-climate -p hornvale-worldgen
```

Expected: temperature-dependent assertions move. **Risk 4 in spec §9 is now
live** — `FREEZE_C = -10.0` sits within 0.5 K of today's median, and
`HABITABLE_MIN_C`, `ICE_C`, `TEMPERATE_BASELINE_C` are on the same scale.
Warming the population moves every world across those thresholds at once.
Decision 0107 says cold ground should degrade gracefully rather than switch;
that is **an expectation to test, not to assume**. Report which of them
actually moved behaviour.

- [ ] **Step 5: Commit** (regenerate per Task 2 Step 7's branch table first)

```bash
git commit -m "feat(climate)!: the carbonate-silicate thermostat, with a drawn residual"
```

---

## Task 5: The latitude profile

**Must land with or after Task 4.** Alone it moves the census median from −10.0
to −20.0 °C.

**Files:**
- Modify: `domains/climate/src/temperature.rs:68`

- [ ] **Step 1: Write the acceptance test as a decision rule**

Spec §3.2 gives three bounds at `S = 1` with the greenhouse at its Earth
anchor, and deliberately does **not** name the functional form:

- area-weighted mean within **1 K of +14 °C**;
- equatorial value within **3 K of +26 °C**;
- polar value within **5 K of −25 °C**.

Area-weighting is the whole point of the defect: `⟨sin²lat⟩ = 1/3` over a
sphere, which is why `30 - 60·sin²` has an area-mean of **+10 K**, not 0.
Weight by `cos(lat)`.

- [ ] **Step 2: Select the form and cite the dataset**

A pure `sin²` cannot satisfy all three — Earth's tropics are flatter. The
implementer picks the form; the three bounds are the contract. **The Earth
zonal-mean dataset must be cited in the doc comment** (kind:
`earth-biosphere`, decision 0106). An uncited fit is the §6 red-flag cell that
started this campaign.

- [ ] **Step 3: Run, regenerate, commit** — Task 2 Step 7's branch table applies.

---

## Task 6: The classifier gate — re-measure, then decide

**This task's deliverable may be "no code change".** Spec §3.4 argues the
specials over-capture because their *inputs* are pathological, and both inputs
were just fixed.

- [ ] **Step 1: Re-measure the biome and soil distributions**

After Tasks 2–5, over a seed sweep large enough to be meaningful (the committed
census is stale until Task 7; use a live study).

- [ ] **Step 2: Apply the gate**

| result | response |
|---|---|
| no class > 50% **and** `dominant-soil-order` no longer frozen | the classifier needs nothing. Record it as the finding; close `CLIM-biome-classifier-mixing` as a **symptom, not a defect** |
| a class still > 50% | the classifier is an independent defect. Fix it here, with the re-measurement as the evidence |

Either way, `ICE_C = -20.0` and `tree_line_m`'s `4000.0`/`40.0` get 0106
provenance. The tree line reaches 0 m only at 100° latitude, so it never floors
on a real world while Earth's reaches sea level near 70° — a provenance defect
independent of this campaign's outcome (`F6`).

**Do not reorder the ladder.** Spec §2.3 establishes that the
`alpine`-warmer-than-`taiga` anomaly is not a ladder violation: `Alpine` is
selected on elevation at any `T ≥ −20 °C`, so it averages over nearly the whole
non-ice population. There is no temperature ladder for it to violate.

---

## Task 7: The readout, the census, and the close

- [ ] **Step 1: Report all six criteria against their frozen baselines**

Criterion 6 over **both** the full 1000 and the spinning subset.

- [ ] **Step 2: A falsified prediction is a result**

If (2) and (3) prove incompatible — every parameterization that spreads
temperature keeps a class dominant — **that is the finding**. Report it; do not
retune to rescue the prediction, and say so in the chronicle if any constant
moved after unblinding.

- [ ] **Step 3: Census regen — STOP, carve-out**

Requires Nathan's explicit authorization. Then, from the branch, pushed, with a
**full SHA**:

```bash
ssh lefford 'cd ~/Projects/hornvale && HV_CENSUS_WORKTREE=canonical \
  HV_CENSUS_REF=<full-sha> scripts/census-run.sh'
```

Goldens are committed **on lefford**. Cost from `docs/timings.md`, not from
prose. `book/src/domesday/` drifts on any census change — it is a pure read
over the census.

- [ ] **Step 4: Book DoD belongs to the campaign close, not this stage**

Chronicle entry, registry status flips (`MAP-craton-rescale-shortfall` →
`shipped`, `CLIM-greenhouse`, `CLIM-astronomy-unmeasured`, `SKY-19` and
`CLIM-cold-attractor`'s corrections), Confidence Gradient re-score, and the
retrospective — including the scratch findings still owed (F2–F6, F8, F10, F12
in the Stage A followups' promotion log).

---

## Self-review

**Spec coverage.** §3.1 → Tasks 3+4. §3.2 → Task 5. §3.3 → Tasks 1+2. §3.4 →
Task 6. §3.5 → Task 3 (the fourth column; the first three landed in Stage A).
§4 → Task 7. §5's ordering is preserved: hypsometry before the thermostat is
calibrated, the latitude profile with or after the thermostat, census last.
§6's constant kinds are attached to each constant as it is introduced rather
than swept up at the end. §7's epoch handling is in every regeneration step,
with its stream-order claim corrected in Task 3.

**Two gaps I am leaving open deliberately, not by omission.** Spec §3.3's
Routes 1 and 2 have no task: Task 2 Step 5's decision rule routes to them only
if Route 3 under-delivers on the grid, and Route 2 is a re-decision of a 0053
constant that deserves its own design call rather than a pre-written task.
And the seam-guard roster is untouched — if Task 4 makes `conquest_victim`'s
neighbours newly reachable, that surfaces in `make gate-full`, which is a
close-time run.

**Type consistency.** `greenhouse_residual` (astronomy, dimensionless −1..1) and
`greenhouse_forcing_k` (climate, kelvin) are deliberately different names for
different quantities; Task 4 is where the conversion happens and must state it.
`CRATON_RADIUS_MAX_RAD` is used in Tasks 1, 2 and their tests under that one
name.
