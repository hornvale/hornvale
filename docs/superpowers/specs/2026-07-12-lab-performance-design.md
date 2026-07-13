# Lab Performance — Design

**Date:** 2026-07-12
**Status:** Approved (brainstorming session)
**Campaign:** Lab Performance (slug-named per decision 0026)
**Provenance:** Ratified insertion in the terrain-overhaul roadmap **before Sculpting** (Nathan, 2026-07-11 "agree absolutely"): Sculpting is another terrain epoch with another refreeze-per-commit tuning season, so every turnaround saving pays across its whole duration. Scope drawn from the twenty-item Crust-era performance brainstorm and the parallel-sessions/CI discussion, then **deduped against main** — several backlog items had already shipped in Crust/Fast Gate/TOOL-* work and were dropped. Five ideonomy pulls (three on the MAP-25 mechanism, two on the whole design) hardened the plan; their load-bearing findings are folded in and called out where they changed a decision.

---

## 1. Goal

**Cut lab and CI turnaround before Sculpting, without ever trading away census trustworthiness.** Every performance change here must be provably byte-identical in its outputs (worlds, metrics, artifacts) — a faster census that returns a different number is a regression, not a win. Success is measured, not asserted: Stage 1 records the baseline shares, and each later stage is judged against a byte-identity guard.

Non-goal: raw worldgen throughput at the expense of determinism, new hardware (the R720 is rejected — CPU-bound, per-core-slower), or `nextest` as the workspace gate (it rebuilds `LazyLock` censuses per process — several times *slower* here).

## 2. Vocabulary (pin the blurring terms)

An ideonomy dictionary pass exposed four words circling one concept. This spec fixes them and uses them consistently:

- **domain** — a subject area of the world: astronomy, terrain, climate, settlements, language. What a metric is *about*.
- **stage** — one step of the worldgen build pipeline. In today's pipeline, stages and domains are 1:1.
- **rung / depth** — a position in the linear build-depth ladder (§4). A build "to depth *D*" runs every stage up to and including rung *D* and no further.
- **view** — the Rust *type* exposing one rung's outputs (`TerrainView`, `FullView`, …). A metric's argument type *is* its declared depth.

"Depth ladder" and "rung" are the load-bearing nouns; "domain"/"stage" are descriptive.

## 3. Scope, and what was cut

**In scope (four stages):**

1. Measurement — a committed build profiler (§3-stage-1 / §7).
2. MAP-25 — the view-typed build-depth ladder (§4), the campaign's big lever.
3. CI path-tiering + `flock` serialization (§5).
4. Two correctness batteries riding existing censuses (§6).

**Cut, with rationale (recorded so a future session doesn't re-add them):**

- **Cross-study world sharing** — *subsumed by MAP-25.* The only refreeze studies sharing a `(roster, pins, seed-prefix)` are `census-lands-drift` ∩ `branches-family` on seeds 0..500 default; MAP-25 already builds lands-drift terrain-only, so a shared cache would only re-save the terrain sub-computation that is already the cheap part. `census-of-the-meeting` uses solo goblin rosters (no overlap); the 10k censuses run solo at author-time. Revisit only if a future study set has two *deep* studies sharing roster+seeds.
- **Terrain levers #2/#3 (precompute per-craton seeds; fBm early-out)** — *already on main.* #2 shipped in Crust `9e0dd39` (`CrustField::lobing_seeds`); #3 is the `angle >= 1.5 * radius_rad` guard in `lobed_envelope` (`domains/terrain/src/crust.rs:96–100`) that returns before `sphere_fbm01`. Pulling them in is a no-op. The remaining terrain wins are different and measurement-gated (§4, conditional sibling).
- **`#17` coarse-level screening censuses** — different worlds, screening-only, never verdicts; explicitly parked pending a fresh sign-off, not revived here.
- **Seed-parallel runner (#5)** — already shipped: `run_pin_set` is `std::thread::scope`, seed-ordered, with `parallel_run_matches_sequential` pinning byte-identity.
- **Release-mode censuses in CI** — already shipped: `scripts/regenerate-artifacts.sh` runs the drift family via `run_release`.

**Recorded as future work (out of this campaign):**

- **Terrain acceleration structure** — spatially pruning the per-sample 8–14-craton iteration. A real design (BVH / coarse grid over the sphere), not a one-liner; scope only if Stage 1 proves it dominant.
- **Predicate pushdown** — the natural MAP-25 successor (build shallow for all seeds, filter on a cheap metric, deepen only survivors). YAGNI: current studies measure all metrics on all seeds.

## 4. MAP-25 — the view-typed build-depth ladder

**The observation.** `WorldView::build_with_roster` (`windows/lab/src/metrics.rs:46`) calls `build_world_with_roster` (`windows/worldgen/src/lib.rs:1596`), which builds the *entire* world — terrain, settlements, species→languages, lexicons, deities — unconditionally. A pure terrain census (`census-lands-drift`) pays the full language/settlement generation cost for metrics it never reads. The pipeline is essentially **linear**: astronomy (sky/genesis) → terrain → climate → settlements → language/naming/deities. Earlier stages never read later ones — a shallow build simply commits fewer facts; the terrain facts it does commit are byte-identical.

**The abstraction (ideonomy lift).** MAP-25 is **projection pushdown** — the lab's selected metrics are a `SELECT` of columns; today's full build is `SELECT *`. Push the projection down so worldgen never materializes unread columns.

**The rung ladder.**

```
Astronomy  <  Terrain  <  Climate  <  Settlements  <  Language
```

Climate is a distinct rung, not folded into Terrain: a coastline metric reads terrain only, while a habitability/moisture/temperature metric needs climate (reconstructed from terrain + insolation). The spec's implementation stage **must enumerate the exact rung of every existing metric** in `windows/lab/src/metrics.rs` — a metric mapped to the wrong (too-shallow) rung is the one hazard this design exists to eliminate, so the mapping is reviewed explicitly, not inferred.

**Mechanism — depth carried by the view type (not a hand-declared enum).** An ideonomy pass rejected a `min_depth` enum field on each metric: it is an assertion the compiler cannot check, and it drifts as extractors are edited. Instead:

- Worldgen exposes **narrowed view types** in a subset relation — `AstronomyView ⊂ TerrainView ⊂ ClimateView ⊂ SettlementView ⊂ FullView` — and a `build_world_to(depth)` entry that stops the pipeline at a rung and returns the narrowest view for it. (Names are plan detail; the subset/`From` relation is the contract.)
- Each metric extractor takes **the narrowest view type it actually reads** as its argument. A metric physically *cannot* read a domain its view type does not expose — **under-building becomes a compile error, not a wrong number.**
- The runner computes the narrowest view satisfying every selected metric of a study (the max rung over the selection) and calls `build_world_to` once at that depth. Deeper views coerce to shallower ones for free (superset → subset), so a mixed study builds to its deepest metric and hands each extractor its own narrower view.

**Astronomy-split, deferred to measurement.** The true data-dependency graph is not a perfect chain — `terrain::generate` takes no sky input, so astronomy and terrain are independent, and a strict linear ladder makes a terrain census pay one genesis it never reads. Whether that matters is empirical (genesis is likely cheap next to terrain fBm). Keep the ladder **linear** for now; Stage 1's profile decides whether to split astronomy off as an incomparable branch. Do not build the DAG speculatively.

**Correctness guard.** A metamorphic test asserts, per study, that depth-scoped metric values are **byte-identical** to full-build values (`build_world_to(required) metrics == build_world_to(Language) metrics`). This is the acceptance gate for Stage 2.

**Conditional terrain sibling.** `strongest()` (`crust.rs:400`) is recomputed independently by `thickness_at`, `age_at`, and `continental_at` (the last via `thickness_at`), so a cell read for all three pays up to 3× the full craton sweep. Collapsing to a single `strongest(p)` sweep per cell is small and provably byte-identical. **Add it to Stage 2 only if Stage 1's terrain profile shows the double/triple read is real and material.** Otherwise leave terrain untouched.

## 5. CI path-tiering and `flock` serialization

**Path-tiering (denylist, fail-safe).** The expensive CI step is "Artifacts are current" (`.github/workflows/ci.yml`), which regenerates ~2000 release-mode census worlds plus every map on *every* push and PR — including PRs touching only prose or the JS clients. The drift regen is a **correctness gate**, so the filter must never wrongly *skip* a real world change. Direction: **denylist** — run the drift regen *unless* the diff touches only known-inert paths; an unknown/unclassified path defaults to running.

- **Principle:** a path is *inert* only if it cannot change any drift-checked artifact. Every generated dir the drift check itself covers is therefore **carved out of the inert set** — a diff that touches a generated file directly must still run the regen, so the drift check can confirm the committed bytes match a fresh regeneration. The authoritative generated set is `ci.yml`'s own `git diff --exit-code` argument list (currently `book/src/gallery/`, `book/src/reference/`, `book/src/laboratory/`, `clients/orrery/testdata/`, `docs/audits/`); the tiering step must derive its carve-out from that same list so the two cannot drift apart.
- Inert set (skip iff the diff is entirely within): `book/**`, `docs/**`, `clients/**` (their bundles have their own CI jobs), `**/*.md`, `LICENSE`, `README` — **minus** the generated dirs above. Concretely: prose/specs/decisions under `docs/` (not `docs/audits/`), narrative book pages (not `gallery/`, `reference/`, `laboratory/`), and client source (not `clients/orrery/testdata/`).
- Implemented as a `git diff --name-only` bash step gating the drift regen — **no new CI action/dependency** (matches the repo's bash-script idiom; a third-party `paths-filter` action is declined per the no-new-tools rule). Sharing the generated-path list with the drift check (not hard-coding it twice) is a plan requirement.
- The cheap gate (`cargo test` / `fmt` / `clippy`) still runs unconditionally; only the drift/artifact regen is tiered.
- No "main always full" backstop: because an unknown path already over-runs, skipping on main is safe by the same filter logic. (If the filter is ever proven wrong, the fix is to widen the run set, not add a backstop.)

**`flock` serialization.** Concurrent local sessions running `make gate`/`make rebaseline` contend on CPU; the worktree-timeout finding measured *contention beats serialization*. Wrap the `gate` and `rebaseline` recipe bodies in `flock` on a repo-local lockfile, with a **visible "waiting on lock" message and a timeout**, so a session queued behind another's long census is legibly waiting rather than mysteriously hung. `gate-fast` (the iteration tool) is *not* locked — it is intentionally the un-serialized fast path.

## 6. Correctness batteries (ride existing censuses)

Both are free riders on worlds a census already builds — no new world generation.

**census-piggyback invariants.** Assert cheap universal properties over every world a census builds (no NaN in any numeric metric; thickness ≥ 0; monotone percentiles; refusal ⇒ all-Absent; …). A negation pass flagged that "free" hides a new failure surface: an invariant violation would otherwise block a refreeze mid-investigation. Therefore the invariants live in a **separate test target** (not inside the census/`render_csv` write path), are **O(1) per world** so "free" stays true, and *gate* (fail the suite) without preventing artifact regeneration.

**pin-reaffirmation battery.** For each pin × seed sample: read the value the *unpinned* world drew (Crust's pin-metered genesis notes expose it — e.g. `pinned continents N (seed draws D)`), pin that exact value, and assert the resulting world is **byte-identical** to the unpinned one. Proves pin isolation (a pin set to the drawn value is a no-op) across pins × seeds. The implementation stage enumerates which pins are metered (coverage is bounded by what the notes expose) and records any un-metered pins as gaps rather than silently skipping them.

## 7. Stage 1 — the profiler (fail-fast, committed)

Instrument `build_world_with_roster` with `std::Instant` spans per stage, run once over a representative seed sample, and record the shares. It answers three questions before any optimization is written:

1. **Validates MAP-25's premise** — terrain censuses really are dominated by the language tail (else the ladder's shape changes).
2. **Sets the astronomy-split** — is genesis cheap enough to keep the ladder strictly linear (§4)?
3. **Gates the terrain sibling** — does terrain's internal split (fBm share, `strongest()`-recompute cost, per-cell craton-iteration cost) justify the single-sweep collapse (§4)?

**Readout (2026-07-12; release, 24-seed build / 8-seed terrain).** Per-stage shares: astronomy **0.0%**, terrain **24.5%**, climate+settlements **49.5%**, culture+religion+species **0.0%**, deep-time **26.0%**. (1) **Premise validated, reframed:** a terrain census skips ~75% by building to Terrain depth — but the skippable mass is climate+settlements + deep-time, *not* a "language tail" (the per-species lexicon build sits inside settlement naming, so culture+religion+species is ~0%). The distinct Settlements rung is justified: it skips deep-time's 26%. (2) **Astronomy-split dropped** — genesis is free (0.0%); the ladder stays strictly linear. (3) **`strongest()`-collapse dropped** — the triple-read is 19.4% of the terrain stage worst-case but production does only a double-read (~13% ≈ 3% of a full build), on frozen `CrustField` code; not worth the risk. See the plan's "Stage 1 Readout" for full numbers and the `pin_enumeration` free-rider finding.

**Kept committed, not thrown away.** A direction/decay pass noted the profile *decays* as later epochs add stages; a committed `--profile` mode (or equivalent) stays honest across Sculpting and pays for itself again there. Behind a flag, zero cost on the normal build path.

## 8. Sequencing and success criteria

Three independent tracks (they share no state, so they can be dispatched in parallel where useful): **A** = Rust worldgen+lab (Stages 1–2); **B** = CI/infra (Stage 3); **C** = correctness (Stage 4).

### Stage 1: Measure
**Goal:** committed build profiler; baseline shares recorded.
**Success:** per-stage and terrain-internal shares reported for a representative seed sample; astronomy-split and terrain-sibling questions answered with numbers.
**Tests:** profiler behind a flag adds no fact/output change (byte-identity of the normal build preserved).
**Status:** Not Started

### Stage 2: MAP-25 view-typed depth ladder
**Goal:** narrowed view types + `build_world_to(depth)`; every metric on its rung; runner builds to the narrowest satisfying view. Conditional `strongest()`-collapse if Stage 1 justified it.
**Success:** each refreeze study builds only to its deepest metric's rung; `census-lands-drift` builds terrain-only.
**Tests:** metamorphic guard — depth-scoped metrics byte-identical to full-build, per study; under-building is a compile error (a metric cannot name a domain outside its view type).
**Status:** Not Started

### Stage 3: CI path-tiering + flock
**Goal:** denylist drift-regen tiering via `git diff`; `flock`-serialized `gate`/`rebaseline` with visible wait + timeout.
**Success:** a prose-only or client-only PR skips the drift regen; a worldgen-touching PR runs it; concurrent local gates serialize instead of contending.
**Tests:** the `git diff` filter classifies representative diffs correctly (inert-only ⇒ skip; any world path ⇒ run); an unknown path ⇒ run.
**Status:** Not Started

### Stage 4: Correctness batteries
**Goal:** census-piggyback invariants (separate O(1) target); pin-reaffirmation battery.
**Success:** invariants run over an existing census's worlds and gate; pin-reaffirmation covers the metered pins × a seed sample.
**Tests:** an injected invariant violation fails the battery without blocking artifact regen; a pin set to its drawn value yields a byte-identical world across the sample.
**Status:** Not Started

## 9. Risks and mitigations

- **A metric mapped to too shallow a rung** → the view-type design converts this to a *compile error*; the metamorphic guard (§4) is the runtime backstop. The explicit per-metric rung review is the primary control.
- **Denylist filter wrong (a world path treated as inert)** → fail-safe by construction (unknown ⇒ run); the inert set is small and conservative, and any miss over-runs rather than shipping stale artifacts.
- **`flock` head-of-line blocking** → visible wait message + timeout; `gate-fast` stays unlocked as the fast path.
- **Piggyback invariants slow the census** → O(1)/world requirement; separate target keeps them off the write path.
- **Stage 1 disproves the premise** → fail-fast: the ladder's shape (or the whole MAP-25 case) is reconsidered *before* Stage 2 is built, not after.
