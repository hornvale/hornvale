# Retrospective: The Freshet

**Closed:** 2026-07-18 · **Outcome:** merged · one-page, process lessons
only (decision 0020). Registry: DOM-5 first slice, SEQ-2 advanced.

## What happened

The campaign started from a named failure, not an abstract gap: seed 42's
first settlement sat beside a sea it could see but never drink, because
nothing in the world distinguished salt water from fresh — "where's water"
had only ever meant "below sea level." Two prior campaigns (Placement &
Drainage, and Sculpting after it) had already computed everything a
classification needed — upstream flow accumulation and an endorheic mask —
for a different purpose (carving waterfalls, condensing settlements) and
never surfaced it as a queryable truth. Three tasks: T1 built the pure
`WaterKind` classification and materialized it on the tectonic globe; T2
exposed it through `LocaleFields`, tuning the one authored threshold against
a real seed; T3 (this task) closed the campaign — chronicle, retrospective,
registry, and a small optional almanac surfacing.

## Lessons

1. **The substrate already existed; this was classification and exposure,
   not new physics.** Every line of the deliverable read fields
   (`elevation`, `sea_level`, `drainage`, `endorheic`) two other campaigns
   had computed for other reasons. That made the correctness question sharp
   and small — get the salt/fresh boundary right on an already-correct
   input — rather than diffuse across a new simulation. The Ground set this
   precedent (a classification layer over existing tectonic fields, zero
   epoch); this campaign is the second data point that the shape
   generalizes to a different substrate.
2. **A domain crate depending on `serde` fails a stricter gate than the
   general allowlist, and the failure mode is worth naming precisely.**
   T2's first attempt added `serde` to `domains/terrain/Cargo.toml` so
   `WaterKind` could derive `Serialize` for `LocaleFields`. It compiled, and
   `external_dependencies_are_allowlisted` passed — `serde` is workspace-
   allowlisted. `domains_depend_only_on_the_kernel` (decision 0002) failed
   anyway: domain crates may depend on the kernel and *nothing else*, a
   strictly narrower rule than the workspace-wide allowlist. The brief had
   already named the fallback (a locale-side `serialize_with` shim), so the
   fix was fast, but the near-miss is worth naming for the next task brief
   that proposes a new workspace dependency inside `domains/*`: check the
   domain-specific test, not just the general one, before writing the code.
3. **The tuning constant needed a real seed, not a guess, and the first
   guess silently produced zero evidence rather than an error.** T1 carried
   forward a placeholder `RIVER_MIN_DRAINAGE = 20.0`; T2's acceptance test —
   sweep 400 points on seed 42 and assert at least one reads `River` — found
   zero. A quick `#[ignore]`d probe dumping seed 42's actual land-cell
   drainage distribution (median 2, p90 11, p99 46) showed 20.0 classified
   only the top ~4% of land as fresh, sparse enough that a 400-point sweep
   plausibly misses every hit — not a logic bug, an empirically-unverified
   constant. `15.0` (~6.7% of land) gave the sweep a comfortable margin.
   The general shape: an authored physical threshold tuned against a
   *description* of a seed's distribution, never against the seed itself,
   is a guess wearing a number: reserve one cheap, throwaway probe test to
   convert the guess into a measurement before committing the constant.
4. **The keystone correctness property had a name before any code existed,
   and that made review mechanical rather than exploratory.** "An endorheic
   feeder river must read fresh; only the terminal sink reads salt" was
   fixed at spec time (the Jordan/Dead Sea example), with an explicit named
   mutation ("all endorheic = salt") that the test suite had to kill. Both
   T1's implementation and its review could check one sentence against one
   test rather than reconstruct the property from the code.
5. **The optional surfacing step (T3) was cheap because the pattern to copy
   was one campaign old and already in the tree.** The Ground's
   `ground_lines`/`AlmanacContext.ground_lines`/`"## The Ground"` triad
   was a direct template for `water_lines`/`AlmanacContext.water_lines`/
   `"## The Waters"` — new field, new builder function, new render section,
   new almanac-artifact re-baseline, all mirroring an existing three-file
   diff shape. The explicitly-skipped water map lens (a fourth PPM,
   deferred as "more rendering code than the close warrants") is the
   counter-example: a good template makes the *cheap* optional step cheaper,
   it doesn't make every optional step cheap.

## Follow-ups

Carried in the worktree's `.superpowers/sdd/followups.md`; the first two are
Nathan's explicitly-banked "B and C in time":

1. **B — named river/lake entities + settlement water-access facts** (SEQ-2:
   rivers as people-infrastructure — toponymy, trade, borders; settlements
   sited by fresh water). Genesis change → census regen + an epoch.
2. **C — closed-basin lake physics** (registry CLIM-endorheic):
   precipitation-routed lake-filling so interior drainage forms real
   freshwater lakes (wet) vs salt playas (arid); flat multi-cell salt lakes;
   brackish estuaries at river mouths; lake-effect climate feedback.
3. **The `hydrology` crate split** (registry DOM-5 full) — split rivers/
   lakes/watersheds/water-table out of `terrain/drainage.rs` once hydrology
   is people-infrastructure, not a land projection.
4. **The scene/render river layer** — rivers/lakes drawn on the Orrery
   globe; a scene-contract addition (world-wasm version bump, cross-repo
   golden refresh).
5. **Census metrics for water** — fresh-water land fraction, rivers-per-
   continent, etc., across 1,000 seeds (The Ground added five such). Needs
   an AWS census regen (carve-out — Nathan's authorization).
6. **The Surmise rewiring** — the parked `the-surmise` campaign's T5
   consumes this: `is_water` → `LocaleFields.water.is_fresh()`, then finish
   its discovery demo on real fresh water and close.
7. **Better terminal-sink detection** — the first slice uses a single-cell
   local minimum; flat multi-cell salt lakes and precise sink geometry fold
   into C.
8. **Life & settlement consuming fresh water** — MAP-7's carrying-capacity
   `K` already has a "freshwater term"; wire it (and biome/agriculture) to
   the real classification rather than a proxy.
