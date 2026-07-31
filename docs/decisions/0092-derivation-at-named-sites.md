# 0092. Derivation happens at named construction sites; readouts take artifacts

**Status:** Accepted (2026-07-31) · **Decider:** Nathan · **Relates to:**
[the-shuttle's chronicle](../../book/src/chronicle/the-shuttle.md),
`docs/superpowers/specs/2026-07-31-the-weir-design.md`

## The principle

A world's terrain sculpt and its coexistence-stack fit are each expensive —
more expensive than building the world they derive from. Every prior
campaign that fixed a caller re-deriving one of these (The Shuttle fixed the
callers) left the convenience wrapper that had invited the mistake still
standing, so the next feature was one innocent-looking call away from
recommitting it. The Weir states the rule the fixes were each implicitly
reaching for: **derivation happens at named construction sites; a readout
takes already-built artifacts, never a bare world.** Absence — deleting the
`_of`-shaped convenience forms outright — is the strongest way to make the
slow path stop being representable; a lint is the mechanical backstop for
the forms that must still exist somewhere.

## The mechanism

`clippy.toml`'s `disallowed-methods` list (the same mechanism 0005's
HashMap/HashSet ban already uses) now bans, workspace-wide:

- `hornvale_worldgen::terrain_of` — sculpts a full tectonic globe.
- `hornvale_worldgen::climate_from` — fits climate over a terrain.
- `hornvale_worldgen::demography_report_from` — fits the coexistence stack,
  the campaign's motivating cost (`Session::start` was reaching it three
  times per session before Stage 2).

A call site compiles only behind a scoped `#[allow(clippy::disallowed_methods)]`
carrying a one-line justification citing this decision. **The `#[allow]`
attribute at the site IS the sanctioned-site list — one source of truth,
greppable by the attribute, never a second document to keep in sync.**

**Mechanism verified, not assumed.** At spec time a probe entry
(`hornvale_worldgen::terrain_of` alone) fired on 87 call sites across
worldgen pre-migration, including internal `crate::terrain_of` calls —
clippy resolves to the canonical path regardless of re-export shape, so the
`cli/tests/architecture.rs`-style source-scan fallback the spec's §6 held in
reserve was not needed. Post-migration (Tasks 1-4 having deleted the
derivation-embedding `_of` wrappers and threaded the shared fit through
`Session::start`), the three entries above fire on 292 call sites workspace-
wide with `--all-targets`. Every one resolved to one of two outcomes:

1. **The composition root's own build path.** `hornvale-worldgen` (crate
   `windows/worldgen`) is the library where all domains meet and the only
   place providers are constructed (Constitution §2.6) — every pub fn inside
   it that derives is itself a named construction site, not a caller of one.
   `mod` brings `vestige.rs`/`render.rs`/`graph_derive.rs`/`alchemy.rs`/
   `history_emit.rs` into the same crate as `lib.rs`, so **one crate-level
   `#![allow(clippy::disallowed_methods)]`** at the top of `lib.rs` covers
   the composition root's entire internal build path — the same bucketing
   this record uses below for `WorldComponents::assemble`. External crates
   get no such allow: the lint still catches a NEW embedded derivation added
   outside worldgen, which is the actual target of the rule.
2. **A named construction site outside worldgen**, each carrying its own
   scoped `#[allow]` with a one-line justification: `Session::start`
   (vessel, the motivating fix — one sculpt, one fit, threaded to every
   consumer), `LocaleContext::build` and `SceneContext::build` (the
   book-entry-point wrapper pattern — derive once, delegate to a `_from`
   twin), the book window's four entry wrappers (`render_volume`,
   `reckoning_at`, `esoteric_lines`, `parse_context`), lab's view-chain
   build path (`WorldView`/`TerrainView`/`ClimateView`'s `build`/
   `build_with_components`/`build_to*`, `simulate_world`) and two metric
   extractors that deliberately recompute the demography fit per read
   against an already-built view's artifacts (documented in each metric's
   own `doc` string, not an oversight), CLI command handlers (`render_dictionary`,
   the REPL's `run`, `cmd_map`, `cmd_paleo_map`), and test fixtures — a
   file-top (or, inside a mixed production/test file, a `mod tests { #![allow(...)]
   }`) blanket allow, since a test deriving its own artifacts once per case
   is exactly the sanctioned posture, not a regression.

No hit required a missed-migration fix; Tasks 1-4 had already done that
work. The **rejection demonstration** (spec §4.1): a scratch fn added to
`cli/src/main.rs` calling `world_builder::terrain_of(world)` with no
`#[allow]` failed `cargo clippy -p hornvale --all-targets -- -D warnings`
with:

```
error: use of a disallowed method `hornvale_worldgen::terrain_of`
  --> cli/src/main.rs:23:20
   |
23 |     let _terrain = world_builder::terrain_of(world);
   |                    ^^^^^^^^^^^^^^^^^^^^^^^^^
   = note: derivation entry point (decision 0092): sculpts a full tectonic
     globe; call only from a named construction site with a scoped #[allow]
     citing 0092
```

The scratch fn was deleted immediately after; `git status` confirmed the
revert left no trace. The lint compiles a new embedding readout to a
rejection, not a silent cost regression.

## The `assemble` ruling (closes The Shuttle's deviation)

`WorldComponents::assemble()` was considered as a fourth lint entry
(`hornvale_worldgen::WorldComponents::assemble`) but every non-test call
site outside worldgen's own composition root — `lab`'s five `View::build`
rungs plus `runner.rs`, `Session::start`, `vessel`'s liveness helper, the
CLI's `proto`/`main` — is a legitimate, independent registry construction,
and the test-fixture count alone runs past 50. Threading `wc` through every
`_from` signature to avoid these calls is churn without a measured win:
registry assembly (species + culture + religion) measures **0.032 s**, and
even `Session::start`'s own profile (spec §1) shows the world build itself
at only 11.4% against the fit's 62.5%. **Ruling: `assemble` is
lint-exempt-by-cost — dropped from the `disallowed-methods` list rather
than added.** This is not a gap in the mechanism; it is the explicit,
measured acceptance The Shuttle's retrospective flagged as an open
deviation, now resolved: `assemble` stays per-readout, accepted-with-
measurement, and does not need a named-site discipline because its cost
never rose to the level (a sculpt, a fit) that motivated one.

## Consequences

- A new readout that wants `terrain_of`/`climate_from`/`demography_report_from`
  must add a scoped `#[allow]` naming this decision — a compile-time
  speed bump proportional to the review a re-embedded derivation deserves,
  not a silent recurrence of the sin The Shuttle fixed once already.
- The sanctioned-site list lives in the source, not in this document: `grep
  -rn "disallowed_methods" --include=*.rs` finds every one, and a future
  audit of "is this still the right list" reads the greps, not a
  hand-maintained registry that can drift from what actually compiles.
- The composition-root crate-level allow is a known, accepted blind spot:
  clippy cannot catch a NEW triple-fit reintroduced *inside*
  `hornvale-worldgen` itself (the exact shape of the original sin). Review
  remains the backstop there, same as it always was for that crate's own
  internal derivation choices.
