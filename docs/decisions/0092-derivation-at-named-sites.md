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

1. **The composition root's own build path — but NOT a crate-level allow.**
   `hornvale-worldgen` (crate `windows/worldgen`) is the library where all
   domains meet and the only place providers are constructed (Constitution
   §2.6) — every pub fn inside it that derives is itself a named
   construction site, not a caller of one. A first pass placed one
   crate-level `#![allow(clippy::disallowed_methods)]` at the top of
   `lib.rs` to cover all ~30 of these production sites at once; **review
   caught that `disallowed-methods` is a single lint with one on/off switch
   per scope**, so that allow also silenced the 24 platform-libm bans
   (decision 0041) across the crate's ~11k-line `lib.rs` and its five
   sibling modules (`vestige.rs`/`render.rs`/`graph_derive.rs`/`alchemy.rs`/
   `history_emit.rs`) — a constitutional determinism guard, disabled as an
   unintended side effect of a convenience-bucketing choice for a *different*
   lint entry. Fixed before merge: the crate-level allow is gone, and each of
   the ~31 production construction sites inside worldgen (the `_of` survivor
   bodies — `climate_of`, `paleoclimate_of`, `world_name_in`, `sky_report`,
   the almanac/history/graph accessors — plus `build_to`/`history_for`, the
   internal build path itself) carries its own function-scoped
   `#[allow(clippy::disallowed_methods)]`, the same granularity
   `kernel/src/math.rs`'s own libm-comparison test already uses. Three
   `#[cfg(test)] mod tests` blocks inside worldgen (`lib.rs`, `alchemy.rs`,
   `vestige.rs`) get a module-scoped allow instead, matching the test-fixture
   posture below. **Verified empirically, not assumed:** a scratch fn calling
   `f64::sin()` outside any allow, added temporarily to `lib.rs`, failed
   `cargo clippy -p hornvale-worldgen --lib -- -D warnings` citing "platform
   libm diverges; use `hornvale_kernel::math::sin`" — proof the libm ban is
   live again in every worldgen scope this decision's allows don't name.
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
- **`disallowed-methods` is one lint, not one-per-entry**: any scope-level
  `#[allow(clippy::disallowed_methods)]` — crate, module, or file — silences
  every configured entry in that scope, not just the one the comment names.
  Production sites therefore carry **function-scoped** allows precisely so
  the platform-libm ban (decision 0041) stays live everywhere in worldgen's
  ~11k-line `lib.rs` and its sibling modules that this decision's allows
  don't explicitly name — proven, not assumed, by the `f64::sin()` scratch
  check above. **Test fixtures are the one place this decision accepts the
  same collateral it fixed in production**: the 18 test-file, 1 example, and
  7 `mod tests` blanket allows (the file-top or module-scoped ones this
  decision authorizes for test posture) each also suppress the libm ban for
  their scope, not only the three `disallowed-methods` entries this record
  adds. That trade is accepted here — test fixtures build worlds through the
  normal generator, which already routes every transcendental through
  `hornvale_kernel::math`, so a test file has no occasion to hand-write
  `.sin()`/`.cos()`/etc. in the first place, and per-site alternatives for
  dozens of call sites per file would be pure noise. It is not free,
  though: a test file that starts calling a raw `f64` transcendental inside
  one of these blanket-allowed scopes would now compile silently. Review is
  the backstop there, the same way it always was for worldgen's own
  construction sites before this decision existed.
- Beyond the libm interaction: clippy still cannot catch a NEW derivation
  chain reintroduced *inside* one of these allowed scopes that merely calls
  `terrain_of`/`climate_from`/`demography_report_from` again (the exact
  shape of the original sin) — the lint fires on the CALL, not on a count of
  how many times a scope calls it. Function-scoping the production sites
  narrows this blind spot to the one function each allow names, rather than
  the whole crate; it does not close it.
