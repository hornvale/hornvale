# Temperature — retrospective

**Completed:** 2026-07-13 (name-only designation per decision 0026; plan
`docs/superpowers/plans/2026-07-12-temperature.md`; second half of the
Kernel Units pair, executed immediately after
`docs/retrospectives/the-datum.md`)

**The second family cost roughly half the review loops of the first — and
the mechanism is worth naming.** Every fix loop the elevation pilot paid
for came back as a dispatch constraint here: type-audit tags written at
birth on every new pub boundary (the pilot's Task 1 fix loop, now a
standing instruction), the audit check inside the first task's gate (the
pilot's adopted process finding), the kernel `UnitError` reused instead of
re-derived. Task after task came back review-clean on the first pass. The
lesson generalizes past units: **a pilot's corrections are only worth
their cost if they are mechanically carried into the successor's
instructions** — as constraints in the dispatch, not as lore the next
executor might rediscover. The pilot/successor pair is the natural unit
for measuring whether process learning actually transferred.

**Two sessions mechanized the same policy directive on the same day — the
parallel-campaign collision surface includes policy scrubs, not just
code.** This campaign's census-policy scrub added a hard platform guard to
`regenerate-artifacts.sh` (refuse local census regeneration unless
explicitly bypassed); a parallel session mechanized the identical directive
as an opt-in flag (`HV_CENSUS=1`, skip-by-default). Both were correct
implementations of the same ruling; the close-time merge had to choose, and
chose opt-in over refuse. Nothing was lost, but the duplicated effort and
the merge decision were both foreseeable: a policy correction announced
mid-campaign to multiple live sessions *will* be mechanized more than once
unless one session is named its owner. The absorb-at-stage-boundaries
discipline already exists for code; this is a data point that it applies
with equal force to process mechanization.

**Both stale-doc finds were comments adjacent to migrated code.** A module
doc still opening with "bare `f64` by documented convention" above a
freshly-typed boundary; a doc comment describing a comparison that the
migration had re-typed one line below. The compiler enumerates signatures;
it does not enumerate prose, and prose *next to* converted code is exactly
where a reader will trust it most. A cheap task-close habit follows: after
a migration task, grep the touched crates for the old convention's
catchphrases ("bare f64", the retired waiver's name, the old type's name)
and read every hit. Both finds would have been caught by that grep in
seconds; both instead survived to the whole-branch review.

## The follow-up register

Recorded verbatim at the final review so nothing silently drops; each item
is record-and-merge, none blocks anything:

1. Harden vs document `from_offset_c` release behavior (debug_assert-only
   validation) — decide in the next family wave.
2. Emit-boundary verdict-class unification (`elevation_m`=waiver /
   `sea_level_m`=wave-3 / `temperature_c`=wave-2 on identically-situated
   serialized floats) + one qualifying clause in decision 0044 ("retired at
   compute boundaries; serialized emit copies keep edge tags").
3. `kelvin()` + `Add<TempAnomaly>` have zero non-test consumers —
   reactive-surface tension, prune-or-wait.
4. `UnitError` dedup (kernel vs paleoclimate structural twins) on the next
   family's turn.
5. worldgen cites `hornvale_paleoclimate::TempAnomaly` at one site
   (cosmetic path mix).
6. RETRO (recorded above and in the pilot's retrospective): mid-branch
   commits don't build standalone — origin-first trade; weigh per-task
   compile checkpoints vs the no-half-typed-boundary guarantee per
   campaign.
7. RETRO (recorded above): both stale-doc finds were comments adjacent to
   migrated code — add a task-close grep for old-convention phrases.

## Estimate vs reality

Five tasks, all review-clean on first pass; one plan-specified narrowing
executed as written (paleoclimate's validating `TempAnomaly::new` not
carried to the kernel — reactive-surface rule, no production caller). The
whole-branch review returned one Important finding (a doc claiming
validation and visibility the code didn't have), fixed in a single wave
with the accumulated minors. Byte-identity held throughout; the locale
window's golden temperature value (38.082618) never moved.
