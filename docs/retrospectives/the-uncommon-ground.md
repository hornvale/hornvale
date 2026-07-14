# The Uncommon Ground — retrospective

**Completed:** 2026-07-13 (name-only designation per decision
`slugs-not-numbers`; plan
`docs/superpowers/plans/2026-07-13-the-uncommon-ground.md`)

**Four ideonomy passes converged on a design, and the last pass was
confirmatory rather than generative — worth naming as a repeatable
pattern.** The design session ran negation × cross-domain re-instantiation,
organon-construction × abstraction-lift, and combination × dimension-
identification as three explicit, named passes over the room-scale variety
problem, each producing a concrete design delta rather than a restatement:
the negation-slot engine replacing a flat catalogue (composites coherent by
construction instead of by convention), the derived/placed split (rung 15
free, rung 30 budgeted), the founder floor borrowed from the settlement
domain's people-placement vocabulary, and the sub-cell `MicroField` as the
actual answer to "miles and miles of forest" rather than the placeholder's
lone jitter scalar. A fourth pass, run after the design's first draft, found
no further deltas — a genuine confirmatory result, not a skipped step. The
pattern worth keeping: run ideonomy passes until one comes back empty, not a
fixed count; three passes each earned their keep here, and a session that
stops at the first pass that finds nothing new is spending its budget
correctly rather than under- or over-investing.

**The terrain-signal reuse dissolved what the design spec had flagged as an
open risk.** The spec listed a per-cell tectonic-proximity accessor as a
plan-time unknown — the substrate proxy and the field-weighted budget both
need one, and it looked like new domain-crate surface. It resolved for free:
`GeneratedTerrain::globe()` already exposed `unrest`, `boundary`, `plate_of`,
`elevation`, `sea_level`, and `endorheic` (the last doubling as the endemic
isolation signal), so the campaign shipped with zero new terrain-domain
API — the one small kernel addition was `Geosphere::hops_between`, a bounded
BFS for the placement repulsion radius, which is layering-neutral (kernel,
not a domain). The lesson generalizes past this campaign: an "open question"
flagged at design time is worth a five-minute grep against the actual
provider surface before it is treated as new scope in the plan — it
collapsed a task's worth of anticipated work into a read.

**The type-audit's enum-over-tagging mistake recurred across three tasks
before the pattern was named plainly enough to stop tripping people.**
Task 2's review caught the plan's own example code tagging enum-typed
fields (`StrangeSite.energy`, `StrangeSite.kingdom`) with position-qualified
type-audit tags meant only for tracked bare primitives (`f64`/`f32`/
integers/`bool`/`char`/`String`/`str`); enum- and struct-typed fields get no
tag at all, and unit-enum declarations get none either. The correction was
recorded as a lesson for Tasks 5 and 6 explicitly, and it still needed a
second correction pass in Task 6's review — the plan's example code was the
recurring source, not the implementers' judgment, which argues the fix
belongs one level up: a plan that includes example code touching the
type-audit should have that example code checked against the tool's actual
rules before the plan is handed to an executor, not caught fresh in each
task's review.

**Clippy discipline slipped for two tasks before the gate caught it, and the
project's existing rule (cost-order fmt/clippy first, every task not just
the gate) is the right rule — it just was not followed.** Tasks 3 and 4 each
added `pub(crate)` functions with no non-test caller yet (the substrate
proxy and the micro-field, both landing ahead of the `describe` integration
that would call them), which is a latent `-D warnings` break under
`clippy::dead_code`-adjacent lints. Task 5 caught it only because it
happened to run the full gate; had Task 5 also scoped its check to one
crate, the break would have ridden all the way to the workspace gate before
surfacing. The interim fix (scoped `#![allow(dead_code)]`, removed once
Task 7's integration added the real callers) was clean, but the miss itself
is the same category of lapse the project's own process guidance already
warns against — clippy belongs in every task's own verification, not
deferred to whichever task happens to run `make gate` first.

**Task 7 scoped its tests to the locale crate alone and silently broke the
CLI, which Task 8 caught only because it happened to build the whole
workspace.** The schema bump (`SubCellTexture` → `Regime`) dropped a field
`cli/src/main.rs` still referenced; `cargo test -p hornvale-locale` stayed
green throughout because the break was entirely in a downstream consumer
crate the scoped test never touched. This is the sharpest process lesson of
the campaign: a task that changes a public shape consumed outside its own
crate is an *integration* task by the shape of its change, whether or not
its brief calls it one, and integration tasks need a full-workspace build as
part of their own verification — not as a hope that a later task's broader
scope will happen to notice.

**The task-review loop earned its cost again, catching two Criticals a
single-platform local run would not have.** Task 6's review found a raw
`f64` comparison in the budget's accept/reject path that would have diverged
cross-platform under the exact discrete-decision-on-continuous-value trap
decision 0041 exists to guard against, and a founder-floor overwrite bug
that would have silently defeated the guarantee the founder floor exists to
provide (a later competitive-placement pass could claim a founder's reserved
cell for a different regime, leaving the "the land geologically implies a
vent, so it gets one" promise unmet without any test failing to say so).
Both were fixed and verified in the same task, and neither would have shown
up in a `cargo test` run on one platform against one seed — they needed an
adversarial read of the logic, not more test iterations of the same logic.

## Estimate vs reality

The ten-task plan ran close to estimate with no task reordering. Three tasks
(2, 3, 6) each needed one fix-and-reverify cycle inside their own commit
range rather than a separate follow-up task, and none of those fixes touched
an already-closed task's surface. The heavy census battery (25 seeds,
~196s), correctly tiered into `make gate-full` rather than the commit gate
per the project's existing fast-gate discipline, passed on its first
post-fix run with real assertions (Task 6's review had also flagged the
census's first draft as tautological — asserting properties that could not
fail given how the test was constructed — and the rewrite that followed
checks budget-minority, determinism, and distinct-cell placement for real).
No Confidence Gradient bet clearly moved: the placed tier's field-weighted
acceptance and founder floor are a *consistency* mechanism — a generated
detail either respects the fields and the repulsion budget it is committed
to, or it does not, which the tests already check — but it operates at
world-genesis time against fields alone, not against a large committed
event ledger with aesthetic requirements layered on top, which is the
harder half of the "refinement at scale" bet the open-questions chapter
still holds open. The chapter is left as is rather than re-scored on
invented grounds, consistent with how the previous campaign's retrospective
handled the same judgment call.

## Close note — integration cadence

The campaign ran as a single continuous session, so the branch met `main`
for the first time at the close preflight (which flagged one unabsorbed
commit — an unrelated `regen → AWS-only` chore). For a single-session
campaign this is the expected cadence; the stage-boundary absorption
discipline (CLAUDE.md Process) targets multi-session campaigns where main
moves under a long-lived branch. Absorbing at close was clean: no file
overlap, no slug/ID collisions, and a `SKIP_CENSUS=1` regen under main's
updated script produced zero artifact drift.
