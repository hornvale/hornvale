# Retrospective — The Benchmark

**Campaign:** the elevation datum · **Closed:** 2026-08-06 · **Branch:**
`the-benchmark`, 16 commits · Spec
`docs/superpowers/specs/2026-08-06-the-benchmark-design.md`, plan
`docs/superpowers/plans/2026-08-06-the-benchmark.md`.

Process lessons only. The product is in
[the chronicle](../../book/src/chronicle/the-benchmark.md).

## 1. Every defect was in the plan text. Four for four.

Not one implementer wrote a bug. All four defects were mine, in the spec or the
plan, and each was caught by something *downstream* of the document that
asserted it:

| defect | caught by |
|---|---|
| retyping `Sub` — subtraction is polymorphic in meaning | the compiler, 21 errors |
| spec §8.3 specified a test that could never pass | reading it again while writing the plan |
| the self-consistency guard was vacuous | a deliberate mutation test |
| Task 3's verification was crate-scoped, leaving the branch red — a **missed golden re-pin** | running a wider suite |

This is a pattern the memory already carried and this campaign did not escape.
The transferable part is *what caught them*: in every case an executable check,
never a re-read. The plan-review step I ran found the second defect but not the
first, third, or fourth — because plan review checks a document against a
document, and three of these were claims about the *world* that only running
something could refute.

**Do:** for each load-bearing claim in a plan, name the command that would
falsify it, and run that command at drafting time. The autopilot skill already
demands this for generated-artifact claims; it should extend to claims about
*type systems and test coverage*, which are equally checkable and were equally
wrong here.

## 2. A guard can be worthless and green, and only a mutation shows it

The self-consistency test — *the emitted band equals the band of the emitted
height* — is a good assertion. It was also completely vacuous, because the
field it reads is emitted on exactly one cell, and that cell is the one place
on seed 42 where both data band identically. Twenty tests stayed green with the
defect fully reinstated.

Nothing short of reintroducing the bug would have found this. Not review, not
the assertion's own wording, not the fact that it was written specifically for
this defect and named after it.

**Do:** for a test written to catch a *specific* known defect, put the defect
back and watch that test fail, before the fix lands. "Require RED" is already
in the local vocabulary (The Timekeeper); this extends it — a RED that comes
from a compile error is not evidence the assertion discriminates.

**Do:** where a test's power depends on which data it happens to sample, assert
that the sample is discriminating. The guard now fails loudly if its probe room
ever stops distinguishing the two cases, which is the failure that just
occurred.

## 3. A deferral whose trigger is a code property has no watcher

Decision 0044 deferred this type on a condition written into the source: *a
height-above-a-datum earns its own type only if it crosses a pub boundary.*
Well-formed, cited, and correct. A later campaign met the condition; nothing
noticed; the wrong datum reached a published schema and was tabulated there as
contract.

The deferral was not sloppy. It was unwatched. Filed as
`PROC-deferral-needs-a-watcher`.

## 4. The `_m` suffix defeated the survey

The spec's site survey listed `domains/climate/src/biome.rs:302` as a
non-breaking bare-`f64` site. Its parameters are named `elevation_m` and
`sea_level_m` — and are typed `ReferenceElevation`. A grep for the *shape* of
the expression read the suffix as the type.

This is the "grep-derived plan is only as complete as its grep" lesson with a
specific new mechanism: **a naming convention that encodes a unit can disguise
a type.** Where a survey's conclusion depends on an operand's type, read the
signature, don't infer from the name.

## 5. Absorption cadence held, and it earned its keep three times

Main moved three times during the campaign (The Collation; The Deep Realm; The
Panes with The Long Age — 82 commits). Absorbing at stage boundaries rather
than at close was correct, and the evidence is that **two of the three merges
were textually clean and semantically broken**:

- The Deep Realm merged with zero conflicts and did not compile — it had added
  a reader of a field this campaign renamed.
- The Panes conflicted on a *generated* golden and broke two tests by pinning a
  schema version this campaign bumped.

Had these arrived together at close, the three causes would have been tangled.
`make preflight` correctly reported GO on ancestry and slug collisions both
times and correctly said it could not score the rest.

**Do:** on a generated-artifact conflict, never hand-merge. Take one side,
regenerate from merged source, then verify *both* sides' contributions survive
— programmatically, by walking the parsed documents. Hand-merging a generated
file invents a state no code produces.

## 6. A doctrine's forecast is not a finding

`docs/design/kernel-units-doctrine.md` said this type would retire the
`elevation-convention` waiver. It retired zero of five sites, because the
correct design keeps the absolute reading beside the new height. The campaign
recorded the correction in the doctrine, with the per-site reason and an
empirical check (deleting a tag makes `type-audit` fail).

**Do:** when a campaign is chartered partly on a document's prediction, check
the prediction as an outcome. It is as falsifiable as any other claim, and
"mostly not retired, and here is why" is a better close than a quiet silence.

## 7. What the gate does not run

The client fixtures the panes decode are checked by `make vessel-check`, which
`make gate` never runs. This campaign changed those fixtures. The check was run
deliberately, from memory of the trap rather than from any prompt in the
process — the gate would have gone green either way.

Standing hazard, already in the notes; restating it because it nearly bit
again.

## Numbers

- 16 commits; `make gate` 3061 tests, 0 failures on the merged result.
- Blast radius of the rejected operator design: 21 compile errors. Of the
  adopted named conversion: 0.
- Seed 42 land relief before: 73.8% `shelf`, 1 cell `alpine`. After: 0%
  `shelf`, 37.7% `alpine`.
- No census regenerated, no committed measurement moved, no epoch. Verified
  rather than assumed: the entire measurement layer already subtracted sea
  level.
- **Every keystone identity fixture is byte-identical to main's tip** —
  `world-seed-42.json`, `pre-branches-seed-42-world.json`, and the
  pre-branches almanac all diff to zero lines. The close's refreeze step was
  therefore a no-op, and that no-op is the strongest available evidence for
  the no-epoch claim: a campaign that touches the presentation layer and
  nothing else should move no world byte, and this one demonstrably did not.

## Pin discipline at close

One miss. Ten of the eleven commits that drifted an artifact re-pinned it in
the same commit. `d428de5b` (the locale field) did not, because its
verification was scoped to `-p hornvale-locale` and the drifting golden lives
in `hornvale-vessel`; the branch sat red until `83e46282` chased it. The rule
held everywhere the drift and the test were in the same crate, and failed the
one time they were not — which is the actual shape of the hazard, and a better
guide than "remember to re-pin."
