# Retrospective — The Compound Word (PROC-19)

One page of process, not product. The product is chronicled; this is what
the close learned that the code does not record.

## What went well

- **A small, well-scoped follow-up executed exactly as sized.** PROC-18's
  own retrospective named this exact gap (`domains/history`'s mixed
  shape) and estimated it as "real but bounded design work." It was: one
  new macro arm (pure addition, zero risk to the 8 already-migrated
  crates), one crate migration, 2 tasks, clean on the first task-level
  review pass, no fix-and-re-review loop needed for either task.
- **The ideonomy pass on the macro-design question (decision #1) found a
  genuinely deeper abstraction without talking the campaign into building
  it.** The pass surfaced that `domains/history`'s "flat" entries aren't
  really flat peers — `GENESIS` is already the root of its own
  dynamic-only subtree — so the fully general fix is "N independent
  roots, each with an optional legs block." Naming this and explicitly
  choosing NOT to build it (a recursive tt-muncher, or an arm explosion,
  for a shape exactly one crate needs today) is the discipline this
  whole macro lineage depends on: every campaign in it (PROC-17→18→19)
  has surfaced a more general version of its own fix and banked it
  rather than built it.

## What was awkward — a documentation-accuracy issue that recurred TWICE in one campaign

- **Task 2's implementer correctly found the spec's "zero manifest diff"
  claim was wrong** — migrating `RESIDUE`/`STRUCTURES` through the new
  arm's `legs {}` block (which deliberately reuses the existing root+leg
  arm's `concat!` composition, exactly as designed) root-qualifies their
  manifest keys. This was a genuine authoring bug in the spec: §4.1's own
  worked example already showed the qualified `concat!("history", "/",
  "residue")` expansion, while §5 separately claimed "every value
  unchanged" — a claim that was true of the *constants* but not of the
  *manifest*, and I never cross-checked the two sections against each
  other before presenting the spec at G3. Correctly resolved (an ideonomy
  pass confirmed accepting the correction beats every alternative on
  every axis, and a full-workspace grep confirmed nothing downstream
  asserts the old text) — but this is the second time in two consecutive
  campaigns (see [[the-single-saying-campaign]]) that a "this generated
  artifact won't change" claim was asserted rather than checked against
  the actual regenerated output before being written into a spec.
- **The FIX to that claim was itself still inaccurate, caught only by the
  final whole-branch review, not by me.** My correction to spec §5 said
  "the generated book page's two `history/*` leg rows are the only
  content affected" — true of the *keys*, false of the *descriptions*:
  all six rows' description text was reworded (dropping `(Task 2)`/
  `(Task 3)` phase parentheticals), a change already present in the
  plan's own Task 2 replacement text, just never reconciled back into the
  spec's determinism section. **This is the same failure mode recurring
  a second time within one correction cycle**: I fixed a "diff will be
  smaller than claimed" bug by writing a new claim about the diff's
  scope, without regenerating the artifact and diffing it myself before
  asserting the new, narrower claim was complete. The whole-branch
  reviewer did what I should have done first — read the actual diff.
  **Generalizable lesson, sharper than PROC-18's version of it**: when
  correcting an inaccurate "this won't change" claim, the fix is not a
  *better* claim, it's a *diff* — paste the actual regenerated output's
  delta into the spec, don't narrate it from memory of what you think
  changed, even the second time around.

## Follow-ups

- **The general "N independent roots, each with an optional legs block"
  macro grammar** remains banked, not built (decision ledger #1) — no
  second crate needs it yet.
- **A standing process gap, not fixed by this campaign**: nothing in the
  brainstorming or writing-plans skills currently forces "regenerate the
  artifact and diff it" before a spec is allowed to assert a claim about
  that artifact's diff. Two campaigns in a row hit variants of the same
  bug. Worth a session-level note (not a code change) that the discipline
  needs to be habitual, not just documented after the fact each time it's
  caught.
