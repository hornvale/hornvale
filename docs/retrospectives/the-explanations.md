# Retrospective: The Explanations

One page, process only. Product story: the chronicle entry.

## What worked

- **The dial-blindness keystone was designed as ONE seam and proven at
  three altitudes.** `effective()` in `account.rs` is the only place the
  wrapper unwraps; the equality tests proved it locally; the reviewer's
  mutation (breaking the seam reddened exactly the dial-blind tests)
  proved it mechanically; the 50-seed study fixture coming back
  byte-identical proved it at population scale — run twice (T4 and the
  Kin-slot fix), cheap both times. Census-neutrality held with zero AWS
  spend, as the G3 ruling required.
- **Measure-first test arms kept the plan honest.** The
  schema-competition test and both anti-vacuity laws were written with
  two arms (the expected outcome and the surprising one), with explicit
  instructions to pin whichever measurement showed. Two arms turned out
  surprising (uniform farming subsistence at seeds 1–5; competition real
  at 3 schemas) and both got pinned as truths rather than forced.
- **The preregistered-numbers header** (priors, β roster, manner ranks,
  admissions — frozen in the plan before any measurement) meant zero
  mid-execution number disputes; the C4 lesson
  (preregister-on-named-axes) carried forward cleanly.
- **The vanishing-realizable lesson caught its second instance.** T4's
  implementer noticed the Kinship/LinkSympathy frames were unreachable
  (T3 bound agents only for Agentive — a plan deviation the T3 review
  missed); the fix wired every agent-bearing slot and added a
  reachability sweep. The C3 planet-probe lesson (a realizable thing must
  never silently vanish) is now a named review lens, and it earns its
  keep.

## What the campaign got wrong, and what it taught

- **The whole-branch review caught a CI-breaker no task gate could see:**
  T1 added two stream labels, no task regenerated the drift-checked
  stream-manifest reference page, and `make gate` does not run the
  artifact check. One `hornvale streams` regen away from a red merge.
  Lesson: **any task that touches a `stream_labels()`/registry-dump
  surface owes the corresponding generated reference page in the same
  task** — add it to the plan template's step list for label-adding
  tasks.
- **A guard arm that cannot redden shipped once more** (L2's
  floor-convergence arm passed on any single-schema collapse — the exact
  measure-don't-narrate class, in the campaign's own headline law). The
  final review caught it; the arm now panics outright. The recurrence
  suggests the lesson's operational form: **an "either measured arm"
  test's fallback arm must FAIL, not assert the fallback state** —
  a legitimate fallback state still deserves a deliberate re-derivation,
  not an inherited green.
- **The schema table's `SlotKind` and the renderer's frame table
  disagreed about LinkSympathy** (slot `None`, frame names a deity) and
  the discrepancy was patched at the consumer (`agent_bearing`'s
  documented exception) rather than the source of truth. Correct under
  merge pressure, but the table should own the truth — followup filed.

## Follow-ups

Widen L1's sweep to seeds 1..=10 (seed 10's foraging hobgoblin makes the
differ arm live today); move LinkSympathy to an agent-bearing `SlotKind`
in the schema table and delete `agent_bearing`'s exception (C7 will read
this table for evidentials — it should be honest by then); the
`beta_of` Generosity→1.5 midpoint is invented and untested (plan-addendum
note; test when a species uses it); `chorus_params::underlying()` is
one-shot vs recursive `effective` (cosmetic). LANG-42/etymology:
lexicalization-over-time deferred with its pointer recorded on LANG-38.
