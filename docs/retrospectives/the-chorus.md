# Retrospective: The Chorus

One page, process only. Product story: the chronicle entry.

## What worked

- **Preregistration survived contact with its own falsification.** The
  pole-ordering criterion was frozen at plan time on a plan-invented
  aggregate (mean of loss/order/stance). T3 measured it false at seeds
  1–6; the reviewer reproduced the exact values independently; the
  amendment (onto LANG-41's own named axes, no threshold moved) was
  labeled as post-partial-unblinding in the ledger, the spec addendum,
  and the test header — three places, one story. The lesson worth
  keeping: **freeze criteria on the theory's named axes, not on
  aggregates the plan invents** — an aggregate bundles quantities whose
  poles point different directions, and the falsification was localized
  in minutes only because the dial's components were decomposable.
- **The measure-don't-narrate reviews kept earning their keep.** The T4
  reviewer found the margin-law test was a near-tautology (parsed lines,
  never compared against `chorus_ground`); the rewritten test was then
  mutation-verified — a deliberately vanished star-class fact reddened it
  by name. Same class as the project's worst historical bug (4 tests
  asserting nothing), caught at review rather than in the field.
- **The layering carried the design.** Putting the pure filter mechanism
  in `domains/language`, the derivation at the composition root, and the
  surface in `windows/book` made "differs beyond vocabulary" structural:
  the dial physically cannot see surface strings. The
  `voice_params` precedent meant T3 was transcription, not invention.
- **Zero draws, zero epoch, genesis byte-identical** — the
  Individuation's shadow posture held end to end; every seed's world.json
  untouched, the-book.md additive-only, verified per task and again at
  the whole-branch review.
- **Same-day cadence held for a fourth consecutive campaign**, with main
  absorbed twice mid-campaign (the-foresight, then the-correspondence at
  close) — both clean, both verified by measurement (book regen
  byte-identical) rather than assumption.

## What the campaign got wrong, and what it taught

- **The census-schema red window was discovered, not planned.** The plan
  said "the census fixtures lag until the pre-merge AWS regen" as if
  schema growth were the same trade as value lag. It is not: growing
  `registry()` fails `load_rows`' exact-header check and reddens 32 tests
  in three files for the whole gap between T5 and the close-time regen.
  The strictness is correct (weakening the header check would blind the
  staleness tripwire), but the plan should have named the red window and
  its exact file list up front instead of amending mid-execution
  (ledger #14). Lesson: **a metric-adding campaign should budget the
  census-schema window explicitly in its plan** — which files redden,
  when they clear, and what "gate green" means in between.
- **The T7 dispatch was killed by a network failure mid-task** and the
  finishing agent had to reconstruct state from the working tree. The
  scratch-report/progress-ledger discipline made that cheap (uncommitted
  registry flips were verified rather than redone), but the recovery
  worked because the killed agent had committed nothing — a
  mid-commit kill would have been messier. No change proposed; noting
  that the ledger-first habit is what made the recovery boring.
- **The brief's arithmetic was wrong twice and caught twice** (a
  calibration-tie expectation at T5 plan time; the pole-ordering clause
  at T3) — both by implementers/reviewers re-deriving from live code
  instead of transcribing. The verbatim-code-plan style keeps working
  *because* the live file is declared the authority; keep saying it in
  every dispatch.

## Follow-ups (promoted from the review roll-up; none merge-blocking)

`probe_tongue`'s unused `_kind` param; duplicate `IS_A` ledger scans in
`render_volume`; `value_text`'s `Value::Entity` branch unexercised;
`chorus_ground`'s Value-variant-agnostic inclusion vs `fragment_for`'s
type match (mirror-obligation comment covers it); stance-suffix strip has
no name-collision guard (unreachable in the closed set today); a
`mean_pairwise` combinator could fold two lab helpers; `the_dial.rs`
recomputes the voice-independent null account per voice; the lab-run
tally counts rows.csv+schema.json as "charts" (pre-existing). LANG-45
(the roster as diversity budget) is the capture that should shape future
species authoring.
