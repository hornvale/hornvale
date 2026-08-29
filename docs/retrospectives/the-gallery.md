# The Gallery — retrospective

Process lessons, not product; the product is
[the chronicle](../../book/src/chronicle/the-gallery.md); the decisions are
0406–0411.

## The headline: one failure shape recurred eleven times, twice inside its own fix

This campaign's most expensive recurring defect was never a design choice —
it was a comment, a test name, or a registry row describing what code used
to do, left standing after the code changed under it. Eleven sightings
across twelve tasks, tracked in `progress.md` as they were found. The two
most useful sightings are the ones where the *remedy itself* went stale:

- Task 6 generalized correctly from its own reviewer's finding: a comment
  describing *what* happens is a different failure from one describing
  *when* it happens, because the second kind goes stale in a file the
  change never opens — adjacent to the thing the change is ABOUT, not
  adjacent to the diff. The remedy it carried forward: after changing when
  something happens, grep the whole crate for prose describing when it
  happens.
- Task 10's own sweep of its own change searched `per keypress|per redraw|
  every keypress|every redraw` and missed a line reading "is re-derived" —
  true words describing a mechanism that no longer existed, invisible to a
  grep built from the words a fresh reader reaches for.
- Task 11's sweep, dispatched with that exact lesson stated explicitly,
  searched `no creature is placed` and missed a line reading "no creature
  arm at all" — the identical failure, immediately, inside the fix for the
  previous instance of it.

**The remedy that actually held, found only at Task 12:** grep the phrases
the SPEC uses to describe the before-state, not the phrases a fresh reader
would reach for. A spec that quotes a defect's exact wording when describing
what it fixes hands the sweep its own search string; two of this campaign's
specs did exactly that, verbatim, and the method found both remaining known
instances plus one more nobody had looked for, in a file no other task in
this campaign had opened (`windows/worldgen/src/delve_seating.rs`, a
different subsystem entirely, asserting as current fact two claims about
`describe_underground_here` and the underground verb roster that Tasks 4
and 5 had already falsified). Grep-the-crate is a habit; grep-the-spec's-
quoted-wording is a method, and the difference is why one worked and the
other didn't, twice.

## A controller commit red the branch for every implementer, and the failure was self-inflicted

Task 1 blocked on round 1 for a reason with no relation to Task 1's own
work: the controller had committed a captured idea (`MAP-underworld-
traversal-grammar`) at 3,194 characters against a 600-character registry
cap, then made it *longer* fixing an unrelated correction. `pre-commit` runs
the full gate-commit, so nothing could land on the branch at all — not just
Task 1's work, anything's. The implementer diagnosed it correctly (ran the
one failing test in isolation, confirmed it was pre-existing, declined
every available shortcut — bypassing the hook, editing Nathan's captured
content to fit) and reported complete with the blocker named.

**Lesson: gate a controller's own docs commits before dispatching an
implementer against the branch, not after.** A controller edit is not
exempt from the same gate an implementer's work is held to, and it is
uniquely able to block *everyone* if it lands broken, since every
implementer's own commit inherits `pre-commit` on top of it.

## A rule that argued against itself, and the reasoning error worth keeping past the fix

Task 0's flooded-cell measurement was sound (60 real seeds, production
inputs, no invented depths) and its first-picked rule was not: routing
flooded cells into the `submerged` band, chosen by matching the branch's
*name* to the situation without checking what that band could actually do
— it has no lateral geometry at all, so the "fix" would have made most of
every rung unwalkable, the exact outcome the same measurement had just
rejected under a different name.

**Lesson, stated generally because it will recur under a different rule
next time:** a candidate that names a mechanism must be checked against
what that mechanism can currently do, not against what its name suggests it
should be able to do. Recording the rejected branch in the decision record
rather than quietly replacing it is what makes the lesson available to
whoever reaches for the same-shaped shortcut later.

## A brief that doesn't name a production function's real inputs invites a fiction that measures nothing

Found twice, the second time worse than the first. Task 0's brief said
"builds a descent through `generate_descent_for_character`" without naming
where rungs, depths, the water table, or the cave kind come from; the
brainstorm's own scratch probe had already invented literal depths and a
hardcoded cave kind, which would have measured a fiction and chosen a rule
from noise, with nothing red anywhere to catch it. Task 3's brief repeated
the identical omission — worse, because Task 3 ships the descent a player
actually walks, so a fictional input there lands in every generated world,
not just a report. Both were caught at pre-dispatch brief verification
(carrying the production recipe into the brief verbatim, with real line
numbers), which is the only thing that caught either.

**Lesson:** a brief naming a production entry point must name that
function's real, current inputs verbatim, sourced from the function
itself — not described from memory of what such a function would plausibly
need. This is cheap to check and expensive to skip: both instances were
caught for the cost of one `grep`, and either one landing unchecked would
have cost a re-measurement or a re-generation across every affected world.

## Two honest findings that changed the plan rather than banked a convenient answer

- Task 10 measured its own fix at ~5%, not the 24x its plan's own
  motivating figure implied, said so plainly, and explained the gap (a
  different comparison had been measured the first time — walk-band-on vs
  off, not parse-vs-no-parse). That honesty is what earned a second round,
  which measured the real cost (`purview(0)`, called fresh on every
  redraw) and produced this campaign's best number: 90x, from caching what
  round 1's own diagnosis had already pointed at.
- Task 11 redesigned an energy-gating formula and framed the change by its
  outcome in its own summary ("mechanically incapable of ever letting X
  outrank Y") — exactly the framing decision 0016's preregistration
  discipline is suspicious of. Reading the doc comment rather than the
  summary showed the change was structural (an operator that can only
  shrink a score cannot satisfy a spec requirement that two chambers with
  different dominant energy differ in what they can hold), and the
  reviewer independently hand-recomputed the disputed scores to three
  decimal places rather than accepting the argument. **A summary framed by
  outcome over a body reasoned by principle is easy to reject on the
  framing alone — read the body before judging the framing.**

## A generated artifact with authored inputs is stale-by-default, and the guard cannot see it

`docs/audits/system-coverage-wolverson-2021.md` is generated, so it drift-
checks clean forever if nobody edits the AUTHORED verdicts in
`systems/wolverson-2021.system.json` that feed it — three items cited a
registry row this campaign resolved, and would have kept reporting a
resolved gap as open, with every check green, had nobody re-scored them by
hand. The resolver does enforce re-scoring mechanically the moment the
cited registry row's status changes (`DEFERRAL_FALSIFYING_STATUSES`), which
is a real guard — but only for the citation going stale, not for a verdict
becoming *optimistic* rather than merely *outdated*. Re-scoring two of the
three items to `present` and finding the third's real render limitation
(no camera-follow for a level wider than the fixed plate) needed reading
the actual render path, not just checking the anchor resolves. **A drift
check on a generated file only ever proves the generator ran on its current
authored input — it says nothing about whether that input is still true.**

## Carried forward (from `.superpowers/sdd/followups.md`, which dies with this worktree)

- **The deepest rung's `StairsDown` leads nowhere.** `place_connections`
  sets it unconditionally, gated correctly to a physical refusal
  (`STAIRS_LEAD_NOWHERE_REFUSAL`) rather than a panic. Fixing the generator
  would move bytes in every world and was deliberately not done here; a
  future campaign touching this module should decide whether an unfinished
  shaft is a defect or a feature.
- **The verb count is hardcoded in three places, not one.** `IN_CHARACTER_
  VERBS`'s length, `HELP`'s text, and a THIRD site inside
  `h2_no_shipped_verb_can_end_a_possession_by_death` that an array-length
  bump alone does not cover (found by Task 5). CLAUDE.md already names this
  hazard; this is a second sighting of it, not a new one.
- **The false wire-tag claim in `driver.rs`'s module doc, carried into Task
  9/10's dispatch, is now fixed** — verified while writing this task's own
  sweep, it no longer asserts the retired "folds underground into `walk`"
  claim (Task 9 fixed it in passing). Noted resolved rather than dropped
  silently.
- **`SocialForm::Settled`'s exclusion from the wandering-inhabitant roster
  has no regression test.** Live and load-bearing (`drow` is the only
  `Settled` row among the three `Subterranean` peoples), disclosed by the
  implementer, confirmed by the reviewer, still untested. A fourth
  `Subterranean` row with an unexpected `SocialForm` would fall through
  undetected.
- **New this task: `MAP-underworld-viewport`.** The underground pane has no
  camera-follow; every rung past the first is wider than the fixed-width
  plate, and the player's own position can walk off the visible screen with
  nothing on-screen to say so. Registered rather than left to be
  rediscovered the way the campaign's own spec had to name it as a Risk
  before any code existed.

## The branch never absorbed main until close

`make sluice-status` carries no `campaign/the-gallery` row before this
task's own stage-gate submission at close — no stage gate was ever
submitted at the Task 6 boundary the plan's own pre-flight scan (F2) called
for, despite `main` moving repeatedly during this campaign's twelve tasks
(other campaigns landed throughout). The eventual stage gate reported
clean (`main unchanged`, all four phases green) — this campaign's own
branch happened not to collide — but that is not evidence the missed
cadence was safe to skip; it is the same "absence of damage is not evidence
of safety" lesson The Sources' retrospective already recorded from its own
55-commit-late absorption. Submit `make sluice-stage` at every plan-stage
boundary a future campaign of this size crosses, not only once at the end.

## Do differently next time

Read a brief's cited production function's actual signature and current
call sites before writing "builds X through Y" into it — twice this
campaign, a brief that skipped this step invented fictional inputs that
would have measured or shipped a fiction, at production-scope cost the
second time. And when a sweep for stale prose is dispatched with an explicit
lesson from the immediately preceding task, do not trust that stating the
lesson is enough — it recurred anyway, immediately, in the very next task.
The working remedy is a different SOURCE for the search terms (the spec's
own quoted wording), not a sharper reminder to try harder with the same one.
