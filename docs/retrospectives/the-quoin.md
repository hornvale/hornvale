# The Quoin — retrospective

**Merged:** 2026-08-29

## The preregistration remedy generalized, and it is not the same claim as "the numbers were right"

The Rail's retrospective named its own headline defect: a preregistered
number computed by a *second implementation* of the resolver rather than the
resolver itself, agreeing with the real one only by luck. This campaign's
Task 0 built the remedy into its own first step — append the five tokens to
`IMPLEMENTED_DEMANDS` on a scratch commit, run the two live instruments the
campaign would later be judged against, record their output, revert — and
every later task's prediction was that resolver's own number.

All four figures (ladder covered, frontier, merchant, flood-watch demand
instances) matched on first run at every one of five tasks; none was
revised. **Stated precisely, because the stronger claim would be false**:
Task 0 derived the predictions with the same resolver that later scored the
outcome, so agreement confirms the implementation did what the resolver
predicted, not that the resolver is right about the world. What it does
confirm is narrower and still real — the remedy generalizes past the one
campaign that discovered the need for it.

## Three defects originated in plan text, zero in implementer code — and the count keeps not moving

- **A merchant-coverage step the plan omitted entirely** — `m04` moves at
  Task 5 (7→8, `existential`), and the original plan's Task 5 had no
  merchant step at all. Caught in the pre-flight conflict scan before any
  task dispatched (Ruling 1).
- **An impossible witness string** — the plan's Task 2 draft required
  `sleep` to inflect as `slept`; Common's past-tense rule is the naive,
  pinned-on-purpose `+ed`, so the true output is `sleeped`. Caught by
  pre-dispatch brief verification against the tree (Ruling 8), replaced with
  a branch table rather than a second guessed string.
- **A witness pairing `Definiteness::Indef` with an expected string that
  required `Def`** — `Part::Determiner`'s arm branches on `spec.definiteness`
  and `Indef` renders `"a tree"`, not `"the tree"`. Caught by pre-dispatch
  verification (Ruling 9), and the fix exposed a real design consequence:
  the definiteness effect's "enforced" branch turned out to be unreachable
  through the field that exists (decision 0446).

All three were caught before an implementer saw them, by the same
mechanism — reading the brief against the tree one task ahead of dispatch —
and none by review. This is the eighth consecutive campaign with the
"defects originate in controlling-session text, none in implementer code"
shape; what is new here is that the source class shifted. Every genuine
defect in this campaign's plan text was a **contradiction checkable against
the tree**, not a second implementation of the resolver — the exact class
PREREG-1 exists to prevent did not recur, and the class that did recur is
one pre-dispatch verification already catches reliably.

## A review can discharge a neighbouring question and read as discharging the constraint

Global Constraint 4 requires each token's module doc to *name* its backing
test — not merely to be backed by one. `definiteness` and `existential` both
had tests that existed, passed, and were wired into the witness table, and
**two independent reviews reported the constraint satisfied** on that basis.
Task 4's reviewer wrote the token "is backed by [tests] ... and wired into
`LADDER_WITNESS`/`ladder_construction` so the mechanical cross-checks
exercise it" — every word true, and an answer to whether the token is
*backed*, not whether the doc *cites* the backing.

Caught only because Task 6 had "verify every token has its named backing
test" as an explicit, independent step rather than trusting the two earlier
reviews' language. The controller read the reviewer's sentence as
discharging the constraint and it discharged a neighbouring one — the
project's own named failure mode, "a narrower question than the claim."

## A correction's blast radius, three times, and once found unprompted

- **Task 2's tongue-side redesign** (a self-inflicted `Argument::Absent`
  defect, corrected to `Argument::Concept`) fixed the code and the test and
  left the module doc citing a test name that no longer existed, describing
  a defect as live that had been fixed. The reviewer found one site; a
  tree-wide grep for the same claim, run because a previous campaign's
  lesson said to, found a **second site the reviewer never named**
  (`packs::NIGHT`'s own doc, present tense). Fixing only the cited site would
  have left the second one standing.
- **Task 5's regenerated audit** found the stale header sentence lived in
  **two source sites** — the generator's `out.push_str` literal and a
  duplicate in the same file's module doc — found by grepping the literal
  tree-wide after fixing the first occurrence. This was the first time in
  this campaign's own history that an *implementer* applied the "grep the
  claim, not the file" lesson unprompted, without the controller naming the
  second site first.
- Task 2 cost **two fix rounds against Task 1's zero**, and both rounds
  trace to one root: a modelling decision (`Argument::Absent` instead of the
  brief's own draft `Argument::Concept`) that silently deviated from the
  brief's own code, deviating in a way it did not disclose. Round 2 was
  purely the blast radius of correcting round 1's code — the defect was
  fixed, but the prose describing the old, now-false behaviour survived
  until a second review found it.

## The standing drift check needs a positive control, and I reported the weaker form as if it were the stronger one

`git diff --exit-code -- <generated paths>` compares the working tree to
itself and never re-runs the generator, so an artifact nobody regenerated
passes silently while being factually wrong. Run exactly that way after
Task 5's commit, it came back clean, and the controller reported "zero
artifact drift" to Nathan. True about what the command measures; misleading
about what was implied. `docs/audits/sentence-coverage.md` was in fact stale
by 12 insertions / 13 deletions — still claiming merchant 7, `m04`
not-yet-covered, existential absent, ladder 17/34. Fault is shared with the
plan (regeneration was assigned to Task 6, and Task 5's brief never listed
the command), closed immediately anyway rather than deferred, because a
stale artifact means every intervening gate run had been reading green over
something wrong. **The remedy used from that point on: regenerate first,
then diff** — a positive control, not merely an anchor. Task 6's implementer
independently applied the same discipline (verifying freshness by mtime, not
the bare diff) without being told the mechanism, which is worth recording as
the lesson landing on its own the second time it mattered.

## A test name that encodes a count is a moving part

`three_entries_sit_at_one_missing_demand` (the merchant corpus's
one-token-short list) was renamed to `four_` after Task 2 added a member, and
back to `three_` after Task 5 removed one (`m04` left the list for
*covered*). Three renames across three tasks, and each rename silently drops
the test from `docs/timings/subfloor-roster.tsv` — the sub-floor tier's own
roster — until the next green chamber run rewrites it. The roster is a
chamber artifact and correctly not hand-edited by this campaign; it is
recorded here rather than fixed, and it currently carries at least three
stale entries from this campaign's renames. The general form: a name that
encodes a fact the test's own subject can change is a name that will need
changing again, and each time it does the commit gate silently drops that
test until an external process notices.

## The produce-side finding, and the instrument that had to be built to state it

PREREG-4 required the chronicle to quote a produce-side demand-instance
figure that did not exist in the resolver as built through Task 6.
`demand_instance_coverage` ran over the whole entry slice with no direction
filter — the campaign selected its five rungs on a produce/parse ratio
(rejecting `wh-question` at 3-produce/5-parse in favour of
`verbless-clause`'s 20/28) using a statistic its own instrument could not
report until Task 6b, dispatched specifically to close that gap (Ruling 11,
decision 0448). The result cuts against the campaign: produce-side coverage
is **31.8%** (203/638) against the **34.8%** (393/1128) composite the report
would otherwise have led with — the composite was flattering the result by
three points, counting 490 parse-side demand instances this campaign never
addressed. Task 6b's own review found one more instance of the blast-radius
lesson above: the first fix round disclosed the split's post-hoc timing in
the code doc but not in the generated artifact itself, where PREREG-4's own
figure actually lives; the second round consolidated the disclosure at the
generator, stated once, pointed to rather than restated where the
flood-watch section needed it.

## Do differently next time

- **Pre-dispatch brief verification against the tree is still catching every
  plan-text defect this campaign produced**, and the class it catches
  shifted from "a second implementation disagreeing with the real one" to
  "a witness string the code cannot actually produce." Both are the same
  practice; keep running it a task ahead of dispatch even after a specific
  defect class stops recurring.
- **A reviewer's sentence answering "is it backed" is not evidence the
  narrower "is it cited" constraint holds.** Where a constraint names a
  specific artifact (a module-doc citation, a generated file's own
  disclosure), give the reviewer that artifact to check directly rather than
  trusting a summary sentence that reads as broader than what it verified.
- **When a positive-control regenerate-then-diff is the right form of a
  drift check, say so in the plan, not only in the retrospective that
  follows the campaign that needed it.** This campaign's own Task 5 brief
  never listed the regeneration command it needed, the same gap The Rail's
  retrospective already named for a different artifact.
- **A test name that encodes a count will need renaming again.** Prefer a
  name describing the *property* under test ("one token short of covered")
  over one encoding the current cardinality, so the next change to the set
  does not force a rename that silently drops the test from the roster.
