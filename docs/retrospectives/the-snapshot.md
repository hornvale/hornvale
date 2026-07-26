# The Snapshot — retrospective

Process lessons from shipping `vessel/session/v1` — one structured document per
committed turn of a possession — plus the client refactor that moved the
transcript pane onto it. Campaign 1 of the Rose Window program, six tasks, two
of them needing one fix round, closed with one whole-branch fix wave. Product
is in the chronicle.

## The one that matters: every plan defect was a sentence written from memory

Nine corrections to the plan landed as their own commits during execution, and
they sort into one bin. Invalid `type-audit` classes (`identifier`, `quantity`
— plausible inventions, not among the eleven ratified ones). The semantics of
`narration.prose` (the verb's response, not the room block). A `#[cfg(test)]`
module in `session.rs` that did not exist. The transcript's day (0, because the
CLI defaults there, not `PossessOpts::default()`'s 0.5). An import specifier
(`@std/assert`, not `jsr:@std/assert`). A TypeScript interface the plan never
mentioned but every new export must be added to (`worker.ts`'s
`VesselExports`). Which files `deno fmt` binds. And two facts about the
`type-audit` parser — a tag line cannot wrap across `///` lines, and a parse
error short-circuits the coverage check, so a malformed tag *hides* untagged
fields.

Not one was a design error. Every one was a claim about the tree stated without
opening the tree. The converse — that everything the plan asserted after
reading the code held — is not provable from the ledger; it is the absence of a
counterexample among the defects that happened to surface. What is provable is
the direction, and it is enough: no defect was traced back to a claim the plan
had checked. The cheap operational form is that a plan sentence about existing
code is either quoted with a path, or it is a *hypothesis*, and the brief
should say which.

## The type-audit root cause was itself written from memory

Seven tag violations reached Task 1's review as a plan defect faithfully
copied by the implementer. The right move was made — ask why nobody noticed,
not just fix the tags — and the fix held for all five later tasks: the command
and the eleven valid classes became a Global Constraint, so no task could
inherit the hole. But the diagnosis written beside it is **false**. The plan
now says `make gate` does not run `type-audit`; it has, since The Named folded
it into the `gate` target. The real hole is duller and more useful: no task
brief before Task 6 ran `make gate` at all, because briefs prescribe a
hand-assembled scoped subset (`fmt`, `clippy`, `cargo test -p <crate>`), and
that subset is assembled from memory — the same failure as everything above,
committed inside the correction for it. The transferable rule: when a brief
narrows the ladder for speed, the narrowing is a claim about the ladder and
needs the same check as any other. (The false sentence is still in the merged
plan; it should be corrected or not inherited.)

## Reading the generated fixture before accepting it paid for the campaign

Task 3's brief required reading the fixture rather than eyeballing its size,
and that surfaced a bug shipped months earlier by The First Mark: `grievance()`
folded with `.sum::<f64>()`, which returns `-0.0` on an empty iterator, so
every unprovoked NPC would have been pinned with a negative zero in a
save-format-class document. The generalization worth keeping is **the first
serialization of any quantity is an audit of it.** The load-bearing half is
negative: the existing `unprovoked_npcs_have_zero_grievance` test could not
have caught this and no strengthening of it could, because `-0.0 == 0.0`. An
equality test can never reach an identity bug. Left as followups: `quantize`
does not normalize `-0.0`, and there are ~45 other `sum::<f64>()` sites.

## Verify-don't-assume ran in both directions, at about the same rate

It corrected subagents three times (a claim that seed 43 had become
possessable — false, verified against the built module; a claim that a
`drive.mjs` reformat was pre-existing — false, it was the plan's own
one-liner; and a census diagnosis where the subagent was right and the
controller's grep was pointed at the wrong file). It corrected the controller
twice (the instinct that `Sum<f64>` folds from `+0.0` — wrong, and `-0.0` is
the correct additive identity; and the expectation that a type-only
TypeScript change would move the bundle — it is erased at bundle time, so
there was nothing to commit). The rule is not "distrust the subagent." It is
that a claim gets checked whoever made it, and the controller's hit rate was
no better than theirs.

## Per-task review structurally cannot see a seam

The most serious finding of the campaign — a full-width `u64` agent id typed
`number` in TypeScript, lossy above 2^53, wrong by 296 for seed 42 — lived in
three files touched in three separate task sessions, and no session ever held
two of them at once. It also *printed* correctly, so it passed casual
inspection. Only the whole-branch review could see it. The corollary for
planning: a field that crosses a language boundary has properties (width,
range) that belong to the seam rather than to any file, so a plan introducing
one should name the boundary itself as a review target instead of trusting the
union of per-file passes.

## A relocated failure invites the wrong attribution

`make vessel-check` was red on a hardcoded settlement-free seed; after that
fix it was red again, on formatting. The hazard is not a gate turning green
for the wrong reason — it never went green — it is that a *moved* failure
reads as someone else's: the implementer called the formatting failure
pre-existing, and it was not. What made that a ten-second lookup instead of an
argument was a baseline recorded in advance — before Task 5 touched anything,
the committed bundles were confirmed to rebuild byte-identically. Pre-recording
the baseline before the task that could disturb it is the practice to keep;
it converts every later "that was already broken" into a check.

## A visual gate a subagent cannot run is replaceable by an argument

Step 7 of Task 5 was a visual check, and a subagent cannot see a rendered
page. Rather than shrug, the client diff was reduced: it changed only *which
string* reached `append()` — same element, same class, no markup or CSS — so
pane-identity reduces to string-identity, which makes a headless
text-equivalence check *complete* rather than a weaker substitute. It was then
run over 13 turns against the real wasm and the real client module (not a
reimplementation), with zero mismatches, and it independently re-confirmed an
earlier design correction: 8 of the 13 turns would have mismatched under the
spec's original `narration.prose` design. A substitute that re-derives a
decision is better evidence than the check it replaced. But the precondition
is what did the work, and it will not hold for the second pane, which adds
markup.

## Absorption cadence

Main did not move during the campaign: its tip and the merge-base are the same
commit. The branch's one meeting with main at preflight was therefore also its
only necessary one. Recorded so this does not read later as a skipped cadence.

## The red gate at close belongs to an earlier campaign — for the third time

`make gate` was red at close. Chased per the closing walk: it is The Vestige's
missed census re-pin, inherited through main. The study asks for every
registered metric, The Vestige added four, and the golden's header still
carries 168 columns without them; `windows/lab/`, `studies/`, and
`book/src/laboratory/` are byte-identical to main. Not ours — but it blocked
our close and would have blocked the next one. This is the third instance in
recent memory (The Casement merged onto a red main carrying The Gathering's
missed calibration re-pin; The Vestige's own retrospective recorded that its
arc *chose* to batch three regenerations), which makes it a standing cost
rather than a scheduling accident: a deferred re-pin is always collected at an
unrelated campaign's close, by whoever has the least context for it. Since The
Local Census made a full regeneration a ~7-minute local job, the batching
rationale has largely expired.
