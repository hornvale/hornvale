# Retrospective — The Manikin

One page of process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-manikin.md): the species model's
identity element lifted out of the roster and replaced by a reference vector
that belongs to no creature, at zero artifact drift.

## The central lesson

**A grep-derived inventory is only as complete as the grep, and this campaign
ran six incomplete ones.**

Every one of them was written in good faith, verified at the time it was
written, and wrong by the time it was executed against:

- The spec's Definition of Done named **five call sites** for
  `SocietyVector::baseline()`. Three of the five were tests. The distinction
  mattered because the task's risk assessment was priced on production reach.
- It named **four prose references** that go stale on the rename. Those four
  had arrived with The Namesake *after* the spec was drafted, and by execution
  there were nine in one file alone — including `psyche_registry()`'s own doc
  comment. Two independent reviewers found the wider set separately.
- It named **six book chapters** carrying the goblin baseline. There were
  **ten**, and twenty-seven sites within them.
- It named **four doc sites in one file**. There were twelve.
- It scoped the campaign to three vector families. There were **four** — the
  articulation vector, which had moved to `hornvale-language` in ECS c3 and
  carried the identical weld verbatim. See the next section.
- The scope extension's own sweep found sites in `worldgen`, `culture`,
  `language`, `vessel` and `history_bake` that the extension's first pass had
  not enumerated either.

**Two techniques worked where the greps did not, and both generalize.**

The first is to **delete the target and let the compiler enumerate the
callers**. Task 2 removed `baseline()` before migrating anything; `cargo
check` then produced an inventory that is complete by construction, in
seconds, with no pattern to get wrong. Any rename or removal of a public item
can be run this way, and should be, in preference to grepping for its name.

The second is a **±2-line co-occurrence window** between the concept word
("goblin") and the frame word ("baseline"/"midpoint"/"reference"). Six of the
stale doc comments were multi-line: the two words sat on different lines and
no single-line grep could see them. A rename that touches prose needs a window
grep, not a line grep.

## The second recurring mechanism: a count is scoped to the paragraph that derived it

This one bit twice in a single task, and it is subtle enough to be worth
stating carefully.

A number that is **correct** in one paragraph becomes **wrong** when moved to a
paragraph with a different scope — and the resulting sentence contains no edit
that looks wrong, because the phrase reads as established fact. It *is*
established fact. Somewhere else.

- Task 6's first round corrected stale articulation dimension counts (five
  found, where review had caught three) — the counts were right before the
  phonology epoch added `tonality`.
- Task 6's second round found that the *fix* had introduced a new false count:
  a gallery sentence said "three enumerations" where there are four. The phrase
  was borrowed verbatim from `manikin.md`, where it is correct — that chapter
  scopes to the nine mind/society/perception dimensions and deliberately
  excludes articulation. Widening the scope silently invalidated the number.

The fix that generalizes: **derive the count from the code, or remove it.** The
second round's repair deleted the count rather than correcting it, on the
reasoning that a claim which cannot go stale beats a correct one that can. Two
chronicle instances of "six-dimension" were correctly left alone — they were
true when written, pre-epoch, and a chronicle records what a past campaign did.

## Scoping to a crate instead of to an idea

The fourth vector family was missed because the spec scoped the campaign to
`domains/species`. The idea — "the identity element must not be a people" —
does not live in a crate; it lives wherever a vector family does, and one of
them had been relocated by an earlier refactor.

Escalated to Nathan mid-execution rather than shipped three-of-four, because
shipping the partial would have produced exactly the stale-claim failure The
Vacancy named: a book chapter announcing the manikin over an engine still
welded to goblin, forcing `language.md` either to lie or to advertise the gap.
The extension moved no value and did not disturb the preregistration.

**The check to add at spec time:** having named the crate, ask which *other*
crates hold an instance of the same construct, and say so in the spec —
especially where a past refactor moved one.

## Search the registry for the mechanism, not only for the subject

`PSY-2` already existed and already described this campaign, in its own words:
*"an abstract reference baseline no species need occupy."* It was missed. A
duplicate `PSY-manikin` was minted **and committed** before the collision was
caught at plan time, and deleted there.

The registry *was* searched. It was searched for the **subject** — elf, dwarf,
human, roster, species — and never for the **mechanism**: baseline, identity,
frame, reference. The row that already existed is filed under psychology,
because that is the layer it de-privileges, not under the peoples it would
eventually admit.

This is the same failure that produced a duplicate `TOOL-24`, which travelled
through a spec, a plan, a study JSON and a decision before anyone noticed
(`docs/retrospectives/the-pyx.md`). Two instances now, from two different
directions, which makes it a class rather than an accident: **grep the registry
for the thing you are about to build, not only for the thing you are building
it for.**

`LANG-53` was the counter-example and shows the discipline works when applied —
it already existed and already named this roster, was found *because* the
registry was searched before minting, and was updated rather than duplicated.

## Smaller things worth carrying

**The 600-character registry budget rejected all three capture rows on first
write.** Not one of them fit. This is now predictable enough to plan for: a
campaign that files rows should draft them at the cap, not draft them and then
discover it. The cap is on the Idea cell only; the Where column is not counted.

**A red main is a coordination problem, not a technical one.** Absorbing main
at `93e30931` pulled in four over-budget registry rows from another session —
`docs_consistency` failed on main itself, and this branch's gate could not pass
until the fix landed. The Salt branch held it, unmerged. Cherry-picking
`fcf30a3c` here as `6181f12a` was Nathan's call, and it is why this branch
carries a commit it did not author. The escalation was the right move: a
parallel session's red gate is not a thing an executing branch should decide
about unilaterally.

**Two deferred minors were triaged rather than fixed**, and both are recorded
here so the judgement is visible rather than lost with the scratch ledger:
seven reworded doc paragraphs left at 82–89 columns against a ~78 convention
(rustfmt does not reflow comments), and a `the_society_fallback_is_the_manikin`
test that restates the const's literals rather than exercising an actual
fallback — the real fallback sites are in `cli/` and unreachable from the
species crate. Neither loses coverage against what it replaced.

## What worked

**Freezing a byte-neutrality claim as a preregistered prediction.** The
prediction held — `git diff --exit-code` over the four watched paths returned
exit 0 and an empty diff, `docs/audits/` included, and `make gate` passed in
474.6 s. The value of preregistering a claim you expect to hold is that the
alternative was specified in advance: any drift would have stopped the campaign
for a re-spec rather than being absorbed by re-pinning the artifact that moved.

**Rejecting the obvious repair on a checkable fact.** Re-anchoring on humans
was killed by one observation — human night vision sits below much of the
roster — not by an argument about anthropocentrism. Nathan supplied it, and it
reversed a recommendation the decision ledger had already recorded (#2
superseded by #3).

**Verifying the rejected alternative rather than asserting it.** The "rostered
manikin" option was declined on a *read* test rather than a remembered one:
`windows/worldgen/tests/non_void_roster.rs` iterates the registry with no
allowlist, so a never-placed kind fails it by construction. A design rejection
that cites a file and a line is one a later campaign can re-open honestly.
