# The Lintel — Retrospective

Process lessons only; the product is in
[the chronicle](../../book/src/chronicle/the-lintel.md).

## The headline number

**Sixteen-plus defects were found during execution, and every one of them was a
defect in the *plan*, not in an implementation.** One more was found by the
plan's own author reviewing it. None was a subagent writing bad code against a
good instruction.

That distribution is the finding. This campaign's plan was unusually detailed —
verbatim code blocks, exact file paths, exact commit messages — and detail of
that kind converts *ambiguity* defects into *assertion* defects: instead of an
implementer guessing wrong, the plan states something false and the implementer
discovers it. Which is strictly better, because a false statement is falsifiable
and a guess is not. But it means the plan-authoring step now carries most of the
campaign's defect mass, and reviewing a plan deserves the rigour of reviewing
code.

The catalogue, by kind:

- **A duplicated kernel primitive.** The plan mandated a hand-rolled path slice
  that `RoomAddr::ancestor` already implemented. Caught by the implementer, and
  — the part worth keeping — the *plan* was corrected before the fix was
  dispatched, so plan and code never drifted apart.
- **A step ordering that made the red step vacuous.** A test-only module must be
  registered in `lib.rs` *during* the red step; otherwise the filter matches zero
  tests and reports success rather than a compile failure. Corrected for three
  later tasks once seen in one.
- **Two unverified file paths.** The stream manifest's generated file is
  `stream-manifest-generated.md`, not the `{{#include}}` wrapper the plan named.
- **A tooling tag written in a position the tool silently ignores** (below).
- **A tautological test** and an **unguarded constant coupling** (below).
- **An unanchored `sed`** (below).

## Three checks that would have passed while verifying nothing

This is the class worth naming, because all three were *green*.

1. **A test that never varied the thing it claimed to test.**
   `the_draw_is_keyed_to_the_world_seed` did not vary the seed. When it was
   rewritten to actually sweep seeds, it passed on the **first run** — the
   property had been real all along and only its coverage was missing. A test
   that has never failed for the right reason is not evidence that the property
   holds; it is evidence that nobody has asked.
2. **A banned-word list guarding a string the source never emits.** The guard
   was real, the ban was real, and the intersection was empty.
3. **A plan step whose invocation would have run zero tests and exited 0.** The
   plan told the closing agent to run the health battery with `-- --ignored`.
   That battery carries no `#[ignore]` attributes (`grep -c` returns 0), so the
   invocation would have selected nothing, printed a pass, and been recorded as
   evidence that behaviour had not moved. **This one was the controller's own**,
   and it was caught by checking the premise rather than the command. The
   corrected step does not say "run it" — it says *prove it ran*, and declares
   that a run reporting `0 tests run` is a failure of the step.

The shared shape: each check's *subject* was empty. The generalizable guard is
that any check whose value depends on a non-empty selection must assert the
selection is non-empty — a count, in the report, on the record.

## A fix round introduced a defect two lines from its own edit

Task 6's first fix round corrected a real functional defect: because every
chamber's prose is identical, aperture names collided, the deduplicating list
collapsed, and roughly **half of all built locales shipped chambers no input
could reach** — contradicting the structure's own documented invariant. The fix
renamed apertures by direction (`further in`) so the path graph, not the prose,
disambiguates.

While narrowing the `examine` line of the help text, the same round left the
`enter` line two lines above still promising the mechanism it had just demoted.
Round 2 closed it.

**Lesson: when a fix edits one line of a block that makes several claims, the
block is the unit of review, not the line.** A help text, a doc comment, and a
row of a table are all blocks in this sense. The diff shows the line; the
correctness question is about the block.

A second lesson from the same round is about scope discipline. The degenerate
prose underneath the bug was inherited and spec-protected, and correctly left
alone — but the *naming and resolution layer* was this task's own code, so the
bug was in scope. The reviewer's line was right: a deliberate deferral of a
functional defect needs a ratified decision, not a caveat in a report.

## An unanchored `sed` corrupted a different task's expected test count

A plan step used an unanchored substitution to update a number, and it also
matched an unrelated occurrence elsewhere in the file — another task's expected
test count. Same failure class as a wildcard match arm: **correct exactly where
you were looking, silently wrong everywhere else.** Anchor every scripted edit,
or use an exact-match editor. This one was cheap to find; it is cheap to find
only because a test count is loudly wrong when it is wrong.

## A cost measurement was nearly quoted from a debug build

The campaign's spike ran roughly **ten times slower in debug than in release**
(`interior_of` 6.166 µs → 0.666 µs; a 24² shadowcast 26.8 µs → 3.5 µs), and a
budget was nearly written from the debug figure. **A number without a build
profile is not a measurement.** Registered as a standing rule in the followup
register, not just as this campaign's near-miss.

The related discipline that did hold: the spike measured a *placement scan* and
the spec labelled it as such rather than calling it a solve, and the
native→wasm ratio was labelled an extrapolation rather than a measurement.
Naming what a number is not is as load-bearing as the number.

## A label claim was wrong until the source was read

The design assumed the existing furnishing stream label already versioned a
seeded *draw*. Reading `streams.rs` and the composer showed it versions a pure
admissibility *filter* — `selection` takes no seed at all. The consequence was
material rather than pedantic: it is why this campaign declares a **new** label
for chamber existence instead of reusing the old one, and why the two have
different blast radii. The general rule this repeats: **a claim about a
save-format contract is not a design claim, it is a source claim**, and it must
be read rather than remembered.

## Two overturns corrected decisions the same session had adopted

Of the brainstorm's three overturns, two reversed positions adopted earlier in
the *same* session: "a body occupies the finest band that has content" (adopted,
then overturned once someone asked what a player pacing a village boundary
does), and the promotion-as-framing move. This is the ideonomy discipline paying
for itself in the cheapest possible currency — a decision reversed in the
session that made it costs a paragraph, and the same reversal after
implementation costs a campaign.

It is also an argument against treating a settled-feeling answer as exempt from
a pass. Both overturned entries felt settled when they were written.

## The spec gap this campaign owns

**Spec §2 froze the walk-band anchor vocabulary to buy byte-identity, and never
stated the consequence: every chamber of a structure composes identical prose.**
The composer takes two booleans and no address, so a locale's chambers cannot
differ. The campaign's headline — walk between a structure's chambers — is
therefore literally true and experientially thin: four doors onto one room.

Two things are true at once and both need saying. The sameness **predates this
campaign** — every built, cold locale has composed that same interior since The
Hearth, and nobody could see it because nothing could stand in two of them and
compare. And fixing it is **epoch-class**, because the pattern inventory is
frozen at a level above this campaign's authority, so deferring it was correct.

Neither of those excuses the omission. The spec chose the freeze knowingly and
should have written down what the freeze costs the player, rather than letting a
reader find it by walking in. **This is the controller's gap, not any
implementer's** — the implementer who found it reported it correctly and in
scope. The remedy applied at close: the chronicle states it plainly and the
gallery transcript now *shows* it, two identical chambers side by side.

## Smaller things worth keeping

**The plan's placement of the closing artifact change was wrong, and the
closing agent had to notice.** The walk-script extension was specified to land
after a step that ends on an **unbuilt** locale, where `enter` only refuses — so
the transcript would have shipped without ever showing descent, and the spec's
headline success criterion ("observed in a transcript") would have gone unmet
while every mechanical check passed. The plan's block was kept, since the
physical refusal is itself a stated criterion, and a second block was added at
the built start room. Generalizes: **a plan step that specifies *where* to
exercise a feature is making a claim about world state, and that claim needs the
same verification as a file path.**

**A misplaced tooling tag should error, not be silently ignored.** A
`type-audit:` verdict written on a struct field's doc comment is never read —
only the struct's attributes are — and the resulting failure points at the field
and says nothing about the correct tag sitting directly above it. Cost one
implementer detour here; it will cost every future one the same way, and it is
registered as a followup rather than absorbed as folklore.

**Controller-side verification of a dismissed anomaly.** A re-reviewer waved off
"exit code 101 from an unrelated later test binary". The dismissal was correct,
but it was unverified, so the full suite was run directly (294 tests, 9 binaries,
0 failed). Cheap, and the alternative is a campaign closing on an unexamined
101.

## Inherited and outbound debt

**Inherited.** `docs/audits/type-audit-report.md` regenerated dirty for this
branch's whole life; the drift predates the branch (last regenerated in
`f9d53abd`). Regenerated and committed at close as part of the gate, not
attributed to this campaign's changes.

**Outbound.** `make vessel-check` is still red on main (seed 43 returns no
settlement), flagged as outbound debt by The Purview and inherited unfixed by
this campaign too. Two campaigns is the point at which "inherited" stops being a
sufficient reason.

**Outbound, and newly found at this close: `make gate-full` always dirties a
drift-checked path.** The heavy-tier chronicle sweep writes its report into
`book/src/laboratory/generated/the-sounding/`, and the report is **wall-clock
timings** — every digit of the CSV differs on every run by construction. So the
close's own drift check reports drift after every `gate-full`, and a closing
agent has to know to revert rather than re-pin. Reverting is demonstrably the
established practice: those artifacts were last pinned 748 commits ago and no
campaign since has re-pinned them. A committed artifact that cannot be
reproduced is not drift-checkable. The fix is to commit the *fitted exponents*
— which are stable, and are what the preregistered hypotheses actually test —
and leave the raw timings out of the tree. Registered; this run's exponents came
back 1.94 / 1.07 against preregistered 2.0 / 1.0, so the science held while
every number moved.

**Outbound, product.** Per-chamber interiors (epoch-class, blocked on the frozen
inventory); a named backward aperture, since `out` leaves the whole structure
and a structure is therefore one-way-in in practice; counting rather than
deduplicating repeated anchor kinds; and a half-stale doc comment in
`chamber_prose.rs` that still explains aperture naming by the rule the fix round
replaced. All registered.
