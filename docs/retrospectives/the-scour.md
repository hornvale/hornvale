# The Scour — retrospective

**Scope:** stage 2 of the derived-working-set program, reduced to its first
obligation — *confirm or kill §6.2's suspect before building anything*.
Process lessons only; the result is in the chronicle and in metaplan §6.3.

## The campaign was small because the previous one was expensive

The Penstock spent a day building three instruments and answered no product
question at all. The Scour then took under an hour, changed twelve lines, and
subtracted a third of the remaining programme. **The second is the return on
the first**, and neither is legible without the other. Worth remembering the
next time an instruments-only campaign looks like it is not delivering.

## Verifying the claim before acting on it

The plan said the twelve sites were "find-and-replace with a test each." That
is a claim, so it was checked first: every site enumerated with its predicate
and subject expression, confirming all twelve share one shape, that four
distinct predicates are involved (so a single-predicate search would miss
most of them — which is how the original count came out at five), that one
site passes a *variable* predicate (fine — `facts_of` takes `&str`), and that
both paths yield commit order so no fold could silently reorder.

The claim survived. That is the point: it cost two minutes and would have
cost hours had it failed after twelve edits.

## The lesson from the previous campaign recurred immediately, and I lost

The Penstock's retrospective names one failure four times: *a cheap check was
run and its result mapped onto a claim it did not support*. A memory was
written about it. Within the hour I did it again, on the first thing I
touched.

`test-worktree-freshness.sh` does not parse under macOS bash 3.2. I had
diagnosed the cause as a U+2026 inside a single-quoted awk body — plausible,
circumstantial, and asserted as fact in a board post, a retrospective and a
registry row **without ever testing it**. This campaign replaced all four
ellipses. The file still did not parse. A minimal reproduction then showed
the block bash blames parses fine in isolation, so the reported *location* is
wrong too.

Three attempts, then stopped per the rule. The registry row now records
**cause unknown**, with the refutation, because an honest unknown is worth
more than a confident wrong cause: the next person will not re-walk the dead
end. The fix attempt was reverted; nothing shipped.

**The generalisation, sharper than last time:** the failure is not
*uncertainty*, it is *unlabelled* certainty. A diagnosis reached by
plausibility and a diagnosis reached by experiment read identically in prose
one hour later. Write which one it is at the moment of writing, because
nothing downstream can recover the difference.

## The same lesson a third time, and this one I did not catch

Nathan read §6.3 and asked why 1.75 s/tick is acceptable "given how little
content we actually have." It is not, and the question exposed an error in
the *previous* campaign's recorded conclusion that neither its reviews nor
this campaign had noticed.

§6.2 concluded "plan cost does not swamp query cost." That was derived from
**slope**: plan and commit are flat per agent, total was superlinear,
therefore neither explains the excess. The falsifier it claims to answer is
about **level**: does planning dominate tick cost? Nothing measured that.
Once §6.3 removed most of the superlinearity, what remained was a large
constant — 5.75 ms per agent-tick even at ten agents — which is exactly where
planning would sit unseen.

This is the *third* instance in two days of one failure: **the question that
was cheap to answer got reported as the question that was asked.** Grepping a
signature for a call frequency; a single-line pattern for a completeness
claim; and now a slope for a level. All three read as verified in prose
afterwards, because prose does not record which question the number came
from.

**The remedy that would have caught all three:** write the claim and the
measurement as a matched pair — *"planning does not dominate (measured: ms in
the A\* path)"* — so a mismatch is visible on the page. §6.2 said "argument
from elimination, not a measurement of query cost" and still did not catch
it, because it named what the argument was *not* rather than what the
falsifier *required*.

Recorded in metaplan §6.4; falsifier-1 is now marked UNSETTLED.

## Determinism made the verification cheap

The strongest evidence this change was behaviour-preserving was not the test
suite — it was that **every deterministic counter the bench prints is
byte-identical across the change**, so only wall time moved. That check was
free, took no new code, and is more convincing than 602 passing tests.

It is available because the instrument was built to report counts alongside
timings. A bench that printed only milliseconds could not have made this
statement at all. **Design perf instruments to emit at least one deterministic
number**, even when the question is about time.

## The oracle held, and was worth keeping

The Penstock deliberately left twenty-one naive scans in the test module
rather than converting them, so they would remain an *independent* check on
the indexed path. Today those oracles silently validated twelve more call
sites than they were written for. A reviewer on that campaign was asked
whether the reasoning was wrong and upheld it; it has now paid twice.

## Process notes

- **Gradient checked, N/A recorded.** The one grep hit in `open-questions.md`
  is a terrain reference. Written down so the next close does not re-derive
  it.
- **This close preceded the merge**, per the guidance The Penstock's close
  added to `submitting-to-the-sluice`. First use of that fix, and it worked
  as intended — the artifacts were written while the result was fresh rather
  than after a second queue trip.

## Deferred, with homes

- The residual superlinearity (~1.11 fitted, ~1.29–1.41 tail) is real,
  smaller, and **unattributed**. Metaplan §6.3 tells stage 3 to measure it
  before building against it.
- `test-worktree-freshness.sh` still does not parse under bash 3.2; cause
  unknown, recorded in `TOOL-bash32-script-class` along with
  `hooks/post-merge`'s `mapfile`, which is bash 4+ and also unfixed.
