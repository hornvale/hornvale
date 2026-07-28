# Retrospective — The Turnstile

One page of process, not product. The product is chronicled; this is what the
campaign learned that the code does not record.

## The docs were the bug

This campaign exists because `CLAUDE.md` named the unlocked command as "the
sanctioned refresh". The collision that prompted it was not carelessness —
the agent ran what the constitution said to run, and the constitution pointed
at the one entry point with no lock on it.

Worth generalizing beyond this instance: **an instruction that names a
command is a load-bearing interface, and it drifts like code.** Reading
`make help` during the sweep turned up worse than the line that caused the
incident — `regen-remote` still described itself as "the only sanctioned
census-regen path", pointing at an AWS box that decision 0063 abandoned
weeks earlier. Anyone reading `make help` was being sent to a machine nobody
uses. Four such assertions were corrected; none of them would have been
caught by any test in the repo, because nothing tests prose.

## An owner correction that was about the right axis, not the right answer

The spec's first draft keyed the lock on the goldens *directory*, reasoning
that the corruption hazard is per-directory and that runs in separate trees
are independent. Nathan overturned it in one sentence: *"I only have one
Lefford and the contention slows the runs down too much."*

The analysis was not wrong about the hazard. It was measuring the wrong
axis — runs in different trees are independent in their **output** and not in
their **clock**, and the binding constraint on a single machine is the
machine. **When a design decision has a correctness axis and a resource
axis, the scarce resource usually decides**, because correctness has other
enforcement mechanisms and wall time has none.

The correction also *simplified* the design, which is the tell that it was
right: one global claim subsumes both the tearing hazard and the contention
hazard, where the per-directory version handled one and left the other.

## Blocking made the original failure worse before it made it better

The second owner instruction — *"write the scripts so that they provide
useful context for the caller"* — turned out to be load-bearing rather than a
polish request. The campaign's root failure was that a contended run and a
normal run looked identical. Adding a blocking lock **deepens** that failure
unless the waiting announces itself: a silent pause on a forty-core box is
indistinguishable from a hang, and would have converted a confusing slow run
into a confusing stuck one.

That reframed the claim file from an implementation detail into the
campaign's instrument, and it is why the tests treat silence as a failure
rather than a cosmetic gap.

## Every constant here was chosen, not derived

Two numbers were reasoned and neither was measured: the 200-world-build
threshold and the 45-minute timeout. Both are flagged as such in the spec and
in the code comments, and the instrument that would let them be revisited —
census rows in `docs/timings.md` — did not exist until this campaign built
it. **Ship the instrument in the same campaign as the guess it will
correct**, or the guess becomes permanent by default.

## The plan's task boundary was reviewable but not committable

Tasks 1 and 2 split the claim's data (file format, threshold) from its
behaviour (acquire, wait, timeout). That is a sound *review* boundary and an
impossible *commit* boundary: clippy runs with `-D warnings`, and the data
half alone is dead code used only by tests. They were merged at execution
time.

The lesson is narrow but reusable: **in a repo whose gate denies warnings, a
task boundary must also be a compiling boundary.** Splitting a type from its
only consumer does not survive.

Two smaller instances of the same shape, both caught by the pre-commit hook
and neither predicted by the plan: an `#[allow]` on a `use` statement does
not cover the call sites of a disallowed type, and making a private function
public moves it into the type-audit's default-deny boundary — a *visibility*
change is a signature change.

## A harness lied three times in one session

The count is worth recording because it stopped being a coincidence:

1. a `/proc/stat` sampler with `%.6g` truncation manufactured a phantom "the
   census uses 8 of 40 cores";
2. a byte-identity check reported `DIVERGED` twice while comparing against a
   file that had never been written;
3. this campaign's own deadlock test reported `DEADLOCK` because
   `SKIP_CENSUS=1` skips the guard entirely and the script legitimately runs
   six minutes against a ninety-second timeout.

Every one was the instrument, not the code. Every one was caught by asking
how the number was produced rather than what it said. **Verdicts from
harnesses deserve the scrutiny given to verdicts from code — most of all
when the verdict is the one that would end the campaign.**

## A clean merge is not evidence of a clean merge

Twice in one close, `git merge` reported zero conflicts and produced a
semantically corrupt registry — keeping *both* the compacted and expanded
copies of a row, yielding a duplicate ID and a 2089-char cell against a
brand-new 600-char cap. `make preflight` warns about exactly this, and it was
right both times.

The reason it happened twice is structural rather than unlucky: two campaigns
were editing adjacent lines of the same table, and both merges were textually
disjoint. **After absorbing main, run the checks that own the file, not just
the gate** — `docs_consistency` caught in one second what the diff did not
show at all.
