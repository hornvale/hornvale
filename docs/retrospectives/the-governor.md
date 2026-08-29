# The Governor — retrospective

Cost and gating for the heavy tier. Process lessons, not product; the product
is in [the chronicle](../../book/src/chronicle/the-governor.md).

**Merged:** 2026-08-28

## The headline: correcting is the least-audited act in the workflow

Six defects this campaign were introduced **by corrections fixing defects**,
and the mechanism is specific rather than general carelessness:

> **A correction tends to add a justifying restatement beside the number it
> repairs, which doubles the surface that can drift.**

The chain, in order:

1. The count of heavy tests in files with zero assertion macros went
   **3 → 2 → 5 → "at least 9" → 6**. Every step repaired the previous step's
   mechanism and broke a new one at a *different layer*: a body-scoped regex
   (scope), then a file-wide scan that verified only a ranked prefix
   (sampling), then an exhaustive scan whose regex counted `panic!` as an
   assertion (predicate), then a correctly exhaustive scan applied to the
   wrong unit — tests with no assertions *of their own* in files that have
   some (unit). The fourth wrong value was produced by an agent explicitly
   warned that the number had been wrong twice and told to count
   exhaustively. It did count exhaustively.
2. The heavy tier's count of committed-artifact writers went **three → two →
   one**. The three-writer claim was true at file level and restated at test
   level; the correction that fixed the staleness hardened the category error
   and propagated the wrong count to five places, one of them append-only.
3. Decision 0426 accumulated four in a row. A `--since` filter selected the
   entire lifetime of the trailer it filtered on, so a twelve-day sample was
   divided by two months and every per-month cost was ~5.3x low — in the
   direction that flattered the option being chosen. The repair introduced a
   wrong row count. The repair for *that* used a ratio **the controller
   supplied**, in the same message that named this pattern, borrowed rather
   than derived and wrong by a rounding. The repair for *that* stated the same
   ratio in two forms in one sentence, which disagreed.

Two things came out of it that are worth carrying:

**The structural remedy beats the arithmetic one.** Changing `60.9` to `60`
would have fixed the instance and left the class. What was applied instead:
**state a derived quantity once, next to its inputs, never in a second form.**
A sweep for that class found four more live disagreements in the same record
(a day count at two precisions in five places, an hours-per-month figure in
six, a per-landing cost in seven sites across a decision, two scripts, a test
and a skill file, and "about one landing in six" glossing 17.6%). It also
recorded a deliberate *non*-collision — 53% appears twice for two unrelated
quantities — so a later reader does not "find" it.

**The review instruction that finally caught it was not "check this fact".**
It was: *re-derive every count, date and range you added, and say explicitly
if they all hold.* That found three further defects, one of them the
controller's. Seventeen other claims were re-derived and held — and recording
an audited-clean result is the point, because assuming it is exactly how this
pattern propagates.

A ratified decision record is the worst place for this to land, since
append-only makes the repair cost an amendment forever. 0426 was still
unmerged, so it could be edited directly. That was luck, not process.

## Bisecting this repository needs `--first-parent`

A manual bisection over the full `git log` between two SHAs produced an
**incoherent PASS/FAIL sequence** rather than a clean flip. The cause is
topological: a full log interleaves commits from parallel campaign branches
that do not dominate each other in the DAG, so "earlier in the list" does not
mean "ancestor of". The tell is concrete — **a PASS at a later list position
than a FAIL**, confirmed non-ancestral with `git merge-base --is-ancestor`.

`git log --first-parent --reverse` collapsed 1,489 commits to 384 mainline
ones and bisected cleanly to a single merge; bisecting *inside* that branch
then separated a raid-timing code change (still passing) from the stream-epoch
bump that actually moved the value. With seventy-plus branches a month through
this repository, this is a standing trap and it was not written down anywhere.

## A fix to a dispatch script cannot be exercised by dispatching

Task 7 diagnosed why twenty-seven heavy runs never reached the cost ledger —
the dispatcher runs in a scratch worktree, and the ledger path resolves to
*that* worktree, so the row lands in a file nothing commits and the next
dispatch's `reset --hard` discards it — and fixed it by mirroring the row into
the canonical checkout.

The fix did not fire on the campaign's own final run, and the reason is
structural rather than a defect: **`make heavy-remote` runs the canonical
checkout's copy of the script, which is main's.** The dispatched ref only ever
reaches the scratch worktree, after the script has already started. So a fix
riding on a branch cannot be exercised by dispatching it. Verified empirically
after a real run (the canonical `docs/timings.md` unmodified; `grep -c mirror`
on that checkout returning 0), not inferred.

The implementer flagged the residual honestly and the reviewer named the
settling test — "the next dispatch of a post-fix ref". That test was run and
could not settle it, for a reason neither could have known without running it.
Any change to the heavy, census or queue dispatch scripts inherits this.

## A docs-only commit skips the commit gate and can break a Rust test about docs

Adding a path to `docs/generated-paths.txt` broke
`generated_paths::every_declared_generated_path_is_written_by_the_regeneration_script`
— which is in the sub-floor roster, so the commit gate does run it. It survived
because the commit was **docs-only** and the pre-commit hook prints "no
Rust-relevant paths staged — skipping". It then survived four subsequent
reviews of adjacent work before a reviewer caught it by reading the criterion
rather than the diff.

The hook's notion of "Rust-relevant" and the roster's contents disagree, and a
test *about* documentation is exactly the crate of test that falls in the gap.
(The declaration itself was also a category error, and removing it was the
repair: the file's own criterion is "a path belongs here when the regeneration
script produces it", and a hand-authored audit table has no producer, so the
drift check on it was permanently vacuous.)

## Reasoning from the layering diagram instead of the manifests

The controller did this twice, and both times a diagram was substituted for a
fact a manifest already held.

- Spec §8.1 argued that a helper hosted in the composition root could not reach
  an in-module unit test in a sibling window crate, and that reaching every
  candidate would therefore require the kernel — constitutional ground. False:
  that crate declares the composition root under `[dependencies]`, not
  `[dev-dependencies]`. All thirteen candidates were reachable and the kernel
  was never needed. Found by an implementer checking the manifest and
  confirming with a scratch compile rather than taking the table's word.
- A conditional gating predicate keyed on the layering would have exempted
  exactly the changes most able to break the tier: seven of the 63 heavy tests
  live in the crate holding the tier's own harness, and the harness sits
  outside the world-generating layers entirely.

**A layering diagram says what *may* depend on what. It never says what does.**

## Implementers declined a controller's inherited claim five times, and were right every time

A relayed campaign attribution for two probes (their own module docs named
different campaigns); a relayed follow-up list of four stale locations that was
actually five, with the fifth in the most-read file in the repository; a
relayed pointer naming the wrong task; a section misattribution ("your rows"
when the rows belonged to another task's section); and a borrowed ratio. In
each case the implementer read the source rather than the relay.

The transferable half: **handing someone a task forces contact with the thing;
handing them a claim invites agreement.** Every one of the five was caught
because the recipient had to go and do something with the claim.

A related asymmetry, from the one time it went right: **a controller's doubt
routed to a verifier costs one command; routed to an implementer it can cost a
correct answer.** A false tension about a witness's ancestry was raised as a
question *to the reviewer* rather than as a finding *to the implementer*, and
dissolved under a single `git merge-base --is-ancestor`. Sent the other way it
would have reopened a correct attribution — the same shape that nearly wrote
the previous campaign's name onto nine failures it did not cause.

## Two tooling traps

- **The `Write` tool resolved an absolute path under the worktree's
  `.superpowers/` into the *main checkout's* copy**, and reported success. The
  file was written; it was not written where it was asked for.
- **`git add -A <paths…>` with one stale pathspec stages nothing at all** — the
  whole invocation aborts — while `gate-commit` reads the *working tree*, not
  the index, so it stays green and gives no signal that the commit is
  incomplete.

## Do differently next time

Absorption ran at the Stage 0/1 boundary rather than only at close, which is
the previous campaign's stated "do differently" and it was worth it: the merge
was content-clean and still broke the gate, because it silently added a test to
the commit gate's selection for the first time and this branch had already
grown that test's frozen token counts past its baseline. Two independent
changes, neither wrong alone. Nothing in the merge diff pointed at it.

The ruling that pulled the absorption forward gave a decisive reason that did
not fire — the heavy roster was unchanged across those 26 commits — and it is
recorded as a null rather than banked as a win. The ruling was still right; the
justification given for it was not what paid.
