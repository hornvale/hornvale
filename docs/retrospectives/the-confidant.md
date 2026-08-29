# The Confidant — retrospective

*Arc III of The Bridle. A possessed host now answers when asked how it feels,
in its own culture's words, and what it says diverges from what its arbitration
computed in two measurable ways. Nine tasks (one added mid-flight), six
decision records (0256–0261). Process lessons only; the campaign's own account
is [the chronicle](../../book/src/chronicle/the-confidant.md).*

## The headline: one failure mode, three people, one night

Each of these was a **real command, correctly run, answering a narrower
question than the claim attached to it.**

1. **The merge queue's operator** ran `git diff --name-only A..B` to ask *what
   did main change?* That is a **symmetric** difference. It reported this
   branch's own work back to us as the contaminating cause of that work's
   effect.
2. **An implementer** verified two failures "pre-existing" against a clean
   checkout of **our HEAD**. *Pre-existing* is a claim about **main**, and on
   main the test passes. Settled by checking out `origin/main` in a throwaway
   tree and running it there.
3. **I** read a green `gate-commit` as evidence the branch was healthy.
   `census_sentinel` is **compiled by that gate and never run by it** — zero
   entries in the sub-floor roster — so the green number said nothing about
   the census values the campaign had moved. I reported it as the headline.

None was careless. None was caught by re-reading. Two were caught by a peer
running a *different* command; the third I caught myself, and only because I
had just been on the receiving end of the other two.

**The transferable form:** when a command's output is about to become a claim,
write the claim and the command side by side and check that the command's
scope covers the claim's scope. "Main changed X" is not what a symmetric diff
computes. "Pre-existing" is not what your own HEAD can witness. "The branch is
healthy" is not what a gate that compiles a test without running it can say.

## Measurement changed the design twice, and both times the confident sentence was wrong

**"These metrics are cheap."** That was an inference, offered as one, and I
offered to measure before registering — which turned out to matter. Measured
against a same-depth arm (the trap of a `metrics: []` baseline dropping the
build rung was avoided deliberately): **2.95 ms per metric per world, about 3×
the stated tolerance.** A memoisation following an in-repo precedent recovered
**3.09×**, corroborated independently by a 45→15 reduction in world builds of
**3.00×**, landing at 0.953 ms.

**Then a non-degeneracy check killed the design anyway.** All 45 columns were
**constant across 1000 worlds** — correctly seed-invariant, verified
mechanically rather than assumed, because constant-across-seeds has one benign
cause and one broken one. They were reshaped out of the census into a
fifteen-row published table (decision 0260).

**A flat column looks exactly like a well-behaved one.** It is the
healthiest-looking thing in the table and it can never detect drift. The check
that would have caught this before any canonical-box time was spent is one
question asked of a new metric family *before* registration: does this column
take more than one value across the seeds it will be computed on?

## Redundancy paid three times in one night

Each of these looks like waste on an ordinary day:

- **A comparator's positive control.** The shared-column diff came back 0 of
  227,000 — and the same comparator run against main's own census showed 2,278
  differences, which is the only reason the zero meant "purely additive"
  instead of "instrument asleep".
- **The accession table's built-in anti-vacuity clause.** The
  insertion-stability test folds the same concepts into generation zero and
  asserts something *does* move. Without it, the campaign's central premise
  rested on a test that would report success for any input.
- **An exposure rule duplicated on purpose**, so the Laboratory's measurement
  is an independent reading rather than an echo of what it measures.

**The trap, and it is the sharpest thing in this campaign: the
cheapest-looking repair is the one that removes the check.** Collapsing the
duplicated rulebook into a single call would have "fixed" the drift by deleting
the instrument that detected it — and it would have read, in a diff, as a tidy
de-duplication. Decision 0261 states the rule that survived: keep both copies,
name each from the other, and hold them with an agreement test proved in both
directions.

## The remedy for a decaying exception is a check, not better prose

A waived flag that reads `false` for the right reason today reads `false` for
the wrong reason forever, because **nobody re-examines a column that already
looks explained.** A comment saying why is a one-directional acknowledgement:
it can only ever be satisfied, so it rots silently.

The repository already holds the working pattern. `seam-guard`'s
`expect(survives: …)` clause **fails** the moment a test starts catching the
thing it declares uncaught — a declaration that reddens when it becomes stale.
Any exception we grant should be shaped that way, or it is prose.

## A consistency test that only reddens on one side is an echo with a better name

If the agreement test between two deliberately independent derivations only
objects when copy A moves, then copy B is the source of truth and nobody said
so. The new test was mutation-proved in **both** directions, which is what
leaves neither copy privileged. The same principle retired an earlier draft
guard: a filter test that reddens on *deletion* would pass against a filter
that excluded everything, so the introspection filter was proved by **widening**
it instead.

## Six census runs, ~90 minutes of the canonical box, against an authorisation for one

Be honest about the arithmetic. The picture when Nathan approved was one
~15-minute run. The campaign consumed six, roughly 90 minutes of a strictly
serial shared box — a **6× drift** from what was approved.

Four were load-bearing:

| run | what it bought |
|---|---|
| control | proved the patch purely additive, 0 of 227,000 shared-column diffs, with a positive control on the comparator |
| the reading | found all 45 columns constant — the finding that killed the design |
| two more | chasing a `main` that moved four times during the exchange |

Two were not, and the reason they existed is the shape error in decision 0260:
the schema bind that made the census a prerequisite was downstream of putting
seed-invariant work in the census at all.

**What was done right:** the operator disclosed the running total to the owner
unprompted, rather than letting it surface later in the timings ledger. **What
should have happened earlier:** a non-degeneracy probe costs one small study,
and would have preceded every one of those runs.

## Plan defects were mine, again

A pre-flight conflict scan, written down rather than felt, found **three
defects before any implementer started — all in my plan text, none in anyone's
code**:

| # | defect |
|---|---|
| D1 | a task whose *test* consumed a later task's output — it could not be green on its own terms |
| D2 | an unstated hard dependency on the concepts a previous task registers |
| D3 | a fix located by **line number** in a file three earlier tasks edit |

Three more surfaced during execution:

- **A probe I prescribed from outside the code was structurally blind.** I
  specified a black-box diff: build a world, append a throwaway cohort, compare
  the JSON. It reports *identical* for every input, because an unregistered
  probe concept never reaches the generation lookup at all. Only the
  implementer's positive control revealed it; without that control I would have
  reported an identical hash as proof. **Never prescribe a probe from outside
  the code.**
- **My own spec told me to grep for re-derivations of the *universe* rule.** I
  did, and found two. I never asked the same question about the **exposure**
  rule — the one this campaign actually changed. A spec instruction is about a
  *class* of duplication; applying it only to the named instance is how the
  real one gets missed.
- **A task exposed a hole the spec had left**, and needed a whole new task
  (4b): worldgen's exposure rule never touched the new concepts, so no real
  culture could hold any of these words and the instrument would have measured
  a structural zero for all fifteen peoples. The implementer refused to author
  an exposure rule merely to make a test pass and built test cultures instead,
  which is exactly right — and is why the hole surfaced as a design question
  rather than as a green test over a mechanism no world exercises.

## Smaller things worth carrying

- **A commit-message heredoc leaked its terminator and a stray paren into a
  permanent record — the third instance in this project.** Fixed by soft-reset
  and recommit while still unpushed. The standing rule (never put backticks or
  a bare terminator inside a commit heredoc) needs a mechanical guard, not a
  fourth reminder.
- **Lexicon growth renames settlements**, and nobody had written that down.
  Adding *any* concept changes which forms are attested across a species'
  whole vocabulary, and settlement naming runs phonotactic repair over exactly
  that set. This campaign moved two proper nouns in a **cross-repo** tile
  fixture. The stop rule said "an existing word for another concept moving" —
  a proper noun is not that, so the letter was not breached. The coupling is
  the finding.
- **The mapping's 50% ceiling was reported as an artifact before anyone could
  read it as a finding.** Eleven of fifteen peoples sit exactly at the bound,
  so the mode *is* the ceiling — a number that would read as a discovery about
  impoverished minds. The published table declares it in its own header rather
  than relying on prose elsewhere.
- **`make game-check` was run by hand** because no gate compiles that crate —
  the predecessor campaign's own lesson, applied. It is the same family as the
  `census_sentinel` miss above: a gate is only as broad as what it compiles
  *and runs*.
- **A guard that text-matches a flag's name refuses you for writing *about* the
  flag**, in a heredoc or any shell-interpolated text, exactly as if you had
  used it. Same family as the backtick trap: describe it, never spell it.

## Deferred, with homes

- **`windows/lab/src/synthetic.rs` carries an inline copy of the manikin
  perception vector.** Deferred deliberately: a parallel campaign was
  restructuring that file at the time. It is a duplicated constant with no
  pointer between copies — decision 0261's shape without decision 0261's
  remedy.
- **How many other rulebooks in this tree are duplicated with no pointer
  between the copies?** Nobody has counted. Decision 0261 states the rule for
  the case where duplication is deliberate; it says nothing about how much
  accidental duplication is out there, and the exposure rule was found by a red
  test three days late rather than by looking.
- **Two sites re-derive the proto-root universe rule without calling the shared
  helper.** Pre-existing, not triggered by this campaign because it added no
  gap-reason variant, and it would diverge the moment one is added.
- **`suppressed` is threaded through the live session path only**, not the
  stateless affect snapshot the Laboratory's health metric and the needs read
  use. Any consumer needing the stateless shape owes that threading.
