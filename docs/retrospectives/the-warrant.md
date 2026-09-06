# The Warrant — retrospective

Process, not product. The product is in
[the chronicle](../../book/src/chronicle/the-warrant.md); every ruling is in
[the campaign ledger](../superpowers/ledgers/2026-09-05-the-warrant.md), 28
entries; the falsified hypothesis and its dated results note are in the spec's
§10.

## Seven defects originated in my own spec, plan and brief text

That is the campaign's dominant pattern and it should lead, because five of the
seven were caught by *reading code or running a probe* and only two needed a
reviewer — which means the cheapest possible check was, in most cases, the one
that was skipped.

They fall into two families, and the second is the one this campaign adds to
the record.

**Family one: a claim about the world that was never checked.** Four instances,
all in text I wrote with confidence.

- **The errand's target.** The spec's §4 specified `object: Text(target)`. The
  arbitration seam exposes `Intent::Do(Action)` and nothing else, and
  `Action::MoveTo(n)` is documented as the *next step*. No destination is
  materialized at the commit site, so the field as written had no possible
  caller. Caught while reading the seam to write Task 1, after a G3 package had
  passed over it. The correction — the object carries the **origin** — is a
  better design than the original, which is exactly why it was not caught by
  re-reading: nothing about the sentence looked wrong.
- **The enumerated registration sites.** The plan named five sites "from
  memory of a subagent's report". Grep found six, one of which the plan had
  missed entirely and one of which it had described but mislocated. The remedy
  was to stop enumerating and instruct the implementer to derive the set from
  the observable.
- **And the grep that replaced it was an enumeration in disguise.** Three
  further sites register the predicate as `for (pred, doc) in [(AGENT_AT, …)]`,
  which `grep 'register_predicate(AGENT_AT'` misses *by construction*. All
  three run real sixty-tick walks with a panicking `.expect` on every committed
  fact, so each would have panicked the moment the campaign's second task
  emitted an errand fact. The observable to derive from was never the call
  shape; it was **which code paths run a real walk**.
- **The grouping rule.** My brief said "any other predicate flushes the group".
  Nothing anywhere sets a creature's errand back to `None`, so the ledger does
  not implement that rule; an intervening `grazed` fact does not end an errand,
  and the correct rendering rule is a readback of the producer rather than a
  choice. The implementer changed it because a rendering regressed; the
  reviewer's argument from the producer is the better one and is what the
  ledger records.

**Family two, and it is new: prose and its own formalisation disagreeing, with
only the prose audited.** Two instances, and they are the same shape at two
scales.

- **A test that a swap passes.** The brief required "a test that fails if
  **either** copy drifts" — correct in prose — and supplied example code
  comparing two `BTreeSet<&str>`s. Set equality is membership in both
  directions, not correspondence: two sets of the same eight strings are equal
  when two have been **swapped between keys**, and a swap is the likelier
  authoring mistake precisely because it leaves no orphan string for a set to
  notice. The implementer transcribed the snippet faithfully. The defect is the
  brief's.
- **A floor over a wider population than its own argument.** The spec's §1
  argued the entire case about the `agent-at` trail's prose and measured it.
  The spec's §10 then froze H2's fifty-per-cent floor over **all committed
  provenance**, a population §1 never discussed. The mechanism can shrink only
  one term of that sum and it adds a new one, so the prediction failed on two of
  three seeds while the quantity actually argued about fell by 65.72%, 65.82%
  and 54.56%.

A brief, a plan and a spec each carry two claims — *what is required*, and
*that the concrete thing shown alongside satisfies it* — and only the first was
audited in either case. **The remedy is one habit: check a specification's
example against its own sentence before checking either against the world.**

## The falsification was reported unamended, and that was the point of writing the rule down first

H2 is falsified. Both the frozen figure and the correctly-scoped one are in the
chronicle, and the narrower figure is stated as *context for why the frozen
prediction came out as it did*, never as a substitute result. The spec's §10
gains an appended, dated note; its threshold is not edited.

The temptation here was real and specific, because the defect was **in my own
preregistration**, which makes "the hypothesis was mis-scoped" feel like a
reason to re-score rather than an admission. Spec §10's own rule — *a null on
H2 is the headline and is not retuned away* — binds when the null is
inconvenient or it binds never. That rule was written before the code, by the
same author who then wanted out of it, which is the whole argument for freezing
it in a document rather than holding it as an intention.

**One asymmetry nearly went unreported and is worth more than the falsification
itself.** H1 was first discharged on seed 11 — the seed whose before-image
fixture happened to exist — and recorded as "HOLDS". H1 was frozen over seeds
7, 14 and 23. So *both* preregistered hypotheses had been evaluated against a
population other than the frozen one; H2's mismatch was ruled a falsification
and reported, and H1's was not surfaced at all. Reporting one and not the other
is not a defensible asymmetry, and the direction of the omission — the one that
made the campaign look better — is exactly the direction that needs a rule
rather than a judgement. The remedy was to go and build the missing
before-images from the merge base, not to narrow the hypothesis. H1 is
discharged as written.

## A transitional guard's retirement is a coverage event

For two tasks the eight glosses existed in two places, and a test held the two
copies equal. When the second copy was deleted the test was deleted with it,
which was **correct** — it could not exist in that form. It was also the only
thing pinning each key to its own gloss. What survived pinned the mode-to-key
mapping, pinned the eight key spellings, and asserted the eight docs were
distinct and non-empty; a swap satisfies all three. Four of the eight glosses
were pinned by nothing, and the campaign's own H1 fixture covered only the four
its seed happened to produce.

"Delete it with the thing it pins" is half an instruction. The other half is:
**name what that guard was the only witness for, and say where that half of the
job goes.** The proof that the replacement earns its place is a mutation, not
an argument — swapping two glosses leaves six tests green (including H1, which
resolves through the real registry) and reddens exactly one.

## Three ways a check stops measuring while staying green

Collected because the campaign produced one of each, and they are distinct
failures.

1. **A flip that retires a field silences the arms that assert ABSENCE over
   it.** The positive arm of an existing test went red and announced itself.
   The **negative control** beside it — a lone creature never flees, counted by
   matching `"fear"` in the provenance — would have gone on passing for exactly
   the wrong reason. A vacuous positive check reads as coverage; a vacuous
   negative control reads as *proof*.
2. **A ceiling mutation cannot fire against a measured zero.** My brief named
   "lowering the ceiling to 0" as the obvious RED proof for a re-pointed
   witness. Seed 42 commits zero fear or belonging errands, so `0 <= 0` still
   passes. Mutating the **filter** instead proved the stronger property — that
   the check reads the predicate field and matches on exact key equality.
3. **A ratchet that says `observed ⊆ sanctioned` is blind to a sanctioned key
   going quiet.** That direction was judged correct, and the point is that it
   was *stated*: a check that does not state its direction reads as total.

Twice this campaign, naming the property and letting the implementer find the
mutation beat prescribing one from outside the code. That is now four times
across the campaign's ledger.

## I relayed a reviewer's example without measuring it

A review supplied a vivid case: a creature that walks for thirst, drinks,
sleeps for a month and then walks for thirst again commits no second errand
fact, so two episodes fold into one line. I put it in the campaign ledger as a
finding and told Nathan about it in the same breath.

**It does not occur.** A drink flips the mode to sated, so the next step
computes a different errand key and commits. Two independent sweeps found zero
errand spans bracketing a drink. The real shape is narrower and better named —
an errand outlives its own activity — and the corrected version is what the
chronicle states, on two measurements rather than on a sentence.

It was one probe away from being checked and the implementer ran that probe.
**A reviewer's sentence can overstate its data, and a relay adds no evidence
while adding authority.** The correction is a ledger entry rather than a silent
edit for that reason.

## A method note that cost 533 seconds

A mutation-restore ran in one bash block as `set -e` plus
`cargo nextest … | grep …`. The shell has `pipefail`; the failing test — which
was *supposed* to fail — aborted the block before the `cp` restore, and the
"RESTORED byte-identical" line that would have said so was never printed. Two
subsequent edits landed on a mutated file and six tests failed for a reason
that looked like the campaign's own change. **The tell was the absent
confirmation line, not the failures.** Restore-and-`diff` belongs in its own
invocation, never downstream of a command expected to fail.

## Two pre-existing defects, found by running rather than building

`windows/vessel/examples/session_length_scaling.rs` panics at runtime on an
unregistered `slept-on` predicate, several ticks into its loop;
`windows/vessel/src/liveness_tests/emitter_scan.rs` has the identical gap and
passes only because its scenarios never emit that fact. Both pre-date this
campaign, both were parked rather than fixed — an epoch about errands should not
grow a diff a reviewer has to hold — and both now have a registry row
(`TOOL-walk-harness-registries-lag-the-committer`).

**The part worth keeping is why the panic was good news.** The re-reviewer went
one step past `cargo build --examples` and actually ran the example. The run got
*past* the campaign's own registration and into the tick loop before failing on
something else, which is positive evidence that the new registration is
correctly wired. A build check would have proved neither thing. It cost twelve
seconds.

## Scope was reduced deliberately, and it was surfaced rather than absorbed

Task 4 — the `Errands` resident tenant — was struck before execution, because
its stated consumer does not exist: `recount` takes `&World`, holds no
`ResidentFolds`, and cannot acquire one without a dependency the layering test
forbids. This was the second "no possible caller" defect in my own text in the
same campaign. The spec's §6 records the strike in place rather than being
quietly rewritten, and Nathan sees it at G6 as a scope reduction with a reason
attached.

## What went right, briefly

The campaign's headline exists because a premise was measured instead of
inherited. Decision 0238 had rested an entire stage ordering on a sentence read
off the code, and G2 ruled that the fidelity premise be rendered before being
asserted. Rendering it took one scripted `possess` run and produced both the
correction to 0238's argument and the fourth-row finding — seed 42 commits no
positional fact at all — that bounded the whole epoch's blast radius before a
line of it was written.
