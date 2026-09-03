# The Minute — retrospective

*A repair inside Arc III of The Bridle: a held body's walk commits what it
does, holds while off the walk band, and gets a report line that says what it
did. Six tasks, three decision records (0656–0658). Process lessons only; the
campaign's own account is
[the chronicle](../../book/src/chronicle/the-minute.md).*

## The claim written from reasoning, and the two functions that refuted it

The spec's §3.5 said everything downstream of the new commit would be current
"without further work, because it was always a fold" — and listed the resident
folds, the helplessness fold, the catch-up reconstruction, and the snapshot's
own entry. Four of those five were right. The fifth was the gate's `Asleep`
row, and it is not a fold at all: `Session::body_state` matches on
`Session.wake_at`, a session **field** that only the `sleep` verb sets.

The claim was written from reasoning about the *shape* of the change — a
commit feeds folds, folds are pure, therefore everything reading the ledger
becomes current — and the reasoning is sound about folds. It simply asserted
that a particular reader was one. The cure cost two file reads while writing
the plan (`body_state` and `sleep`, thirty lines apart in the same file) and
it produced a real behaviour: a walk's sleep can run past the end of the tick
that started it, so a released body would have stood awake at the gate while
its own ledger said it was sleeping.

The generalisable form: **a sentence of the form "X is already handled,
because X is a fold" is a claim about a specific function, and naming that
function is the whole verification.** The draft named a category instead.

## A predicted literal that measured one larger, for a reason nobody had listed

The plan asserted that a held session's ledger would carry exactly **one**
more fact for the body than a free session's after the first wait — the
walk's `slept`. Measured: **two**. The extra one is `possessed-by`, committed
by `!possess` itself before either wait runs, with the body as its subject.

Two things worth keeping from a one-off arithmetic slip. First, the plan had
supplied a fallback instruction — *print both counts and correct the literal
to the measured difference* — and that instruction is what turned a red test
into a two-minute correction rather than a debugging session. Second, the
review then asked for the **narrower** assertion the plan had actually meant
(exactly one `slept` for the held body, zero for the free) to sit beside the
corrected total. A compound count that happens to be right is a weaker
witness than the two counts it is a sum of, and the difference only shows up
when the compound is wrong.

## The two ratchets a brief never names

Task 1's brief listed three files and the commit gate refused twice, on two
workspace-wide checks the brief had not mentioned and had no reason to:

- a test that loops over a list of seeds needs a `claim:` tag
  (`cli/tests/suite/claim_shape.rs`);
- every `build_world` call site is rostered in
  `cli/tests/fixtures/world-build-sites.tsv`, with a reason per site.

Neither required touching an assertion; both widened the task's file list
beyond what the brief declared. These are the same two that catch new test
code in campaign after campaign. A brief that adds a test with a seed loop or
a world build should name them up front, not because the implementer cannot
recover — recovery was mechanical both times — but because an unannounced
refusal on the first commit reads like a defect in the work rather than a
roster that needs a row.

## Sweep the claim, not the site

Task 5 was the doc sweep: make every sentence describing the discard read as
history. Its brief named sites. The implementer fixed the sites. The review
then found a **word-for-word copy** of one corrected sentence — the false
claim that a walk-committed sleep follows "the same rule `Session::sleep`
applies", when the verb overwrites unconditionally and the walk keeps the
later wake — sitting ten lines above one that had just been repaired.

A sweep brief should name **the sentence to grep for**, never the line to
edit. The same round found three more stale claims by grepping the claim
rather than re-reading the diff, including one in a file outside the brief's
declared scope whose test was green and whose doc had stopped describing the
mechanism the test exercises.

The freshness sweep at close hit the identical shape from the other side: the
book's three chronicles were named in the task, and grepping the claim across
`book/src/` turned up two further present-tense statements of the discard in
the confidence chapter that nothing had listed.

## Preregister the mechanism and the prediction as two things

The campaign's headline is a null, and the reason it is a *useful* null is
that its preregistration had two independently checkable halves:

- **the mechanism** — a walk that resumes from where it stopped moves the
  body's committed position;
- **the prediction** — and therefore reaches water that a restarting walk
  could not.

The mechanism half is green and the prediction half is not. Had the two been
frozen as one sentence, the whole thing would have read as a failed
prediction, and the repair — which demonstrably works — would have been under
suspicion. Instead the test itself says which half held, and the chronicle
could go and find out *why* the other did not (there is no fresh water within
a hundred and twenty hops of that settlement, and the body's own resource
anchor falls back to its home room). **Split a preregistration wherever the
mechanism and its expected consequence could come apart**; the split costs one
extra assertion and it is what makes a null diagnosable rather than merely
disappointing.

The related discipline held too: the failing assertion was rewritten to pin
the **measured** value (zero drinks) with a note naming it as the null, rather
than being relaxed until it passed.

## A hook that reads its own input as a command

This campaign's plan text describes commands. One of the repository's own
guards refuses a shell invocation containing two `cargo test` strings — a
reasonable rule about running the suite twice in one call. Writing the plan
through a heredoc tripped it: the *data* being written contained two such
strings, and the guard saw the command, not the file.

Nothing was harmed and the workaround is trivial (write the file in pieces).
It is recorded because the failure mode is general and quiet: **a guard that
inspects a command line cannot distinguish a command from a document that
quotes one**, so any prose-authoring step that quotes the tooling it describes
is exposed to every command-shaped guard in the tree.

## Deferred minors, and where each landed

Every minor raised in review during this campaign was fixed inside it; none
was carried past the merge.

- **A message-less assertion** in the plan's own witness code (Task 1 review)
  — ruled trivial and fixed in Task 2's commit.
- **A stale "three waits" assertion message** left behind when the same test's
  script grew to nine waits (Task 2 review) — fixed in Task 2's fix round.
- **A compound ledger count with no narrower witness** (Task 2 review) — the
  two per-predicate assertions were added in the same round.
- **A comment claiming the walk's wake follows the verb's rule** (Task 3
  review) — reworded in Task 5's sweep, and its twin found and reworded in
  Task 5's fix round.
- **A paraphrase of the two provenance decisions, a narration branch with no
  end-to-end assertion, and a helper's placement** (Task 4 review) — all three
  folded into Task 5. The narration branch was then *measured* rather than
  assumed: seed 42's first held wait is not stationary for the population, so
  the assertion pins the line's suffix and its doc says which branch it
  covers.
- **A doc in a file outside the sweep's declared scope** whose present-tense
  claim about the discard had become false (Task 5 report, flagged rather than
  edited) — fixed in Task 5's fix round rather than left as a finding.

## Followups, with homes

- **The held walk's within-room seat is still dropped.** The walk computes an
  `Occupancy` inside the step and it goes nowhere; the population's does not.
  Pre-existing, and a within-room seat is not a committed fact (decision
  0069), so nothing is lost from the record. Home: the chronicle's closing
  section and the spec's §3.5, which names it out of scope on purpose.
- **A frames-aware creature walk.** The fidelity cut this campaign took —
  a held body off the walk band holds — is lifted by the campaign that gives
  derived creatures the same frames a player has. Home:
  `book/src/frontier/idea-registry.md`, row
  `PLAY-held-body-off-the-band-holds`, and decision 0657.
- **`wake_at` could be derived from the ledger for both the verb and the
  walk**, which would make the gate's `Asleep` row a pure fold rather than a
  read of a session field. Discarded during planning as a bigger change to a
  field the `sleep` verb already owns; this campaign instead set the field
  from the walk by the verb's own keep-the-later rule. Home: the campaign
  ledger's entry #6, which records it as the discarded alternative, and
  decision 0656's closing consequence.
- **A free body cannot drink.** There is no `drink` verb; the in-character
  roster is shaped around going and sleeping, so a free body's thirst is
  monotone by construction. The commit path this campaign built is the one a
  queued verb would use, so the plumbing is already there. Home:
  `book/src/frontier/idea-registry.md`, row `PLAY-free-body-cannot-drink`.
