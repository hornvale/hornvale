# The Cruck — retrospective

*Process lessons. The chronicle carries the product story; the design and
decision records carry the technical contract.*

## The plan invented a mover that does not exist

The plan's fork fixture called a test-only `teleport_for_test` to put a
session inside a forking structure. There is no such function and there never
was: a session starts at its seed's flagship and walks. The brief-verification
step of the dispatch discipline caught it before any implementer ran, and the
fix was to pin a forking seed *by measurement* — a throwaway probe walked five
seeds through the real session start and printed how many children the
threshold had, which found seed 42 forking two ways and seed 14 three.

Plan text is the one code nothing compiles. This is the second campaign in
recent memory whose defects originated in the controller's own prose rather
than in an implementer's, and both were caught only because something read the
plan against the code before executing it.

## A plan stage that "may leave tests red" was unexecutable

The plan's Task 3 landed the production grammar and expected the embedders'
and the session's tests to stay red until Tasks 4 and 5. The commit hook runs
the commit gate on every Rust commit and bypassing it is forbidden, so a red
intermediate commit is not a state this repository can hold. The pre-flight
scan found it and the reorder cost nothing but sequencing: the tree embedders
and the session mechanics are both testable on hand-built structures, so they
landed first with production still drawing chains, and the first commit where
production forks carried the walk repairs with it.

The lesson is narrow and mechanical: a plan that stages "temporarily red" work
is describing a workflow this project does not have. Check a plan's commit
boundaries against the hook, not against the abstraction.

## An inert field is inert only until something reads it

Task 1 added a roles field to the structure and filled it index-wise, while
the function that answered the same question was already brief-aware at one
index. Nothing consumed the field, so nothing disagreed, and the review that
approved the task could not have seen it. Task 5a's consumer read the field
and the gap surfaced immediately. The implementer proved it empirically —
nine tests over the disagreement — before changing anything.

Adding a field nothing reads is not a safe half-step. It is an unverified
claim with a delay fuse, and the only defence is to land the field and its
first consumer close enough together that a review can see them at once.

## A review caught an edit reported as done that was not made

Task 5a's report claimed a documentation fix that the diff did not contain.
The self-review — whose whole purpose is that class of miss — missed exactly
it, and a reviewer reading the diff found it. The fix report had to open by
saying its predecessor was wrong.

## A per-row mutation verdict was inferred, and the inference was wrong

Task 6's mutation record first claimed "9 of 18 rows redden". The table test
loops two climates by two authorities by three postures, which is twelve rows,
not eighteen; and no per-row split had been observed at all, because the
assertion stops at the first mismatch. The number was reasoned from the
mutation's semantics and written as if measured.

The repair was to observe: apply the mutation again, temporarily replace the
inline assertion with a loop collecting every row's verdict, run once, restore
both the test and the mutation from a saved copy, rebuild, and confirm green.
The observed verdict was nine of twelve — the six cold rows and the three warm
command rows, exactly the design's stated prediction — and the instrument that
made it observable was already in the same diff, used by the readout beside it.
Two standing lessons applied late: a mutation proves only what it perturbs, and
measure rather than narrate the mechanism.

## A count transcribed from a report's first column is a claim about the whole set

The controller's own ledger entry, and the spec amendment it authorized,
enumerated the growing method's failing seeds as four and four. Four and four
was the preview column of the implementer's report — the first entries of each
list. The real sets are fifteen and nine, and the implementer's fix report
corrected the controller's document.

A number lifted out of a report is a claim about the population the report
sampled, not about the population the report was measuring. This one was
lifted by the person who had commissioned the measurement.

## A brief that carries a known trap does not spring it

Task 5b's implementer restored a mutation with `git checkout --`, which
discards the whole uncommitted diff rather than the mutation, and lost its
edits once. That exact trap is a recorded lesson. Task 6's brief carried the
warning explicitly, and Task 6 restored from a saved copy throughout and lost
nothing.

Carrying a known operational hazard into the brief that will meet it is
cheaper than any amount of after-the-fact discipline.

## Nine ratchets, none of them in a brief

Every one of these was caught by the commit gate at a task's first commit, and
none appeared in the task brief that met it: reconciliation TSV rows for the
spec and the plan (twice), the unmatched-plan pairing by date (the plan file
had been renamed to its spec's date), claim tags on seed loops (twice), the
world-build-sites roster, lexicon rows (three tasks), and type-audit and plumb
report drift on every change to a public surface.

The standing count of "ratchets every brief misses" was four. It is nine.

## A preregistered claim was narrowed after measurement, and said so

The design predicted that every reachable tree embeds faithfully under both
methods. The allocator's half held over all 2,560 tree-and-seed combinations.
The growing method's did not: it drops one doorway on 24 of them, on two of
the six forking trees. Six structural remedies were tried and every one moved
grown cave bytes, which the design had marked as a stop condition.

The ruling kept the generalisation, narrowed the claim to what is true —
all eight rules and exact freedom accounting over every chain — and pinned the
twenty-four failures by tree and seed as a witness that reddens if the set
moves in either direction. A weakened "most of them" assertion was rejected
for the reason a count ratchet is always rejected: slack hides a regression.
The spec's own hypothesis section carries the amendment loudly, marked as a
post-unblinding narrowing. **Post-unblinding changes: 1.**

## The campaign made a pre-existing parser defect observable

The ways-on footer advertised `the hearth`; the parser accepted only `hearth`.
The defect predates the campaign and was unreachable, because production had
only ever drawn chains and no session had ever printed a named way. The first
real walk that named one found it. Fixed and pinned in both directions by
separate tests: `the_ways_on_footers_own_words_are_accepted_as_typed` checks
the footer's own words are accepted, and
`a_prose_noun_shared_by_two_ways_names_them_rather_than_guessing` plus
`a_chain_resolves_exactly_as_before` check a word the footer does not offer
is still refused.

A campaign that makes a new surface reachable inherits every latent defect on
it. Budget for that rather than treating each as a surprise.

## Sixteen deferred minors reached only the scratch ledger

This is a discipline miss and is recorded as one. Sixteen review findings
across six tasks were written to the campaign's scratch progress file as they
occurred, and never to the committed ledger, which is the durable one. The
close walk found them there and backfilled every one with its outcome. Scratch
is per-worktree and dies with the worktree; had this campaign's worktree been
recycled before the close, sixteen rulings would have been gone.

The in-repo practice is to write a ruling into the committed campaign ledger
*as it occurs*, not to promote it at close. Promotion-at-close is the practice
that has failed repeatedly, and it nearly failed again here.

## No stage gate was submitted at any plan-stage boundary

The standing cadence is to absorb main and submit a stage gate at every
plan-stage boundary. This campaign submitted none. The commit gate ran locally
on every commit, so nothing red was ever committed, but the branch's first
meeting with main came after Task 6 — with seven campaigns landed during a
roughly one-day execution.

The absorption was clean and no code conflict arose, which is luck rather than
evidence. Two of its three recorded concerns are exactly the kind that a
per-boundary absorption surfaces next to its cause: a constant whose
documentation is now a stack of three campaigns' narratives, and a fixture that
auto-merged with edits from both sides and was validated by gates rather than
read. Neither is a defect; both are semantic drift arriving at close instead of
at the boundary that produced it.

## Scratch routing and deferred outcomes

Every deferred minor has an outcome recorded in the campaign ledger's
backfilled section: four were fixed inside the campaign (a documentation link,
a struct doc made true, an untested refusal string and footer arm, and a
generic refusal that advised the one token the next turn would refuse), one was
resolved by deleting the interim shim it belonged to, and the remaining eleven
were accepted with a stated reason — most of them unreachable while an
invariant holds, and each named in the ledger rather than left implicit.

Two limits are carried into the record rather than into a promise: the
tautological roles half of the shape proof is stated in the chronicle's honest
limits, and the growing method's fork failures have a registry row with their
measurement attached.

## The Confidence Gradient

The render–command parity bet gains a re-score. The ways-on footer is a
surface that names things, and it denied a word it had just printed — the same
shape a prior campaign found on four other catalog surfaces, arriving on one
more that no check reached. What is now mechanized is narrow and exact: a test asserts the
footer's own words are accepted as typed. What is not mechanized is the
general rule that any surface which names a thing must accept that name, which
still has no check and will be found the same way this one was.

## Close state

The artifact regeneration moved nothing: the transcripts, session fixtures,
stream manifest and audit reports had all been re-pinned inside the campaign,
and a full regeneration at close produced an empty diff apart from its own
timing row. No census was refreshed and nothing was pushed. The temporary
`IMPLEMENTATION_PLAN.md` was completed and deleted; the durable plan and design
are marked complete.

## The merge went red on a gate no local check covers

The first submission (`req-818ef56bbfa3`) failed the chamber's `clients`
phase after 1,290 s: `clients/game/core/tests/plan.rs` pins the sim's own
`map` picture of seed 42's arrival chamber as a constant, the campaign
regenerated the session fixture that picture describes, and the constant
stayed old. That crate is outside the cargo workspace, so `make
gate-commit` never builds it, and the constant's own doc — written by The
Pavement after the identical miss — names `make game-check` as the gate to
run after any fixture regeneration. The campaign regenerated two client
fixtures and ran none of the three client gates by hand before submitting.
The fix was mechanical (re-take from the sim, not from the client; the two
agreed byte for byte), and the lesson is the one already written above the
constant: a regenerated fixture that crosses a gate boundary obliges you to
run that boundary's gate, and a green workspace suite says nothing about
it. A memory already said this too (`gate-commit does NOT scan clients/`);
a controller memory is inert unless it reaches the checklist that runs
before submission.
