# The Confidence Gradient

Not everything in Hornvale is equally understood, and this book would mislead
if it presented the settled and the speculative in the same voice. This
chapter is the standing map of which is which — and, unlike the world it
describes, it is meant to be **re-scored as campaigns resolve its questions**,
not written once and left.

The honest axis is not *how established the pattern is* — precedent was never
the thing at risk. It is **whether the world can grade itself on the claim, or
whether the claim rests on a human's judgment.** A bet the Laboratory can
score — generate the evidence, measure it, drift-check the number — is a bet
the project can drive to a verdict on its own. A bet that resolves only
against taste stays open for a structural reason, not for lack of effort.
Confidence still runs bottom-up; but the gradient it runs along is
*checkability*, and any claim about the top of the stack made with the
assurance of the bottom should be distrusted.

One sharpening, learned the hard way (The Named, 2026-07-16): **the
drift-check is not the checkable part — the anchor is.** A drift check pins
output against change and has no opinion about whether the output was ever
right, so a wrong number drift-checks green forever and every regeneration
re-ratifies it. The bets below are self-scorable because their metrics are
measured against something outside the generator (Earth's shoreline
development index, a null control, a preregistered threshold); the
drift-check only stops the answer moving once found. Where a committed
artifact has no external anchor — a rendered page, say — drift-checking it
buys stability and nothing else. The Named's defect sat in a drift-checked
artifact, in plain English, for eight days of green runs.

A second sharpening, and a different failure than the first (The Siding,
2026-07-29): **a check is worth only the configurations it actually runs in.**
The Named's lesson was that a drift check has no anchor; this one is that a
drift check can have an anchor and still never fire, because the command that
*regenerates* an artifact and the command that *verifies* it are never invoked
together. The census sat stale for 139 commits — wrong in three missing
columns and twenty-one drifted metrics — while every gate ran green, because
`make gate` `#[ignore]`s the tier that rebuilds it and `make rebaseline` never
touches it. The Sounding's wall-clock timings sat inside the drift-checked
tree for the mirror-image reason: nothing that rewrites them is ever followed
by the diff. Both artifacts were anchored and both were unobserved. The same
campaign wrote two checks into its own spec that could not have failed — a
zero-diff over a tree containing nanosecond timings, and a claim-status probe
that answered "no" from the wrong machine — and caught both only by running
them. So the gradient has a floor beneath checkability: a claim is only as
checkable as the *pairing* of its generator with its verifier, and an unpaired
check scores as unchecked no matter how good its anchor.

The successor campaign (The Timekeeper, 2026-07-30) built the instrument that
was supposed to close that gap and produced eight more instances of the same
shape *from its own plan text*, four of them inside the machine built to
detect them — including a duration alarm that compared each run against itself
and so could never fire. The count is now sixteen across the two campaigns, so
the floor needs stating as a practice rather than an observation: **the only
thing that reliably distinguishes a check that fires from one that does not is
making it fail on command.** A mutation step — corrupt the input, require the
red — found the never-firing alarm; five further findings came from a final
review that ran the system instead of reading it. Reviewing a check against
its specification cannot catch a specification that disagrees with itself, and
one of these did: the contention guard was wired backwards against a rationale
written three lines above it, and passed review as faithful to the plan.

The Sexton (2026-08-13) is the first campaign to **pair** one of those
generators with a verifier in the everyday command, and the result argues the
floor is right. The census is no longer checked only by the tier `make gate`
ignores: a three-world, all-metric sentinel now runs *inside* the commit gate
against the committed census, for about fifteen CPU-seconds. Within hours it
had verified three other campaigns' byte-identity claims — The Millrace's, The
Fathom's, The Holdfast's — on the census path, which is exactly the path a
worldgen campaign cannot cheaply check for itself. That is the pairing the
floor asked for, and it fires on a schedule nobody has to remember.

The same campaign also supplies the sharpest instance yet of the floor's own
failure mode, and it is worth scoring honestly against the bet. Decision 0130's
channel work added **467 CPU-seconds** to the commit gate and made one test the
slowest in the workspace at 187 s — unnoticed for the same structural reason,
because the instrument that would have seen it (`make ci`) had run **nine times
against `make gate`'s 368**. An unpaired check scores as unchecked; so does a
paired one nobody runs. The Sexton folded the alarm into the gate, taking that
instrument from nine samples a month to 368.

**Score: the bet moves toward checkable, and the practice sharpens.** Four more
instances of a check that could not fire arrived from this campaign's own plan
text — an extractor whose grep matched zero of 3,449 real events, a
scratch-sweep test that passed without entering the code under test, a
worktree enumeration that would have switched the main checkout, and a status
read from a file that reported green when the file was empty. All four were
caught by making the check fail on command. But three of the campaign's own
*measuring instruments* also lied — a sampler counting its own `grep`, a
false-negative `grep`, a planted violation that was not one — which extends
the practice by one clause: **make it fail on command, and run the positive
control, because a negative result from an instrument nobody has seen fire is
not evidence.**

**A confirming instance from The Begat (2026-08-16), on a path this passage
has not yet covered: a byte-identity claim.** That campaign's entire
correctness case was a comparison returning zero differences over a
thousand-seed panel — the purest form of the failure this clause names, since
an empty difference and a broken comparison are the same observation. It was
handled the way the clause asks: perturb one value of the thousand, confirm
the comparison reports exactly one disagreement, and only then read the zero
as evidence; and check the column is not trivially constant, because a
constant matches a constant. The score does not move — the practice was
already the bet's — but it extends where the clause is known to apply, from
checks that fire to **claims of no change**, which is the shape every
determinism argument in this project takes.

**Four more instances from The Interlinear (2026-08-25), and the useful part
is that three of the four were caught by a *reviewer* rather than by the author
of the check.** A positive control on a "no shared words between the two
renderings" assertion — neutralise it and a degenerate one-word output passes;
a coverage measurement whose only possible answer was zero, and so could not
distinguish a working resolver from one hardcoded to say "not yet"; a
byte-identity proof between two realizers that passed an empty list at every
call site, and would have survived the two diverging; and a complement
assertion that passed by luck of the draw, which probing all fifteen placed
peoples showed would fail for 33% of them for a reason that was not the one it
names.

**The score does not move; the practice gains a second reader.** Every one of
these was invisible to re-reading and died to running something — a probe
across fifteen peoples, a mutation, one `grep`. What is new is *who* ran it: the
implementer's own report said it plainly — *"neither would have been caught by
re-reading my own work"* — and the campaign's habit of asking each reviewer to
**verify a named mechanism** rather than form an opinion is what converted that
into findings. The clause already says make it fail on command; this extends
where the command should come from.

**A confirming instance on The Begat's path, too.** This campaign's correctness
case was also a claim of no change — a refactor that deleted a field from the
clause structure and rewrote every construction site across two crates, with
`make rebaseline` moving nothing. It was handled as the clause asks: the empty
diff is only evidence because `the-book.md` is rewritten on every rebaseline and
carries three distinct tail shapes, including one with no trailing clause at
all. An empty diff over a file nothing regenerates would have proved nothing.

**The Scarf (2026-08-26) confirms the clause twice and sharpens what counts as
a control.** Its successor campaign collapsed the two clause structures into
one, and both halves of its correctness case were handled as the clause asks.
Two new guards protecting a widened object slot were mutated **individually** —
neutralise the noun-class guard alone, neutralise the evidential zero-copula
guard alone, and each produced the same panic — so both are load-bearing and
the fixture genuinely reaches the branch, which one combined mutation could not
have shown. And its byte-identity claim is the same shape The Begat's was: an
empty diff over the gallery, held up by two controls rather than asserted.

**The score does not move, and the sharpening is about which control you cite.**
Two were available here and they are not the same grade. The **harness** control
is that `docs/audits/type-audit-report.md` moved (752 → 750 tags) when the `pub`
boundary changed, which proves the regeneration command ran and the diff can see
a change — and nothing more, because a different generator wrote it. The
**generator** control is
[The Interlinear](./chronicle/the-interlinear.md)'s: `the-book.md` is rewritten
on every rebaseline through the very realizer path this campaign retyped, and it
carries ninety tongue renderings. Only the second speaks to the claim. The
distinction matters because a harness control is exactly as convincing to *read*
as a generator control and strictly weaker to *hold*, and the first draft of this
passage cited the weaker one. **When claiming no change, name the generator you
proved ran, not merely the command.**

The same campaign produced eleven defects, all of them from its own spec and
plan text and none from the code the implementers wrote — a fourth campaign
confirming the diagnosis two paragraphs below. The one worth adding to this
chapter's inventory of checks that cannot fail is a **command** that cannot
fail: a mass rename written with BSD `sed` and `\b` word boundaries, which
matches nothing, exits 0, and prints no diagnostic. It would have reported
success, moved zero of 67 sites, and left a suite passing *because nothing had
changed*. That is the failure this chapter names, arriving one layer below
where it usually does — not a check whose predicate is wrong, but an edit that
never happened reporting that it did. Verify a mass edit by count; an exit
status is not a measurement.

**The Inquest (2026-08-26) does not move the score either, and sharpens the
practice once more: it matters *which* checks go red.** The practice as it
stands asks that a check be made to fail on command, that it be answerable to an
enumeration, and — the sharpening recorded further down this chapter — that a
positive control be shown to discriminate rather than merely to exist. All three
are satisfied by a uniform red — corrupt
the input, watch everything that touches it fail — and a uniform red is
consistent with one assertion doing all the work while its neighbours ride
along. This campaign's corpus resolver was mutated three times with a
**predicted subset** for each: swap one demand token for another and the
headline count stays at two while the pair changes, so only the by-identity
assertion reddens; add a token nothing implements and only the distance report
reddens, count and identities holding; drop the campaign's own new token and all
three redden. Each mutation landed exactly where it was predicted to. That
establishes something a uniform red cannot — that the three assertions are
*independent*, and that the count is not silently standing in for the identities
it was written not to trust.

The same campaign supplies this chapter's floor with an instance of its own,
from the other direction. Its plan named a byte-golden comparison, in capital
letters, as the single most important check it would run. The check was close to
vacuous: every production call site of the affected realizer passed a `None`
where the new draw would have entered, and the drawn values had no consumer
outside their own module at all, so the artifact could not have moved whether or
not the claim under test was true. The implementer established that
**structurally** rather than reporting the green, then measured the real claim
directly — forty seeds across three species, printing the four pre-existing
drawn axes with and without the new one, a hundred and twenty rows identical —
with a positive control that inserted one extra draw upstream and moved seventy
of the hundred and twenty. The claim is confirmed on far better evidence than
the check would have produced. **A check answering a narrower question than the
claim attached to it survives every reading**, and this one survived being
written down, emphasised, and dispatched; what caught it was somebody asking
what the check could possibly have failed on.

**The Mortise (2026-08-27) does not move the score, and the finding worth
leading with is about the *derivation*, not the check it was meant to settle.**
How many `TongueGrammar {` construction sites the crate carries was asked
three times inside one task: the controller said 25, the implementer said 21,
and a reviewer recounted and said 22 — with its subtraction shown. The
controller recorded 22 as authoritative *because it came with working
attached*, and the working was wrong. Re-derived directly against the tree,
`git grep -c 'TongueGrammar {'` returns 25 raw hits; one is the type's own
`pub struct TongueGrammar {` definition and three are `-> TongueGrammar {`
return-signature false positives, leaving 21 — the implementer's number. The
reviewer's own subtraction had undercounted the signature false positives by
one, and nothing re-ran it before the wrong figure was written into a
permanent retrospective as the corrected version of this campaign's own
thesis, where it stood until a later whole-branch review caught it. **Showing
your working makes a claim checkable, not checked** — a derivation is
unaudited text exactly like the number it supports, and this chapter's own
readers proved measurably more willing to accept a wrong figure that arrived
with arithmetic attached than one that did not. The practice built above —
make it fail on command, run the positive control, verify the mechanism
rather than the account of it — gains a clause here: point it at the check
*of* a claim as readily as at the claim.

The same campaign is a fifth confirming instance of the diagnosis The Scarf
named as a fourth: every substantive defect originated in the controlling
session's own plan or brief text, and none in the code an implementer wrote.
Five defects trace to text written before a line of implementation existed —
an addition instruction naming a placement that does not exist in the target
file, a consumer grep scoped to one crate that missed a match site in
another, the count above, a `make rebaseline` step that cannot see a
byte-golden it needed to, and a realizer edit aimed at a file that
structurally cannot hold the elision it was asked to carry — and each died to
a command run against the tree, never to a re-read.

A whole-branch review, run after eleven scoped task reviews had already
passed clean, found three more guards that read as evidence and proved less
than they claimed, on the shape this chapter's floor already names. `m09` was
credited with realizing an epistemic hedge on a witness that exercised the
hedge only as a lexical fact, which the same file's own doctrine states is
never sufficient for that credit. The spec's own inertness guard — a scan
meant to keep a shipped-but-unwired capability visible rather than silent,
required wherever a campaign declares a corner deliberately unused — matched
only the compound string `"predicate: KNOW"`, so a production site that bound
the value first (`let embedded = Argument::Clause(...)`, a shape the
surrounding code already uses) passed the guard silently while the
campaign's own ledger had just recorded that exact marker as "not
defeatable." And both new drawn axes — the subordinator, the conjunction —
were unobserved on `realize_tongue_deep`, the one function production
actually calls: two mutations survived the entire suite because the
deep-realizer's own tests supplied the drawn marker and then asserted on
something else entirely. All three were found the way this chapter asks —
neutralise it and watch, not read it and trust.

A fourth finding is this chapter's own subject arriving one layer up.
`the_baseline_assignment_accuracy_is_pinned` — the one test that would have
caught the pin movement in the paragraph below — sat red on the branch for nine
tasks, invisible through eleven scoped reviews and a green `gate-commit`
every time, because `docs/timings/subfloor-roster.tsv` listed its insensitive
sibling (a roster-size check, unmoved at eighteen) from the same module and
not the sensitive test itself. `gate-commit` compiled the crate, ran the
wrong test, and printed green; `subfloor_roster_coverage.rs` was satisfied
because the crate carried *an* entry, which is all it checks. This is not a
check whose predicate is wrong — the predicate fires reliably on the test it
is pointed at — it is the wrong check running under the right name, one layer
above where this chapter's instances have so far sat. The review found the
shape recurs twenty-four times repository-wide; nearly all are legitimate,
tests that build worlds and sit deliberately above the sub-floor with the
stage gate covering them, so the number is a scope for the next campaign's
registry row, not a tally of two dozen live defects.

One finding belongs to the world rather than to process, and it cost this
campaign a wrong number of its own before a second review caught it.
Registering `think` moved the Burr assignment-accuracy pin, and the obvious
decomposition — fifteen new correct rows, all of them `think` itself — was
wrong: `think` classifies correctly in thirteen of eighteen tongues, and the
remaining net two is eight *pre-existing* words re-classifying, five gained
and three lost, because adding one word to every daughter's lexicon shifts
the trigram profile the classifier reads for every word already in it. The
Inquest's own entry in that file asserted a subset property for its own
registration — that adding a word moves nothing else. For The Mortise that
property is false: a registration additive in what the lexicon *holds* need
not be additive in what a statistic over it *sees*, and the two are different
claims that happen to share a byte-golden.

**[The Stile](./chronicle/the-stile.md) (2026-08-28) does not move the score,
and the finding worth leading with is a correction that reintroduced the
defect it was written to close.** The campaign's whole-branch review flagged
that its two closure-derivation pins (`r004`, `r183`) were undocumented
against the one thing the ladder's own revision history actually does —
rungs get wired into the *middle* of an existing graph, not merely appended.
The fix wave's remedy, landing in the chronicle, said the pins "go red the
moment a rung is inserted and wired into the middle of the graph." That
sentence is false, and a mutation run at the campaign's own final gate —
rereading its own committed artifact, not a second reviewer — found it: a
rung wired into `r005`, mid-graph but outside either pinned rung's ancestry,
leaves all twenty-six tests green. Two of 214 closures are pinned, not all
of them, and the converse the correction asserted does not hold. It is
[The Mortise](./chronicle/the-mortise.md)'s clause — a derivation is
unaudited text exactly like the number it supports — landing this time on a
correction rather than an original claim, inside the one paragraph whose
whole job was to say honestly what the pins do and do not cover. The same
final pass also found the retrospective's own line count wrong in the commit
that had just corrected it, because that commit grew the file past the
number it wrote — the third wrong count this campaign put into its own
prose, which argues for what it did next: dropping the count rather than
correcting it a third time.

**A second instance predates the campaign and matches this chapter's own
diagnosis from [The Siding](./chronicle/the-siding.md) exactly: a check is
worth only the configurations it runs in.** `docs/audits/sentence-coverage.md`
was written only under `HV_SENTENCE_REBASELINE=1`, a variable nothing in the
repository ever set — not the Makefile, not `scripts/regenerate-artifacts.sh`,
which did not mention the file at all. The path *was* declared in
`docs/generated-paths.txt`, so the tracked-ness half of the drift discipline
looked satisfied; what was missing was a writer, and `git diff --exit-code`
over a path nothing ever writes is silently vacuous no matter how long it has
sat green. It was found by a controller's pre-dispatch reading, not by any
gate. The fix is the pairing this chapter keeps asking for — the generator
now runs inside `make rebaseline` — proven not by trusting the wiring but by
splicing a marker into the report, running the full script, and confirming
the marker landed and nothing else declared moved.

**A third instance sharpens the count-versus-identity distinction
[The Inquest](./chronicle/the-inquest.md) already drew, from the reviewer's
side rather than the implementer's.** Auditing the vocabulary cross-check
between the ladder and the two dialogue corpora, the whole-branch reviewer
held the reported counts constant — swapping one token out of a set and
another in, so a refused set stayed size two and a covered set stayed at
147 — and confirmed the set assertion still failed. A check that only
compared cardinalities would have passed. Holding a mutation's *effect* on a
headline number constant while changing its membership is a sharper probe
than either the campaign's own task-level reviews had run, applied here one
level up, at the branch as a whole.

**A fourth, smaller pair: a guard whose message claimed more than its
assertion checked, and the trap its own author fell into twice while writing
it.** A new test resolving intra-doc links to a renamed headline test said,
in its failure message, that four doc comments "must all name the same
current test" — read as a claim about arity, deleting three of the four
passed green, because the assertion checks distinctness of what remains, not
how many links exist. The message was reworded rather than the assertion,
correctly: distinctness is the property worth having as the test's name
keeps changing under it. And a source-scanning guard that greps its own file
for the pattern it is guarding is self-satisfying if it also matches its own
doc comment — the implementer hit that shape twice while building this one
guard, and excluded its own search literal from the match before either
instance shipped.

**The campaign is a sixth confirming instance of the diagnosis
[The Scarf](./chronicle/the-scarf.md) named as a fourth and The Mortise as a
fifth: every substantive defect traced to controlling-session text, never to
an implementer's code.** A brief's over-broad "do not touch `sentences/`"
constraint — meant to protect two frozen corpora — left the family's own
README stale about which corpus a resolver reads. A review brief attributed
a disclosure to a report file that had never carried it; the report was
clean, the disclosure lived only in a reply. And a controller's own
correction, once a stale claim was found live in one place, fixed exactly
that one place while the identical sentence stood, word for word, in six
more — three in the resolver's source, two in the generated report, one in
the ladder's own draft prose — found only by a grep the whole-branch review
ran that the original fix never did. One datum runs the other direction and
is worth flagging rather than trusting outright: this campaign's own hazard
— a verification claim written for work not performed — was named in later
dispatches after it was first caught, and a subsequent implementer caught
*itself* drafting exactly that shape of claim and removed it before the
report was finalized. Whether naming a failure mode reliably produces a
self-check or this was one attentive session is not something one instance
can answer; it is a hypothesis for the next campaign's data, not a practice
yet.

**The Attestation (2026-08-29) turns this chapter's own earlier finding — a
path declared and never written is "silently vacuous no matter how long it
has sat green" — into a durable structural answer, and produces a further
instance of the running diagnosis while doing it.** Three committed checks
were named as verifying a *state* while being structurally blind to an
*action that did not happen*: a phase roster restated in two places with a
one-sided agreement test, a ledger nothing diffs against what a job owed, and
a decision-block reservation nothing compares against the records actually
minted inside it. The campaign's own headline measurement (H1: the residual
unaccounted-for files resolve into a small number of authors) was
**confirmed and its own motivating number falsified in the same breath** —
the spec's claimed 585 files was arithmetic that missed a second census-gated
directory on the adjacent line of the same conditional block; the real
residual was 132. Its payoff hypothesis (H3: the new freshness reader would
surface an absence nobody already knew about) came back **NULL** on the
honest second try: the first run reported five confident "owed but absent"
jobs and called the hypothesis confirmed, and every one was a legitimate
prose-only narrowing the chamber's own dispatch script already accounts for —
the same shape as this chapter's recurring diagnosis, a check whose stated
guarantee exceeds what it actually verifies, this time committed by the
instrument built to find exactly that shape of defect. Nathan's own ruling —
declare a generated path's absence of an author with a reason
(`none(<reason>)`), rather than deleting the declaration or leaving it wrong
— makes this chapter's "an unpaired check scores as unchecked" into standing
policy: an absence now has a row, instead of a silently narrowed drift check.
And a live collision was caught only because the specced check (reading a
canonical, ssh-only ledger no checkout can reach) could not be built at all:
the substitute — comparing committed decision-block declarations against each
other — reddened immediately on the real tree, on The Scarf and The
Quadrat's already-known double reservation of 0286–0295, waived rather than
fixed because both headers are historical fact.

**A note from The Staff (2026-08-14), amended by The Sluice (2026-08-15),
since this score is read against an instrument this passage names by a label
that no longer exists.** Both halves of Sexton's pairing — the census
sentinel and the duration alarm — lived inside the single gate this chapter
calls `make gate`, which decision 0132 has since split by purpose into
`gate-commit` (local, every commit) and a **stage gate** on the canonical
box, run at each plan-stage boundary. The sentinel costs 12–25 CPU-seconds
depending on host, above the one-second floor that defines `gate-commit`'s
test tier, so it no longer runs at every commit; the duration alarm moved
with it, because both live inside the same `cargo nextest run --workspace`
invocation, which is now the stage gate's body rather than `gate-commit`'s.
Decision 0139 then moved the stage gate again without changing what it runs:
it is a request to the canonical box's serial merge queue now (`make
sluice-stage`), taking the same claim and running the same phases as a merge
does, but reporting instead of pushing. The pairing itself survives —
generator and verifier still run together, just inside the slower gate — but
the **frequency** this passage's score leans on does not automatically
survive with it: nobody has yet counted how often the stage gate runs in a
month, so "368" is not the number to read off this passage anymore, and no
replacement has been measured. Score this bet as still paired, at a
currently unmeasured frequency — not as reconfirmed at the old one.

A third campaign extends the tally in a way that narrows the diagnosis. The
Repertoire (2026-07-31) built a capability probe that touches no world state,
draws no seed and commits no fact — and produced the same family anyway, from
its own plan text: a coverage ratchet that read `REBASELINE=0` and an empty
`REBASELINE=` as permission to rewrite the artifact it was guarding, and a
resolver whose unknown-requirement branch returned *satisfied* rather than
*blocked*, inverting the default-deny posture its own spec had set. Both were
found by mutation — tamper with the input, require the red — and neither by
reading. So the pattern is not a property of measurement code, or of
determinism-critical paths, or of instruments that watch themselves. It is a
property of **plans written as literal code listings**, which get reviewed for
faithful transcription and not for whether the predicate they contain is the
one the spec asked for.

The Ballast (2026-08-15) adds a fourth shape, and it is the one this practice
does not reach. Every instance above is a check whose *predicate* was wrong —
a grep matching nothing, a branch returning satisfied, a guard wired
backwards. Mutation finds all of them, because mutation asks whether a check
can fire. The commit gate's test roster failed differently: its predicate was
correct and it fired reliably, for every test on its list. The list had simply
gone short. One crate had no entry at all, so none of its tests ran anywhere,
and the gate reported green about everything it was looking at while looking
at less than it claimed to.

Making that check fail on command would never have found it. Corrupt the
roster and the gate reddens exactly as designed; the check is not broken.
**Mutation proves a check can fire; it says nothing about whether the check is
pointed at everything.** An allow-list has the additional property that it
gets *faster* as it goes blinder, so the symptom of its decay is
indistinguishable from the improvement it was built to deliver — and the
mechanism documented to keep it current, which wrote its updates to scratch
that the next job erased, had never once run to completion in the file's
entire two-commit history.

**Score: the bet holds, and the practice gains a second half.** Making a check
fail on command remains necessary and remains the cheapest thing that works.
It is not sufficient, because it verifies the check against itself. A check
also needs an *enumeration* it is answerable to — a list of what ought to be
covered, with an absence from that list treated as a failure rather than as
silence. The first practice catches a check that lies. Only the second catches
a check that was never asked.

A fourth campaign puts the sharpening where the *repair* is. The Collation
(2026-08-06) produced the same shape from its own plan text — a spec promising
one test asserting a generated matrix's per-column figures equal the per-corpus
reports' own, and a plan listing that asserted only that the rendered document
*contains* each corpus id and each denominator, which any cell reading
`(217/409)` satisfies. Review caught that. The replacement was a whole-file
byte comparison of the matrix against its own committed copy, **proved to fire
by a mutation**, and it passed a second review on that proof — while still
being unable to fail for the reason the spec named, because the two figures
came from duplicated tally code and rebaselining accepts both documents in the
same pass. The whole-branch review caught the second one, and settled it by
running the finding's own scenario: perturb one renderer, rebaseline, and watch
the byte check go green while a cross-derivation test stays red. So making a
check fail on command is necessary and is **not** sufficient: the mutation has
to be the failure the check was promised against, and a check written to
replace one that could not fire inherits the burden of proof rather than the
credit.

A fifth campaign closes a corner instead of finding a new instance of one.
The Assay (2026-08-07) is a worked instance of decision 0097's middle
corner — the check that cries wolf, sitting beside the check that cannot
fire (this chapter's original floor) and the drift check with no anchor
(The Named). Three gate-resident tests were sweeping up to 200 worlds each
to answer an existence claim — does a live prediction crisis occur
*somewhere*, does every `Hydro` variant appear *somewhere*, does some world
win every toponymic concept — the exact shape 0097 names as deciding on
whichever single draw happens to sit nearest a hunt's break condition.
Moving them onto the census's 1,000-world fixture, as a coverage table and
a measured rate rather than a boolean the next campaign's true change could
flip, is what 0097 §2 prescribed and had not yet been built. The mechanism
this campaign shipped to make that safe — a tripwire that rebuilds three
fixed seeds every commit and compares them against the fixture — was itself
proven by mutation before anything moved onto it, per this chapter's own
standing practice: a corrupted fixture cell turns it red, naming the metric,
the seed, and both values; restoring the cell turns it green again. The
same campaign also produced a sixth instance of a lesson this chapter has
tracked under other names since The Timekeeper — a claim of absence
asserted from an incomplete search, this time by the controller itself,
mid-sentence while cataloguing five other agents making the identical
error, caught only because the dispatch had told the correcting agent to
verify rather than comply. See `docs/retrospectives/the-reassay.md` for the
full account, including a naming collision with an unrelated, already-shipped
campaign that shares this one's title — itself one more claim of absence
("the name is free") nobody checked.

A sixth campaign adds a corner none of the five describes. The Digest
(2026-08-08) set out to codify what the project holds about itself, and its
central task was halted early on the conclusion that the intended finding did
not exist: a rule forbidding numbered identifiers in the idea registry was
enforced by a gate check that passed, against a frozen fixture that the live
corpus matched exactly, with zero identifiers outside the list. Every
observation was correct. The conclusion was wrong. The fixture was written
seventeen days *after* the rule took effect, and in that interval the corpus
grew from 171 numbered identifiers to 403 — so the freeze grandfathered 232
violations and the check has been certifying conformance to them ever since.
This is not a check that cannot fire, nor one without an anchor, nor one whose
generator and verifier are never paired: it fires, it is anchored, it is
paired, and it is green for the right reasons. The defect is that its
**baseline was cut after the damage**, which is invisible in current state by
construction and recoverable only from history. So the floor gains a question
to ask of any conforms-to-a-frozen-list check: *when was the list written, and
what was already true when it was?* The campaign's delta view answers it by
reading git rather than the working tree, which is the only place the answer
lives. The retraction itself is worth recording — the wrong conclusion was
reached twice in one day by the same reasoning, first from a 1,055-commit-stale
timing baseline, and was overturned by a human disagreeing with an artifact
that looked authoritative.

Two campaigns reached this thread independently on the same day, from opposite
ends of it — one from a check that could not fire, one from a check that fired
wrongly. That they collided here, in a merge, is itself the strongest evidence
the chapter offers about how common the family is.

A seventh campaign finds the family in a place none of the six had looked. The
Domesday (2026-08-08) built a generated survey of the thousand-world census
whose whole purpose is finding weaknesses, and produced three unenforced guards
*inside the detector module itself*: a rendered sentence whose stated rule was
false as worded, a hand-frozen roster of measured crates with no live guard
beside a partner roster that had one, and a boundary test that re-implemented
the membership filter in its own body instead of calling the code it tested, so
emptying the real function left it green. A fourth, caught earlier, was a quine
— a test asserting the survey never builds a world, implemented by scanning its
own source for forbidden identifiers, which were present in that file by
definition. All four were found by an independent reimplementation that *ran*
the system rather than reading it. Knowing this failure mode confers no immunity
to it, which is now demonstrated rather than suspected.

The same campaign extends the family beyond checks entirely, and that is the new
corner. A **metric** can be unable to fire. The chorus sky-calibration metric is
a Kendall tau over [−1, +1] that reads exactly −1.000 on all one thousand
worlds, because it correlates a culture's sky capability against its sky-domain
distortion — and distortion in that domain is the fraction of sky facts lost,
while a sky fact is lost precisely when capability falls below the fact's
threshold. The two series are coupled through the same comparison, so every
strictly-comparable pair is discordant and the coefficient saturates by
construction. It is anchored, paired, drift-checked, current, and read by a
preregistered study — and it carries no information about any world. So the
floor gains a second question, asked of measurements rather than of checks:
*can this metric take a different value on a different world, and if not, what
is the study reading it actually scoring?*

An eighth campaign, landing the same day, moves the finding from the check to the *order of its
repair*. The Assize (2026-08-08) inherited a queued task reading "mechanize the
prose discriminator" — the cost batteries' rule for telling a contended run
from a regression, written in a module doc and applied by hand. Measured
against the run it was written for, the rule gives the **wrong answer**:
`genesis` at 2.09x its recorded basis with four control metrics at 0.96-1.29x
reads, under "a real regression is LOCAL", as a regression — while a quiet box
builds the same world in 3948 ms against a 13000 ms ceiling. The rule fails
because the five metrics have different resource profiles, only one of them
sculpting terrain, so a saturated runner starves exactly one and uniformity was
never the right test. Executing the task as written would have promoted a wrong
predicate from prose into code, where it would carry the authority of having
been computed. **A criterion earns mechanization by being correct, not by being
written down**; and the corrected version paid at once, finding a real localised
cost increase on its first run and bisecting it to a single commit while four
named sibling candidates moved it by zero.

The same campaign supplies the corner's other half, from its own repair. The
mechanized verdict then gave a confident wrong answer on a *second* battery,
because the two files' bases had been measured on different machines and the
heavy tier runs on only one of them — a ratio across two machines measures the
machines. The fix is the shape this chapter keeps arriving at: a check that
**states its own applicability** and declines rather than computing, printing
"bases were measured on aarch64-10, this is x86_64-40" and suppressing only the
ratio-derived claim while the raw milliseconds stand.

Two smaller instances from the same campaign are worth the floor's attention
because neither is a test. A committed, published artifact carried a headline
line reading `0 migration events (floor 5). PASS` — a verdict string never
wired to any check at all, which is this chapter's original floor in its purest
form (a check that cannot fire, with no check behind it) and which stayed
invisible for as long as the number it reported was healthy. And a rank-order
bound adopted to replace a rotted threshold turned out to be **unfalsifiable by
the only two mutation controls its file carries** — the correlation moves 0.831
to 0.127 under one of them and never crosses zero. It was retained, relabelled
in place as a directional record rather than a proven guard, and the tempting
threshold that *would* have made it fire was refused because it would have been
chosen for firing. Disclosing an unfalsifiable check beats quietly shipping one,
which is this chapter's practice working rather than a new failure.

A ninth campaign returns to the module the seventh worked in, knowing what the
seventh found there, and finds two more. [The Armature](./chronicle/the-armature.md)
(2026-08-09) set out to declare thirty causal links across the census — *this
quantity should move when that one does* — freeze them from physics before any
correlation was computed, and measure once. The first defect is arithmetic
defeating a guard that reads correctly. The correlation function documents that
it returns nothing when either column is constant, and tests that by asking
whether the variance is at or below zero; but `sum()` accumulates left to right,
so a thousand copies of one value leave the mean about `1e-13` off the constant
and the variance at roughly `3.4e-22` of rounding residue — **strictly greater
than zero**. The function returned correlations computed entirely from summation
noise, on precisely the six rows the campaign existed to interrogate, and the
survey published them. Nothing about that guard is wrong as written; it is
wrong as *evaluated*, which no amount of reading it catches. The second is a
comment disagreeing with the code beneath it in the direction that flatters the
result: the detector's strength branch withheld the observed sign
unconditionally, three lines below a comment stating that the sign is withheld
only "when nothing was measured", so three measured backwards couplings — the
worst of them `r = −0.755` against a declared *positive* link — rendered as bare
strength mismatches beside a headline reading *zero backwards links*.

Both were found by a reviewer who left the repository's own tooling and
re-derived every published number in another language, and one was confirmed by
**reverting the repair** to check that the resulting red was behavioural rather
than a compile error. That count is now seven instances in a single module
across two consecutive campaigns, each of which knew the family by name before
it began. The floor's practice therefore gains a narrower question, aimed at
the residue that mutation testing does not reach: *is this guard's predicate
true of the values it will actually see, or only of the values it describes?*
A zero-variance test is exactly right about mathematics and exactly wrong about
floating-point summation, and the gap between those two is where this one
lived.

The same campaign contributes the family's most useful positive result, and it
belongs here rather than with the failures. Silence from an instrument had been
carrying two meanings — *the claim held* and *the claim could not be tested* —
and the second is indistinguishable from the first at the point of reading.
Six declared links now report `D5 unmeasurable`, naming the frozen column and
its single value, rather than passing quietly; and a sign, once measured, is
never withheld. **An instrument's silence must mean exactly one thing**, and
everything else it might have meant needs its own name. That is the cannot-fire
family stated as a design rule instead of as a caution, and it is the first time
this chapter can offer one.

A tenth campaign moves the family off checks and metrics entirely, onto the
data. The Particular (2026-08-10) registered four predicates for individual
persons, one of which — the day a person died — is committed only once that day
has passed. It has never been committed. Across three seeds and 364 promoted
founders the count is zero, because the arithmetic deriving the death day
subtracts a maturity in *days* from a founding day in *years* and compares the
sum against a present in years; the gating condition is unsatisfiable for every
species in the roster by an order of magnitude. The predicate is registered,
documented, hand-tested on both of its branches, and counted by the capability
probe as vocabulary the world holds. The live-world test that walks every person
and asserts death follows birth contains a conditional that has never once been
entered, and stayed green throughout. So the floor gains a third question, asked
of vocabulary rather than of checks or measurements: *does any world actually
produce this?* Nothing in the suite asks it. The instance was found by scoring a
preregistered prediction numerically at the close, which is the only step in the
campaign that computed a figure the tests did not already assert.

The same campaign supplies a smaller lesson about preregistration itself. Its
size bound — the ledger grows by no more than 2.1% — was **falsified** at 6.25%,
while the exact-count identity beside it held to the fact on every seed. Both
readings are correct and together they localise the cause: the mechanism did
what it was specified to do, and the estimate was computed against a world that
had since been rebuilt, its fact count falling by a factor of three and a half
in the interval. A ratio frozen before the code is only as durable as its
denominator, and a prediction expressed as a *fraction of current state* silently
re-aims itself every time that state moves. Freezing the numerator as an identity
is what made the falsification readable instead of merely disappointing.

An eleventh campaign repairs the tenth's finding and, in doing so, measures how
much of the family the repair itself contained. [The Ell](./chronicle/the-ell.md)
(2026-08-11) moved the unit boundary that made a death uncommittable: the
history bake keeps reasoning in years, and what crosses into the ledger is days,
converted at named functions rather than at inline divides. That created sixteen
unit crossings, and the campaign swept them by mutating each one in turn against
the whole gate. **Five of the sixteen had no guard at all.** The most
consequential was not on anyone's list: the crossing that feeds the census's own
name renderer, whose blast radius is a committed census value the commit gate
never rebuilds — and whose two plausible existing guards cannot catch it *by
construction*, one because it counts zero-gap edges (zero is zero in any unit)
and the other because it keys a map on the year form, which is invariant under
any injective rescaling. Both tests are correct, both are green, and neither is
about the quantity that moved. So the floor gains a fourth question, asked of
conversions rather than of checks, measurements or vocabulary: *which test goes
red if this crossing is deleted?* — answered by deleting it, not by reading the
suite.

Two smaller instances from the same campaign sharpen what a guard has to be
compared against. An invariance test that had been green for months compared a
reconstructed record against a committed one — by comparing **one fixture
against another fixture**. When the ledger's unit moved, the module's stated
premise became false and all four of its tests stayed green, because both sides
were wrong the same way; the sibling case in the same sweep went red on its own,
because there the two sides were production and fixture. And a collision guard
written to detect a new failure shape turned out to be **entailed by the key it
guards**: the key folds the parent's coordinates, so any colliding pair
necessarily has equal parent coordinates and the assertion cannot fail while the
key is what it is. It was kept and relabelled as a tripwire for a future
narrowing — the honest description of what it can do — rather than deleted or
left claiming more than it holds.

**No bet in the map below moved.** The Ell repairs a unit boundary and a derived
key; it resolves no open question about the world, raises nothing from taste-gated
to self-scorable, and leaves every score in this chapter where it stood. Recorded
explicitly, because a campaign that changes the save format and every founder's
name looks from the outside like it should have moved something, and decision
0030's sweep is answered by a statement either way rather than by silence.

A twelfth campaign contributes the family's densest single instance and, with
it, the first useful statistic about *detection*.
[The Compendium](./chronicle/the-compendium.md) (2026-08-15) built one
resolver — the anchor parser and audit in `cli/src/systems.rs`, most of it
resolution logic rather than rendering or its own inline tests — whose
entire purpose is noticing when a citation stops being true, and produced
**four separate false-cleans inside it**: a symbol match that accepted any
name it was a prefix of; a fallible
operator inside a loop, so the guard's count propagated an empty result and
could never fire; an exact string comparison against a status vocabulary whose
real cells carry qualifiers, emphasis and transition arrows, leaving roughly a
fifth of the rows it guards permanently unfalsifiable; and a citation of a test
that is compiled but never run, which the resolver called resolved. Three are
the same category error wearing different faces — treating a syntactic
coincidence as a semantic fact — and each was made after the previous one had
been found and fixed.

The statistic is in *who found them*. One by review, one by the implementer
using the tool rather than testing it, one by the controller reading a task
ahead, and one by accident while hunting better evidence for an unrelated
verdict. **Four detection mechanisms, each of which found exactly one.** Every
prior entry in this thread argues that a particular check could not fire; this
one argues something narrower and more actionable about the searching:
redundant detection is not redundant when each detector has a different blind
spot, and dropping any one of these four as duplicative would have shipped a
false-clean in an instrument whose whole claim is that the citation is the
evidence. The third defect was also, on inspection, the controller's own — a
rule generalized from a single real row whose status happened to be the one
unqualified form — which is this chapter's standing lesson that a correct
observation and a false generalization are routinely the same sentence.

**No bet in the map below moved.** The Compendium ships an instrument and one
reading of it; it resolves no open question about the world and re-scores
nothing here. Its one finding *about* the world is a render gap rather than a
sim one: the first capability in an external catalogue this project cannot
replicate is that catalogue's own first page — entities carrying their own
appearance — and it reads **absent**, because the shipped character-grid
client draws one glyph for the possession and one for everything else in view,
terrain and marks alike. The catalogue's *refusals* are a separate and smaller
set, each tracing to a ratified decision rather than to a deficiency; they
confirm existing positions rather than moving a bet.

**That render gap is now closed, and this bet moves.** [The Legend](./chronicle/the-legend.md)
(2026-08-28) gave every creature its own noun-initial glyph in the walk band
and floor plan, distinct from terrain, so the catalogue's first unmet item
now reads `present`. Per-entity colour still does not exist anywhere in the
client — the smaller, secondary gap the same finding already named — and
that half stays open.

A thirteenth campaign contributes two corners the practice does not reach,
and a correction to a score written above. [The Sluice](./chronicle/the-sluice.md)
(2026-08-16) built a serial merge queue and produced **fourteen** defects from
its own plan text — the largest single tally this thread has recorded, from
the same source every other tally names. Two were guards described as live
that nothing could redden, and one was a check written against a situation
that cannot occur: it grepped for git's default merge subject `Merge branch …`
against a chamber that merges a bare identifier into a detached head, where the
default is `Merge commit '<sha>' into HEAD`. Those belong to the family already
described. The two below do not.

**The first corner is a check whose predicate is right and whose model of the
world is wrong.** A static lint scanned one file for commands that would write
to the real remote. It was defeated three times, and not once by a missing
pattern: first a denylist that could not enumerate the shell's syntax, then a
hand-rolled word boundary that matched nothing at all, then a line-continuation
joiner that inserted a space where the shell deletes the backslash outright —
so a command split across two lines in an unusual place really executes as a
force-push while the auditor reports zero violations. Mutation does not find
this. The check fires; it fires reliably; it is pointed at everything it claims
to be pointed at. What is wrong is its *model of the language it reads*, and
each of the three looked correct by inspection. The lint was deleted rather
than patched a fourth time, because by then it had acquired a worse property
than the thing it protected — an unbalanced brace in the audited file closed
the test wrapper early, so the audited content executed while the audit
reported clean. The replacement is a runtime hook that refuses the operation
rather than a reader that predicts it. So the floor gains a fifth question,
asked of any check that parses rather than executes: *whose model of this
language is this check using, and has that model been made to disagree with
the real one on purpose?*

**The second corner is a generator with no verifier at all, inside the gate.**
The project's whole-world tier authors four committed artifacts. Two of the
four tests compare what they build against a committed copy and fail when they
differ; the other two only write. They assert nothing about what they wrote, so
both artifacts went stale through a run reporting eighty passes out of eighty.
This is The Siding's unpaired check with the pairing broken at the other end:
there, the generator and the verifier existed and were never invoked together;
here the verifier does not exist, and its absence is concealed by a green
number in the same run that produced the drift. **A test that authors an
artifact must also assert it**, and an authoring test that asserts nothing is
not a weak check but a zero one wearing a passing test's clothes.

**And a score in this chapter needs correcting.** The passage above credits
The Ballast with building the missing half of the commit gate's roster loop —
a copy of a green run's list to durable storage, carried back by a person. It
never moved a byte, and neither published explanation of *why* was the cause.
The list has one commit in its entire history. The blocker sat a step earlier
than anyone had looked: the command that writes the list refuses whenever the
machine's exclusive claim is held, and every serialized path in this project
runs that command as a descendant of the process holding the claim — so the
one thing that produces the list declined to, in the one environment where
nothing else was running, and every mechanism downstream faithfully carried an
unchanged file. Two campaigns diagnosed the symptom correctly and the cause
wrongly, each building a remedy for the step it had found. The floor's second
half — a check needs an enumeration it is answerable to — is unchanged and was
right. What is added is narrower and aimed at repairs: **a remedy verified only
at the step it was built for cannot tell you the pipeline ever ran**, and the
cheapest thing that would have settled it, in either campaign, was reading the
file's own history and finding one hand-written entry.

**No bet in the map below moved.** The Sluice changes how work reaches merged
reality; it resolves no open question about the world, raises nothing from
taste-gated to self-scorable, and leaves every score below where it stood. Its
one finding about the world arrived sideways, through a survivor the merge
queue held on: a unit conversion from founding years to ledger days whose
surrounding tests asserted only *ordering*, which a uniform rescaling cannot
disturb. That is [The Ell](./chronicle/the-ell.md)'s question — *which test
goes red if this crossing is deleted?* — answered once more in the negative,
and closed with a cross-check rather than a threshold.

A fourteenth campaign returns the family to the place the first one found it —
a check written from a plan's own text — and contributes the largest tally yet
alongside a corner about *where a check is pointed*.
[The Illumination](./chronicle/the-illumination.md) (2026-08-18) rewrote what
a chart's colour, glyph and weight each mean, and produced **twenty-two**
defects, essentially all of them originating in planning and dispatch text
rather than in implementation. **Twelve share one shape**: a check that reads
as protection while not being pointed at what it claims.

Three of the twelve are worth stating individually, because each fails
differently. A **ceiling guard** was written to catch a rejected continuous
colouring that would produce a unique colour per cell; asked to *demonstrate*
rather than assert that it would fire, the rejected design measured eighteen
distinct colours against a bound of twenty and **passed silently on the exact
regression the guard existed for**. A **test whose comment claimed it pinned a
field** so that "a future change cannot add either by accident" never asserted
anything about that field at all — the predicted red, on removing the field,
never came, and the implementer reported the absence instead of implying
otherwise. And a **guard on an escape-free rendering surface** rendered a
scene containing none of the cells whose behaviour it guarded, so it stayed
green while precisely that path changed underneath it.

**Score: the bet holds; the practice gains a third clause, aimed at
positive controls rather than at checks.** This chapter already asks that a
check be made to fail on command, and that it be answerable to an enumeration.
This campaign adds: **a positive control must be shown to discriminate, not
merely to exist.** One control took the absolute value of a signed quantity to
prove the sign was irrelevant — and removing the absolute value left the test
green, because at the fixture's magnitudes both signs rounded into the same
band. It was found only because a review brief instructed the reviewer to
assume another instance existed. A control that cannot separate the two
hypotheses it names is the cannot-fire family with better manners.

**One bet moves, and it moves on a null rather than a result.** The chapter's
standing caution about drift-checked artifacts with no external anchor gains a
sampling twin. A seasonal signal was measured at eight points across a year,
declined monotonically at every one, and read as a clean null — while the same
cell's reported annual mean was arithmetically inconsistent with all eight
readings. A denser resample found the minimum sitting in the unsampled tail,
and the mechanism was a roughly twenty-three-day oscillation **aliasing**
against a forty-six-day sampling grid. The smooth decline was the beat
frequency of the instrument. So the floor gains a question asked of any
periodic measurement: *does this sampling interval share a period with
anything in the system, and would a null look identical if it did?*

**And one finding is about the world rather than about instruments, recorded
here because it scores a bet the map below carries.** The campaign's entire
colour rewrite is invisible at the view a player opens first: the settlement
walk band is 100% river on all five seeds sampled, so the default outdoor
chart withholds every tint and reports so in its own caption. That is
settlement siting behaving correctly, not a renderer defect — and no committed
page in this book reaches the colour rendering either, since the scripted path
runs with the eye off. It is the self-scorability floor in an unfamiliar
costume: a capability can be built, measured, correct, and **unobservable
through every artifact the project checks itself with**.

A fifteenth campaign contributes the thread's first entry about *what to do
after* a check is found unable to fire, and the answer is not the obvious one.
[The Drift](./chronicle/the-drift.md) (2026-08-23) replaced its preregistered
per-system gate **twice**. The median was replaced because a branch-severing
mutation left it at exactly 100.00% on all three seeds — most systems have one
branch, so a median over that population cannot see a defect that severs
*between* branches. Its replacement, p10, was replaced because two *real*
defects left it at exactly 100.00% too, and this time the blindness is
arithmetic rather than circumstantial: `pct(sorted, 0.10)` at n = 874 reads
index 87, only 27 systems sat below ceiling, so the statistic could not have
read anything else. A quantile at *q* has no resolution until roughly *q* of
the population sits below it.

**The transferable half is how the first replacement was justified.** p10 was
chosen on the strength of a mutation that moved it 100% → 35.14%, which proves
p10 *can* move — under a mutation whose population share was never measured and
which plainly exceeded the only region where p10 has resolution at all. So the
floor gains a sixth question, asked of any instrument swapped in for one that
could not fire: **has the replacement been validated against the defect class
the original was blind to, at the population share that class actually has?**
Validating against a large-population mutation and concluding "it moves"
repeats the original error with a different constant.

**And the campaign then declined to replace a third time, which is the part
worth carrying.** The arm that shipped was *also* found blind to something — a
system with no open entrance leaves its denominator while its levels stay in
the whole-world count. The distinction that stops this becoming an infinite
regress: the first two statistics could not fail against **their own class**,
and this one can, demonstrably. What it is blind to is a class the
preregistration assigned to the **other** arm on purpose, so the complement was
designed in rather than discovered, and folding the two denominators together
to close the gap would destroy the split the spec's own text demands.
**Complementing beats replacing**, and a practice that only ever swaps the
statistic will eventually swap away a working instrument.

A sixteenth campaign adds a **new position on the checkability axis**, and it
is one this passage has been circling without naming.
[The Rail](./chronicle/the-rail.md) (2026-08-29) preregistered four numbers
before writing any code, and every one of them was judged against a resolver
that already existed. The numbers were **computed by a different program**: a
script written during the design pass, which reimplemented the resolver's
demand-closure rule and got one case wrong. Fifteen rungs of a 214-rung ladder
introduce no capability token at all — they exist as free tripwires on
composition — and the script treated each one's absent token as an
*unsatisfiable demand*, so all fifteen were permanently excluded from
*covered* in every predicted figure. The first task to land a token reported
six covered where the prediction said five, said so, and used the resolver's
answer.

**The prediction was not falsified by the world; it had never been run against
the instrument it was predicting.** That is a distinct failure from anything
above it. An unpaired check is a verifier nobody invokes; an unanchored one is
a verifier with no external truth. This is a *third* thing: a fully paired,
fully anchored check whose **target value** came from a second implementation
of the thing under test. Both programs were run, both produced numbers, and
the agreement between them was the untested assumption. So the floor gains a
seventh question, asked of any preregistered figure: **was this number
produced by the instrument that will judge it, or by something that
reimplements it?** If the second, the prediction is a claim about the
reimplementation, and the two agree only by luck.

The same campaign then supplied the passage's sharpest instance of *"a
correction is unaudited text"*. Its design document's coverage paragraph was
wrong twice, in opposite directions — the first crediting the control rungs
for a count they had no part in, the second, written explicitly as a
correction of the first, asserting that none of them ever counts. Both came
from the same buggy script; neither was caught by re-reading. And a later
repair to the campaign's own command list, made to fix five steps that named
an unrunnable command, dropped one character from each step's output path in
the same edit, so every one of the five wrote one file and inspected another.
Each step then ran, exited zero, printed nothing, and **read exactly like a
clean run** — the failure this whole passage is about, authored mechanically
inside the act of preventing it. It was found by the next agent to execute the
list, not by anyone reading it.

Those two are part of a longer tally the campaign kept deliberately:
**thirteen substantive defects, every one originating in a controlling
session's own design or planning prose, none in an implementer's code, and
none found by re-reading.** That is the seventh consecutive campaign with
this distribution. Three of the thirteen run the other way and are the
cheapest lesson in the list — a controller's *stated worry* that a reader
checked instead of accepting, and found unfounded each time. A worry taken
sympathetically costs a round of rework; a worry checked costs one command.

**Score: the bet holds, and the practice gains a seventh question.** The
positive-control clause covers an instrument that cannot fire; this adds the
case where the instrument fires correctly and the number it is compared
against was never its own.

A seventeenth campaign tests the remedy the sixteenth proposed, on the
successor campaign it was proposed for. [The Quoin](./chronicle/the-quoin.md)
(2026-08-29) built its own preregistration the way The Rail's retrospective
demanded: append the five tokens under measurement, run the two live
instruments the campaign would later be judged against, record their output,
revert. All four figures — ladder covered, frontier, merchant coverage,
flood-watch demand instances — matched on the first run, at every one of five
tasks, and none was revised. **The remedy generalizes past the campaign that
discovered the need for it**, and the campaign is explicit about what that
does and does not prove: Task 0 derived the predictions with the same
resolver that later scored the outcome, so agreement confirms the
implementation did what the resolver predicted, not that the resolver is
right about the world. Three genuine defects still originated in this
campaign's own plan text — a merchant-coverage step the plan omitted, a
witness string requiring `sleep` to inflect as `slept` when the grammar's
past-tense rule is a pinned-on-purpose naive `+ed`, and a witness pairing
`Definiteness::Indef` with an expected string that needed `Def` — and every
one is a *different* class from the one PREREG-1 was built to close. All
three were caught by pre-dispatch brief verification against the tree,
before an implementer saw them, not by re-reading and not by the mechanism
under test.

The same campaign supplies a second, independent instance of *complementing
beats replacing* (the fifteenth campaign's own finding, above): its
preregistration bound the chronicle to quote a produce-side demand-instance
figure the resolver, as built through the campaign's own reconciliation
task, could not compute. Rather than report the composite the instrument
already had, or loosen what the criterion meant to fit the instrument that
existed, the campaign built the missing split as its own reviewed task,
disclosed in the generated artifact itself that the split postdates the
implementation work it measures, and reported what it found even though the
result **cuts against the campaign**: the produce-side figure (31.8%) sits
three points below the composite the report would otherwise have led with
(34.8%). A preregistration that cannot be satisfied by the instrument that
exists is a finding about the instrument, not a license to report the number
that instrument happens to produce.

**[The Avowal](./chronicle/the-avowal.md) (2026-09-01) is the clearest
instance of this chapter's founding clause the trope corpus has produced,
because the unpaired check was the corpus's *entire instrument*.**
`cli/src/tropes.rs::resolve` reported a dramatic situation "stageable" the
moment every one of its required tokens was a name in the concept registry —
a generator with no verifier anywhere behind it. Registry membership is
append-only, so the number it produced could only ever climb, and nothing
distinguished a capability the world actually computes from a predicate
somebody typed into `register_predicate` on optimism. Decision 0330 had
already named this exact hazard on the sibling sentence corpus and answered
it with a **realization witness** — a committed artifact that must
demonstrate the capability, built to fail before it is made to pass. The
trope corpus had no equivalent, and the campaign's own leverage arithmetic
showed why that mattered: 38 predicates registered on the strength of
membership alone would have carried the headline from 0/409 to 140/409 with
not one of them witnessed.

The witness this campaign built (decisions 0577→0581→0582→0583, one
supersession per review round) is a small, hard instance of "pair the
generator with a verifier": a situation is `Stageable` only when a committed
`Tableau` stages its actants and the tableau's staged relations equal —
by *set*, not merely by subset — the situation's own required `predicate:`
tokens. It shipped red first, against a one-situation corpus with no witness
registered at all, exactly as 0330 asks. And the pairing survived being
probed three times by three different review rounds, each of which found the
gate's own disclosure of its limits was itself an unpaired claim: 0577 said a
witness "binds vacuously" only in the no-predicate case, and a live probe
showed it bound to *any* situation, predicates or not; 0582's fix then closed
that and declared actant-role assignment "the ONLY disclosed limit," and a
third probe produced a `phenomenon:eclipse` requirement that resolved
`Stageable` while nothing staged an eclipse. The record that finally held
(0583) is the one that stopped asserting completeness at all — "the limits
include X and Y" survives discovering a Z; "X is the only limit" does not,
found or not — which is this chapter's own diagnosis landing a second time,
one layer *inside* the very check built to close the first instance of it.

**Score: the bet does not move — this was always a self-scorable claim, not
a taste-gated one — but the campaign is the strongest confirmation yet that
pairing a generator with a verifier is a property that must itself be
checked for completeness, not assumed once built.** The headline number
stayed at 0 of 36 and 0 of 409 throughout, which is the preregistered null
(spec §5) and not a shortfall: token completion is necessary and was never
sufficient, and the campaign's three bundle completions
(`consanguineal-kin`, `witnessing`, `act-chronology`) shortened `Blocked`
reasons without ever reaching the witness. Migration cost was zero — nothing
had ever claimed `Stageable` under the old membership-only reading, so the
new gate retrofitted no false claims — which will not be true of the next
corpus that adds a witness after its own number has already moved.

**[The Reservoir](./chronicle/the-reservoir.md) (2026-09-02) supplies the
clause's own worked example, on a claim this chapter has repeatedly named as
its hardest shape — a helper that silently still does the expensive thing
looking identical, in a green suite, to one that does not.** Migrating
`worldgen::generated(seed)` to read the fixture only on its seed-42 arm left
every other arm building exactly as before; a suite run proves nothing about
which arm a passing test actually took. So `FIXTURE` was edited to a
nonexistent filename and the target test run again, and it panicked naming
the missing path *from inside the seed-42 branch* — the only way that panic
fires is if the branch under test is the one reading the fixture, which a
build-path panic could not produce. **The positive control was then run
twice, independently**: the implementer's report carries the transcript, and
the reviewer reproduced it from a fresh copy-and-mutate rather than trusting
the pasted output, matching it byte for byte modulo the thread id. Two
independent firings of the same control is a stronger claim than the clause
has asked for anywhere else in this chapter.

**And then the same campaign supplied the control's own boundary, which is
worth more to this chapter than the success is.** The instrument was correct,
run twice, and pointed at *one* test. Two other seed-42 callers of the same
migrated helper existed to compare **two independent builds** of the world,
and routing seed 42 to the fixture left them comparing two reads of one file:
green, correctly named, and asserting nothing about a build. The control that
would have exposed both immediately — break `FIXTURE`, watch which branch
panics — was already built and simply was not aimed at them. What caught them
was a **clock**: the pair ran in 0.06 s for what should have been four ~3.0-
second builds, noticed only from a whole-branch vantage no per-task review
had. So the lesson is not "build the positive control"; the campaign did that,
well. It is that **a positive control proves a branch is reached at the site
you aim it, and choosing the sites is a separate act with its own failure
mode** — here, migrating at a helper's body and never enumerating the 43
callers whose behaviour changed. When a suite gives no signal either way, the
remaining instrument is cost: a test that got 50x faster and asserts the same
thing did not get faster.

**And the same campaign is a fresh instance of the standing diagnosis, from
the controller's own hand, with a sting the earlier instances lack: the
"correction" made the number worse.** Re-deriving call-site counts with
`grep -o '<helper>()' | wc -l` counts *substrings*, not calls —
`seam_world()` also matches inside `played_world()`, `_world()`-suffixed
names throughout the file — so the instrument inflated exactly the two files
carrying such identifiers while looking, at every intermediate total, exactly
as plausible as a real correction would. A reviewer's independent recount
with a word-boundary regex is what caught it, and the honest arithmetic
reads worse than doing nothing: the broken grep's aggregate (244) landed
*further* from the measured truth (239) than the plan's original, uncorrected
figure (240) already was. **Two further, smaller instances from the same
hand make it three, not two.** One read `tail`'s exit status out of
`make type-audit-report | tail; echo $?` and reported the pipeline green on
that basis — the status a shell pipe returns by default is its last
command's, never the one that actually does the work being checked. The
other was a path-existence check run against bare basenames rather than
repo-relative paths, testing each against the repository root and reporting
five legitimate prose references as `MISSING` — a false negative in the
alarming direction, caught before it reached a permanent record only because
the same session re-checked with the correct paths.

**Score: the bet does not move, and the campaign is worth citing for exactly
opposite reasons on its several halves.** The positive control, run twice by
two people, is this chapter's practice working as intended — proof that a
branch is reached, not merely proof that a suite stays green — and its
un-aimed siblings, found by a timing, are the reminder that the practice is
per-site. The broken `grep` is
the practice's target, not its exception: an "observing tool answering a
neighbouring question" is a shape this chapter's own reader has named before
committing it, in the same campaign, while warning three implementers about
it in their own dispatches. A correction is not evidence of correctness; it
is a second claim, checkable exactly like the first, and this one shipped
unchecked until someone else's count disagreed.

**[The Plumb](./chronicle/the-plumb.md) (2026-09-02) states the floor's
converse, which this passage has approached from one side for eight
campaigns and never turned around.** Every clause above concerns a check
that *fires* and proves less than it claims. This is the other half: **a
check's SILENCE is a claim about the check's REACH, not about the tree.**
The campaign changed a duration that varies with a world's rotation period,
which moves output on every world whose day is not exactly one standard day
— seed 42's is 87,988 ticks — and a committed byte-golden moved by 89 of
about 410 lines, 22 affect labels, sixteen of them into the class that feeds
the distress read. Two instruments were consulted and both reported nothing.
Both were correct. `make rebaseline` does not write byte-goldens, so the
`docs/generated-paths.txt` diff cannot see one; and `gate-commit` never ran
the test, because the sub-floor roster carries 386 entries for that crate
and none for this one — the same crate-level coverage rule this chapter
already recorded one layer up, arriving again on a different test. **Two
checks agreeing is worth nothing when they share a blind spot**, and a
ruling was priced against their agreement.

**The score does not move, and the practice gains its inverse clause.**
Everything above says *make it fail on command*; that presumes you know
which check should have failed. Before reading a silence as evidence,
confirm the thing that would have moved is inside what the instrument can
see — which is not a property of the check's predicate, its anchor, or its
pairing, but of its **population**. Two memory entries in this project
already stated the two halves separately and neither fired, because the
campaign had done the right thing one step earlier: it asked for the check
to be *run* rather than predicted, got an honest report, and never asked
whether the instrument could speak. Running the right command is not the
same as running a command that can answer.

## What the world can already check itself on (high confidence)

**The kernel substrate.** Hash-based seeding, coherent noise, append-only
event-sourced storage, deterministic serialization, triple-shaped facts.
Decades of precedent, thousands of implementations, and now stress-tested
under nine domains and five windows without cracking. This layer was
deliberately chosen to be boring, and the choice paid. The substrate has
since grown a shared typed-unit vocabulary — the elevation datum and the
temperature pair, ratified as standing doctrine (decision
0044) — and the way it landed belongs at this
confidence tier for the same structural reason: each migration's central
claim, *this changed nothing*, was scored by the world itself — every
committed artifact regenerated byte-identical — rather than by anyone's
judgment ([The Datum](./chronicle/the-datum.md),
[Temperature](./chronicle/temperature.md)).

That confidence is well placed and slightly too narrow, and
[The Benchmark](./chronicle/the-benchmark.md) says where. "This changed
nothing" is a strong claim about a migration, scoreable by the world — and it
is silent about whether the migration was *complete*. The elevation wave
introduced `ReferenceElevation` and left its sea-level-relative sibling
unbuilt on a stated condition; the condition was later met, nothing watched
for it, and the gap held a real defect that banded three quarters of a world's
land into a marine relief class and published it. A vocabulary can be
byte-identically correct at every step and still be missing the term that
would have made a wrong reading unsayable. So the tier is right about what it
measures and should not be read as covering coverage: the substrate scores
*changes* against itself honestly, and does not yet score its own gaps.

[The Hallmark](./chronicle/the-hallmark.md) (2026-09-02) widens what sits at
this tier and finds another edge of it in the same pass. Decision 0044 — the
doctrine this entry credits — was scoped to coherent physical *quantities*.
Decision 0517 generalizes the placement test to any type, and six shared
vocabularies moved on it in one campaign: three definitions of one error type
collapsing to one, two verbatim copies of a genesis error, a three-valued
sentiment, and a rock roster and a cave roster leaving the domain that only
*named* them. Each of those migrations made this tier's central claim — *this
changed nothing* — and each was scored the same way: a seed-42 world is
byte-identical across all of them, reproduced three times. So the entry is
right about a wider class than it was written for, and for the same reason.

The edge is that the survey licensing those moves also turned up a defect the
byte comparison could not have seen, and it is The Benchmark's point arriving
in a second shape. A deep-time field documented as an absolute day had two
producers, one of which wrote a *year* into it — smaller by a factor of
365.25 — and nothing was broken: the two paths never met at one consumer, so
each was internally consistent, every artifact was byte-identical, and no
assertion anywhere could have been red. Byte-identity scores whether a change
moved the world; it is structurally silent on whether a quantity means one
thing, because a field that means two things *consistently* is perfectly
stable. The tier's boundary therefore sharpens once more: the substrate scores
its changes honestly, is learning to score its gaps, and does not score its
own **coherence** — for that, someone has to read a field's documentation
against each of its producers, which is what a consolidation survey turned out
to be good for and was not built for.

The same substrate now scores its
own *completeness*: [The Correspondence](./chronicle/the-correspondence.md) made
every modeled concept account for its manifestation across the lexical,
perceptual, and cognitive ledgers or record a typed void, so a drift-checked
trial balance — not a reviewer's memory — reports what the world models but
cannot yet name, perceive, or think.

That instrument sat at zero for its whole life until
[The Vernacular](./chronicle/the-vernacular.md) put the first entries in it: nine
concepts declaring, in the registry rather than in a comment, that a star's
spectral class is real and that no culture here can name it. The sharpening is
worth the tier it sits at, because it is about the *instrument* and not the
reading. A trial balance that can report a class and never has is not yet known
to work — and this one, once exercised, immediately found that its own claim
evaporated across a save boundary and that the language layer was minting words
for concepts the registry had just declared unnameable. Confidence here rests on
a ledger having been *made to answer*, not on its having been built.

**The divergence method** — once the year-one research bet, now the project's
own instrument of proof. Generate two worlds differing in a single pin, hold
everything else, and measure whether the downstream culture differs *legibly*.
Year 1 varied the sky: the same land and society under a spinning sky crowned
the cyclic [Wheel-Turner](./gallery/the-gods-seed-42.md), and under a tidally
locked twin crowned the eternal Still Crown — a world with no seasons to
mythologize ([Campaign 5](./chronicle/campaign-5.md); a near-upright locked
world still has none, but [The Wandering Sun](./chronicle/the-wandering-sun.md)
later gave a *tilted* locked world its own libration season, so the Still
Crown reads now as the zero-obliquity limit rather than the whole locked
case). Year 2 inverted it,
varying the observer and holding the sky: two species differing only in their
authored parameter vectors grew different languages and religions, verified by
a 500/500 null control and a blind-attribution metric pinned honest at 0.875
([The Meeting](./chronicle/18-the-meeting.md)). This is no longer *the actual
research*; it is how the research checks itself, and it is applied afresh to
every new layer. [The Pigment](./chronicle/the-pigment.md) applied it to
colour and got the sharpest instance yet, because the observer parameter is a
single scalar: two peoples differing only in night vision descend Berlin &
Kay's ladder to different depths, so the same iron-rich outcrop under the
same light is *yellow* to a goblin and *red* to a kobold — neither holding
the word *brown* that is actually nearest. Self-scorable, and already scored:
the census pins mean hue-depth at 4 and 2 respectively, and flattening the
derivation reddens the claim.

*Sharpened by [The Beholding](./chronicle/the-beholding.md) (2026-08-07),
which pushed the same single scalar past the word and into the picture.* Two
observers derived from nothing but night vision now emit different pixels
from the same rock in the same light, and the pixels are captioned with what
the projection dropped. What makes this a sharpening rather than a repeat is
the shape of its central claim: the prediction that a dichromat separates red
from green less than a trichromat does was frozen **false**, on a measurement
taken before any code existed, with a standing instruction to ship the null.
It came true — 0.0541 against 0.0680 — without a single constant moving,
because the falsification had correctly diagnosed the *metric* rather than
the model. A chromaticity that counts an achromatic channel makes every eye
with a rod a trichromat. That is the divergence method turned on its own
instrument, which is a stronger result than another confirmation would have
been, and it lowers rather than raises the confidence owed to any
observer-varying claim whose metric has not itself been probed.

*[The Lantern](./chronicle/the-lantern.md) (2026-08-08) paid that debt on the
very next claim, and the payment is the reason the tier holds.* It set out to
show that a rod-dominant eye sees where a human does not, and the claim held at
the model level — at an illuminance of `1.6e-6` a human's emitted colour is
`[0, 0, 0]` and a kobold's is not, with the kobold's three slots **equal**,
which is what proves the pixel came from the achromatic path rather than from a
cone channel that happened to survive. But two constants in that term turned out
to be load-bearing, and both are the metric-not-model failure this bet was just
warned about. Normalizing each observer's rod by *its own* curve would have
divided a species' night vision straight back out and rendered a kobold
pixel-for-pixel identical to a human — the divergence computed correctly and
attributed to nothing. And at unit gain the rod's image falls below one screen
count *everywhere in its own regime*, so the term would have shipped green and
changed no pixel anywhere. Both were caught by probing the instrument rather
than by reading the result, which is the practice the previous paragraph asked
for; neither was visible in a passing test. The bet stays where it is, and what
this adds is a second worked instance of the same discipline rather than a
second confirmation.

The same campaign supplies the sharper caution about *populations*. Its material
claim — that two settlements on different bedrock produce visibly different
walls — held across 1505 settlements over eight seeds, at a median of 41 `u8`
steps out of 255. The tenth percentile is 1, and every sampled flagship
settlement stands on the same rock class, plausibly because the biggest
settlements go where the rivers are ([The Confluence](./chronicle/the-confluence.md)).
So the population varies and the head of it does not, and a possession starts
at the head *by default*. That default has been a parameter since decision
0116 — `--target most-populous-settlement` picks a different settlement's
head — and The Hand widened it again: `--creature <ID>` names any already-
derived roster member, so a possession need not start at a head at all. The
caution therefore narrows rather than dissolving: a divergence claim measured
over a population is not automatically a claim about what anyone will see, and
what anyone sees now depends on which body they asked for.

**Population has a physically-grounded, self-checking prior.** Every
settlement used to carry a population number a formula handed it, with no
account of what the land could support and nothing to catch an absurd
total. A carrying-capacity field, closed-form and seed-free, now stands
under every settlement in every world, and its central claim — that
supported capacity tracks the real biomass-by-latitude gradient — is
exactly the kind of bet this chapter cares about: preregistered before the
sweep, measured across two hundred generated worlds (the tropical-and-
temperate band supports roughly 27× the polar band), and frozen only after
the measurement confirmed it, not before. A second guarantee is checked
even more tightly: settlements condense as attractors of a population flow
over that field, so the sum of every settlement's population equals the
sum of the field exactly, by construction, not by tuning ([The
Gathering](./chronicle/the-gathering.md)). This is a genuine promotion, not
a full resolution — see "The standing horizon," below, for the half that
is still ahead.

*Re-scored by [The Vacancy](./chronicle/the-vacancy.md).* The self-checking
half got sharper and the modelling half got narrower, and both belong in the
score. Sharper: a committed readout of where each kind actually lives, plus a
rule that no kind may have zero capacity everywhere, caught four species — the
three chromatic dragons and the owlbear — that had zero carrying capacity on
every cell of every world and had been in the registry, absent from creation,
for four campaigns. A prior that can catch that about itself is doing the work
this section credits it for. Narrower: capacity is a supply term spanning orders
of magnitude multiplied by a condition product bounded in the unit interval, so
an authored ecological niche can only modulate the primary-production signal,
never select against it. A species authored for a particular climate can be
genuinely present there and still rank below species with no affinity for it —
measured, on a people authored for desert that has no desert at all. The
gradient claim and the conservation guarantee are untouched; what is now known
to be beyond the prior is *placing a species where its traits say it belongs*.

*Partially re-scored up by [The Warren](./chronicle/the-warren.md), which
supplies the first counterexample to the "can only modulate, never select"
half.* The reasoning above is sound about the **condition product** —
four tolerances multiplied together, each bounded in the unit interval, cannot
overcome a supply term spanning orders of magnitude. But it silently assumes
that a species' traits reach capacity *only* through that product. A realm does
not. A kind that declares itself subterranean is scored against the chamber
rather than the hillside **and multiplied by whether the cell holds a cave at
all** — a hard zero, not a bounded tolerance, on eighty-eight percent of land.
Measured over twenty-five worlds: 390,813 land cells with non-zero fit fall to
46,993, and no supply magnitude anywhere recovers the excluded ones.

So the sentence needs a qualifier rather than a reversal. *An authored
tolerance* can only modulate. *An authored realm* selects, absolutely, and is
the first mechanism in the model that places a species where its traits say it
belongs by excluding everywhere else. The scope of the win is narrow and worth
stating: it is one binary axis, carried by two fauna kinds, and it does nothing
for the desert-authored people that started this paragraph — a surface kind
still has no gate to be excluded by. Whether that generalises past caves is
open, and is the first thing a campaign placing a *people* underground will
find out.

*Re-scored down by [The Keeping](./chronicle/the-keeping.md), which contradicts
the sentence immediately above.* The gradient claim is **not** untouched — not
because the gradient is wrong, but because the measurement offered for it could
not have disconfirmed it. The polar term of that ratio is exactly zero often
enough that the metric floors it at one percent of a baseline unit to avoid a
division by zero, so a ratio computed against a floored zero is largely a
statement about the floor. The figure is recorded in the metric's own
documentation, one line from the claim it undermines. Two further problems ride
along: roughly one world in twenty is tidally locked, and a locked world's warmth
is organised around the point beneath its star rather than by latitude, so a
tropical-versus-polar comparison on those worlds samples hot and cold ground
alike and reports almost no gradient — they sit inside the pinned average, in
exactly the failure mode this section claims clearance from. And the productivity
field is not the published model its own documentation cites: that model rises
monotonically with temperature and never reaches zero, while the implementation
is a symmetric tent that reaches zero a little above freezing, which is why no
world is inhabited cold.

What survives untouched is the **conservation** guarantee — the sum of every
settlement's population equalling the sum of the field is by-construction
arithmetic, not a measured bet, and nothing here touches it. What is demoted is
the *evidential standing* of the gradient claim, which is a subtler and more
uncomfortable thing than being wrong: the reading itself is plausible, sitting
inside the band the published model predicts from theory alone. It was the
evidence that was not evidence. This chapter's own standard — preregistered,
measured, frozen only after confirmation — was met in form and not in substance,
and the campaign that found it was looking for something else entirely.

The Vacancy's re-score deserves credit here for seeing the symptom first: it
recorded that capacity is *"a supply term spanning orders of magnitude multiplied
by a condition product bounded in the unit interval,"* so an authored niche *"can
only modulate the primary-production signal, never select against it."* That is
the same defect, named a campaign early. The Keeping supplies the cause — the base
field takes the scarcer of its two limits while the layer above it multiplies four
tolerances together, so one half of the model obeys the law of the minimum and the
other half does not — and measures the resulting compression at roughly fourfold.

*Re-scored again by [The Tilth](./chronicle/the-tilth.md) and
[The Tense](./chronicle/the-tense.md), which move the claim sideways rather than
up or down.* The prior is now a strictly finer object than the one this section
was written about, in two independent ways: capacity carries a **species** index
(a cell is worth an amount *to someone*, so an authored niche can select rather
than only modulate — the defect The Vacancy named and The Keeping traced) and an
**era** index (so a glacial maximum makes ground poor instead of switching it
off). Three mutually inconsistent oracles for the word "habitable" — an era mask,
a capacity test, and a separate refugia rule, the first two disagreeing over
roughly half of all land — collapsed to one (decision 0107).

None of that is yet a promotion, and the reason is worth stating precisely. What
the arc bought is that the model can now *express* the thing it was previously
unable to say; what it did not buy is evidence that the values are right. The
gradient claim's evidential standing, which The Keeping demoted, is untouched
here — the floored-polar-term problem and the tidally-locked worlds inside the
pinned average are both exactly as they were. And the change has a measured cost
that no one predicted: replacing a gate with a continuous squeeze **compresses
the variance between worlds**. A seed that had been permanently dead now carries
36 communities across 70 sites; the flagship seed fell from 209 settlements to
122, its chief settlements losing a third to a half of their people. Dead worlds
live and rich worlds thin.

The thousand-world census puts a number on the lower half at the close, and it
is larger than the anecdote suggested: **231 of a thousand worlds could not seat
a goblin flagship before, and one cannot after.** A quarter of the sample
crossed from nameless to peopled, which is a real gain in how much of the seed
space is worth visiting, and it is the half of this trade that is unambiguously
good. The same census found flagships moving decisively inland — 73% coastal to
22% — which nothing predicted and which no bet in this chapter had claimed
either way.

Whether the middle those worlds are converging on is
the right middle is a question about the scale constant and the response curves,
and it is open — but it is now *separable* from the structure, which it was not
before, because the gate and the scale used to be the same knob.

Two known defects are named and unstarted rather than fixed. Capacity reads each
cell's **mean** temperature, and by Jensen's inequality that misestimates any
nonlinear response — overestimating near the optimum, underestimating in the
tails, and the tails are where refugia live. And `per_species_capacity` computes
a **fundamental** niche (could this species live here alone) while the bake reads
it as a **realized** one (does this species live here); ecology has kept those
apart since Hutchinson, and competition exists downstream without ever feeding
back.

*Re-scored sideways again by [The Delvers](./chronicle/the-delvers.md)
(2026-08-07), which measured which layer of the prior actually does the
selecting — and then found that selecting is not the same as separating.* Three
results, in the order they arrive. First, **which axis binds is an authoring
choice, not a model constraint**, and it has a closed form. The condition
response floors its buffer-able axes at the sovereignty floor and passes
elevation a literal zero, so elevation is the limiting axis on every cell of
every world exactly when a kind's authored elevation devotion falls below that
floor — no terrain enters the derivation. *(That flooring structure is now
**pinned** rather than merely observed:
[The Axes](./chronicle/the-axes.md) added
`exactly_one_axis_is_unfloored_and_it_is_the_undercutter`, after finding that
the existing agreement test compares the fast path against a reference which
hardcodes the same structure — so flooring elevation in both leaves it green.
The closed form above is a statement about code that can change; it now fails
loudly if it does. The bet itself is unmoved.)* Confirmed in both directions over
three seeds: the two dwarves authored below their floors are elevation-bound on
100.00% of land, and the one authored above binds there on 8.64–31.59%. That
reproduces from arithmetic alone the earlier measurement that elevation binds
everywhere for goblin, gnoll and human, and reclassifies it: the climate axes
were silent because of how the roster had been written, not because the model
cannot hear them. Second, the same kind authored above its floor has its
temperature or moisture curve binding on **67–91% of land** while both
below-floor dwarves read exactly 0.00% on every climate axis — so an authored
climate niche demonstrably can select, which the paragraphs above had left as an
open question for a surface kind. Third, and the reason this is a sideways move
rather than a promotion: decomposing capacity into its two factors and
correlating the supply factor alone across kind pairs returns
**0.99935–0.99996 on all nine measurements** — over this family the supply term
is very nearly kind-independent, so *every scrap* of per-kind spatial structure
comes from the tolerance layer. That is a sharper statement than "an authored
tolerance can only modulate," and it is deliberately narrower than it sounds:
the correlation is scale-invariant, so it measures how supply **sorts** cells,
not how large it is, and the standing claim that supply's magnitude drowns the
niche is neither confirmed nor discharged by it.

**What that bought, and what it did not, is the finding.** The kind whose
climate niche actually binds is the one *least* separated from its neighbour —
capacity correlation 0.86–0.98 against the hill dwarf, above the frozen
threshold on two seeds of three, a refuted prediction pinned as a witness so a
later separation reddens rather than passing silently. Meanwhile the pair
differing in nothing but an elevation optimum, 150 m against 900 m, separates to
0.69–0.76. **Binding and differentiating are not the same property**, and
nothing in this chapter had distinguished them before; a niche can be read, be
correctly coupled, dominate the limiting product, and still leave two peoples
ecological synonyms. The prior's honest position is therefore that it can now
place a species where its traits say it belongs, and still cannot be relied on
to place two species *differently* on that basis.

One thing this campaign was expected to settle and did not: the question left
above — whether a realm's hard gate generalises past caves, "the first thing a
campaign placing a *people* underground will find out" — is **still open**. Two
subterranean dwarves were authored and then cut mid-campaign, because they had
been given a *low elevation above sea level* to mean *deep*, and depth below the
surface and height above the sea are different quantities: a chamber under a
mountain sits high, and the curve as written selected lowland marshes. The
roster that shipped is entirely of the surface. Placing a people underground now
waits on the underworld being declared as **places** — biomes, the way the sea's
depth layers already are — rather than as a coordinate pushed through a
tolerance curve.

*Re-scored twice by [The Range](./chronicle/the-range.md) (2026-08-08), once
against a claim this chapter already makes and once against The Delvers'
result. The two moves are independent and are kept apart deliberately.*

**First, and this is a confidence-lowering event about how a claim got here:
the realm sentence above was asserted before it was true.** *"An authored realm
selects, absolutely"* was a true statement about a **readout** and a false one
about the **world**. The gate reached `per_species_suitability`, whose only
production caller is a demography report — its own comment says the figure is
never serialized and never identity — while the function that decides where
settlements actually go took no realm parameter and applied no realm gate at
all. Declaring a peopled kind `Subterranean` moves that readout from 99.49% to
5.62% of land and moves the committed seed-42 world by **zero bytes**: the same
hash, the same 7,764 facts, the same flagship village, with three positive
controls run before the null was believed. The Range repaired the identity
path, so the sentence is true now, and nothing about the twenty-five-world
exclusion measurement was wrong — it measured a quantity no world reads.

What should lose confidence is the *procedure that produced the sentence*. The
measurement behind it is the one the peoples programme mandates, and it scored
the top rung of the programme's own probe-validity ladder, whose fourth and
highest rung reads "the readout differentiates the axis". A mechanism can be
authorable, read, correctly coupled and demonstrably differentiating, and still
never touch a world; until this campaign the programme had no rung in which
that sentence could be said. There is now a fifth — **reaches world identity**,
scored by perturbing the axis and asking whether the committed world changed.
Rung four remains necessary. It had simply been reading as sufficient.

**Second, and this sharpens The Delvers rather than reversing it: a preference
applied *outside* the limiting product both binds and differentiates.** The
Range added a biome affinity — a per-kind, per-biome multiplier applied beside
the realm mask rather than folded in as a fifth tolerance axis — and froze two
predictions before the rows existed. Both were confirmed. The arid share of
gnoll's settlements rose from 0.000 to 0.500, against a baseline captured by
running the test red on an empty registry, so a downward-only mask **relocates**
rather than merely thinning. And mean pairwise Pearson correlation between
gnoll's capacity field and the other peopled kinds' **fell on all three seeds a
previous campaign published** — 0.851 → 0.795, 0.790 → 0.706, 0.857 → 0.806,
with all twenty-four individual pairs down, the instrument cross-checked
against a previously published pair value to six places.

The honest reading is not that The Delvers was wrong. It is that the two
campaigns applied a preference at **different points in the same pipeline**.
The Delvers' climate niche sits *inside* the Liebig minimum, where it competes
with an elevation axis passed a literal zero rather than the sovereignty floor,
and a factor the minimum discards cannot separate anything. The Range's biome
affinity multiplies the product from outside, where it cannot be discarded. So
the candidate this chapter now holds is that **where in the pipeline a
preference is applied decides whether it can differentiate, not merely whether
it binds** — a structural property, not a fact about niches.

It is held as a candidate and not as a law, on two grounds. It rests on one
mechanism and two authored occupants. And the alternative reading survives the
evidence: the falls are real but modest, gnoll still correlating at 0.71–0.93
with most peoples afterwards, so the peoples may be so alike in their surviving
tolerances that no factor of this kind could pull them far apart. Pearson is
scale-invariant besides, so this measures how the fields **sort** cells, not how
large they are — the same scope limit The Delvers' own supply-factor correlation
carries.

**A third thing was measured and is deliberately left without a cause.** One
authored row redistributed the entire placement. On seed 7 the total moved 274
→ 287 while gnoll — the kind whose affinity was declared — stayed at 4 and
bugbear went 49 → 153. The attribution is proven rather than argued: the
campaign's second occupant is fauna, chosen so that exactly one *peopled* kind's
placement could move, and a test rebuilds three seeds with and without its row
and asserts the complete list of (people, cell) placements is identical. So
every movement is gnoll's row alone. **Why one row moves everyone is not
established, and no mechanism is narrated here.** What the chapter should carry
is the magnitude: authoring an ecological preference for one kind is not a local
edit to that kind, and a future occupant should expect to move every people's
numbers.

*Re-scored up by [The Radiation](./chronicle/the-radiation.md) (2026-08-10),
which found that the evidence above was measured through an undeviced constant.
The bet was right; the number that scored it understated it.*

**The relocation half is confirmed far more strongly than it was scored, and the
caveat attached to it does not survive.** The affinity row's fallback level — the
factor a kind still takes on ground that is not its country — had never been
derived from anything; it entered the code as illustrative example text and was
adopted as a constant. Derived instead from the kind's own sovereignty floor, so
that a row states a *shape* and never a *level*, the same gnoll row that had been
read as *20 settlements to 2 at an arid share of 0.500* reads **13 settlements to
40 at a share of 0.825**. The count *rises* while the share rises. The caveat
built on the old arm — nine settlements removed for every one relocated, a
downward-only mask that suppresses rather than moves — was a property of the
constant and not of the mask, and it is corrected in place in
[The Range's own chapter](./chronicle/the-range.md). The two descriptive seeds
move the same way (seed 7: 67 → 31 at 0.645; seed 1234: 7 → 6 at 1.000).

**What should lose confidence is a claim this chapter's tier is built to catch:
a justification true of one consumer, silent about the rest.** The level was
defended as *gauge* — a uniform factor cannot reorder a kind's own ranking of
cells, which is true, and was the only property anyone checked. The same factor
multiplies the capacity that becomes a settlement's population, and the history
bake's volume is a function of population, so the level is gauge for one consumer
and load-bearing for the next one downstream. Measured on two arms differing in
nothing but the level, a uniform mask on **one kind of thirty-nine** removes
13.7% of a world's facts. Six rows at the old level took seed 42's history from
552 occupation records to 193 and breached four fidelity floors; derived, the same
six give 704, above the 552 measured with no such rows at all. The general form is
worth carrying past this bet: **a claim that is true and incomplete is more
durable than one that is false, because nothing contradicts it.**

**And the third measurement above — one row redistributing everyone's placement,
deliberately left without a cause — now has half a cause.** Not the row's shape:
its *level*, multiplying outside the limiting product, scaling every occupant's
capacity and therefore every settlement's population and every history the bake
grows from it. That is why one row moved a world. It does not explain the whole
magnitude, and the chapter still holds the standing warning: an ecological
preference authored for one kind is not a local edit to that kind.

**The subterranean question this bet left open is answered.** The Delvers
withdrew two underground dwarves and the section above recorded that placing a
people underground waited on the underworld being declared as *places*. It does
not, for one kind. [The Radiation](./chronicle/the-radiation.md)'s drow settles
exclusively at cave mouths — the share of its settlements on a cell holding an
enterable cave is exactly `1.000000` on all three tested seeds, with no allowlist
— and a five-arm factorial attributes about 94% of its separation from a surface
sibling to the realm gate alone, with the closure arm bit-identical, so no
unenumerated third difference exists. What is **not** answered is the question the
withdrawal was actually about: distinguishing *two* underground kinds by depth.
Drow needs only to differ from surface elves. Mountain-dwarf and Duergar differ
from each other by stratum alone, and the biome vocabulary still has no
subterranean variant, so they remain owed.

*Re-scored by [The Sources](./chronicle/the-sources.md) (2026-08-27), which
moves the precondition the withdrawal was actually waiting on.* Before this
campaign every rung in a subterranean column shared one temperature and one
moisture reading, taken once at the column's deepest point — so two kinds
differing by stratum alone would have been scored against conditions that do
not vary by stratum at all, no matter how their authored optima were placed.
Water, substrate and the new rock-derived energy term now all resolve **per
rung**, each read at that rung's own thermal offset from the surface, with
the deepest rung kept as a fixed point precisely so the change could be
checked rather than assumed. Two kinds seated at different depths in the
same column can now, structurally, read different conditions — which is the
enabling condition the withdrawal named, not yet the separation itself. It is
**not fully resolved**: the biome vocabulary still has no subterranean
variant, so nothing yet gives Mountain-dwarf and Duergar a *kind of place* to
differ by, only a set of scalars that can now vary with depth. And the same
campaign found that one of those scalars carries less discriminating power
than assumed — rock chemistry underground sorts into roughly three
near-constant categories rather than a continuum, so a stratum-only
distinction will have to lean on the axes that do vary continuously with
depth (temperature, moisture, the energy term's own magnitude) rather than
on rock type alone.

**A finding about the contest, not about elves, and it is new.** Two peoples
authored to share a mass and an affinity row have capacity fields that are
**bit-identical** over eleven to nineteen thousand land cells — and they settle on
wholly *disjoint* sets of cells, in different numbers, on every seed. The bake is
not a pure function of the capacity field; iteration order, tie-breaks, migration
and the raid comparison all participate. This chapter has assumed the field
decides placement wherever it reasons about placement at all. It constrains
placement; it does not determine it, and that is now measured with the field held
constant to the bit.

*Re-scored again by [The Muster](./chronicle/the-muster.md) (2026-08-12), which
found that the one consumer still believed exempt is not exempt either.*

**The exemption above does not survive, and the correction sharpens the general
form rather than weakening it.** The paragraph two above concedes that the level
is gauge for *one* consumer — a kind's own ranking of cells — and load-bearing
downstream. The concession was too generous. A change to the level is not a
uniform factor at all: the constructor holds a stronghold at exactly `1.00`
while pulling every lower rung down, so it changes the ladder's **contrast**
rather than its scale, and the factor then reweights biome against every other
condition inside the per-cell limiting product. Measured on seed 42 with every
authored shape held fixed, **all seven kinds carrying a shaped row have their
own cell ranking changed**, and one kind's argmax — the cell the placement
routine would choose as a stronghold — moves outright, with 5 of its top 50
cells surviving. **All eleven row-less kinds are bit-identical**, which is the
control: with no row the factor is `1.0` at every level, and there the level
genuinely is gauge.

So two claims had been wearing one sentence. *A uniform rescale of a whole row
cannot reorder that kind's own ranking* is true and is what this chapter says.
*Changing the level preserves ranking* is false, because the constructor is not
a uniform rescale. For a kind with a shaped row the level is load-bearing in all
four of its consumers and gauge in none. The general form above — a claim true
and incomplete is more durable than one that is false — now has a checkable
successor, ratified as a decision: **when a quantity is described as gauge, name
the transformation it is gauge under, and name the consumers checked.** The
short form named neither, which is exactly how it stayed unfalsified across two
campaigns while being wrong about four consumers out of four.

**And the split this bet's successor asked for is refuted, by its own
instrument.** The open question left standing was whether the level should
become two numbers, one per consumer. A preregistered sweep, its rule frozen
before any measurement, says no — with the caution that the *first* arm said
yes. Varying the level globally collapses the shipped per-kind spread to one
scalar and manufactures the opposition it then reports, because at a scale
factor of one a uniform level simply **is** the no-affinity world. Scaling each
kind's shipped level instead — ordering preserved, and the shipped
configuration reproducing byte-identically at a scale factor of one — satisfies
every band simultaneously with the shipped values interior to the satisfied set.
One quantity, correctly valued. The transferable half is a control, not a
result: **for any one-scalar sweep over a per-kind quantity, ask whether the
shipped configuration reproduces byte-identically somewhere on the grid; if it
cannot, the sweep is not interpolating the shipped world.**

*Re-scored by [The Plat](./chronicle/the-plat.md) (2026-09-03), which moves one
half of the precondition and explicitly does not move the other.* The bet's
standing precondition — placing a people underground "waits on the underworld
being declared as **places** — biomes, the way the sea's depth layers already
are — rather than as a coordinate pushed through a tolerance curve" — was one
sentence covering two requirements, and only now that half of it has moved is
the seam between them visible.

**The reading half moved.** The underworld's regions now carry named roles
derived from the descent plan: an entry, a hall at the graph median, an
innermost chamber, and a landing where a stair comes down into a node and
another leaves it. A column also carries a **tenancy** read off the ledger's
own occupation records — cut and lived in, cut and abandoned, or never cut —
and that is the first time anything underground has said *who was here* rather
than *what the rock is*. So there are places down there to declare, and a
vocabulary that already distinguishes them.

**The biome half did not, and the distance is larger than it looks.** Nothing
this campaign built is legible to a species tolerance curve. A role is a
property of a *graph node*, derived per descent from the plan's shape; a biome
is a property of a *place on the world*, read by the same machinery that reads
temperature and moisture, and the two are not the same kind of object. A curve
cannot ask "is this the sanctum" and get a number, and it should not: the
sanctum of one cave is not comparable to the sanctum of another the way two
cells of tundra are comparable. What the withdrawal actually needed — a
subterranean *kind of place*, so that Mountain-dwarf and Duergar can differ by
stratum — remains exactly as owed as it was after The Sources made the scalars
vary with depth. The shipped roster is still entirely of the surface.

So the bet is **not resolved and its confidence does not move**. What changes
is the shape of what is owed: the precondition was one sentence and is now two
findings, and a future campaign should not read "the underworld has places now"
as discharging it. The general form is worth keeping past this bet — **a
precondition stated as one sentence can have halves that move independently,
and the half that moves first makes the other look closer than it is.**

*Re-scored up, but not resolved, by
[The Tenon](./chronicle/the-tenon.md) (2026-09-04).* The Range's candidate —
that where a preference enters a pipeline decides whether it can differentiate
— now has a second live mechanism outside population placement. Rest quality
multiplies a sleeper's kind-level gain by a substrate response at the consumer,
and actual seed-1234 rooms reverse the ordering: gully-dwarf grades rushes above
ledge while drow grades ledge above rushes. This removes the caveat that the
candidate rested on one mechanism and two authored occupants; the new witness
crosses another domain boundary and reaches both committed choice and recovery.

It does **not** establish the causal half of the candidate. The Tenon compares
two species under one placement of the response term; it does not move the same
term inside and outside an otherwise fixed limiting product. The reversal proves
that a factorized preference can differentiate, not that pipeline placement is
what made it differentiate. Confidence rises because an independent mechanism
reproduces the predicted capability; the claim remains a candidate until one
mechanism supplies the inside/outside control.

**The phenomena interface generalizes.** The bet that one salience-ranked
observation interface could serve religion, perception, and historiography
without any consumer learning which system produced a phenomenon has held
across every domain that has tested it. One caveat corrects the original
forecast: *room description* was expected to ride the phenomena channel too,
and instead took a cleaner road — the semantic query surface, where the sim
emits quantities and the client renders them ([The Scene
Window](./chronicle/21-the-scene-window.md)). The interface is more general
than feared; it is also not the only interface, and that turned out to be the
right shape. The bet has now been confirmed on the *producer* side as well:
[The Elements](./chronicle/the-elements.md) added a wholly new source class —
climate's felt weather — through a `Domain`-trait roster that lets any domain
contribute observations without editing the composition root or a sibling, so
the stream is no longer sky-bound and religion can grow weather-gods where the
land is harsh.

The bet has now also been tested on the channel's *payload*, and the original
shape was wrong in one respect. A phenomenon carried a `description` string
alongside its salience, and the forecast treated that as harmless — prose the
consumer could ignore. It was not harmless, for a reason the interface's own
design implies: an observer context deliberately carries no species, so a
producer cannot know who is looking, and a stored sentence could only ever be
culture-neutral or wrong. The field's *type* guaranteed a leak that no amount of
producer discipline could close. [The Vernacular](./chronicle/the-vernacular.md)
deleted it and moved rendering to the windows, where a speaker is known — and
found the string had been serving as a **sort key**, so its removal reordered
tied phenomena and, through a positional join, moved two deities' periods. The
test written to prove the description was not load-bearing had compared the
gloss *after* the ordering ran, and so had never looked at order at all.

What the bet gets right is confirmed and sharpened: the channel generalizes
because it carries *what was observed*, not *how to say it*. What it got wrong
was assuming a description could ride along inertly. It could not, and the
correction is that a phenomenon now carries a referent and no text. One
qualification stands unresolved: `SkyReport` and `ClimateReport` still carry
domain-resident prose of the same shape, so the guarantee is currently true of
the phenomena channel rather than of the simulation.

The scene seam has since crossed a repository boundary: an
external client now consumes the same documents through a versioned wasm
catalog, byte-identical across platforms
([Goldengrove](./chronicle/goldengrove.md), decision 0055). It has also
started carrying *parameterized* quantities the client evaluates over time —
per-tile temperature elements a viewer reconstructs across the year, with
the seasonal evaluator documented normatively in one place and pinned by a
producer-sourced contract test on both sides of the boundary
([The Isotherm](./chronicle/the-isotherm.md)). The seam holds not just for
static quantities but for the small closed-form functions of them, which is
the more demanding form of the same bet — and
[The Wandering Sun](./chronicle/the-wandering-sun.md) added a second such
function (a locked world's librating-substellar temperature) and, in doing
so, sharpened what "the same function on both sides" requires: not just the
same formula but the same *point of evaluation*. Every earlier cross-seam
value the client read pre-computed off a scene layer, so the producer's
nearest-cell snapping was baked in and invisible; the first value the client
*recomputed* from position diverged from the golden by up to a degree,
because it evaluated at the tile centre while the golden had snapped to a
mesh cell. A closed-form function crosses the seam faithfully only when both
sides sample it at the same coordinate — the fix was a position-based
producer evaluator sampled at the tile centres the client uses, and the
lesson is that the golden must pin the client's computation, geometry
included, not merely its arithmetic. A refinement arrived from the
consumer's side, and it sharpens what the bet does *not* buy: **the seam
holding is not the same as the data being drawn.** A rendering debt had
accumulated silently across three producer campaigns — four layers shipped
and parsed and, in the sharpest case, fully evaluated, with
`circulation_bands` feeding a tested, normatively specified wind evaluator
that rendered no pixels at all. The seam was working perfectly while a
quarter of what crossed it went unseen. That the sim emits a quantity a
client faithfully receives says nothing about whether anyone ever looks at
it, and the discipline this bet still lacks is the check — at a producer
campaign's close, not a campaign later — that some consumer draws what was
just shipped ([The Lens](./chronicle/the-lens.md)).

The mirror image of that caveat arrived next, and it closes the pair: a
consumer can also draw a distinction the producer never made. *The Faces*
shipped `scene/moons/v1` with `bright-icy` as a surface class, selected off a
hash-derived albedo — the client rendering the **word** for an icy moon while
the model held no concept of ice, and deriving every moon's radius from an
**assumed** constant lunar density because composition was never drawn. *The
Reckoning* then drew composition for real, and for the length of one campaign
the repository held **two answers for one quantity**: an icy captured body is
~28% larger at its true density than the contract reported. Unified rather
than deferred ([The Reckoning](./chronicle/the-reckoning.md)). So the bet's
honest statement now has two failure modes on the same axis, and neither is a
seam failure: **the seam holding says nothing about whether the data is drawn
(The Lens), and nothing about whether the data is grounded (The Reckoning).**
A schema is a contract about *shape*, and both campaigns found that shape is
the easy half — a field can be faithfully transported, correctly parsed,
beautifully rendered, and still refer to nothing. The check this bet lacks is
therefore larger than The Lens made it look: not only *does some consumer draw
this?* but *does the producer actually know what it is asserting?*

[The Shadow Track](./chronicle/the-shadow-track.md) took the interface across
a fifth layer — `scene/eclipses/v1`, a *parameterized temporal* query in the
shape of tiles-region: a client asks for a day window and receives that
world's dated eclipses with their solar ground tracks. Two of the bet's open
disciplines got exercised rather than merely restated. The Lens's "does some
consumer draw this?" check was run at the *producer* campaign's own close, and
it earned its keep: the shadow band shipped, parsed, and unit-passed, yet
rendered nothing a viewer could see — it sat at a radius just above the sphere,
beneath the globe's sixty-times-exaggerated mountains, occluded from every
camera angle. jsdom and the geometry unit tests could not see an occlusion; a
screenshot could, and the fix (lift the band above the tallest exaggerated
peak) is a change no non-visual gate would have prompted. And the golden
discipline sharpened in the other direction: the campaign's plan mandated a
committed producer-sourced golden by reflex, but the whole-branch review found
it pinned nothing and contradicted the client's own documented convention. The
distinction the earlier campaigns had blurred is that a golden is the right
instrument only for a value the client *recomputes* (the climate and ephemeris
re-derivations); for a scene document the client merely *parses*, the
end-to-end fixture that reads the real wasm binary **is** the contract, and a
second committed copy only adds a thing to drift. The seam generalizes; the
check that it is *seen* is now practiced at the source; and the golden is
calibrated to the one case that needs it.

[The Turning](./chronicle/the-turning.md) sharpened the *seen* check one turn
further, and in the harder direction. Its diurnal temperature crosses the seam
as another recomputed closed-form function (the client reads a per-cell
amplitude and re-derives the waveform, golden-pinned at the tile centre it
evaluates) — but the finding was about review, not transport. The waveform
shipped physically wrong: it keyed the day/night phase to the *global* fraction
of the day, so the whole planet pulsed in unison instead of a warm band
sweeping per longitude. It passed the Task-1 implementer, its per-task reviewer
(who verified the formula matched the brief *line by line* — and it did), and
three further reviews, because **every one of them checked the code against the
spec, and the spec's formula was itself the error.** Only the producer
campaign's own visual pass — the globe run forward, the lens pulsing the entire
hemisphere at once — caught it. So the check the bet still lacks is larger than
"does some consumer draw this?": it is *does the drawn thing look like the
phenomenon?* A formula can be internally consistent, faithfully transported,
correctly parsed, and **physically wrong**, and no review that treats the
specification as ground truth will see it. The visual pass is the only reviewer
that checks the model against reality rather than against the plan — which makes
it, for physical fields, not a courtesy at the end but the gate that closes the
loop.

[The Gyre](./chronicle/the-gyre.md) carried the seam across a boundary it had
not yet crossed: the first **vector** field (an ocean current, two tangent
components per tile, where every prior layer was a scalar) and the first the
client does not merely colour but **advects** — particles swept along the field
as motion, the living globe's first real animation over a
deterministic keyframe. The interface held: the client's tangent frame is the
exact inverse of the producer's, so the flow points where the sim says. And the
*seen* check earned its keep a second time, in a gentler register — not a
physics error this time but a legibility one: the field shipped correct and
nearly invisible (faint one-pixel specks), and only the screenshot showed that
"transported and parsed" is still not "read." The bet's honest statement now
carries three failure modes on one axis — the data can go undrawn (The Lens),
ungrounded (The Reckoning), or drawn-but-illegible (The Gyre) — none a seam
failure, all invisible to everything but a human looking at the picture.

[The Selvage](./chronicle/the-selvage.md) added a fourth, and it is the one
that reaches furthest back toward the producer. A map tile's samples crossed
the seam correctly, were parsed correctly, and were drawn correctly — and the
client still assembled two adjacent tiles at the wrong edges, because the
*geometric convention* that makes them assemblable appears nowhere in the
document they arrive in. The producer walks a parameter across a tile to lay
out its rows, and that same parameter counts the tiles; so a tile's row axis
and the tile grid's own axis must run the same way, and a tile's last row of
samples is bit-identically its neighbour's first. Every word of that is true
of the contract and none of it is *in* the contract: the client had to
re-derive it from the producer's source, got the sign backwards on one axis,
and produced a discontinuity that could not exist on the real planet. So the
data can also go **drawn-but-mis-assembled** — each document faithful, the
composition of two documents wrong. The bet is unharmed (nothing crossed the
seam incorrectly) but its scope is now clearer: a versioned scene document
carries values and says nothing about the geometry that relates one document
to the next, and a consumer holding several at once is re-deriving that
relationship whether or not anyone wrote it down. The check this suggests is
cheap and not yet practiced — a contract that ships more than one tile should
state its own adjacency convention, rather than leaving each consumer to
reconstruct it from the generator.

[The Snapshot](./chronicle/the-snapshot.md) moved the seam onto an axis it had
never been tested on. Every layer before it was a **query**: a client asks the
world about a place or a day window and receives a document describing what is
there. The session document (`vessel/session/v2` today, `v1` when The Snapshot
shipped it) is an **emit** — one document per committed turn of
an interactive session, and not a view *of* the world but a view *from* an
agent inside it, which means the interesting part is what it must withhold. It
carries the redaction boundary in its own shape: channels grouped by how the
agent came to hold what it holds, rather than fields tagged with provenance, so
a pane that reads one channel does not decline to look outside it but cannot.
The seam held on the new axis, and was proved the honest way — by moving the
one pane that already existed off the prose interface and onto the document,
where it printed the same bytes it printed before, rather than by adding a
second pane that nothing could contradict.

The campaign also refines the golden rule [The Shadow
Track](./chronicle/the-shadow-track.md) had just sharpened, and the tension is
worth stating rather than smoothing: that campaign concluded a committed golden
is the right instrument only for a value the client *recomputes*, and this one
committed a golden for a document its client merely *parses*. The distinction
that survives both is not who reads the artifact but what the artifact pins.
The Shadow Track's dead golden pinned a *client contract* the end-to-end wasm
fixture already pinned better. This one pins something no client-side test can
reach: the session document is declared save-format-class, so a change in what
its bytes *mean* is an epoch event, and the committed fixture is the tripwire
that makes such a change arrive as a reviewable diff instead of arriving
silently — its own failure message names the epoch decision. An in-process
determinism test catches nondeterminism; only a golden held across code changes
catches a meaning that moved. It earned its keep on the day it was written, by
exposing a negative zero that had been folded into every unprovoked NPC's
grievance for months and was invisible to the equality test that guarded it.
And it carries a tie no end-to-end fixture could express: the newest channel in
the newest schema asserted byte-identical against the *oldest* committed golden
in the book, the published seed-42 possession transcript. The end-to-end check
exists too — the wasm smoke driver asserts the schema tag, every channel, and
narration equality against the real binary — so the campaign holds two
instruments rather than one duplicated. The cost The Shadow Track warned of was
still paid: the fixture needed a rebaseline during the final fix wave, and any
second copy of a document must be regenerated whenever the document
deliberately changes, with a rubber-stamped regeneration as the standing
hazard. Here the regeneration *was* the instrument working, and what forced it
belongs to this bet's own tally. A schema is a contract about shape, and shape
does not include **range**: the agent id was faithfully declared, faithfully
transported, faithfully parsed, and quietly truncated on arrival, because
JSON's one numeric type is a float and the producer's identifier is a
full-width 64-bit integer — wrong by 296 for seed 42, and wrong for nearly
every world. It printed correctly, which is why it survived three separate
per-task reviews: the integer, the WebAssembly boundary, and the TypeScript
annotation each sat in a different file, and no single-file review holds two at
once. The three failure modes above are invisible to everything but a human
looking at the picture; this fourth one is invisible to anything smaller than
the whole seam — and unlike them it is *mechanizable*, since nothing in the
ladder yet checks that an integer crossing into JSON fits a double. So the rule
the two campaigns jointly support: **a golden for a value the client
recomputes, or for a save-format-class document whose meaning changes are epoch
events; the end-to-end fixture for a document the client merely parses.** Where
neither holds, a second committed copy is only a thing to drift.

**Re-scored by [The Panes](./chronicle/the-panes.md) (2026-08-06): the bet
The Snapshot deliberately declined to take has now been taken.** That campaign
proved the emit seam by moving an *existing* pane onto the document, and said
plainly why — a second pane that nothing could contradict would have proved
nothing. The Panes added the second pane. It is the harder direction, because
the redaction boundary is only as real as the first consumer that could have
violated it and did not: a map pane is precisely the pane most tempted to
reach outside its channel for world truth, and the shape of the schema is what
stops it. The channel carries **semantic content, never a picture** — cells,
not glyphs — so the sim never learns how anything is drawn, and the client
renders from one document rather than from two sources that could disagree.
Both panes are now pure functions of one snapshot, which is the structural
form of the claim rather than a discipline anyone must keep.

Two things sharpen the score rather than merely confirming it. First, the cost
was **measured, not asserted**: a session-level benchmark this campaign built
prices the emit at 1.249 ms against a 0.173 ms baseline, and the payload
growth is band-dependent in a way a single figure hides — 2.73× out of doors,
1.17× indoors. The bet's premise is that the emit is cheap enough to pay every
turn; that is now a number rather than an expectation, and it paid down a
re-measurement another campaign had left owed. Second, the bet's *weakest*
seam showed itself at the merge, not during the work. A tagged union over
bands is an enumeration of another part of the sim's state space, and a
parallel campaign added a band to that space while this one ran. The merge was
textually clean; nothing in either campaign's documents mentions the other's
surface; the two agreed only because both happened to guard on the same
condition. **The generalisable lesson is that an emit whose shape mirrors
sim state inherits that state's growth, and no gate asks whether the mirror is
still total.** The seam held, and it held for a reason no test had stated —
which is the kind of pass worth recording as a narrower confidence, not a
wider one.

**Re-scored again by [The Sighting](./chronicle/the-sighting.md) (2026-08-07):
the redaction boundary stopped being merely structural and started
withholding.** Both prior tests proved the boundary by building panes that
*could* have reached outside their channel and did not; nothing had yet
required the sim to remove something a pane would otherwise have shown. This
campaign does, and the score improves for a reason that is not the one the bet
anticipated. The withholding turned out to be far harder to make **total** than
to make correct: the narrowing predicate was right in its first commit and
still leaked four times, through `examine`, a needs report, a provoke line, and
a tick's motion narration. Every leak was a surface that *narrated* a creature
rather than one that *returned* one, so none of them appeared in any
enumeration of the channel's readers. The generalisable form is that
**introducing an invariant silently promotes every existing reader of the
underlying data into a potential violation of it, and nothing in the repository
enumerates that set** — the emit seam's own shape does not help, because the
last mile of every channel is prose, and prose cannot be audited for what it
happens to mention. The structural claim survives and is now load-bearing; what
narrows is the confidence that a structurally-correct boundary is
automatically an *observed* one.

Two smaller corrections the same campaign forces on this chapter's arithmetic.
The bet's cost premise has been priced through the **actual** wasm boundary for
the first time — a turn measures 1.57–1.78× native, not the 3.6–3.8× every
derived browser figure here was multiplied by, so the seam is roughly twice as
cheap in the browser as this chapter had assumed. And the payload growth this
campaign added is eleven bytes, against a per-turn derivation cost of about
3.7 ms in release: the bytes were never the term worth watching, the derivation
is.

Every test of this bet so far has pushed on the *producer* side — new source
classes, new layers, new document shapes. [The Vigil](./chronicle/the-vigil.md)
pushed on the **observer** side instead, and the interface took it without
modification. The observation machinery — the lens, the characteristic hour,
the salience ranking — was authored for settling peoples, every one of which
has a place, a society, and neighbours. A dragon has none of these: it never
settles, so the exposure classifier that feeds it finds no biome, no
neighbouring kind, no hearth. It nonetheless observes through exactly the same
path, and the ranking it gets back is visibly its own — lunar eclipses outrank
the sun for a crepuscular creature with a dark-adapted eye. That the consumer
never learns which system produced a phenomenon is the half of this bet that
was already well tested; that the *observer* need not be a member of a society
for the interface to describe it is the half that had never been exercised,
because until now every observer was.

[The Purview](./chronicle/the-purview.md) added the sixth scene kind and the
first **egocentric** one — and with it the first document that carries an
*epistemic* field, since a situated scene describes what an observer knows and
not merely what is there. The structural news is not the schema, though, but
the scheduling: this is the first layer where the producer and a consumer that
draws it shipped in the **same campaign**, which made the "does some consumer
draw this?" check the bet has been asking for since The Lens not a discipline
to remember but one that could not be deferred. It paid immediately and in the
direction nobody was watching. The chart resolved a biome by matching the
climate domain's kebab-case name against the locale window's spaced one, and
every multi-word biome had been quietly resolving to index zero — on seed 42
all thirty-one cells reported *ice* for a tropical seasonal forest. Single-word
biomes matched by coincidence, which is why months of green tests had said
nothing. That is a **producer-side** error in a seam that had already shipped,
found only because something finally drew it; the repair was to compare enums
rather than strings, deleting the round-trip that was the defect class. The
visual pass earned its keep a second time in the same campaign, on the render
rather than the data: the chart passed every assertion while drawing a leaning
parallelogram, because the screen projection did not cancel the lattice's row
offset. So the tally of ways a faithful seam still fails gains a sixth, and it
is the mildest-sounding and the most general — **the picture can misstate the
geometry of a document that is entirely correct**, and no test written against
the document can see it, because the document is not what is wrong.

The bet itself is unmoved and, if anything, better supported: six kinds across
cartographic, temporal, orrery, session-emit, and now situated poles, none of
which required the interface to change. What keeps sharpening is the ledger of
things the seam holding does *not* buy — and the one lesson that now recurs
often enough to be a rule rather than an anecdote is that every entry on that
ledger was found by a human looking at output, never by a test.

[The Occlusion](./chronicle/the-occlusion.md) confirms that rule from outside
the scene-document program entirely, and in the plainest register available:
its four defects were found not in a picture but in **prose**, by building the
CLI and reading what it said. The almanac had been opening for most of the
project's life by naming five stars beneath a flat overcast — a sentence that
contradicts itself inside its own span — and `possess` printed `Ways on: SE,
N, SW.` and then answered `No verb 'se'`. Neither is visible in a diff, because
in both cases each half is correct in isolation: the compass parser accepted
the token and the dispatch arm that reaches it was simply absent; the weather
was computed correctly and appended after the sky was already described. A
codebase with zero TODOs across a hundred thousand lines, a default-deny type
audit, and a 2,319-test gate said nothing about either. So the visual pass
generalizes to a **legibility pass**: for a project whose deliverable is prose
about a world, reading the output is a distinct instrument from testing it, and
the same one that catches a leaning parallelogram catches a sky that argues
with itself.

The campaign also put a sharper edge on what "verified" buys. Its spec claimed
the change could not reach the save format, and *checked* that claim — the sky
report carries no serializer, confirmed by reading the derive rather than
assuming it. The check was sound and the conclusion was false, because the
exposure did not run through serialization but through genesis, where a
people's gods are derived from the sky they observe. Wiring occlusion into the
observation path cost seed 42 twenty-three of its forty-eight deities while
every gate stayed green, since the gate pins facts against the current build
and not against history. What caught it was the cheapest possible instrument,
and one no schema discipline implies: build the world before and after, and
compare the bytes. The ledger's entries were all *found by looking*; this one
adds that a determinism claim is only as good as the route it was checked
along, and that the total check — same seed, both binaries, `cmp` — costs
ninety seconds and subsumes the clever ones.

[The Sextant](./chronicle/the-sextant.md) adds a seventh entry to that ledger,
and it is the first one an instrument found rather than a human looking. A
scene document can be faithful, grounded, drawn, legible, and correctly
assembled with its neighbours — and still be **ruinously expensive to ask
for**, because a versioned schema is a contract about a document's *contents*
and says nothing about the cost of producing one. Every terrain-facing entry
point in the scene window re-derived terrain and climate from the world and
kept neither, so each such document carried about 638 ms of fixed setup;
measured against the
Orrery's real call pattern, which requests one regional document per
level-of-detail tile, **91.6% of a scene call was the planet being rebuilt**
and a single camera move spent roughly fifteen seconds generating the same
world two dozen times. ([The Cistern](./chronicle/the-cistern.md) closed that
the following day — the derivation now happens once per world, and a region
patch measured 11.1× cheaper. The entry stays on the ledger because the
*failure mode* is what it records, not the defect's lifetime. [The
Winnowing](./chronicle/the-winnowing.md) then took the residual The Cistern
named: the globe document's cost is no longer redundant *derivation* but sheer
*volume*, and the schema said nothing about that either. A caller may now name
the per-tile layers it will read — the eight the Orrery's parser actually
extracts are 46.3% of the bytes — which is the same lesson one turn on. A
contract about contents says nothing about the cost of producing a document,
and it says nothing about which parts of one a consumer will use.) The bet is
untouched — nothing crosses the seam incorrectly,
and the interface required no change to be measured. What sharpens is the
same scope lesson The Selvage drew about geometry, transposed to cost: a
document describes itself and not its relationship to the *other* documents a
consumer holds, and a consumer's calling pattern is exactly such a
relationship. So this entry is also the ledger's counter-example to its own
recurring rule. Six failure modes were found by a human reading output; this
one is invisible to reading — every document is correct — and visible only to
a fixture shaped like the consumer's session, since redundancy is a property
of a sequence of calls and cannot appear in any one of them.

[The Pyx](./chronicle/the-pyx.md) adds an eighth entry, and it generalizes the
ledger's own recurring rule one step further. The Cartographer's lesson was
that a determinism claim is only as good as the *route* it was checked along.
The Pyx's is that it is only as good as the **apparatus** it was checked on:
every check in this repository regenerates on the canonical box and compares
against a golden authored on the canonical box, so the machine sits on both
sides of the comparison and cannot be what the comparison detects. In the
vocabulary metrology uses for exactly this distinction, the project had been
enforcing *repeatability* and describing it as *reproducibility*. The audit
that closed the gap found nothing wrong — a full census reproduced on its
authoring host eleven days later with zero bytes different, two clean builds
of one commit hashed identically, and a forty-world all-metric probe was
byte-identical between x86_64/Linux and aarch64/Darwin, including the one
seed whose count decision 0063 had recorded two
machines disagreeing on. The bet on deterministic serialization is
**strengthened, and for the first time by evidence from outside the machine
that authors the goldens**. What sharpens is the scoring instrument rather
than the claim: the cheapest sufficient check turned out to be a comparison of
*binaries* rather than of outputs, which nobody had tried and which the
campaign's own frozen prediction said would not work.

That instrument then needed a correction of its own, and the correction
belongs on this ledger as much as the entry does. Two builds of one commit in
two directories hashed identically on the canonical box, and the campaign
generalized from that single host to a property of the toolchain. Repeating
the comparison on the second machine produced two *different* binaries, each
carrying the absolute path it was built in — a path written in deliberately by
ordinary code asking where its own source tree is, not by debug information as
first supposed. So the oracle is real but conditional: it holds when both
machines build at the same absolute path, which an image supplies for free and
an ad-hoc checkout does not. The entry's shape is therefore the ledger's rule
turned on the ledger's own author — a claim verified on one apparatus is a
fact about that apparatus, and the campaign that had just finished saying so
in prose went on to forget it in a decision record within the hour.

[The Twin](./chronicle/the-twin.md) closes that correction and sharpens the
bet itself. Holding the compiler, the system library, and the build directory
fixed, two machines that share almost nothing else — different processors,
kernels, and operating systems, one of them an appliance that cannot be logged
into — produced the same binary to the byte; and two *different* system
libraries produced the same forty worlds, even compiled the old way, where the
operation everyone suspected leaves the program and enters the library. So the
long-standing explanation for the one recorded cross-machine disagreement is
eliminated, and with it the machine itself. **What determines the output is
the environment, not the host** — which is the strongest form this bet has
been stated in, and the first version of it supported by a comparison in which
only one thing varied. Every earlier cross-machine check in this project moved
five things at once, which is why the disagreement of nineteen July was
observable for four months and diagnosable for none of them. It is still
unexplained. The space it can hide in is now small enough to name: how a build
chooses its compiler, given that this project's pin is silently conditional on
the directory you invoke it from.

[The Escapement](./chronicle/the-escapement.md) (2026-08-24) adds an entry of
a different shape than any before it on this ledger: **byte-identical is not
the same claim as adequately precise, and this project had been treating them
as one.** Every prior entry asked whether two apparatuses agree on the same
computation; this one found that the computation itself, `WorldTime`'s
eight-significant-digit rounding, was internally consistent across platforms
and still wrong — a committed instant's resolution decayed with world age
because time was the one quantity in the system with no bound on its own
magnitude, and significant-digit rounding buys constant precision only when
magnitude is bounded. Both machines agreed, faithfully, on a value that was
losing a day of resolution by world-year 200,000. The sharper form of the
finding is not the deep-time table, though — it is that the identical defect
also produced a present-day correctness bug (a fact failing its own `d <= t`
read-back filter, because the rounding that makes two platforms agree can
still round a value *away* from itself), found independently by a second
campaign that had never heard of the first and was not looking for a
determinism question at all. `WorldTime` is now an exact `i64` tick count and
has left the quantize contract entirely rather than being rounded more
finely (decisions 0186/0188) — the strongest form of "agrees across
platforms" a quantity can have, an exact integer with nothing left to round.
The bet on deterministic serialization is unmoved by this — nothing here
contradicts a prior finding — but the ledger's standing question sharpens
once more: **agreement between two apparatuses says nothing about whether the
quantity they agree on is the right shape for what it is measuring**, and
the only reason this instance surfaced at all was a defect that fell far
enough from a determinism check's usual territory that nobody thought to
look for it there.

**Re-scored by [The Blocking](./chronicle/the-blocking.md) (2026-07-28): one
entry on that ledger is now mechanized, and the move that mechanized it is
worth more than the check.** The entries above are all forms of *the drawn thing
does not match the thing* — undrawn, ungrounded, illegible, mis-assembled,
misstated geometry — and the standing lament is that only a human noticed. The
Blocking's parity contract turns one sub-class into a test: **every noun the
render depicts must answer to `examine`, and every destination it depicts must
be reachable by a named command.** That is precisely the class of defect that had
shipped one campaign earlier, where `look` named a water jar and `examine`
denied it, and it is checkable because the render and the command language are
required to derive from *one* model rather than to agree by vigilance. The
structural half is what makes the tested half possible: a pane input
**synthesizes a command** — an arrow key emits `go n` and the existing verb runs
— so there is one implementation and no second path to drift from. The accepted
cost is permanent and is the reason this is a bet moving rather than a feature
landing: any future pane capability must first be a verb, so nothing will ever be
expressible only by pointing.

The honest scope: this does not close the ledger, it converts one row. A plan
whose every glyph answers can still be *ugly*, and legibility remains
taste-checked — the campaign's own render had to be reworked once because a model
that was faithful drew no walls at all, which no assertion caught and a human
reading the picture did. What changed is that "the render depicts something the
command language denies" has stopped being a thing a human must remember to look
for.

**Re-scored again by [The Handle](./chronicle/the-handle.md) (2026-08-06), which
refutes that last sentence and narrows the row.** The parity contract was real
and it held — for the floor plan it was written against. It said nothing about
the other four surfaces that name things: the room's own prose, the sky, the
chart legend, and the underworld. All four were denying nouns they had just
printed, and the way it came to light was a human reading a transcript and
saying so, which is exactly what the sentence claimed had stopped being
necessary. Six of the seven significant words in the starting room's catalog did
not resolve.

The correction is about the *scope of a mechanization*, not its value. A check
converts the row it covers and leaves the rest of the ledger reading as though
it were covered too — which is the more dangerous state, because the lament that
"only a human noticed" gets quietly retired while remaining true everywhere the
check does not reach. The Handle widens the check from one render to every
catalog surface, and it also finds the limit of the wider version: the check
asserts that *declared* nouns resolve, and is structurally blind to
over-admission, which a mutation demonstrated and only a separate,
opposite-facing test caught. So the row now reads: parity is mechanized in both
the plan and the prose, in one direction, and the second direction is held by
regression tests rather than by a rule.

**Re-scored by [The Quire](./chronicle/the-quire.md) (2026-08-09): the seam
gets its first consumer that structurally *cannot* cheat, and its first
finding of incompleteness rather than of error.** Every earlier test of this
bet was in-tree. Both browser panes are compiled beside the sim; the wasm shim
links it. Their discipline was real and it was a discipline. The Quire's render
crate is outside the cargo workspace and has **no dependency on the simulation
at all**, so the methods a client must not reach — whether a creature would
turn hostile, its grievance against you, the knowledge store — are not
discouraged but absent: there is no symbol to reach. The same move applies one
level down, where the client's hand-written mirror of the schema simply does
not declare the channel the schema itself warns is world truth, so the
forbidden pane cannot be built rather than must not be. This is the first time
the redaction boundary has been enforced by what was *not written down*, and it
is a stronger form of the claim than any prior test produced.

What that strength bought was a negative result, which is the news. A client
that renders only the emitted contract is standing evidence that the contract
is renderable — and this one found a place where it is not. The session's
spatial channel splits `walk` from `chamber`, where `walk` means *not inside a
built structure*; being submerged and being underground both fold into it.
Underground, a pane drawing the exits from the document drew the surface's
exits, while the prose in the same document said *out*. Nothing on the wire
distinguishes the two states except the literal word inside the prose, which no
consumer may parse. So the ledger above gains an entry of a kind it did not
have: not the drawn thing failing to match the thing, but the **document
correctly describing less than a consumer needs, with no defect anywhere to
point at**. Its resolution is instructive too — the pane was deleted, because
the client never needed the exits: command parsing lives in the sim, the key
mapping sends its verb unconditionally, and an invalid move is answered with a
sentence. A consumer's requirement for a channel should be checked before the
channel is designed.

**Re-scored by [The Gallery](./chronicle/the-gallery.md) (2026-08-29): the
exact gap The Quire named — a wire that cannot distinguish two states except
by an unparseable word in the prose — is closed for one of the two states it
named, by giving the state its own wire value rather than a better parser.**
`SpatialChannel` gains `Underground`, carrying its own schema
(`vessel/level/v1`); the pane now emits `band: "underground"` and the `map`
verb draws the same level, with a test (`the_pane_and_the_verb_agree_
underground`) asserting they cannot drift apart. The document no longer
"correctly describes less than a consumer needs" for this state — a consumer
reading the tag alone now knows what Quire's consumer could only have
guessed from prose. **`submerged` is deliberately left exactly as Quire
found it** — still folded into `walk`, still distinguishable only by prose
— because the water column has no lattice for a pane to draw yet; the
frontier row this campaign moved is narrowed rather than closed, and says so
itself. One new limitation surfaced in the same campaign's own closing
audit, of the *other* kind on this ledger — not a wire ambiguity but a
picture that can silently run off its own edge: the pane anchors a
generated level's own corner to a fixed-width plate with no camera-follow,
so a rung wider than that plate (which is EVERY walkable rung — ranks 1-5
run 44x26 to 60x34, and none of them fits) can walk the
player's own marker off the visible screen. Recorded as its own frontier row
rather than left for a future campaign to re-discover the way this ledger's
own Risk section had to name it before any
code existed.

**Amended by [The Lodestar](./chronicle/the-lodestar.md) (2026-08-30), and
the amendment is this ledger's own subject turned on the campaign that
thought it had settled it.** The paragraph above was true of the TAG and
false of the RENDER. The wire carried the underground band's creature in
`SessionLevel::marks`; the terminal client had no marks-drawing pass at
all, having been written one task before the derivation that fills them and
never reopened. So "a consumer reading the tag alone now knows what Quire's
consumer could only have guessed" described a consumer that was throwing
the payload away. Twelve per-task reviews each passed; a whole-branch review
run after the merge found it in the first pass, because the gap lived
between two tasks and belonged to neither.

Worse for the claim, and better for the ledger: **there are two client band
mirrors and the campaign updated one.** `clients/vessel` still declares its
spatial union as walk-or-chamber and goes blank underground, silently,
with its own gate green throughout — and the workspace tripwire built during
that campaign to catch exactly this is hardcoded to the other client's test
directory. The wire fix was real and the consumer-side gap simply moved down
a level, which is the shape this row has now taken twice: **a channel
correctly describing what a consumer does not read is indistinguishable, from
the channel's side, from one that describes too little.** Recorded as
`CLIENT-second-band-mirror`. The Lodestar fixes the first mirror and leaves
the second registered rather than half-fixed.

One older row recurs and one instrument for it becomes mechanizable. The
recurrence: the outdoor chart was geometrically wrong — the sim's own render of
the identical thirty-one cells is five dense rows and the client drew nine
sparse sheared ones — under seventeen green tests and four mutation proofs,
which is The Purview's leaning parallelogram a second time in a different
codebase. Twice is a structural property of picture-versus-property, not an
anecdote. The instrument: the repair pinned the client's projection against the
sim's own ASCII rendering of the same document, byte for byte, and the indoor
plate was pinned the same way pre-emptively and passed. Where the repository
already contains a second, independent renderer of a document, "does the
picture state the document's geometry" stops being taste and becomes a
comparison — which is the first time a row on this ledger has been converted by
something other than a human remembering to look.

**Re-scored by [The Rhumb](./chronicle/the-rhumb.md) (2026-08-16): the
over-admission direction finally produced a defect, and it was in the oldest
verb in the game.** The Blocking's parity contract mechanized one row of this
ledger — *every destination the render depicts must be reachable by a named
command* — and The Handle then narrowed it twice: the check covers declared
nouns only, in one direction, and is structurally blind to over-admission,
which was left "held by regression tests rather than by a rule." The Rhumb is
what that blindness was hiding. `go` accepted eight compass tokens and
dispatched all eight correctly, over a triangular lattice with exactly three
edges per cell, so **five of eight were answered with a refusal** — and which
three worked depended on the orientation of the triangle underfoot, so no
sequence of `go e` walked east. Nothing here is a seam failure: the document
was right, the picture was right, the parser was right, and the dispatch arm
The Occlusion had added was right. What was wrong is that the interface's
*vocabulary* was larger than the world's geometry could honour, and no test in
either direction was looking at the ratio between them. So the ledger gains an
entry of a new kind — **the interface over-admits, and the substrate silently
declines** — and the honest note is that its detection was, once again, a human
typing a direction and watching what happened.

The row also gains its first hard limit on what a mechanization *could* have
caught. The repair keeps the graph fixed and puts the heading in the walker
(decision 0141), which makes all eight directions resolve — but the campaign's
preregistered bound on how faithfully a walked cell can track the ideal course
is **false, and false without limit**: 172.6 step-lengths of drift at 2,000
steps, growing linearly, governed by the local triad's alignment rather than by
latitude. That is not a defect any parity check can convert, because both sides
are behaving correctly; it is the tiling declining to represent a continuous
curve. A checkable contract can require that every offered destination exists.
It cannot require that the ground be able to go there straight.

**Re-scored by [The Quadrat](./chronicle/the-quadrat.md) (2026-08-27): the
comparison instrument this row converted was measured unavailable on a second
surface — and the campaign's first statement of *why* was wrong, which is part
of the re-score rather than a footnote to it.** The Quire's contribution here
was to turn "does the picture state the document's geometry" from taste into a
byte comparison against the sim's own renderer of the same thirty-one facets.
The Quadrat set out to move that pin — reprojecting the perception packet onto a
square raster on both sides so the comparison survived the change — and did not.
The packet describes each perceived facet as a *relative polar offset*; a raster
addresses *absolute* tiles reached by flooring a projected coordinate; and
converting one to the other requires the observer's position *within* its own
tile. Reprojecting without reconstructing that, swept over 200 sub-tile phases:
at worst 24 of 31 marks misplaced, mean 11.5, and only 2 of the 200 phases
exact. The agreement test that was supposed to guard the change could only have
passed by being weakened to "within one tile" — that is, by asserting the
defect.

**The campaign then wrote that the wire does not carry that phase, and the wire
does.** The observer block states the centre's own centroid latitude and
longitude, and the offsets are centroid-to-centroid great-circle quantities, so
the spherical direct problem recovers every facet's absolute coordinate exactly.
Quantization does not stand in the way either, and stating that accurately
matters here of all places: the eight digits are *significant*, not decimal, so
a latitude near the equator keeps a centimetre of ground while a longitude of
large magnitude keeps only sub-metre rounding — against a facet 1.87 km across.
What the render crate lacks is narrower and entirely its own: its parsed mirror
of the document drops the observer block, and the simulation offers no inverse
of the bearing-and-distance construction for it to call. The sweep measures the
shortcut, not the contract.

That makes the resolution the interesting part rather than the obstacle. The
layer moved to the client crate that already holds the mesh, where each facet's
own absolute address — `room`, a packed identifier the packet carries outright —
needs no trigonometry at all, and the two pictures now agree **by construction**,
one projection called from both sides, rather than by two computations being
compared after the fact.

The bet is unharmed and its scope is now stated more exactly, in the corrected
form. A document may be complete and still ask real work of a consumer that
wants to place it in the world's frame: either spherical trigonometry the
consumer writes itself, or a dependency on the simulation to read the exact
address. A zero-dependency renderer remains the strongest available evidence
that a contract is renderable, and what this campaign found is the shape of the
work such a renderer must do for anything the document expresses in the
observer's frame rather than the world's. Whether that work should be removed
(by putting an absolute coordinate on the wire beside the relative one) or left
where it is (as the ordinary price of an observer-relative projection) is not
settled here; the campaign records that agreeing by construction is strictly
better than agreeing by comparison wherever one projection can serve both sides,
which is a stronger form of the instrument this row already carries.

**Re-scored by [The Pavement](./chronicle/the-pavement.md) (2026-09-01): the
row's one hard limit on what a mechanization could catch was a property of a
choice, not of mechanization — and the way it resolved is worth more than the
fact that it did.** The Rhumb left this ledger its sharpest negative result: a
checkable contract "can require that every offered destination exists" but
"cannot require that the ground be able to go there straight," because the
drift it measured — 172.6 step-lengths at 2,000 steps, growing linearly — came
from both sides behaving correctly. It was the tiling declining to represent a
continuous curve. That reading was exactly right about the tiling it was written
under, and it quietly carried a premise: that the tiling was fixed and the
walker was the only thing left to move.

Decision 0141 had made that premise explicit, and justified it — editing room
adjacency would silently change ecology, settlement fitting and path costs. The
justification was checkable and false. At 0141's own commit, `git grep` for the
room-adjacency accessor across every domain crate returns nothing, and it
returns nothing today: adjacency had only ever been read by the layer that draws
the player. So the ground was movable the whole time, and a bet was scored as
structurally unmechanizable on the strength of a constraint that did not exist.

Moving it dissolves the category rather than improving the number. On an
eight-connected quadrilateral lattice there is no ideal course for a walked cell
to drift from, so the drift is not bounded — it is *absent*, and the suite that
measured it is retired as dissolved rather than passing. The ledger's entry of a
new kind, *the interface over-admits and the substrate silently declines*, is
answered by widening the substrate to the vocabulary rather than by narrowing
the vocabulary to the substrate: all eight compass words now resolve to real
edges, and the diagonal that a two-walled corner refuses is refused for a stated
geometric reason rather than by an accident of which triangle lay underfoot.

The re-score the row actually gains is methodological, and it cuts against the
comfortable reading of every other entry here. This chapter records instruments
that convert taste into comparison, and it has learned to treat "no mechanization
could have caught this" as a finding. The Pavement's contribution is that such a
verdict inherits every assumption its author was standing on, and the load-bearing
one is usually not the geometry — it is a decision record that sounded settled.
The check is cheap and was never run for four months: take the reason a decision
gives, and grep for it. Two of this campaign's four measured distortion figures
also moved under the same discipline, and the projection its own specification
first named would have been *worse* than the mesh it replaced — 5.2x against
1.5-2x — which nobody would have discovered by reasoning about addressing.

What does not change is the bet's direction. A zero-dependency renderer is still
the strongest evidence a contract is renderable, and the parity contract still
covers declared nouns in one direction only. What changes is the standing of a
closed question: a superseded decision does not merely stop being true, it keeps
producing correct-looking answers from readers acting in good faith, and this
ledger is one of the places those answers accumulate.


**Terrain shape has Earth-anchored, self-checking acceptance bands, and the
one that stayed open resolved by superseding its own instrument rather than
closing under it.** The Measured Coast preregistered six Earth-anchored
shape metrics (shoreline development, hypsometric bimodality, shelf
fraction, continent count, largest-continent share, plate-size Gini) before
any generator change, exactly the kind of bet this chapter cares about: the
Laboratory generates the evidence, measures it, and drift-checks the number,
with no human judgment call about whether a continent "looks right." Crust's
epoch closed four of six by direct measurement, refuting two of its own
predictions along the way (the tanh-lobing pinch-off hypothesis, the craton-
repulsion hypothesis) before a read-only probe found the actual cause
(sea level sitting in the abyssal plain). Sculpting's epoch closed a fifth —
shelf-fraction, via a wave-cut coastal-erosion mechanism the tuning season
built only once measurement showed the band demanded it — and left the
sixth, shoreline-development, on an honest open verdict: every mechanism the
spec banked for it is now built, the metric moved a real +8% under
measurement, and it still sat below its floor. A dedicated diagnostic
instrument established *why* the floor was hard to reach — the estimator is
not saturated, but the floor's own anchor was partly built on coastline
noise a since-removed generator had produced — and handed the open band
forward, with its evidence, to a named future campaign.

That campaign, rift-and-fit, resolved the question, but not by closing the
band. Its own fitted, continental-scale rift moved the metric the wrong way
(lower than Sculpting's, not higher), which turned out to confirm the
diagnostic rather than refute it: the estimator rewards single-hex-scale
coastline texture almost exclusively, so a large-scale geometric fit was
never going to move it much, and the one lever that does move it (cell-scale
texture) hits a hard fit-verification ceiling before it can close the gap.
The campaign then measured the real planet through the exact same,
unchanged estimator for the first time — and Earth's own coastline scored
*below* the floor every generated world had been held to. A floor no real
planet clears is not a floor; it is the contaminated anchor the diagnostic
had already flagged, now proven. The band was superseded, not closed: an
Earth-anchored range now serves as a sanity floor rather than an acceptance
gate, and single-scale coastline complexity stepped down from this
project's headline shape criterion to a tripwire against degenerate output.
The more interesting bet this surfaced is banked, not built — a coastline
score is the wrong shape for a pass/fail band in the first place, because a
coast is a variable meant to *vary and drive something else* (habitat edge,
harbor geometry, a maritime-versus-continental cultural split) rather than
sit at a constant the Laboratory checks once and forgets. This is the
chapter's discipline working exactly as intended, one step further out: a
bet stays open for a structural reason, and when it resolves, it can resolve
by convicting the instrument instead of the world. See
[Crust](./chronicle/crust.md), [Sculpting](./chronicle/sculpting.md),
[Rift-and-Fit](./chronicle/rift-and-fit.md), and [The Census of Coasts
IV](./laboratory/census-of-coasts-iv.md).

[The Threshold](./chronicle/the-threshold.md) exercised this discipline on a
bet of its own and got the answer the discipline exists to make possible: **no**.
It froze, before a line of code, the claim that a cold creature with a fire in
its house would suffer the cold measurably less than one without — and then
failed to find it, four times over. What makes that worth recording is not the
failure but its shape. Each null was designed to kill one candidate explanation
and did: the fire was too faint (so it was recalculated from an energy balance —
envelope, infiltration, hearth power, the radiant crowding that is why people
sit close — the argument written down and committed *before* anything was
measured again); then the creature never reached the fire (so it was taught to
cross the room, and did); then the instrument could not see where the creature
stood (so it was taught to look). Warmth, walk, and witness each eliminated in
turn, the remaining explanation is not about the machinery at all: **the
creatures who live where it is cold are either already within their own
tolerance, or forty to eighty degrees beyond anything a domestic fire could
offer.** There is nobody in between for a hearth to save.

Two things follow for this chapter. The first is that a preregistered
prediction is only as good as the *sequence* of measurements behind it — a null
that eliminates nothing is a wasted run, and four that each eliminate something
are a result. The second is subtler and concerns the anchor: this bet's own
warning is that a drift check pins output against change and has no opinion
about whether the output was ever right. Here the analogue bit at the level of
the *instrument* — an acceptance protocol verified byte-identity with a command
that could not, by construction, reflect the layer being changed, and four
stages of evidence were vacuous before anyone noticed. The check that a
measurement can move at all belongs beside the measurement, not after it.

[The Millrace](./chronicle/the-millrace.md) supplies the sharpest measurement
of that cost this chapter has, and it is a count rather than an argument. One
of the census columns this chapter counts among the world's self-checks —
`channel-connectivity`, which asks whether a tributary's junction with its
trunk stays inside the channel band — was asking its continuation question
with a stricter test than the network's own, so **82.83% of its walks ended
before reaching the join they existed to test** and scored intact without ever
testing one. The column read 1.0000. Repaired, on sixty-four worlds, it still
reads 1.0000 — the value was right and the claim behind it was empty, which is
the exact failure a drift check cannot see. The campaign then went looking for
the same shape in its own work and found it **five more times**, twice inside
the repair for the first instance and once in the file whose own documentation
is a warning about it. Two consequences for this chapter's confidence
accounting. A column's *value* being stable across campaigns is evidence about
the world only if something independent establishes the column can move at all;
and the discipline that catches these is not review but **mutation** — every
one of the six was settled by neutralising the code under test and watching
whether anything went red. Nothing in the standing gate does that for a
Laboratory metric.

[The Hand](./chronicle/the-hand.md) adds the clause about what to do once you
find that the instrument is structurally blind. Arc II of The Bridle predicted
that routing a possessed body through the creature tick would not change the
ledger's growth rate, on the argument that a driven body *holds* most ticks and
holding commits nothing. Measured before and after: 0.25 facts per body per
tick, identical. The prediction held and the argument behind it is not what
produced the number — the driven walk's facts are discarded unconditionally
before they can reach the ledger, so the rate is flat **whatever the controller
answers**, and forcing the controller to return a real action leaves the
guarding test green. The same measurement had already been vacuous once, for an
unrelated reason, against an earlier implementation that never touched the
ledger at all: two vacuities with different causes, both hiding behind the same
confirming number. What the campaign did next is the part worth keeping. It
declined to run a third measurement and corrected the specification instead. A
prediction whose instrument cannot separate it from its own negation is not
confirmed by agreeing with it, and the residue that survives — *routing a
possessed body through the tick costs no committed facts* — is a strictly
smaller claim than the one preregistered. Recording the smaller claim is what
keeps this chapter's accounting honest; staging measurements until one agrees
is what would corrupt it.

[The Coercion](./chronicle/the-coercion.md) hit the identical shape one arc
later, from the other direction: this time the instrument was blind *before*
anyone ran it, and the blindness was legible in the source. Arc III's two
preregistered tests compare a body driven by an imposed controller against
the same body under its own default controller, expecting the committed act
trail to match. It does — byte-for-byte — and the reason is that the imposed
controller is, today, a stateless pass-through to the very controller it is
being compared against: no fixture, however varied, could have produced a
different number, because the two code paths are one path wearing two names.
A result that is deducible from the source before the code runs is not
evidence the fixtures gathered, and an early draft of this campaign's own
report called the two-fixture pool "a genuine, if weaker, corroborating
measurement" — read, correctly, as overclaiming what a tautology can support.
What is left after the correction is smaller and considerably more
interesting than what was preregistered. The controller stack had never
actually been wired into where a driven body walks on its own; wiring it
in changes nothing the ledger sees (the walk's facts are discarded either
way) and changes something the ledger never held to begin with — a free
body waiting idly holds, and a possessed one keeps arbitrating, so the same
seed under the same clock reads `Pursuing(Fatigue)`/`Eager` free and
`Idle`/`Content` held. Two lessons stack on The Hand's. First, a stub
implementation can make a preregistered test **trivially** true rather than
either confirmed or falsified, and the honest move is neither to hide that
nor to delete the test — it stays as a regression guard for the day the stub
grows real logic, correctly labelled as guarding rather than discovering.
Second, the real finding this chapter should count was never staked in the
spec at all: it surfaced because a body already known to be co-present (The
Hand, above) was, for the first time, actually driven by two different
controllers in the same measured walk, and the divergence was there to find
once someone looked.

*Corrected 2026-09-03 by [The Minute](./chronicle/the-minute.md): both
paragraphs above state the discard in the present tense, and the discard was
itself a defect rather than a design — a held body's walk now commits what it
does. That does not change either lesson, and it sharpens the first: the
instrument The Hand called structurally blind was blind because of a bug, and
the measurement it could not make is now makeable. Read the two present-tense
sentences as descriptions of the code as it stood on those days.*

[The Offer](./chronicle/the-offer.md) pushes the same thread one step further
and supplies this chapter's largest single count of the failure. Arc IV.a
shipped with **seven** checks that could not have failed: two asserting
properties true of any implementation whatever (a subset relation over a
`filter` re-proves an invariant of `filter`, not of the predicate inside it),
one source scanner whose pattern could not match the only syntax anyone would
ever write, two entire specified features with no reachable firing case, and
two production paths held by no assertion at all. The distribution is the part
worth recording rather than the total. **Every one originated in the planning
prose, not in the implementation**, which is the third campaign running to
report that shape; and **not one was found by reading** — each died to a
command someone ran, five of them to a mutation that neutralised the code under
test and left the suite green. Two of the seven were caught before any code
existed, by a grep and by a sixty-combination census run during pre-dispatch
verification, which is the cheapest place this chapter has yet seen one caught.

The remedy the campaign adopted mid-flight is the transferable half, and it
sharpens The Millrace's "the discipline is mutation, not review" into something
a plan can be written against: **specify a regression test by the mutation it
must fail, never by the property it should assert.** A property can be asserted
vacuously; a mutation cannot be failed vacuously. The campaign then found the
rule's own failure mode on the last page — a reviewer's illustrative mutation
that was itself non-discriminating, because the two anchor kinds it swapped
between are perfectly co-located in every room the grammar composes, so the
substitution changes nothing observable. It was caught by running it. **A
mutation is evidence only if something establishes it could have moved the
result**, which is the same clause this chapter already carries about a
column's stable value, arriving one level down.

[The Latch](./chronicle/the-latch.md) continues that distribution and then
breaks it in one place — fourteen defects, every one originating in controller
prose, a fourth campaign running; thirteen caught before they reached committed
code, and **the fourteenth shipped**, removed only after the merge candidate
was assembled. Its **shape shifted too, and the shift is the entry worth
keeping.** The early ones were wrong identifiers a grep catches: a test helper
that does not exist, a constructor asserted to return a
bare value when it returns a `Result`. The late ones were **internal
contradictions no grep can find.** One brief named an integration-test file for
a test while also instructing the implementer to reach the seam through a
private module — two halves of one instruction, each locally reasonable, jointly
unsatisfiable. Nothing mechanical can see that, and re-reading cannot either,
because re-reading checks a claim against the model that produced it and the
model is what is wrong. It died when an implementer tried to build it.

**The campaign's largest error was of that second kind and outranks the twelve
on that list.**
Its specification asserted, as established fact, that nothing a possession
session commits is ever persisted and that no world-writing path exists after
genesis. That sentence shaped an acceptance criterion, a module's
documentation, an idea-registry row, and a decision record — and it is false.
Possession takes a documented `--out` flag; a previous campaign built the save
path deliberately and ruled on how it filters. One command retired the claim at
the Definition-of-Done sweep, four tasks after it should have been checked.

**The generator of the error is the transferable part**, and it is a shape this
chapter has recorded before at smaller scale. The evidence the specification
rested on was a doc comment saying the session ledger is "never written back."
That comment is *true*. It answers its author's question — does a session mutate
the world it borrowed? — and the answer is no. The specification read it as
answering a different question, whether these facts can ever be saved at all,
and the two questions have opposite answers. **A doc comment answers its
author's question, not the one a later reader brings to it**; a constraint read
off one is a hypothesis, and this one went four tasks without being tested
because it was never framed as one.

Three checks also reported green for reasons unrelated to correctness, which is
this chapter's standing concern about instruments arriving from a new
direction. A docs-only commit skipped the commit gate on a path heuristic —
correct about which *files* changed, wrong about which *tests guard them*,
because the check that would have caught the defect is a Rust test that guards
documentation. And the campaign's own three-outcome tripwire is absent from the
sub-floor roster, so the commit gate compiles it and never runs it: it would
have reported green while that test was red, on the very change the test exists
to catch. **A gate's scope and a defect's location can disagree, and the gate
cannot tell you when they do.** Both were caught by a human reading the roster,
not by anything running.

The third is the defect that shipped, and it is a different failure entirely.
The campaign added a verb to the dispatcher and to neither of the two rosters
that gate a verb by the state of the body, so a sleeping character could clear
a barred passage and commit the fact — and it was the only new verb that writes
to the ledger. The check meant to catch exactly this was green throughout **and
was working correctly**: it holds two lists in agreement in both directions,
and a verb missing from both agrees with itself. That was measured in the
defective state rather than inferred. **A two-way agreement check has a blind
zone at zero copies**, and the only instrument that sees into it is a test that
drives the behaviour. Seven green task reviews did not find it either — each
saw a diff that added a verb, and none asked the question only a whole-branch
view asks: which lists is this verb in? What closed it was available the whole
time, since the preceding campaign had shut the identical hole on a different
verb and left the test to copy. But the copy sits outside the cheapest gate by
construction: it costs thirteen seconds because it builds a world, and the
commit gate admits only tests under a second. The check with the hole runs on
every commit; the one that closes it does not.

The entry above is a correction, and it is worth saying so here because the
record failed in the same way the campaign it records did. The campaign's
retrospective opened with "twelve defects, none surviving in implementer code."
The second half was already false the moment it was written — the fourteenth
defect was sitting in committed code two commits earlier, undiscovered — and
the heading was left standing even after that defect was found and written into
a later section of the same file. Nothing re-reads an opening when a body
changes, and the person best placed to notice is the one who has just written
the thing that invalidated it. **A record can outlive its subject inside the
document that named the hazard**, which is the smallest scale at which this
chapter has yet observed it.

[The Mire](./chronicle/the-mire.md) exercised the same discipline on a bet
about weather and world structure that no earlier chapter entry had staked,
and it too came back **no** — a double falsification rather than a single
one. It froze, before any code existed, that a weather-gated modifier on the
connection graph's edge conductance (mud and snowpack lowering it, frozen
ground raising it back) would move the passable fraction of the world's
connection graph by a global, latitude-graded amount: at least a 5% median
swing across two hundred generated worlds (the systemic-effect bet), growing
toward the poles where weather is harshest (the where-it-shows-up bet).
Neither held. The measured median swing is **0.95%**, an order of magnitude
under the floor, and the swing that does exist runs backward: equatorial
cells swing furthest (0.0224), temperate cells less (0.0021), and polar
cells swing **exactly zero** — not merely small, zero on the nose, across
every sampled seed.

The mechanism is the durable part, and it generalizes past this one
measurement: **seasonal variation lives where conditions alternate, not
where they are extreme.** A permanently frozen polar cell has one season,
all year, so it never crosses the conductance threshold in either
direction; an equatorial cell, wet in one season and dry in the next, is
exactly the alternation the instrument can detect. Extremity without
alternation is stasis. Two checks confirmed the null was real rather than
the instrument being blind: a synthetic all-or-nothing probe (every land
edge fully open one day, fully closed the next) registered swings ten to
twenty times the measured median when an effect of that size was
deliberately manufactured, and across a full year only about 4% of real
land edges ever cross the passability threshold at all — most of the graph
is simply always-open or always-closed, regardless of season, which is the
mechanistic reason the systemic swing is small. This null joins the
chapter's growing record of preregistered predictions that came back no —
alongside the fire-warmth bet above and the conflict-cascade criticality
bet below — each recorded as a finding rather than a failure, because a
chapter that only reports confirmations is measuring taste, not the world.

What the null does not settle is stated in the chronicle rather than
smoothed over. The measured quantity is **passability** — whether a route
is open at all — not **cost**, how much slower or harder a route becomes
while it stays open; a large seasonal cost effect could sit entirely
beneath this instrument's threshold-crossing view and be invisible to it.
And the result is a claim about **land only**: water edges were
deliberately left ungated this campaign, so "the poles do not vary" may be
true for land and false for the sea ice that borders it, on coastlines
whose land itself never varies because it is permanently frozen.

*Re-scored sideways by [The Fathom](./chronicle/the-fathom.md) (2026-08-13),
which did not settle the underworld question above but corrected the premise
the sentence rests on.*

**"The way the sea's depth layers already are" was more generous to the sea
than the sea deserved.** That clause reads as though the marine model were a
finished thing to copy. It was not. The sea had the *vocabulary* — five pelagic
strata, each pairing a community with a depth — and it did not have the
*enumeration*: the accessor returned exactly one stratum per cell, the one its
floor lies in, so the water standing above that floor was unaskable. A cell over
a vent reported the vent and nothing about the kilometre of open water above it.
Declaring the underworld as places was therefore never going to be a matter of
copying a working pattern sideways; the pattern had to be built first, and this
campaign built it.

Two things fell out of asking the question for the first time, and both lower
confidence in the sea as an exemplar. **Seed 42 has no cell whose floor reaches
`Abyssal` or `Hadal`** — column heights come back `{1: 1749, 2: 6669,
3: 21478}` over 29,896 ocean cells, so two of the five pelagic strata never
occur as a floor in the flagship world at all, and the deepest arm of the marine
classifier is unreachable there. And **8,916 of 9,695 sea-ice cells (92%) carry
a stratum below the epipelagic** — ice filed four kilometres down, because the
classifier picks sea ice on surface temperature with no depth condition while
taking its stratum from the floor. Neither is a defect this campaign introduced;
both are things that could not be seen while one value came back per cell, and
both were left standing deliberately rather than repaired, because the campaign's
acceptance criterion was that no world byte move.

So the underworld question is **still open**, exactly as stated above, and the
route to it is one step longer than the chapter thought: the column had to
become askable before anything could be declared in it. What is now settled is
only that asking is possible. Whether a realm's hard gate generalises past caves
remains the thing a campaign placing a people underground will find out.

*Re-scored again by [The Underworld](./chronicle/the-underworld.md)
(2026-08-18), which built everything the sentence above was waiting for and
still could not do the thing the withdrawal was about.*

**The precondition is discharged and the bet is not.** The underworld now has
what the paragraph two above said it was waiting on: a depth coordinate that is
not metres above sea level, communities declared as places (twenty-two of them,
in the same five-axis basis the surface uses), chamber conditions that vary from
each other and from the surface, a water table that is non-degenerate on every
seed, and a people settled at a rung rather than at a mouth. Two dwarf kinds
were **drafted as candidates** — in a test-local fixture, deliberately not in
the species registry, so the roster could not become the thing under test — and
scored against a criterion frozen before any of it was measured. The
criterion **failed**: their modal delve rungs are equal on one of three seeds,
and their top-quartile habitats overlap at 7.3 / 16.4 / 16.5% against a floor of
20%. They were not admitted. That is the same conclusion The Delvers reached and
the first time it has been reached from a measurement.

**What lowers confidence is narrower and more surprising than the failure.** A
decomposition run alongside the criterion asked which half of the capacity field
did the separating, and the answer is not the half this campaign built. The
depth-routed *conditions* — chamber temperature and moisture — separate the two
candidates almost completely, at 0.5% top-quartile overlap on seed 42 and 0.0%
on seed 7; **on seed 1234 that statistic is undefined**, its quartile boundary
falling inside a tie of 12, so the reading is two seeds of three and not a flat
three-seed range. The delve
seating multiplier has no quartile at all: four or five distinct values with
284–879 cells tied at the boundary, so its own overlap is undefined on every
seed, and composing it onto the conditions *raises* the overlap toward 16% —
pulling the two kinds slightly back together. **The axis this chapter expected to
separate two underground peoples contributed too little resolution to be
measured**, and the separation that does exist came from a mechanism the chapter
already knew about.

That is not the null the withdrawal predicted. Zero overlap was named in advance
as "the axis separated them into different peoples rather than different
dwarves, which is a failure that would otherwise read as a spectacular success",
and the measurement landed on that branch — for the conditions, not for depth.
So the bet moves from *cannot be asked* to *asked, answered, and answered about
the wrong factor*. Whether two kinds can be separated **by depth** is now a
question about resolution — how many distinct values a depth-derived multiplier
can take before a quartile boundary means anything — rather than a question
about whether the world has a depth at all.

**And one measurement in the chain was wrong in a way that would have inverted
the verdict.** The fit function matched a cave's genus by comparing the cave
kind's name against the corpus's spellings; one of three agreed by coincidence,
so two of three formations silently scored against a genus-blind fallback and
returned identical tables. Broken, the overlap reads 34 / 93 / 84% and clears
the floor on every seed. Repaired, it reads 7.3 / 16.4 / 16.5% and fails on
every seed. Nothing in the gate distinguished the two, and the campaign's other
eleven instrument findings are the reason it was looked for. The chapter's
standing lesson from The Compendium — that a column's *value* is evidence about
the world only if something independent establishes the column can move —
generalises here to a stricter form: **a join is evidence only if something
establishes that it discriminates**, and a fit that always exists is not that.

*Re-scored again by [The Stope](./chronicle/the-stope.md) (2026-08-22), which
gave the underworld the extent every paragraph above assumed it would need and
moved the bet's own quantity by nothing.*

**The route this chapter has been implicitly recommending does not go where it
looks like it goes.** The paragraph above leaves the bet at *asked, answered,
and answered about the wrong factor*: whether two kinds can be separated by
depth is now a question about **resolution** — how many distinct values a
depth-derived multiplier can take before a quartile boundary means anything.
The obvious way to buy resolution is to give the underworld more underworld,
and The Stope did exactly that. A depth band stopped being one interior-less
point per column and became a drawn run of floors; a column became a branch
with its own character, its own barrier and its own root floor on its parent;
entrances became plural and map to floors rather than to columns. On seed 42
the realized chamber population went from 14,976 to 21,328 and the reachable
count from 1,158 to 1,496.

**None of it reached the seating multiplier, and the demonstration is exact.**
The campaign's address change was an epoch — every chamber in every world
relocated — and every drow-seating figure came back byte-identical, because
seating reads the rung and the column and never asks whether a chamber exists.
So the axis that would separate two underground peoples still takes the four or
five distinct values The Underworld measured, and the campaign that multiplied
the underworld's addressable places contributed nothing to it. **Adding places
underground does not add resolution to the quantity that places peoples,
because the two are disjoint derivations.** A campaign that wants depth to
discriminate has to widen the *seating* derivation; widening the lattice
beneath it is a different piece of work that looks like the same one.

**One half does move toward the bet.** The underworld now carries a per-branch
**character** — the first discrete, world-scale axis that varies *within* a
single cave system rather than between cells — measured at 65.25 / 29.86 /
4.89% over 6,136 realized branches, no value near the 80% failure ceiling
frozen before the code. It is not a people, and nothing that places a people
consults it. But the standing precondition since The Delvers has been the
underworld *being declared as places*, and a branch that owns a character and a
run of floors is nearer to a place than a bucket was.

**And a caution that belongs to this chapter's own genre.** The character
shares reproduce the draw's authored weights to within a point, which is the
correct outcome and also means the variety is a **setting** rather than
something the structure produced. A later campaign reading "the underworld
varies now" off that number should establish which of the two it needs. The
same campaign produced the sharper instance: a declared band-eligibility table
per character, which the shipped existence predicate never consults, so 47% of
one character's branches terminate in bands it declares itself ineligible for.
The chapter's series continues one term — a column's value is evidence only if
something establishes the column can move; a join is evidence only if something
establishes it discriminates; **a declaration is evidence about the world only
if something reads it.**

**[The Portolan, part II](./chronicle/the-portolan-world-map.md) (2026-08-23)
adds the first declared exception to the redaction boundary's own claim, and
the bet survives it narrowed rather than broken.** The Panes scored "the
channel carries semantic content, never a picture — cells, not glyphs" and
"both panes are pure functions of one snapshot" as structural facts. The
world map pane is neither: `bin` renders a whole-planet `Grid` — a picture —
directly from `hornvale-terrain`/`hornvale-worldgen` state the snapshot
document never carries and never could (a planet does not fit in a per-turn
document), and hands it to `core` as caller-supplied content the crate
cannot verify, the same category `Source::Look` already established for one
feature name at a time and now generalised to an entire rendered chart. So
the true claim was narrower than scored: not "no pane may see a picture,"
but "a picture crossing into `core` is a declared exception carrying its own
honesty discipline," and this campaign is the second instance of that
exception rather than the first violation of the rule. What the bet's
determinism half still buys was tested directly rather than assumed: H7
(this campaign's own preregistered measurement) ran two identically-seeded
possessions, one additionally exercising the whole map — zoom, scroll,
re-centre, resolution — between every real turn, and their
`vessel/session/v2` snapshots came back byte-identical by construction. A
caller-supplied picture can bypass the wire's content guarantee; it still
cannot become a hidden writer.

*Re-scored again by [The Drift](./chronicle/the-drift.md) (2026-08-23), which
is the strongest available test of the paragraph above and confirms it.*

**The bet does not move, and this time the null has teeth.** The Stope argued
that adding places underground does not add resolution to the quantity that
places peoples, because the two are disjoint derivations — an argument made
from one campaign's evidence. The Drift is the case that would break it if it
were breakable. It deleted a draw, added a draw, dropped a field from the
chamber address, bumped six seed-derivation labels and retired a seventh; every
chamber in every world relocated for the second time in three days; and
underworld reachability went from **7.0% to 100.00%** on all three panel seeds,
which is the largest single change to what is *usable* underground the project
has made. Across the whole campaign exactly **four** drift-checked artifacts
moved, and only **three** of them derive from a world: the stream manifest
(the labels themselves), the type-audit report, and the underworld witness
page — plus `docs/digest/decisions-in-force.md`, which gained one index line
for the campaign's decision record and is not world-derived at all. Every
almanac, the elevation map, every laboratory study, the Domesday survey and
the client fixtures are byte-identical.

A stronger instrument confirms the same null, measured after the paragraph
above was written: the once-per-campaign census refresh ran on the canonical
box at `fb2ef7ecd` (`docs/timings.md`, row stamped 2026-08-24T12:05:18Z,
904.554 s) and **zero goldens moved** — 1,000 worlds, 0 refusals, 226 charts
and 450 charts republished with no value changed. Verified non-vacuously:
both `rows.csv` were rewritten by that run (mtimes minutes old, 1001 lines
each) and came out byte-identical to committed. Six seed-derivation epochs
relocated every chamber in every world and the 1,000-world census did not
move.

So the seating axis still takes the four or five distinct values The Underworld
measured, and a campaign that made the underworld *connected* contributed
nothing to it — for the same structural reason the campaign that made it
*large* contributed nothing. **A derivation that never asks a question is
unmoved by any answer to it**, and the two instances together make that a
property of the seam rather than an accident of either campaign. A campaign
that wants depth to discriminate has to widen the seating derivation; the
underworld beneath it can be rebuilt entirely without touching the score.

**One half moves toward the bet, in the same direction The Stope's did.** The
per-branch character axis is now attached to something a player can actually
walk. Before this campaign a branch's character described a run of levels most
of which were unreachable from any entrance, and the share of a system's levels
reachable from its own doors sat at 11–13% at the median; it is 100% now, with
the unreached-by-band histogram empty on every seed. That still does not place
a people and nothing that places a people consults it — but the standing
precondition since The Delvers has been the underworld *being declared as
places*, and a place nobody can reach is a weaker candidate than one they can.

## Precedented but nontrivial (moderate confidence)

- **Lazy retrospective generation** — committing detail only on observation,
  consistent with a statistical prior. *Caves of Qud* and *Ultima Ratio
  Regum* prove pieces of this can work; nobody has done it against a
  fields-plus-ledger substrate at this scope, and the observe-then-commit loop
  is not yet built. Its self-scorable half is named below.
  **Re-scored by [The Lintel](./chronicle/the-lintel.md) (2026-07-27): the bet
  moves halfway, and only halfway.** The phrase names two mechanisms, and the
  campaign shipped exactly one of them. *Derive-on-demand* now exists at the
  finest band the world has: a chamber's existence, its interior and its prose
  are a pure function of the derived brief, the address and the seed, computed
  when a player walks in and discarded when they leave. Against ~4^9 candidate
  addresses under a single locale, that is the statistical-prior half working at
  its intended ratio — the overwhelming majority of the space is never
  materialized because existence is a predicate rather than a given, and it is
  asserted by test that it stays that way. *Commit-on-observation* does **not**
  exist and was deliberately excluded: The Lintel commits nothing at all, which
  is precisely what preserves byte-identity — the player's position has never
  been a committed datum, so descent needed no schema change and no epoch.
  Promotion-on-touch — the write half, where an observed detail is *kept* — and
  the delta store it implies remain unbuilt, and are the harder half, since they
  are where a lazily generated world can begin to contradict its own prior. So
  the bet's confidence in *derivation* is materially higher than it was, and its
  confidence in the *loop* is unchanged.
  **A supporting claim in that entry is corrected by [The
  Deed](./chronicle/the-deed.md) (2026-08-22), and the score is not.** The
  sentence above rests The Lintel's byte-identity on the player's position
  never having been a committed datum, and that half is now false: a possessed
  body's in-character acts charge time against its own mass and post `agent-at`
  through the same constructor a creature's step uses, so a walk-band step is
  persistent world state and the published possession transcript's day-stamps
  moved with it. The half The Lintel actually turned on survives intact and for
  the reason the two-tier law states: entering a chamber, moving within it and
  leaving still commit nothing. What separates them is that the committed tier
  is the *room*, so the correction is a boundary rather than a reversal. **The
  bet itself does not move in either direction.** Committing on *action* is not
  committing on *observation*; promotion-on-touch — where an observed detail is
  kept — and the delta store it implies are exactly as unbuilt as they were,
  and nothing here brings a lazily generated world any nearer to contradicting
  its own prior. Recorded rather than passed over, because a campaign whose
  headline is *the player now writes to the ledger* looks from outside like it
  should have moved this row, and decision 0030's sweep is answered by a
  statement either way.
  **Promotion-on-touch is BUILT, by [The Chattel](./chronicle/the-chattel.md)
  (2026-08-30), and the paragraph above calling it "exactly as unbuilt as they
  were" is superseded rather than merely dated.** The mechanism is the one this
  row names and not a cousin of it: an anchor is a derived region of a room
  that stays free until something touches it, at which point it *promotes* to a
  ledger entity whose identity was already a pure function of `(room facet,
  kind, ordinal)` — so the object could be named, compared and matched before
  any fact about it existed, and only a change pays. The delta store this row
  says promotion implies is the ledger itself: what a thing IS, where it is,
  whether it is open and whether it is locked are all folds over dated facts,
  read at the instant asked about (decisions 0396, 0399). The prior is
  untouched by an untouched room — a chamber's interior is still a pure
  function of its address and the seed, and a room nobody enters still commits
  nothing.

  **The number, and the status is deliberately left where it is.** A play that
  drives every promoting verb at every noun through seed 1's whole structure
  reaches 4 rooms offering **16 latent slots** and promotes **3** of them —
  the door key, the chest, and the chest's own key — so 13 of 16 slots stay
  free through a deliberately exhaustive session, and a normal one touches
  fewer. Across the full production gate space, Task 1's census of all 60
  combinations found a composed interior offers between 2 and 7 anchors,
  median 2.5, and the latency read that decides "is this slot still free"
  costs ~90 ns against a 22,880-fact played ledger. So the write half now
  exists and is cheap, and the ratio it runs at is the one this row hoped for.
  **A witness firing is not a witness changing status:** this bet's score is
  Nathan's to move, and the two things that would move it are not settled by
  the above. Objects are the *first* thing promotion-on-touch has been built
  for and the easiest — a kind roster is authored, so a promoted thing cannot
  contradict a prior it was never drawn from. The row's own sharp claim is
  about a *lazily generated* detail contradicting its own statistical prior,
  and nothing here tests that, because nothing here promotes a drawn quantity.

  **Re-scored UP by The Custodian (2026-08-30) — up, and not to settled.**
  The move is Nathan's, taken on the numbers The Chattel measured and
  deliberately declined to score on. What has actually changed is the
  *mechanism* half of the phrase, exactly as The Lintel's re-scoring changed
  the *derivation* half and left the loop alone: promotion-on-touch is built,
  it is cheap (~90 ns per latency read against a 22,880-fact played ledger),
  and it runs at the ratio this row hoped for — **3 of 16** latent slots
  promoted under a deliberately exhaustive playthrough, with the thirteen that
  stayed free being the design working rather than coverage missing. That
  ratio is also the honest bound on the evidence: the machinery has been
  exercised on about a fifth of what it claims, by one session, in one world.
  **The half that did not move is the half the row was always about.**
  Objects are the *easy* case, and easy in a way that is structural rather
  than incidental — a kind roster is authored, so a promoted thing cannot
  contradict a prior it was never drawn from, and the delta store has nothing
  to reconcile. The hard version is a detail drawn from a statistical prior —
  terrain, weather — where promoting one observed value could contradict the
  distribution it came from and the store has to decide which wins. Nothing
  in this campaign or the last touches that, so the bet stays in this section
  rather than moving up to what the world can check itself on: what would move
  it there is a promotion the Laboratory can score against the prior it
  departed from, and no such promotion exists yet.
- **Coarse constrains fine.** The design principle — a `ConstantSun` and a
  generated star system are both valid; higher fidelity refines and never
  contradicts lower — *shipped*, and holds from astronomy through religion's
  tiers. Crust sharpened it into a stated contract (decision
  0038): the terrain quantities that are
  *pointwise* — crust thickness and age — are stateless `Field`s any grid may
  resample, while the *mesh-bound* ones (sea level, drainage, placement)
  compute once on the world's canonical grid. So the pointwise half of the
  substrate is now genuinely resolution-free: the render lens samples the
  elevation field below cell scale, and the crust field byte-agrees across
  nested grid levels. The *Dwarf Fortress* move it is sometimes conflated with —
  runtime level-of-detail, refining an *active region* on the fly with the seams
  kept invisible — was the mesh-bound half the field/grid line isolated as the
  remaining work, and [The Room Mesh](./chronicle/the-room-mesh.md) has now laid
  its foundation. A room is a triangular face of the *same* icosphere refined
  deeper, so a level-7 room literally *is* a level-7 triangle: the seam problem
  that made active-region refinement look risky is dissolved structurally, not
  patched, and the dissolution was oracle-validated to `max|Δ| = 0` across all
  327,680 faces of a level-7 globe. Local detail is now summonable per-address
  at arbitrary depth for zero global cost, through an O(1) integer neighbour walk
  and coarse-field inheritance hooks. What is *not* yet built is the layer that
  consumes this substrate: the runtime active-region swap itself, its delta
  store, and the spike-validated adaptive-depth walk that lifts the uniform-depth
  restriction — all deferred, all resting now on a substrate that exists. The bet
  has moved from *no mechanism* to *mechanism shipped, composition pending*.
  [The Region](./chronicle/the-region.md) shipped the first cross-repo
  realization of the pointwise half — a regional tile query
  (`scene/tiles-region/v1`) that samples and barycentrically interpolates the
  continuous fields at arbitrary on-tile density, fed to a client that builds a
  registered globe patch from a tile's address. In doing so it drew the honest
  line the phrase *resolution-free* had blurred: the fields are free to *sample
  and smooth* below cell scale, but carry no sub-cell *information* — the
  ~110 km canonical cell is the physics floor, and interpolation beneath it is
  cosmetic, not fidelity. The active-region swap the bet still awaits inherits
  that boundary: it can refine geometry indefinitely, but never invent physics
  the cells do not hold. [The Massing](./chronicle/the-massing.md) deepened the
  *client* half of that consumption — the globe's level-of-detail ceiling lifted
  (a purely client-side reach for the finer region tiles the substrate already
  served) and the camera's own floor lowered to meet it — and, more pointedly,
  gave the cosmetic-versus-fidelity boundary a *renderer that shows it*. A voxel
  globe draws one block per cell and no gradient between, so the ~110 km floor
  reads as the visible edge of a block rather than hiding inside a smooth slope:
  the honest instrument for the very question of whether the cells' own
  resolution — not the client's — is the next floor worth deepening. That
  producer-side deepening stays deferred; what The Massing added is the
  instrument to judge when it is owed.
  **Re-scored by [The Lintel](./chronicle/the-lintel.md) (2026-07-27):** the
  substrate now has a *second occupied band*. A possessed body can stand at
  nine refinements below the walk band — one address space, a longer path — so
  the uniform-depth restriction is lifted in the narrow case the two-band
  vocabulary defines, and band changes are confined to visible thresholds
  precisely to avoid the thrashing an automatic adaptive-depth walk would
  reintroduce. This does **not** breach the ~110 km physics floor the row draws:
  a chamber's content derives from the *committed occupation history* of its
  walk-band ancestor, not from interpolating fields beneath cell scale. The
  distinction is worth keeping sharp — refining geometry below the floor stays
  cosmetic, while refining *what is recorded to be there* is fidelity the
  ledger already holds. The runtime active-region swap and its delta store are
  still unbuilt.
  **Re-scored by [The Blocking](./chronicle/the-blocking.md) (2026-07-28): the
  principle now has a *number*, at the finest band, and that is the largest
  movement this row has had.** "Higher fidelity refines and never contradicts
  lower" has always been checked by *agreement* — a field resampled at two grid
  levels must byte-agree — which tests that the fine layer does not disagree with
  the coarse one. It says nothing about the fine layer **inventing**. The floor
  plan is the first fine layer whose entire content is a lowering of a coarse
  structure (an anchor graph of chambers and links, itself derived from committed
  history), so the question became answerable in the other direction: how much did
  the fine layer add? The embedder reports its **residual degrees of freedom** and
  the checker compares that number against how much freedom the graph leaves free
  — and it is *exact*, not merely bounded, at every chamber count over two
  thousand seeds. Being **under** budget is a finding too, since it means the seed
  is not filling freedom the graph genuinely left. That converts the principle
  from a design intention into a measured property of one derivation, and it drew
  a line the phrase had left implicit: a plan's extent derives from chamber count
  alone and *spends no draw*, because a coarse constraint that consumes randomness
  is not a constraint, it is another generator.
  **Re-scored by [The Grain](./chronicle/the-grain.md) (2026-08-11): the
  principle now has *teeth*, measured — and this row had been reading it as a
  permission when half of it is a prohibition.** Every re-score above asks what a
  fine layer may *add*: agreement across grid levels, then residual degrees of
  freedom. The Grain asked what a fine layer may *subtract*, and got a number. A
  sub-cell refinement of one nominal field — a room's water kind, banded from a
  three-corner blend of the drainage the cells already hold, fitting no new
  constant and inventing no new physics — passed both of its preregistered local
  hypotheses and the whole commit gate, and **destroyed 29% of the world's fresh
  water at walking depth**, halving thirst-driven fauna movement. "Refines and
  never contradicts" forbade it, and nothing in the suite noticed, because every
  check the principle had was a check on *agreement* and this was a failure of
  *conservation*. The general result is sharp enough to be a rule rather than an
  anecdote: whether a field's values are **ordered** decides whether it may be
  refined by banding a blend at all. An ordinal field (relief) may — a blend moves
  it at most one band and conserves the distribution's shape. A nominal field
  (water, biome) may not — a threshold is maximally nonlinear, so classifying a
  blend is not the area-weighted vote of classifying the corners, and it deletes
  whichever category sits in the thin tails. Nearest-corner assignment is a
  *partition* and conserves area by construction, which is why the existing
  mechanism was already correct, and why the campaign's founding diagnosis (a
  suspiciously flat chart) turned out to describe the *view* rather than the
  field. Two consequences for this row. First, the ~110 km floor it has drawn
  three times is now known to be **asymmetrically** crossable: geometry may refine
  beneath it cosmetically, *what is recorded* may refine beneath it as fidelity
  (The Lintel), and a *category* may not be re-derived beneath it at all. Second,
  the honest response to a view finer than a field's model is neither refinement
  nor silence but **disclosure** — the chart now declares which of its fields are
  decided at grid resolution and are therefore constant beneath it, exactly as its
  colour block already declares what a projection does not carry. The bet's
  confidence is unchanged; what moved is that the principle stopped being a design
  intention with one positive measurement and became a constraint with a
  documented violation, a rule that predicts such violations in advance, and a
  conservation test in the gate.
  **Re-scored by [The Ford](./chronicle/the-ford.md) (2026-08-11): the field/grid
  dichotomy this row has been reasoning inside gains a third category, and the
  ~110 km physics floor is no longer where sub-cell *information* stops.** Decision
  0038 split terrain quantities into *pointwise* fields any grid may resample and
  *mesh-bound* quantities computed once on the canonical grid, and everything since
  has treated that split as exhaustive — which is why refining beneath the cell has
  kept reading as cosmetic. A river is neither. It is **feature-bound**: carried on
  a polyline with a discharge-derived width, evaluated as a function of position at
  any depth, and *the same object* at 110 km and at 27 m rather than interpolated
  between them. That is genuine information below the cell floor, on the producer
  side, and it arrived **without** refining the mesh — the adaptive-refinement
  route this row has been waiting on is not what delivered it. The reason
  generalizes past water: the quantity did not need finer resolution, it needed to
  stop being mesh-bound at all, and no amount of subdivision could have
  substituted, since a linear interpolant on a simplex attains its extrema at its
  vertices and so admits no sub-cell valley for a river to occupy. So the bet's
  confidence rises in a direction it was not pointing — the floor is a property of
  a quantity's *carrier*, not of the grid, and any quantity whose carrier matches
  its dimensionality escapes it. What is unchanged is what the row was actually
  tracking: the runtime active-region swap, its delta store, and adaptive-depth
  refinement of the quantities that legitimately remain mesh-bound are all still
  unbuilt. What is newly open is how many other quantities are miscarried the way
  rivers were.

  **The two re-scores above landed on the same day from two campaigns that did
  not see each other, and they must be read together.** The Grain's rule is a
  prohibition on *mechanism*: a nominal field may not be re-derived beneath the
  floor by thresholding a blend of corner values, because a threshold is
  maximally nonlinear and deletes whatever category sits in the thin tails. The
  Ford does not use that mechanism — it does not blend anything; it evaluates a
  signed distance to an emitted polyline, so the question stops being "what
  class does this interpolated value fall in" and becomes "where is this point
  relative to a feature". On mechanism, therefore, they do not collide, and The
  Grain's rule is arguably the sharpest available statement of *why* changing
  the carrier was the right move rather than refining the field.
  **What is genuinely open is conservation, and it is open in The Ford's
  direction.** The Grain's measured harm was a loss of area — 29% of the world's
  fresh water gone at walking depth. The Ford's channel occupies ~0.026% of land
  where the cell-scale river class occupies ~6.3% of land cells, and 39 of 700
  river cells carry no polyline at all. That is not a defect of The Ford's stage
  1, which moved no consumer and left `river_proximity` and the toponymic gates
  untouched by asserted invariant; it is the question its stage 2 inherits, now
  under decision 0124's requirement that a refinement preregister a conservation
  criterion — which The Ford's own spec, frozen before 0124 existed, did not do.
  The bet's honest state: the floor is crossable in more ways than this row
  once assumed, and each way owes a different proof.
  **Answered, in part, by [The Ford's stage 2](./chronicle/the-ford-stage-2.md)
  (2026-08-11): the inherited conservation obligation was not met — it was
  declined, by not making a refinement claim at all, and the row should record
  which of those two things happened.** Stage 2 was expected to redefine the
  room's water field and mint a new schema. It did neither. The room instead
  gained the *quantity* — a signed distance to the channel and the band edges
  that apply at that spot — appended as trailing keys, with the existing water
  field, its mechanism, the availability predicate and the toponymic gates all
  held fixed by asserted invariant. Nothing coarse was re-derived, so nothing
  had to be conserved, and the byte-cleanliness of the append was checked
  rather than asserted: nineteen insertions and no other change across seven
  regenerated artifacts. That is a third way past the floor, distinct from both
  re-scores above — not refining a field, and not changing a carrier either,
  but **adding a measurement beside the field and letting the consumer set the
  cut**. The Grain's disclosure answer is what makes it legible: the room now
  declares which of its fields were decided at grid resolution and which by the
  channel, so a reader can tell a flat field from a broken one without guessing.
  **What this does not do is close the conservation question, and stage 2
  produced its sharpest witness.** A seed-42 room reports its water as *river*
  while standing twenty times its own outermost band edge from any channel —
  the grid's answer and the network's answer, contradicting each other inside
  one document, because a river short enough to occupy a single cell never
  becomes a polyline. The disagreement is not new; what is new is that it is
  now visible in a single record instead of split across two subsystems that
  never met. A contradiction a reader can see is the precondition for repairing
  it, and the repair — a stated rule for which half wins, or lines for those
  cells — is owed by a later stage. The bet's confidence is unchanged.
  What moved is the menu: crossing the floor by *addition and disclosure* costs
  no conservation proof, and is available to any quantity willing to travel
  beside the coarse field rather than replacing it.
  **Re-scored by [The Rill](./chronicle/the-rill.md) (2026-08-13): the
  principle was *measured failing*, at a magnitude no previous campaign had
  produced, and the repair converts it from a rule a design must respect into
  a property of what kind of quantity is being refined.** Every re-score above
  argues about what a fine layer may add, subtract or disclose. The Rill asked
  the mechanical question underneath all of them — *which quantity is being
  refined* — and got a dichotomy with a measurement on each side. A **scalar**
  refines by area partition: the parts sum to the whole, so the fine answer
  cannot disagree with the coarse one, by construction rather than by test. A
  **direction** refines by a transfer operator between a mesh and its dual, and
  **there is no canonical one**. The campaign built the direction lift first,
  in good faith, and it delivered 26–31% of land to the sea where the coarse
  graph delivers 74–82%, doubled the basin count, and sent 6.0–7.5% of interior
  faces to a terminus outside their own coarse basin. Rebuilt as a partition of
  the scalar with directions *inherited from attachment* rather than computed,
  basin agreement is 41,415 of 41,415. Three consequences for this row. First,
  the row's own long-standing framing was subtly wrong about the mesh: it has
  read "a level-7 room literally *is* a level-7 triangle" as licence to lower
  coarse structure onto rooms, and cells are the icosphere's **vertices** while
  rooms are its **faces**, so a coarse flow edge runs *along* a room's boundary
  and never through it — the primal/dual distinction is load-bearing and the
  active-region swap this row still awaits inherits it. Second, **every local
  invariant held while the composed one failed**, which is the sharpest
  statement yet of what a refinement owes: agreement checked one step at a time
  is not agreement, and the reference must be the coarse graph's *composed*
  answer. Third, the harm has a visible signature — routing a direction out of
  every element gives every element a channel, and the campaign's world came
  out with 5–6% of its land underwater against a 0.5% ceiling. **Saturation is
  the signature of having refined the wrong quantity**, and it is cheap to look
  for. The bet's confidence rises: it now has a mechanical test (is this
  quantity a scalar or a direction?) that predicts violations before they are
  built, alongside The Grain's test (is this field ordinal or nominal?) that
  predicts them for values. What is unchanged is that both tests were bought by
  building the violation first.


## Genuinely open — split by whether the world can grade itself

The remaining low-confidence bets do not sit at one altitude. Each has a
**self-scorable half** the Laboratory could close on its own, wrapped around a
**taste-gated half** that waits on a human read. Naming the seam is most of the
progress.

The split is no longer hypothetical: The Chorus drove one bet of exactly
this shape to a verdict. Whether a derived cultural account differs from
ground truth *as a worldview* (not merely in vocabulary) looked
taste-gated until it was decomposed into distinctiveness ×
recoverability; the preregistered known-groups gate then separated the
uncanny pole from the gibberish pole from the shipped voices on every
measured world ([Study 012](./laboratory/study-012.md)). The residue that
stayed taste-shaped is exactly what the decomposition predicted:
*is it pleasant to read* — a far smaller surface than *is the worldview
right*. That is the template the bets below should expect: the
self-scorable half closes by instrument, and the taste half shrinks to
its honest size.

1. **Refinement at scale.** Generating detail consistent with fields *and* a
   large committed ledger, with aesthetic requirements on top, is constraint
   satisfaction plus taste — and the two halves have very different horizons.
   *Consistency* is self-scorable today: a generated detail either violates a
   committed fact or a field prior or it does not, and that is a metric, not a
   judgment. *Aesthetic quality* is taste, and it is the half that is years
   away and may need ideas that don't exist yet. The honest move is to build
   the consistency tier — checkable now — and stop letting the taste half make
   the whole problem look untouchable.
   **Re-scored by [The Wearing](./chronicle/the-wearing.md) (2026-07-29): the
   taste half shrank, exactly along the template this section's preamble
   predicts.** *A generated place name is too long to say and too uniform to
   believe* reads like pure taste, and had been treated that way. Decomposed,
   most of it was not. **Length** was already instrumented and the instrument
   was being ignored — the metric's declared buckets stopped at 10 characters
   and every world in a thousand-seed census overflowed them, silently, for
   several campaigns; a declared bucket range nothing enforces is an intent, not
   a check. **Syllable count** is a second instrument the campaign had to add,
   because character length cannot separate *shorter words* from *the same
   words spelled tighter* and the diagnosis turned on precisely that
   distinction. **Transparency** is the interesting one: the property that made
   the names read as generated was not any name's opacity but the *uniformity*
   of their readability — 650 of 650 names fully glossable, by construction —
   so the metric that closes it is a **distribution witness whose target is
   explicitly not its maximum**, and whose comment records that a drift back
   toward 1.0 is a regression. Three self-scorable readings where the honest
   prior expectation was one human read. What did **not** move is the residue
   the decomposition leaves: whether a given name is *pleasant*, and whether a
   world's toponymy reads as inherited rather than issued, are still a human's
   call, and the campaign's own success criterion for that half was written
   down as the owner's judgement rather than as a number. So this row's
   confidence in *scoring aesthetic constraints* is materially higher than "the
   half that is years away" allowed; its confidence in *closing* them is
   unchanged. The complementary lesson is a caution for the whole gradient: an
   instrument only scores a bet if something reads it. This one existed,
   drift-checked green, and measured a failure nobody was told about.

   **The Watershed sharpened that caution into its harder form (2026-07-31).**
   There, the instrument was read constantly — and was *wrong*.
   `exposure-sound` reported false on roughly three quarters of all worlds
   because the Laboratory's deliberately hand-maintained duplicate of the
   exposure rules had not learned a rule an earlier commit added. The worlds
   were correct throughout. It was the second such lapse in eleven days, and
   the campaign least able to notice was the one whose central mechanism the
   metric measures. So the caution generalizes: *an instrument scores a bet
   only if something reads it AND the instrument is itself current*, and
   nothing in this repo reddens when a deliberate duplicate falls behind.

   **The Domesday adds the third clause (2026-08-08), and it is the one that
   bites hardest.** *An instrument scores a bet only if it measures the quantity
   the bet is about.* The census's most-cited climate finding — that the climate
   is not merely cold but near-uninfluenced by its own astronomy — was drawn
   from twenty-three astronomy metrics, not one of which is insolation, stellar
   luminosity, or orbital distance. The facts exist and are committed to every
   world's ledger; `anchor-orbit-au` and `insolation-rel` sit in the same
   registration block as `brightening-per-gyr`, which the census does read. The
   evidence for *astronomy does not drive climate* is therefore a single
   orbital-period proxy, and the driver itself was never in the dataset. The
   conclusion may well survive measurement — the survey takes no position on
   that — but its current standing is weaker than the sentence it produced, and
   the first campaign to read all 193 metrics at once is what made that visible.
   The same survey found the complementary gap in the other direction: fourteen
   biology metrics are frozen across all thousand worlds because species life
   history is a pure allometric function of authored mass, class and schedule,
   so no world quantity reaches any creature's physiology. Between the two, the
   census's coverage of *what influences what* is materially thinner than its
   193-metric breadth suggested.

   **[The Armature](./chronicle/the-armature.md) (2026-08-09) put a number on
   that thinness, and the number is worse than the sentence above allowed.**
   Thirty causal links were declared from physics, frozen before any
   correlation was computed, and measured once: five silent, nineteen
   mis-strengthed, six unmeasurable. Ten of the nineteen measure `|r| < 0.1`
   and **seven of those ten were declared moderate** — mean land temperature
   against ocean fraction at `−0.0005`, against mountain coverage at `−0.041`;
   habitable fraction against obliquity at `+0.058`; settlement count against
   mountain coverage at `|r| = 0.015`. So the previous entry's finding generalizes
   past astronomy: climate is uninfluenced by its **terrain** as well, measured
   directly against drivers the census does hold, and settlement reaches the
   land only through one habitability scalar. These readings are not an
   artifact of a blunt instrument — the same frame measures `+0.951` between
   standing tribute and settlement count, and `+0.730` between fertile land and
   temperature, so it sees couplings where they exist. The frame was
   re-measured against the replacement census The Signet's epoch landed, with
   `studies/expectations.json` unchanged, and every count in this entry held.

   Two cautions temper the re-score, and both sharpen it rather than soften it.
   The six biology rows fired as **unmeasurable**, not as refutations: their
   metrics hold one distinct value across a thousand worlds, so the census
   cannot test those claims at all, and the spec's prediction that all six would
   fire came true for a reason that is not evidence about biology. And
   twenty-five of thirty rows firing triggers the campaign's own falsification
   clause, which says a frame that fires nearly everywhere indicts its author.
   That indictment is half right: four failures are strength over-claims with
   the declared sign intact, and three are sign errors the author owns. The
   seven near-zero readings are structurally different — an over-claim gets the
   sign right and the magnitude wrong, while a severed wire produces no signal
   at all — and that partition is a judgement laid over a measurement, which is
   why the frame was published whole rather than pruned to the rows that
   flattered it.

   That campaign also moved the self-scorable half in both directions at once.
   Sonority sequencing made pronounceability a property held **by
   construction** rather than measured after the fact — reverse-sonority
   onsets no language uses are no longer drawable, at zero entropy cost, since
   ordering a template consumes the same draws as picking one. But the
   transparency witness *fell* over 1000 worlds (0.816 → 0.793) while rising
   at the reference seed, which is the distribution witness earning its keep:
   a single-world reading would have recorded the opposite. Neither movement
   touches the taste half, which is unchanged.

   **[The Gazetteer](./chronicle/the-gazetteer.md) (2026-08-19) adds a second
   self-scorable reading, from a different naming surface and a different
   mechanism.** Landscape features are named per culture on the same
   species-salted draw as settlements, and the aggregate cross-people
   measurement — 42,525 of 42,525 pairs diverge — is a tautology rather than
   evidence: `species` is a leg in `Namer::name`'s own derive path, so it
   cannot read otherwise. The informative number was one nobody
   preregistered: within-people collision, over 405 features × 15 peoples.
   Pooled it is 9.15%, but individual peoples split from 0.0000 (gnoll,
   high-elf, wood-elf — every feature distinct) to 0.6247 (kobold — one name,
   `Rara`, covers fifteen places, seven of them volcanoes). Decision 0024
   already rules this outside "defect", so nothing was tuned to close it; the
   reading it adds to this bet is that "inherited vs. issued" is not a single
   property of a world's naming layer, but one that varies *by people* on the
   same mechanism — three peoples here already read as inherited, one reads
   as issued.

2. **Emergent economics that don't degenerate.** The mermaid-bone-farm
   problem: static value tables meeting exploitable production collapse into
   absurdity, and most game economies are faked precisely because real ones
   misbehave. Here too the bet splits. *Degeneracy* is self-scorable — an
   exploit detector is a Lab study: run the production loops, measure whether
   any yields unbounded value divergence. Whether prices flood, crash, and
   recover *legibly* is the partly-taste remainder. The economics campaign
   still begins with a literature phase (experimental economics, auction
   theory, virtual-world economics), but that phase now designs the apparatus
   that would falsify the claim, rather than standing between the project and
   knowing how it will grade itself.

3. **Historiography worth reading.** The systems half is no longer
   architecture-less: any entity's committed facts already replay into a
   derivation, physical deep time now lays down glacial strata and fossil
   shorelines ([Deep Time](./chronicle/deep-time.md)), and the past is being
   made queryable so that `why <ghost-town>` can recount the ice age that
   emptied it. Its measurable properties — focalization, sparsity,
   unreliability — are the guardrails. But *worth reading* is honestly
   taste-gated, and this is the one place the project refuses to fake a metric:
   *Dwarf Fortress* generates accurate history that glazes eyes, and no amount
   of architecture guarantees the sparse, focalized, unreliable account with a
   teller that would not. The human read is the real gate, and it is allowed
   to withhold a pass.
   **Re-scored by [The Confidant](./chronicle/the-confidant.md) (2026-08-25):
   one of the three named guardrails now has a mechanism, and the honest
   scoring of that is narrower than it sounds.** *Unreliability* had never been
   built. Every account the world produced was either true or absent. There is
   now a teller whose account diverges from the truth in two independent,
   inspectable ways — it cannot perceive what its own arbitration suppressed,
   and its culture may hold no word for what it feels — and the divergence is
   *derived*, from psychological attributes authored before the question
   existed, rather than authored as a distribution of unreliability. That is
   what makes it a property of the teller rather than a randomizer. The
   *focalization* guardrail arrives with it, since an account with a speaker is
   focalized by construction. **What has not moved is the bet.** This is one
   creature answering one question about itself, not a historiography, and
   *worth reading* was never gated on whether unreliability was expressible.
   What changes is that the guardrail is no longer a promissory note, so the
   taste half can now be judged against something that exists. The row stays
   low-confidence and the human read stays the gate.
   **Re-scored again by [The Reticence](./chronicle/the-reticence.md)
   (2026-08-26): a third, CHOSEN unreliability shipped, and the honest
   scoring is again narrower than the mechanism sounds.** Where The
   Confidant's two divergences are incapacities a host cannot help, this
   campaign gives the same teller a reason to withhold, mislead, or reveal
   *on purpose* — derived from what its own people believes it is riding and
   what the rider has actually done to it, never authored per host. The
   preregistered measurement found the conduct half genuinely discriminating
   (a host goes quiet on the one subject it has actually been overridden on,
   and stays forthcoming on every other) and the doctrine half **measurably
   inert on the only path a player can currently interrogate** — a structural
   fact about what a host can be asked, not a defect in the willingness
   mechanism, which remains provably sensitive to doctrine at override counts
   the shipped verb surface cannot reach. **What has not moved is the bet.**
   A teller who can choose to lie is one more inspectable property of one
   creature answering one question about itself, not evidence toward
   *historiography worth reading*. The row stays low-confidence and the human
   read stays the gate; what changes, again, is that one more named guardrail
   moved from promissory note to a mechanism with a measured, partly-null
   result.

4. **An inhabited moment worth standing in.** The project's thesis is that the
   world becomes interesting enough to be worth stewarding, and stewardship is
   chosen, not assigned — so the game has to be *seductive*, and seduction is
   the one property in this repository with no instrument pointed at it. The
   census grades values; `tropes/` and `systems/` grade capability; none of
   them can tell you whether a goblin village is worth watching for ten
   minutes. Decomposed along this section's template, the split is unusually
   clean. The **self-scorable half is traversal**: does an act reach through
   the stack and come back changed — does the scene *play out at all*. That
   half is now instrumented, by [The Repertory](./chronicle/the-repertory.md),
   whose verdicts come from running `possess` rather than from any declaration
   about it. The **taste half is whether the moment is worth having**, and it
   waits on a human sitting in a market and reporting what they noticed.
   **Confidence: low, and the split is the whole of the progress so far.**
   Two things are worth stating plainly, because both are load-bearing. First,
   a fully green repertory is entirely compatible with a world nobody wants to
   stand in — traversal is necessary and nowhere near sufficient, and reading
   a green roster as evidence about the taste half is the specific error this
   row exists to prevent. Second, this half must **stay a bet and never become
   a metric**. The campaigns here are very good at making numbers go green, so
   a scored proxy for *is it alive* would be optimised against long before it
   was validated — which is the one failure mode that would leave every gate
   green while the thesis quietly failed. It moves when a human plays and says
   it moved.
   **Annotated by [The Company](./chronicle/the-company.md) (2026-08-30), and
   the annotation is about the WORLD rather than the instrument.** The
   traversal half gained a second instrument, and the first thing it measured
   is that a shared room is rare by construction: across twelve seeds and both
   possession targets, `sensed.present` was empty in **24 of 24** witnesses,
   and sixty days of waiting produced none. The cause is neither a defect nor
   new — `SOC-one-creature-per-settlement` records that a settlement holds
   exactly one derived creature, and the standing ruling is that changing world
   population to restore an incidental co-location guarantee is its own
   campaign, tick cost having been measured superlinear in that dimension.
   **The score does not move, and why it does not is the point.** This is
   evidence about how often the world assembles a scene, which sits upstream of
   whether a scene is worth watching and says nothing about it: a market with
   two people in it can still be dull. What it does change is the honest
   description of the gap. The taste half is not merely unmeasured — in most
   worlds it is currently unreachable, because the moment that would be judged
   does not assemble.
   **Re-scored by [The Tableau](./chronicle/the-tableau.md) (2026-09-01), and
   this one moves the bet rather than annotating it.** The sentence above —
   *the moment that would be judged does not assemble* — was true of a world
   left to itself and is no longer true of the project. A tableau assembles
   the moment on demand: two creatures of chosen species in one room, one of
   them holding something, from four lines of JSON and no seed hunted. The
   taste half is therefore **reachable for the first time**, which is a real
   change in the bet's standing even though the score does not rise. It does
   not rise because reachable is not the same as answered: a human still has
   to sit in the scene and report what they noticed, and nothing about staging
   makes the answer more likely to be *yes*. What has changed is that the
   question can now be ASKED at will rather than waiting on a world that
   happens to oblige — and, honestly, that a staged moment is the easiest
   possible case, so a *yes* here would be the weakest possible evidence. The
   bet is properly gated on an UNSTAGED market being worth standing in, and
   staging is the instrument that lets us find out what such a market would
   have to contain.
   **Annotated by [The Wicket](./chronicle/the-wicket.md) (2026-09-01), on the
   traversal half, and the honest half of it is a NEW limit rather than a
   gain.** Two things a body does now come back different. A room can offer a
   fire to warm at without being a hearthroom — the one room type in any world
   that could offer it before — because the object vocabulary stopped being a
   closed list and a brazier reached the loomroom every reachable structure
   has. And sleep stopped being a flag cleared by any rest: it is a stock, paid
   down over the span a body was actually down, at a rate that differs between
   dozing watchfully and going under, on the planet's own day rather than a
   nominal one. Both are traversal in this row's exact sense — an act reaching
   through the stack and coming back changed — and neither is evidence about
   the taste half.
   **What is worth recording here rather than only in the decision log is the
   ceiling the second one hit.** Where a body sleeps grades how much the sleep
   repays, and the grade can be no finer than the LOCALE, because the ledger
   records the room a body was in and never the spot in it. So a player who
   passes out in the street is repaid exactly as one who found the bed — inside
   the same built, cold locale the two are indistinguishable to the fold. That
   is a texture failure of precisely the kind this row is about, sitting
   underneath a mechanism that scores green, and it is not a rough edge that
   can be tightened: lifting it is a decision about whether fine position is
   serialized at all. **The score does not move.** One more act with a
   consequence is not a market worth standing in, and a limit named is not a
   limit closed.
   **Re-scored by [The Roll](./chronicle/the-roll.md) (2026-09-02): the
   unstaged market now assembles, and the score still does not move.** The
   gate the annotation above named is discharged. A settlement derives as many
   residents as its committed population — named people, drawn apart on their
   own dials, not eighty copies — and the walk ticks the ones within call, so
   the moment assembles because the world is populous rather than because a
   fixture arranged it. The readings this row has collected, each on its own
   instrument: The Company found `sensed.present` empty in **24 of 24**
   witnesses — twelve seeds across both possession targets, the annotation
   above; The Hand, having deleted the player's own twin, measured **0 of 64**
   on the 64-seed probe; and this campaign re-ran that same probe before
   changing anything and read **3 of 64**. After the roll, a fresh possession
   stands in company in **64 of 64** seeds, which is every seed whose home
   settlement can hold company at all. At seed 42 that is 67 neighbours in one
   room, four of them named in the presence line and 63 counted. **Three of the four sentences this row has carried since The
   Company are now spent** — the moment does not assemble; it assembles only
   when staged; a staged moment is the weakest possible evidence. What
   survives is the fourth and always the real one: *a human still has to sit in
   the market and report what they noticed.* The score stays **low** because
   the axis of this chapter is checkability, and nothing about sixty-seven
   people makes the taste half self-scorable — 64 of 64 is a count of company,
   and this row exists in part to refuse exactly that substitution. The change
   is in the standing, not the score: the question can now be asked of a world
   left to itself, at any seed, which is the condition its two previous
   annotations said it was waiting on. It moves when a human plays and says it
   moved, and for the first time there is nothing structural in the way of
   playing.
   **Corrected by [The Plumb](./chronicle/the-plumb.md) (2026-09-02), on The
   Wicket's traversal annotation above, and the correction is a limit rather
   than a gain.** That annotation says the fatigue stock is paid down *"on the
   planet's own day rather than a nominal one"*. Half of it was. The RATE was
   converted to the planetary day; the SPAN — how long a rest lasts — stayed a
   fixed quarter of the nominal standard day, and the two are multiplied
   together. The calibration the rest exists to satisfy — one bout must carry a
   body clear of the band the drive re-engages inside — therefore held only on
   worlds turning faster than about thirty hours; past that a rest repaid 0.03
   against a floor of 0.1, and a body that lay down got straight back up. That
   is the fragmentation The Wicket removed with a span, restored by a
   denominator, on a legally pinnable world. It is converted now. **The score
   does not move**, and the reason is this chapter's own axis: nothing graded
   the world on it. Every test read the two constants at their nominal values,
   so a mechanism correct at one rotation period and wrong across most of the
   legal range was green everywhere, and what found it was an audit asking each
   constant what it varies along — not any measurement of the act.
   **Annotated by [The Pallet](./chronicle/the-pallet.md) (2026-09-03), on the
   traversal half again, and it closes the correction above while leaving The
   Wicket's named ceiling exactly where it was.** The half-conversion is
   finished: the sleep side is denominated in the world's own day too, so a rest
   no longer outlasts a sleep at any legally pinnable rotation, and the running
   test that had been asserting the inversion is gone. On top of that, a body
   now *chooses* where it sleeps — an anchor within the room it is already in,
   never travelled to — and the world records the KIND it chose. That is a new
   kind of legibility rather than a new act: a creature found sleeping in the
   road is now visible in the ledger instead of inferred from an absence, which
   is the tuning signal the design was built to preserve, and preserving it is
   why the chooser is deliberately allowed to choose badly rather than clamped
   to the best site. **The ceiling is unchanged and now has a route out.** A body
   that passes out in the street of a cold, built town is still repaid exactly
   as well as one that found the bed, because the fold re-derives from committed
   facts and the finest location any fact carries is the room. What changed is
   that the durable half of the choice is now IN the ledger, so a later campaign
   that wants the fold to know a bed from a heap of bracken has a fact to read
   rather than a position it is not allowed to keep. **The score does not move**,
   and for this chapter's own reason: none of this is a measurement of whether
   the moment is worth standing in. It is one more act with a consequence, and
   one more limit whose shape is now stated precisely enough to be attacked.

## The standing horizon

Year 1 varied the world and held the observer; Year 2 varied the observer and
held the sky. The current research varies **time**: two worlds identical at
genesis but differing only in their deep-time forcing, and a legibly different
present — different glacial strata, fossil shorelines, refugia, ghost-town
lineages, and history-derived myths — every divergence recountable through the
event ledger to its cause in the past. The falsifiability teeth are the same
shape as before: a blind-attribution metric over many thousands of worlds,
against a zero-forcing null control whose present must be indistinguishable
from its own genesis. It is a bet at the top of the checkability gradient, and,
like the two before it, it is allowed to fail.

**A second horizon is now quantified rather than merely felt.** *The Prospect*
(2026-09-03) measured the density of the inhabited surface for the first time:
across five seeds, **one enterable site per ~84,200 land facets**. A facet at
depth 13 is 1.126 km on a side, so a square mile is 2.04 of them and the aim of
something worth finding in every square mile is **~41,200x away — 4.6 orders of
magnitude.** No bet in the map above moved, because no bet covered this;
what the measurement adds is a **ceiling**, and the ceiling is what makes it a
horizon rather than a defect.

Placed features are born on the 40,962-vertex geosphere and addressed onto the
402,653,184-facet walk band, so **at most one facet in 9,830 can hold a placed
feature of a given kind**, before a seed is built. Worlds today sit at 5.8% of
that ceiling; saturating it buys about 17x and leaves the surface ~2,400x
short. So the shortfall is not a tuning question at any
threshold, and the campaign's own falsified hypothesis is the evidence — H2
predicted a per-facet cave *percentage* and missed by ~1,070× because a placed
point process on a 41,000-point lattice cannot express one.

What that bounds is the class of answer. Density at facet resolution has to come
from **derived** features — a pure function of seed and position, unbounded,
stored nowhere, outside the ledger and therefore outside world history — which
decision 0669 names but no campaign has yet built. The checkable form of the bet
is whether a derived surface can be dense *and* legible at once: whether noise
interacting with macro features produces places that feel found rather than
extruded, and whether a player can tell the difference. That is gradeable by the
world against itself — the same shape as the bets above — and it is allowed to
fail.

**A partial rescore, now that population has a field to vary.** The
carrying-capacity field promoted above (see the high-confidence tier) is an
*equilibrium* snapshot — `population = f(carrying capacity)` in closed
form, no iteration, no clock — and equilibrium is not the same claim as
"vary time." What remains open splits cleanly along the same
checkability line this whole chapter runs on. Giving the field a clock —
temporal relaxation, and the founding/growth/fission/abandonment history
that only becomes tellable once population moves — **has now landed**: *The
Living Community* (the living-community engine's first campaign) grows the
present world as the last frame of a coarse forward history run over the
capacity field, now ticked per-era by paleoclimate, so settlements found,
grow, migrate, and end across ~2000 years and leave standing ruins. That is
the *placement* half of the vary-time horizon; the deeper bet above — a
blind-attribution metric over thousands of worlds against a zero-forcing null
control whose present is indistinguishable from its genesis — is not thereby
settled and stays open at the top of the checkability gradient. A second,
narrower piece is not a time question at all: today's
condensation runs each species' field independently, which loosens the old
rule keeping different peoples' settlements apart. Packing multiple
species onto one landscape properly — footprint-scaled home ranges, a
tunable competition temperature, predator-prey coupling — **has now
landed**: the coexistence stack packs a multi-species density stack (with a
frozen competition temperature), and *The Niche* gave each species a
niche-differentiated carrying-capacity field, so composition varies across
space rather than resolving to one global blend — seed-42's
identical-everywhere settlements broke into distinct regions with
structured strife along their ecotones. It resolved to the
moderate-confidence tier as predicted; it was architecture, not taste. The
goblinoid roster also showed the model's honest limit: same-resource
species differentiate only ~two ways on climate alone, so the fuller payoff
(strongholds, refugia, a creature that owns the cold) was thought to wait on
a roster with distinct resource niches. *The Menagerie* (the entity-component
program's first campaign) built that roster — sixteen kinds spanning
photosynthate to apex predator — and found the limit lies deeper than the
roster: carrying capacity is `supply × fitness`, but supply is a **single**
net-primary-productivity field scaled per species by uptake, so a resource
niche changes a species' *magnitude* everywhere, never its *place*. Only the
climate term is spatial, and climate alone differentiates ~two ways however
many creatures compete. The stronghold payoff waited, precisely, on
**per-axis spatial resource fields** (minerals in the mountains, prey where
the prey is) and a way to read dominance as resource captured rather than
headcount. *The Demesne* (Stage 1 of a named Living-Biomes arc) shipped the
**abiotic half** of exactly that: photosynthate, forage, and mineral became
real per-cell supply fields, dot-producted against each kind's uptake vector
at the existing carrying-capacity site, and the rank-restoration paid off
wherever an abiotic specialist could reach it — seed-42's distinct material
dominants rose 2→4, and the pure-mineral xorn went from a noise-level single
settlement to the single largest domain on the world (≈29k cells), a
mineral-eater owning its mountains as the place-identity model intends. Two
pieces stay open,
both measured rather than asserted: the **prey axis** (dragons and predators
still hold no place) waits on Stage 2's trophic food-web field, and — newly
surfaced by the shipped half — the four small **peoples** do not diversify at
all, because their authored niches carry zero weight on any axis this stage
spatialized, leaving them competing on one shared forage number. That last was
named its own open design question (an order-independent territory force — a
field or a fixed point, never a cumulative tally); the preregistered
`≥6`-distinct-dominants test — a *material*-dominance target spanning all
sixteen kinds — stays honestly `#[ignore]`d as its remaining target. **The
peoples half of that question has since been answered, but from the other
axis.** *The Living Community* separates the four near-identical goblinoids not
by a spatial resource force at all but by **history**: history-first placement
grows them at different founding sites and displaces them along different
climate paths, so they end up holding distinct territories (region-of-influence
overlap 0.055 on seed 42, every sampled seed under 0.06) — diversity by *time*
where the spatial axes had none to give. It resolved to the checkable tier;
it was architecture, not taste. So the rescore moves the **abiotic** stronghold
debt from open to **banked and measured**, holds the prey-stronghold debt
against Stage 2, moves the **peoples-territory** bet from open to **answered by
history-first placement** (a time resolution, not the anticipated spatial one),
and — the field having gained its clock — narrows the vary-time debt to its
blind-attribution core, still open. *The Sundering* (the connection graph's
second slice) then gave that peoples-territory resolution a second, *dynamic*
mechanism: routing the history bake over a time-varying connection graph — a sea
that falls with the ice, opening land bridges, and rises to drown them — so
peoples are confined to the landmasses they can reach. Seed-42's four peoples
resolve onto four sea-bound landmasses, three holding only a subset of them, and
the territory overlap held (0.0466). It stays in the checkable tier; what it did
*not* resolve — the *volume* of the diaspora, throttled by the world's ample
vacant land and by peoples settling glacially-stable ground — it handed, with
measurements, to conflict-as-criticality.

**And conflict-as-criticality has now been tested, and the bet lost.** This
chapter has to be able to say that, or its scores are decoration. The wager was
that organised conflict, once it emerged rather than being floored, would
**self-organize to criticality** — that the size distribution of cascading
displacement would be a power law, the signature of a system holding itself at
its own critical point. It was always a bet at the *top* of the checkability
gradient in the good sense: preregistered, instrument-gradable, adjudicable by
the Laboratory without a human read. *The Tumult* built it and graded it.
Conflict does now emerge — seed 42, which never crowds, resolves 76 conquests
driven by coveted value rather than by density, and the map gains population
rather than losing it. The distribution is **not** a power law and is not close
to one: pooled over a hundred seeds and 2974 conquests, nothing chains beyond
size three, the support spans 0.48 decades against a preregistered threshold of
about 1.5, the per-octave decay is roughly 46-fold where a heavy tail falls two-
to fourfold, and the branching ratio measures **σ ≈ 0.051** against a critical
value of 1 — stable to three figures across a 3.3× change of sample. Geometric
with a hard cutoff, deeply sub-critical. No constant was tuned toward the
hypothesis at any point.

The honest rescore is therefore: **the criticality bet moves from open to
falsified for the mechanism as built** — not "partially confirmed", not
"promising". What it does *not* move is the underlying question, and the
distinction is the useful part. Two builds now bracket it from opposite sides.
The first, a crowding sandpile, had a **drive and no dissipation**, so every
avalanche ran to the depth cap — an artifact, not a tail. The second has
**dissipation and no accumulation**: each hop of a cascade costs real
population, every victim is weaker than whoever displaced it, and a chain dies
within a hop or two, with nothing stored *between* relaxations whose release
could make a large event. Criticality needs both terms, and the missing one has
a name and a shape — a standing dominance relation that concentrates value into
a topple-able structure, whose collapse frees a whole subordinate network at
once. So the residual bet is narrower and better armed than the original: not
"does conflict self-organize?" but "does accumulation-plus-dissipation
self-organize, on this world, at this resolution?" — with a measured null
result to beat rather than a prior to defend.

Two notes for the gradient itself. First, this is the chapter's second bet
driven to a verdict by instrument rather than by taste, after The Chorus — and
the **first whose verdict was no**, which is the more informative of the two
outcomes and the one a confidence map exists to be able to record. Second, the
falsification cost roughly one campaign and produced a sharper successor
question; the alternative — shipping the mechanism and narrating it as
criticality — would have cost nothing and taught nothing, and the drift-check
would have re-ratified the narration every time it ran. A bet is only worth
placing at this altitude if losing it is allowed to be published as a loss.

**The successor question has now been asked, and it lost too.** The residual
bet stated just above — *does accumulation-plus-dissipation self-organize, on
this world, at this resolution?* — was the whole mandate of *The Tithe*, which
built the missing term: a raid whose prize is *mobile* subordinates rather than
evicts, a patron collects tribute from a vassal it cannot fully see, and what
it collects banks in a store of wealth that feeds strength without ever
entering the pressure that kills. That is a literal accumulator rather than a
metaphorical one, and it works — the structure forms at volume, patrons survive
collecting, and a dominant grows without moving. **The shape of the violence
did not change.**

Two things moved and they must not be conflated. **σ roughly doubled**, from
≈ 0.051 to **0.109–0.115** pooled over thirty seeds and 7183 conquests, and to
0.103–0.109 over a hundred seeds and 22 255 — the same factor on both samples,
which makes it a real effect of accumulation rather than sample noise. That is
a genuine result and this chapter should say so. But **σ ≈ 0.1 is not σ ≈ 1**,
and every reading of *shape* is unmoved: the support still spans **0.48
decades** against the preregistered ≈ 1.5, the per-octave decay is still
**17.6-fold** where a heavy tail falls two- to fourfold, and **not one cascade
exceeds three displacements in roughly twenty-two thousand conquests**. Still
geometric with a hard cutoff, still deep in the sub-critical regime. No
constant was tuned toward the hypothesis at any point, and the last mechanism
the campaign added had its predictions **written into the spec before its code
existed** — including, explicitly, that revolts firing while the distribution
stayed geometric would be a *stronger* falsification than the standing null.
Revolts fired. The distribution stayed geometric. **That is the branch the
preregistration named as the harder one to explain away, and it is the branch
that happened.**

The honest rescore is therefore: **the criticality bet moves from falsified for
the mechanism as built to falsified a second time, against a mechanism built
specifically to answer the first falsification's diagnosis.** Not "progress
toward"; not "trending". The right way to hold it is that the *diagnosis* has
narrowed, not that the *bet* has improved. Two builds bracketed the answer as
drive-without-dissipation and dissipation-without-accumulation; a third
supplied accumulation and moved the number without moving the family, which
eliminates "nothing is stored" as the explanation. What is left is
**conduction**. A revolt frees exactly one vassal — collapse-release, where a
fallen patron's entire network is freed at once, was a stated non-goal — and
the relation graph is a set of one-level stars, because a vassal may not itself
take a vassal, and depth was the other stated non-goal. A patron's failure has
nowhere to propagate. An avalanche needs a medium, and this world does not yet
have one.

So the residual bet narrows again and is now nearly bare: not "does conflict
self-organize", not "does accumulation self-organize", but **"does a
*connected* accumulating structure self-organize?"** — with two named,
already-specified levers as its remaining content and two measured nulls behind
it. What that costs the gradient is worth stating. A bet that loses twice in a
row, each time to an instrument, each time with the mechanism built rather than
argued about, is more expensive to keep than to drop; the case for asking a
third time rests entirely on each null having eliminated a *different*
candidate, so that the third question is materially different from the first
two rather than a rephrasing of them. **If the connected version also comes
back geometric, the right conclusion is that this world does not sit at a
critical point, and this chapter should record that as settled rather than
open.**

### The third ask is being spent elsewhere (2026-07-29)

That test — *materially different, or a rephrasing?* — has now been applied,
and the answer is that the connected-cascade question **does not clear its own
bar by much**, while a different question clears it easily. This chapter is
therefore rescored: the criticality bet is **not** being asked a third time in
the form above, and the depth-and-collapse-release levers are **deferred, not
refuted**.

The reason is that three campaigns have been measuring the size distribution of
**events** — how long a cascade of displacements runs — while the property the
project actually wants from its history is a distribution over **entities**:
how large the largest polity gets, how unequal holdings become, whether an
empire is a thing a world can produce at all. Those are different variables with
different mechanisms and different literatures. Event-size criticality is
Bak–Tang–Wiesenfeld, and the conduction diagnosis is correct on its own terms.
Entity-size heaviness is Gibrat and Kesten — a random *multiplicative* factor
against a reflecting lower barrier — which is the standard account of Zipf's law
for city sizes and of the empire-area distributions. **Hornvale has never
measured it, and the bake has no empire-size metric at all.**

Reading the mechanism against that second literature explains the two nulls
without appealing to conduction, and the reading was verified in source rather
than reasoned about. A Kesten process needs a per-entity random multiplier that
persists. The bake's strength is `(population + stores × 0.5) × tech_weight`.
Population is logistic, so its growth is *anti*-proportional to its size near
capacity. Stores decay at 0.95 per epoch to a fixed point set by inflow, and are
destroyed on a community's closure. `tech_weight` takes four values capping at
3.0, is driven by absolute year, and its per-people head start is a draw in
[0, 300) years against era boundaries at 400/900/1400 — so **the world's only
irreversible advantage provably converges to zero relative value at year 1400**.
Every multiplier in the model is shared, capped, or mean-reverting, and no two
communities of one people differ in any authored dimension at all. A model with
no persistent per-entity multiplicative heterogeneity cannot produce a heavy
entity-size tail, and would not do so even with a conduction medium added.

*Re-scored in part by [The Tolerance](./chronicle/the-tolerance.md) (2026-08-05),
which voids one clause of the paragraph immediately above and leaves the more
important one standing.* That campaign made a people a distribution rather than
a point: each settlement now draws its own threat response from its
people's authored mean and dispersion, keyed on where and when it was founded
and fixed for the life of the community. So the clause **"no two communities of
one people differ in any authored dimension at all" is no longer true** — two
towns of one people, on different ground in different centuries, hold genuinely
different temperaments, and the between-settlement variance in that dimension
went from exactly zero to 0.010–0.113 depending on the people. The heterogeneity
is persistent and per-entity, which is two of the three properties a Kesten
process wants.

The third it does not have, and that is the part this chapter must not round
away. The drawn quantity enters the model as a **gate on a decision** — a
community above the threshold may take the initiative, one below it may not —
not as a **multiplier on strength**. Strength is still
`(population + stores × 0.5) × tech_weight`, and every term in it is still
shared, capped, or mean-reverting; nothing about the disposition draw multiplies
anything. A heterogeneous *propensity to act* changes which communities move and
therefore how the history branches, but it does not give a community a
persistent random factor on its own growth, which is the specific thing the
literature says a heavy entity-size tail requires. So the correct rescore is
narrow: **the diagnosis loses its "no heterogeneity exists" clause and keeps its
"no multiplicative heterogeneity exists" clause**, and the entity-size
prediction is unchanged. Nothing here was measured against M2 — The Tolerance
preregistered variance and rate hypotheses, not a size distribution — so this is
a correction to the *argument*, not a new reading of the *bet*. A campaign that
wants to test the Kesten account now has a cheaper route to it than it did: the
authoring pattern for per-entity variation exists and is proven, and what
remains is to point one at a multiplicative term instead of a threshold.

One detail sharpens this rather than softening it, and it is the same point
clause 3 above makes about asymptotes. The new heterogeneity is drawn from a
**uniform** on ±√3σ, clamped to the axis — so it is not merely
non-multiplicative, it is *bounded*, and the probability of a settlement
exceeding its people's support is exactly zero rather than small. Per-entity
variation now exists in this world; **rare** per-entity variation still does
not. The build constraint this chapter already owes a successor campaign is
unchanged, and one more mechanism now sits inside its scope.

**What replaces the bet is narrower, and it is a different shape of claim.**
Not a power law: a **sigmoid**. The wager is that annihilation, coexistence and
domination lie on one saturating response, that the middle is where nearly every
world sits, and that both extremes are **reachable but rare** — a world with no
goblins, and a world under one government, each possible and each unusual. This
is preregisterable, it is falsifiable in both directions, and it is a claim
about a distribution the Laboratory can compute over seeds rather than about a
scaling exponent that needs 1.5 decades of support to be well-posed at all.

It also carries a structural requirement the previous framing never surfaced,
recorded as [decision 0096](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0096-diversity-is-terminal-and-rubberbanding-is-multi-axis.md)
clause 3: rare extremes need **asymptotes, not clamps**. Hornvale's saturating
bounds are presently clamps — population against capacity, `tech_weight` against
3.0, `coexist.rs`'s viability `FLOOR` — and the probability of exceeding a clamp
is exactly zero at any input. On the current response forms the tails are not
rare; they are impossible. That is a build constraint, not a tuning target, and
it is the first thing a successor campaign owes this chapter.

**Confidence: low, and deliberately so.** The sigmoid has not been measured, the
claim that per-world conditions vary widely enough to reach either tail is
**unverified**, and this chapter should not be read as predicting the result. The
one thing it does now assert with the same confidence as the two falsifications
above it is the diagnosis: **this world's history evaluates every people on a
single scalar axis, and on one axis weakness is absolute.** That is checkable in
forty lines of source, it is the standing charge decision 0096 opens, and it is
why the third ask is being spent on a second axis rather than on a deeper graph.

One further note the campaign earns a place for, because it bears on how much
any of the above should be trusted. *The Tithe* amended its own specification
**five times, four of them following a disappointing measurement** — and that
count is disclosed in the spec, in the adjudicating test's own documentation,
and in the chronicle, because a reader who meets only the final histogram has
been misled about how it was arrived at. The protection taken was
preregistration of the last amendment. The lesson for this chapter is that a
confidence score is only as good as the disclosure attached to the measurement
under it: the number here is a falsification, which is the direction that
*cannot* be manufactured by adding mechanisms, and that asymmetry is the reason
the rescore is trustworthy despite the amendment count.

### The sigmoid's first axis is measured, and the null is in (2026-08-02)

*The Contour* built the cheapest test of the sigmoid wager's own diagnosis —
a second contest axis, uncorrelated with strength, entering at the raid
dominance test — deliberately touching no authored species data, so that if
it moved nothing the two costlier campaigns behind it (*The Appraisal*,
*The Deviation*) would be worth reconsidering for one campaign's price rather
than three. It moved nothing. Both of the wager's own preregistered halves
are now measured, matched against a frozen thirty-seed baseline, and both are
null:

- **M3 (peoples-alive-at-bake-end) fell, fractionally, rather than rising.**
  The entire thirty-seed delta is one world losing one people; every other
  seed's count is byte-identical to baseline, including the exact set of six
  extinction seeds. The mechanism rescued zero worlds from total extinction
  and caused zero new ones.
- **M2 (the entity-size distribution — the sigmoid's own headline variable)
  stayed geometric.** Mean, median and IQR sit within a few percent of
  baseline at both thirty and a hundred seeds; the one statistic that moved
  cleanly (max/median) moves inside the band a single outlier seed produces,
  not a distributional shift.

Per §4.3 of the spec, both conditions being met is the null the chapter
above already named as the informative branch: **a second contest axis,
uncorrelated with the first and entering at the decision point, is not
sufficient to hold diversity open in this world** — a finding about decision
0096 clause 1's *chosen mechanism*, not about the axiom, and one that sends
the sequence back to design rather than forward to *The Appraisal*.

**The null itself decomposes, and the decomposition is the part this chapter
must not round away.** `peoples-alive-at-bake-end` is discrete and bounded at
five, the roster's own size, and the baseline sits at that ceiling in 76.7%
of worlds already. "M3 rises" was close to unfalsifiable *upward*: in
twenty-three of thirty seeds the metric could not rise, because all five
peoples were already alive. So the null is really two claims of unequal
strength. **"Does not rescue worlds from extinction" is strong** — six
extinction seeds at baseline, six live, the identical seed set, a detectable
effect measured at exactly zero. **"Does not improve diversity in surviving
worlds" is untested**, because the instrument is saturated at its ceiling in
twenty-three of the twenty-four surviving worlds. The spec asked for a second
half of M3 — the effective-diversity reading `coexist.rs` already computes in
space, which would have headroom inside an all-five-peoples world that a bare
count cannot see — and only the count was ever wired up. Building that half
now, immediately after a disappointing count, would have the *shape* of
metric-chasing even with clean logic behind it, so it is deliberately
deferred to whichever campaign answers this chapter next, with its own
headroom declared in the preregistration before any code exists.

**Rescore.** The sigmoid wager's confidence stays **low**, but the character
of the "low" has changed, and the gradient should say so precisely: it was
*unmeasured* when the bet above was struck; it is now *measured on one axis
and null there*, with the other, headroom-bearing axis still unmeasured
rather than merely undiscussed. That is a materially weaker position for the
multi-axis thesis than "unmeasured" was, and a materially stronger one than
"falsified outright" would be — decision 0096 clause 1 is not itself
falsified by one mechanism's failure to move one metric, but it has now spent
its cheapest test and has one clean finding to show for it: position, alone,
is not the term that holds diversity open here.

**Re-measured after the epoch (2026-08-02).** Position-aware conflict draws
no new stream, but it changes every world's committed history, and the
`history/bake` label was bumped to `/v2` to say so honestly (decision 0006).
That re-mints every draw a second time on top of the mechanism's own effect,
so the numbers above were re-measured on a fresh matched pair taken entirely
on the post-epoch derivation rather than trusted to still describe the
shipped world. Neither null moved: M3 is still falsified, M2 still stayed
geometric, and the extinction set is unchanged in both identity and size
across the epoch — the strongest form the "does not rescue from extinction"
half of the decomposition above can take. **This rescore is unchanged and
stands as written.** One thing about *how* the null holds did shift: where
the pre-epoch reading found a single seed accounting for the whole M3 delta,
the post-epoch reading finds two seeds moving in opposite directions that
cancel exactly — the mechanism is visibly live at the individual-world
level, it simply does not net into more diversity. Full numbers:
`docs/superpowers/plans/the-contour-baseline-v2.md`.

**The saturation half of that decomposition has now been tested, by a campaign
that shares none of its nouns, and it survives.** The argument above turns on a
number that was never a property of the world: the diversity count is bounded by
*the roster's own size*, so with five peoples in play "M3 rises" was close to
unfalsifiable upward. The obvious reading of that is that the ceiling was too
low — that a larger roster would hand the instrument the headroom it lacked.
[The Delvers](./chronicle/the-delvers.md) (2026-08-07) raised the ceiling by half
again, from six settling peoples to nine, and **the saturation did not move**.
Across the same thousand-world census: before, 971 of 1000 worlds ended the bake
holding every one of the six, mean 5.961; after, 967 of 1000 hold every one of
the nine, mean 8.956. Three more peoples, three more survivors, and the share of
worlds pinned against the ceiling fell by four tenths of a percent.

That is a finding about the **instrument**, not about the mechanism — no second
contest axis was added and nothing here retests decision 0096. But it closes off
the cheapest hope this chapter had for the untested half of the null. The count
was not saturated because the roster was small; it is saturated because
extinction is rare in this world at any roster size, and a metric bounded by the
roster will therefore sit on its bound however far the bound is moved. The
effective-diversity reading the spec asked for and never wired up is now the
*only* way to test "does not improve diversity in surviving worlds," rather than
one of two, and enlarging the roster is struck off as an alternative. Its
headroom must still be declared in a preregistration before any code exists.

A related bound was found to have the same defect and was repaired in passing.
The coexistence calibration's ceiling on per-cell claimed diversity had been
frozen as a bare `3.0`, justified in its own text as comfortably below
undifferentiated sharing — where the diversity reading approaches *the species
count*. So `3.0` was never an absolute quantity either; it was three quarters of
a four-species roster, with the dependency compiled into a literal and invisible
until a second roster size existed. It is now derived from the live count and
reproduces `3.0` exactly at a roster of four, which is the strongest form this
repair can take — a no-op at the roster the bound was written for. The floor
stays absolute, because monoculture drives the reading to one however many
peoples exist; only the ceiling ever scaled. This is a post-unblinding change to
a preregistered bound, made deliberately, and it re-derives the bound's *rule*
rather than fitting its *value*.

*Extended by [The Radiation](./chronicle/the-radiation.md) (2026-08-10), which
raised the ceiling a second time.*

**A third data point, and the saturation still does not move.** The roster went
from nine settling peoples to **fifteen**. On the thousand-world census this
campaign regenerated, mean peoples alive at the end of the bake went **8.916 →
14.854** — 99.1% of the ceiling before, 99.0% after. Six more peoples, six more
survivors, and the share of the bound the metric sits on is flat to a tenth of a
percent. The reading offered above as an explanation now has three roster sizes
behind it rather than two: the count is not saturated because the roster is
small, it is saturated because **extinction is rare in this world at any roster
size**. Enlarging the roster was already struck off as an alternative; it is now
struck off with a two-thirds-larger roster and no movement at all.

The census's own per-cell diversity column — a different reading on a different
study from the calibration bound below, and not to be confused with it — moved
**1.96543 → 1.96710**, a rise of nine hundredths of a percent under a
two-thirds-larger roster. Which is the same story in a second instrument: adding
six peoples to the world changes almost nothing about how many of them share a
cell.

The margin is the part worth keeping. Re-measured on the roster that actually
shipped, the mean per-cell claimed diversity is **3.0101** — so the retired
literal would have failed by one hundredth, which is what a compiled-in
dependency looks like when it rots: not a loud failure that names its cause, but
a hair over a line, in exactly the shape most likely to be read as noise and
quietly re-pinned. Against the derived ceiling the same reading is 25% of
undifferentiated sharing where the original band permitted 75%, so the world is
if anything more differentiated than the bound was written to allow. Both halves
of that sentence are only sayable because the bound was re-derived rather than
re-fitted.

*Extended by [The Muster](./chronicle/the-muster.md) (2026-08-12), which found
that the re-derived ceiling cannot be reached at all, and that the instrument
enforcing this bound had been reading an empty store.*

**A derived bound can rot in the opposite direction, and this one has.** The
repair above replaced a literal `3.0` with three quarters of the peopled count,
which was correct and which the roster's growth has since carried to **13.5**.
The reading it bounds cannot exceed the number of kinds actually *present* in a
cell, and the mean claimed cell holds **6.4351** of the eighteen — pooled over
177 336 claimed cells across five seeds. **The ceiling now asks for more
coexistence than the world puts in a cell at all**, so the band is one-sided by
construction: it can only ever be failed from below, and no perturbation of the
quantity it was written to watch can reach its upper edge. A literal rots by
falling behind its dependency; a derived bound rots by outrunning what the
world can produce, and neither failure announces itself.

**And the guard enforcing it was reading nothing.** The test that asserts this
band built its own component set with two stores left empty — the biome
affinities and the habitat realms — both of which are sparse and read through a
default on absence, so an empty store did not raise; it silently supplied the
null hypothesis. Every authored affinity row in the registry scored identically
inside the one test whose purpose was to notice when they changed. With both
stores live the same five worlds read **2.1128** against **2.3734** blind, and
the decomposition matters more than the total: a third of that movement is
interaction between a *single* realm row and the affinity rows, so the affinity
contribution alone is 7.2% rather than the 11% a naive attribution would claim.

**What that costs this chapter's confidence is narrower than it looks, and
sharper.** A repaired guard that stays green proves nothing, so it was
mutation-proven — and the proof is an existence proof rather than a
demonstration that the band tracks the quantity. At today's roster the level
alone cannot cross either edge at **any** value; widening the rows to reach
every kind is necessary and nowhere near sufficient; and of three assignments
differing only in which kind holds which ground, all three move the reading by a
similar amount and only one crosses the floor — the *most* differentiated of the
three does not. The bound is a real instrument again. It is not yet a sensitive
one, and the difference is now written down where it will be read.

### A self-scorable bet was scored, and the instrument did not clear its own bar (2026-08-14)

This chapter's axis is **whether the world can grade itself on a claim**, and
its standing hope is that the self-scorable half of a bet closes by instrument
while the taste half shrinks to its honest size. *The Gnomon* is the first
campaign to run that procedure on an instrument rather than on a world, and the
result sharpens the axis in a direction the chapter had not written down.

The anomaly report is maximally self-scorable by construction: it ranks a
world's census columns by how deep each sits in the thousand-world
distribution, and every input is committed and drift-checked. Its usefulness
claim was preregistered as recall@10 ≥ 0.60 against a label the census cannot
supply — perturb one generative constant, rebuild twenty worlds, and ask
whether the columns the perturbation demonstrably moved surface in the top ten.
**Measured: 0.5667 over 120 pairs. Falsified.** All three controls held; the
figure is published a second way (0.35, excluding the two arms whose
perturbation left the census's observed range entirely and could not fail to
rank) because the second reading is the less flattering one.

**The rescore is not to the report's confidence but to the chapter's own
premise.** "The Laboratory can score this" was being carried as though it
implied "and the score will be good". It does not. Being self-scorable makes a
bet *decidable*; it says nothing about which way it decides, and a
self-scorable instrument can be scored and found wanting exactly as readily as
a self-scorable world claim can. That is the mechanism working, not failing —
but the chapter had only ever illustrated the axis with bets that closed
favourably, and one that closed against itself is the more informative
illustration.

Two specifics worth keeping, because both are about instrument design rather
than about this instrument:

- **A tail rank measures unusualness, not change.** These are different
  quantities and the campaign is the first thing to make the difference cost
  something. One injection moved exactly one column in every world and the
  moved value remained an ordinary value in nineteen of twenty; the world was
  different and it was not *strange*. Any future instrument that ranks by
  extremity inherits this gap.
- **The control that passes can be near-unfalsifiable while the headline
  fails.** The campaign's held-out calibration check passed comfortably
  (in-census share 0.7050, held-out 0.7500, ratio 1.0638 against a tolerance of
  2) — and 70.5% of census worlds already carry a column at the flagging depth,
  so a stationary distribution passes it while flagging nothing useful. It was
  preregistered as a control and explicitly not a usefulness measure, which is
  the only reason its green cannot be read as vindication. **Declaring which of
  a campaign's hypotheses is allowed to count as support, before either is
  measured, is what kept this pair honest.**

### An effect can be self-scorable, confirmed, and unattributable (2026-08-14)

The entry above sharpened this chapter's axis by showing that a self-scorable
bet can be scored *against itself*. *The Repose* sharpens it again from a
direction that is neither a pass nor a failure, and the chapter had no bin for
it: **the instrument confirmed the effect and could not find its cause.**

The question was as self-scorable as they come, and was frozen before any
geohazard code existed: do settlements over-occupy high-unrest ground relative
to the land base rate, stratified by elevation, pooled over thirty seeds? Every
input is generated, every comparison is arithmetic on committed counts, and the
readout ships as a drift-checked artifact. It answered cleanly: flat in the
lowest elevation band, and rising with unrest above it — ×1.572, ×2.578, ×5.395
from the calmest decile to the most violent.

Then the design's own disambiguating arm — added precisely because a bare
reading cannot separate *a true null* from *the mechanism was never wired* —
severed both channels through which unrest is known to reach settlement siting,
and the gradient did not flatten. It held at or above baseline in every band:
152.6%, 105.2% and 101.5% of the baseline's excess survived. **Neither modelled
channel carries the effect the instrument measured.** One of them, the hostility
penalty, actively opposes it.

**What this adds to the axis.** Self-scorability was being carried as a property
of a *claim*; this campaign shows it is really a property of a **statistic**.
The statistic here was scorable and was scored. The attribution — *which part of
the model produces it* — is a different question with a different instrument, and
that instrument returned "none of the ones we named". A world that can grade
itself on an outcome cannot thereby grade itself on a mechanism, and a campaign
that conflates the two will report a confirmed effect as though it were a
confirmed explanation.

The honest scope clause is short enough to carry: *unattributed by this
instrument on this roster*. Both halves of it do work. The mineral channel could
not have been refuted here whatever the data said — only two kinds in the roster
read it at all, and they hold 0.398% of the settled population, so severing it
tests the roster and not the channel. **An ablation on a channel almost nobody
reads is a null with no power, and saying so is the difference between a finding
and a claim.** The chapter's standing hope is that the self-scorable half of a
bet closes by instrument; this is a case where it closed, and left a strictly
larger open question behind it than it started with.

### A third category, between self-scorable and taste-gated (2026-08-14)

This chapter splits a bet into a **self-scorable half** the Laboratory can close
and a **taste-gated half** that waits on a human read, and treats naming the
seam as most of the progress. *The Hearsay* ran both halves of one bet at once
and found the split incomplete: there is a third kind of half, and it is the one
that wastes the most time, because it is indistinguishable from the first until
you trace it.

The bet was whether a derived world's committed history carries a legible
epistemic structure — who could know what, and on what grounds. Half of it
closed by instrument, cleanly. Transmission depth is a pure function of the
committed founding tree: 7,778 (event, holder) pairs on seed 42, a median of
four inheritance steps between the community that witnessed an ending and the
community holding the claim, 15.4% beyond ten steps, reaching the tree's full
depth of 21. Two predictions were frozen before the code and both were refuted,
which is the self-scorable half behaving exactly as this chapter hopes.

The other half — whether the communities holding a story are *independent*
sources — looked equally self-scorable. Every input is committed. The question
is pure structure over a tree. No taste is involved anywhere in it. It resisted
three operationalisations, each plausible, each reviewed, each wrong in a
different way, and the third scored its own motivating scenario at zero.

The cause is not difficulty and not taste. **The world lacks the degree of
freedom the concept is about.** Corroboration is agreement between accounts that
could have differed; in a campaign that carries content unchanged by
construction, no two accounts *can* differ, agreement is constant-true, and
every structural measure is a proxy for a quantity with no variance. The
measures were computing redundancy accurately and calling it corroboration.

So the axis gains a third position, and its diagnostic is a question rather than
a category:

- **Self-scorable** — the instrument exists or can be built, and the world
  varies along the axis being measured.
- **Not-yet-scorable** — the instrument is trivial and the world *does not vary*
  along that axis at all. Looks self-scorable, because all the data is committed
  and the computation is easy. Fails as a constant, or worse, as a plausible
  number that never moves.
- **Taste-gated** — the world varies, the instrument is buildable, and the
  reading is a judgment.

The question that separates the first from the second, asked before any formula
is written: **what would have to vary for this number to move?** If the answer
is something the design holds constant, the bet is not self-scorable yet, and no
amount of instrument work will make it so. It is waiting on a mechanism, not on
a measurement.

The practical consequence for this chapter is that a bet can be *demoted* by
this test without anyone having been wrong about it — the Myth program's
corroboration half moves from self-scorable to not-yet-scorable, and its
precondition is now named and dated: it waits on distortion, which campaign 2
supplies. The half that closed did so on the first honest attempt, and both
halves lived inside a single sentence when the campaign opened.

### A criterion can fail and confirm its own hypothesis (2026-08-15)

*The Glasshouse* froze six criteria before writing a line of the physics they
would grade, and four of them passed. The two that failed are the reason this
entry exists, because the chapter has been treating a preregistered set as
something that passes or fails *as a set*, and this campaign shows the useful
structure is finer than that.

The hypothesis was that a cold, uniform population had three separable causes
and was not an attractor. Two criteria tested the *mechanism* — that the
population would retain its temperature spread rather than being flattened onto
a target, and that no biome class would dominate. Both passed, and the first was
designed to be the one a lazy fix fails: a strong enough thermostat can hit any
median by collapsing everyone onto it. The spread held at 34.92 K against a
31.2 K floor, and the largest biome class fell from 65.1% to 25.0%.

One criterion tested a *magnitude*: land the median within five degrees of
Earth's +8.6 °C. It missed by twelve. And the same measurement that failed it
said why, in a form the criterion itself could not have expressed. The habitable
zone is denominated in the square root of luminosity while insolation goes as
luminosity over radius squared, so luminosity cancels exactly and a uniform draw
in orbital *radius* puts the population at a median insolation of 0.748 by
construction. Across the entire range of the thermostat's free constant — up to
*perfect* compensation — the median moves 7.2 K and two-fifths of worlds stay
cold. The shortfall is a property of the draw's measure, and no climate model
can argue with a measure.

**What this adds to the axis.** A preregistered set is not one bet; it is a
mechanism claim and a level claim wearing the same coat. When the mechanism
criteria pass and the level criterion fails, the honest reading is not "the
campaign half-worked" — it is *the model is right and something upstream sets
the level*, which is a strictly more useful result than a clean pass would have
been. The way to keep that reading available is to write at least one criterion
that a successful fix could fail, and at least one that only a wrong model
could.

### A zero can be a scope error wearing a null's clothes (2026-08-15)

The entry above concerns a number that moved less than hoped. This one concerns
a number that did not move **at all**, and the distinction turned out to be the
whole finding.

One of the six criteria asked that the census's dominant soil order stop being
frozen at a single value. It was `leptosol` on all thousand worlds before the
campaign and `leptosol` on all thousand after — after a change that moved the
median land temperature 8.3 K, halved the median land elevation, and cut
ice-dominated worlds by two-thirds. Read as a measurement, that is an emphatic
null: the strongest available intervention, and a response of exactly zero.

It was not a null. The soil classifier's first question asks whether the soil is
shallower than a quarter of a metre or the ground drops more than 300 m to a
neighbour, and answers `leptosol` if either holds. Every question below that line
reads temperature or moisture. Measured over 307,588 land cells, **72.1% never
reach those questions** — 61.09% by depth alone. The classifier that the
criterion was grading was, for three cells in four, never consulted.

**What this adds to the axis.** The chapter already distinguishes a
*self-scorable* bet from a *not-yet-scorable* one by asking what would have to
vary for the number to move. This is the same question asked one layer lower and
it needs its own name, because the failure looks different: here the world
varies, the instrument is correct, and the statistic is still constant — because
the quantity being varied is **downstream of the branch that decides**. A null
of this kind is indistinguishable from a weak effect by inspection, and the only
thing that separated them was attributing the branch instead of inferring it.
The instruction that follows is cheap and general: when a statistic refuses to
move under a large intervention, find the line that decides it before concluding
anything about the effect size.

### The third position promotes back, and then reappears (2026-08-15)

*The Retelling* supplied the missing mechanism, so the demotion above can be
tested rather than believed. It holds, in both directions.

**The promotion happened.** Once content could vary — a claim's day coarsens
when a story crosses between communities standing differently toward the event
it describes — the precondition became measurable on the first attempt that was
tried. Spearman's rho between an ending's maximum-antichain width and its count
of surviving variants is **0.662** over 408 qualifying endings. The axis that
had been constant now varies, and the instrument that had failed three times
worked immediately. Nothing about the instrument changed; the world did.

That is the third position behaving exactly as this chapter predicted, which is
the strongest thing that can be said for a category invented one campaign
earlier: it made a dated claim about what would unblock a bet, and the
unblocking happened for the stated reason.

**And the position reappeared, one level in.** The campaign specified a model in
which distortion *accumulates* along a path and built one in which it fires at a
*boundary* — the distinction historical linguistics spent a century on,
Neogrammarian sound law against lexical diffusion. Stance turns out to be an
absorbing partition: victim-line and bystander are both closed under descent, so
a story crosses at most one boundary and only two of a four-rung ladder are ever
reached. Whether distortion compounds is therefore **not-yet-scorable** by the
same test: what would have to vary is the number of boundary crossings, and the
design holds it at one.

The useful form of this is that the third position is not a waiting room a bet
passes through once. It recurs at each level of mechanism, and the diagnostic
question survives each recursion unchanged — ask what would have to vary, and
believe the answer even when the instrument is already written and the numbers
already look plausible. A four-rung ladder that only ever uses two rungs
produces perfectly good numbers.

Two campaigns reached that recursion independently and on the same day, from
opposite directions — The Glasshouse from a statistic that would not move under
a large intervention, The Retelling from a ladder that used two of its four
rungs. Neither knew of the other's entry until they collided in a merge. That
is weak evidence the recursion is a property of the axis rather than of either
campaign's subject matter.

### The recursion was scored, and the instrument disagreed with itself (2026-08-17)

*The Palimpsest* supplied what the entry above said was missing — a design in
which the number of boundary crossings can vary, because damage accumulates as
a continuous width rather than spending one rung per firing. So "does
distortion compound" moves off **not-yet-scorable**, and the score is a
qualified yes: over forty worlds, the retained rung distribution spans a median
of five and a maximum of seven rungs under one of three co-equal accumulation
rules, and two under the other two.

**One sentence in the entry above needs striking, and the strike is load-
bearing.** It says victim-line and bystander are both closed under descent.
Bystander is not: a bystander whose subtree contains the attacker has a
perpetrator descendant, and a bystander that is the subject's own parent has a
victim-line child. Only the first of those can produce a path that crosses
stance twice — the 209 that were measured all take it. The second breaks
closure just as really, but `VictimLine` *is* descent-closed, so a path that
steps into it crosses once and can never cross back. The law that actually
holds is narrower — any predicate over the
teller/hearer *lineage relation* is constant along a single-parent walk by
construction — and stance is not such a predicate. It merely behaves like one
because the label that breaks closure holds exactly one community, the named
attacker, so a path entering it must leave at the next step and the crossing
count stops at two rather than growing. The bet's blocker was real; the reason
given for it was not the reason.

The qualification is the part worth keeping, and it is a fourth position rather
than a fourth level of the third. **The instrument was internally incoherent in
a way no amount of asking "what would have to vary" would have found.** The
accumulated width was seeded in days and incremented in a dimensionless count
of generations, then compared against rungs measured in days — a defect in the
design text, which defines the amplitude in one unit and the ladder in another
and never states a conversion. For that reason alone, two of the three rules
never reached a people's coarsest rungs at all: additive's occupied labels stop
at `year` on every one of the forty worlds, touching neither `generation` nor
`lifespan`, and it clears even the second rung on only eleven of them — 4.4% of
pooled claims. So their apparently clean falsification of a predicted
saturation was not a measurement of the world at all. A post-hoc
re-measurement with the units reconciled turns their saturated fraction from
exactly 0.0000 into 0.72–0.75.

The diagnostic that would have caught it is not this chapter's usual one. Asking
what must vary is a question about the *world*; this was a question about
whether two quantities being compared are **commensurable**, which is a question
about the instrument and is answerable before any world is built. The campaign
had in fact killed two earlier designs by exactly that check — an amplitude
measured in days that overshot the ladder 25-fold on a single step, and a ladder
of natural durations against paths that span 8–13 generations — and then shipped
a third instance of it. A check applied twice and forgotten the third time is
the failure mode, not an unfamiliar check.

The one result immune to all of it is worth naming, because immunity has a
cause: the correlation between a people's generation length and the precision it
retains is negative under all three rules in *both* readouts. A unit mismatch
that rescales every people by the same factor cannot disturb a comparison taken
between peoples. A bet that survives a broken instrument is telling you which
term it actually depends on.

### A precondition was supplied, and it pushed the other way (2026-08-18)

*The Parley* supplied a precondition the corroboration half of the Myth bet had
been named as waiting on — **not by this chapter**, whose last re-score named
distortion, which campaign 2 supplied, but by the row the
[idea registry](./frontier/idea-registry.md) carries on the matter: accounts
diverge only where two filters are mismatched, mismatch needs a boundary
crossing, and a boundary crossing needs contact. That is the edge this campaign
built: one that lets an account leave the people that witnessed it. Every model
before it walked parent to child down the founding tree and nothing else, so "do
independent accounts of one event corroborate each other" had no way to become a
question about two peoples at all.

**The diagnostic was right about the variance.** Asking what would have to vary
identified the missing degree of freedom correctly, and adding it made the axis
move immediately: cross-people reach goes from 548 to 4,112 of 23,594 endings,
and accounts held by as many as six peoples exist where three was structurally
impossible before. Nothing about the instrument changed.

**It was silent about the sign, and the sign is the finding.** The frozen
prediction was that two-sided disagreement would rise by more than 3×. It
*falls*, on every one of three co-equal accumulation rules — 0.59×, 0.52×,
0.77× — and identical day sets rise on all three. The mechanism is the edge's own
symmetry: a seam is a channel in both directions, so each side receives the
other's telling and each keeps whichever it can reach least corrupted. Contact
between two peoples is **homogenising**. The precondition for corroboration, once
supplied, consumed the divergence corroboration needs.

So the axis gains a caution rather than a category. *What would have to vary* is
a question about whether a bet can be scored at all; it says nothing about
whether the mechanism that supplies the variance also destroys it. **A
precondition can be its own confound**, and there is no way to find that out
except by building it and looking at the sign.

A second half of the same campaign is worth recording beside it, because it is a
cost of preregistration and not a defect in it. The frozen measure compares the
victim's people against the raider's people **and no other pair** — and the edge's
whole effect is to create *new* pairs of peoples that share an account. Over the
endings that reach two or more peoples, the share carrying some
mutually-exclusive pair rises 2.1× to 4.5×, and the frozen measure is blind to
every one of those pairs by construction. That reading is not licensed as
evidence, because it was not frozen; the falsification stands as measured. But it
names a failure mode this chapter has not carried: **a measure frozen against the
pre-change topology can be structurally unable to see the change.** Freezing
protects against retuning a number after seeing it, and charges for that
protection in the currency of what the number can be about.

The same campaign froze a seed-level control that no world could satisfy — it
asked for a world where the mechanism is absent at hop zero but present as a
graph, and the graph's edges *are* the hop-zero seam. Falsified, correctly, for a
reason that says nothing about the mechanism. The diagnostic transfers to
controls unchanged and had not been applied to one: before freezing a control,
ask whether the world can reach the outcome the prediction requires.

**The re-score.** The corroboration half stays **not-yet-scorable**, and its
blocker is now narrower and dated. The registry's contact precondition is
discharged — contact exists, and on the frozen measure it points away from
divergence. It waits on a
*directed* edge — victim→raider and raider→victim are different stories with
different reach, both restrictions of the undirected ceiling measured here — and
on a claim that carries the witness it came from, without which the pooling
mechanism above is an inference from counts rather than something the world can
show. Both are cheap. Neither existed to be asked for before this campaign.

### Two candidate causes were eliminated, and the measure went blind (2026-08-18)

*The Undertow* set out to discriminate between the two explanations the entry
above leaves standing for the pooling result, and eliminated **both** before it
had written a line of design. That is worth recording on this axis for a reason
that has nothing to do with either mechanism: **the eliminations came from
probes, and each one falsified a premise the campaign's own controller had
written down and recommended.** Asking what would have to vary is a question you
can get wrong twice in a row about the same result.

**Every figure in this entry is re-derived against the tree the campaign merged
into.** It ran to completion without absorbing `main`, a predecessor moved
settlement placement underneath it, and the merge was textually clean — so the
numbers it wrote down described a substrate the merge product does not have. The
re-derivation moved two of its conclusions and is disclosed where each occurs.

**The directed edge is closed, not deferred.** This chapter's last re-score named
it as one of two cheap blockers. It was run: both restrictions were simulated on
the shipped graph, and divergence never *rises* under either arm on any rule —
under the multiplicative rule the one-way arm collapses it *further* than the
undirected edge does. Restricting the channel does not restore disagreement, so
the freeze the predecessor made is exonerated rather than implicated. The axis is
also the minority case: about 55% of cross-people holders cross the seam more
than once, and only about 6% are the hop-zero co-witness line.

**The selection rule is exonerated too, and by a stronger argument than its own
table.** Four people-blind alternatives disagreed with the shipped rule on 1 of
the 4 cells where they could have, and the one that broke — `recency` — maximises
an unbounded hop count and is not shippable as written. Behind that sits a
structural fact, and it is the half that carries the score: under descent — the
ratio's own *denominator* — all 82,209 holders receive exactly one telling,
because the founding tree is a forest and witnesses are never re-entered.
**Half of the ratio was never at stake.** No ordering rule could have moved it,
and the table was measuring a question that was half-vacuous by construction.
That argument is about the shape of a tree and does not move when a substrate
does; the table did, from 0 of 8 to 1 of 4, which is exactly why the score rests
on the argument and not on the table.

**Then the campaign built a mechanism that works and watched the measure nearly
fail to see it.** A seam crossing now costs a penalty derived from how much
contact the two peoples actually have. It reaches: 282 holder-rungs move, 81% of
the near-stranger tercile demonstrably pay, and the reach identity closes to the
unit on all three accumulation rules. It changes what is held: thousands of
communities keep a different account of a war than they did. And the aggregate
this chapter has been quoting moves by **zero on two rules and by two endings of
421 on the third** — 11→11, 11→11, 43→45.

That is the same dissociation, for the third time in this thread, and the repeat
is the finding rather than the incident. A selection change that rewrote 41.9% of
holders moved it by ≤4 events of 100; this one changed 0.10–0.25% of 1.2 million
held tellings and moved it by two. **The blocker is no longer a missing
mechanism. It is a missing measure.** Both of the cheap blockers this chapter
named have now been spent, one discharged and one made irrelevant, and the
corroboration half is still **not-yet-scorable** — but for the first time its
obstacle is an instrument rather than a world.

(As executed, that aggregate moved by exactly zero on all three rules and this
entry said so. The two-event move appeared only on re-derivation. It does not
change the conclusion — a measure that registers a change in 0.25% of 1.2 million
holders as two events is still the wrong instrument — but the absolute form of
the claim did not survive and is not made.)

**The re-score, and one thing that must not be read into it.** The campaign's own
licence is that the penalty's magnitude is *derived* from the ledger rather than
authored, which is what makes a people-aware rule admissible at all. Nothing in
this campaign tested that. The tercile ordering came out inverted rather than
uniform, and an inverted ordering is equally consistent with a **constant**
penalty, since crossings concentrate on a handful of people-pairs; the only sweep
run varied a *global* scalar, which cannot reorder pairs relative to each other.
**A
derivation is not confirmed by a mechanism that works.** The bet that a derived
magnitude does something a well-chosen constant would not is a *new* entry on
this axis, self-scorable, and unscored — which is a better position than the
chapter was in before, and a worse one than the result reads at first glance.

**And the constraint the whole thread runs under was ratified, in a form that
moves the target.** The prohibition is on the *engine* holding a view about a
people — a species carrying a valence, a lookup table deciding conduct. It was
never a prohibition on prejudice existing: the creatures, and the player, are
expected to hold very strong opinions. A world where nobody holds a view about
anybody is not the goal and is a worse simulation than one where views are held
and are wrong. So the thing being built toward is not neutrality but *situated
error* — creatures with strong, mistaken opinions about each other, arrived at
because of how their accounts of a shared history actually travelled. That is a
harder bet to score than neutrality would be, and it is the right one.

A fourteenth campaign moves no bet in the map and yet lands squarely on the
floor beneath it. [The Burr](./chronicle/the-burr.md) (2026-08-19) set out to
give the tongues *character* — audible liquids, per-family word-building — and
carried a preregistered instrument that measures whether a word can be traced
back to its tongue. That number rose across every typological stage, from
0.7202 to a merged 0.7995, and a global parametric change moved it the other
way, exactly as the thesis predicted. But distinguishability is not the claim.
The goal was aesthetic — whether the tongues are *lovely*, not merely
separable — and no metric the Laboratory can run scores that half. A classifier
at 13× the chance floor and a reader who still cannot tell one tongue from
another are both true at once (the campaign's own §6 caution, measured). So the
floor gains a clean exemplar rather than a new question: a bet whose
Laboratory-scorable half can rise handsomely while its *sufficient* half stays
taste-gated, and the honest report names which half the number was about.

### The missing measure exists now, and it separates what the aggregate could not (2026-08-19)

The entry immediately above — *Two candidate causes were eliminated, and the
measure went blind* — closed on a specific sentence: **the blocker on the myth thread's
corroboration half is no longer a missing mechanism but a missing measure**,
and *for the first time its obstacle is an instrument rather than a world*.
[The Touchstone](./chronicle/the-touchstone.md) (2026-08-19) built the
instrument, so that obstacle is discharged.

It is a per-holder belief-delta (`hornvale_hearsay::touchstone`): it promotes
the route- and width-carrying walk out of a test-file copy into the library,
diffs two transmission arms into a component-change vector over every holder —
`{route, day, rung, hops, width}` — and reports a distribution with a
people-pair cut, where the divergence aggregate reported one scalar. And it was
held to a preregistered discrimination frozen before the measurement code
existed: it is valid only if it separates a change that rewrites beliefs from
one that provably does not, on the same panel where the aggregate reads
near-zero for both. It does. Over the selection-rule swap — a working mechanism
the aggregate misses — it fires for **63.77%** of holders while the aggregate
moves +4 of about a hundred; over a people-homogeneous population under an
inert crossing-arm swap, provably zero by the `crossing_penalty` theorem, it
fires for **0.00%** while the aggregate moves +0. Sixty percentage points of
separation where the aggregate separates the two by roughly nothing — which is
exactly what an instrument that can tell a working mechanism from a decorative
one has to do.

**What this does and does not move.** It does not score the corroboration bet
itself — that still waits on a mechanism campaign — but it changes the bet's
*position*: the thread now has an instrument its next mechanism can be believed
through, where three campaigns running had built mechanisms their own headline
number could not see. **Two honesties came with it, because this thread has
paid for their absence.** The instrument reads 63.77% against a route-blind
`Claim`-diff of 62.64% over the same population; the surplus is not
over-counting but the route and width channels living *outside* the `Claim`,
confirmed by the day and hops channels reproducing the `Claim`-diff to the
digit. And the Undertow's 41.9% selection-swap value-churn did not reproduce on
this tree (38.37% now); it was recorded as substrate drift, not rescued, and
the frozen 0.20 success floor was not lowered — the discrimination clears the
unlowered floor more than threefold. The next question is the one the touchstone
is a *precondition* for and does not answer: whether a penalty magnitude read
from contact history does anything a well-chosen constant would not.

### The lean can be derived, but the derivable axes cannot like (2026-08-20)

[The Cant](./chronicle/the-cant.md) takes the thread's other half — not how a
belief *travels* but what an evaluative belief *is* — and asks whether a
believable snap-judgment predisposition between peoples can be **derived** from
authored attributes rather than authored directly. It ships the mechanism the
"situated error" target needs: `v(A→B)` over eight attribute-distance axes,
weighted by the judging people's own psychology, projected onto a
warmth × competence plane, 0021-clean because the only thing that ranks is the
judge's own derived weight-vector. Four of five structural floors hold — the
matrix is asymmetric (all 105 unordered pairs differ by direction), similar
peoples land warm (`r = −0.896`), the fifteen personalities are distinct.

**The mechanism half of the bet advances; a new gap opens beneath it.** The
fifth floor — *it likes* — is falsified: of 210 cross-people judgments, zero
reach admiration (124 contempt, 80 envy, 6 pity). The derivable axes produce a
predisposition that is relational, asymmetric, and derived — exactly what the
prohibition wanted — and that predisposition is *only ever cold*. This is not a
retune artefact: every axis's signature pushes warmth down from its maximum, so
warmth has nowhere to go but down, and there is no positive-warmth channel in
what the current substrate can express. The null was reported, not rescued; no
constant was moved to reach or dodge it.

**The re-score, and the constraint it exposes.** What advances is the demonstration
that situated error is *constructible* — a world's peoples can hold strong,
relational, mutually-inconsistent views arrived at from what they materially are,
with no authored ranking. What is newly owed, and newly scorable, is a
constraint the null makes concrete: a world whose derived prejudice makes every
people a xenophobe toward every other does not *function* — an elven merchant
who can only sneer does not trade — and a simulation of that is worse, not truer,
than one where views are held, are wrong, and still leave room for commerce and
exchange. So the target is sharpened from "derive prejudice" to "derive prejudice
that leaves the world livable," and the levers are named: a positive-warmth
substrate (appearance, kinship), a baseline consideration between sapients, and a
perturbation that recentres sentiment on the neutral point rather than the
negative. **One thing that must not be read into it:** the null is
*near-boundary* — the warmest pair misses admiration by 0.048, and the neutral
point it misses against was a free parameter frozen before the measurement, not a
substrate fact. So "zero admiration" scores the current axes and the current
neutral point together; it is a floor beneath the thread, not a wall.


### A constant column looks exactly like a well-behaved one (2026-08-25)

[The Confidant](./chronicle/the-confidant.md) staked no headline bet of its
own — it built an instrument — and the reason it belongs in this chapter is
what the instrument did to the accounting when it was pointed at itself.

Three measures of conceptual deficiency were registered as forty-five census
columns, one family per people. A thousand-seed run returned **every one of
them constant on every row**. The mechanism was then checked rather than the
number accepted, because constant-across-seeds has one benign cause and one
broken one: these read the real per-world vocabulary and then branch on
whether the culture *has* the concept, which is exposure-determined and
derives from authored species attributes. The word's form varies with the
seed; its existence does not. **Correctly seed-invariant, not a broken read.**

The entry above records that a column's value being stable across campaigns is
evidence about the world only if something independent establishes the column
can move at all. This is that observation's twin, one level down and cheaper
to check: a column can be constant *within a single run*, and it will look
identical in the drift check to the healthiest column in the table. It cannot
detect drift, which is the census's whole job — it moves all thousand rows at
once or never — and it will keep costing whatever it costs, forever, on every
world of every all-metrics study. Both halves of the diagnosis were paid for
in canonical-box time, six runs against an authorisation given for one.

The remedy is one question asked before registration rather than after: *does
this column take more than one value across the seeds it will be computed on?*
The measures themselves were not wrong and did not change — they moved home,
into a window that computes them once and publishes fifteen rows (decision
0260). The precedent for that home had been in the repository the whole time.

**What this does to the chapter's own confidence accounting.** Nothing in the
standing gate asks a new metric whether it can vary, and nothing asks an
existing one whether it still does. Until something does, a census column's
stability is a claim about the instrument and the world jointly, and this
chapter should not read a flat column as agreement between them.

### The substrate is cheap to write and unpriced to read (2026-08-26)

The first entry in the high-confidence tier above is the kernel substrate, and
one of the four things it names is append-only event-sourced storage: chosen to
be boring, and the choice paid. [The Tailrace](./chronicle/the-tailrace.md)
does not disturb that. What it disturbs is a reading of it — that a substrate
whose *writes* are boring has therefore been priced.

Appending a fact is O(1) and always was. Reading a fold over the accumulated
facts is not, and Hornvale's creature-drive stack recomputes five such folds
over an agent's entire committed position trail on every evaluation, per agent,
per tick — plus a sixth that does it once per co-located peer. (A sibling fold
over `rested` events is timed alongside them and is the one that is cleanly
*not* history-proportional; the count here is the trail-walkers, which is the
set that makes the cost grow.) Measured on a fixed roster over two hundred ticks, with the
deterministic columns held as a control and flat, the history term is **70 to
80 per cent of a tick**, which makes total session cost quadratic in session
length. That is a cost of the log, and nothing in the tier above was ever
scored against it: every check that scored the substrate honestly — *this
changed nothing*, byte-identical regeneration — is a check on **output
identity**, and cost is invisible to all of them.

The sharper half is that the shape was worse than the campaign's own
preregistration predicted, and the campaign found that out by accident. It
froze a linear model, `cost = C + k·h`, and looked for `k > 0`. One fold is
**quadratic**: `integrate_thirst` runs a backward scan over the whole sightings
timeline inside a loop over the sightings since the agent last drank, so its
cost is `O(H + S·H)`, with `S` reset only by a `drank` fact. Twenty-three of
fifty agents drank zero times in two hundred ticks. A preregistered hypothesis
was met, and the mechanism behind it was not the one written down.

**What this does to the chapter's own accounting.** There is no cost gate on
the sim at all — no ceiling, no slope check, nothing that runs per commit — and
the two instruments that exist were built by the two campaigns that needed
them. So the substrate's confidence tier should be read as covering *what the
world computes*, not *what computing it costs*: the first is scored
mechanically and continuously, and the second is scored when a campaign
chooses to look. On the two occasions a campaign has looked, it found a
quadratic ([The Penstock](./chronicle/the-penstock.md)'s unindexed scan) and
then a second one nobody had predicted. That is a poor base rate for an
unwatched dimension.

It also supplies this chapter's preamble with a fresh instance of its own
floor. The rule stated there is that the only thing distinguishing a check that
fires from one that does not is **making it fail on command**, and the campaign
found two defects that way that no amount of reading found: a
fold-equals-scan test comparing a function against itself, because the oracle
it compared against was implemented by calling the thing under test; and a
chaos-eviction schedule whose most aggressive form gives the least signal,
because it replaces the state immediately after every step it is meant to
stress. Both were green. Both were pinning nothing. Neither would have been
found by reviewing the tests against their specification.

### A third look at cost, and it found a distribution rather than a pathology (2026-08-29)

[The Overture](./chronicle/the-overture.md) is a client campaign and **no bet
in the map above moved** — it resolves no open question about the world, and
nothing it built crosses the determinism boundary. It contributes one thing to
the accounting immediately above, and only because that accounting counts
occasions rather than results.

The section before this one observes that cost is scored "when a campaign
chooses to look", and that on the two occasions anyone had, each found a
quadratic nobody predicted. This is a third occasion, and it found neither a
quadratic nor anything else pathological — it found a **distribution**, which
is a different kind of answer and worth distinguishing from a clean bill of
health:

```
  settlements               1840 ms    60.2%
  WorldContext::build        830 ms    27.2%   (demography report: 480 ms)
  terrain (genesis)          202 ms     6.6%
  deep time                  181 ms     5.9%
  astronomy                    0.4 ms   0.01%
  ------------------------------------------
  total                     3054 ms
```

Two items are **76%** of world generation. The 480 ms demography report is the
largest single item in the build that has never been profiled by anyone — this
campaign measured it and deliberately did not touch it. So the base rate the
paragraph above reports is unchanged in the direction that matters: three
campaigns have looked at cost, two found an unpredicted quadratic, and the
third found a concentration it did not investigate. Nothing here is a
counterexample to "a poor base rate for an unwatched dimension"; it is one more
observation that the dimension is unwatched.

The measurement is also a reminder of what *this* chapter's floor asks for.
These figures are five agreeing runs on one machine on one day, and they are
already load-bearing for a shipped design decision — a progress substrate
refuses to draw a global percentage *because* one phase is 60% of the whole. A
number with that much weight on it should be re-measured by whoever next
depends on it, not inherited.
### A branch table is not a discriminating test (2026-08-30)

[The Winze](./chronicle/the-winze.md) staked a bet this chapter does not carry
— nothing in the list above is about subsurface residue, memory decay, or a
survivorship shape — and it belongs here anyway, because it ran a
self-scorable measurement twice into the wall this chapter keeps rebuilding:
**an instrument that reports a verdict it cannot support.**

The measurement was frozen before the code and reads like rigour. Three
outcomes were enumerated: the two depth distributions are indistinguishable and
the mechanism is decoration; they separate with overlap and the claim holds;
they separate perfectly and the hazard has become the depth threshold the
design replaced. The result landed on the middle branch, nothing was tuned, and
the campaign could have stopped there.

**It could not have told that pass from a much weaker one.** *Delvings that
broke through sit at their own maximum without anything selecting on depth* and
*breaking through is a tenure lottery and depth is a bystander* produce
**identical pooled distributions**. Breached tenure runs at a median of 17.5
epochs against 3.0, so the weak reading was live and large, and the statistic
that separates them — conditioning on tenure, under which the separation
attenuates from AUC 0.8654 to 0.7599 and *survives*, holding direction in every
stratum — appears nowhere in the frozen criterion. It reached the measurement
through a dispatch that happened to name the property. Had it collapsed, the
criterion as written would have reported support.

The chapter's preamble already holds that the only thing distinguishing a check
that fires from one that does not is making it fail on command. This is the
same floor one level up: **a branch table over outcomes is not a discriminating
test, and both look like rigour.** A criterion is finished when, for the outcome
that would count as support, it names the rival explanation and the statistic
that tells them apart.

**And the second instance is worse, because it precedes the data entirely.**
The mechanism is a per-increment chance of breaking through, and the design
never said what an increment is. Every other rate in the settlement bake is per
epoch, which is the obvious reading. Under it, breaking through and the ordinary
endings are competing risks in time: with constant per-epoch rates `p` and `q`,
the probability that an ending at epoch `t` was a breach is `p/(p+q)`,
independent of `t`. The two groups share a tenure distribution exactly, depth
accrues with tenure, and the preregistered null fires **as a theorem** — on a
mechanism never given a chance to produce the effect. Clocked per metre cut it
works, and nothing reads a depth. **When a preregistered comparison is between
two sub-populations of one process, the clock the process runs on can decide the
answer before any data exists**, and a clock that makes the null a theorem is
not a modelling choice but a way of not running the experiment.

**What this does to the chapter's own accounting.** Self-scorability has been
carried here as a property of a claim, then sharpened into a property of a
statistic. It is narrower still: it is a property of a statistic *plus the
alternatives it can exclude*. Every bet in the sections above that names a
success criterion should be read as scoring the outcome, not the mechanism,
unless something in it says which rival readings a pass rules out.

### A third channel now carries unrest into settlement siting (2026-08-30)

The entry above records a campaign whose instrument confirmed an effect and
could not find its cause: settlements over-occupy high-unrest ground (×1.572,
×2.578, ×5.395 up the unrest deciles), and severing **both** modelled channels
through which unrest reaches siting did not flatten the gradient. Its honest
scope clause is *unattributed by this instrument on this roster*, and the
strongest thing said about it was that neither modelled channel carries the
effect.

There are three channels now. A mining camp is founded by scoring candidate
sites on mineral prospectivity, and prospectivity is
`0.6·setting + 0.3·unrest + 0.1·metamorphic_grade` — so a working is sited
partly *because* the ground is tectonically violent, and the coupling is direct
rather than incidental.

It is far too small to be the missing cause. Pooled over twelve seeds the world
carries 196 mines against 9,394 agrarian settlements, 2.0% of occupations, which
cannot produce a fivefold excess in the top unrest decile. **The finding is not
that the puzzle is solved; it is that the ablation's denominator moved.** A
future disambiguating arm must sever three channels rather than two, and one of
the three did not exist when the arm was designed. This chapter's own warning
about ablations applies with the sign reversed: an ablation on a channel almost
nobody reads is a null with no power, and an ablation that misses a channel
entirely is a null with a hole in it.

### The fourth look at cost found the quadratic and removed it (2026-09-02)

Two sections above record that cost in this project is scored "when a campaign
chooses to look", that on the first two occasions each look found an
unpredicted quadratic, and that a third found a distribution instead. This is
the fourth, and it is the first one that closes a loop rather than opening one:
[The Pawl](./chronicle/the-pawl.md) migrated the creature-drive reads off the
raw position history and onto a session-owned store of advancing accumulators,
and measured the thirst and hunger reads two hundred and thirty times cheaper,
the whole tick a quarter cheaper at two hundred ticks, and the level about four
per cent lower.

**What that does to the accounting is smaller than the numbers suggest, and
saying so is the point of re-scoring rather than celebrating.** Three things
are worth carrying forward.

**First, the quadratic that was found is gone and the shape that found it is
not.** There is still no cost gate on the simulation — no ceiling, no slope
check, nothing that runs per commit — and the two instruments that exist are
still the ones the two campaigns that needed them built. A fourth look
succeeding does not change the base rate of an unwatched dimension; it changes
one number in it.

**Second, and sharper: the campaign's frozen success criteria could not see
its own result, and both instruments were working correctly.** Five of six
preregistered criteria came back not met on the first readout, while a
synthetic sweep over a thousandfold range of history reported that the order of
the computation had changed — a ninety-nine-fold saving at ten thousand facts.
The reconciliation is arithmetic: at the depth an ordinary session reaches, the
predicted saving was smaller than the ecological instrument's own run-to-run
spread on the very column being measured, and the criterion had asked that
instrument to resolve it. **A criterion written against an ecological bench
cannot see a change of order that the ecological range does not reach.** This
chapter's standing floor is that a check is only worth what it can be made to
fail on; the companion is that a *criterion* is only worth the range its
instrument samples, and neither of those is visible in the criterion's own
wording.

**Third, the failure that remained is more interesting than the one that was
fixed, and it was invisible until the fix landed.** Three criteria still fail
after the campaign, all of them on one fold — the fear memory, which costs 93
milliseconds per call at the deepest band measured and is 84% of the six timed
reads' total, against the repaired thirst and hunger pair's combined
five-thousandths of a per cent. Before the repair, that dominance was hidden
inside a total that two folds shared. **Removing the largest known cost is what
makes the next one legible**, which argues for iterating the measurement rather
than treating a single readout as a verdict on a subsystem.

One further note for this chapter's own honesty. The campaign made exactly one
change to production code after seeing its first readout, and reports both
readouts rather than only the second — the first stands unedited, and the
second is explicitly not blind. Nothing in the standing gate would have caught
a quieter version of that ordering, and nothing here claims otherwise; it was
disclosed because the campaign chose to disclose it.

### The gate the fourth look said did not exist now exists (2026-09-02)

The section above, written days earlier, states the standing position on cost
plainly: "there is still no cost gate on the simulation — no ceiling, no slope
check, nothing that runs per commit," and the two instruments that exist are
the ones the two campaigns that needed them built. [The Rack](./chronicle/the-rack.md)
moves that, and the way it moves it is the part worth re-scoring rather than
the fact that it did.

**The instrument that existed was not merely blunt; it was not running.**
`session_cost.rs` bounds a pooled wall-clock median, and its own doc already
conceded that twenty of the fifty samples that median is drawn from exceed the
ceiling individually while the gate passes. That is the bluntness anyone would
have predicted. What nobody had checked is that its millisecond assertions are
gated to a host the test no longer runs on — they fire only on the Mac, and
the tier that runs them moved to the canonical Linux box. Measured at close, on
one quiet box in one profile: **the test passed at main's tip at 81.490 ms
against a 9 ms ceiling.** [The Roll](./chronicle/the-roll.md) had moved that
control roughly twenty-one-fold and every gate stayed green, not because the
threshold was generous but because no threshold was being evaluated.

**A gate's blind zone can be structural rather than statistical, and only one
of those is visible in its own documentation.** This file's standing floor is
that a check is only worth what it can be made to fail on; the companion the
fourth look added is that a criterion is only worth the range its instrument
samples. This is a third: a check is only worth the *configurations it
actually runs in*, and the one place that is never written down is the
intersection of a test's host guard with the tier's host policy — two
correct-looking facts in two files that nothing compares.

**What replaces it changes the shape of the bet, modestly and legibly.**
Per-turn work is now a **count** — folds, plan searches, ledger position folds,
shadowcasts, bodies scanned — asserted per verb class. A count is identical on
every machine, so it belongs in the commit gate and cannot flap; it went red on
the pre-change tree at exactly the preregistered numbers and green after. That
is the first per-commit cost gate this project has had, and it is deliberately
narrow: it covers the turn path of one window, and it cannot see a regression
that performs the same operations more slowly. **The unwatched dimension is
smaller, not closed.** The Rack's own residue — seventy kilobytes of JSON and
one eight-millisecond shadowcast — is exactly the shape no counter bounds, and
it is why this campaign's own wall-clock prediction was falsified while its
counted one landed exactly. *(Corrected 2026-09-03 by The Terrier: the eight
milliseconds were never the shadowcast, which costs 0.012 ms at radius four.
They were the brief re-surveying the whole world's occupation register on
every call, two to five times per indoor turn — a step the counter could not
see because nothing counted it, which is the point this paragraph makes and
the reason the number was filed under the wrong noun. The register is now
built once; the chamber snapshot reads 0.5 ms.)*

### The fifth look at cost found a repetition, not a quadratic (2026-09-03)

Four sections above record that cost in this project is scored "when a campaign
chooses to look", that the first two looks each found an unpredicted quadratic,
that a third found a distribution and a fourth closed a loop, and that the same
campaign that closed it left one fold failing three criteria at ninety-three
milliseconds a call. This is the fifth look, and it is at that fold.

**What it found was not a quadratic. It was a repetition.**
[The Detent](./chronicle/the-detent.md) counted the fold rather than reading it
and found that the whole of its cost was *static terrain, re-sampled every
tick*: fifty agents walking one tick asked the world about 44,694 rooms — about
eleven thousand of them distinct — in order to commit thirty-one facts, and the
ground had not moved between any two of those questions. Holding the verdict
for the session and letting the scan advance over new sightings took the fold
from 93.841 milliseconds per call to 0.096 — 975-fold against a same-box
control, 758 to 1,008-fold against the frozen figure — and its sensitivity to
history from 0.91 to 0.04. The whole tick's level fell about nine per cent.

**What that does to the accounting is again smaller than the numbers, and again
that is the point.** The first per-commit cost gate now exists — the section
above records it — and it is a **count** over one window's turn path. It cannot
see this fold: none of the reads this campaign made a thousand times cheaper is
among the operations it counts, and a count is by construction blind to a
regression that performs the same operations more slowly. So the fifth look
succeeding does not change the base rate of an unwatched dimension either. It
narrows the unwatched region by one fold and leaves the shape of the bet where
the fourth look left it: **cost is still scored when a campaign chooses to
look.**

**The sharper finding is not about cost at all. It is about evidence.** The
mechanism this campaign was expected to build had been named in a committed
record by the campaign immediately before it, in prose that called itself
"legible from the code rather than merely suspected." A count on the criterion's
own instrument found that mechanism reached **zero times**, at every depth, on
two seeds. The proposed fix would have moved the criterion by nothing.

That is the **second consecutive campaign** whose named mechanism was wrong
until someone counted it, and both were written by people who had just spent a
campaign inside the code they were describing. This chapter's standing floor is
that a check is only worth what it can be made to fail on; the fourth look added
that a criterion is only worth the range its instrument samples, and The Rack
added that a check is only worth the configurations it actually runs in. This is
a fourth, and it is about the *reasoning* rather than the instruments: **reading
code produces a hypothesis about a mechanism, never evidence for one.** A count
is cheap — this one took under a minute — and the only thing that makes its
answer usable is a denominator, because a zero and an unwired instrument produce
identical output.

**A criterion can fail at the finish line by succeeding.** The frozen criterion
for this fold counts only runs whose fit clears a goodness-of-fit floor. On the
control column it admitted four of four; on the campaign column it admitted
**none of four** — because the slope is now 0.023 against an intercept of 90.6,
so there is no slope left for a line to explain and a fit to a flat scatter has
a poor fit by construction. The filter did its job correctly twice in the same
campaign, catching exactly the two contended runs the load rule caught
independently. Applied to a criterion that has succeeded, it empties the sample
and leaves the frozen statistic undefined. The readout reports it that way
rather than resolving it silently in either direction, and hands forward the
observation that a criterion about a *slope* wants an effect-size floor rather
than a fit floor.

One further note for this chapter's own honesty, in the same terms the fourth
look used. This campaign also made exactly one change to production code after
seeing its first readout, and reports both readouts rather than only the second:
the first stands unedited, and the second is explicitly not blind. Its
verification clause was itself wrong — it tested a level to decide a question
about a slope — and it was corrected in a ruling written before the change was
made and after the comparison under both readings had been recorded. Nothing in
the standing gate would have caught a quieter version of either ordering, and
nothing here claims otherwise.

### Re-deriving catches what re-reading cannot, fourteen times over (2026-09-03)

Four sections above hold that the wall this chapter keeps rebuilding is **an
instrument that reports a verdict it cannot support**, and that the only thing
separating a check which fires from one which does not is making it fail on
command. [The Nettle](./chronicle/the-nettle.md) is not a world measurement and
stakes no bet in the lists above. It belongs here because it ran that wall
fourteen times in one body of work and can say something about the *shape* of
the failure rather than another instance of it.

**Fourteen claims failed checking; every one was caught by somebody
re-deriving it, and not one by somebody re-reading it.** Half were written by
whoever was coordinating the work. The material was ordinary — inherited
notes, its own prose, its own plans, and two of its own repairs — which is the
point: no instance required carelessness, and re-reading was performed on all
of them.

The strongest instance is a check that was green and blind at once. A rule for
stripping quoted text ran as two passes, single quotes then double, so any two
apostrophes on a line paired regardless of the phrases they sat inside — and a
forbidden command sandwiched between two harmless remarks vanished, returning
approval. **Every test case for that rule had at most one quoted phrase per
line, and the fault needs two.** The cases were reasonable, the coverage read
as broad, and the criterion as written would have reported support forever.
That is the same structure as the branch table three sections above: not a
criterion that was wrong, but a criterion whose *inputs could not contain the
defect*.

A second instance sharpens the point about red results. A guard built with two
arms — one for a new violation, one for a stale exemption — was probed, went
red, and was recorded as discriminating. The red came from the first arm; the
probe could not reach the second, because a single mutation makes both
conditions true at once and the earlier assertion fires. **The second arm was
correct and unexercised, and the run that "proved" it never touched it.** A red
is evidence about the assertion that produced it and about nothing else.

**What the campaign adds beyond another instance is a decomposition.** A claim
about a *particular thing* — this path, this call site, this value — has its
truth in a location, and goes stale only when someone edits that location,
which is the same event as the staling. A claim about *the whole* — nothing
pins this, no test covers that — is falsified by an edit anywhere, by people
who are not looking at it and do not know they have done it. Both are filed
identically, with a confidence marking fixed at writing. The two inherited
records that were false were both of the second kind; the one that held was of
the first. Three cases prove nothing, but the mechanism is structural rather
than statistical: **the second kind has a half-life and nothing in the filing
records it, or the state it was observed against, or the command that would
re-decide it.**

Unscored deliberately, and the reason is this chapter's own: the claim that
re-derivation catches what re-reading does not is itself of the second kind.

### A criterion downstream of a quantizer (2026-09-03)

[The Hachure](./chronicle/the-hachure.md) moves **no bet in the maps above**.
It is a rendering campaign: it changes what the game client draws and changes
nothing about what the world is. It belongs in this chapter for the same reason
the section above it does — it produced one more instance of the wall this
chapter keeps rebuilding, and the instance has a cause the previous ones do not.

The sections above describe criteria whose *inputs could not contain the
defect*: a branch table whose cases each had at most one quoted phrase, a red
result produced by an arm other than the one under test. This campaign's is a
third shape. **Four successive test drafts passed against unfixed code, and
every one of them was measuring a quantizer rather than the thing being
refined.** Each asserted about the rendered relief band. The band's rungs are
hundreds of metres wide; within one ~110 km terrain sample a real height ramp
almost never crosses one. So the refinement under test was enormous — distinct
heights on a plate went from 1–4 to 612–3,860 — and exactly zero of it reached
the observable every draft had chosen.

Not one of the four inputs was unreasonable, and no amount of widening the
cases would have helped, because the defect is not in the inputs at all. It is
one layer downstream, in the function mapping the measured quantity to the
observable. **A criterion can be blind because of what it looks at, and not
only because of what it is shown.**

The repair was to measure first and let the measurement name the observable,
rather than to write a better assertion. That ordering is the part worth
carrying, and it is the same ordering the sections above arrive at from
different directions: an instrument's authority comes from having been made to
fail on the real signal, and a criterion nobody has watched respond to the
signal is a description of an intention.

A second, smaller instance from the same campaign points the same way from the
opposite side. A wrap defect survived both tests written for the feature that
contained it, and was caught by an unrelated invariant — a cache's
byte-identity check — because both new tests drew a full-width window at origin
zero, where the defect cannot occur. **Tests written alongside a feature
inherit the author's model of it, gaps included.** Invariants written for
something else are, structurally, the cheapest independence available.

Unscored, and for this chapter's own stated reason: the claim is about the
whole rather than about a location, and nothing in its filing records the state
it was observed against.
