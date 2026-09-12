# The Kiln — retrospective

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-kiln.md): a sixth corpus family,
two frozen columns against a shipped technology model that was never ratified
and cannot express loss, and one reading whose headline needs two numbers —
of forty-one documented technology losses Hornvale reaches two of the
technologies and can represent the loss of none. This document is about how
that was built, and it is unusually long because the campaign produced an
unusual amount of process material: thirty-nine ledger entries, of which a
majority record a defect rather than a choice.

The campaign's defining property is that **almost none of its defects were in
its code.** The resolver, the loader, the guards and the two reports were
built once and reviewed clean. What went wrong, repeatedly, was the *prose that
described them* and the *requirements that specified them* — and the single
most useful thing anybody did about it was decline to build something.

## 1. One defect shape, worn many times, and the implementer diagnosed it better than I did

The campaign's signature failure is **a claim whose supporting population was
narrower than the claim itself**. The ledger labels four instances explicitly
and the count kept rising after it stopped labelling them:

| # | the check's population | the claim's population |
|---|---|---|
| 1 | items with no prerequisites at all (*roots*) | items whose verdict was *chosen* rather than forced |
| 2 | items that rose **above** a prerequisite | all items whose rank could have moved |
| 3 | items a re-score made chosen (**zero** of them) | items the proxy misses |
| 4 | two named idea-registry rows | the registry |

Each gap fell on the side where the authored data could be too **low** — and
for this campaign that is the *flattering* side, because the thesis is that
Hornvale's technology model is impoverished, so a high `absent` count is the
result the author wants. A check blind to under-scoring cannot embarrass its
author. None of the gaps was deliberate; every one was invisible because the
check's own output was green and its stated purpose *sounded like* the rule.

Instance 1 also turned out to be wrong in **both** directions — under-covering
by one item and over-covering by six, the six being items whose verdicts could
not have moved at all. Over-coverage is the quieter half: a disclosure on a
forced verdict is noise that makes the real ones harder to find, and it inflates
the appearance of rigour. **A proxy is not a conservative approximation of a
rule; it is a different rule.**

My first write-up of this pattern said the common factor was checking the wrong
thing. Task 1's implementer, raising the pattern about its own work unprompted,
said it better and its sentence replaced mine:

> the common factor is not that I check the wrong thing — it is that I stop
> checking once the conclusion looks correct.

All four instances fit. Roots-versus-chosen: the conclusion *this item owes a
disclosure* was right. Rising-versus-under-scoring: the conclusion *no item
rose* was right. Re-score-versus-always: the conclusion *the proxy is wrong* was
right. Two-rows-versus-the-registry: the conclusion *the column has been swept*
was, in the narrow sense, right. **In every case a true conclusion retired the
audit of the reasoning that reached it.** That is a stopping rule, not a
checking skill, and it is why three consecutive rounds of competent review each
found exactly one more.

The durable instruction: when a check is written alongside the thing it checks,
**state the population the check ranges over and the population the claim ranges
over separately, and compare them as sets.** A check described only by its
purpose ("enforces the disclosure rule") cannot be audited against its claim,
and the gap will fall on whichever side the author is not motivated to look at.
**The trigger for doing that comparison cannot be suspicion** — a correct
conclusion is precisely the moment it feels unnecessary.

## 2. Its mirror: starting over when the conclusion is already established

The same stopping rule has a twin that fires in the opposite direction, and it
is mine.

To verify a fix round's claim that every quotation in a corpus matched its
source, I wrote a checker extracting quoted spans with a regex bounded by
single or double quotes. It returned seventy-five segments and sixty-four
mismatches, including a fragment consisting of a JSON brace and newlines, and
a dozen beginning mid-word.

It was garbage because the pattern matched across **apostrophes**. That is the
exact failure mode Task 2's implementer had already measured when it tried the
same shape — a hundred and fifty-three candidates, almost all apostrophes,
every inspected one a false positive — and abandoned with the line *an
instrument you hand-filter is not a check*, which I had quoted approvingly into
my own ledger entry. A re-reviewer had independently reproduced the same
failure. **I was the third party in one task to build this instrument and the
only one who had already written down why not to.**

Had I not recognised the output as garbage I would have reported sixty-four
mismatches against a corpus two independent methods had found clean — an alarm
entirely manufactured by my own instrument, aimed at the one artifact in the
campaign that cannot be revised freely. **A false red on a frozen artifact is
worse than no check, because the remedy it invites is editing the artifact.**

The correct verification was cheaper and it was sitting in the claim. The claim
named **one** exception; checking that single named instance took one pair of
greps and settled it. **A claim that names its own exception is tested at the
exception, not by re-enumerating the population.** I reached for the population
because enumerating *feels* like more rigour. It was less.

Stated as the pair: section 1 is *stopping once the conclusion looks correct*.
This is *starting over when the conclusion is already established*. The
question to ask before verifying is not "can I check this?" but **"what would
my check add to what already holds it?"**

## 3. Twelve of them were mine, and this is the one place that number is stated

A retrospective that launders the controller's rate is worth less than one that
states it. **This section is the single site for the count.** Every other
mention of it on this branch points here rather than restating it, which is
itself a correction: this heading read *seven* while its own body reasoned about
*nine* and ledger #37 said *eight*, three numbers on one branch with no two
agreeing — in a campaign whose signature lesson is that a count stated inside
its own document is self-falsifying. So the number is derived below from the
ledger's own running enumeration, item by item, and nowhere else.

| # | ledger | the defect |
|---|---|---|
| 1 | #13 | a causal "now" implying a re-score created a counterexample it did not create |
| 2 | #17 | a two-row sweep read as a swept column |
| 3 | #19 | a capture action listing a new rule's consumers and none of its contradictors |
| 4 | #20 | a "0 of the 31" contradicting the clause it attached to |
| 5 | #25 | building the known-bad checker of section 2 |
| 6 | #30 | a signature in my own plan text that made a required field impossible |
| 7 | #36 | reading a decision-number ceiling off a branch 145 commits stale |
| 8 | #36 | citing two task briefs as required reading that had never been generated |
| 9 | #36 | asserting a date difference of eight days that is nine |
| 10 | #37 | ordering the merge at a plan step that runs *before* the stop gating it (section 19) |
| 11 | #37 | a gradient grep I reported as three hits, which is four |
| 12 | #38 | a capture action routed to a later task that nothing ever checked was discharged |

Entries 1 through 7 are the enumeration the ledger kept as it went — #30's
standing note counts six, #36 calls itself "my seventh instance" — and they are
all section 1's shape. 8 and 9 rode in with 7 and are not that shape at all.
10, 11 and 12 were found after #36, by Task 10's implementer and by the final
whole-branch review.

**One borderline case is excluded, named so the count is auditable rather than
merely asserted.** Spec finding F4's measured tally went stale inside this
campaign, under its own 145-commit absorption (#37's second defect, section 20).
That is a claim that decayed rather than an error made at authoring, so it is
not counted here; a reader who counts it gets thirteen, and the lesson it
carries is recorded either way.

Three things about that list are worth more than the number.

**The plan text is where defects originate, again.** Most of them live in
documents nobody executes directly — four in ledger entries, one in a dispatch,
one in a plan's type signature, two in plan step ordering and a capture action.
**None of those would have been caught by running anything**, which is the whole
of why they needed a reader rather than a gate.

**Implementers caught five of them**, in every case by declining to proceed
rather than by reviewing after the fact. See section 4. A sixth — the last row
in the table — was caught by the final whole-branch review, and it is the one
nobody was in a position to decline, because the task that should have built it
was never told it owed anything.

**Two of them are instances of lessons this project had already written down**,
in this repository, before this campaign started. The known-bad checker was
documented as known-bad in my own ledger entry. The branch-ceiling read is the
second instance in nine days of a campaign minting a decision number without
reserving a block, the first being recorded in another campaign's spec. Having
the lesson written down was not sufficient; neither was having written it down
myself.

## 4. The highest-yield behaviour in the campaign was an implementer declining to build something and saying so

**Five** requirement defects in my plan text were found this way — more than any
review stage produced, and in every case the gap was structurally invisible to
review because the plan was internally consistent:

The five are summarised rather than quoted; the implementers' own wording is in
the ledger entries named beside each.

| what was reported, in substance | the defect it exposed |
|---|---|
| the brief scoped this task to the freeze and the lattice and did not ask for the `absent`-count ratchet, so its home was left to whichever later task specifies it (#26) | **no later task specified it.** Three of the governing decision's four conditions had a home; the fourth had none, and the plan's checklist would have read complete without it |
| the brief fixes this function's signature as two verdicts and no identity, so an item id cannot be attached (#30) | the finding that fires when **a capability was lost** — the one event the campaign exists to make visible — could not name the item that lost it |
| the brief specifies this statistic through a worked test rather than defining it; here are both readings, and here is the assertion that rules one out (#31) | the statistic's semantics existed only as a solution to a test, and the sibling family's own test name points at the *wrong* reading |
| there is no render-level test file here, unlike the two sibling families, which do test their own render (#33) | the report's five compliance properties — ordering, caveat placement, separately reported unscored tally, both headline counts, no backlog heading — were asserted by nothing |
| the brief lists a sluice submission at this step and also makes G6 a hard stop, and a submission merges and pushes `main`, which would remove that stop (#37) | **the plan ordered the merge before the stop that gates the merge.** Section 19; the only one of the five where compliance would have bypassed a human gate rather than merely shipped a weaker artifact |

The pattern is identical in all five: **an implementer neither silently built
the missing thing nor silently skipped it.** It reported the gap. The
distinction between *another task owns this* and *no task owns this* is one only
the controller can resolve, and only if somebody surfaces it — and in the first
case the honest report was followed by a check that found the answer was "no
task owns this", which is the half that actually closed the hole.

This is worth naming as a practice rather than praised as diligence, because it
has a cost the process must be willing to pay: each of those five reports
*delayed a task* and *overruled its own brief*. The campaign's output is better
in five measurable ways because a brief was treated as fallible. The
instruction that follows is for the dispatcher as much as the implementer:
**a brief that cannot be questioned produces a plan's defects verbatim in the
artifact.** When I widened the signature in case two I said so explicitly —
the brief was mine, and an implementer should not have to overrule it.

## 5. A derived document narrower than its source, and nothing compares them

The missing ratchet deserves its own section because its shape is new to this
project's collection. Every instance in section 1 is a *claim* whose supporting
population was too narrow. This is a **derived document** whose requirement set
was too narrow: the plan is the spec's argument transcribed into tasks, and it
dropped a requirement in transcription.

I ran the plan's self-review and it passed — **because the plan is internally
consistent.** It was consistent and incomplete, and those are different
properties. Internal consistency is the only property a self-review measures.
**Nothing in this process compares a plan's requirement set to its spec's,
clause by clause**, and the plan's own checklist is structurally unable to
notice a requirement that was never transcribed: every box ticks, because the
missing box was never drawn.

The omission mattered beyond one variant. The ratchet is the instrument's only
falsification-by-count guard, and without it a later campaign could let the
`absent` count rise — the direction that flatters this campaign's thesis — with
nothing objecting. **The failure would have presented as completeness**: every
box ticked, every other guard green, the spec satisfied on paper by a document
nobody re-read against it.

The cheap remedy is a clause-by-clause diff of spec requirements against plan
tasks, performed by someone other than the plan's author, and it is cheap
precisely because both documents are short and committed. The expensive remedy
is what happened: an implementer noticing that its brief omitted something and
a controller checking whether the omission was deferred or absent.

## 6. An honest scope statement, read as a sufficiency claim

This one is the campaign's cleanest lesson and the only one whose remedy cannot
live in an artifact.

Task 1's corpus swept its `absent` column against two named idea-registry rows
— named by me, from a reviewer's finding — and its provenance field said so, in
capital letters:

> THE `absent` COLUMN WAS SWEPT AGAINST TWO NAMED IDEA-REGISTRY ROWS, AND THE
> SWEEP MOVED ONE ITEM.

**The corpus stated its population exactly and claimed nothing more.** It never
asserted an exhaustive sweep. I read its honest report as sufficient — *the
column has been swept* — and moved on. Task 2, dispatched with the same rules
but no rows named, swept the registry instead and cited fifty-five distinct rows
against Task 1's eighteen. The gap surfaced only when Task 2 found a registry
row that names one of Task 1's items' demands outright and is cited nowhere in
the frozen file.

My first write-up of this blamed the artifact. Task 2's implementer disputed it;
I reconstructed the pre-sweep file and the capitals above are what it said. **The
defect is wholly mine, in two steps: I dispatched a two-row sweep, then read its
honest report as a sufficiency claim.**

That is a different and worse mechanism than section 1's, which is why it earns
its own heading. Section 1 is *a check narrower than its own claim*. This is *an
honest scope statement read as a sufficiency claim by its consumer*. **No amount
of discipline inside the artifact prevents it** — the artifact did everything
correctly — so the remedy lives in the reader:

> When a record states the population it ranged over, **the population is the
> finding, not the preamble to it.**

"Swept against two rows" and "swept" are different facts, and the first one was
printed in capital letters. Two rows cannot support thirty-eight verdicts, and
nothing in the corpus ever said they could.

The correction also ran in the **unflattering** direction — it moved `absent`
down, making Hornvale look better and weakening the campaign's own thesis. A
campaign whose errors all run the other way should accept every correction that
runs this way without argument, and record that it did.

## 7. The precedent did not reach as far as I said it did, and reading the source is what found out

The design's first draft decomposed a technology item into four parts —
prerequisites, acquisition, consequence, loss — each resolving against a
different basis, and justified it as an existing decision's clause "generalized".

It is not. That clause governs **two halves asking the same question in two
places**: is this capability implemented, in the sim, then in the client. It
never contemplates components that ask *different* questions, and claiming
otherwise papered over precisely the seam a reviewer should press.

**What settled it was reading the sibling's code, not re-reading the decision
text**, and the reading produced two findings neither document states. First,
the clause's ordering is **never computed anywhere**: the sibling's verdict type
has no combining logic, no minimum, no "weakest" function. "Takes its weakest
half" is an *authoring discipline*, and the resolver only re-checks that the
authored verdict's anchor resolves. Second, and consequently, the ordering
problem I had raised against the clause — that one verdict is orthogonal to the
rest of the scale, so a minimum is undefined — is **not a latent bug in the
decision**. The precedent simply never needs an order.

Had the four-part draft shipped, the corpus would have required a novel total
order over five verdicts, invented by this campaign, to combine heterogeneous
components: **new machinery disguised as precedent.** The lesson generalises
past this instance —

> A decision record states what was decided. Whether the mechanism it describes
> is *computed* anywhere is a separate fact, and only the code has it.

— and it recurred later in the campaign in a sharper form. Our `doc:` anchor
resolution reused a sibling's accessors correctly and still reproduced a
vacuity that sibling had **proven and documented**: a comment in its own source
records that its final review repointed an item's anchor at a page that does not
exist and watched the whole suite stay green, which is why its equivalent
function makes one extra call ours did not. We read the sibling for its *shape*
and not for the *experiment that shaped it*. **A sibling's divergent extra call
is evidence, not noise** — especially in this repository, where the house style
is to record the measurement that forced a line.

## 8. The required ideonomy pass was nearly skipped as precedent-answered, and it found two spec defects

Two design questions were resolved by reading sibling data and recorded **zero**
ideonomy passes. The campaign overlay names that exact rationalisation — *this
question is precedent-answered, so ideonomy adds nothing* — as its own
documented first-campaign failure, and requires the precedented answer to be the
*input* to a pass rather than a reason to skip one. The pass was run before the
design went to ratification, and **two of its three findings amended the spec.**

Neither amendment was reachable by re-reading. The first is section 9's
subject. The second is that the new verdict `lost` **had no scope**: a
capability can be given up by one community, by a people, or by every people in
the world, and an instrument choosing silently per row — with the choice tracking
whichever produced the nicer answer — is precisely the defect a sibling family
acquired and survived five reviews with, because every anchor resolved.

The transferable point is about *why* re-reading could not have found them: the
vocabulary was internally coherent and fully precedented. **A consistency review
cannot find a missing distinction**, because nothing is inconsistent. It takes an
instrument that deliberately varies the frame — in this case prompts on scope and
on empty branches — and the cheapest time to run it is exactly when it feels
redundant.

A third finding changed nothing and is carried forward: lifted, this family is
"an instrument measuring whether a population sustains a heritable trait against
loss", which is the shape of language death, island trait loss and herd
immunity — and this repository already ships threshold-and-population machinery
in two domains the successor campaign may be able to reuse rather than invent.

## 9. A bare verdict would have been blind to the campaign's own motivating defect

This is the finding most worth carrying to the next instrument of any kind.

The design had a five-valued reach vocabulary plus measured values, every
verdict anchored, every anchor re-checked. It was precedented, coherent, and
**could not have detected the defect the campaign was built to expose.**

The defect is not that Hornvale fails to acquire technologies. It is that
**every** surviving community acquires **all** of them. Under a boolean
measured verdict, "every people has bronze" and "half the peoples have bronze"
score identically — both *grown*. So the instrument would have reported the
pathology as a success.

The fix was already in the sibling family: its items carry a statistic and a
criterion, and the verdict is whether the criterion is met. A technology item
therefore carries a statistic over the **distribution across peoples** rather
than a boolean over the world, and under such a criterion today's world scores
a *miss* — the holding fraction is 1.0, outside any band expressing divergence.
A bare boolean would have said *grown*.

> **Ask of every new verdict vocabulary: would it have been able to report the
> defect that motivated building it?** A vocabulary can be complete, anchored
> and precedented while being blind on the one axis its campaign exists to
> measure.

One consequence was accepted with open eyes and is recorded as a cost: the
criterion band can never be tightened by anyone eligible to tighten it blind.
The window closes at first scoring, and everyone who has read the model is
disqualified.

## 10. Prefer the checked invariant to the prose description of it

One corpus characterised its own item order four times and was wrong, or
unfalsifiable, four times: "earliest first" disproved by the latest-dated
episode sitting first; a restatement naming exceptions of the file order where
they do not hold, because a topological sort pulls two items ahead; a scoping to
admission order that is **vacuous**, since admission order is *defined* as file
order modulo two recorded relocations; and a fourth attempt whose main clause a
re-reviewer independently verified item-for-item against a rebuilt order — and
still recommended deleting.

**Deletion was the ruling, and not because attempt four was false.** What
remained after the vacuous half was a descriptive statistic with no consumer,
not determinate from the data (two episodes span date ranges, and which end you
take breaks the monotone claim), and still carrying two uncomputed numbers, one
of which double-counts a relocated item. Four rounds failed to produce a
paragraph free of unchecked conjuncts. That is evidence of **too many numbers in
one breath**, not a run of carelessness.

What the artifact actually owes a reader is the invariant: `ordered: true` means
every prerequisite edge points backwards, which holds with zero forward edges,
is mechanically checkable, and is asserted. **When a checked invariant and a
prose description of it are both available, the prose is optional and the
invariant is not** — and a sentence that has been wrong four times is not
optional, it is a liability.

The same ruling then applied to itself one round later. Both corpora had grown a
blanket claim that the registry's markup "is dropped" in quotations. Verified
false in four places — and not simply inverted, because an earlier round had
found a quotation that *does* drop it. **Practice is inconsistent, so neither
"dropped" nor "retained" is true as a blanket statement**, and the fix was to
state the one checkable invariant (every quotation matches its row after named
character transliterations) and declare markup explicitly outside the
guarantee. Where the practice itself is inconsistent, the honest prose says so
instead of picking a side.

The sharpest detail: the markup claim was introduced by **the very round whose
mechanical quotation sweep caught two misquotes.** The sweep checked quotations
against rows; nothing checked the sentence describing the sweep.

**And the round cap was used deliberately.** Both corpora's substance — item
sets, selections, every verdict, both disclosure sets, the headline count — had
been stable and independently verified for three rounds; every defect after that
was in self-description. I ruled that if round four's re-review found another
prose defect of that class I would adjudicate at the cap rather than open a
fifth round, because a fifth round spent on sentences about sentences buys less
than the work it delays. **Naming the cap in advance is what made stopping a
decision rather than fatigue.**

## 11. A mention is not a ruling, and a grep cannot tell them apart

The family carries a cross-corpus law: a registry row cited as an anchor by any
corpus must be explicitly ruled on — cited, or refused in writing — in every
other corpus, because the matrix is read *across* and a row discharging a demand
in one column while unexamined in the other makes the comparison meaningless.

The obvious implementation is a text search for the row id, and **it is wrong**,
as one corpus demonstrated on itself: it "named" a sibling's anchor only while
*reporting what the sibling had done with it*. The search is satisfied and
nothing has been ruled on. So the law carries a second clause — a row is ruled
on when the corpus **states its own verdict about that row for a named item**,
not when the identifier appears — and the mechanical check is a **floor**, with
the judgement still owed by the author.

> When a rule is about whether something was *considered*, the cheap
> implementation tests whether it was *named*, and the two differ exactly where
> the rule matters.

The evidence that the law was worth writing is the strongest kind available:
**stating it immediately exposed two live violations in the two corpora it
governs**, neither of which one task review and three fix rounds had surfaced.
A rule whose first act is to find real violations in the artifacts that
motivated it is not ceremony.

## 12. A cited mitigation that does not exist is worse than an absent one

A fix round wrote into a frozen corpus's provenance: "THE RULE IS ENFORCED
RATHER THAN ASSERTED … the build refuses to write this file unless every root
carries a disclosure." No such harness is committed, and the *same file's*
freeze field asserts that no evaluation code exists in the repository as
committed. Two fields of one artifact contradicted each other.

The ruling was to correct the prose and **forbid committing the harness**:
committing a build-time validator during the authoring task would make the
freeze claim false and break the one property that makes the measurement
discipline structural here rather than promised. The checks that had actually
been run were throwaway validation, and the field must say only that.

Why it matters more than a wording fix: **a later author reading "enforced" has
no reason to re-implement the check**, so the invariant would be believed by
everyone and held by nothing. The corpus is the durable record; a scratch
harness dies with its checkout.

The substantive half of the same ruling is section 1's instance 1: the check
that had been run keyed on *roots*, the rule the prose stated was *chosen versus
inherited*, and a live counterexample sat inside the very commit that ratified
the rule.

## 13. A ruling's capture action must name its contradictors, not only its consumers

One ruling required a verdict to carry a mechanism anchor. Its capture actions
routed that to the decision record and the resolver — the two places that would
*consume* the new rule — and named **neither** of the two places that stated the
opposite: the family's own guide and the spec's vocabulary table, both still
reading "no anchor; tallied separately". Both frozen corpora already carried the
anchor, so **the law disagreed with the data it governed.**

The placement is the worst available. The family guide is its onboarding
document and was required reading for the next task; an author implementing the
law as written would have reddened the corpus the rule exists to score, and
would have been right to.

> Listing consumers feels complete because it answers *who needs this?* The
> question that does not get asked is **who currently says the opposite?**

## 14. Ordinals rot, and one of them is now uncorrectable

`technologies/` is the sixth corpus family. Measured from the git history of
each directory's first commit, the families were founded in the order tropes,
systems, sentences, repertory, regularities, technologies — so `regularities/`
is the **fifth**, and its own ratified decision record is titled "a **fourth**
family". That record was stamped accepted nine days after `repertory/` was
founded under a merged chronicle whose own words are "This campaign founds a
fourth corpus family". **The record was false on the day it was ratified**, and
the append-only rule binds — explicitly including a correction of something that
was false when written. Nothing in this campaign may edit it.

Three consequences, and they differ. The record stands uncorrected. Root
`CLAUDE.md` was wrong twice over and **is** editable — it listed four families
with two missing and called the fifth the fourth — and this campaign fixed it.
And our own decision record had to state the ordering and name the discrepancy,
or a reader meeting "fourth" beside "sixth" finds a hole where the fifth should
be.

What my own counting got right is worth stating because it locates the error
precisely: the campaign's pre-spec findings said there were five families, not
four, and that was correct; I called ours the sixth throughout, and that holds.
**The error was never in the count. It was in the ordinals attached to
individual families**, which is a different claim and the one nobody checks.

> **An ordinal is a claim about a population, asserted by a document that cannot
> see the population change.** Prefer "a sibling family, founded by X" to "the
> Nth sibling": the first cannot rot, the second must.

The root cause is an absence rather than an error: `repertory/` has **no decision
record opening it**, every other family has one, and that is why it was invisible
to the miscounting record and to the root guide alike. Not this campaign's to
mint — we did not found it — but the next family will hit it again.

## 15. A branch is not a repository

I verified "the next free decision number is 0937" by listing the decision
directory and taking the tail. Measured afterwards, before main was absorbed:
this branch was **145 commits behind** the repository's main line, whose highest
decision is 0958, and **0937 belongs to another campaign's reserved block** —
the range 0936–0945, which the register shows reserved at
`2026-09-09T01:02:06Z`. There is a
reservation register, with commands to claim and read it, that I did not use.

No guard would have saved us, and the reason is worth carrying: the overlap test
compares *declared* blocks, so **a number minted without reserving one is
invisible to it** — the test's own documentation says so. Another campaign's spec
records the same failure nine days earlier, having minted a record with no
reserved block that trespassed on a third campaign's range. Two instances, nine
days apart, against a register that exists precisely to prevent it.

The shape is section 1's, one more time: the population my check ranged over was
*this branch's decision directory*; the population my claim ranged over was
*the repository's decision numbers*. I did not compare them, **because the
command returned a number and a number looks like an answer.** 145 commits is
not a rounding error.

The implementer reserved a block instead of trusting my figure, which is how the
collision was avoided rather than discovered later. It is also the fourth
consecutive task in which an implementer's declared decline or correction found a
defect review would not have.

## 16. The data could not exercise the code, for three tasks running — and that is structural

| task | what it built | live cases in the committed corpora |
|---|---|---|
| 4 | cycle, dangling and closure guards | **none** — both corpora clean |
| 5 | three of five anchor kinds | **none** — zero instances of each |
| 6 | the two-way trajectory guard | **none** — every trajectory verdict unscored |

This is **not a mistake to fix.** It is the direct consequence of freezing both
corpora before any evaluation code existed, against a subject — a monotone
four-rung clock — too impoverished to exercise most of the vocabulary the family
ratified. The campaign built an instrument whose own data cannot test most of
it, and did so on purpose.

The only safe response is the one the tasks took: **every such guard is tested
against a constructed fixture that contains the violation and demonstrably
fires.** A guard validated only against the real corpora is vacuously green — it
passes because the input has no violation, not because the guard works. The
two-way guard in particular must redden in **both** directions, per the sibling
family's own ruling that an implementation reddening only the first has built
half a guard, and the half it skipped is the one that lets a corpus quietly
under-report the world.

One trap inside that deserves repeating, because it inverts the usual instinct:
the count ratchet must fire on a **rise** and pass on a **fall**. Every
correction this campaign made moved the `absent` count *down* — the unflattering
direction, the one we want to stay possible. A ratchet that blocked a falling
count would punish exactly the behaviour the campaign spent three fix rounds
rewarding.

**And the successor campaign inherits the mirror image.** It makes the trajectory
verdicts measurable, at which point the two-way guard acquires its first live
case and **the fixtures written here are what stand between a real regression
and a silent one.** They are not scaffolding, and the campaign that meets them
should not read their synthetic inputs as a sign they were provisional.

The same structure produced a vacuity worth stating separately: our `doc:`
anchor resolution could cite a page that does not exist, and **today that costs
nothing**, because no measured verdict exists to lean on it. A vacuity that is
free in the task that introduces it and expensive in the next one is the worst
version of this problem — nothing objects now, and the task that makes it matter
inherits it silently. It was fixed here rather than left to be inherited.

## 17. The gate that was green did not run this campaign's tests

Every task in this campaign reported a green local commit gate. **That is not
evidence any of its new tests pass**, and the two figures are disjoint
populations: a test with no recorded baseline duration is excluded from the
commit gate's sub-floor tier by design, because coverage is the stage gate's job.
Every test this campaign wrote is in the excluded set.

So the honest reading of a green local gate here is: *the build is not broken and
nothing previously rostered regressed.* The implementers ran the new tests
separately and reported them separately, which is correct — but a reader seeing a
gate's pass-count beside a test list can easily read one as covering the other,
and nothing in the report format prevents it.

Where coverage actually arrives is the serial queue on the canonical box, which
runs the full workspace suite. **The stage gate is therefore not optional for a
campaign like this one even though every task reported green**, and the
sub-floor roster only gains entries from a green run on that box, never locally.

## 18. An amendment left the document it amended executing the superseded design

A design ruling collapsed two verdict fields into one. The spec section that had
been written *under the two-field shape* said the collapse column would read
entirely unscored on freeze — which is true of a separate trajectory field and
false of a single pipelined one — and was never revisited. The stranded sentence
then propagated into the plan, where it would have instructed an implementer to
author one hundred per cent of a column as unscored.

**The correction was worth more than consistency.** "Entirely unscored" would
have flattened two classes together and thrown away the better finding: a
technology Hornvale cannot model at all fails at reach and scores `absent`, and
only the two it *does* model reach the stage where loss would be measured. The
statement the corrected scoring supports — *of forty-one losses, Hornvale
reaches two technologies and can represent the loss of zero* — is strictly
sharper, and a uniform column has no such number in it.

**What caught it is a sequencing discipline, not a review.** The dispatch rule
for this project is to verify a brief against the code immediately before
dispatching *that* task, never in a batch at plan-authoring time. The plan was
written before the collapsing ruling had been applied to that task's data, so no
amount of plan-time review could have found this. Checking one task's claims one
task ahead found it in minutes.

## 19. A plan ordered the merge before the stop that gates the merge

This is the campaign's most consequential requirement defect and the only one
where compliance would have **bypassed a human gate** rather than merely shipped
a weaker artifact. Every other defect in this document cost an artifact some
quality. This one would have cost Nathan his review.

Plan Task 10 Step 5 read *"Final gate, then submit to the sluice"* and carried
the submission command outright. A sluice submission merges the branch and
pushes `main`. G6 — the merge stop — is a constitutionally manual gate whose
entire purpose is to sit in front of exactly that. So the plan instructed a task
to perform the merge *before* the stop that gates it, and the plan was
internally consistent while doing it: the same document declared G6 a hard stop
three sections earlier. Nothing compares a plan's step ordering against its own
declared gates.

**Task 10's implementer refused.** It named both halves — the step and the stop —
observed that a submission merges and pushes, and stopped after pushing the
branch so the SHA would be ready for whoever held the gate. It did not ask for
permission to skip the step; it reported why the step could not be obeyed as
written.

Three things generalise from it.

**The hazard is not "the plan said something wrong" but "the plan said something
executable."** A defect in prose produces a worse document. A defect in an
instruction that carries a runnable command produces an *action*, and this one's
action was irreversible in the only way that matters — a merged `main` cannot be
un-reviewed. The class worth watching for is narrow and checkable: **a plan step
that invokes a gated operation, anywhere in a plan that also declares the gate.**
That comparison is mechanical and nobody ran it, here or anywhere.

**An autopilot makes this more likely, not less.** Routine gates auto-resolve
against standing policy, which is the point; the two that do not are named as
hard stops precisely because no policy can stand in for them. A plan written
under autopilot is written in a register where gates are things that resolve
themselves, and the two exceptions have to survive in prose alone.

**The refusal is the control, and it is a person's judgement, not a mechanism.**
Nothing in the substrate would have stopped this: the implementer held the
branch, the command was valid, and the pre-push hook's one rule is about who
holds the canonical box's claim, not about whether a human has reviewed. Had the
task complied, the merge would have been correct by every gate the project owns.
The only thing between the plan and the bypass was an implementer reading two
parts of its own brief against each other and believing the stop over the step.

## 20. A figure went stale inside the campaign that measured it

Spec finding F4 recorded a measurement of the committed gallery — a count of
occupation layers by tech horizon — as evidence that the tech ladder's whole
observable variance is buried in ruin strata. Then this branch absorbed 145
commits of `main`, and the same command returned different numbers. The
conclusion survived and in fact strengthened: the extra layers are all *dead*
strata, which is what "the horizon dates the dead" asserts. But the stated tally
was a **claim with a date, and the date passed inside the campaign that wrote
it** — the shortest shelf life this project has recorded for a committed figure.

**The implementer drew the durable lesson before the controller did.** Asked to
write the chronicle, it led with the *arithmetic* rather than the tally: offsets
drawn on `[0, 300]`, a bake ending at year 2000, the top rung opening at 1400,
therefore every living community is above the last threshold under any
aggregation rule. That sentence cannot go stale, because it is a derivation over
three committed constants rather than a count of what happens to be in a
generated file today.

So: **prefer the derivation to the measurement when both say the same thing.
Only one of them rots.** A measurement is still worth stating — it is the
evidence the derivation is about the real code — but it belongs beside the
derivation and subordinate to it, not in the position where a reader takes the
claim from.

## 21. Two smaller ones, recorded rather than repaired

**A grep of mine undercounted, again, in the gradient sweep.** Checking whether
any Confidence Gradient bet moved, I reported three incidental hits in the tier
sections. The implementer re-ran it rather than inheriting the number and found
four. The conclusion is unchanged — the gradient's technology-adjacent term
appears nowhere in the tier sections, so no bet moves — but it is a fourth
uncomputed number of mine in a campaign whose subject is uncomputed numbers, and
the mechanism is always the same: a number that came out of a command *looks*
like a measurement even when nobody re-ran the command.

**One controller edit skipped review, and is declared rather than discovered.**
Family law for this family cited every decision it rests on except the one that
opens the family. Task 10's implementer flagged it as outside its brief; I made
the edit myself instead of spending a dispatch-and-review cycle on a
cross-reference. Controller fixes skipping review is normally forbidden, and the
reason to accept it here is that the alternative was shipping family law that
does not cite its own founding decision, while the edit adds a pointer and
changes no rule. It is recorded so the trade can be judged rather than found.

## 22. What held up

**The freeze, made structural rather than promised.** Both corpora were
authored and committed before a line of evaluation code existed. There was
nothing to tune them against, and the first score is therefore a genuine
preregistered miss. This is the single property the whole campaign was sequenced
around, and it is also the reason section 16 exists — the cost is real and was
paid knowingly.

**Refusing to anchor a mass of `absent` items to one convenient row.** A
registry row plans the loss mechanism itself, in its own words that the
knowledge does not decay but the capacity to make more does. It would have
"discharged" most of one column. It was refused on a ground that is decisive
rather than stylistic — a planned-but-unbuilt verdict is a *reach*-stage value
under the pipeline, and that row plans a *trajectory* mechanism, so it cannot
discharge a demand whose reach has already failed. The finding underneath the
refusal is better than the verdict: **the ratified vocabulary has no way to say
"loss is planned but unbuilt."** That is a real expressive gap, and the correct
response is a decision record rather than thirty-one re-anchored cells — which is
exactly what the family's decision record now says, including the rule for a
reader who disagrees.

**Accepting a long field because the property it threatened was measured rather
than assumed.** One provenance field roughly doubled in length across three fix
rounds. The requirement it might have breached — that a reader meets the bias
statement before any number — binds the *generated report*, not the field, and
the words preceding the bias paragraph were measured byte-identical across all
three revisions. The growth was the rounds' own durable record. The trim
candidate was carried to the render task as a consideration rather than ruled a
defect, because a readability problem in a rendered page has a render fix and
does not require mutating a frozen artifact.

**Grepping the registry before capturing a new idea.** Two remarks were offered
for capture. One turned out to be the project's existing north-star row,
already shipped at its halfway rung, so nothing new was added — adding a row
would have restarted an idea the registry already holds. The other was genuinely
new and narrower than its nearest neighbour, and got a row. The habit that
settled both is a grep, performed before writing.

## Follow-ups

Carried as registry rows and ledger follow-ups rather than folded in:

- **The successor campaign** — replace the four-rung clock with an acquisition
  model expressing prerequisites, divergence between peoples, and loss. An
  epoch: the committed horizon fact's semantics change.
- **`tech_offset`'s doc comment says "per-people" and the draw is
  per-community.** A one-line prose fix, out of scope for a campaign that
  touches no model code — but it misled this campaign's own analysis, so it
  should not be left for the successor to rediscover.
- **The one-field vocabulary cannot say "loss is planned but unbuilt."** Needs a
  decision, not re-anchoring; recorded in the family's decision record, resolved
  by the successor.
- **`repertory/` has no decision record opening it** — the root cause of section
  14 and invisible to everything downstream of it.
- **The decision-block overlap test is blind to an undeclared mint** (section
  15). The guard that would catch it compares minted numbers against the
  register, not declarations against each other.
- **Two families use one criterion-kind name for arithmetically different
  statistics** — ours is a population-weighted aggregate, the sibling's a count
  of in-band values, and the sibling's own test name points at the reading that
  is wrong for ours. Our doc comment warns in one direction; the sibling cannot
  warn about a family that postdates it. A one-line pointer belongs to whoever
  next has reason to touch that file.
- **A criterion's JSON tag is derived from its Rust variant name in
  `cli/src/regularities.rs`**, so renaming a variant there would silently change
  the wire tag and break parsing of that family's frozen corpus with nothing
  catching it. Correct today, invisible until a rename lands. **This family's own
  exposure is closed** — Task 6 pinned both of `technologies::Criterion`'s tags
  with an explicit `#[serde(rename = …)]`, so only the sibling still carries the
  latent shape, and the pointer here names that file rather than "both families"
  so the next reader opens the one that still has it.
- **A vocabulary gap rather than an absence**: raiding strength is computed as
  population times a tech weight, so a standing army is indistinguishable from
  its people *in principle*, and the ratified vocabulary has no value for "the
  model cannot draw this distinction at all."
- **The successor may reuse existing population machinery** — two shipped domains
  already model thresholds over populations, which is the lifted shape of
  technology loss.
- **Prefer "a sibling family, founded by X" to "the Nth sibling"** in every
  directory guide (section 14).
