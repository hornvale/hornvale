# The Sluice

A gate inspects a thing and lets it through unchanged. A lock — the canal
kind — does something else: it puts the vessel and the water in one chamber
and tests the two together, because the thing that has to be safe is neither
of them alone. Every check this project had ever run was a gate. This
campaign built the lock.

## Two guarantees that never met

The old arrangement had two halves, and each of them was honest about
something.

The first was the campaign gate: take a branch, run the full battery against
it, report green. What that proves is a property of the branch's own tip —
the exact tree the branch's author last wrote, checked out and reset to
exactly that state by construction.

The second was the drift check on merged reality: run the everyday
instruments against the trunk and notice when something has gone red there.
What that proves is a property of the trunk, after the fact.

Neither implies the other, and this is not a matter of degree. What lands on
the trunk is the branch *merged into whatever the trunk is at the moment of
merging* — an object neither half ever constructs. The branch was green
about a tree that no longer exists by the time it merges; the trunk check is
red about a tree nobody chose to build. The weakest statement that would
imply both is *the merge product was tested*, and that statement had never
been true here, not once, in the entire history of the project.

The gap is not theoretical, and it does not require anything to go wrong on
either side. Both halves can be working perfectly and the gap stays open,
because the object it is about is never built.

## The worked example: two campaigns, one number

The project keeps a numbered log of settled engineering choices. A number is
a permanent citation handle: prose, chapters, tests and contributor guidance
all refer to a decision as a bare four-digit token, so a number is supposed
to name exactly one ratified choice forever.

Two campaigns, running in parallel, each took the next free number against
the trunk they had branched from. Both got the same one. Every mechanism
that would normally catch a collision let it through, and each for a
different structural reason:

The version-control system raised nothing, because the two records are
different *files* — the numbers agree and the descriptive part of each
filename does not — so the merge simply kept both, with nothing to resolve.
The generated index of decisions in force renders one line per file, so the
duplicate appeared as an ordinary extra bullet in a sorted, plausible list.
And the check that watches for holes in the numbering could not see it,
because a duplicate creates no hole; it names a collision as a *cause* of
gaps in its own explanatory comment, and is blind to the collision itself.

Both branches were green throughout. The merge that produced the defect was
the one thing nobody tested.

## It happened again, to this campaign, and looking is what caught it

Late in the work, this campaign went to ratify a decision of its own. The
trunk's highest number was one hundred and thirty-six, so one hundred and
thirty-seven read as free. It was not: another live campaign held both one
hundred and thirty-seven and one hundred and thirty-eight on a branch that
had not merged yet.

Avoiding that would ordinarily be the end of the story, but skipping the two
claimed numbers exposed something worse than a near miss. The project holds
two invariants about that log at once. One says *no two records may share a
number*, which under parallel work means skipping any number claimed on a
branch you can see. The other says *the numbering has no gaps*, which means
taking the next contiguous one. Under parallel campaigns those two rules are
mutually exclusive: the no-gaps rule actively pushes you into the collision
that the uniqueness rule exists to catch, and the second campaign to arrive
does the locally correct thing and produces the defect.

That is almost certainly how the original collision was minted. Nobody was
careless.

The durable answer is not a better rule for authors. It is that a number
wants **assigning at the moment of merging**, by the thing doing the
merging, which is the only vantage from which "free" is a well-defined word.
A serial merge queue is the only mechanism this project has ever had that
could do it. It has not been built; the campaign resolved its own instance
by merging the other branch first, through the queue, so that the contiguous
number became genuinely free.

## What the lock is

Two parts, and the split is by what each one costs to be wrong about.

The **mouth** is cheap and refuses early. Before anything takes the machine,
it asks whether this candidate can even be merged into the trunk as the
trunk currently stands, by constructing the merge in memory and looking. A
candidate that cannot is bounced in milliseconds and the queue advances past
it — because a branch that merely needs to absorb the trunk should not
freeze a serial resource the way a genuine failure does. It also checks that
the trunk is still exactly where the queue last left it; anything that
landed out of band breaks the inductive guarantee, and the queue says so
loudly rather than quietly resuming with a weaker promise than it
advertises.

The **chamber** is expensive and runs one candidate at a time, holding the
single claim that already serialised every costly job on that machine. It
builds the real merge, then runs each battery in turn against it: artifact
regeneration, the browser-facing checks, the full test suite, the client
suites, the mutation audit, and the whole-world tier last. Between phases it
returns the tree to a known state, and any legitimate regenerated output a
phase produced is committed *inside* the chamber, so the tree the next phase
sees is clean and the artifacts that landed are the ones the checks saw.

And then the property the whole design exists for: **the tested object is
the pushed object.** Nothing is created after the last green result. The
merge that was gated is the merge that lands.

Behind that sits a policy rather than a mechanism: the queue stops on red,
and the campaign behind it waits. That is head-of-line blocking, adopted
deliberately at a measured rate of three and a half merges a day, which
leaves room for an hour of diagnosis and not four.

## The first thing it did was refuse

Its first real candidate was a four-hundred-and-forty-file epoch from
another campaign — a change to the world's climate large enough to move the
mean land temperature by six and a half degrees.

The first run held. Three phases passed; the mutation audit returned a
survivor, the chamber refused to push, and the trunk did not move. The
distinction between *fails closed by design* and *has failed closed in
anger* is one this campaign had drawn out loud, to that campaign's own
session, hours before it could claim the second half. Now it can.

The second run went green across five phases in forty-four minutes and
pushed the exact tree it had tested.

## Four things only a merge could have found

**It held an innocent candidate.** The survivor the audit found was a
function whose output reaches the ledger and whose call site the candidate
had never touched — it was already there, on the trunk, before the candidate
existed. The queue was right that the merge product was defective and wrong
about whose fault it was, and it has no way to tell the difference. The
technique that would settle it is one this project already uses for
contention: re-run the failing check against the trunk alone, and exonerate
the candidate when the finding reproduces. The queue holds both trees, so it
is the only thing here positioned to do that automatically. It does not yet.

The finding itself was real and is now closed. The function converts a
founding *year* into a ledger *day*; the existing tests around it asserted
ordering — that a person is born before they die, and after their community
was founded — and a unit conversion scales every term by the same factor, so
ordering is structurally blind to a units error. What discriminates is a
cross-check: a founder's own recorded founding day must equal the day
already committed for the community they founded, since both crossings start
from the same year and pass through the same function. That is an invariant
a reader would want anyway, which is the test for whether an assertion
written to close a survivor is a real assertion or a mutation-shaped one.

**The audit produced a verdict inside a full campaign-shaped run for the
first time in this project's history.** Five of its first six queued runs
had died on a dirty tree left behind by whatever ran before them; the two
that succeeded were solitary. Here it returned seven guarded call sites and
one survivor — because the chamber returns the tree to a known state between
phases, which is a small piece of hygiene earning its keep on real work.

**Two of the four whole-world artifacts cannot go red.** The tier that
authors them is an authoring path, not merely an expensive one. Two of the
four tests compare what they build against a committed copy and fail when
they differ. The other two only *write*. They assert nothing about what they
wrote, so their artifacts went stale through a run that reported eighty
passes out of eighty. The general form is worth stating plainly: **a test
that authors an artifact must also assert it, or the drift is invisible by
construction** — and invisible in the most misleading way available, behind
a green number.

**The census's epoch label is wrong, and pinning the merge subject was not
enough.** The generated history of the world-survey reads the subject line
of the change that last touched each survey column, walking first parents.
This campaign made the merge subject a required, campaign-authored headline
for exactly that reason. But every phase commits its own regenerated output,
so the merge sits several commits below the chamber's final state, and the
label the history picks up is a housekeeping line. Pinning the *shape* of
the merge subject is necessary and it is not sufficient; what matters is
which commit the reader lands on.

## The list that never once moved

The everyday commit check runs a named list of tests — those measured fast
enough to belong in an instrument that must finish in seconds. The
documented remedy for a test missing from that list was that the slower
gate, which times everything, rewrites the list on every green run.

That list has exactly one entry in its entire recorded history, written by
hand.

Two explanations for this have been published, and both were downstream of
the cause. The first said the rewrite landed in scratch space that the next
job erased. The second, from the campaign that went looking, built a rescue:
copy a green run's list somewhere durable and have a person carry it back.
The rescue was conditioned on the file having changed, and it never had.

The real blocker was a step earlier than either. The command that writes the
list refuses to write it whenever the machine's exclusive claim is held —
sound in intent, since a contended run's timings would poison the baseline.
But every serialised path in this project runs that command *as a descendant
of the process holding the claim*. So the one thing that produces the list
declined to produce it, in the one environment on the one machine where
nothing else was running at all, and every mechanism downstream faithfully
carried an unchanged file. One component's tests had consequently never been
in the everyday check at all, while every run reported green about
everything it was looking at.

The fix is to ask the question that was actually meant. Not *is the machine
claimed?* but *am I contending with whoever claimed it?* A claim held by
one's own ancestor is the job one is part of — the most serialised moment
available, not contention. A claim held by anyone else still refuses,
unchanged. The chamber then commits the rewritten list like any other
regenerated output, and it lands with the merge product, on the tree that
was tested. The rescue and the carrying-back are gone.

## A tripwire's first firing was a true positive

The world-survey is expensive and sits outside the queue's guarantee
deliberately, so a merge can land with a survey that predates it. What
watches that gap is a fifteen-minute ceiling read against the last recorded
run.

Its first firing was real. The four-hundred-and-forty-file epoch had made
the survey twelve per cent more expensive, and the processor-utilisation
figure beside it was unchanged — thirty-two point one against thirty-two
point three — which distinguishes genuine work from a crowded machine. So
the alarm was not about the machine.

Attributing it took a probe rather than an argument. About a fifth of the
added cost is building the worlds themselves, which is the epoch's physics:
a warmer world puts more land above freezing, so terrain, settlement and
history all genuinely do more. The other four-fifths is measurement, and
inside that, two columns carry ninety-three per cent of the increase. One of
them responded *sub*-linearly to a genuinely larger input and has no
pathology to remove.

The other is roughly half of the entire regression. Its code did not change
at all. It walks a lineage tree, and its inner operation rebuilds each
node's ancestry from scratch — so its cost is quadratic in the size of that
tree. The tree grew by seventeen per cent, and the bill grew by
forty-four. Reasoning from the number of *living* settlements would have
badly understated the input, because the lineage includes the extinct: that
count grew three times faster than the living one.

The epoch did not make that measurement do more meaningful work. It made an
inefficient algorithm's inefficiency visible. And the note in the code
justifying the column's cost, written when it was registered, understates
the measured figure by roughly eighteen times — a claim nothing had ever
re-checked, surfaced only because an unrelated alarm went off next to it.

## What it removed, and what it did not

The campaign began with a commitment that it would end with less process
machinery than it started with, and that commitment was wrong — corrected
mid-flight, by the person who had asked for it. More code does not mean less
simple. The number that actually describes what changed is not lines; it is
how many separate instruments a person has to hold in their head to land a
change.

By that measure: the commands in the landing path went from twelve to five,
and on the write side from seven to three. A single checkpoint used to be
four separate dispatches to the far machine, four job identifiers and four
logs to read; it is one. A request had no durable existence until the job it
became had already finished, and a job killed partway left no record that it
had ever run; now the record is written at the moment of asking. The number
of machines a campaign reasons about went from two to one.

Nine hundred and fifty lines of dispatch machinery were deleted, and the
total line count still went up by rather more than that, most of it test
harness and commentary carrying four rounds of review reasoning. Both
numbers are true and only one of them is the point. What went away was an
absent caller: work handed to a machine nobody was watching, on the
assumption that a system can be its own operator. Every defect the dispatch
layer produced in its two days of life came from that assumption — a claim
file nothing could read, a killed job that released the lock while
thirty-nine cores kept working, an audit refused five times out of six on a
tree an earlier job had dirtied, and a list that never once moved a byte.

The exclusive claim itself was never the problem, and it was kept. What
replaced the dispatch layer is a person resident on the machine, running
each phase in the foreground. The word for what this campaign was
originally hoping to prove is *fewer lines*; the word for what it actually
changed is *one session managing one machine*, instead of many sessions
coordinating another one.

## What it leaves open

The queue holds on red and cannot yet tell a candidate's defect from an
inherited one, which is the first thing it should learn to do. The
fifteen-minute ceiling on the world-survey sits inside that instrument's own
run-to-run spread, so it will fire on ordinary variance and the resulting
red will be blamed on whatever physics changed most recently — the obvious
suspect and the wrong one. And the rule that a repair belongs on the
candidate's own branch rather than folded into the merge commit is an
operator's rule, enforced by review and by nothing else: mechanising it
would mean the chamber inspecting the provenance of its own commits, which
is more machinery than the risk currently warrants and is a known hole
rather than an implicit one.
