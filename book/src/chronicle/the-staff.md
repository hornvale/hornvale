# The Staff

On a single-track line, only the train holding the *staff* may enter the
section. There is one lefford — the one machine every seeded world is
required to agree with — and this campaign gives it one staff. That turned
out to be the smaller of its two findings. The larger one is that most of the
traffic queuing for the section should never have left the station.

## A commit was paying for a merger

Every commit in this project ran the same instrument: fifteen minutes, give
or take, of formatting checks, lints, a type audit, and the entire test
suite — thousands of tests, most of them nowhere near the lines a commit had
actually touched. Summed over a month, that instrument ran four hundred and
seventeen times at an average of a little over seven minutes, which is
forty-nine hours of one machine's attention. The same four hundred and
seventeen calls against the cheap half alone — formatting and lints, no
tests — would have cost under two.

A faster variant already existed, restricted to the crates a commit had
touched rather than the whole tree. Measured rather than assumed, it saved
about ten per cent. The reason is structural, not a tuning failure: selecting
fewer *tests* does not select fewer *binaries*, and a modern compiler builds
every test binary that might contain a selected test regardless of how many
of its tests get run. The variant was scoping the wrong half of the cost.

So the question was reframed. Not "how do we make the existing gate faster"
but "what is a commit actually asking to be checked, and what does an
integration decision need that a single commit does not." Those are different
questions with different honest prices, and pricing them the same way had
been quietly taxing every commit for the second question's answer.

## Three gates, one for each moment

The answer split the one instrument into three, each named for the moment in
a body of work it protects rather than for the machine it happens to run on:

```
  the commit gate     local, seconds        every commit
  the stage gate      queued, minutes       each planning checkpoint
  the campaign gate    queued, tens of minutes   before anything merges
```

The commit gate keeps only what a single change can honestly verify in
seconds: formatting, lints, the type audit, and a *sub-floor* slice of the
test suite — every test whose last recorded duration sits under one second.
A test that has never been timed is excluded rather than guessed at; it
enters the sub-floor slice when the stage gate next measures it.

That last sentence was false when this chapter first carried it, and the
correction belongs here rather than in a footnote. The returning half of the
loop did not exist. The stage gate did re-measure every test and did rewrite
the roster — into scratch space that the next job erases before anyone can
commit it. Nothing ever reached the committed file; the roster's whole
history was two commits, both written by hand. One crate's tests consequently
sat outside the commit gate from the day it merged, while every run reported
green. [The Ballast](./the-ballast.md) built the missing half: a green run's
roster is copied somewhere durable, and a human brings it back
deliberately. Coverage does grow by
measurement rather than hope — but only once someone carries the measurement
home, and an allow-list cannot notice that its own list has gone short.

That correction was itself incomplete, and [The Sluice](./the-sluice.md)
supplies the rest: the carrying-home never happened either, because nothing
was ever produced to carry. The producer refuses while the machine's
exclusive claim is held, and every serialised path runs it beneath the very
process holding that claim. The roster is an ordinary regenerated artifact
now, written and committed inside the merge that was tested, with no rescue
and no human step in the middle.

Everything
costing minutes or more — the full suite, the artifact regeneration checks,
the tests nobody had wired up, and the two verification tiers reserved for
whole-world evidence — moved to the other two gates, which run on the one
machine those checks were always meant to run on.

## What was actually being bought

The instrument this replaced had one number attached to it: fifteen minutes.
A single number invites a single explanation, and the obvious one — the
tests themselves are slow — turned out to be almost entirely wrong. Timed on
a quiet machine, the sub-floor slice's own tests, several thousand of them,
run to completion in four and a half seconds. The other minutes were spent
compiling and linking the binaries those tests live in, and that cost cannot
be selected away, because a change anywhere still requires rebuilding
everything downstream of it.

What the cost tracks instead is *where* a change lands in the project's own
layering — the substrate at the bottom, the domains above it, the presenting
surface on top. Timed end to end, one line changed and reverted at each
level: a change to the substrate costs the same eight minutes the old,
undifferentiated gate cost; a change to a domain costs a minute and a half;
a change to the presenting surface at the top, where nothing else depends on
it, costs seventeen seconds. The commit gate is not a flat seconds-scale
instrument so much as an honest reflection of how much of the project a given
change could plausibly have disturbed — which is the number a commit gate
should have been reporting all along, and never was, because the old
instrument reported the same fifteen minutes regardless of what had changed.

An earlier estimate of the sub-floor slice's own cost, made before it was
built, guessed about fourteen seconds. Measured, it cost a hundred and
seventy-nine — twelve and a half times the guess, on a machine already warm.
The guess was reasonable and it was still off by an order of magnitude,
which is the specific, recurring shape this project keeps re-learning: an
estimate is a claim about a cost, and a cost is answered by running the
thing, not by reasoning about it.

## A queue that had already been built

The two slower gates needed to run one at a time on lefford, alongside the
whole-world verification runs that already claimed the machine for
themselves. The original design for that added a small dedicated component
to hand out turns in order — a ticket window, in effect, sitting in front of
the existing lock.

Before building it, the existing lock was tested for the property the new
component was meant to supply: does it already hand out turns strictly in
the order they arrived? Six requests, spaced apart, then eight requests,
arriving at once — both trials returned the lock to every waiter in exactly
the order it had been asked for. The primitive already there was already a
queue; it had simply never been asked to prove it. The ticket window was
deleted from the design before a line of it was written.

## Three suites that had never run anywhere

Building the machinery that could dispatch a check to a chosen moment
surfaced a smaller, sharper finding: three whole bodies of tests existed in
this project and had never once been run by anything. One of them — the
tests behind the project's own internal fact ledger — had not compiled for
three days. A change elsewhere had widened one value's type from a bare
number to a properly labelled one, exactly as this project's own save-format
discipline requires, and the one place that construction still assumed the
old shape sat outside every gate that would have noticed. Nothing was
watching it, so nothing had.

A second suite, behind the project's browser-facing catalog builder, had
never had a gate of any kind — a documented, acknowledged hole rather than an
oversight. Building its check turned up a check of its own that could not
have caught anything: the first version proved itself by inserting a
comment into the source and confirming the built output changed, and the
tool that builds that output strips comments before anyone can compare them.
The proof was rewritten to change something the builder cannot discard.

The third — the tests behind the project's own coordination log, on the
order of two hundred of them — simply belonged to no gate at all, workspace
or otherwise, because the file that holds them was never declared to be
inside the workspace to begin with. All three now run at the earliest queued
checkpoint, alongside the audit that has caught more of this project's
own drift than any other single check: the audit that watches for functions
no test would notice being quietly broken. It costs the great majority of
that checkpoint's own time by itself and was given a slower moment of its
own to sit in, rather than share one with three suites that finish in
seconds.

## What it measures

Four hundred and seventeen commits at the old rate would have cost this
project's own machine forty-nine hours a month. The same shape of commit,
against the new gate, costs on the order of four and a half — while running
several thousand tests on every one of them, where the old comparison figure
assumed running none. The two numbers are not measuring the same promise:
one counts hours saved against doing nothing, the other against doing
everything, and both are true at once.

Read by layer, one line changed and reverted at a time: the presenting
surface, seventeen seconds; a domain, a minute and a half; the substrate,
eight minutes — the same price the single old gate always charged, now paid
only by the changes that could actually have moved it. A quiet build with no
change at all costs under twenty seconds.

The full suite, run uncontended on lefford at the size it has actually grown
to, passed three thousand five hundred and sixty-one tests in just over four
minutes. And the project's own record of what the suite costs — a file kept
per machine, rewritten only after a clean run — turned out to have frozen a
year's worth of commits earlier than anyone had noticed, at a moment when the
suite was roughly a quarter smaller than it is now. Restarting it surfaced
three individual tests running two to four and a half times slower than the
frozen figure remembered, with no shift in the suite as a whole — drift
accumulated one commit at a time, invisible for exactly as long as nobody was
running the instrument that would have caught it.

## What it leaves reserved

A change to the project's own substrate still costs as much on a single
machine as the old, undifferentiated gate always did — that is a measured
property of how much of the tree depends on it, not a residue this campaign
left unfinished, and several changes to it landing in the same narrow window
can still compete for the same ten processor cores the way they always
could. Nothing above the commit gate runs anywhere but the one queue on
lefford, by design: if that machine cannot be reached, no gate above the
fastest one can run at all, deliberately, rather than falling back to a
comparison the project could no longer trust.

And the coordination log this campaign wired into every session's own
startup now refreshes automatically once a session begins — but it renders
before the refresh has a chance to land, so what a session reads at its very
first moment is, precisely, what the *previous* session last saw. Current
enough to act on for almost every purpose; not current enough to trust with
a warning that another session has just asked everyone else to wait.
