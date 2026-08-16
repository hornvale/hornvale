# The Ballast

Ballast is weight carried for stability rather than for cargo. This campaign
carried none of its own: it existed to settle what the previous one had left
unsettled, and its whole subject was a set of failures that were already
sitting in plain sight.

[The Staff](./the-staff.md) had just replaced one gate with three, and in
doing so it ran the full suite on a clean checkout of merged reality for the
first time in a while. Four things were red. None of them had been broken by
that campaign; all four had been red for some time, unobserved, because the
instrument that would have shown them ran too rarely to be pointed at
anything.

The interesting property of the four was not that they were red. It was that
they were red for four entirely different reasons, and that in every single
case the first, most plausible explanation turned out to be wrong.

## Four reds, four kinds of thing

The first was a set of message-passing tests that failed on one machine and
passed on another. The obvious reading was a concurrency defect — tests
interfering when run in parallel. That reading was mine, and it was wrong; I
had compared a serial run on one machine against a parallel run on another
and attributed the difference to parallelism, which was the one variable I
had not held fixed. The actual cause was that three tests named a specific
machine in their assertions, and a function that excludes the current host
returns different results depending on which host is current. A test that
hardcodes the name of the world it runs in is not testing anything portable.

The second was a calibration measurement whose primary claim had been
falsified months earlier by an entirely different campaign, which had
measured the falsification, accepted it, changed nothing, and left the
assertion standing as a deliberate marker. It was not a bug and it did not
need fixing. What it needed was *provenance* — a record saying that a
statistic which its own mechanism refutes is retired rather than rescued,
so the next reader does not spend a day rediscovering that the red is
intentional.

The third was a genuine gap: a function whose output reached rendered
artifacts while no assertion held its contribution. Neutralise it and the
world renders differently and the suite stays green. That one was simply
fixed.

The fourth was a cost ceiling that failed under load and passed at rest. The
tempting fix — the ceiling was calibrated on a different machine, so
recalibrate it — was measurable, and measuring it refuted it: run alone on
the machine where it failed, on an otherwise idle system, it passed
comfortably. It was not measuring its own subject at all. It was measuring
the machine, from inside a tier that saturates the machine. The correct
response was not a new number but a scheduling constraint, and the repository
already contained the precedent, the argument, and the measurement for
exactly that constraint, applied to a neighbouring test for the same reason.

## What an allow-list cannot see

The campaign's most consequential finding was not among the four. The new
commit gate runs a named list of tests — those measured fast enough to belong
in a gate that must finish in seconds. Tests not on the list are excluded by
design, and the documented remedy was that a test enters the list the next
time the slower gate measures it.

That remedy had never once worked, and could not. The slower gate did
re-measure everything and did rewrite the list — into scratch space that the
next job erases. Nothing wrote the result anywhere durable. The list's entire
recorded history was two entries, both produced by hand.

The consequence was quiet and total: one crate's tests had never been in the
commit gate at all. Every green result anyone had seen was green about
everything except that crate, and the number displayed alongside it was large
enough to look like coverage.

This is a shape worth naming, because it generalises past this instance. **A
gate defined by an allow-list cannot detect that its own list has gone
short.** Adding a component adds nothing to the list; the gate keeps passing,
faster than before, and the speed reads as a virtue. Nothing is red, so
nothing is examined. The failure is invisible precisely in proportion to how
much the list is trusted.

The fix has two halves, and only the first is mechanical: a successful run's
list is now copied somewhere the next job cannot erase, and a person brings
it back deliberately. The second half is a check that fails when a component
has no entry at all — three-valued, in the pattern this project uses
elsewhere, so that a known gap may be declared with a reason, and a
declaration that has quietly become satisfied fails just as loudly as an
undeclared one. A one-directional acknowledgement can only ever be satisfied,
and so it rots.

The first of those two halves was wrong, and the correction belongs here
rather than in a later chapter's footnote. The copy did not work either, and
the reason is that this diagnosis stopped one step short. The list was not
being written and then erased; it was never being written at all. The command
that produces it refuses whenever the machine's exclusive claim is held —
sound in itself, since a contended run's timings would poison the baseline —
and every serialised path in this project runs that command as a descendant of
the process already holding that claim. So the producer declined, in the one
environment on the one machine where nothing else was running, and the copy
faithfully carried an unchanged file, because it was conditioned on the file
having changed. [The Sluice](./the-sluice.md) found it by reading the list's
own recorded history and finding a single hand-written entry where two
successive remedies should each have left a trail. The remedy that stands
asks the question that was actually meant — not *is the machine claimed*, but
*am I contending with whoever claimed it* — and a claim held by one's own
ancestor is now recognised as the job one is part of.

## The pattern under all of it

Five separate times in this campaign, a confident first explanation was
overturned by the cheapest possible measurement — running the thing alone,
running it on the other machine, reading what a script actually contained
rather than what its documentation said it did. In no case was the first
explanation unreasonable. In every case it was available *before* any
measurement, and it survived exactly as long as nobody took a reading.

That is the durable lesson, and it is not about any of the four reds. A
diagnosis inherited from a plausible narrative — including one's own — is a
hypothesis wearing a conclusion's clothes. The measurement that would settle
it is usually far cheaper than the work that follows from believing it.
