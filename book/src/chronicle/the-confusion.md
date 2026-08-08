# The Confusion

A test failed with a message explaining why it had failed. The message was
wrong, and the interesting part is how long it took anyone to ask.

## The sentence nobody measured

Two heavy-tier readouts went red when the settling roster grew from six peoples
to nine. Both stopped with the same diagnosis:

> `Ended::By(raider)` no longer names exactly one occupation record, so every
> initiator rate below is wrong.

That is a precise, technical, entirely plausible claim about a data structure.
It says: raids used to be attributable to a single raider, and at a larger
roster they no longer are. It even suggests its own remedy — if attribution has
become ambiguous, the rates built on it must be rebuilt on something else.

It had never been measured. It was written into the panic string when the guard
was *built*, by an author describing the failure they imagined the guard would
one day catch. When the guard finally fired, it fired for a different reason and
reported the imagined one.

**A diagnosis inside an error message is the most credible-looking hypothesis a
codebase can produce**, because it arrives at precisely the moment the reader is
disposed to believe it: something has just broken, and here is an explanation
already written down, in the voice of whoever knew this code best.

## What was actually wrong

The check compared two counts. The numerator walked the world's occupation
records and counted raid victims, skipping any record belonging to a people
outside a frozen list of six — the roster as it stood when an earlier campaign
preregistered its hypotheses. The denominator was the world's own raid tally,
which counts everything.

That comparison is valid only under a premise the file states in plain sight:
*every occupation record in the shipped roster belongs to one of the six
settling peoples.* True when written. Three new peoples made it false.

Over thirty worlds, the arithmetic is unambiguous:

```
  raid tally, world-wide                6163
  per-record count, unfiltered          6163      exact, on every seed
  per-record count, filtered to six     5782
  the gap                                381
  victims outside the frozen six         381      exactly the gap
```

Attribution was never ambiguous. Every reference resolved to exactly one record,
on every world. The numerator had been frozen and the denominator had not, and
the premise that had been holding them equivalent quietly expired.

**A premise is not a filter.** It is a filter nobody is maintaining, and it fails
silently the moment the world outgrows it.

## The repair that would have been correct and useless

The obvious fix is to freeze the denominator too. It is two lines, it restores a
green suite, and it preserves the frozen population exactly as the file demands.

It is also pointless. The preregistration those readouts protected had already
been discharged — its three hypotheses were adjudicated and their verdicts
written into the record long before. The battery had become a thirty-world
computation that re-answered a settled question on every heavy-tier run, and the
proposed repair would have restored it to doing that correctly.

**Fixing a check says nothing about whether the check should still run.** The
defect and the instrument are separate questions, and diagnosing the first is
seductive precisely because it feels like progress on the second.

## What replaced it

The measurement moved to the thousand-world census, where the population is the
instrument rather than a sample of it — three columns: the rate at which
settlements are raided, the rate at which they raid, and a census-wide invariant
asserting that every raid reference resolves to exactly one record.

That last column is the retired message's claim, converted from an assertion
nobody had tested into one measured on every world: it reads zero on all thousand.

The population changed deliberately in the move, from the frozen six to all nine
settling peoples, because the question is no longer *what did that campaign
measure* but *what is the raid rate*. The historical answer keeps its place in
the record; it does not need a battery re-deriving it forever.

## The migration's own argument, made against itself

Before the census ran, a twelve-world probe reported the victim rate as spanning
0.014 to 0.373, and raiders as taking 1.00 to 1.03 raids each.

The census reads 0.000 to 0.450, and 1.0000 to 1.1086.

The small sample missed **both tails**. It never saw a world where nobody raided
at all — three exist — and it understated the busiest world by a quarter of its
own range. Nothing was wrong with the probe. Twelve worlds cannot see a
three-in-a-thousand event, and no amount of care in reading twelve worlds will
make them able to.

The two rates also turn out to sit at a median ratio of 1.02: a raider is very
nearly one-shot. That makes the offence column nearly a rescaling of the defence
column, and its entire worth is in the tail where the two come apart — which is
another way of saying the same thing. The interesting part of a distribution is
rarely near its middle, and a sample chosen small enough to be cheap is a sample
chosen to contain only the middle.
