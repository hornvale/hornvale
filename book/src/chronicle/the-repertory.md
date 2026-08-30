# The Repertory

*A repertory company keeps a standing set of pieces it can perform on any
given night. The word carries both halves this campaign needed: the script
that was written down, and the performance that either happens or does not.*

Hornvale has an unusually dense apparatus for asking *is it correct*. A census
whose committed table runs to 229 columns. Five-valued trope verdicts.
Byte-identical goldens across two architectures. A default-deny type audit
that fails on any untagged primitive at a public boundary. Mutation testing
for the seams no assertion pins.

None of it can tell you whether a scene plays.

That is not a gap in rigour, it is a gap in *kind*. Every instrument above
resolves statically: a trope against the concept registry, a system against
repository facts, a sentence against the grammar's declared constructions.
Each answers a question about the program. None answers a question about a
world in motion — whether an act reaches through the stack and comes back
changed.

This campaign founds a fourth corpus family, `repertory/`, and its
distinguishing property is a single sentence: **a scene's verdict comes from
running the thing.** Not from a registry row, not from a declared token, not
from a hand-maintained list. The resolver drives a real possession and reads
the snapshot it writes.

## The principle, and why it is a gap rather than a claim

The instrument's shape was fixed by a sentence from the project's owner:

> We should be able to explicitly author anything that we want to be able to
> generate procedurally.

Read once, that is a vision statement. Read again, it is a testable claim
about the distance between two poles — what an author can specify, and what
the world produces unprompted — and the distance is the thing worth
measuring. So every scene carries two verdicts rather than one:

- **AUTHORED** — specify the scene; does the world carry it?
- **REACHED** — do *not* specify it; across many worlds, does it arise anyway,
  and at what rate?

The diagnostic value is neither pole but the middle. **AUTHORED but not
REACHED** says the world can hold a scene it never generates, which means a
pressure is missing or something cheaper is out-competing it. That is the
fundamental-versus-realized niche distinction borrowed intact from ecology,
and it arrives with its own follow-up question: *what is crowding this out?*

`REACHED` is deliberately a **rate**, never a flag. A scene arising in one
world in a thousand and one arising in nine hundred are both "reached" under a
boolean and mean opposite things. Framing it as a rate also collapses a
requirement that had been tracked separately — that a scene must be able to
resolve *differently* across seeds, since a scene with one outcome is a
cutscene rather than a simulation. Those turned out to be one measurement.

Only `AUTHORED` shipped. Recognising a scene inside an *unscripted* run is a
harder problem than asserting on a scripted one, and the schema carries no
unresolved reach field, because a field with no resolver behind it is exactly
the defect the family was founded to avoid.

## The fork that was not a fork

The corpus could plainly be sourced two ways: transcribe found interactive
fiction, or author original scenes. The campaign spent its design effort
deciding between them and the decision dissolved: they are positions on one
axis, **how much of a scene the author fixes**, and they measure opposite
things.

```
  control   what the author fixes           exemplar
  -------   -----------------------------   -----------------------------
       0%   a place and a time              a goblin village bustling
      40%   the participants                a drow and a goblin, a room
      60%   + an intent                     the drow wants the orange
     100%   every beat                      `echo` in the Loud Room
```

Interactive fiction is an authored artifact with every beat specified, so it
probes whether the world can *host* a fixed scene. Original scenes at the low
end probe whether it *produces* one. A found corpus also carries an epistemic
property an authored one cannot: it was written by people who had never heard
of Hornvale, which is the same reason a borrowed situation catalogue makes a
trustworthy instrument and a self-authored one does not.

## Scenes are found, not staged

A scene declares constraints on its participants; a **witness** records a real
`(seed, target, day)` where those constraints hold. Nothing constructs the
situation.

The reason is validity rather than economy. Staging would author the world,
and a verdict obtained in a staged configuration is evidence about the
staging — it would let a scene pass in an arrangement the world could never
produce. The automated search that would *find* a witness is unbuilt; today
they are found by hand. The guarantee survives that gap intact, because a
hand-found witness is still a real world.

## The four scenes that already passed

The corpus ships with a positive control: four scenes that were run and
verified **before the resolver existed**. A body walks east and the room it
senses changes. A body waits and the day above it moves. A body waits and does
not thereby travel — the negative-space form, asserting what must *not*
happen, because a corpus that only ever asserts what can happen ratchets
toward permissiveness and a world that can play out anything has no physics. A
body can tell it shares its room with someone.

This is not ceremony. Had every scene been absent on the first day, the
resolver would never once have demonstrated it could produce `AUTHORED`, and
every later green would have been a green nothing had earned. The same logic
put a test in the suite whose only job is to prove the evaluator can return a
failure at all.

## The orange

The founding scene was chosen by the project's owner:

> A goblin and a drow in a room, and the drow is trying to convince the goblin
> to give him a particularly ripe orange.

It stands red, and is expected to for several campaigns. Decomposed into
beats, it is a task list:

```
  1  a drow and a goblin are co-located        search only; holds today
  2  the goblin holds an orange                object instances + possession
  3  the drow addresses the goblin             a verb that reaches another mind
  4  the goblin understands the request        parse to intent, another mind
  5  the goblin decides                        a disposition response to a request
  6  the orange changes hands, or does not     transfer
  7  the outcome varies across seeds           the decision must be contingent
```

Beat 1 passes — it is the same assertion the founding corpus already resolves.
Beat 2 does not, and the reasons are specific rather than atmospheric. Objects
carry a property vocabulary but no instances; the table that holds them is
build-state rather than world-state and is never serialized. The session
snapshot has no channel for what a creature holds. And of the twenty-five
verbs a possessed body answers to, not one addresses another creature: the
verb that asks, asks the body you are *wearing* how it feels, and the verb
that speaks a line writes it into your own margin.

Only beats 1 and 2 are encoded. The rest have no surface to assert against,
and writing them anyway would have put five unevaluatable records into a
corpus whose entire premise is that a verdict comes from a run.

The declaration that holds the scene green carries its reason in full, and the
verdict `STALE-DECL` exists to delete it: the moment the scene starts working,
the acknowledgement fails. A one-directional admission can only ever be
satisfied, so it rots; this one is made to notice.

### The declaration went stale within hours, and the beat could not have noticed

The Chattel merged while this campaign was in its final task, carrying
*objects that can be held* and putting carried things on the wire. Absorbing
it changed no verdict — `the-orange` still stands declared — but it falsified
two things that had already been written down.

The declaration's stated reason claimed the session snapshot *has no inventory
channel at all*. That had been true when it was written and was false within
the day: `/self/carrying` now exists, a list of `{entity, noun}`. What does
**not** exist is the co-located half — a `social[]` entry is still exactly
`{entity, label, grievance, hostile}` — so the scene is red for a narrower and
more interesting reason than the one recorded.

The second thing is worse and would have outlived the campaign. Beat 2
asserted a pointer named `/social/0/holding`, invented before the vocabulary
existed. The Chattel established `carrying`. A beat asserting a name nobody
will ever create cannot fail *to notice*: when the co-located half eventually
lands, the pointer would still resolve nowhere, the scene would still read
declared, and the ratchet built precisely to catch that moment would sit green
and silent through it. A beat that can never pass is strictly worse than no
beat, because it occupies the place where a working check would go.

Both are the same underlying error — a record written from outside the code,
against a vocabulary the author guessed rather than read — and neither was
findable by re-reading. Absorbing another campaign's work is what surfaced
them.

## What the instrument cannot do

A fully green repertory is entirely compatible with a world nobody wants to
stand in. Traversal is necessary and nowhere near sufficient, and reading a
green roster as evidence about whether a place is *worth being in* is the
specific error the new bet in the Confidence Gradient exists to prevent. That
half stays a bet and never becomes a metric — a scored proxy for aliveness
would be optimised against long before it was validated, which is the one
failure mode that leaves every gate green while the thesis quietly fails.

## Cost, and one collision

Eight tests, 43.6 seconds, dominated by nine real world-builds at roughly 3.7
seconds each. That is cheap enough to run often and far too expensive to run
on every commit for a corpus that moves at campaign cadence, so it stays out
of the commit tier by measurement rather than by omission.

One failure is worth recording because it belongs to a class. The resolver's
scene-to-verdict function was first called `verdict`, and it reddened a guard
belonging to another campaign entirely — a test whose stated premise is that
*the bare word `verdict` names no function anywhere in the command-line crate*,
which is how it constructs a symbol that is a strict prefix of a real one. The
new function falsified that premise from a different file. Nothing was wrong
with either piece of code. A test can encode a claim about the **absence** of a
name, and absence is the one property a newcomer cannot see they are about to
destroy.
