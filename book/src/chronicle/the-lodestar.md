# The Lodestar

*A lodestar is the thing you steer by. This campaign is about four of them
that pointed somewhere they should not have: a renderer that dropped what the
wire carried, three tests that passed while proving nothing, and a number that
was wrong in five documents at once.*

[The Gallery](./the-gallery.md) made the underworld a band a player can walk,
see, remember, and meet something living in. It shipped twelve tasks, each one
individually reviewed, and every one of those reviews came back clean. It
merged.

A whole-branch review run afterwards found, in its first pass, that the
campaign's headline act did not reach the screen.

## The gap that belonged to neither task

The simulation derives a cave chamber's inhabitant from that chamber's own
rock — its substrate, its chemical energy, what a creature's authored niche
makes of both — and puts the result on the wire in the level document's
`marks` list. The terminal client draws the level. It did not draw the marks.

Not through a bug. Through a sentence. The renderer was written one task
before the derivation that fills `marks` existed, and it said so, twice, in
its own documentation:

> `Level::marks` ships `[]` until spec §3.6's placement lands (Task 11) …
> this module deliberately does not write a marks-drawing pass at all.

Task 11 landed. Nobody reopened the renderer. The sentence was true when
written, false three tasks later, and describes exactly the code a reader
would go looking at to decide whether anything was missing.

The consequence a player would meet: descend, stand in a chamber that the
world's own energy budget says can feed a rust monster, and see an empty
room. The creature exists. It is derived, it is lit-gated, it is on the wire,
and it is examinable by name — but the name is not offered anywhere, so the
only way to reach it is to already know it.

**No per-task review could have caught this.** The task that wrote the
renderer was correct: `marks` genuinely was empty then. The task that filled
`marks` was correct: it changed the producer, and its own reviewer verified
the producer. The defect lived in the join, and a join has no owner.

## Three tests that could not fail

The same review found three assertions that were green for reasons unrelated
to what they claimed to guard.

The most consequential guarded a constitutional contract. Serialized floats in
this project are quantized to eight significant digits at the emit boundary
(decision 0033), and exactly one test pinned that for the underworld's new
wire schema. It asserted that the emitted JSON contained the substring
`0.33333333`, for a depth of one third.

One third serializes, unquantized, as `0.3333333333333333` — which contains
that substring. The test passed whether or not the quantization ran. Deleting
the attribute it existed to protect left it green. It had been copied verbatim
from an older test with the same defect, so the pattern had already propagated
once before anyone looked at it.

The second named a rule in its own title — that worked stone stays drained
while natural rock floods — and then built a fixture yielding a single natural
leaf, so the branch carrying the name never executed. The third asserted that
walking only ever adds to what is remembered, using a superset check, which is
trivially true of a set that never shrinks; its guard against vacuity was
already satisfied before the walk began, because descending marks the entrance.

All three are the same shape: **an assertion that cannot distinguish the world
where the code works from the world where it does not.** They are invisible to
reading, because reading tells you what a test says and not what it can
detect.

## The only way to fix a test that proves nothing

A replacement written by the same reasoning that produced the original will
have the same defect, and will look equally correct. So each of the four fixes
here carries a demonstration: the code the test exists to protect was removed,
the test was run, and its failure was recorded before the code was restored.

    a marked cell must draw the mark's own glyph
      left: Some(':')  right: Some('&')

    depth_m must not serialize as the raw, unquantized f64
      ("depth_m":0.3333333333333333)

    a worked leaf must stay drained even under a phreatic water table

    32 attempted steps from a connected level must have GROWN what is
    remembered at least once

Four mutations, four reds, four restorations. A reviewer then reproduced all
four independently rather than reading the transcript of them, which is the
same discipline applied one level up: a claim that a test catches something is
itself a claim that can be checked, and checking it costs a minute.

## What the number said

The review also found the campaign's own account of a defect it had correctly
registered. The underworld pane draws a generated level anchored at its own
corner, with no camera-follow, and silently drops whatever falls outside the
plate. The campaign recorded this as affecting "every rung past the first".

The five walkable rungs run 44×26 to 60×34. The plate is forty columns wide on
every terminal, because the underground band never receives a world plate at
all and only its height grows. **No rung fits.** The marker can leave the
screen on the first one, not the second, and at the smallest supported terminal
a player sees between seven-tenths and four-tenths of the level they are
standing in — with nothing on screen to say so.

The wrong sentence had reached five documents: the chronicle, the retrospective,
the open-questions ledger, the capability audit, and the frozen corpus the
audit is generated from. It originated in one clause of the specification —
`rank ≤ 5`, which reads as though rank zero were walkable — and every later
document inherited it faithfully. Correcting it makes the project's own
capability score *worse*, which is the direction a re-score is least likely to
be pushed and most important to get right.

## Two mirrors

One finding is registered rather than fixed, and naming why is the point.

The wire schema has two client mirrors, not one. The campaign updated the
terminal client and never touched the browser client, which still declares its
spatial union as walk-or-chamber and goes blank underground — silently, with
its own gate green throughout, in the failure mode the campaign's own
specification had been rewritten mid-flight to warn about.

The workspace tripwire built during that rewrite — an exhaustive match over the
band enum, compiled solely so that adding a variant breaks the build — works,
and was verified to work. It is hardcoded to one client's test directory.

Fixing the second mirror and widening the tripwire to every mirror are one job,
and doing half of it would leave a guard that reads as complete. It is recorded
as `CLIENT-second-band-mirror`, unfixed.

## What this campaign is evidence for

Twelve reviews passed and a thirteenth found three Criticals. The difference
was not diligence; the per-task reviews were thorough, and several of them
caught real defects by recomputing numbers from source rather than reading
reports. The difference is that a task-scoped review is scoped to a task, and
three of the four defects here lived in the space between two of them — a
renderer and a producer, a specification clause and the five documents that
inherited it, a wire and the second consumer nobody enumerated.

The cheap instrument for that is not a better review. It is asking, once, at
the end: **which of the things this campaign said it would do can a player
actually observe?**
