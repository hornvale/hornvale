# The Presiding

*Campaign: The Presiding — a world has no religion; its peoples do.*

## One soul speaking for a world

The lab measured a per-world quantity it called `belief-kind`: the sentiment
tag of a world's presiding belief — eternal, cyclic, or ambient. Its
implementation was a single line:

```rust
beliefs_of(&world).first()
```

the first belief minted anywhere in the ledger. Beliefs mint in the order the
composition root iterates species, and that order is the species registry's
`KindId` order, which is alphabetical. So `belief-kind` reported the head
deity of whichever people sorts first among those that hold a pantheon.

On every world Hornvale generates, that people is goblin — and on every
naturally tidally-locked seed, the goblin who holds it is one person. The
founder floor guarantees each people a flagship settlement; it does not
guarantee the settlement grows. A single goblin, placed alone, whose pantheon
was recorded as the religion of a world holding twenty-seven hobgoblins:

```
seed | chief settlements (species : population) | belief-kind read from
   8 | goblin:1  hobgoblin:23  kobold:4          | goblin (pop 1)
  78 | goblin:1  hobgoblin:27                    | goblin (pop 1)
  13 | goblin:1  hobgoblin:19                    | goblin (pop 1)
```

Across the spinning worlds too, the reading came from the least populous
people on the map. Alphabetical order and dominance disagreed on twenty-two
of twenty-two seeds sampled, and the metric always chose the alphabet.

## The row asked for the wrong fix

The project's open-questions ledger knew something was wrong here and filed
it: the presiding belief is *dominance-blind*, decided by registry order
rather than by who actually lives in the world. Make it dominance-aware — let
the most populous people's head deity preside — and the long-sought
tide payoff would return, locked worlds heading their oceans again.

Two measurements dissolved that plan before a line of it was written.

The first: the tide would not return. On all nine locked seeds, the dominant
hobgoblin's own pantheon *also* heads Eternal, not the tide. Swapping goblin
for hobgoblin changes the name of the people and not the sentiment of the
belief; the census reading stays `eternal`, nine times out of nine. The gate
on the tide payoff is elsewhere entirely — a separate movement, still open, in
which the ambient-headed tide belief went extinct as anyone's head — and no
change to *whose* belief presides can touch it.

The second, and deeper: the concept itself was the artifact. "The world's
presiding belief" was well-defined once, when goblin was the only people and
the world had exactly one pantheon. The Branches gave worlds four peoples and
four pantheons, and every sibling religion metric followed — `cult-form-goblin`,
`pantheon-size-kobold`, one reading per people. `belief-kind` alone stayed at
the world scale, a legacy singular measuring a quantity that no longer
existed. Asking which people should own "the world's religion" is asking a
question with a false presupposition. A world has no religion. Its peoples
do.

So the metric was not made dominance-aware. It was retired. In its place, one
reading per people — `belief-kind-goblin`, `belief-kind-hobgoblin`,
`belief-kind-kobold`, `belief-kind-bugbear` — each the head of that people's
own pantheon, Absent when that people holds none. Nothing in the world
changed; `beliefs_of` still returns what it always did. Only the question the
lab asks of it changed, from *what does this world believe* to *what does
each people in it believe*.

## The people who were never there

Three documents — the open question's own row, the acceptance battery that
discovered it, and the frozen-sky calibration itself — all named the culprit
as **bugbear**: alphabetically first in the registry, guaranteed a flagship
by the founder floor, therefore first to commit its pantheon on every seed.
It is a clean mechanism and every clause of it is checkable in the source,
and it is wrong. Bugbear sorts first but never places. The thousand-seed
census reads `belief-kind-bugbear` as Absent on all one thousand worlds. The
people blamed for speaking first for every world speaks for none of them.

The error survived in three places because its premise — the alphabetical
sort — is genuinely true, and the false step is invisible: no one who wrote
it generated a world and looked. It is the same inference The Named caught in
its own renderer weeks earlier, and it was made here twice more,
independently, on the same day. The registry order is real; the placement is
a measurement; the two are not the same fact, and only the second can be
reasoned about by reading code.

## The shape, a third time

The Named demoted block zero from a privileged base case to a peer among
pantheon renderings — *being first in a loop is not a fact about the world*.
The Lens, a layer above, demoted the `natural` render from a base case to a
peer among lenses — *a picture that looks like a photograph has no special
claim to truth*. The Presiding demotes `beliefs_of().first()` from a
world-level fact to a per-people one — *the first belief minted is a fact
about iteration order, not about a world*.

Three campaigns, three layers — renderer, view registry, metric — and in each
the bug wore the same costume: a default that looked like conservation.
Something worked as the base case, the general case was layered around it, and
the base case turned out to encode an assumption no one had stated. The
question that finds it is never *does this work?* — the base case always
renders something plausible — but *what is this actually a fact about?* For
`beliefs_of().first()`, the honest answer was: a single soul the founder
floor happened to place first, speaking for a world that had never heard of
them.
