# The Ell

An ell was a unit of length. In England it was forty-five inches; in Flanders,
twenty-seven. Both were called an ell, both were honest, and a bolt of cloth
measured in one and sold in the other lost forty percent of itself between the
loom and the counter without anyone lying. The unit's name was not the
problem — the name was fine. The problem was that the name did not say which
one it was.

Hornvale's fact envelope carried an ell. Every committed fact may carry a
timestamp, a field documented as *the simulated day this fact was observed*.
The history bake stamped years into it.

## A shared axis wearing a homogeneous type

The envelope is deliberately universal. Astronomy, terrain, climate, religion,
culture and settlement all write facts into one ledger, and a fact is a
subject, a predicate, an object and — optionally — a moment. One field, one
type, every writer.

That uniformity is exactly what hid the defect, and the mechanism is worth
stating precisely, because it is not "somebody forgot the units." A bare
floating-point timestamp shared across nine domains is not one axis. It is as
many axes as there are writers, coincidentally represented by one type, and
the type is silent about which axis any particular value came from. The
readers cannot see the writers; each reader knows what *it* means by a day,
and has no way to ask what the value in front of it meant to whoever wrote it.
Documentation does not close that gap, because documentation is read by the
person who already agrees with it.

The second thing that hid it is arithmetic. Six of the domains stamp their
facts at day zero and never touch the field again — a world's astronomy, its
terrain, its climate are all facts about the world's beginning. Zero is zero
in any unit. For as long as exactly one subsystem carried a non-zero timeline,
there was nothing for a mismatched unit to disagree *with*. The ell was
harmless until a second bolt of cloth arrived.

## The second timeline, and what it cost

That happened when individual persons entered the world. A community's founder
became a named person with a birth, a founding, and — where the arithmetic
allowed it — a death. Promotion computed a birth as the founding minus the
species' age at maturity, and then asked whether the founder's lifespan had run
out before the present.

The founding was a bake **year**. The maturity was a count of **days**. The
present was a year again. The subtraction is a forty-five-inch ell minus a
twenty-seven-inch one, and the comparison that followed was unsatisfiable: for
a death to be committed, a species would have needed its lifespan to exceed its
age at maturity by no more than about five and a half years, where the
narrowest margin in the roster is thirty-nine. Not *rare*. **Impossible, for
every species, in every world.**

The predicate for a person's death was registered, documented, and produced
zero times, in every world ever generated. Four layers agreed it worked. The
concept registry held it; the domain's unit test hand-built both branches and
asserted correctly on each; the capability probe counted it as vocabulary the
world holds, because registration is supply and nobody was measuring demand;
and the live-world test that walks every person and asserts a death follows a
birth contains a conditional that had never once been entered while staying
green. Correctness of a branch says nothing about its reachability, and nothing
in the suite asked *does any world actually produce this?*

## Two repairs, because one would not have held

The obvious repair is to divide somewhere. That is the repair this campaign
refused, and refusing it is the whole design.

**The unit was moved to the boundary.** A history bake reasons in years and is
right to — its era ticks are years, its configuration is years, its internal
arithmetic is years. What crosses into the ledger is days. The conversion
happens where the value is written, which is the same discipline the ledger's
float quantization already follows (decision 0033): convert at the emit
boundary, never in the compute path. Reads cross back at named functions, one
per crate, so that a crossing is a thing with a name rather than a `* 365.25`
sprinkled through fourteen call sites — which is precisely the
documented-unit answer the superseded decision gave, and which this campaign
exists to reject.

**The slot was typed.** The timestamp is no longer a bare float. It carries
`WorldTime`, a type the kernel had defined all along and used four hundred and
forty-five times for fields, phenomena and observers; the fact envelope was the
one time-carrying surface in the kernel that had opted out. Its inner value is
now private behind a validating constructor, so a year cannot be placed in a
day-typed slot without saying so.

Two properties of that constructor are load-bearing and easy to get wrong. It
validates **finiteness only**. A day is a *point on an axis*, not a duration,
and it goes negative legitimately — a founder of a year-zero community is born
before the history record begins, and forty-four of seed 42's founders have
negative birth days. The kernel's duration type rejects negatives, and reaching
for it would have re-broken the same founders in a new way. Conflating a
duration with a point is how a "just use the existing type" repair fails
quietly.

And the guarantee has an edge that must not be overstated. The type guarantees
that **no non-finite value can be constructed**. It does not guarantee that
none can be **deserialized**: the derived deserializer, which is what keeps the
saved shape a bare JSON number rather than a wrapped object, never calls the
validating constructor at all. That path is closed today only because the
project admits exactly one deserializer and it rejects out-of-range numbers
outright. A type-safety claim actually held by a parser is the same layered
agreement that let the original defect survive four green layers, and it is
recorded as a dependency rather than filed as a property of the type.

## The epoch, measured

Every occupation fact in every saved world changed. The seed-42 world went from
13,389 facts to 13,533, and aligning the two by subject and predicate says
exactly what moved:

```
  objects moved     5 predicates   occ-founded  704 facts, 656 moved  25.0 -> 9131.25
                                   occ-ended    474 facts, 474 moved 175.0 -> 63918.75
                                   history-now    1 fact,     1 moved 2000.0 -> 730500.0
                                   person-born  148 facts,  106 moved
                                   name         394 facts, 148 moved  <- not a unit
  day stamps moved 21 predicates   every fact the history bake emits
  untouched        92 predicates   astronomy, terrain, climate, paleoclimate,
                                   topology, religion, culture, species,
                                   alchemy, demography — and settlement's own
                                   genesis facts
```

**Four of those five moved because a unit moved. The fifth is the other
epoch.** Every one of the hundred and forty-eight promoted founders was
*renamed*, by the key change the second half of this chapter is about; the
world changed its clock and its names in one step, and a reader who takes this
table for the unit repair alone will come away with half of what happened.

The partial counts are themselves the check, and the two kinds of partial have
two different causes. Founding facts moved 656 of 704 and person births 106 of
148: the remainder are the **year-zero** cases, where the conversion is the
identity — *zero is zero in any unit*, visible in the data at exactly the places
the argument predicts. `name` moved 148 of 394 for an unrelated reason: 394
things in this world carry a name, and only the 148 that are **people** were
re-keyed. Settlements, deities and the rest keep theirs. A partial count is
evidence only when you can say which exemption produced it, and these are two
exemptions wearing the same shape.

And the deaths arrived:

| seed | founders | deaths before | deaths after |
|---|---:|---:|---:|
| 42 | 148 | 0 | 144 |
| 7 | 122 | 0 | 121 |
| 1000 | 174 | 0 | 173 |
| 3 | 213 | 0 | 205 |
| 99 | 197 | 0 | 194 |

Ninety-six to ninety-nine percent, which is high enough to deserve suspicion —
a rule that fires for everyone is as wrong as one that fires for no one, which
is why the acceptance criterion was two-sided rather than a floor. It survives
interrogation as physics. Over a seven-hundred-thousand-day history, almost
every founder outlives their lifespan; the survivors are the founders of the
youngest communities, and they sort by lifespan as they should — on seed 3 the
long-lived high elves survive at seven in thirteen against the short-lived
hobgoblins' one in twenty.

The sharper property is a separation: **every surviving member of a species was
born after every dead member of it.** The whole-branch review built all five
worlds live and found no exception among their seventeen survivors — which is
the honest weight to put on it. Seed 42 is the only world with a committed
fixture and no harness in the tree computes this metric, so the property is a
reading taken at review, not something the gate holds; and seventeen is a small
enough denominator to be worth naming next to the claim. Survivors are the
latest-born *block*, not the latest-born individual — on seed 42 one surviving
desert-dwarf was born a hundred years
before the last of its kind, and is alive because the whole tail of that
lineage is. A death rule keyed on lifespan produces exactly that shape; a
guard failing at random does not.
The cast itself did not move: the founder counts are identical before and
after, which is the design's central claim and was measured rather than argued.

One consequence ships as a finding rather than a fix. Those hundred and
forty-four deaths are committed, and no window renders them. A world now knows
when its founders died and cannot yet say so. That is the original defect's
sibling one layer out — a fact that exists and is not spoken — and it is worth
more stated than quietly patched at the end of a campaign about saying what you
mean.

## The same shape, one level up

The campaign then found the ell a second time, inside its own repair, and the
second time is the better ending.

Every founder's name is derived from a handle folded out of their occupation —
its people, its site, its founding year, its ending, and its peak population.
Two occupations agreeing on all five collide by construction, and a collision
costs a world one of its remembered founders. The repair looked settled before
it was measured: a key already existed in the same crate that folds a founding
by people, site and year *plus one hop of ancestry*, deliberately excluding
everything after the founding, on the stated rationale that a founder's name
must not depend on how their community later died. Wiring the handle onto that
discipline was supposed to fix the collision and make an honest doc true at the
same time.

Measured over a thousand worlds, it is **three hundred and sixty-six times
worse**. The old key collides in two worlds; the founding-only key collides in
seven hundred and thirty-two, costing 1,582 founders. The mechanism is not a
hash accident and no amount of ancestry repairs it:

```
  DROPPED  bugbear at site 7396, founded 175, ended 175, peak 8, cause Fled
           parent: bugbear at site 29341, founded 0
  KEPT     bugbear at site 7396, founded 175, ended  --, peak 66
           parent: bugbear at site 29341, founded 0
```

A people founds at a site in a year; the attempt is raided and closes in the
same year it opened; a second record — same people, same site, same year, same
parent — carries the community that took. The two are identical in **every
founding-side field there is**, so a second hop of ancestry reaches the same
parent and a third reaches the same grandparent, bit for bit. Only a fact from
after the founding can separate a failed attempt from its successor.

The interesting part is what that means about the key rather than about the
seed. The crate had been deriving two different kinds of value and calling them
by one word. An **identity** key answers *is this the same thing?*, and must
read founding-side facts only, because a value that moves when later events
move is not an identity — it is a summary. A **discrimination** key answers
*give me a distinct deterministic draw across this population*, and may
legitimately read anything the record carries. The founder handle was being
asked to be the second while wearing the first one's name and inheriting its
rationale. One name, two meanings, no marker — this campaign's own defect, one
level up, inside the fix.

Naming the two kinds resolves it without argument. The handle is now the
identity key with the discriminating tail folded visibly on top:

```
  handle = fold( founding_key(occupation, parent),  ended, peak_population )
                 \______ identity ______/           \___ discrimination __/
```

Zero colliding worlds and zero dropped founders across a thousand seeds. The
rule that a founder's name must not depend on how their community later died is
not abandoned — it was **mis-assigned**. It is the identity key's rule, it is
true there without exception, and it was never the handle's to hold. A founder
is named by their founding, its ancestry, and their span.

The cost is real and it is not the one the objection expects. It is not time
asymmetry: the entire history is baked before anything is named, so nothing in
the tail is in the future from the key's point of view. It is **coupling**.
Every post-founding field in the tail is a field some later campaign may
recompute, and each recomputation renames every founder in every world. Trimming
was measured rather than assumed, and refused on the evidence: the ending alone
leaves five colliding worlds per thousand, the peak population alone leaves
five, and the two failure sets are nearly disjoint. One seed appears in both,
which means it holds two separate colliding pairs — one the records can
separate only by how big the community got, one only by when it ended.

## What the widening cost the names

Naming is discrimination, so widening the key could have made founders' *names*
collide more often. Measured across three seeds and 444 founders: six shared
names before, six after — 1.3514% either way, a clean null. It is not the same
six. The per-seed distribution moves from one, two, three to four, one, one;
the widening redistributes which founders share a name without changing how
often it happens. Three seeds and 444 founders is a small base for a rate of
one percent, so the null is a null and not a precise estimate — but an exactly
equal before and after is the shape a double-measurement takes, and the moving
rows are what show this one is real.

## What the unit was, in the end

The campaign's two findings are the same sentence at two scales. A float called
`day` that means years to one writer is one name with two meanings. A key called
a founder's identity that is really a draw-discriminator is one name with two
meanings. Neither was a mistake anyone made — both were correct, locally, at
every site that touched them, and both were invisible to a suite that was
green.

What distinguishes them is only which instrument found them. The first was
found by computing a number the tests did not assert. The second was found by
measuring a design that had already been ratified, over a thousand worlds
rather than a handful, and finding it three hundred and sixty-six times worse
than what it replaced.

The English ell and the Flemish ell were both eventually retired in favour of
the metre, which is not a better length. It is a length with one meaning.
