# The Roll

*A roll is two things at once: the register of who belongs to a place, and
the call that establishes who is present. This campaign builds both — a
settlement's residents are its roll, and the walk ticks the ones within
call.*

## Seven bodies, seven thousand inhabitants

A possession session held **seven bodies**, and the number was authored: three
settlement creatures and four wild ones, two constants at the top of a file.
The world they stood in, read off the committed census over a thousand seeds,
holds about **265 settlements** and **7,400 abstract inhabitants**, at a mean
population of 27. A settlement of eighty held one creature; a world-wide herd
of woolly mammoths was one body at one vertex.

The measured consequence, from the campaigns before this one. Until The Hand,
a possessed player always stood beside a twin of their own body, and that
artifact was the entire supply of company; The Hand deleted the twin and
measured what was left — **no other creature present in 0 of 64 seeds**. This
campaign's own first task re-ran the same 64-seed probe before changing
anything and read **3 of 64**, which is the baseline every number below is
measured against. What separates the two readings is not explained here and no
mechanism is claimed for it: the 3 is a fresh measurement taken at a later
commit, not a correction of the 0. Either way the world was populous on paper
and empty to walk through.

## What a resident is

A resident is a **living person**. Not a new kind of thing: the same
`is-person` and `person-born` a founder carries, the same namer that names a
founder, the same identity-by-lineage every entity in Hornvale has. A
settlement's residents are its population by ordinal — `(settlement, "npc", 0)`
through `(settlement, "npc", population − 1)` — and ordinal 0 is exactly the
entity the one pre-campaign body already was, so a saved session reloads and
*gains* neighbours rather than losing or renumbering the one it had.

They are **derived, not generated**. Minting seven thousand people into every
world at genesis would have been an epoch — new facts on every world, the
keystone golden moved, a thousand-world census re-measured — to answer *who
lives here* for two hundred and sixty settlements the player will never visit.
Instead the derivation is a pure function of (world, settlement) that any
window may call, and its facts land on the session's own ledger. Nothing in
any world file moved.

One consequence had to be ruled on rather than designed around. A resident's
birth day was computed as *now minus a drawn age*, so deriving the same
settlement twice on two different days would commit two different birthdays
against a predicate that permits only one — a panic, on the ordinary act of
re-possessing a saved world a week later. The ruling: **the ledger wins**. A
resident's birth is fixed at its first derivation, as its name already was.

## The roll is a function of where you stand

Which bodies are simulated at all is **observable** — you can see them, talk
to them, watch them walk — and an observable boundary must be a function of the
world rather than of the program's history. That is the campaign's central
ruling, and everything else falls out of it.

The roll takes the observer's room and returns every resident of every
settlement, and every member of every herd, whose **home** is within two
walk-band hops — a 5×5 window of ~1.1 km rooms — ordered by distance, then
settled before wild, then by parent, species and ordinal, truncated to a
budget of 128.

Membership is by *home*, never by where a body has wandered. A body's current
position is its own history; its settlement's room is world state. A villager
who has walked out of the window while their settlement is still in call is
still ticked, which is both the purer function and the cheaper one.

The ordering key had to be repaired for the same reason it exists. Wild bodies
were first keyed by their position in the derivation list — which is the order
settlements and herds entered the window, which is the route the player walked.
Under a budget that actually truncates, that would have made membership depend
on the direction of approach: the same herd, on the roll from the east and off
it from the west. The key now carries the attractor vertex and the species, so
a herd is the same herd from every heading.

The function is purer than the design asked for. It was specified to take the
world time; implementation found that membership is time-invariant by
construction — a home does not move — so the argument was dropped.

## The numbers

At seed 42's flagship the roll is **68 residents, one of them the driven
body** — the settlement's whole population, since ordinal 0 is a resident like
any other — and no wild members at all, because the nearest herd attractor sits
roughly a hundred rooms out. The budget of 128 has never bound in a live
session, which is why the truncation is exercised by tests asking for three.

`look` now says who is here. It names a few and counts the rest:

```text
Here: Dvoashngashngo, Qvoshngavngo, Shngovngo and Shngoqvo, and 63 others.
```

Five preregistered measurements were frozen in the spec before any of the code
that would move them.

**M1 — company.** Across the same 64-seed probe: *seeds built 64, with company
64, population ≥ 2 64*. The prediction was that the two would be equal — that
every seed whose home settlement can hold company would have it — and it held
exactly, against a baseline of 3. There is no seed at which they disagree, so
the defect the measurement was shaped to find does not exist.

**M2 — the budget.** The target was a hundred bodies inside a one-second tick.
At the roll as it actually occurs, 68 bodies cost **72.0 ms per wait**; at the
pessimistic case of a hundred dispersed walkers, one per settlement, **726.6
ms per tick**; at two hundred, 1708.0 ms, reported rather than targeted. Both
readings were taken **contended** — the box never showed a quiet load in
thirty-three minutes of polling — so both are upper bounds. The two margins
are not the same size and are worth stating on their own numbers rather than
under one adjective: the roll as it actually occurs clears the budget by
**13.9x**, the pessimistic dispersed hundred by **1.38x**. The verdict is met
on both instances; only the first has an order of magnitude in hand.

**M3 — the roll is pure.** Two independent sessions of one seed running one
script produce identical rolls and identical body sequences; no settlement
contributes more residents than its population, and no attractor more members
than its headcount.

**M4 — nothing coarse moves.** The world fixture, every almanac, every
laboratory artifact and the whole Domesday survey are byte-identical. The
positive control fired as preregistered: the session snapshot goldens, the
game client's fixtures and the gallery's possession transcripts all **did**
move — `sensed.present` from 0 to 67 on the seed-42 turn-0 fixture, the social
channel from 6 to 67, the self channel unchanged. A null with no control is not
a result. The census is expected to move zero columns; the refresh is queued at
close and its result is recorded in the campaign's ledger rather than claimed
here.

**M5 — individuation is visible.** Every resident draws its deliberation,
its time horizon and its boldness from its kind's existing dispersion, by the
same mechanism that already draws a settlement's disposition around its
people's mean. Whether that is enough to *see* was left open, with a null
declared a finding in advance. Measured: **16 of 64 seeds** show two residents
of the home settlement standing in different rooms by the end of day 3. The
null did not occur. What the count establishes is separation, not attribution —
it is not a controlled comparison against an unperturbed roll, and it should
not be read as one.

## The tick cost was already a constant

The registry row that carried this problem quoted a superlinearity of 2.17
across 100–200 agents, and that figure was stale before this campaign opened.
Post-Scour the same instrument reads a fitted slope of ~1.1 — the cost is
essentially a **constant per body per tick**, about 8.76 ms at 200 agents and
5.75 at 10, which puts roughly a hundred to a hundred and seventy bodies inside
a one-second tick before anything is optimised.

That is the whole reason the roll is the answer and turning the dial is not.
Seven thousand inhabitants at a constant cost is about sixty-five seconds a
tick; a five-fold cut still leaves thirteen. Nothing makes the world affordable
by making each body cheaper. What makes it affordable is ticking sixty-eight of
them instead of seven thousand — and the plan's own stopping rule then fired
against itself. Five optimisations had been named; the first alone (one
population walk per wait instead of two, its facts committed in place, which
also stops the tick cloning the ledger) took a wait from 157.2 ms to 83.5 ms,
and the budget was met four levers early. The remaining four are recorded with
their measured shares — two of them frankly *unmeasured*, because no instrument
in the tree isolates them — rather than done for completeness.

## What sixty-seven people commit

A ceiling test in the vessel had pinned the tick's fact rate to a specific
story: at seed 42 one wild rust monster oscillated between two drives 43 times
in 40 ticks and carried 52.5% of the commits in the second half. On the roll,
that premise is simply gone — the rust monster is not within call.

What replaced it is worth reporting as a finding rather than a rebaseline.
Re-measured at the flagship roll: **no limit cycle at all.** The loudest single
subject accounts for 20 facts of 1,319 in the last half — 1.5% — and all 67
of the driven body's neighbours contribute, at 19 to 20 facts each, within one
fact of one another (67, not 68: the battery counts the bodies the walk
advances, which is the roll minus the one the player is driving).
Zero fear-tagged and zero belonging-tagged facts appear anywhere in the run.
The commit load that one creature's pathology used to carry is now the settled
roster's own even churn of thirst, hunger and sleep. The pathology is not
fixed; its carrier has left the roll.

## Honest limits

**A dormant body is frozen, not slowed.** A resident off the roll is not
ticked at all: its state is whatever facts it has committed, and it is replayed
forward when it returns. A villager you leave for thirty days is, on your
return, where they were thirty days ago plus a bounded catch-up. The middle of
that spectrum — one decision per day while off the roll — is named in the
frontier registry and deliberately not built.

**A crowd indoors is sensed and not drawn.** A chamber seats one body per
anchor cell; sixty-seven neighbours in one settlement room means most of them
are present, examinable and unseated. That is what already happened to a
surplus, and the presence line says so in words rather than letting the count
and the picture disagree. The real fix is a settlement wider than one room and
one structure, which is out of scope and is now its own registry row with these
numbers attached.

**The budget has never bound.** 68 against 128 at seed 42 means the truncation
path has no live witness, only a synthetic one.

**Seed 42's own walk band is static**, and it is the most instructive limit
here. The flagship condenses onto fresh water, so its residents drink where
they stand and commit no position at all — which means that at this seed
"frozen" and "ticked" have identical observable consequences, and a test meant
to prove dormancy passed against code that had dormancy removed. Four tests
moved to seeds whose residents actually walk. A world can be quietly incapable
of exhibiting the thing you are trying to measure.

**The key is still on the floor.** The row that opened this campaign ends
*"whoever raises it: a key waits to move onto a person"*, and this campaign
raises it and leaves the key exactly where it is. A resident can hold the
loomroom key today — the custody mechanism takes any entity — but no verb
transfers a held thing between creatures and no drive makes anyone set one
down, so moving it would make the only lock in the game unopenable. A
capability nothing can reach is not a capability (decision 0398). The person
now exists; the transfer does not.
