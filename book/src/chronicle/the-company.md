# The Company

*A company is the people who happen to be in a room together, and — in the
older theatrical sense — the ones who can therefore play a scene. This
campaign is about both meanings, and about how rarely the first one holds.*

The Repertory left `the-orange` standing red and named its second beat — *the
goblin holds an orange* — as the next campaign's target. That beat is now on
the wire. The campaign's finding is not the beat.

## The first beat had been passing for the wrong reason

`the-orange`'s beat 1 reads *"two creatures share a room, and the possessing
will can tell"*, and it asserted `/social/0/label`. That assertion never tested
co-location. `SocialEntry`'s own doc states the channel's contract without
euphemism:

> membership is world truth ... a consumer must filter it ... rendering it
> unfiltered ships a cheat pane

`social` lists every derived body in the world. Measured across twelve seeds
and both target selectors:

```
  witnesses with a non-empty `social`            24 of 24
  witnesses with a non-empty `sensed.present`     0 of 24
  the same after wait 1, 5, 20 and 60 at seed 42  still 0
```

So the beat passed whenever any NPC existed anywhere, which is always. It is
the *narrower question than the claim* shape: a real assertion, correctly
evaluated, answering less than the sentence attached to it.

**And the measurement was not new.** The doc comment on
`Session::place_creature_at_me` already recorded it — *"seed 42's flagship
possession still finds nobody to provoke after sixty `wait`s"* — with a
registry row, `SOC-one-creature-per-settlement`, and a ruling: a settlement
holds exactly one derived creature, and changing world population to restore an
incidental co-location guarantee is its own campaign, because tick cost was
measured superlinear in that dimension. The independent reproduction is
reassuring about the instrument and says nothing new about the world. It is
recorded here in that order deliberately.

## Where custody belongs, and why it is not a choice

`carrying` joins `PresentEntry`, beside `felt`, and the argument was already
written in the code:

> `felt` — a presence-gated read of another creature's interior, **which is
> why it lives here and not in `social`**

Custody is the same kind of fact: you learn it by standing in the room and
looking. On `social` the same field would hand a possession that has never met
a creature the contents of its hands — where a key is, what a merchant carries
— which is a materially more exploitable disclosure than a mood, and would
deepen a cheat the code already names.

`carried()` became `carried_by(holder)` with `carried` delegating, so the rule
in its own comment survives having a second caller: there is still exactly one
fold for a pane and a verb to disagree about.

## A witness is a query

The Repertory recorded a witness as a concrete world. That was wrong, and the
argument that shows it is one the corpus was already making about something
else.

A pinned entity id is lineage-derived, so it moves the first time derivation
changes — and it moves **silently**, resolving to some other creature or to a
refusal rather than to an error that names the cause. Repertory assertions are
structural and never golden strings for exactly that reason. A pinned id is a
golden string wearing a witness's clothes.

So a scene records a **selector**, and the resolved identity is an observation
the run reports rather than the thing that drives it. A pin survives for the
case it is right for: a scene whose point *is* a particular world, which is a
regression check and not a capability one. The distinction is old — grounded
versus existential — and it is the same one that separates *does this world
still do X* from *does some world do X*.

The search runs in-process, because a world build (~3.5 s) dominates and a
roster is small: build each world once, check every member against it. The
beats still run out-of-process, through the same `possess --script` the
instrument has always used. And a roster member `possess` refuses is a **miss**
rather than an error — seed 42 has one — because propagating it would let a
single unrelated refusal mask every world after it.

## UNWITNESSED

A sixth verdict, and the reason to want it is that it points somewhere else.

**ABSENT** says a capability is missing: the scene ran and a beat failed.
**UNWITNESSED** says the world never assembles the stage. Those are different
findings and they imply different work, and until now the instrument could only
say the first.

It carries the range it searched, so the claim is always *not within N seeds*
and never an unbounded negative. The test that pins it is bounded for the same
reason: proving a negative over the whole seed space is a hang, not a test.

## What `the-orange` is now blocked on

The scene split. `two-in-a-room` carries the mechanism and resolves
**AUTHORED** — at seed 42 the pair that satisfies it is a wild otyugh and a
wild carrion-crawler, which is itself the shape of the finding: settlements are
one-creature by construction, so the only rooms holding two bodies are wild
concentrations.

`the-orange` keeps the species constraint and resolves **UNWITNESSED**. That is
a better statement of its blocker than anything it has carried before. It is no
longer blocked on the program: The Chattel put carried things on the wire and
this campaign put the co-located half beside them. It is blocked on the world
declining to put a drow and a goblin in a room — a claim about Hornvale rather
than about Hornvale's source, and one an existing ruling has already priced.

Beyond the stage, the beats past the second still want the thing no verb does:
address another creature.

## Cost

The corpus now resolves in 70.3 s against The Repertory's 43.6 s. The search is
the entire increment, and it buys the difference between a witness that rots
and one that re-resolves.
