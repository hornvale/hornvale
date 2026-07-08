# Perception

**Questions it answers:** Two peoples stand under the same sky. Why don't
they see the same sky?

Every phenomenon the trace protocol offers up — the sun, the moons, the
night-stars, the felt weight of the seasons — is, as of Campaign Y2-1, read
identically by every observer. Salience is a property of the sky, not of the
eye looking at it, which is fine for one people and false for two. A goblin
and a kobold standing in the same valley under the same heavens do not
notice the same things first, and the frontier map is blunt about the
stakes: give the sky an observer-independent
salience and the world can only ever grow one religion, however many
peoples it places, because "what is most noticed" was never a question with
room for a second answer. Campaign Y2-2 makes salience a function of the
observer, so that a goblin's sun-headed pantheon and a kobold's moon-headed
one can stand side by side, in the same valley, under the same seed —
neither one wrong, each an honest reading of what its species actually
sees.

**Two knobs, each with one job.** Perception splits into a question of
*what is seen* and a question of *what demands attention*, and the design
keeps them strictly apart rather than folding them into one combined
adjustment. **The characteristic hour** decides what is seen: it is the
moment each species observes the sky, and a night-stars phenomenon is only
ever emitted when the observer is actually standing in the dark. No amount
of downstream reweighting can conjure a phenomenon that was never observed
— a nocturnal species does not get to notice a star it never looked up to
see; it has to actually be night where it's looking. **The lens** decides
what demands attention: given the phenomena a species *did* see, the lens
reweights how salient each one is to that species' eyes, so the same night
sky can crown a different phenomenon as most noticeable for a different
observer. Keeping the hour and the lens as separate mechanisms, rather than
merging "what was seen" and "how much it matters" into a single pass, is
what lets each stay simple enough to reason about on its own: the hour is a
question about a calendar, the lens is a question about a species.

**Venue is character, not cause.** The trace protocol's central promise
survives this campaign untouched: consumers of a phenomenon still never
learn which system produced it. What a phenomenon may now carry, alongside
its kind, its period, and its salience, is *where it lives* — the day sky,
the night sky, or the ambient world around the observer at all times. This
is not a backdoor letting religion peek at astronomy; it is character of
exactly the same kind `period_days` already is. A phenomenon's period tells
a consumer whether to expect a cycle; its venue tells a consumer whether the
thing lives overhead by day, overhead by night, or everywhere at once — both
are facts about the phenomenon itself, declared by the one system honest
enough to know them, its producer. The sun declares itself day-sky whether
it is spinning across the day or fixed forever above a locked world's day
side; the moons and the night-stars declare themselves night-sky; the felt
turn of the seasons and the ambient air declare themselves ambient, present
but never watched. A producer that adds a new phenomenon adds one venue
declaration to go with it — no other system is edited to accommodate a new
kind of sky.

**The closed-at-three vector, and why it stops there.** A species'
perception is authored as three dimensions, no more: an **activity
cycle** — diurnal, nocturnal, or crepuscular — that fixes the characteristic
hour; a **night vision** scalar in `[0, 1]` that says how much the dark
rewards attention; and a **sky attention** scalar in `[0, 1]` that says how
much of a species' notice, day or night, goes upward at all rather than to
the ground underfoot. This is the same ontology-trap posture the species
vector itself was built under, restated for a second closed space: no
spectral response curves, no modeling of hearing or smell, no
per-individual variation within a species. A goblin sees a goblin's sky and
so does every other goblin; widening this vector — adding a fourth
dimension, letting two goblins perceive differently from one another — is
real design work with its own tradeoffs to weigh, and it waits for a
campaign willing to weigh them, not a quiet addition to a vector that
happened to have room.

**The lens derivation, and identity at the goblin baseline.** Each
species' lens is three multiplicative weights, one per venue, built from
its perception vector by a formula chosen the same way every Year-2 formula
has been chosen: written so that the goblin baseline reproduces today's
behavior by construction, not by being tuned afterward to match it. Writing
`σ` for sky attention and `ν` for night vision:

```text
day_sky   = activity_factor · (0.5 + σ)
night_sky = (0.5 + ν) · (0.5 + σ)
ambient   = 1.5 − σ
```

where the activity factor is a fixed constant per activity cycle — 1.0 for
diurnal, 0.7 for crepuscular, 0.4 for nocturnal — read off a species'
enumeration, not authored per species alongside the scalars. Goblin's
perception vector is `(Diurnal, 0.5, 0.5)`, and every one of those three
expressions was written to reduce to exactly `1.0` at goblin's values: the
activity factor is 1.0 for diurnal, `0.5 + σ` is `1.0` at `σ = 0.5`, and
`1.5 − σ` is `1.0` at the same point. The goblin lens is therefore
`(1.0, 1.0, 1.0)` — the identity lens — and it is identity in the strongest
sense available to a deterministic simulation: the code path that applies
it performs no multiplication, no clamping, no rounding at all, so a
goblin-only world's observed phenomena are byte-identical to a world
generated before perception existed, not merely numerically equal to enough
decimal places to look that way. Every formula above is also monotone per
dimension, the same discipline the species vector's own formulas keep:
raise sky attention and both `day_sky` and `night_sky` rise while `ambient`
falls, precisely the trade a creature that watches the sky more should make
against noticing what's underfoot.

**The model card.** Kobold's three values, like its six species-vector
values before them, are not measured or drawn — they are read out of the
Dungeons & Dragons 5th Edition System Reference Document, the project's
standing authoring corpus, translated by hand into this campaign's
dimensions.

*Kobold perception values in this chapter are derived from the Dungeons &
Dragons 5th Edition System Reference Document 5.1, available under the
Creative Commons Attribution 4.0 International License (CC-BY-4.0). No SRD
text is reproduced here — only parameter derivations, paraphrased to one
line each.*

| Dimension | Type | Goblin (baseline) | Kobold | 5E derivation |
|---|---|---|---|---|
| Activity cycle (diurnal, nocturnal, or crepuscular) | enum | Diurnal | Nocturnal | darkvision 60 ft, sunlight sensitivity |
| Night vision | scalar `[0,1]` | 0.5 | 0.9 | darkvision; a life underground |
| Sky attention | scalar `[0,1]` | 0.5 | 0.8 | omen-readers and dragon-watchers; the warren's night is spent under the open sky |

Every dimension in this table is **authored**, for both peoples, the same
posture the species model card kept: nothing here is drawn or fit. Kobold's
resulting lens is `(0.52, 1.82, 0.70)` — night vision and sky attention
together make the night sky weigh more to a kobold's eyes than the day sky
ever does to a goblin's, which is the whole design intent stated as a
number: the moons and the night-stars, not the sun, are what a kobold
pantheon will crown. `Crepuscular` is part of the closed vocabulary this
vector is bound to but is claimed by no species this campaign, and rather
than leave it undefined until some future people needs it, its activity
factor (0.7) is authored now and the enumeration variant declared idle in
the model card — the same treatment the species chapter gave deliberation
latency, a dimension banked ahead of the rule that will consume it. A
future crepuscular people is then a data change, not a code change: the
formula above already knows what to do with it.

**The skies ahead:** religion run through each species' own eyes at its own
hour, so the sky each pantheon mythologizes is the sky its people actually
watched, and `why` recounting a head deity not only to its source phenomenon
but to the perception facts that made that phenomenon the one worth
noticing; sky-report prose rendered in each species' own voice, once a
generated tongue exists to speak it; terrestrial phenomena rich enough that
a deep-dwelling people could raise an earthy pantheon — the reading set
aside for the kobold, kept open for a people after it; comparative studies
over the two pantheons a shared valley can now hold; and, if ever, the
perception tiers this vector deliberately excludes — spectral response,
hearing, smell, per-individual variation — each real work for a later
campaign, none a quiet widening of a three-dimensional vector.
