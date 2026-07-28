# The Occlusion

Seed 42's almanac opened, for most of this project's life, with a sentence
that could not be true:

> Night. The vast moon shows its waning crescent face. The small, distant moon
> shows its first-quarter face. Above, the stars keep their stations: one
> smoldering red, one warm yellow, one deep orange, one dim red, one pale
> white. **The sky is a flat overcast.**

Five stars, catalogued by colour, beneath a stratus deck. The world knew it
was overcast — it said so in the same breath — and enumerated the sky anyway.

## The occluder that was already there

The fix turned on noticing that the codebase had already built one occluder
and never called it that. `Venue::DaySky` and `Venue::NightSky` are not merely
labels for where a phenomenon lives; they are an occlusion partition. Daylight
hides the stars, and the venue split is how the system says so. The sun had
been occluding the sky correctly since the beginning.

Cloud is the *second* occluder. It was simply never built.

Once framed that way the mechanism was already in place. `PerceptionLens` is a
multiplicative per-venue weight, and `observe` was written to skip all
arithmetic when that lens is the identity — a byte-level no-op, documented as
such. Weather becomes a lens, the lens is exactly the identity under a clear
sky *by construction*, and an unclouded world is untouched down to the byte.
That last discipline was not invented here either: `perception_lens` had
carried the same guarantee at the goblin baseline for several campaigns.

The one design choice worth recording is that occlusion is **graded, with a
floor**, rather than binary. The atmosphere is not a shutter. A vast moon
behind an overcast is still a glow; a dim red star is simply gone. So the lens
attenuates, and anything falling below a visibility floor is *culled* rather
than demoted — because a star dimmed to a fiftieth is not a faint star, it is
a star you cannot see. Different weather then produces genuinely different
skies from one multiplication and one comparison: cirrus keeps nearly
everything, an overcast keeps the moons as smears, a storm keeps nothing.

The thresholds live in `domains/astronomy`, which receives an abstract
`Visibility` ratio and never learns what dimmed the view. That placement is
not squeamishness about layering. Only astronomy knows which bodies are
bright, and "what survives a dimmed sky" is exactly that question.

Seed 42's day-0 sky now reads:

> Night. The vast moon is a smear of light. The small, distant moon is a smear
> of light. The sky is a flat overcast.

## What the sky is worth

Attenuating the celestial venues raises Ambient's *relative* rank for free,
because the salience sort was already there. The effect on the almanac is the
most pleasing thing this campaign produced:

| phenomenon | clear | overcast |
|---|---|---|
| the tide, under the vast moon | 0.50 | **0.68** |
| a vast moon | 0.64 | 0.19 |
| five neighbour stars | 0.10–0.11 | *gone* |

Under a deck you feel the tide, not the stars. Nobody wrote that rule; it
falls out of ranking a weighted list.

## Where occlusion must not go

Wiring the lens into the observation path changed the world's bytes.

Seed 42 fell from 7,350 facts to 7,126, and its pantheon from **48 deities to
25**.

The campaign's spec had asserted — and the author had verified, by checking
that `SkyReport` carries no `Serialize` — that phenomena are a read rather
than committed state, and therefore owed no epoch. The check was sound and the
conclusion was wrong, because the exposure does not run through serialization.
It runs through genesis. `derived-from-phenomenon` is a *committed predicate*:
a people's gods are derived from the sky they observe, and their settlements
are named for it. Culling the faint night-stars under one overcast morning
destroyed twenty-three deities.

The resolution is a distinction the spec had not drawn. Occlusion is a
property of a **moment's viewing**, not of the world. It belongs on the
reading paths — the almanac's salience list, the walker's sky — and nowhere
near the path that *authors* the world. The observation primitive stays
unoccluded; a separate accessor serves presentation.

This is better physics as well as safer bytes. A culture's pantheon forms over
generations. It should not turn on whether one morning happened to be cloudy.

Half of that bug was caught by a test written by an earlier campaign, which
pins a goblin's identity-lensed observation to the unlensed path bytewise.
Fixing genesis alone left it red, and it was that stubbornness which forced
the clean split rather than a patch over the symptom.

## Three smaller repairs

**The sky belongs to the observer.** `sky_report_from` resolved the flagship
settlement's cell and read the weather there, falling back to cell 0 for a
settlement-less world — so a walker a thousand miles from the capital got the
capital's sky, forever. The observer's cell is now a parameter, and *nowhere
in particular* honestly has no weather at all rather than silently borrowing
cell 0's. Weather had always varied correctly in time; only place was pinned.

**The room's own exits.** Possession printed `Ways on: SE, N, SW.` and then
answered `No verb 'se'`. `parse_compass` had accepted `se`, `southeast`, `n`,
and `north` all along; the verb dispatch simply never reached it. One arm.
The first token anyone types into a text world is a bare direction.

**A hundred and one invisible wonders.** Seed 42 places 101 exotic sites, and
nothing could find them: a random `locale --sample` essentially never lands on
one, because the rarity budget is doing its job. `locale --strange` lists them
— and, because the sites differ by negation vector, each row carries its own
descriptor rather than a bare coordinate.

That column earned itself immediately. Ninety-nine of the hundred and one are
fungal, and ninety-one sit in cold biomes. The candidate scores are not
commensurable: fungal scores `1.0 - unrest`, which is at least 0.6 on any
quiet land, while geothermal scores bare `unrest`, high only near plate
boundaries. Most land is quiet, so fungal wins nearly everywhere. A design
space of four kingdoms by three energy sources ships, in practice, as one
outcome. That is a variety bug rather than a findability one, and it is
recorded rather than fixed here — but it was invisible until something listed
the sites side by side.

## A note on `vessel/session/v1`

The session snapshot is save-format-class, and its golden moved. The reviewed
diff is exactly the `sky` string on four turns: same shape, same fields, same
grouping, same types. Under the epoch discipline — additive changes are free,
meaning changes mint `v2` — this was taken as a *value* correction under an
unchanged contract. The field always meant "the sky as this observer sees it".
It was simply wrong before.
