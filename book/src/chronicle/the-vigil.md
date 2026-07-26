# The Vigil

A dragon has always had words. Since [The Solitary Tongue](./the-solitary-tongue.md)
the three chromatics have spoken a frozen Draconic — a language that barely
drifts, because drift is a function of how many mouths carry a word and how
long each one lives, and a solitary centenarian is the limiting case of few
mouths and long lives. What the dragon did not have was eyes.

This mattered more than it sounds, because of what a dragon's vocabulary is
made of. A settling people learns most of its words from its circumstances: the
biomes it lives in, the neighbours it meets, the hearth and the god and the
home it keeps. A dragon settles nowhere and meets no one, so all of that falls
away, and what remains is the part of language that comes from the body — the
acquisition ladders, where how finely a creature divides the visible world
decides how many words it has for colour and for darkness. For a dragon, almost
the whole lexicon is perceptual. Its language is nearly all eye.

So when the eye was missing, something had to stand in its place, and what
stood there was a goblin's. A hardcoded fallback — diurnal, middling night
vision, middling attention to the sky — fed the classifier whenever a speaker
turned up with no perception of its own. It was written as a stopgap and
labelled honestly as one. But it was never confined to the code: it ran all the
way out to the published dictionary, where each dragon's missing words were
explained, in print, by *"night-vision 0.5"* — the goblin's number, given as a
fact about dragons. Until this campaign, "Draconic" was goblin perception
wearing draconic phonology.

## One clade eye, three schedules

The perception vector has three dimensions, and they are not the same kind of
thing. Night vision is an *organ*. Activity cycle is a *schedule*. Sky
attention is an *allocation*. Only the first reaches language: it alone drives
the acquisition ladders, while the other two shape which phenomena a creature
notices and when it is awake to notice them.

That asymmetry decides the authoring. Night vision is written once, for the
whole draconic clade, because a per-dragon value would hand each dragon its own
inventory of colour words and quietly break the shared tongue — the cognate
tables admit only what every daughter still roots. The other two vary by kind,
and are read off something already written down: each dragon's authored
tolerance for sunlight. The red dragon keeps to open volcanic country under a
high sun, and is diurnal. The black dragon hunts a shaded lowland swamp, and is
nocturnal. The white dragon lives in a polar twilight that is neither, and is
crepuscular — the first creature in the world to occupy that third case, which
had been a legal value with no holder since the day it was defined.

Sky attention runs low across all three, which corrects an intuition worth
naming because it is so easy to have. A dragon flies, and so it is tempting to
call it sky-attentive. But the dimension does not mean *airborne*; it means
*celestial rather than terrestrial* — and it trades directly against noticing
the ground. A dragon on the wing is looking down. Flight is a fact about
vantage, and the vector has no dimension for vantage; borrowing the celestial
one to express it would have put a true thing in a false slot.

## What the eye turned out to say

The acquisition ladder for colour follows the order human languages actually
acquire colour in: dark and light first, then red, then green and yellow, then
blue, then brown. A keen-eyed creature sits *low* on that ladder — an eye that
has spent its history in the dark has spent less of it dividing daylight hues.
At the value the draconic clade received, the ladder stops after red.

The three chromatic dragons are white, black, and red. Their tongue lexicalizes
exactly three hues: light, dark, red. The dragons have words for the colours
they *are*, and for nothing else — while the same eye opens the whole ladder of
darkness beneath, where gloom and shadow and starlight become separate words
for a creature that can tell them apart.

This was written down as a prediction before the dictionary was regenerated,
and it held. It is worth being precise about what kind of result that is: the
ladder was not changed, and the kobold already sat at the same depth, so no
mechanism was discovered here. What the campaign did was let an eye be
authored on its own terms and then find that the language followed. The
consequence was not tuned toward; it was checked for.

## The chain, and the fork it replaced

Underneath the authoring lay a quieter defect. When capacities were separated
into a lattice, the design was described as a chain — speech presupposes
perception, perception presupposes a mind — but what got enforced was a fork:
speech under mind, perception under mind, and no edge between them. The
difference was invisible while no creature stood in the gap. Then the dragons
were given speech without eyes, which the fork permits and the chain forbids,
and the goblin-baseline stopgap is precisely what kept that arrangement
working.

The chain is now true, so it is now enforced, and the stopgap is gone. In its
place a missing perception fails loudly, matching what the religion code had
been doing for the same component all along. The payoff is not for the three
dragons already written; it is for the metallics and further chromatics still
to come. A speaking kind authored without eyes now fails at load, instead of
silently speaking with a goblin's.

## Facts a creature has, versus places it lives

One thing more came loose. Every fact describing a kind's mind, senses, and
speech was published to the ledger behind a single gate that asked whether the
kind *settles*. For four settling peoples the gate was invisible. For a dragon
it withheld everything — not because a dragon lacks a mind or a voice or eyes,
but because it lacks an address.

This was the same confusion a previous campaign had already corrected once, for
society, and left standing in the other three families: sedentism standing in
for something it merely correlates with. Each family is now gated on the
component that produces it. The dragons' minds, their speech, and their new
perception all reach the ledger; their society facts remain absent, and now for
the right reason — a dragon keeps no society, rather than failing to settle.

The commit order was left exactly as it was, down to one society fact sitting
between two mind facts, because emission order is part of the save format. The
result is that the four peoples' worlds are unchanged fact for fact, and the
whole difference a world shows is thirty-nine new facts: three dragons, thirteen
each, and not one existing line altered. A dragon that had a mind for three
campaigns, and a voice for two, is finally written down as having them.
