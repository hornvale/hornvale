# The Shoal

The world has modelled coral reefs, kelp forests, hydrothermal vents, upwelling
zones and five distinct depths of open ocean for a long time. Asked what any of
them looked like, it said:

> broken terrain

Twelve of the twenty-two biomes rendered through a two-item catch-all —
`broken terrain` or `unremarkable ground` — and since roughly two thirds of a
world is sea, that came to about **79% of any honest sample of the globe**.

It was worse than blank, because the habitat clauses were applied on top
regardless of where they landed:

> `hydrothermal vent — broken terrain sun-warmed damp on a rise`
> `kelp forest — unremarkable ground shaded dry in a hollow`

A black smoker two and a half kilometres down, in permanent darkness, described
as sun-warmed. A kelp forest described as dry.

## What The Formations bought

The fix is mostly a table of words, and the interesting part is why it is
*only* a table of words.

Before the biome taxonomy was disentangled, "open water" and "how deep" were
the same field. A pool keyed on that field could say one thing about the sea,
and the sea is not one thing. Now that formation and stratum are separate, the
open-water arm can be read by its depth:

```
epipelagic    open blue water / a drifting sargassum mat / a shoal turning as one
mesopelagic   the twilight water / a scattering layer, rising
bathypelagic  the lightless water / marine snow, drifting down
abyssal       the abyssal plain / a field of manganese nodules
hadal         the trench wall / the trench floor
```

One formation, five voices. That sentence could not have been written a
campaign ago — not because the words were hard, but because there was nowhere
to hang them.

## Reading the field in the water's own terms

The habitat clause was the other half. A room carries a micro-field — relief,
aspect, wetness — and the grammar had exactly one way to say it, the way you
would describe a hillside.

Underwater the same three numbers still mean something; they just do not mean
*that*. Relief is the floor beneath you rather than the ground underfoot.
Aspect is light, and only where light arrives — below the sunlit water nothing
is sun-warmed or shaded, it is simply dark, so the clause is dropped rather
than reworded. And wetness, which is meaningless in the sea, becomes the set of
the current.

The results read like places:

> a spur-and-groove channel sunlit swept by a current
> a chimney field swept by a current over a trough
> marine snow, drifting down in slack water over a seamount
> a pressure ridge in slack water over a seamount

## Only the sea moved

The campaign's other discipline was to leave the land exactly where it was.
Every land formation kept its pool *and its weights*, so its draws are
unchanged, and the two land biomes that had been falling through the catch-all
— `Ice` and `Shrubland` — were the only ones allowed to move. Seed 42's
flagship sits in tropical seasonal forest, which made the published possession
transcripts a free regression test on that claim.

The re-key that preceded the authoring was verified the same way, and more
strictly: after switching the grammar from the old flat enum to the faceted
expression, regenerating every artifact in the book produced **zero** drift. A
mis-mapped arm would have shown up there before a single new word was written.

No world byte moved either, and could not have: the composition root does not
import the locale window at all, so room prose is never a genesis input.

## The gap that hid the gap

The most useful thing the campaign found was not in the code.

After fixing 79% of the world's prose, regenerating every committed artifact
produced no diff whatsoever. Not a small one — none. **No page in the book
sampled a marine room.** The published gallery showed almanacs, possessions,
maps, strange sites, and a stratigraphic column, all of them on land.

So the defect had been invisible not because it was subtle but because nothing
looked. A reader of the book could not have found it; a drift check could not
have found it; only running `locale --sample` and reading sixty lines could.
This chronicle's companion page — *The Look of the World* — exists to close
that hole, and it is deliberately dominated by ocean, because any honest sample
of a world is.
