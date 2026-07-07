# The Gods of Seed 42

This is the Year-1 exit demo (spec §15) — the single comparison the whole
cascade was built to make legible. The version of this page that closed
Year 1 described the pair below as one settlement wearing two skies — the
same flagship, the same caste structure, differing only in which god
presided. The ledger was never quite that tidy (the two skies' climates had
already placed different flagships, a relocation Campaign 5's own
retrospective counted among the sky's consequences even as this page
glossed it), and Campaign Y2-0 (firm ground) made the divergence impossible
to gloss: the placement fix that made a coastal cell's `freshwater` score
honest (seawater is not freshwater; see [Campaign Y2-0's
chronicle](../chronicle/campaign-y2-0.md)) re-scored every settlement site
on the globe, moving the spinning world's flagship outright and re-weighing
the locked world's where it stood. So the spinning world crowns
**Grumoknar** and the locked world crowns **Bolugrak**, two different
villages, at two different scales, on two different corners of the same
globe — and this page now says so, because the divergence is itself the
finding, not a regression to apologize for. Astronomy was always going to
propagate through settlement, culture, and religion; it propagates through
*placement* too, one step earlier in the cascade than this page used to
show.

Once the two flagships are read as their own settlements rather than one
settlement wearing two skies, the comparison gets sharper, not weaker.
Everywhere the sky reaches, the two villages diverge: a different name, a
different population (359 against 522), a different neighborhood (58
settlements scattered across the spinning globe against 27 on the locked
one, the same terminator-driven collapse Campaign 3c's biome map predicted
and Campaign 4a's settlement scatter first measured), and — at the very top of each pantheon — a different presiding
god. Everywhere the sky does not reach, the two villages converge: both
happen to net out in temperate-rainforest and farm it, both grow the
identical five-rung ladder (slave, farmer, artisan, shaman, chief), both
raise an organized priesthood, and both venerate the same two minor deities,
the Tidewalker and the Waning Herald, right down to their periods in days.
The one axis that moves is exactly the axis astronomy controls: the head
deity's tenet. That is a stronger demonstration of the enrichment thesis
than the old, coincidental "same village" framing ever was — divergence
tracks the sky precisely, in both directions.

One honesty check on that convergence: the minor deities' sameness is
*structural* — a moon's orbital period is rotation-blind, so the Tidewalker
and the Waning Herald cannot help but be identical in every world seed 42
generates. The subsistence-and-structure convergence is not structural in
the same way; it is what *this* seed happens to produce, because both
flagships this time land in the same biome class. A different seed can and
does break it — seed 7, [the culture chapter](../domains/culture.md)'s own
exit demo, farms under both skies yet grows a three-rung ladder under one
and the full five-rung ladder under the other. Seed 42's contribution is
the sharper one: the sky-reaches/sky-doesn't-reach split, cashed out on a
single seed where the caste ladder holds still long enough to make the
head-deity contrast the whole story.

## Spinning: the Wheel-Turner presides

Quoted verbatim from the committed [`almanac-seed-42-sky.md`](./almanac-seed-42-sky.md):

```text
The land holds 58 settlement(s).
The chief settlement, Grumoknar, holds 359 souls amid temperate-rainforest.

The goblin village of **Grumoknar**, population 359.

Grumoknar lives by farming.
Its roles, lowest to highest: slave, farmer, artisan, shaman, chief.
```

The "The Gods" section of the same almanac:

```text
## The Gods

An organized priesthood tends a pantheon:

> the Wheel-Turner departs and returns every 0.88 days; its absences are mourned and its returns feasted. *(who presides)*
>
> — derived from the phenomenon *celestial-body*

> the Tidewalker departs and returns every 15.99 days; its absences are mourned and its returns feasted.
>
> — derived from the phenomenon *celestial-body*

> the Waning Herald departs and returns every 32.55 days; its absences are mourned and its returns feasted.
>
> — derived from the phenomenon *celestial-body*
```

A spinning world's sun rises and sets, so the phenomenon religion observes
is periodic, and the eternal-vs-cyclic template (tier 0's original
distinction, unchanged since Campaign 1b) mints a cyclic head deity: **the
Wheel-Turner**, mourned in its absence and feasted on its return, exactly
like the two lesser cyclic deities beneath it.

## Tidally locked: the Still Crown presides

Quoted verbatim from the committed [`almanac-seed-42-locked.md`](./almanac-seed-42-locked.md):

```text
The land holds 27 settlement(s).
The chief settlement, Bolugrak, holds 522 souls amid temperate-rainforest.

The goblin village of **Bolugrak**, population 522.

Bolugrak lives by farming.
Its roles, lowest to highest: slave, farmer, artisan, shaman, chief.
```

The "The Gods" section of the same almanac:

```text
## The Gods

An organized priesthood tends a pantheon:

> the Still Crown is a sun fixed forever above the day side; it has never departed and will never blink. *(who presides)*
>
> — derived from the phenomenon *celestial-body*

> the Tidewalker departs and returns every 15.99 days; its absences are mourned and its returns feasted.
>
> — derived from the phenomenon *celestial-body*

> the Waning Herald departs and returns every 32.55 days; its absences are mourned and its returns feasted.
>
> — derived from the phenomenon *celestial-body*
```

Pin the same seed's rotation to tidally locked and the sun stops setting.
The identical template, fed the identical phenomenon slot with a different
periodicity, mints an eternal head deity instead: **the Still Crown**, a
sun fixed forever above the day side — the frontier's alien-religion note
(a terminator culture's theology hanging on an unmoving sun), cashed. There
is no seasonal deity in this pantheon at all, because a tidally locked world
has no seasons to mythologize — a phenomenon that never existed cannot fail
to clear the pantheon floor.

## What this pair proves

Nothing about religion's code changed between these two worlds, and nothing
about settlement's or culture's code changed either. The same placement
suitability formula, reading the same corrected freshwater field, sent
Grumoknar and Bolugrak to different cells because the two skies' climates
disagreed about where the best cell was; the same `hornvale-culture`
functions, reading each flagship's own environment, happened to agree on
farming and the five-rung ladder; and the same `domains/religion` genesis,
reading each flagship's own salience-ranked phenomena and its own
`SocietySummary`, produced two different pantheons because the phenomena
and the society it was handed differed exactly where the sky differed —
which is the entire claim of the trace protocol (spec §3.1.6): religion
never learns which system produced a phenomenon, so it does not need to
know astronomy exists, or that settlement moved, to mythologize correctly
whatever it is handed. Ask each world's high god *why* it holds its tenet —
`why <id>` in the REPL, backed by `windows/historiography::recount` — and
the two answers cite the same predicate vocabulary, the same provenance
discipline, and genuinely different reasons: one points at a phenomenon
that departs and returns, the other at one that never has and never will.

This is the composition of five campaigns' worth of already-working parts,
not a new invention: astronomy drew the rotation regime, climate
reorganized its bands around it, terrain's globe never moved, settlement
placed a flagship the reorganized climate actually preferred, culture grew
a society from whatever environment that flagship landed in, and religion
mythologized whatever phenomena were left standing at the top of the list.
See [Religion](../domains/religion.md) for the tier-1 model card, [Study
004, the Census of Faiths](../laboratory/study-004.md) for the two
calibrations this pair exemplifies at the scale of ten thousand worlds,
[Campaign 5, The Gods](../chronicle/campaign-5.md) for the Year-1
retrospective this capstone originally closed, and [Campaign Y2-0, Firm
Ground](../chronicle/campaign-y2-0.md) for the placement fix that gave this
page its sharper, truer shape.
