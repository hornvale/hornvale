# The Gods of Seed 42

This is the Year-1 exit demo (spec §15) — the single comparison the whole
cascade was built to make legible. Seed 42's land and society are identical
in both worlds below: the same tectonic globe, the same flagship settlement,
the same emergent caste structure, an organized priesthood tending a
stratified town. The only thing that differs between them is the sky —
spinning in one, tidally locked in the other — and that one upstream change
is the entire explanation for why one world worships a god who leaves and
returns, and the other worships a god who never moves at all.

Both pantheons are ranked (the flagship's structure reaches four castes in
both worlds) and both are organized (a shaman tends both cults), so the
*shape* of the two religions is identical — the same verticality, the same
priesthood. What differs is exactly what astronomy differs on: the head
deity's tenet, and nothing else. The two minor deities, the Tidewalker and
the Waning Herald, are untouched by rotation and appear unchanged in both
pantheons — only the phenomenon at the top of the salience order, the sun
itself, is rewritten by the sky it comes from.

## Spinning: the Wheel-Turner presides

Quoted verbatim from the committed [`almanac-seed-42-sky.md`](./almanac-seed-42-sky.md),
the "The Gods" section of the spinning-sky almanac:

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

Quoted verbatim from the committed [`almanac-seed-42-locked.md`](./almanac-seed-42-locked.md),
the "The Gods" section of the locked-sky almanac:

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

Nothing about religion's code changed between these two worlds. The same
`domains/religion` genesis, reading the same salience-ranked phenomenon
list and the same `SocietySummary`, produced two different pantheons
because the phenomena it was handed differed — which is the entire claim of
the trace protocol (spec §3.1.6): religion never learns which system
produced a phenomenon, so it does not need to know astronomy exists to
mythologize astronomy's output correctly. Ask each world's high god *why*
it holds its tenet — `why <id>` in the REPL, backed by
`windows/historiography::recount` — and the two answers cite the same
predicate vocabulary, the same provenance discipline, and genuinely
different reasons: one points at a phenomenon that departs and returns,
the other at one that never has and never will.

This is the composition of five campaigns' worth of already-working parts,
not a new invention: astronomy drew the rotation regime, climate
reorganized its bands around it, terrain's globe never moved, settlement
and culture read an environment the sky reshaped, and religion mythologized
whatever phenomena were left standing at the top of the list. See
[Religion](../domains/religion.md) for the tier-1 model card, [Study 004,
the Census of Faiths](../laboratory/study-004.md) for the two calibrations
this pair exemplifies at the scale of ten thousand worlds, and [Campaign 5,
The Gods](../chronicle/campaign-5.md) for the full retrospective this
capstone closes.
