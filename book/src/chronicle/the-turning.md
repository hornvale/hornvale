# The Turning

The climate could tell you the average warmth of a place and how its
seasons swung, but not that a desert bakes by afternoon and freezes by
dawn. The temperature field was a function of the year and never of the
day. The Turning gives it the day: a diurnal term, the first campaign of a
longer program to make the world's weather move — and the sibling of The
Wandering Sun, which taught the same field to follow the *year*'s slow
axis. This one follows the fast axis, the world's own turning.

## A third term that sums to nothing

Temperature already carried two terms, a per-cell annual mean and a
hemisphere-signed seasonal anomaly. The Turning adds a third:

```
T(cell, day) = mean(cell) + seasonal(cell, day) + diurnal(cell, day)
```

The diurnal term is the product of a per-cell **amplitude** — how far a
place swings between noon and midnight, large where the air is dry and the
ground continental, near zero over the thermally patient ocean — and a
**waveform** that carries the phase. The single property that makes the
whole thing safe is that the waveform is **zero-mean over one rotation**:
averaged across a day it cancels exactly, so the annual mean is untouched.
The census reads that mean; biomes classify on it; the frozen worlds are
byte-identical to the ones before. A term that adds a day/night cycle to
every rendered temperature while moving not one committed byte is the kind
of change the layering is built to allow — the amplitude ships as one new
`scene/tiles/v1` field, the waveform is re-derived by whoever reads it, and
nothing in the ledger stirs.

## The bug the arithmetic could not see

The waveform's first draft was wrong, and wrong in a way five task reviews
looked straight past. Each reviewer checked that the code matched the
specification's formula, and it did — but the formula keyed the day/night
phase to the *global* fraction of the day, with no dependence on where a
place sat. Every longitude shared one clock. The whole planet warmed at
noon and cooled at midnight in unison, as if the entire globe faced the sun
at once. It is a coherent function; it passes a zero-mean test; it is not a
day/night cycle. A day/night cycle is *local*: at any instant half the
world is lit and half is dark, and the warm band sweeps across the surface
as the world turns beneath a fixed sun.

Nothing in the arithmetic betrayed the error, because the arithmetic was
self-consistent. What betrayed it was a screenshot. Run forward on the
globe, the temperature lens pulsed the *entire* visible hemisphere warm and
then cool together — and the honest caption a builder had written said so
in as many words. The fix was to phase the waveform on **local solar
time**, `frac(day_fraction + longitude/360)`, so a place's warmth depends
on its own meridian's hour. The invariant survived — each cell still sweeps
every local hour over a rotation, so each cell's day still sums to zero and
the mean still holds — and the picture came right: a warm afternoon band
trailing the sub-solar point, a cool pre-dawn band ahead of it, both
sweeping as the world turns.

## Watching it turn

The instrument is the temperature lens under a new "watch a day" hold, the
diurnal twin of The Wandering Sun's "watch a year". Where that one freezes
the daily spin to let a year's seasons run visible, this one holds the
season still and lets a single day run: the terminator sweeps, the lit face
warms into its afternoon, the night face slides toward its dawn, and the
dry interiors flush hot and cold while the oceans hold their breath. The
two holds are opposites and cannot both run at once, so choosing one
releases the other. The world that could only be read a season at a time
can now be read an hour at a time — and the next campaigns, the winds and
the currents and the rain, will have a day to move within.
