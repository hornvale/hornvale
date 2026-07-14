# 0054. No semi-major-axis migration

**Status:** Accepted (2026-07-14) · **Decider:** Nathan

In the context of SKY-1's orbital mechanics, facing a choice between modeling
disk-driven or secular semi-major-axis migration alongside stellar
brightening, we decided **migration is declined** — disk-driven migration ends
before a stable habitable world exists, and post-genesis secular drift of an
isolated planet's semi-major axis is effectively zero on every timescale the
sim can express.

**Context.** SKY-1's residual left semi-major-axis migration as an option
alongside stellar brightening. Adding it would be invention, not derivation —
the "guess wearing physics' clothes" failure mode. Tidal orbital decay is
real physics but belongs to SKY-tidal-braking (a separate concern covering
day-lengthening, lunar recession, and the death of total eclipses).

**Consequence.** Stellar brightening ships in star.rs (`luminosity_at`); the
anchor's orbital elements beyond the Milankovitch triad stay fixed. Supersede
with new information, not a fresh opinion.
