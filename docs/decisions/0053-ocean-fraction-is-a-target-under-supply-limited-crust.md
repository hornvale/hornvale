# 0053. The ocean-fraction pin is a target, not a guarantee, under supply-limited crust

**Status:** Accepted (2026-07-14) · **Decider:** Nathan (route 1 of the single-craton-hypsometry spec)

In the context of single-continent worlds (`continents=1`), whose lone
craton clamps at 0.6 rad (~8.7% of the sphere — below any legal
ocean-fraction-implied land quota), we decided that **when a world's
analytic continental supply falls below `SUPPLY_SHORTFALL_FACTOR`
(0.5) times its land quota, sea level is placed at the isostatic shelf
break — `SHELF_BREAK_LAND_FACTOR × supply` land via the existing
exact-percentile mechanism — instead of forcing the quota by drowning
into the abyssal plain.** The softening is deterministic (pure
arithmetic over already-derived values, zero new stream draws) and
metered as a degradation note in genesis notes.

**Context.** The percentile sea level hits the quota *exactly* by
construction; on a supply-limited world that lands it deep in the
abyssal plain — a broad flat "land" with no shelf and no hypsometric
bimodality. Raising the 0.6 rad clamp was rejected (it exists for
directed lobing; Sculpting owns that redesign), as was weakening the
test (a continent's shelf is a real property). Supply is analytic and
grid-free, so coarse-constrains-fine holds exactly; default worlds
(8-14 cratons) sit far above the activation threshold — the separation
guard (`default_worlds_never_trip_the_supply_fallback`) proves the
fallback cannot rewrite them, so committed artifacts and the frozen
censuses are untouched.

**Consequence.** For supply-limited worlds (a lone craton always; small
pinned counts sometimes), the achieved ocean fraction reported by
`summarize` exceeds a pinned `--ocean-fraction` — the pin conditions the
craton budget as before but the quota itself is honored only up to
supply. Non-limited worlds are byte-identical to before this decision.

**Amendment (same date): the shelf test floor is land-normalized.** The
pre-registered κ calibration hit its STOP rule: no κ satisfies the
whole-sphere shelf floor (`shelf_fraction > 0.02` of all cells; best
18/40 at κ=2.0) because a ~3%-of-sphere continent cannot put 2% of the
sphere within ±200 m of sea level — while at κ = 1.0 the land-normalized
shelf (`shape::shelf_land_ratio`) spans 0.075–0.309 (median 0.171),
overlapping and at the median exceeding the default-world population's
0.097–0.165 (median 0.130), with D ≥ 3.14. Nathan ruled the metric's
denominator, not the world, wrong: the single-craton tests assert
`shelf_land_ratio > 0.05` plus the retained absolute
`shelf_fraction < 0.5` ceiling (which still guards the original
drowned-into-the-abyss failure), and `SHELF_BREAK_LAND_FACTOR` stays 1.0
— the physical shelf break, untuned. This is a re-normalization with
measured evidence, not the "weaken the test" route the spec rejected:
the property "a continent has a shelf" is preserved and now scales with
the continent. The whole-sphere `shelf_fraction` metric itself is
unchanged (the frozen census depends on it); `shelf_land_ratio` is a new
unregistered shape function.
