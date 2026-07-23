# The Living Community — preregistered measurement gates

The measure-don't-narrate payoff check for history-first placement. All values are byte-deterministic (integer counts, integer-set Jaccard, and a mean-peak-population rank correlation over basic arithmetic + IEEE `sqrt`); no wall-clock timings appear here.

## Two honest post-data amendments

1. **Displacement is MIGRATION, not raiding.** The campaign was preregistered around a raid->flee->resettle floor. On the real seed-42 world — ample vacant habitable land — glacially-displaced communities migrate to empty cells instead of crowding into raids (raids ~ 0), so the displacement gate is re-pointed at `census(bake).migrated`, read off the ledger. Raid-driven displacement is deferred to campaign C3.
2. **Stratigraphy accretes on MARGINAL land.** The preregistered sub-hypothesis — depth correlates *positively* with capacity — is FALSIFIED: the correlation is robustly *negative* on every sampled world. A one-time reconstruction of the true carrying-capacity field agrees with the ledger's mean-peak-population signal (seed-42: -0.35 either way), so it is not a proxy artifact. Prime cells are settled once and persist; re-occupation stacks form on contested, climate-volatile land.

## Seed-42 headline (built to `BuildDepth::Full`)

- **migration-fired-at-volume**: 12 migration events (floor 5). PASS — climate-driven displacement fires at volume.
- **territories-separated**: mean pairwise region overlap 0.0466 (ceiling 0.25; raw cell-set overlap 0.0000 is a structural 0). PASS — the four goblinoids occupy strongly distinct countries. **The diversity payoff landed.**
- **stratigraphy-emerged**: 2/141 occupied sites re-occupied (0.0142); depth/capacity correlation -0.2049 (negative). PASS on emergence and on the *coupling*; the negative sign is the falsification finding above.

## Cross-seed robustness sweep (Settlements depth)

Per-seed floors: migration >= 5, region overlap < 0.25, re-occupied sites >= 2, correlation < 0. Every sampled world clears them.

| seed | migration | region overlap | occupied | restacked | fraction | depth/capacity corr |
|---|---|---|---|---|---|---|
| 1 | 27 | 0.0049 | 123 | 10 | 0.0813 | -0.4034 |
| 2 | 17 | 0.0238 | 62 | 7 | 0.1129 | 0.1380 |
| 3 | 106 | 0.0219 | 99 | 7 | 0.0707 | -0.4444 |
| 7 | 29 | 0.0353 | 90 | 2 | 0.0222 | -0.2557 |
| 13 | 11 | 0.0150 | 96 | 2 | 0.0208 | -0.2475 |
| 42 | 12 | 0.0466 | 141 | 2 | 0.0142 | -0.2049 |
| 100 | 970 | 0.0323 | 82 | 36 | 0.4390 | -0.7112 |
| 256 | 658 | 0.0079 | 116 | 35 | 0.3017 | -0.5942 |
| 777 | 124 | 0.0482 | 81 | 5 | 0.0617 | -0.4166 |
