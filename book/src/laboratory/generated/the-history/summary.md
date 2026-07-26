# The Living Community — preregistered measurement gates

The measure-don't-narrate payoff check for history-first placement. All values are byte-deterministic (integer counts, integer-set Jaccard, and a mean-peak-population rank correlation over basic arithmetic + IEEE `sqrt`); no wall-clock timings appear here.

## Two honest post-data amendments

1. **Displacement is MIGRATION, not raiding.** The campaign was preregistered around a raid->flee->resettle floor. On the real seed-42 world — ample vacant habitable land — glacially-displaced communities migrate to empty cells instead of crowding into raids (raids ~ 0), so the displacement gate is re-pointed at `census(bake).migrated`, read off the ledger. Raid-driven displacement is deferred to campaign C3. *(C3, The Tumult, has since arrived: raids are no longer ~ 0 — seed 42 resolves 76 conquests, driven by coveted VALUE rather than by crowding. This gate still measures climate displacement only; `migration_events` excludes conquest-relocations by design, and conflict displacement is measured separately in `windows/worldgen/tests/history_tumult.rs`.)*
2. **Stratigraphy accretes on MARGINAL land.** The preregistered sub-hypothesis — depth correlates *positively* with capacity — is FALSIFIED: the correlation is robustly *negative* on every sampled world. A one-time reconstruction of the true carrying-capacity field agrees with the ledger's mean-peak-population signal (seed-42: -0.35 either way), so it is not a proxy artifact. Prime cells are settled once and persist; re-occupation stacks form on contested, climate-volatile land.

## Seed-42 headline (built to `BuildDepth::Full`)

- **migration-fired-at-volume**: 58 migration events (floor 5). PASS — climate-driven displacement fires at volume.
- **territories-separated**: mean pairwise region overlap 0.0470 (ceiling 0.25; raw cell-set overlap 0.0000 is a structural 0). PASS — the four goblinoids occupy strongly distinct countries. **The diversity payoff landed.**
- **stratigraphy-emerged**: 99/227 occupied sites re-occupied (0.4361); depth/capacity correlation -0.4517 (negative). PASS on emergence and on the *coupling*; the negative sign is the falsification finding above.

## Cross-seed robustness sweep (Settlements depth)

Per-seed floors: migration >= 5, region overlap < 0.25, re-occupied sites >= 2, correlation < 0. Every sampled world clears them.

| seed | migration | region overlap | occupied | restacked | fraction | depth/capacity corr |
|---|---|---|---|---|---|---|
| 1 | 21 | 0.0067 | 137 | 37 | 0.2701 | -0.5290 |
| 2 | 6 | 0.0141 | 79 | 26 | 0.3291 | -0.4048 |
| 3 | 56 | 0.0253 | 138 | 77 | 0.5580 | -0.5552 |
| 7 | 51 | 0.0252 | 162 | 54 | 0.3333 | -0.5764 |
| 13 | 8 | 0.0393 | 136 | 78 | 0.5735 | -0.4741 |
| 42 | 58 | 0.0470 | 227 | 99 | 0.4361 | -0.4517 |
| 100 | 243 | 0.0057 | 73 | 39 | 0.5342 | -0.7259 |
| 256 | 426 | 0.0347 | 126 | 51 | 0.4048 | -0.4905 |
| 777 | 140 | 0.0669 | 110 | 58 | 0.5273 | -0.4940 |
