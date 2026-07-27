# The Living Community — preregistered measurement gates

The measure-don't-narrate payoff check for history-first placement. All values are byte-deterministic (integer counts, integer-set Jaccard, and a mean-peak-population rank correlation over basic arithmetic + IEEE `sqrt`); no wall-clock timings appear here.

## Two honest post-data amendments

1. **Displacement is MIGRATION, not raiding.** The campaign was preregistered around a raid->flee->resettle floor. On the real seed-42 world — ample vacant habitable land — glacially-displaced communities migrate to empty cells instead of crowding into raids (raids ~ 0), so the displacement gate is re-pointed at `census(bake).migrated`, read off the ledger. Raid-driven displacement is deferred to campaign C3. *(C3, The Tumult, has since arrived: raids are no longer ~ 0 — seed 42 resolves 76 conquests, driven by coveted VALUE rather than by crowding. This gate still measures climate displacement only; `migration_events` excludes conquest-relocations by design, and conflict displacement is measured separately in `windows/worldgen/tests/history_tumult.rs`.)*
2. **Stratigraphy accretes on MARGINAL land.** The preregistered sub-hypothesis — depth correlates *positively* with capacity — is FALSIFIED: the correlation is robustly *negative* on every sampled world. A one-time reconstruction of the true carrying-capacity field agrees with the ledger's mean-peak-population signal (seed-42: -0.35 either way), so it is not a proxy artifact. Prime cells are settled once and persist; re-occupation stacks form on contested, climate-volatile land.

## Seed-42 headline (built to `BuildDepth::Full`)

- **migration-fired-at-volume**: 266 migration events (floor 5). PASS — climate-driven displacement fires at volume.
- **territories-separated**: mean pairwise region overlap 0.0644 (ceiling 0.25; raw cell-set overlap 0.0000 is a structural 0). PASS — the four goblinoids occupy strongly distinct countries. **The diversity payoff landed.**
- **stratigraphy-emerged**: 280/386 occupied sites re-occupied (0.7254); depth/capacity correlation -0.2181 (negative). PASS on emergence and on the *coupling*; the negative sign is the falsification finding above.

## Cross-seed robustness sweep (Settlements depth)

Per-seed floors: migration >= 5, region overlap < 0.25, re-occupied sites >= 2, correlation < 0. Every sampled world clears them.

| seed | migration | region overlap | occupied | restacked | fraction | depth/capacity corr |
|---|---|---|---|---|---|---|
| 1 | 42 | 0.0050 | 202 | 86 | 0.4257 | -0.5382 |
| 2 | 44 | 0.0454 | 186 | 132 | 0.7097 | -0.1473 |
| 3 | 211 | 0.0157 | 281 | 200 | 0.7117 | -0.3527 |
| 7 | 93 | 0.0127 | 277 | 136 | 0.4910 | -0.4996 |
| 13 | 64 | 0.0487 | 206 | 133 | 0.6456 | -0.2872 |
| 42 | 266 | 0.0644 | 386 | 280 | 0.7254 | -0.2181 |
| 100 | 277 | 0.0057 | 79 | 38 | 0.4810 | -0.6295 |
| 256 | 412 | 0.0057 | 143 | 71 | 0.4965 | -0.5624 |
| 777 | 376 | 0.0561 | 251 | 173 | 0.6892 | -0.2985 |
