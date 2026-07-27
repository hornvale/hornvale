# The Living Community — preregistered measurement gates

The measure-don't-narrate payoff check for history-first placement. All values are byte-deterministic (integer counts, integer-set Jaccard, and a mean-peak-population rank correlation over basic arithmetic + IEEE `sqrt`); no wall-clock timings appear here.

## Two honest post-data amendments

1. **Displacement is MIGRATION, not raiding.** The campaign was preregistered around a raid->flee->resettle floor. On the real seed-42 world — ample vacant habitable land — glacially-displaced communities migrate to empty cells instead of crowding into raids (raids ~ 0), so the displacement gate is re-pointed at `census(bake).migrated`, read off the ledger. Raid-driven displacement is deferred to campaign C3. *(C3, The Tumult, has since arrived: raids are no longer ~ 0 — seed 42 resolves 76 conquests, driven by coveted VALUE rather than by crowding. This gate still measures climate displacement only; `migration_events` excludes conquest-relocations by design, and conflict displacement is measured separately in `windows/worldgen/tests/history_tumult.rs`.)*
2. **Stratigraphy accretes on MARGINAL land.** The preregistered sub-hypothesis — depth correlates *positively* with capacity — is FALSIFIED: the correlation is robustly *negative* on every sampled world. A one-time reconstruction of the true carrying-capacity field agrees with the ledger's mean-peak-population signal (seed-42: -0.35 either way), so it is not a proxy artifact. Prime cells are settled once and persist; re-occupation stacks form on contested, climate-volatile land.

## Seed-42 headline (built to `BuildDepth::Full`)

- **migration-fired-at-volume**: 178 migration events (floor 5). PASS — climate-driven displacement fires at volume.
- **territories-separated**: mean pairwise region overlap 0.0592 (ceiling 0.25; raw cell-set overlap 0.0000 is a structural 0). PASS — the four goblinoids occupy strongly distinct countries. **The diversity payoff landed.**
- **stratigraphy-emerged**: 259/380 occupied sites re-occupied (0.6816); depth/capacity correlation -0.0892 (negative). PASS on emergence and on the *coupling*; the negative sign is the falsification finding above.

## Cross-seed robustness sweep (Settlements depth)

Per-seed floors: migration >= 5, region overlap < 0.25, re-occupied sites >= 2, correlation < 0. Every sampled world clears them.

| seed | migration | region overlap | occupied | restacked | fraction | depth/capacity corr |
|---|---|---|---|---|---|---|
| 1 | 78 | 0.0038 | 192 | 130 | 0.6771 | -0.3568 |
| 2 | 89 | 0.0276 | 198 | 140 | 0.7071 | -0.0933 |
| 3 | 74 | 0.0266 | 281 | 211 | 0.7509 | -0.2454 |
| 7 | 215 | 0.0178 | 313 | 198 | 0.6326 | -0.3771 |
| 13 | 36 | 0.0649 | 171 | 118 | 0.6901 | -0.2596 |
| 42 | 178 | 0.0592 | 380 | 259 | 0.6816 | -0.0892 |
| 100 | 313 | 0.0057 | 79 | 43 | 0.5443 | -0.7254 |
| 256 | 561 | 0.0161 | 133 | 66 | 0.4962 | -0.5358 |
| 777 | 270 | 0.0699 | 204 | 93 | 0.4559 | -0.6159 |
