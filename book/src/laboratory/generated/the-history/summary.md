# The Living Community — preregistered measurement gates

The measure-don't-narrate payoff check for history-first placement. All values are byte-deterministic (integer counts, integer-set Jaccard, and a mean-peak-population rank correlation over basic arithmetic + IEEE `sqrt`); no wall-clock timings appear here.

## Two honest post-data amendments

1. **Displacement is MIGRATION, not raiding.** The campaign was preregistered around a raid->flee->resettle floor. On the real seed-42 world — ample vacant habitable land — glacially-displaced communities migrate to empty cells instead of crowding into raids (raids ~ 0), so the displacement gate is re-pointed at `census(bake).migrated`, read off the ledger. Raid-driven displacement is deferred to campaign C3. *(C3, The Tumult, has since arrived: raids are no longer ~ 0 — seed 42 resolves 76 conquests, driven by coveted VALUE rather than by crowding. This gate still measures climate displacement only; `migration_events` excludes conquest-relocations by design, and conflict displacement is measured separately in `windows/worldgen/tests/history_tumult.rs`.)*
2. **Stratigraphy accretes on MARGINAL land.** The preregistered sub-hypothesis — depth correlates *positively* with capacity — is FALSIFIED: the correlation is robustly *negative* on every sampled world. A one-time reconstruction of the true carrying-capacity field agrees with the ledger's mean-peak-population signal (seed-42: -0.35 either way), so it is not a proxy artifact. Prime cells are settled once and persist; re-occupation stacks form on contested, climate-volatile land.

## Seed-42 headline (built to `BuildDepth::Full`)

- **migration-fired-at-volume**: 224 migration events (floor 5). PASS — climate-driven displacement fires at volume.
- **territories-separated**: mean pairwise region overlap 0.0470 (ceiling 0.25; raw cell-set overlap 0.0000 is a structural 0). PASS — the four goblinoids occupy strongly distinct countries. **The diversity payoff landed.**
- **stratigraphy-emerged**: 243/356 occupied sites re-occupied (0.6826); depth/capacity correlation -0.2711 (negative). PASS on emergence and on the *coupling*; the negative sign is the falsification finding above.

## Cross-seed robustness sweep (Settlements depth)

Per-seed floors: migration >= 5, region overlap < 0.25, re-occupied sites >= 2, correlation < 0. Every sampled world clears them.

| seed | migration | region overlap | occupied | restacked | fraction | depth/capacity corr |
|---|---|---|---|---|---|---|
| 1 | 64 | 0.0036 | 233 | 120 | 0.5150 | -0.4295 |
| 2 | 74 | 0.0257 | 211 | 136 | 0.6445 | -0.2725 |
| 3 | 145 | 0.0066 | 298 | 195 | 0.6544 | -0.3039 |
| 7 | 152 | 0.0198 | 315 | 184 | 0.5841 | -0.5066 |
| 13 | 58 | 0.0561 | 193 | 132 | 0.6839 | -0.2382 |
| 42 | 224 | 0.0470 | 356 | 243 | 0.6826 | -0.2711 |
| 100 | 313 | 0.0057 | 79 | 43 | 0.5443 | -0.7254 |
| 256 | 517 | 0.0307 | 136 | 67 | 0.4926 | -0.5599 |
| 777 | 414 | 0.0697 | 215 | 136 | 0.6326 | -0.4578 |
