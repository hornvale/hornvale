# 0442. The rung observer hands borrows; an observer that crosses a thread clones

**Status:** Accepted (2026-08-29) · **Decider:** Nathan (autopilot) ·
**Relates:** [0002](0002-domains-depend-only-on-kernel.md),
[0441](0441-the-terminal-opens-before-genesis-and-genesis-runs-on-a-worker.md) ·
[The Overture](../../book/src/chronicle/the-overture.md)

In the context of needing to watch a world arrive rung by rung, facing the
choice between resumability and observation, we decided worldgen gains **one
additive observer callback on `build_to`'s existing rung boundaries**
(`build_world_observed`), that the callback receives **borrows** of the derived
artifacts, and that an observer needing to leave the thread **clones at its own
call site** — accepting that the cost of crossing is paid by whoever crosses.

**Context.** `build_to` always starts from `World::new(seed)` and has no resume
entry point, so resumability would have been a real change to genesis. It does
not need one: the rung boundaries already exist as early returns, so an
observer is sufficient and strictly additive. This is the campaign's **only**
sim-side change.

`RungArtifacts` hands references rather than owned values because a
`GeneratedTerrain` is far too large to clone for every observer, and most
observers — worldgen's own tests, for instance — never leave the thread and so
clone nothing. The client's observer *does* leave the thread
([0441](0441-the-terminal-opens-before-genesis-and-genesis-runs-on-a-worker.md)),
and it clones what it needs into an owned snapshot on the worker, where the
borrows are still valid. That is a choice at one call site, not a widening of
the contract.

**Consequence, measured rather than assumed.** The client's clone costs
**21.9 / 24.5 / 23.7 ms across all four rungs — 0.85% to 0.94% of a 3,054 ms
build** (`--release`, seed 42, three runs):

```
  rung          facts   world_ms  terrain_ms  climate_ms   total_ms
  Astronomy        83      0.090       0.000       0.000      0.090
  Terrain         110      0.135       1.851       0.000      1.986
  Settlements   18,722     5.642       1.896       2.665     10.204
  Full          21,964     7.199       1.619       0.772      9.590
```

A plain clone is therefore the right answer and an `Arc` is not needed: sharing
would buy under a hundredth of the wait and cost every view an indirection plus
a lifetime it does not want. The numbers are recorded here so a later campaign
that grows the ledger can tell whether the conclusion still holds, rather than
inheriting the conclusion alone.

**See also.** Spec §7; `windows/worldgen/src/lib.rs` (`build_world_observed`,
`RungArtifacts`); `clients/game/bin/src/overture/genesis.rs`.
