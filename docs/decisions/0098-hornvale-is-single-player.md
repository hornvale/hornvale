# 0098. Hornvale is single-player, forever

**Status:** Accepted (2026-08-04) · **Decider:** Nathan · **Relates:**
[0001](0001-determinism-is-constitutional.md),
[0007](0007-seed-is-identity.md),
[0022](0022-sim-emits-data-clients-render.md),
[0033](0033-serialized-floats-are-quantized-for-cross-platform-determinism.md),
[0069](0069-fine-position-is-never-serialized.md)

In the context of designing the play layer over a lazily-derived world, facing
the fact that multiplayer and lazy time are mutually exclusive rather than
merely awkward together, we decided that **Hornvale is a single-player
experience, permanently — one observer per world, no shared live sessions** —
accepting that co-op, shared servers and simultaneous play are closed
directions, not deferred ones.

## Why this is closed, not deferred

`UNI-30` is the world's time model: autonomy resolves by lazy evaluation,
*defined* over all time everywhere, **computed only where observed**, with
forward evolution as re-derivation at a larger `T` rather than integration from
saved state. "Now" is not a clock reading but *the frontier of intervention*.

That model has exactly one observer in it, and the singularity is load-bearing:

- With one player, "now" is wherever they are, so **any** region can be
  computed at **any** `T` on demand.
- With N players there are N frontiers of intervention. The moment two players'
  deltas can interact, the world needs a single monotonic clock — because a
  region already advanced to `T=100` by one player's presence **cannot** serve
  another arriving needing `T=90`. Rewinding past a committed delta is not
  available.

So multiplayer does not extend the architecture; it replaces it:

| | single-player | multiplayer |
|---|---|---|
| time | lazy, any `T` on demand | monotonic global clock |
| timeline scrubbing | free | gone — past deltas pin the past |
| computation | where observed | wherever *any* player is, never rewound |
| determinism | seed | seed **+ totally-ordered input log** |
| a living world | free (defined everywhere) | a standing global CPU bill |

Lazy time, `derive(seed, T)`, the no-wall-clock rule, and the whole
derived-view architecture all rest on the single-observer assumption. Choosing
multiplayer would have meant rebuilding that foundation and discarding the
design accumulated on top of it. This record exists so that a future session
tempted to "just add co-op" finds the reason rather than rediscovering it.

## What is given up, plainly

Shared live sessions, co-op, persistent shared servers, and any form of
simultaneous presence in one world. That is a real loss and it is accepted with
open eyes.

## What is *not* given up

**Worlds can be shared without sharing sessions.** Decision 0007 makes the seed
a world's identity, so two people can play *the same world* independently —
same geography, same history, same goblin village, different choices — and
compare what they found. The world catalog (`clients/world-wasm`) and the
external Orrery already work this way. Asynchronous sharing of seeds,
discoveries and replays remains fully available. Only simultaneity closes.

## Consequences

- **The play ledger is a simple overlay, not an ordered input log.** With one
  observer there is no concurrency to totally-order, so it records what the
  player did rather than a rollback-capable intent stream.
- **The no-wall-clock rule survives untouched.** Real-time multiplayer would
  have required a wall-clock→`WorldTime` mapping at an input boundary. Single-
  player needs none: the sim core stays clock-free as the Constitution requires.
- **Global deep history continues during play, and it is nearly free.** The bake
  is `BakeConfig::default_millennia` — two millennia in 25-year epochs, 80
  epochs, inside a measured 1.96 s release world build. An epoch is ≲25 ms and
  play runs at a **sub-epoch** timescale, so the far side of the planet may keep
  rising and falling unnoticed.
- **`UNI-30`'s "never integrate from saved state" resolves against 0033 rather
  than fighting it.** Within a session the bake's state stays resident at full
  precision and is extended an epoch at a time (~25 ms, nothing quantized);
  across a save the world is re-derived from the seed plus deltas (~2 s load).
  That is exactly 0033's *"resumption re-derives from the seed"*, and it keeps
  the Lorenz guard-rail satisfied without paying a full re-derivation per
  advance.

## See also

Frontier `UNI-30` (the living present — the time model this preserves);
[0037](0037-the-room-tier-ledger-is-chunk-partitioned.md) (store-irreversible-
derive-reversible, and partitioning deferred);
[0100](0100-fact-phenomenon-myth.md) (the three registers the play layer writes
into).
