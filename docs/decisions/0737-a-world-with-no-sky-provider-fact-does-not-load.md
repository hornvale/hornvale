# 0737. A world with no sky-provider fact does not load

**Status:** Accepted (2026-09-04) · **Decider:** Nathan (autopilot, spec §7) ·
**Relates:** [0189](0189-a-pre-flip-world-file-does-not-load-and-that-is-the-point.md)

In the context of *The Zenith* deleting the constant-sky fallback, facing old
or hand-built ledgers with no `sky-provider` fact, we decided that **a world
with no `sky-provider` fact fails to reconstruct its sky** — accepting a loud
load failure instead of silently inventing a provider the ledger never
committed.

## Why absence is invalid

A world is a seed plus a ledger (decision 0189). The worldgen composition root
commits `sky-provider = "generated"` unconditionally before firing astronomy,
at every build depth. Its absence therefore does not describe a sanctioned
kind of Hornvale world; it means the world was never built by the current
pipeline.

`sky_of` consequently returns an error naming the missing fact and directs the
operator to regenerate from the seed and pins. There is no compatibility shim
that maps absence to the surviving provider: doing so would make an incomplete
ledger observationally equivalent to a built world and would restore the
implicit fallback this campaign removed.

## Consequence

Old saves that predate the unconditional fact, synthetic tests that construct
only `World::new`, and corrupt ledgers must be regenerated or completed through
the normal builder before their sky can be reconstructed. This is the same
epoch discipline as 0189: deterministic regeneration preserves the world the
project promises; silently interpreting a missing ledger fact does not.

