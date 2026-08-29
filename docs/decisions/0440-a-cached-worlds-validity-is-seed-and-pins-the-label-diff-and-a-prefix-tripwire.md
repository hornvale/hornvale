# 0440. A cached world's validity is seed and pins, the label diff, and a prefix tripwire

**Status:** Accepted (2026-08-29) · **Decider:** Nathan (autopilot) ·
**Relates:** [0001](0001-determinism-is-constitutional.md),
[0016](0016-studies-preregister-hypotheses.md),
[0189](0189-a-pre-flip-world-file-does-not-load-and-that-is-the-point.md) ·
[The Overture](../../book/src/chronicle/the-overture.md)

In the context of a client that pays for genesis on every start, facing the
question of when a previously-built world on disk may be reused, we decided
validity is decided by **three layers, cheapest refusal first** — seed and pins,
then the world's `derived_under` roster diffed against the current stream
labels, then an astronomy-prefix tripwire — accepting that the tripwire proves
nothing beyond the rung it samples.

**Context.** Layer 1 is a free string compare against a sidecar and answers
most restarts without loading a world at all. Layer 2 loads the world but pays
no genesis, and reuses `hornvale::streams::what_moved` rather than a second,
driftable copy of the label roster. Layer 3 rebuilds to `BuildDepth::Astronomy`
(0.4 ms) and compares its facts, in order, against the cached world's leading
facts — catching an undeclared change that no label bump records.

**What the preregistration said, and what happened.**

- **H3 — the tripwire reddens on an undeclared genesis change: SUPPORTED.**
  Mutating `IMPACT_DENSITY_G_CM3`, a moon-density physics constant that is not
  a stream label, changed the astronomy facts and the tripwire refused the
  stale cache across two process runs. The spec flagged this one as having a
  real chance of failing; it did not.
- **H2 — a cached start under 1 s: NULL.** Measured **~1.2–1.4 s cached against
  ~3.6–3.7 s generated**. `load_if_valid` itself costs ~9 ms, so the whole gap
  to the spec's ~870 ms estimate is `Driver::start_from_world`'s own tail —
  terrain re-derivation for the map index, and session start — which this cache
  does not and should not touch.

**Consequence.** The cache ships despite its headline hypothesis coming back
null, and the reason is stated so it can be re-examined rather than assumed:
the residual ~1.2 s is **independent of genesis's own cost**, so the saving
grows as genesis grows toward the spec's projected minute. If a later campaign
finds that tail also scaling with world size, this record's arithmetic weakens
and the cache should be re-priced.

The tripwire's limit is deliberate and must not be oversold: a
settlements-only formula change is invisible to an astronomy prefix. Layers 1
and 2 are the protocol; layer 3 is a cheap extra chance, not a proof.

Pins are taken as an explicit argument rather than recovered from the saved
world, because they are **not recoverable**: worldgen commits no pin-echo fact,
and `World::derived_under` is stream-label metadata, a different thing keyed by
label. And loading is an associated function, not a method, because the whole
point is surviving a process restart, after which no live cache value exists to
call a method on.

**See also.** Spec §6, §10 (H2, H3); `clients/game/bin/src/cache.rs`.
