# 0069. Fine position is never serialized — the two-tier position law

**Status:** Accepted (2026-07-25) · **Decider:** Nathan

In the context of giving Hornvale a situated spatial surface, facing the need for
sub-room position for rendering, collision and local movement, we decided that
**an entity's persisted position is its room, and any finer coordinate exists
only inside the presence bubble and is never serialized** — accepting that no
saved state may ever point into the fine layer.

**Context.** `Position(Room)` is committed truth and already ships (`AGENT_AT`,
latest-wins). A finer position is what rendering and local pathfinding need, and
it matters only while an observer occupies that room. This is
quantize-at-emit-only (decision 0033) applied to space rather than to floats, and
`UNI-32` applied to space rather than to time.

**Consequence.** Three properties fall out of the representation instead of being
defended: no new determinism contract (nothing stored points into the fine layer,
so it may regenerate differently forever without corrupting a world); entering a
room, moving within it, and leaving *cannot* alter the world, byte-identically
and by construction rather than by an additive-latent trick; and cheesing by
re-entry is not defended against because it does not exist — the only thing spent
is turns.

The law is only compiler-enforced if the fine layer is a **distinct type**. This
is why a tile may not be a longer `RoomAddr` path: that would make a tile the same
type as a room, expressible wherever a room position is, and the guarantee would
degrade to policy. See the metaplan's Amendment 1 §1a.2.

**See also.** The Rose Window metaplan §3.1 and §1a.2
(`docs/superpowers/specs/2026-07-25-the-rose-window-metaplan-design.md`);
decision [0033](0033-serialized-floats-are-quantized-for-cross-platform-determinism.md);
`CLIENT-two-tier-position` and `CLIENT-tiles-as-deeper-rooms` in the idea
registry.
