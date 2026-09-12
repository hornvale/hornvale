# 0957. Evaluated astronomy stays native

**Status:** Accepted (2026-09-10) · **Decider:** Nathan

In the context of a renderer needing actual positions, orientation and physical
sizes, we decided to expose native `scene/astronomy-at/v1` observations at exact
ticks, accepting a distinct versioned surface with explicit model limits rather
than a second astronomy implementation inside Bevy.

**Context.** The approved design extends the element-evaluation precedent with
a separate evaluated query. `scene/system/v1` stays elements-only. The native
source initializes its world and contexts once and exchanges serialized documents
across the linker boundary, extending
[0114](0114-a-native-client-drives-across-the-linker-and-reads-across-the-serializer.md).

**Consequence.** Astronomy owns geometry, time and illumination. The anchor radius
is the approved bounded Earth-like rocky mass–radius observation, not a stored
field or cosmetic scale. Missing stellar/wanderer radius and moon spin remain
explicit nulls. Unsupported queries fail. Source/scope/world/revision/request
identity prevents stale replies replacing newer state. Render duration cannot
change a frame's instant. Native limits are documented in the
[schema reference](../../book/src/reference/scene-astronomy-at-v1.md).

**Authority.** [The approved Planetarium design](../superpowers/specs/2026-09-10-the-planetarium-design.md).
