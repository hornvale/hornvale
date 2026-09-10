# The Wanderers

**September 2026 · outcome: shipped — bounded stellar topologies, moving
wanderers, and an additive native/wasm scene contract**

## What was attempted

The campaign began with a single-star solar-system root and asked what it
would take to make the system observable rather than merely generated. The
answer was deliberately layered: generalize the stellar architecture first,
derive wanderer motion from committed elements and time, expose the events a
real observer would notice, and carry the result through `scene/system/v1`.

## What shipped

Hornvale now admits single systems, wide binaries with a circumprimary
anchor, and bounded close binaries with a circumbinary anchor. The primary
`StarSystem::star` compatibility field remains stable; topology-aware mass,
illumination, positions, and orbit admission live beside it.

Each astronomy-first wanderer has a deterministic phase. Its position is
evaluated at an instant rather than sampled into a trajectory. The astronomy
layer derives conjunction, opposition, retrograde, visibility, and
morning/evening observations, and the provider/almanac vocabulary reports
those events without inventing cultural interpretations.

The scene contract appends `stellar` and `wanderers` after the legacy fields.
Native and world-wasm output share the same producer path and are guarded by
byte-identity checks. The seed-42 locked almanac moved as expected: its
wanderers now report their derived retrograde, opposition, and conjunction
events.

## Boundaries kept honest

The campaign does not add arbitrary N-body dynamics, eccentric or inclined
wanderer orbits, close circumsecondary planets, transits or occultations, or
full sibling-world culture. Those branches remain captured in the frontier
registry, with `SKY-multi-star-nbody` added for the general stellar case.

No census or canonical sluice run was performed from this campaign branch. The
campaign's local and scoped checks are evidence for the implementation and
contract; canonical stage/merge admission remains the queue's job.
