# 0081. Locale, chamber, place — "room" unqualified is retired

**Status:** Accepted (2026-07-27) · **Decider:** Nathan

In the context of the room mesh now carrying two scales of place, facing the
fact that one word named both and the ambiguity had already produced a wrong
design, we decided that **a macro place is a *locale*, a micro place is a
*chamber*, either one is a *place*, and "room" unqualified is retired from new
prose and new doc comments** — accepting that the codebase's existing `Room*`
identifiers keep their names.

**Context.** `GLOBE_LEVEL = 6` (`domains/terrain/src/lib.rs`) puts the
canonical grid at ~110 km per cell, and `walk_depth = globe_level + 6`
(`windows/vessel/src/agent.rs`) puts a possessed body six refinements below
that. Six halvings of ~110 km is **~1.7 km**: the place a walking creature
commits its position to is nearly two kilometres across. That size is
deliberate — it is the macro worldgen resolution, chosen so 1000-world censuses
finish, and the scale wilderness travel is designed for. But The Hearth's
anchor vocabulary (`Hearth`, `Bed`, `Alcove`, `Vessel`) was written for human
scale while *deriving* for that 1.7 km place, and nothing in the source says
how big anything is: the number is the product of two constants in different
crates that never meet.

The collision produced a wrong design downstream — a metric glyph lattice drawn
at landscape scale, a correct picture of the wrong thing — and it was caught
only because someone multiplied the two constants together during a design
session. No test would have caught it. A chamber is the walk band plus nine
further refinements, ~3.3 m, against a `MAX_DEPTH` of 29.

**Consequence.** New prose, new doc comments, new player-facing strings and new
spec sections say **locale** or **chamber** where the band matters and
**place** where it does not. Chronicle entries written before this record are
not rewritten: earlier campaigns said "room" and that is what they said.

**`RoomAddr`, `RoomId` and `room_from_text` are NOT renamed.** An address is
correct at every band — a chamber address is a `RoomAddr` with a longer path,
and the type is the address space rather than one scale within it. Renaming
would touch a frozen save-format contract (`AGENT_AT` commits a decimal
`RoomId` string) to fix a prose problem, which is the wrong trade. The
`windows/locale` crate already carried the better word and keeps it.

The cost accepted: the type names and the prose now disagree, so a reader meets
`RoomAddr` in code and "locale" in the book. That is preferable to the
alternative the ambiguity was already producing, and this record is why the
distinction will not silently regrow — the idea registry governs nothing
(decision 0031), so the terminology needed a home that does.

**See also.** [0069](0069-fine-position-is-never-serialized.md) (the two-tier
position law, which is why descent commits nothing);
[0077](0077-zoom-in-the-room-mesh-is-path-truncation.md) (zoom as path
truncation — the same move this campaign uses downward, to read walk-band
terrain from a chamber address);
[The Lintel spec](https://github.com/hornvale/hornvale/blob/main/docs/superpowers/specs/2026-07-27-the-lintel-design.md)
§§2–5; [The Lintel chronicle](../../book/src/chronicle/the-lintel.md).

Ratified at *The Lintel*'s merge gate.
