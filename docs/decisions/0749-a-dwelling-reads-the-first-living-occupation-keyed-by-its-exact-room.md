# 0749. A dwelling reads the first living occupation keyed by its exact settlement room

**Status:** Accepted (2026-09-04) · **Decider:** Nathan (autopilot) ·
**Campaign:** The Housemark · **Relates:**
[0101](0101-geometry-and-society-are-separate-vocabularies.md) (place and
settlement remain distinct),
[0647](0647-a-made-chamber-is-written-from-the-ledger-at-the-walk.md) (the
ledger is read at the walk)

The cube-sphere settlement room and the icosphere occupation vertex are not
inverse address spaces. Reversing a settlement room through
`containing_vertex` mapped 531 of 1,275 living records to a direct neighbour,
producing 222 absent and 41 wrong living-people readings.

We decided that **production indexes a living occupation by the exact packed
settlement-room address used by `built_rooms`**. One ordered reduction over the
settlement roster supplies both the room name and its living occupation; when
several occupations share a player-addressable room, the first settlement in
that deterministic order supplies both. `brief_of` truncates to walk depth and
reads this room-keyed value directly. It never reconstructs a vertex from the
room.

A genuinely unoccupied or synthetic brief still carries no people and no
housemark. The five-seed H3 measured 1,259 distinct built rooms, all 1,259
inhabited, with 16 deterministic shared-room collisions, 15 peoples, and six
housemark classes; every structural signature recovered its class.

**See also.** [The Housemark design](../superpowers/specs/2026-09-04-the-housemark-design.md)
§§4–5 and H3; [campaign ledger](../superpowers/ledgers/2026-09-04-the-housemark.md)
#7.
