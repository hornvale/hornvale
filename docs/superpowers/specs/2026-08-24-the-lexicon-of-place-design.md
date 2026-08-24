# The Lexicon of Place — design

**Date:** 2026-08-24 · **Status:** DRAFT, written for a fresh session to execute ·
**Decider:** Nathan (scope approved in conversation, 2026-08-24) ·
**Author:** Claude (campaign-autopilot)

> **This document is a HANDOFF.** It was written at the end of a long session
> and carries findings that exist nowhere else in the repo. Read §4 ("What
> does NOT change, and why") before proposing anything — three of the renames
> a reader will independently want to make have already been considered and
> rejected on evidence, and one of them was ratified two days ago.

## 1. What this is

A **rename campaign, not a redesign.** The spatial vocabulary has accreted
collisions that make the world hard to talk about: the same word means
different things in different crates, and the most-used type calls a continent
a room. Nothing about the world's structure changes here — only what we call
its parts.

It exists because a larger design conversation (a uniform square grid at every
zoom rung) stalled on the fact that the participants could not be sure they
meant the same thing by "level", "band", or "room". The vocabulary is a
prerequisite for that spec, not a nicety.

## 2. The diagnosis

**One sphere, two indexings, no naming signal.** `Geosphere` is an icosphere.
`CellId` is a **vertex** — where terrain and climate fields are sampled.
`RoomAddr` is a **face** — an area you can occupy. Nothing in either word says
which is a point and which is a patch, and the pair is a primal/dual
distinction that has already produced at least one measured defect elsewhere
in the project.

**`RoomAddr` calls a continent a room.** It addresses every depth from 0 to
`MAX_DEPTH` (29). "Room" is accurate for roughly the bottom third of that
range and misleading above it. `RoomAddr` at walk depth is an *outdoor
locale*, not a room.

**"cell" is actively wrong, not merely vague.** In GIS and raster convention a
cell is an *area*. Hornvale's `CellId` is a *point*. A reader who knows the
convention is misled; one who does not learns the wrong thing.

## 3. Scope

| rename | occurrences | files |
|---|---|---|
| `RoomAddr` → `Facet` | 740 | 50 |
| `RoomId` → `FacetId` | 43 | 15 |
| `CellId` → `Node` | 1,725 | 144 |
| `windows/vessel/src/band.rs` → a depth-named module | — | 1 |
| `level` → `depth`, **only** where it means mesh refinement | judgement | — |

**~2,500 sites.** Most of it is mechanical — the compiler finds every one —
but doc comments and prose also say "cell" and "room", and those need
judgement rather than substitution. A doc that says "the room's cells" is
saying something true about faces and vertices and must survive the rename
still saying it.

**`level` needs care: it currently means three things.** `Geosphere`
subdivision level; `RoomAddr` path depth; and `ChamberAddr.level` (*"a level is
a screen-filling map, not a storey"* — The Drift). Rename only the first two,
to `depth`; leave `ChamberAddr.level` alone, it is a different and correctly
named thing.

## 4. What does NOT change, and why

**Read this section before proposing additions.** Each of these was proposed
during the design conversation and withdrawn on evidence.

**`Band` (kernel) stays.** `kernel/src/band.rs` is TWO DAYS OLD and is the
resolution of exactly the problem this campaign is about: The Drift
(2026-08-23) consolidated `hornvale_terrain::delve::DelveRung`, a mirrored
`hornvale_climate::underworld::DelveZone`, and a bare `u8` rank on
`ChamberAddr::band` into one kernel type, with a written argument about why
decision 0094 did not apply and why 0044's cross-domain clause places it in
the kernel. Renaming it to `Rung` would relitigate a deliberate recent
decision without new information.

**The "band means two things" collision is weaker than it looks.**
`windows/vessel/src/band.rs` defines **no `Band` type** — it holds
`CHAMBER_DEPTH_OFFSET`, `chamber_depth()` and `truncate_to_walk()`, all about
refinement depth. The collision is module-name-level only, and renaming that
module is the whole fix.

**`Stratum` stays.** It is realm-relative position in a column
(`Surface`; `Epipelagic`…`Hadal`; `Regolith`…`Underneath`). "Stratum" is the
accurate word for that. It was proposed for renaming to `Zone` on the grounds
that it and `Band` are confusably both-vertical; with `Band` clearly the cave
ladder and `Stratum` clearly column-position, that objection does not survive.

**`Medium`, `Aperture`, `Chamber` and `Anchor` stay.** A realm as
`(medium, access, strata)` rather than an enum of worlds is the best-designed
part of the current vocabulary, and *access rather than materiality* as the
discriminator is a sharp distinction. `Chamber` is unambiguous. `Anchor` — a
named thing with relations and no coordinate — is exact.

**Do not reuse the word "Room" for anything.** Once `RoomAddr` becomes
`Facet`, the temptation is to promote `Chamber` to `Room`. Resist: every
historical reference, retrospective and decision record uses "room" for the
old meaning, and re-pointing the word at a narrower one makes the archive lie.

## 5. THE FREEZE — the half that matters more than the renaming

**Every serialized spelling keeps saying "room" and "cell".** These are
save-format contracts; changing one silently corrupts every world (CLAUDE.md,
decision 0006).

Seed-derivation stream labels:

```
kernel/src/streams.rs        "room/face"                    ROOM_FACE
kernel/src/streams.rs        "room/child"                   ROOM_CHILD
windows/vessel/src/streams.rs "room/furnishing/v1"          ROOM_FURNISHING
windows/vessel/src/streams.rs "room/chambers/v1"            ROOM_CHAMBERS
windows/vessel/src/streams.rs "room/layout/v1/rectilinear"
windows/vessel/src/streams.rs "room/layout/v1/grown"
windows/vessel/src/streams.rs "room/layout/v1/anchors"
domains/settlement/src/lib.rs "cell-id"                     (a predicate name)
```

Epoch keys:

```
kernel/src/world.rs:346      "room/furnishing" => "v1"
kernel/src/world.rs:348      "room/layout/rectilinear" => "v1"
```

Wire and save strings:

```
windows/vessel/src/knowledge.rs:110   format!("room/{}", vantage.locale.id)
windows/vessel/src/purview.rs:146     strip_prefix("room/")
scene/surrounds/v2                    `cells`, and clients/game's ChartCell mirror of it
```

**The constant NAMES may be renamed; the string VALUES may not.** After this
campaign the code will say `Facet` while the label says `"room/face"`, and
that mismatch is correct and permanent.

### 5.1 The rule, and why it needs a guard

**The next campaign to notice the mismatch will want to finish the job.** That
is the failure this section exists to prevent, and prose alone will not
prevent it.

Partial cover already exists: `stream_labels()` publishes into a generated,
drift-checked manifest, so changing a *stream label* reddens the drift check.

**The `"room/"` knowledge-key prefix has no cover at all.** It is written in
`knowledge.rs:110` and read in `purview.rs:146` — two literals, in two files,
with nothing asserting they agree. Rename one and the fog of war silently
stops working: no test fails, because each side is internally consistent.

**This is the identical shape to the delve-prose coupling fixed on
2026-08-23** (`fix(vessel): pin the delve success literal driver.rs's
discovery gate depends on`), where a client keyed on `starts_with("You worm
down into the dark.")` and the nearest guard checked a looser substring. Use
the same remedy: **a test co-located with the writing side, asserting the
exact literal the reading side depends on, naming that reader by path.**

Closing that gap is a first-class deliverable of this campaign, not a footnote.

## 6. Verification

- **The rename is behaviour-preserving.** The strongest available evidence is
  byte-identity: seed-42 artifacts and the committed fixtures must not move.
  `make rebaseline` followed by a clean `git diff --exit-code` over
  `docs/generated-paths.txt` is the check, and it is nearly sufficient on its
  own — a rename that changed a stream label would move every name in every
  world and light this up immediately.
- **`docs/audits/type-audit-report.md` will drift**, because the type names it
  reports are changing. Regenerate it in the same commit; it is drift-checked.
- The book, the decision records and the retrospectives contain thousands of
  uses of "room" and "cell" in their old senses. **Do not sweep them.** They
  are historical records of what was true when written. The glossary (below)
  is how a reader reconciles them.

## 7. Deliverable: the glossary

A book page fixing these words, including the collisions this campaign
removes and the ones it deliberately keeps: the two vertical ladders (`Band`
the cave rungs, `Stratum` the column position), the three meanings of `level`
before this campaign and the two after, and the frozen-spelling rule with its
reason. This is the artifact that makes the vocabulary durable rather than
merely current.

## 8. Deferred, deliberately: `Place = (Facet, Depth)`

The design conversation reached a further simplification and **explicitly did
not adopt it**: unify every place-address into `(Facet, Depth)` — where you
are on the sphere, and how far above or below its surface. A cave chamber
becomes a room-scale facet at a depth rather than a
`(cell, branch, band, level)` lattice coordinate; buildings, caves, the sea
column and open ground stop being four addressing schemes.

It is attractive, it satisfies decision 0102's positional-not-generational
requirement arguably better than the current scheme, and **it is an epoch to
end all epochs**: every address in every world changes meaning, the census
refreshes, `world-wasm` consumers re-pin. Nathan's ruling was to do the
renaming first and decide this separately, in clear words.

Recorded here so it is not rediscovered from scratch.

## 9. Practical notes for the executing session

- **This campaign conflicts with everything.** It touches kernel → domains →
  windows → clients. Nathan's timing call (2026-08-24) was that the queue is
  quiet and this should go quickly so parallel campaigns can absorb it.
  Submit promptly; expect to absorb `main` and resubmit if it bounces at the
  mouth.
- Claim a decision block (`make decision-block NAME=campaign/<name>`) before
  authoring any decision record. Minting outside your block passes every
  mechanical check and collides later.
- A merge needs an authored `Sluice-Headline:` trailer in the range.
- The blast-radius numbers in §3 were measured on `5ccde4bb1` and will drift;
  re-measure rather than citing them.
