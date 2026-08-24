# The Lexicon of Place

*A lexicon is not a dictionary. A dictionary records what words happen to
mean; a lexicon fixes what they will mean.*

This campaign changed no behaviour. Its headline result is that
`make rebaseline-goldens` wrote **zero bytes** — not one seed-42 world,
session snapshot, scene, surrounds, locale JSON, almanac or elevation map
moved — after ~16,000 sites had been rewritten across the kernel, every
domain, every window, the CLI and the browser clients.

It exists because a larger design conversation stalled. The question was
whether every zoom rung should be a uniform square grid, and it could not be
argued because the people arguing it could not be sure they meant the same
thing by *level*, *band*, or *room*. A vocabulary is a prerequisite for that
conversation, not an ornament on it.

## The diagnosis: one sphere, two indexings, no naming signal

The world is an icosphere. Two different things live on it. `CellId` was a
**vertex** — a point, where terrain and climate fields are sampled. `RoomAddr`
was a **face** — a patch you can occupy. Nothing in either word said which was
which, and they are duals: a face's corners are three vertices.

Worse, each word was independently wrong. In GIS and raster convention a
*cell* is an area, so calling a point a cell misleads a reader who knows the
convention and mis-teaches one who does not. And `RoomAddr` addresses every
depth from the bare icosahedron down to 29 — "room" is accurate for roughly
the bottom third of that range and calls a continent a room above it.

They are now `Vertex` and `Facet`: ordinary mesh vocabulary, symmetric,
unprefixed, stating the point-versus-patch distinction with no gloss needed.

## The word that was already taken four times

The design spec proposed `Node`. It was unavailable, and finding that out cost
one grep:

| where | what "node" already means |
|---|---|
| `windows/chronicle` | `pub struct NodeId(pub u32)` — a graph node |
| `domains/terrain` | a river-network node |
| `domains/astronomy` | the **orbital** node — node line, node longitude |
| `clients/` | DOM / Node.js |

Renaming a mesh vertex to `Node` would have minted a fifth meaning for a word
that already carried four — which is precisely the defect the campaign existed
to remove, committed in the act of removing it. The lesson generalises past
this campaign: **a rename's candidate name is a claim about the whole
repository, and it is cheap to check and expensive to get wrong.**

## The half that outlives the renaming

Every serialized spelling still says "room" and "cell", permanently. The code
says `Facet` while the seed-derivation label says `"room/face"`, and that
mismatch is correct rather than debt: a label is a save-format contract, and
changing one silently produces a different world from the same seed.

The spec anticipated that the next campaign to notice the mismatch would want
to finish the job, and named the `"room/"` knowledge-key prefix as the
uncovered case: *"two literals, in two files, with nothing asserting they
agree ... rename one and the fog of war silently stops working: no test
fails."*

Mutation testing said otherwise, and **the truth was worse than the claim, not
milder.** There are three sites, not two — the spec missed a default-deny
match arm that is the strongest cover of the three. And the number of tests
that object depends on how *tidy* you are:

```text
mutate the writer alone                   6 tests red
mutate the writer and the reader          5 tests red
mutate all three, consistently            2 tests red
```

A rename campaign is tidy. It changes all three at once — the third row. And
the only two objectors are byte-goldens, whose documented remedy is a single
`make rebaseline-goldens`. So the suite goes red, one command makes it green,
and a changed session-snapshot wire format ships to the browser client with a
green gate behind it.

**A red with a one-command answer is worse than no cover at all.** No cover
leaves nothing that looks like a verdict; this leaves a verdict a reasonable
person discharges in ten seconds. That is now decision
[0246](../../../docs/decisions/0246-a-renamed-concept-keeps-its-serialized-spelling-forever.md),
and the remedy is a test that writes the literal out and therefore *cannot* be
rebaselined.

## What the audit found that nothing was looking for

Enumerating the freeze meant grepping the committed data rather than the code,
and the data knew things the spec did not:

- **Two ledger *subject* strings** — `"cell/{}"` in the volcano emitter and
  `"cell/{}/process/{}/block/{}"` in the hazard emitter. These are in every
  committed world.
- **Two census golden columns**, `per-cell-diversity` and
  `cold-built-room-share`, which the calibration batteries assert against, so
  moving one is a census refresh.
- **A fourth meaning of "level"** the spec had counted as three:
  `RegionScene.level`, a cube-face quadtree depth that is a serialized field
  of a cross-repo scene schema. It reads exactly like the icosphere's
  subdivision level and is refinement of a different mesh entirely.
- **A third vertical ladder.** The spec described two, `Band` and `Stratum`.
  The code has three: `Band` in the kernel, `Stratum` in climate, and
  `Horizon` in terrain, whose rosters deliberately mirror one another without
  sharing a derivation.

None of these was hidden. All four were one query away, and all four were
missed by a spec written carefully by someone reasoning about the code instead
of asking it.

## The drift class nobody had a name for

After the type rename, exactly three committed artifacts moved, and all three
carried the **same sentence**. A laboratory metric's doc string names
`NearestCellIndex` by name; that prose is published into the census
`schema.json` and read out again by the Domesday survey.

So a type rename reached committed data **through documentation** — not
through a value, not through a label, not through anything the freeze
inventory covers. No metric name moved and no metric value moved. It is
benign, and it is a class worth knowing about the next time an artifact diff
appears where none was expected.

## Byte-identity held, and it did not hold by itself

The zero-bytes result at the top of this page is not what the first sweep
produced. Three separate things moved committed output, and each is a class
the freeze inventory did not have:

**Rendered prose is a serialization boundary.** Sweepers renamed "992 cells"
to "992 vertices" inside almanac and connections output. The almanac ended up
saying *both* words in one sentence — "region holds only 1 vertex … run 1876,
1654, 914, 638, and 417 cells" — because some sites had already been protected
and some had not. A player reads that string; `vertex` is engine vocabulary.
Rendered prose stays "cell", and every such site now carries a comment saying
why.

**A predicate's description travels in every world.** Four moved — `cell-id`,
`occ-site`, `ocean-fraction`, `highest-elevation-m` — and `world-seed-42.json`
grew by exactly the nine bytes those four account for. A description is
documentation, but it is documentation *inside the serialized registry*, so
decision 0246 freezes it the same way it freezes a name.

**Not every byte-golden is reachable by a rebaseline command.** A diagnostic
dump's header, `"CELL {id} COLUMN"`, is pinned by
`windows/locale/tests/fixtures/column_before.txt`. `make rebaseline-goldens`
does not run that test, so the drift was invisible to every regeneration path
and only the full workspace suite caught it. The golden set is larger than the
set any rebaseline rewrites.

The pattern across all three: **the freeze inventory was built by asking "what
is a contract?" and the answer left out everything that is a contract by
accident** — prose that happens to be rendered, documentation that happens to
be serialized, a label that happens to be pinned.

## The guard is a ratchet, because an allowlist would have been a lie

About 108 files still hold the word legitimately: a `Cell` in the chamber
lattice, a `SurroundsCell` in the chart, a markdown-table cell in the trope
report. A check that failed on the word's mere existence would be red on day
one and trained away within a week. An allowlist of forty type names would
pass a *new* vertex-sense `cell` local sitting in the same file as an
allowlisted one.

So it fails on **novelty**, like `tropes check` and the timings baseline
before it: a file may carry no more `cell`-bearing tokens than the committed
inventory records, and a file absent from the inventory may carry none.
Numbers may fall freely; raising one needs a human's reason.

It counts case-insensitively, which the campaign's own greps did not. The
plan's definition-of-done and all six dispatch briefs specified
`\b\w*[Cc]ell\w*\b`, which cannot match `CELL` — so about eighty
occurrences across five crates were invisible to the check that was supposed
to *prove* the sweep complete. That is the smaller lesson. The larger one is
that **a check that proves the work must not inherit the blind spot of the
check that did it.**

## What is deliberately not done

The design conversation reached a further simplification and did not adopt it:
unify every place-address into `(Facet, Depth)` — where you are on the sphere,
and how far above or below its surface. Buildings, caves, the sea column and
open ground would stop being four addressing schemes.

It is attractive, and it is an epoch to end all epochs: every address in every
world changes meaning, the census refreshes, `world-wasm` consumers re-pin.
The ruling was to do the renaming first and decide that separately, in clear
words. It is recorded as `MAP-place-is-facet-and-depth` so it is not
rediscovered from scratch.

The vocabulary itself is fixed in
[The Lexicon of Place](../reference/lexicon-of-place.md), which is the page a
reader consults when the archive says "cell" and the code says `Vertex`. The
archive is not swept. Each of those uses is a record of what was true when it
was written.
