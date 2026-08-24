# 0247. A mesh vertex is a `Vertex` and a face is a `Facet`

**Status:** Accepted (2026-08-24) · **Decider:** Nathan · **Relates:**
[0246](0246-a-renamed-concept-keeps-its-serialized-spelling-forever.md);
[The Lexicon of Place](../../book/src/reference/lexicon-of-place.md) (the glossary)

In the context of the world being one icosphere carrying two different
indexings — the vertices where fields are sampled, and the triangular faces
you can occupy — and of the old names `CellId` and `RoomAddr` signalling
neither, we decided that **the point is a `Vertex` and the patch is a `Facet`,
unprefixed and symmetric**, and that **`Node` is not available for this
meaning in this repository.**

## Why the old names had to go

`CellId` is a *point*. In GIS and raster convention a cell is an *area*, so
the name actively misleads a reader who knows the convention and mis-teaches
one who does not. It is not vagueness; it is a wrong statement.

`RoomAddr` addresses every depth from the base icosahedron down to depth 29.
"Room" is accurate for roughly the bottom third of that range and calls a
continent a room above it.

Neither word said which of the pair was the point and which the patch, and
that is a primal/dual distinction the project has already paid for getting
wrong once elsewhere.

## Why `Vertex` and not `Node`

The design spec proposed `Node`. It is unavailable, and the evidence was not
in front of the spec's author. **"Node" already means four different things
here**, so minting a fifth would have committed exactly the defect the
campaign existed to remove:

| where | what "node" means there |
|---|---|
| `windows/chronicle/src/config.rs:28` | `pub struct NodeId(pub u32)` — a graph node, ~36 uses, re-exported |
| `domains/terrain/src/branch.rs:448` | `struct Node` — a river-network node |
| `domains/astronomy` | the **orbital** node — "node line", "node longitude" |
| `clients/` | DOM / Node.js — `node`, `createTextNode` |

`Vertex` is held by nothing (one private `VertexGrid` in
`domains/terrain/src/channel.rs`), is the word the spec's own diagnosis
already used for the thing, and appeared in 231 doc-comment lines before the
rename touched anything. It is also the *true* word: these are the vertices of
a subdivided icosahedron.

## Why unprefixed and symmetric

`MeshVertex` was considered. It was rejected because `Facet` carries no
prefix, both name addressings of the **same** icosphere, and "Mesh" does not
by itself say which mesh — `kernel/src/room.rs` calls itself "The Room Mesh".
A prefix on one half and not the other asserts a distinction that is not
there, and lengthens ~1,600 derived names (`MeshVertexMap`,
`NearestMeshVertexIndex`, `mesh_vertex_count`) to do it.

`Vertex` / `Facet` is ordinary mesh vocabulary that states the point-vs-patch
distinction with no gloss needed, and the cascade stays short: `VertexMap<T>`,
`NearestVertexIndex`, `vertices()`, `vertex_count()`, `FacetId`.

## The one word this closes

**"Room" is retired and must not be reused.** The obvious next move once
`RoomAddr` becomes `Facet` is to promote `Chamber` to `Room`. Do not: every
historical reference, retrospective and decision record in this repository
uses "room" for the old, much broader meaning, and re-pointing the word at a
narrower one would silently change what the archive appears to say.
