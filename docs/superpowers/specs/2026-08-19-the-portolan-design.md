# The Portolan — design

**Status:** DRAFT — awaiting G3 review. **Campaign:** The Portolan.
**Branch:** `campaign/the-portolan`.

A cursor in the plate. The map draws terrain and nothing else; a free-roaming
cursor selects a cell; the sim answers what is there; the most specific
feature's name appears in a strip beneath the map. Named for the rhumb-line
charts a navigator works over with a finger.

---

## 0. Provenance — two spikes, and the one that refuted the other

**The first spike measured a design nobody wants.** It rendered every seed-42
feature label onto an ASCII Mercator map, ungated, at three widths. The result
was illegible at all of them: 31.5% of character cells consumed by text at 72
columns with **26 labels contending for one cell**, and still 43.5% of labels
colliding at 288 columns — already wider than a terminal. Names merged into
each other (`RjarRlerjorji`, `SidxerZherQvadvoshao`). It concluded the binding
constraint was spatial **clustering** — volcanoes follow island arcs, rivers
follow drainage — and that no salience gate alone would fix it.

**Every word of that is true and none of it matters**, because the premise was
wrong. Nathan's correction: labels are not painted on the map at all. A cursor
selects, and the name appears elsewhere.

**The second spike then showed the first one's catastrophe was an artifact of
the question.** Painting anchors onto a coarse grid piles 26 labels into a
cell. Asking *what is at this cell* returns:

```
0 features:    377 cells (0.92%)
1 feature:  34,851 cells (85.08%)
2 features:  5,697 cells (13.91%)
3 features:     37 cells (0.09%)
max: 3
```

This is recorded at length because the lesson is not "the first spike was
wasted" — it cost an hour and killed an entire design branch, which is what a
spike is for. The lesson is that **a measurement inherits the question's
framing, and a catastrophic number is not self-interpreting.**

---

## 1. What this campaign produces

- **A free-roaming cursor in the plate** (`clients/game`), rendered as a
  blinking underline, moved by key, at every band the plate draws.
- **A resolution query**: cursor position → the sim → what is at that cell.
- **A status strip** of plate width beneath the map, carrying the most
  specific feature's name.
- **`CellId → Vec<FeatureId>`**, built once at world load.
- **Declared per-class salience** on `FeatureClass`.
- The map pane renders **terrain only**. No labels, no gate, no abbreviation.

### What it does not produce

No fog — names are not yet learned or withheld; that is campaign 3. No
labels on the map, ever. No mouse.

---

## 2. The cursor is a plate primitive, not a map feature

The plate already dispatches by band — "the walk-band chart outdoors, the
chamber-band floor plan indoors" (`spread.rs`). A cursor that knew about
landscape features would need reinventing at every band.

Defined instead as **point into whatever the plate drew, and ask its author
what is there**, it generalises for free:

```
BAND      PLATE DRAWS        CURSOR RESOLVES TO       CLOCK        ANSWERED BY
--------  -----------------  -----------------------  -----------  -------------
world     terrain, Mercator  landscape feature stack  world-life   terrain index
walk      locale chart       cells, marks, agents     per turn     session
chamber   floor plan         chamber features         per turn     session
delve     dungeon / mine     as chamber               per turn     session
```

**This campaign implements the world row only** and leaves the others
unimplemented but *addressable* — the cursor exists at every band and reports
"nothing here yet" where the resolver is absent. That refusal is honest and
cheap; faking a resolution would not be.

### 2.1 Selection already exists — by name

`Session::examine_*` matches a noun and returns its datum. The cursor is the
same operation addressed **by position**. Two addressing modes, one concept —
and `!examine $@` composes them literally: a name-based verb applied to a
position variable. The spec adopts that shape; the exact syntax is §9.

---

## 3. Resolution

### 3.1 The core cannot ask, so the bin answers

`hornvale-game-core` depends on **no hornvale crate** — it is a pure
`vessel/session/v2` → character grid renderer, and that purity is worth more
than the convenience of breaking it. So:

- **core** owns the cursor: its position, its movement, its rendering, and a
  slot for a resolved string it is handed.
- **bin** (which depends on `hornvale-vessel`, in-process) answers *what is at
  this cell* and hands core the string.

A query is therefore a function call, not I/O, and costs nothing per keypress.

**Rejected:** shipping names in the snapshot. That puts 405 features — and
growing — into every document to answer one cursor.

### 3.2 Most specific first, by declared salience

Measured: of 5,734 multi-feature cells, **5,725 (99.84%) form a proper
containment chain**; only 9 are sibling overlaps. A cursor hits a stack, not
a contest.

So the Gazetteer's magnitude-descending order is **backwards here** — it names
the continent before the volcano. The cursor orders by **salience, declared
per class**, lower being more specific, matching the scene protocol's existing
sense (`Mark.salience`: agent 5, flagship 10, other settlement 20).

**Two orderings ship, and neither is "the" order.** They order different
questions, and each says so in its own doc comment:

| ordering | orders | answers |
|---|---|---|
| magnitude descending, identity ascending | within a class | "what are the important features" — the gazetteer page |
| salience ascending (declared per class) | across classes | "what am I pointing at" — the cursor |

Inside a containment chain, extent-ascending *is* specificity, since subset
implies smaller — so the two agree today by construction. They diverge the
moment a class arrives whose size does not track its specificity, which is
exactly why salience is **declared and not inferred**. A new class must state
where it sits.

### 3.3 Zoom does not separate a stack — and the spec says so because it is tempting

The resolution probe ran at the **finest granularity there is**: one geosphere
cell. Max depth 3 *there*. Zoom separates only what shares a cell by spatial
accident, and 99.84% of multi-feature cells are containment, which is
scale-invariant: a volcano is on its continent at every magnification.

**Depth 3 is a floor, not a binning artifact.** Any future design that
proposes zoom as the remedy for stack depth is proposing something the
measurement already refutes.

### 3.4 What the cursor says over nothing

**377 of 40,962 cells (0.92%) resolve to no feature** — 365 land below the
landmass individuation floor (the quantization-artifact islands the Gazetteer
deliberately leaves anonymous), 12 ocean outside the one named sea. A free
cursor lands there.

The strip states terrain without a name. It does not say "error", and it does
not invent one. An unnamed rock is a fact about the world.

### 3.5 A volcano is not always on land

`Volcano ⊂ Sea` is the **second most common** composition, 6.9% of
multi-feature cells: submarine volcanoes. Any renderer or resolver that
assumes a volcano sits on a landmass is wrong on real data. Recorded because
the spec author would have written that assumption in without checking.

---

## 4. The index, and what actually scales

**The feature stack for a cell is immutable for the world's lifetime.**
Terrain does not move. So `CellId → Vec<FeatureId>` is built **once at world
load and never invalidated** — not a per-turn cache. Marks and agents are the
live half and are resolved separately, per turn.

Containment chains grow **linearly** with the number of nested classes, not
combinatorially: adding ranges, provinces and biome regions yields
`Volcano ⊂ Range ⊂ Province ⊂ Landmass` — depth 6 or 8 in a dense world, not
26. The cursor stays cheap.

**What does not stay cheap is `Feature::extent`.** Every feature carries a
`BTreeSet<CellId>` of its whole extent — roughly 50k `CellId`s resident on
every `GeneratedTerrain` at seed 42 — and it reaches `clients/world-wasm` and
`clients/vessel/wasm` through `hornvale-worldgen` with **no non-test consumer
today** (the Gazetteer's final review flagged this). This campaign is its first
consumer, and it consumes it *once*, to build the index.

**Refusal, stated so a later campaign does not have to rediscover it:** this
campaign does NOT re-shape `Feature::extent`, because doing so under a
deadline to serve one consumer is how a data structure ends up shaped for its
first caller. The index makes the pressure visible and measurable; §7 F3 says
how it is measured.

---

## 5. The strip

At 80×24 — the floor, and `render` refuses smaller rather than degrading —
the plate is **40 columns**. Measured: the naive
`"Name (Class), on Name (Class)"` exceeds 40 characters **12.02%** of the time
overall and **100%** on 3-deep stacks (median 61).

So the strip carries **one name: the most specific**. The containment stack is
not lost — it goes to `!examine $@`, which has the entry pane's width.

**This is a fidelity cut and it is flagged as one** (§10). The strip drops
information that exists.

### 5.1 Where the strip goes

The endpaper today is an identity strip spanning the full width beneath both
panes. This campaign adds a **plate-width** strip; the endpaper keeps its job
and its row. The exact row assignment is an implementation choice constrained
by §5.2, and Task 2 reports what it chose.

### 5.2 Ornament may never occupy a cell that carries information

The spread's rule: gutters and rules carry no ink, and reserving them means
*not drawing there*, never a drawn border.

**The cursor obeys it by construction.** A blinking underline is a cell
*attribute*, not a glyph — it modifies the cell without consuming it, so the
terrain beneath stays readable. A drawn box or bracket would consume cells and
is refused by the same rule that refuses drawn borders elsewhere.

---

## 6. Layering and determinism

`clients/game` is outside the cargo workspace with its own toolchain
(`crossterm 0.29`), so the workspace dependency allowlist does not bind it —
and this campaign adds no dependency to it either way.

Decisions 0022 and 0055: the sim emits data, clients render, and the repo
boundary is the determinism boundary. A cursor is a client concern; the
*resolution* is a sim read. **Nothing in this campaign takes a seed draw,
moves a stream, or touches a save-format label. No epoch.**

---

## 7. What is unverified, and how each is settled

**F1 — the strip's row assignment against the 80×24 floor.** Adding a
plate-width strip costs a row the plate has today. **Settled by:** Task 2
renders at exactly 80×24 and reports the resulting plate height, and checks
`render` still refuses 79×24 rather than degrading.

**F2 — resolution cost per keypress.** Expected trivial (an index lookup plus
a name draw). **Settled by:** Task 3 measures it. **Decision rule:** above one
millisecond per cursor move, report it and cache the drawn name rather than
re-deriving per keypress; below, do nothing.

**F3 — the index's memory cost, and the extent pressure behind it.**
**Settled by:** Task 3 reports the index's size for seed 42 alongside the
extent total it was built from. This is the number a later campaign needs to
decide whether `Feature::extent` must change shape; producing it is this
campaign's contribution to that question, not solving it.

**F4 — does `vessel/session/v2` need to move?** Believed not: the cursor is
client state and the resolution is answered in-process by `bin`. **Settled
by:** Task 3 states plainly whether it changed the schema. **If it did, that
is a cross-repo contract change and a STOP** — additive-or-versioned only,
and it must be raised rather than absorbed.

---

## 8. Preregistered measurement

Frozen before the code that would move it (decision 0016).

**H1 — the strip is enough.** With most-specific-first, the strip's single
name fits 40 columns for **at least 95%** of resolvable cells at seed 42.
*Falsified if* it overflows more often, which would mean one name is not the
right unit and the cut in §5 is wrong.

**H2 — the cursor is predictable.** The same cell resolves to the same name
on repeat visits within a session, and across a save/reload. *Falsified by*
any variation — which would mean the ordering is not total and §3.2's
declared salience has a gap.

**H3 — the plate survives the floor.** At exactly 80×24, with the strip added,
the plate still renders and `render` still refuses 79×24. *Falsified by* the
floor moving, which would break "monochrome at 80×24 is the floor" — a
constraint this campaign inherits and may not weaken.

**H1 is the one at real risk**, and its null is accepted in advance: if one
name does not fit, the finding is that the strip needs abbreviation or a
second row, and that is a legitimate result to ship rather than repair by
quietly widening the floor.

---

## 9. Out of scope, carried forward

- **The `!examine $@` syntax itself.** The shape is adopted (an OOC prefix, a
  cursor variable); the exact spelling is deliberately not frozen here —
  Nathan set it aside as not the point yet.
- **The walk / chamber / delve resolvers.** The cursor addresses them; they
  answer "nothing here yet". Wiring them is the natural next campaign and
  needs no change to anything this one ships.
- **Re-shaping `Feature::extent`** (§4).
- **The fog** — names learned rather than given. Campaign 3.
- **Mouse support.** `crossterm` offers it; a cursor moved by key is the
  primitive, and a mouse would be an alternate driver for the same selection.

---

## 10. Flagged for Nathan at G3

- **A fidelity cut (§5):** the strip shows one name where a containment stack
  exists. Measured: the full stack overflows 40 columns 12% of the time and
  always on 3-deep stacks. The stack survives in `!examine`, but the strip
  drops information. **Cuts are Nathan's call.**
- **Two orderings of one feature set (§3.2).** Justified — they answer
  different questions — but this repo distrusts two orderings for good
  reasons, and the justification should be checked rather than accepted.

---

## 11. Decision to promote

> **A cursor points into whatever the plate drew, and the plate's author says
> what is there.** Selection has two addressing modes — by name and by
> position — and they resolve to the same datum. A contested position resolves
> by *declared per-class salience*, most specific first, because features
> nest rather than compete; ordering by magnitude answers a different question
> and belongs to a different surface.

---

## 12. Task outline

**Stage 1 — the cursor**
1. Cursor state and rendering in `hornvale-game-core`: position, key
   movement, blinking underline as a cell attribute. No resolution yet.
2. The strip: plate-width, beneath the plate, at the 80×24 floor. F1, H3.

**Stage 2 — resolution**
3. `CellId → Vec<FeatureId>` built once at load; declared per-class salience;
   `bin` answers the query and hands core a string. F2, F3, F4.
4. H1 and H2.

**Stage 3 — close**
5. Chronicle, retrospective, registry rows, decision, freshness sweep.
   Delete both spike instruments.
