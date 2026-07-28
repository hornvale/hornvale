# The Blocking — the lattice, and rooms worth drawing

**Campaign:** The Blocking — campaign 1 of the Rose Window's **render arc**, the
three-campaign carve agreed 2026-07-28 (this, then The Sighting, then The Panes).
**Date:** 2026-07-28 · **Status:** spec, awaiting G3
**Parent:** `2026-07-25-the-rose-window-metaplan-design.md` **Amendment 2** (§1b)
— read that first; this spec builds only on the amended program.
**Prior rung:** The Lintel (`2026-07-27-the-lintel-design.md`), which made the
chamber band exist and reachable but left every chamber of a structure reading
identically.
**Decisions in force:** 0069 (fine position is never serialized), 0072 (derived
geometry is causal), 0073 (epoch granularity is declared), 0075 **as superseded
by Amendment 2 §1b.7** — and this campaign is the first to *exercise* the
supersession, 0009 (models author, dice roll), 0016 (studies preregister), 0082
(locale / chamber / place).
**Ledger:** `.superpowers/sdd/decision-ledger.md`

---

## 1. What this is

**A chamber gets a floor plan, and a structure's chambers stop being identical.**

Two deliverables, and the second is what makes the first worth having:

1. **The lattice** — a structure's chambers laid out as regions of *one* grid,
   with walls and doorways, derived from the anchor graph and rendered as ASCII
   through a verb. That is Wolverson's chapter 4 in the medium we already ship.
2. **Differentiation** — chambers that differ from one another, and structures
   that differ by what the world knows about them, driven by the brief axes
   The Lintel carries and never reads.

It ships **no field of view, no pathfinding over cells, and no graphical pane.**
Those are The Sighting and The Panes. What it ships is playable the day it lands,
in the transcript pane that already exists.

**Why the two must be one campaign.** A floor plan of four identical rooms is a
prettier rendering of the thinness The Lintel shipped, not a fix for it — and
both halves need the same epoch (§5), so paying for one epoch and getting both is
strictly better than paying twice.

## 2. The inversion, which reframes the whole problem

Wolverson's chapter 4 runs **map → contents**: BSP invents rooms, then things are
placed in them. We have the opposite problem. The anchor graph already exists —
chambers, their contents, their connections — so we run **contents → map**.

That is not dungeon generation. It is **floor-plan synthesis**: given an
adjacency graph, produce a subdivision whose regions have exactly those
adjacencies. The literature is rectangular duals and orthogonal graph drawing,
and the change of goal matters: **a generator is judged by variety, an embedding
by fidelity.**

So the thing this campaign builds is an **embedder, not a generator**, and its
discipline follows directly: it may add **no information beyond the residual
degrees of freedom** (Amendment 1 §1a.7). Where the graph determines something,
the embedder must not re-decide it; where the graph leaves freedom, the seed fills
exactly that much and no more.

## 3. The layout method

### 3.1 BSP, used inversely

`structure_at` builds a **path graph** rooted at the threshold chamber
(`links = (1..len).map(|i| (i-1, i))`), so v1's embedding is a chain of splits:
recursively divide the structure's rectangle, allocating each part to the next
chamber, proportionally to how much that chamber holds. Adjacency is then realized
**by construction** rather than checked afterwards, because consecutive splits
share an edge and that edge is where the doorway goes.

Integer arithmetic throughout — no floats enter the layout path, so cross-platform
byte-identity holds by the same argument the rest of the world uses.

It is the algorithm the tutorial teaches, run backwards: splitting to *allocate*
space among chambers we already have rather than to *invent* rooms we don't.

### 3.2 The method is brief-selected, and one method is not enough

Charting place kind against the geometry each wants predicts where a single
solver fails:

```
                rectilinear   organic     radial/axial   branching
  dwelling      BSP alloc     —           —              —
  fort          BSP alloc     —           —              —
  cave          —             grow        —              —
  temple/cult   —             —           PREDICTED      —
  mine          —             —           —              PREDICTED
  ruin          BSP + wear    grow + wear —              —
```

The empty cells are **typed predictions, not gaps**: a temple wants an axis and a
focus; a mine wants tunnels branching off a shaft. Neither is served by splitting
a rectangle.

**v1 ships two methods** — rectilinear allocation for built places, region growing
for wild ones — selected on `built`, which is the one brief field the code already
reads. Radial and branching are named here as predicted so they arrive as scope
rather than as surprise, and the selection seam is the same shape patterns already
use: the brief chooses.

### 3.3 What the lattice is, and is not

- Cells are **`FRAME`-tier** and never serialized (0069, unchanged). The lattice
  is derived from `(brief, address, seed)` and re-derived on entry.
- A **wall is a non-adjacency**, definitionally. A drawn wall with no
  corresponding non-adjacency is a lie, and §7's checker fails on it.
- The lattice covers **one structure**, not a region: locale-scale space stays
  topological (Amendment 2's law 2 — adjudication changes kind at the band break).

## 4. Differentiation

### 4.1 Chamber roles

A structure's chambers take **roles**, and a role admits a different pattern
subset. Roles derive from the brief:

```
  role         admitted where                      draws
  ----------   ---------------------------------   ----------------------------
  threshold    always (the chamber `enter` lands)  the-threshold, the-screen
  hearthroom   always                              the-fire, the-fireside-bed
  store        peak_population above the hamlet    the-water-jar, +the-strongbox
               floor
  hall         notability == Seat                   +the-high-seat
  workroom     function-dependent                  +the-loom / +the-anvil /
                                                    +the-altar
```

This is the pattern language one rung finer: patterns complete patterns, and a
role is a bundle of patterns that complete each other. It is also
`CLIENT-district-patterns`' shape one band *down* — the same composer, a
different declared vocabulary.

### 4.2 What that costs: new kinds and new patterns

Differentiation needs vocabulary the frozen inventory does not have. v1 adds a
**small, closed set**: roughly four anchor kinds (`Loom`, `Anvil`, `Altar`,
`Strongbox`, `HighSeat` — final list settled in the plan) and one pattern each,
plus the role-selection layer.

Two consequences, both good:

- The **exhaustive `noun` match** in `chamber_prose.rs` will fail to compile until
  every new kind has prose. That guard was built deliberately in The Lintel and
  this is its first real use.
- Adding patterns is an **epoch** by `ROOM_FURNISHING`'s own doc comment, which is
  §5.

### 4.3 The discipline that keeps this a language

`CLIENT-language-not-catalogue` binds: if this campaign's substance turns out to
be *how many* patterns exist, it has gone wrong. The substance is **which roles a
brief admits and which patterns complete which** — adjacency and composition. A
reviewer should be able to point at the role table and the `requires` clauses as
the campaign's content, and at the pattern count as an incidental.

## 5. The epoch, and what it breaks

### 5.1 Three labels, and only one bumps

```
  label                  governs                  this campaign   if bumped
  --------------------   ----------------------   -------------   -------------------
  room/chambers/v1       WHICH chambers exist     DO NOT TOUCH    free today; orphans
                                                                  every in-chamber mark
                                                                  the moment one exists
  room/furnishing/v1     WHICH patterns a place   BUMP -> v2      interiors move, so
                         draws                                    warmth moves, so
                                                                  committed drive history
                                                                  moves
  room/layout/vN         WHERE the solver puts     DECLARE, as    nothing yet — this is
                         them                      causal          its first use
```

**`room/chambers/v1` must not move — and the honest reason is a window that is
closing, not a debt already owed.** Verified: nothing commits at chamber
granularity today. The session's committed facts carry `place: None`, and The
Lintel deliberately commits nothing for a descent, so bumping the chamber label
right now would orphan *nothing*.

That is precisely why the discipline belongs in writing before it stops being
free. The first mark made *inside* a chamber — a burned trading post, an emplaced
object, anything with a chamber address in its `place` — turns every existing
chamber address into a reference, and from that moment a bump silently relocates
history. Bumping furnishing while leaving chambers alone is exactly the
blast-radius split decision 0073 declared two labels for, and this campaign is
where the split first pays: it changes what a room *contains* without touching
what identifies it.

### 5.2 Byte-identity breaks, deliberately, for the first time

New patterns change composed interiors, which changes warmth at an anchor, which
changes committed NPC drive history. So:

- **The health battery becomes the GATE, not a check.** The Hearth and The Lintel
  were both designed to avoid this posture; this campaign cannot, and pretending
  otherwise is how a calibration failure gets mistaken for expected drift.
- The seed-42 galleries re-pin.
- The census probably re-pins too, since `hearth_population_calibration` reads
  interiors. **Census regeneration is an autopilot carve-out** — it needs Nathan's
  explicit authorization, requested at the point of need, never assumed.
- Preregistered study pins must be **invariants** (ordering, sign, family
  membership, "stays zero"), never values, or every future epoch reopens them
  (Amendment 1 §1a.5(b)).

**The mitigation, stated rather than implied:** committed facts are never
retro-changed. History survives an epoch; only future outcomes differ. Worlds are
reproducible *within* an epoch, not across one.

One genuine advantage over the obvious sibling: Minecraft-style epoch bumps leave
visible seams because old chunks are *kept*. We keep nothing but marks, so
re-derivation is total and there are no seams.

### 5.3 The epoch stamp — a thing Amendment 1 asked for and nothing implements

§1a.5 says the player-facing consequence — *history survives, but remembered
places rearrange* — "should be stated rather than discovered." Nothing states it,
because nothing records which epoch a world was made under.

**Record the epoch in the world.** A reload after a bump can then say *"you have
been away; the rooms are not as you remember"* rather than silently rearranging
someone's memory of a place. Small, and it turns a betrayal into a diegetic event.
This is the one place this campaign writes a new datum into a world, and it is
metadata about derivation rather than derived content.

## 6. The parity contract

**Structural half, which is the one that holds: a pane input synthesizes a
command. No pane gets a private path to the simulation.** An arrow key emits
`go n`; the existing verb runs. Parity cannot drift, because there is one
implementation and the map is a keyboard-shortcut layer over the command language.

The consequence is accepted deliberately: **any new pane capability must first be
a verb.** Nothing expressible only by pointing — no drag-select, no mouse-look.
That is what keeps the command language complete as the interface grows, and it is
what makes the screen-reader requirement hold automatically instead of by
vigilance.

**Tested half, in this campaign, with no client needed:** every noun the lattice
render depicts is `examine`-able, and every destination it depicts is reachable by
a named command. This generalizes The Purview's
`examine_accepts_exactly_the_union_of_both_grains`, which is the same test at one
pane — and it is exactly the class of defect that shipped in The Lintel, where
`look` named a water jar `examine` then denied.

### 6.1 This campaign reverses The Lintel's indoor `go` refusal

The Lintel made `go <dir>` refuse indoors and corrected four documents to say so.
That was right: there was nowhere inside to walk. **This campaign creates
somewhere**, so compass movement inside becomes meaningful and `go n` means one
cell north.

Stated plainly so the history does not read as flip-flopping: the refusal was
correct for a world with no intra-chamber space, and this campaign's purpose is
creating that space. The alternative — a second verb (`step n`) preserving the new
meaning of `go` — is rejected: two movement vocabularies for one action is worse
for the player and worse for parity, since arrow keys should map to the verb a
player would actually type.

Intra-chamber movement is **`FRAME`-tier**, so 0069 holds and re-walking a room
stays byte-identical. Only chamber-to-chamber and locale-to-locale movement
touches committed state.

## 7. The checker

Amendment 2 §1b.8's seven rules, now realizable because the lattice exists:

```
  1  soundness    every relation the specified graph asserts is realized
                  in the solved lattice
  2  wall law     every drawn wall IS a non-adjacency; no decorative walls
  3  closure      a fine place's boundary maps entirely onto thresholds
  4  doorways     a shared threshold derives from the EDGE, so two adjacent
                  chambers cannot disagree about it
  5  occupancy    at most one creature per cell
  6  determinism  same (brief, address, seed) -> identical lattice, solved
                  from scratch, no carried state
  7  DOF          residual degrees of freedom reported as a number
```

Rule 7 is what makes §2's embedder discipline checkable rather than aspirational:
if the solver's residual DOF exceeds what the graph leaves free, it is inventing.

## 8. Scope

**In:** the lattice and its two layout methods; the ASCII render and its verb;
chamber roles and the pattern vocabulary they need; the `room/furnishing/v2` bump
and the `room/layout/v1` declaration; the epoch stamp; the checker; intra-chamber
`go`; the parity test.

**Out, with a home:**

- Field of view, cell pathfinding, creature cells — **The Sighting**.
- The wasm ABI, the pane host, focus arbitration, canvas rendering, the sky
  tenant — **The Panes**.
- Radial and branching layout methods (§3.2's predictions) — later campaigns,
  selected by the same seam.
- Player-chosen placement of anything; a named backward aperture (`further out`);
  counting rather than repeating duplicate anchor kinds — recorded followups.

## 9. Success criteria

- **`map` inside a house draws a floor plan** — observed in a transcript, and in
  the committed seed-42 gallery, not demonstrated in a unit test.
- **Two chambers of one structure read and draw differently**, and a `Fort`
  differs from an `Agrarian` place — asserted over a sweep, not one case.
- **The checker's seven rules hold** over generated structures, each its own test.
- **The parity test passes:** every depicted noun is `examine`-able, every depicted
  destination command-reachable.
- **The epoch is honest:** the health battery passes *as the gate*, the galleries
  re-pin in their own commit, and the epoch stamp appears in a world.
- **Determinism:** same seed and brief yield an identical lattice; no float enters
  the layout path.

## 10. Risks

1. **The solve's cost is unmeasured.** The Rose Window metaplan §5 caveat 2 still
   stands, and the placement-scan floor measured during that brainstorm is *not* a
   solver. Measure a real embedding before any budget claim reaches this spec.
2. **The epoch's blast radius is the largest this program has taken.** The health
   battery becomes the gate; a census re-pin needs authorization. The failure mode
   is mistaking a real calibration regression for expected drift.
3. **Differentiation could become a template catalogue** (§4.3). The tell is a
   campaign whose substance is pattern count.
4. **Reversing The Lintel's `go` refusal touches four documents again** (§6.1) —
   cheap, but it must be deliberate and stated, not quietly re-amended.
5. **Legibility is not fidelity.** Amendment 1 §1a.7 named sightlines as the
   criterion and this campaign is where it gets tested: an embedding can be
   perfectly faithful and still read as a maze.

## 11. Definition of Done

`make gate` green with the health battery as the gate; `make gate-full` before
merge; type-audit clean; galleries re-pinned in an isolated commit; the epoch
recorded in `docs/decisions/` (the layout label's causality and the furnishing
bump); chronicle entry; freshness sweep of the room-mesh and possession chapters;
retrospective; registry rows flipped (`CLIENT-refinement-checker`,
`CLIENT-district-patterns` partially, `CLIENT-tile-view`) with **Where**
repointed; Confidence Gradient re-scored if a bet moves.

## 12. Flagged for G3

1. **Byte-identity breaks and the health battery becomes the gate** (§5.2) — the
   first time in this program. Leads the package.
2. **A census re-pin is likely and needs explicit authorization** (§5.2).
3. **`room/furnishing/v1 → v2`, and `room/layout/v1` declared causal** (§5.1) —
   save-format-class label decisions.
4. **The epoch stamp writes a new datum into a world** (§5.3) — small, but it is
   world metadata and therefore schema-adjacent.
5. **This campaign reverses a decision The Lintel just shipped** (§6.1).
6. **The parity contract forbids pointer-only capabilities forever** (§6) — a
   permanent constraint on every future pane, accepted for accessibility.

## 13. Provenance

Brainstormed 2026-07-28 under autopilot, following The Lintel's merge. Nathan
supplied the target (bracketproductions chapters 4–6), the multi-pane framing with
its Rogue-plus-Zork synthesis, the screen-reader parity requirement that produced
§6, and the main-pane-as-slot idea that made the sky a tenant rather than a
special case. Ideonomy passes: the carve (1, one overturn — a two-campaign carve
failed because a floor plan of identical rooms is not an observable end), the
layout inversion (1, one overturn — negation produced contents → map), and the
epoch (1, cross-domain re-instantiation produced the marks-orphaning constraint
and the epoch stamp).
