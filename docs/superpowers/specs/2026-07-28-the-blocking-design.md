# The Blocking — the lattice, and rooms worth drawing

**Campaign:** The Blocking — campaign 1 of the Rose Window's **render arc**, the
three-campaign carve agreed 2026-07-28 (this, then The Sighting, then The Panes).
**Date:** 2026-07-28 · **Status:** spec approved at G3; amended 2026-07-28 by
ledger #7–#12 (the extent derivation §3.4, per-method layout labels §3.2/§5.1, the
measured epoch §5.2, what the stamp records §5.3, the two role-table constraints
§4.1), by ledger #23–#27 during execution (**a wall is a cell**, §3.3/§3.4/§7,
at Nathan's direction), and at close (the measured solve cost, §10 risk 1; which
flagged items survived, §12.1; the declined bump, §8/§11). Those amendments
post-date G3 and lead the **G6** digest.
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

**Each method gets its own stream label**, fully qualified:
`room/layout/v1/rectilinear` and `room/layout/v1/grown` (ledger #7). The unit of
independent change is the *algorithm* — retuning the grower's flood order has
nothing to do with the BSP cut band — and 0073 fixes epoch granularity at
declaration, so the split is made before either label has ever been bumped. A
label is declared only for a method that **exists**: the two predicted methods
above get none until they have code, because a label is permanent once published.
A change to something the methods *share* — the extent derivation, `Lattice`'s
meaning — bumps both.

### 3.3 What the lattice is, and is not

- Cells are **`FRAME`-tier** and never serialized (0069, unchanged). The lattice
  is derived from `(brief, address, seed)` and re-derived on entry.
- A **wall is a CELL that is occupied**, not a property of the boundary between
  two cells. *Amended in Task 4b at Nathan's direction; this section originally
  read "a wall is a non-adjacency, definitionally".* A wall cell is impassable,
  and a drawn wall with no impassable cell under it is a lie that §7's checker
  fails on. The amendment buys a 1:1 render, the model every tilemap engine
  already speaks, a threshold that is a *place*, and a location for the `Screen`
  and `Alcove` anchor kinds that already ship. It concedes thickness, which is
  *more* faithful for turf, cob and rubble-stone building, not less.
- The lattice covers **one structure**, not a region: locale-scale space stays
  topological (Amendment 2's law 2 — adjudication changes kind at the band break).

### 3.4 The extent: how big a plan is

The plan is **as big as the rooms it must hold, plus the fabric between them.**
`extent_for(structure)` is a pure function of the **chamber count** — one named
per-chamber interior side, a block arrangement for 1..=`MAX_CHAMBERS`, anchored at
the origin. It reads no brief field and **consumes no draw**, which is what keeps
§7 rule 7's DOF count equal to the cut positions alone: extent is a coarse
constraint, not a die roll.

*Amended in Task 4b*, where a wall became a cell and therefore started costing
one. Wall lines are one more than the interiors they separate, so the extent is
`cols * CHAMBER_SIDE + (cols + 1)` per axis: 10×10 at one chamber, 19×10 at two,
19×19 at three or four. **Roughly a fifth to two fifths of the extent is exterior
shell** — measured at 19% for three and four chambers, 28% at two and 36% at one,
because a ring's cost is a perimeter against an area and the smallest plan pays
the most. That is a deliberate cost rather than overhead: the drawn border is what
makes the picture read as a **building** rather than as a floating partition
diagram.

Two axes were considered and rejected, both for the same reason (ledger #8):

- **`peak_population`** already governs how *many* buildings a settlement has
  (`structures_of` in `domains/history/src/flesh.rs`), not how big one of them is.
  Reading it here would double-count one signal.
- **`notability`** would make a Seat's plan grander — but `brief.notability`
  describes only the *alive* occupation, and a building's shell outlives its
  occupants. Deriving floor area from a living fact makes a building **shrink when
  its people leave**. Unreachable today (nothing built stands at a ruin: `built`
  is settlement membership), which is exactly why the rule is written down now,
  while it is still free — the same closing window as §5.1's.

Grandeur therefore lives in §4.1's `hall` role and its high seat — in what a room
*contains*. Expressing it as floor area instead would be the catalogue reading of
the same fact, which §4.3 forbids.

**The ceiling is a test, not a taste.** A plan is read in an 80-column transcript,
so the largest extent any chamber count can produce must fit one, asserted over
every count rather than chosen by hand and hoped for.

Deferred with a home: a **durable** extent — what the shell remembers rather than
what the tenants are — needs the ruin signature the brief deliberately omits, and
arrives with it. Its most interesting form is *tech as a material span cap*: a
pre-industrial room is as wide as a roof beam can span, so `tech` would bound a
chamber's width rather than reward it. Recorded in the idea registry, not built.

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

Two constraints on that table, both verified rather than assumed (ledger #12):

- **`peak_population` is not on the brief today.** The brief carries `function`,
  `tech`, `notability`, `people`, `built`, `cold`. The `store` role is its first
  consumer, so the field is added where it is first read — free, since nothing
  here is serialized (`brief.rs`'s own module doc licenses exactly this) — and the
  hamlet ceiling is *hoisted* from `flesh.rs` rather than re-typed, so one number
  does not come to mean two things.
- **The headline may not depend on `cold` or on `Seat`.** The structure the
  seed-42 walk enters has two chambers, sits in tropical seasonal forest, and is
  not a Seat — so every `needs_cold` pattern is filtered out and no high seat ever
  draws. A role table whose differentiation lives in the hearth patterns or in
  `notability == Seat` leaves the flagship transcript **unchanged** and the
  campaign's headline unobserved with every check green. The differentiation that
  carries the headline is the distribution of the warm built patterns already in
  the inventory. Note also that every chamber has at least one link, so a doorway
  cannot be the threshold role's private property.

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

### 5.1 Four labels, and the bump is conditional

```
  label                  governs                  this campaign   if bumped
  --------------------   ----------------------   -------------   -------------------
  room/chambers/v1       WHICH chambers exist     DO NOT TOUCH    free today; orphans
                                                                  every in-chamber mark
                                                                  the moment one exists
  room/furnishing/v1     WHICH patterns a place   BUMP -> v2 IF   interiors move, so
                         draws                    A LIVE READ     warmth moves, so
                                                  MOVES (§5.2)    committed drive history
                                                                  moves
  room/layout/v1/        WHERE the rectilinear    DECLARE, as     nothing yet — this is
    rectilinear          method puts them         causal          its first use
  room/layout/v1/        WHERE the growing        DECLARE, as     nothing yet — this is
    grown                method puts them         causal          its first use
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

### 5.2 Byte-identity: what actually breaks is measured, not assumed

This section previously asserted that the epoch was unavoidable. **It is a
prediction, and the plan tests it** (ledger #10). Two verified facts changed the
shape of this section, and both were found by grepping rather than by reasoning:

1. **`ROOM_FURNISHING` has exactly one occurrence in the workspace — its own
   declaration. Nothing draws from it.** So `room/furnishing/v1 → v2` re-mints
   nothing on its own; by itself it is a declaration, not a cause.
2. **The band a creature stands in is the locale, not the chamber.** NPC thermal
   drives read `interior_of`; `chamber_interior_of` is read only by the chamber
   renderer. And `selection` iterates the inventory in order and filters, so
   *appending* role-gated patterns leaves every existing `(built, cold)` selection
   byte-identical.

Together those mean chamber differentiation can be built out of the **nine
patterns that already exist** — the role decides which of them a chamber draws —
and that new anchor kinds can be appended behind gates no locale composition
opens. Three outcomes are therefore possible, and the vocabulary matters because
two of them are routinely mistaken for each other:

```
  RE-PIN   transcripts move; no metric golden moves.  NOT an epoch. Re-pin.
  EPOCH    a metric or census golden moves.           Health battery becomes the
                                                      GATE; galleries re-pin in an
                                                      isolated commit; census regen
                                                      needs Nathan's authorization.
  LATENT   nothing moves, because every new pattern
           is gated out of every live composition.    Cheapest today, and a TRAP.
```

**If the outcome is EPOCH:**

- **The health battery becomes the GATE, not a check.** The Hearth and The Lintel
  were both designed to avoid this posture; pretending a gate is a check is how a
  calibration failure gets mistaken for expected drift.
- The census re-pins if `hearth_population_calibration` moves. **Census
  regeneration is an autopilot carve-out** — Nathan's explicit authorization,
  requested at the point of need, never assumed.
- Preregistered study pins must be **invariants** (ordering, sign, family
  membership, "stays zero"), never values, or every future epoch reopens them
  (Amendment 1 §1a.5(b)).

**If the outcome is LATENT, the response is mandatory, not optional.** The
inventory would then hold patterns no live composition admits, and the
discontinuity is merely deferred to whichever campaign opens the gate. The gate
condition goes in `INVENTORY`'s own doc comment — which today says flatly that
adding a pattern *is* an epoch. That sentence becomes over-strict the moment role
gating lands, and an over-strict warning is one that gets ignored, which is
precisely how an **undeclared** epoch ships.

Stated plainly because the polarity is easy to get backwards: an epoch is not
only a cost. It is the one mechanism by which a world frozen by its own goldens is
allowed to improve. Avoiding one is not automatically the win.

**The mitigation, whichever outcome lands:** committed facts are never
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

**What the stamp records: the versioned labels and their versions** — not an
opaque counter (ledger #9). A reload then *diffs two label sets* and can name what
moved — the rooms, or the furnishings — instead of issuing one generic warning
about an unspecified rearrangement. The data already exists: every crate publishes
`stream_labels()`, and the book renders a manifest page from it. A counter would be
a second hand-maintained encoding of a fact the tree already knows, and would
drift from it.

Only labels carrying a `/vN` segment are recorded. An unversioned label is
structural and must never move (0073's argument for `room/face` and `room/child`),
so including one would add a row that can never differ.

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
stays byte-identical.

*Corrected in Task 5:* this section originally added "only chamber-to-chamber and
locale-to-locale movement touches committed state," which reads as a statement
about today and is not one. **Nothing commits at chamber granularity at all** —
that is this campaign's own `room/chambers/v1` constraint (§5.1), and it is why
bumping that label is still free. A threshold crossing is therefore no more
`COMMIT`-tier than a cell step is; what distinguishes it is that it re-renders the
chamber, which is a *rendering* difference, not a persistence one. The sentence was
describing the model this program is heading toward, and saying so in the present
tense is how the window in §5.1 gets mistaken for a debt already owed.

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
  8  reachability every Floor cell is reachable from the threshold chamber,
                  through passable cells only
```

Rule 7 is what makes §2's embedder discipline checkable rather than aspirational:
if the solver's residual DOF exceeds what the graph leaves free, it is inventing.

**Amendment 2 §1b.8 listed seven rules; this model earns an eighth.** Rule 8 is
not a bonus check, it is the price of §3.3's amendment paid out loud. Under the
boundary model connectivity was guaranteed by construction — regions tiled the
extent and doorways linked them, so there was nowhere for a mover to be stranded.
Walls as *cells* can **seal a pocket of floor**, and a sealed pocket is a room a
player can see on the plan and never enter. So the grower claims with a separation
rule and never takes a cell back, which makes rule 8 hold by argument rather than
by luck, and rule 8 is what makes the argument falsifiable.

Two of the seven also change form, and both get *stronger*:

- **Rule 2** was "every drawn wall IS a non-adjacency", a claim about a
  separately-derived set of cell pairs. It is now "two `Floor` cells of different
  chambers are never adjacent" — a claim about the world rather than about the
  derivation.
- **Rule 3 stops being tautological.** Under the boundary model it was the
  contrapositive of the wall derivation's own exemption condition read back off
  the same ownership map. It now asserts that the kind map is total, that the
  extent's **outer ring is entirely `Wall`** — the plan is enclosed, which an
  embedder could fail to do and which the boundary model had nothing to say about
  — and that thresholds and doorways name each other in both directions.

## 8. Scope

**In:** the lattice and its two layout methods; the ASCII render and its verb;
chamber roles and the pattern vocabulary they need; ~~the `room/furnishing/v2`
bump~~ **(declined — measured unnecessary, decision 0084; see §12.1)** and the
`room/layout/v1` declaration; the epoch stamp; the checker; intra-chamber `go`;
the parity test.

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

1. **The solve's cost is unmeasured.** ~~The Rose Window metaplan §5 caveat 2
   still stands, and the placement-scan floor measured during that brainstorm is
   *not* a solver. Measure a real embedding before any budget claim reaches this
   spec.~~ **CLOSED — measured, 2026-07-28.** One `allocate` at the 19×19
   four-chamber extent is **~9 µs release** (median 9667 ns then 8709 ns across
   two runs, min 8375, p99 14250) and **174.6 µs debug** — a ~19× gap. Two
   orders of magnitude under the 1000 µs ceiling. A real embedding, not a
   placement scan.

   **The profile is part of the number.** Three earlier figures in the ledger
   are **superseded**: 6.79 µs release at the pre-`owner` 16×16 extent (#15);
   27.6 µs release after per-cell ownership landed (#18); and 182.9 µs at 19×19
   with **no profile stated** (#28), which this measurement identifies as the
   *debug* figure — release had never been taken at that extent. A number
   without a build profile is not a measurement, and this spec's own risk row is
   where that rule earns its keep: quoting 182.9 µs here would have recorded a
   ~20× pessimism as the cost of the solve. The gate-run cost assertion is
   therefore keyed to the **debug** ceiling, because that is the profile the gate
   runs in; a ceiling keyed to the release figure looks rigorous and is flaky.
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

*Amended at close:* the health battery was green **as a check**, not as a gate,
because no epoch occurred (§12.1) — the clause above assumed the outcome the
measurement reversed. The gallery re-pin did land in its own isolated commit
(`67676f3b`), and the two decisions recorded are 0083 (the layout labels'
granularity) and 0084 (the furnishing bump, *declined*).

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

### 12.1 Which flagged items survived contact — recorded at close

The package the owner approved led with an item that **did not happen**, and
that is recorded here rather than quietly dropped.

- **Item 1 — REVERSED.** No epoch. The outcome was **RE-PIN**: exactly one
  committed file moved (the seed-42 possession transcript), no metric golden and
  no census golden moved, and `make gate` came back green at 2413/2413 **as a
  check**. `room/furnishing` stays at v1; a bump with no moved derivation would
  be an *empty* epoch. Ratified as decision 0084. The discontinuity is
  **deferred, not absent** — five patterns carry `at_locale: false` and are
  unreachable from any live read, and the gate that opens them is the first mark
  committed inside a chamber.
- **Item 2 — DID NOT ARISE.** No census re-pin, because no census golden moved.
  No authorization was requested and none was needed.
- **Item 3 — HALF SURVIVED.** `room/layout/v1/rectilinear` and
  `room/layout/v1/grown` were declared causal (decision 0083, one label per
  *algorithm*, none for a predicted method). The `room/furnishing/v1 → v2` half
  is the item 1 declination.
- **Item 4 — SURVIVED AS FLAGGED.** The stamp shipped: a `#[serde(default)]`
  field on `World`, written by the composition root, recording ten versioned
  label rows for a seed-42 world with the version segment stripped from the key
  so a bump is a *value* change. It is schema-adjacent exactly as flagged, and it
  nearly shipped recording a **retired** label as current (a roster keeps retired
  rows, marked only in prose); the fix is highest-version-wins, compared
  numerically.
- **Item 5 — SURVIVED AS FLAGGED.** The Lintel's indoor `go` refusal is
  reversed, and both The Lintel's chronicle and its spec carry an inline note
  saying so, with the reason it is not a flip-flop (§6.1). The band law is
  unchanged; the *inference* drawn from it changed. `back` stays refused indoors.
- **Item 6 — SURVIVED AS FLAGGED, and is now permanent.** The structural half of
  the parity contract holds and the tested half is a test rather than an
  intention. Any future pane capability must first be a verb.

One item the package did **not** flag turned out to be the campaign's largest
model change: **a wall became a cell rather than a boundary**, at the owner's
direction mid-execution (§3.3, §3.4, §7). It reworked Tasks 1–4's lattice core,
retired the doubled render and its coordinate mapping, and earned the eighth
checker rule. A mid-campaign model change of that size arriving unflagged is
itself the finding: the spec had asserted the boundary model as definitional
("a wall is a non-adjacency, definitionally") rather than as a choice, so there
was no flag position for it to occupy.

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
