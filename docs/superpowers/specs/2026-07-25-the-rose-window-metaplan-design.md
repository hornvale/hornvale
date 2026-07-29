# The Rose Window: A Program Metaplan — Design

**Date:** 2026-07-25
**Status:** Amended 2026-07-25 (Amendment 1, §1a — owner-decided; two further ideonomy passes). Body below predates it where marked.
**Parent spec:** `2026-07-05-hornvale-longterm-plan-design.md` (Constitution §3.5 "The Game" and §7's deferral of graphical clients); `2026-07-11-the-walk-metaplan-design.md` (the game layer's spine — this drains its RENDER arc)
**Worktree:** `the-rose-window` (branch `the-rose-window`), off `main` at `3caf9055`
**Autopilot:** engaged (G3/G6 hard stops; ledger at `.superpowers/sdd/decision-ledger.md`)
**Frontier:** the `CLIENT-*` atlas, `book/src/frontier/idea-registry.md` (24 rows; `RENDER-4` elaborated into it)

---

## 1. What this is

A **metaplan**, not a single campaign: the arc from Hornvale's current
observation surfaces — a static atlas, an external 3D Orrery, and the
Casement's single prose pane — to a **multi-pane browser client in which a
possessed agent's immediate surroundings are drawn as well as narrated.**

The name is the Casement's own word, extended. A casement is a pane in a
document through which the document's subject runs live. Several lights on
one interior, each admitting differently, is a rose window — and the
"admitting differently" is not decoration but the literal structure of §4.3:
each pane is a different epistemic channel with a different truncation.

Two framings were tested and discarded during brainstorm, and their
discarding is the reason this is a metaplan:

- **"A web game client."** False. The JS is the small end. The load-bearing
  work is producer-side: a per-turn structured emit, an intra-room tile
  layer, a vitality model, and time at action granularity. Naming it a
  client campaign would have hidden four-fifths of it.
- **"One campaign."** False. The pieces have a **T** dependency shape — one
  narrow contract (§6.1) that everything hangs from, then three largely
  independent bodies of work. That is precisely the property that lets them
  run concurrently across worktrees, as the Walk metaplan was designed for.

This metaplan fixes the spine, the load-bearing laws, the sequencing, and
the evidence. Each campaign named in §6 gets its own spec → plan → execution
cycle.

## 1a. Amendment 1 (2026-07-25) — the fine layer is relational

**Status of this amendment:** owner-decided in conversation, 2026-07-25. It
settles both of §10's open forks (items 1 and 4), supersedes §6.3, reframes §9's
risk 2, and narrows §2's claim about the program's identity. Read it before the
sections it supersedes; those carry inline pointers back here.

### 1a.1 The correction that forced the rest

The metaplan prices the program as a *rendering* surface — "situated and
spatial," the two empty rungs. The owner's restatement of purpose reorders it:

> The main purpose of the text interface is as a substrate for the linguistic,
> economic, behavioral complexity. That's how we talk to characters, read
> letters, research magic, learn languages, reverse-engineer magic artifacts.

Under that purpose the tile grid was answering a question nobody asked. Walk the
named activities and the requirement is the same shape every time — **who is
present, who can hear, whether you are alone, what you can reach, whether there
is light, and for how long** — plus one the seating chart of any banquet makes
obvious: **position is a social statement.** Not one of those is metric. None
asks how many squares away a thing is.

So the fine layer is not a weaker grid. It is the substrate for audience,
privacy, access, and social signification — capabilities Hornvale does not have
today at any granularity finer than the room, which is too coarse to distinguish
a whispered confidence from an announcement. This makes the program a **sim
campaign with a rendering consequence**, not a rendering campaign with a sim
cost, which is the project's own stated order.

### 1a.2 §10 item 4 — is a tile a longer `RoomAddr` path? **No.**

The metaplan's stated cost (cross-parent neighbour lookup) is real but
survivable. The disqualifying cost is different: `RoomAddr { face: u8, path:
Vec<u8> }` means a tile-as-longer-path is **the same Rust type as a room**, and
`RoomId` is a frozen save-format contract (`kernel/src/room.rs:33`). A tile
position would therefore be expressible wherever a room position is — including
`AGENT_AT` — and §3.1's central claim, "byte-identical **by construction**,"
would degrade to byte-identical by policy. Risk 4 (persistence-tier drift) would
stop being structurally impossible and become something a tag has to police.

A distinct fine-layer type makes the two-tier law compiler-enforced. The depth
budget is genuinely available (~29 digits); the idea is affordable, elegant, and
still wrong, because it spends the one type distinction the position law rests
on. `CLIENT-tiles-as-deeper-rooms` is **rejected**, with this as the reason.

### 1a.3 §10 item 1 — prose primacy: **resolved by construction**

Adopt `CLIENT-relational-fine-layer` as the fine layer, with `CLIENT-alive-map`
as the map pane's epistemic frame. Prose and picture then render **one relational
structure two ways**, so they cannot compete as rival accounts; and the map, being
the agent's own remembered artifact, carries no truth claim prose must defend
against. `CLIENT-discovery-pane` is *additive*, not a resolution, and stays a
Campaign 4 candidate. `CLIENT-as-instrument` is a separate question about
decision 0059's ordering and remains open.

### 1a.4 The pipeline

1. The room's shape is derived from its seed.
2. Its **immovable objects** are placed by a constraint solve, with all
   randomness from that same seed and named parameters.
3. **Creatures** are placed in a relational space that maps to and from the grid.
4. Creatures reason in **relations** (`the couch`, `the north doorway`),
   **anchors** (nameable places), and **goals** (`as close as I can get to that
   fish`) — never in coordinates. Goals are an optimization over the space, which
   is `kernel/src/astar.rs`'s existing job: a new `SearchSpace`, not a new
   planner (§4.5 already says this).

The mapping is deliberately **asymmetric**. Relational → grid is a *solve* (many
squares satisfy "near the couch"); grid → relational is a *classification* (given
a square, read off which relations hold). The round trip is therefore not the
identity: a creature placed "near the couch" may also turn out to be "behind the
pillar." Those **incidental relations** are the emergent payoff — accidental
cover, blocked sightlines, awkward seating — and they are why the next item is a
decision rather than an optimization.

### 1a.5 Derived geometry is causal (owner decision)

**Incidental relations may gate outcomes.** A creature is genuinely concealed
because the solver put a pillar between it and the guard, and that is the point.

The consequence is accepted deliberately: **the placement algorithm becomes a
determinism contract** and changing it needs an epoch. The mitigation is that
committed facts are never retro-changed — when geometry matters, the *fact* is
committed (a conversation happened; these three could hear it), so history
survives an epoch and only future outcomes differ. Worlds are reproducible
**within** an epoch, not across one.

The owner expects to burn through many epochs. Two consequences follow, and both
are cheap now and expensive later:

**(a) Split the epoch by blast radius.** Declare two narrow, versioned labels
from the first commit rather than one:

```
  label                  governs                   changes    blast radius
  --------------------   -----------------------   --------   -------------------
  room/furnishing/vN     WHAT objects a room has   rarely     large (contents move)
  room/layout/vN         WHERE the solver puts     often      small (future only;
                         them                                  history is committed)
```

There are only 20 seed-derivation labels workspace-wide today and most carry no
version suffix; `room/child` and `room/face` already exist and are structural,
so they must never move. Epoch *granularity* is decided when a label is declared,
not when it is bumped — put the churn in the small-radius layer on purpose.

The player-facing consequence should be stated rather than discovered: across a
layout epoch, **history survives but remembered places rearrange.** Events are
durable; rooms are not.

**(b) Pin invariants, not values.** Every epoch moves the values preregistered
studies pin, and decision 0016's guard exists to stop a study being edited to
match its new result — so frequent epochs mean frequent owner calls unless the
pins are epoch-durable. The precedent is in the tree: the health null-control
abandoned `prevalence < 0.02` for the *invariant* the metric means (chronicity
stays zero; every distress run recovers), explicitly because loosening the number
would have been the seed-shopping the decision forbids. Anything the layout
epochs touch should be pinned as ordering, sign, family membership, or
"stays zero" — never as a value.

### 1a.6 The autonomy ladder — supersedes §6.3

"Authored templates *or* a constraint solver" was a false fork. They are rungs,
each a superset of the last, and **the relational layer is identical at every
rung** because it reads objects and relations, never how they were placed:

```
  rung  what places things              legibility   variety   culture    when
  ----  -----------------------------   ----------   -------   --------   -----
   1    authored template, seeded pick  guaranteed   low       explicit   now
   2    template + seeded variation     high         medium    explicit   now
   3    constraint solve                tuned        high      implicit   later
   4    solve + historical wear         tuned        high      emergent   later
   5    creatures arrange their own     unknown      total     emergent   someday
```

**Start at rung 1–2.** It is the only band whose legibility is *guaranteed*
rather than tuned, which is exactly what risk 2 is anxious about, and it is
constitutionally native: decision 0009 ("models author, dice roll") permits
offline authoring whose output is committed and drift-checked.

Template reuse is **more** accurate, not less. Real vernacular architecture is
templated; a people's houses look alike, and that is what makes them a people's
houses. A settlement whose rooms share a layout vocabulary is culture made
visible — a signal a per-room solver would destroy by making everything unique.

§6.3's "the intra-room grid, producer-side, derived from pins and noise; the
largest piece" is superseded by a materially smaller campaign: the relational
layer plus rung-1/2 furnishing. Rungs 4–5 are not decoration — Alexander's
argument that places made all at once are dead puts the quality being chased at
the *top* of this ladder — but they are later.

### 1a.7 Risk 2 reframed, not removed

The relational layer does not remove the aesthetic problem; it **moves** it, from
"make derived terrain look right" to "make solved arrangements stable and
readable." Three corrections to how §9's risk 2 should be discharged:

- **Sightlines is the named criterion.** Theatre has solved derived blocking for
  four centuries — a script states relations, a production solves them into
  positions, and the blocking is binding for that run — and the criterion it
  converged on is whether the audience can see who matters. "Every principal
  distinguishable from the viewpoint" is testable, and risk 2 asked for criteria
  as named constants. Extends `CLIENT-choreographer`, which already reaches for
  theatre but stops before taking its criterion.
- **Solve from scratch; never incrementally.** Carrying state between solves for
  stability makes the layout path-dependent — the same constraints can settle
  into different minima depending on how you got there, which is a determinism
  hazard wearing a stability fix's clothes. Stability must come from constraints
  being tight.
- **The seed fills exactly the residual degrees of freedom.** Parametric CAD's
  hard-won lesson is that under-constrained sketches are the ones that wobble,
  and the tool's job is to report the DOF. Adopting that makes "how much variety
  does this room have" a number to assert on rather than a vibe.

### 1a.8 What earns an object its place, and what furnishing reads

**Affordance, not decoration.** An object belongs in a room because it affords an
activity the interface exists to serve — a workbench gives "research an artifact"
a *place*, a hearth makes "who is gathered here" meaningful, a desk is where a
letter is read. This keeps rooms from filling with scenery nobody can act on and
supplies the relational vocabulary for free.

**Furnishing reads world-time, not just the room seed.** Nothing in the metaplan
lets a room change across deep time: a tavern in a thriving settlement and the
same tavern three centuries into its ruin would furnish identically. The input is
`(room seed, settlement state at t, history)` — which is also the first obvious
consumer for The Vestige's residue and forgotten-fraction metrics.

### 1a.9 The discipline that keeps this a language and not a catalogue

The pattern-language reading is deliberate (Alexander, *A Pattern Language* /
*The Timeless Way of Building*): patterns are stated as relations, indexed by
what people *do*, authored and small while the buildings they generate are many
and large. That is "models author, dice roll" and `World { seed, ledger }` in
another domain — a compression scheme, which is always a theory of what matters.

The cautionary half is the one to write down. When software borrowed Alexander,
design patterns became a *catalogue of solutions* rather than a generative
language, and that is when the idea died. **A template vocabulary is a pattern
language only if the composition rules carry the weight** — which patterns
complete which, what must be adjacent to what, what a culture's grammar permits.
If a later spec's substance turns out to be template *count*, it has gone wrong;
if it is adjacency and composition, it is right.

**One standing caution, unresolved.** Alexander's "A City is Not a Tree" argues
real places are semi-lattices — overlapping, non-hierarchical — while `RoomAddr`
is literally a quadtree. The project already pays that tax as an engineering
annoyance (cross-parent neighbour lookup, punted on at face boundaries) without
naming it as a modelling error. The relational layer *is* a semi-lattice, which
is an argument for treating it as the truth and the quadtree as an index over it.
Not a decision this amendment takes; a tension later work should expect.

### 1a.10 What this leaves open

- Which rung within 1–2, and the object/affordance catalogue's first contents.
- `CLIENT-as-instrument` (§10 item 3's neighbour): whether the client is
  promoted to an instrument, against decision 0059's "the Book is primary."
- §10 item 5: the program name `the-rose-window` is still provisional.
- New decisions owed to `docs/decisions/`, beyond §11's three: **derived geometry
  is causal**, and **epoch granularity is declared, not discovered** (the
  two-label split). Numbers unminted here to avoid colliding with parallel
  sessions.

---

## 1b. Amendment 2 (2026-07-27) — the macro/micro band split

**Status of this amendment:** brainstormed 2026-07-27 under autopilot (nine
ideonomy passes, three overturns — two of them corrections to decisions this
same session had adopted). It corrects a **scale collision in the word "room"**
that invalidated §4.4's scope table, re-carves §6's remaining campaigns, and
raises one owner decision that supersedes a ratified decision (§1b.7). Ledger:
`.superpowers/sdd/decision-ledger.md` (15 entries).

### 1b.1 The collision

`GLOBE_LEVEL = 6` (`domains/terrain/src/lib.rs:53-62`, documented as 40,962
cells at ~110 km resolution) and `walk_depth = globe_level + 6`
(`windows/vessel/src/agent.rs:35`). A `RoomAddr` refinement halves the triangle
edge, so **the place a possessed body commits to is ~1.7 km across.**

That is not an accident to be fixed: it is the macro worldgen resolution, chosen
so 1000-world censuses finish, and the owner's stated design target for
wilderness travel is *something worth stopping for every ~1.7 km*. But houses,
castles, dungeons and caves are a different regime — human scale, sparse,
generated on the fly, with only the player's changes persisted — and The Hearth's
shipped anchor vocabulary (`Hearth`, `Bed`, `Alcove`, `Vessel`, `Screen`) is
**chamber-band vocabulary currently deriving for a 1.7 km locale.** Drawing that
interior as a glyph lattice would not merely invent distances; it would invent a
scale, putting "the bed beside the fire" 200 m apart.

### 1b.2 The vocabulary, because the collision was in the words

- **locale** — a macro place, ~1.7 km, the walk band. Already the codebase's
  word (`windows/locale`, `Locale` embedded in `vessel/session/v1`).
- **chamber** — a micro place, human scale, sparse, derived on demand.
- **place** — either, when the band does not matter.
- **"room" unqualified is retired** from new specs and doc comments.

`RoomAddr` / `RoomId` are **not** renamed: under §1b.3's third law an address is
correct at every band, and `RoomId`'s packing is a frozen contract.

### 1b.3 The band notation

```
  band        extent      exists    generated    persists      adjudicates    render
  ---------   ---------   -------   ----------   -----------   ------------   -------------
  GLOBE       ~110 km     always    world seed   seed+ledger   statistical    atlas, orrery
  WALK        ~1.7 km     always    world seed   seed          TOPOLOGICAL    situated chart
  STRUCTURE   ~30-300 m   sparse    on demand    seed + marks  metric         glyph lattice
  CHAMBER     ~3-10 m     sparse    on demand    seed + marks  METRIC         glyph lattice
  DETAIL      < 1 m       never     never        never         n/a            client eyecandy
```

Five laws, each load-bearing:

1. **Exhaustive above the walk band, sparse below it.** Existence below the walk
   band is a *predicate*, not a given — otherwise one locale implies ~262,000
   phantom chamber-sized children.
2. **Adjudication changes kind at the band break** — topological above, metric
   below. This is what dissolves risk 9's "a metric picture claims distances the
   sim lacks": that was never a property of lattices, only of drawing one at the
   wrong band.
3. **Connectivity changes kind too** — mesh adjacency above, a door graph below.
   So a deep address is **identity, not shape**, which discharges §1a.9's
   standing "a city is not a tree" caution instead of inheriting it.
4. **The anchor vocabulary is declared per band.** `Ford`/`Grove` at the walk
   band; `Hearth`/`Bed` at the chamber band. Borrowed from cartography, biology
   and text alike: the address space may be uniform while the vocabulary is
   level-specific — you do not zoom a topographic map into a floor plan.
5. **Persistence differs by band** — derived above, derived-plus-committed-marks
   below, which is The First Mark's shipped additive-latent pattern.

### 1b.4 The brief is the contract

Macro answers *who holds this land*; micro answers *what is standing here*. The
seam between them is one small derived **brief**, so micro generation is a pure
function of `(brief, address, seed, day)` and is testable without a world — the
same shape as campaign 1's snapshot, one ring inward.

Every axis it needs is already committed. `OccupationRecord`
(`domains/history/src/record.rs`) carries:

```
  committed field        licenses in micro generation
  --------------------   ------------------------------------------------------
  people, tongue, deity  pattern vocabulary, materials, inscriptions, shrines
  function               WHICH structures exist (Agrarian/Mine/Trade/Cult/Fort)
  tech horizon           construction: thatch vs dressed stone vs vaulting
  peak_population        how many structures, how dense
  notability             whether monumental structures exist at all
  founded / ended        age -> wear, overgrowth, collapse (The Vestige's input)
  cause of end           THE RUIN SIGNATURE: Burned -> soot and fallen roofs;
                           Plague -> goods intact beside graves; Fled -> open
                           doors, valuables gone; Migrated -> stripped fixtures;
                           Famine -> empty granaries
  ended_by = By(entity)  the antagonist's marks, spoil, reuse
  founded_from, lineage  architectural continuity with the parent community
  stratigraphy           layered foundations, reused stone, an older street grid
```

Plus, from outside history: per-kind carrying capacity (`niche_per_species_k`),
the route graph, and terrain. `cause` is the richest row and no prior spec
anticipated it — it was baked for demography and turns out to make a ruin an
argument about history rather than decoration.

**The brief must be a coordinate in a small orthogonal space** (function ×
people × tech × condition × notability), never a label from a catalogue of place
types. §1a.9 binds: if a later spec's substance is template *count* rather than
adjacency and composition, it has gone wrong.

The sparseness predicate of law 1 is therefore the stratigraphy: a structure
stands at a locale iff the cell's occupation history says something stood there.
That is §7's `DERIVED ENCOUNTER` ("who is there is who lives there") applied to
buildings instead of creatures — no spawn tables.

### 1b.5 Rung 2 is §2's EMPTY settlement rung, and it is the same machinery

`Pattern { name, kind, attach, requires, needs_cold, built }`
(`windows/vessel/src/interior/pattern.rs`) is band-agnostic structure, so
"the-fire requires an alcove" and "the-docks require navigable water" /
"the-courts require `Notability::Seat`" / "the-walls require tech ≥ Bronze and
(`Fort` | `Seat`)" are the same shape. **Two bands, one composer, one validator,
one connectivity rule.** A `Trade` + `Seat` + `Classical` + high-population
coastal site draws docks, warehouse row, market, curia, temple precinct, uptown
villas, tenements, walls and gates, an extramural suburb; a `Backwater`
`Agrarian` `Neolithic` site draws three patterns from the same inventory and is
a hamlet.

**Extent may cross a cell boundary; identity may not.** A locale's brief reads
its containing cell *and* its neighbours' occupations (The Excursion's
neighbour-ring pattern) rather than assuming a city never straddles a boundary —
which would let an invisible line truncate a city, a defect with no symptom
until someone walks to the seam.

### 1b.6 Band transitions, and what descent commits

**The band changes only at a threshold, and thresholds are always visible.**
Descent is a deliberate verb (`enter`, through a threshold anchor); ascent
likewise; **lateral movement never changes band.** One rule removes silent scale
changes, thrashing (§4.3's named demand-paging failure mode, which automatic
transitions would have reintroduced one level up — a player pacing a village
boundary flipping bands every step), and the mismatched lateral step together.
§7 already refused stairs by name ("APERTURES, NOT STAIRS"), and
`AnchorKind::Threshold` already ships as "an anchor that is ALSO a room-graph
edge (the two-level seam)".

> **Clarified by The Blocking (2026-07-28). The law above is UNCHANGED.** The
> Lintel read "lateral movement never changes band" as also meaning a compass
> bearing is meaningless inside a chamber, and refused `go north` indoors on that
> basis. The Blocking gave a chamber an interior lattice, so `go north` indoors is
> now one cell north — and the law still holds, because a cell step stays inside
> the chamber band; it changes no band at all. What the campaign reversed is the
> *inference* The Lintel drew from this law, not the law. (`back` stays refused
> indoors: it retraces a walk-band trail, which is a walk-band operation whatever
> the interior looks like.)

"The world coarsens behind you" survives as a *consequence*, not a rule: fine
bands exist only inside structures, so there is no fine band to be in once you
step out the gate. A move's cost still scales with its band's extent (a 1.7 km
stride is hours, a 3 m step is seconds), which composes with The Action Clock's
shipped per-agent tempo.

**Descent commits nothing.** `Session::go` mutates `self.agent.position` and
pushes to `self.trail` without committing a fact (`session.rs:627`), and the
session's ledger is "written to only by `wait`'s tick (NPC `agent-at` facts).
Never written back." (`session.rs:121`). The player's position has never been a
committed datum, so a band change needs no schema change, no epoch, and no new
save-format surface — and re-entering re-derives identically, which hands us
§3.1's byte-identical-by-construction property for free. `AGENT_AT` remains what
it always was: an NPC datum.

One consequence: the `enter`/`exit` refusal becomes **directional** — refused at
the coarse end (possessing a settlement or culture stays a deferred arc),
permitted at the fine end. Today one constant sentence answers both
(`session.rs:578-582`) and it is byte-pinned in the galleries.

### 1b.7 The lattice answers fine spatial queries — an owner call that supersedes 0075

Traditional line-of-sight and pathfinding behaviour is expected "most of the
time" (owner, 2026-07-27), and that forces the strong form. Lifting the question
gives **granularity matching**: a query is answered at the granularity at which
it is posed, and answering a fine question with a coarse structure gives the
wrong *kind* of answer, not a coarser one. Re-instantiated in databases: **the
lattice is the base table and the anchor graph is an index over it** — the same
truth at two resolutions, where the index narrows the search and the base table
answers the predicate, and where index/table consistency is maintained by
rebuilding from the table, never by patching. That consistency requirement *is*
§1b.8's soundness rule.

One word was doing two jobs, so split it: the **specified graph** is what the
patterns require; the **realized graph** is what the solved place holds, read off
the lattice, incidental relations included. Static relations (what screens what,
what lies beside what) come from the realized graph, so outcomes still read
topology. But *dynamic per-turn queries* — can I see you from where I stand
now — are not static relations, and routing them through a graph would be
bookkeeping theatre. **They run on the lattice.**

Therefore: **symmetric shadowcasting for line of sight, A\* over cells for
pathfinding** (a new `SearchSpace` on the existing kernel planner, as §4.5
already says). Three things make the trade better than it sounds — integer grid
algorithms are *more* determinism-friendly than what they replace (no libm, no
ULP exposure, where the shipped warmth field already uses floats and
`kernel/src/astar.rs` costs are `u64` precisely to avoid float
non-determinism); "less tactically oriented" is a real simplification, since
plausible LOS is wanted and not cover percentages; and §1a.5's mitigation
already covers the epoch, since committed facts are never retro-changed, so a
layout epoch rearranges remembered places while events stay durable.

**What this supersedes.** `windows/vessel/src/interior/mod.rs`'s header promises
that outcomes read topology never metrics, "so a future rendering solve can be
retuned forever without an epoch." The **rule** survives. The **consequence**
does not: the solve is causal, so retuning it needs an epoch, in the
small-blast-radius `room/layout/vN` label §1a.5(a) declared for exactly this
churn. Decision 0075 is **superseded, not reinterpreted**, and owes a new
decision record.

**The cheaper alternative, stated so it can be chosen:** do not promote —
incidental relations stay pure decoration, only pattern-authored relations gate
outcomes, visual tuning stays free forever, and the doc comment stands. It makes
this a materially smaller program. It was rejected because 0072 and §1a.5
explicitly wanted the pillar to conceal ("a creature is genuinely concealed
because the solver put a pillar between it and the guard, and that is the
point") — but that is the owner's call, and it leads the G3 package.

### 1b.8 The checker — the campaign's central invariant

```
  1  soundness    every relation the specified graph asserts is realized
                  in the solved lattice
  2  wall law     every drawn wall IS a non-adjacency; no decorative walls
  3  closure      a fine place's boundary maps entirely onto thresholds
  4  doorways     a shared threshold derives from the EDGE, so two adjacent
                  places cannot disagree about it
  5  occupancy    at most one creature per cell
  6  determinism  same (brief, address, seed, day) -> identical lattice,
                  solved from scratch, no carried state
  7  DOF          residual degrees of freedom reported as a number
```

Rule 3 came from a structurally empty cell in the transition chart: without
closure a player walks off the edge of a chamber into undefined space — the same
bug class as walking through a wall, and invisible until someone tries it.
Rule 7 turns "does this place have enough variety" into an assertion rather than
a feeling, which is §9 risk 2's demand for criteria as named constants.

### 1b.9 Evidence — measured, and honestly bounded

Native, `--release`, medians on the canonical box (the spike is preserved in the
session scratchpad, and the plan should promote it into a cost gate on
`cli/tests/graph_cost.rs`'s pattern):

```
  interior_of   (specified graph, per visit)    0.666 us
  route_within  (anchor A*, per query)          1.000 us

  grid            shadowcast (LOS)   cell A* (worst case)
  -------------   ----------------   --------------------
   16x16   256            2.084 us              32.458 us
   24x24   576            3.458 us              60.833 us
   48x48  2304            7.041 us             325.375 us
   96x96  9216           24.208 us            1975.167 us
```

**Integer LOS is not a budget item.** A 10 m chamber at half-metre cells is
~24², so 3.5 µs native and ~13 µs extrapolated through wasm, against §5's
measured 4.75 ms no-op turn floor.

**Pathfinding is the term to watch**: 61 µs at 24² but ~2 ms at 96²,
worst-case corner-to-corner across a wall. A great hall with ten creatures
re-pathing every turn is a real fraction of a turn.

Three limits stated rather than buried. **The real constraint solve remains
unmeasured** — the spike's "solve" column is a placement scan, not a solver, and
establishes only that bookkeeping is not the cost; §5 caveat 2 still stands.
**Nothing here is measured through the wasm ABI**; §5's own native→wasm ratios
(`look` 0.12→0.46 ms, movement 0.32→1.15 ms) give ~3.6–3.8×, which is an
extrapolation. And **a cost claim without a build profile is not a
measurement**: the same spike ran ~10× slower in debug (`interior_of` 6.166 µs,
24² shadowcast 26.8 µs).

### 1b.10 The re-carve — supersedes §6.3's replacement and §6.4's ordering

```
  campaign        ships                            inherits    epoch?  player sees
  -------------   ------------------------------   ---------   ------  --------------
  The Lintel      band law, the brief, chamber      Hearth,     no      enter a house,
                  derivation, the descent seam      Threshold           walk its rooms
  The Blocking    the lattice solve, integer LOS,   Lintel      YES     the glyph view
                  cell A*, the checker                                   is honest
  The Precincts   rung 2: districts as patterns     Lintel      no      a city has parts
  The Panes       the browser draws both bands      all         no      the roguelike
                                                                          client
```

Vitality (§6.5) is unchanged and still ordered before any combat work. The
Panes remains §6.4. `The Lintel` is deliberately first because it needs no
epoch and fixes the contract the other three inherit — the same reasoning that
put The Snapshot first.

### 1b.11 Settlement density: registered, not fixed here

`Bake::vacant_habitable` (`windows/worldgen/src/history_bake.rs:480-483`)
requires `!self.node_index.contains_key(&cell)`, so **at most one alive
community occupies a geosphere cell** and no two living settlements are closer
than ~110 km. What that forbids is *rival polities in sight of one another*.
What it does not forbid is several *sites* per cell, which are derived micro
concerns — a dwarf hold (the alive occupation), a goblin camp (from the cell's
goblin carrying capacity, independent of settlements), and a human trading post
(from a route crossing) coexist today with no macro change.

The honest tier model is **commit the contingent, derive the regular**: city
spacing is contingent (a ford, a pass, a battle), while market-town (~10–20 km),
village (~2–5 km) and hamlet (~0.5–2 km) spacing follow from agronomy and
walking speed — physical constants, hence derivable. The arithmetic forbids the
alternative anyway: a ~10,000 km² cell at medieval English density holds ~1,000
villages.

Relaxing the founding rule is a small diff and a large revalidation (world
identity moves, so an epoch, a census re-baseline, and the conversion of every
history-adjacent study's pins from values to invariants per §1a.5(b); plus
re-checking The Tumult's, The Margin's and The Accession's invariants). **The
owner wants it soon-ish**; it belongs to its own history-domain campaign whose
declared purpose is the city-state case. Registered as `SOC-dense-settlement`
with `SOC-settlement-tiers` and `SOC-casus-belli`.

### 1b.12 What this leaves open

- The real solver's cost and its algorithm family (BSP / cellular / WFC are all
  admissible per §7; none is chosen).
- Whether the situated chart's walk-band render adopts the topology-true
  "blocking chart" form (recommended) now that the glyph lattice is
  chamber-band-only.
- `CLIENT-as-instrument` (§1a.10) is still open.
- The band notation probably owes its own `docs/decisions/` record; the number
  is unminted here to avoid colliding with parallel sessions.

---

## 2. The spine: the first situated *spatial* surface

> **Narrowed by Amendment 1 (§1a.1):** the program is a sim campaign with a
> rendering consequence, not the reverse. The conjunction below still holds; it
> is no longer the whole identity.

Walking the sibling set — atlas, Orrery, Casement, almanac, Book, REPL, Lab,
and the unbuilt TUI viewer — every existing surface is one of two things: a
view **of** the world from outside it, or a view **from** inside a body,
rendered as text. None is both spatial and situated.

```
                EXOCENTRIC  <------------------------>  SITUATED

  universal     ORRERY: system rung                     (ORRERY-situated-jump)

  global        ATLAS ..... ORRERY: globe rung           ALMANAC (sky from the
                                                          flagship); STAR CHART

  regional      ORRERY: map rung                         >>> THIS PROGRAM <<<
                (Overworld / Diorama styles,
                 Excursion's 9-tile ring)

  settlement    ........... EMPTY ...........            ....... EMPTY .......

  room          ...........                              CASEMENT / vessel
                                                          (prose, one room)

  tile          ........... EMPTY ...........            ....... EMPTY .......
```

So the program's identity is a two-property conjunction nothing else holds:
**situated *and* spatial**. That conjunction is *why* the tile layer is
unavoidable — a situated spatial surface needs a space to be situated in, at
a finer grain than room-as-graph-node — and it is why the empty rungs
(settlement, tile) are exactly the ones this program fills.

`ORRERY-surface-descent` is already the registry row for descending through
those rungs. This program is that descent, entered from below.

## 3. Core commitments

Five laws settle the shape. The first is Nathan's and governs the rest.

### 3.1 The two-tier position law

**An entity's persisted position is its room. A tile coordinate exists only
inside the presence bubble and is never serialized.**

`Position(Room)` is committed truth and already ships: `AGENT_AT` in
`windows/vessel/src/liveness.rs`, latest-wins, with The Quickening's
derived-else-committed read built over it. `Position { x, y }` is what a
roguelike needs for rendering, collision, and pathfinding — and it matters
only while the observer occupies that same room. The rest of the time, only
the room does.

This is **quantize-at-emit-only applied to space**: full precision in the
compute path, coarse at the serialization boundary. It is also `UNI-32`
applied to space rather than time — unobserved, position is *sampled* at
room granularity; possessed, the local region is promoted to *integrated*.
**The tile grid is the shape of the presence bubble.**

Three consequences, each load-bearing:

1. **No new determinism contract.** Nothing stored points into the grid, so
   the grid may regenerate differently forever without corrupting a world.
   An earlier brainstorm pass had this backwards and put a save-format
   contract at the top of the review package; the law removes it.
2. **UNI-32's honesty invariant becomes structural, not disciplinary.** If
   `(x,y)` never persists, entering a room, walking it, and leaving *cannot*
   alter the world — byte-identical by construction. The First Mark had to
   work for that property with its additive-latent pattern; here it falls
   out of the representation.
3. **Cheesing is not defended against; it does not exist.** Re-entering a
   room cannot gain anything, because the world is byte-identical unless a
   mark was committed. The only thing spent is turns.

### 3.2 The persistence notation is the law, written out

Every piece of entity state declares its tier. The table is normative, not
illustrative — the alternative to stating it is deciding it per-datum by
accident, which is the default outcome of silence.

```
  TIER     meaning                                  persists?
  ------   --------------------------------------   ---------
  COMMIT   a fact in the ledger                     yes
  FOLD     derived by folding the ledger            never
  SALT     derived from stable identity             never (but stable forever)
  FRAME    transient, this turn only                never
  STRUCK   removed by an explicit committed event   yes (the removal)

  datum                  tier     keyed by                 lifetime
  --------------------   ------   ----------------------   ---------
  room position          COMMIT   entity (place: None)     forever
  tile position, mob     FRAME    (room seed, entity, t)   turn
  tile position, object  SALT     (object, room)           stable
  held-by                COMMIT   object                   forever
  disposition/grievance  COMMIT   entity (place: None)     forever
  hostility              COMMIT   entity, functional       forever
  drives                 FOLD     entity                   --
  affect                 FOLD     entity                   --
  belief                 FOLD     entity                   --
  knowledge              FOLD     entity                   --
  inventory              FOLD     entity                   --
  name / identity        COMMIT   entity                   forever
  consumed / destroyed   STRUCK   object                   forever
  vitality / wounds      see §3.3
  action cost            see §6.2
```

**Adopt stage management's vocabulary**, which has run this architecture for
four centuries and names six of these concepts better than we did: a script
(canon) / a **prompt book** whose blocking is explicitly non-canonical
annotation, re-derived every production / a performance — mapping onto
ledger / derived views / this session's frame. An actor's **mark** is the
stable per-entity derived position. The props **preset** is exactly
`SALT (object, room)`. **Strike** is the STRUCK tier. **Calling the show**
off a cue sheet is the action clock. An **understudy** is `UNI-18`'s
transmigrating soul.

**Coherence, not identity, is the hard case.** Leave a room mid-chase, step
back in, and a from-scratch derivation would stand the creature idle at its
post. Resolved by making the fine layer a function of **the current tick**
rather than of a stored snapshot, so that:

- immediate return → few ticks elapsed → the creature has barely moved;
- a longer excursion → more ticks → it has drifted back to its post;
- return after a long wait → likewise.

All three behaviours fall out, so **no cache, no eviction policy, and no
hysteresis is required.** (The Excursion's hot-ring/warm-halo policy was
considered for this and is not needed.) Tactical engagement is likewise
derivable, because disposition is `COMMIT`-tier, entity-keyed and *placeless*
(`subject: entity, place: None` — verified in `session.rs`): hostile + 1 tick
is still closing; hostile + 40 ticks is back at its post and still hostile.
Position resets; grievance does not. **Nothing tactical is stored.**

The rule, stated generally: **the fine layer is a pure function of the coarse
layer plus the room's seed, and never feeds back except through an explicit
commit.** That is The Quickening's rule ("only the discrete divergence
commits; the smooth routine stays derived") at the spatial seam instead of
the temporal one, and it recurs at every scale — sub-tile (wound location),
settlement (home range), migration — which is `UNI-37` seen from the storage
side. It is a law, not a room-scale trick.

**Object placement carries one accepted cost.** Salting by `(object, room)`
per decision 0051 (salt by stable identity, never by mint order) gives an
object the same spot on every visit without a byte stored, and moving rooms
re-derives a stable spot in the new room with no extra mechanism. Held is a
*relation*, not a coordinate, so a carried object's fine position derives
from its holder's. The cost: **the player cannot choose which tile to drop
something on** — objects settle at their derived spot. Accepted for v1. If
precise placement ever matters it becomes a narrow explicit exception (an
`emplaced` fact carrying a coordinate), never a general capability, so the
exception stays visible.

### 3.3 Wounds commit; health folds

Vitality is the notation's second unfillable slot and possibly the larger
one: a roguelike is substantially *about* damage, and Hornvale has no
vitality model at all. It is also the datum every ECS tutorial stores and
mutates in place, so it is where the architecture is likeliest to be
violated by reflex.

The notation determines the answer, because every other derived row is a
fold. **Vitality is a fold** over the body's constitution, the injury facts
in the ledger, and elapsed healing time. **No HP counter exists anywhere.**
That is the fifth instance of one pattern (drive = fold, belief = fold,
affect = fold, inventory = fold, health = fold), and it inherits §3.1's best
property for free: a wound is entity-keyed and placeless, so it survives room
transitions exactly as grievance does.

### 3.4 One snapshot per commit; panes are pure projections

The wasm ABI is prose-only today — UTF-8 into a 4096-byte buffer, text out by
ptr/len — while `Session` already exposes `focalized()`, `ways()`, `agent()`,
`knowledge()`, `npc_labels()`, `npc_grievance()` in Rust. So every map, HUD,
and log pane is blocked on a **producer-side emit**, not on JS.

The emit is **one structured snapshot per committed turn**, and every pane is
a pure function of it. No pane queries the port itself. The borrow is
database MVCC's **snapshot isolation**: independent per-pane queries ship a
bug class — a map from turn T beside stats from turn T+1 — and one snapshot
makes pane incoherence impossible by construction rather than by discipline.

Consequences: a new pane costs *zero* API surface, which is what makes a
large API expansion survivable (the expansion goes into one versioned schema
beside the six shipped `scene/*` kinds); and panes unit-test with no wasm at
all, which is `clients/atlas`'s existing shape. This is `UNI-20` — one
ledger, many derived views — re-instantiated one ring outward, which is the
strongest available evidence the decomposition is right.

The snapshot **must carry provenance per datum** (known / sensed / felt), not
merely values. §4.3 forces this and no other consideration produces it.

### 3.5 Authority: the sim adjudicates play on the grid

The tile grid is producer-side. The original argument for this — that
entities carry positions and so must stand on an authoritative grid — is
dissolved by §3.1, since they stand authoritatively on *rooms*. The
conclusion survives on firmer ground: **the sim adjudicates play.** Whether
you can close with a creature this turn is an outcome, and outcomes are the
sim's.

That distinction is load-bearing downstream: a purely *decorative*
client-side grid would be admissible; an *adjudicating* one would not. The
client's sanctioned autonomy band is therefore what `MAP-23` already
licenses — non-deterministic decoration that never re-enters the ledger:
residue, weather particles, torch flicker, tweening between committed turns.

## 4. The architecture

### 4.1 Four clocks

```
  clock     rate                owner   measured cost
  -------   -----------------   -----   ----------------------------------
  frame     continuous          JS      16.7 ms budget; sim contributes 0
  turn      episodic            wasm    4.75 ms (no-op floor)
  bubble    fine ticks          wasm    68,386 ms per simulated day
  world     sampled/lazy        wasm    516 ms per simulated day (bulk)
```

The sim is **not in the frame loop**: it runs in a Web Worker and is
turn-driven, so it cannot stall a frame even at 475 ms — it can only delay a
*response*, which is a much looser budget. Frame rate is therefore a
non-question, and the Orrery already sustains 60 FPS with a three.js voxel
globe and measured 15.5 ms tile builds.

The 130× penalty for resolution (68,386 vs 516 ms per simulated day) is the
arithmetic proof that coarse-constrains-fine is load-bearing here rather than
aesthetic: integrate finely *only* inside the bubble, sample coarsely
outside. The measurement reproduces `UNI-32` from the cost side, unprompted.

**Render cadence is an open choice, not a default.** Making the render
episodic — redraw only on a committed turn — is the classic terminal
roguelike and is nearly free, which frees the entire frame budget to
*animate the last turn's result* rather than to simulate. That is `MAP-23`'s
"keyframes from the sim, life from the client," and it deserves to be the
default rather than the fallback.

### 4.2 The pane set partitions by epistemic channel

Panes do **not** partition by data type (map / text / numbers). They
partition by knowledge source, because a roguelike client's job is as much
to withhold as to show:

```
  pane            shows                    truncated by
  -------------   ----------------------   ---------------------------
  prose           what the agent KNOWS     the knowledge ledger
  tile view       what the agent SENSES    FOV, per-species senses
  status          what the agent FEELS     the affect readout (shipped)
  historiography  what it REMEMBERS        the `why` window (shipped)
  foresight       what it EXPECTS          The Foresight (shipped)
  (debug)         world truth              nothing -- a cheat pane
  (esoteric)      reader-privileged        initiation state (LANG-39)
```

Two payoffs. "Add an inventory pane" stops being a layout question and
becomes "which epistemic channel is this?", which is answerable. And this is
the first place the **reader-relative Book becomes a player-relative UI** —
LANG-39's esoteric unlock already ships in the vessel, and here it is a pane
that materializes when the player's own knowledge ledger holds the
initiation.

The default privilege level is **the player's**. A debug pane is an opt-in
widening, never a baseline that play then narrows.

### 4.3 Two spatial panes at two scales

Conflating these mis-prices both:

- **The tile view** — one room, plus whatever its apertures admit sight
  through (`MAP-perceive-apertures`). Fine, tactical, the roguelike view
  proper. **Cost unmeasured**, because the generator does not exist.
- **The room-graph map** — many rooms, coarse, for orientation. This is what
  the brainstorm's measurements actually priced: 0.010 ms per room describe,
  so a 21×21 neighbourhood is ~10 ms in wasm (inside one frame, uncached)
  and a 41×41 ~50 ms (once per region, cacheable). It is also the pane that
  renders the empty **settlement** rung.

Do not quote the second's numbers for the first.

Rooms are demand-paged, and the borrowed shape is **virtual memory** —
page-fault cost, prefetching, eviction, working set, and **thrashing**, a
failure mode nothing had named (a player pacing a boundary). Two of the
answers already exist: The Excursion's two-radius hot-ring/warm-halo policy
is the eviction policy, and its eager neighbour-ring fetch is the prefetch.

### 4.4 Scope: three nested rungs

> **CORRECTED by Amendment 2 (§1b.1–1b.3).** This table conflates two scales
> under "room": the `agent-at` rung is the ~1.7 km **walk band**, and the
> human-scale **chamber** band it silently assumes does not exist yet. The
> band notation in §1b.3 replaces this table; "tile" is a chamber-band cell.

```
  rung         representation             persisted?   role
  ----------   ------------------------   ----------   ---------------------
  settlement   creature home range        yes          who is around at all
  room         agent-at                   yes          the COMMIT granularity
  tile         derived (x, y)             never        the presence bubble
```

`RoomAddr { face: u8 (0..20 icosahedral faces), path: Vec<u8> (child index
0..4 per refinement) }` already spans the top two: a "room" is a **leaf**, a
"floor" or chunk is a **path prefix at some depth**. The ambiguity is in the
word, not the model, and specs should name the depth they mean.

> **SETTLED by Amendment 1 (§1a.2): NO.** A tile is not a longer path — it
> would be the same type as a room, so the two-tier law would stop being
> compiler-enforced. The fork below is kept for its reasoning.

**Open fork, to be settled before the grid is designed** (§10): a tile may
simply be a *longer path*. `RoomId` packs into a u64 with room for ~29
digits, so one more refinement level is one more digit, and the two-tier
model would collapse into one level at two depths — commit a truncated
address, derive the rest, which is literally quantize-at-emit for space. Not
free: a quadtree leaf is not a 4-/8-connected lattice, and cross-parent
neighbour lookup is the fiddly case The Excursion punted on at face
boundaries.

### 4.5 Pathfinding and collision need no new machinery

`kernel/src/astar.rs` is deliberately generic — its own header says "one
kernel planner serves navigation, GOAP, confabulation, prophecy, each with
its own state space + cost" — and its costs are `u64` integers specifically
to avoid float non-determinism. Tile pathfinding is therefore **a new
`SearchSpace` impl**, not a new planner. Collision is a pure function of the
derived grid plus the room's occupancy set: nothing stored, nothing to
invalidate.

## 5. Evidence: what was measured, and what was not

Measured before designing, on one box (Apple silicon, `--release` native;
Node over the real Casement ABI for wasm), medians of repeated runs:

```
  operation                    native      wasm        note
  --------------------------   ---------   ---------   --------------------
  full genesis (new --seed)    0.53 s      --
  possess to first prompt      1.75 s      3.6-5.0 s   ~1.2 s is SESSION
                                                        start, not genesis
  module instantiate           --          11.4 ms
  `whoami`                     --          0.0017 ms
  `look` (full narration)      0.12 ms     0.46 ms
  movement (`go` / `back`)     0.32 ms     1.15 ms
  room describe (locale)       0.010 ms    ~0.03 ms    from --sample sweep
  tick, 6 s span               --          4.75 ms     the game-turn cost
  tick, 1 min span             --          4.77 ms
  tick, 1 h span               --          7.81 ms
  tick, 1 day span             --          515.61 ms
```

Every budget has margin except cold start. Three caveats stated honestly:

1. **4.75 ms is the *no-op* floor.** At six-second granularity the session
   reports "Time passes; the world keeps its shape" because the NPC layer's
   effects are day-grained. The floor rises by an unknown amount once agents
   act per tick. **Do not extrapolate from 4.75 ms.**
2. **The tile-layer cost is unmeasured** and cannot be estimated from the
   room-describe figure (§4.3).
3. **An unexplained anomaly worth profiling first:** the *first* `wait 1`
   from a fresh session costs 4.4 ms while steady-state sequential `wait 1`
   costs ~475 ms, flat, not growing with the ledger; and a bulk `wait 365`
   costs 47 ms per simulated day against sequential single-day waits'
   ~475 ms per day. Something in the tick path is conditional and 100×
   cheaper on the first call. Whatever it is, it is the cheapest available
   win on the day clock.

**Cold start is the only red budget**, and no client trick fixes it. Levers
in precedence order: the `BuildDepth` ladder already exists (Astronomy /
Terrain / Settlements / Full) and no client path uses it selectively; the
~1.2 s of session-start work (the seam The Retainer had already begun on);
and the diegetic option — **genesis narration as the loading screen**, so
four seconds becomes content instead of latency, which is free and the most
Hornvale-ish of the three.

## 6. The campaign carve

Sequenced by what is inherited, not by what is exciting. Each is its own
spec → plan → execution cycle.

### 6.1 Campaign 1 — The Snapshot

The per-turn structured session emit: one versioned schema carrying the
focalized room, ways, agents, affect, knowledge, and **provenance per
datum**; a widened wasm ABI beside the prose one (prose is not removed); and
the client refactored so every existing pane is a pure projection of it.
Small, and it fixes the contract every later campaign inherits. **Nothing
new becomes playable — what is playable becomes legible.**

### 6.2 Campaign 2 — The Action Clock

> **SHIPPED 2026-07-26**, and the gap statement below is **corrected by the
> build** — see the [spec](2026-07-26-the-action-clock-design.md) §1 and the
> [chronicle](../../../book/src/chronicle/the-action-clock.md). "Nothing
> schedules time at action granularity" was too strong: `MoveTo` already cost
> 0.1 days, and `Rest`/`Hold` already *jumped* to the next waking and the next
> act-crossing — an event-driven scheduler in embryo. The true gap was three
> things, not one absence: three of five actions were free, the single cost was
> uniform across every creature, and the jump machinery belonged to no clock.
> All three rungs shipped (total cost, allometric per-agent tempo, interleaving).

Per-agent action cost: the one genuine gap. Crossing Wolverson's subsystem
inventory against Hornvale's mechanisms (§7) found eleven cells already
answered, refused, or better answered here, and exactly one hole — **nothing
schedules time at action granularity.** Every clock is continuous (frame),
uniform (the `wait` span, which parses f64 *days*), or lazy (the unobserved
world); "your swing takes 100 ticks, the goblin's takes 80" has no home. No
client work at all; measurable the day it lands; the subsystem the tutorial
teaches best.

### 6.3 Campaign 3 — The Ground (the tile layer)

> **SUPERSEDED by Amendment 1 (§1a.6).** Replaced by a materially smaller
> campaign: the relational fine layer plus rung-1/2 authored furnishing. The
> text below describes the abandoned grid-first shape.

The intra-room grid, producer-side, derived from pins and noise; the object
graph; creature binding with a settlement-scoped home range. The largest
piece, and the one carrying §10's open fork. Costs measured as it lands, and
a **controller-driven visual tuning pass budgeted at the outset** — see §9.

### 6.4 Campaign 4 — The Panes

> **RE-ORDERED by Amendment 2 (§1b.10):** three campaigns now precede this one
> (The Lintel, The Blocking, The Precincts), and the glyph lattice this pane
> draws is **chamber-band only** — the walk band gets the topology-true chart.

Client-side, iterative, cheap. Where the tutorial's checklist finally pays
out. Includes the three near-free panes negation surfaced: **replay** (a save
is a seed plus its marks, so a replay is tiny and shareable as a URL),
**historiography**, and **foresight**. The last two need no new *physics* —
the `why` window ships, and The Foresight shipped the GOAP planner and its
goal rung — but each still needs a read surface shaped for a pane, which is
not the same as needing nothing. Cheap, not free.

### 6.5 Campaign 5 — Vitality

Wounds commit, health folds (§3.3). Ordered last only because it needs a
body model for the constitution term; ordered *before* any combat work,
because combat without it would invent a counter.

## 7. Wolverson's tutorial: a checklist, never a template

The tutorial is the best roguelike curriculum available and its value here is
narrow and real: it is the only one that covers the unglamorous middle layer
in depth — initiative and turn cost, a spatial index, a queued effects
system, targeting UI, status effects, spawn tables, hunger clock, ranged
combat, save/load. Read **Section 4 first**: it is Wolverson refactoring away
what Section 1 taught, so reading it first tells you which Section 1 lessons
are scaffolding to discard.

```
  tutorial subsystem      Hornvale mechanism          composite / verdict
  --------------------    ------------------------    ---------------------
  field of view           lossy perception            PROJECTION AS FOV --
                          projection                   the spatial special
                                                       case; no new machinery
  hunger clock            homeostatic drives          DERIVED HUNGER -- a
                                                       fold, not a counter.
                                                       Ships
  item identification     UNI-1 / MEM-8               IDENTIFICATION AS
                                                       INFERENCE
  spawn tables /          derived population +        DERIVED ENCOUNTER --
  difficulty curve        potency (0064)               who is there is who
                                                       lives there. Hornvale
                                                       wins outright
  save / load             0007 + played-world fold    SAVE AS SEED + MARKS
  equipment slots         BIO-1 body / EXP-3 senses   BODY-DERIVED SLOTS
  stairs down / levels    room mesh + apertures       APERTURES, NOT STAIRS
  bloodstains /           MAP-23 eyecandy licence     CLIENT-SIDE RESIDUE
  particle effects
  town portal /           enter/exit refusal          CLASH -- presumes the
  fast travel             (UNI-37)                     scale seam held shut
  dungeon map builders    derived terrain             ADMISSIBLE as
  (BSP / WFC / cellular)                               DERIVATION -- see below
  hit points              (none)                      -> §3.3
  initiative /            (none)                      *** THE GAP *** -> §6.2
  per-action turn cost
```

**Four architectural inversions to refuse explicitly**, because each is
taught as best practice there and is backwards here: in-engine rendering
(bracket-lib draws from inside Rust, and its wasm chapter ships engine plus
renderer together — decision 0022 is the inverse); mutable-ECS-as-truth
against `UNI-20`; rectangular `Vec<TileType>` maps with a stairs-down loop
against the icosahedral room mesh; and alignment-flavoured content against
decision 0021.

**One clarification the spec must state, because the obvious reading is
wrong:** "no procedural map builders" is *not* the lesson of 0022. A builder
that is a deterministic function of pins plus noise, seeded from the room's
own identity, is exactly the derive-never-declare move `windows/locale`
already makes for room description. What is refused is **authored layout** —
designer intent — not the algorithm. BSP, cellular automata, and WFC all
remain available.

## 8. In / out boundary

**In:** the per-turn snapshot schema; a widened wasm ABI; the intra-room tile
layer and object graph; per-agent action cost; a vitality fold; the pane set;
the three near-free panes.

**Out, deliberately:**

- **Multiplayer and observation-by-others.** Untouched by every pass; not
  scoped here.
- **Real-time play.** Forfeits the turn⊥frame decoupling the cheapness rests
  on (§4.1).
- **Continuous coordinates / physics in the bubble.** A chaotic
  forward-integrator seeded from quantized state, which the kernel's Lorenz
  guard-rail forbids. The fine layer's discreteness is protective.
- **Fast travel / town portals.** Presume the `enter`/`exit` scale seam
  `UNI-37` holds deliberately shut.
- **Player-chosen tile placement for dropped objects** (§3.2), accepted as a
  v1 cost.
- **Retiring the prose ABI.** The Casement's chapter and its byte-identity
  smoke keep working throughout.

## 9. Risks

1. **Prose primacy (leads G3 — see §10).** A permanently visible map pane
   demotes focalized prose, which Constitution §3.5 and `RENDER-4` make
   primary. Layout is not a preference the constitution can enforce.
2. **Determinism is not legibility.** *(Reframed by Amendment 1 §1a.7: the
   problem moves rather than vanishing — sightlines is the named criterion,
   solving is from-scratch, and the seed fills the residual DOF.)* The fine-layer placement derivation
   needs an aesthetic criterion — readable, plausible, uncrowded — and this
   project has a documented habit of shipping the first without the second:
   `MAP-68` The Overworld shipped "correct but the first-pass LOOK is not yet
   dialed in," and `MAP-67` The Diorama needed relief exaggeration moved
   120 → 800 because at 120 a 1000 m peak read flat. **Budget the visual
   tuning pass up front, with criteria as named constants.**
3. **API expansion outpacing measurement.** The Retainer exists because a
   per-turn regression was caught by review, and review will not scale here.
   Mitigation: `CLIENT-turn-cost-ratchet` — record per-turn costs in
   `scripts/timed.sh`'s ledger shape (which deliberately never gates,
   because wall times are machine-specific) and gate only a
   **self-normalizing ratio** measured within one run.
4. **Persistence-tier drift.** The position law is *emergent* from
   determinism plus cost, so its accidental version — some state persisting
   because nobody decided — is the default outcome of not enforcing it.
   Mitigation: `TOOL-persistence-class-tag`, default-deny on the
   `tools/type-audit` model.
5. **Cold start** (§5), unmitigated today.

## 10. Flagged for G3 — owner decisions, not autopilot's

> **Items 1 and 4 are SETTLED** by Amendment 1 (§1a.3 and §1a.2). Items 2, 3
> and 5 remain open.

1. **Prose primacy.** Four candidate resolutions are captured and *none is
   adopted*, in ascending order of how structurally each solves it: a
   non-spatial **discovery pane** (`CLIENT-discovery-pane`); the **alive
   map** as the agent's own cartographic artifact, partial and
   mis-remembered, so it is not a truth channel (`CLIENT-alive-map` — note
   `MAP-60` already holds that every style carries an implied maker/era, and
   nothing anticipated the maker being the player's own agent); the
   **instrument reframe** (`CLIENT-as-instrument` — an instrument's panes
   compete for the developer's eye, and the constitutional ordering concerns
   the reader's); and the **relational fine layer**
   (`CLIENT-relational-fine-layer` — `near(hearth)`, `between(door, table)`,
   the only option that resolves it *by construction*, since prose and tiles
   would then render one structure two ways). The last is a genuine fork in
   §6.3's design, not a garnish: adopting it makes risk 2 moot and changes
   what the tile layer *is*.
2. **The snapshot schema is save-format-class.** It joins the six `scene/*`
   kinds as a versioned contract (epoch suffix, never renamed).
   Schema-adjacent by the carve-out list, so it leads regardless of gate.
3. **Program size.** §3.5 and §6 make this producer-side work with
   drift-checked goldens across five campaigns — materially larger than "a
   web client" as first framed. Scaling it down is the owner's call.
4. **Is a tile a longer `RoomAddr` path?** (§4.4.) Settle before §6.3.
5. **The campaign name** `the-rose-window` is provisional.

## 11. Decisions to record in `docs/decisions/`

Three are new and load-bearing enough to belong in the log rather than only
in a spec:

1. **Fine position is never serialized** (§3.1) — the two-tier law, with
   quantize-at-emit-only as its stated lineage.
2. **Wounds commit; health folds** (§3.3) — no HP counter anywhere.
3. **One snapshot per commit; panes are pure projections** (§3.4).

## 12. Definition of done (per campaign, per decision 0013 and 0020)

Each campaign's own spec carries the full list. Program-level: the book gains
a chronicle entry per campaign and a freshness sweep; the `CLIENT-*` registry
rows flip `raw` → `spec'd` → `shipped` with **Where** repointed, never
deleted; each campaign writes a retrospective; and the Confidence Gradient is
re-scored where a campaign moves one of its bets (decision 0030).

## 13. Provenance

Brainstorm 2026-07-25, with four ideonomy passes to convergence:

1. abstraction-lift + dimension-identification, as an **atlas** — produced
   the snapshot contract, the epistemic pane partition, and the four clocks;
   overturned the initial sequencing.
2. substitution + combination, as a **timeline** — produced the coverage
   matrix (§7) and the action-clock gap; overturned the sequencing again and
   reclassified the program as producer-side.
3. tree-finding + negation, as a **map** — produced the territory map (§2),
   the situated-and-spatial identity, the instrument reframe, and the three
   near-free panes; corrected the scope.
4. organon-construction + cross-domain re-instantiation, as a **notation** —
   produced §3.2's persistence law, found vitality as the second unfilled
   slot, and imported stage management's vocabulary. **No overturns.**

Three overturns, all in the first two passes; pass 4 produced none. Nathan
supplied the two-tier position law (§3.1), the `wait`-is-a-day correction
that dissolved a false wall, the per-creature reading of disposition, and the
mob/object manipulation requirement.

Full decision ledger with alternatives discarded:
`.superpowers/sdd/decision-ledger.md`. Followups, including three
pre-existing `main` findings surfaced while benchmarking:
`.superpowers/sdd/followups.md`.
