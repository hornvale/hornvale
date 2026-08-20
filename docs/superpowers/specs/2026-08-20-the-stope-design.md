# The Stope — design

*A stope is the void a working opens as it takes ore out. It is the shape a
mine leaves behind, and it grows.*

**Status:** spec, awaiting G3.
**Autopilot:** engaged. Ledger at `.superpowers/sdd/decision-ledger.md`.
**Relation to The Winze:** The Winze wanted a delving that finds something.
This is the substrate under it — the underworld has no floors, no branches
and one entrance, so there is nowhere for a delving to *go*. The Winze waits.

---

## 1. What occasioned it

The underworld ships as **a five-storey building with four stairwells, where
each storey is exactly one room**, and that room has no interior.

```
ChamberAddr { cell, entrance, band, slot }
  band  = one of five depth classes      (Undercroft .. Sunless)
  slot  = 0..SLOTS_PER_BAND (=4)
  entrance = u8, and EVERY CALLER PASSES 0
```

Adjacency already exists in both directions — sideways within a band,
vertically between bands at the same slot — so `slot` is *already* a branch
that persists downward. Nothing has ever used it as one.

Measured consequences:

- **Realized chambers per land cell: 0.50 / 0.61 / 0.80** across seeds 42 / 7
  / 1234. The underworld is *smaller* than the surface, not larger. (An
  earlier claim of 50-80x was a lattice ceiling — caves times maximum slots —
  and not a count. Corrected in `BIO-underworld-has-no-energy`.)
- `Chamber` is **a point with properties** — rung, stratum, conditions — and
  carries no interior. `RoomAddr` is a *surface* address (icosahedron face
  plus refinement path) and never reaches underground.
- **`CaveKind` (Karst / LavaTube / Fracture) is derived, varies, and nothing
  downstream reads it.** A dissolution maze, a drained lava tube and a
  joint-controlled fracture system generate identically today.

## 2. Keystone

> **A branch owns a character; a character owns a run of floors; an engine
> owns a run.** Interleaving is impossible because the engine cannot reach
> across a branch boundary.

## 3. The design

### 3.1 The address gains a floor; `entrance` stops being a lie

```
TODAY      { cell, entrance, band, slot }
PROPOSED   { cell, entrance, branch, band, floor }

  entrance  0..E   ways IN. Plural, and a mapping to branches.
  branch    0..S   what `slot` already is: a column persisting downward.
                   RENAMED, because "slot" reads as a position and it is an
                   identity.
  band      0..4   depth class. UNCHANGED --- five is right.
  floor     0..F(band)   the missing rung.
```

Floors per band, as ranges rather than constants (the counts are drawn per
branch, and the distribution is Task 2's to measure, not this spec's to
freeze):

```
  Undercroft  1-5      \
  Shallows    3-10      |   15-50 floors in a system that runs the full
  Deeps       5-20      |   ladder --- and most systems terminate earlier,
  Underdeep   5-10      |   because depth_reach_m already says where they stop
  Sunless     1-5      /
```

**A system reaching Sunless should be uncommon.** That is a prediction, not a
setting: `depth_reach_m` already determines termination, so the rate is
measurable before anything is built (§4.1).

### 3.2 A floor is a point with properties, and that is sufficient

A floor carries what a chamber carries now — rung, stratum, rock, temperature,
moisture, water table — plus its engine. **An engine generates the floor's map
from that, in the ordinary roguelike way, at the moment something looks.**

Explicitly NOT required, by Nathan's ruling: intra-floor consistency across
visits beyond what the seed gives, and vertical alignment between floors.
Nobody expects a staircase to land in a corresponding spot on the level below,
and pretending otherwise would buy nothing and cost a great deal.

This keeps the world-scale lattice coarse and pushes every detail into the
engine, which is where detail belongs and where `windows/locale` already
lives.

### 3.3 A branch owns a character; an engine set is `f(character, band)`

```
a branch draws a CHARACTER      generic cave, dwarven kingdom, drow city,
                                duergar hold, the eldritch, ...

engine set = f(character, band)
    Dwarven Kingdom:  Undercroft -> gatehouse   (heavy gate, guard post,
                                     bureaucrat's office, canteen, stair down)
                      Shallows   -> kingdom proper
                      Deeps      -> deep works
                      Underdeep  -> ABSENT; the kingdom ends

a RUN = the floors of one branch within one band, and one engine owns it
```

**Coherence is structural, not a rule to enforce.** No generic-dungeon floor
can appear between two dwarven-kingdom floors, because the engine owns the
whole run; and no dwarven kingdom appears under a blacksmith's cellar, because
the cellar is a different branch.

**A character declares which bands it may occupy.** Drow and Duergar are
Underdeep characters; the eldritch is Sunless; a cellar is Undercroft-only. A
branch that outlives its character's range terminates or hands off. This is
the modularity rule: **a new character is a new table entry and edits nothing
else** — the same discipline `windows/worldgen`'s domain roster already has.

### 3.4 Entrances are plural, and they are what make two doors interesting

`entrance` becomes a real count, and **entrance → branch is a mapping.** Two
entrances that open on different branches are two genuinely different
experiences of one cave system: the well in the town square drops into the
natural cave; the blacksmith's cellar drops into the dwarven works. Same cell,
same system, different character.

Terrain reports one cave per cell with no aperture count today, so the
aperture count is derived here at the composition root, from what the cave
already carries. **`ChamberAddr.entrance`'s own doc anticipates exactly this**
("the field exists so a future terrain change does not require relayering this
type").

### 3.5 Junctions: the underworld becomes a network

Two systems that both reach a shared band, in adjacent cells, may join there.
That realizes `MAP-underworld-shortcut` — *"hard to enter, easy to traverse
once inside; two points far apart on the surface can be close below"* — which
is false by construction today.

**A junction must be DERIVED, never drawn.** The temptation is a random link;
the honest version keys on facts (shared band, cell adjacency, compatible
characters) so a shortcut is a consequence of the geology rather than a die
roll. A drawn link would also be a new draw, and therefore an epoch on top of
an epoch.

### 3.6 Where the variety comes from

```
character per branch    x  which bands it occupies
                        x  where the system terminates      (SHIPS)
                        x  branch count and their characters
                        x  entrance count and branch mapping
                        x  per-chamber conditions            (SHIPS)
                        x  CaveKind                          (SHIPS, UNREAD)
```

Every factor but `character`, `floor` and entrance-count already ships. The
cheapest single win in the campaign is **making `CaveKind` select an engine**:
karst, lava tube and fracture are three shipped, derived, three-valued
characters that nothing reads.

## 4. Preregistration

Frozen before the code (decision 0016). Branch tables, not predictions.

### 4.1 Where do systems terminate, and how rare is Sunless?

Over the seed panel, from `depth_reach_m` alone — **measurable before any
code**: the share of systems whose deepest band is each rung.

```
Sunless is common (>25% of systems)   -> the ladder's ΔT boundaries, not this
                                         spec, decide rarity. Report and stop:
                                         the eldritch band being ordinary is a
                                         finding about The Underworld's
                                         calibration, not this campaign's.
Sunless is 1-15%                      -> proceed. Report the exact rate.
Sunless is ~0%                        -> the deepest characters have nowhere
                                         to live. STOP and report; this is
                                         the same shape as GateScar's 0/0/36.
```

### 4.2 Do the floor counts produce the intended range?

After §3.1 lands: the distribution of total floors per system. The intent is
15-50 for a full-ladder system and fewer for the common case.

```
median total floors < 5      -> the ranges are not producing depth; report
median in 5-25               -> proceed
>50 routinely                -> too many; a system nobody can exhaust is the
                                same defect as one with nothing in it
```

### 4.3 Does character actually vary?

The share of branches by character, per world. **A world that is 90% generic
cave has not solved the oatmeal problem**, and that is the campaign's own
falsification criterion.

```
one character >80% of branches  -> FAILED. Report as the headline. Do NOT
                                   re-weight to rescue it without saying so
                                   in the chronicle.
```

### 4.4 Determinism

Every new count (floors per run, branches, entrances) is a **draw**, and each
one must be keyed on **a place in a fixed lattice, never a generation
ordinal** (decision 0102).

## 5. Save-format consequences — THIS IS AN EPOCH

**The address is the seed-derivation key.** This is the largest consequence
in the campaign and it leads the G3 flagged section:

```rust
fn chamber_key(addr: ChamberAddr) -> String {
    format!("{}/{}/{band}/{}", addr.cell.0, addr.entrance, addr.slot)
}
fn chamber_stream(seed, addr) -> Stream {
    seed.derive(CHAMBER).derive(StreamLabel::dynamic(&chamber_key(addr)))
}
```

Adding `floor` and renaming `slot` changes `chamber_key`, which changes every
chamber's derived stream, which **relocates every chamber in every world**.
That is an epoch, not a drift.

Consequences that follow, and none may be skipped:

- The label takes an **epoch suffix** (`chamber/v2`), never a rename — the
  discipline `settlement/name/v2` set.
- Every committed artifact touching chambers regenerates: the vessel session
  fixtures, the census, the gallery.
- **The Underworld's drow seating moves**, because seating reads chamber
  addresses. Its committed readouts are witnesses and will fire; they must be
  re-derived and re-pinned, not silenced.
- New draws (floors, branches, entrances, junction derivation) each perturb
  stream consumption order.

**Nothing in §3 may be built before the epoch is deliberately accepted.**

## 6. Non-goals

- **No intra-floor consistency guarantee and no vertical alignment.** Nathan's
  ruling; §3.2.
- **No room-grain address underground.** A floor is a point plus an engine;
  `RoomAddr` stays a surface construct until something needs otherwise.
- **No new energy model.** Feeding the underworld is
  `BIO-underworld-has-no-energy` and its own campaign; this one gives it
  somewhere to be fed.
- **No metaphysics.** The Sunless band gets a character slot and no content:
  `thaumic` stays 0.0, UNI-2 unratified, nothing named.
- **No mines, no hazard, no wound.** The Winze resumes after this.

## 7. Task shape

```
0  MEASURE termination rates from depth_reach_m alone (4.1) --- no code
1  the address: floor added, slot renamed to branch, chamber/v2 epoch
2  floors per run: counts drawn, keyed on the lattice
3  character per branch; the band-eligibility table
4  engine sets: f(character, band); CaveKind selects for the generic case
5  entrances: plural, mapped to branches
6  junctions: derived, never drawn
7  MEASURE 4.2 and 4.3; the oatmeal criterion is falsifiable here
8  book, chronicle, retrospective, decision record, epoch note
```

## 8. Provenance

Nathan's design, 2026-08-20, in conversation. Three `ideonomy-plain` passes
across the surrounding work; this spec's shape came from the last —
tree-finding + abstraction-lift + substitution over scale and lattice
organons, on the axes *cardinality*, *autonomy*, *age*, *modularity*,
*longevity*.

The substitution that produced §3.3 is worth recording: I had written "a
**system** draws a character." Moving the owner one rung down to the
**branch** is what makes multiple entrances meaningful, and it is NetHack's
Gnomish Mines relation — a branch with its own generator hanging off a main
dungeon. The scale organon is what exposed the missing `floor` rung and the
vestigial `entrance` one, sitting either side of the grain the lattice
actually addresses.
