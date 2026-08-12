# The Chorography — the program for saying what a place is

*Metaplan. Five campaigns. Status: G3 review.*

Ptolemy separated **geography**, which maps the whole, from **chorography**,
which describes the particular place. Hornvale has excellent geography and a
thin chorography: it can tell you where every cell is, what it is made of and
how wet it is, and it has one flat word for what kind of place it is.

This program is about that word.

## 1. What occasioned it

A request to place dwarves underground. The underworld has no biomes, so the
two subterranean peoples authored for it were cut, and the source says so in
as many words (`domains/species/src/lib.rs:2369`):

> The Delvers (C2c) briefly added two subterranean PEOPLES here and withdrew
> them: a kind whose identity is DEPTH cannot be expressed by an axis measured
> in metres above sea level. **They return when the underworld has biomes.**

Twenty ideonomy passes over the architecture found that "give the underworld
biomes" is the visible end of something larger, and that most of the work is
cheaper than it looks because the project has already reached for the same
pattern in three places independently.

## 2. Keystone

> **A place-type is a point in a space, not a name in a list — and the space is
> the same one at every grain and in every realm.**

The corollary that does the work: *the coarse value is a boundary condition,
not a suggestion.* Adopted verbatim from The Rill, which reached it
independently for hydrology.

## 3. The findings this program rests on

All verified in the tree at `44d7fb9f`, not inferred.

### 3.1 The vocabulary exists and is unused

`domains/climate/src/facets.rs` already carries `Realm { medium, access }` with
`OVERWORLD`/`WATERWORLD`/`UNDERDARK`, five rock strata, five pelagic strata,
three cave `Formation`s mirroring `terrain::CaveKind`, and
`BiomeExpr { realm, formation, stratum }`. `BiomeExpr { UNDERDARK, KarstCave,
Basement }` is a legal value today. **Nothing in the workspace constructs one.**
`BiomeExpr::biome()` `unreachable!()`s on a cave formation, by design
(`facets.rs:305-320`), and the cave variant pool is empty with a documented
"not yet" (`variants.rs:733`).

### 3.2 The single spatial index is the root of five defects

`CellMap<T>` is a dense `Vec` over one `CellId` space; `RoomAddr` is tied to
the one icosahedron; and the accessor is flat:

    domains/climate/src/provider.rs:471    biome_at(&self, cell: CellId) -> Biome

The type knows about strata and the accessor throws them away. Five
consequences, each verified:

| # | Site | What it assumes |
|---|------|-----------------|
| 1 | `provider.rs:703` | `if is_marine() { marine } else { land }` |
| 2 | `graph_derive.rs:126` | `is_marine()` is the connection-graph separator |
| 3 | `provider.rs:1206` | asserts `realm == WATERWORLD` ⟺ `is_marine()` |
| 4 | `vantage.rs:64` | `submerged := stratum != Surface` |
| 5 | census column | `dominant-land-biome` |

An underworld biome is not `WATERWORLD`, so `is_marine()` is false, so a karst
cavern would be counted as **land** — in the census and in the connection
graph. A chamber at `Stratum::Basement` would tell the game client the player
is underwater.

### 3.3 The sea separated community from depth already; it still returns a point

`classify_marine_expr` (`biome.rs:375`) derives `stratum` independently of
`formation`, and states why:

> depth no longer competes with community for the single return slot, so a vent
> is a community AT a depth rather than one that displaced a depth.

But `Stratum::at_depth_m(depth_m)` reads the **floor** depth, so a cell yields
one expression, not the column above it. The vocabulary for a column exists;
the enumeration does not.

### 3.4 The compositional pattern already exists, three times, unnamed

- `variant_pool(formation, stratum, ground)` (`variants.rs:349`) is a live
  cross-product with `GroundKind` — a five-value substrate axis.
- `MicroField { relief, aspect, wetness, openness }` (`locale/src/micro.rs`) is
  a four-axis room-grain vector drawn as pure noise, consulting the world for
  nothing. `openness` is canopy closure; `aspect` drives light.
- The Rill's Task 5 is grounding `wetness` as *a budget and an allocation* —
  cell-scale moisture redistributed by position relative to the local
  watercourse. That is this program's pattern, for one axis, shipping now.

### 3.5 Nothing underground can be told apart

`subterranean_substrate` (`worldgen/src/lib.rs:2397`) passes temperature and
height through unchanged and sets moisture and insolation to **world
constants** (`0.90`, `0.0`). Its own doc concedes that only those two axes
distinguish a chamber, so every chamber in every world reads identically.
`ConditionNiche` (`species/src/lib.rs:350`) offers four axes — temperature,
moisture, insolation, elevation — of which two are constant underground and two
pass through. **This is the root cause of the defect The Delvers diagnosed one
level too shallow**: Duergar was authored on an elevation curve because
elevation was the only axis left that varied.

### 3.6 Consumers migrate one at a time, and the recipe is in the tree

`windows/locale/src/grammar.rs` was already re-keyed from `Biome` to
`Formation`, guarded by a committed before-arm fixture and the assertion that
the re-key changed *not a single draw*. That is expand-contract, executed once
here already. The Rill Task 5 uses the same shape. No campaign in this program
needs a flag day.

### 3.7 The language domain prices the taxonomy

`domains/language/src/accession.rs` acquires concepts in append-only **epoch
cohorts**, under the rule that every word already spoken keeps its form; epoch 2
names its own cause as nine of the ten marine biomes. Every *named* community
is therefore a permanent word in a versioned cohort. Axis values are few and
stable; names are expensive and deliberate. This is a feature — it is the brake
that keeps a generated space from becoming a naming free-for-all.

## 4. The campaigns

    0. THE FATHOM      the column: give every realm its depth coordinate, and
                       kill the five two-realm assumptions. Pure refactor plus
                       the sea's column as its first consumer. Byte-identical
                       worlds. Unblocks everything below.

    1. THE AXES        decompose community into independently-valued axes,
                       shared across grain and realm; unify ConditionNiche with
                       BiomeAffinity into one response vector; anthropogenic
                       communities included. Validated by RECONSTRUCTION against
                       the existing corpus, not by argument. Consumers migrate
                       expand-contract.

    2. THE UNDERWORLD  the dwarves: underdark communities as points in (1)'s
                       space, chamber-addressed, placed by realm-aware capacity.
                       Restores Mountain and Duergar. Consumer of 0 and 1.

    3. THE PLANES      transit realms with their own index, generated top-down
                       from world-derived boundary conditions, on demand.
                       Address-derived, never generation-ordered; events in an
                       override map, never written to the ledger. The observer
                       axis rides the existing KnownChannel.

    4. THE DISTURBANCE the keystone campaign: succession, tenure, arrest, and
                       the back-edge from inhabitants to world. Made places,
                       wrought places, ruins. Undead carrying capacity as a
                       function of historical occupation rather than
                       productivity.

### Why this order

0 before all: it is the smallest, it is a pure refactor with a byte-identity
proof, and every later campaign needs a non-flat accessor.

1 before 2: authoring underworld communities as atoms before the axes exist is
work that gets redone. The Delvers already paid this once.

3 after 1: a plane's communities should be points in the same space, or planes
become a parallel taxonomy and the program has failed.

4 last and biggest: it needs a place-type vocabulary to change *into*, and it
opens a write path from inhabitants to the world, which touches determinism,
stream order and the ledger.

## 5. What is deliberately NOT in this program

- **Grain.** Mangrove fringes, cypress domes and gallery forests are room-scale
  objects (a room is ~1.7 km; a cell is ~110 km). They belong to the fine layer
  — The Grain's territory, consuming The Rill's channel network. Putting them in
  `Formation` would be a category error of the same shape as
  `Formation::KarstCave`.
- **The `Biome` enum → open registry migration.** The atoms survive as a legacy
  projection throughout. Campaign 1 adds a vector *underneath* the named types;
  it does not delete 189 references.
- **Composing the eight parallel place-describing enums.** `Biome`, `Formation`,
  `Stratum`, `Realm`, `RockClass`, `CaveKind`, `BandKind`, `WaterKind` stay as
  they are, hand-mirrored across the terrain/climate border per decision 0094.

## 6. Relationship to live work

Three campaigns are in flight (`the-rill`, `the-docket`, `the-repose`). The
overlap is with The Rill and The Grain, and it is a hand-off rather than a
collision **provided this program follows rather than races**:

- The Rill's Task 5 establishes budget-and-allocation on the water axis. The
  Axes campaign generalizes it; it must not re-derive it.
- The Grain's thesis — the fine layer should vary — is the same rung one
  campaign earlier. The Axes campaign supplies the vocabulary The Grain's fine
  layer currently lacks.
- The Rill's save-format note transfers: grounding an axis must **keep its
  draw** and spend it as variation about the grounded value, or every room in
  every world shifts.

Posted to the board as a `notice` routed to `domains/climate/`,
`windows/locale/`, `domains/terrain/`, `windows/worldgen/`.

## 7. What is unvalidated

**The axis list is derived by argument and has never been tested.** The
proposed axes — physiognomy, energy, water, substrate, light, disturbance — are
this author's synthesis of vegetation science and distinctive-feature
phonology, not a measurement. Campaign 1 must open with a **reconstruction
test**, preregistered:

> Assign an axis vector to all 21 existing `Formation`s and ~50 `Variant`s.
> Two distinct existing names colliding on one vector means the axes are too
> coarse. A name that resists assignment means the list is incomplete. The axis
> set is the *output* of this test, not its input.

Two of the six axes are additionally suspected **collinear** with inputs
`classify_land` already reads: physiognomy and water regime are plausibly pure
functions of temperature and moisture. If so the decomposition adds no
information and is a *factoring* — which buys compositionality, not fidelity.
The campaign must say which it is, measured, rather than claim the stronger one.

## 8. Provenance

Brainstorm of 2026-08-12, under `campaign-autopilot`. Twenty `ideonomy-plain`
passes; the program shape was stable from pass 12 onward, and passes 17-20
produced only cost, procedure, and findings that fit inside existing campaigns.
Nathan's observation that `biome_at(cell)` assumes one map produced campaign 0
and *reduced* the program's total scope.
