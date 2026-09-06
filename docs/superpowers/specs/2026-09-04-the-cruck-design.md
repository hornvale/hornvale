# The Cruck — design

**Campaign:** The Cruck · **The Staple's R2** ("a building's shape has a
reason") · **Decision block:** 0786–0795 · **Ledger:**
[`2026-09-04-the-cruck.md`](../ledgers/2026-09-04-the-cruck.md) ·
**Status:** implemented 2026-09-05, pending close review and merge.

*A cruck is the pair of curved timbers that gives a timber building its
section. Nobody chooses the roofline; it is what the frame allows. This
campaign gives a structure's chamber graph a frame.*

---

## 1. The claim

A built structure's chamber graph — how many chambers, what each is for, and
which opens into which — is **derived from the brief and never drawn**. The
seed keeps exactly the freedom the derivation leaves it: which facets the
chambers stand at, and where the walls fall inside the plan.

Two things are true today and this claim reverses both
(`windows/vessel/src/structure.rs`, read at `3007f164c`):

- `structure_at` draws the chamber COUNT uniformly from `1..=4` off the
  locale's seed, then draws one facet per chamber. The brief is consulted
  once, as an existence gate (`brief.site.as_ref()?`). Its own doc says so:
  *"The brief is a GATE and never a parameter of the draw."*
- `links` is always the path `(0,1),(1,2),(2,3)` in draw order. The role of a
  chamber is then read off its INDEX (`interior::pattern::role_for`): 0 is
  the threshold, 1 the hearthroom, 2 "the place's business", the rest stores.

So a bugbear's cold-climate hall and a lizard-folk's warm-climate hut with
the same seed draw are the same corridor with the same rooms in the same
order. The Staple's R1 (The Housemark) made the threshold's FURNISHING
carry the people; the building around it still carries nothing.

**The direction of derivation is the spine of this design.** For a built
site, form follows use: the brief says what stands there and the grammar
says what shape that takes. For a wild site — a cave, an exotic site — the
derivation runs the other way, and always has: the rock made the form and a
people reads it (The Plat's "use follows form", `windows/worldgen/src/plat.rs`).
So a wild structure keeps today's drawn chain, byte for byte, by argument
rather than by omission (§3.5).

## 2. What the measurement allows

The Staple metaplan §1 fixed the inputs before this campaign existed: over
4,002 occupations on five seeds, `function` is `Agrarian` 98.7% of the time
and `Mine` otherwise, `notability` is `Common` 4,002 times out of 4,002, and
every LIVING occupation is `Classical`. A grammar indexed on those three axes
emits one shape per world. The axes that vary among living dwellings, and
that the vessel already carries on `Brief`, are:

```
  axis                    values                       source
  ----------------------  ---------------------------  --------------------------
  cold                    bool                         Terrain::is_cold at the walk band
  housemark.authority     Command | Common             SocietyVector.sociality (R1)
  housemark.threshold     Inward | Plain | Outward     SocietyVector.in_group_radius (R1)
  site.kind               Settlement | Cave | Exotic   decision 0666
  function                Agrarian (Mine 1.3%)         OccupationRecord (constant, kept derived)
  notability              Common                       OccupationRecord (constant, kept derived)
```

The live society registry carries seven `Hierarchic` and eight `Communal`
peoples with `in_group_radius` from 0.2 to 0.8, so both authority marks and
all three postures have living witnesses (The Housemark's H1 proved all six
cells occupied over 1,259 rooms). Whether `cold` splits each cell is measured
by this campaign's Task 0 (§7, H1), not assumed here.

**Every rule below reads an axis that varies, or reads a constant axis the
way the code already does.** A rule on `function` or `notability` is kept
because standing rule 4 of the Staple metaplan ("derived or absent") forbids
replacing it with a table, not because it will fire this year: the Seat's
hall and the mine's smithy are reachable the day D5 and D4 make them so, and
the grammar owes them a row now so that day costs nothing here.

### 2.1 What is fused today, and the three seams this design names

`structure_at` does three jobs in one function, and the third is done by
code that never sees the first:

```
  seam                   today                              this design
  ---------------------  ---------------------------------  --------------------------------
  grammar   brief -> tree   none; count drawn, links = path   ordered role inventory, one walk
  address   tree -> facets  one draw per chamber              unchanged draw, per-method label
  embed     tree -> lattice allocate/grow assume a CHAIN       generalised to a rooted tree
```

The embedders' chain assumption is stated in their own docs: `allocate.rs`
(*"structure_at builds a PATH graph, so the recursion is a chain"*) and
`grow.rs` (*"every later chamber TUNNELS out of the previous one"*). Both are
one substitution away from a tree — "the previous chamber" becomes "the
parent" — and §5 says exactly what that substitution costs.

## 3. The grammar

### 3.1 Notation

A structure-band rule has four slots, and the notation is the design:

```
  role        attach                       admitted where                  order
  ----------  ---------------------------  ------------------------------  -----
  threshold   ROOT                         always                          1
  hearthroom  beside threshold             always                          2
  hall        beside threshold             notability == Seat              3
  workroom    beside HEARTHROOM if cold    function is Agrarian, Mine,     4
              else beside THRESHOLD if       Fort or Cult (the room is
                posture == Outward           loomroom / smithy / smithy /
              else beside HEARTHROOM if      shrine, as role_for maps
                posture == Inward            them today)
              else (Plain) beside
                HEARTHROOM if Command,
                THRESHOLD if Common
  store       beside HEARTHROOM if cold    function is Some(_) — a place    5
              else beside HEARTHROOM if      with a business keeps its
                Command                      goods; Trade's business IS
              else beside THRESHOLD          the store (role_for today)
```

Reading the attach column as what it is — a justified permeability graph
(Hillier and Hanson): a room's DEPTH from the entry is who controls access
to it.

- **Cold nests everything on the hearth.** One fire heats the rooms that
  open off it; a warm-climate plan can afford rooms that open off the door.
  This is physics and it outranks the social rules below it.
- **Authority sets depth.** A `Command` people's rooms hang off the
  hearthroom — you pass the head of the house to reach anything. A `Common`
  people's rooms hang off the threshold — everyone reaches their own room
  without passing another's.
- **Posture sets who reaches the workroom.** `Outward` (guests are given
  water at the door, R1) puts the workroom at the door: outsiders reach it
  without entering the hearth — a shop-front. `Inward` (a screen at the
  door) puts it behind the hearth. `Plain` defers to authority.
- **The store is the household's, never the guest's**, so posture does not
  move it.

No rule reads `status_basis`, for the reason The Housemark gave: a mapping
from knowledge to a library would be a stereotype, not a spatial consequence.
No rule reads `tech`: it is a world clock, not a place axis (Staple §1.1).

### 3.2 The walk

The inventory is walked once, in order, against the brief. A rule is admitted
if its condition holds AND its parent role is already present AND the
structure is not full. An admitted rule appends one chamber carrying its role
and one link to the first chamber of the parent role. There is no fallback:
a rule whose parent is absent is refused, not re-attached to the root
(ledger #2). The result is a rooted tree at the threshold with every chamber
reachable from index 0, and `threshold == chambers[0]` still holds.

Count is not drawn (ledger #4). The single-chamber built dwelling disappears
as a consequence and the design says so: a hearthroom is always admitted, so
every built structure has a fire, where today a one-chamber cold dwelling
has none (the fire is confined to the hearthroom role).

### 3.3 The bound

`MAX_CHAMBERS = 4` stays, with its compile-time coupling to the collision
scan. The inventory ORDER is the priority order, and the walk stops
admitting when the structure is full. A Seat with a business therefore keeps
its hall and workroom and drops the store; nothing else can reach five today
(ledger #6). Raising the bound is D5's problem.

### 3.4 The shapes this reaches

Writing T, H, W, S for the four roles a living Agrarian dwelling can admit:

```
  cold?   authority   posture    tree                      name        picture
  ------  ----------  ---------  ------------------------  ----------  ---------------
  cold    any         any        T{ H{ W, S } }            deep         T-H<W,S
  warm    Command     Outward    T{ H{ S }, W }            shopfront    W-T-H-S
  warm    Command     Plain      T{ H{ W, S } }            deep         T-H<W,S
  warm    Command     Inward     T{ H{ W, S } }            deep         T-H<W,S
  warm    Common      Outward    T{ H, W, S }              bush         H,W,S all on T
  warm    Common      Plain      T{ H, W, S }              bush         H,W,S all on T
  warm    Common      Inward     T{ H{ W }, S }            backroom     S-T-H-W
```

```
     deep              bush             shopfront          backroom

     [T]               [T]                [T]               [T]
      |               / | \              /   \             /   \
     [H]           [H] [W] [S]        [W]    [H]         [S]    [H]
     / \                                      |                  |
   [W] [S]                                   [S]                [W]
```

Four shapes, three of them trees with a fork. That is the whole reachable
variety on the axes that vary, and the table is the preregistered
prediction H2 tests: the map from `(cold, authority, posture)` to shape is
many-to-one, and the collapses are stated here, before measurement, so that
"cold hides authority" is a prediction and not an excuse. A Mine's smithy or
a Seat's hall adds rows the same way; they are derived and today unreached.

(The count of shapes is incidental — The Blocking §4.3 binds: the substance
is the attach column and the admission column, and a reviewer should be able
to point at §3.1 as the campaign's content.)

### 3.5 Wild sites keep the chain

A cave or exotic site has no grammar to run: `function`, `notability` and
the housemark are all absent, and `cold` alone is not a reason for a cave's
shape. So `structure_at` keeps today's derivation for them exactly — count
drawn, facets drawn, links a path, roles by index (threshold, hearthroom,
then stores) — under the unchanged label `room/chambers/v1`. A wild
structure's roles SHOULD be a reading over its chain (The Plat's entry,
heart, sanctum); that is a later campaign's subject and is registered as
one (ledger #8).

## 4. What the seed still fills

- **Which facets** the chambers stand at: one draw per chamber, the same
  `child_path` extension and the same forward collision scan, under a NEW
  label for built sites, `room/chambers/built/v1` (§6).
- **Where the walls fall**: the rectilinear allocator's cut band, one draw
  per cut, `n - 1` cuts for `n` chambers — unchanged in count, so the
  Blocking's rule-7 equality (`dof == n - 1`) still holds for every tree.
- **Nothing else.** Rose Window §1a.7: the seed fills exactly the residual
  degrees of freedom, and a fully derived topology leaves none at the
  topology.

## 5. The consumers

### 5.1 `Structure`

Gains `roles: Vec<Role>`, index-aligned with `chambers`, and a `parent`
reading derived from `links` (the unique lower-indexed neighbour, which
exists for every chamber but the root because the walk appends children
after parents). The two invariants its doc states are replaced by three:

1. `threshold == chambers[0]`, unchanged.
2. `links` is a rooted TREE at index 0, every link `(parent, child)` with
   `parent < child`. The path graph is the special case where every chamber
   has one child.
3. `roles[i]` is the role of `chambers[i]`; no role appears twice.

> **AMENDED AFTER IMPLEMENTATION (2026-09-05, final review).** Invariant 3
> was never amended for the wild path, and it should have been: a WILD
> structure keeps the pre-campaign index reading (`index_role`), whose
> answer is `Store` at every index `>= 2`, so a four-chamber cave carries
> two `Store` chambers on purpose — kept byte-for-byte by §3.5's wild-path
> promise and asserted by
> `no_role_repeats_on_the_built_path_and_the_wild_path_keeps_its_duplicate_stores`
> (documented on the `Structure` struct,
> `windows/vessel/src/structure/mod.rs:47-58`). So invariant 3 holds for
> BUILT structures only: the grammar admits each role once there. §5.3's
> "role nouns are unique among apertures by construction" is unaffected —
> it holds at the APERTURE level even on a wild chain, since a chamber's
> two apertures are `index_role(i - 1)` and `index_role(i + 1)`, never both
> `Store`, which is the property `named_neighbour` actually needs.

`Role` moves out of `interior::pattern` into the structure module that now
owns it, with a re-export at the old path so `interior` reads it unchanged.
`role_for(index, brief)` is deleted; `chamber_interior_of` takes the role.

### 5.2 The embedders

**Rectilinear (`allocate.rs`).** The chain recursion becomes a tree
recursion. For a node with region `R` and `k` children: cut `R` along its
longer axis into the node's slice and the remainder (one draw, today's
`split`); then cut the remainder PERPENDICULAR to that first cut into `k`
strips (sequential `split`s, `k - 1` draws), so every strip shares a wall
with the node's slice; recurse into each strip with its child. A chain
(`k = 1`) runs exactly today's recursion. `carve` is unchanged: it already
refuses a doorway between two interiors that are not one wall apart, and
the strip construction makes every specified link one wall apart by
construction. Every reachable shape (§3.4, plus the chain) is embedded
non-degenerately at the 19×19 extent — asserted by test over the cross
product of shapes and seeds, not by this paragraph.

**Grown (`grow.rs`).** Chamber `i` tunnels out of `frontier[parent(i)]`
instead of `frontier[i - 1]`. Two draws per chamber, unchanged, so the
rule-7 equality `dof == 2n` holds. Wild sites are chains, so this
generalisation is exercised only by tests until a built site grows —
which none does (`embed_with` dispatches on `built`).

**The checker (`classify.rs`)** is graph-agnostic already: it reads the
realized links back off the cells and compares them to `structure.links` as
a set. No rule changes; rule 1 is what would catch an allocator that puts a
child strip against the wrong wall.

### 5.3 The session

`Session::further_in` keeps "the lowest-numbered higher neighbour" — with
one child that is still the one way in — but `named_neighbour`'s
`FURTHER_IN_WORDS` branch refuses at a fork and names the ways. A way is
named by its role noun (`Role::noun`: "hearth", "store", "loomroom",
"smithy", "shrine", "hall") or, as today, by a prose noun of the destination
chamber, and the existing "prose noun only where exactly one aperture"
restriction relaxes to "unique among this chamber's apertures". Role nouns
are unique among apertures by construction (§5.1 invariant 3). The footer
lists the ways: `Ways on: out, the hearth, the store.` at a fork, and
`Ways on: out, further in.` on a chain, so today's transcripts read the
same wherever the shape is still a chain (ledger #7).

`plan_of` and the client's "chamber i of n" are unchanged; `of` is still
the count and `i` is still an index.

## 6. Determinism, epoch, and artifact branch table

**Labels (ledger #3).** `room/chambers/v1` is NOT bumped: wild sites draw
under it exactly as today (count, then facets). Built sites draw their
facets under the new `room/chambers/built/v1`, one draw per chamber and no
count draw. This is The Blocking §3.2's method split applied one seam up:
the shared code (`child_path`) is untouched, the wild derivation is
unchanged, and the built derivation is new rather than modified. The stream
manifest gains one row. Decision 0073's rule is what this obeys — a label
whose draw sequence changed without a version is forbidden, and the wild
label's sequence did not change.

**Nothing serialized references a chamber.** Verified 2026-09-04 rather
than recalled: every `pack()` in `session.rs` packs a walk-band room
(`settlement_room` at 1889/1903, `position()` at 2828/9586, a walk-band
destination in a test at 11497), and `chamber_id` feeds only the plan and
the `[chamber N]` header. The Blocking §5.1's window — "the first mark made
INSIDE a chamber turns every chamber address into a reference" — is still
open, and this campaign does not close it.

**Nothing in the lab, the almanac, the chronicle or the CLI reads
`structure_at` or `MAX_CHAMBERS`**: `grep -rn "structure_at\|MAX_CHAMBERS"
windows/lab windows/chronicle windows/almanac windows/scene windows/explain
cli/src` returns one comment in `cave_rate_calibration.rs` and no code. So no
census column and no lab golden can move through this derivation.

After the first integrated implementation, run the artifact commands and
classify the observed diff:

```
  observed                                                    response
  ----------------------------------------------------------  -------------------------
  gallery transcripts that enter a BUILT structure move        RE-PIN (expected: exactly
    (possession-seed-42.md: 3 `enter`s, 2 `[chamber` lines;      these two — the over-time
    possession-carry-seed-14.md: 5 and 5; counted 2026-09-04)    walk and room-sample
                                                                 never `enter`, and must
                                                                 not move)
  clients/game/core/tests/fixtures/session-seed-42-chamber     RE-PIN
    .json and session-seed-14-carrying.json move
  book/src/reference/stream-manifest-generated.md gains        RE-PIN
    exactly one row
  docs/audits type-audit / plumb / placement reports move       RE-PIN (pub surface moved)
  a transcript that enters a CAVE moves                        STOP: the wild path was
                                                                 not left alone
  any ledger fact, census column, lab golden, or world JSON    STOP: outside the
    moves                                                        promised boundary
  a locale-band selection changes                               STOP: chamber roles must
                                                                 not reach the locale
```

No concept accession is expected: the roles exist, the anchor kinds exist,
and no new registered kind is named. If the implementation finds it needs
one, that is a separate classification under The Housemark's §7 precedent,
brought to G6 with its measured byte cost.

## 7. Preregistered acceptance claims

Frozen here; the plan's Task 0 runs the first before production code.

### H1 — the axes have living support, and `cold` splits them

Over every distinct player-addressable built settlement room on seeds
42, 13, 7, 1 and 100 (The Housemark's H3 population, 1,259 rooms at
`a126fe1fc`), tabulate `(cold, authority, posture)`.

Decision rule, not a prediction:

```
  finding                                                  response
  -------------------------------------------------------  ----------------------------
  >= 3 of the 4 shapes in §3.4 have >= 20 rooms each        proceed
  exactly 2 shapes have >= 20 rooms                          proceed; H2's recovery table
                                                             shrinks to what occurs, and the
                                                             chronicle says which rows are
                                                             unwitnessed
  cold is constant across all 1,259 rooms                    STOP: the physics row has no
                                                             subject; revise §3.1 before
                                                             building
  fewer than 2 shapes occur                                  STOP: the grammar is a
                                                             constant; refound the campaign
```

### H2 — the shape is the table

For every room in H1's population, the derived `(roles, links)` equals the
§3.4 row for its axes, at 100%. Reported with the same accounting discipline
as R1's H3: built-room, inhabited, collision and per-shape totals, none
silently dropped.

### H3 — the shape is recoverable without the brief

From `(roles, links)` alone, recover the preimage class of §3.4's map — the
set of `(cold, authority, posture)` triples that produce that shape — and
check the room's actual triple is in it, at 100%. A mutation that makes the
store always `beside threshold` must assert its target text exists, then
fail H2 on every cold room and every warm `Command` room.

### H4 — every reachable tree embeds faithfully

For each shape in §3.4 and the chain, over 256 seeds: the allocator
produces a lattice passing all eight checker rules, no chamber below
`MIN_CHAMBER_SPAN` on either axis, and `dof == n - 1` exactly. For the
grower over the same shapes: all eight rules and `dof == 2n` exactly.

> **AMENDED AFTER MEASUREMENT (2026-09-05, ledger #15, a post-G3
> narrowing of a preregistered claim).** The grower's parent
> generalisation realizes a fork on 2,536 of 2,560 (tree, seed) pairs and
> drops one link on 24 — `[(0,1),(0,2),(0,3)]` at the 15 seeds 5, 26, 46, 62, 65, 70, 94, 121, 137, 167, 180, 203, 211, 235, 249
> and `[(0,1),(1,2),(1,3)]` at the 9 seeds 34, 58, 60, 90, 110, 125, 184, 202, 218; on exactly those
> pairs rules 1, 3 and 8 fail together, while rule 2 and `dof == 2n` hold
> on every fork pair. Every structural remedy
> tried also moves GROWN bytes for chains, which §6 marks STOP. Production
> never hands the grower a fork (§3.5: wild sites are chains; built sites
> allocate), so the claim is narrowed to what is true: the grown arm holds
> all eight rules and `dof == 2n` EXACTLY over every chain, and the 24
> fork failures are pinned by tree and seed as a witness test that reddens
> if the set moves in either direction. The allocator's half of H4 stands
> unamended and green.

### H5 — the wild path is byte-identical

A committed golden of `(chambers, links, roles)` for a fixed set of
synthetic cave and exotic briefs and seeds, taken on main BEFORE the change
(Task 0), is reproduced after it. The built path's draws are counted:
exactly `n` under `room/chambers/built/v1`, none under `room/chambers/v1`.

### H6 — the reading boundary holds

The locale-band selection census (`selection(built, cold)`) is unchanged
across every `(built, cold)` combination; no `Fact` is committed by any
structure derivation; `room/furnishing/v1`, both `room/layout/v1/*` labels
and `room/chambers/v1` are untouched.

## 8. Testing and review surfaces

- Unit: the grammar walk over the cross product of the axes (generated from
  the enums, not seven copied fixtures), refusal on absent parent, the cap
  at four, no role twice, `parent < child`.
- Unit: the allocator over every tree on ≤4 nodes (16 rooted labelled
  trees, generated), rule 1 read back through `realized_links`. **CORRECTED
  AFTER IMPLEMENTATION (2026-09-05, final review):** the roster is 10
  parent-pointer trees under invariant 2 (`parent < child`) — 1 + 1 + 2 + 6
  across one, two, three and four nodes; Cayley's 16 counts labelled trees
  without that ordering, which is a different, larger set.
- Integration (`windows/vessel/tests/suite/`): H1–H3 as one readout over
  the five seeds, H4, H5, H6.
- Session: `enter further in` at a fork refuses and names the ways; `enter
  the store` at a fork enters it; a chain reads exactly as today.
- Review: the §3.1 table against the code, row by row; the §6 branch table
  against the actual `make rebaseline` diff.

## 9. What this campaign does not do

- No cycles, no courtyard, no second building at a facet (`built_rooms`
  still gives a settlement one walk-band facet; that widening is D1, and
  plural buildings are R3).
- No change to any interior pattern, any locale-band selection, or the
  housemark.
- No reading of `status_basis` or `tech`; no per-people table.
- No Plat-style reading of a wild chain (registered as a follow-up).
- No mark that changes a shape; no chamber-depth commit.
- No raise of `MAX_CHAMBERS`.
- No builder's-grammar memory on a ruin (CLIENT-housemark-provenance
  stands): a ruined dwelling's shape is derived from its brief as it stands.

## 10. Record consequences

- `SOC-staple-ladder` advances its R2 slice and points here.
- `CLIENT-district-patterns` records that the composer discipline has now
  run at two bands below the locale, and that R3 will lift the structure
  grammar's `Beside(role)` over a district graph.
- A new registry row for the wild-site role reading (ledger #8).
- Decisions, if the implementation validates the design: **0786** a
  structure's chamber graph is a derived tree over a structure-band grammar,
  never drawn; **0787** built and wild chamber draws are split by method
  under two labels; **0788** a fork is named by role, never by index.
- Three source comments naming The Precincts as the campaign that must
  revisit the path graph (`structure.rs:43`, `:104`, `session.rs` at the
  `how_many` refusal) are rewritten: this campaign did.

## 11. G3 flags

1. **A new stream label, `room/chambers/built/v1`, and no bump of
   `room/chambers/v1` (§6, ledger #3).** A label is permanent once
   published. The split follows The Blocking §3.2's method precedent and
   was chosen over a `v2` bump because the wild derivation is unchanged.
2. **Every built structure gains a fire and the one-room built dwelling
   disappears (§3.2, ledger #4).** A consequence of "derived or absent",
   stated rather than hidden.
3. **The §3.4 collapse table is a prediction with three stated collapses**
   (cold hides authority and posture; `Command` hides posture except
   `Outward`; `Common` hides `Plain` from `Outward`). If H1 finds `cold`
   constant, the physics row has no subject and the campaign stops before
   building.
4. **The seed-42 flagship transcript will move.** It enters a built
   structure twice (`enter`, `enter further in`), and a fork there changes
   the second line's shape. RE-PIN, not an epoch; but it is the transcript
   the book's live pane is compared against.

## 12. Provenance

The Staple metaplan (§1, §4 R2, standing rules 1, 2, 4, 5); The Housemark
(R1, decisions 0746–0748, the H3 population and its accounting discipline);
The Blocking (§3.2 method labels, §4.1 roles, §4.3, §5.1, §7); Rose Window
§1a.6, §1a.7, §1b.3 laws 1 and 3, §1b.4; decisions 0069, 0073, 0084, 0666.
Hillier and Hanson, *The Social Logic of Space* (1984), for the justified
graph reading of depth as control. Ideonomy: two G1 passes — the first
overturned two parts of the input recommendation (no household draw, no
`Within`) and added the label split and the isomer framing; the second
added the direction spine and dropped an explicit valency. Full rulings and
discarded branches are in the ledger.
