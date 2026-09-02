# The Brattice — the underworld gets something in the way

**Campaign:** The Circuit, campaign 2 (metaplan
`docs/superpowers/specs/2026-09-01-the-circuit-metaplan.md` §3). Predecessor:
The Crosscut (`docs/superpowers/specs/2026-09-01-the-crosscut-design.md`,
decisions 0566–0568, merged `daa94492c`). Successor: The Plat.
**Ledger:** `docs/superpowers/ledgers/2026-09-02-the-brattice.md`.
**Decision block:** 0616–0625.

A brattice, in a mine, is a partition hung across a working to make the air
go the long way round. Nothing about the working changes for the air: the
loop is still a loop. What changes is which way through is open, and to whom.

## 1. What occasioned it

The Crosscut left the underworld with somewhere to go around and nothing in
the way. Every descent is a series-parallel graph of regions grown before a
level is carved; a cycle may put one of its two paths a floor below the
other; stairs pair by coordinate; two adjacent regions the plan left unlinked
keep solid rock between them. It exports, and reads nothing of, three
attributes: each realm's `LengthClass` (Dormans' four combinations of a long
and a short side), each node's `depth` (hops from the entrance), each node's
innermost `realm`. The spec said in so many words: "The Brattice reads
`LengthClass` to select a pattern." (§3.2 step 6.)

Four facts from the shipped code shape what a gate can be, and the metaplan's
one-line charter assumed two of them the wrong way round:

- **The plan has no attribute slot.** `Node { level, cell, depth, realm }`
  and `Edge { a, b, kind: Passage | Stair { x, y } }` are closed
  (`windows/worldgen/src/circuit.rs:148-187`). `Realm.class` is the only
  semantic tag. Anything a gate needs is added, not migrated.
- **`extend` splices.** The series move deletes a passage and inserts a chain
  into every realm path that carried it (`circuit.rs:906-929`). An attribute
  keyed on an edge during growth would be orphaned by the next `extend`. So
  gates are stamped on the *finished* plan, by a pass that runs after growth
  and touches no edge the grammar might still rewrite.
- **There is no door Thing.** The metaplan's parenthetical "doors as Things
  (The Chattel)" assumed one shipped. The Chattel shipped `threshold`
  (`AffordsPassage`) and `cave-mouth` (`AffordsPassage`, `Openable`), a
  `key` kind (`Portable`), a `strongbox` (`Openable`, `Lockable`), and
  `open`/`close`/`take`/`drop`/`put`/`carrying` — no `door`, no `unlock`, and
  no Thing has ever been placed on a cave rung, because thing identity is
  keyed on a *building* room facet (`windows/vessel/src/thing.rs:12-15`) and
  the underworld composes no interiors. The Chattel's chronicle sentence
  "with `lock` and `unlock` beside them" is false against the tree; unlocking
  is a side effect of `open`. What The Chattel *did* ship that this campaign
  stands on is decision 0396 — a passage is a thing and openness is its
  as-of-day fold — and the custody machinery (`located_in_holder_fact`,
  `held_by`, `lying_in`), which is body-agnostic.
- **Nothing swims or flies.** `MovementMode::{Swim, Fly}` are
  `#[allow(dead_code)]` variants nothing returns
  (`windows/vessel/src/underworld_level/mod.rs:35-57`); `movement_mode(kind)`
  reads the cell kind and nothing else; no species carries a locomotion
  trait. But the species registry holds six kinds that plainly swim
  (reef-shark, killer-whale, giant-octopus, giant-squid, giant-crocodile,
  sea-elf) and three that fly (the dragons), so a capability key has real
  bearers the moment a component says so.

And one fact from the walk, which this campaign does not change and must
say so: `Underground::enter` hardcodes `Character::WildCave` and
`ChamberOrigin::Found` for every rung (`windows/vessel/src/underground.rs:
262-322`). A door needs a maker (§3.3), so a player walking today's world
meets sumps and chutes, and meets a door only when The Plat gives the walk a
worked place to stand in. The plan carries doors for every worked descent
in the world, the panel measures them, and a session test walks one; the
production walk shows them to nobody yet. This is the program's own
ordering — The Plat is "the first production writer of `ChamberOverrides`
so that a Made chamber exists in a world a player can reach" — and it is
flagged rather than silently accepted.

## 2. Keystone

**The same loop, walked with less.** A resident holds every key and walks
the cycle either way; an intruder holds nothing and must go the long way to
fetch what the short way demands. A gate is a *requirement on a way*, a key
is *where you get it*, and Dormans' lock-and-key cycle is the difference
between the two walks. Three commitments follow:

1. **The pattern says where; the world says what.** A cycle pattern is a
   placement rule — which side of a realm, near or far, symmetric or not,
   key or capability. The *substance* of the requirement is derived from the
   rock and the work, the way cycle density is (decision 0568): a door needs
   a maker, a sump needs water-cut rock, a chute needs a floor below. No
   gate is hand-placed and no gate is drawn where a derivation exists.
2. **Valve and asymmetric are one gate seen by two bodies.** The drop is a
   stairway with its up half omitted (decision 0567's Consequence): free
   down, `Fly` up. To a dragon it is asymmetric; to everyone else it is a
   valve. Decision 0347 already says an affordance is a relation between
   object and body; the gate taxonomy's "valve / asymmetric" axis is that
   relation, not a property stamped on the edge.
3. **Solvability is an invariant, not a prediction.** A body holding nothing
   and able only to walk and wade can reach the terminus and every key from
   the entrance, on every descent, by construction — the same product-graph
   reachability serves the stamping pass (which refuses a placement that
   would break it), the guard test, and the readout's "intruder's cost".
   Dormans' safety rule ("the key before the lock") is this reachability
   stated per lock.

## 3. The design

### 3.1 Gates on the plan

```
  Edge  { a, b, kind, gate: Option<Gate> }          Node { …, key: Option<KeyFor> }

  Gate  { toward_b: Way, toward_a: Way,             KeyFor(EdgeIx)  this node holds
          hazard: Option<Hazard>,                                   the key for that
          persistence: Persistence,                                 edge's gate
          pattern: PatternIx }
  Way         = Open | Needs(Requirement)
  Requirement = Key(NodeId)                         the node holding the key
              | Mode(Capability)                    Swim | Fly (worldgen's own enum;
                                                    vessel maps it onto MovementMode)
  Hazard      = Dangerous | Uncertain               STAMPED, realized by nothing here
  Persistence = Permanent | Collapsing              STAMPED, realized by nothing here
```

A door is `Needs(Key(n))` both ways. A sump is `Needs(Mode(Swim))` both
ways. A chute is a `Stair` edge with `toward_b` (down) `Open` and
`toward_a` (up) `Needs(Mode(Fly))`. There is no `Shut` way: nothing in a
cave is passable one way for *every* body, and a `Shut` variant would be a
reserved seam with no constructor, the shape decision 0398 refuses
(ledger #2). A true one-way passage is a captured follow-up.

`hazard` and `persistence` exist so the full taxonomy the metaplan names is
*expressible as data* and so a pattern row can state it; they are exported
and read by nothing this campaign, exactly as the Crosscut exported
`LengthClass`. Danger is The Plat's hoarder; secrecy needs a render change;
a collapsing lock needs a crossing event no fact yet records. Each is a
registry row (§6).

`DescentPlan` gains `patterns: Vec<Applied>` — per realm, the pattern index
drawn, and either `Applied` or `Skipped(reason)` with
`reason ∈ {Inadmissible, Claimed, NoRoom, Unsolvable}` — plus the counter
`skipped_patterns`. Nothing about the graph's nodes, edges, realms or
stairs changes; a plan with every gate ignored is the Crosscut's plan.

### 3.2 The inventory, and the pass that applies it

**The inventory is data; the resolver is code** (decision 0011, the Hearth's
`pattern.rs` shape): a `const CYCLE_PATTERNS: &[CyclePattern]` table in
`windows/worldgen/src/brattice.rs`, one row per pattern, each row citing its
source. Its length is asserted by a test; changing it is a deliberate act
(decision 0016's freeze, applied the way `tropes/` applies it).

```
  CyclePattern { name, source, classes: &[LengthClass], span: Span,
                 gates: &[GateSpec], key: Option<KeySpec>,
                 hazard: Option<(Side, Hazard)>, persistence: Persistence }
  Span     = SameFloor | CrossFloor | Either
  Side     = Long | Short          the realm's two paths by its class; for
                                   LongLong / ShortShort, Long = path_a
           | Descending            path_b of a CrossFloor realm — the path that
                                   changes floor, whichever length it has
                                   (execution amendment, Task 1 ruling D)
  Slot     = Near | Far            by `depth`: the shared endpoint with the
                                   smaller depth is Near
  GateSpec { side, slot, way: Symmetric(ReqKind) | DownFreeUpNeeds(ReqKind) }
  ReqKind  = Key | Natural         Natural resolves by §3.3
  KeySpec  { side, slot }          the interior node of `side` adjacent to
                                   `slot`'s endpoint; no interior node -> NoRoom
```

A gate at `(side, Near)` sits on the edge of that path adjacent to the near
endpoint; `DownFreeUpNeeds` is legal only on a `Stair` edge, which a
cross-floor realm's short side has at both ends.

**The rows frozen by this spec.** Nine — ten as approved at G3, less one
removed in execution (below) — each with a source the repository can
attest; the schema admits more. Dormans' Fig. 9.8 names are transcribed at
spec time and **Nathan confirms them against the figure at G3** — the repo
holds the count "fourteen" and no list (metaplan §5), so the rows below are
the ones whose placement the grammar can express and whose provenance the
Crosscut's organon or the metaplan already states. Rows a future reader of
the figure adds or renames before Task 1 asserts the count are the freeze;
after that they are an epoch of `underworld/gate/v1` (§5).

```
  #  name                        source            classes     span    gates / key / stamp
  1  two-alternative-paths       Fig. 9.8          LongLong    Either  none (named, stamps nothing)
  2  hidden-shortcut             Fig. 9.8          LongShort   Same    hazard Uncertain on Short
  3  dangerous-route             Fig. 9.8          LongShort   Same    hazard Dangerous on Short
  4  lock-and-key-cycle          Fig. 9.8          ShortLong   Same    Short/Near Symmetric(Key);
                                                                       key Long/Far
  5  the-sump                    Dormans' aside    ShortLong   Same    Short/Near Symmetric(Natural)
                                 (conditional lock)
  6  patrol-path                 Fig. 9.8          ShortShort  Same    hazard Dangerous on both
  7  blocked-retreat             Fig. 9.8          LongLong,   Same    persistence Collapsing,
                                                   ShortLong           Long/Near, ways Open
  8  the-chute                   organon PREDICTED LongShort,  Cross   Descending/Near
                                 (ledger, Crosscut) ShortLong,          DownFreeUpNeeds(Natural)
                                                   LongLong
  9  key-downstairs-lock-        organon PREDICTED ShortLong   Cross   Short/Near Symmetric(Key);
     upstairs                                                          key Long/Far (on l+1)
```

**Execution amendments to the table (Task 1, ledger #10), each measured
before it was made.** (i) `Realm.class` was frozen at a realm's CREATION,
where `path_a` has at most three edges and `path_b` at least two, so
`LongShort` could never occur — zero of 4,412 realms — and `try_extend`
later splices chains into both paths, so the exported class described a
graph that no longer existed. A Crosscut latent defect nothing had read: the
class is now recomputed after growth from the realized paths, by the same
rule. (ii) The chute's gate was specified on the `Short` side, which under
`ShortLong` is `path_a`, a same-floor path with no stair: 717 of 823 chute
draws skipped `NoRoom`. The drop belongs on the path that descends, so
`Side::Descending` names it and the row admits every cross-floor class but
`ShortShort`; after the change 823 of 823 apply. (iii) The tenth row, `the-
landing-hall` (`ShortShort × CrossFloor`), drew zero times: a cross-floor
`path_b` is created with at least three edges, so both paths cannot be short.
The cell is empty by construction, this section's own rule forbids a row
nothing selects, and the row is removed; the count is **nine**, asserted, and
a `no_row_is_dead_data` sweep asserts every remaining row is applied somewhere.
The landing hall stays The Plat's name for a stair pair one region apart, on
whatever realm class carries one.

The organon's two whole-run cells (the delving's shaft, the key at the
bottom) and its hub column are The Plat's spine and heart readings; no realm
spans more than two floors, so a row for them would match nothing, and a
pattern nothing selects is dead data, not inventory — the rule that removed
the landing hall above.

**The pass.** After growth, in realm creation order (a parent precedes its
children, so an outer pattern claims before an inner one):

1. Compute the realm's `span` from its two paths' levels and its `Side`s
   from `class`; compute `Near`/`Far` from the endpoints' `depth`.
2. Filter `CYCLE_PATTERNS` to rows admitting `(class, span)` **and** whose
   every `ReqKind` is admissible for this descent by §3.3. Draw **one**
   index from the new leg `underworld/gate/v1/pattern` (keyed by vertex like
   the four plan legs), uniform over the admissible rows in table order.
   The draw is made even when one row or none is admissible, so the draw
   count is data-independent: `dof` gains exactly `realms`.
3. Resolve each `GateSpec` and the `KeySpec` to an edge and a node. Skip
   with `NoRoom` if a side has no interior node where the key wants one;
   with `Claimed` if the edge already carries a gate or the node a key.
4. Stamp tentatively, run the solvability check (§3.4). If the default body
   can no longer reach the terminus or any key node, unstamp and skip with
   `Unsolvable`. Otherwise commit.

No draw decides anything after step 2. Steps 3–4 are deterministic reads of
the graph, which is what makes a skip a fact about the world's geometry
rather than about luck.

### 3.3 Substance from rock and work

A `ReqKind::Natural` or `ReqKind::Key` becomes a `Requirement` by one table,
read from inputs `plan_descent` already has (`kind`, `character`):

```
  on a PASSAGE, worked (DrowTier, or Made when The Plat writes it)  -> Key: a door, and its key
  on a PASSAGE, natural, Karst | Fracture                           -> Mode(Swim): a sump
  on a PASSAGE, natural, LavaTube                                   -> inadmissible
  on a STAIR (the asymmetric way), any rock                         -> Mode(Fly): a chute
```

The reasons are the Crosscut's own (decision 0568): karst and fracture
systems are cut by water and a passage below the water line is a sump; a
lava tube is one dry conduit, so its loops stay open; a chute is a hole in a
floor, which every rock has. A door is a made thing — in a wild cave there
is nobody to have hung it. `worked` is the same term 0568's budget uses, so
the two derivations cannot disagree about which places are worked.

A `Key`-row is inadmissible in a natural descent and a `Natural`-passage row
is inadmissible in a lava tube; both are filtered in step 2, before the
draw, and counted as `Inadmissible` only when the drawn set was empty.

### 3.4 Solvability: one checker, three jobs

`solvable(plan, body) -> Reach` walks the product graph `(node, keys held)`
from `(entrance, ∅)`: a way `Open` is always taken; `Needs(Mode(m))` is
taken iff `body` carries `m`; `Needs(Key(n))` is taken iff `n` is in the
held set; arriving at a node with `key: Some(_)` adds it. The held set is a
bitset over key nodes, so the state space is `nodes × 2^keys`, and a descent
carries at most a handful of keys. `Reach` reports whether the terminus and
every key node were reached, and the shortest gated distance to each.

The **default body** is `{Walk, Wade}` holding nothing — the intruder. The
pass calls `solvable` with it after every tentative stamp (§3.2 step 4). The
guard test (§4.3) asserts it on the panel and on a 400-seed sweep. The
readout (§4.2) reads its distances. **The resident** is the body holding
every key with every capability; for it, every standable cell of the
descent is reachable, which is the Crosscut's whole-descent connectivity
restated for a world that now has gates, and is asserted separately.

Decision 0516 — "a reachable lock implies a reachable key" is left
*empirical* for the strongbox — is not reopened and not touched. That record
governs a building interior whose key placement is a prop-management knob
awaiting residents. A descent gate is a different object: the plan places
both its ends, so its rule is structural, and 0516's own text scopes itself
to the strongbox clause (ledger #5).

### 3.5 Realization: a place, a run, a hole — and the door is not a cell

**A gate is four things co-located, and the realizer keeps them apart**
(G3 ruling, ledger #7): the *requirement* lives on the plan; the *place* is
a cell kind; the *object* is a Thing, carried on the wire as a mark; the
*judgment* is the walk's. The building lattice already draws this line —
`CellKind::Threshold` is a designed opening and "a window is an ANCHOR at a
wall cell, never `CellKind::Window`" (`windows/vessel/src/lattice/mod.rs:
131-137`) — and the first draft of this section crossed it with a `Door`
cell kind. `LevelCellKind` gains three variants, each a place or a
substrate, none an object:

```
  Threshold  the one cell where a passage breaches the wall between two regions —
             EVERY passage has one, gated or not. In a cave it is a squeeze; in a
             building a doorway. movement_mode -> Walk. Passable unless a shut door
             Thing is anchored here (§3.7)
  Deep       standing water too deep to wade, on a passage's RUN (its corridor
             cells); movement_mode -> Swim (Flooded's sibling)
  Drop       the lip of a chute — the vertical threshold; movement_mode -> Walk
             (you may stand at the edge); `down` takes it, `up` from beneath it
             needs Fly
```

- **A threshold** is the one corridor cell that lies on the divider line
  between the two regions' rectangles (`region_rect` keeps a one-cell wall on
  every side, `circuit.rs:131-136`; `connect_cells` carves an L that crosses
  that line once). That the crossing is unique is asserted by a test, not
  assumed. It is stamped for every passage, so a cave map shows where its
  chambers narrow, and it is what a door Thing is anchored at. The client
  draws a threshold as a **squeeze, not a doorway**: the `+` of a built
  doorway is the door's, and the door is a mark (§3.7).
- **A sump** turns into `Deep` every cell of the L-corridor `connect_cells`
  carves for that edge that was **rock before the carve** — the corridor
  proper, never the two walkable endpoints inside the regions or any floor
  the L happens to run along. A `Deep` cell was rock a moment earlier, so no
  walker's connectivity within a region changes; only the way between the
  two regions does.
- **A chute** writes `Drop` at the stair coordinate on level `ℓ` in place of
  `StairsDown`, and on `ℓ+1` writes **no** `StairsUp`: the landing is made
  standable `Floor` and its region reconnected exactly as `place_stair` does
  for a stair foot. The stairs-pairing test is **amended, not deleted**:
  every `StairsDown` below the last rung still pairs with a `StairsUp` at the
  same coordinate and vice versa; every `Drop` pairs with a standable
  non-stair cell at the same coordinate one rung down; no `StairsUp` ever sits
  under a `Drop`.
- **Repairs respect gates.** `shortest_route_within_rect` never overwrites a
  `Threshold`, `Deep` or `Drop` cell, as it never overwrites a stair today
  (`mod.rs:485-490`); `standable_cells_in_rect` counts `Threshold` and `Drop`
  as footing. A repair that carved around a door would defeat it, which is the
  same defect the Crosscut found in a repair that carved through a wall.
- **`unlinked_neighbours_keep_their_wall` is unchanged.** A gate is placed on
  an edge the plan has; it is never a second way through, and the test that
  flood-fills between unlinked neighbours must stay green without edits.
- **The wire.** `vessel/level/v1`'s palette gains kinds `"threshold"`,
  `"deep"`, `"drop"` beside the five it has (`level_doc.rs::entry_for`); a
  door travels in the document's existing `marks` (a `PlanMark` at a cell,
  `level_doc.rs:241`), never as a palette kind. Additive: the palette is a
  sparse list of `(kind, state)` pairs and the client draws an unrecognised
  kind as rock (`clients/game/core/src/level.rs:150-185`), so an old client
  shows a wall where a squeeze is and nothing breaks. The client is taught
  the three glyphs and the door mark as a task in the `clients` lane set, and
  the committed seed-42 fixtures regenerate with the branch table of §5.
  Because `Threshold` is stamped on EVERY passage, the fixtures WILL move if a
  snapshot holds an underworld level; the branch table's middle arm is the
  expected one, not the first.
- **The realization witness** (decision 0577's word; the lattice's
  `doorways` ↔ `Threshold(a, b)` correspondence is its precedent): a test
  asserts, for every plan, that each `Passage` edge realizes exactly one
  `Threshold` cell on its divider; each `Needs(Key(_))` gate has exactly one
  door Thing anchored at that threshold and each key node one key Thing;
  each `Needs(Mode(Swim))` gate has a `Deep` run and each `Needs(Mode(Fly))`
  stair a `Drop`; and nothing of the kind exists that no gate asked for — in
  both directions. The walk never reads the plan's requirement (§3.6); it
  reads the realization, so solvability proved on the plan is a proof about
  the walked level only while this witness is green.

### 3.6 The walk: a body, two seams, three refusals

- **A body has a locomotion.** `hornvale_species` gains a sparse
  `locomotion_registry() -> ComponentStore<KindId, Locomotion>` on the exact
  shape of `habitat_realm_registry` — rows only for kinds that are not the
  default — with `Locomotion { swim: bool, fly: bool }`: `Swim` for the six
  aquatic kinds named in §1, `Fly` for the three dragons. `Body` gains
  `locomotion`, threaded at derivation the way `thermal_strategy` is. This
  is the component-layer home decision 0576's provision table assigns a
  `KindId`-keyed build-state capability; it is never saved (ledger #4).
- **Two seams, not one widened.** `movement_mode(kind)` keeps its signature
  and answers "how does this cell want to be crossed" — it now returns
  `Swim` for `Deep`, so The Gallery's reserved variant is reached. Beside it,
  one actor-aware seam answers "may *this* body cross *from here to there*":
  it composes the mode against the body's `locomotion`, and for a `Door`
  for a `Threshold` asks whether a door Thing is anchored there and, if so,
  its openness fold (§3.7). `Underground::peek` calls it where it calls
  `movement_mode(..).is_some()` today (`underground.rs:427-451`); the corner
  rule keeps asking `movement_mode` alone — a threshold is an opening in the
  wall whether a door in it is shut or not, so a diagonal past one is not a
  two-walled corner.
- **Three refusals**, named constants beside `UNDERGROUND_ROCK_REFUSAL`:
  deep water for a body that cannot swim; a locked door; no way up beneath a
  chute for a body that cannot fly. `StepOutcome::Blocked` stays unused —
  refusal happens at `peek`, where every refusal happens today.
- **The drop's verbs are the stairs' verbs.** `down` on a `Drop` lands on the
  same coordinate one rung below, by the same coordinate pairing `peek_stairs`
  uses (decision 0567); `up` on a cell whose twin one rung above is a `Drop`
  is refused unless the body flies, and then lands on the lip. No new verb;
  the asymmetry is entirely in what `up` will do.
- **Narration reads the mode.** The one `match` on `MovementMode`
  (`session.rs:5041-5045`) gains `Swim => "swim"`; a `down` through a chute
  and an `up` by flight each get a sentence. The `_` arm that would have
  swallowed `Swim` silently is what this bullet exists to name.

### 3.7 Doors and keys as Things

- **A `door` kind** joins `THING_KINDS` with `AffordsPassage`, `Openable`,
  `Lockable` — the cave-mouth's properties plus the strongbox's lock. It is
  the cave-mouth's sibling one band down (decision 0396: a passage is a
  thing and openness is its fold), not a new class. The `key` kind is
  reused as-is.
- **Identity is the plan's position**, on the `thing@passage/<addr>/<kind>`
  precedent (`passage.rs:123`): a key is
  `thing@descent/<vertex>/<level>/<col>.<row>/key` — the node that holds it;
  a door is `thing@descent/<vertex>/<level>/<col>.<row>-<col>.<row>/door` —
  its two nodes, lesser cell first. Ordinal 0; a node holds at most one key
  and an edge at most one gate by construction (§3.2 step 3). Decision 0069
  is obeyed in the letter — no fine position of any *agent* is serialized —
  and §5 says exactly what this identity costs.
- **The binding is structural, never a table and never a fact.** A door
  `Needs(Key(n))`; `open door` succeeds iff the body's custody (`held_by`)
  holds the key whose identity derives from `n`. The lock names no kind and
  the key names no lock — the M+N rule the Chattel's `Lockable` doc states —
  and the strongbox's `ObjectProperty::Portable` literal is never consulted
  for a descent door, so a coin will not open one. The strongbox keeps its
  own hazard and its own follow-up (ledger #5).
- **State is the Chattel's.** A door with no fact is shut and locked
  (`container_is_locked`'s default). `open` posts `lockedness` then
  `openness`, in that order, as it does for a strongbox; `close` shuts it
  without re-locking (decision 0399); passability of the `Door` cell is
  `is_open` at the session's day. A latent key (no fact) lies at its node;
  `take` posts custody; `drop` underground posts `located-in` with the
  possession's current plan node as the place, so `look` can list it — the
  region, never the cell, is the committed grain.
- **A door is anchored at a threshold and is the only thing that makes one
  a doorway.** The Thing's place is the threshold cell of its edge; it goes
  on the wire as a mark at that cell. A threshold with no door is a squeeze
  and needs no Thing — `examine` of a bare squeeze is a captured follow-up,
  not a promoted kind. The anchor rule is deliberately no tighter than the
  building lattice's (an anchor may sit at a wall cell), so a door onto rock
  — the tomb's false door — is admissible later without a new class
  (`MAP-false-door`); this campaign places none.
- **The verbs reach underground.** `look` on a cell whose region holds a
  key, latent or dropped, says so; on a cell adjacent to a threshold with a
  door says so and names the bearing; `examine`, `take`, `open`, `close` resolve those nouns.
  `carrying` already works. No new verb: `unlock` does not exist and is not
  added.

### 3.8 What holds by construction, stated so it can be checked

Each pinned by a unit test or a sweep, not measured:

- **Every gate lies on an edge the plan had; every key lies at a node the
  plan had.** The pass adds nothing to the graph.
- **A node holds at most one key and an edge at most one gate.** `Claimed`
  skips guarantee it.
- **Every plan is solvable for the default body**: terminus and every key
  reachable from the entrance in the product graph (§3.4), by the
  `Unsolvable` skip, asserted over the Crosscut's 3 kinds × 3 characters × 4
  vertices × 400 seeds.
- **Every standable cell is reachable by the resident**, whole-descent, so
  gates cost the resident nothing.
- **A drop has a floor under it and no stair under it**; a stair still pairs
  with a stair.
- **Every passage has exactly one threshold cell**, on the divider line;
  every gate realizes exactly what its requirement names and nothing more
  (the realization witness, §3.5).
- **The Crosscut's four properties and its dof identity survive**, the
  identity extended by exactly `+ realms`:
  `dof = 1 + levels + stairs + 4·(realms − fallback_realms) + 2·extensions +
  failed_draws + realms`.

What the pass **cannot** do, deliberately: gate an edge outside a realm (the
spine between cycles), because a gate on a bridge is unsolvable for the
default body by definition; place two keys for one lock, or one key for two;
make a same-floor passage one-way (no natural substance exists for it — a
scree slope is a captured follow-up with the true valve).

**And the assumption the whole model rests on, named so it can be held to
(G3, ledger #8):** a requirement is a predicate on the *traverser's own state
at the threshold* — what it holds, what it is, what it knows — and on
nothing else. That is what makes solvability a proof over `(node, keys)`. A
door that reads the world's clock (moonrise, a tide), another body (a
pressure plate for two), the path taken (a sequence), or whose far end
depends on state (a portal) is not a gate in this model; the first is
captured (`MAP-world-conditional-gate`), the last is the Crosscut's own wall
(an edge whose endpoint is a function is not series-parallel), and the middle
two are limits stated rather than ideas kept.

## 4. Preregistration

Frozen here, before the code, over the standing panel (seeds 42, 7, 1234),
every cave-bearing vertex, in the readout `hornvale circuit --seed <N>`
already writes to `docs/audits/underworld-circuit-seed-panel.md`. The page
gains a section per readout below; the Crosscut's four sections are
unchanged, and their numbers **may not move because of the pass** — the pass
adds no node, edge, realm or stair, and the four are functions of those alone.
**Execution amendment (Task 1, ruling C):** they DO move once, by the
Crosscut's own deferred minor taken in this campaign — `try_extend` now tests
its capability invariant against the post-extend passage set — and the
attribution is by revert: with that one change reverted the panel is
byte-identical to the baseline; with the gate pass and the class recompute
alone it is byte-identical too. Loop share 0.1233 → 0.1077, membership
0.8548 → 0.8442, cross-floor 841 → 839 of 874, overlap 0.3061 → 0.3030 on
seed 42; every verdict word unchanged. Taken now rather than deferred because
decision 0618 makes every later plan-grammar change an epoch, so this is the
last campaign that can take it for free. Each readout says what it measures
*to* and *from*, the lesson of the Crosscut's loop-share.

### 4.1 Gate yield

**From** the realms whose draw selected an admissible row **to** the realms
whose row was applied in full (no `Claimed`, `NoRoom` or `Unsolvable`
skip), per descent; the panel median. Rows that stamp nothing count as
applied; realms with an empty admissible set are excluded from both sides
and reported beside as `Inadmissible`. Prediction: **median yield ≥ 0.70**.
Below is FALSIFIED and is the headline: it would mean the grammar's
geometry — path lengths, nesting, stair placement — leaves most patterns no
room, which no amount of table-tuning should hide. Skip reasons are
histogrammed beside it, report-only.

### 4.2 Detour cost

**From** the default body's shortest round trip entrance → terminus →
entrance through the product graph (§3.4) **to** the same round trip on the
ungated graph; the ratio, per descent, over descents holding at least one
realized requirement (a door, a sump or a chute); the panel median.
Prediction: **median ≥ 1.10**. A round trip, not a descent, because a chute
costs nothing on the way down and everything on the way back — a one-way
measure would read every chute as free. Below 1.10 is FALSIFIED: the gates
sit where the intruder never needed to go.

### 4.3 Solvability — a guard, not a prediction

100% of panel descents solvable for the default body: terminus and every
key node reached. Asserted, and asserted again on the 400-seed sweep. A
single failure is a red test, never a number on the page.

### 4.4 Report only

The pattern histogram by `LengthClass` and span; counts of doors, sumps and
chutes per descent; the share of worked descents carrying at least one door
(with the disclosure that the production walk reaches none of them yet,
§1); the share of descents whose default-body return path differs from its
outbound path in edge set — Dormans' "unknown return path", expected high
wherever a chute lands and not predicted, because it follows from the chute
by construction.

### 4.5 Determinism and fidelity

Two independent builds of every plan and level on the panel are
byte-identical. The dof identity of §3.8 holds as an equality on every plan.
The Crosscut's four readouts do not move by a digit (§4 preamble). New leg:
`underworld/gate/v1/pattern`, published through `stream_labels()` and added
to `cli/src/streams.rs`'s stamp roster; the four `underworld/plan/v1` legs
are unchanged in label and in consumption order, and the plan a Crosscut
binary would build is the subgraph this one builds.

## 5. Save-format and determinism consequences

**One, by design, and the design says what it is.** Every plan, gate and
level stays `FRAME`-tier under decision 0069 — derived on entry, discarded on
exit. But a descent key's *identity* is a function of a plan node (§3.7),
and taking it posts a `located-in` custody fact whose **subject** is that
identity. From the first world saved holding a descent key, the plan grammar
is a save-format contract: a later change to `underworld/plan/v1`'s draws or
to `underworld/gate/v1/pattern`'s selection would leave that world naming a
key that no longer exists. The Crosscut's brief said "any new draw changes
plan bytes — allowed, no world reads them"; after this campaign a world
does, and the sentence stops being true. Consequences, stated so they can be
held to:

- A change to the plan or gate grammar after this campaign is an **epoch**
  — `underworld/plan/v2` or `underworld/gate/v2`, never a silent edit — and
  is no longer the *empty* epoch The Drift's amendment A.6 refused. Decision
  0618 records it.
- The alternative — descent custody as session-only state, lost on save —
  was considered and refused: decision 0400 makes custody an observable on
  the session snapshot, and two custody rules for one `key` kind would be
  the verb×object table the Chattel exists to avoid.
- An orphaned custody fact is harmless in the way a strongbox key's is: the
  thing it names has no location and appears nowhere.

Everything else is additive: one new stream leg; three palette kinds on a
wire document whose reader draws an unknown kind as rock; one thing kind;
no new predicate (`instance-of`, `located-in`, `openness`, `lockedness` are
The Chattel's); no existing stream's consumption order changes.

Task 0 re-runs the Crosscut's epoch grep with its branch table, and adds:

```
  regenerate clients/game/core/tests/fixtures/ after Task 3
    no fixture differs                -> nothing to do (the seed-42 snapshots
                                         hold no gated cell)
    a fixture differs by palette rows -> REBASELINE=1, diff reviewed, in the
                                         same commit as the kinds
    a fixture differs otherwise       -> STOP; something moved that the pass
                                         must not move
  regenerate docs/audits/underworld-circuit-seed-panel.md after Task 1
    only the new sections and dof differ -> proceed
    a Crosscut number moved              -> STOP; the pass touched the graph
```

## 6. Non-goals

Realizing danger (The Plat's hoarder), secrecy (a `Door` or passage hidden
until found — needs the fog/render seam; `MAP-secret-door-rendering`), a
collapsing gate (`MAP-collapsing-gate`), a true one-way passage on one floor
(`MAP-true-valve`); a requirement of the KNOWN kind — a word, a face — on
the model of 0397's knowledge gate (`MAP-knowledge-key`); a shut door
blocking sight (`MAP-doors-occlude`); a `lock` or `unlock` verb; the strongbox's `Portable`
literal (`PLAY-strongbox-lock-wants-an-unlocks-property`); wiring the walk
to the lattice or reading `character_of` in `Underground::enter`
(`MAP-walk-ignores-the-lattice`, untouched, ledger #3); the carve streams'
missing vertex (`MAP-descent-carves-are-per-world`, untouched); two keys
for one lock; a gate on a spine edge; residents, `ChamberOverrides`, the
intimacy reading (The Plat); junctions, vaults, viewport, dressing prose
(the Crosscut's §6, inherited).

## 7. Acceptance

1. **A locked door, walked.** A session test on a worked descent drives the
   verbs: refused at a threshold whose door is shut, with the locked-door
   refusal; walks the long
   side; `look` names the key; `take key`; returns; `open door`; passes.
   The door stays open on the way back.
2. **A chute, walked.** On a wild descent: `down` on a `Drop` lands one rung
   below at the same coordinate; `up` there is refused; the possession walks
   the realm's lower path to its stairway at the far end and `up` returns to
   the upper floor, then walks the upper path back to the lip. The loop is
   closed by the session's own verbs.
3. **Capability keys, walked.** A body whose species swims crosses a sump
   the default body was refused at, narrated "swim"; a body whose species
   flies goes `up` beneath a chute. Both through `!possess` of a roster body
   or a test-constructed body — a walk, per decision 0398, not a registry
   row.
4. **The readouts** of §4.1–4.4 are on the committed audit page with their
   verdicts in the frozen words; the Crosscut's four did not move.
5. **The existing wall and carve tests pass unmodified**;
   `stairs_pair_by_coordinate_across_adjacent_rungs` is amended as §3.5 says
   and no other stairs assertion is weakened.

## 8. Task shape (detail belongs to the plan)

0. Preflight: epoch grep and branch tables (§5); the inventory count frozen
   after Nathan's G3 confirmation of the Fig. 9.8 names; the client fixture
   branch table.
1. `worldgen`: `Gate`/`Way`/`Requirement`/`Capability` types on the plan; the
   `CYCLE_PATTERNS` table; the stamping pass with its four skip reasons; the
   solvability checker; the `pattern` leg; the dof identity extended;
   construction-property tests and the 400-seed sweep.
2. `worldgen` + `cli`: the four readouts in `circuit_readout.rs`; the audit
   page regenerated with its diff read against §4's preamble.
3. `vessel` realizer: three cell kinds; threshold, sump and chute
   placement; repair exclusions; the pairing test amended; the realization
   witness; the wire palette and the door mark; the client's three glyphs
   and the mark (`clients` lane set).
4. `species` + `vessel` walk: `locomotion_registry`, `Body.locomotion`; the
   actor-aware seam beside `movement_mode`; three refusals; `down`/`up`
   through a chute; narration.
5. `thing` + `vessel` things: the `door` kind; descent thing identities;
   `look`/`examine`/`take`/`open`/`close`/`drop` reaching underground.
6. Acceptance walks (§7.1–7.3) as session tests, including a swimming and a
   flying body.
7. Book, decisions 0616–0620, registry sweep, retrospective, close.

Stage gates after 2, 4 and 6.

## 9. Decisions this campaign expects to mint

- **0616** — A gate is a requirement on a way, stamped on the finished plan
  after growth, and realized as four co-located parts kept apart: a
  requirement on the plan, a place in the rock, an object in the ledger, a
  judgment in the walk; valve and asymmetric are one gate seen by two bodies
  (0347 extended to traversal); a door is an object at a threshold, never a
  cell kind.
- **0617** — A lock's substance is derived from rock and work: a door needs a
  maker, a sump needs karst or fracture, a chute needs a floor below; the
  pattern chooses where, never what.
- **0618** — A descent key's identity is a plan position, and from the first
  saved custody fact the plan grammar is a save-format contract; a later
  grammar change is a non-empty epoch.
- **0619** — The drop is a stairway with its up half omitted, and `Fly` is
  its key; `down` and `up` are its verbs.
- **0620** — The cycle-pattern inventory is a frozen corpus with an asserted
  count, applied one row per realm, with solvability for a body holding
  nothing as the invariant every placement must preserve.

## 10. Provenance

Dormans, *Cyclic Generation*, in Short & Adams (eds.), *Procedural
Generation in Game Design* (2017), ch. 9, pp. 83–95: the fourteen cycle
patterns of Fig. 9.8 (names transcribed here, to be confirmed against the
figure); the lock-and-key attribute aside (conditional / dangerous /
uncertain; permanent / collapsing; valve / asymmetric; safe / unsafe key);
the key-safety rule. Alexander, *A Pattern Language*, 133 (staircase as a
stage) for the landing hall. The project's own prior: the Crosscut's ledger
organon (the length-class × floor-span grid whose PREDICTED cells are rows
8–10), decisions 0347, 0396, 0398, 0399, 0400, 0516, 0566–0568, 0576; The
Gallery's reserved `Swim`/`Fly`; The Chattel's custody machinery; The Latch's
cave-mouth barrier; `MAP-drop-is-a-cross-floor-valve`,
`MAP-underworld-traversal-grammar`, `PLAY-key-placement-stands-in-for-a-
resident`. Brainstorm and ideonomy record:
`docs/superpowers/ledgers/2026-09-02-the-brattice.md`.
