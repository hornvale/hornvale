# The Plat — the underworld gets a reading someone lives by

*Design spec. The Circuit, campaign 3 — the last of the program
([metaplan](2026-09-01-the-circuit-metaplan.md) §3). Predecessors: The
Crosscut (the graph, decisions 0566–0568) and The Brattice (the gates,
0616–0620). Drafted 2026-09-03 under autopilot; Nathan reviews at G3. Ledger:
`docs/superpowers/ledgers/2026-09-03-the-plat.md`.*

## 1. What occasioned it

The Circuit's keystone is that one skeleton carries two readings — the
intruder's puzzle and the residents' circulation. Two campaigns built the
skeleton and the first reading. The Crosscut grew a series-parallel plan per
descent and exported, on every node, a `depth` (hops from the entrance) and a
`realm` (the innermost cycle holding it), and on every realm a `parent`; its
decision 0566 says in as many words that "The Plat reads depth and realm as
Alexander's intimacy gradient and circulation realm." The Brattice stamped
gates onto that plan and read `depth` once — to order a realm's two endpoints
as Near and Far for the intruder — and left the rest where it found it. As of
`f40b32ff1`, `Node.depth` has three readers, all inside `windows/worldgen`,
and none of them is a person.

Three facts about the shipped code set this campaign's scope, each measured
before the design below was written (ledger #2; the instrument is
`windows/worldgen/tests/suite/plat_reading_probe.rs`, committed with the
ledger):

- **A Made chamber exists in the rule and in no world a player can reach.**
  `ChamberOrigin::Made` has a writer — `delve_seating::made_chambers`, whose
  own doc header is "NOTHING IN THE SHIPPED PATH CALLS THIS, AND THAT IS THE
  WHOLE DISCLOSURE" — and every reader passes it `ChamberOverrides::new()`.
  `Underground::enter` hardcodes `ChamberOrigin::Found` for every rung and
  `Character::WildCave` for every descent, so `worked` is false everywhere the
  walk goes and no walked descent can hang a door: the Brattice's own panel
  says 872 of 874 worked descents carry one and the production walk reaches
  none. The worlds do carry the material: on the panel's seeds, 26, 3 and 5
  columns hold a settled underworld people, every one seated at the top or
  second rung (`underworld_capacity_probe`, 2026-09-03), so one descent
  reaches every Made rung the world has.
- **Something already lives in every rung that can feed it, and nothing sites
  it.** The Gallery's Task 11 derives a rung's dominant inhabitant from its
  own substrate and energy (`dominant_inhabitant`, `chamber_resident`) and
  draws it as a species mark at "the last standable cell in ascending `(x, y)`
  order" — a geometric rule chosen so the creature does not greet the
  possession at the stairs. WHO and WHETHER are the energy field's; WHERE is
  an accident of the grid.
- **Every level has a hub, and most have several.** Over 1,200 plans (200
  seeds × 3 kinds × 2 characters, vertex 2), every level has a node of degree
  ≥ 3 (100%) and the typical level has three to six. "The heart is the hub"
  underdetermines the heart. Alexander's own criterion for 129 is a different
  one — "the center of gravity of all the spaces the group occupies" — and it
  has an exact graph reading. The same probe found that a level's within-level
  passage graph is usually **disconnected** — only 23% of karst levels and 33%
  of fracture levels are one piece within themselves; 39% of karst levels are
  three — its islands joined only through the stairs of the level above, so
  any center-of-gravity must be measured under the plan's own metric, through
  stairs, not within a level. A Crosscut structural fact nobody had printed:
  "you can go around in circles", but on one floor you often cannot reach
  every room without going up one.

Two constraints frame everything else. Decision 0618 (The Brattice) makes the
plan grammar a save-format contract: a descent key's identity is its plan
position, so any change to `underworld/plan/v1`'s draws or
`underworld/gate/v1/pattern`'s selection is an epoch. And The Roll's spec
(§7) refuses to move a key onto a body before a transfer verb exists, because
a held key with no way to unhold it makes the only lock in the game
unopenable — which rules out the one thing a hoarder-as-body would have
bought.

## 2. Keystone

> **Use follows form.** A people does not design the graph — the rock and the
> draws did — it moves in and reads it: the shallowest room is the door, the
> room every path passes near is the hall, the deepest is theirs alone. The
> reading is a function of the plan and changes no byte of it; what it names
> is narrated where a people cut the place, and sites the creature everywhere.

Four commitments follow:

1. **The reading is derived, never drawn, and never stamped.** It is a pure
   function over a finished `DescentPlan`. It adds no stream, consumes no
   draw, and rewrites no edge, so decision 0618's epoch is never triggered
   and the Crosscut's four numbers and the Brattice's four cannot move.
2. **Made comes from the ledger at the walk, never from a commit.** A column's
   chamber origins are derived from `occupations_at` and `seat_at` when the
   possession enters, exactly as `made_chambers`'s doc priced it ("~30 lines,
   cheap enough for one verb"), and no dig fact is committed. A cut hall
   outlives its people: `Made` reads the ledger's occupation whether or not
   it is alive, and whether it is alive decides only the tense.
3. **Workmanship is per level and joins the plan as the clause 0568 reserved.**
   `worked(level) = worked(character) || origin(level) == Made`. For every
   input reachable today (all-Found origins) the plan is byte-identical to
   the plan before this campaign; the Made population is new, not moved.
4. **The hoarder is where the plan is innermost, and WHO it is stays the
   energy field's.** The frontier's condition — a creature that "falls out of
   the world's own energy budget; nobody sited it" — is met when WHO and
   WHETHER are unchanged and WHERE is a structural derivation. It sits on
   what lies there. It holds nothing (Roll §7). Decision 0070 governs how an
   encounter would be resolved and says nothing about tenancy; nothing here
   resolves one.

## 3. The design

### 3.1 The reading: one `Role` per node, a `landing` on some

`windows/worldgen/src/plat.rs`, a pure module over `circuit::DescentPlan`:

```
  Role     = Entry | Heart | Sanctum | Chamber        exactly one Entry, one
                                                      Sanctum, at most one
                                                      Heart per level
  Reading  { roles: Vec<Role>,      // parallel to plan.nodes
             landing: Vec<bool>,    // parallel to plan.nodes
             rank: Vec<u8> }        // realm nesting depth of node.realm
  read(plan: &DescentPlan) -> Reading
```

Per level `ℓ`, over the nodes `nodes_on(ℓ)`:

- **Entry** (Alexander 112, the transition; 127, the public end) — the node of
  least `depth`; ties to the lower `NodeId`. This is the node the possession
  arrives at: the plan's `entrance` on level 0, and on every deeper level the
  foot of the stair the spine came down.
- **Sanctum** (127, the private end) — the node of greatest `depth`; ties to
  the lower id. Never the Entry: the probe measured the depth range on a
  level at ≥ 2 on 100% of 6,000 levels.
- **Heart** (129) — among the level's nodes **excluding the Entry**, the graph
  median: the node minimizing the sum of shortest-path distances to every
  other node on the level, where paths run through the whole plan (stairs
  included), because a level's within-level graph is usually disconnected.
  Ties: higher degree, then lower depth, then lower id (unique on 71–79% of
  levels; a two-way tie on ~20%). The Entry is excluded by rule, not
  prediction — Alexander 112 says there is always a transition between the
  outside and the heart — and the probe measured that without the rule the
  median lands on the arrival node on 22–29% of levels. `Heart` is `None`
  where the median IS the Sanctum (a level whose center of gravity is its
  innermost room has no separate hall — 0.0–0.1% of levels, 6.9% of the
  small wild lava-tube levels) or where no candidate remains.
- **Chamber** — every other node. Its intimacy is its `depth`, which is
  already exported; the reading adds no second number.
- **landing** (133, the staircase as a stage) — true on a node that is the
  lower end of one stair and the upper end of another: the possession arrives
  on it from above and leaves it downward. Measured at 14–45% of levels
  depending on rock. The Brattice removed `the-landing-hall` from its
  inventory because the *realm class* it wanted never occurs; the landing is
  a property of a node, not a realm, and it occurs.
- **rank** (98, circulation realms) — the nesting depth of `node.realm`
  through `Realm.parent` (0 for a spine node on no cycle). Exported for the
  readout of §4.3 and for The Precincts; narrated by nothing here.

The reading is computed for **every** plan — wild or worked, Found or Made —
because the hoarder reads it everywhere (§3.5). Only its **vocabulary** is
keyed to Made (§3.4). It is a function of the plan alone: no seed, vertex,
terrain or ledger reaches it, which is what lets The Precincts lift it over a
district graph (metaplan §3: "may share the scaffold").

**What holds by construction:** one Entry and one Sanctum per non-empty level,
never the same node; at most one Heart per level, and never the Entry or the
Sanctum (a level whose only candidate is its Sanctum reads Entry/Sanctum
only); `landing`
implies a `Stair` edge in both directions; `rank` is 0 exactly where
`node.realm` is `None`.

### 3.2 The writer: `Made` from the ledger, per column, at the walk

`windows/worldgen/src/delve_seating.rs` gains the per-column twin of
`made_chambers`:

```
  Tenancy  = Wild | Inhabited | Abandoned
  column_origins(world: &World, terrain: &GeneratedTerrain, vertex: Vertex,
                 rungs: &[Band]) -> Vec<(ChamberOrigin, Tenancy)>   // per rung
```

For each occupation `occupations_at(world, vertex)` returns whose people has
an environment niche (today exactly one people does — `drow` is the only row
of `environment_niche_registry()`, grepped 2026-09-03 — so it is the only
people that can seat underground; a people without a niche is a surface
people by `Seating::all_surface`), take `seat_at(niche, cave, gradient, water_table)` —
the same call and the same three inputs `made_chambers` and the capacity probe
already make — and mark the seated rung `Made`. Tenancy is `Inhabited` if any
occupation seating that rung `is_alive()`, else `Abandoned`. Every other rung
is `(Found, Wild)`. A column with no cave, or no occupation, returns all
`(Found, Wild)`, which is today's answer.

**One writer, two grains.** `made_chambers` (bake-side, over a `History`)
stays as the aggregate the capacity probe reads; `column_origins` is the
ledger-side read a verb can afford. They are pinned to agree: on seed 42, the
set of `(vertex, rung)` `made_chambers` marks at `branch 0, level 0` equals
the set `column_origins` marks `Made` over the same vertices. A second writer
that could drift is the duplicated-pair shape `promote_role`'s doc records as
a live seam once already; the agreement test is what keeps this pair one.

**Branch 0 is inherited, not minted.** The walk has no branch
(`MAP-walk-ignores-the-lattice`); `cave_entrance_addr` already fixes the
walk's address at `branch: 0, level: 0` for The Latch's barrier, and
`made_chambers` marks every branch of a seated rung identically, so the
choice of branch changes nothing about which rung is Made. The debt is
narrowed — the walk now reads the ledger's origin — and not closed: it still
reads no run, junction or per-branch character (§6).

### 3.3 Workmanship joins the plan, per level

`plan_descent` gains `origins: &[ChamberOrigin]` parallel to `rungs`, and
two rules read it:

- `cycle_budget(kind, character, origin)` — the `worked` term is
  `worked(character) || origin == Made`, evaluated per level with that
  level's origin. Decision 0568's text already says "a drow tier or a made
  chamber adds one"; `circuit.rs:357` already says "`ChamberOrigin::Made`
  joins the `worked` term when The Plat gives it a production writer."
- `brattice::admissible` — a `ReqKind::Key` row is admissible for a realm iff
  the level its gate would sit on is worked. A door needs a maker where the
  door is; for the one cross-floor key row (`key-downstairs-lock-upstairs`)
  that is the upper level. `worked()` stays an exhaustive match over
  `Character` and gains the origin as a second argument, so a fourth
  character or a third origin still fails to compile rather than inheriting
  "unworked".

**The regression pin, stated as a test:** for every `(seed, vertex, kind,
character)` the Brattice's 400-seed sweep covers, `plan_descent` with
all-`Found` origins is byte-identical to the plan at `f40b32ff1`. The pin is
mechanical: the sweep already computes those plans and their dof identity;
the new arm feeds all-Found and compares a digest against a fixture taken
before Task 2 touches `circuit.rs`.

**Why this is not an epoch under 0618, stated so G3 can hold it.** 0618 makes
a change to the grammar's *draws or selection* an epoch because a saved
custody fact names a plan position. This campaign adds no draw and changes
no selection rule; it adds an *input* whose only reachable value before this
campaign was `Found` everywhere, and for that value the output is pinned
byte-identical. A plan at a column with a Made rung is a plan that never
existed for any saved world — it is a new population, not a moved one. G3
flags it anyway (§9), because "an input, not a grammar change" is a
distinction a future reader could argue with, and the argument is recorded
here where they will find it.

### 3.4 The walk: origins in, a place sentence out

`Session::delve_at` calls `column_origins` and hands `Underground::enter` the
per-rung origins and tenancies; `enter` stops hardcoding `Found`.
`enter_with_character` stays the test seam it is, gaining the same two
arguments. The realizer already reads `origins`: `choose_leaf_style` gives a
Made leaf a base worked chance of 0.85 against Found's 0.10 (blended half
and half with the inherited bias), and a worked leaf is drained — wetness is
per LEAF, keyed on `LeafStyle.worked`, never on the chamber-wide origin
(`generate_level_with_water`'s own doc; `is_sump`'s `Made` short-circuit
never reaches a leaf). So a Made rung reads as mostly cut and mostly dry, not
dry by fiat, and the level a possession stands in changes only where the
ledger says someone cut it.

`describe_underground_here` gains one sentence, **only in a rung whose origin
is Made**, chosen by the `Role` of the node whose region holds the
possession's cell (a cell on a divider — a `Threshold` — belongs to no region
and gets no sentence, the same rule `drop` already applies):

```
                 Inhabited                              Abandoned
  Entry      "This is the entry of a cut place;     "This was the entry of a cut
              the rock is squared where it was       place, long empty; the
              worked."                               squared rock has dulled."
  Heart      "This hall is the heart of the         "This hall was the heart of
              place; every way through it passes     a place; every way through
              near here."                            it still passes near here."
  Sanctum    "This is the innermost chamber of      "This was the innermost
              the place."                            chamber of a place, long
                                                     empty."
  Chamber    "This is a chamber of a cut place."    "This is a chamber of a cut
                                                     place, long empty."
  + landing  "A stair comes down into it and another leaves it." (either tense)
```

Appended, not woven, so a wild descent reads byte for byte as it did (the
Brattice's own rule for its floor sentence). Every noun the sentence names
answers `examine` (the Gallery's both-directions rule): `hall`, `chamber`,
`entry`, `stair` are added to `underground_nouns` with the same datum, in a
Made rung only.

Nothing changes on the wire. `vessel/level/v1` carries cells, palette, marks
and the rung; a role is prose, and decision 0022 leaves the pane's glyphs to
the client. No client task.

### 3.5 The hoarder: sited by the reading, fed by the field

`resident_cell(level, plan)` replaces "the last standable cell" with **the
first standable cell in the Sanctum's region** — the mirror of the entrance
rule (`enter` stands the possession on the first standable cell in the Entry's
region), now on a node the plan named rather than a corner the grid produced.
WHO and WHETHER are untouched: `dominant_inhabitant`'s roster filter, fit
threshold and tie-break are not this campaign's to move.

The mark's datum gains the hoard: the things whose `located-in` fact names
the Sanctum's region (`region_key`, the key `drop` already posts against),
listed the way `look` lists the floor — *"A xorn moves in the dark here,
drawn to iron-bearing stone, sitting on: a key."* — and unchanged when
nothing lies there. The hoard is a fold over existing facts; no new kind, no
new predicate, no identity. A key the Brattice placed at the Sanctum node is
in the hoard by construction (the latent key is a thing lying there); the
probe measured that at 13–23% of levels carrying a key, so the hoard is
usually what the possession itself left behind.

In a Made rung the same creature is narrated as *kept* rather than moving in
the dark — one word in the datum, keyed to tenancy — because a people that
cut the place did not leave its innermost chamber to whatever wandered in.
That is the whole of the hoarder's relation to residents in this campaign;
what a people would do about a xorn in its sanctum is an agent question
(§6).

### 3.6 The readouts

Two committed audit pages change. Both are regenerated by
`scripts/regenerate-artifacts.sh` and drift-checked through
`docs/generated-paths.txt`.

`hornvale circuit --seed <N>` (`docs/audits/underworld-circuit-seed-panel.md`)
gains a section **"The Made population"** per seed, built from the seed's
committed ledger rather than terrain alone, so the command builds to
`BuildDepth::Full` where it built to `Terrain`. Its existing eight sections
must be byte-identical before and after (no code path a wild plan takes
changes). The new section reports, over the seed's occupied underworld
columns:

- columns, and their seated rung (a histogram over the ladder);
- Made rungs carrying ≥ 1 door, FROM Made rungs (§4.2);
- the Heart's depth decile per Made level (§4.1) and the landing share;
- nested realms smaller than their parent, over ALL the seed's plans (§4.3);
- Sanctums whose region holds a thing at genesis (report only).

`hornvale underworld --seed <N>` (`underworld_readout`) is handed the real
override source its own doc reserved the line for (`underworld_readout.rs:616`,
"the one line Task 4 replaces"), so `made:` stops reading 0 by construction.
It too builds to `Full`.

### 3.7 What holds by construction, stated so it can be checked

- The reading is total and deterministic over any plan; `read` on two equal
  plans is equal; `read` reads no seed.
- All-Found origins reproduce today's plan byte for byte (§3.3's pin).
- A Made rung's leaves lean worked, and its worked leaves are drained
  (`made_chambers_lean_worked_found_chambers_lean_natural` and the per-leaf
  water rule, both unchanged).
- The realization witness (the Brattice's 200-seed × 3-engine sweep) is
  unchanged: the reading realizes no cell, so there is nothing new for it to
  witness, and it must pass unmodified.
- The hoarder stands in the Sanctum's region on every rung that has one, on a
  standable cell; a rung with no standable cell has no hoarder (today's rule).
- No committed fixture moves: the three session fixtures under
  `clients/game/core/tests/fixtures/` carry `band` walk, chamber and chamber
  (grepped 2026-09-03), none underground; the world golden holds no session;
  no thing kind is added (so no concept-registry accession, unlike the
  Brattice's ledger #13).

## 4. Preregistration

Frozen here, before the code (decision 0016). Every readout names the
population it measures FROM and TO (the Brattice's lesson: its gate yield read
1.0 on every seed because its denominator excluded the population that
mattered). Panel: seeds 42, 7 and 1234.

### 4.1 The heart sits shallow

FROM every Made level on the panel (one per occupied column: 26 + 3 + 5 = 34
at `d9749623b`), TO the Heart's depth decile — `(depth(Heart) −
depth(Entry)) · 10 / (depth(Sanctum) − depth(Entry))`, floor. **Prediction:
the decile is ≤ 5 on at least two thirds of Made levels.** Alexander's
gradient puts the common area between the door and the private rooms, not
at the far end. The probe's synthetic sample (vertex 2, 200 seeds, the plan
metric) put the Heart in deciles 0–5 on 72% of karst levels, 73% of
fracture and 76% of lava tube, with the mode at decile 5 in every rock — so
on a panel population of 34 the prediction can fall either side of two
thirds by sampling alone, which is what makes it a prediction rather than a
restatement of the rule. Falsified below two thirds: reported as a finding
about the panel's Made population, and the heart criterion is NOT retuned.

### 4.2 A Made rung usually hangs a door

FROM every Made rung on the panel, TO the share carrying at least one
`Needs(Key)` gate on that level. **Prediction: ≥ 0.5.** The Brattice's 872 of
874 is per *descent* over five worked levels; per single level the probe
measured a key on 58–72% of DrowTier levels, and a Made rung is one level
whose neighbours are wild, so the cross-floor key row rarely helps it.
Report-only beside it: the door share of the same rungs' plans BEFORE this
campaign (all-Found), which is 0 by construction and is printed so the
reader sees what moved from what.

### 4.3 Circulation realms shrink going in

FROM every nested realm on every plan the panel derives (wild and Made), TO
the share whose region area is smaller than its parent's. **Prediction: a
majority, and under two thirds.** Alexander 98 says realms become smaller as
one goes in; the grammar splices chains into both paths after a realm is cut,
so the effect should be present and weak. Synthetic sample: 55–59% (79% in
lava tube, which has almost no nesting).

### 4.4 Report only

Occupied columns per seed and their seated rungs; landings per Made level;
Sanctums holding a thing at genesis; the Made rung's tenancy split
(inhabited / abandoned) per seed; within-level components per level on the
panel's plans (the synthetic sample: 23% of karst levels are one piece, 39%
are three; lava tube 58% one piece — §1).

### 4.5 Determinism and fidelity

Byte-identical rebuilds; the Crosscut's and the Brattice's panel sections
byte-identical before and after; the all-Found regression pin; no stream
label added (`stream_labels()` and `cli/src/streams.rs` unchanged — a test
asserts the manifest did not move).

## 5. Save-format and determinism consequences

**None by design, and the design says why.** No draw is added; no selection
rule changes; the plan for every input reachable before this campaign is
pinned byte-identical (§3.3). The reading is `FRAME`-tier under 0069, derived
on entry and discarded on exit, and nothing it produces becomes a fact
subject. The hoarder's mark moves, and the mark has no identity to move (The
Gallery: "this task ships a query, not a placement engine with tracked
individuals"). The hoard is a fold over `located-in` facts that already
exist. No thing kind, so no accession epoch.

The one save-format-adjacent item, flagged for G3: a column with a settled
underworld people now derives a *different plan* from the one the walk would
have derived yesterday. No saved world can name a key under either plan at
such a column — the Made population was unreachable — so 0618's contract is
not touched; but a reader who takes "the walk's inputs changed at 26 columns
of seed 42" as a grammar change would be wrong for a reason that needs
stating, and it is stated in §3.3.

Task 0 branch tables:

```
  regenerate docs/audits/ after Task 2
    only the new Made section and underworld `made:` moved -> proceed
    a Crosscut or Brattice number moved                    -> STOP; a wild
                                                              plan changed
  regenerate clients/game/core/tests/fixtures/ after Task 3
    no fixture differs                                     -> expected (no
                                                              fixture holds an
                                                              underground band)
    any fixture differs                                    -> STOP; the walk
                                                              changed a band
                                                              it must not
  `cargo run -p hornvale -- streams` after Task 2
    identical                                              -> proceed
    a label added                                          -> STOP; a draw
                                                              was introduced
```

## 6. Non-goals

Residents standing in the Made rung (`MAP-chamber-occupancy`: a home kind
for `(vertex, rung)`, chamber seating into a level, the roll — the mechanism
campaign, and this spec's chief capture); the hoarder as a Body or a Thing
(§3.5; the scale in ledger #2); realizing `hazard: Dangerous` (a creature
*in a passage* is a patroller — a moving body, not a sitter — captured as
`MAP-dangerous-route-is-a-patroller`); a heart PATTERN row in
`CYCLE_PATTERNS` (an epoch under 0620; the probe shows a hub already occurs
on every level, so the row would name what happens rather than make it
happen — `MAP-heart-is-a-hub-at-level-scale` is narrowed to say so); reading
`character_of` in `Underground::enter` (`MAP-walk-ignores-the-lattice`,
narrowed not closed — a drawn `DrowTier` with no settlement is a tier
nobody founded, and the ruin reading is the natural home for it later);
carving by role (region size is the plan's; the Made bias is already 0.85);
furnishing the heart with the Hearth's grammar (`CLIENT-furnishing-ladder`);
any transfer verb; surface districts (The Precincts, which reads §3.1 and
owes it nothing else); the world-conditional gate (metaplan §4, after the
program); the Brattice's inherited non-goals (secrecy, collapse, the true
valve, the knowledge key, occlusion, the strongbox literal, per-world
carves, junctions, vaults, viewport).

## 7. Acceptance

1. **A cut place, walked.** A session test possesses at an occupied
   underworld column of seed 42 (chosen by scanning the 26 for one whose
   Made rung hangs a door — a test over data, tagged `claim:`), descends to
   the Made rung, and `look` reads the Entry sentence in the present tense;
   walks to the Heart and reads the hall sentence; `examine hall` answers;
   a `Threshold` on that rung refuses with the locked-door refusal, so the
   Brattice's verbs are reachable in production for the first time.
2. **A ruin, walked.** At a column whose every occupation has ended (seed
   42 has 67 occupations over 26 columns, so ended ones exist; the test
   scans), the same sentences read in the past tense.
3. **The hoarder sits on the hoard.** On a wild rung with a resident, the
   resident's mark is in the Sanctum's region; the possession drops a thing
   there and `examine <kind>` names it in the datum.
4. **The readouts** of §4.1–4.4 are on the committed pages with their
   verdicts in the frozen words; the Crosscut's and Brattice's sections did
   not move.
5. **Every existing wall, carve, realization-witness and Brattice walk test
   passes unmodified**, except `resident_cell`'s own unit test, which is
   amended to the new rule.

## 8. Task shape (detail belongs to the plan)

0. Preflight — the probe (committed with the ledger), the plan-digest
   fixture for §3.3's pin, the branch tables of §5, the panel baselines.
1. `worldgen`: `plat.rs` — `Role`, `Reading`, `read`; construction tests;
   the probe re-run under the plan metric, its numbers recorded.
2. `worldgen`: `column_origins` + `Tenancy`; `plan_descent` takes origins;
   `cycle_budget`/`worked`/`admissible` per level; the regression pin; the
   agreement test with `made_chambers`; the heavy sweep extended to Made.
3. `vessel`: `delve_at` → `enter` with origins; the place sentence and its
   nouns; `resident_cell` on the Sanctum; the hoard in the datum.
4. `worldgen` + `cli`: the two readouts at `Full`; regenerate; diff read
   against §5's tables.
5. Acceptance walks (§7.1–7.3) as session tests.
6. Book chapter, decisions 0646–0649, registry sweep, retrospective, close.

Stage gates after 2 and 4.

## 9. Decisions this campaign expects to mint

- **0646** — The inhabited reading is a function of the plan: use follows
  form; Entry, Heart (the graph median under the plan's metric, entry
  excluded), Sanctum and landing are derived, drawn by nothing and stamped
  on nothing, so the plan grammar does not move and 0618 is not engaged.
- **0647** — A Made chamber is written from the ledger at the walk, never
  committed: `column_origins` over `occupations_at` and `seat_at`, per
  column, at `delve_at`; workmanship joins the plan per level as
  `worked(character) || origin == Made`, the clause 0568 reserved.
- **0648** — The hoarder sits at the Sanctum on what lies there: WHO and
  WHETHER are the energy field's, WHERE is the reading's, and it holds
  nothing until a transfer verb exists (Roll §7).
- **0649** — A cut place outlives its people: `Made` reads the occupation
  whether or not it is alive, and tenancy decides the tense.

**Flagged for G3 (leads the package):** §3.3's "an input, not a grammar
change" argument against a 0618 epoch; §3.6's move of two audit commands from
`Terrain` to `Full` builds (a regeneration-cost change, measured in Task 4);
the hoarder staying a mark rather than a body (the scale in ledger #2).

## 10. Provenance

Alexander, *A Pattern Language* (1977): 98 Circulation Realms, 112 Entrance
Transition, 127 Intimacy Gradient ("a sequence which begins with the entrance
and the most public parts … and finally to the most private domains"), 129
Common Areas at the Heart ("at the center of gravity of all the spaces the
group occupies, and in such a way that the paths which go in and out of the
building lie tangent to it"), 131 The Flow Through Rooms, 133 Staircase as a
Stage. Graph median (Jordan 1869; Hakimi 1964) as the discrete center of
gravity. The project's own prior: the Circuit metaplan; decisions 0566–0568,
0616–0620, 0070, 0069, 0398, 0400, 0546–0547; The Roll spec §7; the Gallery
spec §3.6; `MAP-heart-is-a-hub-at-level-scale`,
`PLAY-key-placement-stands-in-for-a-resident`, `MAP-chamber-occupancy`,
`MAP-walk-ignores-the-lattice`, frontier §"A pattern language for traversal,
not furnishing". Brainstorm and ideonomy record:
`docs/superpowers/ledgers/2026-09-03-the-plat.md`.
