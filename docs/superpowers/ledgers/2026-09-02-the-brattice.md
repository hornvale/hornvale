# The Brattice — campaign ledger

Campaign: gates for the underworld (The Circuit, campaign 2). Metaplan:
`docs/superpowers/specs/2026-09-01-the-circuit-metaplan.md` §3. Predecessor:
The Crosscut (`docs/superpowers/ledgers/2026-09-01-the-crosscut.md`,
decisions 0566–0568, merged as `daa94492c`). Autopilot engaged from the first
message; Nathan reviews at G3 and G6. Decision block 0616–0625 reserved
(`scripts/decision-block-request.sh the-brattice`, 2026-09-02); 0569–0575
belong to the-crosscut's reservation and are not minted into.

Entries are committed as each ruling occurs (decision 0486), never batched.

#1 [Q] — Visual companion? · **Started at the first message, before any
question** · Why: CLAUDE.md's standing preference ("always use the visual
companion during brainstorming — don't ask, just set it up"); the Crosscut
skipped it because no human was present and flagged the deviation at G3
(its entry #6), and this campaign's brief asks for the companion explicitly
· Discarded: text-only with fenced diagrams (the Crosscut's substitute) —
the brief names the deviation as the thing to correct · Ideonomy: 1 pass
(inversion: what does a companion cost when nobody is watching? — the
screens persist under `.superpowers/brainstorm/` and read as a record of
what was shown, so an absent reader loses nothing and a present one gains
the diagrams; no overturn) · Capture: server started with `--project-dir`
on this worktree; screens are numbered `NN-<topic>.html`.

#2 [G1] — How are gates represented on a series-parallel plan whose realms
already carry a `LengthClass`? · **A gate is a requirement on a WAY, stamped
on the finished plan by a post-growth pass that applies one frozen cycle
pattern per realm; the requirement's SUBSTANCE is derived from rock and work
(a door needs a maker, a sump needs karst, a chute needs a floor below); the
drop is a stairway with its up-half omitted and `Fly` is its key; solvability
for a body holding nothing is an invariant, not a prediction** · Why: the
Crosscut's `try_extend` deletes an edge and splices a chain into every realm
path that carried it (`circuit.rs:906-929`), so an attribute keyed on `(a, b)`
during growth is orphaned — the plan must be final before anything is stamped;
`Node`/`Edge` carry no attribute slot at all today and `Realm.class` is the one
semantic tag (`circuit.rs:148-217`); the frontier's line ("a traversal grammar
is on the right side only for as long as its gates are derived — from ecology,
from hydrology, from what a people cut — and not hand-placed") decides that the
pattern chooses WHERE and the world chooses WHAT; decision 0567's Consequence
already states the drop as "omitting the up half of a pair"; decision 0347 (an
affordance is a relation between object and body) makes "valve vs asymmetric"
a fact about gate × body, not about the gate · Discarded: (i) stamping during
growth — orphaned by `extend`; (ii) a separate `Brattice` struct beside the
plan — two objects for one graph, and The Plat would read both; (iii) a
`Shut` way (a true one-way valve) — nothing natural in a cave is passable one
way for EVERY body, and the drop already is a valve for every body that cannot
fly, so the variant would be a reserved seam with no constructor (the shape
0398 refuses); captured as a follow-up instead; (iv) drawing which capability a
natural lock demands — a draw where a derivation exists is the "for fun"
constant 0568 refused; (v) realizing hazards (dangerous/uncertain) and
collapsing locks — no substrate exists (danger is The Plat's hoarder, secrecy
needs a render change), so they are stamped and exported unread, exactly as the
Crosscut exported `LengthClass` · Ideonomy: 1 pass to convergence (`--more`
tuple: tree-finding × dimension-identification × abstraction-lift; organons
chart × lattice; prompts symmetry, complexity, distribution, autonomy, source).
**Four enrichments, one overturn** — see the organon below. The overturn: the
G1 draft read "capability keys (Swim, Fly)" as attributes a pattern assigns;
the source prompt made them DERIVED from the rock the way density is (0568),
and the symmetry prompt made valve-vs-asymmetric body-relative · Capture: spec
§2–3; registry rows for the true valve, the secret door's rendering, the
collapsing gate, and the strongbox's `Portable` literal (below).

#3 [Q] — Does The Brattice touch `MAP-walk-ignores-the-lattice` or
`MAP-descent-carves-are-per-world`? · **Neither** · Why: the plan is keyed to
the walked descent `(seed, vertex)` (ledger #7 of the Crosscut) and gates ride
the plan, so the walk sees every gate without the lattice being wired; the
carve streams' missing vertex is texture, and a gate is placed in the divider
wall or at a stair coordinate the PLAN owns, so identical carves across
vertices cannot move a gate — both rows stay open with a note that doors and
keys now vary per vertex where rock still does not · Discarded: fixing the
carve keying "while we are in the realizer" — a v2 of three carve legs is an
epoch The Crosscut's Task 0 established nobody needs yet · Ideonomy: 1 pass
(scope prompt) — the per-level / per-descent / per-world substitution confirmed
the gate's key is the plan's key, so the debt is orthogonal · Capture: this
entry; the two rows gain a sentence each at close.

#4 [Q] — Where does a body's capability live? · **A sparse `Locomotion`
component store in `hornvale_species`, keyed by `KindId`, threaded onto `Body`
at derivation the way `thermal_strategy` is** · Why: decision 0576's provision
table — a `KindId`-keyed build-state capability lives in the component layer,
never the ledger; `habitat_realm_registry` is the exact sparse-store precedent
(one consumer, rows only for the non-default); six kinds swim (reef-shark,
killer-whale, giant-octopus, giant-squid, giant-crocodile, sea-elf) and three
fly (the dragons), so the store is non-empty in both modes and 0398 ("a
capability nothing can reach is not a capability") is met by a possessable
body, not a promise · Discarded: a carried/worn item granting a mode (The
Gallery's other candidate) — invents an item class with no grammar behind it;
a `HabitatRealm::Aquatic` — that enum is two-valued about SCORING and its doc
refuses to become a realm vocabulary · Ideonomy: 1 pass (source prompt) —
"where did the capability come from" split it into taxon (here), artefact
(discarded) and knowledge (a future secret-door key) · Capture: spec §3.5.

#5 [Q] — Does the strongbox's "any `Portable` opens every lock" hazard
(`session.rs:8808-8834`) come into scope because a second portable kind is
about to exist? · **Not fixed here; the descent door's requirement is the
SPECIFIC key the plan binds, so the door never consults the `Portable`
literal, and the strongbox keeps its hazard with a follow-up row** · Why: the
lock arm is The Chattel's and decision 0516 says a future campaign meeting the
strongbox's weakened clause must not strengthen it without reopening 0516; a
descent door is a different object whose two ends the plan places, so its rule
is structural, not empirical, and needs no reopening · Discarded: adding
`ObjectProperty::Unlocks` and repointing the strongbox arm — right, cheap, and
someone else's decision to reopen · Ideonomy: 1 pass (tree-finding: siblings
of "door" are cave-mouth, strongbox lid, threshold; only the strongbox has the
hazard, so it is the strongbox's) · Capture: registry row
`PLAY-strongbox-lock-wants-an-unlocks-property`; spec §6.

## The ideonomy organon (G1)

### Tree: where a gate sits in the project's own taxonomy

```
  up:    gate -> obstacle -> traversal constraint -> `requires` (a dependency
         edge; the Hearth's) -> a partial order over regions
  down:  gate -> { lock, valve, hazard, secret }
         lock -> { item lock (door + key), capability lock (sump, chute) }
         key  -> { thing (held), capability (been), knowledge (seen) }
  across (siblings of "door", all passage-shaped things with an openness
         fold, decision 0396): cave-mouth (BarrierState, `clear`),
         strongbox lid (`open`, Lockable), building threshold (AffordsPassage)
         -> a door is the cave-mouth's sibling one band down, not a new class
  levels: edge attribute (plan) / cell kind (level) / thing (identity, facts)
         / refusal string (session) -- one gate, four readings, one truth
```

### Chart: what the traverser needs x what substrate exists today

```
  rows: the requirement the WAY imposes      cols: where it can be realized
                | cell kind          | thing + fold        | session state   | plan stamp only
  --------------+--------------------+---------------------+-----------------+----------------
  a held key    | Door (anchor)      | door: Openable+     | custody fact    | Needs(Key(n))
                |                    | Lockable; key:      | (0400, exists)  |
                |                    | Portable (exists)   |                 |
  a capability  | Deep -> Swim       | --                  | Body.locomotion | Needs(Mode(m))
   (Swim)       | (Flooded's sibling)|                     | (new, species)  |
  a capability  | Drop -> down free, | --                  | Body.locomotion | Needs(Mode(Fly))
   (Fly, return)| up needs Fly       |                     |                 | on the up-way
  nothing, but  | --                 | -- (the hoarder is  | --              | hazard: Dangerous
   it costs     |                    |  The Plat's)        |                 |   STAMPED, unread
  knowledge     | -- (a wall until   | --                  | fog / seen bits | hazard: Uncertain
   (secret)     |  seen: render work)|                     |                 |   STAMPED, unread
  an event      | --                 | (openness fold could| a crossing fact | persistence:
   (collapsing) |                    |  carry it, 0396)    | -- none exists  |  Collapsing
                |                    |                     |                 |   STAMPED, unread
  Read: the top three rows have a full substrate and are REALIZED this
  campaign; the bottom three have none and are stamped, exported and read by
  nothing -- the Crosscut's `LengthClass` discipline applied to the taxonomy.
```

### Lattice: traverser states, ordered by "can pass more"

```
        {Walk,Wade} + key_1 + key_2 ...            (the resident: every key)
            /              \
   {Walk,Wade,Swim}      {Walk,Wade,Fly}          (a shark; a dragon)
            \              /
      {Walk,Wade} + key_1                          (the intruder, one key in)
              |
          {Walk,Wade}                              (the default body, holding
                                                    nothing -- the SOLVABILITY
                                                    witness)
  A gate maps each direction of an edge to the least state that passes it.
  A pattern is a placement rule for gates and keys relative to a cycle's two
  paths. SOLVABILITY = the terminus and every key node are reachable from the
  entrance in the product graph (node x state), starting from the bottom
  element. Dormans' safety rule ("the key before the lock") is the same
  reachability, stated per lock; the capability-lock rule (never on a bridge
  for the default body) is the same reachability, stated per mode. One
  checker serves the placement pass, the invariant and the readout.
  Meet of shark and dragon is the default body: a gate BOTH can pass and the
  default cannot does not exist, which is why a pure valve (`Shut`) has no
  place in the lattice -- it would be a state nobody reaches.
```

### Abstraction-lift and re-instantiations

Lifted: *a partial order imposed on a cycle by requirements that one class
of walker meets everywhere and another class must go and fetch.*

- **Mine ventilation (the name).** A brattice is a partition hung across a
  working to force air the long way round; a regulator is a door with an
  opening; an air-lock is two doors of which one is always shut. The
  campaign's gates are the intruder's brattice: the loop stays a loop for air
  and residents, and one side of it is closed to a body without the key.
  -> the collapsing lock is an air-lock (two gates in series) and is a
  pattern of TWO gates, captured, not this campaign's.
- **Rail token-block working.** A single-line section may be entered only by
  the driver holding the section's token, collected at one end and surrendered
  at the other. -> the key is bound to ONE lock and the binding is structural
  (the plan says which key fits), never a table of kinds.
- **Gated ion channels.** Ligand-gated (a key), voltage-gated (a state of the
  body: a capability), rectifying (one way at a given potential: the drop).
  -> `Way` per direction, `Requirement` = Key | Mode, no third variant.
- **Circuits (the Crosscut's own).** Diode = valve, switch = lock, resistor =
  dangerous lock. -> `hazard` is orthogonal to `Requirement`: a resistor is
  not a switch, and a dangerous door is both.

### Dimensions surfaced (and where each went)

- symmetry: symmetric (door) / asymmetric (drop: free down, Fly up) / valve
  (the SAME drop, for a body that cannot fly) -> body-relative, spec §3.1
- source of the requirement: rock (sump, chute) / work (door) / creature
  (danger, The Plat) / knowledge (secret, captured) -> spec §3.2's table
- when stamped: during growth (orphaned) / after growth (here) / at realization
  (would put a structural fact in the vessel, ledger #3 of the Crosscut) /
  at walk time (the session would be inventing the world)
- distribution: one plan pass stamps everything (concentrated truth); four
  readers realize it (distributed readings) -> "one gate, four readings"
- autonomy: the world resolves capability locks, the player resolves key
  locks by walking, a resident resolves danger (The Plat)
- complexity: simplest = one pattern ("two alternative paths") stamping
  nothing, which every LongLong realm may still draw; richest = nested
  patterns on nested realms, which creation order already sequences

#6 [G2] — Spec self-review against the code, at drafting time · **Three
claims corrected before commit; six verified with their commands** · Corrected:
(i) §3.5's sump would have turned the corridor's two walkable ENDPOINTS
(`nearest_pair` returns cells inside each region, `underworld_level/mod.rs:
503-514`) into `Deep`, severing region floor — now only cells that were rock
before the carve; (ii) §3.6 said the corner rule keeps asking `movement_mode`
"because a shut door is still a solid corner", which is the opposite of what
that seam answers (`Door -> Walk`) — reworded: a doorway is an opening whether
the door is shut or not; (iii) acceptance 2's return leg named the wrong end
of the realm. Verified: the client draws an unrecognised palette kind as rock
(`clients/game/core/src/level.rs:150-185`, the `_` arm), so three new kinds
are additive on the wire and a client task; `entry_for`'s match is exhaustive
(`level_doc.rs:351-357`), so a new variant will not compile without its wire
string; `Node {`/`Edge {` literal sites for the circuit types are all inside
`circuit.rs` (10; the other 34 hits are other crates' `Node`s); the L-corridor
crosses the one-cell divider exactly once for both adjacency directions
(`connect_cells`, `mod.rs:520-527`: horizontal at `a.1` then vertical at
`b.0`, with grid-adjacent rects sharing the orthogonal range) — stated in the
spec as a tested claim, not a fact; `Underground::enter` passes
`Character::WildCave` at two sites (`underground.rs:314, 322`); nine species
carry a plausible locomotion (grep of `KindId("…")` in
`domains/species/src/lib.rs`). Not verifiable here and flagged for G3: the
Fig. 9.8 names · Ideonomy: none for this entry — it is a verification pass,
not a ruling; the rulings it touched are #2 and #4.

## Followups (promoted into the retrospective at close)

- **The production walk reaches no door.** `Underground::enter` hardcodes
  `WildCave`; `character_of` needs a branch the walk does not have
  (`MAP-walk-ignores-the-lattice`). The Plat's `ChamberOverrides` writer is
  the metaplan's own answer; if Nathan wants doors reachable sooner, the
  cheapest route is `enter` reading `character_of(seed, vertex, band, 0)` with
  branch 0 as a stated convention — a one-line change with a convention debt.
- **The strongbox's `Portable` literal** —
  `PLAY-strongbox-lock-wants-an-unlocks-property` (ledger #5).
- **Stale prose in `session.rs:3747`** names `the-key-on-the-ledge`, a pattern
  that does not exist (the shipped one is `the-key-by-the-loom`). Found by the
  reconnaissance; not this campaign's file to rewrite unless Task 5 touches
  that function, in which case fix it in passing.
- **The Chattel's chronicle** says `lock` and `unlock` shipped beside the six
  verbs; neither exists. A one-line correction in the book's freshness sweep
  at close.
- **The Crosscut's deferred minors** in the Brattice's path: `try_extend`'s
  pre-extend capability test (Task 1 touches `circuit.rs`; take it); the
  terminus debug_assert when a region's only walkable cell is a landing
  (Task 3 touches the terminus write; take it); the CELLULAR rustdoc obituary
  (Task 3, `carve.rs`, take it if the file is open); the membership test's
  re-derivation and the gallery 45↔46 wobble — not in the path, leave them.

#7 [Q, ruled with Nathan at G3] — A gate is four things stacked; should they
be four things co-located? · **Yes: requirement on the plan, PLACE as a cell
kind, OBJECT as a Thing carried on the wire as a mark, judgment in the walk —
and the draft's `Door` cell kind is replaced by `Threshold`, stamped on EVERY
passage's divider cell, with the door a Thing anchored at one** · Why: the
building lattice already separates place from object (`CellKind::Threshold`
plus anchors; "never `CellKind::Window`", `lattice/mod.rs:131-137`) and keeps
a `doorways` list with a both-directions correspondence check (`lattice/mod.rs:
373-376`); sumps and chutes already showed requirement and substrate without
an object; the level document already carries `marks` for a thing at a cell
(`level_doc.rs:241`) · Discarded: `Threshold` only where a door is (place
exists only where the object does — the conflation in smaller type); a
`Door` glyph as a cell kind (paints built doorways across a wild cave, the
side-effect pass's finding); splitting the requirement off the edge as its
own node (bipartite, no longer series-parallel) · Ideonomy: 1 pass (tuple:
combination × organon-construction; dictionary; prompts age, side-effect,
animacy). **One correction, two enrichments, two captures.** Correction: the
threshold-everywhere proposal as first stated would have rendered a doorway
at every crossing of a natural cave — the place is real (a squeeze) but the
doorway is the door's, so the door goes on the wire as a mark. Enrichments:
(i) requirement × judgment is the one non-independent pair — the walk never
reads the plan, only the realization — so a REALIZATION WITNESS test (0577's
word) is required, not optional; (ii) `hazard` is not orthogonal to
`requirement`: Dormans' dangerous lock is a requirement of the FACED kind,
which The Plat may promote into the enum — kept a stamp here because nothing
reads it, and said so. Captures: `MAP-knowledge-key` (the KNOWN requirement,
0397's half-built mechanism), `MAP-doors-occlude` (a shut door should block
sight; fog has no occlusion input). Dictionary blur to fix in prose: "key" is
used broadly (what satisfies a requirement) and narrowly (the Thing kind);
the spec now says which at each use where it matters · Capture: spec §3.5,
§3.6, §3.7, §3.8, §6, §7.1, §8 task 3, §9 (0616) amended and say so; the two
rows above.

#8 [Q, ruled with Nathan at G3] — Do doors from literature and games fit
the four-part model, and where do they not? · **They fit wherever the
requirement reads the traverser's own state; every exception reads something
else, and that boundary is the solvability proof's** · Cases: Zork I's trap
door (known + collapsing + valve: fits, stamped half); Durin's password
(known); Alice's door (bodily); consumed keys and keys left in the lock (a
KEY persistence, Dormans' single-use key — cheap, not needed); the twisted
candlestick (a switch: `Switched(node)`, a key you act on and do not carry —
fits the solver); the false door and the mimic (an object with no way —
admissible once the door's anchor rule is the lattice's "at a wall cell",
which the spec now says); the Hawkmouth (a Thing that is a Body — The Plat's
question, not forced). Breaks: the curtain of light and Howl's door (the far
end is a function of state — not a gate, not series-parallel); moonlight,
tides, shutters, the Room of Requirement (a requirement on the WORLD —
Hornvale's cheapest and most valuable extension, and a different proof);
co-op plates (two bodies); the Lost Woods (path history) · Discarded: adding
`When`/`Switched`/`Faced` variants now — each would be a reserved seam with
no constructor (0398) until something realizes it · Ideonomy: this WAS the
pass — Nathan supplied the cross-domain corpus (literature × games) and the
model was read backwards as a capability probe, the tropes/ discipline;
convergence in one pass, no overturn, one relaxation (the anchor rule) ·
Capture: spec §3.7 (anchor rule), §3.8 (the assumption named); rows
`MAP-world-conditional-gate`, `MAP-false-door`; the remote switch folded
into the false-door row; the portal and the co-op plate recorded as stated
limits in §3.8, not as rows.

## G3 record

Nathan approved the spec at `f2488619f` (2026-09-02) after two rulings made
with him present (#7, #8). Of the three flagged items: the Fig. 9.8 names
stand as transcribed, so the inventory is **frozen at ten rows** and Task 1
asserts that count; the save-format contract a descent key's identity creates
is accepted (decision 0618 will record it); doors being unreachable in the
production walk until The Plat is accepted as the program's own ordering. The
companion's screen 3 recorded no click, so the freeze is by Nathan's word in
the terminal, not the A/B. One capture action taken on approval: the
metaplan's §4 gains a line naming the world-conditional gate as the first
extension after this campaign, so The Plat does not meet it unplanned.

#9 [G4] — Plan self-reviewed against the approved spec · **Proceed; three
readings recorded where the plan departs from the spec's letter** · (i) spec
§3.6 "`Body` gains `locomotion`, threaded at derivation" is implemented as an
accessor reading the species store by label, because the value is a pure
function of `species` and a stored field would touch the ~43 `Body {` literal
sites Task 0 counts for no new information; (ii) spec §3.5 "every passage has
one `Threshold`" — a sump's crossing cell is `Deep` (a drowned squeeze), since
one cell has one kind and a one-cell corridor's sump would otherwise realize no
`Deep` at all; the witness test states the rule; (iii) spec §3.2 row 6
"hazard Dangerous on both" — a row stamps one side (`path_a`); the realm is
the unit of the reading and nothing reads the stamp · Why: each is the spec's
intent met by a cheaper mechanism, and each is written into the plan where the
implementer meets it rather than left for a review to find · Discarded: a
`locomotion` field (43 edits); `Threshold` AND `Deep` on one cell (no such
cell kind); two hazard stamps (a redundant claim on unread data) · Ideonomy:
none — a self-review, not a ruling; the rulings it records are readings of
#2 and #7 · Capture: plan header, Tasks 1/3/4; spec unchanged (the plan says
where it departs and why, which is the Crosscut's "amended in execution and
says so" discipline applied one gate earlier).

## Task 0 record

Run inline by the controller (four shell commands and this note; the
dispatch skill's "trivial exact-content changes are cheaper inline" rule).
Epoch grep, unchanged from the Crosscut's spec §5:
`grep -rn "Level\b\|LevelCellKind\|CellGrid" --include=*.rs windows domains kernel cli | grep -v "windows/vessel/src/underworld_level/\|windows/vessel/src/underground.rs\|/tests/\|#\[cfg(test)\]" | grep -n "commit\|Fact\|ledger"`
→ no output, exit 1. No committing reader of a level exists; the campaign's
one save-format consequence remains the descent key's identity (spec §5).
Baselines copied to `.superpowers/sdd/2026-09-02-the-brattice/baselines/` at
`aee210745`. Literal sites: `circuit.rs` `Node {`/`Edge {` = 10; vessel
`Level {` constructions = 4 (`underworld_level/mod.rs:277`,
`underground.rs:1109`, `:1130` (tests), `level_doc.rs:389` (test)).
Inventory frozen at 10 (G3 record).

## Task 1 - BLOCKED at review (implementation complete, two findings)

**Built.** `windows/worldgen/src/brattice.rs` (gate types, the ten-row frozen
inventory, the geometry helpers, the pass, the product-graph solver);
`Node.key` / `Edge.gate` / `DescentPlan.patterns` / `DescentPlan.skipped_patterns`
and the `edge_index` / `gate_between` accessors in `circuit.rs`; the pass hooked
into `plan_descent` after `assign_depth`; the `UNDERWORLD_GATE_PATTERN` label
(`underworld/gate/v1/pattern`) and its roster row; the `dof` identity extended
by `+ realms`; the Crosscut's `try_extend` deferred minor taken.

**Test evidence.** `cargo test -p hornvale-worldgen` -> **485 passed, 1 failed**
(`brattice::tests::a_lava_tube_never_carries_a_sump_and_some_descent_carries_a_chute`).
Green among the 485: `every_plan_is_solvable_for_a_body_holding_nothing`
(200 seeds x 3 kinds x 3 characters), `gates_and_keys_claim_at_most_once_and_add_nothing`,
`dof_counts_every_draw` (the amended `+ realms` identity, 4,800 plans) and
`realms_are_the_mesh_count_and_every_level_is_inside_the_clip` (the 400-seed
sweep - the post-extend capability test starves no level).
`cargo clippy -p hornvale-worldgen --all-targets -- -D warnings` clean;
type-audit `check` rc=0; the CLI's `streams` lib tests 15/15.

### Finding 1 - four of the ten frozen rows are unreachable by construction

Measured over 4,412 realms (60 seeds x 3 kinds x 2 characters, vertex 2), the
class x span support of `Realm.class` is:

```
  LongLong   CrossFloor    68      LongLong   SameFloor   198
  ShortLong  CrossFloor  1203      ShortLong  SameFloor  2518
                                   ShortShort SameFloor   425
```

`LongShort` **never occurs at all**, and `ShortShort x CrossFloor` never occurs.
This is structural, not statistical, and it is a property of the Crosscut, not of
this task: `Realm.class` is frozen at creation from `length_class(len_a, len_b)`,
where at creation `path_a` has at most 3 edges (`segment(u, v, hops)`, `hops`
drawn from `0..3`) and `path_b` has at least 2 (`free_path(.., min_interior: 1)`),
or at least 3 cross-floor. `LongShort` requires `len_a > len_b + 1`, i.e.
`len_a >= 4`. Unreachable. Verified with the Step-10 change reverted as well, so
it is not caused by this task.

Consequence: rows 2 `hidden-shortcut`, 3 `dangerous-route`, 8 `the-chute` and
10 `the-landing-hall` are dead data - exactly what spec 3.2 says the inventory
must not contain ("the rows below are the ones whose placement the grammar can
express"; "a pattern nothing selects is dead data, not inventory"). `the-chute`
being dead means `Mode(Fly)` is never stamped anywhere, so spec 4's chute
predictions and Tasks 3/4/5's chute work have no input.

Outcome distribution over 1,200 plans (200 seeds x 3 kinds x 2 characters,
vertex 3): 12,700 `Applied` (blocked-retreat 4,279; key-downstairs-lock-upstairs
2,460; the-sump 2,484; lock-and-key-cycle 1,693; patrol-path 1,237;
two-alternative-paths 547), 610 `Skipped(Claimed)`, 1,394 `Inadmissible`,
**0 `NoRoom`, 0 `Unsolvable`**; 13,772 gates and 4,153 keys placed.

Measured remedy, not applied (it is a Crosscut semantics change and outside a
Task 1 ruling): recomputing `class` from the realized path lengths in the
attributes phase - `try_extend` splices nodes into existing realm paths after
the class is frozen, so today's stored class is stale (max realized `len_a` is
16 against a frozen ceiling of 3). With that recompute the support becomes
`LongShort CrossFloor 20`, `LongShort SameFloor 63`, and nine of ten rows become
live (`the-landing-hall` stays dead). Nothing outside `circuit.rs` reads
`LengthClass`, and none of the four Crosscut readouts reads `class`.

### Finding 2 - Step 10 moves all four Crosscut readouts

`cargo run -q -p hornvale -- circuit --seed 42` against
`baselines/circuit-panel.before.md`:

```
  loop share          0.1233 -> 0.1077
  cycle membership    0.8548 -> 0.8442
  cross-floor         841/874 = 0.9622 -> 839/874 = 0.9600
  semilattice overlap 0.3061 -> 0.3030
  density ordering    unchanged; every verdict word unchanged
```

Attributed decisively: with the single line
`would_still_cycle_after_extend(level, u, v, &interior)` reverted to
`would_still_cycle(level, &interior)`, the panel is **byte-identical to the
baseline**. The gate pass adds no node and no edge and moves nothing; the
movement is entirely the Crosscut deferred minor (Step 10), which by
construction changes which `extend` moves land. Whether to accept a moved
committed page is the controller's call, not an implementer's.

**Ruling: `patrol-path` stamps `path_a` (`Side::Long`), not both sides** - the
realm is the unit of the reading and nothing reads the stamp; recorded in the
row's `hazard` doc. Cost if wrong: one unread stamp missing from `path_b`.

**Ruling: the roster row sorts before `underworld/level/cellular v1`, not before
`underworld/plan/cycle v1`** as the brief said - `the_stamp_is_exactly_this_roster`
sorts ascending and `gate` < `level`. Cost if wrong: nil; the test pins it.

**Note: `Body.keys` is tagged `bare-ok(count: keys)`** - the brief proposed
`bare-ok(bitset: keys)` and `bitset` is not one of `BARE_OK_CLASSES`
(`tools/type-audit/src/tag.rs`). `KeyFor`'s tuple field is `bare-ok(index: 0)`.

## Task 1 - fix round 1 (controller rulings A, B, C) - COMPLETE

Both blockers resolved. `cargo test -p hornvale-worldgen` -> **487 passed, 0
failed** (lib) plus 3 / 348 / 0 in the other targets; `cargo clippy --workspace
--all-targets -- -D warnings` clean; type-audit `check` rc=0; the CLI `streams`
lib tests 15/15.

**Ruling A - `Realm.class` is recomputed after growth from the REALIZED path
lengths** - a new `recompute_classes` pass in `plan_descent`, between
`assign_depth` and the gate `stamp`, re-deriving `class = length_class(path_a
.len() - 1, path_b.len() - 1)`; `length_class`'s rule is unchanged - **why:**
the class was frozen at creation and `try_extend` splices chains into realm
paths afterwards, so the exported class described a graph that no longer
existed (max realized `len_a` 16 against a creation ceiling of 3), making
`LongShort` structurally unreachable and four inventory rows dead data. A
Crosscut latent defect that nothing read until The Brattice - **cost if
wrong:** the pattern selector would keep reading a stale class, and rows 2, 3
and 8 would stay dead. Pinned by
`circuit::tests::realm_class_is_recomputed_from_the_realized_paths` (200 seeds
x 3 kinds), which asserts the equality AND that a `LongShort` realm is actually
sighted - an equality alone cannot tell the pass apart from one that never runs.

**Ruling B - row 8 `the-chute` widened to `classes: &[LongShort, ShortLong,
LongLong]`, `span: CrossFloor`** - **why:** a drop needs a floor below, not a
short lower path; the organon's `LongShort` cell is structurally rare (20 of
4,412 realms) because a cross-floor `path_b` is laid with at least 3 edges
against a `path_a` of at most 3. `name` and `source` are provenance and are
unchanged; `the-landing-hall` keeps `ShortShort x CrossFloor` alone; the count
stays 10 - **cost if wrong:** the chute is placed on realms the organon did not
name. Measured consequence in the per-row table below: **717 of the row's 823
draws end `NoRoom`**, because under `ShortLong` `sides()` puts `Side::Short` on
`path_a`, the same-floor existing segment, which is never a `Stair`.

**Ruling C - Step 10's `try_extend` fix kept; the four Crosscut readouts are
accepted as moved** - **why:** decision 0618 makes every later plan-grammar
change an epoch of `underworld/plan/v1`, so this is the last campaign that can
take the fix for free - **cost if wrong:** a committed audit page moves without
the defect being real.

Attribution, as measured: `loop share 0.1233 -> 0.1077`, `cycle membership
0.8548 -> 0.8442`, `cross-floor 841/874 = 0.9622 -> 839/874 = 0.9600`,
`semilattice overlap 0.3061 -> 0.3030`; density ordering and every verdict word
unchanged. **Reverting the single call `would_still_cycle_after_extend(level,
u, v, &interior)` to `would_still_cycle(level, &interior)` makes the panel
byte-identical to the baseline** - so the gate pass alone moves nothing, and
neither does Ruling A (the panel after Ruling A is byte-identical to the panel
before it).

### Per-row outcome table, 4,412 realms (60 seeds x 3 kinds x 2 characters, vertex 2)

```
  #  row                           drew  applied  Claimed  NoRoom  Unsolvable
  1  two-alternative-paths          137      137        0       0           0
  2  hidden-shortcut                 24       24        0       0           0
  3  dangerous-route                 39       39        0       0           0
  4  lock-and-key-cycle             506      464       42       0           0
  5  the-sump                       810      759       51       0           0
  6  patrol-path                    333      318       15       0           0
  7  blocked-retreat               1341     1334        7       0           0
  8  the-chute                      823       51       55     717           0
  9  key-downstairs-lock-upstairs   399      362       37       0           0
 10  the-landing-hall                 0        0        0       0           0
     Inadmissible (no row admitted the realm):  0
```

**Two things Task 2's readout must know.** Row 10 `the-landing-hall` is **still
fully dead** - it draws zero, because `ShortShort x CrossFloor` does not occur
even after Ruling A (a cross-floor `path_b` has at least 3 edges, so
`ShortShort` needs `len_a == 2`, which the sample never pairs with it). And row
8 wastes 717 of 823 draws on `NoRoom`: 16% of all realms spend their one
pattern draw on a row that cannot apply to them, displacing rows that could
(chiefly `key-downstairs-lock-upstairs`, the other `ShortLong x CrossFloor`
row). Narrowing row 8 to `&[LongShort, LongLong]` - the two classes where
`sides()` puts `Side::Short` on `path_b` - would recover all 717 at no cost to
the 51 applied. Not done: Ruling B is explicit and this is the controller's
call.

**And the chute is thin.** In the exact slice the brief's test samples (60
seeds, LavaTube, WildCave, vertex 2) there is **exactly one chute**. The test
passes verbatim, as ruled, but by a margin of one placement; any later grammar
change can flip it red without any chute logic being wrong.

**Gate guards met on the way in** (none named by the brief; Task 2 will meet
the same ones): the staged `src/streams.rs` forced a regeneration of
`book/src/reference/stream-manifest-generated.md`; the new public surface
forced one of `docs/audits/type-audit-report.md`; `claim_shape` required
`/// claim: <shape>(...)` lines on the three brief-verbatim seed-looping tests
(added as `rate(seed: 0..60)` on the two existence claims and
`invariant(seed: 0..60)` on the conditional one, test bodies untouched); and
`lexicon_guard` refused 11 new `cell`-bearing tokens. The lexicon ceiling was
NOT raised - raising it needs a human's agreement - so `brattice.rs`'s three
prose uses were reworded to "entry" (the file leaves the inventory) and
`circuit.rs`'s eight real `GridCell` uses carry the sanctioned per-line
`// lexicon: <why this one is an area>` waiver, ceiling unchanged at 124.

Commits: `9bde38c54` (the pass) and `118058d51` (the gate's timings rows).
`gate-commit` rc=0, wall 352.190 s, 3,881 tests green.

## Task 1 - fix round 2 (controller rulings D, E) - COMPLETE

`cargo test -p hornvale-worldgen` -> **488 passed, 0 failed** (lib) plus 3 / 348 / 0;
`cargo clippy --workspace --all-targets -- -D warnings` clean; type-audit rc=0.
The seed-42 circuit panel is **byte-identical** to its post-Ruling-A state, so
neither D nor E moves a readout - they change what is stamped, never the graph.

**Ruling D - `Side::Descending`, a third side naming `path_b` by GEOMETRY
rather than by length; row 8 `the-chute`'s `GateSpec` moved onto it** - **why:**
naming the chute's side by length was the defect. Under `ShortLong`, `sides()`
puts `Side::Short` on `path_a` - the same-floor existing segment, all `Passage`
edges, never a `Stair` - so `DownFreeUpNeeds` refused 717 of the row's 823
draws with `NoRoom` and 55 more with `Claimed`, while the row sat in the
inventory looking live. Only `path_b` ever leaves the anchor level, so the
drop belongs on the path that descends whatever its length - **cost if wrong:**
a row that claims a same-floor passage as if it were a stair. Guarded by
`every_key_row_places_a_key_and_no_natural_row_does`, which now also refuses
any row using `Side::Descending` on a span that is not `CrossFloor` (checked
across `gates`, `key`, `hazard` and `persistence`, not just `gates`).

Measured: row 8 goes from **51 applied / 55 Claimed / 717 NoRoom** to
**823 applied / 0 / 0**, and the chute count in the failing test's own slice
(60 seeds, LavaTube, WildCave, vertex 2) goes from **1 to 31** - the "thin by
a margin of one placement" concern is closed, not merely passed.

**Ruling E - row 10 `the-landing-hall` removed; the inventory is nine rows** -
**why:** `ShortShort x CrossFloor` is structurally empty. A cross-floor
`path_b` is laid with at least 3 edges (`[u, lu, ...interior..., le, end]`), so
a realm that crosses a floor can never have both paths short, and spec §3.2
forbids a row nothing selects ("a pattern nothing selects is dead data, not
inventory") - **cost if wrong:** an inventory row that reads as coverage and
provides none. `the_inventory_is_frozen_at_ten_rows` is renamed
`the_inventory_is_frozen_at_nine_rows`, asserts 9, and additionally refuses the
removed name reappearing without the count moving. The module doc and the
`CYCLE_PATTERNS` doc both record the removal and its reason.

**The standard is now mechanical, not a one-off measurement.** New test
`no_row_is_dead_data` (`claim: structural(seed: 0..100)`, 100 seeds x 3 kinds x
3 characters x 2 vertices = 1,800 plans) asserts every row index appears at
least once among `Outcome::Applied`. It passes on all nine. A row that goes
dead under a later grammar change now reds here instead of sitting in a
healthy-looking table - which is exactly the failure mode Task 1 found by hand
and could only find by hand.

### Per-row outcome table after D and E, same 4,412-realm sample

```
  #  row                           drew  applied  Claimed  NoRoom  Unsolvable
  1  two-alternative-paths          137      137        0       0           0
  2  hidden-shortcut                 24       24        0       0           0
  3  dangerous-route                 39       39        0       0           0
  4  lock-and-key-cycle             506      464       42       0           0
  5  the-sump                       810      759       51       0           0
  6  patrol-path                    333      318       15       0           0
  7  blocked-retreat               1341     1334        7       0           0
  8  the-chute                      823      823        0       0           0
  9  key-downstairs-lock-upstairs   399      362       37       0           0
     Inadmissible (no row admitted the realm):  0
```

4,260 of 4,412 realms carry an applied pattern; the remaining 152 are
`Claimed` - a nested realm reaching for an edge its parent already took, which
is the mechanism working. **`NoRoom` and `Unsolvable` are now zero across the
whole sample**, so both rollback paths remain exercised only by construction;
that is unchanged from round 1 and still worth knowing before trusting them.

#10 [G5, controller rulings during Task 1] — Five rulings, each measured
before it was made (the implementer's sample: 4,412 realms) · **A:** recompute
`Realm.class` after growth from the realized paths (frozen-at-creation class
made `LongShort` unreachable, 0/4,412, and described pre-extension paths — a
Crosscut latent defect nothing had read; `length_class_follows_the_frozen_rule`
tested the function, not the plan's classes: the "input collapses to one
value" shape) · **B:** widen `the-chute` to every cross-floor class but
`ShortShort` (the organon's cell is structurally rare: 20/4,412) · **C:** keep
the `try_extend` post-extend fix and accept the Crosscut's four numbers moving
once, attributed by revert (0618 makes this the last free grammar change) ·
**D:** `Side::Descending` for the chute's gate (under `ShortLong`, `Short` was
`path_a`, a same-floor path: 717/823 `NoRoom` → 823/823 applied) · **E:**
remove `the-landing-hall` (`ShortShort × CrossFloor` never occurs — by the
geometric argument spec §3.2's amendment paragraph now states, after the task
review refuted the first one given ("≥3 edges", which `length_class(2, 3) ==
ShortShort` contradicts); count 9; `no_row_is_dead_data` sweeps the rest and
`no_cross_floor_realm_is_short_short` witnesses the lemma) · Why: spec §3.2's own rule against
dead rows, and the spec's §4 preamble intent (the PASS must not move the
graph) satisfied by attribution rather than by forbidding a fix that would
otherwise cost an epoch · Discarded: narrowing the chute to `[LongShort,
LongLong]` (loses the drop on a long lower path for no reason); keeping the
landing hall as a named-but-dead row (the spec forbids it); deferring the
extend fix (an epoch later) · Ideonomy: none — rulings on measured defects,
not open questions · Capture: spec §3.2 (Side, rows, amendments paragraph),
§4 preamble; the G6 digest leads with C (plan bytes) and E (a post-G3 change
to a count Nathan approved). Deferred minor: `Skip::NoRoom` and
`Skip::Unsolvable` are 0 across the sample, so their rollback paths run only
by construction — a hand-built plan that forces each would cover them.

## Task 1 - fix round 3 (review: one Important, three minors) - COMPLETE

`cargo test -p hornvale-worldgen brattice` -> 11 passed, 0 failed.
`cargo test -p hornvale-worldgen circuit` -> 17 passed, 0 failed. Clippy clean,
type-audit rc=0, `lexicon_guard` + `claim_shape` 8/8.

**Important #1 - the recorded REASON for removing `the-landing-hall` was false;
the decision stands, the argument is replaced.** The four sites said "a
cross-floor `path_b` is laid with at least 3 edges, so both paths cannot be
short". That does not follow: `length_class(2, 3)` is `ShortShort` under the
frozen rule (`2 > 4` false, `3 > 3` false, `2 >= 3` false), and a 3-edge
cross-floor `path_b` is constructible because `try_cycle` calls `free_path(..,
min_interior: 0)`. The true argument is geometric and now sits at all four
sites (module doc, `CYCLE_PATTERNS` doc, the count assertion's message, and
here): `try_cycle` lands the two lower nodes on the SAME grid squares as the
realm's endpoints, so `path_b` is two distance-free stairs around a walk from
`cu` to `ce` on level `l+1` while `path_a` is a walk between those same squares
on level `l`; every passage joins grid-adjacent squares and a grid is
bipartite, so both lengths are congruent to the endpoints' Manhattan distance
mod 2. `ShortShort` needs `|len_a - len_b| <= 1` and not both `>= 3`; equal
parity turns the first into `len_a == len_b`, so it needs `len_a == len_b <= 2`,
while `cu != ce` forces `len_b >= 3`. Contradiction.

**The parity step is load-bearing and is mine, not the review's.** The review
gave the geometric foundation with a case analysis over `len_a in {1, 2, 3}` -
the creation-time range. Since Ruling A the stored class is recomputed AFTER
`try_extend` splices (`path_a` reaches 16 edges), so a creation-time case
analysis would prove the lemma only for a class the plan no longer stores -
the same category of error the review was correcting. Parity closes it: a
spliced detour is itself a walk between the two squares it replaces, so it
moves each length by an even amount and the congruence survives.

**Witnessed, not asserted.** New test `no_cross_floor_realm_is_short_short`
(`claim: invariant(seed: 0..100)`, 100 seeds x 3 kinds x 3 characters x 2
vertices) checks directly that no realm whose `path_b` leaves the anchor level
carries `ShortShort`. It passes; had it failed, the lemma would be wrong and
the row would have to come back, and its message says so.

**Ruling F (minor #2) - `Skip::Inadmissible` removed** - it was never
constructed: `stamp` pushes `Outcome::Inadmissible` for an empty admissible
set and never reaches `try_apply`. `Outcome::Inadmissible` is kept - **cost if
wrong:** a reader would keep counting a bucket that can never fill.
`DescentPlan::skipped_patterns`' doc is corrected to say it counts realms with
no admissible row as well as refused ones, so `patterns.len() -
skipped_patterns` is exactly the number of realms carrying a stamped pattern.

**Ruling G (minor #3) - `try_apply`'s gate loop now also refuses an edge THIS
row already resolved to** (`stamps.iter().any(...)`), matching the hazard and
persistence loops - **why:** two `GateSpec`s of one row can name one edge (on a
two-node path the near and far edges are the same edge), and the later would
have silently overwritten the earlier instead of refusing `Claimed` - **cost if
wrong:** a lost gate with no skip recorded, invisible in every readout.

**Ruling H (minor #4) - `worked()` is an exhaustive `match`** over the
`Character` roster instead of `matches!(.., DrowTier)`, following
`character::bands_of`'s convention - **why:** a sixth character must fail to
compile here rather than inherit "unworked" from a wildcard and quietly lose
its doors.

## Task 2 - complete

Five readout helpers added to `windows/worldgen/src/brattice.rs`
(`gate_yield`, `detour_cost`, `realized_requirements`, `return_differs`,
`skip_histogram`), TDD per the brief (Step 1's three tests written first,
confirmed RED with six `E0425: cannot find function` errors, then made
green by the implementation). Four sections added to
`render_circuit_panel` in `windows/worldgen/src/circuit_readout.rs`, plus
the `verdict()` helper exactly as specified. `docs/audits/underworld-
circuit-seed-panel.md` regenerated via `make rebaseline`.

**Which plan each section reads, since the brief left it to me.** Every
Brattice section (gate yield, detour cost, the solvability guard, the
`gates: doors/sumps/chutes` line, `return differs`, and `patterns by class
and span`) reads the SAME per-vertex `Character::WildCave` plan the
Crosscut's own sections already read (`plan`, built once per vertex). The
one exception is "worked descents with a door", which by the controller's
own definition needs a `Character::DrowTier` plan — reused from the density
loop's per-character `p`, so no extra descent is built. **Consequence, and
it's a real finding, not a bug:** `gates: doors 0 ...` on the page for every
seed is correct — a Key row is inadmissible unless `worked(character)`
(`brattice.rs::admissible`), and `Character::WildCave` is never worked, so
the panel's own descent set never carries a door. The door count only shows
up on the DrowTier line right next to it, which is exactly the disclosure
the frozen wording points at ("the production walk reaches none yet, spec
§1"): doors exist in worked descents, the production (WildCave) walk isn't
one.

**Ruling I - `return_differs`'s path reconstruction is a fresh BFS-with-
parents over the product graph, not a reuse of the existing distance-only
`bfs`.** The brief's own spec (BFS parent-chain outbound, then back,
compare edge sets as `BTreeSet<(NodeId, NodeId)>` normalized `(min, max)`)
needs the actual path, not just its length, and the existing private `bfs`
in `brattice.rs` records only `(node, keys) -> distance`. Added `PathState`/
`PathInfo` type aliases (clippy's `type_complexity` refused the inline
tuple-of-tuple) and a private `shortest_path` that also records a
predecessor state per node, deterministic by construction: BFS visits
states in nondecreasing distance, and among equal-distance arrivals at the
target the smallest key bitset wins (replace the pick only on strictly
smaller distance, ascending `BTreeMap` iteration order does the rest). Cost
if wrong: a nondeterministic pick would make `return_differs` disagree
between two byte-identical builds of the same seed - the renderer test's
two-render equality check would have caught it, and did not fire, across
seeds 42/7/1234.

**Three medians and their verdict words, one line per seed (the numbers on
the committed page, spec §4):**

| seed | gate yield (floor 0.70) | detour cost (floor 1.10) | solvability guard |
|---|---|---|---|
| 42   | median 1.0000 -> PASSED | median 1.2143 over 866 gated descents -> PASSED | 874 of 874 |
| 7    | median 1.0000 -> PASSED | median 1.2069 over 1673 gated descents -> PASSED | 1681 of 1681 |
| 1234 | median 1.0000 -> PASSED | median 1.2000 over 1249 gated descents -> PASSED | 1266 of 1266 |

**Skip histogram (§4.1, report only), `[Inadmissible, Claimed, NoRoom,
Unsolvable]`:**

| seed | inadmissible | claimed | no-room | unsolvable |
|---|---|---|---|---|
| 42   | 0 | 198 | 0 | 1 |
| 7    | 0 | 390 | 0 | 0 |
| 1234 | 0 | 269 | 0 | 0 |

Seed 42's single `Unsolvable` is worth flagging past the ledger: Task 1's
report measured zero `Unsolvable` skips across its whole 4,412-realm and
1,200-plan sample and called the rollback path "exercised only by
construction, not by data." The full 874-descent seed-42 panel exercises it
once for real. Not a defect - `try_apply`'s rollback exists for exactly
this case and the panel stays solvable (874 of 874) - but it means the path
is live, not merely reachable.

**Doors/sumps/chutes and worked-door share (§4.4, report only):**

| seed | doors | sumps | chutes | worked descents with a door |
|---|---|---|---|---|
| 42   | 0 | 3169 | 3224 | 872 of 874 |
| 7    | 0 | 5753 | 5439 | 1676 of 1681 |
| 1234 | 0 | 4223 | 4123 | 1263 of 1266 |

`return differs from outbound`: 159/866 (seed 42), 289/1673 (seed 7),
199/1249 (seed 1234) gated descents - report only, expected high wherever a
chute lands (Dormans' "unknown return path"), not predicted.

**The Crosscut's four numbers, old -> new, per seed (spec §4 preamble's
execution amendment, ruling C) - verdict words unchanged, descent counts
unchanged:**

| seed | descents (unchanged) | loop share | cycle membership | cross-floor | semilattice overlap |
|---|---|---|---|---|---|
| 42   | 874  | 0.1233 -> 0.1077 (FALSIFIED, FALSIFIED) | 0.8548 -> 0.8442 | 841/874 -> 839/874 (PASSED, PASSED) | 0.3061 -> 0.3030 |
| 7    | 1681 | 0.1111 -> 0.0893 (FALSIFIED, FALSIFIED) | 0.8533 -> 0.8421 | 1581/1681 -> 1595/1681 (PASSED, PASSED) | 0.2857 -> 0.2794 |
| 1234 | 1266 | 0.1042 -> 0.0794 (FALSIFIED, FALSIFIED) | 0.8511 -> 0.8413 | 1193/1266 -> 1200/1266 (PASSED, PASSED) | 0.2812 -> 0.2766 |

Every descent count is byte-identical to the pre-Task-2 baseline (874 /
1,681 / 1,266) - the pass adds no node, edge, realm or stair, exactly as
the amended branch table requires. Seed 42's four movements match Task 1's
own measurement of the same ruling exactly (0.1233->0.1077, 0.8548->0.8442,
841->839 of 874, 0.3061->0.3030); seeds 7 and 1234 move by comparable small
amounts, confirming this is one attributable movement (Ruling C's
`try_extend` capability-invariant fix), not new drift from this task.

**Patterns by class and span (§4.4, report only) sum consistently with the
totals above** - checked, not merely trusted: seed 42's `the-sump` row
(3169) equals its `sumps` total exactly, and its three `the-chute` rows
(96 + 37 + 3091 = 3224) equal its `chutes` total exactly. No `lock-and-key-
cycle` or `key-downstairs-lock-upstairs` row appears in any seed's
breakdown, consistent with `doors: 0` - both are Key rows, inadmissible for
`Character::WildCave`.

**Artifacts regenerated:** `docs/audits/underworld-circuit-seed-panel.md`
(the four new sections per seed, plus the Crosscut's expected one-time
movement), `docs/audits/type-audit-report.md` (five new tagged pub items:
`gate_yield`, `realized_requirements`, `detour_cost`, `skip_histogram`,
`return_differs`, each `bare-ok`), `docs/generated-path-writes.tsv` and
`docs/timings.md` (the run's own bookkeeping). `clients/game/core/tests/
fixtures/` and `book/` are untouched, confirmed by `git status`.

**Stage gate submission:** `git push -u origin campaign/the-brattice` then
`make sluice-stage BRANCH=campaign/the-brattice REF=<full-sha>` - see the
task report for the queue row and `sluice-status` output.

## Task 2 - fix round 1 - complete

Review approved with three Important findings, one carrying a controller
ruling that touches Task 1's pass. All three fixed, covering tests
re-run, the page regenerated, committed.

**Ruling F (Important #1, the real one): the round trip is now proved, not
just the forward reach.** `solvable(plan, DEFAULT_BODY)` only ever checked
FORWARD reachability (terminus and every key from the entrance), so
`try_apply` could commit a stamp that leaves the default body able to get
down but with no way back - a chute (down free, up needs `Fly`) into a
cross-floor realm whose upper path a nested child realm later blocks with
a sump is a trap, not a puzzle, and the old guard never saw it. Fixed in
`windows/worldgen/src/brattice.rs::try_apply`: after the existing
`solvable(plan, DEFAULT_BODY)` check and before returning `Ok`, ALSO
require `gated_round_trip(plan, DEFAULT_BODY).is_some()`; on failure,
unstamp everything exactly as the existing branch does and return
`Err(Skip::Unsolvable)`. `Skip::Unsolvable`'s doc and `try_apply`'s own doc
both now say "the terminus, a key, OR return to the entrance." The guard
test `every_plan_is_solvable_for_a_body_holding_nothing` (200 seeds x 3
kinds x 3 characters) now also asserts `gated_round_trip(&p,
DEFAULT_BODY).is_some()` on every committed plan - green, confirming the
new check never regresses a plan that used to build cleanly; it only ever
catches a stamp the OLD code would have committed and now rolls back
instead.

**Consequence, measured, not merely reasoned about:** re-running the panel
after the fix moves seed 42 only - `skips: ... unsolvable 1 -> 2` and
`gates: ... chutes 3224 -> 3223` (`ShortLong CrossFloor the-chute: 3091 ->
3090`) - one chute placement that used to strand the body is now correctly
rolled back and recorded as `Unsolvable` instead of `Applied`. Seeds 7 and
1234 are byte-identical to the pre-fix page; `gated_descents` (866 for seed
42) and the detour-cost median (1.2143) are unchanged, meaning the caught
trap's descent still carries at least one other realized requirement.
`detour_cost`'s and `return_differs`'s docs now name both former `None`
causes and state that, since this ruling, only "no realized requirement"
remains reachable from a committed plan in practice - the round trip is
now an invariant, not a possibility those functions have to guard against
at read time. Added `debug_assert_eq!(costs.len(), gated_descents, ...)`
in `render_circuit_panel` right where the detour-cost median is computed,
so the denominator can never silently understate again; it did not fire
against any of the three seeds.

**Important #2 - `gate_yield`'s own test no longer recomputes the
function's arithmetic.** `gate_yield_is_a_ratio_and_none_without_an_
admissible_realm` now overwrites a cloned plan's `patterns` with a
hand-built vector (`[Applied, Skipped(Claimed), Inadmissible, Applied,
Skipped(NoRoom)]`) and asserts `gate_yield(&hand) == Some(2.0 / 4.0)`
directly, plus `[Inadmissible, Inadmissible]` and `[]` both asserting
`None`. The `(0.0..=1.0)` range check on a real plan is kept as a
sanity check on live data.

**Important #3 - a positive-case test for `return_differs`/`shortest_path`
now exists.** New hand-built fixture `chute_only_descent()` (five nodes,
following `circuit.rs`'s `bare_three_node_path()` idiom): the only way down
is a chute, the only way back is the long way around a plain stair. Two
new tests: `return_differs_when_the_only_way_down_is_a_chute` asserts
`realized_requirements == (0, 0, 1)`, `return_differs == Some(true)`, and
`gated_round_trip == Some(6) > ungated_round_trip == 4`;
`return_differs_is_none_on_the_chute_plan_with_its_gate_removed` asserts
the same plan with the chute's gate stripped reads `realized_requirements
== (0, 0, 0)`, `detour_cost == None`, `return_differs == None`.

**Minor - `shortest_path` vs. `bfs`.** A doc line on `shortest_path` now
states why it is a second BFS rather than a reuse of `bfs`: `bfs` records
only a distance per `(node, keys)` state, which is all `solvable` and
`gated_round_trip` ever needed, while `shortest_path` additionally needs a
predecessor per state to reconstruct the actual path.

**Ruling: lexicon guard (unplanned, caught at the first commit attempt) -
the hand-built fixture's `GridCell` literals grew `windows/worldgen/src/
brattice.rs`'s cell-token count from 0 (not in the inventory) to 12, all
legitimate lattice-square (area) uses - why: `Node.cell` is typed
`GridCell`, and the fixture needs five nodes each on a real grid position,
so the type cannot be avoided. Cost if wrong: raising the ceiling instead
of waiving would have let a future genuine vertex-sense `cell` slip in
unnoticed in this file. Fixed by waiving each of the six carrying lines
(`// lexicon: area` / `// lexicon: GridCell is a lattice square, an area`,
matching `circuit.rs`'s existing precedent for the same type) and
rewording the doc comment's one prose use ("share a grid cell" ->
"share a grid position") rather than waiving prose. `brattice.rs` stays
out of the inventory at 0 counted tokens.

**Test evidence:** `cargo test -p hornvale-worldgen --lib brattice::` 16
passed (was 14; +2 new); `--lib circuit_readout` 1 passed; the full crate
suite 494 lib / 3 / 348 integration / 0 doctests, all green (110 ignored,
6 ignored, as before); `cargo test -p hornvale --test suite -- lexicon_guard`
4 passed; `cargo fmt --check` clean; `cargo clippy -p hornvale-worldgen
--all-targets -- -D warnings` clean; `type-audit -- check` rc=0.

**`make rebaseline`**: rc=0, wall 216.320s. Diff: only `docs/audits/
underworld-circuit-seed-panel.md` (seed 42's three lines above) and
`docs/timings.md`/generated-path-writes bookkeeping. **The four Crosscut
numbers and the three descent counts (874 / 1,681 / 1,266) did NOT move
this round** on any seed - verified line by line against the pre-fix page,
matching the branch table's expectation that nothing here touches the
graph. No STOP triggered. `clients/game/core/tests/fixtures/` and `book/`
untouched.

**Commits:** `3eaf2d579` fix(worldgen): the round trip is now proved, not
just the forward reach (Ruling F); `3ee0747c1` chore(timings): record the
Task 2 fix round 1 gate-commit runs. `gate-commit` on `3eaf2d579`: rc=0,
wall 152.748s, 1061/1061 sub-floor tests passed (the first attempt, before
the lexicon fix, was rc=2, correctly refused by
`lexicon_guard::no_vertex_sense_cell_comes_back`).

**Not resubmitting the stage gate this round** - the controller does.

## Task 3 - complete

**What was built.** The plan's gates became places in the rock.
`LevelCellKind` gained `Threshold`, `Deep` and `Drop` (spec 3.5 wording, each
a place or a substrate, never an object); `movement_mode` answers `Walk` for
a threshold and a drop and `Swim` for deep water, which is the first thing
in the tree to reach The Gallery's reserved `Swim` variant. `Level` gained
`thresholds: Vec<(usize, usize, Cell)>` - the underworld's
`Lattice::doorways`, one entry per `Passage` edge, sump or not, because that
is where a door Thing will anchor in Task 5. `generate_level_with_origin`
stamps the crossing cell of every passage, turns a sump's whole
rock-before-the-carve L into `Deep`, writes a chute's lip as `Drop` and its
landing as standable `Floor` with no `StairsUp` beneath. The wire
(`vessel/level/v1`) gained the three palette kinds; the session's `map` verb
gained the three glyphs; the client gained the six glyph constants, their
twins, and a `"door"`-kind mark drawn as `+` before anything emits one.

**A prior attempt was interrupted mid-edit and most of it survived.** The
crash left seven modified files and no commit. Read hunk by hunk against the
brief: every hunk was KEPT. CHANGED: one clippy fix
(`!landing.is_some_and(..)` -> `landing.is_none_or(..)`, `underground.rs`),
and `is_placed_way`'s rustdoc, which asserted a specific pre-existing defect
on the strength of a single anecdote - see the measurement below. DROPPED:
nothing. The draft never ran a full suite, a lint pass, the client gate or a
rebaseline; all of those are this attempt's.

**The witness sweep.** `the_realization_witnesses_exactly_what_the_plan_
stamped` runs 200 seeds x 3 (cave kind, character) pairs = 600 plans and
every level of each, asserting in both directions: one recorded crossing per
passage, of the kind the gate asks for; the `Threshold` count equals the
non-sump passage count; a chute is a `Drop` over a standable non-stair cell
and every other stairway still pairs `StairsDown`/`StairsUp`; the `Drop`
count equals the chute count. Three positive controls fail the test if the
sweep observes no passage, no sump or no chute. 18.3 s.

**A pre-existing defect the witness exposed, measured before it was fixed.**
`place_stair`'s connector L paved over ways already placed. At HEAD
(`9ddc9b026`), a probe over Karst/`DrowTier`, vertex 1, seeds 0..200 found
**23 broken stairways** - the first seed 10, level 1, where the stairway
`36 -> 40` at `(2, 2)` stood as `Floor` with an orphan `StairsUp` beneath it
on rung 2. It went unobserved because
`stairs_pair_by_coordinate_across_adjacent_rungs` sweeps `Fracture`/
`WildCave` alone. Fixed by one shared predicate, `is_placed_way`, behind the
three call sites that each had their own list or none
(`connect_cells_preserving_ways`, `reconnect_region`,
`shortest_route_within_rect`); the witness's stair half now pins it across
three engine pairs. The probe was temporary and is not committed.

**Ruling: a sump's crossing cell is `Deep`, not `Threshold`** - an execution
amendment to spec 3.5's "every passage has one `Threshold`" - because one
cell has one kind, and a one-cell corridor's sump would otherwise realize no
`Deep` at all. `Level.thresholds` still records a crossing for every
passage. Cost if wrong: a door Thing anchored at a sump's crossing stands in
water, and the count assertion (b) would need restating.

**Ruling: the chute's DOWN half of the walk lands here, not in Task 4** -
`underground.rs::descend` gained a `Drop` arm and `session.rs::take_stairs`
accepts a `Drop` for `down`. Not scope creep by choice: measured. With those
two hunks reverted, `session::tests::a_cross_floor_cycle_is_walked_down_
along_and_back_up_another_stair` fails (rung 0, expected 1) because seed 42's
cross-floor realm now carries a chute, so `down` refused. Spec 3.5 says
`down` takes a chute and that it is free for everyone. Cost if wrong: Task
4's brief writes these same two arms (its steps 4 and 5) and must reconcile
rather than add - the locomotion parameter, the `Fly` up-half,
`NO_WAY_UP_REFUSAL` and `underground_footing_word` are all still Task 4's,
untouched here.

**Deferred minors taken** (both from The Crosscut): the terminus write now
carries two `debug_assert!`s naming the cause of a skipped terminus stairway
(one is the deferred minor exactly, the other its Brattice sibling - the only
footing left being a chute's landing, which the terminus write now avoids
consuming); the `CELLULAR` obituary rustdoc moved off `Algorithm` to
`carve.rs`'s module doc. `unlinked_neighbours_keep_their_wall` passes
UNMODIFIED. `every_walkable_cell_is_reachable_from_every_other` compared a
kind list, so it was widened to `movement_mode(..).is_some()` - the
resident's view - and its doc says so; the same widening landed in
`every_character_engine_keeps_every_level_connected`, the suite's
`standable_cells`, and the session's test-side route helper (minus `Swim`
there, since those walks possess walking bodies).

**Fixture branch: the FIRST arm - no fixture differs.** Verified rather than
assumed: no committed session fixture holds an underworld `level` document
at all (`session-seed-42-turn-0`, `-chamber` and `session-seed-14-carrying`
carry a chamber-band `plan` whose palette's `threshold` is the building
lattice's own `CellKind::Threshold`, unrelated). Spec 3.5 predicted the
middle arm conditionally, "if a snapshot holds an underworld level"; none
does. `make rebaseline` rc=0 in 440.591 s moved exactly two files:
`docs/audits/type-audit-report.md` (bare-ok(index) 243 -> 244, vessel 480 ->
481: the one new tag on `Level.thresholds`) and the `docs/timings.md` row.
No STOP triggered.

**Gates.** The vessel crate's own suite: 620 + 388 passed, 0 failed
(361 s + 526 s). `fmt --check` clean; workspace clippy with `-D warnings`
rc=0; `type-audit -- check` rc=0; `make clients-check-run` rc=0 (102
hornvale-game-core tests, including the new
`a_door_mark_draws_a_doorway_and_every_other_mark_draws_the_mark_glyph` and
the widened `glyph_of_matches_the_shipped_vocabulary`, whose collision check
now covers all nineteen glyphs in the band).

## Task 3 - fix round 1 - complete

Seven items from the task review; all seven addressed, none open.

**Ruling: G — the STAIR-FOOT connector paves through `Deep` while the two
repair sites still never overwrite it — because a foot connector runs inside
one region's rect and so cannot reach the divider, which means the sump's
crossing cell (the thing that makes the gate a gate) is out of its reach and
only the run's inside-the-region tail is shortened; without it a stair whose
connector L crossed a sump's run stood behind a `Swim` requirement THE PLAN
NEVER STAMPED — cost if wrong: a sump's run is one or two cells shorter on
the levels where a stairway crosses it, and a body that could swim loses a
stretch of water it might have swum.**

Implemented as a sibling predicate, `is_placed_way_for_a_foot` =
`is_placed_way` minus `Deep`, used by `connect_cells_preserving_ways` (the
foot connector's only caller is `place_stair`). `reconnect_region` and
`shortest_route_within_rect` keep `is_placed_way` unchanged: they are
re-stitching a region, not making a foot, and there a sump's run is scenery
to route around.

**The defect was real and the new arm catches it — red first, then green.**
With `is_placed_way_for_a_foot` temporarily defined as plain
`is_placed_way`, the witness fails:

```
seed 13 Karst/DrowTier level 0: the way at Cell(30, 14) (Some(StairsDown))
reaches no Floor/Flooded cell of its own region Rect { x: 26, y: 8, w: 8, h: 8 }
without swimming
```

With the ruling in place it passes (22.13 s). That is the walker arm the
review asked for, proven live rather than assumed.

**The witness gained two arms** (spec §3.5, both inside the existing 200-seed
x 3-engine-pair sweep):

- **(d) the `Deep` count arm** (review Important #2): a level none of whose
  passages is a sump has zero `Deep` cells. One pass over the grid, and it is
  the cheap statement of "no unstamped swim".
- **(e) the walker arm** (Ruling G): for every `StairsDown`, `StairsUp`,
  `Drop` and chute landing, a BFS over `Walk`/`Wade` cells ONLY — never
  `Swim` — confined to that cell's own region rect, must reach a
  `Floor`/`Flooded` cell. `Swim` is excluded deliberately: both connectivity
  sweeps ask `movement_mode(..).is_some()`, which `Deep` satisfies, so a
  stair behind deep water reads as connected to both of them. A positive
  control (`feet_walked > 0`) fails the test if the arm never walks.

**Important #3 - three stale wire-vocabulary docs**, all updated to the
eight kinds / sixteen glyphs: `LevelPaletteEntry::kind` in
`windows/vessel/src/level_doc.rs` (which now also names `entry_for` as the
one place the strings are minted and the test that pins them),
`LevelPaletteEntry::kind` in `clients/game/core/src/schema.rs`, and
`clients/game/core/src/level.rs`'s module doc, which said "ten-glyph ...
five kinds" eight lines under a table listing eight.

**Important #4 - the sim-side wire-string test.**
`the_three_brattice_kinds_get_their_own_wire_strings` (`level_doc.rs`, beside
`stairs_up_gets_its_own_kind_string`) pins `"threshold"`, `"deep"`, `"drop"`.
Its doc says why the sim side is where this belongs: the client draws an
unrecognised kind as rock and says nothing, so a typo would ship as a wall
where a squeeze is with the client's own vocabulary test still green.

**Minor #5** - `peek_stairs`'s contract paragraph now states the `Drop` case
(descend), states that a chute is ONE-WAY there because the landing carries
no `StairsUp`, and its refusal paragraph names the `Drop`-over-no-landing
case. **Minor #9** - the witness's chute-landing assertion is tightened to
`Floor | Flooded`; `Threshold` was dead, since the landing is written after
the passage loop. **Minor #10** - the passage loop's plain `connect_cells`
now carries a comment saying that a later passage's L may pave an earlier
one's `Threshold` or `Deep`, that this is chosen (paving only makes a cell
more passable; preserving would let one edge's gate stand in another edge's
corridor), and that the witness's count arms are what forbid destroying a
gate outright.

**Gates.** Vessel: **621 + 388 passed, 0 failed** (131.1 s + 332.0 s) - one
more lib test than the round before, the new wire-string test. Scoped runs:
`--lib underworld_level` 24/24 (8.17 s), `--lib level_doc` 15/15,
`--test suite -- underworld_level_generation` 3/3 (22.66 s). `fmt --check`
clean; workspace clippy `-D warnings` rc=0; `type-audit -- check` rc=0;
`make clients-check-run` rc=0 (102 core tests). `make rebaseline` rc=0 in
248.365 s: **no fixture moved** (the first arm of the branch table again, for
the reason proved last round - no committed fixture holds an underworld level
document) and this time not even the type-audit report, since no pub boundary
changed. Only the `docs/timings.md` row.

#11 [G5, controller ruling during Task 3] — A stair foot behind water ·
**Ruling G: a stair's foot connector paves through `Deep` and preserves
`Threshold`, `Drop` and stairs; the witness walks to every stair, lip and
landing without swimming and asserts no `Deep` on a sump-free level** · Why:
Task 3's `is_placed_way` consolidated three notions of "already a way" and
included `Deep`, so a stair whose foot connector crossed a sump's run kept a
`Deep` cell in the L and the stair became walker-unreachable — a Swim
requirement the plan never stamped, invisible to both connectivity sweeps once
they read `movement_mode(..).is_some()`; the fix reverted reds the new arm on
seed 13 Karst/DrowTier level 0 at (30, 14), so the guard is live · Discarded:
preserving `Deep` and routing the connector around it (a connector confined to
the region may have no route, and the stair then has no foot at all); making
the sump's crossing a `Threshold` (rejected at ledger #9 (ii)) · Ideonomy:
none — a ruling on a measured defect · Capture: spec §3.5 amended; the review's
deferred minors: `peek_stairs`' Drop refusal branch has no unit test; the
terminus fallback can overwrite a chute landing when a region's only walkable
cells are landings (debug-asserted and witnessed, silent in release);
`underground_footing_word` reads `Deep` as "dry" until Task 4 owns the words.

## Task 4 - complete

**What was built.** The walk now reads the body that walks it.

- **`hornvale_species::Locomotion { swim, fly }`**, `WALKER`, and the sparse
  `locomotion_registry()` — nine rows (six swimmers, three dragons), the exact
  shape of `habitat_realm_registry`: absence IS the default, and no kind is
  authored with both modes. `impl Component for Locomotion {}` like its
  neighbours.
- **`Body::locomotion()` is an ACCESSOR** (ledger #9 (i)), not a stored field:
  a pure function of `species`, so the ~43 `Body { .. }` literal sites across
  seven files are untouched. The deviation from the threaded-at-derivation
  pattern beside it is stated in the method's own doc.
- **`Underground::admits(cell, &Traverser)`** — the actor-aware seam BESIDE
  `movement_mode`, not a widening of it. `peek(dir, who)` calls it where it
  called `!open(target)`; the corner rule's oracle still asks `movement_mode`
  alone, because a threshold is an opening and a sump is a hole in the rock
  whether or not this body can take either.
- **Three refusals**, each naming its own physical reason:
  `UNDERGROUND_DEEP_WATER_REFUSAL`, `UNDERGROUND_LOCKED_DOOR_REFUSAL`,
  `NO_WAY_UP_REFUSAL`. All three `pub(crate)` (the older ones are private) so
  the session's own walk test can pin the exact sentence rather than a
  substring probe.
- **`threshold_edge` / `has_door`** read `Level.thresholds` and
  `plan.gate_between`, which is what took `#[allow(dead_code)]` off
  `Underground::plan`. A door is a fact about the plan's EDGE: every passage's
  crossing is a `Threshold` whether gated or not, so the cells cannot answer
  it and never will.
- **The chute's verbs.** `down` on a lip narrates "You let yourself down the
  chute."; `up` from beneath one narrates "You fly up the chute." for a flier
  and refuses a walker with the lip overhead. The stairs' sentence is
  untouched.
- **The footing words** (Task 3's deferred minor): `Deep`, `Threshold` and
  `Drop` no longer read as "dry".

**The reconciliation with Task 3's arms.** Task 3 had already landed the DOWN
half of the chute, because a Crosscut cross-floor walk test needed it. Nothing
was re-added:

- `peek_stairs` gained `loc: Locomotion` and one new arm — `Some(_) if rung > 0
  && descent[rung - 1][cell] == Drop` — placed AFTER the three kind-driven arms
  so a stairway under a chute keeps its own meaning, and BEFORE the catch-all
  that used to swallow it. Task 3's `Drop` (down) arm is unchanged in behaviour;
  only its comment moved, because it said `up` "simply finds no `StairsUp` and
  refuses", which was exactly true then and is now the case Task 4 completes.
- `peek_stairs`'s contract paragraph carried the same claim in prose ("a chute
  is ONE-WAY here"). Rewritten to say what is now true: one-way for a WALKER,
  and the fact that a chute is overhead is read from the rung ABOVE, since the
  landing itself is indistinguishable floor.
- `Session::take_stairs` already accepted `down` on a `Drop`. It gained
  `chute_above`/`by_chute` for the `up` half and the two chute sentences; its
  "no stairway up from here" refusal still fires only when the cell offers no
  up at all, so `down` still never means up.

**TDD evidence.** Species: the sparse-store test failed to COMPILE
(`cannot find function locomotion_registry`, `WALKER not found in this scope`),
then passed. Walk: the three `underground.rs` tests failed to compile with 18
errors naming `Traverser`, `has_door`, `admits` and `peek_stairs`'s arity, then
passed. Session: `the_chute_and_the_sump_read_the_body_that_walks_them` and
`each_live_footing_kind_reads_as_its_own_sentence` were written against the
finished seam and are the acceptance, not the drive.

**Gates.** `hornvale-species` 45 lib + 33 suite, 0 failed. `hornvale-vessel`
626 lib (72.0 s) + 388 suite (146.0 s), 0 failed. `hornvale` (cli) 102 + 54 +
294, 0 failed. `cargo fmt --check` clean; workspace clippy `-D warnings` rc=0;
`type-audit -- check` rc=0; `placement-audit -- check` rc=0. `make rebaseline`
rc=0 in 132.098 s: the only artifact that moved is
`docs/audits/type-audit-report.md` (+2 `bare-ok(flag)`, species 69 -> 71), which
is exactly the two new tags — a pub boundary changed this time, unlike Task 3.

**Ruling H [Task 4, in-task]** — The footing word had to serve two callers and
one of them breaks on a phrase · **The single `underground_footing_word` becomes
`underground_footing_words`, returning `(phrase, label)`** · Why: the brief's
three strings ("a narrow squeeze", "the lip of a chute") read correctly in
`describe_underground_here`'s "The rock here is ___." and are ungrammatical in
`underground_nouns`, which used the SAME word as the noun a player types and as
an adjective ("a narrow squeeze rock — the footing of this passage"). One
function returning both keeps them one decision: a second `match` on the same
cell kind could drift, and a kind added to one table and forgotten in the other
would read as dry footing under a typeable name that no longer fits · Discarded:
single adjectives for all five kinds (dry / flooded / drowned / pinched / sheer
— grammatical everywhere and typeable, but it throws away the brief's strings
for a constraint the brief did not know about); leaving the noun caller
ungrammatical · Cost if wrong: two words per kind instead of one, in one
function, pinned by `each_live_footing_kind_reads_as_its_own_sentence`, which
asserts the SENTENCE and the `examine` label and that the five phrases are
pairwise distinct · Capture: this ledger; the function's own doc.

**Parked finding — the ways-on report is geometric, and now that is
observable.** `underground_ways_from_cell` lists a neighbour whenever
`movement_mode(..).is_some()`, the same oracle the corner rule uses. Before
this task every listed way was walkable by everybody, so the report and the
walk could not disagree. They can now: `look` lists a sump east, and `go e`
refuses a walker with deep water. Left as is deliberately — a player can SEE
water and a door, so a report that hid them would be lying in the other
direction, and the report is about what the rock offers rather than what this
body can take. Named here because it is the "prose contradicting behaviour a
player can observe in one turn" shape the Gallery's own fix round 1 cared
about, and Task 5 (doors' openness fold) is the natural place to decide it
properly.

**Deferred minor — `docs/audits/lexicon-inventory.tsv` raised twice.**
`session.rs` 620 -> 648 and `underground.rs` 156 -> 236. Every new token is
the AREA sense (an underworld level's grid square), which is what the
inventory records; the guard reddened and the ceilings were raised by hand
rather than by `HV_LEXICON_REBASELINE`, which would have rewritten every row.

## Task 4 - fix round 1 - complete

Two Important findings, four minors and one named deviation, all doc-or-shape;
no behaviour changed except MINOR #3, which widened a guard the realizer never
exercises.

**Important #1 - the ways-on report's doc asserted a contract Task 4 broke.**
`underground_ways_from_cell`'s doc and its in-loop comment both said the
sentence "must report what `go` can do" — The Gallery's own fix for a
`look`/`go` disagreement, and an over-claim in the other direction the moment a
sump or a shut door existed. **Controller Ruling I: the BEHAVIOUR stays
geometric this task and Task 5 makes the report actor-aware.** Making it
actor-aware now would hide every plan-gated threshold in the descent, because
the door oracle answers `false` everywhere until §3.7's fold lands. So the doc
was qualified rather than the code changed: it reports what the ROCK OFFERS,
the two divergent cases are named (a sump beside a non-swimmer, a shut door),
the loop comment says `movement_mode` and never `admits` and why, and the same
note sits at the `door_open: &shut` closure so a reader arriving at either site
sees the other.

**Important #2** - `describe_underground_here`'s doc named the renamed
`underground_footing_word` and said the footing is "dry or `Flooded`". It now
names `underground_footing_words`, says five kinds, and says which half of the
pair this caller takes.

**Minor #3** - `by_chute`'s `up` arm excluded only `StairsUp`; it now excludes
`StairsUp | StairsDown | Drop`, with a comment saying it mirrors `peek_stairs`'s
arm ORDER rather than trusting the realizer never to place one of those under a
chute. The two now agree structurally, not by coincidence — which is the
concern Task 4's own self-review raised about itself.

**Minor #4** - the `unreachable!` message in `admits` carried ~18 stray spaces,
`cargo fmt` having joined a line continuation inside the string literal. One
clean sentence now; the spec citation moved out of the panic text into the
comment above it, where it was already stated.

**Minor #6** - `each_live_footing_kind_reads_as_its_own_sentence` deduped
phrases only. It now collects and dedups the typeable LABELS in the same pass,
so two kinds reading as different sentences while collapsing to one noun -
which would make one of them unexaminable while the prose looked fine - fails.

**Deviation, named** - the session's `MovementMode` narration folds `Fly` into
the `"step"` default while `admits` writes the same impossibility as
`unreachable!`. Kept, with a comment saying why: a narration that says "step"
for a mode nobody can be in is harmless; an ADMISSION that silently let an
unknown mode through is not.

**Gates.** `hornvale-vessel` 626 lib (65.0 s) + 388 suite (145.8 s), 0 failed —
including `underground::tests` (13/13), `each_live_footing_kind_reads_as_its_
own_sentence`, `the_chute_and_the_sump_read_the_body_that_walks_them` and
`underground_ways_on_agrees_with_the_levels_real_neighbours`, which computes its
expectation with the geometric oracle and is therefore the test that would have
reddened had the behaviour moved. `fmt --check` clean; workspace clippy
`-D warnings` rc=0. `gate-commit` rc=0 (60.430 s, 1432 + 1387 +
1116 subfloor tests) after the lexicon ceiling for `session.rs` moved 648 ->
652: the four new tokens are the fix round's own comments, all the AREA sense
(a level's grid square), the same reason the round before raised it.

#12 [G5, controller ruling during Task 4] — `look` lists what `go` refuses ·
**Ruling I: the ways-on report stays geometric through Task 4 and becomes
actor-aware in Task 5, when the door oracle reads the ledger fold** · Why:
Task 4's `door_open` oracle answers `false` everywhere until §3.7 lands, so an
actor-aware report today would hide every plan-gated threshold rather than
show a shut door; The Gallery's own fix-round finding was the mirror image
("look says the only way on is out, go n proves that false"), so the
disagreement is a known shape with a known cost, carried one task rather than
papered over — the doc at `underground_ways_from_cell` and at the oracle's
call site now say so · Discarded: making the report actor-aware now (hides
doors); leaving the doc asserting the old contract (the review's Important #1)
· Ideonomy: none — a sequencing ruling · Capture: Task 5's dispatch carries
the requirement; the implementer's own Ruling H (the footing word as a
`(phrase, label)` pair, because a second caller uses it as a typeable noun) is
recorded in the Task 4 section and judged sound by the review.

## Task 5 — complete

**What it built.** A door and a key underground: an identity, a state, five
verbs and a mark.

- `domains/thing`: `"door"` joins `THING_KINDS` (alphabetically, after
  `"cave-mouth"`), with `kinds::DOOR`, an `EVERY_HANDLE` row, a
  `thing_registry` row (`display: "door"`, gloss "a leaf hung in an opening,
  to be shut against what is beyond") and its place in the frozen roster.
- `windows/vessel`: a `chamber_prose` row ("a door" / "A leaf of banded wood
  in the opening, hung to be shut.") and an `object_registry` row carrying
  `AffordsPassage`, `Openable`, `Lockable` — the cave mouth's properties plus
  the strongbox's lock, and deliberately NOT `Portable`.
- `windows/vessel/src/descent_thing.rs` (new): the plan-position identities,
  the folds over them, and the module doc that states §5's cost where the
  spellings are.
- `windows/vessel/src/thing.rs`: `located_in_place_fact`, `lying_at_place`
  (with `lying_in` now a thin wrapper over it), `set_lockedness_role` (with
  `set_lockedness` now a thin wrapper over it, and its "there should be no
  `_role` variant" paragraph corrected rather than deleted).
- `windows/vessel/src/underground.rs`: `enter_with_character`, the named test
  seam, with `enter` calling it — one body, not two that agree.
- `windows/vessel/src/session.rs`: the `door_open` oracle, the actor-aware
  ways-on report, `look`'s two new sentences, `examine`'s two new nouns, an
  underground arm in `take` / `drop` / `open` / `close`, and the door mark.

**The exact role strings** (save-format contracts from the first world saved
holding a descent key — spec §5):

```
region  descent/<vertex>/<level>/<col>.<row>
key     thing@descent/<vertex>/<level>/<col>.<row>/key
door    thing@descent/<vertex>/<level>/<col>.<row>-<col>.<row>/door   (lesser cell first)
```

Ordinal 0 for both. Each is written out as a LITERAL in
`roles_are_pure_functions_of_the_plan_position_and_order_the_door_cells`, so a
spelling change cannot be rebaselined — the discipline `thing_role`'s and
`cave_mouth_role`'s own pins already hold for their namespaces.

**The custody fact's shape, stated once, as shipped.** `take a key`
underground commits

```
subject:    derive_entity_id(Lineage { parent: None, role: "thing@descent/<v>/<l>/<c>.<r>/key", ordinal: 0 })
predicate:  located-in
object:     Value::Entity(<the driven body>)
day:        the session's WorldTime
```

preceded, only when the key is still latent, by the `instance-of` fact
`promote_role` commits for the same subject. `drop` posts the same predicate
with `Value::Text("descent/<v>/<l>/<c>.<r>")` — the REGION, never the cell.
That is the whole of §5: the subject is a function of the plan, so a change to
`underworld/plan/v1`'s draws or to `underworld/gate/v1/pattern`'s selection is
an epoch from here on.

**Ruling I closed (ledger #12).** `underground_ways_from_cell` no longer asks
`movement_mode`; it asks `Underground::peek` with the driven body's own
`Traverser`, the SAME call `step_underground` makes with the SAME door oracle
(`Session::door_is_open`, the `openness` fold at the session's day). So `look`
cannot name a bearing `go` refuses, by construction rather than by two
predicates agreeing. The corner rule keeps its geometric oracle, untouched, for
the reason `peek`'s own comment gives. Both docs (the method's and the oracle's
call site) were rewritten from "held deliberately for one task" to what they
now do. Cost, stated in the doc: a shut door is invisible in the ways-on
sentence — a player learns of it from `look`'s door clause and the chart's `+`.

**Test evidence.**

- `windows/vessel` lib 644 + suite 400, 0 failed. New: six session tests on the
  DrowTier fixture (`a_shut_door_is_named_by_look_absent_from_the_ways_on_list_
  and_refused_by_go`, `a_descent_door_without_its_key_refuses_in_its_own_words_
  and_writes_nothing`, `the_key_at_its_node_opens_the_door_it_fits_and_closing_
  does_not_relock_it`, `a_thing_dropped_underground_lies_in_the_region_and_is_
  taken_again`, `examine_answers_the_key_and_the_door_underground`,
  `a_lit_door_reaches_the_level_document_as_a_door_mark`) plus three in
  `descent_thing`.
- `underground_ways_on_agrees_with_the_levels_real_neighbours` was rewritten to
  compute its expectation with the ACTOR-AWARE predicate (independently of the
  code under test) and gained a positive half: a `Deep` cell is installed beside
  the cell stood on, and the bearing must leave the sentence AND be refused by
  `go` with the water refusal. Left geometric it would have kept passing while
  measuring nothing — the vacuous-agreement shape its own doc already warns
  about once.
- Whole workspace: `nextest run --workspace` 5183 passed / 0 failed (521.3 s);
  the workspace doctests rc=0; `fmt --check`, workspace clippy `-D warnings`,
  `type-audit check`, `placement-audit check` and `plumb check` (691 consts,
  0 undeclared) all rc=0.

**The fixture, and why it needed a production seam.** `Underground::enter`
hardcodes `Character::WildCave`, which gets no `worked` term in
`circuit::cycle_budget`. Measured while writing this: seed 42's first two open,
unbarred cave mouths hang **zero** doors as `WildCave` and **eight** across five
rungs as `DrowTier`. So §3.7's verbs are unreachable through the production
constructor. `enter_with_character` is the twin; `enter` calls it; there is one
body. The fixture searches seed 42's open, unbarred cave mouths in scan order
for a rung-0 gated threshold with a standable neighbour AND a rung-0 key node
with a standable cell in its region, and panics naming the widening if none is
found. First hit: **`Vertex(342)`**, rung 0, three doored thresholds
(`Cell(21,7)`, `Cell(25,12)`, `Cell(34,12)`).

**Stale prose corrected while the file was open.** `Session::take`'s doc named
`interior::pattern`'s `the-key-on-the-ledge`, a pattern that does not exist and
never did; the shipped one is `the-key-by-the-loom` in `Role::Loomroom`. The
correction is loud rather than silent because the fifty lines around it are a
measured defect report, and one unverifiable line inside a measured report reads
as measured too.

**The cost nobody had costed: a thing kind is not five data rows.** Adding
`"door"` to `THING_KINDS` makes `domains/thing::register_concepts` register a
CONCEPT (decision 0025's check-then-map rule), which cascades:
`hornvale_language::EPOCH_COHORTS` needs a new cohort (epoch 19, `&["door"]`,
appended — never seated in epoch 16 beside the other object kinds, which would
re-sort every concept that already has a proto-root assignment); the seed-42
world JSON golden moves because the registry is serialized into `World`; the
proto-root tables and the solitary-tongue lexicon gain a row each; and the
concept manifest, concept registry, dictionary, trope-coverage and trope-matrix
artifacts all move. Every one of those diffs is purely ADDITIVE — one line
inserted, one count incremented, nothing re-sorted — which is the accession
discipline working exactly as its module doc promises. Epoch 18's own comment
had already stated this cost for `brazier`; epoch 19's restates it, because one
instance reads as a one-off and two read as a rule. Accepted via
`make rebaseline-goldens` and `make rebaseline`, both diffs reviewed line by
line before commit; `docs/audits/lexicon-inventory.tsv` raised for
`session.rs` (654 -> 751) and `descent_thing.rs` (new, 65), every token the
AREA sense (a level's grid square).

**Ruling J** — *the door mark's salience is 2, MORE salient than an agent's 5,
which is the opposite ordering `FURNISHING_SALIENCE` (30) takes* — Why: a
furnishing is scenery a creature stands in front of; a door is structure, and a
level chart that dropped it because something stood in the doorway would be a
picture of a passage that is not there. Nothing observable turns on it today —
`clients/game/core/src/level.rs` draws marks in list order with no salience
comparison, so the ordering is exercised only by `level_of`'s `(salience, noun)`
sort, which decides wire BYTE order and not which glyph wins — Cost if wrong: a
future client that picks between two marks on one cell would hide a creature
standing in a doorway; the repair is one constant and the doc says so.

**Ruling K** — *`examine door` takes `chamber_prose::detail`'s ONE authored line
for the kind and APPENDS the state ("It is shut, and locked." / "It stands
open."), rather than the brief's three hand-written datums* — Why: the totality
gate (`tests/suite/kind_totality.rs`, G-b/G-c) exists to make exactly one
authored line per kind reachable and checked; a second sentence written at the
call site is the drift that gate refuses, and the STATE is genuinely not a
property of the kind — Discarded: three literal datums (drifts from the
registry), state-only datums (loses the authored line) — Cost if wrong: the
door's examine reply is two sentences where one was specified; a wording change
is a one-line edit in either place.

**Ruling L** — *`chamber_prose::detail_of_label` is a new label-keyed accessor
returning `Option`, beside the `KindId`-keyed `detail` that panics* — Why: a
thing read back off a committed `instance-of` fact is a `&str` borrowed from the
ledger and `KindId` holds `&'static str`, so such a label cannot become a
`KindId` at all; `noun` already carries this exact asymmetry for the same
reason. `Option` rather than a panic because a ledger label is a runtime string
no roster guarantees, where a `KindId` is from the authored roster and a missing
line there is the quiet failure `detail`'s own doc argues about — Cost if wrong:
a dropped thing whose kind has no prose is silently omitted from `look` and
`examine` rather than panicking; today no such kind can reach a descent floor,
since only `Portable` kinds can be carried and every rostered kind is prosed.

**Deviation, named.** `drop` underground refuses with
`NOWHERE_TO_SET_DOWN_REFUSAL` when the possession stands on a cell no plan
region covers — a divider between two regions. The brief did not anticipate the
case. Refusing beats posting a `located-in` whose place no fold can read back,
which is the exact silent loss that constant exists to prevent one band up.

#13 [G5, controller note during Task 5 — leads the G6 digest] — Registering
the `door` thing-kind appended concept-registry accession epoch 19 and moved
the seed-42 world golden and the proto-root tables · **Accepted as the
shape every new thing-kind produces (The Chattel's kinds did the same);
additive, read line by line by the implementer and to be re-read by the task
review** · Why: a thing-kind is a concept the registry names, and the
registry's accession log is append-only by design — this is not the spec §5
contract (a descent key's identity making the plan grammar save-relevant),
which is a second, separate save-format fact of this campaign, but it is a
committed-artifact movement and belongs beside it at G6 · Capture: this
entry; the Task 5 section's own record; the G6 digest's leading pair.

## Task 5 — fix round 1 — complete

All four findings fixed. `windows/vessel` 644 lib + 400 suite, 0 failed;
`hornvale` (the cli enforcement suite) 294 + 102 + 54 + 1, 0 failed; `fmt
--check` clean, workspace clippy `-D warnings` rc=0, `type-audit check` rc=0,
`plumb check` 691 consts / 0 undeclared.

**Important #1 (controller Ruling J) — one sentence, one producer.**
`take_underground` said `"There is no {typed} here."` where the chamber's
`take_from_the_ledger` says `"You see no {typed} here."` for the same question,
against `examine_underground`'s own stated rule ("BYTE-IDENTICAL to the outdoor
and chamber paths': two wordings for one question is exactly the drift this
campaign exists to remove"). The brief's literal loses to that discipline.

The fix is not a copy of the chamber's format string — that would have made
**ten** copies of a sentence that already existed as nine. `session.rs` now has
one producer, `nothing_here_named(typed) -> String`, and **every** site that
emits this sentence routes through it: `open_or_close` (chamber), `take` (both
the anchor-noun miss and `take_from_the_ledger`), `put_in`'s two holder arms,
`examine`, `examine_chamber`'s two arms, `examine_underground`, and the two new
underground arms. Output is byte-identical at every one, so no test moved. The
function's doc records why it exists: nine copies that happened to agree could
not have caught this, because a tenth is added by *writing* one, not by editing
one.

**Minor #2** — `open_or_close_underground` bound `_cell` from
`doors_adjacent` and then recomputed the cell from the bearing to feed
`door_role_at`. It now passes the returned cell through, so the door's id and
its role derive from one read — the seam `descent_thing.rs`'s own doc argues
for. The two arithmetic paths agreed today and were free to stop agreeing the
day `doors_adjacent` grows a filter; the comment says so.

**Minor #3** — `a_session_beside_a_door` called `enter_with_character` a second
time on a descent `a_worked_descent_with_a_door` had already built. Verified
identical before removing: same terrain handle, same vertex,
`terrain.cave_at(vertex)` (a pure derivation of those two), same seed, same
`Character::DrowTier`. The search now returns `(Underground, DoorFixture)` and
the session installs the object itself; `DoorFixture` carries only the four
places. **Byte-identical is exactly why it was worth removing**, and the
helper's doc says so: nothing could ever have observed the second build, so no
test would have noticed the day one of those five inputs stopped matching. It
also halves the descent generation each of the six door tests pays for.

**Minor #4** — `take_underground`'s doc said "the condition is latency". The
code's condition is IDENTITY: `latent_role` is `Some` whenever the thing IS
`key_here`'s answer, which stays true after a drop-and-retake. The doc now says
that, names the reachable sequence (take, drop, take), and states the cost — one
redundant `instance-of` on a later day, which `Ledger::commit` dedups within a
day and which is precisely the across-days duplication
`thing::set_openness`'s own doc records and calls harmless — plus why gating on
latency instead would be strictly worse (a second ledger read to decide
something the promotion is already idempotent about).

**Covering tests re-run** (all green): the six door tests
(`a_shut_door_is_named_by_look_absent_from_the_ways_on_list_and_refused_by_go`,
`a_descent_door_without_its_key_refuses_in_its_own_words_and_writes_nothing`,
`the_key_at_its_node_opens_the_door_it_fits_and_closing_does_not_relock_it`,
`a_thing_dropped_underground_lies_in_the_region_and_is_taken_again`,
`examine_answers_the_key_and_the_door_underground`,
`a_lit_door_reaches_the_level_document_as_a_door_mark`), the three in
`descent_thing`, and `underground_ways_on_agrees_with_the_levels_real_
neighbours`. Then the whole `hornvale-vessel` crate, because the refusal
producer touched nine call sites across it, and the whole `hornvale` suite,
because six of those nine are in verbs its enforcement tests read back.

**No test asserted the retired sentence**, checked by grep before and after —
which is itself the finding's point: the divergence was invisible to the suite
and visible only to a reader holding both files.

**One artifact moved.** `docs/audits/lexicon-inventory.tsv`'s `session.rs` row
FELL 751 -> 748 (the recomputed-cell block deleted three AREA-sense `cell`
tokens). A number may fall freely; refreshed through
`HV_LEXICON_REBASELINE=1` rather than by hand, and the lower ceiling is the
tighter one.
