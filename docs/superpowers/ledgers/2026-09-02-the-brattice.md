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
