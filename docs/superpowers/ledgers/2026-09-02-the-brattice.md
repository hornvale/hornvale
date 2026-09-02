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
