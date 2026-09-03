# 0597. The roster is a struct of arrays with one append

**Status:** Accepted (2026-09-02) · **Decider:** Nathan · **Campaign:** The Rack

In the context of a possession session that had accumulated three parallel
`Vec`s beside its body list and three scalar side-fields for the driven body,
facing a turn whose every question about who is here re-derived the answer
from the ledger, we decided that **the session owns one struct of arrays with
exactly one append**, accepting that the arrays are unserialized session state
whose agreement with the ledger is a discipline the tests must hold rather
than a property the types can.

## Context

`windows/vessel/src/session.rs` held `bodies: Vec<Body>`, `roll_keys:
Vec<RollKeyStatic>` and `on_roll: Vec<bool>` as three independently-pushed
arrays index-aligned only by convention, plus `driven_affect`, `driven_mode`
and `driven_suppressed` as three scalars that made the driven body a special
case of itself. Decision 0546 already requires a body's index to be stable for
the life of the session (`narrate_motion`'s positional zip and every
user-visible handle depend on it), so the alignment was load-bearing and
unenforced.

## The decision

1. **`Roster` (`windows/vessel/src/roster.rs`) owns six index-aligned
   columns** — `bodies`, `keys`, `on_roll`, `position`, `felt`, `written` —
   with a `BTreeMap<EntityId, Slot>` reverse index beside them and the driven
   slot named at construction.

2. **`push(body, key, felt) -> Slot` is the only append**, and it pushes every
   column in one statement, so an index misalignment is unrepresentable rather
   than merely tested for. `Slot(usize)` is a newtype and the only way to
   index a column; the columns themselves are private and handed out as
   slices.

3. **Every column declares whether it is a VIEW or CONTENT, and says so in its
   own doc.** `position` is a **view**: it must equal `agent_position(&ledger,
   body, day)` at every read, and a disagreement is a writer bug fixed at the
   writer, never papered over by re-folding. `felt` is **content** — a
   resolution, not a projection (decision 0596). `on_roll` is a mask derived by
   The Roll's own recompute; `written` is the append-versus-tick distinction
   `resolved_felt` reads.

4. **VIEW ≡ SCAN is pinned by a test after every verb class**, at two seeds and
   under possession. Zero disagreements is the only passing count.

5. **The column writers are named and few**: `push` (the append, seeding
   `position` from `home` and `felt` from one stateless read), `write` (the
   tick's per-body write-back of both columns), `resolve` (felt only), `place`
   (position only), and `set_on_roll` (the roll mask, replaced wholesale by the
   recompute that owns it). **The driven slot's `position` is written only by
   `place`, at the sites that commit the driven body's `agent-at` fact.**

6. **Nothing here is serialized.** The roster is FRAME-tier in its entirety,
   like `Occupancy` (decision 0069): the ledger remains the only stored truth
   (Penstock §3.1), and every column is re-derivable from the ledger plus the
   seed.

## Consequences

- **A turn's cost over present bodies becomes a scan of two dense arrays.**
  Measured at seed 42's flagship, per snapshot: **67 drive folds → 0** and
  **137 ledger position folds → 0** (the 137 is `colocated_npcs` folding twice
  per other body inside its filter, plus one for the vantage). `needs` fell
  from 27.559 ms to 0.022 ms.
- **The driven body stops being a special case.** `driven_affect`,
  `driven_mode` and `driven_suppressed` are gone; the driven slot holds a
  `Felt` like every other slot, which is decision 0227's "possession selects a
  body" made structural.
- **`place` exists because the tick is not the only writer.** `go`/`retrace`
  move the driven body through `commit_agent_at` without arbitration; with the
  tick as the sole writer the driven slot named the room just left, and the
  VIEW ≡ SCAN test went red on it. `place` does not flip `written`, because a
  move is not a resolution.
- **The invariant test is only as wide as the writers it exercises.** The first
  version of the driven write took `position` from the driven body's solo walk
  while `wait` discards that walk's facts — so under an imposed controller
  (possession) the column could disagree with the ledger, and the test's script
  never possessed. Found in review, fixed at the writer, and pinned by a
  possessed-session test. The rule that follows: enumerate the *controllers*,
  not just the verbs.
- The two helpers over the body slice (`other_bodies`, `on_roll_others`) stay
  **free functions over slices** rather than becoming `Roster` methods: as
  methods they would reborrow the whole roster and collide with `wait`'s
  `&mut self.ledger`.

## See also

Spec §3.1–§3.3 (`docs/superpowers/specs/2026-09-02-the-rack-design.md`);
decision 0596 (`felt` is content); decision 0546 (the roster is append-only and
position-stable); decision 0227 (possession selects a body); decision 0069
(FRAME-tier state); the Penstock metaplan §3.1 and §6.6;
`windows/vessel/src/roster.rs`; `windows/vessel/tests/suite/the_rack.rs` (VIEW ≡ SCAN, P3);
`docs/superpowers/ledgers/2026-09-02-the-rack.md` entries #1, #3 and the Task 3
review; `book/src/chronicle/the-rack.md`.
