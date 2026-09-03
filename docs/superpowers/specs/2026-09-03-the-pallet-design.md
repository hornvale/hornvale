# The Pallet — a body chooses where to sleep

**Campaign B** of the `MAP-one-kind-model` arc (The Wicket was A). Folds in
The Plumb's **Group A** per Nathan's triage of 2026-09-03.

## 1. What already exists, because it changes the scope

The Wicket built more of this than the campaign brief assumed, and checking
before designing is the difference between a five-task campaign and a
nine-task one:

| piece | where | state |
|---|---|---|
| `OfferedVerb::Sleep`, gated on `ObjectProperty::SupportsRest` | `affordance.rs:403,528` | **built** |
| `SiteGrade { Bare, Afforded }` and `gain()` | `liveness.rs:3139` | **built, two-valued** |
| `AFFORDED_REST_GAIN = 1.5`, multiplying `BoutKind::fall` | `liveness.rs:3110` | **built**, with a compile-time `> 1.0` bracket |
| `SLEPT` / `RESTED` predicates carrying spans | `liveness.rs:2500,2517` | **built** |

So the campaign does **not** build a grade. It builds the three things the
grade has no way to express, plus the folded-in Group A.

## 2. What is missing

1. **There is no choice.** `SiteGrade` is derived at line 3247 by asking
   whether *any* anchor in the room offers `Sleep` — a room-level boolean. No
   body ever picks an anchor, so "where did it sleep" has no answer even in
   memory.
2. **There is no record.** Nothing commits what a body slept on, so the tuning
   diagnostic Nathan asked for cannot be read back, and `windows/historiography`
   has nothing to narrate.
3. **The grade is one number for every species.** `AFFORDED_REST_GAIN` is
   tagged `universal(… not a species property)`. That tag is **wrong**, and
   wrong in the exact shape decision 0586 names: its reason is a provenance
   claim ("bounded rather than derived") plus a negation. A xorn gains nothing
   from a bed. The quantity is `per-species`.

## 3. The constitutional boundary, and what it permits

Decision **0069** makes an entity's persisted position its **room**; anything
finer "exists only inside the presence bubble and is never serialized", and
`interior/anchor.rs` says the same of anchors in its own words. So *"slept at
anchor 3"* is unrecordable by design and no part of this campaign attempts it.

The Wicket already made the durable half first-class: `Anchor { kind: KindId }`,
and a `KindId` is a registered concept and a stable string. **The kind is
recordable; the anchor is not.**

Verified against `kernel/src/ledger.rs` rather than assumed: `Value::Text` and
`Fact.place: Option<EntityId>` both exist, so a fact carrying a kind with a
room-granular place is legal today with no kernel change.

## 4. Design

### 4a. Selection is within-room, and never a reason to travel

Nathan, 2026-09-01: *"we don't want things to just wander out of the room to
find the nearest bed."* Selection therefore ranges over the anchors of **the
room the body is already in** and never proposes movement. A body in a room
with nothing to lie on sleeps on bare ground; that is the intended outcome,
not a fallback.

`select_sleep_site(interior, body) -> Option<AnchorId>` returns the best-graded
anchor offering `OfferedVerb::Sleep`, or `None`. `None` is `SiteGrade::Bare`
and is the road.

### 4b. The choice MUST be able to be wrong

Nathan: *"a creature sleeping in an unsafe or unrestful place would be a useful
indicator that something needs tuning."* A chooser that always takes the best
available site **can never produce that signal** — the diagnostic he asked for
would be defined out of existence by the implementation.

So the design deliberately does **not** guarantee a good outcome. Badness
arises from the world rather than from a randomizer: a room may afford nothing,
and the body sleeps where it is. The record (4c) is what makes it visible. **No
clamp, no retry, no search beyond the room.**

This is a real design constraint, not a caveat: any later change that makes the
chooser cleverer must preserve the ability to observe a bad outcome.

### 4c. What is recorded: `SLEPT_ON`, a new predicate

A **new** predicate `SLEPT_ON`, object `Value::Text(kind)`, `place: Some(room)`,
`day` the instant. Additive — `SLEPT`'s existing object (the span) is untouched,
so no save-format contract moves and no world file changes meaning.

Registered by the session, exactly as `SLEPT` and `RESTED` are — not at
genesis.

Two consequences fall out without further work, and they are the argument for
recording a kind rather than a number:

- `windows/historiography` is domain-agnostic by construction — it replays any
  entity's facts against the registry's predicate docs. Narrative is free.
- `windows/lab/src/health.rs` already reads `SLEPT`; the distress read gains
  the site with no new plumbing.

### 4d. The grade becomes `per-species` — the first rung

`AFFORDED_REST_GAIN` becomes a per-species table keyed by `KindId`, in
`domains/species`, following `fatigue_rise_registry`'s shape exactly (The
Wicket built it; The Plumb found it uniform). A default covers unlisted kinds,
as `DEFAULT_FATIGUE_RISE` does.

**And the campaign authors DIFFERENTIATED values, not a uniform table.** The
Plumb's headline finding is that The Wicket built the per-species mechanism for
`FATIGUE_RISE` and left every value identical — a mechanism without a
difference. Repeating that here would be the same defect with this campaign's
name on it.

The other two rungs are **declared, not built**: the per-people ordering needs
kind-to-kind edges (Campaign C) and the per-individual quirk needs
`Lineage`-derived values (Campaign D). Both are recorded as `plumb:` tags in
the committed roster — the mechanism decision 0586 exists to provide — rather
than as TODO comments.

**Preference is innate; safety is situational.** A body knows by instinct that
a bed out-rests a floor; it cannot know whether a given site is *safe* without
being there. Threat therefore stays **out** of the grade, for a later campaign
to supply as a separate term.

### 4e. Group A — the local-day family

Per Nathan's triage, The Plumb's four `per-world` findings land here because
they are the same code:

- `SCAN_LIMIT` (1.5 std days) and `ONE_DAY` (the give-up fallback) bound
  `next_awake_day` in **standard** days;
- `WAKE_SCAN_STEP` samples a locally-periodic signal at a fixed standard-day
  rate — ~3.3 samples per local day at the 4-hour legal minimum, under a doc
  claiming it catches a crepuscular creature's dawn band;
- `SLEEP_BOUT` is the same `TickSpan` shape `REST_BOUT` was.

Converting these **closes the inversion The Plumb shipped knowingly** (a rest
outlasting a sleep past ~96 hours of rotation) and therefore **deletes**
`a_rest_still_outlasts_the_sleep_scans_give_up_fallback_at_the_100_hour_legal_extreme`,
which is a running test asserting the current wrong ordering. Deleting it is
the success condition, not collateral: the test's own doc instructs it.

## 5. What this campaign does NOT do

- No movement toward a site (4a).
- No threat term in the grade (4d).
- No per-people or per-individual rung — declared as tagged findings.
- No new restorable stock; the grade still multiplies fatigue recovery only,
  as The Wicket's own note says.
- No anchor identity in any committed fact (§3).

## 6. Risks

- **A behaviour change moves byte-goldens invisibly.** The Plumb shipped a red
  branch on exactly this: `make rebaseline` cannot write a byte-golden and
  `gate-commit` does not run `affect_trace`. Every task that changes behaviour
  runs the byte-golden tests **by name**, and the plan says so per task rather
  than once.
- **Group A changes sleep spans on every world whose day is not one standard
  day**, so seed 42 moves. Expect goldens; adjudicate the move with a measured
  breakdown, as `affect_trace_golden.rs`'s three precedents do.
- **`SLEPT_ON` is a new registered predicate**, so the concept registry and its
  generated reference page move. That is an ordinary artifact regen, not an
  epoch — no seed-derivation label is added.
