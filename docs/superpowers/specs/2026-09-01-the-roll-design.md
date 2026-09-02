# The Roll — a settlement's residents are a roll, and the walk ticks the ones on it

**Date:** 2026-09-01 · **Registry row:** `SOC-one-creature-per-settlement`
(`elaborated`, high) · **Program:** The Penstock metaplan
(`docs/superpowers/specs/2026-08-22-the-penstock-metaplan.md`) — this is
its stage 5, **the condensation boundary**, entered on need rather than on
the gate §6 wrote for it (see §2.4). · **Ledger:**
`docs/superpowers/ledgers/2026-09-01-the-roll.md`

Decision block: 0546–0555.

**Campaign complete (2026-09-02).** Merged; the chronicle is
`book/src/chronicle/the-roll.md`, the retrospective
`docs/retrospectives/the-roll.md`, and the amendments this document took
during execution are marked in place and recorded in the ledger.

A roll is two things at once: the register of who belongs to a place, and
the call that establishes who is present. This campaign builds both. Every
settlement gets a **roll of residents** — as many as its committed
population — and the walk ticks **the ones within call** of the possessed
body. Nathan's brief: *"a shit-ton more creatures instead of one per
settlement."*

## 1. The problem, measured

A possession session holds **seven bodies**, and the number is authored:

```text
  windows/vessel/src/session.rs:34   const NPC_COUNT: usize = 3;
  windows/vessel/src/session.rs:38   const WILD_COUNT: usize = 4;
```

`derive_npcs` (`liveness.rs:5868`) sorts settlements by population, keeps
the home settlement plus two, and mints **exactly one** body per settlement —
`Lineage { parent: village.id, role: "npc", ordinal: 0 }` (`liveness.rs:5893`).
`derive_wild_npcs` (`liveness.rs:5943`) mints one body per mobile species at
that species' single densest attractor, top four by biomass
(`windows/worldgen/src/lib.rs:2483`). So a settlement of eighty holds one
creature, and a world-wide herd of woolly mammoths is one body at one vertex.

The world it stands in, from the committed census (1000 seeds,
`book/src/laboratory/generated/the-census/rows.csv`):

```text
  column             min     mean     max
  settlement-count    33    264.7     583
  mean-population    6.3     26.9    42.1
  total-population   445   7383.8   18757
```

About 7,400 abstract inhabitants, seven bodies. The measured consequence
(The Company, then The Hand): a fresh possession has **no other creature
present in 0 of 64 seeds**. Before The Hand the player always stood beside a
twin of their own body, and that artifact was the entire supply of company.
`book/src/open-questions.md` carries the finding; the registry row carries
the numbers.

**Two things the registry row says are stale, and this spec corrects them.**
It quotes The Penstock's *pre-Scour* tick cost — "superlinear, 2.17 across
100–200 agents." The metaplan's own §6.3 records the post-Scour state:

```text
                       before (2 runs)    after The Scour (2 runs)
  fitted slope          1.43 / 1.52        1.12 / 1.11
  tail, 100->200        2.17 / 1.99        1.41 / 1.29
  ms/tick @ 200 agents  5722.7             1752.2 / 1712.6
```

The cost is no longer mainly superlinear. It is a **constant**: 8.76 ms per
agent per tick at 200, 5.75 at 10 (§6.4), which puts roughly 100–170 bodies
inside a one-second tick before this campaign touches anything. The residual
superlinearity has three named sites, all in the tick's own code
(§6 below). The second stale claim is the row's implicit model that the
count is a *world* constraint; The Tableau's retrospective already names it
a scale compromise that was read as a ruling.

**And bodies from one settlement are copies.** `body_at` (`liveness.rs:5774`)
derives every field from the settlement's `peopled-by` species and the
settlement's own position: same `home`, same `resource`, same three mind
dials, same boldness, same label (`"{species} of {village}"`,
`liveness.rs:5838`). Deriving eighty of them today would produce eighty
identical creatures making identical decisions, and the chart legend would
dedupe them to one noun (`purview.rs:210`). `PSY-individual-deviation`
records this as "every member is currently identical."

## 2. What must survive

**2.1 Determinism, and identity by lineage.** Same seed and pins,
byte-identical worlds and artifacts. A resident's identity is its lineage
(decisions 0051, 0127), minted through `Ledger::reuse_or_mint_entity`, whose
own doc names this exact case: *"the herder of a settlement is the same
herder in every session, so re-deriving must find it."* Today's one body per
settlement is ordinal 0 of role `npc`; **it keeps that lineage and therefore
its `EntityId`**, and the driven body at possession start is still that
entity (decision 0227, possession selects a body). A saved session from
before this campaign reloads and *gains* residents; it does not lose or
renumber the one it had.

**2.2 Coarse constrains fine.** The settlement's committed `population` is
the coarse truth; the roll refines it to exactly that many residents and
never more. A wild concentration's `HeadcountRender` (`Count(n)` / `Lone`)
is the coarse truth for fauna; the roll instantiates exactly `n` (or one).
Nothing at body resolution may contradict a number the ledger already
committed. Death is the one fine-tier event with a coarse shadow, and it
has none to contradict: `population` is documented as the occupation's
*peak*, not a live count (`windows/worldgen/src/lib.rs:12566`).

**2.3 The working set is visible, and therefore pure.** The Penstock's spine
(§2b): which entities are *simulated at all* is observable, so that boundary
must be a pure function of world state — observer position, `WorldTime`,
seed — never memory, query history or wall time. This campaign builds
exactly that function and nothing else in the metaplan: no view cache, no
eviction, no fan-out. Those are invisible-half work with their own gate.

**2.4 Why stage 5 opens before stages 2–4.** The metaplan gated the
condensation boundary on "stages 2–4, plus a Lab study design," and then
§6.5 found stage 2 not enterable (ledger reads are 0.04% of a tick). The
program's own falsifier (§11, first bullet) says that when query cost is a
minority, stages 2–4 are optimising noise and the program should say so.
It did. What remains of the metaplan that is *needed*, as opposed to
*planned*, is the visible half — because without it there is no way to
have more creatures than the tick can afford, and the tick affords about a
hundred. That is a need, and it is the gate this campaign enters on. The
Lab study the metaplan wanted alongside — how faithfully a field stands in
for the bodies it does not instantiate — is preregistered here in §8 in its
minimal form and left as a follow-up in its full form.

**2.5 Layering, no new dependencies, quantize-at-emit, no fine position
serialized (0069), one body type (0229), models author dice roll.**
Unchanged and binding.

## 3. Design: three tiers, one roll

```text
  tier   what it is                        cardinality            where it lives
  -----  --------------------------------  ---------------------  ----------------------------
  0      the field (demography)            continuous             DemographyReport, unchanged
  1      a RESIDENT: a person, an entity   = settlement.population per settlement;  session ledger,
                                             = HeadcountRender per wild attractor   derived on demand
  2      a BODY: a ticked resident         = the roll: residents within call   Session.bodies
  3      the possessed body                1                      Session.driven, unchanged
```

**3.1 Tier 1 — residents.** `derive_residents(world, ctx, ledger, settlement)
-> Vec<Body>` mints, for `i in 0..population`, the entity
`Lineage { parent: settlement, role: "npc", ordinal: i }` through
`reuse_or_mint_entity`, and commits on the *session* ledger, once per
resident, the facts that make it a person:

- `is-person` (the predicate `domains/person` already registers, line 18);
- `person-born` with a drawn birth day (§3.4) so the species' life history
  applies to it the way it applies to a founder;
- its name (§3.5); and
- nothing else. Where it lives is its lineage (parent = settlement); what it
  is is the settlement's `peopled-by` species, read the way `body_at` reads
  it today.

A resident is a **living person**. Founders (`hornvale_person`, minted at
genesis, all dead by `history-now` on seed 42) and residents (minted on
demand, alive) are one kind of thing at two times, and they share one
vocabulary: whatever a window learns to say about a person, it can say
about either. This is the "one word per concept" rule applied before a
second word gets coined (ledger #3). It is also where `PSY-expertise-per-
individual`'s per-person components will attach when they are built; this
campaign builds the row they attach to and no component beyond §3.3.

Residents are **derived, not generated**. The world ledger does not change;
`world-seed-42.json`, every almanac, every census column and the whole
`book/src/` gallery stay byte-identical, and §8's M4 asserts it. The
derivation is a pure function of (world, settlement) and any window may
call it — the almanac could list a village's residents tomorrow — which is
what "sim first, game as lens" requires of a thing the player can meet.

**3.2 Tier 2 — the roll.** One named, pure function:

```text
  roll(world, observer_room: Facet) -> Vec<RollEntry>
    -- `t: WorldTime` was in this signature until Task 7 found membership is
       time-invariant by construction (a body's HOME decides it, §3.2 below),
       so the function is strictly purer without it; ledger #16/#17 and the
       Task 7 review record the amendment.
    = every resident of every settlement whose room is within R walk-band
      hops of observer_room,
    + every wild body of every stack attractor within R hops, headcount-
      rendered per species (Count(n) -> n bodies, Lone -> 1, Colony -> 0,
      marine excluded exactly as wild_concentrations_from excludes it today),
    ordered by (hop distance ascending, parent EntityId ascending, ordinal
      ascending),
    truncated to the first B entries.
```

A wild body's identity is keyed by (species, attractor vertex, member
index), never by its position in a list, so a herd is the same herd from
every direction of approach; today's `derive_wild_npcs` keys by list
position and stays for the benches and the health battery. Membership of
the roll is decided by a body's **home** (its settlement's room, or its
herd's attractor), not by where it has wandered, so the roll is a function
of world state rather than of a body's own history.

`R` and `B` are two named constants in `windows/vessel`, with doc comments
stating that they are *budgets*, not world facts, and pointing here.
`Session::wait` derives its `bodies` from the roll at the observer's room
each tick instead of from `NPC_COUNT` + `WILD_COUNT`, which retire.
`derive_npcs(k)` and `derive_wild_npcs` **stay**, unchanged in signature and
meaning — the health battery (`windows/lab/src/health.rs:307`), the hearth
calibration (`hearth_population_calibration.rs:804`), `agent_scaling`,
`session_length_scaling` and `id_stability_under_insertion` all call them
with an explicit `k`, and "k settlements, one body each" is a legitimate
instrument shape. The roll is a new caller beside them, not a replacement.

The initial values are `R = 2` (a 5×5 window of ~1.1 km rooms) and
`B = 128`. **These are not predictions; they are the first setting of a
knob §8's M2 measures.** With settlements ~265 to a planet the window will
almost always hold at most one settlement, so the roll in practice is *the
residents of the settlement you are at, plus the local fauna* — mean 27,
flagship-sized 70–90 — and the budget is a backstop for a dense coast or a
large herd, not the normal case.

**3.3 Individuation.** Every resident draws, from its own stream (§3.6),
a deviation on each of the three mind dials a `Body` carries from its
species — `deliberation_latency`, `time_horizon`, `boldness` — as
`species_mean + spread × unit`, where `unit` is one uniform draw on
`[-√3, √3]` (matching standard deviation) and `spread` is the kind's
**existing** `Dispersion.mind` row (`hornvale_species::dispersion_registry`,
The Tolerance), clamped to `[0, 1]`. This is byte-for-byte the mechanism
`windows/worldgen/src/disposition.rs` already uses to draw a *settlement's*
disposition around its people's mean (`perturb`, `UNIT_SD_HALFWIDTH`); the
resident draw is its next rung, keyed by the resident rather than the
occupation. No new registry is added — an earlier draft of this section
said one would be, before the survey found `Dispersion` — and the coverage
tests that already ratchet the dispersion roster total over every minded
kind carry over unchanged. A timid goblin and a rash one are now possible;
a settlement's residents no longer move as one blob because their
deliberation and their tolerance for threat differ. Whether that
difference is *large enough to see* in the walk is a preregistered
measurement (§8, M5), and a null there is a finding, not a failure. The
fatigue rate is not perturbed: The Wicket's per-kind row is unmerged and
this campaign does not reach into it.

**3.4 Age.** A resident's birth day is drawn uniformly over its species'
lifespan, back from the session's start day, so the roll is a population
of mixed age rather than a cohort born the day the player arrived. It is
committed as `person-born` (§3.1). Nothing this campaign builds *reads*
age; the row exists so that the life-history machinery that already
exists for species has an individual to apply to.

**3.5 Names.** Residents of a settlement are named by
`hornvale_language::Namer::new(&world.seed, people, &phonology)` — the same
namer `person_promote.rs:316` already uses for founders — seeded per
resident from its stream (§3.6). A resident's `label` is its name, so
`examine <name>` resolves it the way `examine goblin` resolves the one
body today. Two residents of one settlement with one name are a silent
merge of two identities (the founder path's own post-condition,
`person_promote.rs:140`); the derivation asserts distinctness within a
settlement and, on collision, appends the ordinal rather than dropping the
resident, because a resident cannot be "unremembered" the way a founder
can — its count is the coarse truth (§2.2). Wild bodies are **not**
named: a herd's members are interchangeable, and they share the label
they have today; §4 groups them in prose.

**3.6 Streams.** A resident's draws (deviation, age, name) come from one
`Stream` per resident, salted from the world seed by the resident's
lineage (parent id, ordinal) under one new label declared in the deriving
crate's `streams` module and published through `stream_labels()` into the
generated manifest. The stream is fresh — no existing consumer's draw
order changes, which is what keeps every committed world byte-identical.
Consumption order *within* the resident stream is a save-format contract
from the day it lands: name, then birth day, then the four deviations, in
that order, pinned by a test.

**3.7 Dormancy.** A resident not on the roll is not ticked. Its state is
whatever facts it has committed — position by its latest `agent-at`, else
`home`, exactly as `agent_position` reads today. When it re-enters the
roll, `catch_up` (`liveness.rs:4632`) replays it forward from its last
committed day under the existing `CATCH_UP_STEP_CAP`, which is the
mechanism a sleeping body already uses. **This is a fidelity choice, and
the G3 package leads with it**: a villager the player leaves for thirty
days is, on return, where they were thirty days ago, plus at most one
thousand steps of catch-up. The alternatives — a slow tier that grants
off-roll bodies one decision per day, or advancing them by the field —
are recorded as a registry row (§10) and not built; the spectrum between
"frozen" and "ticked" has an unnamed middle and this campaign names it
without occupying it.

**3.8 The driven body.** Unchanged: possession selects ordinal 0 of the
home settlement (0227), the same entity as today. `PossessTarget::Creature`
already lets a session possess any entity; with residents named, it can
now possess a named one.

## 4. Presence at scale

The dark version of "more creatures" is a `look` that lists eighty goblins.
The snapshot's `sensed.present` (`snapshot.rs:179`) stays **one entry per
body** — the wire carries data, and a client may count, sort or hide as it
likes (0022). The prose is where scale is handled:

- Today `look` names nobody at all: the co-located roster reaches the
  player only through the wire, the chart marks and the verbs (found while
  planning; an earlier draft of this section said "as today"). So the
  presence line is new prose. Up to a small threshold `N_NAMED` (initial
  value 4, a named constant), present creatures are listed by label.
- Beyond it, prose groups by kind with a count and names the first
  `N_NAMED` in roll order: *"Gribble, Tosk, Marn and Ulla, and eleven more
  goblins of Googo; a herd of nine giant elk."* The count is the roll's,
  never an estimate.
- `examine <name>` resolves against the whole co-located roster, as today.

**Chamber seating.** Indoors, `Session::sighting` seats one body per cell by
anchor kind and keeps the rest in `sensed.present` unseated
(`session.rs:6263`, the fourth documented absence). A crowd larger than the
chamber's anchor cells is therefore *present and not drawn*, which is what
happens today for a surplus and is not a regression. The prose says so —
"and eleven more" — rather than letting the count and the
picture disagree silently. **Amended at close (Task 9's review, ledger):
this paragraph wrote a DISTINCT indoor wording, "and eleven more, about the
room", that no task carried and that the implementation does not render.
One wording stands on both bands**, which is The Sighting's "same sentence
on both sides of a doorway" contract applied to a new line; the cost, stated
rather than hidden, is that an indoor reader is not told the uncounted are
unseated, which the chart already shows by not drawing them. Widening a
settlement beyond one room and one
structure is a real question (`liveness.rs:6125` says so in its own words)
and is out of scope (§9).

**The chart** draws one glyph per cell already (`purview.rs:198`); the
legend dedupes by noun, which with names means one entry per named
resident. The legend is **not** grouped: every depicted noun must answer
`examine` (§6 of The Sighting's contract), and a synthetic "eleven more"
row would be a noun nothing resolves. One row per name is the stronger
contract and it is kept.

## 5. What retires, what stays

```text
  retires                            replaced by
  ---------------------------------  ---------------------------------------------
  NPC_COUNT, WILD_COUNT (session.rs) roll(); R and B
  "{species} of {village}" labels    the resident's name (residents only)
  one wild body per world-top-4      headcount-rendered wild bodies within R

  stays, unchanged
  ---------------------------------
  derive_npcs(k), derive_wild_npcs, wild_concentrations_from, body_at
  every lab battery and bench that calls them
  the possessed body's identity (ordinal 0)
  Occupancy at both bands; the four documented absences
```

## 6. The tick at a hundred bodies

Three superlinear sites and two constant-factor sinks are named by the
metaplan and confirmed by this campaign's own read of the tick
(`session.rs:6451` onward). They are in scope because a roll of eighty
lands on them:

```text
  site                                          order      where
  shared_believed_water's whole-band loop       O(A^2)     liveness.rs:1370, from WalkState::begin at :4905
  Ledger::clone() at the top of kernel::tick    O(L)/tick  kernel/src/schedule.rs:196
  the population walk evaluated TWICE per wait  x2         session.rs:6567 and :6640
  Body + HazardMemory cloned per decision point O(steps)   liveness.rs:4988
  agent_position fanned out 4x over the roster  O(A) x4    session.rs:6468, :6485, :6773
```

Each is a behaviour-preserving change: the session snapshot fixture must
not move a byte across this stage, and §8's M4 positive control is the
roster stage, where it must. **The kernel site is not edited.** The wait
already evaluates `step_with_occupancy` once for its occupancy and then
calls `kernel::tick`, which clones the ledger and evaluates the same walk
again; committing the first walk's facts in place — what
`agent_scaling.rs:398-415` already does — removes both the second walk and
the clone in `session.rs` alone. The metaplan's §5.5 seam (a tick contract
with nowhere to hang state) stays open and unneeded here. The preregistered target is §8's M2. If the target is met
before all five are touched, the rest are recorded as follow-ups with
their measured share, not done for completeness.

## 7. The key stays on the floor, with a reason

`SOC-one-creature-per-settlement` ends: *"Whoever raises it: a key waits to
move onto a person."* This campaign raises it and does **not** move the key,
and the reason is decision 0398 — a capability nothing can reach is not a
capability — with 0516 beside it. `held_by` takes any `EntityId`, so a
resident *can* hold the loomroom key today; but no verb transfers a held
thing from one creature to another, and no drive makes a resident set one
down. Moving the key onto a resident before a transfer path exists would
make the only lock in the game unopenable — the exact defect The Chattel's
fix round closed from the other side. So the key stays where The
Custodian put it, the follow-up is recorded with this reason in the
retrospective, and `PLAY-key-placement-stands-in-for-a-resident`'s Where
cell gains the pointer: the resident now exists; the transfer does not.

## 8. Preregistered measurement

Frozen here, before any of the code that would move them (decision 0016).
Each is a count or a budget, never a ratio.

- **M1 — company.** Across the 64-seed probe The Hand used, the number of
  seeds in which a fresh `possess` (flagship target) has at least one
  other creature in `sensed.present` on its first `look`. Today: 0.
  Prediction: equal to the number of those seeds whose home settlement has
  `population >= 2`, which the probe reports beside it. A seed where the
  two differ is a defect.
- **M2 — the budget.** `agent_scaling` at 100 and 200 bodies, release,
  on ambrose with all three load averages under 4 (The Repose's rule),
  before §6 and after. Target: **100 bodies in ≤ 1000 ms per tick** after
  §6. The 200-body figure is reported, not targeted. The pre-§6 run is
  re-measured rather than copied from §6.3 — a committed baseline is a
  claim with a date, and that one is nine days old.
- **M3 — the roll is pure.** A property test: for random (observer room,
  day) the roll is a function — two calls agree — and it is monotone in
  the sense §2.2 requires: no settlement contributes more residents than
  its population, no attractor more than its headcount.
- **M4 — nothing coarse moves.** `git diff --exit-code` over
  `docs/generated-paths.txt` after `make rebaseline` shows no change to
  any world artifact, and the census refresh queued at close moves **zero
  columns** — with the positive control that the vessel session-snapshot
  fixtures and `clients/game/core/tests/fixtures/` **do** move in the
  roster stage. A null with no control is not a result.
- **M5 — individuation is visible.** Of the 64 seeds, the number in which
  two residents of the home settlement stand in **different rooms** by the
  end of day 3 of an unattended session. No prediction is made beyond
  `> 0`; the mechanism changes deliberation, not destination, and it may
  well be that the dials alone do not separate a blob. If the count is 0
  that is the chronicle's headline for §3.3, and the follow-up is the
  next rung of `PSY-individual-deviation`, not a retune.

Decision rules, so the implementer acts rather than predicts:

```text
  after `make rebaseline` in the roster stage
    world-seed-42.json, any almanac, laboratory or domesday artifact moved -> STOP: an epoch, not this campaign
    book/src/gallery/possession-*.md moved               -> expected: they are session renders (M4 control)
    only docs/audits/ + the stream manifest moved        -> expected; commit in the same commit
    vessel/game session fixtures moved                   -> expected (M4 control); commit
  after §6 (each lever)
    session snapshot fixture moved                       -> the lever changed behaviour; revert it
    M2 met before all five levers                        -> stop; record the rest with shares
```

## 9. Out of scope

- **A settlement wider than one room** and a household or dwelling a
  resident belongs to (`liveness.rs:6125`). Residents share the settlement
  room and the within-room `Occupancy` today's bodies share.
- **The transfer verb** and the key's move (§7).
- **The slow tier** and any field-advanced dormancy (§3.7; registry row).
- **Per-settlement species composition.** The ledger carries one
  `peopled-by` species per settlement; `StackSettlement.composition` never
  reaches it (`SOC-people-not-species`). Residents are single-species per
  settlement because the settlement is.
- **Domestication, agroecology, commensals** (`BIO-animal-domestication`,
  `BIO-32`).
- **The invisible half** of the Penstock — views, eviction, fan-out.
- **Log bounding** (Penstock stage 7). A hundred bodies at ~2.5 facts per
  body per tick makes the unbounded ledger bind sooner; this campaign
  measures it (the commit-budget test's rate, at the new roster) and does
  not fix it.

## 10. Capture

- `SOC-one-creature-per-settlement` → `elaborated`, numbers corrected to
  post-Scour, Where → this spec.
- `PSY-individual-deviation` → `elaborated`, Where → this spec.
- New row `SOC-off-roll-slow-tier` (raw): the unnamed middle between a
  frozen body and a ticked one — one decision per day off the roll.
- `PLAY-key-placement-stands-in-for-a-resident`: Where gains §7's pointer;
  status unchanged.
- Decisions, numbered from 0546 at ratification: the roll is a pure
  function of (observer room, `WorldTime`, seed) and it is diegetic
  (Penstock decision 1, minted); a resident is a living person derived on
  demand, never generated; individual deviation is a per-kind spread and
  a lineage-salted draw; a dormant body's state is its committed facts,
  resumed by catch-up.

## 11. Definition of Done

Everything the closing skill requires — chronicle, freshness sweep
(`open-questions.md`'s company finding re-scored; the registry rows in
§10), retrospective, census queued at close with M4's null-and-control
reported — plus: the manifest regenerated in the commit that adds the
stream label; the type-audit report in the commit that adds any pub
boundary; `make game-check` run by hand, because nothing in the workspace
builds `clients/game/bin` and The Hand broke it invisibly.
