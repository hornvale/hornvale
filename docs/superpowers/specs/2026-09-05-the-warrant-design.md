# The Warrant: the typed, compositional intention — design

**Status:** Draft for G3 review (2026-09-05) · **Author:** Claude
(campaign-autopilot) · **Decider:** Nathan · **Stage:** Penstock **7b**
· **Relates:** [decision 0238](../../decisions/0238-stage-7-is-three-stages-and-their-order-is-forced.md);
[the Penstock metaplan](2026-08-22-the-penstock-metaplan.md) §5.6, §5.7, §6;
[decision 0538](../../decisions/0538-the-trail-is-a-resident-index-not-a-cached-hub.md);
`UNI-intention-is-structured`

> **This is an epoch.** It changes what the ledger commits for every creature
> that walks. Ledger entry #1–#5:
> `docs/superpowers/ledgers/2026-09-05-the-warrant.md`.

A *warrant* is both the authority for an errand and the justification for it.
That is what this campaign gives a walking creature: one committed thing
saying where it is going and why, in place of a prose sentence restated at
every step.

---

## 1. The fidelity premise, measured

Decision 0238 makes 7b precede 7c on one argument, and the whole ordering
rests on it:

> **7b must precede 7c, and this is a fidelity call.** The trail is *content*,
> not bookkeeping. Each step's `provenance` is authored prose … and it is
> rendered … Replacing per-step commits with per-errand ones before the
> intention carries its own compositional `why?` would delete readable
> content.

That was read off the code and never rendered. **It has now been rendered.**
The probe is `hornvale possess --seed N --script <look; wait 12; !why 1>`,
which drives the real `windows/historiography::recount` — the same call the
repl's `why` and the session's `!why` make.

**Seed 7, one resident, twelve days — the current output, verbatim and
unabridged in shape:**

```
Kwawkwapzow:
- canonical name of an entity: Kwawkwapzow (asserted by the-roll)
- this entity is an individual person: true (asserted by the-roll, day -25621.52191)
- the day this person was born; ... : -25621.522 (asserted by the-roll, day -25621.52191)
- an agent's position on a day: 3874794977 (asserted by wandered, having found no water yet (thirst), day 5.83239)
- an agent's position on a day: 3874798081 (asserted by wandered, having found no water yet (thirst), day 5.99811)
- an agent's position on a day: 3874798177 (asserted by wandered, having found no water yet (thirst), day 6.16383)
                                    ... 62 more lines, the parenthetical identical on every one ...
- an agent's position on a day: 3874984961 (asserted by wandered, having found no water yet (thirst), day 11.96403)
```

**Measured over three seeds** (40 residents each, 12 sim-days, counting
maximal runs of constant provenance in each resident's own trail):

```
+-------+-----------+---------+----------------+-------------------+
| seed  | agent-at  | errands | distinct       | steps per errand  |
|       | facts     | (runs)  | prose strings  | mean / med / max  |
+-------+-----------+---------+----------------+-------------------+
|   7   |   2600    |    40   |       1        |  65.00 / 65 / 65  |
|  14   |   3080    |    40   |       1        |  77.00 / 77 / 77  |
|  23   |   1364    |   579   |       3        |   2.36 /  2 /  5  |
|  42   |      0    |     0   |       0        |        n/a        |
+-------+-----------+---------+----------------+-------------------+
```

**The premise is half true, and the half that fails is the half the ordering
was built on.**

- **True:** the prose is content. It names the drive, the belief state and the
  direction of the errand, and three renderers put it in front of a reader.
- **False:** *per-step* commits carry that content. A run of identical strings
  carries the information of **one** string. Between 2.36 and 77 steps of a
  seed-7/14/23 trail restate the same sentence; the distinct-string count per
  resident over twelve days is **1, 1 and 3**.

**A per-errand fact is lossless with respect to today's rendered prose, by
construction**, because a run boundary is *defined* as the point where the
string changes. Nothing that a reader can distinguish today is dropped. What
goes is the repetition — and, on seeds 7 and 14, the repetition is 98.5% of
the rendered lines.

**The fourth row is its own finding and it constrains the whole blast
radius.** Seed 42 — the flagship, the keystone, the seed every committed
artifact is built on — **commits no `agent-at` fact at all**: 67 residents,
swept individually, over 90 sim-days, zero. Its roster condenses on water and
never has to walk. So the movement half of this epoch moves **none** of the
seed-42 artifacts, and the exposure lies entirely in synthetic harnesses and
the lab sweep (§7).

**What this does to 0238's argument.** The ordering it forces is *still
right*, but for a stronger reason than the one recorded: not "per-step prose
is content that abstention would delete", but **"the why is content, and it
currently has no home except a string repeated at every step."** 7b gives it a
home. Once it has one, 7c may drop steps without deleting anything at all —
which is a better precondition than 0238 claimed to be establishing. This
spec proposes an amending decision record (§11).

---

## 2. What 7b is, and what it is not

| | delivers | in this campaign? |
|---|---|---|
| 7a | the read side: nothing folds raw history | shipped (The Tailrace, The Pawl) |
| **7b** | **the typed, compositional intention** | **yes** |
| 7c | fact lifetime: what may leave, and how the prefix re-derives | **no** |

**7b removes no fact.** Every per-step `agent-at` fact still commits, on the
same day, with the same object. This is not a scoping preference; it is
forced:

- `agent_position` (`windows/vessel/src/liveness.rs:45`) *is* the position
  store — it folds `agent-at` — and the roster invariant
  `roster.positions()[slot] == agent_position(&ledger, body, day)` is restated
  at five sites in `windows/vessel/src/session.rs` (`:2431`, `:2449`, `:3874`,
  `:9087`, `:20738`).
- Six production folds read the per-step trail as semantics (0238's list, plus
  `position_timeline` and `room_entry_day`), through the `Trail` and
  `LatestVisit` resident tenants (`windows/vessel/src/resident.rs:101`,
  `:273`).
- Several tests assert a *positive* `agent-at` count and would go vacuous or
  panic without one (`the_kerf.rs:270`, `the_roll.rs:1083`,
  `controller_swap.rs:41`, `resident_folds.rs:3820`, `the_detent.rs:252`).

**What 7b changes is the `provenance` field of those facts, and it adds one
fact per errand.** That is the epoch.

> **The idea-registry row overstates 7b's delivery.**
> `UNI-intention-is-structured` says the intention does "the per-errand fact
> that **replaces** per-step commits". Replacement is 7c's job. The row is
> amended in the capture manifest.

---

## 3. The unit: the errand

### 3.1 It already exists and is already named

`Mode` (`windows/vessel/src/liveness.rs:2099`) documents itself as *"the
per-NPC behavioural commitment mode — **the errand an NPC is on** … it carries
across the steps of one walk to give hysteresis … and is **re-derived, never
persisted**."*

An **errand** is a maximal run of constant `Mode` (with the thirst belief bit)
during which the creature moves. Its boundaries are already computed:
`st.mode = resolution.mode` at `liveness.rs:8056`, sixty lines above the
`MoveTo` arm that authors today's prose. The commit site is a comparison
against the previous value at that same line.

So 7b is not the invention of a concept. It is the **promotion of a
tick-local, deliberately un-persisted value into committed truth** — which is
exactly why it costs an epoch, and exactly what the metaplan sanctioned by
name (§5.6): *"the discrete divergence — this agent resolved to go there, on
this day — commits, and the step sequence stays derived and re-derivable from
that commitment."*

### 3.2 The errand is one rung on a scale, and saying so is load-bearing

```
  step  <  ERRAND  <  drive episode  <  mission  <  standing disposition
   |         |             |              |                 |
  1 tick   2-77 steps   out + back    multi-drive      a lifetime
           (measured)   (2 errands)   (§5.8's gap)     (temperament)
```

The rung below is today's design. The rung above — the *drive episode* — was
weighed and rejected: it merges the outbound and homing legs of one thirst
episode into a single fact, which deletes `walking home (sated)` outright.
That would be a real fidelity cut; the errand is not.

Naming the scale keeps the door open for the metaplan §5.8's acknowledged gap
("nothing protects a *multi-tick plan*") without re-litigating this unit: a
mission is a longer-lived intention over the same machinery, not a different
mechanism.

### 3.3 An abandoned errand becomes visible for the first time

Today, a creature preempted mid-errand leaves a trail that simply stops
mentioning that drive; nothing records that it had resolved to go somewhere
and did not arrive. With the errand committed, **the target it never reached
is on the ledger.** That is the raw material the metaplan already asked for —
§5.7's preemption-to-invalidation ratio and §5.9's sunk-cost study both need
to know when a commitment was abandoned — and it arrives as a side effect,
not as extra work.

---

## 4. The representation

`Value` is `{Entity, Text, Number, Flag}` (`kernel/src/ledger.rs:55`); there
is no structured variant, and widening it is a kernel save-format change
touching every match arm, every serialized surface and the clients. So the
errand's two components go where the envelope already puts things:

```
Fact {
    subject:    the creature,
    predicate:  one of eight registered `errand/*` keys   <- the REASON
    object:     Value::Text(room_to_text(&st.pos)),       <- the ORIGIN
    place:      None,
    day:        Some(the tick the errand's first step is charged to),
    provenance: "vessel/liveness",                        <- the PRODUCER
}
```

### 4.0 The object is the errand's ORIGIN, not its target — a correction

The first draft of this section put the *target* in the object. **There is no
target to put there.** The arbitration seam exposes `Intent::Do(Action)` and
nothing else (`liveness.rs:1920`): `Action::MoveTo(n)` names the *next step*,
and `Drive::proposal` is documented as "the next executable step". The
destination a creature is walking toward is never materialized at the commit
site, so the field as first specified had no possible caller.

Three ways out were weighed. **Surfacing the goal through `Drive`/`Resolution`**
is a change to the arbitration seam, which §9 forbids for this campaign and
which would make an epoch also a behavioural change — rejected. **`Value::Flag(true)`,
with everything derived** works but throws away a fact that is free and
useful. **The origin** is available (`st.pos`, at the instant the errand's
first step is charged), is never wrong, and is what makes the errand a
*segment* rather than a point.

The consequence for the rendering is real and improves it. An errand's
**endpoint is derived** — it is the position at the next errand boundary,
read from the same `Trail` the store already keeps — so the recount says
where a creature *got to*, never where it *meant* to go. For an errand that
completed, those coincide. For an **abandoned** errand (§3.3) they do not, and
asserting a target would have made the ledger claim an intention the code
never formed. The one case where a target genuinely exists — `errand/water-known`,
where `st.believed` holds the source — is deliberately *not* special-cased:
one shape for all eight keys, and the believed source is already recoverable
from the belief fold.

### 4.1 The eight keys are the eight arms, one for one

The reason is `Mode`-shaped, not `DriveKind`-shaped — an enrichment from the
ideonomy pass (ledger #5a). `Mode::Homing`/`Idle` is not a pursuit at all, and
`Pursuing(Danger)` is repulsion *from* a threat rather than attraction *to* a
target, so `Intention::Go(target, reason)` as the registry row spells it
under-types two of the eight cases.

```
+---------------------------------+---------------------+--------------------------------------------+
| Mode                            | predicate           | registry doc (the rendered gloss)           |
+---------------------------------+---------------------+--------------------------------------------+
| Pursuing(Thirst), believed      | errand/water-known  | went down to the river it knew (thirst)     |
| Pursuing(Thirst), ignorant      | errand/water-blind  | wandered, having found no water yet (thirst)|
| Pursuing(Hunger)                | errand/forage       | foraged toward richer ground (hunger)       |
| Pursuing(Thermal)               | errand/comfort      | sought a kinder clime (comfort)             |
| Pursuing(Fatigue)               | errand/rest         | turned home, weary, to rest                 |
| Pursuing(Danger)                | errand/flight       | fled the uncanny ground (fear)              |
| Pursuing(Social)                | errand/company      | drifted homeward, missing its people        |
| Homing | Idle                   | errand/home         | walking home (sated)                        |
+---------------------------------+---------------------+--------------------------------------------+
```

The mapping is written as an **exhaustive `match` with no `_` arm**, so
widening `Mode` or `DriveKind` is a compile error rather than a silent
fall-through into the wrong errand — the compiler is the enumeration, and a
wildcard would void it.

### 4.2 Why the reason lives in the predicate

`register_predicate(name, functional, doc)` gives every predicate a doc
string, and that doc is **already the only prose `windows/historiography`
renders for a predicate** — `agent-at`'s doc is what produces the leading
`an agent's position on a day:` on every line quoted in §1. Putting the reason
in the predicate therefore moves the reader-facing words out of the ledger and
into the registry, where `kernel/src/phenomena.rs`'s producer rule already
puts them:

> **A phenomenon carries no text.** A producer cannot know who is looking …
> so a stored string could only ever be culture-neutral or wrong.
> Reader-facing words are realized where the speaker is known.

That gap — *"`Phenomenon`'s doc says a producer stores no reader-facing text;
`Fact.provenance` does not"* — is the convention gap
`UNI-intention-is-structured` exists to close, and this closes it in the
direction the kernel already argues for. A per-drive predicate family is
established convention here rather than a novelty: `drank` / `eaten` /
`rested` / `slept` are already four predicates for one relation.

**Rejected alternatives** (ledger #3): a single `intends` predicate with a
packed object `Text("thirst-known@229504")` — one key, but no per-reason
registry doc, so the prose returns to code and a future language layer has
nothing to realize; the reason in `provenance` — recreates the very gap being
closed, since every other producer in the repo puts a *system* name there
(`astronomy`, `species`, `the-roll`); widening `Value` — right in the
abstract, disproportionate here.

### 4.3 The step's provenance

`agent-at`'s `provenance` becomes `"vessel/liveness"` — the producer, like
every other fact in the repo. It is no longer a semantic field, which restores
the truth of `liveness.rs:2743`'s own claim that *"`provenance` is free-form
prose no fold may key on"* — a claim one test currently violates (§7.2).

### 4.4 Registration is session-only, and that bounds the blast radius

These predicates register on the session's registry clone, exactly as
`agent-at` does (`windows/lab/src/health.rs:336`; `Session::start`). **Verified
by absence with a live control:** `agent-at` is a session predicate and does
not appear in `book/src/reference/concept-registry-generated.md`; neither will
`errand/*`. The concept-registry artifact does not move.

---

## 5. What historiography and the repl render after the flip

The bar Nathan set is *at least as readable*. Two renderings satisfy it and
this spec ships both, because they answer different questions.

### 5.1 The default: the errand line, with its steps rolled up

```
Kwawkwapzow:
- canonical name of an entity: Kwawkwapzow (asserted by the-roll)
- this entity is an individual person: true (asserted by the-roll, day -25621.52191)
- the day this person was born; ... : -25621.522 (asserted by the-roll, day -25621.52191)
- wandered, having found no water yet (thirst): from 3874794977
  — 65 steps, days 5.83239 to 11.96403, ending at 3874984961
```

Sixty-eight lines become four. The seed-23 regime, where errands are short and
alternate, keeps its texture:

```
- sought a kinder clime (comfort): from 812... — 3 steps, days 4.10 to 4.60
- walking home (sated): from 811... — 2 steps, days 4.60 to 4.93
- went down to the river it knew (thirst): from 229408 — 2 steps, days 5.01 to 5.34
```

### 5.2 The per-step view, on request

`why <entity> --steps` (repl) and `!why <who> --steps` (session) keep one line
per step, each parenthetical **resolved from its covering errand** rather than
read out of the fact:

```
- an agent's position on a day: 3874798081 (wandered, having found no water yet (thirst) — step 2 of 65, day 5.99811)
```

This is today's line plus a position-in-errand the current output cannot
express, because today nothing knows how long the errand is.

### 5.3 The compositional `why?`

The chain the registry row asks for is **composed at read, from three facts
and one re-derivation**, never baked into one sentence:

```
this step (agent-at, day 6.16)
  └─ its covering errand (errand/water-blind, set out from 3874794977, day 5.83)
       └─ the drive it serves (thirst)
            └─ when that drive was last discharged (drank, day —— never)
```

Today's string is the leaf of that chain, flattened and stored. After the
flip, each link is a fact and the renderer walks them — which is what makes
the answer to *why?* extensible without another epoch: a later campaign that
commits threshold crossings (§5.7) or suppressed drives adds a link, not a new
string.

### 5.4 The renderer stays predicate-blind for everything else

`recount`'s generic loop is untouched. The errand-aware pass is a
**pre-grouping step** over the fact list — group `agent-at` runs under their
covering `errand/*` fact — leaving every other predicate to render exactly as
it does now. `windows/historiography` gains no dependency on
`windows/vessel`: it keys on the `errand/` prefix, which is registry data.

---

## 6. Where an errand is anchored: a sixth resident tenant

`why?` must find, for a step at day *d*, the latest errand fact for that
entity at or before *d*. That is a binary search over an append-only,
absorbed-once, day-ordered index — **the exact construction decision 0538
admitted** for `Trail`, and distinguished from the rebuilt-per-call hub The
Tailrace deleted.

The Pawl chronicle anticipated this tenant by name: *"the sixth campaign to
need one — hysteresis on drive arbitration, **a typed intention's anchor**, a
threshold-crossing detector — would have built a seventh."* It is the second
of those three, and it lands as a tenant of the existing `ResidentFolds`
store rather than a new cache.

`Errands` is strictly smaller than `Trail` (one entry per errand against one
per step — 1/65th of it in the seed-7 regime), session-owned, never
serialized (decision 0536), and discardable at any instant. Its
discard-and-rebuild equivalence is pinned per tenant the way every other one
is.

---

## 7. Blast radius, measured

### 7.1 Committed artifacts

**Moved:** nothing, on the evidence to date — and the reason is §1's fourth
row. Seed 42 commits no `agent-at`, so:

- `book/src/gallery/possession-seed-42.md`, `possession-over-time-seed-42.md`,
  `possession-carry-seed-14.md` — the `(N stirred)` count is
  `Ledger::commit`'s `Ok(true)` tally (`session.rs:9009`), so it moves **only
  if the fact count moves**. Seed 42 commits no errand facts because it
  commits no steps. **To be confirmed by regeneration, not by this
  paragraph** — it is a prediction and Task 1 owns falsifying it.
- `book/src/laboratory/generated/the-census/rows.csv` — **252 columns,
  measured; zero match `fact|tick|agent|drive|walk|ledger`.** The census
  builds to `BuildDepth` and never runs a session. No census refresh is owed
  by this campaign's *content*; one is owed only if a run shows otherwise.
- `book/src/laboratory/generated/the-history/` — the one heavy-tier writer
  (`cli/tests/suite/history_battery.rs:405`) is a worldgen cascade battery:
  no session, no tick, no `agent-at`.
- `book/src/reference/concept-registry-generated.md`,
  `stream-manifest-generated.md` — unmoved; §4.4, and `windows/vessel` draws
  no stream in the drive tick at all (`liveness.rs` has zero `.stream()` /
  `next_f64` / `Seed::derive` call sites, so no stream-order contract is in
  play).
- `clients/game/core/tests/fixtures/*.json` — the `vessel/session/v2` wire
  shape carries no ledger facts.

**Byte-goldens at genuine risk:**

- `windows/lab/tests/fixtures/affect-trace-seed-42.txt` — routes through
  `health.rs:257`'s `agent_position` fold. Rebaselinable (`Makefile:711`).
- `windows/vessel/src/liveness.rs:11150-11290` — the `hoist_walk_shape`
  literal, ~108 rows of which ~28 are `agent-at` carrying the provenance
  string verbatim, on a synthetic planted terrain that *does* walk. **Hand-
  rewritten only**; its own doc forbids machine rebaselining. This is the
  single largest mechanical edit in the campaign and it is where a wrong
  expectation would look plausible.
- `windows/vessel/tests/fixtures/snapshot-seed-42-*.json` — taken after a
  tick; carry `present`/`social`, not provenance.

### 7.2 One instrument goes vacuous rather than red — the finding that must not be missed

`windows/vessel/tests/suite/tick_commit_budget.rs:527-530` computes
`FEAR_OR_BELONGING_CEILING` by **grepping `provenance` for `"(fear)"` and
`"(belonging)"`**. Its own module doc calls this file *"The Penstock's
feasibility number"*, and the tripwire exists to catch
`PSY-drive-arbitration-limit-cycle` returning.

After this epoch that grep matches nothing, reads **0**, and **passes**. It
does not fail; it stops measuring, inside a green gate, in the one file whose
job is to watch this program's own feasibility number. It must be re-pointed
at the `errand/flight` and `errand/company` predicates **in the same commit
that moves the provenance** — a check that can never fire is worse than an
absent one.

Its sibling constants are safe and it is worth saying why, because a first
reading gets this backwards: `MIN_CONTRIBUTING_RESIDENTS = 60` (`:302`) and
`NON_GROWTH_MARGIN` (`:219`) would be the most likely failures *if 7b dropped
per-step facts* — it does not (§2), and on seed 42 the rate is carried
entirely by `drank`/`rested`/`slept`/`eaten`, which this campaign does not
touch.

### 7.3 Prose that will go stale with nothing to tell you

Nine-plus hand-written pages quote the exact provenance strings and are **not
declared in `docs/generated-paths.txt`**, so no drift check covers them:
`book/src/chronicle/the-foresight.md:144`, `the-wanting.md:107`,
`the-quickening.md:162`, `the-deed.md:143`/`:240`, `the-surmise.md:156`,
`the-tailrace.md:240`; `book/src/frontier/idea-registry.md:1902`;
`docs/audits/the-tenon-rest-site-baseline.md:161`,
`the-escapement-census-attribution.md:285`.

Chronicles are history and are **not rewritten** — they record what was true
when written. The freshness sweep covers the *live* references: the registry
row, and any book chapter that describes the mechanism in the present tense.

### 7.4 Out of scope, and named rather than silently skipped

The **player-possessed** body commits `agent-at` with its own prose
(`session.rs:201`, `:205`: *"walked on (its own errand)"*, *"turned back the
way it came"*). The player has no `Mode`, so there is no arbitration to
promote and no errand to commit. Those two strings stay. **This leaves the
convention half-closed** — NPC steps carry a producer token, player steps
carry prose — and that asymmetry is a G3 flagged item (§10), not an oversight.

---

## 8. What 7c may assume afterwards

1. **Every committed step is covered by an errand fact** that names its reason
   and its origin — so dropping steps deletes position, never *why*. Note the
   asymmetry 7c inherits: an errand's ENDPOINT is derived from the steps
   (§4.0), so a compaction that drops every step of a completed errand must
   fold that endpoint into the errand fact first, or the segment loses one of
   its two ends.
2. **The why survives compaction independently of the steps.** An errand fact
   is ~1/65th the volume of the steps it covers in the walking regime; a
   compaction that keeps errands and folds steps keeps every distinct sentence
   a reader can see today (§1).
3. **The re-derivation target is stated.** A surviving prefix plus the errand
   facts plus the seed re-derives the step sequence, because the step sequence
   is a path search from the errand's start to its target — which §5.6 already
   requires to be derived rather than stored.
4. **Abandonment is on the ledger** (§3.3), so 7c can distinguish "this errand
   completed and its steps are redundant" from "this errand was cut short",
   which is precisely the case where the steps are the only evidence.

7c is **not** designed here and nothing in this spec commits to its policy.

---

## 9. Non-goals

- No fact is removed, compacted, or abstained from (7c).
- No change to `Value`, to `Fact`'s shape, or to any kernel type.
- No change to arbitration, hysteresis, or any drive constant. **If a drive
  constant moves, the campaign has gone wrong** — this epoch is about what is
  written down, not about what creatures decide.
- No preemption work (metaplan §5.8: it is `PSY-6`'s and mostly ships).
- No replan-invalidation machinery (§5.7).
- The player's own errand (§7.4).

---

## 10. Preregistration

Frozen here, before the code that moves it (decision 0016). The probe in §1 is
the instrument; it is re-run unchanged.

- **H1 (the fidelity claim, and the one that matters).** For every resident on
  seeds 7, 14, 23 over 12 days, the **set of distinct reason-glosses rendered,
  and the day each first appears**, is identical before and after the flip.
  *Falsified if any resident's set or first-appearance day differs by one
  element.* This is a claim of exact equality, not of approximation, and it is
  the whole justification for calling the change lossless.
- **H2.** Provenance bytes committed per agent per tick fall by at least 50%
  in the walking regime, and by exactly 0% on seed 42. *Falsified if the
  walking-regime reduction is under 50%, or if seed 42 moves at all.*
- **H3.** Facts committed per agent per tick rise by at most `1/2.36` of the
  `agent-at` rate — the seed-23 regime is the worst case measured, and
  `tick_commit_budget`'s `STEADY_STATE_CEILING` is 2.5 against a measured
  1.06. *Falsified if any seed exceeds the ceiling.*
- **A null on H2 or H3 is a result and is reported as the headline**, not
  retuned away. H1 is not a null candidate: it is a correctness claim.

**Not preregistered, because it is a decision rule rather than a
prediction:** on regenerating artifacts, if `book/src/gallery/` moves →
**stop**, this is an epoch event and Nathan sees the diff; if only
`docs/audits/` moves → regenerate and commit in the same commit; if
`book/src/laboratory/generated/the-census/` moves → a census refresh is owed
and goes through the queue.

---

## 11. Stage carve

| stage | delivers | done when |
|---|---|---|
| 1 | The eight `errand/*` predicates, registered; the exhaustive `Mode`→key match; the commit site at `liveness.rs:8056`. Steps keep their prose. | errand facts commit; every existing test still green; H3 measured |
| 2 | The epoch: `agent-at` provenance → `"vessel/liveness"`. `hoist_walk_shape` literal hand-rewritten; `tick_commit_budget`'s fear/belonging witness re-pointed (§7.2). | H1 and H2 measured; `affect-trace` rebaselined if it moved |
| 3 | The `Errands` resident tenant; discard-and-rebuild equivalence pinned. | equivalence green at every position and every third position |
| 4 | The renderings (§5.1, §5.2, §5.3) in `windows/historiography`, the repl and the session. | before/after captured on seeds 7/23; artifacts regenerated |
| 5 | Decision records; registry-row amendment; book chronicle + freshness sweep; retrospective. | G6 |

**A decision record amending 0238** is owed at close: not a reversal — the
order it forces is right — but a correction of the argument, since 0238's
stated reason ("per-step prose is content") is measurably not why the
ordering holds (§1).

---

## Appendix: reproducing §1

```bash
cargo build -p hornvale --bin hornvale
printf 'look\nwait 12\n!why 1\nrelease\n' > /tmp/why.txt
./target/debug/hornvale possess --seed 7 --script /tmp/why.txt
# and, for the null:
{ echo look; echo "wait 90"; for i in $(seq 1 67); do echo "!why $i"; done; echo release; } > /tmp/all.txt
./target/debug/hornvale possess --seed 42 --script /tmp/all.txt | grep -c "position on a day"   # -> 0
```
