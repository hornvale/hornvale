# The Hand — one body, and the controller is a parameter

*Arc II of The Bridle. Ships the controller stack, demotes GOAP to the
default controller, and merges the two body types that made a controller
stack impossible. Arc I (The Tackle, The Deed) shipped the action layer and
the action suite; this arc makes the thing that performs an action singular.*

---

## 1. Scope

**Ships.** One body type. Possession as *selection* rather than minting. One
tick over every body, with the source of intent a parameter. Arbitration
running for a possessed body, which settles what happens to the host.

**Does not ship.** The imposed controller and `dominated` (Arc III — The
Coercion). Object affordances (Arc IV). Verbs as data (Arc V). The host
*speaking* — this arc computes what a host feels and gives it a route, but
rendering testimony is `PLAY-host-is-a-narrator`'s own work.

**Does not cost a world-generation epoch** — an earlier draft said it did, and
§4 corrects that. It moves the played-world surface and retires one stream
label.

---

## 2. Corrections and additions to the metaplan

### 2.1 The metaplan names the controller stack but not the merge it requires

The Bridle's arc table gives Arc II as *"the controller stack; GOAP demoted
to 'the default controller'"* with the acceptance test *"swap controllers — a
creature on player-input and a player body on GOAP."* It names no types, and
`Npc` and `Agent` appear nowhere in it.

They are the obstacle. A creature is an `Npc` (`liveness.rs`): entity, home,
resource, species, activity, temperature niche, deliberation latency, time
horizon, metabolic class, niche, boldness, threat niche, mass, label. A
possessed body is an `Agent` (`agent.rs`): id, species, perception, position,
village. **No conversion exists anywhere in the tree**, and 23 functions take
`&Npc` or `&[Npc]`, none of which a possessed body can be passed to.

You cannot swap controllers between two things that are not the same kind of
thing. The merge is Arc II's enabling half and the metaplan did not see it;
The Deed found it and recorded it as decision
[0167](../../decisions/0167-a-driver-is-interchangeable-and-a-possessed-body-is-a-creature.md).

**Correcting 0167's own number while we are here:** it says 21 functions.
The true count is **23**. The 21 came from counting `grep` *lines*, and was
approximately right by accident — re-derived here with a multi-line-aware
parse.

### 2.2 The tick does not skip a possessed body, and an early draft of this spec said it should

The tempting shape is: the possessed body joins the roster so others can
perceive it, but the tick skips it, because you are already deciding.

That is wrong, and `PLAY-host-is-a-narrator` says why: *"the host stays aware
and tells you things — affect, local belief and dread, **all three already
computed and all three currently without an honest route to the player**."*
**Arbitration is where a host's inner life is computed.** Skip it and the
host has nothing to be aware *with*, and the row stays unbuildable.

So every body goes through the same tick. Only the source of the intent
differs.

### 2.3 This arc settles `PLAY-what-happens-to-the-host`, and does so deliberately

The registry calls that row *"the crux question [`PLAY-no-victory`]'s empathy
thesis makes central, and nothing answers it"*, with two candidates:
**displaced** (dormant, returns after — possession is a borrowing) and
**co-present** (they are in there, aware — every possession is a
relationship).

Running arbitration for a possessed body **is** the co-present model,
mechanically. A body with live drives and feelings while you ride it is a
body whose occupant is present. Displaced would mean arbitration does not
run.

That is too large a question to settle as a side effect of a refactor, so
this arc settles it **on purpose**, with a decision record. Co-present is
chosen. Everything downstream — `PLAY-host-may-refuse`, `PLAY-host-names-you`,
`PLAY-affect-becomes-testimony`, `PLAY-vacated-host-testifies` — inherits it.

---

## 3. Design

### 3.1 One body type

`Body` replaces `Npc` and `Agent`:

| group | fields |
|---|---|
| identity | `entity`, `species`, `label` |
| place | `home`, `resource`, `village` |
| senses | `perception` |
| drives | `activity`, `temperature_niche`, `metabolic_class`, `niche`, `boldness`, `threat_niche`, `time_horizon`, `deliberation_latency` |
| body | `mass_kg` |

**Every field is derivable from `(species, settlement)`**, which both existing
constructors already have: `derive_npcs` reads `biosphere_registry()` and
`psyche_registry()` by species label and takes `home`/`resource` from the
settlement; `mint_flagship` starts from a `VillageInfo`. So one constructor
serves both, and the two entry points become arguments to it rather than
separate derivations.

**`position` is deliberately absent.** `Npc` has none — position is the
latest committed `agent-at` with a `home` fallback — while `Agent` carries one
explicitly. The merge unifies on ledger-derived, which is what decision
[0069](../../decisions/0069-fine-position-is-never-serialized.md) already says
a persisted position *is*, and The Deed already made a possessed body commit
`agent-at`. `self.agent.position = …` disappears; committing the fact becomes
the position update.

### 3.2 Possession is selection, not minting

Today `agent_entity()` is `EntityId::new(self.agent.id.0)` — a possessed body's
identity is its `AgentId`, a separate seed-derived draw. Meanwhile
`mint_flagship` picks the most-populous settlement and `ordered_for_derivation`
**hoists the home settlement to index 0**.

So a possessed body and derived creature #0 already share a settlement, a
species and a home. They are two representations of one villager, kept apart
only by being different types. Merge them naively and the duplicate becomes
visible: a twin standing where you stand.

Therefore possession stops minting:

```rust
Session { bodies: Vec<Body>, driven: usize }
```

`driven` indexes the roster. `possess --seed 42` sets `driven = 0` — the
flagship settlement's body, already first by the existing hoist. **Possessing
any creature is `driven = i`**, so the metaplan's Arc II acceptance test
("a creature on player-input") needs no new mechanism.

Three consequences:

- **`AgentId` disappears**, and with it its stream draw. Identity becomes the
  derived entity.
- **"Possessed" is not a field on `Body`.** It is a fact about the session,
  and putting it on the body would recouple the driver to the thing driven —
  precisely what decision
  [0168](../../decisions/0168-the-effect-of-an-act-belongs-to-the-body-not-the-driver.md)
  separates.
- `driven: usize` generalises to a controller map in Arc III without the body
  type changing.

### 3.3 One tick, the controller a parameter

```
for body in bodies:
    mode, affect = arbitrate(body, world)          # always, for every body
    intent       = controller_for(body).intend(…)  # GOAP, or player input
    commit(advance_one(body, intent))
```

GOAP becomes `DefaultController` — the metaplan's own phrasing — and player
input becomes a second implementation. Nothing about `advance_one` learns who
chose.

**The payoff is available from the moment this lands and by no other route.**
Every tick now computes both what the host *wanted* (its mode and affect, from
its own drives) and what the body *did* (from you). The gap between them is
derivable with no new machinery: *you made it stay when it wanted to run.*
That is the substrate under `PLAY-host-may-refuse`, `PLAY-soul-autonomy` and
`PLAY-motive-drift`.

**Mind flayers fall out of the same shape.** If the intent source is a
property of the relationship rather than of the body, "body A is driven by
mind B" has the same structure as "body A is driven by a keyboard".
`PLAY-possession-is-natural` becomes buildable rather than needing a parallel
mechanism. Arc III ships it; this arc must not foreclose it.

### 3.4 "Who else is here" is deferred, and says so

`colocated_npcs` and `sensed_npcs` answer *who is here* by filtering the
roster. With a possessed body in that roster they now include **you**, so
`needs` would report your own felt state among the others'.

This arc **keeps today's meaning** by excluding `driven`, with a comment
naming what supersedes it: a component-shaped exclusion (the Infocom/Inform
scenery-flag pattern — creatures in the area lacking an
`ExcludeFromWhoElse`-style marker), served by an indexed query and iterated as
an array. That is Penstock-lineage work and belongs there, not here.

A placeholder that admits it is one, rather than a design decision made by
accident in the wrong campaign.

---

## 4. Drift

**This is NOT a world-generation epoch, and an earlier draft of this spec said
it was.** That draft claimed the vanished `AgentId` draw would shift every
subsequent draw so "every seed generates a different world". Checked, and it is
false in two independent ways:

- `mint_flagship` is called **only** from `Session::start`
  (`session.rs:806`) — never during genesis.
- It does not continue the genesis stream. `mint_at` derives from
  `position.seed(world.seed).derive(VESSEL_AGENT).stream()` — a fresh,
  position-keyed, separately-labelled stream, taken after the world is fully
  built.

So no seed generates a different world, and **the census cannot move**: the lab
never starts a `Session` (`grep -rn 'Session::start' windows/lab/` is empty),
so no census metric observes possession at all. Neither does the Domesday
survey, which is a pure read over the census.

**What actually moves is the played-world surface, and it is the same set The
Deed moved:**

- `book/src/gallery/` possession transcripts
- the committed `vessel/session/v2` client fixtures
- `windows/vessel/tests/fixtures/` session snapshots (not drift-checked — see
  The Deed's F-6; refreshed by `make rebaseline-goldens`)

**The one genuine save-format change is narrower than an epoch and must not be
confused with one:** the `VESSEL_AGENT` stream label is retired, which moves
`book/src/reference/stream-manifest-generated.md`. A stream label is a
permanent contract, so retiring one is deliberate and recorded — but it is a
change to the *label roster*, not to any world's bytes.

**The roster does not grow, it shrinks by one entity.** Today a session holds
`k` derived creatures plus one separately-minted agent. After, it holds `k`
creatures, one of which is driven. The possessed body stops being an extra
entity and becomes one of the `k`. What changes is that the driven body is now
*visible* to perception and occupancy — not that anything was added.

**Verify rather than assume at the drift step:** if a genesis artifact moves
(`cli/tests/fixtures/world-seed-42.json`, any census CSV, the elevation map),
that contradicts the analysis above and is a finding, not a rebaseline.

## 5. Risks

1. **A parallel campaign is restructuring the same file.** Penstock-lineage
   work — intention-commits, the ten remaining unindexed subject scans — lands
   in `liveness.rs`, which this arc rewrites. Two campaigns restructuring one
   5,800-line file is the semantic-collision shape no gate catches. Post a
   board `notice` with `polarity=hold-off` on `windows/vessel/` before Task 1,
   absorb main at every stage boundary, and read the other branch's
   *chronicle*, not only its diff.

2. **The ledger growth problem is inherited, not worsened — an earlier draft
   said otherwise.** The Penstock measured 0.94 facts per agent per tick, flat
   and non-decaying, and traced it: *only the discrete divergence commits, the
   smooth routine stays derived* — but drives removed the default schedule a
   divergence was measured against, so every step is now a divergence. A first
   draft of this spec claimed routing a possessed body through the tick adds a
   body to that regime. It does not: `advance_one` commits on `Intent::Do(..)`
   and nothing on `Intent::Hold`, and a driven body holds whenever its
   controller has no pending command — which is most ticks of a multi-day
   `wait`. The commits a possessed body makes are the ones The Deed already
   ships, arriving by a different path. **Verify this at implementation** with
   a facts-per-tick count before and after; if it moves, the analysis is wrong
   and that is a finding.

   > **Amended at close (The Hand, Task 5). The prediction held; the mechanism
   > named above is not the one operating, and the measurement cannot tell the
   > difference.** Measured 0.25 facts/body/tick before and after, identical to
   > the fact — so the rate did not move, as predicted. But it did not move
   > because `Hold` commits nothing. `Session::wait` discards the driven walk's
   > emitted facts **unconditionally**, so the rate is flat whatever the
   > controller answers: forcing `intend` to return `Intent::Do(Action::Rest)`
   > leaves the guarding test green. The discard is correct and deliberate —
   > the player's verbs are what the body *does*, while the walk supplies what
   > the host *wants*, and committing both would give a possessed body two
   > competing sources of position — but it makes the "commits on `Do`, nothing
   > on `Hold`" argument **untestable in this design**, not confirmed by it.
   >
   > This measurement was run twice and was vacuous both times for two
   > *different* reasons: first against an implementation that never touched
   > the ledger at all, then against one whose facts are discarded before
   > reaching it. A third run was declined rather than staged until it produced
   > a number that looked like agreement. What the flat rate does license is
   > the narrower claim the arc actually needs: **routing a possessed body
   > through the tick costs no committed facts.** That is true, and it is what
   > risk #2 was for.

3. **`AgentId` removal touches a save-format contract.** Any world already
   carrying a played session's facts keys them on the old identity. The epoch
   covers it, but the change must be deliberate and recorded, not incidental.

4. **A latent correctness bug sits inside the code this arc rewrites.**
   `latest_committed_position` selects `f.day <= t` while `Ledger::commit`
   quantizes a fact's day *upward*, so a fact committed at day D is invisible
   to a read at D and `agent_position` falls back to `npc.home`
   (`KNOW-commit-read-same-instant`). It is pre-existing and narrow. Do not
   fix it silently while passing through: either leave it and cite the row, or
   fix it deliberately and measure what moves.

---

## 6. Decisions to record

- **A possessed host is co-present, not displaced.** Arbitration runs for a
  possessed body; it has drives, affect and inclination while ridden. Settles
  `PLAY-what-happens-to-the-host`.
- **Possession selects a body; it does not mint one.** Identity is the
  creature's, not a separate draw.
- **A controller is a parameter of the tick, not a property of the body.**
  GOAP is the default controller, not the only one.
- **One body type.** A creature and a possessed body differ in who is driving
  and in nothing else.

---

## 7. Flagged for G3

1. **The cost is far smaller than first stated, and the first statement is what
   authorised it.** An earlier draft claimed a full world-generation epoch —
   every seed re-deriving, a census refresh at close. §4 now shows that is
   false: genesis is untouched and the census never possesses. What moves is
   the played-world surface plus one retired stream label. **This was
   authorised on the wrong number and should be re-confirmed on the right
   one** — the appetite for scope may reasonably change now that it is cheap.
2. **Co-present settles the design's biggest open question** (§2.3). It is the
   right answer and it is not a small one.
3. **`VESSEL_AGENT` is retired** (§4) — a permanent stream label, so its
   removal is a deliberate save-format act even though no world's bytes move.
4. **This arc knowingly worsens a measured growth problem** (§5.2) rather than
   waiting behind its repair. Sequencing call, made deliberately.

## 8. Deliberately not in this arc

- The imposed controller and `dominated` (Arc III).
- Rendering the host's testimony. This arc *computes* it and gives it a route;
  `PLAY-host-is-a-narrator` renders it.
- Intention-commits and the ledger growth bound (Penstock lineage).
- The component-shaped "who else is here" query (§3.4).
- ECS storage of any kind. The ledger is already an entity-component store
  (subject/predicate/object with SPO/PSO/OSP indexes); the derived roster is
  already the dense array a tick iterates. Neither needs restructuring for
  this arc, and doing it here would hide the merge inside a rewrite.

---

## 9. Definition of done

Beyond the standing DoD (chronicle, retrospective, freshness sweep,
Confidence Gradient re-score, registry flips):

- The metaplan's acceptance test passes: **swap controllers** — a creature on
  player input and a possessed body on GOAP both behave correctly, proven by a
  test that fails if either path special-cases the other.
- A test asserts arbitration runs for a possessed body, since that is what the
  co-present decision means mechanically and it is invisible otherwise.
- The epoch is regenerated, reviewed and committed with the change; a census
  refresh lands at close.
- `PLAY-driver-substitutability` flips status and repoints **Where**;
  `PLAY-what-happens-to-the-host` flips to reflect its settlement.

---

## 10. Provenance

Occasioned by decision 0167 (The Deed), which recorded the `Agent`/`Npc` split
as a must-fix and deferred it. Arc II of The Bridle metaplan
(`2026-08-19-the-bridle-metaplan.md`), which named the controller stack.
Registry rows: `PLAY-driver-substitutability`, `PLAY-what-happens-to-the-host`,
`PLAY-host-is-a-narrator`, `PLAY-possession-is-natural`, `PLAY-soul-autonomy`.
