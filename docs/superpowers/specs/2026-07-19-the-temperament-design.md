# The Temperament — Design

**Date:** 2026-07-19
**Status:** Draft — awaiting G3 review (campaign-autopilot hard stop).
**Campaign:** (confirm numbering at merge)
**Provenance:** The correspondence audit shows the **cognition ledger empty —
0 of 76 concepts** hold a cognitive handle. The Elements made the world *felt*
(a creature at a cold cell has a `cold` percept), the-foresight gave it a
*planner*, the-surmise gave it *belief* — but nothing yet lets a mind *hold and
act on* what it perceives. This campaign closes the first cognition edge: felt
conditions become **drives**, a mind resolves competing drives by **arbitration**,
its species' **psychology** parameterizes that arbitration (so *how* it chooses
is *who it is*), and its **affect** — frustration, confusion, giving-up — is made
legible, debuggable, and measurable. It gives two authored-but-dormant vectors
(`ConditionNiche`, `PsychVector`) their first runtime jobs.

---

## 1. Goal and contract

Turn the felt world into behaviour, characterfully and legibly:
1. **A `Drive` abstraction** — a module with `urgency(agent, world, t) → f64`
   (error from a setpoint) and `affordance(pos, terrain) → Plan`. Thirst (the
   one existing need) is refactored into it **behaviour-preservingly**; a new
   **thermal** drive is added.
2. **Arbitration** — the composition over drives: max-urgency active drive with
   hysteresis, deterministic tie-break, fall to home when none active.
3. **Psychology** — `PsychVector.deliberation_latency` slides arbitration from
   *grab* (argmax urgency) to *weigh* (argmax utility).
4. **Affect** — a derived read of the arbitration state
   (content/searching/frustrated/lost/panicked/**helpless**), surfaced in
   narration, plus a **population health metric** (persistent negative-affect,
   *excluding* benign searching) that self-scores whether cognition works.
5. **The correspondence payoff** — flip `cold`/`heat` cognition `Absent →
   Present`.

**Contract:** entirely within the read-only, session-sandboxed vessel seam —
NPC drive/affect state lives in the session ledger clone, **never genesis**. No
new seed draws, **no save-format change, no epoch**. The `cold`/`heat` cognition
flip is a `#[serde(skip)]` manifest edge (byte-free to every world; only the
manifest view moves). The **thirst refactor is byte-identical** — today's thirst
walk is reproduced exactly (the drive determinism tests are the proof). No
census regen, no AWS spend.

## 2. The gap, precisely

- **Cognition is 0/76** (`cli/tests/correspondence.rs`). `cold`/`heat` register
  with `cognition: Absent(Void::Uncognized{"wave-cognition"})`
  (`domains/climate/src/lib.rs`).
- **One bespoke drive.** `windows/vessel/src/liveness.rs`: a single
  water-typed `DriveParams` (`rise`/`act`/`sated`), drive as a fold
  (`drive_at` = `rise × days-since-drank`), one `decide` policy, one `GoapSpace`
  goal (`hydrated`). No `Drive` trait, no drive set, no arbitration.
- **Two authored vectors sit dormant at runtime.**
  `ConditionNiche.temperature` (`domains/species/src/lib.rs`; a Gaussian
  `{optimum, width}` per species — goblin 18°C/28, kobold 6°C/14) and
  `PsychVector` (`deliberation_latency`, `time_horizon`, …) are read only by
  worldgen, never by the walking mind.
- **A creature does not perceive its own temperature** on the drive path (only
  the render path reads `temperature_c`, for prose).

## 3. The `Drive` abstraction — one shape, two sources

Every need is *error from a setpoint*, exposed identically:

```rust
trait Drive {
    fn urgency(&self, agent, world, t) -> f64;   // error from the setpoint
    fn affordance(&self, pos, terrain) -> Plan;  // how to reduce it
}
```

Two source-shapes, hidden behind the trait (and mapping onto the two planner
spaces the map already identified):
- **stock** (internal, cumulative, event-satisfied): thirst/hunger — a reservoir
  depleting over time, refilled by a consuming action. Affordance = reach a
  resource + consume (`MoveTo(water)` + `Drink`; goal `hydrated`).
- **flow** (external, reactive, state-satisfied): thermal/exposure — the current
  cell's condition vs the niche. Affordance = reach a good cell (`MoveTo(a
  comfortable cell)`; goal `comfortable(position)`, a nav-like space keyed on a
  comfort predicate — mirroring the existing home-return `NavSpace`).

**Stage 0 refactors thirst into this trait behaviour-preservingly** — the single
existing drive becomes `Drive #1`, its walk byte-identical, de-risking the
abstraction before anything new is added.

## 4. The thermal drive (the new flow-drive)

- **Perception:** add `Terrain::temperature(&RoomAddr) -> f64` beside the
  existing `elevation` (one call — `LocaleContext::describe(room, t)
  .fields.temperature_c`); the agent's cell and its 3 mesh neighbours yield
  temperature. **No aperture / look-direction** — a direct field read.
- **Urgency:** deviation of the cell temperature past `ConditionNiche
  .temperature`'s tolerance width from its optimum (its first runtime read).
- **Affordance:** gradient-follow toward the more-comfortable neighbour — a
  near-copy of `downhill_step` with `temperature` for `elevation`, direction set
  by too-cold-vs-too-hot toward the optimum; the same `total_cmp` +
  ascending-`RoomAddr` tie-break. A flow-drive needs no belief cache (it follows
  the directly-felt neighbour temperatures).
- **Per-day vs annual mean:** `describe` currently returns the annual mean;
  the per-day field (`temperature_at(cell, day)`, seasonal/diurnal) is available
  if the drive wants seasonal/diurnal cold — a sub-choice for Stage 2.

## 5. Arbitration — the composition (deterministic)

At `decide` (the single seam that turns state into `Intent`):
- Each drive computes urgency; select the **max-urgency active drive**
  (urgency ≥ its `act` threshold).
- **Hysteresis** (commitment, realized statelessly): a drive engaged at `act`
  disengages only below `act − h`; a challenger switches the served drive only
  if it exceeds the incumbent by a margin δ (incumbent inferred from the recent
  trail; a session-sandboxed errand marker is the fallback if trail-inference is
  fiddly — still not save-format).
- **Aging / no starvation:** the max-urgency rule already prevents starvation
  (a chronically-unmet drive keeps rising until it wins); confirm no pathology.
- Fall to return-home when no drive is active. All comparisons via `total_cmp`
  with ascending-`RoomAddr` ties — **reload-stable**.

## 6. Psychology parameterizes arbitration

`PsychVector.deliberation_latency` slides the selection rule:
- **grab** (low latency, impulsive): argmax *urgency* — seize the loudest need.
- **weigh** (high latency, deliberate): argmax *utility* = urgency × how well the
  chosen action serves it (e.g. distance-discounted).

This is `PsychVector`'s first runtime job. **`time_horizon` anticipation** ("act
on what *will* be urgent") is **deferred** (a clean extension of the same rule).
The tuning must stay deterministic (the psych vector is authored data; the
policy is a pure function of it + the drive urgencies).

## 7. Affect — the derived, legible read

A pure function of the arbitration state (derived, immaterial — matching
"drive == fold"), rendered as narration and sampled by the Lab:

| situation | affect | |
|---|---|---|
| no active drive / plan in progress | **content** | the normal state |
| active drive, exploring (has a gradient) | **searching** | normal — *not* confusion |
| drive + belief, no path | **frustrated** | "I want water but every path is blocked" |
| drive, no belief and no gradient | **lost** | "I don't know what to do" |
| urgent drive + no plan | **panicked** | frustration × urgency |
| sustained futility (aged) | **helpless** | gives up despite an active drive |

- **Searching ≠ confusion** is load-bearing: a creature seeking not-yet-found
  water is puttering normally; counting it would make the health metric
  meaningless. The metric measures frustrated + lost + panicked + helpless.
- **Learned helplessness** is the temporal collapse (persistent negative-affect
  → apathy). Deliberate creatures (long horizon) give up faster on truly futile
  tasks; impulsive ones keep flailing — psychology shapes even how a mind breaks.

## 8. The health metric — the creatures debug the sim

Aggregate **persistent** negative-affect across the population is a self-scoring
proxy for "does cognition work" — the cognition-layer analog of the
correspondence completeness audit. Background is near-zero (creatures ascend
their hierarchy). A spike is disambiguated by its **temporal signature**:
- **decays** → a novel/extreme *world* event (a frost, a drought) the creatures
  adapt to — legitimate.
- **persists** → a *sim* bug — an unsatisfiable need, an unreachable-but-should-
  be-reachable resource (the pinned "real-walk reachability" class).

Delivered as a Lab metric (`windows/lab`) over a seed sweep, and re-scored into
the Confidence Gradient as a self-scorable bet ("creatures satisfy the needs
they should be able to").

## 9. The correspondence payoff

Flip `cold`/`heat`'s manifest cognition edge from `Absent(Void::Uncognized)` to
`Present(CognitiveHandle)` in `domains/climate/src/lib.rs` — the audit moves
0/76 → 2/76. `CognitiveHandle` is still a unit marker, so the *justification* is
exactly this campaign's runtime cognition: `cold`/`heat` now drive behaviour, so
they hold a cognitive handle **in the game's own terms**. Byte-free to worlds
(the edge is `#[serde(skip)]`); the manifest view and its trial balance update.

## 10. Determinism

- **Sandboxed:** all new drive/affect/errand state lives in the session ledger
  clone (like `agent-at`/`drank`), never genesis; new session predicates (if
  any) register per-session, never at genesis. Reading `ConditionNiche`/
  `temperature_at` touches no committed facts.
- **Byte-identity where it must hold:** the thirst refactor (Stage 0) reproduces
  today's walk exactly; `build_world_is_deterministic`, `drive_at_*`,
  `believed_water_*`, `downhill_step_*` and the pinned reachability finding stay
  green. Every new comparison carries the `total_cmp` + ascending-`RoomAddr`
  tie-break for reload-stability.
- No new `Stream` draws; no save-format label; no epoch.

## 11. Staging

0. **Thirst → the `Drive` trait (byte-identical).** Extract the existing thirst
   into `Drive #1`; prove the walk unchanged. De-risks the abstraction.
1. **The thermal drive.** `Terrain::temperature`; the flow-drive (urgency from
   `ConditionNiche`, gradient-follow affordance); the comfort `GoapSpace`/nav
   space. A single-species creature now seeks warmth.
2. **Arbitration + psychology.** Multi-drive `decide` (max-urgency + hysteresis),
   `deliberation_latency` tuning. Two competing drives resolve; different
   psychologies diverge.
3. **Affect + the health metric.** The derived affect read (through helpless),
   narration surfacing, the Lab health metric with the decay-vs-persist read.
4. **The payoff.** Flip `cold`/`heat` cognition → `Present`; regenerate the
   manifest view.

## 12. Evidence battery

- **Behaviour-preservation (Stage 0):** the thirst walk is byte-identical; the
  existing drive determinism tests pass unchanged.
- **Determinism/reload-stability:** the thermal gradient, arbitration, and affect
  are reload-stable (recompute twice → identical); tie-breaks hold.
- **Divergence (the headline proof):** two species differing *only* in
  `PsychVector`, one thirst-vs-cold conflict, produce **visibly different walks**
  and **different affect signatures** — measured via the Lab (the project's own
  instrument), pinned honest.
- **The health metric works both ways:** a normal world shows near-zero
  persistent negative-affect; an injected novel event spikes-then-decays; an
  injected unsatisfiable need spikes-and-persists (the bug signal).
- **`ConditionNiche` is honoured:** a cold-niche species (kobold) tolerates cold
  a warm-niche species (goblin) flees — the niche drives divergent behaviour.

## 13. Success criteria

1. Drives are modules behind a trait; adding a drive touches no other drive and
   no arbitration internals.
2. Thirst is byte-identical; the thermal drive makes a creature seek comfort per
   its `ConditionNiche`.
3. Arbitration resolves competing drives deterministically; `deliberation_latency`
   visibly changes the resolution (grab vs weigh).
4. Affect is a legible derived read through learned helplessness; searching is
   excluded from the health metric.
5. The population health metric distinguishes a hard world (decays) from a broken
   sim (persists); it re-scores the Confidence Gradient.
6. `cold`/`heat` cognition flips to `Present` (audit 0/76 → 2/76), justified.
7. No genesis/save-format change; determinism tests green.

## 14. Explicitly deferred / non-goals

- **Drive coupling** (heat → faster thirst) — a lovely emergent realism, but the
  drives stay independent for the framework.
- **`time_horizon` anticipation** — acting on future urgency; a clean extension.
- **More drives** (hunger, danger/terror, social) — slot into the module without
  touching arbitration.
- **The social layers** — belief-sharing making the group the error-correcting
  unit, and the information economy (value = resource rivalrousness; leak via
  observation, the perception dual) — recorded as vision
  (`SOC-belief-sharing`, `SOC-information-economy`), parked as several campaigns.
- **A real `CognitiveHandle` payload** — it stays a marker; giving it structure
  is future cognition work.
