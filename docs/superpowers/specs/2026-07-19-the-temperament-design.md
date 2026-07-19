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
- **Per-day, not annual mean.** The drive reads the *per-day* field
  (`temperature_at(cell, day)`, seasonal + diurnal) rather than `describe`'s
  annual mean, so a creature seeks warmth at dawn and shade at noon: arbitration
  over the temporal field yields a lived **diurnal rhythm** (§5, cyclicity). A
  small `describe`/`Terrain` extension threads `day` through.

## 5. Arbitration — action-centric, deterministic

At `decide` (the single seam that turns state into `Intent`), arbitration is
**action-centric, not drive-centric**: it does not pick a drive and follow its
gradient — it picks the candidate action (the ≤3 neighbour `MoveTo`s plus any
consume/bask) that best reduces the *weighted drive-set*, so one move can serve
two needs at once (a cell both warmer and nearer water):

- **Utility of an action** = `Σ_drives urgency_d × reduction_d(action)`; pick the
  max-utility action. (Max-urgency-single-drive is the degenerate special case.)
- **`deliberation_latency` slides the rule** (§6): *grab* counts only the loudest
  drive's own reduction (myopic); *weigh* counts the full weighted sum.
- **Soft Maslow via urgency ceilings, not a priority table.** Survival drives
  (thirst → death) can reach urgency 1.0; comfort drives (thermal discomfort) cap
  lower — so severe cold beats mild thirst while nothing beats dying of thirst.
  The hierarchy *emerges* from the urgency ranges.
- **Commitment is an explicit behavioural mode** — `Pursuing(drive)` / `Homing` /
  `Idle`, session-sandboxed (a tick-local mode, never save-format). Transitions
  are hysteretic: engage a drive at its `act`, release below `act − h`, switch the
  pursued drive only when a challenger's utility exceeds the incumbent's by a
  margin δ. The mode *is* the errand — no trail-inference needed; it prevents both
  boundary-dithering and mid-errand flip-flop, buying **purposeful, alive**
  behaviour over twitchy reflexes.
- **No starvation:** an unmet drive's urgency keeps rising until it wins; confirm
  no pathology. Fall to `Homing` when none is active. All comparisons via
  `total_cmp` with ascending-`RoomAddr` ties — **reload-stable**.

## 6. Psychology parameterizes arbitration

`PsychVector.deliberation_latency` slides the action-selection rule (§5):
- **grab** (low latency, impulsive): the action best serving the *single loudest*
  active drive — the nearest relief for the biggest need.
- **weigh** (high latency, deliberate): the action maximizing the *weighted sum*
  over all active drives — the move that best relieves total discomfort.

This is `PsychVector`'s first runtime job, and the seat of temperament: same
conflict, same mechanism, a different creature. **`time_horizon` anticipation**
("act on what *will* be urgent") is **deferred** (a clean extension of the same
rule).
The tuning must stay deterministic (the psych vector is authored data; the
policy is a pure function of it + the drive urgencies).

## 7. Affect — the valence × arousal read

Affect is a pure function of the arbitration state (derived, immaterial —
matching "drive == fold"), grounded in the psychological **circumplex**: a point
in **valence × arousal**, both already derived —
- **arousal = max urgency** over active drives (how activated the mind is);
- **valence = making-progress − blocked** (are the pursued drives *reducing*, or
  is the affordance failing?).

The labels are *regions* of that plane, and affect additionally carries its
**intentional object + reason** (frustrated *about* water, *because* every path
is blocked) — the object/reason is what makes it debuggable and *is* the "message"
a creature emits:

| region (valence, arousal) | label | |
|---|---|---|
| positive, low | **content** | needs met; puttering — the normal state |
| positive, high | **eager / relieved** | chasing a satisfiable need; a drive just met ("drinks deep") |
| neutral, mid | **searching** | seeking with a gradient — normal, *not* confusion |
| negative, high | **frustrated / panicked** | blocked × urgency: "want it, can't reach it" |
| negative, low | **lost** | no basis to move: "don't know what to do" |
| negative, low, *persistent* | **helpless** | given up despite an active drive |

- **Searching ≠ confusion** is load-bearing: a creature seeking not-yet-found
  water is puttering normally; counting it would make the health metric
  meaningless. The metric measures the negative-valence regions, not searching.
- **Positive affect is first-class** (relief on satisfying a drive, eager
  pursuit) — the read is not negative-only.
- **Learned helplessness is a *sticky* scar** — most affect flips with the drive,
  but giving-up reverses slowly (the trust-breaks-fast asymmetry), so a helpless
  creature does not instantly re-engage on a new opportunity. Deliberate creatures
  (long horizon) give up faster on truly futile tasks; impulsive ones flail —
  psychology shapes even how a mind breaks.

## 8. The health metric — a self-scoring family, anchored to a control

Population affect is a self-scoring health signal for the cognition layer — the
analog of the correspondence completeness audit, a View over the population's
affect with a temporal axis. It is a **family**, not one number (the
epidemiology/SRE decomposition):

- **prevalence** — fraction in negative affect *now* (instantaneous distress).
- **chronicity** — fraction *persistently* stuck (helpless/frustrated ≥ N ticks).
  **The bug alarm.**
- **recovery-rate** — the half-life of a distress spike (fast = resilient; none =
  stuck). The decay-vs-persist, quantified.
- **by-cause / by-species** — distress attributed per drive and per species
  (a cold-niche species in a warm world reads high, and *should*) — diagnostic.

**The temporal signature disambiguates a hard world from a broken sim:** a spike
that *recovers* (short half-life) is a novel/extreme world event (a frost, a
drought) the creatures adapt to — legitimate; a spike that *persists* (no
recovery, elevated chronicity) is a bug — an unsatisfiable need or an
unreachable-but-should-be-reachable resource (the pinned "real-walk reachability"
class).

**The metric is anchored, not thresholded arbitrarily** (the "The Named" lesson —
the anchor is the checkable part, not the drift-check). The baseline is a **null
control**: in a resource-abundant, niche-matched world, persistent distress ≈ 0;
the metric is *deviation from that floor*, preregistered and drift-checked,
exactly as the divergence method works. Delivered as a Lab metric
(`windows/lab`) over a seed sweep, re-scored into the Confidence Gradient as a
self-scorable bet.

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
- **The health metric works both ways, against the anchor:** the null control
  (resource-abundant, niche-matched) reads ≈0 persistent distress — the floor;
  an injected novel event spikes then *recovers* (short half-life); an injected
  unsatisfiable need spikes and *persists* (elevated chronicity, no recovery —
  the bug signal). Measured as deviation from the control, preregistered.
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
