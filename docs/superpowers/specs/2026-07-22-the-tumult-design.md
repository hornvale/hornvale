# The Tumult — Living-Community C3, Slice 1: Predation

**Status:** design (G3 re-review — the model was reframed after slice-1's crowding sandpile was falsified)
**Program:** The Living Community engine (campaign 3 of ~5), conflict-as-criticality (SOC-criticality)
**Slice:** predation — conflict is driven by *coveting value* down a *strength* gradient, not by crowding. A genesis epoch.
**Base:** origin/main @d9f6a55e (contains The Sundering).

---

## 1. The payoff

Groups take what other groups have — and not because they ran out of room. The Sea Peoples, the
warfare between Amerindian nations with a continent of land to spare, the Europeans who took
cleared and cultivated indigenous fields rather than break wilderness: none of these are
scarcity. They are **covetousness for specific value** — a fertile valley, a mine, a flock, a
fishing ground — pursued **down a strength gradient**: the strong take from the weaker, and the
displaced, now desperate, take from someone weaker still. Shit rolls downhill.

This slice makes the deep-history bake fight over *value*, not *space*. A community raids the
reachable neighbour whose land is worth more than its own, **when it is strong enough to win** —
regardless of whether empty land exists. Taking a settled place is not a last resort; it is
often the *prize*, because pioneering unknown ground is a gamble (no local knowledge, no proven
crops) and a rival's holding comes already made to work. The loser is driven off and, still
carrying some strength, rolls onto a weaker neighbour — a cascade across the moving-sea graph.

## 2. Context — the falsified first design, and the reframe

Slice-1's *first* design modelled conflict as a **crowding sandpile**: a displaced people
raids only when no vacant land is reachable, and the cascade is the avalanche; the goal was a
power-law size distribution. It was built and measured, and it **falsified honestly**: seed-42
fires *zero* cascades (the world never crowds), and where cascades do occur they run away to the
depth cap — a truncation artifact, not a power law. The diagnosis (six ideonomy passes, ledger
#7–#8): the world sits sub-critical (no crowding → no conflict) or super-critical (runaway),
never at the critical point, because **the model had a drive but no dissipation, and — more
fundamentally — density was never the driver.**

The reframe: conflict is a **driven-dissipative predation system** on a **value field**, ordered
by **strength**. Value accumulates (growth on good land), concentrates (the strong dominate the
weak), and dissipates (war is lossy; the broken die; toppled powers release what they held).
Self-organized criticality is the *signature* of such a system at its edge — so this slice stops
*engineering* for a power law and instead models the real drivers, then **measures** whether they
self-organize. Grounding: kleptoparasitism (the skua robs the puffin), the protection racket /
Danegeld (milk the productive, don't destroy them), Ibn Khaldun's and Turchin's secular cycle,
and the dominance hierarchies of schools, prisons, and captive animals.

## 3. Architecture (constitutional layering)

Everything the mechanism needs is already in the bake — this is a **rewiring**, not a new
subsystem:

- **`windows/worldgen` — `history_bake.rs`.** The conflict logic in `step_community`/`raid` is
  rewritten. Strength reads the `Community`'s existing `population` and `tech`; coveted value
  reads the existing per-cell `capacity` field (which already folds the Demesne's per-axis supply
  and the Confluence's freshwater term — a cell's worth). The moving-sea era graph gives reach.
  The `nearest_occupied` helper (slice-1 T1) seeded the raid-target search and was ultimately
  *subsumed* — `maybe_raid` scans direct neighbours and the roll-downhill scans widening rings
  through a shared BFS helper, so the standalone function was deleted; the `relocate`
  recursion (T2) becomes the roll-downhill cascade; the `BakeCensus` cascade histogram (T1) is
  the falsification instrument; `history_for` (T3) is the measurement entry point. All reused.
- **`windows/lab` / `windows/worldgen/tests`** — the falsification metric (cascade-size
  distribution) and the preregistered gates.
- **No new domain, no new committed field/predicate/stream label.** A raid is a chain of the
  existing `CauseOfEnd::Fled` / `Ended::By` occupation records; strength and value are read from
  state already present.

## 4. The mechanism — predation

### 4.1 Strength and value (both already present)

- **Strength** of a community — its capacity to win a raid — is `population` scaled by a tech
  factor (Iron beats Bronze beats Neolithic; monotone, already tracked as `TechHorizon`).
  Heterogeneous strength is the fuel; equals do not prey on each other.
- **Coveted value** of a cell is its **era-effective** `capacity` (the existing per-cell worth
  times the era's habitability factor — raw capacity would let a raider conquer ground the ice has
  just made worthless). A community covets a neighbour whose cell is worth more than its own — the
  fertile valley, the mine.
- **The settled-land premium.** A *held* cell is worth more than an empty cell of equal capacity,
  because §1's reason is real: pioneering unknown ground is a gamble, and a rival's holding comes
  already made to work. Held cells therefore score `eff_capacity × (1 + SETTLED_PREMIUM)`. This is
  the only term in the model that *increases* conflict; every inhibition in §4.2a reduces it, and
  the ratio between them is what makes the branching ratio a measurable quantity rather than a
  structurally-zero one.

### 4.2 The raid rule (deterministic — the new trigger, density dropped)

Decoupled from pressure entirely. Each epoch, after growth, a community scans its **reachable
occupied neighbours** (over the era graph) and raids the best target that satisfies both:
1. **Covetousness** — the target's cell value exceeds the raider's own (there is something to gain).
2. **Dominance** — the raider's strength exceeds the target's by a margin (it can win).

The best target is the most valuable such cell (tie-broken deterministically by strength then
`CellId`). No target that meets both conditions ⇒ no raid this epoch. **Crowding is never the
trigger.**

### 4.2a Inhibition — predation's third factor

Predation is `motive × capability × **inhibition**`. §4.2 supplies motive (covetousness) and
capability (dominance); without a third factor every people that *can* raid *does*, which is both
unhistorical and — because inhibition is what differs between peoples — throws away the
heterogeneity that is SOC's stated fuel. Each inhibition is a modular veto in the candidate loop
(a conjunction, so they compose without interacting). Slice 1 takes the two cheapest:

1. **No spoils** (momentary) — a target already starving against its own carrying capacity has
   nothing to contend over. Nothing to take ⇒ no raid, however weak it is. This also blocks the
   pathological regress of remnants preying on remnants all the way down.
2. **Disposition** (durable) — a people whose `PsychVector.threat_response` (flee 0 ↔ stand 1)
   falls below a threshold does not raid at all, however strong it is on paper. Authored data, not
   drawn. This is the gate that makes raiding heterogeneous *across peoples*, and it produces an
   **asymmetric** aversion structure for free: A declines B while B raids A, because each people
   gates on its own trait.

The remaining gates (niche-relative value, pairwise aversion, concealment) are §9 non-goals.

(The pre-existing climate paths are unchanged: a cell turned hostile by the ice still drives
*migration* to a refuge or death; over-capacity still starves — those are not conflict.)

### 4.3 The outcome, and dissipation (the fix that was missing)

A raid resolves deterministically and **lossily**:
- The raider **seizes** the coveted site (conquest of immobile land): it takes the cell; the loser
  is driven off (`Fled`, `ended_by = By(raider)`).
- **War is lossy** — a fraction of the combined population is destroyed in the taking (not merely
  transferred). This is the primary dissipation: value leaves the system.
- The displaced loser, still carrying its (reduced) strength, **rolls downhill** — it re-enters
  the raid rule against *its* weaker neighbours, cascading. Each hop it loses more (the war-loss
  and the journey), so a displaced remnant that falls below a **viable minimum dies** rather than
  cascading forever — the second dissipation, and the natural avalanche cutoff.

**"Re-enters the raid rule" is literal — one rule, one substituted baseline.** A raider compares a
candidate against *what it already holds*; a homeless roller holds nothing, so its baseline is
*the best thing it can get for free*. The displaced people therefore makes **one** comparison over
every reachable cell — vacant cells at `eff_capacity`, held cells at
`eff_capacity × (1 + SETTLED_PREMIUM)` and admitted only when the roller clears the dominance
margin over the holder — and takes the best; nothing admissible ⇒ it is lost. This replaces the
vacant-first special case, which was never in this spec: under vacant-first a roller prefers a
marginal empty cell to a rich held one, the branching ratio is zero *by construction* rather than
by physics, and §5's open question cannot be asked.

**Locality is part of the rule, not an afterthought.** "Re-enters the raid rule" also inherits the
raid rule's *neighbourhood*: a raider scans its traversable neighbours, so a roller's comparison is
scoped the same way. The scan walks the era graph outward and stops at the **first ring that
contains an admissible option**, choosing the best value *within that ring* — the codebase's own
existing idiom (both `nearest_dest` and the occupied-cell search are nearest-first BFS with
deterministic tie-breaks). A people may still migrate a long way when its whole neighbourhood is
full, but it never crosses a continent for a marginally better cell, and the settled premium
decides between a vacant and a held cell **at the same distance**, which is the only place it
should decide. A scan over *every* reachable cell — briefly specified here and measured — drops the
distance term entirely: the occupied set drifts toward the globe's high-capacity cells, foundings
inflate, and the world-level population-conservation gate breaches. That is a defect of this
spec, not of the mechanism.

**The strategy is emergent, not enumerated.** A strong remnant preys — it beats holders, and proven
ground scores higher. A weak one flees to the empties — it beats nobody, so held cells never enter
its option set at all. "Shit rolls downhill" falls out of the strength gradient; there is no
`if migrating else raiding` branch anywhere in the mechanism.

The cascade size is the number of displacements in one relaxation (the existing histogram). The
branching ratio — does one raid trigger on average more or fewer than one downhill raid — is what
determines sub-critical / critical / super-critical, and whether the strength-and-value gradients
**self-tune it toward one** (criticality) is the slice's open question, measured not assumed.

### 4.4 Determinism (Lorenz-safe)

Groups do not *choose* — the raid is a total, deterministic function of the frozen epoch state
(strength, value, graph reach, `f64::total_cmp` / `CellId` tie-breaks). No agent decision, no new
seed draw, no stochastic forward-integration of a chaotic pressure variable (the raid reads
state; it does not integrate an ODE). Bounded cascade depth. Same seed ⇒ byte-identical skeleton.

## 5. The falsification metric (headline) — SOC as a measured consequence

The cascade-size distribution is measured over a seed sample and its shape adjudicated:
- **Power law** (heavy-tailed over ≥ ~1.5 decades) ⇒ the predation system self-organized to
  criticality — the payoff.
- **Bell / spike / geometric / no conflict** ⇒ documented falsification, diagnosing the next
  missing ingredient (the explicit dominance hierarchy and its collapse-release; or cohesion).

Unlike the first design, we do **not** engineer toward the power law — value × strength drives the
raids, dissipation bounds them, and the shape is whatever emerges. Either outcome ships.

**Disclosure — this metric's mechanism was amended after an unfavourable observation.** Task 1
shipped and measured first: seed-42 went from 0 raids to 71 with land to spare (the headline), but
the cascade histogram came back **all-zero**, because 69 of 71 losers found vacant land at the
first hop. The displaced-relocation rule was then changed (§4.3: one best-value comparison over
vacant *and* held cells, with the settled-land premium) — a change that raises the branching ratio
by construction. The justification is spec-fidelity, not the metric: §1 already asserted that a
rival's holding "comes already made to work", and §4.3 already said the loser "re-enters the raid
rule", neither of which the vacant-first implementation encoded; under vacant-first the branching
ratio is structurally zero and §5's open question is unaskable rather than answered. The amendment
is nonetheless **post-observation** and is labelled as such wherever this result is reported — in
the Task-3 readout, the chronicle, and the retrospective. The falsification rule is unchanged: the
measured shape ships whatever it is, and no constant is tuned toward a power law.

**Amendment 2 was also post-observation, and it moved the headline the other way.** The locality
clause in §4.3 — nearest-admissible-ring rather than a scan of the whole component — was written
after a *second* unfavourable observation: the unrestricted scan breached the world-level
population-conservation gate. Restoring locality cut seed-42's cascades from 6 to 1 and removed the
campaign's largest single cascade. So the two amendments pull in opposite directions on the metric,
and neither was made to improve it: amendment 1 restored what §1 and §4.3 already asserted, and
amendment 2 repaired a distance term this spec had dropped by accident. Both are labelled
post-observation under the same rule, and the honest consequence is that the seed-42 histogram is
now too thin to adjudicate on its own — §5's verdict rests on the pooled seed sample, and a
strongly sub-critical branching ratio is a legitimate falsification that ships as one.

## 6. Scope — what is slice 1, and what is the next slice (a G3 decision)

**Recommended slice 1 (this spec):** the minimal driven-dissipative predation model of §4 —
value × strength raiding, lossy war, the viable-minimum death, and the roll-downhill cascade.
This is the honest first measurement of "does covetous predation with real dissipation
self-organize?"

**Deferred to the next slice (flagged for Nathan at G3):** the **explicit dominance hierarchy**
— tribute/subordination links (extraction that *milks* rather than evicts, the Danegeld), and the
**collapse-release** (toppling a dominant frees its whole subordinate network as one large
avalanche — the richest dissipation and the clearest power-law source). If slice-1's lossy
dissipation proves insufficient (falsified), this is the diagnosed next lever. Also deferred:
captives/enslavement (taking people as the prize), revenge/grievance (a fold over the raid
ledger), status/prestige and sacred motives. *If Nathan wants the dominance hierarchy in slice 1,
the spec expands to include the subordinate-link + collapse-release; the recommendation is to
measure the minimal model first.*

## 7. The epoch

A genesis epoch: the conflict rewrite changes which communities survive and where, so the
committed skeleton moves — byte-identity breaks, the census regenerates on lefford (0063 —
carve-out, authorized at G6), the seed-42 keystone refreezes at merge. No new committed field,
predicate, or stream label.

## 8. Success criteria — measure, don't narrate

1. **Conflict fires on value, not crowding.** Raids occur on seed-42 (which never crowded) —
   proving the driver is value × strength, not density. If conflict is still inert, the raid
   rule's margins are a calibration finding for Nathan (a fidelity carve-out), not a floor.
2. **The map is not depopulated.** Lossy war + downhill cascades must not empty the world:
   alive-at-`now` stays in the walkable band and the Famine/war-loss share stays under a
   preregistered ceiling. Runaway depopulation is a fidelity finding, never floored.
3. **The falsification metric (headline).** The cascade-size distribution is measured and its
   shape adjudicated (power law vs bell/spike/geometric), across a seed sample. Both outcomes
   ship; a falsification diagnoses the next slice.
4. **A cost gate** bounds the conflict bake wall-time and the max cascade depth reached.

## 9. Non-goals (§9 — read before assuming scope)

- **Crowding / density as a conflict trigger** — dropped; it was never the driver.
- **The explicit dominance hierarchy + tribute/extraction + collapse-release** — the next slice
  (unless promoted at G3). Every *standing relationship* between communities belongs here:
  tribute/Danegeld, alliance, vassalage, colonial rule, employment. They are deferred for a
  structural reason, not a scheduling one — a one-shot outcome is already expressible as a
  `CauseOfEnd`/`Founding` chain in the occupation record, whereas a standing relationship needs a
  persistent inter-community relation the ledger has no shape for (a save-format change and a real
  subsystem, whose substrate is the derived typed-edge social graph).
- **Captives/enslavement, revenge/grievance, status/prestige, sacred motives** — later slices.
- **Cohesion (ʿasabiyya)** — the secular-cycle regulator — a later slice.
- **The remaining inhibition gates of §4.2a** — niche-relative value, pairwise aversion, and
  concealment. Niche-relative value is the notable one: it is arguably a *correctness* fix rather
  than an enrichment, since the bake takes a single global `capacity` field and thereby asserts
  every people values every cell identically — a claim the existing `ConditionNiche` contradicts.
  It is deferred on cost (the bake's capacity input would have to become per-people), not on merit.
- **The wider contact space** — pillage, rent-seeking, colonization, genocide, assimilation,
  proselytization, employment, alliance, trade. Raiding is one cell of it; the program frame and
  its generative kernel (the rivalry of the prize) are captured in the idea registry.
- **A new committed field, predicate, or stream label** — the raid uses existing record shapes.

## 10. Definition of Done (per CLAUDE.md)

- `step_community`/`raid` rewritten to value × strength predation (density dropped); lossy war +
  viable-minimum death + roll-downhill; deterministic + bounded; the histogram tallies cascade sizes.
- The §8 gates plus the cost gate pass (or the falsification is documented, labelled).
- Census regenerated on lefford (authorized at G6); the census-close cascade re-pinned; the
  seed-42 keystone refrozen.
- Chronicle, retrospective (including the crowding→predation reframe and the six-pass ideonomy
  pivot), book freshness sweep, Confidence Gradient re-score (the SOC bet moves), registry flip
  (SOC-criticality → elaborated/slice-1 with the measured result), full gate + artifact drift.

## 11. What is salvaged from the falsified first design

The falsified crowding build (commits through `f8f52397` on this branch) is **not discarded** —
its infrastructure is exactly what this model needs: `nearest_occupied` (the BFS whose ring-walking
survives inside the shared helper, though the function itself was subsumed and deleted),
`relocate` (the roll-downhill recursion), the `BakeCensus` cascade histogram, `history_for` (the
measurement entry point), and the gate scaffolding all carry forward. What changes is the raid
**trigger** (crowding/no-vacant → value × covetousness) and the raid **decision** (displace-only-
when-forced → prey-on-the-weaker-for-gain), plus the two dissipations (lossy war, viable-minimum
death). The plan will edit forward from the current branch state, not revert it.
