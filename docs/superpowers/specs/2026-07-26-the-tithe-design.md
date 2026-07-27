# The Tithe — Living-Community C3, Slice 2: Tribute

**Status:** G3 CLEARED (Nathan, 2026-07-26). Two calls are **provisional and refinable** rather
than deeply justified, and are flagged here so a later reader does not mistake them for settled:
(a) the productivity test for the subordinate branch is `pressure < 1` — a reasonable reading of
"has surplus to take", not a derived threshold; (b) a second bid on an already-subordinated
community **transfers** patronage with no contest, which means patrons can be quietly poached.
Both are cheap to revise once the mechanism is measured.
**Program:** The Living Community engine (campaign 4), conflict-as-criticality
**Slice:** tribute — a raid may end in *subordination* rather than eviction, and the rate is a
guess on both sides. A genesis epoch.
**Base:** `main` @`d6caa514` (contains The Tumult).

---

## 1. The payoff

The Tumult built predation and measured it honestly: **the power law was falsified**, branching
ratio **σ ≈ 0.051**, geometric with a hard cutoff, deeply sub-critical. Its diagnosis was precise
and is this slice's mandate — the campaign's two failed builds bracket the answer:

| build | drive | dissipation | accumulation | result |
|---|---|---|---|---|
| crowding sandpile | yes | **none** | none | runaway to the depth cap |
| predation (The Tumult) | yes | yes | **none** | σ ≈ 0.051, nothing chains |
| **tribute (this slice)** | yes | yes | **yes** | measured |

Self-organized criticality needs drive *and* dissipation *and* something to accumulate — a
structure that stores what the drive produces until it can no longer hold, and then releases.
Predation has no such store: every raid discharges immediately, because taking a cell is a
one-shot transfer. **A standing relation is the store.** A dominant that milks rather than evicts
grows *without moving*, and what it accumulates is a topple-able structure.

The historical case is the one the spec has cited since The Tumult's pivot: the Danegeld, the
protection racket, tax farming. You do not burn the productive village. You come back next year.

## 2. Context — what is already true

- **The ledger already has the shape, and the contrary diagnosis was wrong.** The Tumult's spec
  §9 and its close-time investigation both asserted that a standing inter-community relation
  "needs a persistent relation the ledger has no shape for — a save-format change and a real new
  subsystem." Re-observed at this campaign's opening: `kernel/src/ledger.rs` defines
  `Fact { subject: EntityId, predicate: String, object: Value::Entity(other), day: Option<f64> }`
  — a typed, directed, dated entity→entity edge, **already in use** by the bake's own
  `occ-ended-by` and by religion's `held-by`. A tribute relation needs a *registered predicate*,
  not a new data shape. (This is the "an inherited diagnosis is a hypothesis" lesson: re-observe
  before designing.)
- **Asset mobility was designed and then lost.** The Tumult's decision ledger #8 specified
  "outcome by asset-mobility (mobile ⇒ rustle/extract, target survives; immobile ⇒
  conquer/displace)". It never reached that spec. It is recovered here, and it is also exactly
  what `SOC-contact`'s rivalry kernel predicts.
- **The psych substrate is already wired into the bake.** The Tumult threaded a per-species
  lookup for `MindVector.threat_response` (the disposition gate) through `BakeConfig`. This slice
  reuses that channel for `SocietyVector`, which is carried **solely by `Settled` kinds** — exactly
  the peoples that form tribute relations.

## 3. Architecture (constitutional layering)

A rewiring, not a new subsystem.

- **`windows/worldgen` — `history_bake.rs`.** `maybe_raid` gains a second outcome; the `Bake`
  gains a live relation table and a per-epoch collection step. Everything reads state already
  present (`population`, `eff_capacity`, the era graph, the psych lookup).
- **`windows/worldgen` — `history_emit.rs`.** The relation is emitted as dated facts, exactly as
  occupation records are.
- **`domains/history`** — one new registered predicate.
- **`windows/lab` / `windows/worldgen/tests`** — the falsification metric and the gates.
- **No new domain, no new crate, no new `Fact` shape, no new `CauseOfEnd` variant.**

## 4. The mechanism

### 4.1 The subordination trigger — asset mobility decides

`maybe_raid` today evicts when the target's cell is worth more than the raider's. It now resolves
two outcomes, both still gated by dominance (`strength(raider) > strength(target) × RAID_MARGIN`)
and by the shipped inhibitions (no-spoils, disposition):

1. **The prize is immobile — the cell.** `eff_capacity(target) > eff_capacity(raider)`. Land, a
   mine, forage: takeable only by *occupying* it. ⇒ **evict and seize** (the shipped path,
   unchanged).
2. **The prize is mobile — the people and their product.** The target's cell is *no better*, but
   the target is **productive** — it has growth headroom (`pressure < 1`), so there is a surplus
   to take. ⇒ **subordinate**: the target keeps its cell and begins paying tribute.

Neither ⇒ no raid this epoch.

**Branch 2 is genuine new motive, not a relabelling.** The shipped covet gate does
`if t_val <= raider_val { continue }` — a strong community ignores a poorer neighbour outright.
Under tribute, a neighbour whose *land* is no prize but whose *people* are productive becomes
worth milking. That is the accumulation term: the dominant grows **without moving**.

Note the productivity test is the *inverse* of the shipped no-spoils veto and composes with it
cleanly: a starving target has nothing to seize *and* nothing to farm.

### 4.2 The negotiation — three terms, no new draw

The rate is not a constant. Nobody in this world runs double-entry accounting; both sides are
guessing, and the subjugated withhold. Lifted, this is the **principal-agent problem under
asymmetric information**, whose historical result hands us a deterministic mechanism — because the
asymmetry is already structural in the bake:

| term | reads | meaning |
|---|---|---|
| **assessment** | the target cell's `eff_capacity` × `ASSESS_RATE` | what the dominant *demands*, set from what it can **see** |
| **remittance** | `min(assessment, (surplus + bleed) × (1 − concealment))`, where **surplus is that epoch's growth increment** and **bleed is what can be taken from the standing stock above `FARM_FLOOR`** | what the subordinate *hands over*, paid from what it **has** |
| **concealment** | `(1 − in_group_radius) × CONCEAL_MAX` | the gap the subordinate controls — an insular people hides more from outsiders |
| **shortfall** | `assessment − remittance` | the only signal the dominant reads |

`eff_capacity` and `population` are already two different numbers, so **the information asymmetry
costs nothing and is fully deterministic**. Land tax has always been assessed on area, never on
the granary, for exactly this reason.

### 4.2b A greedy patron can bleed its vassal — the third amendment, and it reverses one of this spec's own rules

**Amendment 3 (owner's call, 2026-07-27), made with its costs stated.** Earlier text capped
remittance at the epoch's growth increment so that tribute would *milk rather than kill*. Task 5's
implementation measured the consequence: that cap guarantees
`population_after ≥ population_at_epoch_start`, so **the tribute loop's own health signal can never
go negative.** The demand eases only when war, famine, climate or crowding hurts the vassal — never
because the patron over-extracted. §4.3's "over-extract → collapse → relax" therefore **did not
close inside the mechanism**, and a second bound sat on top of it: once the assessment exceeds a
vassal's increment the vassal is milked exactly flat, and a flat vassal emits signal `0.0`, so the
demand stops easing and the pair parks.

Milk-don't-kill and the secular cycle want opposite things. The owner chose the cycle:

```
bleed      =  max(0, population − FARM_FLOOR)          // what a patron may take from the STOCK
remittance =  min(assessment, (surplus + bleed) × (1 − concealment))
```

A patron demanding more than the surplus now genuinely **shrinks** its vassal, the health signal
goes negative from tribute alone, the demand eases, the vassal recovers — and the loop closes
inside the mechanism, which is what §1 sells and what neither prior formulation could deliver.

**`FARM_FLOOR` is a floor, not an exemption.** A vassal may be bled down toward it but not through
it, so tribute alone still cannot drive a community to extinction — §8.3 stands, with its claim
restated: *no community is farmed below `FARM_FLOOR` by tribute alone.* Set `FARM_FLOOR` at or above
`VIABLE_MIN` so a bled vassal remains a viable community rather than a husk.

**What this supersedes.** The per-subordinate between-epoch population floor (the guard three tasks
were built against, and itself the fix for this campaign's third non-binding assertion) is **no
longer the invariant** — it is replaced by the `FARM_FLOOR` floor, which is the weaker but now-true
claim. Every seed-42 measurement from Tasks 1–5 becomes a superseded baseline; §5's adjudication
runs on the post-amendment mechanism, and the pre-amendment numbers are retained in the campaign
record as the measurement that *motivated* the amendment, not as results.

### 4.2a Where tribute lands — the store (this is the accumulator)

**Remittance must NOT be added to the dominant's `population`.** A dominant's cell capacity is
unchanged by conquest, so population gained from tribute drives
`pressure = population × NEED / eff` upward until `COLLAPSE_PRESSURE` kills it of Famine: **a
successful extractor would eat itself**, and the readout would report "accumulation does not chain"
when the truth is that mass was added to a fixed container. Tribute therefore lands in a new
per-community scalar:

```
Community { …, stores: f64 }          // wealth, not bodies

remittance  →  dominant.stores
strength     =  (population + stores × STORE_WEIGHT) × tech_weight(tech)
pressure     =  population × NEED / eff          // UNCHANGED — stores never eat
```

Historically exact: tribute becomes granaries, walls and retainers — strength the *local land does
not have to feed*. It also gives §1's criticality argument a literal accumulator rather than a
metaphorical one. `stores` decays slowly (`STORE_DECAY`) so a hoard is not immortal, and it is
lost with the community when it closes — a dominant's fall releases what it held.

**This generalises beyond the slice.** Tree-finding on "one community's product ending up with
another" gives seizure (one-shot), tribute (recurring, coerced), **trade** (recurring, voluntary)
and gift; trade needs this identical `stores` concept, so the accumulator is the shared
prerequisite for the wider contact program, not a slice-2 local.

`in_group_radius` (insular 0 ↔ expansive 1) lives on `SocietyVector`; wiring concealment and
secrecy to it is the reading `SOC-information-economy` already argues for.

**Both errors destabilise, in opposite directions.** Under-assess and the subordinate accumulates
until it can throw off its patron; over-assess and it is crushed. Fixed points do exist — a
concealment that exactly offsets an over-assessment sits still — so the honest claim is that this
system has **no *attracting* equilibrium**, unlike a fixed rate. That is the point, and the direct
answer to why the crowding build was smooth and raid-free.

### 4.3 Adaptive demand — the oscillator

**Corrected before implementation; the first formulation could not oscillate.** It read
`assessment += shortfall × ADAPT_RATE` with `shortfall = assessment − remittance`. But
`remittance = min(assessment, …) ≤ assessment` by construction, so **shortfall is non-negative
always** and the rule is a monotone **ratchet** to the ceiling, not a feedback loop. "Over-extract
→ collapse → tribute falls → demand relaxes" had no mechanism by which demand could ever relax.
A one-signed error term cannot produce a cycle.

**The patron feeds back on its subordinate's *health*, not on the shortfall.** Each epoch after
collection it compares the subordinate against what it saw last time:

```
signal      =  (population_now − population_at_last_visit) / population_at_last_visit
assessment +=  signal × assessment × ADAPT_RATE          // clamped to [0, eff_capacity × ASSESS_MAX]
```

A vassal that **grew** can bear more, so the demand rises; one that **shrank** is being
over-milked, so the demand eases. The error term is genuinely **two-signed**, so the loop can
overshoot in both directions — which is what makes it an oscillator rather than a ratchet. It also
matches the historical story the spec has cited since the pivot: a tax farmer who kills the village
collects nothing next year, and learns. Feedback with delay (the delay is the epoch step, free)
then reproduces the **Ibn Khaldun / Turchin secular cycle** as a *consequence* rather than an
authored feature — and oscillators parked near a threshold are how SOC systems actually sit at
criticality, which is precisely what neither prior build achieved.

**The demand must be able to bind, and the original constants made that impossible.** With
`NEED = 1.0` and `GROWTH_RATE = 0.2`, the logistic increment is
`0.2 × N × (1 − N/eff)`, maximised at `N = eff/2` and therefore **never exceeding `0.05 × eff`**.
An `ASSESS_RATE` of `0.1` puts the assessment at `0.1 × eff` — at least twice the largest surplus
the subordinate's land can ever produce — so `min(assessment, surplus)` selects the surplus branch
on every world, the assessment is decorative, and adapting it changes no remittance anywhere.
**`ASSESS_RATE` must therefore sit below the logistic ceiling `GROWTH_RATE / 4`**, and the two
constants are coupled: any future change to `GROWTH_RATE` re-opens this. A test pins the
relationship rather than the value, so the coupling cannot rot silently.

### 4.4 Representation and lifecycle

- **Live during the bake** — a relation table on `Bake`, alongside `node_index`. Deterministic
  container (`BTreeMap`), iterated in key order.
- **Emitted as dated facts** at the end, exactly as occupation records are: one new registered
  predicate carrying `Value::Entity(dominant)` on the subordinate's subject, dated by `day`.
- **Dissolution is a coherence floor, not a feature.** A relation ends when either party's
  community closes. This is required for the model to be coherent and is *not* the deferred
  collapse-release; what is deferred is the freed subordinates *cascading*, not the cleanup.
- A community has **at most one** patron, and a subordinate may not itself take one. Slice 2's
  relation graph is therefore a set of **one-level stars**, not a tree — depth is the deferred
  chaining lever (§9), so cycles are structurally impossible rather than merely prevented.
- **A second bid on an already-subordinated community transfers the patronage — but only against
  hysteresis.** A raider must clear dominance over the **incumbent patron** (`strength(raider) >
  strength(incumbent) × RAID_MARGIN`), not merely over the subordinate. The old patron still does
  *not* contest — contesting is the deferred protection lever (§9) — it simply loses the relation
  to someone who plainly out-muscles it. Stated explicitly because otherwise the bake's iteration
  order would decide it silently, which is precisely the class of accident the determinism
  discipline exists to prevent.

  **This was revised on measurement, exactly as §3 said it would be.** Without hysteresis the rule
  produced ~87% churn: in a fixture where *no community ever closes* — so dissolutions are
  impossible — 259 formations occurred against a ceiling of 34 first-time subordinations, with only
  3 relations standing at the end. Rival patrons were swapping the same targets back and forth every
  epoch. That defeats the slice's own premise: a store cannot accumulate if the collector changes
  each epoch, and §4.3's adaptive demand can never build history on a relation whose assessment is
  reset to `eff_capacity × ASSESS_RATE` by every transfer.
- **Relation depth is forbidden, and must be enforced rather than assumed.** Keying the table by
  subordinate bounds *out-degree* to one, which is a functional graph — a shape that still admits
  chains and cycles. One-level stars additionally require that **a raider which is itself a
  subordinate takes no vassal, and a target which is itself a patron is not subordinated.** Both
  checks are required; measurement without them showed 57–89% of standing relations sitting under a
  patron who was themselves paying someone. Depth is the deferred chaining lever (§9), and §5
  preregisters the headline on its *absence*, so allowing it in by omission would adjudicate a
  different model than the one preregistered.
- **Cardinality is deliberately unbounded**: a dominant may hold any number of subordinates. No
  arbitrary cap is imposed, because whether a runaway hub forms is exactly the kind of thing this
  slice should *measure* rather than legislate. The maximum subordinates held by any one community
  is therefore a reported metric (§8), and a runaway is a finding, not a failure.

### 4.5 Determinism (Lorenz-safe)

Assessment, remittance, concealment and adaptation are total, deterministic functions of frozen
epoch state and authored species data. **No new seed draw.** No agent decision — the "guess" is a
reading of a visible proxy, not a choice. `BTreeMap`/`BTreeSet`/`Vec` only; every float comparison
via `f64::total_cmp` with a deterministic tie-break. No wall-clock. Assessment is clamped to
`[0, eff_capacity × ASSESS_MAX]`, so no relation diverges and no dominant demands more than its
subordinate's land could ever produce.

**The adaptive loop needs a bound, and the bound must be verified rather than asserted.** A
first-order feedback *with delay* — which is exactly what §4.3 is, the delay being the epoch step —
period-doubles into chaos above a critical gain. This is the precise claim the Lorenz guard-rail
exists to police, so `ADAPT_RATE` carries a stability bound and a test demonstrating the
per-relation assessment series converges or oscillates boundedly rather than diverging. Note the
save-format question is separate and already settled: the whole bake replays from the seed, so
nothing chaotic is ever resumed from a quantized checkpoint.

## 5. The falsification metric (headline)

**Primary — re-measure the cascade-size distribution and adjudicate its shape**, on the same
instrument The Tumult used (`cascade_sizes` via `history_for`), with its committed pooled sample
(seeds 1..=30) and its wider replication (1..=100). The preregistered question: **does adding
accumulation move the branching ratio off σ ≈ 0.051?**

- **Heavy-tailed over ≥ ~1.5 decades** ⇒ accumulation was the missing ingredient; SOC confirmed.
- **Still geometric / sub-critical** ⇒ a second documented falsification, which ships, and which
  says accumulation *alone* is not enough — diagnosing depth (chaining) or release as the next
  lever.

**Secondary, on its own named axis — the secular cycle.** Measure tribute volume and total
population over bake time and test for **oscillation** (a dominant non-zero period), reported
whether or not the cascade distribution moves. This is a separate claim on a separate axis and is
adjudicated separately; it must not be bundled into the primary verdict.

**This axis became live only at amendment 3, and that history is part of the result.** Under the
pre-amendment cap the axis was a *structurally predictable null* — the health signal could not go
negative from tribute, so no amount of measurement could have found a tribute-driven cycle. §4.2b
closed that loop deliberately. So a cycle found here is evidence about the amended mechanism and
must be reported as such; it is **not** evidence that the original milk-don't-kill formulation
cycles, and any readout that omits the amendment is misleading. Use the **per-relation** tribute
series, not raw volume — raw volume tracks the relation count and would report the population's
shape rather than the demand's.

**No constant is tuned toward a heavy tail.** Both outcomes ship. If the mechanism proves inert or
the world depopulates, that is a calibration finding for Nathan, never a floor.

## 6. Scope

Slice 2 is the minimal accumulating structure: the subordination trigger, the three-term
negotiation, adaptive demand, and lifecycle. **The knobs stacked here are opposed** — assessment
raises extraction, concealment lowers it — so, per The Tumult's sequencing lesson, they ship in
**measured stages with a seed-42 readout between each** (`history_for`, not a full census regen —
the census is a once-per-campaign carve-out at the close), and the plan must preserve attribution.

## 7. The epoch

A genesis epoch: subordination changes which communities survive and how they grow, so the
committed skeleton moves. Census regenerates on `lefford` (0063 — a carve-out needing Nathan's
explicit authorization at G6), keystones refreeze at merge. **One new registered predicate** — the
first committed-vocabulary addition since the slice-1 work, and the reason this spec's §2 opens by
re-deriving that the `Fact` shape itself does not change.

## 8. Success criteria — measure, don't narrate

1. **Subordination fires.** Seed-42 forms tribute relations at volume, on targets the shipped
   covet gate would have ignored — proving branch 2 is new motive, not a relabelling.
2. **The structure accumulates.** A dominant's strength measurably rises from tribute without it
   changing cell — the thing predation could not do. Specifically: `stores` rise while `pressure`
   does **not**, so a successful extractor does not starve itself (§4.2a). The maximum subordinates
   held by any one community is reported alongside.
3. **The map is not depopulated** and **no community is farmed below `FARM_FLOOR` by tribute
   alone** (§4.2b's restatement — a vassal may be bled toward the floor, never through it);
   alive-at-`now` stays in the walkable band.
4. **The headline (§5)** is measured and adjudicated, with the secular-cycle axis reported
   separately.
5. **A cost gate** bounds the bake wall-time and the relation-table size.

## 9. Non-goals — all captured in the idea registry, none discarded

- **Protection / the down-flow** — a patron shielding its vassals from third-party raids, and the
  over-reach it creates. The strongest deferred lever; it changes the shipped raid rule.
- **Chained tribute (depth)** — a vassal's vassal remitting upward. Depth is what makes an
  avalanche *large*; deferring it is why §5 admits a sub-critical result as a live outcome.
- **Collapse-release as an avalanche** — freed subordinates cascading. Distinct from §4.4's
  mandatory dissolution.
- **Revolt as a distinct event**, captives/enslavement, revenge/grievance, status/prestige,
  sacred motives, cohesion (ʿasabiyya).
- **The remaining inhibition gates** — niche-relative value, pairwise aversion, concealment-as-
  stealth (see `SOC-inhibition`).
- **Assessment *staleness*.** §4.2's asymmetry is real over time but *not* at the moment of
  conquest — the dominance test has just measured the target's strength, so the dominant does know
  its population then. The stronger model is that information is **fresh at conquest and decays**
  with epochs since last enforcement, which would give adaptive demand a physical cause rather than
  a bare feedback constant. Deferred, recorded, and the current §4.2 wording is the simplification
  it is.
- **Assessment competence varying by species.** Concealment varies (`in_group_radius`) but
  assessment does not; wiring the dominant's accuracy to `SocietyVector.sociality` would restore
  the symmetry and add free heterogeneity. Deferred.
- **A new `Fact` shape, `CauseOfEnd` variant, or stream label.** One predicate; nothing else.

## 10. Definition of Done (per CLAUDE.md)

- The §4 mechanism shipped, deterministic and bounded, in measured stages.
- The §8 criteria met, or the falsification documented and labelled.
- Census regenerated on `lefford` (authorized at G6); pins re-pinned in their drifting commits;
  keystones refrozen at merge.
- Chronicle, retrospective, book freshness sweep, Confidence Gradient re-score (the SOC bet moves
  again — in whichever direction it actually moves), registry flips (`SOC-tribute`, and
  `SOC-criticality` re-scored), full gate + artifact drift.
