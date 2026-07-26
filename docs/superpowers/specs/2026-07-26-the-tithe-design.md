# The Tithe — Living-Community C3, Slice 2: Tribute

**Status:** design (G3 — awaiting Nathan)
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
| **remittance** | `min(assessment, surplus × (1 − concealment))` where **surplus is that epoch's growth increment** — never the standing stock | what the subordinate *hands over*, paid from what it **has** |
| **concealment** | `(1 − in_group_radius) × CONCEAL_MAX` | the gap the subordinate controls — an insular people hides more from outsiders |
| **shortfall** | `assessment − remittance` | the only signal the dominant reads |

`eff_capacity` and `population` are already two different numbers, so **the information asymmetry
costs nothing and is fully deterministic**. Land tax has always been assessed on area, never on
the granary, for exactly this reason.

`in_group_radius` (insular 0 ↔ expansive 1) lives on `SocietyVector`; wiring concealment and
secrecy to it is the reading `SOC-information-economy` already argues for.

**Both errors destabilise, in opposite directions.** Under-assess and the subordinate accumulates
until it can throw off its patron; over-assess and it is crushed. Unlike a fixed rate, this system
has **no equilibrium** — which is the point, and the direct answer to why the crowding build was
smooth and raid-free.

### 4.3 Adaptive demand — the oscillator

Each epoch after collection the dominant adjusts:
`assessment += shortfall × ADAPT_RATE`.

Feedback **with delay** (the delay is the epoch step, free) is an oscillator: over-extract →
subordinate collapses → tribute falls → demand relaxes → regrowth → over-extract. This reproduces
the **Ibn Khaldun / Turchin secular cycle** the spec has cited since the pivot, as a *consequence*
rather than an authored feature — and oscillators parked near a threshold are how SOC systems
actually sit at criticality, which is precisely what neither prior build achieved.

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

### 4.5 Determinism (Lorenz-safe)

Assessment, remittance, concealment and adaptation are total, deterministic functions of frozen
epoch state and authored species data. **No new seed draw.** No agent decision — the "guess" is a
reading of a visible proxy, not a choice. `BTreeMap`/`BTreeSet`/`Vec` only; every float comparison
via `f64::total_cmp` with a deterministic tie-break. No wall-clock. The adaptation is a bounded
first-order update on a per-relation scalar, not a chaotic forward-integration; assessment is
clamped to `[0, eff_capacity × ASSESS_MAX]` so no relation can diverge and no dominant can demand
more than its subordinate's land could ever produce.

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
   changing cell — the thing predation could not do.
3. **The map is not depopulated** and no community is farmed to extinction by tribute alone;
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
- **A new `Fact` shape, `CauseOfEnd` variant, or stream label.** One predicate; nothing else.

## 10. Definition of Done (per CLAUDE.md)

- The §4 mechanism shipped, deterministic and bounded, in measured stages.
- The §8 criteria met, or the falsification documented and labelled.
- Census regenerated on `lefford` (authorized at G6); pins re-pinned in their drifting commits;
  keystones refrozen at merge.
- Chronicle, retrospective, book freshness sweep, Confidence Gradient re-score (the SOC bet moves
  again — in whichever direction it actually moves), registry flips (`SOC-tribute`, and
  `SOC-criticality` re-scored), full gate + artifact drift.
