# BIO-2 — Life-History Allometry — Design

**Date:** 2026-07-14
**Status:** Draft (brainstorming session) — awaiting G3 review
**Campaign:** BIO-2 (Life-History Allometry)
**Provenance:** The life-history layer of the frontier's biological substrate
(idea-registry **BIO-2**), narrowed by a live collision. Selected as the first
validation run for the **campaign-autopilot** skill because its decisions are
science-grounded and checkable ("discovered" rather than taste). During
brainstorm we discovered that the parallel **coexistence-stack** worktree (23
commits, unmerged, active) had already built the mass foundation BIO-23 called
for — the kernel `Mass` newtype, an authored `SpeciesDef.mass`, and Kleiber
home-range. Nathan therefore **narrowed this campaign** to the part
coexistence did *not* build: the per-species life-history axis (metabolic
rate, lifespan, reproductive tempo, maturity) and the clade distinction that
makes it honest. Two ideonomy tuples (2026-07-14) shaped the output design and
caught a metabolic-rate fidelity gap; both are folded in and called out at
§10.

---

## 1. Goal

Add a **neutral life-history layer**: a small authored clade tag on
`SpeciesDef` plus a pure `allometry` module that derives a species'
life-history profile — basal metabolic rate, lifespan, reproductive tempo,
age at maturity, generation length, and a fast–slow "pace-of-life" summary —
from its (coexistence-authored) body mass by published scaling laws, adjusted
by a clade coefficient. The layer **changes no existing world byte** (The
Ground precedent): mass and clade are authored *constants*, so there is **no
new stream and no new draw**, and the derivations surface only through
read-only presentation (almanac, lab metrics). It rewires no existing
consumer. Home-range/footprint is **out of scope** — coexistence owns it.

## 2. Scope and the collision boundary

The mass foundation is **ceded to coexistence-stack** and consumed here, never
re-authored:

| Concern | Owner |
|---|---|
| `hornvale_kernel::Mass` newtype | coexistence-stack (built) |
| `SpeciesDef.mass` + authored per-species values | coexistence-stack (built) |
| Kleiber home-range / footprint | coexistence-stack (built) |
| **`SpeciesDef.metabolic_class` + the `allometry` module** | **this campaign** |
| Life-history derivations, almanac/lab surfaces | **this campaign** |

The only shared edit is `SpeciesDef`: coexistence adds `mass` (+ niche); this
campaign adds `metabolic_class`. That textual coordination is resolved at
merge (see §8).

## 3. Authored input: `MetabolicClass`

One new field on `SpeciesDef`:

```rust
/// Metabolic strategy — selects the allometric normalization coefficient
/// (B₀) only; scaling exponents are universal across classes.
pub enum MetabolicClass {
    /// Warm-blooded (mammal/bird analogue): high, temperature-stable rate.
    Endotherm,
    /// Cold-blooded (reptile/amphibian analogue): ~order-of-magnitude lower
    /// basal rate; realized rate couples to ambient temperature (deferred,
    /// see §10 / CAP-1).
    Ectotherm,
    /// Phototroph (plant-folk/fungal analogue): energy from light, not
    /// forage. Defined-but-unused seam for BIO-15.
    Autotroph,
    /// No metabolism (construct/undead analogue). Defined-but-unused seam
    /// for BIO-13/BIO-22.
    Ametabolic,
}
```

Today's roster: **kobold = `Ectotherm`** (reptilian/draconic, D&D 5E SRD
lineage), **goblin / hobgoblin / bugbear = `Endotherm`**. `Autotroph` and
`Ametabolic` ship defined but unused — the forward seams for the frontier's
plant-folk and constructed peoples. No species selects them yet; their
coefficient tables may be stubbed (`unimplemented` is disallowed — they return
a documented sentinel or the trait is simply not queried for those classes;
see §5).

## 4. The allometry module — laws, and what is discovered vs calibrated

A new `domains/species/src/allometry.rs`: pure functions `f(Mass,
MetabolicClass) -> …`, no world state, no draws. Each is one attributable law.
**Honesty split (this matters for the "checkable decisions" framing):**

- The **scaling exponents** are *discovered* — published, quasi-taxon-
  invariant constants, cited inline.
- The **absolute coefficients** split: metabolic-rate B₀ is *empirically
  anchored* (measured watts); the lifespan / maturity / tempo coefficients are
  *calibrated worldbuilding constants* tuned to a reference species, because
  the absolute lifespan of a fictional humanoid is a design choice, not a
  measured fact. The spec labels each so G3 review is not misled.

| Trait | Form | Exponent (discovered) | Coefficient |
|---|---|---|---|
| Basal metabolic rate | `B₀(class)·M^p` | p = 0.75 (Kleiber 1932) | B₀ empirical: Endotherm ≈ 3.4 W·kg⁻⁰·⁷⁵; Ectotherm ≈ ⅛ of that |
| Maximum lifespan | `k_life(class)·M^p` | p = 0.25 (metabolic-time; empirical mammal 0.15–0.25) | k_life **calibrated** to a reference; Ectotherm k > Endotherm k (cold-blooded live longer per kg) |
| Age at maturity | `k_mat(class)·M^p` | p = 0.25 | k_mat **calibrated**, ~15–20% of lifespan |
| Reproductive tempo (r–K) | mass→[fast,slow], small clade term | heuristic, not a single law | **calibrated**; framed as interbirth-interval / offspring-per-lifetime, *not* litter size (CAP-2) |

**Metabolic rate is basal at a reference temperature** — a pure `f(mass,
class)`. For `Ectotherm` the *realized* rate depends on ambient temperature;
that coupling is deferred (§10, CAP-1). The function and its doc comment say
"basal, reference-temperature" explicitly so no consumer mistakes it for a
realized rate.

Calibration reference: a single ~40 kg `Endotherm` anchor fixes k_life and
k_mat (target lifespan and maturity documented in the module); every other
species scales from mass and clade off that anchor. This keeps one authored
number attributable and the rest derived — the minimal-axis discipline.

## 5. Derived outputs: the `LifeHistory` projection

A derived view struct, **computed on demand, not stored on `SpeciesDef`**
(mirrors the almanac's derived-view pattern):

```rust
pub struct LifeHistory {
    pub basal_metabolic_rate_w: f64, // watts, reference-temp basal
    pub lifespan: Duration,          // see §7 unit decision
    pub age_at_maturity: Duration,
    pub reproductive_tempo: f64,     // r–K position, 0 fast … 1 slow
    pub generation_length: Duration, // f(maturity, lifespan); MEM-7's handle
    pub pace_of_life: f64,           // 0 fast … 1 slow, the headline scalar
}
pub fn life_history(mass: Mass, class: MetabolicClass) -> LifeHistory;
```

- **Generation length** — the highest-value neutral handle (MEM-7 keys memory-
  decay constants to it). Defined `generation_length = age_at_maturity + c ·
  (lifespan − age_at_maturity)` with `c` a documented fraction (~0.3), so it
  sits between first and last reproduction.
- **Pace-of-life** — a normalized fast–slow scalar (derived from lifespan or
  log-mass, mapped to [0,1]); the frontier's "one number," now *derived*.
  Drives the almanac headline phrase.
- **`Autotroph`/`Ametabolic`**: `basal_metabolic_rate_w` is `0.0` for
  `Ametabolic` (no metabolism); `Autotroph` computes a normal basal rate (it
  still metabolizes — the difference is that its energy *source* is light, a
  distinction the trophic layer reads, not this scalar). Time-traits derive
  from mass for every class. No panic paths.

## 6. Output surfaces — read-only, strictly neutral

The liveness-spectrum analysis (ideonomy pass 2) placed the resting point at
the inert end; **no life-history trait needs a phenomenon or ledger fact in
this campaign.**

- **Almanac** — a life-history line/block per species: the pace-of-life
  headline ("fast-lived and prolific" / "slow, long-lived, sparse") plus the
  concrete lifespan / maturity / metabolic figures. Follows the existing
  per-species block pattern in `windows/almanac`.
- **Lab metrics** — one per-species metric per trait (lifespan, maturity,
  metabolic rate, reproductive tempo, generation length, pace-of-life),
  following the per-species metric pattern in `windows/lab/src/metrics.rs`.
- **No** phenomena, **no** ledger facts, **no** rewiring of demography
  natality or generation-length. The possess/vessel layer reads this when a
  reader exists (a later campaign).

**Byte-identity:** because mass and class are constants and nothing is
rewired, all committed artifacts and all 1000 census worlds remain
byte-identical. New lab metrics extend the metric registry (a metric-count
pin updates in this commit — re-baseline in place, per the rebaseline-golden-
pins rule); they add rows, they do not alter existing world bytes.

## 7. Determinism and units

- **Zero new draws.** Authored constants only; no stream, no `Seed`/`Stream`
  use. Determinism contract untouched.
- **Duration typing — OPEN, leads the G3 flagged section.** Lifespan / maturity
  / generation length are durations. `StdDays` lives in `domains/astronomy`,
  and a domain crate may not depend on another domain (constitutional), so
  species cannot reuse it. Two candidates:
  - **(a) Promote a duration newtype to the kernel** (e.g.
    `hornvale_kernel::Years` or a general `Duration`) — consistent with `Mass`
    living kernel-side, and the natural shared home. Risk: conceptual overlap
    with astronomy's `StdDays` (which also models local/standard day-length
    conversion). Mitigation: a *coarse* calendar-scale `Years` is a different
    quantity from `StdDays`' sub-day astronomy; they can coexist, or `StdDays`
    later re-homes to kernel as its own decision. **Recommended.**
  - **(b) Bare `f64` years** with a `type-audit` waiver documenting why no
    newtype. Lower ceremony, weaker guarantee.
- **Metabolic rate** is watts. Lower-priority unit call: ship as bare `f64`
  watts with a `type-audit` verdict this campaign; a `Power`/`Watts` newtype is
  a candidate for the kernel-units thread but not required here.

## 8. Dependency and sequencing

This campaign **consumes** coexistence-stack's `Mass` type and
`SpeciesDef.mass`. Consequences:

- **Brainstorm → spec → plan (G1–G4) proceed now** against the mass foundation
  as a documented precondition. The `allometry` module is pure over `Mass` +
  `MetabolicClass`; the design does not need the authored values.
- **Code execution (G5) sequences after coexistence-stack merges to main.**
  Then this worktree absorbs main (`make preflight`; merge main into the
  branch on an ancestry NO-GO), gaining `Mass` + `SpeciesDef.mass`, and adds
  `metabolic_class` + `allometry` on top. Alternatively, if parallelism is
  wanted, the branch stacks on coexistence-stack — a post-G3 call, not decided
  here.
- The shared `SpeciesDef` edit is a small textual merge (`mass` vs
  `metabolic_class` are disjoint fields).

## 9. Testing

- **Unit (allometry module):** each law's exponent and monotonicity —
  metabolic rate strictly increases with mass; larger mass → longer lifespan
  and later maturity within a class; `Ectotherm` lifespan > `Endotherm`
  lifespan at equal mass; reproductive tempo slows with mass. Reference-anchor
  values hit their documented targets.
- **Cross-species sanity:** on today's roster, the derived ordering is
  coherent (kobold long-lived-for-its-size as an ectotherm; bugbear the
  longest-lived by mass).
- **Neutrality (byte-identity):** existing artifact drift-check stays green;
  a test asserts no new stream label appears in the manifest.
- **Presentation:** almanac block and each lab metric extract for seed 42
  (golden), following existing per-species metric tests.
- **Property:** `Autotroph`/`Ametabolic` never panic; metabolic rate is `0.0`
  for `Ametabolic`.

## 10. Ideonomy-surfaced items and the fidelity call

- **CAP-1 (deferred, Nathan-approved).** Realized (climate-coupled) metabolic
  rate for ectotherms: `realized = basal · thermal_response(ambient, class)`.
  Deferred out of this campaign to preserve neutrality and the domain boundary
  (species cannot depend on climate). To be incorporated later *indirectly*,
  via a function at the `windows/worldgen` composition root where species meets
  climate. Captured for the followup register; evaluate a frontier row (thermal
  ecology / behavioral thermoregulation / Q₁₀ performance curves) when the
  consumer is spec'd.
- **CAP-2 (folded in).** Reproductive trait framed as r–K tempo (interbirth
  interval / offspring-per-lifetime), not literal litter size — mammalian
  litter allometry is dubious for humanoids.
- **Generation length + pace-of-life** — added as derived outputs (ideonomy
  pass 2, rate/age dimensions).

## 11. Decisions (promoted from the campaign decision ledger)

1. **Clade as enum, exponents universal.** Author a `MetabolicClass` enum
   selecting the coefficient B₀; scaling exponents are shared constants
   (quasi-taxon-invariant). Never per-species free coefficients.
2. **Narrow to life-history; cede the mass foundation to coexistence-stack.**
3. **Neutral substrate.** Read-only surfaces only; no phenomena, no ledger
   facts, no consumer rewiring; byte-identical worlds (The Ground precedent).
4. **Basal, reference-temperature metabolic rate (option a);** ectotherm
   climate coupling deferred (CAP-1).
5. **Discovered vs calibrated labelled explicitly** — exponents cited;
   lifespan/maturity/tempo absolute coefficients are calibrated worldbuilding.

## 12. Open questions for G3

1. **Duration unit typing** (§7) — kernel newtype (recommended) vs bare-f64
   waiver. Leads the flagged section (schema/units-adjacent).
2. **Shared `SpeciesDef` schema edit** coordinated with coexistence-stack
   (save-format-adjacent).
3. **Sequencing** — execution-after-merge (recommended) vs stack-on-branch
   (post-G3 mechanics).
4. **Calibration reference** — confirm the ~40 kg endotherm anchor and its
   target lifespan/maturity, or supply preferred anchor values.

## 13. Out of scope

Home-range/footprint (coexistence); dimorphism & mating system (BIO-3);
gene–culture coevolution (BIO-4); fauna on the axis (BIO-7); sanitation /
carrying-capacity terms (BIO-5); realized climate-coupled metabolism (CAP-1);
any rewiring of existing consumers.
