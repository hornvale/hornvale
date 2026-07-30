# The Contour — position as the second contest axis

**Campaign:** The Contour · **Date:** 2026-07-29 · **Status:** spec, pre-plan
**Governs:** [decision 0089](../../decisions/0089-diversity-is-terminal-and-rubberbanding-is-multi-axis.md)
clause 1 · **Successor to:** *The Tumult*, *The Tithe*

---

## 1. The charge

Decision 0089 records that peoples-diversity is a terminal value and that the
only sanctioned mechanism for it is **multi-axis contest**. It also records
that the history bake is **out of compliance**: every contest in Hornvale's
deep history resolves on one scalar.

Verified in source, not inferred:

```
windows/worldgen/src/history_bake.rs:1648
  strength = (population + stores * STORE_WEIGHT) * tech_weight(tech)

windows/worldgen/src/history_bake.rs:2544  (inside maybe_raid's candidate walk)
  if raider_str <= t_str * RAID_MARGIN { continue; }   // RAID_MARGIN = 1.5
```

One axis, one comparison. On one axis there is only absolute advantage, so a
weak people has nothing to trade, nowhere to be better, and no reason to
persist. This campaign adds the second axis in the cheapest form that tests
the thesis, and measures whether it does what three literatures say it must.

**It is deliberately the cheapest of the three campaigns in the sequence**
(The Contour → The Appraisal → The Deviation), because it is the only one that
tests the keystone claim *without* touching authored species data. If a second
axis moves nothing, the other two become much less attractive, and we will
have learned that for one campaign's cost instead of three.

## 2. The mechanism: defensibility

### 2.1 What it is

A cell's **defensibility** is how hard it is to come at — derived, per era,
from the cell's own approach structure in that era's connection graph. It is a
multiplier on the *holder's* side of the dominance test.

Everything it needs already exists. `domains/topology`'s `Edge` carries a
dimensionless `conductance` ("ease-of-travel: higher is easier") and an
`EdgeKind` of `Adjacency | WaterRoute | LandRoute`, and the bake already holds
a per-era `ConnectionGraph` and already walks `traversable_neighbors` inside
the raid scan. **No new seeded draw, no new authored data, no new crate, no
new field on any committed struct.**

### 2.2 Why this axis and not another

Three properties make position the right first axis, and the third is the one
that makes it more than a modifier:

1. **It is uncorrelated with strength by construction.** Defensibility is a
   fact about terrain; strength is population times tech. A second axis that
   correlates with the first is not a second axis.
2. **It is indifferent to who holds the cell**, which is what 0089 clause 1
   requires. It is not a term keyed on weakness; it is a term keyed on
   ground, and that the weak benefit is a byproduct.
3. **It is a cost-of-dominance term**, which is 0089 clause 2's sanctioned
   direction. If defensible ground is also *poor* ground, then the strong
   expand onto rich exposed cells and become takeable, while marginal
   defensible ground shelters whoever happens to be on it. That is the
   frontier diamond — one derived field that is simultaneously the refuge for
   the weak and the cost sink for the strong.

**Property 3 is a hypothesis about this world's geography, not a fact, and
this spec does not assume it.** Whether defensibility anti-correlates with
`eff_capacity` in Hornvale's actual terrain is measured in §4 as M4. If it
comes back uncorrelated or positively correlated, the mechanism still adds a
second axis but the frontier reading is withdrawn, and that must be reported
rather than explained away.

### 2.3 The form

> **Amendment 1 (2026-07-30), taken pre-readout.** This section originally
> specified defensibility as a saturating function of a cell's *aggregate*
> approach ease. The calibration measured that aggregate before any behaviour
> was written — which is the only reason this was caught — and found it is not
> one distribution at all. See §2.3a for what was measured and why the form
> changed. No behavioural readout existed at the time of this amendment, so
> nothing was being chased.

Defensibility is a property of **the approach**, not of the cell. A raid
arrives along one route, and what shelters the defender is the resistance of
*that* route.

```
best_edge(from, to, graph)
    = the MAXIMUM-conductance edge from `from` to `to`
      (parallel edges exist; an attacker takes the easiest one)

cost_exponent(edge) = -ln(edge.conductance)
    # conductance ∈ (0, 1], so this is 0 for a free route and grows without
    # bound as the route gets dearer. It is log(traversal cost).

defensibility(from, to, graph)
    = DEF_MIN + (DEF_MAX - DEF_MIN) * tanh(cost_exponent(best_edge) / DEF_SCALE)
```

`tanh` and `ln` are available libm-backed in `hornvale_kernel::math` (decision
0041). Four properties are load-bearing:

- **It is bounded in `[DEF_MIN, DEF_MAX)`.** `DEF_MIN` *is* attained, by a
  free route (`conductance == 1.0`, which the calibration measured as real).
  That is correct and intended: a cell reached by an unobstructed sea lane is
  the most exposed ground there is, and the model should say so rather than
  reserve an unreachable ideal.
- **It is strictly monotone** in cost, so the ordering of approaches is total
  and deterministic with no plateau to tie-break inside.
- **It is a pure function of `(from, to, graph)`** — no time, no seed, no bake
  state. Recomputable, cacheable per era, trivially testable.
- **Parallel edges resolve by maximum conductance**, which is both the right
  principle (an attacker uses the best road available) and the fix for a
  measured defect: 6.7% of cells carry duplicate `to` values — an `Adjacency`
  and a `LandRoute` to the same neighbour — which an aggregate double-counts
  and a max does not.

`DEF_MIN` and `DEF_MAX` are authored priors. `DEF_SCALE` is **calibrated from
the measured distribution of `cost_exponent` over traversable edges, before
any behavioural constant is written**, and frozen thereafter (§4.4).

**A correction to this spec's earlier reasoning.** The original §2.3 required
defensibility to be a strict asymptote at both ends, citing decision 0089
clause 3. That over-applied the clause. Clause 3 governs the **distribution of
world outcomes** — a world with no goblins, a world under one government must
be reachable and rare — and says nothing about whether an intermediate derived
field may attain its own bounds. A defensibility floor reached by a free sea
lane forecloses no world outcome. The property test asserts monotonicity and
inclusive bounds, not strict interiority.

### 2.3a What the calibration measured, and why the form changed

Three runs, 30 seeds, 142,595 habitable cells, recorded in the plan's Task 2:

```
                 q0.05     q0.50     q0.95     max       present
adjacency_max    0.000977  0.002099  0.008511  0.046512  99.7% of cells
water_route_max  0.000000  0.000000  0.998020  1.000000  12.8% of cells
land_route_max   0.000000  0.000000  0.000000  0.020000   1.9% of cells

high (>=0.5, n=14702) : 100.00% WaterRoute,  0.00% Adjacency
low  (<=0.01, n=119797):  98.81% Adjacency,  0.03% WaterRoute
```

`adjacency_max`'s **global** maximum is 0.046512 — an order of magnitude below
the high population's threshold. The two regimes are **disjoint, with an empty
gap between 0.047 and 0.5** across every measured cell. Approach ease is
therefore not one skewed distribution but two: ~87% of cells reachable only
overland at conductance ~0.001–0.008, and ~13% water-connected at up to 1.0.

Any single saturating transform over the aggregate must flatten one regime or
the other. Reading the approach's own conductance instead removes the problem
rather than finessing it, and it is what the military geography says: a coastal
cell is defensible against land and wide open to sea, and those two facts are
not averageable. This is the resolution of §8's open question 1, which the
measurement promoted from an optional refinement to the main question.

### 2.4 Where it enters

Exactly two call sites change, and both already hold the edge they need.

```rust
// history_bake.rs, maybe_raid's candidate walk (~:2544)
if raider_str <= t_str * self.defensibility(raider_site, n) * RAID_MARGIN {
    continue;                       // dominance, now approach-aware
}
```

The same term enters `Bake::best_home`'s held-land test
(`history_bake.rs:1127`) so that a homeless roller faces the same geography a
seated raider does — there the `from` cell is the ring-walk's origin.
**Those two are the only sites.** Defensibility deliberately does *not* enter
`strength`, `pressure_of`, `eff_capacity`, or tribute assessment — the same
discipline *The Tithe* applied when it kept `stores` out of `pressure_of`.

**What this costs.** Defensibility is no longer a per-cell field, so it is not
directly renderable as a map layer. A per-cell *view* — the minimum
defensibility over a cell's approaches, i.e. its weakest point, which is the
quantity Ammann's envelope model cares about — remains derivable for the
almanac and for M4, but it is a view over the mechanism, not the mechanism.

## 3. Contour as a derived classification

The second deliverable, separable from §2 and buildable after it.

A people's **contour** is the pattern of peaks in the vectors it already
carries — `MindVector`, `SocietyVector`, `PerceptionVector`, all authored, all
closed. Following decisions [0060](../../decisions/0060-the-is-a-classification-predicate.md)
and [0062](../../decisions/0062-the-classification-split.md), contour is
**derived as an `is-a` classification over the existing vectors, never a new
authored field**. It adds no data to any species; it names a shape that is
already there.

This exists for legibility, which pass four of the brainstorm identified as
the thing that makes any of the rest land on a reader: a measurement that says
"effective diversity rose from 2.1 to 2.9" is a number, and "the kobolds are
skirmishers who hold the passes while the hobgoblins took the plains and could
not keep them" is a world. It is also the seam every later campaign in the
sequence reads.

**Cost note:** new registered concepts mean the `concepts` dump and any Lab
census dump touching them must be regenerated and diffed, not just `make
gate`. This is a known recurring miss and is called out in the plan.

## 4. Measurement — preregistered

**This section is frozen before any implementation code exists**, per decision
0016 and `preregistration_guard`. *The Tithe* amended its own spec five times,
four of them after a disappointing measurement, and disclosed that the
cumulative shape was metric-chasing. This spec's protection is that its
predictions are written here, first, with both branches made informative.

### 4.1 The instrument

Four metrics. **M2 is the headline**; M1 is inherited and explicitly demoted.

- **M1 — cascade-size histogram.** The instrument *The Tumult* and *The Tithe*
  reported, retained for continuity. **Not the headline and not the
  hypothesis.** Reported so the two prior nulls remain comparable.
- **M2 — the entity-size distribution.** The rank-size distribution of
  community populations and of holding sizes at bake end, pooled over seeds,
  read against Zipf. **Hornvale has never measured this.** This is the
  variable the sigmoid wager is about.
- **M3 — peoples-diversity at bake end.** Count of peoples with a live
  community, and the effective diversity (the same reading `coexist.rs` uses
  in space, computed here in time). This is the 0089 compliance metric.
- **M4 — the defensibility/value correlation.** Rank correlation between
  `defensibility` and `eff_capacity` over habitable cells. A check on §2.2's
  frontier hypothesis, not on the campaign's.

Sample: seeds 1..=30 for the primary readout and 1..=100 for replication,
matching the two prior campaigns exactly so the numbers are comparable.

### 4.2 Predictions

1. **M3 rises.** A second contest axis increases the number of peoples
   surviving to bake end, and the effective diversity, against the shipped
   baseline. **If M3 does not move, the multi-axis thesis is wrong as built**
   and the campaign ships that as its headline — a second axis was added, it
   was uncorrelated with the first, and diversity did not respond.
2. **M2 is the open question and both branches are informative.** A move
   toward a heavier entity-size tail supports the sigmoid wager. **M2 staying
   geometric while M3 rises is the more interesting result**, not the
   disappointing one: it would show that a second axis is sufficient for
   coexistence but not for multiplicative heterogeneity, which localises the
   remaining term in *The Deviation* (per-community deviation from the
   people's prototype) rather than leaving it unlocated.
3. **M1 is expected to be roughly unchanged.** This campaign adds no
   conduction medium, so a large move in M1 would indicate the mechanism is
   doing something other than what §2 describes, and would need explaining
   before either M2 or M3 is trusted.

### 4.3 What the null proves

If M3 does not move and M2 does not move, the conclusion on the record is:
**a second contest axis, uncorrelated with the first and entering at the
decision point, is not sufficient to hold diversity open in this world.** That
is a real finding about 0089 clause 1's chosen mechanism, and it would send the
sequence back to design rather than forward to The Appraisal. It is not a
reason to add a third mechanism inside this campaign.

### 4.4 The constants discipline

`DEF_MIN`, `DEF_MAX` and `DEF_SCALE` are set once, from the pre-measurement of
the `cost_exponent` distribution described in §2.3, **before any of M1–M4 is
computed**, and are not touched afterwards. If a readout is disappointing, the
constants do not move; the finding is reported. Any deviation from this is an
amendment and is disclosed in the chronicle with its count, as *The Tithe*'s
was.

**Standing amendment count: 1** (§2.3's form change), taken **pre-readout** —
no behavioural measurement existed at the time, so nothing was being chased.
The distinction that matters to a reader is not how many amendments a campaign
made but whether any of them followed a number somebody disliked; this one
followed a number nobody had yet asked for.

**One decision rule is fixed here in advance**, because the calibration has
already shown it may be needed. `cost_exponent` over land approaches spans
roughly 3.1 to 6.9 (conductance 0.047 down to 0.001) while free water routes
sit near 0. If, at a `DEF_SCALE` chosen to put the median approach at
defensibility 1.0, the land population's defensibility range turns out to span
**less than 0.10**, then a single scale cannot grade the 87% of the world that
is land-only, and the fallback is to normalize `cost_exponent` **within the
approach's `EdgeKind`** before the `tanh`. That fallback is specified now,
with its trigger, so that taking it later is executing this spec rather than
amending it. Which branch obtains is a measurement, not a preference.

## 5. Determinism and save format

- **No new seeded draws.** Defensibility is a pure function of the connection
  graph, which is itself derived. Stream consumption order is therefore
  unchanged, and the pin-isolation tests should pass untouched. This is an
  assertion to be *verified by test*, not assumed.
- **The raid derivation moves**, so committed history changes: occupation
  records, conquests, tribute relations, and everything downstream that reads
  them (names, the almanacs, the census).
- **The epoch question is measurement-adjudicated, not asserted.** Decision
  [0084](../../decisions/0084-an-epoch-is-declared-only-when-a-derivation-moved.md)
  is explicit that an epoch is declared only when a derivation moved, and that
  the way to establish this is to run the regeneration and read the diff — a
  campaign that *led* with "byte-identity breaks" measured it and found it had
  not. The plan carries a task that regenerates and diffs before any label is
  touched. The expectation is that `history/bake` does move; the expectation
  is not the evidence.
- **Census goldens are expected to move, and that expectation is not
  evidence.** The same discipline 0084 applies to the epoch applies here: the
  plan regenerates and reads the diff rather than asserting it. If a refresh is
  needed it runs on the canonical host (decision 0081, `scripts/census-run.sh`,
  host `lefford` per 0079). **This is an autopilot carve-out and needs Nathan's
  explicit authorization at the point of running, not at spec approval.**

## 6. Testing

- **Unit** — `defensibility` is a pure function: strict monotonicity in cost,
  inclusive bounds in `[DEF_MIN, DEF_MAX)` with `DEF_MIN` attained at
  `conductance == 1.0`, and determinism across recomputation.
- **Parallel-edge resolution** — a fixture where two edges join the same pair
  with different conductances, asserting the *maximum* is used. This is the
  test that pins the duplicate-edge fix, and it must be mutation-verified
  against a `min` and against a `sum` implementation, since 6.7% of real cells
  carry such duplicates and an aggregate would silently double-count them.
- **Property** — approach ordering is stable under graph rebuild; the same
  era's graph yields the same values.
- **Behavioural** — a fixture where two identical communities differ only in
  the conductance of the route reaching them, and only the cheaply-reached one
  is raided. This is the test that would fail if the term were wired to the
  attacker's side by mistake, or read the wrong end of the edge.
- **Mutation check** — per the standing lesson that tests asserting nothing
  ship green: each behavioural test is verified to fail when `defensibility`
  is stubbed to return a constant.
- **Regression** — the existing pin-isolation batteries in
  `domains/terrain/tests/tectonic_properties.rs` and
  `domains/astronomy/tests/genesis_properties.rs` must be untouched, which is
  the check that no draw moved.

## 7. Non-goals

Explicitly out of scope. Each is a named successor, and none of it may be
smuggled in because it is adjacent:

- **d′ / discriminability, assessment error, MindVector widening** — *The
  Appraisal*. This campaign changes no species data and triggers no species
  epoch.
- **Prototype-inheritance generalization**, per-community deviation — *The
  Deviation*.
- **Depth, collapse-release, conduction** — deferred by the `open-questions.md`
  rescore, not refuted.
- **Trade, gift, coalitions, third parties, institutions-as-cascade** — the
  standing bench.
- **A forward-running or animated reader surface** — the legibility finding is
  served here only by §3's classification.
- **Any change to `strength`, `pressure_of`, `eff_capacity`, or tribute.**

## 8. Open questions carried into planning

1. ~~Whether `approach_ease` should weight `EdgeKind`.~~ **RESOLVED by
   measurement (2026-07-30), and it was not the optional refinement this list
   took it for — it was the main question.** The aggregate is two disjoint
   regimes split exactly on `WaterRoute` versus `Adjacency` (§2.3a), so the
   mechanism now reads the approach's own conductance and the weighting
   question dissolves. Recorded here rather than deleted, because a question
   this list mis-ranked as minor is worth leaving visible.
2. Whether defensibility should be per-era or computed once. It is written
   per-era above because the graph is per-era; the cost is unmeasured.
   Note that the amendment makes this cheaper, not dearer: a per-approach
   lookup reads one edge and needs no per-cell field built or cached at all.
3. Whether §3's contour classification lands in `domains/species` or in a
   window. It is a derived view, which argues for a window, but it is read by
   the bake's own narration, which argues for the domain.
