# The Ceiling — what the rock can support

**Campaign:** The Ceiling. Rung 3 of the Underworld Larder metaplan
(`docs/superpowers/specs/2026-08-24-the-underworld-larder-metaplan.md`).

**Classification:** Architectural, measurement-gated.

**Base:** `26003913d`. **Decision in force:** 0966 (symmetric budget,
asymmetric allocation). **Predecessors:** The Gossan (rung 1), The Sources
(rung 2), The Winze (amendment C.3).

## 1. The one-sentence claim

Measure what the rock can support, then **give it a mouth**: a people that
lives on chemical energy underground, where today nothing settled does.

**The campaign was re-scoped at G3 (ledger #5) and this section replaces its
original claim.** It opened as rung 3 alone — derive the ceiling. Three
campaigns then turned out to be converging on rung 4's *underworld consumer*,
which nobody owned, while nothing needed the ceiling at all. Stage 1's
measurement is unchanged; Stage 2 now authors the consumer instead of the
ceiling, and the ceiling's construction moves to a successor **with the
measurement in hand**. Decision 0966 stands and governs whoever builds it.

## 2. What already exists — measured, not assumed

Every claim here is a command's output taken at `26003913d`, or a citation to
a committed doc comment. Nothing in this section is inferred.

**The budget quantity is a mean of seven.** `windows/worldgen/src/energy.rs:449`:

```rust
let total: f64 = EnergySource::ALL.iter().map(|s| s.yield_at(..)).sum();
total / EnergySource::ALL.len() as f64
```

Its doc records the mean was chosen empirically, not designed: a clamped sum
"pinned **every rung's median to exactly `1.0`**". `EnergySource::ALL` is
**seven** (`energy.rs:342`), documented as "the row's six plus
`DetritalImport`" — so the metaplan's "six energy sources" and the code's seven
are consistent, not a drift. `DetritalImport` reads `drainage`, not lithology:
**one seventh of the budget is surface productivity percolating down**, which
C.3's "what varies per world already is the rock" does not account for.

**The allocation primitive already exists.** `dominant_source`
(`energy.rs:476`) is retained beside the sum, in its own words, so the sources'
"*differences* … survive the sum rather than being discarded by it."

**Composition is well spread.** Re-run at this campaign's base:

```
$ cargo nextest run -p hornvale-worldgen --test suite --run-ignored all \
    --no-capture -E 'test(more_than_one_source_dominates_somewhere)'
dominant-source histogram over 19105 chambers: [720, 7338, 3025, 5013, 2639, 57, 313]
EnergySource::ALL order: [Serpentinization, IronReduction, Radiolysis,
                          SulphideOxidation, Methanogenesis, Geothermal, DetritalImport]
```

All seven occupied; the largest takes 38.4%; `Geothermal` is nearly vestigial
at 0.3%. **But this histogram is POOLED over seeds 42/7/1234** — the one
operation that destroys the between-world signal this campaign needs. The
existing test asserts only `occupied > 1`.

**Magnitude does not separate worlds.** The Sources' Q1, frozen in its spec and
falsified (n=12, recorded in
`windows/worldgen/tests/suite/subterranean_energy_probe.rs`):

```text
separation = IQR({m_s}) / median({IQR(E_s)}) = 0.145249      PREDICTED >= 0.25
```

Twelve per-seed medians sit inside an 0.018-wide band while one world's own
chamber-to-chamber IQR is ~6.9x that. Q2/S1 also falsified: no rung's p10-p90
width clears one `ENERGY` band.

**And the field occupies the bottom third of its own ruler.** S2, same run:

| rung | inert | lean | fed | rich | teeming |
|---|---|---|---|---|---|
| `Undercroft` | 19.08% | 80.92% | 0.00% | 0.00% | 0.00% |
| `Nadir` | 1.96% | 92.36% | 5.68% | 0.00% | 0.00% |

`rich` and `teeming` are **never realized at any rung in any of twelve
worlds**; the largest reading anywhere is `0.424277`. The probe recorded this
and deliberately corrected nothing.

**The two probes are off-gate.** Both carry `#[ignore]` with non-`heavy:`
reasons ("demoted by The Governor 2026-08-28"), so neither runs in any gate and
both are run by hand.

**Rung 3 has never run.** No spec, no branch, no chronicle before this one.

### 2a. The budget has no settled consumer — verified at this base

Reported by The Staple D5B (chokepoint 1) and **re-verified here rather than
taken on report**, because a peer's diagnosis is a hypothesis until re-run:

- `xorn` is the **only** kind in the registry carrying a `CHEMOSYNTHATE`
  weight — one `ResourceVector`, `domains/species/src/lib.rs:3804`.
- `xorn` is `SocialForm::Solitary` (`:3807`), not `Settled`.
- Settlement placement filters on `SocialForm::Settled`
  (`windows/worldgen/src/lib.rs:5552`, `:5902`, `:8088`), and the codebase
  states the consequence itself at `domains/species/src/lib.rs:3300` about
  another non-`Settled` kind: it "moves capacity and occupancy but places no
  settlement."

**So chemical energy underground cannot sustain a settled community today**,
and a ceiling derived over it would be a quantity history cannot observe.

**But the machinery to consume it is already wired, and that is the finding
that makes Stage 2 cheap.** `per_species_capacity_at`'s `Subterranean` arm
(`windows/worldgen/src/lib.rs:2425`) reads `chemosynthate_per_rung` at each
rung of `Band::habitation()` and passes it into `score_at`, whose `per_axis`
array carries `(CHEMOSYNTHATE, chemosynthate)` and is dotted against the
kind's own `niche_weights` (`:2390`). A `Settled`, `Subterranean` kind
weighting `CHEMOSYNTHATE` therefore draws real carrying capacity from rock
chemistry **with no rung-3 ceiling in the path**.

**This retires a standing registry claim.**
`BIO-underworld-has-no-energy` says "Capacity is computed from INSOLATION and
never reads `EnvironmentNiche` … a people seated 800 m down is fed by
sunlight." That is **stale for the `Subterranean` arm**: The Sources wired
both the supply and the tolerance. The row's size clause is unaffected.

## 3. Design

### 3.1 Two quantities, one field, read twice

Decision 0966 needs two coordinates, and the seam is that **neither collapses
the seven-vector prematurely**:

- **Budget** — how much the rock supports. Magnitude.
- **Allocation** — which consumer class it favours. Composition.

`subterranean_energy` is the monolith (mean of seven); `dominant_source` is the
first crack in it. This campaign replaces neither. It asks whether the
composition half carries enough signal to be the allocation axis, then — per
the re-scope in §1 — **builds the consumer that makes the budget observable at
all**, and hands the measurement to whoever builds the ceiling.

**Which is the order the evidence supports, not a retreat from rung 3.** A
ceiling is a bound on consumption, and §2a establishes there is no settled
consumption underground to bound. Deriving the bound first would have produced
a quantity nothing eats and history cannot see.

### 3.2 Stage 1 — the measurement, and nothing else

Stage 1 writes **no production code**. It is the discipline
`winze_energy_probe.rs` states in its own header: "A measurement dispatched
before any design. Changes no production code. Three mechanisms in this line of
work already died from a design written before its substrate was measured."

Two questions, §4 freezes both.

1. **Does composition separate worlds?** The Sources measured the
   between-worlds statistic on the **magnitude** scalar only. No between-worlds
   statistic exists for composition at all. This is the load-bearing claim
   under 0966 and it is unmeasured.
2. **Is the magnitude compression caused by the rock or by the mean?** The
   metaplan attributes it to the rock ("roughly three near-constant categorical
   states") and instructs rung 3 to design against that. A mean of seven gated
   terms compresses by construction. **Nothing has separated the two causes**,
   and the instruction rung 3 inherits depends on which it is.

Both are answerable over the **frozen twelve-seed set at
`BuildDepth::Terrain`**, reusing the existing probe's own world construction —
no new world generation, no new stream, no draw.

### 3.3 The branch table — decision rules, not predictions

Stage 1's result no longer decides whether a ceiling gets built here; it
decides **what kind of consumer Stage 2 authors** and **what the successor
inherits**. Enumerated as branches so the implementer meets a decision rule
rather than a prediction to rescue.

| M1 (composition separates?) | M2 (compression cause) | Stage 2 authors | the successor inherits |
|---|---|---|---|
| **≥ 3 at some rung** | either | a consumer whose niche favours a **named dominant source**, so where it can live is a fact about that world's chemistry | a rich allocation axis; 0966 stands as written |
| **exactly 2** | either | the same, on a **binary** axis — the kind tracks one of two sources | a coarse but real axis; 0966 stands, narrowed |
| **exactly 1** | mean is a major cause | a consumer on the **aggregate** supply, plus the finding that the shipped scalar hides an axis the successor must fix before building the ceiling | a combination rule to repair first |
| **exactly 1** | rock is the cause | a consumer on the **aggregate** supply | **the null as headline**: composition cannot carry allocation, and 0966 is superseded by a record choosing between C.3's original two |

**Row 2 is the one to expect.** No prior measurement supports richness, and
§4's own depth evidence shows the strongest composition signal running along
an axis (depth) that every world shares. Naming the expected row is not a
prediction to rescue — §4's prediction is `M1 >= 2` and nothing below asserts
on which row lands — it is so that a result landing on row 1 or row 4 is
recognised as surprising and gets the scrutiny a surprise deserves.

**Stage 2 ships in every row.** The bottom row is the outcome this project's
own precedent treats as publishable (0016; The Sources shipped two
falsifications as its most valuable output), and even there the consumer gets
built — it simply eats the aggregate rather than a named source.

### 3.4 The calibration finding is a deliverable in every branch

S2's result is independent of M1 and M2: **a budget whose field never exceeds
`0.424277` cannot say "teeming."** `BIO-subterranean-energy-sources`' "an
Underworld as lush as the Overworld needs its own productive base" is
arithmetically unreachable while that holds, whatever the allocation axis
does.

Stage 1 therefore reports the realized range against the ruler the consumer
reads, and **Stage 2 must state what band its kind actually sits in** rather
than assuming a habitable one. Whether to *correct* the calibration is not
decided here (§6): this campaign establishes the gap with a number and names
who must close it.

### 3.5 What Stage 2 builds

**A settled people that lives on chemical energy underground.** Concretely,
and each item is a change the compiler or an existing registry will force
rather than a new mechanism:

- a `BiosphereTraits` row with `TrophicMode::Chemotrophic` and a niche
  weighting `CHEMOSYNTHATE`, alongside whatever else its authoring argues for;
- `SocialForm::Settled`, which is the single property `xorn` lacks and the
  whole reason the supply reaches nothing today (§2a);
- a `HabitatRealm::Subterranean` entry in the sparse `habitat_realm_registry`;
- whatever `metabolic_pairs.rs`' sanctioned-pair table and
  `environment_niche_registry` demand of any new kind.

**It is one kind, not a roster.** The Delvers authored two subterranean
peoples and withdrew both, and `habitat_realm_registry` records why: "the trap
is not authoring a subterranean kind, it is distinguishing two kinds by DEPTH,
which nothing in the model can say." One kind cannot fall into that trap. A
second would have to clear The Tidemark's own rule — no two kinds may differ
only by stratum — and this campaign does not take that on.

**The measured consequence is the deliverable, not the row.** Authoring the
kind is cheap; what this campaign owes is the readout, and §4's M4 makes it an
**ablation** rather than a description. An earlier draft of this section asked
only "does it place, where, how many vertices, and at which rungs" — which
cannot distinguish a working `CHEMOSYNTHATE` weight from a decorative one, and
that is the campaign's own failure mode. `xorn`'s niche is 0.65 `MINERAL` /
0.35 `CHEMOSYNTHATE`; a settled kind on that shape could place entirely on the
mineral half while the chemotrophic weight does nothing, and a readout that
only reported placement would call that success.

**"At which rungs" is two questions, not one, and they have measurably
different answers.** The Staple D5B's probe found underground endpoints where
the **capacity-winning rung** differs from the **seated rung** — its
chokepoint 2 — so "the rock supports X" and "the people occupy X" are distinct
claims today. `per_species_capacity_at`'s `Subterranean` arm keeps only the
best rung (`windows/worldgen/src/lib.rs:2425`), and seating is
`delve_seating`'s own choice. Stage 2's readout **names which of the two it is
reporting at every figure**, and reports both where both are available. It
does not attempt to reconcile them — that is D5B's, per §6.

**And Stage 2 reports how many tolerance axes actually discriminate its
kind** (ledger #7). A kind's tolerance is `ConditionNiche`'s closed four —
temperature, moisture, insolation, elevation — and underground **two are
degenerate**: insolation is documented constant zero, and elevation is metres
above *sea level*, i.e. the surface above the chamber rather than its depth,
which is verbatim The Delvers' withdrawal reason. So the authored kind has
roughly two live axes, one of which (temperature) is gradient-driven and close
to a proxy for depth. The readout states the count and which axes moved,
because the successor that unifies `ConditionNiche` and `Substrate` into the
kernel's open basis (`[[DOM-two-environment-bases]]`, Nathan's ruling
2026-09-11) should inherit a **measurement** of the poverty rather than an
argument for it.

**Source diversity is not realized ecological diversity, and the spec must not
slide between them.** `dominant_source` is **diagnostic** — retained beside the
sum, never consumed by any world number. That seven sources vary in which one
dominates (§2) says nothing yet about whether any of that variety reaches a
living thing; §2a is the proof it currently does not. M1 measures the
*availability* of an allocation axis, and only M4 measures whether a consumer
actually rides it.

## 4. Preregistered measurement

Frozen before the code that would move it (0016). Every statistic reports its
numbers **before** any verdict is drawn — The Sources' own discipline, adopted
here because its S1 nearly hid a bimodal distribution behind a p10-p90 width.

### M1 — Does composition separate worlds?

**The statistic is PER-RUNG, and that is the whole of its validity.** An
earlier draft of this section pooled every underground rung into one histogram
per seed. That draft was wrong, and the reason is measured rather than
suspected — the S3 per-rung histograms from the §2 re-run, pooled over all
twelve seeds:

```
Undercroft  [945, 6703, 6086,    0, 1417,   0, 1276]
Shallows    [946, 6205, 4451, 2726, 1953,   0,  146]
Deeps       [946, 5921, 1593, 5691, 2206,   3,   67]
Underdeep   [946, 4128, 1557, 7422, 2206, 101,   67]
Nadir       [946, 4635, 1557, 6915, 2206, 101,   67]
```

**Composition is driven hard by DEPTH.** `IronReduction` leads the shallow
rungs, `SulphideOxidation` the deep ones, and `SulphideOxidation` and
`Geothermal` are structurally absent at `Undercroft` altogether (both being
depth/gradient-gated). Depth structure is shared by every world, so a pooled
histogram's argmax is largely a function of how many chambers each rung holds
— a structural constant — and pooling would very likely return `M1 == 1` **for
a methodological reason**, which §3.3's table would then read as the
substantive null that supersedes 0966. Holding the rung fixed is what makes
the count answer the question asked.

```text
S      = {1, 7, 42, 99, 123, 256, 512, 777, 1024, 1234, 4096, 9001}   (n = 12)
R      = the five underground rungs {Undercroft, Shallows, Deeps,
                                     Underdeep, Nadir}
h(s,r) = normalized dominant-source histogram over seed s's cave-bearing
         chambers AT RUNG r (a 7-vector summing to 1)
a(s,r) = argmax(h(s,r))              -- seed s's modal source at rung r

M1(r)  = |{ a(s,r) : s in S }|       -- distinct modal sources ACROSS WORLDS
                                        at one rung
M1     = max over r in R of M1(r)

PREDICTION: M1 >= 2
```

A count, not a ratio, and it is the design quantity directly: allocation reads
composition, and the argmax is the simplest possible read. `M1 == 1` — every
world sharing one modal source at every rung — is the falsifier, and it means
the allocation axis is constant across worlds however varied it looks within
one.

**The prediction is `>= 2`, not `>= 3`, and the weakening is deliberate.** Two
is what decision 0966's quadrants actually require: the allocation axis must
take more than one value across worlds. Three would be a claim about richness
that the design does not need and that no prior measurement supports.

**Reported before the verdict, asserted on nothing:** all sixty `h(s,r)` in
full; `M1(r)` for each of the five rungs separately, since a single rung
carrying the whole result is a materially different finding from all five
carrying it; and the pairwise total-variation distances
`TV(h(s,r), h(t,r)) = ½Σ|h(s,r) − h(t,r)|` within each rung, with their
median. A positive M1 sitting on tiny TV distances would mean the argmaxes
differ by sampling noise, and the printed distances are what makes that
visible rather than hidden behind the count.

### M2 — Rock or mean?

Q1's formula, verbatim and unreinterpreted, recomputed with **only the
combination rule substituted** and every other input held fixed:

```text
E_s(rule)  = per-rung energy over seed s's cave-bearing vertices under `rule`
separation(rule) = IQR({median(E_s(rule))}) / median({IQR(E_s(rule))})

rules: (a) mean-of-seven          -- the shipped rule
       (b) max-of-seven           -- the composition-preserving extreme

PREDICTION: separation(b) >= 0.25     -- the bar Q1 set and (a) failed
```

**Positive control, and it is not optional:** `separation(a)` must reproduce
`0.145249` to six decimal places. A reimplementation that does not reproduce
the shipped rule's own published number is measuring something else, and every
conclusion drawn from (b) would be uninterpretable. **If the control does not
reproduce, M2 reports that and asserts nothing else** — a mismatch is itself a
finding about the committed number.

Choosing `max` as (b) is deliberate and narrow: it is the combination rule that
discards the least composition, so it bounds how much the mean is costing. It
is a **diagnostic instrument, not a proposed replacement** — nothing in this
campaign adopts it.

### M3 — Does the ruler stay unreachable? (diagnostic)

Under each rule, the corpus-band occupancy table (§2's five columns) and the
realized max. No prediction; §3.4 makes it a deliverable either way.

### M4 — Is the chemotrophic weight load-bearing, or decorative? (Stage 2)

Frozen now, before the kind is authored, for the same reason M1 and M2 are.

```text
arm (a) FULL        the authored kind, unmodified
arm (b) ABLATED     the same world with the CHEMOSYNTHATE supply zeroed
arm (c) PER-SOURCE  seven variants, each zeroing one EnergySource in turn

placed(arm)   = settlements placed for the kind, over the frozen seed set
capacity(arm) = the kind's summed carrying capacity over cave-bearing vertices

PREDICTION: placed(b) < placed(a)
```

**The falsifier is the campaign's own failure mode.** `placed(b) == placed(a)`
means the `CHEMOSYNTHATE` weight is **decorative** — the kind places on its
other niche axes and chemical energy underground still feeds nothing settled,
which is precisely the defect §2a documents and this campaign exists to close.
That is a RED, not a curiosity: the authoring returns to the niche rather than
the finding being written up.

**Arm (c) is the provenance half, reported and not asserted.** Zeroing each
source in turn and reporting the placement and capacity delta per source says
*which* rock chemistry the kind actually depends on. It is the direct
measurement of the thing `subterranean_energy`'s mean destroys, and it is why
the ablation is per-source rather than a single on/off.

**`DetritalImport` is called out separately in arm (c)'s report**, because it
is not rock: it reads `drainage` and is surface productivity flowing downward
(§2). A kind that turns out to depend on `DetritalImport` is not living on
chemical energy at all — it is eating the surface at a depth, and that would be
a finding about the kind's authoring rather than a success.

**What M4 does NOT measure: trade.** D5B's chokepoint 6 puts the real
bottleneck at exchange topology (seed 5: 6,738 positive capacities, 105
endpoints with neighbours, zero complementary neighbours, zero proposals).
Reaching production and exchange from here would pull this campaign into D2.
Capacity, placement and rung are measured; output and trade are named as the
next consumer's question and handed off in §6.

## 5. Determinism and save format

**This section changed materially when Stage 2 was re-pointed (ledger #5), and
the change is the one a reader must not skim.** The original spec said the
campaign touched neither, which was true of a measurement-only Stage 2 and is
no longer true of one that authors a species kind.

**Stage 1 is unchanged and still touches nothing.** It reads existing fields
over existing worlds through the existing probe's construction path, draws
nothing, registers no stream label, commits no fact, writes no artifact.

**Stage 2 adds a kind, and a kind moves worlds.** Placement, capacity and
occupancy all change, so every artifact whose content depends on placement
moves — which is the intended observable and is handled by `make rebaseline`,
not by an epoch. The precedent is directly on point: The Tidemark's §6 argues
that adding peoples is additive because `Seed::derive(label)` keys every
stream independently, so a draw on one label cannot perturb another's
sequence.

**That precedent is a hypothesis here, not a licence, and Stage 2 verifies it
before committing.** The branch table, so the implementer meets a rule:

- *No new stream label and no reordering of an existing stream's consumption*
  → additive. Rebaseline, commit the moved artifacts in the same commit, no
  epoch suffix.
- *A new stream label is required* → still additive (independently keyed), but
  it is a save-format contract and goes in the stream manifest deliberately.
- *An existing stream's consumption order moves* → **STOP.** That silently
  corrupts every world. It needs an epoch suffix and returns to Nathan before
  any of it is written.

**A census refresh is expected at close** — the committed goldens are what the
calibration batteries assert against, and a new kind moves them.

## 6. What is deliberately NOT in this campaign

- **The ceiling itself.** Rung 3's two-coordinate derived object is not built
  here. It moves to a successor **with Stage 1's measurement in hand**, which
  is strictly more than it would have had. Decision 0966 governs it and stands
  whether or not this campaign ever returns to it.
- **The extensive stock.** A budget that can be *eaten* and *spread through*
  must be extensive (energy density × habitable volume);
  `subterranean_energy` is an intensive ratio and cannot be consumed. Rung
  4's, per The Winze C.4's placement of the field/variant/draw.
  `[[BIO-underworld-budget-is-intensive]]`.
- **A second underworld kind, and any roster.** §3.5's one-kind rule.
- **Correcting the calibration.** §3.4 establishes the gap with a number.
  Which of {retune a source, change the combination rule, rescale the corpus
  ruler} closes it is a separate decision with blast radius across every
  shipped reading — and retuning a source to move a measured number is the
  exact act The Sources' probes forbid in their own doc comments.
- **The marine half of rung 4.** The Tidemark's, by its spec §7 and ledger #5,
  confirmed on the wire both ways. This campaign authors no marine kind and
  gives no `Surface`-realm kind a `CHEMOSYNTHATE` weight.
- **The capacity/seating rung mismatch.** The Staple D5B's chokepoint 2, and
  it is **measured, not open**: its probe found underground endpoints where the
  capacity-winning rung differs from the seated rung, so "the rock supports X"
  and "the people occupy X" are distinct claims. This campaign's readouts name
  which of the two they report (§3.5) and reconcile neither. The reconciliation
  is D5B's.
- **Production, exchange and trade.** D5B's chokepoints 4-6: harvest discards
  astronomical richness, goods collapse to generic A/B stocks, and exchange is
  the real final bottleneck (seed 5: 6,738 positive capacities, 105 endpoints
  with neighbours, **zero** complementary neighbours, zero proposals). M4 stops
  at capacity, placement and rung deliberately. A settled chemotroph that
  places and still trades nothing would be a D2 finding, not a refutation of
  this campaign — and D5B is the campaign that can tell the difference.
- **Naming the tenant.** The Winze §4.6/§7 refuses it (`thaumic` stays 0.0,
  nothing named). A settled people is not a tenant-with-a-range; this campaign
  authors an ordinary species row and names no horror.

## 7. Provenance

One `ideonomy-plain` pass (cross-domain re-instantiation + organon-construction;
list; side-effect / materiality / modularity), ledger #3. It produced the
intensive/extensive fork in §6, sharpened 0966's mechanism to
competition-under-one-budget, and surfaced §3.2's two-causes question. Fire
ecology supplied the closest cross-domain match — fuel load as budget, fuel
composition as regime — and immunology's colonization resistance a second;
both are recorded in the ledger as candidate mechanisms and **neither is
adopted here**.
