# The Ceiling — what the rock can support

**Campaign:** The Ceiling. Rung 3 of the Underworld Larder metaplan
(`docs/superpowers/specs/2026-08-24-the-underworld-larder-metaplan.md`).

**Classification:** Architectural, measurement-gated.

**Base:** `26003913d`. **Decision in force:** 0966 (symmetric budget,
asymmetric allocation). **Predecessors:** The Gossan (rung 1), The Sources
(rung 2), The Winze (amendment C.3).

## 1. The one-sentence claim

The underworld can say how much it supports and which kind of consumer it
supports — **if** the mix of energy sources can tell worlds apart, which
nothing has measured.

The conditional is the campaign. Decision 0966 fixed the shape of the answer
before the measurement, deliberately (0016); Stage 1 asks whether the shape is
inhabitable, and **both outcomes ship**.

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

## 3. Design

### 3.1 Two quantities, one field, read twice

Decision 0966 needs two coordinates, and the seam is that **neither collapses
the seven-vector prematurely**:

- **Budget** — how much the rock supports. Magnitude.
- **Allocation** — which consumer class it favours. Composition.

`subterranean_energy` is the monolith (mean of seven); `dominant_source` is the
first crack in it. This campaign does not replace either. It asks whether the
composition half carries enough signal to be the allocation axis, and builds
the ceiling the answer supports.

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

Stage 2's shape is determined by Stage 1's result, and the branches are
enumerated here so the implementer meets a decision rule rather than a
prediction to rescue.

| M1 (composition separates?) | M2 (compression cause) | Stage 2 builds |
|---|---|---|
| **≥ 3 at some rung** | either | The ceiling on the seven-vector: budget from magnitude, allocation from composition. 0966 stands as written, and the allocation axis is **rich**. |
| **exactly 2** | either | The same ceiling, and the spec records that the allocation axis is **binary, not rich** — the quadrants are reachable but coarse, and a successor wanting finer grain must widen the read rather than assume it. 0966 stands, narrowed. |
| **exactly 1** | mean is a major cause | **The combination rule is the deliverable.** Composition is real within a world but the shipped scalar hides it; Stage 2 fixes `subterranean_energy`'s combination and re-measures M1 against the fixed field. 0966 held in abeyance, not superseded. |
| **exactly 1** | rock is the cause | **The null is the headline.** Composition cannot carry allocation. 0966 is superseded by a record choosing between C.3's original two, and Stage 2 ships the magnitude-only ceiling plus §3.4's calibration finding. |

**Row 2 is the one to expect.** No prior measurement supports richness, and
§4's own depth evidence shows the strongest composition signal running along an
axis (depth) that every world shares. Naming the expected row here is not a
prediction to rescue — §4's prediction is `M1 >= 2` and nothing below asserts
on which row lands — it is so that a result landing on row 1 or row 3 is
recognised as surprising and gets the scrutiny a surprise deserves.

Nothing in that table is a failure. The bottom row is the outcome this
project's own precedent treats as publishable (0016; The Sources shipped two
falsifications as its most valuable output).

### 3.4 The calibration finding is a deliverable in every branch

S2's result is independent of M1 and M2 and is not conditional on either: **a
ceiling derived from a field that never exceeds `0.424277` cannot say
"teeming."** `BIO-subterranean-energy-sources`' "an Underworld as lush as the
Overworld needs its own productive base" is arithmetically unreachable while
that holds, whatever the allocation axis does.

Stage 2 therefore reports, in every branch, the realized range of whatever
quantity it makes the budget, against the ruler the consumer reads. Whether to
*correct* the calibration is **not** decided here (§6) — this campaign
establishes the gap with a number and names who must close it.

### 3.5 What the ceiling IS, in the branches that build one

A derived per-world (and per-rung) reading with two coordinates, no authored
constant on either axis — 0966's consequence clause. It is a **pure function of
committed terrain and climate**, re-derived on read, and it commits nothing.
That keeps it intensive, which §6 explains is deliberate.

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

## 5. Determinism and save format

**Nothing here touches either, and that is verified rather than argued.**

Stage 1 reads existing fields over existing worlds through the existing probe's
own construction path. It draws nothing, registers no stream label, commits no
fact, and writes no artifact. Stage 2's ceiling (§3.5) is a pure re-derivation
from committed terrain and climate.

Consequently: no epoch suffix, no save-format break, no stream-order change, no
`docs/generated-paths.txt` entry. **If Stage 2's branch turns out to require a
committed quantity, that is a scope change and returns to Nathan** rather than
being absorbed — The Winze's amendment C.4 already anticipates it: "A tenant
with a range would add all three [a field, a variant, a draw], which is a
second reason it belongs to its own campaign and its own epoch."

## 6. What is deliberately NOT in this campaign

- **The extensive stock.** A budget that can be *eaten* and *spread through*
  must be extensive (energy density × habitable volume); `subterranean_energy`
  is an intensive ratio and cannot be consumed. That fork is real and is rung
  4's, per The Winze C.4's placement of the field/variant/draw. **This
  campaign derives the density and leaves the volume integral unbuilt**, and
  says so rather than discovering it at rung 4.
- **Correcting the calibration.** §3.4 establishes the gap with a number. Which
  of {retune a source, change the combination rule, rescale the corpus ruler}
  closes it is a separate decision with its own blast radius across every
  shipped reading — and retuning a source to move a measured number is the
  exact act The Sources' probes forbid in their own doc comments.
- **The marine half of rung 4.** The Tidemark's, by its spec §7 and ledger #5,
  confirmed on the wire both ways. This campaign authors no marine kind,
  touches no `TrophicMode`, and gives no `Surface`-realm kind a `CHEMOSYNTHATE`
  weight.
- **Naming the tenant.** The Winze §4.6/§7 refuses it (`thaumic` stays 0.0,
  nothing named) and rung 4 inherits that refusal. A budget bounds something
  the model still declines to name, and `CauseOfEnd::Breached`'s own doc states
  why the silence is the design.
- **Re-cutting the metaplan's rung 4.** Now split between The Tidemark and a
  successor; the edit belongs with the successor's spec. Carried in this
  campaign's ledger Follow-ups.

## 7. Provenance

One `ideonomy-plain` pass (cross-domain re-instantiation + organon-construction;
list; side-effect / materiality / modularity), ledger #3. It produced the
intensive/extensive fork in §6, sharpened 0966's mechanism to
competition-under-one-budget, and surfaced §3.2's two-causes question. Fire
ecology supplied the closest cross-domain match — fuel load as budget, fuel
composition as regime — and immunology's colonization resistance a second;
both are recorded in the ledger as candidate mechanisms and **neither is
adopted here**.
