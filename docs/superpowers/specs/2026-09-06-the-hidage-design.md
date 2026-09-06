# The Hidage — design

**Campaign:** The Hidage · **The Staple's D1 Task 0** ("a settlement has
worked land" — the probe that must precede the rung, standing rule 1) ·
**Decision block:** 0826–0835 · **Ledger:**
[`2026-09-06-the-hidage.md`](../ledgers/2026-09-06-the-hidage.md) ·
**Status:** implemented and closed 2026-09-06: verdict RESCALE, D1 struck
(decision 0827).

*The Tribal Hidage assessed each people's land in hides — the unit of worked
land that feeds one household — before anyone ploughed it. This campaign
assesses each people's catchments before the bake is taught to feed a
settlement from them.*

---

## 1. The claim

This campaign ships **one measurement and its verdict**, and no mechanism.
It asks the metaplan's D1 question in a form that can come back "no":

> If the deep-history bake grew a community toward the summed capacity of
> its **catchment** instead of the capacity of its **own vertex**, would any
> settlement become something other than a hamlet — and would only some?

The verdict is one of four preregistered branches (§4). Two of them kill D1
before a line of design is written. One admits it to a design campaign of its
own. One sends the finding back to the metaplan. Under every branch the
finding is recorded, the metaplan is amended, and nothing in world state
moves.

**This is the metaplan's own discipline applied to its next rung.** The
Staple was refounded by a probe that falsified its worked example before a
vocabulary was authored (`brief_axis_probe`, 4,002 occupations). Standing
rule 1 says the dynamics arc pays that cost at every rung, and this is the
first payment.

## 2. What the code says today

Read at `ceca521f8` (origin/main, 2026-09-05), and one reading corrects the
brief this campaign was opened with.

**2.1 The bake grows a community against one vertex.**
`Bake::eff_capacity` (`windows/worldgen/src/history_bake.rs:1738`) is
`caps_now()[pidx].at(vertex) * factor(era, vertex)`; `grow` (`:4063`) applies
`population *= 1 + GROWTH_RATE * (1 - pressure)` with
`pressure = population * NEED / eff_capacity` — logistic growth toward the
vertex's headcount capacity, collapse at `COLLAPSE_PRESSURE = 2.0`. A
community's ceiling is its own ground.

**2.2 The bake also SITES a community from one vertex.** Genesis
(`history_bake.rs:4342-4420`) seeds each people at 2–4 sites drawn from its
top-64 vertices ranked by `caps × river_factor` — a per-vertex score on a
*different* field from the one the community then grows against — and opens
each at `GENESIS_POP = 10`. Daughters spawn nearby. Sites are **drawn** from
a pool; nothing about a site is a watershed.

**2.3 The catchment exists, and nothing on the production path reads it.**
The metaplan (§1.1, §4 D1) says "genesis uses it; the bake discards it". The
first clause is stale. `domains/demography/src/flow.rs` (each land vertex
climbs to its highest-K neighbour; accumulation is the sum of K over every
vertex whose up-path passes through it) and `condense.rs` (attractors whose
accumulation clears a threshold, `Σ population == Σ K` exactly) are real and
tested — and `condense_tagged` has exactly one caller,
`demography_report_with_beta_from` (`lib.rs:2297`), the Lab's readout
accessor, which no build stage calls. `lib.rs:8202-8210` says so in its own
words: since The Living Community "the deep-history bake, not demography
stack-condensation, is the settlement provider", and the condensation placer
"is gone from genesis". So the watershed model lives only as an instrument,
and **no committed artifact carries a catchment distribution**. The probe has
to compute one, on the bake's own field.

**2.4 The figures the metaplan quotes are in the wrong units.** "A mean
catchment of ~22 with 182 settlements on seed 42" is The Gathering's
dimensionless suitability field at a threshold since retuned twice
(`CONDENSATION_THRESHOLD` doc: 108 settlements at 1.7 since The Confluence,
per-people since The Seam). The bake's field is headcount. Nothing in the
metaplan's numbers can be compared with a `peak_population` of 86; this
campaign's numbers can.

**Consequence for D1.** It is not a reconciliation of two live halves; it is
wiring an existing, draw-free, integer-and-comparison instrument into the one
consumer that would change the world. That is smaller than the metaplan
thought and it inherits the same determinism properties the metaplan
counted on. Whether it is worth an epoch is what §3–§4 measure.

## 3. The measurement

One `#[ignore]`d readout, `windows/worldgen/tests/suite/hidage_probe.rs`,
built like `brief_axis_probe.rs` and `capacity_cost_probe.rs`: five worlds
(seeds **42 / 7 / 13 / 100 / 1234**, default pins), public API only, prints
only, asserts nothing. Decision 0093's claim shape is `readout` — it
quantifies a distribution, and a ratchet here would freeze whatever it found.

### 3.1 The field

For each settling people `p` (the default roster: every kind whose
`social_form` is `Settled`), the probe rebuilds the bake's **growth field** at
present:

```
K_p(v) = per_species_capacity(...)[p].at(v) * seating_p.multiplier(v)
```

`per_species_capacity` is documented bit-identical to the bake's
`per_species_capacity_at` at `EraAdjust::present`; `seating_p` is
`delve_seating::seating_for` for a subterranean people and
`Seating::all_surface` (multiplier 1.0, an IEEE no-op) otherwise — the same
two calls `bake_history_from` makes (`lib.rs:7830-7840`). Stellar inputs come
from `sky_of` as in `capacity_cost_probe.rs`. **Known approximation:** the
bake's last era carries that era's `temp_offset` and `sea_level`
(`bake_eras`, `lib.rs:3924`), not present's. Every statistic below that is a
ratio is taken over one field, so the era choice moves numerator and
denominator together; the count statistic (§3.4) is in headcount and could
move by the era anomaly, which the chronicle must say if the verdict is
close to a bar.

Not summed: the siting field (`caps × river_factor`) and the Lab's
suitability field. Both are named in the chronicle as fields the probe did
not use, with §2's reasons.

### 3.2 The catchment

`hornvale_demography::flow(geo, &K_p)` — the mechanism that exists, unchanged.
For a vertex `v`: `acc_p(v)` is its accumulation, `att_p(v)` the attractor its
up-path ends at. The **catchment multiplier** is

```
m_p(v) = acc_p(v) / K_p(v)        (defined where K_p(v) > 0; m >= 1 always,
                                    and m == 1 exactly at a leaf)
```

Within one people the flow field is a tree, so catchments partition the
people's budget by construction. The probe does not build any other
catchment (§9).

### 3.3 The populations

- **P1 — alive bake settlements.** From `occupations_by_vertex`, every
  occupation with `ended == None`, keyed `(site, people)`. `N_p` is the alive
  count of people `p`; `N_s = Σ_p N_p` per seed.
- **P2 — the top-N attractors.** For each people, the attractors of `K_p`
  (vertices with `att_p(v) == v`) ranked by `acc_p` descending, ties to the
  lower vertex id, truncated to `N_p` (a people with fewer attractors than
  `N_p` contributes all of them, and the shortfall is printed; a people with
  `N_p == 0` contributes none). `A_s` is the union over peoples. It has
  the same size as P1 by construction, up to that shortfall, and needs no
  new constant: it answers
  "if this world's current settlement count sat at its largest catchments,
  what would they be worth".
- **P3 — ended occupations**, the same fields read at ruins, as a D6 readout.

### 3.4 The statistics, per seed and per people

Formulae, so the plan cannot drift from them:

```
c_s      = #{ a in A_s : acc(a) >= HAMLET_POPULATION_CEILING (150) }
c200_s   = #{ a in A_s : acc(a) >= LONGHOUSE_POPULATION_FLOOR (200) }
S1_s     = Gini( acc(a) : a in A_s )
S2_s     = Gini( m(a)   : a in A_s )
S3_s     = Spearman rank correlation( acc(a), K(a) : a in A_s )
S4_s     = max(acc over A_s) / median(acc over A_s)
occ_s    = #{ a in A_s : some alive settlement of the same people sits at a }
attr_s   = #{ (v,p) in P1 : att_p(v) == v }
hops     = distribution over P1 of the up-path length from v to att_p(v)
shared_s = #{ (v,p) in P1 : another alive settlement of p has the same att_p }
a_(v,p)  = peak_population / K_p(v)   over P1      (attainment)
m over P1, and over P3; K and acc medians for P1 against P3
```

Gini is the standard mean-absolute-difference form over the vector, 0 for a
constant vector. Every count is printed with its denominator (`N_s`).
`HAMLET_POPULATION_CEILING` is `pub`; `LONGHOUSE_POPULATION_FLOOR` is a
function-local `const` in `flesh.rs::structures_of` and is mirrored in the
probe with a comment naming its source, the convention
`capacity_cost_probe.rs` uses for `CLIMATE_ERAS`. Where `K_p(v) == 0` at an
alive site, `m` is undefined; such sites are counted and printed separately,
never dropped silently.

## 4. The preregistered decision rule

Frozen here, before the probe is written. Read over `A_s` (P2), on every one
of the five seeds:

```
  finding                                          verdict     response
  -----------------------------------------------  ----------  -------------------------------------------
  c_s == 0 on every seed                           NO CITY     D1 dies: at the world's own settlement
                                                                count, no catchment on the growth field
                                                                reaches the hamlet ceiling. Worked land
                                                                cannot make a city here; the metaplan's
                                                                dynamics arc re-plans from D2.
  c_s / N_s > 0.5 on every seed                    RESCALE     D1 dies: most settlements would clear the
                                                                ceiling, which is a uniform rescale in
                                                                disguise — SETTLERS_PER_CAPACITY already
                                                                does that, and it makes no apex.
  1 <= c_s and c_s / N_s <= 0.25 on every seed     LIVES       D1 proceeds to its own design campaign,
                                                                with §6's costs as its opening budget.
  anything else                                    MIXED       No design. The per-seed table goes back to
                                                                the metaplan §4 D1 as the finding, with
                                                                which seeds fell where; the arc's next step
                                                                is a metaplan decision, not a rung.
```

**Why a count, and why two dead poles.** A flatness statistic cannot fire:
`flow` is the drainage algorithm and drainage-basin sizes are heavy-tailed on
any field it is run over (ledger #2, pass 3). What D1 exists to deliver is
differentiation — some places larger than the ceiling every place sits under
today, and not all of them. That is a count with a floor and a ceiling, taken
against the two population bars the code already has
(`HAMLET_POPULATION_CEILING`, `LONGHOUSE_POPULATION_FLOOR`) rather than one
authored for the occasion.

**The bar is on capacity, not population.** `acc` is summed *capacity*; a
community's realized peak sits at or below it (`a` in §3.4 measures how far
below). So NO CITY is conservative — if capacity cannot clear 150,
population cannot — and RESCALE is the less certain pole: half the catchments
clearing 150 in capacity is a rescale only if attainment is high. The rule
stands as written; the chronicle reports `a` beside the verdict, and if
RESCALE fires with median attainment below 0.5 the MIXED response applies
and the chronicle says why.

### 4.1 Predictions that are not the verdict

Characterizations, stated now so that being wrong is visible:

- **S1_s ≥ 0.25 on every seed** (heavy tail — hydrology). If this fails, the
  "cannot fire" argument in ledger #2 was wrong and flatness was a live
  falsifier after all; say so.
- **S3_s ≥ 0.7 on every seed**: the biggest vertices drain the biggest
  catchments, so D1 would change *sizes* more than *who leads*. If S3 is low,
  D1 also re-orders the flagship — a larger change than the metaplan planned
  for, and D5's "comparative notability" gets a different meaning.
- **median attainment `a` over P1 in [0.5, 1.0]**: growth reaches its
  ceiling. If the median is below 0.5, the ceiling that binds today is
  pressure, remittance or mortality, not capacity, and D1 would lift a
  ceiling nothing touches — the design must begin there.
- **attr_s / N_s < 0.5**: most bake sites are not attractors of their own
  people's field, so D1's reconciliation moves settlements (by `hops`), it
  does not merely resize them.

## 5. Determinism, epoch, and artifact branch table

**Nothing serialized changes.** The probe reads worlds; it commits no fact,
draws under no label, and adds no `pub` item. So:

```
  after `make rebaseline`                     response
  ------------------------------------------  ---------------------------------
  empty diff                                  expected; proceed
  anything under docs/audits/ moved           STOP — a pub boundary changed;
                                              the probe was not written as a
                                              readout. Find the item.
  anything else moved                         STOP — epoch event nobody asked
                                              for; do not commit; diagnose.
```

Three commit-gate ratchets will touch the new file and each is a real
obligation, not a workaround: a `claim:` tag on the seed loop
(`cli/tests/suite/claim_shape.rs`), a row in
`cli/tests/fixtures/world-build-sites.tsv` (`identity:1` — five seeds, four of
which have no fixture, and one world is an anecdote), and the `#[path]` mod
line in `windows/worldgen/tests/suite.rs`. The Staple tripped all three; the
plan carries them as steps.

`clients/` is untouched. Per the brief's lesson from The Cruck, `git status`
after `make rebaseline` is a plan step, and anything under `clients/` moving
is a STOP.

## 6. Cost — budgeted either way

**This campaign.** One test file (~200 lines), one fixture row, one mod
line, one ledger, one spec amendment, one chronicle, one retrospective, one
or two decision records. Runtime: five world builds plus one `flow` per
people per world (integer work over ~40k vertices); `brief_axis_probe` ran in
16 s at The Staple's close and this probe does strictly more of the same, so
under a minute. **No epoch, no census, no pin moves.**

**If D1 LIVES, the rung it admits costs** (the metaplan's rule 3 line item,
stated now so the design campaign inherits a number rather than a mood):

- an **epoch** on the bake's stream (`history/bake/v3` is the current label;
  a growth ceiling read from a different field changes every draw sequence
  after the first epoch, so it is a new label, never an in-place change —
  decision 0073);
- a **census re-baseline** — 1,142.235 s at the most recent `| census |` row
  in `docs/timings.md` (2026-09-05T21:52:29Z, lefford, 40 cores); read that
  ledger at the time, not this figure;
- **history-adjacent pins converted from values to invariants**: at
  `ceca521f8`, `windows/lab/src/metrics.rs` registers 27 `Domain::History`
  and 22 `Domain::Settlement` metrics (a `grep -c` proxy — a claim with a
  date); the walkable band `40..=400` in
  `windows/worldgen/tests/suite/history_placement.rs:58`; the seed-42 flagship
  peak pin at `windows/worldgen/src/lib.rs:12554`; the Domesday survey
  (`book/src/domesday/`) and the two seed-42 almanacs, which re-render;
  and every study whose metric set reaches the bake: `studies/the-census`
  runs `"metrics": "all"`, so every one of those 49 registrations moves
  through it, and `the-namesake` names `name-prefix-settlement-scope`
  (the only study JSON at this SHA that names a settlement metric
  explicitly — the rest reach history only through `"all"`).
- **`SETTLERS_PER_CAPACITY` revisited** (metaplan §6 leaves it open; a
  catchment ceiling makes the constant's job — turning suitability into
  headcount — carry a second meaning).

**If D1 DIES, the metaplan pays** a §4 rewrite (D1 struck, D2 becomes the
first dynamics rung and its probe the next campaign), a `SOC-staple-ladder`
row edit, and a Confidence Gradient re-score if the bet it moves is scored
there. That is prose and one decision record. The catchment code stays as it
is: a Lab instrument, still tested.

**If MIXED,** the metaplan decides; the cost is a brainstorm, not a rung.

## 7. Preregistered acceptance claims

The probe is a readout and has no acceptance claim of its own. What is
tested is the **instrument**:

- **H1 — the statistics are validated on constructed vectors** before the
  probe runs: Gini of a constant vector is 0; Gini of a one-hot vector
  approaches 1 with length; Spearman of an identical ranking is 1 and of a
  reversed one is −1; the count against a bar on a vector straddling the bar
  returns the straddle. Unit tests in the probe file's own module, on the
  commit gate.
- **H2 — a positive control for the verdict rule**: a hand-built K field
  (the `bump_k` shape `condense.rs`'s tests use, on `Geosphere::new(3)`) with
  `N` chosen so exactly one attractor clears an authored bar must return
  LIVES; the same field with the bar above every accumulation must return
  NO CITY; with the bar below every accumulation, RESCALE. The rule function
  is the same one the probe prints its verdict from. This is the control
  memory says every empty-diff or every-seed-agrees result needs.
- **H3 — the field is the bake's**: for seed 42, the probe's `K_p(v)` at one
  alive site of one surface people equals `per_species_capacity`'s value
  there to the last bit (a tautology that guards the seating and roster
  wiring, stated as one). The subterranean multiplier is checked at one drow
  site to be `<= 1.0` and equal to `seating_for`'s.

## 8. Testing and review surfaces

- `cargo test -p hornvale-worldgen --test suite -- hidage` runs H1–H3 on the
  commit gate.
- `cargo test -p hornvale-worldgen --test suite -- hidage_probe --ignored --nocapture`
  is the readout; its output is committed to the chronicle as a table, with
  the SHA it was taken at (rule 5: a number in a document is a claim with a
  date).
- Review reads the per-seed table against §4 with the formulae in §3.4 in
  hand; the verdict is mechanical once the table exists.

## 9. What this campaign does not do

- It does not change the bake, the catchment, a label, a constant, or any
  world. It commits nothing and epochs nothing (metaplan rule 2 applies to
  reading rungs; this is a reading of the dynamics arc's premise).
- It does not build a second catchment (Voronoi around bake sites, or a
  travel-cost radius via `least_cost_from`). §6 of the metaplan leaves the
  shape to D1's design; this probe measures the one mechanism that exists.
- It does not measure catchments across eras. That is D1's climate payoff and
  its design's own probe (ledger follow-ups).
- It does not decide D1's design if D1 lives; that is the next campaign,
  with its own G3.
- It does not fix `worktree-take.sh`'s race (ledger #7, follow-ups).

## 10. Record consequences

At close, under every verdict:

- **Metaplan §1.1 and §4 D1 amended** (the metaplan is the active document
  for this arc): "genesis uses it" becomes "nothing on the production path
  reads it" with §2.3's citations; D1's restatement becomes "wire the
  existing instrument into the bake"; the §4 D1 paragraph gains a
  **Probe result** with the per-seed table, the verdict and the SHA.
- **`SOC-staple-ladder`** row: D1's status per the verdict, Where repointed
  at this spec and the chronicle.
- **Decision 0826** — a dynamics-rung probe falsifies on a count against an
  existing ceiling with a floor and a ceiling, never on flatness (the
  criterion's form, which D2–D6's probes inherit).
- **Decision 0827** — D1's verdict, as recorded.
- Chronicle `book/src/chronicle/the-hidage.md` (+ SUMMARY), retrospective
  `docs/retrospectives/the-hidage.md` (+ README row), a Confidence Gradient
  entry if the verdict moves a scored bet (grep the gradient on the
  invariant, not the wording).

## 11. G3 flags

1. **The brief's premise was wrong on one clause and this spec corrects the
   metaplan in place** (§2.3, ledger #1). Nathan may prefer a superseding
   note over an in-place amendment of an active metaplan; the spec assumes
   in-place, since The Staple's own close amended §1.1 and §4 that way.
2. **The death criterion is not the one the brief named.** The brief said
   "if catchment accumulation is spatially flat, D1 dies"; ledger #2's third
   pass argues flatness cannot fire on a drainage algorithm and replaces it
   with the two-pole count of §4. This is the decision most worth vetoing if
   the argument is wrong.
3. **The bars are the code's existing constants (150 / 200) and the band
   fractions (0.25 / 0.5) are authored here**, from the meaning of "a
   minority apex", not from any peek at the data. No probe has been run. If
   Nathan would set the band differently, now is the only time.
4. **The field is a present-era reconstruction, not the bake's last era**
   (§3.1, ledger #4). Low risk for ratios; named for the count.
5. No save-format, epoch, or determinism-contract decision is made here.
   §6 prices the one D1 would make.

## 12. Provenance

Brainstormed 2026-09-05/06 under `campaign-autopilot`, from the metaplan's
§1.1, §3, §4 D1, §6 and The Cruck's ledger follow-ups, against the tree at
`ceca521f8`. Four ideonomy passes on the approach (ledger #2). Worktree
`.claude/worktrees/the-hidage`, recycled from `the-warp` after the race in
ledger #7.
