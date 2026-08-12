# The Glasshouse — re-centring the temperature baseline

**Status:** draft for G3 review · **Date:** 2026-08-12 · **Branch:**
`campaign/the-glasshouse` · **Autopilot:** engaged

Hornvale generates a population of snowballs. This campaign finds out why,
fixes the three causes, and makes the driver measurable so the next campaign
cannot repeat the mistake.

---

## 1. The measurement

Over the committed 1000-world census
(`book/src/laboratory/generated/the-census/rows.csv`):

```
  mean-land-temperature-c   all      n=1000  median -11.99  mean -10.00
                            spinning n= 952  median -10.49  mean  -9.23
                            locked   n=  48  median -25.90  mean -25.17
  below freezing                              71.3% of worlds
  below -20 C                                 30.5% of worlds

  dominant-land-biome       ice     651 (65.1%)   alpine 295 (29.5%)
                            taiga    20 ( 2.0%)   everything else 34 (3.4%)
  dominant-soil-order       leptosol 1000 (100.0%)
  habitable-fraction        median 0.139
  mountain-coverage         median 0.545   (fraction of land above 2000 m)
```

Earth's reference values: land annual mean **+8.6 °C**, global mean **+14 °C**,
land above 2000 m **~11%**. So the median world is **~19 K colder than Earth**
and its land is **five times more mountainous**.

Two of these are already-registered observations —
`CLIM-cold-attractor` (raw, high) recorded the −11.9 °C / 651-ice read from The
Assay's regen, and `SKY-19` named the orbit placement as the cause. This
campaign is those rows coming due, not a new discovery.

## 2. Root cause

### 2.1 The luminosity cancellation

`star.rs:173` defines the habitable zone as `[0.95·√L, 1.37·√L]` AU.
`anchor.rs:180` draws the orbit **uniform in radius** across it.
`star.rs:195` computes insolation as `L/a²`. Substituting:

```
  a = √L · (0.95 + 0.42u)   =>   S = L/a² = 1/(0.95 + 0.42u)²      u ~ U(0,1)
```

**`L` cancels exactly.** Verified against 20 generated worlds: the maximum
deviation of committed `insolation-rel` from `1/(0.95+0.42u)²` is
**7.2×10⁻⁸**. Insolation is therefore independent of the star, and is drawn
from one fixed distribution for every world:

```
  u=0.000  a=0.950√L  S=1.1080   288·S^0.25 = +22.33 C
  u=0.119  a=1.000√L  S=1.0000   288·S^0.25 = +14.85 C   <-- EARTH
  u=0.500  a=1.160√L  S=0.7432   288·S^0.25 =  -5.75 C
  u=1.000  a=1.370√L  S=0.5328   288·S^0.25 = -27.09 C

  S: median 0.743, mean 0.768, range 0.533-1.108
```

Earth sits at **u = 0.119 — the 12th percentile of Hornvale's own orbit draw.**
88% of worlds are placed at lower flux than Earth.

### 2.2 The registry's framing is backwards

`SKY-19` and `CLIM-cold-attractor` both assert the climate is *"near-uninfluenced
by its own astronomy."* It is the opposite. Measured over 142 spinning worlds
paired against the committed census:

```
  r(mean-land-temperature-c, S = L/a^2)        = +0.980
  r(mean-land-temperature-c, u   raw draw)     = -0.981
  r(mean-land-temperature-c, a   orbit AU)     = -0.224
  r(mean-land-temperature-c, L   luminosity)   = +0.013
  r(a, L)                                       = +0.953
```

Temperature is **almost entirely determined by one astronomical draw**. What is
uninfluential is the *star*, because `L` cancels out of `L/a²`. The census
reached the wrong conclusion because the only astronomical variables it held
were `a` and star class, and `a` is 95% collinear with `L` — so each looks
individually uninformative while their ratio is nearly deterministic. This is
exactly the gap `CLIM-astronomy-unmeasured` predicted ("the census never
measured the driver").

The distinction matters for the fix: *attractor* implies a basin needing
feedbacks to escape, and would put `CLIM-operators` and `CLIM-ice-albedo` on the
critical path. A mis-centred input distribution needs neither.

### 2.3 The three causes, and how they interact

Regressing census land temperature on the insolation term across 142 spinning
worlds gives `slope = 0.9886`, `intercept = -4.26 °C`, `R² = 0.9665`,
residual sd 2.56 K. Decomposing that intercept:

```
  cause                                         land-temp effect   drives uniformity?
  --------------------------------------------- ------------------ ------------------
  1 insolation draw mis-centred (Earth at p12)  -19 K vs Earth     no (but r=0.98)
  2 latitude term area-mean is +10 K, not 0     +10 K  MASKS (1)   no
  3 hypsometry: 54.5% of land above 2000 m      -14 K, near-const  YES
```

Budget: `-4.2` (insolation at median S) `+10` (latitude) `-14.3` (lapse)
`= -8.5`, against an observed spinning median of `-7.8` on the same 142 worlds.

**Cause 2 is currently the only thing keeping worlds as warm as they are.**
`temperature.rs:58-60` documents the latitude term as making the "area-mean
~15 °C", but `lat_term = 30 - 60·sin²(lat)` has an area-weighted mean of
**+10 K**, not 0, because `⟨sin²lat⟩ = 1/3` over a sphere. Fixing it *alone*
moves the census median from −10.0 to **−20.0 °C**. This dictates sequencing:
the latitude fix may never land before the greenhouse.

**Cause 3 is what drives the uniformity**, not the cold. `classify_land`
(`biome.rs:288`) evaluates **specials before Whittaker**: `Ice` if
`temp < ICE_C (-20 °C)`, then `Alpine` if elevation exceeds
`tree_line_m(lat) = 4000 - 40·|lat|`. With 54.5% of land above 2000 m and a
median land temperature of −10.5 °C, those two specials pre-empt the Whittaker
lookup on 95% of worlds, and `leptosol` (thin, steep, rocky soil) follows the
same elevation.

This also explains the ordering anomaly `CLIM-biome-classifier-mixing`
recorded — `alpine` medians **+6.8 °C** (n=295) *above* `temperate-forest`
(−0.1) and `tundra` (−1.4). `Alpine` is selected on elevation regardless of
temperature (any `T ≥ −20 °C`), so it averages over nearly the whole
non-ice population, while `taiga` can only appear on low ground in a 0–7 °C
window. **There is no temperature ladder for it to violate.** The row's
diagnosis ("mixes an elevation axis into a temperature ladder") is correct; the
implied fix (reorder the ladder) is not. See §3.4.

## 3. What this campaign changes

### 3.1 A greenhouse term (`CLIM-greenhouse`)

The bracket and the transfer function contradict each other. **1.37√L is the
*maximum-greenhouse* limit** — the distance at which a world stays habitable
*only* under a dense CO₂ atmosphere. Hornvale places worlds there and then
applies a fixed Earth-like greenhouse (`288 K · S^0.25`), so they freeze. The
HZ definition presupposes a variable greenhouse; the temperature model has a
fixed one.

The mechanism that closes this is the **carbonate–silicate weathering
thermostat**: a colder world weathers more slowly, so CO₂ accumulates and the
greenhouse thickens. This is the standard account of why the habitable zone is
as wide as it is — so the term is `physics`, justified by citation, not an
authored fudge.

Two components, because one alone fails:

- **Derived thermostat** — re-centres the distribution. Anchored at the
  bracket centre, not at Earth's orbit:

  ```
    T_surf(S) = Tc · (S/S0)^m       S0 = 0.743  (bracket-centre insolation)
                                    m  = 0.25·(1-k),  k = thermostat strength
  ```

  Evaluated over the full draw, with the latitude term corrected and
  hypsometry **as it is today** (`k=0`, `m=0.25` is the current model):

  ```
    k=0.00  m=0.2500   land p5 -39.4  med -20.0  p95  +4.9  spread 44.4
    k=0.25  m=0.1875   land p5 -30.0  med -15.0  p95  +3.8  spread 33.8
    k=0.50  m=0.1250   land p5 -20.2  med  -9.9  p95  +2.7  spread 22.9
    k=0.90  m=0.0250   land p5  -3.7  med  -1.5  p95  +1.0  spread  4.7

    anchored at bracket centre, Tc=288 K:
    k=0.50             land p5 -10.1  med  +0.6  p95 +13.7  spread 23.8
    k=0.60             land p5  -8.0  med  +0.6  p95 +11.0  spread 19.0
    k=0.70             land p5  -5.9  med  +0.6  p95  +8.4  spread 14.3
  ```

  `k=0.60, Tc=288 K` reproduces **land +9.3 °C at Earth's insolation**
  (`S=1`) against Earth's actual **+8.6 °C** — anchored on Earth's own value at
  Earth's own flux, which is Earth data, not census calibration (§6) — while
  retaining a 19 K spread. Mean-matching approaches (`k=0.90`) collapse spread
  to 4.7 K and fail §4.

  **These constants are provisional.** They are fitted against *today's*
  hypsometry, whose −14.3 K lapse penalty is baked into the land term. Task
  order in §5 refits `k` after terrain lands.

- **Drawn residual** — supplies the spread. A purely derived thermostat leaves
  temperature a deterministic function of `S`, so `r(S,T)` stays ≈1 and no new
  variety axis appears; only the curve's shape changes. `SKY-19`'s "a cold-edge
  world *saved* by a thick atmosphere" presupposes that some are **not** saved,
  which requires a residual: two worlds at the same orbit differ in atmospheric
  thickness.

**Where the residual is drawn matters.** `domains/climate/src/streams.rs`
records that climate is *"otherwise seed-free (temperature, moisture, and biome
are pure derived reads)"*. Drawing in climate would destroy that property. The
residual is therefore drawn in **astronomy** alongside the other genesis
quantities, committed as a fact, and consumed by climate as a new
`ClimateInputs` field beside `insolation` (`provider.rs:42`). This matches how
insolation already crosses the seam and preserves climate as a derived read.

### 3.2 The latitude profile

Replace `30 - 60·sin²(lat)` with a profile fitted to **Earth's observed
zonal-mean surface temperature**, and state the fit's provenance in the doc
comment.

Acceptance is a decision rule, not a predicted value. At `S=1` with the
greenhouse at its Earth anchor, the profile must satisfy all three:

- area-weighted mean within **1 K of +14 °C** (Earth's global mean);
- equatorial value within **3 K of +26 °C**;
- polar value within **5 K of −25 °C**.

`30 - 60·sin²` fails the first by +10 K. A pure `sin²` form cannot satisfy all
three simultaneously (Earth's tropics are flatter than `sin²`), so the
implementer selects the functional form; the three bounds are the contract.
The Earth zonal dataset used must be cited in the source comment — an
uncited fit is the §6 red-flag cell again.

### 3.3 Hypsometry

Bring the elevation distribution toward Earth's. Target, as a decision rule:

- `mountain-coverage` median falls from **0.545** to within **0.05 of Earth's
  ~0.11**;
- implied mean land elevation falls from **~2200 m** toward Earth's **~840 m**.

The ~2200 m figure is **inferred twice, never measured directly**: once from the
regression intercept (`lapse = 14.26 K / 0.0065 = 2194 m`) and once from
`mountain-coverage = 0.545`. The two agree, which is why it is stated — but the
first implementation task is to **measure mean land elevation directly** and
add it as a census metric, because no committed metric reports it today.

This is the workstream that touches `domains/terrain/`, and therefore the
byte-identity sculpting discipline in `domains/terrain/CLAUDE.md`.

### 3.4 The biome classifier — measure before changing

Nathan scoped the classifier in. §2.3 argues it may need **no change**: the
specials over-capture because their *inputs* are pathological, and both inputs
are being fixed here.

So this workstream is **conditional, and its gate is a measurement**:

- Land §3.1–§3.3, re-measure the biome distribution, **then** decide.
- If no biome class exceeds **50%** and `dominant-soil-order` is no longer
  frozen, the classifier needs nothing; record that as the finding and close
  `CLIM-biome-classifier-mixing` as a symptom, not a defect.
- If a class still dominates, the classifier is an independent defect and is
  fixed here, with the re-measurement as the evidence.

Independent of that gate, two classifier constants get 0106 provenance:
`ICE_C = -20.0` and `tree_line_m`'s `4000.0`/`40.0`. The tree line reaches 0 m
only at 100° latitude, so it never floors on a real world; Earth's tree line
reaches sea level near 70°. That is a provenance defect, not a behavioural one,
and is fixed by citing or re-fitting the constant.

### 3.5 The census must measure the driver (`CLIM-astronomy-unmeasured`)

None of the 23 astronomy metrics is insolation, luminosity, or orbital
distance — which is why §2.2's wrong conclusion was reachable. The facts are
already committed on every ledger (`insolation-rel`, `anchor-orbit-au`,
`hab-zone-inner-au`, `hab-zone-outer-au`), so these are pure read-side
additions. Add as lab metrics:

- `insolation-rel` — the driver itself;
- `zone-position` — the normalized `u = (a - inner)/(outer - inner)`, which is
  the actual drawn variable and the one that correlates at −0.981;
- `mean-land-elevation-m` — per §3.3, the missing terrain metric;
- `greenhouse-forcing-k` — the new drawn quantity, so it is measurable from
  the first census that contains it.

This is the cheapest item in the campaign and the one that prevents recurrence.

**A verified blocker sits on this task.** `mean-land-temperature-c` cannot be
run in a narrow study today — it **panics**:

```
  study metrics                                    result
  ------------------------------------------------ --------------------------------
  "mean-land-temperature-c"                        PANIC: climate-rung extractor
                                                   on a shallower built view
  "mean-land-temperature-c","mountain-coverage"    PANIC (same)
  "mountain-coverage"                              OK
```

The panic is a declared invariant at `windows/lab/src/metrics.rs:610`: the rung
the runner selects for a study containing this metric is shallower than
`Climate`. This is a third instance of `TOOL-rung-tag-unchecked`, and the more
severe failure mode — that row documents metrics that go silently all-`Absent`
at the wrong rung, not ones that panic. The committed census is unaffected only
because it selects the full metric set and so builds `Full`, which is precisely
the masking that row describes.

Consequence for this campaign: **the before-arm cannot be taken with a small
probe.** Either fix the rung selection as part of this task, or take the
before-arm from the committed census and record that choice. Do not plan a
narrow temperature study without checking this first.

## 4. Preregistered hypothesis and success criteria

Per decision 0016 the freeze lives here, in the spec — a study JSON has no
hypothesis field, and nothing mechanical compares a result to it.

**Hypothesis.** The cold and the uniformity have three separable causes
(§2.3) and are not an attractor. Correcting the insolation baseline, the
latitude profile, and the hypsometry will move the census population to span
glacial→temperate→warm with no biome class dominating, and Earth will become an
ordinary draw rather than a 12th-percentile one.

**Success criteria**, measured on a post-change 1000-world census:

1. `mean-land-temperature-c` median within **5 K of Earth's +8.6 °C**.
2. **Spread retained**: p95 − p5 of `mean-land-temperature-c` **≥ 15 K**. This
   is the criterion that fails a lazy fix — a strong thermostat can satisfy (1)
   while flattening the population.
3. **No biome class exceeds 50%** of worlds (from 65.1%).
4. `dominant-soil-order` **no longer frozen** at a single value (from 100%).
5. Earth's insolation (`S=1`) lands between the **25th and 75th percentile** of
   the population's temperature distribution.
6. `r(S, mean-land-temperature-c)` **falls below 0.90** — evidence the drawn
   residual actually decoupled temperature from orbit. Today it is +0.980.

**Falsification is a result.** If (2) and (3) prove incompatible — if every
parameterization that spreads temperature also keeps a class dominant — that is
the finding, and it says the biome specials are an independent defect after all
(§3.4's second branch). Report it; do not retune to rescue the prediction.

## 5. Task order

Order is forced by the interactions in §2.3, not by convenience.

1. **Census columns first** (§3.5) — read-only, no behaviour change. Establishes
   the before-arm on the driver. Must precede everything, because the current
   census cannot see insolation at all.
2. **Measure mean land elevation** (§3.3) — replaces the twice-inferred ~2200 m
   with a number.
3. **Hypsometry** (§3.3) — lands before the thermostat is calibrated, because it
   shifts the land term by ~+9 K and every constant in §3.1's table depends on it.
4. **Greenhouse thermostat + residual** (§3.1) — `k` and `Tc` fitted against
   post-(3) terrain.
5. **Latitude profile** (§3.2) — must land with or after (4); alone it costs
   10 K.
6. **Classifier gate** (§3.4) — re-measure, then decide.
7. **Census regen** — one, at the close. Requires explicit authorization (§7).

## 6. Constants and their kinds (decision 0106)

0106 binds constants this campaign touches. The audit:

```
  constant                     kind             today            action
  ---------------------------- ---------------- ---------------- ------------------
  0.95 / 1.37  (HZ bounds)     physics          UNSOURCED, and   cite; correct the
                               (runaway- /      mis-described as "derived" wording
                               max-greenhouse)  "derived"        in 3 places
  288.0        (surface mean)  earth-biosphere  unsourced        cite
  30.0 / 60.0  (lat term)      earth-biosphere  WRONG comment    replace, cite (3.2)
  LAPSE_C_PER_M                physics          "dry-adiabatic-  cite
                                                ish"
  ICE_C = -20.0                hornvale-choice  unsourced        declare kind
  tree_line 4000/40            earth-biosphere  unsourced        cite or refit
  k, Tc, residual width (NEW)  physics /        n/a              cite thermostat;
                               hornvale-choice                   declare kinds
```

`0.95`/`1.37` are the worst case: `star.rs:2`, `star.rs:19` and the generated
stream manifest all call them "derived", and a repo-wide grep finds no citation.
They are not derived — they are empirical limits from the habitable-zone
literature. 0106 states that **a wrong citation is worse than none**, because a
citation stops the reader checking; that is precisely what happened here.

**The circularity constraint.** 0106's pathology (2): an Earth-contingent value
measured against Hornvale's own census is **circular**. "Worlds should resemble
Earth's temperature range" is Earth-contingent, so:

- **Earth data alone** may settle the *anchors* — `Tc`, the latitude profile
  bounds, the hypsometry target.
- **Hornvale's census** may settle only the *distributional* criteria — spread,
  dominance, percentile placement (§4 criteria 2–6).

Tuning `k` until the census median hits +8.6 °C is the forbidden move. The
census confirms shape; Earth fixes the anchor.

## 7. Determinism, save format, and blast radius

**This is an epoch.** Flagged for G3:

- **A new drawn quantity in astronomy** (§3.1) means a **new seed label** and a
  **new position in stream consumption order** — both save-format contracts per
  `CLAUDE.md`. Every existing world's ledger changes. Per the epoch rule the
  label is introduced as a new label, never a rename, and the pin-isolation
  tests in `domains/astronomy/tests/genesis_properties.rs` must be extended so
  the pinned path consumes the same draws as the unpinned one.
- **Artifact drift is expected to be total, but the response is a branch table,
  not a prediction.** Run `make rebaseline`, then `git diff --exit-code` over the
  full path list, and route on what actually moved:

  ```
    what moved                          response
    ----------------------------------- ----------------------------------------
    book/src/gallery/                   expected (almanac temperatures) - commit
    book/src/laboratory/                expected (study rows) - commit
    book/src/reference/                 ONLY if a stream label or predicate was
                                        added; if it moved and none was, STOP -
                                        an unintended contract change
    docs/audits/type-audit-report.md    expected if any pub boundary changed
    docs/digest/                        expected (decision index + delta report)
    book/src/domesday/                  expected after the regen (pure census read)
    clients/game/core/tests/fixtures/   expected (seed-42 sessions) -
                                        rebaseline-goldens
    nothing moved at all                STOP - the change did not take effect,
                                        or the diff path list is vacuous
  ```

  The last branch is the one to take seriously: `git diff --exit-code` is
  silently vacuous against a path with no index entry, so a newly-introduced
  generated directory must be `git add`ed before the check can ever fail, and
  nothing in `regenerate-artifacts.sh` guards that.
- **`clients/` scene schemas** are cross-repo contracts (decision 0055): if a
  greenhouse value reaches `scene/*`, it is additive-or-versioned only.
- **Census regen is a carve-out** — explicit authorization, run on **lefford**
  with a full SHA, goldens committed there. Budget **15 min** (`docs/timings.md`
  records 776/887/921 s, not 0063's "~7 minutes").
- **There is no CI** (decision 0125). The local gate is the only gate; nothing
  runs the drift check automatically.

## 8. Out of scope

- **`CLIM-ice-albedo`** — its own row carries a sequencing warning against
  landing before the baseline re-centres, and a positive feedback over a
  651/1000-ice census deepens the freeze. This campaign is its unblocker.
- **`CLIM-operators`** — the solver spine. §2.2 removes it from the critical
  path: a mis-centred input distribution needs no relaxation solver.
- **`CLIM-ocean-currents` SST feedback**, **`CLIM-ocean-fraction-band`**
  (ocean-fraction spans only 0.507–0.730, a real narrowness but a separate
  cause), **locked-world enrichment** (`CLIM-locked-regime`; the 48 locked
  worlds at −25.9 °C median are left to it).

## 9. Risks

1. **Criteria (2) and (3) may be incompatible.** Mitigated by §4's explicit
   falsification clause and §3.4's conditional branch.
2. **The thermostat gets calibrated twice** — once provisionally, once after
   terrain. §5 orders the work to make the second fit the real one; the first
   exists only to prove the form spans the target range.
3. **Terrain is the byte-identity pipeline.** `domains/terrain/CLAUDE.md`'s
   discipline governs; hypsometry changes must not perturb sculpting order.
4. **Downstream systems may be tuned to the cold.** `FREEZE_C = -10 °C`
   (`worldgen/lib.rs:2435`) sits almost exactly at today's median land
   temperature (−10.49), and `HABITABLE_MIN_C = -5.0`,
   `TEMPERATE_BASELINE_C = 14.0`, `ICE_C = -20.0` all reference the same scale.
   Warming the population by ~19 K moves every world across those thresholds at
   once. Decision 0107 ("cold ground is not gated, it is poor") means these
   should degrade gracefully rather than switch — but that is an expectation to
   **test**, not to assume, and demography/settlement metrics belong in the
   post-regen review surface.
5. **Two campaigns are live.** `the-beacon` has a worktree. Absorb main at every
   plan-stage boundary; read the other branch's chronicle, not just its diff
   (`make preflight` has no opinion on semantic collision).

## 10. Decisions

Ledgered in `.superpowers/sdd/decision-ledger.md` (#1–#9), promoted here at G3.
The two carve-out escalations Nathan decided directly:

- **#4** — resolve the HZ/greenhouse contradiction by **adding the greenhouse
  knob**, keeping the full bracket (over narrowing it to ~[0.95, 1.05]√L, or
  deferring the knob).
- **#7** — **include** the biome classifier, refined by §3.4 into a
  measurement-gated workstream.
- **Hypsometry in** — the third cause, found after the first two scope calls.

Registry updates this campaign owes: correct `SKY-19` and
`CLIM-cold-attractor` (§2.2); resolve `CLIM-astronomy-unmeasured` (§3.5);
`CLIM-greenhouse` and `CLIM-biome-classifier-mixing` raw → in-progress; a new
row for hypsometry if §3.3 does not close it; note this campaign as
`CLIM-ice-albedo`'s unblocker.
