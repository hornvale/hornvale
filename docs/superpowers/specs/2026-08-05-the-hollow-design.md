# The Hollow — a cave is three questions, and they were all asking one field

**Campaign:** The Hollow (terrain — the cave model)
**Status:** spec, awaiting G3
**Predecessor:** The Lode (2026-07-22) shipped the model; C2a The Deep Realm's
Task 0 found it hollow and stopped.
**Successor:** C2a The Deep Realm resumes on this.

---

## 1. Why

`GeneratedTerrain::cave_at` has shipped since The Lode and has never had a
consumer that cared. C2a The Deep Realm proposed to build an underworld on it,
measured it first, and found a substrate that cannot carry one: over 30 seeds
and 469,122 land cells, 0.26% of land has a cave, 100% of those are `Karst`,
100% sit at `depth_reach_bands = 2`, and 3 of 30 worlds have no cave at all.

The 1000-world census agrees at scale and has been publishing the finding the
whole time: `cave-fraction` puts **999 of 1000 worlds in the `[0, 0.02)`
bucket**, and seed 42's almanac reads *"0% of the land is cave country."*

This campaign fixes the model. It runs **before** C2a resumes, on a cost
asymmetry: today the fix moves derived artifacts; the moment C2a's chamber
graph reads `cave_at`, the same fix moves the underworld's geometry in every
world.

## 2. What is wrong, measured

Three defects. They are symptoms of one disease, stated in §2.4.

### 2.1 Two of three `CaveKind` branches are unreachable

```rust
// existence — provider.rs:264, via lithology.rs:425
cave_proneness = carbonate * porosity * (0.85 + 0.15 * wetting)
exists iff noise < presence_prob(cave_proneness, belt)

// kind — features.rs:36, asked ONLY where existence already passed
carbonate > 0.5 -> Karst
silica    < 0.3 -> LavaTube     <- requires carbonate LOW
else            -> Fracture     <- requires carbonate LOW
```

Existence requires carbonate **high**; both non-`Karst` branches require it
**low**. A cave can therefore only exist on a cell where the `Karst` test has
already passed. `LavaTube` and `Fracture` have been dead code since The Lode.

They are not dead for want of eligible rock. Measured over 10 worlds /
154,249 land cells, ignoring existence: **13.59% of land is karst-eligible,
26.54% lava-tube-eligible, 59.87% fracture-eligible** (`silica` bottoms out at
0.0877, so mafic land is abundant). The rock is there; the ordering forbids it.

### 2.2 The depth mapping cannot express its own range

```rust
depth_reach_bands = 1 + (cave_proneness * 3.0) as u32   // provider.rs:279
```

Band 3 requires `cave_proneness >= 2/3`; band 4 requires **exactly 1.0**.
Measured `cave_proneness` maxes at **0.5073** over 10 worlds — and the
*theoretical* ceiling is `carbonate_max(0.7) x porosity_max(0.8191) x 1.0 =
0.573`. **Band 3 is unreachable at the maximum, not merely rare**; band 4 is
unreachable by construction. The observed 100%-band-2 is the arithmetic
working as written.

Raising `cave_proneness` cannot fix this. The mapping is what is wrong.

### 2.3 `presence_prob` is a probability in name only — the dominant defect

The gate compares a computed ratio against a 4-octave fbm sample, whose mass
is concentrated near 0.5. It is not uniform, so `noise < prob` is not a
Bernoulli(`prob`) trial.

Recovering the CDF empirically — within a `presence_prob` bucket, the observed
hit rate **is** `P(noise < prob)` — over 10 worlds / 154,249 land cells:

```
  presence_prob bucket    land cells    caves    realized rate
  [0.00, 0.05)              133293          0     0.00000
  [0.20, 0.25)                1608          0     0.00000
  [0.25, 0.30)                5829          2     0.00034
  [0.30, 0.35)                8280        115     0.01389
  [0.35, 0.40)                4984        255     0.05116
  [0.40, 0.45)                 255         56     0.21961

  max presence_prob over all land = 0.4132
```

A nominal 35% fires at 1.4%. `presence_prob` never exceeds 0.4132 anywhere on
land, so the gate operates entirely inside the noise's left tail. **This, not
carbonate scarcity, is the leading cause of the 0.26% prevalence.**

This is a defect against stated intent: The Lode's spec §1 introduces
`cave_proneness_at` and `prospectivity_at` as "*probability fields*".

**The sibling code accidentally proves it.** `deposit_at` runs the same gate
and is healthy only because of a bypass: `areal` ores (`ChemicalSediment` |
`Placer`, provider.rs:298) skip the noise test entirely. The census reports
`deposit-density >= 0.3` in 93.2% of worlds and a dominant commodity of **salt
at 98.6%** — an areal ore. The features that appear reliably are exactly the
ones that never ask the gate.

### 2.4 The disease: three questions, one field

Existence, kind and depth all read `carbonate`, and two of them read it in
opposite directions.

```
  derived field over cells
   +- located subsurface feature       <- the shared point-process gate (2.3 lives here)
       +- ore deposit                  <- deposit_kind(rock, boundary, buf, endorheic, age)
       |                                  -> Option<(process, commodity)>
       |                                  KIND FIRST, then presence. No branch can die.
       +- cave                         <- presence FIRST, then kind.
           +- existence  <- carbonate x porosity x wetting
           +- kind       <- carbonate again, INVERTED    [2.1]
           +- depth      <- carbonate again, x3          [2.2]
```

**And nothing was reading the answers.** `depth_reach_bands` is *write-only* on
`main`: constructed at provider.rs:279, declared at features.rs:30, read by no
code in the repository. `cave.kind` is read in exactly one place — the features
lens's palette — and a lens showing one color is not obviously showing one
color. A field nothing reads cannot be observed to be wrong, which is how three
structural defects survived a campaign, a census metric, an almanac line and a
committed map for two weeks.

Two further facts make the concentration fatal rather than merely inelegant:

- **`carbonate` is bimodal with an empty middle.** 133,293 land cells lie in
  `(0, 0.1)`; 20,956 lie at exactly 0.7; **none lie between.** It is
  functionally a limestone flag. No rescaling makes it a smooth driver.
- **The 5-band stratigraphic column is right there and never read**, though
  `depth_reach_bands` is literally a count of it.

### 2.5 What was already right, and must survive

- **Clustering.** 96.7% of cave cells have a caved neighbour. Caves come in
  country, which is geologically correct and is the fbm field earning its
  keep.
- **`Karst`'s own proneness.** `carbonate x porosity x wetting` is a sound
  dissolution model. It is the two questions borrowing it that is wrong.
- **`prehuman_scar_at`** (provider.rs:341) compares the same raw fbm against
  `PREHUMAN_SCAR_THRESHOLD`, a constant its own doc records as calibrated
  against terrain's internal presence noise. It is the one call site that did
  the honest thing, and this campaign must not disturb it.

## 3. The target design

**Invert the derivation order to match the sibling: choose the kind first from
the field that kind's process actually needs, ask existence against that kind's
own proneness through a genuinely uniform gate, and take depth from the column
rather than from a dimensionless ratio.**

### 3.1 Kind first, by strongest process

Replace `cave_kind(buf, near_fault) -> CaveKind` (asked after existence) with a
selector asked *before* it, mirroring `deposit_kind`'s `Option` return:

```rust
/// The void-opening process best supported by this cell's rock, with its
/// own proneness. `None` where no process operates.
fn cave_process(...) -> Option<(CaveKind, f64)>
```

Each kind computes its own proneness from the field its process requires:

| Kind | Proneness reads | Rationale |
|---|---|---|
| `Karst` | carbonate x porosity x wetting | unchanged — dissolution needs soluble rock, connected voids, water |
| `LavaTube` | mafic (low `silica`) x crust youth | basaltic flows drain to leave tubes; old crust has lost them |
| `Fracture` | fault proximity (`boundary_distance`) x brittleness (`induration`, low `metamorphic_grade`) | fault voids need stress and rock that breaks rather than flows |

Selection is **argmax over the three prononesses**, not a priority ladder —
self-calibrating, and it lets the mix respond to the fields rather than to a
hand-chosen order. Ties break by `total_cmp` on the proneness with declaration
order as the deterministic tie-break (the workspace float-sorting rule).
`None` when every proneness is zero.

No branch can be dead under this shape: each kind is selected *by* the field it
is then gated on.

### 3.2 A gate that is actually a probability

Warp the fbm sample through its own CDF **at the cave call site**, so
`noise_u < prob` is a genuine Bernoulli(`prob`) trial.

A monotone transform is the uniquely correct repair here because the gate is
**multi-purpose and one purpose was silently breaking the other**: fbm supplies
both the presence *rate* and the spatial *clustering*. It serves clustering
beautifully (§2.5) and rate terribly. A monotone warp preserves the spatial
ordering *exactly* — so clustering is untouched by construction — while fixing
the marginal.

**The warp must be local to the cave call site, not inside `sphere_fbm01`.**
There are three callers, and changing the shared function would break the two
that are not defective:

- `deposit_at` feeds the raw sample into `deposit_grade_tonnage(process,
  prospectivity, noise)` as a **value**, so rescaling globally would move ore
  grade and tonnage;
- `prehuman_scar_at` compares against a constant **calibrated against the raw
  distribution** (§2.5).

### 3.3 Depth from the column, typed like its sibling

`deposit_depth(process) -> BandKind` (features.rs:202) already establishes the
house form: a subsurface feature's depth is a **`BandKind`, named geology** —
not a bare count. Caves should mirror it.

```rust
pub struct Cave {
    pub kind: CaveKind,
    pub deepest_band: BandKind,   // was: depth_reach_bands: u32
}
```

The band a kind reaches follows its process's host rock, modulated by the
cell's actual column and that kind's proneness: karst dissolves the
sedimentary `Cover` and reaches `Basement` where the carbonate runs deep;
lava tubes stay in volcanic `Cover`; fractures follow faults into `Basement`
and `Roots`.

This is **The Lode's own unimplemented intent**, not a redesign: its spec §5
step 3 specifies "depth-reach from `cave_proneness` x *the cover/carbonate band
depth*". The implementation kept the ratio and dropped the band depth.

Changing the field's type — rather than retuning the `* 3.0` — is deliberate.
A count derived from a dimensionless ratio is the defect; a band derived from
bands cannot reproduce it.

## 4. Preregistration (decision 0016)

Frozen before any implementation commit. **Two instruments, deliberately at
different scales** — the census cannot see kind or depth, and the 30-seed
battery cannot see a distribution.

- **Instrument A** — the 30-seed structural battery, reusing C2a's committed
  `windows/worldgen/tests/deep_realm_substrate.rs` harness (seeds 1..=30,
  `BuildDepth::Terrain`, land = `!is_ocean`).
- **Instrument B** — the 1000-world census `cave-fraction` column, read as its
  **bucket shares**, not a median. (A median floor cannot see a tail.)

Baselines are the measured values in §2, re-run and recorded unchanged before
the first behavioural commit.

| # | Hypothesis | Success criterion | Baseline | What the null proves |
|---|---|---|---|---|
| H1 | Kind-first selection makes all three kinds occur | Each `CaveKind` >= 5% of cave cells (A) | Karst 100%, others 0% | Eligibility (13.6/26.5/59.9% of land) does not survive gating — the per-kind proneness functions, not the ordering, are the binding constraint |
| H2 | Column-derived depth differentiates | >= 3 distinct `deepest_band` values occur, and the modal band < 90% of cave cells (A) | 1 value, 100% | Depth is determined by kind alone and the column adds nothing — `deposit_depth`'s constant-per-process form was right and the modulation is noise |
| H3 | The gate fix lifts prevalence off the floor | census `[0, 0.02)` share < 50%; median cave-fraction in [0.02, 0.25]; **fail if median > 0.5**; zero cave-less worlds in A | 99.9% in `[0, 0.02)`; 3/30 worlds cave-less | The prevalence was set by eligible-rock scarcity rather than by the gate, and §2.3 — though real — is not the leading cause |
| H4 | `presence_prob` becomes a real probability | Per populated bucket, `\|realized - nominal\| / nominal < 0.25` (A) | 0.014 realized vs ~0.325 nominal (23x low) | The warp does not uniformize — the fbm marginal is not what the recovered CDF implies |
| H5 | **Guard.** Clustering survives the warp | Clustered share >= 90% (A) | 96.7% | The transform was not monotone, or fbm's spatial structure did not survive it — §3.2's central claim is false and the approach must be reconsidered |

H5 is the fragile half asserted against the robust half (decision 0097): it is
the falsifier for §3.2's monotonicity argument, and it fails the campaign
rather than being explained away.

**Calibration discipline.** The per-kind proneness constants are calibrated in
a single named task with this table already frozen. A constant retuned after
unblinding to rescue a prediction is stated as such in the chronicle.

## 5. Non-goals

- **`deposit_at` keeps the raw gate.** It shares the §2.3 defect and is masked
  by the `areal` bypass. Fixing it would move `deposit-density`,
  `dominant-commodity`, `mean-ore-grade` and ore grade/tonnage — a materially
  larger artifact change for a model with no visible symptom. Captured as a
  registry row; a source comment at the call site names it.
- **No new `CaveKind` variants.** Sea caves, glacial caves and biogenic voids
  are absent from the taxonomy; that gap is real and captured, not closed here.
- **No underworld.** No chambers, graph, access, or connectivity — that is C2a,
  which resumes on this.
- **No epoch, no new stream label.** See §6.
- **`prehuman_scar_at` is not touched.**

## 6. Blast radius

**World identity does not move.** `features.rs`'s module doc is explicit — "a
deterministic hash-noise point process: no sequential stream draws, no
committed facts, no epoch" — and `cave_at` is a pure query over
already-committed fields. Elevation, biomes, settlements and history are
unaffected. Save-format contracts (decision 0006) are untouched. **This is a
regeneration, not an epoch.**

Derived artifacts that do move:

| Artifact | Why |
|---|---|
| `book/src/laboratory/generated/the-census/{rows.csv, schema.json, summary, cave-fraction.svg}` | the `cave-fraction` metric |
| `book/src/laboratory/generated/census-of-the-meeting/{...}` | same metric, both pin sets |
| `book/src/gallery/almanac-seed-42{,-locked,-sky}.md` | the deep-lines cave count |
| `book/src/gallery/features-seed-42.png` + `.md` | `features_png` colors caves **by kind** — the lens where H1 becomes visible |
| `docs/audits/type-audit-report.md` | `Cave`'s field changes type (§3.3) |

Code that must change with the `Cave` field: **on `main`, none.** Verified by
`grep -rn depth_reach_bands --include=*.rs` — the field is constructed at
provider.rs:279, declared at features.rs:30, and **read by nothing**. Every
other consumer (`render.rs:390`, `worldgen/src/lib.rs:2834`,
`lab/src/metrics.rs:1252`) reads only `cave.kind` or `.is_some()`. The single
reader in the repo is C2a's `windows/worldgen/tests/deep_realm_substrate.rs`,
on the paused `the-deep-realm` branch, updated there rather than here.

A census refresh (`bash scripts/census-run.sh`, ~7 min local, decision 0081) is
required at the pre-merge close.

**A registry collision with the paused C2a branch.** `the-deep-realm` carries
its own `MAP-cave-model-miscalibrated` row — same ID, materially different
prose (two defects; "no consumer, no artifact moves"). This campaign rewrites
the row on `main`. Git will merge both cleanly into one file as **two rows with
a duplicate ID**; `registry_ids_are_unique` catches it, but only after the
fact. **At C2a's resumption, drop its copy and keep this one.** Recorded here
because C2a's ledger is git-ignored and dies with its worktree.

## 7. Risks

- **The per-kind proneness functions are new formulas, and §4's H1 measures
  their *outcome*, not their correctness.** Eligibility percentages say the
  rock exists; they do not say a hand-written proneness will select it at a
  sane rate. This is the campaign's likeliest source of iteration.
- **The CDF warp needs a form.** An erf-based Gaussian approximation is
  available (`libm` is on the dependency allowlist per decision 0041, and
  `kernel/src/math.rs` routes transcendentals through it), fitted to the
  measured marginal. The risk is fitting a magic constant to an undocumented
  distribution — mitigated by H4, which asserts the *realized rate matches the
  nominal one* rather than asserting the constant.
- **Changing `Cave`'s public field breaks C2a's committed Task 0 test.** Known,
  small, and cheaper now than after C2a lands.
- **A second instance of one lesson in two days.** `PSY-distribution-shape`
  (registered 2026-08-05) records that `perturb` draws a dispersion uniform
  because no shape was chosen. §2.3 is the mirror image: a value is *treated
  as* uniform when it is not. Both are "you must know the shape of the
  distribution you are comparing against." Worth a decision record.

## 8. Definition of Done

Beyond the standard DoD (chronicle entry, retrospective per decision 0020, book
freshness sweep, Confidence Gradient re-score if moved):

- The §4 table re-measured and reported, **including any falsified row as the
  headline** if that is what the numbers say.
- `MAP-cave-model-miscalibrated` flipped from `raw` to `shipped` with its
  **Where** cell pointing at this campaign.
- New registry rows: the point-ore half of the gate defect; the missing
  `CaveKind` variants.
- A decision record for the distribution-shape lesson (§7), if it survives
  review as more than a restatement of `PSY-distribution-shape`.
- Census refreshed and the artifact diff reviewed via `make lab-diff
  STUDY=the-census`.

## 9. Flagged for G3

1. **§6 corrects the handoff's premise.** "Nothing consumes a cave, so the fix
   moves no artifact and no golden" is false — three consumers, five artifact
   groups, one census refresh. The ruling's basis survives (world identity is
   untouched, and would not be after C2a), but this is a regen, not free.
2. **§3.3 changes a public field's type** (`depth_reach_bands: u32` ->
   `deepest_band: BandKind`). Not a save-format change, but it is the most
   invasive call in the spec and it breaks a committed C2a test.
3. **§5's scope line** — `deposit_at` knowingly keeps a defect this campaign
   has diagnosed and could fix in the same file.
4. **§4's H3 numbers are the least-grounded in the spec.** The `[0.02, 0.25]`
   band is argued from real-world karst extent, not from anything measured
   here. It is a genuine prediction, and the most likely row to be falsified.

---

## 10. Baseline, measured

Instrument A's baseline, produced before any behavioural commit by
`windows/worldgen/tests/hollow_readout.rs` — the same code path that will
produce the readout, so the campaign's claims are diffs against this block
rather than against memory.

Measured at commit `77124f7609b836d9d6e979aa18cc8d38b1e72256` (the plan
commit; the model code is unchanged from `main`), on the Mac, with:

```bash
cargo test -p hornvale-worldgen --test hollow_readout -- --nocapture
```

Runtime: **14.59 s** (30 worlds to `BuildDepth::Terrain`, debug profile). Under
the 60-second heavy-tier threshold, so the battery stays in the commit gate and
carries no `#[ignore]`.

```
== The Hollow readout — 30 worlds, 469122 land cells
prevalence: 1198 caves = 0.2554% of land; 3 of 30 worlds have NO cave
per-world cave fraction: p50=0.00224 p90=0.00469 max=0.01020
kind Karst: 1198 (100.0000% of caves)
kind LavaTube: 0 (0.0000% of caves)
kind Fracture: 0 (0.0000% of caves)
band Regolith: 0 (0.0000% of caves)
band Cover: 1198 (100.0000% of caves)
band Basement: 0 (0.0000% of caves)
band Roots: 0 (0.0000% of caves)
band Underneath: 0 (0.0000% of caves)
clustering: 1159 clustered / 39 solitary = 96.7446%
gate calibration — nominal presence_prob vs realized hit rate:
  [0.00,0.05)  cells=  407611  caves=      0  realized=0.00000  nominal~0.025
  [0.20,0.25)  cells=    5333  caves=      0  realized=0.00000  nominal~0.225
  [0.25,0.30)  cells=   16557  caves=      8  realized=0.00048  nominal~0.275
  [0.30,0.35)  cells=   23984  caves=    266  realized=0.01109  nominal~0.325
  [0.35,0.40)  cells=   14895  caves=    819  realized=0.05498  nominal~0.375
  [0.40,0.45)  cells=     742  caves=    105  realized=0.14151  nominal~0.425
```

### 10.1 Reading it against §2

Every §2 defect reproduces, on the same 30 seeds and the same 469,122 land
cells §1 cites:

| §4 row | Baseline claimed in §2/§4 | Baseline measured here |
|---|---|---|
| H1 | Karst 100%, others 0% | Karst 100.0000%, `LavaTube` 0, `Fracture` 0 |
| H2 | 1 depth value, 100% | 1 value (`depth_reach_bands = 2`, printed as `Cover`), 100% |
| H3 | 3/30 worlds cave-less; 0.26% of land | 3/30 cave-less; 0.2554% of land |
| H4 | ~0.014 realized vs ~0.325 nominal | 0.01109 realized vs ~0.325 nominal (**29x low**) |
| H5 | 96.7% clustered | 96.7446% clustered |

Two notes on where the numbers differ from §2, neither a correction of it:

- **§2.3's table was a 10-world probe** (154,249 land cells); this is the
  30-seed instrument. The bucket *shape* is identical — monotone, and low by
  more than an order of magnitude throughout — but the individual rates are
  not the same numbers. The largest gap is the top bucket, `0.14151` here
  against `0.21961` there, on 742 cells; H4 is evaluated against **this**
  block, not against §2.3.
- **The buckets are exhaustive over land, which §2.3 did not say.** The six
  bucket counts sum to 469,122 — every land cell — and the six cave counts sum
  to 1,198, every cave. So no land cell has a `presence_prob` in the
  uncovered `[0.05, 0.20)` gap, and none reaches `0.45`. That empty middle is
  §2.4's bimodal `carbonate` showing through the gate: `presence_prob` inherits
  the limestone flag's gap rather than smoothing it.

