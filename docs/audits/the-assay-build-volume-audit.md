# The build-volume audit

**Status:** committed as data, per decision 0093's consequences section and
`docs/retrospectives/the-weir.md:80` ("the broader build-volume audit... an
open follow-up"). **Task:** The Assay, Task 1. **Measured:** 2026-08-07, this
Mac (debug builds unless noted), against
`docs/superpowers/specs/2026-08-07-the-assay-design.md` and decisions
[0093](../decisions/0093-seed-hunting-is-not-a-test-mechanism.md) /
[0097](../decisions/0097-assert-the-robust-half-measure-the-fragile-half.md).

This is a document, not code. It classifies the tests 0093 charters this
audit to inventory — tests that build a world inside a seed loop — and
bounds the wider 224-test Settlements/Full-depth gate population per the
task brief's scope. Nothing here moves or deletes a test; that is later
tasks' job. Where this document's fresh measurement disagrees with the
spec's §3 (recorded at brainstorm time), **the fresh measurement wins and
the disagreement is noted** — see §1.3.

## 1. Methodology and the fresh count

### 1.1 The brief's script, run as specified

The brief's Step 1 script (regex-based: `BUILD` matches
`build_world|generated(|generate(|BuildDepth::`, `LOOP` matches a `for … in`
clause containing a digit range, `SEEDS`, or `seeds`) was run verbatim
against every `*/tests/*.rs` file. It returned **51 candidate
`file::function` matches**.

Manually reading all 51 found it both **over-counts and under-counts**:

- **Four false positives.** `LOOP`'s filter accepts *any* matching loop
  anywhere in the function body, not only the loop that varies the seed. Four
  matches build exactly **one** world at a fixed seed and loop over something
  else entirely, which coincidentally matches the digit-range filter:
  `cli/tests/scene_cost.rs::scene_api_cost_is_bounded_on_seed_42` (loops over
  `0..REGION_TILES`), `cli/tests/session_cost.rs::a_possessed_turn_stays_within_its_ceilings`
  (`0..RUNS`), `domains/astronomy/tests/genesis_properties.rs::the_pin_matrix_is_honored`
  (`0..=3u32` moon counts, one world per pin value at seed 42, not a seed
  sweep), `domains/terrain/tests/tectonic_properties.rs::pinned_supercontinent_is_sutured`
  (one seed-42 world, then `1..cratons.len()` over that world's own cratons).
- **At least twenty-nine false negatives**, all sharing one shape the
  script's `LOOP` filter cannot see: `for seed in [1u64, 7, 42, 99]` (a fixed
  seed **array**, no `..` and no literal substring `"seeds"`/`"SEEDS"`), a
  seed sweep expressed as `.iter().map(|&seed| …)` instead of a `for` loop, or
  a build reached through a same-file helper function (`fn world(seed) ->
  World { build_world(…) }`) rather than inline in the test body. All three
  are common idioms in this codebase.

### 1.2 The refined method

A second script kept the brief's `BUILD` definition but (a) matched any `for
<ident> in <iterable>` or `.map`/`.flat_map`/`.filter_map`/`.for_each`
closure whose bound variable is seed-shaped (`seed`, `s`, `seeds`,
`raw_seed`), (b) accepted an array literal of length > 1, a named
ALL-CAPS constant (`SEEDS`, `MEASURED_SEEDS`, `DIVERGENCE_SEEDS`, …), or a
numeric range as "more than one seed," and (c) resolved one hop of
same-file helper indirection (a test calling a local function that itself
calls `build_world`/`generate(`/`BuildDepth::`). Every candidate this
produced was read by hand against its source to confirm it truly builds
≥2 worlds at varying seeds (not one build plus an unrelated inner loop, the
same trap that produced §1.1's false positives).

**Scope decision for "the 57 seed-loop tests."** 0093's cost concern is
gate seconds paid on every commit. This document therefore treats the named
population as **gate-resident (non-`#[ignore]`) tests that build ≥2 worlds
across a varying seed**, classifies those in full, and separately inventories
the `#[ignore]`d seed-loop-building tests found along the way (§8) without
full per-test classification, since they already pay zero ongoing gate cost.
An `#[ignore]`d test is not automatically "handled," though — §8 flags the
ones that read as a hunt deferred rather than fixed, per the spec's own
warning that "the heavy tier is not a destination."

### 1.3 The fresh count, and where it disagrees with the spec

**This document counts 56 gate-resident seed-loop world-building tests**,
against the spec's brainstorm figure of 57. The methodology in §1.2 is why:
it both drops four of the literal script's false positives and adds
twenty-five more true positives the literal script missed (arrays, `.map`,
one-hop helper indirection) before restricting to gate-resident tests. The
two effects nearly cancel; 56 is not padded or trimmed to reach 57, it is
where independent counting landed. Per-test durations below are read from
`docs/timings/test-baseline-lefford.tsv` (recorded at `b7ce5941`, post-Weir)
unless stated otherwise; that file's `<below-floor>` bucket means the
individual test ran under 1.0 s and was folded, not that it is unmeasured.

Two further disagreements surfaced against spec §3.4, both because the fresh
measurement is a live re-run rather than a copy:

- `diachronic::a_crisis_fires_on_a_real_generated_sky` measures **13.132 s**
  on lefford's committed baseline, not the spec's 7.617 s. Both numbers are
  real; they were taken at different points as other campaigns' physics
  moved the search. The fresh (committed) number wins for planning purposes.
- `exposure::every_core_toponymic_concept_wins_a_root_somewhere_in_a_seed_sweep`
  measures **12.262 s** on lefford, not the spec's 8.412 s, for the same
  reason. Its actual loop range is `0u64..8` (8 candidate seeds), not the
  "up to 9" the spec's prose states — a minor off-by-one in the brainstorm's
  note, not a code change.

A third, more substantive finding: **a fourth reachability-checklist-shaped
test exists that neither the spec's "three hunts" (§3.4) nor its "54 of 57
are not hunts" (§3.5) counted.** See §2.4.

## 2. The five kinds, and the hunts

Per spec §3.5, four of five kinds are not hunts. Restated with this
document's numbers:

| kind | claim shape | hunt? | count among the 56 |
|---|---|---|---:|
| reachability checklist | ∀variant ∃seed | **yes** | 3 (§2.1–§2.3) + 1 new candidate (§2.4) |
| instance hunt | ∃seed | **yes** | 1 (§2.3) |
| distribution readout | rate over seeds | no | see §4 |
| property battery | ∀seed. P(w) | no | see §3 |
| seedless sweep | builds no world | no (not part of the 56; see §5) | — |

(The instance-hunt row overlaps the reachability row's count at
`a_crisis_fires`, which is the single ∃-seed-only hunt; the other three are
∀-variant-∃-seed reachability checklists. Total distinct hunts: 3 canonical
+ 1 candidate = 4, all counted once in the 56.)

### 2.1 `hydro_witness::every_hydro_variant_is_witnessed_on_a_real_world`

`domains/terrain/tests/hydro_witness.rs`. ∀`Hydro` variant, ∃ seed in `0..8`
at `GLOBE_LEVEL` (Terrain rung) witnessing it; breaks early once every
variant is seen. **Measured: 0.51 s** (spec §3.4; below the baseline's 1 s
floor, consistent). **Builds:** up to 8, Terrain rung. **Destination:**
`claim: reachability(census: hydro-variant-coverage)` — Stage 3 registers
exactly this metric.

### 2.2 `exposure::every_core_toponymic_concept_wins_a_root_somewhere_in_a_seed_sweep`

`windows/worldgen/tests/exposure.rs`. ∀ toponymic-domain concept, ∃ seed in
`0..8` (Full rung) where it reaches `ExposureClass::Steeped`. **Measured:
12.262 s** (lefford; spec's brainstorm figure was 8.412 s — see §1.3).
**Builds:** up to 8, Full rung. **Destination:**
`claim: reachability(census: toponymic-roots-won)` — Stage 3's second metric.

### 2.3 `diachronic::a_crisis_fires_on_a_real_generated_sky`

`windows/worldgen/tests/diachronic.rs`. ∃ seed in `1..=200` (Full rung) where
`crisis_from` returns `Some`. **Measured: 13.132 s** (lefford; spec's figure
was 7.617 s — see §1.3). **Builds:** up to 200, Full rung. **Cannot** route
to a synthetic world (already settled, both by the task brief and by the
spec's own correction, spec §3.4: `crisis_from` calls `observations_from`, which requires
`Sky::Generated` and derives events from live orbital mechanics — no fact
set can be hand-committed to stand in). **Destination:**
`claim: rate(census: crisis-fires)` plus one live structural arm at a
census-identified seed (Stage 3's third metric, Stage 4's retirement).

### 2.4 New finding: `non_void_roster::every_kind_is_viable_somewhere`

`windows/worldgen/tests/non_void_roster.rs`. Its own comment states the
shape plainly: *"A small seed set: this is a 'somewhere, ever' existence
check, not a distributional claim... The per-kind DISTRIBUTION is the
occupancy readout's job."* That is exactly hydro_witness's and exposure's
shape — ∀biosphere-registry kind, ∃seed in `[1, 7, 42, 99]` (Full rung, via
the local `viable_kinds_on` helper — the reason the literal script missed
it, §1.1) where the kind is viable somewhere on that world. **Measured:
30.891 s** (lefford). **Builds:** 4, Full rung. **Destination:** the same
instrument as §2.1/§2.2 — `claim: reachability(census: <a
viable-kinds-per-seed metric>)`, a coverage table over the roster.

This test was not named among the spec's "three hunts" (§3.4) nor mentioned
in "the population is heterogeneous" (§3.5). It is flagged here as a fresh
finding rather than folded silently into "the three hunts," since the spec
elsewhere states the count precisely and a future reader should be able to
tell that a fourth was found after ratification, not before.

## 3. Property batteries (∀seed. P(w)) — the kept majority

40 of the 56 are property batteries: a robust claim checked across every
seed in a fixed or ranged set, never hunting for an instance. Grouped by
crate; every row's measurement is the lefford baseline unless marked
`<below-floor>` (under 1.0 s, per §1.3).

### 3.1 `domains/astronomy` (20 tests)

All at Astronomy rung (~0.002 s/world debug, per the spec's §3.2 profiler
run — none individually exceeds the baseline's 1 s floor). Twelve tests in
`genesis_properties.rs`, four in `night_sky_regimes.rs`, four in
`tier_refinement.rs`.

| test | seeds | pin status | reason kept |
|---|---|---|---|
| `every_default_system_satisfies_every_invariant` | `0..128` | default | orbit-in-habitable-zone invariant, unpinned |
| `wanderers_pin_leaves_the_rest_of_the_sky_untouched` | `[1,7,42,99]` × wanderer counts `0..=4` | pin-isolation (compares pinned vs unpinned) | **save-format contract** — stream-consumption-order guard (CLAUDE.md) |
| `obliquity_at_zero_matches_the_anchor_drift_anchor_identity` | `0..64` | default | forcing/anchor identity at day 0, unpinned |
| `forcing_is_pin_isolated` | `0..64` | pin-isolation | **save-format contract** |
| `forcing_zero_pin_yields_zeroed_amplitudes` | `0..64` | pinned (`ForcingPin::Zero`) | pinned regime — no census home (one `pin_set`, §6) |
| `equatorial_daylight_is_flat_and_every_latitude_stays_in_range` | `0..64` | pinned (`RotationPin::PeriodHours(24.0)`) | pinned — no census home |
| `a_locked_worlds_hemispheres_cull_the_sky` | `0..32` | pinned (Locked) | pinned — no census home |
| `a_spinning_worlds_sky_is_whole_from_any_placed_vantage` | `0..32` | pinned (24h + 2 moons) | pinned — no census home |
| `pinned_moon_counts_draw_identical_node_longitudes` | `0..32` | pinned (moon count) | pinned — no census home |
| `anchor_battery_orbit_kepler_and_rotation_invariants` | `0..256` | default | Kepler/rotation invariants, unpinned |
| `neighbor_battery_counts_coordinates_and_determinism` | `0..256` | default | determinism + neighbor-count band, unpinned |
| `alignment_battery_dating_round_trip` | `0..128` | default | solstice dating round-trip, unpinned |
| `locked_worlds_freeze_the_instrument` | `SEEDS` = `[1,7,42]` | pinned (Locked) | pinned — no census home |
| `zero_obliquity_keeps_heliacal_events_but_kills_seasons` | `SEEDS` | pinned (obliquity 0, forcing zero) | pinned — no census home |
| `retrograde_flips_wheeling_not_dates` | `SEEDS` | pinned (spin) | pinned — no census home |
| `epoch_drift_moves_the_equinox_referenced_and_spares_the_orbital` | `SEEDS` | pinned (wanderers=2) | pinned — no census home |
| `every_generated_sky_keeps_the_one_sun_tier_0_promises` | 4 `regimes()` × `0..32` = 128 | mixed (default + 3 pinned) | tier-0/tier-refinement contract across regimes |
| `refinement_adds_structure_only_beneath_the_sun` | 4 × 32 = 128 | mixed | same contract family |
| `the_sun_never_leaves_the_visible_bodies_list` | 4 × 32 = 128 | mixed | same contract family |
| `the_suns_added_period_is_the_day_the_calendar_already_holds` | 4 × 32 = 128 | mixed | same contract family |

All twenty are `<below-floor>` in the lefford baseline (Astronomy-rung
builds are ~1,950× cheaper than Full, spec §3.2). The four `night_sky_regimes`
tests and the pin-isolation pair are named explicitly in the task brief as
staying live regardless of any future migration — the pinned ones because
`the-census.study.json` carries exactly one `pin_set` (§6), the isolation
pair because they are save-format contracts, not consistency checks.
`the_pin_matrix_is_honored` (§1.1's false positive) is **not** in this
table: it builds one world per pin value at a fixed seed, not a seed sweep.

**Migration candidates for a later task:** the five unpinned rows
(`every_default_system_satisfies_every_invariant`,
`obliquity_at_zero_matches_the_anchor_drift_anchor_identity`,
`anchor_battery_orbit_kepler_and_rotation_invariants`,
`neighbor_battery_counts_coordinates_and_determinism`,
`alignment_battery_dating_round_trip`) are candidates for
`claim: invariant(census: <metric>)`; the mixed-regime tier-refinement four
are not, because 3 of their 4 regimes are pinned.

### 3.2 `domains/terrain` (9 tests, excluding the hydro_witness hunt)

Terrain-rung builds cost ~0.492 s/world debug (spec §3.2).

| test | seeds | pin status | measured (lefford) | reason kept |
|---|---|---|---|---|
| `carve_properties::atolls_only_on_warm_submerged_seamounts` | `[1,7,42,99]` | default | `<below-floor>` | atoll composition band, unpinned |
| `carve_properties::trails_exist_age_ordered` | `[1,7,42,99]` | default | `<below-floor>` | seamount-chain age ordering, unpinned |
| `carve_properties::mass_balance_holds` | `[1,7,42,99]` | default | `<below-floor>` | erosion/deposition/ocean-loss books balance |
| `carve_properties::generate_level_books_account_for_every_eroded_unit` | `[1,7,42,99]` | default | `<below-floor>` | trim-volume booking matches replay |
| `tectonic_properties::every_default_globe_satisfies_every_invariant` | `0..64` | default | 7.015 s | plate-count and shape invariants, unpinned |
| `tectonic_properties::genesis_is_deterministic_across_the_sweep` | `[0,17,42,63]`, 2 builds/seed | default | `<below-floor>` | byte-identity across a small fixed set (structural-flavored; see note below) |
| `tectonic_properties::convergent_boundaries_stand_above_continental_interiors_on_average` | `0..16` | default | 1.688 s | uplift-vs-interior elevation invariant, unpinned |
| `tectonic_properties::single_craton_worlds_have_shelves_and_bimodal_hypsometry_across_the_sweep` | `1..=40` | **pinned** (`continents: Some(1)`) | 3.093 s | pinned — **no census home** (named in the task brief) |
| `tectonic_properties::single_craton_genesis_survives_maximal_terrane_stacking` | `1..=40` | **pinned** (`continents: Some(1)`) | 3.118 s | pinned — **no census home** |

`carve_properties::pinned_supercontinent_is_sutured` (§1.1's other false
positive) is **not** in this table: it builds one seed-42 world and loops
over that world's own cratons, not over seeds.

**Note on `genesis_is_deterministic_across_the_sweep`:** this is a
byte-identity claim (build twice, compare) at four fixed seeds, which the
spec's routing rubric (§5) would normally route to `structural(seed: <n>)`
"at ONE fixed seed, never a sweep." It currently sweeps four. This is not a
hunt — determinism at four named seeds is strictly more coverage than one —
but a future task could reasonably narrow it to the rubric's canonical shape
without losing anything the sweep uniquely provides; flagged rather than
acted on here.

**Migration candidates:** the six unpinned rows are candidates for
`claim: invariant(census: <metric>)`; the two pinned rows are not (§6).

### 3.3 `windows/lab` (4 property/readout-flavored tests, excluding hearth)

| test | seeds | measured (lefford) | kind / tag |
|---|---|---|---|
| `the_dial::the_dial_separates_the_poles` | `MEASURED_SEEDS = [1,2,3,42]` | 29.518 s | readout — calibration-lab convention; kept |
| `the_doctrine::the_dial_roster_law_folk_params_are_stable` | `1..=5` | 35.399 s | readout — calibration-lab convention; kept |
| `the_explanations::differing_subframes_do_not_share_one_verb` | `1..=5` | 34.651 s | invariant — subframe/verb biconditional; kept |
| `the_explanations::day_schema_competition_clears_the_floor` | `1..=5` | 34.088 s | invariant — schema-competition floor; kept |

These four are tentatively tagged rather than fully re-derived from their
internals; they read as calibration/readout-flavored rather than hunts, and
none loops to *find* an instance — the loop is over the whole fixed set
every time, with no early break. A later task should confirm the tag before
acting on it.

### 3.4 `windows/worldgen` (remaining 18 tests)

| test | seeds | measured (lefford) | kind / tag / note |
|---|---|---|---|
| `consonance_properties::witnessed_access_and_explanation_hold_over_a_real_world` | `1..=50` | 6.573 s | invariant |
| `deep_grammar::the_coherence_law` | `[1,2,3,4,10]` | 34.458 s | invariant, with an embedded vacuity guard — see note below |
| `deep_grammar::depth_landscape_measured` | `1..=3` | 15.813 s | readout |
| `diachronic::observations_at_day_zero_are_empty` | `1..=3` | 20.853 s | invariant |
| `diachronic::the_accumulation_law` | `1..=3` | 22.074 s | invariant |
| `diachronic::the_ladder_law` | `1..=5` | 36.417 s | invariant |
| `diachronic::the_prophecy_law` | `1..=5` | 30.559 s | invariant |
| `explanations::schema_competition_is_real_across_the_roster` | `1..=3` | 21.542 s | invariant |
| `explanations::no_deity_bearing_schema_ever_fires_agentless` | `1..=10` | 51.107 s | invariant |
| `history_emit::distinct_layers_tie_only_on_genuine_material_matches` | `[42,7,1000]` | not in baseline (see note below) | invariant, with re-measured corroborating counts (see file's own history of re-pins under The Salt/The Generalist/The Tolerance) |
| `solitary_tongue::chromatic_dragons_diverge_less_than_the_goblinoid_family` | `DIVERGENCE_SEEDS = [1,99,777]` | 22.323 s | invariant (margin threshold) |
| `tolerance_draw::the_draw_key_is_reachable_and_its_uniqueness_has_the_measured_shape` | `[1,42,777]` | not in baseline; own comment: "~5 s together" | reachability + invariant, dual-purpose (own doc comment: "the guard that would catch... a draw key that is not actually reachable") |
| `era_substrate::present_era_substrate_is_bit_identical_to_the_unparameterised_field` | `[42,7,1234]` | not in baseline (see note below) | structural (bit-identity) |
| `era_substrate::present_era_capacity_is_bit_identical_to_the_unparameterised_field` | `[42,7,1234]` | not in baseline (see note below) | structural (bit-identity) |
| `era_substrate::ocean_is_never_settleable_at_any_era` | `[42,7,1234]` | not in baseline (see note below) | invariant |
| `beta_calibration_freeze::beta_yields_realistic_coexistence` | `SEEDS = [1,2,3,4,42]` | 28.278 s | readout — preregistered freeze band `[1.5, 3.0]` |
| `hollow_readout::report_cave_substrate` | `SEEDS = 1..=30` (via `measure()`) | not in baseline (see note below) | readout (prints + `land>0`/`worlds==30` sanity) |
| `hollow_readout::cave_substrate_meets_preregistered_criteria` | `SEEDS = 1..=30` (via `measure()`) | not in baseline (see note below) | readout — preregistered §4 criteria, frozen at a named commit |

**Note on the "not in baseline" rows.** Seven rows above are absent from
`test-baseline-lefford.tsv` entirely — not present as an individual row and
not plausibly folded into `<below-floor>` either, since each builds at least
one `BuildDepth::Settlements`/`Full` world through `build_world`/
`build_world_to` (unlike §3.2's terrain-crate rows, which call the terrain
domain's `generate` directly and are cheap enough to fold). Three
(`distinct_layers_tie_only_on_genuine_material_matches`, and the two
`era_substrate` bit-identity tests) build 3 Full-depth worlds each via bare
`build_world`, which the spec's §3.2 profiler run prices at 3.8952 s/world
debug — call it low tens of seconds, not sub-1-second. The two
`hollow_readout` tests build 30 Terrain-depth worlds each through the
composition root (`build_world_to_with_artifacts`), which the same profiler
run prices nearer the window's 0.492 s/world terrain slice — roughly 15 s,
also not sub-1-second. This audit did not re-run any of the five to get an
exact number (out of budget for this task); they are flagged rather than
mis-cited as `<below-floor>`, which the earlier draft of this table did in
error before this check caught it. `the_draw_key_is_reachable_…`'s own doc
comment gives the only first-party number available (~5 s for its three
`BuildDepth::Settlements` builds together).

**Note on `the_coherence_law`:** its primary claim is a ∀-placed-culture
biconditional (schema ↔ noun-class) checked across the sweep — a property
battery. It also carries `any_animate_sky`, an ∃-seed side-assertion
guarding against the biconditional being vacuously true (all-Inanimate).
That side-assertion is 0093-hunt-shaped in isolation, but it rides on
builds the battery already pays for and cannot be split out at zero
marginal cost the way `hydro_witness`/`exposure` can — flagged here rather
than counted as a fifth hunt.

**Two files share a function name.** `hollow_readout.rs::report_cave_substrate`
(worldgen, above) and `deep_realm_substrate.rs::report_cave_substrate`
(§8, appendix — `#[ignore]`d) are different tests in different files; the
duplicate name is coincidental, not a bug in this audit.

## 4. Distribution readouts

Every gate-resident test tagged `readout` above (§3.3, §3.4's readout rows)
is already a distribution readout in spec's sense — measuring a rate or a
frozen band across a sweep, not hunting for an instance — and 0093 protects
all of them ("what this does not license: deleting or narrowing a sweep that
is genuinely measuring a distribution").

**The one that matters most is not in this list, because it is already
`#[ignore]`d: `hearth_population_calibration::cold_built_settlements_are_common_not_rare`
(15 seeds, 104.445 s on lefford, gate-resident despite the name suggesting
otherwise — confirmed non-`#[ignore]`d).** This is decision 0097's own
worked example verbatim: an existence claim near its threshold
("`cold*2 > built`" for at least one of 15 seeds) that is "formally an
invariant and behaviourally a value pin." 0097 already prescribes its fix —
convert to `claim: rate(census: <metric>, [lo, hi])` at n=1000 — so this
document does not re-litigate it, only confirms it belongs in this
inventory as the clearest instance of the row-three failure mode 0097
names.

## 5. Seedless sweeps (build no world)

Two confirmed examples, neither counted among the 56 because neither builds
a world:

- **`deep_realm_chamber::the_lattice_is_fixed_and_existence_is_sparse`**
  (named explicitly in the task brief). Sweeps 5 seeds (`[1,2,3,4,5]`) over
  a hand-built `Cave`/`ChamberAddr` lattice, driving the pure function
  `chamber_exists` directly. It reads like a hunt (multiple seeds) and is
  not one — there is no `generate`/`build_world`/`BuildDepth` call anywhere
  in it.
- **`tectonic_properties::default_worlds_never_trip_the_supply_fallback`**
  (found during this audit, not named in the brief). Sweeps the full
  1,000-seed frozen census population (`0..1000`) calling
  `draw_cratons`/`continental_supply` directly — grid-free, craton-level
  only, no `Geosphere`, no `generate`. Its own comment states the byte-
  identity purpose: proving the supply fallback cannot rewrite default
  worlds whose census fixtures must not drift. A second, independent
  seedless-sweep instance beyond the one the brief named.

## 6. Pinned batteries and the census's one `pin_set`

Confirmed directly against `studies/the-census.study.json`:

```
$ python3 -c "import json; d=json.load(open('studies/the-census.study.json')); print(len(d['pin_sets']), [p['label'] for p in d['pin_sets']], d['pin_sets'][0]['pins'])"
1 ['default'] []
```

carries exactly **one** `pin_set` (`default`, with an empty `pins` list).
Any claim about a pinned regime therefore has no census home unless a new
`pin_set` is added, at 1,000 worlds each. This is why the following stay
live regardless of any migration this campaign or its follow-on performs:

- `tectonic_properties::single_craton_worlds_have_shelves_and_bimodal_hypsometry_across_the_sweep`
  and `single_craton_genesis_survives_maximal_terrane_stacking` (`continents:
  Some(1)`, §3.2).
- The astronomy locked-rotation battery (`night_sky_regimes.rs`'s four
  tests) and the zero-obliquity/pin-isolation tests inside
  `genesis_properties.rs` (§3.1) — nine tests total across the two files
  once `wanderers_pin_leaves_the_rest_of_the_sky_untouched` and
  `forcing_is_pin_isolated`'s save-format-contract status is set aside.

## 7. The wider 224 Settlements/Full-depth gate tests

Per the task brief's scope, this population gets a per-crate summary and
named exceptions, not 224 individual rows. The table is carried forward from
the spec's §3.3 (measured during the brainstorm; this document's Step 1 did
not re-run a workspace-wide `cargo test -- --list` classification, which is
out of scope for the seed-loop audit):

| crate | Settlements/Full | terrain-rung | astronomy-rung | fixture-backed | no build |
|---|---:|---:|---:|---:|---:|
| `hornvale-worldgen` | 138 | 30 | 1 | 0 | 324 |
| `hornvale-book` | 40 | 0 | 0 | 0 | 11 |
| `hornvale` (cli) | 20 | 0 | 0 | 5 | 212 |
| `hornvale-vessel` | 11 | 1 | 0 | 2 | 406 |
| `hornvale-lab` | 9 | 2 | 0 | 7 | 292 |
| `hornvale-scene` | 6 | 3 | 0 | 1 | 84 |
| others | 0 | 215 | 1 | 11 | 1,280 |
| **total** | **224** | 251 | 2 | 26 | 2,609 |

**Named exceptions (What cannot move, spec §6, restated for this audit):**
byte-identity/determinism tests (need the same seed built twice); prose and
rendering (`hornvale-book`'s 41 tests, 712.7 s, asserting rendered strings —
the book's own drift-check is the better lever); save-format round-trips;
CLI/REPL surface tests (need process invocation); `hornvale-vessel`'s action
sequences (2,079 s over 129 tests — not a per-world scalar); and every
pinned regime named in §6 above.

## 8. Off-gate seed-loop-building tests found during the fresh scan (appendix)

These are `#[ignore]`d — zero ongoing gate cost today — and were not part
of the literal Step-1 script's 51 matches (found via the array/`.map`/
helper-indirection refinement, §1.2). They are listed for completeness, not
individually classified into a kind, per this task's scope note ("For the
wider... tests, a per-crate summary table plus the named exceptions is
sufficient"). Grouped by ignore-reason class:

**`heavy:` (deferred to `make gate-full`, generic reason, no per-test cost
argument recorded):**
`cli/tests/history_battery.rs::history_gates_full_world_and_cross_seed`,
`carve_properties::arcs_are_discrete`, `carve_properties::eustatic_dividend_regression`,
`warren_readout::the_blast_radius_readout`,
`health_calibration::the_null_control_holds_across_a_seed_sweep`,
`the_fare_calibration::the_fares_pilot`,
`deep_realm_rehome::report_the_xorn_before_and_after`,
`deep_realm_substrate::report_cave_substrate`,
`deep_realm_substrate::report_h2_depth_weld_and_reachability`,
`generalist_baseline::report_land_distribution_and_pre_human_fits`,
`generalist_baseline::report_the_preregistered_gause_readout`,
`generalist_distinctness::human_is_not_goblin_recentred`,
`generalist_distinctness::substituting_goblins_niche_for_humans_is_detected`,
`occupancy_readout::occupancy_readout_is_current`.

**Flagged: hunt-shaped, deferred rather than fixed.**
`carve_properties::shelf_width_asymmetry` is named explicitly in the spec
(the spec's §5, quoting itself): *"a hunt-shaped test that was `heavy:`-ignored rather
than fixed; deferring a hunt hides it."* It is the worked example for the spec's §6, item 7's
cell/entity-level rate aggregation problem — its fix needs two new metrics,
not a lift-and-shift, which is presumably why it was deferred instead. The
other fourteen `heavy:`-tagged tests above carry the same generic reason
string with no individual cost or shape argument recorded; this audit did
not re-derive each one's shape (that is a full semantic read per test, out
of this task's budget) but notes that the same risk — a hunt hiding behind
a shared ignore reason — cannot be ruled out for any of them without doing
so.

**`probe:`/`calibration:` (run by hand, one-shot):**
`rift_probe::rift_probe_tables`, `approach_ease_calibration::print_approach_ease_quantiles`,
`tense_shadow::temperature_gate_versus_era_mask`, `tilth_probe::tilth_derivation_probe`,
`insolation_probe::dominant_k_peak_under_corrected_locked_insolation`,
`keeping_probe::keeping_task0_probe`, `beta_calibration_sweep::beta_calibration_sweep`.

**`readout:` (chronicle/manual evidence):**
`branches_identity::deity_name_distinctness_readout`.

**Preregistered-not-met / fixture-authoring (deliberately not gate-enforced):**
`occupancy_readout::each_target_region_gains_a_top_ranked_occupant` ("expected
to fail until BIO-supply-drowns-niche lands"),
`occupancy_readout::regenerate_occupancy_readout` (writes the fixture the
drift check reads; must not run where CI could self-heal a real drift).

**Bare `#[ignore]`, no reason string** — a minor finding, not this
document's to fix: `confluence::measure_settlements_near_river_sweep` carries
a plain `#[ignore]` with no reason, unlike every other test in this
appendix.

## 9. Arithmetic check (Step 3)

Counted directly from the per-test lists in §2–§4 (one row per test, no
double-counting across sections — each test above is scoped to exactly one
of astronomy/terrain/lab/worldgen and appears in exactly one section):

- §3.1 (astronomy): 12 (`genesis_properties.rs`) + 4 (`night_sky_regimes.rs`)
  + 4 (`tier_refinement.rs`) = **20**
- §3.2 (terrain, excl. `hydro_witness`): 4 (`carve_properties.rs`) + 5
  (`tectonic_properties.rs`) = **9**, plus the hunt = **10** for the crate
- §3.3 (lab, excl. `hearth`): **4**, plus the rate-shaped test (§4) = **5**
  for the crate
- §3.4 (worldgen, excl. its 3 hunts): `consonance_properties` (1) +
  `deep_grammar` (2) + `diachronic` (4, excl. the hunt) + `explanations.rs`
  (2) + `history_emit` (1) + `solitary_tongue` (1) + `hollow_readout` (2) +
  `tolerance_draw` (1) + `era_substrate` (3) + `beta_calibration_freeze` (1)
  = 1+2+4+2+1+1+2+1+3+1 = **18**, plus its 3 hunts (`a_crisis_fires`,
  `exposure`, `non_void_roster`) = **21** for the crate

Total: 20 (astronomy) + 10 (terrain) + 5 (lab) + 21 (worldgen) = **56**,
matching §1.3. A second, independent tally by kind (rather than by crate)
confirms the same total:

| kind / tag | count |
|---|---:|
| pure property battery (`battery`) | 38 |
| distribution readout (`readout`) | 6 |
| reachability-checklist hunt | 3 |
| instance hunt | 1 |
| pin-isolation / save-format contract | 2 |
| battery with an embedded reachability or vacuity check (dual-purpose, not split out) | 2 |
| existence-near-threshold, rate-shaped per decision 0097 | 1 |
| structural (bit-identity) | 3 |
| **total** | **56** |

And by pin status: 39 unpinned/default, 11 pinned (no census home, §6), 4
mixed-regime (3 of 4 sub-regimes pinned), 2 pin-isolation contracts (compare
pinned against unpinned by design, so "pinned" does not describe them
cleanly) — 39 + 11 + 4 + 2 = 56.

Both cross-checks agree with the crate-by-crate tally and with §1.3's
headline. **The count is 56**, not the spec's brainstormed 57 — a
one-test difference, not a large one, arrived at independently (§1.1–§1.2)
and not adjusted to match either figure.

## 10. Summary table

A single mutually-exclusive partition of the 56 (each test in exactly one
row; sums to 56 by construction, cross-checked against both of §9's
independent tallies):

| # | population | count | disposition |
|---|---|---:|---|
| 1 | Reachability-checklist hunts | 3 (2 canonical + 1 new, §2.4) | retire → census coverage table (Stage 3/4) |
| 2 | Instance hunt | 1 | retire → census rate + one live structural arm |
| 3 | Pin-isolation / save-format contracts | 2 | stay live — determinism contract, not a consistency check |
| 4 | Pure property battery, unpinned/default | 23 | candidates for `invariant(census: …)` in a later task |
| 5 | Pure property battery, pinned | 11 | stay live — no census home (§6) |
| 6 | Pure property battery, mixed-regime (some sub-regimes pinned) | 4 | stay live — no census home for the pinned sub-regimes |
| 7 | Battery with an embedded reachability check, not split out (`the_draw_key_is_reachable_…`) | 1 | candidate for `invariant`, with its reachability half noted for a later task |
| 8 | Battery with an embedded vacuity guard, not split out (`the_coherence_law`) | 1 | candidate for `invariant`; the guard rides on builds already paid for |
| 9 | Distribution readouts | 6 | stay put (decision 0016) |
| 10 | Existence-near-threshold, rate-shaped (decision 0097) | 1 (`cold_built_settlements_are_common_not_rare`) | convert to `rate(census: …, [lo, hi])` — 0097 already prescribes this |
| 11 | Structural (bit-identity) | 3 | stay live |
| **subtotal, the 56** | | **3+1+2+23+11+4+1+1+6+1+3 = 56** | |
| 12 | Seedless sweeps (build no world) | 2 confirmed | stay live — outside the 56 |
| 13 | Off-gate `#[ignore]`d seed-loop builders | 26 | not individually classified; one (`shelf_width_asymmetry`) flagged as a hunt hiding in the heavy tier |
| 14 | Wider Settlements/Full gate tests | 224 | per-crate table, §7 (carried from spec) |

Rows 4–8 (30 tests: 23 + 11 + 4 + 1 + 1) are §3's "property batteries" as a
single pool; rows 4, 7, and 8 (25 tests) are the ones with no pinned-regime
obstruction, i.e. the realistic migration-candidate pool for a later task —
smaller than "40 kept, minus 3 readouts" would suggest, because §6's
pin-set constraint removes 15 of them (rows 5–6) outright.
