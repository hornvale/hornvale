# Lab performance — per-metric rung enumeration (Stage 2 Task 1)

Stage 2 (MAP-25) will thread narrowed `WorldView` subset types through the
lab's metric extractors so each metric builds a world only as deep as it
reads. This document is the primary control for that work: it enumerates,
by hand, the exact rung — `{ Astronomy, Terrain, Climate, Settlements, Full }`
— every metric in `registry()` (`windows/lab/src/metrics.rs`) actually reads.
A metric assigned too shallow a rung here is the one hazard Stage 2 exists to
eliminate; this table is reviewed explicitly rather than inferred
mechanically, then compiler-enforced in a later task.

**Rung ladder** (linear, deepest read wins): `Astronomy < Terrain < Climate <
Settlements < Full`. Climate is a view rung, not a build stop (it is
reconstructed from a terrain-depth world and commits no facts of its own).

Row count check: `grep -c 'name: "' windows/lab/src/metrics.rs` returns 111;
110 are `Metric.name` entries in `registry()` (matching
`registry_metric_count_is_pinned`'s pinned `110`), and 1 (line 3060) is an
unrelated `SpeciesDef.name` field for the `goblin-twin` roster control. All
110 registry entries are enumerated below, in registry order.

## Helper → rung key

Several metrics call a shared helper rather than reading `WorldView` fields
directly in their closure; the metric inherits its helper's deepest read.
Each helper is read once and recorded here so the table below can cite it
tersely.

| Helper | Deepest read | Rung |
|---|---|---|
| `flagship_of` (worldgen) | settlement placement facts (`IS_SETTLEMENT`/`PEOPLED_BY`/`CELL_ID`/`POPULATION`) | Settlements |
| `hornvale_culture::subsistence_of`, `castes_of` | settlement/culture facts on the flagship entity | Settlements |
| `flagship_surplus` | `CELL_ID` settlement fact + `v.climate`/`v.terrain` recompute | Settlements |
| `flagship_coastal` | `CELL_ID` settlement fact + `v.terrain` adjacency | Settlements |
| `species_settlement_count` | `IS_SETTLEMENT` facts + `species_of` | Settlements |
| `pantheon_sig` | `flagship_of` (Settlements) **then** `beliefs_held_by`/`cult_form_held_by`/`observed_phenomena_as_in` (religion facts) | Full |
| `pick_kobold` | pure function over two `PantheonSig`s (no `WorldView` read of its own) | inherits Full from its `pantheon_sig` callers |
| `species_generated_names` | settlement names (Settlements) **plus** flagship belief deity/epithet names (religion) | Full |
| `phonotactic_validity` | `species_generated_names` (Full) + `language_of_in` (language reconstruction) | Full |
| `epithet_honorific` | `flagship_of` + `beliefs_held_by` + `observed_phenomena_as_in` + `lexicon_of` + `language_of_in` + `Namer` | Full |
| `mean_name_length` | `species_generated_names` | Full |
| `name_collision_rate` | settlement names + `beliefs_of` deity names | Full |
| `name_gloss_true` / `settlement_site_concepts` | `NAME_GLOSS` fact + `species_of` + `observed_phenomena_as_at` (per-settlement) | Full |
| `lexicon_regular` | `language_of_in` + `draw_cascade` + `lexicon_of` | Full |
| `independently_steeped_concepts` / `exposure_sound` | roster perception + places/climate + registry concepts (species/culture facts) | Full |
| `hue_depth` | **`v.roster` only** — no `v.world` read at all | see Anomaly 1 |
| `lexicon_regular_family` | loops `lexicon_regular` over `ALL_DAUGHTERS` | Full |
| `goblinoid_proto_assignment` / `monophyly_goblinoid` / `clean_outgroup_kobold` | registry concept universe + `family_daughters` + `lexicon_of` | Full |
| `inventory_closure` | `language_of_in` + `lexicon_of` | Full |
| `divergence_magnitude` | `language_of_in` + `lexicon_of` + `nativize` | Full |
| `divergence_real` | `lexicon_of` for 3 daughters, compares `derivation.modern` | Full |
| `homophony_count` | `lexicon_of` | Full |
| `homophony_stats` (backs `core_homophony`, `confusable_homophony`, `homophony_merger_share`) | `lexicon_of` + pack membership | Full |
| `tone_count_metric` | `language_of_in` + `tone_inventory` | Full |
| `distinguishable_capacity_metric` | `language_of_in` + `distinguishable_capacity` | Full |
| `beliefs_of` | `IS_BELIEF` facts, committed ONLY at `religion::genesis` (Full depth) | Full |

## Enumeration

| # | Metric name | Rung | Justification |
|---|---|---|---|
| 1 | `star-class` | Astronomy | `v.system.star.class_name` |
| 2 | `tidally-locked` | Astronomy | `v.system.anchor.rotation` |
| 3 | `day-length-hours` | Astronomy | `v.system.anchor.rotation` (`Rotation::Spinning`) |
| 4 | `year-std-days` | Astronomy | `v.system.anchor.year` |
| 5 | `year-local-days` | Astronomy | `v.calendar.day_length()` + `v.system.anchor.year` |
| 6 | `obliquity-degrees` | Astronomy | `v.system.anchor.obliquity` |
| 7 | `obliquity-range` | Astronomy | `v.system.forcing.obliquity_amp` |
| 8 | `moons-admitted` | Astronomy | `v.system.moons.len()` |
| 9 | `refused-a-moon` | Astronomy | `v.notes.is_empty()` |
| 10 | `total-tide` | Astronomy | `v.system.moons[].tide_rel` |
| 11 | `months-per-year-innermost` | Astronomy | `v.calendar.months_per_year(0)` |
| 12 | `neighbor-count` | Astronomy | `v.system.neighbors.len()` |
| 13 | `brightest-neighbor-class` | Astronomy | `v.system.neighbors.first()` |
| 14 | `belief-kind` | Full | `beliefs_of(&v.world)` reads `IS_BELIEF` facts committed ONLY at `religion::genesis` (Full depth) — CORRECTED from Astronomy, see Anomaly 2 |
| 15 | `genesis-note-count` | Astronomy | `v.notes.len()` |
| 16 | `plate-count` | Terrain | `v.globe.plate_count` |
| 17 | `ocean-fraction` | Terrain | `v.globe.ocean_fraction` |
| 18 | `mountain-coverage` | Terrain | `v.terrain.geosphere()`/`elevation_at` |
| 19 | `band-count` | Climate | `v.climate.band_count()` |
| 20 | `habitable-fraction` | Climate | `v.climate.habitable_fraction()` |
| 21 | `unrest-coverage` | Terrain | `v.terrain.unrest_at` |
| 22 | `dominant-land-biome` | Climate | `v.climate.biome_map()` |
| 23 | `mean-land-temperature-c` | Climate | `v.terrain.is_ocean` + `v.climate.mean_temperature_at` (climate is deeper) |
| 24 | `settlement-count` | Settlements | `hornvale_terrain::places(&v.world)` |
| 25 | `mean-population` | Settlements | `places` + `POPULATION` fact |
| 26 | `flagship-subsistence` | Settlements | `flagship_of` + `subsistence_of` |
| 27 | `flagship-biome` | Settlements | `flagship_of` + `BIOME` fact |
| 28 | `flagship-coastal` | Settlements | `flagship_of` + `CELL_ID` fact + `v.terrain` adjacency |
| 29 | `flagship-structure-size` | Settlements | `flagship_of` + `castes_of` |
| 30 | `endorheic-coverage` | Terrain | `v.terrain.is_ocean`/`is_endorheic` |
| 31 | `pantheon-size` | Full | `flagship_of` + `beliefs_held_by` (religion) |
| 32 | `cult-form` | Full | `flagship_of` + `cult_form_held_by` (religion) |
| 33 | `pantheon-verticality` | Full | `flagship_of` + `beliefs_held_by` |
| 34 | `head-deity-periodicity` | Full | `flagship_of` + `beliefs_held_by` |
| 35 | `goblin-flagship-roles` | Settlements | `flagship_of` + `castes_of` |
| 36 | `kobold-flagship-roles` | Settlements | `flagship_of` + `castes_of` |
| 37 | `goblin-flagship-population` | Settlements | `flagship_of` (`info.population`) |
| 38 | `kobold-flagship-population` | Settlements | `flagship_of` (`info.population`) |
| 39 | `goblin-flagship-surplus` | Settlements | helper `flagship_surplus` |
| 40 | `kobold-flagship-surplus` | Settlements | helper `flagship_surplus` |
| 41 | `goblin-flagship-coastal` | Settlements | helper `flagship_coastal` |
| 42 | `kobold-flagship-coastal` | Settlements | helper `flagship_coastal` |
| 43 | `goblin-settlement-count` | Settlements | helper `species_settlement_count` |
| 44 | `kobold-settlement-count` | Settlements | helper `species_settlement_count` |
| 45 | `head-deity-domain-goblin` | Full | helper `pantheon_sig` |
| 46 | `head-deity-domain-kobold` | Full | helper `pantheon_sig` |
| 47 | `pantheon-size-goblin` | Full | helper `pantheon_sig` |
| 48 | `pantheon-size-kobold` | Full | helper `pantheon_sig` |
| 49 | `cult-form-goblin` | Full | helper `pantheon_sig` |
| 50 | `cult-form-kobold` | Full | helper `pantheon_sig` |
| 51 | `blind-attribution-correct` | Full | two `pantheon_sig` calls + `pick_kobold` |
| 52 | `phonotactic-validity-goblin` | Full | helper `phonotactic_validity` |
| 53 | `phonotactic-validity-kobold` | Full | helper `phonotactic_validity` |
| 54 | `epithet-honorific-goblin` | Full | helper `epithet_honorific` |
| 55 | `epithet-honorific-kobold` | Full | helper `epithet_honorific` |
| 56 | `name-length-goblin` | Full | helper `mean_name_length` |
| 57 | `name-length-kobold` | Full | helper `mean_name_length` |
| 58 | `name-collision-rate` | Full | `name_collision_rate` (places + `beliefs_of` deity names) |
| 59 | `head-deity-domain-goblin-twin` | Full | helper `pantheon_sig` |
| 60 | `pantheon-size-goblin-twin` | Full | helper `pantheon_sig` |
| 61 | `cult-form-goblin-twin` | Full | helper `pantheon_sig` |
| 62 | `name-length-goblin-twin` | Full | helper `mean_name_length` |
| 63 | `pantheon-cyclic-share-goblin` | Full | helper `pantheon_sig` (`cyclic_share`) |
| 64 | `pantheon-cyclic-share-goblin-twin` | Full | helper `pantheon_sig` (`cyclic_share`) |
| 65 | `name-gloss-true` | Full | `name_gloss_true` / `settlement_site_concepts` (`NAME_GLOSS` + `observed_phenomena_as_at`) |
| 66 | `lexicon-regular-goblin` | Full | helper `lexicon_regular` |
| 67 | `lexicon-regular-kobold` | Full | helper `lexicon_regular` |
| 68 | `exposure-sound-goblin` | Full | helper `exposure_sound` / `independently_steeped_concepts` |
| 69 | `exposure-sound-kobold` | Full | helper `exposure_sound` |
| 70 | `hue-depth-goblin` | Astronomy | helper `hue_depth` — reads `v.roster` only (on the base view); tightest correct rung, see Anomaly 1 |
| 71 | `hue-depth-kobold` | Astronomy | helper `hue_depth` — reads `v.roster` only (on the base view); tightest correct rung, see Anomaly 1 |
| 72 | `shoreline-development` | Terrain | `v.terrain.globe()` + `shape::shoreline_development` |
| 73 | `hypsometric-bimodality` | Terrain | `v.terrain.globe()` + `shape::hypsometric_bimodality` |
| 74 | `shelf-fraction` | Terrain | `v.terrain.globe()` + `shape::shelf_fraction` |
| 75 | `continent-count` | Terrain | `v.terrain.globe()` + `shape::land_component_sizes` |
| 76 | `largest-continent-share` | Terrain | `v.terrain.globe()` + `shape::land_component_sizes` |
| 77 | `plate-size-gini` | Terrain | `v.terrain.globe()` (`plate_of`) + `shape::gini` |
| 78 | `landmass-count` | Terrain | `v.terrain.globe()` + `shape::land_component_sizes` |
| 79 | `lexicon-regular-family` | Full | `lexicon_regular_family` (loops `lexicon_regular` over `ALL_DAUGHTERS`) |
| 80 | `monophyly-goblinoid` | Full | `goblinoid_proto_assignment` + `lexicon_of` |
| 81 | `clean-outgroup-kobold` | Full | `goblinoid_proto_assignment` + `lexicon_of` |
| 82 | `inventory-closure-goblin` | Full | helper `inventory_closure` |
| 83 | `inventory-closure-hobgoblin` | Full | helper `inventory_closure` |
| 84 | `inventory-closure-bugbear` | Full | helper `inventory_closure` |
| 85 | `inventory-closure-kobold` | Full | helper `inventory_closure` |
| 86 | `divergence-magnitude-goblin` | Full | helper `divergence_magnitude` |
| 87 | `divergence-magnitude-hobgoblin` | Full | helper `divergence_magnitude` |
| 88 | `divergence-magnitude-bugbear` | Full | helper `divergence_magnitude` |
| 89 | `divergence-real` | Full | `divergence_real` (`lexicon_of` × 3 daughters) |
| 90 | `homophony-count-goblin` | Full | helper `homophony_count` |
| 91 | `homophony-count-hobgoblin` | Full | helper `homophony_count` |
| 92 | `homophony-count-bugbear` | Full | helper `homophony_count` |
| 93 | `homophony-count-kobold` | Full | helper `homophony_count` |
| 94 | `core-homophony-goblin` | Full | helper `homophony_stats` (via `core_homophony`) |
| 95 | `core-homophony-hobgoblin` | Full | helper `homophony_stats` |
| 96 | `core-homophony-bugbear` | Full | helper `homophony_stats` |
| 97 | `core-homophony-kobold` | Full | helper `homophony_stats` |
| 98 | `homophony-merger-share-goblin` | Full | helper `homophony_stats` (via `homophony_merger_share`) |
| 99 | `homophony-merger-share-hobgoblin` | Full | helper `homophony_stats` |
| 100 | `homophony-merger-share-bugbear` | Full | helper `homophony_stats` |
| 101 | `homophony-merger-share-kobold` | Full | helper `homophony_stats` |
| 102 | `confusable-homophony-goblin` | Full | helper `homophony_stats` (via `confusable_homophony`) |
| 103 | `confusable-homophony-hobgoblin` | Full | helper `homophony_stats` |
| 104 | `confusable-homophony-bugbear` | Full | helper `homophony_stats` |
| 105 | `confusable-homophony-kobold` | Full | helper `homophony_stats` |
| 106 | `tone-count-goblin` | Full | helper `tone_count_metric` (`language_of_in` + `tone_inventory`) |
| 107 | `tone-count-kobold` | Full | helper `tone_count_metric` |
| 108 | `distinguishable-capacity-goblin` | Full | helper `distinguishable_capacity_metric` |
| 109 | `distinguishable-capacity-bugbear` | Full | helper `distinguishable_capacity_metric` |
| 110 | `distinguishable-capacity-kobold` | Full | helper `distinguishable_capacity_metric` |

## Rung distribution

| Rung | Count |
|---|---|
| Astronomy | 16 |
| Terrain | 12 |
| Climate | 4 |
| Settlements | 16 |
| Full | 62 |
| **Total** | **110** |

(Corrected 2026-07-12: `belief-kind` Astronomy→Full and `hue-depth-*` ×2 Full→Astronomy after Task 2's fact-commit trace — see Anomalies.)

## Sanity check against the brief's Step 2 groupings

- Every `star-*`/`*-neighbor`/`moons-*`/`obliquity-*`/`year-*`/`day-*`/`tide`/
  `month`/`genesis-note-count` metric, plus `hue-depth-*` (roster-only):
  Astronomy (16/16 ✓ after corrections). NOTE: `belief-kind` was moved OUT to
  Full — see Anomalies.
- Every `*-continent*`/`plate-*`/`ocean-fraction`/`mountain-coverage`/
  `unrest-coverage`/`endorheic-*`/`shoreline-*`/`hypsometric-*`/`shelf-*`/
  `landmass-*` metric: Terrain (12/12 ✓).
- `band-count`/`habitable-fraction`/`dominant-land-biome`/
  `mean-land-temperature-c`: Climate (4/4 ✓).
- `settlement-count`/`mean-population`/`flagship-*`/`*-settlement-count`:
  Settlements (16/16 ✓).
- Every pantheon/cult/head-deity/flagship-roles/blind-attribution/
  phonotactic/epithet/name-length/name-collision/name-gloss/lexicon-*/
  exposure-*/monophyly-*/clean-outgroup-*/inventory-closure-*/
  divergence-*/homophony-*/tone-count-*/distinguishable-capacity-* metric,
  plus `belief-kind` (religion-genesis fact): Full (62/62 ✓ after
  corrections; `hue-depth-*` moved OUT to Astronomy).

## Anomalies / design notes

Two metrics needed a "read the helper, don't infer the rung from the name"
check. **Both were resolved by controller adjudication after Stage 2 Task 2
traced the actual fact-commit sites in the code — the rows above are the
CORRECTED rungs.** Updated distribution: Astronomy **16**, Terrain 12,
Climate 4, Settlements 16, Full **62** (was 15 / … / 63 before the two
corrections below).

1. **`hue-depth-goblin`/`hue-depth-kobold` → Astronomy (was Full).**
   `hue_depth(v, species)` (`windows/lab/src/metrics.rs:1962`) is
   `v.roster.iter().find(|d| d.name == species)` → `pack_depths(&def
   .perception).hue` — a pure function of the species roster the view is
   *constructed with*, touching no committed fact, no globe, no climate. Since
   `roster` lives on the base `AstronomyView`, the tightest correct rung is
   **Astronomy**. Over-mapping to Full would only have been wasteful (Full is
   never *too shallow*), but the accurate rung is Astronomy and the metamorphic
   guard (Task 6) will confirm byte-identity across depths trivially (the value
   is depth-independent).

2. **`belief-kind` → Full (CORRECTED from Astronomy — this was an
   under-mapping, the dangerous direction).** Task 1 reasoned that
   `beliefs_of(&v.world)` reads a "pre-species genesis belief committed during
   genesis," citing the call-site doc at `lib.rs:2339` and the
   `locked_world_first_belief_is_the_ambient_tide` test. **That is wrong, and
   verified so:** the `IS_BELIEF` fact `beliefs_of` reads
   (`domains/religion/src/lib.rs:310`) is committed at exactly ONE site —
   `religion::genesis` (`domains/religion/src/lib.rs:265`) — whose sole
   pipeline call is `windows/worldgen/src/lib.rs:2086`, inside the
   `culture+religion+species` stage = **Full depth**. Astronomy genesis commits
   no belief facts. The "ambient tide" belief is *conceptually* derived from an
   early sky/tide phenomenon but the *fact* is committed late; the test builds a
   full world and checks the *first* belief, which never established
   commit-depth. Left at Astronomy, a study whose deepest metric is
   `belief-kind` would build to astronomy depth and the metric would read
   `Absent` — silently wrong. This is **the exact hazard the whole design
   exists to eliminate**, and it was caught here (Task 2's stop-point check),
   before types were written, with the metamorphic guard as the runtime
   backstop.

**Load-bearing lesson for Tasks 3-6:** `v.world` is NOT a rung-determining
field the way `v.globe`/`v.terrain`/`v.climate` are — different reads of
`v.world` pin different rungs by *which fact at what commit-depth* they touch,
which the compiler cannot see (every view carries `world`). So the four
`v.world`-reading rungs (the belief-kind Full case, the Settlements bucket, and
the Full bucket) are enforced by the runner's build-depth + the per-study
metamorphic guard, not by the type system. The guard (Task 6) MUST include a
study that selects `belief-kind` (and ideally `hue-depth-*`) so this
correction is machine-checked, not just documented.
