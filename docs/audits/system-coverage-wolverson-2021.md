<!-- GENERATED FILE — do not edit. Regenerate with `hornvale systems --corpus systems/wolverson-2021.system.json report`. -->

# System coverage

## Provenance

- **Corpus:** `wolverson-2021`
- **Source:** Herbert Wolverson, Roguelike Tutorial - In Rust
(bfnightly.bracketproductions.com/rustbook/), table of contents fetched
2026-08-15 (two independent fetches agreed; a third fetch during Task 1
confirmed the same 74-item numbering with no discrepancy). A pedagogical
sequence for building one ECS roguelike on bracket-lib, not a specification
of what a roguelike is: an instrument with known bias, never a standard. Its
ordering is a teaching order, and its later sections are one specific game's
content design. Verdicts are authored by Hornvale about itself, which is a
weaker authority than the blind mapping tropes/tvtropes-2012 used; the
anchor discipline is what constrains it, and it constrains `present` least.
Coverage measures reach against this catalogue only. Introduction,
Contributors and Licensing are unnumbered and excluded from the item list:
they carry no capability claim. Building for the Web (id 1) is numbered and
included as front-matter about the tutorial's own toolchain, not a world
capability.
- **Frozen:** before first measurement, The Compendium

## Reading this report

`refused` and `deferred` are strongly checked: a decision must be in force,
and a registry row must exist and must not read `shipped`. `present` is only
weakly checked — a path that exists is not a working feature, and a
resolvable test name is not proof that the capability is met. `present` is
the verdict this instrument is least entitled to, and that is printed here,
above the tally it most affects.

## Tally

- present: 27 (36%)
- refused: 12 (16%)
- deferred: 21 (28%)
- absent: 9 (12%)
- inapplicable: 5 (7%)

## First unmet

2.6 — **Dealing Damage** (refused).

## Items

| id | title | verdict | anchor |
|---|---|---|---|
| 1 | Building for the Web | inapplicable | reason:the tutorial's own bracket-lib and wasm-pack build toolchain, not a world capability |
| 2.1 | Entities and Components | present | test:hornvale-kernel::commit_and_query_roundtrip |
| 2.2 | Walking A Map | present | test:hornvale-vessel::go_moves_and_back_retraces |
| 2.3 | A More Interesting Map | present | test:hornvale-terrain::every_default_globe_satisfies_every_invariant |
| 2.4 | Field of View | present | test:hornvale-vessel::a_wall_blocks_what_lies_behind_it |
| 2.5 | Monsters | present | test:hornvale-vessel::derive_npcs_are_distinct_and_placed |
| 2.6 | Dealing Damage | refused | decision:0070 |
| 2.7 | User Interface | refused | decision:0022 |
| 2.8 | Items and Inventory | deferred | registry:MAT-object-genus |
| 2.9 | Ranged Scrolls/Targeting | refused | decision:0070 |
| 2.10 | Saving and Loading | present | test:hornvale-kernel::saved_world_reloads_identically |
| 2.11 | Delving Deeper | present | test:hornvale-vessel::delve_has_three_distinguishable_outcomes |
| 2.12 | Difficulty | deferred | registry:PLAY-social-density-is-difficulty |
| 2.13 | Equipment | deferred | registry:MAT-object-genus |
| 3.1 | Nice Walls with Bitsets | refused | decision:0022 |
| 3.2 | Bloodstains | refused | decision:0022 |
| 3.3 | Particle Effects | refused | decision:0022 |
| 3.4 | Hunger Clock | present | test:hornvale-vessel::hunger_folds_eaten_and_resets_on_a_meal |
| 3.5 | Magic Mapping | deferred | registry:PLAY-lit-is-known |
| 3.6 | REX Paint Menu | inapplicable | reason:REX Paint is a bracket-lib asset format and this is the tutorial's own main-menu art |
| 3.7 | Simple Traps | absent |  |
| 4.1 | Refactor Map Building | inapplicable | reason:a refactor of the tutorial's own code organisation into a builder interface |
| 4.2 | Map Building Test Harness | present | test:hornvale-lab::depth_scoped_metrics_match_full_build |
| 4.3 | BSP Room Dungeons | present | test:hornvale-vessel::rule_1_the_realized_graph_is_exactly_the_specified_one |
| 4.4 | BSP Interior Design | present | test:hornvale-vessel::every_anchor_is_placed_exactly_once |
| 4.5 | Cellular Automata Maps | deferred | registry:CLIENT-derived-builders |
| 4.6 | Drunkard's Walk Maps | deferred | registry:CLIENT-derived-builders |
| 4.7 | Mazes and Labyrinths | deferred | registry:CLIENT-derived-builders |
| 4.8 | Diffusion-limited aggregation maps | deferred | registry:CLIENT-derived-builders |
| 4.9 | Add symmetry and brushes to the library | deferred | registry:CLIENT-derived-builders |
| 4.10 | Voronoi Hive Maps | deferred | registry:CLIENT-derived-builders |
| 4.11 | Wave Function Collapse | deferred | registry:CLIENT-derived-builders |
| 4.12 | Prefabs & Sectionals | absent |  |
| 4.13 | Room Vaults | absent |  |
| 4.14 | Layering/Builder Chaining | absent |  |
| 4.15 | Fun With Layers | absent |  |
| 4.16 | Room Builders | present | test:hornvale-vessel::rule_8_every_floor_cell_is_reachable_from_the_threshold |
| 4.17 | Better Corridors | absent |  |
| 4.18 | Doors | present | test:hornvale-vessel::rule_3_the_plan_is_enclosed_and_every_threshold_is_declared |
| 4.19 | Decouple map size from screen size | refused | decision:0022 |
| 4.20 | Section 3 Conclusion | inapplicable | reason:a section summary carrying no capability claim |
| 5.1 | Design Document | inapplicable | reason:the tutorial's own game-design document — a process artifact, not a program capability |
| 5.2 | Raw Files, Data-Driven Design | absent |  |
| 5.3 | Data-Driven Spawn Tables | present | test:hornvale-worldgen::the_wild_never_mints_a_sea_creature |
| 5.4 | Making the town | present | test:hornvale-worldgen::emergent_settlement_count_stays_in_the_sane_band |
| 5.5 | Populating the town | present | test:hornvale-vessel::derive_npcs_actually_includes_the_home_settlement |
| 5.6 | Living bystanders | present | test:hornvale-vessel::a_thirsty_agent_plans_to_water_and_the_tick_walks_it |
| 5.7 | Game Stats | present | test:hornvale-species::potency_is_challenge_rating_over_thirty |
| 5.8 | Equipment | deferred | registry:MAT-object-genus |
| 5.9 | User Interface | refused | decision:0022 |
| 5.10 | Into the Woods! | present | test:hornvale-climate::whittaker_hits_known_corners |
| 5.11 | XP | deferred | registry:PLAY-depth-accrues-on-events |
| 5.12 | Backtracking | present | test:hornvale-worldgen::an_addresss_meaning_does_not_depend_on_which_other_chambers_exist |
| 5.13 | Into the caverns | present | test:hornvale-worldgen::a_cave_mouth_reaches_at_least_one_chamber |
| 5.14 | Better AI | present | test:hornvale-vessel::decide_plans_to_water_when_thirsty_and_home_when_not |
| 5.15 | Spatial Indexing Revisited | present | test:hornvale-vessel::rule_5_a_cell_holds_at_most_one_creature |
| 5.16 | Item Stats and Vendors | deferred | registry:MAT-object-genus |
| 5.17 | Deep caverns | present | test:hornvale::every_cave_kind_has_exactly_one_cave_formation |
| 5.18 | Cavern to Dwarf Fort | deferred | registry:MAP-69 |
| 5.19 | Town Portals | absent |  |
| 5.20 | Magic Items | deferred | registry:MEM-8 |
| 5.21 | Effects | present | test:hornvale-kernel::tick_reads_frozen_snapshot_so_systems_do_not_see_each_others_writes |
| 5.22 | Cursed Items | deferred | registry:MAT-object-genus |
| 5.23 | Even More Items | deferred | registry:MAT-object-genus |
| 5.24 | Magic Spells | deferred | registry:MAP-thaumeme-layers |
| 5.25 | Enter the Dragon | refused | decision:0070 |
| 5.26 | Mushrooms | present | test:hornvale-species::the_dark_trait_combinations_are_named |
| 5.27 | More Shrooms | absent |  |
| 5.28 | Ranged Combat | refused | decision:0070 |
| 5.29 | Logging | refused | decision:0022 |
| 5.30 | Text Layers | refused | decision:0022 |
| 5.31 | Systems/Dispatch | present | test:hornvale-kernel::schedule_orders_a_chain |
| 5.32 | Dark Elf City 1 | deferred | registry:MAP-69 |
| 5.33 | Dark Elf Plaza | deferred | registry:MAP-69 |
