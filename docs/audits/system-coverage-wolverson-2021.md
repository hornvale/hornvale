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

- present: 26 (35%)
- refused: 12 (16%)
- deferred: 21 (28%)
- absent: 10 (14%)
- inapplicable: 5 (7%)

## First unmet

2.6 — **Dealing Damage** (refused).

## Items

| id | title | verdict | anchor | note |
|---|---|---|---|---|
| 1 | Building for the Web | inapplicable | reason:the tutorial's own bracket-lib and wasm-pack build toolchain, not a world capability | front matter by construction (spec §8): included rather than excluded so the reason travels with it |
| 2.1 | Entities and Components | present | test:hornvale-kernel::commit_and_query_roundtrip | an entity is a ledger subject and its components are the facts committed about it, contradiction-checked against the concept registry; kernel/src/component.rs additionally carries a typed ComponentStore keyed by identity. The chapter's other half — drawing an @ — is decision 0022's, not the sim's |
| 2.2 | Walking A Map | present | test:hornvale-vessel::go_moves_and_back_retraces | `go <dir>` walks the locale mesh and `back` retraces; indoors a wall refuses with a physical reason (the_blocking.rs::a_wall_refuses_with_a_physical_reason) |
| 2.3 | A More Interesting Map | present | test:hornvale-terrain::every_default_globe_satisfies_every_invariant | the map is a sculpted planet rather than a rectangle of rooms and corridors, generated as a pure function of seed and pins and asserted against invariants across a seed sweep |
| 2.4 | Field of View | present | test:hornvale-vessel::a_wall_blocks_what_lies_behind_it | windows/vessel/src/lattice/sight.rs is Ford's SYMMETRIC recursive shadowcasting over four quadrants (sight_is_symmetric is a property test over every ordered floor pair); the walk band carries the remembered-vs-seen half through purview.rs, and The Sighting gates the `sensed` channel on it |
| 2.5 | Monsters | present | test:hornvale-vessel::derive_npcs_are_distinct_and_placed | creatures are derived from the world's own demography and placed, and a creature beyond sight appears in neither the sensed channel nor the marks. Matches THIS chapter's monsters, which only notice; they do not chase (spec §12) — and Hornvale's move only inside `wait` |
| 2.6 | Dealing Damage | refused | decision:0070 | no stored, mutable health value may exist anywhere; wounds commit and vitality folds. 0070 closes by ordering combat explicitly after it — combat built first would invent a counter. Not a gap |
| 2.7 | User Interface | refused | decision:0022 | sidebar, health bar, message log and mouse tooltips are drawn from inside the engine; 0022 is the inverse — the sim emits data and clients render. In-repo clients do carry panes, outside the workspace and outside determinism |
| 2.8 | Items and Inventory | deferred | registry:MAT-object-genus | there is no concept for `object` anywhere in the registry — sword → weapon → tool → artifact → object has no member — so nothing material can be picked up, carried or dropped until the genus exists |
| 2.9 | Ranged Scrolls/Targeting | refused | decision:0070 | the chapter's payload is damage at range (magic missile, fireball); 0070 forbids the counter it removes. The scroll half additionally waits on MAT-object-genus, and the targeting UI on 0022 — but the ratified refusal is the load-bearing one |
| 2.10 | Saving and Loading | present | test:hornvale-kernel::saved_world_reloads_identically | a world is a seed plus a ledger serialized to JSON; reload re-derives everything else, and entity ids are never reused across a reload. Decision 0007 makes the save tiny by construction |
| 2.11 | Delving Deeper | present | test:hornvale-vessel::delve_has_three_distinguishable_outcomes | `delve`/`climb` descend into and return from the cave at a cell, with a third outcome for sealed rock; `dive`/`surface` do the same through the water column. Descent is into a derived place, never a regenerated level |
| 2.12 | Difficulty | deferred | registry:PLAY-social-density-is-difficulty | ARGUABLE. Nothing scales challenge to progress today, and the design argues against a power curve (PLAY-depth-is-spent-not-scored: there is no level). The registry's one difficulty row plans a DERIVED axis — connectivity of the local social graph — which is a different mechanism from depth-weighted spawn tables, not the same one unbuilt |
| 2.13 | Equipment | deferred | registry:MAT-object-genus | same blocker as 2.8; CLIENT-coverage-matrix additionally names body-derived equipment slots (morphology decides slots) as the intended shape |
| 3.1 | Nice Walls with Bitsets | refused | decision:0022 | choosing a wall glyph from a neighbour bitmask is glyph selection — a renderer's job under 0022 |
| 3.2 | Bloodstains | refused | decision:0022 | rendering residue, named as such in the campaign spec §3; MAP-23 already licenses client-side residue. The blood itself would also need 0070's refused damage model |
| 3.3 | Particle Effects | refused | decision:0022 | transient visual effects drawn from inside the engine; CLIENT-set-pieces holds the client-side roster, free under decision 0055 |
| 3.4 | Hunger Clock | present | test:hornvale-vessel::hunger_folds_eaten_and_resets_on_a_meal | hunger is a fold over committed `eaten` facts rather than a stored counter, integrates faster over a hot occupancy, and drives behaviour through the homeostatic layer. The counter this chapter adds is exactly what the fold replaces |
| 3.5 | Magic Mapping | deferred | registry:PLAY-lit-is-known | the chart today shows only what the possession has walked; knowledge-lit map is designed and unbuilt, and PLAY-inheritance-blocker names the single line (Knowledge::default()) that keeps a vessel from knowing anything it did not see |
| 3.6 | REX Paint Menu | inapplicable | reason:REX Paint is a bracket-lib asset format and this is the tutorial's own main-menu art | toolchain and scaffolding, not a world capability |
| 3.7 | Simple Traps | absent |  | ARGUABLE. Hazards exist and creatures route around them (danger_routes_a_thirsty_creature_around_a_hazard_to_water), but nothing in Hornvale is CONCEALED, nothing triggers on entry, and no row plans a detection mechanic. Calling this `refused` under 0070 would credit the project with a position on concealment it does not hold |
| 4.1 | Refactor Map Building | inapplicable | reason:a refactor of the tutorial's own code organisation into a builder interface | scaffolding: the chapter adds no world capability, only the seam the following chapters plug into |
| 4.2 | Map Building Test Harness | present | test:hornvale-worldgen::full_depth_produces_both | ARGUABLE. The generator is invocable standalone at a declared BuildDepth and hands its artifacts (terrain, climate) back for inspection with no session anywhere — which is the run half the tutorial's harness exists for. The sweep half is windows/lab (MAP-1, shipped), but its end-to-end run tests are `heavy:`-tiered and #[ignore]d, so citing one would have anchored to a test the gate does not run; the first anchor here, depth_scoped_metrics_match_full_build, evidenced depth-ladder correctness instead. The tutorial's harness also ANIMATES generation inside the game window, which is the half 0022 puts on the client |
| 4.3 | BSP Room Dungeons | present | test:hornvale-vessel::rule_1_the_realized_graph_is_exactly_the_specified_one | windows/vessel/src/lattice/allocate.rs is binary space partition run INVERSELY, and says so against this chapter by name: Wolverson splits a rectangle to invent rooms, Hornvale splits one to allocate space among chambers that already exist, so adjacency is realized by construction |
| 4.4 | BSP Interior Design | present | test:hornvale-vessel::every_anchor_is_placed_exactly_once | a chamber's interior features are derived as an anchor graph and embedded into its cells, faithfully (every_placement_is_faithful) and deterministically — The Hearth's rung 1 of CLIENT-furnishing-ladder |
| 4.5 | Cellular Automata Maps | deferred | registry:CLIENT-derived-builders | organic cave chambers DO exist (lattice/grow.rs, Method::Grown) but by region growing with a separation rule, not by cellular automata. The row names CA explicitly as an admissible derivation nobody has built |
| 4.6 | Drunkard's Walk Maps | deferred | registry:CLIENT-derived-builders | a builder that is a deterministic function of pins plus noise is admissible; this one is unbuilt |
| 4.7 | Mazes and Labyrinths | deferred | registry:CLIENT-derived-builders | no maze generator exists anywhere in the tree |
| 4.8 | Diffusion-limited aggregation maps | deferred | registry:CLIENT-derived-builders | unbuilt; the row keeps the technique available |
| 4.9 | Add symmetry and brushes to the library | deferred | registry:CLIENT-derived-builders | there is no builder LIBRARY to add primitives to — the two embedders are selected by brief, not composed from brushes |
| 4.10 | Voronoi Hive Maps | deferred | registry:CLIENT-derived-builders | weighted Voronoi IS already used, in domains/terrain/src/plates.rs, to partition the sphere into plates whose boundaries become mountains and rifts — but as a tectonic model, never as a builder for walked space. Deferred rather than present for exactly that reason |
| 4.11 | Wave Function Collapse | deferred | registry:CLIENT-derived-builders | the row names WFC explicitly as admissible-as-derivation |
| 4.12 | Prefabs & Sectionals | absent |  | stamping authored map sections is the one thing CLIENT-derived-builders says stays refused — but that refusal lives in a registry row, not a ratified decision, so this cannot be `refused`. Nothing plans it either |
| 4.13 | Room Vaults | absent |  | same as 4.12. The furnishing patterns The Hearth shipped are RELATIONS (attach/requires), deliberately not stamped geometry (CLIENT-language-not-catalogue) |
| 4.14 | Layering/Builder Chaining | absent |  | CLIENT-derived-builders names the chain of builders expressing designer intent as the thing that stays refused, and no decision ratifies that; a chain of derivations does not exist either |
| 4.15 | Fun With Layers | absent |  | applies 4.14's chain; same hole |
| 4.16 | Room Builders | present | test:hornvale-vessel::rule_8_every_floor_cell_is_reachable_from_the_threshold | there are two room builders — Method::Rectilinear (BSP) and Method::Grown (region growing) — chosen by the place's own brief, and the checked rules hold across both |
| 4.17 | Better Corridors | absent |  | there are no corridors to improve: chambers are separated by exactly one wall cell carrying a threshold, so connection is adjacency rather than a carved passage |
| 4.18 | Doors | present | test:hornvale-vessel::rule_3_the_plan_is_enclosed_and_every_threshold_is_declared | a doorway derives from a link in the anchor graph, every plan is enclosed and every threshold declared, and `enter`/`out` cross one. A door has no OPEN/CLOSED state and blocks no sight — CLIENT-breach-and-rubble names that gap |
| 4.19 | Decouple map size from screen size | refused | decision:0022 | a camera and a viewport are the client's, not the sim's. `map out N` is a sim-side ZOOM over a purview, which is a different thing (and CLIENT-snapshot-chart-cannot-zoom records that the structured channel does not carry it) |
| 4.20 | Section 3 Conclusion | inapplicable | reason:a section summary carrying no capability claim | included rather than excluded so the reason is visible |
| 5.1 | Design Document | inapplicable | reason:the tutorial's own game-design document — a process artifact, not a program capability | Hornvale's equivalents (the Constitution, the spec, the decision log) are process artifacts too, and scoring them here would measure the wrong subject |
| 5.2 | Raw Files, Data-Driven Design | absent |  | nothing world-facing is loaded from a data file at runtime. Studies and corpora ARE data (decision 0011), but that is the instrument, not the world; the species roster is a Rust table, and everything else is derived from the seed |
| 5.3 | Data-Driven Spawn Tables | present | test:hornvale-worldgen::the_wild_never_mints_a_sea_creature | ARGUABLE — the DATA-DRIVEN half is exactly why 5.2 is `absent`. What appears at a place is derived from carrying capacity, niche and habitat realm rather than drawn from a weighted table; MAP-encounter-is-a-product plans the refinement that retires wandering-monster tables entirely |
| 5.4 | Making the town | present | test:hornvale-worldgen::emergent_settlement_count_stays_in_the_sane_band | settlements are placed by committed history over a demography fit, never authored; buildings within them carry chambers a possession can enter |
| 5.5 | Populating the town | present | test:hornvale-vessel::derive_npcs_actually_includes_the_home_settlement | the roster of who is in a settlement is derived from its population and species mix; `npcs`, `why` and `needs` read them back |
| 5.6 | Living bystanders | present | test:hornvale-vessel::a_thirsty_agent_plans_to_water_and_the_tick_walks_it | bystanders act on homeostatic drives (thirst, hunger, danger) and their movement is observed and recountable — but ONLY inside `wait`: step_with_occupancy has exactly one call site, in Session::wait, so nothing moves while the player walks |
| 5.7 | Game Stats | present | test:hornvale-species::potency_is_challenge_rating_over_thirty | creatures carry numeric attributes — potency (decision 0064), MindVector's threat_response / deliberation_latency / time_horizon, PerceptionVector, SocietyVector — derived from biology rather than rolled. Two caveats: they are per-SPECIES (PSY-individual-deviation names the missing individual layer), and derived HP is refused by 0070 |
| 5.8 | Equipment | deferred | registry:MAT-object-genus | the section-4 expansion of 2.13; same blocker |
| 5.9 | User Interface | refused | decision:0022 | the second UI pass, same refusal as 2.7 |
| 5.10 | Into the Woods! | present | test:hornvale-climate::whittaker_hits_known_corners | wilderness regions are classified into twenty-two biomes on a Whittaker diagram over derived temperature and moisture, and species bind to them by affinity — a forest is a place the world produces, not a level builder |
| 5.11 | XP | deferred | registry:PLAY-depth-accrues-on-events | progression is designed and unbuilt: depth accrues on events rather than duration, is spent rather than scored, and PLAY-depth-is-spent-not-scored is explicit that there is no level |
| 5.12 | Backtracking | present | test:hornvale-worldgen::an_addresss_meaning_does_not_depend_on_which_other_chambers_exist | there is nothing to persist: a place is a pure function of its address, so returning to it re-derives the same place. The chapter's problem — levels regenerated on re-entry — cannot arise |
| 5.13 | Into the caverns | present | test:hornvale-worldgen::a_cave_mouth_reaches_at_least_one_chamber | The Deep Realm: caves are derived under land cells, every passage is traversable in both directions, and `delve` reaches them from the walk band |
| 5.14 | Better AI | present | test:hornvale-vessel::decide_plans_to_water_when_thirsty_and_home_when_not | ARGUABLE. Behaviour selection is a planner over drives, with foresight, hazard avoidance and give-up conditions — richer than this chapter's mode switch, but NOT player-directed: nothing chases, nothing flees, and creatures act only inside `wait`. The player-facing half is one committed `turned-hostile` fact at a grievance threshold |
| 5.15 | Spatial Indexing Revisited | present | test:hornvale-vessel::rule_5_a_cell_holds_at_most_one_creature | Occupancy is a per-cell index of who stands where, bubble-only and never serialized (decision 0069); the kernel additionally carries SPO/PSO/OSP permutation indexes over the fact ledger |
| 5.16 | Item Stats and Vendors | deferred | registry:MAT-object-genus | objects are the prior blocker; the exchange half is SOC-1's exchange axis and TECH-2's coinage, both unbuilt |
| 5.17 | Deep caverns | present | test:hornvale-terrain::a_strong_process_reaches_one_band_deeper | a cave's depth is a derived BandKind — Regolith / Cover / Roots / Basement — read out of the stratigraphic column by `cave_depth`, so a deeper cavern sits in different rock rather than being the same generator re-run. The first anchor here cited every_cave_kind_has_exactly_one_cave_formation, which is a decision-0094 duplicate-roster check over three enum variants and evidenced nothing about depth. MAP-underworld-chart records that the underworld still has no chart in its own terms |
| 5.18 | Cavern to Dwarf Fort | deferred | registry:MAP-69 | no settlement is ever placed below the surface: occupation sits on the walk band, and subterranean species get cave carrying capacity without founding anything there. MAP-69 makes surface-versus-underground its own future campaign, dwarven over/under commerce included |
| 5.19 | Town Portals | absent |  | no fast travel of any kind. CLIENT-coverage-matrix records this as a CLASH rather than a gap — a portal presumes the enter/exit scale seam, which UNI-37 holds deliberately shut — but nothing ratifies that, so it is not `refused` |
| 5.20 | Magic Items | deferred | registry:MEM-8 | the artifact channel — knowledge carried in an object, whose failure mode is that the instance survives while the recipe does not. Identification is planned as inference (UNI-1), not as a boolean flag |
| 5.21 | Effects | present | test:hornvale-kernel::tick_reads_frozen_snapshot_so_systems_do_not_see_each_others_writes | ARGUABLE. The architectural capability — a generic, decoupled channel by which one system's action reaches any target, applied after a frozen read — is decision 0003's trace protocol plus the kernel tick. The chapter's actual payload (damage, healing, confusion) is refused or absent, so this is a verdict about the substrate, not the effects |
| 5.22 | Cursed Items | deferred | registry:MAT-object-genus | objects first; the cursed/blessed axis would also want UNI-1's inference-over-a-hidden-ruleset rather than a flag |
| 5.23 | Even More Items | deferred | registry:MAT-object-genus | more of what does not exist yet; MAP-19's commonsense ruleset (containers, supporters, parts) is the object-scale model these would sit in |
| 5.24 | Magic Spells | deferred | registry:MAP-thaumeme-layers | magic is designed at length and unbuilt — productions discovered, accords invented — and MAP-spell-corpus is explicit that a published spell list would calibrate parameters, never audit coverage |
| 5.25 | Enter the Dragon | refused | decision:0070 | dragons exist as species and carry a potency (the might order is red, black, white, treant, xorn) — what is refused is the boss FIGHT, which is combat, and 0070 orders combat after a vitality model that does not exist |
| 5.26 | Mushrooms | absent |  | the chapter's subject is a themed fungal LEVEL — a map generator plus the creatures that belong to it — and Hornvale has no per-level content authoring at all, which is the same reason 5.27 is absent. The roster does carry a sessile detritivore (the shrieker) in a subterranean realm, but the test that would have been cited for it asserts a roster-VACANCY table under decision 0094, which is species coverage and not fungal-level generation: right ingredient, wrong subject, exactly as at 4.10 |
| 5.27 | More Shrooms | absent |  | a second content pass of fungal variants and their effects. Hornvale has no per-level content authoring at all, and the effects half is 5.21's payload |
| 5.28 | Ranged Combat | refused | decision:0070 | combat at range is still combat; 0070 governs |
| 5.29 | Logging | refused | decision:0022 | ARGUABLE. What this chapter adds is a log-building API with coloured fragments, which is rendering. The RECORD half genuinely exists — the ledger is append-only and `why <who>` recounts an NPC's dated history — but a session-facing coloured message log is the client's |
| 5.30 | Text Layers | refused | decision:0022 | console layers are rendering, named as such in the campaign spec §3 |
| 5.31 | Systems/Dispatch | present | test:hornvale-kernel::schedule_orders_a_chain | a System declares the predicates it reads and writes and the schedule is the topological order of that data-dependency DAG, tie-broken by stable label, with a single-writer check over functional predicates — derived, never serialized |
| 5.32 | Dark Elf City 1 | deferred | registry:MAP-69 | the drow exist as a subterranean people and the possession design repeatedly assumes a Drow City to be admitted to (PLAY-two-gates), but nothing generates a settlement below ground. MAP-69 is where that campaign is banked |
| 5.33 | Dark Elf Plaza | deferred | registry:MAP-69 | a district-scale set piece inside 5.32's city. The district composer exists one band coarser on the surface (CLIENT-district-patterns), so what is missing is the underground settlement to compose within |
