# The Seam (ECS Campaign 1) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Split `SpeciesDef` into a universal `BiosphereTraits` component plus an `Option<PeopledTraits>`, author the canonical-5E menagerie as biosphere-only kinds, and cut genesis over from the flat carrying path onto The Niche's differentiated niche-K.

**Architecture:** A strangler-fig seam. Stages 1–3 are a pure, byte-identical refactor (traits move, nothing computes differently); stage 4 (menagerie) adds content; stage 5 (cutover) is the single intended-drift step, staged last behind the equivalence shadow. The packer and habitat layer read only `.biosphere`; settlement genesis, phonology, and perception are gated on `peopled.is_some()`.

**Tech Stack:** Rust (edition 2024), `serde`/`serde_json` only, kernel `Seed`/`Stream`, `cargo nextest`. No new crates.

## Global Constraints

- **Determinism:** same seed + pins → byte-identical worlds and artifacts. Every existing peopled world MUST be byte-identical across stages 1–3. Stage 5 drift is intended and lands with the shared AWS census regen (never regenerate census locally).
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only (clippy-enforced). Float sorting uses `total_cmp`.
- **`#![warn(missing_docs)]`** — every new public item, field, and variant gets a one-line doc comment.
- **Typed quantities** at `pub` boundaries carry a `type-audit:` verdict tag; run the type audit before closing (`cargo run --manifest-path tools/type-audit/Cargo.toml -- check`).
- **`cargo fmt`** as the final step before every commit. The commit gate is `make gate`.
- **No new dependencies** (allowlist `[libm, serde, serde_json]`, enforced by `cli/tests/architecture.rs`).
- **Menagerie roster is signed off** (spec §5 / decision-ledger #26): treant, twig blight, giant elk, woolly mammoth, giant goat, otyugh, xorn, rust monster, white/red/black chromatic dragons, owlbear. Author biosphere-only, 5E-grounded.

---

## File Structure

- `domains/species/src/lib.rs` — `SpeciesDef` split into `BiosphereTraits` + `Option<PeopledTraits>`; the 4 peoples re-expressed; the 12 menagerie kinds added (all `peopled: None`).
- `windows/worldgen/src/lib.rs` — biosphere access sites rewritten to `.biosphere.*`; peopled-path consumers guarded on `peopled.is_some()`; the genesis cutover onto niche-K (the prepared `a15a` change).
- `windows/lab/src/roster.rs`, `windows/lab/src/metrics.rs` — access sites updated to the split shape.
- `book/src/chronicle/the-seam.md` — chronicle entry (stage 5).
- `docs/retrospectives/…-the-seam.md` — retrospective (stage 5).
- `book/src/frontier/idea-registry.md` — UNI-22 `raw` → `elaborated` (stage 5).

---

## Task 1: Split `SpeciesDef` into `BiosphereTraits` + `Option<PeopledTraits>`

**Files:**
- Modify: `domains/species/src/lib.rs:361-560` (struct + the 4 registry literals)
- Test: `domains/species/src/lib.rs` (unit test module)

**Interfaces:**
- Produces: `pub struct BiosphereTraits { pub mass: Mass, pub metabolic_class: MetabolicClass, pub niche: ResourceVector, pub condition_niche: ConditionNiche, pub potency: f64 }`; `pub struct PeopledTraits { pub noun, pub psych, pub perception, pub articulation, pub worker_override, pub warrior, pub artisan, pub shaman, pub top }`; `SpeciesDef { pub name, pub family, pub biosphere: BiosphereTraits, pub peopled: Option<PeopledTraits> }`.

- [ ] **Step 1: Write the failing test** — assert the split preserves every peoples' biosphere values and that all four are `peopled.is_some()`.

```rust
#[test]
fn split_preserves_biosphere_and_peopled_presence() {
    let reg = registry();
    let goblin = &reg["goblin"];
    // biosphere moved intact
    assert_eq!(goblin.biosphere.mass, Mass::new(18.1).unwrap());
    assert_eq!(goblin.biosphere.potency, 0.0);
    // the four peoples all speak/settle
    for name in ["goblin", "kobold", "hobgoblin", "bugbear"] {
        assert!(reg[name].peopled.is_some(), "{name} must carry PeopledTraits");
    }
}
```

- [ ] **Step 2: Run it, verify it fails to compile** — `cargo test -p hornvale-species split_preserves_biosphere_and_peopled_presence`. Expected: compile error (`biosphere`/`peopled` fields do not exist yet).

- [ ] **Step 3: Define the two component structs** (above `SpeciesDef`), each field doc-commented, moving the docs from the current `SpeciesDef` fields verbatim.

```rust
/// The biosphere component: every entity has one. The packer and the
/// habitat/niche-K layer read only these traits.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Debug, PartialEq)]
pub struct BiosphereTraits {
    /// Adult individual body mass — the coexistence packer's biomass input.
    pub mass: Mass,
    /// Metabolic strategy — drives life-history allometry (spec BIO-2).
    pub metabolic_class: MetabolicClass,
    /// The species' ecological niche over the resource-axis basis.
    pub niche: ResourceVector,
    /// The species' condition-tolerance profile over the v1 environmental axes.
    pub condition_niche: ConditionNiche,
    /// Magical potency (0 = purely material). Raises the sovereignty floor.
    /// type-audit: bare-ok(ratio: potency)
    pub potency: f64,
}

/// The peopled component: present only for settling, speaking peoples.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Debug, PartialEq)]
pub struct PeopledTraits {
    /// The settlement noun ("village", "warren").
    pub noun: &'static str,
    /// The psychology vector.
    pub psych: PsychVector,
    /// The perception vector.
    pub perception: PerceptionVector,
    /// The articulation vector.
    pub articulation: ArticulationVector,
    /// Worker-role override; `None` = the subsistence worker word.
    pub worker_override: Option<&'static str>,
    /// The warrior-rung word.
    pub warrior: &'static str,
    /// The artisan-rung word.
    pub artisan: &'static str,
    /// The shaman-rung word.
    pub shaman: &'static str,
    /// The top-rung word ("chief", "elders").
    pub top: &'static str,
}
```

- [ ] **Step 4: Reshape `SpeciesDef`** to hold `name`, `family`, `biosphere: BiosphereTraits`, `peopled: Option<PeopledTraits>` (keep the struct's doc comment; drop the moved fields).

- [ ] **Step 5: Rewrite the four registry literals** (lines ~424/464/504/544) into the nested form, e.g.:

```rust
reg.insert("goblin", SpeciesDef {
    name: "goblin",
    family: "goblinoid",
    biosphere: BiosphereTraits {
        mass: /* existing value */,
        metabolic_class: /* existing */,
        niche: /* existing */,
        condition_niche: /* existing */,
        potency: 0.0,
    },
    peopled: Some(PeopledTraits {
        noun: "village",
        psych: /* existing */,
        perception: /* existing */,
        articulation: /* existing */,
        worker_override: /* existing */,
        warrior: /* existing */, artisan: /* existing */,
        shaman: /* existing */, top: /* existing */,
    }),
});
```

- [ ] **Step 6: Run the test** — `cargo test -p hornvale-species split_preserves_biosphere_and_peopled_presence`. Expected: PASS. (Downstream crates will not build yet — that is Task 2.)

- [ ] **Step 7: Commit**

```bash
git add domains/species/src/lib.rs
git commit -m "refactor(species): split SpeciesDef into BiosphereTraits + Option<PeopledTraits>"
```

---

## Task 2: Migrate biosphere access sites to `.biosphere.*`

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (the `def.mass`/`def.niche`/`def.condition_niche`/`def.metabolic_class`/`def.potency` sites — ~20), `windows/lab/src/metrics.rs`, `windows/lab/src/roster.rs`
- Test: workspace build + existing byte-identity tests

**Interfaces:**
- Consumes: `BiosphereTraits` (Task 1).

- [ ] **Step 1: Build to enumerate the breakage** — `cargo build --workspace 2>&1 | grep "no field"`. Expected: errors at each biosphere access site (`no field 'mass' on type '&SpeciesDef'`, etc.).

- [ ] **Step 2: Rewrite each site** `def.mass` → `def.biosphere.mass`, `def.niche` → `def.biosphere.niche`, `.condition_niche` → `.biosphere.condition_niche`, `.metabolic_class` → `.biosphere.metabolic_class`, `.potency` → `.biosphere.potency`. In `windows/worldgen/src/lib.rs` the enumerate/packer sites (~504, 520, 622, and the niche-K layer) read `def.biosphere.mass`/`.niche`/`.condition_niche`/`.potency`.

- [ ] **Step 3: Build clean** — `cargo build --workspace`. Expected: success (peopled sites still compile; they are Task 3).

- [ ] **Step 4: Run the existing byte-identity / worldgen tests** — `cargo nextest run -p hornvale-worldgen -p hornvale-species`. Expected: PASS unchanged (a pure field move).

- [ ] **Step 5: Commit**

```bash
git add windows/worldgen/src/lib.rs windows/lab/src/metrics.rs windows/lab/src/roster.rs
git commit -m "refactor(worldgen,lab): read biosphere traits via .biosphere"
```

---

## Task 3: Guard peopled-path consumers on `peopled.is_some()`

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (settlement genesis, phonology `draw_phonology`, perception `perception_lens`/`pack_depths`, `species_carrying_input`), `windows/lab/src/roster.rs` (`..goblin.articulation`), `windows/lab/src/metrics.rs`
- Test: `windows/worldgen/src/lib.rs` test module

**Interfaces:**
- Consumes: `PeopledTraits`, `SpeciesDef.peopled` (Task 1).
- Produces: a helper `fn peopled(def: &SpeciesDef) -> &PeopledTraits` for peopled-only passes that panics with a clear message if called on a fauna kind, plus `peopled.is_some()` filters at each pass boundary.

- [ ] **Step 1: Write the failing test** — a fauna kind (added minimally here as a fixture, or asserted once Task 4 lands) is skipped by settlement genesis. Interim assertion using a constructed biosphere-only def:

```rust
#[test]
fn fauna_are_skipped_by_settlement_genesis() {
    // A biosphere-only kind carries no PeopledTraits.
    let fauna = SpeciesDef {
        name: "test-beast", family: "test-beast",
        biosphere: BiosphereTraits { /* neutral values */ },
        peopled: None,
    };
    assert!(fauna.peopled.is_none());
    // settlement_species(&[fauna]) yields nothing to settle.
    assert!(settlement_species(&[fauna]).next().is_none());
}
```

- [ ] **Step 2: Run it, verify it fails** — `cargo test -p hornvale-worldgen fauna_are_skipped_by_settlement_genesis`. Expected: FAIL (`settlement_species` not defined / peopled sites still access `def.psych` unconditionally).

- [ ] **Step 3: Introduce the pass-boundary filter.** Where the settlement/phonology/perception passes iterate the species set, filter to peopled kinds: `species_set.iter().filter(|d| d.peopled.is_some())`. Inside each such pass, read `def.peopled.as_ref().expect("peopled pass over a fauna kind")` once and use its fields (`.psych`, `.articulation`, `.noun`, `.warrior`, …). The `family_registry` proto-vector lookup stays inside these guarded passes.

- [ ] **Step 4: Retire `species_carrying_input(psych)` from the fauna path.** It only ever runs for settling species; leave it peopled-gated for now (the cutover in Task 5 removes it from genesis entirely). Confirm no fauna reaches it.

- [ ] **Step 5: Run the test + the byte-identity suite** — `cargo nextest run -p hornvale-worldgen`. Expected: PASS; existing seed-42 peopled worlds still byte-identical.

- [ ] **Step 6: Commit**

```bash
git add windows/worldgen/src/lib.rs windows/lab/src/roster.rs windows/lab/src/metrics.rs
git commit -m "refactor(worldgen): gate settlement/phonology/perception on peopled.is_some()"
```

---

## Task 4: Author the menagerie (biosphere-only)

**Files:**
- Modify: `domains/species/src/lib.rs` (`registry()` — add the 12 kinds; `family_registry()` — add draconic + plant families)
- Test: `domains/species/src/lib.rs`

**Interfaces:**
- Consumes: `BiosphereTraits`, `MetabolicClass`, `ResourceVector`, `ConditionNiche` (existing).

**Authoring method (spec §5; goblinoid precedent, decision-ledger #2/#19):** for each creature, `mass` from D&D 5E canon (kg), `niche` a `ResourceVector` over `{photosynthate, plant-forage, animal-prey, detritus, mineral}` from its 5E ecology, `condition_niche` (temp/moisture/insolation/elevation optima+width, devotion) placed against the C1-measured seed-42 land ranges, `metabolic_class` per clade, `potency` > 0 for the mighty. All `peopled: None`. Roster and axis assignments are fixed by #26:

```
kind          | resource axis     | mass(kg) | class      | potency | climate tile
--------------+-------------------+----------+------------+---------+----------------
treant        | photosynthate     | ~1800    | Autotroph  | > 0     | temperate forest
twig blight   | photosynthate     | ~5       | Autotroph  | 0       | temperate forest
giant elk     | plant-forage      | ~450     | Endotherm  | 0       | temperate
woolly mammoth| plant-forage      | ~6000    | Endotherm  | 0       | cold/tundra
giant goat    | plant-forage      | ~140     | Endotherm  | 0       | alpine/highland
otyugh        | detritus          | ~260     | Endotherm  | 0       | warm wet lowland
xorn          | mineral           | ~55      | Ametabolic | > 0     | subterranean/any
rust monster  | mineral           | ~90      | Ectotherm  | 0       | subterranean
white dragon  | animal-prey (apex)| ~2200    | Endotherm  | high    | cold
red dragon    | animal-prey (apex)| ~2700    | Endotherm  | highest | warm/volcanic
black dragon  | animal-prey (apex)| ~2200    | Endotherm  | high    | swamp/wet
owlbear       | animal-prey       | ~450     | Endotherm  | 0       | temperate forest
```

- [ ] **Step 1: Write the failing test** — the menagerie is present, biosphere-only, and spans the resource axes.

```rust
#[test]
fn menagerie_is_biosphere_only_and_spans_axes() {
    let reg = registry();
    for name in ["treant","twig-blight","giant-elk","woolly-mammoth","giant-goat",
                 "otyugh","xorn","rust-monster","white-dragon","red-dragon",
                 "black-dragon","owlbear"] {
        let d = &reg[name];
        assert!(d.peopled.is_none(), "{name} is fauna: no PeopledTraits");
        assert!(d.biosphere.mass > Mass::new(0.0).unwrap());
    }
    // mighty creatures carry potency
    assert!(reg["red-dragon"].biosphere.potency > 0.0);
    assert!(reg["treant"].biosphere.potency > 0.0);
    // resource niches are partitioned, not four omnivores: the distinct
    // dominant axis differs across creatures (assert via the packer's
    // niche read, e.g. Pianka overlap < 1.0 between treant and white-dragon).
    let overlap = niche_overlap(&reg["treant"].biosphere.niche,
                                &reg["white-dragon"].biosphere.niche);
    assert!(overlap < 0.5, "photosynthate vs apex niches must barely overlap");
}
```

(`niche_overlap` is the existing Pianka-overlap the packer uses; if it is not
`pub`, assert instead on the resource-axis basis constants the goblinoid
literals use — `PHOTOSYNTHATE`, `PLANT_FORAGE`, `ANIMAL_PREY`, `DETRITUS`,
`MINERAL` — via whatever read `ResourceVector` exposes.)

- [ ] **Step 2: Run it, verify it fails** — `cargo test -p hornvale-species menagerie_is_biosphere_only_and_spans_axes`. Expected: FAIL (kinds absent).

- [ ] **Step 3: Author the worked example (white dragon) as the template**, using the exact construction forms the goblinoid literals use (`Mass::new(kg).unwrap()`, `ResourceVector::new(&[(AXIS, weight), …]).unwrap()` over the basis constants, a per-species `*_condition_niche()` helper returning `ConditionNiche` of `ConditionResponse { optimum, width, devotion }`), grounded in 5E + the niche model:

```rust
/// White dragon: an obligate apex that owns the cold; mighty (buffers climate).
fn white_dragon_condition_niche() -> ConditionNiche {
    ConditionNiche {
        temperature: ConditionResponse { optimum: -20.0, width: 25.0, devotion: 0.9 },
        moisture:    ConditionResponse { optimum: 0.4,  width: 0.5,  devotion: 0.3 },
        insolation:  ConditionResponse { optimum: 0.05, width: 0.15, devotion: 0.2 },
        elevation:   ConditionResponse { optimum: 1500.0, width: 3000.0, devotion: 0.4 },
    }
}

reg.insert("white-dragon", SpeciesDef {
    name: "white-dragon",
    family: "draconic",
    biosphere: BiosphereTraits {
        mass: Mass::new(2200.0).unwrap(),                       // 5E adult white dragon
        metabolic_class: MetabolicClass::Endotherm,
        niche: ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap(), // obligate apex
        condition_niche: white_dragon_condition_niche(),
        potency: 0.85,                                          // mighty
    },
    peopled: None,
});
```

- [ ] **Step 4: Author the remaining 11** by the same template against the table above and the C1-measured ranges (each: a `*_condition_niche()` helper + a `reg.insert(…)` literal, `peopled: None`); add `draconic` (white/red/black → radiation) and `plant` (treant/twig-blight) entries to `family_registry()`. (Execution-time authoring; each is one struct literal following Step 3's shape.)

- [ ] **Step 5: Run the test** — `cargo test -p hornvale-species menagerie_is_biosphere_only_and_spans_axes`. Expected: PASS.

- [ ] **Step 6: Run the full species + worldgen suite** — `cargo nextest run -p hornvale-species -p hornvale-worldgen`. Expected: PASS (peoples still byte-identical; fauna placed by habitat, skipped by settlement).

- [ ] **Step 7: Commit**

```bash
git add domains/species/src/lib.rs
git commit -m "feat(species): author the canonical-5E menagerie (biosphere-only)"
```

---

## Task 5: Cut genesis over onto niche-K

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (`build_world_with_roster` / `demography_inputs_for` — genesis reads the niche-differentiated K instead of the flat path; the prepared change is `.superpowers/sdd/a15a-flip.patch`, 101/26 over this file)
- Test: existing worldgen tests + artifact regeneration

**Interfaces:**
- Consumes: `niche_per_species_k`, `demography_report` (existing), the menagerie (Task 4).

- [ ] **Step 1: Apply the prepared cutover.** Review `.superpowers/sdd/a15a-flip.patch` against the post-split file; apply its intent (genesis `build_world_with_roster` consumes `niche_per_species_k` rather than the flat `demography_inputs_for` psych path). If the patch does not apply cleanly over the split, port its change by hand (it points genesis at the niche-K fn the shadow already used).

```bash
git apply --3way .superpowers/sdd/a15a-flip.patch || echo "port by hand: point build_world_with_roster at niche_per_species_k"
```

- [ ] **Step 2: Build + run worldgen tests** — `cargo nextest run -p hornvale-worldgen`. Expected: the genesis-path tests now reflect niche-K; any test asserting the *old* flat composition is updated to the niche composition (the intended behavior change), not deleted.

- [ ] **Step 3: Regenerate the committed non-census artifacts** and confirm the drift is the intended niche cutover:

```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/hv.json
cargo run -p hornvale -- almanac --world /tmp/hv.json > book/src/gallery/almanac-seed-42-sky.md
git diff --stat book/src/gallery/
```

Expected: seed-42 settlement composition changes (niche-differentiated); sky/astronomy artifacts unchanged.

- [ ] **Step 4: Commit** (census fixtures intentionally left drifting — they ride the shared AWS regen, never local):

```bash
git add windows/worldgen/src/lib.rs book/src/gallery/
git commit -m "feat(worldgen): cut genesis over onto niche-differentiated K"
```

---

## Task 6: Verify biogeography emerges (the behavioral exit criterion)

**Files:**
- Test: `windows/lab/tests/` or `windows/worldgen/src/lib.rs` test module

**Interfaces:**
- Consumes: the menagerie (Task 4), the cutover (Task 5), the `composition-variance` metric (shipped by The Niche).

- [ ] **Step 1: Write the exit-criterion test** — the menagerie produces structure the goblinoids could not.

```rust
#[test]
fn menagerie_biogeography_emerges() {
    let world = build_world_with_roster(SEED_42, &full_roster());
    let cv = composition_variance(&world);
    // richer than the goblinoid-only 0.065 (The Niche's readout)
    assert!(cv > 0.065, "composition-variance {cv} should exceed the goblinoid 0.065");
    // at least one resource-partitioned stronghold: a mighty apex dominates a tile
    assert!(has_resource_partitioned_stronghold(&world),
        "a mighty apex should hold a climate tile");
}
```

- [ ] **Step 2: Run it** — `cargo nextest run -p hornvale-worldgen menagerie_biogeography_emerges`. Expected: PASS. If the mighty apex does not hold a stronghold, this is a not-yet-frozen authored-value calibration (spec §9; the B2b precedent #23) — retune the apex's sovereignty/devotion against measured fields, re-observe, do not weaken the assertion.

- [ ] **Step 3: Run the full commit gate** — `make gate`. Expected: green except the known census-schema-lag reds (the deferred AWS regen batch); confirm no *new* failures beyond those.

- [ ] **Step 4: Commit**

```bash
git add windows/worldgen/src/lib.rs
git commit -m "test(worldgen): biogeography emerges from the menagerie"
```

---

## Task 7: Close the campaign

**Files:**
- Create: `book/src/chronicle/the-seam.md`, `docs/retrospectives/2026-07-14-the-seam.md`
- Modify: `book/src/frontier/idea-registry.md` (UNI-22 `raw` → `elaborated`, point at the metaplan + this campaign)

- [ ] **Step 1: Write the chronicle entry** (`book/src/chronicle/the-seam.md`) — the god-struct that had become a world; the split; the menagerie breaking the goblinoid ceiling; at the deliberate altitude (technical, comprehensible without the code).

- [ ] **Step 2: Write the retrospective** (`docs/retrospectives/2026-07-14-the-seam.md`) — process lessons (the strangler-fig seam, the two wrinkles that resolved inside the architecture, the autopilot design run that fed the metaplan).

- [ ] **Step 3: Flip UNI-22** in `book/src/frontier/idea-registry.md` from `raw` → `elaborated`; add cross-links to the metaplan spec and this campaign; run `cargo test -p hornvale --test docs_consistency` (the frontier drift-check) to confirm links resolve.

- [ ] **Step 4: Freshness sweep** — check the species chapter / model-card pointers the goblinoid docs reference (followups.md flagged a dangling "species chapter's model card"); land or soften them.

- [ ] **Step 5: Final gate + commit**

```bash
make gate    # green modulo the known census-lag reds (deferred AWS regen)
git add book/ docs/retrospectives/
git commit -m "docs(the-seam): chronicle, retrospective, UNI-22 elaborated"
```

- [ ] **Step 6: Merge readiness** — invoke `closing-a-campaign`; the census regen (the drifted fixtures) and the push are Nathan's calls (carve-outs). Do not merge or push without his go.
