# The Branches (L1) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give the world its first language family — one authored proto-goblinoid, three daughters (goblin, hobgoblin, bugbear) that descend from it through per-lineage regular cascades, with cognates true by construction and kobold as the clean outgroup.

**Architecture:** A precise delta on The Words. Proto-roots lift from *per-species* to *per-family*: one proto-goblinoid phonology and one proto-root per concept, shared by the three daughters, each of which owns its cascade and present phonology. After the cascade, inherited segments outside a daughter's inventory are **nativized** (feature-nearest merge) so `evolve` guarantees `modern ⊆ inventory`. Family membership and the proto's ancestral vector are authored data in `domains/species`; the proto phonology is drawn and the shared proto-roots are threaded at the composition root (`windows/worldgen`). The engine (`evolve`, the rule family, lexicon assembly, views) is otherwise reused unchanged.

**Tech Stack:** Rust edition 2024, `serde`/`serde_json` only, mdbook. No new crates.

**Spec:** `docs/superpowers/specs/2026-07-09-the-branches-design.md` (governs).

## Global Constraints

- Full gate on every commit: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`; run `cargo fmt` as the final step before every commit.
- Determinism is constitutional: no wall-clock, no `HashMap`/`HashSet` (`BTreeMap`/`BTreeSet`/`Vec` only), float sorts via `total_cmp`. Randomness only from kernel `Seed`/`Stream` (`derive(label)`, `stream()`, `range_u32(lo,hi)` inclusive, `next_f64()`, `pick(&[T])`).
- `domains/language` depends on `hornvale-kernel` and NOTHING else. Family membership, the proto vector, and the shared proto-roots arrive via the composition root — no domain-to-domain edge is added.
- `#![warn(missing_docs)]` everywhere; a one-line doc comment on every public item, field, and variant.
- **Nativization is pure and draw-free** (spec §2.2, §9): a function of `(segments, inventory)` applied after the cascade; it perturbs no stream and is a no-op wherever the proto inventory already sits inside the daughter's (every self-proto lineage). `evolve` gains the postcondition `modern ⊆ inventory`.
- **The singleton path is byte-stable at unit level** (spec §2.4, §6): with `family == species` and `proto_ph == ph`, `build_lexicon` consumes exactly the draws The Words consumed and returns an identical `Lexicon` for identical exposures. This is the keystone — asserted as a unit test, NOT as kobold's world output.
- **The seed-42 world re-baselines world-wide** (spec §6): it gains two peoples (settlement placement over a larger roster, new endonym concepts in every language, shifted coexistence exposures). Determinism (same seed + roster → identical world) is the invariant; the committed galleries/almanacs/dictionary are re-pinned in this campaign's commits, not held byte-stable against the pre-Branches fixture.
- **Epoch discipline:** goblin's proto-roots move under the family label `language/goblinoid/lexicon/root/<concept>`; the per-species `language/goblin/lexicon/root/<concept>` path is documented as retired in `stream_labels()`, never silently renamed. Kobold keeps `language/kobold/lexicon/root/<concept>`.
- **Closed inventories:** the species registry, the family registry, and the sound-rule family stay fixed authored/enumerated sets. Widening any is a later campaign.
- Evidence discipline: implementer reports carry verbatim command transcripts; gate claims are void unless the controller reruns the gate. Preregistration (ADR 0016): any calibration claim is written before measurement, pinned after, never tuned to pass.
- Work on branch `campaign-the-branches` (worktree per `superpowers:using-git-worktrees`). **Note:** local `main` is 28 commits ahead of `origin/main` and carries three uncommitted working-tree edits (Words polish in `ci.yml`, `etymology.rs`, `lib.rs`, `concepts.rs`) — confirm with the user whether those are committed/stashed before branching, so the worktree starts clean.

---

### Task 1: Book — the descent sections (book-driven development, no code)

**Files:**
- Modify: `book/src/domains/language.md` (append the descent sections; one language domain, one chapter)

**Interfaces:**
- Consumes: the spec in full; the existing chapter's voice (read it first — match register and altitude).
- Produces: the prose later tasks must live up to; Task 11 revisits it in the freshness sweep.

- [ ] **Step 1: Write the sections.** Append, at book altitude (technical, comprehensible without the code): **the first family** — why own-line descent left the world with no shared ancestry, and what a proto-language *is* here (a language with no speakers); **descent under anatomical constraint** — one proto-root, three cascades, the articulation vector as the codomain the cascade lands on, and **nativization** as how a daughter absorbs an inherited sound it no longer keeps (`modern ⊆ inventory`); **the loudness axis** — hobgoblin loud, goblin baseline, bugbear quiet-and-guttural, and that voice-loudness already biases the engine; **kobold the outgroup** — a family of one, the negative control; **the re-baseline** — honestly, that adding two peoples re-baselines the whole seed-42 world; the bright scope line (no replacement, no borrowing, no reconstruction — those are L2/L3/L4).
- [ ] **Step 2: Build.** Run: `mdbook build book`. Expected: clean build.
- [ ] **Step 3: Commit.**
```bash
git add book/src/domains/language.md
git commit -m "docs(book): descent sections open The Branches (book-driven development)"
```

### Task 2: Freeze the pre-Branches (post-Words) fixtures

Not for byte-comparison of the whole world — the world re-baselines. These anchor the *structural* keystone (Task 8): kobold's phonology/cascade draw paths unchanged as functions, goblin's non-linguistic facts unchanged, the new peoples appearing.

**Files:**
- Create: `cli/tests/fixtures/pre-branches-seed-42-world.json`
- Create: `cli/tests/fixtures/pre-branches-seed-42-almanac.md`

**Interfaces:**
- Produces: the fixtures `cli/tests/branches_identity.rs` (Task 8) reads.

- [ ] **Step 1: Rebuild, then generate (stdout only — no `2>&1`).**
```bash
cargo build -p hornvale
cargo run -q -p hornvale -- new --seed 42 --out cli/tests/fixtures/pre-branches-seed-42-world.json
cargo run -q -p hornvale -- almanac --world cli/tests/fixtures/pre-branches-seed-42-world.json > cli/tests/fixtures/pre-branches-seed-42-almanac.md
```
- [ ] **Step 2: Verify pristine.** Run `head -1 cli/tests/fixtures/pre-branches-seed-42-almanac.md` (must be the almanac title) and `grep -c "Finished\|Compiling" cli/tests/fixtures/pre-branches-seed-42-almanac.md` → `0`.
- [ ] **Step 3: Commit.**
```bash
git add cli/tests/fixtures/pre-branches-seed-42-*
git commit -m "test(fixtures): freeze pre-Branches seed-42 outputs for the structural keystone"
```

### Task 3: Nativization in the etymology engine

**Files:**
- Modify: `domains/language/src/etymology.rs`
- Test: in-module `#[cfg(test)]` (the file's existing pattern)

**Interfaces:**
- Consumes: `Segment` (`phoneme.rs`), `Phonology { inventory: Vec<Segment>, .. }` (`phonology.rs`).
- Produces:
  - `pub fn nativize(segs: &[Segment], ph: &Phonology) -> Vec<Segment>` — each segment already in `ph.inventory` is kept; each off-inventory segment is replaced by the nearest **same-class** segment in `ph.inventory` (consonant→consonant, vowel→vowel) by feature-mismatch count, ties broken by `Segment`'s `Ord`; if the inventory holds no same-class segment, the segment is left unchanged.
  - `evolve` gains the postcondition `modern ⊆ ph.inventory` by applying `nativize` as its final step.

- [ ] **Step 1: Write failing tests.**
```rust
#[test]
fn nativize_keeps_in_inventory_segments_untouched() {
    let ph = test_phonology(); // permissive: every segment in-inventory
    let word = proto_root(&Seed(1), "test", "water", &ph);
    assert_eq!(nativize(&word, &ph), word); // no-op when all in-inventory
}

#[test]
fn nativize_merges_off_inventory_to_nearest_same_class() {
    // A restrictive inventory lacking the postalveolar sibilant ʃ; ʃ must
    // merge to the nearest same-class (consonant) segment present, never to
    // a vowel.
    let ph = restrictive_no_postalveolar();
    let sh = Segment::Consonant { place: Place::Postalveolar, manner: Manner::Sibilant, voiced: false };
    let out = nativize(&[sh], &ph);
    assert!(ph.inventory.contains(&out[0]));
    assert!(matches!(out[0], Segment::Consonant { .. }));
}

#[test]
fn evolve_output_is_subset_of_inventory_even_from_foreign_proto() {
    // proto drawn from a permissive inventory, evolved into a restrictive one
    let proto_ph = test_phonology();
    let daughter_ph = restrictive_no_postalveolar();
    let proto = proto_root(&Seed(2), "goblinoid", "water", &proto_ph);
    let cascade = draw_cascade(&Seed(2), "bugbear");
    let d = evolve(&proto, &cascade, &daughter_ph);
    assert!(d.modern.iter().all(|s| daughter_ph.inventory.contains(s)));
}
```
Add a `restrictive_no_postalveolar()` fixture beside `test_phonology()` (draw under an `Envelope` with `sibilance: 0.0` or post-filter the inventory to drop `Postalveolar`).
- [ ] **Step 2: Run to verify failure.** Run: `cargo test -p hornvale-language nativize -- --nocapture`. Expected: FAIL — `nativize` not found.
- [ ] **Step 3: Implement `nativize`.**
```rust
/// Merge each segment not already in `ph.inventory` to the nearest
/// same-class inventory segment (consonant→consonant, vowel→vowel) by
/// feature-mismatch count, ties broken by `Segment`'s total order. A segment
/// with no same-class neighbour in the inventory is left unchanged. Pure and
/// draw-free: this is how descent absorbs an inherited sound the daughter's
/// inventory no longer keeps (spec §2.2).
pub fn nativize(segs: &[Segment], ph: &Phonology) -> Vec<Segment> {
    segs.iter()
        .map(|&s| {
            if ph.inventory.contains(&s) {
                return s;
            }
            ph.inventory
                .iter()
                .copied()
                .filter(|c| same_class(*c, s))
                .min_by(|a, b| {
                    feature_distance(*a, s)
                        .cmp(&feature_distance(*b, s))
                        .then(a.cmp(b))
                })
                .unwrap_or(s)
        })
        .collect()
}

/// Whether two segments are both consonants or both vowels.
fn same_class(a: Segment, b: Segment) -> bool {
    matches!(
        (a, b),
        (Segment::Consonant { .. }, Segment::Consonant { .. })
            | (Segment::Vowel { .. }, Segment::Vowel { .. })
    )
}

/// Count of differing features between two same-class segments (place,
/// manner, voicing for consonants; height, backness, rounding for vowels).
/// Cross-class pairs never reach here (filtered by `same_class`).
fn feature_distance(a: Segment, b: Segment) -> u8 {
    match (a, b) {
        (
            Segment::Consonant { place: p1, manner: m1, voiced: v1 },
            Segment::Consonant { place: p2, manner: m2, voiced: v2 },
        ) => (p1 != p2) as u8 + (m1 != m2) as u8 + (v1 != v2) as u8,
        (
            Segment::Vowel { height: h1, backness: b1, rounded: r1 },
            Segment::Vowel { height: h2, backness: b2, rounded: r2 },
        ) => (h1 != h2) as u8 + (b1 != b2) as u8 + (r1 != r2) as u8,
        _ => u8::MAX,
    }
}
```
Then, in `evolve`, after the rule loop and before building the `Derivation`, add `let current = nativize(&current, ph);`.
- [ ] **Step 4: Run tests.** Run: `cargo test -p hornvale-language etymology` (all existing evolve/rule tests plus the three new). Expected: PASS — existing tests unaffected (nativize is a no-op when proto is drawn from the same `ph`).
- [ ] **Step 5: Commit.**
```bash
git add domains/language/src/etymology.rs
git commit -m "feat(language): nativize inherited segments; evolve guarantees modern ⊆ inventory"
```

### Task 4: `build_lexicon` lifts the proto to the family

**Files:**
- Modify: `domains/language/src/lexicon.rs` (signature + pass 1)
- Modify: `windows/worldgen/src/lib.rs` (callers: `lexicon_of` ~977, genesis loop ~1319) — minimal wiring; Task 6 completes it
- Modify: `windows/lab/src/metrics.rs` if it calls `build_lexicon` directly (it uses `draw_cascade`; grep to confirm)
- Test: `domains/language/src/lexicon.rs` `#[cfg(test)]`

**Interfaces:**
- Produces:
  - `pub fn build_lexicon(seed: &Seed, species: &str, family: &str, ph: &Phonology, proto_ph: &Phonology, exposures: &BTreeMap<String, ExposureClass>) -> Lexicon` — pass 1 draws `proto_root(seed, family, concept, proto_ph)` and evolves it through `draw_cascade(seed, species)` landing on `ph` (nativized). `family == species && proto_ph == ph` reproduces The Words exactly.

- [ ] **Step 1: Write failing tests.**
```rust
#[test]
fn singleton_family_reproduces_the_words() {
    // family == species, proto_ph == ph: identical to the old 4-arg call.
    let ph = test_phonology();
    let ex = sea_exposures();
    let lex = build_lexicon(&Seed(1), "test", "test", &ph, &ph, &ex);
    // water's proto-root draws under language/test/lexicon/root/water, as before
    let proto = proto_root(&Seed(1), "test", "water", &ph);
    let expected = evolve(&proto, &draw_cascade(&Seed(1), "test"), &ph).modern;
    match lex.entry("water").unwrap() {
        LexEntry::Root { derivation, .. } => assert_eq!(derivation.modern, expected),
        _ => panic!("water should be a Root"),
    }
}

#[test]
fn two_daughters_share_a_proto_root_but_differ() {
    // same family + proto_ph, different species (different cascades/ph):
    // cognates — same proto, related-but-distinct modern forms.
    let proto_ph = test_phonology();
    let gob_ph = daughter_ph("goblin");
    let hob_ph = daughter_ph("hobgoblin");
    let ex = one_steeped("water");
    let g = build_lexicon(&Seed(3), "goblin", "goblinoid", &gob_ph, &proto_ph, &ex);
    let h = build_lexicon(&Seed(3), "hobgoblin", "goblinoid", &hob_ph, &proto_ph, &ex);
    let (gp, hp) = (root_proto(&g, "water"), root_proto(&h, "water"));
    assert_eq!(gp, hp, "same family+proto_ph ⇒ identical proto-root");
}
```
(`daughter_ph`, `one_steeped`, `root_proto` are small test helpers; `root_proto` returns the `derivation.proto` of a concept's `LexEntry::Root`.)
- [ ] **Step 2: Run to verify failure.** Run: `cargo test -p hornvale-language build_lexicon`. Expected: FAIL — arity mismatch / helpers undefined.
- [ ] **Step 3: Implement.** Change the signature and pass 1:
```rust
pub fn build_lexicon(
    seed: &Seed,
    species: &str,
    family: &str,
    ph: &Phonology,
    proto_ph: &Phonology,
    exposures: &BTreeMap<String, ExposureClass>,
) -> Lexicon {
    let headedness = draw_headedness(seed, species);
    let cascade = draw_cascade(seed, species);
    let mut entries: BTreeMap<String, LexEntry> = BTreeMap::new();
    for (concept, class) in exposures {
        if matches!(class, ExposureClass::Steeped) {
            let proto = proto_root(seed, family, concept, proto_ph);
            let derivation = evolve(&proto, &cascade, ph);
            let views = word_views(&derivation.modern);
            entries.insert(concept.clone(), LexEntry::Root { derivation, views });
        }
    }
    // pass 2 unchanged
    // ...
}
```
Then update the two worldgen callers to pass `def.name` for both `species` and `family` and `&ph` for both `ph` and `proto_ph` **as a temporary singleton shim** (Task 6 replaces the shim with real family resolution). This keeps the workspace compiling and every existing world byte-identical after Task 4.
- [ ] **Step 4: Run tests + gate.** Run: `cargo test --workspace`. Expected: PASS — with the singleton shim, all worlds are unchanged.
- [ ] **Step 5: Commit.**
```bash
git add domains/language/src/lexicon.rs windows/worldgen/src/lib.rs windows/lab/src/metrics.rs
git commit -m "feat(language): build_lexicon draws proto-roots per family (singleton shim keeps worlds stable)"
```

### Task 5: Hobgoblin and bugbear; the family field and registry; new concepts

**Files:**
- Modify: `domains/species/src/lib.rs` (add `family` to `SpeciesDef`; add the two species; add `family_registry`; register the two new endonym concepts)
- Test: `domains/species/src/lib.rs` `#[cfg(test)]`

**Interfaces:**
- Produces:
  - `SpeciesDef` gains `pub family: &'static str` (goblin/hobgoblin/bugbear = `"goblinoid"`, kobold = `"kobold"`).
  - `registry()` gains `"hobgoblin"` and `"bugbear"` (full three-vector authoring below).
  - `pub fn family_registry() -> BTreeMap<&'static str, ArticulationVector>` — proto ancestral vectors for multi-member families; `"goblinoid"` present, singletons absent.
  - Two new concepts registered by the species domain: `hobgoblin-kind`, `bugbear-kind` (`ConceptKind::Living`).

- [ ] **Step 1: Write failing tests.**
```rust
#[test]
fn registry_has_the_goblinoid_triad_and_kobold() {
    let r = registry();
    for name in ["goblin", "hobgoblin", "bugbear", "kobold"] {
        assert!(r.contains_key(name), "{name} missing");
    }
    assert_eq!(r["hobgoblin"].family, "goblinoid");
    assert_eq!(r["bugbear"].family, "goblinoid");
    assert_eq!(r["kobold"].family, "kobold");
}

#[test]
fn family_divides_along_voice_loudness() {
    let r = registry();
    let l = |n: &str| r[n].articulation.voice_loudness;
    assert!(l("bugbear") < l("goblin") && l("goblin") < l("hobgoblin"));
}

#[test]
fn proto_goblinoid_vector_equals_no_daughter() {
    let proto = family_registry()["goblinoid"];
    let r = registry();
    for d in ["goblin", "hobgoblin", "bugbear"] {
        assert_ne!(proto, r[d].articulation, "proto must differ from {d}");
    }
}
```
- [ ] **Step 2: Run to verify failure.** Run: `cargo test -p hornvale-species registry_has_the_goblinoid_triad`. Expected: FAIL — field/species/fn missing.
- [ ] **Step 3: Implement.** Add `pub family: &'static str` to `SpeciesDef`; set `family: "goblinoid"` on goblin, `family: "kobold"` on kobold. Insert the two species (values authored from SRD lore; scalars pinned here, reviewable):

  **hobgoblin** — `noun: "legion"`, psych `{ threat_response: 0.7, deliberation_latency: 0.6, in_group_radius: 0.6, time_horizon: 0.7, sociality: Hierarchic, status_basis: Rank }`, perception `{ activity: Diurnal, night_vision: 0.6, sky_attention: 0.5 }`, articulation `{ labiality: 0.5, vowel_space: 0.5, voicing: 0.6, sibilance: 0.4, voice_loudness: 0.8, exotic: None }`, roles `worker_override: Some("laborer"), warrior: "soldier", artisan: "smith", shaman: "augur", top: "warlord"`.

  **bugbear** — `noun: "lair"`, psych `{ threat_response: 0.7, deliberation_latency: 0.4, in_group_radius: 0.3, time_horizon: 0.3, sociality: Communal, status_basis: Rank }`, perception `{ activity: Nocturnal, night_vision: 0.7, sky_attention: 0.3 }`, articulation `{ labiality: 0.5, vowel_space: 0.4, voicing: 0.7, sibilance: 0.2, voice_loudness: 0.3, exotic: None }`, roles `worker_override: Some("forager"), warrior: "mauler", artisan: "tanner", shaman: "omen-reader", top: "headman"`.

  Add:
```rust
/// Proto ancestral articulation vectors, keyed by family, for families with
/// more than one member (a singleton's proto is itself and is absent here).
/// Each is a distinct point equal to no daughter's vector (spec §3).
pub fn family_registry() -> BTreeMap<&'static str, ArticulationVector> {
    let mut m = BTreeMap::new();
    m.insert(
        "goblinoid",
        ArticulationVector {
            labiality: 0.5,
            vowel_space: 0.5,
            voicing: 0.55,
            sibilance: 0.45,
            voice_loudness: 0.55,
            exotic: ExoticManner::None,
        },
    );
    m
}
```
  In the species `register_concepts`, add `register_concept("hobgoblin-kind", "species", ConceptKind::Living, "...")` and the bugbear analogue (mirror the existing goblin-kind/kobold-kind registrations).
- [ ] **Step 4: Run species tests.** Run: `cargo test -p hornvale-species`. Expected: PASS.
- [ ] **Step 5: Commit.**
```bash
git add domains/species/src/lib.rs
git commit -m "feat(species): hobgoblin and bugbear join the goblinoid family; family registry + proto vector"
```

### Task 6: Composition root — proto phonology and family-aware lexicon wiring

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (add `proto_phonology_of`; make `lexicon_of` and the genesis loop resolve family and thread the proto phonology, replacing Task 4's singleton shim)
- Test: `windows/worldgen/src/lib.rs` `#[cfg(test)]`

**Interfaces:**
- Consumes: `hornvale_species::{registry, family_registry}`, `hornvale_language::{draw_phonology, build_lexicon}`, `envelope_of` (existing).
- Produces:
  - `fn proto_phonology_of(world: &World, family: &str) -> hornvale_language::Phonology` — `draw_phonology(&world.seed, family, &envelope_of(&family_registry()[family]))`.
  - `lexicon_of` / genesis resolve, per daughter: `let family = def.family; let (fam_label, proto_ph) = match family_registry().get(family) { Some(_) => (family, proto_phonology_of(world, family)), None => (def.name, ph.clone()) };` then `build_lexicon(&seed, def.name, fam_label, &ph, &proto_ph, &exposures)`.

- [ ] **Step 1: Write failing tests.**
```rust
#[test]
fn goblin_and_hobgoblin_are_cognate() {
    let world = /* new seed-42 world */;
    let g = lexicon_of(&world, "goblin").unwrap();
    let h = lexicon_of(&world, "hobgoblin").unwrap();
    // a concept both are Steeped in shares an identical proto-root
    assert_eq!(root_proto(&g, "water"), root_proto(&h, "water"));
    // ...but present-day forms may differ (different cascades)
}

#[test]
fn every_goblinoid_word_is_in_its_inventory() {
    let world = /* new seed-42 world */;
    for sp in ["goblin", "hobgoblin", "bugbear"] {
        let ph = language_of(&world, sp);
        let lex = lexicon_of(&world, sp).unwrap();
        for (_c, e) in lex.entries() {
            if let LexEntry::Root { derivation, .. } = e {
                assert!(derivation.modern.iter().all(|s| ph.inventory.contains(s)));
            }
        }
    }
}

#[test]
fn kobold_lexicon_mechanism_is_stable_given_fixed_exposures() {
    let world = /* new seed-42 world */;
    let ph = language_of(&world, "kobold");
    let ex = exposure_of(&world, "kobold").unwrap();
    // singleton path: family==species, proto_ph==ph
    let direct = hornvale_language::build_lexicon(&world.seed, "kobold", "kobold", &ph, &ph, &ex);
    assert_eq!(lexicon_of(&world, "kobold").unwrap(), direct);
}
```
- [ ] **Step 2: Run to verify failure.** Run: `cargo test -p hornvale-worldgen cognate`. Expected: FAIL — `proto_phonology_of` undefined / roots differ under the shim.
- [ ] **Step 3: Implement** `proto_phonology_of` and the family resolution in both `lexicon_of` and the genesis loop, removing the Task 4 shim.
- [ ] **Step 4: Run.** Run: `cargo test -p hornvale-worldgen`. Expected: PASS.
- [ ] **Step 5: Commit.**
```bash
git add windows/worldgen/src/lib.rs
git commit -m "feat(worldgen): draw proto-goblinoid and thread shared proto-roots to the daughters"
```

### Task 7: Stream labels and pin-isolation

**Files:**
- Modify: `domains/language/src/lib.rs` (`stream_labels`)
- Modify: `domains/language/tests/` or the existing pin-isolation test location (grep `genesis_properties`/lexicon pin tests)
- Modify: `book/src/reference/stream-manifest-generated.md` (regenerated)

**Interfaces:**
- Produces: `stream_labels()` describing the family-keyed proto-root path and the retired per-species one; extended pin-isolation coverage.

- [ ] **Step 1: Update `stream_labels()`.** Change the lexicon-root entry to `("language/<family>/lexicon/root/<concept>", "per-concept proto-root drawn from the family's proto phonology; family == species for a singleton stock")`, and add a retired entry `("language/goblin/lexicon/root/<concept>", "(retired at The Branches, superseded by language/goblinoid/lexicon/root/<concept>) pre-Branches per-species goblin proto-root")`. Note in the phonology-inventory entry's doc that a family name (e.g. `goblinoid`) occupies the `<species>` slot for a proto-language with no speakers.
- [ ] **Step 2: Write/extend the pin-isolation test.** Assert (a) the family proto-root draw for a concept consumes the same stream regardless of how many daughters read it, and (b) the singleton (`kobold`) proto-root path consumes exactly what The Words consumed:
```rust
#[test]
fn family_proto_root_is_independent_of_daughter_count() {
    let ph = permissive();
    let a = proto_root(&Seed(9), "goblinoid", "water", &ph);
    let b = proto_root(&Seed(9), "goblinoid", "water", &ph); // second reader
    assert_eq!(a, b);
}
```
- [ ] **Step 3: Regenerate the manifest.** Run: `cargo run -p hornvale -- streams > book/src/reference/stream-manifest-generated.md`.
- [ ] **Step 4: Gate.** Run: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`.
- [ ] **Step 5: Commit.**
```bash
git add domains/language/src/lib.rs domains/language/tests book/src/reference/stream-manifest-generated.md
git commit -m "feat(language): family-keyed proto-root labels + pin-isolation; retire the goblin-species root path"
```

### Task 8: The keystone — determinism, singleton mechanism, structural facts

**Files:**
- Create: `cli/tests/branches_identity.rs`
- Regenerate + commit: the committed seed-42 galleries and almanacs (world re-baseline)

**Interfaces:**
- Consumes: the Task 2 fixtures; `hornvale_worldgen` re-derivation surfaces.

- [ ] **Step 1: Write the keystone test.** Assert, against a freshly built seed-42 world:
  - **Determinism:** two independent builds of seed 42 are byte-identical (`new` twice, diff).
  - **Singleton mechanism:** `lexicon_of(world, "kobold") == build_lexicon(seed, "kobold", "kobold", &kph, &kph, &kobold_exposures)`.
  - **Kobold draw paths unchanged:** `language_of(world, "kobold")` equals the phonology recomputed from the pre-Branches fixture's kobold articulation vector (its phonology is a pure function of seed + vector, unchanged by the roster).
  - **Structural facts:** the world contains hobgoblin and bugbear entities; goblin's non-linguistic facts (e.g. `peopled-by`, biome facts) match the pre-Branches fixture for the settlements that still exist; goblin's committed names differ in text.
  - **Inventory closure across the family** (mirror Task 6's closure test at world scope).
- [ ] **Step 2: Run.** Run: `cargo test -p hornvale --test branches_identity`. Expected: PASS.
- [ ] **Step 3: Re-baseline the committed world artifacts.** Regenerate the seed-42 galleries/almanacs that the CI "Artifacts are current" step owns (see `.github/workflows/ci.yml`) EXCEPT the dictionary (Task 9 changes its format). Inspect a diff sample to confirm the new peoples appear and determinism holds.
- [ ] **Step 4: Commit.**
```bash
git add cli/tests/branches_identity.rs book/src/gallery book/src/reference
git commit -m "test(branches): determinism + singleton-mechanism keystone; re-baseline seed-42 world artifacts"
```

### Task 9: Proto-goblinoid reference page and dictionary cognate columns

**Files:**
- Modify: `cli/src/dictionary.rs` (cognate columns across a family; a proto-form column)
- Modify: the CLI verb table / a new `proto` dump if warranted (grep `cli/src` for the verb registry)
- Modify: `.github/workflows/ci.yml` (the dictionary generation line — note the uncommitted Words-polish edit already touches it; reconcile)
- Modify: `book/src/reference/dictionary-generated.md` and a new `book/src/reference/proto-goblinoid-generated.md` (regenerated)

**Interfaces:**
- Produces: a dictionary that shows, per concept, the goblinoid daughters side by side with the proto-form; a proto-goblinoid inventory/phonotactics/proto-root reference dump.

- [ ] **Step 1: Write a failing CLI/asserting test** that the dictionary output for seed 42 contains a cognate row where goblin/hobgoblin/bugbear forms share a glossed proto-form (`*…`).
- [ ] **Step 2: Implement** the cognate grouping (group daughters by `def.family`; for each family, render the proto-form once and each daughter's `romanize`/`ipa`) and the proto dump.
- [ ] **Step 3: Regenerate** both reference pages; add the proto page to `book/src/SUMMARY.md`.
- [ ] **Step 4: Gate + build book.** Run the full gate and `mdbook build book`.
- [ ] **Step 5: Commit.**
```bash
git add cli/ .github/workflows/ci.yml book/src
git commit -m "feat(cli): dictionary cognate columns + proto-goblinoid reference page"
```

### Task 10: The Lab battery

**Files:**
- Modify: `windows/lab/src/metrics.rs` (extend the family with cross-family regularity, monophyly, clean-outgroup, inventory-closure metrics)
- Create: `studies/branches-family.study.json`
- Regenerate: the study's committed output under `book/src/laboratory/`

**Interfaces:**
- Produces: metrics proving the four/five §7 properties; a committed study result.

- [ ] **Step 1: Write metrics as failing tests first** (the repo's metrics have unit tests; mirror `lexicon_regular` at `metrics.rs:1441`). Metrics: (1) **regularity family-wide** — a proto-segment in a given environment realizes consistently across all of a daughter's words; (2) **monophyly** — every goblinoid `Root`'s `derivation.proto` matches the family proto-root for that concept; (3) **clean outgroup** — no concept's kobold proto-root equals any goblinoid proto-root; (4) **inventory closure** — every daughter `Root.modern ⊆ inventory`.
- [ ] **Step 2: Implement** the metrics; register them (`lab list-metrics` must show them).
- [ ] **Step 3: Author + run the study.** `cargo run -p hornvale -- lab run studies/branches-family.study.json > book/src/laboratory/<study>.md`.
- [ ] **Step 4: Gate.** Full gate + `git diff --exit-code book/src/laboratory/` after a second run (determinism).
- [ ] **Step 5: Commit.**
```bash
git add windows/lab/src/metrics.rs studies book/src/laboratory
git commit -m "feat(lab): the family battery — regularity, monophyly, clean outgroup, inventory closure"
```

### Task 11: Book freshness sweep, chronicle, retrospective, final gate

**Files:**
- Modify: `book/src/domains/language.md` (freshness pass — reconcile with what shipped)
- Create: `book/src/chronicle/<n>-the-branches.md` (+ `SUMMARY.md` entry)
- Create: `docs/retrospectives/campaign-the-branches.md` (decision 0020: process, not product)
- Regenerate: every committed artifact the CI step owns; confirm drift-clean

**Interfaces:**
- Produces: Definition of Done — the book never lags merged reality.

- [ ] **Step 1: Freshness sweep** the language chapter against the shipped mechanism (nativization wording, the loudness scalars actually authored, the world-wide re-baseline framing).
- [ ] **Step 2: Chronicle entry** — the campaign's story at book altitude; add to `SUMMARY.md`.
- [ ] **Step 3: Retrospective** — one page, process lessons (e.g. two spec corrections surfaced during planning: nativization, and the world-wide re-baseline).
- [ ] **Step 4: Full artifact regen + drift check.** Run the CI "Artifacts are current" command list; `git diff --exit-code book/src/` must be clean after regeneration.
- [ ] **Step 5: Final gate.** `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings && mdbook build book`.
- [ ] **Step 6: Commit + update the idea registry** (flip LANG-5 `spec'd → shipped`, Where → this plan / the chronicle).
```bash
git add book docs/retrospectives docs/vision/idea-registry.md
git commit -m "docs(book): The Branches chronicle + freshness sweep + retrospective (campaign close)"
```

---

## Self-Review

**Spec coverage:** §1 goal → Tasks 5,6,9; §2.1 family/lineage split → Tasks 4,6; §2.2 descent + nativization → Task 3; §2.3 proto-is-a-language → Task 6 (`proto_phonology_of`); §2.4 singleton → Tasks 4,8; §2.5 shipped engine reused → Tasks 3,4 (no rule rewritten); §2.6 regularity → Task 10; §2.7 layering → Task 6 (data in species, wiring in worldgen); §3 proto-goblinoid → Tasks 5 (vector), 6 (phonology), 9 (page); §4 the daughters → Task 5; §5 descent → Tasks 4,6; §6 re-baseline/epoch → Tasks 7,8; §7 success + battery → Tasks 8,9,10; §8 non-goals → respected (no replacement/borrowing/reconstruction task exists); §9 determinism → Tasks 3,7,8. No gaps.

**Placeholder scan:** authored scalar values are concrete (Task 5); test helpers named where introduced; no "TBD"/"add error handling". The two places that say "grep to confirm" (metrics caller in Task 4, verb registry in Task 9) are locate-then-edit instructions, not deferred design.

**Type consistency:** `build_lexicon(seed, species, family, ph, proto_ph, exposures)` is used identically in Tasks 4, 6, 8; `nativize(&[Segment], &Phonology) -> Vec<Segment>` and `feature_distance`/`same_class` are consistent across Tasks 3 and 6; `family_registry() -> BTreeMap<&'static str, ArticulationVector>` consistent across Tasks 5 and 6.
