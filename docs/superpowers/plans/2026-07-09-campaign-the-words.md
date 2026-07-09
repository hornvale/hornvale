# Campaign: The Words — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A kernel concept layer (`ConceptDef` beside `PredicateDef`), an authored Swadesh core structured as perception-keyed packs, a per-language lexicon with own-line etymology (drawn proto-roots evolved through a drawn Neogrammarian cascade that lands on the shipped phonology), exposure-derived gaps with two recountable provenances, and proper names regenerated as glossable compounds over site facts.

**Architecture:** Concepts are registered kernel vocabulary, exactly like predicates. The lexicon engine lives in `domains/language` (kernel-only, as today): proto-roots and a sound-change cascade are new draws under new labels; the cascade's codomain is the *shipped* present-day phonology, so nothing existing re-derives. Exposure is derived at the composition root from ledger facts and handed to the engine as an input struct (the `SocietySummary` pattern). Names take an epoch bump to `/v2` and gain a gloss view; the lexicon itself is *not* committed to the ledger (a world is a seed plus a ledger; the lexicon re-derives like phonology does) — only `name-gloss` facts on named entities commit.

**Tech Stack:** Rust edition 2024, `serde`/`serde_json` only, mdbook.

**Spec:** `docs/superpowers/specs/2026-07-09-campaign-the-words-design.md` (governs).

## Global Constraints

- Full gate on every commit: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`; run `cargo fmt` as the final step before every commit.
- Determinism constitutional: no wall-clock, no `HashMap`/`HashSet` (`BTreeMap`/`BTreeSet`/`Vec` only), float sorts via `total_cmp`. Randomness only from kernel `Seed`/`Stream` (`derive(label)`, `stream()`, `range_u32(lo,hi)` inclusive, `next_f64()` in [0,1), `pick(&[T])`).
- `domains/language` depends on `hornvale-kernel` and NOTHING else. It never imports species/climate/settlement — exposure, pack depths, and site concepts arrive via input structs the composition root populates.
- No new external dependencies. `#![warn(missing_docs)]` everywhere; one-line doc comment on every public item, field, and variant.
- **The cascade lands on the shipped phonology** (spec §2.3): sound rules map segments *into the drawn present-day inventory*; a rule whose output segment is not in the inventory applies as identity. `language/<species>/phonology/*` and all pre-existing labels keep byte-identical consumption — the pin-isolation tests extend, never change.
- **No drawn gaps** (spec §2.4): every gap carries exactly one provenance — `Experiential` (recounts to ledger facts) or `Perceptual` (recounts to a perception-vector dimension).
- **Etymology is data** (spec §2.2): a `Derivation` (proto segments, applied rules, modern segments) is computed once and carried whole; surfaces replay it, never re-infer it.
- **Closed inventories everywhere** (spec §2.1): the concept inventory, the pack entries, the compound-recipe table, and the sound-rule family are fixed authored/enumerated sets. Widening any is a campaign decision.
- **Epoch discipline:** name streams move to `language/<species>/name/<kind>/v2`. The v1 labels are never renamed; `stream_labels()` keeps them with a "(retired at The Words)" doc. Old saves keep loading; their committed `name` facts render as before (no gloss — absence, not error).
- **Byte-identity does NOT hold for proper nouns** (re-baseline #2); it holds everywhere else. The keystone is the structural-invariant suite (Task 10). Determinism (same seed → identical output across runs) always holds and is tested.
- Preregistration discipline (ADR 0016): calibration claims written before the census runs; exact rates pinned after measurement, never tuned to pass.
- Evidence discipline: implementer reports carry verbatim command transcripts; gate claims are void unless the controller independently reruns the gate.
- Work on branch `campaign-the-words` (worktree per `superpowers:using-git-worktrees`).

---

### Task 1: The lexicon book sections (book-driven development — no code)

**Files:**
- Modify: `book/src/domains/language.md` (append the lexicon sections; the chapter stays one file — one language domain, one chapter)

**Interfaces:**
- Consumes: the spec (read in full); the existing chapter's voice (read it first — match its register and altitude).
- Produces: the prose later tasks must live up to; Task 13 revisits it in the freshness sweep.

- [ ] **Step 1: Write the sections**

Append to the language chapter, at book altitude (technical, comprehensible without the code): **concepts as promises** — the kernel registry grows a second table, and a registered concept asserts the sim can eventually be asked about the thing; **the Swadesh core as packs** — why a flat universal list would contradict per-species perception, the Berlin & Kay hue ladder with depth keyed to the perception vector, the luminance ladder a night-tuned people extends instead, the body pack's model-carded banking to BIO-1 (the voice-loudness precedent); **own-line descent** — proto-roots, the drawn cascade, the Neogrammarian regularity invariant, and the honest framing that the codomain constraint makes history *new data behind the same present*; **exposure and the two gap provenances** — "no kobold settlement touches coast" versus "kobold eyes are tuned for the dark," both recountable; **glossed names** — names as compounds over site facts, "Ice-Home" sits in tundra by construction, romanization/IPA/gloss as three views of one draw; the bright scope line (no borrowing, no tree, no syntax beyond compound headedness, no frames).

- [ ] **Step 2: Build and verify**

Run: `mdbook build book`
Expected: clean build.

- [ ] **Step 3: Commit**

```bash
git add book/src/domains/language.md
git commit -m "docs(book): lexicon sections open The Words (book-driven development)"
```

### Task 2: Freeze the pre-Words fixtures

Not for byte-comparison — proper nouns will change. Task 10's keystone asserts the *structural* invariant against these: entity ids and every non-linguistic fact unchanged; only name text changes and `name-gloss` facts appear.

**Files:**
- Create: `cli/tests/fixtures/pre-words-seed-42-world.json`
- Create: `cli/tests/fixtures/pre-words-seed-42-almanac.md`

**Interfaces:**
- Produces: the fixtures `cli/tests/words_identity.rs` (Task 10) reads.

- [ ] **Step 1: Rebuild, then generate (stdout only — no `2>&1`)**

```bash
cargo build -p hornvale
cargo run -q -p hornvale -- new --seed 42 --out cli/tests/fixtures/pre-words-seed-42-world.json
cargo run -q -p hornvale -- almanac --world cli/tests/fixtures/pre-words-seed-42-world.json > cli/tests/fixtures/pre-words-seed-42-almanac.md
```

- [ ] **Step 2: Verify pristine**

Run: `head -1 cli/tests/fixtures/pre-words-seed-42-almanac.md` (must be the almanac title line) and confirm no build noise: `grep -c "Finished\|Compiling" cli/tests/fixtures/pre-words-seed-42-almanac.md` → `0`.

- [ ] **Step 3: Commit**

```bash
git add cli/tests/fixtures/pre-words-seed-42-*
git commit -m "test(fixtures): freeze pre-Words seed-42 outputs for the structural-invariant keystone"
```

### Task 3: `ConceptDef` in the kernel registry

**Files:**
- Modify: `kernel/src/registry.rs`
- Test: in-module `#[cfg(test)]` (the file's existing pattern)

**Interfaces:**
- Consumes: nothing new.
- Produces:
  - `pub enum ConceptKind { Substance, Living, Celestial, Terrain, Social, Body, Kin, Quality }` (`Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize`)
  - `pub struct ConceptDef { pub name: String, pub domain: String, pub kind: ConceptKind, pub doc: String }` (derives as `PredicateDef`)
  - `ConceptRegistry::register_concept(&mut self, name: &str, domain: &str, kind: ConceptKind, doc: &str) -> Result<(), RegistryError>` — idempotent for identical defs, `ConflictingDefinition` on conflict (exactly `register_predicate`'s contract)
  - `ConceptRegistry::concept(&self, name: &str) -> Option<&ConceptDef>`
  - `ConceptRegistry::concepts(&self) -> impl Iterator<Item = &ConceptDef>` — name order

- [ ] **Step 1: Write the failing tests** (mirror the four predicate tests: retrievable, unknown-is-none, idempotent re-registration, conflicting re-registration errors; plus name-order iteration and serde round-trip)

```rust
#[test]
fn registered_concept_is_retrievable() {
    let mut r = ConceptRegistry::default();
    r.register_concept("water", "language", ConceptKind::Substance, "the drinkable liquid")
        .unwrap();
    assert_eq!(r.concept("water").unwrap().kind, ConceptKind::Substance);
}

#[test]
fn old_registry_json_without_concepts_still_loads() {
    // Pre-Words saves serialized a registry with no `concepts` field.
    let json = r#"{"predicates":{},"phenomenon_kinds":{}}"#;
    let r: ConceptRegistry = serde_json::from_str(json).unwrap();
    assert_eq!(r.concepts().count(), 0);
}
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-kernel registered_concept -- --nocapture`
Expected: FAIL — `register_concept` not found.

- [ ] **Step 3: Implement**

Add the `concepts: BTreeMap<String, ConceptDef>` field to `ConceptRegistry` **with `#[serde(default)]`** (the save-compat load test above is the reason), and the three methods, cloning `register_predicate`'s conflict logic.

- [ ] **Step 4: Run the kernel tests**

Run: `cargo test -p hornvale-kernel`
Expected: PASS, including all pre-existing registry tests.

- [ ] **Step 5: Commit**

```bash
git add kernel/src/registry.rs
git commit -m "feat(kernel): ConceptDef joins the concept registry beside PredicateDef"
```

### Task 4: Domains register their concepts; `concepts` verb dumps them

**Files:**
- Modify: `domains/astronomy/src/lib.rs` (its `register_concepts`), likewise `domains/climate`, `domains/terrain`, `domains/settlement`, `domains/species`, `domains/religion`
- Modify: `cli/src/` concepts-verb module (find it: `grep -rn "concept-registry-generated" cli/src`) to append a `## Concepts` table (name | domain | kind | doc)
- Modify: `book/src/reference/concept-registry-generated.md` (regenerated)
- Test: extend each domain's existing registration test; CLI drift covered by CI's artifact step

**Interfaces:**
- Consumes: Task 3's `register_concept`.
- Produces: the registered world-thing concepts, by owner —
  - astronomy: `sun`, `moon`, `star`, `night` (kind `Celestial`)
  - climate: `snow`, `rain`, `ice` (`Substance`) + one concept per `Biome` variant, kebab-cased (`tundra`, `taiga`, `temperate-forest`, …, kind `Terrain`)
  - terrain: `stone`, `mountain`, `sea` (`Terrain`; `stone` is `Substance`)
  - species: `goblin-kind`, `kobold-kind` (`Living`)
  - settlement: `home`, `hearth` (`Social`)
  - religion: `god`, `spirit` (`Social`)

- [ ] **Step 1: Write one failing test per domain** (in each domain's existing registration test module), e.g. astronomy:

```rust
#[test]
fn concepts_registered() {
    let mut r = ConceptRegistry::default();
    register_concepts(&mut r).unwrap();
    assert!(r.concept("moon").is_some());
    assert_eq!(r.concept("moon").unwrap().domain, "astronomy");
}
```

- [ ] **Step 2: Verify failures, implement each `register_concepts` extension, verify passes**

Run per domain: `cargo test -p hornvale-astronomy concepts_registered` (and kin).
Biome concepts iterate the `Biome` variants — add a `Biome::concept_name(&self) -> &'static str` helper in `domains/climate/src/biome.rs` rather than hand-listing thirteen strings twice.

- [ ] **Step 3: Extend the `concepts` verb and regenerate the reference page**

```bash
cargo run -q -p hornvale -- concepts > book/src/reference/concept-registry-generated.md
git diff --stat book/src/reference/concept-registry-generated.md   # concepts table appears
```

- [ ] **Step 4: Full gate, then commit**

```bash
cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings
git add domains/ cli/ book/src/reference/concept-registry-generated.md
git commit -m "feat(domains): every domain registers its owned concepts; concepts verb dumps them"
```

### Task 5: The Swadesh packs (authored core, in `domains/language`)

**Files:**
- Create: `domains/language/src/packs.rs`
- Modify: `domains/language/src/lib.rs` (module + re-exports + `register_concepts`)
- Modify: `windows/worldgen/src/lib.rs:123` `register_all` (add `hornvale_language::register_concepts(registry)?;`)
- Test: in-module

**Interfaces:**
- Consumes: kernel `ConceptRegistry`, Task 3.
- Produces:
  - `pub struct PackEntry { pub concept: &'static str, pub kind: ConceptKind, pub doc: &'static str, pub ladder_rank: u8 }` — `ladder_rank` 0 for unranked entries
  - `pub fn universal_stratum() -> &'static [PackEntry]` — water, stone, sun, moon, star, night, day, fire, wind, earth, tree, eat, sleep, die, name, one, two, many (authored; Nathan's to tune; concepts already registered by another domain — e.g. `sun` — are *not* re-registered here: the pack refers, the owner registers)
  - `pub fn color_pack() -> &'static [PackEntry]` — hue ladder ranked by Berlin & Kay stage: `dark` 1, `light` 1, `red` 2, `green` 3, `yellow` 3, `blue` 4, `brown` 5; luminance ladder: `gloom` 1, `shadow` 2, `starlit` 3 (kind `Quality`)
  - `pub fn body_pack() -> &'static [PackEntry]` — eye, mouth, hand, foot, blood, bone (model-carded: shared humanoid, banked to BIO-1)
  - `pub fn kin_pack() -> &'static [PackEntry]` — parent, child, sibling
  - `pub fn compound_recipe(concept: &str) -> Option<(&'static str, &'static str)>` — the closed authored recipe table for KNOWS-OF compounds, `(modifier, head)` in concept ids: `sea → ("many", "water")` … one row per compoundable concept; `None` means the concept cannot compound (must be root or gap)
  - `pub struct PackDepths { pub hue: u8, pub luminance: u8 }` — input struct; derivation from the perception vector lives in worldgen (Task 8)
  - `pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError>` — registers every pack entry not owned elsewhere, domain `"language"`
  - `pub fn in_ladder(entry: &PackEntry, depths: &PackDepths) -> bool` — rank 0 always in; hue entries in iff `ladder_rank <= depths.hue`; luminance likewise

- [ ] **Step 1: Failing tests** — registration ("`water` registered with domain language"), ladder gating ("`blue` (rank 4) out at hue depth 3, in at 4"), recipe closure ("every recipe's modifier and head are themselves registered concepts" — iterate all recipes, assert lookup succeeds), and duplicate-avoidance ("registering packs after astronomy does not conflict on `sun`").

- [ ] **Step 2: Verify failures; implement; verify passes**

Run: `cargo test -p hornvale-language packs`

- [ ] **Step 3: Full gate, commit**

```bash
git add domains/language windows/worldgen/src/lib.rs
git commit -m "feat(language): the Swadesh core as packs — Berlin & Kay ladders, recipes, registration"
```

### Task 6: The etymology engine — proto-roots and the cascade

**Files:**
- Create: `domains/language/src/etymology.rs`
- Modify: `domains/language/src/lib.rs` (module + re-exports); `domains/language/src/naming.rs` (expose the private segment→views rendering as `pub(crate) fn views_of(syllables) …` reuse — do not duplicate romanization)
- Test: in-module + property test in `domains/language/tests/` if one exists (else in-module)

**Interfaces:**
- Consumes: `Phonology`, `Segment`, `Seed`/`Stream`.
- Produces:
  - `pub enum RuleKind { Lenition, Fortition, VowelShift, ClusterSimplify, FinalLoss }` (all derives as other enums)
  - `pub struct SoundRule { pub kind: RuleKind, pub param: u32 }` — `param` selects the rule's drawn variant (e.g. which vowel-shift direction); the full rule family is this closed enum, nothing extensible
  - `pub struct Cascade { pub rules: Vec<SoundRule> }` — 2–4 rules, drawn
  - `pub fn draw_cascade(seed: &Seed, species: &str) -> Cascade` — label `language/<species>/lexicon/cascade`
  - `pub struct AppliedRule { pub rule: SoundRule, pub changed: bool }`
  - `pub struct Derivation { pub proto: Vec<Segment>, pub steps: Vec<AppliedRule>, pub modern: Vec<Segment> }`
  - `pub fn proto_root(seed: &Seed, species: &str, concept: &str, ph: &Phonology) -> Vec<Segment>` — label `language/<species>/lexicon/root/<concept>`; drawn from `ph`'s phonotactic templates (the same stem machinery names use — 1–2 syllables)
  - `pub fn evolve(proto: &[Segment], cascade: &Cascade, ph: &Phonology) -> Derivation` — **pure**; applies each rule uniformly wherever its conditioning environment occurs; **a rule whose output segment is not in `ph`'s inventory applies as identity** (the codomain constraint made mechanical)

Rule semantics (each a total function on segments, one line each in the docs): Lenition — voiceless stop → voiced counterpart; Fortition — fricative → stop at same place; VowelShift — vowel height raised (param 0) or lowered (param 1) one step; ClusterSimplify — two-consonant onset drops its first segment; FinalLoss — word-final coda consonant drops.

- [ ] **Step 1: Failing tests**

```rust
#[test]
fn evolve_is_regular() {
    // Neogrammarian: every segment matching the environment changes; replaying
    // evolve on the recorded proto reproduces the recorded modern exactly.
    let ph = test_phonology(); // helper: draw under a permissive envelope, fixed seed
    let cascade = Cascade { rules: vec![SoundRule { kind: RuleKind::Lenition, param: 0 }] };
    let d = evolve(&proto_with_two_voiceless_stops(), &cascade, &ph);
    let voiceless_left = d.modern.iter().filter(|s| is_voiceless_stop(s)).count();
    assert_eq!(voiceless_left, 0, "lenition must hit every voiceless stop or none");
}

#[test]
fn evolve_lands_in_inventory() {
    // Codomain: every modern segment is in the phonology's inventory.
}

#[test]
fn derivation_replays() {
    // evolve(d.proto, cascade, ph).modern == d.modern — byte-stable replay.
}

#[test]
fn proto_root_is_pure() {
    // Same (seed, species, concept) → identical segments, twice.
}
```

- [ ] **Step 2: Verify failures; implement; verify passes**

Run: `cargo test -p hornvale-language etymology`

- [ ] **Step 3: Full gate, commit**

```bash
git add domains/language
git commit -m "feat(language): etymology engine — proto-roots, drawn cascade, Neogrammarian evolve"
```

### Task 7: The lexicon — assembly, compounds, headedness, word views

**Files:**
- Create: `domains/language/src/lexicon.rs`
- Modify: `domains/language/src/lib.rs` (module + re-exports + extend `stream_labels()` with the three new label patterns and the `/v2` name labels, keeping the v1 labels documented "(retired at The Words)")
- Test: in-module

**Interfaces:**
- Consumes: Tasks 5–6.
- Produces:
  - `pub enum ExposureClass { Steeped, KnowsOf, Unknown { reason: GapReason } }`
  - `pub enum GapReason { Experiential(String), Perceptual(String) }` — the recountable reason, composed upstream ("no settlement in or beside coastal cells"; "hue ladder depth 3 from night-vision 0.8")
  - `pub enum Headedness { HeadFirst, HeadLast }`; `pub fn draw_headedness(seed: &Seed, species: &str) -> Headedness` — label `language/<species>/lexicon/headedness`
  - `pub struct WordViews { pub roman: String, pub ipa: String, pub espeak: String }` — built by the shared rendering `naming.rs` exposes (Task 6 step)
  - `pub enum LexEntry { Root { derivation: Derivation, views: WordViews }, Compound { modifier: String, head: String, views: WordViews }, Gap { reason: GapReason } }` (concept-id strings for modifier/head)
  - `pub struct Lexicon { pub species: String, pub headedness: Headedness, entries: BTreeMap<String, LexEntry> }` with `pub fn entry(&self, concept: &str) -> Option<&LexEntry>` and `pub fn entries(&self) -> impl Iterator<Item = (&str, &LexEntry)>`
  - `pub fn build_lexicon(seed: &Seed, species: &str, ph: &Phonology, exposures: &BTreeMap<String, ExposureClass>) -> Lexicon` — Steeped → `proto_root` + `evolve` → Root; KnowsOf → `compound_recipe` over the two component roots (which must be Steeped — assemble roots first, then compounds; a KnowsOf concept whose recipe components aren't held falls back to Gap with the experiential reason), joined per headedness; Unknown → Gap

- [ ] **Step 1: Failing tests** — exactly-one-of ("every input concept yields exactly one entry"), compound composition ("`sea` at KnowsOf with `water`/`many` steeped → Compound whose roman contains both component romans in headedness order"), fallback ("KnowsOf with an un-held component → Gap, experiential"), purity ("same inputs → identical lexicon, twice"), headedness order flip test.

- [ ] **Step 2: Verify failures; implement; verify passes**

Run: `cargo test -p hornvale-language lexicon`

- [ ] **Step 3: Full gate, commit**

```bash
git add domains/language
git commit -m "feat(language): lexicon assembly — roots, recipe compounds under drawn headedness, reasoned gaps"
```

### Task 8: Exposure at the composition root

**Files:**
- Modify: `windows/worldgen/src/lib.rs` — new pub fns beside `language_of` (~line 510)
- Test: `windows/worldgen/tests/` (follow the existing test-file pattern there; `grep -rn "fn language_of" windows/worldgen/tests` to find the convention)

**Interfaces:**
- Consumes: Tasks 5–7; existing `climate_of`, `terrain_of`, settlement placements, `hornvale_species::PerceptionVector`.
- Produces:
  - `pub fn pack_depths(p: &hornvale_species::PerceptionVector) -> hornvale_language::PackDepths` — authored mapping, doc-commented as the model card: `hue = 2 + ((1.0 - p.night_vision) * 3.0).round() as u8` (goblin 0.5 → 4; kobold 0.8 → 3 — kobolds stop before `blue`), `luminance = if p.night_vision > 0.6 { 3 } else { 1 }`
  - `pub fn exposure_of(world: &World, species: &str) -> Result<BTreeMap<String, hornvale_language::ExposureClass>, BuildError>` — Steeped: universal stratum + in-ladder pack entries + biome concepts of the species' settled cells + `sun`/`moon`/`star`/`night` (everyone's sky) + own social/living kinds; KnowsOf: biome concepts of cells adjacent to settled cells + `sea` if any settled cell lies within 2 cells of a below-sea-level cell; Unknown: everything else, with the experiential reason naming the missing fact ("no settlement in or beside <biome>") — perceptual reasons come from ladder exclusion ("hue rank <r> exceeds depth <d> from night-vision <v>")
  - `pub fn lexicon_of(world: &World, species: &str) -> Result<hornvale_language::Lexicon, BuildError>` — glue: `language_of` + `exposure_of` + `build_lexicon` (the re-derivation path surfaces use; nothing persisted)

- [ ] **Step 1: Failing tests** — "goblin lexicon has a Root for `water` (universal)"; "kobold `blue` is a Perceptual gap and goblin `blue` is not" (perception vectors differ — if both species' night-vision sit on the same side of the mapping, adjust the *test* to the roster's real vectors, never the mapping to the test); "every Unknown entry's reason is non-empty"; "exposure_of is pure across two calls".

- [ ] **Step 2: Verify failures; implement; verify passes**

Run: `cargo test -p hornvale-worldgen exposure`

- [ ] **Step 3: Full gate, commit**

```bash
git add windows/worldgen
git commit -m "feat(worldgen): exposure derivation and lexicon_of — the composition root wires the words"
```

### Task 9: Glossed names at `/v2`

**Files:**
- Modify: `domains/language/src/naming.rs` — `Namer::glossed_name`
- Modify: `windows/worldgen/src/lib.rs` — settlement naming (~line 745), `LanguageDeityNamer` (~line 569), commit `name-gloss` facts; register the `name-gloss` predicate where worldgen registers its predicates
- Test: in-module (naming) + worldgen integration test

**Interfaces:**
- Consumes: Tasks 7–8.
- Produces:
  - `pub struct SiteConcepts<'a> { pub concepts: &'a [&'a str] }` — the entity's own facts as concept ids, composed upstream (settlement: its cell's biome concept + its people's presiding-belief phenomenon concept; deity: its phenomenon concept + its sentiment quality)
  - `Namer::glossed_name(&self, kind: NameKind, salt: u64, morph: &MorphOptions, site: &SiteConcepts, lexicon: &Lexicon) -> (GeneratedName, String)` — derive path gains a `"v2"` leg after the kind label (`…derive("name").derive(kind.label()).derive("v2").derive(&salt…)`); the stream picks 1–2 site concepts that hold Root/Compound entries, compounds their word segments per the lexicon's headedness, applies the kind's existing morphology (epithet honorifics preserved — status-basis keying intact); returns the three-view name plus the gloss string ("ice-home"); if *no* site concept holds a word, fall back to the v1 stem draw *within the v2 stream* (the name is still a pure `(seed, species, kind, v2, salt)` function)
  - worldgen: `name-gloss` fact (functional, doc "the glossed meaning of an entity's generated name") committed beside each `name` fact when a gloss exists

- [ ] **Step 1: Failing tests** — pin-isolation-by-construction ("same (seed, species, kind, salt, site, lexicon) → identical name, independent of any other entity"); gloss truthfulness at unit level ("the returned gloss's concept ids ⊆ site concepts"); v2 independence ("glossed_name ≠ the v1 `name` output for the same salt — the epoch actually moved"); honorific preservation ("Rank morph still prefixes epithets").

- [ ] **Step 2: Verify failures; implement `glossed_name`; verify passes**

Run: `cargo test -p hornvale-language glossed`

- [ ] **Step 3: Wire worldgen** — settlement site concepts, deity site concepts, `name-gloss` facts; run `cargo test -p hornvale-worldgen`.

- [ ] **Step 4: Full gate, commit**

```bash
git add domains/language windows/worldgen
git commit -m "feat(language,worldgen): glossed names at the /v2 epoch — compounds over site facts"
```

### Task 10: The keystone — structural invariants

**Files:**
- Create: `cli/tests/words_identity.rs`

**Interfaces:**
- Consumes: Task 2's fixtures; everything above.

- [ ] **Step 1: Write the suite** (each its own `#[test]`)

1. `entity_graph_unchanged` — build seed 42; the set of entity ids and every fact whose predicate is not in `{name, name-gloss, deity-name, deity-name-ipa, deity-epithet, deity-epithet-ipa}` equals the pre-Words fixture's.
2. `names_wellformed_and_glosses_true` — for every entity with a `name-gloss` fact: the gloss's concept ids each appear among *that entity's own* site facts (re-derive site concepts the same way worldgen does).
3. `every_concept_resolves_once` — for each species and every registered concept: exactly one of Root / Compound / Gap.
4. `derivations_replay` — for every Root entry: `evolve(proto, cascade, ph).modern == modern`.
5. `gaps_have_reasons` — every Gap's reason string is non-empty and names either a fact or a vector dimension.
6. `pin_isolation_holds` — `--species goblin`-pinned world's goblin names identical to the unpinned world's (extend the existing pin test pattern from `genesis_properties.rs` / the Tongues suite).
7. `determinism_holds` — build seed 42 twice; worlds byte-identical.

- [ ] **Step 2: Run the suite**

Run: `cargo test -p hornvale --test words_identity`
Expected: PASS (this is the campaign's definition of "didn't break the world").

- [ ] **Step 3: Full gate, commit**

```bash
git add cli/tests/words_identity.rs
git commit -m "test(keystone): The Words structural invariants — graph, glosses, derivations, pins"
```

### Task 11: Surfaces — `dictionary` verb, REPL `word`, `why` recounting

**Files:**
- Modify: `cli/src/` — new `dictionary` verb (follow the `concepts`/`streams` verb pattern; find the dispatch: `grep -rn "\"concepts\"" cli/src`); REPL `word` verb (find the REPL command table: `grep -rn "\"phonology\"" cli/src`); `why` extension for `name-gloss`
- Create: `book/src/reference/dictionary-generated.md` (committed artifact)
- Modify: `book/src/SUMMARY.md` (add the dictionary page beside phonology); `.github/workflows/ci.yml` "Artifacts are current" step (add the dictionary regen line)

**Interfaces:**
- Consumes: `lexicon_of` (Task 8).
- Produces: `hornvale dictionary --world <path>` → markdown: per species, a table (concept | gloss | word (roman) | IPA | proto | derivation one-liner | or gap + reason); REPL `word <concept>` → both species' entries or reasoned gaps; `why <name>` → appends "named for: <gloss> (<site facts>)" when a `name-gloss` fact exists.

- [ ] **Step 1: Failing test for the verb** (CLI tests exist — follow their pattern: run the binary, assert on stdout structure: header row present, one row per registered concept per species).

- [ ] **Step 2: Implement verb + REPL + `why`; verify**

Run: `cargo test -p hornvale dictionary`

- [ ] **Step 3: Generate and commit the artifact; extend CI**

```bash
cargo run -q -p hornvale -- new --seed 42 --out /tmp/hv-words.json
cargo run -q -p hornvale -- dictionary --world /tmp/hv-words.json > book/src/reference/dictionary-generated.md
```
Add the same two lines to the CI artifact step. `mdbook build book` must pass.

- [ ] **Step 4: Full gate, commit**

```bash
git add cli/ book/ .github/workflows/ci.yml
git commit -m "feat(cli): dictionary verb, REPL word, why recounts name-glosses"
```

### Task 12: The Lab — preregister, instrument, run, pin

**Files:**
- Create: `studies/census-of-words.study.json` (follow `studies/census-of-tongues.study.json` shape)
- Modify: `windows/lab/src/` metrics (find the metric registry: `grep -rn "list-metrics" windows/lab/src`)
- Create: `book/src/laboratory/study-010.md` (study 009 is the latest; verify with `ls book/src/laboratory/` at execution time and take the next free number)

**Interfaces:**
- Consumes: everything above.
- Produces: metric columns — `name-gloss-true` (per settlement, bool: re-derived site concepts contain the gloss's), `lexicon-regular` (per species, bool: all derivations replay), `exposure-sound` (per species, bool: no Root at Unknown, every Gap reasoned — re-derived from the ledger independently of the lexicon pipeline), `hue-depth` (per species, u8), `name-collision-rate` (per world, f64).

- [ ] **Step 1: Preregister** — write the study JSON *and* the book page's claims section BEFORE running: (a) `name-gloss-true` 100% row-by-row; (b) `lexicon-regular` and `exposure-sound` true for both species; (c) goblin `hue-depth` > kobold `hue-depth`; (d) collision rate reported and pinned after measurement (directional claim only: below the Tongues-era pinned rate ×2). Commit the preregistration alone:

```bash
git add studies/census-of-words.study.json book/src/laboratory/study-010.md
git commit -m "lab(prereg): The Words baseline — claims before census (ADR 0016)"
```

- [ ] **Step 2: Implement the metrics; run the study**

```bash
cargo run -q -p hornvale -- lab run studies/census-of-words.study.json
```
Expected: all preregistered claims confirmed; pin exact rates into the study page.

- [ ] **Step 3: Full gate, commit**

```bash
git add windows/lab studies/ book/src/laboratory/
git commit -m "lab: The Words baseline confirmed — glosses true, cascades regular, exposure sound"
```

### Task 13: Re-baseline, book close, registry flip

**Files:**
- Modify: every committed artifact (three seed-42 almanacs, elevation map if names appear, `book/src/gallery/*`, reference dumps, lab studies), per the CI artifact-step command list in `.github/workflows/ci.yml`
- Modify: `book/src/domains/language.md` (freshness), `book/src/domains/settlement.md`, `book/src/domains/religion.md` (glossed-name mentions), `book/src/chronicle/` (new entry, follow `16-the-tongues.md`'s form)
- Create: `docs/retrospectives/2026-07-XX-the-words.md (dated at close)` (decision 0020, process lessons only)
- Modify: `docs/vision/idea-registry.md` — LANG-4 `spec'd → shipped`, Where stays the spec; annotate MAP-3 (own-line sound change shipped ahead of the tree) and LANG-3 (concept layer now exists)

**Interfaces:**
- Consumes: everything.

- [ ] **Step 1: Regenerate every artifact** — run the full CI artifact command list verbatim (the authoritative list is the "Artifacts are current" step), then `git diff --stat` and review: only proper nouns, glosses, dictionary, and study outputs may differ.

- [ ] **Step 2: Freshness sweep** — grep the book for `"names have shape"` / `"not meaning"` / stale Tongues scope lines; update each. `mdbook build book` clean.

- [ ] **Step 3: Chronicle + retrospective + registry** — write the chronicle entry at book altitude; the retrospective per decision 0020; flip LANG-4. Run `cargo test -p hornvale --test docs_consistency`.

- [ ] **Step 4: Final full gate, commit, merge prep**

```bash
cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings
git add -A
git commit -m "docs(close): The Words — re-baseline, chronicle, freshness sweep, LANG-4 shipped"
```

Then invoke `superpowers:finishing-a-development-branch`.

---

## Self-Review Notes

- **Spec coverage:** §3 → Tasks 3–4; §4 → Task 5; §5 → Tasks 6–7; §6 → Task 8; §7 → Task 9; §8 → Tasks 2, 10; §9 → Task 12; §10 → Task 11; §11 → Tasks 1, 13; §12–13 are gates and scope lines enforced throughout.
- **Type consistency check done:** `ExposureClass`/`GapReason` (7) consumed by 8; `PackDepths` (5) produced by `pack_depths` (8); `Lexicon`/`LexEntry` (7) consumed by 9 and 11; `SiteConcepts` (9) re-derived in 10.2 and 12's `name-gloss-true`.
- **Known judgment calls:** the lexicon is derived, never persisted (matches phonology precedent); `name-gloss` is the only new committed fact kind; recipe fallback to Gap keeps `build_lexicon` total; hue-depth mapping constants live in worldgen with the model card, adjustable by Nathan without touching the engine.
