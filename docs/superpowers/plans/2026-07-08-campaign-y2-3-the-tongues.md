# Campaign Y2-3: The Tongues — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give each species its own voice — a feature-bearing phoneme model and generative phonology (`domains/language`), an authored articulation envelope as the species crate's third vector, generative naming grammars, and register built behind a permanent content→render seam — deleting every English-template proper noun and vocabulary/syllable stopgap.

**Architecture:** A kernel-only `domains/language` crate holds the phoneme model (segments as articulatory feature-bundles; romanization and IPA are views), a per-species phonology drawn under the articulation envelope, generative naming grammars, and a register renderer. Religion stops building tenet prose inline and commits structured belief *content*; the renderer (in `domains/language`, behind `LineContent`/`VoiceParams` input structs) voices it. The composition root builds each language, generates all names, and renders tenets. Byte-identity is replaced by structural invariants (entity structure, phonotactic validity, uniqueness, pin-isolation) because every proper noun regenerates.

**Tech Stack:** Rust edition 2024, `serde`/`serde_json` only, mdbook.

**Spec:** `docs/superpowers/specs/2026-07-08-campaign-y2-3-the-tongues-design.md` (governs; the Year-2 metaplan §7 sits above it).

## Global Constraints

- Full gate on every commit: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`; run `cargo fmt` as the final step before every commit.
- Determinism constitutional: no wall-clock, no `HashMap`/`HashSet` (`BTreeMap`/`BTreeSet`/`Vec` only), float sorts via `total_cmp` with deterministic tie-breaks. Randomness only from the kernel `Seed`/`Stream` (`derive(label)`, `stream()`, `range_u32(lo,hi)` inclusive, `next_f64()` in [0,1), `next_u64()`, `pick(&[T]) -> Option<&T>`).
- `domains/language` depends on `hornvale-kernel` and NOTHING else (`cli/tests/architecture.rs` enforces; `domains/*` glob auto-includes it). It never imports religion/culture/species — it defines its own input structs the composition root populates (the `SocietySummary`/`PsychSummary` pattern).
- No new external dependencies. `#![warn(missing_docs)]` on the new crate; every public item, field, and variant gets a one-line doc comment.
- **Spellings are views:** a `Segment` is the truth; `romanize()` and `ipa()` derive surface forms and are never persisted.
- **The content→render seam is permanent:** `render_line(content: &LineContent, voice: &VoiceParams) -> String` is the interface the future grammar reoccupies. v1 fills it with templates. Never post-process finished English strings.
- **The envelope is authored; the phonology is drawn.** Model-card: every articulation dimension declared authored; inventory/phonotactics declared drawn.
- Goblin is the baseline: articulation scalars 0.5, `exotic manner = None`; every derivation (voice params, morphology, inventory bias) is identity/neutral at the goblin vector.
- **Byte-identity does NOT hold this campaign.** The keystone is structural invariants (Task 2, Task 11), not a byte-for-byte fixture. Determinism (same seed → identical output across runs) still holds and is tested.
- Stream labels are permanent (ADR 0006): settlement-name generation moves out of `settlement/*` entirely — names now derive under `hornvale-language`'s own `language/<species>/name/...` labels (the language engine draws the name, not settlement). The old `settlement/name` and `settlement/kobold/name` labels are never renamed and stay documented as retired; no phantom `settlement/name/v2` is minted (nothing under `settlement/*` derives a name any longer). The `name` predicate keeps its identity; the `tenet` predicate is never renamed (old saves keep loading). **[Design correction, post-Task-9 review — supersedes the original `settlement/name/v2` plan below.]**
- Preregistration discipline (ADR 0016): directional calibration claims are written before the census runs; exact rates pinned after measurement, never tuned to pass.
- Evidence discipline (campaign-standing, per the Y2-1 & Y2-2 retrospectives): implementer reports carry verbatim command transcripts; gate claims are void unless the controller independently reruns the gate. A plan-authored test constant or code sketch that a reviewer flags gets fixed with mutation-test evidence, not defended.
- Work on branch `campaign-y2-3-the-tongues` (worktree per `superpowers:using-git-worktrees`).

---

### Task 1: The language book chapter (book-driven development — no code)

**Files:**
- Create: `book/src/domains/language.md`
- Modify: `book/src/SUMMARY.md` (add `- [Language](./domains/language.md)` directly after the Perception chapter)

**Interfaces:**
- Consumes: the spec (read in full) and the voice/altitude of `book/src/domains/perception.md` (read it first — match its register).
- Produces: the chapter later tasks must live up to; Task 14 revisits it in the freshness sweep.

- [ ] **Step 1: Write the chapter**

Content (prose at book altitude — technical and mathematical, comprehensible without the code): why language is Year 2's third substrate (psychology gave minds, perception gave eyes, articulation gives mouths); **phonemes are feature-bearing segments and spellings are views** (romanization for the almanac, IPA for the book — two renderings of one truth); the articulation vector and its closed-ness, with the model card — a table of all six dimensions × (goblin, kobold), every dimension **authored**, each kobold value with its rationale, and **voice-loudness explicitly banked to derive from a body/frailty vector (BIO-1) later** (the nocturnality-banking precedent), including the voice-loudness × exotic-manner interaction ("kobolds can trill, but their names hiss"); the **authored-envelope / drawn-phonology** split; the naming grammars (stem + compounding/reduplication/honorific affixation) and the status-basis keying (a `Rank` society's gods carry honorifics a flat society's don't); the **content→render seam** and register, framed honestly as *v1 is the simple template renderer behind a permanent interface the future oral-formulaic grammar reoccupies* — persist meaning, render surface, the same "spellings are views" principle one level up; the bright scope line (names have shape and voice, not meaning; no lexicon/syntax/sound-change).

- [ ] **Step 2: Build and verify**

Run: `mdbook build book`
Expected: clean build; chapter reachable from SUMMARY.

- [ ] **Step 3: Commit**

```bash
git add book/src/domains/language.md book/src/SUMMARY.md
git commit -m "docs(book): language chapter opens Campaign Y2-3 (book-driven development)"
```

### Task 2: Commit the pre-Tongues fixture + the structural-invariant harness

Byte-identity does not hold, so this fixture is NOT for byte-comparison. It freezes the pre-Tongues seed-42 world so Task 11's keystone can assert the *structural* invariant: entity ids and all non-linguistic facts unchanged, only name text and tenet→structured-content differ.

**Files:**
- Create: `cli/tests/fixtures/pre-tongues-seed-42-world.json`
- Create: `cli/tests/fixtures/pre-tongues-seed-42-almanac.md`

**Interfaces:**
- Produces: the two fixtures Task 11's `tongues_identity.rs` reads.

- [ ] **Step 1: Rebuild, then generate the fixtures (stdout only — no `2>&1`, the Y2-2 lesson)**

```bash
cargo build -p hornvale
cargo run -q -p hornvale -- new --seed 42 --out cli/tests/fixtures/pre-tongues-seed-42-world.json
cargo run -q -p hornvale -- almanac --world cli/tests/fixtures/pre-tongues-seed-42-world.json > cli/tests/fixtures/pre-tongues-seed-42-almanac.md
```

- [ ] **Step 2: Verify pristine**

Run: `head -1 cli/tests/fixtures/pre-tongues-seed-42-almanac.md` (must be `# The Almanac of Seed 42`) and `grep -c "Finished\|Running" cli/tests/fixtures/pre-tongues-seed-42-*.md` (must be 0).

- [ ] **Step 3: Commit**

```bash
git add cli/tests/fixtures/pre-tongues-seed-42-*
git commit -m "test(fixtures): freeze pre-Tongues seed-42 outputs for the structural-invariant keystone"
```

### Task 3: The phoneme model in `domains/language`

**Files:**
- Create: `domains/language/Cargo.toml`
- Create: `domains/language/src/lib.rs`
- Create: `domains/language/src/phoneme.rs`

**Interfaces:**
- Consumes: kernel only.
- Produces:
  - `pub enum Place { Labial, Alveolar, Postalveolar, Velar, Uvular, Glottal }` (`Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord`)
  - `pub enum Manner { Stop, Fricative, Sibilant, Nasal, Trill, Click, Ejective, Approximant }` (same derives)
  - `pub enum Height { High, Mid, Low }`, `pub enum Backness { Front, Central, Back }` (same derives)
  - `pub enum Segment { Consonant { place: Place, manner: Manner, voiced: bool }, Vowel { height: Height, backness: Backness, rounded: bool } }` (same derives)
  - `pub fn romanize(seg: &Segment) -> &'static str`
  - `pub fn ipa(seg: &Segment) -> &'static str`
  - `pub fn sonority(seg: &Segment) -> u8` — a 0–5 sonority rank (Stop 0 … Vowel 5), used by the loudness bias and phonotactics.

- [ ] **Step 1: Cargo.toml** (mirror `domains/species/Cargo.toml`)

```toml
[package]
name = "hornvale-language"
version = "0.1.0"
edition.workspace = true
license.workspace = true
description = "Hornvale language domain: feature-bearing phonemes, generative phonology, naming, and register."

[dependencies]
hornvale-kernel = { path = "../../kernel" }
```

- [ ] **Step 2: `lib.rs` module wiring**

```rust
//! Language, tier 1: a feature-bearing phoneme model (spellings are views),
//! a per-species phonology drawn under an authored articulation envelope,
//! generative naming grammars, and a register renderer behind a permanent
//! content→render seam. Kernel-only; it defines its own input structs the
//! composition root populates and never imports another domain.
#![warn(missing_docs)]

/// The phoneme model: segments as articulatory feature-bundles.
pub mod phoneme;

pub use phoneme::{Backness, Height, Manner, Place, Segment, ipa, romanize, sonority};
```

- [ ] **Step 3: Write failing tests** (in `phoneme.rs`)

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn romanization_and_ipa_are_distinct_views_of_one_segment() {
        let s = Segment::Consonant { place: Place::Alveolar, manner: Manner::Sibilant, voiced: false };
        assert_eq!(romanize(&s), "s");
        assert_eq!(ipa(&s), "s");
        let sh = Segment::Consonant { place: Place::Postalveolar, manner: Manner::Sibilant, voiced: false };
        assert_eq!(romanize(&sh), "sh");
        assert_eq!(ipa(&sh), "ʃ");
    }

    #[test]
    fn sonority_orders_stop_below_vowel() {
        let stop = Segment::Consonant { place: Place::Velar, manner: Manner::Stop, voiced: false };
        let vowel = Segment::Vowel { height: Height::Low, backness: Backness::Central, rounded: false };
        assert!(sonority(&stop) < sonority(&vowel));
    }

    #[test]
    fn a_trill_is_more_sonorous_than_a_sibilant() {
        let trill = Segment::Consonant { place: Place::Alveolar, manner: Manner::Trill, voiced: true };
        let sib = Segment::Consonant { place: Place::Alveolar, manner: Manner::Sibilant, voiced: false };
        assert!(sonority(&trill) > sonority(&sib), "the loudness bias depends on this ordering");
    }
}
```

- [ ] **Step 4: Run to verify fail**

Run: `cargo test -p hornvale-language 2>&1 | tail -10`
Expected: compile errors (types undefined).

- [ ] **Step 5: Implement `phoneme.rs`**

Define the enums with the exact derives from Interfaces. Implement `romanize`/`ipa` as exhaustive matches over a small curated segment set (the segments the engine can draw — keep it to a practical inventory: stops p/b t/d k/g q, fricatives f/v s/z x, sibilants s/z ʃ/ʒ, nasals m n ŋ, trill r, click ǃ, ejective kʼ, approximants l j w, vowels i/e/a/o/u with rounding). Where a feature-bundle has no standard glyph, romanize to an ASCII digraph and ipa to the nearest IPA glyph (document the approximation). `sonority`: Stop/Ejective/Click 0, Fricative/Sibilant 1, Nasal 2, Trill 3, Approximant 4, Vowel 5 — the ordering the tests pin.

- [ ] **Step 6: Register the crate** — add `hornvale-language` to `windows/worldgen/Cargo.toml` dependencies (it will consume it) and confirm the workspace `members` glob covers `domains/*` (it does). Run full gate.

Run: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: green (the new crate compiles, architecture test still passes — language depends only on kernel).

- [ ] **Step 7: Commit**

```bash
git add -A
git commit -m "feat(language): feature-bearing phoneme model — segments, romanization/IPA views, sonority"
```

### Task 4: The articulation vector in the species crate

**Files:**
- Modify: `domains/species/src/lib.rs`
- Modify: `cli/tests/species_identity.rs` (NEW_PREDICATES grows by the new articulation predicates)

**Interfaces:**
- Consumes: kernel; `hornvale_language::{Place, Manner}` for the exotic-manner mapping is NOT imported here (species stays kernel-only — the envelope is abstract dimensions, not language types).
- Produces:
  - `pub enum ExoticManner { None, Trill, Click, Ejective }` (`Clone, Copy, Debug, PartialEq, Eq`)
  - `pub struct ArticulationVector { pub labiality: f64, pub vowel_space: f64, pub voicing: f64, pub sibilance: f64, pub voice_loudness: f64, pub exotic: ExoticManner }` (`Clone, Copy, Debug, PartialEq`)
  - `SpeciesDef { …, pub articulation: ArticulationVector }`
  - predicates `SPECIES_LABIALITY`, `SPECIES_VOWEL_SPACE`, `SPECIES_VOICING`, `SPECIES_SIBILANCE`, `SPECIES_VOICE_LOUDNESS` (Number), `SPECIES_EXOTIC_MANNER` (Text)

- [ ] **Step 1: Failing tests** (in `domains/species/src/lib.rs` tests)

```rust
#[test]
fn goblin_articulation_is_baseline_kobold_hisses_and_is_quiet() {
    let reg = registry();
    let g = &reg["goblin"].articulation;
    assert_eq!(g.labiality, 0.5);
    assert_eq!(g.voice_loudness, 0.5);
    assert_eq!(g.exotic, ExoticManner::None);
    let k = &reg["kobold"].articulation;
    assert!(k.sibilance > 0.5 && k.labiality < 0.5 && k.voice_loudness < 0.5);
    assert_eq!(k.exotic, ExoticManner::Trill);
}

#[test]
fn genesis_commits_articulation_facts() {
    let mut w = World::new(Seed(42));
    register_concepts(&mut w.registry).unwrap();
    let ids = genesis(&mut w).unwrap();
    let k = ids["kobold"];
    assert_eq!(w.ledger.text_of(k, SPECIES_EXOTIC_MANNER), Some("trill"));
    assert!(matches!(w.ledger.value_of(k, SPECIES_SIBILANCE), Some(Value::Number(n)) if *n > 0.5));
}
```

- [ ] **Step 2: Run to verify fail** — `cargo test -p hornvale-species 2>&1 | tail -10` (compile errors).

- [ ] **Step 3: Implement.** Add `ExoticManner`, `ArticulationVector` (doc every field), the `articulation` field on `SpeciesDef`, the five Number + one Text predicates registered in `register_concepts`, and the fact commits in `genesis` (after the perception facts), serializing exotic manner lowercase (`none`/`trill`/`click`/`ejective`). Registry values — goblin: `{0.5, 0.5, 0.5, 0.5, 0.5, None}`; kobold: `{labiality 0.1, vowel_space 0.3, voicing 0.6, sibilance 0.9, voice_loudness 0.2, Trill}`. Extend `NEW_PREDICATES` in `cli/tests/species_identity.rs` by the six new constants (this preserves the Y2-1 superset test since new facts land only under new predicates).

- [ ] **Step 4: Full gate** — `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`. Expected: green; `species_identity` passes.

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "feat(species): articulation vector — the third closed vector, committed as facts"
```

### Task 5: The phonology engine — drawn inventory + phonotactics under the envelope

**Files:**
- Create: `domains/language/src/phonology.rs`
- Modify: `domains/language/src/lib.rs` (module + re-exports)

**Interfaces:**
- Consumes: `Segment`/`Place`/`Manner`/`sonority` (Task 3); the kernel `Seed`.
- Produces:
  - `pub struct Envelope { pub labiality: f64, pub vowel_space: f64, pub voicing: f64, pub sibilance: f64, pub voice_loudness: f64, pub exotic: ExoticSeg }` where `pub enum ExoticSeg { None, Trill, Click, Ejective }` — language's OWN copy of the envelope (populated by the composition root from the species `ArticulationVector`; language does not import species). Doc: "the articulation envelope as language consumes it".
  - `pub struct Phonology { pub inventory: Vec<Segment>, pub onsets: Vec<Vec<Manner>>, pub nuclei: usize, pub codas: Vec<Vec<Manner>> }` (fields doc'd; a syllable template is manner-slots, filled from the inventory at name time).
  - `pub fn draw_phonology(seed: &Seed, species: &str, env: &Envelope) -> Phonology` — draws the inventory (a subset of envelope-permitted segments, high-sonority segments down-weighted when `voice_loudness` is low) and phonotactic templates from `language/<species>/phonology/...` labels.
  - `pub fn permits(env: &Envelope, seg: &Segment) -> bool` — the envelope filter (a labial when `labiality < threshold` is forbidden; the exotic manner present only if it matches `env.exotic`; etc.).

- [ ] **Step 1: Failing tests** (in `phonology.rs`)

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Seed;

    fn goblin_env() -> Envelope {
        Envelope { labiality: 0.5, vowel_space: 0.5, voicing: 0.5, sibilance: 0.5, voice_loudness: 0.5, exotic: ExoticSeg::None }
    }
    fn kobold_env() -> Envelope {
        Envelope { labiality: 0.1, vowel_space: 0.3, voicing: 0.6, sibilance: 0.9, voice_loudness: 0.2, exotic: ExoticSeg::Trill }
    }

    #[test]
    fn envelope_forbids_labials_for_a_low_labiality_species() {
        let bilabial = Segment::Consonant { place: Place::Labial, manner: Manner::Stop, voiced: false };
        assert!(!permits(&kobold_env(), &bilabial));
        assert!(permits(&goblin_env(), &bilabial));
    }

    #[test]
    fn a_quiet_species_admits_its_trill_rarely_or_not_at_all() {
        // Kobold is Trill-capable but low-loudness: the drawn inventory should
        // contain few/no trills relative to a loud species with the same manner.
        let quiet = draw_phonology(&Seed(42), "kobold", &kobold_env());
        let trills = quiet.inventory.iter()
            .filter(|s| matches!(s, Segment::Consonant { manner: Manner::Trill, .. })).count();
        let mut loud = kobold_env();
        loud.voice_loudness = 0.9;
        let loud_ph = draw_phonology(&Seed(42), "kobold", &loud);
        let loud_trills = loud_ph.inventory.iter()
            .filter(|s| matches!(s, Segment::Consonant { manner: Manner::Trill, .. })).count();
        assert!(trills <= loud_trills, "low loudness must not admit MORE trills than high loudness");
    }

    #[test]
    fn draw_is_deterministic() {
        let a = draw_phonology(&Seed(7), "kobold", &kobold_env());
        let b = draw_phonology(&Seed(7), "kobold", &kobold_env());
        assert_eq!(a.inventory, b.inventory);
        assert_eq!(a.onsets, b.onsets);
    }

    #[test]
    fn inventory_respects_the_envelope() {
        let ph = draw_phonology(&Seed(3), "kobold", &kobold_env());
        assert!(ph.inventory.iter().all(|s| permits(&kobold_env(), s)));
        assert!(!ph.inventory.is_empty());
    }
}
```

- [ ] **Step 2: Run to verify fail** — compile errors.

- [ ] **Step 3: Implement `phonology.rs`.**
  - `permits`: a segment passes if (a) it is not a labial when `env.labiality < 0.3`; (b) it is not a vowel outside the `vowel_space`-sized band; (c) if its manner is exotic (Trill/Click/Ejective) it matches `env.exotic`; (d) voiced segments require `env.voicing > 0.2`. (Thresholds are constants; document them.)
  - `draw_phonology`: start from a fixed canonical segment set (defined in `phoneme.rs` as `pub(crate) fn canonical_segments() -> Vec<Segment>`), filter by `permits`, then draw an inventory by iterating candidates in a fixed order and keeping each with a probability that (i) rises with `sibilance` for sibilants, (ii) **falls with `sonority(seg)` when `voice_loudness` is low** — concretely keep-probability `= base − loudness_penalty·sonority·(1 − voice_loudness)` clamped, drawn against `stream.next_f64()`. Always keep at least the vowels the `vowel_space` allows and a minimum consonant set so names are constructible. Draw 2–3 onset templates, a nucleus count, 1–2 coda templates from `manner`-slot patterns. All draws from `seed.derive("language").derive(species).derive("phonology").derive("inventory"|"phonotactics").stream()`.
  - Re-export `Envelope`, `ExoticSeg`, `Phonology`, `draw_phonology`, `permits` from `lib.rs`.

- [ ] **Step 4: Full gate.** Expected green.

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "feat(language): per-species phonology drawn under the articulation envelope"
```

### Task 6: The naming grammars — stems, morphology, psychology keying

> **Design correction (post-Task-9 review, owner decision):** the uniqueness
> re-draw sketched below was **removed**. `Namer::name` takes no `used` set
> and performs no re-draw — a name is a single deterministic draw, a pure
> function of `(seed, species, kind, salt)`. Rationale: a re-draw makes a name
> depend on which *other* settlements a world places, which breaks
> pin-isolation (species displace one another during spacing, so a shared-cell
> goblin name could re-draw differently pinned vs. unpinned). The vast
> phonology name space makes re-draw unnecessary; uniqueness is de-facto and
> measured as a calibration (Task 12). The sketches below retain their
> original wording for the historical record; the shipped `name` signature is
> `name(&mut self, kind, salt, morph)` and the derive path omits the `redraw`
> leg.

**Files:**
- Create: `domains/language/src/naming.rs`
- Modify: `domains/language/src/lib.rs`

**Interfaces:**
- Consumes: `Phonology`, `Segment`, `romanize`, `ipa` (Tasks 3, 5); kernel `Seed`.
- Produces:
  - `pub enum NameKind { Settlement, Deity, Epithet }`
  - `pub struct MorphOptions { pub honorifics: bool }` — keyed from status basis by the composition root (`Rank → true`).
  - `pub struct GeneratedName { pub roman: String, pub ipa: String }` (the two views; `roman` is what commits as the `name` fact).
  - `pub struct Namer<'a> { /* phonology + seed handle */ }` with `pub fn new(seed: &Seed, species: &str, ph: &'a Phonology) -> Namer<'a>` and `pub fn name(&mut self, kind: NameKind, salt: u64, morph: &MorphOptions, used: &BTreeSet<String>) -> GeneratedName` — draws a name for `salt`, applying kind-specific morphology, re-drawing deterministically until `roman` ∉ `used`.

- [ ] **Step 1: Failing tests** (in `naming.rs`)

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::phonology::{draw_phonology, Envelope, ExoticSeg};
    use hornvale_kernel::Seed;
    use std::collections::BTreeSet;

    fn kobold_ph() -> crate::phonology::Phonology {
        draw_phonology(&Seed(42), "kobold", &Envelope {
            labiality: 0.1, vowel_space: 0.3, voicing: 0.6, sibilance: 0.9, voice_loudness: 0.2, exotic: ExoticSeg::Trill })
    }

    #[test]
    fn names_are_deterministic_and_carry_both_views() {
        let ph = kobold_ph();
        let mut n1 = Namer::new(&Seed(1), "kobold", &ph);
        let mut n2 = Namer::new(&Seed(1), "kobold", &ph);
        let a = n1.name(NameKind::Settlement, 10, &MorphOptions { honorifics: false }, &BTreeSet::new());
        let b = n2.name(NameKind::Settlement, 10, &MorphOptions { honorifics: false }, &BTreeSet::new());
        assert_eq!(a.roman, b.roman);
        assert!(!a.roman.is_empty() && !a.ipa.is_empty());
    }

    #[test]
    fn uniqueness_redraw_avoids_collision() {
        let ph = kobold_ph();
        let mut namer = Namer::new(&Seed(2), "kobold", &ph);
        let mut used = BTreeSet::new();
        for salt in 0..50u64 {
            let g = namer.name(NameKind::Settlement, salt, &MorphOptions { honorifics: false }, &used);
            assert!(!used.contains(&g.roman), "re-draw must avoid an in-world collision");
            used.insert(g.roman);
        }
    }

    #[test]
    fn honorific_morphology_appears_only_when_requested() {
        let ph = kobold_ph();
        let mut namer = Namer::new(&Seed(3), "kobold", &ph);
        // Epithets with honorifics enabled must be able to differ from those without.
        let with = namer.name(NameKind::Epithet, 5, &MorphOptions { honorifics: true }, &BTreeSet::new());
        let mut namer2 = Namer::new(&Seed(3), "kobold", &ph);
        let without = namer2.name(NameKind::Epithet, 5, &MorphOptions { honorifics: false }, &BTreeSet::new());
        assert_ne!(with.roman, without.roman, "status-basis keying must change epithet shape");
    }
}
```

(If seed 3 happens to coincide, switch to a seed where the honorific affix changes the surface — do not weaken the assertion.)

- [ ] **Step 2: Run to verify fail** — compile errors.

- [ ] **Step 3: Implement `naming.rs`.**
  - A **stem** = draw a syllable count (per kind: settlement 2–3, deity 2–3 weighty, epithet 1–2 for the descriptive root) then fill each syllable's onset/nucleus/coda manner-slots by `pick`ing matching segments from the phonology inventory. Build `roman` by concatenating `romanize`, `ipa` by concatenating `ipa`; capitalize the first romanized letter.
  - **Morphology:** Settlement → bare stem. Deity → bare weighty stem. Epithet → a descriptive stem, optionally *reduplicated* (double a drawn syllable) and, when `morph.honorifics`, prefixed by a drawn honorific affix (a short bound stem from a dedicated honorific sub-draw). This is what makes goblin (Rank) epithets carry honorifics and kobold (Knowledge) epithets not.
  - **Uniqueness:** after building `roman`, if `used.contains(&roman)`, advance the stem draw deterministically (a re-draw counter folded into the derive label) and rebuild until unused. Bounded loop with a large cap; document the cap.
  - All draws from `seed.derive("language").derive(species).derive("name").derive(kind_label).derive(&salt.to_string()).derive(&redraw.to_string()).stream()`.
  - Re-export the naming types from `lib.rs`.

- [ ] **Step 4: Full gate.** Green.

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "feat(language): naming grammars — stems, morphology, status-keyed honorifics, uniqueness re-draw"
```

### Task 7: Structured belief content — religion emits meaning, not prose

**Files:**
- Modify: `domains/religion/src/lib.rs`
- Modify: `cli/tests/species_identity.rs` (NEW_PREDICATES grows by the structured-content predicates)

**Interfaces:**
- Consumes: kernel; `Phenomenon` (already).
- Produces (Task 8/9 consume these):
  - New predicates `DEITY_NAME`, `DEITY_EPITHET`, `SENTIMENT` (Text). `TENET` is **no longer committed** by new genesis.
  - `pub enum Sentiment { Eternal, Cyclic, Ambient }` derivable from the phenomenon (eternal = aperiodic celestial, cyclic = periodic, ambient = the felt world) — the structured sentiment.
  - `genesis(world, community, phenomena, society, names: &mut dyn DeityNamer)` — the epithet-pool draw and inline tenet building are removed; instead each deity gets a name + epithet via the `DeityNamer` callback and commits `DEITY_NAME`, `DEITY_EPITHET`, `SENTIMENT`, plus the existing `IS_BELIEF`/`HELD_BY`/`DERIVED_FROM_PHENOMENON`/`CULT_FORM`/`HIGH_GOD`. No `TENET`.
  - `pub trait DeityNamer { fn deity(&mut self, salt: u64) -> (String, String); fn epithet(&mut self, salt: u64, sentiment: Sentiment) -> (String, String); }` returning `(roman, ipa)` pairs — religion stays language-agnostic; the composition root supplies a language-backed impl (Task 9). The `ipa` strings are committed under `DEITY_NAME_IPA`/`DEITY_EPITHET_IPA` companion predicates for the `phonology` verb and book.
  - `pub struct Belief { pub id, pub deity: String, pub epithet: String, pub source_kind: String, pub sentiment: Sentiment, pub high_god: bool }` — `tenet` field removed; the rendered tenet is produced by Task 8, not stored here.
  - `beliefs_of` / `beliefs_held_by` rebuilt to read the structured facts.

- [ ] **Step 1: Failing tests.** Rewrite the religion tests to assert structured facts (a belief has a `deity`, an `epithet`, a `sentiment`; NO `tenet` fact is committed; `beliefs_held_by` returns structured `Belief`s). Provide a test `DeityNamer` stub that returns deterministic `("Xar","xar")`-style pairs. Assert `world.ledger.text_of(id, TENET)` is `None` for new beliefs.

- [ ] **Step 2: Run to verify fail.**

- [ ] **Step 3: Implement.** Delete `ETERNAL_EPITHETS`/`CYCLIC_EPITHETS` and the `EPITHET` stream label. Add the new predicates and `Sentiment`. Rewrite `genesis` to derive `Sentiment` from each phenomenon, call the `DeityNamer` for name+epithet, and commit the structured facts. Rewrite `Belief`/`beliefs_of`/`beliefs_held_by`/`cult_form_held_by` accordingly. Grow `NEW_PREDICATES` in `species_identity.rs` — but note: this task changes the goblin ledger too (tenet gone, structured facts in), so the Y2-1 superset test's byte-identity for goblin no longer holds. **Update `species_identity.rs`**: its assertion becomes the Tongues structural invariant (Task 11 owns the full version); for now, mark the pre-existing byte-identity test `#[ignore]` with a comment pointing at Task 11, or narrow it to non-name/non-belief predicates. (The controller will confirm the right call in review.)

- [ ] **Step 4: Full gate.** Note: worldgen/almanac won't compile until Task 8–9 supply the renderer and namer; if so, this task's gate is scoped to `cargo test -p hornvale-religion` plus `cargo build -p hornvale-religion`, and the workspace gate moves to Task 9's boundary. State this explicitly in the report. (Prefer to keep the workspace compiling: provide a temporary composition-root `DeityNamer` stub in worldgen so the workspace builds at every commit — the honest, incremental path.)

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "feat(religion): emit structured belief content — deity/epithet/sentiment facts, no inline tenet"
```

### Task 8: The register renderer — `render_line` behind the seam

**Files:**
- Create: `domains/language/src/register.rs`
- Modify: `domains/language/src/lib.rs`

**Interfaces:**
- Consumes: kernel only.
- Produces:
  - `pub struct VoiceParams { pub formality: f64, pub repetition: f64, pub epithet_density: f64 }` (all `[0,1]`, 0.5 ≡ goblin baseline).
  - `pub enum LineSentiment { Eternal, Cyclic, Ambient }` (language's own copy; the composition root maps religion's `Sentiment` onto it — no cross-domain import).
  - `pub struct LineContent { pub deity: String, pub epithet: String, pub sentiment: LineSentiment, pub period_days: Option<f64>, pub high_god: bool }`
  - `pub fn render_line(content: &LineContent, voice: &VoiceParams) -> String` — **the permanent seam**.

- [ ] **Step 1: Failing tests** (in `register.rs`)

```rust
#[cfg(test)]
mod tests {
    use super::*;

    fn base() -> VoiceParams { VoiceParams { formality: 0.5, repetition: 0.5, epithet_density: 0.5 } }
    fn cyclic(deity: &str, ep: &str) -> LineContent {
        LineContent { deity: deity.into(), epithet: ep.into(), sentiment: LineSentiment::Cyclic, period_days: Some(29.0), high_god: true }
    }

    #[test]
    fn a_cyclic_line_names_the_deity_and_its_return() {
        let s = render_line(&cyclic("Xar", "the Tidewalker"), &base());
        assert!(s.contains("Xar") || s.contains("Tidewalker"));
        assert!(s.contains("29"), "a cyclic tenet states its period");
    }

    #[test]
    fn higher_repetition_lengthens_the_line() {
        let low = render_line(&cyclic("Xar", "the Tidewalker"), &VoiceParams { repetition: 0.0, ..base() });
        let high = render_line(&cyclic("Xar", "the Tidewalker"), &VoiceParams { repetition: 1.0, ..base() });
        assert!(high.len() > low.len(), "repetition echoes a refrain");
    }

    #[test]
    fn render_is_pure_and_deterministic() {
        assert_eq!(render_line(&cyclic("Xar","the Tidewalker"), &base()),
                   render_line(&cyclic("Xar","the Tidewalker"), &base()));
    }
}
```

- [ ] **Step 2: Run to verify fail.**

- [ ] **Step 3: Implement `register.rs`.** v1 = templates assembled under the three knobs: pick a connective register by `formality` (plain vs. archaic word lists), assemble the core proposition from `sentiment`/`period_days`/`deity`/`epithet`, echo a short refrain when `repetition` is high, and control how the epithet is introduced / whether the honorific is repeated by `epithet_density`. Pure function, no streams (register is deterministic in its inputs). Keep the templates small and legible; this is the temporary renderer behind the permanent signature.

- [ ] **Step 4: Full gate.** Green.

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "feat(language): register renderer — render_line behind the permanent content→render seam"
```

### Task 9: Composition-root wiring — build languages, generate all names, render tenets

> **Design correction (post-Task-9 review, owner decision):** no world-wide
> `used` set. Names are pure per-`(seed, species, kind, salt)` draws (see the
> Task 6 correction), so the composition root does NOT maintain a
> `BTreeSet<String>` of used names, and `LanguageDeityNamer` holds no such
> set. Settlement names do NOT move to a `settlement/name/v2` label — they
> move out of `settlement/*` entirely, into `hornvale-language`'s real
> `language/<species>/name/settlement` derivation. `domains/language` gains a
> `stream_labels()` publishing its real labels; `cli/src/streams.rs` adds it
> to the manifest source list; `domains/settlement::stream_labels()` mints no
> `settlement/name/v2` (it would be a phantom). Pin-isolation then holds by
> construction.

**Files:**
- Modify: `windows/worldgen/src/lib.rs`
- Modify: `domains/settlement/src/lib.rs` (delete syllable pools + dead name fns; retire the old name labels in docs — no `/v2` mint)
- Modify: `domains/language/src/lib.rs` (add `stream_labels()`), `cli/src/streams.rs` (register it), `cli/Cargo.toml` (language dep)

**Interfaces:**
- Consumes: everything from Tasks 3–8.
- Produces: `pub fn language_of(world, species) -> Phonology` (rebuildable from seed+species+envelope), `pub fn voice_params(psych) -> VoiceParams`, `pub fn morph_options(psych) -> MorphOptions`, and the `DeityNamer` impl backing religion.

- [ ] **Step 1: Failing tests** (worldgen in-module):

```rust
#[test]
fn goblin_voice_params_are_the_baseline() {
    let reg = hornvale_species::registry();
    let v = voice_params(&reg["goblin"].psych);
    assert!((v.formality - 0.5).abs() < 1e-12 && (v.epithet_density - 0.5).abs() < 1e-12);
}

#[test]
fn seed_42_names_are_non_english_and_unique_per_species() {
    let world = build_world(Seed(42), &SkyPins::default(), SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(), &SettlementPins::default()).unwrap();
    let names: Vec<String> = hornvale_settlement::all_settlements(&world).iter()
        .map(|v| v.name.clone()).collect();
    assert!(!names.is_empty());
    // No settlement name is a legacy syllable-pool word.
    assert!(names.iter().all(|n| !["Zag","Gru","Bol"].iter().any(|s| n.starts_with(s))));
    // Uniqueness within the world.
    let set: std::collections::BTreeSet<_> = names.iter().collect();
    assert_eq!(set.len(), names.len(), "in-world names are unique");
}
```

- [ ] **Step 2: Run to verify fail.**

- [ ] **Step 3: Implement.**
  - Map each species' `ArticulationVector` → language `Envelope`; call `draw_phonology`; build a `Namer`. Maintain a per-world `BTreeSet<String>` of used names for uniqueness across all names in the world.
  - Replace `generate_name`/`generate_species_name` calls with `Namer`-drawn settlement names; **move the settlement name derivation to the `settlement/name/v2` label** and delete `SYLLABLES` and the species syllable pools (and the now-unused `generate_species_name`/`generate_name` bodies, or repoint them at the language engine). Population draws are untouched.
  - Supply religion's `DeityNamer` from the species `Namer` (deity + epithet, honorifics from `morph_options(psych)`); pass it into `hornvale_religion::genesis`.
  - `voice_params(psych)`: `formality = f(status_basis Rank→high, deliberation)`, `repetition = f(sociality Communal→high)`, `epithet_density = f(status_basis Rank→high)` — each identity 0.5 at the goblin baseline. `morph_options`: `honorifics = (status_basis == Rank)`.
  - The almanac context (Task 11) will render tenets via `render_line`; here just ensure the structured facts + names are committed.
  - Add `hornvale_settlement::all_settlements(world) -> Vec<VillageInfo>` if not present (used by the test and Task 11).

- [ ] **Step 4: Full gate.** The whole workspace must compile and pass now (religion's Task-7 stub is replaced by the real namer). Expected green except the almanac still shows old-style tenets until Task 11 — acceptable, tenet rendering moves in Task 11; assert names changed here.

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "feat(worldgen): build per-species languages, generate all names (settlement/name/v2), wire the deity namer"
```

### Task 10: The `phonology` reference verb + book reference page

**Files:**
- Modify: `cli/src/main.rs` (add the `phonology` subcommand), `cli/src/` (a `phonology.rs` render module mirroring `concepts.rs`/`streams.rs`)
- Create: `book/src/reference/phonology.md` (generated, drift-checked)
- Modify: `book/src/SUMMARY.md`, `.github/workflows/ci.yml` (add the generation line to the "Artifacts are current" step)

**Interfaces:**
- Consumes: the language engine; the species registry.
- Produces: `hornvale phonology` → a markdown page: per-species inventory tables (segment, romanization, IPA, features), the phonotactic templates, and IPA transcriptions of sample names.

- [ ] **Step 1: Failing test** (`cli/tests/` or in-module): `phonology` output contains each species name, an IPA column, and at least one sample name with both romanization and IPA; deterministic across two runs.

- [ ] **Step 2–4: Implement**, wire the subcommand (mirror how `concepts`/`streams` are dispatched in `main.rs`), generate `book/src/reference/phonology.md`, add the CI artifact line and SUMMARY entry. Full gate + `git diff --exit-code book/src/reference/phonology.md` after regeneration.

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "feat(cli): phonology reference verb + drift-checked book page"
```

### Task 11: Almanac/REPL rendering through the seam + the structural-invariant keystone

**Files:**
- Modify: `windows/almanac/src/lib.rs` (render tenets from `LineContent` via `render_line`), `windows/worldgen/src/lib.rs` (`almanac_context` builds `LineContent` per belief, derives `VoiceParams`), `cli/src/repl.rs` (`beliefs`/`why` show rendered tenets)
- Create: `cli/tests/tongues_identity.rs`

**Interfaces:**
- Consumes: `render_line`, `VoiceParams`, `LineContent`, `beliefs_held_by`, the Task 2 fixture.

- [ ] **Step 1: Write the keystone tests** (`tongues_identity.rs`) — the structural invariants replacing byte-identity:

```rust
//! The Tongues structural-invariant keystone (spec §8): every proper noun
//! regenerates, so byte-identity is gone. Instead: entity structure and all
//! NON-linguistic facts are unchanged from pre-Tongues; names/tenets differ;
//! names are unique and phonotactically valid; determinism holds.

// (1) Entity structure: same entity count, same non-name/non-belief-content
//     facts as the pre-tongues fixture; only name text + tenet→structured differ.
// (2) Determinism: build seed 42 twice → byte-identical world JSON.
// (3) Uniqueness: no repeated settlement or deity name in seed 42.
// (4) Non-English: no generated name matches the deleted syllable pools.
// (5) Pin-isolation: --species goblin consumes the same language draws for
//     goblin as the unpinned path (compare a goblin settlement present in both).
```

Write each as a concrete test (follow `eyes_identity.rs`/`species_identity.rs` shape; the fixture gives the pre-Tongues world for (1)).

- [ ] **Step 2: Run to verify fail** (rendering + some invariants not yet wired).

- [ ] **Step 3: Implement.** `almanac_context` builds a `LineContent` for each belief (mapping religion `Sentiment` → `LineSentiment`) and stores the rendered tenet in the `PantheonBlock` beliefs for display; the almanac renders those strings. REPL `beliefs`/`why` render likewise. Make the invariants pass (they should, if Tasks 7–9 kept the entity graph fixed and uniqueness works).

- [ ] **Step 4: Full gate** + the keystone suite: `cargo test -p hornvale --test tongues_identity`. Green.

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "feat(almanac,cli): render tenets through the seam; the Tongues structural-invariant keystone"
```

### Task 12: Lab metrics + preregistered calibrations

**Files:**
- Modify: `windows/lab/src/metrics.rs`, `windows/lab/tests/calibration.rs`

**Interfaces:**
- Consumes: the language engine, `beliefs_held_by`, the species registry.
- Produces metrics: `phonotactic-validity-goblin/-kobold` (Flag), `epithet-honorific-goblin/-kobold` (Flag), `name-length-goblin/-kobold` (Numeric).

- [ ] **Step 1: Implement the metrics** (a private helper validates each generated name against its species phonotactics; the honorific flag detects the honorific affix class).

- [ ] **Step 2: Preregistered calibration tests** (directional, before the census; ADR 0016):

```
- phonotactic validity is TRUE for every name of every species, row-by-row (the instrument reproduces its own grammar);
- epithet-honorific is TRUE for goblin (Rank) and FALSE for kobold (Knowledge), row-by-row;
- (optional, may run in The Meeting) phonology-based species attribution beats chance.
```

Write the first two as row-by-row assertions over `census-lands-drift.study.json`. Exact rates (e.g. the measured in-world collision rate — a reported property of the large name space, since there is no re-draw) are pinned in Task 13.

- [ ] **Step 3: Full gate** incl. `cargo test -p hornvale-lab --test calibration`. If phonotactic validity is ever FALSE, STOP and report BLOCKED — the engine is producing names it calls invalid.

- [ ] **Step 4: Commit**

```bash
git add -A
git commit -m "feat(lab): phonotactic-validity, epithet-honorific, name-length metrics + preregistered calibrations"
```

### Task 13: The re-baseline — 10k censuses, Study 008, pinned rows, artifacts, TTS

**Files:**
- Create: `studies/census-of-tongues.study.json`, `book/src/laboratory/study-008.md`, `scripts/tts.sh`
- Modify: `book/src/laboratory/overview.md`, `book/src/SUMMARY.md`, `windows/lab/tests/calibration.rs` (pin measured rates), and regenerate every committed artifact (almanacs, reference dumps, studies).

- [ ] **Step 1: Author `census-of-tongues.study.json`** (10k default seeds, `metrics: all`), mirroring `census-of-eyes.study.json`.
- [ ] **Step 2: Run the author-time censuses** (`cargo run --release -p hornvale -- lab run studies/*.study.json` for each), capturing headline numbers: phonotactic validity (must be 100%), honorific split (goblin 100% / kobold 0%), name-length distributions, the measured in-world collision rate (de-facto, from the large name space — there is no re-draw).
- [ ] **Step 3: Pin** the measured collision/validity counts as exact calibration rows (verify determinism: the drift-study counts must not move).
- [ ] **Step 4: Regenerate all committed artifacts** (the CI "Artifacts are current" command list verbatim) and diff — every proper noun changes (expected, the whole point); anything structural changing is a bug — STOP and diagnose. Add `census-of-tongues` to CI only if the other 10k censuses are CI-run (they are author-time-only — say so in the study page).
- [ ] **Step 5: Write Study 008** (mirror study-007.md): the naming/voice baseline, the preregistered claims with dates, measured results, the phonology-attribution note (defined here, runs in The Meeting). Add the `scripts/tts.sh` espeak-ng convenience (ungated; documented as never-in-CI).
- [ ] **Step 6: Full gate + mdbook build + commit**

```bash
git add -A
git commit -m "chore(artifacts): the Tongues re-baseline + Study 008 (census of tongues, pinned calibrations)"
```

### Task 14: Book close — freshness sweep, chronicle, retrospective, registry flips

**Files:**
- Modify: `book/src/domains/{religion,culture,settlement}.md`, `book/src/domains/language.md` (Task 1's chapter, reconciled with merged reality)
- Create: `book/src/chronicle/<next>-the-tongues.md`, `docs/retrospectives/campaign-y2-3.md`
- Modify: `book/src/SUMMARY.md`, `docs/vision/idea-registry.md` (EXP-7 `spec'd` → `shipped`; MAP-3 stays `elaborated`; BIO-10 stays `raw`)

- [ ] **Step 1: Freshness sweep** — `grep -rn "syllable\|stopgap\|vocabulary\|English-template\|Bolzag\|deleted by The Tongues\|epithet pool" book/src/` and reconcile every hit with merged reality; re-verify the language chapter's example names/IPA against the actual `phonology` output and seed-42 almanac (regenerate, don't trust prose — the Y2-1/Y2-2 lesson).
- [ ] **Step 2: Chronicle + retrospective** at book altitude (chronicle: in-world, no process vocabulary, no registry IDs — the Task-1 boundary lesson; retrospective: process-not-product per decision 0020, ≤ one page, drawing on the SDD progress ledger).
- [ ] **Step 3: Registry** — EXP-7 → `shipped` (Where stays the spec); confirm MAP-3/LANG-1 untouched, BIO-10 stays raw. Run `cargo test -p hornvale --test docs_consistency`.
- [ ] **Step 4: Full gate + mdbook build + docs_consistency.** Green.
- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "docs(book): Campaign Y2-3 close — chronicle, generated names across the book, freshness sweep"
```

---

## Self-Review Notes (kept for the executor)

- **Spec coverage:** §3 phoneme model → Task 3; §4 articulation vector → Task 4; §3 phonology engine → Task 5; §5 naming → Task 6; §6 content→render seam → Tasks 7 (content), 8 (renderer), 9/11 (wiring); §7 composition root → Task 9; §8 save format + structural invariants → Tasks 7, 9, 11; §9 Lab → Tasks 12, 13; §10 reference verb/TTS → Tasks 10, 13; §11 book → Tasks 1, 14; §12 success criteria → Tasks 11 (invariants), 13 (calibrations), 14 (book).
- **The seam is the deliverable (spec §2.2):** Task 7 (content) and Task 8 (renderer signature) must land the `LineContent`/`VoiceParams`/`render_line` interface exactly; a reviewer should reject any Task-7/8 implementation that post-processes finished English instead of rendering from structured content.
- **The identity story is different from Eyes/Peoples:** there is NO byte-identity fixture. Task 2's fixture backs Task 11's *structural* invariants. Do not let an implementer assert byte-identity of names — that would be wrong by construction.
- **Compile-at-every-commit hazard:** Task 7 changes religion's interface before worldgen is rewired (Task 9). Keep the workspace compiling by landing a temporary composition-root `DeityNamer` stub in Task 7 (noted in Task 7 Step 4); the reviewer confirms the workspace builds at the Task 7 boundary.
- **Line numbers drift** — re-grep before editing; they will move as tasks land out of order.
- **Any articulation-vector retune (Nathan's prerogative, spec §4) invalidates Task 12's calibrations and Task 13's census numbers** — retune before Task 12 runs, or redo the pins.
