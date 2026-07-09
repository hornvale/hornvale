# Audible Phonology Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add an espeak-ng phoneme view over `Segment` and use it to author
committed, content-addressed mp3 clips that make the book's phonology-page
sample names audible.

**Architecture:** `espeak()` is a third pure view beside `romanize()`/`ipa()`
in `domains/language`; `GeneratedName` gains an `espeak` field built in the
same render pass as the other two. The CLI grows a hand-rolled CRC-32, an
Espeak + Audio column on the generated phonology page, and a `hornvale voice`
authoring subcommand that shells out to espeak-ng + ffmpeg offline. CI never
runs either tool; a test pins committed audio to exactly the page's
referenced set. Spec: `docs/superpowers/specs/2026-07-08-audible-phonology-design.md`.

**Tech Stack:** Rust (edition 2024), std-only additions; espeak-ng 1.52.0 +
ffmpeg as offline authoring tools (never build/test dependencies).

## Global Constraints

- Dependencies: `serde` + `serde_json` only, workspace-wide. The CRC-32 is
  hand-rolled; espeak-ng/ffmpeg are invoked only by `hornvale voice`.
- No `HashMap`/`HashSet`, no wall-clock time (clippy enforces both).
- Every commit passes: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`.
  Run `cargo fmt` (no `--check`) before each commit.
- Every public item, field, and variant gets a one-line doc comment
  (`#![warn(missing_docs)]` + clippy `-D warnings` make omissions fatal).
- `espeak()` consumes no stream draws — it is a view, so no
  stream-consumption contract changes anywhere in this plan.
- No test may execute espeak-ng or ffmpeg.
- **Task 4 is a human checkpoint (the spec §7 abandon gate).** It is run by
  the main session, never dispatched to a subagent. If the gate is no-go,
  Tasks 5–9 are abandoned.

---

### Task 1: the `espeak()` segment view

**Files:**
- Modify: `domains/language/src/phoneme.rs` (add function after `ipa()`, ~line 565)
- Modify: `domains/language/src/lib.rs:22` (export)

**Interfaces:**
- Produces: `pub fn espeak(seg: &Segment) -> &'static str` in
  `hornvale_language` — the espeak-ng phoneme mnemonic for one segment.

- [ ] **Step 1: Write the failing tests**

Add to the `tests` mod at the bottom of `domains/language/src/phoneme.rs`:

```rust
#[test]
fn espeak_is_a_third_distinct_view() {
    let sh = Segment::Consonant {
        place: Place::Postalveolar,
        manner: Manner::Sibilant,
        voiced: false,
    };
    assert_eq!(espeak(&sh), "S");
    let ng = Segment::Consonant {
        place: Place::Velar,
        manner: Manner::Nasal,
        voiced: true,
    };
    assert_eq!(espeak(&ng), "N");
    let click = Segment::Consonant {
        place: Place::Glottal,
        manner: Manner::Click,
        voiced: false,
    };
    assert_eq!(espeak(&click), "tS");
    let ejective = Segment::Consonant {
        place: Place::Velar,
        manner: Manner::Ejective,
        voiced: false,
    };
    assert_eq!(espeak(&ejective), "k");
}

/// espeak-ng direct phoneme input only accepts its ASCII mnemonics, and
/// `'` is its stress marker. Guard every canonical segment's mnemonic —
/// this is what makes the delegation to `ipa()` for the shared arms safe:
/// if an IPA glyph ever drifts non-ASCII, this fails instead of espeak
/// silently dropping the phoneme.
#[test]
fn every_canonical_espeak_mnemonic_is_bare_ascii_alphanumeric() {
    for seg in canonical_segments() {
        let m = espeak(&seg);
        assert!(
            !m.is_empty() && m.chars().all(|c| c.is_ascii_alphanumeric()),
            "{seg:?} has espeak mnemonic {m:?}"
        );
    }
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p hornvale-language espeak`
Expected: FAIL to compile — `cannot find function espeak`.

- [ ] **Step 3: Implement `espeak()`**

Add after `ipa()` in `domains/language/src/phoneme.rs`:

```rust
/// Render a segment as its espeak-ng phoneme mnemonic (Kirshenbaum-style
/// ASCII), the notation `hornvale voice` feeds espeak-ng's direct phoneme
/// input to author the book's audio clips.
///
/// Verified against espeak-ng 1.52.0, voice `en`. Five segments differ
/// from their IPA glyph: `ʃ`/`ʒ` → `S`/`Z`, `ŋ` → `N`, and two documented
/// approximations in the spirit of the romanization digraphs — the click
/// (voice `en` silently drops Kirshenbaum's `!`) renders as `tS`, and the
/// ejective as plain `k` (`'` is espeak's stress marker, so `k'` cannot be
/// written). Every other mnemonic coincides with the IPA glyph, so those
/// arms delegate to [`ipa`]; the ASCII-guard test keeps that coupling safe.
pub fn espeak(seg: &Segment) -> &'static str {
    match seg {
        Segment::Consonant {
            place: Place::Postalveolar,
            manner: Manner::Sibilant,
            voiced: false,
        } => "S",
        Segment::Consonant {
            place: Place::Postalveolar,
            manner: Manner::Sibilant,
            voiced: true,
        } => "Z",
        Segment::Consonant {
            place: Place::Velar,
            manner: Manner::Nasal,
            voiced: true,
        } => "N",
        Segment::Consonant {
            place: Place::Glottal,
            manner: Manner::Click,
            voiced: false,
        } => "tS",
        Segment::Consonant {
            place: Place::Velar,
            manner: Manner::Ejective,
            voiced: false,
        } => "k",
        other => ipa(other),
    }
}
```

Also update the module doc comment at the top of `phoneme.rs`: in the first
paragraph, change "`romanize` (ASCII-ish, for the almanac) and `ipa` (for
the book) are VIEWS over a segment" to mention all three views, e.g.
"`romanize` (ASCII-ish, for the almanac), `ipa` (for the book), and
`espeak` (espeak-ng mnemonics, for authored audio) are VIEWS over a
segment".

- [ ] **Step 4: Export from the crate root**

In `domains/language/src/lib.rs`, line 22, extend the re-export:

```rust
pub use phoneme::{Backness, Height, Manner, Place, Segment, espeak, ipa, romanize, sonority};
```

- [ ] **Step 5: Run tests to verify they pass**

Run: `cargo test -p hornvale-language espeak`
Expected: PASS (2 tests).

- [ ] **Step 6: Full gate and commit**

```bash
cargo fmt
cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings
git add domains/language/src/phoneme.rs domains/language/src/lib.rs
git commit -m "feat(language): espeak-ng mnemonic view over Segment"
```

---

### Task 2: `espeak_word()` assembly

**Files:**
- Modify: `domains/language/src/phoneme.rs` (add after `espeak()`)
- Modify: `domains/language/src/lib.rs:22` (export)

**Interfaces:**
- Consumes: `espeak(seg: &Segment) -> &'static str` (Task 1).
- Produces: `pub fn espeak_word(segments: &[Segment]) -> String` — the full
  formulation for espeak-ng direct phoneme input, e.g. `[[zv'etnot]]`:
  concatenated mnemonics, a `'` stress marker before the first vowel,
  wrapped in `[[…]]`.

- [ ] **Step 1: Write the failing tests**

Add to the `tests` mod of `phoneme.rs`:

```rust
#[test]
fn espeak_word_stresses_the_first_vowel_and_wraps_for_phoneme_input() {
    use Backness::*;
    use Height::*;
    use Manner::*;
    use Place::*;
    // z-v-e-t-n-o-t — the seed-42 goblin sample "Zvetnot".
    let segs = [
        Segment::Consonant { place: Alveolar, manner: Sibilant, voiced: true },
        Segment::Consonant { place: Labial, manner: Fricative, voiced: true },
        Segment::Vowel { height: Mid, backness: Front, rounded: false },
        Segment::Consonant { place: Alveolar, manner: Stop, voiced: false },
        Segment::Consonant { place: Alveolar, manner: Nasal, voiced: true },
        Segment::Vowel { height: Mid, backness: Back, rounded: true },
        Segment::Consonant { place: Alveolar, manner: Stop, voiced: false },
    ];
    assert_eq!(espeak_word(&segs), "[[zv'etnot]]");
}

#[test]
fn espeak_word_of_a_vowelless_or_empty_run_carries_no_stress_marker() {
    let segs = [Segment::Consonant {
        place: Place::Alveolar,
        manner: Manner::Sibilant,
        voiced: false,
    }];
    assert_eq!(espeak_word(&segs), "[[s]]");
    assert_eq!(espeak_word(&[]), "[[]]");
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p hornvale-language espeak_word`
Expected: FAIL to compile — `cannot find function espeak_word`.

- [ ] **Step 3: Implement**

Add after `espeak()`:

```rust
/// Assemble a whole word's espeak-ng formulation: each segment's mnemonic
/// in order, a `'` stress marker before the first vowel (explicit stress
/// keeps the formulation self-contained rather than leaning on espeak's
/// automatic assignment), wrapped in `[[…]]` for direct phoneme input.
pub fn espeak_word(segments: &[Segment]) -> String {
    let mut body = String::new();
    let mut stressed = false;
    for seg in segments {
        if !stressed && matches!(seg, Segment::Vowel { .. }) {
            body.push('\'');
            stressed = true;
        }
        body.push_str(espeak(seg));
    }
    format!("[[{body}]]")
}
```

- [ ] **Step 4: Export from the crate root**

In `domains/language/src/lib.rs`, extend line 22 again:

```rust
pub use phoneme::{
    Backness, Height, Manner, Place, Segment, espeak, espeak_word, ipa, romanize, sonority,
};
```

- [ ] **Step 5: Run tests to verify they pass**

Run: `cargo test -p hornvale-language espeak_word`
Expected: PASS (2 tests).

- [ ] **Step 6: Full gate and commit**

```bash
cargo fmt
cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings
git add domains/language/src/phoneme.rs domains/language/src/lib.rs
git commit -m "feat(language): espeak_word assembly with explicit first-vowel stress"
```

---

### Task 3: `GeneratedName.espeak` — the third view on names

**Files:**
- Modify: `domains/language/src/naming.rs` (struct at ~line 74, `render()` at ~line 280, imports at line 27)

**Interfaces:**
- Consumes: `espeak_word(&[Segment]) -> String` (Task 2).
- Produces: `GeneratedName` gains `pub espeak: String` — the whole name's
  `[[…]]` formulation, built in the same render pass as `roman` and `ipa`.
  Only `naming.rs::render()` constructs `GeneratedName` anywhere in the
  workspace, so no other call sites change.

- [ ] **Step 1: Write the failing test**

Add to the `tests` mod of `domains/language/src/naming.rs`:

```rust
#[test]
fn a_generated_name_carries_a_wrapped_stressed_espeak_formulation() {
    let ph = kobold_ph();
    let namer = Namer::new(&Seed(1), "kobold", &ph);
    let name = namer.name(
        NameKind::Settlement,
        0,
        &MorphOptions { honorifics: false },
    );
    assert!(
        name.espeak.starts_with("[[") && name.espeak.ends_with("]]"),
        "formulation {:?} must be wrapped for espeak direct phoneme input",
        name.espeak
    );
    assert!(
        name.espeak.contains('\''),
        "formulation {:?} must carry an explicit stress marker (every name has a vowel)",
        name.espeak
    );
}
```

Also extend the existing `generated_names_never_contain_the_unrepresentable_glyph`
test: after the two existing asserts, add

```rust
            assert!(
                !g.espeak.contains('?'),
                "espeak {:?} contains the unrepresentable-segment glyph",
                g.espeak
            );
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p hornvale-language naming`
Expected: FAIL to compile — `no field espeak on type GeneratedName`.

- [ ] **Step 3: Implement**

In `domains/language/src/naming.rs`:

1. Line 27, extend the import:

```rust
use crate::phoneme::{Manner, Segment, espeak_word, ipa, romanize};
```

2. Add the field to `GeneratedName` (after `ipa`):

```rust
    /// The espeak-ng formulation (`[[zv'etnot]]`), the input `hornvale
    /// voice` hands espeak-ng to author the book's audio clip for this name.
    pub espeak: String,
```

3. Rewrite `render()` to collect segments and build the third view in the
same pass:

```rust
    /// Render a sequence of syllables to all three surface views,
    /// capitalizing the romanization's first letter (the IPA and espeak
    /// views keep no case convention).
    fn render(syllables: &[Syllable]) -> GeneratedName {
        let mut roman = String::new();
        let mut ipa_str = String::new();
        let mut segments: Vec<Segment> = Vec::new();
        for syllable in syllables {
            for seg in syllable.segments() {
                roman.push_str(romanize(seg));
                ipa_str.push_str(ipa(seg));
                segments.push(*seg);
            }
        }
        GeneratedName {
            roman: capitalize_first(&roman),
            ipa: ipa_str,
            espeak: espeak_word(&segments),
        }
    }
```

4. Update the `GeneratedName` struct doc comment ("its two views" → "its
three views") and the module doc's mention of the two views if present.

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p hornvale-language naming`
Expected: PASS (all naming tests, including the two touched ones).

- [ ] **Step 5: Full gate and commit**

```bash
cargo fmt
cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings
git add domains/language/src/naming.rs
git commit -m "feat(language): GeneratedName carries its espeak formulation"
```

---

### Task 4: LISTEN GATE — human checkpoint (spec §7 Stage 2)

**Run by the main session, never a subagent.** This is the abandon gate:
if the generated audio disappoints, Tasks 5–9 are dropped, this branch
keeps only Tasks 1–3, and the outcome is recorded in
`docs/vision/idea-registry.md`.

**Files:** none committed (temporary test added and removed; audio in the
session scratchpad only).

- [ ] **Step 1: Temporarily print every sample formulation**

Add to the `tests` mod of `cli/src/phonology.rs` (this test is deleted in
Step 4 — it exists only to dump the gate's inputs):

```rust
    /// TEMPORARY (listen gate, plan task 4): dump every sample name's
    /// espeak formulation for hand-authoring the gate audio. Deleted once
    /// the gate is decided.
    #[test]
    fn print_espeak_formulations() {
        let world = World::new(Seed(REFERENCE_SEED));
        for (species, def) in hornvale_species::registry() {
            let phonology = world_builder::language_of(&world, species);
            let namer = Namer::new(&world.seed, species, &phonology);
            let morph = world_builder::morph_options(&def.psych);
            for salt in 0..SETTLEMENT_SAMPLES {
                let n = namer.name(NameKind::Settlement, salt, &morph);
                println!("{species}\t{}\t{}", n.roman, n.espeak);
            }
            let d = namer.name(NameKind::Deity, 0, &morph);
            println!("{species}\t{}\t{}", d.roman, d.espeak);
        }
    }
```

Run: `cargo test -p hornvale print_espeak_formulations -- --nocapture | grep -P '^\w+\t' > <scratchpad>/formulations.tsv`

- [ ] **Step 2: Author gate audio into the scratchpad**

```bash
cd <scratchpad> && mkdir -p gate
while IFS=$'\t' read -r species roman esp; do
  espeak-ng -v en -s 130 -w gate/tmp.wav "$esp"
  ffmpeg -y -loglevel error -i gate/tmp.wav -codec:a libmp3lame -qscale:a 4 "gate/$species-$roman.mp3"
done < formulations.tsv
rm -f gate/tmp.wav
```

- [ ] **Step 3: Send every mp3 to the user and ask go/no-go**

Send the full `gate/*.mp3` set with SendUserFile. The user listens and
decides. **Go:** proceed to Step 4 and Task 5. **No-go:** delete the
temporary test, add an idea-registry row recording the outcome, stop.

- [ ] **Step 4 (on go): remove the temporary test**

Delete `print_espeak_formulations` from `cli/src/phonology.rs`, then:

```bash
cargo fmt
cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings
git status   # expect: clean tree, nothing to commit
```

---

### Task 5: CRC-32 and content-addressed filenames

**Files:**
- Create: `cli/src/audio.rs`
- Modify: `cli/src/main.rs:4-7` (module declaration)

**Interfaces:**
- Produces (both `pub(crate)`, in module `crate::audio` of the `hornvale`
  binary crate):
  - `fn crc32(bytes: &[u8]) -> u32` — CRC-32/IEEE (zlib polynomial).
  - `fn audio_filename(formulation: &str) -> String` — eight lowercase hex
    digits of `crc32(formulation.as_bytes())` plus `.mp3`, e.g.
    `"3f0c2a91.mp3"`.

- [ ] **Step 1: Create `cli/src/audio.rs` with failing tests**

```rust
//! Content-addressed audio artifacts for the book's phonology page: a
//! hand-rolled CRC-32 names each clip after its espeak formulation, and
//! `hornvale voice` authors any missing clips offline via espeak-ng +
//! ffmpeg (the only place in the workspace that shells out to either).

/// CRC-32/IEEE (the zlib polynomial, reflected, bitwise — no table), the
/// checksum content-addressing the book's audio clips. Hand-rolled to keep
/// the serde-only dependency allowlist intact.
pub(crate) fn crc32(bytes: &[u8]) -> u32 {
    let mut crc: u32 = 0xffff_ffff;
    for &byte in bytes {
        crc ^= u32::from(byte);
        for _ in 0..8 {
            let mask = (crc & 1).wrapping_neg();
            crc = (crc >> 1) ^ (0xedb8_8320 & mask);
        }
    }
    !crc
}

/// The committed filename for a formulation's clip: eight lowercase hex
/// digits of its CRC-32, `.mp3`. Content-addressing is the freshness
/// mechanism — a phonology change alters the formulation, hence the
/// filename, and the artifact-set test fails until `hornvale voice` runs.
pub(crate) fn audio_filename(formulation: &str) -> String {
    format!("{:08x}.mp3", crc32(formulation.as_bytes()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn crc32_matches_the_ieee_known_answer() {
        // The standard check value for CRC-32/IEEE.
        assert_eq!(crc32(b"123456789"), 0xcbf4_3926);
        assert_eq!(crc32(b""), 0);
    }

    #[test]
    fn audio_filenames_are_eight_lowercase_hex_digits_dot_mp3() {
        let name = audio_filename("[[zv'etnot]]");
        assert_eq!(name.len(), 12);
        assert!(name.ends_with(".mp3"));
        assert!(name[..8].chars().all(|c| c.is_ascii_hexdigit() && !c.is_ascii_uppercase()));
        // Content-addressed: distinct formulations, distinct names.
        assert_ne!(name, audio_filename("[[q'aq]]"));
    }
}
```

- [ ] **Step 2: Declare the module and verify tests fail without it**

First run: `cargo test -p hornvale audio` — expected: 0 tests run (module
not declared, file unused). Then in `cli/src/main.rs` add to the module
block (alphabetical, before `mod concepts;`):

```rust
mod audio;
```

- [ ] **Step 3: Run tests to verify they pass**

Run: `cargo test -p hornvale audio`
Expected: PASS (2 tests).

- [ ] **Step 4: Full gate and commit**

```bash
cargo fmt
cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings
git add cli/src/audio.rs cli/src/main.rs
git commit -m "feat(cli): hand-rolled CRC-32 content-addressing for audio artifacts"
```

---

### Task 6: Espeak + Audio columns on the phonology page

**Files:**
- Modify: `cli/src/phonology.rs` (sample-names section, ~lines 69-82; add `sample_names_for`; widen `REFERENCE_SEED`/`SETTLEMENT_SAMPLES` visibility)
- Modify: `book/src/reference/phonology.md` (regenerated)

**Interfaces:**
- Consumes: `GeneratedName.espeak` (Task 3), `crate::audio::audio_filename`
  (Task 5).
- Produces (in `cli/src/phonology.rs`, for Task 7):
  - `pub(crate) const REFERENCE_SEED: u64 = 42;` (was private)
  - `pub(crate) fn sample_names_for(world: &World, species: &str, def: &hornvale_species::SpeciesDef) -> Vec<(&'static str, GeneratedName)>`
    — the exact (kind-label, name) sample set one species' table renders,
    in render order: three settlement names (salts 0..3), then one deity
    name (salt 0).

- [ ] **Step 1: Write the failing test**

Add to the `tests` mod of `cli/src/phonology.rs`:

```rust
    /// Every sample-name row must reference a content-addressed audio clip
    /// whose filename is the CRC-32 of that row's espeak formulation — the
    /// page and `hornvale voice` must agree on names by construction.
    #[test]
    fn sample_rows_carry_espeak_and_content_addressed_audio() {
        let doc = render_phonology();
        assert!(doc.contains("Espeak"), "missing the Espeak column");
        let world = World::new(Seed(REFERENCE_SEED));
        let registry = hornvale_species::registry();
        let (species, def) = registry.iter().next().expect("at least one species");
        let samples = sample_names_for(&world, species, def);
        assert_eq!(samples.len(), SETTLEMENT_SAMPLES as usize + 1);
        for (_, name) in &samples {
            assert!(
                doc.contains(&format!("`{}`", name.espeak)),
                "page must show formulation {}",
                name.espeak
            );
            assert!(
                doc.contains(&format!(
                    "src=\"../audio/{}\"",
                    crate::audio::audio_filename(&name.espeak)
                )),
                "page must reference {}'s clip by its CRC-32 name",
                name.roman
            );
        }
    }
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale sample_rows_carry`
Expected: FAIL to compile — `cannot find function sample_names_for`.

- [ ] **Step 3: Implement**

In `cli/src/phonology.rs`:

1. Widen the two constants:

```rust
pub(crate) const REFERENCE_SEED: u64 = 42;
```

and

```rust
pub(crate) const SETTLEMENT_SAMPLES: u64 = 3;
```

(keep their existing doc comments).

2. Add after `render_phonology()`:

```rust
/// The sample-name set for one species — three settlement names then one
/// deity name, in the order the page's table renders them. Shared with
/// `hornvale voice`, which authors one audio clip per entry, so the page
/// and the artifact set agree by construction.
pub(crate) fn sample_names_for(
    world: &World,
    species: &str,
    def: &hornvale_species::SpeciesDef,
) -> Vec<(&'static str, GeneratedName)> {
    let phonology = world_builder::language_of(world, species);
    let namer = Namer::new(&world.seed, species, &phonology);
    let morph = world_builder::morph_options(&def.psych);
    let mut samples = Vec::new();
    for salt in 0..SETTLEMENT_SAMPLES {
        samples.push(("Settlement", namer.name(NameKind::Settlement, salt, &morph)));
    }
    samples.push(("Deity", namer.name(NameKind::Deity, 0, &morph)));
    samples
}
```

and extend the import at the top: `use hornvale_language::{GeneratedName, Manner, NameKind, Namer, Segment, ipa, romanize};`

3. Replace the sample-names section of `render_phonology()` (the block from
`doc.push_str("### Sample names\n\n");` through the deity row, currently
lines 69-82) with:

```rust
        doc.push_str("### Sample names\n\n");
        doc.push_str(
            "| Kind | Romanization | IPA | Espeak | Audio |\n|---|---|---|---|---|\n",
        );
        for (kind, name) in sample_names_for(&world, species, &def) {
            doc.push_str(&format!(
                "| {} | {} | /{}/ | `{}` | <audio controls preload=\"none\" src=\"../audio/{}\"></audio> |\n",
                kind,
                name.roman,
                name.ipa,
                name.espeak,
                crate::audio::audio_filename(&name.espeak),
            ));
        }
        doc.push('\n');
```

(The `let namer = …` / `let morph = …` lines above the old block move into
`sample_names_for`; delete them from the loop body.)

4. Update the page preamble (the `doc.push_str(&format!(…))` near the top)
to mention the third view: after "both views over it, never stored
independently (spec §3–4)", extend the sentence to "…; the espeak
formulation (audio column) is the third view, authored to clips by
`hornvale voice`."

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p hornvale phonology`
Expected: PASS (all phonology tests including the new one).

- [ ] **Step 5: Regenerate the committed page**

```bash
cargo run -p hornvale -- phonology > book/src/reference/phonology.md
git diff --stat book/src/reference/phonology.md   # expect: sample tables gained two columns
```

(The referenced mp3 files do not exist yet — Task 7 authors them; nothing
asserts their existence until Task 8.)

- [ ] **Step 6: Full gate and commit**

```bash
cargo fmt
cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings
git add cli/src/phonology.rs book/src/reference/phonology.md
git commit -m "feat(cli): espeak + audio columns on the generated phonology page"
```

---

### Task 7: `hornvale voice` — the offline authoring subcommand

**Files:**
- Modify: `cli/src/audio.rs` (add the subcommand)
- Modify: `cli/src/main.rs` (dispatch arm ~line 74, usage text ~line 38)
- Create: `book/src/audio/*.mp3` (authored artifacts, committed)

**Interfaces:**
- Consumes: `sample_names_for`, `REFERENCE_SEED` (Task 6),
  `audio_filename` (Task 5), `crate::flag_value` (exists, `main.rs:92`).
- Produces: `pub(crate) fn cmd_voice(args: &[String]) -> Result<(), String>`
  in `crate::audio`; the committed clip set under `book/src/audio/`.

- [ ] **Step 1: Implement the subcommand**

Add to `cli/src/audio.rs`:

```rust
use hornvale_kernel::{Seed, World};
use std::fs;
use std::path::Path;

/// The pinned espeak-ng voice. Changing it only affects newly authored
/// clips — existing files are content-addressed and never regenerated.
const ESPEAK_VOICE: &str = "en";
/// The pinned espeak-ng speaking rate, words per minute.
const ESPEAK_SPEED: &str = "130";

/// `hornvale voice [--out DIR]`: author any missing audio clips for the
/// phonology page's sample names (default out: `book/src/audio`). The
/// offline authoring step — espeak-ng and ffmpeg run here and nowhere
/// else; output is committed and CI only ever checks the file *set*.
pub(crate) fn cmd_voice(args: &[String]) -> Result<(), String> {
    let out_dir = Path::new(crate::flag_value(args, "--out").unwrap_or("book/src/audio"));
    fs::create_dir_all(out_dir).map_err(|e| format!("creating {}: {e}", out_dir.display()))?;
    let world = World::new(Seed(crate::phonology::REFERENCE_SEED));
    let (mut written, mut kept) = (0u32, 0u32);
    for (species, def) in hornvale_species::registry() {
        for (_, name) in crate::phonology::sample_names_for(&world, species, &def) {
            let path = out_dir.join(audio_filename(&name.espeak));
            if path.exists() {
                kept += 1;
                continue;
            }
            record(&name.espeak, &path)?;
            println!("wrote {}  ({species} {} {})", path.display(), name.roman, name.espeak);
            written += 1;
        }
    }
    println!("{written} clip(s) written, {kept} already present");
    Ok(())
}

/// Author one clip: espeak-ng renders the formulation to a temporary wav,
/// ffmpeg encodes it to mp3 at `out`. Fails loudly naming whichever tool
/// is missing or unhappy.
fn record(formulation: &str, out: &Path) -> Result<(), String> {
    let wav = std::env::temp_dir().join(format!("hornvale-voice-{}.wav", std::process::id()));
    let wav_str = wav.to_str().ok_or("temp dir path is not UTF-8")?;
    let out_str = out.to_str().ok_or("output path is not UTF-8")?;
    run(
        "espeak-ng",
        &["-v", ESPEAK_VOICE, "-s", ESPEAK_SPEED, "-w", wav_str, formulation],
    )?;
    let result = run(
        "ffmpeg",
        &["-y", "-loglevel", "error", "-i", wav_str, "-codec:a", "libmp3lame", "-qscale:a", "4", out_str],
    );
    let _ = fs::remove_file(&wav);
    result
}

/// Run one external authoring tool to completion, inheriting stderr so its
/// diagnostics reach the operator.
fn run(tool: &str, args: &[&str]) -> Result<(), String> {
    let status = std::process::Command::new(tool)
        .args(args)
        .status()
        .map_err(|e| format!("{tool}: {e} (is it installed and on PATH?)"))?;
    if status.success() {
        Ok(())
    } else {
        Err(format!("{tool} exited with {status}"))
    }
}
```

- [ ] **Step 2: Wire the dispatch and usage text**

In `cli/src/main.rs`:

1. Add the dispatch arm after `Some("phonology") => cmd_phonology(),`:

```rust
        Some("voice") => audio::cmd_voice(&args),
```

2. Add to `USAGE` after the `hornvale phonology` line:

```
  hornvale voice [--out <DIR>]             author missing phonology audio clips (espeak-ng + ffmpeg; default out: book/src/audio)
```

- [ ] **Step 3: Verify the command builds and errors helpfully**

```bash
cargo run -p hornvale -- voice --out /tmp/hv-voice-test
ls /tmp/hv-voice-test
```

Expected: one `wrote …` line per sample name (8 with today's two species —
goblin and kobold, four samples each), then `8 clip(s) written, 0 already
present`; the directory holds 8 mp3 files with 8-hex-digit names.
Re-running prints `0 clip(s) written, 8 already present`.

- [ ] **Step 4: Author and commit the real artifacts**

```bash
cargo run -p hornvale -- voice
ls book/src/audio    # expect the same 8 clips
```

- [ ] **Step 5: Full gate and commit**

```bash
cargo fmt
cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings
git add cli/src/audio.rs cli/src/main.rs book/src/audio
git commit -m "feat(cli): hornvale voice authors committed phonology audio clips"
```

---

### Task 8: the artifact-set test

**Files:**
- Create: `cli/tests/audio_artifacts.rs`

**Interfaces:**
- Consumes: the committed `book/src/reference/phonology.md` (Task 6) and
  `book/src/audio/` clips (Task 7). Pure file inspection — no espeak-ng,
  no ffmpeg, no world genesis.

- [ ] **Step 1: Write the test**

Create `cli/tests/audio_artifacts.rs`:

```rust
//! The committed phonology page's audio references and the committed audio
//! directory must be exactly the same set: every referenced clip exists,
//! and no orphaned clip lingers after a phonology change. Content-addressed
//! names make this existence check the entire freshness story — CI never
//! compares waveforms (espeak-ng promises no cross-version byte stability).

use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};

/// The repository root, resolved from the CLI crate's manifest directory.
fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli/ sits directly under the repo root")
        .to_path_buf()
}

#[test]
fn committed_audio_is_exactly_the_page_referenced_set() {
    let root = repo_root();
    let page = fs::read_to_string(root.join("book/src/reference/phonology.md"))
        .expect("committed phonology page");
    let mut referenced = BTreeSet::new();
    for chunk in page.split("src=\"../audio/").skip(1) {
        let name = chunk.split('"').next().expect("a closing quote");
        assert!(
            name.len() == 12 && name.ends_with(".mp3"),
            "malformed audio reference {name:?}"
        );
        referenced.insert(name.to_string());
    }
    assert!(
        !referenced.is_empty(),
        "the page must reference at least one audio clip"
    );

    let mut committed = BTreeSet::new();
    for entry in fs::read_dir(root.join("book/src/audio")).expect("book/src/audio exists") {
        let entry = entry.expect("a readable directory entry");
        committed.insert(entry.file_name().to_string_lossy().into_owned());
    }
    assert_eq!(
        referenced, committed,
        "committed clips (right) must be exactly the page's references \
         (left); run `hornvale voice` for missing clips and delete orphans"
    );
}
```

- [ ] **Step 2: Run the test to verify it passes**

Run: `cargo test -p hornvale --test audio_artifacts`
Expected: PASS.

- [ ] **Step 3: Verify it fails on an orphan (then restore)**

```bash
touch book/src/audio/deadbeef.mp3
cargo test -p hornvale --test audio_artifacts   # expect: FAIL naming deadbeef.mp3
rm book/src/audio/deadbeef.mp3
cargo test -p hornvale --test audio_artifacts   # expect: PASS
```

- [ ] **Step 4: Full gate and commit**

```bash
cargo fmt
cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings
git add cli/tests/audio_artifacts.rs
git commit -m "test(cli): committed audio must be exactly the phonology page's referenced set"
```

---

### Task 9: book sweep — chronicle, freshness, retrospective

**Files:**
- Create: `book/src/chronicle/17-audible-phonology.md`
- Modify: `book/src/SUMMARY.md` (chronicle list, after line 46's Campaign 16 entry)
- Modify: `book/src/domains/language.md` (views paragraph: two views → three)
- Create: `docs/retrospectives/campaign-17.md`

**Interfaces:** none — prose only.

- [ ] **Step 1: Chronicle entry**

Create `book/src/chronicle/17-audible-phonology.md`, following the voice and
altitude of `book/src/chronicle/16-the-tongues.md` (read it first). It must
cover, in the book's deliberate technical register: the espeak-ng mnemonic
as a third view over `Segment` (romanization for the almanac, IPA for the
eye, espeak for the ear); explicit first-vowel stress and the `[[…]]` direct
phoneme input form; CRC-32 content-addressing as the freshness mechanism
(a phonology change renames the artifact, and the set test catches it —
no waveform is ever compared); `hornvale voice` as offline authoring in the
"models author, dice roll" pattern; and the two documented approximations
(click → `tS`, ejective → `k`) as the espeak analogue of the romanization
digraphs. Add the SUMMARY.md line:

```markdown
- [Campaign 17: Audible Phonology](./chronicle/17-audible-phonology.md)
```

- [ ] **Step 2: Freshness sweep of the language chapter**

In `book/src/domains/language.md`, update the surface-views discussion:
wherever it presents romanization and IPA as *the* two views over
`Segment`, present all three (espeak added for authored audio), and add a
sentence pointing at the phonology reference page's audio column. Skim the
rest of the chapter for any "two views" phrasing.

- [ ] **Step 3: Verify the book builds and ships the audio**

```bash
mdbook build book
ls book/book/audio | head    # expect the committed mp3 clips copied through
```

- [ ] **Step 4: Retrospective**

Create `docs/retrospectives/campaign-17.md` (one page, process not product,
per decision 0020 and the style of `docs/retrospectives/campaign-y2-3.md`):
what the listen-test abandon gate cost/bought, whether validating espeak-ng
phoneme input *before* writing the spec changed the design, and anything
learned about committing binary artifacts to the book.

- [ ] **Step 5: Full gate, artifact drift check, and commit**

```bash
cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings && cargo test --workspace
cargo run -p hornvale -- phonology > book/src/reference/phonology.md
git diff --exit-code book/src/reference/phonology.md   # expect: no drift
git add book/src/chronicle/17-audible-phonology.md book/src/SUMMARY.md book/src/domains/language.md docs/retrospectives/campaign-17.md
git commit -m "docs(book): chronicle Campaign 17 — audible phonology"
```
