# The Speakable Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Glossed names parse over the drawn canon templates PLUS the
lexicon's own root forms admitted verbatim (the attested tier), so a name
compounded from a language's own words surfaces them unchanged and
phonotactic repair becomes the identity for native material.

**Architecture:** One new parse branch ("match a whole attested word
here") in `domains/language`'s `conforms` backtracker and
`repair_phonotactics` DP, fed by a new `attested_forms(&Lexicon)` helper;
wired through the single production call site (`Namer::glossed_name`) and
mirrored in `windows/lab`'s string-level validator. No stream draw is
added, removed, or reordered anywhere.

**Tech Stack:** Rust edition 2024, std + serde only (workspace allowlist).
Tests via cargo-nextest. No new dependencies.

**Spec:** `docs/superpowers/specs/2026-07-14-the-speakable-design.md`

## Global Constraints

- Determinism: no `HashMap`/`HashSet` (`BTreeMap`/`BTreeSet`/`Vec` only);
  no wall-clock; float sorts use `total_cmp`. No new stream draws — the
  attested tier is a pure draw-free function of the lexicon.
- Rebaseline, not epoch (ledger #2 / spec §5): the flip commit (Task 3)
  regenerates committed artifacts via
  `SKIP_CENSUS=1 scripts/regenerate-artifacts.sh` in the SAME commit.
  Censuses are NEVER regenerated locally; the census-tier collision pin
  (`windows/lab/tests/calibration.rs::name_collision_rate_is_measured_and_pinned`)
  re-pins at the pre-merge AWS regen, with warning to Nathan first.
- Every commit: `cargo fmt` last, then `make gate` green (fmt + clippy
  `-D warnings` + nextest + doctests). Iterate cost-ordered: fmt/clippy →
  `cargo test -p <crate>` → full gate at commit. Run once, capture
  (`2>&1 | tee /tmp/hv-test.txt`), never re-run to grep a second line.
- Every crate has `#![warn(missing_docs)]`: every new pub item, field,
  and variant gets a one-line doc comment.
- Absorb main at plan-stage boundaries: `make preflight` from this branch
  before Task 1 and after Task 3; on ancestry NO-GO merge main INTO the
  branch and re-gate.
- Worktree: `.claude/worktrees/the-speakable`, branch `the-speakable`.
  All work happens there (subagents: `cd` there and verify the branch
  FIRST; prepend `.claude/skills/dispatching-hornvale-subagents/dispatch-preamble.md`).

---

### Task 1: The attested tier in `conforms` and `repair_phonotactics`

Signatures gain an `attested: &[Vec<Segment>]` parameter; behavior with
`attested = &[]` is byte-identical to today (this task changes no
generated output — the production call site passes `&[]` until Task 3).

**Files:**
- Modify: `domains/language/src/naming.rs` (fns `conforms` ~line 522,
  `repair_phonotactics` ~line 618, enum `RepairStep` ~line 567, plus its
  module tests; production call site at ~line 288 passes `&[]` for now)

**Interfaces:**
- Produces: `pub(crate) fn attested_forms(lexicon: &Lexicon) -> Vec<Vec<Segment>>`;
  `fn conforms(segments: &[Segment], ph: &Phonology, attested: &[Vec<Segment>]) -> bool`;
  `fn repair_phonotactics(segments: Vec<Segment>, ph: &Phonology, attested: &[Vec<Segment>]) -> Vec<Segment>`.
  Task 3 wires `glossed_name` to these; Task 4's battery calls all three.
- Consumes: `Lexicon::entries()` (`lexicon.rs:155`), `LexEntry::Root`'s
  `derivation.modern: Vec<Segment>`, `Segment: Ord`.

- [ ] **Step 1: Write the failing tests** (in `naming.rs`'s existing
  `#[cfg(test)] mod tests`, using the existing `toy_repair_ph()` /
  `toy_segments()` fixtures — one vowel `a`, stop `t`, nasal `n`; onsets
  `[[Stop]]`, nuclei 1, codas `[[Nasal], []]`):

```rust
#[test]
fn an_attested_word_conforms_verbatim_even_where_canon_rejects_it() {
    // "nat" is canon-illegal in the toy phonology (a nasal can never
    // begin a syllable) but attested — the tier admits it whole.
    let ph = toy_repair_ph();
    let (a, t, n) = toy_segments();
    let word = vec![n, a, t];
    assert!(!conforms(&word, &ph, &[]), "test premise: canon rejects nat");
    let attested = vec![word.clone()];
    assert!(conforms(&word, &ph, &attested));
    // Repair of attested material is the identity.
    assert_eq!(repair_phonotactics(word.clone(), &ph, &attested), word);
}

#[test]
fn a_compound_of_attested_words_and_canon_syllables_conforms() {
    // attested "nat" + canon "ta" + attested "nat": parses as
    // [attested][canon syllable][attested] with zero edits.
    let ph = toy_repair_ph();
    let (a, t, n) = toy_segments();
    let word = vec![n, a, t];
    let attested = vec![word.clone()];
    let compound = vec![n, a, t, t, a, n, a, t];
    assert!(conforms(&compound, &ph, &attested));
    assert_eq!(repair_phonotactics(compound.clone(), &ph, &attested), compound);
}

#[test]
fn foreign_material_still_repairs_exactly_as_before() {
    // The Task-1 regression guard: with an attested tier PRESENT but not
    // matching, the old epenthesis behavior is unchanged (the tan.ta.ta
    // case from repair_breaks_an_illegal_seam_cluster_by_epenthesis).
    let ph = toy_repair_ph();
    let (a, t, n) = toy_segments();
    let attested = vec![vec![n, a, t]]; // does not occur in the seam below
    let seam = vec![t, a, n, t, t, a];
    assert_eq!(
        repair_phonotactics(seam, &ph, &attested),
        vec![t, a, n, t, a, t, a],
    );
}

#[test]
fn attested_forms_are_roots_only_deduped_longest_first() {
    // two_word_lexicon(9) holds Steeped roots for water and fire and
    // nothing else; attested_forms yields exactly those modern forms,
    // longest first, no Gap/absent concepts.
    let lex = two_word_lexicon(9);
    let forms = attested_forms(&lex);
    assert_eq!(forms.len(), 2, "exactly the two roots");
    assert!(forms[0].len() >= forms[1].len(), "longest first");
    let water = match lex.entry("water") {
        Some(LexEntry::Root { derivation, .. }) => derivation.modern.clone(),
        other => panic!("water must be a root, got {other:?}"),
    };
    assert!(forms.contains(&water));
}
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale-language an_attested_word 2>&1 | tail -5`
Expected: COMPILE FAILURE (`conforms` takes 2 arguments, `attested_forms`
not found) — a compile-level red is the failing state here.

- [ ] **Step 3: Implement.** Add the helper (near `holds_word`, which it
  mirrors in spirit):

```rust
/// The attested tier (The Speakable): every modern root form the lexicon
/// actually holds, admitted verbatim as parse units beside the drawn
/// canon templates. Descriptive phonotactics — the templates are the
/// morphology's grammar, the lexicon is its own evidence — so a name
/// compounded from the language's own words never needs repair. Deduped
/// and sorted longest-first (ties by `Segment`'s total order) so the
/// repair DP's first-match tie-break is deterministic. Draw-free and
/// pure; Gaps and Compounds contribute nothing (a compound's segments
/// are its two roots in sequence, each already attested).
pub(crate) fn attested_forms(lexicon: &Lexicon) -> Vec<Vec<Segment>> {
    let mut forms: Vec<Vec<Segment>> = lexicon
        .entries()
        .filter_map(|(_, entry)| match entry {
            LexEntry::Root { derivation, .. } if !derivation.modern.is_empty() => {
                Some(derivation.modern.clone())
            }
            _ => None,
        })
        .collect();
    forms.sort_by(|a, b| b.len().cmp(&a.len()).then_with(|| a.cmp(b)));
    forms.dedup();
    forms
}
```

Extend `conforms` — new parameter threaded through the inner `from`, with
the attested branch FIRST (order is irrelevant to the boolean but mirrors
the DP's tie-break):

```rust
fn conforms(segments: &[Segment], ph: &Phonology, attested: &[Vec<Segment>]) -> bool {
    fn from(segments: &[Segment], pos: usize, ph: &Phonology, attested: &[Vec<Segment>]) -> bool {
        if pos == segments.len() {
            return true;
        }
        for word in attested {
            if segments[pos..].starts_with(word) && from(segments, pos + word.len(), ph, attested) {
                return true;
            }
        }
        // ... existing onset/nucleus/coda loops, recursing with `attested` ...
    }
    !segments.is_empty() && from(segments, 0, ph, attested)
}
```

Extend the DP: new variant with a doc comment —

```rust
    /// Emit one attested lexicon word verbatim starting at this position
    /// (`len` segments, zero cost — the attested tier).
    Attested {
        /// How many input segments the attested word spans.
        len: usize,
    },
```

In the `for i in (0..n).rev()` loop, BEFORE the onset loop (first-evaluated
wins ties under the existing strict `<`, so attested-longest-first is the
documented tie-break):

```rust
        for word in attested {
            if segments[i..].starts_with(word) {
                let cost = cost_at(&best, i + word.len());
                if chosen.as_ref().is_none_or(|(c, _)| cost < *c) {
                    chosen = Some((cost, RepairStep::Attested { len: word.len() }));
                }
            }
        }
```

In the replay loop:

```rust
            RepairStep::Attested { len } => {
                out.extend_from_slice(&segments[i..i + len]);
                i += len;
            }
```

Update `repair_phonotactics`'s doc comment: add a leading paragraph naming
the two tiers (canon templates = the morphology's grammar; attested root
forms admitted verbatim = the lexicon's own evidence; The Speakable) and
note the identity-for-native-compounds consequence. Update the module doc
(the paragraph documenting "evolved roots only guarantee inventory
membership, not template conformance") to say the attested tier is how
that gap is closed. Update ALL existing call sites — the production one at
~line 288 (`repair_phonotactics(segments, self.ph)` →
`repair_phonotactics(segments, self.ph, &[])` for now — flipped in
Task 3) and every test call (`conforms(&x, &ph)` → `conforms(&x, &ph, &[])`
etc.).

- [ ] **Step 4: Run the crate's tests**

Run: `cargo test -p hornvale-language 2>&1 | tail -5`
Expected: PASS (all pre-existing tests green with `&[]`; four new tests
green).

- [ ] **Step 5: fmt, clippy, gate, commit**

```bash
cargo fmt && cargo clippy -p hornvale-language --all-targets -- -D warnings 2>&1 | tail -3
make gate 2>&1 | tail -5   # expect green; behavior is unchanged this task
git add domains/language/src/naming.rs
git commit -m "feat(language): attested tier in conforms/repair (The Speakable Task 1)

Parser and repair DP gain an attested-words parse unit (zero-cost,
longest-first tie-break); attested_forms() derives it from a lexicon's
Root modern forms. All call sites pass &[] — no generated output changes
in this commit."
```

---

### Task 2: Widen the Lab's string-level validator

The Lab re-validates committed names from their surface strings,
independently of naming's code path. It gains the same attested tier at
the roman level, wired to the real lexicons. Widening never un-validates,
so this commit also changes no outputs.

**Files:**
- Modify: `windows/lab/src/metrics.rs` (`is_phonotactically_valid`
  ~line 3365, `parse_syllables` ~line 3375, `phonotactic_validity`
  ~line 2473, and the module tests at ~lines 3682/3742/3746)

**Interfaces:**
- Consumes: `hornvale_worldgen::lexicon_of(world, species) -> Result<Lexicon, _>`
  (already used at metrics.rs:2525/2697/2807),
  `hornvale_language::{LexEntry, render_views}`.
- Produces: `fn is_phonotactically_valid(name: &str, ph: &Phonology, attested_roman: &[String]) -> bool`
  and `fn attested_roman_forms(lexicon: &hornvale_language::Lexicon) -> Vec<String>`
  (both private to metrics.rs; Task 3's flip relies on this validator
  accepting the new names, and the seed-0 module probe proves it live).

- [ ] **Step 1: Write the failing test** (module tests at the bottom of
  `metrics.rs`, near the existing `is_phonotactically_valid` tests — those
  build a `Phonology` by hand; reuse whatever local fixture they use):

```rust
#[test]
fn attested_roman_words_validate_where_canon_rejects_them() {
    // A name that is exactly an attested word must validate even when
    // no canon template hosts it; a name that is neither canon-parseable
    // nor attested must still fail.
    let ph = /* the existing hand-built fixture the "qw" test uses */;
    let attested = vec!["nat".to_string()];
    assert!(is_phonotactically_valid("Nat", &ph, &attested));
    assert!(!is_phonotactically_valid("Nat", &ph, &[]));
    assert!(!is_phonotactically_valid("qw", &ph, &attested));
}
```

(Adapt the attested string to the fixture's actual inventory: pick any
letter sequence the fixture's templates cannot parse, e.g. a bare coda
consonant, and attest it. The three assertions are the contract:
attested ⇒ valid, unattested+canon-illegal ⇒ invalid, junk ⇒ invalid.)

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-lab attested_roman 2>&1 | tail -5`
Expected: COMPILE FAILURE (arity) — then after mechanical arity fixes in
old tests, the new test fails on the first assertion until Step 3 lands.

- [ ] **Step 3: Implement.**

```rust
/// The attested tier at the roman level (The Speakable): the lowercased
/// roman rendering of every modern root form `lexicon` holds, deduped,
/// longest-first. The surface-string twin of
/// `hornvale_language`'s segment-level attested tier, so this validator
/// accepts exactly the names `glossed_name` now emits.
fn attested_roman_forms(lexicon: &hornvale_language::Lexicon) -> Vec<String> {
    let mut forms: Vec<String> = lexicon
        .entries()
        .filter_map(|(_, entry)| match entry {
            hornvale_language::LexEntry::Root { derivation, .. }
                if !derivation.modern.is_empty() =>
            {
                Some(hornvale_language::render_views(&derivation.modern).roman.to_lowercase())
            }
            _ => None,
        })
        .collect();
    forms.sort_by(|a, b| {
        b.chars()
            .count()
            .cmp(&a.chars().count())
            .then_with(|| a.cmp(b))
    });
    forms.dedup();
    forms
}
```

`is_phonotactically_valid` and `parse_syllables` gain
`attested_roman: &[String]`; in `parse_syllables`, before the onset loops:

```rust
    for word in attested_roman {
        let w: Vec<char> = word.chars().collect();
        if chars[pos..].starts_with(&w[..])
            && parse_syllables(chars, pos + w.len(), ph, attested_roman)
        {
            return true;
        }
    }
```

`phonotactic_validity` derives the real tier (empty on a `lexicon_of`
error, mirroring how the gloss metrics degrade):

```rust
    let attested = hornvale_worldgen::lexicon_of(v.world(), species)
        .map(|lex| attested_roman_forms(&lex))
        .unwrap_or_default();
    MetricValue::Flag(names.iter().all(|n| is_phonotactically_valid(n, &ph, &attested)))
```

Update the seed-0 live probe
(`phonotactic_validity_holds_for_every_species_name_at_seed_0`) to derive
each species' attested tier the same way and pass it; update the
empty/junk tests to pass `&[]`. Update `is_phonotactically_valid`'s doc
comment (two tiers; note the lexicon-derivation caveat at metrics.rs:2864
applies — resolve species within the roster exactly as the caller at
2473 does via `language_of_in`/`lexicon_of` together).

- [ ] **Step 4: Run the crate's tests**

Run: `cargo test -p hornvale-lab 2>&1 | tail -5`
Expected: PASS.

- [ ] **Step 5: fmt, clippy, gate, commit**

```bash
cargo fmt && make gate 2>&1 | tail -5
git add windows/lab/src/metrics.rs
git commit -m "feat(lab): attested tier in the surface-string name validator (The Speakable Task 2)

Widening only — canon-valid names stay valid, so no metric or artifact
moves in this commit; the tier goes live for generated names in Task 3."
```

---

### Task 3: The flip — glossed names use the attested tier (+ rebaseline)

The behavior change and its rebaseline land in ONE commit.

**Files:**
- Modify: `domains/language/src/naming.rs` (`glossed_name` ~line 288 and
  the module tests that model its repair,
  `settlement_names_carry_a_per_salt_stem_beyond_the_site_words` in
  particular)
- Regenerate: everything `SKIP_CENSUS=1 scripts/regenerate-artifacts.sh`
  touches (`book/src/gallery/*`, `book/src/reference/*`, lab study
  non-census outputs)

**Interfaces:**
- Consumes: Task 1's `attested_forms` + threaded signatures; Task 2's
  widened validator (the seed-0 probe must stay green against the new
  names).
- Produces: the new committed names. Tasks 4–6 assert against this
  behavior.

- [ ] **Step 1: Write the failing test** (naming.rs module tests):

```rust
#[test]
fn glossed_names_surface_their_site_words_verbatim() {
    // The Speakable's core invariant: a glossed name CONTAINS each
    // glossed concept's modern form as a contiguous segment run —
    // audible words, not repair residue. Checked at the roman level
    // via the same render path the committed fact uses.
    let ph = wordy_ph();
    let lex = two_word_lexicon(9);
    let site = SiteConcepts { concepts: &["water", "fire"] };
    let morph = MorphOptions { honorifics: false };
    let namer = Namer::new(&Seed(9), "test", &ph);
    for salt in 0..20u64 {
        for kind in [NameKind::Settlement, NameKind::Deity] {
            let (name, gloss) = namer.glossed_name(kind, salt, &morph, &site, &lex);
            for concept in gloss.split('-').filter(|c| !c.is_empty()) {
                let word = match lex.entry(concept) {
                    Some(LexEntry::Root { derivation, .. }) => {
                        render_views(&derivation.modern).roman.to_lowercase()
                    }
                    other => panic!("gloss concept {concept} must be a root, got {other:?}"),
                };
                assert!(
                    name.roman.to_lowercase().contains(&word),
                    "salt {salt} {kind:?}: name {:?} must audibly contain {concept} = {word:?}",
                    name.roman
                );
            }
        }
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-language glossed_names_surface 2>&1 | tail -8`
Expected: FAIL for at least one salt/kind (wordy_ph's drawn templates
reject some evolved forms; if it happens to pass for seed 9, the property
battery in Task 4 sweeps 64 seeds — but verify the red by temporarily
asserting over `two_word_lexicon(seed)` for seeds 0..12 in a scratch run
and keep whichever seed reds in the committed test).

- [ ] **Step 3: Implement the flip.** In `glossed_name`, after the
  candidate filtering and before the fallback branch (both paths repair —
  only the compound path has attested material, but computing the tier
  once at the top is simplest and pure):

```rust
        let attested = attested_forms(lexicon);
```

and change the one repair call:

```rust
        let mut segments = repair_phonotactics(segments, self.ph, &attested);
```

Update `glossed_name`'s doc comment (the repair sentence gains "over the
canon templates plus the lexicon's attested tier — repair is the identity
for native compounds by construction"). Update the test
`settlement_names_carry_a_per_salt_stem_beyond_the_site_words`: its
`plain` expectation becomes

```rust
        let attested = attested_forms(&lex);
        let plain = render_views(&repair_phonotactics(
            concept_segments(&lex, "water"),
            &ph,
            &attested,
        ))
        .roman;
```

(the deity == plain and settlement != plain assertions stand unchanged).
Check the other module tests' expectations still hold (they assert
structure — purity, gloss subsets, epithet keying — not repaired shapes,
so they should pass untouched).

- [ ] **Step 4: Test the crate, then the workspace**

```bash
cargo test -p hornvale-language 2>&1 | tail -5
cargo nextest run --workspace 2>&1 | tee /tmp/hv-speakable-t3.txt | tail -15
```

Expected: language green. Workspace: worldgen/lab/cli byte-identity and
artifact-comparison tests may red against stale committed artifacts —
inspect /tmp/hv-speakable-t3.txt; ONLY name-bearing expectations may move.

- [ ] **Step 5: Regenerate committed artifacts**

```bash
SKIP_CENSUS=1 bash scripts/regenerate-artifacts.sh 2>&1 | tail -5
git diff --stat book/ | tail -10
```

Expected diff: gallery almanacs / possession transcript / settlement maps'
markdown captions where names changed; `book/src/reference/dictionary-*`,
`phonology.md`, `proto-*`, `concept-registry-*`, `stream-manifest-*`
byte-identical (spec §8.4 — if any of those five moves, STOP: a stream or
lexicon changed, which this campaign must not do; investigate before
committing).

- [ ] **Step 6: Full gate + commit (flip + rebaseline together)**

```bash
cargo fmt && make gate 2>&1 | tail -5
git add domains/language/src/naming.rs book/
git commit -m "feat(language): glossed names surface their words verbatim (The Speakable Task 3)

The flip: glossed_name repairs over canon + the lexicon's attested tier,
so native compounds are identity-repaired and every previously-collapsed
name (seed 42: all bugbear deities 'Bvaash', goblin 'Neb', hobgoblin
'Fee') becomes its actual words. Artifacts rebaselined in-commit
(rebaseline-not-epoch, libm 0041 precedent); dictionary/phonology/proto/
manifest pages byte-identical as required. Census fixtures lag until the
pre-merge AWS regen (standing posture)."
```

- [ ] **Step 7: `make preflight`** (post-flip stage boundary). On
  ancestry NO-GO: merge main into the branch, re-run `make gate`.

---

### Task 4: The 64-seed property battery

**Files:**
- Modify: `domains/language/src/naming.rs` (segment-level battery in the
  module tests — `conforms`/`repair_phonotactics`/`attested_forms` are
  private)
- Create: `domains/language/tests/speakable_properties.rs` (string-level
  battery over the public API)

**Interfaces:**
- Consumes: Tasks 1+3. Public API used by the integration file:
  `draw_phonology`, `build_lexicon`, `ExposureClass`, `Namer`, `NameKind`,
  `MorphOptions`, `SiteConcepts`, `LexEntry`, `render_views`, `Envelope`,
  `ExoticSeg`, `Seed` (verify each is exported from
  `domains/language/src/lib.rs`; re-export any that isn't with a one-line
  doc comment).
- Produces: the spec §6 evidence battery; nothing downstream consumes it.

- [ ] **Step 1: Write the segment-level battery** (module tests; fails
  only if the Task 1–3 machinery is wrong, so expect green — the red was
  Task 3's Step 2):

```rust
/// An envelope swept from seed bits so the 64-seed battery crosses the
/// full phonotactic regime space — including the cluster-heavy draws
/// that caused the collapse (spec §6).
fn swept_envelope(seed: u64) -> Envelope {
    let f = |k: u64| ((seed >> k) & 3) as f64 / 3.0;
    Envelope {
        labiality: f(0),
        vowel_space: (f(2)).max(0.2),
        voicing: f(4),
        sibilance: f(6),
        voice_loudness: f(8),
        tonality: 0.0,
        exotic: ExoticSeg::None,
    }
}

#[test]
fn attested_compounds_repair_to_identity_across_the_seed_sweep() {
    // Spec §6: for 64 seeds, with the lexicon descending from a DIFFERENT
    // (permissive) proto phonology than the daughter's own drawn one —
    // the exact mismatch that caused the collapse — every root conforms
    // under its own (phonology, attested) pair and every 1-2-concept
    // compound repairs to itself.
    for seed in 0..64u64 {
        let proto = wordy_ph();
        let ph = draw_phonology(&Seed(seed), "swept", &swept_envelope(seed));
        let mut exposures = BTreeMap::new();
        for c in ["water", "fire", "moon", "shadow"] {
            exposures.insert(c.to_string(), ExposureClass::Steeped);
        }
        let lex = build_lexicon(&Seed(seed), "fam", "swept", &proto, &ph, &exposures, &[]);
        let attested = attested_forms(&lex);
        for chosen in [
            vec!["water"],
            vec!["moon"],
            vec!["water", "fire"],
            vec!["shadow", "moon"],
        ] {
            if !chosen.iter().all(|c| holds_word(&lex, c)) {
                continue; // exposure may still gap a concept; skip, don't fake
            }
            let raw = compound_segments(&lex, &chosen);
            assert!(
                conforms(&raw, &ph, &attested),
                "seed {seed}: compound {chosen:?} must conform under its own attested tier"
            );
            assert_eq!(
                repair_phonotactics(raw.clone(), &ph, &attested),
                raw,
                "seed {seed}: repair of native compound {chosen:?} must be the identity"
            );
        }
    }
}
```

(Check `build_lexicon`'s parameter order against its signature at
`lexicon.rs:237` — the fixture `two_word_lexicon` passes
`(&Seed, family, species, proto_ph, ph, &exposures, &[])`; mirror it
exactly. If `wordy_ph()`/fixture names differ, reuse the module's
existing fixtures rather than inventing new ones.)

- [ ] **Step 2: Write the string-level battery**
  (`domains/language/tests/speakable_properties.rs`, public API only) —
  the "names are audibly made of words" property, Task 3's unit test
  generalized over the sweep, plus per-salt settlement distinctness:

```rust
//! The Speakable's structural battery (spec §6): across a 64-seed sweep
//! of drawn phonologies, glossed names audibly contain the words their
//! gloss claims, and per-salt settlement names stay distinct.

use hornvale_language::/* the verified public exports, per Interfaces */;
use std::collections::{BTreeMap, BTreeSet};

// swept_envelope: same fn body as the module-test copy (5 lines; small
// enough that duplication beats exporting a test-only helper).

#[test]
fn glossed_names_audibly_contain_their_words_across_the_seed_sweep() {
    for seed in 0..64u64 {
        // proto: a fixed permissive envelope (all dims 1.0, ExoticSeg::None);
        // daughter: swept_envelope(seed); lexicon over water/fire/moon/shadow
        // Steeped, exactly as the module battery builds them.
        // For salts 0..6 and kinds {Settlement, Deity}:
        //   (name, gloss) = namer.glossed_name(kind, salt, &morph, &site, &lex)
        //   for each concept in gloss.split('-'):
        //     word = render_views(root modern segments).roman.to_lowercase()
        //     assert name.roman.to_lowercase().contains(&word)
        // And per (seed, kind=Settlement): the set of names over salts 0..6
        // has at least 5 distinct members (the stem keeps spreading them).
    }
}
```

Write the body out fully (the comment above is the shape; every line is
mechanical given the module battery — build the same fixtures, assert the
two properties). If a public export is missing for any needed item, add
the re-export in `lib.rs` with a doc comment in this same task.

- [ ] **Step 3: Run both batteries**

```bash
cargo test -p hornvale-language 2>&1 | tail -5
```
Expected: PASS, total runtime well under a minute (64 lexicon builds are
cheap; if the file exceeds ~60s it must be split or trimmed, not ignored).

- [ ] **Step 4: fmt, gate, commit**

```bash
cargo fmt && make gate 2>&1 | tail -5
git add domains/language
git commit -m "test(language): 64-seed speakability batteries (The Speakable Task 4)"
```

---

### Task 5: Seed-42 regression probe + distinctness readout

**Files:**
- Modify: `cli/tests/branches_identity.rs` (has `default_generated_seed_42()`
  and per-species helpers already)

**Interfaces:**
- Consumes: `hornvale_worldgen::NAME_GLOSS` (lib.rs:198),
  `hornvale_religion::{DEITY_NAME, beliefs_held_by}` — check
  `branches_identity.rs`'s existing imports and fact-reading helpers and
  mirror their idiom for pulling `(deity-name, name-gloss)` pairs per
  belief entity (via `beliefs_held_by` over each species flagship, or
  directly over ledger facts, whichever the file already does).
- Produces: the merged-world regression + the chronicle's measured
  readout.

- [ ] **Step 1: Write the failing-before test** (it PASSES now, post-flip;
  its value is as a permanent probe — the probe-library posture from
  TOOL-13):

```rust
#[test]
fn deities_with_distinct_glosses_carry_distinct_names_at_seed_42() {
    // The Speakable's exit regression (spec §8.1): before the attested
    // tier, EVERY bugbear deity was "Bvaash", goblin "Neb", hobgoblin
    // "Fee" — repair collapsed all glossed words to one fallback
    // syllable. Distinct gloss now implies distinct name (same-gloss
    // deities may still legitimately share one).
    let world = default_generated_seed_42();
    let mut checked = 0u32;
    for species in ["bugbear", "goblin", "hobgoblin", "kobold"] {
        // gloss -> set of deity names carrying it (skip empty-gloss beliefs)
        let mut by_gloss: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
        // ... pull (deity-name, name-gloss) pairs per the file's idiom ...
        let glosses: Vec<&String> = by_gloss.keys().collect();
        for (i, a) in glosses.iter().enumerate() {
            for b in &glosses[i + 1..] {
                let inter: Vec<_> = by_gloss[**a].intersection(&by_gloss[**b]).collect();
                assert!(
                    inter.is_empty(),
                    "{species}: glosses {a:?} and {b:?} share deity name(s) {inter:?}"
                );
                checked += 1;
            }
        }
    }
    assert!(checked > 0, "no cross-gloss pairs checked — probe is vacuous");
}

#[test]
fn the_shadow_gods_name_is_audibly_the_shadow_word_at_seed_42() {
    // The Bvaash fix, pinned by mechanism not by string: some bugbear
    // deity glossing exactly "shadow" carries the bugbear shadow word
    // (dictionary: Daoqao) audibly in its committed name.
    let world = default_generated_seed_42();
    // shadow_word = lexicon_of(&world, "bugbear") -> entry("shadow") root
    //               -> render_views(modern).roman.to_lowercase()
    // names = deity names whose belief glosses "shadow" (bugbear)
    // assert names.iter().any(|n| n.to_lowercase().contains(&shadow_word))
}
```

Fill both bodies against the file's existing helpers; keep assertions on
mechanism (containment, distinctness), never on exact name strings — the
`goblin_names_are_rebaselined_not_frozen` posture in the same file.

- [ ] **Step 2: Add the readout** (ignored, run manually once for the
  chronicle):

```rust
#[test]
#[ignore = "readout: chronicle evidence, run manually with --nocapture"]
fn deity_name_distinctness_readout() {
    // Prints, per species over seeds 0..50: distinct-deity-name count /
    // deity count. Pre-fix the collapsed species read 1/N; quote the
    // post-fix table in the chronicle entry.
    for seed in 0..50u64 {
        // build world at seed, group deity names per species, print
        // "{seed} {species}: {distinct}/{total}"
    }
}
```

- [ ] **Step 3: Run**

```bash
cargo test -p hornvale --test branches_identity 2>&1 | tail -5
cargo test -p hornvale --test branches_identity -- --ignored deity_name_distinctness_readout --nocapture 2>&1 | tee /tmp/hv-speakable-readout.txt | tail -12
```
Expected: probes PASS; readout prints ~200 lines — save the file, the
chronicle task quotes it. (50 world builds is minutes, not hours; if it
runs long, trim to 20 seeds rather than parallelizing.)

- [ ] **Step 4: fmt, gate, commit**

```bash
cargo fmt && make gate 2>&1 | tail -5
git add cli/tests/branches_identity.rs
git commit -m "test(cli): seed-42 speakability probes + distinctness readout (The Speakable Task 5)"
```

---

### Task 6: Book sweep, chronicle, retrospective

**Files:**
- Modify: `book/src/domains/language.md` (the naming/repair passage —
  grep for `repair` in it; describe the two tiers: canon templates as the
  morphology's grammar, the lexicon's attested forms as its own evidence,
  and why repair is now the identity for native compounds)
- Modify: `book/src/gallery/the-gods-seed-42.md` (hand-authored prose
  quoting pre-fix names — re-quote every quoted almanac line and inline
  name from the regenerated `almanac-seed-42-sky.md` / rebuilt worlds;
  the page's *argument* stands, only its specimens change)
- Modify: `book/src/domains/settlement.md`:173 quotes the kobold flagship
  name — verify against the regenerated almanac and update if it moved
- Create: `book/src/chronicle/the-speakable.md` (+ its `SUMMARY.md` line,
  matching how the previous chronicle entry is listed)
- Create: `docs/retrospectives/2026-07-14-the-speakable.md` (process
  lessons only, one page — decision 0020)
- Modify: `book/src/frontier/idea-registry.md` — LANG-32 `spec'd` →
  `shipped` (Where already points at the spec)

**Interfaces:**
- Consumes: Task 5's readout (`/tmp/hv-speakable-readout.txt`), the
  regenerated artifacts from Task 3, the decision ledger + followups in
  `.superpowers/sdd/` (the retro promotes the followup register).

- [ ] **Step 1: Chronicle entry.** Cover: the Bvaash investigation (what
  a reader sees: seven gods, one name), the mechanism (independent draws:
  templates vs descent; deletion-only repair; the constant fallback), the
  fix (descriptive phonotactics, the attested tier, identity by
  construction), the ideonomy overturn (negation surfaced
  templates-bend-to-words), and the measured readout table. Book
  altitude: technical, comprehensible without the code.
- [ ] **Step 2: Freshness sweep.** The three book pages above; also grep
  the book for quoted collapsed names to catch stragglers:
  `grep -rn "Bvaash\|the Neb\|the Fee" book/src/ --include="*.md"` and fix
  every stale quote (mind false positives — read each hit).
- [ ] **Step 3: Registry flip + retro.** One-line status flip; retro from
  the ledger (what autopilot resolved, the overturn, what the next
  campaign inherits — LANG-33, exonym repair).
- [ ] **Step 4: Build the book, run docs consistency**

```bash
mdbook build book 2>&1 | tail -3
cargo test -p hornvale --test docs_consistency 2>&1 | tail -3
```
Expected: both green.

- [ ] **Step 5: fmt (no-op for docs), gate, commit**

```bash
make gate 2>&1 | tail -5
git add book/ docs/retrospectives/
git commit -m "docs(book): The Speakable chronicle + freshness sweep + retro (Task 6)"
```

---

### Close (G6 — not a task for a subagent)

`closing-a-campaign` from the session: `make preflight`, present the
post-G3 ledger digest to Nathan (determinism entries first), the census
re-pin warning (collision-rate pin + census fixtures at the AWS regen),
then merge per his call. LANG-32 ships; followups promote into the retro.
