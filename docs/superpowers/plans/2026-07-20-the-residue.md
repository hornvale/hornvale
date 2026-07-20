# The Residue Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ship LANG-43 — paradigm slots (Number, Tense) whose cascade-native
form (root+affix joined at the proto stage, then evolved as one unit)
sometimes diverges from the regular form (root and affix evolved
independently, then joined) purely because `evolve`'s word-edge rules
(`FinalLoss`, `ClusterSimplify`) are positional over whatever sequence they
are handed — that divergence IS irregularity, derived, never authored.
Analogical leveling then picks which divergent roots keep their irregular
form via a derived, zero-draw resistance rank (shortest proto wins, per
Zipf's law of abbreviation), proven by a property test measuring all four
standing laws from the spec.

**Architecture:** A new module `domains/language/src/paradigm.rs` holds the
entire mechanism, additive to the crate: a per-species grammaticalization-
depth draw for Number/Tense (mirroring `morphology::morph_depths`'s shape
but its own struct, touching nothing existing), a family-cognate proto-
affix draw (reusing `morphology::draw_morph_proto` directly, promoted to
`pub(crate)`), the cascade-native/regular cell computation, and the
leveling function. One property test file proves the mechanism end-to-end.

**Tech Stack:** Rust (edition 2024), existing `hornvale-language` domain
crate primitives only (`evolve`, `affix`, `Cascade`, `Phonology`, `Segment`,
`Namer`) — no new dependencies, no new draws beyond the two mirrored from
`morph_depths`'s and `draw_morph_proto`'s existing shape.

## Global Constraints

- No `HashMap`/`HashSet` anywhere — `BTreeMap`/`BTreeSet`/`Vec` only, per
  the workspace-wide `clippy.toml` `disallowed-types` lint.
- Every public item, field, and variant gets a one-line doc comment
  (`#![warn(missing_docs)]` is already set in every `domains/language/src/`
  file and must stay set in the new one).
- Every new `pub`-boundary bare primitive (a bare `String`, `u64`, `f64`,
  etc.) needs a `type-audit:` verdict tag in its doc comment, matching the
  convention already in `morphology.rs`/`etymology.rs`
  (`bare-ok(identifier-text)` is the precedent for a bare `String`/`&str`
  used as an opaque label, not a physical quantity).
- Zero new randomness beyond the two draws mirrored from existing
  precedent (§3.1's depth draw, §3.2's family-cognate proto draw) — the
  cascade-native/regular computation (Task 3) and the leveling decision
  (Task 4) are pure functions, no `Seed`/`Stream` use.
- `cargo fmt` as the final step before every commit.
- New permanent seed-derivation streams this plan introduces (save-format
  additions, never renames): `language/<species>/grammar/depth/number`,
  `language/<species>/grammar/depth/tense`, their position streams
  (`language/<species>/grammar/number-position`,
  `language/<species>/grammar/tense-position`), and
  `language/family/<family>/morph/number/plural`,
  `language/family/<family>/morph/tense/past`. Each must be added to
  `domains/language/src/lib.rs`'s `stream_labels()` in the task that
  introduces it.

---

### Task 1: Paradigm grammaticalization depth (`ParadigmDepths`)

**Files:**
- Create: `domains/language/src/paradigm.rs`
- Modify: `domains/language/src/lib.rs` (register the module, add
  `stream_labels()` entries)

**Interfaces:**
- Produces: `pub enum ParadigmDepth` — wait, reuses `morphology::MorphDepth`
  directly (it is `pub` already: `None`/`Particle`/`Affix`) rather than a
  new enum; `pub struct ParadigmDepths { pub number_depth: MorphDepth,
  pub tense_depth: MorphDepth, pub number_position: ClassPosition, pub
  tense_position: ClassPosition }`; `pub fn paradigm_depths(seed: &Seed,
  species: &str) -> ParadigmDepths`. Later tasks (3, 4) consume
  `ParadigmDepths` only to decide whether to run the mechanism at all
  (`Affix` depth) — they take the depth as a plain argument, not by calling
  this function themselves.

- [ ] **Step 1: Write the failing test**

Create `domains/language/src/paradigm.rs` with just enough to compile a
failing test — the module doc, imports, and a stub the test can call
against (this step establishes the file; Step 3 fills in the real body):

```rust
//! LANG-43: paradigm slots (Number, Tense) whose cascade-native form can
//! diverge from a mechanically-regular one, purely as a byproduct of
//! `evolve`'s word-edge rules ([`RuleKind::FinalLoss`],
//! [`RuleKind::ClusterSimplify`]) being positional over whatever sequence
//! they are handed — the divergence is irregularity, derived, never
//! authored. See `docs/superpowers/specs/2026-07-20-the-residue-design.md`.
#![warn(missing_docs)]

use crate::morphology::{ClassPosition, MorphDepth};
use hornvale_kernel::Seed;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn paradigm_depths_is_pure() {
        let a = paradigm_depths(&Seed(1), "test");
        let b = paradigm_depths(&Seed(1), "test");
        assert_eq!(a, b);
    }
}
```

Add `pub mod paradigm;` to `domains/language/src/lib.rs` alongside the
other `pub mod` lines (after `pub mod packs;`, before `pub mod phoneme;`,
keeping the existing alphabetical order):

```rust
pub mod packs;
pub mod paradigm;
pub mod phoneme;
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale-language paradigm_depths_is_pure -- --nocapture`
Expected: FAIL to compile — `cannot find function 'paradigm_depths' in this scope` and `struct 'ParadigmDepths' is private/does not derive PartialEq` (the struct doesn't exist yet).

- [ ] **Step 3: Write the implementation**

Replace the stub body in `domains/language/src/paradigm.rs` (keep the
module doc and the `tests` module from Step 1; insert this above `#[cfg(test)]`):

```rust
/// A tongue's drawn Number/Tense grammaticalization depths and attachment
/// sides — the LANG-43 sibling of [`crate::morphology::TongueMorphology`]'s
/// evidential/noun-class depths, kept as its own additive struct (not
/// folded into `TongueMorphology`) so this campaign touches no existing
/// call site: nothing in the shipped grammar renderer consumes these
/// fields yet (spec §6, no rendering surface in V1).
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ParadigmDepths {
    /// How deeply Number (Singular/Plural) grammaticalizes.
    pub number_depth: MorphDepth,
    /// How deeply Tense (Present/Past) grammaticalizes.
    pub tense_depth: MorphDepth,
    /// Which side of the marked word the Number affix binds.
    pub number_position: ClassPosition,
    /// Which side of the marked word the Tense affix binds.
    pub tense_position: ClassPosition,
}

/// Preregistered Number-depth weights over `[None, Particle, Affix]` —
/// most attested languages mark number morphologically at least
/// optionally, so this skews toward `Affix` more than the epistemic
/// evidential/noun-class weights ([`crate::morphology`]) do; a purely
/// typological prior, unrelated to their worldview-grammaticalization
/// story.
const NUMBER_DEPTH_WEIGHTS: [f64; 3] = [30.0, 20.0, 50.0];

/// Preregistered Tense-depth weights over `[None, Particle, Affix]`.
const TENSE_DEPTH_WEIGHTS: [f64; 3] = [25.0, 25.0, 50.0];

/// The percentage chance (out of 100) the Number affix binds as a suffix
/// rather than a prefix.
const NUMBER_POSITION_SUFFIX_CHANCE: u32 = 70;

/// The percentage chance (out of 100) the Tense affix binds as a suffix
/// rather than a prefix.
const TENSE_POSITION_SUFFIX_CHANCE: u32 = 65;

/// The `weighted_index` bucket order both depth axes share: 0 = `None`,
/// 1 = `Particle`, 2 = `Affix` (matching
/// [`crate::morphology`]'s own `depth_from_bucket` convention).
fn depth_from_bucket(bucket: usize) -> MorphDepth {
    match bucket {
        0 => MorphDepth::None,
        1 => MorphDepth::Particle,
        _ => MorphDepth::Affix,
    }
}

/// Draw `species`' Number/Tense grammaticalization depths and attachment
/// sides — four permanent streams:
/// `language/<species>/grammar/depth/number`,
/// `language/<species>/grammar/depth/tense`,
/// `language/<species>/grammar/number-position`,
/// `language/<species>/grammar/tense-position`. Drawn, independent of
/// evidentiality/noun-class (never shares a stream or a weight table with
/// [`crate::morphology::morph_depths`]).
/// type-audit: bare-ok(identifier-text)
pub fn paradigm_depths(seed: &Seed, species: &str) -> ParadigmDepths {
    let mut number_stream = seed
        .derive("language")
        .derive(species)
        .derive("grammar")
        .derive("depth")
        .derive("number")
        .stream();
    let number_depth = depth_from_bucket(
        number_stream
            .weighted_index(&NUMBER_DEPTH_WEIGHTS)
            .expect("NUMBER_DEPTH_WEIGHTS is fixed and positive"),
    );

    let mut tense_stream = seed
        .derive("language")
        .derive(species)
        .derive("grammar")
        .derive("depth")
        .derive("tense")
        .stream();
    let tense_depth = depth_from_bucket(
        tense_stream
            .weighted_index(&TENSE_DEPTH_WEIGHTS)
            .expect("TENSE_DEPTH_WEIGHTS is fixed and positive"),
    );

    let mut number_pos_stream = seed
        .derive("language")
        .derive(species)
        .derive("grammar")
        .derive("number-position")
        .stream();
    let number_position = if number_pos_stream.range_u32(1, 100) <= NUMBER_POSITION_SUFFIX_CHANCE {
        ClassPosition::Suffix
    } else {
        ClassPosition::Prefix
    };

    let mut tense_pos_stream = seed
        .derive("language")
        .derive(species)
        .derive("grammar")
        .derive("tense-position")
        .stream();
    let tense_position = if tense_pos_stream.range_u32(1, 100) <= TENSE_POSITION_SUFFIX_CHANCE {
        ClassPosition::Suffix
    } else {
        ClassPosition::Prefix
    };

    ParadigmDepths {
        number_depth,
        tense_depth,
        number_position,
        tense_position,
    }
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test -p hornvale-language paradigm_depths_is_pure -- --nocapture`
Expected: PASS (1 test).

- [ ] **Step 5: Add a distribution test and stream_labels entries**

Append to the `tests` module in `domains/language/src/paradigm.rs`:

```rust
    #[test]
    fn paradigm_depths_covers_all_three_buckets_across_many_seeds() {
        // Not a single fixed outcome — confirm the weighted draw actually
        // reaches every MorphDepth bucket over enough seeds, the same
        // sanity check style morph_depths' own test suite uses.
        let mut saw_none = false;
        let mut saw_particle = false;
        let mut saw_affix = false;
        for i in 0..200u64 {
            let d = paradigm_depths(&Seed(i), "test");
            match d.number_depth {
                MorphDepth::None => saw_none = true,
                MorphDepth::Particle => saw_particle = true,
                MorphDepth::Affix => saw_affix = true,
            }
        }
        assert!(saw_none && saw_particle && saw_affix);
    }
```

Add to `domains/language/src/lib.rs`'s `stream_labels()` function (find the
existing block ending `"language/<species>/grammar/class-position"` and
insert immediately after that tuple's closing `),`):

```rust
        (
            "language/<species>/grammar/depth/number",
            "LANG-43: the species' drawn Number grammaticalization depth (None/Particle/Affix), independent of evidentiality/noun-class",
        ),
        (
            "language/<species>/grammar/depth/tense",
            "LANG-43: the species' drawn Tense grammaticalization depth (None/Particle/Affix)",
        ),
        (
            "language/<species>/grammar/number-position",
            "LANG-43: which side of the marked word the Number affix binds",
        ),
        (
            "language/<species>/grammar/tense-position",
            "LANG-43: which side of the marked word the Tense affix binds",
        ),
```

- [ ] **Step 6: Run the full paradigm module's tests and the crate's existing suite**

Run: `cargo test -p hornvale-language paradigm:: -- --nocapture`
Expected: PASS (2 tests: `paradigm_depths_is_pure`,
`paradigm_depths_covers_all_three_buckets_across_many_seeds`).

Run: `cargo test -p hornvale-language`
Expected: PASS, same count as before this task plus the 2 new tests — no
existing test's outcome changes (nothing existing was touched except an
additive `pub mod` line and additive `stream_labels()` entries).

- [ ] **Step 7: Commit**

```bash
git add domains/language/src/paradigm.rs domains/language/src/lib.rs
git commit -m "feat(language): paradigm grammaticalization depth (Number/Tense) — the-residue T1"
```

---

### Task 2: Family-cognate paradigm affix proto-forms

**Files:**
- Modify: `domains/language/src/morphology.rs` (promote `draw_morph_proto`
  to `pub(crate)`, no behavior change)
- Modify: `domains/language/src/paradigm.rs`
- Modify: `domains/language/src/lib.rs` (`stream_labels()` entries)

**Interfaces:**
- Consumes: `crate::morphology::draw_morph_proto(seed: &Seed, family: &str,
  axis: &str, value: &str, proto_ph: &Phonology) -> Vec<Segment>` (promoted
  visibility, Step 1).
- Produces: `pub fn draw_paradigm_affix_proto(seed: &Seed, family: &str,
  axis: &str, value: &str, proto_ph: &Phonology) -> Vec<Segment>` — a thin,
  documented re-export-by-call so `paradigm.rs`'s own callers (Task 3's
  tests, and any future integration) never need to reach into
  `crate::morphology` directly. Task 3 consumes this signature.

- [ ] **Step 1: Promote `draw_morph_proto`'s visibility**

In `domains/language/src/morphology.rs`, change:

```rust
fn draw_morph_proto(
```

to:

```rust
pub(crate) fn draw_morph_proto(
```

(Leave the function body and its doc comment exactly as they are — this is
a visibility-only change, no behavior change, no existing call site is
affected since `pub(crate)` is a strict widening of `private`.)

- [ ] **Step 2: Write the failing test**

Append to `domains/language/src/paradigm.rs`'s `tests` module:

```rust
    use crate::phoneme::{Backness, Height, Tone};
    use crate::phonology::Phonology;

    fn tiny_phonology() -> Phonology {
        Phonology {
            inventory: vec![
                Segment::Consonant {
                    place: crate::phoneme::Place::Alveolar,
                    manner: crate::phoneme::Manner::Stop,
                    voiced: false,
                },
                Segment::Vowel {
                    height: Height::Mid,
                    backness: Backness::Front,
                    rounded: false,
                    tone: Tone::Neutral,
                },
            ],
            onsets: vec![vec![]],
            nuclei: 1,
            codas: vec![vec![]],
        }
    }

    #[test]
    fn paradigm_affix_proto_is_family_cognate() {
        // Same (seed, family, axis, value) → identical proto, twice — the
        // family-cognate law every other family-shared proto in this crate
        // upholds (draw_morph_proto's own doc, mirrored here).
        let ph = tiny_phonology();
        let a = draw_paradigm_affix_proto(&Seed(3), "goblinoid", "number", "plural", &ph);
        let b = draw_paradigm_affix_proto(&Seed(3), "goblinoid", "number", "plural", &ph);
        assert_eq!(a, b);
        assert!(!a.is_empty());
    }
```

Add `use crate::phoneme::Segment;` to the top-level `use` block in
`domains/language/src/paradigm.rs` (alongside the existing
`use crate::morphology::{ClassPosition, MorphDepth};` line).

- [ ] **Step 3: Run test to verify it fails**

Run: `cargo test -p hornvale-language paradigm_affix_proto_is_family_cognate -- --nocapture`
Expected: FAIL to compile — `cannot find function 'draw_paradigm_affix_proto' in this scope`.

- [ ] **Step 4: Write the implementation**

Insert into `domains/language/src/paradigm.rs`, above the `#[cfg(test)]`
module:

```rust
/// Draw `family`'s proto-affix for one paradigm axis-value (e.g.
/// `axis="number", value="plural"`; `axis="tense", value="past"`) — the
/// family-cognate law: every daughter's affix traces to the SAME family
/// proto (only `family`/`axis`/`value` key the draw, never the daughter or
/// its cascade), diverging only through each daughter's own [`crate::
/// etymology::evolve`], exactly like every other family-shared morpheme in
/// this crate. Delegates to [`crate::morphology::draw_morph_proto`] (same
/// one-syllable-fill mechanism every family-cognate proto in this crate
/// uses) rather than duplicating it. New permanent streams:
/// `language/family/<family>/morph/number/plural`,
/// `language/family/<family>/morph/tense/past`.
/// type-audit: bare-ok(identifier-text)
pub fn draw_paradigm_affix_proto(
    seed: &Seed,
    family: &str,
    axis: &str,
    value: &str,
    proto_ph: &crate::phonology::Phonology,
) -> Vec<Segment> {
    crate::morphology::draw_morph_proto(seed, family, axis, value, proto_ph)
}
```

- [ ] **Step 5: Run test to verify it passes**

Run: `cargo test -p hornvale-language paradigm_affix_proto_is_family_cognate -- --nocapture`
Expected: PASS.

- [ ] **Step 6: Add the stream_labels() entries**

Add to `domains/language/src/lib.rs`'s `stream_labels()`, immediately after
the `"language/family/<family>/morph/class/<value>"` tuple's closing `),`:

```rust
        (
            "language/family/<family>/morph/number/plural",
            "LANG-43: the family's Plural affix proto-form, shared by every daughter (family-cognate law)",
        ),
        (
            "language/family/<family>/morph/tense/past",
            "LANG-43: the family's Past-tense affix proto-form, shared by every daughter",
        ),
```

- [ ] **Step 7: Run the full paradigm module's tests and the crate's existing suite**

Run: `cargo test -p hornvale-language paradigm:: -- --nocapture`
Expected: PASS (4 tests total: the 2 from Task 1 plus
`paradigm_affix_proto_is_family_cognate`, plus this task added exactly 1 new
test — count them against the running total, not a specific number, since
Task 1 already established 2).

Run: `cargo test -p hornvale-language`
Expected: PASS, no existing test's outcome changes.

- [ ] **Step 8: Commit**

```bash
git add domains/language/src/paradigm.rs domains/language/src/morphology.rs domains/language/src/lib.rs
git commit -m "feat(language): family-cognate paradigm affix proto-forms — the-residue T2"
```

---

### Task 3: The cascade-native / regular cell computation (`ParadigmCell`)

This is the core mechanism (spec §3.3): the whole reason two candidate
forms can differ is that `evolve`'s `FinalLoss` and `ClusterSimplify` rules
check position (`segs.last()`, `segs[0]`/`segs[1]`) over *whatever sequence
they are handed* — verified directly in
`domains/language/src/etymology.rs`'s `apply_final_loss`/
`apply_cluster_simplify` (no morpheme-boundary marker exists anywhere in a
`Vec<Segment>`). Joining root-proto + affix-proto *before* calling `evolve`
therefore shifts which segment counts as "final" or "initial" relative to
evolving the root alone — no new phonological rule is needed, only calling
the two existing primitives (`evolve`, `affix`) in a different order.

**Files:**
- Modify: `domains/language/src/paradigm.rs`

**Interfaces:**
- Consumes: `crate::etymology::{Cascade, Derivation, evolve}`,
  `crate::morphology::{ClassPosition, MorphForm, affix}`,
  `crate::phonology::Phonology`, `crate::phoneme::Segment`.
- Produces: `pub struct ParadigmCell { pub cascade_native: Derivation, pub
  regular_root: Derivation, pub regular_affix: Derivation, pub
  regular_form: MorphForm, pub is_irregular: bool }`; `pub fn
  realize_paradigm_cell(root_proto: &[Segment], affix_proto: &[Segment],
  position: ClassPosition, cascade: &Cascade, ph: &Phonology) ->
  ParadigmCell`. Task 4 consumes `ParadigmCell.is_irregular` and
  `ParadigmCell.cascade_native.modern`/`regular_form.segments`.

- [ ] **Step 1: Write the failing tests**

Append to `domains/language/src/paradigm.rs`'s `tests` module (these
literal segments were traced by hand against `apply_final_loss`'s and
`lenition`'s exact bodies in `etymology.rs`, so the expected outputs below
are exact, not approximate):

```rust
    use crate::etymology::{Cascade, RuleKind, SoundRule};
    use crate::phoneme::{Manner, Place};

    fn t() -> Segment {
        Segment::Consonant {
            place: Place::Alveolar,
            manner: Manner::Stop,
            voiced: false,
        }
    }

    fn a() -> Segment {
        Segment::Vowel {
            height: Height::Low,
            backness: Backness::Central,
            rounded: false,
            tone: Tone::Neutral,
        }
    }

    fn e() -> Segment {
        Segment::Vowel {
            height: Height::Mid,
            backness: Backness::Front,
            rounded: false,
            tone: Tone::Neutral,
        }
    }

    fn edge_test_phonology() -> Phonology {
        Phonology {
            inventory: vec![
                t(),
                a(),
                e(),
                Segment::Consonant {
                    place: Place::Alveolar,
                    manner: Manner::Stop,
                    voiced: true,
                }, // d, for the lenition case's Lenition output
            ],
            onsets: vec![vec![]],
            nuclei: 1,
            codas: vec![vec![Manner::Stop], vec![]],
        }
    }

    #[test]
    fn final_loss_makes_a_suffixed_root_irregular() {
        // Root "tat" (t,a,t) ends in a consonant: evolved ALONE under
        // FinalLoss, the final /t/ drops ("ta"). But joined with a
        // vowel-initial suffix /e/ BEFORE evolution ("tate"), that same
        // /t/ is no longer word-final — FinalLoss checks the LAST segment
        // of whatever sequence it's given (etymology.rs's apply_final_loss),
        // and the joined sequence's last segment is the suffix's /e/, a
        // vowel, so FinalLoss never fires on the joined form at all.
        let ph = edge_test_phonology();
        let cascade = Cascade {
            rules: vec![SoundRule {
                kind: RuleKind::FinalLoss,
                param: 0,
            }],
        };
        let root_proto = vec![t(), a(), t()];
        let affix_proto = vec![e()];

        let cell = realize_paradigm_cell(
            &root_proto,
            &affix_proto,
            ClassPosition::Suffix,
            &cascade,
            &ph,
        );

        assert_eq!(cell.regular_form.segments, vec![t(), a(), e()], "regular: root's own /t/ already dropped before affixing");
        assert_eq!(cell.cascade_native.modern, vec![t(), a(), t(), e()], "cascade-native: /t/ survives, no longer word-final in the joined form");
        assert!(cell.is_irregular);
    }

    #[test]
    fn a_position_independent_rule_never_diverges() {
        // Lenition is per-segment and position-independent (etymology.rs's
        // apply_segment_rule maps every segment regardless of index), so
        // joining before or after evolution can never change its outcome —
        // confirms the mechanism is non-degenerate: SOME cascades/roots
        // diverge (above), this one never does.
        let ph = edge_test_phonology();
        let cascade = Cascade {
            rules: vec![SoundRule {
                kind: RuleKind::Lenition,
                param: 0,
            }],
        };
        let root_proto = vec![t(), a()];
        let affix_proto = vec![e()];

        let cell = realize_paradigm_cell(
            &root_proto,
            &affix_proto,
            ClassPosition::Suffix,
            &cascade,
            &ph,
        );

        assert_eq!(cell.cascade_native.modern, cell.regular_form.segments);
        assert!(!cell.is_irregular);
    }

    #[test]
    fn realize_paradigm_cell_is_pure() {
        let ph = edge_test_phonology();
        let cascade = Cascade {
            rules: vec![SoundRule {
                kind: RuleKind::FinalLoss,
                param: 0,
            }],
        };
        let root_proto = vec![t(), a(), t()];
        let affix_proto = vec![e()];

        let a_cell = realize_paradigm_cell(&root_proto, &affix_proto, ClassPosition::Suffix, &cascade, &ph);
        let b_cell = realize_paradigm_cell(&root_proto, &affix_proto, ClassPosition::Suffix, &cascade, &ph);
        assert_eq!(a_cell.cascade_native.modern, b_cell.cascade_native.modern);
        assert_eq!(a_cell.regular_form.segments, b_cell.regular_form.segments);
        assert_eq!(a_cell.is_irregular, b_cell.is_irregular);
    }
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p hornvale-language final_loss_makes_a_suffixed_root_irregular a_position_independent_rule_never_diverges realize_paradigm_cell_is_pure -- --nocapture`
Expected: FAIL to compile — `cannot find function 'realize_paradigm_cell'`, `cannot find struct 'ParadigmCell'`.

- [ ] **Step 3: Write the implementation**

Insert into `domains/language/src/paradigm.rs`, above the `#[cfg(test)]`
module (add `use crate::etymology::{Cascade, Derivation, evolve};` and
`use crate::morphology::{MorphForm, affix};` to the top-level `use` block):

```rust
/// One root's paradigm-cell computation for one axis value (spec §3.3):
/// both candidate modern forms, kept fully traceable, and whether they
/// diverge. `regular_root`/`regular_affix` are kept as their own
/// [`Derivation`]s (rather than folding the regular path into one
/// `Derivation`) because "regular" is never a single `evolve` call — it is
/// two independent calls, joined — so keeping both sub-derivations is
/// MORE traceable than a single composite would be, not less.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParadigmCell {
    /// Root-proto and affix-proto joined at the segment level BEFORE
    /// evolution, then evolved as one sequence — the order that lets
    /// `evolve`'s word-edge rules see the true joined boundary.
    pub cascade_native: Derivation,
    /// The root's own derivation, evolved independently of the affix.
    pub regular_root: Derivation,
    /// The affix's own derivation, evolved independently of the root.
    pub regular_affix: Derivation,
    /// `regular_root.modern` and `regular_affix.modern` joined at the
    /// segment level AFTER both were already evolved — today's implicit
    /// grammaticalization order (`crate::morphology::affix`'s existing
    /// call sites all join two already-evolved sides).
    pub regular_form: MorphForm,
    /// Whether `cascade_native.modern` differs from `regular_form.segments`
    /// — the irregularity signal itself, derived, never drawn.
    pub is_irregular: bool,
}

/// Compute both candidate modern forms for one root's one paradigm cell
/// (spec §3.3) and whether they diverge. Pure and total: same inputs
/// always produce the same [`ParadigmCell`], mirroring [`evolve`]'s own
/// purity law — this function calls `evolve` and
/// [`crate::morphology::affix`] and nothing else, so it inherits their
/// purity directly.
pub fn realize_paradigm_cell(
    root_proto: &[Segment],
    affix_proto: &[Segment],
    position: ClassPosition,
    cascade: &Cascade,
    ph: &Phonology,
) -> ParadigmCell {
    let regular_root = evolve(root_proto, cascade, ph);
    let regular_affix = evolve(affix_proto, cascade, ph);
    let regular_form = affix(&regular_root.modern, &regular_affix.modern, position);

    let mut joined_proto = Vec::with_capacity(root_proto.len() + affix_proto.len());
    match position {
        ClassPosition::Prefix => {
            joined_proto.extend_from_slice(affix_proto);
            joined_proto.extend_from_slice(root_proto);
        }
        ClassPosition::Suffix => {
            joined_proto.extend_from_slice(root_proto);
            joined_proto.extend_from_slice(affix_proto);
        }
    }
    let cascade_native = evolve(&joined_proto, cascade, ph);

    let is_irregular = cascade_native.modern != regular_form.segments;

    ParadigmCell {
        cascade_native,
        regular_root,
        regular_affix,
        regular_form,
        is_irregular,
    }
}
```

Add `use crate::phonology::Phonology;` to the top-level `use` block if not
already present from Task 2.

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p hornvale-language final_loss_makes_a_suffixed_root_irregular a_position_independent_rule_never_diverges realize_paradigm_cell_is_pure -- --nocapture`
Expected: PASS (3 tests).

- [ ] **Step 5: Run the full paradigm module's tests and the crate's existing suite**

Run: `cargo test -p hornvale-language paradigm:: -- --nocapture`
Expected: PASS (7 tests total: 2 from Task 1, 1 from Task 2, 3 from this
task, plus this step itself adds no new test).

Run: `cargo test -p hornvale-language`
Expected: PASS, no existing test's outcome changes.

- [ ] **Step 6: Commit**

```bash
git add domains/language/src/paradigm.rs
git commit -m "feat(language): cascade-native vs regular paradigm cell — the-residue T3"
```

---

### Task 4: Analogical leveling (`level_paradigm`)

**Files:**
- Modify: `domains/language/src/paradigm.rs`

**Interfaces:**
- Consumes: `ParadigmCell` (Task 3), `std::collections::BTreeMap`,
  `std::collections::BTreeSet`.
- Produces: `pub struct LeveledCell { pub cell: ParadigmCell, pub survived:
  bool }`; `pub fn level_paradigm(cells: &BTreeMap<String, ParadigmCell>,
  root_protos: &BTreeMap<String, Vec<Segment>>, leveling_fraction: f64) ->
  BTreeMap<String, LeveledCell>`. Task 5's property test consumes this
  signature directly.

- [ ] **Step 1: Write the failing test**

Append to `domains/language/src/paradigm.rs`'s `tests` module (add
`use std::collections::BTreeMap;` to the top-level `use` block):

```rust
    #[test]
    fn leveling_keeps_the_shortest_quartile_irregular() {
        // 8 roots, all ending in /t/ (all diverge under FinalLoss, per
        // final_loss_makes_a_suffixed_root_irregular's own mechanism),
        // lengths 2..=9 segments. leveling_fraction=0.25 → the 2 shortest
        // survive as irregular; the other 6 regularize.
        let ph = edge_test_phonology();
        let cascade = Cascade {
            rules: vec![SoundRule {
                kind: RuleKind::FinalLoss,
                param: 0,
            }],
        };
        let affix_proto = vec![e()];

        let mut root_protos: BTreeMap<String, Vec<Segment>> = BTreeMap::new();
        let mut cells: BTreeMap<String, ParadigmCell> = BTreeMap::new();
        for len in 2..=9usize {
            let id = format!("root-{len:02}");
            // Alternate a/t to keep it a legal onset-free CV*C shape, always
            // ending in /t/ so every one of these diverges under FinalLoss.
            let mut proto = Vec::with_capacity(len);
            for i in 0..len {
                proto.push(if i % 2 == 0 { t() } else { a() });
            }
            if !matches!(proto.last(), Some(s) if *s == t()) {
                proto.push(t());
            }
            let cell = realize_paradigm_cell(&proto, &affix_proto, ClassPosition::Suffix, &cascade, &ph);
            assert!(cell.is_irregular, "root-{len:02} must diverge for this test's premise to hold");
            root_protos.insert(id.clone(), proto);
            cells.insert(id, cell);
        }

        let leveled = level_paradigm(&cells, &root_protos, 0.25);

        let survivor_count = leveled.values().filter(|lc| lc.survived).count();
        assert_eq!(survivor_count, 2, "round(8 * 0.25) == 2 survivors");

        let mut survivor_lengths: Vec<usize> = leveled
            .iter()
            .filter(|(_, lc)| lc.survived)
            .map(|(id, _)| root_protos[id].len())
            .collect();
        survivor_lengths.sort_unstable();
        assert_eq!(survivor_lengths, vec![2, 3], "the two SHORTEST divergent roots survive");

        let leveled_away_count = leveled.values().filter(|lc| lc.cell.is_irregular && !lc.survived).count();
        assert_eq!(leveled_away_count, 6, "the remaining 6 divergent roots regularize");
    }
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale-language leveling_keeps_the_shortest_quartile_irregular -- --nocapture`
Expected: FAIL to compile — `cannot find function 'level_paradigm'`,
`cannot find struct 'LeveledCell'`.

- [ ] **Step 3: Write the implementation**

Insert into `domains/language/src/paradigm.rs`, above the `#[cfg(test)]`
module (add `use std::collections::{BTreeMap, BTreeSet};` to the top-level
`use` block):

```rust
/// One root's paradigm cell after analogical leveling (spec §3.4): which
/// form actually surfaces.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LeveledCell {
    /// The underlying cell computation (Task 3), unchanged.
    pub cell: ParadigmCell,
    /// True if this cell's `cascade_native` form survived leveling (stayed
    /// irregular); false if it regularized to `regular_form`, or if it was
    /// never divergent in the first place (nothing to level).
    pub survived: bool,
}

/// Apply analogical leveling (spec §3.4) across one paradigm category's
/// cells: rank the DIVERGENT roots by their own proto-root segment length
/// (shortest = most resistant, per Zipf's law of abbreviation), and let the
/// shortest `leveling_fraction` (0.0-1.0, rounded to the nearest whole
/// survivor count) keep their `cascade_native` form; the rest regularize.
/// Non-divergent cells are untouched (nothing to level). Deterministic: a
/// `Vec` sort by `(length, id)` — never a `HashMap`, never a draw — so
/// equal-length roots break ties by their own `RootId`'s `Ord` (the
/// `BTreeMap` key's natural alphabetical order), never by insertion order.
pub fn level_paradigm(
    cells: &BTreeMap<String, ParadigmCell>,
    root_protos: &BTreeMap<String, Vec<Segment>>,
    leveling_fraction: f64,
) -> BTreeMap<String, LeveledCell> {
    let mut divergent: Vec<&String> = cells
        .iter()
        .filter(|(_, cell)| cell.is_irregular)
        .map(|(id, _)| id)
        .collect();
    divergent.sort_by(|a, b| {
        let len_a = root_protos[*a].len();
        let len_b = root_protos[*b].len();
        len_a.cmp(&len_b).then_with(|| a.cmp(b))
    });

    let survivor_count = ((divergent.len() as f64) * leveling_fraction).round() as usize;
    let survivors: BTreeSet<&String> = divergent.iter().take(survivor_count).copied().collect();

    cells
        .iter()
        .map(|(id, cell)| {
            let survived = cell.is_irregular && survivors.contains(id);
            (
                id.clone(),
                LeveledCell {
                    cell: cell.clone(),
                    survived,
                },
            )
        })
        .collect()
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test -p hornvale-language leveling_keeps_the_shortest_quartile_irregular -- --nocapture`
Expected: PASS.

- [ ] **Step 5: Run the full paradigm module's tests and the crate's existing suite**

Run: `cargo test -p hornvale-language paradigm:: -- --nocapture`
Expected: PASS (8 tests total).

Run: `cargo test -p hornvale-language`
Expected: PASS, no existing test's outcome changes.

- [ ] **Step 6: Commit**

```bash
git add domains/language/src/paradigm.rs
git commit -m "feat(language): analogical leveling via derived resistance rank — the-residue T4"
```

---

### Task 5: End-to-end property test proving the four standing laws

**Files:**
- Create: `domains/language/tests/paradigm_properties.rs` (mirrors the
  crate's existing property-test convention, e.g.
  `domains/astronomy/tests/genesis_properties.rs`,
  `domains/terrain/tests/tectonic_properties.rs` — one file, real seeds,
  measured assertions, not narrated ones)

**Interfaces:**
- Consumes: `hornvale_language::paradigm::{ParadigmCell, LeveledCell,
  realize_paradigm_cell, level_paradigm}` (all `pub` from Tasks 3-4),
  `hornvale_language::etymology::{Cascade, RuleKind, SoundRule}`,
  `hornvale_language::phonology::Phonology`,
  `hornvale_language::phoneme::{Segment, Place, Manner, Height, Backness,
  Tone}`, `hornvale_language::morphology::ClassPosition`.
- Produces: nothing consumed by later tasks — this is the terminal proof
  artifact.

- [ ] **Step 1: Write the property test**

Create `domains/language/tests/paradigm_properties.rs`:

```rust
//! LANG-43 property tests: the four standing laws from
//! `docs/superpowers/specs/2026-07-20-the-residue-design.md` §4, measured
//! directly against a real multi-root scenario — never narrated.
#![warn(missing_docs)]

use hornvale_language::etymology::{Cascade, RuleKind, SoundRule};
use hornvale_language::morphology::ClassPosition;
use hornvale_language::paradigm::{level_paradigm, realize_paradigm_cell};
use hornvale_language::phoneme::{Backness, Height, Manner, Place, Segment, Tone};
use hornvale_language::phonology::Phonology;
use std::collections::BTreeMap;

fn t() -> Segment {
    Segment::Consonant {
        place: Place::Alveolar,
        manner: Manner::Stop,
        voiced: false,
    }
}

fn a() -> Segment {
    Segment::Vowel {
        height: Height::Low,
        backness: Backness::Central,
        rounded: false,
        tone: Tone::Neutral,
    }
}

fn e() -> Segment {
    Segment::Vowel {
        height: Height::Mid,
        backness: Backness::Front,
        rounded: false,
        tone: Tone::Neutral,
    }
}

fn i() -> Segment {
    Segment::Vowel {
        height: Height::High,
        backness: Backness::Front,
        rounded: false,
        tone: Tone::Neutral,
    }
}

fn scenario_phonology() -> Phonology {
    Phonology {
        inventory: vec![t(), a(), e(), i()],
        onsets: vec![vec![]],
        nuclei: 1,
        codas: vec![vec![Manner::Stop], vec![]],
    }
}

/// 12 roots: 8 end in a consonant (/t/, diverge under FinalLoss), 4 end in
/// a vowel (/i/, never diverge — FinalLoss's own trigger, `is_consonant`
/// on the last segment, never fires). Lengths span 2..=13, deliberately
/// mixed between the two groups so divergence tracks ROOT SHAPE, not root
/// length or insertion order.
fn build_scenario() -> (BTreeMap<String, Vec<Segment>>, Vec<Segment>) {
    let mut root_protos = BTreeMap::new();
    for (idx, len) in [2, 3, 4, 5, 6, 7, 8, 9].into_iter().enumerate() {
        let mut proto = Vec::with_capacity(len);
        for i in 0..len {
            proto.push(if i % 2 == 0 { t() } else { a() });
        }
        if !matches!(proto.last(), Some(s) if *s == t()) {
            proto.push(t());
        }
        root_protos.insert(format!("consonant-final-{idx:02}"), proto);
    }
    for (idx, len) in [3, 5, 7, 9].into_iter().enumerate() {
        let mut proto = Vec::with_capacity(len);
        for i in 0..len {
            proto.push(if i % 2 == 0 { t() } else { a() });
        }
        proto.push(i());
        root_protos.insert(format!("vowel-final-{idx:02}"), proto);
    }
    (root_protos, vec![e()])
}

#[test]
fn purity_same_inputs_same_output() {
    let ph = scenario_phonology();
    let cascade = Cascade {
        rules: vec![SoundRule {
            kind: RuleKind::FinalLoss,
            param: 0,
        }],
    };
    let (root_protos, affix_proto) = build_scenario();
    let root = &root_protos["consonant-final-03"];

    let a_run = realize_paradigm_cell(root, &affix_proto, ClassPosition::Suffix, &cascade, &ph);
    let b_run = realize_paradigm_cell(root, &affix_proto, ClassPosition::Suffix, &cascade, &ph);
    assert_eq!(a_run.cascade_native.modern, b_run.cascade_native.modern);
    assert_eq!(a_run.regular_form.segments, b_run.regular_form.segments);
    assert_eq!(a_run.is_irregular, b_run.is_irregular);
}

#[test]
fn non_degeneracy_some_diverge_some_dont() {
    let ph = scenario_phonology();
    let cascade = Cascade {
        rules: vec![SoundRule {
            kind: RuleKind::FinalLoss,
            param: 0,
        }],
    };
    let (root_protos, affix_proto) = build_scenario();

    let mut irregular_count = 0;
    let mut regular_count = 0;
    for (id, proto) in &root_protos {
        let cell = realize_paradigm_cell(proto, &affix_proto, ClassPosition::Suffix, &cascade, &ph);
        if cell.is_irregular {
            irregular_count += 1;
            assert!(id.starts_with("consonant-final"), "{id} diverged unexpectedly");
        } else {
            regular_count += 1;
            assert!(id.starts_with("vowel-final"), "{id} failed to diverge unexpectedly");
        }
    }
    assert_eq!(irregular_count, 8, "all 8 consonant-final roots diverge");
    assert_eq!(regular_count, 4, "all 4 vowel-final roots stay regular");
}

#[test]
fn leveling_suppresses_a_strict_subset() {
    let ph = scenario_phonology();
    let cascade = Cascade {
        rules: vec![SoundRule {
            kind: RuleKind::FinalLoss,
            param: 0,
        }],
    };
    let (root_protos, affix_proto) = build_scenario();

    let cells: BTreeMap<String, _> = root_protos
        .iter()
        .map(|(id, proto)| {
            (
                id.clone(),
                realize_paradigm_cell(proto, &affix_proto, ClassPosition::Suffix, &cascade, &ph),
            )
        })
        .collect();
    let raw_divergence_count = cells.values().filter(|c| c.is_irregular).count();
    assert_eq!(raw_divergence_count, 8);

    let leveled = level_paradigm(&cells, &root_protos, 0.25);
    let survivor_count = leveled.values().filter(|lc| lc.survived).count();

    assert!(
        survivor_count < raw_divergence_count,
        "leveling must suppress a strict subset: {survivor_count} survivors of {raw_divergence_count} divergent"
    );
    assert_eq!(survivor_count, 2, "round(8 * 0.25) == 2");
}

#[test]
fn the_frequency_prediction_shorter_roots_survive() {
    // Preregistered directional claim (spec §4 law 4): mean proto length of
    // post-leveling survivors is strictly less than the mean proto length
    // of the divergent roots leveling erased. Measured, not narrated.
    let ph = scenario_phonology();
    let cascade = Cascade {
        rules: vec![SoundRule {
            kind: RuleKind::FinalLoss,
            param: 0,
        }],
    };
    let (root_protos, affix_proto) = build_scenario();

    let cells: BTreeMap<String, _> = root_protos
        .iter()
        .map(|(id, proto)| {
            (
                id.clone(),
                realize_paradigm_cell(proto, &affix_proto, ClassPosition::Suffix, &cascade, &ph),
            )
        })
        .collect();
    let leveled = level_paradigm(&cells, &root_protos, 0.25);

    let survivor_lengths: Vec<f64> = leveled
        .iter()
        .filter(|(_, lc)| lc.survived)
        .map(|(id, _)| root_protos[id].len() as f64)
        .collect();
    let leveled_away_lengths: Vec<f64> = leveled
        .iter()
        .filter(|(_, lc)| lc.cell.is_irregular && !lc.survived)
        .map(|(id, _)| root_protos[id].len() as f64)
        .collect();

    assert!(!survivor_lengths.is_empty());
    assert!(!leveled_away_lengths.is_empty());

    let mean = |xs: &[f64]| xs.iter().sum::<f64>() / xs.len() as f64;
    let survivor_mean = mean(&survivor_lengths);
    let leveled_away_mean = mean(&leveled_away_lengths);

    assert!(
        survivor_mean < leveled_away_mean,
        "survivors' mean proto length ({survivor_mean}) must be less than leveled-away roots' mean ({leveled_away_mean})"
    );
}

#[test]
fn every_candidate_form_carries_a_derivation() {
    // Law 5: no candidate form is ever a bare string — both paths remain
    // full Derivations (regular via its two sub-derivations) through to
    // whichever form survives.
    let ph = scenario_phonology();
    let cascade = Cascade {
        rules: vec![SoundRule {
            kind: RuleKind::FinalLoss,
            param: 0,
        }],
    };
    let (root_protos, affix_proto) = build_scenario();
    let root = &root_protos["consonant-final-00"];

    let cell = realize_paradigm_cell(root, &affix_proto, ClassPosition::Suffix, &cascade, &ph);
    assert_eq!(&cell.cascade_native.proto, root_proto_plus_affix(root, &affix_proto).as_slice());
    assert_eq!(&cell.regular_root.proto, root);
    assert_eq!(&cell.regular_affix.proto, &affix_proto);
}

fn root_proto_plus_affix(root: &[Segment], affix: &[Segment]) -> Vec<Segment> {
    let mut joined = root.to_vec();
    joined.extend_from_slice(affix);
    joined
}
```

- [ ] **Step 2: Run the tests and verify they pass**

`realize_paradigm_cell` and `level_paradigm` already exist and are `pub`
from Tasks 3-4, so this step is a compile-and-run of a new integration
file, not a red/green cycle on a missing function — there is no expected
failure here.

Run: `cargo test -p hornvale-language --test paradigm_properties -- --nocapture`
Confirm the file compiles and every one of the 5 tests listed above
(`purity_same_inputs_same_output`,
`non_degeneracy_some_diverge_some_dont`,
`leveling_suppresses_a_strict_subset`,
`the_frequency_prediction_shorter_roots_survive`,
`every_candidate_form_carries_a_derivation`) reports PASS.

Expected: PASS (5 tests).

- [ ] **Step 3: Run the whole crate's test suite once more**

Run: `cargo test -p hornvale-language 2>&1 | tail -30`
Expected: every test PASSES — the paradigm module's 8 unit tests (Tasks
1-4) plus this task's 5 integration tests, plus every pre-existing test in
the crate unchanged.

- [ ] **Step 4: `cargo fmt` and the type-audit check**

Run: `cargo fmt -p hornvale-language`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`
Expected: no output (clean) — every new `pub`-boundary bare primitive this
plan introduced already carries a `bare-ok(identifier-text)` tag (Tasks
1-4's doc comments).

If the type-audit check reports an untagged primitive, add the matching
`type-audit: bare-ok(identifier-text)` line (matching this plan's existing
tag style) to that item's doc comment, then re-run the check.

- [ ] **Step 5: Regenerate the type-audit report**

Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`

- [ ] **Step 6: Commit**

```bash
git add domains/language/tests/paradigm_properties.rs docs/audits/type-audit-report.md
git commit -m "test(language): the four standing laws, measured end-to-end — the-residue T5"
```
