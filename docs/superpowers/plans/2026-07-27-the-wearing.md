# The Wearing Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Place names get shorter, mean something about where they are, and wear down the way real toponyms do.

**Architecture:** Eleven tasks in five stages. **Byte-identity dies at Task 1** and never returns — this campaign is a declared total regeneration under a root-epoch bump, which is exactly what buys it the freedom to re-found the cohort baseline (ledger #9). Because everything reseeds anyway, the stages can be ordered by *dependency* rather than by blast radius: the form space first (S1), then the vocabulary that fills it (S2), then the morphology that consumes it (S3), then the phonotactics underneath it all (S4), then a pure view (S5).

**Tech Stack:** Rust 2024, `domains/language`, `domains/terrain`, `windows/worldgen`, `windows/lab`, `windows/almanac`. No new dependencies.

**Spec:** `docs/superpowers/specs/2026-07-27-the-wearing-design.md`
**Ledger:** `.superpowers/sdd/decision-ledger.md` (9 entries, 9 ideonomy passes, 1 overturn, 1 owner carve-out)
**Followups:** `.superpowers/sdd/followups.md` (F1–F4)

## Groundings that corrected the spec

Run against the real tree before writing this plan. Each changed something the spec asserted or left open.

1. **`CascadeRegime` already exists and is already parameterized.**
   `draw_cascade_with_regime(seed, species, regime)` with
   `CascadeRegime::SETTLED = { min: 2, max: 4 }` (`etymology.rs:158-218`). The
   spec said S3 "runs the existing cascade"; it is cheaper than that — the
   regime seam is already cut, so S3's wear cascade is **a new constant, not a
   new function**.
2. **`PROTO_ROOT_SYLLABLE_RANGE = (1, 2)`** (`etymology.rs:221`). Proto roots are
   *already* short. This confirms §2.2's diagnosis from a second direction: the
   length is not in the roots, it is in compounding plus the drawn stem. No task
   should touch this constant.
3. **The `sea` exposure block is the copyable template for S2.**
   `windows/worldgen/src/lib.rs:3672-3684` classifies `sea` as `KnowsOf` when
   `within_hops(geo, cell, 2, |c| terrain.is_ocean(c))`. Every new
   terrain-derived exposure in Task 4 follows this shape exactly.
4. **"The pack refers, the owner registers."** `packs.rs`'s module doc and
   `register_concepts` (`packs.rs:439`) skip any concept already owned by
   another domain — astronomy owns `sun`/`moon`/`star`/`night`, terrain owns
   `stone`/`mountain`/`sea`. **The spec did not state this.** It means `hill`,
   `river`, `ford`, `coast`, `valley`, `island`, `marsh`, `spring` are
   terrain-shaped and must be registered **in `domains/terrain`**, then merely
   *listed* in the language pack. Only the genuinely linguistic modifiers
   (`high`, `low`, `great`, `little`, `new`, `old`, `under`, `over`,
   `north`, `south`) belong to `domains/language`. Task 3 is split along that
   line.
5. **`Phonology.nuclei: usize` is a struct field**, read at `naming.rs:375`
   (`(0..self.ph.nuclei)`). S4 changes the field's *type*, so **every**
   construction site changes with it — including test fixtures such as
   `phonology.rs:786`'s `nuclei: 1`. Task 8 must sweep them, not just the draw.
6. **`repair_phonotactics` runs AFTER compounding, BEFORE morphology**, and
   `naming.rs:281-283` calls that "the permanent order." S3's wear must be
   placed in that order explicitly rather than left to the implementer:
   **compound → wear → repair → morphology.** Wear before repair, because wear
   may produce a form the synchronic templates reject and repair is what adapts
   it; and repair is the identity for attested native material (The Speakable),
   so a worn native compound survives repair unchanged.

## Test-fixture correction (applies to every task's test snippet)

**The test snippets below were written against a misremembered API and three
of them do not compile as printed.** Task 2's implementer found this; the
correction applies to Tasks 6 and 8 as well. Use these forms:

- `Seed` is a **tuple struct**: write `Seed(42)`, not `Seed::new(42)`.
- `draw_phonology` takes **three** arguments — `(&Seed, &str, &Envelope)`.

Follow the codebase's own helper (`domains/language/tests/speakable_properties.rs:38`)
rather than hand-rolling one:

```rust
fn permissive_proto() -> Phonology {
    draw_phonology(
        &Seed(37),
        "proto",
        &Envelope {
            labiality: 1.0,
            vowel_space: 1.0,
            voicing: 1.0,
            sibilance: 1.0,
            voice_loudness: 1.0,
            tonality: 0.0,
            exotic: ExoticSeg::None,
        },
    )
}
```

Where a later task's snippet says `draw_phonology(&seed, "goblin")`, build the
phonology this way instead and say so in your report. **Where a snippet needs a
`Vec<Segment>` value, build it from the drawn phonology's own inventory — never
hand-construct `Segment` variants**, which is the mistake that makes a test
pass against a phonology that could not have produced the value.

## Global Constraints

- **Branch `the-wearing`, worktree `~/.config/superpowers/worktrees/hornvale/the-wearing`.** Off `origin/main` at `3a7092c3`.
- **No new dependencies** — `serde`/`serde_json` only, workspace-wide (enforced by `cli/tests/architecture.rs`).
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only (enforced by `clippy.toml`). No wall-clock time.
- **Layering is constitutional** — `kernel/` → `domains/*` → `windows/*` → `cli/`. A domain crate depends on `hornvale-kernel` and **nothing else**. `domains/language` never imports `domains/terrain`; the composition root (`windows/worldgen`) is where they meet.
- **Every crate sets `#![warn(missing_docs)]`.** This codebase's comments explain *why*, at length, and name the campaign. Terse comments beside paragraph-length neighbours are a defect.
- **Quantize at emit only.** Never in the compute path.
- **`cargo fmt` is the final step before every commit.** The commit gate is `make gate`. Iterate cost-ordered: fmt + clippy, then `cargo test -p <crate>`, and `--workspace` only at the gate.
- **Byte-identity is NOT a success criterion for this campaign.** It is deliberately destroyed at Task 1. Do not write a byte-identity test and do not treat golden drift as a failure — regeneration is the point. Determinism (*same seed twice → identical*) remains absolute.

## File Structure

```
domains/language/src/accession.rs    T1  re-found cohort 0; re-pin the frozen-roster test
domains/language/src/etymology.rs    T1  ROOT_EPOCH v3 -> v4
                                     T2  epoch-gated reserved subspace in draw_candidate
                                     T6  CascadeRegime::WEAR
domains/language/src/packs.rs        T3  list new concepts; register the language-owned ones
domains/terrain/src/facts.rs         T3  register the terrain-owned toponymic concepts
domains/language/src/naming.rs       T6  wear in the compound pipeline
                                     T7  NameShape and the per-culture draw
                                     T9  position-conditioned reduction
domains/language/src/phonology.rs    T8  nuclei: usize -> Vec<Vec<()>> nucleus templates
windows/worldgen/src/lib.rs          T4  exposure rules for the new concepts
                                     T5  SiteConcepts -> toponymic feature vector
                                     T7  per-culture shape params
windows/almanac/src/lib.rs           T10 render-time qualification
windows/lab/src/metrics.rs           T11 syllable-count + transparency metrics
windows/lab/tests/calibration.rs     T11 re-pin every naming row
```

---

## Stage S1 — the form space (T1–T2)

Byte-identity dies at Task 1. Everything after it is measured against the new baseline, never against `main`.

### Task 1: Re-found the cohort baseline under a root-epoch bump

**Files:**
- Modify: `domains/language/src/accession.rs:34` (`EPOCH_COHORTS`), `:167` (the frozen-roster test)
- Modify: `domains/language/src/etymology.rs:256` (`ROOT_EPOCH`)

**Interfaces:**
- Consumes: nothing.
- Produces: `EPOCH_COHORTS[0]` containing all 91 currently-registered concepts (76 + 15); `ROOT_EPOCH == "v4"`. Task 3 appends the campaign's 19 toponymic concepts to this same cohort 0, taking it to 110.

**Why this is legal** (ledger #9, owner call): The Accession's "never edit a
cohort" rule exists to prevent churn *between* epoch bumps. This campaign is a
declared deliberate regeneration, which the determinism contract explicitly
sanctions via an epoch suffix. A bump is exactly when a baseline is legitimately
re-founded. Do **not** weaken or delete the rule — it still governs every future
campaign that is not bumping the epoch.

- [ ] **Step 1: Merge cohort 1 into cohort 0**

In `EPOCH_COHORTS`, move every string from cohort 1 into cohort 0, keeping cohort 0 sorted, and delete the now-empty cohort 1. The table becomes a single cohort of 91.

Verify the count before editing:

```bash
cd ~/.config/superpowers/worktrees/hornvale/the-wearing
python3 -c "
import re
s=open('domains/language/src/accession.rs').read()
body=s[s.index('EPOCH_COHORTS'):s.index('pub fn concept_epoch')]
for i,c in enumerate(re.findall(r'&\[(.*?)\]',body,re.S)[1:]):
    print(f'cohort {i}:', len(re.findall(r'\"',c))//2)
"
```

Expected: `cohort 0: 76` / `cohort 1: 15`. After the edit, rerun: `cohort 0: 91`.

- [ ] **Step 2: Update the module doc and the test**

`accession.rs`'s module doc says "Add a new `&[...]` to the end of `EPOCH_COHORTS`. **Never edit an existing cohort**". Keep that rule and add the exception beside it — the rule is interval-scoped, and saying so is the whole justification:

```rust
//! Add a new `&[...]` to the end of [`EPOCH_COHORTS`]. **Never edit an
//! existing cohort**: a concept that changed epoch would re-sort, which is
//! exactly the churn this module exists to prevent. Retired concepts stay
//! listed — their slot is spent either way.
//!
//! **The one exception, and its boundary.** That rule is scoped to the
//! interval *between* epoch bumps. A campaign that bumps `ROOT_EPOCH` is a
//! declared total regeneration — every root reseeds regardless, so there is no
//! churn left to prevent — and it may therefore re-found cohort 0 as the
//! roster at that bump. The Wearing (2026-07-27) did exactly this, merging the
//! 15-concept Actants cohort back into a 91-concept baseline before adding its
//! own ~20. Outside a bump the rule is absolute; do not read this exception as
//! permission to edit a cohort in an ordinary campaign.
```

Then re-pin the test, with the justification in the assertion message:

```rust
    /// Cohort 0 is frozen between epoch bumps: it is the roster whose
    /// assignments every later cohort is defined not to disturb. It is
    /// re-founded only by a campaign that bumps `ROOT_EPOCH`, when every root
    /// reseeds anyway and there is no churn left to prevent (The Wearing,
    /// 2026-07-27: 76 + the 15-concept Actants cohort = 91, before that
    /// campaign's own additions land in Task 3).
    #[test]
    fn cohort_zero_stays_the_frozen_landing_roster() {
        assert_eq!(
            EPOCH_COHORTS[0].len(),
            91,
            "cohort 0 is the 91-concept roster re-founded at The Wearing's \
             v4 root-epoch bump; growing it OUTSIDE such a bump would re-sort \
             concepts that already have assignments — append a NEW cohort instead"
        );
    }
```

`later_cohorts_are_non_empty` iterates `.skip(1)` over an now-single-element table and vacuously passes. Leave it: it is correct and will matter again at the next ordinary campaign.

- [ ] **Step 3: Bump the root epoch**

`domains/language/src/etymology.rs:256`:

```rust
const ROOT_EPOCH: &str = "v4";
```

Extend the doc comment above it with a `v4` sentence in the existing style, naming this campaign and the re-founding.

- [ ] **Step 4: Run the language + accession tests**

```bash
cargo test -p hornvale-language 2>&1 | tail -30
cargo test -p hornvale --test accession 2>&1 | tail -20
```

Expected: `hornvale-language` green. `accession` green — both parity tests (`every_registered_concept_has_an_accession_epoch`, `every_accessioned_concept_is_actually_registered`) still hold, because merging cohorts changes no concept's membership in the *union*.

**Expect unrelated failures elsewhere and do not chase them here.** Every fixture-bearing test that pins a generated name is now stale by design. Record which ones fail; they are re-pinned in Task 11.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add domains/language/src/accession.rs domains/language/src/etymology.rs
git commit -m "feat(language): re-found cohort 0 under a v4 root epoch

The Wearing bumps ROOT_EPOCH v3 -> v4, a declared total regeneration. That
is what makes re-founding cohort 0 legal: The Accession's never-edit-a-cohort
rule prevents churn BETWEEN bumps, and at a bump every root reseeds anyway.

Cohort 1 (The Actants, 15) merges back into cohort 0 (76) for a 91-concept
baseline, so this campaign's ~20 toponymic concepts sort by core_rank on merit
rather than being permanently marked as loanwords (ledger #9, owner call).

Byte-identity dies here, deliberately, and does not return."
```

### Task 2: The reserved codomain subspace (LANG-55)

**Files:**
- Modify: `domains/language/src/etymology.rs:409-436` (`draw_candidate`)
- Test: `domains/language/tests/` — new `accession_properties.rs`

**Interfaces:**
- Consumes: `ROOT_EPOCH == "v4"` (Task 1); `crate::accession::concept_epoch(concept) -> u32`.
- Produces: `draw_candidate(seed, family, concept, ph, probe, epoch)` — **note the new `epoch` parameter**. `assign_proto_roots_with_epoch` already computes `epoch_of(concept)` for its sort and must now also thread it here.

**The carve.** Epoch-0 roots draw as they do today. Roots of epoch ≥ 1 draw from a
**disjoint region of the same-length form space**, so a later concept cannot
collide with an earlier one by construction, and does so **without getting
longer** — the length axis is already spoken for by `PROBE_BUDGET` and is the
one axis this campaign must not spend.

Carve on the **final coda**: epoch-0 roots may end open or closed as the
phonology allows; epoch-≥1 roots must end **closed** (a non-empty final coda).
This is same-length, audible, and is real neologism phonology. Where the
phonology admits no closed coda at all, the carve degrades to the identity and
the concept draws from the shared space — correctness before marking.

- [ ] **Step 1: Write the failing test**

Create `domains/language/tests/accession_properties.rs`:

```rust
//! The Wearing (LANG-55): later-epoch proto-roots are drawn from a reserved
//! region of the SAME-LENGTH form space, so a new concept cannot collide with
//! an established one — additivity by construction of the codomain rather than
//! by the assignment ORDER (which is what The Accession bought, at Zipf's
//! expense).
use hornvale_kernel::Seed;
use hornvale_language::{assign_proto_roots_with_epoch_for_test, draw_phonology};

/// A later-epoch concept never receives a root already assigned to an
/// epoch-0 concept, and never receives a LONGER one merely for being later.
#[test]
fn later_epoch_roots_are_disjoint_but_not_longer() {
    let seed = Seed::new(42);
    let ph = draw_phonology(&seed, "goblin");
    let concepts = ["water", "stone", "fire", "hill", "river", "ford"];
    let epoch_of = |c: &str| u32::from(matches!(c, "hill" | "river" | "ford"));

    let assigned = assign_proto_roots_with_epoch_for_test(
        &seed, "goblinoid", &ph, &concepts, &[], epoch_of,
    );

    let old: Vec<_> = ["water", "stone", "fire"]
        .iter()
        .map(|c| assigned[*c].clone())
        .collect();
    for late in ["hill", "river", "ford"] {
        let form = &assigned[late];
        assert!(
            !old.contains(form),
            "{late} collided with an epoch-0 root by construction"
        );
        let longest_old = old.iter().map(Vec::len).max().expect("non-empty");
        assert!(
            form.len() <= longest_old + 1,
            "{late} is {} segments against an epoch-0 max of {longest_old} — \
             the carve spent the LENGTH axis, which is the one axis The \
             Wearing must not spend",
            form.len()
        );
    }
}
```

`assign_proto_roots_with_epoch` is `pub(crate)`. Add a `#[doc(hidden)]` public wrapper in `lib.rs` named `assign_proto_roots_with_epoch_for_test` rather than widening the real function's visibility.

- [ ] **Step 2: Run it and watch it fail**

```bash
cargo test -p hornvale-language --test accession_properties 2>&1 | tail -20
```

Expected: FAIL to compile — `assign_proto_roots_with_epoch_for_test` not found.

- [ ] **Step 3: Thread the epoch and carve the subspace**

In `draw_candidate`, add the parameter and constrain the final coda:

```rust
fn draw_candidate(
    seed: &Seed,
    family: &str,
    concept: &str,
    ph: &Phonology,
    probe: u32,
    epoch: u32,
) -> Vec<Segment> {
    let tier = probe / PROBE_BUDGET;
    let min = PROTO_ROOT_SYLLABLE_RANGE.0 + tier;
    let max = PROTO_ROOT_SYLLABLE_RANGE.1 + tier;
    let base = seed
        .derive(streams::ROOT)
        .derive(StreamLabel::dynamic(family))
        .derive(streams::LEXICON)
        .derive(streams::PROTO_ROOT)
        .derive(StreamLabel::dynamic(ROOT_EPOCH))
        .derive(StreamLabel::dynamic(concept));
    let mut stream = if probe == 0 {
        base.stream()
    } else {
        base.derive(streams::PROBE)
            .derive(StreamLabel::dynamic(&probe.to_string()))
            .stream()
    };
    let namer = Namer::new(seed, family, ph);
    // LANG-55, The Wearing: a later-epoch coinage draws from a reserved
    // region of the SAME-LENGTH form space — it must end on a closed
    // syllable, which epoch-0 roots are free to avoid. Additivity then holds
    // by construction of the codomain, not by the assignment order, so a core
    // (Swadesh) concept registered in a later campaign keeps its short form
    // instead of forfeiting it to arrival order (the cost The Accession
    // knowingly took, §3.3). The `weighty` flag is exactly this constraint
    // and already exists for deity stems.
    //
    // Degradation is deliberate: `draw_syllables(.., weighty = true)` falls
    // back to the open templates when the phonology admits no closed coda at
    // all, so the carve becomes the identity rather than failing. A language
    // that cannot mark its neologisms simply does not mark them.
    let weighty = epoch > 0;
    let syllables = namer.draw_syllables(&mut stream, min, max, weighty);
    crate::naming::segments_of(&syllables)
}
```

Update the two call sites in `assign_proto_roots_with_epoch` to pass `epoch_of(concept)`.

> **Note for the implementer:** `weighty` biases *every* syllable's coda, not
> only the final one (`choose_coda_template`, `naming.rs:394`). That is a
> stronger carve than the doc above describes, and it is acceptable — the
> disjointness argument only needs *some* reserved region. If the resulting
> forms read as too heavy in Task 11's inspection, narrow it to the final
> syllable then, with a measurement, not now on speculation.

- [ ] **Step 4: Run the test**

```bash
cargo test -p hornvale-language --test accession_properties 2>&1 | tail -20
```

Expected: PASS.

- [ ] **Step 5: Run the whole language crate**

```bash
cargo test -p hornvale-language 2>&1 | tail -30
```

Expected: green, except fixture-pinned name assertions. Note them for Task 11.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add domains/language/
git commit -m "feat(language): reserve a codomain subspace for later-epoch roots (LANG-55)

A later-epoch concept draws only closed-final forms, a region epoch-0 roots
are free to avoid. Additivity then holds by construction of the FORM SPACE
rather than of the assignment order — so a future Swadesh word keeps its
short form instead of forfeiting it to arrival order, which is the cost The
Accession took knowingly (its spec 3.3) and this dissolves.

Same-length by design: the length axis is already spent by PROBE_BUDGET and
is the one axis The Wearing must not spend."
```

---

## Stage S2 — descriptor breadth (T3–T5)

### Task 3: Register the toponymic concepts

**Files:**
- Modify: `domains/terrain/src/lib.rs:123-141` — register the terrain-owned concepts (**not** `facts.rs`, which holds predicates; the concept loop is the `for (name, kind, doc) in [...]` block that registers `stone`/`mountain`/`sea` as full `Manifest`s)
- Modify: `domains/language/src/packs.rs:49` (`universal_stratum`) and `:439` (`register_concepts`)
- Modify: `domains/language/src/accession.rs:34` — add all new ids to cohort 0

**Interfaces:**
- Consumes: cohort 0 at 91 (Task 1).
- Produces: cohort 0 at **110** (91 + 19); `packs::universal_stratum()` listing the 19 new ids; `packs::is_core_concept(c) == true` for all of them.

**The ownership split** (grounding 4 — "the pack refers, the owner registers"):

| owner | concepts |
|---|---|
| `domains/terrain` | `hill`, `river`, `lake`, `valley`, `coast`, `island`, `ford`, `marsh`, `spring` |
| `domains/language` | `high`, `low`, `great`, `little`, `new`, `old`, `under`, `over`, `north`, `south` |

Nineteen. Add `bend` to terrain for twenty if a twentieth is wanted; do not pad the list to hit a round number.

- [ ] **Step 1: Write the failing test**

Add to `cli/tests/accession.rs`:

```rust
/// The Wearing: every toponymic concept the campaign added is registered,
/// accessioned, and core — a periphery concept would sort after core inside
/// its cohort and take a longer form, which for `hill` and `river` is exactly
/// backwards (these are the highest-frequency morphemes in the name corpus).
#[test]
fn the_toponymic_concepts_are_registered_and_core() {
    const TOPONYMIC: &[&str] = &[
        "hill", "river", "lake", "valley", "coast", "island", "ford", "marsh",
        "spring", "high", "low", "great", "little", "new", "old", "under",
        "over", "north", "south",
    ];
    let registered = registered();
    let accessioned = accessioned();
    for concept in TOPONYMIC {
        assert!(registered.contains(*concept), "{concept} is not registered");
        assert!(accessioned.contains(*concept), "{concept} has no accession epoch");
        assert_eq!(
            hornvale_language::packs::is_core_concept(concept),
            true,
            "{concept} is periphery; it must be core to win a short form"
        );
    }
}
```

- [ ] **Step 2: Run it and watch it fail**

```bash
cargo test -p hornvale --test accession 2>&1 | tail -20
```

Expected: FAIL — `hill is not registered`.

- [ ] **Step 3: Register the terrain-owned concepts**

In `domains/terrain/src/lib.rs`, extend the existing `for (name, kind, doc) in [...]` loop at `:123-141`. Each entry is a `(name, ConceptKind, doc)` triple and the loop wraps it in a full `Manifest` — so a new concept is **one line**, not a new `Manifest` literal:

```rust
    for (name, kind, doc) in [
        ("stone", ConceptKind::Substance, "rock"),
        ("mountain", ConceptKind::Terrain, "high ground"),
        ("sea", ConceptKind::Terrain, "a body of salt water"),
        ("hill", ConceptKind::Terrain, "ground that rises above what surrounds it"),
        ("river", ConceptKind::Terrain, "fresh water running across land"),
        ("lake", ConceptKind::Terrain, "still fresh water held in a hollow"),
        ("valley", ConceptKind::Terrain, "low ground between heights"),
        ("coast", ConceptKind::Terrain, "where the land meets the sea"),
        ("island", ConceptKind::Terrain, "land the water surrounds"),
        ("ford", ConceptKind::Terrain, "where a river runs shallow enough to cross"),
        ("marsh", ConceptKind::Terrain, "soft wet ground"),
        ("spring", ConceptKind::Terrain, "where water rises from the ground"),
    ] {
```

The loop already sets `lexeme: Correspondent::Present(Lexicalization::Expected)` — which is what makes these concepts lexicalizable — and marks percept/cognition absent. Do not change those; the new entries inherit them, which is correct.

Note `domains/terrain/src/lib.rs:255` iterates `["mountain", "sea"]` in a test; check whether it needs extending.

- [ ] **Step 4: Add all nineteen to the language pack and to cohort 0**

In `packs.rs`, add a `PackEntry` per concept to `universal_stratum()` with `ladder_rank: 0` (unranked — always in the lexicon once exposure permits). `register_concepts` skips the terrain-owned ones automatically; that is the mechanism, not an oversight.

In `accession.rs`, add all nineteen ids to cohort 0 in sorted order and re-pin the test from 91 to 110.

- [ ] **Step 5: Run the tests**

```bash
cargo test -p hornvale --test accession 2>&1 | tail -20
cargo test -p hornvale-terrain 2>&1 | tail -10
cargo test -p hornvale-language 2>&1 | tail -20
```

Expected: `accession` green including the new test.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add domains/terrain/ domains/language/ cli/tests/accession.rs
git commit -m "feat: register nineteen toponymic concepts (LANG-9)

hill/river/lake/valley/coast/island/ford/marsh/spring to terrain, which owns
them; high/low/great/little/new/old/under/over/north/south to language. The
pack refers, the owner registers.

All land in the re-founded cohort 0 as CORE, so they sort by core_rank and
win short forms — 'river' is among the highest-frequency morphemes in the
whole name corpus and must not be long."
```

### Task 4: Exposure rules for the new concepts

**Files:**
- Modify: `windows/worldgen/src/lib.rs:3576-3690` (`exposure_of_in`)

**Interfaces:**
- Consumes: the concepts of Task 3.
- Produces: `exposure_of_in` classifying each new concept per culture. No signature change.

Follow the `sea` block (`:3672`) exactly. A people is `Steeped` in what it lives on and `KnowsOf` what it lives near.

- [ ] **Step 1: Write the failing test**

Add to `windows/worldgen/tests/` (match the existing naming there):

```rust
/// The Wearing: a people settled on a river holds the word for it, and a
/// people that has never seen one carries a Gap with a reason. The gate is
/// the same one `sea` already uses — real proximity, not roster membership.
#[test]
fn river_exposure_tracks_real_proximity() {
    let world = build(42).expect("seed 42 builds");
    for species in placed_species(&world) {
        let lex = hornvale_worldgen::lexicon_of(&world, &species).expect("lexicon");
        match lex.entry("river") {
            Some(LexEntry::Root { .. }) | Some(LexEntry::Compound { .. }) => {}
            Some(LexEntry::Gap { reason, .. }) => {
                assert!(!format!("{reason}").is_empty(), "{species}: empty gap reason");
            }
            None => panic!("{species}: 'river' is registered but absent from the lexicon"),
        }
    }
}
```

- [ ] **Step 2: Run it and watch it fail**

```bash
cargo test -p hornvale-worldgen river_exposure 2>&1 | tail -20
```

Expected: FAIL — `'river' is registered but absent from the lexicon` (the closing `Unknown` loop should actually catch it; if the test passes immediately, the assertion is too weak — strengthen it to require at least one species to hold a real word before proceeding).

- [ ] **Step 3: Add the exposure rules**

After the `sea` block, in the same style and with comments of the same density:

```rust
    // Steeped: the terrain a people actually lives on. `drainage_at` is the
    // river signal The Confluence's siting already uses, so a people whose
    // settlements condensed onto the river network (a measured 0.72 near-river
    // fraction at seed 42) reliably holds the word — the exposure is derived
    // from the same field that put them there, not asserted alongside it.
    for &cell in settled {
        if terrain.water_kind_at(cell) == WaterKind::Fresh {
            classes.insert("river".to_string(), ExposureClass::Steeped);
        }
        if terrain.cave_at(cell).is_some() {
            classes.insert("cave".to_string(), ExposureClass::Steeped);
        }
    }

    // KnowsOf: the coast, on the same two-hop gate `sea` uses. A people can
    // know the shore without living on it; `coast` is the shore as a PLACE,
    // where `sea` is the water — two concepts, deliberately, because toponymy
    // wants the former ("Seaside") and cosmology wants the latter.
    if world.registry.concept("coast").is_some() {
        let near_coast = settled
            .iter()
            .any(|&cell| within_hops(geo, cell, 2, |c| terrain.is_ocean(c)));
        if near_coast {
            classes
                .entry("coast".to_string())
                .or_insert(ExposureClass::KnowsOf);
        }
    }
```

Add the remaining concepts on the same pattern: `hill`/`valley` from `elevation_at` against the local neighbourhood, `island` from a landmass-size test, `marsh` from `hydro_at`, `spring`/`ford` from `drainage_at` with `is_endorheic`. **The relative and evaluative modifiers (`high`, `low`, `great`, `little`, `new`, `old`, `under`, `over`, `north`, `south`) need no terrain gate — they are `Steeped` unconditionally, like the rest of the universal stratum**, because every people that speaks has them.

- [ ] **Step 4: Run the test**

```bash
cargo test -p hornvale-worldgen river_exposure 2>&1 | tail -20
cargo test -p hornvale-worldgen 2>&1 | tail -30
```

Expected: the new test passes; note any fixture drift for Task 11.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add windows/worldgen/
git commit -m "feat(worldgen): exposure rules for the toponymic concepts

A people is Steeped in what it lives on and KnowsOf what it lives near,
on the same gates 'sea' already uses. Falls out culturally differentiated
for free: a landlocked people gets a Gap for 'coast' and a river people
gets 'river', with no per-culture authoring anywhere."
```

### Task 5: Widen `SiteConcepts` to a toponymic feature vector

**Files:**
- Modify: `windows/worldgen/src/lib.rs:4659` (settlement site composition)
- Modify: `windows/lab/src/metrics.rs` — `name-gloss-true`'s doc string (followup F2)

**Interfaces:**
- Consumes: Tasks 3–4.
- Produces: `settlement_site_concepts(cell, terrain, climate, presiding) -> Vec<&'static str>` — a new free function in `windows/worldgen`, mirroring the existing `deity_site_concepts` (`:3977`), which must be `pub` for the lab's re-derivation to reach it.

**The ordering is a contract.** `glossed_name` draws 1–2 concepts from the
vector by index; changing the vector's ORDER changes which concepts a
settlement's name picks. Emit in a fixed, documented order — most specific
first — and never sort it by a value that can vary.

- [ ] **Step 1: Write the failing test**

```rust
/// The Wearing: a settlement's site offers more than its biome and its sky.
/// The concrete claim: at seed 42, at least one settlement's vector contains
/// a terrain-derived concept, and every vector is a subset of the registry.
#[test]
fn the_site_vector_is_wider_than_biome_and_sky() {
    let world = build(42).expect("seed 42 builds");
    let vectors = settlement_site_vectors(&world);
    assert!(!vectors.is_empty(), "seed 42 places no settlements");
    assert!(
        vectors.iter().any(|v| v.len() > 2),
        "every site vector is still <= 2 concepts wide — the widening did not land"
    );
    for v in &vectors {
        for concept in v {
            assert!(
                world.registry.concept(concept).is_some(),
                "site vector names an unregistered concept: {concept}"
            );
        }
    }
}
```

- [ ] **Step 2: Run it and watch it fail**

```bash
cargo test -p hornvale-worldgen site_vector 2>&1 | tail -20
```

Expected: FAIL — `every site vector is still <= 2 concepts wide`.

- [ ] **Step 3: Extract and widen**

Replace the two-line composition at `:4659` with a call to a new `pub fn settlement_site_concepts`, ordered most-specific-first:

```rust
/// The concepts a settlement's own site offers its namer, most specific
/// first — the order is a CONTRACT, because `glossed_name` picks 1-2 by
/// index and a reordering silently renames every settlement in every world.
///
/// Wider than the biome-plus-sky pair The Words shipped (LANG-9: "the naming
/// engine already consumes whatever site facts the composition root offers").
/// Every entry is read off a fact that already existed; nothing here is drawn.
/// type-audit: bare-ok(identifier-text)
pub fn settlement_site_concepts(
    cell: CellId,
    terrain: &dyn Terrain,
    climate: &dyn Climate,
    presiding: Option<&'static str>,
) -> Vec<&'static str> {
    let mut concepts: Vec<&'static str> = Vec::with_capacity(4);
    // Hydrography first: it is what real toponymy reaches for first, and it
    // is the most discriminating fact a cell carries.
    if terrain.water_kind_at(cell) == WaterKind::Fresh {
        concepts.push("river");
    }
    if terrain.cave_at(cell).is_some() {
        concepts.push("cave");
    }
    // ... elevation -> hill/valley, coastal adjacency -> coast, and so on.
    concepts.push(climate.biome_at(cell).concept_name());
    concepts.extend(presiding);
    concepts
}
```

Keep the biome and the presiding phenomenon at the END — they are the least
discriminating, and `glossed_name` should reach them only when nothing sharper
is available.

- [ ] **Step 4: Run the tests, then look at real output**

```bash
cargo test -p hornvale-worldgen 2>&1 | tail -30
cargo run -p hornvale -- new --seed 42 --out /tmp/hv-wearing.json
cargo run -p hornvale -- almanac --world /tmp/hv-wearing.json | head -40
```

Expected: names now gloss to things like `river-hill` rather than `taiga-moon`. **Read the actual output** — this is the first task whose result is judged by eye as well as by assertion.

- [ ] **Step 5: Fix F2 while you are here**

`name-gloss-true`'s doc string in `windows/lab/src/metrics.rs` hardcodes "biome + presiding phenomenon". Update it to describe the wider vector. The assertion itself needs no change — it re-derives whatever the composition root offers.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add windows/worldgen/ windows/lab/
git commit -m "feat(worldgen): a settlement's site offers a toponymic feature vector

Was biome + presiding sky: ~12 biomes against a handful of phenomena, which
is why '-noaboo' appeared verbatim in six committed names. Now reads
hydrography, caves, elevation and coastal adjacency off facts that already
existed on TerrainProvider.

Order is a contract - glossed_name picks 1-2 by index. Most specific first,
biome and sky last."
```

---

## Stage S3 — wear and shape (T6–T7)

### Task 6: Toponymic wear

**Files:**
- Modify: `domains/language/src/etymology.rs:165-175` — add `CascadeRegime::WEAR`
- Modify: `domains/language/src/naming.rs:281-296` — wear in the compound pipeline

**Interfaces:**
- Consumes: `draw_cascade_with_regime` (exists — grounding 1); `evolve(proto, cascade, ph) -> Derivation` (`etymology.rs:675`).
- Produces: `Namer::wear(segments, frequency) -> Vec<Segment>`.

**The order is fixed** (grounding 6): **compound → wear → repair → morphology.**

**Wear is keyed to corpus frequency, not to the syntactic slot** (ledger #3) —
the generic wears most because it recurs most, which derives the
generic/specific asymmetry instead of authoring it, and correctly wears a
*specific* that happens to be ubiquitous.

- [ ] **Step 1: Write the failing test**

```rust
/// The Wearing (LANG-11, opacification): a morpheme that recurs across many
/// of a culture's names wears down; a rare one survives whole. This is
/// Zipf's law of abbreviation and it is the mechanism behind OE ham -> -ham.
#[test]
fn frequent_morphemes_wear_and_rare_ones_do_not() {
    let seed = Seed::new(42);
    let ph = draw_phonology(&seed, "goblin");
    let namer = Namer::new(&seed, "goblin", &ph);
    let stem: Vec<Segment> = /* a fixed 3-syllable form from the inventory */;

    let worn = namer.wear(&stem, 0.95);
    let whole = namer.wear(&stem, 0.02);

    assert!(
        worn.len() < stem.len(),
        "a morpheme in 95% of this culture's names did not wear at all"
    );
    assert_eq!(
        whole, stem,
        "a morpheme in 2% of names wore down; rare forms must survive whole"
    );
}
```

- [ ] **Step 2: Run it and watch it fail**

```bash
cargo test -p hornvale-language frequent_morphemes_wear 2>&1 | tail -20
```

Expected: FAIL to compile — no method `wear`.

- [ ] **Step 3: Add the wear regime and the method**

```rust
impl CascadeRegime {
    /// The wear regime: the short cascade a high-frequency toponymic morpheme
    /// is run through at name-formation time (The Wearing, LANG-11's
    /// opacification phase). One to two rules, against SETTLED's two to four —
    /// wear is a grinding-down, not a millennium of divergence.
    pub const WEAR: CascadeRegime = CascadeRegime { min: 1, max: 2 };
}
```

In `naming.rs`, between compounding and repair:

```rust
    /// Wear `segments` down in proportion to `frequency` — the share of this
    /// culture's names the morpheme appears in.
    ///
    /// This is the mechanism behind `-ham`, `-ton` and `-by`: the generic is
    /// the highest-frequency morpheme in the whole name corpus, so Zipf's law
    /// of abbreviation grinds it hardest. Keying on frequency rather than on
    /// the compound's HEAD slot is deliberate (ledger #3): it derives the
    /// generic/specific asymmetry rather than authoring it, and it correctly
    /// wears a *specific* that happens to be ubiquitous in one culture.
    ///
    /// Below `WEAR_FLOOR` the form is returned untouched — a rare generic
    /// stays whole, which is `-thwaite` beside `-ham`, and is what makes the
    /// resulting transparency a DISTRIBUTION rather than a new constant.
    pub fn wear(&self, segments: &[Segment], frequency: f64) -> Vec<Segment> {
        if frequency < WEAR_FLOOR {
            return segments.to_vec();
        }
        let cascade = draw_cascade_with_regime(self.seed, &self.species, CascadeRegime::WEAR);
        evolve(segments, &cascade, self.ph).modern
    }
```

The frequency itself is computed by the composition root, which is the only
layer that can see a culture's whole settlement scatter — `naming.rs` stays a
pure function of its arguments, as its module doc requires.

- [ ] **Step 4: Run the test, then the crate**

```bash
cargo test -p hornvale-language frequent_morphemes_wear 2>&1 | tail -20
cargo test -p hornvale-language 2>&1 | tail -30
```

- [ ] **Step 5: Wire the frequency in worldgen and drop the drawn stem**

In the settlement loop, count each site concept's occurrences across the
species' own settlements, pass the share to `glossed_name`, and **remove the
2–3 syllable drawn stem** (`naming.rs:277`). That stem is the single largest
length contributor and decision 0024 is explicit that entropy is not how
collisions get fixed. Expect the collision rate to move; it is re-pinned, not
defended (spec §4).

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add domains/language/ windows/worldgen/
git commit -m "feat(language): toponymic wear, and the drawn stem retires

A morpheme wears in proportion to how many of its culture's names use it -
Zipf's law of abbreviation, the mechanism behind OE ham -> -ham. Keyed to
frequency, not to the compound's head slot, so the generic/specific asymmetry
is DERIVED and a ubiquitous specific wears too.

No new machinery: CascadeRegime was already parameterized and RULE_KINDS
already holds VowelShift, ClusterSimplify and FinalLoss - precisely the three
sound changes that perform real toponymic wear. The wear is therefore
Neogrammarian-regular by construction and prints its own derivation.

The 2-3 syllable drawn stem retires. Decision 0024: uniqueness is a
reference-time property and no future work fixes collisions by adding entropy."
```

### Task 7: Name shape as a per-culture distribution

**Files:**
- Modify: `domains/language/src/naming.rs` — `NameShape`, drawn per settlement
- Modify: `windows/worldgen/src/lib.rs` — per-culture shape weights

**Interfaces:**
- Consumes: Task 6.
- Produces: `pub enum NameShape { Simplex, SpecificGeneric, Qualified }`; `MorphOptions` gains `shape_weights: [f64; 3]`.

Drawn per settlement from a per-culture weighted distribution (ledger #6). The
idiom is `Stream::weighted_index(&[f64])` (`kernel/src/seed.rs:254`) as wrapped
by `schemas::select_schema` (`schemas.rs:408`), which sharpens by
`weight.powf(beta)` — **not** `WeightedCategorical::reduce`, which does not
exist anywhere in the tree (followup F1).

- [ ] **Step 1: Write the failing test**

```rust
/// The Wearing: a people's toponymy is recognizable as THEIRS - one shape
/// dominates - but it has a tail. Pure per-settlement variety reads as noise;
/// pure per-culture uniformity loses the variation real systems have.
#[test]
fn a_culture_has_a_dominant_shape_and_a_tail() {
    let world = build(42).expect("seed 42 builds");
    for species in placed_species(&world) {
        let shapes = settlement_shapes(&world, &species);
        if shapes.len() < 20 {
            continue; // too few to speak of a distribution
        }
        let dominant = modal_share(&shapes);
        assert!(dominant > 0.4, "{species}: no dominant shape ({dominant:.2})");
        assert!(dominant < 0.95, "{species}: shape is effectively constant ({dominant:.2})");
    }
}
```

- [ ] **Step 2–5:** Run it failing, add `NameShape` and the draw, run it passing, commit. Simplex takes the specific alone (York); `SpecificGeneric` is the compound (Oxford); `Qualified` adds a modifier and should be the rarest.

---

## Stage S4 — phonotactic texture (T8–T9)

### Task 8: `nuclei` becomes a template set

**Files:**
- Modify: `domains/language/src/phonology.rs:83` (the field), `:507` (the draw), `:786` (the fixture)
- Modify: `domains/language/src/naming.rs:374-377` (the read)

**Interfaces:**
- Produces: `Phonology.nuclei: Vec<usize>` — a set of admissible nucleus sizes, drawn like `onsets` and `codas`. **Every construction site changes** (grounding 5); sweep with `rg 'nuclei'` and fix all of them, fixtures included.

- [ ] **Step 1: Write the failing test**

```rust
/// The Wearing: no natural language puts an obligatory diphthong in EVERY
/// syllable, but `nuclei: usize` did exactly that for half of all drawn
/// phonologies - which is most of what read as obnoxious in Qvooshtvoagootao.
#[test]
fn no_language_requires_a_diphthong_in_every_syllable() {
    for seed in 0..200u64 {
        let ph = draw_phonology(&Seed::new(seed), "goblin");
        assert!(
            ph.nuclei.contains(&1),
            "seed {seed}: every syllable is obligatorily complex ({:?})",
            ph.nuclei
        );
    }
}
```

- [ ] **Steps 2–5:** Run failing; change the field to `Vec<usize>` and draw it as a template set that always admits `1`; update `draw_syllable` to pick a size per syllable; sweep every construction site; run `cargo test -p hornvale-language`; commit.

### Task 9: Position-conditioned reduction

**Files:**
- Modify: `domains/language/src/naming.rs`

Full nuclei under prominence, reduced elsewhere. **This is the rule Task 6's
wear runs** (ledger #5) — unstressed-vowel reduction *is* erosion, seen at a
different time-scale. Implement it once, here, and have `wear` call it rather
than duplicating the logic. LANG-18 records that stress is fixed on the first
vowel today, which is the conditioning environment.

- [ ] **Steps 1–5:** Test that a polysyllabic name's non-initial nuclei are no longer than its initial one; implement; verify Task 6's tests still pass against the shared rule; commit.

---

## Stage S5 — render-time qualification (T10)

### Task 10: Disambiguate co-occurring names at render time

**Files:**
- Modify: `windows/almanac/src/lib.rs`, `windows/almanac/src/connections.rs:243`

Decision 0024's deferred remedy, quoted in its own words: *"almanac and REPL
disambiguate co-occurring same-named settlements from site facts, e.g.
'Ice-Home (taiga)' / 'Ice-Home of the kobolds' … it is a view, so it touches no
save-format contract."*

**No epoch bump, no census regen, no ledger change.** Qualify only where an
actual ambiguity appears in the rendered document — the lazy qualification
natural languages use. Newcastle is "Newcastle" until a second one is in the
room.

- [ ] **Steps 1–5:** Test that a document naming two same-named settlements qualifies both and that a document naming one qualifies neither; implement; commit.

---

## Close (T11)

### Task 11: Metrics, calibration, artifacts, book

**Files:**
- Modify: `windows/lab/src/metrics.rs` — two new metrics
- Modify: `windows/lab/tests/calibration.rs:873` — re-pin every naming row
- Create: `book/src/chronicle/the-wearing.md`, `docs/retrospectives/the-wearing.md`

- [ ] **Step 1: Add the syllable-count metric.** Character length cannot distinguish "shorter words" from "same words spelled tighter", and §2.2 proved spelling was never the defect. Target 2–3.
- [ ] **Step 2: Add the transparency metric** — the share of committed names whose surface still contains its site-concept words verbatim. **The target is explicitly not 100%** (spec §8).
- [ ] **Step 3: Re-pin the calibration rows.** `name_collision_rate_is_measured_and_pinned` and the name-length rows are drift witnesses; append a dated comment in the existing style explaining the cause, as every prior campaign did.
- [ ] **Step 4: STOP — census regen needs Nathan's explicit authorization** (carve-out). Do not run it unprompted. When authorized: `HV_CENSUS=1 bash scripts/regenerate-artifacts.sh`, ~7 min locally, once.
- [ ] **Step 5: `make gate`, then `make gate-full`.** Attribute any red by diffing against the absorb chain before assuming it is this campaign's.
- [ ] **Step 6: Chronicle, retrospective, registry rows** (LANG-9/11/55 → `shipped`, LANG-27's constraint noted as lifted), Confidence Gradient re-score if any bet moved, promote F1–F4 into the retro's follow-up section.

## Self-Review

**Spec coverage.** §5 S1 → T1–T2. §5 S2 → T3–T5. §5 S3 → T6–T7. §5 S4 → T8–T9. §5 S5 → T10. §6 save-format → T1 (epoch), T3 (cohort), T6 (name epoch), T8 (phonology). §7 evidence → T11 steps 1–3. §8 success criteria → T11 step 5, plus T5 step 4's read-it-by-eye check for criterion 3. §9 deferrals → no tasks, correctly.

**Gap found and closed:** the spec's §6 claims S3 "introduces a name epoch (a `/v3` leg on the naming stream)". T6 retires the drawn stem and inserts wear, which changes stream consumption inside `glossed_name` — so the `/v3` leg is required, and T6's step 5 must add it. Flagged here rather than silently omitted; the implementer of T6 owns it.

**Type consistency.** `wear(&self, segments: &[Segment], frequency: f64) -> Vec<Segment>` is used identically in T6 and T9. `NameShape` has the same three variants in T7's test and its interface block. `Phonology.nuclei: Vec<usize>` is consistent across T8's test and interface. `draw_candidate`'s new `epoch: u32` parameter is threaded in T2 only.

**Known imprecision, deliberate.** T4's non-river exposure gates (`hill`, `valley`, `island`, `marsh`, `spring`, `ford`) and T7's steps 2–5 give the pattern and the acceptance test rather than final code, because the exact terrain predicates depend on neighbourhood queries whose shape is best settled against the real `Geosphere` API at implementation time. Each has a failing test that defines "done" precisely. This is a bounded exception to the no-placeholders rule, not a licence to leave others.
