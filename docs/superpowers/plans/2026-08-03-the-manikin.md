# The Manikin Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the goblin-anchored species baseline with a *manikin* — a named
reference vector that belongs to no creature — so that `0.5` stops meaning
"goblin" and starts meaning "the neutral midpoint."

**Architecture:** Three associated consts (`MindVector::MANIKIN`,
`SocietyVector::MANIKIN`, `PerceptionVector::MANIKIN`) replace one `const fn`
(`SocietyVector::baseline`) and two doc-comment-only conventions. Every value is
identical to today's, so the campaign is byte-neutral by construction; the two
tests that currently *assert the weld* between goblin and the baseline are
retargeted so one pins the fallback to the manikin and the other pins goblin's
coincidence with it as an explicitly-labelled characterization.

**Tech Stack:** Rust (edition 2024), `cargo nextest`, `mdbook`. No new
dependencies — the workspace allowlist is `serde`, `serde_json`, `libm` only.

**Spec:** `docs/superpowers/specs/2026-08-02-the-manikin-design.md`

## Global Constraints

- **No new external crates.** The allowlist is `ALLOWED_EXTERNAL` in
  `cli/tests/architecture.rs` (decision 0004, amended by 0041).
- **Every crate sets `#![warn(missing_docs)]`** — every public item, field and
  variant needs a one-line doc comment. All three new consts are public.
- **Every primitive at a `pub` boundary carries a `type-audit:` verdict tag.**
  The grammar is `bare-ok(<class>)`, e.g. `bare-ok(ratio)` — *not*
  `bare-ok(ratio: field)`. A malformed tag has been introduced from plan text
  twice; the grammar is stated once, here, and copied verbatim.
- **`cargo fmt` is the final step before every commit.** fmt-gate skips are the
  single most common review finding.
- **No `HashMap`/`HashSet`, no wall-clock time.** Enforced by `clippy.toml`.
- **Registry IDs (`BIO-39`, `PSY-manikin`, …) may appear ONLY under
  `book/src/frontier/`.** `cli/tests/docs_consistency.rs`'s
  `the_book_carries_no_registry_ids_or_process_vocabulary` bans them everywhere
  else in the book. This plan and the spec cite them freely; the chronicle and
  the new book section must name the *concept* instead ("a unit is not a frame").
- **Idea-registry Idea cells are capped at 600 characters**, enforced by
  `registry_idea_cells_are_within_budget`. Status is a closed vocabulary:
  `raw`, `elaborated`, `spec'd`, `shipped`, `ratified (NNNN)`, `rejected`.
- **The manikin is neutral on scalars and merely default on enums.** Say this in
  those words wherever it is documented (spec §8 item 5). Do not describe
  `Hierarchic` / `Rank` / `Diurnal` as neutral, average, or typical.
- **Byte-neutrality is preregistered (spec §4).** If any committed artifact
  drifts, STOP and report. Do not re-pin a golden to make the diff clean — that
  is explicitly forbidden.

---

## File Structure

| File | Responsibility | Task |
|---|---|---|
| `domains/species/src/lib.rs` | The three consts, the two retargeted tests, the vector and module docs | 1, 3, 4 |
| `cli/src/phonology.rs` | 1 production + 2 test call sites | 2 |
| `cli/src/audio.rs` | 1 production call site | 2 |
| `windows/worldgen/src/lib.rs` | 1 test call site | 2 |
| `windows/worldgen/src/descent.rs` | 2 stale prose references | 2 |
| `windows/worldgen/tests/name_pattern.rs` | 1 stale prose reference | 2 |
| `book/src/domains/species.md` | The "baseline goblin" section + dimension table | 4 |
| `book/src/domains/manikin.md` (new) | The manikin's own short chapter | 4 |
| `book/src/SUMMARY.md` | ToC entry for the new chapter (hand-authored) | 4, 7 |
| `book/src/introduction.md` | 2 goblin-baseline mentions | 5 |
| `book/src/domains/culture.md` | 1 mention | 5 |
| `book/src/domains/language.md` | 3 mentions incl. a table header | 5 |
| `book/src/domains/perception.md` | 3 mentions incl. a table header | 5 |
| `book/src/domains/settlement.md` | 1 mention | 5 |
| `book/src/gallery/the-meeting-seed-42.md` | 3 mentions — **hand-authored**, not generated | 5 |
| `book/src/chronicle/the-manikin.md` (new) | Campaign chronicle | 7 |
| `docs/retrospectives/the-manikin.md` (new) | Process lessons (decision 0020) | 7 |
| `book/src/frontier/idea-registry.md` | `PSY-2` status flip + the new enum-neutrality row | 7 |

**Verified call-site inventory** (grepped at merge `489a9ca5`; re-run
`grep -rn "SocietyVector::baseline\|::baseline()" --include=*.rs domains/ windows/ cli/`
if main is absorbed again before execution):

- Production: `cli/src/phonology.rs:59`, `cli/src/audio.rs:41`
- Test: `cli/src/phonology.rs:282`, `cli/src/phonology.rs:316`,
  `windows/worldgen/src/lib.rs:11650`
- Species-crate test (Task 3 rewrites): `domains/species/src/lib.rs:2897`
- Prose/doc references: `domains/species/src/lib.rs:171` (intra-doc link),
  `windows/worldgen/src/descent.rs:162`, `:227`,
  `windows/worldgen/tests/name_pattern.rs:108`

---

### Task 1: The three manikin consts

Additive and byte-neutral. `SocietyVector::baseline()` stays for now and
delegates, so nothing breaks; Task 2 removes it. Splitting it this way means a
reviewer can reject the *naming and values* without also reviewing the
migration.

**Files:**
- Modify: `domains/species/src/lib.rs` (add consts near each vector type; the
  existing `impl SocietyVector` block is at `:183`)

**Interfaces:**
- Consumes: nothing.
- Produces: `MindVector::MANIKIN`, `SocietyVector::MANIKIN`,
  `PerceptionVector::MANIKIN` — all `pub const <Type>: Self`, all `Copy`.
  Task 2 migrates callers to them; Task 3 asserts against them.

- [ ] **Step 1: Write the failing test**

Add to the `#[cfg(test)] mod tests` block in `domains/species/src/lib.rs`:

```rust
/// The manikin is the model's reference vector: neutral on every scalar,
/// and a designated default on the enums (which have no midpoint to be
/// neutral at — see the spec's flagged item 5). It belongs to no creature.
#[test]
fn the_manikin_is_neutral_on_scalars_and_default_on_enums() {
    let mind = MindVector::MANIKIN;
    for v in [
        mind.threat_response,
        mind.deliberation_latency,
        mind.time_horizon,
    ] {
        assert_eq!(v, 0.5, "every manikin mind scalar is the neutral midpoint");
    }

    let society = SocietyVector::MANIKIN;
    assert_eq!(society.in_group_radius, 0.5);
    assert_eq!(society.sociality, Sociality::Hierarchic);
    assert_eq!(society.status_basis, StatusBasis::Rank);

    let perception = PerceptionVector::MANIKIN;
    for v in [perception.night_vision, perception.sky_attention] {
        assert_eq!(v, 0.5, "every manikin perception scalar is the midpoint");
    }
    assert_eq!(perception.activity, ActivityCycle::Diurnal);
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale-species the_manikin_is_neutral -- --nocapture`
Expected: FAIL to **compile**, with `no associated item named 'MANIKIN' found`
for `MindVector`. A compile failure is the correct red here — the const does not
exist yet.

- [ ] **Step 3: Write minimal implementation**

Add an `impl MindVector` block immediately after the `MindVector` struct
(currently ending at `:164`):

```rust
impl MindVector {
    /// The manikin's mind: the neutral midpoint on every dimension.
    ///
    /// This is the model's reference vector, not any creature's psychology —
    /// no kind is obliged to sit here, and a kind that does, does so by
    /// authorship. See `SocietyVector::MANIKIN` for the full account.
    /// type-audit: bare-ok(ratio)
    pub const MANIKIN: Self = Self {
        threat_response: 0.5,
        deliberation_latency: 0.5,
        time_horizon: 0.5,
    };
}
```

Replace the existing `impl SocietyVector` block (`:183`–`:194`) with:

```rust
impl SocietyVector {
    /// The manikin's society: the reference reading a mixed consumer resolves
    /// for a `Solitary` kind that carries no society vector of its own.
    ///
    /// The manikin is a body that is nobody — the model's reference figure, in
    /// the lineage of the CIE standard observer and ICRP's "standard man". It
    /// is deliberately *not* a species: it has no `KindId`, no entry in any
    /// registry, no mass and no niche, so it can never be placed in a world
    /// and can never be a ghost.
    ///
    /// Note the asymmetry, which is real and not papered over: `0.5` is a
    /// principled **neutral midpoint** on a scalar, but `Sociality` and
    /// `StatusBasis` have no middle, so `Hierarchic` and `Rank` are a
    /// designated **default** rather than a neutral value.
    /// type-audit: bare-ok(ratio)
    pub const MANIKIN: Self = Self {
        sociality: Sociality::Hierarchic,
        status_basis: StatusBasis::Rank,
        in_group_radius: 0.5,
    };

    /// The manikin's society reading.
    ///
    /// Retained only so this task stays additive; `Task 2` removes it and
    /// migrates every caller to [`SocietyVector::MANIKIN`].
    pub const fn baseline() -> Self {
        Self::MANIKIN
    }
}
```

Add an `impl PerceptionVector` block immediately after the `PerceptionVector`
struct (currently ending at `:210`):

```rust
impl PerceptionVector {
    /// The manikin's perception: the neutral midpoint on both scalars, and
    /// `Diurnal` as the designated default schedule.
    ///
    /// As with `SocietyVector::MANIKIN`, `activity` is a default rather than a
    /// neutral value — a schedule has no midpoint.
    /// type-audit: bare-ok(ratio)
    pub const MANIKIN: Self = Self {
        activity: ActivityCycle::Diurnal,
        night_vision: 0.5,
        sky_attention: 0.5,
    };
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test -p hornvale-species the_manikin_is_neutral`
Expected: PASS.

- [ ] **Step 5: Verify the whole species crate is still green**

Run: `cargo test -p hornvale-species`
Expected: PASS, including the two not-yet-retargeted tests
(`goblin_is_the_baseline_vector`, `society_baseline_equals_the_goblin_authored_society`)
— they still pass because `baseline()` now returns `MANIKIN`, which holds
goblin's values.

- [ ] **Step 6: Type-audit and fmt**

Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`
Expected: PASS. If it reports an untagged primitive, the tag grammar is
`bare-ok(ratio)` on its own `///` line — see Global Constraints.

Run: `cargo fmt`

- [ ] **Step 7: Commit**

```bash
git add domains/species/src/lib.rs
git commit -m "feat(species): the manikin — a reference vector that is nobody

Three associated consts give the model an explicit reference figure. Every
value is identical to the goblin-anchored baseline it will replace, so this
commit changes no behaviour; SocietyVector::baseline is retained as a
delegate so the migration lands separately."
```

---

### Task 2: Migrate every consumer and delete `baseline()`

**Files:**
- Modify: `cli/src/phonology.rs:59`, `:282`, `:316`
- Modify: `cli/src/audio.rs:41`
- Modify: `windows/worldgen/src/lib.rs:11650`
- Modify: `windows/worldgen/src/descent.rs:162`, `:227` (prose only)
- Modify: `windows/worldgen/tests/name_pattern.rs:108` (prose only)
- Modify: `domains/species/src/lib.rs:171` (intra-doc link), and delete the
  `baseline()` fn added in Task 1

**Interfaces:**
- Consumes: `SocietyVector::MANIKIN` from Task 1.
- Produces: `SocietyVector::baseline` no longer exists anywhere in the
  workspace.

- [ ] **Step 1: Prove the migration is complete by deleting the target first**

Delete the `baseline()` function from `domains/species/src/lib.rs` — the whole
block added in Task 1 Step 3:

```rust
    /// The manikin's society reading.
    ///
    /// Retained only so this task stays additive; `Task 2` removes it and
    /// migrates every caller to [`SocietyVector::MANIKIN`].
    pub const fn baseline() -> Self {
        Self::MANIKIN
    }
```

- [ ] **Step 2: Run the workspace build to enumerate every caller**

Run: `cargo build --workspace --all-targets 2>&1 | tee /tmp/hv-manikin-callers.txt`
Expected: FAIL, with `no function or associated item named 'baseline' found`
at each of the five call sites. Read the file; do not re-run the build to see
a second error.

This is the migration's checklist, generated by the compiler rather than by a
grep — which is the point. A grep can miss a call; the build cannot.

- [ ] **Step 3: Migrate the five call sites**

In `cli/src/phonology.rs`, all three sites (`:59`, `:282`, `:316`) share this
shape — change each:

```rust
            .unwrap_or(hornvale_species::SocietyVector::baseline());
```

to:

```rust
            .unwrap_or(hornvale_species::SocietyVector::MANIKIN);
```

`cli/src/audio.rs:41` is the identical change.

In `windows/worldgen/src/lib.rs:11650`, change:

```rust
            ..hornvale_species::SocietyVector::baseline()
```

to:

```rust
            ..hornvale_species::SocietyVector::MANIKIN
```

- [ ] **Step 4: Fix the comments that name the old baseline**

`cli/src/phonology.rs:57` and `cli/src/audio.rs:39` both carry a comment
reading "the goblin baseline". Replace the phrase with "the manikin" in both,
so the comment describes what the code now does:

```rust
        // vector — resolve the manikin, same as the phonology page.
```

`domains/species/src/lib.rs:171` holds an intra-doc link that will now dangle:

```rust
/// [`SocietyVector::baseline`]. `in_group_radius` is a bare ratio in `[0, 1]`.
```

becomes:

```rust
/// [`SocietyVector::MANIKIN`]. `in_group_radius` is a bare ratio in `[0, 1]`.
```

`windows/worldgen/src/descent.rs:162` reads "Exactly the midpoint, where
`SocietyVector::baseline` sits." — change the reference to
`SocietyVector::MANIKIN`. Do the same at `:227` and at
`windows/worldgen/tests/name_pattern.rs:108`. These are prose-only edits; do
not change any value or assertion in them.

- [ ] **Step 5: Verify the build is clean and no reference survives**

Run: `cargo build --workspace --all-targets`
Expected: PASS.

Run: `grep -rn "SocietyVector::baseline\|::baseline()" --include=*.rs domains/ windows/ cli/`
Expected: **no output**. Any hit is a missed site.

- [ ] **Step 6: Run the affected crates' tests**

Run: `cargo test -p hornvale-species -p hornvale-worldgen -p hornvale`
Expected: PASS.

- [ ] **Step 7: Commit**

```bash
cargo fmt
git add -A
git commit -m "refactor(species): migrate every consumer to the manikin

SocietyVector::baseline is gone. Two production call sites, three test call
sites, and four prose references that named it — including an intra-doc link
that would have dangled, and two comments The Namesake added last week
describing 0.5 as 'where SocietyVector::baseline sits'."
```

---

### Task 3: Retarget the two tests that assert the weld

The campaign's substance. Today `society_baseline_equals_the_goblin_authored_society`
asserts that the fallback *is* goblin's authored society; that assertion is the
weld this campaign exists to cut.

**Files:**
- Modify: `domains/species/src/lib.rs` — `goblin_is_the_baseline_vector` (`:2514`)
  and `society_baseline_equals_the_goblin_authored_society` (`:2895`)

**Interfaces:**
- Consumes: `MindVector::MANIKIN`, `SocietyVector::MANIKIN` from Task 1.
- Produces: no API; two renamed tests.

- [ ] **Step 1: Replace the fallback test (D6)**

Replace `society_baseline_equals_the_goblin_authored_society` entirely:

```rust
    /// The fallback a mixed consumer resolves is the manikin — stated without
    /// reference to any people. Before The Manikin this test asserted the
    /// fallback equalled *goblin's* authored society, which welded the model's
    /// identity element to one inhabitant of the world.
    #[test]
    fn the_society_fallback_is_the_manikin() {
        assert_eq!(
            SocietyVector::MANIKIN,
            SocietyVector {
                sociality: Sociality::Hierarchic,
                status_basis: StatusBasis::Rank,
                in_group_radius: 0.5,
            },
            "the fallback is the manikin, and the manikin is nobody's"
        );
    }
```

- [ ] **Step 2: Replace the goblin test with a characterization test (D5)**

Replace `goblin_is_the_baseline_vector` entirely:

```rust
    /// CHARACTERIZATION, NOT CONTRACT.
    ///
    /// Goblin is currently authored at exactly the manikin's values. That is
    /// authorship, not definition: goblin was the first people written down,
    /// and nobody ever decided that goblins are unremarkable. Nothing in the
    /// model requires a kind to sit on the manikin, and this test does not
    /// make it a requirement.
    ///
    /// It exists so that characterising goblin — giving it the impulsive,
    /// short-horizon profile it has never actually been given — arrives as a
    /// visible diff on this test rather than as a silent shift in every
    /// goblin-bearing world's language envelope, culture rungs and demography
    /// weights. When that campaign comes, DELETE this test; do not "fix" it.
    ///
    /// The pattern is The Vacancy's, applied in this same registry to the
    /// `Autotroph`/Kleiber divergence.
    #[test]
    fn goblin_is_currently_authored_at_the_manikin() {
        let mind = *psyche_registry().get(&KindId("goblin")).unwrap();
        assert_eq!(
            mind,
            MindVector::MANIKIN,
            "goblin's mind is authored at the manikin (characterization)"
        );

        let society = *society_registry().get(&KindId("goblin")).unwrap();
        assert_eq!(
            society,
            SocietyVector::MANIKIN,
            "goblin's society is authored at the manikin (characterization)"
        );
    }
```

- [ ] **Step 3: Run the tests**

Run: `cargo test -p hornvale-species`
Expected: PASS, with `the_society_fallback_is_the_manikin` and
`goblin_is_currently_authored_at_the_manikin` both present and green, and
neither old test name remaining.

- [ ] **Step 4: Mutation-verify the characterization test actually bites**

A test that asserts nothing ships green. Prove this one fails when it should:
temporarily change goblin's `threat_response` in `psyche_registry()` from `0.5`
to `0.6`.

Run: `cargo test -p hornvale-species goblin_is_currently_authored_at_the_manikin`
Expected: **FAIL** on the mind assertion.

Revert the `0.6` back to `0.5` and re-run.
Expected: PASS.

Do not commit the mutation. This step is the only thing that distinguishes a
real guard from a decorative one.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add domains/species/src/lib.rs
git commit -m "test(species): cut the weld between goblin and the identity element

The fallback test no longer mentions goblin. The goblin test survives as an
explicitly-labelled characterization: goblin sits at the manikin by
authorship, not by definition, so a future characterisation of goblin is a
visible diff rather than a silent shift. Mutation-verified to fail when
goblin moves."
```

---

### Task 4: The docs, and the manikin's own chapter

**Files:**
- Modify: `domains/species/src/lib.rs:7` (module doc), `:155`, `:169`, `:196`
  (the three vector docs)
- Modify: `book/src/domains/species.md`
- Create: `book/src/domains/manikin.md`
- Modify: `book/src/SUMMARY.md`

**Interfaces:**
- Consumes: everything from Tasks 1–3.
- Produces: no API.

- [ ] **Step 1: Fix the module doc**

`domains/species/src/lib.rs:7` currently reads:

```rust
//! social grammar stays code (spec §2). Goblin is the baseline: scalars 0.5,
//! default enum variants; every downstream modulation is the identity function
//! at this vector. The peopled speech data (articulation vector, lexicon,
```

Replace those three lines with:

```rust
//! social grammar stays code (spec §2). The MANIKIN is the reference vector:
//! scalars at the 0.5 midpoint, designated default enum variants; every
//! downstream modulation is the identity function at this vector. It is
//! nobody's — no `KindId`, no registry row — and a kind sitting on it does so
//! by authorship. The peopled speech data (articulation vector, lexicon,
```

- [ ] **Step 2: Fix the three vector docs**

At `:155` (`MindVector`) and `:196` (`PerceptionVector`), the phrase
`0.5 ≡ the goblin baseline` appears. Replace both occurrences with
`0.5 ≡ the manikin's neutral midpoint`.

At `:169` (`SocietyVector`), `resolve [`SocietyVector::baseline`]` was already
repointed in Task 2 Step 4; no further change.

- [ ] **Step 3: Verify docs build without dangling links**

Run: `cargo doc -p hornvale-species --no-deps 2>&1 | grep -i "unresolved\|broken" || echo "no broken intra-doc links"`
Expected: `no broken intra-doc links`.

- [ ] **Step 4: Rewrite the book's baseline section**

In `book/src/domains/species.md`, the section headed **"The closed vector,
baseline goblin"** (around `:33`) and the dimension table (around `:241`–`:264`)
both state the goblin anchoring as fact.

Rename the section to **"The closed vector, and the manikin"**. Its prose must
make three points, in the book's register (technical, comprehensible without
reading the code):

1. Every scalar is a bare ratio whose `0.5` is the manikin's midpoint.
2. The manikin is a reference figure belonging to no creature, and cannot be
   placed in a world because it has no body to place.
3. Goblin happens to be authored at the manikin's values, and that is a fact
   about goblin rather than a fact about the manikin.

In the dimension table, rename the column header `Goblin (baseline)` to
`Manikin` and add a sentence beneath the table noting that goblin's authored
values currently coincide with this column.

**Do not cite registry IDs** (`BIO-39` and friends) anywhere in the book —
name the concept instead. See Global Constraints.

- [ ] **Step 5: Write the manikin's chapter**

Create `book/src/domains/manikin.md`. Roughly 400–600 words, covering:

- What a manikin is: a tailor's reference body, deliberately nobody's.
- Why the model needs one: a bare `0.5` is a datum, and a datum needs a frame.
  The frame used to be a particular people, which meant the number quietly
  changed meaning as the roster grew — the same class of error as reading an
  elevation against the wrong datum, which once made a people's documented
  highland stronghold unoccupiable. (Name the concept; do not cite `BIO-39`.)
- Why it is *not* a species: it has no mass, no niche, and no carrying
  capacity, so it can never be placed and can never become a kind that exists
  in the registry but in no world.
- The lineage: the CIE standard observer, ICRP's "standard man", the
  anthropometric manikin. And the sharper analogy — anchoring a unit on a
  chosen exemplar is the Kilogramme des Archives, whose replacement was not a
  better cylinder but an invariant.
- The honest asymmetry: neutral midpoint on the six scalars, designated default
  on the three enums, because a schedule and an authority-shape have no middle.

- [ ] **Step 6: Add the chapter to the ToC**

`book/src/SUMMARY.md` is hand-authored (unlike the Gallery and Reference H1s,
which are code-generated). Add the entry directly beneath the Species line:

```markdown
    - [The Manikin](./domains/manikin.md)
```

- [ ] **Step 7: Verify the book builds and the drift-check passes**

Run: `mdbook build book`
Expected: PASS, no warnings about a file missing from `SUMMARY.md`.

Run: `cargo test -p hornvale --test docs_consistency`
Expected: PASS — in particular
`the_book_carries_no_registry_ids_or_process_vocabulary` and
`all_knowledge_doc_links_resolve`.

- [ ] **Step 8: Commit**

```bash
cargo fmt
git add -A
git commit -m "docs(species): the manikin, and the frame it replaces

The module and vector docs stop defining 0.5 as goblin's. The book gains a
chapter on the reference body that is nobody — what it is, why a datum needs
a frame, and why it is deliberately not a species."
```

---

### Task 5: The book freshness sweep

The Definition of Done requires that the book never lag merged reality. Six
chapters state the goblin anchoring as the model's frame, and they were found
by grep rather than by memory — an earlier draft of this plan swept only
`species.md`.

**Files** (verified by
`grep -rn "goblin baseline\|baseline goblin\|Goblin (baseline)" book/src/ --include=*.md`):
- Modify: `book/src/introduction.md:124`, `:137`
- Modify: `book/src/domains/culture.md:59`
- Modify: `book/src/domains/language.md:66`, `:87` (table header), `:204`
- Modify: `book/src/domains/perception.md:72`, `:75`, `:115` (table header)
- Modify: `book/src/domains/settlement.md:59`
- Modify: `book/src/gallery/the-meeting-seed-42.md:65`, `:152`, `:164`

**Interfaces:**
- Consumes: the vocabulary settled in Task 4.
- Produces: no API.

**Two rules that decide each edit:**

1. Where the text states the *frame* — "identity at the goblin baseline",
   "the goblin baseline is the identity" — the claim is now false and must
   become "identity at the manikin" / "the manikin is the identity".
2. Where the text describes *goblin's actual values* — a table column of what
   goblin carries, or a worked example at goblin's vector — the claim is still
   true. Rename a `Goblin (baseline)` column header to `Manikin`, but leave the
   surrounding numbers and worked examples alone.

- [ ] **Step 1: Confirm the gallery page is hand-authored, not generated**

Run: `grep -n "the-meeting" scripts/regenerate-artifacts.sh`
Expected: only a `census-of-the-meeting.study.json` line — **no** line writing
`book/src/gallery/the-meeting-seed-42.md`. That file is hand-authored prose and
is safe to edit directly.

This matters because `book/src/gallery/` is inside Task 6's drift-check path.
Editing a *generated* gallery page by hand would be wrong and would surface as
drift; editing this hand-authored one is fine **provided Task 5 is committed
before Task 6 runs**, so the drift check compares regenerated output against a
tree that already contains these edits.

- [ ] **Step 2: Sweep the five domain/introduction chapters**

Apply rules 1 and 2 above to `introduction.md`, `culture.md`, `language.md`,
`perception.md`, and `settlement.md`. Rename both `Goblin (baseline)` table
headers (`language.md:87`, `perception.md:115`) to `Manikin`.

**Do not cite registry IDs** anywhere in the book.

- [ ] **Step 3: Sweep the gallery essay**

`book/src/gallery/the-meeting-seed-42.md` uses "the goblin baseline" three
times to explain why certain expressions collapse to plain constants. Rule 2
applies to `:65` and `:152` — they describe goblin's actual vector
`(Diurnal, 0.5, 0.5)`, which is still exactly true. Rephrase only the framing
words ("the goblin baseline" → "the manikin, where goblin's vector sits"), and
change no number.

- [ ] **Step 4: Verify nothing was missed**

Run:

```bash
grep -rn "goblin baseline\|baseline goblin\|Goblin (baseline)" book/src/ --include=*.md \
  | grep -v "^book/src/chronicle/" | grep -v "^book/src/frontier/"
```

Expected: **no output.** Chronicle entries are historical records of what past
campaigns did and are deliberately excluded — do not rewrite history. The
frontier is excluded because `PSY-2`'s own row legitimately names the idea.

- [ ] **Step 5: Verify the book builds**

Run: `mdbook build book`
Expected: PASS.

Run: `cargo test -p hornvale --test docs_consistency`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add -A
git commit -m "docs(book): sweep the goblin baseline out of six chapters

Every chapter that stated the goblin anchoring as the model's frame now
names the manikin. Chapters describing goblin's actual values keep them —
goblin still sits where it sat; it just no longer defines the midpoint."
```

---

### Task 6: The preregistered readout

Spec §4 froze a prediction before any of this was written: **zero artifact
drift**. This task is where it is unblinded, and it is the one task whose
failure is a finding rather than a bug.

**Files:**
- No source changes expected. If any generated artifact changes, that IS the
  result — stop and report.

**Interfaces:**
- Consumes: Tasks 1–5, **all committed** — the drift check compares regenerated
  output against the committed tree, so any uncommitted hand edit under
  `book/src/gallery/` would read as drift and produce a false falsification.
- Produces: the recorded readout, used by Task 7's chronicle.

- [ ] **Step 1: Regenerate every artifact**

Run: `make rebaseline`
Expected: completes without error. This runs
`scripts/regenerate-artifacts.sh` (the three seed-42 almanacs, the elevation
map, registry/manifest dumps, lab studies, the type-audit report). It does
**not** run censuses, which is correct — the campaign is not authorized for one.

- [ ] **Step 2: Run the preregistered drift check**

Run:

```bash
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/
```

Expected: **exit 0, empty diff.** `docs/audits/` is in the list because the
type-audit report drifts on any pub-boundary change, and omitting it is a
common miss — this campaign adds three public consts, so it is exactly the
case that would surface there.

- [ ] **Step 3: If the diff is NOT empty — STOP**

Do not re-pin. Do not run `make rebaseline-goldens`. Capture the diff:

```bash
git diff book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/ \
  > /tmp/hv-manikin-drift.txt
```

Report the contents. A non-empty diff falsifies the spec's central premise —
it would mean something derives from *which kind* is the baseline rather than
from its values, and the campaign is materially larger than specified. Per
spec §4 that stops work for a re-spec. It is a finding worth the campaign on
its own.

Note the one legitimate exception to expect: if `docs/audits/type-audit-report.md`
changed *only* by adding rows for the three new `MANIKIN` consts, that is the
report tracking new public surface, not a world changing. Verify by reading the
diff — it must contain no numeric change to any world value. Commit it if so
and say which case it was.

- [ ] **Step 4: Run the full commit gate**

Run: `make gate`
Expected: PASS.

Budget `timeout: 3600000` — this measured **22–37 min** on this Mac
(2026-07-30), not the ~4 min decision 0040 budgeted. Run it once and read the
output; do not re-run to grep a second line. **Only one gating agent at a time
on this Mac** — a single gate already saturates ~8.4 of 10 cores, so confirm no
sibling session is gating before starting.

- [ ] **Step 5: Record the readout**

Write the result — whichever way it came out — into
`.superpowers/sdd/readout.md` in the worktree, for Task 7 to promote into the
chronicle:

```markdown
# The Manikin — preregistered readout

**Prediction (spec §4, frozen before implementation):** zero artifact drift.
**Result:** <clean | drifted>
**Command:** git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/
**Output:** <paste, or "empty">
**make gate:** <pass/fail, wall time>
```

- [ ] **Step 6: Commit (only if something changed)**

If the diff was empty, there is nothing to commit and that is the expected
outcome — say so rather than manufacturing a commit.

---

### Task 7: Chronicle, retrospective, and registry status

**Files:**
- Create: `book/src/chronicle/the-manikin.md`
- Modify: `book/src/SUMMARY.md` (chronicle ToC entry)
- Create: `docs/retrospectives/the-manikin.md`
- Modify: `book/src/frontier/idea-registry.md`

**Interfaces:**
- Consumes: the readout from Task 5.
- Produces: campaign close artifacts.

- [ ] **Step 1: Write the chronicle**

Create `book/src/chronicle/the-manikin.md`, in the book's register. It must
carry the readout from Task 5 **whichever way it came out** — a falsified
prediction is a finding, and several campaigns have shipped the null as the
headline.

Cover: what the weld was and how it was pinned by test; why the obvious repair
(re-anchoring on humans) was rejected — humans are not average, their night
vision sits below much of the roster; why the manikin is a value and not a
species; and the enum asymmetry stated honestly.

**No registry IDs anywhere in this file.**

- [ ] **Step 2: Add the chronicle to the ToC**

Add to `book/src/SUMMARY.md` under the Chronicle section, in sequence:

```markdown
    - [The Manikin](./chronicle/the-manikin.md)
```

- [ ] **Step 3: Write the retrospective (decision 0020)**

Create `docs/retrospectives/the-manikin.md` — process lessons, not product.
Registry IDs ARE permitted here (it is not the book). At minimum, record:

- The spec's call-site inventory was wrong (3 of 5 "production" sites were
  tests) and four prose references arrived with The Namesake *after* the spec
  was drafted. The fix that generalizes: Task 2 deletes the target first and
  lets the **compiler** enumerate callers, because a grep-derived inventory is
  only as complete as the grep.
- The 600-char registry budget rejected all three capture rows on first write.
- `LANG-53` already existed and already named this roster, found only because
  the registry was searched before minting a row.
- **`PSY-2` also already existed and was missed**, and a duplicate
  `PSY-manikin` was minted and committed before the collision was found at
  plan time. The registry *was* searched — for the species topic (elf, dwarf,
  human, roster), never for the *idea* (baseline, identity, frame). The lesson
  that generalizes: search the registry for the mechanism you are about to
  build, not only for the subject matter you are building it for. This is the
  same failure that produced a duplicate `TOOL-24`, which travelled through a
  spec, a plan, a study JSON and a decision before anyone noticed.
- The book sweep was six chapters, not the one the spec's DoD named. Found by
  grep; would have shipped a book contradicting itself otherwise.

- [ ] **Step 4: Flip the registry rows**

The row to flip is **`PSY-2`**, not a new one. `PSY-2` predates this campaign
and already described it — *"an abstract reference baseline no species need
occupy"* — which is why the duplicate `PSY-manikin` minted during planning was
deleted before execution. Do not mint another.

Change `PSY-2`'s Status from `spec'd (baseline half)` to
`shipped (baseline half)` and add the chronicle to its **Where**:
`[The Manikin](../chronicle/the-manikin.md)`. The qualifier is load-bearing —
`PSY-2`'s other half (epoch-suffixed migration of unqualified stream labels,
ADR 0006) is untouched by this campaign and must remain visibly open.

Remember: repointing **Where** REPLACES a row's prose — it does not append
to it.

Add one new row in the same section, for the question this campaign
deliberately did not answer (spec §8 item 5). Keep it **under 600 characters**:

```markdown
| PSY-unmarked-enum | **An enum dimension has no midpoint, so its "neutral" is only a default** — the manikin is a principled `0.5` on six scalars but merely inherits `Hierarchic`/`Rank`/`Diurnal` on three enums, with no argument that those are unmarked rather than first-authored. Options: an explicit `Unmarked` variant per enum; a documented default with no neutrality claim (today); or dropping enums for ordinals where one exists. Bites when a kind is authored that is genuinely atypical on an enum axis | raw | med | [The Manikin](../chronicle/the-manikin.md) |
```

- [ ] **Step 5: Verify**

Run: `cargo test -p hornvale --test docs_consistency`
Expected: PASS — checks the 600-char budget, ID uniqueness, the closed status
vocabulary, and the registry-ID ban in the book.

Run: `mdbook build book`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add -A
git commit -m "docs(the-manikin): chronicle, retrospective, and registry close

Records the preregistered readout, the rejected human-anchored repair, and
the enum asymmetry the campaign declined to resolve."
```

- [ ] **Step 7: Promote the decision ledger before teardown**

`.superpowers/sdd/` is git-ignored and dies with the worktree. Confirm every
material entry in `.superpowers/sdd/decision-ledger.md` is reflected in the
spec's decisions or the retrospective **before** the worktree is removed.

- [ ] **Step 8: Run preflight and hand back**

Run: `make preflight`
Expected: GO, or a NO-GO on ancestry if main has moved again — in which case
merge main in, re-run the full gate, and re-run preflight.

Do **not** merge to main or push. Both are Nathan's calls (G6).

---

## Notes for the executor

- **The three-attempt rule applies.** If a step fails three times, stop and
  document what was tried and the exact error rather than trying a fourth.
- **This campaign should change no world.** If you find yourself editing a
  numeric value in a registry, or reaching for `make rebaseline-goldens`, you
  have left the plan — stop and report.
- **Cost-ordered iteration.** `cargo fmt` and `cargo clippy` first, then the
  scoped crate test, and `make gate` only at Task 5. Do not run `--workspace`
  after every edit.
