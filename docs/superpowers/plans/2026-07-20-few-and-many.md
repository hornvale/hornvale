# Few and Many Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ship LANG-44 — the first listener-side epistemic filter: a
per-species `NumeracyRung` (Subitizing/FullCounting/Decimals) caps how
much of a heard quantity a listener retains, reusing one render function
for both a speaker's own rendering and a listener's comprehension
degradation.

**Architecture:** A new module `domains/language/src/numeracy.rs` holds
the rung type, its per-species draw, and the shared render function
(mirrors `domains/language/src/morphology.rs`'s `morph_depths` draw
shape). `windows/book/src/lib.rs` gets one new function,
`comprehend_quantity`, demonstrating the mechanism against the existing
`fact_for` day-length recovery path — no existing function is modified.

**Tech Stack:** Rust (edition 2024), existing `hornvale-language` and
`hornvale-book` crate primitives only (`Seed`, `Value`, `fact_for`,
`cardinal`, `quantity`) — no new dependencies, no new draws beyond the one
mirrored from `morph_depths`'s existing shape.

## Global Constraints

- No `HashMap`/`HashSet` anywhere — `BTreeMap`/`BTreeSet`/`Vec` only.
- Every public item, field, and variant gets a one-line doc comment
  (`#![warn(missing_docs)]` stays set in the new file).
- Every new `pub`-boundary bare primitive needs a `type-audit:` verdict
  tag in its doc comment (`bare-ok(identifier-text)` is the established
  convention for an opaque label like these; `bare-ok(prose)` is the
  convention `cardinal`/`quantity` themselves already use for a rendered
  string).
- Zero new randomness beyond the one draw mirrored from existing
  precedent (`numeracy_rung`) — `render_quantity_at_rung` and
  `comprehend_quantity` are pure functions, no `Seed`/`Stream` use in
  either.
- `cargo fmt` as the final step before every commit.
- One new permanent seed-derivation stream this plan introduces:
  `language/<species>/grammar/numeracy-rung` — must be added to
  `domains/language/src/lib.rs`'s `stream_labels()` in the task that
  introduces it.
- Do not modify `fact_for`, `fragment_for`, `cardinal`, or `quantity` —
  this campaign only reads and reuses them.

---

### Task 1: `NumeracyRung`, its per-species draw, and the shared render function

**Files:**
- Create: `domains/language/src/numeracy.rs`
- Modify: `domains/language/src/lib.rs` (register the module, add the
  `stream_labels()` entry)

**Interfaces:**
- Produces: `pub enum NumeracyRung { Subitizing, FullCounting, Decimals }`
  (derives `Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord` — variant
  declaration order is the intended precision ordering, least to most
  precise); `pub fn numeracy_rung(seed: &Seed, species: &str) ->
  NumeracyRung`; `pub fn render_quantity_at_rung(x: f64, rung:
  NumeracyRung) -> String`. Task 2 consumes `NumeracyRung` and
  `render_quantity_at_rung` directly (as `hornvale_language::numeracy::{NumeracyRung, render_quantity_at_rung}`).

- [ ] **Step 1: Write the failing tests**

Create `domains/language/src/numeracy.rs`:

```rust
//! LANG-44: numeracy as a per-listener quantity register — the first
//! listener-side epistemic filter in this crate (every existing filter,
//! `crate::account`'s four-stage stack among them, is speaker-side). See
//! `docs/superpowers/specs/2026-07-20-few-and-many-design.md`.
#![warn(missing_docs)]

use hornvale_kernel::Seed;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn numeracy_rung_is_pure() {
        let a = numeracy_rung(&Seed(1), "test");
        let b = numeracy_rung(&Seed(1), "test");
        assert_eq!(a, b);
    }
}
```

Add `pub mod numeracy;` to `domains/language/src/lib.rs` alongside the
other `pub mod` lines (after `pub mod naming;`, before `pub mod packs;`,
keeping the existing alphabetical order):

```rust
pub mod naming;
pub mod numeracy;
pub mod packs;
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale-language numeracy_rung_is_pure -- --nocapture`
Expected: FAIL to compile — `cannot find function 'numeracy_rung' in this scope`, `cannot find type 'NumeracyRung'`.

- [ ] **Step 3: Write the implementation**

Insert into `domains/language/src/numeracy.rs`, above the `#[cfg(test)]`
module:

```rust
/// A tongue's numeral-system rung: how far its counting words go past the
/// near-universal subitizing floor. Ordered least to most precise
/// (`Subitizing` < `FullCounting` < `Decimals`) — drawn per species,
/// independent of any psychology/culture vector, the same anti-astrology
/// posture [`crate::morphology::MorphDepth`] already keeps for
/// grammaticalization depth: numeral-system elaborateness is
/// historically/environmentally contingent (trade, agriculture,
/// measurement culture), never a claim about cognitive capability.
/// `Subitizing` means this language's numeral system builds no counting
/// words beyond the near-universal pre-linguistic one/two/few/many floor
/// (rapid exact perception of roughly one to four items is itself a
/// human — and animal — universal, not a rung some cultures lack) — a
/// linguistic-typology claim, never a capacity one.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum NumeracyRung {
    /// No counting words beyond one/two/few/many.
    Subitizing,
    /// Exact integer counting, rendered via `cardinal`-style words.
    FullCounting,
    /// Measured decimal approximation, rendered via `quantity`-style
    /// "about X.Y".
    Decimals,
}

/// Preregistered numeracy-rung weights over `[Subitizing, FullCounting,
/// Decimals]`: full counting is the near-universal typological default;
/// subitizing-only and decimal-measuring systems are both real but
/// minority cases.
const NUMERACY_RUNG_WEIGHTS: [f64; 3] = [10.0, 75.0, 15.0];

/// The `weighted_index` bucket order the draw uses: 0 = `Subitizing`,
/// 1 = `FullCounting`, 2 = `Decimals` (frozen by the weight table above).
fn rung_from_bucket(bucket: usize) -> NumeracyRung {
    match bucket {
        0 => NumeracyRung::Subitizing,
        1 => NumeracyRung::FullCounting,
        _ => NumeracyRung::Decimals,
    }
}

/// Draw `species`' numeracy rung — one permanent stream:
/// `language/<species>/grammar/numeracy-rung`. Drawn, independent of any
/// psychology/culture vector (see [`NumeracyRung`]'s own doc — the
/// anti-astrology line).
/// type-audit: bare-ok(identifier-text)
pub fn numeracy_rung(seed: &Seed, species: &str) -> NumeracyRung {
    let mut stream = seed
        .derive("language")
        .derive(species)
        .derive("grammar")
        .derive("numeracy-rung")
        .stream();
    rung_from_bucket(
        stream
            .weighted_index(&NUMERACY_RUNG_WEIGHTS)
            .expect("NUMERACY_RUNG_WEIGHTS is fixed and positive"),
    )
}

/// Render `x` at `rung`'s own precision — the shared codec both a
/// speaker's own rendering and a listener's comprehension degradation
/// reuse (spec §3.2): the same function serves both directions, never two
/// independently-authored mechanisms. `Subitizing` renders exact "one"/
/// "two" only for those exact values; any other value in `(1, 3)` renders
/// the qualitative "more than one" (a fraction like `1.5` degrades to
/// this, matching the spec's own worked example) rather than rounding to
/// a false-precision exact word; `[3, 5)` renders "few"; `5` and above
/// renders "many". `FullCounting` rounds to the nearest integer and
/// renders via [`crate::clause::cardinal`]. `Decimals` delegates to
/// [`crate::clause::quantity`], unchanged from today's only decimal path.
/// type-audit: bare-ok(prose)
pub fn render_quantity_at_rung(x: f64, rung: NumeracyRung) -> String {
    match rung {
        NumeracyRung::Subitizing => {
            if x == 1.0 {
                "one".to_string()
            } else if x == 2.0 {
                "two".to_string()
            } else if x < 1.0 {
                "less than one".to_string()
            } else if x < 3.0 {
                "more than one".to_string()
            } else if x < 5.0 {
                "few".to_string()
            } else {
                "many".to_string()
            }
        }
        NumeracyRung::FullCounting => crate::clause::cardinal(x.round() as u64),
        NumeracyRung::Decimals => crate::clause::quantity(x),
    }
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test -p hornvale-language numeracy_rung_is_pure -- --nocapture`
Expected: PASS (1 test).

- [ ] **Step 5: Add the ladder-is-real test and the stream_labels() entry**

Append to `domains/language/src/numeracy.rs`'s `tests` module:

```rust
    #[test]
    fn numeracy_rung_covers_all_three_buckets_across_many_seeds() {
        let mut saw_subitizing = false;
        let mut saw_full_counting = false;
        let mut saw_decimals = false;
        for i in 0..200u64 {
            match numeracy_rung(&Seed(i), "test") {
                NumeracyRung::Subitizing => saw_subitizing = true,
                NumeracyRung::FullCounting => saw_full_counting = true,
                NumeracyRung::Decimals => saw_decimals = true,
            }
        }
        assert!(saw_subitizing && saw_full_counting && saw_decimals);
    }

    #[test]
    fn the_ladder_renders_genuinely_different_text() {
        // Law 2 (spec §4): the three rungs must not collapse into fewer
        // effective rungs — a fractional value renders three DIFFERENT
        // strings, one per rung.
        let x = 1.5;
        let subitizing = render_quantity_at_rung(x, NumeracyRung::Subitizing);
        let full_counting = render_quantity_at_rung(x, NumeracyRung::FullCounting);
        let decimals = render_quantity_at_rung(x, NumeracyRung::Decimals);
        assert_eq!(subitizing, "more than one");
        assert_eq!(full_counting, "two");
        assert_eq!(decimals, "about 1.5");
        assert_ne!(subitizing, full_counting);
        assert_ne!(full_counting, decimals);
        assert_ne!(subitizing, decimals);
    }
```

Add to `domains/language/src/lib.rs`'s `stream_labels()` function (find
the existing block ending with the
`"language/family/<family>/morph/tense/past"` tuple — the last entry
LANG-43 added — and insert immediately after that tuple's closing `),`):

```rust
        (
            "language/<species>/grammar/numeracy-rung",
            "The species' drawn numeral-system rung (Subitizing/FullCounting/Decimals) — how far counting words go past the universal subitizing floor",
        ),
```

- [ ] **Step 6: Run the full numeracy module's tests and the crate's existing suite**

Run: `cargo test -p hornvale-language numeracy:: -- --nocapture`
Expected: PASS (3 tests: `numeracy_rung_is_pure`,
`numeracy_rung_covers_all_three_buckets_across_many_seeds`,
`the_ladder_renders_genuinely_different_text`).

Run: `cargo test -p hornvale-language`
Expected: PASS, same count as before this task plus the 3 new tests — no
existing test's outcome changes (nothing existing was touched except an
additive `pub mod` line and an additive `stream_labels()` entry).

- [ ] **Step 7: Commit**

```bash
git add domains/language/src/numeracy.rs domains/language/src/lib.rs
git commit -m "feat(language): NumeracyRung + per-species draw + shared render codec — few-and-many T1"
```

---

### Task 2: The comprehension integration point, proven against real fact_for text

**Files:**
- Modify: `windows/book/src/lib.rs`

**Interfaces:**
- Consumes: `hornvale_language::numeracy::{NumeracyRung, render_quantity_at_rung}` (Task 1); this file's own existing private `fact_for(fragment: &str) -> Option<(String, Value)>` (line 1723), `Value` (from `hornvale_kernel`, already imported in this file), and the existing test helper `fn generated(seed: u64) -> World` (line 2393) plus the existing constant `DAY_LENGTH_STD` (already imported at the top of this file from `hornvale_astronomy::facts`).
- Produces: `fn comprehend_quantity(fragment: &str, listener_rung: NumeracyRung) -> Option<String>` — private, module-scoped (matching `fact_for`'s own visibility), consumed only by this task's own tests. No later task depends on it.

- [ ] **Step 1: Write the failing tests**

Add `use hornvale_language::numeracy::{NumeracyRung, render_quantity_at_rung};`
to the top-level `use` block in `windows/book/src/lib.rs` (alongside the
existing `use hornvale_language::clause::{...};` line).

Append to `windows/book/src/lib.rs`'s existing `#[cfg(test)] mod tests`
block (the same module `fact_for_inverts_fragment_for_over_the_closed_space`
already lives in):

```rust
    /// Law 3 (spec §4): the fixed-speaker special case. Every speaker
    /// today renders at the ceiling rung (`Decimals`), so comprehension
    /// applied at any listener rung must agree EXACTLY with calling
    /// `render_quantity_at_rung` directly on the recovered value — proving
    /// the collapsed `min(Decimals, listener_rung) == listener_rung`
    /// claim is what the code actually does, not merely what the spec
    /// claims.
    #[test]
    fn comprehend_quantity_agrees_with_the_direct_render_at_every_rung() {
        for seed in [1u64, 2, 3] {
            let world = generated(seed);
            for fact in world.ledger.find(DAY_LENGTH_STD) {
                let Value::Number(days) = fact.object else {
                    continue;
                };
                let value = Value::Number(days);
                let fragment = match fragment_for(DAY_LENGTH_STD, &value) {
                    Some(Fragment::Trailing(t)) => t,
                    _ => panic!("expected a Trailing fragment for {days}"),
                };
                let truncated = (days * 10.0).trunc() / 10.0;
                for rung in [
                    NumeracyRung::Subitizing,
                    NumeracyRung::FullCounting,
                    NumeracyRung::Decimals,
                ] {
                    assert_eq!(
                        comprehend_quantity(&fragment, rung),
                        Some(render_quantity_at_rung(truncated, rung)),
                        "comprehend_quantity disagreed with the direct render at rung {rung:?} for {fragment:?}"
                    );
                }
            }
        }
    }

    /// Law 4 (spec §4): no behavior change to the existing path.
    /// `fact_for` itself, unmodified, must still recover the untruncated
    /// surface value exactly as it did before this campaign.
    #[test]
    fn fact_for_itself_is_unchanged() {
        let world = generated(1);
        for fact in world.ledger.find(DAY_LENGTH_STD) {
            let Value::Number(days) = fact.object else {
                continue;
            };
            let value = Value::Number(days);
            let fragment = match fragment_for(DAY_LENGTH_STD, &value) {
                Some(Fragment::Trailing(t)) => t,
                _ => panic!("expected a Trailing fragment for {days}"),
            };
            let truncated = (days * 10.0).trunc() / 10.0;
            assert_eq!(
                fact_for(&fragment),
                Some((DAY_LENGTH_STD.to_string(), Value::Number(truncated)))
            );
        }
    }

    /// A fragment `fact_for` does not recognize returns `None`, exactly as
    /// `fact_for` itself would.
    #[test]
    fn comprehend_quantity_returns_none_for_an_unrecognized_fragment() {
        assert_eq!(
            comprehend_quantity("this is not a real fragment", NumeracyRung::Subitizing),
            None
        );
    }
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p hornvale-book comprehend_quantity_agrees_with_the_direct_render_at_every_rung fact_for_itself_is_unchanged comprehend_quantity_returns_none_for_an_unrecognized_fragment -- --nocapture`
Expected: FAIL to compile — `cannot find function 'comprehend_quantity' in this scope`. (`fact_for_itself_is_unchanged` compiles fine on its own since it only calls the pre-existing `fact_for`, but the whole test binary fails to compile until `comprehend_quantity` exists, so all three report as failing to build together.)

- [ ] **Step 3: Write the implementation**

Insert into `windows/book/src/lib.rs`, immediately after the existing
`fact_for` function (after its closing `}`, before the
`/// The closed complement set a `parse_line` call recognizes...` doc
comment that precedes `parse_context`):

```rust
/// Apply a listener's numeracy rung to a heard quantity fragment (LANG-44
/// spec §3.4): recover the fragment's stated surface value exactly as
/// [`fact_for`] already does, then re-render it at `listener_rung` via
/// [`hornvale_language::numeracy::render_quantity_at_rung`] — the
/// listener retains only what their own rung can express, never more
/// than the speaker actually said. Every speaker today renders at the
/// ceiling rung (`NumeracyRung::Decimals`), so this is the collapsed
/// `min(Decimals, listener_rung) == listener_rung` special case (spec
/// §3.3) — full bidirectional rung variation is a follow-up. Returns
/// `None` for a fragment [`fact_for`] itself would not recognize, or
/// whose recovered value is not a number (e.g. a star class).
fn comprehend_quantity(fragment: &str, listener_rung: NumeracyRung) -> Option<String> {
    let (_, value) = fact_for(fragment)?;
    match value {
        Value::Number(x) => Some(render_quantity_at_rung(x, listener_rung)),
        _ => None,
    }
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p hornvale-book comprehend_quantity_agrees_with_the_direct_render_at_every_rung fact_for_itself_is_unchanged comprehend_quantity_returns_none_for_an_unrecognized_fragment -- --nocapture`
Expected: PASS (3 tests).

- [ ] **Step 5: Run the full book crate's test suite**

Run: `cargo test -p hornvale-book`
Expected: PASS, no existing test's outcome changes — `fact_for`,
`fragment_for`, and every other existing function in this file are
unmodified.

- [ ] **Step 6: `cargo fmt`, clippy, and the type-audit check**

Run: `cargo fmt -p hornvale-book -p hornvale-language`
Run: `cargo clippy -p hornvale-book -p hornvale-language --all-targets -- -D warnings`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`
Expected: all clean. If the type-audit check reports an untagged
primitive, add the matching `type-audit: bare-ok(...)` line to that
item's doc comment (matching this plan's existing tag style), then
re-run the check.

- [ ] **Step 7: Regenerate the type-audit report**

Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`

- [ ] **Step 8: Commit**

```bash
git add windows/book/src/lib.rs docs/audits/type-audit-report.md
git commit -m "feat(book): comprehend_quantity — the listener-rung comprehension filter, proven against fact_for — few-and-many T2"
```
