# Stream Labels Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Every `Seed::derive` call site stops accepting a bare `&str` —
a typo'd inline literal becomes a compile error instead of a silent,
unrelated-but-valid stream.

**Architecture:** An expand-contract migration. `StreamLabel<'a>(&'a
str)` and a new `Seed::derive_typed` method are added *alongside* the
existing `Seed::derive(&str)` (Task 1, purely additive, nothing else
changes yet). Every crate then migrates its own call sites from
`.derive("literal")` to `.derive_typed(streams::CONST)` /
`.derive_typed(StreamLabel::dynamic(x))`, one crate (or file-group) per
task, each independently compiling and independently provable
byte-identical. The final task deletes the old `derive(&str)` and renames
`derive_typed` back to `derive` — a single mechanical, low-risk rename
now that nothing calls the old signature.

**Tech Stack:** Rust, `hornvale-kernel` + every domain/window crate that
draws streams, `tools/type-audit` (a standalone binary outside the
workspace, decision 0027/0028).

## Global Constraints

- `StreamLabel<'a>(&'a str)` is zero-cost and borrowing — never `String`
  or `Cow`. `from_static(s: &'static str) -> StreamLabel<'static>` is a
  `const fn`. `as_str(&self) -> &'a str` is `pub`.
- `stream_labels()`'s own signature (`Vec<(&'static str, &'static str)>`)
  does **not** change anywhere — each crate's implementation calls
  `.as_str()` on its own `StreamLabel` constants when building that Vec.
- No epoch bump, anywhere. Every migration replaces *where* a string is
  referenced from, never the string's own value.
- Type-audit tags on any new `pub` primitive use the exact grammar
  `bare-ok(<class>: <field-name>)` — a field name used as its own class
  (e.g. `bare-ok(label)` instead of `bare-ok(identifier-text: label)`) is
  a malformed tag that recurred in two prior campaigns' own plan text
  this session. State it here once; every task below copies it verbatim.
- Every migration task's test story is the same: run that crate's
  existing determinism/property tests **unchanged**, before your edit and
  after. A pass proves byte-identity (spec "Determinism and save-format"
  — no string VALUE changed, only where it's referenced from); a failure
  means a copy-paste error in a new constant's value, not a logic bug —
  stop and compare the literal you copied against the original grep
  output in this plan, character for character.
- `cargo fmt` is the final step of every task before committing.

---

### Task 1: `StreamLabel` and `Seed::derive_typed` (purely additive)

**Files:**
- Modify: `kernel/src/seed.rs`

**Interfaces:**
- Produces: `pub struct StreamLabel<'a>(&'a str)`; `impl StreamLabel<'static>
  { pub const fn from_static(s: &'static str) -> Self }`; `impl<'a>
  StreamLabel<'a> { pub fn dynamic(s: &'a str) -> Self; pub fn as_str(&self)
  -> &'a str }`; `impl Seed { pub fn derive_typed(&self, label:
  StreamLabel<'_>) -> Seed }`. The existing `pub fn derive(&self, label:
  &str) -> Seed` is untouched — every later task's own migration calls
  `derive_typed`, not `derive`, until the final task renames it.

- [ ] **Step 1: Write the failing tests**

Add to `kernel/src/seed.rs`'s existing `#[cfg(test)] mod tests` block:

```rust
    #[test]
    fn derive_typed_matches_derive_for_the_same_bytes() {
        let seed = Seed(42);
        let via_str = seed.derive("astronomy");
        let via_typed = seed.derive_typed(StreamLabel::from_static("astronomy"));
        assert_eq!(via_str, via_typed);
    }

    #[test]
    fn derive_typed_dynamic_matches_derive_for_the_same_bytes() {
        let seed = Seed(7);
        let label = String::from("goblin");
        let via_str = seed.derive(&label);
        let via_typed = seed.derive_typed(StreamLabel::dynamic(&label));
        assert_eq!(via_str, via_typed);
    }

    #[test]
    fn stream_label_as_str_round_trips() {
        let label = StreamLabel::from_static("terrain");
        assert_eq!(label.as_str(), "terrain");
    }
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-kernel --lib seed:: -- --nocapture 2>&1 | tail -20`
Expected: FAIL — `derive_typed`, `StreamLabel` not found.

- [ ] **Step 3: Implement**

Insert immediately after the closing `}` of `impl Seed` (after the
existing `pub fn stream(&self) -> Stream { ... }` method, still inside
the `impl Seed` block — add this as one more method in that same block):

```rust
    /// Derive a child seed for a typed `label` — the type-safe successor
    /// to [`Seed::derive`] (temporary name during the PROC-17 migration;
    /// the final task in that plan renames this to `derive` once every
    /// call site in the workspace has migrated and the old `&str`
    /// signature is deleted). Byte-identical to `derive(label.as_str())`
    /// — this wraps the exact same hash computation, never a new one.
    pub fn derive_typed(&self, label: StreamLabel<'_>) -> Seed {
        self.derive(label.as_str())
    }
```

Insert this new type immediately before `impl Seed {` (after the
`Seed` struct definition and its doc comment, before the `const
FNV_OFFSET`/`FNV_PRIME`/`splitmix64` block — place it right after `pub
struct Seed(pub u64);` so the type and the struct it services sit
together):

```rust
/// A seed-derivation leg — the only way to call [`Seed::derive_typed`].
/// Wraps a string so a bare `&str`, and therefore a typo'd inline
/// literal, cannot silently compile where a leg was expected. Zero-cost:
/// borrows, never allocates.
/// type-audit: bare-ok(identifier-text: return)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct StreamLabel<'a>(&'a str);

impl StreamLabel<'static> {
    /// A permanent, save-format-contract leg. Called ONLY inside a
    /// crate's own `streams.rs` — never at an ad hoc use site (a
    /// `tools/type-audit` check enforces this — see PROC-17's own plan,
    /// Task 11).
    pub const fn from_static(s: &'static str) -> Self {
        StreamLabel(s)
    }
}

impl<'a> StreamLabel<'a> {
    /// A runtime-computed leg — a species name, a settlement's cell id, a
    /// salt. Legitimately dynamic, never centralizable.
    /// type-audit: bare-ok(identifier-text: s)
    pub fn dynamic(s: &'a str) -> Self {
        StreamLabel(s)
    }

    /// The wrapped string. `pub` because `stream_labels()` in every OTHER
    /// crate needs it to build the `Vec<(&'static str, &'static str)>`
    /// the manifest reads — that return type does not change; only how
    /// each crate builds it does.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn as_str(&self) -> &'a str {
        self.0
    }
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-kernel --lib seed:: -- --nocapture 2>&1 | tail -20`
Expected: PASS — all three new tests, plus every pre-existing test in
this module (unchanged).

- [ ] **Step 5: `cargo fmt`, clippy, type-audit**

Run: `cargo fmt -p hornvale-kernel`
Run: `cargo clippy -p hornvale-kernel --all-targets -- -D warnings`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`

- [ ] **Step 6: Confirm the whole workspace still builds**

Run: `cargo build --workspace 2>&1 | tail -20`
Expected: builds clean — this task is purely additive, so nothing outside
`kernel/src/seed.rs` should even notice.

- [ ] **Step 7: Commit**

```bash
git add kernel/src/seed.rs docs/audits/type-audit-report.md
git commit -m "feat(kernel): StreamLabel + derive_typed, additive -- proc-17 T1"
```

---

### Task 2: migrate `domains/astronomy` (test-only, 14 sites)

**Files:**
- Modify: `domains/astronomy/src/starfield.rs` (5 sites: lines 62, 70, 77,
  99, 133)
- Modify: `domains/astronomy/src/figures.rs` (4 sites: lines 293, 303,
  319, 376)
- Modify: `domains/astronomy/src/facts.rs` (3 sites: lines 1146, 1173,
  1224 — inside `#[cfg(test)]` blocks)
- Modify: `domains/astronomy/src/neighborhood.rs` (1 site: line 162)
- Modify: `domains/astronomy/tests/genesis_properties.rs` (1 site: line
  495)

**Interfaces:**
- Consumes: `hornvale_kernel::StreamLabel`, `Seed::derive_typed` (Task 1).
- Every site in this task uses the identical literal `"astronomy"`, which
  already has a declared constant: `domains/astronomy/src/streams.rs`'s
  `pub const ROOT: &str = "astronomy";` (existing, line 5). This task
  ALSO changes `ROOT`'s own type from `&str` to `StreamLabel<'static>`.

- [ ] **Step 1: Change `ROOT`'s type in `domains/astronomy/src/streams.rs`**

Add `use hornvale_kernel::StreamLabel;` to the top of the file (after the
existing module doc comment). Change:

```rust
/// Root stream label for astronomy.
/// type-audit: bare-ok(identifier-text)
pub const ROOT: &str = "astronomy";
```

to:

```rust
/// Root stream label for astronomy.
/// type-audit: bare-ok(identifier-text: return)
pub const ROOT: StreamLabel<'static> = StreamLabel::from_static("astronomy");
```

Every OTHER constant in this file (`STAR_MASS`, `ANCHOR_MASS`, etc.) stays
untouched in this task — only `ROOT` is used by the 14 sites this task
migrates. (A later, separate concern: astronomy's own PRODUCTION code
already references those other constants correctly via `.derive(...)`,
not `.derive_typed(...)` yet — Step 3 below handles re-pointing every
production caller of `ROOT` and every other astronomy constant to
`derive_typed` in the SAME pass, since `ROOT`'s type change alone would
otherwise break astronomy's own production code that calls
`.derive(ROOT)` expecting a `&str`.)

- [ ] **Step 2: Find astronomy's own production callers of `streams::ROOT` and the other constants**

Run: `grep -rn 'streams::[A-Z_]*\b' domains/astronomy/src/*.rs windows/worldgen/src/lib.rs | grep -v '^\s*//' | grep '\.derive('`

This finds every place ANY astronomy stream constant is passed to
`.derive(...)` today (production code, already using the constant
correctly) — every one of these must change from `.derive(streams::X)` to
`.derive_typed(streams::X)` in this same task, or the crate won't
compile once `ROOT`'s type changes. Record the exact file:line list this
prints; you will need it for Step 3.

- [ ] **Step 3: Migrate every site**

For each of the 14 test sites listed in this task's **Files** section,
replace `.derive("astronomy")` with `.derive_typed(streams::ROOT)`.
`starfield.rs` and `neighborhood.rs` already have `use crate::streams;`
at their top — no new import needed. `figures.rs` and `facts.rs` do not;
add `use crate::streams;` near their existing `use` block.
`tests/genesis_properties.rs` is outside the crate (in `tests/`), so it
needs the crate's PUBLIC path: `hornvale_astronomy::streams::ROOT`,
wrapped as `.derive_typed(hornvale_astronomy::streams::ROOT)` (check
whether this test file already has a `use hornvale_astronomy::...` you
can extend, or use the fully-qualified path inline).

For every production call site Step 2's grep found (in
`domains/astronomy/src/*.rs` and, if any appear, `windows/worldgen/src/
lib.rs`'s `ASTRONOMY_STREAM_ROOT`-aliased import), change `.derive(` to
`.derive_typed(` at that exact call — the constant reference itself
(`streams::ROOT`, `ASTRONOMY_STREAM_ROOT`, or any other astronomy
`streams::` constant name) does not change, only the method name does.

- [ ] **Step 4: Run astronomy's full test suite**

Run: `cargo test -p hornvale-astronomy 2>&1 | tail -60`
Expected: PASS — every test, unchanged count, unchanged results (proves
byte-identity per this plan's Global Constraints test story).

- [ ] **Step 5: Confirm the workspace still builds**

Run: `cargo build --workspace 2>&1 | tail -30`
Expected: clean. If `windows/worldgen` fails to build because of
`ASTRONOMY_STREAM_ROOT`'s changed type, that means Step 3's production-
caller sweep missed a site — go back and fix it in THIS task (worldgen's
own migration is a later task, but this specific call must move in step
with `ROOT`'s type change since nothing else touches it before then).

- [ ] **Step 6: `cargo fmt`, clippy, type-audit**

Run: `cargo fmt -p hornvale-astronomy`
Run: `cargo clippy -p hornvale-astronomy --all-targets -- -D warnings`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`

- [ ] **Step 7: Commit**

```bash
git add domains/astronomy/src/streams.rs domains/astronomy/src/starfield.rs domains/astronomy/src/figures.rs domains/astronomy/src/facts.rs domains/astronomy/src/neighborhood.rs domains/astronomy/tests/genesis_properties.rs docs/audits/type-audit-report.md
# plus any file Step 3's production sweep touched, e.g. windows/worldgen/src/lib.rs
git commit -m "refactor(astronomy): migrate to StreamLabel/derive_typed -- proc-17 T2"
```

---

### Task 3: migrate `domains/terrain` (9 production + 2 test sites; 5 new constants)

**Files:**
- Modify: `domains/terrain/src/streams.rs` (add 5 constants)
- Modify: `domains/terrain/src/crust.rs` (7 sites: lines 100, 101, 102,
  294, 986 production; 1195, 1224 test)
- Modify: `domains/terrain/src/render.rs` (3 sites: lines 53, 54, 55)
- Modify: `domains/terrain/src/rift.rs` (1 site: line 180)

**Interfaces:**
- Consumes: `StreamLabel`, `Seed::derive_typed` (Task 1).
- Produces: `streams::LOBING`, `streams::CRUST_SLICE_0/1/2`,
  `streams::RIFT_CRENULATION` — no other task consumes these.

- [ ] **Step 1: Change every existing terrain constant's type, and add 5 new ones**

In `domains/terrain/src/streams.rs`, add `use hornvale_kernel::StreamLabel;`
after the module doc comment. Change EVERY existing `pub const NAME: &str
= "value";` in this file (17 of them: `ROOT`, `PLATE_COUNT`,
`PLATE_SEEDS`, `PLATE_MOTION`, `MATURITY`, `HOTSPOTS`, `OCEAN_FRACTION`,
`COAST_RENDER`, `CRATONS`, `PLATE_WEIGHTS`, `PLATE_EDGE`, `LITHOLOGY`,
`TERRANES`, `MICROCONTINENTS`, `ARC_GATE`, `RELIEF`, `RIFT`) to `pub const
NAME: StreamLabel<'static> = StreamLabel::from_static("value");`,
preserving each one's own doc comment and updating its `type-audit:
bare-ok(identifier-text)` tag to `type-audit: bare-ok(identifier-text:
return)`. Then append 5 new constants:

```rust
/// The lobing-noise sub-leg under a craton's own stream (spec: two
/// production call sites, `crust.rs:294` and `crust.rs:986`, share this
/// exact literal today).
/// type-audit: bare-ok(identifier-text: return)
pub const LOBING: StreamLabel<'static> = StreamLabel::from_static("lobing");
/// The first of three orthogonal noise slices shared between crust
/// sculpting and its render lens (`crust.rs` and `render.rs`).
/// type-audit: bare-ok(identifier-text: return)
pub const CRUST_SLICE_0: StreamLabel<'static> = StreamLabel::from_static("slice-0");
/// The second of three orthogonal noise slices (see [`CRUST_SLICE_0`]).
/// type-audit: bare-ok(identifier-text: return)
pub const CRUST_SLICE_1: StreamLabel<'static> = StreamLabel::from_static("slice-1");
/// The third of three orthogonal noise slices (see [`CRUST_SLICE_0`]).
/// type-audit: bare-ok(identifier-text: return)
pub const CRUST_SLICE_2: StreamLabel<'static> = StreamLabel::from_static("slice-2");
/// The rift's crenulation-noise sub-leg.
/// type-audit: bare-ok(identifier-text: return)
pub const RIFT_CRENULATION: StreamLabel<'static> = StreamLabel::from_static("crenulation");
```

- [ ] **Step 2: Find terrain's own production callers of the 17 pre-existing constants**

Run: `grep -rn 'streams::[A-Z_]*\b' domains/terrain/src/*.rs windows/worldgen/src/*.rs | grep '\.derive('`

Every result must change `.derive(` to `.derive_typed(` in this task
(same reasoning as Task 2 Step 2/5 — a constant's type change breaks
every existing caller until it migrates too).

- [ ] **Step 3: Migrate the 5 new-constant sites**

In `domains/terrain/src/crust.rs`: lines 100/101/102 —
`seed.derive("slice-0")` / `"slice-1"` / `"slice-2"` become
`seed.derive_typed(streams::CRUST_SLICE_0)` / `streams::CRUST_SLICE_1` /
`streams::CRUST_SLICE_2`. Lines 294 and 986 —
`terrain_seed.derive(streams::CRATONS).derive("lobing")` becomes
`terrain_seed.derive_typed(streams::CRATONS).derive_typed(streams::LOBING)`.
Lines 1195 and 1224 (inside `mod tests`, starting at line 1150) —
`Seed(42).derive("test-craton")` / `Seed(seed_val).derive("test-craton")`
become `Seed(42).derive_typed(StreamLabel::dynamic("test-craton"))` /
`Seed(seed_val).derive_typed(StreamLabel::dynamic("test-craton"))` — this
one stays a literal wrapped in `dynamic`, not centralized, since it is a
one-off test label with no production meaning (per the spec's own rule
for exactly this case).

In `domains/terrain/src/render.rs`: lines 53/54/55 —
`noise_seed.derive("slice-0")` etc. become
`noise_seed.derive_typed(streams::CRUST_SLICE_0)` etc. (the SAME three
constants `crust.rs` uses — this is the whole point of centralizing them).

In `domains/terrain/src/rift.rs`: line 180 —
`noise_seed.derive("crenulation")` becomes
`noise_seed.derive_typed(streams::RIFT_CRENULATION)`.

All three files already have `use crate::streams;` — no new imports
needed. `StreamLabel` needs importing in `crust.rs` for the
`StreamLabel::dynamic(...)` call at lines 1195/1224 — add `use
hornvale_kernel::StreamLabel;` if not already present.

- [ ] **Step 4: Run terrain's full test suite**

Run: `cargo test -p hornvale-terrain 2>&1 | tail -60`
Expected: PASS — unchanged count, unchanged results.

- [ ] **Step 5: Confirm the workspace still builds**

Run: `cargo build --workspace 2>&1 | tail -30`
Expected: clean (same reasoning as Task 2 Step 5 — fix any missed
production caller in THIS task if `windows/worldgen` or another
downstream crate fails).

- [ ] **Step 6: `cargo fmt`, clippy, type-audit**

Run: `cargo fmt -p hornvale-terrain`
Run: `cargo clippy -p hornvale-terrain --all-targets -- -D warnings`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`

- [ ] **Step 7: Commit**

```bash
git add domains/terrain/src/streams.rs domains/terrain/src/crust.rs domains/terrain/src/render.rs domains/terrain/src/rift.rs docs/audits/type-audit-report.md
git commit -m "refactor(terrain): migrate to StreamLabel/derive_typed, mint 5 new leg constants -- proc-17 T3"
```

---

### Task 4: author `domains/language/src/streams.rs`; migrate phonology.rs, grammar.rs, numeracy.rs, lexicon.rs

**Files:**
- Create: `domains/language/src/streams.rs`
- Modify: `domains/language/src/lib.rs` (add `pub mod streams;`)
- Modify: `domains/language/src/phonology.rs` (5 sites: lines 148, 524,
  536, 554 — plus the doc comment at line 519, updated for accuracy but
  not functionally required)
- Modify: `domains/language/src/grammar.rs` (9 sites: lines 121-124,
  129-132, 142-145)
- Modify: `domains/language/src/numeracy.rs` (4 sites: lines 57-60)
- Modify: `domains/language/src/lexicon.rs` (4 sites: lines 89-92 — plus
  the doc comment at line 85)

**Interfaces:**
- Consumes: `StreamLabel`, `Seed::derive_typed` (Task 1).
- Produces: `domains/language/src/streams.rs`'s full constant set (below)
  — Tasks 5, 6, and 7 (windows/worldgen) all consume `streams::ROOT` from
  this file; `hornvale_language::streams::ROOT` is the path an external
  crate (worldgen) uses.

- [ ] **Step 1: Create `domains/language/src/streams.rs`**

```rust
//! Seed-derivation labels for the language domain (save-format contract
//! — a rename silently corrupts every world; deliberate regeneration
//! uses an epoch suffix, e.g. `.../v2`). Authored by PROC-17 — this
//! crate previously had no centralized leg constants at all; every one
//! of these already existed as an inline literal, repeated identically
//! at every call site, before this file existed.

use hornvale_kernel::StreamLabel;

/// Root label for the language domain.
/// type-audit: bare-ok(identifier-text: return)
pub const ROOT: StreamLabel<'static> = StreamLabel::from_static("language");
/// The phonology sub-tree.
/// type-audit: bare-ok(identifier-text: return)
pub const PHONOLOGY: StreamLabel<'static> = StreamLabel::from_static("phonology");
/// The tone-inventory draw, under phonology.
/// type-audit: bare-ok(identifier-text: return)
pub const TONES: StreamLabel<'static> = StreamLabel::from_static("tones");
/// The phoneme-inventory draw, under phonology.
/// type-audit: bare-ok(identifier-text: return)
pub const INVENTORY: StreamLabel<'static> = StreamLabel::from_static("inventory");
/// The syllable-phonotactics draw, under phonology.
/// type-audit: bare-ok(identifier-text: return)
pub const PHONOTACTICS: StreamLabel<'static> = StreamLabel::from_static("phonotactics");
/// The grammar sub-tree.
/// type-audit: bare-ok(identifier-text: return)
pub const GRAMMAR: StreamLabel<'static> = StreamLabel::from_static("grammar");
/// Constituent-order draw, under grammar.
/// type-audit: bare-ok(identifier-text: return)
pub const CONSTITUENT_ORDER: StreamLabel<'static> = StreamLabel::from_static("constituent-order");
/// Copula-presence/form draw, under grammar.
/// type-audit: bare-ok(identifier-text: return)
pub const COPULA: StreamLabel<'static> = StreamLabel::from_static("copula");
/// Article-presence draw, under grammar.
/// type-audit: bare-ok(identifier-text: return)
pub const ARTICLES: StreamLabel<'static> = StreamLabel::from_static("articles");
/// The numeracy-rung draw, under grammar.
/// type-audit: bare-ok(identifier-text: return)
pub const NUMERACY_RUNG: StreamLabel<'static> = StreamLabel::from_static("numeracy-rung");
/// The name sub-tree (settlement/deity/epithet name generation).
/// type-audit: bare-ok(identifier-text: return)
pub const NAME: StreamLabel<'static> = StreamLabel::from_static("name");
/// The generic epoch-2 suffix leg, appended one level deeper than a v1
/// name draw (settlement/deity/epithet all reuse this exact leg).
/// type-audit: bare-ok(identifier-text: return)
pub const V2: StreamLabel<'static> = StreamLabel::from_static("v2");
/// The lexicon sub-tree.
/// type-audit: bare-ok(identifier-text: return)
pub const LEXICON: StreamLabel<'static> = StreamLabel::from_static("lexicon");
/// Compound-headedness draw, under lexicon.
/// type-audit: bare-ok(identifier-text: return)
pub const HEADEDNESS: StreamLabel<'static> = StreamLabel::from_static("headedness");
/// Sound-change cascade draw, under lexicon.
/// type-audit: bare-ok(identifier-text: return)
pub const CASCADE: StreamLabel<'static> = StreamLabel::from_static("cascade");
/// The proto-root draw leg, under lexicon (named `PROTO_ROOT`, not
/// `ROOT`, to avoid colliding with this file's own crate-root constant —
/// the literal value is `"root"`, distinct from `ROOT`'s `"language"`).
/// type-audit: bare-ok(identifier-text: return)
pub const PROTO_ROOT: StreamLabel<'static> = StreamLabel::from_static("root");
/// The probe sub-stream for open-addressing re-draws during merger-aware
/// proto-root assignment.
/// type-audit: bare-ok(identifier-text: return)
pub const PROBE: StreamLabel<'static> = StreamLabel::from_static("probe");
/// The static leg-name literal `"family"`, used by morphology's own
/// family-level derivation (distinct from a `family` VARIABLE holding a
/// dynamic family identifier elsewhere in this crate — that usage wraps
/// via `StreamLabel::dynamic`, never this constant).
/// type-audit: bare-ok(identifier-text: return)
pub const FAMILY_LEG: StreamLabel<'static> = StreamLabel::from_static("family");
/// The morphology sub-leg, under the family-level derivation.
/// type-audit: bare-ok(identifier-text: return)
pub const MORPH: StreamLabel<'static> = StreamLabel::from_static("morph");
/// Grammaticalization-depth draw, under grammar.
/// type-audit: bare-ok(identifier-text: return)
pub const DEPTH: StreamLabel<'static> = StreamLabel::from_static("depth");
/// Evidentiality depth draw, under grammar/depth.
/// type-audit: bare-ok(identifier-text: return)
pub const EVIDENTIAL: StreamLabel<'static> = StreamLabel::from_static("evidential");
/// Noun-class depth draw, under grammar/depth.
/// type-audit: bare-ok(identifier-text: return)
pub const NOUN_CLASS: StreamLabel<'static> = StreamLabel::from_static("noun-class");
/// Noun-class marker position draw, under grammar.
/// type-audit: bare-ok(identifier-text: return)
pub const CLASS_POSITION: StreamLabel<'static> = StreamLabel::from_static("class-position");
/// Number-marking depth draw, under grammar/depth.
/// type-audit: bare-ok(identifier-text: return)
pub const NUMBER: StreamLabel<'static> = StreamLabel::from_static("number");
/// Tense-marking depth draw, under grammar/depth.
/// type-audit: bare-ok(identifier-text: return)
pub const TENSE: StreamLabel<'static> = StreamLabel::from_static("tense");
/// Number-marker position draw, under grammar.
/// type-audit: bare-ok(identifier-text: return)
pub const NUMBER_POSITION: StreamLabel<'static> = StreamLabel::from_static("number-position");
/// Tense-marker position draw, under grammar.
/// type-audit: bare-ok(identifier-text: return)
pub const TENSE_POSITION: StreamLabel<'static> = StreamLabel::from_static("tense-position");
```

- [ ] **Step 2: Register the module**

In `domains/language/src/lib.rs`, add `pub mod streams;` alongside the
crate's other `pub mod` declarations (the crate already has `pub fn
stream_labels()` in `lib.rs` itself — leave that function's body
untouched; it stays hand-documented prose, per the spec's own explicit
scope decision, PROC-18 is the deferred follow-up that would connect the
two).

- [ ] **Step 3: Migrate `phonology.rs`**

Add `use crate::streams;` near its existing `use` block. Line 148:
`phonology_seed.derive("tones")` → `phonology_seed.derive_typed(streams::TONES)`.
Line 524: `seed.derive("language").derive(species).derive("phonology")` →
`seed.derive_typed(streams::ROOT).derive_typed(StreamLabel::dynamic(species)).derive_typed(streams::PHONOLOGY)`
(add `use hornvale_kernel::StreamLabel;` if not already present — `species`
here is a runtime `&str` parameter, a dynamic leg). Line 536:
`phonology_seed.derive("inventory")` → `phonology_seed.derive_typed(streams::INVENTORY)`.
Line 554: `phonology_seed.derive("phonotactics")` →
`phonology_seed.derive_typed(streams::PHONOTACTICS)`. The doc comment at
line 519 (`seed.derive("language").derive(species).derive("phonology")`)
is prose only — update it to read `seed.derive_typed(streams::ROOT)
.derive_typed(StreamLabel::dynamic(species)).derive_typed(streams::PHONOLOGY)`
for accuracy, but this is documentation, not a functional requirement.

- [ ] **Step 4: Migrate `grammar.rs`**

Add `use crate::streams;` and `use hornvale_kernel::StreamLabel;`. Three
identical 4-leg chains (lines 121-124, 129-132, 142-145), each starting
`.derive("language").derive(species).derive("grammar")` then diverging on
the 4th leg (`"constituent-order"` / `"copula"` / `"articles"`). Replace
`.derive("language")` → `.derive_typed(streams::ROOT)`, `.derive(species)`
→ `.derive_typed(StreamLabel::dynamic(species))`, `.derive("grammar")` →
`.derive_typed(streams::GRAMMAR)`, and the 4th leg similarly against
`streams::CONSTITUENT_ORDER` / `streams::COPULA` / `streams::ARTICLES`
respectively.

- [ ] **Step 5: Migrate `numeracy.rs`**

Add `use crate::streams;` and `use hornvale_kernel::StreamLabel;`. Lines
57-60: `.derive("language").derive(species).derive("grammar")
.derive("numeracy-rung")` → `.derive_typed(streams::ROOT)
.derive_typed(StreamLabel::dynamic(species)).derive_typed(streams::GRAMMAR)
.derive_typed(streams::NUMERACY_RUNG)`.

- [ ] **Step 6: Migrate `lexicon.rs`**

Add `use crate::streams;` and `use hornvale_kernel::StreamLabel;`. Lines
89-92: `.derive("language").derive(species).derive("lexicon")
.derive("headedness")` → `.derive_typed(streams::ROOT)
.derive_typed(StreamLabel::dynamic(species)).derive_typed(streams::LEXICON)
.derive_typed(streams::HEADEDNESS)`. The doc comment at line 85 is prose
— update for accuracy, not functionally required.

- [ ] **Step 7: Run language's full test suite**

Run: `cargo test -p hornvale-language 2>&1 | tail -80`
Expected: PASS — unchanged count, unchanged results. If a test fails,
compare the exact literal you copied into `streams.rs` against this
task's own Step 1 code block, character for character — a mismatched
value (not this task's own logic) is the only way this drifts.

- [ ] **Step 8: `cargo fmt`, clippy, type-audit**

Run: `cargo fmt -p hornvale-language`
Run: `cargo clippy -p hornvale-language --all-targets -- -D warnings`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`

- [ ] **Step 9: Commit**

```bash
git add domains/language/src/streams.rs domains/language/src/lib.rs domains/language/src/phonology.rs domains/language/src/grammar.rs domains/language/src/numeracy.rs domains/language/src/lexicon.rs docs/audits/type-audit-report.md
git commit -m "feat(language): author streams.rs; migrate phonology/grammar/numeracy/lexicon -- proc-17 T4"
```

---

### Task 5: migrate `naming.rs`, `etymology.rs`

**Files:**
- Modify: `domains/language/src/naming.rs` (10 sites: lines 177-181,
  223-228)
- Modify: `domains/language/src/etymology.rs` (16 sites: lines 162-165,
  182-186, 336-341, 345)

**Interfaces:**
- Consumes: `domains/language/src/streams.rs`'s full constant set (Task
  4) — `ROOT`, `NAME`, `V2`, `LEXICON`, `CASCADE`, `PROTO_ROOT`, `PROBE`.

- [ ] **Step 1: Migrate `naming.rs`**

Add `use crate::streams;` and `use hornvale_kernel::StreamLabel;`. Lines
177-181: `.derive("language").derive(&self.species).derive("name")
.derive(kind.label()).derive(&salt.to_string())` →
`.derive_typed(streams::ROOT).derive_typed(StreamLabel::dynamic(&self.species))
.derive_typed(streams::NAME).derive_typed(StreamLabel::dynamic(kind.label()))
.derive_typed(StreamLabel::dynamic(&salt.to_string()))` (`kind.label()` and
`&salt.to_string()` are both runtime values — dynamic legs). Lines
223-228: the same chain with an inserted `"v2"` leg after `kind.label()`:
`.derive("language").derive(&self.species).derive("name")
.derive(kind.label()).derive("v2").derive(&salt.to_string())` →
`.derive_typed(streams::ROOT).derive_typed(StreamLabel::dynamic(&self.species))
.derive_typed(streams::NAME).derive_typed(StreamLabel::dynamic(kind.label()))
.derive_typed(streams::V2).derive_typed(StreamLabel::dynamic(&salt.to_string()))`.
The two doc comments (lines 17, 29) are prose — update for accuracy, not
functionally required.

- [ ] **Step 2: Migrate `etymology.rs`**

Add `use crate::streams;` and `use hornvale_kernel::StreamLabel;`. Lines
162-165: `.derive("language").derive(species).derive("lexicon")
.derive("cascade")` → `.derive_typed(streams::ROOT)
.derive_typed(StreamLabel::dynamic(species)).derive_typed(streams::LEXICON)
.derive_typed(streams::CASCADE)`. Lines 182-186: `.derive("language")
.derive(species).derive("lexicon").derive("root").derive(concept)` →
`.derive_typed(streams::ROOT).derive_typed(StreamLabel::dynamic(species))
.derive_typed(streams::LEXICON).derive_typed(streams::PROTO_ROOT)
.derive_typed(StreamLabel::dynamic(concept))` (`concept` is a runtime
value — dynamic). Lines 336-341: `.derive("language").derive(family)
.derive("lexicon").derive("root").derive(ROOT_EPOCH).derive(concept)` →
`.derive_typed(streams::ROOT).derive_typed(StreamLabel::dynamic(family))
.derive_typed(streams::LEXICON).derive_typed(streams::PROTO_ROOT)
.derive_typed(StreamLabel::dynamic(ROOT_EPOCH)).derive_typed(StreamLabel::dynamic(concept))`
(`family` and `ROOT_EPOCH` — check whether `ROOT_EPOCH` is a `&str`
constant already declared in this file; if so it's still a dynamic leg
from `StreamLabel`'s perspective since it isn't `streams::`-declared —
wrap it the same way as any other pre-existing local `&str` value). Line
345: `base.derive("probe").derive(&probe.to_string())` →
`base.derive_typed(streams::PROBE).derive_typed(StreamLabel::dynamic(&probe.to_string()))`.
The two doc comments (lines 158, 176) are prose — update for accuracy,
not functionally required.

- [ ] **Step 3: Run language's full test suite**

Run: `cargo test -p hornvale-language 2>&1 | tail -80`
Expected: PASS — unchanged count, unchanged results.

- [ ] **Step 4: `cargo fmt`, clippy, type-audit**

Run: `cargo fmt -p hornvale-language`
Run: `cargo clippy -p hornvale-language --all-targets -- -D warnings`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`

- [ ] **Step 5: Commit**

```bash
git add domains/language/src/naming.rs domains/language/src/etymology.rs docs/audits/type-audit-report.md
git commit -m "refactor(language): migrate naming.rs, etymology.rs to StreamLabel/derive_typed -- proc-17 T5"
```

---

### Task 6: migrate `morphology.rs`, `paradigm.rs`

**Files:**
- Modify: `domains/language/src/morphology.rs` (17 sites: lines 151-155,
  164-168, 177-180, 216-221)
- Modify: `domains/language/src/paradigm.rs` (16 sites: lines 76-80,
  89-93, 102-105, 114-117)

**Interfaces:**
- Consumes: `domains/language/src/streams.rs`'s full constant set (Task
  4) — `ROOT`, `GRAMMAR`, `DEPTH`, `EVIDENTIAL`, `NOUN_CLASS`,
  `CLASS_POSITION`, `FAMILY_LEG`, `MORPH`, `NUMBER`, `TENSE`,
  `NUMBER_POSITION`, `TENSE_POSITION`.

- [ ] **Step 1: Migrate `morphology.rs`**

Add `use crate::streams;` and `use hornvale_kernel::StreamLabel;`. Lines
151-155: `.derive("language").derive(species).derive("grammar")
.derive("depth").derive("evidential")` → `.derive_typed(streams::ROOT)
.derive_typed(StreamLabel::dynamic(species)).derive_typed(streams::GRAMMAR)
.derive_typed(streams::DEPTH).derive_typed(streams::EVIDENTIAL)`. Lines
164-168: same chain, 5th leg `"noun-class"` instead of `"evidential"` →
`streams::NOUN_CLASS`. Lines 177-180: `.derive("language").derive(species)
.derive("grammar").derive("class-position")` → `.derive_typed(streams::ROOT)
.derive_typed(StreamLabel::dynamic(species)).derive_typed(streams::GRAMMAR)
.derive_typed(streams::CLASS_POSITION)`. Lines 216-221:
`.derive("language").derive("family").derive(family).derive("morph")
.derive(axis).derive(value)` → `.derive_typed(streams::ROOT)
.derive_typed(streams::FAMILY_LEG).derive_typed(StreamLabel::dynamic(family))
.derive_typed(streams::MORPH).derive_typed(StreamLabel::dynamic(axis))
.derive_typed(StreamLabel::dynamic(value))` — **note the two different
uses of "family" here**: `.derive("family")` (the static leg-name
literal) becomes `streams::FAMILY_LEG`; `.derive(family)` (the variable,
one leg later) becomes `StreamLabel::dynamic(family)` — do not conflate
these, they are genuinely different legs at different depths. The doc
comment at line 199 is prose — update for accuracy, not functionally
required.

- [ ] **Step 2: Migrate `paradigm.rs`**

Add `use crate::streams;` and `use hornvale_kernel::StreamLabel;`. Lines
76-80: `.derive("language").derive(species).derive("grammar")
.derive("depth").derive("number")` → `.derive_typed(streams::ROOT)
.derive_typed(StreamLabel::dynamic(species)).derive_typed(streams::GRAMMAR)
.derive_typed(streams::DEPTH).derive_typed(streams::NUMBER)`. Lines 89-93:
same chain, 5th leg `"tense"` → `streams::TENSE`. Lines 102-105:
`.derive("language").derive(species).derive("grammar")
.derive("number-position")` → `.derive_typed(streams::ROOT)
.derive_typed(StreamLabel::dynamic(species)).derive_typed(streams::GRAMMAR)
.derive_typed(streams::NUMBER_POSITION)`. Lines 114-117: same chain, 4th
leg `"tense-position"` → `streams::TENSE_POSITION`.

- [ ] **Step 3: Run language's full test suite**

Run: `cargo test -p hornvale-language 2>&1 | tail -80`
Expected: PASS — unchanged count, unchanged results.

- [ ] **Step 4: Confirm the whole workspace builds**

Run: `cargo build --workspace 2>&1 | tail -30`
Expected: clean — `domains/language` no longer has any raw-literal
`.derive("...")` production call site left. Verify:
`grep -rn '\.derive("' domains/language/src/*.rs | grep -v 'mod tests'`
Expected: no output (empty).

- [ ] **Step 5: `cargo fmt`, clippy, type-audit**

Run: `cargo fmt -p hornvale-language`
Run: `cargo clippy -p hornvale-language --all-targets -- -D warnings`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`

- [ ] **Step 6: Commit**

```bash
git add domains/language/src/morphology.rs domains/language/src/paradigm.rs docs/audits/type-audit-report.md
git commit -m "refactor(language): migrate morphology.rs, paradigm.rs to StreamLabel/derive_typed -- proc-17 T6"
```

---

### Task 7: author `windows/worldgen/src/streams.rs`; migrate `chorus.rs`, `lib.rs`

**Files:**
- Create: `windows/worldgen/src/streams.rs`
- Modify: `windows/worldgen/src/lib.rs` (add `pub mod streams;`; migrate
  5 sites: lines 3068-3069, 3080, 4561, 4984)
- Modify: `windows/worldgen/src/chorus.rs` (46 sites across lines
  413-417, 470-473, 543-547, 557-560, 625-630, 640-643, 920-924, 934-937,
  1007-1011, 1021-1024)

**Interfaces:**
- Consumes: `domains/language/src/streams.rs::ROOT` (Task 4, via
  `hornvale_language::streams::ROOT` — `chorus.rs` derives its own
  schema/lexeme streams UNDER the language domain's own root, reusing the
  SAME constant rather than declaring a second one with the same value).
- Produces: `windows/worldgen/src/streams.rs`'s own constant set (below).

- [ ] **Step 1: Create `windows/worldgen/src/streams.rs`**

```rust
//! Seed-derivation labels owned by `windows/worldgen` itself (the
//! chorus/schema-selection and religion-naming streams — composition-
//! root concerns, not any one domain's). Save-format contracts; a
//! rename silently corrupts every world.

use hornvale_kernel::StreamLabel;

/// The folk causal-schema-selection sub-leg, under a culture's own
/// `hornvale_language::streams::ROOT` derivation.
/// type-audit: bare-ok(identifier-text: return)
pub const SCHEMA: StreamLabel<'static> = StreamLabel::from_static("schema");
/// The sky-domain fact-shape sub-leg, under [`SCHEMA`].
/// type-audit: bare-ok(identifier-text: return)
pub const SKY: StreamLabel<'static> = StreamLabel::from_static("sky");
/// The lexicalization sub-leg for a chosen schema's rendered sentence.
/// type-audit: bare-ok(identifier-text: return)
pub const LEXEME: StreamLabel<'static> = StreamLabel::from_static("lexeme");
/// The doctrine (institutional) causal-schema-selection sub-leg — the
/// doctrine-voice twin of [`SCHEMA`].
/// type-audit: bare-ok(identifier-text: return)
pub const DOCTRINE_SCHEMA: StreamLabel<'static> = StreamLabel::from_static("doctrine-schema");
/// The doctrine-voice twin of [`LEXEME`].
/// type-audit: bare-ok(identifier-text: return)
pub const DOCTRINE_LEXEME: StreamLabel<'static> = StreamLabel::from_static("doctrine-lexeme");
/// The deity-naming stream, epoch v2 (a full flat path, not a composed
/// leg chain — matches `domains/climate`'s own `WEATHER_PHASE` pattern).
/// type-audit: bare-ok(identifier-text: return)
pub const RELIGION_DEITY_V2: StreamLabel<'static> = StreamLabel::from_static("religion/deity/v2");
```

- [ ] **Step 2: Register the module and re-check the existing astronomy re-export**

In `windows/worldgen/src/lib.rs`, add `pub mod streams;` alongside its
other `pub mod` declarations. Near line 12, the existing import
`streams::ROOT as ASTRONOMY_STREAM_ROOT` (aliased from
`hornvale_astronomy::streams`) needs its OWN `use` path checked — confirm
it still resolves (it should; Task 2 changed `hornvale_astronomy::
streams::ROOT`'s TYPE, not its path or name) and update ONLY the call
site that consumes it (Step 4 below), not this import line itself.

- [ ] **Step 3: Migrate `lib.rs`'s 5 sites**

Add `use crate::streams;` and `use hornvale_kernel::StreamLabel;` near
`lib.rs`'s existing `use` block (`lib.rs` is large; add these near the
top-level imports, not inside any specific function). Lines 3068-3069:
`base.derive(kind).derive(&rank.to_string())` →
`base.derive_typed(StreamLabel::dynamic(kind)).derive_typed(StreamLabel::dynamic(&rank.to_string()))`
(both `kind` and `&rank.to_string()` are runtime values). Line 3080:
`world_seed.derive("religion/deity/v2").derive(species)` →
`world_seed.derive_typed(streams::RELIGION_DEITY_V2).derive_typed(StreamLabel::dynamic(species))`.

- [ ] **Step 4: Migrate the astronomy-root call site**

Line 4561: `world.seed.derive(ASTRONOMY_STREAM_ROOT)` →
`world.seed.derive_typed(ASTRONOMY_STREAM_ROOT)` — the imported constant
itself needs no change (it's already `hornvale_astronomy::streams::ROOT`,
already `StreamLabel<'static>` typed since Task 2), only the method name
changes.

- [ ] **Step 5: Migrate the test-only site**

Line 4984 (confirm this sits inside a `#[cfg(test)]` block before
treating it as test-only — if not, treat it as production instead,
following the same rule as every other site in this task):
`Seed(42).derive("religion/deity/v2").derive("goblin")` →
`Seed(42).derive_typed(streams::RELIGION_DEITY_V2).derive_typed(StreamLabel::dynamic("goblin"))`
— `"goblin"` here is a hardcoded test value standing in for a dynamic
species leg, so it wraps via `dynamic`, not a new constant.

- [ ] **Step 6: Migrate `chorus.rs`'s 10 chains**

Add `use crate::streams;` and `use hornvale_kernel::StreamLabel;` near
`chorus.rs`'s existing `use` block. Every chain in this file starts
`.derive("language")` — this becomes
`.derive_typed(hornvale_language::streams::ROOT)` (the crate boundary
means the full path is needed, not a local `streams::ROOT` — this file's
own `streams` module, imported in Step 6's own `use crate::streams;`, is
for `SCHEMA`/`SKY`/`LEXEME`/`DOCTRINE_SCHEMA`/`DOCTRINE_LEXEME` only, a
DIFFERENT module than the one supplying `ROOT`). Confirm
`hornvale_language` is already a dependency this crate can reference (it
is — `chorus.rs` already imports language-domain types elsewhere in this
file) and add `use hornvale_language::streams as language_streams;` (or
reference it fully-qualified each time) to avoid a naming collision with
this crate's own `streams` module.

For each of the 10 chains (lines 413-417, 470-473, 543-547, 557-560,
625-630, 640-643, 920-924, 934-937, 1007-1011, 1021-1024), apply this
exact mapping to every leg:
- `.derive("language")` → `.derive_typed(language_streams::ROOT)`
- `.derive(species)` → `.derive_typed(StreamLabel::dynamic(species))`
- `.derive("schema")` → `.derive_typed(streams::SCHEMA)`
- `.derive("sky")` → `.derive_typed(streams::SKY)`
- `.derive(fact_shape_key(...))` → `.derive_typed(StreamLabel::dynamic(fact_shape_key(...)))`
  (keep the exact same inner argument each chain already passes to
  `fact_shape_key`, e.g. `FactShape::CyclicEvent` or `FactShape::Count`
  — only the outer `.derive(...)` wrapping changes)
- `.derive(predicate)` → `.derive_typed(StreamLabel::dynamic(predicate))`
- `.derive("lexeme")` → `.derive_typed(streams::LEXEME)`
- `.derive("doctrine-schema")` → `.derive_typed(streams::DOCTRINE_SCHEMA)`
- `.derive("doctrine-lexeme")` → `.derive_typed(streams::DOCTRINE_LEXEME)`

Every chain in this file is built entirely from these 9 leg shapes — no
chain has a leg outside this list.

- [ ] **Step 7: Run worldgen's full test suite**

Run: `cargo test -p hornvale-worldgen 2>&1 | tail -80`
Expected: PASS — unchanged count, unchanged results.

- [ ] **Step 8: Confirm the whole workspace builds**

Run: `cargo build --workspace 2>&1 | tail -30`

- [ ] **Step 9: `cargo fmt`, clippy, type-audit**

Run: `cargo fmt -p hornvale-worldgen`
Run: `cargo clippy -p hornvale-worldgen --all-targets -- -D warnings`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`

- [ ] **Step 10: Commit**

```bash
git add windows/worldgen/src/streams.rs windows/worldgen/src/lib.rs windows/worldgen/src/chorus.rs docs/audits/type-audit-report.md
git commit -m "feat(worldgen): author streams.rs; migrate chorus.rs, lib.rs -- proc-17 T7"
```

---

### Task 8: author `windows/chronicle/src/streams.rs`; migrate `generate.rs`, `simulate.rs`, `measure.rs`

**Files:**
- Create: `windows/chronicle/src/streams.rs`
- Modify: `windows/chronicle/src/lib.rs` (add `pub mod streams;` and a
  real `pub fn stream_labels()`, mirroring every other crate's own — this
  crate has neither today)
- Modify: `windows/chronicle/src/generate.rs` (4 sites: lines 14, 24, 28,
  61)
- Modify: `windows/chronicle/src/simulate.rs` (2 sites: lines 39, 40)
- Modify: `windows/chronicle/src/measure.rs` (1 site: line 150)

**Interfaces:**
- Produces: `windows/chronicle/src/streams.rs`'s constants, plus
  `windows_chronicle::stream_labels()` — Task 9 (the `cli/src/streams.rs`
  wiring task) consumes this function.

- [ ] **Step 1: Create `windows/chronicle/src/streams.rs`**

```rust
//! Seed-derivation labels for `windows/chronicle` (the living-community
//! macro-history sounding). Save-format contracts; a rename silently
//! corrupts every world. Every entry here is a full flat path (this
//! crate never composes legs — matches `domains/climate`'s own
//! `WEATHER_PHASE` pattern), authored by PROC-17 (this crate had zero
//! centralization before).

use hornvale_kernel::StreamLabel;

/// The species-generation stream.
/// type-audit: bare-ok(identifier-text: return)
pub const SPECIES: StreamLabel<'static> = StreamLabel::from_static("chronicle/species");
/// The carrying-capacity draw.
/// type-audit: bare-ok(identifier-text: return)
pub const CAPACITY: StreamLabel<'static> = StreamLabel::from_static("chronicle/capacity");
/// The community-formation draw.
/// type-audit: bare-ok(identifier-text: return)
pub const COMMUNITIES: StreamLabel<'static> = StreamLabel::from_static("chronicle/communities");
/// The connection-graph draw.
/// type-audit: bare-ok(identifier-text: return)
pub const GRAPH: StreamLabel<'static> = StreamLabel::from_static("chronicle/graph");
/// The event-simulation stream.
/// type-audit: bare-ok(identifier-text: return)
pub const EVENTS: StreamLabel<'static> = StreamLabel::from_static("chronicle/events");
/// The event-delivery/propagation stream.
/// type-audit: bare-ok(identifier-text: return)
pub const DELIVER: StreamLabel<'static> = StreamLabel::from_static("chronicle/deliver");
/// The replay-measurement stream.
/// type-audit: bare-ok(identifier-text: return)
pub const REPLAY: StreamLabel<'static> = StreamLabel::from_static("chronicle/replay");

/// Every seed-derivation label this crate uses, for the generated stream
/// manifest (mirrors every other crate's own `stream_labels()`).
/// type-audit: bare-ok(artifact: return)
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        (SPECIES.as_str(), "the species-generation stream"),
        (CAPACITY.as_str(), "the carrying-capacity draw"),
        (COMMUNITIES.as_str(), "the community-formation draw"),
        (GRAPH.as_str(), "the connection-graph draw"),
        (EVENTS.as_str(), "the event-simulation stream"),
        (DELIVER.as_str(), "the event-delivery/propagation stream"),
        (REPLAY.as_str(), "the replay-measurement stream"),
    ]
}
```

- [ ] **Step 2: Register the module in `lib.rs`**

Add `pub mod streams;` to `windows/chronicle/src/lib.rs` alongside its
other `pub mod` declarations. If `lib.rs` re-exports items the way
`windows/vessel/src/lib.rs` does (`pub use streams::stream_labels;` —
check that file for the exact pattern), add the same `pub use
streams::stream_labels;` here for consistency.

- [ ] **Step 3: Migrate `generate.rs`, `simulate.rs`, `measure.rs`**

Add `use crate::streams;` to each of the three files. In `generate.rs`:
line 14 `config.seed.derive("chronicle/species")` →
`config.seed.derive_typed(streams::SPECIES)`; line 24
`config.seed.derive("chronicle/capacity")` →
`config.seed.derive_typed(streams::CAPACITY)`; line 28
`config.seed.derive("chronicle/communities")` →
`config.seed.derive_typed(streams::COMMUNITIES)`; line 61
`config.seed.derive("chronicle/graph")` →
`config.seed.derive_typed(streams::GRAPH)`. In `simulate.rs`: line 39
`config.seed.derive("chronicle/events")` →
`config.seed.derive_typed(streams::EVENTS)`; line 40
`config.seed.derive("chronicle/deliver")` →
`config.seed.derive_typed(streams::DELIVER)`. In `measure.rs`: line 150
`config.seed.derive("chronicle/replay")` →
`config.seed.derive_typed(streams::REPLAY)`.

- [ ] **Step 4: Run chronicle's full test suite**

Run: `cargo test -p hornvale-chronicle 2>&1 | tail -60`
Expected: PASS — unchanged count, unchanged results.

- [ ] **Step 5: `cargo fmt`, clippy, type-audit**

Run: `cargo fmt -p hornvale-chronicle`
Run: `cargo clippy -p hornvale-chronicle --all-targets -- -D warnings`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`

- [ ] **Step 6: Commit**

```bash
git add windows/chronicle/src/streams.rs windows/chronicle/src/lib.rs windows/chronicle/src/generate.rs windows/chronicle/src/simulate.rs windows/chronicle/src/measure.rs docs/audits/type-audit-report.md
git commit -m "feat(chronicle): author streams.rs + stream_labels(); migrate generate/simulate/measure -- proc-17 T8"
```

---

### Task 9: wire `worldgen` and `chronicle` into the generated manifest

**Files:**
- Modify: `cli/src/streams.rs`

**Interfaces:**
- Consumes: `hornvale_worldgen::streams::*` (Task 7, though this task
  only needs `stream_labels()`-equivalent coverage — see Step 1),
  `hornvale_chronicle::stream_labels()` (Task 8).

- [ ] **Step 1: Confirm `windows/worldgen` needs its own `stream_labels()` too**

`windows/worldgen`'s new `streams.rs` (Task 7) declares constants but was
NOT asked to add a `stream_labels()` function of its own. Add one now,
appended to `windows/worldgen/src/streams.rs`:

```rust
/// Every seed-derivation label this crate itself owns (not any domain
/// crate's), for the generated stream manifest.
/// type-audit: bare-ok(artifact: return)
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        (SCHEMA.as_str(), "the folk causal-schema-selection sub-leg"),
        (SKY.as_str(), "the sky-domain fact-shape sub-leg"),
        (LEXEME.as_str(), "the lexicalization sub-leg for a chosen schema"),
        (DOCTRINE_SCHEMA.as_str(), "the doctrine-voice twin of the folk schema-selection leg"),
        (DOCTRINE_LEXEME.as_str(), "the doctrine-voice twin of the lexeme leg"),
        (RELIGION_DEITY_V2.as_str(), "the deity-naming stream, epoch v2"),
    ]
}
```

- [ ] **Step 2: Update `cli/src/streams.rs`'s `sources` list**

The existing `render_streams()` function builds `sources` from
`hornvale_worldgen::DOMAINS` (the domain roster) plus three explicit
window-crate entries (`hornvale-kernel`, `hornvale-locale`,
`hornvale-vessel`). Add two more explicit entries — `windows/worldgen`
and `windows/chronicle` are windows, not domains, so they belong in this
explicit list, not the `DOMAINS` roster:

```rust
    sources.push(("hornvale-worldgen", hornvale_worldgen::streams::stream_labels()));
    sources.push(("hornvale-chronicle", hornvale_chronicle::stream_labels()));
```

Insert these two lines immediately after the existing
`sources.push(("hornvale-vessel", ...))` line, before the `sources.sort_by(...)`
call (so the new entries participate in the existing alphabetical sort,
not a hand-maintained order).

- [ ] **Step 3: Update the existing manifest test**

`cli/src/streams.rs`'s own `#[cfg(test)] mod tests` has a
`manifest_lists_every_crate_and_label` test asserting specific strings
appear in the rendered doc. Add two more expected strings to its list:

```rust
            "### hornvale-worldgen",
            "### hornvale-chronicle",
```

- [ ] **Step 4: Regenerate the committed manifest**

Run:
```bash
cargo run -p hornvale -- streams > book/src/reference/stream-manifest-generated.md
```

- [ ] **Step 5: Run the CLI's own streams tests**

Run: `cargo test -p hornvale --lib streams:: -- --nocapture 2>&1 | tail -40`
Expected: PASS — all three existing tests (`manifest_lists_every_crate_and_label`,
`manifest_is_deterministic`, `manifest_sections_are_alphabetical_by_crate`),
still asserting the SAME alphabetical-ordering invariant, now over a
longer section list.

- [ ] **Step 6: `cargo fmt`, clippy, type-audit**

Run: `cargo fmt -p hornvale -p hornvale-worldgen`
Run: `cargo clippy -p hornvale -p hornvale-worldgen --all-targets -- -D warnings`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`

- [ ] **Step 7: Commit**

```bash
git add cli/src/streams.rs windows/worldgen/src/streams.rs book/src/reference/stream-manifest-generated.md docs/audits/type-audit-report.md
git commit -m "feat(cli): wire worldgen + chronicle into the generated stream manifest -- proc-17 T9"
```

---

### Task 10: migrate `kernel`'s own remaining literals

**Files:**
- Modify: `kernel/src/noise.rs` (`OCTAVE_LABELS` array + its `format!`
  fallback, line 75/77)
- Modify: `kernel/src/seed.rs` (6 sites in its own `derive()` unit tests:
  lines 113, 118, 123, 128-129, 214)
- Modify: `kernel/examples/first_light.rs` (2 sites: lines 36, 91)
- Modify: `kernel/tests/determinism.rs` (2 sites: lines 37, 46 — 3
  `.derive(` calls total, "terrain" + "settlement" + "name")

**Interfaces:**
- Consumes: `StreamLabel`, `Seed::derive_typed` (Task 1).

- [ ] **Step 1: Migrate `noise.rs`'s `OCTAVE_LABELS`**

Add `use crate::StreamLabel;` near `noise.rs`'s existing imports. Change
`const OCTAVE_LABELS: [&str; 16] = [...]` (the 16 pre-declared
`"octave-0"`..`"octave-15"` strings) to `const OCTAVE_LABELS:
[StreamLabel<'static>; 16] = [StreamLabel::from_static("octave-0"),
StreamLabel::from_static("octave-1"), ...]` — wrap every one of the 16
existing string entries in `StreamLabel::from_static(...)`, preserving
their exact values and order (read the current array literal directly
from the file before editing; do not re-type the 16 values from memory).
Line 75: `seed.derive(OCTAVE_LABELS[octave as usize])` →
`seed.derive_typed(OCTAVE_LABELS[octave as usize])` (the indexing
expression itself doesn't change, only the outer method). Line 77:
`seed.derive(&format!("octave-{octave}"))` →
`seed.derive_typed(StreamLabel::dynamic(&format!("octave-{octave}")))`
(a runtime-formatted string — dynamic, never centralizable, since
`octave` ranges beyond the pre-declared 16).

- [ ] **Step 2: Migrate `seed.rs`'s own unit tests**

These test `derive`/`derive_typed` itself — wrap every literal in
`StreamLabel::dynamic(...)` rather than centralizing (there is no
`streams.rs` for the kernel's own seed-derivation *mechanism* tests to
reference; these strings exist only to prove two derivations differ or
match). Update the THREE new tests Task 1 already added
(`derive_typed_matches_derive_for_the_same_bytes`,
`derive_typed_dynamic_matches_derive_for_the_same_bytes`,
`stream_label_as_str_round_trips`) — leave those untouched, they already
use `StreamLabel` correctly. For the PRE-EXISTING tests: line 113
`Seed(42).derive("astronomy")` (both occurrences on this line) →
`Seed(42).derive_typed(StreamLabel::dynamic("astronomy"))`; line 118
`Seed(42).derive("astronomy")` / `Seed(42).derive("climate")` → wrap both
in `StreamLabel::dynamic(...)`; line 123 same pattern; lines 128-129
`Seed(7).derive("settlement").derive("name")` (twice) → both legs wrapped
in `StreamLabel::dynamic(...)`; line 214 `Seed(42).derive("x")` → wrapped
in `StreamLabel::dynamic(...)`.

- [ ] **Step 3: Migrate `kernel/examples/first_light.rs`**

This is a real example binary (not a test), demonstrating genesis for a
real seed — treat its call sites like production code for care, but
since `kernel` has no domain-specific `streams.rs` of its own for
"terrain"/"settlement"/"name" (those are OTHER crates' concerns; this
example merely demonstrates `kernel`'s own primitives standalone), wrap
both sites in `StreamLabel::dynamic(...)`: line 36
`SEED.derive("terrain")` → `SEED.derive_typed(StreamLabel::dynamic("terrain"))`;
line 91 `SEED.derive("settlement").derive("name").stream()` →
`SEED.derive_typed(StreamLabel::dynamic("settlement")).derive_typed(StreamLabel::dynamic("name")).stream()`.
Add `use hornvale_kernel::StreamLabel;` to this file's imports.

- [ ] **Step 4: Migrate `kernel/tests/determinism.rs`**

Same reasoning as Step 3 — this integration test demonstrates the
kernel's own primitives, not any domain's real genesis path. Add `use
hornvale_kernel::StreamLabel;`. Line 37: `fbm_2d(seed.derive("terrain"),
0.5, 0.5, 3)` → `fbm_2d(seed.derive_typed(StreamLabel::dynamic("terrain")),
0.5, 0.5, 3)`. Line 46: `seed.derive("settlement").derive("name").stream()`
→ `seed.derive_typed(StreamLabel::dynamic("settlement"))
.derive_typed(StreamLabel::dynamic("name")).stream()`.

- [ ] **Step 5: Run kernel's full test suite and the example**

Run: `cargo test -p hornvale-kernel 2>&1 | tail -60`
Expected: PASS — unchanged count, unchanged results.
Run: `cargo run -p hornvale-kernel --example first_light 2>&1 | tail -20`
Expected: runs to completion, same output as before this task (compare
against a `git stash`-saved baseline run if you want to double-check
byte-for-byte, though this is a demonstration binary with no committed
golden output to diff against).

- [ ] **Step 6: `cargo fmt`, clippy, type-audit**

Run: `cargo fmt -p hornvale-kernel`
Run: `cargo clippy -p hornvale-kernel --all-targets --examples -- -D warnings`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`

- [ ] **Step 7: Commit**

```bash
git add kernel/src/noise.rs kernel/src/seed.rs kernel/examples/first_light.rs kernel/tests/determinism.rs docs/audits/type-audit-report.md
git commit -m "refactor(kernel): migrate noise.rs OCTAVE_LABELS + remaining test/example literals -- proc-17 T10"
```

---

## Remediation: Tasks 10b–10e (inserted mid-execution)

Task 10's own review found that this plan's original research — built from a
`grep '\.derive("'` search for INLINE STRING LITERALS — never caught call
sites already passing an *existing, pre-declared* `&str` constant (e.g.
`.derive(streams::STAR_MASS)`). Every crate below has one or more constants
that were declared in a `streams.rs` file long before this campaign, are
real, permanent save-format labels, and are called via `.derive(CONST)` —
structurally invisible to that grep. `Seed::derive(&self, label: &str)`
still exists today (Tasks 1–10 are all additive/consuming, not destructive),
so none of this has broken anything yet — but Task 12 (the contract step)
deletes that method, and every one of these call sites would stop compiling
if Task 12 ran before this remediation lands. `kernel/src/seed.rs`'s own 3
remaining hits (`derive_typed`'s internal delegation at line 87, and its own
two tests at lines 282/291) are NOT a gap — they are Task 1's and Task 12's
own intentional design, already correctly scoped.

Four new tasks close every real remaining gap, in the same style as Tasks
2–10 (retype the constant, migrate its call sites, run that crate's own
test suite unchanged, prove byte-identity).

---

### Task 10b: kernel's own remaining gap — `room.rs` + `kernel/src/streams.rs`

**Files:**
- Modify: `kernel/src/streams.rs` (retype `ROOM_FACE`, `ROOM_CHILD`)
- Modify: `kernel/src/room.rs` (2 sites: lines 567, 569)

**Interfaces:**
- Consumes: `StreamLabel`, `Seed::derive_typed` (Task 1).

- [ ] **Step 1: Retype `kernel/src/streams.rs`'s two constants**

Add `use crate::seed::StreamLabel;` after the module doc comment. Change:

```rust
/// Root label for a room's base face.
/// type-audit: bare-ok(identifier-text)
pub const ROOM_FACE: &str = "room/face";
/// Label for a room's child descent.
/// type-audit: bare-ok(identifier-text)
pub const ROOM_CHILD: &str = "room/child";
```

to:

```rust
/// Root label for a room's base face.
/// type-audit: bare-ok(identifier-text: return)
pub const ROOM_FACE: StreamLabel<'static> = StreamLabel::from_static("room/face");
/// Label for a room's child descent.
/// type-audit: bare-ok(identifier-text: return)
pub const ROOM_CHILD: StreamLabel<'static> = StreamLabel::from_static("room/child");
```

This file's own `stream_labels()` function already calls these constants by
name in a `vec![(ROOM_FACE, "..."), (ROOM_CHILD, "...")]` literal — since
`stream_labels()`'s return type must stay `Vec<(&'static str, &'static
str)>` (Global Constraint), change that vec to `vec![(ROOM_FACE.as_str(),
"room base face"), (ROOM_CHILD.as_str(), "room child descent")]`.

- [ ] **Step 2: Migrate `room.rs`'s two sites**

Read `kernel/src/room.rs` around lines 567/569 first — the exact
surrounding code (`world.derive(ROOM_FACE).derive(&self.face.to_string())`
and `s = s.derive(ROOM_CHILD).derive(&d.to_string())`) — then replace
`.derive(ROOM_FACE)` → `.derive_typed(ROOM_FACE)`, `.derive(ROOM_CHILD)` →
`.derive_typed(ROOM_CHILD)`, and the two `.derive(&self.face.to_string())`
/ `.derive(&d.to_string())` calls (runtime-formatted values, dynamic) →
`.derive_typed(StreamLabel::dynamic(&self.face.to_string()))` /
`.derive_typed(StreamLabel::dynamic(&d.to_string()))`. Add `use
crate::seed::StreamLabel;` if `room.rs` doesn't already have it in scope.

- [ ] **Step 3: Run kernel's full test suite and the workspace build**

Run: `cargo test -p hornvale-kernel 2>&1 | tail -40`
Run: `cargo build --workspace 2>&1 | tail -30`
Expected: PASS / clean — unchanged test counts (proves byte-identity; no
string value changed, only where each is referenced from).

- [ ] **Step 4: `cargo fmt`, clippy, type-audit**

Run: `cargo fmt -p hornvale-kernel`
Run: `cargo clippy -p hornvale-kernel --all-targets -- -D warnings`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`

- [ ] **Step 5: Commit**

```bash
git add kernel/src/streams.rs kernel/src/room.rs docs/audits/type-audit-report.md
git commit -m "refactor(kernel): migrate room.rs + streams.rs ROOM_FACE/ROOM_CHILD -- proc-17 T10b"
```

---

### Task 10c: migrate `domains/astronomy`'s remaining 21 constants

**Files:**
- Modify: `domains/astronomy/src/streams.rs` (retype all 21 remaining
  constants — every one except `ROOT`, already done in Task 2)
- Modify: `domains/astronomy/src/anchor.rs` (5 sites: lines 54, 74, 103,
  156, 173 — `ANCHOR_MASS`, `ROTATION`, `SPIN_DIRECTION`, `ORBIT`,
  `OBLIQUITY`)
- Modify: `domains/astronomy/src/wanderers.rs` (2 sites: lines 54, 67 —
  `WANDERER_COUNT`, `WANDERERS`)
- Modify: `domains/astronomy/src/neighborhood.rs` (2 sites: lines 89, 90 —
  `NEIGHBORS`, `NEIGHBOR_POSITIONS`)
- Modify: `domains/astronomy/src/star.rs` (2 sites: lines 64, 78 —
  `STAR_MASS`, `STAR_AGE`)
- Modify: `domains/astronomy/src/starfield.rs` (1 site: line 27 —
  `STARFIELD`)
- Modify: `domains/astronomy/src/forcing.rs` (2 sites: lines 92, 110 —
  `FORCING`, `PHASE_OFFSETS`)
- Modify: `domains/astronomy/src/moons.rs` (7 sites: lines 194, 209, 273,
  299, 313, 327, 351 — `MOON_COUNT`, `MOONS`, `MOON_FORMATION`,
  `MOON_INCLINATIONS`, `MOON_NODES`, `MOON_DENSITY`, `MOON_AGE`)

**Interfaces:**
- Consumes: `StreamLabel`, `Seed::derive_typed` (Task 1). `domains/
  astronomy/src/streams.rs::ROOT` is already `StreamLabel`-typed (Task 2)
  — this task retypes every OTHER constant in that same file.

If this task's scope proves too large for one reviewable diff once you
are into it (7 files, 19 call sites, 21 constants), split it at a natural
seam — e.g. `{anchor, star, orbit-adjacent}` vs `{moons, wanderers,
neighborhood, starfield, forcing}` — and report that split rather than
forcing one oversized commit.

- [ ] **Step 1: Retype all 21 remaining constants in `domains/astronomy/src/streams.rs`**

The file already has `use hornvale_kernel::seed::StreamLabel;` (added in
Task 2) and `ROOT` already `StreamLabel`-typed. For each of these 21
constants, change `pub const NAME: &str = "value";` to `pub const NAME:
StreamLabel<'static> = StreamLabel::from_static("value");`, and update
each one's own `type-audit: bare-ok(identifier-text)` tag to `type-audit:
bare-ok(identifier-text: return)` — preserve every existing doc comment
unchanged, only the type/value expression and the tag change:

```
STAR_MASS = "star-mass"
ANCHOR_MASS = "anchor-mass"
ROTATION = "rotation"
ORBIT = "orbit"
OBLIQUITY = "obliquity"
MOON_COUNT = "moon-count"
MOONS = "moons"
NEIGHBORS = "neighbors"
FORCING = "forcing"
PHASE_OFFSETS = "phase-offsets"
NEIGHBOR_POSITIONS = "neighbor-positions"
SPIN_DIRECTION = "spin-direction"
MOON_INCLINATIONS = "moon-inclinations"
WANDERER_COUNT = "wanderer-count"
WANDERERS = "wanderers"
STARFIELD = "starfield"
MOON_NODES = "moon-nodes"
STAR_AGE = "star-age"
MOON_FORMATION = "moon-formation"
MOON_DENSITY = "moon-density"
MOON_AGE = "moon-age"
```

(These 21 values were read directly from the committed file — do not
retype them from memory; open `domains/astronomy/src/streams.rs` and
confirm each one matches exactly before editing.)

- [ ] **Step 2: Check whether any OTHER crate directly references these 21 constants**

Task 2's own review found astronomy's `ROOT` had unexpected callers in
`windows/scene` and `windows/lab` — do the same thorough sweep here. Run:

```bash
grep -rn 'streams::\(STAR_MASS\|ANCHOR_MASS\|ROTATION\|ORBIT\|OBLIQUITY\|MOON_COUNT\|MOONS\|NEIGHBORS\|FORCING\|PHASE_OFFSETS\|NEIGHBOR_POSITIONS\|SPIN_DIRECTION\|MOON_INCLINATIONS\|WANDERER_COUNT\|WANDERERS\|STARFIELD\|MOON_NODES\|STAR_AGE\|MOON_FORMATION\|MOON_DENSITY\|MOON_AGE\)\b' --include=*.rs domains/ windows/ cli/ kernel/ | grep -v domains/astronomy
```

Fix every real hit the same minimal way (`.derive(` → `.derive_typed(`,
nothing else) in this same task.

- [ ] **Step 3: Migrate the 19 call sites listed in Files, above**

For each, replace `.derive(streams::CONST)` with
`.derive_typed(streams::CONST)` — the constant reference itself never
changes, only the method name. `moons.rs`'s `.derive(streams::MOON_COUNT)`
sits inside a chain also touching `.derive(...)` on OTHER already-`ROOT`-
migrated code from Task 2 — confirm you are not re-touching anything Task
2 already migrated, only the NEW constants this task retypes.

- [ ] **Step 4: Run astronomy's full test suite, the cross-crate hits from Step 2, and the workspace build**

Run: `cargo test -p hornvale-astronomy 2>&1 | tail -80`
Run any crate Step 2 found real hits in (e.g. `cargo test -p hornvale-scene -p hornvale-lab -p hornvale-worldgen 2>&1 | tail -60` if applicable)
Run: `cargo build --workspace 2>&1 | tail -30`
Expected: PASS / clean, unchanged counts throughout.

- [ ] **Step 5: `cargo fmt`, clippy, type-audit**

Run: `cargo fmt -p hornvale-astronomy`
Run: `cargo clippy -p hornvale-astronomy --all-targets -- -D warnings`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`

- [ ] **Step 6: Commit**

```bash
git add domains/astronomy/src/streams.rs domains/astronomy/src/anchor.rs domains/astronomy/src/wanderers.rs domains/astronomy/src/neighborhood.rs domains/astronomy/src/star.rs domains/astronomy/src/starfield.rs domains/astronomy/src/forcing.rs domains/astronomy/src/moons.rs docs/audits/type-audit-report.md
# plus any file Step 2's cross-crate sweep touched
git commit -m "refactor(astronomy): migrate remaining 21 constants -- proc-17 T10c"
```

---

### Task 10d: the remaining dynamic-only legs — `terrain`, `lab`, `language`

**Files:**
- Modify: `domains/terrain/src/rift.rs` (1 site: line 179)
- Modify: `domains/terrain/src/plates.rs` (1 site: line 159)
- Modify: `domains/terrain/src/crust.rs` (2 sites: lines 320, 994)
- Modify: `windows/lab/tests/fixture_staleness.rs` (1 site: line 57)
- Modify: `domains/language/src/schemas.rs` (2 sites: lines 621, 626)

**Interfaces:**
- Consumes: `StreamLabel`, `Seed::derive_typed` (Task 1). None of these
  sites need a new constant — every one is a genuinely dynamic,
  runtime-formatted value.

- [ ] **Step 1: Migrate terrain's 4 sites**

`use hornvale_kernel::seed::StreamLabel;` should already be present in
`crust.rs`/`rift.rs` (Task 3); add it to `plates.rs` if not already
there. `rift.rs:179`: `rift_root.derive(&format!("seam-{a}-{b}"))` →
`rift_root.derive_typed(StreamLabel::dynamic(&format!("seam-{a}-{b}")))`.
`plates.rs:159`: `edge_root.derive(&format!("plate-{}", plate.id))` →
`edge_root.derive_typed(StreamLabel::dynamic(&format!("plate-{}",
plate.id)))`. `crust.rs:320` and `:994`:
`lobing_root.derive(&format!("craton-{}", host.id))` /
`lobing_root.derive(&format!("craton-{}", c.id))` →
`lobing_root.derive_typed(StreamLabel::dynamic(&format!("craton-{}",
host.id)))` / the `c.id` equivalent.

- [ ] **Step 2: Migrate `windows/lab/tests/fixture_staleness.rs`'s site**

Add `use hornvale_kernel::seed::StreamLabel;`. Line 57:
`Seed(0).derive(csv).stream()` (`csv: &str` is a fixture/window
identifier parameter, not file content — genuinely dynamic) →
`Seed(0).derive_typed(StreamLabel::dynamic(csv)).stream()`.

- [ ] **Step 3: Migrate `domains/language/src/schemas.rs`'s two sites**

Add `use hornvale_kernel::seed::StreamLabel;` if not already present.
Lines 621 and 626: `Seed(1).derive(&label).stream()` (`label` is a
`String` built via `format!("test/schemas/beta-sweep/{i}")` inside a
test) → `Seed(1).derive_typed(StreamLabel::dynamic(&label)).stream()`, at
both sites.

- [ ] **Step 4: Run the affected crates' test suites and the workspace build**

Run: `cargo test -p hornvale-terrain -p hornvale-lab -p hornvale-language 2>&1 | tail -100`
Run: `cargo build --workspace 2>&1 | tail -30`
Expected: PASS / clean, unchanged counts.

- [ ] **Step 5: `cargo fmt`, clippy, type-audit**

Run: `cargo fmt -p hornvale-terrain -p hornvale-lab -p hornvale-language`
Run: `cargo clippy -p hornvale-terrain -p hornvale-lab -p hornvale-language --all-targets -- -D warnings`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`

- [ ] **Step 6: Commit**

```bash
git add domains/terrain/src/rift.rs domains/terrain/src/plates.rs domains/terrain/src/crust.rs windows/lab/tests/fixture_staleness.rs domains/language/src/schemas.rs docs/audits/type-audit-report.md
git commit -m "refactor: migrate remaining dynamic-only .derive() legs (terrain/lab/language) -- proc-17 T10d"
```

---

### Task 10e: remaining small-crate constants — `climate`, `locale`, `vessel`

**Files:**
- Modify: `domains/climate/src/streams.rs` (retype `WEATHER_PHASE`)
- Modify: `domains/climate/src/weather.rs` (retype the local test-only
  `WEATHER_PHASE_TEST` const at line 332; migrate its 3 call sites —
  lines 247, 278, 321 — plus `WEATHER_PHASE`'s own production call site
  at line 158)
- Modify: `windows/locale/src/streams.rs` (retype `LOCALE_MICRO`,
  `LOCALE_VARIETY`, `LOCALE_SUBSTRATE_DETAIL`, `LOCALE_PLACE`)
- Modify: `windows/locale/src/micro.rs` (1 site: line 12 — `LOCALE_MICRO`)
- Modify: `windows/locale/src/budget.rs` (1 site: line 122 —
  `LOCALE_PLACE`)
- Modify: `windows/locale/src/grammar.rs` (the `draw()` helper's own
  `label: &str` parameter, line ~66, and its internal `.derive(label)`
  call at line 72 — `LOCALE_VARIETY`/`LOCALE_SUBSTRATE_DETAIL` are passed
  INTO this function by their callers, not derived directly)
- Modify: `windows/vessel/src/streams.rs` (retype `VESSEL_AGENT`,
  `VESSEL_WALK`)
- Modify: `windows/vessel/src/agent.rs` (1 site: line 63 —
  `VESSEL_AGENT`)
- Modify: `windows/vessel/tests/walker_battery.rs` (1 site: line 38 —
  `VESSEL_WALK`)

**Interfaces:**
- Consumes: `StreamLabel`, `Seed::derive_typed` (Task 1).

- [ ] **Step 1: `domains/climate`**

In `streams.rs`, add `use hornvale_kernel::seed::StreamLabel;` and change
`pub const WEATHER_PHASE: &str = "climate/weather/phase/v1";` to `pub
const WEATHER_PHASE: StreamLabel<'static> =
StreamLabel::from_static("climate/weather/phase/v1");` (update its
`bare-ok(identifier-text)` tag to `bare-ok(identifier-text: return)`). In
`weather.rs`, add the same import; change line 332's `const
WEATHER_PHASE_TEST: &str = "climate/weather/phase/v1";` to `const
WEATHER_PHASE_TEST: StreamLabel<'static> =
StreamLabel::from_static("climate/weather/phase/v1");`; migrate line
158's `seed.derive(WEATHER_PHASE)` → `seed.derive_typed(WEATHER_PHASE)`
and lines 247/278/321's `Seed(...).derive(WEATHER_PHASE_TEST)` →
`Seed(...).derive_typed(WEATHER_PHASE_TEST)`.

- [ ] **Step 2: `windows/locale`**

In `streams.rs`, add `use hornvale_kernel::seed::StreamLabel;`; retype
all 4 constants the same way (`pub const LOCALE_MICRO: StreamLabel<'static>
= StreamLabel::from_static("locale/regime/micro");` etc. for
`LOCALE_VARIETY`, `LOCALE_SUBSTRATE_DETAIL`, `LOCALE_PLACE`, preserving
each literal value exactly); update `stream_labels()`'s own `vec![...]`
to call `.as_str()` on each (its return type stays `Vec<(&'static str,
&'static str)>`, unchanged). In `micro.rs`: `room_seed.derive(LOCALE_MICRO)`
→ `room_seed.derive_typed(LOCALE_MICRO)`. In `budget.rs`:
`seed.derive(LOCALE_PLACE)` → `seed.derive_typed(LOCALE_PLACE)`. In
`grammar.rs`: change `fn draw(room: Seed, label: &str, pool: Pool) ->
String` to `fn draw(room: Seed, label: StreamLabel<'_>, pool: Pool) ->
String` (its two callers passing `LOCALE_VARIETY`/`LOCALE_SUBSTRATE_DETAIL`
need no change — both are already `StreamLabel<'static>` after Step 2's
own retyping, which coerces fine to `StreamLabel<'_>`); change the
function body's `room.derive(label)` to `room.derive_typed(label)`.

- [ ] **Step 3: `windows/vessel`**

In `streams.rs`, add `use hornvale_kernel::seed::StreamLabel;`; retype
`VESSEL_AGENT`/`VESSEL_WALK` the same way, preserving their literal
values (`"vessel/agent"`, `"vessel/walk"`); update `stream_labels()`'s
`vec![...]` to call `.as_str()` on each. In `agent.rs`:
`.derive(VESSEL_AGENT)` → `.derive_typed(VESSEL_AGENT)`. In
`tests/walker_battery.rs`: `world.seed.derive(VESSEL_WALK)` →
`world.seed.derive_typed(VESSEL_WALK)`.

- [ ] **Step 4: Run all three crates' test suites and the workspace build**

Run: `cargo test -p hornvale-climate -p hornvale-locale -p hornvale-vessel 2>&1 | tail -100`
Run: `cargo build --workspace 2>&1 | tail -30`
Expected: PASS / clean, unchanged counts.

- [ ] **Step 5: `cargo fmt`, clippy, type-audit**

Run: `cargo fmt -p hornvale-climate -p hornvale-locale -p hornvale-vessel`
Run: `cargo clippy -p hornvale-climate -p hornvale-locale -p hornvale-vessel --all-targets -- -D warnings`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`

- [ ] **Step 6: Confirm the workspace-wide sweep is now genuinely clean**

Run: `grep -rn '\.derive(' --include=*.rs domains/ windows/ cli/ kernel/ | grep -v '\.derive_typed(' | grep -v '//' | grep -v '^\s*\*'`
Expected: exactly the 3 lines from `kernel/src/seed.rs` (Task 1's/Task
12's own intentional delegation + comparison tests) and, optionally, the
4-5 stale doc-comment lines already known and accepted as non-blocking
(Tasks 5/6/10). If anything ELSE appears, that is a real remaining gap —
do not proceed to Task 11/12 until it's understood and either fixed here
or explicitly re-scoped.

- [ ] **Step 7: Commit**

```bash
git add domains/climate/src/streams.rs domains/climate/src/weather.rs windows/locale/src/streams.rs windows/locale/src/micro.rs windows/locale/src/budget.rs windows/locale/src/grammar.rs windows/vessel/src/streams.rs windows/vessel/src/agent.rs windows/vessel/tests/walker_battery.rs docs/audits/type-audit-report.md
git commit -m "refactor: migrate remaining climate/locale/vessel constants -- proc-17 T10e"
```

---

### Task 11: a `tools/type-audit` check for inline `StreamLabel::from_static` literals

**Files:**
- Modify: `tools/type-audit/src/extract.rs` (or a new sibling module —
  read this file first to see whether a new check fits inside its
  existing structure or needs its own file; `tools/type-audit/src/walk.rs`
  already walks every workspace `.rs` file, so this check hooks into that
  existing traversal rather than adding a second one)
- Modify: `tools/type-audit/src/audit.rs` (wire the new check into
  the tool's existing pass/fail aggregation)
- Test: `tools/type-audit`'s own existing test suite (read
  `tools/type-audit`'s `Cargo.toml`/test layout first — this tool is
  OUTSIDE the workspace per decision 0027/0028, so it has its own,
  separate `cargo test` invocation)

**Interfaces:**
- Consumes: nothing from earlier tasks in this plan (this is a static-
  analysis rule about a fixed method name, `StreamLabel::from_static`,
  and a fixed exempt filename, `streams.rs` — neither depends on which
  crates have finished migrating).

- [ ] **Step 1: Read the existing tool structure before writing anything**

Read `tools/type-audit/src/extract.rs`, `tools/type-audit/src/walk.rs`,
and `tools/type-audit/src/audit.rs` in full. This tool already scans
every `.rs` file in the workspace for `bare-ok`/`waiver`/`pending` tags
on `pub`-boundary primitives — the new check is a DIFFERENT shape of rule
(a call-expression scan, not a tag scan), so identify the exact function
where source text for each file is already available (to avoid a second,
redundant file-walk) and add the new check there.

- [ ] **Step 2: Write the failing test**

In `tools/type-audit`'s own test suite (mirror its existing test
fixture/harness pattern exactly — read an existing check's test first),
add a test asserting: a fixture file NOT named `streams.rs` containing
`StreamLabel::from_static("some-literal")` is flagged; the identical
fixture renamed to `streams.rs` is NOT flagged; a fixture containing
`StreamLabel::dynamic(some_variable)` (not a literal) is NOT flagged
regardless of filename; a fixture containing
`StreamLabel::from_static(SOME_CONST)` (a named constant, not a string
literal, passed as the argument) is NOT flagged (the check specifically
targets a literal STRING argument, not any argument).

- [ ] **Step 3: Run the test to verify it fails**

Run: `cargo test --manifest-path tools/type-audit/Cargo.toml -- --nocapture 2>&1 | tail -30`
Expected: FAIL — the check doesn't exist yet.

- [ ] **Step 4: Implement the check**

Following the exact pattern the existing tag-scanning checks use in this
codebase (their real structure, read in Step 1 — do not invent a
different code style), implement: for each `.rs` file's source text,
find every `StreamLabel::from_static(` call, extract its argument; if the
argument is a string-literal token (starts and ends with `"`, not an
identifier), and the file's own name (not full path) is not
`streams.rs`, record a violation with the file:line. Wire this into
`audit.rs`'s existing pass/fail aggregation exactly as the other checks
are wired, so `type-audit check` exits non-zero when a violation exists
and `type-audit report` lists it.

- [ ] **Step 5: Run the test to verify it passes**

Run: `cargo test --manifest-path tools/type-audit/Cargo.toml -- --nocapture 2>&1 | tail -30`
Expected: PASS.

- [ ] **Step 6: Run the check against the REAL workspace**

Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check 2>&1 | tail -40`
Expected: PASS (exit 0) — every real `StreamLabel::from_static(...)` call
in the workspace, after Tasks 1-10, lives inside a file literally named
`streams.rs`. If this fails, it means an earlier task minted a constant
somewhere OTHER than that crate's own `streams.rs` — go fix that task's
own commit, don't work around it here.

- [ ] **Step 7: Regenerate the type-audit report**

Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`

- [ ] **Step 8: Commit**

```bash
git add tools/type-audit/src/ docs/audits/type-audit-report.md
git commit -m "feat(type-audit): flag StreamLabel::from_static(literal) calls outside streams.rs -- proc-17 T11"
```

---

### Task 12: the contract step — delete `derive(&str)`, rename `derive_typed` → `derive`

**Do not start this task until Tasks 1-11 (INCLUDING the mid-execution
remediation Tasks 10b, 10c, 10d, 10e) have all landed with clean
reviews.** This is the one task in the whole plan that touches every
crate at once — every migration must already be complete. Task 10's own
review found the original Tasks 1-11 scope, by itself, was NOT
sufficient (built from a grep for inline literals, which missed every
call site already passing a pre-declared `&str` constant) — Step 1's own
grep below is the real, load-bearing check; do not skip it or assume the
task list above is exhaustive just because every numbered task shows
complete.

**Files:**
- Modify: `kernel/src/seed.rs`
- Modify: every file this plan touched in Tasks 2-10 (a global,
  mechanical rename, not a semantic change)

**Interfaces:**
- Consumes: confirms nothing outside this plan's own scope still calls
  `Seed::derive(&str)` (see Step 1).

- [ ] **Step 1: Confirm zero remaining callers of the old signature**

Run: `grep -rn '\.derive("' --include=*.rs domains/ windows/ cli/ kernel/ 2>/dev/null`
Expected: EMPTY output. If anything remains, that is a real gap in an
earlier task — go fix that task's own files, do not patch it here.

Run: `grep -rn '\.derive(' --include=*.rs domains/ windows/ cli/ kernel/ 2>/dev/null | grep -v '\.derive_typed(' | grep -v '//'`
Expected: EMPTY output (every remaining `.derive(` call, if any survive
this filter, is either a comment or a call this plan's own tasks missed
— investigate and fix in the relevant earlier task before proceeding).

- [ ] **Step 2: Delete the old method, rename the new one**

In `kernel/src/seed.rs`, delete the entire old method:

```rust
    /// Derive a child seed for `label`. FNV-1a over the label, mixed with
    /// the parent, then scrambled. Stable forever: changing this breaks
    /// every saved world.
    /// type-audit: bare-ok(identifier-text)
    pub fn derive(&self, label: &str) -> Seed {
        let mut h = FNV_OFFSET ^ self.0;
        for byte in label.as_bytes() {
            h ^= u64::from(*byte);
            h = h.wrapping_mul(FNV_PRIME);
        }
        Seed(splitmix64(&mut h))
    }
```

Rename `derive_typed` to `derive` (update its own doc comment to remove
the "temporary name during migration" sentence Task 1 added, and inline
its body directly rather than delegating to the now-deleted method):

```rust
    /// Derive a child seed for a typed `label`. FNV-1a over the label's
    /// bytes, mixed with the parent, then scrambled. Stable forever:
    /// changing this breaks every saved world.
    pub fn derive(&self, label: StreamLabel<'_>) -> Seed {
        let bytes = label.as_str();
        let mut h = FNV_OFFSET ^ self.0;
        for byte in bytes.as_bytes() {
            h ^= u64::from(*byte);
            h = h.wrapping_mul(FNV_PRIME);
        }
        Seed(splitmix64(&mut h))
    }
```

Also update Task 1's own three tests in this file
(`derive_typed_matches_derive_for_the_same_bytes` etc.) — since
`derive(&str)` no longer exists, `derive_typed_matches_derive_for_the_same_bytes`
can no longer compare against it directly. Replace those three tests
with:

```rust
    #[test]
    fn derive_is_deterministic_for_a_typed_label() {
        let seed = Seed(42);
        assert_eq!(
            seed.derive(StreamLabel::from_static("astronomy")),
            seed.derive(StreamLabel::from_static("astronomy"))
        );
    }

    #[test]
    fn derive_differs_by_typed_label() {
        let seed = Seed(42);
        assert_ne!(
            seed.derive(StreamLabel::from_static("astronomy")),
            seed.derive(StreamLabel::from_static("climate"))
        );
    }

    #[test]
    fn derive_dynamic_and_static_agree_on_the_same_bytes() {
        let seed = Seed(7);
        let owned = String::from("goblin");
        assert_eq!(
            seed.derive(StreamLabel::dynamic(&owned)),
            seed.derive(StreamLabel::dynamic("goblin"))
        );
    }
```

- [ ] **Step 3: Global mechanical rename across every touched file**

Run this from the repository root — it is a pure textual substitution
(`.derive_typed(` → `.derive(`), safe because Step 1 already confirmed no
OTHER `.derive(` call exists anywhere that this rename could collide
with:

```bash
grep -rl '\.derive_typed(' --include=*.rs domains/ windows/ cli/ kernel/ | xargs sed -i '' 's/\.derive_typed(/.derive(/g'
```

(On Linux, drop the empty `''` after `-i`; confirm which platform you're
on before running — this project's own CI runs Linux, macOS is the
common local dev machine, `sed -i` syntax differs between them.)

- [ ] **Step 4: Confirm the whole workspace builds and every test passes**

Run: `cargo build --workspace 2>&1 | tail -40`
Expected: clean.
Run: `cargo test --workspace 2>&1 | tee /tmp/proc17-final-test.txt | tail -60`
Expected: every test passes; grep `/tmp/proc17-final-test.txt` for
`FAILED` to confirm zero.

- [ ] **Step 5: Regenerate the stream manifest and confirm no drift**

Run:
```bash
cargo run -p hornvale -- streams > /tmp/hv-streams-final.md
diff /tmp/hv-streams-final.md book/src/reference/stream-manifest-generated.md
```
Expected: empty diff — no label VALUES changed anywhere in this whole
campaign, so the manifest's content is unchanged (only Task 9's own
earlier regen, which added the worldgen/chronicle sections, is reflected
— this diff should be empty because Task 9 already committed the current
correct state, and nothing since then touched any label's value).

- [ ] **Step 6: `cargo fmt`, clippy, type-audit, full gate**

Run: `cargo fmt --all`
Run: `cargo clippy --workspace --all-targets -- -D warnings`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`
Run: `make gate 2>&1 | tail -60`
Expected: exit 0.

- [ ] **Step 7: Commit**

```bash
git add -A
git commit -m "refactor(kernel): the contract step -- delete derive(&str), rename derive_typed to derive -- proc-17 T12"
```
