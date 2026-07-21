# PROC-18: `stream_labels!` Macro Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a `stream_labels!` macro (`kernel/src/seed.rs`) that declares a
crate's `StreamLabel` constants and generates its `stream_labels()` manifest
function from one authored list, then migrate every crate's `streams.rs` to
it, closing the recurring "constant declared but never reaches the
manifest" bug at compile time.

**Architecture:** One `macro_rules!` macro, two forms (flat; root+leg),
`#[macro_export]`ed from `kernel/src/seed.rs`. Each crate's `streams.rs`
becomes a single macro invocation; `stream_labels()` moves into
`streams.rs` wherever it currently lives elsewhere (astronomy, terrain,
climate all currently define it in `lib.rs`), with a `pub use
streams::stream_labels;` re-export added to match the convention `kernel`,
`windows/chronicle`, `windows/locale`, and `windows/vessel` already use.

**Tech Stack:** Rust `macro_rules!` (decision 0019), the `std`-builtin
`concat!` macro for compile-time string concatenation (no new dependency).

**Full spec:** `docs/superpowers/specs/2026-07-21-proc-18-stream-labels-macro-design.md`

## Global Constraints

- `stream_labels()`'s return type (`Vec<(&'static str, &'static str)>`)
  never changes, anywhere, for any crate.
- No epoch bump anywhere in this campaign — every migrated `StreamLabel`'s
  `.as_str()` value must be byte-identical before and after its task. This
  is a declaration-site refactor only; a value change of any kind is a bug.
- `concat!` is a `std` builtin — using it does not touch the workspace's
  dependency allowlist (serde + serde_json only, decision-log enforced by
  `cli/tests/architecture.rs`).
- No new `tools/type-audit` check is added by this campaign (spec §5.2,
  §8) — the macro's compile-time fusion is the enforcement mechanism.
- `domains/language` is completely untouched by every task in this plan —
  neither its `streams.rs` nor its `lib.rs`'s `stream_labels()`.
- `kernel/src/streams.rs`'s `OCTAVE_LABELS: [StreamLabel<'static>; 16]`
  stays exactly as it is today, hand-authored, outside any macro
  invocation, in the same file as the migrated `ROOM_FACE`/`ROOM_CHILD`.
- Every migration task's testing story is the same: run that crate's
  existing determinism/property tests **unchanged** before and after. A
  pass proves byte-identity — only the *declaration site* moved, never the
  *value* — because this project's determinism discipline (root
  `CLAUDE.md`, "Determinism" section) treats any divergence in a derived
  seed as catastrophic and silent. Cite this framing, not just "tests
  pass," in every task's verification step.
- `make gate` already runs `tools/type-audit -- check` locally (`Makefile`:
  `gate: fmt-check clippy type-audit test`) — it is not a CI-only, later
  check. Run `make gate` at the end of every task that touches a `pub`
  boundary (every task in this plan does).

---

## Task 1: The `stream_labels!` macro itself

**Files:**
- Modify: `kernel/src/seed.rs` (insert after line 79, the closing `}` of
  `impl Seed`, before line 81's `/// A splitmix64 random stream...`)

**Interfaces:**
- Produces: `hornvale_kernel::stream_labels!` (flat form and root+leg
  form, both described below) — every later task in this plan invokes this
  macro. `#[macro_export]` places it at the crate root, so it is invoked as
  `hornvale_kernel::stream_labels! { ... }` from every other crate, or
  `crate::stream_labels! { ... }` from within `hornvale-kernel` itself.

This task is purely additive — nothing outside `kernel/src/seed.rs`
changes, so there is no migration risk yet. It proves the macro compiles
and expands correctly in isolation before any real crate depends on it.

- [ ] **Step 1: Write the macro definition**

Insert this block into `kernel/src/seed.rs` immediately after line 79 (the
`}` that closes `impl Seed { ... }`) and before line 81:

```rust

/// Declares a crate's `StreamLabel` constants and its `stream_labels()`
/// manifest function from one authored list (PROC-18), so a constant and
/// its manifest row can never drift apart — there is no second list to
/// forget.
///
/// Two forms:
///
/// **Flat** — each entry's own literal is already the complete manifest
/// key (matches `windows/chronicle`, `windows/locale`, `windows/vessel`,
/// `windows/worldgen`, `domains/climate`, and `kernel`'s own labels):
///
/// ```ignore
/// hornvale_kernel::stream_labels! {
///     /// The species-generation stream.
///     SPECIES = "chronicle/species" => "the species-generation stream";
/// }
/// ```
///
/// **Root+leg** — a crate-wide `ROOT` prefix plus bare per-leg names,
/// composed into the manifest key at compile time via `concat!` (matches
/// `domains/astronomy` and `domains/terrain`). The `.derive()` call chain
/// at every real use site is unaffected: it still chains
/// `.derive(streams::ROOT).derive(streams::LEG)` as two separate hash
/// steps, exactly as before this macro existed. Only the manifest's
/// documentation string is composed here:
///
/// ```ignore
/// hornvale_kernel::stream_labels! {
///     /// Root stream label for astronomy.
///     root: ROOT = "astronomy" => "root stream for sky genesis";
///     legs {
///         /// Star mass draw.
///         STAR_MASS = "star-mass" => "main-sequence star mass draw";
///     }
/// }
/// ```
#[macro_export]
macro_rules! stream_labels {
    (
        $(#[$root_meta:meta])*
        root: $root_name:ident = $root_value:literal => $root_desc:literal;
        legs {
            $(
                $(#[$leg_meta:meta])*
                $leg_name:ident = $leg_value:literal => $leg_desc:literal;
            )+
        }
    ) => {
        $(#[$root_meta])*
        pub const $root_name: $crate::seed::StreamLabel<'static> =
            $crate::seed::StreamLabel::from_static($root_value);
        $(
            $(#[$leg_meta])*
            pub const $leg_name: $crate::seed::StreamLabel<'static> =
                $crate::seed::StreamLabel::from_static($leg_value);
        )+

        /// Every seed-derivation label this crate uses, for the generated
        /// stream manifest (PROC-18: generated by the `stream_labels!`
        /// macro from the same list that declares the constants above).
        pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
            vec![
                ($root_name.as_str(), $root_desc),
                $( (concat!($root_value, "/", $leg_value), $leg_desc) ),+
            ]
        }
    };
    (
        $(
            $(#[$meta:meta])*
            $name:ident = $value:literal => $desc:literal;
        )+
    ) => {
        $(
            $(#[$meta])*
            pub const $name: $crate::seed::StreamLabel<'static> =
                $crate::seed::StreamLabel::from_static($value);
        )+

        /// Every seed-derivation label this crate uses, for the generated
        /// stream manifest (PROC-18: generated by the `stream_labels!`
        /// macro from the same list that declares the constants above).
        pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
            vec![ $( ($name.as_str(), $desc) ),+ ]
        }
    };
}
```

- [ ] **Step 2: Write the failing tests**

Add a new test module at the end of `kernel/src/seed.rs` (after the
existing `#[cfg(test)] mod tests { ... }` block, i.e. append after the
file's final `}`):

```rust

#[cfg(test)]
mod stream_labels_macro_tests {
    // A private nested module per form, so each form's `stream_labels()`
    // doesn't collide with the other's.

    mod flat {
        crate::stream_labels! {
            /// A test-only flat label.
            ALPHA = "test/alpha" => "the alpha test label";
            /// A second test-only flat label.
            BETA = "test/beta" => "the beta test label";
        }

        #[test]
        fn declares_constants_with_the_exact_values() {
            assert_eq!(ALPHA.as_str(), "test/alpha");
            assert_eq!(BETA.as_str(), "test/beta");
        }

        #[test]
        fn generates_a_manifest_entry_per_constant_in_order() {
            assert_eq!(
                stream_labels(),
                vec![
                    ("test/alpha", "the alpha test label"),
                    ("test/beta", "the beta test label"),
                ]
            );
        }
    }

    mod root_and_leg {
        crate::stream_labels! {
            /// Root stream label for the test crate.
            root: ROOT = "testcrate" => "root stream for the test crate";
            legs {
                /// A test-only leg.
                GAMMA = "gamma" => "the gamma test leg";
                /// A second test-only leg.
                DELTA = "delta" => "the delta test leg";
            }
        }

        #[test]
        fn root_and_legs_keep_their_bare_values() {
            // The real .derive() call chain uses these bare values,
            // never the concatenated manifest string — this is the
            // determinism-critical invariant (spec §4).
            assert_eq!(ROOT.as_str(), "testcrate");
            assert_eq!(GAMMA.as_str(), "gamma");
            assert_eq!(DELTA.as_str(), "delta");
        }

        #[test]
        fn manifest_qualifies_each_leg_under_root() {
            assert_eq!(
                stream_labels(),
                vec![
                    ("testcrate", "root stream for the test crate"),
                    ("testcrate/gamma", "the gamma test leg"),
                    ("testcrate/delta", "the delta test leg"),
                ]
            );
        }
    }
}
```

- [ ] **Step 3: Run the tests to verify they fail before Step 1's macro exists**

This step is written second only because the macro (Step 1) and its tests
(Step 2) are one indivisible unit — the macro has no meaning without a
consumer, so TDD here means: write both, then run once. Confirm:

Run: `cargo test -p hornvale-kernel stream_labels_macro_tests -- --nocapture`
Expected (if Step 1 were skipped): compile error, `cannot find macro
'stream_labels' in this scope`.

- [ ] **Step 4: Run the tests to verify they pass with the macro in place**

Run: `cargo test -p hornvale-kernel stream_labels_macro_tests`
Expected: `test result: ok. 4 passed; 0 failed`

- [ ] **Step 5: Run the full kernel test suite to confirm nothing else moved**

Run: `cargo test -p hornvale-kernel`
Expected: all existing tests still pass (this task added tests, changed
nothing else — `Seed::derive`, `StreamLabel`, and every existing kernel
constant are untouched).

- [ ] **Step 6: Format, lint, commit**

```bash
cargo fmt
cargo clippy -p hornvale-kernel --all-targets -- -D warnings
git add kernel/src/seed.rs
git commit -m "feat(kernel): stream_labels! macro (flat + root/leg forms)

Fuses a crate's StreamLabel constant declarations with their
stream_labels() manifest entries into one authored list (PROC-18),
closing the recurring desync class at compile time. Purely additive —
no existing crate uses it yet."
```

---

## Task 2: Migrate `kernel/src/streams.rs`

**Files:**
- Modify: `kernel/src/streams.rs`

**Interfaces:**
- Consumes: `hornvale_kernel::stream_labels!` (flat form, Task 1) — used
  here as `crate::stream_labels!` since this is the defining crate.
- Produces: no change to any external interface — `ROOM_FACE`,
  `ROOM_CHILD`, and `stream_labels()` keep their exact current names,
  types, and values.

Smallest possible real migration (2 entries) — proves the macro works
end-to-end inside the crate that defines it, before any other crate
depends on it. `OCTAVE_LABELS` is untouched (Global Constraints).

- [ ] **Step 1: Replace the file's constant + function declarations**

Current `kernel/src/streams.rs` (full file) is:

```rust
//! Seed-derivation labels for the kernel — permanent save-format contracts
//! (The Room Mesh spec §6/§10). Changing one silently corrupts every world.

use crate::seed::StreamLabel;

/// Root label for a room's base face.
/// type-audit: bare-ok(identifier-text: return)
pub const ROOM_FACE: StreamLabel<'static> = StreamLabel::from_static("room/face");
/// Label for a room's child descent.
/// type-audit: bare-ok(identifier-text: return)
pub const ROOM_CHILD: StreamLabel<'static> = StreamLabel::from_static("room/child");

/// Static per-octave derivation labels for `noise::fbm_2d`'s common octave
/// range, indexed by octave number (index 0 is a placeholder — octave 0
/// uses the seed directly and never derives). Byte-for-byte the labels
/// `format!("octave-{n}")` produces, so the derived seeds — and every
/// noise value — are bit-identical; the table only removes the per-call
/// `format!` (a heap-allocated `String` per octave per sample), which a
/// Task 6 profiling pass measured at ~52% of whole-world generation time
/// (fbm is called per octave per slice per field sample). Octave counts
/// beyond the table fall back to `format!` with the same label scheme,
/// preserving exact semantics for any count. Save-format contract: never
/// reorder or re-word an entry.
/// type-audit: bare-ok(artifact: return)
pub const OCTAVE_LABELS: [StreamLabel<'static>; 16] = [
    StreamLabel::from_static("octave-0"),
    StreamLabel::from_static("octave-1"),
    StreamLabel::from_static("octave-2"),
    StreamLabel::from_static("octave-3"),
    StreamLabel::from_static("octave-4"),
    StreamLabel::from_static("octave-5"),
    StreamLabel::from_static("octave-6"),
    StreamLabel::from_static("octave-7"),
    StreamLabel::from_static("octave-8"),
    StreamLabel::from_static("octave-9"),
    StreamLabel::from_static("octave-10"),
    StreamLabel::from_static("octave-11"),
    StreamLabel::from_static("octave-12"),
    StreamLabel::from_static("octave-13"),
    StreamLabel::from_static("octave-14"),
    StreamLabel::from_static("octave-15"),
];

/// Every kernel seed label, for the generated stream manifest.
/// type-audit: bare-ok(artifact: return)
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        (ROOM_FACE.as_str(), "room base face"),
        (ROOM_CHILD.as_str(), "room child descent"),
    ]
}
```

Replace the whole file with:

```rust
//! Seed-derivation labels for the kernel — permanent save-format contracts
//! (The Room Mesh spec §6/§10). Changing one silently corrupts every world.
//!
//! `ROOM_FACE`/`ROOM_CHILD` are declared via `stream_labels!` (PROC-18),
//! which also generates `stream_labels()` below. `OCTAVE_LABELS` stays
//! hand-authored, outside the macro: it is a single constant holding an
//! *array* of 16 algorithmically-named labels, not 16 independently
//! meaningful manifest rows, and is deliberately never published in
//! `stream_labels()` (PROC-18 spec §5.1).

use crate::seed::StreamLabel;

crate::stream_labels! {
    /// Root label for a room's base face.
    ROOM_FACE = "room/face" => "room base face";
    /// Label for a room's child descent.
    ROOM_CHILD = "room/child" => "room child descent";
}

/// Static per-octave derivation labels for `noise::fbm_2d`'s common octave
/// range, indexed by octave number (index 0 is a placeholder — octave 0
/// uses the seed directly and never derives). Byte-for-byte the labels
/// `format!("octave-{n}")` produces, so the derived seeds — and every
/// noise value — are bit-identical; the table only removes the per-call
/// `format!` (a heap-allocated `String` per octave per sample), which a
/// Task 6 profiling pass measured at ~52% of whole-world generation time
/// (fbm is called per octave per slice per field sample). Octave counts
/// beyond the table fall back to `format!` with the same label scheme,
/// preserving exact semantics for any count. Save-format contract: never
/// reorder or re-word an entry.
/// type-audit: bare-ok(artifact: return)
pub const OCTAVE_LABELS: [StreamLabel<'static>; 16] = [
    StreamLabel::from_static("octave-0"),
    StreamLabel::from_static("octave-1"),
    StreamLabel::from_static("octave-2"),
    StreamLabel::from_static("octave-3"),
    StreamLabel::from_static("octave-4"),
    StreamLabel::from_static("octave-5"),
    StreamLabel::from_static("octave-6"),
    StreamLabel::from_static("octave-7"),
    StreamLabel::from_static("octave-8"),
    StreamLabel::from_static("octave-9"),
    StreamLabel::from_static("octave-10"),
    StreamLabel::from_static("octave-11"),
    StreamLabel::from_static("octave-12"),
    StreamLabel::from_static("octave-13"),
    StreamLabel::from_static("octave-14"),
    StreamLabel::from_static("octave-15"),
];
```

Note `use crate::seed::StreamLabel;` stays — `OCTAVE_LABELS`'s type
annotation still needs it; the macro invocation uses `$crate::seed::
StreamLabel` internally and does not need the local `use`.

- [ ] **Step 2: Confirm it compiles and the values are unchanged**

Run: `cargo build -p hornvale-kernel`
Expected: no errors.

Run: `cargo test -p hornvale-kernel`
Expected: all tests pass, including any existing test that reads
`ROOM_FACE`/`ROOM_CHILD`/`OCTAVE_LABELS` or calls
`crate::stream_labels()` — a pass proves the values are byte-identical to
before this task (per Global Constraints' determinism framing: only the
declaration site moved).

- [ ] **Step 3: Confirm the type-audit tag boilerplate is now gone and the tool is still happy**

`ROOM_FACE`/`ROOM_CHILD` no longer carry a `type-audit:` tag line (spec
§5.2 — the check can no longer see a raw `StreamLabel::from_static(literal)`
call once it's inside the macro invocation, so there is nothing to tag).
`OCTAVE_LABELS` keeps its existing tag (it's still hand-authored, outside
the macro).

Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`
Expected: exit 0 (no untagged-primitive failures — the macro-declared
constants are `StreamLabel`, a newtype, never a bare primitive, so they
were never taggable in the first place; only `OCTAVE_LABELS`'s existing
tag matters here and it is untouched).

- [ ] **Step 4: Full gate, format, commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
make gate
git add kernel/src/streams.rs
git commit -m "refactor(kernel): migrate streams.rs to stream_labels! (PROC-18)

ROOM_FACE/ROOM_CHILD now declared via the macro from Task 1; values
unchanged. OCTAVE_LABELS stays hand-authored per the spec's explicit
carve-out (an algorithmic array, not independent manifest rows)."
```

---

## Task 3: Migrate `domains/climate/src/streams.rs`

**Files:**
- Modify: `domains/climate/src/streams.rs`
- Modify: `domains/climate/src/lib.rs:62-68` (the current `stream_labels()`
  free function, to be deleted and replaced by a re-export)

**Interfaces:**
- Consumes: `hornvale_kernel::stream_labels!` (flat form, Task 1).
- Produces: `crate::stream_labels()` (via re-export) — unchanged path,
  used by `domains/climate/src/lib.rs:194-195`'s `Domain` trait impl
  (`fn stream_labels(&self) -> Vec<(&'static str, &'static str)> {
  crate::stream_labels() }` — this line is NOT modified by this task).

**Live gap this task fixes:** climate's current `stream_labels()` (in
`lib.rs`) is `Vec::new()` with the doc comment "Every seed-derivation
label this crate uses (none yet)." But `streams.rs` already declares
`WEATHER_PHASE` (added later, by The Firmament campaign, without the doc
comment or the manifest being updated) — a live instance of exactly the
bug this campaign targets, on the smallest possible crate. This task
closes it.

- [ ] **Step 1: Replace `streams.rs`**

Current `domains/climate/src/streams.rs` (full file) is:

```rust
//! Seed-derivation labels for the climate domain (save-format contract — a
//! rename silently corrupts every world; deliberate regeneration uses an epoch
//! suffix, e.g. `.../v2`). Climate is otherwise seed-free (temperature,
//! moisture, and biome are pure derived reads); the only stochastic climate
//! layer is drawn weather (The Firmament).

use hornvale_kernel::seed::StreamLabel;

/// The label deriving the drifting weather-phase noise seed (The Firmament).
/// type-audit: bare-ok(identifier-text: return)
pub const WEATHER_PHASE: StreamLabel<'static> =
    StreamLabel::from_static("climate/weather/phase/v1");
```

Replace it with:

```rust
//! Seed-derivation labels for the climate domain (save-format contract — a
//! rename silently corrupts every world; deliberate regeneration uses an epoch
//! suffix, e.g. `.../v2`). Climate is otherwise seed-free (temperature,
//! moisture, and biome are pure derived reads); the only stochastic climate
//! layer is drawn weather (The Firmament).

hornvale_kernel::stream_labels! {
    /// The label deriving the drifting weather-phase noise seed (The Firmament).
    WEATHER_PHASE = "climate/weather/phase/v1" => "drifting weather-phase noise seed (The Firmament)";
}
```

- [ ] **Step 2: Delete the stale `stream_labels()` in `lib.rs` and re-export the macro-generated one**

In `domains/climate/src/lib.rs`, find and delete this block (currently at
lines 62-68):

```rust
/// Every seed-derivation label this crate uses (none yet).
/// type-audit: bare-ok(identifier-text)
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    Vec::new()
}
```

In its place, add a re-export near the crate's other `pub mod` /
`pub use` declarations at the top of `domains/climate/src/lib.rs` (after
the existing `pub mod streams;` line, matching the precedent already used
by `kernel`, `windows/chronicle`, and `windows/vessel`):

```rust
pub use streams::stream_labels;
```

- [ ] **Step 3: Confirm it compiles and the value is unchanged**

Run: `cargo build -p hornvale-climate`
Expected: no errors. `crate::stream_labels()` (called by the `Domain`
trait impl at `lib.rs:194-195`, unmodified by this task) now resolves
through the re-export instead of the deleted local function.

Run: `cargo test -p hornvale-climate`
Expected: all tests pass. `WEATHER_PHASE.as_str()` is unchanged
(`"climate/weather/phase/v1"`) — verify no test asserts the old empty
`stream_labels()` result (if one does, per Global Constraints this is a
deliberate content fix — the empty-vec assertion was already wrong, since
`WEATHER_PHASE` was live and undocumented; update that assertion to expect
`vec![("climate/weather/phase/v1", "drifting weather-phase noise seed (The Firmament)")]`
rather than reverting the fix).

- [ ] **Step 4: Full gate, format, commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
make gate
git add domains/climate/src/streams.rs domains/climate/src/lib.rs
git commit -m "refactor(climate): migrate streams.rs to stream_labels! (PROC-18)

Also fixes a live gap: WEATHER_PHASE existed in streams.rs but
stream_labels() still returned Vec::new() with a stale 'none yet'
doc comment. Now published."
```

---

## Task 4: Migrate `windows/vessel/src/streams.rs`

**Files:**
- Modify: `windows/vessel/src/streams.rs`

**Interfaces:**
- Consumes: `hornvale_kernel::stream_labels!` (flat form, Task 1).
- Produces: no change — `windows/vessel/src/lib.rs:17`'s existing
  `pub use streams::stream_labels;` is untouched (already correct, this
  crate already follows the post-migration convention).

- [ ] **Step 1: Replace `streams.rs`**

Current `windows/vessel/src/streams.rs` (full file) is:

```rust
//! Seed-derivation labels for the vessel window — save-format contracts.
//! Changing a label silently re-mints every agent and re-routes every
//! battery walk.

use hornvale_kernel::seed::StreamLabel;

/// Stream label for the minted agent's id draw.
/// type-audit: bare-ok(identifier-text: return)
pub const VESSEL_AGENT: StreamLabel<'static> = StreamLabel::from_static("vessel/agent");
/// Stream label for the walker battery's deterministic walk.
/// type-audit: bare-ok(identifier-text: return)
pub const VESSEL_WALK: StreamLabel<'static> = StreamLabel::from_static("vessel/walk");

/// Every vessel seed label, for the generated stream manifest.
/// type-audit: bare-ok(artifact: return)
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        (VESSEL_AGENT.as_str(), "minted agent id draw"),
        (VESSEL_WALK.as_str(), "walker-battery deterministic walk"),
    ]
}
```

Replace it with:

```rust
//! Seed-derivation labels for the vessel window — save-format contracts.
//! Changing a label silently re-mints every agent and re-routes every
//! battery walk.

hornvale_kernel::stream_labels! {
    /// Stream label for the minted agent's id draw.
    VESSEL_AGENT = "vessel/agent" => "minted agent id draw";
    /// Stream label for the walker battery's deterministic walk.
    VESSEL_WALK = "vessel/walk" => "walker-battery deterministic walk";
}
```

- [ ] **Step 2: Confirm it compiles and the values are unchanged**

Run: `cargo build -p hornvale-vessel && cargo test -p hornvale-vessel`
Expected: no errors, all tests pass (`VESSEL_AGENT`/`VESSEL_WALK` values
byte-identical to before).

- [ ] **Step 3: Full gate, format, commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
make gate
git add windows/vessel/src/streams.rs
git commit -m "refactor(vessel): migrate streams.rs to stream_labels! (PROC-18)"
```

---

## Task 5: Migrate `windows/locale/src/streams.rs`

**Files:**
- Modify: `windows/locale/src/streams.rs`

**Interfaces:**
- Consumes: `hornvale_kernel::stream_labels!` (flat form, Task 1).
- Produces: no change — `windows/locale/src/lib.rs:5`'s existing
  `pub use streams::stream_labels;` is untouched.

- [ ] **Step 1: Replace `streams.rs`**

Current `windows/locale/src/streams.rs` (full file) is:

```rust
//! Seed-derivation labels for the locale window — save-format contracts.
//! Changing a label silently moves every room's regime.

use hornvale_kernel::seed::StreamLabel;

/// Stream label for a room's sub-cell micro-field.
/// type-audit: bare-ok(identifier-text: return)
pub const LOCALE_MICRO: StreamLabel<'static> = StreamLabel::from_static("locale/regime/micro");
/// Stream label for a room's descriptor variety draw.
/// type-audit: bare-ok(identifier-text: return)
pub const LOCALE_VARIETY: StreamLabel<'static> = StreamLabel::from_static("locale/regime/variety");
/// Stream label for a room's substrate-detail draw.
/// type-audit: bare-ok(identifier-text: return)
pub const LOCALE_SUBSTRATE_DETAIL: StreamLabel<'static> =
    StreamLabel::from_static("locale/regime/substrate");
/// Stream label for the world's rarity-budget exotic placement pass.
/// type-audit: bare-ok(identifier-text: return)
pub const LOCALE_PLACE: StreamLabel<'static> = StreamLabel::from_static("locale/strangeness/place");

/// Every locale seed label, for the generated stream manifest.
/// type-audit: bare-ok(artifact: return)
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        (LOCALE_MICRO.as_str(), "room sub-cell micro-field"),
        (LOCALE_VARIETY.as_str(), "room descriptor variety draw"),
        (
            LOCALE_SUBSTRATE_DETAIL.as_str(),
            "room substrate-detail draw",
        ),
        (LOCALE_PLACE.as_str(), "world rarity-budget placement pass"),
    ]
}
```

Replace it with:

```rust
//! Seed-derivation labels for the locale window — save-format contracts.
//! Changing a label silently moves every room's regime.

hornvale_kernel::stream_labels! {
    /// Stream label for a room's sub-cell micro-field.
    LOCALE_MICRO = "locale/regime/micro" => "room sub-cell micro-field";
    /// Stream label for a room's descriptor variety draw.
    LOCALE_VARIETY = "locale/regime/variety" => "room descriptor variety draw";
    /// Stream label for a room's substrate-detail draw.
    LOCALE_SUBSTRATE_DETAIL = "locale/regime/substrate" => "room substrate-detail draw";
    /// Stream label for the world's rarity-budget exotic placement pass.
    LOCALE_PLACE = "locale/strangeness/place" => "world rarity-budget placement pass";
}
```

- [ ] **Step 2: Confirm it compiles and the values are unchanged**

Run: `cargo build -p hornvale-locale && cargo test -p hornvale-locale`
Expected: no errors, all tests pass, including
`windows/locale/src/grammar.rs`'s `draw(room: Seed, label: StreamLabel<'_>,
pool: Pool)` helper (a PROC-17 call site — unaffected, since it references
these constants by name, not by declaration mechanism).

- [ ] **Step 3: Full gate, format, commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
make gate
git add windows/locale/src/streams.rs
git commit -m "refactor(locale): migrate streams.rs to stream_labels! (PROC-18)"
```

---

## Task 6: Migrate `windows/chronicle/src/streams.rs`

**Files:**
- Modify: `windows/chronicle/src/streams.rs`

**Interfaces:**
- Consumes: `hornvale_kernel::stream_labels!` (flat form, Task 1).
- Produces: no change — `windows/chronicle/src/lib.rs:20`'s existing
  `pub use streams::stream_labels;` is untouched.

- [ ] **Step 1: Replace `streams.rs`**

Current `windows/chronicle/src/streams.rs` (full file) is:

```rust
//! Seed-derivation labels for `windows/chronicle` (the living-community
//! macro-history sounding). Save-format contracts; a rename silently
//! corrupts every world. Every entry here is a full flat path (this
//! crate never composes legs — matches `domains/climate`'s own
//! `WEATHER_PHASE` pattern), authored by PROC-17 (this crate had zero
//! centralization before).

use hornvale_kernel::seed::StreamLabel;

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

Replace it with:

```rust
//! Seed-derivation labels for `windows/chronicle` (the living-community
//! macro-history sounding). Save-format contracts; a rename silently
//! corrupts every world. Every entry here is a full flat path (this
//! crate never composes legs — matches `domains/climate`'s own
//! `WEATHER_PHASE` pattern), authored by PROC-17 (this crate had zero
//! centralization before) and declared via `stream_labels!` (PROC-18).

hornvale_kernel::stream_labels! {
    /// The species-generation stream.
    SPECIES = "chronicle/species" => "the species-generation stream";
    /// The carrying-capacity draw.
    CAPACITY = "chronicle/capacity" => "the carrying-capacity draw";
    /// The community-formation draw.
    COMMUNITIES = "chronicle/communities" => "the community-formation draw";
    /// The connection-graph draw.
    GRAPH = "chronicle/graph" => "the connection-graph draw";
    /// The event-simulation stream.
    EVENTS = "chronicle/events" => "the event-simulation stream";
    /// The event-delivery/propagation stream.
    DELIVER = "chronicle/deliver" => "the event-delivery/propagation stream";
    /// The replay-measurement stream.
    REPLAY = "chronicle/replay" => "the replay-measurement stream";
}
```

- [ ] **Step 2: Confirm it compiles and the values are unchanged**

Run: `cargo build -p hornvale-chronicle && cargo test -p hornvale-chronicle`
Expected: no errors, all tests pass (`SPECIES`..`REPLAY` values
byte-identical).

- [ ] **Step 3: Full gate, format, commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
make gate
git add windows/chronicle/src/streams.rs
git commit -m "refactor(chronicle): migrate streams.rs to stream_labels! (PROC-18)"
```

---

## Task 7: Migrate `windows/worldgen/src/streams.rs`

**Files:**
- Modify: `windows/worldgen/src/streams.rs`

**Interfaces:**
- Consumes: `hornvale_kernel::stream_labels!` (flat form, Task 1).
- Produces: no change — `cli/src/streams.rs:21` calls
  `hornvale_worldgen::streams::stream_labels()` via its full path (not a
  re-export); that path is unaffected since `stream_labels()` continues to
  live in this same `streams.rs` file, just generated by the macro now
  instead of hand-written.

- [ ] **Step 1: Replace `streams.rs`**

Current `windows/worldgen/src/streams.rs` (full file) is:

```rust
//! Seed-derivation labels owned by `windows/worldgen` itself (the
//! chorus/schema-selection and religion-naming streams — composition-
//! root concerns, not any one domain's). Save-format contracts; a
//! rename silently corrupts every world.

use hornvale_kernel::seed::StreamLabel;

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

/// Every seed-derivation label this crate itself owns (not any domain
/// crate's), for the generated stream manifest.
/// type-audit: bare-ok(artifact: return)
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        (SCHEMA.as_str(), "the folk causal-schema-selection sub-leg"),
        (SKY.as_str(), "the sky-domain fact-shape sub-leg"),
        (
            LEXEME.as_str(),
            "the lexicalization sub-leg for a chosen schema",
        ),
        (
            DOCTRINE_SCHEMA.as_str(),
            "the doctrine-voice twin of the folk schema-selection leg",
        ),
        (
            DOCTRINE_LEXEME.as_str(),
            "the doctrine-voice twin of the lexeme leg",
        ),
        (
            RELIGION_DEITY_V2.as_str(),
            "the deity-naming stream, epoch v2",
        ),
    ]
}
```

Replace it with:

```rust
//! Seed-derivation labels owned by `windows/worldgen` itself (the
//! chorus/schema-selection and religion-naming streams — composition-
//! root concerns, not any one domain's). Save-format contracts; a
//! rename silently corrupts every world.

hornvale_kernel::stream_labels! {
    /// The folk causal-schema-selection sub-leg, under a culture's own
    /// `hornvale_language::streams::ROOT` derivation.
    SCHEMA = "schema" => "the folk causal-schema-selection sub-leg";
    /// The sky-domain fact-shape sub-leg, under `SCHEMA`.
    SKY = "sky" => "the sky-domain fact-shape sub-leg";
    /// The lexicalization sub-leg for a chosen schema's rendered sentence.
    LEXEME = "lexeme" => "the lexicalization sub-leg for a chosen schema";
    /// The doctrine (institutional) causal-schema-selection sub-leg — the
    /// doctrine-voice twin of `SCHEMA`.
    DOCTRINE_SCHEMA = "doctrine-schema" => "the doctrine-voice twin of the folk schema-selection leg";
    /// The doctrine-voice twin of `LEXEME`.
    DOCTRINE_LEXEME = "doctrine-lexeme" => "the doctrine-voice twin of the lexeme leg";
    /// The deity-naming stream, epoch v2 (a full flat path, not a composed
    /// leg chain — matches `domains/climate`'s own `WEATHER_PHASE` pattern).
    RELIGION_DEITY_V2 = "religion/deity/v2" => "the deity-naming stream, epoch v2";
}
```

Note the doc comments for `SKY` and `DOCTRINE_SCHEMA` drop their
`` [`SCHEMA`] ``/`` [`LEXEME`] `` intra-doc link brackets (rustdoc link
syntax) in favor of plain `` `SCHEMA` ``/`` `LEXEME` `` — this is a purely
cosmetic simplification to avoid an unresolved-link warning risk from
`cargo doc` when the reference is inside a macro-expanded doc attribute;
it carries no semantic change.

- [ ] **Step 2: Confirm it compiles and the values are unchanged**

Run: `cargo build -p hornvale-worldgen && cargo test -p hornvale-worldgen`
Expected: no errors, all tests pass — in particular
`windows/worldgen/src/chorus.rs`'s 46 leg-calls (PROC-17's migration
target) are untouched by this task and must keep working identically,
since they reference these constants by name.

- [ ] **Step 3: Full gate, format, commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
make gate
git add windows/worldgen/src/streams.rs
git commit -m "refactor(worldgen): migrate streams.rs to stream_labels! (PROC-18)"
```

---

## Task 8: Migrate `domains/astronomy/src/streams.rs` (root+leg form)

**Files:**
- Modify: `domains/astronomy/src/streams.rs`
- Modify: `domains/astronomy/src/lib.rs:75-133` (delete the current
  `stream_labels()` free function; add a re-export)

**Interfaces:**
- Consumes: `hornvale_kernel::stream_labels!` (root+leg form, Task 1).
- Produces: `crate::stream_labels()` (via re-export) — unchanged path,
  used by `domains/astronomy/src/lib.rs:497-498`'s `Domain` trait impl
  (unmodified by this task).

This is the highest-value single migration: astronomy's current
`stream_labels()` is 21 hand-typed literal strings with **zero mechanical
connection** to the real constants (spec §4) — the macro removes that
disconnect entirely. It also deletes the `ALL_LABELS` array and its
cross-check test (spec §5.2): a macro-generated vec cannot diverge from
the constants it was generated from, so that test has nothing left to
usefully catch.

- [ ] **Step 1: Replace `streams.rs`**

Current `domains/astronomy/src/streams.rs` is the file read in full during
brainstorming — 21 `pub const` legs plus `ROOT`, followed by a
`#[cfg(test)] mod tests` block containing `ALL_LABELS` (a duplicate array
of all 21 non-root constants) and the
`every_stream_label_constant_is_published_in_stream_labels` test.

Replace the **entire file** with:

```rust
//! Seed-derivation labels for astronomy (permanent contracts, spec §3).
//! Declared via `stream_labels!` (PROC-18), which also generates
//! `stream_labels()` (moved here from `lib.rs`) — a constant and its
//! manifest row can no longer drift apart, so the `ALL_LABELS`
//! cross-check test this file used to carry (added after the STAR_AGE
//! incident, The Reckoning) is retired: there is nothing left for it to
//! catch.

hornvale_kernel::stream_labels! {
    /// Root stream label for astronomy.
    root: ROOT = "astronomy" => "root stream for sky genesis";
    legs {
        /// Star mass draw.
        STAR_MASS = "star-mass" => "main-sequence star mass draw";
        /// Anchor mass draw.
        ANCHOR_MASS = "anchor-mass" => "anchor world mass draw";
        /// Rotation regime/period draw.
        ROTATION = "rotation" => "rotation regime and period draw";
        /// Anchor orbital-distance draw.
        ORBIT = "orbit" => "anchor orbital distance draw";
        /// Obliquity draw.
        OBLIQUITY = "obliquity" => "axial tilt draw";
        /// Moon count draw.
        MOON_COUNT = "moon-count" => "how many moons";
        /// Per-moon parameter draws (one stream reused across attempts).
        MOONS = "moons" => "per-moon mass/distance draws (sequential attempts)";
        /// Neighborhood draws.
        NEIGHBORS = "neighbors" => "neighbor class/distance draws";
        /// Deep-time orbital-forcing draws (eccentricity + obliquity oscillation + precession).
        FORCING = "forcing" => "deep-time orbital forcing";
        /// Per-body genesis phase offsets (year, day, and each moon).
        PHASE_OFFSETS = "phase-offsets" => "per-body genesis phase offsets";
        /// Per-neighbor celestial position draws (declination, right ascension).
        NEIGHBOR_POSITIONS = "neighbor-positions" => "per-neighbor celestial position draws (declination, right ascension)";
        /// Spin-direction draw: prograde or retrograde (SKY-22).
        SPIN_DIRECTION = "spin-direction" => "spin-direction draw: prograde or retrograde";
        /// Per-moon orbital-inclination draws (SKY-6).
        MOON_INCLINATIONS = "moon-inclinations" => "per-moon orbital-inclination draws";
        /// Wanderer count draw.
        WANDERER_COUNT = "wanderer-count" => "how many wandering planets";
        /// Per-wanderer parameter draws, sequential (no attempts loop —
        /// `generate_wanderers` draws each wanderer's parameters once, in order).
        WANDERERS = "wanderers" => "per-wanderer parameter draws, sequential";
        /// Background starfield draws: count, then per-star position/brightness (derived catalog — consumed on demand, never in genesis).
        STARFIELD = "starfield" => "background starfield: count + per-star position/brightness (derived on demand)";
        /// Per-moon ascending-node longitude draws (Eclipse Seasons).
        MOON_NODES = "moon-nodes" => "per-moon ascending-node longitude draws";
        /// Stellar age draw (The Reckoning).
        STAR_AGE = "star-age" => "stellar age draw";
        /// Per-moon formation-mechanism draw (The Reckoning). Drawn after admission
        /// and the distance sort, so count/mass/distance stay byte-identical.
        MOON_FORMATION = "moon-formation" => "per-moon formation-mechanism draw (giant impact vs. capture)";
        /// Per-moon density draw (The Reckoning). Drawn after formation, one draw
        /// per moon in every branch — a `GiantImpact` moon's density is a derived
        /// constant, not a drawn one, but it still consumes a draw so that moon
        /// *i*'s density stream position never depends on how many earlier moons
        /// drew `Capture`.
        MOON_DENSITY = "moon-density" => "per-moon density draw (drawn only for captured moons; impact moons still consume it)";
        /// Per-moon age draw (The Reckoning). Drawn after formation, one draw per
        /// moon in every branch, for the same index-stability reason as
        /// `MOON_DENSITY`.
        MOON_AGE = "moon-age" => "per-moon age draw (impact: coeval jitter under the planet's age; capture: an independent fraction of it)";
    }
}
```

- [ ] **Step 2: Delete the old `stream_labels()` from `lib.rs` and re-export the macro-generated one**

In `domains/astronomy/src/lib.rs`, delete the entire block currently at
lines 75-133 (from `/// Every seed-derivation label this crate uses, with
docs...` through the closing `}` of `stream_labels()`).

Add, near `domains/astronomy/src/lib.rs`'s other `pub mod`/`pub use`
declarations (after the existing `pub mod streams;` line):

```rust
pub use streams::stream_labels;
```

- [ ] **Step 3: Confirm it compiles and every value is byte-identical**

Run: `cargo build -p hornvale-astronomy`
Expected: no errors.

Run: `cargo test -p hornvale-astronomy`
Expected: all tests pass, including `genesis_properties.rs`'s
pin-isolation tests (root `CLAUDE.md`: "a pin must consume the same draws
as the unpinned path"). A pass here is the direct proof this migration
changed only where each constant is declared, never its value or its
position in the `.derive()` chain.

- [ ] **Step 4: Confirm the manifest content is unchanged except formatting**

Run: `cargo run -p hornvale -- streams > /tmp/proc18-astronomy-manifest.txt`
Then inspect: every `astronomy/*` line must be byte-identical to what it
was before this task (same 22 rows — `ROOT` + 21 legs — same descriptions).
This is not yet the final manifest regen (Task 10 does that against the
committed file); this is a local sanity check specific to this crate.

- [ ] **Step 5: Full gate, format, commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
make gate
git add domains/astronomy/src/streams.rs domains/astronomy/src/lib.rs
git commit -m "refactor(astronomy): migrate to stream_labels! (PROC-18)

stream_labels() moves from lib.rs (21 hand-typed literal strings with
zero mechanical connection to the real constants) into streams.rs,
generated by the macro. Also retires ALL_LABELS and its cross-check
test — added after the STAR_AGE incident (The Reckoning) as a stopgap;
the macro makes that class of drift structurally impossible, so the
test has nothing left to catch."
```

---

## Task 9: Migrate `domains/terrain/src/streams.rs` (root+leg form, closes the live 5-entry gap)

**Files:**
- Modify: `domains/terrain/src/streams.rs`
- Modify: `domains/terrain/src/lib.rs:62-124` (delete the current
  `stream_labels()` free function; add a re-export)
- Modify: `domains/terrain/src/lib.rs:322-331` (the
  `stream_labels_are_fully_qualified_and_documented` test — its hardcoded
  count must change from 17 to 22)

**Interfaces:**
- Consumes: `hornvale_kernel::stream_labels!` (root+leg form, Task 1 —
  proven once already in Task 8).
- Produces: `crate::stream_labels()` (via re-export) — used by
  `domains/terrain/src/lib.rs:217-218`'s `Domain` trait impl (unmodified).

**This task closes the live gap identified during brainstorming** (spec
§1): `streams.rs` currently declares 22 constants (`ROOT` + 21 legs) but
`stream_labels()` publishes only 17 — `LOBING`, `CRUST_SLICE_0`,
`CRUST_SLICE_1`, `CRUST_SLICE_2`, and `RIFT_CRENULATION` are entirely
undocumented in the generated manifest today, with `make gate` green.
Because the macro requires every declared constant to appear in the same
list that generates `stream_labels()`, this migration cannot silently
carry the gap forward — all 21 legs must be listed, so the 5 missing ones
get real descriptions as part of this task, not as a follow-up.

- [ ] **Step 1: Replace `streams.rs`**

Current `domains/terrain/src/streams.rs` is the file read in full during
brainstorming: `ROOT` plus 21 legs (`PLATE_COUNT` through
`RIFT_CRENULATION`), no test block (unlike astronomy, terrain has no
`ALL_LABELS`-style cross-check — which is exactly why its gap went
uncaught).

Replace the **entire file** with:

```rust
//! Seed-derivation stream labels for tectonic genesis. Labels are permanent
//! save-format contracts; deliberate regeneration uses an epoch suffix
//! (`terrain/plate-count/v2`), never a rename.
//!
//! Retired labels (never reuse, never redraw): "plate-kind" (epoch v2 —
//! superseded by "cratons"; Crust spec §9).
//!
//! Declared via `stream_labels!` (PROC-18), which also generates
//! `stream_labels()` (moved here from `lib.rs`). This closed a live gap:
//! `stream_labels()` previously published only 17 of these 22 entries —
//! `LOBING`, `CRUST_SLICE_0`, `CRUST_SLICE_1`, `CRUST_SLICE_2`, and
//! `RIFT_CRENULATION` were undocumented in the generated manifest.

hornvale_kernel::stream_labels! {
    /// Root stream: every terrain chain hangs off `world_seed.derive(ROOT)`.
    root: ROOT = "terrain" => "root stream for tectonic genesis";
    legs {
        /// Plate count draw.
        PLATE_COUNT = "plate-count" => "how many plates";
        /// Per-plate seed-position draws (two `next_f64` per plate, sequential).
        PLATE_SEEDS = "plate-seeds" => "per-plate seed positions on the sphere";
        /// Per-plate Euler pole draws (axis then rate, sequential).
        PLATE_MOTION = "plate-motion" => "per-plate Euler pole axis and rate draws";
        /// Per-plate orogenic maturity draws.
        MATURITY = "maturity" => "per-plate orogenic maturity draws";
        /// Hotspot count, then per-hotspot position and strength draws.
        HOTSPOTS = "hotspots" => "hotspot count, positions, and strengths";
        /// Target ocean fraction draw.
        OCEAN_FRACTION = "ocean-fraction" => "target ocean fraction draw";
        /// Render-lens coastline noise. Hash-noise only — never consumed as a
        /// `Stream`; the lens draws nothing (Campaign 25 spec §3).
        COAST_RENDER = "coast-render" => "render-lens coastline noise (hash-noise only; no stream draws)";
        /// Craton draws: a margin draw (Task 9 iteration 3' reinterpretation —
        /// originally an independent area-budget fraction; now scales the
        /// ocean-fraction-derived budget instead, same single draw, see
        /// `crust::draw_cratons`'s doc), craton count (budget-scaled, no draw of
        /// its own), then per-craton center (two draws), radius, age —
        /// sequential. Draw counts are a save-format contract.
        CRATONS = "cratons" => "margin draw (scales the ocean-fraction-derived budget, Task 9 iteration 3'), craton count, then per-craton center/radius/age";
        /// Per-plate Voronoi weight draws (heavy-tailed; one per plate).
        PLATE_WEIGHTS = "plate-weights" => "per-plate heavy-tailed Voronoi weight draws";
        /// Plate-edge noise. Hash-noise only — never consumed as a `Stream`.
        PLATE_EDGE = "plate-edge" => "plate-edge noise (hash-noise only; no stream draws)";
        /// Lithology sub-cell patchiness noise (The Ground, spec §2). Hash-noise
        /// only — never consumed as a `Stream`; no draw-order/save-format contract.
        LITHOLOGY = "lithology" => "lithology sub-cell hash-noise (hash-noise only; no stream draws)";
        /// Terrane draws (Sculpting, spec §3): count, then per terrane a host
        /// craton index, rim bearing, size, age. A NEW label — existing stream
        /// consumption order is untouched (epoch v3 save-format contract).
        TERRANES = "terranes" => "terrane count, then per terrane host-craton index/bearing/size/age";
        /// Microcontinent draws (Sculpting, spec §3): a NEW label — a fixed
        /// candidate count (`crust::MICRO_COUNT_MAX`), then per candidate a
        /// position (two draws via `unit_vector`), a radius, and an age,
        /// sequential. Every candidate is drawn in full regardless of whether it
        /// survives the away-from-majors filter (no rejection loop, no
        /// draw-count variance — pin-isolation discipline, mirroring
        /// `TERRANES`'s own no-rejection framing for its bearing draws). Existing
        /// stream consumption order is untouched (epoch v3 save-format contract).
        MICROCONTINENTS = "microcontinents" => "fixed candidate count, then per candidate position/radius/age";
        /// Along-strike arc gating noise (Sculpting, spec §3): gates island-arc
        /// and coastal-range edifices into discrete chains instead of continuous
        /// walls. Hash-noise only — never consumed as a `Stream`, so it carries
        /// no draw-order/save-format contract (see `elevation::boundary_profile_m`).
        ARC_GATE = "arc-gate" => "along-strike island-arc gating noise (hash-noise only; no stream draws)";
        /// fBm relief noise (Sculpting, spec §3): zero-mean multi-octave detail
        /// added in `assemble_elevation`, amplitude scaled by induration and belt
        /// proximity (see `elevation::relief_scale`). Hash-noise only — never
        /// consumed as a `Stream`, so it carries no draw-order/save-format
        /// contract, mirroring `ARC_GATE`.
        RELIEF = "relief" => "fBm relief-detail noise (hash-noise only; no stream draws)";
        /// Rift draws (rift-and-fit, spec §3): ONE global spreading-rate draw.
        /// Per-seam geometry uses hash sub-derivations (`seam-{a}-{b}`) — no
        /// sequential draws, no draw-count variance. A NEW label — epoch v4
        /// save-format contract.
        RIFT = "rift" => "ONE spreading-rate draw; per-seam geometry via hash sub-derivations (seam-{a}-{b}); no other sequential draws";
        /// The lobing-noise sub-leg under a craton's own stream (spec: two
        /// production call sites, `crust.rs:294` and `crust.rs:986`, share this
        /// exact literal today).
        LOBING = "lobing" => "craton lobing-noise sub-leg (hash-noise only; no stream draws)";
        /// The first of three orthogonal noise slices shared between crust
        /// sculpting and its render lens (`crust.rs` and `render.rs`).
        CRUST_SLICE_0 = "slice-0" => "first of three orthogonal crust noise slices, shared with the render lens (hash-noise only; no stream draws)";
        /// The second of three orthogonal noise slices (see `CRUST_SLICE_0`).
        CRUST_SLICE_1 = "slice-1" => "second of three orthogonal crust noise slices (hash-noise only; no stream draws)";
        /// The third of three orthogonal noise slices (see `CRUST_SLICE_0`).
        CRUST_SLICE_2 = "slice-2" => "third of three orthogonal crust noise slices (hash-noise only; no stream draws)";
        /// The rift's crenulation-noise sub-leg.
        RIFT_CRENULATION = "crenulation" => "rift crenulation-noise sub-leg (hash-noise only; no stream draws)";
    }
}
```

- [ ] **Step 2: Delete the old `stream_labels()` from `lib.rs` and re-export the macro-generated one**

In `domains/terrain/src/lib.rs`, delete the entire block currently at
lines 62-124 (from `/// Every seed-derivation label this crate uses, with
docs...` through the closing `}` of `stream_labels()`).

Add, near `domains/terrain/src/lib.rs`'s other `pub mod`/`pub use`
declarations (after the existing `pub mod streams;` line):

```rust
pub use streams::stream_labels;
```

- [ ] **Step 3: Update the hardcoded-count test**

`domains/terrain/src/lib.rs`'s existing test (currently at lines 322-331)
is:

```rust
    #[test]
    fn stream_labels_are_fully_qualified_and_documented() {
        let labels = stream_labels();
        assert_eq!(labels.len(), 17);
        assert_eq!(labels[0].0, "terrain");
        for (label, doc) in &labels[1..] {
            assert!(label.starts_with("terrain/"), "unqualified label {label}");
            assert!(!doc.is_empty());
        }
    }
```

Change `assert_eq!(labels.len(), 17);` to `assert_eq!(labels.len(), 22);`
(21 legs + `ROOT`, now that the migration has closed the 5-entry gap). No
other line in this test changes — the `starts_with("terrain/")` and
`!doc.is_empty()` checks already generalize to every entry, including the
5 newly-published ones.

- [ ] **Step 4: Run the updated test to verify it fails, then passes**

Run: `cargo test -p hornvale-terrain stream_labels_are_fully_qualified_and_documented`

Before Step 1/Step 3 (hypothetically, if only the count assertion were
changed without the streams.rs migration): FAIL, `left == 17, right == 22`.

After Steps 1-3: `test result: ok. 1 passed`.

- [ ] **Step 5: Confirm every other test still passes and values are byte-identical**

Run: `cargo test -p hornvale-terrain`
Expected: all tests pass, including `tectonic_properties.rs`'s
pin-isolation tests (root `CLAUDE.md`: stream consumption order is a
contract) and `carve_properties.rs`. A pass proves every one of the 22
constants' values is unchanged — this migration only changed
*declaration site* and *manifest completeness*, never any `.derive()`
call's actual argument.

- [ ] **Step 6: Confirm the manifest now shows all 22 rows**

Run: `cargo run -p hornvale -- streams | grep '^terrain'`
Expected: 22 lines (previously 17) — `terrain`, `terrain/plate-count`, ...,
`terrain/lobing`, `terrain/slice-0`, `terrain/slice-1`, `terrain/slice-2`,
`terrain/crenulation` all present.

- [ ] **Step 7: Full gate, format, commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
make gate
git add domains/terrain/src/streams.rs domains/terrain/src/lib.rs
git commit -m "refactor(terrain): migrate to stream_labels!, closing a live gap (PROC-18)

stream_labels() previously published only 17 of 22 real constants —
LOBING, CRUST_SLICE_0/1/2, and RIFT_CRENULATION were undocumented in
the generated manifest, with make gate green throughout (terrain, unlike
astronomy, had no ALL_LABELS-style cross-check test to catch it). The
macro's compile-time fusion closes this as a forced side effect: every
declared constant must appear in the same list that generates
stream_labels(), so the gap could not be carried forward silently."
```

---

## Task 10: Final integration — manifest regen, type-audit report regen, gate, sweep

**Files:**
- Modify: `book/src/reference/stream-manifest-generated.md` (regenerated)
- Modify: `docs/audits/type-audit-report.md` (regenerated)

**Interfaces:**
- Consumes: every constant and `stream_labels()` from Tasks 2-9.
- Produces: nothing new — this task only verifies and freshens generated
  artifacts.

- [ ] **Step 1: Regenerate the stream manifest and inspect the diff**

Run:
```bash
cargo run -p hornvale -- streams > /tmp/proc18-manifest-check.txt
git diff book/src/reference/stream-manifest-generated.md
```

If the committed page is stale (it likely is — Tasks 2-9 changed several
crates' outputs), regenerate it properly:

```bash
SKIP_CENSUS=1 bash scripts/regenerate-artifacts.sh
git diff book/src/reference/stream-manifest-generated.md
```

Expected diff content: **only** terrain's 5 newly-published rows
(`terrain/lobing`, `terrain/slice-0`, `terrain/slice-1`, `terrain/slice-2`,
`terrain/crenulation`) and climate's 1 newly-published row
(`climate/weather/phase/v1`) should appear as genuinely NEW lines. Every
other line (astronomy's 22, terrain's original 17, chronicle's 7,
locale's 4, vessel's 2, worldgen's 6, kernel's 2) must be byte-identical
to what was already committed — if any of those differ, a migration task
introduced a value change and must be investigated before proceeding
(Global Constraints: no value changes anywhere in this campaign).

- [ ] **Step 2: Regenerate the type-audit report**

Every migrated constant's `/// type-audit: bare-ok(identifier-text:
return)` tag line is gone (spec §5.2 — the raw-token scan can no longer
see a `StreamLabel::from_static` call once it's inside the macro
invocation). This shrinks the committed report.

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
git diff docs/audits/type-audit-report.md
```

Expected: `check` exits 0 (no untagged-primitive failures — every
migrated item is a `StreamLabel`, a newtype, never a bare primitive, so
none of them were ever taggable). The report's diff should show entries
for `ROOM_FACE`, `ROOM_CHILD`, `WEATHER_PHASE`, and every astronomy/
terrain/chronicle/locale/vessel/worldgen `StreamLabel` constant's tag line
disappearing; `OCTAVE_LABELS`'s tag (untouched, Task 2) must still be
present.

- [ ] **Step 3: Workspace-wide sweep — confirm no crate outside `domains/language` still hand-authors both a `StreamLabel` const and a separate manifest entry**

```bash
grep -rn 'pub const.*StreamLabel<.static> = StreamLabel::from_static' \
  --include='*.rs' domains windows kernel \
  | grep -v 'domains/language' \
  | grep -v 'kernel/src/streams.rs' # OCTAVE_LABELS's own array elements
```

Expected: no output (or only `OCTAVE_LABELS`'s 16 internal array-element
lines in `kernel/src/streams.rs`, which are excluded above and are the
one deliberate exception per Global Constraints). Every other crate's
`StreamLabel` constants are now declared exclusively through
`stream_labels!` invocations, which this grep pattern does not match
(the macro invocation's own body reads `NAME = "value" => "desc";`, not
`StreamLabel::from_static(...)`).

Also confirm `domains/language` is untouched:

```bash
git diff --stat domains/language/
```

Expected: no output (zero changes to this crate across the whole
campaign, per Global Constraints).

- [ ] **Step 4: Full workspace gate**

```bash
make gate
```

Expected: fmt, clippy, type-audit, nextest, and doctests all pass. This is
the final proof that every one of the 8 migrated crates compiles, tests
green, and carries no stale tag or lint violation.

- [ ] **Step 5: Commit the regenerated artifacts**

```bash
git add book/src/reference/stream-manifest-generated.md docs/audits/type-audit-report.md
git commit -m "chore(proc-18): regenerate stream manifest and type-audit report

Manifest gains terrain's 5 previously-undocumented rows and climate's
WEATHER_PHASE row (both live gaps this campaign closed). Type-audit
report shrinks — every migrated crate's per-constant tag boilerplate is
gone, since the check can no longer see a bare StreamLabel::from_static
call once it's inside a stream_labels! invocation."
```
