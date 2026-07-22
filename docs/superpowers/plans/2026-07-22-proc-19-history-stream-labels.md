# PROC-19: Third `stream_labels!` Arm + `domains/history` Migration Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a third arm to `hornvale_kernel::stream_labels!` (`kernel/src/seed.rs`) supporting a root+leg group combined with independent flat entries in one invocation, then migrate `domains/history` to it.

**Architecture:** One new `macro_rules!` arm, placed before the existing root+leg-only arm, that declares a root, its legs (manifest-qualified via `concat!`, exactly like the existing root+leg arm), and one or more flat entries (published under their own complete literal, exactly like the existing flat arm) — all combined into one generated `stream_labels()`. The existing two arms are untouched.

**Tech Stack:** Rust `macro_rules!`, the `std`-builtin `concat!` macro (already used by the existing root+leg arm, no new dependency).

**Full spec:** `docs/superpowers/specs/2026-07-22-proc-19-history-stream-labels-design.md`

## Global Constraints

- `stream_labels()`'s return type (`Vec<(&'static str, &'static str)>`) never changes.
- No epoch bump — every one of `domains/history`'s six constants'
  `.as_str()` value must be byte-identical before and after this
  campaign. This is a pure declaration-site refactor.
- The existing two macro arms (flat-only; root+leg-only, both shipped in
  PROC-18) must not be modified in any way — only a new third arm is
  added to the `macro_rules!` block.
- In the new arm, **flat entries use their own complete literal via
  `.as_str()`, exactly like the plain flat-only arm** — they must NEVER
  be passed through `concat!` with the root. Only `legs {}` entries get
  the root-prefix composition.
- `make gate` already runs `tools/type-audit -- check` locally (Makefile:
  `gate: fmt-check clippy type-audit test`) — it is not a CI-only,
  later-only step.
- `domains/history` is the ONLY crate this campaign touches.

---

## Task 1: The third `stream_labels!` arm

**Files:**
- Modify: `kernel/src/seed.rs` (insert a new arm at line 119, before the
  existing root+leg-only arm's `(` — i.e. the new arm becomes the FIRST
  arm tried; the current root+leg-only arm, currently starting at line
  119, and the flat-only arm, currently starting at line 148, both shift
  down but are otherwise untouched)
- Modify: `kernel/src/seed.rs`'s `stream_labels_macro_tests` module
  (append a new nested test module after `root_and_leg`, currently ending
  at line 456)

**Interfaces:**
- Produces: a third invocation shape for
  `hornvale_kernel::stream_labels!` — `root: NAME = "value" => "desc";
  legs { ... } flat { ... }` — used by Task 2.

This task is purely additive: nothing outside `kernel/src/seed.rs`
changes, and the two existing arms are not touched, so there is zero risk
to any of the 8 crates PROC-18 already migrated.

- [ ] **Step 1: Insert the new arm**

In `kernel/src/seed.rs`, find this exact block (currently starting at
line 117):

```rust
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
```

Insert a new arm immediately after the opening `macro_rules! stream_labels {` line and before this existing root+leg arm, so the block reads:

```rust
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
        flat {
            $(
                $(#[$flat_meta:meta])*
                $flat_name:ident = $flat_value:literal => $flat_desc:literal;
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
        $(
            $(#[$flat_meta])*
            pub const $flat_name: $crate::seed::StreamLabel<'static> =
                $crate::seed::StreamLabel::from_static($flat_value);
        )+

        /// Every seed-derivation label this crate uses, for the generated
        /// stream manifest (PROC-19: a crate whose labels don't all
        /// share one root — some legs compose under the root via
        /// `concat!`, some flat entries are independent roots of their
        /// own, drawn at a different call site entirely).
        pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
            vec![
                ($root_name.as_str(), $root_desc),
                $( (concat!($root_value, "/", $leg_value), $leg_desc) ),+ ,
                $( ($flat_name.as_str(), $flat_desc) ),+
            ]
        }
    };
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
```

(The rest of the file — the existing root+leg arm's body, the existing
flat-only arm, and everything after — is unchanged. Only the new arm
above was inserted, and it is a complete, independent match arm ending in
its own `};`.)

- [ ] **Step 2: Write the failing tests**

In `kernel/src/seed.rs`'s `stream_labels_macro_tests` module, add a new
nested module after `root_and_leg` (which currently ends at line 456,
just before the module's own closing `}` at line 457):

```rust

    mod root_leg_and_flat {
        crate::stream_labels! {
            /// Root stream label for the test crate.
            root: ROOT = "testcrate" => "root stream for the test crate";
            legs {
                /// A test-only leg.
                EPSILON = "epsilon" => "the epsilon test leg";
            }
            flat {
                /// A test-only flat, independent-root entry.
                ZETA = "testcrate/zeta" => "the zeta test entry";
                /// A second test-only flat, independent-root entry.
                ETA = "testcrate/eta" => "the eta test entry";
            }
        }

        #[test]
        fn every_constant_keeps_its_own_bare_value() {
            // The real .derive() call chain uses these bare values.
            // Flat entries are NOT children of ROOT in the derivation
            // sense — they must never be composed with it.
            assert_eq!(ROOT.as_str(), "testcrate");
            assert_eq!(EPSILON.as_str(), "epsilon");
            assert_eq!(ZETA.as_str(), "testcrate/zeta");
            assert_eq!(ETA.as_str(), "testcrate/eta");
        }

        #[test]
        fn manifest_qualifies_legs_under_root_but_not_flat_entries() {
            assert_eq!(
                stream_labels(),
                vec![
                    ("testcrate", "root stream for the test crate"),
                    ("testcrate/epsilon", "the epsilon test leg"),
                    ("testcrate/zeta", "the zeta test entry"),
                    ("testcrate/eta", "the eta test entry"),
                ]
            );
        }
    }
```

- [ ] **Step 3: Run the tests**

Run: `cargo test -p hornvale-kernel stream_labels_macro_tests`
Expected: `test result: ok. 6 passed; 0 failed` (the existing 4 tests from
the `flat` and `root_and_leg` modules, plus the 2 new ones in
`root_leg_and_flat`).

- [ ] **Step 4: Run the full kernel test suite**

Run: `cargo test -p hornvale-kernel`
Expected: all existing tests still pass (this task only inserted a new
match arm and new tests — nothing about `Seed::derive`, `StreamLabel`, or
either existing macro arm changed).

- [ ] **Step 5: Format, lint, commit**

```bash
cargo fmt
cargo clippy -p hornvale-kernel --all-targets -- -D warnings
git add kernel/src/seed.rs
git commit -m "feat(kernel): stream_labels! third arm — root+legs+flat (PROC-19)

Supports a crate whose streams.rs mixes a genuine root+leg group with
independent, already-fully-qualified flat entries in one file (surfaced
by domains/history, which PROC-18 left out of scope for exactly this
reason). Purely additive — the existing flat-only and root+leg-only
arms are untouched."
```

---

## Task 2: Migrate `domains/history` to the new arm

**Files:**
- Modify: `domains/history/src/streams.rs`
- Modify: `domains/history/src/lib.rs` (delete the `stream_labels()` free
  function currently at lines 119-160; add a re-export)

**Interfaces:**
- Consumes: `hornvale_kernel::stream_labels!` (the new root+legs+flat
  form, Task 1).
- Produces: `crate::stream_labels()` (via re-export) — unchanged path,
  used by `domains/history/src/lib.rs`'s `Domain` trait impl (`fn
  stream_labels(&self) -> Vec<(&'static str, &'static str)> {
  crate::stream_labels() }`, currently at lines 176-178 — this block is
  NOT modified by this task).

- [ ] **Step 1: Replace `domains/history/src/streams.rs`**

Current file (full):

```rust
//! Seed-derivation labels for history (permanent contracts, spec §3). This
//! crate is the pure data model plus the local flesh derivations (Task 2);
//! the deep-history bake that actually draws from a `Seed` runs at the
//! composition root (`windows/worldgen`, per decision #6 in the campaign's
//! decision ledger — a domain may depend on nothing but the kernel, so the
//! cross-domain bake cannot live here). Sub-labels for the bake's own draws
//! are documented here as they are implemented (Task 3 onward), the same way
//! this crate's flesh derivations draw against `history/flesh/*`.

use hornvale_kernel::seed::StreamLabel;

/// Root stream label for history — reserved for the deep-history bake
/// (Task 3). The composition root derives `history/flesh` once per
/// occupation before calling into this crate's flesh derivations (Task 2),
/// which further derive their own sub-labels ([`RESIDUE`], [`STRUCTURES`])
/// from that already-scoped seed.
/// type-audit: bare-ok(identifier-text: return)
pub const ROOT: StreamLabel<'static> = StreamLabel::from_static("history");
/// Sub-label for `flesh::residue_of`'s deterministic flavor draws (Task 2).
/// type-audit: bare-ok(identifier-text: return)
pub const RESIDUE: StreamLabel<'static> = StreamLabel::from_static("residue");
/// Sub-label for `flesh::structures_of`'s dwelling-count variance draws
/// (Task 2).
/// type-audit: bare-ok(identifier-text: return)
pub const STRUCTURES: StreamLabel<'static> = StreamLabel::from_static("structures");
/// Root stream label for the deep-history bake's epoch dynamics (Task 3):
/// grow/found/migrate/raid/collapse draws, taken sequentially from one
/// stream in commit order at the composition root
/// (`windows/worldgen::history_bake::bake`).
/// type-audit: bare-ok(identifier-text: return)
pub const BAKE: StreamLabel<'static> = StreamLabel::from_static("history/bake");
/// Root stream label for the deep-history bake's genesis draws (Task 3):
/// how many proto-communities a people seeds with, which sites they take,
/// and their tech-advance offset. The bake further derives a per-people
/// sub-stream `history/genesis/<people-kind>` from this label (a
/// `StreamLabel::dynamic(people.0)` derive), one per entry in the bake's
/// `peoples` list, so each people's genesis draws are independent of draw
/// order across peoples.
/// type-audit: bare-ok(identifier-text: return)
pub const GENESIS: StreamLabel<'static> = StreamLabel::from_static("history/genesis");
/// The per-occupation flesh seed the legibility surface derives before
/// expanding residue/structures on demand: `seed.derive(FLESH).derive(
/// StreamLabel::dynamic(&entity_id))`. Flesh is never committed, so this
/// label scopes only on-demand rendering, but it is a permanent derivation
/// contract and is declared here like every other.
/// type-audit: bare-ok(identifier-text: return)
pub const FLESH: StreamLabel<'static> = StreamLabel::from_static("history/flesh");
```

Replace it with:

```rust
//! Seed-derivation labels for history (permanent contracts, spec §3). This
//! crate is the pure data model plus the local flesh derivations; the
//! deep-history bake that actually draws from a `Seed` runs at the
//! composition root (`windows/worldgen`, per decision #6 in the campaign's
//! decision ledger — a domain may depend on nothing but the kernel, so the
//! cross-domain bake cannot live here). Sub-labels for the bake's own draws
//! are documented here, the same way this crate's flesh derivations draw
//! against `history/flesh/*`.
//!
//! Declared via `stream_labels!`'s root+legs+flat form (PROC-19): `ROOT`/
//! `RESIDUE`/`STRUCTURES` are a genuine root+leg group (real `.derive()`
//! chains compose them), while `BAKE`/`GENESIS`/`FLESH` are independent,
//! already-fully-qualified roots of their own — the bake that draws from
//! them runs at a different call site entirely and never chains off this
//! crate's `ROOT`.

hornvale_kernel::stream_labels! {
    /// Root stream label for history — reserved for the deep-history bake.
    /// The composition root derives `history/flesh` once per occupation
    /// before calling into this crate's flesh derivations, which further
    /// derive their own sub-labels (`RESIDUE`, `STRUCTURES`) from that
    /// already-scoped seed.
    root: ROOT = "history" => "root stream for history: reserved for the deep-history bake (run at the composition root); no draw is made against it directly";
    legs {
        /// Sub-label for `flesh::residue_of`'s deterministic flavor draws.
        RESIDUE = "residue" => "flesh::residue_of's deterministic flavor draws";
        /// Sub-label for `flesh::structures_of`'s dwelling-count variance draws.
        STRUCTURES = "structures" => "flesh::structures_of's dwelling-count variance draws";
    }
    flat {
        /// Root stream label for the deep-history bake's epoch dynamics:
        /// grow/found/migrate/raid/collapse draws, taken sequentially from
        /// one stream in commit order at the composition root
        /// (`windows/worldgen::history_bake::bake`).
        BAKE = "history/bake" => "the deep-history bake's epoch dynamics: grow/found/migrate/raid/collapse draws, taken sequentially from one stream in commit order at the composition root";
        /// Root stream label for the deep-history bake's genesis draws:
        /// how many proto-communities a people seeds with, which sites
        /// they take, and their tech-advance offset. The bake further
        /// derives a per-people sub-stream `history/genesis/<people-kind>`
        /// from this label (a `StreamLabel::dynamic(people.0)` derive),
        /// one per entry in the bake's `peoples` list, so each people's
        /// genesis draws are independent of draw order across peoples.
        GENESIS = "history/genesis" => "the deep-history bake's genesis draws: proto-community count, site picks, and tech-advance offset; further derives a per-people sub-stream history/genesis/<people-kind> via StreamLabel::dynamic";
        /// The per-occupation flesh seed the legibility surface derives
        /// before expanding residue/structures on demand:
        /// `seed.derive(FLESH).derive(StreamLabel::dynamic(&entity_id))`.
        /// Flesh is never committed, so this label scopes only on-demand
        /// rendering, but it is a permanent derivation contract and is
        /// declared here like every other.
        FLESH = "history/flesh" => "the per-occupation flesh seed the legibility surface derives before expanding residue/structures on demand (never committed)";
    }
}
```

- [ ] **Step 2: Delete the old `stream_labels()` from `lib.rs` and re-export the macro-generated one**

In `domains/history/src/lib.rs`, delete the entire block currently at
lines 119-160 (from `/// Every seed-derivation label this crate
documents...` through the closing `}` of `stream_labels()`).

Add, right after the existing `pub mod streams;` line (currently line
16):

```rust
pub use streams::stream_labels;
```

Do **not** modify the `Domain` trait impl (`impl hornvale_kernel::Domain
for History { ... fn stream_labels(&self) -> Vec<(&'static str,
&'static str)> { crate::stream_labels() } }`, currently at lines
162-179) — it keeps working unchanged via the new re-export.

- [ ] **Step 3: Confirm it compiles and every value is byte-identical**

Run: `cargo build -p hornvale-history`
Expected: no errors.

Run: `cargo test -p hornvale-history`
Expected: all tests pass, including `stream_labels_declare_the_root`
(which calls the bare `stream_labels()` function — now resolved through
the re-export) and every `flesh::residue_of`/`structures_of` test. A pass
proves `RESIDUE`/`STRUCTURES`/`BAKE`/`GENESIS`/`FLESH`'s values are
unchanged.

- [ ] **Step 4: Confirm the deep-history bake's tests still pass**

The deep-history bake (`windows/worldgen`) draws from `BAKE`, `GENESIS`,
and `FLESH` via `.derive()` at its own call site — this is this task's
real determinism proof, since `domains/history`'s own tests don't
exercise the bake itself.

Run: `cargo test -p hornvale-worldgen --test history_bake --test history_emit --test history_byte_identity --test history_gates --test history_placement`
Expected: all tests pass, in particular `history_byte_identity.rs` (a
byte-identity test across two builds) — a pass is direct proof this
migration didn't perturb any of the bake's actual derived values.

- [ ] **Step 5: Confirm the manifest has zero content diff**

This is a pure declaration-site move, unlike PROC-18's terrain/climate
tasks — `domains/history`'s manifest was already complete and correct
before this task (spec §1), so there should be no new or changed rows.

Run:
```bash
cargo run -p hornvale -- streams > /tmp/proc19-manifest-check.txt
diff <(grep '^history' /tmp/proc19-manifest-check.txt) <(grep '^history' book/src/reference/stream-manifest-generated.md)
```
Expected: no diff (both should show the same 6 `history`/`history/*`
rows with unchanged descriptions). If `make gate`'s artifact-freshness
step or a manual regen changes anything else in that file, investigate —
any diff outside the `history/*` rows means something else drifted.

- [ ] **Step 6: Full gate, format, commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
make gate
git add domains/history/src/streams.rs domains/history/src/lib.rs
git commit -m "refactor(history): migrate to stream_labels! (PROC-19)

Uses the new root+legs+flat arm (Task 1): ROOT/RESIDUE/STRUCTURES are a
genuine root+leg group; BAKE/GENESIS/FLESH are independent,
already-fully-qualified roots of their own (drawn at a different call
site, windows/worldgen's composition root, never chained off this
crate's ROOT). Pure declaration-site move — every value unchanged,
manifest content unchanged."
```
