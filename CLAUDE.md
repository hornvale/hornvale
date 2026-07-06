# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

Hornvale is a deterministic, multiscalar world simulation observed through
text — "sim first, game as lens." The governing documents are the spec
(`docs/superpowers/specs/2026-07-05-hornvale-longterm-plan-design.md`, the
Constitution especially) and the project book (`book/`, published at
hornvale.github.io/hornvale). When this file and the spec disagree, the spec
governs.

## Commands

```bash
# The full gate — every commit must pass all three:
cargo test --workspace
cargo fmt --check
cargo clippy --workspace --all-targets -- -D warnings

# Single test / single crate / the astronomy property battery:
cargo test -p hornvale-kernel text_of
cargo test -p hornvale-astronomy --test genesis_properties

# The CLI (crate `hornvale` in cli/):
cargo run -p hornvale -- new --seed 42 --out world.json
cargo run -p hornvale -- repl --world world.json
cargo run -p hornvale -- almanac --world world.json
cargo run -p hornvale -- concepts        # registry dump (book reference page)
cargo run -p hornvale -- streams         # stream manifest (book reference page)

# Generated-artifact freshness (CI fails if these drift from committed copies):
cargo run -p hornvale-kernel --example first_light
cargo run -p hornvale -- new --seed 42 --out /tmp/hv.json
cargo run -p hornvale -- almanac --world /tmp/hv.json > book/src/gallery/almanac-seed-42.md
cargo run -p hornvale -- concepts > book/src/reference/concept-registry-generated.md
cargo run -p hornvale -- streams > book/src/reference/stream-manifest-generated.md
git diff --exit-code book/src/gallery/ book/src/reference/

# The project book:
mdbook build book          # or `mdbook serve book` to preview
```

## Architecture

**Layering (constitutional, enforced via Cargo.toml):**
`kernel/` → `domains/*` → `windows/*` → `cli/`.
A domain crate depends on `hornvale-kernel` and **nothing else** — never
another domain. Windows (`windows/almanac`) may depend on domains because
they present them. `cli/src/world_builder.rs` is the **composition root**:
the only file where all domains meet, and the only place providers
(astronomy/climate implementations) are constructed. Adding a domain must
never require editing an existing one.

**A world is a seed plus a ledger.** `World { seed, registry, ledger }`
serializes to JSON; everything else is re-derived deterministically.
Cross-domain communication uses only the kernel's trace protocol:
- **Facts** — subject/predicate/object envelope, append-only, contradiction-
  checked against the concept registry (predicates registered per domain;
  naming conventions are in the book's concept-registry chapter).
- **Phenomena** — the universal read: salience-ranked observations. Consumers
  (e.g. religion) must never learn which system produced a phenomenon.
- **Fields** — typed functions over (space × time), the statistical prior.

**Provider tiers coexist:** the tier-0 `ConstantSun` and the generated star
system are both valid; worlds choose. Higher fidelity refines, never
contradicts, lower ("coarse constrains fine").

## Determinism (constitutional — most bugs here are catastrophic)

- Same seed + same pins → byte-identical worlds, almanacs, and artifacts.
  Tests assert this; CI's drift check enforces it on committed artifacts.
- **No wall-clock time anywhere** (`std::time` is forbidden in kernel and
  domain code). Time is `WorldTime { day: f64 }` — absolute standard days.
- No `HashMap` in anything ordered or serialized — `BTreeMap`/`Vec` only.
  Float sorting uses `total_cmp` with deterministic tie-breaks.
- **Save-format contracts** (changing any silently corrupts every world):
  seed-derivation labels (declared as constants in each crate's `streams`
  module, published via `stream_labels()` into the generated manifest),
  **stream consumption order** (a pin must consume the same draws as the
  unpinned path — see the pin-isolation tests in
  `domains/astronomy/tests/genesis_properties.rs`), the hash/noise constants
  in `kernel/src/seed.rs` and `noise.rs`, and the physics formulas in
  `domains/astronomy` (the spec's model card lists derived vs approximated
  vs drawn). Deliberate regeneration uses an epoch suffix
  (`settlement/name/v2`), never a rename.
- Pins fail loudly (`GenesisError` with the physical reason); generation
  never retries across seeds — the seed is a world's identity.

## Constraints and conventions

- Dependencies: `serde` + `serde_json` only, workspace-wide. No new crates
  (no rand, chrono, clap, thiserror — randomness comes from the kernel's
  `Seed`/`Stream`, CLI parsing is std-only).
- Every crate sets `#![warn(missing_docs)]`; every public item, field, and
  variant gets a one-line doc comment.
- Rust edition 2024. Run `cargo fmt` as the final step before every commit —
  fmt-gate skips have been the most common review finding.
- **Typed quantities:** coherent physical units crossing API boundaries are
  hand-rolled newtypes with validating constructors and named conversions
  (`Au`, `Mm`, `LightYears`, `SolarMasses`, `StdDays`, `LocalDays`, …);
  dimensionless ratios stay bare `f64`. No dimensional-analysis crates.
  Rationale and scope: Campaign 2 spec, design principle 5.
- Ratified decisions (recorded in plan Self-Review Notes — do not
  relitigate): `Fact.day` stays a bare `Option<f64>`; `PredicateDef.name`
  duplicates its registry key.

## Process

Work proceeds in campaigns: spec (`docs/superpowers/specs/`) → implementation
plan (`docs/superpowers/plans/`) → execution → merge. **Definition of Done
for every merged plan includes the project book**: a chronicle entry
(`book/src/chronicle/`) and a freshness sweep of stale chapters — the book
may never lag merged reality. Book prose is written at a deliberate
altitude: technical and mathematical, comprehensible without reading the
code it may show.
