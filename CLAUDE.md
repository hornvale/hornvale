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

# Single test / single crate / the property batteries:
cargo test -p hornvale-kernel text_of
cargo test -p hornvale-astronomy --test genesis_properties
cargo test -p hornvale-terrain --test tectonic_properties

# The CLI (crate `hornvale` in cli/; `hornvale help` lists every flag):
cargo run -p hornvale -- new --seed 42 --out world.json   # plus sky pins (--sky,
                                         # --moons, --rotation, --neighbor, …) and
                                         # terrain pins (--plates, --ocean-fraction,
                                         # --supercontinent)
cargo run -p hornvale -- scout --neighbor red-giant       # scan seeds satisfying pins
cargo run -p hornvale -- repl --world world.json
cargo run -p hornvale -- almanac --world world.json
cargo run -p hornvale -- map --world world.json --out elevation.ppm
cargo run -p hornvale -- concepts        # registry dump (book reference page)
cargo run -p hornvale -- streams         # stream manifest (book reference page)
cargo run -p hornvale -- lab run studies/census-lands-drift.study.json
cargo run -p hornvale -- lab list-metrics

# The type audit — a standalone tool OUTSIDE the workspace (decisions
# `non-workspace-dev-tools-may-use-parser-libraries` / `the-bare-ok-rubric`);
# check is default-deny (any untagged pub-boundary primitive fails), report
# regenerates the committed audit report:
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md

# Generated-artifact freshness: CI regenerates every committed artifact
# (three seed-42 almanacs, the elevation map, registry/manifest dumps, lab
# studies) and fails on drift. The authoritative command list is the
# "Artifacts are current" step in .github/workflows/ci.yml; the shape:
cargo run -p hornvale-kernel --example first_light
cargo run -p hornvale -- new --seed 42 --out /tmp/hv.json
cargo run -p hornvale -- almanac --world /tmp/hv.json > book/src/gallery/almanac-seed-42-sky.md
cargo run -p hornvale -- lab run studies/census-lands-drift.study.json
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/

# The project book:
mdbook build book          # or `mdbook serve book` to preview
```

## Architecture

**Layering (constitutional, enforced by `cli/tests/architecture.rs`):**
`kernel/` → `domains/*` → `windows/*` → `cli/`.
A domain crate depends on `hornvale-kernel` and **nothing else** — never
another domain. Windows (`windows/almanac`) may depend on domains because
they present them (and a window may depend on another window — `windows/lab`
builds worlds through `windows/worldgen`). `windows/worldgen` (crate
`hornvale-worldgen`) is the **composition root**: the library where all
domains meet, and the only place providers (astronomy/climate/terrain
implementations) are constructed. The CLI and every window build worlds
through it (`cli/` re-exports it). Adding a domain must never require
editing an existing one.

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
- **No wall-clock time anywhere**. Time is `WorldTime { day: f64 }` —
  absolute standard days.
- No `HashMap`/`HashSet` — `BTreeMap`/`BTreeSet`/`Vec` only. Float sorting
  uses `total_cmp` with deterministic tie-breaks. (This ban and the
  wall-clock one are enforced workspace-wide by `clippy.toml`
  `disallowed-types`; a justified exception gets a scoped
  `#[allow(clippy::disallowed_types)]` with a comment.)
- **Save-format contracts** (changing any silently corrupts every world):
  seed-derivation labels (declared as constants in each crate's `streams`
  module, published via `stream_labels()` into the generated manifest),
  **stream consumption order** (a pin must consume the same draws as the
  unpinned path — see the pin-isolation tests in
  `domains/astronomy/tests/genesis_properties.rs` and
  `domains/terrain/tests/tectonic_properties.rs`), the hash/noise constants
  in `kernel/src/seed.rs` and `noise.rs`, and the physics formulas in
  `domains/astronomy` (the spec's model card lists derived vs approximated
  vs drawn). Deliberate regeneration uses an epoch suffix
  (`settlement/name/v2`), never a rename.
- Pins fail loudly (`GenesisError` with the physical reason); generation
  never retries across seeds — the seed is a world's identity.

## Constraints and conventions

- Dependencies: `serde` + `serde_json` only, workspace-wide (allowlist
  enforced by `cli/tests/architecture.rs`). No new crates (no rand, chrono,
  clap, thiserror — randomness comes from the kernel's `Seed`/`Stream`, CLI
  parsing is std-only).
- **Models author, dice roll** (Constitution ratified constraint): no ML
  model ever runs in the sim core. Runtime generation is deterministic and
  seeded; models are offline authoring tools whose output is committed and
  drift-checked. See `book/src/frontier/frontier.md` (the book's Frontier
  part) for the wider (non-binding) vision map.
- Every crate sets `#![warn(missing_docs)]`; every public item, field, and
  variant gets a one-line doc comment.
- Rust edition 2024. Run `cargo fmt` as the final step before every commit —
  fmt-gate skips have been the most common review finding.
- **Typed quantities:** coherent physical units crossing API boundaries are
  hand-rolled newtypes with validating constructors and named conversions
  (`Au`, `Mm`, `LightYears`, `SolarMasses`, `StdDays`, `LocalDays`, …);
  dimensionless ratios stay bare `f64`. No dimensional-analysis crates.
  Rationale and scope: Campaign 2 spec, design principle 5. Enforced by
  `tools/type-audit/` (decisions
  `non-workspace-dev-tools-may-use-parser-libraries` / `the-bare-ok-rubric`):
  every primitive at a `pub`
  boundary carries a `type-audit:` verdict tag (`bare-ok(<class>)` /
  `waiver(<reason>)` / `pending(wave-N)`), drift-checked in CI.
- **Ratified decisions live in `docs/decisions/`** — the decision log is the
  durable, grep-able home for settled choices (do not relitigate without new
  information; supersede, never edit). Consult it before reopening an
  architectural or process question. Examples: `Fact.day` stays a bare
  `Option<f64>` (0014); `PredicateDef.name` duplicates its registry key
  (0015); config is JSON not YAML (0012); models author, dice roll (0009);
  studies are data, metrics are code (0011).
- **The documentation map is `docs/README.md`** — what knowledge lives where
  and how an idea flows from first mention to merged reality. For speculative
  directions, `book/src/frontier/idea-registry.md` is the scannable index
  (check it before proposing or reopening any idea; a `rejected`/`ratified`
  row is a closed question), and `book/src/frontier/frontier.md` holds the
  essays behind it — both published as the book's marked Frontier part.

## Process

Work proceeds in campaigns: spec (`docs/superpowers/specs/`) → implementation
plan (`docs/superpowers/plans/`) → execution → merge. **Definition of Done
for every merged plan includes the project book**: a chronicle entry
(`book/src/chronicle/`) and a freshness sweep of stale chapters — the book
may never lag merged reality; a campaign that resolves or moves one of the
**Confidence Gradient**'s bets (`book/src/open-questions.md`) re-scores that
chapter as part of the sweep (decision
`the-confidence-gradient-is-re-scored-not-frozen`). It also includes a one-page campaign
retrospective in `docs/retrospectives/` (decision 0020) — process lessons,
not product. Campaigns are named by sequence number + name; the Year-N
prefix is retired (decision 0017). Book prose is written at a deliberate
altitude: technical and mathematical, comprehensible without reading the
code it may show.
