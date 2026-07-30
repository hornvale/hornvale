# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

Hornvale is a deterministic, multiscalar world simulation observed through
text — "sim first, game as lens." The governing documents are the spec
(`docs/superpowers/specs/2026-07-05-hornvale-longterm-plan-design.md`, the
Constitution especially) and the project book (`book/`, published at
hornvale.github.io/hornvale). When this file and the spec disagree, the spec
governs.

## Directory guides

Several subtrees carry their own `CLAUDE.md` with directory-specific tribal
knowledge that loads when you work there — consult the relevant one before
editing:

- `kernel/` — the determinism substrate: save-format contracts, quantize-at-
  emit-only, `math.rs` (libm transcendentals; floor/sqrt stay intrinsic), the
  `Fbm` derive-once pattern, dense-index storage is `Vec` not a map.
- `domains/` — depend only on the kernel, never a sibling; trace-protocol-
  only; stream consumption order is a contract. `domains/terrain/` adds the
  byte-identity discipline for the sculpting pipeline.
- `windows/` — what a window may and may not do; how to add one.
  `windows/worldgen/` — the composition root and the `BuildDepth` ladder.
  `windows/lab/` — studies are data, metrics are code; nextest is process-per-
  test; censuses regen locally in ~7 min since The Local Census.
- `cli/` — the thin command surface, but also the home of the **workspace-wide
  enforcement tests** (layering, dep allowlist, doc drift, the heavy tier).
- `clients/` — the browser clients and the wasm ABIs. Outside the cargo
  workspace, own toolchains, own gates.
- `tools/type-audit/` — the tag format and the stale-tag-on-signature-change
  footgun.
- `scripts/` — the gate ladder, `regenerate-artifacts.sh`, `census-run.sh`,
  the (abandoned) AWS remote gate.
- `docs/` and `book/src/frontier/` — the knowledge-architecture discipline.

`make doctor` prints the live self-map — layering, gate targets, artifact
commands, decision count, and which worktrees exist. It is the cheapest
orientation for a fresh session.

## Commands

**Where things run (decision 0086, The Siding).** Campaign worktrees and the
commit gate run on the **Mac**; the **heavy tier and censuses run on lefford**,
the canonical box for the artifacts they author. The heavy tier is an
*authoring* path, not merely an expensive one — three of its tests write
committed artifacts and one compares a live probe against lefford-authored
census fixtures — so `heavy-run.sh` carries the same canonical-host guard a
census does. Dispatch it from the Mac with `make heavy-remote REF=<full-sha>`
(a SHA, not a branch name). `make gate` on lefford is not forbidden, but it
oversubscribes a box whose other jobs are long; that contention is what 0086
exists to remove.

```bash
make doctor        # the repo self-map — run this first in a fresh session

# The gate ladder (`make help` lists all targets). The commit gate is
# `make gate`; it runs `cargo nextest run` (test binaries in PARALLEL) plus
# doctests. The heavy live-worldgen batteries (censuses, the full pin
# product, byte-identity rebuilds) are #[ignore]d out of it and run in
# `make gate-full`. The #[ignore] tier AND nextest's parallelism together
# put the commit gate near ~4 min; neither lever alone gets there. The
# batteries this tiering deferred carry a `heavy:` ignore-reason token
# (see cli/tests/heavy_tier.rs):
#   make quick       # cheap half only: fmt-check + clippy + type-audit
#   make gate        # COMMIT GATE: fmt + clippy + type-audit + nextest + doctests (~4 min)
#   make gate-fast   # ITERATION ONLY: the above, scoped to changed crates
#   make gate-full   # full evidence: the commit gate + the cost-tagged heavy tier (scripts/gate-full-heavy.sh)
#   make ci          # THE TIMEKEEPER: whole-workspace suite under the `ci` nextest
#                     # profile (~15 min), writes target/nextest/ci/run.json +
#                     # run.log, alarms on a per-test or whole-suite duration
#                     # shift against docs/timings/test-baseline-<host>.tsv,
#                     # THEN (only if the alarm passed) rewrites that baseline
#                     # from this run. The baseline is per HOST and committed,
#                     # so `git log -p` on it is the archaeology of how the
#                     # suite's cost moved over time; a red run leaves it
#                     # untouched, and re-recording a regression is deliberate
#                     # (re-record in the same commit that caused it).
#                     # ENFORCEMENT NEEDS A HELD BOX CLAIM: the alarm only
#                     # asserts when `hornvale_lab::census_claim::current_holder()`
#                     # names THIS host (HV_CENSUS_CLAIM_PATH or the default
#                     # /tmp/hv-census.claim) — nothing in `make ci` acquires
#                     # that claim itself, so on an ordinary machine with no
#                     # concurrent heavy run it always records and never
#                     # enforces. Verified both branches by hand: a claim file
#                     # naming this host makes a divided-by-ten baseline alarm
#                     # (RED); a claim path pointed at a nonexistent file makes
#                     # the same divided baseline record-only (GREEN, with a
#                     # stderr line naming the suppressed shift count).
#   make preflight   # GO/NO-GO before integrating a campaign branch (run FROM the branch)
#   make prewarm     # warm a fresh worktree's target/ (start right after `git worktree add`)
# nextest is a dev tool, not a workspace dependency (decision 0040); install
# with `cargo install cargo-nextest` or `brew install cargo-nextest`.
# The raw checks `make gate` runs (every commit must pass all):
cargo nextest run --workspace       # unit + integration, parallel (skips the heavy tier)
cargo test --workspace --doc        # doctests (nextest does not run these)
cargo fmt --check
cargo clippy --workspace --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check   # a LINT, not an artifact

# Iterate cost-ordered — the full gate is the FINAL step, not every check:
#   1. fmt + clippy first (cheapest, and the most common review finding).
#   2. Scope tests to what changed: `cargo test -p <crate>` / `--test <name>`.
#      `--workspace` belongs at the pre-commit gate, not each intermediate run.
#   3. Run ONCE, inspect many — never re-run the suite to grep a second line.
#      Trust the exit code (non-zero = failure); `--no-fail-fast` for the whole
#      failure list in one pass:
cargo nextest run --workspace 2>&1 | tee /tmp/hv-test.txt   # then grep the file freely

# Censuses (the measurement instrument's goldens; details in windows/lab/ and
# scripts/). The LIVE census batteries are #[ignore]d with non-`heavy:`
# reasons, so even `make gate-full` skips them; the everyday gate never pays
# for them. Since The Local Census the full ~2000-world census is a ~7-min
# LOCAL run, so the sanctioned refresh is local — once per campaign at the
# pre-merge close, keeping book/src/laboratory/generated/*/rows.csv current
# with main rather than lagging it:
bash scripts/census-run.sh              # THE sanctioned refresh (decision 0081)
bash scripts/census-run.sh status       # is a heavy run already holding the box?
make lab-diff STUDY=the-census          # which metrics moved vs HEAD (review surface)
make census-check                       # analysis-harness gate (needs duckdb + python3)
# Use census-run.sh, NOT `HV_CENSUS=1 bash scripts/regenerate-artifacts.sh`:
# all entry points serialize (one heavy writer per box) but only the wrapper
# ledgers the run in docs/timings.md. `make regen-remote` / scripts/aws-gate/
# are ABANDONED (decision 0063): this box is the single canonical platform —
# AWS differs on ~0.1% of discrete-count metrics, so it cannot be a parallel
# reference. Goldens are authored on one enforced host (decision 0079).

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
cargo run -p hornvale -- possess --seed 42            # walk the world (the game seam)
cargo run -p hornvale -- almanac --world world.json
cargo run -p hornvale -- map --world world.json --out elevation.ppm
cargo run -p hornvale -- concepts        # registry dump (book reference page)
cargo run -p hornvale -- streams         # stream manifest (book reference page)
cargo run -p hornvale -- lab run studies/the-census.study.json
cargo run -p hornvale -- lab list-metrics

# The type audit — a standalone tool OUTSIDE the workspace (decisions
# 0027 / 0028). `check` (above, in the gate) is default-deny: any untagged
# pub-boundary primitive fails. `report` regenerates the COMMITTED report,
# which is a separate thing — an artifact, drift-checked like every other:
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md

# Generated-artifact freshness. The single source of truth is
# scripts/regenerate-artifacts.sh (three seed-42 almanacs, the elevation map,
# registry/manifest dumps, lab studies, the type-audit report); `make
# rebaseline` and CI both call it, so they cannot silently diverge:
make rebaseline                        # regenerate everything EXCEPT censuses
make rebaseline-goldens                # accept drifted byte-golden fixtures (REBASELINE=1)
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/
# docs/audits/ is in that list — the type-audit report drifts on any
# pub-boundary change, and omitting it is a common miss.
# **CI is manual-only** (decision 0042: workflow_dispatch, Actions tab → Run
# workflow). Nothing runs on push. The LOCAL gate is the gate; a red main is
# invisible until someone runs it.

# The browser clients (outside the cargo workspace; see clients/CLAUDE.md):
make vessel-check       # the Casement: deno checks + wasm fmt/clippy + byte-identity smoke
make world-check        # the world catalog: lint + golden byte-identity smoke + size gate

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

A **domain** models a slice of the world; a **window** presents one. Domains
draw world-state and own seed labels; windows read the committed ledger and
render. `windows/explain` is the clearest statement of the contract — it
narrates a world by reading only committed facts, never the in-memory
system, which is how it validates that the ledger is sufficient.

**Clients are outside the workspace and outside determinism.** `clients/`
holds browser clients with their own toolchains (Deno, `wasm32-unknown-
unknown`), excluded from the cargo workspace by `Cargo.toml`. The repo
boundary **is** the determinism boundary (decision 0055): Hornvale
guarantees byte-identical seeded output up to and including the wasm ABI;
what a client does with that output is unconstrained (decisions 0022/0023).
The external Orrery client (a sibling repo) consumes `clients/world-wasm`'s
released catalog — so **scene schemas (`scene/system/v1`, `scene/tiles/v1`,
…) are cross-repo contracts: additive-or-versioned only**, the same
discipline seed labels carry.

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
- **Cross-platform byte-identity via quantization** (decision
  0033): `f64`
  transcendentals route to the platform libm (Apple's vs glibc's), which
  differ in the last ULP, so serialized floats are quantized to 8
  significant digits (`hornvale_kernel::quantize`, libm-free) at every
  serialization boundary — `Ledger::commit`, the lab `render_csv`, and the
  scene/ephemeris JSON. Quantization is at the emit boundary **only**, never
  in the compute path (the noise fields, sculpting, and orbital mechanics
  run at full precision). **Lorenz guard-rail:** a lossy save is safe only
  because reload re-derives from the lossless seed — never seed a chaotic
  forward-integrator from quantized ledger floats; resumption re-derives
  from the seed, and any chaotic checkpoint needs its own full-precision
  format.
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

- Dependencies: `serde`, `serde_json`, and `libm` only, workspace-wide
  (the allowlist is the `ALLOWED_EXTERNAL` const in
  `cli/tests/architecture.rs`; decision 0004, amended by 0041 to admit libm
  for portable transcendentals). No new crates (no rand, chrono, clap,
  thiserror — randomness comes from the kernel's `Seed`/`Stream`, CLI
  parsing is std-only). Clients are outside the workspace and carry their
  own toolchains; this allowlist does not bind them.
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
  0027 / 0028):
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
0030). It also includes a one-page campaign
retrospective in `docs/retrospectives/` (decision 0020) — process lessons,
not product. Campaigns are named by sequence number + name; the Year-N
prefix is retired (decision 0017). Book prose is written at a deliberate
altitude: technical and mathematical, comprehensible without reading the
code it may show.

**Campaign work runs under autopilot by default**: before the first
clarifying question or approval gate of any brainstorm/spec/plan/execution
work, invoke the `campaign-autopilot` skill — it auto-resolves the
routine gates against Nathan's standing policy and ledgers every decision
for his review at the spec and merge stops. Nathan saying "manual mode"
disengages it for the session.

**Campaign branches absorb main at every plan-stage boundary**, not only at
close: run `make preflight` from the branch; on an ancestry NO-GO, merge
main INTO the branch and re-run the gate there. Two exceptions: never
absorb mid-measurement (a preregistered study's baseline and readout must
see the same physics — finish the readout first), and never while main's
checkout shows another session mid-landing (the preflight peeks and warns).
Parallel sessions are the norm; small absorptions keep semantic drift next
to its cause instead of surfacing it at a 105-commit merge. Campaigns run in
git worktrees under `.claude/worktrees/<campaign>/` (untracked); `make
prewarm` warms a fresh one's `target/` — start it in the background right
after `git worktree add`, before the first gate.

**Measurement is preregistered.** A study freezes its hypothesis and its
success criteria *before* the code that would move them (decision 0016), and
`preregistration_guard` enforces that a study can't be quietly edited to
match a result. A falsified prediction is a finding, not a failure — several
campaigns ship the null as the headline. Don't retune a constant to rescue a
prediction after unblinding without saying so in the chronicle.

**The tooling/process backlog is `WORKFLOW_IMPROVEMENTS_PLAN.md`** (TOOL-*
and PROC-* registry rows, staged). Per-campaign process lessons land in
`docs/retrospectives/`; settled choices land in `docs/decisions/` (82
records, append-only — grep before relitigating).
