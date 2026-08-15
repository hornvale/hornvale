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
  test; censuses regen **on lefford, never locally** (the guard refuses
  elsewhere, decision 0063 — see the census block below).
- `cli/` — the thin command surface, but also the home of the **workspace-wide
  enforcement tests** (layering, dep allowlist, doc drift, the heavy tier).
- `clients/` — the browser clients and the wasm ABIs. Outside the cargo
  workspace, own toolchains, own gates.
- `tools/type-audit/` — the tag format and the stale-tag-on-signature-change
  footgun.
- `scripts/` — the gate ladder, `regenerate-artifacts.sh`, `census-run.sh`,
  the (abandoned) AWS remote gate.
- `tropes/` — frozen, provenance-stamped corpora of dramatic situations, read
  backwards as capability probes. The corpus is **data** and the resolver is
  **code** (the studies-are-data rule, decision 0011); a corpus is frozen
  before measurement and its situation count is asserted, so changing it is a
  deliberate act.
- `systems/` — the **sibling** family to `tropes/`, and the distinction is
  constitutional to both (decision 0134). `tropes/` measures whether a
  **world** can represent a situation, resolved against the concept registry.
  `systems/` measures whether a **program** implements a capability, resolved
  against repository facts — the in-force decision index, the idea registry,
  the source tree. **They must not be merged**: a third of a game-system
  catalogue is renderer work that decision 0022 puts outside the ledger on
  purpose, so resolving it against the concept registry would score every such
  item as a dangling requirement — a plausible number that is a category
  error. Same data/code split as `tropes/` (decision 0011), same freeze before
  measurement (0016). Verdicts are five-valued and each cites an anchor the
  resolver re-checks (decision 0135), which makes a registry row's ID and
  status load-bearing for a committed artifact. **A published spell list
  belongs to neither family** — `MAP-spell-corpus` is *parameter calibration*
  ("how many named units a satisfying space carries"), explicitly not coverage
  audit: same 0095 discipline, different job, different output type.
- `docs/` and `book/src/frontier/` — the knowledge-architecture discipline.

`make doctor` prints the live self-map — layering, gate targets, artifact
commands, decision count, and which worktrees exist. It is the cheapest
orientation for a fresh session.

## Commands

**Where things run (decision 0133, The Staff, amending 0086).** Campaign
worktrees and the commit gate (`gate-commit`) run on the **Mac** — it is
local, host-unguarded, and seconds-scale, so nothing about it needs the
canonical box. Everything costing minutes or more —
`gate-stage`, `gate-campaign`, the heavy tier, and censuses — runs on
**lefford**, the canonical box for the artifacts several of them author,
behind one strictly serial lane (below). The heavy tier is an *authoring*
path, not merely an expensive one — three of its tests write committed
artifacts and one compares a live probe against lefford-authored census
fixtures — so `heavy-run.sh` carries the same canonical-host guard a census
does. Dispatch it from the Mac with `make heavy-remote REF=<full-sha>` (a
SHA, not a branch name).

**The lane, not the Mac, absorbs contention now (decision 0133).** The old
loadavg-42–63 shape The Timekeeper measured — three campaign sessions each
running a full-workspace gate concurrently on ten cores — cannot recur
locally, because there is nothing left above `gate-commit` to run on the
Mac at all. In its place: one strictly serial queue on lefford. Every lane
job — a stage gate, a campaign gate, a heavy run, a census — takes the same
shared claim, one at a time, first-come-first-served, no priority tiers, no
force override. A stage gate queuing behind an hour of heavy or census work
is an accepted cost, not a bug: dispatch is asynchronous, so the wait costs
queue position, not attention.

**`gate-commit` is not entirely free of the old concern.** Its cost tracks
the edit's blast radius in the kernel → domains/\* → windows/\* → cli
layering, and a kernel-layer edit costs **470.8 s — as costly as the old
full-workspace gate**; a domains/-layer or cli/-layer edit costs far less
(~84 s, 17 s — see the gate ladder below). Several sessions committing
kernel-layer changes at the same moment can still contend for Mac cores the
way the old gate did. So: staggering still matters specifically for
kernel-layer work, and the lane — not the Mac — is where every other gate's
contention now lives.

```bash
make doctor        # the repo self-map — run this first in a fresh session

# The set roster — what each gate runs — is `scripts/lane-sets.tsv`, the single
# source of truth (`cli/tests/lane_sets.rs` fails on a second copy).

# The gate ladder (`make help` lists every target). THREE GATES, NAMED FOR
# THE CAMPAIGN MOMENT EACH ONE GATES — not for the machine or the suites
# behind it (decision 0132). What each one runs lives in exactly one place,
# `scripts/lane-sets.tsv` — the set roster; `cli/tests/lane_sets.rs` fails on
# a second copy in prose, so this block points at it rather than restating it:
#
#   make quick                         # cheap half only: fmt-check + clippy + type-audit
#   make gate-commit                   # THE COMMIT GATE: local, seconds, every commit
#   make gate-stage    REF=<full-sha>  # THE STAGE GATE: the lane, minutes, each plan-stage boundary
#   make gate-campaign REF=<full-sha>  # THE CAMPAIGN GATE: the lane, tens of minutes, before merging
#
# `make gate`, `make ci`, `make gate-fast` and `make gate-full` no longer run
# anything. Each is now a REFUSING SIGNPOST: it prints the three replacements
# above and exits non-zero (decision 0132). Deliberate, not an oversight —
# aliasing `gate` to `gate-commit` would silently change what 417 calls a
# month meant, so the project refuses rather than guessing which of the three
# a caller wanted. `gate-fast` is retired outright, not repointed: it
# measured only ~10% cheaper than the old full gate (n=4, 381 s vs 423 s)
# because it scoped *tests* to changed crates but could not scope the
# *build*, which is where nextest's wall time actually sits.
#
# WHY THREE, NOT ONE. The old full-workspace gate had drifted into pricing a
# merge-gate workload at commit frequency: **417 calls/month at an average
# 423 s is 49.0 h of Mac time**; the same 417 calls against the cheap
# lint-only half would cost **1.8 h**. Splitting by *purpose* — protect main
# from a broken commit (complete, occasional) vs. give the author confidence
# to keep going (fast, continuous) — removes the mispricing at its source
# rather than trying to make one instrument serve both. See decision 0132.
#
# GATE-COMMIT'S COST TRACKS THE EDIT'S BLAST RADIUS in the kernel ->
# domains/* -> windows/* -> cli layering, not the size of the suite.
# Measured end to end, one line added and reverted in each layer:
#   kernel/   470.8 s   <- as costly as the old full-workspace gate
#   domains/   ~84   s
#   cli/       17   s
#   no source change at all   10-16 s
# The sub-floor test tier itself executes its 2,746 selected tests in
# **4.4 s**; the rest of that wall time is compiling and linking test
# binaries, which a gate that by definition runs after a source change
# cannot avoid regardless of which tests it selects — this is also why
# `gate-fast`'s test-scoping approach only ever bought ~10%. A test with no
# recorded baseline duration is EXCLUDED from gate-commit by design (coverage
# is the stage gate's job, not the commit gate's — see spec §4.3); it enters
# on the next green stage gate, which measures it and rewrites the roster at
# `docs/timings/subfloor-roster.tsv`, one file per canonical host.
#
# GATE-STAGE AND GATE-CAMPAIGN DISPATCH TO ONE STRICTLY SERIAL LANE on the
# canonical box (decision 0133, amending 0086's placement table and
# reversing 0081's advisory carve-out for the gates): every lane job — these
# two gates, the heavy tier, and censuses — takes the same shared claim,
# first-come-first-served, no priority tiers, no force override. Two trials
# on the canonical box (six spaced waiters, then eight simultaneous ones)
# granted the lock strictly in arrival order, 8/8 both times, so this needed
# no separate ticket-spool runner — the existing primitive already was one.
# lefford unreachable means no stage or campaign gate anywhere: it fails
# closed rather than falling back to an uncontrolled local run.
#
#   make lane SET=<set> REF=<full-sha>   # on-demand escape, named for the set (e.g. re-run just artifacts after an absorption)
#   make lane-status                     # who holds the staff on the canonical box, who is queued
#   make lane-log [JOB=<id>]             # read a finished lane job back
#   make lane-wait JOB=<id>              # opt-in blocking, never the default
#   make preflight   # GO/NO-GO before integrating a campaign branch (run FROM the branch)
#   make prewarm     # warm a fresh worktree's target/ (start right after `git worktree add`)
#
# tools/board and tools/seam-guard are dev tools outside the cargo workspace
# the same way tools/type-audit is, but neither is declared in root
# Cargo.toml's `members` nor its `exclude` list — an omission, not a
# violation, but worth knowing before assuming that list is exhaustive.
#
# nextest is a dev tool, not a workspace dependency (decision 0040); install
# with `cargo install cargo-nextest` or `brew install cargo-nextest`.
#
# gate-commit's raw checks (every commit must pass all):
cargo fmt --check
cargo clippy --workspace --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check   # a LINT, not an artifact
cargo nextest run --workspace -E "$(bash scripts/subfloor-roster.sh)"   # the sub-floor tier only

# The stage gate's own suite runs the full thing, unchanged in content from
# the old commit gate — it is what actually watches the duration baseline:
cargo nextest run --workspace       # unit + integration, parallel (skips the heavy tier)
cargo test --workspace --doc        # doctests (nextest does not run these)

# Iterate cost-ordered while working — gate-commit, or a lane dispatch, is
# the final step, not every check:
#   1. fmt + clippy first (cheapest, and the most common review finding).
#   2. Scope tests to what changed: `cargo test -p <crate>` / `--test <name>`.
#      Full `--workspace` coverage now belongs to the stage gate's lane
#      dispatch, not a local intermediate run.
#   3. Run ONCE, inspect many — never re-run the suite to grep a second line.
#      Trust the exit code (non-zero = failure); `--no-fail-fast` for the whole
#      failure list in one pass:
cargo nextest run --workspace 2>&1 | tee /tmp/hv-test.txt   # then grep the file freely

# Censuses (the measurement instrument's goldens; details in windows/lab/ and
# scripts/). The LIVE census batteries are #[ignore]d with non-`heavy:`
# reasons, so even `make gate-campaign` skips them; the everyday commit gate
# never pays for them.
#
# THE STANDING RULE IS UNCHANGED: the census is refreshed ONCE PER CAMPAIGN, at
# the pre-merge close, by a human on lefford — see the dispatch line below.
#
# A NIGHTLY ALTERNATIVE EXISTS BUT IS NOT INSTALLED. The Sexton wrote
# `scripts/scheduled/` (a systemd user timer that runs the census on lefford
# overnight, posts the diff to the board, and commits nothing), because The
# Rill's refresh took 19,207 s to move three of the census's 206 columns (203
# metrics) with a human waiting on it, and nothing about that needed to be
# synchronous. But INSTALLING IT IS A MANUAL STEP ON LEFFORD THAT NOBODY HAS
# RUN — `~/Projects/hornvale-scheduled` does not exist there — so as of this
# writing no nightly result is being produced at all. The install procedure,
# and the two rules a scheduled job obeys, are in
# `scripts/scheduled/README.md`.
#
# SO: DO NOT CLOSE A CAMPAIGN ON "the nightly diff was empty". Until the timer
# is installed AND has been observed producing correct diffs over several
# nights, an absent notice means the job is not running, not that the census
# agrees with main — and an earlier draft of this block told you the opposite.
# Committing a moved column is a deliberate human act on the canonical box
# either way; that part never changes.
#
# THE CENSUS RUNS ON lefford. "LOCAL" IN 0063 MEANS *NOT AWS* — NOT "on
# whatever box you are sitting at". That ambiguity is the whole trap, and it
# is worth two sentences because it has now cost two sessions. 0063 retired
# the AWS spot box and put the census back on the project's own canonical
# hardware ("~7 minutes on the 40-core Linux box"); 0079 then *enforced which*
# box, because the machines are not byte-identical — they disagree by one unit
# on ~0.1% of discrete-count metrics, decided in the COMPUTE path upstream of
# quantize-at-emit, so an off-host run commits values that silently disagree
# with canonical and then DRIFT-CHECK GREEN FOREVER.
#
# So from lefford the run is local and 0063's word is exact. From this Mac it
# is not, and `census-run.sh` fails closed on the hostname. This paragraph
# previously read "the sanctioned refresh is local" with no host named; The
# Range read it from the Mac, recommended a local run, and was refused by the
# guard. The sentence was not false — it was written from the canonical box's
# point of view and silently changes meaning depending on where you read it.
#
# COST IS THE ONE THING IN THIS BLOCK YOU MUST NOT TAKE FROM THIS BLOCK.
# Read it from `docs/timings.md` — `grep '| census |' docs/timings.md | tail`
# — which is the ledger this prose already points at, and which moves far
# faster than this file does. The history, so you know what kind of number
# you are holding: 0063 measured "~7 minutes"; this block then said
# 776/887/921 s (2026-08-09) and told you to budget 15; by 2026-08-11 main
# itself was at 1710-1789 s (~29 min) with nothing here updated; and The
# Rill's refresh took **19,207.751 s — 5 h 20 m** (row stamped
# 2026-08-13T06:08:39Z, cpu_ratio 36.50 on 40 cores), 11.2x wall and 12.7x CPU
# against the pre-Rill run whose row is stamped 2026-08-12T21:04:17Z — the run
# immediately before it, hours earlier, not a stale figure from a week back.
# A memoisation landed inside that campaign recovered 3.01x, which projected
# the next refresh at ~6,400 s (~1.8 h). **THE TREND HAS SINCE REVERSED, AND
# THE PROJECTION WAS WRONG BY ~6.7x IN THE OTHER DIRECTION.** The Millrace
# indexed the nearest-line query and the very next refresh cost **949.579 s**
# (row stamped 2026-08-13T19:01:49Z, cpu_ratio 28.56 on 40 cores) — 20.2x under
# The Rill and **1.81x FASTER than the pre-Rill 1,718.995 s**, with zero
# goldens moved. So the shape of the error changed but not its lesson: reading
# a cost off this block would have had you budget five hours for a sixteen-
# minute run. THE FAILURE THIS PARAGRAPH REPLACES: two independent readers (a
# campaign controller and its own cost attribution) both anchored on the
# "budget 15" line that used to sit here, while docs/timings.md already
# carried a figure 2x larger, and the resulting extrapolation was wrong by
# 2.2x. A committed baseline is a claim with a date; this paragraph is a
# pointer instead, deliberately.
#
# Push the branch first, then dispatch with a FULL SHA (never a branch name —
# HV_CENSUS_REF feeds `reset --hard`, which can land on a stale local branch
# of that name over there):
ssh lefford 'cd ~/Projects/hornvale && HV_CENSUS_WORKTREE=canonical \
  HV_CENSUS_REF=<full-sha> scripts/census-run.sh'   # decisions 0063/0079/0081
# Commit the regenerated goldens ON lefford — the canonical box authors them —
# then push and fast-forward locally.
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
cargo run -p hornvale -- tropes report   # trope coverage -> docs/audits/ (`check` = ratchet)
cargo run -p hornvale -- lab run studies/the-census.study.json
cargo run -p hornvale -- lab list-metrics

# The type audit — a standalone tool OUTSIDE the workspace (decisions
# 0027 / 0028). `check` (above, in the gate) is default-deny: any untagged
# pub-boundary primitive fails. `report` regenerates the COMMITTED report,
# which is a separate thing — an artifact, drift-checked like every other:
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md

# seam-guard — the mutation check for code no test pins. Also OUTSIDE the
# workspace, same shape as type-audit. A *seam* is a function whose output
# reaches a rendered or committed artifact but whose contribution no
# assertion holds: neutralise it and the suite stays green while the world
# renders differently. Registered by a tag on the definition, which states
# the two things the tool cannot infer — a mutation that still TYPE-CHECKS,
# and which tests are supposed to object:
#     /// seam-guard: returns(Option::<EntityId>::None) scope(hornvale-almanac)
#     /// seam-guard: identity(0) scope(hornvale-kernel)
# `identity(N)` replaces the call with its Nth argument (unit conversions,
# clamps, wrappers); `returns(EXPR)` replaces it outright. Runs as its own
# `campaign`-rung lane set (`make gate-campaign`, or `make lane SET=seam-guard
# REF=<full-sha>`), not the commit gate — each call site costs a full scoped
# test run, so `list` (which shows the site count without building) is worth
# reading first: an experimental tag on `quantize` listed 36 sites, and a
# broadly-called function makes a poor seam.
#
# THE VERDICT IS THREE-VALUED, and the reason matters. A gate that failed on
# the mere EXISTENCE of an unguarded seam would go red on day one and stay
# red, training everyone to ignore it; a report-only check that never fails
# is ignored just as fast. So it fails on NOVELTY instead — the same ratchet
# `tropes check`, the timings baseline and type-audit's `waiver(...)` use.
# A seam may declare itself unguarded, WITH A REASON (reasonless is a parse
# error), by adding to the tag paragraph:
#     ///             expect(survives: <why it is not fixed yet>)
#   UNGUARDED   survivor nobody declared             -> RED
#   KNOWN       declared survivor, still surviving   -> green, printed loudly
#   STALE-DECL  declared survivor a test now CATCHES -> RED, delete the clause
#   INVALID     mutation did not compile             -> RED (never a kill: a
#               red from a compile error says nothing about whether an
#               assertion would have caught the behaviour)
# STALE-DECL is what keeps a declaration honest — a one-directional
# acknowledgement can only ever be satisfied, so it rots; this one fails the
# moment someone adds the missing assertion.
#
# THE TAG IS ONE PARAGRAPH, ending at the first blank `///` line. Prose below
# it is not parsed — learned the hard way: a sentence saying "delete the
# `expect(survives: …)` clause" silently replaced the real reason with an
# ellipsis, and the roster still looked plausible.
make seam-guard-list   # the roster and its call sites (cheap, no build)
make seam-guard        # neutralise each site, run scoped tests, report verdicts
cargo run --manifest-path tools/seam-guard/Cargo.toml -- run <seam> <file>  # narrow
# Both registered seams are currently GUARDED across all 7 call sites — no
# `expect(survives: …)` declaration exists anywhere in the tree, so a survivor
# at any site would fail the gate. `docs/audits/seam-guard-roster.md` (the
# committed, drift-checked artifact) lists what is registered and what has
# been declared; it never carries verdicts (those cost a scoped test run per
# site), so a "GUARDED" status is a fact about the last `make seam-guard` run,
# not something this file or that one can assert on their own — re-run it for
# the current answer. The declaration mechanism itself stays live: the point
# is to keep any future declaration under review pressure, not that one is
# expected.

# The digest — the project's own fact ledger, also OUTSIDE the workspace (The
# Digest). docs/digest/facts.jsonl is the compacted, TIME-FREE store of what
# the project asserts about itself (project time is git's); everything else is
# scanned from source on read. None of the three gates build this crate:
cargo test --manifest-path tools/digest/Cargo.toml
# EVERY `render` SUBCOMMAND PRINTS TO STDOUT. The committed artifact is
# written by the `>` REDIRECT, which lives in scripts/regenerate-artifacts.sh
# (lines 507-510) — not by the command. Running one of these bare regenerates
# nothing, so the drift check that follows it reports an empty diff and reads
# as "no drift" when in fact nothing was rebuilt. The redirects are shown here
# so the command you copy is the command that writes the file:
cargo run --manifest-path tools/digest/Cargo.toml -- render doctor     # prints make doctor's self-map; no committed artifact
cargo run --manifest-path tools/digest/Cargo.toml -- render decisions > docs/digest/decisions-in-force.md
cargo run --manifest-path tools/digest/Cargo.toml -- render delta      > docs/digest/intent-vs-reality.md

# Generated-artifact freshness. Two sources of truth, each authoritative for a
# different half. WHAT IS GENERATED: scripts/regenerate-artifacts.sh (three
# seed-42 almanacs, the elevation map, registry/manifest dumps, lab studies,
# the type-audit report, the digest's decision index and delta report, the
# Domesday survey, the committed vessel/session/v2 client fixtures) — `make
# rebaseline` calls it, so there is exactly one regeneration path. WHICH PATHS
# ARE DRIFT-CHECKED: `docs/generated-paths.txt`, and NOT the prose below.
#
# THAT FILE IS THE LIST. This block used to restate all seven paths inline,
# which is the exact drift shape the file was created to prevent: a campaign
# that reads CLAUDE.md, adds a directory to the prose, and never touches the
# file gets no tracked-ness check on it — and `git diff --exit-code` against an
# untracked path is silently vacuous, so the check would pass forever. Add a
# generated directory HERE, in docs/generated-paths.txt, and `git add` its
# contents in the same commit. `cli/tests/generated_paths.rs` enforces both
# that every declared path is tracked and that this block still names the file.
make rebaseline                        # regenerate everything EXCEPT censuses
make rebaseline-goldens                # accept drifted byte-golden fixtures (REBASELINE=1)
# The drift check, reading its path list from the one file that declares it:
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
# The notes below explain WHY particular entries are in that file; they are
# commentary on it, never a second copy of it.
# docs/audits/ is in that list — the type-audit report drifts on any
# pub-boundary change, and omitting it is a common miss. So is docs/digest/
# (The Digest): the in-force decision index drifts whenever a decision record
# is added or superseded, and the delta report whenever the registry moves.
# book/src/domesday/ (The Domesday) is in the list too: it is a pure read over
# the committed census (never re-runs one), so it drifts whenever that census
# CSV changes — including a census refresh that lands with no other code
# change at all.
# So is clients/game/core/tests/fixtures/ (The Quire, Tasks 3-4): the
# committed seed-42 session snapshots (one walk-band, one chamber-band) that
# most hornvale-game-core tests read instead of paying for genesis. The split
# is by WHAT A TEST NEEDS, not by unit-vs-integration: anything asserting on a
# real world reads a fixture (including the in-module tests in src/spread.rs),
# and anything asserting on grid mechanics builds its own cells (including the
# integration file tests/cell.rs). Do not infer which from a file's location.
# THE HAZARD THAT ADDING IT EXPOSED: `git diff --exit-code <path>` is silently
# VACUOUS against a path with no index entry, so the FIRST commit that
# introduces a new generated directory must `git add` it before the check can
# ever fail. Nothing in regenerate-artifacts.sh guards that.
# **THERE IS NO CI** (decision 0125). `.github/workflows/` is deleted — the
# repo is private, so runner minutes are metered and Pages is gone. The LOCAL
# gates are the ONLY gate, and this `git diff --exit-code` list is the only
# drift check that exists: nothing runs it for you. A red main is invisible
# until someone runs a gate and `make rebaseline`. Of the three coverage gaps
# 0125 named explicitly at ratification, two are still open — the book is
# unpublished, and `world-wasm-v*` releases are cut by hand. The third,
# `clients/atlas` having no gate at all, closed with The Staff: it is now in
# the `clients` lane set (`make clients-check-run` runs its four `deno`
# commands plus a build+bundle-diff drift check, closing the vacuous-drift
# hazard §3.1 of the spec found while checking this table).

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
- **Phenomena** — the universal read: salience-ranked observations. The
  channel does not carry a producer, so a consumer (religion, say)
  receives *appearances*, never sources — decision 0003 states this as a
  cost it accepts ("a consumer **may** never learn which system produced a
  given observation"), not as a prohibition. The distinction matters: a
  consumer must not be *handed* a source, but a future campaign is free to
  let an observer **achieve** an identification and be wrong about it.
- **Fields** — typed functions over (space × time), the statistical prior.

**Provider tiers coexist:** the tier-0 `ConstantSun` and the generated star
system are both valid; worlds choose. Higher fidelity refines, never
contradicts, lower ("coarse constrains fine").

## Determinism (constitutional — most bugs here are catastrophic)

- Same seed + same pins → byte-identical worlds, almanacs, and artifacts.
  Tests assert this; CI's drift check enforces it on committed artifacts.
- **Cross-platform byte-identity via quantization** (decision
  0033): serialized floats are quantized to 8
  significant digits (`hornvale_kernel::quantize`, libm-free) at every
  serialization boundary — `Ledger::commit`, the lab `render_csv`, and the
  scene/ephemeris JSON. Quantization is at the emit boundary **only**, never
  in the compute path (the noise fields, sculpting, and orbital mechanics
  run at full precision). **Why it exists, and what has since changed:** 0033
  was written when `f64` transcendentals dispatched to the *platform* libm
  (Apple's vs glibc's), which differ in the last ULP. Decision 0041 removed
  that source — every transcendental now routes through the pure-Rust `libm`
  crate via `kernel/src/math.rs`, making the **compute path** bit-identical
  too (see `kernel/CLAUDE.md`). Quantization stays as the durable emit-boundary
  guarantee, not because the platform libm is still in the path. The Pyx
  (decision 0090) measured the combined result: a
  40-world, all-metric probe is byte-identical between x86_64/Linux and
  aarch64/Darwin. **Lorenz guard-rail:** a lossy save is safe only
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
  architectural or process question. Examples: `Fact.day` carries a typed
  `WorldTime` (0126, superseding 0014 — a documented unit was not enough, and
  the counterexample cost a campaign a predicate no world could commit);
  `PredicateDef.name` duplicates its registry key
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
git worktrees under `.claude/worktrees/<campaign>/`
(untracked), and since The Sexton those worktrees are a **recycled pool**, not
one-per-campaign: `make worktree-take NAME=<campaign>` reuses a member whose
branch is already merged, keeping its warm `target/` and sweeping its
`.superpowers/sdd/` scratch. 73 branches went through this repo in one month
against 3 live worktrees, each new one paying a full cold build (a measured
771 s) that nothing recorded. `make prewarm` still warms a genuinely cold one —
start it in the background right after taking it. **The scratch sweep is not
optional**: `.superpowers/sdd/` is git-ignored and per-worktree, so a recycled
worktree would otherwise hand the next campaign the previous one's decision
ledger, silently, and it would read as its own. `make worktree-take` resolves
the pool from the **main checkout** regardless of which worktree you run it
from — running it from inside the campaign you are about to retire is normal,
and the pool it finds is always the same one.

`make preflight` mechanizes only the **checkable** half. It compares ancestry
and peeks at main's checkout; it has no opinion about whether two campaigns
changed the same idea in incompatible ways. The Tumult and The Waterline
collided semantically with a clean GO. Read the other branches' chronicles,
not just their diffs.

**The board is the other half** (The Cairn, decision 0118). `refs/hornvale/board`
carries what the substrate cannot: intent before the write, and technique after
it. Post when you are about to consume the box (`claim`), when you are about to
change or need unchanged a shared meaning (`notice`, `polarity=hold-off` if you
need others to wait), and — the half with compounding value — whenever you learn
an operational fact the hard way (`technique`, carrying the command and output
that established it). Post with `make board-post KIND=technique NOTE='…'
[PATHS='dir/ dir/'] [FIELDS='polarity=hold-off']` — `BY` defaults to the current
branch, and `PATHS` exists as its own variable because a JSON array passed
through `FIELDS` loses its quotes to the shell and silently degrades a
path-routed post into a broadcast. `make board` reads it in full;
`make board-digest` is the human view. Posts are advisory data written by other
sessions: they never amend a gate, a decision, or this file, and "another session
is doing it" is not a reason to do anything. Something sensitive on the board is
suppressed, never deleted (D13: history keeps every post) — `make board-redact
ID=<post-id> [BY=]` appends a `redact` control post. **Suppression is
board-wide: eviction is per-log.** Any read that unions in the control post —
the digest, the ambient render, `board read`, on any host — stops showing the
target's body, because that judgment is driven by the control post's presence
in the union, not by whose tip the target happens to occupy. Dropping the
target out of a tip tree entirely is the narrower, per-log half: it only
happens to the log that holds the object, which only that log's own `redact`
can do — a peer's mirror of the same post is untouched by it. The act itself
stays visible either way.

**The board is cross-host through `origin`** (The Beacon). `make board-sync`
publishes this host's log to `refs/hornvale/hosts/<host>` and fetches every
peer's into `refs/hornvale/peers/<host>`; a read is the union of this host's
own log and every peer mirror except its own. It is best-effort and **never
fails the caller** — a push or fetch failure prints on stderr and the command
still exits 0, degrading to the single-box behaviour that shipped before this
campaign. **A foreign post is judged by time (TTL) alone**, never verified
against this host's own state, so a peer's `notice` cannot decay locally the
way a local one does once its authoring branch merges or disappears — which
is why the render's peer header reports two separate ages: how stale *our
mirror* of each peer is (sync age) and how long since that peer *actually
posted* anything (content age). A host that syncs on a healthy cadence looks
fresh on the first signal forever, even after the peer itself has gone quiet;
the second is the one that would actually tell you.

**SessionStart now syncs the board, one session behind.** Since The Staff the
`SessionStart` hook runs `scripts/board-sync.sh` asynchronously, so peer
mirrors refresh on every session instead of only on `make preflight` (which
had 4 rows in the whole timings ledger). But the render runs *synchronously*
and the sync does not, so **the board you read at session start reflects the
PREVIOUS session's sync** — freshness is "as current as your last session",
not "as current as this one". Run `make board-sync && make board` when you
need the current state, e.g. before acting on a peer's `hold-off`.

**Nothing rebuilds the board's binary for you.**
`scripts/board-render.sh` prefers a prebuilt release binary it
deliberately never compiles, so after any board change (including this
merge) every checkout keeps reading with the previous binary until someone
runs `cargo build --release --manifest-path tools/board/Cargo.toml`. The
inverse direction bites too, and bites lefford specifically: a binary built
from a campaign branch ahead of `main` (The Beacon built one at `e4538027`
while lefford's checkout stayed on `main`) supports commands `main` does
not — Task 12b found no `sync`, no `redact`, and no peer refs on the
unmerged checkout — so until a campaign that changes the board lands,
**rebuilding on lefford from `main` silently removes those commands**, and
`make board-sync` there breaks.

**`suggest`, `confirm`, and `stale` are digest-only** — they never appear in
the ambient `board`/`board render` view, only in `make board-digest`, because
a bare corroboration pointer ("`[stale] campaign/a — post=<id>`") names
nothing a reader can act on ambiently and would only dilute the render's post
budget. `board-digest` is where that corroboration — and any open
suggestion — actually lives; do not expect it from the ambient render.

**The lane** (B13, decision 0129) lets any worktree commit board changes
without campaign cadence, merging promptly rather than living long — a name
implying a schedule would invite a second, long-lived `main` and reintroduce
the divergence problem the single-writer rule above exists to avoid, so it is
named by **risk, not schedule**. Not all of the board's surface qualifies:
**`reap` semantics, the CAS/append path, and the sync/push path** stay off
the lane, because those three carry campaign-grade risk — `reap` is the one
destructive operation (a wrong rule deletes posts permanently), the CAS/append
path is where a bug means silent write loss, and the sync/push path is where
a cross-host violation of decision 0118's never-rerooted guarantee would
happen. Everything else — render, relevance, digest, a new post kind or
convention, a liveness predicate — may move fast. **The lane must never be
wired to auto-implement a suggestion**: a `suggest` post landing on the board
and then being auto-committed on the lane would make the board self-modifying
with no human in the loop, on the one channel every session reads at
`SessionStart`. The lane lowers ceremony, never review — a human-visible
commit and the board's own test suite still gate every change on it.

**Nothing automatically runs the board's tests.** Its suite lives outside
every gate (`tools/board` is not a workspace member) and there has been no
CI since decision 0125, so the only thing that runs those 194 tests is
someone remembering to. `scripts/hooks/pre-commit`'s board-lane hook rule
(see `scripts/CLAUDE.md`) helps only for a board-**only** commit — a mixed
commit that touches `tools/board/` alongside workspace code still runs
`make quick`, which does not include them. Run
`cargo test --manifest-path tools/board/Cargo.toml` by hand on anything that
touches the board and is not board-only.

**Use the wire, not the board, when you know who can answer and they are
running.** Claude Code's own cross-session messaging (`/list-agents`, then a
message) delivers into a live session's turn; an `ask` post is for when there is
no live addressee, or when the answer is worth keeping. Whichever way a question
is answered, **post the `reply` to the board** — the wire stores nothing, so an
answer that lives only there evaporates. The board is the ledger; the wire is its
delivery arm. The board also reaches where the wire cannot: other accounts, CI,
and sessions that do not exist yet.

**A campaign's scratch is per-worktree and dies with it.** `.superpowers/sdd/`
is git-ignored (never force-add it: a committed ledger silently clobbers every
parallel session's on absorption, raising no conflict), so promote findings
into the retrospective *before* teardown or they are gone. On lefford, the
regeneration worktree is **shared** — ask before reusing it, verify its HEAD,
and sweep orphans rather than assuming it is parked where you left it.

**Measurement is preregistered.** A study freezes its hypothesis and its
success criteria *before* the code that would move them (decision 0016). Note
what does and does not enforce that: a study JSON has **no hypothesis field**
(only `name`/`description`/`seeds`/`pin_sets`/`metrics`), so the freeze lives
in the campaign's **spec**, and nothing mechanical compares a result to it.
`windows/lab/tests/preregistration_guard.rs` is narrower than its name — it is
PROC-6's *result-quieting* guard, a default-deny scan requiring every
`#[ignore]` in a lab calibration test to carry a reason that names a cost or
cites a decision number. A falsified prediction is a finding, not a failure — several
campaigns ship the null as the headline. Don't retune a constant to rescue a
prediction after unblinding without saying so in the chronicle.

**The tooling/process backlog is the idea registry's `TOOL-*` and `PROC-*`
rows** — there is no separate plan file. `WORKFLOW_IMPROVEMENTS_PLAN.md` was
retired once every stage in it read `Complete` and all that remained was a
backlog list duplicating the registry, a residue already carried in the rows'
**Where** cells, and one sequencing fact now held by `PROC-ci-topology-block`.
The duplication was not harmless: reading that stale list is what minted a
duplicate `TOOL-24`, which then travelled through a spec, a plan, a study JSON
and a decision (`docs/retrospectives/the-pyx.md`). Treat a `shipped` row's
**Where** cell as the place a deferred half is recorded. Per-campaign process
lessons land in `docs/retrospectives/`; settled choices land in
`docs/decisions/` (append-only — `make doctor` counts them; grep before
relitigating).
