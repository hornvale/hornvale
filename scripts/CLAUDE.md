# CLAUDE.md — working in `scripts/`

These scripts are the connective tissue of the gate ladder, artifact
regeneration, and census runs (plus `aws-gate/`, kept only as history). Read
the root `CLAUDE.md` "Commands" section for the `make` targets that call
them.

## The single source of truth for artifacts

`regenerate-artifacts.sh` regenerates **every** committed generated artifact
(three seed-42 almanacs, the elevation map, registry/manifest dumps, lab
studies, the type-audit report, the seam-guard roster
(`docs/audits/seam-guard-roster.md`), the digest's decision index + delta report
under `docs/digest/`, and the committed `vessel/session/v2` client fixtures
under `clients/game/core/tests/fixtures/` — one walk-band (turn 0), one
chamber-band, reached via `scripts/possession-chamber.txt`) — and `make
rebaseline` calls it, so there is exactly one regeneration path. Since 0125
retired GitHub Actions there is no second caller to diverge *from*, and no
automatic run at all: **the drift check happens only when a human runs it.**
Key knobs:

- **`SKIP_CENSUS=1`** — skip the census `lab run`s. Everyday local regen sets
  it so the gate stays fast. `ci-census-probe.sh` — a fast first-N-seeds
  spot-check against the committed rows — was the runner's substitute for the
  full census; it still works and is worth running by hand, but nothing calls
  it automatically any more (0125).
- **`HV_CENSUS=1`** — regenerate the censuses. Since The Local Census the
  full ~2000-world census is a ~7-min LOCAL run (was ~1–2 h), so this is the
  sanctioned pre-merge refresh — run it via **`census-run.sh`**, not
  `regenerate-artifacts.sh` directly, once per campaign at the close, keeping
  the fixtures current with main. Since decision 0081 all three entry points
  serialize (one heavy writer per box, bounded 45-min wait, `census-run.sh
  status` to ask); the wrapper additionally ledgers the run in
  `docs/timings.md`
  (decision 0063, superseding 0046's AWS-only mandate). `make regen-remote`
  (the AWS box) is abandoned — this box is the single canonical platform
  (decision 0063; AWS differs on ~0.1% of discrete-count metrics).
- **`census-run.sh`** — run a census on THIS box under a `flock`, so triggers
  from either dev machine (over SSH) queue one-at-a-time instead of contending.
  `scripts/census-run.sh` regenerates the canonical goldens; `HV_CENSUS_REF=<ref>
  scripts/census-run.sh` runs a pushed branch in a scratch worktree. Only this
  box authors goldens (the canonical-machine constraint, decision 0063).
- **The script needs FULL git history.** The digest's delta report walks
  `docs/decisions/` back to the commit that put a rule in force, so a shallow
  checkout (`git clone --depth 1`) cannot answer. The renderer says so instead
  of guessing, but the artifact it then writes differs from the committed one
  and the `docs/digest/` drift check goes red for the wrong reason. This used
  to be handled by a `fetch-depth: 0` on the runner's checkout; with 0125 the
  burden is on whoever clones — never regenerate from a shallow clone.
- After regen, the drift check is `git diff` over the paths declared in
  **`docs/generated-paths.txt`** — the single source of truth, which no guide
  restates (`cli/tests/generated_paths.rs` fails on a second copy, because an
  inline list drifts the moment a generated directory is added). Read it:
  `git diff -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')`.
  The notes that follow explain WHY particular entries are in that file; they
  are commentary on it, not a duplicate of it — note
  **`docs/audits/`** is in the list, and it now holds TWO drift-checked
  reports: the type-audit report (drifts on any pub-boundary change — a common
  miss) and **`seam-guard-roster.md`** (drifts whenever a `seam-guard:` tag is
  added, retagged, or has its `expect(survives: …)` declaration changed or
  deleted — which is the point: it puts every acknowledged-unguarded seam in
  front of a reviewer instead of leaving it buried in a doc comment). So is
  **`docs/digest/`** (the
  in-force decision index drifts when a decision is added or superseded, the
  delta report when the idea registry moves) and **`book/src/domesday/`**
  (2026-08-08, The Domesday): it is a pure read over the committed census,
  never a re-run of one, so it drifts whenever that CSV moves — even from a
  census refresh alone, with no other code change.
  **`clients/game/core/tests/fixtures/`** joined it at The Quire (Task 3,
  extended Task 4): the committed seed-42 session snapshots every
  `hornvale-game-core` test that asserts on a **real world** reads instead of
  paying for genesis — one walk-band, one chamber-band (the second exists so
  the `Spatial::Chamber` mirror has committed coverage too, not just the
  walk-band branch turn 0 always lands on). The split is by what a test needs,
  **not** by unit-vs-integration: `src/spread.rs`'s in-module tests read the
  fixtures, and the integration file `tests/cell.rs` does not. So a red test
  tells you nothing about whether a fixture drifted until you check which kind
  it is.
- **THE HAZARD ADDING A NEW GENERATED DIRECTORY EXPOSES**, and the near-miss
  this campaign actually hit: `git diff --exit-code <path>` is silently
  **VACUOUS** against a path git does not track. Regenerate into a brand-new
  directory, add it to the check, and the check passes — not because the
  artifact matched, but because git had nothing to compare. The first commit
  introducing a generated directory MUST `git add` it before the check can
  ever fail. Nothing in `regenerate-artifacts.sh` guards this; verify a new
  path fails by mutating the generated file and confirming the diff goes red.

## The gate ladder

Three gates now exist, named for the campaign moment each one gates rather
than for the machine or the scripts behind it (decision 0132): `gate-commit`
(local), `gate-stage` and `gate-campaign` (both dispatched to the one lane on
the canonical box, decision 0133). The roster of what each lane set runs is
`scripts/lane-sets.tsv`, the single source of truth this section does not
restate.

- **`subfloor-roster.sh`** — emits `gate-commit`'s nextest filter: every test
  below `BASELINE_FLOOR_SECS` (1.0 s) in the committed duration baseline.
  EXCLUDE-UNKNOWN: a test with no baseline row is not selected here; it is
  picked up on the next green stage gate, which measures it and rewrites the
  roster. Exit 3 means no roster exists for this host — a different thing
  from an empty roster, and `gate-commit-run` treats it as a hard failure
  rather than silently gating nothing.
- **`lane-dispatch.sh`** — validates a set name against `scripts/lane-sets.tsv`
  and a `REF` (a **full SHA**, never a branch name — it feeds `reset --hard`
  on the far end, which can otherwise land on a stale local branch of that
  name there), then ssh's to the canonical box and forks a detached
  `lane-run.sh`. It RETURNS as soon as the job is enqueued and never blocks
  the caller. `make gate-stage REF=<sha>` and `make gate-campaign REF=<sha>`
  are thin wrappers dispatching one or more sets through this script.
- **`lane-run.sh`** — runs one set under the **same shared canonical-box
  claim** `heavy-run.sh` and `census-run.sh` already took (decisions
  0086/0133), forked with `setsid` so a dropped ssh costs nothing. Writes
  `<job-id>.log` and appends an outcome row to `jobs.tsv` on every exit path,
  including a signal — read either back with `make lane-log [JOB=<id>]` or
  `make lane-status`.
- **`lane-outboard.sh`** — the driver for the `outboard` set: three suites
  nothing ran before The Staff — `tools/board`, `tools/digest`, and
  `tools/type-audit`'s own suite (distinct from the `type-audit check` lint
  in `gate-commit`). Not fail-fast: independent suites, so it reports every
  failure in one pass rather than stopping at the first. `seam-guard` is
  deliberately not in it — it shipped here first, but measurement showed its
  7 call sites were 97% of this set's wall time on a set that fires at every
  plan-stage boundary, so it moved to its own `campaign`-rung set instead.
- `gate-full-heavy.sh` — the cost-tagged `heavy:` `#[ignore]`d tier that
  `gate-commit` and the stage gate's own suite both defer (see
  `cli/tests/heavy_tier.rs`). Runs as the `heavy` set, dispatched by
  `gate-campaign`. **Takes the shared box claim** (decisions 0086/0133) —
  here, at the seam, rather than only in a wrapper, because a wrapper cannot
  guard a direct invocation of the script. Where there is no `flock` (macOS
  ships none) it proceeds unserialised with a note rather than failing.
- **`heavy-run.sh`** — run the heavy tier on THIS box under the shared claim,
  the same way `census-run.sh` runs a census. `HV_HEAVY_REF=<sha>` runs a
  pushed ref in a scratch worktree; `status` asks who holds the box and is
  legal from any machine — but it reads the claim in the **local** `/tmp`, so
  from the Mac it always says "no". Use **`make heavy-status`** to ask the
  canonical box instead; that is almost always the question you mean. Carries the canonical-host guard, because the tier
  **authors committed artifacts**: `the-history` (`cli/tests/history_battery.rs`),
  `the-sounding` (`windows/chronicle/tests/sounding_sweep.rs`), and
  `occupancy.csv` (`windows/worldgen/tests/occupancy_readout.rs`) — plus
  `census_fixtures_match_a_probe_of_live_seeds`, which compares a live probe
  against lefford-authored fixtures. Review and commit those artifacts **on
  the canonical box**. Dispatch from the Mac with `make heavy-remote REF=<sha>`.
- `test-heavy-lock.sh` — proves the claim EXCLUDES (second acquirer refused
  while held; a normal exit and a `-9` both release), not merely that a lock
  file exists. Skips where there is no `flock`.
- `preflight-merge.sh` — GO/NO-GO before integrating a campaign branch;
  peeks at main's checkout and warns if another session is mid-landing.
- `doctor.sh` — the repo self-map (`make doctor`); good orientation for a
  fresh session.

## `aws-gate/` — billable, admin-credentialed, handle with care

The remote gate provisions real EC2 spot infrastructure. `gate-remote.sh`
runs the commit gate on a spot box; `gate-remote-verify.sh` is the local-vs-remote
**byte-identity acceptance test** (the ratification gate for any
determinism-config change — e.g. Proposed decision 0061). `panic.sh` is the
emergency stop: it **deactivates the runner identity first** (so nothing can
launch), then terminates tagged instances — that order is asserted by
`test/test_panic.sh` and must not be reversed. Setup/teardown are
confirmation-gated in the Makefile.

## Shell conventions

- Every script must pass `make shellcheck` (all of `scripts/**`). Prefer
  explicit `if`/`then` over `A && B || C` (SC2015) — `C` also runs when `B`
  fails.
- Scripts run under `set -euo pipefail` where they can; `|| true` is used
  deliberately where a step is best-effort (e.g. `panic.sh`, safe to run
  repeatedly).
- The git hook in `scripts/hooks/` runs `make quick` pre-commit (`make
  install-hooks` points `core.hooksPath` at that directory — it is the ONLY
  hook; a second, weaker one at the repo root was deleted 2026-07-31, since
  `core.hooksPath` names one directory and the root copy still advertised
  itself in its own header). `make quick` is skipped when nothing
  Rust-relevant is staged (`.rs`, `Cargo.*`, `clippy.toml`,
  `rust-toolchain.toml`, `.cargo/`, `tools/type-audit/`) so docs-only commits
  are instant; the guards below always run. A **linked worktree may not commit
  to `main`** — the primary checkout may. The hook also carries the
  **golden-pins.sql tripwire guard**: staging any of `windows/lab/tests/{calibration,
  branches_family_calibration,gathering_calibration}.rs` or
  `tools/census/queries/calibrate/golden-pins.sql` runs `make census-check`
  (~2.5 min) before the commit lands. That SQL file deliberately duplicates
  every calibration pin as an independent check against the committed census
  fixture, and the duplication went stale twice (2026-07-13, 2026-07-20)
  because nothing forced it back into sync — this closes the gap by
  construction rather than by memory.
- **The board lane rule** (B13, decision 0129): `tools/board/` is a `.rs`
  tree but not a workspace member (`members = ["kernel", "domains/*",
  "windows/*", "cli"]`), so the Rust-relevant filter above would otherwise
  run a full `make quick` that cannot see it, while skipping the 24.4s
  `cargo test --manifest-path tools/board/Cargo.toml` suite that can. When
  the **entire** staged set is under `tools/board/`, the hook runs that
  suite instead of `make quick`; any other staged path — including a
  **mixed** commit that touches `tools/board/` alongside workspace code —
  falls through to the ordinary filter unchanged, so this only ever adds
  coverage and never drops the workspace gate on a change that needs it.
  This rule covers board-only *commits*; it is not a substitute for a gate
  that runs the board's 194 tests on every push — nothing does that (see
  root `CLAUDE.md`'s board paragraph), so a mixed commit still needs `make
  quick` to catch a workspace regression, and neither arm catches a board
  regression introduced by a change that never gets committed at all.
- **Two more pre-commit guards, both tripwires on the rare commit that needs
  them.** The **conflict-marker guard** refuses a commit whose staged diff
  *adds* a `<<<<<<<`/`>>>>>>>` line — anchored on `^+` so an already-committed
  marker elsewhere is not this commit's problem, and read from the staged diff
  so a half-resolved file sitting unstaged never blocks unrelated work. The
  **stream-manifest tripwire** fires when any `src/streams.rs` is staged: it
  regenerates `book/src/reference/stream-manifest-generated.md` and diffs,
  refusing if they disagree. A stream label is a permanent save-format
  contract (`kernel/CLAUDE.md`), the manifest is the only place the whole set
  is visible for review, and nothing otherwise forces the regen into the same
  commit — so the artifact lags the code and the drift check goes red later, on
  someone else's commit. It runs the REAL check rather than the cheap proxy
  ("is the manifest also staged?") because the proxy false-positives on a
  comment-only edit, and a guard that cries wolf is a guard people bypass.
- **`post-merge` is the second hook in this directory**, and it exists because
  **`pre-commit` never runs on a merge** — git does not invoke it for
  `git merge`, so every guard above is blind to the commit shape most likely to
  need one. It is advisory (always exits 0): by the time it runs the merge
  commit already exists, and blocking would only train a `--no-verify` reflex.
  It speaks only when the merge touched a path in `docs/generated-paths.txt`.
  **It does not replace PROC-12's merge driver, which is strictly better where
  it applies** — but that driver only runs when *both* sides changed a file
  *named in `.gitattributes`*, which leaves two holes. First, **most declared
  generated paths are not Tier B at all** — compare `.gitattributes`' driven
  list against `docs/generated-paths.txt` and the gap is most of it; this file
  deliberately does not restate either list, because a second copy drifts the
  moment a directory is added. Second, the
  one-sided stale merge — this branch regenerated, the incoming side changed
  only the generating code — produces a clean merge no driver can see. The hook
  has its own blind spot to match: a merge carrying code changes alone stales an
  artifact just as thoroughly and says nothing. After any absorption,
  `make rebaseline` and read the diff.
- **`git -C <dir>` DOES NOT SCOPE WHICH REPOSITORY GIT ACTS ON**, and wiring
  the board suite into the lane above is how the project learned it. Git runs
  a hook with `GIT_DIR` and `GIT_INDEX_FILE` **exported**, and from a linked
  worktree — where all campaign work happens — they are absolute paths into
  the real repository (`GIT_DIR=/…/.git/worktrees/<campaign>`). `GIT_DIR`
  outranks `-C`, which only sets the working directory. So every
  `git -C <tempdir>` in the board's hermetic-looking tests operated on the
  developer's own checkout: `git init` re-initialised it and guessed **bare**
  (a worktree gitdir does not end in `/.git`, so `core.bare = true` and the
  primary checkout stopped being a working tree at all), `git config`
  overwrote `user.name` with `board test`, the merge helper landed
  `root`/`work`/`merge` commits on `main` plus `campaign/*` branches, and a
  loose-ref write left `refs/hornvale/peers/dangling -> deadbeef…`, which
  broke `git fetch` repository-wide. The fix is
  `tools/board/src/git.rs`'s `Repo::command`, which scrubs
  `GIT_LOCATION_VARS`/`GIT_IDENTITY_VARS` from every invocation; the guard is
  `tools/board/tests/hermeticity{,_env}.rs`; and the belt is the `env -u`
  prefix on the `cargo test` line here. **Any hook that runs a test suite
  touching git needs that `env -u`** — a temp directory is not isolation when
  the environment names the repository.
