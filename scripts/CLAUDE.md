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
  restates (`cli/tests/suite/generated_paths.rs` fails on a second copy, because an
  inline list drifts the moment a generated directory is added). Read it:
  `git diff -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$' | cut -f1)`
  — `cut -f1` takes the path column; the file's second column is the path's
  author, not a pathspec. The notes that follow explain WHY particular
  entries are in that file; they
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

One local gate and one queue with two mouths, named for the campaign moment
each one gates rather than for the machine or the scripts behind it
(decisions 0132, 0139): `gate-commit` (local, seconds), `make sluice-stage`
(the stage gate) and `make sluice` (the merge), both of which are the SAME
queue on the canonical box behind the SAME serial claim (decision 0133).
The queue retires `gate-campaign` — it gates the merge PRODUCT rather
than a branch tip, running the former campaign-gate phases against the
actual merge commit before pushing it. A stage request is that same run with
`kind=stage`: it merges and gates, runs the `stage`-rung phases, and never
pushes. The roster of what each set runs is
`scripts/lane-sets.tsv`, the single source of truth this section does not
restate.

**`gate-stage`, `preflight` and the five `make lane*` targets are gone**, as
are `scripts/lane-dispatch.sh`, `lane-run.sh`, `test-lane.sh` and
`preflight-merge.sh` (The Sluice, Task 12). What they were the front end of —
an asynchronous dispatch layer serving a caller on another machine — is what
the queue replaced; the `flock` claim they took is not, and survives
unchanged. `scripts/lane-sets.tsv`'s `where` column still reads `lane` for
exactly that reason: it names the claim, not the deleted machinery.

- **`subfloor-roster.sh`** — emits `gate-commit`'s nextest filter: every test
  below `BASELINE_FLOOR_SECS` (1.0 s) in the committed duration baseline.
  EXCLUDE-UNKNOWN: a test with no baseline row is not selected here; it is
  picked up by the next green chamber `gate` phase, which measures it and
  rewrites the roster — genuinely, since The Sluice fixed `ci-record`'s
  refusal-under-its-own-ancestor's-claim (root `CLAUDE.md` carries the whole
  account); the chamber then commits the rewritten roster with the merge
  product. Exit 3 means no roster exists for this host — a different thing
  from an empty roster, and `gate-commit-run` treats it as a hard failure
  rather than silently gating nothing.
- **`lane-outboard.sh`** — the driver for the `outboard` set: three suites
  nothing ran before The Staff — `tools/board`, `tools/digest`, and
  `tools/type-audit`'s own suite (distinct from the `type-audit check` lint
  in `gate-commit`). Runs as a chamber phase of both a stage gate and a
  merge. Not fail-fast: independent suites, so it reports every
  failure in one pass rather than stopping at the first. `seam-guard` is
  deliberately not in it — it shipped here first, but measurement showed its
  7 call sites cost 853.284 s — 99.8% of this set's 855.222 s wall time (the
  `lane:seam-guard` row in `docs/timings.md`, not the standalone probe some
  earlier notes cite) — on a set that fires at every plan-stage boundary, so
  it moved to its own `campaign`-rung set instead.
- `gate-full-heavy.sh` — the cost-tagged `heavy:` `#[ignore]`d tier that
  `gate-commit` and the `gate` set's own suite both defer (see
  `cli/tests/suite/heavy_tier.rs`). Runs as the `heavy` set — either standalone via
  `make heavy-remote REF=<sha>`, or as the LAST of the chamber's phases
  (`sluice-run.sh`, below). **It is a phase of a MERGE and not of a stage gate
  (decision 0426).** That is the pre-0148 arrangement restored: 0148 took it
  off the merge list, where it had lived, and it has never been on the stage
  list at all. The Governor then cut the tier 3.52x (1551.631 s -> 440.269 s
  nextest wall, 118 -> 64 tests, 10 -> 0 failures, measured at `e76ea0497`;
  that campaign's final review restored one test and a re-measure confirmed it
  cost nothing), which is what makes it affordable at
  ~29% of a ~1595.3 s merge — both figures derived once in decision 0426 and
  not restated in a second form here. It stays off the stage list because
  `census_fixtures_match_a_probe_of_live_seeds` compares a live probe against
  committed census fixtures refreshed once per campaign at pre-merge close, so
  a stage gate would red predictably for the whole middle of any
  world-touching campaign. **Takes the shared box claim** (decisions 0086/0133) — here,
  at the seam, rather than only in a wrapper, because a wrapper cannot guard
  a direct invocation of the script. Where there is no `flock` (macOS ships
  none) it proceeds unserialised with a note rather than failing.
- **`heavy-run.sh`** — run the heavy tier on THIS box under the shared claim,
  the same way `census-run.sh` runs a census. `HV_HEAVY_REF=<sha>` runs a
  pushed ref in a scratch worktree; `status` asks who holds the box and is
  legal from any machine — but it reads the claim in the **local** `/tmp`, so
  from the Mac it always says "no". Use **`make heavy-status`** to ask the
  canonical box instead; that is almost always the question you mean. Carries
  the canonical-host guard, because the tier **authors a committed artifact**:
  `the-history` (`cli/tests/suite/history_battery.rs`) — plus
  `census_fixtures_match_a_probe_of_live_seeds`, which compares a live probe
  against lefford-authored fixtures. **This bullet used to name three
  writers**; The Governor (2026-08-28) demoted `sounding_sweep` out of the
  tier, and `occupancy_readout_is_current` only ever *compared* against
  `occupancy.csv` — its writer, `regenerate_occupancy_readout`, was never
  `heavy:` at all, so counting it here was wrong rather than merely stale.
  Review and commit the artifact **on the canonical box**. Dispatch from the
  Mac with `make heavy-remote REF=<sha>`.
- `test-heavy-lock.sh` — proves the claim EXCLUDES (second acquirer refused
  while held; a normal exit and a `-9` both release), not merely that a lock
  file exists. Skips where there is no `flock`.

### The merge queue (`make sluice`; decision 0139, The Sluice)

Retires `gate-campaign`, which gated a BRANCH TIP — the merge that actually
lands, that tip merged into whatever main is at merge time, was never itself
built or tested, which is how two campaigns both minted decision 0134
through a green gate. The queue gates the merge PRODUCT instead and pushes
the exact SHA it tested.

- **`sluice-queue.sh`** — the merge queue's durable state: an
  append-and-rewrite TSV under its OWN flock, deliberately not the shared
  lane claim, so enqueueing never blocks behind a running gate (a caller
  should not wait tens of minutes just to write one line).
  **IT IS NO LONGER A PURE-BASH STATE MACHINE.** `claim`, `set-state` and
  `list` are a THIN SHIM that `exec`s `tools/sluice`, a Rust binary outside
  the cargo workspace; only `add` is still bash (it sources
  `sluice-headline.sh`, resolves three-valued ancestry and coalesces, all of
  which shell out to git). The binary is resolved as a sibling of the script's
  own location, so a caller's cwd cannot change which one runs;
  `HV_SLUICE_BIN` overrides it and is a TEST SEAM ONLY, set by nothing in
  production. **THE SHIM NEVER BUILDS IT.** An earlier version did, with
  `cargo build`, under the script's own `set -euo pipefail` — so a build
  failure (observed: rustup resolving an older, non-overridden toolchain from
  a caller's cwd and choking on the edition2024 manifest) aborted the whole
  script with rc=101 before `exec` ever ran, and all three callers read that
  as something else. It now follows `scripts/board-render.sh`'s precedent:
  prefer a prebuilt release binary and deliberately never compile it. `make
  prewarm` builds it for a fresh worktree; nothing else does.
  **EXIT 3 is reserved** for "no binary and no way to reach one", distinct
  from every code the script or the binary otherwise uses (1/2/4/5), so a
  caller can tell "the queue refused" from "the queue could not be asked" —
  and all three callers now do: `sluice-run.sh` and `sluice-census.sh` refuse
  with rc=13 rather than running unbookkept, and `sluice-drain.sh` returns
  non-zero rather than reporting a queue it never reached as drained. Coalesces by
  ANCESTRY (`git merge-base --is-ancestor`), not branch name, so a rebase or
  a detached ref still supersedes correctly — and never supersedes a request
  already RUNNING inside the chamber, which would otherwise orphan it
  mid-write. Its `kind` column (`merge` | `stage`) is how the stage gate was
  absorbed without a second code path; coalescing is scoped to the kind as
  well as the branch, or a stage request would silently drop a queued merge
  on the same branch. `reported` is the stage kind's terminal state —
  separate from `landed`, which would assert main moved when it did not.
  **`claim` is the transaction; `next` is only a read.** The row state IS the
  interlock that decides who runs a job, so selecting a row and marking it
  `running` must happen under ONE lock acquisition — the flock is released at
  process exit, so two `sluice-queue.sh` invocations are two transactions with
  a window between them. `next` + a separate `set-state running` left that
  window open across the caller's whole mouth check, and on 2026-09-04 it let
  a hand-run drain dispatch a merge a direct `sluice-run.sh` was already
  executing (pids 1240741/1253175, two ~800 KB logs for `48aa9373b6f2`, both
  rc=0). `claim` selects and marks in one locked pass. Without `--sha` it
  takes the first queued row (a dispatcher); with `--sha` it takes the row for
  that ref (an executor, which knows its ref and not its id) and answers with
  two distinct exit codes that callers depend on: **4** = a row exists but is
  not queued, somebody else has it, refuse; **5** = no row at all, which is an
  ad hoc run and is allowed. Conflating those two would either forbid the
  operator escape hatch or re-open the duplicate.
- **`sluice-mouth.sh`** — the checks that run OUTSIDE the lane claim (the
  canal-lock rule: turn a vessel away at the gate, never inside the
  chamber). Prevents a doomed candidate — already merged, unpushed, or
  genuinely conflicting — from consuming the strictly serial lane's queue
  wait (903-1823 s/job, measured over its first 46 jobs). Five-valued exit
  (0 admit / 1 conflict / 2 invalid-or-unpushed / 3 already-merged / 4
  out-of-band), with out-of-band deliberately outranking already-merged so a
  broken induction is never silently read as "nothing to do".
- **`sluice-run.sh`** — the chamber; runs ON the canonical box under the SAME
  shared claim `heavy-run.sh` and `census-run.sh` take (decisions
  0081/0086/0133), so the queue pays for one job, not the six separate
  dispatches a campaign gate used to cost (67% of the lane's first 27.4 h of
  wall time was queue wait for exactly that reason). Merges the candidate,
  then runs `artifacts outboard gate clients heavy` against the real merge
  commit before pushing it, so a broken interaction with main is caught before
  it ever reaches main. A STAGE GATE RUNS THE SAME LIST MINUS `heavy`.
  **The merge list lost `seam-guard` and `heavy` on 2026-08-19 (decision 0148)
  and got `heavy` back on 2026-08-28 (decision 0426)**, after The Governor cut
  the tier 3.52x. `seam-guard` keeps its `campaign`-rung row and its own entry
  point (`make seam-guard`), and that is still the ONLY thing that runs it —
  nothing does so automatically. `heavy` keeps `make heavy-remote
  REF=<full-sha>` as a by-hand entry point, and that is still the only way to
  run the tier at a plan-stage boundary, but the chamber now dispatches it on
  every non-prose merge candidate. The merge product is gated as itself, by
  five phases.
  (`census` refuses as a chamber phase for an unrelated reason: it
  unconditionally clobbers the shared claim on exit.) A `kind=stage` run is the same code with one
  branch turned the other way at the push step: it merges, runs the
  `stage`-rung phases, reports, and exits 0 without pushing — placed AFTER
  the single failure gate, so `kind=stage` can never launder a red run into
  a green one. Unsets `GIT_DIR`/`GIT_INDEX_FILE` once near
  the top (the board-incident hermeticity lesson above), so no child process
  spawned mid-run can silently operate on a different repository.
  **It also takes a bare `req-<id>` in place of branch/sha/kind**, which is how
  the drain dispatches it — and an id is NOT a claim: it reads the row's state
  and refuses **rc=16** unless the row already reads `running`. Running an
  unclaimed row is the 2026-09-04 duplicate by another road (a `queued` row is
  what the next dispatcher's `claim` hands out, and what coalescing may
  supersede mid-write), and it writes no terminal state, so the row becomes a
  permanent ghost. An unreachable queue — `sluice-queue.sh`'s exit 3, above —
  is **rc=13** on both the id and the positional path, never the shim's own
  rc=3, which is outside this script's vocabulary (2/9/10/11/12/13/14/15/16/75)
  and which the drain records as `CHAMBER RED rc=3 — attribution pending`,
  blaming the candidate for a toolchain fault.
- **`sluice-request.sh`** — the caller's side; validates and ssh's, then
  RETURNS without waiting, in the shape the deleted `lane-dispatch.sh`
  established including its two
  hard-won guards (a full 40-char SHA, and a preflight kept outside the
  backgrounded segment so a dispatch that never started cannot report
  success). It also carries the board's HOLD-OFF ADVISORY, inherited from
  `preflight-merge.sh`: submitting is now the moment work asks to integrate,
  which is when another session's `hold-off` matters. Advisory only, never
  fatal, and skippable with `HV_SLUICE_SKIP_BOARD=1` (which
  `scripts/test-sluice.sh` sets for the whole file, so a test run never
  pushes a board ref to the real `origin`).
- **`census-duration-alarm.sh`** — reports a census run's wall against the
  alarm threshold AT THE MOMENT IT IS MEASURED, called from `census-run.sh`.
  **`cli/tests/suite/census_duration.rs` is the ratchet and it reads
  `docs/timings.md` on `main` — which a census run's row no longer reaches
  promptly.** Since the census joined the merge queue (2026-08-27) the row is
  committed onto the run's `census/*` DELIVERY BRANCH with the goldens, so it
  lands on main only when a campaign merges that branch, which may be days
  later or never. Measured that day: four runs, three past the threshold, and
  the ledger on main still showing a two-day-old figure as its most recent
  census — the alarm could not see the runs that would have tripped it, while
  the file the root guide tells you to consult for census cost read as current.
  This is the live half; the Rust test still guards committed history. It reads
  `CENSUS_ALARM_SECS` OUT of the Rust source rather than restating it (the
  `test-census-guard.sh` discipline), and if it cannot find that constant it
  **refuses to report a threshold** rather than inventing one — a bound stated
  in a message is a claim, and a made-up one is worse than silence. Exit 0
  always: a census that ran long is not a census that failed.
- **`sluice-drain.sh`** — the operator's harness: CLAIM the next row, gate it,
  dispatch on `kind`, set the terminal state, repeat. It claims (not `next`)
  and then dispatches the runner with **the request ID and nothing else**
  (`bash "$runner" "$ID"`), so the runner reads branch/sha/kind out of the row
  that authorised it rather than trusting positional arguments. **There is no
  exported claim-id environment variable any more** — The Sluicegate deleted
  it, and the spec's success criterion is that its NAME appears nowhere under
  `scripts/` or `tools/`, which is why this paragraph describes it rather than
  spelling it. This bullet asserted the export for the whole life of its
  replacement. The variable meant "your row is already claimed", and it leaked
  into the whole phase tree: a nested `sluice-run.sh` — one invoked from inside
  `scripts/test-sluice.sh`, itself running as an `outboard` phase — inherited
  it, believed its own row was claimed, and skipped the interlock entirely.
  The id is an argument now, so it dies with the process's argv. **And the id
  is not itself a claim**: the runner reads the row's STATE and refuses (rc=16)
  unless it reads `running`, because an id proves only that the caller can
  type. **It is no longer the only bookkeeper**, and that is the point:
  `sluice-run.sh` and
  `sluice-census.sh` each claim their own row when nothing claimed it for
  them, and set their own terminal state when they did. A run launched by hand
  is a documented escape hatch; a run launched by hand that leaves its row
  reading `queued` is a ghost the next dispatcher will launch a second time. Every script it calls was
  committed and tested; **this one lived in a session scratchpad for weeks**,
  ungated and untested, while doing real gating work — and caused two defects in
  one night (2026-08-27). Promoted with its two decision rules extracted as
  functions (`mouth_applies_to`, `dispatch_for`) precisely so they could be
  tested; `HV_DRAIN_LIB=1` sources it for those functions without draining.
  **It never decides whether a job SHOULD run** — a redundant census, a decision
  minted outside its block, a schema bump that breaks a consumer are all invisible
  here and must stay so. It decides ORDER and MECHANISM; vetting is the operator's.
  Two rules it encodes, both learned the hard way: a `census` is exempt from the
  mouth (a merge-conflict verdict cannot speak to a job that never merges main —
  it refused campaign/the-sources over conflicts in files a census does not read),
  and a `census` never goes to `sluice-run.sh` (census-run.sh takes the shared
  claim itself and deletes the claim file on exit, so nesting it clobbers the
  outer job's own claim). An unknown `kind` is gated and sent to the chamber —
  failing toward the check.
- **`test-sluice-drain.sh`** — tests those two rules, their negative controls,
  and that they AGREE about what a census is. The agreement test is the load-
  bearing one: the original defect was not either rule alone but the two
  disagreeing, one exempting a census while the other did not. Mutation-tested
  in both directions (gate-everything, and dispatch-census-to-the-chamber); each
  mutant kills two assertions.
- **`test-sluice.sh`** — property tests for the queue, shaped after
  the deleted `test-lane.sh`: pins the properties the queue would be worthless without
  (flock ordering, coalescing by ancestry, never superseding a running
  request, the mouth's five-valued exit semantics) rather than merely
  asserting a lock file exists, and SKIPS (not fails) on a host without
  `flock`, since `sluice-queue.sh` only ever runs on the canonical box.

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

## Utility scripts

- **`mutate.py`** — applies ONE text substitution to a source file and
  **refuses to no-op silently**: it dies unless the search text occurs, and
  occurs exactly once, leaving the file byte-unchanged on either refusal.
  `python3 scripts/mutate.py <file> <old> <new>`. Use it for **every mutation
  demonstration** — neutralise a line, run the scoped suite, read the red as
  proof an assertion is really holding the behaviour. That procedure's entire
  value rests on the substitution having happened, and this project has got
  that step wrong three times in two campaigns (The Axes, then The Underworld
  twice): `cargo fmt` had rewrapped the line a `sed` was searching for, the
  file was left untouched, the suite reported `ok`, and that `ok` was
  indistinguishable from a robust implementation. It deliberately does **not**
  restore — `cp` from a copy taken before the first mutation, never
  `git checkout -- <file>`, which reverts your uncommitted work along with the
  mutation and makes the resulting absence read as a pass. Not part of any
  gate and not mandatory; whether an ad-hoc `sed` is ever acceptable for a
  mutation demo is an open call recorded in The Underworld's retrospective.
- **`shapecheck.py`** — compares the key-path SHAPE of two JSON documents
  (dicts/lists/scalars, values ignored), so a drifted byte-golden's diff can
  be answered structurally rather than by eyeballing a large single-line
  file: did the wire shape move, or only a value? Standard-library only, no
  venv, no install. The Rhumb used it to establish that a drifted
  `vessel/session/v2` fixture had 133 identical key paths on both sides with
  1 of 888 leaf values differing (`.narration.prose`), so the golden's move
  was a value, not a schema change. Not part of any gate — it informs the
  human call a golden's drift always needs (epoch vs. moved value), the same
  judgment `CLAUDE.md`'s "Deliberate regeneration uses an epoch suffix"
  line names. Run it via `make shapecheck OLD=<path> NEW=<path>` or
  directly: `scripts/shapecheck.py OLD.json NEW.json` (exit 0 identical
  shapes, 1 differ, 2 usage/parse error).

## Shell conventions

- Every script must pass `make shellcheck` (all of `scripts/**`). Prefer
  explicit `if`/`then` over `A && B || C` (SC2015) — `C` also runs when `B`
  fails.
- Scripts run under `set -euo pipefail` where they can; `|| true` is used
  deliberately where a step is best-effort (e.g. `panic.sh`, safe to run
  repeatedly).
- `make install-hooks` points `core.hooksPath` at `scripts/hooks/` — the
  ONLY hooks directory; a second, weaker one at the repo root was deleted
  2026-07-31, since `core.hooksPath` names one directory and the root copy
  still advertised itself in its own header. That directory now holds three
  hooks: `pre-commit`, `post-merge`, and `pre-push` (see below for the
  latter two). `pre-commit` runs `make gate-commit` (not `make quick` —
  that was true when this line was written and the hook has since moved up a
  rung), gated on a Rust-relevant staged path: `.rs`, `Cargo.*`, the lint and
  toolchain configs, `.cargo/`, and the two audit tools whose own behaviour
  changes what the gate says. The exact predicate is the `rust_relevant`
  regex in the hook; read it there rather than from this list, which has
  drifted from it once already.

  **A docs-only commit is no longer instant, and that is deliberate (The
  Nettle).** The old premise was "Rust paths staged -> Rust checks matter";
  the counterexample is a Rust test whose SUBJECT IS PROSE. `docs_consistency`
  scans `book/src` and `docs/`, sits in the sub-floor tier, and so never ran
  on the commits it exists to check — one campaign made ~12 such commits and
  sent a prose defect to the merge queue to be found on the canonical box
  after it had taken the shared serial claim. The fast path is now **inverted
  rather than deleted**: no Rust-relevant path staged runs `make docs-tests`,
  the prose-subject tests, and still skips fmt/clippy/type-audit, which a
  markdown edit genuinely cannot move. Measured 5.7-24.0 s warm, the pole
  being three worldgen byte-identity probes; the guards below always run
  regardless. A **linked worktree may not commit
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
  `make rebaseline` and read the diff. **Tested by `scripts/test-post-merge.sh`
  since The Attestation** — added the same way `test-pre-push.sh` was, after
  Task 3 of that campaign added a second column to
  `docs/generated-paths.txt` and this hook's own pathspec array (a quoted
  `"${generated[@]}"` expansion, unlike every other reader's unquoted
  `$(...)`) went silent on every merge for all ten declared paths, and
  nothing exercised the hook to catch it. Confirmed to fail red against the
  pre-fix hook before being wired into the `outboard` set.
- **`pre-push` is the third hook in this directory**, added after an
  incident (2026-08-16): a subagent force-pushed campaign WIP over
  `origin/main` while probing bash quote-splitting semantics, and its
  dispatch had named that exact prohibition as the single most important
  constraint in prose. Prose in a prompt is not a control; this hook is,
  because `core.hooksPath` is repository-level and fires for every push from
  every session with no opt-in, and the chamber (`sluice-run.sh`) disables
  hooks only for its own `commit`, never for a `push`. It gates the
  **destructive class only** — a delete (local sha all zeros) or a
  non-fast-forward (force push: the remote sha is non-zero and not an
  ancestor of the local sha) — and only when the remote is not a local path
  (`file://` or a filesystem path, so scratch bare repos and tests stay
  unrestricted). An ordinary fast-forward to a **branch** is deliberately NOT
  gated — that is how a candidate reaches the queue. **`main` IS gated now
  (2026-08-19), and this sentence used to say it was not.** The old text read
  "an ordinary fast-forward, including to `main` … that is Nathan's normal
  no-PR workflow"; decision 0139 made the merge queue the only route by which
  `main` advances, and the failure then happened twice — a push that killed a
  green candidate mid-merge, and `ca6f34310` landing with no queue row at all.
  A push to `refs/heads/main` is allowed only from inside a **live claim on
  the canonical box**, which is the chamber and nothing else. Not a hostname
  test: the chamber runs on lefford and so does a human pushing by hand there,
  so "am I on lefford" would have allowed the exact landing that prompted it.
  Refuse unless `HV_PUSH_OK=1` is set; the refusal message says so, because a
  guard nobody can satisfy is a guard people work around. **Ancestry that cannot be
  verified locally (a shallow clone, or a remote sha this side has never
  fetched) fails CLOSED, deliberately** — checked with an explicit
  `git cat-file -e <sha>^{commit}` before ever calling `merge-base
  --is-ancestor`, not left to fall out of that command's own error handling.
  Mutation-tested (`scripts/test-pre-push.sh` — which **nothing ran** until it
  was added to the `outboard` set on 2026-08-19; before that this very
  sentence asserted a property whose evidence was produced only when someone
  remembered to type the command): with that explicit check
  removed, the same scenario still refuses in practice, because `git
  merge-base --is-ancestor` exits 128 — still nonzero — on an object it does
  not have; the explicit check exists for a deliberate, distinguishing
  refusal message, not because the bare command would otherwise fail open.
  The exact incident shape (`21847b08` -> `fb71dd2e` on `refs/heads/main`,
  where the old sha is not an ancestor of the new one) is a named test case,
  not merely an illustrative one — both objects are real commits in this
  repository. **Never test this hook against a real network remote**: drive
  it directly (`$1`/`$2` args, ref lines on stdin, exactly as git does) for
  the refusal cases, and use a `file://` bare repo under `mktemp -d` for the
  allow cases that need a real push.
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
