# The Charter: composition and quality evidence

Status: **pending integrated adopter and supported-host evidence**. This document
is an evidence ledger, not a passing verdict. The binding acceptance properties
are [design §7–8](../superpowers/specs/2026-09-04-the-charter-design.md).
The [tool guide](../../tools/digest/README.md) explains the actual command and caps.
Saved reports below are examples; the tool never reloads them as fresh evidence.

## Sources and independent enrollment

| Item | Exact source / observation |
|---|---|
| Frozen bootstrap | `6fe554d0ebc6e2bd847ad224a9eb8b1b2e877b1f` |
| Task 5 preparation base | `28fafd81c14ae08cfc686b30199264d9d4e47187` |
| Reviewed Thing commit and merge-base | Pending controller integration record |
| Reviewed census commit and merge-base | Pending controller integration record |
| Full changed-file rosters and shared-file classification | Pending controller integration record |
| Cargo.lock reconciliation and dependency expansion | Pending Cargo resolution and inspection |
| Composed implementation SHA | Pending reviewed integration |
| Measured Mac script/host SHA | Pending committed diagnostic and run |
| Canonical diagnostic candidate SHA | Pending stage-only diagnostic branch |
| Canonical tested merge-product SHA and complete log | Pending completed queue job |
| Diagnostic-only diff | Pending; only the script invocation in the disposable branch's outboard command is permitted |
| Later evidence documentation commit | Pending; distinct from measured source |

Independent adoption is established by the actual merge bases, full diffs and
composed execution, not by a clean textual merge. A contributor-specific semantic
host/protocol edit would fail that experiment and reopen the design. Ordinary
hook timing/artifact changes must be classified and reconciled explicitly.

## Acceptance properties

Pending entries below are required evidence, not implied passing checks. Exact
commands, output and source SHA will replace these placeholders after integration.

| Design §7 property | Command / evidence | Observation and limitation |
|---|---|---|
| Independent enrollment | Pending branch `git merge-base` and `git diff --name-only` records | Bootstrap above is fixed; both reviewed package branches are still required. |
| Real Thing evidence | Pending adapter tests and real `digest context domains/thing` report | Must discriminate wrong/missing lender, missing registration and roster disagreement through actual APIs; no worldgen/save claim. |
| Guard discrimination | Pending adapter tests and real `digest context windows/lab/src/publish.rs` report | Must detect over-admission, over-refusal and current-source/compiled-host mismatch; no queue execution claim. |
| No vacuous success | Pending exact protocol/CLI test output | Include missing required observation, malformed/empty response, protocol/identity/reference errors and unmatched scope. |
| Compositional determinism | Pending exact composition tests plus re-rendered actual collected envelopes | Fix checkout context and retain semantic inner order; request timings stay outside report text. |
| Fresh invocation boundary | Pending saved-report refusal and isolated local-input mutation | Assert mutation took effect and distinguish behavioral failure from compilation failure. |
| Existing behavior | Pending integrated all-package tests/clippy/fmt, renderer commands and generated diff inspection | A predicted empty diff is not evidence of unchanged artifacts. |
| Process lifecycle | Pending Mac and canonical fixture test logs | Require direct child waited, no running invocation descendants and finished readers; no portable grandchild-reaping claim. |
| Useful cost | Pending all six workload/host series below | Context must invoke only pure contributors; no simulation suite, world construction or live census. |

## Quality scenarios

| Design §8 scenario | Exact command / evidence | Qualification and limits |
|---|---|---|
| `charter.isolation` | Pending host fixture tests, two roots and hostile Git/target overrides | Harness ownership self-tests are narrower and do not replace host isolation evidence. |
| `charter.determinism` | Pending fixed-envelope composition tests | Identical CLI hashes alone do not prove envelope-order invariance. |
| `charter.recovery` | Pending both-host live parent/grandchild lifecycle logs | Timeout, cancellation, overflow and contributor error all need failure evidence and cleanup. |
| `charter.resource-bounds` | Pending boundary/overflow tests and sequential real composition | Initial caps remain policy, not latency or process-tree memory guarantees. |
| `charter.cost-attribution` | `bash scripts/charter-measure.sh`; actual runs pending | Retain every failure. Whole-phase lock waiting versus compilation remains unavailable. |
| `charter.extension` | Pending full adopter diffs, bases and integration review | No semantic dispatcher edits may be concealed as integration. |
| `charter.diagnostics` | Pending CLI diagnostics plus real failure report | Identify phase, contributor/requirement where known, actionable cause; no completed report before validation. |
| `charter.portability` | Pending integrated all-package Mac and canonical outboard logs | Supported qualification hosts are macOS and Linux; context alone rejects unsupported OSes. |
| `charter.usability` | Pending independent review of two actual reports and one failure report | Reader must locate authored claims, named observations, failed obligation and owning source without conversation history. |

## Measurement method and all samples

Run `bash scripts/charter-measure.sh` from the committed candidate. It emits
`charter-measure` JSON lines, including source, pinned and selected toolchain,
host, load, exact argv/cwd, elapsed, exit status, raw time/phase diagnostics and
available RSS. It scrubs all six inherited Git path overrides for script-owned
Git operations. Each workload receives a fresh owned detached worktree; its
tracked Rust pin remains present and `tools/digest/target` starts absent.

Each series is **one host build + one selected cold request + ten warm requests**.
Do not count `make context-prepare` as a selected-workload cold sample: it builds
all Digest members. Cold here does not mean an empty dependency or OS cache.
Warm attempts carry `prepared_before_request`; failures cannot silently establish
preparation. No run deletes or redirects an active checkout's target cache.

| Host | Workload / scope | Cold host build | Cold selected request | Warm 1–10 |
|---|---|---|---|---|
| Mac | Thing / `domains/thing` | Pending | Pending | Pending, 0/10 measured |
| Mac | census / `windows/lab/src/publish.rs` | Pending | Pending | Pending, 0/10 measured |
| Mac | combined / `.` | Pending | Pending | Pending, 0/10 measured |
| lefford Linux | Thing / `domains/thing` | Pending | Pending | Pending, 0/10 measured |
| lefford Linux | census / `windows/lab/src/publish.rs` | Pending | Pending | Pending, 0/10 measured |
| lefford Linux | combined / `.` | Pending | Pending | Pending, 0/10 measured |

Mac `/usr/bin/time -l` RSS is bytes; Linux GNU Time RSS is KiB. The diagnostic
retains raw values and normalizes to bytes. These are each utility's available
maximums, not aggregate simultaneous process-tree memory. Missing memory on a
failed/interrupted run is recorded as unavailable. Per-phase timing does not
separate lock waiting, compilation and scheduling. The diagnostic's wall timer
includes process launch/polling and excludes its subsequent ownership cleanup;
it is not a substitute for the raw utility output.

A failed host build makes that workload's requests unavailable; other workloads
still run. A failed request remains a numbered sample. SIGINT/SIGTERM terminate
owned work across the measurement session, including nested process groups,
before worktree removal. A cleanup failure emits the complete available failed
sample before aborting later work, and retains its raw files and the owned
worktree with explicit paths. Output is retained whenever a sample is unsuccessful,
including zero-exit interruption. Children must not detach into another session.
The diagnostic does not weaken the host's 5 s contributor / 600 s machine-phase
caps or its 250 ms termination grace. Its own one-hour command safety deadline
is separately labeled.

Canonical measurements are pending ordinary serialized stage-only queue work.
No side-channel SSH compute job is part of this evidence. The diagnostic-only
branch is never merged back; the submitted candidate and the actual tested
merge product must both be recorded. Unavailable logs remain unavailable results.

No steady-state performance claim is earned yet. Propose a target only after
both-host series are present, retaining workload/host spread and all failures.
The approved operational caps remain unchanged.

## Actual report review and remaining evidence

The two scoped reports, one representative failure report, independent reader
findings, corrections and limitations are pending reviewed integration. Dependency
expansion, report sizes, generated-file inspection, stage result and final review
also remain pending. Routine prewarm costs are separate workflow observations;
they must not be folded into these context cold/warm samples.
