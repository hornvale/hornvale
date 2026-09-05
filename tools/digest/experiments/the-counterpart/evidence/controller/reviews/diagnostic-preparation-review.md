# Counterpart diagnostic preparation: scoped review

Spec verdict: PASS for Q11 diagnostic transport repair.

Quality verdict: PASS; no actionable findings within this scoped change. Ready for a queued canonical diagnostic. CANNOT VERIFY Linux preparation or full behavioral qualification until the actual queued run. This does not accept Task 5/6 behavioral evidence.

## Reviewed scope

Commit `e6d96a5fc0603576ce3e20b25fa67fb3c41a93c5`, based on `226f145e56894d853e1af5fc4dbd73bbb078323d`. Actual diff changes only `scripts/lane-outboard.sh`, adding 57 lines. Read the exact code, existing `run.capture`, `run.reconstruct`, `run.verify_source`, `run.validate_sample` and Charter measurement/signal implementation, scripts guidance, controller ledger Q11 and original retained failure, and the author's completed local report/receipts. No tracked file was edited and no build, full panel, remote job, test suite or commit was run by this reviewer.

## Verified behavior and evidence

The transport adds separately measured `cargo fetch --locked --manifest-path tools/digest/Cargo.toml` in a newly reconstructed owned frozen-base clone, after successful normal outboard checks and existing self-tests. New persistent mktemp evidence parent, preparation clone and subsequent dossier output prevent reuse of the failed scratch. Source is reconstructed from repository history plus the retained bundle and explicit prerequisite. Full commit/tree and input hashes match the frozen base before fetch; tracked cleanliness/source/tree and input hashes are rechecked afterward. Preparation cannot silently reconcile locks or ship source changes.

Both signals are routed to the existing supervisor. All Git preparation and fetch subprocesses use the existing bounded capture implementation (3600-second deadline; independent 16-MiB stream polling thresholds, with documented possible overshoot). Crucially, capture persists the exact sample before raising on interruption/cleanup uncertainty; the inline body therefore cannot proceed to its later Git checks or assay after unsafe cleanup. Ordinary nonzero/deadline/bound failures are validated before leaving preparation, and the shell exits on Python failure. Available evidence and owned directories are retained. No new process supervisor or cleanup implementation is introduced.

The unchanged full CLI follows only after preparation succeeds, into the new dossier path. Its metadata/build requests remain locked/offline, without target filtering. Frozen runner/checker/rules/panel/owners/bundle and source objects are unchanged. Diagnostic scope is explicit in the script and author report; this branch is not a merge candidate or permanent gate amendment.

The original Linux first-base metadata record remains retained with exit 101 and error identifying unavailable windows-link v0.2.1 under offline mode. Its archive hash matches its receipt; the archive contains no build.json or observe.json. It has not been rewritten into a success or used as a new source checkout.

## Actual read-only verification output

Placement:

```text
/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-diagnostic
codex/counterpart-prepared-diagnostic
```

Inspected local evidence directory:
`/Users/nathan/.local/state/hornvale/counterpart-prepared-mac-28lmcls8`.

```text
Exact exercised body equals committed heredoc: True
Body SHA256: 3544804703520d028b873b5762c7ffe5466cb27f2d003b5a94e0e1910c6e4e52
dependency-fetch.json validated; exit= 0 elapsed= 0.1499795001000166 command= ['cargo', 'fetch', '--locked', '--manifest-path', 'tools/digest/Cargo.toml']
offline-metadata.json validated; exit= 0 elapsed= 0.18385399994440377 command= ['cargo', 'metadata', '--locked', '--offline', '--format-version', '1', '--manifest-path', 'tools/digest/Cargo.toml']
Packages: 53 windows-link: ['0.2.1']
Before/after source equals frozen base: True
Before/after inputs equal frozen base: True
Only changed path: scripts/lane-outboard.sh
Original Linux failure archive hash matches: True
```

The validation above decodes and verifies both command samples' raw hashes, sizes, success and failure flags with the existing validator. It does not rerun fetch or metadata. The qualification receipt records zero specimen builds and zero observations, consistent with the exact exercised preparation body and the metadata-only follow-up.

Author's completed report supplies successful shellcheck, bash syntax and diff checks plus ordinary hook evidence (`75 passed, 246 skipped`); these were not rerun. The implementation commit shown by git is `e6d96a5fc0603576ce3e20b25fa67fb3c41a93c5` and initial git status was clean.

## Limits

Mac cache qualification proves the exact body ran successfully on that prepared Mac; it does not establish Linux cache preparation. The explicit fetch warms the shared Cargo cache rather than claiming hermetic cache closure. Canonical queued run, full 11-arm observations, reserved challenge and independent replay remain pending. Q11's preparation prerequisite must remain explicit in replay instructions and cost accounting; no accepted checks or scoring rules are changed by this repair.
