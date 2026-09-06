# Counterpart prepared diagnostic transport

DONE: diagnostic-only locked preparation transport qualified and committed for scoped review. Nothing was pushed, enqueued, remotely executed or merged. The frozen experiment implementation is unchanged. The full assay remains the controller's canonical job.

Placement evidence:

```text
/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-diagnostic
codex/counterpart-prepared-diagnostic
```

Commit: `e6d96a5fc0603576ce3e20b25fa67fb3c41a93c5`. Based on controller `226f145e56894d853e1af5fc4dbd73bbb078323d`. Only `scripts/lane-outboard.sh` changed, with 57 added lines after normal successful outboard checks. Exact diff is retained as `/Users/nathan/.local/state/hornvale/counterpart-prepared-mac-28lmcls8/diagnostic-transport.patch`. The old `codex/counterpart-diagnostic` remains `ad4c65d41997949d7d06d4b3cf40920c6bd4f9a2`.

The transport runs existing Counterpart self-tests, creates a new persistent evidence parent under `${XDG_STATE_HOME:-$HOME/.local/state}/hornvale`, sets run.git_audit_directory there, and records the chamber source SHA through supervised run.git. It reconstructs a NEW owned clone from the repository plus bundle at frozen base `5cc62d8b6ae30552f22a68e85fd9a2885ac6e1dd`, tree `7350d1b4abc83775de94f35860a47e158c61dedc`. It records source/input identities, checks them against the frozen panel, then runs separately:

```sh
cargo fetch --locked --manifest-path tools/digest/Cargo.toml
```

Fetch uses the frozen run.capture/run.validate_sample supervisor and retains exact streams, failure flags, command and base-source attribution. SIGINT/SIGTERM use the existing supervisor handler. Available evidence is persisted before failure reactions. Source/tree/input hashes and tracked cleanliness are checked again afterward, with all records outside the owned checkout. No lock reconciliation, source repair, metadata target filtering, network-enabled scoring, new supervisor or persistent gate was introduced. The unchanged full offline CLI follows only after preparation succeeds, into a NEW dossier sibling:

```sh
python3 tools/digest/experiments/the-counterpart/run.py \
  --panel tools/digest/experiments/the-counterpart/panel.json \
  --output "$counterpart_evidence_parent/dossier"
```

The observed Linux failure is the behavioral RED, not manufactured by this change: first base metadata returned 101 because windows-link v0.2.1 was unavailable offline; direct child was collected and all timeout/interruption/cleanup failure flags were clear. Read the controller's retained failed-command.json and stage-receipt.json and listed records.tar.gz; no failed scratch was reused. Existing Digest preparation/offline documentation and controller Q11 authorize the separate fetch.

Mac qualification executed the exact inline preparation body extracted from the committed script, in a fresh owned clone, then invoked only:

```sh
cargo metadata --locked --offline --format-version 1 --manifest-path tools/digest/Cargo.toml
```

Actual outcomes:

```text
fetch_exit_code: 0
fetch_seconds: 0.1499795001000166
metadata_exit_code: 0
metadata_seconds: 0.18385399994440377
metadata_package_count: 53
windows-link: 0.2.1
source_and_locks_unchanged: true
specimen build count: 0
specimen observation count: 0
```

This is a prepared Mac cache result, not proof of Linux preparation until the canonical diagnostic runs. All exact bytes/commands remain in dependency-fetch.json and offline-metadata.json. preparation-before.json/preparation-after.json and qualification-receipt.json retain the lock/pin/source hashes. transport-prepare.py records exactly what was exercised; its SHA256 is `3544804703520d028b873b5762c7ffe5466cb27f2d003b5a94e0e1910c6e4e52` and was checked equal to the committed heredoc. Parent: `/Users/nathan/.local/state/hornvale/counterpart-prepared-mac-28lmcls8`. No local diagnostic evidence was committed into production paths.

Validation and ordinary commit hook:

```text
shellcheck scripts/lane-outboard.sh        # exit 0, no output
bash -n scripts/lane-outboard.sh           # exit 0, no output
git diff --check                          # exit 0, no output
check-bash32: ok
pre-commit: no Rust-relevant paths staged — running the prose-subject tests instead of 'make gate-commit'.
Finished test profile [optimized + debuginfo] target(s) in 49.44s
Summary [5.690s] 75 tests run: 75 passed, 246 skipped
[codex/counterpart-prepared-diagnostic e6d96a5fc] diagnostic: prepare locked Counterpart dependencies before offline assay
1 file changed, 57 insertions(+)
```

The ordinary hook rebuilt the recently changed production dependencies/CLI after Zenith; this is separate from the source-only preparation qualification, which ran no specimen compilation. No bypass, local full assay, off-host census or additional broad suite was used. All yielded commands were polled in bounded foreground waits through completion.

Final git status is empty. Frozen experiment subtree and Charter supervisor diff against controller base is empty; implementation identity remains `02796fe0ae719a55f431f8aedc2ab6853c9a2776`. Panel, checker, compare rules, owner records and bundle are unchanged. This diagnostic branch must never merge. Controller owns scoped review and any push/stage-only submission. Local compilation slot is released.
