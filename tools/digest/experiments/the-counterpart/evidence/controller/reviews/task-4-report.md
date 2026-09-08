# Task 4 — Settlement specimens

Working directory: /Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-settlement
Branch: codex/counterpart-settlement
Frozen base: 5cc62d8b6ae30552f22a68e85fd9a2885ac6e1dd

Delivered only owners/settlement.json, patches/settlement-*.patch and bounded evidence/settlement-author/* under tools/digest/experiments/the-counterpart/. Production sources restored before artifact commit. No Thing file was written. Root-owned prewarm timing row remains unstaged for controller.

## Independently authored specimens

- settlement-safe: one tuple adds settlement-common-room to Settlement's social concept manifest loop; accepted solo with all four frozen questions satisfied by direct raw-fact interpretation.
- settlement-collision: one tuple adds counterpart-marker under Settlement ownership; accepted solo. Prospective shared-name collision input, with no joint result observed by this author.
- settlement-lender-rename: changes hearth to settlement-hearth in that loop; Settlement accepts, Thing refuses its now-absent lender. Base-green/solo-violating registration and borrowing; ownership unknown. Existing production tests remain untouched and are not claimed green for this deliberately changed lender.

Each exact nonempty patch was captured using git diff --binary -- domains/settlement/src/lib.rs from the disposable mutated source. No unit-test roster or other source change appears in any patch. Safe means only the four frozen questions, not full-domain or admission safety.

## Commands and actual outcomes

The selected observer command was cargo run --quiet --manifest-path tools/digest/Cargo.toml --package digest-counterpart --bin digest-counterpart --target-dir /Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-settlement/tools/digest/target --locked --offline (CARGO_TARGET_DIR/CARGO_BUILD_TARGET/CARGO_BUILD_TARGET_DIR unset). Each child ran synchronously with subprocess timeout=3600 and stdout/stderr retention; tool-session yields were immediately polled to completion.

Base rc0, 8.944s; safe rc0, 0.730s; collision rc0, 0.780s; lender rename rc0, 0.810s. All are completed raw observations, including the expected captured refusal. Restored-base rc0, 0.663s and stdout byte-identical to initial base. No failed/no-op speculative attempt occurred. Raw stdout/stderr and source hashes are retained; expected panic diagnostics are preserved, not discarded.

All patches pass git apply --check on restored frozen source. JSON list fields and patch scope verified. Both Cargo.lock SHA256s match before/after; actual git diff over both locks and both domain lib.rs files is empty. No lock adjustment inferred or invented.

## Independence and effort

Read task4/shared contract, frozen observer/questions, baseline Settlement and Thing production source, kernel registry, domains/CLAUDE.md and existing digest-thing candidate source. domains/AGENTS.md was absent; used actual CLAUDE.md. Task3 brief was absent; obtained only owner field schema from controller/committed shared-interface line. No sibling author record, patch, worktree or outcomes inspected. No security-blindness claim.

Owner JSON records the timed interval after initial drafting through qualification, explicitly including prewarm wait and excluding untimed initial reading/drafting. Raw command times above are actual selected run durations; controller reports prewarm rc0, 359.107s. No claim of measured active labor or parallel overlap.

Ordinary artifact commit hook result and commit SHA follow after completion.

## Completed artifact commit

Commit: 9c47f253db7b968152c6bb6dfa8787845779a340

Ordinary hook ran make docs-tests and returned zero. Actual summary:

```text
Summary [   4.607s] 75 tests run: 75 passed, 246 skipped
[codex/counterpart-settlement 9c47f253d] test(digest): retain independent Settlement counterpart specimens
18 files changed, 425 insertions(+)
```

Post-commit status:
```text
 M docs/timings.md
```
Only controller-owned docs/timings.md prewarm row remains unstaged. Compilation slot returned after source restoration, restored-base rebuild, hook and final verification.
