# Task 3 — Thing author report

DONE: Three independently authored, compiling Thing source specimens retained as experiment data.

## Source and ownership evidence

```
/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-thing
codex/counterpart-thing
base 5cc62d8b6ae30552f22a68e85fd9a2885ac6e1dd
commit 846f3fa27760480ed5aac7f1f6a30368bd23faa5
```

Delivered only `owners/thing.json`, three `patches/thing-*.patch`, and bounded original build/raw evidence under `evidence/thing-author/`, all relative to `tools/digest/experiments/the-counterpart/`. No production mutation was committed. Each patch was produced by `git diff --binary -- domains/thing/src/lib.rs` and applies to the frozen base. Assertion-checked replacements target the actual roster and component function (plus exact BORROWED declaration for home); every source diff was nonempty.

## Actual runs

All builds used `cargo build --locked --manifest-path tools/digest/Cargo.toml -p digest-counterpart`; each ran to completion under a 3600-second subprocess timeout. Each then executed `tools/digest/target/debug/digest-counterpart`. Full original stdout/stderr and attempt metadata are retained, including empty output streams.

| Variant | Build exit / seconds | Raw exit / seconds | Observed name effect |
| --- | --- | --- | --- |
| thing-safe-addition | 0 / 18.854 | 0 / 0.182 | counterpart-token absent before Thing; thing-owned afterward |
| thing-joint-claim | 0 / 0.831 | 0 / 0.190 | counterpart-marker absent before Thing; thing-owned afterward |
| thing-unusual-borrow | 0 / 0.864 | 0 / 0.174 | home settlement-owned before and after Thing; explicitly borrowed |

Every raw object has 20 source names and 20 component names, equal in both directions and duplicate-free. Both registrations accepted. Borrowing declarations are unique, in the roster, non-self and supplied before Thing. Every source label's final owner matches its borrowing declaration or Thing ownership, no extra Thing-owned label exists, and both lender owners survive. Existing candidate contribution reports all three of its observations satisfied. These are observed raw properties, not reliance on contributor declarations as an oracle. The four accepted questions remain for the independent checker to evaluate in the integrated assay.

The shared-name collision is a prediction about the eventual composed specimen, not an observation made here. No sibling author variant, record, checkout or implementation was inspected. Inputs shared with the other author were the reviewed base, frozen questions/contract/raw observer, and `counterpart-marker`. Baseline Settlement production source was read to verify that `home` actually exists. No sibling-domain dependency was added.

## Scope and attempts

All three source mutations succeeded on their first compile/raw attempt. No rejected or incomplete specimen attempts were discarded. Initial `domains/AGENTS.md` lookup found no such file; the actual `domains/CLAUDE.md` was read. Missing optional config/script lookups were read-only and did not affect source qualification.

The word safe refers only to the four finite registry obligations. Production FROZEN test data remains unchanged in the disposable patches by controller direction; no full-domain, save-format, world-behavior or admission safety is claimed. No tests are skipped by any comparator or experiment runtime here. The author did not run the full-domain suite. The eventual assay must execute all frozen questions in every valid arm.

The owner record has plain string lists, per-variant supply/consume deltas, and explicit negative assumptions. In particular an imports-only narrow comparison omits `unclaimed:counterpart-token` and `unclaimed:counterpart-marker`; a known import list cannot describe the unclaimed namespace. Top-level supplies/consumes are explicitly the union across alternatives, not one simultaneous mutation.

## Restoration and verification

```
3 nonempty patches apply cleanly; JSON records parse; all raw property checks true; production source and lock diffs empty.
source before SHA-256 63d1c154cc1cdcadc7a35c472d77c4a416d62a3ef9626cbc70b76f02f4d6c9d2
source after  SHA-256 63d1c154cc1cdcadc7a35c472d77c4a416d62a3ef9626cbc70b76f02f4d6c9d2
tools/digest/Cargo.lock diff: empty
```

Each production source was restored immediately after observation and again in `finally`. Before/after SHA equality is an integrity check, not an atomic-capture guarantee. The controller withheld mutations until the owned prewarm finished and staggered the three builds against the other owner. An ordinary commit gate ran in a separate checkout during feasibility observations: timings are not performance benchmarks. Author driver elapsed 21.713 seconds. Recorded drafting interval: 2026-09-05T18:26:06.241091+00:00 to 2026-09-05T18:28:39.024828+00:00; this excludes initial study and includes waiting. No invented author overlap.

## Ordinary artifact commit

```
pre-commit: no Rust-relevant paths staged — running the prose-subject tests instead of 'make gate-commit'.
    Finished `test` profile [optimized + debuginfo] target(s) in 6.81s
     Summary [  12.023s] 75 tests run: 75 passed, 246 skipped
[codex/counterpart-thing 846f3fa27] Retain independent Thing registry specimens and observations
846f3fa27760480ed5aac7f1f6a30368bd23faa5
```

Hook exited 0 without bypass. The 246 skipped are the repository's ordinary prose-subject hook filter, unrelated to the assay's four always-run obligations. Full hook log: `/tmp/counterpart-thing-commit.log`. Final `git status --short`:

```
 M docs/timings.md
```

That one prewarm timing row remains unstaged for the controller. Compilation slot was returned before commit. Task 5 is responsible for immutable source commit objects and the bundle reconstructed from these patches; this author made no source commits.
