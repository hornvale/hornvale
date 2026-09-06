# Task 4 review

## Spec Compliance

- ✅ Spec compliant for Task 4. Base `5cc62d8b6ae30552f22a68e85fd9a2885ac6e1dd`; reviewed head `9c47f253db7b968152c6bb6dfa8787845779a340`. The 18-file diff contains only the Settlement owner record, three patch-data files and bounded author evidence. Each patch changes only the owned Settlement registration loop (`tools/digest/experiments/the-counterpart/patches/settlement-safe.patch:5`, `settlement-collision.patch:5`, `settlement-lender-rename.patch:5`). No production source or lock changes ship.
- ✅ The safe addition and prospective marker claim are real, distinct, observable additions in retained before/after registries (`tools/digest/experiments/the-counterpart/evidence/settlement-author/settlement-safe.stdout.json:1`, `settlement-collision.stdout.json:1`). Neither is a no-op or a claim of observed joint behavior.
- ✅ Lender rename is base-green/solo-violating: baseline supplies hearth, while the renamed specimen supplies settlement-hearth and Thing refuses the missing declared lender. Component rosters remain equal; ownership is explicitly unknown after refusal (`tools/digest/experiments/the-counterpart/evidence/settlement-author/base.stdout.json:1`, `settlement-lender-rename.stdout.json:1`; `owners/settlement.json:170`). Completed process rc0 remains distinct from the refused subject (`evidence/settlement-author/runs.json:81`).
- ⚠️ Cannot verify across tasks: formal independent checker judgments, all composed arms, candidate/comparator scoring, immutable dossier capture and replay, and freeze/reserved-challenge chronology belong to Task 5 and later. The owner explicitly labels its judgments as hand interpretations, not checker output (`tools/digest/experiments/the-counterpart/owners/settlement.json:94`).
- ⚠️ Cannot verify from artifact diff alone: absence of contemporaneous source readers/writers or sibling-result exposure. Controller supplied the compilation-slot and author-independence clarification; source-sharing disclosure is explicit (`tools/digest/experiments/the-counterpart/owners/settlement.json:222`). Before/after hashes are integrity evidence, not atomic capture proof.

## Strengths

- Minimal reconstructible patches exercise the intended production behavior without adjusting tests or rosters. Raw observation retains both actual facts and separately identified candidate results (`tools/digest/experiments/the-counterpart/patches/settlement-lender-rename.patch:9`; `evidence/settlement-author/settlement-lender-rename.stdout.json:1`).
- Expected panic diagnostics are retained as part of the negative observation, not hidden or misreported as an uncontrolled attempt failure (`tools/digest/experiments/the-counterpart/evidence/settlement-author/settlement-lender-rename.stderr.txt:2`). These diagnostics are expected assay output, not compiler/test warnings.
- Restoration records equal before/after hashes for both locks and an empty production diff; initial/restored raw outputs are exactly equal (`tools/digest/experiments/the-counterpart/evidence/settlement-author/restoration.json:2`, `base.stdout.json:1`, `restored-base.stdout.json:1`).
- Scope and effort claims are qualified: safety applies only to four questions, source sharing is disclosed, and elapsed time explicitly excludes untimed initial study while including prewarm wait (`tools/digest/experiments/the-counterpart/owners/settlement.json:227`, `owners/settlement.json:241`).

## Issues

- Critical: none.
- Important: none.
- Minor: none.

## Assessment

**Task quality: Approved.** The small patch inputs and retained actual outputs satisfy the independent Settlement author task. Their evidence supports the stated solo outcomes without claiming broader admission safety or independent checker authority.

## Checks and evidence

- Read the supplied diff in one review pass. Initial combined tool output truncated the evidence section; recovered that missing section and parsed its large raw JSON additions from the review package for legibility. No changed production file was separately read, and no tests or gate suites were rerun.
- Review-only JSON inspection printed: `Base/restored exact raw equality: True`. The first extraction command accidentally included the review-package stat header and raised JSONDecodeError; corrected the reviewer extraction to start at actual diff entries. This was a review tooling error, not an artifact error.
- Existing author hook evidence in `task-4-report.md` (not independently rerun):

```text
Summary [   4.607s] 75 tests run: 75 passed, 246 skipped
[codex/counterpart-settlement 9c47f253d] test(digest): retain independent Settlement counterpart specimens
18 files changed, 425 insertions(+)
```

- First-command checkout evidence:

```text
/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-settlement
codex/counterpart-settlement
```

Only this requested review report was written; no index, branch, source, or lock mutation was performed.
