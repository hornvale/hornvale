# Task 5 independent review

Spec verdict: Changes requested for persisted evidence validation. Cannot verify full Task 5 acceptance: the actual 11-arm canonical assay is pending.

Quality verdict: Changes requested (two P2 findings). The implementation is suitable for diagnostic development, but repair these validation gaps and refresh the recorded implementation identities before collecting accepted canonical evidence. No checker, owner, or comparison-rule tuning against outcomes is requested.

## Scope and attribution

Reviewed actual run.py, compare.py, test_run.py, test_compare.py, README, panel.json, bundle and source-only receipts against approved spec and Task 5 plan, plus ledger Q9. Read root guidance and review skill; no additional tools/digest AGENTS.md exists. No implementation edits, commits, subagents, Rust builds, assay, or expensive tests were run.

Range: d5f43b410cac6150a4e13b2958eebe46d134377e..179a706fd0d521f6239fe1b3d3647175c1263f49. b82b0e1b8 and af0de6511 are controller documentation; 5c2d83c49 is previously reviewed Task 4 data. Task 5 implementation is 792a2df84 and 9536a75d8; panel/bundle preparation is 179a706fd. Frozen checker.py and contract.json have no diff in this range.

## Findings

1. **P2 — Validate invocation attribution before accepting command evidence.** `tools/digest/experiments/the-counterpart/run.py:204-208` checks command role uniqueness and sample success/stream hashes, but never checks required `command` or `cwd`, nor role-specific expected invocation. Therefore a successful unrelated process record can stand in for build or observation while the arm-level source labels remain correct. The existing fixture itself lacks these command fields and is accepted. A narrow probe replacing the observe command with `['python3', 'unrelated.py']` and cwd with `/unrelated/source` returned `completed=True`. This fails the Task 5 requirement to reject misattributed results and retain exact invocation identity. Require expected owned checkout/target and role-specific argv (including locked/offline settings) and bind each sample to its arm/source capture context; reject missing or contradictory attribution. This is accidental evidence-mixing protection, not a claim to cryptographically authenticate a hostile author.

2. **P2 — Revalidate candidate and supplemental evidence copied into the dossier.** `tools/digest/experiments/the-counterpart/run.py:209-223` recomputes checker outcomes and primary comparisons, but never requires `arm['candidate'] == raw['candidate']`, and never validates `imports_only_supplement` written at lines 321-329. Removing or fabricating the candidate or supplying an invented supplemental score still returns `completed=True`. The candidate record is load-bearing for later correlated-wrong-answer interpretation; the supplement is the explicit missing-negative-assumption comparison. A dossier marked complete can therefore retain contradictory evidence for precisely those claims. Require exact candidate equality with retained raw observation, and recompute/validate the required supplement roster, request scope, selection and score from the frozen pairs and raw outcomes. Keep candidate separate from checker authority.

## Verified strengths and boundaries

- All four checker questions execute independent of suggestions. Selectors consume metadata/declarations, not facts or candidate predictions; suggestions persist before builds/behavior. Both base/arm Cargo graphs are captured, with repository membership separate from outboard workspace root. Unknown scope and missing assumptions trigger full fallback; raw and effective confusion sets remain separate.
- Q9 remains explicit: finite per-owner normalization, retained originals/negative quotes, different Thing delta versus Settlement post-roster semantics, and manual integration cost. It grants no general declaration authority. Queue/author costs are explicitly unavailable rather than invented; final interpretation still owes the integration-cost accounting.
- CLI refuses an existing output directory; owned clone/target, full source SHA/tree checks, exact implementation bytes and input hashes are enforced on execution. Source construction uses exact patches, rejects empty/out-of-scope effects and conflicts, and preserves explicit base prerequisites. No caller reset occurs.
- Git, metadata, build and observation reuse Charter's bounded supervisor. Results persist before unsafe/interruption handling; failing validation stops subsequent sampling; owned directories remain. Poll thresholds can overshoot and README discloses this.
- `git bundle verify` succeeded for 10 specimen refs plus one required base. Bundle/checker/rules/contract SHA256 match panel. The source-only replay receipt explicitly records 11 arms, zero compilations and zero observations; it does not claim behavioral replay.
- Existing 51-test implementation evidence was read and accepted as suite evidence, not rerun. Only narrow probes addressing concrete uncovered validation cases were run.

## Cannot verify / cross-task acceptance

- Actual full unreserved 11-arm compilation/observation on canonical Linux; safe non-no-op pair, joint-only collision, lender dependency failure, unusual borrowing, actual fallback counts, outcome-change counts and preparation/checking costs. Source reconstruction alone establishes none of these.
- Task 6 reserved compiling challenge after final owner/checker/rule/roster freeze; correlated wrong answer; preservation/adjudication of disagreements; supported Mac/Linux behavioral qualification; independent full behavioral reconstruction.
- Controller freeze.json, Stage 2 green acceptance, complete normalization/author/queue cost interpretation, Task 7 close narrative, preclose canonical census and G6. These are pending work, not failures attributed to this implementation range.
- No assurance of complete environmental closure, hostile-author authentication, atomic capture of a concurrently edited tree, generalized selector benefit, or production admission is claimed.

## Review evidence

First command output:

```text
/Users/nathan/.config/superpowers/worktrees/hornvale/federation-next
codex/federation-next
```

Narrow Python probes imported the existing DossierTests fixture, changed only the named stored fields, and called summarize with the unchanged panel/contract:

```text
unrelated command/cwd: completed=True
missing candidate: completed=True
contradictory candidate: completed=True
fabricated imports-only supplement: completed=True
checker.py frozen SHA256 matches: True
compare.py frozen SHA256 matches: True
contract.json frozen SHA256 matches: True
bundle SHA256 matches: True
```

```text
tools/digest/experiments/the-counterpart/specimens.bundle is okay
The bundle contains these 10 refs:
[10 specimen refs verified; retained panel records all exact IDs]
The bundle requires this ref:
5cc62d8b6ae30552f22a68e85fd9a2885ac6e1dd
The bundle uses this hash algorithm: sha1
```

No gate was rerun and no commit was created by this reviewer.
