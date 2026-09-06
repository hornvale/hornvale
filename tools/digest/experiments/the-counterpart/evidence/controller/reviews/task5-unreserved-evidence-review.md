# Task 5 full unreserved Linux evidence review

Spec verdict: PASS for the retained original 11-arm unreserved assay and its stated finite interpretation.

Quality verdict: PASS; no actionable findings in the scoped evidence package. Remaining diagnostic gates and overall Stage 2 acceptance are CANNOT VERIFY from this package. This review does not accept Task 6 reserved results or substitute for Task 7 fresh behavioral replay.

## Scope and method

Reviewed committed evidence at controller `1527c714f1bbff4298c011b3335d001bb3659d39`: `evidence/unreserved-linux/{archive-receipt.json,controller-validation.json,summary.json,records.tar.gz}`, frozen contract/runner/comparators, ledger's actual-result section and operational interpretation checklist. The checklist was treated as operational guidance, not authority or a newer result. Chamber source is `3f8b21d9a96a274034dbd086ada811caf2ad4939`.

No source execution, builds, full replay, remote jobs, tracked edits or commits were performed. Programmatic read-only validation was independently run over all retained bytes. A separate direct translation of the four contract questions checked every raw fact record without calling checker.evaluate; frozen summarize was also run separately for identity/attribution/selection validation.

## Integrity and attribution

Verified archive size 10,140,392 bytes and SHA256 `d40fb36db93be3b4fcf50c8b7ceb0d67e5a0a9a3d9a3899ee0b1d94e2a1ae5cb`; exactly 330 JSON member names, all member SHA256/lengths and 144,688,229 uncompressed bytes. All 292 standalone command samples pass raw-stream/hash/size/success/failure-flag validation. Duplicate-key-aware JSON parsing was used.

Archived manifest equals current original frozen panel. Current run.py, compare.py and checker.py bytes equal their frozen implementation commit. Frozen summarize verifies all 11 source commit/tree/arm labels, input/lock/pin hashes, exact command argv/cwd/capture context and both metadata graphs, full four-question roster, candidate copy and primary/supplemental results. Every embedded arm and command equals its separate retained arm/command file; selections match their separately persisted file. Thus the earlier R1/R2 repairs cover the actual recorded dossier as well as fixtures.

Original summary exactly matches its committed copy. Recomputed summary arms match it. Recomputed class properties match as question-ID sets: rereading sorted JSON changes lender `joint_violating` list order from registration/borrowing to borrowing/registration. The first strict equality probe stopped on this ordering-only difference; inspection identified it, and comparison then used question sets for that field. No outcome, arm, question or property changed, so this is not a finding or evidence repair. The original records were not modified.

## Independent behavioral interpretation

All 44 raw answers agree with the accepted contract: 36 satisfied, 5 violated, 3 unknown.

- Safe pair is observable, not two no-ops: Thing introduces `counterpart-token`; Settlement introduces `settlement-common-room`. Base, both solos and joint satisfy all four questions.
- Collision pair introduces `counterpart-marker` independently under Thing and Settlement. Base and both solos satisfy all questions. Joint registration refuses the undeclared collision; borrowing/components remain satisfied and ownership is unknown because composition is incomplete. This supports joint-only interaction for these designed sources.
- Lender rename removes the prior `hearth` while BORROWED still consumes it from Settlement. Registration and borrowing fail in the solo and lender joint; ownership is unknown. This is an existing dependency failure, not joint-only interaction.
- Unusual Thing borrowing adds `home` with an explicit Settlement borrowing alongside `hearth`. Its solo and unusual pair remain accepted with correct owners, preserving the legitimate case.

The raw production refusal explicitly names the counterpart-marker collision and missing cession. Separately retained Charter candidate registration already reports `contradicted`. Therefore this is independent corroboration/classification of existing detection, not a new production detector or evidence that the prior candidate missed this collision.

## Comparisons and limits

Independently summed all primary confusion sets and compared them with controller-validation.json; all agree:

| Rule | Raw selections | Effective selections | Fallback requests | Raw missed violations | Effective missed violations |
| --- | ---: | ---: | ---: | ---: | ---: |
| Path | 28 | 40 | 7 | 2 | 0 |
| Cargo | 37 | 37 | 0 | 0 | 0 |
| Agreement | 37 | 37 | 0 | 0 | 0 |

Cargo and agreement have identical proposed/effective question sets on every arm, with 5 selected violations, 29 selected satisfactions and 3 selected unknowns. Their finite comparison is a null: zero demonstrated selection benefit from the manually normalized owner mechanism over Cargo on this panel. Path effective counts are 5 violations, 32 satisfactions and 3 unknowns. Unknown checker answers are separate from unknown mapping/fallback requests.

Base has no changed inputs and proposes an empty set under all three rules, while all four checker obligations still execute. No selection skips actual checks. Selected satisfied checks are not demonstrated unnecessary work. Q9's finite adapters/manual integration cost remain explicit; no selector or owner record was tuned against these outputs.

The imports-only collision supplement uses the existing joint observation and the right-owner request against the left source. Raw proposal is empty and misses registration; its explicit missing-negative-scope unknown restores all four questions, including registration. It is neither another arm nor a fourth primary comparator.

## Preparation and costs

Exact fetch argv is `cargo fetch --locked --manifest-path tools/digest/Cargo.toml`, attributed to base in the new preparation checkout. Before/after source and input hashes equal the frozen base, and the chamber label agrees with the archive receipt. Retained stderr records downloads of windows-link 0.2.1 and windows-sys 0.61.2. Fetch succeeded in 1.140632246620953 seconds. The subsequent actual metadata/build commands remain locked/offline; no graph target filter or source/lock change appears.

Independent sums reproduce per-arm wall 123.9780108332634 seconds, nested preparation 59.67734382394701 seconds, and observation-only execution 0.13211985677480698 seconds. These are nested recorded scopes, not additive totals or full CLI/campaign wall. Separate fetch and queue/author/manual-normalization costs must retain their own scopes. Reused sequential target/cache and one host series do not establish cold-build or throughput performance.

## Exact review output

```text
/Users/nathan/.config/superpowers/worktrees/hornvale/federation-next
codex/federation-next
Archive verified: 330 JSON members; 144688229 uncompressed bytes; 292 standalone commands validated
Frozen summary and properties agree (question ID sets normalized for ordering)
Independent raw-fact interpretation: {'satisfied': 36, 'violated': 5, 'unknown': 3}
All independent checks passed
```

Machine-readable independent totals are retained in sibling `unreserved-review-validation.json`. No gate was rerun and no commit was created by this reviewer.

## CANNOT VERIFY / pending acceptance

Normal Stage 2 is recorded green and the prepared diagnostic outboard phase returned success; the remaining diagnostic gate/client phases were still running in the supplied status. Their completion and full Stage 2 acceptance are not established here. Reserved challenge integration/scoring, supported-host comparisons and Task 7 independent fresh behavioral reconstruction remain separate work. This archive validation is another reading of the original run, not a second execution or enlarged specimen population. No general selector benefit, admission privilege or complete environmental closure follows.
