# Task 6 reserved source packaging review

Spec verdict: PASS for reproducibility packaging of the existing reserved authorship case under the controller-dispatched Q10 scope. Ready to integrate as experiment data after the controller's prerequisites.

Quality verdict: PASS. No actionable finding in `41ceed7a270771833e8b738366e61721e5ab1ab7..fa707895e761b944794207180a1218784f591cdd`. Full Task 6/formal integration acceptance remains CANNOT VERIFY.

Worktree: `/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker`.
Branch: `codex/counterpart-challenge`.
Reviewed artifact commit: `fa707895e761b944794207180a1218784f591cdd`.

## Evidence and review

Read the author report, complete source record, failed/corrected drivers, provenance, construction verification and audit summary before examining all 43 raw Git sample records. The range adds only `reserved-source.json`, `specimens-reserved.bundle`, and construction evidence. It changes no original preregistration, patch, raw observation, frozen instrument, panel or production source.

I independently recovered the bundle into a temporary repository initialized from a depth-one, no-tags fetch of only `5cc62d8b6ae30552f22a68e85fd9a2885ac6e1dd`. The initial reachable commit list contained exactly that base and the repository was shallow. Bundle verification and fetch succeeded; the recovered source was `b0ae89d8a93cda6fe8932ed24e38db8d64865613`, tree `17905910c5b081c3c706daa0b6e0709f0f3a4432`, with exactly the declared base as its single parent. The bundle header contains only that prerequisite and the reserved specimen ref. Its actual SHA256 is `f164f67e4803b522f1a17b70fd7fa61255944d8913be7296576f4483f3d50d9c`, length 5443 bytes.

The recovered diff has exactly the two preregistered paths: `domains/thing/src/lib.rs` and `tools/digest/packages/thing/src/lib.rs`. Both patched blob hashes match the original preregistration. All base/arm input hashes and lengths match recovered objects. The candidate collector's separate before/after hashes and lengths also match; it remains outside the frozen arm.inputs set and is nevertheless part of the source tree and explicit changed-path inventory. No enrollment or scope extension is smuggled into this record.

A fresh exact original-patch application at the recovered base, using `git apply --index --whitespace=error`, followed by `git write-tree`, produced the identical full tree. This independently confirms the source object's reproduction from the original patch rather than trusting the author's boolean recovery claims. Verification commands used 30-second subprocess bounds, completed in the foreground, and operated only in temporary reviewer storage. The temporary repository was removed on completion.

All 43 retained commands are source-only Git operations. Every sample has exit 0, a waited direct child, no timeout/output-limit/interruption/launch/cleanup failure, and independently matching decoded stdout/stderr hashes and byte lengths. The original startup TypeError is retained with traceback and the original driver. The corrected driver differs solely by passing `prereg_path.read_bytes()` instead of a Path to `load_json`; inspection confirms the failed call precedes the first Git invocation. The source patch and preregistration were unchanged.

The additional post-preregistration exposure is explicitly disclosed: selected frozen runner helper mechanics were read, and importing that runner also imported checker/comparator definitions. The driver calls source construction and verification helpers, not scoring or observation. This is consistent with packaging an already immutable case; it is not a new blindness claim. All frozen file identities and the supervisor identity still match. The existing author report retains the normal artifact hook result, 75 passed and 246 skipped; I did not rerun it.

The existing qualification evidence remains what it was: preliminary Mac compilation/raw observation from the prior subtask. This packaging does not create a new behavioral measurement, prove successful formal comparison, or establish portable verdict reuse.

## CANNOT VERIFY — remaining full Task 6 scope

- Green normal Stage 2 and canonical unreserved qualification, including every queue/phase receipt. These remain controller prerequisites to scoring/integration acceptance.
- Combined panel/bundle/freeze integration and its complete arm counts, identities, changed-input handling, and formal treatment of the collector path outside the mapped production subjects.
- Original frozen checker/comparator results for the reserved object, independently adjudicated mismatches, unknown scope, novelty against the complete unreserved panel, and unusual-borrowing over-refusal.
- Canonical Linux reserved behavioral dossier, exact diagnostic merge-product and diagnostic-only diff, both-host evidence validation, and host-specific cleanup/resource conclusions.
- Independent full behavioral replay of the final committed reserved package and subsequent Task 7/G6 acceptance. The independent source reconstruction in this review is narrower and does not replace that replay.

## Actual verification output

```text
/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker
codex/counterpart-challenge
PASS: 43 retained Git samples, all rc=0, completion flags and stream hashes/lengths verified
PASS: failed/corrected driver differs only in Path-to-read_bytes call
PASS: artifact-only scope, frozen identities, preregistration and patch unchanged
PASS: fresh shallow base-only recovery; bundle verified; exact parent, input/blob hashes and two changed paths
PASS: independent original-patch application yields tree 17905910c5b081c3c706daa0b6e0709f0f3a4432
Source b0ae89d8a93cda6fe8932ed24e38db8d64865613
Bundle f164f67e4803b522f1a17b70fd7fa61255944d8913be7296576f4483f3d50d9c; 5443 bytes
HEAD fa707895e761b944794207180a1218784f591cdd
Status ''
```

No Cargo, source build, raw observer, evaluate/suggest/score, remote job, implementation edit, commit or delegation was performed by this reviewer. Only this ignored review report was written in the campaign checkout.
