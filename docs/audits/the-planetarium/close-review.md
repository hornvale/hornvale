# The Planetarium — close review and capture routing

This is the review candidate's close record. Whole-branch technical review is
approved with three documentation corrections recorded below. The final canonical
stage passed after main integration, including the Linux visual-client checks.
Census profiling/accounting is complete with the scope recorded below. Final
close review is approved; Nathan approved G6 on 2026-09-11. Canonical landing remains pending.
Nothing in this file grants publication or reports a landed campaign.

## Review artifacts

The final movie is `task9-clean-300-02/study.mp4` under
`/Users/nathan/Downloads/Hornvale Planetarium/`; full-resolution stills are in its
`frames/` directory. `Planetarium Review.app` contains the exact frozen executable,
film and world. The clean capture revision is
`81ba2bfa6d1654c1e99d28b18ab8dc03d602c7ae`; the candidate's later audit commits
are separate. The [final visual audit](final-review.md) records hashes, native
comparison, actual movie review, app launch and limitations. The
[performance audit](performance.md) records actual distributions and setup.

## Post-G3 rulings for Nathan

The approved physical prerequisite is the leading model qualification: a derived
Earth-like radius uses a documented, tested eight-knot mass–radius relation over
0.5–2 Earth masses, with 32.5% iron / 67.5% magnesium-silicate composition assumed.
It adds no saved field, simulation random draw or dynamical feedback.

| Ledger entry | Adopted ruling |
|---|---|
| 7 | Pin the tested Bevy/Rust pair and require a real early GPU witness. |
| 8–9 | Include the user-approved physical-radius prerequisite, documented domain and composition assumption. |
| 10–11 | Execute the reviewed plan with source checks, isolated branch and staged independent reviews. |
| 12 | Convert the named calendar and system reference frames explicitly, using native evaluated astronomy. |
| 13 | Qualify the actual Metal renderer before relying on an illustrative concept. |
| 14 | Refuse invalid native luminosity at unsupported epochs rather than clamp it into a plausible light. |
| 15 | Use physical radius as the sea reference and positive native terrain relief at 1:1 scale. |
| 16 | Use sourced stellar direction for body lighting; disclose static cosmetic clouds and haze; make no eclipse-shadow or stellar-disc claim. |
| 17 | Enforce the measured orbital envelope: at least twice outer body radius, with a finite projection-error witness; near-surface exploration remains unqualified. |
| 18 | Separate half-open film samples from integer tick rounding; validate empty catalogs and clear pending state on reset. |
| 19 | Bind an external film to the actual clean compiled revision; retain old packages and never relabel their identity. |
| 20 | Bound source waiting and reject late publication; do not claim forced preemption of native or GPU calls. |

The [full ledger](../../superpowers/ledgers/2026-09-10-the-planetarium.md) contains
each question, precedent, ideonomy pass, rejected alternative and task review.
[Spec section 12](../../superpowers/specs/2026-09-10-the-planetarium-design.md#12-execution-decisions-for-final-review)
promotes the material rulings; decisions 0956–0958 preserve the cross-campaign
architecture. Task 9's crater profile and timing correction are implementation
refinements within that scope, not additional physical assertions.

## Capture manifest

Every campaign scratch file was enumerated, all task reports/reviews and the
committed ledger were read before close prose, and Task 9's delta was read again
after qualification. The final reviewer must explicitly identify anything from
scratch or ledger that did not reach a committed home. The durable archive and file-hash manifest are recorded below; subsequent
canonical and profile evidence is retained in the companion artifact directories.

| Item | Durable home and outcome |
|---|---|
| Physical model, basis, geometry, clock and provenance rulings | Spec section 12 and ledger entries 7–20; qualified limitations retained. |
| Task 3 catalog/decomposition minors | Retrospective's deferred-items section: fixed in Tasks 4/6; intentional unconsumed fields documented in client guide. |
| Task 4 pending-scene reset | Same retrospective section: fixed and independently re-reviewed. |
| Task 5 compressed controls/CLI | Same section: expanded in Task 6. |
| Task 6 source inventory portability | Same section: root-anchored and tested in Task 7. |
| Task 7 clean-claim negative cases | Same section and final visual audit: positive plus individually rehashed semantic contradictions in Task 9. |
| Moon-detail comparison and rejected drafts | Final visual audit's refinement section and retrospective: draft 04 retained, earlier images and reasons preserved. |
| Manual orbit/pan delivery | Final visual audit's UI records and retrospective: remains unproven after three tool attempts, separate from scripted handlers. |
| Future game and 2D/2.5D/3D scope | Frontier rows RENDER-film-to-client and RENDER-observation-presentation retain demand-driven library ownership and G6 status. |
| Climate → plants → herbivores → peoples film | Frontier row RENDER-climate-migration-film: raw, requires a causal-source audit. |
| Eclipse, retrograde, trade, rivers, settlement and language stories | Frontier row RENDER-film-story-candidates: raw stories, neither sourced claims nor schedule. |
| Near-surface precision | Frontier row RENDER-near-surface-precision: raw follow-up motivated by the 6,342-point, 0.101748006-pixel orbital witness. |
| Rendered consumer of the scene interface | Confidence Gradient's phenomena-interface bet: narrow measured support, no gameplay/general visualization claim. |
| Observation Series provenance and useful packaging | Package audit and native client guide: exact per-frame records, acknowledged readback and verification retained in the new film structure. |
| Process lessons | Retrospective: early moving witness, asset readiness, Retina/input review, misleading preview glyphs, clean build identity, encoder metadata, locale-safe mutation, hook gap, measurement origin/contention, and interrupted broad local regeneration from a stale Task 1 brief. |

## Canonical checks

Stages 1–3 have actual green reports in the ledger. Stage 3's complete report is
preserved with the controller movie review. Earlier client phases did not include
the new visual Linux CPU gate; the final stage request must establish that result.
The authorized census request is `req-fa1223fd7e82-20260911T001301Z` at
`fa1223fd7e82d059b61788506f90abcaf271dc9c`. It remains queued at this entry.
The actual diff from that ref through the clean capture revision has no changes
in kernel, domains, windows, CLI or visual-source paths. Recheck that identity at
the final candidate before using the eventual census result.

Heavy and the actual merge remain after G6 through the canonical queue. The
Planetarium tracker stays in progress until an approved candidate actually lands;
the unrelated inherited tracker is preserved.


## Task review and retained qualification

All allocated local implementation tasks have passed independent review, including
Task 9's terminal benchmark-write error fix after a compiled behavioral regression.
The successful GPU capture and review app remain at `81ba2bfa6`; the later
terminal failure propagation has CPU/scoped-review evidence only. The final visual
audit names that distinction and the retained shutdown warning.

Routing locations recorded at `b5fa355c3` were: spec section 12
at line 380; retrospective deferred-items section at line 47; final visual audit
refinement at line 36 and UI evidence at line 124; frontier game/climate/camera/
story rows at lines 2244/2245/2246/2247; Confidence Gradient witness at line 2267;
client library ownership at line 8. These are lookup locations, not frozen line
contracts; the named headings and row IDs remain the durable identifiers.


Final stage request `req-62fc0480f1e7-20260911T010052Z` is queued at
`62fc0480f1e7de0448e8a848ad6652087ee8b54e`. Its normal push and request returned
zero. Whole-branch review is evaluating that same candidate while the canonical
queue works; no final stage or census result is claimed yet.


## Final whole-branch review

The independent review of `cb0331192..62fc0480f` approved the implemented
technical scope and code quality with no new Critical or Important defect.
Three Minor findings were corrected: the guide now distinguishes unrendered
stellar illumination from unresolved wanderer markers; current plan/ledger
summaries agree with the Stage 3 green report; and the retrospective now promotes
the concrete Task 1 stale-brief/local-regeneration incident. It records the
interruption and the exact retained plumb-roster versus absent fixture drift.
The explicit scratch/ledger survival audit found no other unpromoted substantive
ruling, parked finding or idea. Archive both its report and the subsequent scoped
documentation review after that review finishes, then record the actual manifest.
These findings do not grant G6 or turn queued canonical requests into completed checks.


## Reviewed scratch archive receipt

After the scoped documentation re-review approved M1–M3 with no new findings,
the archive command copied this campaign's scratch to
`/Users/nathan/Downloads/Hornvale Planetarium/campaign-review-12849d497`.
It contains 100 files plus `MANIFEST.json`; every copied file hash was checked
against its original. Manifest SHA-256:
`3b94a51f2da0042b538c5ddcff9d3b5496788806aa24069c5ff03dd91120e581`.
The receipt is `task9-controller-review-02/archive-receipt.json` beside the
preserved GPU evidence. The archive includes all task/fix reports and reviews,
whole-branch and scoped documentation reviews, capped/full review packages,
controller notes, routing records and the archive script. Primary GPU packages
and raw measurement logs remain in their original durable Downloads directories.

This is the completed implementation-review archive at `12849d497`, before
canonical close. Subsequent canonical reports or integration changes require their
own durable receipt; this snapshot does not claim to contain future results.


## Canonical census result

The authorized census finished green: queue request
`req-fa1223fd7e82-20260911T001301Z`, 1387 s end to end; its census timing row
records 1366.487 s. Delivery commit `88e8aeb7ff0e37dc13c996cd9512a8dc9bf17681`
on `census/fa1223fd7e82-20260911T015448Z` contains no moved scientific goldens.
The actual diff contains six timing rows and the operational write-manifest's
reference/audit file counts. The runner's TIMINGS ROW ONLY description is therefore
incomplete, despite the correct null-golden verdict. The full log and delivery
diff are preserved in `task9-controller-review-02/canonical-census*`.
The delivery merge is prepared on the campaign branch, but its normal commit
hook refused: this run exceeds the 1320 s census alarm and owes a profiling
finding. The merge is not committed. Main remains untouched by this campaign;
final stage results and the profiling obligation remain pending.


## Held final stage — main integration

Request `req-62fc0480f1e7-20260911T010052Z` was held with rc=10 at merge,
before any stage phase. Of 1961 s reported wall time, 1948 s was waiting
for the canonical lock. Main advanced to `f22860af3`; the conflicts were
`windows/scene/src/lib.rs` and the generated plumb/type-audit reports.

Integration commit `6d5d5a8bb1452149197e1114711b81eb33b1747f` preserves both
Planetarium's evaluated-astronomy error annotation and the incoming eclipse
observer annotations. The two reports were regenerated by their own authors.
Focused tests passed 67/67, visual-source tests 4/4, and the normal commit gate
passed all four subfloor chunks (143.893 s). Independent integration review and
a 300-observation source comparison are recorded next when complete.

The earlier uncommitted census-delivery merge was backed up in full and aborted
to permit this main integration. Its delivery branch and timing evidence remain
intact; its live-profiling obligation remains open. No timing was deleted from
a committed branch and no alarm changed. This stage resubmission repairs the
merge conflict; it does not discharge the separate census-close obligation.

CPU replay at the integrated source `6d5d5a8bb` matched the captured initial
document semantically and all 300 exact-tick observation replies byte-for-byte.
`task9-controller-review-02/native-main-replay.json` names the actual checkout
separately from the original capture binding reused for protocol comparison.
The temporary example was removed; no new GPU or capture qualification is claimed.

Independent scoped integration review approved with no findings at `6d5d5a8bb`.
Main remains `f22860af3`; the final merge preview is clean. Recovery backup,
fix/review reports, packages and replay evidence are durably archived in
`task9-controller-review-02/held-stage-recovery/` with 17 file hashes;
manifest SHA-256 `83abeb0e89277dc0cd5366d0c128774f24023e1e64b58e951d40b298a0e3ae6d`. The next action is a fresh stage
submission. The earlier held request is not relabeled as passing.


## Final canonical stage — green report

Request `req-a9593ffa367e-20260911T120920Z` at
`a9593ffa367ecbbca89a362fc27ef5b907380a7f` is **reported green**: all four
phases returned zero in 2127 s. The tested merge product is
`914aaf8a53651163894300495f5210c1570e2bef`; the final artifact commit is
`3f2030a2f1bae992b3c5254f4592fb0d81b4e4d6`. Main was unchanged at
`f22860af31313a7dc9ad48ed24aa008d539912bf`; root fetched that same main
after reading the report and confirmed a clean merge preview.

Measured phases: artifacts 281.565 s, outboard 147.279 s, gate 861.821 s,
clients 817.181 s. The Linux visual-client section actually ran: 79 Rust
tests plus 6 Python dependency tests passed. This establishes the client CPU
gate on Linux, not a Linux GPU-quality claim. The final artifact diff contains
only four operational files: generated-path write counts, timings, subfloor
roster, and the lefford test baseline (120 insertions / 95 deletions). No
scientific fixture or renderer source changed in that authoring delta.

Full log, actual diff and hashed report are preserved in
`task9-controller-review-02/canonical-stage5*`. The earlier held request remains
a failed merge attempt; this fresh report is the successful qualification.
The captured movie/app remain bound to `81ba2bfa6`, with the separately
recorded integrated-source replay proving 300/300 reply byte agreement.

The census's null-golden result is already known, but its 1366.487 s timing
still owes the requested live profile before delivery incorporation. No
operator profiling result has been reported. That obligation, final G6
approval, and the post-approval merge/heavy run remain open.


## Census-close obligation completed

The original census delivery and scoped live-profile finding were committed
together at `e8fdb804ce9aa1c9f6a8927b77be8bb598ab4ddc`. The normal hook
passed all 76 prose-subject tests in 4.027 s, including the census alarm's
per-run finding check. Its first attempt correctly refused an overlong idea
index cell; root restored the existing cell and put the new profile link in
the Where column, retaining the full reasoning in the audit. No test, threshold
or calibration reference was weakened.

Main contact at this boundary remains `007936ed6`, already absorbed. No
simulation or visual-client source changed after the successful stage and
300-reply comparison. The seed-42 keystone is checked against current main;
no fresh physical or GPU claim is inferred from these operational/doc changes.
The [profile audit](census-profile.md) is the final scope of census attribution.
The G6 decision and subsequent queue merge/heavy phase are still required.


## Final independent review — ready for G6

The scoped final integration/profile review approved `e8fdb804c` with no
findings. It explicitly found the measured main-study-only attribution adequate
to discharge this referral, while retaining the unproven full-pipeline/scaling
claims as limits. Its report is durably preserved with the controller evidence.
All campaign implementation, visual, canonical and census-accounting work needed
for the G6 package is now complete; Nathan's visual/merge decision remains open.

The controller evidence snapshot contains 92 file hashes at
`task9-controller-review-02/pre-g6-evidence-manifest.json`, SHA-256
`174766c0f6267812500da122924782288895909dc056392f73e46297659b7696`. It precedes this final approval-receipt metadata;
the original 4K package and census raw-profile archive retain their separate
manifests. This does not grant publication or report a landed campaign.


## G6 approval — 2026-09-11

Nathan reviewed the package at `83a4fa3d08da54b4339ebaccf1df3b0891e028e8`
and approved the visual direction and merge: “Yeah, it's fine. We'll need to
refine it in future campaigns, but it's fine for now.” This accepts the bounded
pilot with its recorded limitations; further aesthetic refinement stays with
future campaigns. It does not claim the broader visual ambition is finished.
The post-approval changes record this decision and prepare the authorized merge;
the frozen film/app identity remains `81ba2bfa6`. Publication is outside scope.
The actual canonical merge and heavy result must be read before reporting landing.
