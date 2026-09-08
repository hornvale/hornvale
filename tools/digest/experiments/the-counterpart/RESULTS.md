# The Counterpart: what separate changes can break together

**Draft through Task 7 evidence capture, 2026-09-06.** The original eleven-arm experiment, reserved case, final twelve-arm primary replay and independent replay are qualified as a reviewed finite package. Stage 3 gate, final review, preclose census and G6 remain pending; the campaign is not merged or closed.

The practical question is simple: can two owners change their own code safely, and can we tell when their changes need to be checked together? We tested the real Thing–Settlement concept registry. One owner supplies names that the other uses, and both register names in the same namespace.

The first result is concrete: two acceptable additions collide when combined. A separate safe pair and a legitimate borrowing change work together. The second result is a limit: the additional owner agreement selected exactly the same questions as Cargo dependencies on all eleven original arms. It demonstrated **no selection benefit over Cargo** in this population.

## The questions and actual sources

Every valid arm ran the same four questions: whether registration completed, whether source and component rosters agreed, whether borrowed names had their declared lender, and whether ownership was correct. All checks ran regardless of a proposed selection. An unknown answer after refusal remains unknown; it is not a successful ownership check.

Two owners authored source patches and declarations separately. The checker was independently derived from a shared question contract. The sources are reproducible experiment objects based on `5cc62d8b6ae30552f22a68e85fd9a2885ac6e1dd`; defective source patches are data, not production changes. The [panel](panel.json), [freeze](freeze.json), [source bundle](specimens.bundle) and [instructions](README.md) retain the inputs and their identities.

| Pair | Each change alone | Combined result |
| --- | --- | --- |
| Safe additions | Thing adds `counterpart-token`; Settlement adds `settlement-common-room`. Both satisfy all four questions. | All four satisfied. Both additions are observable. |
| Shared-name collision | Each owner independently adds `counterpart-marker`. Both satisfy all four questions. | Registration refuses the undeclared collision; ownership is unknown after refusal. Components and borrowing satisfy. |
| Removed lender | Settlement renames `hearth` while Thing still borrows that name. Registration and borrowing already fail in this solo. | The dependency failure remains. This is not joint-only interference. |
| Legitimate unusual borrowing | Thing explicitly borrows Settlement’s `home` as well as `hearth`. Its solo is accepted. | All four satisfied when combined with the safe Settlement addition. |

These pairs reuse source arms: the original population is **eleven unique arms**, including the base, and **44 answers: 36 satisfied, 5 violated, 3 unknown**. Existing production registration and the Charter candidate already detect the shared-name collision. Counterpart independently corroborates and classifies that behavior; it did not invent the production guard.

## Did a richer agreement improve selection?

Each rule proposed questions before building or observing an arm. “Raw” means the proposal; “effective” includes the required full fallback when scope is unknown.

| Rule | Raw questions selected | Effective questions selected | Requests requiring fallback | Raw missed violations | Effective missed violations |
| --- | ---: | ---: | ---: | ---: | ---: |
| Declared paths | 28 | 40 | 7 | 2 | 0 |
| Cargo dependencies | 37 | 37 | 0 | 0 | 0 |
| Owner agreement | 37 | 37 | 0 | 0 | 0 |

Cargo and agreement propose identical question sets on every original arm. Each effectively selects 5 violated, 29 satisfied and 3 unknown answers. Path fallback selects 5 violated, 32 satisfied and 3 unknown answers. Satisfied selected checks have not been shown unnecessary. The unchanged base proposes no questions under any rule, but its four checks still execute.

The agreement also costs human interpretation: the owners independently used different subject and supply-list conventions, requiring explicit finite adapters. Original declarations and normalization reasons remain available. This work earned no demonstrated selection advantage over Cargo in the original panel.

A supplementary request removes the collision’s negative namespace assumption. Its raw empty proposal misses registration; explicit unknown scope restores all four questions. This reuses an existing joint observation. It is neither another source arm nor a fourth primary rule.

The reserved case is a deliberate checker/candidate disagreement. The frozen
checker reports registration, components and borrowing satisfied, but ownership
violated: the raw registry assigns `key` to Settlement although the declaration
does not borrow it. The candidate reports all three of its observations as
satisfied because it derives ownership from that same raw registry. The
candidate is retained separately and does not become checker authority.

The full twelve-arm primary dossier contains **48 answers: 39 satisfied, 6
violated, 3 unknown**. The independent dossier has the same counts and the
same four outcomes on every arm. The original **eleven-arm** denominator above
remains **44 answers: 36 satisfied, 5 violated, 3 unknown**; the reserved
singleton is not silently folded into that historical count.

For the reserved request, path and Cargo each effectively selected all four
questions and caught the ownership violation. Agreement initially selected no
questions because its frozen declarations lack a matching reserved variant;
its unknown-scope fallback selected all four and caught the violation. Across
the full twelve-arm panel, path selected 32 raw / 44 effective questions with
8 fallback requests, Cargo selected 41 / 41 with no fallback, and agreement
selected 37 / 41 with one fallback. Each effective rule missed zero violated
questions. Agreement therefore still has no demonstrated selection advantage
over Cargo, including the reserved challenge.

## Evidence, cost and unfinished qualification

The [original Linux records](evidence/unreserved-linux/archive-receipt.json) retain 330 exact JSON members and 292 standalone command samples. An [independent review](evidence/controller/reviews/task5-unreserved-evidence-review.md) checked the archive, all source and command attributions, all 44 raw answers, selections and supplemental results. This is an independent reading of the first run, not another execution.

The per-arm wall times sum to 123.978 seconds, including 59.677 seconds of nested preparation. Observation subprocesses total 0.132 seconds. The first build took 11.786 seconds; later builds took 1.020–1.533 seconds with the same owned target reused sequentially. These nested scopes are not additive and do not measure full campaign wall time or cold-build throughput. Separate locked dependency preparation took 1.141 seconds; request-to-start queue time was 1,414 seconds. Human normalization effort was not timed and remains unavailable rather than zero.

Both the [normal Stage2 gate](evidence/controller/stage-2.json) and [prepared diagnostic gate](evidence/controller/stage-2-diagnostic.json) completed green. The latter tested merge product `951e8057c73bc48b77cd92b07c0a4cffa084e814` and finished at artifact commit `466d6659b65fd530bcbc3926a99d841d5a3fb40d`. Stage2 was accepted at `7047c0e4`. Diagnostic invocations remain isolated from the campaign’s production changes.

Two earlier operational failures remain retained: a reconciliation merge conflict before the assay, and [missing Linux dependency preparation](evidence/linux-preparation-failure/) before any build or observation. Neither is a behavioral arm failure. Explicit locked fetching repaired preparation while the frozen offline runner and source panel stayed unchanged.

The [Mac base run](evidence/mac-base/receipt.json) qualifies a one-arm CLI path. The [independent source reconstruction](evidence/independent-source-only/source-only-receipt.json) recovered eleven source identities, but did not build or execute them. Its reader disclosed seeing predictions embedded in the panel; procedural independence is not blinded adjudication.

The [primary full12 archive](evidence/primary-full12-replay/archive-receipt.json)
contains 358 bounded members, and the [independent archive](evidence/independent-full12-replay/archive-receipt.json)
contains 359. Both retain the full raw dossiers, dependency preparation
metadata and source identities while excluding checkout and target trees.
The frozen `run.summarize` check accepted both dossiers independently. All
twelve source commit/tree identities and all four outcomes per arm compare
equal; candidate copies also compare equal and remain separate from checker
results. Both runs report the same Linux host string and committed identities.
The independent invocation discloses a separate checkout and target with a
shared canonical Cargo registry cache, and is independent from the originating
scratch/execution rather than blinded to embedded predictions.

The primary arm wall total was 127.765 seconds and the independent total was
127.794 seconds; observation subprocess totals were 0.144 and 0.143 seconds.
Queue and author costs remain null. These timing differences are environmental
run variation, not behavioral disagreement; nested arm times are not a cold
throughput or campaign wall measurement.

**[PENDING: Stage3 gate, whole-branch review, preclose census result and actual G6 disposition. Do not call the campaign merged or closed before those steps.]**

This finite experiment grants no permission to skip tests. It establishes no complete environmental closure, atomic capture of live edits, portable verdict reuse, latency SLO, natural fault frequency or general semantic independence. The broader federation program remains open.
