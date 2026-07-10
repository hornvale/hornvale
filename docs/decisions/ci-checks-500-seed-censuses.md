# CI checks 500-seed censuses

**Status:** Accepted (2026-07-10) · **Decider:** Nathan

In the context of the CI "Artifacts are current" step rerunning
`census-of-skies` at 10,000 seeds on every build — violating the scale
discipline the study pages themselves state (500-seed populations are the CI
determinism guard; 10,000-seed runs are author-time-only) — we decided that
**CI reruns and drift-checks only the 500-seed census family; no 10,000-seed
study is ever a CI obligation**, accepting that the committed 10,000-seed
artifacts (Study 001's summary and charts) are no longer re-proven
reproducible on every build.

**Context.** `census-of-skies` was the Lab's first study and CI's original
drift guard, and simply stayed in the CI list after `census-lands-drift`
superseded it. The two run the same seeds-from-0, default-pin,
every-metric-the-registry-knows configuration — `census-lands-drift` at 500
seeds is a strict prefix of the same population — so the 10k rerun bought no
coverage, only ~20x the compute. The Laboratory overview already documents
the convention this ratifies: the author-time census family (all 10k
studies, `census-of-skies` included) is "run by hand, never reran by CI,"
while `census-lands-drift` and `census-of-the-meeting` are "the CI-checked
half of every census." Study 001's own text declares the sky census
author-time and historical. No committed artifact changes: the 10k summary
and charts stand frozen as the published measurement, exactly as Studies
002–011 already treat theirs.

**Consequence.** CI's lab-run list is `census-lands-drift` and
`census-of-the-meeting` (both 500 seeds, both under `--release` per the
debug/release byte-identity cross-check). A future committed-and-CI-checked
study must be authored at 500 seeds; a 10,000-seed headline run stays an
author-time act whose numbers are quoted in prose, re-read by hand when
deliberately regenerated. Determinism of the shared generator surface is
guarded at 500 seeds; a defect that only manifests past seed 499 is accepted
as out of the drift check's reach.

**See also.** Decision 0011 (studies are data, metrics are code); decision
0016 (studies preregister hypotheses); the Laboratory overview
(`book/src/laboratory/overview.md`, the author-time-census-family and
self-check sections); Study 002's scale-discipline paragraph
(`book/src/laboratory/study-002.md`). Slug filename per decision 0026.
