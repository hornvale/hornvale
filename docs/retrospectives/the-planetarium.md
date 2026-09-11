# The Planetarium — retrospective

**Close state:** local implementation and moving review qualified; whole-branch technical
review approved, with three documentation findings corrected. Canonical close
results and Nathan's visual/merge decision remain pending. Product scope is in the
[chronicle](../../book/src/chronicle/the-planetarium.md); rulings and task outcomes
are in the [ledger](../superpowers/ledgers/2026-09-10-the-planetarium.md).

## What changed the process

A real moving witness was essential early. The upstream renderer example emitted
a clear frame at frame 0 and a mesh by frame 9: an example's warm-up count was
not an asset-readiness contract. Actual UI use then exposed Retina sizing,
quick clicks, dolly limits and caption clearance that CPU tests could not see.
Conversely, a tool preview garbled caption glyphs that were correct in the
original PNG and decoded MP4. Review the artifact before repairing the renderer.

Build cost and rendering cost were different problems. The first full Bevy build
took 21m05s; a later 300-frame 4K capture took 85.079s. A lingering preview from
our own earlier inspection contended with the first clean capture. Closing that
owned window helped establish the next measurement; unrelated processes were
left alone. Fresh process start is not a cold filesystem or shader cache. The final run met
its p95 target under another campaign's compile/test load; its worse p99 and
154.991 ms maximum stayed in the report. Startup instrumentation initially began
inside live::run, after parsing. Moving it to the first statement of main required
a new clean build and qualification, not relabeling the earlier 9.562 s result.
The corrected-clock movie and all 300 PNGs matched the already-played film exactly;
independent provenance checks and a recorded byte join preserved the review.

Clean provenance required the actual compiled revision, not a caller's label.
An external film copy bound to a clean build avoided the self-referential problem
of committing a film containing its own commit hash. Old dirty captures stayed
intact. The first clean encode then failed honestly: output flags alone left
color metadata absent in ffprobe. A one-frame setparams probe established the
fix before a new complete capture. A corruption fixture separately exposed a
Mac locale issue: tr emitted two-byte UTF-8 where the test meant one byte;
LC_ALL=C exercised the intended case without weakening the production guard.

The review machinery caught lifecycle and evidence gaps. Worker timeout tests
had to release a fake worker before asserting, or the test's own teardown could
hang. Task 4's first bridge tests were not red-first; compiling mutation evidence
was recorded instead of pretending otherwise. A clean automatic Git merge did
not run the repository's commit hook. An explicit 43.565s local gate repaired
the verification gap before further implementation; future absorptions use
--no-commit followed by ordinary git commit. Main was absorbed during execution,
not left for the final merge.

A stale Task 1 brief sent the worker into broad local artifact regeneration.
The controller applied the current canonical-placement rule after the world
builder/reader groups and interrupted the run in Group D. No census option was
set; no world, system or moon fixture diff remained. Transient glyph and
underworld-panel touches also left no retained diff. The radius module's expected
plumb-roster update did remain. Check inherited command placement before dispatch;
unrelated expensive artifact authoring belongs to the canonical queue. The first
task also ran the commit gate manually and then through the normal hook: use the
hook as the commit gate instead of paying for the same verification twice.

## Deferred review items and their homes

- Task 3 catalog validation and renderer decomposition were addressed in Tasks 4
  and 6; Task 8 documents the deliberately unconsumed fields.
- Task 4's pending-scene reset was fixed and independently re-reviewed.
- Task 5's compressed control/CLI code was expanded while Task 6 extended it.
- Task 6's caller-relative source inventory was fixed and tested in Task 7.
- Task 7's clean-provenance suggestion is fixed in Task 9: a clean positive and
  three individually rehashed contradictions reach the semantic guard.
- Task 9's fourth moon draft retains a continuous crater bowl and corrected
  tangent-normal sign; a directional regression failed before the correction.
  Three earlier appearances remain preserved with their rejection reasons in
  [final-review.md](../audits/the-planetarium/final-review.md).
- Manual mouse-drag delivery remains unproven after three tool attempts. The
  pointer did not move; no renderer failure or successful manual orbit was inferred.
  The final audit names the actual UI records and separates scripted controls.
- Task 9's benchmark-output failure falsely returned CLI success. A real failed
  write reproduced it; terminal AppExit propagation fixed it and passed scoped
  independent re-review. The successful capture retains its original revision.
- The post-completion Bevy destroyed-window warning remains nonblocking and
  documented in the performance/final audits; it was not suppressed.
- Near-surface precision and future film stories remain frontier follow-ups.
  They do not imply a universal renderer or a scheduled film series.

Scratch reports and reviews were read before drafting this page, and the Task 9
delta was read again after qualification. The [close audit](../audits/the-planetarium/close-review.md) records routing and
the 100-file implementation-review archive. Final review found the Task 1
regeneration lesson missing; it is promoted above and passed scoped re-review. The Confidence Gradient has a relevant scene-interface
bet: a real rendered consumer strengthens it within this bounded astronomical
scope; it does not establish gameplay or general world visualization.
