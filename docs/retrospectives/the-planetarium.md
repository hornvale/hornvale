# The Planetarium — retrospective

**Close state:** implementation and close preparation in progress; final visual,
canonical and merge evidence still pending. Product scope is in the
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
left alone. Fresh process start is not a cold filesystem or shader cache.

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

## Deferred review items and their homes

- Task 3 catalog validation and renderer decomposition were addressed in Tasks 4
  and 6; Task 8 documents the deliberately unconsumed fields.
- Task 4's pending-scene reset was fixed and independently re-reviewed.
- Task 5's compressed control/CLI code was expanded while Task 6 extended it.
- Task 6's caller-relative source inventory was fixed and tested in Task 7.
- Task 7's clean-provenance negative-test suggestion, moon-detail comparison and
  manual mouse-drag witness remain assigned to Task 9; final outcomes are owed here.
- Near-surface precision and future film stories remain frontier follow-ups.
  They do not imply a universal renderer or a scheduled film series.

Scratch reports and reviews were read before drafting this page. Their final
Task 9 delta, durable artifact archive and exact routing locations remain part
of close preparation. The Confidence Gradient has a relevant scene-interface
bet: a real rendered consumer strengthens it within this bounded astronomical
scope; it does not establish gameplay or general world visualization.
