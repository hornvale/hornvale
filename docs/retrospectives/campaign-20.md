# Campaign 20 (The Scene Window) — retrospective

**Merged:** 2026-07-09

**Recurring findings.** None of this campaign's own findings restate a
prior retrospective's by name — the closest cousin is general, not specific:
proving a refactor moved zero bytes (Campaign 19's synodic-fix ordering
lesson is a cousin in spirit — verify before you build on top) is by now
routine discipline here, and it held: the kernel dedup task regenerated both
pre-existing committed rasters after the move and diffed them byte-for-byte
against the versions already in the gallery before any downstream code was
allowed to depend on the moved type. One genuinely new failure mode showed
up instead, worth watching for recurrence rather than filing as a repeat:
see the review-package note below.

**Estimate deltas.** No stage-level estimates were made for this campaign,
so there is nothing to compare against — say so rather than pad.

**Spec vs. reality.** Four implementation tasks landed against a plan whose
code was, in three of the four, transcribed close to verbatim from the spec
and plan documents rather than designed fresh during implementation — the
kernel index was moved "not rewritten" by explicit instruction, and the
scene crate's struct, sampling loop, and CLI command all matched the plan's
own listings on first compile, needing only clippy-driven micro-adjustments
(a `manual_is_multiple_of` rewrite, a let-chain collapse) that the plan's
prose had already flagged as toolchain-dependent. Review across all four
tasks found almost nothing: one substantive fix, in the schema reference
page, where "ocean flag: true at or below sea level" overstated the actual
predicate (`elevation < sea_level`, strictly less-than) and was corrected to
"below sea level." Everything else — field order, error messages, the
golden fixture's byte content, the CLI's usage text — matched what the plan
specified. The verbatim-transcription style this campaign leaned on hardest
of any so far appears to have matured: a plan detailed enough to be copied
rather than interpreted produces implementations with almost nothing left
for review to catch, provided (as here) the plan's own tables and formulas
were themselves checked against the code they claim to describe before the
plan was approved.

**The review package ballooned on a committed artifact, not on code.** The
task that added `book/src/gallery/scene-tiles-seed-42.json` — a single-line,
compact JSON document holding five 32,768-entry arrays — produced a review
diff of roughly 1.6 MB, almost all of it that one generated file's byte
body, dwarfing the actual code change (five small edits across four files).
The review package had to be hand-trimmed before it was usable: the array
bodies elided in favor of a one-line note pointing at the file. This is a
new problem, not a repeat of the malformed-table or missed-artifact misses
named in earlier retrospectives — those were about a plan's *completeness*,
this is about a review tool choking on a committed artifact's *size*. It is
also not a one-off: this project commits large generated artifacts by
design (rasters, audio, and now scene JSON), and every one of them will hit
review the same way the day it's first added or meaningfully changed. Worth
a standing rule for future large-artifact tasks: a review package generator
should elide or truncate array/binary bodies of committed generated files by
default, the way this one had to be trimmed by hand — the file's *presence*
and its *diff stat* are what review needs to see; its full byte content is
what the drift check already verifies.

**Do differently next time — the golden fixture earned its place.** This
campaign introduced a pattern not used before: a small, hand-eyeballable
fixture (eight tiles by sixteen) asserted byte-for-byte against the schema's
live output in a normal test, deliberately separate from the much larger
gallery example that CI regenerates and diffs on every build. The two serve
different jobs and neither subsumes the other. The gallery example is large,
realistic, and *automatically* kept current — it proves the pipeline still
produces exactly what it always produced, but nobody reads 32,768 numbers to
notice a schema change. The golden fixture is small, *deliberately* static
— its own comment says regenerating it is "the epoch decision point" — and
a diff against it is something a reviewer can actually read: a field
reordered, a type changed, a key renamed all show up as a legible four-line
diff instead of a silent pass. Nothing in this campaign exercised the
fixture catching a real regression, so the pattern's value is inferred
rather than demonstrated — but the cost was genuinely small (one ignored
regeneration test, a ten-line duplicated test helper because integration
tests can't see the library's `#[cfg(test)]` scaffolding) against a real
gap the gallery-example check cannot fill on its own. Worth reusing for the
next schema a window ships, and worth watching, over a campaign or two more,
for whether it ever actually catches something the drift check would have
missed.
