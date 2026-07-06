# Campaign L0: The Laboratory

**July 2026 · 11 commits · outcome: complete, merged — the first moonshot, and the instrument that makes the rest measurable**

## What was attempted

Turn determinism into an instrument. Extract the composition root into a
library the CLI and a new harness could share (paying the long-deferred
WASM-reuse debt in the same cut); build a metrics registry, a JSON study
format, a deterministic runner, summary tables, and hand-rolled SVG charts
with no plotting dependency; wire a `hornvale lab` command; and run the
first study — a census of ten thousand skies — publishing its results as
committed, drift-checked book artifacts.

## What landed

**The Lab is live.** A study is a small JSON file that names a seed range,
a set of sky pins, and a selection of metrics — *data selecting code-side
measurements, not code selecting data*, which is the line that keeps the
config from becoming a language. `hornvale lab run` builds one full world
per seed, extracts every selected metric, and writes a raw CSV, a
distribution summary, and a bar chart per metric. Ten thousand worlds census
in about a fifth of a second; the "shrink the study if it's too slow" risk
never came close to materializing, and much larger studies are plainly
affordable.

**The instrument proved itself the day it shipped.** [Study 001: The Census
of Skies](../laboratory/study-001.md) reports a calibration metric with
known ground truth — belief-kind must equal tidal-locking at tier 0 — and it
does, 457 eternal faiths against 457 locked suns, exact to the single world.
That equality is now a test run over the drift study on every build, so the
instrument is not merely calibrated once but kept so. Alongside it stands the
first genuinely unknown number the project has ever produced: **3.6% of
skies are refused a moon** by the stability inequalities, cross-confirmed by
an independent genesis-note count.

**The results cannot silently rot.** A small 500-seed drift study reruns in
CI, regenerates its committed summary and charts, and fails the build on any
diff — the same discipline that guards the almanacs, now guarding the
Laboratory. The full ten-thousand-seed census is an author-time run whose
committed output is its own referee.

## What was learned

- **The extraction was behavior-free, and provably so.** Moving the
  composition root out of the CLI into `windows/worldgen` was refereed by
  the committed almanac artifacts: byte-identical before and after, a
  rename with doc comments and nothing else. The debt it repaid (a library
  target reusable by a future WASM build) was collected in the same motion.
- **Charts without a plotting library are a taste problem before a code
  problem.** The chart module was designed against an explicit contract —
  fixed viewBox, one accent color, currentColor text so light and dark book
  themes both read, nice-number axes — and then reviewed twice for drift
  into that committed surface. One review caught a genuine hazard: publishing
  every study into one shared directory meant a study named `census` could
  delete the artifacts of one named `census-drift`. The fix was structural
  (each study owns a subdirectory), not a cleverer string match.
- **The adversarial final pass earned its place again.** Exactly as in
  Campaign 2b, fuzzing the finished branch found what the happy path did not:
  a seed range near `u64::MAX` that wrapped silently in release instead of
  failing loudly, pin-set labels that were never validated and could corrupt
  the very CSVs and tables they named, and a `list-metrics` command that
  omitted the per-metric documentation the spec had promised. All three were
  contained validation gaps, and all three are closed.

## Deferred, deliberately

Parallel execution and statistical tests (they arrive with the first
hypothesis study, which needs them); configuration-defined metrics (never —
metrics are code by design); the workshop-paper draft (it consumes the Lab;
a separate effort); YAML studies (blocked only by the no-new-dependencies
rule, revisited if that budget ever loosens). One cosmetic edge remains
noted: the seed-range display line reads as an empty range at the single
degenerate `u64::MAX`/one-seed boundary — unreachable by any real study.

## Artifacts

[Study 001: The Census of Skies](../laboratory/study-001.md) — ten thousand
skies measured, the calibration proven exact, and the moon-refusal rate
answered with a number. Its drift-study twin reruns on every CI build.
