# The Reservoir

*A reservoir is filled once and drawn from many times. The test suite fills
its own, repeatedly, from the same spring, and nothing in the repository
noticed. This campaign gives it one stored world to draw on, and a ratchet so
the refilling cannot quietly come back.*

## Two obvious answers, and one repository that already knew both were wrong

A full seed-42 world build costs about three seconds in a debug build, four
fifths of it in one stage (`climate+settlements`). An instrumented sample of
the first 285 tests in a full-workspace run — the run itself stopped early on
a pre-existing flake, unrelated to this campaign and recorded rather than
fixed — found **71 builds of the identical world** among 107 world builds
observed. Given that number, two remedies suggest themselves immediately, and
both are wrong for the same underlying reason.

The first is **memoize it**: build the world once per process, behind a
`OnceLock`, and let every test that wants it clone the cached copy. The
second is **ban rebuilding outright**, marking the raw constructors deprecated
the way decision 0092 already polices three derivation entry points via
`clippy.toml`'s `disallowed-methods`.

Both founder on the same fact: **nextest runs one test per process.** A
`OnceLock` populated inside process A is invisible to process B, and the
campaign's own probe measured it directly — 93 of 100 world-building
processes built exactly *one* world before exiting, so a per-process memo
recovers on the order of 7%, not 90%. This was not a new discovery. **Decision
0032, ratified four weeks before this campaign began**, rejected the identical
idea for a different cache — the census summary `windows/lab` recomputed on
every run — in almost the same words: "cargo-nextest (its process-per-test
model re-initialises the `LazyLock` census once *per test* — strictly worse
for this suite, not better)." The repository had already written down why the
first obvious answer fails; nobody had connected it to this problem until the
measurement forced the question.

The clippy route fails on a different axis, and 0092 supplies the evidence
against its own extension. `disallowed-methods` is one lint with one on/off
switch per scope, and 0092's own record shows a single crate-level `#[allow]`
there silencing all 24 platform-libm bans (decision 0041) across worldgen as a
side effect — a constitutional determinism guard going dark because it shared
a bucket with an unrelated lint entry, caught only in review. 0092 carried
about 31 sites under that mechanism. This problem's surface is an order of
magnitude larger — 350 sites across 200 files at the campaign's start — and
grandfathering hundreds of sites into the same on/off switch would go dark on
0041's guard in every one of those scopes. Trading a cost problem for a
determinism-guard hole is the wrong trade in a project whose whole premise is
byte-identical determinism.

## The design that survives process-per-test: a fixture and a bidirectional roster

Since the carrier of reuse has to survive a process boundary, it has to live
on disk. `cli/tests/fixtures/world-seed-42.json` already existed — 5.5 MB, 21,635
facts, the byte-golden two tests already build the world and compare against.
Nothing read it as an input; it existed only as an assertion target. Loading
it is roughly **200 times cheaper than rebuilding it** (~15 ms against ~3.0 s),
because `World::from_json` deserializes a ledger while `build_world` runs
astronomy, terrain sculpting, and the climate fit from scratch.

`hornvale_worldgen::seed_42_world()` reads that fixture at runtime — never
`include_str!`, because baking 5.5 MB into every test binary that wants a
world would regress the exact compilation-unit cost this project's gate
already fights (CLAUDE.md records 260 test targets costing +157.5 s of kernel
time merely to exist). It lives in `windows/worldgen`, the composition root
every crate that builds a world already depends on, so no new layering edge
appears; `env!("CARGO_MANIFEST_DIR")` expands against whichever crate defines
the macro, so a loader with one fixed `../../` prefix works from any caller
only if it lives in exactly one crate — verified with a scratch probe rather
than assumed, with a cross-crate negative control showing `cli`'s own
`../../` resolving outside the repository entirely.

Freshness is not re-derived by a new mechanism; it improves on 0032's own
pattern. 0032 split its guarantee across a suite test and a CI regeneration
step; decision 0125 later deleted CI outright, which is where that half of
0032's guarantee used to live for every committed artifact. Here the guarantee
lives entirely in the suite: `seed_42_world()`'s own byte-identity test pins
load equals build, and the two tests that already built seed 42 and compared
it byte-for-byte (`lens_purity.rs`, `repose_byte_identity.rs`) keep doing
exactly that, unmodified — if they needed to change, the scheme would be
wrong.

For enforcement, a source-scan ratchet in `cli/tests/suite/` — the shipped
idiom `test_binary_ratchet.rs` already established for a structurally
identical problem — scans every crate's `src/**` and `tests/**` for the five
real world-build entry points and checks a roster
(`cli/tests/fixtures/world-build-sites.tsv`) in **both directions**: a site
present but not rostered fails ("a new world build needs a reason"), and a
rostered row whose site no longer exists also fails ("delete the row"). The
second direction is what keeps a roster from rotting into a document nobody
trusts — 0092's own stated objection to a roster file ("never a second
document to keep in sync") is sound against a one-directional list and
dissolves once a stale row is itself an error. Each row carries a reason —
`build-path`, `artifacts`, `identity`, `production`, or `unmigrated(<why>)` —
and the ratchet is one assertion: the count of `unmigrated` rows may never
grow. Scanning `src/` and not only `tests/` mattered concretely: the single
largest call site, 84 callers of `seam_world` in `windows/vessel`, lives
inside a `#[cfg(test)] mod tests` block *inside* production `src/`, and a
tests-only scan would have missed the flagship entirely.

## What a fixture cannot supply, stated as a taxonomy rather than an afterthought

Two of five reason codes exist because the fixture is structurally
incomplete, not because migrating those sites was deferred for convenience.
`GeneratedTerrain` and `GeneratedClimate` are `Clone` but deliberately not
`Serialize` — climate's own doc comment says "recomputed on demand, never
serialized" — so a disk fixture carries the committed ledger but never the
terrain sculpt or the climate fit. A caller needing those objects keeps an
`artifacts` reason permanently. And only the dominant identity — seed 42,
generated sky, default pins — got a fixture here; fourteen distinct identities
were observed in the sample, and a caller needing a different seed or pin set
keeps an `identity` reason. Two of the campaign's own migration candidates
turned out to need exactly that reason once someone actually read their
bodies rather than ranking them by caller count: `worldgen::constant(seed)`
builds under a different sky choice entirely, and `book::generated(seed)`'s
47 callers pass seed 1, 2 or 3 — never 42 — so neither could read the fixture
that exists. Both are named and sized as the campaign's sized follow-on
rather than smuggled into this one.

## The measured payoff, and the debt counter it must not be confused with

Six modules were migrated and measured, in each case comparing the module's
scoped test suite before and after with no test body edited, at equal or
higher machine load on the "after" run than its own "before" — so every figure
below is a lower bound, never a best case:

```
  windows/vessel   session::tests    549.53 -> 324.81 CPU-s   (-224.7, 41%)
  windows/scene    surrounds         132.95 ->  32.97 CPU-s   (-100.0, ~4.0x)
  windows/worldgen exposure           77.12 ->  18.34 CPU-s    (-58.8, ~4.2x)
  windows/vessel   session_snapshot   81.70 ->  42.49 CPU-s    (-39.2, 48%)
  windows/vessel   the_blocking       63.89 ->  42.64 CPU-s    (-21.3, 33%)
  windows/worldgen --lib             491.49 -> 368.47 CPU-s   (-123.0, 25%)
  ----------------------------------------------------------------------
  total                                                        -567.0 CPU-s
```

239 call sites moved across the three migration tasks (110 in `windows/vessel`,
54 across `windows/scene` and `windows/worldgen`'s exposure suite, 75 across
Task 5's remaining seed-42 helpers — 32 in `windows/vessel`
(`session_snapshot.rs` and `the_blocking.rs`) and 43 in `windows/worldgen`),
and `UNMIGRATED_CEILING` dropped from 350 to 334 — sixteen points.

That gap between 239 call sites and 16 roster points is the number this
campaign is most likely to be misread by, so it is worth stating plainly:
**the roster counts sites textually present in a file, never callers reached
at runtime.** A single helper's row gates however many call sites route
through it. The sharpest instance is `windows/worldgen/src/lib.rs::generated`:
migrating it to read the fixture behind an `if seed == 42` guard stopped 43
call sites from building for every seed *other* than 42 — and the file's own
roster row did not move at all, because `build_world(` is still textually
present in the source. Reading "350 → 334" as "the campaign closed 4.6% of
the redundancy" mistakes a debt counter for a performance metric; the payoff
is the 567.0 CPU-seconds measured directly, and the roster's job is only ever
to stop the debt from growing back.

## A correction made twice, and the second one made the number worse

Two numbers in this campaign's own record were wrong before they were right,
and both corrections are worth naming because the second one inverted the
usual shape of a correction.

The design spec's original payoff estimate read `~3,000 ms -> ~1,110 ms =
~2.7x` for an artifact-needing test. It was invalid by construction: the
numerator came from a quiet-box profiling run and the denominator from a
contended-box run taken at load average 24-32 — exactly the fault
`docs/timings.md`'s own header exists to prevent, dividing two numbers that
were never measured on comparable ground. The measured figures that replaced
it (4.0x and 4.2x on the two modules that actually fit that description) sit
between that invalid estimate and a second, differently-invalid derivation
(~8.5x, computed correctly from one run but still a guess rather than a
measurement of the modules in question) — a reminder that both were guesses
about a cost the campaign could simply measure, and the measurement was two
commits away the whole time.

The second correction has a sharper edge. Re-deriving call-site counts with
`grep -o '<helper>()' | wc -l` counts substring occurrences, not calls —
`world()` matches inside `seam_world()`, and every `_world()`-suffixed
identifier in a file inflates the count silently. The instrument's output
looked exactly as plausible as a real correction at every step, and the
resulting aggregate (244) landed *further* from the truth than the number it
was correcting (240) already was. A reviewer's independent recount with a
word-boundary regex found the true figure: **239**. The frozen numbers in
this record and in decision 0606 are the word-boundary count, not either
grep-based one — the open-questions chapter's standing bet about checks that
cannot fail carries the full account, because this is an instance of it from
the controlling side rather than the code.

## What was ratified

**Decision 0606** records the mechanism: a source-scan ratchet rather than a
`disallowed-methods` entry, the five reason codes, and the rule that
`unmigrated` may only ever shrink. **Decision 0607** records that the seed-42
fixture is now an input a test may consume and not only an assertion target,
extends decision 0032's pattern to a world rather than a study summary, and
states the improvement on it — the freshness guarantee now lives entirely in
suite tests rather than split across a CI step decision 0125 deleted. 0607
also carries a smaller, easily-missed consequence: the loader's
`env!("CARGO_MANIFEST_DIR")` widens decision 0090's frozen list of
absolute-build-path embeddings (`cli/tests/fixtures/manifest-dir-uses.txt`)
from two entries to three, which does not change what 0090's cross-host
binary-identity oracle *requires* — both hosts still qualify by building at
the same absolute path — only how many sites now have to keep that condition
true.

## What this campaign did not do

It did not make a world build faster — the 80.8%-of-a-build
`climate+settlements` stage is untouched and is a real, separate optimisation
target. It did not push more tests down `build_world_to`'s existing depth
ladder — that lever is the build-depth ladder shipped by
[Lab Performance](./lab-performance.md), and reopening it here would blur two
efforts. It did not serialize the derived terrain and climate
objects, which would widen the payoff for artifact-needing tests from ~4x
toward the ~200x a bare fact read gets, and is its own save-format-adjacent
campaign. And it authored no second or third fixture, leaving 83 call sites
behind two named, sized, and explicitly out-of-scope identities (a seed-1
generated world for `book::generated`'s 47 callers, and a seed-42 constant-sky
world for `worldgen::constant`'s 36) — because a committed world fixture is a
determinism reference, and minting one is a deliberate act every time.
