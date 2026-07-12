# Retrospective: Common Ground

**Campaign:** Common Ground (cross-platform float quantization) · July 2026

A one-page process retrospective — lessons about how the work went, not what
it produced.

## What went well

- **Measure the drift surface before choosing the fix.** The instinct was to
  "quantize the world fixture." Grepping every committed artifact for
  high-precision floats replaced that guess with a map: the world ledger was
  127 floats, but the lab censuses were ~31k and the scene tiles ~62k, while
  the almanacs and maps were *zero*. The measurement redirected the work
  from one boundary to four — and, just as usefully, ruled three coarse
  artifacts out, preventing needless changes to their producers.
- **Reason about the choke point instead of patching symptoms.** The ledger,
  the CSV, and the scene JSON each had exactly one serialization seam. One
  shared kernel primitive at those seams beat scattering `format!` precision
  specifiers across dozens of call sites.
- **Let CI history date the regression precisely.** Walking the run list
  pinned the red to a specific merge and separated the *first* failure (the
  artifacts step) from the *current* one (a newer fixture guard) — same root
  cause, two symptoms. That stopped a wild-goose chase after the guard test.

## What was tricky

- **`tee | grep` hid a second failure.** The first full-suite run reported
  one failing test; a second, identical failure in a later test binary was
  truncated from the piped output. Re-running with `--no-fail-fast` and
  reading the captured file surfaced it. Lesson: for a save-format epoch that
  shifts many goldens, assume *several* fixtures broke and enumerate them up
  front, rather than fixing the first and re-running.
- **The epoch's blast radius reached beyond the artifacts.** Three classes of
  test broke that a naive "regenerate the fixtures" plan missed: two frozen
  *deserialize-only* world fixtures (requantized in place via a one-shot
  value walk), and two unit tests asserting raw committed values (updated to
  expect the as-committed quantized form). Right-sizing found them via a
  hardcoded-float scan, per the re-baseline-golden-pins habit — in the same
  change, not deferred.

## What to carry forward

- **Check the numbering/convention conventions at merge, every time.** The
  plan reserved decision `0027`; on inspection, decision 0026 had *just*
  retired numbered decisions in favour of slugs. Following the plan blindly
  would have minted a stale artifact. The parallel-campaign-collision habit
  earned its keep again.
- **A lossy transform in a determinism-first system demands a stated safety
  argument.** The Lorenz question ("won't we rediscover chaos if we run off
  this?") was the right challenge, and answering it — reload re-derives from
  the lossless seed, so quantized floats are never initial conditions — was
  worth banking as a guard-rail, not just a reassurance.
