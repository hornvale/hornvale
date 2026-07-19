# Retrospective — The Confluence

One page of process, not product. The product is chronicled; this is what
the close learned that the code does not record.

## What went well

- **A denominator artifact was caught before it could contaminate the
  graded readout, not after.** The near-river fraction the emergence
  keystone measures is only meaningful relative to how much land counts as
  "near a river" in the first place — and that ruler (`RIVER_REACH`) is the
  same knob the field itself reads. Widening it to clear a preregistered
  absolute bar would have worked, and would have proven nothing: at
  `RIVER_REACH = 7`, 90.3% of all seed-42 land already sits within reach of
  some river, so a settlement fraction tracking that ambient coverage is
  zero enrichment wearing a passing number. The task's own probe caught
  this *before* the graded readout ran, not by peeking at a failing result
  and rationalizing a fix afterward — the discipline that makes a
  preregistered measurement worth anything is exactly this: notice the
  ruler is broken before you know whether the test passes with it.
- **The delta under a fixed ruler is the actual science; the absolute
  floor is a formality.** Once `RIVER_REACH` was pinned at its pre-existing
  value (never touched to make a number look better), the honest
  measurement was the *before/after delta* at that fixed reach: five seeds,
  every one up by 0.08–0.18 over its own pre-repoint baseline. That
  five-for-five, seed-by-seed improvement is the campaign's real evidence
  that the field term does what it claims. The single preregistered
  absolute threshold (0.7) exists so a test can produce a boolean the gate
  can check — it is not where the confidence comes from, and treating it
  as if it were would have been the wrong lesson to draw from a passing
  CI run.

## What the campaign taught mid-execution (T3)

- **Re-fitting one constant to fix a coverage regression can quietly erode
  the keystone that motivated it, and the fix is to re-run the keystone,
  not just the number it was chasing.** Sharpening the freshwater term
  concentrated population catchments along river corridors tightly enough
  to under-shoot the settlement-count band at the old condensation
  threshold. The naive fix — lower the threshold until the count recovers
  — turned out to trade directly against the near-river fraction: several
  threshold values that restored a healthy settlement count left the
  keystone sitting at 0.699–0.702, a coin flip against its own 0.7 floor,
  not evidence of anything. The sweep kept going until it found a point
  (1.7) with real margin on *both* axes, and re-ran the T2 keystone test
  explicitly rather than assuming a settlement-count fix couldn't touch a
  different measurement. Two preregistered numbers sharing one physical
  knob is a pattern worth watching for generally: fixing one against a
  hard floor is not free just because the other one wasn't the thing you
  were tuning.
- **A seed-42-only recalibration is real evidence at n=1, not proof at
  scale, and the retrospective is the honest place to say so.**
  `CONDENSATION_THRESHOLD = 1.7` clears both bars on seed 42, measured live
  (not against the census fixture, which lags this campaign by policy —
  censuses regenerate once per campaign, pre-merge, on Nathan's AWS step).
  Whether 1.7 holds its margin across the full seed population is a
  question only the eventual post-merge census regen can actually answer.
  This is not a defect in the campaign's method — a live, non-census
  re-measurement is the correct instrument for iterating inside a `SKIP_
  CENSUS=1` gate — but it is a scope boundary worth stating plainly rather
  than letting a seed-42 result quietly read as a population-level claim
  it was never measured to support.

## What T4's close taught

- **A payoff test flipping from failure to success can break sibling tests
  that baked the old failure into their own premise, and those are just as
  much this task's responsibility as the flip itself.** The parked Surmise
  reachability test flipping to success was the expected, celebrated
  outcome — but three real, end-to-end integration tests in
  `possession_moves.rs` had written the *previous* measured reality
  (a co-located NPC departs and never returns) directly into their own
  assertions and doc comments. Fixing only the named test and declaring
  the gate green would have shipped a red suite; the campaign's actual
  Definition of Done is the whole gate, not the one test the brief named.
  Each of the three had to be re-derived from a fresh measurement of the
  new real behavior, not patched to merely stop failing.
- **A structural payoff can retire an end-to-end coverage path, and the
  honest move is to name the gap, not manufacture a substitute.** The
  co-located-NPC departure narration (`Session::narrate_motion`'s named
  branch) had exactly one end-to-end test, against seed 42's real flagship
  settlement. That settlement is now, by this campaign's own design,
  reliably on-water — so it can never again produce a real departure to
  narrate. There was no way to preserve that coverage without either
  faking a departure that doesn't happen or restructuring `Session` to
  accept injectable terrain (a real option, but out of this task's scope).
  The test was rewritten to prove the honest adjacent claim instead — a
  co-located NPC that never leaves must never be *falsely* narrated as
  departing — and the lost coverage is named as a followup rather than
  silently absorbed.
- **A payoff test can surface a second, unrelated finding for free, and it
  is worth chasing down rather than filing as noise.** The reachability
  measurement returned 2 drinks over a 100,000-day wait where the drive
  cycle's own math implies thousands — worth explaining, not shrugging at.
  Tracing it (temporary debug prints, removed before commit) found a real
  floating-point boundary: the zero-distance on-water case is new (no
  settlement had ever landed exactly on its own water source before), and
  at that exact boundary the closed-form `Hold` jump's rounding coincides
  with the strict-progress guard's "same value twice" check, ending the
  tick two cycles in. It doesn't weaken the payoff (2 is still the
  measured, asserted `>= 1`) and it isn't a regression this campaign
  introduces — but it is a genuine, newly-exposed quirk, and it went into
  the followup register with the exact mechanism named rather than into a
  vague "seems fine" note.
