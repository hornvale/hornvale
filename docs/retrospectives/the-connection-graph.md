# Retrospective — The Connection Graph (C2 slice 1)

One page of process, not product. The product — the derived transport
graph, the natural-routes-not-built-roads reframe, the ocean-is-the-real-
separator finding, the isolation-predicts-divergence payoff — is chronicled.
This is what the close learned about *how* a seven-task, legibility-only
slice was run, immediately after the highest-uncertainty campaign the
program had attempted (The Living Community).

## What went well

- **The legibility-first / no-epoch discipline held for the whole slice.**
  Every task self-checked against the same constraint: derive, never commit.
  No task reached for a shortcut that would have made a task easier at the
  cost of an epoch (e.g. caching the graph on the world, or memoizing a
  region id as a fact) — the composition-root-only, purely-functional shape
  the G2 design decision specified survived seven tasks of implementation
  pressure untouched. Task 7's byte-identity test formalizes the same
  discipline as an assertion rather than a convention: it grepped the
  committed ledger for the graph's own vocabulary (`edge`/`route`/`graph`/
  `connection`/`conductance`) and found none, which is the cheapest possible
  proof that "no epoch" was never quietly relitigated mid-slice.

- **The visual pass caught a framing bug a green test would have shipped.**
  `reachable_regions` at the isolation threshold resolves seed 42 into
  **29,911** components — a technically correct number that reads as
  broken ("29,911 regions divide the known world") because there is no
  general open-ocean traversal edge, only specific coast-to-coast sailing
  lanes, so almost every one of the globe's ~40,000 mostly-ocean mesh cells
  becomes its own trivial one-cell "region." Every acceptance test passed
  against that number; nothing was wrong mechanically. Reading the actual
  rendered gallery page during Task 6 is what caught that the number itself
  was the defect, not the code producing it — filtering to regions of two
  cells or more turned it into "10 real regions," legible and honest at
  once. A metric can be exactly correct and still need a human eye before
  it is fit to publish.

- **A falsified geometric assumption, caught by measuring instead of
  asserting.** Task 4's fixture design assumed a ring of elevated terrain
  with one gap would behave like a mountain range with a single pass — a
  hard, if narrow, separator. On the icosphere it does not: elevation's
  symmetric cost tax means a great-circle path can dip through the one gap
  early and travel entirely within one hemisphere afterward, so a
  one-breach ring is topologically equivalent to no separator at all,
  path-length-wise. This surfaced only because the fixture was built against
  a real `Geosphere` and verified with a live least-cost probe, not assumed
  to work from the elevation model's description. The fix — ocean as the
  hard separator (genuinely excluded from the search space), elevation as a
  cost tax only — is now the chronicle's headline finding, and it would not
  exist without that measurement.

## What the campaign taught mid-execution

- **Two subagents parked on background monitors before this task's report
  was written; the recovery was to foreground-poll them to completion.**
  This is the same lapse The Living Community's retrospective flagged for
  its own subagents (backgrounding a long-running gate and ending a turn to
  "wait," which the dispatch contract forbids) — recurring once more in this
  campaign despite being a named, written-down rule. The standing rule (run
  long gates in the foreground, blocking; never end a turn on an unobserved
  command) is already verbatim in every dispatch brief; a rule stated once
  and then re-broken twice more argues for a mechanical check (a dispatch
  harness that refuses to launch a background watcher for a gate command) in
  addition to the written instruction, the same durable-half-vs-cheap-half
  split The Living Community's retrospective proposed for the seed-label
  footgun.

- **A cosmetic drift compounds silently across a seven-task chain if no task
  is dedicated to sweeping it.** The progress ledger accumulated several
  small, non-blocking findings along the way (a doc comment quoting 5000m
  where the fixture code sets 3000m; a stray captured stdout line in a
  regenerated report; a commit message naming a deferred deliverable) — none
  individually worth stopping a task for, each correctly deferred to "the
  final review." Because this slice's DoD task explicitly re-reads the whole
  branch before closing, none of them survived to the close, but a slice
  without a dedicated final-review task would have shipped them. Worth
  keeping as a standing shape: small findings are cheap to defer *only* if
  something is guaranteed to sweep them before merge.

## A follow-up worth promoting

The scene-edge-emission assessment (Task 6) measured real numbers — roughly
912 water-route and 149 land-route edges for seed 42, a genuinely small,
additive payload — and still correctly deferred wiring them into the client,
because the actual blocker was derivation cost and plumbing, not payload
size, and the one client that could draw them cannot yet. That is the
"measure before deciding, then defer anyway if the numbers don't change the
call" pattern working exactly as intended; captured here so whichever future
slice wires Orrery up to render routes starts from those numbers instead of
re-measuring them.
