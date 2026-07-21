# Retrospective — The Corrigendum (LANG-49, falsification & revision slice)

One page of process, not product. The product is chronicled; this is what
the close learned that the code does not record.

## What went well

- **A post-G3 scope question ("should this also cover weather prediction /
  rain dances?") got the same research discipline a pre-G3 brainstorm
  would have, instead of either a reflexive yes or a reflexive no.**
  Checking the actual climate substrate before answering found two real,
  separately-sized gaps — the seasonal signal is either exactly periodic
  (no naive model would ever miss) or pure untied noise (no true period
  to extrapolate at all), and climate has never committed a single fact
  to the ledger, so the whole Account/Observability/chorus machinery this
  campaign leans on doesn't exist there yet. Both findings changed the
  actual recommendation mid-conversation (from "sounds buildable" to
  "this is really two more campaigns"), and Nathan's own final call
  (split it out, captured as MAP-63) came from seeing the real substrate,
  not from a scope guess. Worth generalizing: a scope question that
  arrives after G3 deserves the same "check before designing" discipline
  the brainstorming skill already mandates before G3 — nothing about
  crossing that gate should relax it.
- **The plan-writing phase itself caught a real spec gap before any code
  existed**, the earliest a catch has landed in this session's run of
  campaigns: tracing the actual call chain for the spec's own claimed
  test update (`the_prediction_line_omits_honestly_beyond_the_teaching_horizon`)
  found it doesn't need updating at all (it never calls `ladder_of`), while
  a DIFFERENT, spec-unmentioned integration test file
  (`windows/worldgen/tests/diachronic.rs`) has two tests that break
  outright — one needing a re-pin, one (`the_prophecy_law`) needing a full
  rewrite, since its own assertion ("doctrine's predictions are never
  wrong") is exactly the guarantee this campaign removes. Both were folded
  into the plan with complete replacement code before Task 1 ever ran,
  not discovered mid-implementation.
- **The same class of cross-task staleness surfaced twice in one campaign,
  and both times a later task caught it rather than it reaching the final
  review or main.** Task 2's review was correctly scoped to the worldgen
  crate (per its own brief) and so had no way to see that a sibling
  book-crate test had gone stale from Task 1's change; Task 3 found and
  fixed it. Task 4 went further and found a THIRD, unrelated case — a
  different, already-merged campaign's (The Consonance) own close had
  left `concept-registry-generated.md` and a census artifact stale, a gap
  that predates this branch entirely. Neither implementer was told to look
  for these; both found them by actually running the full test suite and
  reading real failures rather than trusting a task-scoped green. Worth
  naming as the pattern to watch for generally: a per-task review's own
  scoping (deliberately narrow, for a fast focused gate) means staleness
  in a sibling crate or an unrelated prior campaign's artifacts is only
  ever caught by whichever later step happens to run the broader suite —
  which worked here, three times, but is closer to a lucky net than a
  designed one.
- **A brief's own caution ("don't assume the brief's step list is
  exhaustive — read the round-trip test yourself") transmitted correctly
  and worked twice.** Neither Task 3 nor Task 4's plan text specified the
  exact new `ReckoningLine` enum variant each task would need; both
  implementers independently derived that a new variant was required by
  reading `every_reckoning_line_round_trips` directly, and both additions
  were confirmed non-vacuous (genuinely reachable on live seeds, or
  synthetically covered where not) by their reviewers. This is what the
  brief-writing discipline is *for* — not enumerating every consequence in
  advance, but pointing the implementer at the invariant that would catch
  a missed one.

## What was awkward

- **The final reviewer, not any earlier gate, is the one who actually
  looked at the rendered prose end-to-end** (the future-tense taught line
  beside the past-tense correction, same culture, same paragraph) and
  confirmed it reads as intended rather than as a contradiction. No task
  brief asked for this, and no per-task reviewer was positioned to check
  it (each saw only their own task's slice of the rendered output). This
  project's own "visual pass catches physics" lesson generalizes to prose
  output too — worth a standing reminder that a rendered-text campaign's
  plan should name an explicit "read the real output" step somewhere
  before the final review, not rely on the final review happening to
  think of it.
- **Task 4's commit bundled a genuinely unrelated fix** (the swept
  Consonance-era artifact drift) rather than splitting it into its own
  commit, for defensible reasons (leaving it uncommitted would have failed
  CI's drift check regardless of whose gap it was) but at a real cost to
  bisect-attribution — a future `git blame` on those two files lands on
  this campaign, not the one that actually caused the drift. The final
  reviewer rated this Minor and correctly did not block on it, but a
  cleaner move next time: commit the unrelated sweep separately, even
  inside the same task, so its own history stays honest.

## Follow-ups

- **LANG-51** (the saros-as-ratio revised model — a culture recovering
  from a crisis by discovering an integer-ratio relationship, reusing
  [[the-consonance-campaign]]'s own detection shape, rather than simply
  re-fitting the same naive average) — captured during brainstorm; waits
  on this campaign shipping a real crisis to revise away from.
- **LANG-52** (a `ConflictState`-native falsification — a queryable,
  Lab-measurable conflict entry rather than book-rendered prose) —
  captured; the render-only version this campaign ships is deliberately
  the smaller first step.
- **MAP-63** (the seasonal-return prediction ladder, climate's own
  analog) — captured mid-campaign from a direct scope question; real, but
  two campaigns' worth of new ground (seasonal-weather physics, and
  climate's first-ever fact-commit infrastructure), not a task.
- **The concept-registry/chorus-study drift Task 4 swept up** predates
  this branch and belongs to The Consonance's own incomplete close — noted
  here so a future audit of that campaign's own retrospective can connect
  the two records; The Consonance's own close-time checks (streams
  manifest, type-audit report) did not happen to cover the concept
  registry or the chorus-study census dump, which is worth generalizing
  into the closing-a-campaign walk's own artifact-freshness step rather
  than leaving each campaign to rediscover it piecemeal.
