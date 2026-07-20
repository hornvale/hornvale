# Retrospective — The Temperament

One page of process, not product. The product is chronicled; this is what the
close learned that the code does not record.

## What went well

- **The byte-identical-first ladder held all the way to the affect read.**
  Stage 0 extracted thirst behind a `Drive` trait, proven byte-identical before
  a second drive existed; Stage 2 wired thirst + thermal through one
  `arbitrate` whose thirst-only path was pinned equal to the old `decide`; and
  Stage 3A's `Resolution` refactor — `arbitrate` now returns the affect
  alongside the intent — was byte-safe by construction, because `decide` simply
  takes `.intent`. Every stage that could break the possession walk was proven
  not to before the next was built. The one stage that *did* change the walk
  (3A's `needs` renders affect now, not a thirst scalar) changed it visibly and
  on purpose, and the regenerated fixture reads as legible affect tracking the
  drive cycle — the intended delta, not a surprise.
- **The correspondence payoff was a two-line flip that footed automatically.**
  Stage 4 flipped cold/heat's cognition edge to `Present`; `cli/tests/
  correspondence.rs` asserts the ledger *foots* (covered + void = total) rather
  than hardcoding the count, so two edges moving kept it green without editing
  the test — only the generated manifest page (0/76 → 2/76) needed regenerating.
  The trial-balance-not-magic-number discipline paid off exactly as designed.

## What the campaign taught mid-execution

- **The affect model had to be designed against the REAL drive parameters, not
  abstract thresholds — and the first cut was wrong.** The initial derivation
  read the circumplex regions off an arousal threshold (loud → frustrated, quiet
  → lost). It broke on contact with the shipped constants: thirst's `act` is
  0.85, so whenever a drive is active arousal is already high, and the quiet
  "lost" region was unreachable. Worse, thirst's serviceability is *binary* (1.0
  for the affordance's own step, 0.0 otherwise), so "how satisfying is the
  chosen action" could not separate eager from searching — both a Drink and a
  step read 1.0. The fix came from reading the actual code, not the spec's
  prose: the honest distinguisher is *knowledge* — eager when the drive is met
  or beelining to a KNOWN reachable source, searching when following a gradient
  toward an unknown one, frustrated when a known source is out of reach, lost
  when there is no basis to move. Design the felt-state boundaries against the
  numbers the drives actually produce, or they name regions the system can't
  enter.
- **The health metric's null control taught the metric's own purpose.** The
  plan assumed a distress signal that varies across worlds. Running the harness
  showed the opposite: a healthy world reads *zero*, because condensed on-water
  settlements let creatures drink in place, and — the deeper finding — a
  creature denied water *searches* rather than despairs, so genuine
  blocked-distress (Lost/Frustrated) essentially never arises on a real mesh. A
  barren world yields endless Searching, not Lost. That reframed the deliverable:
  the metric is a *regression alarm* that is valuable precisely by staying
  quiet, not a varying score. It also reshaped the validation — the
  spike-recover and spike-persist signatures are exercised on constructed affect
  traces (the reduction's detection logic), because no real world produces them
  on its own, while the null control runs end-to-end on a seed sweep. A metric
  you cannot make fire on real data is validated by proving it *would* fire on
  the pattern it is meant to catch.
- **The affect read superseded a field, and removing it was the honest
  follow-through.** `DriveParams.sated` existed only to gate the old three-band
  `needs` narration; once affect replaced that, `sated` was set-but-never-read —
  and its own doc cited a prior review (The Foresight) that removed dead fields
  rather than leave misleading docs. Following that precedent (removing the
  field, not just re-documenting it) kept the type-audit and the struct honest.

## Followups (register)

1. **Drive coupling** — heat should quicken thirst (a hot creature dehydrates
   faster); the drives are independent today.
2. **Behavioural learned helplessness** — the `Helpless` label exists and the
   metric measures chronic distress, but a helpless creature does not yet
   *behave* differently (stop trying); the sticky give-up is deferred.
3. **More drives** — hunger, danger/terror, social; each is its own `Drive`
   implementor and its own campaign (fatigue overlaps the activity cycle;
   safety needs threats; social needs the social graph).
4. **The `time_horizon` psych dimension** — anticipation (act before a drive
   crosses `act`); today only `deliberation_latency` is wired.
5. **A synthetic distress scenario harness** — the health metric's fault
   signatures are tested on constructed traces because real worlds don't
   despair; a synthetic terrain/NPC scaffold (a boxed-in creature, a
   known-but-unreachable source) would exercise the sim end-to-end into
   distress, closing the gap between the reduction test and the harness.

## Confidence Gradient

Checked `book/src/open-questions.md` for a moved bet. The self-scoring health
metric IS a new bet — "can a population score its own cognitive health?" — and
this campaign answers a first slice: yes, as a regression alarm anchored to a
null control, though genuine distress proved rare enough that the metric's live
value is guarding against future breakage rather than diagnosing present worlds.
That nuance (the alarm is armed by staying quiet) is the scoring, recorded in
the idea registry's cognition row and the chronicle; the Confidence Gradient's
world-generation bets are untouched, as this sits in the cognition layer beneath
that chapter's scope, exactly as [The Wanting](../book/src/chronicle/the-wanting.md)'s
did.
