# Retrospective — The Rains

Weather Program campaign 3: a precipitation **derived-field epoch** — an
upwind moisture-budget trace replacing the single-pass rain shadow, plus
typed precipitation / snow-fraction / seasonal-regime / diagnostic-cloud
fields and the client's precipitation lens and cloud overlay. Process
lessons only; the product is in the chronicle.

## What worked

- **The epoch calibration crisis was caught by a downstream test, not by
  eye.** The first prescribed moisture constants over-dried the world
  (habitability 10%→2%), breaking 38 hand-authored content assertions the
  implementer could not rebaseline. The implementer correctly *escalated*
  rather than weakening the assertions, and the fix was architectural — the
  budget trace modulates a preserved base floor instead of setting wetness
  outright. The lesson from earlier campaigns held: hand-authored content
  assertions are a real accuracy gate, and a green suite that required
  gutting them would have been a false green.

- **The retune was structural, not a knob-hunt.** Rather than search the
  constant space for a value that happened to keep the world alive, the fix
  changed the *shape* of the formula (drying-modulation-of-a-floor), which is
  why it recovered 37 of 38 assertions with a single deliberate re-pin. A
  falsified prediction (over-drying) became a mechanism discovery
  (`CONVECTIVE` subtracted on every overland hop was capping equatorial
  habitability).

- **The visual verification paid for itself again.** The precipitation lens,
  cloud overlay, and biome lens — inspected by eye off the merged-tree wasm —
  confirmed the epoch's payoff (arid interiors, wet coasts, advecting clouds,
  polar snow, rearranged biomes) that no automated test asserts. The Gyre's
  legibility lesson carried forward: the clouds shipped as bright streaks,
  not the specks that campaign first drew.

## What was hard

- **The close was a four-way merge, not a two-repo ship.** Main moved 22
  then 15 commits *during* the close — The Confluence, The Elements, The
  Single Sculpt, and The Deep Grammar all landed on the same reference seed
  between this campaign's start and its merge. Absorbing them was the bulk of
  the closing work, and the conflicts were semantic, not textual: The Single
  Sculpt had rethreaded the almanac line functions (`rains_lines` had to
  adopt the `*_from(&terrain,&climate)` pattern), and two campaigns had each
  re-pinned the same settlement headcount off the same base.

- **The settlement pin could not be reasoned — it had to be measured on the
  merged tree.** The Confluence pinned it to 4 (freshwater term), The Rains
  to 9 (moisture epoch); the combined world was neither — it re-derived to 6.
  Guessing either branch's value would have shipped a red gate. The rule
  *measure, don't narrate* applied to a merge resolution, not just a test.

- **The epoch cascade reached into two other campaigns' fresh tests.** A
  seed's kobold, no longer organized on the drier map, dropped out of The
  Deep Grammar's taught-contrast landscape and shifted a moon-manner lexeme
  in the explanations suite. Both were legitimate re-derivations (the
  morphology-layer pin was unchanged; only the settlement-driven *organized*
  status moved), but confirming that — rather than assuming — meant running
  the derivation-layer sibling test to prove the morphology had not moved.
  These surfaced one at a time behind nextest's fail-fast; a single
  `--no-fail-fast` pass would have shown both at once and did, once used.

## Process notes

- **`--no-fail-fast` is the right tool for an epoch's cascade.** Discovering
  cross-campaign re-pins one gate-run at a time (~10 min each) is the
  expensive path; one no-fail-fast suite surfaces the whole casualty list in
  a single pass. Reach for it as soon as the first cascade failure appears.

- **The wasm must be rebuilt from the merged tree for the visual check.** The
  pre-merge (task-time) wasm carried the epoch's climate but not the merge;
  the visual verification used a binary rebuilt from the merged worktree so
  the eye-check reflected reality, not a stale snapshot.

- **The census debt is real and deferred by design.** The epoch's 1000-seed
  census baselines are stale until the Nathan-authorized AWS regen at the
  merge; the local gate skips censuses. This campaign is the *second* pending
  epoch in flight with The Confluence — the sequencing (land one epoch,
  regen, then the other) matters, and The Confluence landing first made this
  campaign's regen the sole pending one, avoiding the race.

## Follow-ups

- **Precipitation phase** (sleet / freezing rain / ice pellets / hail) —
  captured in the idea registry; a hydrometeor classifier over a vertical
  temperature column, a later level-0 refinement (hail is convective → the
  drawn-storms campaign).
- **Lake evaporation** — no lake surface exists yet (terrain marks endorheic
  basins but does not fill them); a hook for after the hydrology campaign.
- **The cloud→insolation feedback and the operator/relaxation solver** remain
  the level-2 feedback campaign's work; cyclicity, not field count, is its
  trigger.
