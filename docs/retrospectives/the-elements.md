# Retrospective — The Elements

One page of process lessons. The product — the phenomena-source roster and felt
weather — is in [the chronicle](../../book/src/chronicle/the-elements.md).

## What worked

- **A fidelity fork surfaced to Nathan mid-stage, not decided silently.** The
  first conservative calibration muted mild climes entirely — the world would
  have gained the *mechanism* but changed almost nothing visible, because
  settlements sit in mild country. Rather than accept that (defensible) reading
  or unilaterally widen it, the choice — *is the world felt broadly, or only
  where notable?* — went back to Nathan with both options and their deltas. He
  chose felt-broadly; the result feels the world everywhere yet keeps
  weather-gods rare. Escalating the calibration, not the mechanics, was the
  right cut.

- **The performance regression was caught by a timeout, not papered over.** When
  `regenerate-artifacts.sh` blew past 20 minutes, the reflex could have been to
  raise the timeout and re-freeze. Instead it was profiled: world *generation*
  had regressed from ~3s to 52s because the naming/religion stages observe the
  world dozens of times and each observation rebuilt the entire climate. The fix
  (build sources once per generation) was pure — byte-identical output — and
  sped the whole suite 30min → 3min. Regenerating and re-timing *yourself* after
  a subagent reports green is what turned a silent 10× slowdown into a fix.

- **Byte-identity as the merge's semantic gate.** Absorbing the-doctrine (56
  commits, both sides rewriting `worldgen/lib.rs`) auto-merged textually. Trust
  came not from the clean merge but from regenerating the world and proving a
  fresh seed-42 matches the re-frozen golden, that it carries both campaigns'
  content, and that `make gate` runs both test suites green.

## What to carry forward

- **The observation seam is a hot path — a provider must be cheap to *hold*, not
  just to *ask*.** The emitter itself was fast; the cost was *constructing* it
  per observation. Any future domain that joins the phenomena roster with an
  expensive-to-build provider must be built once and reused, never per
  `observe`. This is now the pattern (sources built once per generation); honor
  it.

- **Subagent round-trips are expensive; match the tool to the fix.** Stage 2
  took four passes (implement → recalibrate → perf-fix → merge), each a slow
  round-trip. The `NearestCellIndex` cache and the percept-flip were small enough
  to do inline; the emitter and the source-hoisting genuinely needed the agent's
  context. When a fix is a localized formula or a one-field change, prefer
  inline; reserve the round-trip for changes that span the wiring.

- **Stage-boundary absorption lagged the campaign's length.** `main` moved 56
  commits (an entire campaign) while The Elements ran, and the absorption all
  landed at close. It merged cleanly this time — the only conflict was a
  generated report — but a 56-commit merge of the composition root is a
  semantic-collision hazard that got lucky. For a multi-day campaign, absorb at
  each stage boundary even when main looks quiet.

## Handed forward (followups)

- **Genesis re-derives the terrain several times (~9 full rebuilds per world).**
  Found while fixing the phenomena perf regression; it is *pre-existing*, not
  ours, and dominates both debug and release genesis time. It is
  byte-identity-risky (threading one terrain/climate through every genesis stage
  touches the composition root broadly), so it was deliberately left standing.
  Captured durably as an idea-registry row (`PERF-genesis-terrain-rebuilds`) to
  be handled soon. **This is the campaign's flagged follow-up.**
- Terrain / demography standing-condition sources (elevation band, coastal,
  strife/wilderness/refugia) — the roster now makes them cheap to add.
- `ice` remains a percept `Gap` (a cold, wet cell reads as snow); a distinct ice
  phenomenon, if wanted, is future work.
