# The Wakeful Sun — retrospective

One page, process not product. Swapped The Slumber's wake read from a
fractional-day window to the real sun (`Terrain::solar_altitude` +
`LocaleTerrain::with_calendar`), latitude × season × the terminator.

## What worked

- **A reserved seam made a cross-crate change nearly frictionless.** The Slumber
  shipped with `is_awake` explicitly framed as a body-swap and the astronomy
  substrate (`solar_altitude_at`, latitude, locked→None) already there. The
  refinement was: add one Terrain method with a byte-identical default, override
  it in the one live adapter, thread a calendar. 88 vessel tests passed
  untouched; the fallout was a single null-control line. **Lesson: naming the
  future body-swap in the shipping campaign — and confirming its substrate —
  turns the follow-on from a rewrite into a wiring job.**

- **The default-that-preserves-the-old-behavior pattern, again.** As The
  Kindling's coupling collapsed to the flat rate at thermoneutral, here the
  `solar_altitude` DEFAULT is the fractional-day sun, so every planted/synthetic
  test terrain kept its exact behavior and only the live `LocaleTerrain` gained
  the real sun. The regression surface shrank to the worlds that actually change.

- **Correct physics surfaced a too-strict test, and the fix was the honest
  semantics.** Under a real sun and a diurnal climate, a healthy seed-42 world
  caught one transient distress tick in 240 — so the null control moved from
  "prevalence == 0" to "no chronic distress, negligible transient." That is what
  the metric always meant; literal-zero was an artifact of the coarse cycle.

## What to watch

- **Layering forced the injection point.** `LocaleContext` (in `locale`) can't
  build a calendar (that's `worldgen`, which depends on `locale`), so the
  calendar is built at the `vessel`/`lab` layer and threaded into `LocaleTerrain`.
  Worth stating: a domain/window can't reach *up* for a capability; inject it
  from where the composition already happens.

- **Emergent behavior arrived un-asked-for and unverified.** Polar-night
  hibernation and locked-world always-awake fall out of the same read, but no
  test exercises a polar or locked world's wake cycle end-to-end yet. Captured as
  a followup — the general engine (PSY-9) is where those get their harness.

## Followups (captured to PSY-9)

- End-to-end coverage of polar-night hibernation and locked-world fatigue-only
  rest (a harness scenario at extreme latitude / a locked world).
- The rest of the chronobiology engine: the endogenous free-running oscillator
  (jet lag, cave-drift), other zeitgebers, the periodic-grid of predicted
  creatures.

## Confidence Gradient

Checked `open-questions.md`: this sits in the cognition/drive layer beneath the
world-generation bets, moving none. No re-score.
