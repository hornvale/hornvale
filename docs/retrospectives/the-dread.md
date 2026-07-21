# The Dread — retrospective

One page, process not product. Added the fifth drive, danger/fear, as the
avoidance twin of hunger: a flow drive that flees a threat field (the uncanny +
lethal extremes) down its gradient, with signed serviceability so it reshapes the
other drives' paths.

## What worked

- **The hunger↔danger symmetry was a design gift, and ideonomy found it.** Coming
  off The Provender, the temptation was to build danger from scratch. The
  cross-domain pass instead surfaced that danger is hunger reflected through
  approach→avoidance — `threat_niche · hazard_field` is `diet_niche ·
  resource_field` with the sign flipped — so `flee_step` is `forage_step`
  inverted, and the whole drive fell out of the flow-drive template thermal
  already established. **Lesson: the campaign right after a rich one should ask
  first whether the new thing is the old thing negated. Often it is, and the
  build collapses to a reflection.**

- **The default-that-preserves-old-behavior pattern, a fourth time — to its
  cleanest end yet.** `threat_value` defaults to `0.0` (safe); danger commits no
  fact and adds no action. The result: 285 tests passed with zero registration
  churn, genesis byte-identical, and — because seed 42's walk never nears a
  strange site — the possession galleries byte-identical *too*. The leanest drive
  produced the smallest blast radius: only the source changed, not one artifact.

- **A cross-domain reading became load-bearing mechanism, not flavor.** The
  artificial-potential-field idea (navigation = attractive goals + repulsive
  obstacles) is *why* danger's serviceability is signed rather than clamped — the
  one real change to the arbitration contract, and the thing that makes a thirsty
  creature route around a hazard instead of dying in it. The ideonomy pass didn't
  decorate the design; it supplied its central move.

## What to watch

- **A flow drive can't veto from a safe cell — anticipation was required, and it
  is a genuine model choice.** The keystone test failed first because a creature
  standing safely beside a hazard felt nothing and so couldn't refuse the step in.
  The fix — urgency reads the max threat over the cell *and its neighbours* — is
  correct (fear is anticipatory) but it means danger is roused while a hazard is
  merely adjacent. The null-control held (strangeness is sparse), so it does not
  inflate real distress; but if a future world seeds dense strangeness, "wary
  beside every strange site" could read as more fear than intended. Watch the
  by-cause-danger fraction when the strangeness budget grows.

- **The threat field is deliberately thin.** v1 reads only *placed*-exotic
  strangeness plus a lethal-temperature backstop — the ambient derived strangeness
  and the full habitability model (sea-level, elevation) were skipped to keep the
  per-step read cheap (the hot-path lesson from the phenomena seam). It is the
  right coarse rung, but it means danger fires only near the sparse exotic sites;
  do not mistake a quiet by-cause-danger for the drive being inert.

## Followups (captured to a PSY row + PSY-10)

- The per-kind **threat niche** (negative weights = attraction: the fire
  elemental, the undead's holy-ground dread, the predator's inverted fear of its
  prey); the **boldness** psychology dial (cower↔brave, with its danger-*approach*
  extreme); **remembered danger** (a `believed_hazard`, the inverted twin of
  believed-water); **fear-contagion** (reading others' distress affect as a threat
  cue). Predator-as-moving-hazard rides on **PSY-10** — danger is its
  perception-side, built first.
- The `arbitrate` `Disposition`-struct tidy is now six drives/dials deep and
  overdue.

## Confidence Gradient

Checked `open-questions.md`: this sits in the cognition/drive layer beneath the
world-generation bets, moving none. No re-score.
