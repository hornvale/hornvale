# The Belonging — retrospective

One page, process not product. Added the sixth and last Temperament drive,
social affiliation: a comfort-tier flow drive that pulls a lonely creature home,
company proxied by home-proximity (Tier-0), reachability-gated so it never
falsely distresses.

## What worked

- **Naming the landmine at G3 saved the campaign from it.** The spec flagged,
  before a line was written, that perceiving individual agents was both a
  determinism and a null-control hazard, and chose a company *field*
  (home-proximity) instead. That call is exactly what made the build tractable —
  the alternative would have dragged multi-agent perception, its ordering
  determinism, and a much harder null-control into a drive that is meant to be
  the gentlest of the six. The reserved population field + attachment are the
  honest Tier-1.

- **The reachability gate turned a real conflict into the drive's best idea.**
  The first model (a plan-home affordance that read *unreachable* as maximal
  loneliness) broke everything at once: it produced false distress in the
  null-control (6% of ticks — creatures whose budget-limited plan home merely
  timed out) AND it re-broke the thirst-stranding harness (a homeward pull
  rescued the stranded creature, since its home sat on its water). Both failures
  had one root: an unreachable home was being treated like an unmet *survival*
  need. Gating the drive's pull on reachability — head home when you can, go
  dormant when you can't — fixed the null-control and left the stranding harness
  untouched in a single stroke, and it is the *correct* semantics: loneliness is
  comfort, and an unreachable home is a relocation, not a death. **Lesson: when a
  new comfort drive collides with the survival harness, the fault is almost always
  that the comfort drive is being scored on the survival scale. Find the tier
  confusion.**

- **The default-preserves-behavior pattern, a fifth time, to the same clean end.**
  No new predicate, no new action, `threat`-style default; the result was 285
  tests green, genesis byte-identical, and — because the seed-42 herder forages
  close to home and never crosses the loneliness threshold — the possession
  galleries byte-identical too. Two drives running (Dread, Belonging) and neither
  moved a committed artifact.

## What to watch

- **There is no chronic-social-distress harness — by design, and it is worth
  stating.** The spec's test plan imagined a "stranded from home reads chronic
  social-Frustrated" scenario. The reachability gate makes that impossible: a
  free creature with a reachable home always heads home (Searching), and one with
  an unreachable home goes dormant — social *never* chronically distresses, which
  is precisely *why* the null-control holds. So the by-cause-social branch reads
  zero in every real world, and the homing behaviour is proven through the
  arbitration unit test, not an end-to-end distress run. If a future refinement
  (a *boxed-in* creature, the population field, out-group aversion) gives social a
  genuine distress mode, add the harness then.

- **The reachability cutoff is a discontinuity.** Loneliness rises with hop-
  distance up to the plan budget, then drops to zero (dormant) beyond it — a
  creature just inside the budget feels the most homesick, one just outside feels
  nothing. It is behaviourally invisible (both cases act sanely — head home, or
  do nothing) and it only bites at the ~budget boundary, which no natural forager
  reaches. But it is a wart; the population field (a smooth spatial company
  gradient) would dissolve it.

## Followups (captured to a new PSY row)

- The population field (Tier-1: affiliate at any village), attachment (specific
  others — the first true multi-agent perception), the gregariousness niche
  (solitary ↔ eusocial, the sign-flip at solitary), status/dominance (reading the
  authored `Sociality`), out-group aversion (a stranger as a mild threat — where
  social meets The Dread), and construct/hive affiliation (the ametabolic
  own-drive).
- **The `arbitrate` `Disposition`-struct tidy is now six drives deep** — the
  clear next consolidation campaign, and the one Nathan set aside at this
  campaign's G1.

## Confidence Gradient

Checked `open-questions.md`: this sits in the cognition/drive layer beneath the
world-generation bets, moving none. No re-score.
