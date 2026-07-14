# Retrospective: Eclipse Seasons

**Closed:** 2026-07-14 · **Outcome:** merged · one-page, process lessons only
(decision 0020).

## What happened

Brainstormed from the stale `SKY-eclipse-seasons` registry row (its opening
clause — "moons have no orbital inclination" — had already gone stale when
SKY-6 shipped inclination). Thirteen minutes before this spec landed, a
parallel session had committed *The Long Count*, claiming the identical
`moon-nodes` stream and the same dated-eclipse core from a different
starting point (a long-clock audit). Nathan adjudicated within the hour:
Eclipse Seasons owns the eclipse core, since it went deeper on the
contested ground (ground tracks, sight tiers, the full recurrence ladder).
Both sessions amended their specs same-day to record the adjudication; this
campaign adopted the Long Count plan's naming for the shared surface
(`eclipses.rs`, `Moon.node_longitude_deg`) so its already-written eclipse
tasks — the original plan's pre-rescope Tasks 1–5 — transferred verbatim
rather than being rewritten. Twelve tasks executed under
subagent-driven development with the campaign-autopilot skill, seven
ledgered decisions, one whole-branch final review, and one shared AWS
census regen coordinated with The Long Count at close.

## Lessons

1. **Ceding cleanly, adopting verbatim, beat racing or forking.** The
   collision cost one spec amendment and no wasted implementation, because
   the losing side's plan tasks were reusable text, not sunk work — the
   naming reconciliation (decision ledger #1) explicitly preserved the
   Long Count's already-written task bodies under this spec's ownership.
   The same pattern that made The Long Count's retro note 3 true ("plan
   code samples are schematics, not gospel") cuts the other way here: when
   the schematic is *good*, transfer it rather than re-deriving it.
2. **Campaign-autopilot arrived mid-execution and was adopted at the next
   gate.** The skill wasn't in play at spec/plan time; ledger entry #4
   records the dispatch-checklist fix it prompted (see lesson 3) but the
   larger fact is procedural: a skill landing mid-campaign doesn't force a
   restart — it slots in at the next natural checkpoint (G5 here) and the
   campaign finishes under it cleanly.
3. **A CI-only check cost a review round before it was mechanized.** The
   type-audit gate (`tools/type-audit -- check`) doesn't run in
   `gate-fast`, so Task 2's pub-boundary tags weren't caught until CI —
   one avoidable review round. The fix was process, not tooling: every
   subsequent dispatch for a pub-adding task now instructs running
   `type-audit -- check` before commit (ledger #4). Cheaper than adding the
   check to the fast gate, and it held for the remaining ten tasks with no
   repeat.
4. **The same test-helper bug was caught independently twice.** The plan's
   own scaffolding for night-longitude tests carried an antipode-formula
   bug; two different implementers (Tasks 7 and 9) hit it in isolation and
   fixed it the same way, without cross-referencing each other's work. Two
   independent catches of the same planted defect is a healthier signal
   than one catch would have been — it means the review discipline isn't
   riding on a single implementer's attentiveness — but it also means the
   plan-writing lesson from The Long Count's retro (verify test scaffolding
   against the real API, don't transcribe it as gospel) generalizes past
   that one campaign.
5. **Reviewers earned their keep on the two findings that mattered.** The
   Task 6 review caught and correctly resolved whether the pantheon
   reshuffle (a new always-present phenomenon changing every mooned seed's
   derived deity names) needed a naming epoch — it did not, and the review
   traced the reasoning back to the libm-migration precedent rather than
   guessing (ledger #5). The Task 6 rate-guard review verified the
   coarse-vs-dated consistency test actually bound the two tiers together
   rather than merely compiling. Both are exactly the kind of finding a
   fast, mechanical review pass would miss — worth naming explicitly
   because most of the other nine per-task reviews surfaced only minors.
6. **The census-staleness trade held its shape a third time.** Schema
   growth (four metrics) left 32 fixture-reconstruction tests red on-branch
   until the one shared AWS regen — identical in mechanism to The
   Self-Describing Sky's and The Long Count's closes. Coordinating a single
   regen across both same-day campaigns (rather than paying the AWS cost
   twice) was the right call and is now the precedent, not a one-off.

## Follow-ups

Carried in the worktree's `.superpowers/sdd/followups.md`, promoted here
verbatim:

- Add a test for the `None`-on-degenerate-synodic branch of
  `moon_ecliptic_latitude_deg` (Task 2 review, minor — carried to final-review
  triage).
- Consider wiring `tools/type-audit -- check` into `make gate-fast` (it is
  CI-only today; cost Task 2 a review round).
- Astronomy `stream_labels()` has no declaration-order test (settlement has
  one — `domains/settlement/src/lib.rs:265-280` pattern); Task 1's ordering
  slip would have been caught by one.
- `tier_refinement.rs`'s added eclipse-salience loop is redundant with the
  outer loop (plan-mandated; consider dropping at a future review).
- DRY: a shared `on_day_side` helper for `solar_eclipse_sight` /
  `lunar_eclipse_seen`; a single source for `l_moon`; a direct test for
  `moon_ecliptic_longitude_deg` (Task 7 review minors).
- Event-day phenomena carry both the coarse promise and the dated occurrence
  (ledger #6: keep both, deliberately — they are semantically distinct
  statements); revisit if possess playtesting finds the double listing
  noisy.
- `eclipse_events` at t≈0 gets a negative `window_from` from the placed
  pass — tolerated by the ceil/floor arithmetic but untested (Task 9 review
  minor).
- Almanac eclipse tests use contains-only checks (no paragraph-separation or
  no-eclipse-fallback coverage); the fallback wording is solar-specific
  though it fires for lunar absence too (Task 10 review minors).
- Prose-cohesion nit from final-review triage: the coarse promise names
  moons by size-word, the dated lines by ordinal — could read as different
  moons to a careful reader.
- Standstill facts (the 18.6-year moonrise-azimuth extreme) ride this
  campaign's committed `moon-node-period-days` beat but the instrument
  itself is the deferred follow-up row (`SKY-standstills`, registered at
  close).
