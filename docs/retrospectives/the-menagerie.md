# Retrospective — The Menagerie (ECS program, campaign 1)

Process lessons only; the product story is the chronicle. This campaign opened
the entity-component program: a program metaplan, then campaign 1
(spec → plan → subagent-driven execution). Named "The Seam" in the metaplan;
published as "The Menagerie" after a slug collision (below).

## What worked

- **Program metaplan first.** The god-struct problem was too big for one
  campaign, so the design run produced a *metaplan* (7 strangler-fig campaigns)
  before campaign 1's own spec. This kept campaign 1 scoped to a shippable seam
  while the firm choices (rate model, concurrency, determinism contracts) were
  settled once, for the whole program. Worth repeating for any multi-campaign
  architectural push.
- **Campaign-autopilot across a large design run.** The overlay carried a very
  long brainstorm (six ideonomy drills + a pressure-test), the metaplan G3, and
  campaign 1's G3 with zero vetoes; the two hard stops (spec review, merge) were
  where Nathan's attention actually landed. The ledger (#31–#51) is the durable
  record the conversation would otherwise have lost.
- **The strangler-fig seam held.** Splitting the struct (byte-identical), then
  guarding consumers (byte-identical), then authoring content, then cutting over
  (the one intended-drift step) meant every step before the cutover was
  reversible and provably behavior-preserving — the existing artifact
  drift-check certified each one for free.

## What bit, and the lesson

- **A genesis cutover's blast radius is under-predicted by the plan.** The plan
  expected the cutover to drift "census fixtures"; it drifted **12 always-run
  tests** across cli/lab/scene/worldgen, plus reframed a named determinism
  contract (species-pin isolation, now pin-*determinism* under a competitive
  stack) and shrank the golden world 276→66 settlements. Lesson: when a task
  changes committed *genesis behavior*, budget a reconciliation pass for the
  whole test surface, not just the obvious fixtures — and have the implementer
  STOP-and-report on the non-mechanical ones rather than blind-update (it did,
  correctly).
- **Verify a fix hypothesis before recommending it.** When the exit criterion
  failed, the controller hypothesized that measuring strongholds by
  biomass/share (not density) would reveal them. A throwaway re-measurement
  **overturned** it — biomass collapsed *worse*, and the real limit was deeper
  (a single resource-supply field, so resource niches are magnitude-not-spatial).
  Running the measurement before taking the hypothesis to Nathan saved a wrong
  recommendation. Keep doing this: a strong first-principles story is a reason to
  test, not to assert.
- **An exit criterion can hit a model boundary; ship the structural win.** The
  campaign's payoff ("fauna hold resource-partitioned strongholds") was not
  achievable under the current habitat model. The honest move — taken — was to
  reframe: ship the structural deliverables (split + menagerie + cutover, all
  validated), leave the payoff as a preregistered `#[ignore]`d test, and capture
  the real fix (per-axis spatial resource fields) as a named next increment. A
  campaign is allowed to discover its stated payoff needs a prerequisite; forcing
  or weakening the assertion would have been the failure.

## Process gaps to close

- **Check chronicle-slug availability when naming a campaign.** "The Seam"
  collided with the already-shipped game-layer Seam (windows/vessel). The
  collision surfaced at *close*, when the chronicle wouldn't write, not at
  naming. Cheap guard: `ls book/src/chronicle/` for the intended slug the moment
  a campaign is named.
- **Stage-boundary absorption cadence held, but only just.** Main moved 21
  commits (then 1 more) during execution; both absorptions were clean textually,
  but the first surfaced a real semantic collision (a main test iterating the
  roster through a peopled-only accessor, which our fauna panicked). The
  preflight's "semantic collisions hide under clean merges" warning earned its
  keep — always re-run the full gate on the merged result, never assume.

## Carried forward

- **Per-axis spatial resource fields** (the real fix for fauna strongholds) and
  **exposing per-cell resource share** on the coexistence stack — captured in the
  followup register and idea registry; the `#[ignore]`d
  `menagerie_fauna_hold_resource_partitioned_strongholds` test is their
  preregistered target.
- **Peoples-fauna competition in one mixed stack** (a dragon's crag excluding
  villages) — deliberately deferred to keep the cutover at The Niche's scope.
- **Instance `EntityId` stability under roster growth** — the alphabetical-mint
  renumbering is benign now (worlds re-derive) but is the instance-side of the
  `KindId`-label question campaign 2 takes up.
