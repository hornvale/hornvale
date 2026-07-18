# Retrospective — The Wanting (The Walk Milestone 2: the drive rung)

One page of process, not product. The product is chronicled; this is what
the close learned that the code does not record.

## What went well

- **The GOAP-boundary reasoning held up under its own weight, not just as a
  slogan.** Decision-ledger #2 drew the line at cardinality-1 — one drive,
  no arbitration, no planner — for a specific, checkable reason: arbitration
  is only a problem once *two* setpoints compete for the same agent's next
  move. A single drive, by construction, never reaches that fork, so
  building the A* engine now would have nothing to arbitrate between. That
  argument was tested at G3 and again at G4 (the plan self-review) and held
  both times without needing to be re-litigated — the scope stayed exactly
  as small as the reasoning justified, neither creeping toward "just a
  little planning" nor under-delivering the drive rung itself.
- **Ideonomy's two decisive passes both survived to the shipped mechanism
  unchanged.** Pass A (cross-domain reinstantiation, decision #3) named
  drive = belief = index — a fold over the one event ledger, never a second
  source of truth — before a line of `drive_at` existed, and the shipped
  function is exactly that fold, tested for reload-stability precisely
  because the pass predicted it would need to be. Pass A again (control-
  theory reinstantiation, decision #4) named the thermostat dead-band as the
  textbook fix for a memoryless drive's thrash *before* any thrash was ever
  observed in code — and the no-thrash mutation test (T3) exists only
  because that failure mode was named in advance, not discovered by
  surprise. Two passes, zero overturns, both load-bearing at ship.
- **The reserved-seam discipline (decision #9) produced exactly the seam it
  promised and nothing more.** The risk named at G3 was over-reservation — a
  speculative planner dressed as "just an interface" — and the shipped
  `Goal`/`Intent`/`decide`/`world_view` seam is checkable against that risk
  directly: no cost function, no arbitration, no action graph, one function
  whose body is the whole reactive controller. A reviewer scanning
  `liveness.rs` for scope creep would find none to flag.

## What the campaign taught mid-execution

- **A closing task inherits open trade-offs, and this one inherited two, not
  one.** Task 3's report flagged the over-time transcript would go stale
  once the tick's body changed (the same shape The Quickening's own close
  named) — but T4's own regen-and-diff step surfaced a *second*, unflagged
  instance: the frozen **day-0** transcript also carries a `wait 90` line,
  and it too changed output once the drive model replaced the fixed clock
  schedule. Nobody's report predicted this one specifically; it fell out of
  actually running the regen rather than reasoning about it in advance. The
  lesson generalizes past this campaign: "the transcript that exercises
  `wait`" is not one artifact, it is every artifact that happens to contain
  a `wait` line, and a closing task's regen-and-diff step is the only
  reliable way to find all of them — flagging in a report is necessary but
  the diff is what's sufficient.
- **Two review catches, both structural rather than cosmetic, both in
  `liveness.rs`.** The T1 review found `resource_room`'s neighbor-elevation
  comparison using native float ordering instead of `total_cmp` — a
  determinism-contract violation the constitution bans outright, caught
  before it could ever produce a platform-dependent resource choice. The T3
  review found something sharper: a misconfigured `DriveParams` (thresholds
  collapsed together) didn't just fail an assertion, it **hung** — an
  unbounded same-day `GoTo`/`GoTo` alternation the existing `Hold`-only
  break couldn't catch, discovered by mutation-testing the no-thrash guard
  rather than by reading the code. The fix (a strict day-progress guard) is
  now defense-in-depth for any future `DriveParams`, not just an assertion
  scoped to the one authored constant (`SUSTENANCE`). Both catches came from
  review passes explicitly built to look for exactly this class of bug —
  mutation-verification and the float-sort rule are cheap precisely because
  they're routine, not because the bugs they catch are rare.
- **The felt-state read needed a plan correction the brief's own author
  flagged in advance, and the correction held.** Spec §4.5 wanted the
  possessed agent to perceive its own drive; G4's self-review caught that
  the player's moves are never committed as `agent-at` (only NPCs' are), so
  `drive_at(player)` would fold an empty history and report eternal,
  meaningless thirst. The shipped `needs` reads co-located NPCs instead — a
  smaller, honest slice that still delivers "the drive is visible," with the
  player's own felt state riding the sibling player-acts-mutate campaign.
  Catching this at G4, before any test was written against the wrong target,
  is the same "read the numbers before writing the story" discipline The
  Quickening's demography finding used — applied here to an interface
  contract instead of a data field.

## Followups (register, promoted verbatim)

1. **The GOAP / A* planner** — multi-step plans to reach a goal,
   deterministic A* seed-tiebroken, over authored verbs. The reactive drive
   is the rung below; the planner is its own campaign, triggered by followup 2.
2. **Multiple competing drives + arbitration** — thirst vs fatigue vs hunger
   vs safety; competition forces prioritization, which is the goal rung.
   Cardinality-1 was deliberately chosen to avoid this here.
3. **Fatigue, safety, social drives** — fatigue overlaps the existing
   activity-cycle too closely to be a clean second drive; safety needs
   threats that don't yet exist; social is heterogeneous and needs the
   social graph plus other agents to be social *with*.
4. **Emotion as appraisal** — the rung above drive: the affective readout of
   the setpoint gap (a parched agent near reachable water feels differently
   than one that cannot reach it).
5. **The drive reads the projection, not ground truth** — the decisive move
   the future planner makes: seeking where the agent *believes* the
   resource is, which can be false or deceived. This slice reads ground
   truth; belief-reading is the Milestone-2 belief campaign, and the
   `world_view` seam is exactly where it slots in.
5b. **The player's own felt drive** — the spec wanted the possessed agent to
   perceive its own drive, but the player's moves aren't committed, so this
   slice shows the drive on the NPCs instead. The player's own drive needs
   player-acts-mutate (Campaign IV) to commit its moves first.
6. **The drive steers the player's options** — the possessed agent's own
   drive constraining or coloring its available verbs (a parched player's
   actions weighted toward water) — the drive as a game mechanic, not just
   flavor.
7. **Player-acts-mutate (Campaign IV)** — the player satisfying its own
   drive by acting and the world remembering it — the sibling slice that
   unblocks followups 5b and 6.

## Confidence Gradient

Checked `book/src/open-questions.md` for a moved bet, the same grep The
Quickening's own retro ran (`walk`, `liveness`, `agent`, `npc`, `possess`,
`inhabited`, `game layer`) plus this campaign's own vocabulary (`needs`,
`drive`, `motivation`, `homeostatic`). The only hits are the unrelated
room-mesh term ("O(1) integer neighbour walk") already dismissed by that
retro. No row in this chapter moved, for the identical reason given then:
the Confidence Gradient is scoped to Year-1/Year-2 *world-generation*
research bets (refinement at scale, emergent economics, historiography worth
reading, the deep-time forcing horizon) — none of which reference agents,
NPCs, or the game layer. This campaign's payoff sits beneath that chapter's
bookkeeping, not inside it, exactly as The Quickening's did one campaign
earlier.

The idea registry (`book/src/frontier/idea-registry.md`) is this campaign's
correct home for the idea-level movement, and it was updated: **PSY-6**
(the motivation engine) now records that the *drive rung* shipped — a real
homeostatic setpoint with memory and a hysteresis dead-band, cardinality-1 by
design — while the *goal rung* (arbitration, A* search, the psychology-
vector cost function, planning-over-belief) stays exactly as deferred as
before, with the reserved `decide`/`world_view` seam named as the slot the
eventual planner fills as a body-swap. **UNI-20** (the derived-view
architecture) now records the drive as a second same-session instance of
the same shape The Quickening's NPC-position view already proved: drive =
fold = belief = index, four names for one idea over the one ledger.
