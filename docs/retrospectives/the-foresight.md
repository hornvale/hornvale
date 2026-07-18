# Retrospective — The Foresight (The Walk Milestone 2: the GOAP goal rung)

One page of process, not product. The product is chronicled; this is what
the close learned that the code does not record.

## What went well

- **The general-engine decision (decision-ledger #2) held up as the right
  abstraction, not a speculative one.** UNI-19's own framing — one small,
  tense-agnostic A* serving navigation, GOAP, confabulation, and prophecy —
  was a plausible-sounding argument before any code existed. It was tested
  against a concrete counter-pressure at G4: build the general kernel
  primitive, or a vessel-specific planner scoped to just this campaign's
  needs? The general form won because A* is genuinely domain-agnostic (a
  search over a small trait — `State: Ord`, `successors`, `goal`,
  `heuristic`), so a general primitive costs nothing extra to build and
  avoids rewriting the search per tense later. The discipline that made this
  safe rather than speculative: build the general engine plus **exactly
  one** instantiation (GOAP), with navigation/confabulation/prophecy
  explicitly named as followups, not sketched into the code as unused
  generality. `kernel/src/astar.rs` shipped with nothing in it that isn't
  exercised by GOAP's own tests.
- **Ideonomy's passes both survived to the shipped mechanism unchanged.**
  The graph-organon pass (cross-domain reinstantiation, decision #2) named
  the general kernel primitive and its determinism keystone before a line
  of `astar` existed — and the shipped function is exactly that: a pure
  `BTreeMap`/`BTreeSet` search with a total-order tie-break, no HashMap, no
  RNG, proven byte-identical across planted equal-cost ties. A second pass
  (decision #4's scale organon, weighing the minimal `{move, drink}` action
  set against the full MAP-27 verb-chemistry DSL) and a third (decision #5's
  combination pass, action-types × what-makes-a-plan-multi-step) together
  produced the precondition chain — the one design choice that makes this
  campaign GOAP rather than dressed-up pathfinding. Neither pass needed
  revisiting once code existed; both predicted the load-bearing mechanism in
  advance.
- **The reserved-seam payoff (The Wanting's decision #9) landed exactly as
  designed, with zero seam changes.** `decide`'s signature, `Intent`'s
  generalization from `{GoTo, Hold}` to `{Do(Action), Hold}`, and the
  `view`/`Perceived` parameter all survived this campaign untouched in
  shape — only `decide`'s *body* changed, from a hand-written proximity
  check to a full A* search. A reviewer scanning the diff for a seam change
  would find none; the whole campaign is a body-swap, which is precisely
  what a "seam reserved, not built" decision is supposed to buy the next
  campaign.

## What the campaign taught mid-execution

- **The drive refactor was bigger than "add a planner."** The Wanting's
  drive model computed thirst from a gradual rise-and-fall against
  proximity to the resource; The Foresight replaced the underlying event
  entirely — drive is now a fold over discrete, planned `drank` events, not
  continuous position. That is not an additive change; T3's diff touched
  588 lines of `liveness.rs`, more than either of the two feature tasks
  around it, because the drive model's *shape* changed underneath the
  planner, not just its trigger. A closing task inheriting "the planner
  plugs into the existing drive" would have been wrong about the size of
  the plug.
- **A mutation that doesn't fail is informative, not just noise.** T1's
  first attempted mutation of the tie-break (swapping the ordered
  `BTreeSet<(f,g,state)>` for a `BTreeMap<(f,g), State>` side-table that
  overwrites on tie) did not reproduce a failure — the test asserts
  self-consistency across repeats, and a deterministically-wrong-but-stable
  mutation stays internally consistent within one process. The real
  falsifying mutation needed an actual source of run-to-run variance
  (grouping tied states in a `HashSet`, whose iteration order is
  process-randomized) run across separate process invocations, which then
  failed 8/8 — including disagreeing with itself *within* a single run's
  100-iteration loop. The lesson generalizes past this campaign: verifying
  a "stable across repeats" assertion needs a mutation that can actually
  vary between repeats, not one that is merely wrong.
- **The T3 review catch was a genuine termination gap, not a style
  nit.** The idle-jump guard in `DriveMovements::step` had two halves
  (`next_act <= day` and `next_act > self.to.day`); dropping the first half
  and running a genuinely-unreachable-water test exceeded a 60-second
  timeout in debug and took 42.5 measured seconds even in release — the
  loop terminates (bounded by `MAX_STEPS`), but at a real, measured
  performance cost 10,000× the ordinary case. This is the second
  termination-class bug this liveness code has produced across two
  campaigns (The Wanting's own T3 found a hang from collapsed thresholds);
  the fix this time is a regression test that mutation-verifies the
  specific guard, not just an assertion that the ordinary path works. The
  same review pass also pruned two now-dead `DriveParams` fields
  (`fall`, `initial`) left over from the pre-planner model, correcting a
  doc comment that had gone stale mid-refactor (claiming the planner still
  consulted `sated` for the return-home decision, when only `Session::needs`'s
  felt-state prose reads it post-refactor).

## Followups (register, promoted verbatim)

1. **Arbitration — goal selection among competing drives.** The trigger
   this campaign's cardinality-1 scope deliberately deferred; the next
   `decide` body-swap.
2. **The psychology-vector cost function.** Uniform action cost this slice;
   a personality-weighted `successors` cost is PSY-6's distinctive payoff.
3. **Planning over belief, not truth.** `view` is ground truth this slice;
   projecting a possibly-false belief through it is UNI-16's decisive move
   and a body change to what fills `view`, not a seam change.
4. **The MAP-27 verb-chemistry action DSL.** Two hardcoded actions this
   slice; the general property-bag/authored-verb form is the richer action
   set.
5. **Plan caching.** Re-plan every decision point this slice (deterministic,
   stable, cheap enough not to matter yet); cache and invalidate when
   measured to be worth it.
6. **The other tenses of the A\* engine.** Backward (confabulation),
   spatial (navigation), forward (prophecy) — each a future `SearchSpace`
   instantiation of the same kernel function.
7. **The player plans too.** The possessed agent's own goals surfacing as
   available/suggested actions — rides player-acts-mutate.

## Confidence Gradient

Checked `book/src/open-questions.md` for a moved bet, the same grep The
Wanting's own retro ran, plus this campaign's added vocabulary (`plan`,
`goal`, `A*`, `precondition`, `astar`, `GOAP`). No hit beyond the same
unrelated room-mesh term ("O(1) integer neighbour walk") both prior retros
already dismissed. **No row moved, for the same reason stated twice
before**: the Confidence Gradient is scoped to Year-1/Year-2
*world-generation* research bets (refinement at scale, emergent economics,
historiography worth reading, the deep-time forcing horizon) — none of
which reference agents, NPCs, planning, or the game layer. A planning agent
is a genuine capability leap for the *liveness* layer, but that layer sits
beneath this chapter's bookkeeping, not inside it; three consecutive
liveness campaigns (The Quickening, The Wanting, The Foresight) have now
confirmed this boundary rather than asserted it once.

The idea registry (`book/src/frontier/idea-registry.md`) is this
campaign's correct home for the idea-level movement, and it was updated:
**PSY-6** now records that the *goal rung* shipped — the reserved seam's
first body-swap, a real A* planner over a precondition chain, with
arbitration/cost/belief still exactly as deferred as before. **UNI-19** now
records that the general kernel engine itself shipped, not just its
framing — `hornvale_kernel::astar` over `SearchSpace`, with the
PATH-DETERMINISM keystone proven by a planted-tie test — and that GOAP is
its first instantiation, with the other tenses now one `SearchSpace` impl
away rather than a hypothetical reuse. **MAP-27**'s row gained a note that
this campaign's two hardcoded actions are the smallest concrete precedent
for "a verb IS a GOAP operator," with the richer property-bag DSL remaining
the followup.
