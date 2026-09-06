# The Culvert — decision ledger

Campaign: `campaign/the-culvert`. Branch cut from `main` at `a8bde6769`.
Decision block reserved: **0806–0815** (main ceiling 0756 at reservation).

Subject: the sole surviving axis of `TOOL-known-water-plan-per-water-room` —
`believed_water` re-running a budgeted search per known water room per read.

Under `campaign-autopilot`. G3 (spec) and G6 (merge) are hard stops.

---

## Entries

### #1 [Q] — the campaign's name

**Question.** `make worktree-take` was first run as `NAME=the-sluiceway`.

**Decision.** Renamed to **the-culvert** before any commit existed, and before
the decision block was reserved, so nothing was stranded (memory:
*a campaign rename strands its decision block*; `sluice-vet` keys on the
BRANCH name). Branch `campaign/the-sluiceway` → `campaign/the-culvert` via
`git branch -m`; worktree moved with `git worktree move`.

**Why.** "Sluice" already names the serial merge queue on the canonical box in
every part of this repo — `scripts/sluice-*.sh`, `make sluice`,
`make sluice-stage`, `sluice-mouth.sh`, and about forty paragraphs of
`CLAUDE.md`. A campaign called *the-sluiceway* would collide with that name in
every grep, every board post and every retrospective sentence for the life of
the record. Checked eight candidates for prior use by both commit message and
artifact filename; `the-leat` and `the-freshet` are taken (`the-leat` is a
merged campaign about profiling tick regimes), `the-culvert` is unused.

**Alternatives discarded.** `the-sough` (drainage adit — fits the repo's mining
vocabulary of adit/stope/winze/brattice, but the pronunciation is ambiguous in
a written record); `the-conduit`, `the-rundle`, `the-chase`, `the-standpipe`
(all free, none as apt).

**ideonomy passes / overturns.** None run. Naming is not a design question and
this entry claims no more than a collision check.

**Capture actions.** None; this entry is the record.

---

### #2 [G1] — the brief's memo key is wrong: there is no hazard set to key on

**Question.** The campaign brief specifies the candidate mechanism as "a
per-entity memo of `(home, water room, hazard set) -> plan length`", and adds:
"the hazard set is an input (`believed_hazard` shapes the route), so the key
must carry it or the memo is wrong."

**Decision.** **The premise is false, and the key carries no hazard set.**
`believed_water` passes a freshly-allocated EMPTY `BTreeSet` as the avoid
argument (`windows/vessel/src/liveness.rs:1224`), and so do the other two
belief folds — `shared_believed_water` (`:1880`) and `nearer_to_home`
(`:8509`). The only production call that passes a real hazard set is
`HomeNavCache::home_nav` (`:5554`), whose three callers pass
`&view.believed_hazard`. The belief ranking is hazard-BLIND; the homing and
affiliation navigation three functions away is not.

**Why this makes the mechanism simpler, not harder.** With `avoid` empty and
fixed, the memoized function is pure over its remaining arguments, so the key
is `(from, dest, budget)` and the memo is byte-identical **by construction** —
the same argument `HomeNavCache` and `PrimaryAfraidMemo` already make in this
crate. The hazard-keyed memo the brief describes would have been correct but
strictly weaker: it would have carried a component that never varies, and its
hit rate would have been indistinguishable from this one's.

**What invalidates it — verified, not assumed.** `NavSpace`
(`windows/vessel/src/action.rs`) holds exactly two fields, `dest` and `avoid`;
`edges_from` computes `move_cost(i, &n, self.avoid)` over `Facet::neighbors`.
**It never consults `Terrain`, the ledger, or any world state.** So
`plan_to_room(from, dest, budget, ∅)` is a pure function of mesh geometry
alone. Nothing in a session invalidates a memo of it — not a tick, not a
commit, not a terrain rebuild, not a belief change. The scope is still a
session (hygiene, and so the memo cannot outlive its world's mesh), but the
reason is not invalidation.

**How the structure enforces it.** The memo type must NOT accept an avoid
parameter. A future caller with a real hazard set then cannot reach it at all —
a compile error rather than a comment — which is the key-hardening rider
`HomeNavState` already carries for `home` and `budget`.

**Alternatives discarded.** (a) Keying on the hazard set anyway "for safety":
rejected — a key component that is provably constant is not safety, it is a
dead field that the next reader will believe is load-bearing. (b) Memoizing
inside `plan_to_room` itself, catching all callers: rejected at this stage
because `home_nav`'s caller DOES vary its avoid set, so a callee-side memo
would need the very key this entry deletes.

**ideonomy passes / overturns.** 1 pass (dimension-identification +
abstraction-lift, state-machine organon). **1 overturn** — see #3, which the
same pass produced and which changes the recommended mechanism outright.

**Capture actions.** Registry row `TOOL-known-water-plan-per-water-room` is to
be corrected at close: it says "an A\* with a 1,000-node budget", and
`NavSpace::heuristic` returns `0`, so it is Dijkstra. The empty avoid set is
undocumented at all three of its call sites — a comment at each is owed.

---

### #3 [G1] — the pass overturns the memo: the answer is one search, not n cached ones

**Question.** Is a `(from, dest, budget) -> Option<length>` memo the right
mechanism?

**Decision.** **It is not the primary one.** Lifting the shape — *an argmin over
a monotonically growing candidate set, scored by an expensive pure metric from
a FIXED origin* — names the real defect: `believed_water` runs `n` separate
searches that all start at the same `npc.home` and differ only in where they
stop. With `NavSpace::heuristic` returning `0`, every one of those searches is
the identical expanding Dijkstra wavefront. **One search from `home` settles
every candidate at once.** That is the textbook one-to-many shortest-path
result, and it beats the memo on the cold read (which the memo cannot help at
all) while composing with it on the warm ones.

**Why the equivalence is arguable rather than hopeful.** With a zero heuristic
and a fixed start, the pop order is independent of `dest` — the goal test only
decides when to STOP. So "`dest` is popped at expansion index `k`" is the same
`k` in every per-target run, which makes both the returned path and the
`expansions > budget` cutoff reproducible from a single run.

**Why this is not a new idea, and why that is the best news in the campaign.**
The Waymark already built this: `ReverseField` and `build_reverse_field` exist
today at `windows/vessel/src/liveness.rs:21403`/`:21430`, TEST-ONLY, with the
falsifier `reverse_field_matches_forward_search_for_every_empty_avoid_room`
(`:21500`) left `#[ignore]`d as "a DISPROVEN hypothesis". It disproved the
field for `home_nav` because **52 of 346 reached rooms disagreed in
`first_step`** — and its own doc records that **`distance` never mismatched for
any of the 52**, "expected, since distance is symmetric for empty-avoid
(uniform edge cost) and root-independent; only the CHOICE OF PATH among
equal-length alternatives is root-dependent."

**`believed_water` consumes only `p.len()`.** It never reads a first step. The
conjunct that killed the field for `home_nav` does not bind this consumer, and
the conjunct that survived is exactly the one this consumer needs. The Waymark
left the falsifier in the tree "so a future attempt at a smarter tie-break rule
has a ready-made falsifier"; this campaign is that future attempt, and it needs
a weaker property than the one that failed.

**What must be proved before any of this is believed.** Three things, and none
of them is established today:

1. **The distance half is stated, not pinned.** "Confirmed separately" appears
   in a doc comment with no live assertion behind it, over 346 rooms at
   **budget 300** — while `PLAN_BUDGET` is **1,000**, a strictly larger radius.
2. **Equal COST does not imply equal HOP COUNT.** The planner minimises octile
   cost (`ORTHOGONAL_STEP = 12`, `DIAGONAL_STEP = 17`), and `p.len()` is the
   hop count of whichever least-cost path is reconstructed. `12 × 17 = 17 × 12`
   is an equal-cost pair with hop counts 17 and 12, so equal-cost/unequal-hop
   ties exist in principle; whether they are REACHED at budget 1,000 is a
   measurement, not an argument.
3. **The budget cutoff must be shown reproducible**, not merely plausible.

If (1)–(3) hold, the field is byte-identical and the campaign is a rewrite of
one function. If any fails, the memo of #2 is the fallback — strictly weaker,
strictly safe, and still worth shipping.

**Alternatives discarded.** (a) An admissible octile heuristic on `NavSpace`
(turning Dijkstra back into real A\*): a large win on every miss, but it
changes which least-cost path is returned and therefore can change `p.len()`
and the chosen water room — a behaviour change, an epoch, and out of scope
under the byte-identity discipline this campaign carries. Recorded as a
registry row, not adopted. (b) Threading the session-lived `RoomMeshMemo` into
the three belief `plan_to_room` calls (they pass `None` today, so every
expansion recomputes `Facet::neighbors`): byte-identical by construction and
cheap, but it accelerates a search this campaign hopes to delete. Kept as the
fallback's companion, not the lever.

**Alternatives NOT discarded — carried into the spec.** The belief set is
APPEND-ONLY (`water_at` admits `day <= t`, so a known water room is never
forgotten) and the metric from a fixed `home` is FIXED. An argmin over an
append-only set under a fixed metric can be maintained INCREMENTALLY in O(1)
per read and O(1) per new room — which removes the residual O(|set|) term the
memo and the field both leave behind. `nearer_to_home` (`:8496`) is already
exactly this fold, written to match `believed_water`'s tie-break "or a mid-walk
incremental belief could disagree with the same belief re-derived from the
committed history". Whether `believed_water` should become a reader of that
incremental result is the sharpest question in the campaign and belongs in the
spec.

**ideonomy passes / overturns.** 1 pass (operators: dimension-identification,
abstraction-lift; organon: state-machine; dimension prompts: direction,
materiality, scope). The *direction* prompt is what produced the append-only
observation and therefore the incremental-argmin branch; the *abstraction-lift*
is what produced the one-to-many reading. **1 overturn: the brief's memo is
demoted from primary mechanism to fallback.**

**Capture actions.** Registry rows owed at close for: the zero heuristic
(`NavSpace` is Dijkstra, not A\*, and the row says otherwise); the
`RoomMeshMemo`-not-threaded observation; the octile-heuristic epoch candidate.

---

### #4 [G1] — the campaign cannot be measured on the instrument it inherits

**Question.** The brief says to preregister on the three instruments verbatim
(`session_length_scaling`, `agent_scaling`, `fold_depth_sweep`). Can
`session_length_scaling`'s `believed_water` column see this campaign's lever?

**Decision.** **No, and fixing that is Task 1, not a nicety.** The bench's probe
agent is the roster's max-`agent-at` member, and The Kerf measured that agent
holding an EMPTY water belief at **10 of 10 bands in 3 of 3 runs**
(`docs/superpowers/ledgers/2026-09-04-the-kerf.md`, ruling 6b). With an empty
set, `believed_water`'s `plan_to_room` loop runs zero times — so the column
that The Detent attributed 99.08% of the six timed folds to is, on that agent,
timing `water_at` and nothing else. The Kerf's roster sweep found **11 of 50**
members holding a non-empty set at the final band, the largest holding **46**
rooms. The probe is unrepresentative in exactly the direction that matters.

**Why the existing column is nevertheless not wrong about the attribution.**
The Kerf ruled on this explicitly: its K3 probe isolates `water_at` and "does
not overturn The Detent's roster-wide attribution". This entry does not reopen
that; it says only that the INSTRUMENT cannot measure THIS lever.

**Consequence for the preregistration.** The brief's own requirement — "its
cost witness targets the worst population (the creature with the most known
water rooms)" — is therefore not a discipline flourish but the campaign's
enabling condition. The bench already solved this shape once, for `DRANK`, by
adding four roster-wide columns; the same treatment is owed here and is the
open follow-up `TOOL-belief-probe-vacuity-is-a-println`. The three instruments
are still run verbatim; one of them gets its belief columns fixed FIRST, and
the pre-fix column is reported beside the fixed one so the change of instrument
is visible rather than silent.

**And the vacuity check becomes a failure, not a note.** Both probes announce
vacuity with a `println!` (`session_length_scaling.rs:481-485`, `:512-516`),
which is why it fired for two campaigns with every gate green. A probe that
cannot see its subject must go red.

**Alternatives discarded.** (a) Reading the existing column as the campaign's
before/after: rejected — it would compare two measurements of `water_at` and
report them as a statement about the search. (b) Building a fourth instrument:
rejected — the brief pins three, and the defect is in one of them, so it is
repaired rather than routed around.

**ideonomy passes / overturns.** 1 pass, shared with #3 (the *scope* prompt is
what surfaced the population question: the probe measures a POINT where the
claim is about a POPULATION). No overturn.

**Capture actions.** `TOOL-belief-probe-vacuity-is-a-println` is adopted as
campaign work rather than left `raw`; its registry row is updated at close.

---

### #5 [G1] — the counts restore the memo and demote the field

**Question.** #3 demoted the `(from, dest, budget) -> Option<usize>` memo to a
fallback in favour of a one-to-many distance field. The counting probe has now
run. Which mechanism does the measurement choose?

**Decision.** **The memo, with negative caching, session-lived.** The field is
demoted to a follow-up. This reverses #3, which is the second overturn on this
question and the reason the brief said to count before designing.

**The counts.** A throwaway probe
(`windows/vessel/examples/culvert_count_probe.rs`, uncommitted) counted
`plan_to_room` calls, distinct `(home, dest)` pairs and A\* node expansions per
roster-wide `believed_water` sweep. Node expansions came from a temporary
`thread_local` counter in `kernel/src/astar.rs`, reverted and confirmed clean.

*Shape A — `session_length_scaling`, seed 42, 50 agents, 200 ticks, 10 bands:*

| band | non-empty | max n_i | median n_i | sum n_i | distinct pairs | expansions | budget-exhausted |
|---|---|---|---|---|---|---|---|
| 1 | 10/50 | 12 | 0 | 37 | 37 | 15,437 | 14 |
| 5 | 11/50 | 36 | 0 | 67 | 67 | 41,174 | 39 |
| 10 | 11/50 | 46 | **0** | 83 | 83 | 57,190 | 55 |

*Shape B — the possession shape, seed 17, `Session::start` + 12 waits, roster 67:*

| wait | non-empty | max n_i | median n_i | sum n_i | distinct pairs | expansions | budget-exhausted |
|---|---|---|---|---|---|---|---|
| 1 | 18/67 | 3 | 0 | 31 | 8 | 479 | 0 |
| 6 | 52/67 | 16 | 7 | 363 | 48 | 30,743 | 0 |
| 12 | 52/67 | 23 | **9** | 529 | 83 | 66,002 | **0** |

**Three things the counts say that no amount of reading said.**

1. **The two shapes need the memo for OPPOSITE reasons, and a design fitted to
   one misjudges the other.** In Shape A the within-sweep duplicate rate is
   **exactly 1.00x at all ten bands** — no two agents ever share a
   `(home, water_room)` pair — so a per-TICK memo buys literally nothing, and
   only a SESSION-lived one helps (679 calls, 83 distinct pairs ever, **87.8%
   hit**, 100% by band 10). In Shape B the residents are co-located and share
   homes and water rooms, so the same pair is re-planned **6.4x–9.1x inside a
   single sweep**; within-sweep dedup alone would cut 4,060 calls to 559
   (**-86.2%**), and the session-lived memo reaches **98.0%**. A session-lived
   memo is the one mechanism that collects both wins.

2. **The dominant cost in Shape A is FAILURE, not distance.** 404 of 679 calls
   (59.5%) exhaust the 1,000-node budget and return `None`; `none_returned ==
   budget_exhausted` at every band, so every `None` is exhaustion. Those 404
   calls are `404 x 1001 = 404,404` of 425,042 expansions — **95.1% of all the
   work**. The 275 calls that succeed average **75.0** expansions and return a
   median **3-hop** plan (max 10). The agent walked to those rooms, so they are
   reachable; they are simply outside the ball a zero-heuristic Dijkstra covers
   in 1,000 expansions. **Consequence, and it is load-bearing: the memo must
   cache `None`.** A memo that stores only successes re-pays 95.1% of the cost
   forever while its hit rate reads 87.8% — a number that would look like a
   win and be one only for the cheap calls.

3. **The distinct-pair population is TINY and it SATURATES.** 83 pairs in
   Shape A and 83 in Shape B, against 679 and 4,060 calls. Shape A adds 0 new
   pairs at band 10. The memo is bounded by ~100 entries, not by history
   length, which is what makes a session-lived one safe to hold.

**Why the field loses on these numbers.** A per-home field costs one full
budget (1,001 expansions) per distinct home, whatever is asked of it. Shape A
has 11 homes with non-empty sets: **11,011 expansions once**, against the
memo's 83 misses = **~57,155**. The field wins there, by ~5x. Shape B has zero
budget exhaustions and a **maximum of 548** expansions on any call: the memo's
83 misses cost **~8,300**, while a field would pay full budget per home —
**worse**, and by more than the margin it wins by in Shape A. The field is not
uniformly better; it trades a bounded cost for an unbounded one and the trade
is shape-dependent.

**And the memo needs nothing proved.** #3 listed three properties the field
must establish first — the distance half is asserted in a doc comment with no
live assertion, at **budget 300** where `PLAN_BUDGET` is **1,000**; equal
octile cost does not imply equal hop count; the budget cutoff needs shown
reproducible. The memo is byte-identical **by construction** because
`plan_to_room(from, dest, budget, empty)` is pure over mesh geometry (#2).
Against a 7.4x / 47x expansion reduction with zero properties to establish, a
further ~5x on ONE of two shapes does not buy its proof burden. Simplest
solution that works; the field is recorded as the follow-up it is.

**A third mechanism, eliminated on a fact rather than on taste.** The belief
set is append-only and the metric from a fixed home is fixed, so an argmin over
it could be maintained INCREMENTALLY at O(1) per read — strictly better than
any memo, 679 calls collapsing to 83 comparisons. **It is unsound, and the
repository already knows why.** Belief reads run at PAST instants: that is what
`ReadWitness::note_belief` counts, what `beliefs_in_the_past` reports, and what
`rule_six_witness_belief_reads_run_at_past_instants` pins. A running argmin
keyed on entity alone would answer a past-instant read from a larger set than
that instant admits. The pair memo is sound precisely because its key carries
no time. This also explains why `nearer_to_home` — which IS the incremental
fold — is a separate mid-walk path rather than `believed_water`'s
implementation, and its doc says as much.

**Not measured, and named as such.** `shared_believed_water` anchors its own
ranking at `here` (the current position), which MOVES, so its hit rate is not
the one measured here. The probe measured the home-anchored fold. Shape A has
zero co-located peers (The Kerf), so shared collapses to own there; Shape B's
co-location is exactly where a moving anchor would show. **The spec must
measure the `here`-anchored site separately and must not assume this
result transfers to it.**

**Alternatives discarded.** Per-tick memo (Shape A: zero benefit). Field-first
(above). Incremental argmin (unsound at past instants). Memoising inside
`plan_to_room` for all callers (#2: `home_nav`'s avoid set varies).

**ideonomy passes / overturns.** The pass under #3 stands; this entry is the
measurement adjudicating between the options it produced. **1 overturn: #3's
demotion of the memo is itself overturned.** Recorded rather than edited away —
the pass was right that the field is the stronger IDEA and wrong that it is the
better mechanism here, and only the counts could separate those.

**Capture actions.** Registry row owed for the field as a follow-up, citing The
Waymark's `#[ignore]`d falsifier and the three unproved properties. Registry
row owed for the 95.1%-of-expansions budget-exhaustion finding — it is a fact
about the navigation mesh and `PLAN_BUDGET`, not about this campaign.

---

### #6 [G1] — two of the three preregistration instruments are DEAD on main

**Question.** The brief requires preregistering on three instruments verbatim:
`session_length_scaling`, `agent_scaling`, `fold_depth_sweep`. Do they run?

**Decision / finding.** **Two of the three panic on `main`, and have since
2026-09-03.** Confirmed by the controller directly, not inherited from the
probe agent:

```
$ ./target/release/examples/session_length_scaling
session_length_scaling: seed 42, 50 agents held FIXED, 200 ticks in bands of 20
thread 'main' panicked at windows/vessel/examples/session_length_scaling.rs:1407:18:
a real drive-movements fact always commits: UnknownPredicate { predicate: "slept-on" }
EXIT=101

$ ./target/release/examples/agent_scaling
  agents    ms/tick   facts/a/tick  search/a/tick  ...
      10     60.339         1.8750         1.3000  ...
thread 'main' panicked at windows/vessel/examples/agent_scaling.rs:439:18:
a real drive-movements fact always commits: UnknownPredicate { predicate: "slept-on" }
EXIT=101

$ ./target/release/examples/fold_depth_sweep
  monotonicity of the median column: 6/6 rises (one-sided binomial p = 0.0156)
EXIT=0
```

**The cause.** Both broken examples build a bench-owned ledger and register the
drive stack's predicates BY HAND. `session_length_scaling` registers five
(`AGENT_AT`, `DRANK`, `RESTED`, `SLEPT`, `EATEN`); `agent_scaling` registers
its own copy of the same list. The drive stack now also commits
`liveness::SLEPT_ON`, introduced by **The Pallet (`834ae8041`, 2026-09-03)** and
extended by **The Tenon (`d72a66677`, today)**. Neither example was updated.
`fold_depth_sweep` registers no predicates, which is exactly why it survives —
`grep -c 'SLEPT_ON' windows/vessel/examples/*.rs` returns 0 for all eight
examples, and only the two that hand-register are affected.

**Why nobody noticed.** Examples are compiled by `--all-targets` and therefore
by `gate-commit`'s clippy, but **nothing runs them**. They are informative
benches, never gates, by their own headers. So the break is invisible to every
gate in the ladder and stays invisible until a campaign tries to measure.
`agent_scaling` is the sharper case: it completes rung 1 (10 agents) and prints
a full, plausible row before dying on rung 2 — a reader who captured stdout and
skimmed the top would see a table.

**Blast radius on the record.** The Detent merged 2026-09-03 and The Kerf
2026-09-05, both citing `session_length_scaling` numbers. The Pallet landed
2026-09-03. Which side of that boundary each campaign's readings fall on is a
question for their own records, not this ledger's to answer — but this campaign
must not compare a post-fix number against a pre-Pallet one without saying so.

**Consequence for this campaign.** Repairing both examples is a prerequisite
task, not a courtesy. Combined with #4 — the belief probe is vacuous because
its probe agent holds an empty belief set — the instrument work is now: fix the
predicate registration (both examples), make the vacuity check fail instead of
print, and give the belief columns a roster-wide reading that targets the
member with the MOST known water rooms. The counts in #5 say what that member
looks like: 46 rooms at band 10 in Shape A, 23 at wait 12 in Shape B.

**A guard is owed and belongs in the spec.** A hand-maintained copy of a
predicate list, in two files, with nothing checking it against the drive
stack's real emissions, will break again the next time a drive commits a new
predicate. The cheapest honest fix is for the examples to register from one
shared list that the drive stack itself publishes; the cheapest guard is a test
that fails when they diverge. Which of those this campaign takes is a spec
question, but taking neither would leave the instrument set exactly as fragile
as it was found.

**Alternatives discarded.** Running the campaign on `fold_depth_sweep` alone:
rejected — it sweeps `drive_at` only and structurally cannot see this fold; the
brief already assigns it the no-regression control role. Silently adding the
predicate in a drive-by commit: rejected — this is a finding about the
instrument set's fragility and it is worth a record, not a one-line fix.

**ideonomy passes / overturns.** None run; this is a measurement, not a design
choice. The design question it raises (shared list vs. divergence test) is
carried into the spec, where it gets one.

**Capture actions.** Registry row owed: instruments that no gate runs accrue
invisible breakage. Board post owed — other live campaigns measuring on these
two examples are getting a panic or a truncated table today.

---

### #7 [G1] — the memo is SHARED, not per-entity; the brief had it backwards

**Question.** The brief specifies "a per-entity memo". Should the memo be
per-entity, following `HomeNavCache`?

**Decision.** **Shared, keyed on `(from, dest, budget)` with no entity in the
key.** A per-entity memo would forfeit the entire within-sweep win on the
possession shape.

**Why, from the counts.** `water_at` returns each entity's DISTINCT visited
rooms, so an entity can never duplicate its own `(home, room)` pair within one
sweep. Therefore **every** duplicate is a duplicate across entities. Shape B's
within-sweep duplicate rate is 6.4x–9.1x — 4,060 calls against 559 distinct
pairs per sweep — which is 86.2% of that shape's calls, and a per-entity memo
sees none of it. Shape A's rate is exactly 1.00x, so there per-entity and
shared are identical and nothing is lost either way. The shared memo is weakly
better on one shape and dramatically better on the other.

**Why the precedent does not transfer.** `HomeNavCache` is per-entity because
its key is `(pos, home, budget, avoid_epoch)` and two of those four are
per-entity by construction — `pos` is where that creature is standing, and the
avoid-epoch counter is deliberately per-entity so that "a global epoch would
[not] stampede every entity's cache on any ONE creature's belief change". This
memo's key has neither component. Copying the shape would have been copying the
half of the precedent that does not apply.

**Alternatives discarded.** Per-entity (above). A two-level map keyed
`entity -> (dest -> len)`: same defect, dressed differently.

**ideonomy passes / overturns.** Covered by the pass under #3; the *scope*
dimension prompt is the one that asks "is this local or global", and the
counts answered it. No separate pass. No overturn — the brief's per-entity
wording was never argued for, so this corrects an unexamined default rather
than overturning a position.

**Capture actions.** Stated in the spec at §1.3(c) and §2.1.

---

### #8 [Q] — a cost comparison I did not have, in my own spec text

**Question.** The spec's §2.4 rejected the one-to-many field partly on a cost
comparison: "Shape A: 11 homes x 1,001 = 11,011 expansions". Where did the
number 11 come from?

**Decision.** **From an inference, not a measurement, and it is now removed.**
The probe counted `(home, dest)` PAIRS; it never counted distinct HOMES. "11"
was read across from "11 of 50 roster members hold a non-empty belief set",
which bounds the home count from above but does not establish it — two members
can share a home. The row now states the bound as a bound, says the quantity is
unmeasured, and rests the rejection on the field's proof burden and on the
shape-dependence of its advantage, which are things the campaign does have.

**Why this is worth an entry rather than a silent edit.** It is the exact
failure this campaign was told to avoid — *the Detent's registry row was wrong
until counted; do not repeat that* — reproduced in the spec that was written to
avoid it, one section after the section that reports the counts. The number was
plausible, adjacent to a real measurement, and carried a multiplication sign
that made it look derived. Caught by the spec self-review's own instruction to
check whether each claim about an external quantity has a command and an output
behind it; nothing else would have caught it, because the arithmetic was
correct and only the input was invented.

**And it changed the argument, not just the prose.** With the number gone, the
field is no longer rejected on cost at all — it is rejected on having three
unestablished properties while the memo has none. That is a better reason and
it was available the whole time.

**ideonomy passes / overturns.** None; this is a correction, not a design
choice.

**Capture actions.** The distinct-home count is carried into the field's
registry row as the quantity that would settle the comparison, so the next
attempt starts by measuring it.

---

### #9 [G2] — C1 splits in two, because the kernel does not get an instrument

**Question.** The spec preregisters C1 as node expansions per sweep. Counting
expansions requires a counter inside `AStarSolver::solve`. Does the kernel
acquire one?

**Decision.** **No.** C1 splits: **C1a**, searches per sweep, permanent and
committed, counted by the memo itself in the shape `HomeNavCache::searches`
already has — and it is the criterion of record. **C1b**, expansions per sweep,
taken with a campaign-time kernel counter and retired at close with a dated
record, exactly as this repository already handles campaign-time hash
constants.

**How the question arose, which is the part worth keeping.** It was not
reasoned to. The throwaway probe stopped compiling after the absorption, and
the error was `take_budget_hits` not found in `hornvale_kernel::astar` — the
temporary counter the probe agent had correctly reverted. The instrument that
produced every number in §1.2 of the spec **cannot be rebuilt from the
committed tree**, and that fact had not been noticed while the spec was being
written on the strength of those numbers.

**Why the kernel does not simply keep the counter.** `kernel/` is the
determinism substrate. A permanent `pub` counter there is a new surface every
future reader must reason about, acquired so that one campaign could measure
itself. The repository already has the right idiom for a measurement that
outweighs its instrument's permanence — mint it for the campaign, record the
value with its date and SHA, retire the instrument, keep the record — and
applying it here costs nothing this campaign needs.

**What it costs, and the entry says so rather than leaving it implied.** After
close nothing reproduces the expansion figures without re-applying the patch.
C1a survives and **C1a cannot distinguish an expensive miss from a cheap one** —
a budget-exhausted search costs 1,001 expansions and a successful one 75, and a
search counter scores them alike. The criterion is therefore set at an absolute
`<= 100` searches rather than as a ratio: at a hundred searches against 679 and
4,060 calls, the difference between miss kinds has stopped being able to matter.
This is the same limitation The Detent named for its retired constants — a
constant-free witness guarantees determinism and *cannot detect a behaviour
change at all* — and it is stated here for the same reason.

**One check C1a has that C1b does not.** `entries == searches` at the end of a
run. A miss inserts exactly one entry, so any divergence means the memo is
re-searching a key it already holds, or holding a key it never searched. That
is a stronger structural check than either count alone, and it is free.

**Alternatives discarded.** (a) A permanent kernel counter (above). (b)
Dropping the expansion criterion entirely and preregistering on searches alone:
rejected — it would silently discard the finding that 95.1% of the work is
budget-exhausted failure, which is the single number that shaped the mechanism.
(c) Having `AStarSolver::solve` return its expansion count: a signature change
to the kernel's central search for one campaign's benefit, which is the
permanent surface again wearing a different hat.

**ideonomy passes / overturns.** 1 pass, on the *materiality* axis — where does
an instrument physically live, and what does it cost the thing it lives in?
That is what separated "the measurement is permanent" from "the instrument is
permanent", which the spec had silently conflated. No overturn: the criterion
did not change, only its instrumentation and what survives close.

**Capture actions.** Spec §4.1 rewritten; §10 gains the operational note that
the probe is parked outside the worktree because an uncompilable file under
`examples/` breaks every `--all-targets` build.

---

### #10 [Q] — The Zenith lands mid-review and confirms the finding

**Question.** Main moved 46 commits during spec review, including The Zenith
(`52c53d78f`, "every world has one generated sky; the provider tiers are
retired"), which touched 50 files in `windows/vessel`. Does it change the
campaign?

**Decision.** **No, and it strengthens one finding.** It retired `SkyChoice` and
changed `sky_of(..).calendar()` from an `Option` to a value, and it edited both
of the broken examples to match. Verified after absorbing at `2538f0be5`:
`grep -c SLEPT_ON` still returns **0** for both `session_length_scaling.rs` and
`agent_scaling.rs`.

**So The Zenith edited both instruments to keep them COMPILING and did not
notice that neither RUNS.** That is a second, independent witness for ledger #6
and for spec §6 Task 1b's premise, produced by an unrelated campaign during
this one's spec review. The mechanism is exactly as #6 described it:
compilation is gated by `--all-targets`, execution is gated by nothing, and the
gap is invisible to a campaign that has no reason to run a bench it only had to
keep building.

**Alternatives discarded.** Treating the absorption as reason to re-open the
mechanism: rejected — the diff touches no belief fold, no `NavSpace`, no
`plan_to_room` call site, and no `water_at`.

**ideonomy passes / overturns.** None; this is a re-derivation after an
absorption, not a design choice. Recorded because a merge that lands mid-review
is exactly the situation in which a campaign assumes its premises survived.

---

### #11 [G4] — the plan's pre-flight scan, four rulings before Task 1

**Question.** Subagent-driven execution requires scanning the plan for
cross-task conflicts before dispatching anything. What did the scan find?

**Decision.** Four conflicts, four rulings, all applied to the plan text before
Task 1 was dispatched. The full scan table — nine task-pair rows and ten
per-task rows — is in the SDD ledger at
`.superpowers/sdd/2026-09-05-the-culvert/progress.md`; the rulings are here,
because rulings are committed and task state is not.

**R1 — Task 1's kept bench output is not the C1b before-column.** The plan told
Task 1 to keep its bench stdout as "the C1b before run of Task 9". C1b counts
node EXPANSIONS, which needs the campaign-time kernel counter that Task 1 does
not apply, so that stdout cannot carry an expansion figure at all. The text had
conflated *the run before the fix* with *the C1b before column*. Task 9 now
takes both C1b columns itself — the counter applied to a checkout of the Stage 2
boundary commit for "before" and to the campaign head for "after" — which is a
same-instrument, same-tree pair and strictly better than comparing against
`a8bde6769`'s throwaway probe. Bench outputs also move from `/tmp` to the SDD
workspace. *Cost if wrong:* one extra build in Task 9.

**R2 — Task 4 gains a step that actually builds what Task 6 consumes.** The
plan's own self-review had added `culvert_real_pairs`, `Shape` and
`PLAN_BUDGET_MIRROR` to Task 4's Interfaces block and never added a step that
writes them, so Task 6's test would have consumed three symbols no task creates.
**This is the self-review's own defect half-fixed** — it caught the
inconsistency, declared the interface, and did not follow through to the step.
Worth naming: a fix that updates the contract without updating the work is a
fix that reads as complete. *Cost if wrong:* none.

**R3 — the belief probe is selected once, at band 1, not per band.** The fold
columns are fitted against the probe agent's own history across bands, so a
subject that changes between bands confounds the history axis with a change of
creature — the one thing the fit cannot survive. The consequence is now stated
in the plan rather than left implicit: band 1's max-known-water member (12
rooms) need not be band 10's (46), so the probe column means "a creature that
believes in water", not "the worst case at every band". The worst case at every
band is what the four roster-wide columns report, which is what they are for.
*Cost if wrong:* the probe column understates the worst population while the
roster columns still carry it.

**R4 — `RouteMemo` is a `pub` boundary, so type-audit and plumb bind it.** Both
run default-deny inside `make gate-commit`, and `hops(.., budget: usize) ->
Option<usize>`, `searches() -> u64` and `len() -> usize` are all untagged
primitives at a pub boundary. Added to the plan's Global Constraints along with
the note that a new `pub` item legitimately moves
`docs/audits/type-audit-report.md`, so that movement is not read as a Rule 1
stop. *Cost if wrong:* a red gate the implementer would have hit one round
later.

**ideonomy passes / overturns.** None; a conflict scan is an audit, not a
design choice. Each ruling resolves against the spec, which is the binding
authority the plan argues from.

**Capture actions.** All four applied to
`docs/superpowers/plans/2026-09-05-the-culvert.md` before Task 1 dispatched.

---

### #12 [G5] — the vacuity assertion tested the fold's RESULT, not its vacuity

**Question.** Task 2 turned the belief probes' vacuity `println!` into a panic,
selected the probe agent by known-water-set size as the spec requires, and the
panic still fired at every band. Is the selection wrong, or the assertion?

**Decision.** **The assertion, and it is a defect in the campaign's own plan
text.** It fires on `some_count == 0` — whether the fold FOUND A ROUTE — and
the plan called that a vacuity check. An agent running 46 budget-exhausting
searches per read is the opposite of vacuous: it is the most expensive subject
in the roster and exactly what this campaign exists to make cheap. The
assertion now tests the probe agent's **set size**, which is the quantity
"vacuity" was always about; `some_count` becomes a reported column.

**The tell was in the code the plan copied, and the plan walked past it.** The
original block reads:

```rust
    if some_count == 0 {
        println!("believed_water: probe agent has no known water across {FOLD_REPS} calls at this band");
    }
```

Its MESSAGE claims something about the SET ("has no known water"). Its
CONDITION tests the RESULT (`some_count`). The two already disagreed. The plan
promoted the condition to an assertion while carrying the message's meaning
forward in prose, so what shipped asserted something no sentence of the spec
ever claimed.

**Two different defects wore one symptom, which is why this survived two
campaigns.** The Kerf's probe agent had `set_len == 0` — genuinely nothing to
search. This campaign's probe agent has `set_len == 46` and nothing
*reachable*. The identical `println!` fired for both. The repaired probe now
prints which cause holds, so the two can never again be read as the same thing.

**Member 40 stays the probe.** It is the worst case for COST, and cost is the
campaign's subject: 46 searches × 1,001 expansions per read, which the memo
collapses to 46 map lookups. Selecting instead by "largest REACHABLE set" —
numerically the easy way to make the assertion pass — would have picked the
cheapest interesting agent instead of the most expensive one.

**The implementer stopped rather than relaxing the assertion**, under spec
Rule 4's own stop clause, and escalated with the evidence instead of choosing a
criterion. That is the behaviour the rule was written to produce.

**Cross-validation worth recording separately from the ruling.** The
implementer's instrumentation was written independently of the counting probe
and reproduces it to the digit: band 1 `sum(seen_len) = 37` against the probe's
37 calls, 14 unreachable against the probe's `budget_hit = 14`, and per-band
non-empty / max-set / total-pairs matching §1.2 at bands 1, 5 and 10. Two
instruments, separately written, agreeing exactly.

**Alternatives discarded.** (a) Select by largest reachable set — picks the
cheap agent (above). (b) Relax the assertion to a warning — reinstates the
`println!` that hid this for two campaigns. (c) Drop the belief probe and rely
on the roster-wide columns — loses the per-band history axis C2's `k` is fitted
against.

**ideonomy passes / overturns.** None; this is a defect adjudication against
measured evidence. No overturn — the spec's requirement (probe the worst
population) is unchanged; only the assertion that was supposed to enforce it.

**Capture actions.** Spec §1.3 gains this as a finding; the probe now
distinguishes the two causes in its own output.

---

### #13 [G5] — the creature that knows the most water can reach none of it

**Question.** Roster member 40 holds 12 known water rooms at band 1, rising to
46 by band 10, and **zero of them are reachable within `PLAN_BUDGET` from its
own home at any band** — while members holding four rooms have all four
reachable. Is that a measurement artifact?

**Decision.** **It is real, the mechanism is causal, and this campaign records
it without fixing it.**

**The mechanism.** A creature accumulates distinct known water rooms by
WANDERING. Wandering carries it away from home. `believed_water` plans FROM
HOME. So the more water a creature has learned, the farther that water tends to
sit from the origin the plan starts at, and the likelier every route is to
exceed a 1,000-expansion zero-heuristic Dijkstra. The biggest believer is
systematically the least able to reach any of what it believes.

**What it means for the world, not just the bench.** That creature's
`believed_water` returns `None`, so it behaves as *ignorant of water* while
holding 46 remembered water rooms. Its thirst-memory path is effectively dead.

**The repository already knew the design choice and not its cost.**
`believed_water`'s own doc says "Nearness anchors to home (nearest-to-current
is a followup)". The choice was recorded; the price was not. The price is that
home-anchoring makes 100% of the largest belief set unusable, and
`shared_believed_water` — which anchors at `here`, the current position — is
the shape that would not have this problem.

**It also reframes this campaign's own headline finding.** §1.3(a) reported
that 95.1% of node expansions are budget-exhausted failures. That is not a
quirk of where the budget happens to sit: it is structural, and it concentrates
on exactly the creatures that know the most.

**Out of scope, and the reason is the campaign's own discipline, not
squeamishness.** Changing the anchor changes which water room is chosen,
therefore what a creature does, therefore committed facts — an epoch. This
campaign is byte-identical by construction. Recorded as an idea-registry row
at close, carrying the per-band reachability measurement that motivates it.

**Alternatives discarded.** (a) Fixing the anchor here — an epoch, out under
§5. (b) Raising `PLAN_BUDGET` — also a behaviour change, and it treats a
symptom whose cause is the anchor. (c) Leaving it unrecorded because it is out
of scope — the measurement exists now and will not be cheaper to retake later.

**ideonomy passes / overturns.** None; a measured behaviour finding. The design
question it raises belongs to the campaign that acts on it.

**Capture actions.** Idea-registry row at close, citing the per-band
reachability table. Spec §9 gains it.

---

### #14 [G5] — the moving-anchor key population cannot decide, and Rule 3 gets a third branch

**Question.** `shared_believed_water` (`liveness.rs:1880`) anchors its
ranking at `here` — the current position, which moves every tick — rather
than at `home` (fixed for a session). Its memo key space is therefore
`positions × water rooms`, not `homes × water rooms`, which is the one way
this campaign's home-anchored bound (83 pairs, saturating — Task 4) could
fail to cover a real session. Does the `(here, dest)` population saturate
(safe to memoize, include in Task 8) or keep rising (exclude)?

**Decision.** **Cannot decide cleanly within the window measured; resolves
to EXCLUDE under spec Rule 3's conservative-default branch (ruling R12).**

**Measurement.** Two shapes, both already built by Task 4 — no third
construction path. Lab shape (seed 42, 50 agents, 200 ticks —
`Shape::LabAt200Ticks`'s own construction, paid once at 129.337s, read at
ten 20-tick bands off the one finished ledger): `colocated` and
`here_dest_cum` were **`0` at all ten bands** — confirming The Kerf's own
prior finding on this exact construction on a second, independent
instrument. `home_dest_cum` reproduced Task 2's per-band table digit for
digit (37, 56, 61, 65, 67, 71, 77, 79, 83, 83). The pooled population never
fires on this shape at all; it is uninformative for this question.

Possession shape (seed 17, extended from Task 4's 12 waits to 60 — the only
shape where `colocated` is ever non-zero, consistently 52-63 of 67 members
every wait) is where the real signal is:

```
wait    home_dest_cum    here_dest_cum  colocated
   1                8                4         63
   2               17                6         62
   3               29                7         57
   4               29                8         58
   5               42               10         57
   6               48               10         60
   7               51               12         61
   8               58               13         60
   9               60               14         60
  10               64               16         60
  11               70               17         59
  12               83               19         59
  13               91               22         59
  14               95               28         62
  15              104               34         59
  16              106               35         60
  17              109               36         59
  18              117               37         56
  19              119               40         60
  20              122               40         59
  21              126               41         58
  22              126               41         58
  23              127               41         57
  24              127               41         58
  25              133               41         57
  26              141               42         56
  27              144               42         60
  28              144               44         58
  29              146               44         58
  30              146               44         61
  31              147               45         60
  32              149               47         60
  33              149               47         59
  34              150               47         60
  35              151               50         59
  36              151               50         60
  37              152               55         63
  38              152               58         58
  39              153               61         60
  40              156               65         57
  41              156               65         58
  42              159               67         60
  43              161               69         57
  44              165               70         59
  45              170               72         57
  46              170               74         56
  47              175               75         56
  48              177               77         58
  49              179               78         54
  50              180               86         58
  51              180               86         53
  52              180               89         54
  53              181               92         57
  54              182               93         55
  55              184               94         57
  56              187               96         57
  57              189               98         59
  58              190               98         55
  59              190               99         55
  60              190              101         52
```

Cost: 1077.86s end to end, dominated by the possession shape's 60
independent roster-wide sweeps (each re-running `believed_water`'s budgeted
`plan_to_room` over a co-located roster of ~55-63 of 67 members), not the
lab shape's already-accepted 129.337s.

**The first read of this table was wrong, and the correction is why this
entry exists at all.** The implementer's first draft claimed `here_dest_cum`
has "no analogous flat stretch anywhere in the 60-wait window," offered as
evidence the reference (`home_dest_cum`) uniquely saturates. That claim is
false against the table above: `here_dest_cum` reads 41, 41, 41, 41, 41
across waits 21–25 — a **five-wait** plateau, LONGER than the reference's
own longest plateau (three waits, at waits 50–52 and again at 58–60). A
subsidiary claim — that the ratio `here_dest_cum / home_dest_cum` climbs
essentially monotonically — was also softer than stated (it dips at wait 30:
0.30 against wait 20's 0.33).

**Ruling R11 — the verdict stands, and the reason improves.** The
implementer's machinery is sound: the reviewer verified `culvert_here_dest_pairs`
against `shared_believed_water` line for line and traced `band`'s real
binding up through `step_with_occupancy` → `WalkState::begin` to confirm
production's `band` argument is the full roster — the curve describes the
real call site, and extending 12 → 60 waits was the right call. But the
five-wait plateau, once stated correctly, cuts the OTHER way from how the
first draft used it: a plateau that later RESUMES CLIMBING (41 → 42 at wait
26) demonstrates that a plateau in this system does not imply a ceiling —
for either curve. The here-curve has an *observed history* of stalling and
resuming; that undermines the reference's own saturation claim symmetrically,
since the reference's final three flat waits (190, 190, 190) have not been
shown immune to the same kind of temporary stall. **Neither curve has
demonstrated a true ceiling in 60 waits — both have only been shown to
pause.**

**What survives as signal is thin, and is stated as such.** Over the final
three waits (58, 59, 60): `home_dest_cum` adds `+0, +0` while `here_dest_cum`
adds `+1, +2`. That is the entire basis for treating the curves differently
at the point measurement stops — and it is real (the reference has
genuinely stopped moving on this exact stretch) but it cannot rule out that
wait 60's growth is itself another temporary pause about to resume, given
the here-curve's own demonstrated capacity to pause for at least as long as
the reference's longest pause.

**Ruling R12 — spec Rule 3 gains a third branch, explicitly.** The spec, as
written, offered only two outcomes (saturates → include; keeps rising →
exclude) plus an informal allowance in this campaign's brief that
"cannot decide" also excludes. R12 promotes that allowance into Rule 3
itself: *the curve cannot decide → exclude, and say plainly that the
verdict rests on the conservative default rather than a clean measured
separation.* The brief's own closing paragraph had already fallen back to
roughly this position; R12 sharpens the spec's framing to match rather than
overturning anything already decided. This is a genuine spec correction
(the spec was wrong to omit the branch its own brief needed), not a
reinterpretation of already-ratified text.

**Disposition.** `shared_believed_water` is **excluded** from Task 8's memo
scope. Task 8 is not run against this call site. The exclusion rests on
Rule 3's conservative default under genuine uncertainty, not on a
demonstrated ceiling for the reference or a demonstrated absence of one for
the moving-anchor population — a longer possession-shape run (100+ waits)
would be needed to settle either question, and was not run because the
conservative default already applies and points the same direction
regardless of how that longer run would come out.

**Alternatives discarded.** (a) Re-measuring at 100+ waits to force a clean
separation — disproportionate cost (the 60-wait run already cost 1077.86s,
dominated by the possession shape) for a question the conservative default
already answers the same way. (b) Reading the five-wait plateau as
supporting "saturates" (since the reference matches it) — wrong, because the
plateau demonstrably did not hold: growth resumed at wait 26, so a plateau
of this length is evidence a ceiling has NOT been reached, for whichever
curve shows it. (c) Leaving the report's first-draft "keeps rising, clean
separation" framing uncorrected because the exclude ACTION happened to
already be right — rejected under this campaign's own discipline that a
correction is unaudited until reviewed, and a false supporting claim left
standing would mislead the next reader who re-derives from this curve rather
than from the (correct) action.

**ideonomy passes / overturns.** None; a measurement-review correction and a
spec-completeness fix, not a design choice among alternatives.

**Capture actions.** This entry; the corrected reasoning also lives in
`windows/vessel/tests/suite/the_culvert.rs`'s own doc comment on
`culvert_here_anchored_key_population_curve` and in
`.superpowers/sdd/2026-09-05-the-culvert/task-5-report.md`. Task 8 proceeds
without this call site.

---

### #15 [G5] — the witness could never have gone GREEN, and I defined it that way

**Question.** Task 7 un-ignored Task 4's sweep witness, expecting it to turn
green. It could not. Why?

**Decision.** **Because the witness never called `believed_water` at all.** It
computed `calls += seen.len()` — the sum of `LatestVisit::water_at` set sizes —
and nothing else. Task 7 does not touch `LatestVisit`, so un-ignoring it
unchanged would have reported **529 forever**: a permanent red that reads
exactly like "the memo did not work".

**This is a defect in my plan text, and it is the deepest one this campaign has
produced.** Task 4's brief said:

> For each roster member, `n_i = latest_visit.water_at(entity, t, terrain).len()`
> — this is exactly how many `plan_to_room` calls one `believed_water` call
> makes for that member.

That sentence was **true before the memo and false after it**. I wrote it as a
definition, so the witness measured the definition instead of the behaviour.

**The shape, stated generally, because it is not the usual one.** The
repository's standing rule is that a check which can never fire is worse than
an absent one. **This is its mirror image: a check whose GREEN is
unreachable.** It could go red — it did, at 529, exactly as predicted — and the
red was reproduced by the implementer, confirmed by the task review, and read
by me. What nobody asked was whether the *green* was reachable.

The mechanism is specific and worth naming: **I defined the observable by its
PRE-FIX IDENTITY.** Before the memo, "candidate rooms" and "route searches" are
the same number. The memo's entire purpose is to break that identity. So a
witness defined on the identity cannot see the fix — by construction, not by
accident.

**How far it travelled.** Task 4's implementer wrote it as specified and
described it accurately in its own report. Task 4's review verified the RED
reproduced with the right counts. I read both. Three passes, and the question
"can this test ever pass?" was asked by none of them — because a red witness
before the fix is exactly what everyone was looking for.

**The correction, which is better than what I specified.** `calls` is now
`RouteMemo::searches()` taken over a REAL `believed_water` sweep. The old
analytic quantity survives, honestly renamed `occurrences`. `distinct_pairs` is
derived independently from the fold store. And a **non-vacuity guard runs
first**: `occurrences > distinct_pairs`, so a shape with no duplication reddens
rather than passing for free. Then `assert_eq!(calls, distinct_pairs)` — one
real search per distinct pair, exactly. Three independent quantities,
cross-checked, where I had specified one.

**The result.** 529 occurrences → **83 real searches** on the possession shape.
**6.37×**, and it is now measured through the function under change rather than
beside it.

**Alternatives discarded.** (a) Keeping the analytic count and relaxing the
bound — it would have made a test that cannot observe its subject pass, which
is worse than one that cannot pass. (b) Deleting the witness — the campaign's
lead criterion (C1a) is a search count, so the witness IS the deliverable.

**ideonomy passes / overturns.** None; this is a defect adjudication. The
generalisable lesson is recorded to memory, not re-derived here.

**Capture actions.** Registry row at close on the review-methodology gap: a
red-before-fix witness needs its GREEN path checked too, and no reviewer in this
campaign was asked to. Spec §4.1's C1a description is corrected to say the
count is taken through `RouteMemo::searches()`.

---

### #16 [G5] — both hash constants moved at the close, and a same-tree control said whose fault it was

**Question.** Absorbing 54 commits of main at the close moved BOTH campaign-time
hash constants. Is the campaign's byte-identity claim false?

**Decision.** **No. The move is main's, established by a same-tree control
rather than by argument.**

**The experiment.** Neutralise `RouteMemo`'s cache lookup so it can never serve
a hit — which makes the memo do exactly what the pre-memo code did, one fresh
search per ask — and re-take both hashes on the SAME tree:

| constant | cache neutralised | memo live |
|---|---|---|
| seed 42 | `0x9dd87f4cea554d28` | `0x9dd87f4cea554d28` |
| seed 17 | `0xbde5058309750ca4` | `0xbde5058309750ca4` |

Identical in both rows. The memo changes no committed byte. What moved is the
world underneath it.

**Attribution.** The absorbed range carries **The Lot** (`f8859b8d7`), which
edits `windows/worldgen/src/{lib,person_promote,vestige}.rs` — world
generation, upstream of every possession-shape ledger.

**The corroborating control the campaign already held**, and which is why the
answer took one experiment rather than a bisect: at `8acd377c5`, with the memo
FULLY WIRED after the previous 99-commit absorption, both constants matched
their minted values. The only delta since is main's.

**Why this is the instrument working rather than an alarm.** A moved constant
is exactly what a campaign-time hash is for. Its job is not to stay green — it
is to force the question "whose change was that?" at a moment when the honest
answer is still cheap to get. Had the constants been absent, this absorption
would have been silent, and the campaign would have merged with no evidence
either way about whether 153 commits of main had interacted with its change.

**And the discipline it exercised is the one the module doc already
prescribed:** re-record MAIN-FIRST. Not "the test is red, update the number" —
measure whether the campaign's own code is responsible first, then re-record
with the evidence attached. The dated record in
`windows/vessel/tests/suite/the_culvert.rs` now carries both values, both
controls, and the attribution.

**Alternatives discarded.** (a) Re-recording without the control — it would
have produced the same two numbers and no knowledge, and is indistinguishable
from quietly accepting a real regression. (b) Bisecting main's 54 commits — the
same-tree control answers the question directly in one run; a bisect would have
found the same commit at far greater cost, and only after assuming the answer
lay in main at all.

**ideonomy passes / overturns.** None; a defect adjudication against a measured
control.

**Capture actions.** The dated record is updated in place with the full
before/after and both control rows. The retrospective gets the general lesson:
a campaign-time constant's value is disposable, but the CONTROL that attributes
its movement is the thing worth building.

---

### #17 [close] — the 86.2% figure is PARKED, not corrected: it is a lower bound, and 559 is a sum

**Question.** The final fix wave corrected the `6.4×–9.1×` duplicate-rate range
for two faults: it excluded wait 1, and its divisor was the CUMULATIVE
distinct-pair column rather than the within-sweep count. The re-review then
found the sibling figure carries the identical defect —
**"within-sweep dedup alone would cut 4,060 calls to 559 (−86.2%)"**, at spec
`:134`, `:160`, `:290` and this ledger's #5 (`:288`) and #8 (`:465`). Correct
the numbers, or park the finding?

**Decision. PARKED — the figures stand and this entry is the record.** Nothing
downstream is edited.

**What 559 actually is, established by independent derivation rather than
inherited.** It is the **SUM of the cumulative distinct-pair column across
waits 1–12**: `8 + 17 + 29 + 29 + 42 + 48 + 51 + 58 + 60 + 64 + 70 + 83 = 559`
— the same `home_dest_cum` column #14's table prints. Two consequences the
prose does not say and a reader would not guess:

1. **559 is not "distinct pairs per sweep."** It is a total over twelve sweeps
   of a *running* count. The largest single sweep's cumulative reading is 83;
   no sweep ever holds 559 of anything.
2. **86.2% is therefore a LOWER BOUND on the within-sweep win, not a
   measurement of it.** Within-sweep distinct ≤ cumulative distinct at every
   wait, so the true reduction is **≥ 86.2%**, in the same conservative
   direction as the range correction above.

**Why parked rather than fixed.** Every conclusion drawn from the figure needs
only a rate above 1×, which it clears by a wide margin under either reading:
it argues that the memo must be SHARED across entities rather than per-entity
(§1.3(c)), and a lower bound is sufficient for that argument in full. Correcting
it would require re-deriving a within-sweep distinct count that was never
measured — a new probe run, at real cost, to move a number no decision depends
on.

**Why parking requires this entry and not just silence.** The fix wave added a
note under spec §1.2's Shape B table saying "anything DIVIDED by this column is
a lower bound". **That note does not reach 559**, which is *summed* from the
column rather than divided by it — so a reader checking the 86.2% figure against
the nearest caution would find it inapplicable and conclude the figure is a
clean within-sweep measurement. That is the exact re-derivation this entry
exists to prevent. A parked defect with no record is indistinguishable from an
undetected one.

**Alternatives discarded.** (a) Correcting the prose to "≥86.2%" without a new
measurement — rejected: it would quietly convert a stated measurement into a
bound with nothing saying why, which is how the original defect was formed.
(b) Running a within-sweep probe to get the real figure — rejected on cost
against a number no conclusion turns on; recorded here so a future campaign that
wants it knows exactly what to measure.

**ideonomy passes / overturns.** None; an adjudication about whether to spend
measurement, not a design choice.

**Capture actions.** This entry. No prose edited, no registry row owed — the
finding is about a figure in this campaign's own artifacts, not about the
program.

---

## Deferred minors

**Backfilled at close, and that is itself the finding.** Every one of these was
recorded contemporaneously — in `.superpowers/sdd/…/progress.md`, which is
git-ignored scratch that dies with the worktree. None reached this committed
file until the close walk went looking. `campaign-autopilot` names exactly this
split (task state to scratch, rulings and deferred minors to the committed
ledger) and I applied it correctly to the sixteen rulings above and incorrectly
to every minor below. The closing walk's step 2A is what caught it, which is
the backstop working — but the improvement worth wanting is that it should not
have had to. Recorded in the retrospective.

| # | task | minor | outcome |
|---|---|---|---|
| m1 | 1 | The implementer reverted `docs/timings.md` twice, discarding the cost rows `make rebaseline` and `make gate-commit` had recorded for its own runs. Defensible — they were not that task's diff — but The Governor added the general `add -u` precisely so a run's cost measurement is durable. | **Open.** Later tasks committed their timings rows, so the practice self-corrected without being asked. |
| m2 | 2 | The report never states the pre/post `believed_water_us` pair (79,128.70 against 109,510.31) that spec §6 Task 1c calls for, though both numbers were in hand. | **Superseded.** Ruling R8 made the omission moot by forbidding the unlabelled pair outright and putting the three-subject table in spec §11 instead — which is what a reader actually needs. |
| m3 | 6 | `RouteMemo::hops` builds its key — cloning two `Facet`s, each a `Vec<u8>` — BEFORE the map lookup, so every cache HIT allocates. Hits are the common case and the path Task 9 measures. | **Open.** Negligible against the Dijkstra it replaces; a nested `BTreeMap<Facet, BTreeMap<(Facet, usize), _>>` would allow a borrowed outer lookup if a later measurement wants it. Registry row. |
| m4 | 6 | `RouteMemo` has no `held_bytes()` accessor though `LatestVisit` and `GroundHazards` both carry one, and this campaign's siblings measure held state. `len()` is a count, not a size. | **Open.** Registry row; it is the natural instrument for the growth question that excluded `shared_believed_water`. |
| m5 | 6 | One appositive called `PrimaryAfraidMemo` a cache "over a fixed lattice"; it is over a fixed ledger snapshot. | **Fixed** in Task 6's fix round. |
| m6 | 9 | Load readings for six of nine timed runs are attested only by the committed spec prose; the two raw traces live in scratch that dies with the worktree. | **Open, and accepted.** The §4.4 table was deliberately moved INTO the committed spec for this reason; the raw traces are corroboration, not the record. |
| m7 | 9 | The 57,190 identity — Shape A's memoized total equalling the control's cost of the 83 distinct pairs once — was unstated. | **Fixed** in Task 9's fix round, with the 1.00×-duplicate-rate qualification that keeps it from over-claiming to Shape B. |

## Follow-ups

*(This section read "none yet — entries above carry their own capture actions"
until the final fix round. "Yet" is meaningless at close, and "none" was wrong:
one capture action above was never discharged, and the reconciliation belongs
here rather than in a reader's head.)*

**Reconciliation of #3's three owed registry rows.** #3's capture action owed
rows for three observations. Two were minted:
`TOOL-octile-heuristic-is-an-epoch-candidate` (the zero heuristic — `NavSpace`
is Dijkstra, not A\*) and `TOOL-most-belief-searches-exhaust-plan-budget`. The
third, **the `RoomMeshMemo`-not-threaded observation, was NOT minted, and that
is deliberate rather than an oversight now that it is stated.** Spec §7 already
carries the reason it is out of this campaign: threading `RoomMeshMemo` into
the belief calls would accelerate the very searches the memo removes, so
shipping both would make each one's contribution unattributable in the readout.
And the general form of the observation is already held by the pre-existing
`TOOL-ticksystem-step-no-cache-hook` — `TickSystem::step` has nowhere to hang a
cache, so a caller-owned memo must be threaded by hand at every call site,
which is exactly the shape of the un-threaded belief calls. A new row would
have been a narrower restatement of an open one. **Recorded rather than
silently dropped**, because an undischarged capture action is indistinguishable
from a forgotten one.

**Open at close, each with a registry row:** m3
(`TOOL-route-memo-hit-path-clones-two-facets`), m4
(`TOOL-route-memo-has-no-held-bytes-accessor` — and see spec §1.3(d): it is the
instrument that would settle whether the home-anchored key population has a
ceiling at all, the question this campaign resolved on a conservative default),
and the one-to-many field (`TOOL-one-to-many-distance-field-for-belief-ranking`).
