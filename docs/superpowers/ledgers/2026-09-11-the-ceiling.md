# The Ceiling — decision ledger

**Campaign:** The Ceiling (rung 3 of the Underworld Larder metaplan).
**Branch:** `campaign/the-ceiling`. **Base:** `26003913d`.
**Started:** 2026-09-11.

Autopilot is engaged. Entries are written as rulings occur, per
`campaign-autopilot` and decision 0486 (The Cartulary).

---

## #1 [Q] — What is this campaign, given The Tidemark holds the marine half?

**Backfilled.** Recorded after the fact; the ruling was made before this
ledger existed.

**Question.** Nathan asked to pick up "The Tenant" — the campaign the
`BIO-chemotrophy` registry row names as owning rung 4 ("no `Surface`-realm
kind weights it yet, so a vent's own supply reaches no consumer — THE
TENANT's job, rung 4"). Investigation found that description is no longer
available:

- A campaign named **The Tenant did run**, merging `7b0438986` on
  2026-08-30. Its entire content is decision 0516 (a reachable lock implies
  a reachable key is left empirical), a ten-line doc comment in
  `windows/vessel/src/interior/pattern.rs`, and a timings row. Chamber locks
  and keys, following The Chattel. **Unrelated to the larder.** It left no
  spec, plan, ledger, chronicle, retrospective, or row in
  `docs/audits/campaign-reconciliation.tsv`.
- **`campaign/the-tidemark` is live** (worktree `.claude/worktrees/the-shoal`,
  spec stage, last commit 2026-09-11) and has **already absorbed the marine
  half of rung 4**, deliberately and with a documented boundary. Its spec §7:
  "The underworld's trophic half stays THE TENANT's. This campaign takes the
  *marine* half of the larder metaplan's rung 4 only: the vent commensal
  weights `CHEMOSYNTHATE` and consumes `marine_chemosynthate_supply_field`."
  Its ledger #5 records the same forensics on The Tenant, reached
  independently.

So the registry sentence Nathan was pointing at describes work now in
flight in another campaign.

**Decision (Nathan's, surfaced as a hard stop).** This campaign is **rung 3,
THE CEILING** — the derived underworld carrying capacity. Rung 4 remains
open for a successor, minus the marine half.

**Why this rather than rung 4.** Rung 4's own metaplan entry says "Needs
1-3." Rungs 1-2 shipped (The Gossan, The Sources). **Rung 3 has never run** —
no spec, no branch, no chronicle. Building the tenant on an underived ceiling
would invert the program's own dependency order.

**Why it was surfaced rather than auto-resolved.** No precedent existed in
any link of the autopilot chain — the metaplan records rung 3's design
question as literally "Nathan has not ruled" — and the candidate answers
diverge materially in what gets built. Both conditions for escalation held.

**Alternatives discarded.**
- *Rung 4 now, collapsing rung 3 into it* — larger, and it needs naming,
  which The Winze explicitly refuses (§4.6/§7: `thaumic` stays 0.0, nothing
  named).
- *Re-cut the metaplan first as its own short campaign* — real work (see
  Follow-ups), but ceremony ahead of the measurement that would inform it.
- *Hold until The Tidemark lands* — its `HabitatRealm::Marine` forces open
  all three realm-match sites and its M1 may reclassify the vent
  representations, but neither reaches rung 3's derivation. Not a blocker.

**Ideonomy passes / overturns:** none run for this entry. It is a scope
question settled by the metaplan's own dependency order plus a ruling from
Nathan, not a design space. The design passes belong to #3 onward.

**Capture actions:** board `notice` `2797ec8f8eab` (the boundary, the C.3
ruling, and the name collision); wire message to the live Tidemark session
accepting its boundary as written; Follow-ups below.

---

## #2 [Q] — C.3: should the underworld's budget be symmetric?

**Backfilled.** Same sitting as #1.

**Question.** The Winze's amendment C.3 argues the bound on a spreading
horror should be **symmetric** with the bound on a spreading ecology — "both
eat the same rock. One budget with two kinds of consumer is a mechanism; a
special-case cap on monsters is a knob wearing a mechanism's clothes." The
metaplan carries this into rung 3 as its open design question and records
"**Nathan has not ruled**." It is a fidelity/world-type tradeoff, which
`campaign-autopilot` makes an unconditional hard stop.

**Decision (Nathan's).** **Symmetric budget, asymmetric allocation.** One
budget with two kinds of consumer — C.3's mechanism claim stands — but
**composition decides the split, not magnitude**.

**Why, and why this is a third option C.3 did not offer.** C.3 states its own
cost as: "barren and deadly" and "lush and safe" both become unreachable
world-types, because rich rock feeds both a rich ecology and a far-ranging
horror. That amendment is dated 2026-08-24. **The Sources then measured
something that undercuts the premise** — and the metaplan itself carries the
finding, in rung 3's own inherited-diagnosis paragraph:

> do not design this rung against lithology carrying the variety budget. Rock
> chemistry underground was measured as roughly **three near-constant
> categorical states** … variety survives in *composition* — which mechanism
> dominates, not how much arrives — and per-world **presence** of an axis at
> all.

If variety lives in composition rather than magnitude, then a symmetric
*budget* whose *allocation* is composition-dependent can still reach both
extreme world-types: low total with a composition favouring the horror is
"barren and deadly"; high total with a composition favouring ecology is "lush
and safe". C.3's stated cost would dissolve without weakening its mechanism
claim.

**This is a hypothesis, not a result, and the distinction is load-bearing.**
Nothing has measured whether composition carries enough independent signal to
separate those world-types. Establishing it is this campaign's central
preregistered measurement, and a null is a publishable finding that sends the
ruling back to C.3's binary (decision 0016; the null-as-headline precedent is
well established here).

**Alternatives discarded.**
- *Symmetric, accepting C.3's stated cost* — the amendment as written. Loses
  two world-types Nathan has named as wanted ("Carpenter's Apocalypse
  Trilogy … three world-ending scenarios that differ from each other").
- *Asymmetric* — keeps the world-types, and earns C.3's own objection: the
  cap's value is identical in every world by construction, which is authoring
  rather than simulating.
- *Not ruling yet* — would have forced the scope answer in #1 toward
  re-cutting the metaplan or holding.

**Ideonomy passes / overturns:** one pass, informal, run at question-framing
time — implication-mining C.3's stated cost against the finding that
postdates it. It produced the third option, which Nathan adopted, so this
entry records **one overturn of the amendment's own binary framing**. A
formal multi-operation pass on the *derivation* — what quantity the budget
is, how allocation reads composition — has **not** been run and is #3's job,
before any approach is proposed.

**Capture actions:** board `notice` `2797ec8f8eab`; wire message to the
Tidemark session, whose ledger flagged this as "not this campaign's call to
make unilaterally"; the hypothesis is carried forward as this campaign's
preregistration and must be frozen in the spec before the code that would
move it.

---

## Follow-ups

- **The metaplan's rung 4 needs re-cutting, and nobody owns it.** Rung 4 is
  now split: The Tidemark takes the marine half, a successor takes the
  underworld half. The metaplan (`docs/superpowers/specs/2026-08-24-the-underworld-larder-metaplan.md`)
  still describes it as one rung owned by "THE TENANT". The Tidemark's own
  ledger flagged this and declined to act — "worth separating when THE TENANT
  is scheduled; not this campaign's call to make unilaterally." Scheduling
  this campaign is that moment, but the edit belongs with the rung-4
  successor's spec, not here; recorded so it is not lost a third time.
- **The name "The Tenant" is now ambiguous and should be retired or
  qualified.** It denotes (a) the 2026-08-30 lock-and-key landing that minted
  0516, (b) the metaplan's rung 4, and (c) The Winze C.2's ALIVE position ("a
  tenant that eats, spreads, and can be killed, fled, or bargained with"),
  which is where the rung took its name. Two sessions have now independently
  burned effort disambiguating it. The rung-4 successor should take a fresh
  name.
- **A decision-only campaign leaves no reconciliation row.** The Tenant is
  invisible to the coverage test in `docs/audits/campaign-reconciliation.tsv`
  because that test keys on spec-and-plan campaigns. Not this campaign's to
  fix; it is the mechanical reason "did The Tenant close?" could not be
  answered by looking where one would look. (Independently observed by The
  Tidemark's ledger — two campaigns reaching the same finding is itself
  evidence it is worth fixing.)
- **`marine_chemosynthate_supply_field`'s doc reserves rung 4 in the present
  tense** and names THE TENANT. It becomes stale the moment The Tidemark
  lands. Theirs to update, flagged to them on the wire; noted here in case it
  survives their sweep.

---

## #3 [G1] — Ideonomy pass on the rung-3 derivation

**Tuple** (`ideonomy-plain`, one pass): operators *cross-domain
re-instantiation* + *organon-construction*; organon *list*; dimension-prompts
*side-effect*, *materiality*, *modularity*.

**What it produced.** Four results, two of them load-bearing.

1. **The side-effect is the main effect.** A single budget with two consumer
   classes does not merely bound them, it makes them **compete**. Promoting
   that to the main effect yields four world-types on two axes (budget
   magnitude × allocation winner), including both C.3 thought it was giving
   up. C.3 concluded they were lost because it assumed allocation tracks
   magnitude; #2's ruling severs that, and **the severing is what buys the
   quadrants back**. This is a sharper statement of the ruling's mechanism
   than #2 records, and it supersedes #2's looser phrasing without changing
   the ruling.

2. **Materiality: the budget is INTENSIVE and rung 4 needs an EXTENSIVE
   one.** `subterranean_energy` is a mean of seven `[0,1]` terms — verified
   at `energy.rs:449`, and its doc records the mean was chosen empirically
   because a clamped sum pinned every rung's median to exactly 1.0. A
   per-vertex ratio cannot be eaten, depleted, or spread through, and rung
   4's brief is literally "something that eats the budget and spreads."
   **Nothing in the current quantity supports "eats."** This is the
   campaign's central design fork and it was not visible before the pass.

3. **Cross-domain: fire ecology is the same structure, solved.** Fuel load is
   the budget, fuel composition decides regime, and the domain's artifact —
   the fire regime, classified on frequency × intensity and derived rather
   than authored — is what this rung is trying to build. Its substantive
   import: a rich system can be *safer* than a poor one, because frequent
   small draws prevent accumulation. Immunology's colonization resistance
   supplies a second: the incumbent may exclude by **diversity** rather than
   by weight. Both are candidate mechanisms, neither is adopted here.

4. **Modularity: the seam wants to be one 7-vector read twice.**
   `subterranean_energy` is monolithic (mean of 7); `dominant_source` is the
   first crack in it. Fully-modular is budget = f(vector) and allocation =
   g(vector), with neither collapsing the vector prematurely.

**A measurement risk to #2's ruling, surfaced by the pass and recorded
because it is the kind of thing a campaign talks itself out of.** A mean of
seven gated terms is itself a compression machine. The Sources attributed
magnitude-compression to the rock (~three near-constant categorical states).
**Those are two different causes of one observation and nothing has separated
them.** If compression is largely the combination rule, composition is more
available than The Sources implied; if it is the rock, composition may be too
coarse to separate world-types and #2's null is live. Separating them is a
prerequisite measurement, not an optional one, and it belongs in the
preregistration.

**Overturns:** one — the pass overturned this session's own framing of #2's
mechanism (composition-as-variety → competition-under-one-budget). #2's
ruling stands; its stated reason is now more precise.

**Capture actions:** approaches to Nathan before any spec text; the
intensive/extensive fork and the two-causes-of-compression risk both lead the
approach comparison; fire-regime and colonization-resistance recorded above
as candidate mechanisms rather than adopted ones.

---

## #4 [G2] — The spec, and the two defects its self-review caught

**Decision.** Spec written to
`docs/superpowers/specs/2026-09-11-the-ceiling-design.md` and proceeding to
G3 without a section-by-section approval round, per autopilot's G2 policy.
Decision 0966 minted first, from the reserved block `0966-0975`
(`make decision-block NAME=the-ceiling`, allocated on lefford) rather than by
picking a number locally — the failure mode blocks exist to remove.

**The self-review's verification pass, run at drafting time rather than review
time**, per autopilot's fold-in rule. Two claims were checked by command, and
one of them mattered:

- **`separation = 0.145249` REPRODUCES EXACTLY at `26003913d`.** The spec's M2
  positive control requires reproducing a number measured 2026-08-26. A
  committed baseline is a claim with a date, and had the world moved under it
  the spec would have shipped a control guaranteed to fail. It did not move;
  every S1 width, the S2 occupancy table and the realized max `0.424277` all
  reproduced too.
- **The dominant-source histogram re-run at base**, quoted in both 0966 and
  the spec rather than inherited from The Sources' prose.

**DEFECT 1, caught and fixed: M1 pooled the rungs.** The first draft computed
one histogram per seed over all underground rungs. The re-run printed the
per-rung S3 histograms, which show composition driven hard by DEPTH —
`IronReduction` leads shallow, `SulphideOxidation` deep, and two sources are
structurally absent at `Undercroft` entirely. Depth structure is shared by
every world, so a pooled argmax is largely a function of each rung's chamber
count, and **M1 would very likely have returned 1 for a methodological
reason** — which §3.3's table would then have read as the substantive null
that supersedes 0966. M1 is now per-rung, `M1 = max_r |{a(s,r) : s in S}|`.
This is the same pooling defect this campaign had already identified in the
existing instrument two hours earlier, reproduced in its own preregistration;
it was caught only because the verification run printed a table the draft did
not know existed.

**DEFECT 2, caught and fixed: a wrong attribution.** Two passages credited the
metaplan with placing "a field, a variant, and a draw" on rung 4's side of the
line. That sentence is The Winze's amendment C.4, not the metaplan's. Both
corrected to cite C.4 with its own words.

**A third change, made on the evidence rather than on review:** M1's
prediction was weakened from `>= 3` to `>= 2`. Two is what 0966's quadrants
actually require — the allocation axis must take more than one value across
worlds — and three was a richness claim the design does not need and no prior
measurement supports. The branch table keeps the `>= 3` row so richness is
still *reported* when it occurs, and §3.3 names row 2 as the expected one so
that a surprise is recognised as one.

**Ideonomy passes / overturns:** none for this entry; the design passes were
#3's and the spec is their capture. The two defects were found by verification,
not by ideation, which is the distinction autopilot's own fold-in rule draws.

**Capture actions:** decision 0966 committed at `2d799dfa8` with README row and
regenerated digest, 45 docs-consistency tests green; spec committed; G3 package
next.

---

## #5 [Q] — Stage 2 re-pointed from the ceiling to the consumer

**Surfaced at G3, ruled by Nathan.** New information arrived after #1's scope
ruling and cut against the argument #1 was decided on.

**What arrived.** The Staple D5B reported its chokepoint 1: the seven sources
are averaged into one scalar, and "the only chemosynthate consumer is Xorn,
which is excluded from settled history. Thus geothermal/redox energy currently
cannot sustain settled communities."

**Re-verified rather than taken on report** (an inherited diagnosis is a
hypothesis; this campaign had already been burned once today by a peer's
correction that was itself wrong — see the wire exchange behind #4). All three
legs hold at `26003913d`, and the citations are in spec §2a.

**Why it overturns #1's reasoning.** #1 chose rung 3 because rung 4 "Needs
1-3" and building the consumer first would invert the program's dependency
order. Two facts break that argument:

1. **The dependency does not exist in the code.**
   `per_species_capacity_at`'s `Subterranean` arm already reads
   `chemosynthate_per_rung` and dots it against the kind's own niche weights
   (`windows/worldgen/src/lib.rs:2425`, `:2390`). A `Settled`, `Subterranean`
   kind weighting `CHEMOSYNTHATE` draws real carrying capacity **today**, with
   no rung-3 ceiling in the path. Verified before the re-scope was accepted,
   because the whole ruling rests on it.
2. **The Tidemark already won this argument for the marine half.** Its ledger
   #5: the marine consumer "is waiting on an underworld ceiling it never
   reads." The same reasoning transfers, and #1 did not notice that it had
   accepted the argument for one half while rejecting it for the other.

**And the scheduling fact.** Three campaigns now converge on rung 4's
underworld consumer and **nobody owns it** — The Staple D5B's D6 is blocked on
it, The Tidemark holds only the marine half, and the name it was reserved
under ("THE TENANT") belongs to an unrelated landing. Nothing needs rung 3
today.

**Decision.** Stage 1's measurement is unchanged. Stage 2 authors a settled
underworld chemotroph instead of the ceiling. The ceiling moves to a successor
**with the measurement in hand**, and decision 0966 stands and governs it.

**Alternatives discarded.** *Continue as spec'd* (ships a derivation with no
settled consumer; leaves D5B blocked on unowned work). *Both, staged* (larger,
and The Winze C.4 puts a consumer's field/variant/draw in its own epoch, so it
likely crosses a save-format boundary the spec claimed it did not). *Ceiling
here, consumer as a separate concurrent campaign* (two campaigns over the same
files in the same week — the collision shape CLAUDE.md says no gate has an
opinion about).

**The name is NOT changed, deliberately.** "The Ceiling" now covers a campaign
whose Stage 2 is not the ceiling. Renaming would touch the branch, decision
0966's own `**Campaign:**` line, the reconciliation row, the ledger filename
and the spec path; The Tidemark renamed mid-campaign this week and its
commits record a sweep that still missed a site. A slightly loose name is
cheaper than a rename with residue, and §1 states the re-scope in the document
a reader meets first.

**Ideonomy passes / overturns:** none run for this entry, and this is the
second `Q` in this ledger to say so. It is a scheduling-and-dependency
question settled by a verified code read plus Nathan's ruling, not a design
space — the design pass that matters (#3) already produced the
intensive/extensive fork that this re-scope acts on. Flagged in the re-issued
G3 package so it can be pushed back on rather than buried.

**Capture actions:** spec §1, §2a, §3.1, §3.3, §3.5, §5 and §6 rewritten;
`BIO-underworld-has-no-energy`'s stale capacity clause recorded in §2a;
wire message to The Staple D5B confirming its finding, warning about the
pooling hazard, and pointing at the existing probe scaffolding its stalled
join was rebuilding; G3 re-issued.

## Follow-ups (added at #5)

- **`BIO-underworld-has-no-energy` carries a stale capacity clause.** "Capacity
  is computed from INSOLATION and never reads `EnvironmentNiche` … fed by
  sunlight" is false for the `Subterranean` arm since The Sources wired both
  the supply and the tolerance. The row's *size* clause is unaffected. Not
  edited here — the row is another campaign's evidence and the correction
  belongs with whoever next touches it, but it is recorded so the next reader
  does not act on it.
- **The capacity/seating rung mismatch** (The Staple D5B chokepoint 2): the
  `Subterranean` arm keeps only the best rung and nothing has measured whether
  the capacity-winning rung differs from the seating rung. That campaign is
  measuring it; this one asked for the number on the board rather than racing
  it.
