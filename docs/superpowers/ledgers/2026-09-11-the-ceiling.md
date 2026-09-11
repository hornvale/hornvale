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

---

## #6 [G2] — The Staple D5B's review, and the hole it found

**Received via Nathan** (D5B is an OpenAI model and reaches this session only
through the board or him; the wire message sent to `resume-03` at #5 may never
have arrived, and the board post below is the channel that counts).

**It endorsed four things and corrected one.** Endorsed: 0966's hypothesis;
M1's per-rung per-world shape; M2's mean-vs-max shape; and the positive
control's necessity in the exact terms the spec already used ("must reproduce
0.145249; otherwise the new measurement is not comparable"). Independent
arrival at the same control requirement is worth recording — it was the
verification that nearly did not happen.

**THE HOLE, and it is a real one.** §3.5 asked Stage 2 for "does it place,
where, how many vertices, and at which rungs". **That readout cannot
distinguish a working `CHEMOSYNTHATE` weight from a decorative one.** `xorn`'s
niche is 0.65 `MINERAL` / 0.35 `CHEMOSYNTHATE`; a settled kind on that shape
could place entirely on its mineral half while the chemotrophic weight does
nothing, and the readout as specified would have reported success. That is the
campaign reproducing the exact defect §2a documents and it exists to close.

The remedy is D5B's: an **ablation**, now frozen as M4 — full energy vs zeroed
chemosynthate vs seven per-source variants, with `placed(b) < placed(a)` as the
prediction and `placed(b) == placed(a)` as a RED that sends the authoring back
to the niche rather than being written up as a finding. Arm (c) is the
provenance half D5B asked to preserve, and it calls out `DetritalImport`
separately because a kind depending on it is eating the surface at depth, not
living on chemical energy.

**A second correction, smaller and sharper.** "At which rungs" is two
questions. D5B's probe has now **measured** that the capacity-winning rung and
the seated rung differ at some underground endpoints — at #5 this ledger
recorded that as unmeasured, on their earlier report, and it is no longer. So
"the rock supports X" and "the people occupy X" are distinct claims, and every
Stage 2 figure must name which one it reports. The reconciliation stays D5B's.

**A third, taken as a guard on vocabulary.** "Do not treat source diversity as
realized ecological diversity yet" — `dominant_source` is diagnostic and is
consumed by no world number. §3.5 now says so explicitly: M1 measures whether
an allocation axis is *available*; only M4 measures whether a consumer rides
it. The spec was at risk of sliding between the two, and §2 already contained
both claims without distinguishing them.

**Scope held rather than expanded.** D5B asked for "capacity, placement, output
and trade". M4 measures capacity, placement and rung and **stops**; output and
exchange reach into D2, whose bottleneck D5B has already measured (seed 5: zero
complementary neighbours, zero proposals). Named in §6 as the handoff, with the
note that a chemotroph which places and still trades nothing is a D2 finding
rather than a refutation of this campaign.

**Ideonomy passes / overturns:** none; this is an incorporated external review,
not a design space this session opened. The overturn is D5B's, and it is
recorded as theirs.

**Capture actions:** spec §3.5 rewritten, M4 added and M3/M4 ordered, §6
extended with two handoffs; board post to D5B's channel carrying the
acceptance, the M4 text, and the two facts this session verified that their
report did not have (the `Subterranean` arm already dots `CHEMOSYNTHATE`
against the niche, and `BIO-underworld-has-no-energy`'s capacity clause is
stale).

---

## #7 [Q] — Multiple nutrient / medium axes: was it lost, and does it belong here?

**Nathan asked** whether the project has, or has ever captured, the axes a real
niche varies on — soil pH, texture (clay vs sand), NPK, moisture tolerance,
height above the water table — so the Underworld shows "tremendous richness and
variety" on a graph, "just as (I hope) we have on the surface."

**Answer to the first half: CAPTURED, NOT LOST, NOT IMPLEMENTED.**
`DOM-two-environment-bases` (raw), raised by The Hallmark §5 on 2026-09-01. It
even records the coordination note that it should follow campaign/the-sources,
which has since shipped. So the idea is ten days old and unstarted.

**Answer to the second half, and it is worse than the row knew.** Measured at
`26003913d`:

- A kind's tolerance is `species::ConditionNiche` — a **closed struct of four
  fields**: temperature, moisture, insolation, elevation
  (`domains/species/src/lib.rs:387`). Its dual, the place's reading, is
  `worldgen::Substrate`, the same four (`windows/worldgen/src/lib.rs:2981`).
  **Adding an axis is a struct change, not a registry addition.**
- **Underground, two of the four are degenerate.** `LIGHT`'s kernel doc says
  it is "constant zero underground"; `elevation` is metres above **sea
  level** — the height of the surface above the chamber, not the chamber's
  own depth, which is verbatim The Delvers' withdrawal reason ("a kind whose
  identity is DEPTH cannot be expressed by an axis measured in metres above
  sea level"). So a subterranean kind has effectively **two** live tolerance
  axes, and one of them (temperature) is gradient-driven and therefore close
  to a proxy for depth.
- **The richness is already computed and thrown away.**
  `terrain::MaterialBuffer` carries nine fields — silica, grain, induration,
  carbonate, metamorphic_grade, porosity, soil_depth, basement, margin. They
  feed `EnergySource` yields and collapse into one scalar. Nothing can have a
  *tolerance* on any of them. Captured as
  `BIO-lithology-is-not-a-tolerance-axis`.
- **`SUBSTRATE` does not rescue it.** The six-axis kernel basis has a
  `SUBSTRATE` axis, but its valence is **Nominal** — "the numeric value indexes
  an unordered set and is never a magnitude" (`kernel/src/ecology.rs:381`). A
  gradient like "more clay than sand" is unsayable on a nominal axis by
  construction.

**So the hoped-for surface richness is not there either.** The surface carries
the same four axes; it simply has more of them live. That half of Nathan's
sentence is answered "no", and it is the more important half.

**Nathan's ruling, same sitting:** `ConditionNiche` and `Substrate` should move
into the kernel and be unified.

**Verified before recording, because the ruling turns on it:** the kernel
**already holds the general form of both** —
`EnvironmentNiche(BTreeMap<u16, (EnvironmentAxis, AxisPreference)>)` at
`kernel/src/ecology.rs:187` is the preference,
`EnvironmentVector(BTreeMap<u16, f64>)` at `:428` is a place's reading, over
`environment_v1_basis()` (append-only, typed valences). So the work is
**deleting a duplicate, not designing a mechanism**, and the kernel is the only
layer both current homes can depend on: `species` is a domain and `worldgen` a
window, and a domain may not depend on a window, which is exactly why the four
fields are "spelled twice" with nothing enforcing agreement. Same forced-
duplication shape The Tidemark met with `HabitatRealm` vs `climate::Realm`.

**One clarification the ruling needs, recorded so it is not lost in
execution:** "unified" must mean *collapsing the closed four-axis pair onto
the kernel's open basis*, **not** merging niche and reading into one type.
They are duals — a niche is a response over axis values, a reading is a point
in axis space — and the kernel's existing pair already models that correctly.
Merging them would be a category error.

**Scope: NOT this campaign.** It touches every kind's authored niche, the
capacity and suitability paths, and the tolerance product, and it will move
world numbers. Recorded as the successor, and it is now a better-specified one
than the ceiling was. This campaign's Stage 2 readout gains one obligation
instead: **report how many tolerance axes actually discriminate its kind**, so
the successor inherits a measurement of the poverty rather than an argument
for it.

**Ideonomy passes / overturns:** none; this was a factual audit of what exists,
answered by reading the code and the registry, plus Nathan's ruling.

**Capture actions:** `DOM-two-environment-bases` amended with the measured
facts and Nathan's ruling; `BIO-lithology-is-not-a-tolerance-axis` added;
spec §3.5 gains the axis-discrimination readout (next commit).

---

## #8 [Q] — Light underground is not zero, and this ledger said it was

**Nathan's correction at G3 approval:** light underground is not zero
everywhere — torches, magical fire, bioluminescence — and "a sufficiently
strong magical light might end up having a few sprigs of grass growing
underneath it." He added that it does not necessarily change the campaign,
"but that's not a completely correct statement."

**He is right, and the error is mine twice over.** #7 and spec §3.5 both said
light is "documented constant zero underground", citing
`kernel/src/ecology.rs:391`. Two things are wrong with that:

1. **The kernel doc is stale against a measurement in another domain's own
   suite.** `domains/climate/tests/suite/underworld.rs:232-248` carries
   preregistered H5, measured 2026-08-17: the `LIGHT` axis takes **`{0.0,
   0.2}`** across the underworld corpus — 0.0 below the entrance rung, 0.2 for
   the four communities that break the surface. So "constant zero" was already
   false by measurement, three weeks before this campaign quoted it.
2. **The test names the two absent mechanisms itself**, and they are exactly
   Nathan's: "there is no bioluminescence term and `MaterialBuffer::thaumic`
   is identically zero in this tier."

**And the emitters already exist, one layer over.** The Lantern shipped
`windows/vessel/src/light.rs` with a torch at 1900 K, a hearth, lava and
fungi, and `windows/worldgen/tests/suite/lantern_probe.rs:34` models
bioluminescence explicitly as "a narrow emission near 490 nm".
`MAP-interior-light` calls it "the first light in the project that is not the
star." **None of it reaches the ecology axis** — the same shape as #7's
lithology finding: computed, then discarded at the niche boundary.

**So the correct statement is that the axis is UNFED, not meaningless**, and a
lit underworld is a supply problem rather than a modelling refusal. That is a
materially different thing to hand a successor: "this axis is dead" closes a
door that "this axis is dark" leaves open, and Nathan's grass-under-a-lamp is
the case that proves it.

**The methodological failure, stated plainly because it is the third of its
kind in this campaign.** I read a doc comment and repeated its claim without
checking whether a measurement had overtaken it — the same error The Tidemark
made about `OccupationRecord` this morning and that this session corrected, and
the same error #2 corrected in C.3's own cost claim. A committed doc is a
claim with a date. This campaign has now been on both sides of it in one
sitting.

**Scope: unchanged, and Nathan said so.** This campaign feeds no light source
and authors no emitter. What changed is one sentence of spec §3.5 and one
registry row, so the successor is not told an axis is dead when it is merely
dark.

**Ideonomy passes / overturns:** none; a factual correction from Nathan,
verified against the climate suite before being recorded.

**Capture actions:** `BIO-underground-light-is-unfed` added, carrying the H5
measurement, the two named absent mechanisms, the shipped-emitter pointers and
the stale kernel doc; spec §3.5 corrected from "degenerate/constant zero" to
"starved/unfed" with the distinction stated; `kernel/src/ecology.rs:391`'s
stale sentence recorded as a follow-up rather than edited — it is a kernel-layer
edit outside this campaign's surface, and the row now carries the correction.

## Follow-ups (added at #8)

- **`kernel/src/ecology.rs:391` says `LIGHT` is "constant zero underground".**
  Falsified by climate's own H5 (`{0.0, 0.2}`, 2026-08-17). Not edited here;
  a kernel-layer doc fix belongs with whoever next opens that file, and
  `BIO-underground-light-is-unfed` carries the correction meanwhile.

---

## #9 [G4] — Plan review, self-resolved

**Decision.** Plan written to `docs/superpowers/plans/2026-09-11-the-ceiling.md`
(nine tasks) and proceeding to execution without a ratification round, per
autopilot's G4 policy. Execution is subagent-driven per Nathan's standing
preference.

**Self-review against the approved spec.** Coverage checked section by section
and recorded in the plan's own Self-Review block: §2/§2a → Tasks 1 and 5;
§3.2 → Tasks 1-3; §3.3 → Task 4; §3.4 → Task 3 and Task 7; §3.5 → Tasks 5 and
7; §4's M1/M2/M3/M4 → Tasks 3, 1-2, 3, 6; §5 → Task 8; §7 → Task 9. §6 maps to
no task by construction — it is the NOT list.

**Three things the plan does deliberately, recorded so a reviewer does not
read them as defects.**

1. **Task 1 ships M2's positive control ALONE and FIRST**, before the
   measurement it controls. A re-implementation that cannot reproduce
   `0.145249` makes every M2 number uninterpretable, so the control is not a
   step inside M2 — it is the gate on whether M2 may be read at all. Its
   Step 5 says to stop rather than widen the tolerance.
2. **Two steps name a PROPERTY instead of prescribing code** — the niche's
   authored values (Task 5) and the per-source ablation arms (Task 6). This is
   autopilot's own rule ("never prescribe a specific mutation from outside the
   code"): a plan author does not know which values discriminate and the
   implementer does, after reading. Stated in the plan's self-review so it
   reads as intent.
3. **Task 8 gates committing at all**, and its three-way branch refuses one
   case outright: a moved stream consumption order stops and returns to
   Nathan with an epoch. That is the only case in this campaign that can
   silently corrupt every world.

**Two escalation points are written into the plan rather than left to
judgment.** Task 4 Step 2 escalates branch-table row 4 to Nathan before any
Stage 2 code, because that row supersedes decision 0966 and a decision-log act
is his. Task 6 Step 2 invokes the three-attempt rule explicitly on a red M4:
after three failed niche authorings, stop and escalate rather than continuing
to tune.

**Ideonomy passes / overturns:** none for this entry. The plan is the capture
of #3's design pass and the spec's own branch tables; no new design space was
opened in decomposing it.

**Capture actions:** plan committed at `a09e88d7a` with the reconciliation
row's `plans` column filled; two memories written outside the repo (the
`-A N` truncation family, and campaign-name pointers being claimed by
unrelated work).

---

## #10 [G5] — Pre-flight scan: three rulings before Task 1

The SDD pre-flight and `dispatching-hornvale-subagents`' step 1 (verify the
brief against the code), run together before any dispatch. **Budget was three
minutes of grep; it found two defects in my own plan text, one of them
silent.**

**Cross-task scan table** (every pair sharing a file or an interface):

| tasks | shared | produced → consumed | finding |
|---|---|---|---|
| 1 → 2 | `ceiling_composition_probe.rs` | `separation`, `CombinationRule` | clean |
| 1 → 3 | same file | world-construction idiom | clean |
| 1, 6 | the suite registration file | module registration | **R1** |
| 5 → 6,7 | `THE_KIND` | the authored `KindId` | clean — carried via ledger, not re-derived |
| 6 → 7 | `ceiling_tenant_probe.rs` | the probe's world setup | clean |
| 2,3,4,6,8,9 | the committed ledger | append-only | clean |
| every task | agrees with itself? | tests vs code vs files | **R2** in Task 6 |

**Ruling R1 — the registration file does not exist.** The plan said
"Modify `windows/worldgen/tests/suite/mod.rs`" in five places. **There is no
such file.** Registration is `windows/worldgen/tests/suite.rs`, which uses
`#[path = "suite/<name>.rs"] mod <name>;` (its line 224 registers
`subterranean_energy_probe`). Plan corrected in all five places. *Cost if
wrong: none — this is a verified path, not a judgment.*

**Ruling R2 — M4's ablation as specified was UNBUILDABLE, and this is the
silent one.** The plan's arm (b) said "the same world with the `CHEMOSYNTHATE`
supply zeroed". **No caller can do that.** `per_species_capacity_at` builds
`chemosynthate_per_rung` *internally*, inside
`per_species_capacity_at_with_invariant`; it is not an argument. An
implementer would have discovered this only after writing the probe.

Ruled: **arm (b) ablates the KIND'S NICHE**, via `species_biosphere:
&[&BiosphereTraits]`, which *is* an argument. This is not a downgrade — it
measures the question M4 actually asks ("is this kind's chemotrophic weight
load-bearing") more directly than zeroing the field would. **Arm (c) re-scoped**
from a per-source placement counterfactual to a supply-side diagnostic over the
kind's occupied vertices, for the same reachability reason. *Cost if wrong: a
narrower per-source result than D5B asked for — a true narrow number in place
of a placement delta the API cannot produce.*

**Ruling R3 — `CHEMOSYNTHATE` must be the DOMINANT weight (> 0.5).** Prompted
by D5B's contract question (board `ask`, thread `the-ceiling`), and decided
rather than deferred because their shadow probe needs it now. Not sole and not
a minority share:

- **Minority is the decorative trap.** `xorn` is 0.65 `MINERAL` / 0.35
  `CHEMOSYNTHATE` — exactly the shape that lets a kind place on its mineral
  half while the chemotrophic weight does nothing. That is the failure D5B's
  own review caught in the spec, and M4 exists to catch it; a minority share
  would make M4 likely to RED for a reason that is the authoring's fault.
- **Sole (1.0) was rejected**: it makes M4 pass trivially and asserts a purity
  the model has no reason to claim.

The exact values stay the implementer's (Task 5), per the property-not-
prescription rule; the *dominance* is the contract. *Cost if wrong: the kind
is a purer chemotroph than the fiction wants, and a successor re-authors one
row.*

**Capture actions:** plan corrected for R1 and R2 (eight edits); board `reply`
`01200fcdeca0` answering D5B's four contract questions, carrying R2 as a
warning that their shadow probe may already assume the unreachable ablation,
and declining to offer my probe's stdout as a stable interface — naming the
`pub` registry and functions instead.

---

## #11 [Q] — D5B's capacity/seating denominator, and what it does NOT license

**Asked for on the board and answered** (`reply`, thread `the-ceiling`). On
seed 5, their public endpoint probe found **2 unique live underground Drow
endpoints** (sites 894, 14056). Both — 2 of 2 — carry **capacity-winning rung
`Nadir`** and **seated rung `Undercroft`**, seated multiplier 0.875. Arm
observations are 4 of 4 because each site appears in both their arms; the
**independent-site denominator is 2**, and they state the distinction
themselves rather than reporting the larger number.

**Decision: carried into Task 7 as a PRIOR, asserted on nowhere.** Two sites,
one seed, one pre-existing kind is an anecdote, and D5B scopes it exactly that
way — "not a claim about every underground vertex or future Ceiling kind". The
plan now quotes it with its denominator and forbids both asserting against it
and assuming it generalizes.

**What it does earn.** The readout must not be built assuming the two rungs
agree — which is a design constraint, not a result. And the *direction* is
worth stating because it runs against the energy: the divergence is
`Nadir` vs `Undercroft`, opposite ends of the ladder, while S2 measured `fed`
occupancy opening only at `Nadir` and the per-rung histograms put
`SulphideOxidation` dominant deep. **The food is deep and the people are
seated shallow.** If this campaign's chemotroph reproduces that, it is seated
away from its own subsistence — a chronicle-grade finding, with its own
denominator.

**Worth recording about the exchange itself, not only its content.** D5B
reported the *smaller* number as the denominator and explained why the larger
one (4) was an artifact of counting arm-observations rather than sites. That
is the discipline this project's own memory keeps relearning — reconcile a
summary count against its visible rows — arriving unprompted from a peer.

**Ideonomy passes / overturns:** none; an inherited measurement, scoped on
receipt.

**Capture actions:** plan Task 7 Step 1 rewritten to quote the number, its
denominator and its scope limit; board reply to follow.

---

## #12 [M] — Task 2 measured: M2 is rock, not mean

**Measured 2026-09-11**, `windows/worldgen/tests/suite/ceiling_composition_probe.rs::max_of_seven_separates_worlds_the_mean_does_not`, `Q6_SEEDS` (n=12), `BuildDepth::Terrain`:

- `separation(mean-of-seven) = 0.145249` — control, reproduces Task 1's (and
  the originally published) number exactly.
- `separation(max-of-seven) = 0.040745` — the composition-preserving
  diagnostic. `ratio max/mean = 0.2805`.

**PREREGISTRATION NOT MET (spec §4): 0.040745 < 0.25.** Worse than a plain
miss: `max-of-seven` separates worlds *less* than the mean it was meant to
bound, not more. Swapping the combination rule does not merely fail to help —
it moves the statistic the wrong way, so the diagnostic's own premise
("bounds what the mean is costing") does not hold either. **The rock is the
cause, not the combination rule; the metaplan's inherited diagnosis stands
unqualified.** No `EnergySource` was retuned; the test's assertion now pins
this measured null (tolerance `5e-7`), matching
`subterranean_energy_probe.rs`'s own pattern for a falsified prediction.

**Branch-table row (spec §3.3): not yet fully determined — M1 is Task 3's
job, not this task's.** What this measurement DOES settle: **row 3 ("exactly
1, mean is a major cause") is excluded.** M2 measured the opposite of that
row's condition. The three rows still open, pending M1 (Task 3):

| M1 result | row that lands |
|---|---|
| exactly 1 | **row 4** — the null as headline: composition cannot carry allocation, 0966 superseded by a record choosing between C.3's original two |
| exactly 2 | row 2 — binary axis, 0966 stands narrowed (the spec's own "expected" row) |
| ≥ 3 at some rung | row 1 — named-dominant-source axis, 0966 stands as written |

If M1 lands "exactly 1", M2's measurement here means the campaign lands on
row 4, not row 3 — the more consequential of the two "exactly 1" outcomes for
Stage 2's design (aggregate-supply consumer either way, but row 4 additionally
supersedes decision 0966 rather than leaving it standing with a combination-
rule repair still owed).

**Housekeeping:** `CombinationRule::MaxOfSeven`'s `#[allow(dead_code)]` is
removed (this task's test constructs it) and its doc comment now points at
`max_of_seven_separates_worlds_the_mean_does_not` instead of a future task.

---

## #13 [G5] — M2's result, and the inverted rationale it exposed

**Measured (Task 2, commit `ab6da0871`).** `separation(mean-of-seven) =
0.145249` (control, reproduced exactly); `separation(max-of-seven) =
0.040745`. Preregistered bar `>= 0.25` **not met**, and `max` separated
**3.6x worse** than the rule it was meant to indict.

**The spec's rationale for choosing `max` was INVERTED, and the measurement is
what exposed it.** The spec said `max` "discards the least composition". A
formal check of the two rules says the opposite: `mean = (1/7)Σyᵢ` gives every
source a nonzero derivative (1/7), so none is discarded, only diluted; `max`
gives the winner derivative 1 and the other six **exactly 0**, so a
world-to-world difference in any non-winning source is invisible to it. `max`
discards the **most**. Spec §4 corrected in place with the correction stated
rather than the sentence quietly deleted.

**A THIRD CAUSE THE BRANCH TABLE NEVER CONTEMPLATED: order-statistic
saturation.** Every `yield_at` arm is a saturating function (a `bump()` over
silica times a `water_gate()` saturating at 0.1–0.4), and
`subterranean_energy`'s own doc records that four to six of seven sources are
simultaneously non-trivial at most chambers. So the winner usually sits near
its own ceiling, and which ceiling wins is set by a few shared near-constant
inputs. This is neither "the rock" nor "the mean" — the two options §3.3
offered.

**Ruling: the branch table's ACTIONS stand, its VOCABULARY is narrowed.** M2
legitimately excludes row 3 — a non-averaging rule failing too is real
evidence against "the mean is a major cause". But row 4's label "rock is the
cause" must be read as "**not the mean**": `separation(max) = 0.040745` does
not independently confirm the metaplan's ~three-categorical-states diagnosis.
Recorded in the spec so a successor deciding what to fix knows it must still
distinguish the rock from saturation. *Cost if wrong: a successor spends
effort separating two causes that turn out to be one.*

**My review prompt overstated the evidence, and the reviewer caught it.** I
wrote that the per-rung histograms show `IronReduction` leading shallow
"across all twelve seeds". They are **pooled** over twelve seeds, which shows
the aggregate argmax is depth-determined and **not** that every seed agrees at
every rung — a pooled majority can hide per-seed disagreement. That per-seed
question is precisely M1's, and it has not run. This is the second time in
this campaign that a pooled statistic was mistaken for a per-seed one, the
first being the M1 defect the spec's own self-review caught; the two share a
cause and it is mine.

**Deferred minor, folded into Task 3 rather than a fix round.** Task 2's test
pins the **exact** measured null (`|max - 0.040745| < 5e-7`), while
`subterranean_energy_probe.rs`'s own falsified predictions pin **direction
only** (`separation < 0.25`). The report characterises it as matching that
precedent; it actually matches the positive-control pattern. Direction-only is
the right semantic for a falsified prediction — it asserts *the prediction
still fails* rather than *this number has not moved*, and an exact pin will red
on any unrelated upstream change in a way that reads as "M2 broke". The exact
value belongs in the doc comment with its date, where it already is. Task 3
touches the same file, so it carries the change; no fix round for a Minor.

**Ideonomy passes / overturns:** none; this is a measured result plus a
correction of my own text, not a design space.

**Capture actions:** spec §4 corrected (inverted rationale, the measured
numbers, saturation as a third cause, and what the branch table's vocabulary
can no longer claim); `CombinationRule::MaxOfSeven`'s doc comment inherits the
same false framing and is folded into Task 3's dispatch.

---

## #14 [Q] — Underground, only ONE tolerance axis varies with depth

**Flagged by campaign/the-tidemark** (wire, alongside its M1 result) and
**re-verified here rather than inherited** — they explicitly said their own
reviewer was still assessing and to treat it as reported, not confirmed.

**Verified at `windows/worldgen/src/lib.rs`.** `tolerance_liebig_with_fixed`
reads exactly two things off the per-rung `Substrate` — `height_asl_m` and
`temperature_c`. The `fixed` half comes from `EraInvariantTolerance::build`,
which is a per-**vertex** `VertexMap` with **no rung dimension**, built from
`climate.moisture_at(vertex)` and surface insolation.

| axis | what a subterranean kind is scored on |
|---|---|
| temperature | per-rung — **the only depth-varying axis** |
| moisture | the **surface weather** above the chamber |
| insolation | surface insolation |
| elevation | `height_asl_m`, the surface height, constant across rungs |

**`Substrate.moisture` never reaches the tolerance product.** The real
per-rung chamber moisture — saturated below the water table, the thing
`chamber_moisture` exists to compute — is fed to the ENERGY supply and
nowhere else. A cave-dweller authored to "like damp" is being asked about the
weather above it.

**This corrects THIS campaign's own text for the second time on the same
subject.** Ledger #7 and spec §3.5 said "roughly two live axes (temperature,
moisture)". It is one. #7's conclusion — that the underworld is axis-poor —
was right and understated; the specific count was wrong, and I reached it by
reading the `Substrate` struct's fields rather than the function that consumes
them. **A struct's fields are what a place CAN say; the consumer decides what
is HEARD.**

**Consequences taken now, not deferred.**
- **Task 5** must not author a moisture preference expecting it to track
  chamber wetness. Plan corrected; if a moisture curve is authored anyway its
  comment must say it reads surface moisture.
- **Task 7's axis-discrimination readout** now has a specific prior: expect 1.
  It still MEASURES rather than asserting — a readout that assumes its own
  answer is worthless — but a result of 2+ would be the surprise.
- **Spec §3.5** carries the verified table and the code excerpt.
- `BIO-underground-tolerance-is-one-axis` added; it sharpens
  `DOM-two-environment-bases` considerably, because the unification Nathan
  ruled on is now known to be repairing a path where three of four authored
  axes are either surface-sourced or constant underground.

**Two further flags from the same message, RECORDED AND NOT ACTED ON** —
reported by a peer, unverified here, and neither blocks this campaign:
- `Seating::rung` is a `Band` and no pelagic stratum is expressible in one, so
  their marine kinds seat at `Band::Surface` and share the bake's
  `(vertex, rung)` node slot. If any rung-3 successor assumes that pair
  uniquely identifies an occupant, that becomes false in the marine case.
- A shipped kind forced to `Marine` yields capacity exactly 0.0 at all 40,962
  vertices, because its curves were authored for land. **The transferable
  shape:** a realm gate can be perfectly correct and still produce an all-zero
  field because the kind's own curves were authored for somewhere else. Folded
  into Task 6's expectations — if M4's placement comes back zero everywhere,
  suspect the authored curves before the realm plumbing.

**Ideonomy passes / overturns:** none; a verification of a peer's flag.

**Capture actions:** spec §3.5 table, plan Task 5, registry row, and the
Task 6 note above. Reply to The Tidemark confirming the flag with the
verification they had not yet completed.

---

## #15 [M] — Task 3 measured: M1 = 3, composition separates worlds at every rung

**Measured 2026-09-11**, `windows/worldgen/tests/suite/ceiling_composition_probe.rs`
(`composition_separates_worlds_at_some_rung`), `Q6_SEEDS` (n=12),
`BuildDepth::Terrain`, per-rung as spec §4 requires — never pooled.

**Result: `M1 = 3` (>= 2 preregistered, spec §4). PREREGISTRATION MET.**

| rung | M1(r) | distinct argmaxes | median within-rung pairwise TV |
|---|---|---|---|
| Undercroft | 2 | {1, 2} | 0.1424 |
| Shallows | 2 | {1, 2} | 0.1924 |
| Deeps | 3 | {1, 2, 3} | 0.2127 |
| Underdeep | 3 | {1, 3, 4} | 0.2436 |
| Nadir | 2 | {1, 3} | 0.2350 |

(Indices into `EnergySource::ALL`: 0 Serpentinization, 1 IronReduction,
2 Radiolysis, 3 SulphideOxidation, 4 Methanogenesis, 5 Geothermal,
6 DetritalImport.)

Every rung independently clears the `>= 2` bar, not only the maximum over
rungs — this is not one rung carrying the whole result. Median within-rung TV
distances run 0.1424–0.2436, none near zero, so the argmax disagreement is a
real histogram-shape difference between worlds, not two near-tied entries
flipping on sampling noise. All sixty `h(s,r)` were printed by the test run
before any verdict (`cargo nextest run -p hornvale-worldgen --test suite
--run-ignored all --no-capture -E 'test(composition_separates_worlds)'`,
73.09s, 1 passed).

**Branch-table row landed (spec §3.3): "≥ 3 at some rung."** Stage 2 authors
a consumer whose niche favours a **named dominant source**; the successor
inherits a **rich allocation axis**, and decision 0966 **stands as written**
— no supersession. Per spec §4's own framing this is the **surprising** row:
"Row 2 is the one to expect... no prior measurement supports richness." Row 1
landed instead. Worth flagging for Stage 2's own scrutiny rather than treating
as merely confirmatory — the spec explicitly asked for that scrutiny if row 1
or row 4 landed.

**No retuning.** No `EnergySource` was touched to produce this result; the
histogram is `dominant_source`'s shipped behaviour, unmodified, read off the
same terrain/moisture/drainage construction Task 1's `pooled_sample` already
used.

**Two corrections carried from Task 2's review, both applied in this same
file/commit** (not a separate Minor — same file Task 3 touches):
1. `CombinationRule::MaxOfSeven`'s doc comment (and the enum-level doc above
   it, which repeated the same claim) no longer calls `max` "the
   composition-preserving extreme." `mean` gives every source derivative
   `1/7` (diluted, not discarded); `max` gives the winner derivative `1` and
   the other six exactly `0` — it discards the MOST composition, not the
   least. Reframed as what it actually is: a genuinely non-averaging rule,
   useful as a diagnostic precisely because dilution-by-averaging predicts it
   should separate worlds at least as well as `mean` — and measured, it did
   not.
2. `max_of_seven_separates_worlds_the_mean_does_not`'s falsified-prediction
   assertion changed from an exact pin (`(max - 0.040_745).abs() < 5e-7`) to
   direction-only (`max < 0.25`), matching
   `subterranean_energy_probe.rs`'s own wording for its two falsified
   predictions. An exact pin asserts a number hasn't moved and reds on any
   unrelated upstream drift, reading as "M2 broke"; direction asserts the
   finding (still short of the bar) and is what actually matters. The exact
   measured value (`0.040745`, 2026-09-11) stays in the doc comment.

**Ideonomy passes / overturns:** none; a measured result plus a documentation
correction, not a design-space move.

**Capture actions:** this entry; row 1's branch-table consequence
("named-dominant-source consumer, rich allocation axis, 0966 stands") is
Stage 2's brief now, superseding the row-2 expectation spec §3.3 and this
ledger's earlier entries assumed.


---

## #15 [Q] — "One axis" was too strong, and the ladders are not the same

**The Tidemark re-ran #14 rather than inheriting it** — the discipline this
campaign asked of them this morning, returned. They confirmed the underworld
finding with line numbers and corrected two things.

**Correction 1: say TOLERANCE, not placement.** `CHEMOSYNTHATE` *does* arrive
per rung, through the **supply** product — which is exactly what rung 2 built
and what spec §2a verifies. So **placement sees two per-rung inputs, not
one**. #14's row and spec paragraph were scoped to tolerance and correct, but
read as a claim about the whole scoring path, which would make this program's
own shipped per-rung energy field look unreached. Both corrected. *This is a
scoping defect, not a factual one, and it is the kind that propagates: a later
reader citing "one axis" would have understated rung 2's own delivery.*

**Correction 2: the delve ladder is not the pelagic ladder, and I generalized
across them without checking.** `marine_habitat.rs:195` builds each marine
band with `height_asl_m: SeaLevelHeight::from_metres(-field.depth_m)`, so
*there* elevation IS band depth and varies — and it is the hard gate, since
`if elevation <= floor_buf` short-circuits before temperature. Marine has
three live per-band inputs; the delve ladder has two. **I asserted my arm's
count of their arm on the strength of a shared consumer**, which is the same
error class as reading a struct's fields instead of its consumer — one level
up. Recorded in the spec as an explicit instruction not to generalize either
ladder's axis count to the other.

**WHAT THE EXCHANGE PRODUCED THAT NEITHER SIDE HAD.** My over-strong warning
was wrong in its specific claim and right in substance, and it found a live
defect in their campaign: `marine_habitat.rs:190` sets `insolation:
field.light` per band — the depth-attenuated light ladder — **and nothing
reads it.** Not the tolerance path (per-vertex `fixed`), not the supply path
(`score_at`'s PHOTOSYNTHATE comes from `base_carrying.at(vertex)`). An
authored field that is populated, plausible, and inert.

Their M5 counted *any* authored difference, so **a pair of kinds separated
only by insolation curves would have scored 1 and passed** — a distinctness
test satisfied by a distinction the engine cannot see. They have changed M5 to
exclude axes that do not arrive (their `67c1c3bf1`), and the kind it
endangered is their kelp tender, a phototrophic people of the photic zone
differentiated by the one axis that does not arrive.

**The transferable rule, and it is worth more than either finding:** a
distinctness or coverage test must count only the axes its own engine READS.
Counting authored differences measures the author, not the world.

**Taken for this campaign's own Task 7.** The axis-discrimination readout must
report which axes *arrive*, not which are *authored* — otherwise it reproduces
their M5 defect on my side. The plan already says "report whether it varies
across the kind's occupied vertices", which is an arrival test rather than an
authoring test, so no change is needed; recorded here because I checked rather
than assumed, and because the next reader of that step should know why it is
phrased that way.

**One inheritance for the successor.** `Substrate.moisture` is already
populated with the real per-rung chamber value, so a future per-rung moisture
reader needs no threading — the field is waiting. The Tidemark's marine
equivalent is a constant (`MARINE_MOISTURE`) with a comment conceding marine
moisture could not matter however finely computed, so that half is genuinely
dead on their side and merely unread on ours.

**Ideonomy passes / overturns:** none; a peer's verification of my claim,
which overturned its scope twice.

**Capture actions:** registry row and spec §3.5 rescoped to tolerance with the
marine contrast stated; Task 7's phrasing checked against their M5 defect and
found already correct.
