# The Tidemark — campaign ledger

Campaign: architectural continuation of The Vent and The Living Vent, taking up
`WAT-sea-peoples`. Provisional title: "The Tidemark". Branch: `campaign/the-tidemark`.

## #1 [G1] — What should a sea-peoples campaign deliver, given the Waterworld
## overlay has no non-test caller?

**Decision:** Wire the overlay into a reachable build path and make a marine
people its first consumer, in one campaign. Concretely: the overlay is
constructed by a `BuildDepth` rung rather than by a test's explicit
`WaterWorldConfig`; `HabitatRealm` grows a `Marine` variant whose arms the
compiler enumerates; one peopled marine kind carries an `EnvironmentNiche` row
scored against the corpus already in `domains/climate/src/axes.rs`; a two-way
agreement test in `windows/worldgen` pins `HabitatRealm` against
`climate::facets::Realm`; and vent phase is an input to seating, so a failing
vent yields an occupation with a cause.

**Why:** Measured, not assumed — every entry point to the Waterworld subsystem
(`waterworld_from`, `observe_waterworld`, `observe_waterworld_snapshot`,
`WaterWorldConfig` construction) is called only from
`windows/worldgen/tests/suite/waterworld.rs`. Two campaigns of Waterworld are
real, tested, and off the user path. Adding peoples on top of that without
wiring it would deepen a subsystem nothing constructs. The realm half is
well-precedented: The Underworld / The Delvers / The Radiation / The Range
shipped a complete template for a non-surface peopled realm (realm gate,
`per_species_suitability_masked` availability gate, `EnvironmentNiche`,
realm-aware capacity and seating, `substrate_response` curve), and
`substrate_response`'s own doc anticipates a third variant stopping the
compiler. The Delvers' withdrawal condition — "they return when the underworld
has biomes" — does not bind here: `climate::facets::Realm::WATERWORLD` already
exists with five pelagic strata and an `Access::Dive`, and the environment
basis already carries `reef`, `kelp-forest`, `vent`, `coral-head`,
`kelp-canopy`, `smoker-field`, `vent-plume`, `abyssal-plain` and an open-water
set descending the pelagic ladder.

**Alternatives discarded:**
- *Realm axis only* (add `HabitatRealm::Marine`, leave the overlay unreached) —
  deepens an unobservable subsystem and risks the Delvers' withdrawal a second
  time.
- *Wire the overlay only, peoples later* — cheaper and better-evidenced for its
  successor, but leaves `WAT-sea-peoples` raw for a second campaign to pay the
  context cost again.
- *Spike first* (probe whether marine substrate discriminates across seeds) —
  folded in as a measurement task inside the campaign rather than run as a
  separate throwaway.
- *A thin port of the drow template* (seating at a vertex, vent phase ignored) —
  competent but is a port of shipped work; it forgoes the one thing only
  Waterworld supplies (below).

**Ideonomy passes / overturns:** Three passes, two overturns.
- Pass 1 (tree-finding, dimension-identification; chart; purpose/autonomy/
  modularity) **overturned the headline**. Walking the realm tree put Marine as
  the third of four siblings — Surface and Subterranean shipped, Aerial (MAP-11's
  third medium) empty — and the chart's top-right cell surfaced the framing the
  campaign now carries: the marine realm is the **first realm whose habitat can
  expire**. A cave does not cool and clog; a vent does (`VentState`:
  absent/nascent/active/weakening/failed, with migration along a candidate ring
  and `Failed` keeping source identity). That makes wiring the overlay
  load-bearing instead of incidental, and is not reachable by porting the
  underworld template.
- Pass 2 (substitution, combination; dictionary; age/autonomy/materiality)
  **overturned the mechanism**. Defining "realm" precisely exposed that
  `species::HabitatRealm` and `climate::facets::Realm` are the same axis
  expressed twice, with the species copy missing exactly the realm climate
  already names. The duplication is forced by constitutional layering (a domain
  may not depend on a sibling), so the campaign owes a two-way agreement test at
  the composition root, not merely a new variant. Pass 2 also surfaced the
  reading ambiguity recorded as #2.
- Pass 3 (cross-domain re-instantiation; procedure; intentionality/reversibility)
  produced **no new option and no overturn** — it enriched the expiring-habitat
  idea (obligate vent fauna persist by larval dispersal rather than by the
  settlement surviving; `OccupationRecord.cause` already carries `Fled` and
  `Migrated` for the intentional/forced distinction) and confirmed convergence.

**Capture actions:** Discarded alternatives recorded above with reasons. The
aerial-realm observation and the dispersal-vs-migration distinction are carried
in Follow-ups below rather than minted as registry rows, since `MAP-11` and
`BIO-34` already cover them.

## #2 [Q] — Does "sea peoples" mean residents of the water column, or seaborne
## raiders?

**Decision:** The MAP-11 reading — residents of the water column (merfolk,
sahuagin, kuo-toa) — not the Bronze Age Collapse sense of surface-dwelling
peoples who arrive *by* sea.

**Why (precedent):** `MAP-11` is `WAT-sea-peoples`' structural parent and states
the aquatic case explicitly as "an aquatic people (sahuagin, merfolk, kuo-toa)
inverts land/water suitability and lives in a 3D water volume". The registry
chain answers this before memory or convention is reached. The row's own
enumeration ("sea peoples, merfolk, tritons, elves, and dragons") is what makes
it ambiguous, since it lists *sea peoples* alongside the aquatic kinds rather
than as one of them.

**Alternatives discarded:** The raider reading is a coherent and interesting
campaign — it connects to `SOC-casus-belli`, which records that an occupation
carries how it ended but never *why* anyone did it — but it needs no realm axis
and no Waterworld overlay at all, so it is a different campaign wearing this
row's name.

**Ideonomy passes / overturns:** Two. The question was raised by G1's pass 2
(dictionary organon, term-overlap cross-check), which is what separated the two
readings. A dedicated pass was then run at Nathan's request
(organon-construction, abstraction-lift; scale; animacy/hierarchicalness/scope)
and did **not** overturn the decision, but strengthened and extended it:

- *Abstraction-lift* stripped the proper nouns from both readings and left one
  structural distinction — **a medium is either a habitat or a corridor**. The
  two readings are not rivals about one row; they are two relations to the same
  sea. The shape recurs across domains (mountains as home vs. pass, rivers as
  dwelling vs. trade route), which is the lift's own sanity check.
- *Scale* ordered the readings by how much of a people's life the sea accounts
  for: 0% inland, ~10% coastal fishers, ~30% maritime traders and raiders
  (the Bronze Age reading), ~50% amphibious, ~90% water-column people who can
  surface, 100% obligate marine (the adopted reading). This gives the decision a
  **mechanical** basis rather than only a precedential one: `HabitatRealm` is a
  categorical gate whose availability mask is `{0.0, 1.0}`, so it can express
  only the endpoints. The 30% position needs no realm variant at all, which is a
  firmer reason to call it a separate campaign than "MAP-11 is the parent row".
- **The material finding is that the middle of the scale is already occupied —
  by a PEOPLE, not just by fauna.** Checking the roster rather than reasoning
  about it turned up `sea-elf`: a settling people with `SWIM`, a marine-dominant
  resource axis and an authored `(depth, SST)` shelf affinity, which is
  `Surface` today only by ABSENCE from a sparse registry. Its own rows say why
  that is right — "a settled coastal people does not live entirely in the
  water", so "it sleeps ashore, on what it built. Not `ALREADY_BUOYED`."
  `giant-crocodile` is the same question in fauna form (the roster's stated
  amphibious case, land-dominant at 0.6 `ANIMAL_PREY`). Introducing
  `HabitatRealm::Marine` creates a classification question for both that nothing
  currently answers, and a reader will assume a sea elf belongs to the new
  variant; acting on that assumption strips a settling people of the shore it
  builds on. Spec §3.6 and measurement M4 were added for this.
- **Consequence for scope:** part of `WAT-sea-peoples`' own enumeration
  ("elves") is therefore already shipped. What is missing is the **obligate**
  marine kind — the far end of the scale from sea-elf's shelf — and the realm
  gate is exactly the separation between the two. That also answers G3 question
  3 (whether one authored kind is enough): the contrast pair already exists, so
  the gate does visible discriminating work from day one.

**Capture actions:** Flagged to Nathan in-session rather than resolved silently,
since the two readings diverge materially; proceeding on the MAP-11 reading
pending his correction at G3. The raider reading is recorded in Follow-ups.

## #3 [Q] — Does the marine consumer belong to this campaign or to THE TENANT?

**Decision:** Split by question, not by realm. The Tidemark does **placement**: the
`Marine` realm gate, the availability mask, an `EnvironmentNiche` row, and
seating. It does **not** give any marine kind a `CHEMOSYNTHATE` weight or touch
`TrophicMode` — that is THE TENANT's, and the boundary is recorded here so the
two campaigns cannot both claim it.

**Why (precedent):** The underworld already separated these into different
campaigns, per kind. `drow` carries a realm gate and an `EnvironmentNiche` and
weights no chemosynthate; `xorn` weights `CHEMOSYNTHATE` at 0.35 and settles
nothing (`environment_niche_registry`'s own doc: "they are fauna, they settle
nothing, and authoring a niche for a kind that places no community would be a
value no consumer reads"). So placement-vs-trophic is already the house split,
and applying it to the marine realm is symmetry with the shipped case rather
than a new rule. `marine_chemosynthate_supply_field`'s doc assigns the marine
chemotroph explicitly: "authoring a marine chemotroph, or giving an existing
marine kind a `CHEMOSYNTHATE` weight just to make the supply look used, is THE
TENANT's job (rung 4), not this one's."

**Alternatives discarded:** Absorbing the trophic half would take a deliverable
another program has claimed and specified; deferring the whole campaign until
THE TENANT lands would block on prerequisites this work does not need (below).

**Ideonomy passes / overturns:** One pass (dimension-identification; chart;
symmetry/homogeneity). No overturn — it confirmed the boundary and sharpened it
into the four axes above (question asked, registry touched, gate shape,
realm). The homogeneity prompt surfaced the observation recorded in Follow-ups.

**Capture actions:** Boundary stated as a spec constraint; the observation about
THE TENANT's prerequisite chain recorded in Follow-ups.

## #4 [Q] — The campaign's name, after a collision

**Decision:** "The Tidemark". The campaign opened as "The Shoal" and the branch
was cut as `campaign/the-shoal` before the name was checked.

**Why:** "The Shoal" is already a merged campaign — `book/src/chronicle/the-shoal.md`
and `docs/retrospectives/the-shoal.md` both exist, and it was itself
marine-flavoured (its retrospective's first lesson is that no gallery page
sampled a marine room), so the collision would have been actively confusing
rather than merely duplicative. Caught by
`docs_consistency::campaign_reconciliation_covers_every_campaign_record`
refusing the spec's first commit, not by the session. Candidates were checked
mechanically against `docs/retrospectives/`, `book/src/chronicle/`,
`docs/superpowers/specs/` and the branch list; "the-fathom" was also taken
(3 files), and "the-tidemark" was free on all four. It names the campaign's
headline — the line a receding habitat leaves — rather than a zone of the sea.

**Alternatives discarded:** the-benthos (precise but names seabed life, while
seating spans the whole pelagic ladder), the-littoral (same narrowness, at the
other end of the column), the-anchorage, the-strand, the-demersal — all free,
none naming the expiring-habitat claim.

**Ideonomy passes / overturns:** None, and stated explicitly rather than left
blank: this was a forced rename resolved against a mechanically checked
candidate list, not a design question.

**Capture actions:** The worktree directory remains `.claude/worktrees/the-shoal`
and is deliberately NOT renamed — `CARGO_MANIFEST_DIR`, `CARGO_TARGET_TMPDIR`
and `CARGO_BIN_EXE_*` are baked at compile time, so renaming a worktree with a
warm `target/` hands it stale paths (CLAUDE.md's worktree note, and
`scripts/worktree-take.sh`'s own grep+touch mitigation). The branch is the
identity that matters; the pool member's directory name is not.

## #5 [G2] — Scope expansion: six peoples and a subsistence roster

**Decision:** At Nathan's direction the campaign grows from one marine people to
**six**, chosen for variety across shipped trait axes, plus the flora and fauna a
marine subsistence web needs. The campaign also absorbs the **marine half** of
the underworld-larder metaplan's rung 4, so the vent commensal weights
`CHEMOSYNTHATE` and becomes the first consumer of
`marine_chemosynthate_supply_field`.

**Why the expansion does not reproduce the Delvers' defect:** because the
instruction was "variety across relevant axes", not "more kinds". The hazard is
specific and documented — `habitat_realm_registry` states it as "the trap is not
authoring a subterranean kind, it is distinguishing two kinds by DEPTH, which
nothing in the model can say", and The Delvers withdrew two peoples over it. The
remedy is made **testable** rather than promised: spec §3.4 carries the rule
"no two marine kinds may differ only by stratum", and M5 measures all 15 pairs
and reports the minimum axis-difference count. A pair scoring 0 is the defect
reproduced, and the stated remedy is to merge or re-author, never to argue the
depths are far apart.

**Why the Tenant absorption:** verified rather than assumed, after Nathan asked
whether THE TENANT had closed. A campaign of that name **did** run and minted
decision 0516 ("A reachable lock implies a reachable key is left empirical",
2026-08-30) — chamber locks and keys, following The Chattel, and unrelated to
the larder. It left no chronicle, retrospective, spec, plan, ledger or
reconciliation row. The *larder's rung 4* is untouched and still open:
`BIO-chemotrophy` is `raw` and still reads "no `Surface`-realm kind weights it
yet, so a vent's own supply reaches no consumer — THE TENANT's job, rung 4", and
`marine_chemosynthate_supply_field`'s doc still reserves it in the present tense.
So the reservation is held by an unstarted rung, not by work in flight, and this
ledger's own earlier Follow-up already argued the marine half is misfiled: the
supply is shipped rung-2 work queued behind an underworld ceiling it never
reads. The underworld half, and The Winze's unruled symmetric-budget question,
stay with THE TENANT untouched.

**Alternatives discarded:** leaving the chemotroph to THE TENANT (would ship the
campaign's headline — a habitat that expires — with no people depending on a
vent); absorbing rung 4 entirely (would require ruling The Winze's symmetric
budget question, which is Nathan's and unruled).

**Ideonomy passes / overturns:** None run for the expansion itself — it is a
direction from Nathan, not a question the session resolved. One was run for the
axis slate's *shape* in the sense that the axes were enumerated from the code
rather than invented; the slate remains a proposal and G3 question 3 puts it to
Nathan explicitly.

**Capture actions:** Spec §3.4 (the slate and the distinctness rule), §3.7 (the
subsistence roster and the named-kind-vs-aggregate line), §7 (the narrowed
Tenant boundary), M5 (pairwise distinctness), M6 (the web closes), and M2
rewritten two-sided so the nomad's zero settlements is a prediction rather than
a failure. The observation that a decision-only campaign leaves no
reconciliation row is recorded in Follow-ups.

## #6 [Q] — How amphibious can `sea-elf` be, given a pinned balance invariant?

**Decision (Nathan's, at a fidelity carve-out):** **Reach, not residence.**
`sea-elf` keeps its shelf-confined residence and gains explicit deep *reach* —
diving, water-breathing, and meeting tritons, merfolk and abyssal elves where
those peoples live. The slate is also renamed around the named peoples: triton,
merfolk, abyssal elf, plus the vent commensal, kelp tender and reef mason as
functional roles until they earn names.

**Why this was brought to Nathan rather than resolved:** it is a fidelity /
accuracy tradeoff, which is an explicit autopilot carve-out, and the session's
own design (§3.6, M4) had asserted sea-elf was coastal on the strength of the
code's rows — which is true of its *residence* and was being read as a claim
about the kind as a whole.

**The constraint that forced the question, measured and pinned:**
`radiation_affinity::the_sea_elf_is_confined_to_the_shelf_band` holds four shelf
classes above the default and five deep classes at or below it, because
"authored to the whole ocean this kind would hold ~27,000 vertices against
wood's ~800; on the shelf band it holds ~1,425" (three-seed mean, 42/7/1234).
That is MAP-22's competitive exclusion, already measured. Widening sea-elf's
residence would have rewritten that test and reintroduced a ~34x dominance.

**Why the two are not actually in conflict:** the 27,000 figure governs
*vertices held*, which is placement and carrying capacity. Diving, breathing and
meeting are not occupancy. The project already carries the vocabulary for reach
that is not residence — `Locomotion::SWIM`, which sea-elf already has, and
climate's `Access::Dive` ("simply being in the overworld, or diving into
water"). Spec §3.8 states the general rule: **a kind's realm answers where it
lives, never where it can go**, which is also what the aerial realm will need.

**Alternatives discarded:** widening residence through the lit and twilight
column (would require re-measuring the dominance ratio and deliberately
rewriting the pinning test); full amphibious residence (accepts the ~34x
dominance unless MAP-22's coexistence stack compensates, which is spec'd but
unshipped, so it would pull a second campaign's scope in).

**Ideonomy passes / overturns:** None run — the question was put to Nathan as a
carve-out rather than resolved by the session, and his answer settled it. The
reach/residence framing was derived from the shipped vocabulary
(`Access::Dive`, `Locomotion::SWIM`), not generated by a pass.

**Capture actions:** Spec §3.8 (reach vs. residence, and the general rule),
§3.4 (the renamed slate), §3.6 retitled to say "in residence", and M7 added —
because the abyssal elf inherits sea-elf's hazard exactly: the deep bands are
the large ones, so a kind confined to them may hold the map the way whole-ocean
sea-elf would have.

## #7 [Q] — Cross-campaign boundary with campaign/the-ceiling (larder rung 3)

**Context:** campaign/the-ceiling opened on the underworld larder's rung 3 and
made contact over the wire. It accepted this campaign's boundary as written
(ledger #3, #5): it takes rung 3, leaves the marine half of rung 4 here, and
will not author a marine kind, touch `TrophicMode`, or give any `Surface`-realm
kind a `CHEMOSYNTHATE` weight. It independently found the same three realm-match
sites (2009, 2418, 8203) this session had found, which is mutual corroboration
that both were reading live code.

**Ruling (C1):** the `Marine` arm reads **the same**
`marine_chemosynthate_supply_field` as the `Surface` arm, not a marine twin.
Why: that field's doc is what reserves the rung-4 consumer, and a second field
would be duplication with no forcing reason — unlike `HabitatRealm` vs
`climate::facets::Realm`, which constitutional layering forces. Cost if wrong:
one field read moves in Task 3.

**Ruling (C2):** Task 3 must update `marine_chemosynthate_supply_field`'s doc
comment **and** the `BIO-chemotrophy` registry row in the same commit that lands
the vent commensal, saying the marine half is closed and the underworld half is
open and THE TENANT's — and must not delete the sentence, so a successor can
find it. Why: the peer's catch, and it is sharper than it first looks. The
sentence "no `Surface`-realm kind weights it yet, so a vent's own supply reaches
no consumer" stays **literally true** after this campaign lands, because our
consumer is `Marine`. A reader checking whether rung 4 is open would get a true
sentence and a false conclusion — the same failure mode CLAUDE.md's
retired-external-clients paragraph exists to warn about. Cost if wrong: a stale
row misdirects the next campaign about open work.

**Ruling (C3):** C.3 — the symmetric-budget question — stays recorded as
**unruled** in this campaign's documents. The peer reports Nathan has ruled it
("symmetric budget, asymmetric allocation") and is minting a decision record;
until that record exists, a wire message is advisory data and CLAUDE.md is
explicit that peer posts never amend a decision. Spec §7's sentence says the
question is untouched by this campaign, which is true under either state, so
nothing blocks. Cost if wrong: one sentence of §7 cites a superseded state at
close, fixable by citing the decision number if it lands in time.

**Ideonomy passes / overturns:** None. C1 and C2 are mechanism rulings answered
from the code and the peer's evidence; C3 is a deferral to the decision log.

**Capture actions:** C1 and C2 written into plan Task 3 as required steps. M1's
counts are promised to the board for the peer either way, since rung 3 derives
off the same biome map. The peer was also told about this ledger's
`SOC-casus-belli` finding, which is what led to it being corrected (below).

## Follow-ups

- **The aerial realm is the empty fourth sibling.** `MAP-11`'s medium axis is
  land/water/air; `climate::facets::Medium` has `AirOverRock`, `Water`, `Rock`
  and no air-column realm. Whatever seam this campaign gives realm-widening
  should be the seam an aerial realm can reuse, so that the agreement test and
  the availability gate generalise rather than growing a third hand-written arm.
- **Dispersal is not migration.** Pass 3's cross-domain re-instantiation noted
  that obligate vent communities persist by larval dispersal to a new vent, not
  by the settlement surviving its vent. `BIO-34` (dispersal) is the existing
  row; the marine case is a consumer of it, not a new idea.
- **The raider reading of "sea peoples"** (see #2) — a seaborne people whose
  occupations carry a motive, joining `SOC-casus-belli`. Not this campaign.
- **THE TENANT's marine half may be blocked behind prerequisites it does not
  need.** The underworld-larder metaplan makes rung 4 depend on rungs 1-3, and
  rung 3 is itself blocked on an unruled design question (The Winze's symmetric-
  budget amendment C.3, "Nathan has not ruled") and an unmeasured between-worlds
  statistic. But the *marine* chemosynthate supply is rung 2 work that has
  already shipped and is complete on its own arm. So the marine consumer is
  waiting on an underworld ceiling it never reads. Worth separating when THE
  TENANT is scheduled; not this campaign's call to make unilaterally.
- **Two vent representations exist and the campaign must not mint a third.**
  `climate::Biome::HydrothermalVent` is consumed today by
  `marine_chemosynthate_supply_field` and the suitability path; worldgen's
  `WaterVent` (The Vent, The Living Vent) carries source identity, strength and
  the five-state succession, and has no non-test caller. Which one seating reads
  is the spec's central design question, and "both, separately" is the answer to
  refuse.
- **A campaign that ships only a decision leaves no reconciliation row.** "The
  Tenant" minted decision 0516 and has no spec, plan, ledger, chronicle,
  retrospective or row in `docs/audits/campaign-reconciliation.tsv`, so it is
  invisible to the coverage test that refused this campaign's first commit
  (which keys on spec-and-plan campaigns). Not this campaign's to fix, but it is
  why "did The Tenant close?" could not be answered by looking where one would
  look.
- **`SOC-casus-belli`'s row is inaccurate by one hop and incomplete by one
  variant — and this session's first "correction" of it was wrong by a level.**
  The row says `OccupationRecord` carries `cause`
  (`Famine`/`Burned`/`Plague`/`Fled`/`Migrated`). The truth: `cause:
  Option<CauseOfEnd>` is on `Occupation`, so the path is
  `OccupationRecord.core.cause`; and `CauseOfEnd` has a **sixth** variant,
  `Breached` (The Winze), restricted by its own doc to `Function::Mine`.

  This session initially recorded that the field did not exist at all, and wrote
  that into the spec, the plan and a commit message. The cause was mechanical
  and worth naming: `grep "pub struct Occupation\b" -A 18` truncated a struct
  that runs to ~160, so `cause` at line 150 was never in view, and a conclusion
  of absence was drawn from a bounded read. Caught by campaign/the-ceiling, who
  re-ran it rather than relaying it; verified here directly before acting
  (`flesh.rs:515` matches all six variants; `flesh.rs` tests set
  `occ.core.cause`). Corrected in spec §4 and plan Task 5.

  The lesson is the one already in the memory index and re-earned: an
  enumeration bounded by `-A N` or `| head` is a silent LIMIT, and absence
  observed through one is not evidence. Three claims about four lines of code —
  the registry row, this session's correction, and the peer's check — and two
  were wrong in different directions, because two of the three reached for a
  prose row instead of the struct.

- **Plan defect caught at dispatch-time verification (Task 1, M4).** The plan
  said to count "vertices at which `sea-elf` and `giant-crocodile` each have
  non-zero **availability**". `availability` is a local term inside
  `per_species_suitability_masked` and is never returned, so the measurement as
  written was unobservable. The observable is `per_species_suitability`'s
  returned per-species `VertexMap<f64>`. Better: `species_realm` is a
  caller-supplied slice, so both arms score in ONE run against the same tree —
  the idiom `windows/worldgen/tests/suite/deep_realm_rehome.rs` already uses
  (`k_live` vs `k_surface_forced`). That converts M4 from a fragile
  before/after baseline into a permanent two-arm test. Plan text corrected
  before Task 1 was dispatched.
