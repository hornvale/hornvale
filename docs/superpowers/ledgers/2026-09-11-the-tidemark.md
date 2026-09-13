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

## Task 1 — execution record

Commit `d6775158c` ("feat(the-tidemark): add the marine habitat realm"),
branch `campaign/the-tidemark`. This section is the committed home for the
three records the brief asked to be kept "in the ledger" — they had been
written only into per-worktree scratch (`.superpowers/sdd/2026-09-11-
the-tidemark/task-1-report.md`), which is git-ignored and dies with the
worktree; folded in here at review round 1's request (IMPORTANT #2).

**The compiler-produced site list**, obtained by adding `HabitatRealm::Marine`
and running `cargo build --workspace --all-targets` repeatedly, fixing one
reported site at a time, never grepping ahead (`substrate_response`'s own
doc promises the `match` is exhaustive with no wildcard, so the compiler
enumerates the sites rather than the spec or the implementer guessing at
them):

1. `domains/species/src/lib.rs:4774` — `substrate_response`'s `match realm`
   — the real curve, authored per pre-flight ruling P1's companion
   resolution (a).
2. `windows/worldgen/src/lib.rs:2008` — `per_species_suitability_masked`'s
   `let (best, availability) = match realm`.
3. `windows/worldgen/src/lib.rs:2417` — the identically-shaped match inside
   `per_species_capacity_at_with_invariant` (the dimensional capacity twin
   of site 2; not named in ruling P1's text but the same architectural
   pattern, treated identically).
4. `windows/worldgen/src/lib.rs:8202` (pre-fmt line number) — the
   `seatings: Vec<crate::delve_seating::Seating>` build's
   `match species_realm[i]` — delve-seating / pelagic-ladder assignment,
   explicitly Task 2's scope per the pre-flight conflict table ("marine
   availability mask + pelagic seating").
5. `windows/worldgen/tests/suite/hidage_probe.rs:373` — a test-fixture copy
   of site 4's seating match (decision 0092's test-fixture posture: copied,
   not shared).
6. `windows/worldgen/tests/suite/hidage_probe.rs:425` — a second match in
   the same file, over `pf.realm`, restricted to ALIVE occupations.
7. `windows/worldgen/tests/suite/staple_d3b_probe.rs:690` — a third copy of
   site 4's seating match.

Site 1 is `domains/species`, which compiles before `hornvale-worldgen`; the
other six surfaced only once site 1 was fixed and the dependent crate could
compile again.

**M4's counts** (`windows/worldgen/tests/suite/marine_realm_zero_movement.rs`,
seed 42, `BuildDepth::Terrain`, 40962 total vertices), including the
positive control added at review round 1 (IMPORTANT #1):

| kind | live non-zero | surface-forced non-zero | equal? |
|---|---:|---:|---|
| drow (positive control — Subterranean live, forced Surface) | 874 | 11283 | **no** (required) |
| sea-elf | 40799 | 40799 | yes |
| giant-crocodile | 40799 | 40799 | yes |

sea-elf and giant-crocodile are additionally asserted bit-identical
(`f64::to_bits()`) at every one of the 40962 vertices, not merely
count-equal. The drow row is what makes the sea-elf/giant-crocodile
equalities a real finding rather than an arithmetic certainty: without a
kind whose live and forced realms genuinely differ, a bug that made
`per_species_suitability` ignore `species_realm` entirely would leave the
whole file green.

**`make gate-commit`'s exit code and duration**, both runs:

- Standalone run before committing (after fixing the sub-floor
  `world_build_sites` roster gap): `rc=0`, `wall=44.976s`.
- The commit's own `pre-commit` hook re-running the full gate as part of
  `git commit`: `rc=0`, `wall=41.892s`.

**Review round 1 (spec: narrow; quality: approved-with-findings, 0
Critical / 4 Important).** Findings and dispositions: M4 lacked a positive
control (fixed — drow row above); this record was scratch-only, not ledgered
(fixed — this section); `habitat_realm_registry`'s doc comment asserted
"only kinds that are not `Surface` appear" and `HabitatRealm`'s own doc
comment said the placement layer needs "a two-valued question", both made
false by this commit's own two `Surface` rows and third variant (fixed in
`domains/species/src/lib.rs`); the three new `substrate_response` constants
had no assertion pinning the argued devotion ordering
(fixed — extended `domains/species/tests/suite/coverage.rs`'s
`the_two_realms_order_hardness_oppositely`); and `HabitatRealm::Marine`'s
own doc comment described an availability gate ("`1.0` where the vertex is
wet, `0.0` otherwise") that no code implements yet (fixed — the doc now
names Task 2, matching the mask arms it describes).

## Task 2 — execution record

Branch `campaign/the-tidemark`. Wires the Waterworld overlay into the
deep-history bake, makes both marine availability arms real, and seats a
`Marine` kind on the pelagic ladder.

### M1 — do the two vent representations coincide?

Measured **before any seating code was written**, by
`windows/worldgen/tests/suite/marine_vent_representation_probe.rs` (an
`#[ignore]`d probe: three terrain-depth world builds). `|A|` is the count of
vertices whose climate biome is `HydrothermalVent`; `|B|` the count hosting a
worldgen `WaterVent`; `|A ∩ B|` the vertices carrying both.

| seed | `|HydrothermalVent|` | `|WaterVent|` | `|A ∩ B|` | smaller set | overlap |
|---|---:|---:|---:|---:|---:|
| 42 | 684 | 623 | 174 | 623 | 27.9% |
| 7 | 802 | 585 | 175 | 585 | 29.9% |
| 3 | 587 | 509 | 150 | 509 | 29.5% |

**The prediction holds on all three seeds**: the intersection is well under
half the smaller set (27.9–29.9%, against a preregistered ceiling of 50%).
The two are different phenomena, exactly as spec §8 argued — one is derived
straight from `SeafloorFeature::Ridge` with no draw, the other a seeded
admission over seabed vertices that carry an edifice or a plate boundary.

**Ruling — the authoritative representation for seating is `WaterVent`.** It
is the only one of the two carrying a succession phase, which is what spec
§4's expiring habitat needs. `Biome::HydrothermalVent` is unchanged and keeps
its existing consumer (`marine_chemosynthate_supply_field`, read on the
`Surface` arm); the campaign records it as the coarser, ridge-derived thing
and mints no third representation. The spec's large-overlap branch — "stop
and report it as the headline" — did not fire.

### The instant placement reads: `WorldTime::GENESIS`

Named at the call in `bake_history_from`, and in `MarineHabitat::at_instant`'s
own doc. Two reasons, in order of weight:

1. **The overlay already names genesis for its own ambient fields**
   (`ambient_marine_fields`, extracted from `waterworld_from`, which has
   always derived its `fields` at `WorldTime::GENESIS`). Reading the
   succession at a different tick would put two instants inside one
   overlay — ambient temperature at genesis, vent temperature elsewhere — and
   nothing downstream could say which world it was looking at.
2. Placement at the `Settlements` rung is a genesis-time act. A habitat
   *expiring* over world-time is spec §4's concern and a later task's: it
   moves the habitat off the era-invariant hoist rather than moving this
   instant.

The read consumes no draw. That is checked rather than asserted:
`marine_ladder_vents::vent_admission_stays_keyed_to_its_own_vertex`
re-derives every admitted vent's four values from
`seed.derive(WATERWORLD_VENT).derive("vertex/<n>")` **alone** and requires
them bit-identical to what the overlay built — which can only hold if the
per-vertex sub-stream is the only consumer of that key. No stream label was
added and no draw order changed.

### Where the overlay is constructed, and why not one frame up

Inside `bake_history_from`, not in the `Settlements` rung's closure that
calls it. The closure was the brief's stated insertion point, and the
deciding fact against it is `history_for`: the standalone measurement entry
point routes through `bake_history_from` **precisely so** its output stays
byte-identical to the settlement stage's own bake. An overlay built in the
closure would have to reach `history_for` as `None`, and the two bakes would
then silently disagree about whether the sea has vents in it.

`WaterWorldConfig { enabled }` **survives as a knob**, passed `true`
unconditionally. The alternative ("build only when the world has marine
vertices") is already performed by `waterworld_from` itself — a world with no
ocean vertex yields an empty `WaterWorld` and `WaterWorld::at` short-circuits
— so an `enabled` gate at the call site would be a coarser second copy of
that test. What the flag buys that the emptiness check cannot is the
**ablation seam**: `enabled: false` is the only way to ask what a world looks
like with the overlay withheld, which is the control
`an_ablated_overlay_leaves_the_marine_ladder_exactly_where_it_was` needs.

### The seating, and the two things that changed shape

`MarineHabitat` (`windows/worldgen/src/marine_habitat.rs`) is
`subterranean_substrate_field_per_rung`'s marine sibling: per vertex, one
`Option<Substrate>` and one chemosynthate value per band of
`Realm::WATERWORLD.strata()`. Two constructors over one column walk
(`waterworld::marine_columns`, extracted from `waterworld_from` so the two
cannot disagree): `ambient` (pure, no draws, no vents — what the readout path
`per_species_suitability_masked` hoists for itself) and `at_instant` (the
vent-bearing reading, hoisted once per bake on
`EraInvariantSupply::build_at`). Both arms score every band and take the
best; `availability` is `1.0` where some band scored and `0.0` where none
did, outside the Liebig minimum exactly as the cave mask is.

**`delve_seating`'s `Marine` arm moved from multiplier `0.0` to
`Seating::all_surface`.** Task 1's `0.0` was correct while no ladder existed;
leaving it would have multiplied the whole pelagic ladder away and shipped
Task 3's peoples unplaceable. `Seating` prices a *rock chamber* — its `rung`
is a `hornvale_kernel::Band`, in which no pelagic stratum is expressible — so
a marine people has nothing for that map to discount, takes multiplier
exactly `1.0`, and sits at `Band::Surface`, the one band meaning "not in the
rock column". Unobservable today (no kind is `Marine`); Task 3 is where it
first has an effect, and is the right place to review it.

### Proving the ladder is not vacuous

`windows/worldgen/tests/suite/marine_ladder_vents.rs` scores one frozen kind
twice over one frozen world, through the production entry point placement
calls (`per_species_capacity_at`), differing only in whether the hoist
carries the vent-bearing habitat. At seed 42: 623 vents, the vent layer moves
**392** `(vertex, band)` habitat slots at genesis, a `Marine` kind reaches
**29,679 of 40,962** vertices, and the vent layer moves **376** of them. The
same kind on the `Surface` arm is bit-identical across the two hoists.

**Two source mutations, each with its target text asserted present before
being perturbed, confirm the two halves of the test are independently live:**

| mutation | precondition | downstream |
|---|---|---|
| zero the vent contribution in `WaterWorld::at` (temperature **and** chemistry) | **RED** — habitat slots moved 392 → 0 | (masked) |
| make the `Marine` capacity arm score the surface substrate instead of the band | green — 392 slots still move | **RED** — capacity moved 376 → 0 |

Both restored; `grep -c -F` on the restored text confirms it.

**The probe kind is synthesised, and that is itself a finding.** No shipped
kind can score in the water column at all: `human` forced to `Marine` yields
capacity exactly `0.0` at all 40,962 vertices, because its elevation curve is
authored for land (`ConditionResponse::eval` underflows kilometres below sea
level) and its resource weights read axes the sea does not supply. Task 3's
kinds must carry a sub-sea-level elevation optimum and `MARINE_FORAGE` /
`CHEMOSYNTHATE` weights, or they will place nowhere and M2 will read zero for
a reason that is not the realm gate.

### No existing world moved

`bash scripts/regenerate-artifacts.sh` then the drift check over
`docs/generated-paths.txt` moved **only** the two audit bookkeeping reports —
`docs/audits/type-audit-report.md` (two per-class counts and the worldgen row,
from the new `pub` items) and `docs/audits/plumb-roster.md` (the file and
constant counts, from the two new constants). The almanacs, the elevation
map, the registry/manifest dumps, the lab studies, the Domesday survey and
the client fixtures are all byte-identical. `cargo nextest run -p
hornvale-worldgen --no-fail-fast`: **1172 passed, 0 failed** (641.2 s),
including `history_byte_identity` and `graph_byte_identity`, which are the
build-twice-byte-identical assertions this change had to survive.

### Findings carried forward

- **The five strata are distinguished at placement by DEPTH ALONE**, except
  the seabed band at a vertex a vent is lighting. This entry's first draft
  said "by depth, temperature and chemosynthate", which was right about
  which inputs *reach* the arm and wrong about which of them *vary by band*
  — two different questions, and conflating them is what spec §3.4's
  corrected table now separates. Verified in the tree at fix round 1:
  `WaterFields::from_substrate` is handed one
  `climate.temperature_at(vertex, time)` for every sample of a vertex, so
  the column is one temperature; ambient `chemistry` is
  `has_edifice ? 1.0 : 0.0`, a per-vertex terrain property, and
  `WaterWorld::at` folds a vent's chemistry in at `seabed_sample_index`
  alone; non-seabed `height_asl_m` is `-band_entry_depth_m(band)`, the
  global constants {0, 200, 1000, 4000, 6000} m, identical at every ocean
  vertex. `insolation` does vary by band and **nothing reads it**.
  `moisture` is a constant.
- **Threading light through would add no axis**, and was refused rather than
  deferred. The light reaching the readout is `climate.insolation()` — a
  world scalar — attenuated by `exp(-depth/1000)`; it carries no latitude,
  so it is a deterministic function of depth and duplicates the axis the
  ladder already has. The gap is not that light is missing; it is that
  nothing in the pelagic column varies per vertex except the seabed depth.
- **The marine tolerance substrate is frozen at genesis across every
  paleoclimate era**, and that is a ruled hoist rather than an invariance.
  `marine_habitat` rides `EraInvariantSupply`; its two siblings do not —
  `substrate_field_at` and `subterranean_substrate_field_per_rung` are
  rebuilt inside the era loop with that era's temperature offset and
  sea-level re-datum. This is the first realm *tolerance substrate*, not
  merely a supply field, to be hoisted this way. **Ruling (fix round 1):
  leave it hoisted.** The campaign's headline is vent-driven expiry, which
  reaches placement through `WaterWorld::at` regardless of the era loop, so
  M3 is unaffected; what is deferred is climate-driven marine change, and a
  per-era rebuild is a different campaign's cost. Recorded as a limitation
  in spec §3.4 and in the field's own doc, which previously defended the
  hoist circularly ("cannot vary across a series that names no instants of
  its own" — true only because this code chose not to recompute it).
- `per_species_suitability_masked` builds the **ambient** habitat, so a
  `Marine` kind scored through the readout path sees no vents. That
  asymmetry is deliberate — the readout path holds no seed and cannot build
  the overlay without a draw — but any measurement that compares readout
  against placement for a marine kind must account for it.

### Fix round 1 (review: spec pass, quality approved-with-findings, 0 Critical / 4 Important)

M1's counts reproduced exactly under review, the determinism promise was
independently re-proved, and the departure from the brief's stated insertion
point (building the overlay inside `bake_history_from` rather than in the
`Settlements` closure) was verified correct and endorsed. Four findings, all
addressed:

- **I3 — the band-variation correction above**, applied to every doc comment
  that stated or implied otherwise: `marine_habitat`'s module doc gains a
  "what actually varies by band" section, both `MarineHabitat` field docs say
  it, the `insolation` assignment says outright that it is populated and read
  by nothing, and both realm arms in `lib.rs` say what their `max` is really
  ranking on.
- **I1 — a cited test that did not exist.** Two doc comments named
  `the_marine_habitat_read_consumes_no_draw` and `grep` found the name only
  in those two comments: an assumption wearing a verification's clothes. The
  test is now written. It holds what is actually holdable from inside the
  process — reading genesis, then another instant, then genesis again returns
  the same genesis answer bit for bit, and the overlay is unchanged across
  all three — and its own doc says plainly what it does **not** hold (the
  absence of a draw, which is carried by `WaterWorld::at`'s signature taking
  no `Seed`, and by the empty regen drift check).
- **I4 — the wiring this task exists to create was unguarded.** Reverting
  `bake_history_from`'s `EraInvariantSupply::build_at` to `build` left the
  entire suite green. The honest outcome test was **measured and found
  impossible**: two synthetic marine probes run through the real bake at
  seed 42 (a CHEMOSYNTHATE-only chemotroph whose habitable set grows 403 →
  740 under the vent layer, and a thermophile the vent layer improves at 390
  vertices, peak capacity 61.05 → 104.83) each produced 4 occupations, and in
  neither case did a single site sit on a vent-improved vertex — the bake
  seeds an ancient world and marches epochs rather than taking a capacity
  argmax, so a capacity field that moves at hundreds of vertices need not
  move one site. So the property is guarded where it lives, in the call:
  `the_bake_hoists_the_vent_bearing_marine_habitat` scans
  `bake_history_from`'s own body (bounded to that item, both needles asserted
  present) for the overlay construction, `build_at`, and the named instant.
  Verified to redden on exactly the named reversion, with every other test in
  the file still green.
- **The promoted minor — an unmeasured mechanism claim** in
  `marine_realm_zero_movement.rs`'s header, which asserted a
  reclassification "still moves thousands of vertices" without measuring it.
  Now measured by a third arm in that test: sea-elf and giant-crocodile each
  score 29,679 forced `Marine` against 40,799 live, a margin of **11,120
  vertices**, printed every run and asserted as a relationship rather than
  pinned to a literal. Neither "the whole globe" (the pre-Task-2 claim) nor
  "zero everywhere" (the plausible guess that motivated measuring).

### Gate

`make gate-commit`: **rc=0, wall=45.364 s** (user 84.210 s, sys 20.303 s,
cpu_ratio 2.30; 462 sub-floor tests, 4 chunks, all green).

Four gate checks had to be satisfied by hand before it went green, recorded
because each is a step a reader would otherwise have to rediscover: the new
`build_at` needed `#[allow(clippy::too_many_arguments)]` (8/7); `PELAGIC_BANDS`,
`pelagic_index`'s return and `MarineHabitat::chemosynthate` needed
`type-audit:` verdicts (the field's tag belongs on the STRUCT doc, not the
field's own — a field-level tag is not read); both audit reports had to be
regenerated; the M1 probe's seed loop needed a `/// claim: readout(...)` line
(decision 0093's claim-shape lint); and both new test files needed rows in
`cli/tests/fixtures/world-build-sites.tsv` (`artifacts:1` each — both need a
live `GeneratedTerrain`/`GeneratedClimate`, which the committed seed-42
fixture does not carry).

## Task 3 — execution record

The six obligate marine peoples: `triton`, `merfolk`, `abyssal-elf`,
`vent-commensal`, `kelp-tender`, `reef-mason`. Spec §3.4's slate, authored
against the arriving-axis table rather than against the shape of an existing
kind.

### M5 — pairwise distinctness (the campaign's headline guard)

A permanent test, not a measurement:
`domains/species/tests/suite/marine_distinctness.rs::no_pair_of_marine_kinds_differs_only_by_stratum`.

**Minimum over all 15 pairs: 5, at (`reef-mason`, `triton`).** Full table
printed on every run. Per-pair counts: 11, 9, 6, 6, 9, 10, 9, 9, 9, 7, 7, 10,
5, 9, 10.

Four axes are EXCLUDED, one more than the spec asks for, and the extra one is
a finding:

- `elevation` — this *is* stratum;
- `moisture`, `insolation` — spec §3.4's table marks both as not arriving;
- **`temperature` — the table marks it ARRIVING, which is true of the plumbing
  and false of the effect.** `tolerance_liebig` returns early on
  `elevation <= floor_buf`, and elevation is the only UNFLOORED axis, so its
  value cannot exceed its own `devotion`. Every people the roster ships is
  authored PREPARED (`devotion_elev` 0.30 against sovereignty floors of
  0.42–0.45), so the fast path fires at every vertex and temperature, moisture
  and insolation are computed and discarded. `sea_elf_condition_niche` has said
  "PREPARED: never binds" since The Radiation; what was not stated is the
  consequence for M5.

So the axis that actually differentiates six marine peoples per-vertex is
`biome_affinity` — a graded factor resolved by biome NAME, multiplied outside
the Liebig minimum. All six carry rows; the store goes 8 → 14.

### M2 — placement, two-sided, seed 42, `BuildDepth::Full`

`windows/worldgen/tests/suite/marine_peoples_placement.rs::m2_the_settled_marine_peoples_place_and_the_gregarious_one_does_not`.

| kind | settlements |
|---|---:|
| abyssal-elf | 3 |
| kelp-tender | 4 |
| reef-mason | 4 |
| triton | 3 |
| vent-commensal | 3 |
| **merfolk** | **0** |

Surface + subterranean total: **368**. Marine total: **17** (4.4%). Both poles
green: every `Settled` marine kind places ≥ 1 and far under the surface total,
and `merfolk` places exactly 0 — `SocialForm` reaches placement.

And each lands in the biome its affinity names, which is the design working
rather than a coincidence (`book/src/gallery/settlement-seed-42.md`): abyssal
elf on `bathypelagic`, kelp tender on `upwelling`, reef mason on `coral-reef`,
triton on `upwelling`, vent commensal on **`hydrothermal-vent`**.

### M7 — the abyssal elf's held vertices

Same file, `m7_the_abyssal_elf_does_not_dominate_the_map`. "Held" = the strict
argmax of `per_species_capacity` over the settling roster, and the instrument
is stated because the spec's ~800/~1,425 figures were taken with a different
one — a NON-comparative count cannot answer M7 at all, because a marine kind
reads non-zero at every vertex holding a water column (~29,700 of 40,962)
whatever its elevation optimum. So the family's own anchors are re-measured in
the same run.

| seed | abyssal-elf | sea-elf | wood-elf | drow |
|---|---:|---:|---:|---:|
| 42 | 336 | 182 | 0 | 18 |
| 7 | 368 | 443 | 0 | 295 |
| 1234 | 522 | 19 | 0 | 264 |
| **mean** | **408.7** | **214.7** | **0** | — |

**No domination: the abyssal elf is 1.9x its marine sibling's mean and the same
order as the family**, against a 10x ceiling. Band confinement was applied
BEFORE the measurement rather than as a remedy after it
(`the_abyssal_elf_is_confined_to_the_deep_bands` pins the mirror of
`the_sea_elf_is_confined_to_the_shelf_band`, and asserts the two rows
PARTITION the ocean).

`wood-elf` holds zero under this instrument at all three seeds — it is never
the strict argmax anywhere. A fact about the instrument, not about wood-elf,
and the reason M7's anchor is `max(wood, sea)` rather than `min`.

### W5 — the kelp tender's `PHOTOSYNTHATE` weight: the NULL

`w5_the_kelp_tenders_photosynthate_weight_is_measured_not_assumed`, ablating
the weight against the same marine weight so only the photosynthate term
changes: **the weight moves capacity at 0 of 29,679 scoring vertices, largest
absolute move 0.** `PHOTOSYNTHATE` is fed by `base_carrying`, the terrestrial
NPP field, which is exactly `0.0` at every submerged vertex.

Kept, not deleted: the weight is the honest statement of what the kind eats on
the axis the kernel reserves for it, and what is missing is a marine primary-
production supply. The phototrophy itself is NOT inert — `TrophicMode` reaches
`prey_pressure_from`, which excludes a phototroph from the prey base.

### I4 — the wiring guard is now BEHAVIOURAL, and the scan is narrowed

The brief asked whether an outcome test is possible now that real marine kinds
exist. It is, and it fires:

**All 3 of the vent commensal's seed-42 sites sit on vent-improved ground** —
capacity 5.75–5.88 ambient against 36.6–38.1 with the vent layer, a 6.3x lift
— while the vent layer improves only 337 of 40,962 vertices (0.82%), so three
sites landing there by chance is ~6e-6.

Verified by mutation (target text `grep -c -F`-checked present first):
reverting `EraInvariantSupply::build_at` → `build` inside `bake_history_from`
takes it from 3 of 3 to **0 of 3** and relocates all three vertices
(4229/7053/7954 → 29907/29909/36658). Restored and re-checked.

`the_bake_hoists_the_vent_bearing_marine_habitat` is KEPT, narrowed to the two
needles no outcome test holds: that the overlay is constructed at all, and that
the instant is named `GENESIS` (a vent lit at another tick may light the same
vertices). Both docs cross-reference each other.

### The C2 obligation, and a correction to how it was framed

Both prose sites are rewritten in the same commit as the vent commensal. But
the brief's framing — "the vent commensal reads **the same**
`marine_chemosynthate_supply_field` the `Surface` arm reads" — is not what the
plumbing does, and writing it that way would have made the correction false:

- the `Marine` arm feeds `CHEMOSYNTHATE` from **`MarineHabitat::chemosynthate`**
  (Task 2's per-band field, the marine sibling of `chemosynthate_per_rung`);
- `marine_chemosynthate_supply_field` is read on the `Surface` arm and on the
  `Marine` arm's dry-vertex fallback, which `availability = 0.0` multiplies
  away.

So the substantive instruction is honoured — **no marine twin supply field was
authored** — and both sites now say exactly that: the marine half of rung 4 is
CLOSED, this field still has no live consumer, and the underworld half is open
and belongs to THE TENANT. The old sentence is kept, not deleted, because a
successor needs to find it.

### What else the six required

`coverage.rs`'s exhaustive tables were the enumeration, as ruled. Beyond them:
`biosphere`/`habitat_realm`/`locomotion`/`fatigue_rise`/`sleep_grade`/`psyche`/
`dispersion`/`society`/`perception`/`family_of`/`KIND_CONCEPTS`/
`biome_affinity`/`pathogen_hosts`; `articulation_registry` and
`lexicon_registry` in `domains/language`; a new accession cohort, appended
last (one cohort, not six — and NOT folded into the elf family's own cohort
despite one member being an elf, because the rule is about arrival, not
descent). Its epoch INDEX is deliberately not stated here or at the cohort:
`campaign/underworld-peoples` appends one too and both claimed the same
number, so whichever merges second takes the next. Nothing breaks either way
— `concept_epoch` reads the position and no index is hardcoded — but a
comment asserting a number it cannot guarantee would have become false at a
merge nobody had done yet.

Registry counts moved: biosphere 39 → 45, realm store 5 → 11, `Settled` 15 →
20, `society_registry` 15 → **21** (merfolk), psyche/perception 18 → 24,
pathogen hosts 15 → 20, biome affinity 8 → 14.

### Three vertices this cohort promoted, each a deliberate edit

- **`SocialForm::Gregarious` × minded** — decision 0068's own reason for
  existing, shipped with zero instances and left "deliberately dark" by The
  Vacancy because settlement-free peoples were unaudited downstream. `merfolk`
  is the occupant, and M2 is that blocker turned into a two-sided measurement
  rather than waved.
- **`TrophicMode::Chemotrophic`** — a second carrier (`vent-commensal`), which
  `chemotrophic_is_declared_and_unwitnessed` explicitly predicted and asked to
  be justified rather than silently widened. `SANCTIONED` gains
  `(Ectothermic, Chemotrophic)`.
- **`ThermalStrategy::Unmodelled`** — a fourth witness (`kelp-tender`) and its
  first PEOPLE, which makes `autotroph_is_computed_as_an_endotherm_today`'s
  known divergence newly load-bearing for a settlement's demography.

### Two authoring constraints discovered by being violated

- **`in_group_radius` is a BANDED axis, not a free scalar.**
  `windows/vessel::housemark` partitions it into three inclusive bands with
  deliberate gaps (0.36–0.49 and 0.61–0.64 REFUSE), and an authored 0.45
  panicked the housemark roster. Reef mason 0.45 → 0.35, triton 0.4 → 0.5,
  vent commensal 0.5 → 0.55.
- **`night_vision` is QUANTIZED by the eye model.** `beholding_probe`
  requires two kinds with different values to render different swatches, and
  an authored 0.45 against goblin's 0.5 produced one identical eye. All six now
  snap to values the roster already distinguishes — which is the campaign's own
  M5 discipline arriving on a second axis: a difference finer than the model
  can render is a distinction the engine cannot see.

### A latent defect this cohort made reachable

`windows/vessel::focalize::render` renames a surface reading of a marine biome
to "open water" in the PROSE and went on naming the noun by the raw class, so
`look` printed a place the prose never mentioned. Unreachable for the life of
that code — nothing put an observer on a marine vertex until now. Fixed
(display is the renamed place; the class stays a typeable alias), and both
tests that looked the noun up by display now look it up by handle.

### Determinism and drift

Authoring kinds moves worlds, as the brief expected. `make rebaseline` +
`make rebaseline-goldens` run; the seed-42 committed fact count moves
20,109 → 22,835; 24 new phonology audio clips authored with `hornvale voice`;
the history showcase repointed 10626 → 9884 (10626 renders an EMPTY column
now, which `docs_consistency` caught). **No new stream label, no new external
dependency, no epoch.**

### M8 — the shallow kinds hold the ocean, and depth confines DOWNWARD ONLY

Found after the slate was authored, from M7's own printed distribution rather
than from a probe written to look for it. **Not retuned** — the controller's
ruling, and the right one: it was found post-unblinding, and M2 cannot see it.

Held vertices (strict argmax of per-species capacity over the settling
roster), seeds 42 / 7 / 1234:

| kind | 42 | 7 | 1234 |
|---|---:|---:|---:|
| **reef-mason** | **26,344** | **18,213** | **26,769** |
| hobgoblin | 5,996 | 7,937 | 4,487 |
| triton | 2,164 | 1,952 | 1,048 |
| kelp-tender | 276 | 430 | 524 |
| abyssal-elf | 336 | 368 | 522 |
| vent-commensal | 377 | 224 | 396 |

The reef mason is the argmax across very nearly the whole ocean (~29,700
water-column vertices at seed 42), while placing a normal **4** settlements.

**The mechanism, stated because the number alone invites the wrong repair.**

1. Every ocean column has an `Epipelagic` band at `height_asl_m = 0`
   (`marine_columns` walks `strata()` from the surface down to the vertex's
   own floor). A kind whose elevation optimum is near the surface is
   therefore near-optimal at the surface band of EVERY ocean vertex, however
   narrow its curve: at optimum −40 m and width 350 m, `z = 0.114` and the
   bump is 0.993.
2. So **depth confines downward only.** A deep kind is confined because deep
   columns are rarer — that is what makes M7's abyssal elf well-behaved. A
   shallow kind is confined by nothing.
3. Among shallow kinds the argmax then falls to the largest remaining term.
   `BiomeAffinity`'s `default` IS the kind's sovereignty floor, so the reef
   mason's 96 kg (floor 0.4448) beats the triton's 82 kg (0.443) at every
   unlisted class, by ~0.4%, and both weight `MARINE_FORAGE` at 1.0. A
   0.4% edge decides 26,000 vertices.

**This is MAP-22's competitive-exclusion problem arriving in a new realm** —
the same shape `radiation_affinity::the_sea_elf_is_confined_to_the_shelf_band`
records ("authored to the whole ocean this kind would hold ~27,000 vertices
against wood's ~800"), with one difference that matters for the remedy.

**Band confinement on the sea-elf precedent was considered and rejected**
(controller's ruling). Sea-elf's confinement repaired a kind that was
*authored* too broadly; the reef mason is near-optimal everywhere for a
STRUCTURAL reason, so confining it would simply hand the ocean to the triton.

### Two guards that were over-strong rather than stale

Both had been green on an assumption nothing had ever tested, and both are
corrected rather than loosened — recorded because "re-pin it" was the wrong
instinct in each case:

- **The Land list's qualification equality.** `qualified ==
  in_a_repeating_group` read 174 against 175. `land_list_labels`'s own
  comment already documents the exception — the label map is keyed by VERTEX,
  two settlements can stand on one, and "only the first claimant can wear it".
  A co-tenant can therefore sit in a repeating group unqualified while its
  group-mate is qualified and the lines stay distinct. It had never happened
  before. Now subtracted BY NAME: every unqualified member of a repeating
  group must be provably a co-tenant.
- **`genesis_observes_an_unoccluded_sky`.** Production observes through
  `observe_with_sources(.., &sources)`; the test re-derives through
  `observed_phenomena_as_at`, which takes the DEFAULT sources. The two agree
  only while the extra sources contribute nothing, and drow's new flagship sits
  where a `rain` phenomenon arises. Scoped to the sky sources the
  reconstruction can see, with the residual gap named at the site.

### The cost this campaign imposed on an existing instrument

`founder_collision`'s 0–2999 positive-set sweep budgets **~800 s** in its own
`#[ignore]` reason (prior sweeps: 692.89 / 744.21 / 768.72 / 798.87 s). At
this roster it ran **8,160 s without finishing and was stopped** — a **≥10.2x**
rise. Cause: it builds 3,000 worlds through the deep-history bake on ten
threads, and the settling roster went 15 → 20, so every world pays for five
more peoples' placement and promotion.

**Consequence, stated plainly because it is the one thing this task did not
finish:** the three `founder_collision` tests pin a positive set taken from
that sweep, and re-taking it is now an hours-long job rather than a
thirteen-minute one. They are RED and un-re-pinned. The instrument is
offline-only and `#[ignore]`d, so this cost was invisible until someone ran
it — and the next campaign to widen the roster should budget for it before
starting, not discover it at the end.

### The whole-workspace figure, with its date

**6,089 tests, 1,315.696 s** (`cargo nextest run --workspace
--no-fail-fast`, MacBookPro, 2026-09-11, at the close of the Task 3 fix
round; an earlier run the same evening read 1,326.787 s for the same test
count, so ~1,320 s is the figure and ~11 s is its noise). The first
whole-workspace number this campaign has taken, recorded because the campaign
has just taken the roster from 39 kinds to 45 and Task 4 adds nine more: if
that moves materially, the next reader deserves two numbers rather than an
anecdote.

### Fix round 2: two statements this record made that were false

Both are corrected in the tree; recorded here because both are the campaign's
own named hazard committed by the campaign that named it.

- **"Marine villages are simply not the default walk."** True of the test
  fixtures, false of the program: `cli/src/main.rs`'s no-flag arm still
  resolved `Flagship`, so `possess --seed 42` opened in open blue water with
  "Ways on: surface." Re-aiming the fixtures had left untouched the half that
  was actually asked about. The CLI's no-flag arm now resolves
  `LandSettlement`; `--target flagship` still reaches the ledger's first
  settlement.
- **M5's header: "every axis it counts is an axis that reaches a placed
  settlement."** False. `per_species_capacity_at` takes `BiosphereTraits`,
  `HabitatRealm` and `BiomeAffinity` only, so `sociality`, `status_basis`,
  `in_group_radius` and `activity_cycle` reach placement nowhere. **M5's
  minimum is 5; the placement-reaching separation for that same worst pair is
  2** (`mass`, `biome_affinity`). The assertion is spec-compliant and
  unchanged — §3.4 names `SocietyVector` and `PerceptionVector` among the
  model-carried axes — but M5 measures distinct KINDS, never distinct PLACES.
  M2 and M7 are the placement measurements.

Two guards were also repaired rather than re-pinned: the pantheon
reorganization check had become vacuous (it compared sorted sentiment vectors
whose lengths differ, 129 against 90, so `assert_ne!` passed on cardinality
before reading a sentiment — and the positive content pin it replaced had been
deleted with nothing put back), and the `focalize` marine-noun fix had shipped
with its own witness re-aimed away in the same commit. Both now
mutation-verified red on the exact revert.

### Known-red at the close of this task, and why

**Five tests, in two groups, neither of them unfinished re-pinning.**

The three `founder_collision` tests are blocked on the sweep above, which is
now an hours-long instrument. The two census ones are blocked on the canonical
box:

`census_sentinel::the_first_three_census_worlds_match_the_committed_rows` and
`tripwire::the_committed_census_agrees_with_a_live_rebuild_of_the_tripwire_seeds`
are **RED, expected, and census-blocked** — not unfinished work. Both compare
a live rebuild against the committed census fixtures, authoring six peoples
moved the census, and a refresh runs on the canonical box. Deferred to the
pre-merge close deliberately: Tasks 4 and 5 move the world twice more, so a
census taken now would be invalidated twice before it could be used. This is
exactly the shape CLAUDE.md predicts when it keeps the heavy tier off the
stage-gate list — a census-backed assertion "would red predictably for the
whole middle of any world-touching campaign".

## The absorb — execution record

`origin/main` absorbed into `campaign/the-tidemark` on 2026-09-11/12. Merge
commit `c78116d0f` (parents `2cf803c41` and `807e86ae9`), follow-up
`5146e38a6`. 179 commits, 63 conflicts: 27 generated artifacts, 10 byte-golden
fixtures, 26 source files. Full report:
`.superpowers/sdd/2026-09-11-the-tidemark/absorb-report.md` (scratch — the
durable half is below).

`campaign/underworld-peoples` landed four `HabitatRealm::Subterranean` peoples
— `kuo-toa`, `duergar`, `svirfneblin`, `mountain-dwarf` — while this campaign
authored six obligate `Marine` ones.

### The three reconciled counts

Each RE-DERIVED from the merged data, never added from the two branches' pins.

| registry | main | this branch | merged |
| --- | --- | --- | --- |
| `biosphere_registry` | 43 | 45 | **49** |
| `EPOCH_COHORTS` | 24 | 24 | **25** |
| `habitat_realm_registry` | 7 | 11 | **15** |

`habitat_realm_registry` was 11 on this branch, not the 5 the absorb brief
carried: 5 was the figure in the store's own doc comment, frozen at Task 1 and
left stale when Task 3 added six `Marine` rows. **A count stated inside its own
document is self-falsifying** — the doc comment now names all three groups
(3 + 4 + 6 + 2 explicit `Surface`) so the arithmetic can be re-checked rather
than trusted.

The 49 is carried in a message that names both campaigns and keeps its
arithmetic visible: thirty-nine before either, plus four Underworld peoples,
plus six marine ones.

### The cohort order, and its reason

`domains/language/src/accession.rs`: **the Underworld Peoples' cohort takes the
earlier index, ours the later.** The reason is principled and belongs in the
comment, which now carries it: this module orders by **ARRIVAL**, and their
cohort arrived on `main` first. Both are pure appends onto the same 23-cohort
base, so neither ordering moves any pre-existing concept's proto-root; the
order decides only the ten new kinds' own roots.

The "arrival, not descent" paragraph — why `abyssal-elf-kind` is not folded
into the elf family's cohort — survives verbatim, and so does the
number-withheld paragraph that predicted this exact collision; it now records
how the collision was resolved instead of being deleted.

**Measured afterwards, because the prediction was worth checking:** the reorder
moved no new kind's drawn name at all. What did move many names is settlement
placement — `Dazha` is duergar's on the merged world and was gully-dwarf's on
both branches — which is why the `windows/book` sentence goldens had to be
re-pinned from measurement rather than unioned.

### kuo-toa

Left `Subterranean` WITH `SWIM`, with a comment at the row saying why:
`MAP-11`, the registry parent of this campaign's own `WAT-sea-peoples`, names
"sahuagin, merfolk, kuo-toa" as its aquatic-people examples, so a reader
meeting the new `Marine` variant would reasonably "correct" it — and the
`{0.0, 1.0}` mask has no middle value, so that correction would strip a settled
people of its Underdark home outright. The ambiguity exists only because this
campaign adds `Marine`, so resolving it is this campaign's.

### Three tests needed more than a re-pin

- **`genesis_observes_an_unoccluded_sky`** — the vantage divergence this
  campaign documented has a SECOND direction. Task 3's filter covers a
  committed belief the reconstruction cannot see; duergar's is the mirror (a
  source the reconstruction sees but ranks below its own salience cut). Exact
  equality becomes a bounded, reported, order-preserving subsequence: order,
  multiplicity and every shared source stay exact.
- **`land_list_lines_are_all_distinct_at_seed_42`** — `main` grew a SECOND way
  for a group member to stay bare (`rendered_lines`'s `(site N)` dedup, which
  qualifies the later member and leaves the first). The claim moves from
  per-row to per-group: at most one bare member per group, and it must be the
  first in place order.
- **`history_emit::distinct_layers_tie_only_on_genuine_material_matches`** —
  the {42, 7, 1000} panel stopped exhibiting any tie (0 over 6,545 pairs), so
  its assertions ran on nothing and `ties > 0` caught it. Seed 1 joins the
  panel, chosen by a new `#[ignore]`d `sweep_for_a_tying_seed` (seven of seeds
  0..24 tie) rather than guessed.

A fourth conflict was a genuine duplicate design, not a disagreement:
`the_roll.rs`'s neighbour selection, where both campaigns independently
replaced "the most populous other settlement" with a search. `main`'s form is
kept because it scans the whole roster instead of the twelve most populous;
this campaign's rationale paragraph is kept with it.

### Final test state

`cargo nextest run --workspace --no-fail-fast`: **6109 run, 6106 passed, 3
failed, 248 skipped.** Doctests green. `make gate-commit` rc=0.

- `census_sentinel`, `tripwire` — known red, left red (only lefford authors
  census goldens).
- **`founder_collision` is NO LONGER RED.** Both non-ignored tests pass on the
  merged world. A result, not an accident.
- **`survivorship_probe::the_separation_survives_conditioning_on_tenure` is a
  NEW red, reported and not tuned.** Stratified z = **1.326** against
  `Z_SUPPORTS` 1.96, pooled z 5.244 (previously stratified z 3.197, AUC
  0.7160). The pair-weight direction still supports breached-deeper (429 vs
  376); it is the stratified z alone that fell. The file is byte-identical on
  both branches, so only the WORLD moved — under ten new peoples. Decision
  0016 and the test's own message both say report rather than rescue, and spec
  §5.2 forbids tuning `BREACH_FREE_PATH_M` to separate them again. **This one
  is the controller's call.** Not verified, and it would cost a cold build of
  each ref to verify: whether either branch alone was green here.

## The survivorship amendment — execution record

Implements `docs/superpowers/specs/2026-09-12-the-tidemark-survivorship-amendment.md`,
committed at `750be0be2` **before this work existed**. Subject:
`windows/worldgen/tests/suite/survivorship_probe.rs`. Panel unchanged (E.9's
twelve seeds), `BREACH_FREE_PATH_M` unchanged, `Z_SUPPORTS` unchanged,
`DELVE_M_PER_PERSON_EPOCH` unchanged. Population unchanged at 269 workings —
32 breached, 148 ordinarily ended, 89 still open.

### R1 — quintile strata, replacing the `STRATA` literal

The five-bucket literal is gone. Strata are now quintiles of the **pooled ended**
tenure distribution (breached ∪ ordinary, n=180), so both groups are cut on the
same boundaries; ties in integer epochs fall to the lower stratum (a stratum is
`(previous, this]`); a quintile empty for either group contributes zero to every
sum and is skipped. The van Elteren accumulation (`ΣU`, `ΣE`, `ΣV`,
`z = (ΣU−ΣE)/√ΣV`) is untouched — only the bucket boundaries moved.

**Derived cut points (panel):** Q1 `e ≤ 2`, Q2 `e ≤ 6`, Q3 `e ≤ 10`,
Q4 `e ≤ 21`, Q5 `e ≤ ∞`.

| stratum | epochs | breached n | breached med | ordinary n | ordinary med | AUC |
|---|---|---|---|---|---|---|
| Q1 | 1–2 | 1 | 6.0 | 52 | 12.0 | 0.144 |
| Q2 | 2+–6 | 3 | 27.3 | 26 | 55.4 | 0.282 |
| Q3 | 6+–10 | 5 | 161.3 | 29 | 136.7 | 0.600 |
| Q4 | 10+–21 | 4 | 242.3 | 26 | 271.4 | 0.481 |
| Q5 | 21+–∞ | 19 | 725.0 | 15 | 718.6 | 0.575 |

**Stratified AUC 0.4977, z −0.034.** Median-direction pair weight: **430
supporting, 234 opposing**. Pooled (unstratified) AUC 0.7959, z 5.244 — unmoved,
since the amendment touches only the conditioning.

**Beside the old numbers.** Frozen-literal scheme: stratified AUC 0.5857,
z 1.326, pair weight 429 supporting / 376 opposing, same 269 workings. The
literal's top bucket spanned `21+` (60 of 80 epochs, 42% of pair mass); the
quintile scheme's top bucket spans `21+` too but now holds 34 of the 664
comparable pairs rather than 342, and the mass has redistributed into Q1 (52
pairs' worth of the floor spike sitting alone against one breached working).

**P1 (stratified z rises above 1.96): FAILED.** Not marginally — z fell from
1.326 to −0.034, i.e. the conditioned statistic is at the null, not below the
boundary. Under §5 of the amendment this is the pole named *"§5.2's claim is
unsupported at 49 kinds, on an instrument that can now be trusted to say so —
the stronger finding of the two."* Nothing was tuned in response and nothing
may be: §1 fixes the response in advance.

**`the_separation_survives_conditioning_on_tenure` is now RED**, on its first
assertion, at **z = −0.034**. Its second assertion (pair-weighted median
direction remains breached-deeper) still passes at 430 vs 234, so the test's two
halves disagree; that disagreement is recorded rather than reconciled.

### R2 — censoring sensitivity arm

Same R1 cut points, comparing breached against **all non-breached**
(ordinary 148 + still-open 89 = 237). Reports; asserts nothing.

| stratum | epochs | breached n | breached med | non-breached n | non-breached med | AUC |
|---|---|---|---|---|---|---|
| Q1 | 1–2 | 1 | 6.0 | 55 | 12.0 | 0.136 |
| Q2 | 2+–6 | 3 | 27.3 | 40 | 52.8 | 0.317 |
| Q3 | 6+–10 | 5 | 161.3 | 43 | 137.1 | 0.567 |
| Q4 | 10+–21 | 4 | 242.3 | 41 | 265.7 | 0.482 |
| Q5 | 21+–∞ | 19 | 725.0 | 58 | 1081.6 | 0.369 |

**Stratified AUC 0.3946, z −1.803.** Median-direction pair weight: **215
supporting, 1441 opposing**. Pooled (unstratified) AUC 0.6810, z 3.323.

**Still-open placement, measured rather than assumed** (the amendment's §5 P2
reasoning depends on it): Q1 3, Q2 14, Q3 14, Q4 15, Q5 43. They are spread
across all five strata by founding date, not piled at the top — though Q5 does
carry 48% of them.

**P2 (R2 weakens the separation relative to the primary but does not reverse its
direction): HELD ON THE STATISTIC, FAILED ON THE PAIR-WEIGHT DIRECTION**, and
the two halves must be stated separately because they disagree.
- *Weakening, statistic:* yes. AUC 0.4977 → 0.3946, z −0.034 → −1.803.
- *Direction, statistic:* no reversal of sign relative to the primary — both sit
  below AUC 0.5, i.e. both are on the breached-**shallower** side. But the
  primary is only 0.0023 below 0.5, so "the direction of the primary" is barely
  defined; P2's reasoning was written expecting a positive primary and the
  primary is null.
- *Direction, pair weight:* **reversed.** 430/234 supporting in the primary
  becomes 215/1441 opposing in the sensitivity arm — driven by Q5, where the
  non-breached median (1081.6 m, 58 workings, 43 of them still open) is well
  above the breached median (725.0 m).

Under §5's clause *"if the direction reverses: the exclusion was carrying the
result, and that is a finding about §5.2's design that outranks P1 either way"* —
whether the pair-weight reversal counts as that reversal is a campaign-level
judgment, not this record's to make.

### The four-seed control (E.9), same run

Control cut points differ because the control's own ended population is n=59:
Q1 `e ≤ 2`, Q2 `e ≤ 6.2`, Q3 `e ≤ 12.8`, Q4 `e ≤ 25.4`, Q5 `e ≤ ∞`. Control
stratified AUC 0.6533, z 1.295 (pair weight 65 supporting / 10 opposing);
control pooled AUC 0.7598, z 2.347 against the panel's 5.244. The gate's own
assertions — direction agreement and the control being the weaker read — both
still pass. Its doc's stale `z 3.536` / `5.896` figures were corrected to the
measured 2.347 / 5.244; the control has fallen back below `Z_DECIDES`, which
that doc had recorded as no longer true.

### What was not touched

`BREACH_FREE_PATH_M`, `Z_SUPPORTS`, `Z_DECIDES`, `DELVE_M_PER_PERSON_EPOCH`,
`PANEL`, `CONTROL`, the hazard, the bake. Stratum count stayed at five (the
amendment's own number, fixed before any cut point existed).
`the_still_open_population_is_not_a_third_arm_of_the_comparison` is unmodified
and verified green in its own run (pooled AUC 0.7959 excluded vs 0.6810 pooled-in,
still-open median 327.1 m above the ordinary 67.3 m) — it was cancelled rather
than run in the four-test pass, so it was run again alone to confirm.

Stale committed numbers in the module's own prose were corrected to this run's
(269/32/148/89, floor spike 21.6% vs 3.1%, medians 524.2/67.3/327.1, pooled
AUC 0.7959 / z 5.244, median tenure 24.0 vs 6.0). Those had been carrying the
pre-Murrain 249-working figures.


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
