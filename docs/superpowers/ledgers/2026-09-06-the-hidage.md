# The Hidage — decision ledger

Campaign: **The Hidage** — The Staple's **D1 Task 0 probe**: the preregistered
measurement that decides whether "a settlement has worked land" may start
(metaplan standing rule 1). Named for the Tribal Hidage, the assessment of each
people's land in hides — the unit of worked land that feeds one household.
Branch: `campaign/the-hidage`. Decision block: **0826–0835** (reserved
2026-09-05 on lefford; main ceiling 0788 at reservation).
Spec: `docs/superpowers/specs/2026-09-06-the-hidage-design.md`.

Tree read at `ceca521f8` (origin/main, The Cruck merged).

## Entries

#1 [Q] — **Does settlement genesis use the catchment, as metaplan §1.1 and §4
D1 say?** · **Decision: no — nothing on the production path does; the
correction is load-bearing and the metaplan is amended, not the probe
narrowed.** · Why: the brief and the metaplan both state that "genesis uses
[the catchment]; the bake discards it". Checked before designing anything:
`windows/worldgen/src/lib.rs:8202-8210` says the deep-history bake, not
demography condensation, has been the settlement provider since The Living
Community, and that the `coexist::pack`/`condense_stack` placer is "gone from
genesis". `condense_tagged` has exactly one caller,
`demography_report_with_beta_from` (`lib.rs:2297`), which is the Lab's
readout accessor and is called from no build stage. The bake's own genesis
(`history_bake.rs:4342-4420`) seeds each people at 2–4 sites drawn from its
top-64 vertices by river-weighted **per-vertex** capacity, opening each at
`GENESIS_POP = 10`, and grows every community logistically toward
`eff_capacity` — a per-vertex read again. So both halves of the model that
decide where a settlement is and how big it gets are single-vertex reads, and
the catchment (`domains/demography/src/flow.rs`, `condense.rs`) exists only as
a Lab instrument. Two consequences: (a) D1 is "wire the catchment the code
already has into the bake", not "reconcile two live halves"; (b) **no
committed artifact carries the catchment distribution**, so the probe must
compute it itself, on the bake's own field. Also: the metaplan's "~22 people,
182 settlements" figures are from The Gathering's dimensionless field at an
earlier threshold (`CONDENSATION_THRESHOLD` doc: 108 at 1.7 since The
Confluence, per-people since The Seam) and are not comparable with anything
in headcount. · Alternatives discarded: treating the metaplan as authoritative
(it lagged the code, as it did at its own close — Staple ledger #7); narrowing
the probe to the Lab's condensation (it gates nothing, in different units). ·
ideonomy passes / overturns: 0 — a factual finding with one right answer,
stated explicitly per the backfill rule. · Capture: spec §2; metaplan §1.1 and
§4 D1 amended at close (spec §10); this entry.

#2 [G1] — **How does a preregistered probe decide that D1 would be a uniform
rescale in disguise?** · **Decision: falsify on a COUNT against the ceilings
that already exist, with both a floor and a ceiling — never on flatness.** ·
Why: four `ideonomy-plain` passes.
Pass 1 (dimension-identification + organon-construction → list; prompts
autonomy, distribution, symmetry) found the draft thin in three places: the
bake has three fields in play (the growth field `eff_capacity`, the siting
field `caps × river_factor`, and the Lab's suitability field) and the probe
must name which it sums over; within one people the flow field is a tree, so
catchments **partition by construction** and §6's partition-or-overlap
question is open only across peoples and between bake sites sharing an
attractor; and nothing in the draft checked whether the bake's growth even
reaches its ceiling — a peak of 86 against a vertex capacity that may be
higher would mean the binding ceiling is pressure or remittance, which D1
does not touch. It also added the ended population (2,996 ruins) as a free
readout for D6.
Pass 2 (combination → atlas; prompts size, visibility) crossed populations
against readouts and produced the coincidence count — how many of the top-N
attractors already carry an alive settlement — and insisted the observable
(`peak_population`) be printed beside both hidden fields.
Pass 3 (cross-domain re-instantiation → spectrum; prompts scope,
reversibility) **overturned the criterion.** Re-instantiated in hydrology,
`flow` is the drainage algorithm, and drainage-basin areas are heavy-tailed
by construction (Hack's law): a Gini-of-accumulation "flatness" test would
return "not flat" on any field the algorithm is run over, so it could never
fire — the guard-that-cannot-go-red shape this project has recorded before.
What can actually falsify D1 lies on a spectrum with two dead poles: no
top-N catchment clears the hamlet ceiling (worked land cannot make a city),
or most of them do (every settlement becomes a town — a rescale that
`SETTLERS_PER_CAPACITY` already performs). The live band is a minority apex.
Pass 4 (tree-finding → cycle; prompts animacy, polarity) added nothing pass 3
had not: convergence.
· Alternatives discarded: Gini/percentile-ratio flatness as the death clause
(kept as a characterization statistic with a stated prediction); a
multi-source Voronoi partition around the bake's actual sites (invents a
mechanism the code lacks; the flow tree is the mechanism that exists); a
travel-cost catchment via `least_cost_from` (§6 leaves it to D1's design);
era-varying catchments (D1's climate payoff, not this probe's question —
named as unmeasured). · ideonomy passes / overturns: **4 passes, 1 overturn
(pass 3), material additions in passes 1–2, pass 4 empty.** · Capture: spec
§3–§4; decision 0826 (the criterion's form) at close.

#3 [Q] — **Is this campaign the probe alone, or the probe and D1's design?** ·
**Decision: the probe alone; D1's design is its own campaign, conditional on
the verdict.** · Why: standing rule 1 puts the probe BEFORE the rung; decision
0016 freezes the criterion before the code that could move it; and the
precedent is this arc's own founding — The Staple ran Task 0 first and the
result refounded the campaign. Folding a design behind a probe that may kill
it would spend spec effort on a rung the probe exists to make cheap to lose.
The spec's cost section budgets both verdicts (the brief's "budget the finding
either way"). · Alternatives discarded: a conditional two-part spec (a second
G3 inside one campaign, which the autopilot has no gate for). · ideonomy
passes / overturns: pass 3's `scope` and `reversibility` prompts (#2) covered
this — the probe is reversible and commits nothing; the rung is an epoch. 1
pass, no overturn. · Capture: spec §1, §6, §9.

#4 [Q] — **Which capacity field does the probe sum, and how does it get it?**
· **Decision: the bake's growth field — per-people present-era headcount
capacity times the delve-seating multiplier — reconstructed from public API,
not exposed from the bake.** · Why: `eff_capacity` is the read D1 replaces,
so it is the field whose watersheds matter. The bake builds it privately in
`bake_history_from` (`caps_by_era`), one slice per paleoclimate era, scaled
by `seatings[i].multiplier` (a 1.0 no-op for surface peoples). The probe
reproduces the present slice with `per_species_capacity` (documented
bit-identical to `per_species_capacity_at` at `EraAdjust::present`) and
`delve_seating::seating_for`, both `pub`, following `capacity_cost_probe.rs`'s
setup and its stated convention of mirroring private constants rather than
widening the API for a readout. The last bake era differs from present only
by that era's `temp_offset` and `sea_level` (`bake_eras`, `lib.rs:3924`); the
spec names this as an approximation and the ratio statistics are over one
field either way. · Alternatives discarded: a new `pub` seam returning the
bake's slice (a pub item costs a type-audit tag and a report drift for a probe
that commits nothing); the Lab's suitability field (#1). · ideonomy passes /
overturns: pass 1's field dimension (#2) is the pass; no overturn. ·
Capture: spec §3.1.

#5 [Q] — **Name.** · **Decision: The Hidage.** · Why: grepped first —
`the-terrier` (the register of holdings) is a merged campaign; `the-extent`,
`the-hidage`, `the-glebe`, `the-assart`, `the-perambulation` are all free in
retrospectives, specs, chronicle and branches. The Tribal Hidage assesses
each people's land in hides, per people, before the land is worked — which is
this probe exactly. · ideonomy passes / overturns: 0 — a naming choice,
stated. · Capture: this entry.

#6 [Q] — **The visual companion.** · **Decision: not launched.** · Why:
Nathan's standing preference is to set it up during brainstorming without
asking, but this brainstorm ran unattended under autopilot with no reader at
the browser, and no question in it was visual (the organons above are lists,
a spectrum and a branch table). A tab opened on an empty desk records
nothing; the G3 package carries the branch table in text. · ideonomy passes /
overturns: 0 — a process call, stated. · Capture: this entry; if Nathan wants
a diagram of the two fields at G3, it is one drawing.

#7 [Q] — **A `make worktree-take` race recycled this campaign's worktree.** ·
**Decision: take a different member by hand, leave the other session's alone,
record the defect as a follow-up, and post the technique.** · Why: a
just-taken worktree sits at `origin/main` with a clean tree, which is exactly
the predicate `scripts/worktree-take.sh` uses to judge a member recyclable.
Observed 2026-09-05T23:13Z: this session's take of `the-cruck` → `the-hidage`
was recycled four seconds later by another session's
`make worktree-take NAME=the-spillway`, leaving `campaign/the-hidage` behind
as a branch with no worktree and a stale registry entry (repaired with
`git worktree repair`, non-destructively). Recovered by recycling `the-warp`
(merged, clean, tip ≠ `origin/main`, no open files) with the script's own
steps, then touching this ledger immediately so the tree reads dirty. Posted
to the board as a technique. · ideonomy passes / overturns: 0 — an incident,
not a design choice. · Capture: `## Follow-ups`; board post.

#8 [G4] — **Plan review before execution.** · **Decision: proceed with the
plan at `cf67474a6`.** · Why: self-reviewed against the spec — every §3.4
statistic has a print line, §4's rule is the `verdict` function with its own
test, §7's H1–H3 are Task 0/1 tests, §5's branch table is Task 1 Step 7, §10's
records are Task 3. Three defects found in the plan's own text and fixed
before commit: `hornvale_worldgen::World` is not re-exported (kernel path
used); a per-people minimum printed `f64::MAX` on an empty row (helper
added); Task 3 said "add" a plan row the plan commit already carries
("flip"). H3's surface arm asserts `K > 0` rather than bit-equality against
a second `per_species_capacity` call, because the field IS that call's
output times 1.0 and the equality that matters is the tag-position assert in
`world_and_fields`; the spec's wording is discharged by construction and
this entry says so. · Alternatives discarded: a pre-flight that runs every
plan command (does not earn its cost — autopilot skill). · ideonomy passes /
overturns: 0 — a review gate, not a design choice. · Capture: this entry.

## Follow-ups

- **`scripts/worktree-take.sh` should refuse to recycle a member whose branch
  tip is exactly `origin/main`.** A finished campaign's tip is its own commit;
  only a fresh take (or a fast-forwarded branch, rare) sits at main's tip.
  Alternatively, the taker writes a marker the script honours. Either closes
  the race in #7. Not fixed here: outside this campaign's scope, and a
  `scripts/` change that should carry its own test.
- **The metaplan's D1 restatement is stale on its first clause** (#1). The
  amendment lands with this campaign's close (spec §10), not before, so the
  probe's verdict and the correction are one edit.
- **Era-varying catchments** — whether a cooling era shrinks a watershed
  faster than it shrinks the apex vertex — is D1's climate payoff (metaplan §3
  consequence 3) and is not measured here. If D1 lives, its design's own
  probe should take it.

## Deferred minors

(none yet)

## Capture manifest

- This ledger; the spec; a `campaign-reconciliation.tsv` row for the spec.
- Board: technique post on the worktree-take race (2026-09-05).
