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

## Task 0 — the instrument (implemented at `665733eab`)

- **A plan-text defect, caught by the control it prescribed.** The brief's
  H2 construction (peaks `[0, 3, 7, 11]` on `Geosphere::new(2)`) yielded two
  basins, not four: vertices 7 and 11 sit at dot 0.447 from vertex 0, and the
  height-10 peak's tail (`10 × 0.447^8 ≈ 0.016`) tilted their neighbourhoods
  so each drained into vertex 0. The implementer moved the minor peaks to
  vertex 0's far set (`[0, 2, 4, 8]`, dot −0.447, clamped to zero
  contribution) and got exactly four on the first try; the assertion was
  untouched, per the plan's own branch. Lesson already on record — never
  prescribe a construction from outside the code; name the property — and
  the plan named the property, which is why this cost one attempt and not a
  weakened test. No ideonomy pass: a task-boundary factual finding.
- **Unrelated stale binary swept.** `domains/alchemy`'s test binary in the
  recycled worktree was stale (`cargo clean -p hornvale-alchemy` fixed it);
  a pool member's warm `target/` is warm for its previous campaign's tree.
- `docs/timings.md` gained gate-commit rows (uncommitted); they ride with
  Task 1's commit.

## Task 1 — readout

HEAD at run time: `9b72518a18116fc61f2edf0faa1137a9aa5eac60` (the pre-fix run;
the committed probe is `20f48585e`, whose re-take was byte-identical except the
footer). Machine: `MacBookPro.local`.
Wall time: 17.767 s real (16.904 s user, 0.642 s sys) for
`cargo test -p hornvale-worldgen --test suite -- hidage_probe --ignored --nocapture`,
five world builds plus one `flow` per settling people per world — well under
the sub-two-minute bound that would have required hoisting `hops_to_attractor`
(it already takes the people's `&Flow` rather than recomputing it per call,
applied from the start rather than measured into afterward).

**H3 correction, beyond a name fix.** The brief's H3 code, taken verbatim,
loops over every alive occupation and asserts `K_p(v) > 0` at each one. Run
as written, it fails deterministically on seed 42: an alive bugbear
settlement (peak population 37) sits at vertex 22182, where the probe's
present-era `K` is exactly `0.0`. This is not a wiring bug. Traced with a
throwaway debug print in `bake_history_from` (reverted before commit): the
bake's own last simulated era (`bake_eras`, `lib.rs:3924`) carries
`temp_offset = -12.6`, `sea_level = -1856.29` for seed 42 — not
`EraAdjust::present`'s `temp_offset = 0.0`, `sea_level = -1820.29` — so a
community founded and grown against that colder, lower-sea-level era can
outlive it once the probe's present-day reconstruction (spec §3.1's own
named **known approximation**, and G3 flag 4) drops its site's capacity to
zero. Spec §3.4 requires exactly this be "counted and printed separately,
never dropped silently" (the probe's `k_zero_sites`), and spec §7 itself
states H3 as a witness "at **one** alive site", not at every one. So H3 was
narrowed to match §7 literally: it now asserts `surface_checked > 0`,
`sub_checked > 0`, and a new `surface_positive_k_seen` (at least one alive
surface site witnesses a positive present-era capacity), rather than
asserting positivity at every alive site. The subterranean arm (`multiplier
<= 1.0`) was left exactly as the brief has it — it held on first run and is
a static bound, not an era-dependent one. RED was the original strict
assertion failing on real seed-42 data (an assertion failure, not a compile
error — the acceptable stopping point Step 2 names); GREEN is the
spec-aligned version above.

**Re-taken after fix round 1** (`hops_to_attractor`'s closing assert absorbed
a `None` expected — the K==0 case — into a vacuous `Some(cur) == Some(cur)`;
the call site now skips K==0 sites entirely and the assert compares against
`expected` directly). Only the wall-time footer line moved in the diff
against the pre-fix readout — the printed `P1 hops to attractor:`
median/max and `m` median/max were byte-identical on every seed, because the
erroneous zero-hop entries the bug added sat alongside genuine zero-hop
entries (a site that is its own attractor) already in the distribution and
did not move the median or the max; full diff in the task report.

Probe output, verbatim:

```text
    Finished `test` profile [optimized + debuginfo] target(s) in 0.02s
     Running tests/suite.rs (target/debug/deps/suite-295e07b6625e1279)

running 1 test
HAMLET_POPULATION_CEILING = 150  LONGHOUSE_POPULATION_FLOOR = 200

== seed 42 ==  alive N_s 390  ended 822  |A_s| 390 (shortfall 0)
  c_s (acc >= 150) 390 / 390   c200_s 388 / 390   occupied-by-an-alive-site 21 / 390
  S1 gini(acc) 0.352  S2 gini(m) 0.323  S3 spearman(acc,K) 0.398  S4 max/median(acc) 5.60
  P2 acc: min 183.2 median 1504.1 max 8424.7   P2 m: median 34.57 max 138.66
  P1 alive sites: attractors 52 / 390   sharing an attractor 354 / 390   K==0 2   multi-people attractor 86
  P1 hops to attractor: median 2 max 9   m: median 2.95 max 93.57   attainment peak/K: median 0.66 min 0.07 max 2.05
  P1 K median 51.7 acc median 136.0  |  P3 ended: n 822  K median 56.2 acc median 210.1 m median 4.78
    bugbear        N_p   63  attractors   858  top-N acc min    771.8 med   1118.0 max   3869.6  c150  63  c200  63  occupied   4  K@top med   65.9
    desert-dwarf   N_p   34  attractors   869  top-N acc min   2107.9 med   2853.3 max   5901.7  c150  34  c200  34  occupied   1  K@top med   55.6
    desert-elf     N_p   13  attractors   876  top-N acc min    747.9 med    904.6 max   2069.2  c150  13  c200  13  occupied   1  K@top med   15.6
    drow           N_p   24  attractors   147  top-N acc min    183.2 med    288.9 max   1136.4  c150  24  c200  22  occupied   2  K@top med   32.1
    gnoll          N_p    5  attractors   832  top-N acc min   1254.8 med   1453.6 max   2245.4  c150   5  c200   5  occupied   1  K@top med   22.6
    goblin         N_p    6  attractors   876  top-N acc min   3307.1 med   3635.3 max   4339.7  c150   6  c200   6  occupied   0  K@top med   40.9
    gully-dwarf    N_p    3  attractors   829  top-N acc min   1341.9 med   1439.8 max   1491.5  c150   3  c200   3  occupied   0  K@top med   36.9
    high-elf       N_p    2  attractors   832  top-N acc min   2629.4 med   3284.3 max   3939.2  c150   2  c200   2  occupied   0  K@top med   36.2
    hill-dwarf     N_p   26  attractors   743  top-N acc min   1468.8 med   1973.1 max   3890.9  c150  26  c200  26  occupied   2  K@top med   36.3
    hobgoblin      N_p   40  attractors   799  top-N acc min   2125.0 med   3008.6 max   8424.7  c150  40  c200  40  occupied   1  K@top med   83.0
    human          N_p    7  attractors   923  top-N acc min   2403.9 med   2693.5 max   3361.4  c150   7  c200   7  occupied   2  K@top med   35.9
    kobold         N_p  130  attractors   585  top-N acc min    638.4 med   1254.8 max   7530.8  c150 130  c200 130  occupied   7  K@top med   50.4
    sea-elf        N_p    3  attractors  1469  top-N acc min    440.6 med    475.7 max    498.2  c150   3  c200   3  occupied   0  K@top med   33.3
    snow-elf       N_p    5  attractors   998  top-N acc min   1012.5 med   1298.7 max   1853.0  c150   5  c200   5  occupied   0  K@top med   26.8
    wood-elf       N_p   29  attractors   832  top-N acc min    938.3 med   1231.1 max   3939.2  c150  29  c200  29  occupied   0  K@top med   36.8

== seed 7 ==  alive N_s 250  ended 406  |A_s| 250 (shortfall 0)
  c_s (acc >= 150) 250 / 250   c200_s 250 / 250   occupied-by-an-alive-site 8 / 250
  S1 gini(acc) 0.262  S2 gini(m) 0.218  S3 spearman(acc,K) 0.614  S4 max/median(acc) 3.13
  P2 acc: min 387.6 median 2054.4 max 6421.6   P2 m: median 48.46 max 114.84
  P1 alive sites: attractors 46 / 250   sharing an attractor 211 / 250   K==0 3   multi-people attractor 74
  P1 hops to attractor: median 1 max 5   m: median 2.37 max 100.55   attainment peak/K: median 0.82 min 0.08 max 1.54
  P1 K median 37.4 acc median 86.1  |  P3 ended: n 406  K median 38.4 acc median 76.7 m median 1.97
    bugbear        N_p    8  attractors  1224  top-N acc min   1951.9 med   2242.3 max   3029.3  c150   8  c200   8  occupied   0  K@top med   68.4
    desert-dwarf   N_p   12  attractors  1226  top-N acc min   4118.8 med   4884.2 max   6421.6  c150  12  c200  12  occupied   0  K@top med   55.8
    desert-elf     N_p    7  attractors  1219  top-N acc min   1060.6 med   1302.7 max   1427.4  c150   7  c200   7  occupied   1  K@top med   22.0
    drow           N_p    3  attractors   318  top-N acc min    387.6 med    399.7 max    514.9  c150   3  c200   3  occupied   1  K@top med   32.8
    gnoll          N_p   13  attractors  1211  top-N acc min    825.0 med    979.2 max   1301.6  c150  13  c200  13  occupied   1  K@top med   31.5
    goblin         N_p   36  attractors  1236  top-N acc min   1707.1 med   2254.7 max   4219.3  c150  36  c200  36  occupied   2  K@top med   39.6
    gully-dwarf    N_p    3  attractors  1213  top-N acc min   1417.5 med   1417.8 max   1434.6  c150   3  c200   3  occupied   0  K@top med   34.0
    high-elf       N_p   34  attractors  1273  top-N acc min   1064.5 med   1342.7 max   2420.8  c150  34  c200  34  occupied   0  K@top med   37.0
    hill-dwarf     N_p   11  attractors  1139  top-N acc min   1796.5 med   2125.1 max   3322.4  c150  11  c200  11  occupied   0  K@top med   36.4
    hobgoblin      N_p   39  attractors  1174  top-N acc min   2398.0 med   2909.3 max   6257.1  c150  39  c200  39  occupied   1  K@top med   84.9
    human          N_p   12  attractors  1270  top-N acc min   2531.8 med   2903.9 max   4124.4  c150  12  c200  12  occupied   0  K@top med   35.6
    kobold         N_p   52  attractors   987  top-N acc min   1282.6 med   1811.9 max   4481.1  c150  52  c200  52  occupied   2  K@top med   39.8
    sea-elf        N_p    2  attractors  1417  top-N acc min    500.8 med    512.1 max    523.4  c150   2  c200   2  occupied   0  K@top med   40.4
    snow-elf       N_p    6  attractors  1423  top-N acc min   1535.0 med   1907.0 max   1990.6  c150   6  c200   6  occupied   0  K@top med   30.3
    wood-elf       N_p   12  attractors  1273  top-N acc min   1511.6 med   1575.8 max   2420.8  c150  12  c200  12  occupied   0  K@top med   37.1

== seed 13 ==  alive N_s 262  ended 799  |A_s| 262 (shortfall 0)
  c_s (acc >= 150) 262 / 262   c200_s 262 / 262   occupied-by-an-alive-site 17 / 262
  S1 gini(acc) 0.446  S2 gini(m) 0.366  S3 spearman(acc,K) 0.542  S4 max/median(acc) 11.24
  P2 acc: min 275.2 median 1365.7 max 15352.3   P2 m: median 39.37 max 218.53
  P1 alive sites: attractors 44 / 262   sharing an attractor 208 / 262   K==0 19   multi-people attractor 51
  P1 hops to attractor: median 1 max 7   m: median 4.97 max 156.91   attainment peak/K: median 0.65 min 0.05 max 1.02
  P1 K median 50.9 acc median 201.2  |  P3 ended: n 799  K median 68.1 acc median 167.8 m median 3.45
    bugbear        N_p   69  attractors  1085  top-N acc min    630.8 med   1164.7 max   4808.1  c150  69  c200  69  occupied   4  K@top med   64.3
    desert-dwarf   N_p    3  attractors  1064  top-N acc min   6238.4 med   6961.6 max   7405.5  c150   3  c200   3  occupied   0  K@top med   59.6
    desert-elf     N_p    3  attractors  1071  top-N acc min   1785.6 med   1873.9 max   1895.3  c150   3  c200   3  occupied   0  K@top med   15.0
    drow           N_p    5  attractors   319  top-N acc min    275.2 med    310.8 max    418.7  c150   5  c200   5  occupied   0  K@top med   12.7
    gnoll          N_p   28  attractors  1064  top-N acc min    792.8 med   1011.9 max   1927.6  c150  28  c200  28  occupied   1  K@top med   23.7
    goblin         N_p    5  attractors  1097  top-N acc min   4211.4 med   4979.7 max   6385.5  c150   5  c200   5  occupied   0  K@top med   41.7
    gully-dwarf    N_p    3  attractors  1084  top-N acc min   1826.0 med   1880.0 max   1886.0  c150   3  c200   3  occupied   0  K@top med   37.6
    high-elf       N_p   25  attractors  1080  top-N acc min    846.5 med   1267.4 max   2806.2  c150  25  c200  25  occupied   1  K@top med   32.6
    hill-dwarf     N_p    6  attractors  1045  top-N acc min   2787.5 med   3073.8 max   3479.7  c150   6  c200   6  occupied   0  K@top med   37.3
    hobgoblin      N_p   39  attractors  1057  top-N acc min   2221.4 med   3276.0 max  10791.5  c150  39  c200  39  occupied   3  K@top med   84.3
    human          N_p    3  attractors  1120  top-N acc min   4602.3 med   5592.8 max   6436.4  c150   3  c200   3  occupied   1  K@top med   36.2
    kobold         N_p   39  attractors   836  top-N acc min    865.9 med   1843.8 max  15352.3  c150  39  c200  39  occupied   3  K@top med   60.4
    sea-elf        N_p    4  attractors  1346  top-N acc min    537.7 med    645.1 max    709.7  c150   4  c200   4  occupied   0  K@top med   33.3
    snow-elf       N_p   26  attractors  1197  top-N acc min    535.9 med    665.9 max   2322.4  c150  26  c200  26  occupied   4  K@top med   20.7
    wood-elf       N_p    4  attractors  1080  top-N acc min   2382.3 med   2745.1 max   2806.2  c150   4  c200   4  occupied   0  K@top med   37.7

== seed 100 ==  alive N_s 60  ended 99  |A_s| 60 (shortfall 0)
  c_s (acc >= 150) 60 / 60   c200_s 60 / 60   occupied-by-an-alive-site 2 / 60
  S1 gini(acc) 0.328  S2 gini(m) 0.265  S3 spearman(acc,K) 0.754  S4 max/median(acc) 2.93
  P2 acc: min 513.1 median 2929.3 max 8571.4   P2 m: median 72.69 max 180.71
  P1 alive sites: attractors 24 / 60   sharing an attractor 28 / 60   K==0 1   multi-people attractor 27
  P1 hops to attractor: median 1 max 6   m: median 12.56 max 77.28   attainment peak/K: median 0.40 min 0.08 max 0.91
  P1 K median 36.7 acc median 501.9  |  P3 ended: n 99  K median 28.5 acc median 377.5 m median 13.23
    bugbear        N_p    4  attractors  1275  top-N acc min   2769.4 med   3435.8 max   5627.9  c150   4  c200   4  occupied   0  K@top med   67.1
    desert-dwarf   N_p    9  attractors  1296  top-N acc min   4192.6 med   4804.9 max   6129.5  c150   9  c200   9  occupied   0  K@top med   56.3
    desert-elf     N_p    2  attractors  1305  top-N acc min   1532.7 med   1807.7 max   2082.6  c150   2  c200   2  occupied   0  K@top med   27.9
    drow           N_p    2  attractors   407  top-N acc min    610.8 med    690.1 max    769.4  c150   2  c200   2  occupied   1  K@top med   28.3
    gnoll          N_p    4  attractors  1243  top-N acc min   1163.4 med   1285.5 max   2241.9  c150   4  c200   4  occupied   0  K@top med   25.7
    goblin         N_p    4  attractors  1286  top-N acc min   3814.6 med   3835.8 max   7603.6  c150   4  c200   4  occupied   0  K@top med   41.8
    gully-dwarf    N_p    5  attractors  1228  top-N acc min   1103.0 med   1458.5 max   2035.5  c150   5  c200   5  occupied   0  K@top med   36.2
    high-elf       N_p    4  attractors  1409  top-N acc min   1626.5 med   1984.6 max   2518.2  c150   4  c200   4  occupied   0  K@top med   32.4
    hill-dwarf     N_p    4  attractors  1202  top-N acc min   2490.1 med   3362.2 max   5060.5  c150   4  c200   4  occupied   0  K@top med   36.9
    hobgoblin      N_p    3  attractors  1215  top-N acc min   4597.6 med   4797.9 max   8571.4  c150   3  c200   3  occupied   0  K@top med   84.4
    human          N_p    5  attractors  1378  top-N acc min   3089.2 med   3450.6 max   6378.5  c150   5  c200   5  occupied   0  K@top med   36.0
    kobold         N_p    4  attractors   962  top-N acc min   3913.7 med   4790.2 max   5262.3  c150   4  c200   4  occupied   0  K@top med   46.0
    sea-elf        N_p    5  attractors  1458  top-N acc min    513.1 med    555.7 max   1017.6  c150   5  c200   5  occupied   0  K@top med   33.3
    snow-elf       N_p    1  attractors  1472  top-N acc min   1901.4 med   1901.4 max   1901.4  c150   1  c200   1  occupied   0  K@top med   14.6
    wood-elf       N_p    4  attractors  1409  top-N acc min   1626.5 med   1984.6 max   2518.2  c150   4  c200   4  occupied   1  K@top med   32.4

== seed 1234 ==  alive N_s 44  ended 870  |A_s| 44 (shortfall 0)
  c_s (acc >= 150) 44 / 44   c200_s 44 / 44   occupied-by-an-alive-site 3 / 44
  S1 gini(acc) 0.320  S2 gini(m) 0.238  S3 spearman(acc,K) 0.813  S4 max/median(acc) 3.07
  P2 acc: min 349.1 median 2297.2 max 7061.0   P2 m: median 65.03 max 109.97
  P1 alive sites: attractors 17 / 44   sharing an attractor 12 / 44   K==0 3   multi-people attractor 26
  P1 hops to attractor: median 1 max 5   m: median 12.13 max 91.96   attainment peak/K: median 0.11 min 0.05 max 1.13
  P1 K median 33.3 acc median 338.0  |  P3 ended: n 870  K median 32.1 acc median 343.4 m median 12.04
    bugbear        N_p    2  attractors   617  top-N acc min   2356.9 med   2835.2 max   3313.4  c150   2  c200   2  occupied   0  K@top med   61.9
    desert-dwarf   N_p    5  attractors   603  top-N acc min   3776.8 med   4060.2 max   4673.1  c150   5  c200   5  occupied   0  K@top med   52.2
    desert-elf     N_p    0  attractors   575  top-N acc min      0.0 med      0.0 max      0.0  c150   0  c200   0  occupied   0  K@top med    0.0
    drow           N_p    5  attractors   239  top-N acc min    349.1 med    374.5 max    997.8  c150   5  c200   5  occupied   1  K@top med   26.2
    gnoll          N_p    0  attractors   565  top-N acc min      0.0 med      0.0 max      0.0  c150   0  c200   0  occupied   0  K@top med    0.0
    goblin         N_p    6  attractors   573  top-N acc min   2291.9 med   2966.2 max   3272.1  c150   6  c200   6  occupied   0  K@top med   35.9
    gully-dwarf    N_p    2  attractors   638  top-N acc min   1766.8 med   1792.9 max   1818.9  c150   2  c200   2  occupied   0  K@top med   33.8
    high-elf       N_p    2  attractors   593  top-N acc min   2133.0 med   2161.2 max   2189.4  c150   2  c200   2  occupied   0  K@top med   29.2
    hill-dwarf     N_p    4  attractors   576  top-N acc min   2058.6 med   2252.6 max   3659.5  c150   4  c200   4  occupied   0  K@top med   34.0
    hobgoblin      N_p    5  attractors   593  top-N acc min   4134.8 med   4506.8 max   7061.0  c150   5  c200   5  occupied   1  K@top med   79.4
    human          N_p    3  attractors   581  top-N acc min   2656.6 med   2946.1 max   2978.8  c150   3  c200   3  occupied   1  K@top med   32.4
    kobold         N_p    0  attractors   559  top-N acc min      0.0 med      0.0 max      0.0  c150   0  c200   0  occupied   0  K@top med    0.0
    sea-elf        N_p    4  attractors  1311  top-N acc min    361.8 med    410.7 max    483.0  c150   4  c200   4  occupied   0  K@top med   33.3
    snow-elf       N_p    4  attractors   602  top-N acc min   1682.2 med   1756.9 max   2169.8  c150   4  c200   4  occupied   0  K@top med   32.9
    wood-elf       N_p    2  attractors   593  top-N acc min   2133.0 med   2161.2 max   2189.4  c150   2  c200   2  occupied   0  K@top med   29.2

== per-seed (c_s, N_s) [(390, 390), (250, 250), (262, 262), (60, 60), (44, 44)]
== VERDICT (spec §4, mechanical): Rescale
test hidage_probe::hidage_probe ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 493 filtered out; finished in 17.68s

```

Verdict line as printed: `== VERDICT (spec §4, mechanical): Rescale`

#9 [Q] — **Spec §4's attainment caveat: does RESCALE stand?** ·
**Decision: yes — the verdict is RESCALE under the rule as written AND under
the caveat's own correction, on every seed.** · Why: the caveat says the bar
is on capacity and RESCALE is "a rescale only if attainment is high", with
MIXED applying "if RESCALE fires with median attainment below 0.5". Two seeds
have a median below 0.5 (seed 100: 0.40; seed 1234: 0.11), so the caveat is
live and must be worked, not waved. Worked from the readout (`12c7009de`):
divide the bar by each seed's own median attainment and count the top-N
catchments still clearing it, from the per-people min/median/max rows.

```
  seed   median a   bar / a    P2 rows below the corrected bar        c'/N   > 0.5
  ----   --------   --------   -----------------------------------   -----   -----
  42     0.66        227       drow only (min 183, med 289): <= 12    >= 378/390   yes
  7      0.82        183       none (min 388)                          250/250   yes
  13     0.65        231       none (min 275)                          262/262   yes
  100    0.40        375       none (min 513)                           60/60    yes
  1234   0.11       1364       drow (5, max 998) + sea-elf (4, max 483)  35/44   yes
```

The caveat's concern — that capacity overstates population enough to make a
capacity-majority a population-minority — cannot bite at these margins:
the median top-N catchment is 1,366–2,929 headcount against a 150 bar (9–20x),
and the multiplier m over P2 has a median of 35–73. Ruling reading of the
caveat: it asks whether the majority survives attainment, and it does, so
the printed verdict stands and the chronicle reports `a` beside it, as §4
requires. · Alternatives discarded: reading "median attainment" as pooled
over all seeds (would also pass, but a per-seed reading is the stricter one
and matches every other clause's "on every seed"); declaring MIXED because
two seeds are under 0.5 (that reads the caveat as a fifth clause, which it
is not — it is a correction to the bar, and the corrected bar still fires). ·
ideonomy passes / overturns: 0 — a mechanical application of a frozen rule,
with the arithmetic shown. · Capture: this entry; Task 3's verdict section;
decision 0827.

**The plan's own sentence, disclosed:** this entry was written against spec
§4's caveat and did NOT cite the implementation plan, which froze a stronger
form of the same clause before the probe ran —
`docs/superpowers/plans/2026-09-06-the-hidage.md`, Task 3 Step 1: *"if the
verdict is RESCALE and median `a < 0.5`, the branch is MIXED and the entry
says why."* The spec says the MIXED **response** applies; the plan says the
**branch is** MIXED, and it makes the trigger a bare per-seed comparison with
no arithmetic in between. Under the plan's literal reading, seeds 100 (0.40)
and 1234 (0.11) put this campaign on **MIXED** — no verdict, the per-seed
table goes back to the metaplan §4 D1 as the finding, and the arc's next step
is a metaplan decision — rather than on **RESCALE**, which strikes D1 and
licenses the metaplan to assert that worked land is a uniform rescale on this
field. Those two responses differ in exactly what the record may claim, so the
difference is not cosmetic. The reading taken above is that the caveat
corrects the **bar** (150 divided by each seed's own median attainment) rather
than adding a fifth clause: every other §4 clause is quantified "on every
seed", the caveat's own preamble frames itself as a statement about what the
bar measures ("the bar is on capacity, not population"), and the corrected bar
still fires on every seed, tightest at 35 of 44. **But that is a
post-unblinding interpretation of preregistered text, made by the party the
interpretation favours**, and it is disclosed here rather than absorbed. It is
put to Nathan at G6 for ratification, with the alternative named: read the
plan's sentence literally, the verdict is MIXED and the arc returns to the
metaplan with the same table and no strike. Nathan may choose either; nothing
downstream of this ledger assumes the ratification.

**The characterization predictions (§4.1), against the readout:**

```
  prediction                          42     7      13     100    1234   verdict
  ----------------------------------  -----  -----  -----  -----  -----  -------
  S1 gini(acc) >= 0.25 (heavy tail)   0.352  0.262  0.446  0.328  0.320  HELD
  S3 spearman(acc, K) >= 0.7          0.398  0.614  0.542  0.754  0.813  FAILED (3 of 5)
  median attainment in [0.5, 1.0]     0.66   0.82   0.65   0.40   0.11   FAILED (2 of 5)
  attr / N < 0.5                      0.13   0.18   0.17   0.40   0.39   HELD
```

S3 failing means D1 would not only resize settlements but re-order them:
the biggest vertex is not reliably the biggest basin. Attainment failing on
the two small worlds (60 and 44 alive against 99 and 870 ended) says the
binding ceiling there is not capacity — a D6 observation, recorded for the
metaplan rather than acted on.

**Secondary findings the metaplan should carry** (all from the readout, none
acted on here):
- **Bake sites cluster into few watersheds.** 354 of 390 alive sites on seed
  42 share an attractor with another site of their people (211/250, 208/262,
  28/60, 12/44 elsewhere); only 52 of 390 sites ARE attractors. A catchment
  per settlement would need a split rule before it could be wired in — §6's
  partition question, answered in the bake's own terms.
- **Alive sites with zero present-era capacity exist**: 2, 3, 19, 1, 3 per
  seed. The bake's last era is not the present (spec §3.1's approximation is
  real), or those communities are on ground that no longer feeds them.
  Counted and printed, never dropped from `m`.
- **Attainment above 1.0 occurs** (max 2.05 on seed 42): a community standing
  above the capacity the PROBE's present-era reconstruction gives its vertex.
  `COLLAPSE_PRESSURE = 2.0` does not explain a reading of 2.05, and the
  approximation does: the bake grew that community against its own last era's
  `temp_offset`/`sea_level`, not the present's (spec §3.1, and the same cause
  as the K==0 sites above).
- **Occupancy is low**: 21 of 390 top-N attractors on seed 42 carry an alive
  settlement of that people (8/250, 17/262, 2/60, 3/44). The places the flow
  field would rank first are mostly not where the bake put anyone.
- **The catchment field DOES carry an apex** — S4 (max over median
  accumulation) is 2.9–11.2 — which contradicts §4's RESCALE response text
  "it makes no apex". What dies is D1 as "wire the catchment in as the
  growth ceiling at today's scale": every settlement becomes a town. Whether
  a RESCALED catchment (normalised so the walkable band holds) would make a
  differentiated apex is a different question, and it is metaplan §6's open
  `SETTLERS_PER_CAPACITY` item, not this rung. Recorded so the record does
  not overclaim in either direction.

## Task 1 — rulings (implemented at `12c7009de`, fixed at `20f48585e`)

- **H3 narrowed, and accepted.** The brief's loop asserted `K > 0` at EVERY
  alive surface site; seed 42 has two alive sites with zero present-era
  capacity (the bake's last era carries its own `temp_offset`/`sea_level`,
  spec §3.1's named approximation — traced by the implementer with a
  reverted debug print, `git diff lib.rs` empty). The delivered H3 asserts
  the spec's own wording, "at one alive site", keeps both realm counters
  non-zero, and leaves the load-bearing wiring check — the tag-position
  `assert_eq!` in `world_and_fields` — unconditional. Ruling: the spec's
  claim is discharged and the wiring is guarded; the brief overstated the
  spec. Cost if wrong: a swapped realm branch that zeroes every surface site
  would still fail the witness, so the narrowing tolerates only the per-site
  benign case. No ideonomy pass: a task-boundary factual ruling.
- **A plan-text defect, again mine.** The plan's `hops_to_attractor` closed
  with `assert_eq!(Some(cur), expected.or(Some(cur)))`, which is
  `Some(cur) == Some(cur)` whenever flow has no attractor for `v` — exactly
  the K==0 sites H3 had just surfaced. The reviewer caught it because the
  dispatch asked, by name, whether the assert could pass vacuously. Fix
  round 1: the call site filters on `k > 0.0` and the assert compares
  against `expected` directly. **The re-taken readout moved no hops line**:
  the spurious zeros sat among genuine zero-hop self-attractor sites and
  never shifted a median or a max, so the defect was invisible in the
  output it corrupted — which is the reason the assert had to be honest
  rather than the numbers merely re-checked.
- **A clippy allow was added** (`#![allow(clippy::disallowed_methods)]`,
  file-level) with a comment citing decision 0092 and the two sibling
  probes carrying the same line; the reviewer confirmed both. Accepted.

## Task 2 — stage gate

Submitted at the Task 1 plan-stage boundary, per the standing cadence (and
per The Cruck's recorded miss of exactly this).

- `req-f02fb4f7d3e1-20260906T153314Z`, `f02fb4f7d`, 2026-09-06T15:33Z —
  **REFUSED AT THE MOUTH**: merge conflict between `origin/main` and the
  candidate on `docs/audits/campaign-reconciliation.tsv`, where both sides had
  appended rows. The box was never taken, which is the mouth working as
  designed: a conflict costs milliseconds instead of a claim.
- Main absorbed locally at `30604a92f` (union resolution on the TSV — both
  sides' rows kept; the digest's decision index regenerated in the same
  commit, since main had added records of its own).
- `req-4feb86d81540-20260906T161648Z`, `4feb86d81`, 2026-09-06T16:16Z —
  resubmitted. **Outcome: `reported`, all stage phases rc=0 in 1345 s, main
  unchanged at `fd7f1d4f4`** (read back from `make sluice-status` at
  2026-09-06T16:5xZ; it queued behind a running census and one stage gate,
  then ran ~22 minutes). The merge product this gated is the absorbed tree at
  `4feb86d81`; the four docs-only commits after it carry no Rust change.

## Task 3 — verdict and records

**The verdict is RESCALE** (`20f48585e`), and D1 is struck.

Applied mechanically against spec §4, over `A_s` (P2), on all five seeds:

```
  seed    c_s / N_s   ratio   c200_s / N_s   median a
  ----    ---------   -----   ------------   --------
  42      390 / 390   1.00    388 / 390      0.66
  7       250 / 250   1.00    250 / 250      0.82
  13      262 / 262   1.00    262 / 262      0.65
  100      60 /  60   1.00     60 /  60      0.40
  1234     44 /  44   1.00      44 /  44     0.11
```

Clause 1 (`c_s == 0` on every seed) is false everywhere; clause 3
(`c_s / N_s <= 0.25`) is false everywhere; clause 2 (`c_s / N_s > 0.5` on
every seed) holds on all five at the extreme value 1.00. So the printed
`== VERDICT (spec §4, mechanical): Rescale` is the rule as frozen, not a
reading of it.

**The attainment caveat does not flip it** — worked in #9 and not re-derived
here. Two seeds sit under the caveat's 0.5 trigger (100 at 0.40, 1234 at
0.11), so the caveat is live; dividing the 150 bar by each seed's own median
attainment gives corrected bars of 227 / 183 / 231 / 375 / 1364, and the
count still clearing them is `>= 378/390`, `250/250`, `262/262`, `60/60`,
`35/44` — a majority on every seed, tightest at 35 of 44 on seed 1234. The
branch is RESCALE, not MIXED.

**The §4.1 predictions** (from #9, unchanged): S1 >= 0.25 **held** (0.352,
0.262, 0.446, 0.328, 0.320); attr/N < 0.5 **held** (0.13, 0.18, 0.17, 0.40,
0.39); S3 >= 0.7 **failed** on 3 of 5 (0.398, 0.614, 0.542, 0.754, 0.813);
median attainment in [0.5, 1.0] **failed** on 2 of 5 (0.40, 0.11).

**The one qualification the records carry, per #9.** The catchment field does
carry an apex — S4 is 2.9–11.2 and S1 is 0.26–0.45 — so §4's RESCALE response
text ("it makes no apex") is wrong about the field. What dies is D1 *as
specified*: wiring the catchment in as the growth ceiling at today's scale
makes every settlement a town. Whether a rescaled catchment would make a
differentiated apex is metaplan §6's open `SETTLERS_PER_CAPACITY` question,
not a rung, and neither decision record decides it.

### Records written

- `docs/decisions/0826-a-dynamics-probe-falsifies-on-a-count-against-an-existing-ceiling.md`
  — the criterion's form, which D2–D6's probes inherit (from #2's pass 3 and
  spec §4).
- `docs/decisions/0827-d1-is-struck-worked-land-is-a-uniform-rescale-on-the-growth-field.md`
  — the verdict, with the per-seed table, the attainment arithmetic and the
  four predictions.
- `docs/superpowers/specs/2026-09-04-the-staple-metaplan.md` — §1.1's "genesis
  uses it" corrected (#1), §4 D1's restatement corrected, a **Probe result**
  paragraph appended to D1, and one bracketed struck-note each beside §2.1's
  dependency diagram and §3 consequence 3.
- `book/src/frontier/idea-registry.md` — `SOC-staple-ladder` status sentence
  and Where.
- `book/src/chronicle/the-hidage.md` (+ `book/src/SUMMARY.md`).
- `docs/retrospectives/the-hidage.md` (+ `docs/retrospectives/README.md` row).
- `docs/audits/campaign-reconciliation.tsv` — spec and plan rows flipped to
  `shipped`, chronicle and retro rows added; spec and plan `**Status:**`
  headers updated.

### Book freshness sweep — what was corrected, and what was left

Swept on the INVARIANT the verdict touches — "the production path reads a
catchment" — not on the wording.

Corrected:

- `book/src/domains/settlement.md` (a first pass at `bed5ada34`, the rest
  here): the "the condensation flow still builds the capacity field the
  history reads" passage and the seed-42 settlement-count paragraph, both put
  in the bake's terms; "An attractor becomes a committed settlement once …"
  → *became … until The Living Community made the bake the provider; the flow
  survives as the Lab's readout of the field*; "because each species now
  condenses its field independently, two peoples may settle overlapping
  ground" → past tense, with "and the Lab's readout still does" kept, because
  the overlap consequence is a real property of that instrument.
- `book/src/domains/overview.md`: the Settlement row's Inputs cell named "the
  sibling `demography` domain's field **and flow**"; the flow is an input to
  nothing on the production path, so the cell names the field only.

Left, with the reason:

- **The Tier 1 heading** ("settlements condense out of a carrying-capacity
  field (Campaign 4a … re-founded on a field by *The Gathering*)"). It is a
  tier label naming the campaigns that built it, in a chapter whose tier
  ladder is a historical record; it asserts no present-tense provision, and
  the paragraph beneath it now carries the correction.
- **The chronicle-pointer paragraph and "the tier ladder ahead"** (~line 470):
  "[The Gathering] for the move from a suitability scatter to the
  carrying-capacity field and its condensation" is a claim about what that
  campaign did, which is still true. No present-tense provision is asserted.

### Confidence Gradient — no bet moved

Grepped `book/src/open-questions.md` on the invariant (`hamlet`,
`SETTLERS_PER_CAPACITY`, `catchment`, `every settlement`, `city`) and read
every hit. Three regions are the near misses and none is moved: the
carrying-capacity promotion and its partial rescore (~1328, ~3902) score the
field's *existence and clock*, which this probe reads rather than changes;
the entity-size passage (~4105) scores whether a heavy tail over *entities*
is reachable without persistent per-entity multiplicative heterogeneity, and
a uniform rescale of a ceiling is not that mechanism — the verdict corroborates
that bet without moving it, and this campaign measured no entity sizes at all;
and The Staple's own entry (~5466) is filed **unscored** by its own text. No
re-score paragraph written, per decision 0030's condition (resolves or moves).

### Deferred minors found in Task 3

**Four Task 3 rows were added to the table above** — the epigraph's stance,
the chronicle not quoting the caveat's literal MIXED wording, the
`SOC-staple-ladder` compaction, and the review ruling that promoted the
FALSE-STATEMENT minors into fix round 1 rather than deferring them. Three are
accepted as-is; the fourth records what was fixed. The earlier tasks'
minors stand as recorded, including the Task 1 one
(`multi_people_attractor_sites` counting per `(vertex, people)` P1 entry, not
per distinct vertex), which the chronicle reports in exactly those terms.

### No census, no world moved

`git status` before the records commit showed one modified tracked file,
`docs/timings.md` (Task 0/1 gate-commit rows), and nothing under any
generated path. The probe commits nothing and adds no `pub` item (spec §5),
so no census is needed at this close.

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
  probe should take it. **(Moot: D1 is struck; carried only as a note for D2's
  brief.)**

## Deferred minors

| task | minor | outcome |
| --- | --- | --- |
| 0 | `median`'s even-length branch has no test (the brief's case is odd-length) | deferred to the final review |
| 0 | `gini` guards `mean <= 0.0` where `== 0.0` is the stated case; unreachable on non-negative inputs | accepted as-is |
| 1 | `multi_people_attractor_sites` counts per `(vertex, people)` P1 entry, not per distinct vertex; a vertex hosting two peoples' alive occupations counts twice | accepted as-is: consistent with every sibling P1 statistic; read it as "P1 entries whose vertex is a multi-people attractor" (Task 3) |
| 3 | chronicle epigraph reads as a stronger null than measured ("ploughing would change nothing worth having") while the body says D1 would re-order settlements | accepted as-is: the body is precise; an epigraph is a stance |
| 3 | chronicle's verdict section does not quote the caveat's literal MIXED wording; decision 0827 carries it in full | accepted as-is |
| 3 | `SOC-staple-ladder` compaction to fit 600 chars dropped "one flow ends in a sink" and shortened R2's clause; both survive in metaplan §1.1/§2.2 and decision 0786 | accepted as-is: a compaction, recoverable |
| 3 | review ruling: minors that are FALSE STATEMENTS in permanent records (retro:6 "second in a row", metaplan:424 "moves out of §6", metaplan §6 still awaiting a probe that has reported, §4 D1's wrong-units figure with no local note, retro:126 omitting the digest regen, chronicle "three to eleven") were promoted into fix round 1 rather than deferred; wording minors stayed deferred | fixed in Task 3 fix round 1 |

## Capture manifest

- This ledger; the spec; a `campaign-reconciliation.tsv` row for the spec.
- Board: technique post on the worktree-take race (2026-09-05).
- **Decisions** `0826` (the criterion's form, from #2) and `0827` (the RESCALE
  verdict, from #9 and the Task 1 readout). The block 0826–0835 is otherwise
  unused; 0828–0835 go unspent.
- **Metaplan** `docs/superpowers/specs/2026-09-04-the-staple-metaplan.md`: §1.1
  correction (#1), §4 D1 restatement plus the **Probe result** paragraph, and
  one bracketed struck-note each in §2.1 and §3 consequence 3.
- **Registry** `SOC-staple-ladder` (`book/src/frontier/idea-registry.md`):
  status sentence and Where.
- **Chronicle** `book/src/chronicle/the-hidage.md` + `book/src/SUMMARY.md`;
  **retrospective** `docs/retrospectives/the-hidage.md` +
  `docs/retrospectives/README.md`.
- **Reconciliation** `docs/audits/campaign-reconciliation.tsv`: spec and plan
  rows `shipped`, `chronicle-the-hidage` and `retro-the-hidage` added; the
  spec's and the plan's `**Status:**` headers record closure.
- **Confidence Gradient:** no bet moved; the hits read and the reason are in
  `## Task 3 — verdict and records`.
