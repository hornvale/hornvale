//! THE STOPE, Task 7: did the campaign solve the **oatmeal problem**?
//!
//! The campaign's own falsification criterion, and a publishable null. Spec
//! §4.3: *"a world that is 90% generic cave has not solved the oatmeal
//! problem"*. Nothing here changes production code — every quantity is a pure
//! read over shipped entry points.
//!
//! # WHICH PREREGISTRATION GOVERNS — §4.2 IS SUPERSEDED
//!
//! **Do not apply spec §4.2's table.** It gates *median total floors in 5-25*.
//! Amendment **C.5** replaces it outright with a different pair of quantities:
//!
//! ```text
//! MAIN-LINE DEPTH   the spine, surface to termination.    GATED.
//! TOTAL FLOORS      main line plus every branch.          REPORTED, never gated.
//! ```
//!
//! C.5's reasoning, because it decides what this file measures: §3.1's frozen
//! per-band ranges (Undercroft 1-5, Shallows 3-10, Deeps 5-20, Underdeep 5-10,
//! Nadir 1-5) sum to **15-50 once**, which is exactly the spine — branches
//! increase *count* without increasing *depth*. So the gate belongs on depth,
//! and the total is reported with only an arithmetic bound (4 x 50 = 200).
//!
//! # THE DENOMINATORS, STATED RATHER THAN IMPLIED
//!
//! - A **cave system** is a cave-bearing LAND cell, exactly as
//!   [`termination_probe`](super::termination_probe) defines it: terrain
//!   reports at most one `Cave` per cell, ocean cells are excluded explicitly
//!   and counted separately, and every "share of systems" below has
//!   cave-bearing land cells as its denominator.
//! - A **branch** is a realized branch column of a system's **canonical
//!   lattice**, `(cell, entrance 0)`. `entrance_mouth`'s own doc states the
//!   rule this follows: *"The system's canonical lattice is `(cell, entrance
//!   0)` and every mouth addresses INTO it"* — entrances are apertures into one
//!   graph, not separate lattices. So the branch population is
//!   `branch_count_of(seed, cell, 0)` columns per system, and every "share of
//!   branches" has that population as its denominator.
//! - **Branch count is measured per ENTRANCE and reported per SYSTEM**, and
//!   these are different distributions. `branch_count_of(seed, cell, entrance)`
//!   is keyed per entrance, so a system with three entrances each drawing one
//!   branch has a per-system total of 3 while every draw came out 1. **C.1's
//!   mode-must-be-1 gate is applied to the per-entrance draw** — the quantity
//!   C.1 actually froze — and the per-system total is reported beside it,
//!   never gated, so a bookkeeping choice cannot falsify C.1.
//!
//! # THE ACCEPTED ASYMMETRY THIS PROBE INHERITS
//!
//! `chamber_exists` gates `branch` against `branch_count_of(cell,
//! addr.entrance)` — entrance N's own drawn width — while `entrance_mouth`
//! picks its side branch from entrance 0's. Task 5 recorded that as accepted,
//! not an oversight. The consequence here: reachability is walked over
//! whatever addresses the shipped `passages_from` admits (entrance carried
//! along), and reached branches are attributed by their `branch` index alone,
//! under the shared-lattice reading `entrance_mouth`'s doc states. A branch
//! index reached only through a non-zero entrance whose width exceeds
//! entrance 0's is therefore attributed to a column the canonical lattice did
//! not realize; that is the same asymmetry, seen from the measurement side,
//! and it is named here rather than silently normalised away.
//!
//! # WHAT ACTUALLY GATES A BRANCH FROM REACHING THE NADIR
//!
//! **Read this before reading `P(Nadir access | branch reached Underdeep)`,
//! because the answer is decided by the code and not by the number.**
//! `chamber_exists` (`windows/worldgen/src/chamber.rs`) is the whole of the
//! shipped access predicate, and its gates are, in order:
//!
//! 1. `branch >= BRANCHES_PER_SYSTEM` — the lattice ceiling.
//! 2. `floor >= LEVELS_PER_BRANCH_CEILING` — the lattice ceiling.
//! 3. `branch >= branch_count_of(..)` — C.1's drawn width.
//! 4. **`band > rung_rank(rung_at_depth(cave.depth_reach_m, gradient))`** —
//!    the terrain's own depth budget. This is the only depth gate.
//! 5. `floor >= levels_in_branch(..)` — Task 2's drawn run length. Every frozen
//!    range bottoms out at 1, so this never empties a band.
//! 6. `chamber_stream(..).next_f64() < EXISTENCE_DENSITY` — a fixed 0.5 coin
//!    per address.
//!
//! **Neither `bands_of` nor `barrier_of` appears anywhere in it.** Verified by
//! grep over the whole tree: `bands_of` has exactly one production consumer,
//! `junctions_at`, where it decides whether two *systems* join sideways at a
//! shared band; `barrier_of` has **no production consumer at all** outside its
//! own module and the CLI pin parser. So:
//!
//! > **B.7 froze a question whose answer B.6 had already decided.** B.5 makes
//! > the barrier "what lets you through at all" at the Nadir, and B.6 in the
//! > same amendment defers every *effect* of the barrier to a later campaign
//! > ("this one ships the dial, not the noise it makes"). A dial with no
//! > consumer cannot gate anything, so `P(Nadir access | reached Underdeep)`
//! > could not have landed on B.7's `0-10%` arm however the draws fell. The
//! > quantity B.7 gates is not a quantity this campaign's scope contains.
//!
//! That is a **second preregistration defect in this campaign**, recorded
//! rather than repaired, exactly as B.8 records the first. It is reported
//! here, in these words, rather than smoothed over by choosing a denominator
//! that yields a comfortable number.
//!
//! What remains measurable is what the *rock and the coin* do, and it is
//! measured three ways so no single definition can carry the finding alone:
//!
//! - **lattice existence** — the branch realizes at least one chamber at the
//!   Nadir rank (`chamber_exists`, the shipped predicate);
//! - **graph reachability** — a Nadir chamber on that branch is reachable
//!   through `passages_from` from an open mouth, which is the only place C.4's
//!   descent sequence could gate;
//! - **the character table** — `bands_of(character_of(..))` contains `Nadir`,
//!   the strongest gate that exists anywhere in the tree even though
//!   `chamber_exists` never consults it.
//!
//! # THE FROZEN BRANCH TABLES, AS DECISION RULES
//!
//! Each is encoded below and the arm is **asserted**, not printed, so a later
//! change that moves a rate out of the band this campaign reported under
//! reddens here instead of writing a different number into a log nobody reads.
//!
//! ```text
//! §4.3  THE OATMEAL CRITERION (the headline)
//!   one character > 80% of branches -> FAILED
//!   no character exceeds 80%        -> report the full distribution
//!
//! C.5   MAIN-LINE DEPTH
//!   median 5-40 and max <= 50 -> proceed
//!   median < 5                -> the ranges are not producing depth
//!   any system > 50           -> STOP; the measurement is summing branches
//!   (median > 40, max <= 50)  -> THE TABLE NAMES NO ARM. See below.
//!
//! C.5   TOTAL FLOORS PER SYSTEM   reported; assert only max <= 200
//!
//! C.1   BRANCH COUNT (per entrance)
//!   mode is 1 -> proceed
//!   mode > 1  -> the tree is not a tree
//!   (a TIE for the top) -> THE TABLE NAMES NO ARM. See below.
//!
//! B.7   P(Nadir access | branch reached Underdeep)
//!   0-10% -> the intent
//!   >10%  -> the gate is not gating
//!   ~0%   -> the endgame is unreachable; STOP
//!
//! B.7   BARRIER STATE SHARE   reported as a distribution, NEVER gated
//! ```
//!
//! **Three of these tables have holes, and all three are recorded rather than
//! repaired** (B.8's discipline — repairing a table *now*, knowing the answer,
//! is what preregistration exists to prevent):
//!
//! - C.5's depth table names no arm for `median > 40 with max <= 50`.
//!   [`DepthVerdict::UnspecifiedByTheBrief`] exists for it. It did not fire.
//! - C.1's branch-count table names no arm for a TIE at the top of the
//!   histogram, and this is the only classifier here where a tie is possible
//!   at all. [`BranchCountVerdict::UnspecifiedByTheBrief`] exists for it.
//!   Until review round 1 the tie was broken toward `ModeIsOne` — the PASSING
//!   arm — by a `Reverse(*count)` inside the `max_by_key`, which is B.8's own
//!   defect class committed inside the file that records B.8. It does not bind
//!   today: the top two are 58.37% and 27.08%.
//! - B.7's Nadir table **overlaps**: `~0%` is a subset of `0-10%`, so a rate
//!   of 0.4% satisfies two arms with opposite verdicts. Encoded here under the
//!   only self-consistent reading — *exactly zero* is `EndgameUnreachable`,
//!   `(0, 10%]` is `TheIntent` — and the overlap is named so the next reader
//!   does not discover it by landing in it. It did not bind: every reading is
//!   far above 10%.
//!
//! # MEASURED, 2026-08-22, seeds 42 / 7 / 1234
//!
//! Wall time for the whole probe (three `BuildDepth::Terrain` worlds, the
//! lattice scan and the reachability walk): **1.13 s**, warm tree. Population:
//! 3,821 cave systems over 42,299 land cells, 6,136 realized branch columns,
//! 0 ocean-cell caves.
//!
//! ## THE HEADLINE: §4.3 IS NOT FAILED. The campaign did produce variety.
//!
//! ```text
//! share of branches by character            denominator: realized branches
//!   seed        WildCave     FungalGardens      DrowTier      verdict
//!   42       917  65.13%      420  29.83%      71   5.04%     Varied
//!    7      1761  64.79%      815  29.99%     142   5.22%     Varied
//!   1234    1326  65.97%      597  29.70%      87   4.33%     Varied
//!   POOLED  4004  65.25%     1832  29.86%     300   4.89%     Varied
//! ```
//!
//! The largest share is **65.25%**, well under §4.3's 80% ceiling, and it is
//! stable to 1.2 points across the panel. It is also close to `character_of`'s
//! own authored weights (65 / 30 / 5), which is the correct outcome and worth
//! saying explicitly: the draw is not being reshaped by the lattice.
//!
//! **But read §4.3's own words before calling this a win.** §4.3 asks whether
//! *character* varies, and 65% of every branch in every world is still one
//! thing. The criterion is cleared; "solved" is a stronger claim than the
//! criterion makes, and the ratio is an authored constant rather than
//! something the structure produced.
//!
//! ## Branch count — C.1's gated draw, PER ENTRANCE (Ruling R1)
//!
//! ```text
//!   branches drawn      42          7        1234       POOLED
//!     1              726 59.07%  1374 57.68%  1066 58.80%  3166 58.37%
//!     2              320 26.04%   663 27.83%   486 26.81%  1469 27.08%
//!     3              112  9.11%   227  9.53%   171  9.43%   510  9.40%
//!     4               71  5.78%   118  4.95%    90  4.96%   279  5.14%
//!   verdict          ModeIsOne  ModeIsOne  ModeIsOne  ModeIsOne
//! ```
//!
//! **Reported, never gated — the per-SYSTEM total, summed across entrances:**
//! pooled 41.61% of systems carry 1 column, 25.10% carry 2, and the tail runs
//! to **13** (one system, seed 7). That tail is exactly why R1's distinction
//! matters: applying C.1's mode-is-1 test to the per-system total would be
//! testing a quantity C.1 never froze, and 13 columns in one system would read
//! as a violation of a rule about a draw that came out 1 every time.
//!
//! ## Entrances per system
//!
//! ```text
//!   entrances     42          7         1234       POOLED
//!     1       622 71.17%  1182 70.32%   892 70.46%  2696 70.56%
//!     2       176 20.14%   337 20.05%   241 19.04%   754 19.73%
//!     3        49  5.61%   122  7.26%    93  7.35%   264  6.91%
//!     4        27  3.09%    40  2.38%    40  3.16%   107  2.80%
//! ```
//!
//! Reproduces `entrance_count`'s authored 70 / 20 / 7 / 3 weights.
//!
//! ## Main-line depth — C.5's gated quantity
//!
//! ```text
//!   seed    p10   p25  median   p75   p90   max   min   verdict
//!   42      6.0  14.0    23.0  29.0  34.0    44     1   Proceed
//!    7      6.0  10.0    24.0  32.0  37.0    46     1   Proceed
//!   1234    8.0  18.0    27.0  33.0  38.0    49     1   Proceed
//!   POOLED  7.0  12.0    24.0  32.0  37.0    49     1   Proceed
//! ```
//!
//! Median 23-27, inside C.5's `[5, 40]`; max 49, under the 50 ceiling with one
//! floor to spare, which is what a full-ladder spine drawing near its maxima
//! looks like. The `> 50` arm never fires, so the measurement is not summing
//! branches into the spine.
//!
//! ### Reproducing C.5's prior reading: 24 and 27 exactly, 23 against 22
//!
//! C.5 records **22 / 24 / 27**. Seeds 7 and 1234 reproduce to the unit; seed
//! 42 reads **23** here. Three things were checked before reporting it:
//!
//! 1. **It is not a percentile convention.** [`pct_floor`] is printed beside
//!    [`pct`] for exactly this, and both give 23.0 — the two order statistics
//!    either side of seed 42's median position are both 23.
//! 2. **It is not the world moving.** Task 0's `termination_probe` was re-run
//!    on this tree and reproduces its own committed histogram byte for byte
//!    (`[77, 131, 399, 53, 214]`, 24.49 / 43.25 / 42.34% Nadir). The
//!    terminating-rung distribution that decides how many bands a spine sums
//!    over has not moved since C.5 was written.
//! 3. **It is one unit at the exact median.** Seed 42's population is
//!    dominated by `Deeps`-terminating systems (399 of 874), whose spine sums
//!    `Unif{1..5} + Unif{3..10} + Unif{5..20}` with mean 22.0 — so the
//!    population median sits precisely on the 22/23 boundary.
//!
//! C.5's measurement left no committed code, so which convention produced 22
//! cannot be recovered. **The disagreement moves no arm** — 22 and 23 are both
//! deep inside `[5, 40]` — and it is recorded rather than resolved by adjusting
//! this probe's definition, which is stated in full above and reproduces two of
//! three seeds exactly.
//!
//! ## Total floors per system — REPORTED, never gated (C.5)
//!
//! ```text
//!   seed    p10   p25  median   p75   p90   max
//!   42      7.0  18.0    28.0  44.0  69.0   147
//!    7      8.0  13.0    28.0  43.0  74.0   158
//!   1234    9.0  22.0    33.0  54.0  77.0   152
//!   POOLED  8.0  17.0    30.0  48.0  74.0   158
//! ```
//!
//! Max observed 158 against the arithmetic ceiling of 200 — the only thing
//! asserted about this quantity. Nathan's "probably structurally impossible"
//! judgement about 200 holds: the widest system on the panel reaches 79% of it.
//!
//! ## Barrier state — REPORTED, never gated (B.7)
//!
//! ```text
//!   state    42          7         1234       POOLED
//!   Sealed  347 24.64%  691 25.42%  495 24.63%  1533 24.98%
//!   Warded  366 25.99%  656 24.14%  486 24.18%  1508 24.58%
//!   Thin    347 24.64%  693 25.50%  502 24.98%  1542 25.13%
//!   Open    348 24.72%  678 24.94%  527 26.22%  1553 25.31%
//! ```
//!
//! Flat to within 0.7 points of uniform, which is `barrier_of`'s authored
//! `range_u32(0, 3)` reproducing itself. **Neither of B.7's two named findings
//! fires**: nothing is above 90% `sealed`, and `sealed` is not absent. But the
//! finding B.7 did not think to name does: **a quarter of every underworld is
//! `Open` — "a portal into Chaos, civilization-ending"** — which is not a
//! scarce thing, and is a controller question about the authored weights rather
//! than anything this probe can adjudicate.
//!
//! ## `P(Nadir access | branch reached Underdeep)` — B.7, CONDITIONED
//!
//! Numerator and denominator are conditioned alike here, which is what B.7
//! asks for. The unconditional counters live in the section below and are
//! **different numbers**; the probe conflated them until review round 1.
//!
//! ```text
//!   reading                  42            7          1234        POOLED
//!   lattice existence   275/430 63.95%  960/1413 67.94%  677/1052 64.35%  1912/2895 66.04%
//!   graph reachability    1/2   50.00%    7/15   46.67%    2/10   20.00%    10/27   37.04%
//!   character table     314/430 73.02% 1000/1413 70.77%  726/1052 69.01%  2040/2895 70.47%
//!   verdict, all three  GateNotGating on every seed and every reading
//! ```
//!
//! **Arm: `>10%` — the gate is not gating.** And the module header above says
//! why that could not have come out otherwise: no shipped code gates access.
//! The lattice reading is the terrain's `P(Nadir | Underdeep)`, which B.4 had
//! already measured at 80-83% and called "far too common", attenuated by the
//! 0.5 existence coin. The character reading is `bands_of`'s table, which
//! admits `Nadir` for `WildCave` and `DrowTier` — that is 70% of branches by
//! construction, and it is the strongest gate that exists anywhere.
//!
//! ## THE SECOND FINDING, AND IT IS BIGGER THAN THE CONDITIONAL
//!
//! The graph reading's denominator is **2 / 15 / 10 branches**. That is not a
//! sampling accident; it is the result. Unconditionally, over all 6,136
//! realized branches:
//!
//! ```text
//!   realizes a Nadir chamber in the lattice   1928 / 6136   31.421%
//!   reaches the Underdeep BY WALKING            27 / 6136    0.440%
//!   reaches the Nadir     BY WALKING            22 / 6136    0.359%   <- ratcheted
//! ```
//!
//! **THESE ARE UNCONDITIONAL COUNTS, AND TWO OF THEM WERE NOT.** Until review
//! round 1 both Nadir counters were incremented inside `absorb`'s
//! `if b.reached_underdeep` / `if b.lattice_underdeep` blocks and then printed
//! against `self.branches` under the word UNCONDITIONAL — a conditioned
//! numerator over an unconditional denominator, reading 0.163% and 31.160%.
//! The nesting is *correct* for B.7's conditional, whose denominator is
//! conditioned the same way, so both pairs are now counted and printed
//! separately and each is named where it is printed.
//!
//! **What the nesting was deleting is the interesting half.** Of the 22
//! branches that reach a Nadir chamber by walking, **12 never reach an
//! Underdeep chamber on the same branch** — you arrive at the bottom without
//! passing through the layer above. That is the majority of the population,
//! and the conditioned counter erased exactly it.
//!
//! **The deep underworld exists and is very nearly unwalkable.** An independent
//! instrument already said so and nobody had read it as this:
//! `docs/audits/underworld-lattice-seed-panel.md` reports seed 42 as **21,328
//! chambers, 1,496 reachable from 511 open entrances** — 7.0%. This probe's own
//! walk reproduces that pair **exactly** on all three seeds (1496/511,
//! 3277/1070, 2493/831), from a different call site.
//!
//! **That corroborates THE WALK, and only the walk.** It is a reachable/open-
//! mouth pair, so it pins this probe's traversal against an independently
//! written one — it says nothing about the Nadir tallies, which is precisely
//! where the defect above was. This module printed it immediately after the
//! Nadir figure as though it covered that too; it is now attached to what it
//! actually covers, and the POOLED row says outright that no committed line
//! corroborates it (the witness prints per-seed pairs only, and no 7266/2412
//! line exists in it).
//!
//! ## THE MECHANISM — NOT PERCOLATION, AND THE REAL ONE IS A DESIGN LEVER
//!
//! **The account this module gave until review round 1 was wrong, and it was
//! wrong in a checkable way** — which is why the evidence is now printed every
//! run rather than asserted in prose. It read: *"C.4's descent rule meeting the
//! 0.5 existence coin, so traversing an n-floor run costs ~0.5^(n-1)"*. That
//! is a closed form, and it fails two independent tests.
//!
//! **Test 1 — route ablation** (printed every run, under `ROUTE ABLATION`).
//! Restrict the walk to entrance 0's mouth, the surface head, which is the
//! only route the descent-chain story describes:
//!
//! ```text
//!   nadir reached, all mouths                  22 / 6136 = 0.359%
//!   nadir reached, entrance 0 only              2 / 6136 = 0.033%  <- 9.1% of it
//!   nadir reached, no lateral moves            12 / 6136 = 0.196%
//!   nadir reached, entrance 0 AND no lateral    0 / 6136 = 0.000%
//! ```
//!
//! **91% of Nadir reaches do not descend from the surface at all**, and the
//! last line is the sharpest statement of it: the pure spine-descent route the
//! closed form describes delivers **exactly zero** branches. The 2 the
//! head-only walk does find get there by stepping sideways onto another branch
//! first. (The two ablations needing a production change were taken with
//! `scripts/mutate.py` and the tree restored — see the mutation ledger below.)
//!
//! **Test 2 — density sweep.** `0.5^(n-1)` predicts a constant elasticity
//! `d ln R / d ln p` equal to the number of floors that must exist — at least
//! 10 by §3.1's minima, and ~19-28 for a typical spine. Mutating
//! `EXISTENCE_DENSITY` alone:
//!
//! ```text
//!   p     nadir reached / 6136     entrance-0-only share   elasticity
//!   0.3      9   0.147%                    0%
//!   0.5     22   0.359%  (shipped)         9.1%              1.75
//!   0.7     97   1.581%                   57.7%              4.41
//!   0.9   1023  16.672%                   83.7%              9.37
//! ```
//!
//! The elasticity is not constant and every value is 3-15x below what the
//! closed form requires. The absolute prediction misses by five orders of
//! magnitude: a full-ladder spine at the pooled median depth of 24 gives
//! `0.5^23 = 1.2e-7`, against an observed `22/1928 = 1.1e-2` among the branches
//! that realize a Nadir chamber at all.
//!
//! **The true mechanism is `entrance_mouth` -> `root_floor_of`, and it is a
//! dial.** A side entrance does not descend; it maps to its branch's ROOT
//! FLOOR, and `root_floor_of` picks that band **uniform over the parent's
//! realized bands** — today always all five, since every frozen range bottoms
//! out at 1. So roughly one side entrance in five is a door opening directly
//! into the Nadir band. Printed every run, under `DEEP-MOUTH CENSUS`:
//!
//! ```text
//!   680 DRAWN side-branch mouths (entrance > 0 AND branch > 0)
//!     band 0 Undercroft  161  23.68%   surviving chamber_exists: 25
//!     band 1 Shallows    126  18.53%                             12
//!     band 2 Deeps       148  21.76%                             18
//!     band 3 Underdeep   108  15.88%                              7
//!     band 4 Nadir       137  20.15%                             12
//! ```
//!
//! That is `root_floor_of`'s uniform pick reproducing itself to within 4
//! points, visible directly. **The denominator is the load-bearing part and it
//! is easy to get wrong**: a further 923 non-head entrances fall back to the
//! surface head (a width-one system, or a side branch that roots nowhere), and
//! folding those in puts 68% of the population at band 0 and understates the
//! deep share threefold. A drawn side-branch mouth is exactly
//! `entrance > 0 && branch > 0`, since `root_floor_of` refuses branch 0.
//!
//! Lateral branch moves supply most of the remainder (22 -> 12 with lateral
//! disabled). The rising elasticity and the rising entrance-0 share are the
//! same fact, and they are the signature of a **mixture**: at the shipped
//! p = 0.5 a near-linear deep-mouth route dominates, and only above p ~ 0.7
//! does the high-exponent descent chain take over at all.
//!
//! **So the design statement is not "the deep underworld is nearly
//! unwalkable".** It is: **the deep underworld is essentially only reachable
//! through a door that opens directly into it.** That is a live design lever —
//! `root_floor_of`'s band pick — and not a percolation constant. Weighting that
//! pick toward the shallow bands makes the Nadir rarer without touching the
//! geology, the ladder or the existence coin; leaving it uniform is what makes
//! the endgame reachable at all today.
//!
//! This is **not** B.7's `~0%` arm, which is about the conditional and does not
//! fire. It is a quantity no frozen table covers; the rate itself now carries a
//! ratchet band ([`NADIR_WALK_RATE_FLOOR`]) so a change of this size cannot
//! pass silently again, but no *verdict* is invented for it, because inventing
//! one after unblinding is the thing preregistration exists to stop. It is the
//! controller's to rule on.
//!
//! ## THE JOINT TABLE — (character x terminating band), "the campaign's product"
//!
//! Counts of realized branches, pooled over the panel. The terminating band is
//! the deepest rank at which the branch realizes a chamber; `(none)` is a
//! column whose every existence draw failed.
//!
//! ```text
//!   character      Undercroft  Shallows    Deeps  Underdeep    Nadir   (none)
//!   WildCave              234       864      948        637     1265       56
//!   FungalGardens         101       429      415        301      557       29
//!   DrowTier               12        65       69         45      106        3
//! ```
//!
//! Per seed, so the panel's spread is visible rather than averaged away:
//!
//! ```text
//!   seed 42        Undercroft  Shallows    Deeps  Underdeep    Nadir   (none)
//!   WildCave               68       132      410        104      185       18
//!   FungalGardens          37        66      192         43       73        9
//!   DrowTier                3         8       35          8       17        0
//!
//!   seed 7         Undercroft  Shallows    Deeps  Underdeep    Nadir   (none)
//!   WildCave               85       601      125        287      648       15
//!   FungalGardens          37       293       63        141      273        8
//!   DrowTier                5        50       11         25       48        3
//!
//!   seed 1234      Undercroft  Shallows    Deeps  Underdeep    Nadir   (none)
//!   WildCave               81       131      413        246      432       23
//!   FungalGardens          27        70      160        117      211       12
//!   DrowTier                4         7       23         12       41        0
//! ```
//!
//! **Read the rows against `bands_of`, because the table exposes something the
//! eligibility table was supposed to prevent.** `FungalGardens` declares
//! `[Undercroft, Shallows, Deeps]` — it is not eligible for the Underdeep or
//! the Nadir — and yet **858 of its 1,832 branches (47%) terminate there**.
//! `DrowTier` declares `[Underdeep, Nadir]` and 146 of its 300 (49%) terminate
//! shallower.
//!
//! That is not a contradiction in the data; it is the module header's
//! mechanism restated as a number. `chamber_exists` never consults `bands_of`,
//! so the eligibility table constrains **where a character may be MET through
//! `junctions_at`** and constrains nothing about **where its branch's chambers
//! EXIST**. Both readings are defensible and the tree ships only the second, so
//! the joint table is a table of *terminating depth by character*, not of
//! *habitation by character*. Naming that is the point of printing it: a reader
//! who takes this table as "where each character lives" will be wrong for 47%
//! of the FungalGardens rows, and nothing in the tree would tell them.
//!
//! The band shape is otherwise the same for all three characters — each row is
//! close to the panel's terminating-rung distribution scaled by that
//! character's share — which is the expected consequence of two independent
//! draws: the character is keyed on the branch, the terminating band on the
//! rock and the coin, and neither reads the other.
//!
//! # NON-VACUITY AND THE POSITIVE CONTROLS
//!
//! Every distribution asserts its corpus is non-empty — systems, branches,
//! both conditional denominators, every character and every barrier state —
//! because a share over an empty corpus is `0/1 = 0` and reads as a finding.
//! The reachability walk additionally asserts both that something is reachable
//! and that not everything is, so neither a dead walk nor an ungated one can
//! pass as a measurement.
//!
//! **The joint table's guard was the exception, and it was an implication
//! rather than a guard** (review round 1). `!joint.is_empty()` sat behind
//! `branches > 0`, and `absorb` inserts a joint entry for every branch, so it
//! could not fail unless the assertion in front of it already had: the table
//! this file calls "the campaign's product" had no guard of its own, and an
//! empty *cell* passed trivially. It is now guarded by two clauses that can
//! each fail with `branches > 0` — the table must occupy at least two distinct
//! terminating bands (a table entirely in the `(none)` column, or piled into
//! one band, is degenerate), and every character must occupy at least one cell
//! with a realized band.
//!
//! **The headline was unasserted too**; see [`NADIR_WALK_RATE_FLOOR`].
//!
//! Mutations of the SHIPPED derivation, applied with `scripts/mutate.py`
//! (which refuses a pattern that matches zero or more than one site), each
//! reddening the assertion it targets and nothing else. The tree was restored
//! and `git diff` confirmed empty after each:
//!
//! ```text
//! A  character_of: 0.05/0.35 -> 0.01/0.05   WildCave 65.13% -> 94.96%
//!    => OatmealVerdict::Failed, panics on §4.3's assertion. The roster stays
//!       occupied (FungalGardens 3.76%, DrowTier 1.28%), so the red is the
//!       oatmeal arm and not the vacuity guard in front of it.
//! B  levels_in_branch: the range_u32 draw -> constant 1
//!    median depth 23.0 -> 3.0
//!    => DepthVerdict::RangesNotProducingDepth, panics on C.5's assertion.
//! B2 floors_range(Deeps): (5, 20) -> (45, 60)
//!    median depth 23.0 -> 63.0, max 44 -> 84
//!    => DepthVerdict::MeasurementSumsBranches, the OTHER direction of the
//!       same gate, proving the >50 arm is reachable and not decorative.
//! C  branch_count_of: 0.60 -> 0.05   mode 1 (59.07%) -> 2 (80.15%)
//!    => BranchCountVerdict::TreeIsNotATree, panics on C.1's assertion.
//! D  EXISTENCE_DENSITY: 0.5 -> 0.7   walked-Nadir 0.359% -> 1.581%
//!    => panics on the headline ratchet. This is the control the probe did
//!       NOT have: the same one-character change exited 0 before F3.
//! E  this probe's own `terminating = Some(rank)` -> `None`
//!    the joint table collapses entirely into the `(none)` column (4004 /
//!    1832 / 300 by character) while `realized` — and therefore every
//!    conditional denominator — is untouched
//!    => panics on the joint table's own guard. What it proves is about the
//!       OLD guard: `!joint.is_empty()` saw 6,136 entries on that tree and
//!       was satisfied, and every vacuity assertion ahead of it passed, so
//!       the campaign's product table reported nothing at all through a run
//!       that would have exited 0.
//! ```
//!
//! Two further mutations were taken as MEASUREMENTS rather than controls —
//! they produce the mechanism evidence above and assert nothing:
//!
//! ```text
//! S  EXISTENCE_DENSITY: 0.5 -> 0.3 / 0.7 / 0.9   the density sweep table
//! L  passages_from: both lateral `addr.branch +/- 1` pushes disabled
//!    walked-Nadir 22 -> 12; entrance-0-only 2 -> 0
//! ```
//!
//! A red from a compile error would prove nothing about an assertion, so each
//! mutation was chosen to type-check and change a number.
//!
//! Test fixture (decision 0092): calls the composition-root entry points
//! directly, the sanctioned posture for this crate's live-worldgen batteries.
#![allow(clippy::disallowed_methods)]

use std::collections::{BTreeMap, BTreeSet};

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Band, CellId, Seed};
use hornvale_terrain::{Cave, GeothermalGradient, TerrainPins, rung_at_depth, rungs};
use hornvale_worldgen::chamber::{
    ChamberAddr, RunAddr, chamber_exists, entrance_count, entrance_mouth, levels_in_branch,
    passages_from, rung_rank,
};
use hornvale_worldgen::character::{
    BarrierPins, BarrierState, CHARACTERS, Character, bands_of, barrier_of, branch_count_of,
    character_of,
};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to_with_artifacts,
};

/// Seeds the campaign preregisters on (spec §5), matching every other
/// live-worldgen probe in this suite.
const SEEDS: [u64; 3] = [42, 7, 1234];

/// §4.3's ceiling: a character holding a share strictly above this has made
/// the world oatmeal.
const OATMEAL_CEILING: f64 = 0.80;

/// C.5's gated band for main-line depth, inclusive at both ends.
const DEPTH_FLOOR: f64 = 5.0;
/// The top of C.5's gated band for the median.
const DEPTH_MEDIAN_CEILING: f64 = 40.0;

/// C.5's hard ceiling on one system's main-line depth. **Not a preference:**
/// it is the sum of §3.1's frozen per-band maxima (5 + 10 + 20 + 10 + 5),
/// which is what a full-ladder spine can draw at most. `floors_range` is
/// private to `chamber.rs`, so this probe cannot derive it and states its
/// provenance instead. A reading above it means the measurement is summing
/// branches into the spine, not that the world got deeper.
const MAIN_LINE_DEPTH_CEILING: u32 = 50;

/// C.5's arithmetic bound on total floors per system: the depth ceiling times
/// the lattice's branch width (4 x 50). Reported, never a gate on the design.
const TOTAL_FLOORS_CEILING: u32 = 4 * MAIN_LINE_DEPTH_CEILING;

/// B.7's ceiling for `P(Nadir access | branch reached Underdeep)`.
const NADIR_GATE_CEILING: f64 = 0.10;

/// THE HEADLINE RATCHET, floor — the share of all realized branches reaching
/// a Nadir chamber by walking, pooled over the panel.
///
/// **Why a ratchet exists here at all.** Every frozen table in this file gates
/// a quantity the spec preregistered, and none of them gates the number this
/// probe's own finding is about. Review round 1 demonstrated the gap the
/// cheapest possible way: mutating `EXISTENCE_DENSITY` from 0.5 to 0.7 — one
/// character of shipped production code — moved this rate 4.4x and the probe
/// still exited 0, because the whole block was printed rather than asserted.
///
/// **Why a BAND and not a point.** The quantity is a count of 22 branches out
/// of 6,136; pinning the exact integer would redden on any change that moved
/// the world at all, which trains a reader to re-baseline without looking. The
/// band is roughly half to double the measured 0.359% (11 to 46 branches) —
/// wide enough to survive incidental motion, tight enough that every point of
/// the density sweep in this module's header lands outside it.
const NADIR_WALK_RATE_FLOOR: f64 = 0.0018;

/// THE HEADLINE RATCHET, ceiling. See [`NADIR_WALK_RATE_FLOOR`].
const NADIR_WALK_RATE_CEILING: f64 = 0.0075;

/// The delve ladder's habitation rungs as `(rank, rung)`, shallowest first —
/// **derived from [`rungs`] through the shipped [`rung_rank`]**, never
/// restated as `0..5`. `rung_of_rank` is private to `chamber.rs`, so this is
/// the sanctioned route from a test crate; it is `junctions.rs`'s
/// `habitation_bands` verbatim, and it is the reason this file contains no
/// literal band ceiling anywhere.
fn habitation_bands() -> Vec<(u8, Band)> {
    let mut bands: Vec<(u8, Band)> = rungs()
        .iter()
        .filter_map(|&rung| rung_rank(rung).map(|rank| (rank, rung)))
        .collect();
    bands.sort_by_key(|&(rank, _)| rank);
    bands
}

/// The lattice rank of one named rung, by the same route.
fn rank_of(rung: Band) -> u8 {
    rung_rank(rung).expect("a named habitation rung has a rank")
}

/// Percentile of an ascending slice of counts, by the round-half-away index
/// convention (`round((n-1) q)`).
fn pct(sorted: &[u32], q: f64) -> f64 {
    if sorted.is_empty() {
        return f64::NAN;
    }
    let i = ((sorted.len() - 1) as f64 * q).round() as usize;
    f64::from(sorted[i])
}

/// The same percentile by the FLOOR-index convention (`floor((n-1) q)`).
///
/// Printed beside [`pct`] for the median alone, and it earns its place: C.5
/// records seed 42's main-line median as 22 and this probe reads 23 under
/// `pct`. Reporting both settles which of "the two measurements disagree" and
/// "the two conventions disagree" is true, from the data rather than from an
/// argument — see the module header's reproduction note.
fn pct_floor(sorted: &[u32], q: f64) -> f64 {
    if sorted.is_empty() {
        return f64::NAN;
    }
    let i = ((sorted.len() - 1) as f64 * q).floor() as usize;
    f64::from(sorted[i])
}

/// A short spelling for a terminating band, including the "realized nothing"
/// case. Exhaustive over [`Band`]: a sixth rung fails this to compile
/// rather than being folded into a neighbour.
fn band_word(rank: Option<u8>) -> String {
    match rank {
        None => "(none)".to_string(),
        Some(r) => rungs()
            .iter()
            .find(|&&rung| rung_rank(rung) == Some(r))
            .map(|rung| format!("{rung:?}"))
            .unwrap_or_else(|| "(off-ladder)".to_string()),
    }
}

// --- The frozen branch tables, encoded ------------------------------------

/// §4.3's oatmeal criterion — the campaign's own falsification criterion.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum OatmealVerdict {
    /// One character holds strictly more than 80% of branches. FAILED, and it
    /// is the headline.
    Failed,
    /// No character exceeds 80%. Report the full distribution, and the joint
    /// (character x terminating band) table, which is the campaign's product.
    Varied,
}

/// Apply §4.3's table to a set of per-character shares.
fn classify_oatmeal(shares: &[f64]) -> OatmealVerdict {
    if shares.iter().any(|&s| s > OATMEAL_CEILING) {
        OatmealVerdict::Failed
    } else {
        OatmealVerdict::Varied
    }
}

/// C.5's main-line depth table.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum DepthVerdict {
    /// Median in `[5, 40]` and no system above 50 — proceed.
    Proceed,
    /// Median below 5 — the frozen ranges are not producing depth.
    RangesNotProducingDepth,
    /// Some system reads above 50 — the ranges cannot produce that, so the
    /// measurement is summing branches into the spine. STOP.
    MeasurementSumsBranches,
    /// Median above 40 with every system at or under 50. C.5's table names no
    /// arm here; recorded rather than rounded into `Proceed`.
    UnspecifiedByTheBrief,
}

/// Apply C.5's table. The `> 50` arm is tested FIRST on purpose: it says the
/// instrument is wrong, and an instrument's verdict about itself outranks the
/// verdict it would otherwise deliver about the world.
fn classify_depth(median: f64, max: u32) -> DepthVerdict {
    if max > MAIN_LINE_DEPTH_CEILING {
        DepthVerdict::MeasurementSumsBranches
    } else if median < DEPTH_FLOOR {
        DepthVerdict::RangesNotProducingDepth
    } else if median <= DEPTH_MEDIAN_CEILING {
        DepthVerdict::Proceed
    } else {
        DepthVerdict::UnspecifiedByTheBrief
    }
}

/// C.1's branch-count table, applied to the PER-ENTRANCE draw.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum BranchCountVerdict {
    /// The mode is 1 — a spine with occasional side-descents.
    ModeIsOne,
    /// The mode is above 1 — the tree is not a tree.
    TreeIsNotATree,
    /// **The histogram has no unique mode.** Two or more counts tie for the
    /// top, so "the mode is 1" is neither true nor false. C.1's table names no
    /// arm for it, exactly as C.5's names none for `median > 40 with
    /// max <= 50`, and it is recorded here rather than resolved by a
    /// tie-break — this classifier used to break the tie toward the PASSING
    /// arm, which is a tie silently rounded into a green verdict.
    ///
    /// It does not bind today: the top two counts are 58.37% and 27.08%. It is
    /// the only classifier in this file where a tie is possible at all.
    UnspecifiedByTheBrief,
}

/// Apply C.1's table to a histogram indexed by count.
///
/// A tie for the top is [`BranchCountVerdict::UnspecifiedByTheBrief`], never
/// broken toward an arm — see that variant.
fn classify_branch_count(hist: &BTreeMap<u8, usize>) -> BranchCountVerdict {
    let Some(top) = hist.values().copied().max() else {
        return BranchCountVerdict::UnspecifiedByTheBrief;
    };
    let modes: Vec<u8> = hist
        .iter()
        .filter(|&(_, n)| *n == top)
        .map(|(count, _)| *count)
        .collect();
    match modes.as_slice() {
        [1] => BranchCountVerdict::ModeIsOne,
        [_] => BranchCountVerdict::TreeIsNotATree,
        _ => BranchCountVerdict::UnspecifiedByTheBrief,
    }
}

/// B.7's table for `P(Nadir access | branch reached Underdeep)`.
///
/// **The table overlaps** — `~0%` is a subset of `0-10%` — so this encodes
/// the only self-consistent reading: exactly zero is unreachable, everything
/// else at or under 10% is the intent. See the module header.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum NadirGateVerdict {
    /// `(0%, 10%]` — the intent. Report the exact rate.
    TheIntent,
    /// `> 10%` — the gate is not gating.
    GateNotGating,
    /// Exactly 0 — the endgame is unreachable, which is the same defect as an
    /// ungated one. STOP.
    EndgameUnreachable,
}

/// Apply B.7's table.
fn classify_nadir_gate(rate: f64) -> NadirGateVerdict {
    if rate == 0.0 {
        NadirGateVerdict::EndgameUnreachable
    } else if rate <= NADIR_GATE_CEILING {
        NadirGateVerdict::TheIntent
    } else {
        NadirGateVerdict::GateNotGating
    }
}

// --- The reading ----------------------------------------------------------

/// One realized branch column of one system's canonical lattice.
struct BranchReading {
    /// The branch's character — `character_of(seed, cell, 0, branch)`.
    character: Character,
    /// The branch's barrier state — the derived default, unpinned.
    barrier: BarrierState,
    /// The deepest rank at which this branch realizes at least one chamber,
    /// or `None` if every existence draw on the column failed.
    terminating_band: Option<u8>,
    /// The branch realizes at least one chamber at the Underdeep rank.
    lattice_underdeep: bool,
    /// The branch realizes at least one chamber at the Nadir rank.
    lattice_nadir: bool,
    /// A chamber of this branch at the Underdeep rank is reachable from an
    /// open mouth through `passages_from`.
    reached_underdeep: bool,
    /// A chamber of this branch at the Nadir rank is reachable likewise.
    reached_nadir: bool,
    /// The Underdeep rank is reachable when the walk is seeded from
    /// **entrance 0's mouth alone** — the surface head, which is the only
    /// route the "descend the spine" story describes. The route ablation
    /// behind this module's mechanism note.
    reached_underdeep_head_only: bool,
    /// The Nadir rank is reachable from the surface head alone, likewise.
    reached_nadir_head_only: bool,
    /// `bands_of(character)` contains `Nadir` — the character table's own
    /// verdict, which `chamber_exists` never consults.
    table_admits_nadir: bool,
}

/// One cave system's reading.
struct SystemReading {
    /// Drawn aperture count.
    entrances: u8,
    /// The per-entrance branch draw, one entry per entrance — C.1's GATED
    /// quantity.
    branch_counts: Vec<u8>,
    /// The per-system total, summed across entrances — REPORTED only.
    branch_total: u32,
    /// The spine: floors drawn on branch 0 across every band the rock admits.
    main_line_depth: u32,
    /// Main line plus every realized branch of the canonical lattice.
    total_floors: u32,
    /// Mouths that pass `chamber_exists` — the doors that are actually open.
    open_mouths: usize,
    /// Chambers reachable from those mouths by `passages_from`. Cross-checks
    /// directly against the committed witness's `reachable ... from ... open
    /// entrances` line.
    reachable_chambers: usize,
    /// THE DEEP-MOUTH CENSUS: the band each NON-HEAD mouth (`entrance > 0`)
    /// opens into, as drawn, before `chamber_exists` adjudicates it. A side
    /// entrance is mapped by `entrance_mouth` through `root_floor_of`, whose
    /// band pick is uniform over the parent's realized bands — so this is
    /// where the deep-mouth mechanism is visible directly.
    mouth_bands: Vec<u8>,
    /// The same, restricted to mouths that survive `chamber_exists` — the
    /// doors that are actually open.
    open_mouth_bands: Vec<u8>,
    /// Non-head entrances whose mouth fell back to the surface head (a
    /// width-one system, or a side branch that roots nowhere). Counted so the
    /// deep-mouth denominator is visibly the DRAWN population, not the roster.
    head_fallbacks: usize,
    /// The realized branch columns of the canonical lattice.
    branches: Vec<BranchReading>,
}

/// Chambers reachable from any open mouth of one system, by `passages_from`,
/// with the mouths seeded into ONE shared set (C.3's union rule, the shape
/// `underworld_readout::reachable_union` uses).
fn reachable_union(
    seed: Seed,
    cave: &Cave,
    gradient: GeothermalGradient,
    mouths: &[ChamberAddr],
) -> BTreeSet<ChamberAddr> {
    let mut seen: BTreeSet<ChamberAddr> = BTreeSet::new();
    let mut queue: Vec<ChamberAddr> = Vec::new();
    for &entry in mouths {
        if seen.insert(entry) {
            queue.push(entry);
        }
    }
    while let Some(addr) = queue.pop() {
        for next in passages_from(seed, cave, gradient, addr) {
            if seen.insert(next) {
                queue.push(next);
            }
        }
    }
    seen
}

/// Read one cave system through the shipped entry points only.
fn read_system(
    seed: Seed,
    cell: CellId,
    cave: &Cave,
    gradient: GeothermalGradient,
    bands: &[(u8, Band)],
) -> SystemReading {
    let deepest = rung_rank(rung_at_depth(cave.depth_reach_m, gradient))
        .expect("rung_at_depth never returns Surface");
    let underdeep = rank_of(Band::Underdeep);
    let nadir = rank_of(Band::Nadir);

    let entrances = entrance_count(seed, cell);
    let branch_counts: Vec<u8> = (0..entrances)
        .map(|e| branch_count_of(seed, cell, e))
        .collect();
    let branch_total: u32 = branch_counts.iter().map(|&c| u32::from(c)).sum();

    // The canonical lattice is `(cell, entrance 0)` — `entrance_mouth`'s own
    // doc. Every mouth addresses into it.
    let width = branch_count_of(seed, cell, 0);

    // Reachability: seed every OPEN mouth into one shared walk. `chamber_at`
    // is the readout's openness test but needs a stratum column; the
    // existence half of its verdict is `chamber_exists`, which is the gate
    // this probe is about.
    // Each mouth is paired with the entrance index that produced it —
    // `entrance` left `ChamberAddr` entirely (The Drift, amendment A.3), so
    // an address can no longer carry its own aperture index; the pairing
    // that used to ride inside `ChamberAddr::entrance` is now explicit here.
    let all_mouths: Vec<(u8, ChamberAddr)> = (0..entrances)
        .map(|e| {
            let m = entrance_mouth(seed, cell, e);
            let band =
                Band::from_rank(m.band).expect("entrance_mouth only names a habitation rank");
            (
                e,
                ChamberAddr {
                    cell,
                    branch: m.branch,
                    band,
                    level: m.floor,
                },
            )
        })
        .collect();
    // THE DEEP-MOUTH CENSUS, taken before adjudication, over the mouths that
    // actually carry a draw. TWO exclusions, and the second decides the
    // denominator:
    //
    //  - entrance 0 is the surface head BY DEFINITION (`entrance_mouth`
    //    returns `HEAD` with no draw at all), so it is not a drawn mouth;
    //  - a non-zero entrance ALSO returns `HEAD` when the system has width 1,
    //    or when its picked side branch roots nowhere. Those are fallbacks,
    //    not deep-mouth draws, and they are `branch == 0` — the only way a
    //    mouth can carry branch 0, since `root_floor_of` refuses branch 0.
    //
    // So a DRAWN side-branch mouth is exactly `entrance > 0 && branch > 0`.
    // Folding the fallbacks in would put ~2/3 of the population at band 0 and
    // understate the deep share threefold.
    let head_fallbacks = all_mouths
        .iter()
        .filter(|(e, m)| *e > 0 && m.branch == 0)
        .count();
    let mouth_bands: Vec<u8> = all_mouths
        .iter()
        .filter(|(e, m)| *e > 0 && m.branch > 0)
        .map(|(_, m)| {
            m.band
                .rank()
                .expect("a chamber's band is a habitation rank")
        })
        .collect();
    let mouths_by_entrance: Vec<(u8, ChamberAddr)> = all_mouths
        .into_iter()
        .filter(|&(_, entry)| chamber_exists(seed, cave, gradient, entry))
        .collect();
    let open_mouth_bands: Vec<u8> = mouths_by_entrance
        .iter()
        .filter(|(e, m)| *e > 0 && m.branch > 0)
        .map(|(_, m)| {
            m.band
                .rank()
                .expect("a chamber's band is a habitation rank")
        })
        .collect();
    let mouths: Vec<ChamberAddr> = mouths_by_entrance.iter().map(|&(_, m)| m).collect();
    let reached = reachable_union(seed, cave, gradient, &mouths);
    let reached_branch_bands: BTreeSet<(u8, u8)> = reached
        .iter()
        .map(|a| {
            (
                a.branch,
                a.band
                    .rank()
                    .expect("a chamber's band is a habitation rank"),
            )
        })
        .collect();

    // THE ROUTE ABLATION. The same walk seeded from entrance 0's mouth alone
    // — the surface head, `EntranceMouth { branch: 0, band: 0, floor: 0 }` by
    // definition. Every other mouth is a SIDE entrance, which
    // `entrance_mouth` maps through `root_floor_of` to a band drawn uniformly
    // over the parent's realized bands, so it can open directly into any band
    // including the Nadir. Restricting to the head is therefore exactly the
    // "you must descend the whole spine" story, and the gap between the two
    // walks is how much of the deep reach does NOT come from descending.
    let head_mouths: Vec<ChamberAddr> = mouths_by_entrance
        .iter()
        .filter(|(e, _)| *e == 0)
        .map(|&(_, m)| m)
        .collect();
    let reached_head = reachable_union(seed, cave, gradient, &head_mouths);
    let head_branch_bands: BTreeSet<(u8, u8)> = reached_head
        .iter()
        .map(|a| {
            (
                a.branch,
                a.band
                    .rank()
                    .expect("a chamber's band is a habitation rank"),
            )
        })
        .collect();

    let mut main_line_depth = 0u32;
    let mut total_floors = 0u32;
    let mut branches = Vec::new();

    for branch in 0..width {
        let mut terminating: Option<u8> = None;
        let mut realized: BTreeSet<u8> = BTreeSet::new();
        for &(rank, rung) in bands {
            if rank > deepest {
                break;
            }
            let run = RunAddr {
                cell,
                branch,
                band: rung,
            };
            let levels = levels_in_branch(seed, run);
            total_floors += u32::from(levels);
            if branch == 0 {
                main_line_depth += u32::from(levels);
            }
            let occupied = (0..levels).any(|level| {
                chamber_exists(
                    seed,
                    cave,
                    gradient,
                    ChamberAddr {
                        cell,
                        branch,
                        band: rung,
                        level,
                    },
                )
            });
            if occupied {
                terminating = Some(rank);
                realized.insert(rank);
            }
        }
        let character = character_of(seed, cell, 0, branch);
        branches.push(BranchReading {
            character,
            barrier: barrier_of(seed, cell, 0, branch, &BarrierPins::default()),
            terminating_band: terminating,
            lattice_underdeep: realized.contains(&underdeep),
            lattice_nadir: realized.contains(&nadir),
            reached_underdeep: reached_branch_bands.contains(&(branch, underdeep)),
            reached_nadir: reached_branch_bands.contains(&(branch, nadir)),
            reached_underdeep_head_only: head_branch_bands.contains(&(branch, underdeep)),
            reached_nadir_head_only: head_branch_bands.contains(&(branch, nadir)),
            table_admits_nadir: bands_of(character).contains(&Band::Nadir),
        });
    }

    SystemReading {
        entrances,
        branch_counts,
        branch_total,
        main_line_depth,
        total_floors,
        open_mouths: mouths.len(),
        reachable_chambers: reached.len(),
        mouth_bands,
        open_mouth_bands,
        head_fallbacks,
        branches,
    }
}

/// Everything this probe tallies, per seed and pooled.
#[derive(Default)]
struct Tallies {
    /// Cave-bearing land cells — the system denominator.
    systems: usize,
    /// Ocean cells carrying a cave, excluded and counted so the choice is
    /// visible rather than implied.
    ocean_caves: usize,
    /// **Every** land cell, cave-bearing or not — the population [`systems`]
    /// is drawn FROM, not a disjoint remainder. `land_cells - systems` is the
    /// cave-free half; the two printed numbers must never be added.
    ///
    /// [`systems`]: Tallies::systems
    land_cells: usize,
    /// Realized branch columns of every canonical lattice — the branch
    /// denominator.
    branches: usize,
    /// Branch counts as DRAWN, one entry per entrance (C.1's gated quantity).
    branch_count_per_entrance: BTreeMap<u8, usize>,
    /// Branch counts summed per system across entrances (reported only).
    branch_total_per_system: BTreeMap<u32, usize>,
    /// Entrance counts per system.
    entrances_per_system: BTreeMap<u8, usize>,
    /// Share of branches by character.
    by_character: BTreeMap<Character, usize>,
    /// Share of branches by barrier state.
    by_barrier: BTreeMap<BarrierState, usize>,
    /// The joint (character x terminating band) table — the campaign's
    /// product.
    joint: BTreeMap<(Character, Option<u8>), usize>,
    /// Main-line depth per system.
    depths: Vec<u32>,
    /// Total floors per system.
    totals: Vec<u32>,
    /// Branches realizing a chamber at the Underdeep rank.
    lattice_underdeep: usize,
    /// **CONDITIONED.** Of [`lattice_underdeep`], those also realizing a
    /// chamber at the Nadir rank — B.7's numerator, conditioned exactly as
    /// its denominator is.
    ///
    /// [`lattice_underdeep`]: Tallies::lattice_underdeep
    lattice_nadir_given_underdeep: usize,
    /// **UNCONDITIONAL.** Every branch realizing a Nadir chamber, whether or
    /// not it realizes an Underdeep one. This is the population figure, and
    /// it is a different number: nesting it inside the Underdeep test — which
    /// is what this probe did until review round 1 — silently drops every
    /// branch that reaches the bottom without the layer above.
    lattice_nadir_all: usize,
    /// Branches with a mouth-reachable Underdeep chamber.
    reached_underdeep: usize,
    /// **CONDITIONED.** Of [`reached_underdeep`], those also reaching a Nadir
    /// chamber — B.7's graph numerator, conditioned like its denominator.
    ///
    /// [`reached_underdeep`]: Tallies::reached_underdeep
    reached_nadir_given_underdeep: usize,
    /// **UNCONDITIONAL.** Every branch reaching a Nadir chamber by walking.
    /// The headline number, and the one the chronicle quotes.
    reached_nadir_all: usize,
    /// Of [`reached_nadir_all`], those reaching the Nadir WITHOUT reaching
    /// the Underdeep on the same branch — arriving at the bottom without
    /// passing through the layer above. This is the population the old nested
    /// counter was deleting, and it is the majority of it.
    ///
    /// [`reached_nadir_all`]: Tallies::reached_nadir_all
    reached_nadir_without_underdeep: usize,
    /// THE ROUTE ABLATION: branches reaching the Underdeep when the walk is
    /// seeded from **entrance 0's mouth alone** (the surface head).
    reached_underdeep_head_only: usize,
    /// THE ROUTE ABLATION: branches reaching the Nadir from the surface head
    /// alone. The evidence for this module's mechanism note.
    reached_nadir_head_only: usize,
    /// Underdeep-realizing branches whose CHARACTER table admits `Nadir`.
    table_admits_nadir: usize,
    /// Mouths that pass `chamber_exists`, summed over systems.
    open_mouths: usize,
    /// Chambers reachable from an open mouth, summed over systems — the
    /// cross-check against the committed witness.
    reachable_chambers: usize,
    /// THE DEEP-MOUTH CENSUS, by band: every DRAWN side-branch mouth.
    mouth_bands: BTreeMap<u8, usize>,
    /// The same, restricted to mouths surviving `chamber_exists`.
    open_mouth_bands: BTreeMap<u8, usize>,
    /// Non-head entrances that fell back to the surface head, excluded from
    /// the census above and counted here so the exclusion is visible.
    head_fallbacks: usize,
}

impl Tallies {
    /// Fold one system's reading in.
    fn absorb(&mut self, sys: &SystemReading) {
        self.systems += 1;
        *self.entrances_per_system.entry(sys.entrances).or_default() += 1;
        for &c in &sys.branch_counts {
            *self.branch_count_per_entrance.entry(c).or_default() += 1;
        }
        *self
            .branch_total_per_system
            .entry(sys.branch_total)
            .or_default() += 1;
        self.depths.push(sys.main_line_depth);
        self.totals.push(sys.total_floors);
        self.open_mouths += sys.open_mouths;
        self.reachable_chambers += sys.reachable_chambers;
        self.head_fallbacks += sys.head_fallbacks;
        for &band in &sys.mouth_bands {
            *self.mouth_bands.entry(band).or_default() += 1;
        }
        for &band in &sys.open_mouth_bands {
            *self.open_mouth_bands.entry(band).or_default() += 1;
        }
        for b in &sys.branches {
            self.branches += 1;
            *self.by_character.entry(b.character).or_default() += 1;
            *self.by_barrier.entry(b.barrier).or_default() += 1;
            *self
                .joint
                .entry((b.character, b.terminating_band))
                .or_default() += 1;
            // CONDITIONED (B.7's pair): numerator and denominator conditioned
            // alike. UNCONDITIONAL: over every realized branch. Both are kept
            // because they answer different questions, and they are counted in
            // separate statements so neither can be read as the other.
            if b.lattice_underdeep {
                self.lattice_underdeep += 1;
                if b.lattice_nadir {
                    self.lattice_nadir_given_underdeep += 1;
                }
                if b.table_admits_nadir {
                    self.table_admits_nadir += 1;
                }
            }
            if b.lattice_nadir {
                self.lattice_nadir_all += 1;
            }
            if b.reached_underdeep {
                self.reached_underdeep += 1;
                if b.reached_nadir {
                    self.reached_nadir_given_underdeep += 1;
                }
            }
            if b.reached_nadir {
                self.reached_nadir_all += 1;
                if !b.reached_underdeep {
                    self.reached_nadir_without_underdeep += 1;
                }
            }
            if b.reached_underdeep_head_only {
                self.reached_underdeep_head_only += 1;
            }
            if b.reached_nadir_head_only {
                self.reached_nadir_head_only += 1;
            }
        }
    }

    /// Merge a seed's tallies into the pool.
    fn merge(&mut self, other: &Tallies) {
        self.systems += other.systems;
        self.ocean_caves += other.ocean_caves;
        self.land_cells += other.land_cells;
        self.branches += other.branches;
        for (k, v) in &other.branch_count_per_entrance {
            *self.branch_count_per_entrance.entry(*k).or_default() += v;
        }
        for (k, v) in &other.branch_total_per_system {
            *self.branch_total_per_system.entry(*k).or_default() += v;
        }
        for (k, v) in &other.entrances_per_system {
            *self.entrances_per_system.entry(*k).or_default() += v;
        }
        for (k, v) in &other.by_character {
            *self.by_character.entry(*k).or_default() += v;
        }
        for (k, v) in &other.by_barrier {
            *self.by_barrier.entry(*k).or_default() += v;
        }
        for (k, v) in &other.joint {
            *self.joint.entry(*k).or_default() += v;
        }
        self.depths.extend_from_slice(&other.depths);
        self.totals.extend_from_slice(&other.totals);
        self.lattice_underdeep += other.lattice_underdeep;
        self.lattice_nadir_given_underdeep += other.lattice_nadir_given_underdeep;
        self.lattice_nadir_all += other.lattice_nadir_all;
        self.reached_underdeep += other.reached_underdeep;
        self.reached_nadir_given_underdeep += other.reached_nadir_given_underdeep;
        self.reached_nadir_all += other.reached_nadir_all;
        self.reached_nadir_without_underdeep += other.reached_nadir_without_underdeep;
        self.reached_underdeep_head_only += other.reached_underdeep_head_only;
        self.reached_nadir_head_only += other.reached_nadir_head_only;
        self.table_admits_nadir += other.table_admits_nadir;
        self.open_mouths += other.open_mouths;
        self.reachable_chambers += other.reachable_chambers;
        self.head_fallbacks += other.head_fallbacks;
        for (k, v) in &other.mouth_bands {
            *self.mouth_bands.entry(*k).or_default() += v;
        }
        for (k, v) in &other.open_mouth_bands {
            *self.open_mouth_bands.entry(*k).or_default() += v;
        }
    }

    /// The per-character shares, in [`CHARACTERS`] order.
    fn character_shares(&self) -> Vec<f64> {
        CHARACTERS
            .iter()
            .map(|c| {
                self.by_character.get(c).copied().unwrap_or(0) as f64 / self.branches.max(1) as f64
            })
            .collect()
    }

    /// **The headline rate**: the share of all realized branches that reach a
    /// Nadir chamber by walking, unconditionally. This is the quantity
    /// [`NADIR_WALK_RATE_FLOOR`]/[`NADIR_WALK_RATE_CEILING`] ratchet.
    fn nadir_walk_rate(&self) -> f64 {
        self.reached_nadir_all as f64 / self.branches.max(1) as f64
    }

    /// Print every table this probe reports, under `label`.
    ///
    /// `witness_covers` says whether the committed witness
    /// (`docs/audits/underworld-lattice-seed-panel.md`) carries a comparable
    /// line for this label. It prints **per seed** and nothing else, so the
    /// POOLED row cannot claim corroboration that does not exist: the witness
    /// has no pooled pair, and this probe printed the claim unconditionally
    /// until review round 1.
    fn report(&mut self, label: &str, witness_covers: bool) {
        self.depths.sort_unstable();
        self.totals.sort_unstable();
        println!(
            "\n== {label} ==  land cells {} (OF WHICH cave-bearing: {} — the two are \
             nested, never summed; {} land cells carry no cave)  realized branches {}  \
             (ocean cells carrying a cave: {}, excluded)",
            self.land_cells,
            self.systems,
            self.land_cells.saturating_sub(self.systems),
            self.branches,
            self.ocean_caves
        );
        // WHAT THIS CORROBORATES IS THE WALK, AND ONLY THE WALK. The witness
        // reports the same reachable/open-mouth pair from a different call
        // site, so agreement pins this probe's traversal. It says nothing
        // about the Nadir tallies below, which is where review round 1 found
        // the defect — attaching it to those was the mistake.
        if witness_covers {
            println!(
                "  reachability cross-check (corroborates THE WALK, not the Nadir \
                 tallies below): {} chambers reachable from {} open mouths — the \
                 committed witness `docs/audits/underworld-lattice-seed-panel.md` \
                 prints this pair per seed",
                self.reachable_chambers, self.open_mouths
            );
        } else {
            println!(
                "  reachability cross-check: {} chambers reachable from {} open mouths \
                 (POOLED — the committed witness prints per-seed pairs only, so no \
                 committed line corroborates this row)",
                self.reachable_chambers, self.open_mouths
            );
        }

        println!("  -- branch count PER ENTRANCE (C.1, GATED) --  denominator: entrances drawn");
        let entrances: usize = self.branch_count_per_entrance.values().sum();
        for (count, n) in &self.branch_count_per_entrance {
            println!(
                "     {count} branch(es)  {n:>7}  {:>6.2}%",
                *n as f64 / entrances.max(1) as f64 * 100.0
            );
        }
        println!(
            "     verdict {:?}",
            classify_branch_count(&self.branch_count_per_entrance)
        );

        println!("  -- branch count PER SYSTEM (reported) --  denominator: cave systems");
        for (total, n) in &self.branch_total_per_system {
            println!(
                "     {total:>2} branch(es) {n:>7}  {:>6.2}%",
                *n as f64 / self.systems.max(1) as f64 * 100.0
            );
        }

        println!("  -- entrances per system (reported) --  denominator: cave systems");
        for (count, n) in &self.entrances_per_system {
            println!(
                "     {count} entrance(s) {n:>7}  {:>6.2}%",
                *n as f64 / self.systems.max(1) as f64 * 100.0
            );
        }

        println!(
            "  -- share of branches by CHARACTER (§4.3, GATED) --  denominator: realized branches"
        );
        for c in CHARACTERS {
            let n = self.by_character.get(c).copied().unwrap_or(0);
            println!(
                "     {:<15} {n:>7}  {:>6.2}%",
                format!("{c:?}"),
                n as f64 / self.branches.max(1) as f64 * 100.0
            );
        }
        println!(
            "     verdict {:?}",
            classify_oatmeal(&self.character_shares())
        );

        println!("  -- share of branches by BARRIER STATE (reported, NEVER gated) --");
        for (state, n) in &self.by_barrier {
            println!(
                "     {:<10} {n:>7}  {:>6.2}%",
                format!("{state:?}"),
                *n as f64 / self.branches.max(1) as f64 * 100.0
            );
        }

        println!("  -- MAIN-LINE DEPTH (C.5, GATED) --  denominator: cave systems");
        println!(
            "     p10 {:.1}  p25 {:.1}  median {:.1} (floor-index {:.1})  p75 {:.1}  \
             p90 {:.1}  max {}  min {}",
            pct(&self.depths, 0.10),
            pct(&self.depths, 0.25),
            pct(&self.depths, 0.50),
            pct_floor(&self.depths, 0.50),
            pct(&self.depths, 0.75),
            pct(&self.depths, 0.90),
            self.depths.last().copied().unwrap_or(0),
            self.depths.first().copied().unwrap_or(0),
        );
        println!(
            "     verdict {:?}",
            classify_depth(
                pct(&self.depths, 0.50),
                self.depths.last().copied().unwrap_or(0)
            )
        );

        println!("  -- TOTAL FLOORS PER SYSTEM (C.5, REPORTED, never gated) --");
        println!(
            "     p10 {:.1}  p25 {:.1}  median {:.1}  p75 {:.1}  p90 {:.1}  max {}",
            pct(&self.totals, 0.10),
            pct(&self.totals, 0.25),
            pct(&self.totals, 0.50),
            pct(&self.totals, 0.75),
            pct(&self.totals, 0.90),
            self.totals.last().copied().unwrap_or(0),
        );

        println!(
            "  -- P(Nadir access | branch reached Underdeep) (B.7) — CONDITIONED: \
             numerator and denominator alike --"
        );
        let lattice =
            self.lattice_nadir_given_underdeep as f64 / self.lattice_underdeep.max(1) as f64;
        let graph =
            self.reached_nadir_given_underdeep as f64 / self.reached_underdeep.max(1) as f64;
        let table = self.table_admits_nadir as f64 / self.lattice_underdeep.max(1) as f64;
        println!(
            "     lattice existence  {:>6}/{:<6} = {:>6.2}%   verdict {:?}",
            self.lattice_nadir_given_underdeep,
            self.lattice_underdeep,
            lattice * 100.0,
            classify_nadir_gate(lattice)
        );
        println!(
            "     graph reachability {:>6}/{:<6} = {:>6.2}%   verdict {:?}",
            self.reached_nadir_given_underdeep,
            self.reached_underdeep,
            graph * 100.0,
            classify_nadir_gate(graph)
        );
        println!(
            "     character table    {:>6}/{:<6} = {:>6.2}%   verdict {:?}",
            self.table_admits_nadir,
            self.lattice_underdeep,
            table * 100.0,
            classify_nadir_gate(table)
        );
        // The conditional above has a denominator that is itself the finding.
        // Printed UNCONDITIONALLY as well, because a conditional read off a
        // near-empty denominator is exactly the shape that reads as reassuring
        // and is not. No preregistered table covers these; they are reported,
        // not classified — except the walked Nadir rate, which carries the
        // ratchet below because it is the headline.
        //
        // EVERY NUMERATOR HERE IS UNCONDITIONAL. Until review round 1 the two
        // Nadir counters were nested inside the Underdeep test and printed
        // under this heading, so the number labelled unconditional was a
        // conditioned count over an unconditional denominator.
        println!(
            "  -- UNCONDITIONAL, over all realized branches (separate counters from \
             the conditioned pair above) --"
        );
        println!(
            "       reaches Underdeep by walking  {:>6}/{:<6} = {:>6.3}%",
            self.reached_underdeep,
            self.branches,
            self.reached_underdeep as f64 / self.branches.max(1) as f64 * 100.0
        );
        println!(
            "       reaches Nadir     by walking  {:>6}/{:<6} = {:>6.3}%   <- THE HEADLINE",
            self.reached_nadir_all,
            self.branches,
            self.nadir_walk_rate() * 100.0
        );
        println!(
            "         ... of those, WITHOUT reaching the Underdeep on the same branch: \
             {}/{} — arriving at the bottom without passing through the layer above",
            self.reached_nadir_without_underdeep, self.reached_nadir_all
        );
        println!(
            "       realizes Nadir in the lattice {:>6}/{:<6} = {:>6.3}%",
            self.lattice_nadir_all,
            self.branches,
            self.lattice_nadir_all as f64 / self.branches.max(1) as f64 * 100.0
        );
        // THE ROUTE ABLATION — the mechanism evidence. If the deep reach were
        // the spine being descended, restricting the walk to the surface head
        // would barely move it.
        println!(
            "       ROUTE ABLATION, walk seeded from ENTRANCE 0's mouth alone (the \
             surface head):"
        );
        println!(
            "         reaches Underdeep {:>6}/{:<6} = {:>6.3}%  ({:>5.1}% of the \
             all-mouths count)",
            self.reached_underdeep_head_only,
            self.branches,
            self.reached_underdeep_head_only as f64 / self.branches.max(1) as f64 * 100.0,
            self.reached_underdeep_head_only as f64 / self.reached_underdeep.max(1) as f64 * 100.0
        );
        println!(
            "         reaches Nadir     {:>6}/{:<6} = {:>6.3}%  ({:>5.1}% of the \
             all-mouths count)",
            self.reached_nadir_head_only,
            self.branches,
            self.reached_nadir_head_only as f64 / self.branches.max(1) as f64 * 100.0,
            self.reached_nadir_head_only as f64 / self.reached_nadir_all.max(1) as f64 * 100.0
        );
        // THE DEEP-MOUTH CENSUS — the other half of the mechanism evidence,
        // and the half that names the lever. `entrance_mouth` maps a side
        // entrance to its branch's root floor, and `root_floor_of` picks that
        // band UNIFORM over the parent's realized bands (all five today, since
        // every frozen range bottoms out at 1). So roughly one side entrance
        // in five is a door that opens directly into the Nadir band.
        let non_head: usize = self.mouth_bands.values().sum();
        println!(
            "       DEEP-MOUTH CENSUS: {non_head} DRAWN side-branch mouths (entrance > 0 \
             AND branch > 0; a further {} non-head entrances fell back to the surface \
             head and are excluded), by the band `entrance_mouth` maps them to — \
             DRAWN / surviving `chamber_exists`:",
            self.head_fallbacks
        );
        for (band, n) in &self.mouth_bands {
            let open = self.open_mouth_bands.get(band).copied().unwrap_or(0);
            println!(
                "         band {band} {:<12} {n:>5} {:>6.2}%   open {open:>5}",
                band_word(Some(*band)),
                *n as f64 / non_head.max(1) as f64 * 100.0
            );
        }

        println!("  -- JOINT (character x terminating band) — the campaign's product --");
        println!(
            "     {:<15} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10}",
            "character", "Undercroft", "Shallows", "Deeps", "Underdeep", "Nadir", "(none)"
        );
        for c in CHARACTERS {
            let mut cells: Vec<String> = Vec::new();
            for &(rank, _) in &habitation_bands() {
                let n = self.joint.get(&(*c, Some(rank))).copied().unwrap_or(0);
                cells.push(format!("{n:>10}"));
            }
            cells.push(format!(
                "{:>10}",
                self.joint.get(&(*c, None)).copied().unwrap_or(0)
            ));
            println!("     {:<15} {}", format!("{c:?}"), cells.join(" "));
        }
        println!(
            "     (row totals are the character shares above; band words: {})",
            habitation_bands()
                .iter()
                .map(|&(rank, _)| band_word(Some(rank)))
                .collect::<Vec<_>>()
                .join(", ")
        );
    }
}

/// C.1's tie arm is reachable, and the passing arm is not where a tie lands.
///
/// A pure test over [`classify_branch_count`], costing no world build, because
/// the arm it covers cannot be reached from the shipped panel — the top two
/// counts are 58.37% and 27.08%, so without this the variant would be
/// unexercised code asserting nothing. It is the guard on review round 1's F5:
/// the classifier used to break a 1-vs-2 tie toward `ModeIsOne` through a
/// `Reverse(*count)` in its `max_by_key`, quietly rounding an unspecified
/// reading into C.1's passing arm.
#[test]
fn a_tie_for_the_modal_branch_count_is_not_rounded_into_the_passing_arm() {
    let tied: BTreeMap<u8, usize> = [(1u8, 100usize), (2, 100)].into_iter().collect();
    assert_eq!(
        classify_branch_count(&tied),
        BranchCountVerdict::UnspecifiedByTheBrief,
        "a 1-vs-2 tie must not resolve to an arm C.1 never named"
    );

    // The two arms C.1 DOES name still resolve, so the guard above has not
    // simply swallowed the classifier.
    let one: BTreeMap<u8, usize> = [(1u8, 101usize), (2, 100)].into_iter().collect();
    assert_eq!(classify_branch_count(&one), BranchCountVerdict::ModeIsOne);
    let two: BTreeMap<u8, usize> = [(1u8, 100usize), (2, 101)].into_iter().collect();
    assert_eq!(
        classify_branch_count(&two),
        BranchCountVerdict::TreeIsNotATree
    );
}

/// claim: rate(share of branches by character, by barrier state and by
/// terminating band, and the main-line depth and branch-count distributions;
/// seeds 42 / 7 / 1234) —
/// the campaign's own falsification criterion. Applies §4.3's oatmeal table,
/// C.5's main-line-depth and total-floors tables, C.1's branch-count table and
/// B.7's Nadir-gate table, and **asserts the arm each one lands in**.
///
/// # What would change the verdict
///
/// - `character_of`'s thresholds (`windows/worldgen/src/character.rs`) — the
///   oatmeal share is that draw and nothing else. Widening `WildCave` past
///   80% reddens the oatmeal assertion.
/// - `floors_range` (`windows/worldgen/src/chamber.rs`) — the frozen per-band
///   ranges are the whole of main-line depth. Shrinking them below a median of
///   5 reddens the depth assertion; a change that let one system exceed 50
///   reddens it in the other direction.
/// - `branch_count_of`'s weights — the mode-is-1 assertion.
/// - `rung_at_depth`'s ΔT boundaries, `CAVE_REACH_CEILING_M`, or the
///   gradient clamp — these decide `deepest`, so they move both depth and the
///   Nadir conditional.
/// - `EXISTENCE_DENSITY` — the per-address coin, which decides which of the
///   drawn floors are realized and therefore every terminating band. It also
///   moves the ratcheted headline hard and non-linearly: 0.5 -> 0.7 is 4.4x,
///   0.7 -> 0.9 another 10.5x (see the density sweep in the module header).
/// - `root_floor_of`'s band pick (`windows/worldgen/src/character.rs`) — the
///   uniform-over-realized-bands draw that puts ~1 side entrance in 5 straight
///   into the Nadir band. This is the mechanism behind the walked-Nadir rate,
///   and the one dial that moves it without touching geology or the ladder.
/// - Giving `barrier_of` or `bands_of` a consumer inside `chamber_exists`
///   would make B.7's question answerable for the first time; the Nadir
///   assertions here are the baseline that change must be read against.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn did_the_stope_solve_the_oatmeal_problem() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let bands = habitation_bands();

    let mut pooled = Tallies::default();
    let mut per_seed: Vec<(u64, Tallies)> = Vec::new();

    for seed_value in SEEDS {
        let seed = Seed(seed_value);
        let artifacts = build_world_to_with_artifacts(
            seed,
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
            &wc,
            // Terrain is the deepest rung this probe reads — `cave_at` and
            // `geothermal_gradient_at` are both `GeneratedTerrain`, and the
            // chamber lattice is derived from a seed and those two facts.
            BuildDepth::Terrain,
        )
        .expect("probe seed builds");
        let terrain = artifacts
            .terrain
            .expect("terrain is Some at BuildDepth::Terrain");
        let geo = terrain.geosphere();

        let mut t = Tallies::default();
        for cell in geo.cells() {
            if terrain.is_ocean(cell) {
                if terrain.cave_at(cell).is_some() {
                    t.ocean_caves += 1;
                }
                continue;
            }
            t.land_cells += 1;
            let Some(cave) = terrain.cave_at(cell) else {
                continue;
            };
            let gradient = terrain.geothermal_gradient_at(cell);
            let sys = read_system(seed, cell, &cave, gradient, &bands);
            t.absorb(&sys);
        }

        pooled.merge(&t);
        per_seed.push((seed_value, t));
    }

    for (seed_value, t) in per_seed.iter_mut() {
        t.report(&format!("seed {seed_value}"), true);
    }
    pooled.report("POOLED", false);

    // --- Vacuity guards ----------------------------------------------------
    // Every share below is 0/1 = 0 over an empty corpus, which is
    // indistinguishable from a genuine zero. Each denominator is asserted
    // non-empty before any arm is read off it.
    assert_eq!(
        per_seed.len(),
        SEEDS.len(),
        "every seed in the panel must contribute a reading"
    );
    for (seed_value, t) in &per_seed {
        assert!(
            t.systems > 0,
            "seed {seed_value}: no cave systems — the probe is vacuous"
        );
        assert!(
            t.branches > 0,
            "seed {seed_value}: no realized branch columns — every share below \
             would be 0/1 and read as a finding"
        );
        assert!(
            t.lattice_underdeep > 0,
            "seed {seed_value}: no branch realizes an Underdeep chamber — B.7's \
             conditional has an empty denominator and its arm means nothing"
        );
        assert!(
            t.reached_underdeep > 0,
            "seed {seed_value}: no branch has a mouth-reachable Underdeep \
             chamber — the graph reading of B.7's conditional is vacuous"
        );
        // THE JOINT TABLE'S OWN GUARD. `!joint.is_empty()` — which is what
        // stood here until review round 1 — cannot fail while `branches > 0`,
        // because `absorb` inserts an entry for every branch: it was the
        // branch guard restated, and the campaign's product table had none of
        // its own. Both clauses below CAN fail with branches > 0: a table
        // whose every branch realized nothing lands entirely in the `(none)`
        // column, and a table concentrated in one band is a degenerate product
        // even though the corpus is full.
        let occupied_bands: BTreeSet<u8> = t
            .joint
            .iter()
            .filter(|&(_, n)| *n > 0)
            .filter_map(|(&(_, band), _)| band)
            .collect();
        assert!(
            occupied_bands.len() >= 2,
            "seed {seed_value}: the joint (character x band) table occupies {} \
             terminating band(s) — a product table concentrated in one band (or \
             entirely in the `(none)` column) is degenerate, and a share read \
             off it means nothing. Occupied: {occupied_bands:?}",
            occupied_bands.len()
        );
        for c in CHARACTERS {
            let realized: usize = t
                .joint
                .iter()
                .filter(|&(&(ch, band), _)| ch == *c && band.is_some())
                .map(|(_, n)| *n)
                .sum();
            assert!(
                realized > 0,
                "seed {seed_value}: {c:?} occupies no cell of the joint table with \
                 a realized terminating band — its whole row is `(none)`, so the \
                 table says nothing about where that character bottoms out"
            );
        }
    }
    // The character axis must be occupied, or a 100% share would be a
    // one-member roster rather than oatmeal.
    for c in CHARACTERS {
        assert!(
            pooled.by_character.get(c).copied().unwrap_or(0) > 0,
            "no branch anywhere on the panel draws {c:?} — the character axis \
             has collapsed and the oatmeal share is measuring one member"
        );
    }
    for state in [
        BarrierState::Sealed,
        BarrierState::Warded,
        BarrierState::Thin,
        BarrierState::Open,
    ] {
        assert!(
            pooled.by_barrier.get(&state).copied().unwrap_or(0) > 0,
            "no branch anywhere on the panel draws {state:?} — the barrier \
             distribution has collapsed"
        );
    }

    // --- §4.3: THE OATMEAL CRITERION, the headline -------------------------
    // Asserted per seed AND pooled: a pooled share can hide one seed that went
    // oatmeal, and §4.3 is written about "a world".
    for (seed_value, t) in &per_seed {
        assert_eq!(
            classify_oatmeal(&t.character_shares()),
            OatmealVerdict::Varied,
            "seed {seed_value}: one character holds more than {:.0}% of \
             branches — §4.3 FAILED, and it is the campaign's headline. Do NOT \
             re-weight `character_of` to rescue this without saying so in the \
             chronicle. Shares: {:?}",
            OATMEAL_CEILING * 100.0,
            t.by_character
        );
    }
    assert_eq!(
        classify_oatmeal(&pooled.character_shares()),
        OatmealVerdict::Varied,
        "pooled: one character holds more than {:.0}% of branches — §4.3 FAILED",
        OATMEAL_CEILING * 100.0
    );

    // --- C.5: MAIN-LINE DEPTH ---------------------------------------------
    for (seed_value, t) in &per_seed {
        let median = pct(&t.depths, 0.50);
        let max = t.depths.last().copied().unwrap_or(0);
        assert_eq!(
            classify_depth(median, max),
            DepthVerdict::Proceed,
            "seed {seed_value}: main-line depth median {median:.1}, max {max} — \
             C.5's table no longer selects `Proceed`. Re-read C.5 rather than \
             adjusting these bounds; a reading above {MAIN_LINE_DEPTH_CEILING} \
             means this probe is summing branches into the spine."
        );
    }
    assert_eq!(
        classify_depth(
            pct(&pooled.depths, 0.50),
            pooled.depths.last().copied().unwrap_or(0)
        ),
        DepthVerdict::Proceed,
        "pooled: C.5's main-line-depth table no longer selects `Proceed`"
    );

    // --- C.5: TOTAL FLOORS PER SYSTEM, the ONLY assertion it gets ----------
    // Arithmetic, not a preference: 4 columns x a 50-floor spine.
    let widest = pooled.totals.last().copied().unwrap_or(0);
    assert!(
        widest <= TOTAL_FLOORS_CEILING,
        "a system carries {widest} floors, above the arithmetic maximum of \
         {TOTAL_FLOORS_CEILING} (4 branches x {MAIN_LINE_DEPTH_CEILING}) — the \
         frozen ranges cannot produce that, so the measurement is wrong"
    );

    // --- C.1: BRANCH COUNT, per entrance (Ruling R1) -----------------------
    for (seed_value, t) in &per_seed {
        assert_eq!(
            classify_branch_count(&t.branch_count_per_entrance),
            BranchCountVerdict::ModeIsOne,
            "seed {seed_value}: the modal branch draw is not 1 — the tree is not \
             a tree (C.1). Report as a finding; do NOT re-weight \
             `branch_count_of`. Histogram: {:?}",
            t.branch_count_per_entrance
        );
    }
    assert_eq!(
        classify_branch_count(&pooled.branch_count_per_entrance),
        BranchCountVerdict::ModeIsOne,
        "pooled: the modal branch draw is not 1 (C.1)"
    );

    // --- B.7: P(Nadir access | branch reached Underdeep) -------------------
    // The lattice and character readings carry denominators in the hundreds
    // and are asserted per seed. The GRAPH reading's denominator is 2 / 15 / 10
    // — which is itself the finding, not a sampling accident — so its arm is
    // asserted only on the pool, and the unconditional rate is what the
    // headline should quote.
    for (seed_value, t) in &per_seed {
        let lattice = t.lattice_nadir_given_underdeep as f64 / t.lattice_underdeep as f64;
        let table = t.table_admits_nadir as f64 / t.lattice_underdeep as f64;
        assert_eq!(
            classify_nadir_gate(lattice),
            NadirGateVerdict::GateNotGating,
            "seed {seed_value}: the lattice-existence reading of B.7's \
             conditional is {:.2}% ({}/{}), which is no longer the `>10%` arm. \
             The mechanism note in this module's header explains why that arm \
             is structural; a move off it means something now gates access, and \
             that is a finding to report, not a bound to adjust.",
            lattice * 100.0,
            t.lattice_nadir_given_underdeep,
            t.lattice_underdeep
        );
        assert_eq!(
            classify_nadir_gate(table),
            NadirGateVerdict::GateNotGating,
            "seed {seed_value}: the character-table reading of B.7's conditional \
             is {:.2}% ({}/{})",
            table * 100.0,
            t.table_admits_nadir,
            t.lattice_underdeep
        );
    }
    let graph = pooled.reached_nadir_given_underdeep as f64 / pooled.reached_underdeep as f64;
    assert_eq!(
        classify_nadir_gate(graph),
        NadirGateVerdict::GateNotGating,
        "pooled: the graph-reachability reading of B.7's conditional is {:.2}% \
         ({}/{})",
        graph * 100.0,
        pooled.reached_nadir_given_underdeep,
        pooled.reached_underdeep
    );

    // --- THE HEADLINE, RATCHETED -------------------------------------------
    // The unconditional walked-Nadir rate is the number this probe's finding is
    // actually about, and until review round 1 nothing asserted it. It is
    // banded, not pinned — see NADIR_WALK_RATE_FLOOR for why, and for what
    // would legitimately move it.
    let nadir_walk = pooled.nadir_walk_rate();
    assert!(
        (NADIR_WALK_RATE_FLOOR..=NADIR_WALK_RATE_CEILING).contains(&nadir_walk),
        "the unconditional walked-Nadir rate is {:.4}% ({}/{}), outside the \
         ratcheted band [{:.4}%, {:.4}%] this campaign measured and reported. THIS \
         IS THE HEADLINE NUMBER, so read the doc comment before touching the \
         band. Three changes move it LEGITIMATELY and want a re-baseline with the \
         new figure written into this module's header: `EXISTENCE_DENSITY` (the \
         per-address coin — 0.5 -> 0.7 alone moves this 4.4x), `floors_range`'s \
         frozen per-band ranges (they set how long a run is, and the deep-mouth \
         account depends on the shallow bands staying long), and `root_floor_of`'s \
         band pick (uniform over the parent's realized bands today, which is what \
         puts ~1 side entrance in 5 straight into the Nadir band). A move with \
         NONE of those three touched is a finding, not a bound to widen.",
        nadir_walk * 100.0,
        pooled.reached_nadir_all,
        pooled.branches,
        NADIR_WALK_RATE_FLOOR * 100.0,
        NADIR_WALK_RATE_CEILING * 100.0,
    );

    // --- The reachability cross-check --------------------------------------
    // An independent instrument already reports this pair for the same panel:
    // `docs/audits/underworld-lattice-seed-panel.md`, written by
    // `underworld_readout.rs` through `chamber_at` rather than
    // `chamber_exists`. Agreement is not asserted here — the two use different
    // openness predicates — but the counts are printed side by side so a reader
    // can see this probe's walk is the same walk, and a divergence of the ORDER
    // of magnitude that would change the finding is caught by the bound below.
    assert!(
        pooled.reachable_chambers > 0 && pooled.open_mouths > 0,
        "no chamber is reachable from any mouth anywhere on the panel — the \
         reachability walk is vacuous, not reporting a real zero"
    );
    assert!(
        pooled.reachable_chambers < pooled.totals.iter().sum::<u32>() as usize,
        "every drawn floor is reachable — the walk is not being gated by \
         `chamber_exists` at all and the graph reading above is vacuous"
    );
}
