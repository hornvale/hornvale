//! THE DRIFT, Task 0: **how much of the underworld can a player reach today?**
//!
//! The campaign's baseline, taken *before* any production change, with the
//! instrument that will later judge the change. Nothing here touches
//! production code — every quantity is a pure read over shipped entry points
//! ([`chamber_exists`], [`passages_from`], [`entrance_count`],
//! [`entrance_mouth`]).
//!
//! # WHAT IS MEASURED, AND WHY THE DENOMINATORS ARE WHAT THEY ARE
//!
//! Spec §6 preregisters **four** quantities. Each is stated here in the units
//! this file computes it in, because a share is only as meaningful as its
//! denominator, and each carries the arm table that is actually applied —
//! which for the first quantity is **not** the one §6 was written with:
//!
//! ```text
//! SHARE OF A SYSTEM'S LEVELS REACHABLE FROM ITS OWN ENTRANCES   <- GATED
//!     Per system, over systems with at least one OPEN mouth. §6 gated the
//!     MEDIAN of this distribution; amendment C replaced that with the p10;
//!     amendment D replaced the p10 with the SHARE OF SYSTEMS BELOW 100%
//!     REACHABLE. D's table is the one applied here — see
//!     [`ConnectivityVerdict`] and "§6's GATE WAS REPLACED TWICE" below.
//!         0%        -> the intent: every system fully connected
//!         (0%, 5%]  -> report, with the unreached-by-band histogram
//!         > 5%      -> a connectivity defect survives §4.5's guarantees. STOP
//!
//! SHARE OF ALL EXISTING LEVELS REACHABLE                        <- GATED
//!     The whole-world figure. Levels in systems with NO open entrance stay
//!     inside the denominator on purpose: an unreachable system is a real
//!     outcome and must stay visible rather than be defined away.
//!         >= 90%    -> the intent
//!         <  90%    -> either a connectivity hole survives §4.5's
//!                      guarantees, or entrance-less systems are commoner
//!                      than expected. Report WHICH, with the split.
//!     A RATCHET sits above this arm, tighter than it — see
//!     [`WORLD_SHARE_FLOOR`].
//!
//! LEVELS PER SYSTEM                                             <- REPORTED
//!     §6: "REPORTED, NEVER GATED. The count roughly doubles by construction;
//!     the interesting quantity is the distribution, not the mean." No verdict
//!     type exists for it here, on purpose — see "THE ABSOLUTE COUNTS ARE NOT
//!     COMPARABLE ACROSS THIS CAMPAIGN" below, which is the sentence that must
//!     travel with every one of these numbers.
//!
//! BRANCHES PER BAND                                             <- GATED
//!     The drawn width of a system at a band, over every (system, habitation
//!     band) pair the lattice admits — `branch_count_of`'s own distribution,
//!     which after amendment A.3 answers per BAND rather than per entrance.
//!         mode 1    -> proceed, matching C.1's intent one level down
//!         mode > 1  -> report as a finding; do NOT re-weight to rescue it
//!     A histogram with no unique mode is [`BranchesPerBandVerdict::
//!     UnspecifiedByTheBrief`], never broken toward an arm.
//! ```
//!
//! # §6's GATE WAS REPLACED TWICE, AND BOTH REPLACEMENTS WERE FORCED BY A MUTATION
//!
//! This is the most useful thing the campaign learned about its own
//! instruments, and a reader who sees only the final arm learns none of it.
//!
//! **First replacement — amendment C: the MEDIAN could not fail.** Task 4's
//! reviewer severed lateral movement between branches, a real connectivity
//! break, and measured across the panel:
//!
//! ```text
//!                          whole-world    per-system p10    per-system MEDIAN
//!   unmutated                 100.00%          100.00%            100.00%
//!   lateral moves refused      67.82%           35.14%            100.00%
//! ```
//!
//! The median held at **exactly 100.00% on all three seeds while connectivity
//! was broken**, and the reason is structural rather than unlucky: C.1 of The
//! Stope makes the branch-count mode 1, so most systems have a single branch,
//! so most systems are unaffected by anything that severs *between* branches.
//! A median over a population dominated by one-branch systems cannot see a
//! branch-severing defect at all.
//!
//! **Second replacement — amendment D: the p10 could not fail either, for a
//! narrower but identical reason.** `pct(sorted, 0.10) = sorted[round(0.10 ·
//! (n − 1))]`. With n = 874 systems on seed 42 that index is 87, so p10 can
//! move off 100% only when roughly **≥ 10% of systems** sit below it. Task 7's
//! top-band-orphan defect touched **3.09% / 2.08% / 2.21%** of systems, and
//! the reviewer's independent `entrance_mouth` mutation moved the whole-world
//! share to 99.43 / 99.50 / 99.54%. **p10 read a perfect 100.00% on every seed
//! under both.** C's validation had shown only that p10 *can* move, against a
//! mutation whose population share was never measured and plainly exceeded
//! 10% — the one region where p10 has any resolution at all.
//!
//! The lesson D states, and the reason this section exists: *a replacement
//! statistic must be validated against the defect class the original was blind
//! to, at the population share that class actually has.* Every quantile has a
//! blind zone; choosing one without measuring the blind zone is choosing
//! blind.
//!
//! **The gated arm is therefore the share of systems below 100% reachable.**
//! It has no blind zone: one affected system moves it. It went 0 → 27/35/28
//! under Task 7's defect while every quantile stood still.
//!
//! **p10 and the median are printed here as CONTEXT and are never asserted.**
//! Under both defects measured above they read 100.00% while a real
//! connectivity failure was live, so a quantile in this output says nothing
//! about whether anything was checked. The report labels them, and states
//! their blind zone in the same breath, because a quantile reported without
//! its resolution is the defect amendment D exists to correct.
//!
//! **AND D's OWN ARM HAS A BLIND SPOT, FOUND BY THIS TASK'S CONTROLS.** It is
//! not a fourth replacement — it is the reason §6 gates *two* arms rather than
//! one, made concrete. D's denominator is systems **with an open mouth**, so a
//! system that draws no aperture at all does not fail that arm; it leaves the
//! population. Mutation R3 below takes twenty of seed 42's systems to zero
//! apertures and D's arm reads a clean `Intent` — 0 of 854 — while 575 levels
//! are unreachable and the whole-world share is 98.12%. The two arms answer
//! different questions ("does a system reach its own levels?" versus "is the
//! underworld reachable?") and neither subsumes the other. The pattern across
//! all three findings is the same one D.2 states: **a statistic's blind zone
//! is a property of its denominator, and it must be measured, not assumed.**
//!
//! # THE ABSOLUTE COUNTS ARE NOT COMPARABLE ACROSS THIS CAMPAIGN
//!
//! Spec amendment A.5, and it belongs in the same breath as any count below.
//! Task 0 measured 21,328 levels on seed 42 and today's reading is 30,537 —
//! but the earlier figure is a **sum over per-entrance sublattices** and
//! today's is **one lattice per system**, because amendment A.3 took
//! `entrance` out of the address entirely. The count therefore moves for a
//! structural reason that has nothing to do with §4.1's deleted coin, in both
//! directions at different tasks (21,328 → 42,820 → 30,272 → 30,537). The
//! per-system and whole-world SHARES are the comparable quantities, which is
//! why §6 gates on shares and not on counts. Reporting "levels went down"
//! without this paragraph inverts the campaign's own story.
//!
//! **ONE WALK PER SYSTEM, FROM THE UNION OF ITS OPEN MOUTHS — never a sum of
//! per-entrance walks.** Two mouths into one lattice reach overlapping sets,
//! and adding them counts a level twice. Before The Drift (Task 4, spec
//! amendment A.3) the union was written for a hazard this file called
//! INERT — `passages_from` propagated `addr.entrance` unchanged, so a walk
//! seeded at entrance *e* could never leave entrance *e*'s own sublattice,
//! and the union agreed with a naive sum by construction. **That hazard is
//! now LIVE.** `entrance` left `ChamberAddr` entirely, so every mouth of a
//! system addresses INTO the same shared lattice, and two mouths' walks
//! routinely overlap — summing them would double-count. The union is
//! written once, in [`reachable_union`], and both shares read off it.
//!
//! **A mouth is OPEN when the level it names exists.** That is exactly what
//! `docs/audits/underworld-lattice-seed-panel.md` counts as an "open
//! entrance": `underworld_readout` tests the mouth address with `chamber_at`,
//! which is [`chamber_exists`] plus content resolution and returns `None`
//! precisely when existence is refused. So this probe's openness test and the
//! committed artifact's are the same predicate, and the two are comparable
//! line for line.
//!
//! # THE DENOMINATOR IS THE SYSTEM'S ONE SHARED LATTICE, NOT A SUM OVER ENTRANCES
//!
//! **This section used to argue the opposite, and the reversal is the whole
//! story of Task 4.** Before The Drift, `chamber_exists` gated on
//! `branch_count_of(seed, vertex, addr.entrance)` and every per-address stream
//! carried `entrance`, so each entrance realized its OWN private sublattice —
//! the committed witness summed existence over `0..drawn_entrances`, which is
//! how seed 42 reached 21,328 (pre-Task-1) or 42,820 (post-Task-1, pre-Task-4)
//! levels from 1,229 drawn entrances. Task 0's baseline measured this and
//! Nathan's ruling (spec amendment A) found it could not express the
//! campaign's own worked example — two doors into ONE Spider Cave.
//!
//! So `entrance` left `ChamberAddr` and `RunAddr` entirely (amendment A.3).
//! `levels` below is now the size of the system's ONE shared lattice, walked
//! ONCE regardless of how many entrances open into it — see [`read_system`].
//! Entrances survive only as which aperture a mouth resolves through
//! ([`entrance_count`], [`entrance_mouth`]); they no longer size the
//! population at all. This is why `levels` FELL from Task 1's 42,820 to
//! 30,272 on seed 42 even though nothing about the terrain or the existence
//! gate changed — a sum over per-entrance sublattices became a single
//! lattice's own size, which is smaller by construction whenever a system
//! draws more than one entrance. **Report this as a structural consequence
//! of amendment A.5, never as a regression**: the spec named this move in
//! advance for exactly this reason.
//!
//! **No more "head-lattice only" secondary reading.** It used to report the
//! entrance-0 sublattice alone, which was a distinct, smaller population than
//! the whole system's summed reading. With one shared lattice per system that
//! secondary reading is now definitionally identical to `levels`/`reachable`
//! themselves, so keeping it would print the same two numbers twice under a
//! different label — see [`SystemReach`]'s own doc.
//!
//! # MEASURED VALUES — BEFORE, 2026-08-23, tree at `campaign/the-drift` prior to Task 1
//!
//! ```text
//! seed 42
//!   systems 874    levels 21328   reachable 1496   entrances 1229 drawn / 511 open
//!   systems with an open mouth 450
//!   per-system reachable share   p10 3.70%   median 12.50%   p90 57.14%
//!   whole-world reachable share  7.01%   (1496 of 21328)
//!   levels per system            mean 24.40   p10 4   median 17   p90 54
//! seed 7
//!   systems 1681   levels 42131   reachable 3277   entrances 2382 drawn / 1070 open
//!   systems with an open mouth 940
//!   per-system reachable share   p10 3.74%   median 13.04%   p90 50.00%
//!   whole-world reachable share  7.78%   (3277 of 42131)
//!   levels per system            mean 25.06   p10 4   median 17   p90 57
//! seed 1234
//!   systems 1266   levels 36393   reachable 2493   entrances 1813 drawn / 831 open
//!   systems with an open mouth 715
//!   per-system reachable share   p10 3.23%   median 11.11%   p90 40.00%
//!   whole-world reachable share  6.85%   (2493 of 36393)
//!   levels per system            mean 28.75   p10 6   median 20   p90 64
//! ```
//!
//! Seed 42's three pre-change headline integers reproduce spec §1 and the
//! pre-Task-1 committed witness exactly: **1,496 reachable of 21,328 existing
//! (7.01%) from 511 open entrances**, across 874 systems.
//!
//! **The whole-world share was stable across the panel at 6.85-7.78%**, and
//! every seed's per-system median sat an order of magnitude under §6's 95%
//! intent. The pre-change world was not marginal on either quantity.
//!
//! # MEASURED VALUES — AFTER TASK 1, 2026-08-23 (`chamber_exists`'s existence
//! # coin deleted, spec §4.1) — SUPERSEDED BELOW, kept for the movement record
//!
//! ```text
//! seed 42
//!   systems 874    levels 42820   reachable 39140   entrances 1229 drawn / 1101 open
//!   systems with an open mouth 874
//!   per-system reachable share   p10 83.87%   median 100.00%   p90 100.00%
//!   whole-world reachable share  91.41%   (39140 of 42820)
//!   levels per system            mean 48.99   p10 9   median 34   p90 105
//! seed 7
//!   systems 1681   levels 84424   reachable 78677   entrances 2382 drawn / 2155 open
//!   systems with an open mouth 1681
//!   per-system reachable share   p10 85.85%   median 100.00%   p90 100.00%
//!   whole-world reachable share  93.19%   (78677 of 84424)
//!   levels per system            mean 50.22   p10 9   median 33   p90 112
//! seed 1234
//!   systems 1266   levels 72304   reachable 66986   entrances 1813 drawn / 1636 open
//!   systems with an open mouth 1266
//!   per-system reachable share   p10 84.27%   median 100.00%   p90 100.00%
//!   whole-world reachable share  92.64%   (66986 of 72304)
//!   levels per system            mean 57.11   p10 12   median 39   p90 128
//! ```
//!
//! Seed 42 moved from 1,496 reachable of 21,328 (7.01%) to 39,140 of 42,820
//! (91.41%) after Task 1 — levels itself roughly doubled (the coin used to
//! also suppress about half of every drawn floor from EXISTING at all, not
//! only from being reached) and the reachable count grew 26.2x. Every seed
//! cleared both of spec §6's preregistered intents there for the first time.
//!
//! # MEASURED VALUES — AFTER TASK 4, 2026-08-23 (`entrance` dropped from the
//! # address, spec amendment A.3) — THE CURRENT TREE
//!
//! ```text
//! seed 42
//!   systems 874    levels 30272   reachable 30272   entrances 1229 drawn / 1154 open
//!   systems with an open mouth 874
//!   per-system reachable share   p10 100.00%   median 100.00%   p90 100.00%
//!   whole-world reachable share  100.00%   (30272 of 30272)
//!   levels per system            mean 34.64   p10 8   median 28   p90 68
//! seed 7
//!   systems 1681   levels 60119   reachable 60119   entrances 2382 drawn / 2250 open
//!   systems with an open mouth 1681
//!   per-system reachable share   p10 100.00%   median 100.00%   p90 100.00%
//!   whole-world reachable share  100.00%   (60119 of 60119)
//!   levels per system            mean 35.76   p10 8   median 29   p90 75
//! seed 1234
//!   systems 1266   levels 49002   reachable 49002   entrances 1813 drawn / 1728 open
//!   systems with an open mouth 1266
//!   per-system reachable share   p10 100.00%   median 100.00%   p90 100.00%
//!   whole-world reachable share  100.00%   (49002 of 49002)
//!   levels per system            mean 38.71   p10 9   median 31   p90 76
//! ```
//!
//! **Seed 42: `levels` FELL from 42,820 (Task 1) to 30,272, and `reachable`
//! ROSE to meet it exactly — 30,272 of 30,272, 100.00%.** This is the shape
//! spec amendment A.5 predicted in advance: "the count falls for a reason
//! that has nothing to do with the deleted coin," because 21,328/42,820 were
//! sums over private per-entrance sublattices and 30,272 is one shared
//! lattice's own size. **All three panel seeds now read EXACTLY 100.00% on
//! both arms** — per-system median and whole-world share both at the
//! ceiling, not merely past spec §6's 95%/90% intents. `entrances` (the
//! drawn aperture count) is unchanged from Task 1 on every seed, because
//! `entrance_count` is untouched by this task; `open entrances` rose
//! slightly (1101->1154 on seed 42) because a mouth now resolves against the
//! system's ONE shared lattice rather than its own entrance's private one,
//! which is strictly more permissive.
//!
//! # MEASURED VALUES — AFTER TASK 5, 2026-08-23 (`branch_count_of`,
//! # `character_of` and `barrier_of` re-keyed onto `(vertex, band, ...)`, spec
//! # amendment A.3) — THE CURRENT TREE
//!
//! ```text
//! seed 42
//!   systems 874    levels 30537   reachable 30537   entrances 1229 drawn / 1140 open
//!   systems with an open mouth 874
//!   per-system reachable share   p10 100.00%   median 100.00%   p90 100.00%
//!   whole-world reachable share  100.00%   (30537 of 30537)
//!   levels per system            mean 34.94   p10 8   median 34   p90 60
//! seed 7
//!   systems 1681   levels 59227   reachable 59227   entrances 2382 drawn / 2193 open
//!   systems with an open mouth 1681
//!   per-system reachable share   p10 100.00%   median 100.00%   p90 100.00%
//!   whole-world reachable share  100.00%   (59227 of 59227)
//!   levels per system            mean 35.23   p10 9   median 35   p90 65
//! seed 1234
//!   systems 1266   levels 48294   reachable 48294   entrances 1813 drawn / 1651 open
//!   systems with an open mouth 1266
//!   per-system reachable share   p10 100.00%   median 100.00%   p90 100.00%
//!   whole-world reachable share  100.00%   (48294 of 48294)
//!   levels per system            mean 38.15   p10 10   median 38   p90 65
//! ```
//!
//! **`levels` moved by a small amount on every seed (30272->30537 seed 42,
//! 60119->59227 seed 7, 49002->48294 seed 1234) — a few tenths of a percent,
//! not the multi-thousand-level structural jump Task 1 and Task 4 each
//! produced.** The mechanism is different from both of those: nothing about
//! the LATTICE'S SHAPE changed (still one shared lattice per system,
//! `entrance` still carries no address weight); what changed is that
//! `branch_count_of` — the C.1 gate `chamber_exists` reads before admitting a
//! branch — now answers PER BAND instead of once per system. A branch that
//! was globally admitted or globally refused before Task 5 can now be
//! admitted at some bands and refused at others, so individual (branch,
//! band) vertices flip in both directions; the small net movement is the sum
//! of many small, band-local flips rather than one directional shift. `open
//! entrances` fell slightly on every seed too (1154->1140 seed 42,
//! 2250->2193 seed 7, 1728->1651 seed 1234) for the same reason —
//! `entrance_mouth`'s own `branch_count_of` reference query (`Band::
//! Undercroft`, see its doc) no longer always agrees with the branch's
//! actual landing-band width, so a mouth that used to resolve now
//! occasionally names a branch its landing band does not realize. **All
//! three panel seeds still read EXACTLY 100.00% on both reachability arms**
//! — the movement is entirely in the denominator (which chambers exist),
//! not in the reachability property Task 4 established.
//!
//! # MEASURED VALUES — AFTER TASK 7, 2026-08-23 (the descent rule rewritten
//! # onto the band-transition edges; the lateral rule deleted; entrances
//! # landed in top-band branches — spec §4.6)
//!
//! ```text
//! seed 42
//!   systems 874    levels 30537   reachable 30414   entrances 1229 drawn / 1229 open
//!   systems with an open mouth 874
//!   per-system reachable share   p10 100.00%   median 100.00%   p90 100.00%
//!   whole-world reachable share  99.60%   (30414 of 30537)
//!   levels per system            mean 34.94   p10 8   median 34   p90 60
//!   systems below 100%           27 of 874 = 3.09%
//!   unreached levels by band     rank 0: 120   rank 1: 3
//!   side mouths off their band's width  0 of 134 = 0.00%
//! seed 7
//!   systems 1681   levels 59227   reachable 58987   entrances 2382 drawn / 2382 open
//!   per-system reachable share   p10 100.00%   median 100.00%   p90 100.00%
//!   whole-world reachable share  99.59%   (58987 of 59227)
//!   systems below 100%           35 of 1681 = 2.08%
//!   unreached levels by band     rank 0: 142   rank 1: 98
//!   side mouths off their band's width  0 of 268 = 0.00%
//! seed 1234
//!   systems 1266   levels 48294   reachable 48135   entrances 1813 drawn / 1813 open
//!   per-system reachable share   p10 100.00%   median 100.00%   p90 100.00%
//!   whole-world reachable share  99.67%   (48135 of 48294)
//!   systems below 100%           28 of 1266 = 2.21%
//!   unreached levels by band     rank 0: 90   rank 1: 56   rank 2: 13
//!   side mouths off their band's width  0 of 237 = 0.00%
//! ```
//!
//! Three things moved and each has a different cause. **`levels` did not
//! move at all** on any seed — `chamber_exists` is untouched by this task, so
//! the population is byte-for-byte the one Task 5 left, which is what makes
//! the other two readings comparable rather than confounded.
//!
//! **`open entrances` rose to exactly `drawn`** (1140 -> 1229 on seed 42, and
//! the same on the other two). Every drawn mouth is now open, by
//! construction rather than by luck: a door lands at level 0 of the TOP
//! habitation band, on a branch drawn against that band's own width; level 0
//! of a realized run always exists, and no cave's depth budget stops short of
//! the shallowest band. What used to close a door was the mismatch this task
//! retired — 47.8% / 53.0% / 51.9% of drawn side mouths named a branch their
//! independently drawn landing band did not realize. **That share is now
//! 0.00% on all three seeds**, and the probe asserts it rather than printing
//! it, because it is a property of the construction and not a statistic.
//!
//! **Reachability FELL, by 0.40 / 0.41 / 0.33 points, and this is a finding
//! rather than noise.** Seed 42 goes 30,537/30,537 (100.00%) to
//! 30,414/30,537 (99.60%) — 123 levels lost. The cause is the deleted lateral
//! rule and nothing else, and the band histogram is what establishes that
//! rather than an argument: **120 of seed 42's 123 unreached levels sit at
//! rank 0**, the top band, with 3 one rung below; the other two seeds have
//! the same shape, decaying with depth (142/98, then 90/56/13). Below the top
//! band every branch is guaranteed a parent above it (spec §4.5's second
//! guarantee), so once a band is entered the ladder carries you down. The top
//! band has no band above it to be entered from: an Undercroft branch is
//! reachable only if a door lands on it, or if some branch below is a child
//! of both it and an already-reached branch. When neither holds, that branch
//! — and whatever hangs beneath it — is cut off. **2.1-3.1% of systems are
//! affected**, so the loss is concentrated in a few systems rather than
//! spread thinly across all of them.
//!
//! **Both of §6's arms still clear their intents, including amendment C's
//! replacement.** The per-system p10 (C.2's gated statistic, chosen because
//! the median could not fail) reads 100.00% on all three seeds, and the
//! whole-world arm reads 99.6% against a 90% intent. The shortfall is
//! reported here because it is real and has a mechanism, not because it
//! breaches anything.
//!
//! # MEASURED VALUES — AFTER TASK 7b, 2026-08-23 (every top-band branch is
//! # named by an entrance — spec amendment E.2) — THE CURRENT TREE
//!
//! ```text
//! seed 42
//!   systems 874    levels 30537   reachable 30537   entrances 1626 drawn / 1626 open
//!   systems with an open mouth 874
//!   per-system reachable share   p10 100.00%   median 100.00%   p90 100.00%
//!   whole-world reachable share  100.00%   (30537 of 30537)
//!   levels per system            mean 34.94   p10 8   median 34   p90 60
//!   systems below 100%           0 of 874 = 0.00%
//!   unreached levels by band     none
//!   side mouths off their band's width  0 of 543 = 0.00%
//! seed 7
//!   systems 1681   levels 59227   reachable 59227   entrances 3177 drawn / 3177 open
//!   per-system reachable share   p10 100.00%   median 100.00%   p90 100.00%
//!   whole-world reachable share  100.00%   (59227 of 59227)
//!   systems below 100%           0 of 1681 = 0.00%
//!   unreached levels by band     none
//!   side mouths off their band's width  0 of 1065 = 0.00%
//! seed 1234
//!   systems 1266   levels 48294   reachable 48294   entrances 2331 drawn / 2331 open
//!   per-system reachable share   p10 100.00%   median 100.00%   p90 100.00%
//!   whole-world reachable share  100.00%   (48294 of 48294)
//!   systems below 100%           0 of 1266 = 0.00%
//!   unreached levels by band     none
//!   side mouths off their band's width  0 of 749 = 0.00%
//! ```
//!
//! **The residual is ZERO, which is the number amendment E.4 said in advance
//! it would be.** Task 7's review had already measured that forcing an
//! entrance onto every top-band branch takes the unreached count to 0 on all
//! three seeds; this is that lever pulled, and it lands on the predicted
//! value rather than near it. The band histogram is EMPTY on every seed —
//! not merely small — so there is no residual left to attribute.
//!
//! **The gated arm is amendment D's, and it reads 0.00%.** The share of
//! systems below 100% reachable goes 3.09 / 2.08 / 2.21% -> 0.00 / 0.00 /
//! 0.00%, against an intent arm of 0%. p10 and the median are context only
//! here: amendment D established that both are structurally blind to a
//! defect affecting under ~10% of systems, which is exactly the size this
//! one was, and they read 100.00% before this task as well as after it.
//!
//! **`levels` did not move on any seed** (30537 / 59227 / 48294, identical
//! to Task 7's) — `chamber_exists` is untouched again, so the denominator is
//! byte-for-byte the one Task 7 left and the reachability movement is not
//! confounded with a population change.
//!
//! **Apertures rose 32 / 33 / 29%** — 1229 -> 1626, 2382 -> 3177, 1813 ->
//! 2331 — and that is E.2's stated cost rather than a side effect. A system
//! opens `max(free draw, top-band width)` doors now, because a system with
//! fewer doors than top-band branches cannot name them all. Every drawn
//! mouth is still open (`drawn == open` on all three seeds), which Task 7
//! established and this task preserves.
//!
//! **Every number in the block moved because both entrance legs took an
//! epoch** (`chamber/entrance-count/v2`, `chamber/entrance-mouth/v2`), so
//! the counts and mouths are drawn from new parent seeds. The rise above is
//! therefore the sum of the epoch's reshuffle and the width floor, not the
//! floor alone; the floor is what makes the reachability result true by
//! construction, and the guarantee is asserted over constructed widths in
//! `chamber::tests::every_top_band_branch_is_named_by_an_entrance` rather
//! than inferred from these three seeds.
//!
//! # THE READOUT — TASK 8, 2026-08-23, ALL FOUR OF §6's QUANTITIES
//!
//! No production code moved between Task 7b and this task: the three headline
//! triples are byte-identical to the block above, which is what makes the
//! readout a reading of the campaign's product rather than of a fourteenth
//! intermediate state. What is new is that the frozen tables are now
//! **applied and asserted** instead of printed, and that §6's fourth
//! quantity — branches per band — is measured here for the first time.
//!
//! ```text
//!                                          seed 42    seed 7   seed 1234
//! [GATED] systems below 100% reachable        0.00%     0.00%      0.00%
//!         (amendment D.3, intent 0%)       0 of 874  0 of 1681  0 of 1266
//!                                          -> Intent -> Intent  -> Intent
//! [GATED] whole-world reachable share       100.00%   100.00%    100.00%
//!         (§6, intent >= 90%)                -> Intent -> Intent -> Intent
//!         RATCHET floor 100.00%                cleared on all three seeds
//! [GATED] branches per band, mode               1         1          1
//!         (§6, arm `mode 1`)             -> ModeIsOne (all three seeds)
//! [REPORTED] levels per system, mean          34.94     35.23      38.15
//!            p10 / median / p90            8/34/60   9/35/65   10/38/65
//! [context]  per-system share p10/med/p90  100/100/100 on all three seeds
//! ```
//!
//! **Branches per band, in full** — `branch_count_of`'s drawn width at every
//! `(system, habitation band)` pair, which is exactly where `chamber_exists`
//! consults it:
//!
//! ```text
//!   width          1              2             3            4
//!   seed 42    2604 (59.59%)  1080 (24.71%)  470 (10.76%)  216 (4.94%)
//!   seed 7     5035 (59.90%)  2113 (25.14%)  841 (10.01%)  416 (4.95%)
//!   seed 1234  3801 (60.05%)  1647 (26.02%)  610 ( 9.64%)  272 (4.30%)
//! ```
//!
//! **The mode is 1 and the distribution reproduces C.1's authored weights —
//! 60 / 25 / 10 / 5 — to within half a point on every seed.** That is the
//! reading §6 wanted and it is worth one sentence of why it is not trivial:
//! Task 5 re-keyed this draw from `(vertex, entrance)` onto `(vertex, band)`,
//! so a distribution measured over per-entrance draws is not evidence about
//! the per-band one. Restricting the population to bands that actually
//! realize a chamber does not move the mode either (59.90 / 60.37 / 60.11%
//! at width 1), so no width is being drawn into a band the world never fills
//! in.
//!
//! **The whole-world share is at its ceiling on all three seeds, and until
//! this task nothing asserted it on two of them.** `SEED_42_BASELINE` pinned
//! seed 42's triple exactly; seeds 7 and 1234 had no equivalent, so a
//! connectivity regression confined to them would have printed and exited 0.
//! [`WORLD_SHARE_FLOOR`] closes that, and it is proved to fire rather than
//! assumed to — see "EVERY GATED ARM IS PROVED TO FIRE" below.
//!
//! **What the campaign moved, stated in shares because the counts are not
//! comparable** (amendment A.5, again): the whole-world reachable share goes
//! **7.01% -> 100.00%** on seed 42, 7.78% -> 100.00% on seed 7, 6.85% ->
//! 100.00% on seed 1234. The share of systems reaching all of their own
//! levels goes from a median of 12.50% per system to *every system reaching
//! every level*. The level COUNT went 21,328 -> 30,537 on seed 42, and that
//! number says nothing about the campaign at all: the first is a sum over
//! per-entrance sublattices and the second is one lattice per system.
//!
//! # EVERY GATED ARM IS PROVED TO FIRE — controls, not arguments
//!
//! Four mutations of the SHIPPED derivation, applied with `scripts/mutate.py`
//! (which refuses a pattern matching zero or more than one site), each run
//! against the whole panel. The tree was restored from a `cp` copy after each
//! and `git diff -- windows/worldgen/src/` confirmed empty.
//!
//! ```text
//! R1  entrance_mouth: every door lands at `floor: 0` -> `floor: 1`, so a
//!     system whose top-band run drew a single floor loses ALL of its doors.
//!                                 seed 42       seed 7      seed 1234
//!       whole-world share          89.26%       87.58%        87.38%
//!       systems with an open mouth 769 of 874   1445 of 1681  1091 of 1266
//!       systems below 100%         16 (2.08%)   19 (1.31%)    17 (1.56%)
//!       per-system p10 / MEDIAN    100.00% / 100.00% on ALL THREE SEEDS
//!     => amendment D's arm panics (Report, not Intent). §6's whole-world arm
//!        would also have reddened (89.26% < 90%).
//!
//! R2  entrance_count: `free.max(width)` -> `free.max(width) -
//!     u8::from(width == 1)`, so every system whose top band is one branch
//!     wide and whose free draw is 1 opens NO door at all.
//!                                 seed 42       seed 7      seed 1234
//!       whole-world share          59.53%       58.78%        59.85%
//!       systems with an open mouth 499 of 874   962 of 1681   733 of 1266
//!       systems below 100%         0 (0.00%)    0 (0.00%)     0 (0.00%)
//!       per-system p10 / MEDIAN    100.00% / 100.00% on ALL THREE SEEDS
//!     => §6's whole-world arm panics (BelowIntent). AMENDMENT D's GATED ARM
//!        READ `Intent` WITH 40% OF THE UNDERWORLD UNREACHABLE, because its
//!        denominator is systems WITH an open mouth and these systems have
//!        none.
//!
//! R3  the same mutation, narrowed to `width == 1 && vertex.0 % 16 == 0` — the
//!     magnitude dial, so the defect sits where a REAL one would.
//!                                 seed 42       seed 7      seed 1234
//!       whole-world share          98.12%       97.58%        97.16%
//!       systems with an open mouth 854 of 874   1636 of 1681  1230 of 1266
//!       systems below 100%         0 (0.00%)    0 (0.00%)     0 (0.00%)
//!       §6's whole-world arm       Intent       Intent        Intent
//!       per-system p10 / MEDIAN    100.00% / 100.00% on ALL THREE SEEDS
//!     => THE RATCHET IS THE ONLY THING THAT FIRES. Every other gate and
//!        every quantile reads its passing arm while 575 of seed 42's levels
//!        sit in twenty systems no door opens onto.
//!
//! R4  branch_count_of: the width-1 weight `r < 0.60` -> `r < 0.05`.
//!       branches per band, mode    2 (79.20%)   2 (79.89%)    2 (81.31%)
//!     => §6's branches-per-band arm panics (ModeIsAboveOne). Reachability is
//!        untouched — 100.00% on all three seeds — which is what makes this a
//!        clean control on that arm alone.
//! ```
//!
//! **R3 is why the floor is exactly 1.0, and it is the third time this
//! campaign has caught a statistic that could not fail.** Amendment C caught
//! the median, amendment D caught the p10, and R3 catches **amendment D's own
//! replacement** — not because D chose badly, but because D's arm answers a
//! different question ("does a system reach its own levels?") than the
//! whole-world arm ("is the underworld reachable?"), and a system with no
//! door is invisible to the first and fatal to the second. The two arms are
//! complements, not redundancies, and §6 gates both for that reason.
//!
//! **A wide band would have swallowed the case the ratchet exists for.**
//! Amendment D.1 records a real defect at 99.43 / 99.50 / 99.54% whole-world;
//! R3 sits at 97.16-98.12%. §6's 90% arm passes both comfortably. Any band
//! loose enough to be called slack passes them too, and the property is exact
//! by construction rather than statistical, so there is nothing for slack to
//! absorb.
//!
//! Wall time for the whole probe (three `BuildDepth::Terrain` worlds and
//! ~1.6M `chamber_exists` calls) is ~1.5 s in the optimized test profile.
//! It is `#[ignore]`d anyway, with the same reason every live-worldgen battery
//! in this suite carries: the cost that matters is the world build, and the
//! commit gate does not pay for those (decision 0132).
//!
//! # THE BAND CEILING IS DERIVED, NEVER RESTATED
//!
//! [`habitation_ranks`] walks the delve ladder through the shipped
//! [`rung_rank`], the same route `junctions.rs` uses, because
//! `chamber::rung_of_rank` is private. No literal `0..5` appears anywhere in
//! this file; a sixth habitation rung widens the scan without an edit here.
//!
//! Test fixture (decision 0092): calls the composition-root entry points
//! directly, the sanctioned posture for this crate's live-worldgen batteries.
#![allow(clippy::disallowed_methods)]

use std::collections::{BTreeMap, BTreeSet};

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Band, Seed, Vertex};
use hornvale_terrain::{Cave, GeothermalGradient, TerrainPins, rungs};
use hornvale_worldgen::chamber::{
    BRANCHES_PER_SYSTEM, ChamberAddr, LEVELS_PER_BRANCH_CEILING, chamber_exists, entrance_count,
    entrance_mouth, passages_from, rung_rank,
};
use hornvale_worldgen::character::branch_count_of;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to_with_artifacts,
};

/// The seeds the campaign preregisters on (spec §5), matching every other
/// live-worldgen probe in this suite.
const SEEDS: [u64; 3] = [42, 7, 1234];

/// Seed 42's baseline, as `(levels, reachable, open_entrances)` — the exact
/// triple the committed witness `docs/audits/underworld-lattice-seed-panel.md`
/// renders, re-baselined once already.
///
/// **Pinned as an equality on purpose, and it has now broken five times,
/// exactly as designed.** Before Task 1 landed this held `(21328, 1496,
/// 511)` — spec §1's opening figure. Task 1 (spec §4.1) deleted
/// `chamber_exists`'s existence coin and moved it to `(42820, 39140, 1101)`.
/// Task 4 (spec amendment A.3) moved it a second time, to `(30272, 30272,
/// 1154)`: dropping `entrance` from `ChamberAddr` collapses each system's
/// per-entrance sublattices into ONE shared lattice, so `levels` fell (a
/// structural consequence of amendment A.5, not a regression — see this
/// module's header) while `reachable` rose to EXACTLY equal it: every one of
/// the three panel seeds now reads 100.00% whole-world reachable, not merely
/// past spec §6's 90% intent but at its ceiling. **Task 5 (re-keying
/// `branch_count_of` onto `(vertex, band)`, spec amendment A.3) moved it a
/// third time, to `(30537, 30537, 1140)`**: this movement is far smaller
/// than either of the first two (a few tenths of a percent, not a
/// multi-thousand-level jump) and comes from a different mechanism — no
/// lattice shape changed, but individual `(branch, band)` vertices that were
/// globally admitted or refused before Task 5 now flip independently per
/// band, so the net change is the sum of many small band-local flips.
///
/// **Task 7 (the descent rule, spec §4.6) moved it a FOURTH time, to
/// `(30537, 30414, 1229)`**, and this movement is the only one of the four
/// where `levels` stayed put: `chamber_exists` is untouched, so the
/// population is identical and the two numbers that moved moved for reasons
/// this task owns. `open entrances` rose to equal `drawn` because every mouth
/// now lands somewhere that exists by construction; `reachable` fell by 123
/// because the lateral `branch ± 1` rule is gone and a top-band branch no
/// door landed on can be cut off. The module's own "MEASURED VALUES" block
/// records all four movements in full, with the band histogram that
/// establishes the third one's mechanism.
///
/// **Task 7b (spec amendment E.2) moved it a FIFTH time, to `(30537, 30537,
/// 1626)`.** `levels` stayed put for the second task running — `chamber_
/// exists` is untouched — so the two numbers that moved are again this
/// task's own. `reachable` ROSE by the 123 Task 7 lost, back to exactly
/// `levels`: every top-band branch is now named by an entrance, which was
/// the one thing §4.5's guarantee pair could not say, and with it a
/// system's whole lattice is one reachable component by construction.
/// `open entrances` rose 1229 -> 1626 because a system opens at least as
/// many doors as its top band has branches, and because both entrance legs
/// took an epoch and are therefore drawn from new parent seeds.
///
/// A move from any OTHER cause from here on — a terrain change, a stream
/// relabelling, a lattice constant — is a determinism finding, and this
/// equality still catches that.
///
/// A band was considered and rejected: this is not a noisy statistic but a
/// deterministic count over a fixed seed, and a band around a deterministic
/// count only buys room for an undetected change.
const SEED_42_BASELINE: (usize, usize, usize) = (30537, 30537, 1626);

// --- The frozen branch tables, encoded ------------------------------------
//
// The idiom is `stope_variety_probe.rs`'s: one enum per preregistered table,
// one classifier that applies it, an `UnspecifiedByTheBrief` variant wherever
// the table leaves a gap, and an `assert_eq!` on the arm the reading actually
// lands in — never a bound rounded toward the arm the campaign wanted.

/// Amendment D.3's ceiling for the "report" arm of the gated per-system
/// statistic: a share of systems below 100% reachable strictly above this is
/// a connectivity defect that survives §4.5's guarantees, and the spec says
/// STOP rather than widen.
const SYSTEMS_BELOW_FULL_REPORT_CEILING: f64 = 0.05;

/// §6's intent for the whole-world reachable share.
const WORLD_SHARE_INTENT: f64 = 0.90;

/// The `>= 95%` intent both RETIRED per-system tables shared — §6's own
/// (on the median) and amendment C.2's (on the p10).
///
/// Kept only so [`retired_quantile_arm`] can state what those tables *would*
/// have said about today's reading. Neither is gated; see
/// [`RetiredQuantileArm`].
const RETIRED_QUANTILE_INTENT: f64 = 0.95;

/// The floor of the retired tables' middle arm (`50 - 95%` reports).
const RETIRED_QUANTILE_REPORT_FLOOR: f64 = 0.50;

/// **THE RATCHET** on the whole-world reachable share — the campaign's
/// headline number, and until this task nothing on seeds 7 and 1234 asserted
/// it at all.
///
/// # Why a ratchet exists here
///
/// §6's whole-world arm is `>= 90%`, and the shipped world reads 100.00%. Ten
/// points of slack between the reading and the gate is ten points a defect
/// can consume in silence. The Stope shipped exactly that shape — a printed
/// headline nothing asserted, where one character of production code
/// (`EXISTENCE_DENSITY` 0.5 -> 0.7) moved the rate 4.4x and the probe still
/// exited 0. `SEED_42_BASELINE` pins seed 42's triple exactly, but seeds 7 and
/// 1234 had no equivalent, so a connectivity regression confined to them would
/// have printed and passed.
///
/// # Why it is EXACTLY 1.0, and not a band with slack
///
/// This is not a noisy statistic. After spec §4.5's two guarantees and
/// amendment E.2's third, a system's lattice is **one reachable component by
/// construction**, every system draws at least one aperture
/// (`entrance_count` answers `free.max(width)` with `free >= 1`), and every
/// drawn mouth is open — so the share is exactly 1.0 as a *property*, not as
/// a measurement that happens to land there. IEEE division gives exactly
/// `1.0` for `n/n` at any finite non-zero `n`, so no epsilon is needed and
/// none is used.
///
/// A wide band was considered and rejected, and the two known defect
/// magnitudes are why: Task 7's top-band orphans read 99.60 / 99.59 / 99.67%
/// and the reviewer's `entrance_mouth` mutation read 99.43 / 99.50 / 99.54%.
/// Any band loose enough to be called "slack" swallows both. The contrast
/// with `stope_variety_probe`'s `NADIR_WALK_RATE_FLOOR` is deliberate: that
/// one bands a count of 22 branches out of 6,136, where incidental motion is
/// real and pinning the integer would train a reader to re-baseline without
/// looking. This one is a saturating share whose every deviation is a
/// finding.
///
/// The band's other end is not a second constant because it cannot be
/// crossed: `reachable <= levels` is asserted separately in the same test, so
/// the ratchet is one-sided by construction rather than by omission.
///
/// # WHAT WOULD LEGITIMATELY MOVE IT
///
/// Exactly one thing, and it is already written down as owed. Spec §7's
/// non-goal and amendment B.2 both name **restricted passage** — locked
/// doors, boss encounters, collapses that magic can clear, and the rare
/// chamber that stays lost with something interesting in it. That layer
/// deliberately makes part of a system unreachable, and when it lands this
/// ratchet must move *with a spec amendment that says so*, not be widened to
/// accommodate a red.
///
/// Everything else that could move it is a finding, not a legitimate move:
///
/// * a branch that descends into nothing, or a band with no parent above it
///   (spec §4.5's two guarantees);
/// * a top-band branch no entrance names (amendment E.2's third guarantee);
/// * a system drawing zero apertures, which leaves its levels in the
///   denominator with nothing to reach them. **This is the case amendment D's
///   gated arm cannot see, and it is measured rather than argued**: mutation
///   R3 in this module's header takes twenty of seed 42's systems to zero
///   apertures, and D's arm reads `Intent` — 0 of 854 — while 575 levels are
///   unreachable. D's denominator is systems *with* an open mouth, so a
///   system with no door leaves the population instead of failing in it. The
///   two arms are complements, and this is why the ratchet is not redundant
///   with D's;
/// * any change to `passages_from`'s edge set.
///
/// # WHAT MOVING IT WOULD BREAK
///
/// R3's defect class — a system drawing zero apertures — leaves its levels
/// unreachable but also drops the system itself out of
/// `systems_with_open_mouth`, amendment D's own denominator. That is why R3
/// is invisible to amendment D's gated arm (the bullet above) and it is also
/// why loosening THIS floor is not a safe compensating move: §6's whole-world
/// arm (`>= 90%`), amendment D's per-system arm and this ratchet all read the
/// affected world through denominators R3 shrinks in step, so widening any
/// one of the three to "tolerate" a mouth-less system widens all three at
/// once, for the same reason. Closing that blind spot for good needs a
/// standing assertion this module does not yet have —
/// `summary.systems_with_open_mouth == summary.systems` — asserted in
/// addition to, not instead of, this floor.
const WORLD_SHARE_FLOOR: f64 = 1.0;

/// Amendment D.3's table — **the gated per-system arm**, and the third
/// statistic to hold this job.
///
/// §6 gated the median; amendment C replaced it with the p10 after a mutation
/// proved the median could not fail; amendment D replaced the p10 after two
/// further defects proved it could not fail either. This share has no blind
/// zone: one affected system moves it. See the module header.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ConnectivityVerdict {
    /// Exactly 0% — the intent: every system reaches all of its own levels.
    Intent,
    /// `(0%, 5%]` — report, with the unreached-by-band histogram.
    Report,
    /// `> 5%` — a connectivity defect survives §4.5's guarantees. STOP.
    DefectSurvivesTheGuarantees,
    /// The reading is not a share at all — non-finite, or outside `[0, 1]`.
    /// D.3's table names no arm for that, and a classifier that folded it
    /// into `Intent` would report a broken instrument as a perfect world.
    /// Reachable only through a malformed input; see the classifier test.
    UnspecifiedByTheBrief,
}

/// Apply amendment D.3's table.
///
/// The malformed-input arm is tested FIRST, for `classify_depth`'s reason in
/// `stope_variety_probe.rs`: a verdict about the instrument outranks the
/// verdict the instrument would otherwise deliver about the world.
fn classify_connectivity(share_below_full: f64) -> ConnectivityVerdict {
    if !share_below_full.is_finite() || !(0.0..=1.0).contains(&share_below_full) {
        ConnectivityVerdict::UnspecifiedByTheBrief
    } else if share_below_full == 0.0 {
        ConnectivityVerdict::Intent
    } else if share_below_full <= SYSTEMS_BELOW_FULL_REPORT_CEILING {
        ConnectivityVerdict::Report
    } else {
        ConnectivityVerdict::DefectSurvivesTheGuarantees
    }
}

/// §6's whole-world table, unchanged by any amendment — the one arm of the
/// preregistration that never had to be replaced.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum WorldShareVerdict {
    /// `>= 90%` — the intent.
    Intent,
    /// `< 90%` — either a connectivity hole survives §4.5's guarantees, or
    /// entrance-less systems are commoner than expected. Report WHICH.
    BelowIntent,
    /// Not a share. Same reasoning as
    /// [`ConnectivityVerdict::UnspecifiedByTheBrief`].
    UnspecifiedByTheBrief,
}

/// Apply §6's whole-world table.
fn classify_world_share(world: f64) -> WorldShareVerdict {
    if !world.is_finite() || !(0.0..=1.0).contains(&world) {
        WorldShareVerdict::UnspecifiedByTheBrief
    } else if world >= WORLD_SHARE_INTENT {
        WorldShareVerdict::Intent
    } else {
        WorldShareVerdict::BelowIntent
    }
}

/// §6's fourth preregistered quantity, applied to `branch_count_of`'s own
/// distribution over every `(system, habitation band)` pair.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum BranchesPerBandVerdict {
    /// The mode is 1 — proceed, matching C.1's intent one level down.
    ModeIsOne,
    /// The mode is above 1. §6: "report as a finding; do NOT re-weight to
    /// rescue it." This is a REPORT arm, not a STOP arm — the assertion pins
    /// whichever arm the panel lands in, it does not demand this one be
    /// empty.
    ModeIsAboveOne,
    /// **The histogram has no unique mode**, or is empty. Two counts tied for
    /// the top makes "the mode is 1" neither true nor false, and §6 names no
    /// arm for it. Recorded rather than tie-broken, for the reason
    /// `stope_variety_probe.rs`'s twin variant records: a tie broken toward
    /// the passing arm is a green verdict the brief never issued.
    UnspecifiedByTheBrief,
}

/// Apply §6's branches-per-band table to a histogram indexed by drawn width.
fn classify_branches_per_band(hist: &BTreeMap<u8, usize>) -> BranchesPerBandVerdict {
    let Some(top) = hist.values().copied().max() else {
        return BranchesPerBandVerdict::UnspecifiedByTheBrief;
    };
    let modes: Vec<u8> = hist
        .iter()
        .filter(|&(_, n)| *n == top)
        .map(|(width, _)| *width)
        .collect();
    match modes.as_slice() {
        [1] => BranchesPerBandVerdict::ModeIsOne,
        [_] => BranchesPerBandVerdict::ModeIsAboveOne,
        _ => BranchesPerBandVerdict::UnspecifiedByTheBrief,
    }
}

/// What a **retired** per-system table would have said — §6's own (median) or
/// amendment C.2's (p10), which share the `>= 95% / 50-95% / < 50%` arms.
///
/// **Every variant is named `WouldRead…` on purpose.** These tables are not
/// gates and nothing in this file asserts on them. Amendment D established
/// that both statistics read a perfect 100.00% under two different real
/// connectivity defects — Task 7's top-band orphans (3.09 / 2.08 / 2.21% of
/// systems) and the reviewer's `entrance_mouth` mutation — so a verdict-shaped
/// value here would be exactly the thing D.4 forbids: a quantile presented as
/// though it had checked something. The names are the guard.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum RetiredQuantileArm {
    /// `>= 95%` — what the retired tables called "the intent".
    WouldReadTheIntent,
    /// `50 - 95%` — "report, and find what is still cutting runs".
    WouldReadReport,
    /// `< 50%` — "the deletion did not do what this spec claims. STOP."
    WouldReadTheDeletionFailed,
    /// Not a share. Present for the same reason as its siblings, and for the
    /// same reason no arm here is ever asserted.
    UnspecifiedByTheBrief,
}

/// Apply a retired per-system table to a quantile of the share distribution.
fn retired_quantile_arm(q: f64) -> RetiredQuantileArm {
    if !q.is_finite() || !(0.0..=1.0).contains(&q) {
        RetiredQuantileArm::UnspecifiedByTheBrief
    } else if q >= RETIRED_QUANTILE_INTENT {
        RetiredQuantileArm::WouldReadTheIntent
    } else if q >= RETIRED_QUANTILE_REPORT_FLOOR {
        RetiredQuantileArm::WouldReadReport
    } else {
        RetiredQuantileArm::WouldReadTheDeletionFailed
    }
}

/// The habitation band ranks, ascending — **derived from the delve ladder**
/// through the shipped [`rung_rank`], never restated as a literal range.
///
/// `chamber::rung_of_rank` is private to that module, so filtering [`rungs`]
/// through [`rung_rank`] is the sanctioned route from a test crate; it is
/// `junctions.rs`'s `habitation_bands` with the rung dropped, since this file
/// needs only the rank.
fn habitation_ranks() -> Vec<u8> {
    let mut ranks: Vec<u8> = rungs().iter().filter_map(|&rung| rung_rank(rung)).collect();
    ranks.sort_unstable();
    ranks
}

/// Percentile of an ascending slice of shares, by the round-half-away index
/// convention (`round((n-1) q)`) — the convention every other probe in this
/// suite uses, so the numbers are comparable across files.
fn pct(sorted: &[f64], q: f64) -> f64 {
    if sorted.is_empty() {
        return f64::NAN;
    }
    let i = ((sorted.len() - 1) as f64 * q).round() as usize;
    sorted[i]
}

/// The same percentile over an ascending slice of counts, so the two
/// distributions this file prints share one index convention.
fn pct_usize(sorted: &[usize], q: f64) -> usize {
    if sorted.is_empty() {
        return 0;
    }
    let i = ((sorted.len() - 1) as f64 * q).round() as usize;
    sorted[i]
}

/// A share, with an explicit zero for an empty denominator so a vacuous
/// reading is never rendered as a plausible number.
fn share(numerator: usize, denominator: usize) -> f64 {
    if denominator == 0 {
        0.0
    } else {
        numerator as f64 / denominator as f64
    }
}

/// The levels of one cave system reachable from ANY of `mouths`, by
/// [`passages_from`], with every mouth seeded into ONE shared set.
///
/// The union, not a sum: see this module's header. The shape mirrors
/// `underworld_readout::reachable_union`, which is what the committed witness
/// counts with.
fn reachable_union(
    seed: Seed,
    cave: &Cave,
    gradient: GeothermalGradient,
    mouths: &[ChamberAddr],
) -> BTreeSet<ChamberAddr> {
    let mut seen: BTreeSet<ChamberAddr> = BTreeSet::new();
    let mut frontier: Vec<ChamberAddr> = Vec::new();
    for &mouth in mouths {
        if seen.insert(mouth) {
            frontier.push(mouth);
        }
    }
    while let Some(addr) = frontier.pop() {
        for next in passages_from(seed, cave, gradient, addr) {
            if seen.insert(next) {
                frontier.push(next);
            }
        }
    }
    seen
}

/// One cave system's reading.
///
/// **No more "head-lattice only" secondary reading** (The Drift, amendment
/// A.3). Before this campaign each entrance realized its own private
/// sublattice, so "the entrance-0 sublattice alone" was a distinct, smaller
/// population than the whole system — the pair this struct used to carry as
/// `levels_head`/`reachable_head`. With `entrance` gone from `ChamberAddr`
/// there is exactly ONE lattice per system, so that secondary reading is now
/// definitionally identical to `levels`/`reachable` and carries no
/// information a reader could not already see; keeping it would print the
/// same two numbers twice under different labels.
#[derive(Clone, Debug, Default)]
struct SystemReach {
    /// Apertures this system draws, whether or not they open onto anything.
    drawn_mouths: usize,
    /// Apertures whose named level exists — the witness's "open entrances".
    open_mouths: usize,
    /// Levels that exist in this system's ONE shared lattice.
    levels: usize,
    /// Levels reached by one walk from the union of the open mouths.
    reachable: usize,
    /// Drawn mouths that are NOT the literal head (`branch > 0`) — the
    /// denominator of the mismatch share below.
    drawn_side_mouths: usize,
    /// Drawn side mouths naming a branch their own landing band does not
    /// realize. **This was ~50% before Task 7 and is 0 by construction
    /// after it** — see [`SystemReach`]'s own note and `entrance_mouth`'s
    /// doc.
    side_mouths_off_their_bands_width: usize,
    /// Which bands the UNREACHED levels sit in, by rank — the diagnostic
    /// for Task 7's residual, see [`ReachSummary::unreached_by_band`].
    unreached_by_band: BTreeMap<u8, usize>,
    /// §6's fourth quantity: `branch_count_of`'s drawn width at each
    /// habitation band of this system, as `width -> count`. One entry per
    /// `(system, band)` pair the lattice admits.
    branches_per_band: BTreeMap<u8, usize>,
    /// The same histogram restricted to bands where this system **realizes
    /// at least one chamber**. Reported as context: a width the world never
    /// fills in is a draw nobody sees, and if the two histograms disagreed
    /// about the mode that would itself be the finding.
    branches_per_realized_band: BTreeMap<u8, usize>,
}

/// Read one cave system through the shipped entry points only.
fn read_system(
    seed: Seed,
    vertex: Vertex,
    cave: &Cave,
    gradient: GeothermalGradient,
    ranks: &[u8],
) -> SystemReach {
    let entrances = entrance_count(seed, vertex);

    // EXISTENCE, over the system's ONE shared lattice — walked once, not once
    // per drawn entrance (The Drift, amendment A.3: every entrance now
    // addresses INTO the same lattice, rather than realizing its own).
    // Bounded by the LATTICE's own ceilings, never by a run's drawn length:
    // bounding by the draw would make `chamber_exists`'s level gate
    // unfalsifiable here, the same trap `underworld_readout`'s module doc
    // records.
    let mut levels = 0usize;
    let mut existing: BTreeSet<ChamberAddr> = BTreeSet::new();
    // §6's FOURTH quantity, taken on the same sweep because it is a read of
    // the same gate's input: `chamber_exists` consults `branch_count_of` at
    // exactly these `(vertex, band)` pairs, so this is the draw's own shipped
    // distribution rather than a re-derivation of it.
    let mut branches_per_band: BTreeMap<u8, usize> = BTreeMap::new();
    let mut branches_per_realized_band: BTreeMap<u8, usize> = BTreeMap::new();
    for &rank in ranks {
        let band = Band::from_rank(rank).expect("ranks come from habitation_ranks()");
        *branches_per_band
            .entry(branch_count_of(seed, vertex, band))
            .or_insert(0) += 1;
        let mut realized_here = false;
        for branch in 0..BRANCHES_PER_SYSTEM {
            for level in 0..LEVELS_PER_BRANCH_CEILING {
                let addr = ChamberAddr {
                    vertex,
                    branch,
                    band,
                    level,
                };
                if chamber_exists(seed, cave, gradient, addr) {
                    levels += 1;
                    realized_here = true;
                    existing.insert(addr);
                }
            }
        }
        if realized_here {
            *branches_per_realized_band
                .entry(branch_count_of(seed, vertex, band))
                .or_insert(0) += 1;
        }
    }

    // THE MOUTHS. `EntranceMouth` is a plain struct of `branch`/`band`/`floor`
    // with no accessors, so the address is assembled from its fields; the
    // entrance index comes from the loop, because the mouth type does not
    // carry the aperture it belongs to. Every mouth now resolves into the
    // SAME shared lattice `levels` walked above.
    // THE MISMATCH MEASUREMENT (The Drift, Task 7). Before this task the
    // side-branch pick was sized against `Band::Undercroft`'s width and the
    // door was then landed by `root_floor_of` at an independently drawn
    // band, so a mouth could name a branch its LANDING band did not
    // realize — measured at 47.8% / 53.0% / 51.9% of drawn side mouths
    // across this panel. It was benign only because `chamber_exists`
    // refused the address downstream. Counted here, over the same
    // population, so the fix is verified rather than asserted.
    let mut drawn_side_mouths = 0usize;
    let mut side_mouths_off_their_bands_width = 0usize;

    let mouths: Vec<ChamberAddr> = (0..entrances)
        .map(|entrance| {
            let mouth = entrance_mouth(seed, vertex, entrance);
            let band =
                Band::from_rank(mouth.band).expect("entrance_mouth only names a habitation rank");
            if mouth.branch > 0 {
                drawn_side_mouths += 1;
                if mouth.branch >= branch_count_of(seed, vertex, band) {
                    side_mouths_off_their_bands_width += 1;
                }
            }
            ChamberAddr {
                vertex,
                branch: mouth.branch,
                band,
                level: mouth.floor,
            }
        })
        .filter(|&addr| chamber_exists(seed, cave, gradient, addr))
        .collect();

    let reached = reachable_union(seed, cave, gradient, &mouths);

    // WHERE the unreached levels sit. Task 7 deleted the lateral rule, so a
    // branch is entered only through the bands above and below it; the
    // hypothesis this measures is that what is left unreached is a TOP-BAND
    // branch no mouth landed on and no lower branch links back to, plus
    // whatever hangs beneath it. A band histogram falsifies that directly if
    // the residual is spread down the ladder instead.
    let mut unreached_by_band: BTreeMap<u8, usize> = BTreeMap::new();
    for addr in existing.difference(&reached) {
        *unreached_by_band
            .entry(
                addr.band
                    .rank()
                    .expect("existing chambers sit on habitation bands"),
            )
            .or_insert(0) += 1;
    }

    SystemReach {
        drawn_mouths: usize::from(entrances),
        open_mouths: mouths.len(),
        levels,
        reachable: reached.len(),
        drawn_side_mouths,
        side_mouths_off_their_bands_width,
        unreached_by_band,
        branches_per_band,
        branches_per_realized_band,
    }
}

/// One seed's whole-world reading — the object Task 1's re-run reused unchanged.
#[derive(Clone, Debug)]
struct ReachSummary {
    /// The seed this reading is of.
    seed: u64,
    /// Cave-bearing LAND vertices. Ocean caves are excluded and counted
    /// separately, exactly as the committed witness does.
    systems: usize,
    /// Cave-bearing ocean vertices, counted rather than silently dropped.
    ocean_systems: usize,
    /// Levels that exist, over every system and every drawn entrance.
    levels: usize,
    /// Levels reachable, one union walk per system.
    reachable: usize,
    /// Apertures drawn across the world.
    drawn_entrances: usize,
    /// Apertures whose named level exists.
    open_entrances: usize,
    /// Systems with at least one open mouth — §6's per-system denominator.
    systems_with_open_mouth: usize,
    /// Per-system reachable share, over those systems only, ascending.
    per_system_share: Vec<f64>,
    /// Existing levels per system, ascending — §6's REPORTED, never gated,
    /// quantity.
    levels_per_system: Vec<usize>,
    /// Drawn side-branch mouths across the world.
    drawn_side_mouths: usize,
    /// Of those, how many name a branch their landing band does not realize.
    side_mouths_off_their_bands_width: usize,
    /// Systems whose reachable share is strictly below 100% — amendment
    /// C.2's second reported quantity, and the count that says whether a
    /// whole-world shortfall is broad or concentrated.
    systems_below_full: usize,
    /// Unreached levels by band rank, summed over the world.
    unreached_by_band: BTreeMap<u8, usize>,
    /// §6's fourth quantity, pooled: drawn branch width -> how many
    /// `(system, habitation band)` pairs draw it.
    branches_per_band: BTreeMap<u8, usize>,
    /// The same, restricted to bands that realize a chamber — context.
    branches_per_realized_band: BTreeMap<u8, usize>,
}

impl ReachSummary {
    /// §6's whole-world quantity: the share of all existing levels reachable.
    fn world_share(&self) -> f64 {
        share(self.reachable, self.levels)
    }

    /// **Amendment D's gated per-system quantity**: the share of systems with
    /// an open mouth that reach strictly less than all of their own levels.
    fn share_below_full(&self) -> f64 {
        share(self.systems_below_full, self.systems_with_open_mouth)
    }

    /// Print the reading. Unconditional: a block read only on failure is a
    /// block nobody reads.
    fn report(&self) {
        let shares = &self.per_system_share;
        let levels_each = &self.levels_per_system;
        let mean_levels = if self.systems == 0 {
            0.0
        } else {
            self.levels as f64 / self.systems as f64
        };
        println!("seed {}", self.seed);
        println!(
            "  systems {}   ocean caves {}   levels {}   reachable {}",
            self.systems, self.ocean_systems, self.levels, self.reachable
        );
        println!(
            "  entrances {} drawn, {} open   systems with an open mouth {}",
            self.drawn_entrances, self.open_entrances, self.systems_with_open_mouth
        );
        // §6's REPORTED-NEVER-GATED quantity. The incomparability sentence
        // rides with it in the output, not only in the module header, because
        // this line is what a reader copies into a summary.
        println!(
            "  levels per system            mean {:.2}   p10 {}   median {}   p90 {}",
            mean_levels,
            pct_usize(levels_each, 0.10),
            pct_usize(levels_each, 0.50),
            pct_usize(levels_each, 0.90),
        );
        println!(
            "    (REPORTED, NEVER GATED. Counts are NOT comparable to Task 0's — \
             amendment A.5: those summed per-entrance sublattices, these are one \
             lattice per system.)"
        );

        println!("  --- §6's GATED ARMS ---");
        println!(
            "  [GATED] per-system connectivity (amendment D)  systems below 100% \
             {} of {} = {:.2}%  ->  {:?}",
            self.systems_below_full,
            self.systems_with_open_mouth,
            100.0 * self.share_below_full(),
            classify_connectivity(self.share_below_full())
        );
        println!(
            "  [GATED] whole-world reachable share  {:.2}%   ({} of {})  ->  {:?}",
            100.0 * self.world_share(),
            self.reachable,
            self.levels,
            classify_world_share(self.world_share())
        );
        println!(
            "  [GATED] branches per band  {}  ->  {:?}",
            histogram(&self.branches_per_band),
            classify_branches_per_band(&self.branches_per_band)
        );

        println!("  --- CONTEXT ONLY, NEVER GATED ---");
        println!(
            "  [context] per-system reachable share   p10 {:.2}%   median {:.2}%   \
             p90 {:.2}%",
            100.0 * pct(shares, 0.10),
            100.0 * pct(shares, 0.50),
            100.0 * pct(shares, 0.90)
        );
        println!(
            "    BLIND ZONE: `pct` reads sorted[round(q*(n-1))], so a quantile at q \
             cannot move off 100% until roughly q of the population sits below it \
             — p10 is blind to a defect touching under ~10% of systems, the median \
             to one under ~50%. Both read exactly 100.00% under two REAL \
             connectivity defects (spec amendments C.1, D.1). §6's retired median \
             table would read {:?}; amendment C.2's retired p10 table would read \
             {:?}. NEITHER IS ASSERTED.",
            retired_quantile_arm(pct(shares, 0.50)),
            retired_quantile_arm(pct(shares, 0.10)),
        );
        println!(
            "  [context] branches per REALIZED band  {}  ->  {:?}",
            histogram(&self.branches_per_realized_band),
            classify_branches_per_band(&self.branches_per_realized_band)
        );
        let unreached: Vec<String> = self
            .unreached_by_band
            .iter()
            .map(|(rank, n)| format!("rank {rank}: {n}"))
            .collect();
        println!(
            "  [context] unreached levels by band     {}",
            if unreached.is_empty() {
                "none".to_string()
            } else {
                unreached.join("   ")
            }
        );
        println!(
            "  [asserted] side mouths off their band's width  {} of {} = {:.2}%  \
             (was ~50% before Task 7)",
            self.side_mouths_off_their_bands_width,
            self.drawn_side_mouths,
            100.0
                * share(
                    self.side_mouths_off_their_bands_width,
                    self.drawn_side_mouths
                )
        );
    }
}

/// Render a `width -> count` histogram with each width's share, so a mode is
/// legible rather than inferred from raw counts.
fn histogram(hist: &BTreeMap<u8, usize>) -> String {
    let total: usize = hist.values().sum();
    hist.iter()
        .map(|(width, n)| format!("{width}: {n} ({:.2}%)", 100.0 * share(*n, total)))
        .collect::<Vec<_>>()
        .join("   ")
}

/// Measure one seed, end to end. **Task 1's re-run reused this unchanged.**
fn reach_summary(seed: Seed, wc: &WorldComponents) -> ReachSummary {
    let artifacts = build_world_to_with_artifacts(
        seed,
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        wc,
        // Terrain is the deepest rung this probe reads: `cave_at` and
        // `geothermal_gradient_at` are both `GeneratedTerrain`, and the
        // chamber lattice is a function of the seed and those two facts.
        BuildDepth::Terrain,
    )
    .expect("probe seed builds");
    let terrain = artifacts
        .terrain
        .expect("terrain is Some at BuildDepth::Terrain");
    let geo = terrain.geosphere();
    let ranks = habitation_ranks();

    let mut summary = ReachSummary {
        seed: seed.0,
        systems: 0,
        ocean_systems: 0,
        levels: 0,
        reachable: 0,
        drawn_entrances: 0,
        open_entrances: 0,
        systems_with_open_mouth: 0,
        per_system_share: Vec::new(),
        levels_per_system: Vec::new(),
        drawn_side_mouths: 0,
        side_mouths_off_their_bands_width: 0,
        systems_below_full: 0,
        unreached_by_band: BTreeMap::new(),
        branches_per_band: BTreeMap::new(),
        branches_per_realized_band: BTreeMap::new(),
    };

    for vertex in geo.vertices() {
        // `cave_at` refuses an ocean vertex as its first act, so the ocean test
        // is a COUNT of a case that never carries a cave today, not a guard
        // the walk depends on — and the day it does carry one, the artifact
        // and this probe both say so instead of silently including it.
        let Some(cave) = terrain.cave_at(vertex) else {
            continue;
        };
        if terrain.is_ocean(vertex) {
            summary.ocean_systems += 1;
            continue;
        }
        summary.systems += 1;
        let gradient = terrain.geothermal_gradient_at(vertex);
        let sys = read_system(seed, vertex, &cave, gradient, &ranks);

        summary.drawn_entrances += sys.drawn_mouths;
        summary.open_entrances += sys.open_mouths;
        summary.levels += sys.levels;
        summary.reachable += sys.reachable;
        summary.levels_per_system.push(sys.levels);
        summary.drawn_side_mouths += sys.drawn_side_mouths;
        summary.side_mouths_off_their_bands_width += sys.side_mouths_off_their_bands_width;
        for (rank, n) in &sys.unreached_by_band {
            *summary.unreached_by_band.entry(*rank).or_insert(0) += n;
        }
        for (width, n) in &sys.branches_per_band {
            *summary.branches_per_band.entry(*width).or_insert(0) += n;
        }
        for (width, n) in &sys.branches_per_realized_band {
            *summary
                .branches_per_realized_band
                .entry(*width)
                .or_insert(0) += n;
        }
        if sys.open_mouths > 0 && sys.reachable < sys.levels {
            summary.systems_below_full += 1;
        }
        if sys.open_mouths > 0 {
            summary.systems_with_open_mouth += 1;
            summary
                .per_system_share
                .push(share(sys.reachable, sys.levels));
        }
    }

    summary.per_system_share.sort_by(|a, b| a.total_cmp(b));
    summary.levels_per_system.sort_unstable();
    summary
}

/// **THE PREREGISTERED READOUT (Task 8).** Measures all four of §6's
/// quantities across the panel, prints them split into GATED and CONTEXT, and
/// **asserts the arm each frozen table lands in**.
///
/// It began life as Task 0's baseline, which printed §6's arms rather than
/// gating them because they described a world the campaign had not yet built.
/// That is no longer true, and the arms are now applied.
///
/// # What is ASSERTED
///
/// 1. **Non-vacuity, before any share is read.** Every ratio below is `0/0`
///    over an empty corpus and renders as a plausible `0.00%`, which is
///    indistinguishable from a genuine zero. Each denominator is asserted
///    non-empty first.
/// 2. **Amendment D.3's per-system connectivity table** — the gated arm,
///    intent 0%. See [`ConnectivityVerdict`].
/// 3. **§6's whole-world table**, intent `>= 90%`. See
///    [`WorldShareVerdict`].
/// 4. **The ratchet on the whole-world share** — [`WORLD_SHARE_FLOOR`],
///    tighter than arm 3 and asserted on every seed, not only seed 42.
/// 5. **§6's branches-per-band table.** See [`BranchesPerBandVerdict`].
/// 6. **Seed 42's triple**, exactly — see [`SEED_42_BASELINE`].
/// 7. **The union is not a sum.** `reachable <= levels` per seed: a sum of
///    per-entrance walks can exceed the level count, a union cannot, so this
///    is the cheapest standing check that the double-count hazard the spec
///    names has not reappeared. It is also what makes [`WORLD_SHARE_FLOOR`]
///    one-sided rather than incomplete.
///
/// # NOTHING RUNS THIS FOR YOU
///
/// Every assertion listed above sits behind this test's own `ignore`
/// attribute, whose reason puts it in the `heavy` set — and `heavy` is
/// not in the merge queue's phase list (decision 0148 took it off, with
/// `seam-guard`, because the two were 80.5% of a merge's wall time). So this
/// readout, its ratchet, both gated arms and seed 42's equality run **only
/// when a human types `make heavy-remote`** — on no schedule, and not at a
/// merge. The honest sentence about anything below is *"it closes that on any
/// heavy run"*, never *"it closes that"*. Still a strict improvement on the
/// state before this campaign, where even a heavy run printed and passed.
///
/// # What is NOT asserted, and must never be
///
/// **The p10 and the median.** Amendment D.4 is explicit: under both defects
/// this campaign measured they read a perfect 100.00% while a real
/// connectivity failure was live. They are printed under a `[context]` label
/// with their blind zone stated in the same breath; asserting them would
/// manufacture confidence from a statistic proved unable to fail. See
/// [`RetiredQuantileArm`], whose variant names carry the same warning.
///
/// **Levels per system.** §6: "REPORTED, NEVER GATED", and amendment A.5 adds
/// that the absolute counts are not comparable across this campaign at all.
///
/// claim: rate(the four quantities spec §6 preregisters — per-system
/// connectivity, whole-world reachable share, levels per system, branches per
/// band; seeds 42 / 7 / 1234) — the campaign's readout. Nothing here is
/// asserted "for all seeds": the panel says what the world is, the frozen
/// tables say which arm that is, and the by-construction guarantees behind the
/// result are asserted over CONSTRUCTED widths in `chamber.rs`'s own tests
/// (spec §6's closing line: a guarantee is asserted, never measured into
/// existence). The only equality is against seed 42's committed witness
/// (decision 0093).
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn the_drift_reachability_baseline() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");

    let summaries: Vec<ReachSummary> = SEEDS.iter().map(|&s| reach_summary(Seed(s), &wc)).collect();

    for summary in &summaries {
        summary.report();
    }

    assert_eq!(
        summaries.len(),
        SEEDS.len(),
        "every seed in the panel must contribute a reading"
    );
    assert!(
        !habitation_ranks().is_empty(),
        "the delve ladder yielded no habitation rank — the whole band scan is \
         vacuous and every count below would be 0"
    );

    for summary in &summaries {
        let seed = summary.seed;
        assert!(
            summary.systems > 0,
            "seed {seed}: no cave systems — the probe is vacuous"
        );
        assert!(
            summary.levels > 0,
            "seed {seed}: no level exists anywhere — every share is 0/0 and \
             reads as a finding"
        );
        assert!(
            summary.systems_with_open_mouth > 0,
            "seed {seed}: no system has an open mouth — §6's per-system \
             distribution has an empty corpus"
        );
        assert_eq!(
            summary.per_system_share.len(),
            summary.systems_with_open_mouth,
            "seed {seed}: the per-system distribution must carry exactly one \
             entry per system with an open mouth"
        );
        assert!(
            summary.reachable <= summary.levels,
            "seed {seed}: {} levels reached but only {} exist — a walk was \
             summed rather than unioned",
            summary.reachable,
            summary.levels
        );
        assert_eq!(
            summary.side_mouths_off_their_bands_width, 0,
            "seed {seed}: {} of {} drawn side mouths name a branch their landing \
             band does not realize. Task 7 was supposed to make this unaskable by \
             construction — see `entrance_mouth`'s doc",
            summary.side_mouths_off_their_bands_width, summary.drawn_side_mouths
        );
        assert!(
            summary.drawn_side_mouths > 0,
            "seed {seed}: no side-branch mouth was drawn at all, so the zero \
             above is vacuous"
        );
        // AMENDMENT D.3's GATED ARM — the THIRD statistic to hold this job.
        // §6 gated the median, amendment C replaced it with the p10, and
        // amendment D replaced that with this share, because a mutation
        // proved each of the first two could read a perfect 100.00% while
        // connectivity was broken. This one has no blind zone: one affected
        // system moves it. Its intent is 0% and amendment E.2 is what makes
        // that arm reachable rather than aspirational — every top-band branch
        // is named by an entrance, every branch descends (§4.5), every branch
        // below has a parent (§4.5), so a system's lattice is one component.
        // The denominator is asserted non-empty above, so this zero is a real
        // zero and not an empty corpus. A red here says a system somewhere
        // cannot reach part of its own underworld — a finding about the
        // guarantee set, not a bound to widen.
        assert_eq!(
            classify_connectivity(summary.share_below_full()),
            ConnectivityVerdict::Intent,
            "seed {seed}: {} of {} systems with an open mouth reach less than \
             100% of their own levels ({:.2}%). Amendment D's gated arm is 0% \
             and E.2 is supposed to make it structural — see \
             `chamber::every_top_band_branch_is_named_by_an_entrance`",
            summary.systems_below_full,
            summary.systems_with_open_mouth,
            100.0 * summary.share_below_full()
        );

        // §6's WHOLE-WORLD ARM, the one table no amendment had to replace.
        assert_eq!(
            classify_world_share(summary.world_share()),
            WorldShareVerdict::Intent,
            "seed {seed}: whole-world reachable share {:.2}% ({} of {}) is below \
             §6's 90% intent. §6 requires the SPLIT: is a connectivity hole \
             surviving §4.5's guarantees, or are entrance-less systems commoner \
             than expected? The unreached-by-band histogram in the report above \
             separates them.",
            100.0 * summary.world_share(),
            summary.reachable,
            summary.levels
        );

        // THE RATCHET, which is tighter than the arm above and is asserted on
        // every seed rather than only on seed 42 — see WORLD_SHARE_FLOOR for
        // why it is exactly 1.0 and what would legitimately move it.
        assert!(
            summary.world_share() >= WORLD_SHARE_FLOOR,
            "seed {seed}: whole-world reachable share {:.2}% ({} of {}) fell \
             below the ratchet floor of {:.2}%. §6's arm would still call this \
             the intent, which is exactly why the ratchet exists. This is a \
             connectivity FINDING, not a bound to widen: read \
             WORLD_SHARE_FLOOR's \"what would legitimately move it\" list, and \
             move it only with a spec amendment that says so.",
            100.0 * summary.world_share(),
            summary.reachable,
            summary.levels,
            100.0 * WORLD_SHARE_FLOOR
        );

        // §6's FOURTH quantity. The panel lands on `ModeIsOne`, so that is
        // what is pinned; §6's other named arm is a REPORT arm, not a STOP
        // arm, so a red here is an instruction to write the finding down and
        // NOT to re-weight `branch_count_of` to rescue it.
        assert!(
            !summary.branches_per_band.is_empty(),
            "seed {seed}: the branches-per-band histogram is empty, so its \
             verdict is vacuous"
        );
        assert_eq!(
            classify_branches_per_band(&summary.branches_per_band),
            BranchesPerBandVerdict::ModeIsOne,
            "seed {seed}: branches per band is {} — §6's arm is `mode 1`. If \
             the mode has moved above 1, §6 says report it as a finding and do \
             NOT re-weight `branch_count_of` to rescue it.",
            histogram(&summary.branches_per_band)
        );

        assert!(
            summary.open_entrances <= summary.drawn_entrances,
            "seed {seed}: more open mouths ({}) than drawn ({})",
            summary.open_entrances,
            summary.drawn_entrances
        );
    }

    let s42 = summaries
        .iter()
        .find(|s| s.seed == 42)
        .expect("seed 42 is in the panel");
    assert_eq!(
        (s42.levels, s42.reachable, s42.open_entrances),
        SEED_42_BASELINE,
        "seed 42's baseline moved. Spec §1 and \
         docs/audits/underworld-lattice-seed-panel.md both record \
         (levels, reachable, open entrances) = {SEED_42_BASELINE:?}. If The \
         Drift's deletion has landed, this is the expected break and the new \
         triple belongs in SEED_42_BASELINE and in this module's header; if it \
         has not, something moved the world and that is the finding."
    );
}

/// Every arm of every frozen table in this file is reachable, and no gap is
/// rounded into a neighbouring arm.
///
/// A pure test over the four classifiers, costing no world build, because
/// **the panel reaches exactly one arm of each** — `Intent`, `Intent`,
/// `ModeIsOne`, `WouldReadTheIntent`. The four enums declare **14** variants
/// between them (4 + 3 + 3 + 4), so without this the other **ten**
/// would be unexercised code asserting nothing, and a classifier that silently
/// folded a gap into its passing arm would read as a healthy green forever.
/// It is `stope_variety_probe.rs`'s
/// `a_tie_for_the_modal_branch_count_is_not_rounded_into_the_passing_arm`,
/// widened to every table this file applies.
///
/// The boundary cases are the point. `0.05` is the last share amendment D.3
/// calls "report" and `0.050001` is the first it calls STOP; `0.90` is §6's
/// first "intent" and `0.8999` its last "below". A classifier written with `<`
/// where the table says `<=` moves a real defect one arm to the left, and
/// nothing else in this suite would notice.
#[test]
fn every_frozen_arm_is_reachable_and_no_gap_is_rounded_into_a_passing_arm() {
    // AMENDMENT D.3 — the gated per-system arm.
    assert_eq!(
        classify_connectivity(0.0),
        ConnectivityVerdict::Intent,
        "exactly zero systems below full is D.3's intent"
    );
    assert_eq!(
        classify_connectivity(0.0001),
        ConnectivityVerdict::Report,
        "one affected system in ten thousand is the REPORT arm, not the intent \
         — D.3's whole point is that it has no blind zone"
    );
    assert_eq!(
        classify_connectivity(SYSTEMS_BELOW_FULL_REPORT_CEILING),
        ConnectivityVerdict::Report,
        "5% inclusive is still the report arm"
    );
    assert_eq!(
        classify_connectivity(0.0501),
        ConnectivityVerdict::DefectSurvivesTheGuarantees,
        "just above 5% is the STOP arm"
    );
    assert_eq!(
        classify_connectivity(f64::NAN),
        ConnectivityVerdict::UnspecifiedByTheBrief,
        "a NaN share must never resolve to Intent — a broken instrument \
         reported as a perfect world is the failure mode this variant exists \
         for"
    );

    // §6 — the whole-world arm.
    assert_eq!(
        classify_world_share(1.0),
        WorldShareVerdict::Intent,
        "the shipped reading"
    );
    assert_eq!(
        classify_world_share(WORLD_SHARE_INTENT),
        WorldShareVerdict::Intent,
        "90% inclusive is the intent"
    );
    assert_eq!(
        classify_world_share(0.8999),
        WorldShareVerdict::BelowIntent,
        "just under 90% is not the intent"
    );
    assert_eq!(
        classify_world_share(1.5),
        WorldShareVerdict::UnspecifiedByTheBrief,
        "a share above 1 means the walk was summed rather than unioned, which \
         is a statement about the instrument and not about the world"
    );

    // §6 — branches per band.
    let one: BTreeMap<u8, usize> = [(1u8, 101usize), (2, 100)].into_iter().collect();
    assert_eq!(
        classify_branches_per_band(&one),
        BranchesPerBandVerdict::ModeIsOne
    );
    let two: BTreeMap<u8, usize> = [(1u8, 100usize), (2, 101)].into_iter().collect();
    assert_eq!(
        classify_branches_per_band(&two),
        BranchesPerBandVerdict::ModeIsAboveOne
    );
    let tied: BTreeMap<u8, usize> = [(1u8, 100usize), (2, 100)].into_iter().collect();
    assert_eq!(
        classify_branches_per_band(&tied),
        BranchesPerBandVerdict::UnspecifiedByTheBrief,
        "a 1-vs-2 tie must not resolve to an arm §6 never named"
    );
    assert_eq!(
        classify_branches_per_band(&BTreeMap::new()),
        BranchesPerBandVerdict::UnspecifiedByTheBrief,
        "an empty histogram has no mode"
    );

    // The RETIRED tables. Reachable, so the report can say what they WOULD
    // have said — and never asserted anywhere else in this file.
    assert_eq!(
        retired_quantile_arm(1.0),
        RetiredQuantileArm::WouldReadTheIntent
    );
    assert_eq!(
        retired_quantile_arm(0.60),
        RetiredQuantileArm::WouldReadReport
    );
    assert_eq!(
        retired_quantile_arm(0.10),
        RetiredQuantileArm::WouldReadTheDeletionFailed
    );
    assert_eq!(
        retired_quantile_arm(f64::INFINITY),
        RetiredQuantileArm::UnspecifiedByTheBrief
    );
}
