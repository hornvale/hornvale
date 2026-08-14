//! The Repose's exposure readout (spec §6): do settlements over-occupy
//! high-unrest ground relative to the land base rate?
//!
//! BASELINE FIRST (spec §6.7). This file is written and run BEFORE any
//! geohazard code exists, so a reading of ~1 is learned before anything is
//! built on the premise that it is not.
//!
//! Stratified by elevation band (spec §6.4) because unrest correlates with
//! two OPPOSED things at once — see the plan's §0.2: the mineral-reward
//! channel attracts and the hostility penalty repels, and an unstratified
//! ratio can read ~1 because they cancel, which is indistinguishable from no
//! effect.
//!
//! World-building idiom reused verbatim from `occupancy_readout.rs` and
//! `demesne.rs`.
//!
//! # Dated measurement (2026-08-12, Task 1 baseline, CORRECTED in fix round 2,
//! band labels RELABELLED in fix round 3)
//!
//! **This section states the CURRENT, corrected reading only.** The
//! population-filter defect fix round 2 repaired (below) invalidated every
//! number originally recorded here; none of the original Task-1 figures are
//! repeated in this section; where they matter historically they are kept,
//! clearly labelled superseded, in the fix-round-1 and fix-round-2 sections
//! that follow. Fix round 3 renamed the four elevation-band labels (see
//! `BANDS`'s doc comment and the land-share table just below) — a pure
//! relabelling that moved no number; the fix-round-3 section at the end of
//! this doc proves that byte-for-byte.
//!
//! Committed fixture: `fixtures/repose-exposure.csv`, seeds 1..=30, 440,715
//! settleable land cells and **26,146** settlements pooled over the sweep
//! (population 1,467,398) — spec §6.2's population applied correctly to
//! both sides of the readout (land AND settlements).
//!
//! **The band labels are datum-neutral metre ranges, and land share by band
//! is the context every reader needs before reading the table below.**
//! `elevation_at` is relative to an isostatic reference datum, not to sea
//! level: sea level itself sits far below zero on that datum
//! (`terrain.sea_level()` on seed 42 is -2,936.17 m — `waterline_probe.rs`'s
//! correction header). So height-above-sea-level on ordinary continental
//! land routinely reaches several kilometres, and an absolute 2500 m cut
//! captures nearly half of all settleable land — not a mountain fringe:
//!
//! | band | land cells | land share | settlement share |
//! |---|---|---|---|
//! | `0-250m` | 50,792 | 11.5% | 65.5% |
//! | `250-1000m` | 55,764 | 12.7% | 14.4% |
//! | `1000-2500m` | 140,937 | 32.0% | 13.0% |
//! | `2500m+` | 193,222 | 43.8% | 7.0% |
//!
//! `2500m+` is the LARGEST band by land area of the four, at 43.8% — a
//! sentence like "the effect is strongest in mountain terrain" would be a
//! claim about mountains this data does not support. The stratification
//! itself is unaffected by any of this: it still separates land
//! monotonically by elevation, exactly what spec §6.4 asks for as confound
//! control. What was wrong, and is fixed as of this section, is only that
//! the ORIGINAL labels (`lowland`/`upland`/`highland`/`montane`) imported an
//! Earth intuition this world's datum does not support.
//!
//! **Deciles are NOT degenerate.** Unaffected by the fix (land accounting was
//! never the buggy side): land splits almost exactly evenly across the ten
//! unrest deciles — 44,044 to 44,149 cells each (a spread under 0.25%),
//! despite unrest being a smooth field with a real mass of cells near zero
//! on calm interiors. Tied values did not collapse the low deciles;
//! `decile_of`'s rank-based partition handles them cleanly.
//!
//! **Pooled exposure ratio (settlement share ÷ land-area share), by band ×
//! decile (0 = calmest, 9 = most unrest), corrected baseline:**
//!
//! | band | decile 0 | decile 4 | decile 9 | direction across deciles |
//! |---|---|---|---|---|
//! | `0-250m` | 5.59 | 5.81 | 5.46 | flat, no clear trend (range 5.46-5.97) |
//! | `250-1000m` | 0.992 | 1.109 | 1.559 | rising, ×1.57 (minor wobble at d3, d5) |
//! | `1000-2500m` | 0.321 | 0.345 | 0.827 | rising, ×2.58 (one flat step d0→d1) |
//! | `2500m+` | 0.073 | 0.116 | 0.392 | rising, ×5.39 (minor wobble at d1, d7) |
//!
//! **The confound the header names is still visible in the corrected data,
//! and the headline SURVIVES the correction essentially unchanged in
//! shape**, though every absolute number moved. `0-250m` settlements sit at
//! ~5.5-6× the land-area base rate with no clear trend across deciles
//! (fertile/coastal pull swamps any unrest signal there); the three higher
//! bands climb broadly WITH unrest from calmest to most-unrest decile (not
//! perfectly monotonic step-to-step, but the first-to-last rise is large
//! and one-directional in all three: ×1.57, ×2.58, ×5.39). These three rise
//! factors are close to the ORIGINAL contaminated reading's (×1.6, ×2.5,
//! ×5.5) — expected, and explained below in the fix-round-2 section: the
//! excluded settlements were entirely absent from the three higher bands
//! already (their true elevation could never band them there), so those
//! three bands' RATIOS moved only by the common rescale of the shared
//! pooled-total denominator, which preserves a band's shape across deciles
//! even as its absolute level shifts. `0-250m`'s absolute level and pattern
//! changed more, since that is where the excluded settlements had been
//! silently counted. Spec §6.5's three outcomes anticipate exactly this
//! kind of reading, not a null: settlements DO measurably over-occupy
//! high-unrest ground, but only above the coastal fringe — the same
//! qualitative headline as originally reported, now resting on the correct
//! population.
//!
//! **Per-people dispersion, corrected — dominated by ONE land-dwelling
//! kind, not by sample size.** Totals across the sweep: `pooled` 26,146
//! settlements / 1,467,398 population; `twig-blight` 25,510 / 1,454,060
//! (97.6% of the corrected pooled settlement total on its own);
//! `shrieker` 352 / 8,100; `rust-monster` 104 / 2,367; `drow` 153 / 2,204;
//! `kobold` 27 / 667. `giant-squid`, `reef-shark` and `sea-elf` no longer
//! appear at all — every settlement they had was excluded (see fix round
//! 2). `twig-blight` alone now tracks the pooled pattern almost exactly
//! (its own band × decile table is within a few settlements of pooled's at
//! every stratum, and it is the only one of the five surviving kinds present
//! in all four bands); the remaining four kinds are low-count enough that a
//! single settlement still moves their own share by several percent.
//!
//! **Those four low-count kinds sit at EXACTLY 100% of the `2500m+` band —
//! and this is a real preference, not a repeat of the fix-round-2 artifact.**
//! `kobold` (n=27), `rust-monster` (n=104), `drow` (n=153) and `shrieker`
//! (n=352) each place every one of their settlements in `2500m+` and none
//! anywhere else. Two things distinguish this from the marine-default
//! artifact fix round 2 removed: `band_of`'s fallback is the FIRST band
//! (`0-250m`), never the last, so a default-branch bug would read as
//! spurious `0-250m` concentration, not `2500m+` — the opposite band from
//! what these four kinds show; and the probability of landing 100% in
//! `2500m+` by chance is vanishingly small given that band's own 43.8% land
//! share — for `kobold`'s smallest sample, `0.438^27 ≈ 2.1×10⁻¹⁰` (the other
//! three kinds' probabilities are smaller still, by many more orders of
//! magnitude, since they have larger n). This reads as a genuine niche
//! preference for high ground, not an artifact of either defect this file
//! has already found and fixed.
//!
//! # Fix round 1 (2026-08-12): guard-encoding repairs
//!
//! **The numbers quoted in this section were measured against the
//! ORIGINAL, marine-contaminated Task-1 fixture, before fix round 2's
//! population-filter correction (below) — kept here as the historical
//! record of what was found and repaired, not as current fact.** The
//! discrimination-guard numbers happen to be byte-identical in the
//! corrected fixture too (land/soil accounting was never the buggy side —
//! reconfirmed in fix round 2 below), so that half is still accurate as
//! stated. The ceiling-guard numbers are NOT: `sea-elf` (the kind that
//! triggered the original ceiling breach) no longer exists in the corrected
//! fixture at all, because its four settlements were entirely marine. The
//! repair itself (scoping the ceiling to `pooled` rows) remains correct and
//! necessary regardless.
//!
//! **As first encoded (Task 1, 2026-08-12), both heavy-tier guards below
//! FAILED against the original baseline, and neither failure looked like a
//! bug in this probe** (each traced to a specific, reproducible cause):
//!
//! - `unrest_deciles_differ_in_andosol_share`: andosol_share ran the
//!   OPPOSITE direction from the guard's original directional assumption —
//!   0.92% at decile 0 falling monotonically to 0.00% at decile 9, and
//!   monotonically decreasing within EVERY band checked separately (not
//!   merely in the pooled mix). `SoilOrder::Andosol` requires volcanic
//!   parent rock AND `mean_temp_c > 5.0`; high-unrest ground in this terrain
//!   model runs colder within a band than low-unrest ground does, which is
//!   enough to gate Andosol out even where the parent rock qualifies. A
//!   genuine finding, not degeneracy or a probe defect (the deciles are
//!   demonstrably NOT uniform; they just disagreed with the original
//!   encoding's assumed sign).
//! - `exposure_ratios_are_within_absurdity_bounds`: `sea-elf` (4 total
//!   settlements across the whole 30-seed sweep, ALL of them marine — see
//!   fix round 2) read exposure ratios up to 22.95, over the 20.0 ceiling,
//!   at three separate (decile, `0-250m`) strata. A single coastal-specialist
//!   settlement moves its own share by 25%; the ceiling was never
//!   calibrated against a per-people denominator this small. `pooled`'s own
//!   ratios all stayed under 8.
//!
//! Nathan ruled on both findings above; recorded here per decision 0016 and
//! this project's standing rule ("don't retune a constant to rescue a
//! prediction after unblinding without saying so"). **Both are repairs of
//! the plan text's encoding, not rescues of a falsified prediction** — in
//! neither case did the measured baseline change, and in neither case was a
//! threshold loosened to make a specific number pass.
//!
//! - **Discrimination guard.** Originally asserted `hi > lo * 2.0` — a
//!   DIRECTIONAL claim spec §6.7's own text never made ("assert the unrest
//!   deciles genuinely differ in andosol share" — DIFFER, not "differ in a
//!   specific direction"). The deciles do differ, enormously (0.0092 to
//!   0.0000 is a bigger relative move than the original 2× threshold
//!   demanded) — only the encoding's assumed sign was wrong. Repaired to a
//!   direction-free `(hi - lo).abs() > 0.002`. A `hi/lo` ratio form was not
//!   available regardless of direction, because the top decile measures
//!   exactly `0.0` and any ratio divides by zero; `0.002` is roughly a
//!   quarter of the observed `0.00915` spread, chosen to leave headroom
//!   against ordinary noise while still failing if the field ever went
//!   genuinely flat.
//! - **Ceiling guard.** Originally bounded every row regardless of `people`.
//!   Spec §6.3 asks for per-people dispersion to be REPORTED; spec §6.7 asks
//!   for a ceiling but never names the population it bounds against. Scoped
//!   to `pooled` rows only — per-people rows are still computed and written
//!   to the fixture completely unchanged, they are simply no longer
//!   asserted on.
//!
//! Both guards passed against the (still marine-contaminated) fixture as it
//! stood at the end of fix round 1 — see the task-1 report's fix-round-1
//! addendum for the exact commands and output. Both are RE-VERIFIED against
//! the corrected baseline in fix round 2 below.
//!
//! # Fix round 2 (2026-08-12): the population-filter defect
//!
//! **Critical finding, independently confirmed from the committed fixture
//! before this fix**: the settlement side of this readout never applied
//! the `is_settleable` predicate the land side already used. A marine
//! settlement's attractor cell is ocean — negative elevation-above-sea-level
//! — so `band_of`'s loop never satisfied any threshold and fell through to
//! its `BANDS[0]` (the `0-250m` band) default, and its decile was computed
//! against `sorted_unrest`, a distribution built ONLY from settleable land: a
//! meaningless lookup for a cell that was never in that distribution. The
//! fingerprint was unmistakable: `giant-squid` (30,971 settlements),
//! `reef-shark` (2,121) and `sea-elf` (4) each read EXACTLY 100.0% `0-250m`
//! with zero everywhere else — not an ecological distribution — and
//! `giant-squid` alone was 51.9% of the original pooled total.
//!
//! **The fix applies the SAME `is_settleable` closure — the identical
//! object already bound once per seed for the land tally, not a re-derived
//! copy — to the settlement loop.** Per spec §6.2's exact definition
//! (settleable land = not ocean AND non-zero carrying capacity), this
//! excludes both true marine settlements and the much smaller residual of
//! non-ocean cells with zero carrying capacity (e.g. a founder-floor
//! settlement placed at a barren cell by [`hornvale_demography::stack_condense`]'s
//! floor mechanism, which bypasses the normal density threshold and can
//! land anywhere with nonzero inflow) — the same reason the small residual
//! of excluded settlements is not purely 100%/0% split by kind; see the
//! per-kind counts below.
//!
//! **Excluded per people (old total → new total, excluded count and
//! share):**
//!
//! | people | old settlements | new settlements | excluded | excluded share |
//! |---|---|---|---|---|
//! | `giant-squid` | 30,971 | 0 | 30,971 | 100.0% |
//! | `reef-shark` | 2,121 | 0 | 2,121 | 100.0% |
//! | `sea-elf` | 4 | 0 | 4 | 100.0% |
//! | `shrieker` | 719 | 352 | 367 | 51.0% |
//! | `rust-monster` | 163 | 104 | 59 | 36.2% |
//! | `drow` | 175 | 153 | 22 | 12.6% |
//! | `kobold` | 27 | 27 | 0 | 0.0% |
//! | `twig-blight` | 25,510 | 25,510 | 0 | 0.0% |
//! | **pooled** | **59,690** | **26,146** | **33,544** | **56.2%** |
//!
//! Pooled population fell from 1,523,644 to 1,467,398 (56,246 excluded) —
//! a smaller *proportional* drop than the settlement count's, because the
//! excluded settlements were disproportionately small (marine specialists
//! at low individual headcount) next to `twig-blight`'s large, entirely-
//! land-based population, which the fix does not touch at all.
//!
//! **Regression guard**: `no_settlement_in_the_readout_sits_outside_the_settleable_land_population`
//! (below) independently re-derives the settleable-land-only settlement
//! count per seed and asserts it equals `exposure_rows`' own pooled total.
//! Verified to actually catch the regression, not just describe it: with
//! the `is_settleable` check temporarily removed from the settlement loop,
//! the guard failed with `left: 59690, right: 26146` — exactly the old
//! (contaminated) and new (corrected) pooled totals, confirmed against each
//! other independently of the fixture. Restored before commit; full output
//! pasted in the task-1 report's fix-round-2 addendum.
//!
//! **Both fix-round-1 guards RE-VERIFIED against the corrected baseline —
//! same no-tuning rule, reported honestly:**
//!
//! - **Discrimination guard.** The land-cell-weighted andosol-share spread
//!   the `0.002` threshold was calibrated against is BYTE-IDENTICAL to
//!   before this fix (decile 0: 0.009151, decile 9: 0.000000 — land and
//!   soil accounting were never the buggy side, so this could not have
//!   moved). The `0.002` threshold's headroom is therefore unchanged
//!   (`0.009151 - 0.002 = 0.007151` of margin) and needed no re-picking.
//!   Guard PASSES.
//! - **Ceiling guard.** Re-run against `pooled` rows only in the corrected
//!   fixture: the maximum pooled `exposure_ratio` is now 5.97 (down from
//!   the pre-fix-round-2 maximum of ~8, since the removed settlements had
//!   been inflating `pooled`'s own `0-250m` numbers too), comfortably under
//!   the 20.0 ceiling. Guard PASSES.
//!
//! **Re-derived headline**: whether settlements over-occupy high-unrest
//! ground was reopened by this defect and re-measured, not assumed to
//! survive. It DOES survive, with the same qualitative shape reported
//! above under "Dated measurement" — flat `0-250m`, broadly rising through
//! the three higher bands — now measured against the population spec §6.2
//! actually specifies.
//!
//! # Dated measurement (2026-08-12, Task 2: the three counterfactual arms)
//!
//! Task 2 ablates the channels by which unrest could reach siting, through
//! `hornvale_worldgen::ChannelMask`. Task 1's baseline is unchanged by any of
//! it — `ChannelMask::NONE` is an IEEE-754 no-op, and the committed fixture
//! above is byte-identical after the threading.
//!
//! **Instrument sensitivity (the calibration a later reader needs before
//! judging any future null).** 30 seeds, 59,690 settlements total of which
//! 26,146 are on settleable land (that figure matching Task 1's corrected
//! pooled total exactly, independently re-derived).
//!
//! **UNITS, because the first encoding of this table got them wrong.** It
//! reported a SYMMETRIC DIFFERENCE over a denominator of SETTLEMENTS. Those
//! are different things: a settlement that RELOCATES contributes TWO cells to
//! a symmetric difference, the one it left and the one it took, so the
//! original "3,036 of 26,146 = 11.6%" was inflated. **The inflation has no
//! 2× ceiling** — an earlier draft claimed one, but 2× is the PURE-RELOCATION
//! case; an arm that also creates settlements inflates further, and arm B's
//! own row does exactly that at 399/193 = 2.07×. The figure to
//! quote is **`vacated`** — baseline attractor cells that hold no settlement
//! under the arm — which is 1:1 with baseline settlements. See [`Movement`]
//! for the full definition; measured values are in the fix-round-2 section at
//! the end of this doc.
//!
//! Both positive controls fire, so the harness is NOT blind and arm C's null
//! is decidable. Arm C is green: no siting-path source reads a soil order or
//! a soil fertility.
//!
//! **A movement count cannot attribute, so the gradient was re-taken under
//! each arm** (`which_channel_carries_the_exposure_gradient`). Pooled
//! exposure ratio, decile 0 → decile 9 rise factor. The rise factor is a
//! ratio of two ratios within one arm, so the arm-dependent
//! `total_settlements_of(people)` denominator cancels exactly; the land-share
//! denominator is byte-identical across arms by construction. Compare rise
//! factors, not absolute `d0`/`d9` levels.
//!
//! | band | base | arm A (hostility off) | arm B (mineral unrest off) | **arm AB (BOTH off)** |
//! |---|---|---|---|---|
//! | `0-250m` | ×0.978 | ×1.109 | ×0.976 | **×1.109** |
//! | `250-1000m` | ×1.572 | ×1.880 | ×1.554 | **×1.873** |
//! | `1000-2500m` | ×2.578 | ×2.751 | ×2.506 | **×2.660** |
//! | `2500m+` | ×5.395 | ×5.836 | ×5.125 | **×5.459** |
//!
//! **THE COMBINED ARM IS THE DECISIVE READING, and it is a post-hoc addition
//! (fix round 2, 2026-08-12) declared as such.** Arms A and B were measured
//! and read before it existed. Without it the residual is a SUBTRACTION over
//! two separately measured ablations, which silently assumes the two channels
//! are ADDITIVE. `{hostility, mineral_unrest}` severs unrest from siting
//! through BOTH wires at once and reads the residual directly.
//!
//! **Severing both channels does not reduce the gradient. It leaves it at or
//! ABOVE baseline in every band.** Measured as excess over unity, the
//! combined arm retains 101.5% of the baseline gradient in `2500m+`
//! (4.459 vs 4.395), 105.2% in `1000-2500m`, and 152.6% in `250-1000m`.
//! (`0-250m` is deliberately absent from that series: its baseline rise is
//! BELOW unity, so "share of the excess over unity" divides by a negative
//! number and is undefined as a share — completing the series there yields
//! −495.5%, which means nothing. That band is read through the sign-reversal
//! paragraph below instead.) So
//! the residual is no longer an inference from two subtractions — it is
//! **directly measured**: with unrest disconnected from siting through every
//! wire this campaign could find, the settlement pattern still over-occupies
//! high-unrest ground by essentially the same factor.
//!
//! **The additivity assumption was in fact wrong, which is why this arm was
//! worth running — and the failure is not even in a consistent DIRECTION.**
//! Subtracting the two single-channel deltas from the baseline predicts a
//! combined value; comparing that prediction against the direct reading, per
//! band:
//!
//! | band | predicted by subtraction | direct (arm AB) | deviation | verdict |
//! |---|---|---|---|---|
//! | `0-250m` | ×1.107 | ×1.109 | +0.002 | super, WITHIN rounding noise |
//! | `250-1000m` | ×1.862 | ×1.873 | **+0.011** | **SUPER-additive** |
//! | `1000-2500m` | ×2.679 | ×2.660 | −0.019 | sub-additive |
//! | `2500m+` | ×5.566 | ×5.459 | −0.107 | sub-additive |
//!
//! **Do not summarise this as "the channels are sub-additive".** An earlier
//! draft did, generalising from the `2500m+` row alone; `250-1000m` runs the
//! other way, and its +0.011 deviation exceeds the ±0.003 envelope implied by
//! printing rise factors to three decimals, so it is a real reversal rather
//! than noise. `0-250m` is also super-additive but by +0.002, inside that
//! envelope, and is not resolvable either way.
//!
//! This makes the case AGAINST the subtraction stronger, not weaker: the two
//! channels do not combine additively, and the sign of the error is not even
//! stable across bands, so no correction factor could have rescued the
//! inferred residual. Only the direct reading was ever going to answer it.
//!
//! **THE `0-250m` BAND CROSSES UNITY UNDER ABLATION — the gradient there does
//! not merely rise, it REVERSES SIGN.** Baseline ×0.978, arm A ×1.109, arm AB
//! ×1.109. A rise factor below 1 means low-elevation settlements slightly
//! AVOID high-unrest ground as unrest climbs; above 1 means they prefer it.
//! With the modelled unrest channels severed, the sign flips.
//!
//! What that licenses: in this band the modelled channels were **suppressing**
//! a latent attraction — remove them and the underlying preference for
//! high-unrest lowland ground becomes visible. It is the only band where an
//! ablation changes the DIRECTION of the relationship rather than its
//! magnitude, and it is arguably the most interesting single number in the
//! table.
//!
//! What it does NOT license: any statement about magnitude or mechanism.
//! ×0.978 and ×1.109 both sit close to unity, this is one pooled statistic
//! over 30 seeds with no dispersion measured across them, and no
//! preregistered prediction covered it. Arm B's power limit applies here as
//! everywhere, so "the modelled channels" means hostility plus a mineral term
//! 99.6% of the population does not read — in practice this is arm A's
//! doing, which the near-identical arm A and arm AB values corroborate.
//! Treat it as a lead worth its own measurement, not a result.
//!
//! **THE HEADLINE IS THE RESIDUAL, AND IT IS NOW DIRECTLY MEASURED: on the
//! shipped roster, essentially ALL of the observed gradient is UNATTRIBUTED.
//! Severing unrest from siting through BOTH of its channels at once leaves
//! the gradient at or above baseline in every band (`2500m+` ×5.459 against a
//! baseline ×5.395). Hostility opposes the effect, soil is disconnected, and
//! the mineral channel is untestable here. The arms DO NOT DECOMPOSE the
//! effect.** Task 1 established that soil never reaches siting
//! and that andosol is anti-correlated with unrest, and concluded that the
//! mineral/prospectivity channel was therefore "the only remaining candidate
//! explanation". After Task 2 that conclusion is not available, and neither
//! is its negation.
//!
//! **The scope clause "on the shipped roster" is load-bearing and must not be
//! dropped when this is restated.** An unscoped "explained by none of the
//! three channels" would be a negative claim about the mineral channel —
//! exactly what the arm-B paragraph below forbids, since that arm cannot test
//! it. What is claimed is that the gradient is unattributed BY THIS
//! INSTRUMENT ON THIS ROSTER. Something else co-varying with unrest is siting
//! these settlements, and identifying it is OPEN.
//!
//! Arm by arm, with each arm's evidential weight stated rather than implied:
//!
//! - **Arm A (hostility) — WELL-POWERED AND CONCLUSIVE. The penalty is a
//!   BRAKE on the effect, not its cause.** Ablating it RAISES the rise in
//!   every band (×5.395 → ×5.836 in `2500m+`, ×1.572 → ×1.880 in
//!   `250-1000m`), which is the physically correct sign: removing a term
//!   that repels settlement from high-unrest ground lets more settlement
//!   onto it. It reaches the whole population — `hostility` is a term in
//!   `carrying_capacity` for every kind, and it vacates 1,518 baseline
//!   settlement cells, 5.81% of the settleable-land population — so this can
//!   be stated plainly.
//! - **Arm B (mineral) — a working POSITIVE CONTROL but UNDERPOWERED for
//!   attribution. The mineral hypothesis is UNTESTED, not refuted.** It
//!   vacates 193 baseline settlement cells (0.74%), so the harness
//!   demonstrably sees the channel; the
//!   gradient falls slightly in all four bands (×5.395 → ×5.125 in
//!   `2500m+`, ~6% of the effect). That 6% is a fact about the ROSTER, not
//!   about the channel, and the reason is quantified in the next paragraph.
//!   **Do not read this arm as a refutation.** A well-powered version of it
//!   does not exist today and cannot be built without changing the roster.
//! - **Arm C (soil) — the connectivity null, decidable and green.** No
//!   siting-path source reads a soil order or a soil fertility.
//!
//! **Why arm B is underpowered — a ROSTER fact, quantified, and worth
//! knowing on its own.** Only two shipped kinds declare any `MINERAL` niche
//! weight, and both are pure-`MINERAL`
//! (`ResourceVector::new(&[(MINERAL, 1.0)])`, `domains/species/src/lib.rs`
//! at the `xorn` and `rust-monster` entries). In the settleable-land
//! population this readout measures:
//!
//! - **`xorn` does not appear AT ALL** — zero settlements across the whole
//!   30-seed sweep, and it is absent from the committed fixture's `people`
//!   column entirely. It is a registered kind that places nothing. (`xorn`
//!   is `Subterranean`, so `per_species_suitability`'s cave-availability
//!   gate zeroes its K on every cell without a cave; that is the likely
//!   cause but this probe did not measure it, and a registered-but-unplaced
//!   kind is worth its own look.)
//! - **`rust-monster` holds 104 of the 26,146 settlements — 0.4%.**
//!   `twig-blight` alone is 97.6% and takes nothing from that axis.
//!
//! So arm B ablates a channel **99.6% of the settled population does not
//! read**. Its small contribution measures how little the CURRENT ROSTER
//! reaches through the mineral channel, and must not be read as "the mineral
//! channel is intrinsically weak" or as evidence against the mineral
//! hypothesis. A roster with a mineral-weighted PEOPLE in it would have to
//! re-take this reading — the same shelf life spec §6.6 declares for arm C.
//!
//! # Task 2, fix round 2 (2026-08-12): movement in the right units
//!
//! **The instrument-sensitivity table originally reported a SYMMETRIC
//! DIFFERENCE against a denominator of SETTLEMENTS.** Those are different
//! units: one settlement that relocates contributes TWO cells to a symmetric
//! difference. Superseded figures, kept so the correction is legible: "arm A
//! moved 3,036 of 26,146 = 11.6%" and "arm B moved 399 = 1.5%". Arm A's was
//! inflated by exactly 2× and arm B's by 2.07× — **2× is not an upper bound**,
//! only the pure-relocation case; an arm that also creates settlements
//! exceeds it, as arm B does.
//!
//! **Corrected, decomposed** (30 seeds, settleable-land population, base
//! 26,146 of 59,690 total attractor cells). `vacated` is the quotable
//! share-of-baseline figure; see [`Movement`]:
//!
//! | arm | vacated | share of 26,146 | newly occupied | net | changed cells |
//! |---|---|---|---|---|---|
//! | A — hostility off | 1,518 | **5.81%** | 1,518 | +0 | 3,036 |
//! | B — mineral unrest off | 193 | **0.74%** | 206 | +13 | 399 |
//! | AB — both off | 1,555 | **5.95%** | 1,533 | −22 | 3,088 |
//!
//! **The decomposition earns its keep immediately** — none of this was
//! visible in a symmetric difference, which sums the two halves and cannot
//! tell relocation from creation. Arm A vacates and re-occupies exactly
//! 1,518 cells; arm B nets +13; the combined arm nets −22.
//!
//! **WITHDRAWN, and by measurement (fix round 3, 2026-08-12): arm A is NOT
//! "pure relocation".** This paragraph read arm A's pooled `net == 0` as
//! "moves settlements without creating or destroying a single one". That
//! inference does not follow from a POOLED sum, so it was turned into a
//! per-seed assertion — and the assertion went RED. Measured per seed:
//!
//! - Occupied-cell net is **nonzero in 29 of 30 seeds**, spanning **−21 to
//!   +12**; only seed 29 nets zero. The pooled `+0` is these thirty values
//!   cancelling, not an absence of change.
//! - The settleable SETTLEMENT-count delta per seed is **identical to the
//!   cell net at every seed** (same vector, −21..+12), so on this population
//!   no arm-side cell hosts two settlements. That 1:1-ness is measured here,
//!   not assumed — [`hornvale_demography::stack_condense`] can place two
//!   settlements on one cell, so it had to be checked rather than inherited
//!   from the baseline.
//!
//! Corrected statement: **arm A produces no net change in AGGREGATE
//! settleable-land occupancy across the sweep, while creating and destroying
//! settlements within individual worlds.** Nothing is asserted about how
//! relocation-dominated it is: the honest guard would need a threshold, none
//! was preregistered, and inventing one after seeing the numbers is the
//! rescue this campaign forbids.
//!
//! Everything else in the Task-2 section above is measured in the corrected
//! units, and the positive controls are unaffected (both arms still move
//! settlements, which is all they assert).
//!
//! # Dated measurement (2026-08-14, Task 7: the knownness column)
//!
//! The readout gains a tenth column, `knownness` — the population-weighted
//! mean of [`hornvale_worldgen::knownness`]'s stock over the settlements of
//! each stratum, so that spec §7's named risk ("knownness ships with no
//! consumer and cannot be seen to be wrong") is closed by an actual consumer.
//! **The regen was purely ADDITIVE and that was proven rather than asserted**:
//! both fixtures cut to columns 1–9 are byte-identical (md5
//! `f44dda61bb5685a8da819944c56e256b` before and after), so every Task-1 and
//! Task-2 number above still reads exactly as it did.
//!
//! **The column is SPARSE, and the sparsity is the finding rather than a
//! defect.** 10 of the 240 rows are non-zero. All ten are `twig-blight` and
//! its `pooled` reflection, and all ten sit in unrest deciles 8 and 9 — which
//! is where they must be, since an edifice is an island-arc feature and an arc
//! is high-unrest ground by construction.
//!
//! **Per people, over the 30-seed settleable-land sweep** (measured directly,
//! not read off the column):
//!
//! | people | settlements | on an edifice | remembering | generation | half-life | horizon |
//! |---|---|---|---|---|---|---|
//! | `twig-blight` | 25,510 | 409 | **89** | 15.70 y | 31.40 y | 313.9 y |
//! | `shrieker` | 352 | **0** | 0 | 25.53 y | 51.07 y | 510.7 y |
//! | `drow` | 153 | **0** | 0 | 140.95 y | 281.90 y | 2,819.0 y |
//! | `rust-monster` | 104 | **0** | 0 | 48.50 y | 97.00 y | 970.0 y |
//! | `kobold` | 27 | **0** | 0 | 30.24 y | 60.48 y | 604.8 y |
//!
//! **Read the four zeros correctly: they are "no mountain", NOT "forgot".**
//! Four of the five peoples place not one settlement on an edifice cell
//! anywhere in the sweep, so their stock is zero for want of anything to
//! remember. Conflating that with forgetting would be the campaign's own
//! headline claim asserted on a population that cannot support it.
//!
//! **The one people that does live on mountains is mostly wrong about them.**
//! Of `twig-blight`'s 409 settlements on an edifice, **89 remember an
//! eruption and 320 — 78.2% — do not**, at a half-life of 31.4 years against
//! eruption intervals authored at 200–5,000 years. That is spec §1's Vesuvius
//! sentence measured on a world: towns on the flanks of a mountain nobody
//! remembers going off. It is an OBSERVATION and no part of it was
//! preregistered.
//!
//! **The cross-species spread is also an observation, and the instrument
//! CANNOT test the obvious hypothesis.** `drow` carry a `LifeSchedule::paced`
//! factor of 5.0 and therefore a 2,819-year memory horizon against
//! `twig-blight`'s 314 — nine times the reach — so "long-lived peoples
//! remember longer" would be a natural prediction and this readout has **zero
//! power** to test it: `drow` hold no edifice settlements at all, so the
//! comparison has no sample on one side. See
//! [`hornvale_worldgen::knownness`]'s module doc for why this campaign
//! declines to preregister that claim in any case (the spec froze "no
//! cross-species claim" before the axis went live, and adding one after
//! seeing that it had is the post-hoc move decision 0016 exists to prevent).
//! A campaign that wants the question answered needs a roster whose
//! long-lived peoples settle volcanic ground, and should say so in a fresh
//! spec before measuring.
//!
//! **`now` is the same in every world.** The present frame `present_year`
//! reports is exactly 2000.0 years on all 30 seeds — a constant of the bake
//! configuration, not a per-world draw — so none of the variation above comes
//! from asking different worlds about different moments.
#![allow(clippy::disallowed_methods)]

use std::collections::{BTreeMap, BTreeSet};

use hornvale_demography::stack_condense::HeadcountRender;
use hornvale_kernel::{CellId, KindId, Seed, World, quantize};
use hornvale_worldgen::{
    ChannelMask, SettlementPins, SkyChoice, WorldComponents, build_world_from_components,
    climate_from, demography_report_from, demography_report_from_masked, generation_length_of,
    knownness, terrain_of,
};

/// How many unrest deciles the readout stratifies into.
const DECILES: usize = 10;

/// Elevation bands, in metres above sea level, as (label, lower-inclusive
/// bound). The top band is open. Chosen to separate the coastal fringe from
/// the higher ground that spec §6.4 names as the repelling half of the
/// confound.
///
/// **Labels are explicit, datum-neutral metre ranges (fix round 3,
/// 2026-08-12), not terrain names.** They originally read `lowland`/
/// `upland`/`highland`/`montane` — Earth-intuitive names this world's
/// numbers do not support: `elevation_at` is relative to an isostatic
/// datum, not sea level, and sea level itself sits far below zero on that
/// datum (`terrain.sea_level()` on seed 42 is -2,936.17 m — see
/// `waterline_probe.rs`'s correction header). So height-above-sea-level
/// routinely runs to several kilometres on ordinary continental land, and
/// the top band (`2500m+`, née "montane") turns out to hold 43.8% of all
/// settleable land — the LARGEST of the four bands, not a mountain fringe.
/// See this file's module doc for the full land-share table and its
/// implications. The stratification itself — separating land monotonically
/// by elevation, spec §6.4's confound control — is unchanged: only the
/// labels moved, never the thresholds, order, or semantics. Renaming is
/// PROVEN not to move a number in the task-1 report's fix-round-3 addendum
/// (a byte-for-byte diff of every non-label column, before vs. after).
const BANDS: [(&str, f64); 4] = [
    ("0-250m", 0.0),
    ("250-1000m", 250.0),
    ("1000-2500m", 1000.0),
    ("2500m+", 2500.0),
];

/// One stratum's readout: a (decile, band, people) cell of the design.
#[derive(Debug, Clone, PartialEq)]
struct ExposureRow {
    /// Unrest decile, 0..DECILES (0 = calmest tenth of settleable land).
    decile: usize,
    /// Elevation band label, from `BANDS`.
    band: &'static str,
    /// The people this row is for, or "pooled" for the all-peoples row.
    people: &'static str,
    /// Settleable land cells in this stratum, summed over seeds.
    land_cells: u64,
    /// Settlements whose attractor cell falls in this stratum.
    settlements: u64,
    /// Total headcount at those settlements.
    population: f64,
    /// settlement share / land-area share.
    exposure_ratio: f64,
    /// population share / land-area share.
    weighted_ratio: f64,
    /// Share of this stratum's land cells classified `Andosol` — the
    /// discrimination guard's input (spec §6.7).
    andosol_share: f64,
    /// **Population-weighted mean knownness** of this stratum's settlements
    /// (Task 7): `sum(population * stock) / sum(population)`, or 0.0 where
    /// the stratum holds no population.
    ///
    /// **The mean is over EVERY settlement in the stratum, not over the few
    /// that have a mountain**, so it reads as a share: how much of this
    /// stratum's population lives with a remembered eruption. A settlement on
    /// a cell with no edifice contributes a 0 by
    /// [`hornvale_worldgen::knownness`]'s own definition — there is nothing
    /// there to remember — and only 409 of the 26,146 settleable-land
    /// settlements, 1.56%, sit on an edifice at all (measured, module doc).
    /// A reader who wants "of those with a mountain, how much is remembered"
    /// must divide by that share; this column deliberately does not, because
    /// the denominator is itself a finding rather than a constant.
    knownness: f64,
}

/// The seed-`n` world at full build depth, built through the composition
/// root exactly as `occupancy_readout.rs` does.
fn world_of(seed: u64, wc: &WorldComponents) -> World {
    build_world_from_components(
        Seed(seed),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
        wc,
    )
    .expect("seed builds at default pins")
}

/// Which elevation band a land cell falls in. Metres ABOVE SEA LEVEL, never
/// above the isostatic datum: `terrain.sea_level()` on seed 42 is
/// -2,936.17 m, and the two disagree on thousands of cells — the exact trap
/// `waterline_probe.rs`'s correction header documents.
fn band_of(terrain: &hornvale_terrain::GeneratedTerrain, cell: CellId) -> &'static str {
    let above = terrain.elevation_at(cell).get() - terrain.sea_level().get();
    let mut chosen = BANDS[0].0;
    for (label, lower) in BANDS {
        if above >= lower {
            chosen = label;
        }
    }
    chosen
}

/// The unrest decile of a cell, given the sorted settleable-land unrest
/// values for its world. Ties break to the LOWER decile so the mapping is a
/// deterministic function of the value, not of iteration order.
fn decile_of(sorted_unrest: &[f64], u: f64) -> usize {
    let n = sorted_unrest.len();
    if n == 0 {
        return 0;
    }
    let rank = sorted_unrest.partition_point(|v| *v < u);
    ((rank * DECILES) / n).min(DECILES - 1)
}

/// A present species' rendered headcount, converted to a plain `f64` "how
/// many bodies" figure for the population axis of this readout. `Count(n)`
/// is exact; `Lone` (a sub-one-body presence, rendered as flavor rather than
/// `Count(0)`) counts as one body present; `Colony(v)` — reserved for a
/// future colonial/hive extent, not fired by any Stage-A species today —
/// counts as its own apportioned extent. Not a save-format quantity: this
/// readout's own convention, applied consistently across every settlement.
fn headcount_of(render: HeadcountRender) -> f64 {
    match render {
        HeadcountRender::Count(n) => f64::from(n),
        HeadcountRender::Lone => 1.0,
        HeadcountRender::Colony(v) => v,
    }
}

/// Accumulated land-base tallies for one (decile, band) stratum, summed over
/// every seed's settleable land — independent of "people": land has no
/// owner, so this table is shared by every per-people row and the pooled
/// row alike.
#[derive(Debug, Clone, Copy, Default)]
struct LandTally {
    /// Settleable land cells in this stratum.
    cells: u64,
    /// Of those, the ones classified `Andosol`.
    andosol: u64,
}

/// Accumulated settlement tallies for one (decile, band, people) stratum.
#[derive(Debug, Clone, Copy, Default)]
struct SettlementTally {
    /// Settlements whose attractor cell falls in this stratum.
    count: u64,
    /// Total headcount ([`headcount_of`]) at those settlements.
    population: f64,
    /// `sum(population * knownness_stock)` over those settlements — the
    /// NUMERATOR of the population-weighted mean, accumulated rather than the
    /// mean itself so that seeds pool without a weighted average of weighted
    /// averages. Divided by `population` at row-render time.
    knownness_weight: f64,
}

/// Build the full exposure-row vector for `seeds`: one row per (decile,
/// band, people) cell of the design, "pooled" plus every kind that founds at
/// least one settlement anywhere in the sweep. Pure aside from world genesis
/// — same `seeds` in, byte-identical rows out.
///
/// **Judgement call (a seed with no settlements at all):** nothing special —
/// a seed simply contributes zero to every settlement tally it would
/// otherwise have touched, exactly like any other seed's non-contribution to
/// a stratum it has no settlements in. The hazard this guards against is not
/// "a seed with zero settlements" (harmless) but "a PEOPLE with zero
/// settlements across the WHOLE 30-seed sweep", which would otherwise divide
/// a zero numerator by a zero denominator when its settlement SHARE is
/// computed below. Guarded explicitly at that point (see
/// `total_settlements_of`/`total_population_of` below): a people with no
/// settlements anywhere never enters the `peoples` roster in the first
/// place, so the divide never happens; and even if it somehow did, every
/// ratio below reads a zero share rather than a NaN whenever its
/// denominator is zero.
fn exposure_rows(seeds: impl IntoIterator<Item = u64>) -> Vec<ExposureRow> {
    exposure_rows_masked(seeds, ChannelMask::NONE)
}

/// [`exposure_rows`] under a channel ablation (Task 2). At
/// [`ChannelMask::NONE`] this IS `exposure_rows` — the delegation above is
/// the only caller that path has, so the committed fixture is what pins it.
///
/// **Only the SETTLEMENT side moves under a mask.** The land tally, the
/// unrest deciles, the elevation bands and the `is_settleable` population are
/// all derived from terrain and climate through the UNMASKED
/// `carrying_inputs_of` below. That is deliberate: an arm that moved its own
/// population could not be attributed, since a ratio would then shift for two
/// reasons at once and neither could be separated.
///
/// **PRECISELY WHICH denominator that fixes, and which it does not (corrected
/// fix round 3, 2026-08-12 — this doc previously claimed a blanket
/// "byte-identical to the baseline's", and `which_channel_carries_the_exposure_gradient`
/// cites THIS function as its authority, so the error propagated).**
/// `exposure_ratio` is `settlement_share / land_area_share`:
///
/// - `land_area_share`'s denominator is `total_land`, summed from the land
///   tally above. Arm-INVARIANT, byte-identical across arms.
/// - `settlement_share`'s denominator is `total_settlements_of(people)`,
///   which sums this arm's own per-stratum settlement counts. **It DOES move
///   between arms** — an arm can create or destroy settlements, not only
///   relocate them (measured: arm B nets +13, arm AB nets −22).
///
/// The consumer is safe anyway, but for a reason worth stating rather than
/// assuming: it reads the `d9/d0` RISE FACTOR, and both strata divide by the
/// same per-arm `total_settlements_of(people)`, so that denominator cancels
/// exactly. Absolute `d0`/`d9` levels are NOT comparable across arms; rise
/// factors are.
fn exposure_rows_masked(
    seeds: impl IntoIterator<Item = u64>,
    mask: ChannelMask,
) -> Vec<ExposureRow> {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    // The build-local dense index -> KindId mapping, built ONCE from the
    // exact same `wc.biosphere` ordering `demography_report_from` uses
    // internally (ascending-KindId order) — see `demesne.rs:317`.
    let kinds: Vec<KindId> = wc.biosphere.iter().map(|(k, _)| *k).collect();

    let mut land: BTreeMap<(usize, &'static str), LandTally> = BTreeMap::new();
    let mut settle: BTreeMap<(usize, &'static str, &'static str), SettlementTally> =
        BTreeMap::new();
    let mut peoples: BTreeSet<&'static str> = BTreeSet::new();
    // TASK 7. A people's generation length, resolved ONCE and reused for
    // every settlement of that people in every seed. `generation_length_of`
    // re-assembles every canonical registry on each call, and the answer is a
    // property of the roster rather than of the world (its `world` argument is
    // unused today), so resolving it per settlement would cost ~26,000
    // registry assemblies to learn five values. `None` is a real answer, not
    // a lookup failure: an `Ametabolic` kind has no mass-derived life history,
    // and `memory_half_life` has an authored fallback for exactly that.
    let mut generations: BTreeMap<&'static str, Option<hornvale_kernel::Years>> = BTreeMap::new();

    for seed in seeds {
        let world = world_of(seed, &wc);
        let terrain = terrain_of(&world).expect("terrain reconstructs");
        let climate = climate_from(&world, &terrain).expect("climate reconstructs");
        let geo = terrain.geosphere();

        // Settleable land = not ocean AND positive carrying capacity — the
        // one predicate `ConditionNiche`'s corrected frame uses, and no
        // other (task brief, step 3.3).
        let capacity = hornvale_demography::carrying_capacity(
            geo,
            &hornvale_worldgen::carrying_inputs_of(geo, &terrain, &climate),
        );
        let is_settleable = |cell: CellId| !terrain.is_ocean(cell) && capacity.at(cell) > 0.0;

        // This seed's settleable-land unrest distribution, sorted once and
        // reused for every decile lookup below (land cells AND settlements
        // alike) — `decile_of`'s contract.
        let mut sorted_unrest: Vec<f64> = geo
            .cells()
            .filter(|&c| is_settleable(c))
            .map(|c| terrain.unrest_at(c))
            .collect();
        sorted_unrest.sort_by(|a, b| a.total_cmp(b));

        let soils = hornvale_worldgen::soil_of(&terrain, &climate, geo);

        for cell in geo.cells() {
            if !is_settleable(cell) {
                continue;
            }
            let decile = decile_of(&sorted_unrest, terrain.unrest_at(cell));
            let band = band_of(&terrain, cell);
            let entry = land.entry((decile, band)).or_default();
            entry.cells += 1;
            if *soils.get(cell) == hornvale_terrain::SoilOrder::Andosol {
                entry.andosol += 1;
            }
        }

        // TASK 7. The moment knownness is asked about: this world's own
        // present frame, the same "today" every other backward-looking read
        // in this repo measures from. Not `WorldTime::GENESIS` and not an
        // arbitrary epoch — a stock evaluated at genesis would report what a
        // people knew before it existed.
        let now = hornvale_kernel::WorldTime::new(hornvale_worldgen::ledger_day_of_bake_year(
            hornvale_worldgen::present_year(&world),
        ))
        .expect("the present frame is a finite day");

        let report = demography_report_from_masked(&world, &wc, &terrain, &climate, mask)
            .expect("demography report reconstructs");
        for s in &report.stack_settlements {
            // FIX ROUND 2 (2026-08-12): spec §6.2 fixes the population as
            // settleable land only. A marine settlement's cell fails
            // `is_settleable` (it's ocean), so it must be excluded here with
            // the SAME predicate object the land tally above already used —
            // not a re-derived copy. Before this fix the settlement loop
            // applied no filter at all: a marine settlement's negative
            // elevation-above-sea-level fell through `band_of`'s loop to the
            // `BANDS[0]` (the `0-250m` band) default, and its decile was computed
            // against `sorted_unrest`, a distribution built ONLY from
            // settleable land — meaningless for a cell that was never in it.
            // See this file's module doc for the measured blast radius.
            if !is_settleable(s.cell) {
                continue;
            }
            let decile = decile_of(&sorted_unrest, terrain.unrest_at(s.cell));
            let band = band_of(&terrain, s.cell);
            let population: f64 = s
                .rendered
                .iter()
                .map(|(_, r)| headcount_of(*r))
                .sum::<f64>();
            let people = kinds[s.dominant as usize].0;
            peoples.insert(people);

            // TASK 7. What THIS people still knows of the mountain under this
            // settlement, at this world's present. The holder is the
            // settlement's dominant kind — the same key
            // `hornvale_worldgen::volcano_name` uses for the name that people
            // has for the mountain, so a people that forgets its mountain
            // loses its name for it too.
            let generation = *generations.entry(people).or_insert_with(|| {
                generation_length_of(&world, people)
                    .and_then(|y| hornvale_kernel::Years::new(y).ok())
            });
            let stock = knownness(Seed(seed), &terrain, people, generation, s.cell, now).stock;

            let per_people = settle.entry((decile, band, people)).or_default();
            per_people.count += 1;
            per_people.population += population;
            per_people.knownness_weight += population * stock;

            let pooled = settle.entry((decile, band, "pooled")).or_default();
            pooled.count += 1;
            pooled.population += population;
            pooled.knownness_weight += population * stock;
        }
    }

    // Land-area share's denominator: total settleable land across the WHOLE
    // sweep and every stratum — the same figure for every row regardless of
    // which people it belongs to, since land has no owner.
    let total_land: u64 = land.values().map(|t| t.cells).sum();

    // Each people's own settlement/population totals — the denominators of
    // ITS settlement share and population share. A people that founds zero
    // settlements anywhere never appears in `peoples` (built above from
    // actually-observed `dominant` tags), so this never divides 0 / 0 for a
    // ghost row; see the judgement-call note on this function.
    let total_settlements_of = |people: &'static str| -> u64 {
        settle
            .iter()
            .filter(|((_, _, p), _)| *p == people)
            .map(|(_, t)| t.count)
            .sum()
    };
    let total_population_of = |people: &'static str| -> f64 {
        settle
            .iter()
            .filter(|((_, _, p), _)| *p == people)
            .map(|(_, t)| t.population)
            .sum()
    };

    // "pooled" first, then every observed people in ascending order — a
    // fixed, deterministic row order independent of hash/iteration quirks
    // (both sides are `BTreeSet`/`Vec` already, but the explicit "pooled
    // first" placement is a rendering choice, not a data property).
    let mut people_order: Vec<&'static str> = vec!["pooled"];
    people_order.extend(peoples.iter().copied());

    let mut rows = Vec::with_capacity(people_order.len() * DECILES * BANDS.len());
    for &people in &people_order {
        let total_settlements = total_settlements_of(people);
        let total_population = total_population_of(people);
        for decile in 0..DECILES {
            for (band, _) in BANDS {
                let land_tally = land.get(&(decile, band)).copied().unwrap_or_default();
                let settle_tally = settle
                    .get(&(decile, band, people))
                    .copied()
                    .unwrap_or_default();

                let land_area_share = if total_land > 0 {
                    land_tally.cells as f64 / total_land as f64
                } else {
                    0.0
                };
                let settlement_share = if total_settlements > 0 {
                    settle_tally.count as f64 / total_settlements as f64
                } else {
                    0.0
                };
                let population_share = if total_population > 0.0 {
                    settle_tally.population / total_population
                } else {
                    0.0
                };
                // Guarded on the DENOMINATOR (land_area_share), not the
                // numerator: a stratum with no land at all reads ratio 0.0
                // rather than 0/0 = NaN, and a people with no settlements
                // anywhere already reads settlement_share/population_share
                // 0.0 above for the same reason.
                let exposure_ratio = if land_area_share > 0.0 {
                    settlement_share / land_area_share
                } else {
                    0.0
                };
                let weighted_ratio = if land_area_share > 0.0 {
                    population_share / land_area_share
                } else {
                    0.0
                };
                let andosol_share = if land_tally.cells > 0 {
                    land_tally.andosol as f64 / land_tally.cells as f64
                } else {
                    0.0
                };
                // Guarded on the WEIGHT's own denominator, matching the
                // shares above: a stratum this people has no population in
                // reads 0.0 rather than 0/0 = NaN. Named `_mean` rather than
                // taking the field's shorthand so it does not shadow the
                // `knownness` FUNCTION this file imports.
                let knownness_mean = if settle_tally.population > 0.0 {
                    settle_tally.knownness_weight / settle_tally.population
                } else {
                    0.0
                };

                rows.push(ExposureRow {
                    decile,
                    band,
                    people,
                    land_cells: land_tally.cells,
                    settlements: settle_tally.count,
                    population: settle_tally.population,
                    exposure_ratio,
                    weighted_ratio,
                    andosol_share,
                    knownness: knownness_mean,
                });
            }
        }
    }
    rows
}

/// Render the repose exposure readout CSV for every seed in `seeds`. Pure
/// aside from world genesis: same `seeds` in, byte-identical string out (the
/// drift check below depends on this). Quantizes every float at THIS
/// boundary only — `exposure_rows` runs at full precision throughout.
fn render_repose_exposure(seeds: impl IntoIterator<Item = u64>) -> String {
    let rows = exposure_rows(seeds);
    let mut out = String::from(
        "decile,band,people,land_cells,settlements,population,exposure_ratio,weighted_ratio,andosol_share,knownness\n",
    );
    for r in &rows {
        out.push_str(&format!(
            "{},{},{},{},{},{},{},{},{},{}\n",
            r.decile,
            r.band,
            r.people,
            r.land_cells,
            r.settlements,
            quantize(r.population),
            quantize(r.exposure_ratio),
            quantize(r.weighted_ratio),
            quantize(r.andosol_share),
            quantize(r.knownness),
        ));
    }
    out
}

#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn repose_exposure_readout_matches_the_committed_fixture() {
    let committed = include_str!("fixtures/repose-exposure.csv");
    let rendered = render_repose_exposure(1..=30);
    assert_eq!(
        rendered, committed,
        "repose exposure readout drifted — if this is intended, rewrite the \
         fixture with `cargo test -p hornvale-worldgen --test repose_exposure \
         -- --ignored rewrite_repose_exposure_fixture` and commit the diff \
         WITH the change that moved it"
    );
}

/// DISCRIMINATION (spec §6.7). Enforces that the unrest deciles genuinely
/// SEPARATE on andosol share — direction: it catches a probe that has become
/// blind, never a world that has become uniform. Without this the readout
/// passes green while measuring nothing (The Benchmark's vacuous-and-green
/// failure).
///
/// **Deviation from the brief, noted per the task's resolution of
/// ambiguity:** the brief's draft summed `andosol_share` (already a
/// per-stratum SHARE, in `[0, 1]`) across the four elevation bands within a
/// decile, which can reach 4.0 and is dimensionally meaningless as a
/// discrimination statistic. Replaced with a `land_cells`-weighted MEAN of
/// `andosol_share` across the four bands within a decile — a proper `[0, 1]`
/// share of that decile's land, guarded against a zero denominator (a decile
/// that happens to carry no land at all, e.g. under decile collapse — see
/// this file's dated measurement note above).
///
/// **Post-unblinding repair (2026-08-12, fix round 1), recorded per decision
/// 0016 and this project's standing rule against retuning a prediction after
/// unblinding without saying so:** the ORIGINAL encoding asserted a directional
/// `hi > lo * 2.0` (top decile at LEAST double the bottom). Run for real
/// against the committed baseline, it FAILED: `unrest_deciles_differ_in_andosol_share`
/// panicked with `(bottom 0.0092, top 0.0000)` — andosol share runs the
/// OPPOSITE direction from what that encoding assumed (see this file's dated
/// measurement note above for why: `SoilOrder::Andosol` requires
/// `mean_temp_c > 5.0`, and high-unrest ground runs colder within a band).
/// Spec §6.7's own words are "assert the unrest deciles genuinely differ in
/// andosol share" — DIFFER, not "differ in a specific direction". The deciles
/// plainly do differ (enormously: 0.0092 to 0.0000 is a bigger relative move
/// than the original `2×` threshold demanded), so the guard's INTENT was
/// already satisfied; only the plan text's directional encoding was wrong.
/// This is a repair of that encoding error, not a rescue of a falsified
/// prediction — the direction found (andosol decreasing with unrest) is left
/// exactly as measured and reported in the module doc above, unchanged by
/// this fix. Repaired as an absolute, direction-free separation: a plain
/// `hi/lo` RATIO is not available here regardless of direction, because the
/// top decile measures exactly `0.0` and any ratio form divides by zero — an
/// absolute difference is the only shape that survives that. The `0.002`
/// threshold is roughly a quarter of the observed `0.00915` spread between
/// deciles 0 and 9: enough headroom that ordinary sweep-to-sweep noise won't
/// trip it, while still failing if the field ever went genuinely flat. Test
/// name and the verbatim `heavy:` ignore string are unchanged.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn unrest_deciles_differ_in_andosol_share() {
    let rows = exposure_rows(1..=30);
    let pooled: Vec<&ExposureRow> = rows.iter().filter(|r| r.people == "pooled").collect();
    let weighted_mean_andosol_share = |decile: usize| -> f64 {
        let land: u64 = pooled
            .iter()
            .filter(|r| r.decile == decile)
            .map(|r| r.land_cells)
            .sum();
        if land == 0 {
            return 0.0;
        }
        let weighted: f64 = pooled
            .iter()
            .filter(|r| r.decile == decile)
            .map(|r| r.andosol_share * r.land_cells as f64)
            .sum();
        weighted / land as f64
    };
    let lo = weighted_mean_andosol_share(0);
    let hi = weighted_mean_andosol_share(DECILES - 1);
    assert!(
        (hi - lo).abs() > 0.002,
        "unrest deciles do not separate on andosol share \
         (bottom {lo:.4}, top {hi:.4}) — the probe is measuring nothing and \
         would pass green regardless"
    );
}

/// FLOOR AND CEILING (spec §6.7). Enforces BOTH directions on the POOLED row
/// only: an absurdly LOW exposure ratio and an absurdly HIGH one both fail. A
/// floor alone cannot catch a runaway, and a bound asserted only against the
/// side you expect to move is not a bound.
///
/// **Post-unblinding repair (2026-08-12, fix round 1), recorded per decision
/// 0016 and this project's standing rule against retuning a prediction after
/// unblinding without saying so:** the ORIGINAL encoding iterated every row
/// regardless of `people`, so `sea-elf` (4 total settlements across the whole
/// 30-seed sweep) tripped the `20.0` ceiling at three strata (up to 22.95) —
/// a single settlement moving a 4-settlement kind's own stratum share by 25%,
/// not a runaway. Spec §6.3 asks for per-people dispersion to be REPORTED;
/// spec §6.7 asks for a ceiling but never names the population it bounds.
/// Per-people rows are still computed and written to the fixture completely
/// unchanged by this fix — they remain the §6.3 deliverable — they are simply
/// no longer asserted on here. **This guard now covers `pooled` rows only**;
/// a later reader must not mistake it for coverage of the whole fixture. Test
/// name and the verbatim `heavy:` ignore string are unchanged; the `20.0`/
/// `is_finite()` thresholds are unchanged, only the population they run over.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn exposure_ratios_are_within_absurdity_bounds() {
    for r in exposure_rows(1..=30)
        .iter()
        .filter(|r| r.people == "pooled")
        .filter(|r| r.land_cells > 0)
    {
        assert!(
            r.exposure_ratio < 20.0,
            "absurd-HIGH exposure ratio {:.2} at decile {} band {} people {} \
             — a runaway, not a finding",
            r.exposure_ratio,
            r.decile,
            r.band,
            r.people
        );
        assert!(
            r.exposure_ratio.is_finite(),
            "non-finite exposure ratio at decile {} band {} people {}",
            r.decile,
            r.band,
            r.people
        );
    }
}

/// POPULATION GUARD (spec §6.2, fix round 2, 2026-08-12). Direction: this
/// catches a settlement entering the readout from a cell the land tally
/// never counted. It cannot catch a settleable-land cell being misbanded.
///
/// **Regression guard for the fix-round-2 defect**: the settlement loop
/// once applied no `is_settleable` filter at all, so a marine settlement's
/// cell (ocean, negative elevation-above-sea-level) fell through `band_of`'s
/// loop to the `BANDS[0]` (the `0-250m` band) default and was counted
/// against a decile distribution built only from settleable land. 52% of the
/// pooled sweep (three kinds reading exactly 100.0% `0-250m` with zero
/// everywhere else) was contaminated this way before the fix — see the module doc's
/// fix-round-2 paragraph for the measured blast radius.
///
/// Deliberately does NOT call `exposure_rows` and trust its internal
/// filter — that would be circular, proving only that the function agrees
/// with itself. Instead it INDEPENDENTLY re-derives, per seed, the
/// settleable-land-only settlement count (the same `is_settleable` shape,
/// written out again here on purpose: this guard's whole job is to notice
/// if the two ever disagree) and asserts it equals `exposure_rows`' own
/// pooled settlement total. If the filter in the settlement loop is ever
/// removed or weakened, the independently-counted total stays fixed while
/// the readout's pooled total rises by however many marine settlements
/// leaked back in, and this assertion fails.
///
/// claim: structural(seed: 1..=30) — an exact count identity between two
/// independent derivations of spec §6.2's population over one fixed sweep,
/// not a per-seed property and not a search for an instance.
///
/// **This tag was MISSING when the test landed in Task 1**, and
/// `cli/tests/claim_shape.rs` is a default-deny workspace lint, so the branch
/// was red on `make gate` from that commit until Task 2 found it. Recorded
/// rather than quietly fixed: a crate-scoped green
/// (`cargo test -p hornvale-worldgen`) cannot see this lint, because the
/// enforcement tests live in `cli/`.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn no_settlement_in_the_readout_sits_outside_the_settleable_land_population() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let mut settleable_only_total: u64 = 0;
    for seed in 1..=30u64 {
        let world = world_of(seed, &wc);
        let terrain = terrain_of(&world).expect("terrain reconstructs");
        let climate = climate_from(&world, &terrain).expect("climate reconstructs");
        let geo = terrain.geosphere();
        let capacity = hornvale_demography::carrying_capacity(
            geo,
            &hornvale_worldgen::carrying_inputs_of(geo, &terrain, &climate),
        );
        let is_settleable = |cell: CellId| !terrain.is_ocean(cell) && capacity.at(cell) > 0.0;
        let report = demography_report_from(&world, &wc, &terrain, &climate)
            .expect("demography report reconstructs");
        settleable_only_total += report
            .stack_settlements
            .iter()
            .filter(|s| is_settleable(s.cell))
            .count() as u64;
    }

    let rows = exposure_rows(1..=30);
    let readout_pooled_total: u64 = rows
        .iter()
        .filter(|r| r.people == "pooled")
        .map(|r| r.settlements)
        .sum();

    assert_eq!(
        readout_pooled_total, settleable_only_total,
        "the readout's pooled settlement total ({readout_pooled_total}) does not \
         equal the independently-counted settleable-land-only settlement total \
         ({settleable_only_total}) — a settlement outside spec §6.2's population \
         (settleable land) has entered the readout"
    );
}

/// The mask's identity element is an IEEE-754 no-op at both of its
/// application points. Direction: this catches the `NONE` path computing
/// anything other than the formula it replaced; it cannot catch the
/// arithmetic being wrong in a way the ORIGINAL formula was also wrong in.
///
/// Bit-level, not approximate: `to_bits()` equality, because "close enough"
/// is exactly the class of drift the determinism contract forbids.
///
/// **Deviation from the task brief's draft, and why the draft could not
/// work.** The brief compared `per_species_suitability(…, ChannelMask::NONE)`
/// against a `suitability_fields_unmasked(…)` sibling. After the threading
/// there IS no unmasked sibling: `per_species_suitability` *delegates* to
/// `per_species_suitability_masked(…, NONE)`, so a comparison of the two is a
/// comparison of one code path against itself — VACUOUS, and green no matter
/// what the mask does. The same objection kills the equivalent comparison at
/// the report level (`demography_report_from` likewise delegates). A
/// self-comparison is precisely the evidence shape this project has been
/// burned by before, so it is not shipped here.
///
/// What is shipped instead is an INDEPENDENT recomputation. Each of the two
/// application points has exactly one pre-mask formula, and the test writes
/// that formula out again, by hand, from the terrain — then asserts the
/// shipped `NONE` path agrees bit-for-bit over every cell of three worlds:
///
/// - `hostility` — `carrying_inputs_of` (which is `carrying_inputs_at` at
///   `NONE`) must equal `terrain.unrest_at(cell).clamp(0.0, 1.0)`.
/// - `MINERAL` supply — `mineral_supply_field` (which is
///   `mineral_supply_field_masked` at `NONE`) must equal
///   `0.0` at sea and `terrain.prospectivity_at(c) * scale` on land.
///
/// **The CHAIN-level identity is carried by a different, older guard**, and
/// deliberately: `repose_exposure_readout_matches_the_committed_fixture`
/// above re-renders 26,146 settlements over 30 seeds through
/// [`demography_report_from_masked`] at [`ChannelMask::NONE`] — and that
/// fixture was authored in Task 1, before any mask existed. If any rung of
/// the four-deep threading delegated with something other than the identity,
/// or dropped the mask, that fixture drifts.
///
/// **It goes through the MASKED entry point, and that makes the guard
/// STRONGER than the un-masked wording it replaced (corrected fix round 2,
/// 2026-08-12).** `exposure_rows` now delegates to
/// `exposure_rows_masked(seeds, ChannelMask::NONE)`, so the fixture exercises
/// the mask-carrying code path end to end rather than a parallel unmasked
/// one. A guard that ran the OLD path could only prove the old path still
/// works; this one proves the path every arm uses is byte-identical to
/// pre-mask reality at the identity. A pre-existing golden authored against
/// the pre-mask code, re-rendered through the post-mask code, is a stronger
/// statement about the whole pipeline than any assertion this test could
/// make about it, and it costs nothing extra.
///
/// claim: structural(seed: [1, 42, 30]) — bit-identity of two arithmetics
/// over three named worlds, every cell of each. Three seeds, not thirty:
/// a bit-identity that holds on every cell of three whole globes and fails on
/// a fourth would be a different defect (a seed-dependent code path) than
/// anything this seam can express.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn channel_mask_none_is_bit_identical_to_the_unmasked_path() {
    let wc = WorldComponents::assemble().expect("components assemble");
    for seed in [1u64, 42, 30] {
        let world = world_of(seed, &wc);
        let terrain = terrain_of(&world).expect("terrain reconstructs");
        let climate = climate_from(&world, &terrain).expect("climate reconstructs");
        let geo = terrain.geosphere();

        // Application point 1: the unrest hostility penalty.
        let inputs = hornvale_worldgen::carrying_inputs_of(geo, &terrain, &climate);
        for cell in geo.cells() {
            let expected = terrain.unrest_at(cell).clamp(0.0, 1.0);
            assert_eq!(
                inputs.get(cell).hostility.to_bits(),
                expected.to_bits(),
                "seed {seed}, cell {cell:?}: hostility at ChannelMask::NONE diverged \
                 from the formula it replaced"
            );
        }

        // Application point 2: the unrest term inside mineral prospectivity.
        //
        // TWO SCALES, and the non-unit one is the load-bearing case (fix
        // round 2, 2026-08-12). `x * 1.0` is an exact IEEE-754 no-op, so a
        // single pass at `scale = 1.0` — which is what
        // `MINERAL_SUPPLY_SCALE` happens to be today — cannot distinguish
        // the shipped `prospectivity * scale` from a path that dropped,
        // reordered or misapplied `scale` entirely. `2.5` is an ordinary
        // non-power-of-two multiplier chosen to make the operand actually
        // participate; the shipped constant's own value is checked by the
        // `1.0` pass.
        for scale in [1.0_f64, 2.5] {
            let mineral = hornvale_worldgen::mineral_supply_field(geo, &terrain, scale);
            for cell in geo.cells() {
                let expected = if terrain.is_ocean(cell) {
                    0.0
                } else {
                    terrain.prospectivity_at(cell) * scale
                };
                assert_eq!(
                    mineral.get(cell).to_bits(),
                    expected.to_bits(),
                    "seed {seed}, cell {cell:?}, scale {scale}: mineral supply at \
                 ChannelMask::NONE diverged from the formula it replaced"
                );
            }
        }
    }
}

/// One seed's settlement attractor cells under one channel mask, **one entry
/// per settlement, NOT deduplicated**. Per-seed (never pooled across seeds)
/// so that two different seeds' identical cell indices cannot cancel; the
/// caller derives the set and sums the per-seed differences.
///
/// **Returns a `Vec`, not a `BTreeSet`, and the difference is load-bearing
/// (fix round 3, 2026-08-12).** [`hornvale_demography::stack_condense`] can
/// place TWO settlements on ONE cell — `stack_condense.rs` asserts exactly
/// that case — so a set collapses them and a cell count is not a settlement
/// count in general. It happens to be 1:1 at BASELINE (26,146 attractor cells
/// against Task 1's 26,146 settlements), but that is a measured coincidence
/// of the baseline, not a property, and it was never established for an
/// arm's own output. Any claim about settlements being CREATED or DESTROYED
/// has to count this `Vec`; only claims about which cells are OCCUPIED may
/// use the set.
///
/// Takes an ALREADY-BUILT world/terrain/climate: the four arms differ only
/// in the mask, and the world they are read against is the same one — genesis
/// is by far the expensive half, so building it once per seed and taking
/// four reports off it costs a quarter of what four independent 30-seed
/// sweeps would, for byte-identical results (the report is pure over the
/// committed world; see [`demography_report_from`]'s doc).
fn attractor_cells_of(
    world: &hornvale_kernel::World,
    wc: &WorldComponents,
    terrain: &hornvale_terrain::GeneratedTerrain,
    climate: &hornvale_climate::GeneratedClimate,
    mask: ChannelMask,
) -> Vec<CellId> {
    demography_report_from_masked(world, wc, terrain, climate, mask)
        .expect("demography report reconstructs")
        .stack_settlements
        .iter()
        .map(|s| s.cell)
        .collect()
}

/// The counterfactual arm (spec §6.6, amended by plan §0.1/§0.2).
///
/// Direction: arms A and B are POSITIVE CONTROLS — they must MOVE siting, and
/// a green here means the harness can detect movement. Arm C is the null
/// under test. C alone would be an empty diff with no positive control, which
/// is exactly the evidence shape that has misled this project before.
///
/// **On the arm-C `include_str!` grep:** it is a coarse instrument. It sees
/// the WHOLE `domains/demography/src` directory (11 files today, enumerated
/// by reading the directory at test time rather than by a fixed list of
/// `include_str!` paths) and ten named worldgen functions, matches four
/// spellings, and would miss a soil term reaching siting through a
/// helper in a file it does not include, through a re-exported alias, or
/// through a value passed in from a caller that read the soil itself. It is
/// NOT a proof of absence. It is a tripwire on the specific wiring this
/// arm's null depends on, and the `assert!(move_a.vacated > 0)` /
/// `assert!(move_b.vacated > 0)` positive controls are what carry the real
/// evidential weight.
///
/// **Arm C runs FIRST in the body even though it is the last arm
/// logically.** It is a static scan costing microseconds, while the controls
/// cost a 30-seed sweep; and if the null is stale, the sweep's numbers are
/// being read against a claim that no longer holds, so there is nothing to
/// measure. Failing before the expensive half is the honest order.
///
/// # Dated measurement (2026-08-12, Task 2)
///
/// See the module doc's Task-2 section for the measured per-arm [`Movement`]
/// figures — `vacated` / `newly_occupied` / net, the calibration of this
/// instrument's sensitivity, which a later reader needs before judging any
/// future null it reports.
///
/// claim: readout(seed: 1..=30, off-gate heavy:) — reports how many
/// settlements each ablation moves over one fixed sweep, and asserts only
/// that the counts are non-zero (the positive controls). Not a rate: no
/// threshold on the counts is claimed, precisely because none was
/// preregistered and inventing one after unblinding would be a rescue.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn the_counterfactual_arms_separate_a_true_null_from_a_wiring_gap() {
    // ARM C, FIRST because it is free and because a stale null makes the
    // expensive half unreadable. Soil fertility has no application point in
    // the siting path, so the arm is a CONNECTIVITY assertion: no siting-path
    // code reads a soil order or a soil fertility. If this ever fails,
    // someone wired The Ground into siting and this probe's null is stale —
    // which is precisely the shelf life spec §6.6 declares.
    //
    // Clause 1: the demography domain, where the whole siting ARITHMETIC
    // lives (carrying capacity, the coexistence pack, both condensations).
    // The claim being made is "this domain does not know soil exists at
    // all", so the scan must cover the WHOLE domain.
    //
    // ENUMERATED AT RUNTIME, not by a list of `include_str!` paths (fix
    // round 2, 2026-08-12). The first encoding named four files —
    // carrying_capacity, coexist, condense, stack_condense — while claiming
    // the domain, leaving niche.rs (home of `ConditionNiche`, the natural
    // place a soil axis would be added), founder.rs, flow.rs, footprint.rs,
    // render.rs, byproducts.rs and lib.rs unscanned. A hand-written path
    // list cannot make a whole-domain claim, because a file added tomorrow
    // is not in it; reading the directory can.
    let demography_src = concat!(env!("CARGO_MANIFEST_DIR"), "/../../domains/demography/src");
    let sources = rust_sources_under(std::path::Path::new(demography_src));
    assert!(
        sources.len() >= 11,
        "expected the demography domain to have at least the 11 sources it \
         had when this clause was written, found {} — this fires when the \
         scan finds FEWER sources than the domain is known to have, which \
         almost always means it is pointed at the wrong directory rather \
         than that the domain shrank. Verify the path before raising this.",
        sources.len()
    );
    for (path, src) in &sources {
        for spelling in SOIL_SPELLINGS {
            assert!(
                !src.contains(spelling),
                "the demography source {path} now mentions `{spelling}` — The \
                 Ground has been wired into the siting arithmetic, and this \
                 probe's arm-C null is STALE. Re-take the reading and rewrite \
                 this assertion."
            );
        }
    }

    // Clause 2: the composition root's siting chain, function by function.
    // NOT whole-file: `windows/worldgen/src/lib.rs` is a ~13k-line god file
    // that ALREADY contains `classify_soil(` — inside `soil_of`, which Task 1
    // established is called from five places, NONE of them in the siting
    // path. A whole-file grep here is therefore red on arrival and proves
    // nothing; the arm has to name the functions whose output actually
    // reaches K.
    let worldgen = include_str!("../src/lib.rs");
    for signature in SITING_CHAIN {
        let body = body_of(worldgen, signature);
        for spelling in SOIL_SPELLINGS {
            assert!(
                !body.contains(spelling),
                "the siting-chain function `{signature}` now mentions \
                 `{spelling}` — The Ground has been wired into siting, and \
                 this probe's arm-C null is STALE. Re-take the reading and \
                 rewrite this assertion."
            );
        }
    }

    let wc = WorldComponents::assemble().expect("components assemble");
    let arm_a_mask = ChannelMask {
        hostility: true,
        ..ChannelMask::NONE
    };
    let arm_b_mask = ChannelMask {
        mineral_unrest: true,
        ..ChannelMask::NONE
    };

    // POST-HOC ADDITION (fix round 2, 2026-08-12), declared as such because
    // this campaign preregisters its measurements. The first three arms were
    // measured and read BEFORE this fourth mask existed; it was added after
    // seeing them. It strengthens the INSTRUMENT rather than rescuing a
    // prediction — the residual was previously a SUBTRACTION over two
    // separately-measured ablations, which silently assumes the channels are
    // additive, and this reads it directly instead — but the distinction
    // between "added to measure better" and "added to get a better number"
    // is exactly what preregistration exists to keep visible, so it is
    // stated here and in the module doc rather than left to be inferred.
    let combined_mask = ChannelMask {
        hostility: true,
        mineral_unrest: true,
        ..ChannelMask::NONE
    };

    let mut base_all = 0usize;
    let mut base_settleable = 0usize;
    let mut move_a = Movement::default();
    let mut move_b = Movement::default();
    let mut move_ab = Movement::default();
    // PER-SEED, because a pooled net of zero can hide +3 and -3 in different
    // worlds (fix round 3). Both are needed: cells answer "which ground is
    // occupied", settlements answer "were any created or destroyed".
    let mut per_seed_net_a: Vec<i64> = Vec::new();
    let mut per_seed_settlement_delta_a: Vec<i64> = Vec::new();
    for seed in 1..=30u64 {
        let world = world_of(seed, &wc);
        let terrain = terrain_of(&world).expect("terrain reconstructs");
        let climate = climate_from(&world, &terrain).expect("climate reconstructs");
        let geo = terrain.geosphere();

        // THE POPULATION UNDER MEASUREMENT, defined ONCE per seed from the
        // UNMASKED world and applied identically to every arm. Two reasons it
        // is not re-derived per arm: an arm moves carrying capacity, so an
        // arm-local predicate would move the population and the settlements
        // at the same time, and no count could then be attributed to either;
        // and this is the same `is_settleable` spec §6.2 fixes for the
        // exposure readout above, so the arms are calibrating the instrument
        // on the population that readout actually measures.
        let capacity = hornvale_demography::carrying_capacity(
            geo,
            &hornvale_worldgen::carrying_inputs_of(geo, &terrain, &climate),
        );
        let is_settleable = |cell: &CellId| !terrain.is_ocean(*cell) && capacity.at(*cell) > 0.0;

        // One entry per settlement; `cells_of` derives the occupied-cell set.
        let base_v = attractor_cells_of(&world, &wc, &terrain, &climate, ChannelMask::NONE);
        let a_v = attractor_cells_of(&world, &wc, &terrain, &climate, arm_a_mask);
        let b_v = attractor_cells_of(&world, &wc, &terrain, &climate, arm_b_mask);
        let ab_v = attractor_cells_of(&world, &wc, &terrain, &climate, combined_mask);

        // SETTLEMENT counts (not cell counts) on the settleable-land
        // population — the only quantity that can support a "created or
        // destroyed" claim. See [`attractor_cells_of`].
        let settlements_in = |v: &[CellId]| v.iter().filter(|c| is_settleable(c)).count() as i64;
        per_seed_settlement_delta_a.push(settlements_in(&a_v) - settlements_in(&base_v));

        let base: BTreeSet<CellId> = base_v.iter().copied().collect();
        let a: BTreeSet<CellId> = a_v.iter().copied().collect();
        let b: BTreeSet<CellId> = b_v.iter().copied().collect();
        let ab: BTreeSet<CellId> = ab_v.iter().copied().collect();

        base_all += base.len();
        base_settleable += base.iter().filter(|c| is_settleable(c)).count();
        per_seed_net_a.push(movement_of(&base, &a, &is_settleable).net());
        // SAME predicate on BOTH sides of every difference — the fix-round-2
        // lesson from this file's module doc, applied to a set difference
        // rather than a ratio: filtering one side only would count every
        // marine settlement as "moved".
        move_a += movement_of(&base, &a, &is_settleable);
        move_b += movement_of(&base, &b, &is_settleable);
        move_ab += movement_of(&base, &ab, &is_settleable);
    }
    let pct = |n: usize| 100.0 * n as f64 / base_settleable as f64;
    println!("REPOSE ARMS: base attractor cells {base_all} (settleable {base_settleable})");
    for (label, m) in [
        ("A hostility", move_a),
        ("B mineral", move_b),
        ("AB combined", move_ab),
    ] {
        println!(
            "REPOSE ARMS: arm {label:12} vacated {} ({:.2}% of base) | newly-occupied {} | \
             net {:+} | changed cells {}",
            m.vacated,
            pct(m.vacated),
            m.newly_occupied,
            m.net(),
            m.changed_cells()
        );
    }

    // ARM A IS **NOT** PURE RELOCATION — a claim this file made and this
    // block RETRACTED by measuring it (fix round 3, 2026-08-12).
    //
    // The module doc read arm A's POOLED `net == 0` as "moves settlements
    // without creating or destroying a single one". A pooled net cannot
    // support that: +3 in one world and -3 in another sums to zero and looks
    // identical to no change anywhere. Nor could a CELL net, since two
    // settlements can share a cell (see `attractor_cells_of`). So both were
    // checked PER SEED, as assertions, and both went RED. The numbers are
    // printed below and recorded in the module doc; the claim is withdrawn.
    //
    // NOTHING IS ASSERTED HERE NOW, deliberately. The honest guard would be a
    // threshold on how relocation-dominated the movement is, and no such
    // threshold was preregistered; inventing one after seeing the numbers is
    // the rescue this campaign forbids. The positive controls below are the
    // assertions, and they are unaffected — they only ever claimed that
    // movement exists.
    let pooled_net: i64 = per_seed_net_a.iter().sum();
    let nonzero = per_seed_net_a.iter().filter(|n| **n != 0).count();
    let span = |v: &[i64]| {
        (
            v.iter().copied().min().unwrap_or(0),
            v.iter().copied().max().unwrap_or(0),
        )
    };
    let (net_lo, net_hi) = span(&per_seed_net_a);
    let (d_lo, d_hi) = span(&per_seed_settlement_delta_a);
    println!(
        "REPOSE ARMS: arm A per-seed occupied-cell net: pooled {pooled_net:+}, but \
         {nonzero} of {} seeds NONZERO, range {net_lo:+}..{net_hi:+} - NOT pure relocation",
        per_seed_net_a.len()
    );
    println!("REPOSE ARMS: arm A per-seed occupied-cell nets: {per_seed_net_a:?}");
    println!(
        "REPOSE ARMS: arm A per-seed settleable SETTLEMENT-count delta: range \
         {d_lo:+}..{d_hi:+}: {per_seed_settlement_delta_a:?}"
    );

    // POSITIVE CONTROLS, asserted on the SETTLEABLE-LAND population — the one
    // the exposure readout above measures, and therefore the one arm C's null
    // is a null about. Movement among marine attractor cells would prove the
    // mask does something, but not that it does something where the effect
    // under investigation lives. If either of these is zero, the harness is
    // blind and arm C's null above means nothing.
    //
    // Asserted on `vacated` rather than on the symmetric difference the first
    // encoding used. That is a STRENGTHENING, not a change of intent:
    // `vacated > 0` implies `changed_cells > 0`, and `vacated` is the figure
    // this test tells a reader to quote (see [`Movement`]).
    assert!(
        move_a.vacated > 0,
        "arm A (hostility ablated) vacated NO settleable-land baseline \
         settlement cell across 30 seeds — the ablation harness cannot see \
         movement in the population spec §6.2 measures, so arm C proves nothing"
    );
    assert!(
        move_b.vacated > 0,
        "arm B (mineral unrest ablated) vacated NO settleable-land baseline \
         settlement cell across 30 seeds — the ablation harness cannot see \
         movement in the population spec §6.2 measures, so arm C proves nothing"
    );
}

/// One arm's movement against the baseline, **in units a reader can safely
/// divide by the baseline settlement count**.
///
/// **The distinction this type exists to enforce (fix round 2, 2026-08-12).**
/// The first encoding of the arms summed
/// `base.symmetric_difference(&arm).count()` and reported it as "settlements
/// moved", against a denominator of baseline SETTLEMENTS. Those are different
/// units. A settlement that RELOCATES contributes TWO cells to a symmetric
/// difference — the one it left and the one it took — so the figure is
/// inflated against that denominator, and the module doc's original
/// "3,036 of 26,146 = 11.6%" was a count of changed CELLS over a count of
/// SETTLEMENTS. **2× is the PURE-RELOCATION case, not an upper bound:** an
/// arm that also creates settlements inflates further, and arm B measures
/// 399/193 = 2.07×.
///
/// **Which figure to quote: `vacated`, as a share of the baseline.** It is
/// 1:1 with baseline settlements — 26,146 attractor cells against Task 1's
/// 26,146 settlements — which is a MEASURED property of the baseline, not a
/// general one: [`hornvale_demography::stack_condense`] can place two
/// settlements on one cell, so an ARM's own cell count needs its own check
/// (fix round 3 made it, per seed; see the module doc). It answers the
/// question the arms are asked: how much of the settlement pattern this
/// readout measures does the channel account for. `newly_occupied` and `net`
/// are reported because a channel can also create or destroy settlements
/// rather than only relocate them, and a reader cannot tell relocation from
/// creation without both.
#[derive(Debug, Clone, Copy, Default)]
struct Movement {
    /// Baseline attractor cells holding no settlement under the arm — 1:1
    /// with baseline settlements, so this is the share-quotable figure.
    vacated: usize,
    /// Cells holding a settlement under the arm but not at baseline.
    newly_occupied: usize,
}

impl Movement {
    /// Cells whose occupancy CHANGED either way. This is exactly what a
    /// symmetric difference counts — kept, named honestly, so the old figure
    /// remains comparable and nobody re-derives it by accident.
    fn changed_cells(self) -> usize {
        self.vacated + self.newly_occupied
    }

    /// Signed change in occupied cells. Zero POOLED does NOT mean pure
    /// relocation — arm A pools to zero while 29 of its 30 seeds are
    /// individually nonzero (module doc, fix round 3). Read it per seed.
    fn net(self) -> i64 {
        self.newly_occupied as i64 - self.vacated as i64
    }
}

impl std::ops::AddAssign for Movement {
    fn add_assign(&mut self, rhs: Self) {
        self.vacated += rhs.vacated;
        self.newly_occupied += rhs.newly_occupied;
    }
}

/// One seed's [`Movement`] of `arm` against `base`, both sides filtered by
/// the same `keep` predicate.
fn movement_of(
    base: &BTreeSet<CellId>,
    arm: &BTreeSet<CellId>,
    keep: &impl Fn(&CellId) -> bool,
) -> Movement {
    Movement {
        vacated: base.difference(arm).filter(|c| keep(c)).count(),
        newly_occupied: arm.difference(base).filter(|c| keep(c)).count(),
    }
}

/// The spellings arm C treats as "a soil term has reached here". Coarse by
/// construction — see [`the_counterfactual_arms_separate_a_true_null_from_a_wiring_gap`]'s
/// doc for what it cannot see.
const SOIL_SPELLINGS: [&str; 4] = [
    "classify_soil",
    "SoilOrder",
    "soil_of(",
    "terrain::fertility(",
];

/// Every function in `windows/worldgen/src/lib.rs` whose output reaches a
/// species' per-cell K, and therefore reaches settlement condensation: the
/// four rungs of The Repose's mask threading plus the supply/substrate fields
/// `per_species_suitability_masked` reads. Named by their exact signature
/// line so [`body_of`] fails loudly rather than silently scanning nothing if
/// one is renamed.
///
/// **THE LIST PINS BODIES, NOT ENTRY POINTS, and the difference is the whole
/// value of the clause (fix round 2, 2026-08-12).** [`body_of`] slices one
/// function's own text; it does not follow calls. So naming a DELEGATOR pins
/// a wrapper and leaves the arithmetic unscanned. `substrate_field` is
/// exactly that shape — a 16-line wrapper that builds the insolation field
/// and immediately hands off to `substrate_field_at`, which is where the
/// per-cell `Substrate` is actually assembled from terrain and climate, and
/// therefore the single most plausible place a future campaign would add a
/// soil axis. The first encoding named only the wrapper: someone could have
/// added a soil term to `Substrate` in `substrate_field_at`, and arm C would
/// have stayed green while its null silently rotted.
///
/// Swept the other eight entries for the same wrapper/body shape when this
/// was fixed. `substrate_field` was the only delegator: `forage_supply_field`,
/// `prey_supply_field`, `detritus_supply_field` and
/// `marine_forage_supply_field` each hold their own `CellMap::from_fn` body,
/// and the four mask rungs hold theirs. **Re-run that sweep if you add an
/// entry** — a name here is worth only the body it actually points at.
const SITING_CHAIN: [&str; 10] = [
    "pub(crate) fn demography_report_with_beta_from(",
    "pub fn per_species_suitability_masked(",
    "pub fn carrying_inputs_at(",
    "pub fn mineral_supply_field_masked(",
    "pub fn substrate_field(",
    "pub fn substrate_field_at(",
    "pub fn forage_supply_field(",
    "pub fn prey_supply_field(",
    "pub fn detritus_supply_field(",
    "pub fn marine_forage_supply_field(",
];

/// Every `.rs` source under `dir`, as `(display path, contents)`, sorted by
/// path so the scan order is deterministic. Recurses, so a domain that grows
/// a submodule directory is still covered whole.
fn rust_sources_under(dir: &std::path::Path) -> Vec<(String, String)> {
    let mut out = Vec::new();
    let entries = std::fs::read_dir(dir)
        .unwrap_or_else(|e| panic!("read {}: {e}", dir.display()))
        .collect::<Result<Vec<_>, _>>()
        .expect("read every directory entry");
    let mut paths: Vec<std::path::PathBuf> = entries.iter().map(|e| e.path()).collect();
    paths.sort();
    for path in paths {
        if path.is_dir() {
            out.extend(rust_sources_under(&path));
        } else if path.extension().is_some_and(|e| e == "rs") {
            let text =
                std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {path:?}: {e}"));
            out.push((path.display().to_string(), text));
        }
    }
    out
}

/// The source text of one top-level function in a rustfmt-formatted file:
/// from its signature line to the first line that is exactly `}` at column
/// zero, which is where rustfmt closes a top-level item and nowhere else
/// inside one. Panics if the signature is absent — a renamed function must
/// fail this probe loudly, not silently scan an empty string.
fn body_of(src: &str, signature: &str) -> String {
    let start = src
        .find(signature)
        .unwrap_or_else(|| panic!("siting-chain function `{signature}` not found in lib.rs"));
    let rest = &src[start..];
    let end = rest
        .find("\n}\n")
        .unwrap_or_else(|| panic!("no top-level close brace after `{signature}`"));
    rest[..end].to_string()
}

/// ATTRIBUTION (Task 2). Which channel carries the exposure gradient Task 1
/// measured — the question a movement COUNT cannot answer.
///
/// **Why this test exists and the arms test is not enough.** The arms above
/// count how many settlements MOVED under each ablation. A count is
/// direction-free: it says the harness can see the channel, not which way the
/// channel pushes, and certainly not which channel produces the rising
/// exposure ratio. Attributing on a movement count would be this project's
/// named top failure mode — the right measurement against the wrong claim.
/// So this readout re-takes Task 1's actual statistic (pooled exposure ratio
/// by band, decile 0 vs decile 9) under each ablation and reports how the
/// RISE FACTOR moves.
///
/// **What is and is not arm-invariant here (corrected fix round 2,
/// 2026-08-12; the earlier wording claimed a blanket byte-identity that is
/// not true).** `exposure_ratio` is `settlement_share / land_area_share`, and
/// the two halves behave differently under a mask:
///
/// - `land_area_share`'s denominator — total settleable land over the sweep —
///   IS byte-identical across arms, because [`exposure_rows_masked`] derives
///   the land tally, the deciles, the bands and the `is_settleable`
///   population from the UNMASKED `carrying_inputs_of`.
/// - `settlement_share`'s denominator is `total_settlements_of(people)`, and
///   that is **arm-dependent**: an arm may create or destroy settlements, not
///   only relocate them, so a people's sweep total can differ between arms.
///
/// The statistic reported below is immune to that anyway, because it is a
/// RATIO OF TWO RATIOS within a single arm: `rise = ratio(d9) / ratio(d0)`,
/// and both strata divide by the same per-arm `total_settlements_of(people)`,
/// so that denominator **cancels exactly**. What a rise factor cannot absorb
/// is settlements moving BETWEEN deciles — which is precisely the signal.
/// Read the rise factors, not the absolute `d0`/`d9` levels, when comparing
/// arms.
///
/// **This test asserts only that the instrument is not blind**, never a
/// direction or a magnitude. The measured numbers go in the module doc, where
/// a reader can weigh them; encoding a directional expectation here after
/// unblinding would be a rescue, and this campaign publishes what it finds.
///
/// **THE ARMS ARE NOT EQUALLY POWERED, and the printed table does not say so
/// — read the module doc's Task-2 section before drawing anything from the
/// `armB-mineral` rows.** Arm A's term is in `carrying_capacity` for every
/// kind. Arm B's channel is read by two roster kinds, one of which (`xorn`)
/// places no settlement at all and the other (`rust-monster`) 0.4% of the
/// population — so a small arm-B row is a fact about the roster, and this
/// readout CANNOT test the mineral hypothesis on the shipped roster. A row
/// near the baseline here is UNTESTED, never refuted.
///
/// **The COMBINED arm is a POST-HOC addition (fix round 2, 2026-08-12),
/// declared as such.** The first three arms were measured and read before it
/// existed. Without it the residual is a SUBTRACTION over two separately
/// measured ablations, which silently assumes the two channels are ADDITIVE —
/// an assumption this file warns against elsewhere and has no evidence for.
/// `{hostility, mineral_unrest}` severs unrest from siting through both wires
/// at once, converting the residual from an inference into a direct reading.
/// It strengthens the instrument rather than rescuing a prediction, but the
/// distinction between those two is exactly what preregistration exists to
/// keep visible, so it is stated rather than left to be inferred.
///
/// claim: readout(seed: 1..=30, off-gate heavy:) — reports the pooled
/// exposure-ratio rise factor per band under four masks over one fixed sweep,
/// and asserts only that each arm perturbs the settlement distribution.
/// `claim_shape` does NOT flag this test — it detects a literal seed-binding
/// loop and this one passes `1..=30` straight to a helper — so the tag is
/// here by choice. It is the readout carrying the most interpretive load in
/// the file, which is exactly the kind that should declare its quantifier.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn which_channel_carries_the_exposure_gradient() {
    let arm_a_mask = ChannelMask {
        hostility: true,
        ..ChannelMask::NONE
    };
    let arm_b_mask = ChannelMask {
        mineral_unrest: true,
        ..ChannelMask::NONE
    };
    let combined_mask = ChannelMask {
        hostility: true,
        mineral_unrest: true,
        ..ChannelMask::NONE
    };

    let arms: [(&str, Vec<ExposureRow>); 4] = [
        ("base", exposure_rows_masked(1..=30, ChannelMask::NONE)),
        ("armA-hostility", exposure_rows_masked(1..=30, arm_a_mask)),
        ("armB-mineral", exposure_rows_masked(1..=30, arm_b_mask)),
        (
            "armAB-combined",
            exposure_rows_masked(1..=30, combined_mask),
        ),
    ];

    let ratio_at = |rows: &[ExposureRow], band: &str, decile: usize| -> f64 {
        rows.iter()
            .find(|r| r.people == "pooled" && r.band == band && r.decile == decile)
            .map(|r| r.exposure_ratio)
            .unwrap_or(0.0)
    };
    for (label, rows) in &arms {
        for (band, _) in BANDS {
            let lo = ratio_at(rows, band, 0);
            let hi = ratio_at(rows, band, DECILES - 1);
            let rise = if lo > 0.0 { hi / lo } else { f64::NAN };
            println!(
                "REPOSE ATTRIBUTION: arm {label:14} band {band:10} d0 {lo:.4} \
                 d9 {hi:.4} rise x{rise:.3}"
            );
        }
    }

    // BLINDNESS GUARD, and nothing more: each arm must actually change the
    // pooled settlement distribution, or the table above is three copies of
    // one reading and says nothing about attribution. Direction and magnitude
    // are deliberately unasserted (see this test's doc).
    let base_settlements: Vec<u64> = arms[0]
        .1
        .iter()
        .filter(|r| r.people == "pooled")
        .map(|r| r.settlements)
        .collect();
    for (label, rows) in arms.iter().skip(1) {
        let arm_settlements: Vec<u64> = rows
            .iter()
            .filter(|r| r.people == "pooled")
            .map(|r| r.settlements)
            .collect();
        assert_ne!(
            arm_settlements, base_settlements,
            "arm {label} left the pooled settlement distribution untouched — \
             the attribution table is measuring one reading four times"
        );
    }
}

/// KNOWNNESS HAS A CONSUMER, AND THE CONSUMER CAN SEE IT (Task 7). Spec §7
/// names "knownness ships with no consumer and cannot be seen to be wrong" as
/// a risk; this is the assertion that closes it, and The Hollow is why it is
/// not left to the fixture alone.
///
/// **Direction: it catches a knownness column wired to a constant** — either
/// constant, and the pair is what makes that true. Both assertions run over
/// SETTLED strata only (`settlements > 0`), because a stratum with no
/// settlements reads 0.0 by the row-render guard whatever knownness does, and
/// including those would have let a constant-1.0 wiring through: the column
/// would still have varied (1.0 where settled, 0.0 where empty) and a bare
/// "not constant" test would have passed it. That was a real hole in this
/// test's first encoding, found by mutating the stock to 1.0 rather than by
/// reading it. It does NOT check that any
/// particular stratum's value is right, and it asserts no direction, no
/// magnitude and no cross-species ordering: the per-people spread it prints is
/// an OBSERVATION under spec §6.3's per-people requirement, never a tested
/// prediction. See `hornvale_worldgen::knownness`'s module doc for why this
/// campaign declines to preregister a cross-species memory claim even though
/// the axis it would run on is now live.
///
/// **What the numbers mean before anyone reads them.** The column is a
/// population-weighted mean over EVERY settlement in a stratum, and a
/// settlement whose cell carries no edifice contributes a 0 because there is
/// no mountain to remember. Only 1.56% of settleable-land settlements sit on
/// an edifice (measured, module doc), so a stratum mean of 0.01 does not mean
/// "everyone half-remembers"; it means a small, remembering minority inside a
/// large, mountainless majority. The right comparison is BETWEEN strata and
/// between peoples, never against 1.
///
/// **The per-people means this prints are NOT a memory comparison**, and the
/// module doc's Task-7 table is the reason: four of the five peoples read
/// exactly zero because they place no settlement on an edifice anywhere in
/// the sweep, not because they forgot. A reader who takes those zeros as
/// evidence about memory is reading a settlement-siting fact as a
/// transmission fact.
///
/// claim: readout(seed: 1..=30, off-gate heavy:) — reports the knownness
/// column's spread over one fixed sweep and asserts only that it varies. Not
/// a rate: no threshold on any value is claimed, because none was
/// preregistered and inventing one after unblinding would be a rescue.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn the_readout_can_see_a_people_remember_and_a_people_forget() {
    let rows = exposure_rows(1..=30);

    // The per-people observation (spec §6.3). Population-weighted over all of
    // a people's strata, which is the same statistic as the column itself,
    // pooled up one level.
    let mut peoples: Vec<&'static str> = rows.iter().map(|r| r.people).collect();
    peoples.dedup();
    for people in &peoples {
        let population: f64 = rows
            .iter()
            .filter(|r| r.people == *people)
            .map(|r| r.population)
            .sum();
        let weighted: f64 = rows
            .iter()
            .filter(|r| r.people == *people)
            .map(|r| r.knownness * r.population)
            .sum();
        let mean = if population > 0.0 {
            weighted / population
        } else {
            0.0
        };
        let highest = rows
            .iter()
            .filter(|r| r.people == *people)
            .map(|r| r.knownness)
            .fold(0.0_f64, f64::max);
        println!(
            "REPOSE KNOWNNESS: people {people:14} population {population:12.0} \
             mean knownness {mean:.8} highest stratum {highest:.8}"
        );
    }

    // SETTLED pooled strata only — see this test's doc for why an empty
    // stratum must not count on either side.
    let settled: Vec<f64> = rows
        .iter()
        .filter(|r| r.people == "pooled" && r.settlements > 0)
        .map(|r| r.knownness)
        .collect();
    let remembering = settled.iter().filter(|k| **k > 0.0).count();
    let highest = settled.iter().copied().fold(0.0_f64, f64::max);
    println!(
        "REPOSE KNOWNNESS: settled pooled strata {} — {remembering} remember something, \
         highest {highest:.8}",
        settled.len()
    );

    assert!(
        highest > 0.0,
        "not one of the {} settled pooled strata remembers an eruption — knownness reaches \
         the readout as a constant zero and cannot be seen to be wrong",
        settled.len()
    );
    assert!(
        remembering < settled.len(),
        "all {} settled pooled strata remember something — nobody is forgetting anywhere, \
         which is what a knownness welded to 1 would look like. If the world really has \
         changed this much, this is a FINDING and not a defect: re-read it before touching \
         the assertion.",
        settled.len()
    );
}

/// Rewrites the committed fixture. Deliberately NOT part of any gate: it
/// would silently rewrite the artifact the drift check above exists to
/// check.
#[test]
#[ignore = "regenerates the committed repose exposure fixture; run by hand - the drift check above is the gate"]
fn rewrite_repose_exposure_fixture() {
    std::fs::write(
        concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/repose-exposure.csv"
        ),
        render_repose_exposure(1..=30),
    )
    .expect("write repose-exposure.csv fixture");
}
