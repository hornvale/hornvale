-- The pin-provenance report (spec §3): every SQL-expressible pinned
-- calibration constant, recomputed from the committed fixture and compared
-- to the value pinned in windows/lab/tests/calibration.rs. Duplication is
-- deliberate — an independent second path from fixture to pin, written
-- against the raw CSV-backed views rather than the Rust MetricValue model.
-- When a pin re-pins in Rust (a re-baseline), update the literal here IN THE
-- SAME COMMIT — the golden-pins mismatch is the tripwire that catches a
-- forgotten half of a re-pin. (Catch-up 2026-07-16, regen 945f62b: this file
-- had silently missed every calibration.rs re-pin since its creation —
-- census-check was not being run at re-pin time. All literals below were
-- resynced to calibration.rs in the epoch-v4 re-pin commit.)
-- (Resync 2026-07-20, The Demesne / BIO-35 Stage 1 local regen on lefford:
-- the genesis-changing per-axis supply moved the census; all 16 drifted
-- literals below were re-synced to calibration.rs in this same commit.)
-- (Resync 2026-07-21, The Living Community epoch (history-first placement)
-- regen on lefford (0063): history-first placement re-placed every world,
-- moving the census again; all drifted literals below were re-synced to
-- calibration.rs in this same commit. Two notable movements: kobold
-- present-rows RISE sharply (123 -> 772 — history-first placement seats
-- kobolds in far MORE worlds, opposite the Demesne trend) and the mean
-- name-collision-rate RISES (0.064 -> 0.179 — reversing the prior regen's
-- name-dedup improvement). Two structural null-control invariants that used
-- to be exact zero (cult-form TVD, pantheon-size mean-diff) are now tiny
-- nonzero measurements: history-first placement is path-dependent, so the
-- goblin-solo/goblin-twin-solo builds are no longer perfect clones on every
-- seed — still well inside the directional sampling bound, so this is a
-- re-pinned measurement, not a broken invariant.)
-- (Resync 2026-07-22, The Sundering (moving-sea epoch) regen on lefford
-- (0063): the moving-sea epoch re-placed every world again; all drifted
-- literals below were re-synced to calibration.rs in this same commit.)
-- (Resync 2026-07-27, The Vacancy regen on lefford at the merged SHA (0063):
-- the roster grew 16 -> 29 kinds and ANIMAL_PREY gained a real supply field.
-- Measured identically before and after main's x86-64-v2 codegen bump, so the
-- roster is the sole cause of every move below. Re-synced FROM calibration.rs,
-- which stays primary.)
-- (Resync 2026-07-26, The Tumult (predation epoch) regen on lefford (0063):
-- the deep-history bake now resolves conflict as predation — a community
-- covets a richer neighbour's site, wins the fight and seizes it — so the
-- surviving settlement roster differs on many seeds; all 13 drifted literals
-- below were re-synced to calibration.rs in this same commit. Two notable
-- movements: the mean name-collision-rate FALLS (0.183 -> 0.172, reversing
-- the last two regens' rise — predation prunes the roster rather than
-- displacing it, so fewer names are drawn per world), and the kobold mean
-- name length jumps far more than goblin's (12.69 -> 13.09 vs 13.38 ->
-- 13.41), the same reseating that INVERTED the directional claim in
-- kobold_flagships_are_less_coastal_than_goblin_flagships — a falsified
-- preregistered hypothesis, deliberately left failing for owner review
-- rather than flipped to match the data. RESOLVED 2026-07-26: that test is
-- RETIRED, not flipped — the shipped model predicts a ZERO difference on
-- that axis, since the bake is niche-blind end to end; the replacement
-- preregisters the re-selection rate itself against `threat_response` in
-- windows/lab/tests/disposition_calibration.rs, which is a live-worldgen
-- battery with no census column and so has no second path here either.)
-- (Resync 2026-07-28, The Wearing regen on lefford (0063), regen commit
-- f32d6ce2: the campaign shortened names — the drawn settlement stem retired,
-- name shape became a per-culture drawn distribution, the nucleus became a
-- template set, and reduction became position-conditioned. ONLY naming and
-- lexicon columns moved (21 of ~340; settlement placement, pantheon structure,
-- terrain and climate are byte-identical), so only the naming literals below
-- drifted and every placement/pantheon literal is untouched. Three new pins
-- are ADDED for the campaign's three new metrics — name-syllables-goblin,
-- name-syllables-kobold and name-transparency — which had no second path
-- here before because they had no census to be measured against.
-- Two notable movements: the mean name-collision-rate RISES 3.4x
-- (0.186 -> 0.627) and the zero-collision world count goes to ZERO (39 -> 0),
-- which is SANCTIONED, not a defect — decision 0024 holds that uniqueness is
-- a reference-time property and forbids fixing it with drawn entropy, and the
-- retired stem was exactly such a fix; the relief Task 10 shipped is
-- render-time qualification. And both mean name lengths fall by a quarter to
-- two fifths (goblin 13.397 -> 9.563, kobold 13.212 -> 7.639), which is the
-- campaign's PRIMARY preregistered claim landing: the medians, 9.33 and 7.40,
-- are inside the mean-name-length metric's own declared buckets, which stop
-- at 10. Re-synced FROM calibration.rs, which stays primary.)
--
-- Counts and exact structural zeroes compare with `computed = pinned`;
-- quantized means/SMDs compare with `abs(computed - pinned) < 1e-6` (the
-- fixture's floats passed `quantize`, 8 significant digits, at the
-- ledger-commit boundary — see CLAUDE.md's Determinism section).
--
-- NOT translated here (Rust-only; the source test is still the first path,
-- this file just doesn't add a second one for these):
--   * band_count_matches_the_known_function_of_rotation,
--     flagship_subsistence_matches_biome_and_coastal_columns,
--     pantheon_verticality_matches_stratification,
--     head_deity_is_eternal_exactly_when_tidally_locked,
--     kobold_structures_never_enslave_and_top_out_with_elders,
--     the_slave_rung_is_an_exact_function_of_rank_surplus_and_scale
--     — per-row structural invariants (day-length thresholds, the
--     biome+coastal -> subsistence mapping, a role-string top-rung check)
--     with no single re-pinned literal to duplicate; SQL would just restate
--     the same predicate the Rust loop already checks every commit, not add
--     an independent path to a MEASURED value.
--   * obliquity_range_is_wider_on_moonless_worlds
--     — a directional-only claim (a strict inequality between two computed
--     means), no exact literal pinned.
--     (kobold_flagships_are_less_coastal_than_goblin_flagships stood here
--     too until it was retired on 2026-07-26 — see the header note above.)
--   * phonotactic_validity_is_true_for_every_generated_name,
--     epithet_honorific_is_true_for_goblin_and_false_for_kobold,
--     name_gloss_true_is_100_percent_row_by_row,
--     lexicon_is_regular_and_exposure_sound_for_both_species
--     — boolean invariants that hold 100%/0% by construction (grammar,
--     morph_options, name-gloss truthfulness, exposure soundness); no
--     drifting literal, and re-deriving "is this name valid" needs the
--     phonotactic/lexicon machinery itself, not just the fixture.
--   * census_fixture_matches_live_run — the `#[ignore]`d heavy live-rerun
--     guard, not a calibration constant.
--   * null_control_blind_attribution_is_at_chance's `picks_twin` — no longer
--     implied by `decided = 0` (The Living Community epoch re-pin made
--     `decided` a nonzero 64; see below). Still not translated here: the
--     tiered `pick_second` preference (lunar, then cyclic-share, then size)
--     that decides WHICH side a decided pair picks isn't reimplemented in
--     SQL, only the equality test that determines indistinguishable-vs-
--     decided; `picks_twin` stays a Rust-only pin.
--   * null_control_distributions_are_within_the_sampling_bound's directional
--     bounds (TVD < 0.15, |SMD| < 0.2) — sampling-theory margins, not
--     re-pinned measurements; only the file's exact STRUCTURAL zero pins are
--     translated below.

WITH agg AS (
  -- Single-pass conditional aggregation over "the-census" (1000 seeds).
  SELECT
    -- a_frozen_sky_never_heads_a_cyclic_pantheon.
    -- PER-PEOPLE readings (The Presiding, SKY-25): the world-level
    -- "belief-kind" column is retired — it recorded whichever people the
    -- component registry iterated first, which is a fact about a loop, not
    -- about a world. These count one reading per (seed, people) and are NOT
    -- comparable to the pre-2026-07-17 world-level literals.
    (count(*) FILTER (WHERE "tidally-locked" = true AND "belief-kind-bugbear" = 'eternal')
     + count(*) FILTER (WHERE "tidally-locked" = true AND "belief-kind-goblin" = 'eternal')
     + count(*) FILTER (WHERE "tidally-locked" = true AND "belief-kind-hobgoblin" = 'eternal')
     + count(*) FILTER (WHERE "tidally-locked" = true AND "belief-kind-kobold" = 'eternal'))
      AS locked_eternal,
    (count(*) FILTER (WHERE "tidally-locked" = true AND "belief-kind-bugbear" = 'ambient')
     + count(*) FILTER (WHERE "tidally-locked" = true AND "belief-kind-goblin" = 'ambient')
     + count(*) FILTER (WHERE "tidally-locked" = true AND "belief-kind-hobgoblin" = 'ambient')
     + count(*) FILTER (WHERE "tidally-locked" = true AND "belief-kind-kobold" = 'ambient'))
      AS locked_ambient,
    (count(*) FILTER (WHERE "tidally-locked" = false AND "belief-kind-bugbear" = 'eternal')
     + count(*) FILTER (WHERE "tidally-locked" = false AND "belief-kind-goblin" = 'eternal')
     + count(*) FILTER (WHERE "tidally-locked" = false AND "belief-kind-hobgoblin" = 'eternal')
     + count(*) FILTER (WHERE "tidally-locked" = false AND "belief-kind-kobold" = 'eternal'))
      AS spinning_eternal_exceptions,
    -- goblin_flagship_coastal_split_is_pinned
    count(*) FILTER (WHERE "flagship-coastal" = true) AS flagship_coastal,
    count(*) FILTER (WHERE "flagship-coastal" = false) AS flagship_inland,
    -- goblin_heads_are_always_solar_and_mooned_kobold_heads_always_lunar
    -- (the moonless-split calibration row; the "always solar/lunar"
    -- invariants above it have no separate literal to duplicate)
    count(*) FILTER (
      WHERE refusal IS NULL AND "head-deity-domain-kobold" IS NOT NULL
        AND "tidally-locked" = false AND "moons-admitted" = '0'
        AND "head-deity-domain-kobold" = 'solar'
    ) AS moonless_solar,
    count(*) FILTER (
      WHERE refusal IS NULL AND "head-deity-domain-kobold" IS NOT NULL
        AND "tidally-locked" = false AND "moons-admitted" = '0'
        AND "head-deity-domain-kobold" = 'lunar'
    ) AS moonless_lunar,
    -- blind_attribution_beats_chance_decisively
    count(*) FILTER (WHERE "blind-attribution-correct" = true) AS blind_correct,
    count(*) FILTER (WHERE "blind-attribution-correct" IS NOT NULL) AS blind_total,
    -- the mooned+spinning "correct_mooned == total_mooned" perfect-
    -- attribution claim, recast as a mismatch count (pinned 0): among mooned,
    -- spinning pairs, no incorrect attribution.
    count(*) FILTER (
      WHERE "blind-attribution-correct" = false AND "moons-admitted" <> '0'
        AND "tidally-locked" = false
    ) AS mooned_spinning_mismatches,
    -- name_collision_rate_is_measured_and_pinned
    count(*) FILTER (WHERE "name-collision-rate" = 0.0) AS collision_zero,
    count(*) FILTER (WHERE "name-collision-rate" > 0.0) AS collision_nonzero,
    count(*) FILTER (WHERE "name-collision-rate" IS NULL) AS collision_absent,
    avg("name-collision-rate") AS collision_mean,
    -- name_length_distributions_are_measured_and_pinned
    count(*) FILTER (WHERE "name-length-goblin" IS NOT NULL) AS goblin_len_present,
    avg("name-length-goblin") AS goblin_len_mean,
    count(*) FILTER (WHERE "name-length-kobold" IS NOT NULL) AS kobold_len_present,
    avg("name-length-kobold") AS kobold_len_mean,
    -- name_syllable_distributions_are_measured_and_pinned (The Wearing, new)
    count(*) FILTER (WHERE "name-syllables-goblin" IS NOT NULL) AS goblin_syl_present,
    avg("name-syllables-goblin") AS goblin_syl_mean,
    count(*) FILTER (WHERE "name-syllables-kobold" IS NOT NULL) AS kobold_syl_present,
    avg("name-syllables-kobold") AS kobold_syl_mean,
    -- name_transparency_is_measured_and_pinned (The Wearing, new). The min and
    -- max are pinned alongside the mean because the SPREAD is the finding: a
    -- mean of 0.827 with every world reading 0.827 would be the same
    -- uniformity defect the campaign removed, wearing a different number.
    count(*) FILTER (WHERE "name-transparency" IS NOT NULL) AS transparency_present,
    avg("name-transparency") AS transparency_mean,
    min("name-transparency") AS transparency_min,
    max("name-transparency") AS transparency_max,
    -- goblin_hue_depth_exceeds_kobold_hue_depth (a structural constant, so
    -- its mean over present rows equals the per-row pinned value exactly)
    avg("hue-depth-goblin") FILTER (
      WHERE "hue-depth-goblin" IS NOT NULL AND "hue-depth-kobold" IS NOT NULL
    ) AS goblin_hue_mean,
    avg("hue-depth-kobold") FILTER (
      WHERE "hue-depth-goblin" IS NOT NULL AND "hue-depth-kobold" IS NOT NULL
    ) AS kobold_hue_mean,
    -- fixture size sanity (implicit in several "present + absent == 1000"
    -- row-count assertions across this file)
    count(*) AS row_count
  FROM "the-census"
),
meeting_pairs AS (
  -- Per-seed (goblin-solo, goblin-twin-solo) signature pairs from
  -- "census-of-the-meeting" — the same three columns
  -- `collect_sig`/`pick_second` compare in null_control_blind_attribution_
  -- is_at_chance, joined on seed instead of a Rust BTreeMap.
  SELECT
    (gs.domain = 'lunar') AS gs_lunar, (ts.domain = 'lunar') AS ts_lunar,
    gs.cyclic AS gs_cyclic, ts.cyclic AS ts_cyclic,
    gs.size AS gs_size, ts.size AS ts_size
  FROM (
    SELECT seed, "head-deity-domain-goblin" AS domain,
           "pantheon-cyclic-share-goblin" AS cyclic,
           "pantheon-size-goblin" AS size
    FROM "census-of-the-meeting"
    WHERE pin_set = 'goblin-solo'
      AND "head-deity-domain-goblin" IS NOT NULL
      AND "pantheon-cyclic-share-goblin" IS NOT NULL
      AND "pantheon-size-goblin" IS NOT NULL
  ) AS gs
  JOIN (
    SELECT seed, "head-deity-domain-goblin-twin" AS domain,
           "pantheon-cyclic-share-goblin-twin" AS cyclic,
           "pantheon-size-goblin-twin" AS size
    FROM "census-of-the-meeting"
    WHERE pin_set = 'goblin-twin-solo'
      AND "head-deity-domain-goblin-twin" IS NOT NULL
      AND "pantheon-cyclic-share-goblin-twin" IS NOT NULL
      AND "pantheon-size-goblin-twin" IS NOT NULL
  ) AS ts USING (seed)
),
meeting_agg AS (
  -- A pair is "indistinguishable" (pick_second returns None) exactly when
  -- its lunar-domain status, cyclic-share, and size all agree — the same
  -- condition the three `if`/`match` tiers in `pick_second` fall through on.
  SELECT
    count(*) AS total_pairs,
    count(*) FILTER (
      WHERE gs_lunar = ts_lunar AND gs_cyclic = ts_cyclic AND gs_size = ts_size
    ) AS indistinguishable
  FROM meeting_pairs
),
head_domain_diff AS (
  -- null_control_distributions_are_within_the_sampling_bound's exact
  -- head-domain TVD = 0 pin, recast as a per-category count-diff sum (0 iff
  -- TVD = 0, since both pin sets have equal size).
  SELECT coalesce(sum(abs(coalesce(a.n, 0) - coalesce(b.n, 0))), 0) AS diff
  FROM (
    SELECT "head-deity-domain-goblin" AS cat, count(*) AS n
    FROM "census-of-the-meeting"
    WHERE pin_set = 'goblin-solo' AND "head-deity-domain-goblin" IS NOT NULL
    GROUP BY 1
  ) a
  FULL OUTER JOIN (
    SELECT "head-deity-domain-goblin-twin" AS cat, count(*) AS n
    FROM "census-of-the-meeting"
    WHERE pin_set = 'goblin-twin-solo' AND "head-deity-domain-goblin-twin" IS NOT NULL
    GROUP BY 1
  ) b ON a.cat = b.cat
),
cult_form_diff AS (
  -- Same treatment for the exact cult-form TVD = 0 pin.
  SELECT coalesce(sum(abs(coalesce(a.n, 0) - coalesce(b.n, 0))), 0) AS diff
  FROM (
    SELECT "cult-form-goblin" AS cat, count(*) AS n
    FROM "census-of-the-meeting"
    WHERE pin_set = 'goblin-solo' AND "cult-form-goblin" IS NOT NULL
    GROUP BY 1
  ) a
  FULL OUTER JOIN (
    SELECT "cult-form-goblin-twin" AS cat, count(*) AS n
    FROM "census-of-the-meeting"
    WHERE pin_set = 'goblin-twin-solo' AND "cult-form-goblin-twin" IS NOT NULL
    GROUP BY 1
  ) b ON a.cat = b.cat
),
pantheon_size_stats AS (
  -- The exact pantheon-size SMD = 0 pin, recast as a mean-difference proxy:
  -- SMD's numerator is the mean gap, and a nonzero pooled SD (true here,
  -- pantheon size varies) makes mean-diff = 0 iff SMD = 0.
  SELECT
    (SELECT avg("pantheon-size-goblin") FROM "census-of-the-meeting"
       WHERE pin_set = 'goblin-solo') AS mean_a,
    (SELECT avg("pantheon-size-goblin-twin") FROM "census-of-the-meeting"
       WHERE pin_set = 'goblin-twin-solo') AS mean_b
),
namelen_stats AS (
  -- null_control_name_length_smd_is_pinned's full standardized-mean-
  -- difference recomputation (the one MEETING pin that is a genuine
  -- nonzero measurement, not a structural zero): pooled population SD
  -- across both solo builds, matching `std_mean_diff`'s `/n` (not
  -- Bessel-corrected) variance.
  SELECT
    (SELECT avg("name-length-goblin") FROM "census-of-the-meeting"
       WHERE pin_set = 'goblin-solo') AS mean_a,
    (SELECT avg("name-length-goblin-twin") FROM "census-of-the-meeting"
       WHERE pin_set = 'goblin-twin-solo') AS mean_b,
    (SELECT var_pop("name-length-goblin") FROM "census-of-the-meeting"
       WHERE pin_set = 'goblin-solo') AS var_a,
    (SELECT var_pop("name-length-goblin-twin") FROM "census-of-the-meeting"
       WHERE pin_set = 'goblin-twin-solo') AS var_b
),
checks AS (
  SELECT 'locked-eternal per-people head count (calibration.rs::a_frozen_sky_never_heads_a_cyclic_pantheon)' AS pin,
         CAST(locked_eternal AS DOUBLE) AS computed, 149.0 AS pinned, locked_eternal = 149 AS ok FROM agg
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): 41 -> 40.
  SELECT 'locked-ambient per-people head count (calibration.rs::a_frozen_sky_never_heads_a_cyclic_pantheon)',
         CAST(locked_ambient AS DOUBLE), 39.0, locked_ambient = 39 FROM agg
  UNION ALL
  SELECT 'spinning-yet-eternal per-people head count (calibration.rs::a_frozen_sky_never_heads_a_cyclic_pantheon)',
         CAST(spinning_eternal_exceptions AS DOUBLE), 11.0, spinning_eternal_exceptions = 11 FROM agg
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): 536 -> 535.
  -- The Tumult (predation) re-pin, 0063: 535 -> 531.
  SELECT 'goblin flagship coastal count (calibration.rs::goblin_flagship_coastal_split_is_pinned)',
         CAST(flagship_coastal AS DOUBLE), 552.0, flagship_coastal = 552 FROM agg
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): 235 -> 234.
  -- The Tumult (predation) re-pin, 0063: 234 -> 238.
  SELECT 'goblin flagship inland count (calibration.rs::goblin_flagship_coastal_split_is_pinned)',
         CAST(flagship_inland AS DOUBLE), 214.0, flagship_inland = 214 FROM agg
  UNION ALL
  SELECT 'moonless-solar kobold head count (calibration.rs::goblin_heads_are_always_solar_and_mooned_kobold_heads_always_lunar)',
         CAST(moonless_solar AS DOUBLE), 33.0, moonless_solar = 33 FROM agg
  UNION ALL
  SELECT 'moonless-lunar kobold head count (calibration.rs::goblin_heads_are_always_solar_and_mooned_kobold_heads_always_lunar)',
         CAST(moonless_lunar AS DOUBLE), 61.0, moonless_lunar = 61 FROM agg
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): 703 -> 700.
  -- The Tumult (predation) re-pin, 0063: 700 -> 702 (the 768-row pool is
  -- unchanged, so accuracy rises 0.911 -> 0.914).
  SELECT 'blind-attribution correct count (calibration.rs::blind_attribution_beats_chance_decisively)',
         CAST(blind_correct AS DOUBLE), 695.0, blind_correct = 695 FROM agg
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): 771 -> 768.
  SELECT 'blind-attribution total count (calibration.rs::blind_attribution_beats_chance_decisively)',
         CAST(blind_total AS DOUBLE), 759.0, blind_total = 759 FROM agg
  UNION ALL
  SELECT 'mooned+spinning blind-attribution mismatches (calibration.rs::blind_attribution_beats_chance_decisively, correct_mooned == total_mooned)',
         CAST(mooned_spinning_mismatches AS DOUBLE), 0.0, mooned_spinning_mismatches = 0 FROM agg
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): 50 -> 48.
  -- The Tumult (predation) re-pin, 0063: 48 -> 43.
  -- The Wearing re-pin, 0063: 39 -> 0. Not one world in a thousand now draws
  -- a wholly duplicate-free name set. Sanctioned; see the header note.
  SELECT 'zero-collision world count (calibration.rs::name_collision_rate_is_measured_and_pinned)',
         CAST(collision_zero AS DOUBLE), 0.0, collision_zero = 0 FROM agg
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): 723 -> 722.
  -- The Tumult (predation) re-pin, 0063: 722 -> 727.
  -- The Wearing re-pin, 0063: 731 -> 770 (every previously zero-collision
  -- world moved into this bucket; the absent count below is unmoved).
  SELECT 'nonzero-collision world count (calibration.rs::name_collision_rate_is_measured_and_pinned)',
         CAST(collision_nonzero AS DOUBLE), 770.0, collision_nonzero = 770 FROM agg
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): 227 -> 230.
  SELECT 'absent name-collision-rate count (calibration.rs::name_collision_rate_is_measured_and_pinned)',
         CAST(collision_absent AS DOUBLE), 230.0, collision_absent = 230 FROM agg
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): 0.178_726_790_236_740_12 ->
  -- 0.183_235_100_516_883.
  -- The Tumult (predation) re-pin, 0063: 0.183_235_100_516_883 ->
  -- 0.185_804_141_557_143 (predation prunes the roster, so fewer names
  -- are drawn per world and the rate FALLS).
  -- The Wearing re-pin, 0063: 0.185_804_141_557_143 -> 0.627_418_410_740_260_4,
  -- a 3.4x rise and by far the largest single movement this pin has recorded.
  SELECT 'mean name-collision-rate (calibration.rs::name_collision_rate_is_measured_and_pinned)',
         collision_mean, 0.627_418_410_740_260_4, abs(collision_mean - 0.627_418_410_740_260_4) < 1e-6 FROM agg
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): 771 -> 769.
  -- The Wearing, 0063: UNMOVED at 766 — naming decides no placement, so both
  -- name-length means below are measured over exactly the prior population.
  SELECT 'goblin name-length present-row count (calibration.rs::name_length_distributions_are_measured_and_pinned)',
         CAST(goblin_len_present AS DOUBLE), 766.0, goblin_len_present = 766 FROM agg
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): 13.461_308_710_376_134 ->
  -- 13.382_874_198_569_583.
  -- The Tumult (predation) re-pin, 0063: 13.382_874_198_569_583 ->
  -- 13.397_077_864_229_757.
  -- The Wearing re-pin, 0063: 13.397_077_864_229_757 -> 9.563_241_092_950_387
  -- (-28.6%); median 9.33, inside the metric's own declared buckets.
  SELECT 'mean goblin name length (calibration.rs::name_length_distributions_are_measured_and_pinned)',
         goblin_len_mean, 9.563_241_092_950_387, abs(goblin_len_mean - 9.563_241_092_950_387) < 1e-6 FROM agg
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): 772 -> 769.
  -- The Wearing, 0063: UNMOVED at 762.
  SELECT 'kobold name-length present-row count (calibration.rs::name_length_distributions_are_measured_and_pinned)',
         CAST(kobold_len_present AS DOUBLE), 762.0, kobold_len_present = 762 FROM agg
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): 12.748_786_009_455_962 ->
  -- 12.690_321_674_122_243.
  -- The Tumult (predation) re-pin, 0063: 12.690_321_674_122_243 ->
  -- 13.211_758_902_624_661 (kobold moves far more than goblin — the same
  -- reseating that inverts the coastal-rate ordering).
  -- The Wearing re-pin, 0063: 13.211_758_902_624_661 -> 7.638_659_840_944_880_5
  -- (-42.2%); median 7.40. Kobold shortens half again as hard as goblin.
  SELECT 'mean kobold name length (calibration.rs::name_length_distributions_are_measured_and_pinned)',
         kobold_len_mean, 7.638_659_840_944_880_5, abs(kobold_len_mean - 7.638_659_840_944_880_5) < 1e-6 FROM agg
  UNION ALL
  -- The Wearing (2026-07-28), NEW pins: the three metrics the campaign added.
  -- Mean syllable count is spec §8 criterion 2 (target 2-3). Both daughters
  -- land inside at census scale — which CORRECTS the four-seed probe that
  -- read goblin at 3.03-3.16, above the range.
  SELECT 'goblin name-syllables present-row count (calibration.rs::name_syllable_distributions_are_measured_and_pinned)',
         CAST(goblin_syl_present AS DOUBLE), 766.0, goblin_syl_present = 766 FROM agg
  UNION ALL
  SELECT 'mean goblin name syllables (calibration.rs::name_syllable_distributions_are_measured_and_pinned)',
         goblin_syl_mean, 2.853_548_007_963_447_7, abs(goblin_syl_mean - 2.853_548_007_963_447_7) < 1e-6 FROM agg
  UNION ALL
  SELECT 'kobold name-syllables present-row count (calibration.rs::name_syllable_distributions_are_measured_and_pinned)',
         CAST(kobold_syl_present AS DOUBLE), 762.0, kobold_syl_present = 762 FROM agg
  UNION ALL
  SELECT 'mean kobold name syllables (calibration.rs::name_syllable_distributions_are_measured_and_pinned)',
         kobold_syl_mean, 2.278_410_790_682_414_3, abs(kobold_syl_mean - 2.278_410_790_682_414_3) < 1e-6 FROM agg
  UNION ALL
  -- Transparency's target is explicitly NOT 1.0 (spec §8). It read exactly
  -- 1.00 before this campaign — 650 of 650 names — because nothing ever wore.
  -- A drift back UP toward 1.0 is a REGRESSION here, not an improvement.
  SELECT 'name-transparency present-row count (calibration.rs::name_transparency_is_measured_and_pinned)',
         CAST(transparency_present AS DOUBLE), 770.0, transparency_present = 770 FROM agg
  UNION ALL
  SELECT 'mean name-transparency (calibration.rs::name_transparency_is_measured_and_pinned)',
         transparency_mean, 0.826_729_134_389_610_3, abs(transparency_mean - 0.826_729_134_389_610_3) < 1e-6 FROM agg
  UNION ALL
  SELECT 'minimum name-transparency (calibration.rs::name_transparency_is_measured_and_pinned)',
         transparency_min, 0.24705882, abs(transparency_min - 0.24705882) < 1e-6 FROM agg
  UNION ALL
  SELECT 'maximum name-transparency (calibration.rs::name_transparency_is_measured_and_pinned)',
         transparency_max, 1.0, abs(transparency_max - 1.0) < 1e-6 FROM agg
  UNION ALL
  SELECT 'mean goblin hue-depth (calibration.rs::goblin_hue_depth_exceeds_kobold_hue_depth)',
         goblin_hue_mean, 4.0, abs(goblin_hue_mean - 4.0) < 1e-6 FROM agg
  UNION ALL
  SELECT 'mean kobold hue-depth (calibration.rs::goblin_hue_depth_exceeds_kobold_hue_depth)',
         kobold_hue_mean, 2.0, abs(kobold_hue_mean - 2.0) < 1e-6 FROM agg
  UNION ALL
  SELECT '"the-census" fixture row count (calibration.rs — implicit in several present+absent==1000 row-count assertions)',
         CAST(row_count AS DOUBLE), 1000.0, row_count = 1000 FROM agg
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): 325 -> 324.
  -- The Tumult (predation) re-pin, 0063: 324 -> 323.
  SELECT 'indistinguishable solo-pair count (calibration.rs::null_control_blind_attribution_is_at_chance)',
         CAST(indistinguishable AS DOUBLE), 323.0, indistinguishable = 323 FROM meeting_agg
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): 64 -> 63.
  -- The Tumult (predation) re-pin, 0063: 63 -> 64.
  SELECT 'decided solo-pair count (calibration.rs::null_control_blind_attribution_is_at_chance)',
         CAST(total_pairs - indistinguishable AS DOUBLE), 64.0, total_pairs - indistinguishable = 64 FROM meeting_agg
  UNION ALL
  SELECT 'head-domain distribution diff (calibration.rs::null_control_distributions_are_within_the_sampling_bound, head TVD == 0)',
         CAST(diff AS DOUBLE), 0.0, diff = 0 FROM head_domain_diff
  UNION ALL
  -- cult-form TVD is no longer exactly zero under history-first placement
  -- (path-dependent condensation order breaks the perfect solo/twin-solo
  -- clone on a couple of seeds); pinned to the measured count-diff.
  -- The Sundering (moving-sea epoch, 0063): 2 -> 4.
  -- The Tumult (predation) re-pin, 0063: 4 -> 6.
  SELECT 'cult-form distribution diff (calibration.rs::null_control_distributions_are_within_the_sampling_bound, cult-form TVD re-pinned nonzero)',
         CAST(diff AS DOUBLE), 6.0, diff = 6 FROM cult_form_diff
  UNION ALL
  -- Same movement as cult-form above: pantheon-size mean-diff, formerly an
  -- exact-zero structural pin, is now a tiny measured nonzero value.
  -- The Sundering (moving-sea epoch, 0063): -0.010_282_776_349_614_053 ->
  -- -0.012_919_896_640_825_712.
  -- The Tumult (predation) re-pin, 0063: -0.012_919_896_640_825_712 ->
  -- -0.015_503_875_968_992_276.
  SELECT 'pantheon-size mean diff (calibration.rs::null_control_distributions_are_within_the_sampling_bound, SMD re-pinned nonzero)',
         mean_a - mean_b, -0.015_503_875_968_992_276, abs(mean_a - mean_b - -0.015_503_875_968_992_276) < 1e-6 FROM pantheon_size_stats
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): -0.057_530_513_798_514_59 ->
  -- -0.065_161_843_432_313_42.
  -- The Tumult (predation) re-pin, 0063: -0.065_161_843_432_313_42 ->
  -- -0.064_965_927_887_856_34.
  -- The Wearing re-pin, 0063: -0.064_965_927_887_856_34 ->
  -- 0.026_557_760_190_573_92. The magnitude stays an order of magnitude
  -- inside the ±0.2 sampling bound and is in fact SMALLER than before; the
  -- sign change carries no information at this scale (387 rows a side).
  SELECT 'name-length SMD (calibration.rs::null_control_name_length_smd_is_pinned)',
         (mean_a - mean_b) / sqrt((var_a + var_b) / 2.0), 0.026_557_760_190_573_92,
         abs((mean_a - mean_b) / sqrt((var_a + var_b) / 2.0) - 0.026_557_760_190_573_92) < 1e-6
    FROM namelen_stats
)
SELECT pin, computed, pinned, ok FROM checks ORDER BY pin;
