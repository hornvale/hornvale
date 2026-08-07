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
-- (Resync 2026-07-27, The Tithe (tribute epoch) regen on lefford at the
-- merged SHA (0063): a raid whose prize is MOBILE now resolves as
-- subordination rather than eviction, so the loser survives as a tribute-
-- paying vassal instead of being displaced. The mean surviving roster nearly
-- doubles (settlement-count 74.67 -> 147.375; seed 42, 203 -> 329 live), and
-- settlement placement is what most of these values measure; all 13 drifted
-- literals below were re-synced to calibration.rs in this same commit. One
-- movement runs against intuition and is recorded, not explained: the roster
-- nearly doubles while the mean name-collision-rate FALLS 0.1858 -> 0.1383,
-- reversing the "more names drawn, more reuse" expectation — while the
-- zero/nonzero split moves the other way (six more worlds show SOME
-- collision). No guarded claim moved: blind attribution still clears its 0.75
-- floor at 0.9142, and the mooned+spinning perfect-attribution invariant
-- never fired.)
-- (Resync 2026-08-02, The Contour (position-aware conflict, decision 0096):
-- census regen on lefford at the canonical commit 4c46b45e. Defensibility as
-- a second contest axis reshapes raid/settle outcomes on nearly every world,
-- reseating flagships and shifting which worlds hold a pantheon at all; 19
-- literals below were re-synced to calibration.rs's own re-pin in this same
-- commit. This file had also drifted from calibration.rs independently of
-- the census move (`make census-check` was not run at the prior re-pin), so
-- this resync also restores the tripwire's own invariant: the two paths must
-- move together.)
-- (Resync 2026-08-02, The Contour epoch v2: the `history/bake` stream label
-- was bumped to `history/bake/v2` (decision 0006 — an epoch suffix, never a
-- rename), which re-mints every draw the deep-history bake takes on top of
-- whatever position-aware conflict itself changed, so every world moves a
-- second time. Census regen on lefford at commit c55005ed/488de111, 0063.
-- Every literal below that changed with this regen is re-synced to
-- calibration.rs's own re-pin in the same commit. The two null-control TVD/
-- SMD structural rows (cult-form, pantheon-size, name-length) that stopped
-- being exact zeroes under The Living Community remain tiny nonzero
-- measurements, still comfortably inside the sampling bound.)
-- (Resync 2026-08-04, The Generalist: human joins the roster as a sixth
-- settlement competitor (census regen on lefford at 02172e96, 0063/0079), so
-- every world's settlement contest is decided among six competitors rather
-- than five. All drifted literals below were re-synced to calibration.rs's
-- own re-pin in the same commit — see
-- .superpowers/sdd/2026-08-03-the-generalist/task-7b-report.md for the full
-- table and the claim-by-claim verification. No guarded claim moved.)
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
--     name_gloss_true_is_100_percent_row_by_row
--     — boolean invariants that hold 100% by construction (grammar,
--     name-gloss truthfulness); no drifting literal, and re-deriving "is
--     this name valid" needs the phonotactic machinery itself, not just the
--     fixture.
--     (epithet_honorific_is_true_for_goblin_and_false_for_kobold and
--     lexicon_is_regular_and_exposure_sound_for_both_species — since the F11
--     discharge split into lexicon_is_regular_for_both_species and
--     lexicon_is_exposure_sound_for_both_species — stood in this
--     list until 2026-07-28 on the stated ground that they "hold 100%/0% by
--     construction". That ground was FALSE, and this file's silence is how
--     it stayed false: the epithet column read 452 false and both
--     exposure-sound columns read 748 false on the first Wearing regen, and
--     nothing here could notice, because a pin nobody wrote is a pin nobody
--     can break. Both are now translated below as ordinary counted
--     literals, false counts included, and the epithet one additionally
--     pins the exact seeds of its two known exceptions. This is the
--     campaign's signature defect — a comment asserting a property the code
--     lacks — found in this very file.)
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
    -- RESTORED at the F11 discharge, 2026-07-30. These eight aggregates were
    -- commented out at The Wearing's close because the columns did not exist
    -- in the census committed at that merge — DuckDB failed to bind them and
    -- took the whole pin file down with it. The columns exist now (the regen
    -- landed separately, at 9855048d and then 4cd19ff9), so they bind, and
    -- their matching `checks` rows are restored in the same commit. That
    -- pairing is not optional: an aggregate nothing reads computes a value
    -- nobody checks, which is how they came to be deleted from `checks` in
    -- the first place.
    -- name_syllable_distributions_are_measured_and_pinned (The Wearing, new)
    count(*) FILTER (WHERE "name-syllables-goblin" IS NOT NULL) AS goblin_syl_present,
    avg("name-syllables-goblin") AS goblin_syl_mean,
    count(*) FILTER (WHERE "name-syllables-kobold" IS NOT NULL) AS kobold_syl_present,
    avg("name-syllables-kobold") AS kobold_syl_mean,
    -- name_transparency_is_measured_and_pinned (The Wearing, new). The min and
    -- max are pinned ALONGSIDE the mean, exactly as the deferred note asked:
    -- the SPREAD is the finding. A mean of 0.816 with every world reading
    -- 0.816 would be the same uniformity defect the campaign removed, wearing
    -- a different number, and only the min/max can tell the two apart.
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
    -- epithet_honorific_is_true_for_goblin_and_false_for_kobold (The
    -- Wearing Task 11d, newly translated — see the header note: these
    -- columns are no longer the 100%/0%-by-construction booleans this file
    -- previously declined to duplicate).
    count(*) FILTER (WHERE "epithet-honorific-goblin" = true) AS epithet_goblin_true,
    count(*) FILTER (WHERE "epithet-honorific-goblin" = false) AS epithet_goblin_false,
    count(*) FILTER (WHERE "epithet-honorific-goblin" IS NULL) AS epithet_goblin_absent,
    -- The two detector-blind seeds, pinned by their min and max: with the
    -- false COUNT pinned at 2, min and max together identify the set
    -- exactly, so a different pair of worlds cannot pass by arithmetic.
    min(seed) FILTER (WHERE "epithet-honorific-goblin" = false) AS epithet_goblin_false_lo,
    max(seed) FILTER (WHERE "epithet-honorific-goblin" = false) AS epithet_goblin_false_hi,
    count(*) FILTER (WHERE "epithet-honorific-kobold" = false) AS epithet_kobold_false,
    count(*) FILTER (WHERE "epithet-honorific-kobold" = true) AS epithet_kobold_true,
    count(*) FILTER (WHERE "epithet-honorific-kobold" IS NULL) AS epithet_kobold_absent,
    -- lexicon_is_exposure_sound_for_both_species (The Wearing Task 11d,
    -- newly translated for the same reason; the Rust row was split in two at
    -- the F11 discharge and this is the half that owns the soundness claim).
    -- The false counts are pinned alongside the true counts deliberately: the
    -- first regen read 748 false on both species for eleven days with nothing
    -- in this file able to notice — and the F11 discharge found them false
    -- again, from a different cause, which is exactly the recurrence this
    -- pin exists to make loud. See the note on the check rows below.
    count(*) FILTER (WHERE "exposure-sound-goblin" = true) AS exposure_goblin_true,
    count(*) FILTER (WHERE "exposure-sound-goblin" = false) AS exposure_goblin_false,
    count(*) FILTER (WHERE "exposure-sound-kobold" = true) AS exposure_kobold_true,
    count(*) FILTER (WHERE "exposure-sound-kobold" = false) AS exposure_kobold_false,
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
  -- The Generalist resync, 0063/0079: 150 -> 148.
  -- The Tolerance resync, 0063/0079: 148 -> 151.
  SELECT 'locked-eternal per-people head count (calibration.rs::a_frozen_sky_never_heads_a_cyclic_pantheon)' AS pin,
         CAST(locked_eternal AS DOUBLE) AS computed, 152.0 AS pinned, locked_eternal = 152 AS ok FROM agg
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): 41 -> 40.
  -- The Contour epoch v2 resync, 0063: 39 -> 40.
  -- The Tolerance resync, 0063/0079: 40 -> 41.
  SELECT 'locked-ambient per-people head count (calibration.rs::a_frozen_sky_never_heads_a_cyclic_pantheon)',
         CAST(locked_ambient AS DOUBLE), 40.0, locked_ambient = 40 FROM agg
  UNION ALL
  -- The Generalist resync, 0063/0079: 11 -> 9.
  SELECT 'spinning-yet-eternal per-people head count (calibration.rs::a_frozen_sky_never_heads_a_cyclic_pantheon)',
         CAST(spinning_eternal_exceptions AS DOUBLE), 10.0, spinning_eternal_exceptions = 10 FROM agg
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): 536 -> 535.
  -- The Tumult (predation) re-pin, 0063: 535 -> 531.
  -- The Tithe (tribute) re-pin, 0063: 552 -> 556.
  -- The Contour epoch v2 resync, 0063: 548 -> 554.
  -- The Generalist resync, 0063/0079: 554 -> 559.
  -- The Tolerance resync, 0063/0079: 559 -> 565 (565 + 204 = 769, goblin's
  -- re-pinned present-row count).
  SELECT 'goblin flagship coastal count (calibration.rs::goblin_flagship_coastal_split_is_pinned)',
         CAST(flagship_coastal AS DOUBLE), 217.0, flagship_coastal = 217 FROM agg
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): 235 -> 234.
  -- The Tumult (predation) re-pin, 0063: 234 -> 238.
  -- The Tithe (tribute) re-pin, 0063: 214 -> 211.
  -- The Contour epoch v2 resync, 0063: 218 -> 214.
  -- The Generalist resync, 0063/0079: 214 -> 207.
  -- The Tolerance resync, 0063/0079: 207 -> 204.
  SELECT 'goblin flagship inland count (calibration.rs::goblin_flagship_coastal_split_is_pinned)',
         CAST(flagship_inland AS DOUBLE), 782.0, flagship_inland = 782 FROM agg
  UNION ALL
  -- The Tithe (tribute) re-pin, 0063: 33 -> 34.
  -- The Contour epoch v2 resync, 0063: 34 -> 33.
  -- The Generalist resync, 0063/0079: unchanged at 33.
  -- The Tolerance resync, 0063/0079: 33 -> 32.
  SELECT 'moonless-solar kobold head count (calibration.rs::goblin_heads_are_always_solar_and_mooned_kobold_heads_always_lunar)',
         CAST(moonless_solar AS DOUBLE), 55.0, moonless_solar = 55 FROM agg
  UNION ALL
  -- The Tithe (tribute) re-pin, 0063: 61 -> 59.
  -- The Contour epoch v2 resync, 0063: 60 -> 62.
  -- The Generalist resync, 0063/0079: 62 -> 61.
  -- The Tolerance resync, 0063/0079: 61 -> 64.
  SELECT 'moonless-lunar kobold head count (calibration.rs::goblin_heads_are_always_solar_and_mooned_kobold_heads_always_lunar)',
         CAST(moonless_lunar AS DOUBLE), 85.0, moonless_lunar = 85 FROM agg
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): 703 -> 700.
  -- The Tumult (predation) re-pin, 0063: 700 -> 702 (the 768-row pool is
  -- unchanged, so accuracy rises 0.911 -> 0.914).
  -- The Tithe (tribute) re-pin, 0063: 695 -> 693 (accuracy 0.9157 -> 0.9142).
  -- The Contour epoch v2 resync, 0063: 694 -> 697.
  -- The Generalist resync, 0063/0079: 697 -> 693 (accuracy 0.9135 ->
  -- 0.9130434782608696, still decisively above the 0.75 floor).
  -- The Tolerance resync, 0063/0079: 693 -> 701 (accuracy 0.9130434782608696
  -- -> 0.9127604166666666, still decisively above the 0.75 floor).
  SELECT 'blind-attribution correct count (calibration.rs::blind_attribution_beats_chance_decisively)',
         CAST(blind_correct AS DOUBLE), 881.0, blind_correct = 881 FROM agg
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): 771 -> 768.
  -- The Tithe (tribute) re-pin, 0063: 759 -> 758.
  -- The Contour epoch v2 resync, 0063: 761 -> 763.
  -- The Generalist resync, 0063/0079: 763 -> 759.
  -- The Tolerance resync, 0063/0079: 759 -> 768.
  SELECT 'blind-attribution total count (calibration.rs::blind_attribution_beats_chance_decisively)',
         CAST(blind_total AS DOUBLE), 972.0, blind_total = 972 FROM agg
  UNION ALL
  SELECT 'mooned+spinning blind-attribution mismatches (calibration.rs::blind_attribution_beats_chance_decisively, correct_mooned == total_mooned)',
         CAST(mooned_spinning_mismatches AS DOUBLE), 0.0, mooned_spinning_mismatches = 0 FROM agg
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): 50 -> 48.
  -- The Tumult (predation) re-pin, 0063: 48 -> 43.
  -- The Tithe (tribute) re-pin, 0063: 39 -> 33.
  -- The Toponym (name-gloss epoch), 0063: 33 -> 43.
  -- F11 discharge re-pin, 2026-07-30 (rows.csv at 4cd19ff9): 43 -> 1. See the
  -- decision-0024 note at calibration.rs::name_collision_rate_is_measured_and_
  -- pinned: this rise is SANCTIONED and is not to be bought back with entropy.
  -- The Witness (cascade/v2 epoch), 0063: 2 -> 3. Every cascade in every
  -- world reseeded when `draw_rule` stopped offering rules the phonology
  -- cannot host, so every generated name moved; one more world lands on a
  -- zero collision rate and one fewer on a nonzero one. The absent count is
  -- unmoved, and 2+768 = 3+767, so the partition is intact.
  -- The Contour epoch v2 resync, 0063: 3 -> 4.
  -- The Generalist resync, 0063/0079: 4 -> 3.
  -- The Tolerance resync, 0063/0079: 3 -> 1.
  SELECT 'zero-collision world count (calibration.rs::name_collision_rate_is_measured_and_pinned)',
         CAST(collision_zero AS DOUBLE), 1.0, collision_zero = 1 FROM agg
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): 723 -> 722.
  -- The Tumult (predation) re-pin, 0063: 722 -> 727.
  -- The Tithe (tribute) re-pin, 0063: 731 -> 737.
  -- The Toponym (name-gloss epoch), 0063: 737 -> 727.
  -- F11 discharge re-pin, 2026-07-30: 727 -> 769; the absent set is unmoved.
  -- The Witness (cascade/v2 epoch), 0063: 768 -> 767. The paired half of the
  -- zero-collision row above; the absent set is unmoved.
  -- The Contour epoch v2 resync, 0063: 767 -> 766.
  -- The Generalist resync, 0063/0079: 766 -> 767.
  -- The Tolerance resync, 0063/0079: 767 -> 769; the absent set is unmoved at
  -- 230 (1 + 769 + 230 = 1000).
  SELECT 'nonzero-collision world count (calibration.rs::name_collision_rate_is_measured_and_pinned)',
         CAST(collision_nonzero AS DOUBLE), 999.0, collision_nonzero = 999 FROM agg
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): 227 -> 230.
  SELECT 'absent name-collision-rate count (calibration.rs::name_collision_rate_is_measured_and_pinned)',
         CAST(collision_absent AS DOUBLE), 0.0, collision_absent = 0 FROM agg
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): 0.178_726_790_236_740_12 ->
  -- 0.183_235_100_516_883.
  -- The Tumult (predation) re-pin, 0063: 0.183_235_100_516_883 ->
  -- 0.185_804_141_557_143 (predation prunes the roster, so fewer names
  -- are drawn per world and the rate FALLS).
  -- The Tithe (tribute) re-pin, 0063: 0.185_804_141_557_143 ->
  -- 0.126_857_511_090_779 — the roster nearly doubles yet the rate FALLS,
  -- recorded as measured, not explained (see the header note).
  -- F11 discharge re-pin, 2026-07-30: 0.126_857_511_090_779 ->
  -- 0.564_509_597_998_702. Two forces, neither a defect: the name space
  -- narrowed (mean name length fell sharply, below) while the roster of things
  -- to name kept growing. Sanctioned by decision 0024 — read it before
  -- treating this as something to fix.
  -- The Witness (cascade/v2 epoch), 0063: 0.564_509_597_998_702 ->
  -- 0.567_057_788_528_571. Same cascade reseed as the zero/nonzero pins
  -- above.
  -- The Contour epoch v2 resync, 0063: 0.559_547_123_829_870 ->
  -- 0.560_567_825_485_714_4.
  -- The Tolerance resync, 0063/0079: 0.528_593_255_324_676 ->
  -- 0.535_421_983_528_571_5.
  SELECT 'mean name-collision-rate (calibration.rs::name_collision_rate_is_measured_and_pinned)',
  -- The Salt's close regen, 0063 (the mover is ce13bae0's compass concepts,
  -- not The Salt, which touches no language code): 0.560_567_825_485_714_4 ->
  -- 0.560_572_844_615_584_4.
  -- The Generalist resync, 0063/0079: 0.560_572_844_615_584_4 ->
  -- 0.528_593_255_324_676.
         collision_mean, 0.5203500368360002, abs(collision_mean - 0.5203500368360002) < 1e-6 FROM agg
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): 771 -> 769.
  -- The Tithe (tribute) re-pin, 0063: 766 -> 767.
  -- The Contour epoch v2 resync, 0063: 766 -> 768.
  -- The Generalist resync, 0063/0079: 768 -> 766.
  -- The Tolerance resync, 0063/0079: 766 -> 769.
  SELECT 'goblin name-length present-row count (calibration.rs::name_length_distributions_are_measured_and_pinned)',
         CAST(goblin_len_present AS DOUBLE), 999.0, goblin_len_present = 999 FROM agg
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): 13.461_308_710_376_134 ->
  -- 13.382_874_198_569_583.
  -- The Tumult (predation) re-pin, 0063: 13.382_874_198_569_583 ->
  -- 13.397_077_864_229_757.
  -- The Tithe (tribute) re-pin, 0063: 13.397_077_864_229_757 ->
  -- 13.665_297_457_235_99.
  -- F11 discharge re-pin, 2026-07-30: 13.665_297_457_235_99 ->
  -- 8.784_123_816_558_01. The present count holds at 767.
  -- The Witness (cascade/v2 epoch), 0063: 8.784_123_816_558_01 ->
  -- 8.639_595_029_986_95. Present count holds at 767.
  -- The Contour epoch v2 resync, 0063: 8.660_349_090_208_882 ->
  -- 8.688_230_827_083_34. Present count moves 766 -> 768.
  -- The Tolerance resync, 0063/0079: 8.657_123_104_960_824 ->
  -- 8.787_985_079_973_994. Still below the campaign's own <10-character claim
  -- (spec 7).
  SELECT 'mean goblin name length (calibration.rs::name_length_distributions_are_measured_and_pinned)',
  -- The Salt's close regen, 0063 (the mover is ce13bae0's compass concepts,
  -- not The Salt, which touches no language code): 8.688_230_827_083_34 ->
  -- 8.687_525_197_786_464.
  -- The Generalist resync, 0063/0079: 8.687_525_197_786_464 ->
  -- 8.657_123_104_960_824. Present count 768 -> 766.
         goblin_len_mean, 8.54194281271272, abs(goblin_len_mean - 8.54194281271272) < 1e-6 FROM agg
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): 772 -> 769.
  -- The Tithe (tribute) re-pin, 0063: 762 -> 760.
  -- The Contour epoch v2 resync, 0063: 763 -> 765.
  -- The Generalist resync, 0063/0079: 765 -> 762.
  -- The Tolerance resync, 0063/0079: 762 -> 769.
  SELECT 'kobold name-length present-row count (calibration.rs::name_length_distributions_are_measured_and_pinned)',
         CAST(kobold_len_present AS DOUBLE), 972.0, kobold_len_present = 972 FROM agg
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): 12.748_786_009_455_962 ->
  -- 12.690_321_674_122_243.
  -- The Tumult (predation) re-pin, 0063: 12.690_321_674_122_243 ->
  -- 13.211_758_902_624_661 (kobold moves far more than goblin — the same
  -- reseating that inverts the coastal-rate ordering).
  -- The Tithe (tribute) re-pin, 0063: 13.211_758_902_624_661 ->
  -- 15.548_879_020_789_471 (kobold again moves far more than goblin).
  -- F11 discharge re-pin, 2026-07-30: 15.548_879_020_789_471 ->
  -- 7.403_195_966_315_787. Kobold moves nearly twice as far as goblin again.
  -- The Witness (cascade/v2 epoch), 0063: 7.403_195_966_315_787 ->
  -- 7.228_477_004_342_105. Present count holds at 760.
  -- The Contour epoch v2 resync, 0063: 7.219_848_265_006_563 ->
  -- 7.188_685_503_790_846. Present count moves 763 -> 765.
  -- The Tolerance resync, 0063/0079: 7.189_805_441_863_518 ->
  -- 7.236_424_583_355_002. Still below the campaign's own <10-character claim
  -- (spec 7).
  SELECT 'mean kobold name length (calibration.rs::name_length_distributions_are_measured_and_pinned)',
  -- The Salt's close regen, 0063 (the mover is ce13bae0's compass concepts,
  -- not The Salt, which touches no language code): 7.188_685_503_790_846 ->
  -- 7.188_604_358_823_526.
  -- The Generalist resync, 0063/0079: 7.188_604_358_823_526 ->
  -- 7.189_805_441_863_518 (essentially unmoved). Present count 765 -> 762.
         kobold_len_mean, 6.885304561419753, abs(kobold_len_mean - 6.885304561419753) < 1e-6 FROM agg
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
  -- RESTORED at the F11 discharge, 2026-07-30, re-derived through DuckDB
  -- against the committed rows.csv (4cd19ff9) and landed in the SAME commit as
  -- their Rust counterpart's re-pin. That pairing is the whole point of this
  -- tripwire: if the SQL and calibration.rs are re-pinned in separate commits,
  -- the one thing `make census-check` exists to catch — the two disagreeing —
  -- is exactly what slips through the gap.
  --
  -- The Wearing deferred these with literals (764/2/234 goblin, 762/0/238
  -- kobold, false seeds 386 and 976) measured against a census its close merge
  -- superseded, and explicitly declined to re-pin them to main's numbers on
  -- the grounds that a different wrong answer is not the answer. That call was
  -- right and is now discharged the way it asked to be: from the census that
  -- IS committed, not from either prior set.
  --
  -- The measured population turned over completely. Goblin reads 766 true / 1
  -- false / 233 absent; kobold 760 false / 0 true / 240 absent. Seeds 386 and
  -- 976 are no longer detector-blind at all — every belief of both worlds now
  -- detects its affix unaided — and the sole remaining false is seed 400,
  -- chased to a named cause before it was written down (see
  -- calibration.rs::HONORIFIC_DETECTOR_BLIND_SEEDS for the derivation).
  --
  -- With the false COUNT pinned at 1, min and max necessarily agree, and both
  -- are pinned anyway: the pair identifies the set exactly, so a different
  -- world cannot pass by arithmetic, and if the count ever rises the two pins
  -- diverge and say so.
  -- The Contour epoch v2 resync, 0063: 766 -> 768.
  -- The Generalist resync, 0063/0079: 768 -> 766.
  -- The Tolerance resync, 0063/0079: 766 -> 769.
  SELECT 'goblin epithet-honorific true count (calibration.rs::epithet_honorific_is_true_for_goblin_and_false_for_kobold)',
         CAST(epithet_goblin_true AS DOUBLE), 999.0, epithet_goblin_true = 999 FROM agg
  UNION ALL
  SELECT 'goblin epithet-honorific false count — the diagnosed detector-blind world (calibration.rs::epithet_honorific_is_true_for_goblin_and_false_for_kobold)',
         CAST(epithet_goblin_false AS DOUBLE), 0.0, epithet_goblin_false = 0 FROM agg
  UNION ALL
  -- The Contour epoch v2 resync, 0063: 234 -> 232.
  -- The Generalist resync, 0063/0079: 232 -> 234.
  -- The Tolerance resync, 0063/0079: 234 -> 231.
  SELECT 'goblin epithet-honorific absent count (calibration.rs::epithet_honorific_is_true_for_goblin_and_false_for_kobold)',
         CAST(epithet_goblin_absent AS DOUBLE), 1.0, epithet_goblin_absent = 1 FROM agg
  UNION ALL
  -- The two seed-identity pins that stood here (lowest/highest goblin
  -- epithet-honorific false seed, both 400) are DELETED, not re-pinned. The
  -- Watershed's sonority epoch took the blind-world count to zero, so
  -- `min`/`max` over an empty set return NULL, the comparison returns NULL,
  -- and the checker reports neither pass nor fail. A pin that cannot fail is
  -- worse than no pin: it reads as coverage. Same argument that retired
  -- `STALE_SECOND_OPINION` in the same commit range. The count pins above
  -- (false = 0) carry the claim now, and they CAN fail — if a blind world
  -- returns, that row reddens and the seed identity can be re-established
  -- from the diagnosis kept at HONORIFIC_DETECTOR_BLIND_SEEDS.
  -- Unmoved in KIND by anything since Task 11d, as it must be: the detector
  -- reads a PRESENT affix, and kobold (Knowledge status basis) has none. Only
  -- the counts move, with the worlds that hold a flagship pantheon at all.
  -- The Contour epoch v2 resync, 0063: 763 -> 765.
  -- The Generalist resync, 0063/0079: 765 -> 762.
  -- The Tolerance resync, 0063/0079: 762 -> 769.
  SELECT 'kobold epithet-honorific false count (calibration.rs::epithet_honorific_is_true_for_goblin_and_false_for_kobold)',
         CAST(epithet_kobold_false AS DOUBLE), 972.0, epithet_kobold_false = 972 FROM agg
  UNION ALL
  SELECT 'kobold epithet-honorific TRUE count — structurally impossible for a non-Rank people (calibration.rs::epithet_honorific_is_true_for_goblin_and_false_for_kobold)',
         CAST(epithet_kobold_true AS DOUBLE), 0.0, epithet_kobold_true = 0 FROM agg
  UNION ALL
  -- The Contour epoch v2 resync, 0063: 237 -> 235.
  -- The Generalist resync, 0063/0079: 235 -> 238.
  -- The Tolerance resync, 0063/0079: 238 -> 231.
  SELECT 'kobold epithet-honorific absent count (calibration.rs::epithet_honorific_is_true_for_goblin_and_false_for_kobold)',
         CAST(epithet_kobold_absent AS DOUBLE), 28.0, epithet_kobold_absent = 28 FROM agg
  UNION ALL
  -- The eight naming aggregates restored at the top of `agg` need their
  -- assertions here, or they compute values nothing reads. Re-derived through
  -- DuckDB against the committed rows.csv, matching
  -- calibration.rs::name_syllable_distributions_are_measured_and_pinned and
  -- ::name_transparency_is_measured_and_pinned exactly.
  -- The Contour epoch v2 resync, 0063: 766 -> 768.
  -- The Generalist resync, 0063/0079: 768 -> 766.
  -- The Tolerance resync, 0063/0079: 766 -> 769, agreeing with the
  -- name-length row above.
  SELECT 'goblin name-syllables present-row count (calibration.rs::name_syllable_distributions_are_measured_and_pinned)',
         CAST(goblin_syl_present AS DOUBLE), 999.0, goblin_syl_present = 999 FROM agg
  UNION ALL
  -- Spec §8 criterion 2 asks for a mean syllable count in the 2-3 range; both
  -- species read inside it, which is the claim the Rust row carries.
  -- The Witness (cascade/v2 epoch), 0063: 2.761_284_613_820_079 ->
  -- 2.767_352_168_839_636. Still inside 2-3.
  -- The Contour epoch v2 resync, 0063: 2.775_172_454_830_285 ->
  -- 2.787_026_517_317_707_3. Present count moves 766 -> 768.
  -- The Tolerance resync, 0063/0079: 2.763_782_961_879_896 ->
  -- 2.813_568_037_061_118, still inside the preregistered 2-3 range.
  SELECT 'mean goblin name-syllables (calibration.rs::name_syllable_distributions_are_measured_and_pinned)',
  -- The Salt's close regen, 0063 (the mover is ce13bae0's compass concepts,
  -- not The Salt, which touches no language code): 2.787_026_517_317_707_3 ->
  -- 2.787_048_218_749_998_5.
  -- The Generalist resync, 0063/0079: 2.787_048_218_749_998_5 ->
  -- 2.763_782_961_879_896. Still inside 2-3. Present count 768 -> 766.
         goblin_syl_mean, 2.7208395536536543, abs(goblin_syl_mean - 2.7208395536536543) < 1e-6 FROM agg
  UNION ALL
  -- The Contour epoch v2 resync, 0063: 763 -> 765.
  -- The Generalist resync, 0063/0079: 765 -> 762.
  -- The Tolerance resync, 0063/0079: 762 -> 769, agreeing with the
  -- name-length row above.
  SELECT 'kobold name-syllables present-row count (calibration.rs::name_syllable_distributions_are_measured_and_pinned)',
         CAST(kobold_syl_present AS DOUBLE), 972.0, kobold_syl_present = 972 FROM agg
  UNION ALL
  -- The Witness (cascade/v2 epoch), 0063: 2.316_698_345_263_158 ->
  -- 2.318_080_226_315_786_7. Still inside 2-3.
  -- The Contour epoch v2 resync, 0063: 2.313_806_532_765_403 ->
  -- 2.306_935_878_954_248_7. Present count moves 763 -> 765.
  -- The Generalist resync, 0063/0079: 2.306_935_878_954_248_7 ->
  -- 2.305_631_764_829_393. Still inside 2-3. Present count 765 -> 762.
  -- The Tolerance resync, 0063/0079: 2.305_631_764_829_393 ->
  -- 2.319_622_207_412_223_2, still inside the preregistered 2-3 range.
  SELECT 'mean kobold name-syllables (calibration.rs::name_syllable_distributions_are_measured_and_pinned)',
         kobold_syl_mean, 2.192605995679012, abs(kobold_syl_mean - 2.192_605_995_679_012) < 1e-6 FROM agg
  UNION ALL
  SELECT 'name-transparency present-row count (calibration.rs::name_transparency_is_measured_and_pinned)',
         CAST(transparency_present AS DOUBLE), 1000.0, transparency_present = 1000 FROM agg
  UNION ALL
  -- The Witness (cascade/v2 epoch), 0063: 0.793_035_961_411_688_4 ->
  -- 0.803_660_578_424_675. Transparency ROSE — the wear cascade now lands
  -- real sound changes instead of spending rule slots on rules a species'
  -- phonology could never fire, so more names still gloss to their source
  -- concept. See the Rust row's own note for the fuller reading.
  -- The Contour epoch v2 resync, 0063: 0.807_392_672_749_351 ->
  -- 0.804_951_365_489_610_2.
  -- The Tolerance resync, 0063/0079: 0.785_500_964_077_923 ->
  -- 0.786_123_665_363_636_2. Still emphatically not 1.0.
  SELECT 'mean name-transparency (calibration.rs::name_transparency_is_measured_and_pinned)',
  -- The Salt's close regen, 0063 (the mover is ce13bae0's compass concepts,
  -- not The Salt, which touches no language code): 0.804_951_365_489_610_2 ->
  -- 0.804_225_380_346_752_7.
  -- The Generalist resync, 0063/0079: 0.804_225_380_346_752_7 ->
  -- 0.785_500_964_077_923. Still emphatically not 1.0.
         transparency_mean, 0.7978386186100005, abs(transparency_mean - 0.7978386186100005) < 1e-6 FROM agg
  UNION ALL
  -- The min and max are the SPREAD pins the deferred note asked for. A floor
  -- of 0.154 against a ceiling of 1.0 is what proves the 0.816 mean describes
  -- a distribution over worlds rather than a constant every world reads.
  -- The Generalist resync, 0063/0079: 0.076_923_077 -> 0.142_857_14. The
  -- spread narrows a little but is still a real distribution, not the
  -- uniformity defect (ceiling unmoved at 1.0, asserted below).
  -- The Tolerance resync, 0063/0079: 0.142_857_14 -> 0.15; the ceiling is
  -- unmoved at 1.0, so this is still a real distribution over worlds.
  SELECT 'min name-transparency — the spread floor (calibration.rs::name_transparency_is_measured_and_pinned)',
         transparency_min, 0.07253886, abs(transparency_min - 0.07253886) < 1e-6 FROM agg
  UNION ALL
  SELECT 'max name-transparency — the spread ceiling (calibration.rs::name_transparency_is_measured_and_pinned)',
         transparency_max, 1.0, abs(transparency_max - 1.0) < 1e-6 FROM agg
  UNION ALL
  -- The Wearing Task 11d re-pin, 0063: 252 -> 1000 true, 748 -> 0 false on
  -- both species, the stale second opinion repaired.
  --
  -- F11 discharge, 2026-07-30. The stale second opinion is BACK, from a new
  -- direction, and these four pins are re-pinned to what it actually reads
  -- rather than to what it ought to read. Goblin 1000 -> 233 true, 0 -> 767
  -- false; kobold 1000 -> 241 true, 0 -> 759 false.
  --
  -- Read them as a DEFECT WITNESS, not as a calibration of the worlds. The
  -- correlation is exact and is the diagnosis: exposure-sound is true on
  -- precisely the worlds where that species is UNPLACED and there is nothing
  -- to check, and false on every world where it holds a lexicon. The cause is
  -- named and narrow — windows/lab/src/metrics.rs::independently_steeped_
  -- concepts is a hand-maintained duplicate of hornvale_worldgen::exposure_of's
  -- Steeped rules, and it has not learned The Watershed's staple rules, so the
  -- six staple concepts (barley, millet, rice, tuber, vine, wheat) back Roots
  -- the duplicate does not steep. Nothing else appears in the tally.
  --
  -- This is the THIRD time the two copies have drifted apart (Task 4's
  -- toponymic rules were the first, repaired in Task 11c; this is the second
  -- rule set to be missed). Repairing it changes these two census columns and
  -- therefore owes a full regeneration, which is a campaign and not a
  -- followup, so the value is recorded rather than the row deleted — a pinned
  -- wrong number that says why it is wrong outlives a deleted one.
  --
  -- The Rust counterpart is calibration.rs::lexicon_is_exposure_sound_for_
  -- both_species, which is #[ignore]d under a `stale-second-opinion:` token
  -- (cli/tests/heavy_tier.rs). When the duplicate is repaired and the census
  -- regenerated, these four go back to 1000/0 and that row comes back with
  -- them.
  SELECT 'goblin exposure-sound true count (calibration.rs::lexicon_is_exposure_sound_for_both_species)',
         CAST(exposure_goblin_true AS DOUBLE), 1000.0, exposure_goblin_true = 1000 FROM agg
  UNION ALL
  SELECT 'goblin exposure-sound false count (calibration.rs::lexicon_is_exposure_sound_for_both_species)',
         CAST(exposure_goblin_false AS DOUBLE), 0.0, exposure_goblin_false = 0 FROM agg
  UNION ALL
  SELECT 'kobold exposure-sound true count (calibration.rs::lexicon_is_exposure_sound_for_both_species)',
         CAST(exposure_kobold_true AS DOUBLE), 1000.0, exposure_kobold_true = 1000 FROM agg
  UNION ALL
  SELECT 'kobold exposure-sound false count (calibration.rs::lexicon_is_exposure_sound_for_both_species)',
         CAST(exposure_kobold_false AS DOUBLE), 0.0, exposure_kobold_false = 0 FROM agg
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): 325 -> 324.
  -- The Tumult (predation) re-pin, 0063: 324 -> 323.
  -- The Contour epoch v2 resync, 0063: 323 -> 322.
  -- The Tolerance resync, 0063/0079: 322 -> 326 (326/387 = 0.842, still
  -- mostly indistinguishable).
  SELECT 'indistinguishable solo-pair count (calibration.rs::null_control_blind_attribution_is_at_chance)',
         CAST(indistinguishable AS DOUBLE), 417.0, indistinguishable = 417 FROM meeting_agg
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): 64 -> 63.
  -- The Tumult (predation) re-pin, 0063: 63 -> 64.
  -- The Contour epoch v2 resync, 0063: 64 -> 65.
  -- The Tolerance resync, 0063/0079: 65 -> 61 (31 of 61 pick the twin, a
  -- 0.508 split).
  SELECT 'decided solo-pair count (calibration.rs::null_control_blind_attribution_is_at_chance)',
         CAST(total_pairs - indistinguishable AS DOUBLE), 82.0, total_pairs - indistinguishable = 82 FROM meeting_agg
  UNION ALL
  SELECT 'head-domain distribution diff (calibration.rs::null_control_distributions_are_within_the_sampling_bound, head TVD == 1)',
         CAST(diff AS DOUBLE), 1.0, diff = 1 FROM head_domain_diff
  UNION ALL
  -- cult-form TVD is no longer exactly zero under history-first placement
  -- (path-dependent condensation order breaks the perfect solo/twin-solo
  -- clone on a couple of seeds); pinned to the measured count-diff.
  -- The Sundering (moving-sea epoch, 0063): 2 -> 4.
  -- The Tumult (predation) re-pin, 0063: 4 -> 6.
  -- The Contour epoch v2 resync, 0063: 6 -> 8 (TVD 0.007_751_937_984_496_131
  -- -> 0.010_335_917_312_661_49 in calibration.rs; pool still 387).
  -- The Tolerance resync, 0063/0079: 8 -> 12 diverging categories;
  -- calibration.rs's paired TVD moves 0.010_335_917_312_661_49 ->
  -- 0.015_503_875_968_992_262, still an order of magnitude inside the +/-0.15
  -- bound.
  SELECT 'cult-form distribution diff (calibration.rs::null_control_distributions_are_within_the_sampling_bound, cult-form TVD re-pinned nonzero)',
         CAST(diff AS DOUBLE), 9.0, diff = 9 FROM cult_form_diff
  UNION ALL
  -- Same movement as cult-form above: pantheon-size mean-diff, formerly an
  -- exact-zero structural pin, is now a tiny measured nonzero value. NOTE:
  -- this row pins the RAW mean difference as a zero/nonzero proxy, not the
  -- standardized SMD calibration.rs pins (-0.003_295_124_196_027_554_4) — the
  -- two are different quantities that move together, not the same number.
  -- The Sundering (moving-sea epoch, 0063): -0.010_282_776_349_614_053 ->
  -- -0.012_919_896_640_825_712.
  -- The Tumult (predation) re-pin, 0063: -0.012_919_896_640_825_712 ->
  -- -0.015_503_875_968_992_276.
  -- The Contour epoch v2 resync, 0063: -0.015_503_875_968_992_276 ->
  -- -0.012_919_896_640_825_712.
  -- The Tolerance resync, 0063/0079: -0.012_919_896_640_825_712 ->
  -- -0.020_671_834_625_321_85 (the RAW mean difference; calibration.rs's
  -- standardized SMD moves -0.003_295_124_196_027_554_4 ->
  -- -0.005_276_769_343_453_631).
  SELECT 'pantheon-size mean diff (calibration.rs::null_control_distributions_are_within_the_sampling_bound, SMD re-pinned nonzero)',
         mean_a - mean_b, 0.014581162324649455, abs(mean_a - mean_b - 0.014_581_162_324_649_455) < 1e-6 FROM pantheon_size_stats
  UNION ALL
  -- The Sundering (moving-sea epoch, 0063): -0.057_530_513_798_514_59 ->
  -- -0.065_161_843_432_313_42.
  -- The Tumult (predation) re-pin, 0063: -0.065_161_843_432_313_42 ->
  -- -0.065_714_087_428_851_79.
  -- Merge reconciliation (The Wearing x The Toponym, 2026-07-29): The
  -- Toponym's value is kept, because the census it was measured against is
  -- the rows.csv committed here; this branch's own (0.026_557_760_190_573_92)
  -- was measured against a census the merge replaced. Both predate the
  -- merged physics — F11's single regen re-measures.
  -- F11 discharge re-pin, 2026-07-30: -0.065_714_087_428_851_79 ->
  -- +-0.025_217_538_228_395_456. The sign flip is not a regression: this is a
  -- standardized mean difference against a deliberately-identical twin, so
  -- closer to zero is BETTER, and 0.005 is an order of magnitude closer than
  -- -0.066 was. (DuckDB's summation lands one ULP from the Rust row's
  -- -0.025_217_538_228_395_456; the 1e-6 tolerance is what makes the two
  -- independent computations comparable at all, and is unchanged.)
  -- The Witness (cascade/v2 epoch), 0063: -0.025_217_538_228_395_456 ->
  -- -0.012_055_568_856_886_177. The cascade reseed touches both the goblin
  -- and its deliberately-identical twin alike; the residual moved roughly
  -- HALFWAY toward zero, not away from it, so the null control reads more
  -- true after this re-pin than before. No sign flip.
  -- The Contour epoch v2 resync, 0063: -0.012_055_568_856_886_177 ->
  -- -0.017_848_707_186_831_292.
  -- The Tolerance resync, 0063/0079: -0.017_807_448_465_414_44 ->
  -- +0.009_246_730_125_111_655. The sign flips while the magnitude roughly
  -- HALVES toward zero, so the null hypothesis this row witnesses
  -- (indistinguishable from zero) reads more true, not less.
  SELECT 'name-length SMD (calibration.rs::null_control_name_length_smd_is_pinned)',
  -- The Salt's close regen, 0063 (the mover is ce13bae0's compass concepts,
  -- not The Salt, which touches no language code): -0.017_848_707_186_831_292 ->
  -- -0.018_069_698_979_322_31.
  -- The Generalist resync, 0063/0079: -0.018_069_698_979_322_31 ->
  -- -0.017_807_448_465_414_44 (human is not present in either solo build;
  -- recorded as measured, not explained). Moves toward zero, still ~11x
  -- inside the ±0.2 sampling-theory bound.
         (mean_a - mean_b) / sqrt((var_a + var_b) / 2.0), -0.0014424983240824225,
         abs((mean_a - mean_b) / sqrt((var_a + var_b) / 2.0) - -0.0014424983240824225) < 1e-6
    FROM namelen_stats
)
SELECT pin, computed, pinned, ok FROM checks ORDER BY pin;
