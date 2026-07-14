-- The biography of one world: every metric of one seed, across every
-- committed snapshot of the canonical census (spec §3; run history.sh first).
-- Change the seed to follow a different world.
SELECT epoch_label, commit_date, metric,
       coalesce(CAST(value_num AS VARCHAR), value_text, CAST(value_flag AS VARCHAR)) AS value
FROM census_history
WHERE seed = 42 AND study = 'the-census'
ORDER BY metric, commit_date;
