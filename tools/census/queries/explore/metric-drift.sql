-- How one numeric metric's distribution moved across history epochs
-- (run history.sh first). Swap the metric name to track another.
-- Note: the brief's original alias was `at`, which DuckDB (v1.5.4) fails
-- to parse unquoted in ORDER BY (reserved-word collision, verified
-- against this build) — renamed to `since`.
SELECT epoch_label, min(commit_date) AS since,
       count(*) AS n, avg(value_num) AS mean,
       quantile_cont(value_num, 0.5) AS median
FROM census_history
WHERE metric = 'hypsometric-bimodality' AND study = 'the-census'
GROUP BY epoch_label
ORDER BY since;
