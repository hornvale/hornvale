-- The emergent conjunction: worlds where kobolds occupied a site, a ruin
-- exists, and tribute was paid -- and the world-time a replay must start from
-- to see all three. greatest() IS the maximum over three days; IS NOT NULL IS
-- the set intersection. This is the whole of TOOL-first-occurrence-index's
-- promise, expressed against the census rather than a bespoke index.
--
-- ============================================================================
-- TWO THINGS THE NEXT AUTHOR OF A CENSUS QUERY NEEDS (The Gnomon, 2026-08-14)
-- ============================================================================
--
-- 1. NOTHING REGISTERS A CENSUS QUERY. THIS FILE IS UNGATED.
--
--    `tools/census/manifest.json` enumerates *studies* (live, frozen,
--    sidecars) -- it has no queries key at all. `tools/census/check.sh` reads
--    exactly one query, by hard-coded name:
--
--        pins="$(duckdb "$db" -csv -c ".read tools/census/queries/calibrate/golden-pins.sql")"
--
--    So this file -- the campaign's headline demonstration, naming three
--    census columns by hand -- is referenced by no gate, no test, and no
--    manifest. Rename or re-epoch a `first-day-*` column and this query breaks
--    silently while the book page that describes it keeps describing it. If
--    you add a query under `explore/`, assume the same: it is documentation
--    that happens to be executable, not a checked artifact.
--
-- 2. duckdb's greatest() IS NULL-TOLERANT, UNLIKE STRICT SQL.
--
--    In duckdb, greatest(a, b, NULL) returns the max of the non-NULL
--    arguments rather than NULL. Measured, not assumed:
--
--        $ duckdb -c "SELECT greatest(1, 5, NULL) AS g, greatest(NULL, NULL) AS all_null;"
--        g = 5, all_null = NULL
--
--    (all-NULL is still NULL; a single non-NULL argument wins.) That is
--    harmless HERE only because the three
--    IS NOT NULL guards in the WHERE clause make it moot -- by the time a row
--    reaches the projection, none of the three can be NULL. Copy the
--    greatest() pattern WITHOUT copying those guards and the query silently
--    stops meaning "the intersection": a world holding only one of the three
--    predicates would return that one day as its `replay_from`, and a replay
--    started there would never show the conjunction. The IS NOT NULL clauses
--    are load-bearing semantics, not defensive noise.
SELECT seed,
       greatest("first-day-occ-people-kobold",
                "first-day-is-ruin",
                "first-day-pays-tribute-to") AS replay_from
FROM "the-census"
WHERE "first-day-occ-people-kobold" IS NOT NULL
  AND "first-day-is-ruin"           IS NOT NULL
  AND "first-day-pays-tribute-to"   IS NOT NULL
ORDER BY replay_from
LIMIT 25;
