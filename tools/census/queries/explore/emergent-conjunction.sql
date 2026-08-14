-- The emergent conjunction: worlds where kobolds occupied a site, a ruin
-- exists, and tribute was paid -- and the world-time a replay must start from
-- to see all three. greatest() IS the maximum over three days; IS NOT NULL IS
-- the set intersection. This is the whole of TOOL-first-occurrence-index's
-- promise, expressed against the census rather than a bespoke index.
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
