-- Filter template: "interesting" is a query, not a generation stage
-- (spec: the census is an unselected population sample). Edit freely.
SELECT seed, "star-class", "moons-admitted", "tidally-locked",
       "pantheon-size", "ocean-fraction", "settlement-count"
FROM "the-census"
WHERE CAST("moons-admitted" AS INTEGER) >= 2 AND "pantheon-size" >= 6
ORDER BY "settlement-count" DESC
LIMIT 25;
