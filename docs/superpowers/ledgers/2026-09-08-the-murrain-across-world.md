# The Murrain Across Worlds — decision ledger

Campaign: **The Murrain Across Worlds** — durable follow-up prose for the
committed across-world population comparison after The Murrain landed on
`b096c7a7b`.

## Entries

#1 [G1] — **Does Lefford's across-world comparison require a new metric?** ·
**Decision: no; keep this follow-up prose-only.** The two committed 1,000-row
census CSVs already contain settlement-count, total-population,
mean-population, peoples-placed, and kobold-settlement-count, which are enough
to record the aggregate and per-world result. They do not contain drow, so the
seed-42 drow movement remains a local observation and no drow proxy or
across-world series is invented. · **Why:** Lefford's comparison is already a
measurement of committed artifacts, not a request to change the instrument;
the result's important distinction is redistribution across worlds and
peoples, not a missing epidemiology mechanism. · **Alternatives discarded:** a
new drow column (would require metric definitions, the census roster and
schema, a Sluice census on lefford, and artifact delivery before it could be
interpreted); a kobold-as-drow proxy (category error and especially unsafe with
small denominators); reopening The Murrain's population or disease model
(outside this follow-up's scope). · **Ideonomy: 1 pass (dimension-identification
and scale; no overturn; surfaced the decomposability boundary between
aggregate, per-world, seed-local, and people-level readings).** · **Capture:**
the durable comparison is in `book/src/chronicle/the-murrain.md`; any future
drow series is deferred as a separate measurement campaign.
