# Timing ledger

Append-only record of expensive runs — full-fixture regens, censuses, full
gates — so runtime creep is visible *before* it forces a scramble (the suite
instruments the world but never watched its own wall time). One row per
deliberate milestone run, written by [`scripts/timed.sh`](../scripts/timed.sh)
(`make timings` to view). Times are machine- and load-specific — read
`host`/`cores`/`cpu_ratio`, not the raw seconds, across different machines.
`cpu_ratio = (user+sys)/wall` ≈ parallelism achieved: it separates *more work*
(user climbs) from *more contention* (wall climbs, ratio falls). This file is
NOT drift-checked and never gates the build; it is a record you read. The
build-failing tolerance-band version is a later step (`TOOL-suite-timing-ledger`).

The first row is backfilled by hand from the fast-gate-tiers investigation
(2026-07-13): the pre-tiering `cargo test --workspace` on an M1 Max under
~8 parallel sessions — the 43.5-min worst case that motivated the tiering.
Its low `cpu_ratio` (3.6 on a 10-core box) is the contention signature.

| when (UTC) | label | wall_s | user_s | sys_s | cpu_ratio | commit | branch | host | cores |
|---|---|---|---|---|---|---|---|---|---|
| 2026-07-13T00:00:00Z | suite-full (pre-tiering, backfilled) | 2610.89 | 9246.93 | 36.88 | 3.56 | a2d39fa | main | m1max | 10 |
| 2026-07-13T22:49Z | regen-remote: census-as-data (the-census 1000 + meeting; box) | 1803 | ? | ? | ? | 9643ef5 | census-as-data | aws-c7a.16xlarge-spot | 64 |
| 2026-07-14T22:14:35Z | rebaseline | 202.305 | 146.973 | 1.384 | 0.73 | 90b7f96 | sculpting | MacBookPro | 10 |
| 2026-07-19T17:31:57Z | rebaseline | 216.546 | 800.341 | 10.129 | 3.74 | 7b65382 | the-rains | MacBookPro | 10 |
| 2026-07-19T17:38:14Z | rebaseline | 256.409 | 836.827 | 10.433 | 3.30 | e198adb | the-rains | MacBookPro | 10 |
| 2026-07-21T04:53:14Z | rebaseline | 171.711 | 198.154 | 5.262 | 1.18 | 834e7814 | lang-49 | MacBookPro | 10 |
| 2026-07-21T11:12:16Z | rebaseline | 171.442 | 198.615 | 4.648 | 1.19 | edf3da33 | the-freshwater | MacBookPro | 10 |
| 2026-07-21T16:03:30Z | rebaseline | 372.962 | 320.950 | 9.349 | 0.89 | 4d9e996b | the-living-community | MacBookPro | 10 |
| 2026-07-21T19:17:42Z | rebaseline | 321.622 | 309.111 | 7.489 | 0.98 | dacb158e | the-living-community | MacBookPro | 10 |
| 2026-07-21T19:23:46Z | rebaseline | 6.462 | 17.327 | 3.590 | 3.24 | 0887f41a | the-living-community | MacBookPro | 10 |
| 2026-07-21T19:35:11Z | rebaseline | 302.023 | 309.906 | 8.266 | 1.05 | dc97bec7 | the-living-community | MacBookPro | 10 |
| 2026-07-22T06:06:04Z | rebaseline | 274.547 | 304.998 | 9.886 | 1.15 | 0888f1e9 | the-thoroughfare | MacBookPro | 10 |
