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
| 2026-07-21T01:32:39Z | rebaseline | 215.415 | 238.161 | 6.334 | 1.13 | a47e10d0 | the-living-community | MacBookPro | 10 |
| 2026-07-21T01:53:27Z | rebaseline | 234.678 | 257.944 | 8.312 | 1.13 | a47e10d0 | the-living-community | MacBookPro | 10 |
| 2026-07-21T12:37:53Z | rebaseline | 237.938 | 260.758 | 5.634 | 1.12 | f682aee4 | the-living-community | MacBookPro | 10 |
| 2026-07-21T13:00:15Z | rebaseline | 243.534 | 261.259 | 6.157 | 1.10 | f682aee4 | the-living-community | MacBookPro | 10 |
