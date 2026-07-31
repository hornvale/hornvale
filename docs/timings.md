# Timing ledger

Append-only record of expensive runs — full-fixture regens, censuses, full
gates — so runtime creep is visible *before* it forces a scramble (the suite
instruments the world but never watched its own wall time). One row per
deliberate milestone run, written by [`scripts/timed.sh`](../scripts/timed.sh)
(`make timings` to view). Times are machine- and load-specific — read
`host`/`cores`/`cpu_ratio`, not the raw seconds, across different machines.
`cpu_ratio = (user+sys)/wall` ≈ parallelism achieved: it separates *more work*
(user climbs) from *more contention* (wall climbs, ratio falls). `waited_s` is
time spent QUEUED behind another heavy run (decision 0081), not work — it
separates a *queued* run from a *slow* one. Rows predating the column simply
lack it; this file is not drift-checked, so history is left as it was. This file is
NOT drift-checked and never gates the build; it is a record you read.

**The build-failing half now exists, and it is a different file.** The
Timekeeper (decision
[0088](decisions/0088-the-suite-watches-its-own-clock.md)) gave the suite a
per-test clock: `make ci` runs the workspace under the `ci` nextest profile,
compares every test against a committed per-host baseline at
`docs/timings/test-baseline-<host>.tsv`, fails on a per-test shift (≥ 5 s and
> 2× recorded) or a whole-suite shift (> 25 % on the id intersection), and only
then rewrites the baseline. A red run never records. That file is the one to
read with `git log -p`; *this* one keeps the wall time of each deliberate run,
including `make ci` itself under the label `ci`.

The first row is backfilled by hand from the fast-gate-tiers investigation
(2026-07-13): the pre-tiering `cargo test --workspace` on an M1 Max under
~8 parallel sessions — the 43.5-min worst case that motivated the tiering.
Its low `cpu_ratio` (3.6 on a 10-core box) is the contention signature.

One label changed what it measures, and the rows must not be read as a trend.
`scene-profile` ran one traversal of the scene workload at The Sextant
(2026-07-28, 8.3 s). At The Cistern (2026-07-29) the profiler became a
**two-pass** instrument — the `&World` path and the `SceneContext` path in a
single run, so the before and after share a box, a build and a world — which
roughly doubles its wall time (16.0 s) while the thing it measures got about
eleven times cheaper. The campaign's number is the ratio the run *prints*, not
the wall time of the run.

| when (UTC) | label | wall_s | user_s | sys_s | cpu_ratio | waited_s | commit | branch | host | cores |
|---|---|---|---|---|---|---|---|---|---|---|
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
| 2026-07-22T06:41:12Z | rebaseline | 275.388 | 305.336 | 9.701 | 1.14 | f1dbfe8b | the-thoroughfare | MacBookPro | 10 |
| 2026-07-22T07:03:51Z | rebaseline | 434.524 | 366.639 | 9.281 | 0.87 | d0e62a58 | the-thoroughfare | MacBookPro | 10 |
| 2026-07-22T18:06:50Z | rebaseline | 676.481 | 466.116 | 14.044 | 0.71 | 61ebb6b8 | the-sundering | MacBookPro | 10 |
| 2026-07-25T02:09:00Z | rebaseline | 412.243 | 492.346 | 19.681 | 1.24 | f6a0fdda | the-vigil | MacBookPro | 10 |
| 2026-07-25T23:59:49Z | rebaseline | 352.919 | 429.656 | 19.519 | 1.27 | 2b63c488 | the-vigil | MacBookPro | 10 |
| 2026-07-26T02:21:38Z | rebaseline | 197.299 | 241.899 | 11.853 | 1.29 | 58f70b3f | the-vigil | MacBookPro | 10 |
| 2026-07-26T20:08:18Z | rebaseline | 203.599 | 247.521 | 12.502 | 1.28 | 1095ebc9 | the-waterline | MacBookPro | 10 |
| 2026-07-28T01:22:59Z | census | 596.284 | 9213.428 | 291.955 | 15.94 | 0 | 39abfeae | the-turnstile | lefford | 40 |
| 2026-07-28T13:09:03Z | census | 1098.866 | 12870.156 | 272.621 | 11.96 | 0 | 7e23dd49 |  | lefford | 40 |
| 2026-07-28T19:12:34Z | rebaseline | 294.969 | 304.818 | 10.094 | 1.07 | 0 | 346fd2ee | main | MacBookPro | 10 |
| 2026-07-28T19:55:30Z | scene-profile | 8.322 | 8.098 | 0.224 | 1.00 | 0 | ef762ca6 | the-sextant | lefford | 40 |
| 2026-07-29T01:26:45Z | census | 865.165 | 11131.688 | 203.931 | 13.10 | 0 | f2bfd829 | the-wearing | lefford | 40 |
| 2026-07-29T02:33:03Z | census | 878.899 | 12427.595 | 295.149 | 14.48 | 0 | 03edfe6b | the-toponym | lefford | 40 |
| 2026-07-29T03:25:05Z | census | 1026.585 | 11155.518 | 164.057 | 11.03 | 0 | 3e9d2ad5 | the-wearing | lefford | 40 |
| 2026-07-29T03:46:29Z | census | 1283.922 | 12240.453 | 150.414 | 9.65 | 365 | 03edfe6b | the-toponym | lefford | 40 |
| 2026-07-29T05:08:44Z | scene-profile | 16.028 | 15.646 | 0.351 | 1.00 | 0 | 3944ff02 | the-cistern | lefford | 40 |
| 2026-07-29T05:20:44Z | rebaseline | 698.257 | 834.569 | 16.261 | 1.22 | 0 | 3944ff02 | the-cistern | lefford | 40 |
| 2026-07-29T14:01:37Z | rebaseline | 616.116 | 726.715 | 35.219 | 1.24 | 0 | a0690132 | the-cistern | lefford | 40 |
| 2026-07-29T15:53:41Z | rebaseline | 295.277 | 307.249 | 10.248 | 1.08 | 0 | 6a5b3697 | the-running-head | MacBookPro | 10 |
| 2026-07-29T16:57:36Z | scene-profile | 22.666 | 22.410 | 0.227 | 1.00 | 0 | f4f20e30 | the-winnowing | lefford | 40 |
| 2026-07-29T18:38:05Z | rebaseline | 659.510 | 793.420 | 45.868 | 1.27 | 0 | f0aaef15 | the-watershed | lefford | 40 |
| 2026-07-29T19:36:34Z | rebaseline | 496.760 | 610.372 | 32.500 | 1.29 | 0 | f0aaef15 | the-watershed | lefford | 40 |
| 2026-07-30T00:15:04Z | census | 828.715 | 12514.555 | 322.431 | 15.49 | 0 | a1d65542 | main | lefford | 40 |
| 2026-07-30T17:24:01Z | ci | 986.349 | 7946.701 | 190.717 | 8.25 | 0 | babddc97 | main | MacBookPro | 10 |
| 2026-07-30T17:46:26Z | ci | 958.088 | 7962.323 | 181.286 | 8.50 | 0 | 1f862cde | main | MacBookPro | 10 |
| 2026-07-30T18:42:10Z | ci | 757.854 | 20840.419 | 216.350 | 27.78 | 0 | 6807a9f2 | the-pigment | lefford | 40 |
| 2026-07-30T19:20:08Z | census | 837.165 | 12726.296 | 310.796 | 15.57 | 0 | 7f9942dc | followups-post-the-wearing | lefford | 40 |
| 2026-07-31T00:44:59Z | heavy | 8187.657 | 9999.679 | 29.486 | 1.22 | 0 | 7842ca07 | the-winnowing | lefford | 40 |
| 2026-07-31T01:24:01Z | rebaseline | 637.032 | 345.043 | 13.042 | 0.56 | 0 | 09d1855c | the-watershed | MacBookPro | 10 |
| 2026-07-31T03:37:40Z | gate | 1761.518 | 8076.942 | 187.467 | 4.69 | 0 | 6be77e8e | gate-timings | MacBookPro | 10 |
| 2026-07-31T07:03:18Z | gate | 593.481 | 12089.663 | 242.876 | 20.78 | 0 | d3c88e35 | the-shuttle | lefford | 40 |
| 2026-07-31T08:25:13Z | gate | 500.934 | 10363.553 | 264.362 | 21.22 | 0 | b57ec101 | the-shuttle | lefford | 40 |
| 2026-07-31T08:35:20Z | ci | 477.447 | 10217.321 | 191.838 | 21.80 | 0 | 133b1e48 | the-shuttle | lefford | 40 |
| 2026-07-31T13:38:43Z | rebaseline | 492.647 | 355.233 | 11.716 | 0.74 | 0 | 322ae369 | the-watershed | MacBookPro | 10 |
| 2026-07-31T13:56:50Z | gate | 445.307 | 8019.569 | 265.280 | 18.60 | 0 | 927ae26d | the-weir | lefford | 40 |
| 2026-07-31T14:46:46Z | rebaseline | 214.365 | 364.381 | 33.316 | 1.86 | 0 | 28ac6058 | the-weir | lefford | 40 |
| 2026-07-31T15:00:18Z | gate | 417.510 | 7535.285 | 232.015 | 18.60 | 0 | 23f454ba | the-weir | lefford | 40 |
| 2026-07-31T15:20:47Z | gate | 402.103 | 7445.289 | 187.380 | 18.98 | 0 | b2294dbc | the-weir | lefford | 40 |
| 2026-07-31T15:28:08Z | ci | 396.822 | 7454.111 | 188.653 | 19.26 | 0 | 4126ccde | the-weir | lefford | 40 |
