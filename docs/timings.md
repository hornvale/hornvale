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

One `gate` row on `campaign/the-repertoire` (2026-08-01, wall 263.699s) was
not a deliberate milestone run: composing a commit message with a heredoc
containing backticks let the shell expand them as command substitution,
which executed a real `make gate` as a side effect. It is kept rather than
deleted — it agrees with the same branch's deliberate `gate` row to within
one percent, so it is a genuine measurement, and quietly dropping a real
result would be the less honest edit. Read it as an accidental but valid
sample, not as a second deliberate run.

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
| 2026-07-30T21:25:38Z | rebaseline | 670.039 | 1024.521 | 34.061 | 1.58 | 0 | 4596cb05 | the-pigment | lefford | 40 |
| 2026-07-30T22:58:12Z | rebaseline | 488.736 | 710.178 | 39.216 | 1.53 | 0 | 706e053f | the-pigment | lefford | 40 |
| 2026-07-30T23:25:23Z | ci | 775.605 | 21006.053 | 201.175 | 27.34 | 0 | 9ddccf35 | the-pigment | lefford | 40 |
| 2026-07-31T00:44:59Z | heavy | 8187.657 | 9999.679 | 29.486 | 1.22 | 0 | 7842ca07 | the-winnowing | lefford | 40 |
| 2026-07-31T01:07:51Z | ci | 998.788 | 22285.706 | 203.889 | 22.52 | 0 | 9ddccf35 | the-pigment | lefford | 40 |
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
| 2026-07-31T16:04:22Z | census | 607.287 | 12173.083 | 342.523 | 20.61 | 0 | 59d23fdd |  | lefford | 40 |
| 2026-07-31T16:49:52Z | rebaseline | 159.194 | 166.875 | 9.041 | 1.11 | 0 | da2c252f | the-watershed | MacBookPro | 10 |
| 2026-07-31T16:56:15Z | gate | 346.029 | 2832.863 | 61.301 | 8.36 | 0 | 54288ab9 | the-watershed | MacBookPro | 10 |
| 2026-07-31T18:15:42Z | gate | 1.053 | 0.814 | 0.093 | 0.86 | 0 | 0d6544f8 | campaign/the-vernacular | ambrose | 12 |
| 2026-07-31T18:16:07Z | gate | 12.860 | 34.609 | 10.772 | 3.53 | 0 | 0d6544f8 | campaign/the-vernacular | ambrose | 12 |
| 2026-07-31T18:22:53Z | gate | 378.851 | 2889.848 | 120.752 | 7.95 | 0 | 0d6544f8 | campaign/the-vernacular | ambrose | 12 |
| 2026-07-31T18:26:43Z | rebaseline | 132.869 | 127.977 | 4.155 | 0.99 | 0 | 0d6544f8 | campaign/the-vernacular | ambrose | 12 |
| 2026-07-31T18:50:12Z | rebaseline | 117.128 | 129.623 | 4.007 | 1.14 | 0 | 2c651f7b | campaign/the-vernacular | ambrose | 12 |
| 2026-07-31T19:17:08Z | gate | 373.903 | 2861.177 | 112.684 | 7.95 | 0 | 5f00b398 | campaign/the-vernacular | ambrose | 12 |
| 2026-07-31T19:38:40Z | gate | 6.211 | 18.605 | 10.473 | 4.68 | 0 | 91e669a9 | campaign/the-vernacular | ambrose | 12 |
| 2026-07-31T19:45:21Z | gate | 382.775 | 2836.830 | 123.900 | 7.73 | 0 | 91e669a9 | campaign/the-vernacular | ambrose | 12 |
| 2026-07-31T19:47:48Z | rebaseline | 107.687 | 127.775 | 3.601 | 1.22 | 0 | 91e669a9 | campaign/the-vernacular | ambrose | 12 |
| 2026-07-31T20:19:29Z | gate | 407.296 | 2795.066 | 108.228 | 7.13 | 0 | f61060e9 | campaign/the-vernacular | ambrose | 12 |
| 2026-07-31T21:15:55Z | gate | 348.167 | 2776.714 | 109.211 | 8.29 | 0 | 9ef8cd33 | campaign/the-vernacular | ambrose | 12 |
| 2026-07-31T21:18:26Z | rebaseline | 105.200 | 127.239 | 3.950 | 1.25 | 0 | 9ef8cd33 | campaign/the-vernacular | ambrose | 12 |
| 2026-07-31T21:35:58Z | gate | 328.230 | 2770.509 | 83.040 | 8.69 | 0 | da36a86a | campaign/the-vernacular | ambrose | 12 |
| 2026-07-31T22:36:12Z | gate | 1.038 | 0.809 | 0.095 | 0.87 | 0 | 9351a4d9 | campaign/the-vernacular | ambrose | 12 |
| 2026-07-31T22:37:39Z | gate | 75.957 | 71.859 | 64.330 | 1.79 | 0 | 9351a4d9 | campaign/the-vernacular | ambrose | 12 |
| 2026-07-31T22:40:21Z | gate | 91.914 | 146.866 | 68.784 | 2.35 | 0 | 9351a4d9 | campaign/the-vernacular | ambrose | 12 |
| 2026-07-31T22:48:00Z | gate | 312.824 | 2727.291 | 66.302 | 8.93 | 0 | 9351a4d9 | campaign/the-vernacular | ambrose | 12 |
| 2026-07-31T22:50:24Z | rebaseline | 105.470 | 128.790 | 4.020 | 1.26 | 0 | 9351a4d9 | campaign/the-vernacular | ambrose | 12 |
| 2026-07-31T23:04:01Z | nav-bench | 414.373 | 413.773 | 0.552 | 1.00 | 0 | 7919beb3 | the-waymark | lefford | 40 |
| 2026-07-31T23:31:48Z | gate | 254.061 | 7088.980 | 276.690 | 28.99 | 0 | 2e8861a5 | the-waymark | lefford | 40 |
| 2026-08-01T00:25:23Z | gate | 251.868 | 6960.325 | 296.487 | 28.81 | 0 | e977941d | the-waymark | lefford | 40 |
| 2026-08-01T00:29:32Z | ci | 215.852 | 6768.620 | 188.571 | 32.23 | 0 | b7ce5941 | the-waymark | lefford | 40 |
| 2026-08-01T03:52:06Z | gate | 374.547 | 2892.149 | 101.340 | 7.99 | 0 | e24c65a5 | campaign/the-repertoire | ambrose | 12 |
| 2026-08-01T05:18:24Z | rebaseline | 96.421 | 113.512 | 2.595 | 1.20 | 0 | 50e13e8f | campaign/the-repertoire | ambrose | 12 |
| 2026-08-01T05:20:46Z | rebaseline | 95.858 | 113.369 | 2.643 | 1.21 | 0 | 50e13e8f | campaign/the-repertoire | ambrose | 12 |
| 2026-08-01T05:26:36Z | gate | 263.699 | 2725.541 | 46.732 | 10.51 | 0 | 50e13e8f | campaign/the-repertoire | ambrose | 12 |
| 2026-08-01T05:32:06Z | rebaseline | 97.246 | 114.146 | 2.506 | 1.20 | 0 | 88b11b45 | campaign/the-repertoire | ambrose | 12 |
| 2026-08-01T05:37:03Z | gate | 266.087 | 2779.809 | 45.562 | 10.62 | 0 | 61fcc2f7 | campaign/the-repertoire | ambrose | 12 |
| 2026-08-01T05:58:21Z | rebaseline | 95.951 | 113.646 | 2.623 | 1.21 | 0 | d627db62 | campaign/the-repertoire | ambrose | 12 |
| 2026-08-01T06:24:49Z | gate | 261.089 | 2717.272 | 44.759 | 10.58 | 0 | 8b4927e5 | campaign/the-repertoire | ambrose | 12 |
| 2026-08-01T06:47:20Z | rebaseline | 97.617 | 113.345 | 2.666 | 1.19 | 0 | 813b6e22 | campaign/the-repertoire | ambrose | 12 |
| 2026-08-01T06:59:35Z | gate | 297.581 | 2616.119 | 95.368 | 9.11 | 0 | 021a005b | campaign/the-repertoire | ambrose | 12 |
| 2026-08-01T07:23:17Z | gate | 236.467 | 2428.581 | 45.044 | 10.46 | 0 | f372fdcd | campaign/the-repertoire | ambrose | 12 |
| 2026-08-01T13:44:06Z | gate | 276.853 | 2318.461 | 65.112 | 8.61 | 0 | 84e5c617 | campaign/the-repertoire | ambrose | 12 |
| 2026-08-01T20:53:26Z | gate | 291.538 | 2481.986 | 82.033 | 8.79 | 0 | 55f998eb | campaign/the-scaffold | ambrose | 12 |
| 2026-08-01T21:19:49Z | gate | 355.136 | 2754.334 | 59.809 | 7.92 | 0 | 606bf5e1 | the-commonplace | MacBookPro | 10 |
| 2026-08-01T21:26:22Z | gate | 303.998 | 2324.708 | 87.210 | 7.93 | 0 | 3a567d76 | campaign/the-scaffold | ambrose | 12 |
| 2026-08-01T21:48:20Z | gate | 315.739 | 2336.680 | 97.580 | 7.71 | 0 | 4c773a21 | campaign/the-scaffold | ambrose | 12 |
| 2026-08-01T22:01:52Z | rebaseline | 108.912 | 123.203 | 3.773 | 1.17 | 0 | 65110815 | campaign/the-scaffold | ambrose | 12 |
| 2026-08-01T22:11:43Z | gate | 302.570 | 2351.425 | 101.294 | 8.11 | 0 | 65110815 | campaign/the-scaffold | ambrose | 12 |
| 2026-08-01T22:13:44Z | gate | 438.898 | 2540.931 | 123.370 | 6.07 | 0 | 9eb73b5e | main | MacBookPro | 10 |
| 2026-08-01T22:16:04Z | gate | 253.264 | 2315.880 | 57.179 | 9.37 | 0 | 65110815 | campaign/the-scaffold | ambrose | 12 |
| 2026-08-01T22:47:16Z | gate | 315.156 | 2365.518 | 91.575 | 7.80 | 0 | 44420712 | campaign/the-scaffold | ambrose | 12 |
| 2026-08-01T22:56:19Z | gate | 323.997 | 2389.446 | 92.605 | 7.66 | 0 | 73b14431 | campaign/the-scaffold | ambrose | 12 |
| 2026-08-01T23:39:02Z | gate | 165.192 | 456.693 | 109.433 | 3.43 | 0 | ce912d50 | campaign/the-vernacular | ambrose | 12 |
| 2026-08-01T23:41:31Z | rebaseline | 127.301 | 131.065 | 4.400 | 1.06 | 0 | ce912d50 | campaign/the-vernacular | ambrose | 12 |
| 2026-08-01T23:47:03Z | gate | 292.867 | 2500.848 | 70.963 | 8.78 | 0 | ce912d50 | campaign/the-vernacular | ambrose | 12 |
| 2026-08-02T00:00:11Z | gate | 353.287 | 2618.048 | 135.460 | 7.79 | 0 | 203e68b8 | campaign/the-vernacular | ambrose | 12 |
| 2026-08-02T00:04:25Z | rebaseline | 121.411 | 127.804 | 4.328 | 1.09 | 0 | 203e68b8 | campaign/the-vernacular | ambrose | 12 |
| 2026-08-02T00:09:43Z | gate | 274.552 | 2463.049 | 66.328 | 9.21 | 0 | 203e68b8 | campaign/the-vernacular | ambrose | 12 |
| 2026-08-02T00:20:16Z | gate | 286.565 | 2455.237 | 70.527 | 8.81 | 0 | 3bed59f4 | campaign/the-vernacular | ambrose | 12 |
| 2026-08-02T00:22:32Z | rebaseline | 109.913 | 127.914 | 4.360 | 1.20 | 0 | 3bed59f4 | campaign/the-vernacular | ambrose | 12 |
| 2026-08-02T00:52:24Z | gate | 394.912 | 2572.374 | 153.079 | 6.90 | 0 | 46f16fd8 | campaign/the-vernacular | ambrose | 12 |
| 2026-08-02T00:54:38Z | rebaseline | 109.987 | 128.565 | 4.310 | 1.21 | 0 | 46f16fd8 | campaign/the-vernacular | ambrose | 12 |
| 2026-08-02T01:12:58Z | gate | 12.560 | 15.810 | 31.212 | 3.74 | 0 | 3c09ec47 | campaign/the-vernacular | ambrose | 12 |
| 2026-08-02T01:20:08Z | gate | 367.005 | 2514.518 | 116.943 | 7.17 | 0 | 3c09ec47 | campaign/the-vernacular | ambrose | 12 |
| 2026-08-02T01:36:47Z | gate | 386.484 | 2510.414 | 151.339 | 6.89 | 0 | 08b70ba8 | campaign/the-vernacular | ambrose | 12 |
| 2026-08-02T02:15:58Z | gate | 421.786 | 2587.536 | 144.976 | 6.48 | 0 | b6d65ccf | campaign/the-vernacular | ambrose | 12 |
| 2026-08-02T12:23:50Z | gate | 393.872 | 2483.160 | 85.322 | 6.52 | 0 | 1c4677c2 | campaign/the-vernacular | ambrose | 12 |
| 2026-08-02T12:25:55Z | rebaseline | 124.393 | 134.577 | 4.842 | 1.12 | 0 | 1c4677c2 | campaign/the-vernacular | ambrose | 12 |
| 2026-08-02T01:11:10Z | rebaseline | 241.888 | 611.368 | 38.413 | 2.69 | 0 | 6ed8473a | the-pigment | lefford | 40 |
| 2026-08-02T01:16:36Z | gate | 264.054 | 7510.613 | 313.009 | 29.63 | 0 | 44b4aa11 | the-pigment | lefford | 40 |
| 2026-08-02T01:22:48Z | gate | 243.303 | 6945.752 | 217.844 | 29.44 | 0 | f51b5c42 | the-pigment | lefford | 40 |
| 2026-08-02T01:26:12Z | rebaseline | 203.257 | 315.252 | 25.053 | 1.67 | 0 | f51b5c42 | the-pigment | lefford | 40 |
| 2026-08-02T03:29:08Z | gate | 423.840 | 2717.003 | 102.811 | 6.65 | 0 | f449ea1c | the-witness | MacBookPro | 10 |
| 2026-08-02T05:46:34Z | census | 581.789 | 12265.353 | 332.049 | 21.65 | 0 | f449ea1c |  | lefford | 40 |
| 2026-08-02T06:19:54Z | gate | 316.247 | 2520.230 | 60.483 | 8.16 | 0 | 789ec2fc | the-witness | MacBookPro | 10 |
| 2026-08-02T12:41:22Z | rebaseline | 163.952 | 170.576 | 8.626 | 1.09 | 0 | 240adaf4 | the-contour | MacBookPro | 10 |
| 2026-08-02T12:47:05Z | gate | 149.187 | 653.434 | 55.191 | 4.75 | 0 | 240adaf4 | the-contour | MacBookPro | 10 |
| 2026-08-02T13:32:47Z | census | 789.248 | 17501.605 | 344.865 | 22.61 | 0 | b1f5d7ac |  | lefford | 40 |
| 2026-08-02T13:33:40Z | gate | 17.045 | 45.665 | 3.201 | 2.87 | 0 | 4c46b45e | the-contour | MacBookPro | 10 |
| 2026-08-02T13:38:14Z | gate | 89.200 | 604.913 | 19.963 | 7.01 | 0 | 4c46b45e | the-contour | MacBookPro | 10 |
| 2026-08-02T14:16:54Z | gate | 150.033 | 1050.166 | 42.860 | 7.29 | 0 | 4c46b45e | the-contour | MacBookPro | 10 |
| 2026-08-02T14:29:10Z | rebaseline | 152.239 | 169.408 | 8.307 | 1.17 | 0 | ef29ef10 | the-contour | MacBookPro | 10 |
| 2026-08-02T14:32:58Z | rebaseline | 148.002 | 170.209 | 8.789 | 1.21 | 0 | ef29ef10 | the-contour | MacBookPro | 10 |
| 2026-08-02T15:19:12Z | census | 672.082 | 17206.211 | 318.107 | 26.07 | 0 | c55005ed |  | lefford | 40 |
| 2026-08-02T15:22:20Z | rebaseline | 131.340 | 127.157 | 6.974 | 1.02 | 0 | 488de111 | the-contour | MacBookPro | 10 |
| 2026-08-02T15:38:43Z | gate | 84.851 | 121.833 | 55.551 | 2.09 | 0 | 488de111 | the-contour | MacBookPro | 10 |
| 2026-08-02T15:44:42Z | rebaseline | 105.733 | 128.681 | 6.753 | 1.28 | 0 | 488de111 | the-contour | MacBookPro | 10 |
| 2026-08-02T15:45:37Z | gate | 32.083 | 172.515 | 6.909 | 5.59 | 0 | 488de111 | the-contour | MacBookPro | 10 |
| 2026-08-02T16:40:05Z | gate | 67.331 | 181.264 | 23.614 | 3.04 | 0 | 01c7b930 | the-contour | MacBookPro | 10 |
| 2026-08-02T17:01:58Z | gate | 92.777 | 177.493 | 6.316 | 1.98 | 0 | acdbec69 | the-contour | MacBookPro | 10 |
| 2026-08-02T17:26:19Z | gate | 451.660 | 2250.323 | 122.717 | 5.25 | 0 | 3a2cd0d5 | the-contour | MacBookPro | 10 |
| 2026-08-02T12:37:39Z | rebaseline | 176.673 | 140.571 | 5.615 | 0.83 | 0 | d88dbe70 | main | ambrose | 12 |
| 2026-08-02T12:47:25Z | gate | 574.814 | 2990.483 | 149.911 | 5.46 | 0 | d88dbe70 | main | ambrose | 12 |
| 2026-08-02T12:55:47Z | gate | 395.612 | 2531.925 | 85.499 | 6.62 | 0 | d88dbe70 | main | ambrose | 12 |
| 2026-08-02T14:46:27Z | rebaseline | 130.267 | 133.842 | 4.951 | 1.07 | 0 | 6db788ec | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-02T15:09:52Z | rebaseline | 143.921 | 140.007 | 5.216 | 1.01 | 0 | 385b1c27 | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-02T15:11:36Z | gate | 71.230 | 136.505 | 29.064 | 2.32 | 0 | 385b1c27 | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-02T23:19:08Z | gate | 371.887 | 2519.152 | 83.049 | 7.00 | 0 | 385b1c27 | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-02T23:21:30Z | rebaseline | 131.947 | 142.856 | 5.404 | 1.12 | 0 | 385b1c27 | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-02T23:40:33Z | rebaseline | 130.931 | 133.092 | 4.856 | 1.05 | 0 | b7d4a6de | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-02T23:42:31Z | gate | 88.366 | 154.116 | 46.273 | 2.27 | 0 | b7d4a6de | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-02T23:48:25Z | gate | 309.092 | 2519.270 | 77.731 | 8.40 | 0 | b7d4a6de | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T00:03:54Z | gate | 374.215 | 2576.936 | 107.454 | 7.17 | 0 | 7116a436 | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T00:05:56Z | rebaseline | 112.039 | 131.998 | 4.486 | 1.22 | 0 | 7116a436 | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T00:28:14Z | gate | 361.013 | 2564.540 | 121.101 | 7.44 | 0 | ad5f9d03 | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T00:54:29Z | gate | 373.978 | 2583.378 | 127.383 | 7.25 | 0 | 78267f7a | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-02T17:33:10Z | rebaseline | 143.444 | 130.881 | 7.494 | 0.96 | 0 | 84632ff8 | the-contour | MacBookPro | 10 |
| 2026-08-02T17:46:25Z | gate | 287.025 | 2200.520 | 58.245 | 7.87 | 0 | 84632ff8 | the-contour | MacBookPro | 10 |
| 2026-08-02T05:33:24Z | heavy | 7388.713 | 10218.680 | 176.483 | 1.41 | 0 | f449ea1c |  | lefford | 40 |
| 2026-08-03T01:04:00Z | gate | 359.826 | 2354.047 | 126.893 | 6.89 | 0 | da08ae07 | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T01:10:16Z | rebaseline | 85.705 | 105.542 | 4.535 | 1.28 | 0 | da08ae07 | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T12:06:55Z | gate | 353.064 | 2217.949 | 113.713 | 6.60 | 0 | d72654bc | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T12:31:36Z | gate | 398.198 | 2365.153 | 130.633 | 6.27 | 0 | b5a64014 | main | ambrose | 12 |
| 2026-08-03T13:09:47Z | gate | 447.046 | 2460.006 | 149.739 | 5.84 | 0 | 14e0f4df | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T13:11:50Z | rebaseline | 86.936 | 105.181 | 4.594 | 1.26 | 0 | 14e0f4df | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T13:35:15Z | gate | 417.036 | 2253.607 | 148.049 | 5.76 | 0 | 8ee274be | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T13:37:43Z | rebaseline | 87.723 | 105.616 | 4.555 | 1.26 | 0 | 8ee274be | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T14:19:49Z | gate | 418.119 | 2305.281 | 150.085 | 5.87 | 0 | 81307b7c | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T14:21:34Z | rebaseline | 88.214 | 105.996 | 4.365 | 1.25 | 0 | 81307b7c | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T14:41:33Z | gate | 435.769 | 2252.489 | 157.858 | 5.53 | 0 | 4c132f60 | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T14:43:21Z | rebaseline | 92.406 | 104.754 | 4.997 | 1.19 | 0 | 4c132f60 | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T15:08:05Z | gate | 5.885 | 7.693 | 4.640 | 2.10 | 0 | 7feabdd2 | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T15:10:50Z | gate | 150.221 | 180.013 | 87.814 | 1.78 | 0 | 7feabdd2 | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T15:17:09Z | gate | 361.491 | 2215.768 | 84.826 | 6.36 | 0 | 7feabdd2 | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T15:18:55Z | rebaseline | 92.227 | 107.538 | 4.931 | 1.22 | 0 | 7feabdd2 | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T15:41:18Z | gate | 407.858 | 2240.789 | 129.681 | 5.81 | 0 | 6c3df999 | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T15:43:13Z | rebaseline | 98.014 | 109.120 | 5.325 | 1.17 | 0 | 6c3df999 | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T16:05:44Z | gate | 539.472 | 2403.735 | 202.835 | 4.83 | 0 | 7e1613db | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T16:07:46Z | rebaseline | 98.626 | 107.785 | 5.348 | 1.15 | 0 | 7e1613db | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T16:45:49Z | gate | 486.257 | 2247.662 | 182.691 | 5.00 | 0 | 5b54c1ec | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T16:47:37Z | rebaseline | 91.675 | 106.154 | 4.580 | 1.21 | 0 | 5b54c1ec | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T17:20:33Z | rebaseline | 120.573 | 106.176 | 4.793 | 0.92 | 0 | a3944ddf | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T17:25:08Z | gate | 249.584 | 545.935 | 158.288 | 2.82 | 0 | a3944ddf | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T17:32:35Z | gate | 398.589 | 2263.361 | 143.838 | 6.04 | 0 | a3944ddf | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T17:48:25Z | rebaseline | 118.045 | 107.180 | 4.954 | 0.95 | 0 | dbedf7f2 | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T17:51:51Z | rebaseline | 90.124 | 105.805 | 4.854 | 1.23 | 0 | dbedf7f2 | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T18:03:44Z | gate | 468.810 | 2290.052 | 194.046 | 5.30 | 0 | dbedf7f2 | campaign/the-vernacular-3 | ambrose | 12 |
