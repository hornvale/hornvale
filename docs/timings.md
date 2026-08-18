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

Since The Sexton the ledger covers **every command that makes a human wait**,
not only the expensive milestone runs it was built for: `prewarm`, `preflight`,
`gate-fast`, `quick` and the three client checks record rows alongside `gate`,
`ci`, `rebaseline`, `census` and `heavy`. The reason is decision 0086's
amendment generalised — a cost with no label is invisible to every decision
about cost, and `prewarm` had zero rows against 73 branches in a month.

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

**A pre-commit `gate` row names the tree it gated, not `HEAD` at run time.**
`timed.sh` stamps `git rev-parse --short HEAD`, and a commit gate by definition
runs *before* the commit it clears — so the automatic stamp is always the
PARENT. Left alone that reads as the previous commit having been gated twice,
which is the opposite of what the ledger is for. So the `commit` column is
corrected by hand after the commit lands (The Sighting's two 2026-08-06 `gate`
rows were stamped `92177164`, Task 4's SHA, and name `21ff57b9` — the Task 5
tree they actually gated). The correction is to the LABEL, never to a measured
number: no wall, user, sys or ratio value in this file is ever edited.

**Which means each commit corrects the PREVIOUS round's rows**, and that is the
convention rather than a shortfall: a commit cannot contain its own hash, so a
run's row is corrected by the next commit that touches this file. A row still
carrying its parent's SHA is therefore the most recent run, not an oversight —
check the `when` column before assuming otherwise.

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

One `heavy` row is **reconstructed, not written by `timed.sh`**: the
2026-08-05 row at commit `7138ce75` (wall 2431.901s). The Scatter dispatched
two heavy runs an hour apart to choose between two scheduling shapes, and
`heavy-run.sh` writes this ledger inside the *shared* `hornvale-heavy-wt`
worktree — so the second dispatch's `reset --hard` discarded the first run's
ledger edit before anyone had committed it. The row is rebuilt verbatim from
that run's own `timed.sh` line (`wall=2431.901s user=44463.133s
sys=244.831s cpu_ratio=18.38`), which is the same arithmetic the script would
have written, and its log survives at
`/tmp/hornvale-heavy/heavy-20260805T194052Z-3264372.log` on `lefford`. Kept
for the same reason as the accidental `gate` row above — it is a genuine
measurement, and it is half of the comparison the row below it exists to
settle. **Any two heavy dispatches in a row lose the first one's ledger edit
this way**; commit the row before dispatching again.

Both 2026-08-05 `heavy` rows also carry a hand-filled `branch` cell.
`heavy-run.sh` records the run worktree's branch, and a `HV_HEAVY_REF`
dispatch checks out a detached HEAD, so it wrote neither. Both commits are on
`the-scatter`.

**A note on the four `gate (RED, ...)` rows of 2026-08-09 (The Range, task 4).**
`scripts/timed.sh` ledgers a run's wall time under the label it was given
whether the command SUCCEEDED or not — it prints `rc=<n>` to stdout and the
table has no column for it. `make gate` stops at the first failing test, so a
red gate's wall time is however long it took to reach that test: those four
rows read 47–177 s against a healthy gate's ~900 s, and without this note
they would read as a dramatic speed-up. The rows are kept rather than deleted
(they are real measurements of real commands) and relabelled so they cannot be
mistaken for gate timings. **Adding an `rc` column to the table would fix this
properly**; it is recorded here rather than done, because the schema is
consumed by more than this file.

**Every note belongs above this line, never between two rows.** The note above
first landed *inside* the table, between the last RED row and the census row
after it. Markdown needs a header plus a delimiter row to start a table, so
prose in the middle ends the table: the six rows below it rendered as literal
pipe-delimited text, and any reader or tool walking the table stopped there.
Below the table is no better a home, because `timed.sh` appends new rows with
`>> "$LEDGER"` — anything at the file's end is overtaken by the next run. So
the preamble is the only position that survives both Markdown and the writer.

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
| 2026-08-01T16:08:05Z | rebaseline | 105.505 | 114.418 | 2.705 | 1.11 | 0 | 9fab7c44 | campaign/the-particular | ambrose | 12 |
| 2026-08-01T16:12:03Z | rebaseline | 108.063 | 114.538 | 2.649 | 1.08 | 0 | 9fab7c44 | campaign/the-particular | ambrose | 12 |
| 2026-08-01T16:36:50Z | rebaseline | 108.766 | 113.282 | 2.779 | 1.07 | 0 | 3126c7bc | campaign/the-particular | ambrose | 12 |
| 2026-08-01T16:47:15Z | rebaseline | 104.098 | 113.237 | 2.764 | 1.11 | 0 | 78b66c69 | campaign/the-particular | ambrose | 12 |
| 2026-08-01T18:00:00Z | rebaseline | 106.946 | 115.737 | 2.575 | 1.11 | 0 | 354e9c32 | campaign/the-particular | ambrose | 12 |
| 2026-08-01T18:12:33Z | rebaseline | 103.073 | 121.544 | 3.688 | 1.21 | 0 | 354e9c32 | campaign/the-particular | ambrose | 12 |
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
| 2026-08-02T18:22:15Z | rebaseline | 159.639 | 163.081 | 8.891 | 1.08 | 0 | 75e83585 | the-namesake | MacBookPro | 10 |
| 2026-08-02T19:46:31Z | rebaseline | 147.934 | 165.387 | 8.431 | 1.17 | 0 | 47b41b18 | the-namesake | MacBookPro | 10 |
| 2026-08-02T19:52:56Z | gate | 121.629 | 670.257 | 32.149 | 5.77 | 0 | 47b41b18 | the-namesake | MacBookPro | 10 |
| 2026-08-02T20:29:08Z | rebaseline | 157.454 | 165.306 | 9.597 | 1.11 | 0 | 2b759d00 | the-namesake | MacBookPro | 10 |
| 2026-08-02T20:32:45Z | gate | 135.131 | 693.372 | 46.566 | 5.48 | 0 | 2b759d00 | the-namesake | MacBookPro | 10 |
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
| 2026-08-03T18:22:56Z | rebaseline | 111.782 | 107.398 | 4.858 | 1.00 | 0 | caf8c18a | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T18:27:22Z | rebaseline | 113.898 | 108.582 | 4.868 | 1.00 | 0 | caf8c18a | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T18:38:29Z | gate | 654.076 | 2472.148 | 266.565 | 4.19 | 0 | caf8c18a | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T18:50:29Z | gate | 432.477 | 2251.522 | 108.745 | 5.46 | 0 | 0ea0d497 | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T18:52:23Z | rebaseline | 106.064 | 112.347 | 5.910 | 1.11 | 0 | 0ea0d497 | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T14:03:28Z | census | 718.137 | 17769.333 | 344.830 | 25.22 | 0 | f784ed9f |  | lefford | 40 |
| 2026-08-03T14:11:11Z | gate | 410.942 | 2480.918 | 120.699 | 6.33 | 0 | eb9fb8a7 | the-namesake | MacBookPro | 10 |
| 2026-08-03T13:59:17Z | gate | 440.992 | 2351.455 | 139.497 | 5.65 | 0 | 530d67ac | the-contour | MacBookPro | 10 |
| 2026-08-03T14:17:08Z | gate | 290.060 | 2265.689 | 62.815 | 8.03 | 0 | 74dc6714 | the-namesake | MacBookPro | 10 |
| 2026-08-03T14:19:07Z | rebaseline | 110.803 | 128.913 | 7.816 | 1.23 | 0 | 74dc6714 | the-namesake | MacBookPro | 10 |
| 2026-08-03T17:17:31Z | rebaseline | 105.440 | 128.043 | 7.028 | 1.28 | 0 | 6181f12a | the-manikin | MacBookPro | 10 |
| 2026-08-03T17:25:41Z | gate | 474.645 | 2307.077 | 101.616 | 5.07 | 0 | 6181f12a | the-manikin | MacBookPro | 10 |
| 2026-08-03T18:06:43Z | gate | 699.292 | 2338.491 | 105.473 | 3.49 | 0 | 91ee526a | the-manikin | MacBookPro | 10 |
| 2026-08-03T16:59:00Z | rebaseline | 131.491 | 131.179 | 8.346 | 1.06 | 0 | 23d58a60 | the-salt | MacBookPro | 10 |
| 2026-08-03T18:04:19Z | gate | 191.355 | 232.027 | 25.414 | 1.35 | 0 | 23d58a60 | the-salt | MacBookPro | 10 |
| 2026-08-03T17:20:45Z | gate | 62.415 | 58.727 | 130.067 | 3.02 | 0 | 93e30931 | main | MacBookPro | 10 |
| 2026-08-03T17:26:02Z | gate | 289.167 | 231.107 | 176.003 | 1.41 | 0 | 93e30931 | main | MacBookPro | 10 |
| 2026-08-03T17:31:55Z | gate | 318.130 | 149.214 | 244.041 | 1.24 | 0 | 93e30931 | main | MacBookPro | 10 |
| 2026-08-03T17:33:09Z | gate | 23.619 | 119.378 | 5.034 | 5.27 | 0 | 93e30931 | main | MacBookPro | 10 |
| 2026-08-03T17:38:36Z | gate | 125.650 | 960.711 | 31.735 | 7.90 | 0 | 93e30931 | main | MacBookPro | 10 |
| 2026-08-03T17:42:24Z | gate | 125.313 | 960.289 | 31.568 | 7.92 | 0 | 93e30931 | main | MacBookPro | 10 |
| 2026-08-03T17:47:37Z | gate | 292.825 | 2222.186 | 63.099 | 7.80 | 0 | 93e30931 | main | MacBookPro | 10 |
| 2026-08-03T17:53:32Z | gate | 328.195 | 2243.266 | 71.100 | 7.05 | 0 | 93e30931 | main | MacBookPro | 10 |
| 2026-08-03T18:05:27Z | gate | 667.582 | 2277.110 | 77.800 | 3.53 | 0 | 93e30931 | main | MacBookPro | 10 |
| 2026-08-03T18:35:27Z | gate | 654.015 | 2340.043 | 314.:00 | 4.06 | 0 | 93e30931 | main | MacBookPro | 10 |
| 2026-08-03T18:40:50Z | gate | 305.256 | 2259.102 | 64.010 | 7.61 | 0 | 93e30931 | main | MacBookPro | 10 |
| 2026-08-03T19:21:56Z | rebaseline | 122.296 | 128.660 | 7.782 | 1.12 | 0 | d1b6a97d | the-salt | MacBookPro | 10 |
| 2026-08-03T19:28:58Z | gate | 393.753 | 2469.247 | 100.474 | 6.53 | 0 | a6fdf285 | the-salt | MacBookPro | 10 |
| 2026-08-03T19:42:21Z | census | 727.740 | 17876.885 | 355.798 | 25.05 | 0 | 9568ed22 |  | lefford | 40 |
| 2026-08-03T20:01:39Z | gate | 101.906 | 629.048 | 21.238 | 6.38 | 0 | 02d69fdf | the-salt | MacBookPro | 10 |
| 2026-08-03T20:16:58Z | gate | 97.543 | 613.219 | 21.378 | 6.51 | 0 | 1f0f5fb0 | main | MacBookPro | 10 |
| 2026-08-03T20:43:35Z | census | 683.489 | 17516.083 | 325.052 | 26.10 | 0 | 06d5be2c |  | lefford | 40 |
| 2026-08-03T20:55:25Z | gate | 518.681 | 2430.054 | 176.152 | 5.02 | 0 | 4b25863f | main | MacBookPro | 10 |
| 2026-08-03T21:21:03Z | gate | 959.898 | 2767.421 | 278.186 | 3.17 | 0 | 46615e05 | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T21:23:20Z | rebaseline | 125.072 | 118.348 | 6.619 | 1.00 | 0 | 46615e05 | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T21:07:06Z | gate | 379.665 | 2410.494 | 107.385 | 6.63 | 0 | f50e4c20 | the-manikin | MacBookPro | 10 |
| 2026-08-03T21:09:00Z | rebaseline | 103.116 | 129.155 | 8.232 | 1.33 | 0 | f50e4c20 | the-manikin | MacBookPro | 10 |
| 2026-08-03T21:47:39Z | gate | 943.454 | 2803.757 | 305.956 | 3.30 | 0 | 523ee30c | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-03T21:49:43Z | rebaseline | 115.785 | 123.487 | 7.713 | 1.13 | 0 | 523ee30c | campaign/the-vernacular-3 | ambrose | 12 |
| 2026-08-04T11:44:08Z | rebaseline | 116.605 | 144.932 | 7.615 | 1.31 | 0 | 546002a2 | the-generalist | MacBookPro | 10 |
| 2026-08-04T12:13:20Z | gate | 76.899 | 97.198 | 31.708 | 1.68 | 0 | 546002a2 | the-generalist | MacBookPro | 10 |
| 2026-08-04T12:20:03Z | gate | 365.561 | 2516.550 | 77.687 | 7.10 | 0 | 546002a2 | the-generalist | MacBookPro | 10 |
| 2026-08-04T12:32:43Z | rebaseline | 121.642 | 149.461 | 9.754 | 1.31 | 0 | 08587f8c | the-generalist | MacBookPro | 10 |
| 2026-08-04T12:36:31Z | rebaseline | 115.857 | 146.934 | 8.365 | 1.34 | 0 | 08587f8c | the-generalist | MacBookPro | 10 |
| 2026-08-04T12:42:15Z | gate | 315.349 | 2529.486 | 60.959 | 8.21 | 0 | 08587f8c | the-generalist | MacBookPro | 10 |
| 2026-08-04T12:44:41Z | rebaseline | 118.097 | 146.957 | 8.573 | 1.32 | 0 | 08587f8c | the-generalist | MacBookPro | 10 |
| 2026-08-04T13:01:35Z | census | 749.068 | 18822.177 | 336.629 | 25.58 | 0 | 02172e96 |  | lefford | 40 |
| 2026-08-04T13:04:42Z | gate | 112.370 | 706.178 | 21.765 | 6.48 | 0 | 74a7827d | the-generalist | MacBookPro | 10 |
| 2026-08-04T13:35:00Z | gate | 358.481 | 2491.138 | 68.839 | 7.14 | 0 | 74a7827d | the-generalist | MacBookPro | 10 |
| 2026-08-04T14:20:29Z | gate | 522.991 | 2592.550 | 139.955 | 5.22 | 0 | fbaf2178 | the-generalist | MacBookPro | 10 |
| 2026-08-04T16:54:45Z | gate | 384.571 | 2605.402 | 99.251 | 7.03 | 0 | f07aae5f | the-generalist | MacBookPro | 10 |
| 2026-08-04T02:39:32Z | gate | 326.615 | 2311.107 | 80.296 | 7.32 | 0 | a487c9ea | the-mire | MacBookPro | 10 |
| 2026-08-04T03:15:18Z | gate | 344.513 | 2303.765 | 89.744 | 6.95 | 0 | 3cdad120 | the-mire | MacBookPro | 10 |
| 2026-08-04T12:13:12Z | gate | 394.842 | 2387.065 | 98.743 | 6.30 | 0 | 96350751 | the-mire | MacBookPro | 10 |
| 2026-08-04T14:19:21Z | rebaseline | 194.204 | 130.901 | 7.135 | 0.71 | 0 | d77cb7b7 | the-mire | MacBookPro | 10 |
| 2026-08-04T14:26:34Z | gate | 413.853 | 2302.109 | 98.727 | 5.80 | 0 | d77cb7b7 | the-mire | MacBookPro | 10 |
| 2026-08-04T15:03:44Z | gate | 1001.935 | 2749.895 | 443.046 | 3.19 | 0 | ec9d9fed | main | MacBookPro | 10 |
| 2026-08-04T15:05:51Z | rebaseline | 119.400 | 126.719 | 8.132 | 1.13 | 0 | ec9d9fed | main | MacBookPro | 10 |
| 2026-08-04T15:57:05Z | gate | 316.463 | 2236.339 | 67.917 | 7.28 | 0 | 2cc693f3 | the-mire-perf | MacBookPro | 10 |
| 2026-08-04T17:19:13Z | gate | 352.905 | 2519.550 | 86.877 | 7.39 | 0 | 53ca82e1 | the-mire-glacier | MacBookPro | 10 |
| 2026-08-04T17:04:41Z | gate | 450.188 | 2767.221 | 122.004 | 6.42 | 0 | c9fbd894 | the-generalist | MacBookPro | 10 |
| 2026-08-04T17:53:31Z | gate | 747.619 | 2643.080 | 90.868 | 3.66 | 0 | 8a448c3f | the-ember | MacBookPro | 10 |
| 2026-08-04T17:54:45Z | gate | 791.846 | 3028.650 | 114.458 | 3.97 | 0 | 6204d6ea | the-mire-glacier | MacBookPro | 10 |
| 2026-08-04T17:56:55Z | rebaseline | 119.623 | 138.522 | 6.283 | 1.21 | 0 | 6204d6ea | the-mire-glacier | MacBookPro | 10 |
| 2026-08-04T18:06:41Z | gate | 395.435 | 2739.657 | 92.727 | 7.16 | 0 | 07ce101d | the-ember | MacBookPro | 10 |
| 2026-08-04T18:30:49Z | gate | 308.879 | 2532.273 | 60.412 | 8.39 | 0 | 07ce101d | the-ember | MacBookPro | 10 |
| 2026-08-05T00:07:56Z | rebaseline | 123.612 | 137.912 | 7.274 | 1.17 | 0 | 5c73dbae | the-tolerance | MacBookPro | 10 |
| 2026-08-05T01:02:07Z | gate | 14.323 | 20.272 | 10.961 | 2.18 | 0 | e8276e86 | the-tolerance | MacBookPro | 10 |
| 2026-08-05T01:04:14Z | rebaseline | 122.935 | 132.847 | 8.768 | 1.15 | 0 | e8276e86 | the-tolerance | MacBookPro | 10 |
| 2026-08-05T01:05:42Z | gate | 64.706 | 90.281 | 27.330 | 1.82 | 0 | e8276e86 | the-tolerance | MacBookPro | 10 |
| 2026-08-05T01:09:26Z | rebaseline | 103.020 | 132.215 | 8.052 | 1.36 | 0 | e8276e86 | the-tolerance | MacBookPro | 10 |
| 2026-08-05T01:10:08Z | gate | 30.768 | 95.099 | 4.936 | 3.25 | 0 | e8276e86 | the-tolerance | MacBookPro | 10 |
| 2026-08-05T01:19:18Z | gate | 126.744 | 596.856 | 57.458 | 5.16 | 0 | e8276e86 | the-tolerance | MacBookPro | 10 |
| 2026-08-05T01:21:41Z | gate | 106.174 | 611.961 | 40.045 | 6.14 | 0 | e8276e86 | the-tolerance | MacBookPro | 10 |
| 2026-08-05T01:25:14Z | gate | 139.182 | 693.788 | 41.673 | 5.28 | 0 | e8276e86 | the-tolerance | MacBookPro | 10 |
| 2026-08-05T01:34:49Z | gate | 388.563 | 2407.873 | 83.620 | 6.41 | 0 | e8276e86 | the-tolerance | MacBookPro | 10 |
| 2026-08-05T01:36:43Z | rebaseline | 102.836 | 130.842 | 7.142 | 1.34 | 0 | e8276e86 | the-tolerance | MacBookPro | 10 |
| 2026-08-05T01:39:34Z | rebaseline | 112.428 | 131.680 | 6.705 | 1.23 | 0 | 7d9df938 | the-tolerance | MacBookPro | 10 |
| 2026-08-04T23:25:00Z | gate | 696.582 | 2954.362 | 192.597 | 4.52 | 0 | 6bc92442 | campaign/the-keeping | ambrose | 12 |
| 2026-08-04T23:59:57Z | rebaseline | 127.907 | 134.832 | 7.075 | 1.11 | 0 | 79a254ba | campaign/the-keeping | ambrose | 12 |
| 2026-08-05T00:01:55Z | gate | 103.499 | 128.361 | 24.463 | 1.48 | 0 | 79a254ba | campaign/the-keeping | ambrose | 12 |
| 2026-08-05T00:05:43Z | gate | 146.561 | 663.531 | 32.824 | 4.75 | 0 | 79a254ba | campaign/the-keeping | ambrose | 12 |
| 2026-08-05T00:15:00Z | gate | 518.055 | 2405.306 | 119.467 | 4.87 | 0 | 79a254ba | campaign/the-keeping | ambrose | 12 |
| 2026-08-05T00:24:51Z | gate | 542.369 | 2641.739 | 105.219 | 5.06 | 0 | 79a254ba | campaign/the-keeping | ambrose | 12 |
| 2026-08-05T00:37:37Z | gate | 707.087 | 2797.854 | 131.829 | 4.14 | 0 | 79a254ba | campaign/the-keeping | ambrose | 12 |
| 2026-08-05T01:46:37Z | rebaseline | 112.665 | 135.991 | 7.450 | 1.27 | 0 | de69e492 | campaign/the-keeping | ambrose | 12 |
| 2026-08-05T01:55:14Z | gate | 506.746 | 2845.136 | 129.978 | 5.87 | 0 | de69e492 | campaign/the-keeping | ambrose | 12 |
| 2026-08-05T02:38:10Z | rebaseline | 127.499 | 133.942 | 7.524 | 1.11 | 0 | 18da834b | the-tolerance | MacBookPro | 10 |
| 2026-08-05T03:19:57Z | gate | 316.503 | 2449.712 | 68.402 | 7.96 | 0 | ca20b986 | the-tolerance | MacBookPro | 10 |
| 2026-08-05T12:54:21Z | rebaseline | 114.132 | 131.856 | 6.396 | 1.21 | 0 | 494d29d5 | the-tolerance | Greyjoy | 10 |
| 2026-08-05T13:11:20Z | gate | 331.525 | 2439.536 | 81.701 | 7.60 | 0 | 494d29d5 | the-tolerance | Greyjoy | 10 |
| 2026-08-05T13:49:06Z | gate | 290.327 | 2372.060 | 55.279 | 8.36 | 0 | 7d635f81 | the-tolerance | Greyjoy | 10 |
| 2026-08-05T04:27:18Z | gate | 87.624 | 228.283 | 37.585 | 3.03 | 0 | e664e208 | the-fare | MacBookPro | 10 |
| 2026-08-05T04:33:03Z | gate | 305.792 | 2525.855 | 63.028 | 8.47 | 0 | e664e208 | the-fare | MacBookPro | 10 |
| 2026-08-05T04:35:17Z | rebaseline | 114.076 | 141.669 | 8.689 | 1.32 | 0 | 5de66959 | the-fare | MacBookPro | 10 |
| 2026-08-05T11:34:53Z | gate | 306.076 | 2470.362 | 60.069 | 8.27 | 0 | c9db0876 | the-fare | MacBookPro | 10 |
| 2026-08-05T12:38:53Z | gate | 583.788 | 2817.339 | 158.576 | 5.10 | 0 | 618afe11 | campaign/the-tilth | ambrose | 12 |
| 2026-08-05T14:02:11Z | rebaseline | 133.613 | 127.540 | 7.308 | 1.01 | 0 | 9644f535 | campaign/the-tilth | ambrose | 12 |
| 2026-08-05T14:03:29Z | gate | 1.633 | 1.093 | 0.221 | 0.80 | 0 | 9644f535 | campaign/the-tilth | ambrose | 12 |
| 2026-08-05T14:04:06Z | gate | 14.094 | 21.208 | 12.548 | 2.40 | 0 | 9644f535 | campaign/the-tilth | ambrose | 12 |
| 2026-08-05T14:05:01Z | gate | 18.111 | 24.040 | 13.091 | 2.05 | 0 | 9644f535 | campaign/the-tilth | ambrose | 12 |
| 2026-08-05T14:07:18Z | gate | 114.471 | 191.081 | 51.752 | 2.12 | 0 | 9644f535 | campaign/the-tilth | ambrose | 12 |
| 2026-08-05T15:21:01Z | gate | 27.431 | 40.080 | 22.035 | 2.26 | 0 | 901ba89b | campaign/the-tilth | ambrose | 12 |
| 2026-08-05T15:23:11Z | gate | 103.901 | 162.318 | 50.784 | 2.05 | 0 | 901ba89b | campaign/the-tilth | ambrose | 12 |
| 2026-08-05T14:17:15Z | rebaseline | 105.793 | 132.341 | 6.671 | 1.31 | 0 | 73c9e10c | the-tolerance | Greyjoy | 10 |
| 2026-08-05T14:24:04Z | gate | 349.030 | 2478.170 | 83.254 | 7.34 | 0 | 44720a65 | the-tolerance | Greyjoy | 10 |
| 2026-08-05T14:29:14Z | gate | 295.247 | 2390.865 | 58.066 | 8.29 | 0 | 44720a65 | the-tolerance | Greyjoy | 10 |
| 2026-08-05T15:18:51Z | gate | 372.518 | 2449.326 | 96.419 | 6.83 | 0 | 3511485c | the-tolerance | Greyjoy | 10 |
| 2026-08-05T15:24:07Z | gate | 290.841 | 2398.507 | 53.861 | 8.43 | 0 | 3511485c | the-tolerance | Greyjoy | 10 |
| 2026-08-05T21:17:18Z | rebaseline | 159.817 | 164.234 | 7.674 | 1.08 | 0 | e8f13103 | campaign/the-tilth | ambrose | 12 |
| 2026-08-05T20:21:26Z | heavy | 2431.901 | 44463.133 | 244.831 | 18.38 | 0 | 7138ce75 | the-scatter | lefford | 40 |
| 2026-08-05T21:10:09Z | heavy | 2773.022 | 51741.211 | 565.566 | 18.86 | 0 | 239d24a7 | the-scatter | lefford | 40 |
| 2026-08-05T22:09:38Z | gate | 289.986 | 2413.614 | 55.767 | 8.52 | 0 | d578dcbe | the-scatter | Greyjoy | 10 |
| 2026-08-06T14:27:58Z | rebaseline | 150.354 | 169.257 | 9.387 | 1.19 | 0 | 81cfb885 | campaign/the-tilth | ambrose | 12 |
| 2026-08-06T14:41:21Z | gate | 787.528 | 4477.482 | 190.538 | 5.93 | 0 | c29b9e87 | campaign/the-tilth | ambrose | 12 |
| 2026-08-06T01:56:38Z | gate | 323.841 | 2416.550 | 57.456 | 7.64 | 0 | 77124f76 | the-hollow | MacBookPro | 10 |
| 2026-08-06T02:03:05Z | gate | 9.065 | 8.207 | 0.260 | 0.93 | 0 | 34cfaeb7 | the-hollow | MacBookPro | 10 |
| 2026-08-06T02:08:58Z | gate | 336.525 | 2496.290 | 81.198 | 7.66 | 0 | 34cfaeb7 | the-hollow | MacBookPro | 10 |
| 2026-08-06T02:14:07Z | gate | 304.033 | 2423.435 | 56.123 | 8.16 | 0 | 34cfaeb7 | the-hollow | MacBookPro | 10 |
| 2026-08-06T02:29:11Z | gate | 345.438 | 2501.520 | 90.366 | 7.50 | 0 | 6f8c9340 | the-hollow | MacBookPro | 10 |
| 2026-08-06T02:40:50Z | gate | 360.098 | 2525.193 | 83.453 | 7.24 | 0 | 99ee9707 | the-hollow | MacBookPro | 10 |
| 2026-08-06T02:51:25Z | gate | 358.844 | 2489.440 | 95.819 | 7.20 | 0 | d03ef60c | the-hollow | MacBookPro | 10 |
| 2026-08-06T03:29:54Z | gate | 383.787 | 2540.656 | 101.801 | 6.89 | 0 | f3998ced | the-hollow | MacBookPro | 10 |
| 2026-08-06T03:38:26Z | gate | 319.502 | 2453.323 | 60.322 | 7.87 | 0 | 594aa25a | the-hollow | MacBookPro | 10 |
| 2026-08-06T03:41:03Z | rebaseline | 102.712 | 132.066 | 5.524 | 1.34 | 0 | 594aa25a | the-hollow | MacBookPro | 10 |
| 2026-08-06T06:58:40Z | census | 664.575 | 17143.913 | 333.797 | 26.30 | 0 | 3db11d11 |  | lefford | 40 |
| 2026-08-06T07:17:57Z | gate | 312.333 | 2438.997 | 56.082 | 7.99 | 0 | 912ad31c | the-hollow | MacBookPro | 10 |
| 2026-08-06T15:50:13Z | rebaseline | 171.506 | 173.078 | 10.225 | 1.07 | 0 | 16cf8aeb | campaign/the-tilth | ambrose | 12 |
| 2026-08-06T16:08:12Z | gate | 1025.783 | 4662.590 | 276.874 | 4.82 | 0 | 4c06cf7c | campaign/the-tilth | ambrose | 12 |
| 2026-08-06T16:20:59Z | census | 718.655 | 19605.118 | 295.539 | 27.69 | 0 | 1e7f09bf |  | lefford | 40 |
| 2026-08-06T17:28:43Z | gate | 367.647 | 2532.291 | 78.381 | 7.10 | 0 | 1062e80c | the-panes | MacBookPro | 10 |
| 2026-08-06T17:30:45Z | rebaseline | 106.176 | 134.280 | 7.542 | 1.34 | 0 | 1062e80c | the-panes | MacBookPro | 10 |
| 2026-08-06T17:43:45Z | gate | 385.550 | 2531.921 | 68.661 | 6.75 | 0 | 1062e80c | the-panes | MacBookPro | 10 |
| 2026-08-06T11:49:18Z | gate | 339.464 | 2511.679 | 80.086 | 7.63 | 0 | 6379754b | the-deep-realm | MacBookPro | 10 |
| 2026-08-06T12:14:00Z | gate | 325.646 | 2394.584 | 58.751 | 7.53 | 0 | 30059dff | the-deep-realm | MacBookPro | 10 |
| 2026-08-06T12:41:49Z | rebaseline | 106.377 | 132.016 | 6.314 | 1.30 | 0 | 30059dff | the-deep-realm | MacBookPro | 10 |
| 2026-08-06T13:08:13Z | gate | 384.773 | 2472.755 | 71.790 | 6.61 | 0 | 76b5b356 | the-deep-realm | MacBookPro | 10 |
| 2026-08-06T14:15:40Z | gate | 357.352 | 2503.880 | 73.151 | 7.21 | 0 | 5c9357bb | the-deep-realm | MacBookPro | 10 |
| 2026-08-06T15:09:53Z | rebaseline | 173.398 | 137.336 | 5.896 | 0.83 | 0 | 2dae6905 | the-deep-realm | MacBookPro | 10 |
| 2026-08-06T15:28:41Z | gate | 385.376 | 2506.516 | 76.974 | 6.70 | 0 | 565eb4f5 | the-deep-realm | MacBookPro | 10 |
| 2026-08-06T16:25:12Z | rebaseline | 126.499 | 134.179 | 7.454 | 1.12 | 0 | c5cde152 | the-deep-realm | MacBookPro | 10 |
| 2026-08-06T16:38:56Z | gate | 660.455 | 2569.861 | 92.248 | 4.03 | 0 | 6e92dd9d | the-deep-realm | MacBookPro | 10 |
| 2026-08-06T14:45:03Z | gate | 353.814 | 297.223 | 101.274 | 1.13 | 0 | e225ce5e | corpus-two-matrix | MacBookPro | 10 |
| 2026-08-06T15:10:18Z | gate | 580.444 | 2500.498 | 83.247 | 4.45 | 0 | b98e793e | corpus-two-matrix | MacBookPro | 10 |
| 2026-08-06T15:49:46Z | gate | 636.632 | 2549.304 | 89.985 | 4.15 | 0 | ee5f4fb2 | main | MacBookPro | 10 |
| 2026-08-06T16:28:45Z | rebaseline | 150.480 | 138.727 | 7.690 | 0.97 | 0 | 468494e7 | the-benchmark | MacBookPro | 10 |
| 2026-08-06T16:40:06Z | gate | 621.080 | 2560.249 | 82.490 | 4.26 | 0 | 5e553379 | the-benchmark | MacBookPro | 10 |
| 2026-08-06T17:02:51Z | rebaseline | 109.356 | 137.098 | 7.342 | 1.32 | 0 | 0e727067 | the-benchmark | MacBookPro | 10 |
| 2026-08-06T17:23:02Z | gate | 388.189 | 2567.052 | 97.894 | 6.87 | 0 | 6555eed7 | the-benchmark | MacBookPro | 10 |
| 2026-08-06T16:46:40Z | rebaseline | 181.104 | 144.474 | 6.537 | 0.83 | 0 | 0c84eabd | the-deep-realm | MacBookPro | 10 |
| 2026-08-06T16:54:47Z | gate | 444.047 | 2519.087 | 83.850 | 5.86 | 0 | 20b7d36e | the-deep-realm | MacBookPro | 10 |
| 2026-08-06T17:11:49Z | rebaseline | 186.544 | 171.258 | 9.295 | 0.97 | 0 | b3148d37 | campaign/the-tilth | ambrose | 12 |
| 2026-08-06T17:20:33Z | gate | 447.634 | 1324.368 | 128.639 | 3.25 | 0 | cf431391 | campaign/the-tilth | ambrose | 12 |
| 2026-08-06T17:28:09Z | gate | 247.689 | 1182.554 | 59.179 | 5.01 | 0 | cf431391 | campaign/the-tilth | ambrose | 12 |
| 2026-08-06T17:59:53Z | gate | 4.158 | 1.594 | 0.789 | 0.57 | 0 | cf431391 | campaign/the-tilth | ambrose | 12 |
| 2026-08-06T18:00:05Z | gate | 2.782 | 1.584 | 0.534 | 0.76 | 0 | cf431391 | campaign/the-tilth | ambrose | 12 |
| 2026-08-06T18:05:02Z | gate | 281.827 | 1242.600 | 65.008 | 4.64 | 0 | cf431391 | campaign/the-tilth | ambrose | 12 |
| 2026-08-06T18:14:36Z | gate | 499.858 | 2821.896 | 122.792 | 5.89 | 0 | cf431391 | campaign/the-tilth | ambrose | 12 |
| 2026-08-06T18:32:01Z | gate | 817.613 | 4583.020 | 193.157 | 5.84 | 0 | 2dbb22d6 | campaign/the-tilth | ambrose | 12 |
| 2026-08-06T17:36:36Z | rebaseline | 141.315 | 141.188 | 7.466 | 1.05 | 0 | 8ba49736 | the-benchmark | MacBookPro | 10 |
| 2026-08-06T18:37:00Z | rebaseline | 134.737 | 137.291 | 8.549 | 1.08 | 0 | 39106718 | the-long-age | MacBookPro | 10 |
| 2026-08-06T18:45:48Z | gate | 375.801 | 2567.115 | 72.833 | 7.02 | 0 | 39106718 | the-long-age | MacBookPro | 10 |
| 2026-08-06T18:52:57Z | gate | 394.192 | 2526.848 | 76.250 | 6.60 | 0 | 79e831ba | the-long-age | MacBookPro | 10 |
| 2026-08-06T18:57:39Z | gate | 409.664 | 2689.197 | 91.443 | 6.79 | 0 | 3b366219 | the-panes | MacBookPro | 10 |
| 2026-08-06T19:00:23Z | rebaseline | 112.759 | 137.837 | 8.061 | 1.29 | 0 | 3b366219 | the-panes | MacBookPro | 10 |
| 2026-08-06T19:10:13Z | rebaseline | 124.429 | 136.715 | 7.240 | 1.16 | 0 | 40b1b410 | the-long-age | MacBookPro | 10 |
| 2026-08-06T19:16:56Z | gate | 346.069 | 2628.372 | 80.034 | 7.83 | 0 | fe7a388d | the-long-age | MacBookPro | 10 |
| 2026-08-06T19:26:09Z | gate | 321.855 | 2583.330 | 67.528 | 8.24 | 0 | 91d11731 | the-long-age | MacBookPro | 10 |
| 2026-08-06T21:07:34Z | rebaseline | 130.475 | 136.631 | 6.543 | 1.10 | 0 | 60f945fb | the-delvers | MacBookPro | 10 |
| 2026-08-06T19:42:13Z | rebaseline | 144.807 | 138.269 | 6.450 | 1.00 | 0 | d36a6a79 | the-benchmark | MacBookPro | 10 |
| 2026-08-06T19:48:18Z | gate | 350.346 | 2554.864 | 80.639 | 7.52 | 0 | d36a6a79 | the-benchmark | MacBookPro | 10 |
| 2026-08-06T20:00:48Z | gate | 326.865 | 2554.833 | 65.095 | 8.02 | 0 | 58230387 | the-benchmark | MacBookPro | 10 |
| 2026-08-06T20:02:40Z | rebaseline | 104.930 | 136.389 | 6.706 | 1.36 | 0 | 58230387 | the-benchmark | MacBookPro | 10 |
| 2026-08-07T00:33:04Z | rebaseline | 149.462 | 138.208 | 8.684 | 0.98 | 0 | 27d3fa42 | the-handle | MacBookPro | 10 |
| 2026-08-07T00:50:13Z | gate | 520.580 | 2678.804 | 123.832 | 5.38 | 0 | 27d3fa42 | the-handle | MacBookPro | 10 |
| 2026-08-06T23:35:40Z | gate | 363.912 | 2555.297 | 73.436 | 7.22 | 0 | 21ff57b9 | the-sighting | MacBookPro | 10 |
| 2026-08-06T23:47:00Z | gate | 460.403 | 2579.029 | 101.528 | 5.82 | 0 | 21ff57b9 | the-sighting | MacBookPro | 10 |
| 2026-08-07T00:18:33Z | gate | 418.504 | 2619.181 | 86.871 | 6.47 | 0 | 5cbc909c | the-sighting | MacBookPro | 10 |
| 2026-08-07T00:21:14Z | rebaseline | 150.639 | 141.205 | 5.745 | 0.98 | 0 | 5cbc909c | the-sighting | MacBookPro | 10 |
| 2026-08-07T00:57:27Z | gate | 574.322 | 2640.020 | 108.904 | 4.79 | 0 | 7eb98c45 | the-sighting | MacBookPro | 10 |
| 2026-08-07T00:59:39Z | rebaseline | 120.854 | 137.073 | 7.838 | 1.20 | 0 | 7eb98c45 | the-sighting | MacBookPro | 10 |
| 2026-08-07T01:20:55Z | gate | 373.515 | 2671.551 | 87.646 | 7.39 | 0 | c98e9242 | the-sighting | MacBookPro | 10 |
| 2026-08-07T01:23:05Z | rebaseline | 121.829 | 137.833 | 8.663 | 1.20 | 0 | c98e9242 | the-sighting | MacBookPro | 10 |
| 2026-08-07T02:02:45Z | gate | 399.433 | 2590.847 | 88.344 | 6.71 | 0 | 1b6d3b40 | the-sighting | MacBookPro | 10 |
| 2026-08-07T02:04:48Z | rebaseline | 110.299 | 137.418 | 8.270 | 1.32 | 0 | 1b6d3b40 | the-sighting | MacBookPro | 10 |
| 2026-08-07T02:34:41Z | gate | 479.044 | 2573.458 | 88.225 | 5.56 | 0 | 1b6d3b40 | the-sighting | MacBookPro | 10 |
| 2026-08-07T02:36:46Z | rebaseline | 112.913 | 136.768 | 8.616 | 1.29 | 0 | 1b6d3b40 | the-sighting | MacBookPro | 10 |
| 2026-08-07T04:17:56Z | gate | 337.048 | 2553.404 | 81.700 | 7.82 | 0 | 7b2c48fc | the-sighting | MacBookPro | 10 |
| 2026-08-07T04:20:44Z | rebaseline | 104.639 | 138.821 | 9.205 | 1.41 | 0 | 7b2c48fc | the-sighting | MacBookPro | 10 |
| 2026-08-07T04:35:32Z | gate | 349.929 | 2568.873 | 98.502 | 7.62 | 0 | edec90f1 | the-sighting | MacBookPro | 10 |
| 2026-08-06T21:12:49Z | rebaseline | 129.192 | 135.293 | 7.157 | 1.10 | 0 | 034e28da | the-delvers | MacBookPro | 10 |
| 2026-08-06T21:20:27Z | gate | 360.076 | 2548.282 | 66.791 | 7.26 | 0 | 43ab5e6c | the-delvers | MacBookPro | 10 |
| 2026-08-06T21:49:39Z | gate | 430.549 | 2517.111 | 62.784 | 5.99 | 0 | 00f29022 | the-delvers | MacBookPro | 10 |
| 2026-08-06T20:17:13Z | rebaseline | 186.903 | 175.151 | 9.867 | 0.99 | 0 | 35df3763 | campaign/the-tilth | ambrose | 12 |
| 2026-08-06T20:22:41Z | rebaseline | 164.259 | 176.145 | 9.287 | 1.13 | 0 | 35df3763 | campaign/the-tilth | ambrose | 12 |
| 2026-08-06T20:34:11Z | gate | 628.942 | 2066.020 | 241.997 | 3.67 | 0 | 35df3763 | campaign/the-tilth | ambrose | 12 |
| 2026-08-06T20:48:17Z | gate | 820.120 | 4545.753 | 182.868 | 5.77 | 0 | 35df3763 | campaign/the-tilth | ambrose | 12 |
| 2026-08-06T21:26:49Z | census | 732.938 | 19406.708 | 292.611 | 26.88 | 0 | 1d19d84e |  | lefford | 40 |
| 2026-08-06T21:43:15Z | gate | 783.394 | 4392.992 | 151.750 | 5.80 | 0 | 684a44b7 | campaign/the-tilth | ambrose | 12 |
| 2026-08-06T23:08:59Z | gate | 787.233 | 4367.422 | 152.497 | 5.74 | 0 | 979508f8 | campaign/the-tilth | ambrose | 12 |
| 2026-08-07T00:23:52Z | gate | 806.086 | 4339.758 | 157.964 | 5.58 | 0 | ae85d279 | campaign/the-tilth | ambrose | 12 |
| 2026-08-07T00:26:15Z | rebaseline | 142.897 | 169.763 | 8.039 | 1.24 | 0 | ae85d279 | campaign/the-tilth | ambrose | 12 |
| 2026-08-07T01:45:45Z | rebaseline | 197.448 | 185.091 | 8.013 | 0.98 | 0 | c84c39f2 | the-handle | MacBookPro | 10 |
| 2026-08-07T01:58:00Z | gate | 645.780 | 4277.908 | 136.641 | 6.84 | 0 | d31599d1 | the-handle | MacBookPro | 10 |
| 2026-08-07T00:57:07Z | rebaseline | 237.637 | 188.426 | 7.357 | 0.82 | 0 | 386e3fd5 | the-delvers | MacBookPro | 10 |
| 2026-08-07T01:41:30Z | gate | 792.789 | 3535.051 | 115.633 | 4.60 | 0 | 4731b926 | the-delvers | MacBookPro | 10 |
| 2026-08-07T02:16:28Z | gate | 617.082 | 4086.449 | 88.953 | 6.77 | 0 | a2ab54fd | the-delvers | MacBookPro | 10 |
| 2026-08-07T02:31:47Z | gate | 689.853 | 4100.118 | 96.138 | 6.08 | 0 | 16873d47 | the-delvers | MacBookPro | 10 |
| 2026-08-07T02:52:38Z | rebaseline | 163.328 | 182.160 | 8.696 | 1.17 | 0 | af7e740d | the-handle | MacBookPro | 10 |
| 2026-08-07T03:02:42Z | gate | 604.761 | 4330.421 | 133.278 | 7.38 | 0 | af7e740d | the-handle | MacBookPro | 10 |
| 2026-08-07T12:59:55Z | rebaseline | 121.999 | 178.413 | 6.762 | 1.52 | 0 | f034d089 | the-handle | MacBookPro | 10 |
| 2026-08-07T13:08:41Z | gate | 518.384 | 4082.029 | 86.863 | 8.04 | 0 | f034d089 | the-handle | MacBookPro | 10 |
| 2026-08-07T12:15:37Z | gate | 591.033 | 4406.664 | 124.325 | 7.67 | 0 | 91d979dd | the-sighting | MacBookPro | 10 |
| 2026-08-07T12:18:36Z | rebaseline | 127.277 | 179.163 | 8.232 | 1.47 | 0 | 91d979dd | the-sighting | MacBookPro | 10 |
| 2026-08-07T13:16:07Z | rebaseline | 134.623 | 179.342 | 7.217 | 1.39 | 0 | 4564a3c0 | the-handle | MacBookPro | 10 |
| 2026-08-07T13:24:57Z | gate | 529.350 | 4175.021 | 105.854 | 8.09 | 0 | 4564a3c0 | the-handle | MacBookPro | 10 |
| 2026-08-07T13:50:43Z | gate | 740.908 | 4403.448 | 154.674 | 6.15 | 0 | e94ca801 | the-delvers | MacBookPro | 10 |
| 2026-08-07T17:47:25Z | census | 767.820 | 19731.888 | 302.834 | 26.09 | 0 | 82a7aa5e | campaign/the-assay | lefford | 40 |
| 2026-08-07T19:46:31Z | rebaseline | 146.495 | 182.682 | 7.706 | 1.30 | 0 | f5e1638c | the-beholding | MacBookPro | 10 |
| 2026-08-07T20:10:49Z | gate | 680.938 | 4388.532 | 141.952 | 6.65 | 0 | b5e2b316 | the-beholding | MacBookPro | 10 |
| 2026-08-07T20:15:38Z | rebaseline | 140.400 | 181.091 | 8.276 | 1.35 | 0 | 8a62bad1 | the-beholding | MacBookPro | 10 |
| 2026-08-07T20:25:22Z | gate | 577.329 | 4368.126 | 92.153 | 7.73 | 0 | 8a62bad1 | the-beholding | MacBookPro | 10 |
| 2026-08-07T22:18:52Z | gate | 683.708 | 4317.449 | 93.630 | 6.45 | 0 | c8d8ab14 | the-beholding | MacBookPro | 10 |
| 2026-08-08T01:55:19Z | rebaseline | 250.045 | 214.393 | 7.813 | 0.89 | 0 | da78dcd3 | the-delvers | MacBookPro | 10 |
| 2026-08-08T02:01:27Z | gate | 173.638 | 108.947 | 86.440 | 1.13 | 0 | 09e65f20 | the-delvers | MacBookPro | 10 |
| 2026-08-08T02:19:12Z | rebaseline | 166.891 | 217.276 | 11.459 | 1.37 | 0 | 85730c75 | the-delvers | MacBookPro | 10 |
| 2026-08-08T02:22:40Z | rebaseline | 159.313 | 214.665 | 10.386 | 1.41 | 0 | 85730c75 | the-delvers | MacBookPro | 10 |
| 2026-08-08T03:19:32Z | gate | 472.789 | 1333.093 | 164.533 | 3.17 | 0 | 386c3b80 | the-delvers | MacBookPro | 10 |
| 2026-08-08T03:42:54Z | rebaseline | 252.660 | 220.484 | 8.423 | 0.91 | 0 | 386c3b80 | the-delvers | MacBookPro | 10 |
| 2026-08-08T03:59:05Z | census | 815.433 | 21897.244 | 287.595 | 27.21 | 0 | 74ecda14 |  | lefford | 40 |
| 2026-08-08T05:15:04Z | gate | 613.275 | 5056.748 | 106.835 | 8.42 | 0 | df6f1913 | the-delvers | MacBookPro | 10 |
| 2026-08-08T06:00:06Z | gate | 608.220 | 5123.521 | 103.373 | 8.59 | 0 | e5245de4 | the-delvers | MacBookPro | 10 |
| 2026-08-08T00:12:04Z | rebaseline | 164.135 | 178.418 | 7.747 | 1.13 | 0 | 3f37bc41 | the-lantern | MacBookPro | 10 |
| 2026-08-08T00:14:49Z | rebaseline | 150.686 | 179.456 | 8.125 | 1.24 | 0 | 3f37bc41 | the-lantern | MacBookPro | 10 |
| 2026-08-08T01:12:54Z | gate | 826.120 | 4307.802 | 158.899 | 5.41 | 0 | 0682786f | the-lantern | MacBookPro | 10 |
| 2026-08-08T02:49:18Z | gate | 581.457 | 4382.219 | 115.123 | 7.73 | 0 | 7f198ea5 | the-lantern | MacBookPro | 10 |
| 2026-08-08T03:43:54Z | gate | 1045.020 | 4457.466 | 195.217 | 4.45 | 0 | a5503433 | the-lantern | MacBookPro | 10 |
| 2026-08-08T04:52:07Z | gate | 660.707 | 4466.117 | 117.796 | 6.94 | 0 | a67f6627 | the-lantern | MacBookPro | 10 |
| 2026-08-08T05:01:41Z | rebaseline | 203.265 | 183.944 | 7.357 | 0.94 | 0 | a67f6627 | the-lantern | MacBookPro | 10 |
| 2026-08-08T00:23:07Z | rebaseline | 169.240 | 177.922 | 9.206 | 1.11 | 0 | 490757f9 | campaign/the-assay | ambrose | 12 |
| 2026-08-08T00:44:28Z | gate | 1032.408 | 5219.448 | 296.296 | 5.34 | 0 | 8ab36d1e | campaign/the-assay | ambrose | 12 |
| 2026-08-08T02:13:54Z | gate | 923.743 | 4955.322 | 244.078 | 5.63 | 0 | 1aa73b90 | campaign/the-assay | ambrose | 12 |
| 2026-08-08T12:08:17Z | gate | 525.463 | 4316.741 | 99.502 | 8.40 | 0 | 6e65e3c5 | campaign/the-assay | MacBookPro | 10 |
| 2026-08-08T12:10:50Z | rebaseline | 132.231 | 179.320 | 9.711 | 1.43 | 0 | 6e65e3c5 | campaign/the-assay | MacBookPro | 10 |
| 2026-08-08T13:48:18Z | census | 823.684 | 22174.657 | 294.179 | 27.28 | 0 | 7517be80 |  | lefford | 40 |
| 2026-08-08T14:10:57Z | gate | 636.735 | 4987.082 | 102.351 | 7.99 | 0 | 5127fbdd | the-delvers | MacBookPro | 10 |
| 2026-08-08T12:35:32Z | rebaseline | 172.902 | 181.686 | 9.892 | 1.11 | 0 | 475d271c | the-lantern | MacBookPro | 10 |
| 2026-08-08T12:54:45Z | gate | 1143.876 | 4681.693 | 280.780 | 4.34 | 0 | 475d271c | the-lantern | MacBookPro | 10 |
| 2026-08-08T13:24:09Z | gate | 1051.374 | 4361.224 | 94.984 | 4.24 | 0 | c7e6e649 | the-lantern | MacBookPro | 10 |
| 2026-08-08T13:27:30Z | rebaseline | 200.844 | 179.857 | 7.158 | 0.93 | 0 | c7e6e649 | the-lantern | MacBookPro | 10 |
| 2026-08-08T14:15:10Z | rebaseline | 183.085 | 213.350 | 9.490 | 1.22 | 0 | 23f375e3 | the-delvers | MacBookPro | 10 |
| 2026-08-08T14:42:55Z | gate | 650.025 | 5224.269 | 107.718 | 8.20 | 0 | f3b502d9 | the-delvers | MacBookPro | 10 |
| 2026-08-08T18:52:08Z | rebaseline | 134.646 | 212.083 | 8.781 | 1.64 | 0 | 6d52efef | the-digest | Greyjoy | 10 |
| 2026-08-08T19:04:04Z | gate | 683.824 | 5265.549 | 103.707 | 7.85 | 0 | 6d52efef | the-digest | Greyjoy | 10 |
| 2026-08-08T16:19:17Z | census | 826.858 | 22249.330 | 310.619 | 27.28 | 0 | 5ec42fee |  | lefford | 40 |
| 2026-08-08T16:45:45Z | gate | 650.518 | 5333.971 | 115.743 | 8.38 | 0 | 9c96e45f | the-confusion | MacBookPro | 10 |
| 2026-08-08T19:44:29Z | gate | 720.883 | 5367.866 | 139.872 | 7.64 | 0 | ee007ceb | the-digest | Greyjoy | 10 |
| 2026-08-08T23:54:16Z | rebaseline | 175.806 | 214.011 | 9.624 | 1.27 | 0 | c3f03eda | the-quire | Greyjoy | 10 |
| 2026-08-09T01:12:34Z | gate | 930.476 | 5505.971 | 172.640 | 6.10 | 0 | c3f03eda | the-quire | Greyjoy | 10 |
| 2026-08-09T01:59:17Z | rebaseline | 191.397 | 221.731 | 10.139 | 1.21 | 0 | e6e101fb | the-quire | Greyjoy | 10 |
| 2026-08-09T02:30:36Z | rebaseline | 213.930 | 240.968 | 15.772 | 1.20 | 0 | 32b8237b | the-quire | Greyjoy | 10 |
| 2026-08-09T03:05:11Z | rebaseline | 230.757 | 226.166 | 8.568 | 1.02 | 0 | 243be98b | the-domesday | Greyjoy | 10 |
| 2026-08-09T03:24:02Z | gate | 1118.304 | 5388.444 | 132.908 | 4.94 | 0 | 243be98b | the-domesday | Greyjoy | 10 |
| 2026-08-08T22:22:53Z | rebaseline | 221.906 | 214.565 | 7.569 | 1.00 | 0 | e9cb4a09 | the-tare | Greyjoy | 10 |
| 2026-08-08T22:37:51Z | census | 818.223 | 22085.898 | 289.309 | 27.35 | 0 | fb4629ce |  | lefford | 40 |
| 2026-08-08T22:58:34Z | gate | 666.946 | 5332.498 | 125.161 | 8.18 | 0 | ce74f902 | the-tare | Greyjoy | 10 |
| 2026-08-09T00:07:00Z | census | 823.758 | 22170.172 | 286.555 | 27.26 | 0 | 426eaa18 |  | lefford | 40 |
| 2026-08-09T00:22:37Z | gate | 875.512 | 5361.649 | 147.067 | 6.29 | 0 | 6ce36e9f | the-tare | Greyjoy | 10 |
| 2026-08-09T00:47:44Z | gate | 837.112 | 5361.162 | 133.124 | 6.56 | 0 | b1797d16 | the-tare | Greyjoy | 10 |
| 2026-08-09T00:50:25Z | rebaseline | 160.991 | 215.814 | 10.296 | 1.40 | 0 | b1797d16 | the-tare | Greyjoy | 10 |
| 2026-08-09T03:00:31Z | gate | 7.112 | 18.233 | 3.675 | 3.08 | 0 | 4ca26481 | the-range | Greyjoy | 10 |
| 2026-08-09T03:21:25Z | gate | 1237.949 | 5561.846 | 169.508 | 4.63 | 0 | 4ca26481 | the-range | Greyjoy | 10 |
| 2026-08-09T04:40:05Z | gate | 19.210 | 30.705 | 18.173 | 2.54 | 0 | ebc56cbf | the-range | MacBookPro | 10 |
| 2026-08-09T04:51:35Z | gate | 675.175 | 5442.585 | 143.219 | 8.27 | 0 | ebc56cbf | the-range | MacBookPro | 10 |
| 2026-08-09T05:16:03Z | rebaseline | 169.717 | 219.438 | 11.587 | 1.36 | 0 | 46ecfeea | the-range | MacBookPro | 10 |
| 2026-08-09T05:28:54Z | gate | 698.010 | 5490.849 | 151.606 | 8.08 | 0 | 46ecfeea | the-range | MacBookPro | 10 |
| 2026-08-09T06:07:47Z | gate (RED, aborted at first failure — NOT a gate timing) | 117.613 | 236.854 | 77.933 | 2.68 | 0 | d844cd48 | the-range | MacBookPro | 10 |
| 2026-08-09T06:10:25Z | gate (RED, aborted at first failure — NOT a gate timing) | 47.745 | 178.973 | 13.642 | 4.03 | 0 | d844cd48 | the-range | MacBookPro | 10 |
| 2026-08-09T06:17:40Z | gate (RED, aborted at first failure — NOT a gate timing) | 129.348 | 821.637 | 30.417 | 6.59 | 0 | d844cd48 | the-range | MacBookPro | 10 |
| 2026-08-09T06:30:15Z | gate (RED, aborted at first failure — NOT a gate timing) | 176.679 | 1020.216 | 72.533 | 6.18 | 0 | d844cd48 | the-range | MacBookPro | 10 |
| 2026-08-09T11:00:43Z | census | 920.964 | 22515.362 | 398.599 | 24.88 | 0 | eb2a660d |  | lefford | 40 |
| 2026-08-09T11:20:42Z | gate | 139.015 | 136.696 | 65.900 | 1.46 | 0 | d3457790 | the-range | MacBookPro | 10 |
| 2026-08-09T12:49:30Z | gate | 339.976 | 1315.759 | 44.130 | 4.00 | 0 | 54ef9a8c | the-range | MacBookPro | 10 |
| 2026-08-09T13:18:59Z | census | 886.849 | 22735.004 | 364.368 | 26.05 | 0 | 101a5395 |  | lefford | 40 |
| 2026-08-09T13:58:32Z | gate | 69.416 | 95.223 | 20.386 | 1.67 | 0 | 584cb85f | the-range | MacBookPro | 10 |
| 2026-08-09T14:31:43Z | gate | 673.663 | 5411.968 | 114.709 | 8.20 | 0 | 12fdd917 | the-range | MacBookPro | 10 |
| 2026-08-09T15:27:31Z | gate | 1024.541 | 5599.183 | 292.478 | 5.75 | 0 | 36f242b0 | the-range | MacBookPro | 10 |
| 2026-08-09T03:42:08Z | gate | 166.773 | 970.250 | 52.143 | 6.13 | 0 | a1397eb4 | the-domesday | Greyjoy | 10 |
| 2026-08-09T03:56:04Z | gate | 609.495 | 5351.635 | 105.095 | 8.95 | 0 | 3c8bce1d | the-domesday | Greyjoy | 10 |
| 2026-08-09T11:32:23Z | gate | 712.251 | 5392.274 | 132.705 | 7.76 | 0 | d5556484 | the-domesday | MacBookPro | 10 |
| 2026-08-09T16:33:22Z | gate | 1687.264 | 5427.848 | 188.191 | 3.33 | 0 | 1f919b35 | the-armature | MacBookPro | 10 |
| 2026-08-09T19:17:17Z | gate | 681.660 | 5374.890 | 125.911 | 8.07 | 0 | b8e54666 | the-armature | MacBookPro | 10 |
| 2026-08-09T16:50:36Z | rebaseline | 1031.462 | 221.929 | 24.358 | 0.24 | 0 | a0425d42 | the-quire | MacBookPro | 10 |
| 2026-08-09T17:01:34Z | gate | 598.129 | 120.308 | 34.201 | 0.26 | 0 | a0425d42 | the-quire | MacBookPro | 10 |
| 2026-08-09T15:58:04Z | rebaseline | 219.623 | 221.226 | 12.718 | 1.07 | 0 | f376b02e | the-range | MacBookPro | 10 |
| 2026-08-09T16:28:04Z | gate | 1618.466 | 5563.237 | 145.263 | 3.53 | 0 | f232ea27 | the-range | MacBookPro | 10 |
| 2026-08-09T14:27:59Z | rebaseline | 88.974 | 120.295 | 3.950 | 1.40 | 0 | 89c5c9c5 | campaign/the-whetstone | ambrose | 12 |
| 2026-08-09T14:58:36Z | rebaseline | 154.631 | 149.990 | 10.195 | 1.04 | 0 | 89c5c9c5 | campaign/the-whetstone | ambrose | 12 |
| 2026-08-09T15:00:31Z | gate | 61.111 | 82.301 | 68.257 | 2.46 | 0 | 89c5c9c5 | campaign/the-whetstone | ambrose | 12 |
| 2026-08-09T15:09:18Z | gate | 460.776 | 2176.987 | 174.637 | 5.10 | 0 | 89c5c9c5 | campaign/the-whetstone | ambrose | 12 |
| 2026-08-09T15:17:41Z | ci | 412.317 | 2190.124 | 164.131 | 5.71 | 0 | 6cb43b46 | campaign/the-whetstone | ambrose | 12 |
| 2026-08-09T16:04:25Z | census | 775.777 | 21482.605 | 338.046 | 28.13 | 0 | 4c2156a4 |  | lefford | 40 |
| 2026-08-09T16:14:37Z | gate | 458.530 | 2197.705 | 174.988 | 5.17 | 0 | 121da186 | campaign/the-whetstone | ambrose | 12 |
| 2026-08-09T16:16:29Z | rebaseline | 111.946 | 145.349 | 10.084 | 1.39 | 0 | 121da186 | campaign/the-whetstone | ambrose | 12 |
| 2026-08-09T19:55:57Z | gate | 414.534 | 2641.861 | 147.536 | 6.73 | 0 | 07e09902 | the-armature | MacBookPro | 10 |
| 2026-08-10T13:08:26Z | gate | 544.580 | 2048.597 | 153.368 | 4.04 | 0 | 26695a7f | the-armature | MacBookPro | 10 |
| 2026-08-09T23:39:41Z | gate | 409.207 | 2236.787 | 161.801 | 5.86 | 0 | 3a8c88f6 | campaign/the-cairn | ambrose | 12 |
| 2026-08-10T01:02:24Z | gate (RED, aborted at test 44/3283 — not a gate cost) | 24.238 | 42.983 | 7.331 | 2.08 | 0 | e092781f | campaign/the-cairn | ambrose | 12 |
| 2026-08-10T01:10:45Z | gate | 409.351 | 2222.986 | 159.006 | 5.82 | 0 | e092781f | campaign/the-cairn | ambrose | 12 |
| 2026-08-09T19:24:28Z | rebaseline | 211.979 | 155.238 | 7.988 | 0.77 | 0 | 58fcbecc | the-quire | MacBookPro | 10 |
| 2026-08-09T19:37:58Z | gate | 398.608 | 2656.919 | 143.351 | 7.03 | 0 | 58fcbecc | the-quire | MacBookPro | 10 |
| 2026-08-09T19:53:22Z | rebaseline | 120.383 | 157.567 | 7.035 | 1.37 | 0 | f466f316 | the-quire | MacBookPro | 10 |
| 2026-08-09T20:33:46Z | rebaseline | 86.543 | 152.313 | 8.764 | 1.86 | 0 | 5442b5dd | the-quire | MacBookPro | 10 |
| 2026-08-09T20:38:56Z | gate | 281.713 | 2018.793 | 118.298 | 7.59 | 0 | 5442b5dd | the-quire | MacBookPro | 10 |
| 2026-08-09T16:42:40Z | gate | 42.239 | 9.524 | 1.359 | 0.26 | 0 | bde7ea5b | the-range | MacBookPro | 10 |
| 2026-08-09T17:10:54Z | gate | 1573.493 | 742.990 | 142.404 | 0.56 | 0 | 48273750 | the-range | MacBookPro | 10 |
| 2026-08-09T19:29:23Z | gate | 375.331 | 2055.533 | 104.130 | 5.75 | 0 | 48273750 | the-range | MacBookPro | 10 |
| 2026-08-09T22:11:09Z | gate | 303.086 | 2011.615 | 99.506 | 6.97 | 0 | 0e9f71b2 | the-range | MacBookPro | 10 |
| 2026-08-09T22:25:06Z | rebaseline | 178.263 | 150.929 | 8.414 | 0.89 | 0 | 9363ff1c | campaign/the-particular | MacBookPro | 10 |
| 2026-08-09T22:34:08Z | gate | 290.502 | 792.287 | 67.996 | 2.96 | 0 | 88bed44d | campaign/the-particular | MacBookPro | 10 |
| 2026-08-09T22:38:12Z | gate | 128.288 | 413.589 | 32.564 | 3.48 | 0 | 88bed44d | campaign/the-particular | MacBookPro | 10 |
| 2026-08-09T22:45:41Z | rebaseline | 80.575 | 147.217 | 8.415 | 1.93 | 0 | 88bed44d | campaign/the-particular | MacBookPro | 10 |
| 2026-08-09T22:52:55Z | gate | 348.570 | 2091.756 | 128.937 | 6.37 | 0 | 88bed44d | campaign/the-particular | MacBookPro | 10 |
| 2026-08-09T22:23:31Z | rebaseline | 136.973 | 153.062 | 7.758 | 1.17 | 0 | c19b61f7 | the-quire | MacBookPro | 10 |
| 2026-08-09T22:39:04Z | gate | 563.267 | 2443.566 | 142.836 | 4.59 | 0 | b16005b7 | the-quire | MacBookPro | 10 |
| 2026-08-09T23:06:44Z | gate | 633.696 | 2408.448 | 141.599 | 4.02 | 0 | 94f1a58e | the-radiation | MacBookPro | 10 |
| 2026-08-10T02:01:15Z | rebaseline | 126.519 | 153.323 | 10.027 | 1.29 | 0 | 1931a904 | campaign/the-cairn | ambrose | 12 |
| 2026-08-10T02:12:01Z | gate | 623.565 | 2967.549 | 239.978 | 5.14 | 0 | 1931a904 | campaign/the-cairn | ambrose | 12 |
| 2026-08-10T03:07:08Z | gate | 394.469 | 2066.178 | 122.606 | 5.55 | 0 | bc9c12ca | the-radiation | MacBookPro | 10 |
| 2026-08-10T10:00:29Z | rebaseline | 166.237 | 157.215 | 9.119 | 1.00 | 0 | 419d3278 | the-radiation | MacBookPro | 10 |
| 2026-08-10T10:02:20Z | gate | 68.266 | 83.991 | 41.361 | 1.84 | 0 | 419d3278 | the-radiation | MacBookPro | 10 |
| 2026-08-10T10:03:32Z | gate | 39.038 | 84.424 | 16.313 | 2.58 | 0 | 419d3278 | the-radiation | MacBookPro | 10 |
| 2026-08-10T10:09:28Z | gate | 255.735 | 2061.482 | 100.763 | 8.46 | 0 | 419d3278 | the-radiation | MacBookPro | 10 |
| 2026-08-09T22:56:34Z | rebaseline | 107.223 | 152.765 | 8.804 | 1.51 | 0 | a97ac55a | campaign/the-particular | MacBookPro | 10 |
| 2026-08-09T23:06:59Z | gate | 609.521 | 2343.316 | 151.764 | 4.09 | 0 | a97ac55a | campaign/the-particular | MacBookPro | 10 |
| 2026-08-10T01:45:39Z | rebaseline | 127.620 | 156.252 | 10.190 | 1.30 | 0 | 88aef9dc | campaign/the-signet | MacBookPro | 10 |
| 2026-08-10T01:51:35Z | gate | 272.482 | 2134.445 | 114.845 | 8.25 | 0 | 88aef9dc | campaign/the-signet | MacBookPro | 10 |
| 2026-08-10T02:15:37Z | rebaseline | 114.912 | 154.800 | 10.212 | 1.44 | 0 | 9c6b95bf | campaign/the-signet | MacBookPro | 10 |
| 2026-08-10T02:21:35Z | gate | 284.371 | 2112.746 | 126.739 | 7.88 | 0 | 9c6b95bf | campaign/the-signet | MacBookPro | 10 |
| 2026-08-10T02:40:05Z | rebaseline | 109.909 | 156.297 | 9.723 | 1.51 | 0 | ed57246b | campaign/the-signet | MacBookPro | 10 |
| 2026-08-10T02:40:39Z | gate | 3.844 | 5.094 | 2.040 | 1.86 | 0 | ed57246b | campaign/the-signet | MacBookPro | 10 |
| 2026-08-10T02:45:41Z | gate | 291.280 | 2129.468 | 118.224 | 7.72 | 0 | ed57246b | campaign/the-signet | MacBookPro | 10 |
| 2026-08-10T03:03:48Z | rebaseline | 200.074 | 158.414 | 9.014 | 0.84 | 0 | 5f8e3b18 | campaign/the-signet | MacBookPro | 10 |
| 2026-08-10T03:08:04Z | gate | 52.126 | 84.010 | 26.128 | 2.11 | 0 | 5f8e3b18 | campaign/the-signet | MacBookPro | 10 |
| 2026-08-10T03:13:03Z | gate | 256.533 | 2069.197 | 101.954 | 8.46 | 0 | 5f8e3b18 | campaign/the-signet | MacBookPro | 10 |
| 2026-08-10T03:37:07Z | rebaseline | 111.097 | 156.105 | 9.495 | 1.49 | 0 | d99a4c19 | campaign/the-signet | MacBookPro | 10 |
| 2026-08-10T03:43:51Z | gate | 328.105 | 2139.246 | 168.508 | 7.03 | 0 | d99a4c19 | campaign/the-signet | MacBookPro | 10 |
| 2026-08-10T04:10:31Z | rebaseline | 114.255 | 156.864 | 10.167 | 1.46 | 0 | d05f89b9 | campaign/the-signet | MacBookPro | 10 |
| 2026-08-10T04:16:36Z | gate | 347.746 | 2143.352 | 188.071 | 6.70 | 0 | d05f89b9 | campaign/the-signet | MacBookPro | 10 |
| 2026-08-10T04:40:40Z | gate | 367.501 | 2142.911 | 187.664 | 6.34 | 0 | 62bd32d2 | campaign/the-signet | MacBookPro | 10 |
| 2026-08-10T02:01:15Z | rebaseline | 126.519 | 153.323 | 10.027 | 1.29 | 0 | 1931a904 | campaign/the-cairn | ambrose | 12 |
| 2026-08-10T02:12:01Z | gate | 623.565 | 2967.549 | 239.978 | 5.14 | 0 | 1931a904 | campaign/the-cairn | ambrose | 12 |
| 2026-08-10T09:59:10Z | gate | 346.491 | 2077.436 | 113.068 | 6.32 | 0 | 9e196cf7 | campaign/the-signet | MacBookPro | 10 |
| 2026-08-10T10:01:26Z | rebaseline | 122.330 | 156.725 | 11.028 | 1.37 | 0 | 9e196cf7 | campaign/the-signet | MacBookPro | 10 |
| 2026-08-10T10:30:13Z | gate | 399.194 | 2766.195 | 163.882 | 7.34 | 0 | b65abbfa | the-radiation | MacBookPro | 10 |
| 2026-08-10T11:31:21Z | rebaseline | 134.047 | 214.081 | 12.716 | 1.69 | 0 | b65abbfa | the-radiation | MacBookPro | 10 |
| 2026-08-10T11:44:26Z | gate | 141.502 | 924.342 | 53.130 | 6.91 | 0 | b65abbfa | the-radiation | MacBookPro | 10 |
| 2026-08-10T12:36:40Z | rebaseline | 134.750 | 215.701 | 13.357 | 1.70 | 0 | 2bd26ff1 | the-radiation | MacBookPro | 10 |
| 2026-08-10T13:32:19Z | rebaseline | 154.293 | 210.338 | 12.984 | 1.45 | 0 | 834fee5c | the-radiation | MacBookPro | 10 |
| 2026-08-10T14:37:13Z | rebaseline | 185.990 | 213.737 | 12.750 | 1.22 | 0 | 59904d50 | the-radiation | MacBookPro | 10 |
| 2026-08-10T17:13:38Z | rebaseline | 131.539 | 211.853 | 13.984 | 1.72 | 0 | cda3e3c4 | the-radiation | MacBookPro | 10 |
| 2026-08-10T18:15:15Z | rebaseline | 127.071 | 212.247 | 13.766 | 1.78 | 0 | cbcf2935 | the-radiation | MacBookPro | 10 |
| 2026-08-10T12:07:30Z | rebaseline | 201.426 | 157.966 | 8.343 | 0.83 | 0 | 12046ad9 | campaign/the-particular | MacBookPro | 10 |
| 2026-08-10T12:17:37Z | gate | 431.641 | 2595.365 | 151.810 | 6.36 | 0 | 6fd3160e | campaign/the-particular | MacBookPro | 10 |
| 2026-08-10T13:08:12Z | rebaseline | 242.973 | 160.486 | 9.357 | 0.70 | 0 | 2fa333e7 | campaign/the-particular | MacBookPro | 10 |
| 2026-08-10T13:12:39Z | gate | 134.287 | 580.288 | 42.008 | 4.63 | 0 | 2fa333e7 | campaign/the-particular | MacBookPro | 10 |
| 2026-08-10T13:19:54Z | gate | 378.616 | 2111.765 | 118.044 | 5.89 | 0 | 2fa333e7 | campaign/the-particular | MacBookPro | 10 |
| 2026-08-10T13:25:35Z | gate | 335.821 | 2091.030 | 109.572 | 6.55 | 0 | 2fa333e7 | campaign/the-particular | MacBookPro | 10 |
| 2026-08-10T14:03:37Z | gate | 690.572 | 2125.383 | 174.449 | 3.33 | 0 | 162df445 | campaign/the-particular | MacBookPro | 10 |
| 2026-08-10T14:26:18Z | gate | 282.108 | 2068.115 | 108.860 | 7.72 | 0 | 6d5369de | campaign/the-particular | MacBookPro | 10 |
| 2026-08-10T13:53:05Z | gate | 246.286 | 597.441 | 56.589 | 2.66 | 0 | 43b91fc0 | the-armature | MacBookPro | 10 |
| 2026-08-10T14:04:42Z | gate | 596.929 | 2067.602 | 109.774 | 3.65 | 0 | d51e13a3 | the-armature | MacBookPro | 10 |
| 2026-08-10T14:34:48Z | rebaseline | 128.926 | 160.771 | 11.007 | 1.33 | 0 | 147dab73 | campaign/the-particular | MacBookPro | 10 |
| 2026-08-10T14:46:29Z | gate | 511.306 | 2143.033 | 120.630 | 4.43 | 0 | 9bf32382 | campaign/the-particular | MacBookPro | 10 |
| 2026-08-10T16:01:59Z | gate | 268.627 | 2070.073 | 103.159 | 8.09 | 0 | 4a8123bd | campaign/the-particular | MacBookPro | 10 |
| 2026-08-10T22:50:41Z | rebaseline | 132.896 | 156.361 | 10.299 | 1.25 | 0 | 29b9cfcd | campaign/the-grain | ambrose | 12 |
| 2026-08-10T22:51:44Z | gate | 5.565 | 4.144 | 1.010 | 0.93 | 0 | 29b9cfcd | campaign/the-grain | ambrose | 12 |
| 2026-08-10T23:15:10Z | gate | 572.319 | 2409.113 | 202.108 | 4.56 | 0 | 29b9cfcd | campaign/the-grain | ambrose | 12 |
| 2026-08-10T23:43:42Z | rebaseline | 174.264 | 155.550 | 10.153 | 0.95 | 0 | cc171e04 | campaign/the-grain | ambrose | 12 |
| 2026-08-10T23:52:37Z | gate | 475.885 | 2100.280 | 210.312 | 4.86 | 0 | cc171e04 | campaign/the-grain | ambrose | 12 |
| 2026-08-11T00:03:10Z | gate | 487.473 | 2366.972 | 173.618 | 5.21 | 0 | cc171e04 | campaign/the-grain | ambrose | 12 |
| 2026-08-11T00:21:07Z | rebaseline | 112.138 | 159.709 | 11.029 | 1.52 | 0 | 208b1795 | campaign/the-grain | ambrose | 12 |
| 2026-08-11T00:36:47Z | gate | 581.556 | 2378.197 | 211.451 | 4.45 | 0 | 208b1795 | campaign/the-grain | ambrose | 12 |
| 2026-08-11T01:18:19Z | rebaseline | 125.800 | 157.022 | 10.345 | 1.33 | 0 | 582dfcb9 | campaign/the-grain | ambrose | 12 |
| 2026-08-11T01:29:51Z | gate | 602.152 | 2428.010 | 215.761 | 4.39 | 0 | 0037ff40 | campaign/the-grain | ambrose | 12 |
| 2026-08-11T02:33:37Z | gate | 756.198 | 2380.987 | 223.809 | 3.44 | 0 | 3cc002c7 | campaign/the-grain | ambrose | 12 |
| 2026-08-11T03:12:17Z | census | 742.780 | 21271.996 | 326.765 | 29.08 | 0 | 2b3aa426 |  | lefford | 40 |
| 2026-08-10T18:31:52Z | rebaseline | 128.235 | 218.085 | 12.916 | 1.80 | 0 | cb40aa16 | the-radiation | MacBookPro | 10 |
| 2026-08-10T18:50:33Z | rebaseline | 115.718 | 211.740 | 12.154 | 1.93 | 0 | 2a7d77cc | the-radiation | MacBookPro | 10 |
| 2026-08-10T19:18:43Z | rebaseline | 117.257 | 210.628 | 13.128 | 1.91 | 0 | 5eab416a | the-radiation | MacBookPro | 10 |
| 2026-08-10T19:44:40Z | rebaseline | 119.110 | 213.945 | 12.590 | 1.90 | 0 | e955c330 | the-radiation | MacBookPro | 10 |
| 2026-08-10T20:02:47Z | rebaseline | 112.311 | 203.702 | 11.877 | 1.92 | 0 | bd2498a9 | the-radiation | MacBookPro | 10 |
| 2026-08-10T20:04:46Z | rebaseline | 110.075 | 214.032 | 12.776 | 2.06 | 0 | bd2498a9 | the-radiation | MacBookPro | 10 |
| 2026-08-11T00:28:30Z | census | 906.416 | 26024.736 | 316.985 | 29.06 | 0 | 07117d05 |  | lefford | 40 |
| 2026-08-11T01:20:06Z | rebaseline | 133.614 | 212.124 | 13.982 | 1.69 | 0 | 6df8935c | the-radiation | MacBookPro | 10 |
| 2026-08-11T02:48:23Z | rebaseline | 142.557 | 217.672 | 13.578 | 1.62 | 0 | 581468bb | the-radiation | MacBookPro | 10 |
| 2026-08-11T02:55:23Z | rebaseline | 128.923 | 215.188 | 13.967 | 1.78 | 0 | 581468bb | the-radiation | MacBookPro | 10 |
| 2026-08-11T03:27:22Z | rebaseline | 123.628 | 212.751 | 13.432 | 1.83 | 0 | cfc028b8 | the-radiation | MacBookPro | 10 |
| 2026-08-11T12:48:39Z | census | 1709.884 | 55231.248 | 351.697 | 32.51 | 0 | 77a534d7 |  | lefford | 40 |
| 2026-08-11T12:53:20Z | gate | 117.285 | 377.736 | 49.286 | 3.64 | 0 | 4d4c554c | the-ford | MacBookPro | 10 |
| 2026-08-11T12:56:57Z | rebaseline | 138.787 | 215.143 | 12.682 | 1.64 | 0 | 4d4c554c | the-ford | MacBookPro | 10 |
| 2026-08-11T13:06:12Z | rebaseline | 140.214 | 216.476 | 13.471 | 1.64 | 0 | 4d4c554c | the-ford | MacBookPro | 10 |
| 2026-08-11T13:13:25Z | gate | 414.210 | 2826.503 | 175.864 | 7.25 | 0 | 4d4c554c | the-ford | MacBookPro | 10 |
| 2026-08-11T13:36:34Z | rebaseline | 158.553 | 218.624 | 13.989 | 1.47 | 0 | 4d705d6a | the-ford | MacBookPro | 10 |
| 2026-08-11T13:44:39Z | gate | 456.043 | 2922.700 | 192.646 | 6.83 | 0 | 4d705d6a | the-ford | MacBookPro | 10 |
| 2026-08-11T12:48:34Z | rebaseline | 210.198 | 221.922 | 15.190 | 1.13 | 0 | 9137c6ee | campaign/the-grain | ambrose | 12 |
| 2026-08-11T13:12:06Z | gate | 871.123 | 3621.770 | 280.788 | 4.48 | 0 | fefdde7c | campaign/the-grain | ambrose | 12 |
| 2026-08-11T13:20:28Z | rebaseline | 135.236 | 217.349 | 12.887 | 1.70 | 0 | e4e32877 | campaign/the-grain | ambrose | 12 |
| 2026-08-11T13:32:40Z | gate | 684.332 | 3120.950 | 203.679 | 4.86 | 0 | a5874f81 | campaign/the-grain | ambrose | 12 |
| 2026-08-11T13:54:56Z | gate | 795.773 | 2784.123 | 218.544 | 3.77 | 0 | 08656f2a | campaign/the-grain | ambrose | 12 |
| 2026-08-11T15:14:08Z | rebaseline | 241.337 | 223.370 | 12.169 | 0.98 | 0 | f82979f0 | the-muster | MacBookPro | 10 |
| 2026-08-11T16:37:32Z | rebaseline | 163.806 | 222.247 | 13.917 | 1.44 | 0 | ebe10025 | the-muster | MacBookPro | 10 |
| 2026-08-11T16:51:32Z | gate | 767.164 | 2923.563 | 179.807 | 4.05 | 0 | ebe10025 | the-muster | MacBookPro | 10 |
| 2026-08-11T17:43:00Z | rebaseline | 152.613 | 220.374 | 13.699 | 1.53 | 0 | a751ef47 | the-muster | MacBookPro | 10 |
| 2026-08-11T18:02:01Z | gate | 1072.746 | 2896.450 | 186.441 | 2.87 | 0 | a751ef47 | the-muster | MacBookPro | 10 |
| 2026-08-11T19:27:49Z | rebaseline | 172.335 | 226.489 | 13.990 | 1.40 | 0 | 39badb3a | campaign/the-ell | MacBookPro | 10 |
| 2026-08-11T19:30:51Z | gate | 3.582 | 3.918 | 0.711 | 1.29 | 0 | 39badb3a | campaign/the-ell | MacBookPro | 10 |
| 2026-08-11T19:45:11Z | gate | 848.515 | 2976.326 | 182.582 | 3.72 | 0 | 39badb3a | campaign/the-ell | MacBookPro | 10 |
| 2026-08-11T22:41:36Z | gate | 478.048 | 238.904 | 188.507 | 0.89 | 0 | 0e7ef367 | campaign/the-ell | MacBookPro | 10 |
| 2026-08-11T22:49:18Z | rebaseline | 336.516 | 210.643 | 21.070 | 0.69 | 0 | 0e7ef367 | campaign/the-ell | MacBookPro | 10 |
| 2026-08-11T23:06:33Z | gate | 986.644 | 2587.558 | 283.154 | 2.91 | 0 | 0e7ef367 | campaign/the-ell | MacBookPro | 10 |
| 2026-08-12T00:15:38Z | rebaseline | 134.612 | 213.851 | 13.317 | 1.69 | 0 | 3b90d2cb | campaign/the-ell | MacBookPro | 10 |
| 2026-08-12T00:23:58Z | gate | 490.840 | 2896.335 | 262.613 | 6.44 | 0 | 3b90d2cb | campaign/the-ell | MacBookPro | 10 |
| 2026-08-12T00:46:06Z | rebaseline | 136.263 | 214.817 | 13.091 | 1.67 | 0 | f94462a4 | campaign/the-ell | MacBookPro | 10 |
| 2026-08-12T00:55:30Z | gate | 490.796 | 2910.945 | 202.498 | 6.34 | 0 | b2096eb1 | campaign/the-ell | MacBookPro | 10 |
| 2026-08-12T01:01:29Z | gate | 353.434 | 2780.671 | 127.569 | 8.23 | 0 | b2096eb1 | campaign/the-ell | MacBookPro | 10 |
| 2026-08-11T15:03:53Z | rebaseline | 270.670 | 221.893 | 12.667 | 0.87 | 0 | d012cbc8 | the-ford | MacBookPro | 10 |
| 2026-08-11T15:15:53Z | gate | 677.105 | 3063.305 | 180.818 | 4.79 | 0 | 7b62955c | the-ford | MacBookPro | 10 |
| 2026-08-11T18:55:42Z | gate | 493.419 | 2996.830 | 166.714 | 6.41 | 0 | cb452ef9 | the-ford | MacBookPro | 10 |
| 2026-08-11T19:45:04Z | gate | 813.997 | 2913.290 | 160.402 | 3.78 | 0 | 1a0ef764 | the-ford | MacBookPro | 10 |
| 2026-08-11T19:47:22Z | rebaseline | 130.977 | 211.091 | 10.824 | 1.69 | 0 | 1a0ef764 | the-ford | MacBookPro | 10 |
| 2026-08-11T20:42:53Z | gate | 55.522 | 58.128 | 43.773 | 1.84 | 0 | f913dec0 | the-ford | MacBookPro | 10 |
| 2026-08-11T20:46:57Z | rebaseline | 237.107 | 216.811 | 12.327 | 0.97 | 0 | f913dec0 | the-ford | MacBookPro | 10 |
| 2026-08-11T21:00:07Z | gate | 761.579 | 2952.939 | 180.084 | 4.11 | 0 | f913dec0 | the-ford | MacBookPro | 10 |
| 2026-08-11T21:35:41Z | gate | 714.766 | 2934.534 | 173.484 | 4.35 | 0 | 852b0489 | the-ford | MacBookPro | 10 |
| 2026-08-11T21:28:10Z | gate | 421.592 | 12073.940 | 576.360 | 30.01 | 0 | e4109d81 | main | lefford | 40 |
| 2026-08-11T21:34:14Z | gate | 330.952 | 9364.161 | 322.224 | 29.27 | 0 | 68845661 | main | lefford | 40 |
| 2026-08-11T21:42:09Z | rebaseline | 210.710 | 671.782 | 35.453 | 3.36 | 0 | ac25390d | main | lefford | 40 |
| 2026-08-11T22:21:28Z | rebaseline | 153.013 | 217.485 | 14.246 | 1.51 | 0 | 68c1529a | the-ford | MacBookPro | 10 |
| 2026-08-11T22:35:19Z | gate | 811.636 | 2924.244 | 240.257 | 3.90 | 0 | 68c1529a | the-ford | MacBookPro | 10 |
| 2026-08-11T22:37:15Z | rebaseline | 226.874 | 787.099 | 41.482 | 3.65 | 0 | 6d1124b3 | main | lefford | 40 |
| 2026-08-11T22:55:40Z | gate | 1060.547 | 2728.664 | 275.952 | 2.83 | 0 | 595fe472 | the-ford | MacBookPro | 10 |
| 2026-08-11T23:19:30Z | rebaseline | 1354.109 | 227.214 | 14.480 | 0.18 | 0 | 71fe3ee4 | main | MacBookPro | 10 |
| 2026-08-11T23:55:48Z | gate | 632.778 | 2851.547 | 136.570 | 4.72 | 0 | ec5ac2e3 | the-ford-sweep | MacBookPro | 10 |
| 2026-08-12T02:04:27Z | rebaseline | 191.298 | 226.844 | 13.537 | 1.26 | 0 | d64b8db2 | campaign/the-ell | MacBookPro | 10 |
| 2026-08-12T02:17:35Z | gate | 609.032 | 3261.270 | 269.918 | 5.80 | 0 | 16ec65c1 | campaign/the-ell | MacBookPro | 10 |
| 2026-08-12T02:55:15Z | census | 1789.103 | 55621.340 | 376.212 | 31.30 | 0 | 5f5a746f |  | lefford | 40 |
| 2026-08-12T03:07:53Z | gate | 349.474 | 2783.264 | 121.222 | 8.31 | 0 | 2328b129 | campaign/the-ell | MacBookPro | 10 |
| 2026-08-12T03:10:08Z | rebaseline | 129.634 | 220.716 | 13.330 | 1.81 | 0 | 2328b129 | campaign/the-ell | MacBookPro | 10 |
| 2026-08-12T04:07:51Z | rebaseline | 206.785 | 228.672 | 17.177 | 1.19 | 0 | d55430f2 | the-muster | MacBookPro | 10 |
| 2026-08-12T04:38:07Z | rebaseline | 213.426 | 223.716 | 11.829 | 1.10 | 0 | f65d7989 | the-muster | MacBookPro | 10 |
| 2026-08-12T04:57:42Z | rebaseline | 211.870 | 232.128 | 10.980 | 1.15 | 0 | 1ff501f2 | the-muster | MacBookPro | 10 |
| 2026-08-12T04:03:52Z | rebaseline | 250.038 | 224.777 | 11.427 | 0.94 | 0 | b7ed06e0 | chore/the-ell-followups | MacBookPro | 10 |
| 2026-08-12T05:09:02Z | rebaseline | 208.745 | 221.349 | 10.138 | 1.11 | 0 | dab64dd8 | chore/the-ell-followups | MacBookPro | 10 |
| 2026-08-12T05:25:53Z | gate | 644.266 | 3285.785 | 232.398 | 5.46 | 0 | dccaf5e9 | chore/the-ell-followups | MacBookPro | 10 |
| 2026-08-12T05:28:12Z | rebaseline | 138.528 | 225.337 | 13.428 | 1.72 | 0 | dccaf5e9 | chore/the-ell-followups | MacBookPro | 10 |
| 2026-08-12T11:23:51Z | rebaseline | 142.291 | 225.507 | 14.173 | 1.68 | 0 | 73285531 | the-muster | MacBookPro | 10 |
| 2026-08-12T15:47:28Z | gate | 132.100 | 254.771 | 43.597 | 2.26 | 0 | f85223b5 | the-repose | MacBookPro | 10 |
| 2026-08-12T16:02:04Z | gate | 761.500 | 2886.958 | 166.103 | 4.01 | 0 | f85223b5 | the-repose | MacBookPro | 10 |
| 2026-08-12T16:05:03Z | rebaseline | 161.655 | 222.301 | 14.855 | 1.47 | 0 | f85223b5 | the-repose | MacBookPro | 10 |
| 2026-08-12T16:24:10Z | gate | 644.038 | 2913.948 | 142.511 | 4.75 | 0 | 546b4a44 | the-repose | MacBookPro | 10 |
| 2026-08-12T16:39:33Z | gate | 11.052 | 19.625 | 10.565 | 2.73 | 0 | 546b4a44 | the-repose | MacBookPro | 10 |
| 2026-08-12T16:47:33Z | gate | 453.130 | 3021.394 | 171.831 | 7.05 | 0 | 546b4a44 | the-repose | MacBookPro | 10 |
| 2026-08-12T16:49:53Z | rebaseline | 132.682 | 220.595 | 11.130 | 1.75 | 0 | 546b4a44 | the-repose | MacBookPro | 10 |
| 2026-08-12T17:30:09Z | gate | 650.533 | 2857.125 | 135.101 | 4.60 | 0 | 41a0f30b | the-repose | MacBookPro | 10 |
| 2026-08-12T17:50:03Z | rebaseline | 188.339 | 227.111 | 16.028 | 1.29 | 0 | 59270967 | the-repose | MacBookPro | 10 |
| 2026-08-12T17:59:30Z | gate | 546.919 | 2860.563 | 177.718 | 5.56 | 0 | 59270967 | the-repose | MacBookPro | 10 |
| 2026-08-12T18:29:04Z | rebaseline | 324.897 | 228.379 | 13.868 | 0.75 | 0 | 680edf8e | the-repose | MacBookPro | 10 |
| 2026-08-12T18:43:45Z | gate | 856.590 | 3169.730 | 182.693 | 3.91 | 0 | 680edf8e | the-repose | MacBookPro | 10 |
| 2026-08-12T18:51:31Z | gate | 459.683 | 2880.680 | 134.834 | 6.56 | 0 | 680edf8e | the-repose | MacBookPro | 10 |
| 2026-08-12T19:20:26Z | rebaseline | 168.164 | 211.060 | 11.479 | 1.32 | 0 | f46f0cd1 | the-repose | Greyjoy | 10 |
| 2026-08-12T19:24:02Z | gate | 200.136 | 429.073 | 75.111 | 2.52 | 0 | f46f0cd1 | the-repose | Greyjoy | 10 |
| 2026-08-12T19:51:20Z | rebaseline | 123.472 | 218.548 | 10.156 | 1.85 | 0 | f46f0cd1 | the-repose | MacBookPro | 10 |
| 2026-08-12T19:58:44Z | gate | 429.436 | 2937.945 | 178.083 | 7.26 | 0 | f46f0cd1 | the-repose | MacBookPro | 10 |
| 2026-08-12T17:55:43Z | gate | 90.032 | 92.405 | 29.586 | 1.35 | 0 | 813a89d5 | the-fathom | MacBookPro | 10 |
| 2026-08-12T18:05:45Z | gate | 493.815 | 2775.592 | 132.539 | 5.89 | 0 | 813a89d5 | the-fathom | MacBookPro | 10 |
| 2026-08-12T18:08:25Z | rebaseline | 153.717 | 220.493 | 13.471 | 1.52 | 0 | 813a89d5 | the-fathom | MacBookPro | 10 |
| 2026-08-12T18:14:54Z | gate | 377.168 | 2825.364 | 120.830 | 7.81 | 0 | 813a89d5 | the-fathom | MacBookPro | 10 |
| 2026-08-12T11:48:08Z | rebaseline | 163.713 | 224.357 | 12.141 | 1.44 | 0 | 94355fdd | the-rill | MacBookPro | 10 |
| 2026-08-12T11:55:56Z | gate | 459.335 | 3261.585 | 179.805 | 7.49 | 0 | 94355fdd | the-rill | MacBookPro | 10 |
| 2026-08-12T14:31:52Z | rebaseline | 172.909 | 242.408 | 13.642 | 1.48 | 0 | 677637cc | the-rill | MacBookPro | 10 |
| 2026-08-12T20:41:22Z | gate | 572.134 | 3873.430 | 163.493 | 7.06 | 0 | 4fa4d379 | the-rill | MacBookPro | 10 |
| 2026-08-12T22:40:40Z | gate | 321.735 | 2485.126 | 99.036 | 8.03 | 0 | 7784cb12 | the-rill | MacBookPro | 10 |
| 2026-08-12T22:49:31Z | gate | 417.690 | 3607.968 | 121.235 | 8.93 | 0 | 7784cb12 | the-rill | MacBookPro | 10 |
| 2026-08-12T17:03:19Z | rebaseline | 319.229 | 214.276 | 15.031 | 0.72 | 0 | c3371305 | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-12T15:57:39Z | gate (RED, aborted at test 32/3443 — NOT a gate timing) | 299.058 | 1044.114 | 118.091 | 3.89 | 0 | 217b4579 | campaign/the-beacon | ambrose | 12 |
| 2026-08-12T16:15:28Z | gate | 764.565 | 3196.520 | 237.086 | 4.49 | 0 | 217b4579 | campaign/the-beacon | ambrose | 12 |
| 2026-08-12T16:37:02Z | rebaseline | 251.291 | 242.063 | 15.189 | 1.02 | 0 | d6521c71 | campaign/the-beacon | ambrose | 12 |
| 2026-08-12T18:41:05Z | gate | 804.506 | 3167.345 | 232.787 | 4.23 | 0 | be113f1b | campaign/the-beacon | ambrose | 12 |
| 2026-08-12T21:04:17Z | census | 1718.995 | 55074.823 | 330.455 | 32.23 | 0 | 7304eb09 |  | lefford | 40 |
| 2026-08-12T21:23:50Z | gate | 792.397 | 3164.587 | 278.844 | 4.35 | 0 | 65ef15c3 | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-12T21:30:14Z | rebaseline | 140.212 | 223.903 | 13.811 | 1.70 | 0 | 65ef15c3 | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-13T01:30:36Z | rebaseline | 129.899 | 174.998 | 6.758 | 1.40 | 0 | 46b18c8e | campaign/the-holdfast | ambrose | 12 |
| 2026-08-13T01:55:26Z | gate | 397.595 | 2560.744 | 162.618 | 6.85 | 0 | e30862db | campaign/the-holdfast | ambrose | 12 |
| 2026-08-12T23:46:14Z | gate | 524.352 | 3799.965 | 207.319 | 7.64 | 0 | 9706bfbc | the-rill | MacBookPro | 10 |
| 2026-08-13T06:08:39Z | census | 19207.751 | 700215.149 | 783.494 | 36.50 | 0 | d8ed9bd6 |  | lefford | 40 |
| 2026-08-13T11:20:38Z | rebaseline | 112.526 | 227.012 | 10.664 | 2.11 | 0 | cc576f08 | the-rill | MacBookPro | 10 |
| 2026-08-13T11:51:02Z | rebaseline | 130.291 | 228.438 | 10.932 | 1.84 | 0 | 734d0a1b | the-rill | MacBookPro | 10 |
| 2026-08-13T12:08:20Z | gate | 489.124 | 3536.576 | 177.917 | 7.59 | 0 | 856d1523 | the-rill | MacBookPro | 10 |
| 2026-08-13T16:33:11Z | quick | 10.416 | 8.060 | 0.637 | 0.83 | 0 | 760b0121 | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T16:52:38Z | gate | 470.049 | 3333.589 | 131.641 | 7.37 | 0 | a3726caf | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T17:02:48Z | gate | 426.943 | 3356.194 | 135.646 | 8.18 | 0 | a3726caf | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T17:12:04Z | gate | 430.710 | 3335.997 | 138.597 | 8.07 | 0 | a3726caf | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T17:27:28Z | gate | 432.104 | 3333.637 | 138.146 | 8.03 | 0 | a3726caf | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T17:27:50Z | gate | 2.012 | 1.321 | 0.261 | 0.79 | 0 | a3726caf | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T17:30:31Z | gate | 143.330 | 629.350 | 49.983 | 4.74 | 0 | a3726caf | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T17:48:46Z | gate | 146.366 | 626.899 | 50.986 | 4.63 | 0 | d426f96f | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T17:57:13Z | gate | 440.488 | 3334.040 | 142.877 | 7.89 | 0 | d426f96f | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T18:11:38Z | preflight | 4.227 | 0.362 | 0.583 | 0.22 | 0 | 1ec240e9 | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T18:20:02Z | gate | 453.471 | 3348.510 | 147.192 | 7.71 | 0 | 1ec240e9 | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T18:22:37Z | rebaseline | 125.192 | 198.559 | 7.849 | 1.65 | 0 | 1ec240e9 | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T18:23:05Z | prewarm | 0.811 | 0.226 | 0.284 | 0.63 | 0 | 1ec240e9 | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T18:30:14Z | gate | 428.842 | 3350.700 | 133.602 | 8.12 | 0 | 1ec240e9 | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T18:30:14Z | gate-fast | 429.339 | 3350.735 | 133.658 | 8.12 | 0 | 1ec240e9 | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T18:32:36Z | game-check | 71.797 | 184.728 | 16.243 | 2.80 | 0 | 1ec240e9 | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T18:33:42Z | vessel-check | 56.763 | 75.907 | 7.724 | 1.47 | 0 | 1ec240e9 | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T18:34:30Z | world-check | 46.945 | 61.741 | 6.717 | 1.46 | 0 | 1ec240e9 | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T19:13:14Z | rebaseline | 36.728 | 218.968 | 9.481 | 6.22 | 0 | d8435f61 | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T19:13:51Z | rebaseline | 37.018 | 219.513 | 9.188 | 6.18 | 0 | d8435f61 | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T13:40:29Z | gate | 475.516 | 3635.033 | 163.694 | 7.99 | 0 | 8e007e2a | campaign/the-millrace | MacBookPro | 10 |
| 2026-08-13T13:42:59Z | rebaseline | 143.694 | 230.531 | 11.703 | 1.69 | 0 | 8e007e2a | campaign/the-millrace | MacBookPro | 10 |
| 2026-08-13T14:18:40Z | gate | 410.831 | 3452.995 | 120.050 | 8.70 | 0 | 8e22fc23 | campaign/the-millrace | MacBookPro | 10 |
| 2026-08-13T14:46:11Z | gate | 401.733 | 3459.289 | 120.083 | 8.91 | 0 | 56cca795 | campaign/the-millrace | MacBookPro | 10 |
| 2026-08-13T15:21:03Z | gate | 465.518 | 3557.896 | 168.378 | 8.00 | 0 | a5eefec5 | campaign/the-millrace | MacBookPro | 10 |
| 2026-08-13T16:07:39Z | gate | 455.370 | 3325.260 | 156.346 | 7.65 | 0 | 097108f7 | campaign/the-millrace | MacBookPro | 10 |
| 2026-08-13T16:09:58Z | rebaseline | 131.191 | 227.271 | 12.458 | 1.83 | 0 | 097108f7 | campaign/the-millrace | MacBookPro | 10 |
| 2026-08-13T16:40:19Z | gate | 428.645 | 2876.961 | 162.115 | 7.09 | 0 | 7a18edee | campaign/the-millrace | MacBookPro | 10 |
| 2026-08-13T17:19:14Z | gate | 397.700 | 2985.809 | 151.760 | 7.89 | 0 | 4bb7b4a9 | campaign/the-millrace | MacBookPro | 10 |
| 2026-08-13T17:21:36Z | rebaseline | 133.377 | 228.738 | 14.277 | 1.82 | 0 | 4bb7b4a9 | campaign/the-millrace | MacBookPro | 10 |
| 2026-08-13T17:48:54Z | gate | 412.071 | 3043.338 | 178.460 | 7.82 | 0 | d6f8fb52 | campaign/the-millrace | MacBookPro | 10 |
| 2026-08-13T17:51:22Z | rebaseline | 137.554 | 228.837 | 13.568 | 1.76 | 0 | d6f8fb52 | campaign/the-millrace | MacBookPro | 10 |
| 2026-08-13T18:31:01Z | gate | 353.081 | 2933.928 | 126.989 | 8.67 | 0 | 53a73c7d | campaign/the-millrace | MacBookPro | 10 |
| 2026-08-13T18:33:19Z | rebaseline | 130.899 | 227.586 | 13.529 | 1.84 | 0 | 53a73c7d | campaign/the-millrace | MacBookPro | 10 |
| 2026-08-13T19:01:49Z | census | 949.579 | 26793.028 | 330.171 | 28.56 | 0 | 476f578d |  | lefford | 40 |
| 2026-08-13T19:25:00Z | gate | 345.675 | 2899.941 | 125.765 | 8.75 | 0 | f9766097 | campaign/the-millrace | MacBookPro | 10 |
| 2026-08-13T19:27:16Z | rebaseline | 129.575 | 226.835 | 13.231 | 1.85 | 0 | f9766097 | campaign/the-millrace | MacBookPro | 10 |
| 2026-08-13T19:35:12Z | gate | 356.958 | 2949.735 | 126.718 | 8.62 | 0 | 11efd527 | campaign/the-millrace | MacBookPro | 10 |
| 2026-08-13T19:37:26Z | rebaseline | 127.760 | 226.699 | 12.894 | 1.88 | 0 | 11efd527 | campaign/the-millrace | MacBookPro | 10 |
| 2026-08-13T19:44:42Z | gate | 356.234 | 2966.450 | 126.160 | 8.68 | 0 | 9db4f8df | campaign/the-millrace | MacBookPro | 10 |
| 2026-08-13T19:47:02Z | rebaseline | 133.561 | 227.300 | 13.538 | 1.80 | 0 | 9db4f8df | campaign/the-millrace | MacBookPro | 10 |
| 2026-08-13T20:16:53Z | rebaseline | 18.544 | 78.723 | 6.793 | 4.61 | 0 | b82572ef | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T20:18:06Z | rebaseline | 59.556 | 215.479 | 10.221 | 3.79 | 0 | b82572ef | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T20:19:30Z | rebaseline | 62.477 | 216.708 | 10.308 | 3.63 | 0 | b82572ef | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T20:20:58Z | rebaseline | 67.744 | 216.519 | 10.243 | 3.35 | 0 | b82572ef | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T20:30:49Z | gate | 551.188 | 3275.460 | 218.811 | 6.34 | 0 | eaa16a6b | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T20:44:35Z | quick | 9.785 | 7.963 | 0.562 | 0.87 | 0 | 69876dab | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T20:52:28Z | gate | 391.977 | 2868.289 | 157.465 | 7.72 | 0 | de001f3f | chore/ci-duration-triage | MacBookPro | 10 |
| 2026-08-13T20:54:55Z | rebaseline | 139.398 | 225.888 | 17.866 | 1.75 | 0 | de001f3f | chore/ci-duration-triage | MacBookPro | 10 |
| 2026-08-13T20:08:31Z | ci | 778.090 | 3486.519 | 265.627 | 4.82 | 0 | de001f3f | main | MacBookPro | 10 |
| 2026-08-13T21:10:59Z | ci | 312.548 | 2737.764 | 112.446 | 9.12 | 0 | 13bdd375 | main | MacBookPro | 10 |
| 2026-08-13T21:39:01Z | rebaseline | 118.465 | 170.907 | 6.986 | 1.50 | 0 | 2cf15549 | campaign/the-holdfast | ambrose | 12 |
| 2026-08-13T21:48:49Z | gate | 581.983 | 2914.020 | 213.047 | 5.37 | 0 | 2cf15549 | campaign/the-holdfast | ambrose | 12 |
| 2026-08-13T22:06:49Z | gate | 429.749 | 3154.100 | 154.255 | 7.70 | 0 | 465bfcaa | the-fathom | MacBookPro | 10 |
| 2026-08-13T22:09:22Z | rebaseline | 124.224 | 218.258 | 13.258 | 1.86 | 0 | 465bfcaa | the-fathom | MacBookPro | 10 |
| 2026-08-14T00:56:09Z | census | 920.212 | 26755.045 | 325.981 | 29.43 | 0 | 5eb5d5f5 |  | lefford | 40 |
| 2026-08-14T01:31:44Z | gate | 10.607 | 9.742 | 0.287 | 0.95 | 0 | 11c55e14 | campaign/the-gnomon | MacBookPro | 10 |
| 2026-08-14T01:41:59Z | gate | 601.846 | 2812.232 | 206.344 | 5.02 | 0 | 11c55e14 | campaign/the-gnomon | MacBookPro | 10 |
| 2026-08-14T02:09:06Z | gate | 346.538 | 2806.997 | 120.296 | 8.45 | 0 | 42de6b2d | campaign/the-gnomon | MacBookPro | 10 |
| 2026-08-14T03:09:58Z | gate | 89.056 | 102.250 | 56.933 | 1.79 | 0 | 284832d0 | campaign/the-gnomon | MacBookPro | 10 |
| 2026-08-14T03:16:27Z | gate | 358.513 | 2798.279 | 118.956 | 8.14 | 0 | 284832d0 | campaign/the-gnomon | MacBookPro | 10 |
| 2026-08-14T03:24:55Z | gate | 376.610 | 2754.946 | 126.286 | 7.65 | 0 | e5d07691 | campaign/the-gnomon | MacBookPro | 10 |
| 2026-08-14T03:31:38Z | gate | 402.809 | 2754.107 | 132.855 | 7.17 | 0 | e5d07691 | campaign/the-gnomon | MacBookPro | 10 |
| 2026-08-13T22:31:58Z | ci | 706.738 | 3011.844 | 279.553 | 4.66 | 0 | 883c63ae | main | MacBookPro | 10 |
| 2026-08-13T22:50:52Z | rebaseline | 56.474 | 202.620 | 9.170 | 3.75 | 0 | cec0ebeb | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T22:58:31Z | gate | 450.857 | 2892.454 | 189.725 | 6.84 | 0 | cec0ebeb | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T23:30:38Z | quick | 9.843 | 8.001 | 0.555 | 0.87 | 0 | 9467e6c4 | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T23:31:59Z | quick | 12.269 | 13.802 | 5.469 | 1.57 | 0 | 82127c11 | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T23:35:44Z | gate-fast | 68.397 | 66.056 | 22.238 | 1.29 | 0 | dbb48adc | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T23:49:17Z | gate-fast | 652.227 | 2700.393 | 142.155 | 4.36 | 0 | dbb48adc | campaign/the-sexton | ambrose | 12 |
| 2026-08-14T00:02:06Z | gate-fast | 372.635 | 2671.241 | 129.699 | 7.52 | 0 | dbb48adc | campaign/the-sexton | ambrose | 12 |
| 2026-08-14T00:03:08Z | rebaseline | 36.117 | 202.787 | 8.982 | 5.86 | 0 | dbb48adc | campaign/the-sexton | ambrose | 12 |
| 2026-08-14T00:04:29Z | quick | 9.899 | 7.927 | 0.604 | 0.86 | 0 | dbb48adc | campaign/the-sexton | ambrose | 12 |
| 2026-08-13T23:39:36Z | rebaseline | 138.749 | 176.943 | 7.452 | 1.33 | 0 | 62f2ae83 | campaign/the-holdfast | ambrose | 12 |
| 2026-08-13T23:50:31Z | gate | 647.096 | 2585.853 | 184.428 | 4.28 | 0 | 62f2ae83 | campaign/the-holdfast | ambrose | 12 |
| 2026-08-13T23:54:03Z | rebaseline | 155.906 | 166.498 | 6.938 | 1.11 | 0 | 7b9b56c6 | main | ambrose | 12 |
| 2026-08-14T00:25:41Z | gate | 173.039 | 571.172 | 108.324 | 3.93 | 0 | 69efacb5 | campaign/the-sexton | ambrose | 12 |
| 2026-08-14T00:31:30Z | gate | 338.099 | 2412.621 | 123.277 | 7.50 | 0 | 69efacb5 | campaign/the-sexton | ambrose | 12 |
| 2026-08-14T00:32:21Z | rebaseline | 33.943 | 184.632 | 8.545 | 5.69 | 0 | 69efacb5 | campaign/the-sexton | ambrose | 12 |
| 2026-08-14T00:32:55Z | preflight | 3.222 | 0.397 | 0.641 | 0.32 | 0 | f6556e1a | campaign/the-sexton | ambrose | 12 |
| 2026-08-14T00:58:54Z | gate | 506.433 | 3088.289 | 206.998 | 6.51 | 0 | dcd568e4 | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T01:00:12Z | preflight | 3.531 | 0.413 | 0.679 | 0.31 | 0 | a9c8dd18 | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T01:41:34Z | quick | 18.092 | 26.779 | 17.951 | 2.47 | 0 | 439828a9 | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T01:50:03Z | gate | 405.969 | 2391.591 | 159.884 | 6.28 | 0 | 30e2a639 | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T11:48:19Z | quick | 21.014 | 38.530 | 27.699 | 3.15 | 0 | a1dc294f | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T12:20:56Z | rebaseline | 55.266 | 186.244 | 8.749 | 3.53 | 0 | a1dc294f | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T12:25:30Z | gate | 113.887 | 122.471 | 78.583 | 1.77 | 0 | a1dc294f | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T12:27:52Z | rebaseline | 33.667 | 184.877 | 8.428 | 5.74 | 0 | a1dc294f | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T12:29:57Z | gate | 87.849 | 414.909 | 41.108 | 5.19 | 0 | a1dc294f | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T12:40:17Z | gate | 136.024 | 532.587 | 84.780 | 4.54 | 0 | a1dc294f | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T12:42:56Z | gate | 130.190 | 584.973 | 77.217 | 5.09 | 0 | a1dc294f | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T12:45:53Z | gate | 133.721 | 640.218 | 79.782 | 5.38 | 0 | a1dc294f | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T12:49:06Z | gate | 133.844 | 638.007 | 79.670 | 5.36 | 0 | a1dc294f | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T12:57:56Z | gate | 180.529 | 875.101 | 89.584 | 5.34 | 0 | a1dc294f | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T13:04:49Z | quick | 11.586 | 8.442 | 0.753 | 0.79 | 0 | a1dc294f | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T14:08:10Z | quick | 10.273 | 16.248 | 17.941 | 3.33 | 0 | 7e3b61ee | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T14:08:19Z | quick | 8.084 | 11.278 | 16.428 | 3.43 | 0 | 7e3b61ee | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T14:08:43Z | quick | 4.505 | 6.240 | 5.652 | 2.64 | 0 | 7e3b61ee | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T14:09:14Z | quick | 15.835 | 18.594 | 14.959 | 2.12 | 0 | 7e3b61ee | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T14:11:36Z | quick | 10.283 | 7.952 | 0.623 | 0.83 | 0 | 7e3b61ee | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T15:07:32Z | quick | 6.845 | 9.452 | 9.600 | 2.78 | 0 | 84589e49 | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T15:08:10Z | quick | 21.110 | 27.303 | 32.219 | 2.82 | 0 | 84589e49 | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T15:16:51Z | rebaseline | 53.075 | 186.679 | 9.110 | 3.69 | 0 | 84589e49 | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T15:26:55Z | quick | 27.933 | 34.770 | 42.677 | 2.77 | 0 | 84589e49 | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T18:14:22Z | quick | 2.324 | 1.408 | 0.359 | 0.76 | 0 | 7d79b1d1 | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T18:15:32Z | quick | 33.529 | 50.964 | 58.217 | 3.26 | 0 | 7d79b1d1 | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T18:16:04Z | quick | 9.654 | 7.961 | 0.539 | 0.88 | 0 | 7d79b1d1 | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T03:04:01Z | prewarm | 234.141 | 1127.170 | 57.140 | 5.06 | 0 | 64b98cbd | campaign/the-hearsay | MacBookPro | 10 |
| 2026-08-14T03:24:47Z | quick | 29.071 | 45.763 | 10.191 | 1.92 | 0 | 68d2ac4c | campaign/the-hearsay | MacBookPro | 10 |
| 2026-08-14T03:26:39Z | quick | 28.023 | 37.404 | 11.029 | 1.73 | 0 | 75eb12be | campaign/the-hearsay | MacBookPro | 10 |
| 2026-08-14T03:27:25Z | quick | 19.492 | 11.401 | 0.517 | 0.61 | 0 | 75eb12be | campaign/the-hearsay | MacBookPro | 10 |
| 2026-08-14T03:37:32Z | rebaseline | 64.315 | 200.978 | 12.710 | 3.32 | 0 | 9a5ff2c2 | campaign/the-hearsay | MacBookPro | 10 |
| 2026-08-14T03:39:41Z | quick | 16.264 | 26.183 | 11.993 | 2.35 | 0 | 9a5ff2c2 | campaign/the-hearsay | MacBookPro | 10 |
| 2026-08-14T10:57:58Z | quick | 10.968 | 9.886 | 0.361 | 0.93 | 0 | 36e720d7 | campaign/the-hearsay | MacBookPro | 10 |
| 2026-08-14T10:58:27Z | quick | 12.640 | 10.069 | 0.408 | 0.83 | 0 | 36e720d7 | campaign/the-hearsay | MacBookPro | 10 |
| 2026-08-14T11:06:15Z | quick | 10.245 | 9.584 | 0.243 | 0.96 | 0 | 5783bfe5 | campaign/the-hearsay | MacBookPro | 10 |
| 2026-08-14T11:15:51Z | rebaseline | 72.354 | 196.623 | 8.519 | 2.84 | 0 | e7891131 | campaign/the-hearsay | MacBookPro | 10 |
| 2026-08-14T11:25:22Z | gate | 138.355 | 270.175 | 45.198 | 2.28 | 0 | e7891131 | campaign/the-hearsay | MacBookPro | 10 |
| 2026-08-14T11:26:58Z | gate | 66.315 | 50.687 | 6.803 | 0.87 | 0 | e7891131 | campaign/the-hearsay | MacBookPro | 10 |
| 2026-08-14T11:29:26Z | gate | 66.645 | 73.823 | 19.466 | 1.40 | 0 | e7891131 | campaign/the-hearsay | MacBookPro | 10 |
| 2026-08-14T10:55:05Z | quick | 19.290 | 43.248 | 9.851 | 2.75 | 0 | 876aa60c | campaign/the-axes | MacBookPro | 10 |
| 2026-08-14T11:07:47Z | quick | 14.909 | 26.452 | 7.498 | 2.28 | 0 | 37c2bcbd | campaign/the-axes | MacBookPro | 10 |
| 2026-08-14T11:14:14Z | quick | 33.956 | 24.017 | 10.119 | 1.01 | 0 | baa5be6a | campaign/the-axes | MacBookPro | 10 |
| 2026-08-14T11:20:28Z | quick | 12.425 | 10.195 | 0.471 | 0.86 | 0 | d7327e4c | campaign/the-axes | MacBookPro | 10 |
| 2026-08-14T11:32:55Z | quick | 29.580 | 25.782 | 9.983 | 1.21 | 0 | f4c58bdb | campaign/the-axes | MacBookPro | 10 |
| 2026-08-14T12:18:05Z | quick | 13.447 | 20.249 | 8.090 | 2.11 | 0 | 1923be0c | campaign/the-axes | MacBookPro | 10 |
| 2026-08-14T12:19:03Z | quick | 14.656 | 23.222 | 8.851 | 2.19 | 0 | 1923be0c | campaign/the-axes | MacBookPro | 10 |
| 2026-08-14T04:08:48Z | quick | 18.768 | 26.885 | 18.327 | 2.41 | 0 | a917db11 | campaign/the-gnomon | MacBookPro | 10 |
| 2026-08-14T04:13:21Z | rebaseline | 56.151 | 198.919 | 12.196 | 3.76 | 0 | 4d9570e9 | campaign/the-gnomon | MacBookPro | 10 |
| 2026-08-14T04:14:22Z | preflight | 3.747 | 0.798 | 0.637 | 0.38 | 0 | a1a33028 | campaign/the-gnomon | MacBookPro | 10 |
| 2026-08-14T04:22:26Z | gate | 301.681 | 2518.280 | 119.022 | 8.74 | 0 | 01ccce74 | campaign/the-gnomon | MacBookPro | 10 |
| 2026-08-14T11:09:07Z | rebaseline | 55.293 | 198.773 | 12.004 | 3.81 | 0 | 1d9e342c | campaign/the-gnomon | MacBookPro | 10 |
| 2026-08-14T11:10:03Z | quick | 14.410 | 15.063 | 7.572 | 1.57 | 0 | 1d9e342c | campaign/the-gnomon | MacBookPro | 10 |
| 2026-08-14T11:19:08Z | gate | 540.763 | 2548.152 | 154.537 | 5.00 | 0 | 29d815e5 | campaign/the-gnomon | MacBookPro | 10 |
| 2026-08-14T11:31:51Z | gate | 575.018 | 2545.669 | 151.611 | 4.69 | 0 | 29d815e5 | campaign/the-gnomon | MacBookPro | 10 |
| 2026-08-14T12:03:07Z | gate | 320.654 | 2503.168 | 122.491 | 8.19 | 0 | 29d815e5 | campaign/the-gnomon | MacBookPro | 10 |
| 2026-08-14T12:10:41Z | gate | 316.402 | 2546.647 | 121.706 | 8.43 | 0 | 67fd64f6 | campaign/the-gnomon | MacBookPro | 10 |
| 2026-08-14T12:11:42Z | rebaseline | 56.150 | 205.716 | 16.243 | 3.95 | 0 | 67fd64f6 | campaign/the-gnomon | MacBookPro | 10 |
| 2026-08-14T12:18:07Z | rebaseline | 81.088 | 201.545 | 11.315 | 2.63 | 0 | c1a50f37 | the-repose | MacBookPro | 10 |
| 2026-08-14T12:18:56Z | quick | 22.534 | 32.620 | 25.615 | 2.58 | 0 | c1a50f37 | the-repose | MacBookPro | 10 |
| 2026-08-14T12:32:42Z | gate | 441.969 | 2948.757 | 171.655 | 7.06 | 0 | 72826827 | the-repose | MacBookPro | 10 |
| 2026-08-14T12:47:19Z | gate | 102.049 | 97.231 | 68.113 | 1.62 | 0 | 3370ad9f | the-repose | MacBookPro | 10 |
| 2026-08-14T12:56:54Z | gate | 522.478 | 2620.341 | 199.732 | 5.40 | 0 | 3370ad9f | the-repose | MacBookPro | 10 |
| 2026-08-14T13:04:44Z | gate | 307.689 | 2574.047 | 118.257 | 8.75 | 0 | 3370ad9f | the-repose | MacBookPro | 10 |
| 2026-08-14T13:05:47Z | quick | 10.202 | 9.497 | 0.279 | 0.96 | 0 | 3370ad9f | the-repose | MacBookPro | 10 |
| 2026-08-14T13:39:50Z | rebaseline | 56.407 | 202.623 | 11.640 | 3.80 | 0 | 1efd2c9e | the-repose | MacBookPro | 10 |
| 2026-08-14T13:41:47Z | gate | 102.482 | 342.910 | 64.395 | 3.97 | 0 | 1efd2c9e | the-repose | MacBookPro | 10 |
| 2026-08-14T13:43:09Z | rebaseline | 36.767 | 199.051 | 12.805 | 5.76 | 0 | 1efd2c9e | the-repose | MacBookPro | 10 |
| 2026-08-14T13:51:10Z | gate | 310.129 | 2579.068 | 118.156 | 8.70 | 0 | 1efd2c9e | the-repose | MacBookPro | 10 |
| 2026-08-14T13:53:25Z | quick | 10.381 | 9.684 | 0.259 | 0.96 | 0 | 1efd2c9e | the-repose | MacBookPro | 10 |
| 2026-08-14T14:19:44Z | rebaseline | 78.594 | 197.243 | 9.096 | 2.63 | 0 | c5930e24 | the-repose | MacBookPro | 10 |
| 2026-08-14T14:38:59Z | gate | 358.320 | 2586.426 | 167.747 | 7.69 | 0 | c5930e24 | the-repose | MacBookPro | 10 |
| 2026-08-14T14:41:07Z | quick | 10.351 | 9.600 | 0.257 | 0.95 | 0 | c5930e24 | the-repose | MacBookPro | 10 |
| 2026-08-14T15:15:53Z | rebaseline | 87.533 | 200.451 | 12.226 | 2.43 | 0 | 438b1a76 | the-repose | MacBookPro | 10 |
| 2026-08-14T15:26:12Z | gate | 439.693 | 2733.206 | 189.622 | 6.65 | 0 | 438b1a76 | the-repose | MacBookPro | 10 |
| 2026-08-14T15:27:23Z | quick | 10.522 | 9.747 | 0.272 | 0.95 | 0 | 438b1a76 | the-repose | MacBookPro | 10 |
| 2026-08-14T15:51:45Z | rebaseline | 94.338 | 200.352 | 8.968 | 2.22 | 0 | bb289123 | the-repose | MacBookPro | 10 |
| 2026-08-14T16:13:13Z | gate | 552.840 | 2636.128 | 225.719 | 5.18 | 0 | bb289123 | the-repose | MacBookPro | 10 |
| 2026-08-14T16:20:46Z | gate | 306.420 | 2542.454 | 116.682 | 8.68 | 0 | bb289123 | the-repose | MacBookPro | 10 |
| 2026-08-14T16:21:34Z | quick | 10.250 | 9.563 | 0.274 | 0.96 | 0 | bb289123 | the-repose | MacBookPro | 10 |
| 2026-08-14T16:38:57Z | quick | 26.827 | 28.842 | 44.165 | 2.72 | 0 | 36f4f7bc | the-repose | MacBookPro | 10 |
| 2026-08-14T16:40:14Z | rebaseline | 48.030 | 198.096 | 11.188 | 4.36 | 0 | 3f123e0d | the-repose | MacBookPro | 10 |
| 2026-08-14T16:48:07Z | gate | 459.123 | 2647.621 | 188.329 | 6.18 | 0 | 3f123e0d | the-repose | MacBookPro | 10 |
| 2026-08-14T17:32:52Z | rebaseline | 34.481 | 195.106 | 11.162 | 5.98 | 0 | 7d33b8fc | the-repose | MacBookPro | 10 |
| 2026-08-14T17:39:36Z | gate | 353.524 | 2552.071 | 124.535 | 7.57 | 0 | c7c20c4c | the-repose | MacBookPro | 10 |
| 2026-08-14T18:04:42Z | rebaseline | 53.335 | 201.161 | 12.067 | 4.00 | 0 | 44b0ca06 | the-repose | MacBookPro | 10 |
| 2026-08-14T18:07:52Z | gate | 172.995 | 206.865 | 87.885 | 1.70 | 0 | 44b0ca06 | the-repose | MacBookPro | 10 |
| 2026-08-14T18:14:17Z | gate | 348.979 | 2589.168 | 127.573 | 7.78 | 0 | 44b0ca06 | the-repose | MacBookPro | 10 |
| 2026-08-14T18:15:15Z | rebaseline | 37.144 | 198.064 | 11.818 | 5.65 | 0 | 44b0ca06 | the-repose | MacBookPro | 10 |
| 2026-08-14T18:16:15Z | quick | 10.775 | 9.896 | 0.345 | 0.95 | 0 | 44b0ca06 | the-repose | MacBookPro | 10 |
| 2026-08-14T18:32:18Z | rebaseline | 88.936 | 198.997 | 9.685 | 2.35 | 0 | ad807d8b | the-repose | MacBookPro | 10 |
| 2026-08-14T12:17:21Z | rebaseline | 127.388 | 205.482 | 14.105 | 1.72 | 0 | 2922712e | main | MacBookPro | 10 |
| 2026-08-14T12:59:41Z | quick | 15.400 | 24.642 | 9.866 | 2.24 | 0 | d2ed7ebb | campaign/the-axes | MacBookPro | 10 |
| 2026-08-14T14:17:33Z | gate | 501.672 | 2765.821 | 146.820 | 5.81 | 0 | 5e667b4b | campaign/the-axes | MacBookPro | 10 |
| 2026-08-14T14:24:01Z | gate | 334.192 | 2523.643 | 116.869 | 7.90 | 0 | 5e667b4b | campaign/the-axes | MacBookPro | 10 |
| 2026-08-14T14:30:14Z | gate | 310.889 | 2505.804 | 113.713 | 8.43 | 0 | 5e667b4b | campaign/the-axes | MacBookPro | 10 |
| 2026-08-14T14:31:12Z | rebaseline | 38.594 | 195.737 | 10.980 | 5.36 | 0 | 5e667b4b | campaign/the-axes | MacBookPro | 10 |
| 2026-08-14T14:52:49Z | preflight | 3.419 | 0.790 | 0.595 | 0.41 | 0 | ff8eec1d | campaign/the-axes | MacBookPro | 10 |
| 2026-08-14T15:00:09Z | rebaseline | 73.403 | 196.509 | 10.948 | 2.83 | 0 | c32112c0 | main | MacBookPro | 10 |
| 2026-08-14T17:29:42Z | quick | 16.035 | 32.540 | 10.883 | 2.71 | 0 | 6f47ca24 | campaign/the-staff | MacBookPro | 10 |
| 2026-08-14T17:49:50Z | quick | 10.344 | 9.555 | 0.321 | 0.95 | 0 | a4c09d6f | campaign/the-staff | MacBookPro | 10 |
| 2026-08-14T17:50:17Z | quick | 10.315 | 9.557 | 0.315 | 0.96 | 0 | 1cf31753 | campaign/the-staff | MacBookPro | 10 |
| 2026-08-14T17:59:53Z | quick | 10.379 | 9.624 | 0.306 | 0.96 | 0 | 8cb710a9 | campaign/the-staff | MacBookPro | 10 |
| 2026-08-14T18:06:05Z | quick | 22.302 | 9.949 | 0.445 | 0.47 | 0 | e45d2cd6 | campaign/the-staff | MacBookPro | 10 |
| 2026-08-14T18:34:58Z | gate-commit | 38.460 | 58.075 | 20.852 | 2.05 | 0 | 281e7daa | campaign/the-staff | MacBookPro | 10 |
| 2026-08-14T18:35:22Z | gate-commit | 10.425 | 9.728 | 0.277 | 0.96 | 0 | 281e7daa | campaign/the-staff | MacBookPro | 10 |
| 2026-08-14T18:45:05Z | gate-commit | 470.818 | 1051.253 | 121.897 | 2.49 | 0 | 281e7daa | campaign/the-staff | MacBookPro | 10 |
| 2026-08-14T19:02:02Z | gate-commit | 15.569 | 38.395 | 9.650 | 3.09 | 0 | f5902979 | campaign/the-staff | MacBookPro | 10 |
| 2026-08-14T19:05:14Z | gate-commit | 84.863 | 89.220 | 68.326 | 1.86 | 0 | 00bc06dd | campaign/the-staff | MacBookPro | 10 |
| 2026-08-14T19:10:53Z | gate-commit | 16.985 | 40.039 | 10.614 | 2.98 | 0 | 7600fcb0 | campaign/the-staff | MacBookPro | 10 |
| 2026-08-14T19:13:11Z | gate-commit | 17.153 | 40.114 | 10.702 | 2.96 | 0 | 2787282d | campaign/the-staff | MacBookPro | 10 |
| 2026-08-14T19:15:57Z | gate-commit | 15.908 | 38.744 | 9.689 | 3.04 | 0 | e4f9cb9b | campaign/the-staff | MacBookPro | 10 |
| 2026-08-14T20:13:35Z | quick | 10.577 | 9.735 | 0.265 | 0.95 | 0 | 47830413 | campaign/the-staff | MacBookPro | 10 |
| 2026-08-14T20:42:44Z | quick | 23.645 | 27.880 | 34.787 | 2.65 | 0 | dc0b08a6 | campaign/the-staff | MacBookPro | 10 |
| 2026-08-14T20:57:38Z | lane:outboard | 855.222 | 4720.972 | 378.628 | 5.96 | 0 | 9de7fc9b | campaign/the-staff | MacBookPro | 10 |
| 2026-08-14T21:02:43Z | lane:outboard | 17.548 | 26.754 | 25.862 | 3.00 | 0 | 690ecb9d | campaign/the-staff | MacBookPro | 10 |
| 2026-08-14T21:17:07Z | lane:seam-guard | 853.284 | 4731.543 | 366.363 | 5.97 | 0 | 690ecb9d | campaign/the-staff | MacBookPro | 10 |
| 2026-08-14T22:13:30Z | preflight | 3.238 | 0.813 | 0.614 | 0.44 | 0 | 0e64f598 | campaign/the-staff | MacBookPro | 10 |
| 2026-08-14T22:13:47Z | preflight | 2.730 | 0.807 | 0.617 | 0.52 | 0 | 0e64f598 | campaign/the-staff | MacBookPro | 10 |
| 2026-08-14T18:37:14Z | rebaseline | 67.760 | 201.226 | 12.645 | 3.16 | 0 | aaa4c8c2 | the-repose | MacBookPro | 10 |
| 2026-08-14T18:39:37Z | quick | 125.494 | 76.843 | 113.601 | 1.52 | 0 | aaa4c8c2 | the-repose | MacBookPro | 10 |
| 2026-08-14T18:50:17Z | gate | 632.300 | 2782.018 | 193.304 | 4.71 | 0 | 5270a9e9 | the-repose | MacBookPro | 10 |
| 2026-08-14T18:57:33Z | gate | 358.186 | 2534.929 | 120.739 | 7.41 | 0 | 5270a9e9 | the-repose | MacBookPro | 10 |
| 2026-08-14T15:12:44Z | rebaseline | 73.542 | 200.442 | 10.755 | 2.87 | 0 | 1bc34a99 | campaign/the-hearsay | MacBookPro | 10 |
| 2026-08-14T15:13:36Z | quick | 19.942 | 44.267 | 17.547 | 3.10 | 0 | 1bc34a99 | campaign/the-hearsay | MacBookPro | 10 |
| 2026-08-14T15:36:53Z | census | 882.487 | 27996.712 | 330.979 | 32.10 | 0 | 17e0525a |  | lefford | 40 |
| 2026-08-14T18:32:47Z | gate | 380.333 | 2516.308 | 116.721 | 6.92 | 0 | 8278f0be | campaign/the-hearsay | MacBookPro | 10 |
| 2026-08-14T19:57:18Z | quick | 34.299 | 67.147 | 57.167 | 3.62 | 0 | dfb96996 | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T20:17:52Z | quick | 29.729 | 35.406 | 50.291 | 2.88 | 0 | 14521c43 | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T20:54:41Z | rebaseline | 56.861 | 185.992 | 8.132 | 3.41 | 0 | fca5a3de | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T21:06:43Z | quick | 24.840 | 30.096 | 41.706 | 2.89 | 0 | fca5a3de | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T22:29:35Z | rebaseline | 61.413 | 201.886 | 9.638 | 3.44 | 0 | 2ad0bb19 | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T22:40:01Z | gate | 234.254 | 207.711 | 151.314 | 1.53 | 0 | 2ad0bb19 | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T22:43:56Z | gate | 37.472 | 62.607 | 10.432 | 1.95 | 0 | 2ad0bb19 | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T22:45:02Z | rebaseline | 37.459 | 204.946 | 9.354 | 5.72 | 0 | 2ad0bb19 | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T22:46:54Z | gate | 99.205 | 446.240 | 47.443 | 4.98 | 0 | 2ad0bb19 | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T22:54:33Z | gate | 196.011 | 639.619 | 128.796 | 3.92 | 0 | 2ad0bb19 | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T23:00:23Z | gate | 179.954 | 700.155 | 112.707 | 4.52 | 0 | 2ad0bb19 | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T23:09:06Z | gate | 1.691 | 1.245 | 0.164 | 0.83 | 0 | 2ad0bb19 | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T23:12:59Z | gate | 217.996 | 780.288 | 116.853 | 4.12 | 0 | 2ad0bb19 | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T23:50:02Z | rebaseline | 52.196 | 204.942 | 9.037 | 4.10 | 0 | 2ad0bb19 | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-14T23:52:04Z | quick | 23.466 | 22.726 | 29.565 | 2.23 | 0 | 2ad0bb19 | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-15T00:37:48Z | quick | 39.820 | 43.208 | 69.727 | 2.84 | 0 | 4c806ece | campaign/the-glasshouse | ambrose | 12 |
| 2026-08-15T13:03:57Z | preflight | 4.203 | 0.868 | 0.679 | 0.37 | 0 | a2bfb602 | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-14T23:00:33Z | quick | 35.638 | 57.084 | 62.932 | 3.37 | 0 | 76deda29 | campaign/the-staff | MacBookPro | 10 |
| 2026-08-14T23:15:36Z | rebaseline | 32.896 | 197.294 | 11.488 | 6.35 | 0 | 8caab759 | campaign/the-staff | MacBookPro | 10 |
| 2026-08-14T23:28:51Z | quick | 10.216 | 9.506 | 0.255 | 0.96 | 0 | e9c9c283 | campaign/the-staff | MacBookPro | 10 |
| 2026-08-15T00:43:38Z | quick | 13.854 | 12.999 | 9.016 | 1.59 | 0 | 0686e893 | campaign/the-staff | MacBookPro | 10 |
| 2026-08-15T13:05:43Z | preflight | 2.586 | 0.828 | 0.677 | 0.58 | 0 | a2e5050d | campaign/the-staff | MacBookPro | 10 |
| 2026-08-14T19:22:26Z | rebaseline | 35.251 | 62.046 | 3.157 | 1.85 | 0 | 15c0bee8 | the-repose | MacBookPro | 10 |
| 2026-08-14T19:23:54Z | quick | 42.289 | 62.142 | 82.409 | 3.42 | 0 | 15c0bee8 | the-repose | MacBookPro | 10 |
| 2026-08-14T19:33:02Z | gate | 542.308 | 2828.932 | 210.373 | 5.60 | 0 | b98d6853 | the-repose | MacBookPro | 10 |
| 2026-08-14T19:41:58Z | preflight | 2.262 | 1.053 | 0.828 | 0.83 | 0 | 3da51569 | the-repose | MacBookPro | 10 |
| 2026-08-14T20:02:13Z | rebaseline | 55.758 | 196.988 | 10.732 | 3.73 | 0 | 3da51569 | the-repose | MacBookPro | 10 |
| 2026-08-14T20:05:46Z | quick | 34.549 | 36.849 | 65.503 | 2.96 | 0 | 3da51569 | the-repose | MacBookPro | 10 |
| 2026-08-14T21:43:07Z | gate | 512.947 | 2668.916 | 204.075 | 5.60 | 0 | 53bbc1ee | the-repose | MacBookPro | 10 |
| 2026-08-14T22:31:45Z | gate | 345.076 | 2515.712 | 121.416 | 7.64 | 0 | 53bbc1ee | the-repose | MacBookPro | 10 |
| 2026-08-14T22:36:43Z | rebaseline | 145.989 | 201.048 | 11.483 | 1.46 | 0 | 5919375d | main | MacBookPro | 10 |
| 2026-08-15T01:58:41Z | quick | 22.242 | 53.562 | 25.238 | 3.54 | 0 | c9fb7701 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T13:04:39Z | preflight | 2.597 | 0.858 | 0.661 | 0.58 | 0 | c4d9f756 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T13:04:50Z | preflight | 2.560 | 0.848 | 0.640 | 0.58 | 0 | c4d9f756 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T00:18:34Z | gate | 878.425 | 3212.096 | 529.032 | 4.26 | 0 | c9fb7701 | main | MacBookPro | 10 |
| 2026-08-15T13:05:13Z | preflight | 2.420 | 0.819 | 0.616 | 0.59 | 0 | 56e65760 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T13:15:48Z | quick | 16.762 | 41.902 | 26.098 | 4.06 | 0 | fb51bce1 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T13:19:23Z | quick | 39.626 | 36.513 | 23.889 | 1.52 | 0 | fb51bce1 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T13:22:20Z | quick | 24.880 | 36.029 | 24.358 | 2.43 | 0 | a161568b | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T14:06:51Z | quick | 21.551 | 35.819 | 29.962 | 3.05 | 0 | 53828f54 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T14:25:37Z | quick | 29.835 | 32.738 | 33.883 | 2.23 | 0 | 197cf430 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T14:27:01Z | quick | 15.213 | 11.669 | 0.370 | 0.79 | 0 | 197cf430 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T14:39:53Z | quick | 13.959 | 18.883 | 7.174 | 1.87 | 0 | 19a05efc | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T13:19:30Z | quick | 41.183 | 48.483 | 42.616 | 2.21 | 0 | 577fe6b8 | campaign/the-staff | MacBookPro | 10 |
| 2026-08-15T13:26:22Z | gate-commit | 219.270 | 529.785 | 74.511 | 2.76 | 0 | d3df5f3c | campaign/the-staff | MacBookPro | 10 |
| 2026-08-15T15:20:35Z | gate-commit | 85.599 | 39.445 | 11.006 | 0.59 | 0 | d3df5f3c | campaign/the-staff | MacBookPro | 10 |
| 2026-08-15T00:18:34Z | gate | 878.425 | 3212.096 | 529.032 | 4.26 | 0 | c9fb7701 | main | MacBookPro | 10 |
| 2026-08-15T13:08:11Z | rebaseline | 84.678 | 212.559 | 10.762 | 2.64 | 0 | 127e3ce8 | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-15T13:09:46Z | quick | 22.224 | 52.588 | 24.356 | 3.46 | 0 | 127e3ce8 | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-15T13:12:33Z | gate | 143.501 | 475.872 | 51.308 | 3.67 | 0 | c040e53c | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-15T13:16:36Z | rebaseline | 34.134 | 207.930 | 10.370 | 6.40 | 0 | c040e53c | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-15T13:37:40Z | quick | 10.904 | 10.175 | 0.423 | 0.97 | 0 | 86d68530 | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-15T16:04:28Z | census | 979.539 | 31393.080 | 293.656 | 32.35 | 0 | 8c221747 |  | lefford | 40 |
| 2026-08-15T15:21:02Z | gate-commit | 16.309 | 39.219 | 9.909 | 3.01 | 0 | 9095e27f | campaign/the-staff | MacBookPro | 10 |
| 2026-08-15T14:59:16Z | quick | 13.802 | 11.114 | 0.403 | 0.83 | 0 | dc975beb | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T15:24:10Z | gate-commit | 3.333 | 3.111 | 0.416 | 1.06 | 0 | 34eeb081 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T15:24:38Z | gate-commit | 13.518 | 14.337 | 8.423 | 1.68 | 0 | 34eeb081 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T15:28:04Z | gate-commit | 195.168 | 746.594 | 68.516 | 4.18 | 0 | 34eeb081 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T15:29:03Z | gate-commit | 19.465 | 40.317 | 10.805 | 2.63 | 0 | 34eeb081 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T16:40:14Z | gate-commit | 16.048 | 18.225 | 9.173 | 1.71 | 0 | 3d6aebd6 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T16:42:01Z | gate-commit | 84.413 | 114.040 | 27.245 | 1.67 | 0 | 3d6aebd6 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T17:46:10Z | gate-commit | 59.119 | 65.027 | 37.412 | 1.73 | 0 | bfd65307 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T17:49:33Z | gate-commit | 48.435 | 65.208 | 39.296 | 2.16 | 0 | c3abd9df | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T18:01:52Z | gate-commit | 53.185 | 67.040 | 41.876 | 2.05 | 0 | b2f00256 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T18:08:54Z | gate-commit | 58.297 | 69.375 | 44.263 | 1.95 | 0 | 60053729 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T18:12:11Z | gate-commit | 19.352 | 47.296 | 10.728 | 3.00 | 0 | f87f562a | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T18:35:33Z | gate-commit | 148.929 | 182.076 | 112.329 | 1.98 | 0 | a7ed8335 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T18:40:41Z | gate-commit | 172.239 | 202.572 | 120.549 | 1.88 | 0 | a7ed8335 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T18:42:36Z | gate-commit | 17.318 | 46.120 | 10.634 | 3.28 | 0 | a7ed8335 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T18:44:02Z | gate-commit | 17.309 | 46.255 | 10.359 | 3.27 | 0 | c393bf69 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T19:13:44Z | rebaseline | 50.286 | 195.927 | 11.987 | 4.13 | 0 | c393bf69 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T16:10:43Z | gate-commit | 139.630 | 88.018 | 94.071 | 1.30 | 0 | c0211b18 | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-15T16:30:54Z | gate-commit | 103.227 | 40.367 | 23.486 | 0.62 | 0 | c0211b18 | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-15T17:18:54Z | gate-commit | 133.610 | 90.466 | 106.473 | 1.47 | 0 | c252f9a8 | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-15T14:23:19Z | rebaseline | 49.449 | 212.862 | 11.217 | 4.53 | 0 | 69fdaeb1 | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-15T14:33:38Z | rebaseline | 34.025 | 209.652 | 10.696 | 6.48 | 0 | 69fdaeb1 | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-15T14:34:54Z | quick | 30.518 | 31.388 | 23.349 | 1.79 | 0 | 69fdaeb1 | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-15T15:05:49Z | quick | 24.327 | 29.774 | 28.396 | 2.39 | 0 | 0cdd1445 | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-15T17:36:16Z | gate-commit | 80.916 | 60.722 | 77.773 | 1.71 | 0 | fb398743 | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-15T17:39:30Z | gate-commit | 137.010 | 145.564 | 110.010 | 1.87 | 0 | fb398743 | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-15T18:31:14Z | gate-commit | 159.593 | 192.220 | 130.200 | 2.02 | 0 | c4aead2e | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-15T18:39:56Z | rebaseline | 99.103 | 217.676 | 11.417 | 2.31 | 0 | 55213204 | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-15T18:40:55Z | gate-commit | 18.871 | 41.929 | 9.999 | 2.75 | 0 | 1516d8e1 | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-15T20:15:14Z | gate-commit | 157.406 | 114.417 | 143.807 | 1.64 | 0 | 531390b2 | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-15T20:15:32Z | gate-commit | 12.344 | 14.194 | 2.313 | 1.34 | 0 | 531390b2 | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-15T20:16:11Z | gate-commit | 16.430 | 41.401 | 9.973 | 3.13 | 0 | 531390b2 | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-15T20:16:34Z | gate-commit | 16.249 | 41.433 | 9.997 | 3.17 | 0 | 531390b2 | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-15T20:29:51Z | gate-commit | 17.754 | 42.542 | 10.492 | 2.99 | 0 | 3eb50a51 | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-15T21:11:20Z | gate-commit | 18.051 | 42.834 | 10.812 | 2.97 | 0 | 72d2d1ad | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-15T21:12:03Z | gate-commit | 16.120 | 41.445 | 10.134 | 3.20 | 0 | 72d2d1ad | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-15T21:12:49Z | gate-commit | 16.334 | 41.446 | 10.109 | 3.16 | 0 | cf4f131f | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-15T23:21:55Z | gate-commit | 99.509 | 44.605 | 13.430 | 0.58 | 0 | 728334a1 | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-15T23:33:03Z | preflight | 2.900 | 0.939 | 0.833 | 0.61 | 0 | e854d973 | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-15T14:36:45Z | prewarm | 159.339 | 1137.387 | 58.180 | 7.50 | 0 | f9dd35f6 | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T15:08:32Z | quick | 11.498 | 31.865 | 7.594 | 3.43 | 0 | 07565bc0 | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T15:12:13Z | quick | 10.498 | 9.792 | 0.264 | 0.96 | 0 | 07565bc0 | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T15:21:02Z | gate-commit | 16.309 | 39.219 | 9.909 | 3.01 | 0 | 9095e27f | campaign/the-staff | MacBookPro | 10 |
| 2026-08-15T15:26:49Z | preflight | 6.391 | 0.878 | 0.715 | 0.25 | 0 | 0bb7a6ce | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T15:32:20Z | gate-commit | 66.769 | 76.296 | 17.417 | 1.40 | 0 | 61a73c35 | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T15:32:27Z | preflight | 3.553 | 0.855 | 0.670 | 0.43 | 0 | cb754783 | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T15:35:19Z | rebaseline | 48.322 | 192.674 | 10.457 | 4.20 | 0 | cb754783 | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T15:54:00Z | gate-commit | 134.972 | 383.782 | 91.037 | 3.52 | 0 | 152f278c | campaign/the-ballast | MacBookPro | 10 |
| 2026-08-15T16:47:24Z | gate-commit | 193.621 | 144.801 | 124.145 | 1.39 | 0 | bf0a87e5 | campaign/the-ballast | MacBookPro | 10 |
| 2026-08-15T17:57:45Z | gate-commit | 73.142 | 84.447 | 99.990 | 2.52 | 0 | 13b07c4f | campaign/the-ballast | MacBookPro | 10 |
| 2026-08-15T18:16:34Z | gate-commit | 19.265 | 39.752 | 10.340 | 2.60 | 0 | 6b401dc3 | campaign/the-ballast | MacBookPro | 10 |
| 2026-08-15T18:24:46Z | preflight | 3.190 | 0.856 | 0.629 | 0.47 | 0 | 95efc676 | campaign/the-ballast | MacBookPro | 10 |
| 2026-08-15T18:46:49Z | gate-commit | 49.133 | 39.057 | 10.478 | 1.01 | 0 | 95efc676 | campaign/the-ballast | MacBookPro | 10 |
| 2026-08-15T18:54:08Z | rebaseline | 45.698 | 194.319 | 11.417 | 4.50 | 0 | e814df0c | campaign/the-ballast | MacBookPro | 10 |
| 2026-08-15T19:53:48Z | preflight | 3.164 | 0.841 | 0.617 | 0.46 | 0 | f08c7ab5 | campaign/the-ballast | MacBookPro | 10 |
| 2026-08-15T21:27:51Z | gate-commit | 180.009 | 246.152 | 97.336 | 1.91 | 0 | 7715d705 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T21:28:16Z | gate-commit | 24.767 | 49.376 | 10.879 | 2.43 | 0 | 6a216e48 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T21:30:08Z | preflight | 4.190 | 0.991 | 0.849 | 0.44 | 0 | 6a216e48 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T22:29:21Z | preflight | 4.700 | 1.145 | 1.093 | 0.48 | 0 | 1b535648 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T22:34:30Z | rebaseline | 155.454 | 213.879 | 21.204 | 1.51 | 0 | 6c3e4975 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T22:37:56Z | gate-commit | 205.272 | 51.724 | 15.579 | 0.33 | 0 | 6c3e4975 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-16T00:01:00Z | gate-commit | 64.715 | 46.390 | 11.693 | 0.90 | 0 | 6c3e4975 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-16T00:01:28Z | gate-commit | 17.675 | 46.179 | 10.569 | 3.21 | 0 | 6c3e4975 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-15T16:10:10Z | gate-commit | 64.597 | 50.279 | 17.561 | 1.05 | 0 | faf184fe | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T16:32:41Z | gate-commit | 28.295 | 39.499 | 11.005 | 1.78 | 0 | f5d452f0 | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T16:49:05Z | rebaseline | 37.205 | 197.455 | 11.846 | 5.63 | 0 | 59b5eb4a | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T16:52:29Z | gate-commit | 27.804 | 46.165 | 16.429 | 2.25 | 0 | 59b5eb4a | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T17:01:03Z | gate-commit | 33.275 | 45.537 | 16.413 | 1.86 | 0 | f88d5c0e | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T17:07:56Z | gate-commit | 16.338 | 39.583 | 10.029 | 3.04 | 0 | 0087b428 | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T17:32:42Z | rebaseline | 37.775 | 194.050 | 11.157 | 5.43 | 0 | 12148929 | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T17:34:20Z | gate-commit | 25.609 | 40.463 | 10.507 | 1.99 | 0 | 12148929 | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T17:48:03Z | rebaseline | 33.709 | 194.303 | 10.817 | 6.09 | 0 | 11056a28 | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T18:09:06Z | rebaseline | 18.252 | 63.604 | 4.083 | 3.71 | 0 | f622f7ea | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T18:11:30Z | rebaseline | 37.869 | 197.015 | 11.939 | 5.52 | 0 | f622f7ea | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T18:20:42Z | gate-commit | 17.913 | 39.324 | 10.367 | 2.77 | 0 | f622f7ea | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T18:36:24Z | rebaseline | 38.240 | 197.252 | 11.469 | 5.46 | 0 | 70b0030e | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T18:53:06Z | rebaseline | 32.981 | 194.693 | 10.692 | 6.23 | 0 | 4c10140b | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T19:30:00Z | rebaseline | 37.045 | 196.105 | 10.644 | 5.58 | 0 | a6d18f52 | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T19:33:59Z | gate-commit | 27.325 | 39.435 | 10.931 | 1.84 | 0 | a6d18f52 | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T20:05:46Z | rebaseline | 36.697 | 198.494 | 11.674 | 5.73 | 0 | 847a7c54 | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T20:08:07Z | rebaseline | 35.317 | 197.247 | 11.512 | 5.91 | 0 | 847a7c54 | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T20:09:11Z | gate-commit | 19.614 | 40.368 | 10.974 | 2.62 | 0 | 847a7c54 | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T20:22:05Z | rebaseline | 32.906 | 197.926 | 11.845 | 6.37 | 0 | 9cc2fb25 | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T20:45:52Z | rebaseline | 33.325 | 198.233 | 12.302 | 6.32 | 0 | fdeff9a2 | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T20:47:14Z | gate-commit | 28.841 | 39.304 | 10.749 | 1.74 | 0 | fdeff9a2 | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T20:57:46Z | gate-commit | 26.327 | 44.824 | 17.359 | 2.36 | 0 | 19779d72 | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T21:32:18Z | gate-commit | 113.376 | 264.459 | 63.299 | 2.89 | 0 | 84d516b3 | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T21:35:16Z | rebaseline | 48.418 | 198.240 | 12.423 | 4.35 | 0 | bea31c07 | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T21:38:51Z | gate-commit | 16.332 | 39.193 | 10.188 | 3.02 | 0 | 8dc48419 | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-15T22:28:36Z | preflight | 3.284 | 0.970 | 0.965 | 0.59 | 0 | bfd21abc | campaign/the-compendium | MacBookPro | 10 |
| 2026-08-16T00:50:21Z | gate-commit | 24.139 | 104.522 | 19.004 | 5.12 | 0 | 1a201f30 | campaign/the-sluice | lefford | 40 |
| 2026-08-16T00:51:39Z | gate-commit | 32.089 | 35.955 | 3.394 | 1.23 | 0 | 1a201f30 | campaign/the-sluice | lefford | 40 |
| 2026-08-16T01:04:43Z | gate-commit | 22.813 | 21.950 | 0.723 | 0.99 | 0 | 1a201f30 | campaign/the-sluice | lefford | 40 |
| 2026-08-16T01:06:10Z | gate-commit | 38.904 | 110.719 | 41.944 | 3.92 | 0 | 1a201f30 | campaign/the-sluice | lefford | 40 |
| 2026-08-16T01:07:50Z | gate-commit | 38.385 | 106.777 | 40.728 | 3.84 | 0 | 1a201f30 | campaign/the-sluice | lefford | 40 |
| 2026-08-16T01:19:32Z | gate-commit | 38.385 | 105.520 | 40.058 | 3.79 | 0 | 4524134b | campaign/the-sluice | lefford | 40 |
| 2026-08-16T02:24:45Z | gate-commit | 40.924 | 113.722 | 42.391 | 3.81 | 0 | f9142505 | campaign/the-sluice | lefford | 40 |
| 2026-08-16T02:54:08Z | gate-commit | 39.629 | 106.088 | 38.868 | 3.66 | 0 | 0667eebf | campaign/the-sluice | lefford | 40 |
| 2026-08-16T09:09:55Z | gate-commit | 38.291 | 107.284 | 37.800 | 3.79 | 0 | 54a4926a | campaign/the-sluice | lefford | 40 |
| 2026-08-16T09:51:09Z | gate-commit | 38.704 | 112.946 | 41.398 | 3.99 | 0 | 6d04333a | campaign/the-sluice | lefford | 40 |
| 2026-08-16T09:58:30Z | gate-commit | 38.712 | 110.614 | 40.131 | 3.89 | 0 | 63f95aa6 | campaign/the-sluice | lefford | 40 |
| 2026-08-16T10:03:29Z | gate-commit | 38.484 | 109.875 | 41.770 | 3.94 | 0 | eb7a4ccf | campaign/the-sluice | lefford | 40 |
| 2026-08-16T10:09:55Z | gate-commit | 37.588 | 106.040 | 41.367 | 3.92 | 0 | 2be0ddc3 | campaign/the-sluice | lefford | 40 |
| 2026-08-15T23:49:38Z | gate-commit | 42.830 | 39.281 | 72.541 | 2.61 | 0 | fbf3cd25 | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-15T23:52:55Z | gate-commit | 187.121 | 303.295 | 83.085 | 2.06 | 0 | fbf3cd25 | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-15T23:53:38Z | gate-commit | 20.368 | 43.410 | 11.368 | 2.69 | 0 | fbf3cd25 | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-15T23:53:51Z | preflight | 3.502 | 0.992 | 0.972 | 0.56 | 0 | 1e856f05 | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-16T12:43:14Z | gate-commit | 197.814 | 118.718 | 163.391 | 1.43 | 0 | 1e856f05 | campaign/the-glasshouse | MacBookPro | 10 |
| 2026-08-16T13:35:13Z | sluice:artifacts | 90.591 | 685.866 | 31.459 | 7.92 | 0 | f62bf653 |  | lefford | 40 |
| 2026-08-16T13:35:20Z | sluice:outboard | 5.664 | 8.566 | 24.250 | 5.79 | 0 | b5e0b882 |  | lefford | 40 |
| 2026-08-16T13:40:50Z | sluice:gate | 330.142 | 9002.093 | 460.972 | 28.66 | 0 | ecc799f5 |  | lefford | 40 |
| 2026-08-16T13:46:23Z | sluice:clients | 332.612 | 703.320 | 69.189 | 2.32 | 0 | b2aebbcc |  | lefford | 40 |
| 2026-08-16T14:17:47Z | sluice:heavy | 1883.754 | 30553.913 | 615.708 | 16.55 | 0 | 8ab963e5 |  | lefford | 40 |
| 2026-08-16T19:19:00Z | gate-commit | 29.011 | 44.173 | 19.406 | 2.19 | 0 | f8e937a8 | campaign/the-rhumb | MacBookPro | 10 |
| 2026-08-16T19:23:27Z | gate-commit | 228.531 | 647.536 | 64.419 | 3.12 | 0 | f8e937a8 | campaign/the-rhumb | MacBookPro | 10 |
| 2026-08-16T19:42:38Z | gate-commit | 54.129 | 71.761 | 37.999 | 2.03 | 0 | 24cd985e | campaign/the-rhumb | MacBookPro | 10 |
| 2026-08-16T20:05:07Z | gate-commit | 66.418 | 86.547 | 35.585 | 1.84 | 0 | 83ec4890 | campaign/the-rhumb | MacBookPro | 10 |
| 2026-08-16T20:42:25Z | gate-commit | 53.524 | 61.547 | 38.379 | 1.87 | 0 | 8339603c | campaign/the-rhumb | MacBookPro | 10 |
| 2026-08-16T21:14:21Z | gate-commit | 50.478 | 61.522 | 40.305 | 2.02 | 0 | 54f00563 | campaign/the-rhumb | MacBookPro | 10 |
| 2026-08-16T21:45:25Z | gate-commit | 53.961 | 62.620 | 41.929 | 1.94 | 0 | a5bc7445 | campaign/the-rhumb | MacBookPro | 10 |
| 2026-08-16T21:57:50Z | rebaseline | 47.691 | 215.698 | 11.253 | 4.76 | 0 | cfc71ac7 | campaign/the-rhumb | MacBookPro | 10 |
| 2026-08-16T15:13:33Z | gate-commit | 83.046 | 1150.303 | 205.303 | 16.32 | 0 | 89928b7c | campaign/the-sluice | lefford | 40 |
| 2026-08-16T15:39:33Z | gate-commit | 60.736 | 389.759 | 159.832 | 9.05 | 0 | d839ba31 | campaign/the-sluice | lefford | 40 |
| 2026-08-16T16:25:28Z | gate-commit | 58.958 | 378.289 | 153.573 | 9.02 | 0 | e2012af4 | campaign/the-sluice | lefford | 40 |
| 2026-08-16T16:31:06Z | gate-commit | 40.627 | 119.647 | 41.025 | 3.95 | 0 | e2012af4 | campaign/the-sluice | lefford | 40 |
| 2026-08-16T17:31:32Z | gate-commit | 44.737 | 120.518 | 41.045 | 3.61 | 0 | dbcfde15 | campaign/the-sluice | lefford | 40 |
| 2026-08-16T19:20:56Z | rebaseline | 142.224 | 213.799 | 9.696 | 1.57 | 0 | 6b36ac1f | campaign/the-begat | MacBookPro | 10 |
| 2026-08-16T19:21:40Z | gate-commit | 22.287 | 29.531 | 26.065 | 2.49 | 0 | 6b36ac1f | campaign/the-begat | MacBookPro | 10 |
| 2026-08-16T19:27:35Z | gate-commit | 342.320 | 786.049 | 164.907 | 2.78 | 0 | 6b36ac1f | campaign/the-begat | MacBookPro | 10 |
| 2026-08-16T19:28:51Z | gate-commit | 27.336 | 49.899 | 11.930 | 2.26 | 0 | 6b36ac1f | campaign/the-begat | MacBookPro | 10 |
| 2026-08-16T19:27:32Z | gate-commit | 62.558 | 52.386 | 85.564 | 2.21 | 0 | cc1552a6 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-16T19:28:40Z | rebaseline | 57.847 | 219.430 | 11.066 | 3.98 | 0 | cc1552a6 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-16T19:35:34Z | gate-commit | 136.754 | 36.778 | 9.162 | 0.34 | 0 | cc1552a6 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-16T19:36:41Z | gate-commit | 20.207 | 35.624 | 8.331 | 2.18 | 0 | cc1552a6 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-16T19:41:53Z | gate-commit | 24.295 | 56.571 | 12.734 | 2.85 | 0 | cc1552a6 | campaign/the-retelling | MacBookPro | 10 |
| 2026-08-16T20:14:10Z | rebaseline | 118.587 | 215.781 | 8.800 | 1.89 | 0 | 59afd515 | campaign/the-begat | MacBookPro | 10 |
| 2026-08-16T20:19:42Z | gate-commit | 312.423 | 749.634 | 197.225 | 3.03 | 0 | 59afd515 | campaign/the-begat | MacBookPro | 10 |
| 2026-08-16T20:20:49Z | gate-commit | 22.631 | 55.229 | 12.026 | 2.97 | 0 | 59afd515 | campaign/the-begat | MacBookPro | 10 |
| 2026-08-16T20:31:37Z | gate-commit | 22.503 | 55.723 | 11.948 | 3.01 | 0 | 85b8d3bb | campaign/the-begat | MacBookPro | 10 |
| 2026-08-16T20:34:06Z | gate-commit | 33.563 | 60.453 | 12.786 | 2.18 | 0 | 4383074c | campaign/the-begat | MacBookPro | 10 |
| 2026-08-16T20:41:32Z | sluice:artifacts | 165.291 | 1465.874 | 87.453 | 9.40 | 0 | 32ac0933 |  | lefford | 40 |
| 2026-08-16T20:41:53Z | sluice:outboard | 20.530 | 46.691 | 28.698 | 3.67 | 0 | da17115f |  | lefford | 40 |
| 2026-08-16T20:48:57Z | sluice:gate | 423.206 | 11558.833 | 619.944 | 28.78 | 0 | 5eb71564 |  | lefford | 40 |
| 2026-08-16T21:05:01Z | sluice:seam-guard | 964.414 | 19474.820 | 1074.693 | 21.31 | 0 | f905923a |  | lefford | 40 |
| 2026-08-16T21:10:39Z | sluice:clients | 337.573 | 715.881 | 75.619 | 2.34 | 0 | 3d9e0acf |  | lefford | 40 |
| 2026-08-16T21:42:21Z | sluice:heavy | 1901.292 | 29944.400 | 740.613 | 16.14 | 0 | 5f59a1aa |  | lefford | 40 |
| 2026-08-16T23:35:30Z | census | 855.533 | 27774.646 | 307.433 | 32.82 | 0 | 1e92c152 |  | lefford | 40 |
| 2026-08-16T23:46:39Z | rebaseline | 123.305 | 1072.961 | 54.283 | 9.14 | 0 | 1e92c152 | follow-up/census-ratchet | lefford | 40 |
| 2026-08-17T00:06:54Z | sluice:artifacts | 88.558 | 664.675 | 31.468 | 7.86 | 0 | c25e08e6 |  | lefford | 40 |
| 2026-08-17T00:07:00Z | sluice:outboard | 5.678 | 8.487 | 24.026 | 5.73 | 0 | 73dd0f17 |  | lefford | 40 |
| 2026-08-17T00:12:49Z | sluice:gate | 348.287 | 9142.738 | 423.538 | 27.47 | 0 | 08d385ee |  | lefford | 40 |
| 2026-08-17T00:28:40Z | sluice:seam-guard | 950.887 | 19361.401 | 1043.845 | 21.46 | 0 | b52c5a5c |  | lefford | 40 |
| 2026-08-17T00:32:38Z | sluice:clients | 237.286 | 423.881 | 33.943 | 1.93 | 0 | 5dd69a0c |  | lefford | 40 |
| 2026-08-17T01:04:29Z | sluice:heavy | 1911.399 | 29772.539 | 662.669 | 15.92 | 0 | 87d74f2f |  | lefford | 40 |
| 2026-08-17T01:24:17Z | sluice:artifacts | 89.746 | 653.202 | 31.745 | 7.63 | 0 | 7c6d3741 |  | lefford | 40 |
| 2026-08-17T01:24:41Z | sluice:outboard | 23.764 | 12.561 | 33.541 | 1.94 | 0 | af55d4d4 |  | lefford | 40 |
| 2026-08-17T01:30:23Z | sluice:gate | 341.435 | 9114.894 | 405.374 | 27.88 | 0 | f725cfc3 |  | lefford | 40 |
| 2026-08-17T01:46:17Z | sluice:seam-guard | 953.421 | 19376.201 | 1049.566 | 21.42 | 0 | 81c4d1f2 |  | lefford | 40 |
| 2026-08-17T01:50:24Z | sluice:clients | 247.207 | 427.562 | 33.407 | 1.86 | 0 | 991cb9e4 |  | lefford | 40 |
| 2026-08-17T02:21:38Z | sluice:heavy | 1873.557 | 29789.874 | 672.800 | 16.26 | 0 | 87e8bf1a |  | lefford | 40 |
| 2026-08-16T23:28:27Z | rebaseline | 81.991 | 214.778 | 11.980 | 2.77 | 0 | ec62ac56 | campaign/the-rhumb | MacBookPro | 10 |
| 2026-08-16T23:32:45Z | gate-commit | 234.486 | 750.463 | 132.149 | 3.76 | 0 | ec62ac56 | campaign/the-rhumb | MacBookPro | 10 |
| 2026-08-16T23:42:11Z | rebaseline | 44.934 | 219.707 | 15.544 | 5.24 | 0 | dc1238dc | campaign/the-rhumb | MacBookPro | 10 |
| 2026-08-16T23:43:20Z | gate-commit | 23.271 | 50.873 | 13.247 | 2.76 | 0 | 92a4b059 | campaign/the-rhumb | MacBookPro | 10 |
| 2026-08-16T23:47:36Z | gate-commit | 24.862 | 51.709 | 13.213 | 2.61 | 0 | 7c1d3e24 | campaign/the-rhumb | MacBookPro | 10 |
| 2026-08-17T00:05:01Z | vessel-check | 49.883 | 70.386 | 5.942 | 1.53 | 0 | c1ac3568 | campaign/the-rhumb | MacBookPro | 10 |
| 2026-08-17T00:11:15Z | gate-commit | 101.615 | 118.068 | 68.745 | 1.84 | 0 | 35227e3b | campaign/the-rhumb | MacBookPro | 10 |
| 2026-08-17T00:20:09Z | vessel-check | 75.126 | 38.377 | 2.533 | 0.54 | 0 | a21dceb7 | campaign/the-rhumb | MacBookPro | 10 |
| 2026-08-17T00:21:00Z | game-check | 47.844 | 123.110 | 5.502 | 2.69 | 0 | a21dceb7 | campaign/the-rhumb | MacBookPro | 10 |
| 2026-08-17T00:22:30Z | gate-commit | 81.638 | 77.031 | 61.220 | 1.69 | 0 | a21dceb7 | campaign/the-rhumb | MacBookPro | 10 |
| 2026-08-17T00:23:28Z | gate-commit | 27.535 | 51.768 | 13.438 | 2.37 | 0 | a21dceb7 | campaign/the-rhumb | MacBookPro | 10 |
| 2026-08-17T00:24:25Z | gate-commit | 29.056 | 52.949 | 14.222 | 2.31 | 0 | 3dcbdf25 | campaign/the-rhumb | MacBookPro | 10 |
| 2026-08-17T00:33:53Z | gate-commit | 28.160 | 39.303 | 10.489 | 1.77 | 0 | cc8cb292 | campaign/the-rhumb | MacBookPro | 10 |
| 2026-08-17T00:34:47Z | gate-commit | 31.633 | 53.614 | 14.803 | 2.16 | 0 | cc8cb292 | campaign/the-rhumb | MacBookPro | 10 |
| 2026-08-17T00:36:17Z | gate-commit | 28.307 | 53.562 | 14.363 | 2.40 | 0 | cc8cb292 | campaign/the-rhumb | MacBookPro | 10 |
| 2026-08-17T00:38:35Z | gate-commit | 23.484 | 51.058 | 12.983 | 2.73 | 0 | 3908bdd4 | campaign/the-rhumb | MacBookPro | 10 |
| 2026-08-17T00:38:49Z | vessel-check | 14.159 | 14.825 | 0.702 | 1.10 | 0 | 3908bdd4 | campaign/the-rhumb | MacBookPro | 10 |
| 2026-08-17T00:39:31Z | game-check | 42.139 | 118.535 | 2.752 | 2.88 | 0 | 3908bdd4 | campaign/the-rhumb | MacBookPro | 10 |
| 2026-08-17T02:24:01Z | sluice:artifacts | 95.909 | 731.079 | 33.577 | 7.97 | 0 | 9aae0d27 |  | lefford | 40 |
| 2026-08-17T02:24:25Z | sluice:outboard | 23.392 | 12.006 | 32.303 | 1.89 | 0 | 1bc85658 |  | lefford | 40 |
| 2026-08-17T02:30:14Z | sluice:gate | 348.649 | 9302.803 | 413.553 | 27.87 | 0 | 1d73a6e5 |  | lefford | 40 |
| 2026-08-17T02:46:09Z | sluice:seam-guard | 955.121 | 19366.643 | 1045.522 | 21.37 | 0 | 068d1b8c |  | lefford | 40 |
| 2026-08-17T02:50:09Z | sluice:clients | 239.248 | 436.256 | 34.684 | 1.97 | 0 | e9eaa7c6 |  | lefford | 40 |
| 2026-08-17T03:20:49Z | sluice:heavy | 1839.783 | 29809.414 | 678.699 | 16.57 | 0 | 06a61774 |  | lefford | 40 |
| 2026-08-17T12:30:34Z | sluice:artifacts | 94.110 | 662.951 | 33.104 | 7.40 | 0 | 58feb338 |  | lefford | 40 |
| 2026-08-17T12:31:07Z | sluice:outboard | 32.895 | 20.707 | 34.389 | 1.67 | 0 | faeb5fb8 |  | lefford | 40 |
| 2026-08-17T12:36:39Z | sluice:gate | 331.929 | 9134.515 | 383.786 | 28.68 | 0 | 91002cdc |  | lefford | 40 |
| 2026-08-17T12:52:31Z | sluice:seam-guard | 951.506 | 19365.811 | 1049.211 | 21.46 | 0 | b60af966 |  | lefford | 40 |
| 2026-08-17T12:56:28Z | sluice:clients | 236.774 | 425.152 | 32.391 | 1.93 | 0 | 7a1c35a9 |  | lefford | 40 |
| 2026-08-17T13:28:09Z | sluice:heavy | 1900.933 | 29954.184 | 695.520 | 16.12 | 0 | b5ee7d2f |  | lefford | 40 |
| 2026-08-17T15:59:51Z | prewarm | 61.449 | 357.186 | 34.023 | 6.37 | 0 | f92bc68c | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T16:16:57Z | gate-commit | 208.061 | 97.512 | 71.261 | 0.81 | 0 | b0f20c71 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T16:18:14Z | gate-commit | 45.070 | 50.399 | 12.455 | 1.39 | 0 | b0f20c71 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T16:23:31Z | gate-commit | 47.009 | 50.430 | 12.038 | 1.33 | 0 | 5e2f9fa4 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T16:24:13Z | gate-commit | 25.894 | 51.456 | 12.771 | 2.48 | 0 | 5e2f9fa4 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T16:40:50Z | rebaseline | 133.009 | 224.918 | 16.460 | 1.81 | 0 | 11653fb0 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T16:43:06Z | gate-commit | 115.454 | 232.597 | 71.301 | 2.63 | 0 | 11653fb0 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T16:44:04Z | gate-commit | 40.018 | 52.896 | 13.285 | 1.65 | 0 | 11653fb0 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T16:51:01Z | gate-commit | 49.555 | 51.478 | 12.628 | 1.29 | 0 | 890be043 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T16:51:41Z | gate-commit | 21.978 | 50.404 | 12.355 | 2.86 | 0 | 890be043 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T17:15:04Z | rebaseline | 50.565 | 213.966 | 11.064 | 4.45 | 0 | 70ecc0df | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T17:22:52Z | rebaseline | 50.070 | 214.907 | 11.019 | 4.51 | 0 | 70ecc0df | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T17:24:35Z | gate-commit | 94.707 | 86.945 | 75.806 | 1.72 | 0 | 70ecc0df | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T17:27:19Z | gate-commit | 23.358 | 51.386 | 12.762 | 2.75 | 0 | 70ecc0df | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T17:28:12Z | gate-commit | 22.950 | 51.112 | 12.482 | 2.77 | 0 | 1d2ca5cc | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T17:51:16Z | rebaseline | 46.104 | 212.933 | 10.244 | 4.84 | 0 | 401a045b | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T17:52:45Z | gate-commit | 67.983 | 37.037 | 8.643 | 0.67 | 0 | 401a045b | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T17:55:27Z | gate-commit | 100.533 | 89.892 | 84.754 | 1.74 | 0 | 401a045b | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T18:11:47Z | rebaseline | 48.867 | 215.472 | 11.039 | 4.64 | 0 | 64233aca | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T18:13:37Z | gate-commit | 94.982 | 87.049 | 79.363 | 1.75 | 0 | 64233aca | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T18:14:21Z | gate-commit | 22.407 | 50.686 | 12.493 | 2.82 | 0 | 64233aca | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T18:22:04Z | rebaseline | 18.560 | 67.705 | 4.388 | 3.88 | 0 | 52836b48 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T18:22:52Z | rebaseline | 44.841 | 214.080 | 11.610 | 5.03 | 0 | 52836b48 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T18:31:46Z | gate-commit | 145.786 | 82.591 | 93.561 | 1.21 | 0 | 52836b48 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T18:34:06Z | gate-commit | 43.972 | 53.237 | 13.672 | 1.52 | 0 | 52836b48 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T18:46:01Z | gate-commit | 22.256 | 50.329 | 11.986 | 2.80 | 0 | 29ffefce | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T19:07:19Z | rebaseline | 49.281 | 213.494 | 10.983 | 4.56 | 0 | 692687d3 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T19:11:13Z | gate-commit | 117.195 | 115.039 | 138.447 | 2.16 | 0 | 692687d3 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T19:13:06Z | gate-commit | 23.622 | 50.951 | 13.074 | 2.71 | 0 | 692687d3 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T19:29:58Z | vessel-check | 50.873 | 64.566 | 5.136 | 1.37 | 0 | e4d0fb69 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T19:31:26Z | game-check | 82.257 | 181.139 | 19.314 | 2.44 | 0 | e4d0fb69 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T19:33:53Z | rebaseline | 138.550 | 226.044 | 17.556 | 1.76 | 0 | e4d0fb69 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T19:41:16Z | gate-commit | 289.452 | 91.318 | 116.735 | 0.72 | 0 | e4d0fb69 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T20:05:23Z | gate-commit | 106.733 | 80.563 | 72.698 | 1.44 | 0 | 6253689d | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T20:08:26Z | gate-commit | 55.175 | 50.422 | 12.917 | 1.15 | 0 | 6253689d | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T20:28:55Z | rebaseline | 48.163 | 216.349 | 11.301 | 4.73 | 0 | 8b60de81 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T20:35:43Z | gate-commit | 358.715 | 93.515 | 130.942 | 0.63 | 0 | 8b60de81 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T20:39:02Z | gate-commit | 82.271 | 50.305 | 12.333 | 0.76 | 0 | 8b60de81 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T20:47:52Z | gate-commit | 118.349 | 94.562 | 91.909 | 1.58 | 0 | 91df2587 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T01:39:37Z | gate-commit | 291.818 | 127.059 | 129.564 | 0.88 | 0 | 257a540c | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T01:41:11Z | gate-commit | 57.614 | 55.827 | 13.673 | 1.21 | 0 | 257a540c | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T01:57:11Z | gate-commit | 336.654 | 172.137 | 266.487 | 1.30 | 0 | 9f28df80 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T02:35:12Z | rebaseline | 62.859 | 212.274 | 11.169 | 3.55 | 0 | c7fc1207 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T02:43:31Z | vessel-check | 38.853 | 51.002 | 2.330 | 1.37 | 0 | c7fc1207 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T02:44:13Z | world-check | 37.569 | 51.082 | 2.059 | 1.41 | 0 | c7fc1207 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T02:45:24Z | game-check | 64.540 | 140.323 | 8.336 | 2.30 | 0 | c7fc1207 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T02:46:22Z | gate-commit | 41.371 | 55.908 | 15.328 | 1.72 | 0 | c7fc1207 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T02:46:48Z | gate-commit | 19.253 | 17.183 | 2.657 | 1.03 | 0 | c7fc1207 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T02:50:13Z | gate-commit | 57.884 | 50.738 | 12.393 | 1.09 | 0 | c7fc1207 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T03:30:52Z | rebaseline | 89.167 | 228.269 | 17.586 | 2.76 | 0 | 9a222976 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T03:52:18Z | vessel-check | 37.592 | 49.761 | 2.234 | 1.38 | 0 | 9a222976 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T03:53:12Z | game-check | 49.865 | 126.170 | 7.739 | 2.69 | 0 | 9a222976 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T03:54:25Z | gate-commit | 36.646 | 51.067 | 13.068 | 1.75 | 0 | 9a222976 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T03:54:54Z | gate-commit | 22.076 | 50.409 | 12.291 | 2.84 | 0 | 9a222976 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T04:03:52Z | vessel-check | 32.467 | 46.688 | 2.059 | 1.50 | 0 | 16b21b00 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T04:04:39Z | game-check | 45.299 | 121.854 | 7.608 | 2.86 | 0 | 16b21b00 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T04:05:51Z | rebaseline | 65.903 | 215.278 | 11.658 | 3.44 | 0 | 16b21b00 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T04:16:17Z | gate-commit | 559.445 | 229.207 | 318.188 | 0.98 | 0 | 16b21b00 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T04:18:15Z | gate-commit | 64.435 | 50.456 | 12.510 | 0.98 | 0 | 16b21b00 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T11:25:06Z | game-check | 52.086 | 124.245 | 2.347 | 2.43 | 0 | e75631e2 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T11:26:33Z | gate-commit | 68.495 | 52.373 | 13.882 | 0.97 | 0 | e75631e2 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T11:28:01Z | gate-commit | 22.034 | 50.246 | 12.395 | 2.84 | 0 | e75631e2 | campaign/the-illumination | MacBookPro | 10 |
