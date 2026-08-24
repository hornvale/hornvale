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

**One `gate-commit` row is contamination, not a cost datum: the 2026-08-17
`3857.559` s row at `37796ef1`** (The Underworld), against that gate's normal
~85 s on this host. It was measured during a runaway-`rg` storm — an extension
had 826 concurrent ripgrep processes against a load average of 417/701/648 on
ten cores — and the row's own `cpu_ratio` of **0.12** is the signature, which is
exactly what that column is for. The number is left as measured, per this
file's rule that no measured value is ever edited; only read it as a
contention sample. **Nothing downstream moved**: the per-test baseline
`docs/timings/test-baseline-<host>.tsv` was not written, because the Mac's
`gate-commit` does not rewrite it (the stage gate does), so neither the
sub-floor roster nor the duration alarm saw the storm. The neighbouring rows at
`cpu_ratio` 2.49 → 0.92 → 0.77 → 0.12 are the same storm ramping, and the
post-restart run at `37796ef1` on `Greyjoy` still read 0.25.

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
| 2026-08-17T00:52:32Z | gate-commit | 84.786 | 62.029 | 110.496 | 2.03 | 0 | fba8e8e4 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-17T00:56:36Z | gate-commit | 39.857 | 17.638 | 26.282 | 1.10 | 0 | fba8e8e4 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-17T01:01:05Z | gate-commit | 246.226 | 273.931 | 58.113 | 1.35 | 0 | fba8e8e4 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-17T01:19:44Z | gate-commit | 141.606 | 93.507 | 90.609 | 1.30 | 0 | b8ce5f5d | campaign/the-burr | MacBookPro | 10 |
| 2026-08-17T01:29:50Z | gate-commit | 30.951 | 16.948 | 26.211 | 1.39 | 0 | db54cdf4 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-17T01:32:16Z | gate-commit | 110.359 | 98.895 | 59.208 | 1.43 | 0 | db54cdf4 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-17T01:47:50Z | gate-commit | 168.165 | 78.920 | 95.364 | 1.04 | 0 | 7c5fe8b4 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-17T01:56:40Z | gate-commit | 52.871 | 52.345 | 13.343 | 1.24 | 0 | 0e7d5757 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-17T02:14:15Z | gate-commit | 28.764 | 53.138 | 13.933 | 2.33 | 0 | 9c46cc5a | campaign/the-burr | MacBookPro | 10 |
| 2026-08-17T00:20:23Z | prewarm | 82.092 | 384.996 | 41.104 | 5.19 | 0 | 1e92c152 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T00:54:29Z | gate-commit | 205.456 | 123.933 | 106.649 | 1.12 | 0 | 1e92c152 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T01:42:39Z | gate-commit | 48.006 | 56.513 | 16.418 | 1.52 | 0 | dcb1acc0 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T02:43:17Z | gate-commit | 226.736 | 103.714 | 106.944 | 0.93 | 0 | 3ee3aeb5 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T03:03:46Z | gate-commit | 26.257 | 52.069 | 13.358 | 2.49 | 0 | 8dbeff4f | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T03:45:33Z | gate-commit | 78.904 | 51.838 | 20.791 | 0.92 | 0 | 34368559 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T03:57:11Z | gate-commit | 356.923 | 108.235 | 164.853 | 0.77 | 0 | 26fb1fed | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T06:42:35Z | gate-commit | 3857.559 | 202.440 | 264.206 | 0.12 | 0 | 37796ef1 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T11:39:46Z | gate-commit | 308.026 | 56.019 | 20.056 | 0.25 | 0 | 37796ef1 | campaign/the-underworld | Greyjoy | 10 |
| 2026-08-17T12:08:46Z | gate-commit | 630.172 | 210.931 | 175.901 | 0.61 | 0 | 7bc88170 | campaign/the-underworld | Greyjoy | 10 |
| 2026-08-17T12:41:28Z | gate-commit | 441.057 | 220.603 | 342.501 | 1.28 | 0 | 060bf5d9 | campaign/the-underworld | Greyjoy | 10 |
| 2026-08-17T13:29:22Z | rebaseline | 54.213 | 216.266 | 11.116 | 4.19 | 0 | eb9921af | campaign/the-underworld | Greyjoy | 10 |
| 2026-08-17T13:32:12Z | gate-commit | 37.419 | 57.705 | 38.270 | 2.56 | 0 | eb9921af | campaign/the-underworld | Greyjoy | 10 |
| 2026-08-17T14:06:46Z | rebaseline | 158.900 | 220.331 | 18.222 | 1.50 | 0 | 6f758949 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T14:16:16Z | gate-commit | 409.923 | 157.357 | 323.140 | 1.17 | 0 | 6f758949 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T14:17:31Z | gate-commit | 32.895 | 53.824 | 12.823 | 2.03 | 0 | f21f8ec6 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T14:49:40Z | gate-commit | 257.754 | 155.758 | 144.839 | 1.17 | 0 | 20044a44 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T14:51:19Z | gate-commit | 22.373 | 49.862 | 12.253 | 2.78 | 0 | 20044a44 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T14:58:02Z | gate-commit | 301.574 | 154.935 | 262.614 | 1.38 | 0 | 6414337f | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T16:03:21Z | gate-commit | 611.583 | 172.784 | 435.521 | 0.99 | 0 | 9fd5edb8 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T16:23:58Z | gate-commit | 165.997 | 120.422 | 195.459 | 1.90 | 0 | c9dfee34 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T16:51:01Z | gate-commit | 120.356 | 76.760 | 68.746 | 1.21 | 0 | 767ceb5a | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T17:40:33Z | gate-commit | 305.786 | 288.843 | 284.668 | 1.88 | 0 | 07b61300 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T17:41:28Z | gate-commit | 22.657 | 50.289 | 12.256 | 2.76 | 0 | 07b61300 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T18:08:30Z | gate-commit | 188.322 | 160.884 | 251.898 | 2.19 | 0 | 46316ca8 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T18:36:53Z | gate-commit | 306.760 | 254.092 | 313.512 | 1.85 | 0 | d765fc59 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T18:38:04Z | quick | 11.272 | 10.283 | 0.471 | 0.95 | 0 | d765fc59 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T18:38:58Z | gate-commit | 22.202 | 50.264 | 12.401 | 2.82 | 0 | d765fc59 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T19:02:13Z | gate-commit | 419.054 | 628.633 | 314.709 | 2.25 | 0 | 41509877 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T19:03:02Z | gate-commit | 22.513 | 50.382 | 12.460 | 2.79 | 0 | 41509877 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T19:28:07Z | gate-commit | 427.606 | 326.015 | 312.371 | 1.49 | 0 | dd131e66 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T19:38:07Z | gate-commit | 485.848 | 181.723 | 370.184 | 1.14 | 0 | dd131e66 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T19:40:08Z | gate-commit | 68.265 | 51.039 | 13.267 | 0.94 | 0 | dd131e66 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T20:00:45Z | gate-commit | 356.708 | 131.043 | 209.335 | 0.95 | 0 | 700390c4 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T20:01:48Z | gate-commit | 33.836 | 55.358 | 15.237 | 2.09 | 0 | 700390c4 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T21:01:07Z | rebaseline | 72.064 | 238.237 | 12.289 | 3.48 | 0 | 4f9d288d | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T22:08:10Z | rebaseline | 46.837 | 231.318 | 12.205 | 5.20 | 0 | 4f9d288d | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T22:17:42Z | quick | 76.885 | 63.255 | 165.312 | 2.97 | 0 | 4f9d288d | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T22:18:51Z | gate-commit | 21.833 | 50.010 | 12.239 | 2.85 | 0 | 4f9d288d | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T22:19:20Z | gate-commit | 21.869 | 50.020 | 11.879 | 2.83 | 0 | 46817a8c | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T22:27:22Z | gate-commit | 422.179 | 202.885 | 373.969 | 1.37 | 0 | 7d8401fb | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T22:45:58Z | quick | 78.297 | 63.429 | 166.891 | 2.94 | 0 | f297eb39 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T22:59:17Z | gate-commit | 22.318 | 49.995 | 12.519 | 2.80 | 0 | f297eb39 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-17T23:45:17Z | rebaseline | 52.519 | 230.795 | 11.833 | 4.62 | 0 | d95b92e9 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-18T00:44:51Z | quick | 86.317 | 69.567 | 192.408 | 3.04 | 0 | d95b92e9 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-18T00:45:55Z | rebaseline | 55.804 | 231.977 | 12.481 | 4.38 | 0 | d95b92e9 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-18T00:59:19Z | gate-commit | 22.035 | 49.884 | 12.495 | 2.83 | 0 | d95b92e9 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-18T01:00:12Z | gate-commit | 22.270 | 50.163 | 12.333 | 2.81 | 0 | 8cf1bdca | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-18T01:17:00Z | quick | 107.140 | 81.696 | 231.888 | 2.93 | 0 | 930812eb | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-18T01:47:35Z | gate-commit | 104.981 | 53.289 | 14.729 | 0.65 | 0 | 930812eb | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-18T01:54:34Z | rebaseline | 118.382 | 241.597 | 16.523 | 2.18 | 0 | b7109190 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-18T02:13:08Z | census | 884.611 | 29094.529 | 304.849 | 33.23 | 0 | 223e7d57 |  | lefford | 40 |
| 2026-08-18T03:18:47Z | gate-commit | 44.670 | 50.259 | 12.295 | 1.40 | 0 | 8df714ed | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-16T23:35:30Z | census | 855.533 | 27774.646 | 307.433 | 32.82 | 0 | 1e92c152 |  | lefford | 40 |
| 2026-08-16T23:46:39Z | rebaseline | 123.305 | 1072.961 | 54.283 | 9.14 | 0 | 1e92c152 | follow-up/census-ratchet | lefford | 40 |
| 2026-08-17T00:06:54Z | sluice:artifacts | 88.558 | 664.675 | 31.468 | 7.86 | 0 | c25e08e6 |  | lefford | 40 |
| 2026-08-17T00:07:00Z | sluice:outboard | 5.678 | 8.487 | 24.026 | 5.73 | 0 | 73dd0f17 |  | lefford | 40 |
| 2026-08-17T00:12:49Z | sluice:gate | 348.287 | 9142.738 | 423.538 | 27.47 | 0 | 08d385ee |  | lefford | 40 |
| 2026-08-17T00:28:40Z | sluice:seam-guard | 950.887 | 19361.401 | 1043.845 | 21.46 | 0 | b52c5a5c |  | lefford | 40 |
| 2026-08-17T00:32:38Z | sluice:clients | 237.286 | 423.881 | 33.943 | 1.93 | 0 | 5dd69a0c |  | lefford | 40 |
| 2026-08-17T01:04:29Z | sluice:heavy | 1911.399 | 29772.539 | 662.669 | 15.92 | 0 | 87d74f2f |  | lefford | 40 |
| 2026-08-17T02:20:29Z | rebaseline | 72.965 | 228.652 | 15.727 | 3.35 | 0 | 60dbacde | campaign/the-burr | MacBookPro | 10 |
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
| 2026-08-17T03:38:59Z | rebaseline | 156.094 | 227.098 | 17.251 | 1.57 | 0 | 0aad391c | campaign/the-burr | MacBookPro | 10 |
| 2026-08-17T03:47:23Z | gate-commit | 459.180 | 258.909 | 201.353 | 1.00 | 0 | 0aad391c | campaign/the-burr | MacBookPro | 10 |
| 2026-08-17T07:14:18Z | rebaseline | 5733.147 | 222.826 | 43.165 | 0.05 | 0 | c986d15e | campaign/the-burr | MacBookPro | 10 |
| 2026-08-17T08:07:47Z | rebaseline | 4973.375 | 225.486 | 53.192 | 0.06 | 0 | c986d15e | campaign/the-burr | MacBookPro | 10 |
| 2026-08-17T13:27:40Z | gate-commit | 33.324 | 53.317 | 13.043 | 1.99 | 0 | c986d15e | campaign/the-burr | Greyjoy | 10 |
| 2026-08-17T15:47:32Z | rebaseline | 48.691 | 213.739 | 10.909 | 4.61 | 0 | dfe1f7b1 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-17T16:40:44Z | rebaseline | 109.339 | 229.869 | 18.427 | 2.27 | 0 | dfe1f7b1 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-17T16:42:20Z | gate-commit | 34.815 | 52.923 | 13.632 | 1.91 | 0 | dfe1f7b1 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-17T19:42:09Z | gate-commit | 410.117 | 118.667 | 234.349 | 0.86 | 0 | 698189c6 | campaign/the-burr | MacBookPro | 10 |
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
| 2026-08-18T12:00:45Z | rebaseline | 47.309 | 213.856 | 11.033 | 4.75 | 0 | f20656fe | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T12:13:31Z | rebaseline | 119.294 | 211.903 | 9.920 | 1.86 | 0 | f20656fe | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T12:26:14Z | game-check | 0.889 | 0.803 | 0.171 | 1.10 | 0 | f20656fe | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T12:27:22Z | game-check | 45.375 | 124.504 | 4.994 | 2.85 | 0 | f20656fe | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T12:28:01Z | vessel-check | 34.831 | 33.518 | 1.092 | 0.99 | 0 | f20656fe | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T12:28:33Z | gate-commit | 25.504 | 29.088 | 59.824 | 3.49 | 0 | f20656fe | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T12:32:10Z | gate-commit | 199.896 | 130.718 | 190.449 | 1.61 | 0 | f20656fe | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T12:38:53Z | gate-commit | 23.933 | 50.758 | 12.837 | 2.66 | 0 | f20656fe | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T12:43:58Z | vessel-check | 29.713 | 30.571 | 1.176 | 1.07 | 0 | 8b81e02a | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T12:45:01Z | game-check | 56.584 | 125.999 | 3.162 | 2.28 | 0 | 8b81e02a | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T12:45:53Z | rebaseline | 47.303 | 215.332 | 11.547 | 4.80 | 0 | 8b81e02a | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T12:46:59Z | gate-commit | 22.861 | 50.957 | 12.936 | 2.79 | 0 | 8b81e02a | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-17T00:49:26Z | quick | 9.764 | 31.311 | 5.397 | 3.76 | 0 | 1e92c152 | campaign/the-palimpsest | MacBookPro | 10 |
| 2026-08-17T00:50:07Z | quick | 17.314 | 19.765 | 2.725 | 1.30 | 0 | 1e92c152 | campaign/the-palimpsest | MacBookPro | 10 |
| 2026-08-17T00:55:51Z | gate-commit | 322.604 | 1034.289 | 82.552 | 3.46 | 0 | 1e92c152 | campaign/the-palimpsest | MacBookPro | 10 |
| 2026-08-17T00:58:26Z | gate-commit | 58.497 | 54.374 | 14.222 | 1.17 | 0 | 1e92c152 | campaign/the-palimpsest | MacBookPro | 10 |
| 2026-08-17T01:30:42Z | quick | 14.088 | 10.873 | 0.611 | 0.82 | 0 | b63d47a0 | campaign/the-palimpsest | MacBookPro | 10 |
| 2026-08-17T01:42:31Z | quick | 31.229 | 28.725 | 12.595 | 1.32 | 0 | b180a013 | campaign/the-palimpsest | MacBookPro | 10 |
| 2026-08-17T01:43:33Z | quick | 16.976 | 11.473 | 1.051 | 0.74 | 0 | b180a013 | campaign/the-palimpsest | MacBookPro | 10 |
| 2026-08-17T01:46:39Z | gate-commit | 152.638 | 216.968 | 49.814 | 1.75 | 0 | b180a013 | campaign/the-palimpsest | MacBookPro | 10 |
| 2026-08-17T01:47:39Z | gate-commit | 44.977 | 38.816 | 9.770 | 1.08 | 0 | b180a013 | campaign/the-palimpsest | MacBookPro | 10 |
| 2026-08-17T01:49:25Z | gate-commit | 29.597 | 54.043 | 14.306 | 2.31 | 0 | b180a013 | campaign/the-palimpsest | MacBookPro | 10 |
| 2026-08-17T01:56:59Z | quick | 13.933 | 10.867 | 0.504 | 0.82 | 0 | befd0c9f | campaign/the-palimpsest | MacBookPro | 10 |
| 2026-08-17T01:57:24Z | quick | 11.394 | 10.376 | 0.368 | 0.94 | 0 | befd0c9f | campaign/the-palimpsest | MacBookPro | 10 |
| 2026-08-17T01:58:41Z | gate-commit | 48.774 | 66.174 | 27.026 | 1.91 | 0 | befd0c9f | campaign/the-palimpsest | MacBookPro | 10 |
| 2026-08-17T02:04:09Z | quick | 15.813 | 16.248 | 6.798 | 1.46 | 0 | 07776a15 | campaign/the-palimpsest | MacBookPro | 10 |
| 2026-08-17T02:04:33Z | quick | 11.294 | 10.337 | 0.357 | 0.95 | 0 | 07776a15 | campaign/the-palimpsest | MacBookPro | 10 |
| 2026-08-17T02:05:38Z | gate-commit | 48.315 | 65.962 | 28.116 | 1.95 | 0 | 07776a15 | campaign/the-palimpsest | MacBookPro | 10 |
| 2026-08-17T02:09:40Z | quick | 19.431 | 16.579 | 7.499 | 1.24 | 0 | 97476e12 | campaign/the-palimpsest | MacBookPro | 10 |
| 2026-08-17T02:10:44Z | gate-commit | 52.606 | 66.132 | 30.190 | 1.83 | 0 | 97476e12 | campaign/the-palimpsest | MacBookPro | 10 |
| 2026-08-17T02:15:37Z | quick | 14.391 | 16.952 | 4.844 | 1.51 | 0 | 31a37c9e | campaign/the-palimpsest | MacBookPro | 10 |
| 2026-08-17T02:16:54Z | gate-commit | 54.598 | 71.772 | 30.286 | 1.87 | 0 | 31a37c9e | campaign/the-palimpsest | MacBookPro | 10 |
| 2026-08-17T02:27:49Z | quick | 27.056 | 16.544 | 10.294 | 0.99 | 0 | a63897ac | campaign/the-palimpsest | MacBookPro | 10 |
| 2026-08-17T02:28:36Z | quick | 17.002 | 12.400 | 0.463 | 0.76 | 0 | a63897ac | campaign/the-palimpsest | MacBookPro | 10 |
| 2026-08-17T02:30:10Z | gate-commit | 69.682 | 72.515 | 27.318 | 1.43 | 0 | a63897ac | campaign/the-palimpsest | MacBookPro | 10 |
| 2026-08-17T02:42:58Z | gate-commit | 121.090 | 71.494 | 42.073 | 0.94 | 0 | 347fab09 | campaign/the-palimpsest | MacBookPro | 10 |
| 2026-08-17T03:01:17Z | quick | 3.309 | 2.297 | 0.267 | 0.77 | 0 | 3c4636d9 | campaign/the-palimpsest | MacBookPro | 10 |
| 2026-08-17T03:01:53Z | quick | 21.038 | 12.426 | 0.685 | 0.62 | 0 | 3c4636d9 | campaign/the-palimpsest | MacBookPro | 10 |
| 2026-08-17T03:05:16Z | gate-commit | 29.691 | 51.911 | 13.483 | 2.20 | 0 | 3c4636d9 | campaign/the-palimpsest | MacBookPro | 10 |
| 2026-08-17T11:39:43Z | quick | 97.876 | 12.851 | 2.724 | 0.16 | 0 | a6d2fe14 | campaign/the-palimpsest | Greyjoy | 10 |
| 2026-08-17T11:41:38Z | gate-commit | 85.172 | 52.858 | 19.405 | 0.85 | 0 | a6d2fe14 | campaign/the-palimpsest | Greyjoy | 10 |
| 2026-08-17T12:01:10Z | quick | 127.559 | 11.614 | 1.412 | 0.10 | 0 | cfa414c9 | campaign/the-palimpsest | Greyjoy | 10 |
| 2026-08-17T12:04:30Z | gate-commit | 182.261 | 52.435 | 18.655 | 0.39 | 0 | cfa414c9 | campaign/the-palimpsest | Greyjoy | 10 |
| 2026-08-17T12:16:48Z | rebaseline | 106.849 | 229.591 | 14.861 | 2.29 | 0 | 6d6a3dd3 | campaign/the-palimpsest | Greyjoy | 10 |
| 2026-08-17T12:17:19Z | quick | 11.145 | 10.174 | 0.367 | 0.95 | 0 | 6d6a3dd3 | campaign/the-palimpsest | Greyjoy | 10 |
| 2026-08-17T12:29:44Z | quick | 11.027 | 10.123 | 0.324 | 0.95 | 0 | c5f43256 | campaign/the-palimpsest | Greyjoy | 10 |
| 2026-08-17T12:46:11Z | quick | 14.231 | 11.052 | 0.684 | 0.82 | 0 | a52eaf16 | campaign/the-palimpsest | Greyjoy | 10 |
| 2026-08-17T13:03:39Z | quick | 56.188 | 10.219 | 0.333 | 0.19 | 0 | 9fd71a1a | campaign/the-palimpsest | Greyjoy | 10 |
| 2026-08-17T13:07:19Z | gate-commit | 147.434 | 53.518 | 13.988 | 0.46 | 0 | 9fd71a1a | campaign/the-palimpsest | Greyjoy | 10 |
| 2026-08-17T13:31:02Z | quick | 19.711 | 27.719 | 20.941 | 2.47 | 0 | ba896c80 | campaign/the-palimpsest | Greyjoy | 10 |
| 2026-08-17T13:34:31Z | gate-commit | 108.780 | 195.623 | 52.476 | 2.28 | 0 | ba896c80 | campaign/the-palimpsest | Greyjoy | 10 |
| 2026-08-17T15:50:36Z | gate-commit | 33.770 | 50.491 | 13.178 | 1.89 | 0 | 09658e9d | campaign/the-palimpsest | MacBookPro | 10 |
| 2026-08-17T15:59:27Z | sluice:artifacts | 84.429 | 674.483 | 29.008 | 8.33 | 0 | e1f1d952 |  | lefford | 40 |
| 2026-08-17T16:00:00Z | sluice:outboard | 33.065 | 20.956 | 34.341 | 1.67 | 0 | 68bff407 |  | lefford | 40 |
| 2026-08-17T16:05:34Z | sluice:gate | 333.044 | 9172.216 | 399.324 | 28.74 | 0 | 9b00027e |  | lefford | 40 |
| 2026-08-17T16:21:29Z | sluice:seam-guard | 955.218 | 19363.140 | 1049.785 | 21.37 | 0 | 2ccb5ab0 |  | lefford | 40 |
| 2026-08-17T16:25:28Z | sluice:clients | 238.684 | 426.406 | 32.751 | 1.92 | 0 | 81c7dac7 |  | lefford | 40 |
| 2026-08-17T16:56:50Z | sluice:heavy | 1881.494 | 30431.045 | 689.016 | 16.54 | 0 | 66b8f193 |  | lefford | 40 |
| 2026-08-17T19:46:42Z | rebaseline | 54.960 | 211.327 | 10.893 | 4.04 | 0 | d5886f17 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-17T19:49:23Z | gate-commit | 153.628 | 131.445 | 119.317 | 1.63 | 0 | d5886f17 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-17T19:59:39Z | rebaseline | 233.843 | 214.753 | 12.289 | 0.97 | 0 | bec57a47 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-17T20:03:36Z | gate-commit | 32.656 | 89.607 | 17.448 | 3.28 | 0 | bec57a47 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-17T20:09:29Z | gate-commit | 244.754 | 37.610 | 10.082 | 0.19 | 0 | bec57a47 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-17T20:10:07Z | gate-commit | 20.953 | 36.423 | 8.654 | 2.15 | 0 | bec57a47 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-17T20:14:38Z | rebaseline | 64.228 | 226.765 | 12.697 | 3.73 | 0 | bec57a47 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-17T20:21:33Z | gate-commit | 379.597 | 633.736 | 220.864 | 2.25 | 0 | bec57a47 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-17T20:43:12Z | gate-commit | 809.367 | 200.782 | 490.596 | 0.85 | 0 | edea963e | campaign/the-burr | MacBookPro | 10 |
| 2026-08-18T01:44:27Z | rebaseline | 120.444 | 216.370 | 10.313 | 1.88 | 0 | dc2838aa | campaign/the-burr | MacBookPro | 10 |
| 2026-08-18T01:49:55Z | gate-commit | 244.871 | 132.768 | 266.344 | 1.63 | 0 | dc2838aa | campaign/the-burr | MacBookPro | 10 |
| 2026-08-18T02:24:51Z | rebaseline | 214.670 | 220.200 | 10.728 | 1.08 | 0 | bdf84fda | campaign/the-burr | MacBookPro | 10 |
| 2026-08-18T03:10:25Z | gate-commit | 35.272 | 54.358 | 13.622 | 1.93 | 0 | bdf84fda | campaign/the-burr | MacBookPro | 10 |
| 2026-08-18T11:24:44Z | rebaseline | 59.602 | 209.282 | 9.862 | 3.68 | 0 | 15c897c4 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-18T11:29:33Z | rebaseline | 55.210 | 213.610 | 11.687 | 4.08 | 0 | 15c897c4 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-18T12:40:05Z | gate-commit | 22.978 | 50.018 | 13.262 | 2.75 | 0 | 15c897c4 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-17T20:10:49Z | sluice:artifacts | 82.112 | 661.454 | 26.701 | 8.38 | 0 | 35b0eadc |  | lefford | 40 |
| 2026-08-17T20:11:22Z | sluice:outboard | 33.395 | 21.350 | 34.612 | 1.68 | 0 | 35e8288f |  | lefford | 40 |
| 2026-08-17T20:16:44Z | sluice:gate | 321.562 | 8983.654 | 335.588 | 28.98 | 0 | 9aab91cc |  | lefford | 40 |
| 2026-08-17T20:32:36Z | sluice:seam-guard | 951.976 | 19331.583 | 1052.161 | 21.41 | 0 | 9bfef696 |  | lefford | 40 |
| 2026-08-17T20:36:31Z | sluice:clients | 234.165 | 421.496 | 33.061 | 1.94 | 0 | 0d89a6f3 |  | lefford | 40 |
| 2026-08-17T21:08:14Z | sluice:heavy | 1903.133 | 30358.416 | 690.625 | 16.31 | 0 | 8e723b81 |  | lefford | 40 |
| 2026-08-18T12:44:56Z | rebaseline | 58.879 | 215.196 | 11.065 | 3.84 | 0 | e38ad3d1 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-18T13:29:56Z | gate-commit | 431.023 | 183.685 | 381.399 | 1.31 | 0 | 02bb1046 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-18T13:30:39Z | gate-commit | 23.645 | 50.204 | 13.220 | 2.68 | 0 | 02bb1046 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-18T14:37:54Z | gate-commit | 387.992 | 120.583 | 222.655 | 0.88 | 0 | 7ceea22a | campaign/the-burr | MacBookPro | 10 |
| 2026-08-18T14:39:23Z | gate-commit | 20.131 | 36.571 | 8.953 | 2.26 | 0 | 7ceea22a | campaign/the-burr | MacBookPro | 10 |
| 2026-08-18T14:48:03Z | gate-commit | 366.994 | 129.386 | 216.473 | 0.94 | 0 | 7ceea22a | campaign/the-burr | MacBookPro | 10 |
| 2026-08-18T14:52:40Z | gate-commit | 24.961 | 50.963 | 13.468 | 2.58 | 0 | 7c21df8b | campaign/the-burr | MacBookPro | 10 |
| 2026-08-18T16:03:53Z | rebaseline | 63.376 | 231.690 | 11.726 | 3.84 | 0 | b58e53f8 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-18T16:10:57Z | gate-commit | 403.402 | 149.871 | 233.767 | 0.95 | 0 | b58e53f8 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-18T21:40:06Z | rebaseline | 65.302 | 224.751 | 11.637 | 3.62 | 0 | 04ae6654 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-18T22:02:21Z | gate-commit | 317.996 | 102.043 | 147.933 | 0.79 | 0 | 04ae6654 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-18T22:40:11Z | rebaseline | 59.016 | 215.551 | 12.019 | 3.86 | 0 | 9a25a1b9 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-18T22:52:22Z | gate-commit | 180.105 | 50.627 | 13.530 | 0.36 | 0 | 9a25a1b9 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-19T00:06:32Z | rebaseline | 86.278 | 220.120 | 13.256 | 2.70 | 0 | ca270b0a | campaign/the-burr | MacBookPro | 10 |
| 2026-08-19T00:57:18Z | gate-commit | 99.724 | 52.974 | 14.204 | 0.67 | 0 | ca270b0a | campaign/the-burr | MacBookPro | 10 |
| 2026-08-19T01:37:18Z | gate-commit | 1055.687 | 127.921 | 319.718 | 0.42 | 0 | c234d518 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-18T03:27:04Z | rebaseline | 62.594 | 229.283 | 11.571 | 3.85 | 0 | 85a5333e | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-18T03:36:34Z | gate-commit | 377.733 | 256.319 | 224.469 | 1.27 | 0 | 85a5333e | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-18T04:22:58Z | gate-commit | 701.127 | 246.026 | 642.609 | 1.27 | 0 | b43127f9 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-18T05:26:17Z | gate-commit | 488.213 | 216.464 | 390.210 | 1.24 | 0 | 1a76ae32 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-18T05:51:58Z | gate-commit | 22.536 | 50.270 | 12.630 | 2.79 | 0 | 982b5b0e | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-18T11:51:07Z | gate-commit | 92.758 | 54.361 | 13.737 | 0.73 | 0 | 4a88e60f | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-18T15:24:35Z | gate-commit | 527.686 | 173.648 | 454.658 | 1.19 | 0 | ea95434f | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-18T15:25:37Z | gate-commit | 31.899 | 54.527 | 14.466 | 2.16 | 0 | cb3ed009 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-18T01:57:41Z | gate-commit | 156.953 | 109.637 | 44.518 | 0.98 | 0 | 0c37a4c3 | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T01:58:32Z | gate-commit | 26.207 | 51.915 | 13.189 | 2.48 | 0 | 0c37a4c3 | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T02:04:04Z | gate-commit | 24.330 | 52.540 | 12.803 | 2.69 | 0 | cd8221be | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T02:04:41Z | gate-commit | 24.264 | 51.222 | 12.911 | 2.64 | 0 | cd8221be | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T02:08:18Z | gate-commit | 53.023 | 50.445 | 12.358 | 1.18 | 0 | a6d0f21e | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T03:22:54Z | gate-commit | 45.418 | 54.984 | 14.074 | 1.52 | 0 | d64f34db | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T03:28:37Z | gate-commit | 18.206 | 19.652 | 12.508 | 1.77 | 0 | 19818345 | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T03:29:55Z | gate-commit | 62.772 | 71.664 | 31.534 | 1.64 | 0 | 19818345 | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T03:31:11Z | gate-commit | 33.102 | 52.874 | 13.784 | 2.01 | 0 | 19818345 | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T03:39:54Z | gate-commit | 118.925 | 88.466 | 48.446 | 1.15 | 0 | a515ae62 | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T03:41:51Z | gate-commit | 56.718 | 55.498 | 13.550 | 1.22 | 0 | a515ae62 | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T03:51:39Z | gate-commit | 100.793 | 138.681 | 56.363 | 1.94 | 0 | f6b7ce01 | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T03:52:29Z | gate-commit | 25.013 | 51.733 | 12.729 | 2.58 | 0 | f6b7ce01 | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T04:01:28Z | gate-commit | 60.485 | 75.028 | 45.136 | 1.99 | 0 | 58d66894 | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T04:02:06Z | gate-commit | 22.288 | 50.169 | 12.497 | 2.81 | 0 | 58d66894 | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T04:23:03Z | gate-commit | 240.160 | 98.539 | 90.574 | 0.79 | 0 | f8757936 | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T04:24:23Z | gate-commit | 22.764 | 50.516 | 12.735 | 2.78 | 0 | f8757936 | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T04:39:42Z | gate-commit | 64.153 | 74.878 | 46.619 | 1.89 | 0 | 8d760577 | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T04:40:33Z | gate-commit | 22.001 | 49.909 | 12.396 | 2.83 | 0 | 8d760577 | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T06:04:12Z | gate-commit | 59.725 | 70.854 | 42.110 | 1.89 | 0 | b0678cdd | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T06:04:56Z | gate-commit | 21.702 | 49.766 | 12.389 | 2.86 | 0 | b0678cdd | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T06:16:05Z | gate-commit | 57.643 | 72.711 | 46.347 | 2.07 | 0 | 3c26ee30 | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T06:16:42Z | gate-commit | 21.677 | 49.869 | 12.328 | 2.87 | 0 | 3c26ee30 | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T06:38:06Z | gate-commit | 21.919 | 49.831 | 12.047 | 2.82 | 0 | 36845820 | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T06:38:57Z | gate-commit | 21.685 | 49.854 | 12.392 | 2.87 | 0 | 36845820 | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T06:53:10Z | gate-commit | 21.925 | 49.988 | 12.133 | 2.83 | 0 | e22ae141 | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T06:53:53Z | gate-commit | 21.652 | 49.807 | 12.474 | 2.88 | 0 | e22ae141 | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T07:10:49Z | rebaseline | 48.577 | 214.439 | 11.456 | 4.65 | 0 | 87f1962a | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T07:13:21Z | gate-commit | 23.007 | 49.826 | 12.339 | 2.70 | 0 | 87f1962a | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T07:50:34Z | gate-commit | 69.989 | 77.351 | 54.818 | 1.89 | 0 | 68a8a61d | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T07:52:02Z | gate-commit | 21.919 | 49.978 | 12.455 | 2.85 | 0 | 68a8a61d | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T11:41:17Z | gate-commit | 88.630 | 79.679 | 53.038 | 1.50 | 0 | d2f0363a | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T11:42:49Z | gate-commit | 36.440 | 55.286 | 13.477 | 1.89 | 0 | d2f0363a | campaign/the-parley | MacBookPro | 10 |
| 2026-08-18T12:14:15Z | sluice:artifacts | 115.785 | 949.587 | 48.713 | 8.62 | 0 | 35042bdb |  | lefford | 40 |
| 2026-08-18T12:14:50Z | sluice:outboard | 34.127 | 22.348 | 34.566 | 1.67 | 0 | b0d42a18 |  | lefford | 40 |
| 2026-08-18T12:21:03Z | sluice:gate | 373.100 | 9926.342 | 532.780 | 28.03 | 0 | b737a442 |  | lefford | 40 |
| 2026-08-18T12:36:54Z | sluice:seam-guard | 950.717 | 19359.641 | 1053.342 | 21.47 | 0 | 8bf927b4 |  | lefford | 40 |
| 2026-08-18T12:41:17Z | sluice:clients | 263.036 | 500.552 | 45.967 | 2.08 | 0 | 786e5386 |  | lefford | 40 |
| 2026-08-18T13:13:08Z | sluice:heavy | 1911.136 | 31223.021 | 700.108 | 16.70 | 0 | 3f99f2ff |  | lefford | 40 |
| 2026-08-18T15:46:18Z | gate-commit | 48.547 | 41.141 | 100.262 | 2.91 | 0 | 631504e8 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-18T15:47:28Z | rebaseline | 61.208 | 236.199 | 11.375 | 4.04 | 0 | 631504e8 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-18T15:51:39Z | gate-commit | 231.095 | 151.897 | 121.376 | 1.18 | 0 | 631504e8 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-18T15:52:11Z | gate-commit | 24.115 | 54.069 | 14.278 | 2.83 | 0 | a70aebef | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-18T13:08:17Z | rebaseline | 36.110 | 214.176 | 12.170 | 6.27 | 0 | 61b77372 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T13:15:34Z | gate-commit | 303.031 | 122.693 | 164.929 | 0.95 | 0 | 61b77372 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T13:19:50Z | gate-commit | 21.453 | 36.600 | 8.942 | 2.12 | 0 | 61b77372 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T15:19:14Z | gate-commit | 233.084 | 56.091 | 16.833 | 0.31 | 0 | 61b77372 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T15:20:30Z | gate-commit | 59.982 | 56.534 | 18.687 | 1.25 | 0 | 61b77372 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T15:40:06Z | sluice:artifacts | 119.524 | 1104.994 | 53.328 | 9.69 | 0 | d148ecbb |  | lefford | 40 |
| 2026-08-18T15:40:41Z | sluice:outboard | 35.227 | 23.223 | 35.552 | 1.67 | 0 | c43c3844 |  | lefford | 40 |
| 2026-08-18T15:47:17Z | sluice:gate | 395.547 | 10388.255 | 564.031 | 27.69 | 0 | b1bec9b9 |  | lefford | 40 |
| 2026-08-18T16:03:17Z | sluice:seam-guard | 959.978 | 19356.973 | 1056.794 | 21.26 | 0 | 83826873 |  | lefford | 40 |
| 2026-08-18T16:07:56Z | sluice:clients | 278.751 | 548.766 | 51.277 | 2.15 | 0 | 534f1495 |  | lefford | 40 |
| 2026-08-18T16:40:02Z | sluice:heavy | 1925.752 | 31287.277 | 732.384 | 16.63 | 0 | 4a940057 |  | lefford | 40 |
| 2026-08-18T20:08:27Z | gate-commit | 335.254 | 1037.731 | 139.052 | 3.51 | 0 | 82f6d153 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-18T20:13:30Z | gate-commit | 26.226 | 53.747 | 14.122 | 2.59 | 0 | 82f6d153 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-18T22:27:42Z | gate-commit | 102.738 | 60.301 | 16.289 | 0.75 | 0 | c78e6cc1 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-18T22:28:40Z | gate-commit | 34.499 | 57.874 | 15.586 | 2.13 | 0 | c78e6cc1 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-18T22:29:40Z | gate-commit | 24.128 | 53.458 | 14.673 | 2.82 | 0 | c78e6cc1 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-18T22:43:52Z | gate-commit | 35.359 | 56.821 | 16.883 | 2.08 | 0 | b156cbd5 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-18T22:57:36Z | gate-commit | 39.770 | 54.029 | 15.712 | 1.75 | 0 | ee763751 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-18T23:34:30Z | gate-commit | 328.642 | 86.903 | 79.400 | 0.51 | 0 | 09b52ef0 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-18T23:36:36Z | gate-commit | 83.574 | 54.134 | 15.132 | 0.83 | 0 | 09b52ef0 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T00:06:28Z | gate-commit | 112.771 | 99.501 | 74.754 | 1.55 | 0 | d21294fb | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T00:07:42Z | gate-commit | 50.476 | 58.765 | 15.942 | 1.48 | 0 | d21294fb | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T00:44:00Z | gate-commit | 183.799 | 118.689 | 150.221 | 1.46 | 0 | 1734c44b | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T00:44:59Z | gate-commit | 23.848 | 53.254 | 14.432 | 2.84 | 0 | 1734c44b | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T00:50:09Z | gate-commit | 135.538 | 57.901 | 16.497 | 0.55 | 0 | d49865f0 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T01:35:14Z | gate-commit | 390.850 | 41.475 | 12.629 | 0.14 | 0 | 23771120 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T01:39:24Z | gate-commit | 227.179 | 56.578 | 15.219 | 0.32 | 0 | 23771120 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T01:43:19Z | gate-commit | 168.128 | 54.559 | 14.994 | 0.41 | 0 | 23771120 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T02:21:03Z | gate-commit | 49.205 | 53.072 | 14.856 | 1.38 | 0 | 89a151e3 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T02:23:23Z | gate-commit | 23.111 | 52.826 | 14.203 | 2.90 | 0 | 89a151e3 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T02:35:02Z | gate-commit | 24.687 | 56.733 | 14.078 | 2.87 | 0 | fb175876 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T02:35:59Z | gate-commit | 23.068 | 52.740 | 14.205 | 2.90 | 0 | fb175876 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T02:56:47Z | gate-commit | 23.734 | 53.030 | 14.296 | 2.84 | 0 | f88b6e9b | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T02:57:58Z | gate-commit | 23.203 | 52.839 | 14.165 | 2.89 | 0 | f88b6e9b | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T03:16:23Z | gate-commit | 52.432 | 82.393 | 81.007 | 3.12 | 0 | 7bd2dd96 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T03:17:08Z | gate-commit | 23.484 | 53.097 | 14.259 | 2.87 | 0 | 7bd2dd96 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T03:32:20Z | rebaseline | 54.449 | 212.123 | 10.532 | 4.09 | 0 | 678f35a2 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T03:33:29Z | gate-commit | 25.692 | 53.079 | 14.432 | 2.63 | 0 | 678f35a2 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T03:37:15Z | gate-commit | 24.827 | 53.894 | 14.128 | 2.74 | 0 | edb54eb0 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T03:42:13Z | rebaseline | 36.656 | 210.148 | 10.103 | 6.01 | 0 | edb54eb0 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T03:58:37Z | gate-commit | 81.108 | 80.950 | 66.620 | 1.82 | 0 | edb54eb0 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T03:59:41Z | rebaseline | 44.032 | 214.084 | 10.468 | 5.10 | 0 | edb54eb0 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T04:01:17Z | gate-commit | 23.433 | 52.894 | 13.850 | 2.85 | 0 | edb54eb0 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T04:02:28Z | gate-commit | 23.228 | 53.069 | 14.168 | 2.89 | 0 | edb54eb0 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-18T18:24:13Z | gate-commit | 267.590 | 154.833 | 120.480 | 1.03 | 0 | 82f6d153 | campaign/the-illumination | MacBookPro | 10 |
| 2026-08-18T18:44:37Z | sluice:artifacts | 79.851 | 664.331 | 26.569 | 8.65 | 0 | 8279799f2 |  | lefford | 40 |
| 2026-08-18T18:45:11Z | sluice:outboard | 33.092 | 21.278 | 34.284 | 1.68 | 0 | 1f25ca9e1 |  | lefford | 40 |
| 2026-08-18T18:50:34Z | sluice:gate | 323.584 | 9098.613 | 343.421 | 29.18 | 0 | f82b707de |  | lefford | 40 |
| 2026-08-18T19:06:41Z | sluice:seam-guard | 966.351 | 19410.698 | 1063.825 | 21.19 | 0 | c0675afbf |  | lefford | 40 |
| 2026-08-18T19:10:51Z | sluice:clients | 250.369 | 445.395 | 34.166 | 1.92 | 0 | 96a551a60 |  | lefford | 40 |
| 2026-08-18T19:42:39Z | sluice:heavy | 1907.762 | 31163.657 | 715.546 | 16.71 | 0 | 852829240 |  | lefford | 40 |
| 2026-08-18T17:55:46Z | gate-commit | 128.242 | 126.356 | 323.309 | 3.51 | 0 | 10fa2aca | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-18T18:06:48Z | gate-commit | 638.691 | 440.824 | 266.075 | 1.11 | 0 | 10fa2aca | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-18T18:09:26Z | rebaseline | 89.641 | 234.914 | 10.950 | 2.74 | 0 | 10fa2aca | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-18T18:10:24Z | gate-commit | 22.859 | 37.340 | 8.778 | 2.02 | 0 | 10fa2aca | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-18T18:15:39Z | gate-commit | 36.477 | 56.466 | 17.625 | 2.03 | 0 | 10fa2aca | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-18T18:30:35Z | rebaseline | 53.825 | 225.905 | 9.200 | 4.37 | 0 | b77d2a32 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-18T18:41:33Z | gate-commit | 26.690 | 54.805 | 13.892 | 2.57 | 0 | 925b0db8 | campaign/the-underworld | MacBookPro | 10 |
| 2026-08-18T19:46:15Z | sluice:artifacts | 122.416 | 1065.598 | 52.257 | 9.13 | 0 | 6f1388ca0 |  | lefford | 40 |
| 2026-08-18T19:46:50Z | sluice:outboard | 35.149 | 22.468 | 34.834 | 1.63 | 0 | d2f0b0975 |  | lefford | 40 |
| 2026-08-18T19:53:47Z | sluice:gate | 416.112 | 11133.497 | 565.062 | 28.11 | 0 | 2235b2a6a |  | lefford | 40 |
| 2026-08-18T20:11:39Z | sluice:seam-guard | 1072.457 | 21053.864 | 1087.979 | 20.65 | 0 | b061ab89e |  | lefford | 40 |
| 2026-08-18T20:17:06Z | sluice:clients | 326.766 | 589.548 | 49.196 | 1.95 | 0 | d205bb454 |  | lefford | 40 |
| 2026-08-18T20:50:57Z | sluice:heavy | 2030.215 | 32677.492 | 708.372 | 16.44 | 0 | 81aa4ecba |  | lefford | 40 |
| 2026-08-18T20:53:40Z | sluice:artifacts | 81.103 | 692.309 | 26.398 | 8.86 | 0 | 1ae44aa6c |  | lefford | 40 |
| 2026-08-18T20:54:14Z | sluice:outboard | 33.041 | 18.918 | 26.604 | 1.38 | 0 | 34e850c8f |  | lefford | 40 |
| 2026-08-18T21:00:16Z | sluice:gate | 362.495 | 9829.661 | 352.981 | 28.09 | 0 | 732dd84b5 |  | lefford | 40 |
| 2026-08-18T21:18:09Z | sluice:seam-guard | 1072.291 | 21142.952 | 1104.774 | 20.75 | 0 | 1ddef5087 |  | lefford | 40 |
| 2026-08-18T21:22:18Z | sluice:clients | 249.609 | 445.282 | 33.529 | 1.92 | 0 | 2289bf021 |  | lefford | 40 |
| 2026-08-18T21:55:49Z | sluice:heavy | 2010.228 | 32681.298 | 682.128 | 16.60 | 0 | 4ed029135 |  | lefford | 40 |
| 2026-08-18T22:55:25Z | prewarm | 75.116 | 354.051 | 73.199 | 5.69 | 0 | 7e786b18 | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-18T23:11:07Z | gate-commit | 541.450 | 134.433 | 219.381 | 0.65 | 0 | 7e786b18 | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-18T23:12:31Z | gate-commit | 24.029 | 54.167 | 14.684 | 2.87 | 0 | 7e786b18 | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-18T23:18:32Z | gate-commit | 24.789 | 55.429 | 15.074 | 2.84 | 0 | 0ff93088 | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-18T23:19:39Z | gate-commit | 23.539 | 54.317 | 14.792 | 2.94 | 0 | 0ff93088 | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-18T23:24:43Z | gate-commit | 7.292 | 6.240 | 9.038 | 2.10 | 0 | 7493115f | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-18T23:37:23Z | gate-commit | 744.793 | 879.928 | 468.806 | 1.81 | 0 | 7493115f | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-18T23:38:15Z | gate-commit | 25.877 | 55.232 | 15.162 | 2.72 | 0 | 7493115f | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T00:02:32Z | gate-commit | 714.167 | 244.119 | 641.723 | 1.24 | 0 | 5ee8cfe5 | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T00:03:46Z | gate-commit | 64.027 | 58.412 | 15.707 | 1.16 | 0 | 5ee8cfe5 | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T00:16:57Z | gate-commit | 210.871 | 117.445 | 396.849 | 2.44 | 0 | add28593 | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T00:20:26Z | gate-commit | 140.263 | 92.445 | 317.034 | 2.92 | 0 | add28593 | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T00:32:19Z | gate-commit | 692.191 | 194.368 | 327.103 | 0.75 | 0 | add28593 | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T00:34:09Z | gate-commit | 74.204 | 55.923 | 15.202 | 0.96 | 0 | add28593 | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T00:52:43Z | gate-commit | 279.699 | 84.092 | 328.995 | 1.48 | 0 | dc794631 | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T00:57:15Z | gate-commit | 249.019 | 87.674 | 323.923 | 1.65 | 0 | dc794631 | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T01:12:04Z | gate-commit | 876.166 | 172.745 | 329.389 | 0.57 | 0 | dc794631 | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T01:13:17Z | gate-commit | 54.488 | 56.033 | 15.175 | 1.31 | 0 | dc794631 | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T01:48:12Z | gate-commit | 525.908 | 56.154 | 15.402 | 0.14 | 0 | e95ff43a | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T01:48:12Z | gate-commit | 192.650 | 56.251 | 15.427 | 0.37 | 0 | e95ff43a | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T11:53:06Z | gate-commit | 765.962 | 828.982 | 477.658 | 1.71 | 0 | 564e07fa | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T11:53:55Z | gate-commit | 23.925 | 54.689 | 14.711 | 2.90 | 0 | 564e07fa | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T12:08:23Z | gate-commit | 69.710 | 54.922 | 159.217 | 3.07 | 0 | 3780374c | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T12:10:57Z | gate-commit | 112.791 | 86.836 | 253.404 | 3.02 | 0 | 3780374c | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T12:20:16Z | gate-commit | 542.798 | 395.293 | 296.787 | 1.28 | 0 | 3780374c | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T12:21:14Z | gate-commit | 23.990 | 53.877 | 14.471 | 2.85 | 0 | 3780374c | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T12:53:26Z | gate-commit | 293.245 | 54.803 | 15.432 | 0.24 | 0 | faa64ddc | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T12:54:11Z | gate-commit | 24.978 | 54.853 | 15.121 | 2.80 | 0 | faa64ddc | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T12:59:07Z | gate-commit | 24.921 | 54.570 | 15.113 | 2.80 | 0 | ed8aaa7c | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T13:00:05Z | gate-commit | 26.023 | 54.719 | 15.240 | 2.69 | 0 | ed8aaa7c | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T13:38:16Z | rebaseline | 30.586 | 78.142 | 5.491 | 2.73 | 0 | 1331e0c3 | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T13:40:14Z | rebaseline | 112.685 | 256.881 | 27.675 | 2.53 | 0 | 1331e0c3 | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T14:02:32Z | gate-commit | 410.072 | 517.491 | 361.161 | 2.14 | 0 | 1331e0c3 | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T14:04:10Z | gate-commit | 25.222 | 55.398 | 15.400 | 2.81 | 0 | 1331e0c3 | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T14:45:53Z | gate-commit | 475.894 | 503.917 | 144.496 | 1.36 | 0 | 64ff6cbb | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T14:48:11Z | gate-commit | 37.931 | 61.175 | 15.866 | 2.03 | 0 | 64ff6cbb | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T15:08:07Z | gate-commit | 37.732 | 55.139 | 15.053 | 1.86 | 0 | 8f5e3031 | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T16:04:18Z | gate-commit | 771.902 | 201.548 | 453.289 | 0.85 | 0 | b881b94f | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T16:06:56Z | gate-commit | 33.527 | 57.803 | 16.954 | 2.23 | 0 | b881b94f | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-18T22:05:24Z | sluice:artifacts | 83.439 | 719.609 | 26.737 | 8.94 | 0 | 9e106c731 |  | lefford | 40 |
| 2026-08-18T22:05:58Z | sluice:outboard | 33.994 | 21.608 | 33.821 | 1.63 | 0 | 9a7eaa372 |  | lefford | 40 |
| 2026-08-18T22:11:56Z | sluice:gate | 357.555 | 9842.959 | 353.289 | 28.52 | 0 | 15628c4dd |  | lefford | 40 |
| 2026-08-18T22:29:43Z | sluice:seam-guard | 1067.188 | 21078.038 | 1100.658 | 20.78 | 0 | ed657a441 |  | lefford | 40 |
| 2026-08-18T22:33:59Z | sluice:clients | 255.138 | 444.848 | 33.452 | 1.87 | 0 | 890e2fe9e |  | lefford | 40 |
| 2026-08-18T23:07:19Z | sluice:heavy | 2000.206 | 32770.650 | 682.442 | 16.72 | 0 | 1ef460bb1 |  | lefford | 40 |
| 2026-08-19T00:10:07Z | prewarm | 202.255 | 1276.984 | 67.032 | 6.65 | 0 | 88379dfa | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T01:48:37Z | gate-commit | 1297.066 | 88.782 | 34.120 | 0.09 | 0 | f5942d9f | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T01:50:32Z | rebaseline | 57.290 | 232.078 | 11.587 | 4.25 | 0 | f5942d9f | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T01:51:20Z | gate-commit | 34.536 | 62.592 | 22.513 | 2.46 | 0 | f5942d9f | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T01:52:13Z | gate-commit | 24.117 | 54.725 | 14.528 | 2.87 | 0 | f5942d9f | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T03:25:03Z | gate-commit | 42.460 | 54.350 | 23.527 | 1.83 | 0 | 9cd716fe | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T03:26:16Z | gate-commit | 49.580 | 81.960 | 36.165 | 2.38 | 0 | 9cd716fe | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T03:27:11Z | gate-commit | 23.881 | 54.412 | 14.413 | 2.88 | 0 | 9cd716fe | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T03:38:29Z | gate-commit | 44.930 | 59.360 | 24.935 | 1.88 | 0 | b2a500a9 | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T03:40:33Z | gate-commit | 37.437 | 68.785 | 24.932 | 2.50 | 0 | b2a500a9 | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T03:41:16Z | gate-commit | 24.246 | 54.693 | 14.427 | 2.85 | 0 | b2a500a9 | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T03:52:58Z | rebaseline | 48.625 | 228.990 | 11.619 | 4.95 | 0 | 94fcfa8e | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T03:54:12Z | gate-commit | 55.216 | 97.628 | 33.918 | 2.38 | 0 | 94fcfa8e | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T03:58:33Z | gate-commit | 35.390 | 56.085 | 15.270 | 2.02 | 0 | 94fcfa8e | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T04:08:22Z | gate-commit | 51.781 | 73.089 | 32.429 | 2.04 | 0 | 7ead99be | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T04:09:57Z | gate-commit | 24.270 | 55.292 | 14.773 | 2.89 | 0 | 7ead99be | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T04:15:01Z | gate-commit | 9.273 | 11.442 | 6.431 | 1.93 | 0 | 213620b8 | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T04:16:41Z | gate-commit | 60.107 | 101.688 | 42.397 | 2.40 | 0 | 213620b8 | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T04:23:17Z | rebaseline | 54.947 | 231.505 | 11.454 | 4.42 | 0 | 9646b4cb | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T04:26:15Z | gate-commit | 76.344 | 92.552 | 32.670 | 1.64 | 0 | 9646b4cb | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T04:31:55Z | gate-commit | 52.434 | 72.944 | 33.545 | 2.03 | 0 | 5e249334 | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T04:32:35Z | gate-commit | 23.384 | 54.338 | 14.378 | 2.94 | 0 | 5e249334 | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T04:42:18Z | gate-commit | 53.911 | 73.157 | 34.002 | 1.99 | 0 | 1ca43081 | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T04:43:42Z | gate-commit | 23.356 | 54.162 | 14.388 | 2.94 | 0 | 1ca43081 | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T04:50:55Z | gate-commit | 53.135 | 77.365 | 41.182 | 2.23 | 0 | ba44ff23 | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T04:51:33Z | gate-commit | 23.456 | 54.387 | 14.435 | 2.93 | 0 | ba44ff23 | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T05:09:51Z | gate-commit | 53.902 | 74.112 | 40.700 | 2.13 | 0 | 286519b1 | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T05:28:48Z | gate-commit | 67.115 | 117.821 | 39.832 | 2.35 | 0 | 21152158 | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T05:29:27Z | gate-commit | 20.316 | 37.150 | 8.590 | 2.25 | 0 | 21152158 | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T05:31:29Z | gate-commit | 63.747 | 81.608 | 49.840 | 2.06 | 0 | 21152158 | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T05:35:48Z | gate-commit | 23.366 | 54.165 | 14.045 | 2.92 | 0 | 9c90c386 | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T05:38:47Z | gate-commit | 23.765 | 54.241 | 14.674 | 2.90 | 0 | 9c90c386 | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T05:53:32Z | gate-commit | 61.985 | 74.422 | 38.920 | 1.83 | 0 | 5f8353bb | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T05:55:45Z | gate-commit | 23.461 | 54.316 | 14.409 | 2.93 | 0 | 5f8353bb | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T00:18:20Z | sluice:artifacts | 82.452 | 706.910 | 25.204 | 8.88 | 0 | 1d5a97eef |  | lefford | 40 |
| 2026-08-19T00:18:55Z | sluice:outboard | 34.557 | 22.255 | 35.056 | 1.66 | 0 | a4a5e903d |  | lefford | 40 |
| 2026-08-19T00:24:52Z | sluice:gate | 357.153 | 9849.391 | 355.338 | 28.57 | 0 | d3c360028 |  | lefford | 40 |
| 2026-08-19T00:42:42Z | sluice:seam-guard | 1069.408 | 21100.988 | 1101.827 | 20.76 | 0 | 18c91384d |  | lefford | 40 |
| 2026-08-19T00:46:50Z | sluice:clients | 248.662 | 444.305 | 33.638 | 1.92 | 0 | 2de56d9e7 |  | lefford | 40 |
| 2026-08-19T01:20:23Z | sluice:heavy | 2012.540 | 32822.154 | 688.957 | 16.65 | 0 | dc376ccd7 |  | lefford | 40 |
| 2026-08-19T01:46:22Z | sluice:artifacts | 85.514 | 708.993 | 24.236 | 8.57 | 0 | bc7f72c41 |  | lefford | 40 |
| 2026-08-19T01:46:58Z | sluice:outboard | 35.390 | 22.545 | 35.620 | 1.64 | 0 | 36d4733e9 |  | lefford | 40 |
| 2026-08-19T01:52:57Z | sluice:gate | 358.259 | 9829.842 | 356.741 | 28.43 | 0 | a86da7a53 |  | lefford | 40 |
| 2026-08-19T02:10:42Z | sluice:seam-guard | 1065.440 | 21104.708 | 1097.015 | 20.84 | 0 | 6b2a81b40 |  | lefford | 40 |
| 2026-08-19T02:14:50Z | sluice:clients | 247.754 | 449.755 | 32.737 | 1.95 | 0 | b772ad190 |  | lefford | 40 |
| 2026-08-19T02:49:15Z | sluice:heavy | 2064.411 | 32768.758 | 671.520 | 16.20 | 0 | 8051c9a94 |  | lefford | 40 |
| 2026-08-19T11:03:40Z | rebaseline | 108.791 | 230.359 | 10.332 | 2.21 | 0 | b5c6278c | campaign/the-burr | MacBookPro | 10 |
| 2026-08-19T11:38:50Z | gate-commit | 435.099 | 513.353 | 319.980 | 1.92 | 0 | b5c6278c | campaign/the-burr | MacBookPro | 10 |
| 2026-08-19T11:03:55Z | rebaseline | 75.349 | 234.598 | 11.758 | 3.27 | 0 | 711dbe31 | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T11:04:54Z | gate-commit | 43.978 | 55.445 | 15.510 | 1.61 | 0 | f90711ae | campaign/the-adit | MacBookPro | 10 |
| 2026-08-19T11:09:51Z | sluice:artifacts | 106.298 | 791.154 | 36.551 | 7.79 | 0 | 02cc59d17 |  | lefford | 40 |
| 2026-08-19T11:10:26Z | sluice:outboard | 34.779 | 22.091 | 35.565 | 1.66 | 0 | 054498b4d |  | lefford | 40 |
| 2026-08-19T11:17:03Z | sluice:gate | 396.201 | 10190.968 | 436.099 | 26.82 | 0 | 50aac259a |  | lefford | 40 |
| 2026-08-19T11:34:50Z | sluice:seam-guard | 1067.306 | 21095.971 | 1111.683 | 20.81 | 0 | bcd4b187c |  | lefford | 40 |
| 2026-08-19T11:39:16Z | sluice:clients | 265.377 | 465.489 | 34.678 | 1.88 | 0 | 666d7f5f2 |  | lefford | 40 |
| 2026-08-19T12:13:02Z | sluice:heavy | 2026.461 | 33016.541 | 687.275 | 16.63 | 0 | d06ce9065 |  | lefford | 40 |
| 2026-08-19T14:14:53Z | rebaseline | 64.280 | 232.464 | 11.953 | 3.80 | 0 | fa2222ca | campaign/the-burr | MacBookPro | 10 |
| 2026-08-19T14:43:16Z | gate-commit | 321.126 | 98.152 | 127.691 | 0.70 | 0 | fa2222ca | campaign/the-burr | MacBookPro | 10 |
| 2026-08-18T22:24:13Z | prewarm | 227.374 | 1303.906 | 165.840 | 6.46 | 0 | 2fc70c9a | campaign/the-planes | MacBookPro | 10 |
| 2026-08-19T01:15:34Z | gate-commit | 833.061 | 225.798 | 446.114 | 0.81 | 0 | 752af3bf | campaign/the-planes | MacBookPro | 10 |
| 2026-08-19T01:16:26Z | gate-commit | 34.993 | 59.957 | 15.527 | 2.16 | 0 | 752af3bf | campaign/the-planes | MacBookPro | 10 |
| 2026-08-19T01:48:41Z | gate-commit | 1204.794 | 180.757 | 439.427 | 0.51 | 0 | adb29c61 | campaign/the-planes | MacBookPro | 10 |
| 2026-08-19T01:49:24Z | gate-commit | 24.647 | 54.838 | 14.727 | 2.82 | 0 | adb29c61 | campaign/the-planes | MacBookPro | 10 |
| 2026-08-19T13:50:34Z | gate-commit | 581.139 | 167.313 | 302.899 | 0.81 | 0 | 4910ced5 | campaign/the-planes | MacBookPro | 10 |
| 2026-08-19T13:51:55Z | gate-commit | 23.260 | 50.883 | 12.453 | 2.72 | 0 | 4910ced5 | campaign/the-planes | MacBookPro | 10 |
| 2026-08-19T14:02:41Z | prewarm | 241.724 | 1117.414 | 59.981 | 4.87 | 0 | ca6f3431 | campaign/the-crucible | MacBookPro | 10 |
| 2026-08-19T14:03:31Z | gate-commit | 253.379 | 166.401 | 25.406 | 0.76 | 0 | ca6f3431 | campaign/the-crucible | MacBookPro | 10 |
| 2026-08-19T14:04:58Z | gate-commit | 24.683 | 52.013 | 12.646 | 2.62 | 0 | 241fba55 | campaign/the-crucible | MacBookPro | 10 |
| 2026-08-19T14:20:18Z | gate-commit | 85.094 | 54.060 | 14.674 | 0.81 | 0 | 2c8b3e0c | campaign/the-crucible | MacBookPro | 10 |
| 2026-08-19T14:24:56Z | gate-commit | 39.783 | 57.875 | 14.430 | 1.82 | 0 | 85f22f00 | campaign/the-crucible | MacBookPro | 10 |
| 2026-08-19T14:26:40Z | gate-commit | 87.031 | 58.372 | 15.041 | 0.84 | 0 | fc74b412 | campaign/the-crucible | MacBookPro | 10 |
| 2026-08-19T14:28:33Z | gate-commit | 106.280 | 56.463 | 14.127 | 0.66 | 0 | 61525b19 | campaign/the-crucible | MacBookPro | 10 |
| 2026-08-19T14:30:21Z | gate-commit | 101.599 | 58.046 | 14.281 | 0.71 | 0 | 356e8461 | campaign/the-crucible | MacBookPro | 10 |
| 2026-08-19T14:31:57Z | gate-commit | 85.501 | 57.419 | 13.907 | 0.83 | 0 | 336ef095 | campaign/the-crucible | MacBookPro | 10 |
| 2026-08-19T14:33:22Z | gate-commit | 58.029 | 57.079 | 13.806 | 1.22 | 0 | 50f530d8 | campaign/the-crucible | MacBookPro | 10 |
| 2026-08-19T14:34:33Z | gate-commit | 64.413 | 56.208 | 13.289 | 1.08 | 0 | 46077c4a | campaign/the-crucible | MacBookPro | 10 |
| 2026-08-19T14:35:41Z | gate-commit | 62.355 | 56.807 | 13.755 | 1.13 | 0 | e72e64f4 | campaign/the-crucible | MacBookPro | 10 |
| 2026-08-19T14:37:42Z | gate-commit | 48.315 | 52.178 | 12.984 | 1.35 | 0 | 7fd236b7 | campaign/the-crucible | MacBookPro | 10 |
| 2026-08-19T14:38:48Z | gate-commit | 43.871 | 55.425 | 17.309 | 1.66 | 0 | 9594a9e2 | campaign/the-crucible | MacBookPro | 10 |
| 2026-08-19T14:42:57Z | gate-commit | 204.943 | 55.584 | 17.685 | 0.36 | 0 | ed0a6675 | campaign/the-crucible | MacBookPro | 10 |
| 2026-08-19T14:45:43Z | gate-commit | 68.497 | 48.435 | 12.202 | 0.89 | 0 | 1d060584 | campaign/the-crucible | MacBookPro | 10 |
| 2026-08-19T14:46:49Z | gate-commit | 23.389 | 47.021 | 11.415 | 2.50 | 0 | 7c728de3 | campaign/the-crucible | MacBookPro | 10 |
| 2026-08-19T14:52:04Z | gate-commit | 27.609 | 47.278 | 10.919 | 2.11 | 0 | 614ff322 | campaign/the-crucible | MacBookPro | 10 |
| 2026-08-19T15:01:57Z | gate-commit | 22.242 | 43.835 | 10.221 | 2.43 | 0 | 1bf66f50 | campaign/the-crucible | MacBookPro | 10 |
| 2026-08-19T15:04:48Z | gate-commit | 21.570 | 40.974 | 8.428 | 2.29 | 0 | d6cdc926 | campaign/the-crucible | MacBookPro | 10 |
| 2026-08-19T15:42:53Z | gate-commit | 23.120 | 41.646 | 8.496 | 2.17 | 0 | f3b736be | campaign/the-crucible | MacBookPro | 10 |
| 2026-08-19T16:29:09Z | gate-commit | 23.412 | 41.457 | 8.523 | 2.13 | 0 | eacb73fb | campaign/the-crucible | MacBookPro | 10 |
| 2026-08-19T16:33:34Z | gate-commit | 22.089 | 40.922 | 8.504 | 2.24 | 0 | eacb73fb | campaign/the-crucible | MacBookPro | 10 |
| 2026-08-19T13:06:06Z | gate-commit | 155.608 | 3128.939 | 358.767 | 22.41 | 0 | ca6f34310 | fix/census-path-and-main-guard | lefford | 40 |
| 2026-08-19T13:11:15Z | gate-commit | 42.428 | 128.596 | 49.369 | 4.19 | 0 | ca6f34310 | fix/census-path-and-main-guard | lefford | 40 |
| 2026-08-19T13:12:32Z | gate-commit | 42.258 | 123.617 | 47.813 | 4.06 | 0 | 9fb0d5122 | fix/census-path-and-main-guard | lefford | 40 |
| 2026-08-19T13:22:47Z | gate-commit | 44.397 | 128.369 | 49.104 | 4.00 | 0 | 06b74a94b | fix/census-path-and-main-guard | lefford | 40 |
| 2026-08-19T13:24:31Z | sluice:artifacts | 76.393 | 607.656 | 26.489 | 8.30 | 0 | 50a9e6a6a |  | lefford | 40 |
| 2026-08-19T13:25:09Z | sluice:outboard | 37.279 | 24.007 | 36.127 | 1.61 | 0 | 3256252cb |  | lefford | 40 |
| 2026-08-19T13:31:15Z | sluice:gate | 366.172 | 9912.337 | 405.463 | 28.18 | 0 | b2ca6c0a0 |  | lefford | 40 |
| 2026-08-19T13:48:55Z | sluice:seam-guard | 1059.738 | 21081.740 | 1101.145 | 20.93 | 0 | 2b28d51d3 |  | lefford | 40 |
| 2026-08-19T13:53:11Z | sluice:clients | 255.835 | 456.697 | 35.321 | 1.92 | 0 | 4337b206d |  | lefford | 40 |
| 2026-08-19T14:26:36Z | sluice:heavy | 2004.911 | 32921.038 | 705.474 | 16.77 | 0 | e03935864 |  | lefford | 40 |
| 2026-08-19T14:52:36Z | rebaseline | 56.502 | 229.248 | 11.657 | 4.26 | 0 | 1dae62b0 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-19T14:57:03Z | gate-commit | 216.864 | 111.827 | 167.037 | 1.29 | 0 | 1dae62b0 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-19T15:13:29Z | rebaseline | 45.212 | 231.296 | 11.324 | 5.37 | 0 | 713b2f73 | campaign/the-burr | MacBookPro | 10 |
| 2026-08-19T15:30:40Z | census | 918.457 | 29509.410 | 292.817 | 32.45 | 0 | a0e6eb1aa |  | lefford | 40 |
| 2026-08-19T16:17:13Z | gate-commit | 4.932 | 4.430 | 0.493 | 1.00 | 0 | 67cad5491 | fix/census-two-tier-budget | lefford | 40 |
| 2026-08-19T16:18:47Z | gate-commit | 58.158 | 200.159 | 53.550 | 4.36 | 0 | 67cad5491 | fix/census-two-tier-budget | lefford | 40 |
| 2026-08-19T16:20:22Z | gate-commit | 44.106 | 125.686 | 47.812 | 3.93 | 0 | 67cad5491 | fix/census-two-tier-budget | lefford | 40 |
| 2026-08-19T16:21:20Z | gate-commit | 41.682 | 125.676 | 47.236 | 4.15 | 0 | 67cad5491 | fix/census-two-tier-budget | lefford | 40 |
| 2026-08-19T16:27:16Z | sluice:artifacts | 59.887 | 531.345 | 21.366 | 9.23 | 0 | e2532446b |  | lefford | 40 |
| 2026-08-19T16:27:51Z | sluice:outboard | 35.054 | 21.891 | 35.839 | 1.65 | 0 | 247073f1a |  | lefford | 40 |
| 2026-08-19T16:34:39Z | sluice:gate | 407.606 | 10862.979 | 556.179 | 28.02 | 0 | af1ee1bea |  | lefford | 40 |
| 2026-08-19T16:52:24Z | sluice:seam-guard | 1064.683 | 21126.571 | 1100.189 | 20.88 | 0 | 10c9d6736 |  | lefford | 40 |
| 2026-08-19T16:56:56Z | sluice:clients | 271.736 | 479.999 | 33.366 | 1.89 | 0 | bb692f678 |  | lefford | 40 |
| 2026-08-19T17:31:07Z | sluice:heavy | 2051.130 | 32959.710 | 697.260 | 16.41 | 0 | d2d20ad13 |  | lefford | 40 |
| 2026-08-19T17:48:10Z | rebaseline | 122.297 | 234.204 | 12.389 | 2.02 | 0 | 20cf6d7f | campaign/the-crucible | MacBookPro | 10 |
| 2026-08-19T17:49:09Z | gate-commit | 24.404 | 43.246 | 9.558 | 2.16 | 0 | 20cf6d7f | campaign/the-crucible | MacBookPro | 10 |
| 2026-08-19T20:37:03Z | gate-commit | 31.124 | 57.075 | 17.829 | 2.41 | 0 | 724751c3 | campaign/the-crucible | MacBookPro | 10 |
| 2026-08-19T20:42:47Z | gate-commit | 20.672 | 38.139 | 8.007 | 2.23 | 0 | 724751c3 | campaign/the-crucible | MacBookPro | 10 |
| 2026-08-19T16:51:41Z | gate-commit | 60.777 | 52.547 | 118.257 | 2.81 | 0 | d26f5681 | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T16:52:01Z | gate-commit | 11.383 | 10.660 | 0.293 | 0.96 | 0 | d26f5681 | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T16:53:08Z | rebaseline | 52.425 | 230.604 | 11.578 | 4.62 | 0 | d26f5681 | campaign/the-gazetteer | MacBookPro | 10 |
| 2026-08-19T17:46:35Z | sluice:artifacts | 102.126 | 899.700 | 40.546 | 9.21 | 0 | 2b3c37b97 |  | lefford | 40 |
| 2026-08-19T17:47:10Z | sluice:outboard | 34.583 | 22.500 | 35.666 | 1.68 | 0 | c4f5cf868 |  | lefford | 40 |
| 2026-08-19T17:53:48Z | sluice:gate | 397.978 | 10531.917 | 535.110 | 27.81 | 0 | a7ada90e2 |  | lefford | 40 |
| 2026-08-19T18:12:55Z | sluice:seam-guard | 1146.604 | 21376.137 | 1162.352 | 19.66 | 0 | 647f84696 |  | lefford | 40 |
| 2026-08-19T18:17:24Z | sluice:clients | 268.727 | 494.661 | 39.490 | 1.99 | 0 | c892983ac |  | lefford | 40 |
| 2026-08-19T18:51:28Z | sluice:heavy | 2043.859 | 32904.942 | 715.913 | 16.45 | 0 | dec46ffde |  | lefford | 40 |
| 2026-08-19T16:05:57Z | rebaseline | 92.854 | 238.191 | 12.258 | 2.70 | 0 | c8e95860 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T16:11:06Z | gate-commit | 296.308 | 572.632 | 189.243 | 2.57 | 0 | c8e95860 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T16:31:33Z | remeasure:undertow_readout | 122.817 | 115.879 | 5.909 | 0.99 | 0 | 7b65e30d | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T16:32:13Z | remeasure:probe_crossing_scale | 39.460 | 37.526 | 1.156 | 0.98 | 0 | 7b65e30d | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T16:36:14Z | remeasure:probe_seam_direction | 240.812 | 253.854 | 14.907 | 1.12 | 0 | 7b65e30d | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T16:37:11Z | remeasure:probe_tiebreak_rules | 56.746 | 53.860 | 2.312 | 0.99 | 0 | 7b65e30d | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T16:38:45Z | remeasure:probe_argmin_defect_crossing_arms | 94.537 | 91.129 | 2.527 | 0.99 | 0 | 7b65e30d | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T16:40:26Z | remeasure:parley_readout | 100.871 | 95.406 | 5.115 | 1.00 | 0 | 7b65e30d | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T16:42:43Z | gate-commit | 41.818 | 63.044 | 17.841 | 1.93 | 0 | 7b65e30d | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T17:01:31Z | gate-commit | 35.799 | 56.143 | 16.266 | 2.02 | 0 | f4625217 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T17:02:23Z | rebaseline | 39.277 | 233.724 | 12.650 | 6.27 | 0 | f4625217 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T17:27:06Z | remeasure:probe_argmin_setdiff | 91.779 | 92.850 | 2.324 | 1.04 | 0 | 34ac452b | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T17:31:31Z | gate-commit | 27.043 | 64.221 | 15.950 | 2.96 | 0 | 34ac452b | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T17:32:26Z | gate-commit | 23.711 | 55.405 | 14.606 | 2.95 | 0 | e9186f49 | campaign/the-undertow | MacBookPro | 10 |
| 2026-08-19T18:54:33Z | sluice:artifacts | 89.508 | 733.037 | 29.133 | 8.52 | 0 | 01f4718b1 |  | lefford | 40 |
| 2026-08-19T18:55:08Z | sluice:outboard | 34.929 | 21.859 | 35.420 | 1.64 | 0 | 4f73047a7 |  | lefford | 40 |
| 2026-08-19T19:01:25Z | sluice:gate | 376.713 | 10234.320 | 431.323 | 28.31 | 0 | f939f0c20 |  | lefford | 40 |
| 2026-08-19T19:20:35Z | sluice:seam-guard | 1150.144 | 21624.849 | 1150.957 | 19.80 | 0 | d03b4812b |  | lefford | 40 |
| 2026-08-19T19:24:49Z | sluice:clients | 253.873 | 444.215 | 31.035 | 1.87 | 0 | 03efbd683 |  | lefford | 40 |
| 2026-08-19T20:03:41Z | sluice:heavy | 2331.074 | 34777.573 | 726.646 | 15.23 | 0 | eaa65f910 |  | lefford | 40 |
| 2026-08-19T19:06:14Z | gate-commit | 3.928 | 3.771 | 0.156 | 1.00 | 0 | 17725d9da | fix/probe-tier-and-lean-merge | lefford | 40 |
| 2026-08-19T19:09:39Z | gate-commit | 189.025 | 2648.607 | 291.864 | 15.56 | 0 | 17725d9da | fix/probe-tier-and-lean-merge | lefford | 40 |
| 2026-08-19T20:08:13Z | sluice:artifacts | 58.768 | 532.107 | 18.952 | 9.38 | 0 | 6e9431735 |  | lefford | 40 |
| 2026-08-19T20:08:48Z | sluice:outboard | 35.040 | 22.062 | 35.321 | 1.64 | 0 | 53cdc91f1 |  | lefford | 40 |
| 2026-08-19T20:14:45Z | sluice:gate | 356.682 | 9870.309 | 340.178 | 28.63 | 0 | 080ed2125 |  | lefford | 40 |
| 2026-08-19T20:17:45Z | sluice:clients | 180.173 | 356.002 | 12.044 | 2.04 | 0 | 802efc296 |  | lefford | 40 |
| 2026-08-19T16:54:47Z | gate-commit | 42.036 | 79.176 | 27.499 | 2.54 | 0 | b0277ee07 | fix/census-yellow-teeth | lefford | 40 |
| 2026-08-19T16:55:38Z | gate-commit | 41.767 | 113.382 | 38.968 | 3.65 | 0 | b0277ee07 | fix/census-yellow-teeth | lefford | 40 |
| 2026-08-19T17:59:39Z | gate-commit | 140.786 | 609.573 | 204.536 | 5.78 | 0 | 9da5ef385 | fix/census-yellow-teeth | lefford | 40 |
| 2026-08-19T20:29:35Z | sluice:artifacts | 61.041 | 529.741 | 21.119 | 9.02 | 0 | fa0eaec1f |  | lefford | 40 |
| 2026-08-19T20:30:11Z | sluice:outboard | 35.861 | 22.860 | 36.156 | 1.65 | 0 | 4bf3d92ff |  | lefford | 40 |
| 2026-08-19T20:36:24Z | sluice:gate | 372.616 | 10123.744 | 502.949 | 28.52 | 0 | ee98dcf33 |  | lefford | 40 |
| 2026-08-19T20:39:15Z | sluice:clients | 170.640 | 364.925 | 19.552 | 2.25 | 0 | 3a48f8aa6 |  | lefford | 40 |
| 2026-08-19T20:45:06Z | rebaseline | 79.620 | 232.572 | 12.057 | 3.07 | 0 | 5b3edebd | campaign/the-crucible | MacBookPro | 10 |
| 2026-08-19T20:47:00Z | gate-commit | 96.555 | 558.666 | 29.225 | 6.09 | 0 | 5b3edebd | campaign/the-crucible | MacBookPro | 10 |
| 2026-08-19T20:49:59Z | sluice:artifacts | 62.629 | 540.542 | 21.672 | 8.98 | 0 | 65f072caa |  | lefford | 40 |
| 2026-08-19T20:50:35Z | sluice:outboard | 34.987 | 22.329 | 35.673 | 1.66 | 0 | 0b636033b |  | lefford | 40 |
| 2026-08-19T20:56:37Z | sluice:gate | 362.360 | 9908.997 | 360.100 | 28.34 | 0 | e10ed1b87 |  | lefford | 40 |
| 2026-08-19T20:59:32Z | sluice:clients | 174.492 | 358.880 | 17.518 | 2.16 | 0 | 5fa3ba224 |  | lefford | 40 |
| 2026-08-19T21:27:01Z | sluice:artifacts | 59.755 | 536.770 | 25.050 | 9.40 | 0 | 019e744c5 |  | lefford | 40 |
| 2026-08-19T21:27:36Z | sluice:outboard | 34.556 | 21.633 | 35.336 | 1.65 | 0 | abbd38eae |  | lefford | 40 |
| 2026-08-19T21:33:47Z | sluice:gate | 371.488 | 9985.746 | 342.271 | 27.80 | 0 | 62af7b91c |  | lefford | 40 |
| 2026-08-19T21:33:06Z | gate-commit | 116.270 | 800.687 | 76.272 | 7.54 | 0 | 627354795 | fix/sluice-record-and-mouth | lefford | 40 |
| 2026-08-19T21:35:39Z | sluice:artifacts | 61.962 | 543.552 | 21.264 | 9.12 | 0 | 057a97521 |  | lefford | 40 |
| 2026-08-19T21:36:15Z | sluice:outboard | 35.825 | 22.694 | 36.126 | 1.64 | 0 | bcccaa642 |  | lefford | 40 |
| 2026-08-19T21:42:11Z | sluice:gate | 355.835 | 9830.380 | 334.579 | 28.57 | 0 | 57a2fb40a |  | lefford | 40 |
| 2026-08-19T21:45:04Z | sluice:clients | 173.428 | 352.869 | 10.911 | 2.10 | 0 | 897c80f0d |  | lefford | 40 |
| 2026-08-19T21:51:53Z | sluice:artifacts | 119.502 | 989.406 | 43.897 | 8.65 | 0 | 3727727ca |  | lefford | 40 |
| 2026-08-19T21:52:28Z | sluice:outboard | 34.769 | 22.012 | 35.342 | 1.65 | 0 | efe861f89 |  | lefford | 40 |
| 2026-08-19T21:59:08Z | sluice:gate | 399.905 | 10909.994 | 421.366 | 28.34 | 0 | 0a286a712 |  | lefford | 40 |
| 2026-08-19T22:03:47Z | sluice:clients | 278.946 | 508.607 | 33.318 | 1.94 | 0 | b925ea3d8 |  | lefford | 40 |
| 2026-08-19T22:14:45Z | sluice:artifacts | 84.859 | 654.492 | 29.279 | 8.06 | 0 | 260a9028f |  | lefford | 40 |
| 2026-08-19T22:15:20Z | sluice:outboard | 34.937 | 22.001 | 35.653 | 1.65 | 0 | fedac74fb |  | lefford | 40 |
| 2026-08-19T22:21:25Z | sluice:gate | 364.388 | 9885.760 | 364.022 | 28.13 | 0 | 9010086fb |  | lefford | 40 |
| 2026-08-19T22:25:02Z | sluice:clients | 216.650 | 403.301 | 22.550 | 1.97 | 0 | 196401c37 |  | lefford | 40 |
| 2026-08-19T23:27:25Z | rebaseline | 66.902 | 235.973 | 12.910 | 3.72 | 0 | 71c7591f | campaign/the-bridle | MacBookPro | 10 |
| 2026-08-19T23:28:50Z | gate-commit | 57.139 | 150.625 | 18.361 | 2.96 | 0 | 71c7591f | campaign/the-bridle | MacBookPro | 10 |
| 2026-08-19T23:29:30Z | gate-commit | 23.704 | 49.423 | 11.914 | 2.59 | 0 | 71c7591f | campaign/the-bridle | MacBookPro | 10 |
| 2026-08-19T23:37:48Z | rebaseline | 51.456 | 230.769 | 12.393 | 4.73 | 0 | 068d1d62 | campaign/the-bridle | MacBookPro | 10 |
| 2026-08-19T23:38:27Z | gate-commit | 29.440 | 59.347 | 14.820 | 2.52 | 0 | 068d1d62 | campaign/the-bridle | MacBookPro | 10 |
| 2026-08-19T23:39:18Z | gate-commit | 22.663 | 48.783 | 11.276 | 2.65 | 0 | 068d1d62 | campaign/the-bridle | MacBookPro | 10 |
| 2026-08-19T23:46:38Z | rebaseline | 51.565 | 232.163 | 12.123 | 4.74 | 0 | 44202cce | campaign/the-bridle | MacBookPro | 10 |
| 2026-08-19T23:47:23Z | gate-commit | 27.385 | 55.937 | 14.833 | 2.58 | 0 | 44202cce | campaign/the-bridle | MacBookPro | 10 |
| 2026-08-19T23:47:59Z | gate-commit | 22.759 | 48.748 | 11.260 | 2.64 | 0 | 44202cce | campaign/the-bridle | MacBookPro | 10 |
| 2026-08-20T01:19:50Z | rebaseline | 38.096 | 230.797 | 11.931 | 6.37 | 0 | d8297bc1 | campaign/the-bridle | MacBookPro | 10 |
| 2026-08-20T01:24:35Z | gate-commit | 22.127 | 48.446 | 11.160 | 2.69 | 0 | d8297bc1 | campaign/the-bridle | MacBookPro | 10 |
| 2026-08-20T01:33:09Z | sluice:artifacts | 104.529 | 809.515 | 34.207 | 8.07 | 0 | 4d3d34ef4 |  | lefford | 40 |
| 2026-08-20T01:33:45Z | sluice:outboard | 35.534 | 21.988 | 36.111 | 1.64 | 0 | eef5208cd |  | lefford | 40 |
| 2026-08-20T01:40:15Z | sluice:gate | 389.674 | 10311.323 | 386.181 | 27.45 | 0 | 63a94e19e |  | lefford | 40 |
| 2026-08-20T01:44:01Z | sluice:clients | 225.947 | 421.186 | 24.497 | 1.97 | 0 | 583ecb835 |  | lefford | 40 |
| 2026-08-20T02:25:14Z | sluice:artifacts | 100.462 | 692.004 | 31.698 | 7.20 | 0 | 9f5045a1b |  | lefford | 40 |
| 2026-08-20T02:25:50Z | sluice:outboard | 35.388 | 22.152 | 36.528 | 1.66 | 0 | 6003d1f0f |  | lefford | 40 |
| 2026-08-20T02:32:13Z | sluice:gate | 382.038 | 10222.403 | 387.072 | 27.77 | 0 | 0e7f3e8ca |  | lefford | 40 |
| 2026-08-20T02:35:17Z | sluice:clients | 184.303 | 369.823 | 12.833 | 2.08 | 0 | 228d38abd |  | lefford | 40 |
| 2026-08-20T02:20:32Z | gate-commit | 124.262 | 532.871 | 136.830 | 5.39 | 0 | 21bea6fe | fix/the-tackle-close-followups | MacBookPro | 10 |
| 2026-08-20T02:36:56Z | sluice:artifacts | 62.391 | 540.197 | 19.711 | 8.97 | 0 | fc1dff12a |  | lefford | 40 |
| 2026-08-20T02:37:32Z | sluice:outboard | 35.491 | 21.910 | 35.683 | 1.62 | 0 | d1827eb91 |  | lefford | 40 |
| 2026-08-20T02:43:29Z | sluice:gate | 356.370 | 9846.901 | 341.166 | 28.59 | 0 | d123648f8 |  | lefford | 40 |
| 2026-08-20T02:46:31Z | sluice:clients | 182.037 | 369.786 | 11.417 | 2.09 | 0 | ebbbc2d08 |  | lefford | 40 |
| 2026-08-20T15:08:40Z | prewarm | 147.748 | 1062.686 | 38.642 | 7.45 | 0 | 6d7e1ca9 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-20T15:21:06Z | gate-commit | 38.442 | 113.582 | 26.941 | 3.66 | 0 | 6d7e1ca9 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-20T15:22:01Z | gate-commit | 22.545 | 50.405 | 11.460 | 2.74 | 0 | 6d7e1ca9 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-20T16:15:28Z | gate-commit | 22.560 | 50.358 | 11.434 | 2.74 | 0 | db23f36b | campaign/the-deed | MacBookPro | 10 |
| 2026-08-20T16:16:21Z | gate-commit | 22.553 | 50.378 | 11.842 | 2.76 | 0 | db23f36b | campaign/the-deed | MacBookPro | 10 |
| 2026-08-20T16:37:33Z | gate-commit | 33.077 | 105.880 | 21.607 | 3.85 | 0 | 76f65816 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-20T16:39:19Z | gate-commit | 31.060 | 53.571 | 11.815 | 2.11 | 0 | 76f65816 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-20T16:47:12Z | rebaseline | 53.688 | 235.307 | 12.373 | 4.61 | 0 | 9cae88e3 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-20T16:48:15Z | gate-commit | 38.875 | 107.644 | 18.285 | 3.24 | 0 | 9cae88e3 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-20T16:48:53Z | gate-commit | 24.779 | 51.539 | 11.932 | 2.56 | 0 | 9cae88e3 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-20T16:59:11Z | gate-commit | 34.917 | 91.722 | 17.524 | 3.13 | 0 | b1cb04d5 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-20T16:59:48Z | gate-commit | 22.797 | 50.350 | 11.628 | 2.72 | 0 | b1cb04d5 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-19T21:10:11Z | rebaseline | 58.417 | 229.504 | 11.890 | 4.13 | 0 | 39f8629d | campaign/the-planes | MacBookPro | 10 |
| 2026-08-19T21:11:54Z | gate-commit | 85.511 | 84.519 | 43.602 | 1.50 | 0 | 39f8629d | campaign/the-planes | MacBookPro | 10 |
| 2026-08-19T21:12:17Z | gate-commit | 22.682 | 49.363 | 11.004 | 2.66 | 0 | 39f8629d | campaign/the-planes | MacBookPro | 10 |
| 2026-08-19T21:42:31Z | gate-commit | 35.827 | 49.548 | 11.006 | 1.69 | 0 | 93ebbbac | campaign/the-planes | MacBookPro | 10 |
| 2026-08-19T22:00:00Z | gate-commit | 49.664 | 49.574 | 11.034 | 1.22 | 0 | 56e49e71 | campaign/the-planes | MacBookPro | 10 |
| 2026-08-19T22:52:19Z | gate-commit | 59.584 | 53.555 | 12.278 | 1.10 | 0 | e2698c03 | campaign/the-planes | MacBookPro | 10 |
| 2026-08-19T22:58:56Z | gate-commit | 74.370 | 51.272 | 12.385 | 0.86 | 0 | e61a11fc | campaign/the-planes | MacBookPro | 10 |
| 2026-08-19T23:10:47Z | prewarm | 202.181 | 1078.527 | 41.013 | 5.54 | 0 | b5a0e047 | campaign/the-winze | MacBookPro | 10 |
| 2026-08-19T23:11:09Z | gate-commit | 58.390 | 90.815 | 15.727 | 1.82 | 0 | b5a0e047 | campaign/the-winze | MacBookPro | 10 |
| 2026-08-19T23:22:10Z | gate-commit | 40.220 | 97.943 | 25.542 | 3.07 | 0 | ccd9076e | campaign/the-winze | MacBookPro | 10 |
| 2026-08-19T23:23:20Z | gate-commit | 24.144 | 49.454 | 11.601 | 2.53 | 0 | ccd9076e | campaign/the-winze | MacBookPro | 10 |
| 2026-08-20T00:15:00Z | gate-commit | 4.603 | 9.304 | 1.753 | 2.40 | 0 | dc6cfb1f | campaign/the-winze | MacBookPro | 10 |
| 2026-08-20T00:16:06Z | gate-commit | 27.781 | 59.757 | 21.165 | 2.91 | 0 | dc6cfb1f | campaign/the-winze | MacBookPro | 10 |
| 2026-08-20T00:18:23Z | gate-commit | 22.034 | 48.528 | 11.335 | 2.72 | 0 | dc6cfb1f | campaign/the-winze | MacBookPro | 10 |
| 2026-08-20T01:02:55Z | gate-commit | 24.520 | 49.959 | 11.806 | 2.52 | 0 | 7a3a4691 | campaign/the-winze | MacBookPro | 10 |
| 2026-08-20T02:14:15Z | gate-commit | 43.542 | 74.498 | 24.306 | 2.27 | 0 | 664e02ef | campaign/the-winze | MacBookPro | 10 |
| 2026-08-20T02:15:23Z | gate-commit | 32.592 | 52.360 | 11.853 | 1.97 | 0 | 664e02ef | campaign/the-winze | MacBookPro | 10 |
| 2026-08-20T11:42:42Z | gate-commit | 26.175 | 59.583 | 22.743 | 3.15 | 0 | decf4c57 | campaign/the-winze | MacBookPro | 10 |
| 2026-08-20T11:46:52Z | gate-commit | 22.025 | 48.264 | 11.353 | 2.71 | 0 | decf4c57 | campaign/the-winze | MacBookPro | 10 |
| 2026-08-20T13:45:42Z | gate-commit | 23.805 | 49.708 | 11.536 | 2.57 | 0 | a98b0b38 | campaign/the-winze | MacBookPro | 10 |
| 2026-08-20T13:46:04Z | gate-commit | 22.301 | 48.882 | 11.600 | 2.71 | 0 | a98b0b38 | campaign/the-winze | MacBookPro | 10 |
| 2026-08-20T13:58:16Z | gate-commit | 22.653 | 49.193 | 11.659 | 2.69 | 0 | 5fce5e86 | campaign/the-winze | MacBookPro | 10 |
| 2026-08-20T14:25:01Z | gate-commit | 59.659 | 305.375 | 25.612 | 5.55 | 0 | ffc980c3 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T13:52:58Z | prewarm | 62.641 | 442.709 | 19.177 | 7.37 | 0 | 74aac3f8 | campaign/the-cupel | MacBookPro | 10 |
| 2026-08-20T14:31:53Z | prewarm | 152.860 | 1065.125 | 39.042 | 7.22 | 0 | 74aac3f8 | campaign/the-tally | MacBookPro | 10 |
| 2026-08-20T14:32:59Z | gate-commit | 32.459 | 86.391 | 14.755 | 3.12 | 0 | 74aac3f8 | campaign/the-tally | MacBookPro | 10 |
| 2026-08-20T14:34:01Z | gate-commit | 29.113 | 49.795 | 10.794 | 2.08 | 0 | 74aac3f8 | campaign/the-tally | MacBookPro | 10 |
| 2026-08-20T14:34:49Z | gate-commit | 31.857 | 52.425 | 12.363 | 2.03 | 0 | 74aac3f8 | campaign/the-tally | MacBookPro | 10 |
| 2026-08-20T15:08:36Z | sluice:artifacts | 58.582 | 535.531 | 22.588 | 9.53 | 0 | 7e963c86d |  | lefford | 40 |
| 2026-08-20T15:09:12Z | sluice:outboard | 35.559 | 22.084 | 35.742 | 1.63 | 0 | ca9facf30 |  | lefford | 40 |
| 2026-08-20T15:15:12Z | sluice:gate | 359.770 | 9900.907 | 339.753 | 28.46 | 0 | 651e5c2f4 |  | lefford | 40 |
| 2026-08-20T15:18:06Z | sluice:clients | 173.634 | 351.356 | 11.524 | 2.09 | 0 | cc02b80e5 |  | lefford | 40 |
| 2026-08-20T15:28:11Z | rebaseline | 51.084 | 231.651 | 12.281 | 4.78 | 0 | c2349725 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T15:37:47Z | gate-commit | 89.773 | 513.991 | 35.140 | 6.12 | 0 | 9cb439bd | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T15:38:30Z | gate-commit | 25.445 | 51.826 | 12.542 | 2.53 | 0 | 9cb439bd | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T15:40:51Z | gate-commit | 23.118 | 50.453 | 11.983 | 2.70 | 0 | 9cb439bd | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T16:51:38Z | gate-commit | 27.864 | 55.224 | 12.464 | 2.43 | 0 | a1ce22bd | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T16:53:13Z | gate-commit | 28.477 | 52.238 | 12.162 | 2.26 | 0 | a1ce22bd | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T17:29:08Z | rebaseline | 53.293 | 234.001 | 11.961 | 4.62 | 0 | fd60b8ea | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T17:31:25Z | gate-commit | 23.682 | 50.543 | 11.546 | 2.62 | 0 | fd60b8ea | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T17:41:45Z | gate-commit | 40.581 | 54.073 | 12.676 | 1.64 | 0 | fd60b8ea | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T18:06:55Z | gate-commit | 30.263 | 70.042 | 25.125 | 3.14 | 0 | fb7caf16 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T18:07:46Z | gate-commit | 22.284 | 49.543 | 11.427 | 2.74 | 0 | fb7caf16 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-19T22:53:32Z | prewarm | 136.240 | 850.457 | 41.544 | 6.55 | 0 | 21f06adb | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-19T23:28:49Z | gate-commit | 69.186 | 80.460 | 27.507 | 1.56 | 0 | 21f06adb | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-19T23:30:56Z | gate-commit | 23.541 | 49.012 | 11.471 | 2.57 | 0 | 21f06adb | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-20T01:04:10Z | gate-commit | 2.190 | 1.971 | 0.104 | 0.95 | 0 | 14c7725c | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-20T01:04:24Z | gate-commit | 2.541 | 2.178 | 0.231 | 0.95 | 0 | 14c7725c | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-20T01:05:00Z | gate-commit | 24.488 | 50.804 | 11.771 | 2.56 | 0 | 14c7725c | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-20T01:05:41Z | gate-commit | 23.856 | 49.744 | 12.237 | 2.60 | 0 | 14c7725c | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-20T02:08:17Z | gate-commit | 44.616 | 52.594 | 12.576 | 1.46 | 0 | f868465d | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-20T02:17:59Z | gate-commit | 22.286 | 48.541 | 11.523 | 2.70 | 0 | 9999449a | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-20T02:51:27Z | gate-commit | 22.486 | 48.739 | 11.626 | 2.68 | 0 | 698ab0dd | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-20T02:52:07Z | gate-commit | 22.247 | 48.641 | 11.295 | 2.69 | 0 | 48869474 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-20T03:00:42Z | gate-commit | 22.237 | 48.556 | 11.262 | 2.69 | 0 | 1a6caa11 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-20T03:29:40Z | gate-commit | 20.275 | 29.983 | 12.082 | 2.07 | 0 | 2013e5d5 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-20T03:30:50Z | gate-commit | 59.393 | 316.847 | 31.863 | 5.87 | 0 | 2013e5d5 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-20T03:33:03Z | gate-commit | 22.361 | 49.130 | 11.038 | 2.69 | 0 | 2013e5d5 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-20T03:36:13Z | gate-commit | 39.096 | 75.875 | 45.712 | 3.11 | 0 | 2013e5d5 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-20T04:05:36Z | gate-commit | 22.290 | 48.512 | 11.037 | 2.67 | 0 | 9f69e4e2 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-20T04:08:21Z | gate-commit | 22.171 | 48.515 | 11.001 | 2.68 | 0 | 9f69e4e2 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-20T04:08:58Z | gate-commit | 21.844 | 48.413 | 11.096 | 2.72 | 0 | 9f69e4e2 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-20T04:16:11Z | gate-commit | 21.999 | 48.428 | 10.798 | 2.69 | 0 | 81d940d9 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-20T04:32:00Z | gate-commit | 22.247 | 48.396 | 10.890 | 2.66 | 0 | 81d940d9 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-20T04:33:54Z | gate-commit | 22.009 | 48.504 | 10.873 | 2.70 | 0 | 81d940d9 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-20T04:46:32Z | gate-commit | 39.499 | 78.030 | 47.487 | 3.18 | 0 | 64c80be3 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-20T04:47:35Z | gate-commit | 22.118 | 48.522 | 11.224 | 2.70 | 0 | 64c80be3 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-20T15:58:01Z | prewarm | 264.308 | 1096.565 | 38.047 | 4.29 | 0 | 62627de2 | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T15:58:26Z | game-check | 168.414 | 357.371 | 13.023 | 2.20 | 0 | 62627de2 | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T16:00:16Z | rebaseline | 105.120 | 234.201 | 10.508 | 2.33 | 0 | 62627de2 | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T16:02:21Z | game-check | 97.270 | 291.738 | 3.724 | 3.04 | 0 | 62627de2 | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T16:04:24Z | gate-commit | 118.086 | 388.223 | 31.845 | 3.56 | 0 | 62627de2 | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T16:06:09Z | gate-commit | 35.070 | 56.238 | 12.097 | 1.95 | 0 | 62627de2 | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T16:06:49Z | gate-commit | 33.521 | 54.738 | 12.042 | 1.99 | 0 | 62627de2 | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T16:08:01Z | gate-commit | 23.288 | 50.255 | 11.797 | 2.66 | 0 | 62627de2 | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T16:16:50Z | gate-commit | 22.638 | 49.773 | 11.868 | 2.72 | 0 | 69dec43c | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T16:22:58Z | game-check | 53.697 | 283.834 | 5.485 | 5.39 | 0 | bdf95861 | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T16:24:45Z | gate-commit | 22.755 | 49.338 | 11.789 | 2.69 | 0 | bdf95861 | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T16:40:56Z | game-check | 54.247 | 289.676 | 6.104 | 5.45 | 0 | b590d290 | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T16:43:34Z | game-check | 53.035 | 282.263 | 4.119 | 5.40 | 0 | b590d290 | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T16:45:22Z | gate-commit | 23.038 | 49.906 | 12.036 | 2.69 | 0 | b590d290 | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T15:58:00Z | rebaseline | 157.490 | 242.035 | 10.821 | 1.61 | 0 | c1a4fb8c | campaign/the-cupel | MacBookPro | 10 |
| 2026-08-20T16:06:21Z | sluice:artifacts | 85.442 | 649.343 | 28.101 | 7.93 | 0 | abec69bd3 |  | lefford | 40 |
| 2026-08-20T16:06:57Z | sluice:outboard | 35.565 | 22.497 | 36.319 | 1.65 | 0 | 7aca4abcd |  | lefford | 40 |
| 2026-08-20T16:13:01Z | sluice:gate | 364.288 | 9958.475 | 366.516 | 28.34 | 0 | eb6544b50 |  | lefford | 40 |
| 2026-08-20T16:16:00Z | sluice:clients | 178.401 | 356.813 | 11.414 | 2.06 | 0 | 28c72e9a9 |  | lefford | 40 |
| 2026-08-20T17:04:51Z | rebaseline | 108.663 | 242.659 | 11.811 | 2.34 | 0 | c45d3120 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-20T17:05:47Z | gate-commit | 34.099 | 107.923 | 20.123 | 3.76 | 0 | c45d3120 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-20T17:06:10Z | gate-commit | 23.138 | 50.633 | 11.712 | 2.69 | 0 | d999032f | campaign/the-deed | MacBookPro | 10 |
| 2026-08-20T17:10:50Z | gate-commit | 31.737 | 77.385 | 17.724 | 3.00 | 0 | d999032f | campaign/the-deed | MacBookPro | 10 |
| 2026-08-20T17:11:31Z | gate-commit | 22.728 | 50.363 | 11.859 | 2.74 | 0 | d999032f | campaign/the-deed | MacBookPro | 10 |
| 2026-08-20T17:19:15Z | gate-commit | 41.667 | 62.259 | 16.280 | 1.88 | 0 | 0a89d8d3 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-20T17:20:12Z | gate-commit | 33.691 | 54.914 | 12.282 | 1.99 | 0 | 0a89d8d3 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-20T18:02:28Z | gate-commit | 26.219 | 56.535 | 12.656 | 2.64 | 0 | 1f62d49c | campaign/the-deed | MacBookPro | 10 |
| 2026-08-20T18:03:09Z | gate-commit | 22.973 | 50.612 | 11.587 | 2.71 | 0 | 1f62d49c | campaign/the-deed | MacBookPro | 10 |
| 2026-08-20T18:20:31Z | gate-commit | 26.649 | 61.331 | 15.552 | 2.89 | 0 | e594084e | campaign/the-deed | MacBookPro | 10 |
| 2026-08-20T18:23:18Z | gate-commit | 22.362 | 50.162 | 11.672 | 2.77 | 0 | e594084e | campaign/the-deed | MacBookPro | 10 |
| 2026-08-20T16:53:06Z | rebaseline | 52.335 | 232.932 | 11.459 | 4.67 | 0 | d9ed93ad | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T16:56:09Z | gate-commit | 31.311 | 89.760 | 18.932 | 3.47 | 0 | d9ed93ad | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T16:56:54Z | gate-commit | 22.797 | 50.436 | 11.426 | 2.71 | 0 | a7f00ebe | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T18:51:18Z | game-check | 0.240 | 0.171 | 0.043 | 0.89 | 0 | a7f00ebe | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T18:51:25Z | game-check | 0.735 | 0.696 | 0.173 | 1.18 | 0 | a7f00ebe | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T18:51:56Z | game-check | 1.306 | 1.334 | 0.470 | 1.38 | 0 | a7f00ebe | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T18:53:45Z | game-check | 95.588 | 530.928 | 7.751 | 5.64 | 0 | a7f00ebe | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T18:58:18Z | game-check | 85.996 | 515.570 | 4.828 | 6.05 | 0 | a7f00ebe | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T18:59:51Z | gate-commit | 31.456 | 53.748 | 12.365 | 2.10 | 0 | a7f00ebe | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T19:14:17Z | game-check | 124.156 | 522.501 | 5.395 | 4.25 | 0 | 5e87b9cb | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T19:16:02Z | gate-commit | 35.590 | 54.529 | 12.353 | 1.88 | 0 | 5e87b9cb | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T19:30:00Z | game-check | 119.566 | 511.065 | 7.914 | 4.34 | 0 | 10066d8e | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T19:32:02Z | gate-commit | 24.043 | 50.922 | 12.301 | 2.63 | 0 | 10066d8e | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T17:32:45Z | sluice:artifacts | 114.009 | 891.601 | 40.773 | 8.18 | 0 | c806677f1 |  | lefford | 40 |
| 2026-08-20T17:33:20Z | sluice:outboard | 35.020 | 21.076 | 33.089 | 1.55 | 0 | cc94eb220 |  | lefford | 40 |
| 2026-08-20T17:39:59Z | sluice:gate | 398.605 | 10777.476 | 426.686 | 28.11 | 0 | b59a73c4b |  | lefford | 40 |
| 2026-08-20T17:44:43Z | sluice:clients | 283.994 | 748.582 | 37.533 | 2.77 | 0 | 06119a466 |  | lefford | 40 |
| 2026-08-20T18:35:16Z | rebaseline | 72.757 | 234.080 | 11.594 | 3.38 | 0 | dc324195 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-20T18:36:55Z | gate-commit | 82.758 | 396.714 | 36.901 | 5.24 | 0 | dc324195 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-20T19:28:42Z | rebaseline | 72.489 | 229.619 | 10.256 | 3.31 | 0 | 552d38ef | campaign/the-deed | MacBookPro | 10 |
| 2026-08-20T19:30:08Z | gate-commit | 59.001 | 215.127 | 28.279 | 4.13 | 0 | 552d38ef | campaign/the-deed | MacBookPro | 10 |
| 2026-08-20T19:30:52Z | gate-commit | 23.685 | 50.732 | 12.181 | 2.66 | 0 | 552d38ef | campaign/the-deed | MacBookPro | 10 |
| 2026-08-20T20:24:04Z | rebaseline | 68.476 | 233.802 | 11.789 | 3.59 | 0 | f9064cd8 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-20T20:25:04Z | gate-commit | 42.703 | 96.236 | 26.545 | 2.88 | 0 | f9064cd8 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-20T20:26:28Z | gate-commit | 27.232 | 51.518 | 11.626 | 2.32 | 0 | f9064cd8 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-20T20:35:17Z | gate-commit | 25.110 | 57.747 | 12.645 | 2.80 | 0 | f1a555c9 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-22T21:22:05Z | rebaseline | 70.540 | 232.148 | 11.529 | 3.45 | 0 | 21f32555e | campaign/the-deed | MacBookPro | 10 |
| 2026-08-22T21:23:30Z | gate-commit | 29.987 | 62.969 | 26.026 | 2.97 | 0 | 21f32555e | campaign/the-deed | MacBookPro | 10 |
| 2026-08-22T21:29:21Z | rebaseline | 45.561 | 236.785 | 11.172 | 5.44 | 0 | 98a892918 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-22T21:30:24Z | gate-commit | 27.662 | 54.726 | 13.885 | 2.48 | 0 | 98a892918 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-20T18:12:54Z | rebaseline | 72.187 | 234.603 | 12.190 | 3.42 | 0 | 2f24e28e | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T18:14:08Z | gate-commit | 73.583 | 411.105 | 35.170 | 6.06 | 0 | 2f24e28e | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T18:14:52Z | gate-commit | 22.763 | 50.235 | 11.784 | 2.72 | 0 | 2f24e28e | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T18:37:04Z | quick | 13.184 | 11.343 | 0.335 | 0.89 | 0 | 5bdf0f38 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T18:38:11Z | rebaseline | 61.885 | 236.083 | 12.164 | 4.01 | 0 | 5bdf0f38 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T18:39:24Z | rebaseline | 56.349 | 234.954 | 11.537 | 4.37 | 0 | 5bdf0f38 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T18:50:08Z | gate-commit | 46.055 | 65.631 | 16.723 | 1.79 | 0 | 5bdf0f38 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T18:51:01Z | gate-commit | 27.800 | 52.009 | 12.130 | 2.31 | 0 | 5bdf0f38 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T19:31:28Z | gate-commit | 29.708 | 62.238 | 16.966 | 2.67 | 0 | 231d188f | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T19:32:16Z | gate-commit | 26.736 | 51.377 | 12.166 | 2.38 | 0 | 231d188f | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T20:04:25Z | rebaseline | 60.108 | 243.683 | 11.957 | 4.25 | 0 | 4dfb552a | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T20:05:34Z | rebaseline | 40.345 | 236.206 | 11.833 | 6.15 | 0 | 4dfb552a | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T20:06:18Z | rebaseline | 38.698 | 238.769 | 13.366 | 6.52 | 0 | 4dfb552a | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T20:07:43Z | rebaseline | 66.860 | 239.343 | 11.696 | 3.75 | 0 | 4dfb552a | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T20:09:06Z | rebaseline | 65.835 | 240.142 | 12.197 | 3.83 | 0 | 4dfb552a | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T20:09:41Z | gate-commit | 11.231 | 19.572 | 5.401 | 2.22 | 0 | 4dfb552a | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T20:10:11Z | gate-commit | 16.616 | 22.741 | 5.275 | 1.69 | 0 | 4dfb552a | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T20:11:26Z | rebaseline | 68.268 | 239.842 | 12.124 | 3.69 | 0 | 4dfb552a | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T20:12:24Z | gate-commit | 52.226 | 233.927 | 27.812 | 5.01 | 0 | 4dfb552a | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T20:20:48Z | gate-commit | 35.366 | 54.931 | 12.244 | 1.90 | 0 | 4dfb552a | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T20:42:24Z | rebaseline | 70.105 | 245.672 | 12.594 | 3.68 | 0 | b8abc2b2 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T20:43:41Z | rebaseline | 65.421 | 241.889 | 12.520 | 3.89 | 0 | b8abc2b2 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T20:44:54Z | rebaseline | 57.857 | 239.311 | 12.182 | 4.35 | 0 | b8abc2b2 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T20:46:25Z | rebaseline | 67.947 | 238.833 | 11.234 | 3.68 | 0 | b8abc2b2 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T20:47:50Z | rebaseline | 77.291 | 241.810 | 12.037 | 3.28 | 0 | b8abc2b2 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T20:48:53Z | rebaseline | 55.853 | 235.435 | 12.055 | 4.43 | 0 | b8abc2b2 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T20:54:18Z | gate-commit | 39.816 | 77.243 | 33.507 | 2.78 | 0 | b8abc2b2 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T20:57:19Z | gate-commit | 23.670 | 50.230 | 11.989 | 2.63 | 0 | b8abc2b2 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-21T22:47:23Z | gate-commit | 40.202 | 87.498 | 39.020 | 3.15 | 0 | 6bbf659f | campaign/the-stope | MacBookPro | 10 |
| 2026-08-20T18:41:38Z | sluice:artifacts | 167.541 | 1531.517 | 81.490 | 9.63 | 0 | 5ed9b5f21 |  | lefford | 40 |
| 2026-08-20T18:42:31Z | sluice:outboard | 52.534 | 65.720 | 40.089 | 2.01 | 0 | c71ebe594 |  | lefford | 40 |
| 2026-08-20T18:49:51Z | sluice:gate | 440.380 | 12056.664 | 473.227 | 28.45 | 0 | 1bf21b055 |  | lefford | 40 |
| 2026-08-20T20:29:59Z | sluice:artifacts | 59.753 | 536.218 | 21.017 | 9.33 | 0 | d0b1e90ce |  | lefford | 40 |
| 2026-08-20T20:30:35Z | sluice:outboard | 35.513 | 22.542 | 36.141 | 1.65 | 0 | c91fa7a80 |  | lefford | 40 |
| 2026-08-20T20:36:33Z | sluice:gate | 357.867 | 9875.942 | 340.653 | 28.55 | 0 | 626a6b7e7 |  | lefford | 40 |
| 2026-08-20T20:39:37Z | sluice:clients | 183.773 | 594.814 | 12.333 | 3.30 | 0 | 10738cc5d |  | lefford | 40 |
| 2026-08-20T20:57:12Z | sluice:artifacts | 115.925 | 1013.888 | 46.757 | 9.15 | 0 | 289b68b15 |  | lefford | 40 |
| 2026-08-20T20:57:48Z | sluice:outboard | 35.177 | 22.083 | 36.315 | 1.66 | 0 | 2c14fc515 |  | lefford | 40 |
| 2026-08-20T21:04:33Z | sluice:gate | 404.359 | 10898.466 | 426.439 | 28.01 | 0 | 926fc501f |  | lefford | 40 |
| 2026-08-20T21:08:58Z | sluice:clients | 265.015 | 692.969 | 32.690 | 2.74 | 0 | 0131ae0a7 |  | lefford | 40 |
| 2026-08-20T19:37:53Z | rebaseline | 57.383 | 240.044 | 12.736 | 4.41 | 0 | 3050f7da | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T19:39:23Z | game-check | 81.518 | 507.183 | 6.008 | 6.30 | 0 | 3050f7da | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T19:39:51Z | gate-commit | 23.732 | 50.224 | 12.017 | 2.62 | 0 | 3050f7da | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T19:55:31Z | rebaseline | 65.709 | 238.660 | 10.958 | 3.80 | 0 | dc2c5077 | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T19:57:58Z | game-check | 131.961 | 522.073 | 6.980 | 4.01 | 0 | dc2c5077 | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T19:58:25Z | gate-commit | 23.333 | 49.654 | 11.845 | 2.64 | 0 | dc2c5077 | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T20:00:10Z | gate-commit | 23.404 | 49.835 | 11.984 | 2.64 | 0 | dc2c5077 | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T20:24:00Z | game-check | 104.082 | 511.024 | 7.544 | 4.98 | 0 | 544ac78f | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T20:24:28Z | gate-commit | 25.002 | 49.734 | 11.719 | 2.46 | 0 | 544ac78f | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T20:25:56Z | rebaseline | 59.310 | 233.727 | 11.436 | 4.13 | 0 | 544ac78f | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T20:28:44Z | game-check | 114.850 | 512.453 | 6.005 | 4.51 | 0 | 544ac78f | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T20:29:24Z | gate-commit | 35.591 | 54.331 | 12.258 | 1.87 | 0 | 544ac78f | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T20:31:14Z | gate-commit | 25.071 | 50.648 | 12.291 | 2.51 | 0 | 544ac78f | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T20:36:26Z | gate-commit | 23.552 | 49.798 | 11.790 | 2.61 | 0 | 9c3633ca | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T20:40:53Z | gate-commit | 24.647 | 48.146 | 10.503 | 2.38 | 0 | 53ff339c | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T20:41:43Z | rebaseline | 50.202 | 232.836 | 10.144 | 4.84 | 0 | 53ff339c | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T20:42:44Z | gate-commit | 26.880 | 51.119 | 12.037 | 2.35 | 0 | 53ff339c | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T20:46:40Z | gate-commit | 39.000 | 53.510 | 12.222 | 1.69 | 0 | e1f25106 | campaign/the-stylus | MacBookPro | 10 |
| 2026-08-20T23:00:10Z | sluice:artifacts | 113.422 | 1017.110 | 43.552 | 9.35 | 0 | 1435dec14 |  | lefford | 40 |
| 2026-08-20T23:00:45Z | sluice:outboard | 34.717 | 20.643 | 30.545 | 1.47 | 0 | 1d354633b |  | lefford | 40 |
| 2026-08-20T23:07:29Z | sluice:gate | 403.174 | 10921.330 | 429.051 | 28.15 | 0 | 7dafec336 |  | lefford | 40 |
| 2026-08-20T23:12:03Z | sluice:clients | 273.822 | 1085.455 | 39.956 | 4.11 | 0 | 96539eb09 |  | lefford | 40 |
| 2026-08-21T16:17:21Z | prewarm | 154.337 | 1094.036 | 41.797 | 7.36 | 0 | d656064e | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T18:00:52Z | gate-commit | 30.907 | 83.762 | 17.138 | 3.26 | 0 | 63177e6c | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T18:09:57Z | gate-commit | 21.813 | 45.352 | 12.667 | 2.66 | 0 | 0a6ddbdc | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T18:16:40Z | gate-commit | 20.995 | 45.020 | 12.216 | 2.73 | 0 | f6de3ada | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T18:25:05Z | gate-commit | 20.894 | 44.777 | 11.997 | 2.72 | 0 | 6fdb3057 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T18:33:55Z | game-check | 79.155 | 495.873 | 4.881 | 6.33 | 0 | 126126b0 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T18:34:28Z | gate-commit | 21.365 | 45.134 | 12.297 | 2.69 | 0 | 126126b0 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T18:53:00Z | game-check | 0.232 | 0.139 | 0.047 | 0.80 | 0 | f1c8e875 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T18:54:26Z | game-check | 84.104 | 507.472 | 5.283 | 6.10 | 0 | f1c8e875 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T18:54:56Z | gate-commit | 21.385 | 45.134 | 12.372 | 2.69 | 0 | f1c8e875 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T19:04:22Z | game-check | 0.420 | 0.254 | 0.100 | 0.84 | 0 | 7103c0c8 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T19:04:29Z | game-check | 0.738 | 0.494 | 0.258 | 1.02 | 0 | 7103c0c8 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T19:04:33Z | game-check | 0.567 | 0.366 | 0.147 | 0.90 | 0 | 7103c0c8 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T19:04:55Z | game-check | 0.724 | 0.508 | 0.261 | 1.06 | 0 | 7103c0c8 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T19:06:20Z | game-check | 76.141 | 490.410 | 3.122 | 6.48 | 0 | 7103c0c8 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T19:08:32Z | game-check | 0.655 | 0.421 | 0.147 | 0.87 | 0 | 7103c0c8 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T19:09:56Z | game-check | 77.083 | 497.558 | 3.348 | 6.50 | 0 | 7103c0c8 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T19:10:16Z | game-check | 0.603 | 0.419 | 0.135 | 0.92 | 0 | 7103c0c8 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T19:15:47Z | game-check | 0.688 | 0.809 | 0.411 | 1.77 | 0 | 7103c0c8 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T19:16:02Z | game-check | 0.573 | 0.436 | 0.099 | 0.93 | 0 | 7103c0c8 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T19:16:24Z | game-check | 0.565 | 0.435 | 0.095 | 0.94 | 0 | 7103c0c8 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T19:16:52Z | game-check | 2.764 | 4.431 | 3.561 | 2.89 | 0 | 7103c0c8 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T19:16:56Z | game-check | 0.770 | 0.504 | 0.213 | 0.93 | 0 | 7103c0c8 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T19:18:51Z | game-check | 77.090 | 493.219 | 4.543 | 6.46 | 0 | 7103c0c8 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T19:20:19Z | game-check | 77.289 | 498.715 | 3.373 | 6.50 | 0 | 7103c0c8 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T19:20:58Z | gate-commit | 21.332 | 45.056 | 12.215 | 2.68 | 0 | 7103c0c8 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T19:22:57Z | game-check | 78.024 | 501.446 | 3.397 | 6.47 | 0 | 389d498d | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T19:27:18Z | game-check | 78.374 | 494.646 | 5.041 | 6.38 | 0 | 5e8a25d0 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T19:27:45Z | gate-commit | 21.388 | 45.125 | 12.339 | 2.69 | 0 | 5e8a25d0 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T19:44:08Z | game-check | 78.681 | 499.530 | 4.204 | 6.40 | 0 | 7c369aaa | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T19:44:34Z | gate-commit | 21.448 | 45.164 | 12.275 | 2.68 | 0 | 7c369aaa | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T20:09:09Z | game-check | 0.235 | 0.145 | 0.050 | 0.83 | 0 | 30b39498 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T20:09:13Z | game-check | 0.816 | 0.653 | 0.411 | 1.30 | 0 | 30b39498 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T20:10:55Z | game-check | 78.482 | 497.475 | 4.497 | 6.40 | 0 | 30b39498 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T20:11:29Z | gate-commit | 22.560 | 46.145 | 13.018 | 2.62 | 0 | 30b39498 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T20:53:38Z | gate-commit | 23.128 | 50.334 | 12.229 | 2.71 | 0 | 8795d53c | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T21:02:38Z | gate-commit | 22.486 | 47.540 | 10.429 | 2.58 | 0 | 4e33f410 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T21:03:15Z | gate-commit | 22.927 | 48.144 | 10.804 | 2.57 | 0 | 4e33f410 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T21:03:54Z | gate-commit | 23.241 | 49.128 | 11.310 | 2.60 | 0 | 4e33f410 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T21:05:14Z | gate-commit | 22.652 | 49.608 | 11.535 | 2.70 | 0 | 4e33f410 | campaign/the-chroma | MacBookPro | 10 |
| 2026-08-21T21:57:26Z | sluice:artifacts | 62.754 | 535.626 | 20.539 | 8.86 | 0 | 8f974ad3b |  | lefford | 40 |
| 2026-08-21T21:58:02Z | sluice:outboard | 35.264 | 21.978 | 34.767 | 1.61 | 0 | 47a864244 |  | lefford | 40 |
| 2026-08-21T22:03:53Z | sluice:gate | 350.961 | 9834.190 | 334.572 | 28.97 | 0 | f5970ba20 |  | lefford | 40 |
| 2026-08-21T22:06:59Z | sluice:clients | 185.993 | 971.943 | 25.816 | 5.36 | 0 | fe6880381 |  | lefford | 40 |
| 2026-08-21T22:54:32Z | rebaseline | 71.941 | 238.575 | 13.668 | 3.51 | 0 | cc3dc3dc | campaign/the-stope | MacBookPro | 10 |
| 2026-08-21T22:55:10Z | gate-commit | 31.978 | 68.078 | 14.309 | 2.58 | 0 | cc3dc3dc | campaign/the-stope | MacBookPro | 10 |
| 2026-08-21T22:55:45Z | gate-commit | 34.203 | 53.983 | 12.076 | 1.93 | 0 | ae87d777 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T01:34:27Z | gate-commit | 13.270 | 27.188 | 6.903 | 2.57 | 0 | ae87d777 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T01:35:32Z | gate-commit | 19.452 | 23.425 | 7.457 | 1.59 | 0 | ae87d777 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T01:35:47Z | gate-commit | 11.414 | 11.093 | 0.227 | 0.99 | 0 | ae87d777 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T01:36:44Z | gate-commit | 44.184 | 182.333 | 26.012 | 4.72 | 0 | ae87d777 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T01:37:24Z | gate-commit | 22.249 | 46.974 | 9.820 | 2.55 | 0 | ae87d777 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T01:38:05Z | gate-commit | 22.193 | 46.950 | 9.874 | 2.56 | 0 | ae87d777 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T01:39:38Z | gate-commit | 23.586 | 49.807 | 11.486 | 2.60 | 0 | ae87d777 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T01:40:09Z | gate-commit | 22.722 | 48.422 | 10.748 | 2.60 | 0 | ae87d777 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T01:41:25Z | gate-commit | 30.798 | 78.295 | 13.123 | 2.97 | 0 | ae87d777 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T01:42:29Z | gate-commit | 23.132 | 50.485 | 11.894 | 2.70 | 0 | ae87d777 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T01:52:47Z | gate-commit | 32.444 | 66.928 | 35.912 | 3.17 | 0 | d10755c3 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T01:54:26Z | gate-commit | 32.563 | 68.210 | 37.295 | 3.24 | 0 | a62e12fe | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T13:10:24Z | gate-commit | 44.375 | 116.529 | 46.377 | 3.67 | 0 | eb630f0a0 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T13:11:42Z | gate-commit | 28.745 | 79.897 | 13.425 | 3.25 | 0 | eb630f0a0 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T13:20:12Z | gate-commit | 34.408 | 70.903 | 35.908 | 3.10 | 0 | eb630f0a0 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T13:34:10Z | gate-commit | 33.186 | 71.097 | 42.156 | 3.41 | 0 | 87cbb9bf8 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T13:34:52Z | gate-commit | 21.190 | 45.221 | 12.415 | 2.72 | 0 | 87cbb9bf8 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T14:56:58Z | gate-commit | 16.880 | 24.645 | 15.464 | 2.38 | 0 | 1ec9a6532 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T14:57:53Z | gate-commit | 24.346 | 48.871 | 16.415 | 2.68 | 0 | 1ec9a6532 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T15:07:49Z | gate-commit | 55.467 | 59.132 | 19.951 | 1.43 | 0 | 26c822e43 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T15:42:09Z | gate-commit | 15.335 | 31.738 | 17.148 | 3.19 | 0 | 464e87c78 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T15:42:27Z | gate-commit | 6.969 | 6.657 | 0.222 | 0.99 | 0 | 464e87c78 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T15:44:26Z | gate-commit | 39.466 | 86.334 | 38.887 | 3.17 | 0 | 464e87c78 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T15:45:34Z | gate-commit | 23.297 | 48.942 | 11.431 | 2.59 | 0 | 464e87c78 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T15:47:24Z | gate-commit | 40.145 | 103.428 | 39.313 | 3.56 | 0 | 464e87c78 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T15:55:26Z | gate-commit | 42.015 | 84.682 | 42.622 | 3.03 | 0 | dc84e62b5 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T16:26:24Z | gate-commit | 100.618 | 486.236 | 41.741 | 5.25 | 0 | 5a4632ce5 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T21:20:48Z | rebaseline | 86.680 | 230.634 | 9.955 | 2.78 | 0 | 814f82305 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T21:34:27Z | rebaseline | 61.396 | 238.352 | 11.586 | 4.07 | 0 | 814f82305 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T21:40:23Z | gate-commit | 71.251 | 224.763 | 44.585 | 3.78 | 0 | 814f82305 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T21:41:32Z | gate-commit | 23.631 | 51.061 | 11.914 | 2.66 | 0 | 814f82305 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-21T22:32:35Z | prewarm | 57.285 | 366.317 | 15.137 | 6.66 | 0 | a00c1abc | campaign/the-wick | MacBookPro | 10 |
| 2026-08-21T22:58:50Z | gate-commit | 6.065 | 5.773 | 0.194 | 0.98 | 0 | 7c86f299 | campaign/the-wick | MacBookPro | 10 |
| 2026-08-21T22:59:20Z | gate-commit | 7.598 | 9.893 | 0.911 | 1.42 | 0 | 7c86f299 | campaign/the-wick | MacBookPro | 10 |
| 2026-08-21T22:59:45Z | gate-commit | 11.774 | 14.004 | 0.954 | 1.27 | 0 | 7c86f299 | campaign/the-wick | MacBookPro | 10 |
| 2026-08-21T23:00:27Z | gate-commit | 30.224 | 65.327 | 17.440 | 2.74 | 0 | 7c86f299 | campaign/the-wick | MacBookPro | 10 |
| 2026-08-21T23:05:07Z | gate-commit | 22.980 | 49.844 | 11.683 | 2.68 | 0 | 7c86f299 | campaign/the-wick | MacBookPro | 10 |
| 2026-08-21T23:05:44Z | gate-commit | 23.260 | 49.951 | 11.832 | 2.66 | 0 | 7c86f299 | campaign/the-wick | MacBookPro | 10 |
| 2026-08-22T00:08:12Z | gate-commit | 2.204 | 2.012 | 0.089 | 0.95 | 0 | b1af9a39 | campaign/the-wick | MacBookPro | 10 |
| 2026-08-22T00:08:26Z | gate-commit | 2.113 | 2.010 | 0.078 | 0.99 | 0 | b1af9a39 | campaign/the-wick | MacBookPro | 10 |
| 2026-08-22T00:08:41Z | gate-commit | 2.120 | 2.008 | 0.090 | 0.99 | 0 | b1af9a39 | campaign/the-wick | MacBookPro | 10 |
| 2026-08-22T00:08:57Z | gate-commit | 3.809 | 5.204 | 0.547 | 1.51 | 0 | b1af9a39 | campaign/the-wick | MacBookPro | 10 |
| 2026-08-22T00:09:13Z | gate-commit | 2.917 | 3.887 | 0.504 | 1.51 | 0 | b1af9a39 | campaign/the-wick | MacBookPro | 10 |
| 2026-08-22T00:09:53Z | gate-commit | 8.199 | 21.006 | 4.454 | 3.11 | 0 | b1af9a39 | campaign/the-wick | MacBookPro | 10 |
| 2026-08-22T00:11:09Z | gate-commit | 17.136 | 30.132 | 4.600 | 2.03 | 0 | b1af9a39 | campaign/the-wick | MacBookPro | 10 |
| 2026-08-22T00:11:33Z | gate-commit | 10.995 | 10.703 | 0.220 | 0.99 | 0 | b1af9a39 | campaign/the-wick | MacBookPro | 10 |
| 2026-08-22T00:11:52Z | gate-commit | 10.987 | 10.700 | 0.216 | 0.99 | 0 | b1af9a39 | campaign/the-wick | MacBookPro | 10 |
| 2026-08-22T00:12:36Z | gate-commit | 31.548 | 71.597 | 17.055 | 2.81 | 0 | b1af9a39 | campaign/the-wick | MacBookPro | 10 |
| 2026-08-22T00:13:11Z | gate-commit | 22.759 | 50.082 | 11.783 | 2.72 | 0 | b1af9a39 | campaign/the-wick | MacBookPro | 10 |
| 2026-08-22T00:18:07Z | game-check | 90.283 | 546.494 | 11.573 | 6.18 | 0 | 04a0db70 | campaign/the-wick | MacBookPro | 10 |
| 2026-08-22T00:18:30Z | gate-commit | 22.498 | 49.934 | 11.533 | 2.73 | 0 | 04a0db70 | campaign/the-wick | MacBookPro | 10 |
| 2026-08-22T12:58:49Z | sluice:artifacts | 93.777 | 659.168 | 28.232 | 7.33 | 0 | 439177f0e |  | lefford | 40 |
| 2026-08-22T12:59:25Z | sluice:outboard | 35.470 | 20.698 | 31.079 | 1.46 | 0 | 65af6ce01 |  | lefford | 40 |
| 2026-08-22T13:05:27Z | sluice:gate | 362.567 | 9879.595 | 364.927 | 28.26 | 0 | 07b1458a4 |  | lefford | 40 |
| 2026-08-22T13:09:07Z | sluice:clients | 218.967 | 1009.561 | 29.909 | 4.75 | 0 | 4160a2e51 |  | lefford | 40 |
| 2026-08-22T21:35:58Z | rebaseline | 87.243 | 242.129 | 9.957 | 2.89 | 0 | f0013304f | campaign/the-deed | MacBookPro | 10 |
| 2026-08-22T21:38:02Z | gate-commit | 88.036 | 133.118 | 20.556 | 1.75 | 0 | f0013304f | campaign/the-deed | MacBookPro | 10 |
| 2026-08-22T22:15:46Z | rebaseline | 91.469 | 237.660 | 10.208 | 2.71 | 0 | 95d7cd598 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-22T22:20:15Z | gate-commit | 36.264 | 90.626 | 15.205 | 2.92 | 0 | 95d7cd598 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-22T22:23:33Z | gate-commit | 28.324 | 59.377 | 16.923 | 2.69 | 0 | 95d7cd598 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-22T22:24:20Z | gate-commit | 22.995 | 50.835 | 12.027 | 2.73 | 0 | 95d7cd598 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-22T23:28:26Z | rebaseline | 48.996 | 231.733 | 11.338 | 4.96 | 0 | 9ecabd731 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-22T23:54:58Z | rebaseline | 58.969 | 236.522 | 12.641 | 4.23 | 0 | 0dd111c2b | campaign/the-deed | MacBookPro | 10 |
| 2026-08-23T00:07:26Z | gate-commit | 32.736 | 89.151 | 24.067 | 3.46 | 0 | 0dd111c2b | campaign/the-deed | MacBookPro | 10 |
| 2026-08-23T00:08:14Z | gate-commit | 25.596 | 51.252 | 12.440 | 2.49 | 0 | 0dd111c2b | campaign/the-deed | MacBookPro | 10 |
| 2026-08-23T00:10:28Z | game-check | 89.063 | 525.544 | 9.853 | 6.01 | 0 | 823892de4 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-23T01:10:31Z | gate-commit | 38.073 | 88.795 | 39.265 | 3.36 | 0 | cd3a2da6d | campaign/the-deed | MacBookPro | 10 |
| 2026-08-22T21:45:18Z | rebaseline | 56.674 | 237.740 | 11.781 | 4.40 | 0 | 67d5a60cb | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T21:46:34Z | gate-commit | 35.176 | 108.029 | 20.781 | 3.66 | 0 | 67d5a60cb | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T22:04:15Z | gate-commit | 44.404 | 122.344 | 37.559 | 3.60 | 0 | 438efdea4 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T22:05:01Z | gate-commit | 26.022 | 53.874 | 12.423 | 2.55 | 0 | 438efdea4 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T22:06:04Z | gate-commit | 22.845 | 50.600 | 11.799 | 2.73 | 0 | 438efdea4 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T22:10:52Z | rebaseline | 94.923 | 241.083 | 12.620 | 2.67 | 0 | 57f5ca137 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T22:12:01Z | rebaseline | 59.393 | 238.363 | 12.455 | 4.22 | 0 | 57f5ca137 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T22:13:16Z | rebaseline | 67.137 | 238.108 | 11.441 | 3.72 | 0 | 57f5ca137 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T22:17:31Z | gate-commit | 53.247 | 95.700 | 33.796 | 2.43 | 0 | 57f5ca137 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T22:58:20Z | gate-commit | 41.693 | 117.219 | 42.524 | 3.83 | 0 | bfd2c4645 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T22:59:29Z | gate-commit | 22.889 | 50.570 | 11.800 | 2.72 | 0 | bfd2c4645 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T23:36:44Z | rebaseline | 52.244 | 237.829 | 12.041 | 4.78 | 0 | ab00d1cf4 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T23:38:19Z | gate-commit | 22.952 | 50.516 | 11.666 | 2.71 | 0 | ab00d1cf4 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T23:40:18Z | gate-commit | 22.583 | 50.535 | 11.496 | 2.75 | 0 | 47002938f | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T15:09:36Z | prewarm | 175.634 | 1116.563 | 41.260 | 6.59 | 0 | 5592b8ec5 | campaign/the-stride | MacBookPro | 10 |
| 2026-08-22T15:28:24Z | gate-commit | 31.658 | 85.328 | 17.306 | 3.24 | 0 | e49c95e08 | campaign/the-stride | MacBookPro | 10 |
| 2026-08-22T15:30:50Z | gate-commit | 21.742 | 45.660 | 12.761 | 2.69 | 0 | 7d2b5b6f5 | campaign/the-stride | MacBookPro | 10 |
| 2026-08-22T21:31:24Z | gate-commit | 27.833 | 52.330 | 12.775 | 2.34 | 0 | 7f4aca161 | campaign/the-stride | MacBookPro | 10 |
| 2026-08-22T21:41:53Z | game-check | 202.638 | 730.876 | 8.353 | 3.65 | 0 | 7c52b214d | campaign/the-stride | MacBookPro | 10 |
| 2026-08-22T22:19:48Z | gate-commit | 27.393 | 52.585 | 12.163 | 2.36 | 0 | 2bc94dfe8 | campaign/the-stride | MacBookPro | 10 |
| 2026-08-22T23:28:55Z | sluice:artifacts | 117.275 | 1033.582 | 46.558 | 9.21 | 0 | 0b71d4808 |  | lefford | 40 |
| 2026-08-22T23:29:30Z | sluice:outboard | 35.196 | 22.136 | 36.002 | 1.65 | 0 | c6923f7f4 |  | lefford | 40 |
| 2026-08-22T23:36:15Z | sluice:gate | 404.440 | 10913.750 | 433.236 | 28.06 | 0 | 10cb686cd |  | lefford | 40 |
| 2026-08-22T23:43:12Z | sluice:clients | 417.373 | 1479.229 | 42.389 | 3.65 | 0 | 1fddcc552 |  | lefford | 40 |
| 2026-08-22T23:47:18Z | gate-commit | 22.976 | 49.249 | 11.142 | 2.63 | 0 | c0d94fbcb | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T23:47:50Z | gate-commit | 22.870 | 49.275 | 10.839 | 2.63 | 0 | c0d94fbcb | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T23:48:48Z | gate-commit | 22.942 | 51.033 | 12.084 | 2.75 | 0 | c0d94fbcb | campaign/the-stope | MacBookPro | 10 |
| 2026-08-22T23:49:59Z | rebaseline | 63.395 | 239.567 | 10.442 | 3.94 | 0 | b0da2f100 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-23T00:01:01Z | gate-commit | 35.333 | 55.939 | 12.487 | 1.94 | 0 | 220bd35e7 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-23T00:05:01Z | sluice:artifacts | 127.602 | 1175.419 | 59.004 | 9.67 | 0 | e5062478e |  | lefford | 40 |
| 2026-08-23T00:05:38Z | sluice:outboard | 36.796 | 24.365 | 36.952 | 1.67 | 0 | ff4fdb731 |  | lefford | 40 |
| 2026-08-23T00:12:40Z | sluice:gate | 421.831 | 11352.786 | 466.409 | 28.02 | 0 | e1ba86585 |  | lefford | 40 |
| 2026-08-23T00:20:00Z | sluice:clients | 439.140 | 1568.117 | 55.405 | 3.70 | 0 | 787d58261 |  | lefford | 40 |
| 2026-08-23T00:03:19Z | gate-commit | 26.325 | 52.756 | 12.035 | 2.46 | 0 | 22ddabbe0 | campaign/the-stope | MacBookPro | 10 |
| 2026-08-23T00:04:39Z | rebaseline | 50.388 | 237.431 | 10.783 | 4.93 | 0 | e7ebd9a6b | campaign/the-stope | MacBookPro | 10 |
| 2026-08-23T00:22:49Z | sluice:artifacts | 63.356 | 551.152 | 22.514 | 9.05 | 0 | 0fb0ed542 |  | lefford | 40 |
| 2026-08-23T00:23:24Z | sluice:outboard | 35.438 | 22.311 | 36.456 | 1.66 | 0 | ca1fed0ee |  | lefford | 40 |
| 2026-08-23T00:29:18Z | sluice:gate | 353.502 | 9900.150 | 340.750 | 28.97 | 0 | 7f391d318 |  | lefford | 40 |
| 2026-08-23T01:11:00Z | gate-commit | 135.944 | 2441.801 | 207.000 | 19.48 | 0 | 77f2551d5 | fix/sluice-mouth-before-box | lefford | 40 |
| 2026-08-23T01:26:59Z | sluice:artifacts | 120.906 | 1056.905 | 49.047 | 9.15 | 0 | 3638f50f0 |  | lefford | 40 |
| 2026-08-23T01:27:36Z | sluice:outboard | 36.724 | 22.929 | 37.083 | 1.63 | 0 | abc9594e3 |  | lefford | 40 |
| 2026-08-23T01:34:25Z | sluice:gate | 408.941 | 11018.493 | 443.934 | 28.03 | 0 | d91cda826 |  | lefford | 40 |
| 2026-08-23T01:41:31Z | sluice:clients | 425.411 | 1474.550 | 40.641 | 3.56 | 0 | 408ed813c |  | lefford | 40 |
| 2026-08-22T14:57:16Z | gate-commit | 53.455 | 87.693 | 37.481 | 2.34 | 0 | cca1560e2 | campaign/the-penstock | ambrose | 12 |
| 2026-08-22T16:46:41Z | gate-commit | 529.304 | 1440.309 | 151.255 | 3.01 | 0 | cca1560e2 | campaign/the-penstock | ambrose | 12 |
| 2026-08-22T18:36:30Z | gate-commit | 70.935 | 34.895 | 12.623 | 0.67 | 0 | 716506630 | campaign/the-penstock | ambrose | 12 |
| 2026-08-22T19:10:48Z | gate-commit | 149.907 | 145.010 | 52.045 | 1.31 | 0 | dae4be7e1 | campaign/the-penstock | ambrose | 12 |
| 2026-08-22T19:13:28Z | gate-commit | 92.738 | 60.633 | 35.693 | 1.04 | 0 | dae4be7e1 | campaign/the-penstock | ambrose | 12 |
| 2026-08-22T19:22:34Z | gate-commit | 6.601 | 2.362 | 0.988 | 0.51 | 0 | 6c52411d0 | campaign/the-penstock | ambrose | 12 |
| 2026-08-22T19:24:49Z | gate-commit | 93.643 | 61.076 | 35.205 | 1.03 | 0 | 6c52411d0 | campaign/the-penstock | ambrose | 12 |
| 2026-08-22T19:26:54Z | gate-commit | 96.982 | 60.209 | 34.467 | 0.98 | 0 | 6c52411d0 | campaign/the-penstock | ambrose | 12 |
| 2026-08-22T19:33:52Z | gate-commit | 95.860 | 61.003 | 35.446 | 1.01 | 0 | 917c13e45 | campaign/the-penstock | ambrose | 12 |
| 2026-08-22T19:35:46Z | gate-commit | 90.502 | 59.674 | 34.664 | 1.04 | 0 | 917c13e45 | campaign/the-penstock | ambrose | 12 |
| 2026-08-22T19:50:13Z | gate-commit | 107.851 | 65.203 | 39.178 | 0.97 | 0 | 482b326c2 | campaign/the-penstock | ambrose | 12 |
| 2026-08-22T19:50:47Z | gate-commit | 23.993 | 14.405 | 3.543 | 0.75 | 0 | 482b326c2 | campaign/the-penstock | ambrose | 12 |
| 2026-08-22T19:52:45Z | gate-commit | 98.718 | 64.611 | 40.077 | 1.06 | 0 | 482b326c2 | campaign/the-penstock | ambrose | 12 |
| 2026-08-22T20:29:26Z | gate-commit | 30.882 | 22.434 | 7.267 | 0.96 | 0 | 2bf2eed82 | campaign/the-penstock | ambrose | 12 |
| 2026-08-22T20:32:25Z | gate-commit | 152.779 | 183.873 | 54.435 | 1.56 | 0 | 2bf2eed82 | campaign/the-penstock | ambrose | 12 |
| 2026-08-22T20:35:38Z | gate-commit | 100.779 | 65.593 | 39.919 | 1.05 | 0 | 2bf2eed82 | campaign/the-penstock | ambrose | 12 |
| 2026-08-22T21:03:49Z | gate-commit | 143.480 | 70.227 | 42.243 | 0.78 | 0 | 49452b027 | campaign/the-penstock | ambrose | 12 |
| 2026-08-22T21:06:21Z | gate-commit | 120.607 | 70.897 | 42.403 | 0.94 | 0 | 49452b027 | campaign/the-penstock | ambrose | 12 |
| 2026-08-22T21:23:41Z | gate-commit | 105.888 | 67.369 | 39.706 | 1.01 | 0 | abccf8146 | campaign/the-penstock | ambrose | 12 |
| 2026-08-22T21:48:17Z | gate-commit | 113.111 | 72.889 | 42.088 | 1.02 | 0 | abccf8146 | campaign/the-penstock | ambrose | 12 |
| 2026-08-22T21:49:56Z | gate-commit | 91.269 | 65.344 | 36.917 | 1.12 | 0 | abccf8146 | campaign/the-penstock | ambrose | 12 |
| 2026-08-22T21:52:50Z | gate-commit | 92.863 | 65.154 | 37.787 | 1.11 | 0 | abccf8146 | campaign/the-penstock | ambrose | 12 |
| 2026-08-22T23:15:25Z | gate-commit | 105.325 | 67.058 | 38.915 | 1.01 | 0 | 307e85bf7 | campaign/the-penstock | ambrose | 12 |
| 2026-08-23T01:25:45Z | gate-commit | 98.027 | 66.183 | 38.848 | 1.07 | 0 | 7eb8fa633 | campaign/the-penstock | ambrose | 12 |
| 2026-08-23T01:28:58Z | gate-commit | 94.056 | 66.140 | 38.817 | 1.12 | 0 | f5e72719d | campaign/the-penstock | ambrose | 12 |
| 2026-08-23T12:17:44Z | gate-commit | 117.486 | 66.574 | 39.951 | 0.91 | 0 | 562979f85 | campaign/the-penstock | ambrose | 12 |
| 2026-08-23T01:44:13Z | sluice:artifacts | 117.904 | 1081.744 | 52.429 | 9.62 | 0 | 132854255 |  | lefford | 40 |
| 2026-08-23T01:44:52Z | sluice:outboard | 38.234 | 25.159 | 38.174 | 1.66 | 0 | 63d276d35 |  | lefford | 40 |
| 2026-08-23T01:51:35Z | sluice:gate | 402.308 | 10964.708 | 447.335 | 28.37 | 0 | 003b5e200 |  | lefford | 40 |
| 2026-08-23T01:58:47Z | sluice:clients | 432.081 | 1566.152 | 49.540 | 3.74 | 0 | 4b283b384 |  | lefford | 40 |
| 2026-08-23T01:12:26Z | rebaseline | 74.647 | 238.501 | 12.001 | 3.36 | 0 | ffdd3760c | campaign/the-deed | MacBookPro | 10 |
| 2026-08-23T01:14:31Z | gate-commit | 86.699 | 473.239 | 43.760 | 5.96 | 0 | ffdd3760c | campaign/the-deed | MacBookPro | 10 |
| 2026-08-23T01:53:19Z | rebaseline | 38.420 | 232.024 | 11.476 | 6.34 | 0 | 5325c79ad | campaign/the-deed | MacBookPro | 10 |
| 2026-08-23T02:17:06Z | vessel-check | 43.144 | 68.752 | 4.262 | 1.69 | 0 | 63cfabaa5 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-23T02:18:15Z | vessel-check | 29.140 | 29.655 | 0.859 | 1.05 | 0 | 63cfabaa5 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-23T02:21:21Z | vessel-check | 14.767 | 15.707 | 0.580 | 1.10 | 0 | 63cfabaa5 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-23T02:45:16Z | gate-commit | 36.539 | 63.878 | 21.368 | 2.33 | 0 | 88de455bc | campaign/the-deed | MacBookPro | 10 |
| 2026-08-23T03:05:02Z | gate-commit | 23.390 | 51.335 | 11.410 | 2.68 | 0 | 768b74fd9 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-23T02:49:18Z | sluice:artifacts | 94.941 | 752.210 | 34.847 | 8.29 | 0 | 8ab4d38bf |  | lefford | 40 |
| 2026-08-23T02:49:55Z | sluice:outboard | 37.454 | 23.240 | 38.057 | 1.64 | 0 | 90b3bccf3 |  | lefford | 40 |
| 2026-08-23T02:56:14Z | sluice:gate | 378.924 | 10388.836 | 412.945 | 28.51 | 0 | 9a0cad4bf |  | lefford | 40 |
| 2026-08-23T03:03:26Z | sluice:clients | 431.258 | 1500.872 | 39.941 | 3.57 | 0 | 32c1ea555 |  | lefford | 40 |
| 2026-08-23T03:07:27Z | gate-commit | 90.119 | 497.484 | 62.019 | 6.21 | 0 | 162db1bbe | campaign/the-deed | MacBookPro | 10 |
| 2026-08-23T11:47:02Z | sluice:artifacts | 62.369 | 551.361 | 22.706 | 9.20 | 0 | e16563b61 |  | lefford | 40 |
| 2026-08-23T11:47:42Z | sluice:outboard | 39.047 | 23.975 | 37.796 | 1.58 | 0 | eb2e74a20 |  | lefford | 40 |
| 2026-08-23T11:54:11Z | sluice:gate | 388.600 | 10347.378 | 367.805 | 27.57 | 0 | 7fa1c574d |  | lefford | 40 |
| 2026-08-23T11:56:16Z | gate-commit | 19.706 | 47.793 | 12.243 | 3.05 | 0 | c01c87ba8 | campaign/the-deed | MacBookPro | 10 |
| 2026-08-23T11:58:27Z | sluice:artifacts | 61.855 | 550.884 | 22.270 | 9.27 | 0 | c3d4cd8c0 |  | lefford | 40 |
| 2026-08-23T11:59:05Z | sluice:outboard | 37.294 | 23.041 | 35.257 | 1.56 | 0 | a04076489 |  | lefford | 40 |
| 2026-08-23T12:05:10Z | sluice:gate | 365.535 | 10289.413 | 366.849 | 29.15 | 0 | 97f69beea |  | lefford | 40 |
| 2026-08-23T12:06:49Z | gate-commit | 18.966 | 46.889 | 11.755 | 3.09 | 0 | e205013cc | campaign/the-deed | MacBookPro | 10 |
| 2026-08-23T12:09:01Z | sluice:artifacts | 65.021 | 546.900 | 21.807 | 8.75 | 0 | 23d2fac9b |  | lefford | 40 |
| 2026-08-23T12:09:40Z | sluice:outboard | 38.016 | 23.872 | 37.451 | 1.61 | 0 | 49a335a17 |  | lefford | 40 |
| 2026-08-23T12:15:43Z | sluice:gate | 362.553 | 10261.867 | 351.241 | 29.27 | 0 | 26d657070 |  | lefford | 40 |
| 2026-08-23T13:04:30Z | sluice:artifacts | 62.243 | 545.784 | 20.300 | 9.09 | 0 | 21587718e |  | lefford | 40 |
| 2026-08-23T13:05:08Z | sluice:outboard | 37.559 | 23.039 | 37.436 | 1.61 | 0 | 5fe00f61e |  | lefford | 40 |
| 2026-08-23T13:11:16Z | sluice:gate | 368.495 | 10297.802 | 369.542 | 28.95 | 0 | 100fe5b59 |  | lefford | 40 |
| 2026-08-23T13:16:48Z | sluice:clients | 330.842 | 1350.915 | 21.783 | 4.15 | 0 | 97b93d38e |  | lefford | 40 |
| 2026-08-23T16:44:42Z | gate-commit | 454.147 | 1124.203 | 165.274 | 2.84 | 0 | 49bf2427c | campaign/the-scour | ambrose | 12 |
| 2026-08-23T16:48:06Z | gate-commit | 110.271 | 68.196 | 40.168 | 0.98 | 0 | 49bf2427c | campaign/the-scour | ambrose | 12 |
| 2026-08-23T15:36:29Z | gate-commit | 95.133 | 1493.082 | 162.461 | 17.40 | 0 | f7e79abc2 | fix/clients-phase-and-golden-criterion | lefford | 40 |
| 2026-08-23T15:55:23Z | sluice:artifacts | 98.073 | 743.440 | 36.069 | 7.95 | 0 | 1a0b09f76 |  | lefford | 40 |
| 2026-08-23T15:56:01Z | sluice:outboard | 37.430 | 23.347 | 37.483 | 1.63 | 0 | c023c69db |  | lefford | 40 |
| 2026-08-23T16:02:31Z | sluice:gate | 389.947 | 10475.625 | 383.527 | 27.85 | 0 | b099a5d3a |  | lefford | 40 |
| 2026-08-23T16:06:40Z | sluice:clients | 248.582 | 1495.982 | 22.670 | 6.11 | 0 | 9e68c5cb2 |  | lefford | 40 |
| 2026-08-23T15:15:54Z | gate-commit | 318.742 | 1045.280 | 77.760 | 3.52 | 0 | 49bf2427c | campaign/the-scour | MacBookPro | 10 |
| 2026-08-23T15:17:32Z | gate-commit | 20.135 | 47.847 | 12.079 | 2.98 | 0 | 49bf2427c | campaign/the-scour | MacBookPro | 10 |
| 2026-08-23T16:13:31Z | sluice:artifacts | 96.447 | 721.497 | 33.389 | 7.83 | 0 | 31c567059 |  | lefford | 40 |
| 2026-08-23T16:14:10Z | sluice:outboard | 38.296 | 23.967 | 37.225 | 1.60 | 0 | ef581db12 |  | lefford | 40 |
| 2026-08-23T16:20:38Z | sluice:gate | 388.272 | 10413.201 | 375.239 | 27.79 | 0 | 3948ec56f |  | lefford | 40 |
| 2026-08-23T16:25:04Z | sluice:clients | 265.792 | 1502.331 | 25.875 | 5.75 | 0 | a47a4d200 |  | lefford | 40 |
| 2026-08-23T15:55:25Z | gate-commit | 184.570 | 772.272 | 44.631 | 4.43 | 0 | 49bf2427c | campaign/the-mirror | MacBookPro | 10 |
| 2026-08-23T15:56:29Z | gate-commit | 21.998 | 48.206 | 12.571 | 2.76 | 0 | 49bf2427c | campaign/the-mirror | MacBookPro | 10 |
| 2026-08-23T15:57:20Z | gate-commit | 42.780 | 53.073 | 13.581 | 1.56 | 0 | 49bf2427c | campaign/the-mirror | MacBookPro | 10 |
| 2026-08-23T17:14:14Z | sluice:artifacts | 93.133 | 740.846 | 32.931 | 8.31 | 0 | 07dafd50a |  | lefford | 40 |
| 2026-08-23T17:14:52Z | sluice:outboard | 37.808 | 23.386 | 37.001 | 1.60 | 0 | 521555875 |  | lefford | 40 |
| 2026-08-23T17:21:08Z | sluice:gate | 375.552 | 10354.803 | 407.745 | 28.66 | 0 | ea66b1369 |  | lefford | 40 |
| 2026-08-23T17:25:14Z | sluice:clients | 246.012 | 1513.767 | 28.014 | 6.27 | 0 | 8ec1ba13d |  | lefford | 40 |
| 2026-08-23T17:26:54Z | sluice:artifacts | 92.334 | 731.423 | 32.813 | 8.28 | 0 | b38509391 |  | lefford | 40 |
| 2026-08-23T17:27:34Z | sluice:outboard | 38.869 | 23.817 | 36.004 | 1.54 | 0 | 98b4a630c |  | lefford | 40 |
| 2026-08-23T17:34:03Z | sluice:gate | 389.386 | 10439.209 | 390.978 | 27.81 | 0 | 6f6fc210d |  | lefford | 40 |
| 2026-08-23T17:38:06Z | sluice:clients | 242.312 | 1463.108 | 23.704 | 6.14 | 0 | e18aea73c |  | lefford | 40 |
| 2026-08-23T17:39:14Z | sluice:artifacts | 61.786 | 541.234 | 20.664 | 9.09 | 0 | 539124d5f |  | lefford | 40 |
| 2026-08-23T17:39:53Z | sluice:outboard | 38.199 | 23.637 | 37.067 | 1.59 | 0 | aa2817100 |  | lefford | 40 |
| 2026-08-23T17:45:57Z | sluice:gate | 364.686 | 10246.247 | 350.239 | 29.06 | 0 | 67f23ca91 |  | lefford | 40 |
| 2026-08-23T17:50:10Z | sluice:clients | 251.925 | 1441.846 | 17.245 | 5.79 | 0 | 7775d62e7 |  | lefford | 40 |
| 2026-08-23T19:21:40Z | gate-commit | 239.083 | 245.542 | 101.700 | 1.45 | 0 | 27312e02c | campaign/the-leat | ambrose | 12 |
| 2026-08-23T19:24:37Z | gate-commit | 155.618 | 69.608 | 40.331 | 0.71 | 0 | 27312e02c | campaign/the-leat | ambrose | 12 |
| 2026-08-23T21:10:01Z | sluice:artifacts | 61.592 | 543.066 | 24.696 | 9.22 | 0 | bd8938743 |  | lefford | 40 |
| 2026-08-23T21:10:39Z | sluice:outboard | 37.822 | 23.592 | 37.574 | 1.62 | 0 | f0b1942d1 |  | lefford | 40 |
| 2026-08-23T21:16:51Z | sluice:gate | 371.783 | 10287.837 | 355.234 | 28.63 | 0 | ac174ab14 |  | lefford | 40 |
| 2026-08-23T21:20:59Z | sluice:clients | 247.381 | 1442.997 | 17.686 | 5.90 | 0 | dc7c22373 |  | lefford | 40 |
| 2026-08-23T13:04:59Z | prewarm | 115.703 | 770.465 | 43.646 | 7.04 | 0 | 8d88cc005 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-23T13:19:50Z | gate-commit | 38.419 | 83.512 | 44.031 | 3.32 | 0 | 8d88cc005 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-23T13:56:10Z | gate-commit | 18.917 | 46.179 | 12.108 | 3.08 | 0 | abeea4ecd | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-23T14:51:30Z | gate-commit | 21.137 | 46.271 | 12.426 | 2.78 | 0 | 4e4ce9f7b | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-23T15:29:55Z | gate-commit | 22.521 | 47.168 | 12.922 | 2.67 | 0 | 121bea519 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-23T16:07:20Z | gate-commit | 30.004 | 51.905 | 13.278 | 2.17 | 0 | ff3d1f207 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-23T17:07:40Z | gate-commit | 19.627 | 45.251 | 11.512 | 2.89 | 0 | 0cc78d784 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-23T17:08:18Z | gate-commit | 19.289 | 45.214 | 11.311 | 2.93 | 0 | 0cc78d784 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-23T17:10:04Z | gate-commit | 24.218 | 48.038 | 12.521 | 2.50 | 0 | 0cc78d784 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-23T17:49:15Z | gate-commit | 30.312 | 51.147 | 13.354 | 2.13 | 0 | 0b21e6177 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-23T19:10:45Z | gate-commit | 45.334 | 140.231 | 28.828 | 3.73 | 0 | ac0d5fda1 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-23T19:13:34Z | gate-commit | 30.755 | 48.676 | 12.111 | 1.98 | 0 | ac0d5fda1 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-23T19:46:36Z | gate-commit | 55.789 | 63.749 | 32.829 | 1.73 | 0 | 8d1b6e146 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-23T20:49:18Z | gate-commit | 31.035 | 64.006 | 37.215 | 3.26 | 0 | 3e12a87af | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-23T20:51:45Z | gate-commit | 18.949 | 45.985 | 11.579 | 3.04 | 0 | 3e12a87af | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-23T21:06:53Z | gate-commit | 26.067 | 56.205 | 21.027 | 2.96 | 0 | 02b61162b | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-23T21:07:47Z | gate-commit | 18.821 | 46.058 | 11.643 | 3.07 | 0 | 02b61162b | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-23T21:23:35Z | rebaseline | 101.141 | 235.718 | 10.052 | 2.43 | 0 | 1e412e7d2 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-23T21:24:55Z | gate-commit | 30.943 | 63.420 | 21.760 | 2.75 | 0 | 1e412e7d2 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-23T22:37:05Z | gate-commit | 54.737 | 95.075 | 45.964 | 2.58 | 0 | 0292de87f | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-23T22:38:41Z | rebaseline | 89.675 | 234.693 | 11.081 | 2.74 | 0 | 0292de87f | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-23T22:41:27Z | gate-commit | 19.683 | 46.501 | 11.950 | 2.97 | 0 | 0292de87f | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-23T23:12:24Z | rebaseline | 43.062 | 233.454 | 11.494 | 5.69 | 0 | 90f8d07a9 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-23T23:13:02Z | gate-commit | 23.555 | 48.485 | 11.889 | 2.56 | 0 | 90f8d07a9 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-23T23:13:55Z | gate-commit | 22.821 | 47.512 | 11.990 | 2.61 | 0 | 90f8d07a9 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-23T23:33:16Z | gate-commit | 19.691 | 46.220 | 11.832 | 2.95 | 0 | 07a187ac5 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-23T23:35:36Z | gate-commit | 20.628 | 47.189 | 12.229 | 2.88 | 0 | 07a187ac5 | campaign/the-portolan | MacBookPro | 10 |
| 2026-08-23T23:54:20Z | sluice:artifacts | 100.865 | 810.051 | 33.787 | 8.37 | 0 | 5d0e75514 |  | lefford | 40 |
| 2026-08-23T23:54:59Z | sluice:outboard | 37.674 | 23.890 | 37.623 | 1.63 | 0 | 39f10bfc0 |  | lefford | 40 |
| 2026-08-24T00:01:44Z | sluice:gate | 405.025 | 10955.894 | 449.294 | 28.16 | 0 | c43dafe6e |  | lefford | 40 |
| 2026-08-24T00:08:07Z | sluice:clients | 383.207 | 3450.336 | 46.161 | 9.12 | 0 | 91dc00fbc |  | lefford | 40 |
| 2026-08-24T01:46:04Z | gate-commit | 547.659 | 1403.535 | 174.780 | 2.88 | 0 | 45a037860 | campaign/the-forebay | ambrose | 12 |
| 2026-08-24T01:52:41Z | gate-commit | 115.070 | 65.693 | 37.752 | 0.90 | 0 | d8fbabd71 | campaign/the-forebay | ambrose | 12 |
| 2026-08-24T02:03:00Z | gate-commit | 56.478 | 62.730 | 47.859 | 1.96 | 0 | d8fbabd71 | campaign/the-forebay | ambrose | 12 |
| 2026-08-24T02:15:06Z | gate-commit | 691.973 | 1515.236 | 172.510 | 2.44 | 0 | d8fbabd71 | campaign/the-forebay | ambrose | 12 |
| 2026-08-24T02:18:34Z | rebaseline | 160.701 | 267.353 | 19.098 | 1.78 | 0 | d8fbabd71 | campaign/the-forebay | ambrose | 12 |
| 2026-08-24T02:21:13Z | gate-commit | 115.722 | 67.518 | 39.936 | 0.93 | 0 | d8fbabd71 | campaign/the-forebay | ambrose | 12 |
| 2026-08-24T02:28:17Z | gate-commit | 63.842 | 48.458 | 19.548 | 1.07 | 0 | 7f24ce368 | campaign/the-forebay | ambrose | 12 |
| 2026-08-24T02:29:37Z | gate-commit | 52.989 | 36.236 | 12.224 | 0.91 | 0 | 7f24ce368 | campaign/the-forebay | ambrose | 12 |
| 2026-08-24T02:43:13Z | gate-commit | 33.048 | 46.208 | 37.337 | 2.53 | 0 | f34be5869 | campaign/the-forebay | ambrose | 12 |
| 2026-08-24T02:45:51Z | gate-commit | 117.188 | 80.357 | 70.133 | 1.28 | 0 | f34be5869 | campaign/the-forebay | ambrose | 12 |
| 2026-08-24T02:47:21Z | gate-commit | 51.175 | 36.049 | 12.606 | 0.95 | 0 | f34be5869 | campaign/the-forebay | ambrose | 12 |
| 2026-08-24T02:56:32Z | gate-commit | 122.483 | 87.424 | 106.510 | 1.58 | 0 | 5463cb83e | campaign/the-forebay | ambrose | 12 |
| 2026-08-24T02:58:04Z | gate-commit | 49.669 | 35.847 | 12.058 | 0.96 | 0 | 5463cb83e | campaign/the-forebay | ambrose | 12 |
| 2026-08-24T02:59:38Z | gate-commit | 49.112 | 35.354 | 12.155 | 0.97 | 0 | 5463cb83e | campaign/the-forebay | ambrose | 12 |
| 2026-08-24T03:11:34Z | rebaseline | 50.696 | 202.241 | 7.021 | 4.13 | 0 | 86ef4991c | campaign/the-forebay | ambrose | 12 |
| 2026-08-24T03:12:32Z | gate-commit | 2.677 | 2.228 | 0.595 | 1.05 | 0 | 86ef4991c | campaign/the-forebay | ambrose | 12 |
| 2026-08-24T03:16:25Z | gate-commit | 157.098 | 409.763 | 107.118 | 3.29 | 0 | 86ef4991c | campaign/the-forebay | ambrose | 12 |
| 2026-08-24T03:17:29Z | rebaseline | 50.698 | 206.049 | 7.070 | 4.20 | 0 | 86ef4991c | campaign/the-forebay | ambrose | 12 |
| 2026-08-24T03:19:07Z | gate-commit | 49.998 | 35.587 | 12.036 | 0.95 | 0 | 86ef4991c | campaign/the-forebay | ambrose | 12 |
| 2026-08-24T03:32:11Z | gate-commit | 48.625 | 34.857 | 12.014 | 0.96 | 0 | b0402e75d | campaign/the-forebay | ambrose | 12 |
| 2026-08-24T03:33:57Z | gate-commit | 48.714 | 34.751 | 11.931 | 0.96 | 0 | ba91ab828 | campaign/the-forebay | ambrose | 12 |
| 2026-08-24T03:41:54Z | rebaseline | 33.388 | 201.962 | 6.928 | 6.26 | 0 | 7d89fa977 | campaign/the-forebay | ambrose | 12 |
| 2026-08-24T03:43:28Z | gate-commit | 48.882 | 34.822 | 11.778 | 0.95 | 0 | 7d89fa977 | campaign/the-forebay | ambrose | 12 |
| 2026-08-24T03:46:37Z | gate-commit | 49.353 | 34.921 | 11.979 | 0.95 | 0 | dbe129891 | campaign/the-forebay | ambrose | 12 |
| 2026-08-24T02:59:18Z | gate-commit | 75.596 | 581.278 | 108.215 | 9.12 | 0 | 71a1d9e99 | fix/seam-guard-ignores-untracked | lefford | 40 |
| 2026-08-24T03:00:16Z | gate-commit | 38.453 | 127.539 | 47.486 | 4.55 | 0 | 71a1d9e99 | fix/seam-guard-ignores-untracked | lefford | 40 |
| 2026-08-24T03:02:57Z | gate-commit | 36.946 | 121.960 | 47.469 | 4.59 | 0 | 2157300d9 | fix/seam-guard-ignores-untracked | lefford | 40 |
| 2026-08-24T03:05:17Z | sluice:artifacts | 61.035 | 542.520 | 23.501 | 9.27 | 0 | 26f12a54a |  | lefford | 40 |
| 2026-08-24T03:05:57Z | sluice:outboard | 39.964 | 27.209 | 39.494 | 1.67 | 0 | b3655ae13 |  | lefford | 40 |
| 2026-08-24T03:12:02Z | sluice:gate | 364.738 | 10263.819 | 350.851 | 29.10 | 0 | 89bbfb7c1 |  | lefford | 40 |
| 2026-08-24T03:18:02Z | sluice:clients | 359.065 | 3384.581 | 33.587 | 9.52 | 0 | bb6da9248 |  | lefford | 40 |
| 2026-08-24T03:49:18Z | rebaseline | 34.639 | 201.913 | 6.933 | 6.03 | 0 | 9f587f4f3 | campaign/the-forebay | ambrose | 12 |
| 2026-08-24T03:50:27Z | gate-commit | 49.297 | 36.015 | 11.892 | 0.97 | 0 | 368f05bf8 | campaign/the-forebay | ambrose | 12 |
| 2026-08-24T11:58:48Z | gate-commit | 126.414 | 69.478 | 41.594 | 0.88 | 0 | 91889116e | campaign/the-forebay | ambrose | 12 |
