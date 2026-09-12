# The Planetarium — measured presentation performance

The final 60-second 1920×1080 interactive run meets the approved p95 ≤33.33 ms
target: **20.389792 ms**. This is measured production-app performance, with source
queries, camera controls, inspection UI and the retained moon material enabled.

## Reproduction and scope

Artifact root: `/Users/nathan/Downloads/Hornvale Planetarium/`.
The clean build is `81ba2bfa6d1654c1e99d28b18ab8dc03d602c7ae`; the frozen executable
is `task9-qualification/planetarium-final-02`, SHA-256
`3c8fb8bd3e21f2d903eb0b6761dc1338f4dde6f12e8f227019a2773701f6c21c`.
`task9-qualification/clean-freeze-02.json` records the empty Git status, executable,
and external-film hashes. The source is the retained
`source-preview-048212519/world.json`; world SHA-256
`77168f2bc1a8db9c01b37b31b66ac4757e1133862f8249aa8d80bb0194285bf8`.
Film settings are preserved in `task9-interactive-final-02/samples.json` and the
verified [final package](final-review.md). The three-shot 10-second film still
uses 300 samples, native ticks 0–3588 from the authored [0,3600) interval,
4×MSAA, the original camera/light/exposure/DoF settings and physical scale.

Host: MacBookPro.local, Apple M1 Max, 64 GiB unified memory, macOS 15.6.1 arm64,
Metal backend, Bevy 0.19.1. `task9-qualification/host.json`, the runtime adapter
log and lockfile preserve the environment. This was a fresh process using
unflushed filesystem/OS/shader caches; it is not a cold-cache claim. The controller
reserved its CPU/GPU jobs. Other desktop activity remained present, including
Rust compiler processes and a running test suite from another campaign worktree,
VS Code helpers and WindowServer. `final-benchmark-contention-02.txt` preserves
an actual process snapshot. No other session's process was stopped or cache flushed.

Run, from the clean worktree, using the frozen executable and external film:

```sh
/usr/bin/time -l "$ARTIFACTS/task9-qualification/planetarium-final-02" inspect \
  --world "$ARTIFACTS/source-preview-048212519/world.json" \
  --revision 81ba2bfa6d1654c1e99d28b18ab8dc03d602c7ae \
  --film "$ARTIFACTS/task9-qualification/final-film-02.json" \
  --benchmark-out "$ARTIFACTS/task9-interactive-final-02"
```

`ARTIFACTS` denotes the artifact root above. Output directories must be new;
the quoted command records the actual run, rather than permission to overwrite it.
The benchmark rejects screenshot recording. The window logged physical 1920×1080,
logical 960×540, scale 2. Source observation acceptance, uploaded scene meshes/images
and a nonempty fully compiled pipeline cache establish readiness; two further
seconds warm the scene before sampling. First-load time starts as the first
statement of `main`, before CLI/bundle/film parsing, and ends at that GPU readiness point. The raw report names this clock
origin. This is program-entry timing; operating-system loader time before `main`
is included only in the external process wall measurement.

The fixed real-time script calls production `Playback` and `OrbitCamera` methods:
0–10 s forward film; 10–20 s reverse; 20–30 s paused frame 150 with orbit at 0.08 rad/s;
30–40 s paused frame 150 with pan at 0.008 normalized units/s; 40–50 s paused frame 150
with dolly at 0.008/s; 50–60 s forward film. This establishes application-script
behavior; manual OS mouse delivery remains a separate controller check.
`Instant` differences at production `PreUpdate` supply every raw frame interval.
Sampling stops at the first update at/after 60 s. The stored interval sum is
60.096513959 s and the final included sample starts 59.995895500 s after the
measurement origin; the first interval overlaps that origin by one frame.

## Results

Nearest-rank percentiles, computed by the retained `analyze-performance.py`:

| Metric | Final clean run |
|---|---:|
| Actual frame intervals | 4,435 |
| Frame p50 / p95 / p99 | 13.365084 / 20.389792 / 43.588209 ms |
| Longest frame interval | 154.991000 ms |
| Completed query-service samples | 868 |
| Query p50 / p95 / p99 | 46 / 129 / 225 µs |
| First-load readiness | 3.027038167 s |
| Measured script target | 60 s |
| Full process wall time | 65.72 s |
| Maximum resident set (`time -l`) | 648,282,112 bytes |
| Peak memory footprint (`time -l`) | 753,946,304 bytes |
| Source/camera error | null |

The Bridge retains query latencies only after explicit benchmark opt-in, and
drains every completed sample each update. The initial query is preserved
separately in `startup_query_service_micros`. These are source-service durations;
frame samples also record the shown frame and pending state. The process memory
figures include initialization and final report/executable-hash collection.
There is no claim about dedicated GPU allocation from these RSS measurements.
Raw evidence is `task9-interactive-final-02/samples.json`; `summary.json` and
`task9-qualification/interactive-final-02.log` preserve reduction and process usage.
A benign Bevy window-destruction warning occurred on shutdown after completion.

The preliminary draft 02 run remains `task9-interactive-01`: 7,095 frames,
p95=10.273416 ms, 899 queries, first-load 2.544591041 s. Its executable is identified
by the immediately preceding draft's hash in `provenance-join.json`. It was a
dirty development run with an earlier material; the final result above governs.
The first clean run, `task9-interactive-final-01`, also remains preserved:
6,689 intervals, p95 16.571291 ms and a 9.561812125 s **live-entry** readiness time.
Review found that this timer excluded CLI/film parsing. Commit `0ebdb75fc` moved
its origin to the first statement of `main`; its normal gate passed in 89.563 s,
and timing commit `81ba2bfa6` passed 76 prose tests in 5.132 s. A new clean build,
60-second run, repeat witness and complete package were then produced, rather
than changing the labels on old evidence. The final contended run above governs.
Clock origin and desktop load both differ across these runs; no isolated causal
performance comparison is claimed.

## Capture and repeatability

Same frozen executable, native source and film; final package
`task9-clean-300-02` contains 300 ordered 3840×2160 PNGs and the 10-second 30 fps MP4.

| Metric | Result |
|---|---:|
| Mean capture/readback/write per frame | 0.418969985 s |
| Mean complete per-frame operation | 0.443670053 s |
| Last frame record elapsed time | 137.778835625 s |
| Capture + encode + package validation wall | 189.18 s |
| Capture process-tree maximum RSS (`time -l`) | 3,076,734,976 bytes |
| Separate standalone verifier wall | 30.88 s |
| Two fresh renderer qualification passes | 34.48 s |

`commands-02.json`, `capture-final-02.log`, `verify-final-02.log`, `package-summary-02.json` and the package's
`frames.jsonl` are the primary timing records. The capture RSS includes the
encoder/verification path; it is a different workload from the interactive run.

`task9-repeat-final-02` holds two fresh production-renderer passes of exact frames
0/89/90/209/210/299, then the consecutive sequence 210–219: 32 actual 4K PNGs total.
All 16 corresponding pairs have matching observation/camera hashes, exact-pixel
fraction 1.0, mean channel error 0 and maximum channel error 0. All 16 PNGs in the
first pass also match their complete-package PNG bytes and native astronomy/camera
values exactly. GPU byte identity is measured here, without a cross-host promise.

Across frames 210–219, each pass has mean temporal RGB8 channel variance
2.020392895, RMS temporal channel standard deviation 1.421405196 and 9.31665943%
of pixels changing at least once. Intended camera/source motion contributes to
that variance. Corresponding repeat frames differ by zero, so there is no measured
repeat-dependent noise in this sequence. The retained `analyze-repeat.py`,
`comparison.json` and `package-comparison.json` expose both measurements.
