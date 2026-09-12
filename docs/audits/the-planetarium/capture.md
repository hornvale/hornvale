# The Planetarium: acknowledged GPU capture

Measured 2026-09-10 on macOS 15.6.1 (24G90), arm64, Apple M1 Max,
Metal backend. Bevy 0.19.1, wgpu 29.0.4, image 0.25.10 and
`rustc 1.96.1 (31fca3adb 2026-06-26)`; optimized development profile, not release.

## Result and scope

The final production capture binary wrote all **300 RGB PNG frames at 3840×2160**
in a foreground process that exited 0. `/usr/bin/time -l` measured **88.74 s
real, 48.75 s user, 15.19 s sys**, maximum resident set **749,305,856 bytes**
and peak memory footprint **1,076,644,992 bytes**. These are the host tool's
separate process measurements, not a claim about discrete GPU memory use.
The output contains 528,025,396 bytes of PNGs.

This is a development qualification on base
`385df541db36399df046178c6a3c6c79681d5781` with explicitly recorded dirty source.
The final run records HEAD `3d84d84f67aed8f91f8e0dd6da581e52b1f9809d` (a controller docs commit within the task range). It retains the approved pilot film and earlier allowed source binding
`1e11630202fd5db21a54944ab5f460afc28889ff`. It is **not** a clean final package:
there is no encoding, final manifest, or `COMPLETE` marker. Task 7 must repin a
package-local film to a clean source revision and issue fresh source queries;
these frames cannot be relabeled as that result.

Capture executable SHA-256:
`718e9b1e547b18a1a76faf5e9d7b433a54273da55f8aae461a5bf40b208adb18`.
All tracked and untracked `clients/visual` source file hashes are preserved in
`source-files.json`, alongside a development patch, dirty status, executable,
font, presentation settings and source revision in `provenance.json`.

## Production acknowledgment contract

`CaptureMachine` owns Preparing → AwaitingObservation → Warming →
AwaitingReadback → Writing → next observation / Complete, with terminal Failed.
The actual renderer drives this machine. Each screenshot callback carries its
issued frame ID and returns the image; it does not write a file. Wrong,
duplicate and obsolete acknowledgments cannot transition a different frame.
There is one outstanding GPU screenshot. SceneCatalog's existing lock spans
readiness, warmup, readback, validation and writing. Failure keeps reset and
further scene application blocked; rebuild is the safe recovery. The source
query, camera and caption advance only after the preceding write acknowledgment.

Production settings are width 3840, height 2160, frames 300, warmup_frames 3,
timeout_seconds 120. Settings are immutable through the machine's public API.
Each stage has its own deadline; progress within warmup does not reset it.
Preparing includes source initialization via Bridge::open_timeout. Each
AwaitingObservation budget covers source work via Bridge::observe_timeout,
including draining earlier interactive work and waiting for the exact query,
then renderer readiness. Condition-variable wakeups retain the original absolute
deadline. A timeout permanently closes the Bridge, clears pending/completed
slots, prevents future submission and late publication, and detaches its worker
handle so Drop cannot join a stuck query. Native Rust work cannot be forcibly
interrupted: an abandoned thread may finish privately, and the CLI's error exit
terminates the process. Blocked fake initializers/queries and spurious wakeups
are tested, with fake workers released afterward. Explicit device polls are capped to the smaller of one second and the remaining
stage budget, and deadlines are checked before and after every app update.
Synchronous driver/shader work inside an app update is not preemptible by this
Rust driver; any overrun is rejected immediately when the update returns.
Likewise, synchronous filesystem operations are checked after returning, not
forcibly interrupted. This is a bounded source wait and GPU-poll contract, not
an OS-level preemption promise. A
missing callback fails at AwaitingReadback with its frame ID and stage, rather
than creating a success marker or advancing the world.

Source-owned meshes and textures must exist before extraction, producing a
named missing-asset error otherwise. Readiness also requires the exact applied
observation identity, caption glyph atlas images, uploaded scene assets and
output image, at least three extraction updates, and a nonempty fully successful
pipeline cache with no waiting pipelines. Pipeline errors fail the run. Then
three additional warmup updates hold the same observation/camera/caption.

The target is an independent `Rgba8UnormSrgb` Image; no window is created.
The built-in `Screenshot::image` route supplies tightly packed rows. The writer
checks dimensions, RGBA8 sRGB format and exact byte count; encodes RGB8 PNG;
decodes it and compares all pixels against the readback; then uses create_new,
write_all, flush, sync_all and close before acknowledging Writing. Existing
files are never overwritten. `image` is a direct dependency on the already
locked Bevy image version; no second codec version was introduced.

The shared production camera/material/geometry settings and pilot JSON did not
change. Exposure remains fixed at EV100 13.3. TAA, motion blur and temporal
history are absent; Gaussian focus is spatial. Seeded body-fixed pigment,
cloud detail and moon surface presentation retain their existing deterministic
seeds. No temporal effect was restored. Reset/backpressure tests remain green.

## Diagnostic and actual-scene qualification

Permanent evidence root: `/Users/nathan/Downloads/Hornvale Planetarium/`.
All runs used fresh directories; earlier witnesses were retained.

| Run directory | Frames and size | Wall seconds | Maximum RSS bytes | Peak footprint bytes |
| --- | --- | ---: | ---: | ---: |
| task6-diagnostic-01 | 3 at 257×129 | 20.27 | 423493632 | 384240256 |
| task6-diagnostic-02 | 3 at 257×129 | 2.83 | 426426368 | 393611904 |
| task6-scene-60-01 | 60 at 3840×2160 | 25.97 | 729169920 | 1083165888 |
| task6-full-300-01 | 300 at 3840×2160 | 111.00 | 781139968 | 1095617664 |
| task6-full-300-02 (final binary) | 300 at 3840×2160 | 88.74 | 749305856 | 1076644992 |

The diagnostic is a separate executable/run with an opaque calibration UI
scene on the same renderer target and capture path. It never appears in study
footage. At width 257, the raw RGBA row has 1,028 bytes, deliberately exercising
GPU row alignment. Decoding all three PNGs found exact red/green/blue/white
corners at (1,1), (255,1), (1,127), (255,127), in that orientation. Center pixel
(128,64) was exactly (frame ID,128,64) for frames 0, 1, 2. The first run's text
counter clipped at the right edge; its byte counter still verified identity.
Only the diagnostic font was reduced from 18 to 12 pixels, and run 02 visibly
shows the full `FRAME 000002` label. Both checks are retained. This establishes
channels, orientation, row padding removal and frame correspondence; there was
no observed reason to switch to manual GPU buffers.

The first 60 actual frames constitute two seconds at the unchanged 30 fps.
Their per-frame capture cost, including readiness/warmup, readback, PNG
roundtrip and durable file write, was mean 0.316649 s, median 0.271958 s,
min 0.217082 s, max 2.007223 s. First full-run costs were mean 0.333864 s, median 0.271990 s, min
0.209126 s, max 1.714165 s. The final binary's full-run costs were mean
**0.268572 s**, median **0.259127 s**, min **0.199759 s**, max **0.582115 s**.
These are repeated-run measurements, not a causal speedup claim for the
source-wait fix. Source query and
PNG hashing/record persistence are additionally accounted in frame_seconds
and total wall time. Every individual timing is in frames.jsonl.

All 60 first-run PNGs were **byte-identical to the corresponding 60 full-run
PNGs**: a 60/60 repeated-render witness. After the bounded source-wait fix,
all **300/300 final-binary PNGs were byte-identical to the first full run**.
This qualifies this host/settings, not cross-GPU determinism. All 300 full-run
PNG hashes are distinct.

The implementer decoded every PNG, verified its dimensions/mode/name/hash,
checked each fresh observation's hash, binding, request ID and tick (12×frame),
and matched captions to the unmodified film. The copied world hash matches the
binding. Frames 0, 150, 210 and 299 were visually inspected: source-backed scene,
limb, approved framing and captions are intact. The controller independently
decoded all 60+300 files and checked all mappings, plus equality of each emitted
astronomy object with previously independently qualified native source output.
The controller repeated its full independent decode/source checks for final
run 02, confirmed all 300 hashes equal run 01, and viewed its actual 4K frame 90.
Controller checks are retained in all actual-run directories. Final moon appearance
and interactive performance remain Task 9's explicit acceptance work.

## Evidence layout and commands

Each actual run contains film.json; source/world.json; source/initial.json;
source/observations/000000.json onward; frames/000000.png onward; frames.jsonl;
provenance.json; source-files.json; development-source.patch; and capture.json.
Records retain frame, request ID, exact ticks, camera, caption, image path/hash,
observation path/hash and timings. capture.json says only that the requested
frame files were written; the 60-frame run has full_film=false and both have
package_complete=false. No old Observation Series record is synthesized.

Full records SHA-256:
`dd1f6f74f7e825a4310e6383821d21548abadfc14d40b66dd942500a0b0940fc`.
First frame SHA-256:
`697904cb5aa62d0dee2aac2508b0ccb91a91034a20433a9147b632ec3ceb1bb9`.
Final frame SHA-256:
`e58f57d6464ff0053b95e8fdb347c9f09347ccc068d742bc9e839976277d3cb9`.

Run commands from the campaign worktree:

```sh
cargo +1.96.1 build --manifest-path clients/visual/Cargo.toml -p planetarium --example capture_diagnostic --bin planetarium
/usr/bin/time -l clients/visual/target/debug/examples/capture_diagnostic '/Users/nathan/Downloads/Hornvale Planetarium/source-preview-048212519/world.json' clients/visual/planetarium/films/pilot.json '/Users/nathan/Downloads/Hornvale Planetarium/task6-diagnostic-02'
/usr/bin/time -l clients/visual/target/debug/planetarium capture --world '/Users/nathan/Downloads/Hornvale Planetarium/source-preview-048212519/world.json' --revision 1e11630202fd5db21a54944ab5f460afc28889ff --film clients/visual/planetarium/films/pilot.json --out '/Users/nathan/Downloads/Hornvale Planetarium/task6-scene-60-01' --limit 60
/usr/bin/time -l clients/visual/target/debug/planetarium capture --world '/Users/nathan/Downloads/Hornvale Planetarium/source-preview-048212519/world.json' --revision 1e11630202fd5db21a54944ab5f460afc28889ff --film clients/visual/planetarium/films/pilot.json --out '/Users/nathan/Downloads/Hornvale Planetarium/task6-full-300-02'
```

Stdout/stderr, time outputs, CPU red/green/build/check logs and executable
inventory checker are under task6-evidence-01. The PNG inventory checker ran
after the timed GPU processes exited. There were no local root stage/heavy,
census, full artifact regeneration or local full-workspace test commands.

## CPU checks and implementation boundaries

The initial five transition tests failed to compile because the capture module
did not yet exist, then passed after implementing the actual driver contract.
The final six capture tests cover wrong/duplicate/obsolete callbacks; failed
readback; missing-readback timeout; real missing-texture detection before frame
zero; readiness/warmup deadlines; malformed dimensions/stride and no overwrite.
Fault handling is CPU tested; an actual device-loss event was not injected.

Scoped fmt check, clippy with -D warnings, and tests passed. Test totals:
5 renderer lifecycle unit tests, 40 renderer integration tests, 8 application
worker/observation unit tests, 10 application integration tests, and empty doctest
suites. The GPU example is not invoked by CPU tests.

```sh
cargo +1.96.1 fmt --manifest-path clients/visual/Cargo.toml -p planetarium -p hornvale-bevy-view --check
cargo +1.96.1 clippy --manifest-path clients/visual/Cargo.toml -p planetarium -p hornvale-bevy-view --all-targets -- -D warnings
cargo +1.96.1 test --manifest-path clients/visual/Cargo.toml -p hornvale-bevy-view -p planetarium
```

The approved modification scope was expanded only as preflight allowed:
renderer/lifecycle acknowledgment plumbing, application capture module and
CLI, diagnostic example, direct existing image codec dependency, and the bounded
source-worker waits approved in ledger #20. The first 300-frame qualification
predated the bounded source-wait follow-up; a fresh second full run qualifies
that final binary, without relabeling the first run. The deferred
Task 5 compressed action/error boundaries in main.rs and control_recording.rs
were expanded while touched, without changing live recording behavior.


The source-wait follow-up had a second RED/green cycle: missing observe_timeout
and receive_initial methods failed compilation, then blocked initialization and
drain tests passed. The final suite additionally exercises continuous spurious
wakeups during the exact query wait, verifies late replies are discarded after
terminal close, and uses the bounded API for successful exact queries. All fake
workers are released after the bounded-return assertion. Malformed `--limit`
with no value now errors instead of silently choosing a full capture.
