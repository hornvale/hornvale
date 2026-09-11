# The Planetarium — final visual evidence

The refined candidate has a complete, independently verifiable 4K package and
measured interactive/repeatability evidence. **Nathan's final G6 visual/merge
acceptance and publication remain ungranted.** Controller movie/UI review is recorded below; canonical close evidence and G6
remain separate obligations. This audit does not complete the campaign or move main.

## Exact delivered candidate

Artifact root: `/Users/nathan/Downloads/Hornvale Planetarium/`.
Final package: `task9-clean-300-02/`. Actual movie: `study.mp4`, 3840×2160,
30 fps, 300 frames, 10 seconds, SDR sRGB transfer / BT.709 matrix and primaries /
limited-range yuv420p. Original lossless PNGs are `frames/000000.png` through
`frames/000299.png`; representative frames are 0, 89, 90, 209, 210 and 299.
The PNG originals remain the full-resolution visual authority beside the lossy MP4.

The package's clean source/build revision is
`81ba2bfa6d1654c1e99d28b18ab8dc03d602c7ae`. Runtime Git status was empty and
`build_tree_clean`/`rendering_source_tree_clean` are true. Later audit commits may
advance the campaign tip; they do not relabel this capture. The exact executable
is retained at `task9-qualification/planetarium-final-02`.

| Item | SHA-256 |
|---|---|
| Manifest | `f5a000d46c59826f6242fcb1ea04cb4e00c5dc5ea60a3179dc7a72caa9e3abef` |
| MP4 | `4d0da56b5d6cb0d7a7186009176f9e05992576fc4f642b1e8564fe522c31e3b1` |
| Package film definition | `215aa8abf7bb1eb7ab8b9072ae2a9a35432671a73d6c85dd9f5fe598a53823b5` |
| Frame records | `21179584992262db75bae14635003e6680fd0a859a4f7eee3470ece5a8b3b1d2` |
| Executable | `3c8fb8bd3e21f2d903eb0b6761dc1338f4dde6f12e8f227019a2773701f6c21c` |
| Source world | `77168f2bc1a8db9c01b37b31b66ac4757e1133862f8249aa8d80bb0194285bf8` |

The package manifest carries each PNG, observation, source, font and provenance
hash. `task9-qualification/package-summary-02.json` and `clean-freeze-02.json` preserve
the separate external-film serialization/hash used to invoke the command.

## Evidence-based visual refinement

The original `task7-clean-300-02` package remains untouched. Original frame 299
showed an overly soft, cloud-like moon surface; the decoded MP4 frame 209 and
original globe/caption composition supplied no reason to change shots, colors,
physical scale or lighting. The initial plan is retained in
`task9-qualification/visual-plan.md`.

Four fixed-camera drafts are preserved as `task9-moon-draft-01` through `04`:

1. Cosmetic tangent normals made rims readable, but regularly spaced marks and
   similar sizes looked manufactured.
2. Varied placement and sizes plus flatter maria removed the placement pattern,
   but the actual maria-rich moon's detail became too subdued.
3. More visible, overlapping marks still read as stamped rings. Independent
   controller inspection caused a crater-profile reassessment.
4. A continuous zero-edge-slope bowl, lower/broader irregular rim and corrected
   Mikk tangent-space Y derivative removed the stamped-ring appearance. The
   retained treatment gives restrained shading near the terminator while keeping
   the source's maria-rich face quieter. The controller independently accepted
   this as the candidate for final qualification, without granting G6.

`task9-moon-draft-04/fixed-comparison.json` proves that frame 299's camera,
caption, complete native astronomy and source binding are exactly the original's.
Its PNG SHA-256 is
`ca40e457e44fc95af82e5c3ca88b32c359304b52340e3e3f8553c5e397bce6ee`.
The normal map changes material shading only. No vertex, radius, orbit, physical
orientation, spin, save field or simulation random draw changes. Crater locations,
sizes and slopes remain cosmetic marks conditioned by the source's cratering and
maria descriptors; they are not resolved lunar geography. This is disclosed in
inspection, material comments, client README and capture provenance.

The final repeat witness covers both sides of cuts 89/90 and 209/210, the opening
and closing frames, and consecutive frames 210–219 using the production renderer.
Every corresponding pixel and observation/camera hash agrees between fresh runs;
all selected PNGs also match the complete package exactly. The
[performance audit](performance.md) records full frame/query distributions,
startup/memory/capture costs and actual temporal variance. The final 1080p run's
p95 20.389792 ms meets the 33.33 ms target; the p99 43.588209 ms and a 154.991 ms maximum remain
visible in the raw data.

## Validation and remaining boundaries

`make visual-check-run` passed client formatting, clippy, all 77 Rust tests and
doctests, dependency-boundary inspection and 6 Python guard tests. The source
query collector and cosmetic normal regressions include retained compiled RED
and GREEN evidence. The existing package suite now covers a positive clean
fixture and individually rehashed build-revision, dirty-build and dirty-runtime
contradictions, including the manifest's duplicate provenance. It reaches the
clean-build semantic guard instead of stopping at an earlier hash/mismatch check.

Implementation commit `00c733f4c` passed the normal local commit gate in 40.406 s;
timing commit `4d1e07354` passed its normal 76-test prose hook in 4.110 s.
Review then found that startup timing began after argument/film parsing. Commit
`0ebdb75fc` moved the origin to `main` entry; scoped clippy and 10 unit tests passed,
its normal gate passed in 89.563 s, and timing commit `81ba2bfa6` passed 76 prose
tests in 5.132 s. Every final measurement and the whole package were regenerated
from that new clean revision; the first clean package and its live-entry timing
are still preserved. The full fresh package completed in 189.18 s. A separate command
`planetarium-final-02 verify --out task9-clean-300-02` returned
`VERIFIED study package` in 30.88 s. Exact command arguments and process usage are
in `task9-qualification/commands-02.json`, the capture/verify logs and the performance audit.

The controller independently decoded all 300 RGB 4K PNGs, checked hashes,
ordering, times and bindings, and found 300 unique PNGs. All 300 native astronomy
payloads and cameras equal Task7. Exactly 62 original PNGs remain byte-identical
(frames 90–115 and 174–209); the broader middle shot is not claimed identical.
`task9-controller-review-01/check.json`, `check-package.py` and `ffprobe.json`
preserve that independent check.

All 300 PNG hashes and the complete MP4 bytes in the corrected-clock package
are identical to the first clean package `task9-clean-300-01`, whose movie was
actually played. `task9-qualification/package-summary-02.json` records this
explicit hash join. The controller independently re-decoded and verified all
300 new-package PNGs, times, bindings and clean provenance; its
`task9-controller-review-02/check.json`, `check-package.py` and
`visual-review-join.json` confirm the exact media-byte join. Both packages remain
preserved. Source/build/manifest hashes changed and are not relabeled as old ones.

The controller played that byte-identical MP4 in QuickTime through observed times
0.306, 2.910, 5.492, 8.074 and 9.650 seconds to the 10-second paused endpoint,
with no visible cut/history defect. Full-resolution originals/decoded frames and
six 360-pixel phone frames were inspected. Caption-band RGB is exactly equal
within frame pairs 0/89, 90/209 and 210/299; preview-tool glyph artifacts were
checked against original PNGs rather than attributed to the film. Evidence is
`task9-controller-review-01/{caption-band-check.json,phone-*.png,playback-*.png,`
`playback-*.txt,decoded-*.png}`. This is controller moving review, not Nathan's G6.

The UI attempt used the first clean executable at `4d1e07354`, before the
startup-clock-only correction. The manual-drag trial was not repeated on
`81ba2bfa6`. Its 140 acknowledged
control records showed a single unchanged request/time/camera and no errors
during a requested Sky drag; the pointer remained at its start. Manual OS drag
delivery therefore remains unproven. Subsequent real wheel input changed the eye
and entered free-camera mode; R reset and Right reached frame 1/tick 12. The
controller closed its owned app, and the process exited 0. The exact evidence is
`task9-controller-ui-01/{controller-check.json,app.log,bundle-provenance.json,`
`drag-start.png,drag-end.png,wheel.png,reset-step.png,controls/}`. Scripted controls
have not been substituted for the missing manual-drag witness.

The final `Planetarium Review.app` bundles the exact `81ba2bfa6` executable,
external film and source world. Root actually launched it through Sky, observed
the sourced frame 0 at physical 1920×1080 without a visible error, then closed
its owned window. `task9-controller-review-02/review-app.json`,
`review-app-smoke.jpg` and `review-app-smoke.txt` preserve hashes and the smoke
witness. This is a launch check, not a replacement manual-drag trial. Canonical
final stage/census accounting and the final G6 digest remain controller close work.

The established model/presentation limitations remain: the approved Earth-like
mass-radius assumption, source sea-level datum with physical positive relief,
source point illumination, static cosmetic cloud/haze layers, unsupported moon
spin, no eclipse shadows, and the bounded orbital camera envelope. The source
schema/model rulings and post-G3 ledger remain governing context for G6.

## Source, tools, assets and publication

Hornvale source and authored procedural presentation code use the repository's
MIT license. Bevy 0.19.1 declares MIT OR Apache-2.0. The installed FFmpeg 8.1.1
build reports GPL version 3 or later; its actual `-L` output is preserved as
`task9-qualification/ffmpeg-license.txt`. Libre Baskerville Regular is frozen to
upstream commit `9852edf75ece3af500a5ec61245f94788c3d4633`, under SIL OFL 1.1;
its SHA-256 is `df9fddf43dbd7de435c316b86a52b3d6b3ad2f6fb2ed3f6fd8bdc1835f30eec1`.
`font-OFL.txt` and `font-provenance.json` preserve the actual asset attribution.
The film uses native generated world data and procedural materials; no downloaded
planet photographs or external moon maps are embedded. Source/tool/asset license
records describe the local evidence, and no publication or redistribution was
performed. Publication remains a separate decision from G6 visual/merge approval.
