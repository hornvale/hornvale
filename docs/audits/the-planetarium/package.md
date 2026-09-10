# The Planetarium — verifiable study packaging

Task 7 implements `visual/study/v1` separately from Observation Series episode
packets. The Atlas-specific exporter expects different identities, filenames,
viewport and optional-video behavior; no fictional packet correspondence is used.

`planetarium capture` creates an absent output directory, records fresh source
observations and exact rational presentation times, and packages a full qualified
3840×2160, 30 fps, 300-frame film. Limited runs remain incomplete development
witnesses. Any failure preserves the directory and writes FAILED; reruns choose a
new directory. `planetarium verify --out DIR` requires no world/film arguments.

The manifest binds world, initial document, film, ordered frame records, video,
font, source inventory and runtime/build provenance. ObservationMirror independently
replays the source binding, requested ticks and body inventory. Camera and caption
are recomputed from the film and recorded observation; PNGs are hash-checked and
fully decoded with the existing image 0.25.10 dependency. JSON float_roundtrip
preserves the exact camera f64 values across serialization.

Every package path must be relative, with normal components and no symlinks.
ffmpeg/ffprobe use explicit argument arrays. Encoding must succeed with libx264;
probe independently requires all 300 decoded frames, 4K, 30/1, yuv420p and the
recorded color tags. An independent 64×36 RGB witness from every decoded video
frame is compared with its corresponding PNG (mean absolute channel difference
at most 8/255). This detects gross content/order substitution while accommodating
lossy compression and different downsampling kernels; it is not a proof against
substitution of near-identical adjacent frames. Exact source-to-frame identity is
established separately by the ordered observation and PNG records.

The output is SDR: full-range RGB8 sRGB PNGs are converted using a BT.709 matrix
to limited-range YCbCr, retaining the sRGB transfer and BT.709 primaries tags.
Original lossless PNGs remain the high-quality source. Actual encode/decode color
qualification and clean package hashes are recorded below after the fresh run.

Build provenance is embedded by a build script that reruns for every build,
recording actual build HEAD, whole-tree clean status and rustc version. Runtime
HEAD alone cannot produce a clean claim: build status, runtime status and both
revisions must agree with the film binding. Runtime source inventory/diff commands
are anchored to `git rev-parse --show-toplevel`, rejecting an unexpectedly empty
client inventory. Capture records the actual selected Bevy GPU/backend and its
executable SHA-256. Clean capture rechecks source cleanliness before packaging.
This is reproducibility evidence, not a signed supply-chain attestation.

Manifest is written after encoding, then the same content verifier runs with only
the marker requirement deferred. A synced pending marker is atomically renamed
to COMPLETE only after success. Public verification validates its manifest hash.
A digest is not an authorization to publish; visual acceptance remains separate.

CPU fixtures preserve the public qualified profile using compressed flat 4K PNGs
and 300 ordered observations. Only media-process boundaries are stubbed; all real
manifest, identity, camera, caption, PNG, path and completion checks execute.
Fixtures are disposable and explicitly synthetic. The fresh package uses the real
encoder and probe. Tests cover missing frame, duplicate index, changed ticks even
after rehash, camera/caption/time contradiction, world binding, corrupted PNG and
video, decoded-video mismatch, traversal/symlinks, interrupted completion, fresh
output refusal, missing encoder, nonzero exit, missing codec and bad probe profile.

The first real clean attempt (`task7-clean-300-01`, build
`61eb2454373746306f676522e8eff7f4caec736b`) preserved all 300 frames and a video,
but correctly refused COMPLETE: ffmpeg 8.1.1 omitted transfer/primaries tags when
only output flags named them. Its FAILED file names the probe mismatch. A
one-frame reproduction demonstrated that appending explicit frame metadata with
`setparams=range=limited:color_primaries=bt709:color_trc=iec61966-2-1:colorspace=bt709`
retains all four required probe tags. The conversion itself is unchanged. The
encoder fixture now fails without this argument. The failed run is preserved;
final qualification uses a fresh clean revision and fresh frames in another root.
