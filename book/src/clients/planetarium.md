# Planetarium

Planetarium is Hornvale's native Bevy astronomical observation client. Its
interactive view and ten-second film share the same sourced bodies, lighting,
materials and camera primitives. It is a scientific observer, with unrestricted
world access, not a possession or gameplay session.

The application lives in `clients/visual/planetarium`, composed from the reusable
`hornvale-visual-source` and `hornvale-bevy-view` libraries. Another application
can supply permitted serialized documents to the view without importing the
Planetarium film or captions. The view has no implicit world source. A future
situated game needs its own observer-limited producer and mirror; hiding this
scientific source's truth in a UI does not provide that boundary.

## Commands and controls

From the repository root, use the pinned toolchain. `WORLD` is a real saved world;
`FILM` is a `planetarium/film/v1` file whose source binding matches it and `REV`.
The committed `clients/visual/planetarium/films/pilot.json` preserves an earlier
source revision; changing code does not silently rebind an existing film.

```sh
cargo +1.96.1 run --locked --manifest-path clients/visual/Cargo.toml -p planetarium -- inspect --world "$WORLD" --revision "$REV" --film "$FILM"
cargo +1.96.1 run --locked --manifest-path clients/visual/Cargo.toml -p planetarium -- capture --world "$WORLD" --revision "$REV" --film "$FILM" --out "$NEW_DIRECTORY"
cargo +1.96.1 run --locked --manifest-path clients/visual/Cargo.toml -p planetarium -- verify --out "$NEW_DIRECTORY"
```

Drag to orbit; shift-drag or middle-drag pans; the wheel dollies. Select a body,
press F to focus and R to restore the authored view. F on an unresolved marker
aims at it without inventing a safe physical radius. Space toggles playback;
left/right step frames; V reverses and +/- change rate. The scrub bar seeks
within the supported film interval. Tab toggles inspection metadata and controls.

Simulation time, presentation time and render work are independent. For frame
`i` of `N`, exact ticks are `start + round((end-start)*i/N)`, with integer
arithmetic and offset ties rounded away from zero. The pilot samples frames
0–299 at 30 fps over `[0,10)` seconds; ticks are 0–3588 in steps of 12, within
the supported 0–3600-tick interval. Seeking or reversing requests the native
instant; it does not extrapolate missing physics. Rendering may take longer than
one thirtieth of a second without changing a captured frame's time. No temporal
GPU history supplies scientific state.

## What the picture means

The [evaluated astronomy document](../reference/scene-astronomy-at-v1.md) owns
identity, position, orientation, physical dimensions and source illumination.
Within a shot, body-size and distance ratios remain physical. Stars and
wanderers have no supplied radius and appear as points/inspection markers.
Source terrain elevation is reconstructed relative to its sea datum. Relief is
not an invented walkable landscape. The Earth-like anchor radius is a declared
mass–radius model, not a simulated interior.

Materials interpret source biome, elevation and moon descriptors. Fine pigment,
moon surface appearance and fixed moon orientation are cosmetics; there is no
resolved crater geography or physical moon spin. Cloud shapes and the 12 km
shell are static cosmetics. The 80 km atmospheric scattering treatment, exposure,
water roughness and focus are presentation choices. The selected seed-42 interval
has separately recorded eclipse-avoidance evidence; renderer shadows do not
validate an eclipse. No generated concept image is used as physical source data.

The camera is orbital: it stays at least twice each body's outer radius from its
center, including positive terrain relief. Near-surface landings are unsupported.
The reusable conversion supports origins through 1e12 km, camera-relative
coordinates through 2e10 km and vertical FOV 0.005–2.5 radians; the interactive
pilot further bounds camera distance to 2e9 km. Physical radius starts at 0.001 km;
radius plus relief/atmosphere must not exceed 1e6 km. Unsupported geometry fails
before scene acceptance. Origin subtraction occurs in f64 before the f32 GPU
conversion; whole-universe precision is not promised.

## Capture evidence and failure

A full study contains 300 ordered 3840×2160 PNGs, exact per-frame observations,
film/camera/caption records, source and asset hashes, toolchain/GPU provenance,
and an MP4. A fresh absent output directory is mandatory. Warm-up, asset readiness,
GPU readback and frame ordering are checked. Missing or duplicate frames, encoder
failure, failed reads or mismatched provenance leave an incomplete directory;
`--limit` is a development witness and never completes a qualified study.

The verifier replays semantic identity and film state, decodes PNGs, checks every
hash, probes the MP4's 4K/30 fps/300-frame profile and checks coarse decoded-video
correspondence to the PNGs. The SDR output uses BT.709 primaries/matrix, sRGB
transfer and limited-range YCbCr; original RGB8 PNGs remain the lossless source.
`COMPLETE` binds the verified manifest and is written only after success. It is
not permission to publish or an automatic judgment of visual quality.

Run `make visual-check` for the CPU gate. GPU qualification separately requires
an actual inspect/capture/verify run on a capable named host, moving review,
full-resolution and phone-size stills, and recorded timing/provenance. The qualified
Task 7 package was captured at `dd37a8e1899e6ac4ad0277f55a0eacb85a3ebefa` on Apple
M1 Max/Metal with Rust 1.96.1, Bevy 0.19.1 and ffmpeg 8.1.1. Its 300-frame capture
took 85.079 s, and capture through encode/verification took 126.17 s. These are
capture measurements, not an interactive frame-rate claim. Later source revisions
must not borrow that clean-build identity. Cross-host GPU pixels are not promised
byte-identical.

The caption font is Libre Baskerville under the bundled SIL Open Font License.
`clients/visual/planetarium/assets/provenance.json` records upstream revision,
asset hashes and sizes; `OFL.txt` retains its license. Bevy's dependency licenses
and the client's MIT license remain separate. The generated art-direction concept
is a design reference only and is not a runtime asset.
