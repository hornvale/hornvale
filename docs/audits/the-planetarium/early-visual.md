# The Planetarium: early GPU witness

Task 3, 2026-09-10. This is an emerging visual direction and a real moving
source-backed scene, not final visual acceptance or the Task 6 capture package.
Bevy 0.19.1 rendered the files on Apple M1 Max / Metal. The reusable
`hornvale-bevy-view` has no transitive Hornvale source or simulation dependency;
Planetarium owns the native worker and serialized observation channels.

## Preserved evidence

All paths below are relative to
`/Users/nathan/Downloads/Hornvale Planetarium/`. Large review files stay outside Git.

| Artifact | Path | SHA-256 |
| --- | --- | --- |
| Actual 3840×2160 still | `task3-4k-witness-01/frame-00000.png` | `e1ada3682768f18b9692c23c0f126bc03fa081f9bc737e820ea52c1c84498d23` |
| Actual H.264 movie | `task3-moving-witness-01/draft.mp4` | `ce27343a72aab0e0b2490c207aff03ca8be27e33a4163f8877c5da2cfcb9acac` |
| Exact source world | `source-preview-048212519/world.json` | `77168f2bc1a8db9c01b37b31b66ac4757e1133862f8249aa8d80bb0194285bf8` |

The movie contains 60 ordered 1920×1080 frames at 30 fps (2 seconds), with
60 distinct original PNGs and 60 decoded MP4 frames. Exact source ticks are
0, 60, …, 3540. The camera is fixed in physical coordinates for this shot;
observed moon translation and globe rotation supply the movement. Each PNG
finishes before the next source request. Both bodies retain their true radii
and separation. The still is a separate lit-limb shot at tick 0.

Each witness directory preserves `initial.json`, `draft.json`, frame PNGs,
per-frame `observation-NNNNN.json`, and the actual reconstructed surface texture.
The moving directory also preserves `verification.json` (all original/decoded
frame hashes, dimensions and identity checks), `decoded/`,
`decoded-motion-strip.png`, and phone-size first/last images. `phone.png` in the
still directory is a 360×203 review resize. These resizes are QA derivatives,
not substitutes for the full-resolution renders.

The loaded world is seed 42, one star, two moons and two wanderers. Binding is
`planetarium-pilot`, `scientific:unrestricted`, the exact world hash above, and
declared native source revision `1fc81e6acd570488eaeb275122a8927ad257c82a`.
The renderer was uncommitted at capture: `draft.json` records the dirty status
honestly and executable SHA-256
`7feabd760c207f69585ac33234a0a90c97999ab0363e670bd58f80d4796535c5`.
The revision identifies the native source baseline, not a clean renderer build.

## Physical and presentation conventions

- The emitted bulk radius is the spherical sea-surface reference. The anchor
  radius is 7126.4059 km; source sea level is −1820.2915 m. Relief is
  `max(reconstructed_elevation_m - sea_level_m, 0)` at 1:1. This is a declared
  rendering datum, not a source-modeled geoid or interior/terrain coupling.
  Raw source elevation is never called height above sea level.
- Tiles use the native latitude/longitude order, Z north, and longitude
  −180° to +180°. A convex 3×3 scalar reconstruction followed by bilinear
  sampling smooths the coarse export without adding extrema or invented
  geography. Biome IDs are not averaged. Sea-ice coverage uses a normalized
  valid-ocean mask; neighboring icy land cannot create new ocean ice.
- The custom globe mesh uses physical relief and radial normals. The moon
  keeps its emitted radius (first moon 2274.7776 km). Its albedo/tint and
  maria/cratering descriptors drive disclosed static cosmetic material
  variation. Null spin remains fixed cosmetic orientation, not inferred
  synchronous rotation. Missing-radius wanderers are point markers.
- Positions are subtracted from the camera origin in f64 kilometres before
  f32 conversion at 1000 km per render unit. No distance compression or body
  enlargement is used. Camera origins are bounded to 1e12 km and relative
  coordinates to 2e10 km; FOV is 0.005–2.5 radians. CPU projection witnesses
  include a large origin and nearby moon, below 0.25 pixel error at 4K.
- Body PBR uses point lights at the emitted stellar positions and current
  evaluated `luminosity_rel`. The photometric presentation reference is
  127000 lux at 1 AU for luminosity 1; Bevy intensity is
  `4π × 127000 × luminosity_rel × (AU_km / km_per_unit)²` lumens in render
  coordinates. Point range is 1e9 render units, exceeding the declared
  camera envelope; source radius is zero and shadows are disabled.
- Bevy's atmospheric shader accepts directional illumination independently
  of the body light layers. A separate atmosphere-only directional light
  uses the evaluated anchor direction/flux. Body PBR does not receive this
  directional light. Atmospheric illumination is therefore an anchor
  directional approximation; it is not off-anchor body illumination.
  Stellar disks are explicitly off because stellar radius is unavailable.
- The atmospheric shell is a cosmetic 80 km Earth-scattering treatment at
  0.18 optical density, centered explicitly on the actual anchor with
  metre-to-render scale 1e-6. Exposure is fixed EV100 13.3, AcesFitted
  tonemapping, no ambient light, MSAA off, no temporal history. Water/land/ice
  roughness is 0.24/0.86/0.65 and reflectance 0.35. Clouds are not rendered;
  the static climate export is not represented as evolving weather.
- Caption height is 6% of frame height with a 6% bottom margin. This Mac
  witness explicitly loads `/System/Library/Fonts/Supplemental/Georgia.ttf`,
  SHA-256 `4f54eb299fccea7f103edeb0d92437359bfd4441811d53222b82b335369f6218`.
  No font file or machine-specific path is bundled in the reusable view or
  committed film defaults. Missing requested font paths return an error.
  Later packaging must resolve font provenance and redistribution rights.

## Visual review and revisions

The first GPU appearance was rejected: atmospheric washout obscured the
terminator, categorical boundaries looked gridded, the globe touched the
frame edge, and the caption lacked an explicit UI camera. A body-only pass
isolated the atmosphere problem. Lower optical density, fixed exposure and a
phase-aware camera restored dark oceans, a readable terminator and a bright rim.

The first companion view clipped the moon and its cosmetic maria were harsh
spots. A wider physical camera and smooth static material variation corrected
those failures. Inspecting the actual surface texture located a dark polar
outline in the treatment of missing sea ice over land. Normalizing the
valid-ocean coverage removed it; two behavioral tests cover adjacent icy land
and icy/open ocean. The final still restores top margin. The larger serif
caption remains readable in 360-pixel-wide review copies.

The full-resolution still, first/last moving frames, five ordered decoded MP4
frames, and phone-size derivatives were inspected. The moon moves visibly;
the globe and its glint change; caption pixels remain identical across all
60 originals. The controller independently reviewed successive looks. Live
playback review is controller-owned and is not claimed by this report.

The palette, limb, ocean glint and quiet caption now establish a usable pilot
direction. Geography still reads smooth and coarse, especially the broad
polar/latitude appearance; this is explicitly carried into Task 5 refinement.
The moon has no source-resolved crater geography. No generated concept image
is used as a geography map. The concept was inspected as aesthetic reference.

## Reproduction and validation

The current command is a synchronous offscreen draft driver:

```sh
cargo +1.96.1 run --manifest-path clients/visual/Cargo.toml -p planetarium -- inspect \
  --world '/Users/nathan/Downloads/Hornvale Planetarium/source-preview-048212519/world.json' \
  --revision 1fc81e6acd570488eaeb275122a8927ad257c82a \
  --film clients/visual/planetarium/films/pilot.json --output NEW_DIRECTORY
```

The committed pilot is 60 frames at 1920×1080, step 60, companions shot.
The witnessed film inputs additionally selected the local Georgia path;
`draft.json` preserves those exact settings. For the still the input used
one frame, 3840×2160 and `shot: "limb"`.

Encoding used `ffmpeg -framerate 30 -i frame-%05d.png -frames:v 60 -c:v libx264
-threads 2 -crf 18 -pix_fmt yuv420p -movflags +faststart draft.mp4`.
`ffprobe` confirmed H.264, 1920×1080, 30/1 fps, 60 frames and 2.000000 seconds.

The renderer disables pipelined rendering for explicit synchronous driving.
Readiness checks applied observation identity, GPU meshes/images, caption
layout/atlases and compiled pipelines after extraction; screenshot completion
is observed before returning. It does not rely on a fixed two-update drain.

Outboard validation on the implementation that produced these witnesses:

- `cargo +1.96.1 fmt --manifest-path clients/visual/Cargo.toml --all --check`: exit 0.
- `cargo +1.96.1 clippy --manifest-path clients/visual/Cargo.toml --workspace --all-targets -- -D warnings`: exit 0.
- `cargo +1.96.1 test --manifest-path clients/visual/Cargo.toml --workspace -j2`: 18 view tests and 4 source tests passed, including the real native CLI comparison.
- `cargo +1.96.1 build --manifest-path clients/visual/Cargo.toml -p planetarium -j2`: exit 0.
- Resolved dependency-tree inspection: the view's only Hornvale package is itself.

Client-only lint exceptions cover presentation math, GPU deadline timing and
draft directory naming; simulation time comes only from exact source ticks.
The root already excludes all of `clients/visual`, so no redundant root
exclude entries were added. Linux/X11 prerequisites and the exact tagged Bevy
feature qualification are recorded in the campaign toolchain notes; this GPU
witness is Metal, not a claim of Linux runtime qualification.

Task 4 owns FilmClock, full scrub scheduling and reset lifecycle integration;
Task 5 owns interactive controls and authored visual refinement; later tasks
own verified capture packaging and performance. For the current draft a
binding change requires a new Renderer, so capture cannot cross bindings.
The controller owns the Stage 1 queue submission and independent review.

## Task 3 review fixes

Independent review found three boundary/evidence gaps. Replies now require
exactly one light per stellar body, and mirror acceptance requires the native
`star:0` / `star:1` catalog inventory appropriate to the initial topology.
A missing binary light returns an error without replacing the accepted snapshot.

Physical geometry is checked before app/assets creation and mirror acceptance:
radii must be at least 0.001 km; radius plus terrain (or the atmosphere shell)
must not exceed 1e6 km. Source elevation and sea datum are bounded to ±1e9 m,
so reconstruction/subtraction cannot overflow. Scaled radii must remain within
1e-6–1e7 render units. These are client support limits, not altered source values.

The actual camera support is now explicitly orbital: center distance must be
at least twice the outer body radius, including maximum positive source relief.
The renderer checks this before publishing a pending scene. Task 5 inspection
and dolly controls must enforce this limit; near-surface rendering needs further
precision work. An expanded CPU probe found 69120 pixels of Y error with a
camera 0.2 km above a 999999.8 km body at FOV 0.005, after accounting for near
clipping. That unsupported near-surface case now returns `ViewError::Range`.

The qualified CPU test exercises real `CameraPose::transform`, f32 quaternion,
body transform, view and Bevy projection matrices in both screen axes against
an f64 reference. It covers rotated and nearly-up-aligned views, origins through
1e12 km, relative coordinates through 2e10 km, FOV 0.005–2.5, physical radius
limits, actual moon/globe radii, and the twice-radius orbital boundary.
Across 6342 visible samples the maximum measured error is 0.101748006 pixels,
below 0.25 at 3840×2160. A regression separately rejects the close-surface case.

These changes add validation and CPU evidence; accepted geometry, lighting and
camera calculations are unchanged. All preserved still/movie observations were
checked against the new limits: minimum center/outer-radius ratios are
3.4976302888 (still) and 66.9378071599 (movie). The existing witnesses remain
valid dirty-build evidence with their original executable hash; no artifact was
overwritten or relabeled as a render of the validation-fix binary.
