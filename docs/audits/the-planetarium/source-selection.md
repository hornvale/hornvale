# The Planetarium pilot: frozen source and directed interval

Measured 2026-09-10, Task 5. This is a selected visual study, not a prevalence claim or final visual approval. It completes the selection procedure in [source-qualification.md](source-qualification.md).

## Identity and interval

The film is [pilot.json](../../../clients/visual/planetarium/films/pilot.json): 3840×2160, 30 fps, 300 samples over presentation [0,10) seconds. The selected seed is 42, generated without pin overrides by the command recorded in the earlier qualification. The exact saved world freezes all ledger/registry and derived-under data; its SHA-256 is `77168f2bc1a8db9c01b37b31b66ac4757e1133862f8249aa8d80bb0194285bf8` (5,195,359 bytes). Local source: `/Users/nathan/Downloads/Hornvale Planetarium/source-preview-048212519/world.json`.

Authoring binding: source `planetarium-pilot`, scope `scientific:unrestricted`, native revision `1e11630202fd5db21a54944ab5f460afc28889ff`. The source tree was not modified by Task 5. Later package-local film provenance must bind the actual clean capture HEAD and fresh queries, as ledger 19 requires; none of these drafts is relabelled.

The exact source interval is [0,3600] ticks, with 100,000 ticks per standard day. `FilmClock` samples ticks 12×frame for frames 0..299; tick 3600 is an additional avoidance probe, not a presentation frame. Captions and camera offsets use pure frame sampling; physical body states are never interpolated. The source anchor basis turns about 14.7645° across this interval. Single-star topology, two moons and two unresolved wanderers are present. Missing wanderer radii remain point markers; missing moon spin remains absent.

## Camera bounds and appearance

Every authored sample was checked against every physical body's current emitted position. The anchor radius is 7126.4059 km; maximum positive source relief is 4.828275900 km. Required center distance is at least twice outer radius. Measured minimum ratios: anchor 2.440333, moon:0 74.529924, moon:1 317.291562. Interactive dolly clamps at each body exclusion sphere and the 2-billion-km camera-origin envelope; focus and pan/orbit refuse invalid geometry. Kilometre conversion is the view's shared `KM_PER_UNIT = 1000`, not application camera scale.

The three ranges are [0,90), [90,210), [210,300). Establishing space gives way to a closer sweep toward the native lit side; the final cut preserves actual moon/world size and distance ratios. Current positions remain sourced during all camera motion. Shader shadows are disabled; this is not the eclipse-avoidance test.

Shared settings are serialized in the film. Native stellar position/luminosity determines illumination; no fill light or fabricated stellar sphere is added. Positive elevation-minus-sea relief remains 1:1 with source-derived normals. The 3×3 convex reconstruction and bilinear scalar sampling retain broad source shapes. Stable presentation pigment grain is bounded to ±12%; it does not add selectable terrain or physical landmarks. Water roughness is 0.38 and material reflectance 0.28. The atmosphere is a disclosed cosmetic Earth scattering treatment (0.18 density, 80-km shell), not a simulated vertical profile.

The cloud treatment uses exported cloud_fraction (range 0..0.99753386; mean 0.38847356) to condition a static seeded translucent pattern. Its 12-km shell is a presentation layer, not a measured cloud altitude; it casts no shadows and invents no weather evolution. It rotates only with the exported body basis. The first bright fine pattern was rejected as too busy; the broader translucent candidate is qualified by actual images and motion, not by a claim that the native source resolves those cloud shapes.

MSAA4 and bounded Gaussian focus run on the actual Apple M1 Max / Metal GPU. Focus distances are converted from kilometres to render units; sensor/aperture and maximum 2-pixel blur are presentation settings. There is no temporal GPU history. Captions use unmodified Libre Baskerville Regular, upstream commit `9852edf75ece3af500a5ec61245f94788c3d4633`, font SHA `df9fddf43dbd7de435c316b86a52b3d6b3ad2f6fb2ed3f6fd8bdc1835f30eec1`. The original OFL and provenance are app-owned assets; no proprietary system font is required.

## Avoidance evidence

Controller evidence is under `/Users/nathan/Downloads/Hornvale Planetarium/task5-native-avoidance-01/`. The native command `hornvale scene eclipses --world <world.json> --from 0 --until 0.036` returned scene/eclipses/v2, ticks 0..3600, `events: []`. The command's bounds are standard days. The actual export SHA is `69787645d637c08c7e32199d435cb610c9330486078b8a8d9369b4f317950639`.

An independent native probe evaluates all 300 exact frame instants plus endpoint 3600, including the native sun angular-diameter model. Native sun angular diameter stays 0.520272640..0.520275518°. For moon:0 the minimum solar separation is 50.514137° and minimum anti-solar separation 128.712978°; for moon:1 they are 92.810519° and 86.845548°. Native overlap thresholds remain below 1.693435° and 1.443121°. The separate physical anchor+moon angular envelopes stay about 2.01057° and 1.27162°; both directions remain clear at every sampled instant. The dated native scan covers the inclusive interval; sample checks alone do not assert unsampled GPU behavior.

`native-samples.jsonl` SHA `7779a18b5dc16c13586bb4206e44feb60d73afe2d57c673820d989030ad98ff0`. Reproduction source, lockfile, analysis script and the complete numerical summary remain beside it. The controller compared all 301 application replies in task5-stills-01 to the independent native astronomy objects: exact equality at every instant, matching ticks/bindings. These angular observations qualify avoidance without inventing a physical stellar radius. They do not validate a stellar mesh, finite-disc rendering, or a new eclipse model.

## Visual and control evidence

All artifacts below are fresh directories under `/Users/nathan/Downloads/Hornvale Planetarium/`; earlier witnesses are preserved. Each draft.json records the actual capture HEAD, working-tree status, executable hash, source binding, complete sampled camera/caption records and frame/source hashes.

- task5-stills-01: real 4K grain/normals/font/MSAA/focus candidate.
- task5-stills-02: real 4K water roughness/reflectance A/B.
- task5-cloud-stills-01: rejected sharp cloud pattern; all actual PNG captions present.
- task5-cloud-stills-02: broader translucent cloud treatment and closer, more illuminated sweep.
- task5-cloud-stills-03: final caption clearance, five real 3840×2160 frames (0, 90, 180, 270, 299) and all 301 source replies; draft.json SHA `9199291e9e4cf8f4948a323844348fad9587d660eaae58cc68bb9129ced6bcb1`.
- task5-moving-01: full 10-second moving review, played through both cuts in QuickTime; its close-shot caption clearance prompted the final adjustment.
- task5-moving-02/review.mp4: final adjusted close framing, 300 frames, 30 fps, 10.000 s, 1920×1080, SHA `ec7e30d8bd5ae77ca97d7f38e5567f5a121d7ebc042ccb2cda4f02ef7ddf73f7`. draft.json SHA `efba5d0b54c3d77423007cb0b3e67eddaa900b6f1900f42cea73cb0a922cd7ae`. The actual 360-pixel phone copy at frame 209 has clear black separation above its caption; controller accepted the composition correction.
- task5-inspection-01/controller-ui and task5-inspection-02/controller-ui: actual Computer Use screenshots, including native DPI and resize.
- task5-inspection-02/controls.mp4: 915 acknowledged live screenshots encoded with their real elapsed cadence, plus final hold: 100.88 s, 1920×1080 padded without stretching resized content, SHA `eaab3e7e362be0cd84cfa0be354edaaf939bacebc08b493275c3511d1208d132`. Actual observed source time, camera and rate are recorded at screenshot request time; this is control evidence, not exact film export.

Actual input confirmed picking and focus, paused camera movement by focus/dolly, reset, playback/pause, frame step, scrub click, reverse, signed rates, film toggle and resizing at native scale 2. The initial unbundled DPI result was 3024×1832; applying the desired physical extent after Winit learns the native scale produced 1920×1080 with logical 960×540. Native resizing to 3024×1832 and back stayed readable. Unsupported inspection glyphs were replaced by ASCII, and captions are shown in clean film mode rather than overlapping inspection help.

The automation's drag action left the cursor at its start point. Therefore orbit and pan are tested as application/camera handlers, not claimed as manually validated input. The controller captured this tool limitation rather than adding an OS-input bypass. Final 4K packaging, formal performance acceptance and G6 remain later obligations. The pre-cloud inspection overlay averaged approximately 100 fps at physical 1080p; that preliminary observation does not qualify the final cloud material. Both final review drafts identify capture HEAD `34ac1c330f9379c92a07ad76b5adfa773073022b`, executable SHA `1f575147e3456bea1c727889dc35cd9e35c32a507ed2b821eba1d8b5d1eaa34b` and their actual dirty working-tree file lists. They are development witnesses, not clean-HEAD packages.

The final film also passed 601 actual source queries: forward 0..299, reverse 299..0, then direct frame 150. Native astronomy, sampled camera and caption at frame 150 were exactly equal on all three visits; report `task5-moving-02/direction-seek-check.json`. Pure boundary/order tests separately cover the authored film. Physical samples remain native; clouds and pigment are static presentation textures.
