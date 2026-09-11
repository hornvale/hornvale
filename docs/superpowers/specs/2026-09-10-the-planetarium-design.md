# The Planetarium — a cinematic Bevy observation client

**Date:** 2026-09-10

**Status:** G3 approved by Nathan on 2026-09-10: “LGTM; let's proceed to the plan!” Implementation and technical qualification are complete. Nathan approved the final visual direction and merge at G6 on 2026-09-11, accepting this pilot for now with further refinement in future campaigns. Canonical merge landed at `c0b76af87388ef33fa841b659684cfba52994a0a` on 2026-09-11; publication is outside scope.

**Campaign:** The Planetarium

**Decision ledger:** [The Planetarium](../ledgers/2026-09-10-the-planetarium.md)

## 1. Purpose and deliverable

Build the first beautiful, moving astronomical view of Hornvale in Bevy. The
same scene must support free camera and time controls and an authored ten-second
film sequence. The deliverable is both an inspectable native client and an actual
3840×2160, 30 fps, 300-frame visual study with simple captions. It is a pilot for
later individually directed films, not an approved public episode or a generic
film-authoring system.

Visual quality is part of completion. An exported video that merely proves a
pipeline, or a technically correct scene that Nathan does not find compelling,
does not meet the goal. The [generated glacier contact sheet](assets/2026-09-10-the-planetarium-concept.png)
from this conversation records the preferred light stylization, color and
sculptural depth; it is not a prediction of achievable frame quality or an input
map. It was generated with the built-in ImageGen tool for art-direction review;
its landscapes, paths and caption are illustrative, not Hornvale observations.

## 2. Relationship to existing policy

This proposed campaign schedules a graphical observation client explicitly.
On approval, it is a narrow exception to the graphical-client deferral in the
[long-term design §7](2026-07-05-hornvale-longterm-plan-design.md#7-explicitly-deferred).
It retains the Constitution's simulation ownership and query-surface principles.
It does not decide whether a future situated game should privilege text, maps,
or 3D; that broader possibility remains captured in the frontier.

It proposes the following changes to the
[Observation Series design](2026-09-09-hornvale-observation-series-design.md):

- One primary idea or observable may use multiple scales, shots and visual
  layers when they explain that same idea; a scale change is deliberate and
  preserves orientation and claim scope.
- Films are individually directed. Shared software grows through film
  campaigns; no fixed shot template is required.
- Early production follows visual quality and available evidence. The daily
  cadence, mandatory astronomical release staircase and seven-package reserve
  are suspended until production has earned a sustainable cadence.
- This ten-second visual study is a production artifact, outside the existing
  30–60 second public episode definition. Publication still requires Nathan's
  approval of the exact video and copy.

Existing episode manifests and records are historical inputs; this design does
not relabel them approved or change the meaning of their evidence. The earlier
spec remains available as history, with a supersession pointer added when this
design is approved and implemented.

## 3. Observed starting point

Inspected at `b6b374f6d2dea329d904b56322a09b1dfb29983f`:

- `windows/scene/src/lib.rs::SystemScene` emits the anchor, moons, stellar root
  and wanderers as orbital elements. It does not emit current positions.
- `domains/astronomy/src/ephemeris.rs` contains stellar and wanderer position
  evaluators and source illumination at an instant. The anchor position helper
  is private. Calendar and sky-position code supply additional orientation and
  phase observations; moon orbit/orientation must be reconciled with those
  existing producers, not guessed in a shader.
- `MoonSurface` emits physical radius and seeded albedo, cratering, maria and
  tint descriptors. These descriptors do not constitute a resolved moon terrain.
- `scene/tiles/v1` supplies anchor-world terrain and environmental fields.
  `scene/neighbors/v1` supplies a sky catalog. The observation capability audit
  records prior exercised exports; it is not a current Bevy rendering witness.
- The Observation Series shipped strict records, deterministic export and local
  package checks; its retrospective identifies the static preview renderer as
  the unresolved presentation problem.

Source inspection establishes available seams, not successful execution of the
new design. No Bevy build, GPU capture or performance measurement has been done
for this campaign. Official Bevy documentation establishes candidate facilities,
not that they work together on the selected host:
[introduction](https://bevy.org/learn/quick-start/introduction/) and
[rendering examples](https://github.com/bevyengine/bevy/tree/main/examples/3d).

A fresh native seed-42 world was built during drafting and `scene system` was
exported twice from it: `scene/system/v1`, topology `single`, two moons and two
wanderers; `cmp` found the repeated files identical. The ledger records the
commands and output. This qualifies a source candidate, not its visual quality.

### Planning amendment: physical anchor radius

The planning source audit found that `Anchor` carries mass but no physical
radius, and the terrain substrate explicitly uses a unit sphere. Nathan approved
adding a physical-radius prerequisite on 2026-09-10: “Include a physical-radius
prerequisite”. The campaign therefore adds a scientifically documented,
deterministic mass–radius observation at the astronomy layer before rendering a
physical globe. Its model, range, units and reference tests are fixed in Task 1
of the implementation plan. The initial model uses a declared Earth-like rocky
composition; it does not claim simulated interior composition. Bevy consumes
the emitted radius. No cosmetic sphere size substitutes for this observation.

This extends the approved source work; artifact/save effects are tested and
classified, not presumed absent. Existing terrain retains its angular contracts;
a bulk radius does not silently redefine river widths or movement distances.

## 4. Pilot subject and visual direction

The study observes passage of time in one selected system. Select and record a
real seed/pin set with an anchor, at least one moon and a wanderer; do a bounded
source search if the first candidate lacks them. Seed 42 is a starting candidate,
not a promised final scene. Selection serves composition, never a prevalence
claim. Once selected, freeze the revision, pins and represented time interval.

The first visual composition is anchor-centered: a lit world limb, a readable
moon and enough system context to understand their motion. A wanderer may be
shown as a sourced point when its surface/radius is not supplied. The study does
not invent a detailed sibling world to fill that gap. Existing single, wide
binary and close binary source shapes need parser coverage; the visually
finished pilot exercises one declared topology rather than claiming three
completed treatments.

Art direction:

- Saturated but controlled color, readable dark regions, convincing light and
  material separation; space remains visually quiet.
- Sculptural relief and carefully bounded atmospheric treatment at the world
  limb. Use exported terrain and descriptors where present.
- Selective focus expresses attention. Avoid blurring the phenomenon itself;
  the strong miniature effect of a valley is not mandatory at orbital scale.
- Smooth, purposeful cameras. Cuts between wide and close views are allowed;
  an uninterrupted universe-to-ground zoom is not required.
- One short caption at a time, readable on a phone, with film controls and
  diagnostic metadata hidden during capture.

A starting edit has three beats: 0–3 s establish the selected world and its
context; 3–7 s approach its lit limb while sourced motion continues; 7–10 s
reframe the moon and leave a clear final composition. This is an initial shot
brief, revisable against actual renders. Caption language remains observational;
the study makes no eclipse or ecological causal claim.

## 5. Truth, appearance and scale

Hornvale owns identity, physical dimensions, orbital motion, time and emitted
environmental state. Bevy owns presentation geometry, materials, exposure,
camera focus and UI. A visual effect must use existing physical observations
when those already answer its question.

Cosmetic material detail may interpret emitted descriptors. It must be stable
for the same presentation seed/settings and identified as cosmetic in the
production record. It must not create selectable facts, physical landmarks,
walkable terrain or behavioral consequences. No AI-generated concept image
serves as authoritative terrain or event evidence.

The pilot preserves physical body-size and distance ratios within each shot.
Use camera placement and cuts to make them legible. Optional screen-space
selection markers remain visibly markers and are hidden from the film. A body
with no emitted physical radius is a point/marker, not a fabricated sphere.
Body enlargement, compressed orbital distances, invented orbital geometry and
claims inferred from such geometry require a separate explicit fidelity
decision; this design does not authorize them.

Rendering-space origin shifts and unit conversions are allowed provided they
preserve relative geometry. Precision tests must cover the supported camera
range. The supported range is bounded to the selected scene and disclosed in
the client; whole-universe coordinate precision is not a pilot requirement.

Eclipse refinements remain owned by their current campaign. The pilot's chosen
interval avoids presenting an unverified eclipse; the selection record must
explain the check used. A pretty renderer-produced shadow is not a validated
Hornvale eclipse observation.

## 6. Architecture and observation seam

Proposed home: `clients/visual/`, an independent Cargo workspace with pinned
dependencies and its own checks. Planetarium is its first application; reusable
libraries have names and responsibilities independent of that application.
This does not require a separately published package or another repository.

```text
clients/visual/
  source/       hornvale-visual-source  — native observation library
  bevy/         hornvale-bevy-view      — reusable Bevy presentation library
  planetarium/  hornvale-planetarium    — first application and film direction
```

- `hornvale-visual-source` owns one initialized Hornvale world/context and
  exposes load and observation requests. It has no Bevy dependency and renders
  nothing. Reusable native source lifecycle and query handling belong here;
  authoritative observations remain in the simulation's windows/domain layers.
- `hornvale-bevy-view` accepts serialized observation documents; its dependency
  graph contains no Hornvale simulation crate or Planetarium application. It
  owns the reusable data-to-entity mapping, world/body identity handling,
  coordinate conversion, time-request plumbing, materials, camera primitives,
  selection and capture mechanisms that the pilot actually requires.
- `hornvale-planetarium` is the composition root connecting source requests and
  replies with the view. It owns the pilot's chosen seed/interval, astronomical
  scene composition, authored shots, caption text, controls layout and film
  definition. Neither library depends on this application.

Keep reusable astronomical visuals as cohesive modules in the Bevy library;
reuse does not require every module to be dimension-neutral. A later sprite
renderer can share observation/identity/time plumbing while supplying different
geometry and materials. An orthographic 3D treatment can reuse 3D components
where appropriate. No 2D backend, universal scene model, general plugin host or
additional fine-grained crate split is required by this campaign. Introduce
further abstractions when a second concrete use exposes the need.

Library tests must instantiate the source or view without depending on the
Planetarium application. The dependency check enforces both directions:
application → libraries is allowed; libraries → application and Bevy view →
simulation are forbidden. This verifies structural reuse without building a
second product merely to prove it.

Reusability does not make observation authority universal. The pilot uses an
unrestricted scientific observation source. A future situated game must use
its own session/observer-limited producer and display mirror, following the
existing native-game precedent (0114/0115); it must not fetch Planetarium's
world truth and rely on hiding it in the UI. The renderer has no implicit
world-data source. Cache and entity bindings include source/observation-scope
identity, and switching that scope discards incompatible state. Gameplay
commands, consequences and permitted time controls remain the future game's
own composition and source contract.

This extends the native driver/serializer pattern of decision 0114. A small
public driver API and dependency checks enforce the separation. The application
does not spawn the CLI and rebuild the world for each frame or query.

Static scene data is loaded once and reused. For a chosen instant, a new
semantic observation surface in `windows/scene` returns the evaluated geometry
and illumination the renderer needs. Its proposed document name is
`scene/astronomy-at/v1`; planning must confirm naming against the live roster.
It is separate from the elements-only `scene/system/v1`, whose meaning stays
intact. This extends The Wanderers' elements-only presentation policy with a
distinct evaluated query; it does not put snapshots into the element catalog.
The CLI exposes the same producer for fixture and offline export work.

The evaluated document must identify its schema, world/seed binding, exact
instant, coordinate frame, units, body identities and positions/orientations,
and source directions/contributions required by this scene. Omitted values and
unsupported queries are explicit errors or documented absence, never zeros
that look like measurements. Body IDs are stable within the bound world and
revision; array ordering alone is not a cross-world identity.

Reuse Hornvale's existing ephemeris/calendar evaluators. If an existing helper
needs a public observation wrapper, add it at the owning layer, with tests; do
not copy astronomy into Bevy. The new adapter must reconcile the documented
element-evaluation precedent with direct reuse of the native implementation.
Both paths must agree on the same world/time before rendering is trusted.

Requests and replies carry an identity so an old asynchronous reply cannot
replace a newer scrubbed time, a different world or a different observation
scope. Caching is keyed by source/scope identity, world binding and exact query.
Interactive scheduling may wait or coalesce requests;
it may not silently extrapolate a missing physical result. Export waits for
the exact requested observation.

## 7. Three independent clocks

- Simulation time is an exact Hornvale instant. Scrub, pause, reverse and speed
  controls request observations; they never integrate a second orbital model.
- Presentation time is the film playhead/camera timeline. An explicit mapping
  maps its positions to simulation instants. Camera motion may continue while
  simulation time is paused.
- Render work is the time the machine spends producing frames. Export frame
  indices determine presentation time independently of wall-clock rate.

For the 300-frame study, samples cover `[0, 10)` seconds at 30 fps. Mapping to
exact simulation instants uses a documented tick-rounding rule. Seeking directly
to a frame and reaching it through playback must produce the same semantic
observation and camera state. Reverse/scrub invalidates or resets temporal
render history as needed; temporal GPU effects do not define physical state.

## 8. Interaction and film capture

The client provides orbit/pan/dolly camera controls, focus on a selected body,
reset to the authored shot, pause/play, bounded time scrubbing and playback-rate
control. It also offers a clean film view and an optional inspection view with
identity, time, units and presentation disclosures. Editing the simulation,
possessing an individual and gameplay mechanics are future work.

The authored sequence and interactive view share body mapping, materials,
lighting and observation consumption. Capture may use more samples and higher
resolution; it must not swap in a different world or authored orbit animation.

Export uses persistent renderer state and GPU readback to produce ordered,
complete PNG frames, then an MP4 via the existing ffmpeg tooling. Warm-up,
asset readiness, temporal-history reset and readback completion are explicit
parts of capture; an offscreen/windowless path is a candidate to validate,
not an assumed Bevy capability on this host. A normal GPU window may host the
capture if needed; the output dimensions remain the requested dimensions.

Every frame carries a corresponding observation/time record. A package includes
the source revision, seed/pins, source documents, film definition, renderer and
toolchain versions, presentation parameters, asset hashes, frame hashes and
video hash. The implementation may reuse existing Observation Series code where
its contract fits; it must not fabricate per-frame packet provenance merely to
satisfy the old assembler. Retain package verification even if a small separate
study manifest is needed.

Fresh output directories and a completion marker distinguish complete packages
from interrupted runs. Missing/duplicate frames, failed GPU reads, unavailable
ffmpeg or failed encoding leave an incomplete study and a descriptive error.
There is no success fallback to a static image or text film.

## 9. Quality and verification

Two independent obligations govern acceptance.

Semantic/technical checks cover world binding, units, exact time, body identity,
source/evaluated-geometry correspondence, backward and out-of-order scrubbing,
world/scope-switch invalidation, library independence from Planetarium,
source isolation from the renderer, fixed frame count, capture completion and
package hash verification. The same query order must not change semantic bytes.
Cross-host GPU pixel byte identity is not promised. Repeat-render variability
must be measured and recorded instead of assumed absent.

Visual review covers the full moving export and representative stills at full
resolution and phone size: composition, color, silhouette, lighting, surface
quality, focus, motion continuity, caption readability and absence of temporal
artifacts. A representative Bevy still and short moving draft appear early so
visual feedback can change the implementation. Nathan judges the final moving
result at campaign close. Technical checks cannot auto-approve that judgment.

Record first-load cost, observation-query cost, interactive frame times, peak
memory, capture time per frame and total export wall time on the named host/GPU.
An interactive preview targets 30 fps at 1920×1080 on the selected development
machine; this is an acceptance target to measure, not a forecast. If unmet,
profile and simplify presentation cost while preserving observations, or bring
the measured limitation to Nathan. Do not silently shrink the 4K final target.

The client has its own format/lint/test checks and architecture guard, integrated
with the repository's client checks during implementation. CPU tests belong in
ordinary automation. GPU smoke and visual checks run on a documented capable
host and are required evidence, not silently skipped successes. Any expensive
canonical work follows the existing serial queue policy; GPU validation on the
Mac does not imply a local workspace stage gate or census.

## 10. Campaign boundaries and sequencing

The implementation plan, written only after G3 approval, should establish:

1. A qualified scene/data witness and an early real Bevy look-development image
   and moving draft, with the source/render boundary in place.
2. Correct time-dependent observations and interactive camera/time behavior.
3. Stable 4K capture, a refined ten-second edit, verification and visual review.
4. Client gate integration, source/usage documentation and campaign close.

This is a dependency outline, not the implementation plan. Choose an exact Bevy
release/toolchain during planning after checking available versions and host
support; pin them for the campaign. Dependency upgrades are deliberate work.

Out of scope: a universal film editor, a finished public episode reserve,
publication, arbitrary N-body dynamics, eclipse refinements, full sibling-world
generation, resolved terrestrial vegetation/fauna/crowds, seamless global
streaming, multiplayer, a gameplay rewrite or a conversion of simulation domains
into Bevy ECS components.

Later film campaigns may add climate/migration, ecological layers, language or
trade views, new exports and visual tools. Each earns its own story, witness and
render treatment. Their existence is not a requirement to finish this pilot.

## 11. G3 review flags

- **Policy scope:** explicitly scheduling this graphical client and changing the
  earlier Observation Series cadence and single-scale/grammar restrictions.
- **Schema seam:** a separate evaluated astronomy query, sharing native
  evaluators rather than putting another astronomy implementation in the client.
  No save epoch or new simulation draws are proposed. Any measured artifact
  changes or need to change simulation behavior require classification before
  proceeding; this document does not predict an empty regeneration diff.
- **Presentation boundary:** cosmetic material detail is permitted and recorded;
  physical ratios stay intact. Scale compression and invented physical features
  are not authorized by this spec.
- **Unmeasured feasibility:** the visual target, 4K capture path and interactive
  frame target require actual Bevy/GPU evidence. The earlier days/weeks discussion
  was a rough estimate, not a delivery commitment.

G3 approval accepts this campaign's scope and boundaries. It does not approve a
public film, replace final visual review, or authorize a merge.

## 12. Execution decisions for final review

These post-G3 rulings sharpen the approved scope. Nathan accepted the pilot and
approved its merge at G6 on 2026-09-11; canonical merge landed at `c0b76af87388ef33fa841b659684cfba52994a0a` on 2026-09-11. The [ledger](../ledgers/2026-09-10-the-planetarium.md) records
the alternatives, evidence and costs. Cross-campaign ownership is recorded in
decisions [0956](../../decisions/0956-planetarium-schedules-a-directed-graphical-study.md),
[0957](../../decisions/0957-evaluated-astronomy-stays-native.md) and
[0958](../../decisions/0958-visual-libraries-are-independent-of-the-film.md).

- **Radius (#8–9):** the approved prerequisite uses eight frozen knots from
  Zeng's Earth-like rocky curve, piecewise-linearly interpolated on 0.5–2 Earth
  masses. Its 32.5% Fe / 67.5% MgSiO3 composition is an assumption. No stored
  Anchor field, new random draw or existing-dynamics feedback was added.
- **Source coordinates and validity (#12, #14):** map calendar equatorial
  directions into the native system plane with Rz(pi) * Rx(-obliquity). Refuse
  nonpositive/nonfinite native luminosity at unsupported instants rather than
  clamping it or disallowing every negative tick.
- **Elevation and illumination (#15–16):** the physical bulk radius is the
  view's spherical sea reference, with positive (elevation minus sea level)
  displaced 1:1. This does not assert a source geoid. Actual-position stellar
  point lights illuminate solid bodies; separate anchor-directional feeds
  drive the cosmetic atmosphere. No apparent stellar disk or eclipse shadow
  is claimed. Static cloud shapes and their 12 km shell are cosmetics; the
  atmosphere's 80 km extent is a presentation choice.
- **Supported camera (#17):** orbital views remain at least twice a body's
  outer physical radius from its center; the pilot bounds camera distance to
  2e9 km. The measured finite precision grid is not a near-surface guarantee.
- **Time and lifecycle (#18):** presentation frames are half-open, while rounded
  simulation ticks may repeat or reach the end tick. Reset empties displayed
  entities and pending observations without requiring an invalid anchorless
  native document.
- **Clean capture (#19):** freeze an external film copy to the actual clean
  compiled revision and produce fresh observations and images. Do not relabel
  old output. A later documentation commit does not rewrite a capture's identity.
- **Failure deadlines (#20):** source waiting is bounded and late replies cannot
  publish into failed state. This is not forced preemption of native worker,
  operating-system or GPU calls.
- **Implementation sequence (#7, #10–11, #13):** Bevy 0.19.1 and Rust 1.96.1 are
  pinned; real moving GPU witnesses precede final package tooling. Source
  conformance wrappers belong to the evaluated-source task after the radius
  prerequisite. The initial native GPU qualification established feasibility,
  not final aesthetic or performance acceptance.
