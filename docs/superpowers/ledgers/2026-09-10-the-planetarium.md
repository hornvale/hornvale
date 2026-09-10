# The Planetarium — decision ledger

Status: G3 and physical-radius prerequisite approved; G4 plan self-review complete; ready for execution.
Branch: `campaign/the-planetarium`.
Starting revision: `b6b374f6d2dea329d904b56322a09b1dfb29983f`.

## Conversation decisions carried forward

Nathan wants individually directed, beautiful short films. Each film or small
group of films is a development campaign that grows the observation and
rendering tools it needs. One idea may cross several scales and visual layers.
Simple captions belong in the film; technical provenance belongs beside it.
The desired appearance is lightly stylized, colorful, sculptural, with selective
focus / a tilt-shift impression where appropriate. The generated glacier contact
sheet establishes taste, not demonstrated rendering quality.

Nathan prefers astronomy for the first films, following The Wanderers; another
campaign is planning eclipse refinements. Nathan chose Bevy after discussing
Blender, Godot and Unity, citing prior Bevy experience and the value of its
strengths for a future procedural visual client. He accepted a first scene with
interactive camera/time controls and a short authored sequence. This is approval
of the direction, not of an implementation plan or final film.

These are backfilled conversation records. Earlier ideonomy passes explored
films as directed sequences and the progression from film to interactive client;
they were not recorded contemporaneously. The live rulings below have their own
passes.

## #1 [G1] — first campaign and engine

- Question: what is the bounded next deliverable after the technically landed
  Observation Series?
- Decision: The Planetarium, an in-repository Bevy client outside the simulation
  Cargo workspace; one selected astronomical scene, free camera/time controls,
  and a ten-second 4K authored sequence made from the same representation.
- Why: Nathan's explicit Bevy preference and accepted scene scope; decisions
  0022/0023 separate simulation from rendering; 0114 supplies a native driver
  precedent. The Observation Series retrospective identifies the static text
  renderer as the gap.
- Alternatives discarded: a universal film generator (premature and expressly
  unwanted); a Blender-only production path (does not serve the newly chosen
  interactive direction as directly); Godot/Unity as primary engines (Nathan
  selected Bevy). Blender remains a possible asset tool.
- Ideonomy passes / overturns: two live passes, no engine overturn. Combination
  of modularity with discovered versus invented content clarified that a film
  commissions shared observation/rendering pieces and authored shots. A second
  substitution pass across predictable versus stochastic motion and old versus
  emerging tools found no further material change: keep the production loop
  reproducible and the engine replaceable at the observation seam.
- Capture actions: draft spec; expand the existing
  `RENDER-observation-presentation` row rather than minting a duplicate renderer
  idea. Capture the broader film-to-client progression separately.

### Shared parts and authored choices

```text
                        Shared across films       Authored for a film
World observations      source/query adapters     seed, interval, target
Visual interpretation   materials, scene mapping  palette emphasis, focus
Audience direction      capture, camera controls shots, captions, pacing
```

The cells are different jobs. Reusing a material does not establish the truth of
a new observation, and a measured orbit does not choose an effective camera.

## #2 [Q] — how Bevy obtains positions and handles time

- Question: should Bevy evaluate serialized orbital elements, or ask the native
  Hornvale evaluator for the selected instant?
- Decision: use a small native source driver and an evaluated semantic query,
  separate from the elements-only system catalog. Bevy consumes serialized
  results through an isolated view crate. Separate simulation, presentation and
  rendering clocks; a direct seek and sequential playback must agree on the
  semantic observation and camera state.
- Why: 0114's native driver containment, 0117's rule against re-deriving world
  decisions, and the available ephemeris functions. The Wanderers and the system
  schema document an element-evaluation precedent, so the spec explicitly adds
  a distinct evaluated-query path without changing the element catalog.
- Alternatives discarded: a per-frame CLI subprocess (repeated initialization);
  duplicating astronomy in Bevy (additional implementation to keep in agreement);
  sampled positions stuffed into the existing element document (mixed meanings).
- Ideonomy passes / overturns: two passes applied to this question, no overturn.
  Substitution across predictable/stochastic playback and historical/current
  observations tested the loop observe → stage → capture → seek → observe.
  Dimension-identification then crossed periodic/continuous playback with
  source/view hierarchy: stale asynchronous replies and temporal render history
  must not redefine physical time. The draft already covers both; the latter
  pass produced no further material improvement.
- Capture actions: spec §§6–8; source/evaluator correspondence and direct-seek
  checks in §9. Schema-policy extension leads the G3 flags.

### Time and ownership check

```text
                         Source authority          Presentation authority
Repeated orbit query     same instant, same state  camera may differ
Continuous preview       requested instants       wall-time responsiveness
Fixed-frame film         explicit instant map     camera/caption playhead
Out-of-order seek        exact query, reply ID     reset temporal history
```

An absent cell here would be an unowned behavior. None needs Bevy to integrate
an alternate astronomical world.

## #3 [Q] — pilot scope and visual fidelity

- Question: what can the first beautiful scene show without requiring a complete
  terrestrial renderer or the eclipse campaign's unfinished work?
- Decision: one selected astronomical system, an anchor-centered composition,
  sourced moons and a wanderer point if no physical radius is available. Keep
  physical size/distance ratios within a shot; use cameras and cuts for clarity.
  Permit recorded cosmetic materials, not new world facts. Preserve the glacier
  study as an explicitly labeled art reference. The first native witness makes
  seed 42 a qualified inventory candidate, not a frozen final visual choice.
- Why: Nathan requested astronomy first and accepted the ten-second scene
  experiment. MoonSurface's supplied radius and descriptors, the registry's
  MAP-49/MAP-50 cautions about invented detail, and RENDER-sourced-effects make
  the truth/appearance boundary precedented.
- Alternatives discarded: glacier migration as this pilot (much larger source
  and visual scope); arbitrary sibling-world landscapes (not supplied); orbital
  compression/body enlargement (an unnecessary fidelity decision for this
  first study); depending on eclipse refinements (another campaign owns them).
- Ideonomy passes / overturns: two passes applied to this question, no overturn.
  Combination crossed shared/authored work with discovered/invented detail;
  dimension-identification checked close/global views against one-shot/recurring
  use. Local material variation belongs in appearance, while reusable view
  components still need per-film source qualification. The second pass found no
  further material change beyond the draft's explicit boundaries.
- Capture actions: spec §§4–5; RENDER-climate-migration-film and
  RENDER-film-to-client rows; eclipse and broader terrain follow-ups below.

## #4 [G2] — design self-review

- Decision: present the complete draft for G3; do not start implementation
  planning yet.
- Why: the spec covers the accepted product, ownership, source seam, clocks,
  capture, error behavior, visual acceptance, checks and explicit exclusions.
- Review corrections: preserved the generated concept as a labeled image;
  made the graphical-client deferral exception explicit; distinguished a new
  evaluated query from The Wanderers' elements-only policy; qualified seed 42
  with a fresh source export; made missing ffmpeg/capture fail the pilot rather
  than inherit the earlier assembler's package-only success behavior.
- Alternatives discarded: declaring source inspection a renderer witness;
  treating rough days/weeks estimates as a schedule; silently treating this
  ten-second study as a 30–60 second approved Observation Series episode.
- Ideonomy passes / overturns: G2 is consistency review; no additional pass.
  All live Q rulings above have non-zero passes.
- Capture actions: expanded the existing renderer registry row to `spec'd`
  with a draft-spec pointer; captured two raw future directions; kept the
  longer-term ideas outside the pilot's acceptance requirements.

## Follow-ups

- G3 review refinement: the shared visual libraries live under `clients/visual/`;
  Planetarium is the first application in that workspace. Further 2D/2.5D/3D
  renderers, gameplay adapters and additional library splits are demand-driven,
  not added to this pilot's implementation scope (ruling #5 below).

- Preserve the climate → plants → herbivores → peoples proposal for a later film
  campaign. Audit every causal link before claiming it; the user supplied it as
  a vision, not as an assertion of current end-to-end capability.
- Keep the eclipse campaign independent. A later film consumes its completed
  observation surface; The Planetarium does not amend eclipse physics.
- Potential later films include apparent retrograde motion, an eclipse, trade
  rerouting through a pass, river-course change and settlement response, and
  language/contact frontiers. These are candidate stories, not sourced claims
  or a scheduled release sequence.
- A future visual game may extend directed playback through time scrubbing,
  free camera and inspection to interaction. Revisit text/map/3D primacy only
  in that future scope; this observer does not decide the situated game's form.
- Return to the Observation Series' existing records and packaging code during
  planning. Retain useful provenance/verification behavior without forcing a
  ten-second visual study through the old episode definition.
- A rendered-image review must establish what actually works before extending
  the scope to terrestrial detail or a broader game.

## Verification notes

Read-only orientation used the repository's doctor and board in the preceding
conversation. Source inspection for this design used:

- `sed -n '1017,1040p' windows/scene/src/lib.rs`: `SystemScene` contains schema,
  seed, star, world, moons, stellar, wanderers; the system scene has elements,
  not evaluated positions.
- `rg -n '^pub .*fn|^pub struct' domains/astronomy/src/ephemeris.rs`: existing
  `stellar_positions_at`, `wanderer_position_at`, `stellar_illumination_at` and
  related observation evaluators. The anchor position helper is private.
- `sed -n '1285,1335p' windows/scene/src/lib.rs`: `MoonSurface` emits physical
  radius, albedo, seeded cratering/maria/tint descriptors, density and formation.
- `sed -n '4158,4198p' windows/worldgen/src/lib.rs` in the preceding turn:
  history-bake era construction fills `ice` with false. This is a source finding,
  not a measured climate-to-migration experiment.

No Bevy renderer, capture path, frame-time measurement, export-byte comparison,
or visual acceptance is claimed by this design work, except for the narrow
system-scene export comparison recorded below (which is not a Bevy export).

### Fresh native source witness

From this worktree's freshly built `target/debug/hornvale`, at the starting
source revision plus documentation changes:

```sh
target/debug/hornvale new --seed 42 --out /tmp/hornvale-planetarium-20260910/world.json
target/debug/hornvale scene system --world /tmp/hornvale-planetarium-20260910/world.json > /tmp/hornvale-planetarium-20260910/system-a.json
target/debug/hornvale scene system --world /tmp/hornvale-planetarium-20260910/world.json > /tmp/hornvale-planetarium-20260910/system-b.json
cmp /tmp/hornvale-planetarium-20260910/system-a.json /tmp/hornvale-planetarium-20260910/system-b.json
```

Actual output: `world of seed 42 ... (20109 facts; village: Doaba)`; parsed
`schema=scene/system/v1 seed=42 topology=single moons=2 wanderers=2`.
`cmp` exited 0. Top-level fields were
`schema,seed,star,world,moons,stellar,wanderers`.

### Worktree and checks

`make prewarm` completed with exit 0, wall 241.292 s, and added one measured row
to `docs/timings.md`; `git diff -- docs/timings.md` confirmed that exact change.
It is kept with the design, not erased as incidental state. The initial ledger
commit `d99f847e6` ran the normal pre-commit prose gate: 75 passed, 352 skipped.
Final design verification is recorded by the next commit's normal hook output.

The final design's first prose-gate attempt found the new spec absent from
`docs/audits/campaign-reconciliation.tsv` in
`campaign_reconciliation_covers_every_campaign_record`. Its actual missing set
contained only this spec. Source inspection of the population check and three
existing active-campaign rows established the required ten-column entry; the
repair adds this campaign as `active`, citing its spec and ledger, without
claiming implementation or approval. An earlier whitespace check also removed
Markdown hard-break spaces from the spec header. Neither repair changes a test.

The preserved concept image was generated with the built-in ImageGen tool in
the preceding conversation. Prompt summary: three cinematic stills of an
invented glacial world at orbital, regional and valley scales, high-color
terrain, aggregate flow ribbons, and the caption "As the ice advances, life
moves." It is an aesthetic reference only; no pixels assert Hornvale behavior.

## #5 [Q] — reusable library ownership beyond Planetarium

- Question: Nathan asked whether the Bevy/Hornvale connection belongs outside
  Planetarium-specific crates, given eventual 2D, 2.5D or 3D gameplay.
- Decision: refine §6 to one independent `clients/visual/` Cargo workspace with
  `hornvale-visual-source` and `hornvale-bevy-view` reusable libraries and the
  `hornvale-planetarium` application. Preserve the three structural roles from
  the previous draft; give them durable library ownership rather than an
  application-specific home. No package publication or separate repo is needed.
- Why: the accepted film-to-client direction, Nathan's review question,
  decisions 0022/0023's toolchain separation, and 0114/0115's native driver and
  display-mirror boundaries. Current `clients/game` already separates its native
  driver from a renderer whose manifest contains no simulation dependencies.
- Alternatives discarded: hiding shared code inside the Planetarium application
  and extracting it only after coupling has grown; a universal 2D/3D abstraction
  designed without another consumer; another repository/public package before
  release/version independence is needed; reusing an unrestricted scientific
  source as a situated game's authority.
- Ideonomy passes / overturns: two passes of cross-domain re-instantiation,
  using direction and complexity. The abstract structure is reusable facilities
  serving individual productions; a theater's stock equipment and production's
  staging expose the split between accumulated capabilities and per-show
  choices. Applied back to Hornvale, the inventory is:
  - shared source library: native lifetime, query/request handling;
  - shared Bevy library: presentation mechanisms, including astronomy modules;
  - production application: chosen observations, scene assembly and direction;
  - future game: its own allowed observations, inputs and consequences.
  The first pass refined ownership and made scope-bound caches explicit. The
  convergence pass compared a minimal shared library with a full generic engine
  framework and found no further material improvement: implement only the pilot's
  needed mechanisms and test independence from its application. One ownership
  refinement, no engine or pilot-scope overturn.
- Capture actions: spec §6 and cache/verification requirements updated; existing
  RENDER-film-to-client already captures the wider direction, so no duplicate
  registry row. G3 remains pending; this question is not spec approval.

## #6 [G3] — spec approved; proceed to planning

- Nathan: “LGTM; let's proceed to the plan!”
- Decision: the revised design, including reusable source/view libraries outside
  the Planetarium application, is approved for implementation planning.
- Scope: approved design boundaries remain in force; this is neither final
  visual acceptance, publication approval nor authorization to merge.
- Capture actions: spec status updated; detailed plan and four-stage tracker
  follow on this branch. No ideonomy pass applies to a direct human approval.

## #7 [Q] — pinned renderer and visible-first sequencing

- Question: which initial release and implementation order make the visual
  risk visible without building a general engine first?
- Decision: pin Bevy `=0.19.1`, retain Rust `1.96.1`, and build a real source-fed
  moving draft in stage 1. Start with one persistent renderer, an image target
  and one outstanding capture request; qualify the windowless path on the Mac,
  retaining a window-hosted image target if necessary.
- Why: the tagged Bevy Cargo.toml declares version 0.19.1 and rust-version
  1.95.0. The tagged externally-driven renderer example uses an image target
  and Screenshot::image; the headless-renderer example documents main/render
  world latency. These are source precedents, not an executed GPU witness.
  Hornvale's rust-toolchain.toml pins 1.96.1. Local system_profiler identifies
  an Apple M1 Max, 32 GPU cores, Metal 3; ffmpeg -version reports 8.1.1.
- Evidence: curl -fsSL fetched
  https://raw.githubusercontent.com/bevyengine/bevy/v0.19.1/Cargo.toml and
  examples/app/{headless_renderer,externally_driven_headless_renderer}.rs.
  rg returned version = 0.19.1, rust-version = 1.95.0 and Screenshot::image.
- Alternatives discarded: floating engine versions; writing custom GPU-copy
  machinery before qualifying the built-in screenshot path; finishing controls
  and provenance before seeing actual movement.
- Ideonomy passes / overturns: two organon-construction passes using notation,
  visibility and reversibility. First notation:
  source(tick) -> applied state -> ready assets -> submitted capture(frame) ->
  acknowledged image(frame) -> advance; reverse/seek -> discard history.
  Making the hidden render latency explicit moved frame identity and the early
  moving witness forward. Second pass inverted windowless/window-hosted and
  preview/final paths: output dimensions and observations stay fixed while the
  hosting choice stays reversible. No further material improvement; no engine
  overturn.
- Capture actions: these constraints go into the plan; multi-frame pipelining
  is deferred until measurements justify its complexity.

## #8 [Q] — physical anchor radius is absent

- Question: how can the pilot preserve globe/moon size ratios when the source
  has no anchor radius?
- Evidence: domains/astronomy/src/anchor.rs::Anchor has mass, orbit, year,
  rotation, obliquity and greenhouse_residual, with no physical radius.
  domains/terrain/src/channel.rs explicitly describes the terrain unit sphere
  and absent planet radius. Targeted Rust-source searches found no anchor/world
  radius definition. Moon radius is already derived from mass and density.
- Recommendation pending Nathan: include a simulation-side physical-radius
  prerequisite with a documented model and scientific justification.
- Alternatives: change to a moon-centered pilot; a cosmetic anchor sphere is
  rejected because it violates the approved physical-ratio requirement.
- Ideonomy passes / overturns: two notation/visibility/reversibility passes.
  Writing radius -> mesh scale -> body-size/distance ratio exposed a missing
  physical input, not a missing export. Moving the assumption from hidden
  renderer constant to explicit source model improves visibility; trying an
  isolated moon avoids the missing input but changes the approved subject.
  The convergence pass found no third option preserving both the subject and
  the existing source unchanged. This overturns the drafting assumption that
  the anchor's physical dimensions could simply be exported.
- Capture actions: async question sent during planning; dependent radius work
  remains unapproved while independent plan work continues.

## #9 [Q] — approved radius prerequisite and bounded source model

- Nathan's answer: “Include a physical-radius prerequisite”. The source-side
  work is authorized; the anchor-centered pilot remains the subject.
- Decision: add a derived `anchor_radius(EarthMasses)` observation returning
  Megameters, using a frozen subset of the Zeng author's 2019 Earth-like rocky
  mass–radius curve with declared piecewise-linear interpolation on [0.5,2]
  Earth masses. The model assumes 32.5% Fe / 67.5% MgSiO3; it does not claim
  Hornvale has simulated that composition. No new random draw or stored Anchor
  field is part of the plan. Radius is not fed into existing angular terrain
  or dynamics in this campaign.
- Why: the source's existing anchor mass range and the moon-radius precedent
  allow a narrow derived observation; the published table brackets the whole
  range. The attractive 2016 analytic shortcut explicitly covers 1–8 Earth
  masses, so it would extrapolate over half of this model's admitted interval.
- Evidence: retrieved
  https://lweb.cfa.harvard.edu/~lzeng/tables/massradiusEarthlikeRocky.txt;
  49 numerical rows, SHA-256
  dcc5080f2186983b7e36200373878dc06a8d8083ec21ce1c4f670659c0404b38.
  The author's planetmodels.html identifies the composition and Earth-unit
  axes. The plan freezes eight bracketing rows and cites the analytic paper's
  stated range. Python evaluation of the planned interpolation gives
  approximately 0.8178, 0.9980 and 1.2113 Earth radii at masses 0.5,1,2; this
  is a calculation over the retrieved data, not an executed Rust implementation.
- Alternatives discarded: constant density across the full mass range; analytic
  formula outside its published range; a new composition distribution and random
  stream; cosmetic renderer radius; moon-centered scope change.
- Ideonomy passes / overturns: two notation/visibility/reversibility passes.
  `source mass -> supported table bracket -> interpolation -> unit conversion
  -> emitted radius` makes both range and the physical assumption visible.
  Trying the simpler analytic notation exposed its unsupported lower interval.
  The convergence pass tested moving the model into a stored field versus a
  derived observation: derivation preserves the existing construction surface
  and keeps future model revision explicit. No further material improvement;
  analytic shortcut rejected before adoption.
- Capture actions: spec planning amendment and concrete Task 1 added; the
  existing astronomy model-card page will carry the implemented model. Broader
  composition, atmospheric envelopes and changed dynamics remain outside this
  prerequisite and are not promised future work.

## #10 [G4] — implementation-plan self-review

- Decision: the four-stage, nine-task plan matches the approved spec and the
  newly approved radius prerequisite; proceed under the standing SDD preference
  when execution begins. This turn prepares the plan, not an implementation.
- Review: mapped all spec sections to tasks; checked live source signatures and
  tagged Bevy facilities; checked exact clocks, scope/request identity, library
  dependency directions, actual GPU evidence, capture errors, package integrity,
  visual/performance acceptance and G6/publication boundaries.
- Corrections made during review: Task 1 is now an independently testable
  physical-radius implementation rather than an unresolved research instruction;
  shot sampling explicitly takes observed body positions; the unfinished tracker
  inherited at root is preserved verbatim, with a separate Planetarium section.
- Alternatives discarded: approving visual quality through tests alone; silently
  treating an inherited in-progress tracker as disposable; deferring the real
  moving-image checkpoint until production tooling is complete.
- Ideonomy passes / overturns: G4 uses spec self-review; the nontrivial planning
  choices are recorded with passes in #7–9. No new scope overturn.
- Capture actions: permanent plan, appended four-stage tracker and reconciliation
  links committed together; final prose-gate evidence is the commit-hook result.
  No build, test run of new code, render, performance target or merge is claimed.

## #11 [G5] — execution authorized and pre-flight completed

- Nathan: “Let's goooooooo!” — execute the approved plan.
- Task consistency and shared-interface pre-flight tables are recorded in this
  plan's SDD progress file. No scope conflict was found.
- Ruling: Task 1 records geometry conformance cases and source qualification;
  Task 2 adds their executable evaluated-geometry wrappers/tests. This follows
  Task 1's explicit “subsequent source implementation” file note and avoids
  pulling the evaluated scene into the radius-only increment.
- Stage 1 is in progress. G6 final visual/merge approval remains required.

## #12 [Q] — name the two astronomy reference frames

- Ruling: preserve both existing source conventions and explicitly convert
  calendar equatorial directions with `Rz(pi) * Rx(-obliquity)` into the
  native ephemeris system plane. Task 1 review correctly exposed an ambiguous
  equality; Task 2 must prove the converted equality, including nonzero phase.
  This is a coordinate conversion, not a change to simulation physics.
- Why: the native anchor is +X at phase zero, making its center sightline -X;
  Calendar's phase-zero solar direction is +X. A basis cannot erase that
  difference without naming which reference it maps from and to. Source
  anchors: ephemeris.rs `anchor_position_at`, calendar.rs `solar_equatorial`.
- Cost if wrong: body orientation and lighting disagree; executable geometry
  conformance in Task 2 must refuse that outcome before rendering.
- Dictionary of source convention × physical interpretation (combination,
  dictionary, materiality/source prompts):
  - Calendar solar direction: an informational equinox convention, not a
    second physical star position.
  - Ephemeris center sightline: physical anchor-to-orbital-center direction.
  - Converted solar direction: the calendar vector expressed in the native
    system frame; coherent and directly testable.
  - Resolved stellar direction: native direction to one actual modeled star;
    substituting the calendar center direction in a binary is incoherent.
  - Locked surface: fixed substellar body longitude under Calendar's model;
    this does not mean an inertially fixed body basis.
  - Basis conversion: a reversible mapping of coordinates, not an orbital
    modification or a cosmetic scene rotation.
- Ideonomy passes / overturns: two passes. The combination exposed two
  distinctions the old prose blurred: reference center versus resolved stars,
  and locked longitude versus inertial orientation. The convergence pass
  checked these definitions for overlap/circularity against the native source
  functions; no further material option. No change to approved fidelity.
- Alternatives discarded: changing genesis orbital phase; comparing vectors
  before frame conversion; rotating rendered lighting to disguise a mismatch.
- Capture actions: original implementer owns the prose fix and numerical
  evidence; independent scoped re-review precedes the evaluated source task.

## #13 [G5] — native Bevy render qualification

- Bevy 0.19.1 with the plan's explicit features and Rust 1.96.1 compiled.
  Optimized cold dev build: 21m05s, exit 0. The unmodified tagged upstream
  externally-driven headless-renderer example then exited 0 on Apple M1 Max
  through Metal, writing ten 500×500 PNGs.
- Visual inspection: screenshot0 is clear color only; screenshot9 shows the
  lit mesh, floor and shadow. The first-frame result is direct evidence for
  the plan's asset/pipeline/readback readiness requirement. This is a toolchain
  smoke witness, not a Hornvale moving-image or 4K acceptance result.
- Screenshot9 SHA-256:
  `3018399aa47e37bbab3c9a9a5776a1d1190253919903e9c587c5ec75bf10a720`.
  Runtime log: `/tmp/planetarium-bevy-runtime.log`; build log:
  `/tmp/planetarium-bevy-build.log`. Scratch manifest path is recorded in
  `/tmp/planetarium-bevy-qualification-path`; reproducible source:
  <https://raw.githubusercontent.com/bevyengine/bevy/v0.19.1/examples/app/externally_driven_headless_renderer.rs>.
- Read-only canonical prerequisites returned lefford, cargo present, X11
  1.8.4 and xkbcommon 1.5.0. This does not claim a Linux build or stage pass.
- Capture actions: carry readiness and offscreen ShadowLodOrigin findings into
  Task 3; keep production rendering and aesthetic evidence separate.

## Task 1 — complete

- Task 1: fix round 1/5 (1 addressed, 0 open — ambiguous frame equality;
  commits 63c45db62..7a158eb7b). Independent scoped re-review approved spec
  and quality, with no new breakage or out-of-scope observations.
- Task 1: complete (commits d61ad3f99..7a158eb7b, review clean).
- Evidence: 7 radius tests; astronomy 279 unit + 51 integration tests; existing
  seed-42 golden; local commit hook passed all four subfloor chunks. The
  prose fix hook passed 75/75. The controller retained the actual red/green,
  command, hash and gate outputs in the task report; no repeated suite was
  needed to resolve the review's execution-evidence qualification.
- Ruling and alternatives for the fix are captured in #12, with two ideonomy
  passes. The derived radius remains observation-only; evaluated source and
  moving Hornvale witness are still outstanding Stage 1 work.

## #14 [Q] — reject invalid native luminosity at extreme epochs

- Ruling: the evaluated scene refuses nonfinite or nonpositive native stellar
  luminosity/flux with an explicit invalid-query/model-validity error. Keep
  ordinary negative ticks and existing simulation producers unchanged. Do not
  clamp values or render negative luminosity as darkness.
- Why: `domains/astronomy/src/star.rs::luminosity_at` computes the existing
  unbounded linear brightening law. Task 2 validation found negative values
  at extreme pre-genesis i64 ticks. The plan already requires invalid-query
  errors and source limitations; accepting such a value would fabricate a
  plausible-looking observation. Positive output is not a claim that this
  simplified stellar model covers all stages of stellar evolution.
- Scale of source intervention (dimension-identification; scope/autonomy):
  1. Forward every number: widest apparent time scope, consumer silently owns
     invalid physics. Rejected.
  2. Refuse invalid native contributions at the observation producer: adopted;
     scope is the failed query, with an explicit reason and original ticks.
  3. Impose an arbitrary universal epoch cutoff: rejects otherwise usable
     observations without source evidence. Rejected.
  4. Clamp or replace contributions: autonomous presentation changes physical
     source values. Rejected.
  5. Replace stellar evolution: a new scientific model beyond this campaign.
- Ideonomy passes / overturns: two passes. Separating scope and intervention
  revealed that rejecting all negative ticks would unnecessarily narrow valid
  use; preserve them and test ordinary negative plus extreme invalid epochs.
  The convergence pass distinguished numerical admissibility from scientific
  lifecycle coverage, adding the model limitation above. No further material
  option; no authorized accuracy tradeoff is being introduced.
- Cost if wrong: callers may receive an explicit refusal for an epoch a future
  model can support; source model/version evolution can revise that boundary.
- Capture actions: Task 2 owns the refusal, regression evidence and emitted
  limitation note; independent review checks the implementation.

## Task 2 — complete

- Task 2: complete (commits 4e06e3349..048212519, review clean).
  Independent spec and quality review found no Critical, Important or Minor
  issue. Controller resolved cross-task qualifications against Task 1's clean
  review and #14; later GPU/interaction/capture criteria remain outstanding.
- Native geometry: 4 tests; evaluated scene: 1 unit + 3 integration; CLI: 2;
  source: 1 unit + 3 integration. Existing astronomy and scene-system goldens
  passed. Final normal hook: 1425/1425, 1393/1393, 1300/1300, 445/445;
  wall38.775s, rc0. Two earlier guard failures were fixed with a documented
  standard-library `Cell` lexicon exception and one justified topology-fixture
  build-site roster row; their cost rows remain in docs/timings.md.
- Current `luminosity_rel` was added to evaluated lights under the planned
  additional-contribution provision. This keeps epoch evolution in the source
  for off-anchor lighting. Native model validity/refusal ruling is #14, with
  two ideonomy passes; no new simulation model or accuracy tradeoff.
- Measured dirty-worktree source sample: 1000 queries, mean11.219µs,
  p5010.958µs, p9511.750µs; open24.301ms, initial width64 302.431ms.
  Includes parse/evaluate/quantize/serialize, excludes request construction and
  initial terrain from per-query cost. These are samples, not guarantees.
- Controller also compiled and ran a separate scratch consumer of Source,
  exporting 512×256 native tiles plus ticks0/25000/50000 with source revision
  048212519. Preserved inputs at
  `/Users/nathan/Downloads/Hornvale Planetarium/source-preview-048212519/`.
  World SHA256: `77168f2bc1a8db9c01b37b31b66ac4757e1133862f8249aa8d80bb0194285bf8`;
  initial JSON SHA256: `376767c88d474ab814ba55cd3b750f81980fd0f539ff9326360414f8185d2fa2`.
  The 18,005,513-byte initial document includes source sea level -1820.2915m;
  elevations are reference-datum values, not heights above that sea level.
  This is preserved source evidence, not a film or accepted visual.

## #15 [Q] — declare the globe's presentation elevation reference

- Ruling: use the emitted physical bulk radius as the spherical sea-surface
  reference for this view. Positive land displacement is exactly
  `(elevation_m - sea_level_m) / 1000` kilometres, without an exaggeration
  factor; ocean depth influences material color. This is a declared rendering
  reference convention, not a source-modeled geoid or interior/terrain coupling.
- Why: the approved plan requires physical radius, actual exported sea level
  and unexaggerated relief. Terrain emits reference-datum elevations and a
  separately derived sea threshold. Its raw zero is not sea level. No code
  should silently use that raw zero as the visible sea surface.
- Negated conventions × source relationship (discovery/invention and
  predictability prompts):

  ```text
  convention                     radius origin       altitude treatment
  emitted radius at sea          native fixed        native difference, 1:1
  arbitrary display globe size   invented/tunable    relative only; rejected
  raw elevation means sea height native fixed        wrong datum; rejected
  flattened land                 native fixed        discards relief; rejected
  exaggerated relief             native fixed        invented multiplier; rejected
  new geoid/interior coupling     new source model    beyond this view's scope
  ```

- Ideonomy passes / overturns: two passes. Negating the nominal reference
  exposed a false equivalence between isostatic zero and sea level; naming the
  rendering convention keeps that difference explicit. The convergence pass
  checked repeatability and ownership: the source radius/altitudes remain fixed,
  while presentation never claims a new coupled shape model. No further option
  improved the approved bounded spherical view.
- Cost if wrong: the rendered reference surface needs rebinding when a native
  geoid/shape contract exists. No simulation/save quantity is changed; the
  current convention and limitation must appear in the production record.
- Capture actions: Task 3 documents and tests this mapping. G6 carries the
  convention alongside the radius model and other source limitations.

## #16 [Q] — separate stellar body illumination from atmosphere lighting

- Ruling: qualify one point light at each actual emitted stellar position for
  solid-body PBR, deriving intensity from current emitted luminosity with the
  declared render-unit conversion. Feed the built-in atmosphere one separate
  directional light per star using its emitted anchor direction/flux; isolate
  that directional light from solid surfaces. Unsupported eclipse shadows stay
  disabled. Disable the atmosphere's apparent stellar disk because the source
  does not emit a stellar radius.
- Evidence: the installed pinned `bevy_pbr-0.19.1` shader
  `src/atmosphere/functions.wgsl`, inspected with `rg` and `sed`, loops every
  view directional light in `sample_local_inscattering` (line 216),
  `sample_sun_radiance` (248), and ground reflection (491), without consulting
  RenderLayers. The sun-radiance path emits a disk when its angular-size and
  intensity settings are positive. This is code inspection; GPU validation
  remains Task 3 work, not an inferred success.
- Why: per-body directional lights could give each moon its actual incident
  direction and flux, but the atmosphere would add those duplicate feeds.
  Point transport preserves the source-position relationship for solid bodies
  and leaves only the intended directional feeds in the atmosphere shader.
- Illumination routing as a mixing graph (cross-domain re-instantiation;
  polarity and hierarchy):

  ```text
  musical source -> duplicate open buses -> unintended amplification [negative]
  musical source -> dedicated receiver buses -> controlled sum [positive]
  native star -> actual-position point -> all solid bodies [inverse-square]
  native star -> anchor directional -> atmosphere [declared approximation]
  apparent-size default -> invented stellar disk [reject]
  source revision -> shared binding -> both light treatments [one authority]
  ```

- Ideonomy passes / overturns: two passes. The audio-routing analogy exposed
  that “isolated objects” do not imply isolated effects when a downstream
  consumer ignores the routing filter. The convergence pass checked the shared
  parent/independent receiver graph and its opposite (one uniform light for
  everything); this retained the split and prompted the explicit disk-default
  check. No additional source model or new rendering subsystem is warranted.
- Alternatives discarded: one anchor light for all moons loses their observed
  illumination geometry; repeated per-body directional lights contaminate the
  atmosphere; an immediate custom atmosphere shader is unnecessary before
  qualifying the existing point-plus-directional arrangement.
- Cost if wrong: replace the presentation light routing after measured GPU
  evidence. Actual source positions, luminosity and model contracts stay intact.
  The bounded atmosphere remains a cosmetic treatment, not a simulated profile.
- Capture actions: Task 3 verifies point range/culling, scale conversion and
  actual anchor/moon shading; the production record distinguishes solid-body
  transport from the directional atmosphere treatment and records disk absence.

## Task 3 — review and fix round 1

- Independent review of `7d3a5156d..a1a9a3089` found three Important issues:
  incomplete binary light inventories can reach a renderer panic; finite but
  unsupported radius/elevation values lack geometry bounds; quarter-pixel tests
  omit the actual f32 camera/body projection. All three returned to the original
  implementer for fix round 1 and scoped behavioral verification. No finding is
  dismissed on the basis of the successful native visual witness.
- Deferred Minor: initial-document validation currently validates only part of
  the nested static catalog. Carry to Task 4's reset/identity work and Task 8's
  public schema documentation; the final review must decide whether additional
  validation is needed. This is separate from the blocking light inventory bug.
- Deferred Minor: the new 629-line renderer combines construction, application
  and readiness/capture. Tasks 4 and 6 should separate the lifecycle and capture
  responsibilities they extend; carry the observation into their briefs and
  final review, rather than start another implementation wave during this fix.
- Ruling: Task 3's persistent offscreen Bevy app qualifies its GPU bootstrap;
  the visible inspection window and controls remain required Task 5 deliverables.
  Task 3 explicitly starts synchronous exact observation and draft capture;
  Task 5 owns orbit/pan/dolly, time controls and film/inspection switching.
  This resolves the review's scope ambiguity without dropping interactive work.
- Maturity/complexity spectrum (substitution, age and complexity prompts):

  ```text
  isolated illustrative frame ... persistent native GPU scene ... full interactive film client
                           Task3 occupies this middle band
  substitute a visible host window: adds UI hosting, not source-motion evidence
  substitute an earlier hand-authored image: loses the required native/GPU witness
  substitute final control/package maturity now: duplicates Tasks4–7 responsibilities
  ```

  Two ideonomy passes: the first separated a program's visible window from its
  persistent rendering behavior; the convergence pass checked which later
  obligations would disappear under that substitution. None may disappear, so
  the original task allocation stands. Cost if wrong: deliver the visible host
  earlier; no data/model contract changes and no final acceptance is granted.
- Unchanged-source qualifications resolved: Task 2's native source and CLI
  correspondence were independently reviewed at `048212519`; root Cargo.toml
  currently excludes `clients/visual`. A fresh locked cargo-metadata traversal
  at the Task 3 head found view closure 448 packages with no Hornvale package
  except the view itself, and source closure 45 packages with no Bevy or
  Planetarium dependency, including resolved normal/build/dev edges. The
  full metadata is retained in task scratch; Task 8 still owes the durable guard.
- Root independently checked all 60 PNG hashes/dimensions/order, exact inner
  and outer ticks, unchanged source binding, constant original caption bands,
  video/still hashes and actual 3840×2160 still dimensions. Viewed every decoded
  frame in ordered contact sheets and phone-size images. Moon translation,
  world rotation and persistent caption are present. Live desktop playback is
  not yet verified: Computer Use access was granted, but QuickTime input
  remains unreliable; preserve this limitation through the controls review.
- Actual draft artifacts remain in Downloads/Hornvale Planetarium, with the
  dirty renderer and local Georgia font documented. They are early visual
  evidence; neither final visual acceptance nor a clean final package is claimed.

## #17 [Q] — enforce the measured orbital camera envelope

- Ruling: this pilot supports camera-center distances at least twice each
  body's outer physical radius, including maximum positive terrain height above
  the emitted sea reference. Reject nearer views before publishing a scene;
  retain the source sizes/distances. Task 5's authored poses and dolly controls
  must use the same boundary. Near-surface viewing needs subsequent precision
  work and is not silently claimed by this orbital pilot.
- Evidence: Task 3 fix-round probing used the actual f32 camera/body/projection
  path. The saved focused test log `/tmp/planetarium-review-close-camera2.log`
  reports `projection error 0,69120; distance=1000000 fov=0.005` for a camera
  0.2km above a 999999.8km body, and exits with the assertion failed. The
  implementer retained visible reference points beyond the near clip. The
  previous center-only test could not detect that local/body cancellation.
  Boundary witnesses for the enforced envelope are part of the ongoing fix;
  this ruling does not predict their result.
- Why: the approved spec requires an explicit bounded camera range and allows
  a rejected unsupported view. It does not require orbital and near-surface
  precision to be solved in one pilot. Existing captured compositions fit this
  orbital range; source geometry is not a tuning parameter.
- Conditioning grid (abstraction lift, direction and naturalness): the concrete
  problem is subtracting/rotating body-scale f32 values near a tiny visible
  separation. Its abstract form is a representation losing a small difference
  between large quantities, familiar in numerical measurement generally.

  ```text
  camera direction   current source-preserving f32   new representation       altered physical size
  approaching surface measured failure; reject       future precision work    false geometry; reject
  bounded orbital     qualify boundary witnesses     unnecessary if passes    false geometry; reject
  retreating farther  qualify range/projection       future larger envelope   false geometry; reject
  ```

- Ideonomy passes / overturns: two passes. The first distinguished source
  facts from their synthetic coordinate representation and identified the
  missing near-surface representation as future work. The convergence pass
  followed an inward/outward camera movement through the boundary: consistent
  rejection must also constrain interactive dolly and authored shots, not only
  direct renderer calls. No additional physical model or size distortion helps.
- Alternatives discarded: ignore the failed visible sample; change physical
  dimensions; or expand this task into universal near-surface coordinates.
  A measured supported envelope is the smallest option consistent with the spec.
- Cost if wrong: expand the presentation coordinate implementation and its
  supported range in a later refinement; no simulation/save data is changed.
  G6 must disclose this camera limit alongside the other pilot limitations.


## Task 3 — complete; Stage 1 canonical result pending

- Implementation range `7d3a5156d..64c2575f3`; one fix round addressed all three
  Important findings. Independent scoped review approved complete stellar-light
  inventories, bounded geometry and the real f32 camera/body projection test.
- Final focused tests: 25 view and 4 source tests passed, fmt/clippy passed.
  Normal commit hook passed in 43.352s (`/tmp/planetarium-review-commit2.log`).
  The first hook correctly refused two lexical-token additions in a comment
  and test name; rewording removed them without a waiver or guard change.
- The supported orbital test sampled 6,342 visible points; worst measured error
  was 0.101748006 pixels. This is measured finite coverage, not a universal proof.
  Existing draft captures satisfy the range and retain their original hashes
  and dirty-build provenance. No recapture is claimed for this validation fix.
- Early visual inspection includes the full-resolution still, phone review and
  all 60 decoded moving frames in order. Live desktop video playback remains
  unverified because Computer Use input did not reliably operate QuickTime.
  Task 5 still owes the actual interactive window and controls witness.
- The two earlier Minors remain assigned: consumed initial catalog validation
  in Task 4/8, renderer responsibility split while extending lifecycle/capture
  in Task 4/6. No additional waiver or new visual approval is implied.

## #18 [Q] — Task 4 quantization and empty-catalog meanings

- Ruling: preserve the approved signed-offset rounding formula. The film samples
  presentation frames over a half-open interval; rounded simulation ticks can
  repeat, including the end tick. Do not clamp ticks merely to make an incorrect
  plan sentence true. Spec section 7 separates those clocks; the plan is corrected.
- Executed integer probe on 2026-09-10: for frame 299 of 300, start=0/end=1
  gives tick=1; start=0/end=-1 gives tick=-1. The same formula gives -3 for
  frame 2 of 3 over 0 to -5. These results disprove the prior unconditional
  simulation-endpoint exclusion while preserving its signed-offset test.
- Empty-catalog ruling: test zero rendered entities at reset, before a new reply,
  and an optional moon/wanderer inventory becoming empty in a valid new source.
  `documents.rs` requires an anchor and nonempty stellar illumination; a fake
  anchorless native astronomy document would exercise a different protocol.
  Reset must remove old selection, material bindings and entities even if the
  next source reuses IDs. A full observation may then repopulate the new scene.
- Ideonomy passes / overturns: two dimension-identification passes, using purpose
  and side-effects. First map: distinguish time sampling from time quantization,
  and source completeness from displayed-state lifetime. The convergence pass
  checks reverse intervals and reused IDs: neither should inherit old state or
  require altered source facts. It adds regressions at these borders but no new
  clock or schema. Both plan wording defects were overturned.

  ```text
  PURPOSE: exact time                           PURPOSE: complete source
  frame sampling -- rounding boundary          native inventory -- reset boundary
        |                  |                         |                  |
  no frame at 10s     repeated/end ticks       anchor required     no displayed entities
  side effect: reject a valid rounded tick     side effect: stale bodies if only replacing
               if these regions are merged                 matching IDs
  ```

- Alternatives rejected: endpoint clamping (changes the documented mapping),
  an anchorless source schema (unneeded protocol expansion), or testing only
  mirror JSON while leaving ECS entities alive (misses the reset requirement).
- Capture: plan and Task 4 brief corrected before dispatch; the implementer must
  record real boundary/reset tests, pending state and worker failure behavior.


## Stage 1 — canonical request submitted

- Absorbed main `214c1b67d52ff36165881f98ee1315ba875dee1d` in merge
  `0268062a71db95c354c469e9ce620f4378198e66`. Actual conflicts were limited
  to type-audit-report.md and plumb-roster.md. Regenerated both using their own
  `cargo run --manifest-path tools/{type-audit,plumb}/Cargo.toml -- report`
  authors and staged those results; no hand-selected generated inventory.
  Normal merge commit hook passed all four subfloor chunks, 182.889s, rc=0.
- Controller documentation commit `cf982ae363e8515e90e09ba8c6e5d4a3bd03a5d5`
  passed all 75 prose-subject tests. Pushed that exact branch tip normally.
- `make sluice-stage BRANCH=campaign/the-planetarium
  REF=cf982ae363e8515e90e09ba8c6e5d4a3bd03a5d5` returned request
  `req-cf982ae363e8-20260910T192747Z`, kind=stage, host=lefford.
  Submission log: `/tmp/planetarium-stage1-submit.log`. No report is claimed yet.
- Task 4 development proceeds against the reviewed source/view foundation while
  the queue works on the fixed Stage 1 SHA. Stage 1 remains In Progress until
  its actual report is inspected; a later green development test cannot replace it.


## #19 [Q] — freeze the final film against the actual capture revision

- Ruling: Task 5 commits a complete authored film referring to an already-existing
  source revision. Once implementation is committed and the final capture tree
  is clean, explicitly freeze a package-local copy against that actual full HEAD.
  Keep world bytes, source/scope IDs, ticks, shots, appearance and assets fixed;
  record any intentional change. Query fresh initial/observation documents with
  that binding. Never relabel an earlier capture or silently rewrite its records.
- Why: Source::open currently validates the caller's revision format and stores
  it; it does not attest which code was compiled. A final package must record the
  actual clean build/source revision, executable hash and exact film bytes.
  The capture definition is an artifact, so it can live beside its output outside
  Git while the reusable authored direction stays committed. No commit needs to
  embed its own SHA. Spec sections 4/8 and the plan's clean final capture rule
  remain intact; this sharpens the controller workflow, not the source protocol.
- Source/visibility chart (combination):

  ```text
  origin                 committed authoring record      package-visible evidence
  authored direction     complete film at existing SHA  exact copied shots/settings
  actual clean build     source code and lockfile        full HEAD and executable hash
  evaluated observation  producer implementation        freshly queried binding/time JSON
  old draft              old provenance retained        never relabeled as final output
  ```

- Ideonomy passes / overturns: two. First combined where a value originates with
  where its claim is visible: authoring data and runtime evidence have different
  recording moments. Second combined content preservation with identity changes:
  refreshing a revision requires fresh queries and re-verification, not a search
  and replace in old observations. No additional producer abstraction or schema
  is needed; the implementation already accepts explicit film paths and bindings.
- Alternatives rejected: stamping an arbitrary old revision on a new build,
  weakening film-binding validation, leaving final capture dirty, or adding an
  unnecessary source-subtree attestation system. Final capture must instead use
  exact, reviewable data from a clean existing revision.
- Capture actions: Task 7/9 package instructions clarified. Any repin that changes
  actual semantic observations or shot inputs triggers normal qualification and
  visual review; the workflow does not predict that those outputs stay unchanged.


## Stage 1 — complete, canonical report inspected

- Request `req-cf982ae363e8-20260910T192747Z` reported green in 1414s.
  Actual chamber log is `sluice-cf982ae363e8-20260910T195743Z`, retrieved with
  `make sluice-log JOB=sluice-cf982ae363e8-20260910T195743Z`; the request ID
  itself is not the log basename. Local saved log: `/tmp/planetarium-stage1-report.log`.
- Every stage phase exited zero: artifacts 279.378s, outboard 96.258s,
  gate 823.099s, clients 197.410s. Actual merge product
  `94fbd6dacb573f6edbbf3841e6d714fb669787e7`, final authored tree
  `0e5501e62acb79b824914aaf635578ec49ca5e2a`.
- Terminal report explicitly says nothing pushed; main unchanged at
  `3aff906d4c52d9d3a4fa0551fec5f84248139e38`, kind=stage, rc=0.
  This checks the Stage 1 SHA, not subsequent Task 4 changes. New visual client
  CPU integration into the canonical client phase is still Task 8; current
  view/source tests were run locally under their separate workspace.
- Existing remote log warnings concern Git garbage collection and Deno bundle's
  experimental status. No cleanup or unrelated tool-policy change was attempted.
  No heavy tier, census or final visual approval is claimed by this stage result.

## Task 4 — implementation reviewed; fix round 1

- Implemented in `5d2225b896ced4b4b531dca41eb6c72e5015baa8`; 37 view,
  4 source and 5 app tests passed. Normal hook passed all four subfloor chunks
  in 97.509s. Its initial lexical-token refusal was corrected without a waiver.
- Independent review requires one correction: a contradictory duplicate of
  committed A must error even while B is pending, preserving both committed A
  and pending B. The check currently sits only in the no-pending branch.
  Original implementer is fixing it with a compiling behavioral regression.
- The small Minor is included in the same fix round: queue a scene application,
  reset before its system runs, then verify it cannot repopulate the empty scene.
  This tests correct existing cleanup behavior; it is not a new protocol or waiver.
- Task 3's consumed static catalog validation and lifecycle split Minors are
  addressed by Task 4 according to independent review. Remaining readback/setup
  separation and any proven failure recovery belong to Task 6.
- Measured source-worker round trips, 1000 exact queries: p50 17.292us,
  p95 36.041us, max 148.458us; initial world plus512-wide document 746.706ms.
  Dirty implementation measurement on the fixed seed42 world, declared source
  revision cf982ae36; no GPU-rate claim. Bounded scheduling test demonstrates
  active A plus1000 queued requests -> latest999 and exactly two source calls.
- Bridge tests were initially written alongside implementation. This deviation
  is explicit: a compiling mutation retaining the first queued request failed
  (actual0, expected999), then restoration passed. A first mutation exposed a
  test-cleanup deadlock; assertions were moved after releasing the fake worker,
  and the mutation then failed normally. No sleep-based ordering or bypass.


## Task 4 — complete after one fix round

- Fix `231ca8aefe20c47d937bb03de0b0f04e0cb4e1c9` moves the committed reply
  conflict check ahead of pending-request classification. Compiling regression
  now verifies an error preserves committed A and pending B. A queued-scene/reset
  test also proves the deferred update cannot repopulate the cleared scene.
- Independent scoped review approved both findings with no new findings.
  39 view tests passed after the fix, fmt/clippy passed; normal hook44.473s,
  all four chunks green. Earlier4source/5app tests passed; the fix touches no
  bridge/source implementation. No extra gate rerun was substituted for review.
- Reusable handoff: FilmClock/PresentationTimeline, ObservationState and bounded
  Bridge, SceneCatalog/SceneTarget with real assets/selection/reset ownership.
  Task5 now owns the visible window, playback pacing, authored shots/captions,
  controls and their actual moving visual evidence. Task6 retains explicit capture
  completion and failure recovery responsibilities. No final visual approval yet.


## Task 5 — working inspection foundation and visual iteration

- Foundation committed as `b9364d631a73827143854d41d82841b29ec320d0`.
  Normal hook passed all four subfloor chunks in67.478s. Its initial refusal
  required a structural claim-shape declaration on the frame-partition test;
  the declaration and focused guard were corrected before the successful hook.
  Task5 is still in progress, with independent task review and Stage2 pending.
- Actual native eclipse export for the candidate inclusive interval0..3600ticks
  (CLI standard-day arguments0..0.036) returned `scene/eclipses/v2`, seed42,
  events[]. World SHA remains77168f2bc1a8db9c01b37b31b66ac4757e1133862f8249aa8d80bb0194285bf8.
  A separate native probe sampled all300 frames plus endpoint, including native
  solar angular diameter and eclipse thresholds. Moon0 minimum solar separation
  50.514136657deg exceeds its maximum native threshold1.693434031deg; moon1
  minimum solar92.810519168deg and anti-solar86.845548279deg exceed1.443120246deg.
  All301 application astronomy payloads, ticks and bindings agreed with the
  independent probe. This is selected-interval avoidance, not eclipse validation
  or permission to invent physical stellar spheres/shadows.
- Native evidence, reproducer, hashes and executable provenance live at
  `/Users/nathan/Downloads/Hornvale Planetarium/task5-native-avoidance-01/`.
  Native paths were clean at1e11630202fd5db21a54944ab5f460afc28889ff;
  client implementation was in progress. The helper's initial compile error
  concerned SceneError conversion and was resolved locally; no native code changed.
- Real Computer Use review of the bundled app found and verified corrections
  for Retina startup dimensions, quick scrub clicks, extreme dolly input and
  inspection text/caption overlap. Actual target1920x1080 atscale2 was observed.
  Body picking/focus, paused camera movement, authored reset, exact frame step,
  play/pause, reverse and film toggle worked. Native window resize to3024x1832
  and back retained readable controls. Approximately100fps on the overlay is
  preliminary observation; Task9 still owes formal performance measurements.
- Mouse dragging remains a live-input evidence limitation: the Computer Use
  drag operation leaves the visible pointer at its start in two input versions.
  Click and wheel delivery work. Orbit/pan geometry and pointer ownership have
  app tests; they are not relabeled manually demonstrated drag behavior.
- Preserved actual interaction evidence is in
  `/Users/nathan/Downloads/Hornvale Planetarium/task5-inspection-02/`:
  915 acknowledged PNGs and915 state-at-request records, all images checked
  against declared dimensions, strictly increasing timestamps and no error records.
  The100.727323708s sampled span includes798 images at1920x1080 and117 at3024x1832.
  `controls.mp4` preserves variable cadence and aspect ratio,100.88s including
  final hold; SHAeaab3e7e362be0cd84cfa0be354edaaf939bacebc08b493275c3511d1208d132.
  Recording was stopped with no pending acknowledgement before clean window exit.
- QuickTime input/playback now works through its actual file dialog. The control
  movie played through100.88s, with intermediate changing states observed. Earlier
  playback difficulty is resolved. A transient garbled player preview was checked
  against both originalPNG and independently decodedMP4; both files were intact.
  Visual alarms must be confirmed against actual artifact bytes before changing
  renderer code, as the earlier caption-preview false alarm already established.
- Current appearance remains under review. Softer water highlights help, but
  the first cloud-fraction-driven cosmetic prototype was too sharp and busy and
  was rejected. Source-driven static cloud appearance and a more revealing camera
  angle are being refined; physical scale and source time remain unchanged.
  This checkpoint approves no final visual result, package or merge.
