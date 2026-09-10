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
