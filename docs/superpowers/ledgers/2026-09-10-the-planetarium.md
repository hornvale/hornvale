# The Planetarium — decision ledger

Status: design ready for G3 presentation; G3 has not been approved.
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
