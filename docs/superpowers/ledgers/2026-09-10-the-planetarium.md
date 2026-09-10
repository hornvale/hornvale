# The Planetarium — decision ledger

Status: design in progress; G3 has not been presented or approved.
Branch: `campaign/the-planetarium`.
Starting revision: `b6b374f6d` (full revision recorded in the verification notes).

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

## Follow-ups

- Preserve the climate → plants → herbivores → peoples proposal for a later film
  campaign. Audit every causal link before claiming it; the user supplied it as
  a vision, not as an assertion of current end-to-end capability.
- Keep the eclipse campaign independent. A later film consumes its completed
  observation surface; The Planetarium does not amend eclipse physics.
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
or visual acceptance is claimed by this design work.
