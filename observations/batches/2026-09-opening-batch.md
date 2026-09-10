# 2026-09 opening observation batch

Batch status: **draft internal resequence**. The opening run has one
end-to-end package, `HV-009`. It is a draft candidate, not a reviewed,
approved, or published package. The numbered `HV-001` through `HV-008`
underworld records remain the internal spatial production-path pilot; they
are not public opening records.

## Truthful batch amendment

The prior eight-record public opening is withdrawn. The capability matrix
supports exactly one opening-scale observation path today: the witnessed
stellar-neighborhood path in `HV-009`. Reusing it under eight captions or
relabeling the underworld pilot as astronomical-to-surface coverage would not
create eight independent atlas cells.

`HV-009` remains its stable record identity while appearing first in the
release order. No public package has been published, so this changes no public
identifier. The next opening records remain unassigned until the precise
capability boundaries below are implemented and witnessed. This is the
smallest amendment that preserves the approved astronomical-to-surface spine
without inventing evidence.

## Active opening record

| Release order | Episode | Object | Scale | Primary axis | Count unit | Grammar | Evidence | Package |
|---:|---|---|---|---|---|---|---|---|
| 1 | HV-009 | notable stellar neighborhood | astronomical neighborhood | apparent brightness and sky position | stars | spatial | draft | draft |

`HV-009` uses the declared fixture world and its sole accepted command:

```text
cargo run -p hornvale -- scene neighbors --world cli/tests/fixtures/world-seed-42.json
```

Its exporter preserves the complete `scene/neighbors/v1` document in
`spatial.readout`; the Atlas observation preview plots only its supplied RA,
declination, brightness, and magnitude fields. The committed first packet is
`observations/fixtures/HV-009/expected-frame-000.json`; two 900-packet
exports have the recorded sequence SHA-256
`8e45bc6d6fbd9a04607b387d72d82557fb46d404520f10a2b86b03e362ad1c6b`.

The candidate was generated on 2026-09-09. Its earliest review or publication
date is 2026-09-16, satisfying the seven-day buffer. Its manifest, caption,
evidence status, and editorial status all remain `draft`; approval is `null`.

## Deferred astronomical-to-surface cells

These are capability requirements, not episode records. Each row keeps one
object, one scale, and one primary axis; none assigns an unsupported public
claim or an episode identifier.

| Release position | Object | Scale | Primary axis | Unit | Current boundary | Required capability work |
|---:|---|---|---|---|---|---|
| 2 | primary system | system | orbital arrangement | bodies and orbital elements | `needs_renderer` | Add a spatial orrery grammar for authoritative `scene/system/v1`; then bind that scene through the observation exporter. |
| 3 | planetary surface | world | spatial arrangement | map tiles | `needs_observation_surface` | Bind the exercised `scene/tiles/v1` map path into `observation/frame/v1`; Atlas already parses its map grammar. |
| 4 | terrain elevation field | field | elevation | meters | `needs_observation_surface` | Select the producer's `elevation_m` field through an observation adapter; retain its meters unit and existing Atlas field grammar. |
| 5 | habitat biome classification | habitat | spatial distribution | biome tiles | `needs_observation_surface` | Select the producer's biome tiles and legend through an observation adapter; retain the supplied categorical legend. |
| 6 | geographic regional tile | region | spatial arrangement | sampled tile nodes | `needs_renderer` | Add a regional-tile grammar for authoritative `scene/tiles-region/v1`; then add its observation adapter. |

## Deferred social and non-spatial cells

These requirements preserve the distinctions the atlas requires. `Unassessed`
means the opening capability matrix records no authoritative producer and
compatible Atlas grammar for that exact cell; a capability audit must identify
the first implementation boundary before the state can truthfully be narrowed
to simulation, observation surface, or renderer.

| Object | Scale | Primary axis | Unit or observable | Current boundary | Internal requirement |
|---|---|---|---|---|---|
| settlement | settlement | spatial arrangement | settlements | `needs_renderer` | The realized `settlement-map` has no compatible Atlas grammar; add that grammar before an observation adapter. |
| occupation | occupation | persistence or duration | occupation intervals | unassessed | Audit an authoritative occupation interval producer and a temporal grammar; do not substitute settlement or population. |
| population | population | quantity | persons | unassessed | Audit an authoritative population-count producer and a quantity/distribution grammar; do not substitute settlement or occupation. |
| individual | individual | viewpoint or resolution | one projected individual | unassessed | Audit an individual witness and close-reading grammar; do not project an aggregate record as an individual. |
| chambers | depth band | transition | chamber changes across world time | `needs_observation_surface` and `needs_renderer` | Export a bounded time series, then add a temporal grammar; repeated static packets are not a transition witness. |
| cave systems | depth band | relation | typed junction edges and components | `needs_observation_surface` and `needs_renderer` | Export selected graph structure, then add a relational grammar; aggregate junction facts are not a relational film. |
| chamber run | cave system | viewpoint or resolution | one selected run's floors and material sequence | `needs_observation_surface` and `needs_renderer` | Export a selected run independently, then add a close-reading grammar; three printed examples are not a selected witness. |

## Internal-pilot boundary

The `HV-001` through `HV-008` manifests and caption drafts remain draft
underworld spatial records. Their command, fixtures, deterministic export, and
renderer tests still exercise the production path. They are retained as an
internal pilot only and are not counted as opening coverage, a reserve, or
publicly approved work.

## Review boundary

- No status changed to `reviewed`, `approved`, or `published`.
- No video, caption package, or social-network action is authorized by this
  record.
- The public opening needs a follow-up implementation task: first bind an
  existing `scene/tiles/v1` surface through the exporter, or implement a
  system renderer and bind its scene through the exporter, before another
  independently supported opening record can be added.
