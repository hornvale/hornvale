# 2026-09 opening observation batch

Batch status: **draft internal pilot**. Seven new evidence statements have
been checked against the current producer readout; HV-001 retains its earlier
draft wording for later editorial tightening. No exact video/copy package has
been approved. The seven-approved-package reserve is pending Nathan's manual
review. Nothing in this record authorizes publication.

## Realized surface and scope

This pilot uses the one observation source and renderer path that exists now:

```text
cargo run -p hornvale -- underworld --seed 42
  → observation/frame/v1 spatial packets
  → atlas spatial observation renderer
```

At revision `0d7067a0f20aafbe88fb67358537f56963fba5b0`, the producer emitted
5,910 bytes with SHA-256
`70328bb4f83aa088a7ec88e398ddbf8ab20049f2ded24d51b422880692238c01`.
Those bytes matched the committed HV-001 frame fixture exactly. HV-001 retains
its original producer revision
`ed8e6f3369f2b45097d702a5c28cc84f1ae35f56`; HV-002 through HV-008 pin the
revision at which this batch review repeated the producer command.

The eight records occupy distinct object/scale/axis cells supported by that
readout. They do not pretend the current exporter can carry another producer,
and they do not claim four visual grammars. Reusing one readout makes this a
pipeline and editorial pilot, not a claim of broad atlas coverage.

## Episode matrix

| Episode | Object | Scale | Axis | Count unit | Grammar | Evidence | Package |
|---|---|---|---|---|---|---|---|
| HV-001 | geography | world | spatial arrangement | chambers | spatial | draft | draft |
| HV-002 | entrances | world | quantity | entrances | spatial | reviewed | draft |
| HV-003 | cave systems | world | quantity | cave systems | spatial | reviewed | draft |
| HV-004 | chambers | depth band | distribution | chambers | spatial | reviewed | draft |
| HV-005 | chambers | rock layer | distribution | chambers | spatial | reviewed | draft |
| HV-006 | chambers | world | provenance | chambers | spatial | reviewed | draft |
| HV-007 | cave systems | depth band | topology or connectivity | cave systems | spatial | reviewed | draft |
| HV-008 | chamber run | cave system | viewpoint or resolution | floors | spatial | reviewed | draft |

Every manifest names one value for object, scale, axis, count unit, visual
grammar, and observation sentence. Every source command is the exact current
repository-root command accepted by the exporter.

## Reproduction record

Each manifest requests 120 packets at 30 frames per second. The batch was
exported from clean per-episode directories. `source_digest` was
`fnv1a64:5344fcc22e8d960d` for every episode because every packet observes the
same producer bytes. The atlas fixture's tested viewports are 390×844 (phone)
and 1440×900 (laptop).

The sequence checksum is SHA-256 over the exact bytes of `frame-000.json`
through `frame-119.json`, concatenated in lexical order.

| Episode | Frames | Sequence SHA-256 |
|---|---:|---|
| HV-001 | 120 | `26814f8c63301fd3cf545b1e6ea744712ccf2ac5e4e70e40281a1d63271a25b1` |
| HV-002 | 120 | `d6b02e23c8bc2b3c27b99fab7cfb0917d90f310a67a5e2cae4258179e35280d2` |
| HV-003 | 120 | `5f410474bf45c16c194c1f2372159de5b7b26dc4e919f9e3ed039d8f2ddf8bbe` |
| HV-004 | 120 | `db3ae88c5242ac275a5708e83ef116e3362ff49a6bfe8a2c1630d0ad2005c334` |
| HV-005 | 120 | `af0d2b702c00289b86a7f750193735cd0abefbf3bc0acc7de1e822f2a46fbf8c` |
| HV-006 | 120 | `f019b5001d7d2e7c20b7796a8ff9583fb7b84fbf16d28b9dc81160e65eb949cf` |
| HV-007 | 120 | `98c6c53883aac2e2c1a4eb581bbb49dd378b3f6dd433d24d04095004a9a587cc` |
| HV-008 | 120 | `5e8c95896488f00f0f01f91ee7d3d4e837185202cb1df4cdc5be8fecb6e18216` |

These checksums cover authoritative JSON packets, not PNGs or videos. The
current renderer has a deterministic browser preview and phone/laptop layout
tests, but this docs/data task did not create or approve exact video files.

## Unsupported observation surfaces

The following are honest backlog requirements, not episodes in this pilot:

| Needed surface | Exact object | Scale | Observable | Capability state | Why omitted |
|---|---|---|---|---|---|
| temporal packet/export path | chambers | depth band | transition through world time | needs_observation_surface | `observation/frame/v1` currently carries one static underworld readout; repeating it does not demonstrate change |
| typed relation packet/export path | cave systems | depth band | junction graph edges and components | needs_observation_surface | the producer reports aggregate junction facts but does not export graph structure for a relational film |
| close-reading packet/export path | chamber run | cave system | one selected run's floors and material sequence | needs_observation_surface | the producer prints three examples, but the exporter cannot select and structure one run independently |
| temporal renderer | chambers | depth band | transition through world time | needs_renderer | the atlas observation renderer currently accepts only `spatial` packets |
| relational renderer | cave systems | depth band | typed junction relations | needs_renderer | no relation-node/edge render contract exists |
| close-reading renderer | chamber run | cave system | selected run detail | needs_renderer | the current presentation remains the general spatial readout |

No simulation extension is requested by these rows: the current readout
already establishes the static facts used here. The missing work is an
authoritative observation surface and corresponding renderer, to be planned at
least fourteen days before demonstration and tested at least seven days before
publication.

## Review boundary

- Evidence review checked HV-002 through HV-008 against the producer fixture
  and kept causal language out of distribution claims. HV-001 remains an
  evidence draft because its existing phrase "across depth bands" needs an
  editorial decision about whether it describes distribution or connectivity;
  the pilot does not silently rewrite the committed renderer fixture.
- Renderer tests, rather than an invented screenshot record, establish the two
  layout dimensions and preservation of authored labels.
- Caption files contain advisory primary drafts and optional replies only.
- All eight exact videos, rendered labels, and caption packages still require
  Nathan's manual review. Until then every `editorial_status` remains `draft`,
  every `approval` remains `null`, and the approved reserve count is **0 of 7**.
