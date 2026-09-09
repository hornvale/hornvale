# 2026-09 opening observation batch

Batch status: **draft internal pilot**. All eight evidence records and caption
packages remain drafts. No exact video/copy package has been reviewed or
approved by Nathan. The seven-package approval reserve remains pending
Nathan's manual review. Nothing in this record authorizes publication.

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
| HV-002 | entrances | world | quantity | entrances | spatial | draft | draft |
| HV-003 | cave systems | world | quantity | cave systems | spatial | draft | draft |
| HV-004 | chambers | depth band | distribution | chambers | spatial | draft | draft |
| HV-005 | chambers | rock layer | distribution | chambers | spatial | draft | draft |
| HV-006 | chambers | world | provenance | chambers | spatial | draft | draft |
| HV-007 | cave systems | depth band | topology or connectivity | cave systems | spatial | draft | draft |
| HV-008 | chamber run | cave system | viewpoint or resolution | floors | spatial | draft | draft |

Every manifest names one value for object, scale, axis, count unit, visual
grammar, and observation sentence. Every source command is the exact current
repository-root command accepted by the exporter.

## Reproduction record

Each manifest requests 900 packets at 30 frames per second, a declared
30-second episode. The batch was
exported from clean per-episode directories. `source_digest` was
`fnv1a64:5344fcc22e8d960d` for every episode because every packet observes the
same producer bytes. The atlas fixture's tested viewports are 390×844 (phone)
and 1440×900 (laptop).

The sequence checksum is SHA-256 over the exact bytes of `frame-000.json`
through `frame-119.json`, concatenated in lexical order.

| Episode | Frames | Sequence SHA-256 |
|---|---:|---|
| HV-001 | 900 | `2d1cad2d9b01a2aba3aa5e3415efce363a1c85a0958a0ee34ae82a0b1fb02d1c` |
| HV-002 | 900 | `4fb27ea29e7ca9549e3b649593dd763e3b5230febf9df00a5e8884c0124190a3` |
| HV-003 | 900 | `3a74b3cfa51ebf5f85af46801f017302fe5a0087896e0d2145fff3374c48eafd` |
| HV-004 | 900 | `76bcc2567779bfb5f4331a8385e440cdb61cd3a83510099d558f5e8728e5cbff` |
| HV-005 | 900 | `5a9a752d1162fc8b0da24d11def7fc700cb993a3340255609a9b8d3a2bebe551` |
| HV-006 | 900 | `da1c6964bdcc6bc9d94a5de847344ccf8cc2943e2e80a76d64b9884b0a52ce5f` |
| HV-007 | 900 | `f95e63ea64e2e6399b9aadddd22e2878082d8bd0f8631e7cd67c2a48ec92ee6e` |
| HV-008 | 900 | `723ede96f6e9d3f30f0869bc85901a63527e80e509241cd05be621ca34f26db7` |

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

- Evidence status remains `draft` for all eight manifests; no evidence record
  is represented as reviewed. HV-001's existing phrase "across depth bands"
  is explicitly pending an editorial decision about whether it describes
  distribution or connectivity. The pilot does not silently rewrite the
  committed renderer fixture.
- Renderer tests, rather than an invented screenshot record, establish the two
  layout dimensions and preservation of authored labels.
- Caption files contain advisory primary drafts and optional replies only.
- Exact video/copy review by Nathan remains pending for all eight packages.
  Until then every `editorial_status` remains `draft`, every `approval` remains
  `null`, and seven-package approval remains pending with a reserve count of
  **0 of 7**.
