# 0055. External clients consume Hornvale as a versioned wasm catalog

**Status:** Accepted (2026-07-14) · **Decider:** Nathan

In the context of Goldengrove (an external 3D planetarium client) needing
world data without a network service or a duplicated-physics port, we
decided **external clients consume Hornvale through a wasm catalog crate
built and released from this repo**, never by re-implementing generation
logic client-side or by standing up a server.

**Context.** `clients/world-wasm/` is an out-of-workspace `cdylib` exposing
a hand-rolled `extern "C"` ABI (`hw_*` prefix; vessel's `hv_*` stays
separate) — no wasm-bindgen, matching decision 0023's "clients carry their
own toolchains" posture. The seed is a `u64` argument to `hw_new`/
`hw_new_pinned`, never smuggled into the pins JSON, keeping decision 0007's
seed-is-identity contract legible at the ABI boundary. Any `hw_new*` call
invalidates whatever world the instance previously held — one live world
per instance, no implicit multi-world state. The golden smoke test asserts
the wasm catalog's `scene/system/v1` and `scene/tiles/v1` output is
byte-identical to the native `hornvale scene system|tiles` CLI output for
the same seed and pins, and a size gate holds the artifact to ≤ 1 MiB.
Built catalogs publish as GitHub release assets on `world-wasm-v*` tags
(decision 0052's deploy-built posture, not decision 0018's committed-bundle
one — the catalog is versioned by tag, not by git blob). Scene schemas
(`scene/system/v1`, `scene/tiles/v1`, …) become cross-repo contracts the
moment a second repo parses them: additive-or-versioned only, the same
discipline decision 0006 already holds for seed-derivation labels, now
extended across the repo boundary.

**Consequence.** The repo boundary *is* the determinism boundary: Hornvale
guarantees byte-identical, seeded output up to and including the wasm ABI;
what a client does with that output (rendering, interaction, client-side
state) is unconstrained and, per decision 0022, not the sim's concern. This
buys Goldengrove (and any future external client) a single source of truth
for physics with no second implementation to drift, at the cost of a
release-and-pin workflow instead of a live dependency — a client is always
one `world-wasm-vN` behind until it re-pins. Nathan ratifies this record at
the Goldengrove campaign's merge gate, closing out the "Decisions to
promote at merge" item the spec flagged in advance.

**See also.** [Goldengrove
spec](https://github.com/hornvale/hornvale/blob/main/docs/superpowers/specs/2026-07-14-goldengrove-design.md)
§§2–3, 8; decision 0022 (sim emits data, clients render); decision 0023
(in-repo clients carry their own toolchains); decision 0007 (the seed is a
world's identity); decision 0006 (seed-derivation labels are permanent
contracts); decision 0052 (deploy-built wasm, the Casement precedent).
