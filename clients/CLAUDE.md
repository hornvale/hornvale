# CLAUDE.md — working in `clients/`

Everything here is **outside the cargo workspace** (`Cargo.toml`'s `exclude`
list) and outside the determinism guarantee. The workspace rules you have
internalised — the serde/serde_json/libm allowlist, `#![warn(missing_docs)]`,
the `BTreeMap`-only ban — do **not** bind this tree. Read the root
`CLAUDE.md` "Architecture" section for where the boundary sits; this is the
map of what is on the far side of it.

## The determinism boundary is the repo boundary (decision 0055)

Hornvale guarantees byte-identical seeded output **up to and including the
wasm ABI**. What a client does with that output — rendering, interaction,
client-side state, floating-point in a shader — is unconstrained and
explicitly not the sim's concern (decisions 0022/0023). This is why a client
may carry a toolchain the workspace bans.

The corollary bites in the other direction: **the scene schemas are contracts
the moment a second repo parses them.** `scene/system/v1`, `scene/tiles/v1`,
`scene/moons/v1`, `locale/room/v1`, … are additive-or-versioned only, the
same discipline decision 0006 holds for seed-derivation labels — now across a
repo boundary, where you cannot fix both sides in one commit. A client is
always one release behind until it re-pins.

## The three clients

| | what it is | ABI | how it ships |
|---|---|---|---|
| `atlas/` | the map viewer (Deno/TS, canvas) | none — parses a committed scene JSON | bundle **committed** to `book/src/gallery/atlas.js`, drift-checked |
| `vessel/` | the Casement: the live-possession exhibit | `hv_*` (`vessel/wasm/`) | JS bundles committed; the **wasm is deploy-built, never committed** (decision 0052) |
| `world-wasm/` | the world catalog external clients consume | `hw_*` | GitHub release asset on a `world-wasm-v*` tag — versioned by tag, not by git blob |

Both wasm crates are hand-rolled `extern "C"` — **no wasm-bindgen** (decision
0023: clients carry their own toolchains, and the ABI stays legible). The
prefixes are deliberately disjoint (`hv_*` vessel, `hw_*` catalog) so a page
can host both.

`world-wasm`'s seed is a `u64` argument to `hw_new`/`hw_new_pinned`, **never
smuggled into the pins JSON** — that keeps decision 0007's seed-is-identity
contract visible at the ABI. Any `hw_new*` call invalidates whatever world
the instance held: one live world per instance, no implicit multi-world
state.

The external **Orrery** (a sibling repo, `../orrery`) is a consumer of
`world-wasm`, not a member of this tree. Changing a scene schema is a
cross-repo migration.

## Deno is pinned to 2.9.2 exactly

Both `deno.json` files say so, and the CI jobs pin the same version, because
**the bundle is drift-checked** — a different Deno emits different minified
output and reddens the check for no semantic reason. If you bump it, bump it
in `clients/atlas/deno.json`, `clients/vessel/deno.json`, and both CI jobs
together, and regenerate the bundles in the same commit.

## Gates

```bash
make vessel-check   # deno fmt/lint/check/test + wasm fmt/clippy + byte-identity smoke
make world-check    # lint + golden byte-identity smoke + a ≤ 1 MiB size gate
make wasm-vessel    # build the Casement wasm into book/src/gallery (deploy does this too)
make wasm-world     # build the catalog wasm

# Per-client, from the client's directory:
deno fmt --check && deno lint && deno task check && deno task test
deno task build      # then `git diff --exit-code` the bundle it wrote
```

`make gate` does **not** run any of these — the workspace gate cannot see
this tree. A client change needs its own gate run, explicitly.

**The byte-identity smoke is the load-bearing test.** `world-check` asserts
the wasm catalog's scene output is byte-identical to the native `hornvale
scene …` CLI output for the same seed and pins; `vessel-check` asserts the
wasm opening matches the committed native transcript. These are what stop the
wasm build from silently becoming a second, drifting implementation of the
physics. Never rebaseline one to make it pass — a diff there means the two
paths genuinely disagree, which is the bug.

## Bundles are build output that happens to be committed

`book/src/gallery/atlas.js`, `vessel.js`, `vessel-worker.js` are committed
**and** drift-checked (decision 0018), so editing them by hand is always
wrong — edit `src/` and rebuild. The `.wasm` files are the opposite
(decision 0052): built at deploy, never committed, so they are absent from a
fresh checkout until you run `make wasm-vessel`.

When testing a client in a browser, **rebuild the bundle first**. Serving a
stale `dist`/gallery bundle against fresh source has burned this project more
than once: the page renders, the test passes, and it is testing the previous
commit.
