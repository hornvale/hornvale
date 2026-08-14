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

Both `deno.json` files say so, because **the bundle is drift-checked** — a
different Deno emits different minified output and reddens the check for no
semantic reason. If you bump it, bump it in `clients/atlas/deno.json` and
`clients/vessel/deno.json` together, and regenerate the bundles in the same
commit. (Until 0125 two GitHub Actions jobs pinned the version too, and were
a third and fourth place to keep in sync; they are gone, so the two
`deno.json` files are now the whole story.)

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

**`gate-commit` still does not run any of these — it is local-only and
cannot see this tree, so a purely local commit is never checked here.** That
part has not changed. What has: since 0125 retired GitHub Actions, this
paragraph used to say nothing ran them for you at all, and that stopped
being true this campaign. `gate-stage`'s `clients` lane set
(`make clients-check-run`) now runs `vessel-check`, `world-check`,
`game-check`, **and** `atlas-check` together, dispatched at every plan-stage
boundary (The Staff) — a real backstop, not a manual-discipline promise. A
client change still needs to reach a pushed stage gate before anything
catches it automatically; nothing local does, so don't mistake a clean
`gate-commit` for a clean client tree.

**`atlas` has a `make` target now: `make atlas-check`**, folded into
`clients-check-run`. It used to be the one real hole — its checks lived only
in the deleted `ci.yml`, and a proposal to add the target was declined at
0125 (2026-08-11) — but The Staff closed it. What it runs, and what you can
still run by hand from `clients/atlas/` while iterating:

```bash
deno fmt --check && deno lint && deno task check && deno task test
deno task build && git diff --exit-code ../../book/src/gallery/atlas.js
```

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
