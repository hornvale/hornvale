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

## The clients

| | what it is | ABI | how it ships |
|---|---|---|---|
| `atlas/` | the map viewer (Deno/TS, canvas) | none — parses a committed scene JSON | bundle **committed** to `book/src/gallery/atlas.js`, drift-checked |
| `vessel/` | the Casement: the live-possession exhibit | `hv_*` (`vessel/wasm/`) | JS bundles committed; the **wasm is deploy-built, never committed** (decision 0052) |
| `lot/` | the Lot: the four-stage life-draw exhibit | `hl_*` (`lot/wasm/`) | JS bundles committed to `book/src/gallery/lot.js` and `lot-worker.js`, drift-checked; the **wasm is deploy-built, never committed** (decision 0052) |
| `world-wasm/` | the world catalog external clients consume | `hw_*` | GitHub release asset on a `world-wasm-v*` tag — versioned by tag, not by git blob |

This heading read **"The three clients"** until The Lot, and it was already
wrong when that campaign arrived: `game/` — two Rust crates, its own
`make game-check` — has never been in this table. A count in a heading rots
the moment a directory is added, so it is gone rather than corrected to four.

**The Lot moved off the catalog onto its own wasm crate (Task 10b, ledger
#17).** It briefly lived as four `hw_lot*` exports inside `world-wasm`, which
grew the catalog past its release-asset size gate on the canonical box's
older binaryen — an exhibit in an unpublished book is not a released
download, so `lot/wasm/` carries no size gate of its own, the same shape as
the Casement's `vessel/wasm/`.

Three wasm crates are hand-rolled `extern "C"` — **no wasm-bindgen** (decision
0023: clients carry their own toolchains, and the ABI stays legible). The
prefixes are deliberately disjoint (`hv_*` vessel, `hw_*` catalog, `hl_*`
lot) so a page can host all three.

`world-wasm`'s seed is a `u64` argument to `hw_new`/`hw_new_pinned`, **never
smuggled into the pins JSON** — that keeps decision 0007's seed-is-identity
contract visible at the ABI. Any `hw_new*` call invalidates whatever world
the instance held: one live world per instance, no implicit multi-world
state.

The external **Orrery** (a sibling repo, `../orrery`) is a consumer of
`world-wasm`, not a member of this tree. Changing a scene schema is a
cross-repo migration.

## Deno is pinned to 2.9.2 exactly

All three `deno.json` files say so, because **every bundle is
drift-checked** — a different Deno emits different minified output and reddens
the check for no semantic reason. If you bump it, bump it in
`clients/atlas/deno.json`, `clients/vessel/deno.json` and
`clients/lot/deno.json` **together**, and regenerate every committed bundle
(the five named under "Bundles are build output" below) in the same commit. (Until 0125 two GitHub Actions jobs pinned the version too, and
were further places to keep in sync; they are gone, so the `deno.json` files
are now the whole story. This paragraph said **two** files until The Lot added
the third — the number is exactly the kind of thing that goes stale, so check
the directory rather than trusting it.)

## Gates

```bash
make vessel-check   # deno fmt/lint/check/test + wasm fmt/clippy + byte-identity smoke
make world-check    # lint + golden byte-identity smoke + a ≤ 1 MiB size gate
make lot-check      # the Lot exhibit: deno checks + bundle drift + a wasm smoke
make wasm-vessel    # build the Casement wasm into book/src/gallery (deploy does this too)
make wasm-world     # build the catalog wasm
make wasm-lot       # build the Lot exhibit's own wasm

# Per-client, from the client's directory:
deno fmt --check && deno lint && deno task check && deno task test
deno task build      # then `git diff --exit-code` the bundle it wrote
```

**`gate-commit` still does not run any of these — it is local-only and
cannot see this tree, so a purely local commit is never checked here.** That
part has not changed. What has: since 0125 retired GitHub Actions, this
paragraph used to say nothing ran them for you at all, and that stopped
being true this campaign. The `clients` phase
(`make clients-check-run`) now runs `vessel-check`, `world-check`,
`game-check`, `atlas-check` **and** `lot-check` together — in FOUR parallel
arms, not five, because `world-check-run` and `lot-check-run` both build
`wasm-world` into one path and so share an arm (see the Makefile note) — and
the canonical box's chamber
runs that phase in both the stage gate (`make sluice-stage`) and the merge
queue (`make sluice`) — a real backstop, not a manual-discipline promise. A
client change still needs to reach a pushed, queued request before anything
catches it automatically; nothing local does, so don't mistake a clean
`gate-commit` for a clean client tree.

**FIVE parallel arms, not four.** This paragraph used to say `world-check-run`
and `lot-check-run` shared one arm because both built `wasm-world` into the
same path. Task 10b (ledger #17) gave the Lot its own crate and its own
`wasm-lot` target writing a separate path (`book/src/gallery/lot.wasm`), so
the two no longer share a writer and `lot-check-run` runs as its own,
fifth arm (see the Makefile note beside `clients-check-run`).

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

**The size gate's trigger point is host-dependent, because binaryen isn't
pinned.** Measured on lefford during The Staff: this Mac carries binaryen
131, lefford carries binaryen **108**. `world-check` still PASSES there —
gzipped wasm came in at 382,089 bytes against the 524,288-byte ceiling — but
that ceiling was calibrated against 131's ~337 KiB output, and 108 emits
about 11% larger, cutting headroom from ~34% to ~27%. Not wrong (the gate
exists to catch unbounded growth, and a smaller margin is more
conservative, not less safe), but a future flap right at the ceiling on one
host and not the other is this, not a regression — check `binaryen
--version` on both machines before chasing a phantom size regression.

## Bundles are build output that happens to be committed

`book/src/gallery/atlas.js`, `vessel.js`, `vessel-worker.js`, `lot.js` and
`lot-worker.js` are committed **and** drift-checked (decision 0018), so editing
them by hand is always wrong — edit `src/` and rebuild. The `.wasm` files are the opposite
(decision 0052): built at deploy, never committed, so `vessel.wasm`,
`world.wasm` and `lot.wasm` are all absent from a fresh checkout until you run
`make wasm-vessel`, `make wasm-world` and `make wasm-lot`. Three exhibits are
dark without them now, not one — the Lot needs its own `lot.wasm` since
Task 10b (ledger #17) gave it a wasm crate separate from the catalog.

When testing a client in a browser, **rebuild the bundle first**. Serving a
stale `dist`/gallery bundle against fresh source has burned this project more
than once: the page renders, the test passes, and it is testing the previous
commit.

## The native visual workspace

`visual/` contains the reusable native source, the independent Bevy view and
Planetarium, its first application. Read [its guide](visual/README.md) before
editing it. `make visual-check` runs its pinned Rust 1.96.1 CPU checks; both
`clients-check-run` lists now include `visual-check-run` as an additional arm.
The earlier five-arm account above is historical. No new lane phase is needed.

The resolved Cargo metadata guard follows all-feature normal/build/dev paths.
The source cannot reach Bevy/view/application; the view cannot reach simulation,
source or application. The root workspace never links Bevy. The libraries' tests
instantiate independent consumers without Planetarium. GPU inspect/capture and
visual acceptance are separate required evidence, never a silently passing skip.
The scientific source is unrestricted; a future situated game must bring its own
limited producer/mirror, not hide scientific truth in controls.
