# The Casement

*A pane in a document through which the document's subject runs live.*

The gallery already held a possession. [A Possession — seed 42, day
0](../gallery/possession-seed-42.md) is a frozen transcript: the native
binary walked a goblin through seed 42's rainforest, and the prose was
committed verbatim, drift-checked by CI like any other artifact. It is a
photograph of the seam The Seam built. This campaign made the pane live.
What shipped is the possess loop compiled to WebAssembly and embedded in a
new chapter, [A Possession, Live](../gallery/possession-live.md): a box for
any `u64` seed, a *possess* button, and a verb prompt. Type a seed, press
the button, and the reader's own browser derives the world — sky,
tectonics, climate, settlements, the goblin — and hands back prose that is
**byte-identical** to what the native binary would print for that seed.
Nothing is a recording; the simulation runs in the page.

## Zero source changes, because the constraints were already the right ones

The remarkable part is what did *not* happen. The kernel, the two domains
the vessel reaches (astronomy and terrain), the worldgen composition root,
and the vessel window itself compiled to `wasm32-unknown-unknown` with
**zero source changes** — a thin cdylib shim (`clients/vessel/wasm/`) is
the only new Rust. This is not luck; it is the constitution collecting a
dividend. Every constraint the determinism doctrine imposed for its own
reasons turns out to be exactly a wasm-portability constraint. No
wall-clock time means no `std::time` to stub out on a target that has no
clock. No `HashMap`/`HashSet` means no iteration-order nondeterminism to
reconcile across a second platform. No threads means the single-threaded
wasm target is not a special case. And routing every `f64` transcendental
through the pure-Rust `libm` (decision 0041) — adopted to make Apple's
libm and glibc agree in the last ULP — means the browser's math is the
*same* math, function for function, with no platform libm underneath at
all. The portability was paid for years ago under a different invoice.

The size was measured, not guessed. Compiled at `opt-level = "z"` the
module is **474.55 KiB raw, 193.10 KiB gzipped** — comfortably under the
spec's 1 MiB gate, and the payload the reader actually downloads is the
gzipped figure. The obvious lever, `opt-level = 3`, was measured and
**rejected**: it produces a 726.19 KiB module (a 53% larger binary) to buy
roughly 2% off genesis time, a delta well inside run-to-run noise. Ties go
to the smaller binary, so `z` is pinned in `clients/vessel/wasm/Cargo.toml`
with the measurement recorded beside it. Seed-42 genesis — a full world
derived in the module — runs in a **few seconds** (measured 3.9–5.8 s
across runs on an M1 Max, contention-dependent: roughly 4 s idle, 5.5 s
under load). The chapter says "a few seconds" and means it; the honest
number is a range, because the browser's scheduler is not ours to pin.

## The sandbox is the empty imports object

A WebAssembly module declares what it needs from its host in an *imports
object*. The vessel's is **empty**. Five exports go out — genesis and the
verb entry points — memory goes in, prose comes out, and the module can
consult nothing else: no network, no clock, no DOM, no random source. The
determinism the native binary asserts by discipline the wasm module asserts
by *construction* — there is no smuggling channel, because there is no
channel at all. `clients/vessel/`'s driver instantiates the module with
`{}` and would fault if a single import appeared; the sandbox is not a
promise in prose but the shape of the artifact.

## Deploy-built, and identity held by CI, not by frozen bytes

The wasm binary is **never committed** (decision 0052). It is built on
deploy — `make wasm-vessel` in the book workflow — and the repository
carries only its Rust source and the Deno client. A binary in git would be
a second source of truth for the same world, drift-prone and reviewable by
nobody; leaving it out means the *only* committed description of seed 42's
possession is the transcript the native binary already drift-checks. What
guarantees the live pane agrees with that transcript is a new CI job,
`vessel`, which builds the module and runs it through a byte-identity smoke
against the **committed transcript** — the same bytes The Seam froze. The
native and browser paths are held to one another not by shipping identical
bytes but by proving, on every push, that they *produce* identical bytes.
`make vessel-check` runs that gate locally; because the outside-workspace
crate is invisible to `make gate`, it is a real second commit gate, not a
convenience.

## What this opens

The casement is a primitive, and its sequels are named rather than built.
Four are recorded against the campaign's spec: the **vigil**, a casement
whose day advances in wall-clock time while the page sits open, the sky
wheeling — client-side presentation, determinism waived as the Live Orrery
already waives it; the **diptych**, two casements side by side on different
seeds, the comparative form the lab performs over a thousand worlds reduced
to what a reader does with two; **chapter-pinned casements**, a possession
standing in the exact phenomenon a chapter describes, which wants a
possess-at-position the vessel does not yet expose; and **attract mode**,
the walker battery replaying itself so the exhibit plays between readers.
None of these is the Vessel arc's spine. Milestone I stays **native-first**:
the
projection graduates against a real second consumer where the tools are
sharpest, and the casement follows it to the browser rather than leading.
What shipped here is that the document can now *run its own subject* — the
book stopped describing the simulation and started being a place to stand
in it.
