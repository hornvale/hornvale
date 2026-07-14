# 0047. The Casement's wasm is deploy-built, never committed

**Status:** Accepted (2026-07-13) · **Decider:** Nathan

In the context of the Casement (the possess loop as a live wasm exhibit in
the project book) and the established committed-bundle pattern (atlas.js /
orrery.js are committed to book/src/gallery and drift-checked in CI), we
decided that **vessel.wasm is built at deploy time in book.yml and never
committed** — `book/src/gallery/vessel.wasm` is gitignored, `make
wasm-vessel` produces it locally, and the deploy workflow runs the same
target before `mdbook build`.

**Context.** The committed bundles this diverges from are ~50 KB of
readable JS; the wasm is a ~half-megabyte binary that re-derives on every
worldgen change — committing it would grow git history by that much per
change and add a cross-host byte-reproducibility obligation
(--remap-path-prefix and friends) that buys nothing a build step doesn't.
The Casement's freshness guarantee does not need frozen bytes: the CI
`vessel` job's smoke driver asserts the wasm's seed-42 opening is
byte-identical to the committed native transcript
(book/src/gallery/possession-seed-42.md), which `make rebaseline` keeps
current. Identity is held by that check, not by the artifact. The JS
bundles (vessel.js, vessel-worker.js) stay committed and drift-checked —
they are small, readable, and follow decision 0023's pinned-Deno
reproducibility regime.

**Consequence.** Book deploys gain a pinned-toolchain Rust build step
(~2-3 min with cache). A local `mdbook serve` shows a dark casement until
`make wasm-vessel` runs — the chapter says so in place. The wasm crate
lives outside the workspace (tools/type-audit precedent), so the workspace
gate does not cover it; `make vessel-check` and the CI vessel job are its
gates.
