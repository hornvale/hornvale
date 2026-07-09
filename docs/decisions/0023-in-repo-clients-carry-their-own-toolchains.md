# 0023. In-repo clients carry their own toolchains

**Status:** Accepted (2026-07-09) · **Decider:** Nathan

In the context of building the first Ring-3 client (the atlas viewer) with
real automated tests, facing the no-new-dependencies rule (decision 0004)
and an earlier vanilla-JS sketch of the in-book viewer, we decided that
**Ring-3 clients living in this repository sit under `clients/` — a new
top-level layer outside the Cargo workspace — carry their own dependency
stacks and toolchains (per decision 0022: clients never link the crates),
commit their built artifacts into the book, and are verified by dedicated
CI jobs**, accepting a second toolchain in the repository and built
artifacts in version control.

**Context.** Decision 0022 already places clients outside the workspace
with their own stacks; the binding posture was always *the workspace takes
no client dependencies*, not *clients take none*. The committed-artifact
pattern is decision 0009 ("models author, dice roll") applied to clients:
build offline, commit the output, drift-check its freshness. The owner
chose TypeScript with unit tests over the "no npm, no build step" sketch;
the toolchain is Deno (one pinned binary — native TS, built-in test
runner/formatter/linter, single-file bundling), which keeps the client's
dependency surface near zero.

**Consequence.** `clients/atlas/` is the first citizen; its CI job (pinned
Deno version, fmt/lint/check/test/build, bundle-freshness diff) runs
beside the Rust gate, never inside it. The Rust workspace's rules and CI
steps are untouched. Future in-repo clients follow the same shape; the
registry's viewer row is updated to match reality on merge.

**See also.** Decisions 0022, 0009, 0004; the atlas-viewer spec
(`docs/superpowers/specs/2026-07-09-atlas-viewer-design.md`); the
rendering-strategy spec (Ring 3).
