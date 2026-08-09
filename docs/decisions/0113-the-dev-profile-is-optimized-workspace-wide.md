# 0113. The dev profile is optimized workspace-wide

**Status:** Accepted (2026-08-09, G6) · **Decider:** Nathan · **Relates:**
[0033](0033-quantize-at-emit-boundary.md),
[0040](0040-nextest-is-the-commit-gate-runner.md),
[0041](0041-libm-for-portable-transcendentals.md)

In the context of a commit gate that had drifted from its 4-minute budget
(0040) to roughly fifteen, facing a profile that attributed **70% of the
heaviest test's self time to `core` and `alloc`** rather than to any Hornvale
frame, we decided that **the dev profile is optimized for the whole workspace,
its dependencies, and `libm` — not crate by crate** — accepting that a
campaign editing one crate now compiles that crate optimized.

## Context

Five `[profile.dev.package.hornvale-*]` entries had accumulated, one per
campaign that profiled its own hot crate. Each was individually correct and
individually measured. Together they had a blind spot none of them could see:

**Generic code monomorphises into the crate that instantiates it, and is
compiled at that crate's opt-level.** Marking `hornvale-kernel` optimized
optimizes the kernel's own code and does nothing for the copy of
`BTreeMap::get` that un-marked `hornvale-demography` stamped out for itself.
The per-crate shape can only ever optimize *definitions*; the cost is at the
*uses*.

Separately, cargo leaves **dependencies** at opt-level 0 in dev even when
workspace members are opted. Since 0041 every transcendental in the simulation
routes through `libm`, so the single most-called function in the project was
the one thing in the build with no optimization at all: `libm::exp` measured
**23.7%** of one `hornvale-book` test, essentially all of it inside `scalbn`'s
bit-shuffling helpers.

## The decision

`[profile.dev] opt-level = 2`, `[profile.dev.package."*"] opt-level = 2`, and
`[profile.dev.package.libm] opt-level = 3` (matching `release`, so the debug
suite exercises the same libm codegen as the release-built artifacts it asserts
byte-equality against). The five per-crate entries are superseded and removed.

## The cost that turned out not to exist

The per-crate shape existed to protect compile time. Measured on this box,
whole-workspace `cargo nextest run --workspace --no-run`: **771 s optimized
against ~780 s unoptimized.** An optimizer that deletes code leaves codegen and
the linker less to do, and the two effects cancel. The constraint the earlier
shape was designed around was a hypothesis nobody had priced.

## Consequences

- **This is not a determinism change, and that is verified rather than
  argued.** Rust never enables fast-math and leaves `-ffp-contract` off, so
  opt-level moves no FP result — and the repo already depends on this, since
  `release` optimizes everything while the dev suite pins hundreds of exact
  strings against release-built artifacts. Verification run: full suite green,
  and `make rebaseline` + `git diff --exit-code` over all six generated trees
  produced zero diff, including the 1,000-world Domesday survey.
- **It does NOT extend to `target-cpu`.** `.cargo/config.toml` stops at
  `x86-64-v2` precisely because `v3` enables FMA and reopens the
  floating-point-contraction question (0033). Opt-level and target-cpu are
  different knobs; this record licenses only the first.
- **A future per-crate `opt-level` entry is now a smell**, not a tool. If one
  crate needs different treatment, the question to answer first is why the
  workspace-wide setting is wrong for it.
- **This buys the TEST build and nothing else, which the census proved.**
  Authorized and run at close on lefford: the 1,000-world goldens came back
  **byte-identical** (Linux x86_64, against changes authored on aarch64
  Darwin — a cross-platform confirmation as well as a determinism one), and
  **3% faster**, 22,170 → 21,482 processor-seconds. Not the large gain
  predicted, and the reason is the scope of this record: **the census runs in
  `release`, already optimized at 3**, so this decision cannot touch it. Its
  3% is the two algorithmic fixes alone. Do not cite this record's 2x for any
  release-profile path.

## See also

[The Whetstone chronicle](../../book/src/chronicle/the-whetstone.md);
[the retrospective](../retrospectives/the-whetstone.md) §2–§3;
the `[profile.dev]` comment block in the root `Cargo.toml`, which carries the
measurements.
