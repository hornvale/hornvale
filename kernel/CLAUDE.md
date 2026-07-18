# CLAUDE.md — working in `kernel/`

The kernel is the determinism substrate. Everything above it re-derives from
a seed; the guarantees that make that possible live here, and **most bugs in
this crate are catastrophic and silent** — they corrupt every world without a
crash. Read the root `CLAUDE.md` "Determinism" section first; this adds the
kernel-specific contracts and footguns.

## The three save-format contracts in this crate

Change any of these and the same seed produces a different world — every
committed almanac, map, and census silently diverges. None of them may change
without a deliberate **epoch** (a `/v2` label suffix, never a rename or an
edit in place):

1. **Seed-derivation labels** (`streams.rs`). Every `pub const` here is a
   permanent contract; the doc comment on the module says so. Adding a *new*
   label is safe (it perturbs nothing existing — that's the whole point of
   labeled derivation); changing or reusing one is an epoch. Retired labels
   are never reused or redrawn.
2. **The hash/noise constants** (`seed.rs`, `noise.rs`) — the FNV/splitmix
   constants, the lattice-hash multipliers. These are the arithmetic identity
   of the world.
3. **Stream consumption order.** A pinned path must consume the *same* draws
   in the *same* order as the unpinned path. This is invisible in the types
   and only caught by the pin-isolation tests in the domains — if you add a
   draw, add it so old worlds are unperturbed.

## Quantize at the emit boundary ONLY (decision 0033)

`quantize.rs` rounds floats to 8 significant digits so serialized worlds are
byte-identical across platforms (Apple libm vs glibc differ in the last ULP).
Quantize **only** at serialization boundaries (`Ledger::commit`, CSV/JSON
emit) — **never** in the compute path. The noise fields, sculpting, and
orbital mechanics run at full precision. **Lorenz guard-rail:** never seed a
chaotic forward-integrator from quantized ledger floats; resumption
re-derives from the lossless seed. A lossy save is safe *only* because reload
re-derives.

## Transcendentals route through `math.rs` (decisions 0033/0041)

Every `f64` transcendental in the workspace goes through `math.rs`, which
calls the pure-Rust `libm` crate — NOT the inherent `x.sin()` etc., which
dispatch to the platform libm. This makes the *compute path* bit-identical,
not just the serialized output. **But only transcendentals belong here.**
IEEE-754 guarantees exact results for `sqrt`, `abs`, `floor`/`ceil`/`round`,
`mul_add`, and arithmetic, so those stay as inherent methods everywhere —
routing them through `libm` would be a pure slowdown (a software `floor`
call instead of a hardware `roundsd`; see the perf sweep and Proposed
decision 0061).

## Noise: build a sampler once, sample many

`noise::Fbm` (and terrain's `SphereFbm`) precompute their per-octave/per-slice
seeds once, then sample without re-deriving. `fbm_2d`/`sphere_fbm01` are the
random-access convenience forms that construct a sampler per call — fine for
one-off use, but a hot per-cell loop with a fixed seed must build the sampler
above the loop. `Seed::derive` was the dominant cost in world generation
until this pattern landed; keep it out of inner loops.

## Bans enforced here (and workspace-wide)

- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only. Float sorts
  use `total_cmp` with a deterministic tie-break.
- **No wall-clock time** — time is `WorldTime { day: f64 }`, absolute standard
  days.
- Both are enforced by `clippy.toml` `disallowed-types`; a justified
  exception gets a scoped `#[allow(clippy::disallowed_types)]` with a comment.
- Every `pub` item, field, and variant carries a one-line doc comment
  (`#![warn(missing_docs)]`) and a `type-audit:` tag if it exposes a
  primitive (see `tools/type-audit/CLAUDE.md`).

## Before you touch this crate

Run the full `make gate` before pushing — a kernel change ripples through
every domain, and a byte-identity regression only shows up in the artifact
drift check and censuses, not in a unit test. See the memory note on
boundary changes.
