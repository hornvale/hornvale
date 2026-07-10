# Serialized floats are quantized for cross-platform determinism

**Status:** Accepted (2026-07-10) · **Decider:** Nathan · **Campaign:** Common Ground

In the context of CI having gone red on every push to `main` since
2026-07-07 — the byte-exact drift checks failing on a Linux/x86_64 runner
for artifacts frozen on a macOS/arm64 dev machine — facing the fact that
Rust's `f64` transcendentals (`sin`/`cos`/`atan2`/`powf`) dispatch to the
**platform libm** (Apple's vs glibc's), which differ in the last ULP, and
that those differences reach every committed golden file, we decided that
**floats are quantized to a fixed number of significant digits at each
serialization boundary**, so the same seed yields byte-identical committed
artifacts on every platform:

- A kernel primitive `quantize(f64) -> f64` rounds to `QUANTIZE_SIG_DIGITS`
  (**8**) significant decimal digits using Rust's **libm-free** float
  formatting/parsing (`core::fmt` Grisu/Dragon + `core::num` dec2flt
  Eisel-Lemire — pure Rust, bit-identical on every platform). Two values
  differing only by sub-quantum libm noise collapse to the same `f64`.
- Applied at the four boundaries that carried high-precision floats: the
  **ledger** (`Ledger::commit` canonicalizes every `Value::Number` and
  `Fact.day`), the **lab CSV** (`render_csv`), and the **scene/ephemeris
  JSON** (serde `serialize_with` adapters + the ephemeris example). The
  almanacs, PNG maps, and reference dumps carried no high-precision floats
  and were left untouched.
- This is a deliberate **save-format epoch**: every committed world,
  census, scene, and ephemeris was regenerated in the same change. It
  changes stored *values*, not stream labels, so no `stream_labels` change
  is needed.

accepting that quantization is ~99.97% effective rather than a proof of
cross-platform identity — a value landing within sub-quantum distance of a
rounding boundary can still flip, which the same CI drift checks catch and
which is resolved by re-pinning, exactly like any golden's lifecycle.

## The Lorenz guard-rail (load-bearing)

A lossy save format is safe here **only because reload re-derives providers
from the lossless `u64` seed**, never from the stored floats: `terrain_of`
and the sky path call `generate(world.seed, …)` at full precision, and the
quantized ledger is a posterior *record*, not dynamical *state*. So
importing a world and running off it recovers full-precision initial
conditions from the seed — the 8-digit floats never seed an integrator.

**The rule this establishes:** dynamical resumption always re-derives from
the seed. **Never seed a chaotic forward-integrator from quantized ledger
floats** — an 8-digit perturbation as an initial condition is Lorenz's
butterfly, and would bloom into a divergent trajectory. If a chaotic
subsystem ever needs true checkpoint/resume, it gets its own full-precision
(or seed-plus-input-schedule) checkpoint format, distinct from the
published save.

## Alternatives considered and rejected

- **Platform-specific artifact sets** (build an amd64 set on Docker/an R720,
  key the fixture on `target_arch`). Rejected: "amd64" is not one bucket —
  glibc libm results can change across glibc versions, so a runner-image
  bump silently re-breaks the golden with zero commits, and every new
  platform mints another bucket. O(platforms) maintenance, and it
  institutionalizes the divergence instead of removing it.
- **A portable `libm` crate** (bit-identical transcendentals at the source).
  Cleanest in principle, but violates the workspace's serde-only
  no-new-crates rule, and it re-derives every committed float anyway (the
  same epoch regen), so it is not cheaper.
- **Tolerance-based comparison** (approximate float compare instead of
  byte-exact). Rejected: it abandons the byte-identity contract the
  fixtures exist to defend.

## Context

The determinism contract ("same seed + pins → byte-identical worlds") held
*within* a platform but was being enforced *across* platforms via golden
files frozen on one dev's Mac while CI runs Linux. Quantization at the
serialization boundary — not in the compute path — restores true
cross-platform byte-identity without perturbing the simulation: the noise
fields (integer-hash value/fbm, already libm-free), sculpting, and orbital
mechanics all still run at full `f64`; only emitted representations are
canonicalized. See `book/src/chronicle/common-ground.md`.
