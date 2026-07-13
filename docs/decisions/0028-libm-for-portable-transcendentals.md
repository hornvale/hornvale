# 0028. libm for portable transcendentals

**Status:** Accepted (2026-07-13) · **Decider:** Nathan

In the context of the constitution's cross-platform byte-identity requirement,
facing the fact that Rust's inherent `f64` transcendentals (`x.sin()`,
`x.powf(y)`, …) dispatch to the *platform* libm — Apple's on macOS, glibc's on
Linux — which differ in the last ULP, we decided that **every `f64`
transcendental in the workspace routes through `hornvale_kernel::math`, backed
by the pure-Rust `libm` crate**, and that **`libm` joins the dependency
allowlist** (amending [0004](0004-no-new-dependencies.md)).

**Context.** 8-sig-fig quantization (decision
`serialized-floats-are-quantized-for-cross-platform-determinism`) makes the
*serialization* boundary portable, but the *compute* path still sees
full-precision platform-libm values — and a discrete threshold on such a value
(a percentile sea level, a merger test) can flip between platforms even when
the quantized output would agree, cascading into divergent discrete choices.
That is the divergence that froze CI (macOS-pinned homophony means and
lands-drift seed 1 disagreeing on Linux). Routing transcendentals through the
`libm` crate makes the compute path itself bit-identical on every platform,
upstream of both the threshold and the quantizer.

**Why `libm` is a justified exception to 0004.** 0004 bans convenience crates
(rand, chrono, clap, thiserror) because each adds hidden state, a supply-chain
surface, and a determinism *risk*. `libm` is the opposite: a `no_std`,
pure-Rust, dependency-free implementation of the C libm transcendentals that
*removes* a determinism risk (platform-libm divergence) instead of adding one.
It runs in the sim core precisely because it is deterministic; randomness still
comes only from the kernel's `Seed`/`Stream`. Measured cost: ~1.04× on equal
hardware — the spike's 9-hour run that suggested pathology was a thermal-
throttled laptop, not algorithmic expense. Effectively free.

**Scope.** Only transcendentals live in `kernel::math`. IEEE 754 requires exact
results for `sqrt`, `abs`, `floor`/`ceil`/`round`, `mul_add`, `powi`, and
arithmetic, so those inherent methods remain allowed everywhere. A clippy
`disallowed-methods` lint (`clippy.toml`) enforces the routing — new code
cannot call `f64::sin` &c. directly.

**Quantization stays.** `libm` and quantization are complementary, not
alternatives: `libm` fixes the compute path; quantization remains the
emit-boundary defense-in-depth (and covers non-transcendental platform
differences the routing does not touch). Whether `libm` makes quantization
*redundant* is a separate later evaluation (the-datum thread), not settled
here — the Lorenz guard-rail in the constitution still holds.

**Consequence.** Adding a transcendental requires a `kernel::math` wrapper plus
its clippy-ban entry. The cross-platform divergence that froze CI is expected
to resolve; the validating evidence is regenerating the artifacts on Linux and
confirming byte-identity with the committed macOS set (this campaign's
cross-platform check).

**See also.** [0004](0004-no-new-dependencies.md) (no new dependencies —
amended here); `serialized-floats-are-quantized-for-cross-platform-determinism`;
`kernel/src/math.rs`; `clippy.toml`.
