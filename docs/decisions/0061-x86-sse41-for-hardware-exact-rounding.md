# 0061. x86 builds enable SSE4.1 so exact rounding uses hardware `roundsd`

**Status:** Proposed (2026-07-18) · **Decider:** Nathan (pending
`make gate-remote-verify`)

In the context of the noise inner loop calling `f64::floor` per sample
(`value_noise_2d`, `kernel/src/noise.rs`), facing the fact that the default
`x86-64` target is SSE2-only — so `floor`/`ceil`/`round`/`trunc` cannot use
the hardware `roundsd` instruction (SSE4.1) and instead compile to a
**software `floor()` function call** — we decided (proposed) to enable
SSE4.1 for x86 builds (`-C target-feature=+sse4.1`, or the cleaner
`target-cpu=x86-64-v2`) scoped to x86 targets in `.cargo/config.toml`,
accepting that this touches determinism-critical codegen and therefore may
not land until the cross-platform byte-identity gate signs off.

**Context.** A flamegraph of the `hornvale-lab` metrics suite (the gate's
2nd-largest consumer; the profiled test was `core_homophony_is_eliminated_
at_seed_42_by_the_injective_assignment`, ~20s isolated in debug) attributed
that test's cost ~60% to world generation, and within it **`floor` at ~8%
self-time — as a called function, not an inlined instruction**. Cause: the
baseline `x86-64` microarchitecture level lacks SSE4.1, whose `roundsd`
lowers `floor`/`ceil`/`round`/`trunc` to a single instruction. ARM (Apple
Silicon) already has this for free (`frint*`), so x86 baseline is the only
platform paying the software-floor call. This is a **compute-path** cost:
it recurs per noise sample per cell per field per world, so it taxes the
gate, release, and the AWS censuses alike — unlike the dev `opt-level`
lever (0000-series `TOOL-hot-crate-opt`), which is debug-only.

Measured (release, `different_seeds_differ`, single-threaded), on top of
the noise-seed hoisting already merged:

| | time |
|---|---|
| baseline (SSE2) | 18.4 s |
| `+sse4.1` | **15.6 s (~15% faster)** |

`floor`/`ceil`/`round`/`trunc` are **IEEE-754 exact** — `roundsd` returns
the identical bits to a software floor — so this is byte-identical in
principle, and **a full local artifact regen under `+sse4.1` produced zero
drift** (`book/src/gallery|reference|laboratory`). Transcendentals are
untouched: they still route through the pure-Rust `libm` crate
(`kernel/src/math.rs`, decisions 0033/0041), so the platform-libm
divergence those decisions guard is not reintroduced. This proposal only
changes how an *exact* operation is lowered.

**Consequence.** If ratified: every world build gets ~15% cheaper on the
compute path, byte-identically, at no cost to the model. What we knowingly
defer: **local zero-drift proves same-machine identity, not cross-platform
identity — which is the whole point of the determinism edifice.** The
ratification gate is `make gate-remote-verify` (the local-vs-remote
byte-identity acceptance test; the same libm go-live gate), run with the
flag set, confirming the x86+SSE4.1 build is byte-identical to the AWS
reference box. The flag must be **scoped to x86 targets** (`[target.
x86_64-*]` in `.cargo/config.toml`); ARM ignores it. Restrict to
`+sse4.1`/`x86-64-v2` — **not** `+fma`/`+avx`, which enable FMA contraction
and would change float results (a real epoch). Because floor is exact, a
contributor who builds *without* the flag still gets identical bits (just
slower), so the flag is a performance knob, not a save-format contract —
but the remote gate is the acceptance bar before it goes live.

**See also.** decisions 0033 (cross-platform byte-identity via quantization)
and 0041 (transcendentals route through the `libm` crate); CLAUDE.md
"Determinism" and the `gate-remote-verify` Makefile target; the noise-seed
hoisting and dev `opt-level` work that preceded this (session perf sweep,
2026-07-17/18).
