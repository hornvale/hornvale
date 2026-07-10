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

**Downstream consumers must use quantization-aware tolerances.** A consumer
that *reproduces* a quantity from quantized elements (the orrery's
`ephemeris.ts` recomputes orbital phases from the published `scene-system`
elements) sees the 8-digit granularity **amplified** by any division: a
rotation phase is `t/day_length`, so at `t = 360` and `day_length ≈ 0.88`
the ~1e-8 element granularity becomes a ~1e-6 phase discrepancy. Such a
cross-language reproduction check therefore asserts to ~1e-5, not to full
float precision — it still catches any real formula divergence (off by
orders of magnitude more) but cannot demand more precision than the
published, quantized elements carry.

## Rendered per-cell views are platform-local (not cross-platform checked)

Quantization makes serialized *floats* byte-identical, but it cannot make a
*categorical* decision identical when that decision thresholds a full-
precision libm value: a biome (`biome_at` on temperature) or ocean
(`elevation >= sea_level`) classification flips across platforms whenever a
cell sits within a last-ULP hair of a threshold, and with thousands of cells
at least one does. This surfaces only in the two artifacts that encode
per-cell classification — the PNG maps and `scene-tiles.json` — as a changed
biome index or pixel color.

These are **rendered views, not canonical state** (a world is a seed plus a
ledger; the maps are derived presentations). So they are **excluded from the
cross-platform byte-drift check** (`git diff --exit-code` pathspec excludes
`book/src/gallery/*.png` and `scene-tiles.json`); their byte-identity is
platform-local. The canonical numeric artifacts — `world.json`, the
censuses, the ephemeris, `scene-system.json` — remain strictly checked and
*are* byte-identical everywhere, so any real worldgen change still surfaces
loudly there. The trade-off accepted: CI no longer catches drift confined to
those ~8 image/tile files. Each map's `.md` sidecar carries a one-line note
recording that its raster is a platform-local render. (The only way to close
this gap entirely is a portable/vendored `libm` so the classifications
themselves become bit-identical — considered, deferred: it needs a
no-new-deps override and re-derives every committed value.)

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
