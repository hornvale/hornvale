# 0090. The canonical host is audited, not assumed — and the audit came back clean

**Status:** Accepted (2026-07-30) · **Decider:** Nathan · **Refines:**
[0063](0063-census-regen-is-local-again.md),
[0079](0079-census-goldens-are-authored-on-one-enforced-host.md) ·
**Relates to:** [0033](0033-serialized-floats-are-quantized-for-cross-platform-determinism.md),
[0041](0041-libm-for-portable-transcendentals.md)

Decision 0063 ruled that one machine is the single canonical platform, on the
strength of one measurement: lefford and an AWS box disagreed by one unit on
~0.1% of census values, discrete counts decided by a comparison upstream of
quantize-at-emit. 0079 then mechanized the ruling — a declared hostname,
enforced at every write path, failing closed.

**The mechanism of that divergence was never diagnosed, and its conditions no
longer hold.** The codegen baseline pin (`.cargo/config.toml`, `3a7092c3`)
landed on 2026-07-27, eight days after the measurement, and it moved the hot
floating-point path: on the default `x86-64` baseline LLVM cannot emit
`roundsd`, so every `f64::floor()` was a *library call* into per-host glibc —
a bare `floor` symbol at 4.62% of census self-time under `perf` — inside
`Fbm::sample`, twice per noise sample. Nobody re-measured afterwards.

Underneath that sat a larger gap. Every determinism guarantee the project
enforces is a **repeatability** guarantee in the ISO 5725 sense — same box,
short interval: the drift check compares a fresh lefford run against a
lefford-authored golden. **Reproducibility** — whether any other apparatus,
or the same apparatus later, reaches the same values — had never been
measured. Lefford was Le Grand K: a physical artifact serving as a standard,
never assayed against a copy.

## The measurement

Three layers, all run 2026-07-30 at
`9855048d` (L0/L1) and `8962db1b` (L2). Both
hosts on rustc 1.96.1; lefford Linux x86_64/glibc 2.36 on 40 cores, the Mac
Darwin arm64 on 10.

| Layer | Question | Result |
|---|---|---|
| L0 | Does lefford reproduce the goldens it authored? | **Zero diff** across the whole generated tree — the 1000-row census, 520 charts, both summaries, the schemas, the gallery and audit artifacts. 900.652 s, rc=0. |
| L1 | Is the release binary a pure function of the source? | **Byte-identical.** Two clean builds at the same SHA in different directories both hash `fb8c368c…`; zero differing bytes. |
| L2 | Do two platforms agree, value for value? | **Byte-identical.** Both 40-world probe CSVs hash `ddb999ff…` across x86_64/Linux and aarch64/Darwin. |

**Seed 681 — the tripwire 0063 named — reads `divergence-magnitude-hobgoblin
= 5` on lefford, on the Mac, and in the committed golden.** The single value
AWS disagreed on is now agreed on by two architectures and two operating
systems.

## The ruling

**0063's divergence does not reproduce under today's pins.** That is what was
shown, and the record says so at exactly that width.

**This refines 0063; it does not supersede it.** Velaryon was never run, so
nothing here licenses a second authoring host. 0079's authorship discipline
is untouched and the hostname guard stays exactly as it is. What changes is
that the single-platform ruling now rests on a *re-measured* premise instead
of an unexamined one, and the premise it rests on has moved: the divergence
that justified it is no longer observable between two current hosts.

**What was not shown, stated so it is not later misread.** The §2 hypothesis —
that the `floor`-as-library-call path was the mechanism — is *consistent with*
these results and remains **unproven**. Confirming it requires rebuilding
without `target-cpu=x86-64-v2` and showing divergence return. IEEE-754 `floor`
is exactly representable, so a conforming glibc should not have diverged at
all; either the mechanism is elsewhere or one box was non-conforming. The
honest position is that the 2026-07-19 observation stands as an observation
whose cause is still unknown.

**Binary identity is adopted as the cross-host oracle.** This was the
campaign's surprise and it inverts a preregistered prediction (§5 of the spec
predicted L1 would fail on embedded build paths). It did not, because the
workspace declares no `[profile.release]`, so cargo's default `debug = false`
leaves no debuginfo and therefore no absolute paths in the binary. The
consequence is operational: **a candidate host can be qualified by building
there and comparing one SHA-256** — seconds, no census, no goldens tree, no
storage. Any future host-qualification starts there and runs a census only if
the hashes match.

**That oracle is conditional and the condition is now load-bearing.** It holds
only while release builds carry no debuginfo. `[profile.profiling]` sets
`debug = true` and is correspondingly *not* directory-reproducible. Adding
`debug` to the release profile would silently revoke the oracle, and nothing
currently detects that.

## Consequences

- Host qualification has a cheap first gate (binary hash) and an expensive
  confirmation (census diff), in that order.
- `docs/timings.md` gains a `census` row at 900.652 s for a run that authored
  nothing — recorded because the run happened, per 0087's "timings are a
  record."
- The residual risk 0079 was built against is unchanged: exactly one host may
  *commit* goldens. This decision makes that a measured choice rather than an
  inherited one.
- Migrating 0079's guard from a hostname to a toolchain fingerprint is now
  clearly the right shape — a hostname cannot catch lefford drifting from
  itself, and L0/L1 together are what a fingerprint would assert. Carried as a
  followup under TOOL-cross-host-assay, not built here.
