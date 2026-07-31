# 0091. glibc does not explain 0063, and the machine does not matter

**Status:** Accepted (2026-07-30) · **Decider:** Nathan · **Refines:**
[0063](0063-census-regen-is-local-again.md),
[0090](0090-the-canonical-host-is-audited-not-assumed.md) ·
**Relates to:** [0033](0033-serialized-floats-are-quantized-for-cross-platform-determinism.md),
[0041](0041-libm-for-portable-transcendentals.md),
[0079](0079-census-goldens-are-authored-on-one-enforced-host.md)

Decision 0063 observed a ~0.1% discrete-count divergence between lefford and
an AWS box and never diagnosed it. Decision 0090 audited the canonical host,
found it reproduces, and left two questions open. The Twin answers both, by
removing the thing that made them unanswerable: **every prior cross-host
comparison varied the machine, the OS, the glibc, the rustc, and the absolute
build path simultaneously.** With five differences and one observation, no
mechanism was separable. An upstream container fixes four of them.

## The measurements

Both halves at `13046985`, 2026-07-30.

**Half A — is glibc the mechanism?** Four builds, rustc held constant at
1.96.1, glibc and the codegen flag varied. Compared on *probe output* (the
40-seed all-metric window from The Pyx), not binaries: two glibcs link
different symbols, so binary difference there says nothing about worlds.

```
  arm  glibc   flag           probe output sha256
  ---  ------  -------------  -------------------
  A    2.36    x86-64-v2      ddb999ff…
  D    2.36    none           ddb999ff…
  B    2.41    x86-64-v2      ddb999ff…
  E    2.41    none           ddb999ff…
```

All four identical, and identical to The Pyx's runs on both lefford and an
aarch64 Mac.

The flag's absence was verified before the comparison, not assumed:
**147 `roundsd` instructions with the flag, 0 without.** The unflagged arms
genuinely restored the library-call path that
`.cargo/config.toml` describes.

**Half B — does the machine matter?** Identical image, glibc, rustc, and
`/build` path on lefford and velaryon (Talos, containerd, 24 amd64 cores):

```
  lefford   cb4ba9068301cf4f53bafc24e56317602332a84f4fe25864eaf121973e2ddb1d
  velaryon  cb4ba9068301cf4f53bafc24e56317602332a84f4fe25864eaf121973e2ddb1d
```

## The ruling

**The codegen hypothesis is falsified.** With the flag off, `f64::floor()`
compiles to a library call into glibc, and two *different* glibcs (2.36 and
2.41) produce byte-identical worlds. This was the leading explanation for
0063 since July and it is wrong — for the reason the hypothesis always
carried as a caveat: IEEE-754 `floor` is exactly representable, so any
conforming implementation returns the same value. Both conform.

**A machine is not, by itself, a source of divergence.** Two different
x86_64 machines — different CPUs, kernels, container runtimes, and operating
systems (Debian vs Talos) — produced the same binary once the toolchain,
libc, and build path were fixed. 0063's divergence therefore lives in one of
the things the container equalized, not in the hardware.

**What 0063's divergence probably was, recorded as a candidate rather than a
finding.** The Twin's own instrumentation surfaced a better hypothesis by
accident. An environment probe reported `rustc 1.77.1` for the host arms;
that was the probe running outside the repository, where **rustup falls back
to lefford's default toolchain instead of `rust-toolchain.toml`'s pin.** The
builds were unaffected (verified: `rustup show` reports
`1.96.1 … (overridden by '/tmp/twin-host/rust-toolchain.toml')`). But it
means a *toolchain pin that only applies inside the working tree* has been
live on the canonical box all along. If July's AWS regeneration invoked cargo
in any way that missed that override, it compiled with a different rustc, and
a one-unit disagreement in a threshold-decided count follows easily. This
cannot be tested — the AWS box is gone (0063 abandoned it) — so it is
recorded as the strongest surviving candidate and not as an answer.

**What this does NOT license.** Velaryon has been shown to produce an
identical *binary* under a fixed environment. That is evidence toward 0063's
"proven byte-identical" clause and nothing more. It is **not** authorization
for velaryon to author census goldens: no output-level census comparison has
been run there, and 0079's authorship discipline is deliberately untouched.
Whether to widen authorship is a separate decision.

## Consequences

- 0063's divergence is **still unexplained**, but the space is much smaller:
  glibc is eliminated, the codegen baseline is eliminated, and bare hardware
  is eliminated. What remains is the toolchain-resolution candidate above and
  the possibility that the original comparison was confounded in a way no
  longer recoverable.
- **Qualifying a host is now a known, cheap procedure**: same upstream image,
  same `/build` path, compare one `sha256sum`. It needs no custom image, no
  registry, and no change to any machine's trust store.
- The container is the instrument, not the deployment. Nothing about how
  Hornvale builds or ships changed, and `target-cpu=x86-64-v2` is untouched —
  the unflagged builds were throwaway diagnostics.
- A Job targeting velaryon requires tolerations for **both** its
  `platform=x86` and `gpu=true` `NoSchedule` taints. Without them it stays
  `Pending` indefinitely rather than failing, which is the failure mode most
  easily mistaken for a slow build.
- **Follow-up worth doing on its own merits:** make the toolchain pin
  independent of the working directory, or at least make a wrong-toolchain
  build loud. A pin that silently does not apply outside the tree is a
  determinism hazard regardless of whether it caused 0063.
