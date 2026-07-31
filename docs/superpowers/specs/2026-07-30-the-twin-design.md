# The Twin — Design

**Campaign:** The Twin · **Date:** 2026-07-30 · **Status:** spec, awaiting G3
· **Backlog row:** TOOL-cross-host-assay (continues The Pyx) ·
**Follows:** [The Pyx](2026-07-30-the-pyx-design.md),
[decision 0090](../../decisions/0090-the-canonical-host-is-audited-not-assumed.md)

A container makes two machines twins in everything except the machine. That
is the whole campaign: every cross-host comparison this project has ever run
varied five things at once, which is why none of them could name a cause.

## 1. The problem

[The Pyx](2026-07-30-the-pyx-design.md) audited whether the canonical host
reproduces its own goldens. It does. It also left two questions open, and
both are open for the same reason.

**Q1 — can a second machine author?** Decision 0063 says a second host may
not commit goldens "unless it is **proven byte-identical**", and 0090 supplied
the cheap proof instrument — binary hashing — while amendment 2 bounded it:
the oracle holds **only when both hosts build at the same absolute path.**
Nothing has yet been run on a second machine.

**Q2 — what caused 0063's divergence?** Still unknown. The best-localized
story is that on the default codegen baseline `f64::floor()` compiles to a
library call into per-host glibc (`.cargo/config.toml`'s own comment: a bare
`floor` symbol at 4.62% of census self-time), and that the `x86-64-v2` pin of
2026-07-27 removed that call eight days *after* the divergence was measured.

The Pyx withdrew the experiment for Q2 as unsound, correctly: the flag is
`cfg(target_arch = "x86_64")`-scoped, so removing it moves only the Linux arm
of a Linux-vs-Mac comparison. **But the withdrawal named the wrong
requirement.** It said the test needs "two x86_64 Linux *hosts*". It needs two
x86_64 Linux **glibcs**, which is not the same thing and is much cheaper to
obtain.

**The common cause.** Every comparison to date varied the machine, the OS, the
glibc, the rustc, and the absolute build path simultaneously — lefford vs AWS,
lefford vs the Mac. With five differences and one observation, no mechanism
was ever separable. A container fixes four of them.

## 2. What is now known (verified 2026-07-30, by command)

```
  fact                                   value                        how
  -------------------------------------  ---------------------------  ----------
  rust:1.96.1 exists and pulls           rustc 1.96.1 (31fca3adb      docker pull
                                          2026-06-26)                  on lefford
  ...which is EXACTLY the repo pin       rust-toolchain.toml 1.96.1   git
  ...and exactly both hosts' rustc       lefford + Mac, same string   rustc -V
  image glibc                            Debian 2.41-12+deb13u3       in-container
  lefford HOST glibc                     Debian 2.36-9+deb12u14       ldd
  container reaches crates.io            `cargo search serde` -> ok   in-container
  repo is public                         isPrivate: false             gh
  velaryon                               10.4.0.30, Talos 1.12.5,     kubectl
                                          containerd 2.1.6, amd64
  velaryon allocatable                   23950m cpu / 32 GiB          kubectl
  velaryon TAINTS                        platform=x86:NoSchedule      kubectl
                                          gpu=true:NoSchedule
  Mac tooling                            kubectl 1.33.3 + working     kubectl
                                          cluster context
```

**The image's glibc differs from lefford's host glibc.** That is the variable
Q2 needs, available on one machine, with no cluster involved.

**Velaryon's two taints are load-bearing.** A Job without tolerations for both
does not fail — it stays `Pending` forever, which is the failure mode most
likely to be mistaken for a slow build.

## 3. Decisions

**D1. Use the upstream `rust:1.96.1` image; build no image of our own.** It
carries the pinned toolchain exactly. Consequences: no Dockerfile, no registry
push, and — importantly — no need to install the private `step-ca` root into
lefford's trust store. (Measured: lefford reaches `registry.goldentooth.net`
at 10.4.11.8 and gets HTTP 200 only with `-k`; the CA is untrusted there. That
whole problem is avoided rather than solved.)

**D2. The build path is `/build` on every arm.** This is 0090 amendment 2's
condition, made structural rather than remembered. Both `env!("CARGO_MANIFEST_DIR")`
sites bake an absolute path into the binary, so an identical path is what
makes the hashes comparable at all.

**D3. Source arrives by `git clone` at a SHA, on every arm including the
local ones.** Not a bind mount: a mount carries `.git`, `target/`, and local
scratch, so the two sides would differ in content while appearing to differ in
environment. A shallow clone at a fixed SHA is identical by construction.

**D4. The matrix is run as two independent halves, cheapest first.** Q2 needs
no cluster and settles a question open since 0063; Q1 needs the cluster. Half
A can ship even if half B is blocked.

**D5. No PVC.** Each arm is a one-shot build whose product is a hash printed
to stdout. `emptyDir` is sufficient; a persistent build cache is an
optimization for a repeated workflow this campaign does not have.

**D6. Nothing this campaign runs authors an artifact.** No census, no golden,
no commit of generated output. If Q1 comes back green, *whether to let
velaryon author* is a separate decision for Nathan, not a consequence of this
measurement.

## 4. The design

The cell grid. Rows are environments, columns are the codegen flag.

```
                             | flagged (x86-64-v2) | unflagged (default) |
                             |  = how we ship      |  = diagnostic only  |
 ----------------------------|---------------------|---------------------|
  lefford HOST   glibc 2.36  |  A                  |  D                  |
  rust:1.96.1 @ lefford      |  B                  |  E                  |
                 glibc 2.41  |                     |                     |
  rust:1.96.1 @ VELARYON     |  C                  |  F                  |
                 glibc 2.41  |                     |                     |
```

### Half A — Q2, the glibc question (no cluster)

Cells `D` and `E`: same machine, same rustc, no flag, **different glibc**.

- `D` — build unflagged on lefford's host (glibc 2.36).
- `E` — build unflagged in `rust:1.96.1` on lefford (glibc 2.41).
- Compare the **probe output**, not the binary. Binaries will differ here
  regardless (different glibc means different linked symbols), so the binary
  oracle does not apply; what matters is whether the *worlds* differ.
- Run `studies/the-pyx-probe.study.json` — the 40-seed, all-metric window
  centred on seed 681 that The Pyx already committed — from each build, and
  diff the CSVs.
- `A` vs `B` is the control, flagged, same comparison. The hypothesis predicts
  `A == B` (floor is an instruction, glibc irrelevant) and permits `D != E`.

Removing the flag is a **throwaway diagnostic build**. `target-cpu=x86-64-v2`
stays; it is worth 4.6% on a census and its byte-identity was verified when it
landed. Nothing here proposes shipping without it.

### Half B — Q1, the machine question (the cluster)

Cells `B` and `C`: identical image, identical glibc, identical rustc,
identical `/build` path. **The machine is the only variable** — the first time
this project has had that.

- `B` — `docker run` on lefford, clone to `/build`, `cargo build --release -p
  hornvale`, print `sha256sum`.
- `C` — the same, as a Kubernetes `Job` on velaryon, dispatched from the Mac
  with `kubectl`.
- Compare the two hashes.

The Job must carry **both** tolerations and pin itself to velaryon:

```yaml
  nodeSelector: { kubernetes.io/hostname: velaryon }
  tolerations:
    - { key: platform, operator: Equal, value: x86,  effect: NoSchedule }
    - { key: gpu,      operator: Equal, value: "true", effect: NoSchedule }
  restartPolicy: Never
```

If the hashes match, run the probe on both arms as confirmation and compare
the CSVs — the same instrument Half A uses, so one comparison harness serves
both halves.

## 5. Preregistered predictions

Frozen before any arm runs (decision 0016). The Pyx's L1 prediction was
falsified and that miss was the campaign's most useful result, so these are
stated at the width they can be wrong at.

| Cell pair | Prediction | Confidence | If it fails |
|---|---|---|---|
| `A` vs `B` (flagged, glibc differs) | **identical probe output** | high — this is what 0033+0041 claim, and The Pyx already saw x86_64-Linux == aarch64-Darwin | Cross-platform byte-identity is violated *with* the flag, which would be a first-order finding and would redirect the campaign |
| `D` vs `E` (unflagged, glibc differs) | **identical probe output** — i.e. the codegen hypothesis is **wrong** | low-to-moderate. IEEE-754 `floor` is exactly representable, so a conforming glibc must return the same value; the hypothesis has always required a *non-conforming* libm | If they differ, the hypothesis is **confirmed** and 0063's four-month-old mystery has a named mechanism |
| `B` vs `C` (same image, machine differs) | **identical binary hash** | moderate. It is what 0090's oracle predicts, but 0090 has already been wrong once by generalizing from one host | Velaryon cannot author, 0063's single-platform ruling is reinforced on fresh evidence, and the *difference* is now diagnosable because only one variable moved |

**What the null proves.** If `D == E`, the codegen hypothesis is dead and
0063's divergence is *still* unexplained — a real result, and one that should
be recorded as closing a line of enquiry rather than as a disappointment. The
remaining candidates then narrow to the two things a container does **not**
equalize: the CPU microarchitecture and the kernel.

**What no result here can establish.** That velaryon may author goldens. That
is decision 0079 territory and a separate call.

## 6. Non-goals

- **Building a custom image, running a registry, or touching lefford's trust
  store.** D1 removes the need for all three.
- **A persistent build cache or a general-purpose cluster build system.** D5.
- **Changing `target-cpu=x86-64-v2`.** The unflagged builds are throwaway.
- **Changing 0079's hostname guard** or letting velaryon commit anything (D6).
- **Explaining why lefford's two Pyx builds hashed identically while the
  Mac's differed.** Still unexplained, still deliberately not guessed at; it
  is not on this campaign's path.

## 7. Verification

- Every arm writes its evidence to a file under `/tmp/twin-evidence/` on the
  host that ran it; nothing is read from a pipe.
- Each arm prints, and the write-up records: `rustc -V`, `ldd --version`,
  `uname -m`, the resolved SHA, the build path, and the `sha256sum`.
- The probe comparison is `diff` of two CSVs plus an explicit seed-681
  readout, matching The Pyx's method so the two campaigns' numbers are
  comparable.
- A `Pending` Job is treated as a **failure to investigate**, never as a slow
  build — that is what a missing toleration looks like.

## 8. Definition of Done

- [ ] Half A run; `A`/`B`/`D`/`E` evidence retained.
- [ ] Half B run, or explicitly recorded as blocked with the reason.
- [ ] §5 predictions compared against results; every miss labelled as a miss.
- [ ] A decision record: what Q2's answer is, and whether Q1 changes 0063.
- [ ] Chronicle entry + `SUMMARY.md`; retrospective with followup table.
- [ ] Book freshness sweep; Confidence Gradient re-scored if a determinism
      bet moved.
- [ ] `make gate` green before merge.
