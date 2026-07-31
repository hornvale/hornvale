# The Twin — retrospective

**Campaign:** The Twin (TOOL-cross-host-assay) · **Closed:** 2026-07-30 ·
**Spec:** `docs/superpowers/specs/2026-07-30-the-twin-design.md` ·
**Decision:** [0091](../decisions/0091-glibc-does-not-explain-it-and-the-machine-does-not-matter.md)

Process lessons, not product. Both halves came back as predicted, which makes
this a thin retrospective on results and a thick one on how the experiment got
cheap.

## The withdrawal named the wrong requirement, and that cost a campaign

The Pyx withdrew the codegen experiment as unsound. The withdrawal was
correct — comparing an x86_64 host against an aarch64 Mac moves only one arm,
because the flag is `cfg(target_arch = "x86_64")`-scoped. But the sentence it
wrote down was *"needs two x86_64 Linux **hosts** with different glibc"*, and
that is not what it needs. It needs **two glibcs**. A host was never the unit.

The upstream `rust:1.96.1` image carries glibc 2.41; lefford's host carries
2.36. One machine holds both. An experiment that had been filed as "campaign
two, needs a cluster" was actually twenty minutes on a box we already had.

**The lesson is about how a blocker is written down.** Saying "needs two
hosts" hid the real requirement inside an incidental one, and then the note
was believed on re-reading. The habit worth keeping: when recording why
something cannot be done, **state the property required, not the apparatus
imagined** — "needs a second glibc" would have been solved on sight.

## The anti-vacuity check was the highest-value step in the plan

Task 1 existed only to prove the diagnostic build differed from the shipping
one, by disassembling both and counting `roundsd`: **147 with the flag, 0
without.**

Had it been skipped and `RUSTFLAGS` not actually overridden
`.cargo/config.toml`'s `target.'cfg(...)'` entry, half A would have compared
the shipping configuration against itself and returned a reassuring,
meaningless "identical" — and the campaign would have reported a null result
about a hypothesis it had never tested. There would have been no symptom.

This is the same shape as The Timekeeper's alarm that could never fire, and
the same fix: **require RED before trusting GREEN**, mechanically, as a step
in the plan rather than as a habit.

## The instrumentation error was worth more than the result

An environment probe reported `rustc 1.77.1` for two of the four arms. The
builds were fine; the probe ran `rustc --version` from `/tmp/twin-evidence`,
outside any repository, so rustup answered with lefford's **default**
toolchain rather than `rust-toolchain.toml`'s pin. Confirmed by
`rustup show`: `1.96.1 … (overridden by '/tmp/twin-host/rust-toolchain.toml')`.

Two things follow, and the second is the campaign's best output.

First, the immediate discipline: **an environment probe must run where the
work ran.** Reporting a toolchain from a different directory than the build is
reporting a different fact than the one claimed, and it would have put a wrong
environment table into a decision record.

Second — the project's rustc pin is **conditional on the working directory**.
Step outside the tree and the canonical box silently offers a three-year-old
compiler. That is a live determinism hazard today, independent of any history,
and it is a much more plausible cause of 0063's original divergence than the
glibc story just falsified. It cannot be confirmed (the AWS box was
decommissioned by 0063 itself), so 0091 records it as a candidate, not a
finding.

**Both of this session's most useful findings came from a broken measurement
rather than a working one** — this, and The Pyx's binary-path discovery, which
also surfaced only because a guard test forced the question "what exactly does
this protect?" Neither was on any plan.

## Two shell idioms that produced false facts

Both were reported to Nathan as findings before being caught.

1. `command -v $t && $t version --short || echo "-- absent"` reported kubectl,
   docker, podman and skopeo as **absent from the Mac**. Modern kubectl removed
   `--short`, so the version call failed and the `||` branch fired — `cmd && A
   || B` prints `B` whenever *A* fails, not only when `cmd` is missing. This
   produced a fabricated blocker and a request that Nathan choose between
   installing tooling and copying cluster credentials, neither of which was
   needed.
2. `set -euo pipefail` plus `ldd --version | head -1` aborted a script with
   SIGPIPE (exit 141) *after* its real work had completed, which briefly read
   as a failed run.

**Separate presence from capability.** `command -v` answers "is it there";
anything else is a second question with its own failure modes. When a probe
says a tool is missing on a machine its owner uses daily, the probe is the
likely defect.

## What the container actually bought

Worth stating because it generalizes past this campaign. Every previous
cross-host comparison — lefford vs AWS in 0063, lefford vs the Mac in 0090 —
varied the machine, OS, glibc, rustc, and absolute build path *at once*. Five
differences and one observation cannot name a mechanism. The container fixes
four, which is why half B is the first comparison in this project's history
where a disagreement would have been **diagnosable**.

The container is an instrument, not a deployment. Nothing about how Hornvale
builds or ships changed.

## Follow-ups

| # | Item | Why | Where |
|---|---|---|---|
| 1 | Make the rustc pin independent of the working directory, or make a wrong-toolchain build loud | lefford's default is 1.77.1 and the pin only applies inside the tree; a silent three-year toolchain gap on the canonical box is a determinism hazard regardless of whether it caused 0063 | TOOL-cross-host-assay |
| 2 | Carried from The Pyx: migrate 0079's guard from hostname to toolchain fingerprint | Now better motivated — 0091 shows the *environment*, not the machine, is what determines output | TOOL-cross-host-assay |
| 3 | Carried from The Pyx: add `--out` to `hornvale lab run` | Every probe run still writes ~175 files into the goldens tree that must be deleted by hand, invisible to `git diff --exit-code` | TOOL-cross-host-assay |
| 4 | Carried from The Pyx: diagnose `census-run.sh status` vs the lock | Unchanged; `make ci`'s contention suppressor asks the same question | TOOL-cross-host-assay |
| 5 | An output-level (census) comparison on velaryon, if authorship there is ever wanted | 0091 proves binary identity only; no world was generated on velaryon | needs a decision first |
| 6 | Still unexplained: why The Pyx's two lefford builds hashed identically while the Mac's differed | Not on this campaign's path and still deliberately not guessed at | open |

## What did not happen

No image built, no registry, no trust-store change, no PVC, no authored
artifact, and `target-cpu=x86-64-v2` untouched — all §6 non-goals, all
respected. Velaryon authored nothing and 0079's guard is unchanged.
