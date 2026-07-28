# 0079. Census goldens are authored on one host, and it is enforced

**Status:** Accepted (2026-07-26) · **Decider:** Nathan · **Refines:** [0063](0063-census-regen-is-local-again.md)

Decision 0063 established that census goldens regenerate locally on the
canonical box rather than on AWS, and recorded *why* one box: the machines are
not byte-identical. On ~0.1% of census values — discrete-count metrics like
`divergence-magnitude`, where a count is settled by a comparison in the
**compute** path, upstream of the quantize-at-emit boundary ([0033](0033-cross-platform-byte-identity-via-quantization.md))
— two boxes disagree by one unit. Quantization absorbs last-ULP float noise at
serialization; it cannot un-flip a count already decided.

**Nothing enforced it.** Setting `HV_CENSUS=1` on any machine ran the census
and rewrote the committed goldens. Worse, every script comment and campaign
plan restating "this box is the single canonical platform" was written *on*
that box, so read from anywhere else the same sentence asserts that whatever
machine you are sitting at is canonical.

That is not a hypothetical. During The Waterline, asked to regenerate the
census, the agent was about to run it on the wrong machine and was stopped only
by the owner asking "are you running this on Lefford?".

The failure mode is silent. A wrong-host run does not error; it commits
~1-in-1000 wrong values, which then drift-check green forever — the exact
pathology recorded for drift checks generally.

## The ruling

The canonical host is declared once, in `scripts/census-canonical-host.txt`,
and enforced at **every** path that can write the committed goldens:

| entry point | guard |
|---|---|
| `scripts/regenerate-artifacts.sh` (`HV_CENSUS=1`) | shell, hoisted to the top of the script so a wrong-host run is refused in ~20 ms rather than after ~4 minutes of unrelated regeneration |
| `scripts/census-run.sh` | shell |
| `cargo run -p hornvale -- lab run <census study>` | Rust (`windows/lab/src/census_guard.rs`), checked in `cmd_lab_run` and again at `publish` |

The CLI path is the one that mattered most and was found last: it is the
command printed in CLAUDE.md's artifact-freshness section, so it is the path a
developer following the documentation takes.

**One declaration, two readers.** The shell guard reads
`census-canonical-host.txt` at runtime; the Rust guard bakes it in with
`include_str!`. Two independent copies of a hostname would be a defect waiting
to happen.

**Fails closed.** No `hostname` binary, an unexpected FQDN, an unreadable
declaration — all refuse. A false refusal on the canonical box is recoverable
in seconds; a false permit is a silent corruption that survives every future
drift check.

**Narrow by construction.** The Rust guard refuses only when *both* the study
is a census study *and* its output lands inside the committed goldens tree, so
an ordinary `lab run` of a scratch study keeps working everywhere, as does
`make rebaseline`'s routine publish of the non-census `the-chorus` study.

**Not overridable by environment.** `CANONICAL_CENSUS_HOST` is assigned
unconditionally from the declaration file, not defaulted from the environment.
Changing the canonical box means editing one line in version control, visible
in review.

## Consequences

- Regenerating the census from a laptop is now impossible rather than merely
  discouraged; the refusal message carries the correct cross-machine
  invocation, including the two traps that have each cost a wasted run
  (`HV_CENSUS_WORKTREE=canonical`, and passing a **SHA** rather than a branch
  name, because `HV_CENSUS_REF` feeds `reset --hard` and can otherwise land on
  a stale local branch of that name).
- A residual gap is recorded rather than closed: the Rust guard identifies a
  census study by name pattern (`the-census`, `census-of-*`). A future
  census-class study named outside that convention would slip past. Gating on
  the output path alone would be safer but would refuse `the-chorus`, which
  legitimately publishes into the same tree.

**Refined by [0081](0081-one-heavy-writer-per-box-claimed-at-the-write-seam.md)** (2026-07-27): one heavy writer per box, claimed at the write seam and serialized with a bounded, legible wait. Use `scripts/census-run.sh`.
