# 0848. cargo-sweep reclaims dead build generations, and only by hand

**Status:** Accepted (2026-09-07) · **Decider:** Nathan ·
**Relates:** [0004](0004-no-new-dependencies.md) (the dependency allowlist,
which this does not touch),
[0040](0040-cargo-nextest-is-the-gates-test-runner.md) (the dev-tool pattern
this follows), [0148](0148-a-merge-runs-four-phases-and-probes-run-by-hand.md)
and [0426](0426-the-heavy-tier-is-a-phase-of-the-queue-again.md) (expensive
maintenance stays off automatic paths); `CLAUDE.md` (Commands)

In the context of six campaign worktrees having grown to 144 GB against a
183 GB checkout, facing a `target/` that cargo grows without bound and never
reclaims, we decided that **`cargo-sweep` is an adopted dev tool with `make
sweep` / `sweep-dry` / `sweep-exact` targets, run only when a human types
one**, accepting that nothing warns you when a tree has re-accreted.

## The measurement

`target/debug/deps` is keyed by a metadata hash, so every *structural*
configuration ever built keeps a permanent copy of every artifact it produced.
On this repo's `main` checkout:

```text
executables in target/debug/deps : 1152
distinct test targets            :  307
dead copies                      :  845   (73%)
worst single target (`hornvale`) :   40 generations, ~20 MB each
```

**What multiplies them is structural churn, not editing.** One knob at a time
on a scratch crate, counting retained executables:

```text
baseline                       2
package version bump           4
RUSTFLAGS change               6
feature added                  8
different toolchain           10
```

Editing a source file overwrites in place and adds none — so the accrual
tracks toolchain upgrades, `Cargo.lock` churn and `.cargo/config.toml` changes,
at campaign cadence rather than per-commit. Reclaimed on adoption: `main`'s
`target/` 22 GB → 8.2 GB, plus 1,022.92 MiB recursively across `tools/*` and
`clients/*`; the whole checkout went 183 GB → 9.0 GB once the worktrees were
also released.

**This does not violate [0004](0004-no-new-dependencies.md).** cargo-sweep is a
developer tool installed with `cargo install cargo-sweep` or `brew install
cargo-sweep` — not a crate the workspace links against, absent from every
`Cargo.toml`. It joins `scc`, `shellcheck`, `yq`, `cargo-nextest` (0040) and
the out-of-workspace runners under `tools/`. `make sweep-check` fails with an
install hint, exactly as `nextest-check` does.

**Cargo cannot do this itself.** `cargo clean` takes no age or reachability
option (it is all-or-nothing, and a full clean costs the 771 s cold build the
worktree pool exists to avoid). `-Z gc` exists but is nightly-only and governs
the *global registry* cache, not `target/`; this repo pins stable 1.96.1.

## Why no automatic home

Both candidates were measured and refused, and the refusals are the decision's
substance:

- **Age-based inside `worktree-take`.** `--time N` deletes by mtime, and a
  parked worktree's *live* artifacts are as old as its last build. On a member
  idle longer than the threshold every artifact is old, so the sweep destroys
  the warm `target/` the pool exists to preserve. Verified on a tree backdated
  60 days: `--time 7` proposes the entire tree, working set included.
- **Stamp-based inside `worktree-take`.** `--stamp` → build → `--file` is
  mark-and-sweep, and it is exact and age-independent — verified reducing 10
  generations to the 2 live ones while `cargo build` still reported `Fresh`.
  But the mark must be **complete** or it collects live objects: verified that
  `--stamp` → *partial* build → `--file` proposes deleting the live test
  binaries. `worktree-take` deliberately does not build, so it cannot supply a
  complete mark. `prewarm` does, but `worktree-take` runs it only on a *cold*
  worktree — the one with nothing to reclaim — and tells a recycled member "no
  prewarm needed", so the wiring would serve the wrong case.

So this follows the `seam-guard` arrangement (0148): a reclamation pass whose
guarantee moves at campaign cadence is not a phase of any gate.

**Consequence.**
- `SWEEP_DAYS` defaults to **30**, not 7. A live campaign worktree is rebuilt
  continuously so 7 would be safe for it, but the threshold cannot distinguish
  "old but reachable" from "old and dead", and 30 keeps a month-idle worktree
  warm. Lower it deliberately when reclaiming a tree you accept rebuilding.
  The variable is namespaced because bare `DAYS` is already `board-digest`'s,
  defined nowhere so the tool applies its own 14-day default — a `DAYS ?= 30`
  here would have silently retimed that command.
- `sweep-exact` is workspace-scoped and **not** recursive: its build does not
  cover `clients/*/target` or the `tools/*` crates, so a recursive `--file`
  would read their untouched artifacts as garbage. Plain `make sweep` covers
  those.
- **Nothing reports re-accretion.** There is no CI (0125) and no gate phase, so
  a tree at 40 GB looks exactly like a tree at 4 GB until someone runs
  `make sweep-dry`.

**See also.** `CLAUDE.md` (Commands, the sweep block);
`scripts/worktree-take.sh`'s baked-path comment, which is why artifact *reuse*
across worktrees was rejected in the same sitting: `CARGO_MANIFEST_DIR`,
`CARGO_TARGET_TMPDIR` and `CARGO_BIN_EXE_*` are baked at compile time, and a
shared or cloned `target/` hands one worktree another's paths while cargo
reports `Fresh` — verified, and silent when the source path still exists.
