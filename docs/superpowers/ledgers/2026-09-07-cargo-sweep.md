# cargo-sweep adoption — decision ledger

Branch: `tooling/cargo-sweep` · Decision: **0848** · No spec: a bounded
dev-tool adoption on the [0040](../../decisions/0040-cargo-nextest-is-the-gates-test-runner.md)
pattern, not a design campaign. Branch named `tooling/*` after the existing
`tooling/ideonomy-fork` precedent rather than `campaign/*`, so it does not
consume a campaign slot or the worktree pool.

Occasion: a worktree cleanup found six campaign worktrees holding 144 GB
against a 183 GB checkout. Nathan: *"cargo-sweep sounds like a really good
idea. We do try to avoid new dev tools, but this seems like a no-brainer to
adopt given the nature of our workflow."*

---

### #1 [Q] — may a new dev tool be adopted at all?

**Question.** [0004](../../decisions/0004-no-new-dependencies.md) forbids new
dependencies; does cargo-sweep need Nathan's ratification as an exception?

**Decision.** No exception is needed. Adopted under the existing dev-tool
carve-out.

**Why (precedent).** 0040 settled this shape explicitly for nextest: a tool
installed with `cargo install` / `brew install`, absent from every
`Cargo.toml`, "is a *developer tool* … not a crate the workspace links
against", joining `scc`, `shellcheck`, `yq` and the `tools/` runners. 0004
governs the *link* allowlist. Followed 0040's install-hint convention too
(`make sweep-check`, modelled on `nextest-check`).

**Alternatives discarded.** (a) Hand-rolled pruning script — rejected: it
would need a retention heuristic of its own, and a wrong one deletes live
artifacts. (b) Wait for cargo to do it natively — rejected on measurement:
`cargo clean` takes no age or reachability option, and `-Z gc` is nightly-only
and governs the global registry cache, not `target/`; this repo pins stable
1.96.1.

**ideonomy passes / overturns.** 1 pass (abstraction-lift + procedure;
longevity, naturalness) / 0 overturns on this entry. Enrichment: the
*naturalness* axis is what prompted checking for a native cargo GC rather than
assuming none — the "man-made accumulation" framing asks whether the producer
could reclaim its own output. It cannot, verified above.

**Capture actions.** Decision 0848; `CLAUDE.md` sweep block.

---

### #2 [Q] — where does it run, and in which mode?

**Question.** Wire sweeping into an automatic path (a gate phase, or
`worktree-take`), or leave it manual?

**Decision.** Manual only — `make sweep` / `sweep-dry` / `sweep-exact`, run
when a human types one. No gate phase, no `worktree-take` step.

**Why (precedent + measurement).** 0148 and 0426 both price expensive
maintenance off automatic paths, and `seam-guard` is the standing example of a
check that "runs only when a human types `make seam-guard`" because its
guarantee moves at campaign cadence. Structural-churn accrual moves at exactly
that cadence — measured, editing a source file adds no generation at all.

**Alternatives discarded, both on measurement rather than taste.**
- *Age-based inside `worktree-take`* — on a member idle past the threshold
  every artifact is old, so the sweep eats the warm `target/` the pool exists
  to preserve. Verified on a tree backdated 60 days: `--time 7` proposes the
  whole tree, working set included.
- *Stamp-based inside `worktree-take`* — exact and age-independent (verified
  10 generations → the 2 live ones, `cargo build` still `Fresh`), but the mark
  must be complete: verified that stamp → *partial* build → `--file` proposes
  deleting the live test binaries. `worktree-take` never builds, so it cannot
  supply a complete mark. `prewarm` can, but `worktree-take` invokes it only on
  a *cold* worktree — the one with nothing to reclaim — and tells a recycled
  member "no prewarm needed". The wiring would serve the wrong case.

**ideonomy passes / overturns.** 1 pass / **1 overturn.** The pass's
abstraction-lift stripped the idea to *"a content-keyed cache with unbounded
retention needs a reclamation policy; age is a proxy for deadness,
reachability is the truth"* — i.e. mark-and-sweep GC, with `--time` as TTL
eviction. That named the entering candidate (stamp mode inside
`worktree-take`) as a **mark phase**, which immediately raised GC's own
precondition — an incomplete mark collects live objects — and the probe
confirmed it. The candidate was the design going into the pass and it did not
survive. Retained enrichment from the same pass: Docker's deliberate split of
`prune --until` (age) from `prune --filter dangling` (reachability) is why
*both* modes ship rather than one, and the *longevity* axis argued the natural
retention unit is the campaign boundary, not a clock — which is what makes 30
days a proxy rather than a principle.

**Capture actions.** Decision 0848 ("Why no automatic home"); Makefile comment
carrying both refusals so the next reader does not re-propose them.

---

### #3 [Q] — default retention threshold

**Question.** What should the default `--time` be?

**Decision.** 30 days, exposed as `SWEEP_DAYS`.

**Why.** A live campaign worktree is rebuilt continuously, so 7 would be safe
for it; a *parked* one's live artifacts are as old as its last build, and the
threshold cannot distinguish "old but reachable" from "old and dead". 30 keeps
a month-idle worktree warm. Measured on main, the two differ by 5.8x: `--time
7` proposed 21.44 GiB, `--time 30` proposed 3.70 GiB.

**Alternatives discarded.** `--installed` (keep only current-toolchain
artifacts) — measured "nothing to clean" here, because all three installed
toolchains match; it reclaims only after a `rustup toolchain remove`.

**ideonomy passes / overturns.** Covered by #2's pass (the *longevity* axis is
where the threshold question came from); no separate pass.

**Capture actions.** Decision 0848 Consequence; Makefile comment.

---

### #4 [Q] — variable naming

**Question.** Name the override `TIME`, `DAYS`, or something namespaced?

**Decision.** `SWEEP_DAYS`.

**Why.** Both shorter names are taken or hazardous, and this was caught by
running the command rather than by reading. `TIME` is GNU `time`'s format-string
environment variable, so `TIME ?= 30` inherits a set value and corrupts
`--time`. `DAYS` is worse: `board-digest` already passes bare `$(DAYS)`,
*defined nowhere*, so its tool applies its own documented 14-day default — a
`DAYS ?= 30` here would have silently retimed `make board-digest` to 30 days
with nothing failing. Verified after the rename: `make -n board-digest` still
expands to `digest` with no argument.

**ideonomy passes / overturns.** None — mechanical naming, not a design call.

**Capture actions.** Decision 0848 Consequence; this entry.

---

## Follow-ups

- **`sweep-exact` is workspace-scoped and not recursive**, because its build
  does not cover `clients/*/target` or the `tools/*` crates and a recursive
  `--file` would read their untouched artifacts as garbage. A future variant
  could stamp each of those roots and build each, making an exact sweep
  whole-repo. Not done: no measured need, and it multiplies the
  incomplete-mark hazard by the number of roots.
- **Nothing reports re-accretion.** There is no CI (0125) and no gate phase, so
  a tree at 40 GB reads exactly like one at 4 GB until someone runs
  `make sweep-dry`. A cheap `du`-threshold advisory in the `SessionStart` render
  or `make doctor` would close the observability half without putting the
  destructive half on an automatic path. Registry-worthy if it recurs.
- **`worktree-take`'s baked-path comment has rotted numerically** — it says
  "42 of the 46" files read `CARGO_MANIFEST_DIR` / `CARGO_TARGET_TMPDIR` /
  `CARGO_BIN_EXE_*`; the live count is 75 of 84. The mechanism is unaffected
  because the grep is derived at runtime, which is the design working as
  intended; only the prose is stale. Left untouched here to keep this branch to
  its subject.
