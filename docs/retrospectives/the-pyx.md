# The Pyx — retrospective

**Campaign:** The Pyx (TOOL-cross-host-assay) · **Closed:** 2026-07-30 ·
**Spec:** `docs/superpowers/specs/2026-07-30-the-pyx-design.md` ·
**Decision:** [0090](../decisions/0090-the-canonical-host-is-audited-not-assumed.md)

Process lessons, not product. The product is in the decision record and the
chronicle.

## The ideonomy pass inverted the recommendation, and that was the campaign

The session opened on a hardware question — a Kubernetes node had become
available, could it run censuses? — and the first recommendation was to
recruit it. One ideonomy pass (abstraction-lift + cross-domain
re-instantiation over a map organon) overturned that before any work started,
on three moves worth naming because each is reusable:

- **Materiality.** Compare the *binaries*, not the outputs. Output equality is
  implied by binary equality, and a binary comparison costs seconds. The
  expensive measurement had been assumed to be the only one available.
- **ISO 5725 (cross-domain: analytical chemistry).** *Repeatability* and
  *reproducibility* are different claims. The project had been enforcing the
  first and describing it as the second. Naming the distinction produced the
  control experiment — lefford against its own past — which nobody had ever
  run and which needed no new infrastructure at all.
- **Cardinality-in-time.** The interesting second apparatus was not another
  machine; it was the *same* machine later.

The campaign that shipped shares no tasks with the campaign that was about to
be planned. **The pass earns its keep on the decisions that feel settled** —
this one felt like a straightforward infrastructure question.

## The most valuable result was the falsified prediction

Three predictions were frozen in spec §5 before anything ran. Two held. The
third — that the binary comparison would fail benignly on embedded build paths
— was wrong, and it was the only one that changed what the project can do
next: binary hashing is a *live* cross-host oracle, so qualifying a machine
costs seconds rather than a census.

Had the prediction not been frozen, the green result would have read as
unremarkable confirmation rather than as a discovery. **Preregistration is
what converted "the hashes matched" into "the cheap oracle is available and
we assumed it wasn't."**

**And then the campaign drew the wrong conclusion from its own good result —
after a day spent warning about exactly this.** The explanation reached for
was "release carries no debuginfo, so no source paths." It was written into
decision 0090, into a chronicle, and into a guard test, and it is false.
Running L1's procedure on the *second* host produced **two different binaries**
(`5bae5217…` vs `2b8a4f65…`), each embedding its own build directory, with
`debug = false` throughout. The paths come from two production
`env!("CARGO_MANIFEST_DIR")` expansions (`cli/src/main.rs:1150`,
`windows/lab/src/blackbox.rs:23`) and have nothing to do with debug
information.

The corrected claim is narrower and carries a condition: binary identity is an
oracle **only when both hosts build at the same absolute path** — which a
container gives for free, so the oracle survives and campaign two is where it
becomes usable. Decision 0090 carries amendment 2. Why lefford's two builds
contained no such string is **still unexplained** and is deliberately not
guessed at.

**The generalization was the defect, not the measurement.** L1 ran on one host
and the conclusion was written as though it were a property of the toolchain.
Every measurement in this campaign stands; the sentence built on top of one of
them did not. **A single-host result is a fact about that host until a second
host says otherwise** — which is the campaign's own thesis, applied one level
up and missed on the first pass.

## Three claims were drafted confidently and were wrong

All three were caught by running a command, and all three had already been
written into a document:

1. **Thread-count divergence.** The leading hypothesis for 0063's unexplained
   disagreement was that `available_parallelism()` differs between a 40-core
   and a 24-core box, changing a reduction order. Reading
   `windows/lab/src/runner.rs:210-258` falsified it in two minutes: workers
   own contiguous seed ranges, results land in per-offset slots, and every row
   is computed independently. Core count changes *who* computes a row, never
   the row.
2. **`preregistration_guard`.** A content-grep for the identifier found
   nothing, which suggested the guard did not exist; it exists only as a
   *filename*. Then the name suggested it enforces study hypotheses; it
   actually scans `#[ignore]` reason strings. **Two wrong conclusions in
   opposite directions about the same file**, from a grep and a name.
3. **Where the probe writes.** Spec D5 asserted the probe "publishes nothing
   into `book/src/laboratory/generated/`" — and that claim passed the G3
   review. It is false: `lab run` takes no output flag and always calls
   `publish`, which wrote **175 files** into the goldens tree. Because the
   directory is untracked, `git diff --exit-code` — the repo's standard
   freshness check — cannot see it.

The third is the one to generalize. It was verified only because the *plan*
needed a literal command, which forced the question "what path, exactly?"
The spec had been approved with the claim in it. **Drafting-time verification
catches what review does not**, which is the standing autopilot rule, and this
campaign is another instance of the rule being needed rather than a
counterexample.

## `census-run.sh status` reported "no heavy run in progress" during a run

A completion watcher polled `bash scripts/census-run.sh status` and broke out
after one 30-second iteration, reporting the run complete. The run had 14
minutes left. The log shows the run holding `/tmp/hv-census.lock`, while
`status` reports on the *claim* (`/tmp/hv-census.claim`) — so a
`census-run.sh` invocation that takes the lock is not necessarily visible to
`status`.

This did no harm here (the log tail was read after the run finished, and the
diff was taken later still). It matters because `status` is the documented way
to ask whether the canonical box is busy, and because `make ci`'s contention
suppressor asks the *same* question — a heavy run invisible to `status` is
also invisible to the timing alarm's "am I contended?" check. **Not
investigated in this campaign; filed as a followup rather than fixed, because
diagnosing it properly means reading the lock/claim interaction across
`census-run.sh`, `heavy-run.sh`, and `census_claim.rs`.**

## The campaign ran under a colliding ID for its whole life

The backlog row was minted as **TOOL-24** by reading
`WORKFLOW_IMPROVEMENTS_PLAN.md`, whose list stops at TOOL-23, and taking the
next integer. `TOOL-24` was already the idea registry's **"World-derivation
performance — the recompute pattern."** The ID travelled through the spec, the
plan, the study JSON's description, decision 0090, and the followup table
before the close caught it. Renamed to `TOOL-cross-host-assay`.

Two things made it survive so long. The obvious one: the backlog file is not
the ID authority, the **registry** is, and the two disagree because the
registry has kept minting TOOL rows that the backlog never listed. The
subtler one: `cli/tests/fixtures/registry-numbered-ids.txt` *contains*
TOOL-24, so the check for "no new numbered ID" would have stayed green — the
frozen list makes a taken ID look available to anyone who greps it for
permission rather than for occupancy.

Decision `0026-slugs-not-numbers` already prescribes the fix, and following it
from the start would have avoided this entirely: **a new ID should be a slug,
because slugs cannot collide by arithmetic.** This lesson is already in the
project's memory as "scan the registry category, don't grep — IDs may be
banked," and it still happened, because the number was derived from a file
that looked authoritative and was not.

## Two path slips, both caught by the tooling

The decision ledger was first written to the main checkout's path (the Edit
failed — the file did not exist there, which is exactly the scratch-in-worktree
rule protecting itself), and a backlog edit did land in the main checkout
before being reverted. Neither reached a commit. Working across a main
checkout and a worktree with near-identical absolute paths is a live hazard;
the mitigation that worked was checking `git status` in the main checkout
after edits, not care at edit time.

## Follow-ups

| # | Item | Why | Where |
|---|---|---|---|
| 1 | Migrate 0079's guard from hostname to **toolchain fingerprint** | A hostname cannot catch lefford drifting from itself — the exact failure L0 was built to detect. L0+L1 together are what a fingerprint would assert | TOOL-cross-host-assay |
| 2 | Add `--out` to `hornvale lab run` | A probe should not have to write 175 files into the goldens tree and then delete them; the untracked directory is invisible to `git diff --exit-code` | TOOL-cross-host-assay |
| 3 | ~~Test that release builds stay debuginfo-free~~ → **SHIPPED as a guard on the real property**: `cli/tests/build_path_embedding.rs` freezes the two production `env!("CARGO_MANIFEST_DIR")` sites | The debuginfo framing was wrong (see above) and a test built on it would have sat green while the oracle broke. What actually bounds the oracle is how many places embed an absolute build path | done |
| 4 | Diagnose `census-run.sh status` vs the lock | `status` reported idle during a live run; `make ci`'s contention suppressor asks the same question | TOOL-cross-host-assay |
| 5 | Campaign two — velaryon recruitment | Gated on this campaign's green result; now starts with a binary-hash comparison, not a census | spec §6 |
| 6 | Test the §2 codegen hypothesis **in campaign two**, not against the Mac | The rebuild-and-re-probe as originally scoped could not have settled it: `target-cpu=x86-64-v2` is `cfg(target_arch = "x86_64")`-scoped, so the Mac arm never carried it and aarch64 floors in hardware (`frintm`) regardless — removing the flag moves one arm and not the other, and never reproduces 0063's two-x86_64-Linux-boxes configuration. Needs two x86_64 Linux hosts with **different glibc**, both unflagged; velaryon in a container is that instrument, and pins glibc deliberately | spec §7 (corrected) |

## What did not happen

No container image, no Job manifest, no registry push, no change to 0079's
guard, no supersession of 0063 — all §6 non-goals, all respected. The audit
did not run velaryon, and the decision record says so at that width rather
than implying a second authoring host has been qualified.
