# The Whetstone — retrospective

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-whetstone.md).

## 1. The stale baseline pointed at the wrong crate, confidently

`docs/timings/test-baseline-MacBookPro.tsv` is committed, is the thing
`make ci` alarms against, and reads like the authoritative answer to "what is
slow". It named `hornvale-book` as the hot crate — 4,040 s across 39 tests,
43% of the suite. A fresh measurement on the same box named
`hornvale-vessel`, at 39%, with book fourth.

Both were true when recorded. The baseline was written 2026-07-30 under the
loadavg 42–63 that The Timekeeper's own retrospective documents, and its rows
are inflated roughly sevenfold and *unevenly* — contention does not scale every
test by the same factor, so it reorders them. Ten days of campaigns then moved
the actual costs underneath it.

**The lesson is not "distrust the baseline"** — it is doing its job, which is
alarming on *change*, not ranking by *cost*. The lesson is that a ranking read
off it is a different question than the one it answers, and needs its own
fresh, quiet-box run. Cost 20 minutes here, spent profiling the wrong crate
first.

**Follow-up:** the file's header explains the format but not its provenance.
A `# recorded at loadavg N` field, written by `ci-record`, would make a
contended baseline self-identifying. Related to The Timekeeper's open blind
spot (1), which is the same gap seen from the other side.

## 2. Five campaigns each fixed this correctly and each leaked through the same hole

The five `[profile.dev.package.hornvale-*]` entries were not wrong. Each was
added by a campaign that profiled its own hot crate, measured a real
improvement, and wrote down what it measured. The comments are good comments.

And the aggregate was a systematic under-fix, because **generic code
monomorphises into the crate that instantiates it and compiles at that crate's
optimization level**. Marking the kernel optimized never touched the copy of
`BTreeMap::get` that un-marked `hornvale-demography` had stamped out for
itself. The measured size of the leak: 70% of the heaviest test's self time in
`core` and `alloc`, with `hornvale_vessel`'s own frames at 0.02%.

This is a **failure mode of local fixes that no local review can catch** — from
inside any one campaign, the fix looks complete and the measurement confirms
it. What surfaced it was not better reasoning; it was bucketing a flamegraph
**by owning crate** rather than by function, which made a number visible that
no per-function view puts next to itself.

**Generalise it:** when a per-item mitigation list reaches its fourth or fifth
entry, that is evidence about the *category*, not a request for a sixth entry.
Ask what the list is a proxy for.

## 3. The cost everyone was avoiding had not been measured

The per-crate shape exists to protect iteration speed: optimizing everything is
"obviously" slower to compile. Measured, a full workspace test rebuild was
771 s optimized against ~780 s unoptimized — free, because an optimizer that
deletes code leaves codegen and the linker less to do.

Nobody had checked. The tradeoff was treated as known for five campaigns
running, and it was not real. This is the campaign-autopilot verification rule
(PROC-18/19) in a new costume: the unverified claim was not in a spec, it was
in the *shared assumption that kept a spec from being written*.

**The check:** before accepting an architectural constraint of the form "we do
X the awkward way because Y would cost too much", find the measurement of Y.
If there isn't one, that constraint is a hypothesis.

## 3b. `exec_time` is elapsed time, and the A/B nearly reported a phantom

The per-crate before/after table showed `hornvale` (the CLI crate) getting
**slower** — 520 s to 643 s — while everything else halved. It was almost
written up as a real cost of the change.

It is not. nextest's `exec_time` is each test's *wall* duration, and ten tests
share ten cores; a test that waits is billed as though it worked. The CLI's
heavy tests spawn the `hornvale` binary (and, in one case, `bash
scripts/gate-fast.sh`, which runs cargo) as **separate processes**, so they are
the ones most exposed to how busy the box is — and the after run is busier per
second precisely because it finishes in half the time. Run alone, the largest
of them costs 12.7 s, against the 30 s the before arm billed it and the 43 s
the after arm did. Both in-suite figures were contention.

**The lesson:** a summed-`exec_time` column looks like a CPU budget and is not
one. It ranks tests within a run; it does not account for processor time
across runs whose parallel occupancy differs. The suite's own wall clock is the
number that survives the comparison, and a suspicious per-item move should be
re-measured **solo** before it is explained.

## 4. Writing the numbers before running them

The chronicle's results table was drafted with plausible before-numbers in it
while the before-run was still executing, and had to be corrected from the
actual output. Nothing shipped wrong — but the draft existed, and a draft that
looks finished is exactly how PROC-18 landed four times across three campaigns.

**What worked:** the A/B arms were run and recorded as separate JSON files
before any prose was written about them, so the correction was mechanical.
**What to do next time:** leave the cell literally empty, not plausibly filled.
An empty cell cannot be shipped by accident.

## 5. Mutation-proofing caught nothing, and was still worth it

Three fixes, three mutation proofs (`max`→`min` on the ledger read, an axis
swap in a capacity loop, plus the pre-existing suite as the byte-identity
oracle). All three went RED as intended; none revealed a defect.

That is the expected outcome and not a reason to stop. The proofs are what
license the phrase "bit-identical" in the chronicle. Without them the claim
would rest on the same reasoning that produced the code, which is not
independent evidence. Cost: about four minutes each.

## 6. What was left on the table, deliberately

- **The census is unmeasured.** It is 22,000 processor-seconds per run, on
  lefford, and every one of these fixes should help it — the world build is
  most of what it does. But a census is an explicit authorization carve-out and
  runs on another box. Claiming a census speedup without running one would be
  the exact failure this retrospective's §3 is about, pointed the other way.
- **`ResourceVector`'s representation.** The kernel's own dense-index rule says
  it should be an array; its `PartialEq` currently distinguishes "axis recorded
  as 0.0" from "axis absent", so converting it changes meaning. Recorded as an
  idea-registry row rather than smuggled into a perf pass.
- **The gate's non-test half.** `make gate` is fmt + clippy + type-audit +
  nextest + doctests. Only the nextest half was measured and attacked. Clippy
  over `--all-targets` is a second full check-build of the workspace and was
  never put on the scale.
