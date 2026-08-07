# Retrospective — The Sighting

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-sighting.md): anchors embed into
lattice cells, symmetric integer shadowcasting narrows the `sensed` channel
sim-side, `vessel/plan/v1` gains `marks` with its first writer, and the
occlusion term the campaign was named for is dead in every world that ships
today.

Nine tasks, nineteen commits, eleven fix rounds across five reviewed tasks.
Two tasks reviewed clean on the first pass.

---

## The dominant lesson: introducing an invariant promotes every existing reader into a potential violation of it

This is the one to carry forward, and it generalises well past sight.

Before this campaign, "which creatures are co-located" was an ordinary read
with no invariant attached. The campaign introduced one — *some co-located
creatures must not be disclosed* — and in doing so **silently reclassified
every existing consumer of that data as a potential leak.** Nothing enumerated
that set. Not the codebase, because the set is not a type or a trait or a
module. Not my plan, which named the one consumer I had thought of. Not the
review checklists, which check the code a task wrote.

Task 5 took **five fix rounds** and found **four leaks**, one per round, each
one found by asking the same question of the previous fix: *does the reasoning
that justified this apply anywhere else?*

| round | leak | shape |
| --- | --- | --- |
| 2 | `examine` answered on a creature the player could not see | narrates |
| 3 | the needs report listed an unseen creature's needs | narrates |
| 4 | `provoke` / `soothe` named the creature in their success line — and a bare `provoke` silently *selected* the hidden one | narrates |
| 5 | `wait`'s motion narration announced an unseen arrival | narrates |

**All four were surfaces that NARRATED a creature, not surfaces that RETURNED
one.** Every sweep organised around *reads* — grep for the field, grep for the
accessor, enumerate the callers of the roster — missed the next one, because
the leaking sites did not read the thing they leaked in any way a grep can
see.

And "enumerate by what a site emits" is *still* not sufficient on its own,
which is the sharper half of the lesson. `narration.prose` reads no creature
field, appears in no grep for one, and is how **every** verb's text reaches the
wasm client. That is why gating at the **verb** was the right layer rather
than a convenient one: the verb is the last place where the narrowing is
expressible before the text becomes an opaque string.

**The transferable practice**, for the next campaign that introduces an
invariant over existing data:

1. Write the invariant as a sentence about *what the player may learn*, not
   about which function returns what.
2. Enumerate every site that could **communicate** that thing — including
   sites that never touch the underlying data.
3. Gate at the narrowest layer where the invariant is still expressible.
   Downstream of that layer everything is a string, and a string cannot be
   audited.

---

## Four secondary lessons, each with its evidence

### A mutation can pass for the wrong reason

Round 3's mutation "passed" — the test went red as intended. It went red at an
**earlier assertion** than the one it claimed to exercise, so the mutation
proved nothing about the guard it was aimed at. The reviewer noticed the
failure line, wrote a third mutation that reached the intended assertion, and
got the real signal.

A mutation is only evidence when the failure lands **where you predicted it**.
Record the assertion the mutation is supposed to kill, and check the failure
message names that assertion. "It went red" is not the observation; "it went
red *there*" is.

### A gate needs a positive control, and the arrival arm did not have one

A suppress-everything control — make the narrowing return nothing at all —
passed **442 green tests** on `wait`'s arrival arm until round 5 added a
positive control. Departures had one from the start; arrivals did not, and the
asymmetry was invisible because both arms were tested and both suites were
green.

The failure mode this protects against is the one this campaign kept meeting:
**a bug whose symptom is absence.** A gate that suppresses everything and a
gate that works correctly are indistinguishable to any test that only ever
asserts that something was hidden. Every negative control needs a paired
positive one, and the pairing should be checked per code path, not per feature.

### Reachability was measured rather than assumed

The `wait` leak was found by reading, and the obvious next move is to write a
reproduction. A 200-turn indoor sweep **never fired the arrival branch**. So
the leak was reported as **latent** — real in the code, unreachable by any
sequence we could produce — and pinned by feeding the narration function its
input vector directly, rather than dressed up as a repro that was not one.

Saying "latent, and here is the sweep that did not reach it" is more useful
than either "reproduced" (false) or "theoretical" (understated). A future
generation change that makes arrivals common inherits a test that already
holds.

### Every review finding traced to plan or spec text. That is three campaigns running.

Not one finding this campaign traced to an implementer's transcription error.
The Panes recorded the same thing; The Timekeeper recorded that eight of
sixteen instances came from its own plan text, four of them inside the detector
itself. This is now the base rate rather than an observation.

The two most expensive defects of this campaign are both **specification**
defects:

- The spec said the pane "draws only lit cells." That was never built and
  cannot be — there is no unlit concept for a cell at the wire, `CellKind` is
  closed at three variants with a warning against widening it, and the plan's
  own step only ever said to gate *creatures*. The implementer refused to build
  blank-rendering for an unreachable wire value and flagged it instead. It had
  already been reported to the owner in the wrong form before the correction
  landed. The spec now carries a dated correction rather than leaving a false
  claim in the campaign's own design document.
- The plan proposed a per-verb-class cost ceiling. The real axis is
  indoors/outdoors, and a probe pooling by sequence position overturned it —
  after which it emerged that **twenty of fifty pooled samples already exceeded
  the turn budget and the gate passed.** The wrong axis would have shipped a
  ceiling blind to the two indoor samples already over.

Both were caught by a reviewer *measuring the thing the plan asserted* rather
than checking that the code matched the plan. Task reviews that verify
conformance to the plan cannot catch a wrong plan; only re-derivation can.

---

## The cost lesson: a number can be right when taken and wrong an hour later

The campaign's first task measured a turn through the wasm ABI, which nobody
had ever done. The measured ratio is **1.57–1.78×** native, against the
**3.6–3.8×** that every browser figure in this repository was derived from.

The archaeology matters more than the number. The 3.6–3.8× figure was a
**valid** handle-to-handle measurement, correctly taken and correctly
committed. It was superseded **ninety-six minutes later** by a commit that
moved snapshot construction inside the timed handle. Eleven days passed before
anyone re-measured, during which the figure travelled into a metaplan, a
registry row, and several derived browser estimates.

Nothing in the gate ladder watches for this. A committed measurement carries no
statement of what it was measuring *through*, and the commit that invalidates
one is never the commit that cites it. The cheap mitigation, offered without
having built it: a measurement's committed text should name the code path it
timed precisely enough that a reader can check whether that path still exists.

A second instance in the same campaign: a committed doc claimed a sighting
derivation costs 42 µs. That was the anchor-placement sweep alone — **one line
of the derivation** — quoted as the cost of the whole. The real figure is
~8.5 ms dev / ~3.7 ms release, understated by about 195×. Found by
re-measuring, not by reading, and independently re-measured by the reviewer
before it was believed.

**Corollary, and it is now twice-evidenced:** a measured number's *scope* is
as perishable as its value. Re-measure a cited figure at the point of citation
when the citation is load-bearing.

---

## Process traps hit this campaign

### A report file was written into the main checkout, not the worktree

Task 3's report path was built from the repository root instead of from `pwd`,
so a git-ignored file landed in the primary checkout. It **looked filed** —
the writing agent's `ls` found it, at the path it had constructed — and it
would have died at teardown with nobody noticing. The main checkout was
verified clean afterwards and no parallel session's briefs were touched.

This is the same cwd trap that misplaced a worktree and a commit earlier in the
same session. The mitigation is mechanical and belongs in every subagent
dispatch: **build every scratch path from `pwd`, and `ls` it after writing.**
It was added to the dispatch preamble mid-campaign and the trap did not recur.

### The published client bundle was never rebuilt

Tasks 7 and 8 changed the client's TypeScript — creature marks, and the chart
schema allowlist — and **neither ran `deno task build`**, so
`book/src/gallery/vessel.js` still held the pre-campaign client through
eighteen of nineteen commits. `make vessel-check` cannot see this: it
type-checks sources and drives the wasm binary, and the bundle is an artifact
checked only by the close's `git diff --exit-code` over `book/src/gallery/`.

Caught at the close, which is where the ladder is designed to catch it, so the
process worked. But the window is a whole campaign wide, and any client task
whose work is meant to be *visible in the book* should rebuild the bundle in
its own commit rather than leaving it to the close. Worth a line in the client
directory guide.

### The gate was contended and it was noticed rather than trusted

Gate wall time ranged 311–574 s with `cpu_ratio` between 7.39 and 4.79 on a
suite that grew by four tests. That is contention, not regression, and Task 6's
measurement was deliberately re-taken on a quiet box (load 1.4–2.1) rather than
read off a contended run. The close's own gate ran 337 s at `cpu_ratio` 7.82.

---

## Deferred minors, promoted from the campaign ledger

These were reviewed, judged not worth a fix round, and would otherwise have
died with the worktree.

- **Task 1.** `turn_cost.rs` states `3.685 / 0.980` and labels 0.980 a "fresh
  three-run average". It is the average of the **stale** values. Fresh is
  0.993, giving 3.71× — the same conclusion. A number relabelled without
  recomputation, which is a miniature of this campaign's cost lesson.
- **Task 2.** `the_embedder_invents_no_more_than_the_graph_leaves_free`'s
  degrees-of-freedom half is structurally vacuous: `choices += 1` fires at most
  once per `ids()` iteration, so `dof <= ids().len()` cannot fail. The
  faithfulness half of the same test is real; the dof half asserts arithmetic.
- **Task 2.** `reaches` has an undocumented precondition — when `to` is an
  impassable neighbour of the frontier it returns `false` immediately, aborting
  the flood. Harmless only because `is_faithful` pre-checks passability, which
  is a coupling nothing states.
- **Task 3.** `is_symmetric` names a property it does not check. It is a
  centre-in-band predicate, and at call sites the name reads as an assertion
  about the whole algorithm.
- **Task 3.** Private helpers are unevenly type-audit tagged (`edge`,
  `first_col`, `last_col`, `transform` untagged; the rest tagged). No gate cares
  — the audit is a `pub`-boundary lint — but the file is inconsistent with
  itself.
- **Task 5.** `purview`'s `debug_assert` is **release-invisible by design**. If
  a client ever calls `purview` from indoors, a release build discloses and
  nothing says so. The committed doc says the assert "costs nothing in release"
  and never states the flip side. One sentence is owed.
- **Task 8.** Roughly eleven other pre-existing `scene/surrounds/v1` references
  remain repo-wide — CLI help text, module docs, and one intentionally-named
  golden, `surrounds_v1_bytes_are_pinned`. Outside the task's scoped three
  files and predating the campaign. Worth a sweep someday.

## One non-finding worth recording as a non-finding

An **unsensed** creature still perturbs a **sensed** creature's felt state,
through the roster that the co-location affect term sums over. That is presence
without identity — the same class as the ambient "stirred" count — and it is
arguably the behaviour we want: a room feels crowded whether or not you can see
who is crowding it. Recorded so that a later reader who finds it does not
mistake it for a leak the campaign missed.

## The autopilot ledger

Four decisions auto-resolved against standing policy, one of which overturned
its own framing (the keystone moved from a mechanism question to an authority
question under an ideonomy pass, which is the pass earning its keep). Two hard
stops were honoured: spec review, and this close. The full reasoning is in the
campaign's decision ledger, summarised in the chronicle and in the spec's
§7.
