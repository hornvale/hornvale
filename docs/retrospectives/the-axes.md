# The Axes — retrospective

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-axes.md): a five-axis basis over a
74-name corpus, a sixth axis measured empty, a named prediction that held, and a
keystone corollary made executable that immediately caught eight violations.

## 1. The gate's own criterion could not fail, and nothing in review would have found it

The metaplan gated this campaign on a reconstruction test whose criterion —
*no collisions, no resisters* — is satisfied **perfectly by one axis with
twenty-one values, which is the enum.* A floor with no ceiling. It would have
passed on the degenerate solution and read as a result.

What is worth recording is *how* it was found. Not by reading the spec again;
the spec had been read several times. It came out of an `ideonomy-plain` pass on
the `cardinality` dimension prompt — "there is exactly one version of this idea,
or there are a billion" — which is a question nobody asks a criterion they are
about to adopt. The project already knows this shape
(`a-floor-without-a-ceiling`, in the operator's own memory index); the lesson
here is that **a structured perturbation found it and repeated reading did
not.**

Five passes ran. Passes 1 and 2 each produced an overturn — pass 1 reframed the
whole question from *spike vs preregister* to *fit vs test*, pass 2 reversed a
decision about `tolerance_liebig` from "re-key site" to "keep unchanged". Passes
3, 4 and 5 produced no overturns and progressively smaller findings. **The
overturn rate is the convergence signal**, not the finding count: passes kept
producing useful spec content long after they stopped changing any decision.

## 2. Four plan defects, all found by executing the plan, none by reviewing it

Every one was in the plan author's own text, and the plan had been self-reviewed
against the spec before it shipped.

| defect | how it surfaced |
|---|---|
| Task 2 committed a file that does not compile until Task 3 | violates two standing NEVER rules; caught on reading them, not on writing the step |
| the type-audit step sat in Task 6 | the gate fired in Tasks 1 **and** 3, the moment a `pub` item appeared |
| Task 4 compared axis *identifiers*, not values | satisfied by construction — the assignment writes all five axes for nearly every name |
| Task 7 was vacuous outright | nothing outside tests consumes `EnvironmentVector`, so the "byte-identity" check had no second term |

The Task 2 one is worth a sentence more, because the fix was free and the panic
was not. The plan insisted a red commit was needed to prove the freeze preceded
the fit. It was not: **the freeze was already committed, in the spec, dated** —
which is exactly where `CLAUDE.md` says preregistration lives. The test file
*mechanises* the freeze; it is not the freeze. Folding Tasks 2 and 3 cost
nothing.

The general shape, which this project keeps re-deriving: a plan step's claim is
only tested when someone runs it. Reviewing a plan compares a document to a
document.

## 3. Two false greens, both caught by a guard rather than by looking

**A mutation that never applied.** `cargo fmt` had rewrapped a single-line
constructor into eight lines, so a `python` replacement of the one-line form
matched nothing. The test then reported `ok` — and that `ok` looked exactly like
a robust implementation. Only the `assert old in s, "TARGET NOT FOUND"` line
distinguished them. This is the second time the project has recorded this exact
mechanism, and the assert is the entire defence.

**A vacuous range.** Checking whether absorbing `main` had moved the code the
fit derives from, the first query was `git log main..origin/main -- <paths>`,
which returned empty and read as reassuring. It returned empty because `main`
and `origin/main` *were the same commit*. The real range was
`campaign/the-axes..main`, which contains 36 commits. The habit that saved it —
already in this project's memory as `an-empty-diff-needs-a-positive-control` —
is to run the query against something known to be non-empty before believing a
blank result.

## 4. `git checkout -- <file>` reverts uncommitted work, and its absence read as a pass

Reverting a mutation with `git checkout -- windows/worldgen/src/lib.rs` also
reverted the **uncommitted structural test** in that same file. The next
mutation run therefore executed against a tree with no such test, and its
absence from the output was briefly read as the test having passed.

Two consequences, and the second is the sharper one:

- **Commit before mutating.** The retry did, and the demonstration then worked.
- **A test that does not appear in the output is not a test that passed.** The
  run reported `287 passed; 2 failed` and looked healthy. Nothing flags a test
  that has ceased to exist.

## 5. The blinding was lost, and saying so was better than the alternative

Spec §6.5's arm 2 called for the axes to be fitted on land and the marine names
assigned *without revising the axis list*. The implementer authored all 74 names
in one pass. The hold-out's blinding was gone before the arm ran, and the
implementer noticed only after the fact.

What was salvageable was not the blinding but the **containment question**,
which is a property of the committed data and indifferent to authorial intent:
did the sea use any axis, or any axis *value*, that no land name uses? That
survives the contamination, and comparing values rather than identifiers is what
made it a test rather than a formality.

Recorded in the test's own doc comment, not downgraded quietly. A weakened arm
that says so is worth more than a strong-sounding arm that does not.

## 6. A rule applied reactively is not the rule you wrote down

The collision check produced two failures — `ice`/`snowfield`, `desert`/`erg` —
and the fix was a genuine insight: a formation is a genus and must decline any
axis its own variants disagree on. That rule went into the module's
documentation as a general statement.

It was applied to exactly the two formations that had collided.

Six tasks later, the coarse-constrains-fine check found **eight** variants
contradicting their genus on substrate, every one of them a place the rule
applied and had not been applied. The documentation was accurate; the code was
not; and the gap was invisible because the two collisions the rule was born from
were both fixed.

**Writing a general rule in response to a specific failure is where this happens.**
The failure names two sites. The rule names a class. Nothing prompts you to
enumerate the class, and a doc comment stating the general form makes it *look*
enumerated.

## 7. A generated artifact mis-merged, and every guard that should have caught it was structurally blind

Absorbing `main` auto-merged `docs/audits/type-audit-report.md` and got **six
rows wrong** — `bare-ok(count)` came out 413 against a true 416. Git reconciled
two divergent count tables line by line instead of recomputing them, producing a
file that was internally plausible and false.

Three guards existed and none applied:

- `type-audit check` passes either way — the **lint** and the **report** are
  different things, and only the lint runs in the gate.
- the drift check compares the committed file against a regeneration, but the
  merge had already poisoned the committed side.
- **merge commits skip the pre-commit hook**, so `make quick` never ran.

This is `generated-artifact-has-no-merge` firing exactly as recorded, and the
only correct operation on that class of file is regeneration. Worth carrying
forward: **after any absorption, regenerate rather than trust the merge**, and
do it before the drift check rather than after, because the drift check cannot
tell a poisoned baseline from a clean one.

## 8. Three homonyms now live in one workspace, and one of them produced a wrong number

`Formation` is both climate's communities and `astronomy::moons::Formation`
(`GiantImpact`, `Capture`). `Substrate` is both `worldgen`'s four environmental
fields and `alchemy`'s metallic/organic/saline vector. And the legacy `Biome`
spelling disagrees with `Formation`'s on two names (`coral-reef` vs `reef`;
`epipelagic`, which is a *stratum wearing a formation's coat*).

The first one did measurable damage. A scan for domain-to-domain vocabulary
crossings returned four, which nearly became the premise of an architectural
argument. Reading the hits: two were doc comments, one was the homonym, and
**one** was a real crossing. The corrected figure changed the recommendation
from "a program" to "one cheap narrow change".

A campaign whose subject is naming discipline that leaves its own homonyms
unrecorded is leaving the next grep to fail the same way. Captured as
`DOM-kernel-owns-vocabulary` and in the follow-ups below.

## 9. The timing alarm fired twice on contention, and the third run settled it

The gate ran three times. The suite was green every time — 3555 passed, 124
skipped, zero failures — and the red was always `timings_alarm`.

| run | loadavg at start | `cpu_ratio` | wall | verdict |
|---|---|---|---|---|
| 1 | (I was building concurrently) | 5.81 | 501.7 s | WHOLE-SUITE +30% |
| 2 | 12.03 on 10 cores | 7.90 | 334.2 s | PER-TEST, 7 tests at 2.0–2.2× |
| 3 | 8.48, no other builds | **8.43** | **310.9 s** | **green, no alarms** |

This is CLAUDE.md's documented blind spot 1 firing for real, with numbers: the
guard asks only whether a *census claim* is held, so parallel agent sessions are
invisible to it and it enforces against thoroughly contended timings. Five
worktrees exist on a box whose stated working ceiling is two to three.

Two things made the diagnosis rather than the excuse:

- **`cpu_ratio` is the tell.** The documented saturating range on this host is
  8.25–8.50. Run 1 reported 5.81 — *less* parallelism achieved, not more work
  done. A real regression raises wall time at unchanged `cpu_ratio`; contention
  lowers `cpu_ratio`.
- **A uniform multiplier is not a pathology.** Run 2's alarm named seven
  unrelated tests in three crates, every one at 2.0–2.2×, and told the reader
  *"something specific went pathological in this test; investigate it
  directly."* Seven pathologies do not share a multiplier. The message is
  well-written for the single-test case and actively misleading for this one.

Two hypotheses were checked and ruled out before contention was accepted: the
baseline was **not** stale (recorded on this host one commit back, after
everything the branch absorbed), and it was **not** the wrong host's file
(`hostname -s` is `MacBookPro` and that is the file being read, so blind spot 2
was not in play).

**The baseline was not re-recorded to make the gate pass.** The green came from
a quiet box, and the re-record then happened automatically because The Sexton
folded the recorder into the gate. It verifies itself: the `<below-floor>`
aggregate moved **2837 → 2850**, exactly the thirteen sub-second tests this
campaign added, and every other row moved down or mixed. **The Fathom's F-12 is
genuinely discharged** — it asked for a step nobody was routed to, and there is
now no step to miss.

## 10. What held up well

**The freeze was genuinely frozen, and it held.** The ten predicted resisters
came back exactly, with no substitutions, and the compression bounds were
derived from corpus size — a number known before any assignment existed — rather
than from the fit's outcome. Nothing was retuned after unblinding. The one
correction made during the fit (the genus rule) was to the *assignment*, which
§4.4 explicitly leaves free, and it moved no bound.

**Every guard this campaign wrote was proven to fire by mutation**, separately
where an invariant had two halves. The one that mattered most: flooring
elevation in *both* `tolerance_liebig` and its local `eager` reference leaves the
pre-existing agreement test **green** while the new structural test fails. The
gap the spec claimed was real, and it was demonstrated rather than argued.

**The scope reduction was surfaced, not taken.** `EnvironmentNiche` would have
had no producer in this campaign — the F-5 shape this program has now catalogued
three times — and the decision to defer it was Nathan's, not the implementer's.

## Follow-ups

**A-1 — `EnvironmentNiche` waits for a consumer** (campaign 2). The invariant
that makes the addition safe is pinned and proven, so campaign 2 inherits a
guarded seam rather than an unguarded one — the opposite of how F-5 and F-9 were
handed forward.

**A-2 — world-side measurement, pending The Glasshouse.** Spec §6.3's marine and
cave contrastive-feature counts, and §6.5 arm 3 (`MicroField`'s four room axes).
A world at −11.99 °C median samples a cold corner of the Whittaker diagram; any
figure taken now would be re-derived post-epoch.

**A-3 — arm 4 is a recorded forward prediction.** Campaign 2's underworld
communities should land in the cave region of the space. Unevaluable until it
runs; recorded, not gated.

**A-4 — `DISTURBANCE` is a declared axis nothing can occupy.** Campaign 4's to
fill.

**A-5 — the two name vocabularies are reconciled in one place only.**
`axis_geometry.rs`'s `RECONCILE`, with a guard that fails if a rated biome stops
resolving. A third vocabulary would need the same treatment.

**A-6 — the homonyms are unrecorded in the tree itself.** Worth a board
`technique` so it reaches sessions that will never read this file.

**A-7 — P-4's sample is small.** 49 ordered pairs over eight authored species.
The 77.6% concordance is real but thin; a campaign that adds authored species
should re-run it before leaning on the figure.
