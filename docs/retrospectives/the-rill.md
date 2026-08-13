# Retrospective — The Rill

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-rill.md): a keystone rewritten
mid-flight after measurement falsified the original, two preregistered
intervals breached and shipped as findings, a third prediction falsified
outright, a drainage network fifteen times denser at cell scale and a hundred
times finer below it, and a census that grew elevenfold.

This campaign's scratch died with its checkout. Everything below was promoted
out of it before teardown.

## The count: six controller-authored defects, seven correct refusals

Six tasks, ten review passes, eight fix rounds, one absorption, one canonical
census. **Every defect this campaign found in its own plan text originated in
controller-authored writing** — the spec, the plan, and the task briefs — and
in four separate cases the implementer measured the premise, found it false,
and refused the brief.

1. **Task 1 — the founding premise was wrong, and building it would have
   introduced the bug it was written to prevent.** Spec §4.3 claimed that
   `drainage` being an upstream *cell count* made the width law
   scale-dependent, so `Q` had to become a drained area. It does not:
   `cell_edge` already carries the count→area conversion, because cells tile
   the sphere and a locally hexagonal tiling has spacing
   `√(2/√3)·√A = 1.0746·√A`. Measured across levels 4–7,
   `cell_spacing / √(4π/cell_count)` is **1.07824 at every level**, against
   the geometric constant 1.0745998. Applying the plan would have multiplied
   by area while keeping `edge`, applying the grid factor twice and rescaling
   every width by `√(N₆/N_L)` — ×2 at level 5, **×1/64 at level 12**. The
   implementer refused and the spec section was rewritten to say the opposite
   of what it said.
2. **Task 4 — the keystone rested on a face/cell duality error.** The spec
   asked for a routing construction on the room mesh. Cells are the
   icosphere's *vertices*; rooms are its *faces*; an edge joining two adjacent
   vertices is shared by exactly two faces, so a coarse flow edge runs **along**
   a room's boundary and never through it. There is no canonical lift. The
   implementer built it honestly, measured what it cost, disclosed the
   falsification itself, and the keystone was replaced.
3. **Task 5 — the implementer overrode an explicit STOP, after measuring the
   premise false.** The brief forbade committing a gallery move. It measured
   first: the flattened key-set difference is zero, and the only leaves that
   change are `micro.wetness` and the descriptor's wetness clause. The review
   verified it independently rather than reasoning about it — parsing every
   JSON hunk and comparing flattened leaf paths with list indices preserved:
   **added_leaves 0, removed_leaves 0, keyorder_diffs 0**. The STOP's premise
   was false on the artifacts, and an override carrying correct evidence is
   legitimate.
4. **Task 6 Step 4 — the controller's "correction" of the brief was itself the
   error.** The brief cited The Ford's "H1/H2/H4"; the controller corrected it
   to H2-1…H2-4 on the strength of a grep of the code. The Ford ran in **two
   stages with two hypothesis sets**: stage 2's H2-1…H2-4 (tests) and stage 1's
   H1/H2/H4 (lab metrics, in its retrospective). "H1/H2/H4" was an *exact*
   citation — including its gaps at H3 (void) and H5 (deferred), which a loose
   reference would not reproduce. The agent refused the correction, and **the
   campaign's one unlooked-for finding is in the set the correction said not
   to look for**.

Two further defects, found before dispatch rather than by refusal: the spec
carried **two hypotheses numbered R-6** (Task 4's channel-area ceiling and
Task 5's "a walk gets damper"; the shipped one stayed, the documentary one
became R-7 and pushed R-8 along), and Task 5's brief named a consumed
interface, `subdivide::flow_at`, that **had been deleted in Task 4** — a stale
interface riding through a spec rewrite untouched.

### Three more refusals, none of which was a defect in the plan

The four above are the refusals that *found a defect*. Three others cost
nothing and are the more ordinary case — an agent declining to assert what it
had not established. They are worth counting because the healthy rate is the
whole point.

5. **Task 2 refused to write an unmeasured number.** `metrics.rs:6917`'s
   "disabling the repair drops seed 42 to 0.92361" is likely stale, but
   re-measuring it means disabling the confluence repair. It declined rather
   than write a number it had not measured, and said which number and why.
6. **Task 4 refused a review finding it could not reproduce.** Finding 4(c)
   claimed `worst_reach`'s message could name the wrong cell; it is a monotone
   maximum asserted inside the loop, so it cannot. The implementer made the
   value per-cell anyway — so the message no longer *depends* on that
   argument — and declined to claim a defect it had not found. Making the
   change and refusing the diagnosis are separable, and it did both.
7. **The final-review fixer escalated rather than extending silently.** It
   found a fourth site for Important 2 — the *published* `doc:` literal at
   `metrics.rs:3473`, the one flowing into both `schema.json` files and the
   Domesday page, still asserting that `lab_channel_connectivity` "measures the
   joins" — and refused to edit it unasked, because doing so moves three
   committed artifacts before the final gate. It was right that it needed
   fixing (a published description is worse than a private comment, not out of
   scope) and right to ask rather than widen its own brief.

The distribution is the project's documented pattern and the diagnosis has not
changed: a controller writes prose that asserts things and dispatches it to
agents who execute against it, so a controller's mistake meets nothing until
somebody checks the premise. What is worth adding is the **rate of correct
refusal**. Seven briefs, corrections or findings were refused on measured
evidence and every refusal was right. A campaign whose implementers cannot
refuse is a campaign that ships its controller's arithmetic.

### The transferable form of defect 4

**A label that resolves is not a label that resolves uniquely.** The
pre-dispatch verification grepped the code, found the stage-2 labels, and
stopped at the first referent; the retrospective where stage 1's table lives
was never opened. The dispatching discipline says to grep every named
identifier. It does not say to keep looking after the first hit resolves, and
that is the gap.

## 1. A guard whose reference lives inside the thing it checks

This is the campaign's signature failure and the most transferable thing in
it. It is inherited — the prior arc closed with seven instances of one shape —
and it recurred **seven or more times here**, in five different disguises. The
family resemblance is exact: an assertion is written, it passes, and its
reference turns out to be derived from the very thing it is supposed to
constrain, so it could not have failed.

1. **A committed fixture with no reader asserts nothing.** Task 1's brief said
   the fixture "says" no band edge may move. That is not true of an *unread
   file*; the implementer added `no_level_6_band_edge_has_moved` to make it
   true.
2. **A pin that cannot see the change it is supposed to see.** That same
   level-6 pin reads a compiled-in hex table and calls `band_edges` directly:
   no globe, no generation, no network. Task 2 added 202 vertices to seed 42
   and the pin **could not have fired**. The only thing in the repo pinning
   network topology was a golden explicitly marked re-baselineable — and Task 3
   had to re-baseline it, which is why Task 2's fix round asserting the borrow
   *directly* mattered.
3. **Three one-step invariants that all passed while the composed one failed.**
   The falsified R-4 asserted local properties — one step, one face's descent.
   All three were green while coastal delivery fell from 74–82% to 26–31% and
   the basin count doubled. **Every local invariant can hold while the global
   one fails.** R-4 was restated as a composed basin claim, and the composed
   form reads 41,415/41,415.
4. **A coverage figure that is an identity in disguise.** The falsified
   design's "containment 74%" was `flow_at(room).is_some()`, true exactly when
   a room's globe-level ancestor is a flowing face. Seed 42: 16,376 flowing /
   22,159 land faces = **73.9%**, the measured 73.97% to three figures. It
   would not have moved if the subdivision were deleted.
5. **A green that could not witness its claim, with a mechanism.** Task 3's
   round-2 doc edit was chained in one shell call with two test invocations;
   the workspace's two-run guard rejected the *entire* command before anything
   ran; the implementer re-ran only the test half, saw green, and reported the
   edit as landed. **No test reads a doc comment.** Rule adopted: after editing
   a file no test can observe, the verification is a diff, not a test run.
6. **An assertion that passes with the campaign's own contribution deleted.**
   Task 5's shipped R-7 statement is carried entirely by the climate-supply
   term, which predates the campaign. The allocation acts, and the supply-only
   arm scores the same or better.
7. **An estimator blind to everything but its endpoints.** The geometric mean
   of *consecutive* Strahler order ratios telescopes: the product of
   `S_w/S_{w+1}` is `S_1/S_max`, so `R_b = (leaves / top-order segments)^(1/(k−1))`
   and nothing else. See §6.
8. **A tautological floor.** `unrendered_sinks <= sinks_total`, deferred as a
   Minor, is true by the definition of its two terms.

**What actually worked against this class, in order of strength.**

- **A positive control beats a "it was green first".** Task 4's rebuild could
  not watch R-4 pass on a known-good base, because the base did not compile
  (see §4). The substitute — mutate the composed relation so basin agreement
  genuinely breaks, show the red with its number, revert, re-grep — is
  *stronger*, not weaker. Two controls were built, one driving agreement to
  0.0000% and a subtler one to 80.3718%, both type-checking.
- **A mutation that leaves a test green is worth as much as one that reddens
  it.** Task 1 perturbed the width coefficient: the pin went red and the
  invariance test stayed **green, deliberately**, because a different
  coefficient is still scale-free. That is what proves two tests independent
  rather than one duplicated change-detector.
- **Verify a revert by re-running after `touch`, never by grepping the
  source — because the source is not what ran.** Hit live at the close, while
  mutation-proving the flip-count pin. The mutation was applied with
  `sed -i.bak`, and the revert was `mv file.bak file` — which restored an
  mtime *older* than the compiled test binary, so cargo skipped the rebuild
  and re-ran the **mutated** binary against reverted source. `grep` showed the
  correct value while the run reported the mutated one. Here it produced a
  false **red**, which is loud and self-correcting. **Apply the mutation in
  the other order and the identical mechanism produces a false green** — the
  direction that silently invalidates a mutation proof, by reporting that a
  mutated assertion passed when what ran was the unmutated build. This
  campaign leaned on mutation proofs repeatedly and rested several
  independence claims on them, so the hazard is load-bearing for how much all
  of that evidence is worth. It is a documented repo trap; what is new is that
  the false-green direction is the dangerous one and the check is one word
  (`touch`).
- **A reference from outside the repository.** Task 1's review parsed the
  690-line fixture in Python and recomputed every row *outside* the tree —
  661/661 bit-exact. Task 5's draw-order witness verified against a **different
  campaign's** before-arm and got relief/aspect/openness byte-identical for all
  200 rooms.
- **If the owning test can compute a number, print it; if not, name its
  population beside it.** Adopted in Task 3 after round 2 fixed three
  prose-drifted-from-population defects **and introduced two more of the same
  class in the same commit, inside the defect it was fixing**. The defence
  existed in that very commit: four attribution figures were made
  computed-and-printed locals and none of those drifted; both failures were in
  the two still typed by hand.
- **Grep the assertion *shape*, not the region of the file.** Task 3's class
  sweep read down files it already had open, which finds floors near what is
  being edited, and walked past a third instance sixty lines below in the file
  it was editing. The shape to grep is `>= FLOOR`, `> N`, `.len() >=` against
  any population derived from the network. Done that way, the class had exactly
  eight members and no ninth. The same lesson recurred in Task 4's rebuild,
  where the sweep found six members including a stale figure **160 lines below
  the constant written to retire stale figures**.

**A number in a doc comment is a number that will be wrong.** That is the
one-line form, and it now has enough instances to be treated as a law.

## 2. `make preflight` returned GO against a stale ref

The first preflight run reported *"ok: main (44d7fb9f) is an ancestor of
HEAD"*. True, and meaningless: local `main` was **67 commits behind
origin/main**. **Preflight never fetches.** It went NO-GO the moment main was
current, and the absorption that followed was substantial — a decision-number
collision, a census refresh and a metric-set change on the other side.

The GO is only as fresh as your last fetch, and nothing in the script says so.
A one-line `git fetch` before the ancestry check, or a comparison against
`origin/main` rather than `main`, closes it.

## 3. A decision-number collision is invisible to preflight

`0129` was minted on both sides. Ours was
`0129-a-sub-threshold-watercourse-is-a-narrow-channel` (Task 3, unmerged);
main carried `0129-the-board-gets-a-risk-scoped-lane-and-a-path-scoped-hook-rule`
(ratified). Preflight's both-sides-added check compares **filenames**, and the
slugs differ — so a *number* collision passes it silently.

Ours renumbered to **0130**: the other record is ratified and append-only,
ours had never been ratified, which is exactly what makes it the one that may
move. The renumber moved six references and every other `0129` hit was
classified and left alone (the board decision's seven, the registry narrating
its own earlier renumber, and five digit coincidences inside hash literals and
coordinate data).

**This is the second instance in this campaign, and the number we collided
with was itself the product of an earlier renumber** — the board decision was
0128 until The Muster took 0128. The project already knows that registry IDs
collide by arithmetic; the decision log has the same property and no check.
The fix is a check that compares *numbers*, not filenames.

**Nothing enforces the decision *index*, and it had already lost a row.**
`docs/decisions/README.md`'s table ran 0127 → 0129 → 0130 with **0128 missing**
— found by Task 3 and left, restored at this close.

The precise shape is worth stating, because a gap check does exist and stayed
green throughout. `docs_consistency`'s `no_gaps_in_the_decision_log` reads the
**record filenames** in `docs/decisions/` and asserts `0001..=last` is
unbroken; `0128-name-the-transformation-a-quantity-is-gauge-under.md` was
present the whole time, so the check was correct and silent. Its neighbour
`docs/digest/decisions-in-force.md` is generated and listed 0128 all along.
The only surface that lost the row is the one that is **hand-maintained and
unchecked** — the README table — and the reason the guard cannot see it is
that the two read different things: files versus a table *about* files.

That is the same gap as §3's, one level down. A renumber edits the table by
hand, and a hand-edited index sitting beside a generated twin is exactly where
a row goes missing unnoticed. A check comparing the table's numbers against
`docs/decisions/*.md` closes it, and probably wants to be the same test as the
number-collision check.

## 4. A plan commit swept a file deletion into the index

`git log --diff-filter=D` puts the deletion of `subdivide.rs` in **336c2e22 —
a plan commit**. An agent that died mid-task had removed the file; the
deletion was swept into the index by a commit that took the whole index, while
`lib.rs`'s `pub mod subdivide;` stayed. **The branch carried a non-compiling
commit.**

Two consequences beyond the red base. The deletion is **invisible in the review
diff** — the file was already gone — so the following commit is repairing a red
base rather than performing the deletion its brief asked for; and the step that
required watching a guard pass on a known-good tree became unsatisfiable,
because there was no known-good tree.

`git commit` commits the whole index. Use `git commit -- <paths>`.

## 5. A stale baseline in `CLAUDE.md` cost a 2.2× wrong prediction

The controller told Nathan the running census would take 1.3–2.4 h. It took
**5.34 h**. Both the controller and the campaign's own cost attribution
anchored on `CLAUDE.md`'s *"776 s / 887 s / 921 s … Budget 15"*.

`docs/timings.md` — the ledger `CLAUDE.md`'s own prose points at — showed the
**immediately preceding census at 1,718.995 s**, its row stamped
2026-08-12T21:04:17Z against The Rill's own 2026-08-13T06:08:39Z, with rows at
1,709.9 / 1,789.1 / 1,719.0 going back two days. **Main's own census cost had already
doubled and `CLAUDE.md` was never updated.** Against the true before-arm the
growth is 11.2× wall and 12.7× CPU, which makes the attribution's independently
measured **9.1–9.4× per world** approximately right. *The attribution was
sound; the number it was multiplied by was stale.*

Two rules, both already in the project's memory and both violated here:

- **A committed baseline is a claim with a date.** The claim trusted was four
  days old and already 2× wrong.
- **When a doc points at a ledger, read the ledger.** The pointer was there.
  Neither reader followed it, in either direction, for two independent
  estimates.

That block in `CLAUDE.md` has been restated from `docs/timings.md` as part of
this close, as a *pointer* rather than a figure, precisely so the next drift
cannot be silent.

## 6. "0 flips" was an inference from equal aggregates; the number is 34

The review reported that the campaign's allocation **flips 0 of 420 steps**,
inferring it from grounded and supply-only both scoring 276/420. The
implementer measured it and got **34** — a genuine per-step reversal counter,
`if arms[1] != arms[2] { allocation_flips += 1 }`, which no aggregate
difference can produce.

**Equal totals are consistent with no change and with equal-and-opposite
change, and the review picked the wrong branch.** The re-reviewer settled it
against itself, and diagnosed its own error precisely: `276 = 214 + 62` *and*
`276 = 216 + 60`, so its "60/75 = 0.8000" was a correct decomposition of the
*supply* arm attributed to the *grounded* arm.

The corrected claim is also the stronger one, because it is a claim about the
world rather than a non-event: **the allocation acts 34 times and buys
nothing** — in scope, grounded 214/345 = 0.6203 against supply-only 216/345 =
0.6261, very slightly worse. Never "the allocation is inert".

A reviewer's sentence can overstate its data. **The number is the finding, not
the summary.**

## 7. The two-run test guard rejected a ledger append

The workspace's guard against chaining two cargo invocations rejected an append
to the campaign ledger, because the **ledger prose named two cargo
invocations**. The guard scans the command string, so quoted narrative
describing a run is indistinguishable to it from the run.

This is the same guard as §1.5, seen from the other side. There it rejected a
compound command wholesale and an edit chained with tests silently did not
happen; here it cost one retry on a document. The guard is right to be blunt —
the failure mode it prevents is worse — but **writing about tests trips it**,
and both of its costs this campaign were paid by prose rather than by code.

(Second scratch-writing mishap of the same session: a ledger append truncated
mid-line because a heredoc was wrapped in `bash -c '…'` and the prose contained
apostrophes. Neither touched the repository. Write scratch with a plain quoted
heredoc.)

## 8. A kernel panic is a measurement, and this one named its own defect

An agent working on Task 4's rebuild was killed by a **kernel panic** — the
watchdog reporting no check-ins for 94 seconds, the compressor at 100% of its
segment limit with 66 swapfiles, all three top CPU consumers in the pageout
path. Memory exhaustion.

Reproduced under a bounded measurement — one test, single-threaded,
`/usr/bin/time -l` — at **23.7 GB maximum resident set, SIGKILL**. That is
~269 million nodes in a single call against a documented ~25,000 per cell, and
it localised the defect immediately:

```rust
let first = ((share as u128 * cut as u128) >> CUT_BITS) as u64;
(first, share)          // the second child gets the WHOLE parent share
```

The second part should be `share - first`. As written the second child never
shrinks, never reaches its stopping threshold, and recurses to the depth
backstop while spawning a full subtree at every level.

Three things about this are worth carrying.

- **The doc comment three lines above stated the correct behaviour** — "the
  second is the remainder, so nothing is created or lost at any depth." This
  campaign's recurring class **inverted**: every other instance was prose
  drifting from code; this was code drifting from prose, and the prose was
  right.
- **It broke the keystone, not merely the memory budget.** `first + share >
  share`, so the children's shares sum to more than the parent's — the
  partition does not conserve, which is precisely what "subdivide the scalar"
  was supposed to guarantee by construction. The test that would have caught it
  is the one that was killed before reaching its assertion.
- **The process did not fail; it was interrupted before the step that catches
  this.** The work was uncommitted and unreviewed, from an agent that died
  mid-task. Review is exactly the step that catches a conservation function
  that does not conserve.

The plan gained a **memory budget** as a global constraint: bounds stated in
doc comments *and* asserted as node counts, peak RSS measured beside wall time,
and a hard node cap in any probe over a recursive structure so a broken
stopping rule fails fast with a number instead of taking the machine down. The
resulting bound is a derived number rather than prose — `RILLS_PER_CELL_MAX =
2^15`, from a level-6 cell's catchment being 8191.6× the minimum, giving 8,192
leaves on the even arm and a size-biased overshoot of 1.5376 giving 12,596 on
the drawn arm, against **12,570–12,602 measured**. The cheap version of that
guard walks one cell under a 400,000-node ceiling in 0.07 s and 16.5 MB and
sits in the commit gate; against the unfixed code it aborts at the ceiling.

## 9. Absorbing mid-measurement was correctly declined, and the reason was specific

The absorption was held until the Task 5/6 boundary, against the usual
stage-boundary rule, and the exception was not generic caution. Task 5 had
committed its before-arm and R-7/R-8 measured against it, and main carried
`measure(terrain): attribute land elevation to the isostatic base`. **R-7's
reference is the elevation the walk descends.** Absorbing under the fixture
would have had the readout and the baseline seeing different physics, in the
exact quantity being measured, in the one task designed to be attributable.

It was equally not deferred past that boundary, and for a mechanical reason:
main had just refreshed the census *and* moved the metric set, so a census
authored on a ref predating those would commit goldens against a stale metric
set and then drift-check green forever.

The reconciliation's own lesson: **a clean textual merge proves nothing where
it matters most.** The only two conflicts were the decision-number collision,
in two indexes; all code auto-merged, including the two files where a semantic
collision would have lived. The check that had value was reading the other
campaign's chronicle and verifying the module list and re-export block were
*coherent* rather than merely non-conflicting.

## 10. Small notes

- **A generated index is resolved by regeneration, never by hand.** One of the
  two collision conflicts was in `docs/digest/decisions-in-force.md`, which
  `regenerate-artifacts.sh` writes.
- **State which revision authored a canonical artifact when it is not the
  branch tip.** The census ran for 5 h 20 m while the branch advanced two
  commits. Both were shown unable to move a row — one test-only plus doc
  prints, one proven byte-identical across 96 worlds with a passing tripwire —
  but a reader who notices the gap and is not told will assume the worst.
- **An unsatisfiable brief item is a finding when the agent proves it.** The
  memoisation brief required channel-less worlds in the byte-identity set.
  **None exist** — 5,448 worlds checked have zero, the probe column reads a
  true `0.0` rather than absent so both shapes were visible, and the study pin
  sets carry sky pins only, so no terrain pin can force one. The agent said so
  with evidence rather than faking the set.
- **Scope a cache by construction, not by discipline.** The memoisation's real
  risk was never speed but cache scope: a static, a colliding key or a lifetime
  outliving one world would have served one world's sweep to another and
  authored 1,000 corrupted goldens that drift-check green forever. The design
  that answers it has *no key at all* — a private `OnceCell` on the per-world
  view, `OnceCell` and not `OnceLock` deliberately, so `!Sync` stops it crossing
  worker threads even by accident.
- **Assert pointer identity, not equality, when the claim is "computed once".**
  Equality alone stays green against a memo that silently re-sweeps.
- **The `4^6 = 4096 rooms share one grid cell` figure is loose repo-wide.** It
  is true of faces per *face* and false of faces per *cell*, where the answer is
  ~8,192, and the difference decided a review dispute by exactly a factor of
  two. It appears in `CLAUDE.md`, in specs quoting it, and in a published
  reference page. Left alone deliberately: it wants a ruling, not a unilateral
  edit.

## Deferred minors, promoted from the campaign ledger

These were reviewed, judged not worth a fix round, and would otherwise have
died with the worktree. Roughly 32 Minors were raised across the five
implementation tasks; most were repaired in-branch, and what follows is what
was still live at the close.

- **Task 2.** `domains/terrain/src/channel.rs:561-562` computes `cell_spacing`
  and `local_slope` for the terminal non-river cell and discards both.
- **Task 2.** The report's "every line gains a terminal row" is untrue of
  fixture line 17, which ends at a confluence.
- **Task 2.** `windows/vessel/tests/fixtures/snapshot-seed-1-chamber-occupied.json`
  moved the **other** way — `−1.7488e-4 → +3.0625e-4`, sign flipped and
  magnitude nearly doubled. Benign (both outside `band_edges[0]`, and
  `channel_bands` unmoved), but the report presented only the seed-42
  improvement, which is a one-sided reading of a two-sided move.
- **Task 3.** The outlet-kind **6090 / 1086 / 0** breakdown is documented at
  `rill_properties.rs:261` and unasserted — a number in a doc comment with no
  test behind it, which is this campaign's own named law.
- **Task 3.** `transects_with_strongest` re-derives `walk_depth` rather than
  taking it, and `channel_properties.rs:443`'s cap-bounded floor lacks the note
  the other two cap-bounded floors were given.
- **Task 3.** "91 loud vertices of 14,606 (0.62%)" is hand-arithmetic over two
  printed numbers — correct, and over the right population, but computed by a
  human rather than by the test that owns it. Relatedly, "the one loud vertex
  reads `NotACrossing`" is asserted twice and is **not independently verifiable
  from the aggregate output** the test prints.
- **Task 5.** `windows/locale/src/grammar.rs:95-104` expresses the same
  ground/ice/other partition as a three-way match on the **renderer** side,
  without calling `micro::wetness_is_grounded` — the remaining place the render
  half and the grounding half could diverge. A reviewer verified the two agree
  exactly *today*, so this is drift risk rather than divergence. Both sites now
  carry a comment naming the other and the hazard; the code was deliberately
  left alone, because collapsing them is a behaviour change and the close is
  not where an unverified one ships.

**Task 4's Minors 7–12 were deferred by number only.** The ledger records the
deferral and not the findings, so they are **unrecoverable**. This list is
therefore known-incomplete and must not be read as the full set. The one
exception is `rill_probe.rs:98`'s hand-typed "89%", which was pulled back in as
a member of finding 5's class and fixed in-branch.

## Follow-ups

No `followups.md` exists; these are promoted from the ledger with their
measured numbers so nobody re-derives them.

1. **Index `ChannelNetwork::nearest_line`** (`domains/terrain/src/channel.rs:890`).
   It is the inner loop of both hot census paths and the only real fix for the
   quadratic. **Blocked on byte-identity, not on effort:** its strict-`<`
   lowest-index tie-break reaches the *serialized sign* of
   `bank_signed_distance`, so any change to the search order is a
   determinism-contract change and needs byte-identity evidence and its own
   scoped work.

   **`rill_reading` is in this scope too** (`domains/terrain/src/branch.rs:790-800`).
   Its nearest-branch search over `here` then `geo.neighbors(here)` uses the
   same strict-`<` first-wins rule, so the neighbour enumeration order and the
   cell's own priority are equally a contract. The reason is one step longer
   than `nearest_line`'s and was initially got wrong in a doc: `RillReading`'s
   `distance` and `band_edges` are indeed never serialized, but
   `rill_reading → grounded_wetness → micro.wetness` **is**, and this
   campaign's own blast radius moved `micro/wetness` in the gallery, three
   vessel snapshots and two game-core fixtures. Both tie-breaks are now
   documented as contracts at their definitions.
2. **`channel-connectivity` is superlinear and will overtake everything.** It
   walks one path per polyline and does 7 `transverse_at` calls per hop, each an
   O(V) scan, so `walks × V` grew 19.7 × 16.5 = **up to 326×** (measured ≥180×,
   from <0.03 to 5.50 CPU-s/world). It is only **7.5%** of census cost today
   because it started from a null. Another 4× densification makes it ~16× dearer
   and it becomes the dominant term. By contrast `lab_band_transects` is linear
   — the 256-transect cap holds probe count flat.
3. **`make preflight` does not fetch** (§2). A one-line `git fetch`, or an
   ancestry comparison against `origin/main`, closes it.
4. **Nothing checks decision *numbers* for collision** (§3). Preflight's
   both-sides-added check compares filenames; two records may share a number
   and differ in slug, and that has now happened twice in one campaign.
5. **The census is still ~3.7× dearer than this campaign found it.** The
   memoisation is a real 3.014× wall / 3.010× CPU (851.74 s → 282.59 s on the
   attribution harness; 72.03 → 23.93 CPU-s/world), and peak RSS *fell* 2.2%,
   so there is no space/time trade in it. But a future full census projects to
   ~6,400 s (~1.8 h) against the pre-Rill 1,719 s. Both halves belong in any
   future budgeting.
6. **`channel-band-monotonicity-untruncated` regressed past its pre-repair
   state and nobody predicted it**: 64/64 at ≥ 0.99 with all reaching 1.0 at
   The Ford, now **12/64 with none reaching 1.0**. The shipped axis is
   unaffected at 1.0. The cause is Tier 1 density, not a broken repair — with
   3,606 polylines where there were 183, a transect meets an *unrelated* line
   far sooner and ~24% are truncated before reaching `dry`. Not repaired,
   deliberately: the truncated metric is the correct one. Two cautions for
   whoever picks it up — the comparison to the pre-repair 17/64 is a count at
   one threshold and not a distribution comparison, and the current values are
   all high (min 0.9724).

   **Two baseline facts the re-measurement established and could not fix.**
   The Ford's other H2-2 figure — "19 insertions, 0 deletions, 0 replacements
   over 7 artifacts" — was a one-off manual diff taken at its stage-2 close: it
   is **not re-runnable, and no instrument in the repo reproduces it**. And
   `channel-transect-dry-reach`, one of the three census columns this campaign
   moved, **has no recoverable baseline at all** (its post-value is mean
   0.7577, min 0.6890, max 0.8281). So "exactly three columns moved of 203" is
   true and stands as the blast radius, but one of the three moved *from an
   unknown value* — a fact worth carrying, because a future reader comparing
   against it will otherwise assume a before-arm exists.
7. **The level-6 width-law fixture is declared permanently un-repinnable with
   no sunset.** That is right inside this campaign; after it, a legitimate
   recalibration must hand-edit hex or delete the test. Its authority ends
   where a deliberate recalibration of `CHANNEL_WIDTH_COEFF` begins, and that
   sentence is owed to whoever attempts one.
8. **R-5's estimator wants replacing before it is quoted again** (§6 of the
   chronicle). The classical Horton estimator is a log-regression over *all*
   orders, not a ratio of endpoints. The pass/fail is very likely robust —
   interior per-order ratios sit at ~2.68, below the 3.0 floor, so a regression
   estimator would also miss — but that has **not** been measured and must not
   be asserted.
9. **`channel-connectivity` is largely vacuous and the column's stillness was
   cited as evidence.** Its walk continues only while the next cell is
   classified `WaterKind::River`, but `ChannelNetwork::build`'s reach predicate
   is `!Ocean && downhill.is_some()` — strictly wider since Task 3. A tributary
   ending on a sub-threshold trunk breaks the walk *before* `owner[last_cell]`
   and scores `intact` without crossing its join, which is the only thing the
   measurement is about. **The mechanism is certain; the magnitude is not
   measured and no figure for it is recorded anywhere** — the review estimated
   one and correctly refused to assert it. Two pieces of work, and they are
   separable: (a) quantify the affected fraction; (b) the repair, which is
   testing `owner[last_cell].is_some()` rather than the water class. (b)
   **moves a census column**, so it needs its own scoped work and its own
   refresh and must not be slipped into a documentation pass. Note also that
   the metric's own doc still measures "20 joins across 183 walks" as of
   Task 2, against a shipped network of 3,606 polylines.
10. **~32 Minor review findings were deferred across the five implementation
   tasks** to the whole-branch review. The still-live ones are written out
   above, under *Deferred minors, promoted from the campaign ledger* — the
   campaign ledger they used to live in is gone, so that section is now the
   record. Read its closing paragraph too: **Task 4's Minors 7–12 were deferred
   by number only and are unrecoverable**, so the list is known-incomplete by
   six.
11. **A `make ci` on this branch will alarm, and the alarm is predicted.** The
   campaign moved `hornvale-locale::water_reading` **1.53 s → 12.27 s** (8.0×,
   part of it a deliberate 400 → 1600 sample increase), added the 1,492-line
   `rill_properties.rs`, and the memoisation guard adds **~30–39 s** to the lab
   lib binary. None of that is the whole story, because the committed baseline
   was already stale before this campaign touched it: Task 3 measured
   `docs/timings/test-baseline-MacBookPro.tsv` directly and found **only 177 of
   its 534 rows still exist, with the shared subset totalling 0.47×** — it
   predates The Whetstone's optimized dev profile. The file contains **no
   `water_reading`, `wetness_reading` or `rill_properties` row at all**, so the
   moved tests cannot even be compared. The resolution is a post-merge
   `make ci` on a quiet `ambrose` with the baseline re-recorded in that same
   commit, not an edit here; this entry exists so that whoever sees the red
   does not spend a session attributing it to one task.
