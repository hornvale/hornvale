# The Portolan, part II — retrospective

**Merged:** 2026-08-23. Process lessons only. The result is in
[the chronicle](../../book/src/chronicle/the-portolan-world-map.md).

## Essentially every defect this campaign found was in the controller's plan text, not in an implementer's code

Six tasks, one Critical, several Important findings, and reading back
through the ledger, the pattern is stark: implementers wrote correct code
against a plan that specified things that did not exist, or specified them
backwards. A partial list, each verified against the repository rather than
taken on the ledger's word:

- The plan's own Step 1 test for `mercator::project` destructured its return
  as `(_row, col)`; the function returns `(row, col)`. A faithful port would
  have compiled, run, and asserted against the wrong tuple element while
  reading as if it asserted the right one.
- The plan said "port unchanged in behaviour" for `project`, while specifying
  an `Option`-returning signature — the spike it was ported from never
  returns `None` at all. The two instructions contradicted each other.
- Step 9 of the Task 2 plan called `g.cells()` on `hornvale-game-core`'s
  `Grid`, a method that does not exist.
- The plan named a Task 2 test file (`tests/suite.rs`) that cannot exist:
  `hornvale-game-core` sits outside the cargo workspace's test-binary
  consolidation entirely.
- Global Constants named `WALK_FIXTURE`, `test_driver()`, and `test_world()`
  — none of which exist anywhere in `clients/game`. The real fixtures are
  pulled with `include_str!` and a real `Driver` is built with
  `Driver::start(42, PossessTarget::Flagship)`.
- Task 5's brief specified `Visited(BTreeSet<CellId>)`. `CellId` is a bare
  `u32` with no rung structure, so it **cannot express** the rung-aware
  containment that same brief demanded of it — not a naming quibble, a type
  that cannot perform the operation it was assigned.
- §3.3's clamp/central-line caption was mapped to Task 1 by the plan's own
  self-review table. Task 1 merged without it. No later task's step list
  claimed it either. It sat unowned for two tasks and was found only because
  a reviewer went looking for who owned it, not by re-reading the mapping.

None of these reached a player. Every one was caught by an implementer, a
reviewer, or a controller re-verification before it shipped. The cost was
real anyway: each is a fix round, a re-review, or (in §3.3's case) an entire
task's worth of scope discovered four tasks later than it should have been
assigned.

**One defect in this campaign was not plan text, and "essentially every"
rather than "every" is there for it.** Choosing `Focus::Map` alone as the
world-view trigger silently regressed a shipped, chronicled feature: map
focus previously meant a cursor over the walk-band chart with the strip
naming whatever it pointed at (The Stylus, The Portolan part I), and the new
code replaced that view unconditionally the moment `Focus::Map` was entered,
with no way back to it. This was not a plan specifying something that did
not exist — it was correct code doing exactly what its own task asked,
quietly deleting a feature the task's own brief never mentioned because
nobody writing that brief re-checked what `Focus::Map` already did. It was
caught only because the reviewer happened to know the earlier campaign,
which is not a repeatable safeguard — a differently-staffed review could
have let it through clean. Fixed the same task cycle: the world view became
explicit state (`world_view: bool`, default off), with a regression test
pinning the byte-identical old behaviour by reintroducing the bug and
watching it fail.

## A self-review table that maps a requirement to a task does not verify the task did it

This is the general form of the §3.3 loss above, and it is worth stating on
its own because the table *looked* like verification. It recorded "§3.3 →
Task 1" and was read, including by the controller, as though that entry
meant the requirement was covered. A mapping is a plan, not an audit — it
records an intention at planning time and says nothing about whether the
intention survived contact with the task that was supposed to carry it. The
gap was only found because a reviewer, working from the spec rather than the
table, asked "who owns this?" and got no answer.

**The same table lost a second requirement the same way, and the loss stood
through this retrospective's own first draft.** H2 — "the cursor resolves
consistently across zoom" — appears three times in the spec (§8, §12, §A9)
and the table mapped it to "Task 3 Step 5," beside H3. H3 got a real test.
**H2 got no test, no measurement, no doc, no retirement, and no deferral
anywhere on the branch** — not falsified, not confirmed, simply never
looked at again after the table recorded where it was supposed to go, and
this retrospective's own author did not catch it either; the campaign's
final review did, by re-auditing the table against the spec rather than
trusting it a second time. The lesson from the first loss was written down
in this very section and the table that caused it was never re-checked
against what it claimed to cover. Disposed at final review: not measured,
but a datum computed instead of a guess — only 10.9% of terrain cells are
ever an `area_majority` representative at zoom 0, which is consistent with
H2 being **false at the cell level** for most cells, as a straightforward
consequence of an already-known undersampling defect rather than anything
specific to the cursor. Recorded in the spec beside H1's retirement (§A10c);
owned by whichever future campaign fixes that undersampling first.

## Five instances of a check that reads as protection while not being pointed at what it claims

The same defect shape recurred five times, and in four of the five only
mutation testing or a final-review re-derivation revealed it — reading the
test never would have:

1. **The compose test.** `a_supplied_world_plate_replaces_the_band_view_and_
   nothing_else` used a synthetic plate with exactly one non-blank cell, and
   `blit` skips blanks — so a "merge instead of replace" bug would have left
   the test green. The shipped code was correct; the test could not have
   caught it if it were not. Fixed by strengthening the fixture and proving
   the strengthened version red-then-green against a real mutation.
2. **The "genuine ground truth" test.** Added to replace a hand-mirrored test
   that reconstructed the implementation's own private call sequence instead
   of deriving an answer independently — and the replacement turned out to be
   *weaker*. A mutation swapping `world_view_cell`'s `(row, col)` argument
   order passed the new test, which compares only ocean/land booleans, and
   was caught instantly by the old, "merely regression-value" test comparing
   exact feature names. Both tests are load-bearing; framing the old one as
   disposable was wrong.
3. **The type-audit tag.** `format_chain` and `resolve_chain_at` carried
   `bare-ok(identifier-text: return)` while actually returning composed,
   punctuated prose — the crate's own sibling functions all use
   `bare-ok(prose: return)` for exactly this shape. `type-audit check` is
   syntax-only: it can confirm a tag is well-formed and cannot tell whether
   its *class* is the right one. A copied tag from a nearby, correct call
   site read as coverage and was not.
4. **The delve success literal.** The client's cave-discovery gate depends on
   `narration.prose.starts_with("You worm down into the dark.")`, exact
   prefix, period included. A pre-existing sim-side test guarded the same
   prose, but with `.contains("You worm down into the dark")` — looser than
   the coupling that came to depend on it. A reword inserting one word after
   "down" would have broken the client silently while the sim's own test
   stayed green. This instance predated the campaign; the campaign found it
   while building the first thing that actually depended on the exact
   string.
5. **H6b's own plate-rendered negative check, inside the test written to
   prove this campaign's central rule.** `h6b_co_location_does_not_disclose_
   a_settlement` walks past a real settlement and asserts its glyph never
   appears on the rendered plate, at the design plate size, at every zoom
   rung. It genuinely never appears — but the final review found, by
   rendering at successively finer resolutions until the mechanism-level
   question could even be asked, that this specific settlement's glyph is
   absent at the design plate (any rung, any window offset) and at 400×200
   (the resolution an existing, working precedent test uses for a cave
   cell), appearing only past 1200×600 — more than three times the client's
   own zoom ceiling. So the glyph's absence at every rung the test covers is
   not evidence the discovery gate is doing anything: nothing this client's
   zoom ladder can ever reach would draw that settlement whether or not it
   were discovered. The check is real, and it is checking the wrong thing —
   not "does the gate withhold this," but "is this resolution too coarse to
   draw it regardless," which was already true before Task 5 ever wrote a
   line of discovery code.

Each of the five passed a normal read. What exposed them was, respectively:
a deliberately strengthened fixture with a mutation proof, an adversarial
mutation on a field the implementer had not thought to vary, comparison
against a sibling call site's tag rather than the letter of the tag's own
grammar, building the first real consumer of a string that had only ever
been a docstring's example before, and — the one instance mutation could
not have found, since nothing about the code was wrong — rendering the same
scene at ten times the resolution until the picture itself contradicted
what the coarse version implied.

## A measurement's instrument was never checked because its output was the answer already expected

The campaign's own sharpest-sounding finding was wrong, and the way it went
wrong is the finding worth keeping. At the coarsest zoom, the cursor's
single-centre-point resolution agreed with the plate's 49-vote-majority
drawn glyph only 52.5% of the time (420/800) — reported to Nathan as the
campaign's sharpest result, because a wrong name half the time is exactly
the "wrong name indistinguishable from a right one" failure this project has
already spent effort guarding against elsewhere. It was never independently
sanity-checked before being relayed, because it *looked like the kind of
number this campaign expected to find*.

It was an artifact of a broken test harness: both the measurement and the
"genuine ground truth" test in the previous section called
`Driver::world_plate(w, h)` with an already-fitted plate size instead of the
raw terminal size, double-applying the fit and rendering a misaligned 32×16
grid against coordinates computed for 40×20. Of course two different
pictures disagreed about what was drawn where. Corrected, the same
measurement reads 800/800 — not "much better", the honest old number simply
does not exist any more, because the instrument that produced it never
measured the real thing. **The pre-fix disagreement rate is now unmeasured,
not smaller.** The underlying effect (a majority vote and a single sample
point must disagree at coastlines, at coarse enough zoom) is real in kind and
was never quantified correctly.

The scrutiny was asymmetric, and that asymmetry is the lesson: a controller
who had, two turns earlier, caught the opposite failure — an over-normalized
"ratio" claim with the wrong sign — applied less checking to a number that
confirmed a defect already believed in than to one that would have cleared
the code. A number that fits the expected story needs the same instrument
check as one that does not.

## The reference a hypothesis is measured against can itself be the thing that is wrong

H1 asked whether the whole-planet plate shows one coherent largest landmass,
and it was benchmarked, from the start, against the Gazetteer's committed
`elevation_ascii` (72×24, nearest-cell). Three re-framings — sampling method,
then plate size — each moved the plate's own rendering closer to
self-consistency and never resolved the disagreement with that reference,
because the reference itself was the miscalibrated instrument: at its
resolution it had merged two genuinely separate landmasses into one
(49.9%/13.9%, against every finer instrument's ~27%/25%). What broke the
deadlock was not a finer version of the *same* check — it was an instrument
sharing no machinery with the plate at all: a different projection, a
different sampling strategy, a different resolution. Two independent
instruments agreeing with each other and disagreeing with the shared
ancestor is a stronger result than either agreeing with a refined version of
that ancestor would have been.

## The three-re-framing stop rule worked, and the retirement it produced is not the same move as a fourth re-framing

The campaign set, in advance, a cap of three defensible post-unblinding
re-framings before a question stops being evidence and returns to the owner.
It fired exactly once, at H1‴, and the diagnosis above — the reference was
wrong — was found *because* the question went to Nathan instead of being
reframed a fourth time. Retiring H1 as mis-specified is not a fourth
framing of the same question; a fourth framing would have kept adjusting the
test until some configuration said yes, while retiring abandons the original
claim as unanswerable-as-posed and reports a different, already-measured
property (H8) instead. The chronicle states this distinction explicitly
because it is easy to read a retirement as a convenient way to stop losing,
and it was not that here — the retirement is what the diagnosis earned.

## Estimate delta: a task split mid-flight, and what the split surfaced

Task 3 was planned as one task and became 3a/3b once scroll-and-zoom math
turned out to be expressed in plate *dimensions* — building it against a
40-column plate and then changing the plate's width (which 3a itself had to
do, to answer H1‴) would have invalidated 3b's own tests before they were
written. Splitting also forced a deliverable neither planned task had been:
nothing in the codebase called `plate::draw` at all before 3a, so "wire the
plate into the live redraw path" turned out to be real, unplanned work
rather than a formality. And the split surfaced a latent bug from Task 2 that
had been invisible for two tasks: `draw_with` normalized a window origin
against screen size instead of the virtual chart, which nothing exercised
until zoom existed to move the two apart.

## Deferred minors, with homes

- ~~`mercator::Frame` supports `pole_lat_deg = -90` through a sign factor in
  `to_frame`/`from_frame`, but `frame_for` never produces that value, so the
  branch is dead code, untested, and reviewer-verified correct by hand
  rather than by a test. Left as documented dead code rather than deleted or
  tested — YAGNI, at the final review's discretion.~~ — **RESOLVED at final
  review.** Correcting "YAGNI, keep it documented" was itself wrong: the
  sign factor was not merely unused, it was a REFLECTION for the case it
  handled, not a rotation — `sign * lat_deg` with no corresponding sign on
  longitude — so a future campaign that found a way to reach it would have
  gotten a wrong answer, not a dead one. Deleted, with a `debug_assert!`
  standing in place of the invariant it relied on (`pole_lat_deg >= 0.0`,
  true of every `Frame` this crate constructs). Choosing "document and defer"
  for something later found to be actively wrong rather than merely unused
  is itself worth keeping as a lesson: unreachable-and-correct and
  unreachable-and-wrong look identical from the outside of a `#[cfg(test)]`
  boundary, and only reading the branch's own arithmetic tells them apart.
- The H1‴ component-size figures (27.6%/25.0% for the shipped plate) are
  unverifiable from the repository: the measurement script was deleted
  before commit, per its own brief. The reviewer's independent check —
  8-connectivity biases *toward* merging components, so a methodology tilted
  against the falsification conclusion still failing to find one dominant
  landmass strengthens rather than weakens it — is recorded in the chronicle
  and spec §A10b, but the script itself is gone and the numbers cannot be
  regenerated without rewriting it.
- `clamp_caption`'s cross-regime exact-equality path: `centre_on(-90.0, 0.0)`
  yields a `Frame` byte-identical to `frame_for(true)` regardless of the
  world's actual rotation regime, which would emit the locked-world caption
  on a spinning world through this one narrow trigger. Judged unreachable in
  play — `unproject`'s frame latitude is bounded to ±85°, so landing on
  geographic lat exactly ±90.0° through the projection chain is measure-zero.
  **The documentation gap this bullet flagged is closed at final review**:
  `clamp_caption`'s own doc now states the ±85° argument directly (it
  previously defended only the same-regime coincidence), so a reader no
  longer has to reconstruct the bound from `unproject`'s doc to trust the
  claim. Still no test, by the same measure-zero reasoning — a test would be
  asserting behaviour at an input the function can never receive.
