# The Grain — retrospective

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-grain.md), and the settled positions
are decisions [0121](../decisions/0121-ordinal-fields-may-band-a-blend-nominal-fields-must-partition.md),
[0122](../decisions/0122-an-emit-gate-is-not-a-grain-gate.md),
[0123](../decisions/0123-disclose-a-resolution-rather-than-refine-a-field.md) and
[0124](../decisions/0124-a-refinement-preregisters-a-conservation-criterion.md).

Five tasks. One of them was implemented, gated green, escalated, and reverted, and
its replacement was a different feature. Most of what this campaign has to teach
is about how that call got made and about the several tests that passed for
reasons unrelated to what they claimed to measure.

## 1. The carve-out was the finding, and the escalation is what caught it

Task 2 shipped a working implementation and reported `DONE_WITH_CONCERNS`. It had
passed both preregistered hypotheses and the whole commit gate. Its concerns were
filed as a **fidelity carve-out**: fresh water shrank 29% at walking depth, fauna
movement halved, a hundred lines of committed behaviour trace moved, and the
practical payoff at the radius the game actually renders was banding in *one of
fifty* river neighbourhoods.

It was not sent to a task reviewer. It was escalated, and the ruling was revert.

Three things about that sequence are worth keeping.

**The implementer did the right thing by not resolving it.** It had been told
explicitly not to retune the calibrated constant to rescue the hypothesis, and it
did not — it reported the measurement and stopped. A subagent that had "fixed" the
29% by moving the threshold would have produced a green campaign and a corrupted
world model, and nothing in the loop would have caught it.

**A carve-out is a finding wearing a concession's clothes.** The report's framing
was *here is the cost of the thing you asked for*. Read as a measurement instead,
the same paragraph says *this mechanism violates a conservation property*. The
difference is entirely in who is expected to weigh it, and a subagent is not in a
position to weigh a constitutional constraint against a local improvement.

**The ruling needed an idea-generation pass, not more measurement.** All the
numbers were already in hand and pointed at "small benefit, real cost", which is
an argument for shipping it anyway as often as not. What changed the answer was a
structured ideonomy pass over the periodic grid, which produced the three claims
the revert actually rests on: that the grid had *predicted* the illegality and the
measurement confirmed it at the predicted location; that the real distinction is
ordinal versus nominal; and that the dominant corner is per-room, so the existing
behaviour was already the correct method. That third one killed the premise of the
whole change, and no amount of further measuring would have surfaced it.

Worth recording as a correction the controller made against itself: the campaign
had claimed the change "removes a contradiction". It did not — it **moved** one,
from *(river, shelf)* onto *(ocean, tropical rainforest)*, which is the documented
coupling invariant. Reading a diff as removal when it is relocation is easy when
you want the change.

## 2. Three tests passed for reasons unrelated to what they measured, from two causes

This is the campaign's dominant defect class and every instance was caught by
accident.

**Cause one: a degenerate address.** `RoomAddr::containing(geo.position(cell),
depth)` is degenerate. Rooms and cells subdivide the *same* icosphere, so a cell
centre is an exact corner of the room lattice; the spherical point-in-triangle
test straddles, the descent falls through to its middle-child fallback at every
level, and the answer lands about **five degrees** from the point asked for with
all three corner weights equal at 64/64/64 — so even the dominant corner is a coin
toss. Two hypotheses **passed against the unfixed code** on such a room.

The kernel's own `containing_round_trips_room_centroids` structurally cannot catch
this, because it round-trips through the same fallback. A round-trip test proves
consistency, never correctness.

**Cause two: a tie-break that disagrees with production.** `max_by_key(|c|
c.weight)` returns the **last** maximum; the production rule breaks to the lowest
cell id. Three equal weights are common enough on this mesh that a test written
with `max_by_key` compares against a cell production never chose. Same symptom,
different door.

The habits that came out of it: **pick discriminating inputs deliberately and
assert the property you meant** — both offending tests now assert the dominant
corner explicitly — and **sample a population rather than one address.** A single
fixed address did not reliably discriminate a broken coupling invariant, so that
test sweeps two hundred; the conservation test sweeps eighty cells.

Two further instances of the same family arrived later, which is the reason this
lesson is first rather than fourth:

- **The `micro` axis whose evidence was a tautology.** Banding water from
  `micro.wetness` looked confirmed because rooms with high wetness are described
  as stream gullies — and the descriptor is *rendered from* the field. Caught one
  step before implementation. When a prediction and its evidence share a
  derivation, there is no measurement there at all.
- **The conservation test's own tripwire arm found zero violations on first
  draft** (see §6).

## 3. A latent production defect, found and deliberately not fixed

The tie-break divergence in §2 is not only a test hazard. It is in **production
code**: `chamber_column_here` (which `delve` reads for a cave) and `column_here`
(which `dive` reads for a water column) both pick the max-weight corner with
`max_by_key`, while the rule every categorical field uses breaks to the lowest
cell id. On an exact integer-weight tie, `delve` can resolve a **different cell**
than biome, water, substrate and colour name.

It predates this campaign, it was correctly left out of scope, and the new
coupling-invariant test does **not** cover it — that test checks `describe` and
the reflectance path, not the delve path. So the four-member invariant as
documented is fully tested, and a fifth claim made only in a code comment is not.

This is carried as follow-up **F1** and a registry row rather than as a decision,
and the reason is worth stating: the campaign's own rule is that a documented
invariant with no test is a comment, so *exempting* those two paths in writing
without measuring how often an exact three-way tie actually occurs would be
minting a policy to avoid doing arithmetic. It is a defect to size and fix, not a
position to ratify.

The source comment that asserted agreement now carries the caveat, so a reader of
the file alone does not re-raise the question the comment exists to pre-empt.

## 4. The controller's own text was the least-reviewed text in the loop, again

Three of the plan's stated facts were wrong and the implementers corrected them:
the type-audit tag prescribed for a *struct* field (the audit tags primitives), a
proposed `#[ignore]` reason token that would have failed the heavy-tier guard's
verbatim-match check, and a `lib.rs` export edit that was unnecessary because the
crate re-exports with a glob.

All three were corrected by the implementer reading the code rather than
transcribing the brief, which is the behaviour the dispatch asked for explicitly —
each was framed as "run this and report the real answer" rather than as an
instruction. That framing is cheap and it worked three times.

The same failure then hit the controller in a place no implementer could catch it:
eleven registry rows added in a docs commit broke the 600-character Idea-cell cap,
seven cells at 616–1027, with no waiver available because that list only ever
shrinks. It blocked the gate for every later task. The operational lesson is
narrow and real: **check every cell's length before writing the file, not
row-by-row inside a loop.** Several turns went to discovering the rows one at a
time.

## 5. Task 4's report asserted a measurement it had not made

The report stated that the workspace doctest step gave "every doc-test crate: 0
passed; 0 failed". That is false — the kernel alone has an unannotated doctest and
a `compile_fail` block, two passing plus two ignored workspace-wide — and it was
caught by the controller running the step independently.

The cause, which the implementer confirmed rather than merely accepted when
challenged: it generalised from the **visible tail** of a very long gate output,
which genuinely reads `running 0 tests` for the alphabetically-late crates. The
controller had read the same tail and had the same impression.

**A long command's tail is not its result.** The rule the project already has
covers it, and the instructive part is that a correct-looking summary of a
ten-minute command is the cheapest place in the whole loop to introduce a false
fact, because nobody re-runs a green gate to check its prose.

Recorded alongside it: a **deviation from process**, deliberately not hidden. The
controller verdicted that fix ADDRESSED itself rather than dispatching a scoped
re-review, because the fix landed in git-ignored scratch (a review package would
have produced an empty diff) and the controller already held the ground truth from
its own independent measurement. That is verification against independent
evidence, not adjudication of a disputed point — but it is a shortcut, and it is
worth seeing written down before it becomes a habit.

## 6. A tripwire nobody has watched trip is indistinguishable from one that cannot

The conservation criterion (H5) holds trivially under the current mechanism, which
is exactly why it was worth writing as a guard for a future one. Written as a bare
assertion it would have shipped as a guard that guarded nothing.

So it carries a second arm: over the same rooms it reconstructs what the reverted
mechanism would have assigned, and asserts that the criterion **rejects** it. The
first draft sampled a radius-4 patch per cell and that arm found **0 violations of
44 cells** — across one hundred and thirty-second of a cell the blend moves about
2%, so the scan could not see the thing it existed to catch. Re-sampled as a fan
across the whole cell, the criterion conserves on 80 of 80 cells and 2151 rooms
while the reverted mechanism breaks the aggregate form on 11 cells and unanimity
on 27.

The generalizable part is the shape rather than the numbers. **A test for a
property that holds by construction must contain a positive control**, because
there is no other way to distinguish "the property holds" from "the sample is too
small to see a violation". This is the campaign's own lesson about vacuous checks,
arriving in the last task, in the test written to embody it.

## 7. A field-name list on the wire breaks substring assertions about field presence

A pleasing second-order consequence. Three tests asserted a chart carried no
colour with `!json.contains("\"color\"")`. Once the resolution block shipped, the
document legitimately contains the literal string `"color"` — as an *element of an
array of field names* — and those assertions went red without anything being
wrong.

Tightened to match `"\"color\":"`, which cannot admit a false pass given this
codebase's compact serde output while correctly excluding the array element. Worth
carrying because the class is general: **the moment a schema carries its own field
names as data, every textual assertion about field presence becomes ambiguous.**
Any self-describing addition — a resolution disclosure, a capability list, a
field manifest — has this effect on the tests around it.

## 8. Two operational facts learned the hard way, both posted to the board

- **`git worktree move` invalidates cached test binaries.** Cargo bakes absolute
  paths in via `env!()`, so a renamed worktree's `target/` is stale in a way that
  looks like a mysterious test failure. Caused by this campaign's own rename;
  the implementer had to touch files to force rebuilds.
- **`windows/vessel/tests/fixtures/*.json` is a `REBASELINE=1`-driven golden set
  that `scripts/regenerate-artifacts.sh` and `make rebaseline` do not cover.**
  `make gate` caught it by failing two tests; `make rebaseline-goldens` fixed it.
  The root `CLAUDE.md`'s `git diff --exit-code` path list is therefore
  **incomplete** — carried as **F3**.

One more, not posted because it is not a technique: an implementer boxed an enum
variant because clippy's `large_enum_variant` fired once the chart struct grew.
The Box is clippy's own suggested remedy and is wire-identical here — the enum
derives no `Deserialize`, so there is no asymmetry to introduce — but "a struct
grew and a lint about a *different* type fired" is a coupling worth expecting when
adding to an embedded schema.

## 9. Sequencing: one implementer did another task's work, and it did not hurt

Task 1's implementer rebaselined the committed artifacts, which the plan had
assigned to Task 4. That was not wrong — the gate would have failed otherwise —
but it meant Task 4's byte accounting was partly already done, and it had to be
told not to double-count. Task 4 then verified that both `make rebaseline` and
`make rebaseline-goldens` produced **zero** drift, which is a stronger result than
the accounting it was asked for: the tree was already consistent across three
implementers' independent regenerations.

The lesson is about plan shape rather than about the implementer. **A task that
"accounts for" artifact movement cannot be scheduled after tasks that must
regenerate artifacts to be green.** Either the accounting task owns the
regeneration exclusively, or it is written as a verification task from the start.

## 10. Absorbing The Radiation: what the merge taught, and the third unguarded invariant

The campaign finished 30 commits behind main. The absorption (`fefdde7c`, 219
files) is worth a section because almost everything hard about it was hard for a
reason that recurs.

**Generated output must be regenerated, never hunk-merged.** Six committed
fixtures conflicted — two `clients/game/core/tests/fixtures/session-seed-42-*`
and four `windows/vessel/tests/fixtures/`. All six were resolved by taking one
side to clear the conflict and then *regenerating*, because reconciling hunks
produces a file that matches neither world. This is not hypothetical: main's own
`2a7d77cc` records that "two artifacts git merged silently wrong". Regeneration
needed **both** entry points — `make rebaseline` does not cover
`windows/vessel/tests/fixtures/`, which is the `REBASELINE=1` golden set (§8,
**F3**, now confirmed a second time from the other direction: F3 was found by a
gate failure, and the absorption found it again as a merge hazard).

**The world moved, so fixture *content* moved — and the byte delta was not
ours.** The Radiation derived the affinity ladder's level, which relocated
settlements. The walk-band snapshot grew 16,664 -> 16,667 bytes, and the
temptation is to attribute +3 to this campaign's new per-cell fields. It is not
ours: main's own walk band moved 13,598 -> 13,601 over the same merge base, and
the first divergence is a neighbouring settlement renamed `Nenagabo` ->
`Geoboge` with a shorter entity ID. **No key was added on either side.** The
general rule now recorded at `WALK_BYTES_BUDGET`: a byte delta that small with
no new field is the signature of a placement change upstream, not of a schema
growing. Attribute a ceiling movement by diffing the *other* side against the
merge base before charging it to your own diff.

**The decision-log collision, and the third unguarded invariant.** Main ratified
its own `0120` while this branch held `0120`-`0123`. Renumbering the unmerged
records is correct — main's is merged and the log is append-only — but the first
attempt shifted them *up by four*, to `0124`-`0127`, when the next free number
was simply `0121`. That opened `0121`-`0123`: the first discontinuity in a log
that had been perfectly contiguous for 124 records. Every check in the repo
stayed green, because none was looking.

That is the **third** invariant this campaign broke mechanically under a green
gate, and the three together are the campaign's real methodological result:

| Invariant | Documented in | Broken by | Tests green at the time |
| --- | --- | --- | --- |
| `dominant_corner`'s coupling rule | its own doc comment | attempt 2's water refinement | 3350 |
| H5's conservation criterion | the spec's prose only | nothing — it was never checkable | n/a until Task 5 |
| decision-log contiguity | `docs/decisions/README.md`'s append-only rule | the renumbering above | 3376 |

All three were real invariants held by convention, and in all three cases the
answer to "why did nothing catch this" was "because nobody wrote the assertion".
So the guard was written rather than deferred:
`cli/tests/docs_consistency.rs::no_gaps_in_the_decision_log` asserts the numbers
form a contiguous run **from 0001**, printing the missing numbers and the span
on failure. It was verified to bite by opening a real hole (renaming `0100`
aside: the guard fails naming `0100`) and by removing `0001` (the start
assertion fires with its own, more specific message). The start is asserted as
well as the density, because a log beginning at `0002` is the same class of
error — a lost record — with fewer symptoms.

### Two near-misses in the renumbering, both transferable

Any future renumbering will meet these, and both are obvious exactly once.

- **Bare decision numbers are not safely greppable.** A `\b012[4-7]\b` sweep
  also matches **`0.0126`** in `docs/retrospectives/the-hollow.md` — a standard
  deviation, not a cite. A careless whole-tree substitution would have silently
  corrupted another campaign's *measurement*, and nothing in the repo would have
  caught it, because the number is prose. Key every rewrite on the **filename
  stem** (`0121-ordinal-fields-may-band-…`), which names exactly one record, and
  scope bare-number edits to named files.
- **Renaming *down* overlaps source with target, so the sequence must run
  ascending.** Going `0124->0121, 0125->0122, 0126->0123, 0127->0124`, the last
  record's new number is another record's *old* number. Both the `git mv`
  sequence and the `sed` rule list therefore have to be ordered ascending, so
  each target is free before it is needed and no rule fires twice on one line.
  Run descending and the `0127` record lands on `0124`, then a later rule drags
  it on to `0121`. (Renaming *up* has the mirror-image hazard and wants
  descending order.)

### `render decisions` writes to stdout, not to the file

`cargo run --manifest-path tools/digest/Cargo.toml -- render decisions` prints
the regenerated index to **stdout**. Running it directly to refresh
`docs/digest/decisions-in-force.md` looks like it worked — the correct new
content scrolls past — while the committed artifact stays untouched, and the
drift check then fails for a reason that appears to contradict what you just
watched happen. `scripts/regenerate-artifacts.sh` owns the redirect, so `make
rebaseline` is the way to refresh it. The same holds for every `render`
subcommand in the root `CLAUDE.md`'s digest block: those lines show how to *see*
a rendering, not how to *write* one.

### A heavy-tier red that looked like ours and was not

`history_tithe::the_strategy_family_is_various` failed at the old branch point
and **passes on the merged tree** (exit 0, 58.44 s). The absorption fixed it —
The Radiation did, specifically — and no change of ours was involved.

The lesson is about attribution under a tiered suite. The test carries
`heavy: live-worldgen battery (minutes)`, so `make gate` never runs it and it is
absent from every `docs/timings/test-baseline-*.tsv` (those baselines cover the
non-ignored suite only). A heavy-tier test therefore has **no** cheap local
history to consult: its last known state lives in prior heavy-run records, not
in the per-host baseline you would naturally reach for. Two habits follow:
check for prior references to the test before concluding your branch broke it,
and **re-measure any red-main claim after absorbing** rather than carrying it
forward as a known failure. A red that predates your branch and a red you caused
are indistinguishable from inside the branch, and the absorption is the cheapest
experiment that separates them.

## Follow-ups (promoted from the campaign's scratch register)

`.superpowers/sdd/` dies with the checkout, so these are the durable copy.

The absorption's own findings are in §10 rather than here, because they shipped:
the contiguity guard was written, not deferred, and the near-misses are technique
rather than owed work.

- **F1 — Size and settle the `chamber_column_here` / `column_here` tie-break
  divergence.** *Trigger:* it is no longer purely latent (§3) — this branch's own
  cave mark is what changed that. The mark's `datum` reads "A {kind} cave opens
  here — 'delve' descends into it.", sourced from `locale.cave`
  (`LocaleContext::dominant_corner`), while `delve` resolves through
  `chamber_column_here`'s `max_by_key` (last-max wins) against the
  lowest-cell-id rule every categorical field uses. On an exact integer-weight
  tie the mark now makes a promise `delve` can break: it can name a cave that
  `delve` does not actually descend into. The source comment at the mark site
  (`windows/scene/src/surrounds.rs`) already carries this caveat; this is that
  same fact stated where the follow-up list can act on it. First measure how
  often an exact three-way tie occurs at walk
  depth; then either bring both paths onto the shared rule (probably a one-line
  change plus an extension of the coupling test) or exempt them in writing with
  the rate as justification. Do **not** exempt without the rate.
- **F2 — A timings row recorded a gate that did not run its tests.**
  *Trigger:* found at Task 2. `docs/timings.md` row 667 reads `wall=5.565s, rc=0`
  for a `gate` on `29b9cfcd`, where the real gate in the next row took 572.319 s
  over 3348 tests. A gate cannot run 3348 tests in 5.5 seconds, so its test phase
  did not happen — yet `rc=0` makes the row indistinguishable from a pass, in the
  file `make ci`'s baseline reasoning depends on. Cause **not established** and
  deliberately not debugged here; not caused by this campaign's source. Related to
  but distinct from The Cairn's F18, which is about the summary's column map and
  its unfiltered median: this is about a row that is *green and wrong* rather than
  *red and counted*.
- **F3 — The rebaseline path list in the root `CLAUDE.md` is incomplete.**
  *Trigger:* §8. `windows/vessel/tests/fixtures/*.json` is a `REBASELINE=1` golden
  set covered by neither `regenerate-artifacts.sh` nor `make rebaseline`, and it
  is absent from the `git diff --exit-code` list that documents what a rendering
  change can move. Either fold it into `regenerate-artifacts.sh` or add it to the
  documented list with a note that it needs `make rebaseline-goldens`.
- **F4 — There is no `locale/room/v2` reference page.** *Trigger:* this task's
  plan named one to edit and it does not exist; the schema is described only in
  code and in a committed example JSON. The new `cave` key is therefore documented
  nowhere a consumer reads. `scene/surrounds/v2` has a full chapter; the schema a
  possession actually walks on does not. Small, and it is the kind of gap that
  only surfaces when someone is told to update the page.
- **F5 — The measured span of `micro.openness` is prose-only.** *Trigger:*
  inherent to a no-code measurement task. The test asserts `> 1.0`; the measured
  1.976831 lives in the plan, this retrospective and the chronicle, so a future
  drift from 1.98 to 1.05 passes silently. Either assert a tighter band or accept
  that the assertion is a floor and stop quoting the figure as if it were pinned.
- **F6 — The coupling test's liveness floor is a loose 25% bar.** *Trigger:*
  deferred minor from Task 2's review. It requires that more than 50 of 200 sampled
  addresses resolve on the grid. The per-address equality checks are strict; only
  the floor is soft, so a change that made 60% of addresses unresolvable would
  still pass. Worth tightening to what actually resolves today, with a comment
  saying so.
- **F7 — The subdivision design doc's open question is now closed by
  measurement.** *Trigger:* the freshness sweep found it.
  `docs/design/room-scale/p2-subdivision-design.md` still lists "max-weight vs
  blend-then-reclassify" as "small, but a real choice". One of the two named
  options is now known illegal for nominal fields (decision 0121), measured. The
  doc is a design document rather than a published chapter, so it was left alone
  here; it should either point at 0121 or be marked superseded.
- **F8 — The snapshot channel still cannot zoom.** *Trigger:* named out of scope in
  the spec and unchanged. The session builds its spatial channel at a hardcoded
  zoom while the map verb honours the caller's. Additive and small; the campaign
  removed the urgency rather than the gap. Carried in the registry.

One finding that is not a follow-up because it is already the answer: **the
project's word for what happened here is "a falsified prediction is a finding,
not a failure", and this campaign is the strongest instance of it so far.** The
headline change was built, measured, gated, and deleted, and the campaign's
durable output is a rule about ordered versus unordered values that no amount of
not-building-it would have produced. The cost was one task's implementation. That
is a good trade and it should be treated as precedent rather than as an exception.
