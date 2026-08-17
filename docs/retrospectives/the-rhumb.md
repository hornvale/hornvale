# The Rhumb — retrospective

**Merged:** 2026-08-16

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-rhumb.md): compass navigation
resolved through a carried rhumb course, the three-edge adjacency graph
untouched, and a preregistered hypothesis half-falsified with the mechanism
measured rather than guessed.

## 1. Almost every defect this campaign found was in the plan text

This is the useful page. Across seven tasks, the defects found by
implementers and reviewers were, with two exceptions, in prose **I** wrote —
not in the code anyone wrote from it.

| the defect, in my plan or brief text | what it would have cost | found by |
|---|---|---|
| `test_session(42)` / `session_for_seed(42)` — invented helper names, no such function anywhere in the crate | three tasks written against an API that does not exist | the pre-dispatch grep |
| **`Session<'_>` borrows its `World`**, so no helper can return a bare `Session` at all | a type-level impossibility, not a naming slip; it dictates the shape of every test in three tasks | the pre-dispatch grep |
| `Session` is not `Clone` and must not become one (it carries caches whose purpose is to outlive a tick), but Task 5's test called `s.clone()` | a test that cannot compile, or a cache invariant quietly broken to make it | the pre-dispatch grep |
| a `type-audit:` tag placed on a **field's** doc comment, when the extractor reads only the struct's own | a red `type-audit check` on a tag that looks right | the implementer |
| a probe script that walked one session through all eight directions **in sequence** | the campaign's headline number, wrong — see §3 | Task 3, two tasks later |
| "all nine PASS" written beside an enumeration of eleven | a count in prose next to an enumeration is a second source of truth that cannot be kept honest | the implementer |
| a run command naming `-p hornvale-game-core`, which is in the workspace `exclude` list and unreachable by `-p` | a control that cannot be invoked | the pre-flight scan |
| `other => panic!("{other:?}")` where `Turn` has no `Debug` | does not compile | the implementer |
| **four separate vacuous assertions** | see §2 | reviewer mutation, three times; the implementer once |

Two things about this table. First, the detection mechanism that paid best was
the cheapest: **grepping one task ahead, before dispatch, for every name the
brief uses.** It is three minutes and it caught the three that would have cost
the most. Second, the exceptions prove the rule — the implementers' own
deviations were, in each case, *corrections* to my text found by running code
rather than reasoning about it, and one of them (the H3 control) was better
than what I had asked for.

The generalisable form: **a plan is code that runs on a person, and nothing
type-checks it.** Every name in a brief is an assertion about the repository,
and the only cheap way to check an assertion is to grep it.

## 2. Four vacuous assertions, and none was found by a passing suite

Each of these was green, and green for a different wrong reason:

- `a_step_length_lies_between_its_smallest_and_largest_neighbour` — a
  first-neighbour sample **is** one of the three values, so it sits in
  `[min, max]` trivially. The reviewer mutated `step_length_rad` to return
  exactly that sample: all 11 tests passed.
- The continuity assertion `assert_ne!(second.reckoned, first.reckoned)` —
  passes under a re-seed mutation, because a re-seeded point still differs
  from the previous one.
- H1's first probe — "latitude advances" plus "reckoned ≠ cell" — passes under
  the same mutation, for the same reason: a re-seeded point is still one step
  ahead of its cell.
- H3 — dead-reckoned, memoryless, and naive walks all reached exactly 101
  rooms against a `> 6` bound. It passed against the precise mutation its own
  doc claimed it would fire on.

The pattern is one thing said four ways: **when the broken version and the
correct version both satisfy the assertion, the assertion is about neither.**
Every one was caught by *mutating the code and re-running*, and none by reading
the test. The corollary the campaign now has evidence for: an assertion written
in the same sitting as the implementation it checks, by the same author, is
structurally unable to catch that author's misunderstanding — which is why
Task 2's review was deliberately bought at the expensive tier and told to
derive the mathematics from first principles rather than check code against
tests. It paid on the first finding.

H3's fix is the honest-outcome case worth keeping: re-scoped, it now states in
the code that **both H3-family tests are unaffected by a re-seed mutation**, so
H3 was never evidence about dead reckoning at all. Recording that beat keeping
a flattering name.

## 3. The headline number was wrong, and it was structurally checkable for free

Task 1 reported **3 of 8 directions refused**. The truth is **5 of 8**. The
probe walked one session through all eight directions in sequence, so every
successful `go` moved the possession and directions 2–8 were measured from
different cells.

The part that stings: a triangle has three edges, so at most three compass
buckets can resolve from one cell and **at least five must refuse**. "Five
succeeded" is not a surprising reading — it is an arithmetic impossibility, and
Task 1's own report asserted the three-neighbour fact two sentences from the
contradicting count. It was propagated into a board post and into a report to
Nathan before Task 3 caught it by measuring properly.

Two rules out of it. **A probe of what is available *at* a location must not
move.** And **check a count against the structure that bounds it before
believing it** — the bound was free and available the whole time.

## 4. The falsification surfaced from metric-chasing that was caught

A test fixture was moved from an address where the assertion failed to one
where it passed. That is metric-chasing by the standard test: the move would
not have been made had the assertion passed.

Both halves belong here. It happened — and it was caught, because the
implementer disclosed the change in full including the sweep that motivated it,
and the reviewer then **re-derived the mechanism instead of accepting the
stated one**. The stated mechanism (latitude) was wrong; the real one (local
triad alignment) falsified the hypothesis outright and showed the error is
unbounded. Had the disclosure been thinner, or had the review checked the
fixture against the claim rather than against the geometry, the campaign would
have shipped a passing test pinning a property that is false in general.

The resolution was to record the falsification, leave the preregistered text
untouched, keep the assertion with its documentation corrected to say what it
actually pins, and **not move the fixture again**.

## 5. A correction I owed publicly: the "two pre-existing vacuous tests" claim was false

I reported that the campaign had found two pre-existing vacuous tests in the
committed suite, and called it the campaign's second-best finding. It was not
true. The reviewer checked out the commit immediately prior, ran both tests
**unmodified** with a hard panic at exactly the point described, and neither
fired: at that commit the walker moved every iteration and a structure was
entered. Both tests were genuinely exercising real behaviour.

What was actually true is narrower and still good practice: the two tests
passed through a vacuous state in the implementer's own **working copy** during
Task 5, between the prose change and the test-parsing fix — a state no commit
and no gate ever ran. The implementer's own text said this accurately. **I read
it as a claim about the committed suite and amplified it.**

This is a failure mode the project has recorded before — a report's careful
sentence overstated by its reader, with the load-bearing detail ("*at which
commit*?") never checked. It is worth naming that the amplification happened at
the *controller* layer, where reports are summarised for a human, because that
is the layer with the least direct contact with evidence and the most
narrative pressure. The only thing that could have caught it is the thing that
did: checking out the commit and running the test.

## 6. The absorption cadence was missed, and the bill arrived at close

This branch met `main` exactly once — at Task 7. CLAUDE.md's cadence is an
absorption at every plan-stage boundary, and seven task boundaries went by
without one.

The merge itself was clean, so the cost was not conflicts. It was **staleness
of premises**, and two of them were load-bearing:

- `make gate-stage` and `make preflight`, which this campaign's plan and brief
  both instruct the closer to run, were **both retired** by The Sluice while
  this branch was working. Each is now a refusing signpost. The replacement,
  `make sluice-stage BRANCH=… REF=…`, gates the real main+branch merge product
  and never pushes.
- The decision number — see §7.

Neither would have been a surprise at a Task 3 or Task 5 absorption. Both were
surprises at close, which is the most expensive moment to meet them.

## 7. A claimed decision number tells you a claim exists, not how many

The spec reasoned carefully about this and still got it wrong, which is what
makes it worth writing down. It said, correctly, that `ls docs/decisions/` is
**not sufficient** — an unmerged branch can hold a number, decision-number
collisions are silent (different slugs, no conflict marker, no gap for
`no_gaps_in_the_decision_log` to catch), and the board is the only place the
claim exists before the file does. It synced the board, found The Sluice's
claim on **0139**, and concluded the next free number was **0140**.

0140 was also taken. The Sluice minted **two** decisions, and the board post
named the one it had taken so far, not the count it would end with.

So the sharpened rule: **a board claim establishes that a number is taken, and
nothing about how many.** The only sufficient check is against `main` at the
moment of writing the file, re-checked immediately before merge — which, on a
branch that absorbs at every stage boundary, is a check that happens for free
several times. This campaign used **0141**, verified against `origin/main`'s
own tree after absorption rather than against a board post.

## 8. Operational residue

- **Three heredoc-corrupted commit messages** — two implementers, once each,
  plus one near-miss of mine. `git commit -m "$(cat <<'EOF' … EOF)"` leaks a
  literal `EOF` and `)` into the body. Write the message to a file and use
  `git commit -F`, then read it back with `git log -1 --format=%B`. One
  implementer caught its own by reading the log back, which is the only
  reliable detection.
- **The shell's cwd silently reset to the main checkout twice**, and a
  `git add` ran against `main` both times. Both failed harmlessly on a
  pathspec miss — but only a `git log` check established that, not the exit
  code. Every worktree command carried an explicit `cd` afterwards.
- **Two regeneration paths were needed and the plan named one.**
  `make rebaseline` moved the transcripts, the client fixture and a timings
  row; the two sim-side byte goldens stayed red until `REBASELINE=1` /
  `make rebaseline-goldens`. A close that ran only the documented path would
  have handed a red branch to the gate.
- **A golden's drift needed a shape decision, and eyeballing 67 KB is not
  deciding.** The answer — 133 key paths identical on both sides, 1 of 888
  leaf values moved, and that one is prose — came from walking both documents
  and diffing the *set* of key paths. That tool is now
  `scripts/shapecheck.py` rather than scratchpad residue.
- **A deliberately red branch was carried across two tasks and shortened.**
  Three artifact tests went red at Task 5 on one embedded-string diff. The
  ruling was to rebaseline as soon as Task 5's review cleared rather than
  wait for Task 7, because a branch carrying three known reds is how a fourth,
  real red gets lost.

## 9. The stage gate caught a real defect that no local gate could

The close ran green locally — `gate-commit` 1425/1425, `docs_consistency`
23/23, drift clean — and the stage gate came back **held**: `clients phase
rc=2 (vessel-check-run)`.

It was ours, and the mechanism is worth the space.
`clients/vessel/wasm/drive.mjs` — the Casement's wasm smoke driver — walked
the possessed character by **parsing a direction out of the opening prose**:

```js
const ways = golden.match(/^Ways on: (.+)\.$/m);
assert.notEqual(ways, null, "opening lists its ways on");
const dir = ways[1].split(", ")[0].toLowerCase();
```

Task 5 deleted that sentence. The opening now reads *"No direction here is
closed; the nearest ground lies E, NW, SW."*, so the regex matched nothing and
the driver died on its own assertion. A/B against `main` confirms attribution
rather than assuming it: `Ways on:` appears 8× in main's committed transcript
and 2× in ours, and the two survivors are chamber-band (`out, further in`),
which this campaign never touched.

**Why nothing local saw it.** `clients/` is outside the cargo workspace with
its own toolchains, so `make vessel-check` is in none of the three gates —
it is exactly the "gate freshness ≠ complete" class this project already
knows about, and the campaign regenerated `clients/game/core/tests/fixtures/`
diligently while never running the *sibling* client's smoke test. The
generated-artifact discipline covers committed fixtures; it says nothing about
a hand-written driver that **parses** one.

**The repair is not a re-pin.** The comment above that parse explained itself:
it avoided hardcoding a compass point because "a worldgen epoch may reshape the
seed-42 opening room's exits, and this smoke asserts *walking works*, not any
particular geography." That reason is exactly what this campaign abolished —
after 0141 all eight points resolve from every walk-band cell — so the driver
now sends a fixed `go n` and pins the exits sentence separately. Hardcoding is
the *stronger* check here, not the lazier one: if any of the eight ever stops
resolving, the smoke fails, whereas a parse would quietly follow the prose
wherever it went. Verified by running it, not by reasoning: full
`clients-check-run` (vessel + world + game + atlas) exits 0.

**The generalisable lesson.** A test that derives its input by parsing
human-facing prose has a dependency the type system cannot see and no
drift-check will report, because the prose is *allowed* to change. Grep for
consumers of a sentence before rewriting it — and note that this campaign's
own §1 lesson (grep every name in the brief) would not have caught this one,
because the coupling is to a *string*, not a symbol. The instrument that
caught it was the merge-product gate, which is the argument for the sluice in
one line: a branch tip that gates green locally is not evidence about the
object that lands.

## 10. Deferred, with homes

Every one has a registry row, which is the point of listing them:
`NAV-north-up-needs-per-cell-position` (Task 6's blocker, and the successor
campaign's first move), `NAV-bias-correcting-resolution` (whether minimising
*accumulated* cross-track would bound the error — a different algorithm, not a
fix), `TOOL-compass-bucketing-duplicated-in-a-test`,
`TOOL-course-bearing-compares-with-float-eq`, and the four carried-forward
appearance rows from spec §8 (`RENDER-appearance-signal-protocol`,
`RENDER-surface-mixture`, `RENDER-nominal-cover-class`,
`RENDER-fogged-world-map-rung`).

**Do differently next time.** Absorb at stage boundaries even when the branch
is a single session's work — the cost is not conflicts, it is premises going
stale under you. Grep every name in a brief before dispatching it. And when
summarising an implementer's report for a human, quote the sentence rather
than characterising it.

## 11. A phrase that named a set it did not own

The stage gate held red on the `clients` phase — the first `kind=stage`
request this repository ever ran, failing on its first real use, which is the
best possible outcome for a new instrument.

`clients/vessel/wasm/drive.mjs` line 65:

```js
const ways = golden.match(/^Ways on: (.+)\.$/m);
assert.notEqual(ways, null, "opening lists its ways on");
```

Task 5 changed that sentence deliberately: `go` now accepts all eight compass
points, so a three-item list implied five were closed, which was false.

**The instruction was followed correctly and the outcome was still wrong.**
The brief said: *if the existing test suite has tests asserting the old
sentence, update them and list every one.* The implementer did exactly that —
four Rust tests, found, fixed, listed. But "the existing test suite" silently
denoted the **cargo workspace**, and `clients/` is outside it by construction:
decision 0055 puts the determinism boundary at the repo boundary, and
`Cargo.toml`'s `exclude` list is what made the phrase wrong. The scope of an
English noun phrase was set by a manifest nobody reads while writing prose.

This is not "we forgot to grep `clients/`". A reader checking the instruction
against the code could not have found the gap, because the gap is in neither —
it is in the correspondence between them.

**Five consumers, one assertion.** Beyond `drive.mjs`: `transcript.ts` was
misclassifying the new line as body prose rather than meta, `entry.rs`'s module
doc described protecting a sentence that no longer exists, and two Rust tests
parsed it. Only the wasm smoke had an assertion, which is why only it went red.

**The counterintuitive half is that every schema check passed.** A key-path
diff put the vessel goldens at **133 key paths identical** with **1 of 888 leaf
values** moved — `.narration.prose`. The wire was provably fine. What broke was
a consumer parsing prose out of a correctly-shaped payload: exactly the failure
a schema guarantee cannot exclude, and one the cross-repo scene-contract
discipline does not cover either, since that governs *shape* and this was
*content* in a field whose content is the point.

**The rule earned:** a change to any `describe_*` sentence is a cross-tree
change. Grep its consumers across `.ts`, `.mjs` and `.js` as well as `.rs`.
The only gate that can catch it is the sluice's `clients` phase, and
discovering it there costs a slot on a serial resource every campaign shares.

**The repair was the stronger kind, not a re-pin.** The comment above that
parse justified itself: it avoided hardcoding a compass point because *"a
worldgen epoch may reshape the seed-42 opening room's exits."* That reason is
precisely what this campaign abolished. So the driver now sends a fixed `go n`
and pins the exits sentence separately — a regression in any of the eight now
fails the smoke, where a parse quietly followed the prose wherever it went. A
test that could not fail was replaced with one that can.

**Credit:** the queue operator ran `make vessel-check` against `main` alone
before attributing the red, having previously held an innocent candidate for a
trunk defect. Without that control the failure could not have been attributed
from the campaign's side. It also disclosed its method so the verdict could be
discounted, and offered a re-run in case of a flake — declined, because a fixed
regex against a deliberately changed string is deterministic by construction.
