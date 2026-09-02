# The Avowal — retrospective

Process, not product. The chronicle carries what was built.

## The campaign's real subject is wiring, not vocabulary

Two mechanisms this campaign touched were already built and already silent
before a line of code was written: `domains/history::descent::ancestor()`
("reserved and currently unconsumed," its own doc says, and still is — its
sibling `kinship()` got wired to `parent-of`/`kin-of`, `ancestor()` itself
did not); and `windows/sentiment::snap_judgment` (commits nothing — "no
world is built," its own module doc says, and stays that way on purpose at
the people grain). A third, the tone tier (`tonality` reads `0.0` on all 23
authored rows), was found in passing and is not this campaign's to fix.
Kinship (the ledger) and affect (the component layer) both got wired this
campaign; acts got a third home (session state) neither prior example used.
A project this large accretes capability faster than it accretes the words
for it, and finding these was a byproduct of an audit built to measure
something else entirely — the trope corpus's stageability count.

**A fourth candidate on this list turned out to be wrong, and the reason is
its own instance of the section below.** The brief handed to this task named
`kernel/src/manifest.rs`'s `Manifest` as a fourth built-and-unwired
mechanism, quoting its module doc verbatim: "Stage 1 lands the types only —
nothing constructs a `Manifest` yet." The spec (§4.1) had already cited that
same sentence as a reason `Provision` reuses `Correspondent<T, V>`'s *shape*
without reusing `Manifest` itself. **The sentence is false, and checking it
cost one `grep`.** `ConceptRegistry::register_manifest` is, by its own doc,
"the only public path to add a concept to the registry"; every domain's
`register_concepts` constructs a `Manifest` per concept (astronomy,
settlement, language, religion, thing, species, terrain, climate — nine call
sites outside tests, none of them gated behind `#[cfg(test)]`); the
composition root (`windows/worldgen/src/lib.rs`) calls `register_concepts`
for every world; and `hornvale concepts --manifest` renders the result. The
mechanism is fully live and has been for a while — `PROC-16`'s own registry
row already says "Shipped: the `Manifest` choke-point is the sole
concept-registration path" — the module doc simply never got updated past
whatever stage it described when written. Corrected at the source
(`kernel/src/manifest.rs`'s module doc) rather than left standing. It changes
nothing about the decision to build `Provision` separately, which never
depended on `Manifest` having callers — but it is one more instance of the
next section's pattern (a committed record asserting more than the code
does), separate from that section's own tally of five and arguably worse:
this one was quoted across a spec, a decision, and this very task's brief,
inherited by each without being checked against the tree, and discovered
only by writing this retrospective — not by any task's own review — asking
what `grep -rn register_manifest` actually returns.

## A committed record asserted more than the code did, five times

Every one of these lived in a ratified decision or this campaign's own
ledger, not in scratch, and every one was caught by a *later* reader running
something rather than by re-reading:

1. Decision 0577's original text said a relation-less witness "binds
   vacuously to any situation whose requirements name no predicate token."
   A live probe (round 2) found it bound to **any** situation, predicates or
   not — the disclosed limit was narrower than the real one.
2. The fix for (1) — decision 0582 — closed with "actant-role assignment
   ... is now the ONLY disclosed limit." A third review's live probe (a
   `phenomenon:eclipse` requirement resolving `Stageable` with nothing
   staging an eclipse) found that false too. **This is the pattern's
   sharpest instance**: the record fixing the first false-completeness claim
   made a second one, in the same paragraph shape, about a different limit.
3. Decision 0578, as shipped, claimed `parent-of` would be "the first
   predicate in the workspace to exercise the functional-contradiction guard
   ... as a relation." False — `pays-tribute-to`, `person-founded` and
   `occ-founded-from` all predate this campaign and are all `functional:
   true` `Value::Entity` relations. The false framing was inherited from an
   earlier review comment and repeated without being checked.
4. The same ledger entry recording (3) also claimed a specific test case had
   been added to `kinship_facts.rs`. It had not — checked directly against
   the file's git history at review time.
5. **The fix for (3) and (4)** — the same commit that reversed `parent-of`'s
   direction and restricted it to `Ancestor(1)` — left both predicates'
   `register_predicate` doc strings describing the OLD, just-rejected
   direction. The fix fixed the data and left the prose describing the
   defect it had just removed. Found by the next review round, not by the
   implementer re-reading the diff they had just written.

Three of the five (2, and the pair in 3/4 considered together with their
fix in 5) landed **inside the commit meant to close the previous one** — a
correction adding a fresh instance of the shape it was correcting, the same
finding several prior campaigns' retrospectives already carry
(`a-correction-is-unaudited-text`). What eventually held, decision 0583, is
the record that stopped asserting completeness at all: "the limits include X
and Y" survives discovering a Z; "X is the only limit" does not survive any,
found or not. That is not a stronger enumeration — it is refusing to make
the claim that kept failing.

## A truth standard applied at n > 1, and never checked at n = 1

Decision 0584 restricted `parent-of` to `Ancestor(1)` on the ground that
"37 generations removed" fails the registered concept `parent`'s own
definition ("one's father or mother"). That test was never run at `n = 1`,
and it fails there too. `domains/history::descent`'s own module doc says the
ledger "does not commit a genealogy… What the edge encodes is descent at an
unknown remove" — seed 42's founding gaps run to a median of 50 years and a
maximum of 975, and `remove()` only rounds `gap_years /
generation_length_years` to the nearest integer. `Ancestor(1)` is exactly as
much an inference as `Ancestor(37)` was; it is a narrower one, not a
different kind of claim. Nothing in decision 0578, decision 0584, the
chronicle, or this file said so — a `grep -i "genealog\|gap_years\|
generation length"` across all four returned zero hits — until the final
whole-branch review caught it. Not fixed by amending 0584: the campaign's
decision block (0576-0585) is exhausted, and a clarifying paragraph would
edit an in-force record in place, which this campaign has been careful never
to do elsewhere. Disclosed instead where a reader of the fact actually meets
the claim — `PARENT_OF`'s own doc comment in `domains/person/src/lib.rs` —
and in the chronicle.

## A property test caught a real hash collision inspection had missed

`ActHandle` folds four constituents (`actor`, `deed`, `patient`, `day`)
through four *separate* `mix` steps — structurally closing the class of
hazard `ancestor()`'s own doc names (a fixed permutation iterated has fixed
points). An early draft still collided: it folded a patient-presence tag
directly against a raw `EntityId` (`mix(1, entity.get())`), and `mix`'s
first step is a bare XOR, so `mix(a, a) == 0` for *any* `a` — `EntityId::
new(1)`, the smallest legal id, collided with the literal tag `1` on the
first property-sweep run, not on inspection. `ancestor()`'s own doc already
names exactly this hazard class as something to check empirically even when
avoided structurally; the property sweep is what actually caught the
recurrence. Fixed by folding each presence tag against the already-avalanched
accumulator instead of a raw id.

## Two vacuous tests shipped, both caught by mutation

- A stream-draw guard (Task 5's `kinship_resolution_draws_no_stream`)
  compared two live builds of the *same* code to each other — a tautology
  under determinism, incapable of going red for the class of defect it
  claimed to catch. Confirmed by inserting a real, value-affecting stream
  draw into the kinship pass: the old test stayed green throughout; a new
  test comparing against an *independent* pre-task baseline fixture caught
  it immediately.
- A witness test (Task 4) would have passed with zero relations staged, had
  `witness_binds` shipped as a table-membership check rather than a real
  binding check — confirmed by mutating `witness_stages` to check only that
  a tableau existed for a situation id, never that it actually staged: three
  tests went genuinely red, proving they had been exercising the real
  machinery rather than a name.

Neither was found by review reading the assertion; both were found by
corrupting the implementation and watching the test fail to notice.

## A claim relayed with reasoning attached is still unverified

The controller told Task 5's implementer that `parent-of` would be the
workspace's first `functional: true` relation — stated with a plausible
reason attached (a new kind of guard exercise), inherited from an earlier
Task 2 review comment, and never checked against the tree. It was false (see
finding 3 above), and it became a false sentence in a ratified decision
record before anyone ran `grep functional.*true` against
`domains/history/src/lib.rs`. A claim arriving with a reason attached is more
persuasive than one arriving bare, and persuasiveness is not evidence.

## Nothing checks that a ledger entry cited by a decision exists

`cli/tests/suite/docs_consistency.rs::decision_cites_in_sources_resolve`
validates that a decision citation *inside source code* resolves to a real
decision file. It does not run in the other direction: nothing validates
that a ledger-entry citation *inside a decision record* ("see ledger entries
#8 and #9") resolves to a ledger entry that exists. Decision 0577 and 0581
both cited "ledger entries #8 and #9" before entry #9 had been written — the
citation was authored on the assumption a ledger entry would land in the
same commit, and it did not, silently, until a later self-review noticed and
wrote the entry in place rather than back-dating it. This is a real gap, not
fixed by this campaign: a citation from a durable record into a per-campaign
scratch-adjacent ledger has no drift check in either direction.

## Main is red, and it is not this campaign's

Two `repertory_corpus` tests fail on pristine `origin/main`:
`no_scene_has_fallen_below_its_recorded_floor` and
`every_founding_scene_passes_every_beat`, both on `walk-changes-the-room` —
one of four positive controls for that instrument. Confirmed in a disposable
detached worktree at `origin/main`, not on this branch, and confirmed
independently at two different points in this campaign (Task 6's report and
Task 8's own check): the same two tests, the same failure, unrelated to
anything either task touched. It is invisible to the everyday gate because
`repertory_corpus` is not in the sub-floor tier — `make gate-commit` reads
1010/1010 green while these two fail — and decision 0125 removed CI, so
nothing runs the full workspace suite against `main` on a schedule. Already
posted to the board. Not fixed here: a failed positive control on an
instrument this campaign does not own deserves its own investigation, not a
drive-by repair from a campaign passing through.

## What was deferred, and the reason recorded

**Person-scale affect** (`feels-toward`) — `windows/sentiment::snap_judgment`
is people-to-people; Polti's `feels-toward` is person-to-person. Nathan ruled
at brainstorm that these are two different predicates, and only the
people-scale grain ships. Registering `feels-toward` against the people-scale
producer would move the trope number on a grain the corpus never asked for —
the exact mis-scoring decision 0136 exists to prevent — so
`bundle:felt-affect` stays at 2 of 3 on purpose. Recorded as an idea-registry
row (`PSY-affect-grain-is-what-it-points-at`).

**Combat** (`bundle:interpersonal-violence`) — no attack/strike/wound
machinery exists anywhere in `windows/vessel`; building it was explicitly out
of scope (spec §6).

**Entity-carrying scene marks** — `hornvale_scene::Mark` carries no entity
id, so `present_at`/`witnessed` need a caller-supplied `&[EntityId]` pool
rather than reading identity off the chart directly; `anyone_present` (the
chart-only read) can answer *whether* someone is present, never *who*.
Recorded as `SCN-marks-carry-no-entity-id`.

## Followups

- **A stale module doc can survive being cited by name across three
  documents.** `kernel/src/manifest.rs`'s "nothing constructs a `Manifest`
  yet" was false by the time this campaign's own spec quoted it, and nothing
  caught the citation chain until this task's own close. Fixed at the
  source; worth asking, generally, whether any other module doc this
  project cites by quotation is equally out of date — this campaign checked
  one and found it wrong.
- **The tone tier is entirely at its default.** Not this campaign's to fix;
  recorded so the next campaign that touches `windows/sentiment` or the
  concept registry's tone axis does not rediscover it from zero.
- **The `repertory_corpus` red on `main` was investigated and fixed after this
  section was first written, and the original text — *"needs an owner and an
  investigation"* — was left standing for several hours while the fix sat on
  `main`.** That is this retrospective's own subject happening to this
  retrospective: a committed record asserting something the code had stopped
  saying. Recorded rather than quietly rewritten, because the interval is the
  finding. What it actually was: not a world regression and not a red `main`,
  but a **race in the test harness**. `run_at` named its scratch directory
  `hv-repertory-<pid>-<seed>`, and the two tests that both iterate
  `the-founding.scene.json` — `every_founding_scene_passes_every_beat` directly
  and `no_scene_has_fallen_below_its_recorded_floor` through
  `every_committed_scene` — run concurrently in one process, so at a shared seed
  they wrote the same `script.txt` and read back the same `snapshot.json`. Each
  received the other's run; the scene failed a beat it passes alone and scored
  `Absent`, which the floor test reports as a regression that never happened.

  **Why no gate ever saw it, which is the durable part:** this project gates
  with nextest, which is **process-per-test**, so every test holds its own pid
  and those paths cannot collide there. Only libtest's default — threads inside
  one process — reproduces it. Measured at commit `4400e3081`, one tree,
  concurrency the only variable: `--test-threads=1` gives 11 passed in 138.3 s;
  the default gives 9 passed, 2 failed in 12.9 s. **The failing run is faster,
  because the scene tests abort early instead of running their scenes**, and
  that inversion is the tell.

  Fixed by naming the scratch directory per *call* through an atomic counter,
  with `a_scratch_name_is_unique_per_call_even_at_one_seed` demonstrated red
  against the restored `(pid, seed)` scheme before being accepted.

  **The controller error worth keeping is separate from the bug.** A red was
  reported to the board from a *single* observation, with an unbisected cause
  attached (The Pavement's walk band) that four green probes later disproved.
  The truth only surfaced on re-running the exact commit that had failed, which
  passed. A symptom this convincing — a positive control reporting `ABSENT`
  under an assertion whose own message says a red means the world changed —
  deserves a second run before it deserves a diagnosis.
- **A decision-to-ledger citation has no drift check.** `docs_consistency`
  checks source-to-decision citations; a decision-to-ledger-entry citation
  (like 0577's and 0581's own "see ledger entries #8 and #9") can point at
  nothing and nothing will say so.
