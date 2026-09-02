# Campaign The Wicket — retrospective

**Merged:** 2026-09-01

## The headline: TWENTY-THREE instances of ONE shape

Every review round of this campaign found the same defect wearing different
clothes: **a statement whose form outran what it could support.** Not vagueness
— the opposite. The statements were specific, and specificity is what made them
convincing.

The numbered list below is **1-18**, the campaign's own rounds. The final
whole-branch review added **19-23** and a mechanism the eighteen do not
contain; they have their own section after this one rather than being
appended here, because four of the five sit in blocks this campaign edited in
the same pass and that is the finding, not the count.

The ledger entry is cited beside each one, because the retrospective and the
ledger number these differently and a record nobody can cross-walk is a record
nobody checks.

1. (#6) `SupportsRest` advertised a gate `Session::sleep` has never consulted,
   and three doc comments said so.
2. (#17) Two guards (`no_verb_by_object_table_exists`,
   `no_hardcoded_anchor_kind_gates_warm`) searched source text for
   `AnchorKind::` — a literal the re-key makes impossible — so both would have
   read green in every possible tree forever.
3. (#19; the sweep is **plan text**, #11) A doc sweep could not see
   `tableau.rs:38`'s "the vocabulary of things is currently a closed enum",
   because the sentence names the concept without naming the identifier the
   grep was written against.
4. (#23; **plan text** — the plan's own literal code) A vacuity guard that
   passes vacuously: `assert!(checked >= INVENTORY.len())` is satisfied by
   `0 >= 0`, while its message asserts it cannot be.
5. (#29) Past-tensing a false claim launders it. "`key` and `cave-mouth`
   **were** the first rows with no anchor-kind variant" is grammatically past
   and still factually wrong — `key` got a variant four tasks after the
   sentence was written. Two reviewers read the line and neither flagged it,
   because a reader audits claims about *now* and accepts claims about *then*.
6. (#32) Three of one task's property tests were vacuous on first writing.
7. (#32) An assertion that cannot fail: both sides were one-line calls to the
   same function, and the test's own message admitted it — which makes it
   honest and still misleading.
8. (#32) A site count off by four: "changed at all three registration sites"
   where there are seven.
9. (#37) `SLEEP_BOUT`'s doc said the floor "never binds" on the ordinary path
   and named a committed golden as evidence. Instrumenting that golden found it
   binding in **8 of 18** sleeps. The fixture cited as proof was the disproof.
10. (#50; **the controller's**, in a task report and a module doc) A named
    mitigation that could not fire: both pointed at `session_length_scaling`'s
    `fatigue_us` column as where an `O(trail)` regression would show, and that
    probe was passing `sites: None`, so the trail merge never ran under it.
11. (#45, #48) A fixture whose fallback and whose subject answer the same thing
    pins neither: every body's `home` equalled the room it slept in, so the
    whole position-trail merge could be deleted green.
12. (#27; **the spec's** cost headline, which is the controller's) "No enum
    edit, no match arm, no macro, no dispatcher edit" — true of the grammar
    path and too strong overall.

**The Definition-of-Done sweep then produced six more, which is itself the
finding.** A closing document is written fast, from notes, about work already
believed correct — the exact conditions this shape needs.

13. (#58) `SLEPT_PROVENANCE`'s doc still said `sleep` "commits its `rested`
    fact", after Task 8 moved the method onto `slept`. Found by me, in the
    sweep.
14. (#58) `sleeping_needs_no_bed`'s doc said the same, and additionally that
    the method "sets `wake_at`" unconditionally, which Task 8 made conditional.
    Found by me, in the sweep.
15. (#60) **The sharpest instance in the campaign.** `Pattern::at_locale`'s
    FIELD doc still said a pattern with `at_locale: false` "cannot move a
    world: no live read can reach it" — the exact claim ledger #25/#28
    corrected on the `INVENTORY` doc **thirty lines below it**, and falsified
    by this campaign's own proof kind: `the-brazier` is `at_locale: false` and
    renders in a committed gallery transcript. We corrected the sentence we
    grepped for and left its twin one screen away. **The lesson this campaign
    wrote down — grep the claim, not the identifier — is exactly what would
    have caught it, and we did not apply it to our own correction.** Found by
    review.
16. (#58, #60) `affordance.rs`'s sweep doc said `EVERY_HANDLE` and
    `THING_KINDS` "name the same **16** kinds". Both hold 17. True when Task 3
    wrote it; Task 5 falsified it by appending `brazier` to both lists — and
    because the two lists still *agree*, the claim survived and only its number
    died, which is why nothing looked wrong. Found by review.
17. (#61) **In a decision record, about this very failure mode.** Decisions
    0556 and 0557 both cited "58 occurrences" of a bare `KindId("` literal
    across "two authoring tables". Re-measured at close: **78**, across
    **three** — Task 4 took `chamber_prose_registry` from 1 occurrence to 18
    when it made prose a component table. A Task-3-era census was transcribed
    into a closing document without re-measuring. The ruling never depended on
    the number; only the evidence was wrong, and decision records are
    append-only, so this was fixable then and not afterwards. Found by review.
18. (#62) Compressing a chain produced a false code citation.
    `MAP-wilderness-affords-no-rest`'s rewrite said the heat carriers are
    confined by "`the-fire` requires `Alcove`, `roles: [Hearthroom]`".
    `the-fire` is `roles: EVERY_ROLE`, under a comment written to prevent
    exactly that reading; the role gate is on `the-alcove`. **The version I
    replaced stated the three-link chain correctly** — the rewrite compressed
    it to two links and made it false, which is the known hazard that
    compressing a branch table deletes a branch, arriving in prose. Found by
    review.

### Attribution, stated against the ledger rather than estimated

An earlier draft of this page said "roughly a third originated in my plan
text, including instances 4 and 12", which was an estimate dressed as a count —
in the document about forms outrunning support.

The ledger's own explicit tallies: **two of the first four** (#23) and **two of
the first eight** (#32) are the controller's, in plan text — instances **3**
and **4**. #50 names a third, instance **10**, authored by the controller in a
task report and a module doc. **Three of twelve is a quarter**, and that is the
number the ledger supports.

**A quarter is the floor, not the figure.** Instance 12 is the *spec's* cost
headline, and the spec is the controller's document, though no ledger entry
tallies it that way — count it and it is a third. Of the six DoD instances, 13
and 14 are the controller's and were self-found; 17 and 18 are the controller's
and were review-found. So the honest statement is a range with its two
endpoints named, and the reason it is a range is that "mine" was never defined
before it was counted.

### The operational refinement, in an implementer's words

> *"three of my four had honest, specific messages describing a check they were
> not performing, and none was found by re-reading."*

That is the transferable part. **Re-reading a check compares it against the
model that produced it, so it cannot see the gap.** A precise failure message
is evidence about the author's intent and evidence about nothing else.

**What DOES find it splits in two, and an earlier draft of this paragraph
collapsed them.** It said "every one of the instances above was found by
running", which was true of the twelve it was written against and false the
moment six more were added — the same shape, in the sentence describing the
shape, which is why it is corrected here rather than quietly.

- **Running** found 1-12 and 17: a mutation, an instrumented golden, a live
  probe, a census, a re-measured `git grep`. Every one is a check or a number
  whose truth is a fact about execution.
- **Reading a CLAIM against the thing it names** found 13-16 and 18. These are
  not checks; they are sentences, and nothing executes a sentence. What finds
  them is not looking harder at the sentence — that is the failure mode above —
  but taking the claim and going to the code it is about, which is the
  campaign's own rule: **grep the claim, not the identifier.** Instance 15 is
  the proof, because it is the one we had the rule for and did not apply.

Neither half is "read more carefully". Both are *go and check against
something outside the text*, and the only difference is whether the something
runs.

## The whole-branch review found five more, and a MECHANISM the eighteen did not contain

The count above is the campaign's own rounds. A final review of the branch as
one object added instances **19-23** (ledger #64, #65), and four of the five
sit in doc blocks this campaign had edited *in the same pass*. All five were
found by reading a claim against the thing it names, not by running — which
strengthens the split above rather than complicating it.

19-20 are the new mechanism, and it is worth naming separately because the
eighteen do not contain it. The other instances are claims that **decayed**
(true when written, falsified later) or were **never true**. These are neither:

> **An amendment changed the code, updated one paragraph of a doc block, and
> left the block's HEADLINE stating the design it had just superseded.**

Concretely: Task 9 deleted `const FATIGUE_RISE` from `windows/vessel/src/
liveness.rs` and left its sixteen-line doc comment standing with no separator,
so that comment became the head of **`FATIGUE_FALL`'s** doc. Three readings
followed from one deletion. `FATIGUE_FALL`'s rustdoc summary documented the
*rise* rate. The block restated a lookup-miss convention (`0.0`, never accrues)
that review had already **inverted** — the code reads
`unwrap_or(DEFAULT_FATIGUE_RISE)`, and that constant's own doc, a thousand
lines below, says in as many words that `0.0` was judged inverted. And it
asserted the fall terms deliberately stay on the STANDARD day, twelve lines
above the paragraph in the same block saying fix round 1 converted them.

Why it is the highest-consequence finding on the branch: `FATIGUE_FALL` is the
constant a future tuner opens, and the actionable misreading is a **double
conversion**. It happened in the campaign's most-reviewed file, and it survived
a fix round, a stage gate and a definition-of-done sweep, because every one of
those audits was pointed at what the amendment *changed* rather than at what
the amendment *orphaned*.

The transferable move is narrow enough to do every time: **when a deletion
leaves a doc comment behind, the next item is not the next task — it is
re-reading the whole block that comment now heads, from its summary line
down.** A doc block has a headline and a body, and a diff shows you the body.

The other three: `RadiatesHeat` "has exactly one mechanically-supported
carrier", falsified 45 lines below by the `brazier` row this campaign added
(21); "seven of the **fourteen** kinds a room's grammar can place", a stale
count carried through a deliberate rewrite of that exact sentence and instance
16's twin in the same file (22); and a verb-dispatch comment reading "no new
concept, no new cost dial, no new predicate" two lines above the arm Task 8
repointed to `Action::Sleep`, where `SLEPT` is a new predicate (23). Ledger #65
adds a fourth kind of error the list has no other example of — a weakness
**bounded in the wrong direction**: `REST_BOUT`'s registered follow-up named a
fast-rotating world, and the arithmetic breaks on a slow one (`L > 1.25` std
days), because the follow-up borrowed the direction of the bug it sat beside.

## What this campaign's gates do NOT run today

Worth a reader knowing before trusting a local green: **none of this
campaign's own gates is in `docs/timings/subfloor-roster.tsv`** — zero
`kind_totality::*` (G-a..G-f), zero `fatigue_stock::*`, and two of five
`action_module::*` — so `make gate-commit` compiles this work and runs almost
none of it. That is by design (a test with no recorded baseline duration is
excluded) and it self-heals when the chamber's `gate` phase rewrites the roster
on the merge run, but between now and then the campaign's deliverable is
invisible to the gate a developer actually types (ledger #66).

## The answer was already committed, four times

Four design questions this campaign asked had answers sitting in the repository
before it started, and in three of the four the evidence was in a file the
campaign was already editing.

- **The 48-seed role sweep.** A comment in `interior/pattern.rs` recorded that
  `Role::Loomroom` is chamber index 2 in 24 of 24 structures and `Role::Shrine`
  in zero, and drew the general conclusion in so many words. The campaign put
  its proof kind in a shrine anyway, and two implementers plus a reviewer read
  that file before anyone connected the two.
- **`INVENTORY`'s latency rule**, whose own doc stated the epoch conditions the
  brazier append had to satisfy.
- **The accession-cohort rule**, which nothing predicted and running found.
- **`ticks_per_local_day`'s tidal-lock convention**, already settled.

The standing rule is *grep before proposing*, and it failed here in its
sharpest form: the evidence was not in a decision record or a retrospective but
in a code comment beside the table being edited.

## A criterion supplied from outside the code needed correction three times

Twice by me, once by a reviewer.

- I told Task 9's implementer that a species with no sleep-debt row does not
  sleep, so the "(for most species) mandatory" exception would fall out for
  free. **The codebase said otherwise** — seven neighbouring species traits all
  fall back to a documented neutral — and the trap fired inside the task, where
  a fixture's `species: "test"` silently became rate 0.0 and broke a walk test
  with nothing reporting a bad species.
- A review prescribed the `FATIGUE_FALL` guard as "a saturated body drops below
  `FATIGUE_ACT` in 3-4 cycles". The implementer checked and found it does not
  discriminate — 0.6, 1.0 and 1.6 all pass — and substituted a criterion that
  reddens both ways.
- A review suggested gating the sleep floor on `is_awake`; the implementer
  reproduced the measurement first, then found the suggestion incomplete.

`campaign-autopilot`'s rule — *never prescribe a specific mutation from outside
the code; name the property and let the implementer find one* — binds **review
instructions and controller messages**, not only plan text. That extension is
the durable finding.

## Estimate deltas

- **The campaign was scoped ~2.3x too large, from a published measurement.**
  The frontier essay said thirty-four variants and thirty-six exhaustive match
  sites; there were 15 and 3, and had been on the day the essay was written.
  The plan was built against the advertisement.
- **Two stage gates, both green**, at 1325 s and 1386 s. The pairing earned its
  cost: the second tested stage 5 *plus* a 63-commit absorption including a
  full lattice rewrite, and had it reddened, the first gate's green would have
  narrowed the candidate causes from ten tasks to two.
- **A ratchet enumeration derived from static reading is a floor, never a
  total.** Six were listed; running found two more, one of which panics every
  world genesis.

## Spec vs. reality

- **A branch table is not portable between tasks with opposite premises.** The
  STOP-on-gallery rule was written for a task whose premise is *nothing
  changes* and carried into a task whose premise is *exactly one thing
  changes*. An implementer correctly stopped on the line that was the
  campaign's deliverable.
- **A branch table is a RULE, not an enumeration.** Three outcomes were listed
  and the world had a fourth. The question that decides it is *world-derived or
  source-derived*, which covers the fifth case too.
- **Fixing the defect made the metric worse, and the metric had been flattered
  by the defect.** `tick_commit_budget` rose from 1.008 to 1.058 when the nap
  fragmentation was fixed, because an oversleeping body commits nothing. A
  budget improved by a defect reads exactly like a budget improved by a fix,
  and nothing in the instrument distinguishes them.
- **When a constant's doc states a budget, the budget is already a test.**
  `REST_FALL` survived a 90x mutation range; its own doc already stated the
  derivation as a checkable inequality, and writing that inequality down as a
  `const _: () = assert!(...)` made a bad value fail to *compile*.
- **A correction is unaudited text, observed in real time.** Ledger #25 was
  itself a correction, written confidently, and introduced a false mechanism
  while fixing a true staleness. The date was right and I stopped checking.

## Do differently next time

- **Run something.** Where a check's guarantee matters, mutate it. Re-reading
  is structurally incapable of finding the shape this campaign is made of.
- **Grep the CLAIM, not the identifier — and apply it to your own
  corrections.** The campaign wrote this rule down at instance 3 and then broke
  it at instance 15, in the same file as the correction, thirty lines apart.
  When a false sentence is fixed, the next move is not the next task: it is
  grepping for the *proposition* that sentence made, because a twin written by
  the same author in the same file will not contain the identifier that led you
  there.
- **When a deletion orphans a doc comment, re-read the block it now heads —
  from the summary line, not from the diff.** Instances 19-20: deleting
  `const FATIGUE_RISE` left its sixteen-line doc attached to the constant
  below it, and every audit afterwards looked at what the amendment changed
  rather than at what it orphaned. A diff shows a block's body; a doc block's
  damage is in its headline.
- **Re-measure every number a closing document inherits.** Instance 17 is a
  Task-3-era census transcribed into an append-only decision record without
  re-running the command. The command was in the ledger beside the number and
  takes one second. A number carried forward is a claim with a date on it, and
  a decision record is the worst place to discover that.
- **Do not compress a chain of citations.** Instance 18 turned a correct
  three-link grammar chain into a false two-link one while shortening a row to
  fit a character cap. If a citation will not fit, cite fewer links **and say
  so**, or point at the file; never merge two links and keep both their labels.
- **Ask what the simplest implementation my fixture cannot tell from the real
  one is.** A mutation survey that only asks *does the feature's own mutation
  redden* misses the case where the fixture's fallback and its subject agree.
- **Before writing a probe to establish reachability, look for the committed
  census column.** A standing measurement at n=1000 outlives the task; a
  throwaway at n=5 does not, and this campaign wrote the throwaway first.
- **Past-tense a claim only after checking it was ever true.** The past-tense
  rule is good and its failure mode is that a false sentence starts reading as
  settled history.
- **Do not run `git add -A` in a checkout a helper is working in.** It staged
  an unfinished file into a docs-only commit and then gated against it. Commit
  with an explicit pathspec whenever anything else is live.
- **Run `docs_consistency` after any docs or registry edit.** The pre-commit
  hook skips the commit gate when no Rust-relevant path is staged, while
  `docs_consistency` and `generated_paths` read exactly those files. This was
  in memory and was not applied, and the cost fell on an implementer who had to
  prove a red was not its own.

## Deferred minors, and where each landed

Step 2B of `closing-a-campaign` is explicit that a minor sitting only in the
ledger is not routed — *"it's in the ledger" is not a location*. Every deferred
minor this campaign recorded, with its outcome and the commit that carries it:

| Deferred at | Finding | Outcome |
|---|---|---|
| Task 2 review | `the_dispatch_scan_catches_an_anchor_kind_keyed_table` named a type that no longer exists; deferred because renaming edits `subfloor-roster.tsv`, which selects by exact name | **Taken** in the final fix wave — renamed `the_dispatch_scan_walks_a_multi_line_signature`, roster row `subfloor-roster.tsv:2994` updated, test re-run by name. `b10c7b8d7` |
| Task 6 review | `affordance.rs`'s third correction "lost the em-dashes its neighbours use" | **Declined**, ledger #67. The named paragraph carries three em-dashes, so the finding targets a different passage or no longer applies. Inventing a target for a cosmetic finding *in this campaign* would itself have been an instance |
| Task 7 review | Task 8 must land before pre-merge close, or the census would encode a creature dozing ~22 times over 39 days | **Discharged** — Task 8 landed (`f279c105a`) and removed the fragmentation; the census subsequently moved nothing |
| Task 9, self-flagged | `fatigue_rise_registry` had no coverage ratchet, unlike its siblings | **Fixed** in Task 9's fix round — `every_biosphere_kind_carries_a_fatigue_rise_row`, mutation-proved by deleting `xorn`'s row. `85c8cf5ac` |
| Task 9, self-flagged | `fatigue_rise_for` rebuilt a 38-node map per call | **Fixed** in the same round — borrowed-store `FatigueRiseTable`, needing no `Body` change. `85c8cf5ac` |
| Task 10 → out of scope | The people half (*what a people sleeps on*) and individual half (*this one likes a sleeping bag*) of rest quality | **Parked as registry rows** — `PSY-rest-quality-is-a-grade-not-a-gate`; they need `MAP-one-kind-model`'s additions two and three |
| Task 7 → registered | `REST_BOUT` is a fixed 0.25 std days, not converted to the local clock | **Registered follow-up**, and its *direction* corrected in the final fix wave: the repayment falls below the hysteresis band at **slow** rotation (`L > 1.25` std days), not fast, as the original note said |

Nothing was parked at a fix-loop cap; no loop reached its five-round breaker.
