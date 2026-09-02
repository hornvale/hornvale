# Campaign The Wicket — retrospective

**Merged:** 2026-09-01

## The headline: EIGHTEEN instances of ONE shape

Every review round of this campaign found the same defect wearing different
clothes: **a statement whose form outran what it could support.** Not vagueness
— the opposite. The statements were specific, and specificity is what made them
convincing.

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
