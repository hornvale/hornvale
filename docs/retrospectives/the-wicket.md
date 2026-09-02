# Campaign The Wicket — retrospective

**Merged:** 2026-09-01

## The headline: twelve-plus instances of ONE shape

Every review round of this campaign found the same defect wearing different
clothes: **a statement whose form outran what it could support.** Not vagueness
— the opposite. The statements were specific, and specificity is what made them
convincing.

1. `SupportsRest` advertised a gate `Session::sleep` has never consulted, and
   three doc comments said so.
2. Two guards (`no_verb_by_object_table_exists`,
   `no_hardcoded_anchor_kind_gates_warm`) searched source text for
   `AnchorKind::` — a literal the re-key makes impossible — so both would have
   read green in every possible tree forever.
3. A doc sweep could not see `tableau.rs:38`'s "the vocabulary of things is
   currently a closed enum", because the sentence names the concept without
   naming the identifier the grep was written against.
4. A vacuity guard that passes vacuously: `assert!(checked >= INVENTORY.len())`
   is satisfied by `0 >= 0`, while its message asserts it cannot be.
5. Past-tensing a false claim launders it. "`key` and `cave-mouth` **were** the
   first rows with no anchor-kind variant" is grammatically past and still
   factually wrong — `key` got a variant four tasks after the sentence was
   written. Two reviewers read the line and neither flagged it, because a
   reader audits claims about *now* and accepts claims about *then*.
6. Three of one task's property tests were vacuous on first writing.
7. An assertion that cannot fail: both sides were one-line calls to the same
   function, and the test's own message admitted it — which makes it honest and
   still misleading.
8. A site count off by four: "changed at all three registration sites" where
   there are seven.
9. `SLEEP_BOUT`'s doc said the floor "never binds" on the ordinary path and
   named a committed golden as evidence. Instrumenting that golden found it
   binding in **8 of 18** sleeps. The fixture cited as proof was the disproof.
10. A named mitigation that could not fire: the report and a bench's module doc
    both pointed at `session_length_scaling`'s `fatigue_us` column as where an
    `O(trail)` regression would show, and that probe was passing `sites: None`,
    so the trail merge never ran under it.
11. A fixture whose fallback and whose subject answer the same thing pins
    neither: every body's `home` equalled the room it slept in, so the whole
    position-trail merge could be deleted green.
12. The campaign's own cost headline — "no enum edit, no match arm, no macro,
    no dispatcher edit" — was true of the grammar path and too strong overall.

**Roughly a third originated in my plan text**, including instances 4 and 12.

### The operational refinement, in an implementer's words

> *"three of my four had honest, specific messages describing a check they were
> not performing, and none was found by re-reading."*

That is the transferable part. **Re-reading a check compares it against the
model that produced it, so it cannot see the gap; only running something can.**
Every one of the instances above was found by running — a mutation, an
instrumented golden, a live probe, a census — and none by looking harder. A
precise failure message is evidence about the author's intent and evidence
about nothing else.

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
