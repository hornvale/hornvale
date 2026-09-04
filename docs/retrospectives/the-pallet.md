# Campaign The Pallet — retrospective

**Merged:** 2026-09-03 · **Ledger:**
`docs/superpowers/ledgers/2026-09-03-the-pallet.md` (19 entries) ·
**Decisions:** 0696, 0697, 0698

Process lessons only. The product — the local-day conversion of the sleep path,
`select_sleep_site`, the `slept-on` predicate, and a 39-row species table with
seven distinct values — is in the chronicle.

Shape, stated first because it prices everything below: **five tasks, four of
them code, three with fix rounds** (Task 1 needed two). Every one of the four
lessons here is a failure, and three of the four were found by somebody other
than the person who made them.

## 1. An error is loud only if its callers propagate it

I verified `Ledger::commit`'s **signature**, saw it returns a `Result`, and
wrote this into an implementer's brief as a reassurance:

> an unregistered predicate is not silent … a scenario that commits `SLEPT_ON`
> without registering it **fails loudly at the point of commit**. That makes
> this enumeration self-checking.

It is false. `Ledger::commit` does return the error; two callers throw it away.
`windows/lab/src/health.rs:162` and `:278`, both `run_simulation` variants, are
`Err(_) => break` — any commit failure silently truncates the run. The
implementer found it by turning both arms into `panic!()` and re-running, which
is a positive control on the **error path** and is not a move I had thought to
ask for. Two `hearth_population_calibration` tests had been passing on
simulations that stopped at day 1.

Three things worth separating:

- **The error was in kind, not degree.** A `Result` says an error is
  *available*, never that anyone reads it. The check I skipped costs one grep:
  `Err(_) =>` appears **16** times under `windows/` and `domains/`, and
  `Err(_) => break` in two files.
- **A correction is unaudited text.** This was not plan prose — it was my
  *correction* of plan prose, written to replace a framing I had judged poor. It
  was more confident than the thing it replaced ("self-checking") and carried no
  command-and-output pair, which is precisely what a standing memory entry and
  the autopilot skill both require for a sentence of this class.
- **A signature is a claim about one function; loudness is a claim about a
  graph.** Verify the CALLER, never the signature.

## 2. A replacement test can cover a different BRANCH than the test it replaced

Task 1's success condition was deleting a test — the running falsifier the
previous campaign had left asserting the inversion. A test deleted and a test
added in the same task reads as a *replacement*, and nothing checks that the new
one exercises the same code path.

The deleted test used `permanent_night: true`, forcing the give-up-fallback arm
— the constant the inversion had actually been measured on. Its replacement used
`permanent_night: false` plus awake-at-noon, forcing the sleep-floor arm
instead. Both are good tests, both are "about" the rest/sleep ordering at the
100-hour extreme, and they take **different arms of the same `match`**. The
ratcheted arm was unpinned and the whole suite was green.

**A test's identity is its branch, not its subject or its name.** The check is
not "did we add a test" but *mutate what the old test pinned and confirm
something still reddens*. Two contributing causes are worth carrying: almost
every fixture in this area uses a terrain reporting no day, so the constant and
the runtime formula are identical there and three of four mutations are
invisible; and a byte-golden catching a conversion is a **change detector, not a
property witness**, so it must not be counted as coverage for one.

The remedy took two fix rounds and ended better than the original: all four
converted quantities now have a dedicated property witness, and the last one
found the property the original defect statement had been pointing at all along.

## 3. A relayed measurement is an unaudited claim — to whoever publishes it

I handed an implementer a reviewer's control result (all seven table rungs set
to 4.00, fixture byte-identical) and asked for it to be folded into a module
doc. It re-ran the mutation instead, and said why:

> I'd published it as measured evidence in a durable artifact, so inheriting it
> unverified wasn't acceptable.

That is the correct instinct and it is this campaign's own subject one level up.
**A number becomes evidence under the name of whoever publishes it, not whoever
first ran it.** The same round also caught two published numbers of mine that
were wrong: a count read "four of the six species" against a roster figure of 24
where the true figure is 25 — produced by arithmetic on my own prose rather than
by counting the live table — and a byte pair printed in the order that reads as
"the golden grew" when it had shrunk. Both sat in the module doc that is the
task's most-cited artifact, beside four prior adjudications a future campaign
would pattern-match against; a reversed pair there does not merely mislead, it
teaches the wrong reading convention for every adjudication above it.

The recount was done by `awk` over the live table, on the explicit ground that
**a count re-derived by the method that generated the error reproduces the
error**.

Its sibling, from the same round: I claimed a `SleepTraits` struct avoided
taking "the workspace's FIRST `#[allow(clippy::too_many_arguments)]`". Measured:
**75 in the workspace, 16 in the file being edited.** The ruling survived
untouched because it never rested on that premise, but the false clause was left
*visible* in the ledger and in the doc rather than quietly swapped — a durable
ledger's value is to a campaign that cannot re-derive it, and a reader who took
that clause at face value would have learned this workspace forbids something it
does 75 times.

## 4. A rule can be correct and undecidable for one case

Adding a registered predicate means finding every site that builds its own
registry. The implementer's selection rule was *"does this scenario actually
commit `slept-on`"* — sound, and better than my static grep, which it beat by
catching a harness that plants built-and-cold rooms and therefore composes a
fireside bed deterministically.

Review found a fourth site the rule could not decide. `simulate_world`
(`windows/lab/src/health.rs:307`) takes `world: &World` and comments that it is
"same as `Session::start`", registering five predicates without this one. Its
answer is **a function of the world**, not a static fact about the call site: no
amount of reading it settles the question, only quantifying over worlds does.

It does not fire today, and this campaign **armed** it — before Task 3 there was
no predicate to omit. Compounding lesson 1: the omission would have been
invisible, because the swallowed error turns a truncated run into a fast green
one. With the registration removed, the two affected tests pass in 0.019 s and
0.050 s against a committed 0.053 s and 0.144 s. A ~3x ratio on a 50-millisecond
test is not something anyone notices.

**When a selection rule is a predicate over call sites, ask whether any site's
answer depends on runtime data.** Those are the ones a static sweep and a
careful reading both miss, and they are exactly where a static rule feels
safest.

## 5. Two scripted dedup attempts died on their own assertions, and that is the good news

The 380-commit absorption produced five textual conflicts, one semantic conflict
git merged cleanly (a 2-tuple that became a 3-tuple, in a different hunk — it
compiled nowhere, which is the loud end of that failure mode), and **three
duplicated idea-registry rows**. Both sides inserted at the same place, so git
kept both blocks and raised nothing; `docs_consistency::registry_ids_are_unique`
caught it.

The resolution was **not** a dedup. Main's copy of each row was the better one,
in three different ways: one had gained a remedy pointer, one had been refuted
outright (the defect we filed was already fixed), and one had moved from `raw`
to `shipped` with a measurement attached. Two scripted attempts to dedup by
equality died on their own assertions before writing anything — **which is the
only reason our three inferior copies did not overwrite them.**

Two things generalize. First, an aggregate-shaped file (the registry is one)
must never be resolved by a script that assumes duplicates are identical;
identity is the assumption that fails, silently, in the direction of whoever ran
the script last. Second, **assert before writing**: both scripts failed loudly
because the assertion preceded the mutation, and a script that had written first
and checked after would have destroyed three better rows and reported success.

The absorption cadence itself is the honest miss: main was absorbed **once**, at
the Task 4 boundary, at 380 commits. That is not a stage-boundary cadence.

## Deferred minors, and where each landed

`closing-a-campaign` step 2B: *"it's in the ledger" is not a location.* The
ledger carries exactly **one** entry under an explicit "Deferred minor" heading;
the rest were raised in review and routed inside their own fix round, which is
the healthy case. All of them, with outcome and location:

| Minor | Finding | Outcome |
|---|---|---|
| #16 | Spec §4d asks for the two unbuilt rungs as tagged seams; neither has a constant to hang a tag on — both are the absence of a *number* | **Doc, not tag.** Declared in `SiteGrade`'s own doc (`windows/vessel/src/liveness.rs`), which is where a campaign building either one starts. **Consequence to know:** they do NOT appear in `docs/audits/plumb-roster.md`'s Fidelity table, so that table is not where a future reader will find them |
| #8 F2 | `SCAN_LIMIT` and `ONE_DAY` were read by nothing — constants whose sole function was to hold a tag | **Fixed**, Task 1 fix round 1. Promoted to file level beside the other three anchors and read by the two new tests |
| #8 F4 | Latent non-termination: `local_day / 20` is `0` for `day_ticks()` in `1..=19`, and a zero step never advances the scan | **Fixed**, Task 1 fix round 1. `.max(1)` with the invariant stated inline; the old fixed step made this structurally impossible and the conversion made it only situationally so |
| #8 F5 | Three stale doc claims about the retired constants | **Fixed**, Task 1 fix round 1 |
| #10 | Seven assertion strings carried literal multi-space runs — a non-raw Python heredoc ate the Rust `\`-continuations. Compiles, formats, passes; only reading the string shows it | **Fixed**, Task 1 fix round 2. The exact trap this project's memory already names, hit anyway |
| #12 | `Err(_) => break` in both `run_simulation` variants | **Idea-registry row**, `TOOL-lab-run-simulation-swallows-commit-errors`. Deliberately not fixed: changing an error policy in the lab's simulation loop is its own campaign with its own question (fail fast, or continue with the truncation surfaced?) |
| #13 | The spec asked for `place: Some(room)` and rooms have no `EntityId` | **Ruled and fixed**, Task 3 fix round 1: `place: None`, with the reasoning at the constant. Now [decision 0698](../decisions/0698-a-kind-is-committable-where-an-anchor-identity-is-not.md) |
| #14 | Fourth registration site (`simulate_world`) | **Fixed**, Task 3 fix round 1 |
| #8 F3 | `tools/plumb`'s walk cannot see a `let` binding, and this campaign moved three live numbers into that blind spot | **Idea-registry row**, `TOOL-plumb-walk-blind-to-let-bindings`, `raw`, with both probe results. Tool untouched: auditing every integer literal in a function body has an obvious false-positive problem |
| #17/#19 | The seven-rung derivation rule lives in prose and nothing ties a row to the traits it cites; the tonne threshold lands on `giant-crocodile`'s exact mass. `DEFAULT_SLEEP_GRADE` is documented as human's row and nothing pins the equality | **Recorded, not converted**, in [decision 0697](../decisions/0697-what-an-afforded-site-is-worth-is-a-property-of-the-sleeper.md)'s consequences. Both are shared with `fatigue_rise_registry`, which the brief instructed this task to follow, so closing one without the other would be the worse outcome |

## What went right, briefly, because it is repeatable

**Naming a PROPERTY beat prescribing a CONSTRUCTION, three times on one task.** I
gave "the two step sizes must bracket a transition" and the implementer found
the crepuscular band; I gave "the two bounds must disagree about whether the scan
finds anything" and it found the fast-world give-up. A plan author does not know
which fixture discriminates; the implementer does, after reading.

**Checking the tree before designing halved the campaign.** Grepping the sleep
path before writing a line of spec found that the previous campaign had already
built the offered verb, the two-valued grade, the multiplier and the bracket. The
campaign went from nine tasks to five, and the three genuinely missing things
were nameable in one sentence each.

**A vacuous red-phase is a process smell; a vacuous test is a defect.** Task 2's
implementer reported that its red phase was partly vacuous — the stub returned
`None` unconditionally, so the test asserting `None` passed for the wrong reason.
Writing that down rather than reporting "red-first confirmed, 3 tests" is what
let review ask the sharper question, which is whether the case is non-vacuous
against the *shipped* code. It is.

## Carry forward

- **Verify the CALLER, not the signature.** A `Result` is an availability claim.
  A positive control on the error path — make the error happen, confirm
  something says so — is the branch-shaped form of the control an empty diff
  needs.
- **A test's identity is its branch.** When a task deletes a test and adds one,
  mutate what the deleted test pinned and confirm something reddens.
- **Re-run a relayed measurement before publishing it under your own name**, and
  re-derive a disputed count by a method other than the one that produced it.
- **Ask whether a selection rule over call sites has a case whose answer depends
  on runtime data.** That case is invisible to both a grep and a careful read.
- **Assert before writing, in any script that resolves an aggregate file.** Two
  scripts that failed loudly saved three better rows here.
