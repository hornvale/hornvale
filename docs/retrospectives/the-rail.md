# The Rail — retrospective

**Merged:** 2026-08-29

## The preregistration was wrong and the instrument was right

Four numbers were frozen before any code was written. All four came out where
the *corrected* prediction said they would:

| criterion | predicted | measured |
| --- | --- | --- |
| ladder covered / frontier | 11 / 20 | **11 / 20** |
| the-merchant entries | 6 of 12 (`m05 m06 m07 m08 m09 m10`) | **6 of 12**, same set |
| the-merchant demand instances | 21 of 30 (70.0%) | **21 of 30 (70.0%)** |
| the-flood-watch | 0 of 139; 270 of 1128 (23.9%) | **0 of 139; 270 of 1128 (23.9%)** |

The word *corrected* is doing all the work, and this is the campaign's most
transferable process finding.

The preregistration originally read **9 covered / 22 frontier**. It was
computed by a script the controlling session wrote while drafting the spec —
not by the resolver the criterion would be judged against. That script built a
rung's demand set as `{introduces of each ancestor} ∪ {own introduces}`, and a
**control rung introduces nothing**, so its set contained a `null` and
`{null, …} ⊆ {strings}` is never true. All **fifteen** control rungs were
permanently excluded from *covered*, in every number the campaign predicted.

The resolver skips a null correctly. So the first task that landed a token
reported **6 covered where the plan predicted 5**, said so, and used the
resolver's answer — which is exactly what the plan told it to do, and the only
reason the defect surfaced at the first opportunity rather than at the last.

**The preregistration was not falsified by the world. It was arithmetic that
had never been run against the instrument it was predicting.** A number
computed by a second implementation of the thing under test is not a
prediction about the thing under test; it is a prediction about the second
implementation, and the two agree only by luck. The correction went into the
spec with both wrong versions named, before four of the five tokens existed —
which is the ordering that keeps it a correction rather than a retune after
unblinding.

That correction is itself the campaign's clearest instance of a documented
hazard. **The spec's coverage paragraph was wrong twice, in opposite
directions, and the second draft was written as a correction of the first.**
Draft one credited the control rungs for a count they had no part in; draft
two asserted that none of the fifteen is ever covered. Both came from the same
buggy script; neither was caught by re-reading. Writing a fix *feels* like the
careful act, and nobody audits the audit.

## Every substantive defect originated in controlling-session text — a seventh campaign

The Scarf, The Reticence, The Inquest, The Mortise, The Offer and The Stile
each recorded this distribution. It held again, and the list is long enough to
be worth itemizing with what killed each one, because **not one was found by
re-reading**.

| # | defect, in controller text | what killed it |
| --- | --- | --- |
| 1 | the 9/22 preregistration excluding all 15 control rungs | the resolver, run by Task 2 |
| 2 | the spec's coverage paragraph, wrong twice in opposite directions | the same run |
| 3 | a property valence carrying the property word in the predicate **and** object slots — the right string from a reused field, the `m02` trap one layer up | reading the brief against the code before dispatch |
| 4 | "three exhaustive `Argument` match sites" — one of the three has a pre-existing catch-all, so `Absent` was silently absorbed into a wrong surface | Task 2's reviewer reading the object-resolution path |
| 5 | a brief specifying a documented partial render where the crate's own law says render-fully-or-gap | controller ruling on Task 4's report |
| 6 | the same wrong shape carried forward into Task 5's brief from the spec | the Task 5 implementer, overriding it |
| 7 | the spec asserting *"they are a planet"* becomes sayable — crediting the campaign with a fix it does not make **and** describing a mechanism the design does not have | writing the assertion out as a line of test code during plan self-review |
| 8 | the spec requiring the parse to nominate a canonical paradigm row | the Task 6 implementer reading `Clause`, which has no person field |
| 9 | five plan steps naming a whole-workspace test command a local guard refuses, and three steps addressed to the **wrong actor** (an implementer taking the canonical box) | the Task 7 implementer, which reported it rather than working around it |
| 10 | the fix for #9 dropping a character from five redirect paths, so each step wrote one file and grepped another | the Task 8 implementer |
| 11 | a controller ledger entry attributing an embedded-number fix to the person filter | the Task 8 implementer's own mutation control |
| 12 | a controller worry that the frontier test pins only a count | the Task 5 reviewer, which read the assertion |
| 13 | a controller worry that a pin-isolation test could assert its own output | the Task 7 reviewer, which went to the kernel's `Seed::derive` |

**Number 10 deserves its own paragraph, because it is the failure class this
campaign spent nine tasks hunting, shipped into the plan by the act of
correcting the plan.** The commit fixing #9 added a guard override to five
workspace-run steps and, in the same edit, dropped the `t` from each step's
redirect path: every step wrote `/tmp/rail-Nb.log` and grepped
`/tmp/rail-tNb.log`. The grep matches nothing, prints nothing, and **reads
exactly like a clean run** — a check that runs, succeeds, and answers nothing.
It was authored mechanically, in a commit whose whole purpose was to make the
plan's commands correct.

The last three rows run the other way and are worth keeping for that reason:
in each, a reviewer or implementer **checked a controller's stated worry and
found it unfounded**, rather than agreeing with it. A worry accepted
sympathetically costs a fix round; a worry checked costs one command. Two of
the three were settled by reading code the worry was about, and the third by
reading the kernel primitive underneath it.

## Seven incomplete corrections — the class the table above hides

The table above is organized by **who authored the defect**, and that grouping
merges two classes needing different remedies. "Every substantive defect
originated in controller text" is a seventh repetition of a finding six
campaigns already recorded. The other class is rarer and is the one worth
carrying forward: **seven times this campaign, a correction introduced a fresh
error** — usually a claim rewritten in one place whose twin somewhere else was
left standing. The controlling session's ledger kept a running tally and it
reached seven; each is reconstructed here with the mechanism, because a count
without mechanisms is not actionable.

| # | the correction | what it left behind |
| --- | --- | --- |
| 1 | spec §1.1's coverage paragraph, correcting the 9/22 preregistration | credited the fifteen control rungs for a count they had no part in |
| 2 | spec §1.1 again, correcting draft one | asserted the opposite — that none of the fifteen is ever covered. Same buggy script behind both |
| 3 | Task 4's fix round for a stale-backing-list finding | rewrote the `grammar.rs` note when behaviour changed and left its twin in `sentence_corpus.rs` stale — **the same blast-radius shape as the finding it was fixing**, inside the round fixing it |
| 4 | Task 4 landing `r003` | left a neighbouring substitution count stale; the Task 5 implementer found and fixed it unprompted |
| 5 | the commit fixing plan row 10's unrunnable commands | dropped a `t` from five redirect paths — see the paragraph above |
| 6 | Task 5's fix for two stale forward references to the locative valence | a **third** one survived at `clause.rs:1677`, in the realizer arm that implements it, and was fixed only in Task 8 |
| 7 | correcting the 9/22 expectation in the plan's table and in the spec | left it in the DoD's own verification step, where it sat for eight tasks — a reader of that step alone would have scored a matching run as a MISS |

Rows 1, 2 and 5 already have their paragraphs above; rows 3, 4, 6 and 7 are
recorded here for the first time. Note their shape: **four of the seven are a
correction that fixed every instance it looked at and none it did not**, and
in two of those (3 and 6) the instance it missed was of the *same class* the
correction was about. Writing a fix *feels* like the careful act, and nobody
audits the audit.

The operational remedy is in "Do differently next time" below, and it is one
sentence long.

## Five implementer overrides, every one correct

Each was reported as a deviation rather than either obeyed silently or
departed from silently, which is the mechanism working.

1. **Task 2, about the code.** Reported 6 covered / 14 frontier against a
   predicted 5 / 15 and used the resolver's answer. This is what exposed the
   preregistration defect above.
2. **Task 5, about a design.** The brief specified a tongue's locative arm as
   "copula, like `Nominal`". It built a gap instead. The reviewer's argument
   for the override is better than the brief's: with the locative binding an
   object, a copula-only arm emits **byte-for-byte the surface the nominal
   valence produces** — *the merchant is a tree*, a classification standing
   where a location belongs. It does not say less than the clause means; it
   says something false.
3. **Task 6, about the code.** The brief demanded it "name which row is
   canonical". It reported that the question does not arise, because person
   lives on the subject. The reviewer confirmed by **tracing the
   counterfactual** rather than reading the argument sympathetically: delete
   the person narrowing and *"I are a planet."* parses successfully.
4. **Task 7, about the plan's own commands.** It reported the unrunnable
   steps, used the guard's documented override for verification only, and
   correctly did **not** run the stage gate a step told it to.
5. **Task 8, about a mutation control — the most valuable of the five.** Its
   first control for two new tests deleted the person filter, and both tests
   stayed **green**. They pin the subject-pronoun fallback, not the filter. It
   found controls that do redden, corrected the docs to say so, and thereby
   caught itself about to ship a false claim about what its own tests prove.

## Three prescribed verifications found absences rather than confirming presences

- A mutation step in Task 3 initially went **uncaught**: no test exercised the
  deep realizer's intransitive path at all. The step's value was the gap, not
  the confirmation.
- Task 9's dispatch named the `grep -c "Type {"` miscount trap explicitly —
  it produced three wrong figures in The Scarf and three more in The Mortise.
  It fired: the grep returned **32** construction sites, the compiler **25**.
  Naming it cost one sentence, and the report carried both numbers. A reviewer
  then reconciled the count backward from the post-diff grep and reproduced 32
  exactly.
- Task 1's report claimed "no entry-level coverage resolver has been written"
  for a corpus the same commit resolved 1128 demands over. The score is not
  un-writable; it is zero, for a structural reason.

## A count is not a membership, twice

Two independent instances say the same thing about this campaign's own
instruments. The frontier stayed at 16 while `r005` left it and
`present-progressive` arrived — a real fact about the graph, since
progressives are traced to locative sources. And the merchant corpus's
one-token-short set stayed at three entries while `m08` left for zero and
`m11` arrived from two. Both surfaced only because the assertions are over
**ordered vectors** rather than sizes; a compensating pair of changes passes
any assertion that checks only a count.

## Tooling findings

**The whole-workspace-test-run guard pattern-matches command TEXT wherever it
appears, including in prose about commands.** It fired three times this
session on things that were not test runs: twice on controller ledger prose
inside a heredoc, and once on an implementer, which had to reword a **commit
message** to land a commit. The workaround each time was to write the text to
a file and `cat` it in. The guard is doing real work — it caught the genuine
defect in the plan text at row 9 above — and the finding is only that its
blast radius includes documentation, which it has no way to distinguish today.

**Files written under `/tmp` vanished between tool calls.** One implementer
hit this and moved its scratch to the session scratchpad; the DoD pass did the
same. Anything a later step must read back belongs in the session scratchpad,
not `/tmp`.

## Follow-ups, promoted from the campaign's own register

Deferred by the spec, with reasons, and now idea-registry rows:

1. **A tongue's adjectival-encoding strategy** — Stassen (1997)'s typology of
   how property words are encoded (as verbs, as nouns, or as a distinct
   adjective class). A drawn axis with its own weight table and stream label,
   not a rendering detail. Row: `LANG-tongue-adjectival-encoding-strategy`.
2. **Adposition order as a drawn axis** — Dryer (WALS 85) makes adposition-NP
   order one of the strongest correlates of verb-object order, so it is *not*
   an independent draw and needs its correlation with `ConstituentOrder`
   designed. Row: `LANG-adposition-order-correlates-with-verb-object-order`.
   It explicitly does **not** close `LANG-role-marking`: this campaign built
   the first adposition *surface*, in Common, for locative predication, which
   is a different job from marking an adjunct's role.

Registered during execution:
`LANG-prosody-needs-a-stated-transcription-convention` (implemented by this
campaign's transcription convention) and
`LANG-do-support-is-unnamed-by-its-corpus-entry`.

Open, not scoped here:

3. **The merchant corpus's cheapest remaining path is five tokens.** Greedy:
   `wh-question`, `temporal-adverbial`, `existential`, `witness-set`,
   `named-entity-list` covers the rest. `polar-question` was the sixth
   token this campaign landed, subtracting it from the remaining path.
4. **A coverage resolver tuned to `the-flood-watch`.** It scores 0 of 139 for
   a structural reason (≈8 demands per entry, conjunctive coverage), and
   deciding what *covered* should mean for a corpus the grammar was never
   built toward is a decision to take before a number exists to chase.
5. **`m08` scores covered on a production capability although its speaker is
   the player.** The corpus states no direction and 0387 forbids inferring
   one, so this is the method working as specified — and still a real limit on
   what "6 of 12" means. A direction-aware score is a later question.
6. **Singular *they* takes plural agreement in real English, and Common will
   not.** Fixing it means letting a pronoun's chosen form override its
   features — a different mechanism from the person axis this campaign built.
   Caught in plan self-review, not in review of the plan: the spec's assertion
   survived a self-review pass that checked the document against itself, and
   died to writing the assertion out as a line of test code and asking which
   paradigm row would produce it.

## Do differently next time

- **Compute a preregistered number with the instrument, not beside it.** If a
  criterion will be judged by a resolver, the prediction is that resolver's
  output on a stubbed input — never a fresh script that reimplements it. Every
  number this campaign got wrong came from the second implementation, and the
  cost of being right was one run.
- **When a correction touches commands, run one.** Row 10 was a plan fix that
  broke five verification paths and would have read green forever. A
  correction is unaudited text; a correction *to a command* is unaudited text
  that looks executable.
- **When you change a claim, grep the CLAIM, not the file.** The operational
  remedy for the seven above, generalized by the controlling session from Task
  4's finding and passed into Task 5's dispatch. A claim lives wherever it was
  ever written down, and the file you are editing is only the copy you
  happened to notice; the twin is found by searching for the assertion —
  a distinctive phrase, a number, a token name — never by re-reading the
  edit's own neighbourhood. Six of the seven would have died to one `git grep`.
- **Name the campaign's own failure shape in each dispatch.** Two data points
  now: the grep-count trap was named and was caught with both numbers
  reported; the verification-claim hazard was named in a predecessor's later
  dispatches and an implementer caught itself. Not yet a practice — two
  instances — but it costs a sentence to keep testing.
- **A carried-findings list reads as a checklist, and a checklist is a poor
  instrument for "do not do this anywhere."** Task 9 carried a four-item list
  of one pattern — silent degradation, where a realizer strips or appends
  around a surface it has not checked. The task fixed all four and, **in the
  same commit, one function away, wrote a fifth**: the new
  `realize_tongue_polar_question` strips a trailing `.` only *if* present and
  then appends `?` to whatever remains. The reviewer's diagnosis is the part
  worth keeping, and it was passed on as a diagnosis rather than a scolding:
  *fixing four listed instances of a pattern while writing a fifth suggests
  the items were pattern-matched rather than the principle internalized.*
  Every campaign in this repo carries deferred minors forward through
  dispatches, and this is the first measurement of that mechanism's blind
  spot — it arrived with a same-commit counterexample, which is as clean as
  the evidence for a process claim ever gets. The cheap remedy is to state the
  **principle** at the head of a carried list and ask for it to be applied to
  the code the task is about to write, not only to the sites named.
