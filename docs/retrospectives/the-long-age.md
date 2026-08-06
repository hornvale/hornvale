# Retrospective — The Long Age

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-long-age.md): lifespan gained a
third input, the settled drift row stopped being constant in lifespan, and no
world moved.

## Every defect this campaign hit originated in my own plan or spec text

Four of them. Every measurement a subagent reported was correct; every wrong
statement was one I had written. This is now the fifth consecutive campaign
where that split holds exactly, and it has stopped being an observation.

| # | defect | where it came from |
|---|---|---|
| 1 | tasks assumed to commit independently | the plan's task decomposition |
| 2 | type-audit tag in the wrong position | the plan's Task 1 code block |
| 3 | P3 predicted one new tag; the spec's own code carried three | the spec, against itself |
| 4 | M2's test called `life_history` directly, never touching the consumer whose mutation it specified | the plan's Task 4 test |

Defect 3 deserves singling out because it was **internally inconsistent within
one document**: §5 predicted a delta of one tag while §3.2, three sections
earlier, showed three tags in its own code block. Nothing external was needed to
catch it — only reading the spec against itself, which is exactly what the G2
self-review is supposed to be and evidently was not. The self-review checked
that the argument held together, not that the *numbers* in one section matched
the *code* in another.

**The cheap fix to try next time:** at spec self-review, extract every numeric
prediction into a list and re-derive each one from the section it depends on,
rather than re-reading prose for coherence.

## The pre-dispatch read overturned an approved spec decision

D2 chose a sparse component store for the schedule, citing `dispersion_registry`
— The Tolerance's, the campaign immediately before this one in the same
programme. The precedent was real, recent, and *did not transfer*. Reading the
call sites before writing the plan found three things:

- `dispersion` is **not in `WorldComponents` at all** — zero occurrences. It is
  read directly at its one consumer.
- The schedule has **six** consumers, and every one already holds the biosphere
  row. `render_life_history_line(name, biosphere)` takes the row and nothing
  else.
- `WorldComponents::from_stores` has **ten** callers, each of which would have
  gained a twelfth parameter.

The lesson is not "check your precedents", which is too weak to act on. It is
sharper: **a precedent transfers on consumer-count, not on component-shape.**
`dispersion` and `schedule` are the same kind of object — a per-kind authored
component — and that similarity is what made the citation feel safe. What
differs is how many places read them, and that is the property the storage
decision actually turns on. A one-consumer component wants its own store; a
six-consumer component wants to ride the row its consumers already hold.

This matters for the autopilot specifically, since "answer from precedent" is
its first move. A precedent citation should name the property it turns on, not
merely the resemblance.

## The null and the mutation requirement are in tension, structurally

This is the campaign's most portable finding.

The programme requires every campaign to prove its axis is *visible* by
mutation, not merely by a green test. This campaign also deliberately ships the
channel with **zero occupants**, because that null is what makes the epoch cheap
for the campaigns that follow.

Those two commitments collide, and not by accident:

> A consumer that resolves its subject **by name from a canonical registry**
> cannot be mutation-proven to read an authoring channel that nothing yet
> authors. Only a consumer handed the record **directly** can, because a test
> can fabricate one.

Two of this campaign's four consumers fall on each side of that line, and the
split is a property of the code's shape rather than of anyone's care. It was
found by running the mutation and watching it stay green — which is the
cheapest possible way to learn it, and an argument for running mutations even
when you are confident.

The resolution — re-siting the mutation onto a consumer that takes the row
directly — was an improvement, not a consolation: `render_life_history_line`
writes into the committed gallery almanacs, so reddening it proves the channel
can move a *committed artifact*, which the genealogy path would not have shown.

**The residue is honest rather than hidden**: that `generation_length_of`
forwards the schedule is a code-reading argument, disclosed at the test, and
C2c closes it for free by routing its first `Paced` kind through that function.
Recorded as a follow-up rather than left to be rediscovered.

## The workspace-wide pre-commit hook breaks task independence

`scripts/hooks/pre-commit` runs `make quick` — workspace clippy plus the
type-audit check *and* report freshness — on every Rust-relevant commit
regardless of what is staged. So a task that changes a widely-called signature
**cannot commit alone**; the call sites its plan assigned to later tasks must
compile first.

Task 1 handled this correctly on its own initiative: it threaded a
semantically-null `ALLOMETRIC` placeholder into Tasks 3 and 4's files,
implemented none of their behaviour, and said so. But it then created a hazard
the plan had no answer for — **later tasks had to *replace* rather than *add*,
and a leftover placeholder would have been a silent bug**: it compiles, it
passes, and it ignores the authored schedule forever. That is precisely the
rung-2 failure the whole programme is built to prevent, arriving through the
commit hook rather than through the design.

It was caught by making the sweep explicit
(`grep -rn "LifeSchedule::ALLOMETRIC"`) a required, reported step in both later
tasks. **Any future plan whose first task changes a shared signature should
budget for this**: either fold the call sites into that task, or make the
placeholder sweep an explicit deliverable of the tasks that inherit them.

## Preregistration held where it mattered and was corrected in the open where it did not

P1 and P3 held exactly: one committed artifact moved
(`docs/audits/type-audit-report.md`), the stream manifest did not move, no
epoch was owed. P3's *count* was wrong (defect 3 above) and was corrected
**visibly, with the reason stated**, rather than quietly rewritten — because
revising a preregistered number after seeing the result is the move this
project forbids doing silently, and the fact that only an illustrative detail
was wrong is not a licence to edit it as though nobody would mind.

Worth stating as a rule: **a preregistered claim and its illustrative
arithmetic are different objects.** The claim carrying the null was "one file
moves", never "+1 tag". Knowing which is which *before* the result arrives is
what makes the correction honest rather than convenient.

## A generated artifact merged textually is silently wrong

Absorbing main at the close (The Panes, 26 commits) merged **cleanly, with no
conflicts** — and produced a `docs/audits/type-audit-report.md` with the wrong
numbers.

The report is aggregate counts by class and by crate. The Long Age moved two
rows (`bare-ok(ratio)`, `species`); The Panes moved four others
(`bare-ok(count)`, `identifier-text`, `index`, `vessel`). The rows are adjacent
lines in one table, so git resolved the region in this branch's favour and
carried **this branch's stale vessel count** — 237 where main said 254 — into a
file that neither campaign had ever generated. No conflict marker, nothing red,
a plausible-looking table.

`make rebaseline` corrected it to the true union, and the delta against main
returned to exactly the two rows this campaign owns. The commit gate's
type-audit *report-freshness* check would also have caught it, so the defence
in depth held. But the sequence is worth naming because the clean merge is the
trap:

**A generated artifact has no meaningful merge. Regenerate it after every
absorption and let the diff be the answer — never trust a conflict-free merge
of a file whose contents are computed.** This generalises past the type-audit
report to every drift-checked artifact, and it argues for regenerating *before*
reading the post-merge diff, not after, so the numbers you reason about are
real ones.

## Confidence Gradient

`book/src/open-questions.md` checked against this campaign's territory. **No bet
moved — N/A.** The terrain bet concerns coastline shape and the game-layer bets
concern the possession loop's liveness; neither is touched by a life-history
authoring channel. The language-drift territory this campaign does move is not
currently scored as a bet.

## Follow-ups

- **C2c closes M2's code-reading gap for free.** Route its first `Paced` kind
  through `generation_length_of` and the argument becomes an assertion.
- **`reproductive_tempo` and `pace_of_life` saturate at 1.0** for any strongly
  paced kind, so a very long-lived kind and a merely long-lived one read the
  same. No consumer distinguishes them today; C2c and C2d will have opinions,
  and that is when the ceiling question should be reopened — with a real
  occupant to argue from.
- **`BIO-survivorship-curve` and `BIO-senescence-onset`** were captured as
  registry rows and deliberately not built: no consumer reads a curve, and a
  field nothing reads cannot be seen to be wrong.
- **The `Settled` threshold is authored, not measured.** Reusing 120 years for
  both arms is argued from decision 0066's single-product wording and is
  byte-neutral by 38 years of margin. Whether a long-lived settled people
  *should* land on `{1,2}` rather than something slower is a question a real
  roster can answer and this campaign could not.
