# Retrospective — The Radiation

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-radiation.md): six elves, a roster
of fifteen peoples, an affinity ladder whose level is derived rather than
authored, two falsifications shipped as headlines, and the close of the peoples
programme.

## The spine: twenty-four defects, every one in controller-authored text

Six implementer tasks, seven review passes, four fix rounds, one absorption of
fifty-six commits, one canonical census, one authorized fidelity cut.
**Twenty-four defects were found in text the controller wrote** — the spec, the
plan, and the six task briefs. Nineteen of them carry arc positions 21 through
39 in the running count this programme has kept since The Range; five more were
found in Tasks 5 and 6 and never numbered.

| task | defects in controller text |
| --- | --- |
| 1 | 3 (arc 21–23) |
| 2 | 5 (arc 24–28) |
| 3 | 5 (arc 29–33) |
| 4 | 6 (arc 34–39) |
| 5 | 4 |
| 6 | 1 |

The distribution is the same as The Range's and the diagnosis has not changed:
the controller produces prose that *asserts things* and dispatches it to agents
who execute against it, so a controller's mistake meets nothing until someone
happens to check the premise. What this campaign adds is that the premise can be
checked, found wrong, and then *replaced by a second wrong premise* by the same
process.

Three of the twenty-four are worth the campaign's name.

## 1. `0.25` was never derived, and that is where it came from

The biome-affinity ladder's fourth step — the level a row falls back to away from
its home ground — was `0.25`. It is the single most world-shaping constant this
programme has shipped, and its provenance is this:

- It appears in The Range's implementation plan at four places. **Every one of
  them is inside test-fixture code the controller wrote as illustration.**
- It appears in The Range's spec **nowhere**.
- The Range's implementer adopted the example as an authored constant.
- This campaign's Task 3 adopted that constant as house style for six more kinds.

An example became a convention became a world-shaping constant, unexamined
because it looked like a detail. At `0.25`, six new rows took seed 42's history
from 552 occupation records to 193 and breached four deliberate fidelity floors
at once. Derived, the same six rows give 704 — *above* the 552 measured with no
elf rows at all, because the derivation relaxes The Range's two rows as well.

**The lesson is not "check your constants."** It is that illustrative code in a
plan is *read as authoring guidance*, because that is exactly what it looks like.
A plan that shows a worked example shows a value, and there is no mark on that
value distinguishing "this is the number" from "this is what a number looks
like." The cheapest available fix is to write illustrative literals as something
that cannot survive — `DERIVE_ME`, a `todo!()`, a comment on the line — rather
than as a plausible float.

And the campaign nearly repeated it in the repair. The obvious move was to raise
the level to `0.50`. That was rejected on the record: it swaps one undesigned
number for another with a floor-shaped justification attached. The number that
shipped is not a better guess; it is not a guess. It is
`sovereignty_floor(mass, potency)` — the model's existing answer to the question
the level was silently answering — mapped through the same algebra a condition
tolerance already uses.

## 2. A repair is as likely to carry the defect as the thing it repairs

This is the campaign's thesis at its third storey, and it happened twice in the
same task.

The Task 3 review found a false mechanism claim in a document. **Fix round 1
corrected it by writing a second false mechanism claim** — this time in the
*spec erratum*, the document the next task reads first. The false claim was
produced by generalising one true measurement, taken on the single occupant for
which it holds, with **no positive control run to establish the readout could
detect the effect at all**.

The re-review found it by running the control that had not been run:

```
  desert-elf 50 → 500 kg   RED, 12 rows move, all its own   ← the positive control
  wood-elf   55 → 550 kg   RED, 36 rows: wood 12, high 12, DROW 12
  drow       52 → 500 kg   GREEN — because drow's row is wood.clone(), so its
                           level is a function of WOOD-ELF's mass, never its own
```

The one arm fix round 1 ran is the *only* arm for which its rule holds. And the
inverted rule had already reached the spec erratum, where it declared dead the
one mechanism this campaign had just made live — one commit earlier.

**A null result needs a positive control before it is generalised.** One extra
mutation on a second subject would have caught this at the source, both times,
for about ninety seconds of machine time. The re-review's third arm is also the
one that discovered the real structure: drow and high-elf take wood's row
entire, so mass reaches the field through the affinity level for the six kinds
whose rows are self-derived and for neither clone-taker. That is not a caveat on
the rule; it *is* the rule.

The same shape appeared once more, in the chronicle repair for The Range. The fix
opened *"that last clause is false"*, corrected the clause it named, and left the
adjacent clause standing — in bold, with a *stronger* modal than the one it had
just corrected. It too was false, by a wide margin. **Correcting a sentence is
not the same as auditing the sentence next to it.**

## 3. Nathan's diagnosis: two quantities wearing one number

At the Task 5 stop, Nathan named the condition under all of the above, and the
naming is the most valuable single artifact of the campaign.

> **The affinity level is two quantities wearing one number.** A rung sets how a
> kind *ranks* cells, and it sets how *productive* that kind is on them. The
> level is arbitrary for the first job and load-bearing for the second.

So *"the level is gauge"* was never a careless claim. It was a **true statement
about one of the two jobs, applied to both** — which is precisely why it survived
a spec, a plan, a decision ledger and a merged chronicle unchallenged. Nobody
caught it because it was not obviously wrong; it was obviously right, about half
of what it described. It is the shared root of Task 3's collapse, of the
"uniform affinity is inert" clause, and of fix round 1's inverted mass rule.

Captured as the registry row `BIO-affinity-level-is-two-quantities`, after a
registry-first check confirmed no existing row covers it (`BIO-26` and `BIO-40`
are about the sovereignty floor itself; `BIO-subsistence-feeds-capacity` is about
a missing capacity term; none is about one factor serving two consumers with
different tolerances for its level).

**The framing rule, and it is load-bearing.** The derivation is an
**improvement**, not a solution. It makes the level principled; it does **not**
split the job, and the same class of surprise can recur the next time one number
serves two consumers. A chronicle or retrospective that said "we derived it, so
it is fixed" would commit the campaign's own thesis one last time on the way out.
Both documents are written to that rule deliberately.

The general form worth carrying: **when a quantity's justification is true of one
of its consumers, ask how many consumers it has.** A claim that is true and
incomplete is far more durable than a claim that is false, because nothing
contradicts it.

## 4. A check whose failure message names its own cause

Two instances, both found in the closing sweep, both the same shape.

`the_independent_reading_covers_every_staple_worldgen_can_steep` had been failing
for a whole campaign with the message *"the duplicate is stale again"*. Measured
at its own subject: millet was `rooted = false` **and** `dup_steeped = false` —
worldgen and the duplicate **agreed**. The criterion the test existed to enforce
lived in a *comment*, while the body asserted only one side of it, so two
completely different causes ("the duplicate drifted" and "the witness's world
moved") produced an identical red, and the message named the wrong one. Repaired
by asserting the sweep's own criterion as a precondition — the shape its sibling
test always had — and then re-witnessed.

`p6_seed_42s_committed_world_moved` compared `len() == len()` on two rosters and
reported a message about which peoples hold ground. It could never assert its own
message: two equal-sized rosters with different membership pass. The authored
constant beside it still named nine peoples while all fifteen hold ground at seed
42, so the property held *more strongly than ever* and the constant was the stale
part. Repaired at the cause: the roster is derived, and the assertion compares
the set.

**Both fail in the safe-looking direction, which is why neither was red about the
real thing.** The rule this campaign would write: *a failure message is a claim,
and an unasserted claim is a comment.* If the message names a cause the body
cannot distinguish, the body is the defect.

Two dormant staleness bugs of the identical shape — an authored list sitting
beside a roster that is derived — were fixed while working the list, neither of
which anything was red about.

## 5. Process facts, learned the hard way

- **A conflict-free merge runs no hook; a conflicted one runs `pre-commit`.** So
  the *easy* merge is the unguarded one. (The project's standing note said merge
  commits skip the hook outright; that did not hold here, and it is what caught
  a bad type-audit report.)
- **Two generated artifacts merged *wrong* without conflicting.** Four collided
  and announced themselves; two auto-merged cleanly and silently. One lost seven
  lines of the type-audit report — caught by the hook. The other **dropped all
  four `person` predicates** an incoming campaign had just added to the concept
  registry, and *nothing* caught it but regeneration. A conflicted generated file
  is the safe case. Regenerate after every absorption, unconditionally; a clean
  auto-merge is not evidence.
- **The controller re-dispatched into a checkout left dirty by an interrupted
  agent without sweeping it first**, which the dispatching skill explicitly
  requires. It cost a confused diff rather than a defect, this time.
- **The seven-path drift list is not the whole artifact surface.** Two surfaces
  outside it reddened during Task 2 — the audio clips and the history page's
  hand-authored prose, which lives in the *generator*, not in the `.md`.
- **`make rebaseline` has no partial-failure story.** A census column change
  fails the schema-backfill step, and `set -e` takes the *five* generators after
  it down with it. A campaign that trusted the exit code would have shipped an
  unregenerated survey, type-audit report, trope reports and digest with no
  signal distinguishing that from "nothing drifted". It also orphans a zero-byte
  `.tmp` file that is untracked, so the seven-path `git diff --exit-code` is
  structurally blind to it — and this is exactly the campaign most likely to
  reach for `git add -A`, which would commit it.
- **`make lab-diff` cannot straddle a schema widening.** It validates *both*
  CSVs against the *live* study schema, so it refuses outright on any campaign
  that adds a metric — the campaigns where a reviewer most wants it. Worked
  around by padding the old CSV with an empty column in schema position.
- **Two tiers need two expected-red files, not two sections of one.** The
  nextest freeze compares its whole contents against one `cargo nextest run`; a
  heavy-tier name added to it would sit on the left of a diff whose right side
  never ran it, and would read as a deviation on every green run forever.
- **`--ignored` runs every ignored test regardless of reason.** A prediction
  parked under the `PREREGISTERED, not met:` idiom is *not* skipped by
  `cargo test -- --ignored`; the tier that skips it is the heavy tier, whose
  roster is discovered by grepping for the `heavy:` token specifically.

## 6. Two premise-checks that paid for themselves

Both from the decision ledger, promoted here because that ledger dies with the
campaign.

**A real number with the wrong denominator reads exactly like a finding.** The
campaign opened by flagging "the ocean is 2.7× the land" as its leading risk —
29,896 ocean cells against 11,066 land. The number is correct and the conclusion
was false, because *no elf gets all the land*. In the right unit, per-elf
habitat, sea-elf's shelf band is 1,425 cells: larger than desert-elf's 241,
smaller than snow-elf's 4,633, a nineteen-fold spread across the family with no
outlier. The runaway existed only in the wrong denominator. Same shape as The
Range's cost estimate, which multiplied a world-build by a thousand against a
pass that already paid it.

**One measurement settled what an hour of reasoning could not.** Whether a sea
people is structurally possible was answered in minutes by measuring capacity on
ocean cells for three marine kinds and one terrestrial one, and by reading the
admissibility chain rather than assuming it. The whole sea-elf design rests on
that one fact. The Range spent an hour on a confident causal story that a single
premise-check dissolved; this campaign spent four minutes instead, twice.

## 7. What the process got right

- **The falsifications shipped as headlines, and neither axis was widened.** The
  concentration prediction failed for desert-elf; the honest and available move
  was to redefine success as "any authored biome", which would have made the
  prediction pass six times over after seeing the answer. It was rejected on the
  record, the axis stayed frozen, and the falsification was carried in the
  project's existing idiom with its diagnosis attached and a successor axis owed
  to a future campaign. The disposition claim was left **red** on purpose, with
  the honest note that the pre-campaign roster already read 1.045 against a
  falsification bar of 1.0 — it was within 5% of failing before this campaign
  touched anything.
- **A falsifier was corrected *before* unblinding, and dated.** The single-arm
  design for drow could not distinguish the realm gate from the resource vector,
  so the conclusion did not follow from its condition. Expanding it to a
  five-arm factorial was recorded in the ledger before any measurement, which is
  what separates it from a retune.
- **An operationalization is itself a preregistration.** The homophony clause's
  spec text supplied no quantifier that code could execute; the implementer
  invented one, it fired, and the axis was changed after seeing that. The change
  is defensible — the replacement is *not* a pure weakening, since a consistent
  1.5× leak passes the retired ceiling and fires the shipped assertion — but the
  cost is real and it is declared in three places rather than buried. **The
  quantifier belongs in the spec**, written by the person who will *not* see the
  number first.
- **A blocking implementer.** Task 3 authored six correct rows, breached four
  fidelity floors, and **stopped** rather than lowering the floors — verifying
  first, by stashing, that the reds were its own. Every floor's comment said in
  advance that breaching it was a finding for the owner and not a re-pin. That
  those comments were written earlier, by other campaigns, is why this one
  worked.
