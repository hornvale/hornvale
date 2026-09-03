# The Brattice — retrospective

Process lessons, not product; the product is
[the chronicle](../../book/src/chronicle/the-brattice.md); the decisions are
0616–0620. The Circuit's second campaign, eight tasks (0–7), three stage gates,
two absorptions of `main`, no task exceeding three fix rounds.

## A test over the function is not a test over the data

The campaign's first blocked task found that four of the ten frozen inventory
rows could never be drawn, because one of the four length classes did not occur
in any plan — zero of 4,412 realms measured. The cause was the predecessor's:
the class was computed when a realm was created, where one of its paths has at
most three edges, and the grammar then went on splicing chains into both paths.
The stored class described a graph that no longer existed. Nothing had read it,
so nothing was wrong until something wanted to use it.

**A test existed and passed the whole time.** It fed hand-written lengths to the
classifying function and checked the function's rule, which is correct. It never
asked what classes the plans actually carried. That is the same shape the
project has now recorded three times in two campaigns — a test whose input
collapses to a range the defect cannot live in — and the instrument that found
it was neither reading nor a unit test: it was one loop over the realized plans
printing the support of a value, which took minutes.

**Lesson: when a value is exported for a later campaign to read, the first
campaign that reads it should print its distribution before designing against
it.** The exported attribute is the interface, and an interface with an
unmeasured range is a guess. The repair — recompute after growth by the
identical rule — is three lines; the test that pins it asserts *both* the
equality and that a previously-unreachable class is actually sighted, because an
equality alone cannot tell a working pass from one that never runs.

## "Empty by construction" failed a second time, in a smaller way, and was caught

The predecessor's retrospective closed by saying that "by construction" is the
strongest claim a design makes and gets a sweep at the moment it is typed. This
campaign wrote one anyway, in a task ruling: a tenth inventory row was removed
because its cell was "empty by construction — a cross-floor lower path has at
least three edges, so both paths cannot be short."

That does not follow. A two-edge path against a three-edge one classifies as
short-short under the frozen rule, and the review said so. **The decision stood
and the argument was replaced**, which is the right outcome and the only reason
this is a paragraph rather than a defect: the row really is unreachable, by a
parity argument — both of a realm's paths are unit-step walks between the same
pair of grid squares, a grid is bipartite, so their lengths are congruent mod
two, which forces both short paths to be equal and at most two while the lower
one is at least three.

**The part worth carrying is why the review's own suggested repair was not
taken.** The review offered a case analysis over the creation-time path lengths.
Since this campaign's own first ruling, the stored class is recomputed *after*
growth, where a path reaches sixteen edges — so a creation-time case analysis
would have proved a lemma about a class the plan no longer stores. That is the
same category of error the review was correcting, one level down. Parity closes
it because a spliced detour is itself a walk between the two squares it
replaces, so it moves a length by an even amount and the congruence survives
every growth move.

And then the lemma was **witnessed** as well as argued: a sweep over 1,800 plans
looks for the forbidden shape directly and says, in its own failure message,
that if it ever fires the row must come back. An argument in four places is
still an argument; a sweep is a fact.

## The invariant was half of an invariant, and the design's own words said so

The frozen design asked that a body holding nothing reach the terminus and every
key *from* the entrance. Task 2's review found what that misses: a chute is free
downward, so a chute taken down into a realm whose upper path a nested sump
later blocks leaves the default body in a place it cannot leave. Forward
reachability is satisfied. The descent is a trap.

The correction is that solvability is the **round trip**, enforced at stamp time
and asserted on every descent. The measured consequence is exactly one chute
placement on the panel's first seed converted from placed to refused — small,
and it is a cave you could fall into and not get out of.

**What makes this a process lesson rather than a bug report is that the design
already contained the answer.** It cited Dormans' "unknown return path" as the
thing a cross-floor cycle buys. Unknown is not absent. A one-way measure would
also have read every chute's detour cost as zero, which the readout's own frozen
wording had already noticed and defended against by measuring a round trip. Two
places in one document said "round trip" while the invariant said "reach", and
nobody read them against each other until a reviewer did.

## A brief that writes the test body can write a tautology

Three of this campaign's review findings are the same finding: a test that
re-derives the implementation's own arithmetic and therefore witnesses only that
the function runs.

- Task 1, two minors, both **plan-mandated**: the determinism test's "the leg
  ignores the vertex" half is unwitnessed, and the "the pass adds nothing" half
  asserts no node or edge counts.
- Task 2, an Important, also **plan-mandated**: the gate-yield test recomputed
  the ratio the same way the function does. Fixed by overwriting a cloned plan's
  outcome vector with a hand-built one and asserting a literal.

The predecessor recorded this shape twice as a *deferred minor discovered in
review*. Here it is upstream of that: the implementation plan supplied the
snippets, and an implementer following the brief faithfully produced the
tautology. The brief is the leverage point — a test body written in a plan is
the one place a reviewer is least likely to push back, because it reads as a
requirement rather than as a draft.

**Lesson: when a plan ships a test body, it must also ship what that test would
fail against.** A snippet with no stated counterexample is a snippet nobody has
checked.

## Every task's review found a doc comment asserting a contract the change broke

Not one task escaped this, and the instances are worth listing because they are
all the same mechanism and none of them is a comment that was wrong when it was
written:

- Task 1: `skipped_patterns`' doc omitted a case the pass could produce.
- Task 3: three wire-vocabulary docs still said "five kinds / ten glyphs" above a
  table listing eight; `peek_stairs`' contract paragraph did not mention the new
  cell kind.
- Task 4: the ways-on report's doc said the sentence "must report what `go` can
  do" — true until the same task made a sump refusable, false the moment it did;
  and `describe_underground_here`'s doc named a function that had just been
  renamed.
- Task 5: `take_underground`'s doc said the condition was latency when the code's
  condition is identity; `peek_stairs`' "a chute is ONE-WAY here" survived into
  the task that made it one-way only for a walker.
- Task 6: a walk's doc called its fixture "unchanged" in the very commit that
  widened it from four conditions to six and moved it to a different vertex.

**These are not stale comments; they are comments the change invalidated, in the
same diff.** A rustdoc paragraph is an assertion with no test behind it, so it
fails silently and forever, and it fails *most* where the change is most
interesting — the docs that had to be written carefully in the first place are
exactly the ones that state a contract. The cheap habit the campaign converged
on and should have started with: for each function the diff touches, re-read its
own doc paragraph as though it were an assertion in the diff.

## The ideonomy pass on the door changed the design, not the vocabulary

Two rulings were made with Nathan present, and both were ideonomy passes rather
than choices among prepared options.

The first asked whether a gate's four aspects should be four co-located things.
The first draft had a `Door` cell kind, and the pass's *side-effect* prompt
produced the objection that killed it: a `Door` glyph paints built doorways
across a wild cave. The result is the design's spine — `Threshold` at every
passage's crossing, a door as a Thing anchored at one, on the wire as a mark —
plus **two enrichments neither the spec nor the brief would have produced**: that
requirement and judgment are the one non-independent pair, so a realization
witness is required rather than optional; and that danger is not orthogonal to
requirement (Dormans' dangerous lock is a requirement of a *faced* kind), which
is why the hazard stamp is exported unread rather than absent.

The second read the model backwards as a capability probe against a corpus of
doors from literature and games that Nathan supplied — the discipline the trope
corpora use. It converged in one pass with one relaxation (the anchor rule, so a
door onto rock stays admissible later) and, more valuably, produced the model's
**boundary as a stated limit**: a requirement is a predicate on the traverser's
own state at the threshold, and every case that broke — a door that reads the
clock, a plate that needs two bodies, a corridor that remembers your route —
breaks by reading something else. That sentence is now in the spec, in decision
0616's neighbourhood, and in the chronicle, and it is what a future campaign
will need in order to know that world-conditional gates are a different proof
rather than a bigger table.

## A harness crash, and the diff that survived it

Task 3's implementer was killed mid-edit, leaving seven modified files and no
commit. The task was not restarted from scratch: a fresh implementer inherited
the partial diff and read it **hunk by hunk against the brief**. Every hunk was
kept; one was changed (a rustdoc that asserted a specific pre-existing defect on
the strength of a single anecdote, replaced by a measurement); none was dropped.

That was the right call, and the reason it was safe is worth naming: the
inherited diff had never run a suite, a lint pass, the client gate or a
rebaseline, so all of those were the second attempt's, in full. The risk in
inheriting a partial diff is not the code — it is inheriting the *belief* that
something was checked. The rustdoc that had to be replaced was precisely that
shape: an unverified claim sitting inside otherwise-good work, and it read as
verified because everything around it was.

The measurement that replaced it found a real pre-existing realizer defect: a
stair connector paving over ways already placed, 23 broken stairways over 200
seeds, invisible because the existing pairing test swept two engine
combinations and not the one that produced them.

## Two absorptions of `main`, and what each cost

Both were taken at task boundaries as the standing rule says, and neither was
free.

- After Task 2, a stage gate was **refused at the mouth in milliseconds** because
  the branch conflicted with `main` in the regenerated type-audit report. That is
  the mouth working: the conflict cost a rebase and a regeneration, not a
  chamber slot.
- Before Task 5, absorbing The Plumb and The Rack brought three semantic
  conflicts that git raised as none: a new default-deny gate over authored
  numeric constants (which wanted tags on this campaign's own new constants), a
  roster refactor that moved a test seam, and a lexicon ceiling that had moved
  underneath the branch.

**The pattern in both is that the expensive half was never the textual merge.**
It was the new gate a merged campaign brought with it, which this branch's code
had to satisfy retroactively. Nothing warns about that, and nothing can; the
mitigation is that absorbing at every task boundary keeps the retroactive bill
to one task's worth of code rather than a campaign's.

## The lexicon ceiling moved five times

`docs/audits/lexicon-inventory.tsv` was raised in Tasks 1, 4 (twice), 5 and 6,
and fell once in Task 5's fix round. Every token was the AREA sense of "cell" —
a grid square of an underworld level — which is the sense the inventory records,
and each raise was done by hand or through the rebaseline flag with the diff
read, never by blanket rewrite.

This is the guard doing its job at some friction, and the friction is the point:
a campaign that carves levels *will* add area-sense tokens, and the alternative
(raising the ceiling once, generously, at the start) would have blinded the guard
to the vertex-sense token it exists to catch. It caught one at Task 1 and refused
the commit.

## No Confidence Gradient bet moved

Checked rather than assumed: `grep -n -i
"underworld\|cave\|descent\|lock\|traversal" book/src/open-questions.md` returns
89 lines, all of them in one bet's discussion — whether the world can place a
*people* underground, a settlement-seating question whose standing precondition
is the underworld being declared as *places*. Gates are not places and do not
touch it. **No bet moved, and none was re-scored** (decision 0030's obligation
is discharged by the check, not by an edit).

## Two merged chronicles overstate, and the correction went into this one

The campaign's book sweep found two sentences on `main` that are now false or
were always false:

- The Chattel's chronicle says six verbs shipped "with `lock` and `unlock`
  beside them." Neither verb exists and neither was added.
- The Gallery's chronicle says swimming and flight are "designed and not
  implemented, on purpose." Both are implemented now, by exactly the route that
  sentence predicted.

**Neither file was edited**, because a chronicle is immutable once it merges —
the boundary is `main`, and the rule protects a history someone could have
relied on. The corrections are stated in The Brattice's own chronicle, loudly
enough to be found by anyone reading the Chattel's or the Gallery's and
following the trail. The alternative (a quiet in-place fix) would have made the
book agree with itself while destroying the record that it once did not.

## Where the ledger's follow-ups landed

| follow-up (ledger) | outcome |
|---|---|
| The production walk reaches no door (`enter` hardcodes `WildCave`) | **Still open, deliberately.** Disclosed in the spec §1, on the committed audit page, in the chronicle, and in decision 0617's consequences; `MAP-walk-ignores-the-lattice` gained the sentence. The Plat's `ChamberOverrides` writer is the program's own answer. |
| The strongbox's `Portable` literal | **Carried as a row**, `PLAY-strongbox-lock-wants-an-unlocks-property`. Not fixed here: the fix reopens decision 0516, which is someone else's decision to reopen. The descent door never reads the literal, so the strongbox is now the only lock carrying the hazard. |
| Stale prose naming `the-key-on-the-ledge` | **Fixed in Task 5**, in passing, because that task had the file open — and loudly, since the fifty lines around it are a measured defect report and one unverifiable line inside a measured report reads as measured. |
| The Chattel's chronicle overstating the verbs | **Corrected in this campaign's chronicle**, not in the Chattel's file (above). |
| The Crosscut's `try_extend` pre-extend capability test | **Taken** (ruling C). It moves four committed readouts once, verdict words unchanged, attributed by revert. Taken now because decision 0618 makes every later plan-grammar change an epoch. |
| The Crosscut's terminus `debug_assert` | **Taken in Task 3**, with a sibling assertion for the case this campaign introduced (a chute's landing being a region's only footing). |
| The Crosscut's `CELLULAR` rustdoc obituary | **Taken in Task 3**; moved off the enum onto the module doc. |
| The Crosscut's membership-test re-derivation; the gallery 45↔46 wobble | **Left**, as the ledger said: not in this campaign's path. |

## Deferred minors, with the cost named

Each was disclosed by a review or a self-review and accepted rather than fixed.
Outcomes are stated so the next campaign does not rediscover them as new.

**Closed inside the campaign** (they owe an outcome, not a cost):

- Task 1's `skipped_patterns` doc omitting the no-admissible-row case — corrected
  in fix round 3, along with removing a skip reason that was never constructed.
- Task 3's `underground_footing_word` reading deep water as "dry" — closed in
  Task 4, which owns the footing words.
- Task 4's parked finding that the ways-on report was geometric and could name a
  bearing `go` refuses — closed in Task 5 (ruling I), by making the report ask
  the same actor-aware predicate the step does, with the same door oracle.
- Task 5's doc paragraph that had merged with its neighbour's, leaving a constant
  undocumented — closed in Task 6 while the file was open.

**Still open, each with its cost:**

- **No synthetic fixture forces the two rollback paths.** `NoRoom` and
  `Unsolvable` were zero across a 4,412-realm sample; the full first-seed panel
  now exercises `Unsolvable` twice for real, so the path is live rather than
  merely reachable, but a hand-built plan that forces each would be the proper
  cover. Cost: a rollback path exercised only by data that happens to occur.
- **Two determinism/claim tests are unwitnessed halves** (plan-mandated, Task 1):
  "the pattern leg ignores the vertex" and "the pass adds nothing to the graph"
  are asserted without node or edge counts. Cost: the assertions read as coverage.
- **The solver rebuilds its key-node list per search**, scans positions linearly,
  and hand-rolls its draw. Cost: nothing observable; `brattice.rs` is now over a
  thousand lines and the solver is its natural first split.
- **`Body.keys` is tagged `bare-ok(count: keys)`** because `bitset` is not one of
  the audit's classes. Cost: the tag names the wrong shape.
- **`peek_stairs`' drop-refusal branch has no unit test** (it is covered by the
  acceptance walk). Cost: a walk-level test failing tells you less about which
  branch broke.
- **The terminus fallback can overwrite a chute landing** when a region's only
  walkable cells are landings. Debug-asserted and witnessed; silent in a release
  build. Cost: a terminus stairway silently skipped in a shape nobody has yet
  produced.
- **The witness's walker arm includes chute landings trivially** (a landing is
  written as floor), and its zero-deep-water arm has no positive control that
  some level in the sweep actually has no sump. Cost: two arms weaker than they
  read.
- **A `claim:` tag on a seeded-helper consumer is decorative** — a pre-existing
  blind spot in the scanner, not this campaign's. Cost: a tag that looks
  enforced and is not.
- **Two named deviations, both kept with a comment saying why:** the session's
  narration folds the flight mode into the default "step" phrasing while the
  admission seam treats the same case as unreachable (a narration for a mode
  nobody can be in is harmless; a silent admission is not); and `drop` refuses
  outright when a possession stands on a divider no plan region covers, rather
  than posting a location no fold can read back.

## Do differently next time

Print the support of any exported value before designing against it — the
predecessor's four properties got a sweep and its one exported *attribute* did
not, and that attribute is what cost this campaign its first blocked task. And
when a plan ships a test body, ship the counterexample it is supposed to fail
against; three of this campaign's findings are one implementer faithfully
writing three tautologies a brief had already written.
