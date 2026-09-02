# The Crosscut — retrospective

Process lessons, not product; the product is
[the chronicle](../../book/src/chronicle/the-crosscut.md); the decisions are
0566–0568. The Circuit's first campaign, seven tasks, three stage gates, no
task exceeding two fix rounds.

## The spec refined an object its own consumer had never read

The design that Nathan approved at G3 keyed the descent plan to a run address
and said, in as many words, that it *refined* `passages_from` — the chamber
lattice's passage function. Writing the implementation plan meant reading
`Underground::enter`, which is the code that actually builds the levels a
player walks, and `enter` reads none of that: it builds one level per
habitation rung under a single vertex and never touches a run, a branch, an
entrance or a junction. The lattice's edge half has no walking consumer at all,
a fact the idea registry had already recorded.

Had the correction not been caught, the campaign would have shipped a
structurally correct plan for a place nobody stands in — and the plan would
have looked right against every document, because the documents describe the
lattice accurately. Nothing mechanical would have objected: the code would have
compiled, the tests would have passed, and the readouts would have measured a
real graph.

**The check that would have caught it at G3 is one line: grep the consumer of
the thing you say you are refining, before writing that you refine it.** Not
"read the function you are refining" — the passage function is fine and does
what its docs say. The question is who calls it, and the answer was nobody on
the path this campaign cared about. A refinement claim is a claim about a
*consumer*, and it is verifiable in a single command.

This is a new instance of a shape already in the memory index — verify the
proposition, not a cheaper neighbour. Reading `passages_from` verifies that
`passages_from` works. It does not verify that refining it changes what a
player sees.

## "By construction" was written before the sweep that would have tested it

The design listed four properties as holding *by construction*, each pinned by
a unit test rather than a measurement. Three of them do. The fourth — every
level has at least one loop of its own — was reasoned from the algebra of the
two grammar operations, where it is true, and asserted about the geometry those
operations run on, where it is not: a cross-floor cycle anchored on one level
spends cells on the level below.

It was falsified by a sweep the reviewer ran rather than by anything the
implementation had: 400 seeds × 12 vertices × every rock-and-workmanship
combination, 72,000 levels, 24 of them starved. The first repair (a free-cell
count reserve) cut that to 9 and the residue was diagnostic — those nine were
not short of cells, they were *enclosed*, which is a thing no count can
detect. The invariant that finally held is a capability test ("could this level
still close a loop?") rather than a quantity, and it took the count to zero.

**Lesson: an invariant claimed "by construction" gets a sweep before the phrase
is written, not after.** The phrase is doing real work in a design document —
it is what licenses pinning a property with a unit test instead of measuring
it — so it is exactly the claim that should be cheapest to falsify and most
expensive to leave unchecked. The sweep here cost minutes. Two rounds of
renegotiation with a spec lower bound in play cost considerably more, and the
second round only happened because the first repair was chosen from a
plausible mechanism (scarcity) rather than from a diagnosed one (enclosure).

## The review that found the worst defect measured instead of reading

Task 3's realizer repairs connectivity after writing a stairway onto a cell, in
case the cell it overwrote was the only way across a region. The repair was
passed the whole level's extent and was blind to the plan. Where the nearest
route around a new stairway ran through a neighbouring region the plan had
deliberately left unlinked, it carved a corridor through that wall — destroying
the single property the campaign's realization exists to create, while making
every connectivity assertion in the suite *more* likely to pass.

No test was red. The reviewer wrote a probe: on how many levels does a pair of
grid-adjacent regions with no plan edge have a walkable route between them
anyway? Answer: 609 of 1,620 — **37.6%**. The fix is one argument narrowed
(the region's own rectangle instead of the level's extent), and the property
now has its own test, captured red against the unscoped version before the fix
was restored.

**Lesson: when a change's failure mode is "the artifact is too permissive",
reading the code will not find it, because permissiveness is what every
existing assertion rewards.** The instrument has to be a measurement of the
property that is *supposed to be absent*. The same reviewer had already
disclosed the risk in the abstract ("no test asserts the non-adjacency wall")
and it took the number to make it actionable.

## Preregistration held under pressure, twice, and the honest costs are visible

The loop-share metric's bluntness was seen at Task 2, while the grammar was
being written and before any verdict existed: because a cave has one doorway on
the west edge, a descent where no loop attaches at that doorway reads exactly
zero for every region, however many loops it has. The temptation to redefine
the metric was real and the redefinition would have been defensible.

The ruling was that the frozen metric stays frozen and may FALSIFY, that the
bluntness is itself a finding, and that a report-only companion is printed
beside it — clearly marked as added *after* the behaviour was seen, so it can
never be read as a prediction. It falsified: median 0.11 against a floor of
0.50, with the companion at 0.85. Both numbers are on the committed page and
both readings are in the chronicle.

The second instance runs the other way and is the more interesting one, because
it costs a *pass* rather than a null. The density ordering passed on all three
seeds, on both halves — and every median equals the budget function exactly,
because the grammar reaches its target on essentially every level. So the
readout confirms the budget is derived and reached; it does not witness that
the world varies as the rule claims. That is a live instance of the shape
already in the memory index: a test whose input collapses to one value cannot
distinguish computed from assumed. Recording it inside decision 0568 — the
decision the pass supports — rather than in a footnote is the point: the caveat
travels with the claim.

## Two mechanical carry-forwards that cost a commit each

Both were known repo facts that brief snippets predated, and both cost a
failed gate rather than a review round:

- A test that loops over seeds needs a `claim:` doc tag (decision 0093). Task
  0's brief snippet had none, and `gate-commit`'s sub-floor tier caught it on
  the first commit attempt. It recurred in every later task's snippets and was
  carried in the dispatch notes from Task 0 onward.
- Brief snippets are not `cargo fmt`-clean, and `fmt --check` is the first
  thing the gate runs.

**Lesson: a brief that ships code carries the repo's own default-deny lints in
its checklist.** These are cheap to state once and were, after Task 0.

## Carried forward, from the ledger

Registered rather than left to be rediscovered:

- **The walk and the lattice disagree**, and the walk is what a player stands
  in. Now a registry row of its own rather than a paragraph in a spec
  correction.
- **Descent carves are per-world.** The carve streams key on the world seed
  with no vertex, so every cave in a world carves identical interiors; the plan
  is per-vertex, the carves are not. Pre-existing, and newly *visible* because
  structure now varies where texture still does not. Also a row.

Deferred minors, each disclosed by a review and accepted with the cost named:

- The lengthening move tests its capability invariant against the *pre*-extend
  passage set, which still contains the edge it is about to remove. The correct
  test is the post-extend set. Conservative in the safe direction (it can only
  refuse an extension it might have allowed), so it defers cleanly.
- The terminus stairway is silently skipped when its region's only walkable
  cell is already a landing. Wants a debug assertion naming the cause rather
  than a silent skip.
- The retired partition label's retirement note sits as the leading rustdoc of
  a carver enum rather than as a module-level comment — a reader meets an
  obituary where they expected a type.
- The membership metric's test re-derives the share the same way the
  implementation computes it, line for line. It witnesses that the function
  runs; it does not independently witness the number.
- The realm-ownership test's node half restates the implementation's rule
  rather than deriving it another way (Task 2 review). Same shape as the
  bullet above; accepted with the same cost.
- The walk test resets the session's underground on its skip branch when
  nothing has set it (Task 4 review). Harmless; accepted.
- A kind that carried neither character on a seed renders NOT MEASURABLE in
  the worked-vs-wild half of the readout (Task 5 review). Sensible, and
  unspecced — §4.2 is silent on the case. Accepted; the page says what it did.
- `docs/generated-path-writes.tsv` records the gallery directory's tracked
  count as 45 where the previous row read 46, with no gallery file differing
  (Task 5). Pre-existing, unexplained, triaged may-land by the final review;
  recorded here so the next regeneration that moves it has a prior sighting.

Two more were disclosed and then closed before merge, so they owe an outcome
rather than a cost:

- Task 0's tree-premise test carried a helper that duplicated a tree walk; Task
  3 deleted the test along with the tree it pinned.
- Task 2's `dof` floor (`1 + rungs + realms`) sat far below the observed count
  and would not have caught a dropped draw site; the final fix wave replaced it
  with an exact identity counted at the failure site (ledger #14).

## The sweep that could not see the defect

The final review found a coordinate collision: on a middle rung a region is the
upper end of one stairway and the lower end of another, the two coordinates
were drawn independently, and on ~9% of five-rung descents they coincided —
after which the realizer overwrote one stairway with the other and the descent
verb hit a debug assertion on the way back up.

**A test existed for exactly this contract and passed the whole campaign**, and
the reason is worth keeping: it swept two rungs. A two-rung descent has no
middle rung, so the sweep's 200 seeds could not produce a single instance of
the shape the defect needs — 200 negatives that were all vacuous. Widened to
the real five-rung habitation ladder, it fails on the first colliding seed.

The generalisation is not "sweep more seeds". It is that a fixture reduced for
speed can quietly delete the *structural* precondition of the defect class the
test exists for, and no amount of seed breadth recovers it. When a fixture is a
reduced version of the real object, ask what shapes the reduction removed
before trusting the sweep's negatives.

## Do differently next time

Before writing that a campaign *refines* an existing mechanism, run one grep
for that mechanism's consumers and paste the output into the design. And treat
"by construction" as a phrase requiring evidence at the moment it is typed —
it is the strongest claim a design document makes, it licenses replacing a
measurement with a unit test, and this campaign spent two renegotiation rounds
discovering that its own instance of it was false in 0.25% of the space.
