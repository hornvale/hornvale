# The Pavement — retrospective

Process, not product. The product is in
[the chronicle](../../book/src/chronicle/the-pavement.md); the rulings are in
[the campaign ledger](../superpowers/ledgers/2026-08-30-the-pavement.md).

## The one that would have shipped

**My specification named a projection measurably worse than the one it
replaced, while claiming an improvement.** The naive cube-sphere normalize
distorts 5.2x; the icosphere it displaced measures 1.5–2x. Caught before
implementation, fixed by requiring the tangent warp (1.41x), ratified as
decision 0512.

The failure was not arithmetic. It was inferring a *geometric* consequence from
an *addressing* change — "a quad lattice is more regular than a triangular one,
therefore less distorted" — which is plausible, sounds like reasoning, and is
wrong. The existing standing rule says to verify claims about generated output
and tool behaviour by running the command. This campaign extends it: **a claim
about a mathematical consequence needs the function evaluated, not the parameters
inspected.** Printing the ratio takes seconds. Reasoning about it produced a
confident wrong answer that would have degraded the world.

Two of the campaign's four distortion figures moved under the same discipline
once it was applied.

## A new contamination shape: the right command, the wrong tree

**I grepped The Legend's worktree, confirmed a field existed, and wrote it into
my specification as a fact about mine.** Verified false three ways afterwards.
The claim was true — of another campaign's branch.

This is a distinct failure from the family already recorded (a claim running
ahead of its evidence). Here the evidence was real, the command was correct, and
the *subject* was wrong. Parallel worktrees make it cheap to make: several
checkouts of the same repository sit side by side, and a shell whose working
directory resets between calls will happily answer about whichever one it is
standing in.

The remedy is mechanical and worth adopting: **when a grep establishes a fact
that will be written down, print the path it searched alongside the result.** A
bare match is indistinguishable from a match in the wrong tree.

The absorption later brought those fields in for real, which is a tidy ending
and not a vindication.

## The gate that reported a truncated list and read like a complete one

`make game-check` is seven make recipe lines, two of which invoke the test
runner — one per client manifest. Make aborts at the first failing line, so
while the core suite was red **the bin suite never ran at all**. I read one
failure and five passes and told Nathan the client failures had gone from five
to one. Fixing the one surfaced five more, four of them pre-existing and
invisible for the whole campaign.

The green case is trustworthy, because every line ran. Only the **red** case
misleads — which is the worst possible way round, because red is exactly when
anyone is counting. This is the observing-tool shape again: the gate answers
"what failed before I stopped", not "what fails", and nothing in its output
distinguishes those. The comparison worth making is `subfloor-run-chunked`,
which reports all three chunks rather than stopping at the first.

Unaudited siblings with the same structure: `vessel-check`, `world-check`,
`atlas-check`.

## Scope is part of a result

For most of the campaign every failure count I reported was **workspace-only**,
because `clients/` sits outside the cargo workspace and has its own gate. The
client suite was red the entire time. A count is not a fact about the tree unless
its scope is stated with it, and "the tests pass" is a sentence with a missing
argument in a repository that has four independent gates.

## The atomic commit, as a legitimate pattern with a price

Three tasks could not be ordered so that every intermediate state compiled: the
rhumb cannot survive the base-geometry change, and the movement verb cannot lose
the rhumb before eight-connectivity exists to replace it. Since bypassing the
pre-commit hook is forbidden without exception, "commit red and fix forward" was
not available.

So the group landed as one commit of 121 paths, deliberately, with the cost
quantified before accepting it rather than discovered afterwards. Recording it
because the reflex is to treat a large commit as a process failure, and sometimes
it is the only arrangement that keeps every commit green. What makes it
defensible is that the alternative was named and priced: a shim whose only
consumer would have been the gate, for one commit's benefit.

A related note: the hook refused my own reflexive reach for the bypass flag on a
docs-only change, correctly. The guard earned its keep on its author.

## Brief the hypothesis together with its refutation

The most productive thing I did all campaign was to hand a subagent a lead
**and** the arithmetic showing the lead was insufficient. The water-width brief
carried a plausible `√2` explanation and the calculation proving it could not
flip the verdict. The agent refuted it on two independent grounds and found the
real cause by instrumenting the code — a test asserting on the width of the
reach it was transecting while the gate prices whichever channel each room
independently wins.

A brief carrying only the plausible lead invites a fix that cannot work, and it
arrives looking finished. This generalizes past this campaign and belongs in
every debugging brief that has a favourite theory.

## Subagents beat my briefs three times, in the same direction

- I specified `cell_delta` returning an `Option`; the implementer made it
  **total**, which converts the widening into a compile error at all eight call
  sites. My own ruling's complaint had been that the widening was silent — the
  implementer solved the problem my ruling only policed.
- I named a bounded seed search as the preferred fixture repair. The implementer
  **measured** it (~36.6 s against 4–5 s per test), took the fallback my brief
  allowed, and paid for the pinned literal with a loud premise assertion naming
  the seed and its cause.
- I asked for one agreement test. The implementer added a **roster** check as
  well, on the correct reasoning that the drifted constant might not be the only
  one restating the walk depth absolutely — closing the class rather than the
  instance.

The common factor: each brief stated a *property* to achieve and left the
mechanism open. Where I specified the mechanism instead, I was the weakest link
in the chain. Keep prescribing properties.

## I found the symptom, routed around it, and left the cause

The worst finding of the campaign was mine, and I had already half-seen it.
Mid-campaign I measured that `heading_rose`'s greedy matching disagreed with a
bearing-derived cost on 1,216 of 4,800 samples (25.3%), and I used that
measurement correctly — to reject keying movement cost on the compass word. Then
I stopped. The measurement was evidence about the *assignment rule*, and I read
it only as evidence about the *thing I was deciding*.

The cause sat there for the rest of the campaign and shipped into the final
review, where an adversarial reader found that `go e` walks west at 0.82% of the
walk band. Reproduced through the shipped CLI in three commands.

**The rule this suggests:** when a measurement surprises you on the way to a
different decision, it has told you two things — one about your decision and one
about the thing you measured. Write the second one down as its own finding
before you move on, even when your immediate decision is settled. A number that
was interesting enough to change your mind is interesting enough to explain.

**And the deeper one, which the review stated better than I can:** the campaign
replaced a 45° bucket rule that was wrong by at most 22.5° by construction but
sometimes emitted a duplicated letter. It traded a **visible** inconsistency for
an **invisible** one, and measured only the first. Ask, of any repair: what did
the old thing guarantee that the new thing does not? Greedy guaranteed
cardinality and the bucket rule guaranteed a bound. Losing a guarantee is not
visible in a diff, and nothing in the suite was watching the one that was lost —
every assertion checked that each neighbour got exactly one word, which is the
one property greedy can never fail.

## Cardinality is not accuracy, and six findings shared that shape

The compass defect is the sixth instance in this campaign of a test measuring the
neighbouring question:

| what was measured | what mattered |
|---|---|
| every neighbour gets one word | the word points the right way |
| the width of the reach being transected | the width of the channel the gate prices |
| what failed before the recipe aborted | what fails |
| 31 placed rooms | 31 cells (they coincided) |
| a figure's named constant | the mesh the figure was keyed to |
| the count in a printout | the count in the tree |

None was findable by mutation, because in each case the code was doing what the
test said. Naming the shape per-task in a brief is what made the later ones
cheap, and it is the single practice from this campaign most worth keeping.

## Where my own rulings were too narrow

Ruling C covered every call site of the function I was changing. It missed that
the function *it* called had five more consumers, which would have silently
admitted corner-cutting diagonals. **Auditing the function you are changing is
not auditing the function you are changing it for.** The correction arrived from
a reviewer, not from me.

## Counting

Seven counting errors in campaign prose: eight call sites reported as seven,
eleven files as one, seven sites as five, nine as "twelve of eighteen", ten as
nine, face 16 as 14, 49.6% as "three fifths". No single one was load-bearing.
The rate is the finding, and the cheap fix is to let the command that produced
the number also produce the sentence.

## The ledger convention arrived mid-campaign, and the gate is what noticed

[The Cartulary](../../book/src/chronicle/the-cartulary.md) landed while this
campaign was executing, requiring a committed ledger for any campaign with a
spec and a plan. This campaign had forty-four rulings in git-ignored,
per-worktree scratch — precisely the material that convention exists to stop
losing, and precisely the five-times-recorded loss it cites.

The absorption's first commit was refused by the freshness check until the ledger
existed. That is the right moment for a new convention to bind: not when it
merges, but when a branch absorbs it. Worth knowing that a mid-campaign
absorption can hand you a new obligation, and that the obligation was worth
paying — the ledger is 20 K of rulings that would otherwise have died with the
worktree.

## A ratio is two numbers, and I checked one of them

Fix round 1 reported that a bound had improved from 1.1051 to 0.7487 grid
spacings. I relayed that to Nathan as a real improvement. It was not: the fix had
switched the *divisor* from the icosphere's edge to the cube's facet arc, and
`π/2 ÷ acos(1/√5) = 1.4188`. Same physical error, larger unit.

The consequence was worse than a wrong sentence. A live threshold of 1.5 came to
admit **2.13 real spacings** while its own doc still claimed it excluded a second
one — a 42% loosening of a bound, invisible in a diff because the constant's
value never changed.

**The lesson is not "check your units", which nobody disagrees with.** It is that
a *reported improvement* deserves the same scrutiny as a reported failure, and
gets far less, because it arrives as good news. Every review instinct in this
campaign fired correctly on red results and passed straight over a green one. The
question that would have caught it in one line: *did the numerator change, or did
the denominator?*

I had written this exact shape into my own notes — evaluate the curve, not the
constant; verify the proposition, not a cheaper neighbour — a few hours earlier,
and then walked into it. Writing a lesson down is not the same as holding it, and
the gap between those two is where a checklist earns its keep over a memory.

## The trilemma nobody had named

Three campaigns moved around the same three-way tradeoff without stating it:

- **Bijection** — every neighbour gets exactly one compass word, every word names
  at most one neighbour.
- **Bounded per-room accuracy** — no word points more than some stated angle away
  from where it goes.
- **Invertibility** — out and back returns you.

You can have any two. The 45°-bucket rule took accuracy and invertibility and
emitted duplicate letters. Greedy took bijection and invertibility and produced a
156° lie. The optimal assignment takes bijection and accuracy and gives up 1.55%
of round trips.

Each campaign fixed the leg that was visibly broken and silently gave up a
different one, because **nothing named the third leg**, so nothing measured it on
the way past. The general form: when a repair trades one property for another,
the property being surrendered is usually the one with no assertion — that is
precisely why it was available to surrender. Ask, of any fix: *what did the old
thing guarantee that the new thing does not?* A lost guarantee never appears in a
diff.

## Two things left for someone else

- **A flake nobody owns.** `repertory_corpus::no_scene_has_fallen_below_its_
  recorded_floor` fails under full-suite load and passes in isolation,
  reproduced on a stashed clean tree; two full runs of mine had it green. A
  flaky red in the merge queue's gate phase is indistinguishable from a real
  one.
- **The decision index is 21% incomplete and nothing checks it.**
  `docs/decisions/README.md` indexes 226 of 287 records, and `main` is missing
  the same 61, so no branch caused it. The generated in-force index is complete
  and correct; only the hand-maintained entry point drifts. Since the project's
  own instructions send every session to grep that tree before relitigating, a
  decision nobody can find reads as a decision nobody made — which is the exact
  failure this campaign spent its first day undoing.
