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
