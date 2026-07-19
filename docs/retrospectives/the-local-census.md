# Retrospective — The Local Census

One page of process, not product. The product is chronicled; this is what the
close learned that the code does not record.

## What went well

- **The A/B generation diff was the right proof, and the metamorphic guard
  alone would have been the wrong one.** The census path is watched by a
  metamorphic test (depth-scoped metrics must equal a forced-full build,
  byte-for-byte) — and it passed on the very first build of the fix. But that
  guard proves the refactor is *self-consistent*, not that it *preserved the
  pre-change values*: it compares the new code against the new code at two
  depths. The decisive evidence was generating the same eight seeds on clean
  main and on the branch and diffing the two tables directly — byte-identical.
  For a performance refactor whose entire promise is "same output, less work,"
  the before/after value diff is the gold standard; the internal invariant is
  necessary but not sufficient, and it is easy to mistake one for the other.
- **The census-probe "drift" was correctly read as pre-existing, not caused.**
  Running the probe reported the regenerated rows differing from the committed
  goldens — the kind of red result that invites a panicked hunt for the bug
  just introduced. But the committed census fixtures lag main by design (they
  refresh once per campaign on the cloud box, and main was twenty-six commits
  past that), so the goldens were already stale before this campaign touched
  anything. The A/B against clean main settled it in one step: same drift with
  and without the change means the change is innocent. The lesson is to reach
  for the branch-relative comparison, not the golden-relative one, whenever
  the goldens are known to lag.
- **Ideonomy's tree-finding earned its keep as a completeness check, not a
  ritual.** The scoping Explore named two metric extractors as the cost; the
  exhaustion sweep the tree-finding pass demanded found a *third* observation
  site the two-extractor summary had folded over. The fix's target set was
  proven complete by grep before a line was rewritten, rather than discovered
  incomplete after.

## What the campaign taught mid-execution

- **A scouting agent's cost breakdown is a hypothesis to measure, not a fact
  to build on — in both directions.** The Explore was directionally right and
  quantitatively wrong twice. Its headline correction was invaluable: the cost
  was ~190 sculpts per world, not the ~20 the task premise assumed, and
  concentrated in one settlement loop. But its *post-fix* estimate (~55
  CPU-s/world, from collapsing every source rebuild) was optimistic — the real
  floor is ~105. Both numbers were plausible and only one survived contact
  with the profiler. The fix direction came from the map; the fix *scope* had
  to come from the measurement.
- **The second optimization was built, measured, and reverted — and that is a
  success, not waste.** The obvious next step after threading the climate was
  to build the phenomena sources once per world instead of once per settlement.
  It was implemented in full (a public `WorldPhenomena` handle with two
  methods), and it bought exactly nothing: the sculpt had been the whole cost,
  and the source assembly around it was already cheap. Reverting it — rather
  than keeping a clean-looking abstraction that widened the public API for no
  measured benefit — is what kept the campaign's number honest. The rule the
  close reaffirmed: measure the second win before you commit to it, and let the
  measurement, not the aesthetics, decide whether it ships.
- **The stage-boundary absorption cadence was honored and cheap.** Main had
  moved twenty-six commits since the session began; fast-forwarding before
  branching, and confirming the cost map still held on the fresh tip, cost one
  command and avoided scoping against stale code. The campaign branched from a
  current main and never had to reconcile a divergence at close.

## Followups (register)

1. **Metric-purity lint** — a check that fails when a lab metric calls the
   sculpting entry points (`climate_of` / `terrain_of`) directly instead of
   reading the view's pre-built `climate()` / `terrain()`. The cross-domain
   reading of this campaign's own bug (the database N+1 query): the durable fix
   is not just to batch the work once but to make the batched path the only
   ergonomic one, so the redundancy cannot regrow the next time a metric is
   authored. Named here rather than shipped, because a lint is its own small
   campaign and the abstraction that would have carried it (the reverted
   handle) earned no other keep.
2. **Refresh the census goldens locally** — once the AWS-drop decision lands,
   the fixtures that lag main can regenerate on the authoring machine, closing
   the change-to-confirmation gap this campaign's whole point was to eliminate.

## Confidence Gradient

Checked `book/src/open-questions.md` for a moved bet, grepping this campaign's
vocabulary (`census`, `AWS`, `regen`, `author-time`, `laboratory`, `metric`,
`local`). The hits are about drift-checking numbers being *correct* (a wrong
number drift-checks green forever) and the census studies as evidence — not
about the *cost* or *location* of running them. No row in that chapter makes a
bet on where the census runs or what it costs; the Confidence Gradient is
scoped to world-generation research bets, and this campaign's payoff — moving
a computation from a rented machine to the local one, byte-identically — sits
beneath that chapter's bookkeeping, not inside it. The idea registry is the
correct home for the movement, and it was updated: **PERF-lab-metric-rebuilds**
now records shipped.
