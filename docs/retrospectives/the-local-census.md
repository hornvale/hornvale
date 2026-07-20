# Retrospective — The Local Census

One page of process, not product. The product is chronicled; this is what the
close learned that the code does not record.

## The spine: measure, never infer

The campaign's one transferable lesson is that it nearly stopped nine-tenths
short, and what saved it was refusing to trust its own reasoning about where
the time went.

- **The first fix looked like the win, and was a tenth of it.** Threading the
  view's climate into the name-gloss metric cut the census 2.7×, byte-
  identical, clean story, natural stopping point. A flamegraph taken *after*
  it showed the terrain sculpt was still 91% of the cost — the fix had
  unmasked, not removed, the sculpting. Had the campaign shipped at 2.7% it
  would have left a 49× win on the table, and every reason to stop looked
  sound. The only thing that revealed the truth was the profile.
- **A source grep structurally could not find the remaining sculpts.** They
  were inside worldgen readouts the metrics called *by name* — `lexicon_of`,
  `accounts_of`, `demography_report` — each of which sculpts internally. A
  scan of `metrics.rs` sees a function call, not a terrain pipeline three
  frames down. The scoping Explore, working from source, had confidently
  reported the language metrics "do not rebuild terrain"; they were the single
  largest remaining source. Only the flamegraph, which follows the call graph
  into the callee, could see it.
- **The one time the campaign inferred instead of measuring, it was wrong —
  and the miss is instructive.** After the first fix, a build-sources-once
  optimization was reasoned out from first principles (the sources were
  rebuilt per settlement; collapse them), implemented in full, and measured to
  buy *nothing* — the sculpt had been the entire cost and the assembly around
  it was already cheap. It was reverted. That null result, three fixes before
  the profile-driven loop hit its stride, was the campaign's own proof that
  the reasoning it trusted was unreliable and the profiler was not.

## What the profile-driven loop taught

- **The loop converged because each round's proof was cheap and total.**
  Every fix was verified by regenerating the same eight seeds on a clean build
  of the unchanged code and diffing the two tables — byte-for-byte, the
  strongest possible statement that a performance change preserved behavior.
  Because that check was one command and unambiguous, the loop could run six
  times without accumulating doubt. The metamorphic depth-ladder guard, which
  passed on the first build of every fix, was *necessary but not sufficient*:
  it proves a refactor is self-consistent, not that it preserved the pre-
  change values. The clean-main A/B is the check that actually pins behavior.
- **The tail crossed a boundary, and naming the boundary was the decision.**
  After five fixes the profile showed the remaining sculpt was no longer in a
  lab readout — it was inside genesis, which builds every world for `new` and
  the almanac too, and whose deeper reaches ("Fix D") change the composition
  root's public signature. That is a different, riskier campaign wearing the
  same idiom, and the close surfaced it as a choice rather than grinding into
  it. The one genesis-internal re-sculpt that was byte-identical *in place*
  (naming the world, reusing the globe genesis already keeps) was taken; the
  signature-changing one was deferred. Knowing which side of the line a fix
  sits on — and that the line existed — was worth more than the fix.
- **The headline was re-measured at low load before it was believed.** The
  early baseline (2281s) was taken while other work ran; before writing "49×"
  anywhere, main was re-timed at idle. It came back 2272s — the original was
  honest — but the campaign did not know that until it checked, and a
  performance retrospective that quotes a number it measured under contention
  is quoting noise.

## Byte-identity across a genesis crossing

The A/B protocol scaled unchanged through five lab fixes; the sixth touched
genesis, and there the census A/B was not enough on its own. A genesis change
had to also leave the frozen seed-42 world, its almanac, and the committed
gallery identical — a wider regen-and-diff than the lab fixes needed. It did,
because the reused globe is provably the one genesis had already sculpted and
kept (The Single Sculpt's own invariant), but the campaign ran the wider check
rather than reasoning that it would pass. The rule generalizes: a fix's
verification surface is set by what it touches, not by which campaign it
belongs to, and a fix that reaches into genesis inherits genesis's fixtures.

## Followups (register)

1. **Metric-purity lint** — fail when a lab metric (or a worldgen readout a
   metric calls) reaches a sculpting entry point (`climate_of`/`terrain_of`/
   `exposure_of`) rather than a threaded `_from` variant. The whole campaign
   was one class of bug re-appearing in nine places; a lint makes the tenth
   impossible instead of waiting for the next flamegraph to find it.
2. **Genesis-perf campaign** — "Fix D" (the view chain re-sculpts terrain
   genesis already built and discarded, ~37% of the post-campaign build) plus
   any further genesis-internal re-sculpts a profile of `new` surfaces.
   Changes `build_world_to` to return its terrain/climate; touches every
   genesis caller; helps all of `new`/almanac/census. Its own scoping + G3.
3. **Refresh the census goldens locally** — once the AWS-drop decision lands,
   the fixtures that lag main regenerate on the authoring machine in ~5
   minutes, closing the change-to-confirmation gap the campaign existed to
   eliminate.

## Confidence Gradient

Checked `book/src/open-questions.md` for a moved bet (grepping `census`, `AWS`,
`regen`, `author-time`, `laboratory`, `metric`, `local`, `flamegraph`). The
hits concern the *correctness* of census numbers (a wrong number drift-checks
green forever) and the studies as evidence — not the cost or location of
running them. No row bets on where the census runs or what it costs; the
Confidence Gradient is scoped to world-generation research bets, and this
campaign's payoff — moving a computation from a rented machine to the local
one, byte-identically, fifty times faster — sits beneath that chapter's
bookkeeping. The idea registry is the correct home, and
**PERF-lab-metric-rebuilds** records shipped.
