# Retrospective — The Residue (LANG-43)

One page of process, not product. The product is chronicled; this is what
the close learned that the code does not record.

## What went well

- **Grounding the spec in a research pass before writing it caught a false
  claim in the campaign's own idea-registry row before any code was
  written.** LANG-43's row asserted a frequency/usage-salience signal
  "already in the packs" — a targeted Explore-agent survey of
  `domains/language/src/` before brainstorming confirmed this was false
  (`packs.rs`'s only numeric field gates Berlin-&-Kay color acquisition,
  not word frequency). Finding this during research, not mid-implementation,
  meant the spec could design the real signal (Zipf's law of abbreviation,
  root-proto length) from the start rather than discovering the gap
  partway through a task and re-planning.
- **Hand-tracing the keystone test case against the actual `evolve()`
  implementation before writing any plan code paid off directly.** The
  core mechanism (`FinalLoss` behaving differently depending on whether a
  suffix was joined before or after evolution) was verified by reading
  `apply_final_loss`'s real source and tracing exact segment arithmetic by
  hand before it went into the plan. Every implementer subagent confirmed
  the traced values matched on first run, and the final reviewer
  independently re-derived the same arithmetic from scratch and agreed —
  the keystone mechanism had zero surprises across five separate reviews.
- **Ideonomy caught a real overturn, not just enrichment.** The
  MorphDepth-reuse question (fold Number/Tense into LANG-40's existing
  depth mechanism, or hard-code universal affixation for simplicity) was
  answered one way, then a negation pass reversed it: the None/Particle/
  Affix trichotomy is structurally generic, not inherently tied to LANG-40's
  epistemic-worldview story, so reuse with fresh weights cost about the
  same as hard-coding while correctly producing isolating-typology
  daughters for free. Recorded as ledger #3's overturn — the second time
  this session an "obvious" pre-pass answer didn't survive a pass.
- **Two implementers independently caught and fixed the same class of bug
  in the plan's own test-data generators** (a start-indexed alternation +
  conditional pad that collapses consecutive proto lengths onto the same
  actual segment count), using the identical end-indexed fix each time,
  and both fixes were independently re-verified by their task reviewers
  via hand-trace rather than trusted on report alone. The second
  implementer (Task 5) was pre-warned about the first occurrence and
  fixed its own copy before transcribing the brief — the dispatch-level
  warning worked exactly as intended.

## What was awkward

- **A registry-ID citation slipped through five task reviews and one
  whole-branch review, and only surfaced at `make gate` on the merged
  tree.** The plan's own `stream_labels()` string literals cited
  "LANG-43:" directly — invisible to every review because the generated
  book page those strings render into
  (`book/src/reference/stream-manifest-generated.md`) was *stale*
  throughout the whole review cycle (a separate, already-caught defect),
  so the citation only became live in the committed artifact once the
  whole-branch review's own fix regenerated that file. The book-purity
  drift-check (`docs_consistency`) is not part of any task-level review's
  checklist and isn't in `make gate`'s fast path either — it only runs
  during the close-time `make gate` on the fully merged, fully regenerated
  tree. This is the same class of gap as the type-audit check (also
  CI/close-time-only, also invisible to `make gate`'s fast path) that
  Task 5 found on `level_paradigm` — a second instance of "a generated
  artifact's regen is what makes a defect visible, and nothing regenerates
  it until the very end." Fixed directly by the controller rather than
  another subagent round-trip, since the fix was two words repeated six
  times.
- **The plan itself had two small, harmless arithmetic/generator slips**
  (a "7 tests" vs. actual 6 count in Task 3's step text; the length-
  collision bug in both Task 4's and Task 5's test generators). None
  affected correctness — implementers caught and reported every one — but
  it's worth noting the plan-writing pass did not itself run the generator
  logic by hand before committing to the brief text, only the CORE
  mechanism's arithmetic got that treatment. A future plan touching
  synthetic test-data construction should hand-trace the generator, not
  just the function under test.

## Follow-ups

- **LANG-46** (grammaticalization depth as a diachronic pathway across
  generations, not a fixed per-daughter snapshot) captured to the idea
  registry during brainstorm — real, deliberately out of scope, needs a
  multi-hop genealogy engine that doesn't exist yet.
- **No Book/CLI rendering surface exists yet** for Number/Tense paradigms
  or their irregular forms — a natural next step once a campaign wants
  tongues to actually speak with plurals and past tenses, but this
  campaign's own scope (spec §6) deliberately stopped at the mechanism and
  its proof.
- **Book-purity and type-audit checks should get an earlier checkpoint** in
  future plans that add `stream_labels()` entries or new `pub`-boundary
  primitives: neither is part of `make gate`'s fast tier, so both defects
  this campaign hit were only caught at the close-time full gate, not by
  any of the five task reviews or the whole-branch review that preceded it.
