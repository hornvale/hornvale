# Retrospective — The Wearing

One page of process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-wearing.md); the shipped
mechanism is nineteen toponymic concepts, a per-culture name shape, a nucleus
template set, frequency-keyed wear, and a render-time qualifier.

## What went wrong, and what would have prevented it

- **The stage-boundary absorption cadence was missed, and it cost the
  campaign's most expensive artifact twice.** `main` was absorbed twice
  (`c17b9f8a`, `166d4ad9`), both in the campaign's first half. The last three
  stages and the whole of Task 11 ran without one, so the close had to swallow
  **174 commits** in a single merge with 29 conflicts. Among them were ~5,500
  new lines in the module that decides settlement placement, and therefore
  every generated name. Two authorized census regens — roughly 35 minutes of
  canonical-box time, taken under an explicit owner carve-out — were
  invalidated before they could be merged. The rule that would have prevented
  it is not "absorb more"; it is **the census regen is the last act before
  merge, after the final absorb.** It is the most expensive artifact the
  project has and the most sensitive to other people's work, and any ordering
  that puts an absorb after it is spending it on a world that is about to
  change. The debt this left is recorded as F11, spelled with a greppable
  `stale-census:` token whose reason strings a test holds verbatim.

- **The campaign's signature defect was a comment asserting a property the
  code lacks — at least twelve instances, counted.** Every stage found at
  least one, in code, in doc comments, in reports, and in the decision record:
  a `#[test]` doc naming a bound the test did not enforce, a function doc
  claiming a consumption the function skips on one branch, an error path
  described as handled that was a panic, a "worst case is bounded, +18"
  followed two lines later by "+8 to +23". The instructive one is
  **ledger #10**, where the controller wrote an unhedged overclaim into the
  decision ledger *while dispatching agents to hunt exactly that defect out of
  the code*. Reviewing for a defect class does not immunize the reviewer
  against it, and prose in a decision record is the least-checked surface in
  the repository. The mitigation that actually worked was mechanical: every
  measured claim carries the command that produced it, and reviewers were
  asked to re-derive rather than to read.

- **Three plan-authored acceptance tests were vacuous, and one plan snippet
  did not compile.** Task 2's test passed with its feature hardcoded off (its
  fixture drew a phonology in which the carve was never in play). Task 4's
  exposure gate was unreachable on every seed. Task 7's test passed
  unmodified on the pre-change tree. Task 2's brief snippet did not compile
  against the signature it targeted. **Code blocks inside an implementation
  plan are the one kind of code in this project that nothing checks** — not
  `cargo check`, not clippy, not the gate — and they arrive carrying the
  authority of an approved plan. The practice that caught all four was making
  implementers *run the plan's test on the pre-change tree first and report
  the result*; two of them reported "it already passes", which is the finding.

- **A metric pinned only against a fixture cannot detect that it has gone
  stale against the code.** `name-gloss-true` read `false` at all four probed
  seeds because Task 7 introduced a three-concept name shape and the metric's
  accept set enumerated singles and ordered pairs. Its calibration row read the
  *committed census fixture*, where the flag was still true, and stayed green.
  It was found by probing the live registry, not by any test, and it would
  otherwise have surfaced as an unexplained red immediately after a
  seventeen-minute census. Every metric whose calibration reads a fixture has
  this exposure; the cheap countermeasure is to re-measure the *invariant*
  rows live before a regen rather than trusting the fixture through it.

- **A deferred debt was mislabelled twice, and its discharge recipe was
  wrong.** The campaign declared up front that byte-identity dies at Task 1
  and that "fixture-pinned name assertions will fail from Task 1 onward —
  Task 11 re-pins." Task 11 did not re-pin them, and by the close the same
  31 rows were being described in two task reports as "the long-standing
  inherited artifact/golden debt this campaign did not create." They are the
  campaign's own drift: the first divergence in the world-identity fixture is
  the concept `coast`, registered by this campaign's Task 3. Followup F11
  then prescribed the wrong command — it says they "clear with
  `bash scripts/regenerate-artifacts.sh`". Run at the close, that cleared
  **one of thirty-two** (the vessel transcript, which reads a book gallery
  page). The other 31 read `<crate>/tests/fixtures/` keystone goldens, which
  take `make rebaseline-goldens` (and which covers only five of them), plus
  the offline `hornvale voice` pass for the audio set. **A deferred item is
  only as good as its discharge recipe, and nobody had run the recipe.** The
  general lesson is the campaign's own signature defect in a new place: a
  followup register is prose, and prose asserting that a command clears a
  failure is a claim like any other — run it once, at deferral time, and
  write down what it actually left behind.

- **A G3-approved non-negotiable was amended mid-execution.** The spec listed
  world-level pin isolation for names among the determinism obligations
  "unchanged and non-negotiable"; keying wear to a morpheme's frequency in its
  culture's own name corpus makes it false. That was an owner call
  (ledger #10), taken with the magnitude measured across seven seeds rather
  than argued. Recorded here because a clause approved at a gate and
  contradicted during execution should leave a trace at the close, not only in
  a ledger.

## What worked

- **Mutation-testing the deliverable caught what review-by-reading did not,
  repeatedly.** Three of the four defects above were found by *breaking the
  feature and checking the test noticed* — never by reading. This became the
  standing ask, and by the later tasks implementers were running their own
  batteries and reporting the survivors instead of the successes. Two Task 9
  mutations survived their first battery and both changed production output;
  they got new tests rather than a footnote.

- **Measuring before implementing.** Task 8's attribution separated the fix
  (−1.29 characters, winning at all four seeds) from the reseed it rode on
  (+0.67 mean, and at one seed swamping the fix into a net regression) with a
  consumption-identical control. Task 7 measured a calibration crossover into
  place and recorded the value it rejected. Task 9 verified there was no
  reseed to separate rather than asserting it. In every case the flattering
  version of the number was the wrong one.

- **The investigation that declined to ship.** Asked whether 2% wear survival
  was the phenomenon or the guard, Task 8b built the preferred alternative
  rule, measured it agreeing with the shipped rule on all 690 production
  decisions, **changed nothing**, and reported that the bottleneck was one rung
  further up. Its recommendation — keep the registry row at `in-progress`, and
  publish the funnel rather than the bare 2% — was endorsed and is what the
  chronicle does.

## Confidence Gradient

**Re-scored: "Refinement at scale"** (`book/src/open-questions.md`, the
genuinely-open tier). The grep of the campaign's territory found no
language-, lexicon-, phonology- or toponymy-specific bet anywhere in the
chapter (`lexicon`, `phonolog`, `toponym`, `onomast`, `etymolog`, `collision`,
`transparen` all return zero hits; `naming` returns two, neither a bet). The
only row in scope is bet 1, whose *aesthetic quality* half the chapter called
"the half that is years away". This campaign decomposed one instance of it and
found three self-scorable readings inside — length, syllable count, and
transparency-as-a-distribution — leaving a smaller taste residue, which is the
template the section's own preamble predicts. The re-score also carries the
campaign's sharper caution: an instrument only scores a bet if something reads
it, and this one was green and ignored for several campaigns.

## Follow-ups

Carried from `.superpowers/sdd/followups.md` (F1–F11), unresolved at close:

| | |
|---|---|
| **F1** | ~~Four idea-registry rows cite~~ **CLOSED 2026-07-29.** One row (`MAP-31b`), not four, cited `WeightedCategorical::reduce(β)` as shipped machinery; no such type exists. Corrected to name `Stream::weighted_index` under `schemas::select_schema`, and to say outright that the combinator is *proposed, not shipped*. The three-row overcount was itself an unverified claim — `git log -S` shows LANG-37's citation was already removed by the row-compaction pass, and UNI-28 and LANG-38 never carried one. |
| **F2** | ~~Discharged in-campaign~~ **CLOSED 2026-07-29, but not by the in-campaign edit.** Task 5's fix was correct and then went stale a *second* time: the close merge brought The Toponym's characteristic climate variant into `settlement_site_concepts`, making the vector twelve sources, not eleven. Doc corrected again; the assertion was never at risk (it calls the composition root). |
| **F3** | **STILL OPEN, restated.** Blocked on F11 — the committed census is `main`'s and contains no name this campaign generated. Two claims corrected on the committed `rows.csv`: overflow is 88.5% / 86.6% of present rows, not "every one of the 1000"; and the overflow row *is* already visible in the summary (`>= 10 · 679 · 67.9%`). The real gap is that nothing reads it. Readout after the regen is named in the register. |
| **F4** | **CLOSED 2026-07-29 — its condition did not fire.** The nucleus fix sufficed, and the evidence is now structural rather than a four-seed sample: `no_language_requires_a_diphthong_in_every_syllable` (400 drawn phonologies) and `a_diphthong_admitting_language_still_speaks_simple_syllables` (share bounded 0.3–0.7) both pass on the merged tree, making the ≥95%-two-vowel condition unreachable by construction. The onset draw is untouched, and touching it now costs `phonotactics/v2` under 0086. |
| **F5** | `Hydro::Spring` and `Hydro::Aquifer` are unreachable in `hydrogeology` on every seed — a terrain-domain branch-order bug found from the language side. |
| **F6** | Adding a fifth people *reduced* seed 42's settlement count by 34. Entirely `main`'s behaviour; handed to the placement campaign as a measurement. |
| **F7** | A leading `Tonogenesis` is provably the identity, appears in 8 of 20 production wear cascades, leads in 7, and **is the entire cascade in 3** — the lever on the name cycle's opacification phase. |
| **F8** | **CLOSED 2026-07-29 by decision [0086](../decisions/0086-an-epoch-freezes-when-it-can-be-stamped-on-a-saved-world.md)** — a new record refining 0006, not an amendment (the log's own rule forbids editing an Accepted record's substance, and 0041/0044/0083/0084 are the house form for a refinement). The rule is sharpened in the record: Task 7's "with fixtures regenerated" clause is wrong, because F11 landed the `/v3` code *without* the regen. The freeze test is mechanical — a world saved off `main` carries `language/<species>/name/settlement: v3` in `derived_under`, so `/v3` is frozen and the next consumption change owes `/v4`. Both misleading doc comments (`glossed_name`, `draw_phonotactics`) corrected in place. |
| **F9** | Two compounding layers stack (lexicon recipe × name shape); shortening syllables cannot reach the multiplier. |
| **F10** | A *relative* position rung (`the northern Roa`) would separate wherever the coordinate does, read as toponymy, and cost less — and Task 3 already registered the vocabulary. |
| **F11** | The deferred census: 35 `#[ignore]`d rows (20 + 7 + 3 fixture-blocked, 5 live seed pins) and two commented `golden-pins.sql` blocks, greppable as `stale-census:`. **Do not discharge by absorbing `main` first and regenerating after** — that is the loop that killed both campaign regens. |
| **F12** | *(New, at close.)* **CLOSED 2026-07-29 — discharged by the keystone refreeze, not deferred.** The 31 standing golden/artifact failures are the campaign's own Task-1 fixture drift, not inherited debt, and F11's recipe for them is wrong. `regenerate-artifacts.sh` clears one; `make rebaseline-goldens` reaches five more; `hornvale-book` (14), the `the_first_mark` battery (5), `doctrine`/`explanations`/`solitary_tongue` (4) and `audio_artifacts` (which needs the offline `hornvale voice` pass) each need their own. Accepting them is a deliberate world-identity re-baseline and wants the owner's eye, not a drive-by at the close. — *The multi-route breakdown was right and was used: commits `a29a1f29` and `dc3d4b64` took the set 30 → 0 (6 via `make rebaseline-goldens`, 1 via `regenerate-artifacts.sh`, 23 hand-re-pinned), plus an ordering constraint F12 had not found (goldens → artifacts → goldens, because `session_snapshot` holds both a `REBASELINE` golden and an artifact-reading test that aborts the recipe). No census was regenerated. F11's closing "Also red — the artifact/golden set" paragraph is therefore **stale**; what remains of F11 is the 35 `stale-census:` rows and the two `golden-pins.sql` blocks, nothing more.* |

### Post-close follow-up pass — 2026-07-29

The documentation half of this register was worked immediately after the merge,
on `followups-post-the-wearing`: **F1, F2, F4, F8 and F12 closed; F3 left open
and restated** (it is blocked on F11's regen, which is authorized and run
separately). F5, F6, F7, F9, F10 and F11 are untouched and stand as written.

Two of the five closures found the followup's own text to be wrong in the
campaign's signature way — F1 overcounted four rows where one carried the
citation, and F2 was recorded as discharged when a sibling campaign's merge had
already re-staled it. Both corrections are measured and recorded in
`.superpowers/sdd/followups-docs-report.md` and in the register entries. The
lesson is small and specific: **a followup entry is a claim, and it decays like
any other** — the ones written at a close describe a tree that the close merge
itself was in the process of changing.
