# The Seedbed — decision ledger

Campaign: **The Seedbed** — a fourth corpus family measuring whether the world
*grows* known macro-regularities, seeded from Epstein & Axtell, *Growing
Artificial Societies* (1996).

Autopilot engaged (`campaign-autopilot`). G3 and G6 are hard stops.

---

## #1 [G1] — What shape does "can you grow it?" measurement take?

**Question.** Three candidate homes: (A) a fourth corpus family with its own
resolver; (B) an extension of the Domesday's `studies/expectations.json` +
`windows/lab/src/domesday/detect.rs`; (C) additional lab metrics only.

**Decision.** (A), a fourth family, following decision 0135's template — its
own directory, resolver, verdict vocabulary and artifacts — plus one thing
0135's family did not need: a regression guard.

**Why (precedent cited).** Decision 0135 opens a family when the *resolution
basis* differs: `tropes/` resolves against the concept registry, `systems/`
against repository facts, `sentences/` against the grammar. A macro-regularity
resolves against **measurement over the committed census**, which is a fourth
basis. 0135 explicitly accepts duplicated loader/renderer/ratchet across
families that will never share code.

**Alternatives discarded.**

- (B) rejected on provenance. Ideonomic negation of "frozen *external*
  catalogue" lands precisely on `expectations.json`: 30 claims Hornvale makes
  *about itself*, checked for internal coherence. A growth corpus imports a
  regularity from outside and asks whether the world reproduces it. Same
  machinery, opposite provenance — 0095's instrument/standard distinction makes
  them complements, not competitors. Folding one into the other would give
  `expectations.json` a provenance field it has no business carrying.
- (C) rejected on the whole precedent chain 0330 / 0577 / 0581 / 0583: a number
  recorded with no claim attached cannot be wrong, and every family has been
  forced to add a demonstration after shipping without one. `rank-size-slope`
  is the live instance — see #3.

**Ideonomy: 1 pass (negation + organon-construction/list; reversibility,
naturalness, direction), 0 overturns of the top-line, 3 material design
changes:**

1. (B) is the *negation* of (A), so the spec states the division of labour
   rather than arguing (B) down.
2. **Reversibility asymmetry.** A built parser stays built; a *grown*
   regularity dies the moment the history bake is retuned. Coverage in the
   other three families is a ratchet; here it can silently decay. This family
   needs a two-way regression guard the others never needed.
3. **The deferred set is principled.** On the direction axis the census records
   one steady endpoint per world, while the book's regularities are
   *oscillating* (migration waves, boom–bust) and *accumulating* (wealth
   concentration). The census is structurally blind to exactly those two, so
   trajectory items are deferred-with-anchor rather than omitted.

**Capture actions.** Discards routed in #4.

---

## #2 [Q] — What ultimately verifies a corpus verdict?

**Question.** Nathan, unprompted, during brainstorming: *"The eventual intended
form of verification of these corpuses is the documentation Hornvale provides
about its own capabilities. When you think about it, nothing else really
suffices."*

**Decision.** Adopted, with one restriction that is load-bearing: the anchor is
**generated, drift-checked** documentation, never hand-written prose. Mechanized
as a new anchor kind, `doc:<path>`, which resolves only if
`docs/generated-paths.txt` gives that path a generator — its second column names
the path's author, and `none(hand-written prose, never regenerated)` is refused.

**Why (precedent cited).** The directive extrapolates a trend the project has
already walked five steps of, each step forced by the previous one being
falsified: registry-token naming (0577) → hand-maintained declaration list
(0330) → `path:` anchor → `test:` anchor (defeated by `#[ignore]`) →
realization witness (0581/0583). Steps 1–5 are the program certifying itself to
itself; generated documentation is the first surface a reader outside the loop
can falsify. `windows/explain` is the existing instance of the principle: it
narrates by reading only the ledger, "which is how it validates that the ledger
is self-describing."

**Alternatives discarded.** Hand-written book prose as an anchor — rejected as a
restatement of the `IMPLEMENTED_DEMANDS` failure 0330 names: a declaration that
moves the score without moving the world.

**Ideonomy: 1 pass (tree-finding + timeline organon; complexity, symmetry), 0
overturns, 2 design changes:**

1. **Arity picks the surface.** A growth regularity is a property of a
   *population* of worlds; `explain`/almanac narrate *one*. A within-world claim
   can be narrated per-world; a cross-world claim belongs in the Domesday, which
   is already generated prose over the census. Two documentation homes, chosen
   by the arity of the claim.
2. **The honest bound, stated in the spec so nobody later mistakes it for a
   second witness.** A passage generated from the same census the verdict is
   computed from is the same fact rendered twice: it buys *reader-
   falsifiability*, not independent evidence. Its teeth therefore depend on the
   passage carrying the **criterion and the verdict**, not just the number —
   which is a change to the Domesday renderer, not merely a new anchor kind.

**Capture actions.** Generalizing `doc:` to the other three families is a
cross-family change this campaign will not make silently — `growths/`
demonstrates it first; the generalization goes to G3 flagged and, if approved,
to a decision record of its own.

---

## Follow-ups

*(populated as they occur)*

---

## #3 [G2] — Design self-review

**Naming.** `regularities/`, not `growths/`. The three existing directories name
their *unit* — a trope, a system, a sentence — and the unit here is a
regularity; "a growth" is not idiomatic for the thing an item is. File suffix
`.regularity.json`, matching `.trope.json` / `.system.json`.

**Verdict vocabulary is six-valued, not five.** `grown` / `flat` / `refused` /
`deferred` / `absent` / `inapplicable`. 0135 grants each family its own
vocabulary; 0136's refused/deferred/absent triple is preserved intact. The
addition is `flat` — measured, criterion unmet — which the other three families
have no way to express, because a grammar either parses or does not. A
generative test's most valuable output is the measured miss, and folding it into
`absent` would discard the finding.

**External-claim verification, run at drafting time rather than asserted**
(autopilot's standing check, and its imperative-mood corollary):

| claim in the spec | command that settled it |
| --- | --- |
| wave one needs no new metrics | `head -1 .../the-census/rows.csv \| tr , '\n' \| grep -E "settle\|raid\|tribute\|..."` — returns `rank-size-slope`, `settlement-count`, `mean-population`, `total-population`, `raid-victim-rate`, `raid-initiator-rate`, `climate-displacement-events`, `tribute-relations-standing`, `granary-raid-phase-concentration`, `cascade-rules-fired-*` |
| the census is 1,000 worlds, terminal state | `wc -l` = 1001; `studies/the-census.study.json` keys are `seeds`/`pin_sets`/`metrics`, no time axis |
| the Domesday is generated prose | `head book/src/domesday/demography.md` → "GENERATED FILE — do not edit. Regenerate with `hornvale lab domesday`" |
| `generated-paths.txt`'s 2nd column names an author | read the file: values `artifacts`, `census`, `heavy`, `none(...)` |
| existing anchor kinds | `cli/src/systems.rs` parse arm: decision / registry / test / path / reason. No `doc:` — it is genuinely new |
| population never degrades capacity | `eff_capacity` = `caps_now()[pidx].at(vertex) * factor(era, vertex)`; no occupation term |
| Sugarscape's rule roster | `pdftotext` of the source, Appendix B read in full |

**Not verified, and deliberately left as a branch table rather than a
prediction:** the regression guard's runtime, and therefore whether it belongs
in `gate-commit` or the stage gate. Spec §6 enumerates both responses instead of
guessing one.

**Ideonomy: no separate pass.** This entry records self-review of decisions
already taken under passes in #1 and #2; no new option space was opened.

---

## Follow-ups

- **F1 — `docs/audits/campaign-reconciliation.tsv`'s `ledgers` column cannot be
  populated by any row.** `campaign_reconciliation_covers_every_campaign_record`
  requires every path the TSV names to lie in `CAMPAIGN_RECORD_DIRECTORIES`,
  which deliberately excludes `docs/superpowers/ledgers/`; a sibling test
  asserts ledgers are evidence columns, not population; a third test guards the
  column's existence in the parser. So two guards disagree about whether the
  column may hold anything, and the answer today is no. Found the hard way: this
  campaign is the first row in the file's history to populate it, which reds the
  prose gate. Row left blank, like every other. Registered as
  `TOOL-reconciliation-ledgers-column-is-unusable`.

---

## #4 [G3] — Spec approved

Nathan, 2026-09-07: *"LGTM. No disagreements."* All five flagged items approved
as written — the six-valued vocabulary including `flat`, the `doc:` anchor with
the cross-family generalization deliberately deferred, the disclosed
pre-freeze measurement of `rank-size-slope`, the regression guard's intended
teeth on future bake work, and the unmeasured guard-placement branch table.

No revisions. Proceeding to the implementation plan (G4: self-review against
this spec, then execute).

---

## #5 [G4] — Plan self-review, and one amendment to an approved spec

**Amendment.** The spec's six-valued vocabulary could not express a frozen-but-
unmeasured item, and authoring both the criterion and the verdict at freeze time
makes one of them a guess. Added `unmeasured` as an explicit **lifecycle state**
— tallied separately, never a coverage verdict, and cleared by a ratchet test
after the first run. This is what lets plan Task 2 freeze the corpus *before any
evaluation code exists*, making the 0016 freeze structural rather than a
promise. Spec §4.1 records it, marked as post-G3.

Flagged for G6 rather than re-opening G3: it strengthens the approved discipline
rather than changing what is built, and an amendment that strands its plan
executing the superseded design is the failure mode being avoided — the plan was
written against the amended spec, not the original.

**Self-review found three gaps, all fixed inline:**

1. Spec §6's guard-placement branch table had **no task step**. Added as Task 5
   Step 5, with the roster rule stated: only a green chamber run may write a
   sub-floor row; never hand-author one.
2. Task 8 **deferred a layering decision to its implementer** and carried no
   code — the plan-skill's own placeholder prohibition. Decided it instead:
   `windows/lab` reads the corpus with its own minimal four-field view (it may
   not depend on `cli`, and the kernel is not a corpus home), which is a
   deliberate duplication and therefore carries a two-way agreement test under
   decision 0261. Concrete code and three tests now in place, including the
   negative — an unscored metric must gain no claim line.
3. Task 8's agreement test may invert the dev-dependency direction. Rather than
   predict what `architecture.rs` permits, the step names both placements and
   tells the implementer to check the enforcement test before choosing.

**Spec coverage:** every section maps to a task; all six success criteria have a
task that discharges them. The one design rule with no mechanical enforcement is
§5's arity rule (within-world claims anchor per-world, cross-world claims anchor
to the Domesday) — wave one's items are all cross-world, so there is nothing to
enforce yet. Recorded rather than silently skipped.

**Ideonomy:** no separate pass; this is self-review of an approved design, and
the amendment narrows rather than opens the option space.

---

## #6 [G5] — Execution rulings, promoted from scratch

**A controller process defect, recorded first because it is mine.** Entries #1–#5
were written here; every ruling from Task 1 through Task 7 was then written into
`.superpowers/sdd/2026-09-07-the-seedbed/progress.md`, which is git-ignored and
dies with the worktree. `campaign-autopilot` is explicit that task state belongs
there and *rulings* belong here, and names a prior campaign that made exactly
this split wrong in a single sitting. Caught by Task 7's reviewer, not by me.
The rulings follow, in the order they were made.

**Task 2 — amending a frozen corpus pre-measurement.** Decision 0016 freezes a
corpus against MEASUREMENT, and nothing had been measured (Task 7 was the
first), so authoring corrections were legal. The binding constraint is narrower
and was carried into every fix dispatch: each correction had to be decidable
**without census values**, and implementers were forbidden from consulting them.

**Task 2 — the blind/unblind scope split.** A criterion can be vacuous two ways.
*Logically* — it restates a design fact (`cascade-rules-fired-goblin >= 1` is
true because the crate has a cascade); detectable from the metric's definition,
so in scope. *Empirically* — it could fail in principle but the world's range
never goes there; detecting that **requires reading the census**, so acting on it
would unblind the corpus. Permanently out of scope. The durable remedy for the
second is not a tighter band but a recorded one: every measurable item states
its FALSIFYING WORLD, enforced by a test.

**Task 2 — escalating a Minor against the skill's default.** Low discriminating
power was filed Minor, which normally defers to the final review. Escalated
because it has a deadline no other Minor has: a tighter band must be authored by
a session that has not read the census, and every session that scores the corpus
is thereafter disqualified. After Task 7 measured, nobody in this campaign could
fix it.

**Task 4 — `FractionInBandAtLeast` divides by `worlds`, not by the present
slice.** A world where the statistic cannot be computed is not evidence *for* the
regularity; dividing by the present slice would let a statistic absent almost
everywhere score highly on the few worlds where it appears. Absence counts
against the claim. Sibling kind `PresentOnFraction` already divided by `worlds`,
so this also removed a disagreement between the two fraction-based kinds.

**Task 5 — the stale-deferral hole, escalated on measured exposure.** All nine
`deferred` items cite ONE registry row, so that row flipping to `shipped` would
mis-verdict a fifth of the corpus in a single move. The consequence differs from
the sibling `systems` family: there a stale deferral misreports a capability,
here it **withholds an item from measurement** — the corpus keeps claiming it
cannot measure something it now can, so the item never re-enters the queue.

**Task 6 — the roadmap classification is a structured field, not parsed prose.**
Two notes with the same content in different words were landing in different
buckets, and `Item::note`'s doc still claimed "Never parsed" after Task 6 made it
parsed. The reviewer's constraint drove the design: **the corpus is frozen, so
fixing it by re-wording a note is exactly the data edit the freeze forbids.** The
fix had to be in the schema.

**Task 6 — Task 7 gains a `measure` mode.** The plan assumed `check` reports a
finding per `unmeasured` item; `audit_item` returns `None` for those by design,
so `check` exited 0. The suggested workaround — flip each item to `flat`, run
`check`, read the findings — interrogates a frozen corpus by mutating it, and a
half-finished run leaves it in a state nobody authored. A read-only `measure`
mode is what the first measurement actually is.

## #7 [G5] — Controller incident: I committed a reviewer's live mutation

While verifying the new stale-deferral guard's negative control, the re-reviewer
flipped `TOOL-a-regularity-corpus-can-measure-a-trajectory` from `raw` to
`shipped`. Concurrently I was committing capture rows to **that same file**. My
`git add` captured the flip; `7a0ddd208` shipped a false status that reddened the
branch tip, since all nine deferred items cite that row. Fixed in `9f7f62af5`.

**Explicit paths were not sufficient, and that is the lesson.** The standing rule
is "explicit paths, never `add -A` in a shared worktree". I used explicit paths
and it happened anyway, because the collision was on the file I meant to commit.
Path discipline protects against sweeping *unrelated* files; it does nothing when
a subagent is licensed to mutate the same file. The real control is temporal —
do not commit a tracked file while a review that mutates it is in flight, or
fence the reviewer to a scratch copy.

The guard caught it one commit after being built, which is the only redeeming
part: a false `shipped` withholding items from measurement is precisely the
failure it was argued into existence for.

## #8 [G5] — The first measurement, and a correction to my reading of it

Recorded verdicts, re-derived by the controller from `rows.csv`:

| item | verdict | number |
| --- | --- | --- |
| `sug-wealth-skew` | **flat** | median −0.577645, band [−1.2, −0.8], 17/1000 in band |
| `sug-predation-is-bounded` | grown | 968/1000 in [0.02, 0.5]; all 32 misses below 0.02, none above 0.5 |
| `sug-retaliation-deters` | grown | median 0.274309 ≤ 0.5 — but the **max over 1000 worlds is 0.4229** |
| `sug-credit-makes-hierarchy` | grown | median 88.0 against a bound of 1.0 |

Four measurable items are **three independent claims** (r = 0.999 merges the raid
pair): two grew, one did not. `sug-retaliation-deters` had an **empty failing
side** on this population.

**My proposed lesson was over-general, and the reviewer named the right variable.**
I read this as "prefer bands imported from outside empirical laws". But
`sug-predation-is-bounded` *also* took both poles from the source and passed,
because its band is enormous against the data's span; and `sug-wealth-skew`'s
failure was already explained item-specifically by its own pre-measurement note,
which cites the metric rustdoc recording that condensation is deliberately not
tuned to a rank-size target. n = 1, with the mechanism already named.

The separating variable is **band width relative to the statistic's dispersion**,
and the rule worth carrying to the next corpus is a *preregistered reachability
check*, authorable while blind:

> For each criterion, state the value the statistic must take to redden, and
> argue **from the mechanism** that a plausible world produces it.

That check catches all three weak items without seeing any data. The corpus
already states each item's FALSIFYING WORLD; the missing half is *why this
world's machinery can produce it*. Carry it to `axelrod-1984`.

## Final fix wave (post-whole-branch review): four findings, all closed

**F1 — the `doc:` anchor was never checked against the page it cites.**
`resolve_anchor`'s Doc arm asked only "is this path declared generated?", so
every generated path in `docs/generated-paths.txt` backed every measured
verdict equally; repointing an item at `book/src/domesday/climate.md` left the
whole suite green. `doc_states_the_claim` now opens the anchored page and
requires a line carrying both `CLAIM_MARKER` and the item's title, and
`GeneratedPaths` carries the repository root so a declared page can be opened
rather than only recognized. The page-provenance test now resolves the page
from `item.anchor` (it resolved it from the census column's `domain`, so its
subject was never the authored anchor) and additionally asserts the anchor IS
the page the Domesday writes that claim onto. Re-proved by mutation: the same
repoint now reddens 7 tests and `regularities check` at rc=1.

Two consequences worth recording. The guard fixtures in
`regularity_corpus.rs` can no longer invent a title — they borrow one from the
committed corpus, because a fixture titled `T` reddens on the anchor before the
audit reaches the verdict comparison the fixture is about. And
`the_binary_check_fails_on_drift_with_a_clean_audit` moved its mutation from
`title` to `note`: the title stopped being audit-invisible at this fix, and
that test's own second assertion caught it.

**F2 — the headline number had no test that could fail.** Both `all(...)`
quantifiers in `grown_claims` survived mutation to `any(...)`, because today's
merged claim is all-grown and every measurable item is measured, so on this
corpus the two quantifiers agree. `claim_reading` is now the public seam
`render` itself calls, and three synthetic corpora over the same near-collinear
pair exercise the rule: all-grown (the positive control, and the witness that
the merge happened at all), grown+flat, grown+unmeasured. Each mutation now
reddens exactly one of them.

**F3 — `regularities report` regenerated on the wrong side of the census
refresh.** It sat in the Group B+C `spawn` block, reaped well before the
`HV_CENSUS` block, so a census-refresh run authored the coverage report from the
OLD census and the Domesday claim lines from the NEW one. The stale report
matched the committed one byte-for-byte, so `add -u` staged nothing and the
delivery looked clean — then the drift test reddened on the delivery branch's
merge gate. Moved beside `lab domesday` as a serial trailer; its old comment's
`HV_CENSUS` claim was right and its ORDER conclusion was wrong, and the
correction says so where the line now sits.

**F4 — a count that goes stale at the merge.** `docs/generated-paths.txt`'s
`docs/audits/` header stated "25 tracked, 8 `artifacts`-written by name; 8
exceptions" — correct on this branch, wrong the instant it merges, and it
auto-merges cleanly so nothing objects. The block's own prose already warned
that a count stated inside its own document goes wrong the moment a row lands,
and had already been corrected once from 21/13/5. The three numbers are deleted
rather than refreshed; the three re-derivation commands the block already prints
are the answer and cannot rot. **`docs/generated-path-writes.tsv` still needs a
human at merge time**: base `15 24`, `origin/main` `15 26`, this branch `16 25`
— the correct product is `16 27`, which is neither side.

**Also, and it is the campaign's thesis made visible:** the Items table prints
no measured number, so a reader could not see that none of the three passes had
a reachable failing side. It now carries `measured` (`Measurement::summary`, the
gate's own string) and `observed` (the scored column's min and max). Derived,
not asserted: `sug-retaliation-deters` reads max 0.422855 against a ceiling of
0.5, `sug-predation-is-bounded` max 0.453316 against an upper pole of 0.5 (only
its 0.02 floor discriminated), and `sug-credit-makes-hierarchy` min 0.000000
against a floor of 1. `median_text` and `fraction_text` now quantize: their doc
said they owed the quantize-at-emit contract nothing "because never committed",
which stopped being true the moment the summary reached this artifact.

---

## #9 [G5] — The final review, its fix wave, and two parked residuals

**Fit to land.** The freeze was verified *mechanically*: `(id, verdict, statistic,
criterion)` extracted as structured JSON at all six commits touching the corpus
and diffed pairwise. No band, bound, statistic or criterion kind was ever altered
after the freeze, and both post-measurement edits moved nothing in
criterion-space. **All six demotions cost the campaign apparent `grown`s** — had
they stayed the tally would read 9 grown / 1 flat instead of 3 / 1. The freeze was
tightened against the campaign's interest at every step.

**Two vacuous guards found and fixed**, both mutation-proved:

1. **The `doc:` anchor was never checked against the page it cites** — the
   campaign's headline mechanism. Repointing an anchor to a generated page
   carrying no claim left 186/186 green. Now the Doc arm opens the page and
   requires a line carrying the claim marker *and* the item's title.
2. **The headline number had no failing test.** Both `all(...)` predicates behind
   `2 of 3 measured claim(s) grew` survived mutation to `any(...)`, because
   today's merged claim happens to be all-grown. Now pinned by synthetic
   merged-group fixtures that kill one assertion each.

Also fixed: `regularities report` regenerated on the *wrong side* of the census
refresh (authored from the old census while the Domesday claim lines used the
new one — fails closed, but in the wrong place); and three hard-coded counts in
`docs/generated-paths.txt`'s header were deleted in favour of the re-derivation
commands beside them.

**Parked residual A — the title half of the new anchor check is untested.**
Dropping `&& line.contains(title)` leaves 68/68 green: the negative control is a
page with *no* marker, so nothing exercises "page has a marker but not *this*
title". Behaviour is correct today (verified). *Ruling:* park. One fixture
anchoring a `society` item at `settlement.md` closes it. Cost if wrong: the
per-item specificity of a per-item check is unguarded, so a future anchor
mis-pointed *between two pages that both carry claims* would pass.

**Parked residual B — two wrong counts in a doc comment**, in the commit whose
headline fix was deleting wrong counts. `measured_reading` says "35 rows" and
"ten rows"; actual is 41 and 4. *Ruling:* park. Doc-only, no behaviour, not
published. Cost if wrong: none beyond the irony.

**Merge-time action requiring a human.** `docs/generated-path-writes.tsv`'s
`docs/audits/` row: base `15 24`, `origin/main` `15 26`, branch `16 25`. Both
sides moved independently, so it **auto-merges cleanly to a wrong value**. Set it
to `16 27` at the merge.

---

## Close

Definition of Done complete on the branch before submission: chronicle
(`book/src/chronicle/the-seedbed.md`, wired into `SUMMARY.md`), retrospective
(`docs/retrospectives/the-seedbed.md`, carrying the deferred-minor table and
where each landed), decision **0908**, a Confidence Gradient re-score
(*a preregistered threshold can be external and still unreachable*),
CLAUDE.md's directory guide for `regularities/`, and the reconciliation row.

`main` absorbed at `b096c7a7b`; three conflicts, all in regenerator-owned
artifacts, resolved by regeneration rather than by hand — which produced
`docs/audits/ 16 27` for the contested `generated-path-writes.tsv` row,
matching the value the final review derived independently. `gate-commit` rc=0
on the merged product; drift clean over every declared path. The digest's
in-force index was stale after 0908 and was regenerated — caught by the close
walk, not by a gate.

Nothing pre-existing in the idea registry required a status flip; the 47 rows
this campaign added are new captures, and
`TOOL-a-regularity-corpus-can-measure-a-trajectory` deliberately stays `raw`
because nine `deferred` corpus items anchor to it.
