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
