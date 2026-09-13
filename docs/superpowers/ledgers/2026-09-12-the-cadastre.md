# Decision ledger — The Cadastre (2026-09-12)

Campaign: widen `technologies/asimov-1989` from a 41-item cluster sample of
the invention.cards catalogue to a census of all 1,484 items, with the
catalogue's own `Built on` lattice.

Autopilot engaged (Nathan, this session). G3 and G6 are hard stops.

## Entries

**#1 [G1] — Grow the corpus in place, stand up a second one, or leave it?**

*Question.* No corpus in any of the six families has ever grown after its
freeze (`git log -S'items.len(), '` over `cli/tests/suite/` returns only the
commits that first authored each assertion; `henrich-2004-extended` was born
extended, not widened). So there is no precedent either way, and the three
candidates diverge materially.

*Decision.* **Widen `asimov-1989` in place to the full catalogue**, discharge
the cross-corpus obligation the widening creates against
`henrich-2004-extended` as one written class refusal, and report a rate rather
than a raw `absent` count.

*Why.* Two measured constraints decided it, neither of which was in the
pre-pass recommendation.

1. *The cross-corpus rule makes a THIRD corpus the most expensive option, not
   the cleanest.* `cross_corpus_ruling_gaps` (`cli/src/technologies.rs:1255`)
   fires on `registry:` anchors: a row cited by one corpus must be mentioned by
   every sibling. Verified rather than reasoned — one synthetic `deferred` item
   citing `registry:TECH-4` was added to `asimov-1989` and the SIBLING was
   audited:

   ```
   error: technology coverage audit found 1 finding(s) for `henrich-2004-extended`:
   registry:TECH-4 is cited by asimov-1989's item inv-probe-xyz but is never
   mentioned anywhere in henrich-2004-extended ...
   ```

   A new corpus makes *both* existing corpora owe rulings on every row it
   cites. Widening in place makes only `henrich-2004-extended` owe them.

2. *0136 clause 2 forbids an instrument silently switching what it measures.*
   Growing the id from 41 to 1,484 items does exactly that unless the original
   sample stays individually recoverable. So the 41 arc items keep their ids
   and their `source` arc attribution inside the widened corpus, and the
   campaign reports both series.

*Alternatives discarded.*
- *Grow in place with no sample-preservation* — rejected: silently switches
  what the id measures, and destroys the comparability of the committed
  35/6/0 reading.
- *A new corpus alongside* — rejected on constraint 1 above; it is the worst
  case for the sibling-ruling cost, and the two corpora would share one source
  and one declared bias, so the matrix gains no column of its own.
- *A frozen source manifest carrying the enumeration and lattice with no
  verdicts* — genuinely attractive and rejected only on Nathan's explicit
  direction to expand the corpus. Recorded because it is the cheap fallback if
  the scoring cost overruns: acts 1 and 2 of the decomposition below are blind
  and carry no ruling obligation at all.

*Ideonomy passes / overturns.* One pass (abstraction-lift + tree-finding,
rendered as a matrix, on polarity/scope/purpose). **One partial overturn:** the
pass produced the sample-vs-census lift, which is what surfaced constraint 2
and the reporting discipline, and the four-act decomposition below, which is
what made the fallback in the last bullet nameable. The sibling-ruling cost
(constraint 1) was found by reading the resolver during the pass, not by the
pass itself.

*The decomposition the pass produced,* because later tasks are keyed to it:
widening is four separable acts with different freeze status — **enumerate**
(blind, cheap), **import the lattice** (blind, mechanical), **score** (NOT
blind; where every hazard lives), **band a criterion** (forbidden outright —
family law disqualifies any session that has read the distribution, and every
session now alive has).

*Capture actions.* Spec §3. The manifest-only fallback wants an idea-registry
row if this campaign does not land it.

---

**#2 [Q] — Does the catalogue's own lattice contain a cycle?**

*Question.* `parse` rejects a cycle in `presupposes` as a parse error. If the
real `Built on` graph has one, the corpus cannot be authored from it without a
cycle-breaking rule — and a cycle-breaking rule is a selection decision, which
must be blind and stated before it is applied.

*Decision.* **Proceed on the measurement, and make full acyclicity a Task-1
gate with a named branch if it fails.**

*Why.* Measured on a real connected closure rather than assumed: a BFS from 8
random seeds exhausted at **152 nodes / 201 edges with 0 cycles**, roots
`biped` and `law-of-octaves`. A 40-item random sample independently showed
`Built on` present on **40/40** items, mean **1.40** edges/item, max 3 — so the
lattice is dense and near-rootless, and the projected full graph is ~2,080
edges.

*What this does NOT establish.* 152 of 1,484 nodes is 10%; zero cycles there is
evidence, not proof. Task 1 must run the acyclicity check over the whole graph
before authoring, and the branch table is in the spec rather than a prediction
here: cycle found -> STOP and report, do not invent a tie-break mid-task.

*Ideonomy passes / overturns.* None; this is an empirical feasibility check
with one right answer, not a design choice.

*Capture actions.* Spec §5, Task 1's gate.

---

**#3 [Q] — `absent` or `inapplicable` for the ~1,200 post-Classical items?**

*Question.* Hornvale's `TechHorizon` has four values topping out at
`Classical`. Most of the catalogue is 17th–20th century science. Is the
100-inch telescope `absent` ("nobody's yet") or `inapplicable` ("the world
deliberately lacks a precondition")?

*Decision.* **`absent`. This corpus scores no `inapplicable` at all, and
`provenance` says so and why.**

*Why.* Three reasons, in strength order.

1. *Observed sibling practice.* The Kiln's spec §4.1 explicitly nominated
   `inapplicable` for exactly this ("Asimov is not owed a world"), and the
   corpus it produced scores **zero** of them — 35 `absent`, 6 `deferred`. The
   family has never once used the value. Precedent is what the corpus did, not
   what its spec anticipated.
2. *`inapplicable` asserts a design intent that does not exist.* It means the
   world **deliberately** lacks a precondition. No ratified decision says
   Hornvale has no industrial era; the four-token model is an unfinished model,
   not a declared ceiling. Scoring 1,200 items `inapplicable` would manufacture
   a deliberate choice out of an unbuilt one.
3. *It is the one verdict with no guard.* 0136 deliberately does not ratchet
   the `inapplicable` tally. Routing the bulk of a 1,484-item corpus through
   the single unratcheted verdict puts the campaign's whole mass where nothing
   watches it.

*The polarity trap, stated because it is the reason this entry exists.* Ledger
#12 established that a high `absent` count is **this family's flattering
result**. `inapplicable` is flattering in the opposite direction — it converts
"we cannot do this" into "we needn't." **Both available verdicts are
self-serving, in opposite directions**, so neither can be chosen on which one
looks honest. It is decided on the three reasons above instead, and it is the
lead flagged item at G3 because it is the call most worth Nathan overruling.

*Alternatives discarded.*
- *A date-keyed mechanical rule (attested after the Classical ceiling ->
  `inapplicable`)* — rejected: it equates Asimov's Earth chronology with
  Hornvale world-years, which is a category error, and it would dress a
  judgement as a derivation.

*Ideonomy passes / overturns.* One (polarity substitution — the flip is what
produced "both directions are self-serving", which is the entry's actual
finding). No overturn.

*Capture actions.* Spec §4.2; G3 flagged items.

---

**#4 [G4] — The cheap scoring method was measured against ground truth and it
FAILED. 50% recall, and the misses are all in the flattering direction.**

*Question.* Scoring 1,443 new items is the campaign's only real cost. The
obvious economy is a keyword sieve over the idea registry: match each item's
`introduces` token and title against registry row text, adjudicate the hits by
hand, score the rest `absent`. Does it work?

*The test, and why this ground truth is the right one.* The Kiln scored the 41
arc items BY HAND, producing 6 `deferred`. That is a hand-authored answer key
for a population the sieve can be run against blind. Run (1,787 registry rows,
>=2 shared tokens of 4+ characters, stopworded):

```
items flagged as candidates by the sieve: 23 of 41
deferred RECOVERED by the sieve: 3 of 6
MISSED: ['inv-animal-dom', 'inv-coal-mining', 'inv-turnplow']
```

*Decision.* **The sieve is REFUSED as a filter.** It is not merely weak, it is
weak in both directions at once: it misses half the true positives while
flagging 56% of the population for adjudication, so it buys almost no labour
and costs half the signal.

*Why this is disqualifying rather than a tuning problem.* **Every miss scores
`absent` when the truth is `deferred`, and ledger #12 established that a high
`absent` count is this family's flattering result.** An instrument whose error
is unbiased is a noisy instrument; one whose error runs entirely toward the
self-serving answer is a broken one. `inv-animal-dom`'s own note records that
its `deferred` was reached "by searching the idea registry" — and the sieve
did not find it.

**Tuning it against these 6 is refused too**, and that refusal is the load-
bearing half: 6 positives is the only answer key this campaign will ever have,
and fitting a threshold to it consumes the control. A sieve tuned to score 6/6
on the only set where the truth is known tells you nothing about the 1,443
where it is not.

*The corroborated prior.* The Kiln's own follow-up on `MEM-8` records that the
sweep which found it "was never surfaced by any keyword sieve." That was one
anecdote; this is a measured recall figure on a real answer key, and it agrees.

*Alternatives now on the table.* Carried to Nathan rather than auto-adopted --
see #5, because the honest remaining options differ in campaign SIZE by an
order of magnitude, which is a scope question and not a technique question.

*Ideonomy passes / overturns.* One (substitution on the instrument's error
direction, which is what turned "50% recall is weak" into "50% recall biased
entirely toward the flattering answer is disqualifying"). No overturn -- the
pass sharpened the reason, not the verdict.

*Capture actions.* This entry; #5; the spec's scoring section needs replacing
before any plan is written against it.

---

**#5 [Q] — The verdict vocabulary has no value for "admitted but not yet
examined", and that is the deepest argument in the campaign.**

*Question.* If items cannot be scored cheaply, can they be admitted unscored?

*Finding.* **No.** The nine values are `present`, `refused`, `deferred`,
`absent`, `inapplicable`, `grown`, `flat`, `lost`, `unmeasured` -- and
`unmeasured` does not mean "not looked at": family law defines it as "reach
PASSED, trajectory unscored" and requires a `test:`/`path:` mechanism anchor,
which is a positive claim that the world models the capability. `verdict` is a
required field on every item.

*Consequence, stated plainly because it governs the choice in #5's sibling
decision.* **Admitting an item to this corpus obliges a claim about it.** There
is no way to say "we took in the whole catalogue and have examined a tenth of
it." A census therefore costs a census's worth of judgement -- the corpus
format enforces it -- and the only artifact that can carry an unexamined
population is one that carries no verdicts at all.

*That is the manifest fallback from #1, arriving a second time by a different
road,* which is why it is now a live option rather than a footnote.

*Ideonomy passes / overturns.* One (the vocabulary read as a pipeline, asking
what each value presupposes; `unmeasured` was the candidate and it fails on its
anchor requirement, not on its name).

*Capture actions.* #5 carried to Nathan at the scope stop.

---

**#6 [Q] — `TechHorizon` dates RUINS, not the living world, and two of this
campaign's arguments were built on the opposite reading (Nathan, correcting the
controller).**

*The correction.* This session repeatedly described `TechHorizon`'s four values
as "Hornvale's technology model" and "our ceiling is Classical". Nathan: the
horizon applies to *the technology of discovered ruins*, not to the technology
of the world itself. Verified against every consumer rather than accepted on
authority:

```
domains/history/src/record.rs:142   OccupationRecord.tech       an occupation's tech
domains/history/src/flesh.rs:644    occ.core.tech >= Iron       fleshing occupation records
windows/worldgen/src/vestige.rs:321 tech: TechHorizon::Iron     vestiges, i.e. ruins
domains/history/src/lib.rs:73       OCC_TECH = "occ-tech"
windows/worldgen/src/history_bake.rs:2900  tech_for(year)       dates a PAST occupation
```

Every reader is occupation- or vestige-side. **There is no living-world
technology model at all.**

*What it invalidates, specifically.* Two things, and neither is a detail.

1. **The 500 CE threshold** (#3's and the spec's §4.1). It was derived from
   "our top horizon is `Classical`, and the Classical era ends around 500 CE."
   That is a fact about how old ruins get, and it was being used to bound what
   living peoples could do. Wrong quantity.
2. **#3's `inapplicable` reasoning**, which argued the value would "manufacture
   a deliberate choice out of an unbuilt one" by pointing at the four-token
   ceiling. The verdict SURVIVES and its reason gets stronger: with no
   living-world model of any kind, there is no precondition the world
   *deliberately* lacks, because nothing about it was decided. `absent` stays;
   the argument is rewritten.

*What it does NOT invalidate.* The Kiln's F2 finding — that the shipped model
is a monotone clock in which prerequisites, divergence and loss are
inexpressible — stands, and is strengthened: it is not merely that the model
cannot express loss, it is that the thing being modelled is ruin-dating and
the living world has no capability state to lose.

*How this got past three gates.* The controller read `tech_for`, `tech_weight`
and `TechHorizon`'s definition and never once read a CALLER. The definition is
domain-neutral ("Stone-tool, pre-metal"); only the call sites say what it is
*for*. This is the project's own recorded lesson about reading a constraint off
a construction site instead of its consumers, reproduced exactly.

*Ideonomy passes / overturns.* None — a factual correction from the project
owner, verified in code.

*Capture actions.* Spec §4.1 and §4.2 rewritten; this entry; the retrospective
owes the caller-reading lesson.

---

**#7 [G1-revised] — The selection rule is TWO blind rules unioned, then closed,
and the population is 301.**

*Question.* #1 adopted a census of all 1,484. #4 and #5 then showed a census
cannot be scored honestly at that size. What replaces it?

*Decision.* **Seed = the catalogue's three named arcs UNION everything the
catalogue attests before 1700; then close under the catalogue's own `Built on`
relation.** Measured: **301 items, 401 edges, 0 cycles, 1 root (`biped`), 260
new to hand-score.** The closure step adds only 7 above the cut (the steam
chain), so the era cut is very nearly self-closing.

*Why two rules and not one.* They are near-independent and each misses what the
other catches. Measured at the 500 CE cut, where the comparison was first run:
closure alone 77, era alone 98, **overlap only 41**. Closure alone excludes
`pottery` — which `TECH-2` in the idea registry names explicitly as the
pyrotechnology ladder's first rung — along with irrigation, calendar, law,
medicine, coin, arch, aqueduct. **A rule that drops the capability the
project's own registry most explicitly plans is not a principled rule**, and
the controller recommended it for two turns.

*Why closure is not optional.* Decision 0386 derives demands by transitive
closure over `presupposes`, and `presupposes` may name only in-corpus items, so
any corpus drawn from a subset of a linked catalogue silently loses its
outside edges at authoring time. The Kiln documents this as a known distortion:
the demand set "UNDER-DESCRIBES every such item's real prerequisites."
Closure repairs it. `Built on` is the direction that terminates (one root);
`Led to` does not, so closure is defined on `Built on` alone.

*Why the era cut cannot flatter us — the objection the controller raised and
then withdrew.* Selection keyed to our own code makes the selection the
measurement (The Kiln, ledger #7), and "items appropriate to our scenario"
looked like exactly that. It is not, for a reason #6 supplies: **there is no
living-world technology model, so no item's verdict depends on its date.**
Every item scores on the same basis whatever era it is from. The cut changes
which capabilities are examined, never how well the world does on them, so the
gaming vector is absent rather than merely unlikely.

*The threshold is a judgement and is recorded as one.* 1700 comes from Nathan's
reading of the setting's intellectual reach — "perhaps we've had a Descartes
somewhere; I don't think we've had a Newton" — with deliberate headroom. Noted
at the time and accepted: `< 1700` **includes** Newton (`Calculus` 1669, `Laws
of motion` and `Universal gravitation` both 1687); `< 1650` is the cut that
matches the phrase literally and gives 249 items. 1700 was kept on the
instrument argument: **a corpus that stops exactly where the world is thought
to stop can never report that the world stops too early.** An `absent` on
universal gravitation is a finding; its exclusion is an invisible decision.

*Alternatives discarded.*
- *Census of all 1,484* (#1's adopted answer) — withdrawn on #4 and #5: it
  cannot be scored honestly, and ~1,400 of its cells are permanently `absent`
  under any model Hornvale will have, so they can never discriminate.
- *Closure alone, 77 items* — rejected above; drops `pottery`.
- *Era cut alone, no closure* — rejected: reintroduces the truncation this
  campaign exists to repair.
- *`< 1650`, 249 items* — rejected on the ceiling-indictment argument.

*Ideonomy passes / overturns.* One (scope substitution on the selection rule —
narrow to one neighbourhood, widen to all of humanity — which is what exposed
that the two candidate rules overlapped on only half their union and therefore
should be unioned rather than chosen between). **One overturn: the census
adopted at #1 is withdrawn.**

*Capture actions.* Spec rewritten to this population. The withdrawn census
keeps its one interesting question — whether the `absent` fraction differs
between sample and full population — which wants an idea-registry row rather
than a campaign.

## Follow-ups

- **The manifest-only fallback** (#1) wants an idea-registry row if this
  campaign does not land: the enumeration plus the lattice, frozen, carrying no
  verdicts and therefore owing no sibling rulings.
- **The census's one good question survives its rejection** (#7): does the
  `absent` FRACTION differ between the arc sample and the whole catalogue? Not
  answerable without scoring the whole catalogue, so it is a registry row, not
  a campaign.
- **`tech_offset` is drawn per-community, not per-people**, and its doc comment
  says otherwise — inherited from The Kiln's own follow-ups, and #6 makes it
  more confusing rather than less, since the whole structure is ruin-side.

---

**#8 [G4] — Plan self-review against the spec**

*Two gaps found and closed before the plan was committed, both of the same
shape: a spec requirement with no step to implement it.*

1. **No task wrote `provenance` or `frozen`.** Family law requires the
   selection rule to be stated in `provenance` and applicable by someone who
   has never read `history_bake.rs` — the single most load-bearing prose in
   the artifact, and six tasks said "record it in provenance" while none
   created it. Now Task 2 Step 5, with the five things it must carry in order.
2. **Success criterion 4 — edge completeness — was verified by nothing.**
   Closure guarantees zero dropped edges, which is the entire point of the
   campaign, and the plan asserted it rather than checking it. Now Task 2
   Step 6, a script that diffs every item's `presupposes` against the source
   and expects an empty result in BOTH directions (dropped AND invented).

*Why both are the same defect.* Each was a claim the spec makes that the plan
restated instead of implementing — the project's own recorded pattern of a
clause vacuously satisfied hiding a deliverable nobody built.

*Ideonomy passes / overturns.* None; a checklist pass, not a design decision.

*Capture actions.* Plan Task 2 Steps 5 and 6.

---

## Task 1: Fetch, close, and verify the population

**Step 1 — enumeration fetched twice, independently.** Both fetches of
`https://invention.cards/browse/` (2026-09-13, ~01:20 UTC) are byte-identical:
**150,326 bytes each**, `diff` empty. FETCHES AGREE; proceeded.

**Step 2 — parsed the enumeration.** Applying the brief's own regex
(`href="/([a-z0-9-]+)/">`) yields **1,484** items, exactly matching The Kiln's
prior count. **But this count is corroborating a shared bug, not confirming
correctness — branch fired.** Direct inspection of the page found **1,486**
`<li>` entries and 1,487 `/slug/` hrefs (1,486 items + the page's own
`/browse/` footer self-link, which sits outside any `<h2>`/`<ul>` section and
was never a candidate match to begin with). The 2-item gap is a charset bug in
the prescribed slug pattern, which excludes anything outside `[a-z0-9-]`:

- `2,4-d` (title "2,4-D", year 1944) — slug contains a comma.
- `mössbauer-effect` (title "Mössbauer effect", year 1958) — slug contains a
  non-ASCII letter (ö).

Both are real catalogue items with real `/slug/` pages. Broadening the slug
capture to `[^"/]+` recovers both; re-parsing gives **1,486** distinct items,
the count recorded as this session's enumeration total (`all-items.json`).
**Neither item is in the seed** (both post-1700, absent from all three arcs)
**nor reachable via `Built on` from the seed** (checked directly: neither slug
appears as a value in any closed-population item's `Built on` list), so this
bug has **zero effect on the closed population or on `population.json`** — it
only corrects the reported size of the source enumeration.

**Step 3 — seed built from the two blind rules.** Arc counts, fetched
independently and matching The Kiln's prior figures exactly:
- `knights`: **16**
- `republic-of-letters`: **10**
- `steam-diffusion`: **15**
- arc union: **41**

Era (`year < 1700`, over the corrected 1,486-item enumeration): **294**.
Seed (arc union ∪ era): **298** — unaffected by the Step 2 correction, since
both newly-recovered items postdate 1700. Matches The Kiln's prior 298
exactly.

**Steps 4–5 — fetched every seed item and closed under `Built on`.** 301 pages
fetched (301 new; cache now warm at `/tmp/cadastre/pages/`), 0.3 s sleep
between requests. BFS to fixpoint on `Built on` only:

**CLOSED: 301 items, 401 edges.** Matches The Kiln's prior 301/401 exactly.
The closure adds exactly 3 items beyond the 298-item seed: `coke-iron`,
`heat-capacity`, `mercury-thermometer`.

**Step 6 — acyclicity gate.** **CYCLES: 0.** Roots (items with an empty
`Built on`): **1** (`biped`). Matches The Kiln's prior 0 cycles / 1 root
exactly. Stop condition did not fire; proceeded.

**Step 7 — `Led to` agreement with `Built on`.** Over the 301-item closed
population:
- built-on edges: **401**
- inverted led-to edges: **401**
- led-to with no built-on: **0**
- built-on with no led-to: **0**

Full agreement in both directions — no asymmetry to launder.

**Step 8 — emitted `population.json`.** `/tmp/cadastre/population.json`: a
JSON array of **301** objects, each exactly
`{slug, title, year:int, field, built_on:[slug], led_to:[slug]}`, covering the
closed population and nothing else (verified: correct key set on every
record, `year` is `int` on every record, 301 distinct slugs, valid top-level
JSON array). All 301 items' `title`/`year`/`field` came from the browse-page
enumeration; none needed a page-metadata fallback.

**Branches that fired:** Step 2's "count differs" branch (the enumeration is
1,486, not 1,484 — a slug-regex charset bug, recorded above; the two
recovered items do not touch the seed or the closure). No other branch table
entry fired; neither stop condition (Step 1 fetch mismatch, Step 6 cycle)
triggered.

**Script and cache are throwaway**, per ledger #13: `/tmp/cadastre/*.py` and
`/tmp/cadastre/pages/*.html` are not part of this commit and are not added to
the repository.

---

**#9 [G5] — The catalogue holds 1,486 items, not 1,484, and this campaign's
own confirming fetch reproduced the error.**

*Finding (Task 1, flagged by the implementer against its own brief).* The slug
pattern `[a-z0-9-]+` — prescribed in the plan, inherited from the controller's
spec work, and matching what The Kiln used — silently drops two real items:
`2,4-d` (comma) and `mössbauer-effect` (non-ASCII `ö`). Broadening to `[^"/]+`
yields **1,486**.

*Verified independently by the controller* on a separately cached copy of the
page, rather than accepted from the report:

```
href="/2,4-d/"
href="/mössbauer-effect/"
ASCII-only regex: 1484
broadened regex:  1486
```

*Ruling.* **Adopt 1,486 as the catalogue's size. The population stands at 301,
unchanged.** Both recovered items postdate 1700, appear in no arc, and are
named in no item's `Built on` list — checked directly against the closed graph
by the implementer and not merely argued — so seed, closure and
`population.json` are untouched.

*Why this is the campaign's sharpest process finding.* Spec §2.1 claimed a
"third independent fetch" had reproduced The Kiln's 1,484 exactly, and offered
it as evidence of the catalogue's stability. **The fetch was independent; the
PARSER was not.** Reusing the same slug pattern could only ever confirm the
pattern, so the agreement measured nothing about the catalogue. This is the
project's own recorded failure — a peer confirmation that sampled nothing new —
reproduced by the session that had the note.

*What it costs if wrong.* Nothing downstream: the corpus is built from
`population.json`, which never contained either item. The exposure is a false
denominator in committed prose, which is exactly what #9 corrects.

*Blast radius, since a correction has one.* The Kiln's
`docs/audits/technology-coverage-asimov-1989.md` and the frozen corpus's
`provenance` both carry 1,484. Both are merged and append-only, so the
correction lands in THIS campaign's `provenance` (Task 2 Step 5) and the
chronicle (Task 6 Step 3), never by editing them. The spec is unmerged and was
corrected in place.

*Ideonomy passes / overturns.* None; an adjudication of an implementer's
finding against a controller-verified measurement.

*Capture actions.* Spec §4 corrected; Task 2 Step 5 and Task 6 Step 3 inherit
the correction; the retrospective owes the same-instrument lesson.

**#10 [Task 2] — Five judgement calls the brief left open while rebuilding the
corpus at its closed 301-item population.**

*1. `henrich-2004-extended`'s frozen-size assertion is left at 41, untouched.*
`cli/tests/suite/technology_corpus.rs` carries exactly two `assert_eq!(c.items
.len(), 41)` lines — one loading `asimov-1989`, one loading
`henrich-2004-extended` (misleadingly named
`the_collapse_corpus_is_frozen_at_its_declared_size`, a pre-existing naming
oddity this task did not touch). The brief's Step 7 says "both assertions
currently read `41`" and shows one target line (`301`), which reads as
ambiguous about whether both should change. Task 2's own interface note is
explicit that only the asimov side is in scope this task ("Task 3 rescores;
Task 4 discharges the sibling"), so only the `asimov-1989` assertion moved, to
301. Changing `henrich-2004-extended`'s to anything but 41 would have been
false — that corpus file is untouched — and reads as the brief describing the
file's PRE-EDIT state (two literal `41`s) rather than instructing both to
change.

*2. New item ids are `"inv-" + slug`, verified against the existing
convention rather than invented.* All 41 arc items' ids already equal
`"inv-" + <the slug recovered from their own source string>`, checked
programmatically before authoring a single new item. So every one of the 260
new items gets `id = "inv-" + slug` too — stable, readable, and mechanically
derivable from `population.json` with no judgement per item.

*3. New items' `source` carries only the attested year, no person or place,
matching the brief's own worked example exactly.* `population.json` (Task 1's
interface) carries `{slug, title, year, field, built_on, led_to}` — no
attributed person or place, unlike the original 41's hand-authored `source`
strings. The brief's Step 4 example (`inv-pottery`) already reflects this:
`"... invention.cards \`/pottery/\` (6,000 BCE)"`, with no attribution clause.
Followed verbatim rather than re-scraping the 301 cached pages under
`/tmp/cadastre/pages/` for attribution the interface doesn't carry (and which,
checked directly, isn't even present in the cached static HTML — the site
renders it client-side from a payload the static fetch doesn't capture).

*4. The declared-bias re-count in `provenance` is geography for the original
41, topic (`field`) for all 301 — not geography for all 301.* Family law
requires the bias claim re-counted for the new population (Step 5, item 5).
The original claim's evidence was geographic (26 of 41 in Europe, 0 in the
Americas/Africa/Oceania). That count is unavailable for the 260 new items from
either interface Task 1 handed off (`population.json` has no place field; the
cached page HTML doesn't carry it either — checked directly on `pottery.html`,
where the only `author` field is the SITE's author, not the invention's).
Rather than fabricate a place count or silently drop the bias section,
`provenance` states the geographic count as still true and unchanged for the
41 arc items specifically, and adds a genuine re-count over all 301 using the
one taxonomy the interface actually supports — the catalogue's own `field`
tag (Science 95, General 47, Geography 36, Space 36, Culture 31, Math 29, War
18, Design 9) — naming plainly that this is a narrower recount than the
original claim's grain, not a claim that geographic bias has vanished.

*5. `cli/tests/suite/technology_coverage.rs`'s three baseline-pinned tests
moved with `novelty_baseline`, though the brief's file list didn't name this
file.* Raising `novelty_baseline("asimov-1989")` from 35 to 295 (Step 7) is a
change to a function this file's own tests pin directly:
`the_two_baselines_match_the_committed_corpora` asserted the live baseline AND
the live `absent` count both equalled 35; two more tests asserted the ratchet
fires/passes at 34/35/36 around that baseline. All three would have failed
after Step 7's edit landed — not a pre-existing failure, but one Task 2's own
required change introduces — so fixing them (34/295/296, per the CLAUDE.md/
global-instructions rule "never disable tests, fix them") is in scope even
though the brief's Files list under-named it. `cli/tests/suite/
technology_corpus.rs`'s own two frozen-size assertions were unaffected by this
edit and needed no further change.

*6. `inv-biped` (the population's sole root) needed an authored `disclosure`
Step 4 says new items shouldn't carry, because the resolver's chosen/inherited
rule is unconditional.* Step 4 says new items get no `disclosure` at this
step. But `is_chosen` reads the closure, not authorship history:
`inv-biped` has an empty `presupposes`, so it is trivially CHOSEN, and
`disclosure_gaps` (correctly) flagged it as a finding until it carried one —
confirmed by running the resolver per Step 8 ("let the resolver enumerate the
real set... follow the finding, not this plan"). The disclosure authored is
deliberately thin and says exactly that: `inv-biped`'s `absent` verdict is an
unauthored Task 2 placeholder, not a search result, and Task 3 inherits and
must revisit this disclosure if it changes the verdict. This does not
contradict Step 4's "no disclosure" instruction in substance — it says the
placeholder was NOT searched — it only satisfies the resolver's mechanical
requirement that a `disclosure` field be present and non-empty for any
CHOSEN item, honestly authored rather than left as a blocking finding.

*Consequence for Task 3.* The audit run at the end of Task 2
(`cargo run -p hornvale -- technologies check asimov-1989`) found exactly 17
findings before repair: 16 items losing their disclosure (the arc items that
became INHERITED once closure restored their real, `absent` ancestors) and 1
item gaining one (`inv-biped`, newly CHOSEN as the population's sole root).
The brief's own Step 3 list of "10, MEASURED" items undercounted by 6 —
`inv-papyrus`, `inv-literature`, `inv-horse`, `inv-alphabet`, `inv-library`
and `inv-parchment` were already CHOSEN non-roots in the 41-item corpus (per
family law's own worked example about `inv-parchment`) and became INHERITED
the same way the 10 named roots did. All 17 are now resolved; `technologies
check asimov-1989` exits 0.

---

**#11 [G5] — The held-out control: 4 of 6 recovered, the aggregate contaminated,
and two disagreements that look like defects in the FROZEN corpus.**

*What was run.* Task 3 Step 1: an agent re-scored the 41 arc items blind, given
the catalogue pages, the idea registry and the decision log, and forbidden the
corpus, the audits and every task report.

*Result.*

```
KILN'S 6 deferred:    animal-dom coal-mining library literature turnplow writing
CONTROL'S 6 deferred: alphabet animal-dom horse library turnplow writing
RECOVERED 4/6 · MISSED coal-mining, literature · EXTRA alphabet, horse
per-item disagreement: 4 of 41 = 10%
anchors on all four recovered: EXACT (BIO-animal-domestication, MEM-4, BIO-8, MAP-8)
```

*Ruling on the branch table.* The plan's Step 1 table says misses of 1-2 →
record the recall and proceed; 3 or more → stop. **Two missed, so the campaign
proceeds**, with the measured recall stated in `provenance` as a declared
limitation of the sweep.

*The contamination, and why it changes what the control measured.* The agent
reported, unprompted, that `book/src/frontier/idea-registry.md` — a file its
own procedure REQUIRES it to read — carries the row `TECH-invention-corpus`,
whose text states *"First reading … absent 35, deferred 6 — zero `present`."*
It therefore knew the target aggregate partway through, and returned exactly
35/6. **The aggregate is worthless as a control and the item set is not**:
knowing "six" cannot tell you WHICH six, and the sets differ on four items.
The set comparison is the measurement; the tally is discarded.

**This is the same defect as #9, one level up.** There, a confirming count
reused the same parser and so confirmed the parser. Here, a confirming tally
was available to the thing being measured and so confirmed nothing. Twice in
one campaign, agreement was produced by the instrument rather than by the
world.

*Structural consequence — the control can never be clean as things stand.* The
spoiler lives in the one file every scorer must read, so withholding it is not
available. A future control needs the registry served with that row redacted,
or the row must stop restating a corpus's scored tally. Recorded as a
follow-up; not repaired here, because repairing it mid-campaign would mean
editing the instrument between the control and the sweep.

*The four disagreements are NOT swept, and two of them accuse the frozen
corpus.*
- `horse` — control says `deferred` on `registry:BIO-animal-domestication`,
  the identical row The Kiln cites for `animal-dom`. Domesticating the horse
  is animal domestication; the corpus scores it `absent`.
- `alphabet` — control says `deferred` on `registry:MAP-8`, the identical row
  The Kiln cites for `writing`, reporting that `frontier.md`'s MAP-8 essay
  names phonetic borrowing explicitly.
- `coal-mining`, `literature` — control says `absent` where The Kiln says
  `deferred`; the agent flagged `coal-mining` as a close breadth call
  (`TECH-3` names ore, not fuel).

**If `horse` and `alphabet` are right, the frozen corpus is wrong in the
FLATTERING direction** — `absent` where a registry row plans it — which is the
exact error ledger #12 was written to prevent, found by the instrument built to
catch it.

*Ruling: the 41 arc verdicts are NOT changed.* Spec §10 and this plan's Task 2
require the arc items to keep their verdicts, and there is a stronger reason
than consistency: these same verdicts are the control's answer key. Re-scoring
them using the control's output would consume the control and make the
measurement unrepeatable. The four disagreements are recorded here, carried to
the chronicle, and go to Nathan at G6 as a finding about `asimov-1989` for a
successor to rule on.
Cost if wrong: the corpus keeps two verdicts that are probably too flattering,
visibly documented, for one campaign.

*Ideonomy passes / overturns.* One (polarity again — asking which direction
each disagreement errs in, which is what separated "the control missed two"
from "the control accuses the corpus of two").

*Capture actions.* `provenance` states the measured recall; chronicle carries
the four disagreements; follow-ups carry the registry-spoiler defect.
