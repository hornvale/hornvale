# Retrospective — The Assize

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-assize.md): five red heavy-tier
instruments cleared, two of them onto the census, one preregistration
adjudicated, and a cost discriminator corrected before it was mechanized.

The campaign was named for the weight you subtract to make a scale read true.
It turned out to be about the scales rather than the things weighed: no product
code changed anywhere in it, and the census regen proved it — 194 → 196
columns, and **zero shared columns moved across all 1000 rows.**

## The headline: ten author predictions, ten refutations, and every measurement held

The Delvers ran this count to five and The Confusion sharpened it. This campaign
ran it to nine, and the author was the controller in every case.

| # | prediction | measurement |
|---|---|---|
| 1 | "Move it to the census" is the cure for all five reds | `disposition_calibration` already samples **60 worlds**; a thousand would not save a bound encoding pre-Tolerance physics |
| 2 | Both 12-seed panels are equivalent and retire the same way | one measures a quantity `build_world_to` **discards**; the other does not |
| 3 | Relation-years beats the stock, because the flow *accumulates* and a stock is *steady* | stock **+0.934**, relation-years **+0.891**. Count-only 0.934, duration-only 0.457 — duration dilutes |
| 4 | `top_share` earns its own column at −0.769 | `top_share >= 1/stock` by construction; decays to −0.539 excluding small worlds |
| 5 | A census column should be a normalized share | `stock/occupations` **+0.623** against raw **+0.934** |
| 6 | `ρ > 0` alone suffices; the span guard makes it safe | ρ survives **both** mutations (0.831 → 0.127 → 0.754); the span guard is **dormant** |
| 7 | desert's newly-authored kind ranks 2 of **26** | 2 of **29** — 26 was the *pre-regeneration* count |
| 8 | The indoor regression is the light/fabric work | four of five candidates moved it by **zero**; it is the palette-key widening |
| 9 | A 36-world probe showing 0 zeros means an inertness guard can never fire | the census finds **13 zero-tribute worlds in 1000** |

Not one measurement was overturned. The asymmetry is now so consistent across
four campaigns that it should be read as a property of the work rather than as
a run of bad luck: **in this codebase, a mechanism you can articulate but have
not measured is more likely wrong than right.**

Two of these are worth their own note. **#6 and #9 were caught by subagents
correcting the controller**, not by the controller's own review — and in both
cases the agent's honest report was more valuable than a clean green would have
been. #7 was caught the same way, against a number the controller had put in
the plan.

## An instrument that asserts a verdict it never computed, three times in one campaign

This is the shape The Confusion named, and it recurred three times here in
forms that campaign had not enumerated.

1. **A failure message explaining its own cause, wrongly.** `history_battery`
   failed with *"seed-42 displacement does not fire at all at Full depth: the
   gates do not survive the cascade."* Measured: zero at **both** depths, on a
   world with 420 occupations. The cascade was never involved. Written at build
   time as a prediction, read at failure time as a finding.

2. **A published artifact printing an unconditional PASS.** `summary.md`'s
   `migration-fired-at-volume` line was never wired to any check. While the
   number was healthy nobody could tell. At `mig42 = 0` it rendered, in a
   committed artifact, as `"0 migration events (floor 5). PASS"`.

3. **A retrospective follow-up that was itself false.** The Confusion's own
   follow-up said `scene_cost` asserts before printing. It does not, and all
   five metrics were sitting in the very log the follow-up described. An
   incorrect follow-up, inside the retrospective about inherited diagnoses.

The generalisation, and it is narrower than "distrust error messages":
**anything that states a verdict — a panic message, a rendered artifact, a
retrospective bullet — should be checkable against the thing it claims to have
measured.** All three of these were persuasive precisely because they were
specific.

## A red gate freezes the artifact it writes, and nothing notices

`history_battery` **writes** `book/src/laboratory/generated/the-history/` as its
last step. A test that panics before its write step cannot regenerate its
artifact. So the artifact silently froze at 2026-08-06 while two roster
campaigns re-decided settlement placement world-wide underneath it.

Removing the assertion *unmasked* the staleness rather than causing it, and the
regeneration moved 18 of 21 numbers.

**This is a new failure class, distinct from the ones already recorded.** The
drift check catches an artifact that changes; `make rebaseline` catches an
artifact its script writes. Neither can see an artifact whose *writer is red*:
the drift check passes because nothing moved, and the writer never ran. The
longer the red persists, the staler the artifact, and the two facts never meet.

Worth stating because the repo has three separate artifact-freshness mechanisms
and this fell through all of them.

## The measured cost of a sign claim, and why concentration beats distribution

`disposition_calibration`'s bounds were replaced with sign claims — set from
the mechanism rather than fitted to data — on the argument that a fitted
threshold has now died twice on this axis. That argument holds. But the trade
has a price, and it is measurable:

- `ρ > 0` (distributed across nine peoples) **cannot be falsified** by either
  mutation the file carries. ρ moves 0.831 → 0.127 under one of them — an
  enormous signal — and the sign claim throws all of it away.
- `separation > 1.0` (concentrated on the two extremes) catches that same
  mutation cleanly.

So the refinement: **a sign claim keeps its teeth when it is concentrated where
the mechanism breaks first, and loses them when it is spread across the whole
population.** Both are equally durable against roster growth; only one is
falsifiable. `ρ > 0` is retained and honestly relabelled a *directional record
rather than a proven guard*, and the temptation to set it at `0.3` — which
*would* have caught the mutation — was refused, because choosing a threshold
because it fires is the fitting the rest of that file now forbids.

## A discriminator can be wrong, not merely unwritten

The cost gates' documented rule was *"all metrics inflating together is the
machine; a real regression is local."* Applied to the real data it gives the
**wrong answer**: genesis at 2.09× its basis with four controls at 0.96–1.29×
reads as a regression, while a quiet box builds the same world in 3948 ms
against a 13000 ms ceiling.

The rule fails because the five metrics have **different resource profiles** —
only `genesis` sculpts terrain across a large grid, so it is the only one a
saturated runner starves. Uniformity was never the right test.

The lesson is about the order of operations. The queued task was "mechanize the
prose discriminator". Had it been done as written, a wrong rule would have been
promoted from prose into code, where it would have been harder to question and
would have carried an air of having been computed. **Check whether a criterion
is correct before making it executable; mechanizing a wrong rule makes it
worse, not better.**

The correction paid immediately: on its first real use the new verdict found a
genuine localised cost increase, which bisected cleanly to one commit.

## Small probes miss rare events, again, in the campaign that says so

Both new census columns were designed off scratch probes, and the census
corrected both:

```
                    probe                    census (1000 worlds)
displacement   48 worlds: 12.5% zero,   137/1000 (13.7%) zero,
               median 6, max 578        median 10, max 1924
tribute stock  36 worlds: ZERO zeros    13/1000 worlds hold none
```

The zero *rate* survived; the *tail* did not, off by 3.3×. And the tribute
probe's "0 zeros over 36 worlds" led to an explicit written conclusion — that
an inertness guard "could essentially never fire" — which is false on thirteen
worlds. Had the obvious floor been written from the probe, it would ship
broken.

This is The Confusion's three-in-a-thousand finding recurring inside the
campaign that cites it. The probes were not careless. **When the deliverable is
a distribution, sample size is not a cost-quality tradeoff; it decides whether
the instrument can see the thing at all.**

The practical consequence: the calibration tests' teeth are in a **span**
assertion, not an inertness floor, and the zero share is reported rather than
pinned — pinning the number that justified the migration would re-create the
defect one level up.

## What went well

- **Two defect classes, sorted before any code.** The ideonomy pass that
  charted the five reds on freeze-age × threshold-animacy put every red in the
  *mechanical-threshold* column and left the *derived* column occupied only by
  the two passing panels. That is what separated "too few samples" from "a
  rotted literal" and stopped four of five reds being pushed at the census.
- **Every retirement left a pointer.** Each deleted guard names the census
  column that took its question. Deleting a guard without saying where its
  question went is how a question gets silently dropped.
- **A column was declined for a stated reason.** An invariant sibling mirroring
  `raid-attribution-unresolved` was refused as structurally vacuous —
  `emit_history` already `.expect`s what it would assert. Following a precedent
  because it is a precedent would have bought a permanently-green column.
- **The probe that survives, survives for a reason.** `tribute_stock_agreement_probe.rs`
  is kept because it is the only place the census-visible stock can be checked
  against the bake's discarded flow. Same justification The Confusion used, and
  it is stated in the file rather than assumed.
- **Absorbing before the census, not after.** Main moved 28 commits mid-campaign
  (The Digest). Absorbing first meant the census was baked on the merged tree;
  absorbing after would have meant running it twice, since a generated artifact
  has no merge.

## The name collision was the second, and the first is written down in the book

This campaign was named "The Assize" only at close. It was named **The Tare**,
and a campaign of that name had merged eleven days earlier — so the chronicle
and retrospective written here **overwrote that campaign's two pages**, silently,
because both are keyed by campaign name and to git a rewritten file is an
ordinary edit.

Three things make this worse than an oversight, and all three are the campaign's
own subject matter.

1. **The metaphor collided too.** The earlier chronicle opens *"A tare is the
   weight you subtract to get a true reading."* Independently reinvented, word
   for word in substance. An evocative name that fits this project's register is
   exactly the name someone already used.
2. **`make preflight` cannot see it.** Its collision check is *both-sides-added*
   — a slug minted on the branch and on main since the merge base. A slug that
   already existed on main before the campaign began is invisible to it. The
   check is correct for what it enforces and reads as total.
3. **It had happened before, and the record is in the book.** `open-questions.md`
   describes The Reassay hitting "a naming collision with an unrelated,
   already-shipped campaign that shares this one's title — itself one more claim
   of absence ('the name is free') nobody checked." That sentence sits in the
   chapter this campaign was obliged to re-score, which is to say: the lesson was
   written down, in a document the Definition of Done required reading, and it
   was hit anyway.

**The scan belongs at the naming moment, not the close.** By close the name had
reached the spec filename, the plan filename, every in-code campaign
attribution, three registry rows, and — through metric `doc` strings — the
committed census `schema.json`, whose correction cost a canonical-box census
run. At the ledger entry it would have cost one `ls`.

## Follow-ups

- **`ρ > 0` has no demonstrated failure mode.** Retained as a directional
  record. If a future campaign gives that file a third mutation control, this
  is the assertion to test against it.
- **The `session_cost` indoor ceiling was raised 18 → 40 to accommodate
  per-cell colour** (`c25bb1d2`). The feature's price, not a defect — but the
  cost is real and unoptimised, and a per-cell colour cache is a legitimate
  future campaign. The bisect table is at the constant.
- **`each_target_region_gains_a_top_ranked_occupant` is still red by design**,
  and now known to have been failing where its own doc claimed it passed. It
  awaits `BIO-supply-drowns-niche`, whose diagnosis this campaign strengthened:
  all three target regions are topped by a sessile autotroph or a detritivore.
- **Seed 100 fell sharply in the-history regen** — occupied 131 → 65, restacked
  77 → 21 — against every other seed rising. Flagged, not explained;
  attributing one outlier would need its own bisect.
- **lefford carries stale state nobody swept.** Its main checkout sits at
  `552889db`, `hornvale-assay-census` is parked on a long-merged campaign
  branch, and an untracked `canonical/` directory sits inside the repo. Left
  alone deliberately — sweeping another session's worktrees is destructive —
  but it is accumulating.
- **The offence/defence raid-column watch item from The Confusion is untouched**
  and still owed; this campaign did not re-measure that ratio.
