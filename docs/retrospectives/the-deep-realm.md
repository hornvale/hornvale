# Retrospective — The Deep Realm

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-deep-realm.md): the underworld is a
derived graph of addressed chambers, and its shape is a re-expression of one
terrain field and a fair coin.

## The dominant lesson: for an enum widening, the compiler is the enumeration

Task 1 widened two enums. Counting the exhaustive matches that had to change:

| source | count | notes |
|---|---|---|
| the plan's file list | 3 | and **two of the three were wrong** |
| reading each named file before dispatch | 5 | found `windows/vessel/`, which the plan never mentioned |
| the compiler, during implementation | 7 | found `biome_class_of_formation` at the composition root |

The plan asserted that `biome.rs`, `crops.rs` and `variants.rs` each matched
`Formation` exhaustively. `biome.rs` only *constructs* Formations; `crops.rs`'s
`arable()` is a `matches!` fallthrough that already returns the correct answer
for a cave. Neither needed touching. Meanwhile the file it omitted entirely —
`windows/vessel/src/session.rs` — held a six-arm `Stratum` match with no
catch-all.

*A grep-derived plan is only as complete as the grep* was already a standing
lesson, and it is not the right one here, because **no amount of grepping would
have been sufficient either**. The right formulation is narrower and more
useful: for a change whose blast radius is defined by the type system, a file
list is a warm start and the compiler is the ground truth. Write the plan's list
as an orientation aid, say in the plan that it is not the sweep, and instruct
the implementer to let `-D warnings` finish the enumeration — and never to
silence an exhaustiveness error with a wildcard.

## I wrote "measure the blast radius, do not predict it" and then predicted it

Before dispatching Task 6 I found `xorn` four times and `rust-monster` three
times in the committed seed-42 world fixture, concluded that re-authoring their
niches would move world identity, wrote that into the plan as a hazard, and
brought it to the owner as a decision worth his time. He approved proceeding.

**World identity did not move.** Those occurrences were a concept-registry entry
(`"xorn-kind"`) and a `species-name` naming fact. Neither is placement. Fauna
dominance is computed live; only peopled, psyche-bearing species commit
placement facts at all.

The instruction that caught this was one I had written in the same paragraph —
*measure it, do not predict it* — and the implementer followed it and reported
the truth. The error is the same shape as the one this campaign corrected in its
own spec on day one: the spec claimed a grep for `Cave`/`CaveKind` returned "no
consumers," when three were live and two of them simply never named the type.

**The generalisable form: a symbol appearing in a committed fixture tells you
nothing about which *fact* carries it.** Grep finds strings; blast radius is
about relationships. The cost here was small — an unnecessary decision brought
to the owner — but the same inference in the other direction would have shipped
a silent re-pin.

## Pre-dispatch plan review earned its cost before every single task

Not one task went out unchanged. Reading the actual files first found:

- **Task 1** — three defects (above).
- **Task 2** — the plan said to copy a composed-label pattern and never said
  what the key *was*. `StreamLabel::dynamic` hashes the string, so the first
  implementer's spelling would have been permanent, and nothing flagged the
  choice as load-bearing.
- **Task 4** — the override seam had **nothing to override**. `Chamber` was
  `{ addr, stratum }`; `addr` is the key and `stratum` is a pure function of
  `addr.band`. The task would have demonstrated the seam against nothing.
- **Task 5** — the plan anticipated two refusal outcomes; the measured
  substrate has three.
- **Task 6** — the world-identity hazard (wrong, but the instruction to measure
  was right and is what corrected me).

The Hollow's retrospective observed that every plan defect it hit originated in
plan text, and recommended reading the file at plan self-review. This campaign
moved that step to *pre-dispatch* — after the plan is written, immediately
before the implementer sees it — and it caught something every time. The two
are not the same step: self-review checks the plan against itself, and this
checks the plan against the code as it stands *today*, which for a
five-day-old plan on a moving main is a different question.

## An inherited diagnosis is a hypothesis — including a subagent's

Two of the six implementer reports contained a confidently-stated causal claim
that was wrong. Both times the *measurements* were right.

**The affect-trace drift.** Task 6 attributed a 1e-4 golden drift on unrelated
creatures to "shared resource-competition normalization." There is none:
`per_species_suitability` hoists its supply fields out of the per-species loop,
so species are independent there. The real path runs
`niche → suitability → the demography coexistence fit → the shared predator/prey
pressure fields → every other creature's danger-sense and hunger → its affect`.
The re-pin was correct either way; the *explanation* was not, and the two send a
successor to different files.

**The falsification scoring.** Task 8 measured beautifully and then scored spec
§7 as "not triggered." Read clause by clause, two of its three clauses fail.

This is the standing lesson turned on the agents rather than on a predecessor
campaign: **keep a report's measurements, re-derive its attributions.** It cost
about ten minutes each time and both corrections changed what the campaign
concluded. The pattern is consistent enough to plan around — a reviewer should
budget explicit time to re-derive every *because* in a subagent's report while
taking its numbers at face value.

## A compound rate is not a measurement

Task 3's connectivity guard reported "410/1000 entrances reach a chamber (41%)"
and it read as alarming. It was not a connectivity measurement at all:
`passages_from` returns empty both when a chamber is *isolated* and when the
entrance address *holds no chamber*, and those mean opposite things. Decomposed:

```
  entrance chamber EXISTS       515/1000 = 0.5150
  of those, reach a neighbour            = 0.7961   <- the actual connectivity
```

Connectivity is healthy and lands near the 0.75 the density model predicts. The
alarming number was the *other* half — and that half turned out to be a design
statement worth naming (roughly half of all caves are sealed), which then
changed what the descent verb had to say.

**Before believing a rate, ask what its denominator mixes.** A single number
over a population that contains two different failure modes measures neither.

## Preregistering a prediction worked, and cost nothing

Before Task 8 ran, the ledger recorded: `EXISTENCE_DENSITY = 0.5` over a fixed
16-address lattice should produce a binomial chamber count with narrow relative
spread, structurally close to the falsification. It recorded a second thing too
— that H2's own wording ("chambers per cell is heavily zero-weighted") would
read TRUE for a trivial reason, since 88% of land cells hold no cave, so the
statistic had to be reported per *cave* as well as per *cell*.

Both landed. The measured coefficients of variation matched theory to three
decimals, and H2 on the per-cell statistic did pass for exactly the trivial
reason predicted. Writing the prediction down beforehand is what made the result
a *confirmation* rather than a post-hoc "we knew" — and, more usefully, it is
what made the instrument report the right statistic in the first place, because
the prediction shipped in the dispatch as an instruction.

The cost was two paragraphs in a scratch file. This is cheap enough to be
routine, and it is not what decision 0016 currently asks for — 0016 preregisters
a study's hypothesis before the code that would move it. This is smaller and
later: predicting what a *specific run* will show, immediately before running it.

## A dead branch came from my own plan text

I wrote into Task 4: *"`Made` is absorbing. Assert this on the resolver."* The
implementer did exactly that, correctly. The result is a branch —
`resolve_origin`'s `default == Made` case — with no live caller, because
`chamber_at` always passes `Found` and this campaign ships no writer.

That is the precise shape the predecessor campaign existed to remove, arriving
in new code written by the process that had just finished writing about it. The
Hollow said the same thing about itself, one campaign earlier. **Naming a bug
class does not confer immunity to it, and a plan author is not exempt from the
class they are guarding against.**

It was kept rather than removed, because the invariant is real and the digging
campaign will need it — but The Hollow's own remedy was applied: name a derived
thing's first consumer in the same campaign, *or say plainly that it has none*.
The function now says it has none, names its future consumer, and cites what
that campaign will read. A stated deferral is a different object from an
unnoticed dead branch.

## A correction to The Hollow's handoff

Its retrospective states that a branch carrying a duplicate registry row is
"the class `make preflight` structurally cannot see."

**`make preflight` sees it.** Its both-sides-added slug check named
`MAP-cave-model-miscalibrated` explicitly, before the merge, in the run that
opened this campaign. Recorded so the claim does not propagate: the check
exists, it works, and it is the reason the duplicate never landed.

## Operational notes

- **The gate ran clean at every task boundary** and never once caught something
  the per-task verification had missed. Six full gates at ~5-6 minutes each. On
  this evidence the per-task `-p <crate>` runs plus the pre-commit hook are
  carrying nearly all the weight, and a gate every *two* tasks would likely have
  been sufficient. Worth watching rather than acting on yet — one campaign is
  not a sample.
- **`Regolith` occurs in 0 of 55,947 caves**, which made H3's mutation
  fabricate a value the live generator cannot produce. The implementer handled
  this properly: it extracted a *real* terrain-authored cave and downgraded a
  copy of that same cave, rather than fabricating one in isolation. It also
  mutated the production budget gate, confirmed both halves reddened (11/11 and
  12/12 identical counts), reverted, and confirmed green. That is the strongest
  form of this evidence and it should be the template.
- **`windows/worldgen/tests` cannot reach `windows/vessel`**, so the proof that
  the vessel passes terrain's real budget through to the lattice is a
  code-reading argument rather than an assertion. Disclosed at the test.

## Confidence Gradient

`book/src/open-questions.md` was checked against this campaign's territory.
**No bet moved — N/A.** Its terrain bet concerns coastline shape; its
game-layer bets concern the possession loop's liveness, which this campaign
extends with two verbs but does not re-score. The reachability finding is about
*distance to a feature*, which no current bet stakes a claim on.

## Follow-ups

- **The depth weld is now the campaign's own named successor, with a number.**
  The only place-character the underworld has arrives through a three-valued
  depth budget welded to the existence gate. Splitting it is the one lever with
  measured leverage.
- **Reachability.** Seven of thirty flagship starts cannot reach an enterable
  cave. Nothing was tuned; the levers belong to terrain and were just
  calibrated. If this is to be addressed it needs a campaign that can argue it
  on mechanism.
- **The `CaveKind`↔`Formation` correspondence is checked one direction only.**
  A `Formation` cave variant with no backing `CaveKind` passes silently, and the
  open idea of environment-keyed cave kinds (sea, glacial, biogenic) is a named
  future violator.
- **Digging must eventually create chambers, not merely re-origin them.** The
  override seam is a content lens; existence deliberately ignores it.
- **`resolve_origin`'s absorbing rule is untested against a sequence.** With one
  override per address in a map, "its own *latest* override fact" is trivially
  satisfied.
- **`BandKind::Underneath` is carried and never occurs.** Deliberate — the
  address ladder must absorb a depth-weld fix without relocating anything — and
  asserted empty so a change is noticed.
