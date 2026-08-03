# The Vernacular, part 3 — the world says it in its own words

**Status:** Draft for G3 review · **Date:** 2026-08-02 · **Branch:**
`campaign/the-vernacular-3` · **Parent spec:**
[The Vernacular](2026-07-31-the-vernacular-design.md) (§3.1 and §5 stage 3
govern; this refines them rather than replacing them)

Parts 1–2 made a phenomenon's *content* machine-readable and taught the
registry to declare a concept nameable by nobody. Part 3 makes the world
actually speak: text stops being stored, and starts being rendered for a
particular people at the moment someone reads.

## 1. The finding that shapes everything else

`ObserverContext` — everything a phenomenon producer knows — is
`{ place, time, lens, position }`. **No species, no culture, no lexicon**, and
constitutionally so: the phenomena channel hands consumers appearances, and a
producer must not learn who is looking (decision 0003).

Therefore:

> **`Phenomenon.description` cannot be made honest by rewriting it.** A single
> stored string is *by construction* not per-culture, so whatever prose it holds
> is either neutral or wrong.

That is why every leak in the parent spec's audit clustered on that field. It
was never a discipline problem; the field's *type* — one `String`, produced
without an observer — guaranteed it.

## 2. The mechanism

**`description` is deleted, not relocated.** `Phenomenon` becomes
`{ kind, referent, period_days, salience, venue }`. Text stops existing inside
the sim.

Rendering moves to the windows, each receiving a **speaker** from the
composition root — following the pattern `AlmanacContext::place_labels` already
establishes in its own doc: *"The composition root fills this… a window may not
reach back to the root."* The root supplies the speaker; the window renders.
Text is therefore **derived at the point of reading**, not stored one layer out.

Two channels, both per-speaker:

- **Referential and quantitative** → `ClauseSpec::from(referent)` →
  `realize_common`, with counts through `numeracy` gated on the speaker's
  counting rung.
- **Colour** → `color_naming::name_color(reflectance, illuminant, observer,
  speaker)` under `illuminant::daylight(&star)`, with `at_elevation` supplying
  the noon/dusk distinction `twilight_words` was hand-writing.

### 2.1 The render pipeline is a one-way state machine

```
[world state] --produce--> [referent] --realize--> [ClauseSpec] --render(speaker)--> [text]
                    ^                                                                  |
                    +------------------- NEVER (the one-way rule) ---------------------+
```

Every transition points forward. This sharpens what the parent spec's stage 4
lint is actually asserting: not "no prose here, no concept ids there" as two
rules, but **no backward edge in this machine**. Prose in the content register
and a concept id in the expression register are the same defect — an edge
pointing the wrong way — seen from opposite ends.

### 2.2 What has a channel, and what still does not

| channel | authored by | needs at render | exists |
|---|---|---|---|
| words (branch A) | registry + lexicon | speaker's lexicon | yes |
| quantities (B) | `numeracy` | speaker's counting rung | yes |
| colour (A/D) | The Pigment's spectral model | reflectance, illuminant, observer, speaker | yes |
| stance (D) | `account.rs` | speaker's disposition | yes, **out of scope** (render-ladder L3/L4, ledger #6) |
| spatial frame (C) | — | an observer-relative frame | **no** |

Branch C remains genuinely unserved. Part 3 does not build it; `figures.rs`'s
`region_word`/`describe` stay as they are, and the parent spec's §8 risk 2
stands.

**Colour applies to material referents only.** A moon and a star have
reflectance and a spectrum; an eclipse, a tide, a season and the ambient air do
not. The renderer must treat "this referent has no colour" as the common case,
not the exception.

## 3. What this does to the almanac — an editorial decision, made deliberately

Threading a speaker everywhere means the almanac is rendered in the flagship
people's own terms. It stops being an author's neutral reference and becomes
**an editorial apparatus around a culture's account**: headers, ordering and
salience labels stay English (the parent spec's branch E), while the content is
that people's.

This *sharpens* branch E rather than contradicting it. **Branch E is the
apparatus, not the document.**

**The document must name its speaker.** One world now has as many renderings as
it has peoples, and the committed artifact is a projection that picks one.
A projection whose choice is invisible reads as neutral fact. The almanac's
header states whose account it is.

## 4. Two couplings this introduces, both new

**4.1 The flagship choice becomes part of the artifact's identity.** The
almanac's prose now depends on which people is speaking, and the flagship is
derived from settlement placement (`windows/worldgen/src/lib.rs:5941`). So a
future campaign that moves settlement placement moves *all almanac prose* —
a coupling that did not exist when prose was placement-independent. This is
honest (the account really is that people's) but it must be declared, and §3's
named-speaker header is what makes it legible when it happens.

**4.2 Rendering may cost what the census cannot afford.** `lexicon_from`'s own
doc calls it *"almost all of the post-name-gloss census cost."* Threading a
speaker means a lexicon at every render site, and artifact regeneration renders
three seed-42 almanacs while the census renders thousands of worlds.

**This is the risk most likely to bite late, and it is measured before it is
designed around.** Plan 3b's first task measures `make rebaseline` wall time
before and after. The mitigation, if needed, is architectural rather than
clever: the speaker is built **once per world** and carried in the context, not
rebuilt per phenomenon. That is the same shape as the kernel's
"build a sampler once, sample many" rule.

## 5. Decomposition — three plans, one kind of movement each

Sequenced so that each plan's gate measures exactly one thing.

**3a — `star-class` becomes a concept id.** The parent spec's stage 3 item 1.
`domains/astronomy/src/facts.rs` commits `Value::Text("yellow dwarf (G)")`
while the registry declares `yellow-dwarf` nameable by no one. The class is
doubly redundant: derivable from mass, and `Star::t_eff` now agrees with it
(K/G at 5191 K, G/F at 5907 K).

*This is the only plan in the whole campaign that moves committed facts*, and
it goes first precisely so its epoch measurement under decision 0084 runs
against a tree that is not simultaneously churning every rendered string.
**Gate: facts move — measured and declared. Artifacts substantially still.**

Do **not** delete the fact, and do **not** drop `class_name` from
`scene/neighbors/v1`: considered and rejected in the parent spec, because the
scene is an out-of-world instrument (decision 0022) and the field is a
cross-repo contract.

### 5.1 What plan 3a actually contains — three findings from writing it

Writing 3a's plan surfaced three things the section above did not know. All
three narrow or redirect it; none changes the sequencing rationale.

**(a) The Book already re-centres, and the sentence in question is the
author's.** `book/src/gallery/the-book.md` renders three registers side by
side: a culture's account (*"Xoaboa is **the earth** with two moons. The moons
cross because they are Boko's kin. The day returns, as all things return."*),
an italicised ground truth (*"In truth, Xoaboa is a planet orbiting a
yellow-white dwarf (F)…"*), and a priesthood's teaching. **No creature says
"orbiting."** The vantage is not leaking — §3.1's registers are already visible
in a committed artifact, which is a stronger demonstration than this campaign
had realised it owned.

Nathan's ruling: Earth's spectral taxonomy **may** stand in the ground-truth
register, on the campaign's own line — *units are the author's frame; names are
the world's*, and an italicised "in truth" is the author speaking. So 3a
changes the **fact's value** to a concept id and renders it back out as
"a yellow-white dwarf (F)" from that id. Nothing a creature says changes; the
ledger stops holding a name.

**(b) The scene and the lab metric are unaffected.**
`windows/scene/src/lib.rs:941` and `windows/lab/src/metrics.rs:735` both read
`system.star.class_name` — the **struct field**, not the fact. So no
`scene/*/v1` schema is touched and no census metric moves. Verified, not
assumed.

**(c) 3a touches the knowledge/Echo round-trip, which §5 said it would not.**
`windows/book/src/lib.rs:1992`'s `fact_for` **parses rendered prose back into a
fact** — `"orbiting a yellow-white dwarf (F)"` → `(STAR_CLASS, Text(...))`.
That is a backward edge in §2.1's state machine, deliberate and load-bearing
(The Echo's transfer law). Changing the fact's value breaks the round-trip
unless `fragment_for` and `fact_for` move together, so **keeping that pair
inverse is in 3a's scope**: render id → prose, parse prose → id. This is the
campaign's central defect in its most load-bearing form, and 3a does not remove
it — it keeps it working while the value underneath becomes honest. Removing
the backward edge is stage 4's job.

**3b also inverts the direction of authority** (Nathan, 2026-08-03). Part 3a
left `Star.class_name` — display *prose* — as the authority: `facts.rs`
string-matches prose → id (`class_concept(&system.star.class_name)`, guarded by
`.expect()`) to decide what to commit. That was the right minimal change for a
plan gated on moving one fact, but it inverts the campaign's thesis: **the
ledger's content is currently downstream of a rendering decision.**

3b makes the **id primary and the display derived**. Concretely: the producer
should decide the concept from the physics it already has — mass for the star,
the `NeighborClass` variant for a neighbour — and `class_display` should be the
only path from id to prose. `class_name` then stops being an authority and
becomes one more rendering, which is what every other string in this campaign
became. Two `.expect()`s disappear with it, since a `NeighborClass` maps to a
concept totally rather than through a string that might not be in a table.

Note what this does to the whole-branch review's C1: with the id derived
directly, `SPECTRAL_CLASSES` stops being a lookup the ledger depends on and
becomes purely a render table — so the "two hand-maintained copies" hazard that
required a test to close is dissolved structurally rather than guarded.

**3b — delete `description`; thread the speaker.** ~10 readers, ~20 producers.
Renders through `ClauseSpec` + `numeracy`. Includes §4.2's cost measurement and
§3's named-speaker header.
**Gate: zero committed facts move; artifacts move once, substantially.**

**3c — the colour path.** `daylight_words`, `twilight_words` and
`neighborhood.rs`'s `class_color` collapse into `name_color` under the star's
own illuminant.
**Gate: zero facts; content changes, per culture, visibly.**

`SkyReport`/`ClimateReport` (15 readers) are a separate surface and are **not**
in these three. They come after, or in a part 4.

## 6. Preregistered measurement

Frozen before the code, per decision 0016.

**3a — the epoch question.** Prediction: changing `star-class`'s value from
prose to a concept id moves committed facts, and the derivation that moved is
the fact's own value and nothing downstream of it — no name, no deity, no
settlement. If anything downstream moves, the class was load-bearing somewhere
undeclared, and that is the finding.

**3b — the cost question.** Prediction: per-world speaker construction keeps
`make rebaseline` within **1.25×** its current wall time. Above that, §4.2's
hoist is mandatory rather than optional. Measured, not estimated.

**3c — the payoff.** Prediction: two peoples with different Berlin & Kay depths
render the *same sky* with different colour words, in a committed artifact —
the same result The Pigment measured for an outcrop, now for the sky. A null
here (both peoples say the same thing) is a finding about the star's colour
landing in a basin both lexicons reach, not a failure.

### 6.1 Readout — 3a

Written *alongside* the frozen predictions above, never over them: decision
0016's preregistration is worthless if the numbers can be edited after
unblinding. 3b and 3c are not yet measured.

**Measured at** `d2c1f8cf` — the last commit in 3a to move a committed value —
against a seed-42 ledger captured before task 1, whose facts are identical to
the committed fixture at `6db788ec`. Both ledgers hold **26309** facts;
comparison is index-aligned, fact by fact.

**Six facts moved, and only six.**

| predicate | count | movement |
|---|---|---|
| `star-class` | 1 | `yellow dwarf (G)` → `yellow-dwarf` |
| `neighbor-class` | 5 | `red giant` → `red-giant`; `sun-like star` → `sun-like-star`; `orange giant` → `orange-giant`; `red dwarf` → `red-dwarf`; `white dwarf` → `white-dwarf` |

Nothing downstream moved — no name, no deity, no settlement; seed 42's village
is still `Goodogododaga`. Artifact drift across all three tasks was confined to
`book/src/reference/concept-registry-generated.md` (one predicate's doc string)
and `docs/audits/type-audit-report.md` (tags for the new `_public` mirror).
**`book/src/gallery/` did not move at all**, and the author's ground-truth
sentence in `the-book.md` — "orbiting a yellow-white dwarf (F)" — is
character-identical. That is the round trip working as designed: the ledger
holds the registered id, the author's register renders the Morgan–Keenan
display back out of it.

**The prediction held**, as stated, with nothing to qualify.

**Why nothing downstream moved — read off the source rather than inferred.**
Two paths consume a `star-class` *value*, and they do not move for two
different reasons — neither of which is "no reader exists."

The first cannot fire. `chorus_ground` flattens the fact into a `GroundFact`,
and `filtered_disposition` can carry a value's text into a culture's account.
It never fires, for two structural reasons. `observability_table()` gives
`star-class` `Requirement::Instrumental`, which returns `Lost(BeyondCapability)`
without reading the object at all, and `NeededConcept::Fixed("star")`, so even
the lexicon check tests the constant `"star"` rather than the fact's value.
The only requirement that *does* read the value — `Taxonomic` — is assigned to
`star-class` solely by `pathological_params()`, the dial's gibberish pole,
where `world_carving: Some("earth")` substitutes every text fact to the same
target regardless of what the truth text says.

The second fires on every world, and was kept in step rather than never
running. `windows/book`'s `render_world_margin` reads `entry.fact.object` for
every `Lost` fragment — which `star-class` always is at the floor — and
renders it through `fragment_for`; `book/src/gallery/the-book.md:44,61` are
literally its output. It did not move because `fragment_for` was updated in
the same task to render the registered id back through
`hornvale_astronomy::class_display`, recovering the identical Morgan–Keenan
string the old prose fact held. The round trip is preserved by construction,
not by the consumer being unreachable.

That construction has one observable edge: `class_display`'s lookup is by
concept id, so an *old saved `world.json`* whose `star-class` fact still
holds the pre-change prose text (e.g. `"yellow dwarf (G)"`) finds no match,
and both `fragment_for` and `explain_sky`'s `None` arm drop the star clause
silently rather than erroring — a deliberate, code-commented choice, since
worlds re-derive byte-identically from their seed and are not expected to
carry old ledgers forward, but it is the one place this change is observable
against an existing file, and is recorded here for that reason.

`neighbor-class` has no value reader at all outside the `--neighbor` pin
round-trip; the census's `brightest-neighbor-class` metric recomputes its
kebab-case from the live `NeighborClass` enum, never from the ledger. The
zero-diff `book/src/laboratory/` is the independent check on all of that:
`the-chorus.study.json` is regenerated by `scripts/regenerate-artifacts.sh`, so
a value-dependent account would have shown up there.

**The epoch question: no epoch is owed.**

Decision 0084's test is whether a *derivation* moved. It did not. A spectral
class is a pure function of the star's drawn mass, and the mass draw is
untouched — same seed, same mass, same bucket boundaries (`class_name_of_mass`,
extracted in task 1 and proved to move nothing). What changed is which of two
strings the ledger writes for that one unchanged class: the registered concept
id instead of its author-frame display. That is a re-spelling of a derived
label, not a moved derivation — the same shape as 0084's own `room/furnishing`
declination, which recorded the decline rather than the bump.

Declaring one anyway would be the **empty epoch** 0084 names as a defect: a
permanent manifest row asserting a discontinuity in derivation history that did
not occur, which every world written afterwards would carry as a fiction. There
is also no label to bump. Seed-derivation labels are declared per *algorithm*
and never in advance (decision 0083); no algorithm changed, and astronomy's
stream labels are untouched — `STAR_MASS` and `NEIGHBORS` above all. Minting a
label for the class *naming* would be exactly the pre-declaration 0083 forbids.

**One consideration weighed and deliberately not allowed to decide it.** This is
a committed-fact value change at the determinism boundary. Nothing in-repo
carries either predicate across the wasm ABI — `clients/` was grepped for
`.rs`, `.ts`, `.js` and `.json` with zero hits on `star-class` or
`neighbor-class`, reproduced independently at this readout — but a sibling repo
reading a *released world JSON* directly would see the value change. So this is
consumer-visible without being an in-repo break. That is a **compatibility**
fact and is recorded here as one; it is not an epoch. 0084 asks whether the
derivation moved, and the answer to that question does not change with who is
reading the output. The right response to a consumer-visible value change is the
additive-or-versioned discipline the scene schemas already carry, applied at
whatever release publishes it — not a derivation epoch that would misdescribe
what happened.

**What would have changed the verdict.** Any committed fact outside
`star-class`/`neighbor-class` moving — a name, a deity, a settlement — would
have meant the class text was load-bearing in an undeclared derivation, and the
epoch would have been owed and named after *that* derivation. So would a moved
metric or census golden, which is 0084's EPOCH case by definition. Neither
happened, and both were checked rather than assumed.

**One guard added while deciding.** `facts.rs` `.expect()`s that every
`NeighborClass` display is in `SPECTRAL_CLASSES`, and nothing asserted it: seed
42 draws five of the six variants, leaving `BlueGiant` correct only by
inspection, so a drift between the two tables would be a worldgen panic on the
first seed to draw it. `every_neighbour_class_is_in_the_spectral_table`
(`domains/astronomy/src/neighborhood.rs`) now covers all six, and was shown to
fail — naming the orphaned variant — with a pair removed from the table.

## 7. What composes, and why it is worth building

A star's **colour** is nameable per culture — different peoples, different
words, same light. A star's **class** is `Void::Unnamed` — nameable by nobody
here. Same star, two registers, and after part 3 the code expresses both.

That is the parent spec's §3.1 demonstrated rather than argued, and it is the
most interesting thing this part can ship.
