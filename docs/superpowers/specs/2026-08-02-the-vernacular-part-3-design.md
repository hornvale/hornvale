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

## 7. What composes, and why it is worth building

A star's **colour** is nameable per culture — different peoples, different
words, same light. A star's **class** is `Void::Unnamed` — nameable by nobody
here. Same star, two registers, and after part 3 the code expresses both.

That is the parent spec's §3.1 demonstrated rather than argued, and it is the
most interesting thing this part can ship.
