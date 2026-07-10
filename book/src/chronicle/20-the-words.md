# Campaign 20: The Words

**July 2026 · 22 commits · outcome: complete, merged — gives the world a
concept layer, a per-language lexicon with real history, and a truthful
gloss on every name it mints; the preregistered collision-rate claim,
measured twice, still fails, honestly**

## What was attempted

The Tongues gave both peoples a real phonology and drew a bright line in
front of it on purpose: a generated name had shape and voice and nothing
more, no gloss, no etymology, nothing a listener could look up. The Words
sets out to step over that line — deliberately, and only as far as the
line was drawn to allow. It asks the kernel to register a closed inventory
of what the world's peoples can mean (a `ConceptDef` beside the existing
`PredicateDef`), gives each species-culture a small set of authored
Swadesh packs over that inventory (a universal stratum, a Berlin & Kay
color ladder keyed to the perception vector that already made a kobold's
eyes different from a goblin's, a luminance ladder that trades off against
it, and placeholder body/kin packs banked against vectors this project
has not authored yet), and asks every word a language actually holds to
carry its own history: a drawn proto-root, aged through that language's
own ordered sound-change cascade, own-line and never shared between
goblin and kobold. A settlement's name, a deity's name, and a deity's
epithet — already real generated sounds since Campaign 16 — were asked to
carry real generated *meaning* too: a truthful compound over the entity's
own committed facts, never invented for the occasion. And, because a
design that changes a naming pipeline this deeply invites exactly the
kind of collision regression a phonology-only campaign would not, the
plan preregistered a Lab claim about the collision rate before the census
that would measure it ran (ADR 0016) — a claim this campaign's own
census went on to fail, twice, and reported as failed rather than
loosened.

## What landed

**Concepts are promises, not words.** A `ConceptDef` — a kebab-case id, an
owning domain, a one-line gloss, a kind drawn from a small closed set —
sits beside the predicate registry every fact already had to obey. Each
domain registers the concepts it already represents at construction, the
same discipline it keeps for predicates; a species may hold a concept and
still have no word for it, exactly as a settlement may exist unnamed.
Registering a concept asserts only that the sim represents the thing
closely enough for a lexicon, a fact, or a player's question to someday
resolve against it — a promise, not a lexicalization.

**A Swadesh core built as packs, not one universal list.** A single
"starting vocabulary" would claim every species carves the same
distinctions out of experience, a claim [Perception](../domains/perception.md)
already spent a whole chapter disproving. The shared core is instead a
set of small, closed packs: a universal stratum (water, stone, sun,
night, fire, the basic kin terms); a color pack that descends the
Berlin & Kay implicational ladder only as far as a species' own eyes
carved the distinctions, so kobold's keener night vision buys it more
words for kinds of dark and fewer for kinds of color, a gap and an
abundance read as the same fact twice; and placeholder body and kin
packs, banked exactly as voice-loudness was banked in Campaign 16,
unchanged on the day a body vector exists to re-key them.

**Own-line descent.** Every word a language lexicalizes starts as a
drawn proto-root and ages through that language's own two-to-four-rule
sound-change cascade — lenition, fortition, a vowel shift, cluster
simplification, final-segment loss — landing only on phonology the world
already shipped a mouth for. Regularity is not a hope but an invariant a
computer checks: the Neogrammarian hypothesis, restated as a property
the world enforces, where replaying a cascade over a recorded proto-form
must reproduce the recorded modern form exactly, every time. Goblin and
kobold each get their own proto-tongue and their own cascade, run once,
independently — no tree drawn behind the two lines this campaign grows
in parallel.

**Exposure and two gap provenances.** Every (language, concept) pair
resolves to a root (a concept the species' world saturates), a compound
(a concept merely adjacent — a transparent joining of roots already
held), or a gap, and a gap always carries a reason: **experiential**,
recounting to a ledger fact (no kobold settlement touches coast, so
kobold has no word for the open sea), or **perceptual**, recounting to a
vector dimension that overrides exposure entirely (a kobold steeped in a
blue sky every day of its life still has no word for blue, because pack
depth is checked first and kobold eyes never carved that far down the
ladder). A lexicon is allowed to be silent, but never silently.

**Glossed names.** A settlement's name now draws from its own cell's
biome, its notable sky, and the phenomenon its presiding belief reveres;
a deity's name and epithet draw from the phenomenon it mythologizes and
the sentiment religion already derived for it — a fourth rendering
alongside a name's romanization, IPA transcription, and espeak
formulation, the same "spellings are views" discipline restated once
more. `dictionary`, the REPL's `word`, and `why`/`recount` all read this
new content back at display time (Task 11); a keystone test suite proves
the whole graph — glosses, derivations, exposure, pins — holds at seed 42
before any census runs at scale (Task 10).

**The collision-rate claim, measured, failed, and left failed.** [Study
010, the Census of Words](../laboratory/study-010.md) preregistered five
Lab claims. H1 (name-gloss truthfulness), H2 (lexicon regularity and
exposure soundness, both species), and H3 (the pack-depth ordering) all
confirmed exactly, with zero exceptions across 10,000 seeds. H4 — that
glossed compounds would keep the name-collision rate below twice the
Tongues-era pinned figure (4.678%) — did not survive contact with the
census: pure site-concept compounds pigeonholed catastrophically (a
mean 86.8% collision rate, the distinct `(biome, phenomenon)` pairs
available to a species numbering under 20 against well over 100
settlements to name). This was ruled a design defect, not a measurement
artifact, and fixed twice: first by adding a drawn, per-settlement
proper-name stem alongside the site-concept compound (down to
10.7–11.0%), then by widening that stem from 1–2 to 2–3 syllables once
the residual rate traced to stem entropy (down to 4.91% at the
CI-guarded 500-seed population, 4.94% at 10k — a ~17.6x improvement over
the first measurement). The bound stood at 4.678% throughout. The final
figure misses it by 0.24 percentage points. Per ADR 0016, the bound was
not widened to let the claim pass; the measured rate is pinned as-is and
H4 is recorded as failed. Whether the bound itself — inherited from a
free-stem era this campaign's own design retired — was ever the right
number for a glossed-compound regime is the open question the study
hands to the campaign after this one.

## What was learned

- **A preregistered bound is a promise about the future, not a knob.**
  The easy move, mid-campaign, would have been to treat 4.678% as
  negotiable once the first measurement made it clearly unreachable
  under the original design. It stayed fixed. The design changed twice
  instead, and when the second fix still missed, the honest number
  shipped rather than a widened bound dressed up as vindication.
- **Measuring a design decision beats trusting it, even inside one
  campaign.** The free-stem-to-glossed-compound change was reasoned
  about in the spec and looked safe on paper — two site concepts still
  sounds like plenty of entropy. The census caught what the reasoning
  missed (a pigeonhole problem hiding in plain sight) before any world
  shipped it, exactly the discipline the Lab exists to provide.
- **A closed vocabulary earns its keep by staying closed.** The concept
  registry, the Swadesh packs, and the sound-change rule family are each
  small and authored on purpose, the same posture the psychology,
  perception, and articulation vectors already established — every
  addition has to be argued for, not folded in because a struct had
  room.

## Deferred, deliberately

A syntax that lets two words assemble into a sentence rather than sit
together as a single compound noun, and the case, tense, and agreement
machinery it would carry; pidgin, creole, loanwords, and language-shift
dynamics, which need two peoples who have actually met — this campaign
draws its own bright line in front of exactly this, on purpose;
meaningful toponyms, waiting on terrain features worth naming; the
richer, generative oral-formulaic teller that Campaign 16's
content→render seam was built to make room for; a paralinguistic layer
for the calls and cries that are a different kind of sound than speech
entirely; and per-individual idiolect, excluded by the same closed-vector
posture that already excludes per-individual psychology and perception.

## Artifacts

[Study 010: The Census of Words](../laboratory/study-010.md) — the five
preregistered claims, three confirmed exactly at 10k scale, one (the
collision rate) measured twice and failed honestly both times. [The
Language chapter](../domains/language.md) — the concept layer, the
Swadesh packs, the etymology cascade, exposure, and glossed names, with
the campaign's own bright line drawn again at the chapter's close. [The
dictionary reference](../reference/dictionary-generated.md) — every
concept's gloss, word, proto-form, and derivation, per species, drift-
checked from seed 42. `cli/tests/` — the structural-invariant keystone
suite proving the glossing and exposure graph holds before any census
runs at population scale.
