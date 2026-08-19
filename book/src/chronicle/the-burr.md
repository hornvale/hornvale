# The Burr

A burr is a rolled /r/ — the exact segment seventeen of the eighteen shipped
tongues did not have — and also what we call a regional accent. The campaign
took both readings as its subject and shipped the second by way of the first.

```
awk -F'|' '/^## /{s=$0; sub(/^## /,"",s)} NF>6 && $4 ~ /[A-Z]/ {
  w=$4; gsub(/ /,"",w); if (w ~ /[lrLR]/) c[s]++; t[s]++ }
  END{for (k in t) printf "%-16s %3d/%d\n", k, c[k]+0, t[k]}' \
  book/src/reference/dictionary-generated.md
```

Every tongue but one returned zero. Kobold, the sole exception, was also the
only species whose `exotic: Trill` had ever been authored by hand — because
`exotic_manner` filed the trill alongside clicks and ejectives, gating an
ordinary alveolar consonant behind the same tier as two genuinely marked ones,
and the sonority term in `keep_probability` was monotonically hostile to every
sonorant for every species quiet enough to want one. Seventeen tongues could
not produce a liquid because the model would not let them draw one. That was
the proximate defect. The deeper one, found only after the proximate one was
repaired, is the campaign's real subject.

## A claim stated so it could be wrong

The campaign's thesis was written into its own spec as a falsifiable
sentence: **a tongue's perceived character is a discrete property of which
rules build it, and is not reachable by tuning the continuous articulation
vector.** Parametric versus typological — a knob versus a rule — and the
spec committed, before any code moved, to a classifier that would measure
the claim rather than a chronicler asserting it after the fact.

That classifier is a character-trigram model trained per tongue over its own
generated wordlist, scored on held-out **assignment accuracy**: given a word,
how reliably can the tongue that produced it be named. Its baseline, measured
against the committed dictionary before a single generation rule changed, was
**0.7201897018970189** — 72% on an eighteen-way task against a 5.6% chance
floor. That number complicated the campaign's own premise on the way in. A
machine could already tell these tongues apart three-quarters of the time; a
reader could not tell them apart at all. Those are different claims, and only
one of them was ever in doubt — but the headroom for the campaign's central
prediction was 0.28, not the near-unbounded room a lower baseline would have
offered, and the spec was amended to say so before Stage 2 ran rather than
after.

## The global repair, and the negative result that proves the point

Stage 2 ungated the trill and added `ensure_minimum_sonorants`, a
profile-conditioned floor guaranteeing that any tongue whose bundle calls for
a sonorant gets one, deterministically. It is a real defect repair,
independent of everything that follows, and it moved every quiet species'
phoneme inventory toward holding a liquid — uniformly, by construction,
because the floor does not know which family it is talking to.

The readout was a fall. Assignment accuracy went **0.7202 → 0.6795**, down
0.041, in the direction opposite the campaign's own prediction. Liquid
coverage in the dictionary did not move at all — still 3 of 18 tongues, no
elf, no dwarf — because the floor changes a species' *inventory*, and the
inherited lexicon is capped by the *proto's* phonotactic templates: proto-elf
now genuinely contained /r/ and its onset templates were `sibilant, stop,
nasal`, with no slot a trill could ever fill. A segment a language can say
and a segment its words actually contain turned out to be different claims,
and the gap between them is where Stage 2's whole effect went.

The fall is not a setback to the thesis; it is the sharpest evidence for it,
delivered by failing. A uniform change applied to a genuinely diverse
population is a homogenising force — it makes eighteen different inventories
resemble each other, which is precisely what a distinguishability classifier
penalizes. Parametric, applied globally, flattens. The question the rest of
the campaign existed to ask was whether the opposite move — the same
capability, granted per family instead — would do the opposite thing.

## The bundles, and the contrast that is the thesis

Stage 3 shipped `Typology`: a named, authored row per family — morphology,
onset law, coda law, harmony, orthography — rather than four free-standing
knobs. Four bundles, each exercised by at least one roster family:
`templatic` for the dwarves, `sonorant-open` for the elves, `isolating-tonal`
for the dragons, `concatenative` — today's unmodified engine, the campaign's
own control — for goblinoid, plant, and the unfamilied kinds. The elf
bundle's decisive addition was an onset law requiring a sonorant in the
second consonant slot **by construction**, not by draw — the piece a coda
law alone could never buy, because a coda law lengthens a word's tail while
the region this campaign was chasing is mostly about how a word opens.

The readout reversed Stage 2's sign entirely:

```
proto-elf roots carrying a liquid       0    ->  102
dictionary liquid coverage              3/18 ->  6/18
assignment accuracy                0.6795 -> 0.7900   (+0.110)
```

All four newly-audible tongues were elf family, and not marginally — Drow
49/91, Snow-elf 48/88, High-elf 44/85, Sea-elf 43/83, roughly half of every
elf lexicon now carrying a liquid where all four had none. The contrast with
Stage 2, measured on the same instrument two tasks apart, is the campaign's
headline result:

| Change | Shape | Accuracy | Lexical effect |
|---|---|---:|---|
| Stage 2, the sonorant floor | global, uniform | −0.041 | none |
| Stage 3, the typology bundles | per-family, authored | +0.110 | half the elf lexicon |

A parametric change homogenised a diverse population; a typological one
differentiated it. The thesis was stated as a claim that could be wrong, and
it was not — but it took a failed measurement, not merely a successful one,
to say so with the same confidence in both directions.

## Root-and-pattern, and a tone the dragons cannot yet speak

Stage 4 built the one piece of genuinely new generative machinery in the
campaign: for `Morphology::Templatic`, each concept receives a three-
consonant skeleton assigned injectively over the concept universe, and a word
surfaces by threading a vocalic template through it — the shape a reader
would recognize from Semitic and Khuzdul-style morphology, arrived at
independently rather than fit to either. The regenerated dwarf dictionary
shows the mechanism working end to end: *blood* is **Napad** /napad/, *bone*
is **Jagap** /jagap/, *fire* is **Vadad** /vadad/, *child* is **Sapaj** —
clean, alternating consonant-vowel skeletons, visibly templatic rather than
merely pronounceable. This stage owed the campaign's only save-format
event, `ROOT_EPOCH` `v3` → `v4` — every proto root in every world reseeded,
justified specifically by Stage 4's change to `assign_proto_roots` itself,
not by the earlier phonology repairs, which reseed the draw without ever
owing a new label.

Stage 4 also reached the tone tier that Stage 1's own audit had found built,
tested, and never once exercised — every shipped species carried
`tonality: 0.0`, so `Tonogenesis` had been dead code by construction since
the day it shipped. Raising draconic's family-level tonality to 0.7 makes
the proto-draconic etymon carry a real second tone: the proto for *blood* is
**Nánken** /na˥nken/. The tier fires — the first time any shipped species has
ever reached it — but the three dragon daughters are themselves authored as
atonal, so each nativizes the toned proto back down into its own toneless
inventory before a reader ever hears it: **Nánken** surfaces as **Dadgod**.
The tone is bound at the root and stripped before it reaches speech — reached
but not yet audible, the same shape Stage 2 first produced, recorded rather
than quietly patched over.

## Surface: how each family looks on the page

Stage 5 wired `Typology.orthography` down every rendering path — the
dictionary, drawn names, compounds, grammar — so that a bundle's spelling
convention, not only its phonotactics, distinguishes one family's page from
another's. Elf words render with diacritics (**Zroongtong** →
**Zrooṅtoṅ**), dwarf words with an apostrophe at compound and name seams,
where two consonants actually meet (**Mabashsajat** → **Mabash'sajat** — the
dwarf dictionary's own root-and-pattern skeletons never place two consonants
adjacent inside a single root, so the apostrophe is a name- and compound-
level mark, not a root-level one). Every other family — every bundle mapped
to `Digraph`, including the dragons — renders byte-identical to before, which
is what makes the elf and dwarf movement legible as the campaign's own rather
than incidental drift.

## Two caps, recorded rather than hidden

The campaign ships six of eighteen tongues with audible liquids where eight
were reachable. All four elf daughters carry them; the drow-adjacent and
gully lines do; desert-elf and wood-elf do not, and the reason is not a
per-daughter gap in attestation but a single function crossing a boundary it
should not. `nativize`, the routine that substitutes a proto segment onto a
daughter's own inventory, filters candidates to the same broad class — "both
consonants" — and then minimises a feature distance that weighs place,
manner, and voicing equally. For a proto /r/, a sibilant differs from it in
manner alone; an approximant differs in place *and* manner. The sibilant
wins the distance calculation and the liquid vanishes into a consonant with
no sonorancy at all, even on a daughter whose own onset law explicitly calls
for one. The mechanism the campaign built is sound; a different, older layer
erases its output on two of the eight tongues it should have reached. Fixing
`nativize` to weight sonorancy above place is a fidelity tradeoff outside
this campaign's own scope — it would move every nativized word in every
tongue, an epoch-scale churn of its own — and is recorded as a known,
measured cap rather than shipped as a silent ceiling.

The second cap is the dragons' tone, already described above: bound at the
proto, stripped at nativization, present in the ledger and absent from
speech until a future campaign makes the dragon `articulation_registry` rows
themselves tonal.

## What the number is, and what it is not

The campaign's last measurement, after absorbing two rounds of concurrent
work from main, put assignment accuracy at **0.7994579945799458** — the
highest reading the run ever produced, against the 0.7202 baseline it began
from. That is a real, non-trivial rise, and it is also not, by itself, a
claim that the tongues became *lovelier*. The classifier measures
distinguishability; the campaign's actual goal was aesthetic, and those are
different instruments. A tongue family can become more separable and no more
beautiful — the spec said so before Stage 1 ever ran, and nothing measured
since has removed the need to say it again at close. The sufficient half of
the campaign's claim — that these tongues now sound like something, not
merely that a model can tell them apart — remains taste-gated, unmeasured by
anything committed here, and is recorded as still owed rather than declared
solved by a rising number.

## What is not done

`nativize`'s sonorancy-blind distance function caps liquid coverage at 6 of
18 where 8 are reachable, and is Nathan's call to fix, not this campaign's.
The dragon daughters remain atonal, so the tone tier the campaign activated
lives only in the ledger's proto forms. The derivation trace reports "no
change" wherever nativization silently rewrites a segment — an
independent, pre-existing defect this campaign's own investigation found
twice, once on a liquid and once on a tone, and fixed neither, because
repairing what the trace *says* would not repair what nativization *does*.
And the campaign's own instrument, by its own design, was never built to
answer the question that motivated it in the first place: whether a reader,
handed a page of names, would call any of this beautiful.
