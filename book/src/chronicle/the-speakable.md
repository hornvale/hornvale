# The Speakable

**July 2026 · outcome: merged — the phonotactic-repair collapse that
crushed a species' every glossed name onto one fallback syllable is fixed
at the honest cheap point: a language's own words are now admitted verbatim
as evidence of what it may say, so a name built from those words surfaces
them unchanged, and seed 42's bugbear shadow-god stops being "Bvaash" and
becomes literally *Daoqao*, the dictionary's word for shadow**

## What a reader saw

Open the almanac at seed 42 and read the bugbear pantheon, and every god
had the same name. Seven distinct deities — a moon, a second moon, the sun,
a wanderer, an eclipse — each mythologizing a different phenomenon, each
carrying a truthful and *distinct* gloss, and every one of them called
**Bvaash**. The goblin priesthood's gods were all **Neb**; the hobgoblin
legion's all **Fee**. Only the kobold warren came out healthy, its gods
severally named. The glosses never lied — the machinery underneath knew
exactly which god meant which phenomenon and could recount it on demand —
but the *sound* on the surface had collapsed a whole pantheon onto a single
syllable, and no amount of vocabulary growth could route around it: new
words descended to the same rejected shapes and minted homophones, not
distinctness.

## The mechanism

The collapse sat in a seam between two subsystems that
[The Tongues](./16-the-tongues.md) and [The Words](./27-the-words.md) had
built to be independent on purpose. A species' **synchronic phonotactics** —
the onset, nucleus, and coda templates a name is assembled from — are drawn
from the phonology stream. A species' **lexicon** descends by its own line
from proto-goblinoid through an ordered sound-change cascade, drawn from the
etymology stream. Nothing tied the templates a name must fit to the word
shapes the lexicon actually produced, and roughly half of all drawn
phonologies land in a regime where the two disagree: onsets are drawn one or
two consonants long, codas zero or one, and a draw that admits *only*
two-consonant onsets rejects every CV-shaped word an evolved lexicon holds.

When a glossed name compounded a language's own words and those words would
not parse against the drawn templates, the naming pipeline reached for the
adapter it had always had: a repair pass — an epenthesis-and-deletion
dynamic program with a small constant table and a final minimal-syllable
fallback — meant to bend a foreign shape onto a native template. But the
repair was deletion-heavy, and a word that cannot be repaired by inserting
or deleting a segment gets deleted down to nothing; every native word in
turn hit the *same* constant fallback, and the whole pantheon landed on the
identical minimal syllable. The repair was doing exactly what it was written
to do. It was simply pointed at material it should never have been asked to
touch — a language's own words, treated as if they were loans from a
stranger.

## The fix

The idea that shipped inverts the relationship the collapse assumed. Going
in, the two obvious repairs both kept words subordinate to templates: draw
the templates more permissively so they happen to admit native shapes, or
teach repair to *substitute* toward the nearest legal segment instead of
deleting. The first re-rolls every phonology and still guarantees nothing —
hobgoblin drew a single-consonant onset and collapsed anyway. The second
keeps the words-bend-to-templates asymmetry and mangles shapes on the way
(`Daoqao` would come out `Naoqao`). Ideonomy's first pass negated the going-in
recommendation outright: *words bend to templates* became *templates bend to
words*. Phonotactics, in a real language, is a generalization **over** the
lexicon — the description a linguist writes *after* surveying what the
words do, common law rather than imposed code — not a filter the words must
pass to earn their existence.

So the admitted parse set for a glossed name is now the drawn canon
templates **plus the lexicon's own modern root forms, admitted verbatim** —
an *attested tier* beside the canonical one. A name compounded from a
language's own words parses with zero edits, by construction, and surfaces
those words unchanged. Repair is thereby the identity function for native
compounds — not by a special case bolted on, but because a language's words
are, definitionally, well-formed in that language. The structural invariant
The Words froze is not dropped but *redefined*: "well-formed" now means
canon-or-attested. And repair keeps its whole apparatus — the constants,
the deletion/epenthesis DP, the fallback — intact and idle, waiting for the
one job it was always right for: genuinely foreign material, the exonyms and
loanwords a later campaign will render through a native phonology for the
first time.

## What it changed, and what it did not

At seed 42 the bugbear shadow-god is now **Daoqao** — the bugbear
dictionary's own word for shadow, surfaced whole — where before it was a
featureless *Bvaash*. Gods that gloss different phenomena now carry
different names. Kobold, the control that never entered the pathological
regime, is byte-identical to before. The change is draw-free: repair and
nativization are pure functions over already-drawn material, no stream's
consumption order moved, so the campaign **rebaselines rather than epochs**,
following the precedent decision 0041 (libm) set when it moved every
transcendental's last bit without retiring a stream.
The five language reference pages — the dictionary, the phonology, the proto
tongue, the concept registry, the stream manifest — regenerate byte-identical;
only names that had previously been *repaired* move.

A twenty-seed readout confirms the surface distinctness the collapse
destroyed is restored: across seeds 0–19, each species' count of distinct
deity names now tracks its count of distinct deity *glosses*, where the
pathological species had previously flattened to one. The readout also
shows the residual it does not fix — a warren with three shadow-glossing
gods still lands three deities on one name (kobold 1/3 at one seed) — but
that collision is honest and structural: it is the same name for the same
meaning, because a deity's name today may gloss over at most a couple of
hardcoded concepts, so two gods that mean the same thing *should* sound
alike. Widening a deity's descriptor breadth so same-meaning gods diverge is
a separate question the idea registry tracks, and the sequel campaign's
work — not repair damage this campaign left on the floor.

## What was learned

- **Independence between two subsystems is a design choice with a seam, and
  seams leak.** The phonology draw and the etymology cascade were each
  correct in isolation and correct by their own tests; the defect lived
  only where a name asked them to agree and nothing had ever made them.
  Drawing two things independently is not the same as drawing two things
  that compose.
- **A repair pass is only honest about the material it was scoped for.**
  The deletion-heavy DP and its constant fallback were the right tool for
  adapting a stranger's word and exactly the wrong tool for a language's
  own — and the failure was invisible until a name actually reached it,
  because every test that exercised repair had (reasonably) fed it foreign
  input. The fix was not to make repair gentler but to stop calling it on
  native material at all.
- **The linguistically honest model and the smallest-blast-radius model
  were the same model.** Admitting the lexicon as its own evidence is what
  a descriptive linguist means by phonotactics; it is also the change that
  leaves every previously-conformant name byte-identical and touches only
  the names that were already broken. When the honest abstraction and the
  cheap one coincide, that is usually the signal the abstraction is right.

## Artifacts

[The Language chapter](../domains/language.md) — the attested tier described
beside the canon templates it stands with, and why repair is the identity
for a native compound. [The Gods of Seed 42](../gallery/the-gods-seed-42.md)
— the exit demo whose bugbear pantheon was the collapse's most legible
victim, re-quoted against the regenerated world. The measurement battery
proves the property the fix exists to guarantee: a name built from a
language's own words surfaces those words verbatim, with zero repair edits,
on every seed.
