# The Residue

*An irregular verb is not an exception. It is what a regular sound change
looks like from the wrong side of a word's edge.*

Every language this project has generated so far inflects its words
uniformly: attach the marker, get the marked form, every time, for every
root. Real languages are not this tidy — *foot/feet*, *was/were*, the
English strong verbs — and this campaign gives Hornvale's tongues the
same texture, without ever authoring a table of exceptions.

The mechanism rides entirely on machinery that already existed. A tongue's
sound-change cascade turns a proto-root into its modern form by applying a
short, drawn sequence of rules — a stop voices between vowels, a
consonant cluster simplifies, a word-final consonant drops. That last
rule only looks at whatever segment currently sits at the end of the
sequence it's given; it has no idea a word is "a root" or "a root plus a
suffix." So a root evolved on its own, with its own final consonant
exposed to that rule, can lose a sound that the very same root keeps when
a vowel-starting suffix is joined onto it *before* the cascade ever runs —
because in the joined sequence, that consonant is no longer at the edge.
Same root, same suffix, same cascade, two different outcomes, decided
entirely by the order two already-existing operations run in. That
divergence is irregularity, arriving as a byproduct rather than a
decision.

Where a root's shape makes the two orders disagree, this campaign now
resolves the tie in favor of whichever form is more resistant to
leveling — and resistance is read off the root itself, never authored:
shorter proto-roots are ranked more resistant, the literal claim behind
Zipf's law that short words are frequent words and frequent words hold
onto their quirks. The shortest divergent roots in a category keep their
irregular shape; the rest regularize toward the form a listener would
expect from the pattern alone. Nothing about which specific roots survive
was chosen — it falls out of proto length, which falls out of the family's
own drawn phonology.

This is the first time in the project a linguistic mechanism shipped
without a single new draw. The two pieces that do draw — whether a
tongue marks Number or Tense with a bound affix at all, and what that
affix's proto-form is for the whole family — reuse existing,
already-proven machinery unchanged in shape, only pointed at two new
grammatical categories. Everything downstream of those two draws is a
pure function: run the numbers once, get the same irregularity forever.

What shipped is the mechanism and its proof, not yet its voice in the
Book. Four measured claims stand behind it: the computation is
deterministic; some roots in a category diverge and others don't, tracking
their own shape rather than a coin flip; leveling actually removes some of
that divergence rather than relabeling all of it; and — the one
falsifiable prediction the campaign staked itself on — the roots that
survive leveling are measurably shorter, on average, than the ones that
don't. All four were checked against a real, constructed scenario before
this campaign called itself done.

What's still reserved: no tongue's rendered sentence yet uses an
irregular plural or past tense — this campaign built the substrate a
future one will read from, the same way an earlier campaign's family-
cognate morphology waited a cycle before it spoke in the Book. Nor does
grammaticalization depth itself drift across generations; a tongue's
whether-it-marks-this-at-all is a single snapshot, not a centuries-long
slide from free particle to fused affix, though the machinery this
campaign built is exactly the kind that slide would run through, whenever
someone builds the genealogy to carry it.
