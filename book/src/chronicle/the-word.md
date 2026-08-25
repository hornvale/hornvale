# The Word

*A passage, not a campaign: one ruling, carried out.*

[The Lexicon of Place](the-lexicon-of-place.md) made a mesh vertex a `Vertex`
everywhere in the code, and then stopped short of the text a player reads. Its
argument was that "vertex" is engine vocabulary and an almanac is read by a
person, so the rendered prose kept saying "cell", with a comment at each site
explaining the exception.

Nathan reversed it in one sentence: *one word for each concept.*

The reversal is worth more than the exception was. **A second word for the same
concept is precisely the defect a vocabulary campaign exists to remove**, and
splitting the vocabulary by audience reintroduces that defect on purpose — with
a boundary that has to be re-adjudicated every time someone writes a new line of
output. "Is this string read by a player?" is not a question with a stable
answer; a REPL readout, a table header, a CLI usage line and an error message
all sit at different points on it, and the campaign had in fact answered it
inconsistently within a single almanac paragraph.

The genuine exception is not the audience but the **wire**. A string that is
serialized — a seed-derivation label, an epoch key, a predicate name or
description, a JSON key, a census column — stays frozen forever, because
changing it silently produces a different world from the same seed. That is a
save-format argument and it does not generalise to prose.

So the almanac, the gazetteer, the connections report, the CLI's usage text and
the REPL all say vertex now.

## What the passage found on its way through

**A plural nobody could see.** The sweep had renamed one noun and left its
pluralizer behind, so `"{size} vertex{}"` with a suffix rule that yields `"s"`
rendered **"5548 vertexs"**. Byte-identity could not catch it, and not by
accident: seed 42's sampled sites all sit *outside* the largest connected
region, so the branch carrying that string is never taken by any committed
artifact. A golden proves what it renders and says nothing about what it does
not.

**Five pluralizers, none of them shared.** `windows/almanac` alone carries
`connections::plural`, `history::pluralize`, `history::structure_plural`,
`lib::pluralize_people` and `qualify::plural` — three of them byte-identical
copies of naive `+s`, each documented as adequate "because the biosphere roster
has no irregular plurals". `vertex`/`vertices` is the irregular that breaks
that premise, and it arrived the same way "cell" did: by convergent emergence,
with every author reaching independently for the obvious local fix.

**And the right home for the fix already exists.** `CommonVocabulary` is a
*total* concept→word map, built from the registry at render time rather than
stored in a world, with the shape this needs already in it: a declared
exception wins, otherwise a mechanical rule derives. Extending it from identity
to inflection costs no committed byte. Meanwhile the generated tongues already
inflect for number — `domains/language`'s paradigms draw a per-species number
depth and let irregularity *emerge* from sound change rather than drawing it —
so Common is the one language in the world still hardcoding English.

That is a campaign, and it is specced separately. This passage only moved the
word.
