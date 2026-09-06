# The Ken

*Ken: the range of what a creature perceives and could know. The name is
also the question this campaign asks of every sentence a possession
speaks: whose ken is it in?*

Nathan filed thirteen bug reports against the possession surface in one
sitting. Five of them looked, on first reading, like five unrelated
annoyances — a stray classification, an unhelpful number, a sentence that
never said anything, a mismatched noun. They turned out to share one
mechanism: **content from a frame the player character does not occupy,
spoken in the character's own voice.**

The project already draws the distinction this campaign enforces. `!whoami`,
`!why` and `!examine` are the author's frame — the instrument a session can
be asked to report from outside the fiction — and `look`, `examine` and the
turn header are the creature's. `domains/astronomy/src/facts.rs` had
already ruled that a star's spectral class is committed as a concept id and
"never as Morgan-Keenan prose — no creature in this world could have
invented that taxonomy." The possession surface was simply the one place
still breaking a rule the domain had already written down.

## Three leaks of knowledge, two of structure

The sun's spectral class reached creature prose directly: *"The sun, a
yellow dwarf (G), climbs the morning sky."* Nothing about a G-type star is
available to a body standing under it, and nothing needed inventing to fix
it — `daylight_words` already reads the class to choose its light words, it
just also printed the class it read. Dropping the appositive left the
sentence exactly as legible and considerably more honest: *"The sun climbs
the morning sky."*

The turn header carried a raw facet id and a fractional day —
`[room 3733133217, day 0.01172]` — reporting positions in engine units a
character has no way to hold in their head. Neither datum was actually
needed anywhere a player could see: `!whoami` already answers *"A
white-dragon of the wilds (agent 9630022852472602624), day 0, room
3733133217,"* which is the author's-frame instrument's own home for exactly
this information. The first attempt at a fix went too far the other way —
`[room]` and `[chamber]`, a header that satisfied "no id, no day" while
saying nothing at all, caught only because a coordinator ran `sort -u` over
a real multi-room transcript and got one line back for six positions. The
header now spends its one remaining slot on something that actually varies
with position: a walk-band facet's `descriptor_noun` (*"buttressed
canopy," "a stream gully"*) or a chamber's role (*"threshold," "hearthroom"*),
so `[room — buttressed canopy]` and `[chamber — hearthroom]` orient a
character the way the old header only pretended to.

The exits clause was structure, not knowledge, but it leaked the same way:
*"No direction here is closed; the nearest ground lies N, NE, E, SE, S, SW,
W, NW."* printed on every ordinary outdoor turn, spending twelve words
twice to say that nothing was unusual. Openness is the default in this
world; a wall is news. The clause now earns its line only when a bearing is
genuinely refused — which happens at exactly the 24 cube-corner facets
where three quadrants meet — and stays silent everywhere else. Reading the
two computations behind it (`exits` and `heading_rose`) all the way through
showed that the spec's own middle case — some ground missing, nothing yet
refused — cannot occur at all: the two partition the same eight bearings by
construction, so the branch is provably dead rather than merely unobserved,
and the campaign records that as a finding rather than leaving it untested.

## One report, two defects wearing each other's clothes

The fifth report — *"I see a white dragon… it says 'a black-dragon'"* —
looked like a single wrong-species bug. Reproducing it with a staged
tableau of three dragons found two independent defects instead. First,
`presence_line` built its display noun from a body's `species` field while
`examine` matched on `label` — so the game printed *"Here: a wild
black-dragon"* and then answered *"You see no a wild black-dragon here"* to
its own sentence. Second, once the presence line was fixed to display the
label, an ambiguous typed noun still resolved by picking an arbitrary
match: with three dragons present, `examine dragon` confidently answered as
whichever one sorted longest, with no signal to the player that a choice
had even been made.

Displaying the label surfaced a defect the codebase had already found and
deliberately left alone: a wild creature's label read `"a wild {species}"`,
which renders as *"The a wild carrion-crawler looks lost"* — a doubled
article, known and left in place specifically because fixing it moves
committed goldens. This campaign paid that cost rather than display a noun
it already knew to be malformed, dropping the article from the wild label
convention to match the settled rule the label's own doc comment already
stated. And because a singleton creature's label now stands bare beside a
count clause that still said *"16 wild xorn,"* the count clause lost its
"wild" too, in the same commit that introduced the inconsistency — a
self-inflicted defect fixed before it ever reached a review.

The ambiguous needle is now refused rather than guessed: possessing the
white dragon of a three-dragon staged tableau, `examine dragon` answers
*"'dragon' could mean more than one thing here: black-dragon and
red-dragon. Be more specific,"* leaving The Roll's own exact-match and
longest-extension rules untouched — refusal only replaces the case where
neither rule can produce a genuine winner.

## What this closes, and what it does not

Five of Nathan's thirteen reports are closed. The remaining eight — command
grammar and rendering, and the `Room`/`Chamber` vocabulary question — are
two more campaigns' work, already scoped and waiting. Nothing here touches
a save-format contract, a stream label, or a seed derivation: every change
is downstream of the ledger, reshaping what a character's own senses are
allowed to report rather than what the world computes.
