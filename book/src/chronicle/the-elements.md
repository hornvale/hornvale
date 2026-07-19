# The Elements

For all its modelled richness — temperature that swings by hour and season,
moisture that pools and parches, winds that bend around mountains — the world
could not be *felt*. Its inhabitants looked up and saw the sun, the moons,
eclipses, the wandering stars; and that was all they could observe, because the
sky was the only thing that spoke into the one channel through which anything is
observed at all. Religion grew celestial gods. Names glossed celestial
phenomena. The lived experience of a whole planet was astronomical, and the
ground beneath it — its heat and cold, its rain and snow — was mute. This
campaign gave the world its weather, and in doing so widened the channel through
which every future sense will reach it.

## One channel, and what was allowed into it

The Phenomena stream is Hornvale's universal read: a salience-ranked list of
observations that any consumer — religion, naming, and eventually perception and
cognition — draws from without ever learning which system produced them. It is
the seam between the modelled world and everything that interprets it. And it
carried nine kinds, eight of them from astronomy and one a tier-0 gloss of the
air. Not because climate had nothing to say, but because there was no way for it
to speak: the composition root wired a *fixed, literal* list of sources — the
sky and a constant stub — and the rich climate model, which held every
temperature and wind value in the world, was never on it. The narrowing was not
a decision anyone made; it was a coupling no one had removed.

So the first move was structural, and it rhymes with a shape the project already
trusts. Registering a concept is a thing every domain does through a roster —
the composition root folds `DOMAINS` and each contributes its vocabulary,
editing no one else. Contributing a *phenomenon source* is the same shape, and
had simply never been built that way. The `Domain` trait gained one method; the
six hand-wired fan-outs became one roster fold; and a small provisions channel
lets the composition root — the only layer where domains may legally meet —
build a cross-domain provider like the generated climate and hand it to its
domain to reclaim. The plumbing changed and nothing else did: the same world,
byte for byte. Only now the ground had a voice.

## Weather as a standing condition, felt in proportion to its extremity

The world has no clock. It is a pure function of place and time — you *sample*
it, you do not *run* it — so its weather could never be an event that happens
(a storm that rolls in, a front that passes). But time is a real axis of that
function: [The Wandering Sun](./the-wandering-sun.md) gave temperature its
season and [The Turning](./the-turning.md) gave it its day, so a desert
genuinely bakes at noon and freezes at dawn as you vary the hour. Weather here
is therefore a *standing condition that varies continuously with place and
time*: a dry heat, a biting cold, falling rain, falling snow — emitted by the
generated climate as it reads the observer's cell and the world's day.

The delicate question was salience, because salience is what crosses religion's
threshold and mints a god, and a careless wave of weather-phenomena would have
grown a weather deity in every temperate valley and drowned the pantheons. The
answer is that salience tracks *deviation from the temperate*, not the raw
value: a phenomenon is loud in proportion to how far its cell departs from a
mild baseline. Two thresholds, deliberately decoupled, fall out of that. The
*emission* band is narrow, so a merely-warm land is still felt — its almanac
says "a warm clime," sub-floor, unremarked by any god. The *deity* floor is far
out, reached only by a genuinely brutal climate. The consequence is quietly
naturalistic: peoples settle where the land is kind, so their skies still hold
their gods; only a people driven into a harsh country — a true desert, an ice
waste — grows a god of the heat or the cold. Weather-worship is not sprinkled
across the world; it is earned by hardship, and stays rare.

## The loop closes from both sides

Because the correspondence audit had already been taught to look in both
directions, this campaign's success was legible the moment it landed. Rain and
snow had been *orphan phenomena* waiting for concepts and *unperceived concepts*
waiting for phenomena — the same gap seen from two sides. Emitting them, and
then flipping their percept edges to name the kinds they now produce, struck
both entries out at once: they left the orphan list and the unperceived list in
the same stroke. The ledger that measures the world's completeness ticked toward
whole, and it did so without moving a single byte of any world — the
correspondence between a modelled thing and its name is metadata, not state.

## The seam is a hot path

There is a lesson the campaign paid for and is worth keeping. Wiring the
generated climate onto the observation channel meant that *building* a source
now did real work — deriving temperature and moisture across the globe — and the
naming and religion stages of genesis observe the world many times over, once
per settlement and once per candidate deity. Done naïvely, each observation
rebuilt the entire climate, and world generation swelled from a few seconds to
nearly a minute. The fix was not cleverness but arithmetic: build the sources
once per generation and reuse them. Genesis returned to its baseline, and the
whole test suite ran ten times faster as a side effect. The observation seam is
travelled far more often than it looks; a provider placed on it must be cheap to
*hold*, not merely cheap to *ask*. A deeper version of that same waste — genesis
still re-derives the terrain several times across its stages — was found and
left standing, noted for its own day.
