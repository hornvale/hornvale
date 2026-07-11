# The Rising Tone

**July 2026 · outcome: complete, merged — opens a suprasegmental tier on the
phoneme model and repairs merger-homophony the way real languages did it, by
turning a lost sound into pitch**

## What was attempted

The homophony campaign began as a bug report: distinct core concepts kept
colliding onto identical short word-forms — seed 42's goblins named *hand*,
*many*, and *night* all *Noa*. The first fix, an injective family-proto
assignment (epoch `root/v2`), gave every concept a distinct proto-root and
eliminated every *draw-side* collision. But the 1,000-seed instrument then
delivered an uncomfortable verdict: `homophony-merger-share` read **1.00** for
every daughter. The entire residual was now the sound-change cascade and
nativization collapsing two genuinely distinct proto-roots onto one modern
form — bugbear, with the smallest inventory, still carried core homophony in
four worlds out of five. The draw side was solved; the *diachronic* side was
the whole remaining problem.

Two facts turned a patch into an epoch. The first is that this residual and the
world's exotic-species ambitions are the same problem wearing two hats. The
phoneme model was **segmental only** — places and manners of articulation, a
handful of vowels, gated per species by an articulation envelope — with no
suprasegmental tier at all: no tone, no phonation, no length. A phoneme
inventory is the alphabet of distinguishable signals a channel can carry, and a
channel's capacity is dimensions times levels. Homophony is insufficient
capacity, and it bites hardest exactly where the dimensions are fewest. A future
serpentine people with few places to articulate would drown in collisions a
humanoid never notices.

The second fact is that the repair for both was sitting in the history of real
language. Tone in Chinese and Vietnamese *arose from lost segmental contrasts* —
a voicing distinction on a consonant collapsed, and its trace survived as pitch
on the neighbouring vowel. That is precisely the residual here: when the cascade
merges two roots that differed in the very segment the merger destroyed, the
lost contrast can be reborn as a tone. This campaign builds that — a new
phonology epoch, a superset of `root/v2`.

## The needle two alternatives could not thread

The Constitution forbids sporadic, lexical sound rules: *every rule applies
uniformly wherever its conditioning environment occurs*. That single sentence
rules out the two obvious repairs. Recording which words collided and pulling
them apart is a lexical patch, forbidden outright. Re-assigning a colliding
daughter's proto-root breaks cognate descent — the shared ancestor is what makes
the family tree true. Both alternatives buy homophony reduction by spending an
invariant the world is not willing to spend.

Tonogenesis threads the needle both missed. It is a **regular conditioned sound
change** — "a syllable that lost feature X acquires tone Y" — applied to every
syllable whose environment matches, whether or not that syllable would ever have
collided with anything. The rule reads the voicing of the consonant a *prior*
merging step dropped, from the derivation's own recorded history rather than any
new roll of the dice, and writes the attested tone onto the stranded nucleus:
a lost voiced consonant lowers the pitch, a voiceless one raises it. Because it
is a pure, replayable function of the proto, the cascade, and the phonology,
regularity holds by construction and the proto is never touched. Homophony
reduction becomes a *consequence*: two roots the merger would have collapsed
stay distinct exactly when they differed in the tonogenetic feature — and where
they lost the same feature, they still merge, because real tonogenesis does not
resolve every homophone either.

## What landed

**A tier, not a patch.** Tone is a property of the syllable nucleus — a fourth
field on the vowel, orthogonal to height, backness, and rounding, and the last
key in the deterministic sort so that an atonal world orders and renders exactly
as a world with no tone dimension at all. Most peoples stay atonal: they carry a
single neutral tone with no contrast, and their output changes only through the
epoch's reseed, never by gaining tone.

**Pitch authored from the body plan.** A seventh scalar, `tonality`, joins the
articulation vector — atonal zero for the shipped humanoids, higher for the
serpents and birds the bestiary will someday hold. It maps to a drawn tone
inventory: one tone (atonal), or a contrastive high and low over a neutral
default. This is "models author, dice roll" — the propensity is authored, the
specific inventory seeded.

**A floor on capacity, reached by pitch.** Rather than a minimum *segment count*
that would force a few-place species to grow un-serpentine consonants, the epoch
guarantees a minimum of *distinguishable syllables* — onsets times nuclei times
codas times tones — reachable through segments *or* pitch. A tone-capable species
short of the floor is widened by admitting tones, keeping its character; an
atonal species is never widened, its residual low capacity accepted as a
realistic tail rather than repaired into blandness.

**Homophony re-read as a comprehension cost.** The campaign's sharpest turn came
from a reframing: homophony is not merely a generation aesthetic. It taxes the
*listener*. Real speakers tolerate homophones because context disambiguates —
but a deterministic sim, where creatures must actually communicate, cannot lean
on context the way a human does. And yet, as the genetic code shows with its
sixty-one codons for twenty amino acids, a collision is harmless when the
colliding meanings never compete in the same breath. So the Lab gained a
**confusable-versus-free** split: core homophones that share a semantic domain
(two body-terms, say) are confusable — the listener cannot separate them by
topic — while cross-domain collisions are free. Measured over a thousand worlds,
the confusable count is roughly half the core count. Most of the residual tail
is free homophony a hearer resolves by subject, which turns "accept the atonal
tail" from a shrug into a measurement.

## What the reseed actually moved

Appending tonogenesis to the drawn cascade family reseeds every derivation in
every world — the deliberate epoch bump — so every generated artifact and every
calibration pin was regenerated and re-pinned to its honest new value in the
same commit. The homophone counts fell across the board (goblin's mean from 4.04
to 3.62, bugbear's from 12.19 to 11.23), but the fall is the *reseed*, not tone:
the shipped peoples are atonal, so tonogenesis is inert for them, and the new
cascades simply happen to merge fewer forms. The divergence-magnitude battery
came back byte-for-byte unchanged in aggregate — a quiet reassurance that the
family's measured structure is robust to which particular cascade each daughter
draws. Tone renders as a diacritic in the romanization and a Chao tone letter in
the IPA; espeak stays deliberately tone-blind, an honest audio limit rather than
a fabricated pitch the voice cannot really produce.

## What was learned

- **Measure before you size the fix.** The cheap four-seed diagnostic that
  opened the campaign said a third of the collisions were mergers; the
  thousand-seed instrument overturned it, and the merger side turned out to be
  the *whole* problem. Every design choice downstream rests on the census, not
  the sample.
- **The right floor is on the channel, not the alphabet.** A capacity floor
  reachable by pitch lets a few-place species meet the bar and stay itself; a
  segment-count floor would have made every exotic people a little more
  humanoid. The constraint is on how many things a language can *distinguish*,
  never on which sounds it must own.
- **Homophony is a two-sided coin.** Counting collisions on the generation side
  was only ever half the story. Splitting the count into confusable and free —
  the parsing-side cost — is what makes a residual defensible rather than merely
  tolerated, and it did so without a single new sporadic rule.

## Coda: core homophony reaches zero

Tone drove the tonal path's residual to nothing, but the shipped peoples are
atonal, so for them the confusable tail stayed real — bugbear carried a
same-domain core collision in more than half its worlds. The measurement the
tone epoch built is what made the next step legible, and the fix turned out to
be the same idea one level up. Tonogenesis chooses, for a *tonal* language, to
preserve a lost contrast rather than let two words merge; the **merger-aware
assignment** chooses, for *any* language, an ancestral form that will not merge
in the first place. Because every daughter's cascade is a pure, known function,
the family can pick each core concept's shared proto-root to survive all three
daughters' descent still distinct — regular (no sporadic rule, `evolve`
untouched), cognate-safe (the proto stays shared), and exact: core homophony is
now zero for every shipped people on every seed, measured 791 family-summed
collisions down to none. It cost the core vocabulary about three percent in
length and one durable concession — the assignment now depends on which
daughters a family has, so a future sibling could reshuffle a few ancestral
roots — the honest price of a guarantee that spans the whole family at once.
Free, cross-domain collisions are left where they fell, realistic texture rather
than a defect. The campaign that opened on *hand = many = night = Noa* closes
with no two core concepts sharing a word anywhere.

## Deferred, deliberately

Phonation — the dragon's growl, the whine — is IPA-native but espeak-weak, so it
waits for its own later epoch rather than shipping a feature the audio cannot
honestly render. Contour tones (rising, falling), vowel length, and
nasalization stay banked, as does `Tone::Mid`: voicing is a single bit, so this
epoch's rule writes only high and low, and the middle tone awaits a future
three-way conditioning source. The shipped goblinoid and kobold peoples stay
atonal by authoring; tone is for the bestiary to come. And no shipped world
exercises the tonal path — a tone-capable species lives only as a Lab control,
proving tone-count above one and the capacity floor met by pitch, the way the
null-control twin proves name independence without ever joining the roster.

## Artifacts

The tone tier and the tonogenesis rule live in [the Language
chapter](../domains/language.md) — the `Tone` enum, the vowel's fourth field,
and `RuleKind::Tonogenesis` reading the derivation's own history. The
seed-swept family battery (`studies/branches-family.study.json`) and its
calibration suite grow three metric families: `confusable-homophony-*` (the
parsing-cost measurement), `tone-count-*` (one for the shipped atonal peoples),
and `distinguishable-capacity-*` — the whole approach to measurement is set out
in [the Laboratory overview](../laboratory/overview.md). Two entries in the
decision log record the load-bearing choices — that tonogenesis is a regular
conditioned merger-repair rather than a lexical patch, and that the capacity
floor is reached by pitch rather than by adding un-characteristic segments to an
atonal species. Every seed-42 almanac, the dictionary, and both CI censuses were
re-baselined for the epoch; the shipped peoples' names are unchanged in
character, moved only by the reseed beneath them.
