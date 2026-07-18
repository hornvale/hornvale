# Language

**Questions it answers:** Two peoples name the same handful of things — a
village, a god, a god's favorite title. Why don't the names sound anything
alike?

Before this campaign, every proper noun the world minted came from the same
place: a short pool of syllables, drawn and capitalized, no mouth behind it.
Goblin village names were two or three syllables off one pool (`Gruugish`,
`Grumoknar`); a deity's epithet was picked from one of two fixed English
lists, six words each, regardless of which species was doing the
venerating — [Religion](./religion.md)'s own the Wheel-Turner and the Still
Crown were both entries off that list, not sounds any particular people's
throat produced. Kobold gained a body in Campaign Y2-1, and eyes in Campaign
15, but never a mouth: its names were drawn from a second syllable pool
built by copying the first and editing it by hand, exactly the kind of
divergence-by-authored-instinct the species and perception vectors were
built to replace with a formula. Psychology gave the world minds; perception
gave it eyes; this campaign, the third of Year 2's spine, gives it **mouths**
— a phonology substrate, `domains/language`, that lets a name's sound be a
consequence of who is speaking it rather than a pool an author happened to
seed. Every proper noun the world mints now — a settlement, a deity, a
deity's epithet — is a real generated sound: seed 42's goblin flagship is
**Ngebvngadxnotnoaboo**, whose highest god is **Noaboo the Xngoknoaboo**;
its kobold neighbor is **Roqrraxaxoqrrak**, whose own highest god is
**Roqrraxogrra Xogrra**. (These are the names the world renders today;
[The Branches](../chronicle/the-branches.md) re-baselined every one of them
when goblin joined a language family, and The Speakable moved them again
where a name had been quietly crushed by phonotactic repair — see "The
first family" and "Phonotactics as a description, not a filter," below, for
why a proper name is not held byte-stable across that kind of change.) See
[Religion](./religion.md) for how a belief now carries that name as
committed content rather than a frozen sentence.

**Phonemes are feature-bearing segments; spellings are views.** The
substrate's internal truth is never a string. A `Segment` is a bundle of
articulatory features — for a consonant, place, manner, and voicing; for a
vowel, height, backness, rounding, and, since the phonology epoch ([The Rising
Tone](../chronicle/the-rising-tone.md)), a suprasegmental **tone** — the same
move the species and perception vectors already made, an authored point in a
small feature space rather than an opaque label. Tone is orthogonal to the
segmental features: it lives on the syllable nucleus, so most peoples carry a
single neutral tone with no contrast and render exactly as they did before the
tier existed, while a tone-capable people draws high and low as a second,
pitched channel over the same consonants and vowels. A handful of segments a people's anatomy
affords but standard IPA does not name cleanly are permitted as **authored
extensions**, each carrying a defined romanization and a best-effort
IPA-adjacent notation rather than being forced into the nearest existing
symbol. Nothing about a segment is ever stored as text: it renders three ways,
on demand, from the same feature bundle — `romanize` for a plain-script
surface form, `ipa` for a transcription, and `espeak` for the espeak-ng
phoneme mnemonic an offline synthesizer voices (added in Campaign 17,
[Audible Phonology](../chronicle/17-audible-phonology.md)) — and a name
built from a sequence of segments inherits all three renderings for free.
The almanac reads the romanization; the book's phonology page reads the IPA,
and its audio column plays clips authored from the espeak formulation; all
three are views of one underlying truth, never separate facts that could
drift apart, exactly as [Perception](./perception.md)'s venue is a fact
about a phenomenon rather than a second phenomenon standing in for the
first.

**The articulation vector, and why it stays closed.** A species' phonology
is built from an **articulation vector** — the species crate's third closed
vector, after the six-dimension psychology vector and the three-dimension
perception vector, authored per species with goblin sitting at every
scalar's 0.5 and every enumeration's goblin variant, the identity-at-baseline
discipline both earlier vectors already keep. Seven dimensions: each one a
named capacity the phonology engine intersects with whatever it draws, so that
any difference between two peoples' sounds recounts to a dimension by name
rather than to an author's ear. The seventh, **tonality**, was the phonology
epoch's own deliberate widening — the warning the earlier draft of this
paragraph kept ("widening this vector is real design work belonging to a
campaign willing to weigh it") discharged by a campaign that weighed it: tone is
a capacity a body plan affords, so it earns a dimension rather than a hidden
constant. The shipped humanoids sit at zero on it (atonal); the value earns its
keep when the bestiary grows a serpent or a bird. Widening the vector further
stays what it was — real design work belonging to a campaign willing to weigh
it, nothing here growing without someone choosing to grow it.

**The model card.** Every dimension below is **authored**, for both
peoples — nothing in this table is drawn or fit, the same posture the
species and perception model cards keep. Kobold's column continues the
project's authoring method: a translation, by hand, of a reptilian,
draconic-kin people's anatomy into seven numbers a formula can read.

| Dimension | Type | Goblin (baseline) | Kobold | Rationale (kobold) |
|---|---|---|---|---|
| Labiality | scalar `[0,1]` | 0.5 | 0.1 | reptilian/draconic mouth — few bilabials/labiodentals |
| Vowel-space breadth | scalar `[0,1]` | 0.5 | 0.3 | a tighter vowel set |
| Voicing contrast | scalar `[0,1]` | 0.5 | 0.6 | — |
| Sibilance | scalar `[0,1]` | 0.5 | 0.9 | the draconic hiss — sibilant-rich |
| Voice loudness | scalar `[0,1]` | 0.5 | 0.2 | small, frail, and stealthy — a dampable phonology |
| Tonality | scalar `[0,1]` | 0.0 | 0.0 | atonal — the shipped humanoids carry no pitch contrast; tone is for the future bestiary |
| Exotic manner (none, trill, click, or ejective) | enum | None | Trill | a signature resonant manner the anatomy affords |

Voice loudness carries a second life beyond its row. The species crate does
not yet hold a body — no frailty, no size, no build — so 0.2 is authored
today as a plain guess at what a small, stealthy anatomy implies, and the
model card banks it now to be *derived*, later, from a body/frailty vector
once one exists: the same treatment nocturnality received before the
perception vector existed to spend it, an authored placeholder standing in
for a formula that has not been written yet, its value unchanged on the day
the formula arrives because 0.2 is already what that formula will compute.
Voice loudness also reaches sideways into the row below it, in the vector's
one load-bearing interaction: exotic manner names what a species'
anatomy *can* produce, but the phonology engine's draw down-weights any
exotic manner by how loud the species is willing to be, so a kobold is
anatomically capable of a trill and only rarely lets one surface in a
generated name: kobolds can trill, but their names hiss. Click
and ejective sit in the same closed enumeration as trill, claimed by
neither people this campaign — the same treatment the perception vector
gave a crepuscular activity cycle before any species needed it: idle
variants of a vocabulary already closed, ready the day a species anatomy
calls for one, rather than added when that day arrives.

**The envelope is authored; the phonology is drawn.** The articulation
vector only says what a species' anatomy *affords* — it is an envelope, not
an inventory. The actual sounds a people's names use are **drawn**, per
species-culture, from labeled streams under that envelope's constraint: a
**phoneme inventory**, a subset of the envelope-permitted segments biased by
voice loudness exactly as the exotic-manner draw is, and **syllable
phonotactics** — onset, nucleus, and coda templates with a drawn complexity
— built once and reused for every name that species-culture ever generates.
Same seed, same labels, same language, every time: the phonology engine is
pure and deterministic end to end, drawing nothing that the envelope did not
first permit.

**Tone: a second channel, and how a lost sound becomes pitch.** Tonality maps
to a drawn **tone inventory** — one tone for an atonal people (the neutral
default, no contrast), a contrastive high and low for a tone-capable one — over
its own labeled stream, isolated so that adding it leaves an atonal people's
draw byte-for-byte unchanged. Tone matters twice over. It is a dimension of
*capacity*: the number of distinguishable syllables a language can form is its
onsets times its nuclei times its codas times its tones, and the engine
guarantees a floor on that product reachable through pitch — so a few-place
people meets the bar by drawing tones rather than by growing consonants it has
no anatomy for, keeping its character. And it is the endpoint of a sound change
that is diachronically real: **tonogenesis**. When a merging rule drops a
consonant — a two-consonant onset simplifying, a final coda falling — the
voicing it carried is not simply lost; it is reborn as a tone on the neighbouring
vowel, a dropped voiced consonant lowering the pitch and a voiceless one raising
it, exactly as tone arose in Chinese and Vietnamese. This is a *regular*
conditioned change, applied wherever its environment occurs rather than hunted
word by word, so two roots a merger would have collapsed stay distinct precisely
when they differed in the sound the merger destroyed — the segmental contrast
transposed into pitch rather than erased. Cognate descent is untouched, because
the rule reads the derivation's own history and never the shared proto. The
shipped peoples are atonal, so tonogenesis is inert for them and their words
change only through the epoch's reseed; the mechanism waits, complete, for the
first tone-capable people the world admits.

**Naming grammars, and the status-basis keying.** Three kinds of name this
substrate generates — settlement names, deity names, deity epithets, place
names excluded because no terrain feature carries a meaningful, generated
name yet. Every name starts from a **stem**, a sequence of syllables built
from the drawn phonotactic templates over the drawn inventory, its length
drawn per name-kind so a settlement's name and a god's name are not built to
the same scale. A fixed set of morphological operations then shapes the
stem into its kind: **compounding**, two stems joined; **reduplication**, a
syllable doubled; **honorific affixation**, a bound affix drawn from its own
class. Settlement names stay bare stems or lightly affixed ones; deity names
stay weighty bare stems; **epithets** are the morphologically rich kind, a
compound or reduplicated descriptive stem with an honorific optionally
stacked on top. Which epithets actually carry that honorific is not left to
chance: epithet morphology keys to the psychology vector's **status basis**,
the same dimension culture already reads to decide whether a settlement's
ladder tops out at a chief or at elders. A `Rank`-basis society — goblin —
affixes dominance honorifics onto its gods' titles; a `Knowledge`- or
`Generosity`-basis society — kobold — builds descriptive compounds instead,
with no dominance marking anywhere in the title. A god's epithet-shape
recounts to how its people organize authority, identity at the goblin
baseline exactly as culture's caste ladder already is. Every generated name,
whatever its kind, carries a romanization, an IPA transcription, and an
espeak formulation, the same "spellings are views" split the phoneme model
itself keeps.

**The content→render seam.** Before this campaign, religion built an
English sentence inline and committed it whole as a tenet fact —
[Religion](./religion.md)'s own eternal and cyclic templates were exactly
this: prose, assembled once, frozen into the ledger as a string. This
campaign stops that. Religion commits **meaning** instead of prose: a
deity's generated name and epithet,
the phenomenon it mythologizes (already committed), its periodicity
(eternal, or cyclic with a period), its sentiment (watched, mourned and
feasted, or feared), and its rank. A renderer living in `domains/language`
turns that structured content into voiced text through one function,
`render_line`, taking a content struct and a voice-parameter struct that
language defines and owns — language never imports religion, the same
constitutional posture that keeps perception's phenomena blind to their own
producers. The composition root maps a belief's committed facts into that
content struct, derives a species' voice parameters from its psychology
vector, and calls the renderer; the almanac and the REPL render every tenet
this way, at display time, from facts rather than from a frozen sentence.
**This signature is the campaign's permanent contribution, not a stopgap.**
Turning finished English back into finished English by post-processing was
considered and refused — a string has nowhere to grow, but an interface
does. v1 fills `render_line` with a template assembler working under three
voice knobs derived from psychology, identity at the goblin baseline:
**formality**, choosing an archaic or a plain connective; **repetition**,
echoing a refrain; and **epithet density**, how many honorifics stack in a
line. A `Rank`-basis people renders formal and honorific-dense — goblin,
again the identity case; a `Communal`- or `Knowledge`-basis people renders
repetitive and descriptive — kobold — so a myth's *telling* recounts to the
same vector dimension its gods' *names* already do. A future generative
oral-formulaic grammar reoccupies this exact interface without touching
religion, the composition root's wiring, or the almanac that reads the
result — the only temporary part of the seam is the template assembler
sitting behind it. Persist meaning, render surface: the same principle a
romanization states about one phoneme sequence, restated one level up about
an entire told belief.

The seam has since grown a second, more general storey: the **clause
layer** (`clause.rs`), where a language-neutral `ClauseSpec` — frame,
subject, complement, number, definiteness, a modifier tail — realizes into
a Common sentence through a **construction inventory**: the grammar as
data, an ordered list of surface parts one interpreter walks forward. The
Self-Writing Book program builds on this layer ("Vebe is a planet with two
moons…"), and since The Echo the same inventory runs **backward**: a
parser binds the parts against a closed complement lexicon and
reconstructs the `ClauseSpec` a sentence came from, under the standing
round-trip law `parse(realize(spec)) == spec`. One committed grammar, two
directions — production and comprehension cannot drift apart, because
they are the same data read opposite ways.

**The full model card.** The articulation-vector table above types the
species' *envelope* — six authored capacities per people. Everything this
domain builds *from* that envelope sorts into the same four kinds
[Religion](./religion.md)'s own model card already keeps:

- **Drawn** — labeled streams, `domains/language`'s `stream_labels()`: the
  per-species **phoneme inventory**
  (`language/<species>/phonology/inventory`), a subset of the
  envelope-permitted segments biased by voice loudness and sibilance;
  **syllable phonotactics** (`language/<species>/phonology/phonotactics`),
  onset/nucleus/coda templates with a drawn complexity; and every generated
  **stem** — settlement (`language/<species>/name/settlement`), deity
  (`language/<species>/name/deity`), epithet
  (`language/<species>/name/epithet`) — each a single deterministic draw,
  salted by the settlement cell's or belief's own id, never re-drawn.
- **Derived** — fixed functions over what is already drawn or authored, no
  stream of their own: the envelope carry, `envelope_of`
  (`windows/worldgen`), a direct 1:1 mapping of the species
  `ArticulationVector` onto language's own `Envelope` copy, the only place
  either vector is ever converted, language never importing species to do
  it; the voice knobs, `voice_params`, deriving formality, repetition, and
  epithet density from the psychology vector's status basis, sociality, and
  deliberation latency (identity 0.5 at the goblin baseline); naming
  morphology, `morph_options`, gating honorifics to a `Rank`-basis status;
  and, since Campaign 17, the **espeak formulation** (`espeak`,
  `espeak_word`) — a derived view computed over the same drawn segment
  sequence every name already carries: for all but five segments it
  delegates outright to the authored IPA table below, and for those five —
  where the synthesizer's own notation diverges from IPA — a hand-authored
  override string stands in (`ʃ`/`ʒ` → `S`/`Z`, `ŋ` → `N`, click → `tS`,
  ejective → `k`); either way the view is computed fresh from a segment
  already drawn or authored, never itself drawing anything new.
- **Authored:** the segment feature space itself and its curated
  extensions (a segment a species' anatomy affords but standard IPA does
  not name cleanly, each given a defined romanization and a best-effort
  IPA-adjacent notation by hand); the **romanization** and **IPA**
  per-segment tables `romanize`/`ipa` read from, one hand-chosen symbol per
  feature bundle; and the committed **audio clips** — an authored, offline
  artifact, generated by the `hornvale voice` subcommand (espeak-ng and
  ffmpeg, two pinned settings), never run by the sim core or CI, and named
  by a CRC-32 checksum of the espeak formulation so a phonology change is
  caught through the filename it now expects rather than a waveform
  comparison — "models author, dice roll" (decision 0009) applied to sound
  instead of to a name bank.
- **Approximated (declared):** two of espeak's five overrides are
  documented compromises rather than faithful sounds — the click renders as
  `tS` because espeak-ng's English voice silently drops a bare `!`, and the
  ejective renders as plain `k` because `'` is the synthesizer's stress
  marker and cannot appear inside a phoneme — the same kind of declared
  approximation the romanization already makes spelling a click `ts` and an
  ejective `kx`: a segment outside the target notation's coverage given a
  defined nearest neighbor rather than a symbol that lies about it. Voice
  loudness carries a declared approximation of its own, upstream of
  phonology entirely: its authored 0.2 for kobold is a plain guess standing
  in for a formula not yet written, banked to become *derived* once a
  body/frailty vector exists to read (see the vector table's rationale,
  above).

**The bright line (as Campaign 16, The Tongues, drew it — since stepped
over; see "Meaning arrives," below).** A generated name had shape and
voice — a mouth-feel recountable to a dimension, a sound built from a real
inventory and real phonotactics — and nothing more. It had no gloss, no
etymology, no meaning a player could look up. The Tongues built no lexicon
and no syntax; it authored no sound-change and aged no name into a fossil
of an older one; meaningful toponyms, language demographics, and
everything that would let a name mean something rather than merely sound
like something stayed explicitly out of scope, work for whichever campaign
took up language change and contact once the world's peoples started
meeting each other. Campaign 27, The Words, is that campaign, for the
lexicon and sound-change half of the line; contact, toponyms, and syntax
still wait on the other side of it — see "The tongues still ahead," below.

Reference: [Phonology](../reference/phonology.md), the drift-checked dump
of each species' inventory, phonotactics, and sample names, regenerated
from seed 42 by `hornvale phonology`. Laboratory: [Study 008, the Census of
Tongues](../laboratory/study-008.md), the phonotactic-validity,
epithet-honorific, and collision-rate calibrations at 10k scale. Chronicle:
[Campaign 16, The Tongues](../chronicle/16-the-tongues.md); [Campaign 17,
Audible Phonology](../chronicle/17-audible-phonology.md).

**The tongues still ahead (updated by Campaign 27, The Words — a lexicon
and own-line sound-change shipped; see "Meaning arrives," below):** a
syntax that lets two words assemble into a sentence rather than sit
together as a single compound noun, and the case, tense, and agreement
machinery it would carry; pidgin, creole, and language-shift dynamics,
which need two peoples who have actually met — The Words draws its own
bright line in front of exactly this, deliberately (see its closing
paragraph, below); meaningful toponyms, built once terrain features are
worth naming; the oral-formulaic grammar that reoccupies the render seam
Campaign 16 shipped; a paralinguistic layer for mating and alarm calls, a
different kind of sound than speech entirely; music and the other media a
fuller expressive engine would eventually carry; per-individual idiolect,
which the closed-vector posture excludes exactly as it excludes
per-individual psychology and perception; and the articulation vector's own
voice-loudness dimension, banked to be derived from a body/frailty vector
this project has not yet authored.

**Meaning arrives.** The chapter above ends on a promise: a lexicon, and the
meaning a name could carry, once something exists to say what a word is
*about*. It exists now. A mouth already had shape and voice; this is the
turn where a sound starts meaning something — carefully, and only exactly
as much as it is allowed to.

**Concepts are promises.** The kernel's concept registry — until now the
home of every registered predicate and phenomenon kind, the vocabulary a
fact or an observation is allowed to use — grows a second table. A
`ConceptDef` sits beside a `PredicateDef`: a kebab-case id (`water`, `moon`,
`goblin-kind`), the domain that owns it, a one-line English gloss, and a
kind drawn from a small closed set — substance, living, celestial, terrain,
social, body, kin, quality. Registering a concept is not registering a word
for it; a species may hold the concept `blue` and still have no word that
names it, exactly as a settlement may exist without yet having a name. What
registration asserts is narrower and load-bearing anyway: that the sim
represents this thing, or has deliberately authored it, closely enough that
a lexicon, a fact, or a player's question could someday resolve against it.
A concept nobody registered is not merely nameless — it is not yet a
promise the sim has made. Each domain registers the concepts it already
owns, at construction, the same discipline it already keeps for predicates:
astronomy contributes the sun, the moon, the stars, the night; climate
contributes snow, rain, ice, and one concept per biome it already
generates; terrain contributes stone, mountain, the sea; settlement
contributes home and hearth; religion contributes god and spirit; species
contributes the peoples themselves, goblin-kind and kobold-kind — which
means an endonym and an exonym now fall out of the same mechanism that
gives a kobold a word for stone, rather than needing a mechanism of their
own. Language owns the rest: the shared core described below. The inventory
stays closed under the same ontology-trap posture every closed vector in
this book already keeps — widening it is a campaign's decision, never an
incidental edit made in passing while building something else.

**The Swadesh core as packs.** A single universal word-list — the kind
historical linguists call a Swadesh list, a fixed roster of concepts every
human language is assumed to lexicalize — would be a lie the moment two
species perceive different worlds, and this book has already spent a
chapter proving they do. A flat "starting vocabulary" is a claim that every
species carves the same distinctions out of experience; [Perception](./perception.md)
exists precisely because that claim is false between a goblin and a kobold.
The shared core is therefore not one list but a set of small **packs**,
each authored and closed — enough to seed a lexicon, not an aspiration to
completeness. A **universal stratum** holds what these two species
actually share by being embodied, terrestrial, mortal, and social: water,
stone, sun, night, fire, eat, sleep, die, the basic kin terms — small and
defensible rather than universal in name only. A **color pack** carries the Berlin &
Kay ladder — the anthropological finding that a language's color
vocabulary grows in a fixed implicational order, dark/light before red,
red before green-or-yellow, green-or-yellow before blue, and so on — and a
species descends that ladder only as far as its own eyes carved the
distinctions. Kobold's night vision sits far above goblin's baseline, so
its hue ladder halts one rung short of blue: it holds words for dark,
light, red, and green-or-yellow, and stops. What a species does not spend
descending the hue ladder it spends instead on a second, orthogonal
**luminance ladder** — more words for kinds of dark rather than more words
for kinds of color — and a night-tuned people extends exactly that ladder
further than a day-tuned one does. The two ladders trade off by
construction, not by author's whim: a kobold's gap at blue and its
abundance of words for gloom, shadow, and starlit dark are the same fact,
read twice. Recountable, the way every gap in this system must be: kobolds
have no word for blue, because kobold eyes are tuned for the dark. A
**body pack** ought to key to a body plan the way the color pack keys to a
perception vector, but no body vector yet exists for it to read — so the
pack ships now as a shared humanoid core (eye, mouth, hand, foot, blood,
bone), model-carded as an authored placeholder standing in for a formula
not yet written, the same posture voice-loudness took above: unchanged on
the day a body vector exists to re-key it. A **kin pack**, similarly
minimal today, is banked the same way against the social-structure axes a
kinship system would eventually want to read.

**Own-line descent.** Every word this campaign mints for a language starts
as a **proto-root**: a drawn form, one or two syllables built from that
language's own phonotactic templates — the identical stem machinery that
already builds a settlement's name or a god's — one proto-root per concept
that language actually lexicalizes. A proto-root is not itself heard by
anyone; it ages. Each language draws its own ordered **sound-change
cascade**, two to four rules from a small closed family — lenition, a
voiceless stop turning voiced; fortition, a fricative hardening to a stop;
a vowel shift, raised or lowered a step; cluster simplification, a
two-consonant onset losing its first member; final-segment loss, a
word-final consonant dropped — and applies that cascade to every proto-root
in turn to reach the modern form actually spoken today. Regularity is not
a hope here but an invariant a computer can check: this is the
**Neogrammarian hypothesis**, the nineteenth-century claim that sound laws
tolerate no exceptions, restated as a property the world enforces rather
than merely asserts — a rule fires on every proto-root whose environment
it matches, or on none, and replaying the cascade over a recorded
proto-form must reproduce the recorded modern form exactly, every time.
That very regularity is what makes descent lossy: a cascade that drops an
onset or a coda can collapse two distinct proto-roots onto one modern word,
so a family's proto-roots are not merely drawn distinct but assigned
**merger-awarely** — each core concept's shared ancestral form is chosen to
survive *every* daughter's cascade still distinct, so no two core concepts
are ever homophones in any daughter (core homophony is zero on every seed),
while the proto stays shared so cognate descent is untouched. Incidental
collisions among the periphery — a rarely-named biome, a deep colour — are
left as realistic texture. The cascade's target is fixed and deliberate: it
lands only on the phonology this world already shipped a mouth for, never
widening or retuning the inventory a name already draws from. A rule that would
produce a segment outside that inventory applies as the identity function
instead, changing nothing, rather than quietly inventing a new sound no
one's anatomy was ever authored to make. This is the honest shape of the
whole exercise: history is *new data behind the same present*, not a
rewrite of it. Every language gets its own proto-tongue and its own
cascade, run once, independently — own-line descent, no shared ancestor
between goblin and kobold, no tree drawn behind the two lines this campaign
grows in parallel.

**Exposure and the two gap provenances.** A species does not deserve a
word for everything its ancestors could theoretically have named; it
deserves a word for what its world actually put in front of it. Every
(language, concept) pair resolves to exactly one of three outcomes. A
concept the species' world **saturates** — its settled biomes and
whatever those biomes hold, its own sky, its own social kinds, the
universal stratum, whatever a pack's depth grants it — earns an evolved
**root**, a proto-form aged through the language's own cascade. A concept
merely **adjacent** — a biome one cell removed from anywhere the species
actually lives, the sea for a people that has never touched a coast but
has stood within sight of one — earns a **compound** where an authored
recipe exists to build one from roots the species already holds
("big-water" for a sea it knows of without having been steeped in); today
that recipe table holds exactly two entries, the sea and the mountain.
Adjacency without a recipe — most concepts, `coral-reef` among them — buys
no compound; it falls through to the same outcome as a concept with **no
path** at all to that species' experience: a **gap**, and the gap always
carries a reason, because a lexicon's silence is exactly as recountable
as its speech. That reason is one of two kinds, and never a third. An
**experiential** gap recounts to a fact the ledger holds and a lexicon
consulted — no kobold settlement touches coast, so kobold has no word for
the open sea — or, honestly, to the authored recipe table itself when an
adjacent concept simply has no entry there yet: `coral-reef` reads "gap
(experiential): no compound recipe for 'coral-reef'" in the generated
dictionary, missing recipe rather than missing exposure. A **perceptual**
gap recounts instead to a vector dimension, and it overrides exposure
entirely: pack depth is checked first, so a kobold steeped in a blindingly
blue sky every day of its life still has no word for blue, because kobold
eyes are tuned for the dark and the color ladder never reached that far
down regardless of how thoroughly the world exposed the color. Two
provenances, never a shrug: a lexicon is allowed to be silent, but never
silently.

**Glossed names.** A proper name has always been a real sound; it now also
carries a real meaning, and that meaning is never invented for the
occasion — it compounds the entity's own committed facts. A settlement's
name draws from its own cell's biome, its notable sky, and the phenomenon
its presiding belief reveres; a deity's name and its epithet draw from the
phenomenon it mythologizes and the sentiment religion already derived for
it. "Ice-Home" sits in tundra by construction: not because an author
decided a cold place should sound cold, but because a tundra cell's biome
concept was among the very facts the draw compounded over, in the order
that language's own headedness — head-first or head-last, drawn once per
language and binding on every compound it builds — puts them. A settlement
name adds one more element the way real toponymy does: alongside its site
words it carries a drawn stem unique to that settlement — two or three
syllables of that language's own phonotactics tacked onto the gloss,
"Ice-Homenaknot" rather than a one-syllable English suffix like "-wick" —
a proper-name element that names no concept and so never enters the gloss,
there because a census showed site words alone give a species only a
handful of names against a hundred settlements (Study 011; see
[Settlement](./settlement.md) for the exact collision figures the census
pinned before and after this stem existed). A name's
sound already rendered three views of one drawn segment sequence — a
romanization, an IPA transcription, and an espeak formulation; it now
renders a fourth, a gloss — the concepts it compounds, and the site facts
that earned each one — the same "spellings are views" discipline restated
once more, never a second fact standing beside the first that could
someday drift from it.

This campaign draws its own bright line, a step further out than the one
above but still short of a living tongue. No word crosses from one
people's mouth into another's: no borrowing, no loanword, no calque, and
therefore still no tree behind goblin and kobold — two lines of own-line
descent running the same machinery in parallel, never meeting. Grammar
grows by exactly one parameter, compound headedness, and stops there: no
case, no tense, no agreement, nothing that lets two words assemble into a
sentence rather than sit together as a single compound noun. And a word
still carries no sense beyond its gloss — no frames, no figurative
reach, no second meaning a listener could be misled by. A name finally has
a story behind its sound. It does not yet have a grammar to tell that
story in, or a second people's ear to carry it to.

**The first family.** The previous campaign drew its own bright line at the
close of the chapter above: "no tree behind goblin and kobold — two lines
of own-line descent running the same machinery in parallel, never meeting."
This campaign steps over that line, deliberately, in the smallest way that
produces a real family. Own-line descent left the world with no shared
ancestry to measure anything against — each language's proto-root was drawn
from that language's own phonology, so nothing about goblin's history and
nothing about kobold's history was ever the same fact told twice, and a
resemblance between two languages' words, had one ever appeared, would have
been coincidence rather than kinship. A family needs a single ancestor two
or more descendants actually share, and nothing before this campaign built
one. Proto-goblinoid is that ancestor, and it is not a new kind of object: a
**proto-language**, here, is simply a language with no speakers — an
authored ancestral articulation vector, its own point in the same
six-dimension space every people's envelope already occupies; a phonology
drawn under that vector by the same machinery that draws a daughter's; and
one proto-root per concept, drawn once from that phonology under
family-level labels and shared, unmodified, by every daughter. Proto-goblinoid's
vector is authored equal to no daughter's — not goblin's, not any of the
three — so that "all three diverge" is a literal fact about the family tree
rather than a description of two lineages and a third that never moved. The
alternative, authoring proto-goblinoid equal to goblin's own vector, was
considered and refused: it would make goblin the frozen ancestor, which no
living language actually is, and would collapse a future reconstruction
effort's target to a trivial copy of a language already on the record. The
proto is never committed to the ledger; a world is a seed plus a ledger, and
proto-goblinoid re-derives exactly as every daughter phonology already does.
It surfaces only as a reference page and as the shared head standing over
every goblinoid cognate set's derivation. The family it seeds is `{goblin,
hobgoblin, bugbear}` — three lineages descending one ancestor — with kobold
explicitly outside it (see "Kobold, the outgroup," below).

**Descent under anatomical constraint.** Own-line descent already gave
every language its own cascade — lenition, fortition, a vowel shift,
cluster simplification, final-segment loss, drawn once and applied
uniformly. This campaign changes nothing about that machinery; it changes
what the cascade is applied *to*, and adds one function after it runs. Each
daughter still draws its own cascade under its own species label
(`language/<species>/lexicon/cascade`, unchanged) and its own present
phonology, exactly as before, but now evolves the *shared* proto-root — not
one it drew for itself — through that cascade. Three cascades over one
proto-root necessarily produce three modern forms: the concept `water`'s
proto-root `*gʷat` descends to `wat-` in goblin, `vad-` in hobgoblin, and
`gwad-` in bugbear — a cognate set whose members are provably related,
because they share one recorded ancestor, and whose correspondences are
regular, because each cascade obeys the same Neogrammarian invariant every
cascade already obeyed. The articulation vector's role shifts with it:
before, an envelope only bounded what a species' phonology could draw into
its inventory; now it is also the **codomain** a lineage's cascade lands
on — the target anatomy erosion converges toward, not merely the space
erosion is drawn from. A cascade rule whose *output* would fall outside a
daughter's inventory still applies as identity, unchanged from before. But
descent adds a second failure mode a same-species draw never had: an
*inherited* proto segment that no rule in the cascade ever touches, and
that the daughter's own inventory never happened to include either. That
segment is **nativized** — merged, by a fixed, draw-free function, to the
nearest segment the daughter's inventory does hold, measured by
articulatory-feature distance (place, manner, and voicing for a consonant;
height, backness, and rounding for a vowel) — the way a real inventory
absorbs a sound its speakers no longer make rather than preserving a
phoneme nobody's mouth produces anymore. `evolve` therefore gains a
stronger postcondition than any earlier campaign needed: the modern form is
always a subset of the daughter's own inventory, `modern ⊆ inventory`, and
a daughter's descent genuinely *loses* ancestral contrasts rather than
merely reshaping them. Nativization is a no-op exactly where the proto
inventory already sits inside the daughter's — every self-proto, singleton
lineage, which is to say kobold and every world generated before this
campaign — so the postcondition holds trivially there and the singleton
mechanism is untouched.

**The loudness axis.** Voice loudness was never a decorative number. The
model card already made it load-bearing once, biasing the phonology
engine's inventory draw and down-weighting any exotic manner by how loud a
species is willing to be — a kobold can trill, the model card says, but its
names hiss. It would be tidy if this campaign gave the dimension a second,
symmetrical job — biasing *which* cascade rules a lineage draws, fortition
for the loud and lenition for the quiet — and an early pass through this
chapter said exactly that. It is not what the code does. `draw_cascade`
picks each lineage's two-to-four rules from one closed, uniformly-weighted
set — lenition, fortition, a vowel shift, cluster simplification,
final-segment loss — with no term for loudness or any other vector
anywhere in the draw; a hobgoblin cascade is exactly as likely to come up
lenition-heavy as a bugbear's is to come up fortition-heavy. Loudness's
second job runs through the inventory it already shapes, not through rule
selection. It bites twice, both times downstream of the same drawn
inventory: first as the cascade's existing codomain constraint — a rule's
proposed output only lands when that segment already sits in the daughter's
own inventory, so a lineage whose loudness-shaped inventory is sparse
rejects more of its cascade's proposed changes back to identity than a
lineage whose inventory is rich; second as nativization's target — whatever
inherited proto segment the cascade leaves untouched and the daughter's
inventory still does not hold gets merged, draw-free, to the nearest
segment that inventory does hold. A quiet lineage's descent is therefore
dominated less by its own cascade's rule-by-rule sound changes and more by
this closing nativization pass, because its inventory has less room to
receive what the cascade proposes. Hobgoblin sits loud, authored at
roughly 0.8, a martial, disciplined, commanding people built as a diurnal
legion society, and draws a comparatively rich inventory that admits more
of whatever cascade it happens to draw. Goblin holds the family's
baseline, its vector unchanged from every earlier chapter's 0.5 identity
point — the family's middle case, but no less a genuine descendant for it,
because its vocabulary now descends the same shared proto-root every other
daughter does rather than being drawn fresh from its own phonology (see
"The re-baseline," below). Bugbear sits quiet, authored at roughly 0.3, a
large, hairy, stealthy ambush predator, and draws a sparser inventory that
leaves more of proto-goblinoid's phonemic space for nativization to
collapse. Measured over the thousand-seed family battery, bugbear's
descent nativizes noticeably more inherited segments than hobgoblin's on
average — a real, measured gap, driven by inventory size feeding a fixed
codomain-and-merger mechanism, not by a cascade quietly favoring one
direction of sound change. The gap holds in aggregate over the sweep, not
on every single seed: with two independent draws (a cascade, an inventory)
feeding a chain of per-segment decisions, a quiet lineage can still land a
rich inventory on a given seed and out-diverge a loud one that draws
poorly, so the ordering is a population-level tendency, not a per-world
guarantee. Each daughter also carries its own authored psychology and
perception vectors, taken from the same SRD-lore method kobold's did,
because a species is a whole people and needs all three; those two vectors
sit outside this mechanism entirely and never touch a lineage's descent,
exactly as culture and perception have never touched a language's
phonology envelope before now.

**Kobold, the outgroup.** A family needs a control, and kobold is it.
Kobold shares no ancestor with the goblinoids: it is a **family of one**,
and for a family of one, the proto *is* the daughter — its proto-roots draw
from its own phonology, under its own `language/kobold/...` labels, exactly
as every language's did before this campaign existed. "Family" is the
general mechanism here; a singleton is its degenerate case, not a special
exemption written around it. That equivalence is the campaign's
byte-stability keystone: kobold's `build_lexicon`, given identical
exposures, is byte-identical to what The Words already shipped — the
singleton mechanism does not change because two unrelated peoples joined
the world it inhabits. That claim is deliberately narrow. It is a
unit-level invariant, tested against fixed exposures, and it is not a claim
that kobold's *world-integrated* output stays fixed — that output does
shift, because the world itself now holds two more peoples to place, name,
and worship (see "The re-baseline," below). The two claims are not in
tension: byte-identity holds at the mechanism, determinism holds at the
world, and neither one is asked to cover what the other already does.
Kobold's role as negative control is exactly this: because it shares no
proto with any goblinoid, no goblinoid cognate set may ever trace to a
kobold form and no kobold word may ever trace to proto-goblinoid — a
property the Lab battery asserts directly, from ground truth, today. It is
the same property a later campaign's blind comparative method will have to
recover with no ledger to consult — the honest reason a clean outgroup
belongs in this campaign rather than being assumed.

**The re-baseline.** Say plainly what changes: goblin's proto-roots now
descend from proto-goblinoid instead of being drawn from goblin's own
phonology, so goblin's present-day vocabulary — and every proper name built
over it — changes. But the blast radius is wider than one language's word
list. Two more peoples join the world outright: hobgoblin and bugbear are
now placed, named, and worshipped, so settlement placement draws over a
larger roster, the concept layer gains endonym and exonym concepts for both
new peoples, and every language in the world — kobold included — grows
lexicon entries for them as coexistence brings species into each other's
exposure. Seed 42's world re-baselines **world-wide**, not just along the
goblinoid branch. The project's epoch discipline governs the change rather
than papering over it: the old per-species draw label,
`language/goblin/lexicon/root/<concept>`, is retired rather than silently
repurposed — it stays in `stream_labels()` with a `(retired at The
Branches)` doc, so an old save keeps loading and keeps rendering the
committed facts it already carries, while new generation draws under the
family-level label, `language/goblinoid/lexicon/root/<concept>`, instead.
Proper-noun byte-identity does not hold across this campaign, exactly as it
did not across The Words before it — a name that used to render
`Xnebsvobned` may render something else entirely once its roots descend
from a shared ancestor rather than a private draw. What must hold, and is
tested, is determinism: the same seed and the same roster reproduce the
identical world, byte for byte, every time. The committed seed-42 galleries
and the generated dictionary are regenerated and re-pinned **in this
campaign's own commits** — not deferred to some later cleanup — because a
drift check that tolerates its own campaign's drift is not a drift check at
all.

This campaign plants the tree and nothing more. Every concept a goblinoid
daughter lexicalizes keeps the root it inherited — no daughter innovates a
new root or loses one it was given, so the true rate at which languages
actually replace their vocabulary, the Dolgopolsky ranking a real
comparative linguist would recognize, is not yet a fact this world can
show; that differential retention is the next arc's work. No word crosses a
family boundary either: daughters do not borrow from one another and
nothing here builds a Sprachbund, so loanword resistance — the true
Leipzig-Jakarta ranking — waits on contact between peoples who have
actually met. And no one *infers* any of this: the proto, the cascades, and
the cognate sets are all asserted from ground truth the sim already holds,
never reconstructed blind from surface wordlists the way a real historical
linguist works from evidence alone — that blind comparative method, scored
against the ground truth this campaign commits, is the capstone the whole
descent arc has been building toward. One quieter non-goal rides along with
the other three: a linguistic family this deliberate implies a species
clade, but wiring the language tree to a deeper species phylogeny is
explicitly not this campaign's work — noted as a future consistency
constraint, not built as one. A family has an ancestor now, and three lines
that can prove they share it. It does not yet have a history of change
within any one line, a memory of contact between lines, or an ear capable
of reconstructing either without being told.

**Phonotactics as a description, not a filter.** The family tree above
exposed a fault that had been latent since the mouth was first drawn. A
name is assembled from a species' **synchronic phonotactics** — its onset,
nucleus, and coda templates — and those templates are drawn from the
phonology stream entirely independently of the lexicon, which descends by
its own line through the sound-change cascade. Nothing tied the shapes a
name must fit to the shapes the words actually take, and the two disagree
often: roughly half of all drawn phonologies land in a regime where a
template set rejects its own language's evolved words outright — a draw that
admits only two-consonant onsets cannot parse a single CV-shaped root. When
a glossed name compounded such a language's own words, the naming pipeline
reached for the **repair** pass it had always carried for foreign shapes —
an epenthesis-and-deletion dynamic program with a small constant table and a
minimal-syllable fallback — and that pass, being deletion-heavy, erased
every unparsable native word down to the same constant syllable. A whole
pantheon's gods, each glossing a distinct phenomenon and each carrying a
truthful and distinct meaning, collapsed onto one sound: seed 42's seven
bugbear deities were all *Bvaash*, goblin's all *Neb*, hobgoblin's all
*Fee*. The glosses never lied; the surface had simply lost the distinctions
the meanings still held.

The repair was correct. It was pointed at the wrong material. In a real
language, phonotactics is a **generalization over the lexicon** — the
description a linguist writes after surveying what the words do, not a law
the words must satisfy to exist. So the admitted parse set for a glossed
name is now the drawn canon templates **plus the lexicon's own modern root
forms, admitted verbatim** — an *attested tier* standing beside the
canonical one. A name built from a language's own words parses with zero
edits, by construction, and surfaces those words unchanged: seed 42's
bugbear shadow-god is now literally *Daoqao*, the bugbear dictionary's word
for shadow. Repair is thereby the **identity function for a native
compound** — not by a special case, but because a language's own words are,
definitionally, well-formed in that language. The structural invariant The
Words froze is not dropped but redefined: "well-formed" now reads
canon-*or*-attested. The canon tier remains the morphology's grammar, the
templates a *new* stem is drawn to fit; the attested tier is the language's
own standing evidence of what it may say. And repair keeps its whole
apparatus intact and idle, waiting for the one job it was always right for —
genuinely foreign material, the exonyms and loanwords a later campaign will
render through a native phonology for the first time, where a stranger's
word really must bend to a native shape. The change draws nothing: repair
and the attested admission are pure functions over already-drawn material,
so it rebaselines the names it moves rather than retiring a stream, and
every previously-conformant name is byte-identical.
