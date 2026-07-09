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
**Fnabnget** (`/fnabŋet/`), whose head god is **Nodvnotngak the Nebsxad**;
its kobold neighbor is **Rakrra** (`/rakrra/`), whose own head god is
**Rragratxok Ragxoq**. See [Religion](./religion.md) for how a belief now
carries that name as committed content rather than a frozen sentence.

**Phonemes are feature-bearing segments; spellings are views.** The
substrate's internal truth is never a string. A `Segment` is a bundle of
articulatory features — for a consonant, place, manner, and voicing; for a
vowel, height, backness, and rounding — the same move the species and
perception vectors already made, an authored point in a small feature space
rather than an opaque label. A handful of segments a people's anatomy
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
discipline both earlier vectors already keep. Six dimensions, no more: each
one a named capacity the phonology engine intersects with whatever it draws,
so that any difference between two peoples' sounds recounts to a dimension
by name rather than to an author's ear. Widening this vector — a seventh
dimension, a per-individual variation within a species — is real design work
belonging to a campaign willing to weigh it, the same posture the species
and perception vectors were built under and the same warning the frontier
map keeps repeating: nothing here grows without someone choosing to grow it.

**The model card.** Every dimension below is **authored**, for both
peoples — nothing in this table is drawn or fit, the same posture the
species and perception model cards keep. Kobold's column continues the
project's authoring method: a translation, by hand, of a reptilian,
draconic-kin people's anatomy into six numbers a formula can read.

| Dimension | Type | Goblin (baseline) | Kobold | Rationale (kobold) |
|---|---|---|---|---|
| Labiality | scalar `[0,1]` | 0.5 | 0.1 | reptilian/draconic mouth — few bilabials/labiodentals |
| Vowel-space breadth | scalar `[0,1]` | 0.5 | 0.3 | a tighter vowel set |
| Voicing contrast | scalar `[0,1]` | 0.5 | 0.6 | — |
| Sibilance | scalar `[0,1]` | 0.5 | 0.9 | the draconic hiss — sibilant-rich |
| Voice loudness | scalar `[0,1]` | 0.5 | 0.2 | small, frail, and stealthy — a dampable phonology |
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

**The bright line.** A generated name has shape and voice — a mouth-feel
recountable to a dimension, a sound built from a real inventory and real
phonotactics — and nothing more. It has no gloss, no etymology, no meaning
a player could look up. This campaign builds no lexicon and no syntax; it
authors no sound-change and ages no name into a fossil of an older one;
meaningful toponyms, language demographics, and everything that would let a
name mean something rather than merely sound like something stay explicitly
out of scope, work for whichever campaign takes up language change and
contact once the world's peoples start meeting each other.

Reference: [Phonology](../reference/phonology.md), the drift-checked dump
of each species' inventory, phonotactics, and sample names, regenerated
from seed 42 by `hornvale phonology`. Laboratory: [Study 008, the Census of
Tongues](../laboratory/study-008.md), the phonotactic-validity,
epithet-honorific, and collision-rate calibrations at 10k scale. Chronicle:
[Campaign 16, The Tongues](../chronicle/16-the-tongues.md); [Campaign 17,
Audible Phonology](../chronicle/17-audible-phonology.md).

**The tongues ahead:** a lexicon and a syntax, and the meaning a name could
carry once those exist; sound-change, etymology, and names as fossils of an
older tongue, which want deep time under them before they mean anything;
meaningful toponyms, built once terrain features are worth naming;
pidgin, creole, and language-shift dynamics, which need two peoples who
have actually met; the oral-formulaic grammar that reoccupies the render
seam this campaign ships; a paralinguistic layer for mating and alarm
calls, a different kind of sound than speech entirely; music and the other
media a fuller expressive engine would eventually carry; per-individual
idiolect, which the closed-vector posture excludes exactly as it excludes
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
The cascade's target is fixed and deliberate: it lands only on the
phonology this world already shipped a mouth for, never widening or
retuning the inventory a name already draws from. A rule that would
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
has stood within sight of one — earns a **compound** instead, a
transparent joining of roots the species already holds ("big-water" for a
sea it knows of without having been steeped in). A concept with **no path**
at all to that species' experience earns neither: a **gap**, and the gap
always carries a reason, because a lexicon's silence is exactly as
recountable as its speech. That reason is one of two kinds, and never a
third. An **experiential** gap recounts to a fact the ledger holds and a
lexicon consulted: no kobold settlement touches coast, so kobold has no
word — not even a compound — for the open sea. A **perceptual** gap
recounts instead to a vector dimension, and it overrides exposure
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
language and binding on every compound it builds — puts them. A name's
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
