# Language

**Questions it answers:** Two peoples name the same handful of things — a
village, a god, a god's favorite title. Why don't the names sound anything
alike?

Every proper noun the world has minted so far comes from the same place: a
short pool of syllables, drawn and capitalized, no mouth behind it. Goblin
village names are two or three syllables off one pool (`Gruugish`,
`Grumoknar`); a deity's epithet is picked from one of two fixed English
lists, six words each, regardless of which species is doing the venerating —
[Religion](./religion.md)'s own **the Wheel-Turner** and **the Still Crown**
are both entries off that list, not sounds any particular people's throat
produced. Kobold gained a body in Campaign Y2-1, and eyes in Campaign 15,
but never a mouth: its names are drawn from a second syllable pool built by
copying the first and editing it by hand, which is exactly the kind of
divergence-by-authored-instinct the species and perception vectors were
built to replace with a formula. Psychology gave the world minds; perception
gave it eyes; this campaign, the third of Year 2's spine, gives it **mouths**
— a phonology substrate, `domains/language`, that lets a name's sound be a
consequence of who is speaking it rather than a pool an author happened to
seed.

**Phonemes are feature-bearing segments; spellings are views.** The
substrate's internal truth is never a string. A `Segment` is a bundle of
articulatory features — for a consonant, place, manner, and voicing; for a
vowel, height, backness, and rounding — the same move the species and
perception vectors already made, an authored point in a small feature space
rather than an opaque label. A handful of segments a people's anatomy
affords but standard IPA does not name cleanly are permitted as **authored
extensions**, each carrying a defined romanization and a best-effort
IPA-adjacent notation rather than being forced into the nearest existing
symbol. Nothing about a segment is ever stored as text: it renders two ways,
on demand, from the same feature bundle — `romanize` for a plain-script
surface form and `ipa` for a transcription — and a name built from a
sequence of segments inherits both renderings for free. The almanac reads
the romanization; the book's phonology page reads the IPA; both are views of
one underlying truth, never two facts that could drift apart, exactly as
[Perception](./perception.md)'s venue is a fact about a phenomenon rather
than a second phenomenon standing in for the first.

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
whatever its kind, carries both a romanization and an IPA transcription, the
same "spellings are views" split the phoneme model itself keeps.

**The content→render seam.** Today, religion builds an English sentence
inline and commits it whole as a tenet fact — [Religion](./religion.md)'s
own eternal and cyclic templates are exactly this: prose, assembled once,
frozen into the ledger as a string. This campaign stops that. Religion
commits **meaning** instead of prose: a deity's generated name and epithet,
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

**The bright line.** A generated name has shape and voice — a mouth-feel
recountable to a dimension, a sound built from a real inventory and real
phonotactics — and nothing more. It has no gloss, no etymology, no meaning
a player could look up. This campaign builds no lexicon and no syntax; it
authors no sound-change and ages no name into a fossil of an older one;
meaningful toponyms, language demographics, and everything that would let a
name mean something rather than merely sound like something stay explicitly
out of scope, work for whichever campaign takes up language change and
contact once the world's peoples start meeting each other.

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
