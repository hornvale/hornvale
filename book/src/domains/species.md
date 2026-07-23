# Species

**Questions it answers:** What makes one people different from another, and
what has to stay fixed for that difference to be legible rather than
arbitrary?

Year 1 built one people. Every settlement in Hornvale, tier 0 through tier 1,
was goblin; its social structure grew from land and pressure, but its
psychology was never named because there was only one psychology to name.
Year 2's first campaign, The Peoples, gives the world a second people —
kobolds, nocturnal deep-dwellers — and, underneath both, the substrate that
makes the difference real rather than cosmetic: a new kernel-only domain,
`domains/species`, holding nothing but authored definitions of what a people
is disposed to do. Placement and social structure now diverge between
goblin and kobold for reasons a player can be told, not for reasons baked
separately into two copies of the same code.

**Species are data; the social grammar is code.** The domain follows a
rhyme already ratified elsewhere in the project — studies are data, metrics
are code — restated here as: species are data, the social grammar is code.
A species is a point in a small, closed parameter space, never a table of
rules of its own. The alternative was tried on paper and refused: a
per-species `(role, condition)` table living in data reads, at first, like
configuration, and becomes, by the second or third species, a bespoke
interpreter for a language nobody designed on purpose — the ontology trap
with a welcome mat out front. Species as code — one Rust type per people —
was refused for the opposite reason: every new people would be a code
change, and the Laboratory could never sweep a space it isn't a point in.
Culture's rule table stays exactly what it was, one reviewed function, and
grows a parameter; adding a third people later edits no code that goblins or
kobolds depend on, only a row in a registry.

**The closed vector, baseline goblin.** What was authored as one closed
six-dimension psychology vector is, since *The Cloister* (Campaign 4 of the
Dragons program), two closed vectors cleaved along who carries them. The
**mind vector** — three scalars bounded in `[0, 1]` (threat response,
deliberation latency, time horizon) — is carried by every minded kind: the
four settling peoples and, since *The Eremite*, the three solitary dragons
too. The **society vector** — one scalar (in-group radius, `[0, 1]`) and two
enumerations (sociality mode: hierarchic or communal; status basis: rank,
knowledge, or generosity) — is carried only by a `Settled` kind; a
`Solitary` creature carries none. Goblin is the baseline for both: every
scalar sits at exactly 0.5, every enum at its goblin variant, and every
downstream formula that reads either vector is built so the baseline value
reproduces today's behavior — not tuned to match, but constructed to match,
by writing the formula as an expression like `0.5 + time_horizon` or
`1.5 − threat_response` that evaluates to today's plain constant precisely
when the vector reads "goblin." A consumer that needs a society reading for
a kind that carries none resolves `SocietyVector::baseline()`, defined to
equal goblin's authored society dims exactly, so a solitary creature's
absent society reads as "goblin-shaped," never as an error or a zero. This
is the mechanism, not merely a claim, behind the campaign's keystone test: a
world generated with the species pin restricted to goblin must be
byte-identical to a world generated before species existed at all, because
"goblin" and "no species substrate" are, by construction, the same input to
every formula in the cascade — and *The Cloister*'s own keystone test
extends the claim one level: cleaving the vector in two moves no byte of any
generated world or reference artifact, verified by full regeneration and
diff, not merely asserted.

**The authoring corpus.** Kobold's six numbers are not measured, drawn, or
fit — they are read. The project's method for authoring a new people is to
take the Dungeons & Dragons 5th Edition System Reference Document as a
corpus of decades of playtested lore and translate a race's flavor and
statistics into the six dimensions by hand: the same "models author, dice
roll" pattern already governing this project's other authoring work — a
person makes the judgment call once, in the open, and the simulation then
runs deterministically from what they wrote down. Every authored kobold
value carries a one-line derivation naming the piece of lore behind it, and
where the source material pulls in two directions the chapter follows one
coherent reading and says which. The corpus's value outlasts this campaign:
it is one consistent measurement frame that will eventually span peoples
and beasts alike — a future water buffalo or tyrannosaur sits in the same
kind of dimensions a kobold does — a vein the frontier map notes as still
to mine.

*Kobold parameters in this chapter are derived from the Dungeons & Dragons
5th Edition System Reference Document 5.1, available under the Creative
Commons Attribution 4.0 International License (CC-BY-4.0). No SRD text is
reproduced here — only parameter derivations, paraphrased to one line each.*

**The kobold mind card** (the three individual dims, carried by every minded
kind — including a solitary creature with no society of its own):

| Dimension | Type | Goblin (baseline) | Kobold | 5E derivation |
|---|---|---|---|---|
| Threat response (flee ↔ stand) | scalar `[0,1]` | 0.5 | 0.8 | cowardly in the open field, but entrenched at home behind the traps and tunnels the warren has prepared |
| Deliberation latency | scalar `[0,1]` | 0.5 | 0.7 | communal decisions arrive by slow consensus |
| Time horizon | scalar `[0,1]` | 0.5 | 0.8 | generational works — tunnel complexes, egg-tending — that pay off only across years |

**The kobold society card** (the three community dims, carried only by a
`Settled` kind; a solitary creature carries none, and a consumer that needs a
society reading for one resolves the goblin-equal baseline instead):

| Dimension | Type | Goblin (baseline) | Kobold | 5E derivation |
|---|---|---|---|---|
| In-group radius | scalar `[0,1]` | 0.5 | 0.2 | insular warrens; loyalty runs tight and does not extend past the pack |
| Sociality mode | enum | Hierarchic | Communal | pack tactics and communal egg-tending, not a chief's household |
| Status basis | enum | Rank | Knowledge | trap-cunning and craft esteemed over raw dominance |

Kobold's values are authored judgment, not a fixed translation — the
design's requirement is only that they land somewhere meaningfully unlike
the goblin baseline, and that 0.5 (or the goblin enum variant) always means
goblin. A vocabulary rides alongside the vector, one word per rung: kobold's
worker rung is always "digger" — fixed regardless of subsistence mode,
unlike goblin's, which still names the subsistence worker itself
(farmer/herder/fisher/forager) — and kobold's warrior, artisan, shaman, and
top rungs render as "warden," "shaper," "keeper," and "elders" where
goblin's render as "warrior," "artisan," "shaman," and "chief"; the
settlement itself is a "warren" rather than a "village." A structural
consequence rides along for free: kobold's knowledge-based status basis
(below) makes the rank-gated slave rung unreachable, so no kobold ladder
ever needs a word for it, and every kobold structure tops out at "elders,"
never "chief" (`domains/culture`'s structure function; see
[Culture](./culture.md)). These role words are common nouns, not proper
nouns, and stay English by convenience even after Campaign Y2-3 (The
Tongues) gave the world a generated tongue — that campaign gave *names* a
real sound (a settlement, a deity, a deity's epithet), deliberately drawing
the line at the lexicon a common noun like "digger" or "chief" would need;
see [Language](./language.md)'s bright line. This vocabulary keeps waiting,
now on a lexicon and a lens for common nouns, not merely a phonology.

**Idle by design; banked for later.** Not every dimension pulls its weight
the day it is authored. Deliberation latency is authored, differs
meaningfully between the two peoples, and this chapter's first campaign
declared it idle — consumed by no rule at all — banked for a future
salience or negotiation rule, once observers judge each other's
decisiveness directly; that rule still has not arrived. Campaign Y2-3 (The
Tongues) spent a first, narrower share of it without waiting for the
negotiation rule: `domains/language`'s `voice_params` folds deliberation
latency in, half and half with status basis, to derive the formality voice
knob (see [Language](./language.md)) — so the dimension is no longer idle
exactly as first banked, only not yet spent on the decisiveness-facing rule
it was banked for. A banked dimension can be spent gradually, on more than
one rule, rather than all at once; the full model card below tracks which
dimension is spent where. Kobold nocturnality was banked the same way
through this chapter's first campaign,
more literally: an activity-cycle dimension had no seat in this closed
vector, so nocturnality lived only as authored prose with nowhere to run.
Campaign 15 (The Eyes) spent it: `domains/species` now also holds a closed
three-dimension **perception vector** — activity cycle, night vision, sky
attention — a second component keyed to the same kind alongside the mind and
society vectors above, and kobold's authored nocturnality finally reaches a
formula, a species-specific characteristic hour and salience lens that
crowns the moons and the night sky over the sun. See
[Perception](./perception.md) for the vector, the lens derivation, and what
it let religion do with two peoples instead of one. Campaign Y2-3 (The
Tongues) added a third: a closed six-dimension **articulation vector** —
labiality, vowel-space breadth, voicing contrast, sibilance, voice
loudness, and an exotic manner a species' anatomy affords — again authored
at the goblin baseline and read from the same D&D 5E corpus for kobold. Since
*The Dissolution* the articulation vector is owned by `domains/language`
itself — its true home, the domain that reads it to build every name — rather
than by `domains/species`, keyed to the same kind. See
[Language](./language.md) for the vector itself and what it let every name
in the world become.

**In-group radius went idle, the reverse direction.** Where deliberation
latency and nocturnality were banked *before* a consumer existed, in-group
radius lost its only consumer: it used to scale the coastal term of the
settlement-placement suitability formula, and *The Gathering* retired that
formula outright in favor of a carrying-capacity field whose coastal bonus
is a fixed constant, not psychology-scaled. The dimension stays authored
and committed to the ledger — nothing about the vector's shape changed —
but nothing reads it today. It is captured here rather than silently
dropped, exactly the discipline this chapter already applies to dimensions
banked before their time; a future consumer (a coexistence-stack home-range
term is the natural candidate) can pick it back up without re-authoring it.

**The ontology-trap posture.** This closedness is a stance, not an
oversight. The frontier map warns, more than once, against traits that grow
without a designer choosing to grow them — an inheritance hierarchy here, an
extensible trait bag there — and this vector is where that warning is
finally enforced: six dimensions, no more, no per-individual variation, no
inheritance between species. A seventh dimension, or a psychology that
varies goblin-to-goblin rather than only goblin-to-kobold, is real work with
its own design principles; it does not arrive as a quiet addition to a
struct that happened to have room.

**The full species model card.** Since *The Dissolution* (the entity-component
program's third campaign) there is no `SpeciesDef` struct at all. A kind is a
**set of components**, each keyed by the kind's stable `KindId` label, each
authored and owned by the one domain that presents it, composed only at
worldgen. `domains/species` authors the universal **biosphere** component
(`BiosphereTraits` — mass, metabolic class, resource niche, condition niche,
potency — the row every kind carries and the packer and habitat model read)
and, since *The Cloister*, two psychology components where there used to be
one: the **mind** vector (3 — threat response, deliberation latency, time
horizon), carried by every minded kind, dragons included; and, only for a
people that settles and speaks, the **society** vector (3 — sociality,
status basis, in-group radius) alongside the **perception** (3) component.
`domains/language` authors the **articulation** (6) and the social
**lexicon** — the speech a kind that speaks carries. The biosphere-only
fauna of the menagerie (dragons, treant, xorn, …) simply have no rows in the
peopled registries, and none of this table; their dragon-ness is that absence,
expressed rather than declared. The one biosphere dimension the fauna carry
distinctly is **potency** — a creature's magical might — assayed, like `mass`,
from the D&D 5E corpus: it is the kind's adult Challenge Rating over thirty
(`CR/30`), nonzero only for the supernatural set (dragons, treant, xorn) and
zero for mundane beasts. It is the term that buys the mighty their rarity,
through the sovereignty floor the habitat model reads. A second universal
biosphere dimension is **social organization** (`SocialForm`: sessile, solitary,
gregarious, or settled) — orthogonal to the mind, perception, and speech
*capacities* a creature may or may not carry. Only the settling peoples build
settlements; and the capacities compose as a **nested lattice** rather than the
old all-or-none bundle — speech presupposes perception presupposes a mind, and a
settling people carries the full cluster, yet a solitary creature (a dragon) may
carry a mind without perception, speech, or a society of its own — a `Solitary`
kind carries no `SocietyVector` at all, and a consumer that needs a society
reading for one resolves `SocietyVector::baseline()`, the goblin-equal values.
What the old `Option<PeopledTraits>`
once guaranteed by the shape of a type — the peopled traits together or none —
worldgen now enforces as this load-time nested-capacity check across the
registries. Every dimension here is still **authored**. Nothing in this table is
drawn, fit, or measured; species is data written once by a person reading a
corpus, the same posture each of the four sub-vector sections above keeps on
its own. The "consumer" column names the actual formula that reads each
dimension today, not the formula a dimension was originally authored for —
the two differ for one row, called out below the table.

| Vector | Dimension | Type | Goblin | Kobold | Consumer |
|---|---|---|---|---|---|
| Mind | Threat response (flee ↔ stand) | authored, scalar `[0,1]` | 0.5 | 0.8 | culture's warrior-rung threshold; demography's hostility factor (carrying-capacity field) |
| Mind | Deliberation latency | authored, scalar `[0,1]` | 0.5 | 0.7 | language's formality voice knob (partial — see below) |
| Mind | Time horizon | authored, scalar `[0,1]` | 0.5 | 0.8 | culture's artisan-rung threshold; demography's freshwater factor (carrying-capacity field) |
| Society | In-group radius | authored, scalar `[0,1]` | 0.5 | 0.2 | idle since *The Gathering* — see below |
| Society | Sociality mode | authored, enum | Hierarchic | Communal | language's repetition voice knob |
| Society | Status basis | authored, enum | Rank | Knowledge | culture's slave-rung gate; language's formality/epithet-density knobs and honorific gate |
| Perception | Activity cycle | authored, enum | Diurnal | Nocturnal | perception's characteristic hour and lens activity factor (Crepuscular idle — see below) |
| Perception | Night vision | authored, scalar `[0,1]` | 0.5 | 0.9 | perception's night-sky lens weight |
| Perception | Sky attention | authored, scalar `[0,1]` | 0.5 | 0.8 | perception's day-sky/night-sky/ambient lens weights |
| Articulation | Labiality | authored, scalar `[0,1]` | 0.5 | 0.1 | language's labial-segment gate (envelope) |
| Articulation | Vowel-space breadth | authored, scalar `[0,1]` | 0.5 | 0.3 | language's permitted-vowel band (envelope) |
| Articulation | Voicing | authored, scalar `[0,1]` | 0.5 | 0.6 | language's voiced-segment gate (envelope) |
| Articulation | Sibilance | authored, scalar `[0,1]` | 0.5 | 0.9 | language's sibilant keep-probability bonus (drawn inventory) |
| Articulation | Voice loudness | authored, scalar `[0,1]` | 0.5 | 0.2 | language's high-sonority keep-probability penalty and exotic-manner down-weighting (banked to derive from a future body/frailty vector — see [Language](./language.md)) |
| Articulation | Exotic manner | authored, enum | None | Trill | language's exotic-segment gate (Click/Ejective idle — claimed by neither shipped people) |

Two rows are worth reading carefully rather than at face value. **Deliberation
latency**'s consumer is a genuine but partial one: Campaign Y2-3 folds it,
half and half with status basis, into language's formality knob, so the
dimension is spent on *how a myth is told*, not yet on the decisiveness-
facing salience or negotiation rule this chapter originally banked it for —
see "Idle by design; banked for later," above. **Activity cycle**'s
`Crepuscular` variant is authored into the closed enumeration and its lens
activity factor (0.7) is authored and ready in `domains/perception`'s
formula, but no shipped species carries the value that would call it — a
future crepuscular people is a data change, not a code change, exactly as
Click and Ejective wait in the articulation vector's own enumeration for a
species anatomy that claims one.

**A solitary dragon carries this table's Mind row and none of its Society
row.** The three chromatic dragons are `Solitary`, not `Settled`: each
authors a mind vector (threat response 0.95, deliberation latency 0.5, time
horizon 0.90 — an apex that stands, a banked deliberation dial, a
centuries-long hoarder) but no society vector at all. A consumer that needs
a society reading for a dragon — there is none among the unplaced-path
consumers today — resolves `SocietyVector::baseline()`, defined to equal
the goblin row above exactly.

**No drawn parameter lives on a species.** Every cell in the table above is
authored; `stream_labels()` in `domains/species` returns an empty vector
because there is nothing in this domain a seed ever touches. The seeded
draws that make two peoples sound and settle differently are real, but they
belong to the domains that consume this vector, not to species itself:
`domains/language` draws a phoneme inventory and syllable phonotactics
*under* the articulation envelope above (see [Language](./language.md)'s own
model card), and `domains/demography` folds threat response and time
horizon into the carrying-capacity field each species reads (see
[Settlement](./settlement.md)) — no draw at all any more, since *The
Gathering* moved population from a per-site draw to a field readout, but
still a real per-species divergence these two psychology scalars produce. A
species is the fixed point those draws (and, now, this one field) are taken
with respect to, never a distribution of its own.

**The peoples ahead:** deliberation latency's still-unspent half — the
salience or negotiation rule that would read a people's decisiveness
directly, now that language's formality knob has spent only the telling of
a myth, not the deciding of anything; a lexicon that would
give this chapter's role vocabulary (`digger`, `chief`, `elders`) a sound of
its own, the one stopgap Campaign Y2-3 (The Tongues) deliberately left
standing (it gave names a mouth, not common nouns a meaning); comparative
religion across more than a pair, once a third people exists to make
"comparative" mean something beyond two pantheons on one globe; drawn
variation, both per-species and eventually per-individual, in place of one
authored point per people; a physiology this vector does not yet have, so
habitat affinity and temperance stay shared rather than species-derived;
inter-species politics, trade, and conflict; and, past two, however many
further peoples the registry is asked to hold.
