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

**The closed vector, baseline goblin.** The species vector is closed at six
dimensions — four scalars bounded in `[0, 1]` (threat response, deliberation
latency, in-group radius, time horizon) and two enumerations (sociality
mode: hierarchic or communal; status basis: rank, knowledge, or generosity).
Goblin is the baseline: every scalar sits at exactly 0.5, every enum at its
goblin variant, and every downstream formula that reads the vector is built
so the baseline value reproduces today's behavior — not tuned to match, but
constructed to match, by writing the formula as an expression like
`0.5 + time_horizon` or `1.5 − threat_response` that evaluates to today's
plain constant precisely when the vector reads "goblin." This is the
mechanism, not merely a claim, behind the campaign's keystone test: a world
generated with the species pin restricted to goblin must be byte-identical
to a world generated before species existed at all, because "goblin" and
"no species substrate" are, by construction, the same input to every
formula in the cascade.

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

**The kobold model card.**

| Dimension | Type | Goblin (baseline) | Kobold | 5E derivation |
|---|---|---|---|---|
| Threat response (flee ↔ stand) | scalar `[0,1]` | 0.5 | 0.8 | cowardly in the open field, but entrenched at home behind the traps and tunnels the warren has prepared |
| Deliberation latency | scalar `[0,1]` | 0.5 | 0.7 | communal decisions arrive by slow consensus |
| In-group radius | scalar `[0,1]` | 0.5 | 0.2 | insular warrens; loyalty runs tight and does not extend past the pack |
| Time horizon | scalar `[0,1]` | 0.5 | 0.8 | generational works — tunnel complexes, egg-tending — that pay off only across years |
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
yet. Deliberation latency is authored, differs meaningfully between the two
peoples, and is still consumed by no rule — declared idle in the model card
rather than force-fitted into a formula that does not need it, because a
dimension can exist for the contrast it will eventually explain rather than
for use on a deadline; it remains banked for a future salience or
negotiation rule, once observers judge each other's decisiveness. Kobold
nocturnality was banked the same way through this chapter's first campaign,
more literally: an activity-cycle dimension had no seat in this closed
vector, so nocturnality lived only as authored prose with nowhere to run.
Campaign 15 (The Eyes) spent it: `domains/species` now also holds a closed
three-dimension **perception vector** — activity cycle, night vision, sky
attention — sitting alongside this six-dimension psychology vector on the
same `SpeciesDef`, and kobold's authored nocturnality finally reaches a
formula, a species-specific characteristic hour and salience lens that
crowns the moons and the night sky over the sun. See
[Perception](./perception.md) for the vector, the lens derivation, and what
it let religion do with two peoples instead of one. Campaign Y2-3 (The
Tongues) added a third: a closed six-dimension **articulation vector** —
labiality, vowel-space breadth, voicing contrast, sibilance, voice
loudness, and an exotic manner a species' anatomy affords — again authored
at the goblin baseline and read from the same D&D 5E corpus for kobold, and
again sitting alongside the other two on the same `SpeciesDef`. See
[Language](./language.md) for the vector itself and what it let every name
in the world become.

**The ontology-trap posture.** This closedness is a stance, not an
oversight. The frontier map warns, more than once, against traits that grow
without a designer choosing to grow them — an inheritance hierarchy here, an
extensible trait bag there — and this vector is where that warning is
finally enforced: six dimensions, no more, no per-individual variation, no
inheritance between species. A seventh dimension, or a psychology that
varies goblin-to-goblin rather than only goblin-to-kobold, is real work with
its own design principles; it does not arrive as a quiet addition to a
struct that happened to have room.

**The peoples ahead:** deliberation latency, still idle after three
campaigns' worth of dimensions consuming its neighbors; a lexicon that would
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
