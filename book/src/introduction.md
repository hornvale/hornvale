# Introduction

This is the project book of _Hornvale_: the living technical record of a
multiscalar world simulation observed through text. It describes the system
*as it actually exists* — its principles, its architecture, what each campaign
of work built, and what remains open.

It is deliberately not the [vision book](https://hornvale.github.io/book/),
which explains *why* this project exists and what it wants to feel like. Read
that one first if you want the soul; read this one if you want the machine.

## The altitude of this book

This book is written at a deliberate level of abstraction: technical,
mathematical, and conversational. Rust appears where it illuminates — a type
signature that *is* the design, a ten-line function worth savoring — but
never as a substitute for explanation. The test: every chapter must be fully
comprehensible without reading any code it shows, because this book is not a
code review. The code is the implementation; this book is the
*comprehension* — and keeping its principal human author fully cognizant of
the system is not a courtesy but a methodological requirement. Hornvale
claims research ambitions, and a result that no human comprehends is output,
not research.

(Interactive evaluation blocks — type a seed, watch a world generate, right
in the page — are planned once the kernel is compiled to WebAssembly. The
standard mdBook "run this code" button won't work here, since it executes on
the public Rust playground, which doesn't know Hornvale exists; our own
in-browser widgets will.)

## How this book stays honest

Documentation drifts. This book has two defenses:

1. **Generated artifacts.** Hornvale is deterministic by constitution: the
   same seed always produces the same world, byte for byte. So the Gallery's
   world documents are regenerated from pinned seeds, and they change *only
   when the system's behavior changes*. A changed artifact is a semantic
   changelog entry, legible without reading code — and CI regenerates the
   artifacts and fails if the committed copies differ, so the Gallery cannot
   silently go stale. (Generating the reference chapters from the live
   system is the remaining piece, planned for Campaign 1b.)

2. **The comprehension gate.** Every campaign closes by updating this book,
   and the update is reviewed by a human who must be able to explain the
   campaign from the book alone. If he can't, the chapter is rewritten until
   he can.

## Where things stand

**Campaigns 1a and 1b are complete.** The kernel exists (seeds, noise,
fields, the fact ledger, refinement, the trace protocol), and above it the
entire domain cascade exists at tier 0: a sun that never sets, a uniformly
mild climate, one vale, the goblin village of Gruugish, a five-rung caste
ladder, and one belief — derived, with queryable provenance, from the most
salient thing in the sky. The `hornvale` tool creates worlds, answers
questions about them interactively, and renders the almanac; an end-to-end
suite proves the whole path byte-deterministic. The world is very small, and
every part of it can explain itself, which is the entire point.

**Campaign 2 is complete: new worlds have real skies.** A generated world's
sun rises and sets on a seed-drawn day length; its moons show phases; its
night holds notable stars; its almanac has a Calendar. Worlds carry their
experimental configuration — the pins — as facts in their own ledgers, and
rebuild their skies from them at load. The campaign's exit criterion is
proven and published: the same seed's goblins worship a returning god under
a spinning sun and an unblinking one under a tidally locked sun, with
religion's code untouched — the enrichment thesis's first real data point.

**Campaign 3 gives the world land.** Terrain grows a tectonic globe —
plates, elevation, a drainage skeleton — and climate turns that globe and
the sky together into circulation bands and a biome map, the first proof
that two domains importing nothing from each other still produce a
legibly coupled world: coarse constrains fine, all the way from rotation
to which biome a given cell grows.

**Campaign 4 places a real settlement scatter and grows a society from
it.** The single hand-fed vale is retired; every world scores every
habitable cell for suitability and places a spaced scatter, with the
global suitability argmax standing as the flagship. That flagship's
subsistence mode and caste ladder are no longer a fixed template — they
emerge from the land's own fertility, population, and threat, functions
the Lab can check row by row against ten thousand worlds.

**Campaign 5 gives that society gods, and closes Year 1.** Religion stops
minting one belief and mints a pantheon, shaped by the sky's phenomena and
the flagship's own social strata; `why`/`recount` generalizes into a
domain-agnostic provenance replay for any entity, not just a belief. Every
domain the plan named is now at tier 1, and the astronomy-to-theology
cascade — one rotation bit at genesis producing a different god's name at
the top of an almanac page, with no domain aware of any other — is proven
rather than promised.

**Campaign Y2-0 (Firm Ground) opens Year 2 by fixing what Year 1 got
wrong before building on it.** A placement-suitability conflation had made
the flagship coastal in every censused world, silently forbidding two of
culture's four subsistence modes from ever appearing; the fix is one line
at the composition root, and every committed artifact and 10k census
re-baselines exactly once so later campaigns inherit corrected numbers
instead of re-deriving them mid-flight.

**Campaign Y2-1 (The Peoples) gives the world a second people.** A new
kernel-only domain, `domains/species`, holds authored psychology vectors —
goblin as the baseline, kobold authored from the D&D 5E SRD — consumed by
settlement's now-joint placement and culture's role vocabulary and
thresholds, with a byte-identity superset test proving the goblin path is
untouched by construction, not by convention. Two peoples now compete for
the same land, diverging in where they settle and how they organize for
reasons a player can be told; a census turns up competitive exclusion, an
unauthored emergent property of joint placement that runs in both
directions.

**Campaign 15 (The Eyes) gives each people its own eyes, and closes the
gap Campaign Y2-1 left open.** A closed three-dimension perception vector —
activity cycle, night vision, sky attention — sits alongside the
psychology one on every species definition, authored the same way and read
from the same D&D 5E corpus. A species-specific salience lens and
characteristic hour replace the single observer-independent salience every
prior campaign assumed, built so the goblin baseline is the identity
function by construction; religion now runs once per species-flagship,
each through its own lens, so a goblin's sun-headed pantheon and a
kobold's moon-headed one stand side by side on the same globe under the
same seed. A 10,000-seed census confirms the two exact claims the campaign
staked out before running it — goblin heads are always solar, and kobold
heads are always lunar on every world that carries a moon — and measures,
rather than assumes, what happens on the moonless remainder: the sun
reclaims most kobold pantheons there, a fact about eternal night-stars and
finite moons, not a defect in the rule that found it.

**Campaign Y2-3 (The Tongues) gives every name in the world a mouth.** A new
kernel-only phoneme model and a third closed species vector — six
articulation dimensions, again authored at the goblin baseline and read
from the D&D 5E corpus — feed a phonology engine that draws a real inventory
and real phonotactics per species-culture. Every settlement, deity, and
deity epithet the world mints is now a generated sound rather than a
syllable-pool guess or a shared English word list, carrying both a
romanization and an IPA transcription from the same underlying feature
bundle. Religion's own tenet fact — assembled English, frozen at genesis —
is retired; a belief now commits a deity's name, epithet, and sentiment as
structured content, and a permanent content→render seam, `render_line`,
turns that content into voiced text at display time, under three knobs
derived from psychology so a `Rank`-basis people's telling reads formal and
honorific-dense and a `Knowledge`-basis people's reads repetitive and
descriptive — the same status-basis split that already shapes each
people's caste ladder now shaping how each people speaks. A 10,000-seed
census measures what a vast, per-species name space actually buys: 100%
phonotactic validity, honorific affixation present in every goblin epithet
and absent from every kobold one, and a mean cross-world name collision
rate of 2.79%, against the roughly 1,100-name ceiling — and correspondingly
routine collisions — the old ten-syllable pool lived under.
