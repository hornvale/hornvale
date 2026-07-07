# The Frontier — a non-binding vision map

**Status: SPECULATIVE. This document is not a specification and governs
nothing.** It records the maximum-ambition directions the project has
deliberately chosen to keep in view, each tagged with a confidence level, so
that sequencing decisions are informed and hard-won thinking is not
re-derived from scratch a year later. When any item here becomes real work
it earns a proper spec in `docs/superpowers/specs/`, and that spec — never
this map — is what binds. Where this document and a spec disagree, the spec
wins by definition. The public book (`book/`) describes only merged reality;
this file is the opposite end of the pipe, and stays out of the book.

Treat every idea below as a hypothesis with a confidence tag, not a promise.
The discipline that governs the code governs this document too: an idea
earns elaboration only when something concrete needs it, and the smallest
version that changes a downstream output is always preferred to the
elaborate one.

---

## The governing principle: ambition is bounded by verification surface

The deepest lesson of the project so far is that **the ceiling on ambition
is not cleverness but checkability.** Astronomy could be enormously ambitious
because physics supplies ground truth: mass–luminosity relations, Kepler's
third law, and stability inequalities are all things a world can be *wrong*
about, and tests can catch the wrongness. The most ambitious directions are
therefore the ones that maximize the number of checkable invariants, and the
honest way to read every confidence tag below is as an estimate of how much
of the idea can be verified rather than merely admired.

This produces a gradient, and the tiers deserve names — "name which tier an
ambition occupies" is only a usable instruction when the tiers are crisp.
**Formula-checked** systems sit at the top: a claim either matches a
physical law or it doesn't (astronomy). **Calibration-checked** systems sit
just below: the mechanisms are real (Hadley cells, lapse rates) but the
constants are tuned to Earth as a single ground-truth sample, so tests can
check distributions and qualitative reorganizations rather than exact
values — climate is the first resident of this tier, and settlements will
be the second. **Literature-checked** systems sit in the middle: a
generated joke can be checked against whether it cites norms the culture
actually holds, even if whether it *lands* is a matter of taste.
**Taste-checked** goals sit at the bottom and require a human in the loop
rather than a bigger single ask — chart legibility and prose quality live
here, and so, honestly, does "is this goblin funny." Naming which tier an
ambition occupies is the first act of designing it.

---

## The frontier map

1. **The Laboratory** *(high confidence; shipped in Campaign L0)*. A batch
   harness that sweeps seeds and pin-sets, extracts metrics, and publishes
   drift-checked distributions. It is listed first because it is the
   instrument that makes every other item on this list measurable: any
   claim that "cultures plagued by trickster gods develop more defensive
   architecture" becomes a study one can actually run. One discipline,
   ratified before the juicy claims arrive: determinism cuts both ways —
   seeds can be swept until a desired pattern appears — so a study
   preregisters its hypothesis in its JSON before the sweep runs, and the
   book's laboratory pages publish negative results (decision 0016). That is
   the difference between the Lab as instrument and the Lab as anecdote
   generator. Three expansion axes are already visible, each to be built
   when a campaign pulls for it rather than on spec: **time as a study
   axis** (every current census is a snapshot sweep across seeds; once
   peoples evolve, metrics want sampling along `WorldTime` within one
   world — population trajectories, settlement founding and abandonment
   rates); **sensitivity studies** (hold the seed, vary one pin, measure
   the downstream distributions — the instrument that catches "ocean
   fraction secretly dominates everything"); and **paired comparisons**
   (tier-0 versus generated provider on the same seed, turning "higher
   fidelity refines, never contradicts" from a unit-tested promise into a
   statistically monitored one).

2. **The epistemic layer** *(architecture-ready; whether it is fun is open)*.
   Player and scholar knowledge as a second, provenance-tagged ledger,
   distinct from the true generative ledger. Maps become documents with
   authors and errors; source criticism becomes a mechanic. The single
   biggest under-ambition risk, because the architecture already supports it
   and the payoff is large. The tier-0 move needs none of the second ledger:
   the almanac is already a *document*, currently written by an omniscient
   narrator. Generate the same document from a vantage point — one
   location's observable phenomena, one hemisphere of sky — and it becomes
   an honest, wrong-by-omission text through the existing almanac-context
   seam, with the diff against the true almanac supplying the measurable
   wrongness score years early.

3. **Deep language** *(high confidence on mechanics)*. Per-culture
   proto-languages plus deterministic sound-change laws applied over
   historical time; place names as fossils, etymology as archaeology.
   Sound-change engines are solved technology. Prestige and liturgical
   conservatism act as a *damping field* on change, producing diglossia,
   frozen ceremonial registers, and dead languages.

4. **Myth phylogenetics** *(medium confidence; genuine research)*.
   Proto-myths as motif structures propagated through noisy cultural
   transmission, then reconstructed by comparative method and scored against
   the true generation tree. Nobody has a laboratory for this; Hornvale
   would be one.

5. **Models author, dice roll** *(high confidence; now a ratified
   constraint)*. Language models used offline as authoring amplifiers that
   produce committed data — grammars, motif libraries, lexicons, expert-
   system rule bases — while runtime generation stays purely deterministic.
   There is never a runtime model in the simulation core. See the ratified
   decision in the long-term-plan spec.

6. **Paleoclimate as the first history** *(high confidence;
   formula-checked — the top of the gradient)*. Everything built so far is
   genesis-time: the world has a present but no past. The cheapest deep time
   on offer is Milankovitch-style glacial cycling — a pure function of
   orbital elements the astronomy domain already holds (obliquity,
   eccentricity, precession), so ice ages derive from the existing sky with
   no new draws. That yields strata: fossil shorelines, "this valley was
   under ice," refugia — archaeology-grade material for myth ("the frost
   retreated"), a past that predates all witnesses for the epistemic layer
   to reconstruct and mis-reconstruct, and biogeography later. Social
   history sits low on the verification gradient; this history sits at the
   very top, which is why it should come first.

7. **Population as a field, settlements as its condensations** *(high
   confidence; calibration-checked — the tier's second resident)*. The
   tier-0 settlement model is a static equilibrium: suitability scoring
   plus separation answers "where would settlements be," never "how did
   they get there." The upgrade path: a carrying-capacity field derived
   from climate and terrain, a population-density field relaxing toward
   it, and discrete settlements as condensations of that field — textbook
   coarse-constrains-fine, with the field as the tier-0 prior that any
   finer dynamics must integrate to. History then arrives as an era-ticked
   pass at genesis, appending founding, growth, fission, and abandonment
   to the ledger as facts — so ghost towns, mother-city lineages, and
   migration waves exist as narrative substrate the moment historiography
   and religion look, because a settlement abandoned two eras ago *and
   why* is a story in a way a settlement that merely exists is not. The
   species-psychology substrate slots in as per-species dispersal appetite
   and tolerance bands feeding the same dynamics, yielding niche
   partitioning and contact zones (which writing and religion later
   consume). One tension resolved in advance: era-scale ticking happens at
   genesis and lands in the ledger, keeping the ledger the single source
   of history and observation cheap; lazy fine-scale derivation can come
   later, constrained by the coarse record. This is the social history
   that item 6 deliberately sequences behind physical history.

8. **Writing as a culture acquiring its own ledger** *(high confidence on
   mechanics; downstream of surplus, stratification, and contact)*. The
   architecture gives literacy a meaning most simulations cannot: the
   world already distinguishes facts (durable, append-only) from phenomena
   (salience-ranked observations whose producer is hidden). An oral
   culture should know the world only through phenomena — lossy, decaying,
   drifting into myth over generations; that religion consumes phenomena
   without learning their source *is* oral epistemology, already shipped.
   A literate culture gets to freeze phenomena into facts: a recorded
   eclipse stays a dated eclipse instead of becoming a dragon story.
   Writing changes a people's relationship to time, and the trace protocol
   expresses that natively; the historiography window is the payoff —
   in-world chronicles whose reliability varies with the script's
   maturity, feeding the epistemic layer (item 2) exactly the biased
   sources it wants. The evolution is friendly to the emergent-structure
   style: scripts appear where the structure ladder already grows artisans
   and surplus (accounting tokens → pictographs → rebus → phonetic
   borrowing), and are mostly *borrowed* rather than invented —
   independent invention happened perhaps three or four times on Earth —
   so script families become a fossil record of contact between peoples,
   legible in ruins. Deep language (item 3) supplies the sounds; this
   supplies their shadow on clay.

---

## The unification thesis

The deepest structural claim the project has arrived at: **science, magic,
theology, and roguelike identification are one mechanic — inference over a
hidden, seeded ruleset.** A culture investigating its sky, a mage probing a
second physics drawn per seed, a priesthood reading intent into phenomena,
and a player identifying an unknown potion are all doing the same thing at
different altitudes. This means the Laboratory's extractor-over-a-ledger
design is also the first sketch of a simulated scholar's cognition: an
epistemic agent is a lab-runner working from partial, biased data.

Two corollaries. Magic as a *second physics* promotes pins from parameter
selection to rule-system selection, making comparative metaphysics a
controlled science. And interventionist or trickster deities are adversarial
noise injected into a civilization's inference process — which turns the
vision book's "belief makes gods real" loop into a concrete feedback edge
between the ledger and its agents.

---

## The expressive-culture cluster

A family of systems that all reduce to the same shape: a small parameter
vector per culture or species, consumed by **one seeded grammar engine that
targets four media — text, image, music, and space.** The language
campaign's prose engine should be designed as this substrate from the start,
even if it ships with only one medium lit.

- **Music and tuning** *(medium; post-culture)*. The Pythagorean comma is a
  hidden ruleset that is *mathematically necessary* rather than seeded —
  every culture's tuning is a negotiated compromise with incommensurability,
  and the compromise cascades into both musical form and theology (pure
  intervals and "the gods keep their harmonies" versus equal temperament and
  its "indisputable deception"). Tuning stance is the reflexivity field
  applied to music, and a belief-ledger fact with theological provenance.
  Audio, like the First Light bitmap, would be hand-rolled deterministic PCM
  — a listenable, drift-checked gallery.

- **Species perception** *(medium; needs a species layer)*. Per-species
  visual profiles — photoreceptor channels, rod/cone balance, luminance
  range, grounded in comparative-vision literature. The load-bearing
  consequence: **salience becomes a function of both the phenomenon and the
  observer,** so different species see different night skies from identical
  heavens, and therefore build different astronomies and religions in the
  same valley, each empirically correct about what it sees. The
  `ObserverContext` extension point left in Campaign 1a is where this lands.
  Downstream: art palettes conditioned on species gamut and ambient
  illuminant; colour lexicons (Berlin & Kay) as evidence of a species'
  vision; a drawn atmospheric *scattering regime* (Rayleigh blue, Mie-dust
  warm, haze orange) feeding sky and twilight colour.

- **Humor** *(taste-gated; spot-checked, not automated)*. Benign-violation
  theory makes humour a *function* of a culture's norms, fears, body plan,
  and hierarchy — all parameters the project already plans to hold. Goblin
  jokes about tall folk striking their heads are body-plan incongruity plus
  status inversion; a humourless species is one whose threat baseline leaves
  the benign-violation set empty. Jokes are the fastest norm-probes in the
  game: a joke you must have explained measures your cultural distance from
  its teller.

- **Drama** *(medium; couples to lifespan)*. A performed narrative is an
  error-correcting code tuned to its culture's transmission channel, and
  **lifespan sets the channel.** Short-lived oral cultures need
  high-redundancy encodings (formulaic epic, communal spectacle); near-
  immortals have a near-lossless channel and inverted scarcity, so elvish
  opera might be performed *once, ever*, brief and dense because
  reinterpretability sustains a two-century discussion — and death becomes
  the exotic subject rather than the universal tragedy.

- **Architecture** *(high; the most tractable of the cluster)*. Settlement
  layout as constraint satisfaction over body plan, environment, social
  structure, and threat model — with a real pedigree in shape grammars
  (Stiny) and space syntax (Hillier). The parameter vector: a status-
  projection rule (which spatial axis encodes rank), a sacred-placement rule,
  a household unit, a commons rule, and a defense mode. Christopher
  Alexander's *A Pattern Language* is the authored pattern library this
  wants — "models author, dice roll" for space — and his "A City Is Not a
  Tree" supplies a quality criterion: living settlements are overlapping
  semilattices, not clean hierarchies, and overlap is measurable, hence a
  candidate Lab metric. **Sacred placement is derivable, not drawn:** order-
  legitimating (priestly) religion sites its temple at the center; spirit-
  mediating (shamanic) religion sites its shrine at the liminal edge (Turner;
  van Gennep) — so a player can read a culture's theology off its map.

  *Near-term hook:* when Campaign 4's settlement tiers are designed, tier 1
  should be a layout grammar, not a population count at a point.

---

## The species-psychology substrate — the layer beneath the cluster

*(High potential value; verification is weaker here — flagged.)*

The single idea that unifies the expressive-culture cluster rather than
sitting beside it: **culture is the scaffolding a species builds around its
own psychology.** Humor, drama, sacred architecture, and prestige-damping of
language all silently assume a psychology; making that psychology an explicit,
bounded, per-species parameter vector turns four separate systems into
consequences of one.

Candidate axes, to be kept minimal and promoted only on demonstrated
downstream need: threat response (freeze / flight / fight as a distribution —
fae going *tharn* versus goblins joining a brawl before parsing the sides);
deliberation latency (the impulse-to-action gap that makes the goblin mid-
brawl side-switch a *known cultural element*); sociality mode (eusocial /
pack / pair-bonded / solitary — the master axis, since it reshapes whether a
"bystander effect" is even a coherent notion); in-group radius; time horizon
(coupling to the lifespan parameter under drama); and status basis (which
feeds the architecture status-projection rule directly).

**Two honest cautions, louder here than anywhere else.** First, the
verification story is weaker: there is no real-world goblin to be wrong
about, so the checkable claim is *internal coherence and downstream
consequence* — does the goblin profile actually produce goblin culture,
architecture, and jokes that hang together? — not correspondence to ground
truth. The Lab is what makes even that testable. Second, this is where the
ontology trap is most dangerous: six axes is a design, sixteen is folk
psychology ossified into false science. An axis that changes no settlement,
joke, or myth has not earned its place.

*Sequencing:* this is a strong Year-2 candidate for its own small "species
substrate" campaign, sitting **upstream** of the culture campaigns (C4/C5) so
they consume a psychology layer rather than each reinventing one piecemeal.
It does not alter the Year-1 roadmap.

---

## Social structure as orthogonal axes — cashing the substrate

*(High confidence on direction; literature-checked via cross-cultural data.)*

The tier-0 social model is one linear ladder with threshold-gated rungs
drawn from a closed six-role vocabulary — the right lean start, and every
society it can express is a human chiefdom, more or less stratified. Three
hardcoded assumptions cap the exoticism: a **single hierarchy** (no
moieties, age-grades, parallel priestly/secular ladders, or acephalous
segmentary societies — nothing whose org chart is not a ladder); a
**closed role vocabulary** (every culture stratifies into the same six
nouns); and **environment-only inputs** (the same valley produces the same
society regardless of who lives in it — precisely the input the
species-psychology substrate above exists to supply).

The fix, when its campaign comes, is decomposition rather than a bigger
enum: orthogonal axes — authority locus (hereditary / achieved /
gerontocratic / distributed), legitimacy source (sacred / martial / wealth
/ consensus), descent and household form, exchange mode — with the
psychology vector acting as a *viability filter* over combinations, so a
eusocial species' viable structure space barely overlaps a solitary
territorial one's. Exotic structures then emerge from combinatorics nobody
authored — a gerontocratic gift-economy moiety system falls out of axis
values plus a disposition, and the interesting ones are the corners of the
space no one foresaw. It is the same generative-over-taxonomic move the
rest of the project makes. The verification hook that keeps it honest:
Murdock's *Ethnographic Atlas* is a real cross-cultural joint distribution
to sanity-check the axis space against — more ground truth than most
social systems ever get. Downstream, religion, the architecture cluster's
status-projection rule, and writing all read from axes; none of them can
read anything from a role ladder.

---

## Provenance interrogation — the `explain` verb

*(High confidence; small; no new architecture required.)*

Determinism makes derivation traces essentially free: because every derived
thing is a pure function of the seed and the ledger, justifications need not
be *stored*, only *replayed on demand* with tracing switched on. "Why is the
shaman's hut here?" re-runs that settlement's derivation in trace mode and
emits the actual decision path down to the stream draw that broke a tie. This
is a natural CLI verb — `hornvale explain --world w.json <thing>` — sibling
to `almanac` and `scout`, with a REPL "more info" affordance over it, and it
doubles as the tool for explaining a study's outliers.

*Near-term hook:* tier-1 climate makes this even cheaper than the general
story, because climate draws **nothing** — it is a pure dataflow with no
stream draws to capture, and the answer to "why is this desert here?" is
already computed as intermediate values (prevailing wind → upwind ridge →
moisture stripped). The rain-shadow question is `explain`'s natural tier 0
and a candidate Campaign 4 opener, not a someday-verb.

The important distinction: the debug "why" and the player "why" are
different objects. `explain` reads the *true* generative trace, omniscient.
The player-facing counterpart is a generated **ethnography** built only from
the player-knowledge ledger, with its own evidence provenance (including
unreliable informants). The *difference* between the two documents — what the
player got wrong — is measurable, and therefore a score, a gameplay beat, or
both. Asking an NPC to explain a joke is this in miniature: the explanation
is a norms lesson, and needing it marks the asker an outsider.

---

## Two sequencing notes for the remaining Year-1 campaigns

Both are about cashing dividends already banked, not proposing new systems.

- **Tidal lock is the cheapest alien-religion generator.** The Year-1 exit
  demo is two worlds differing only in astronomy yielding legibly different
  religions, and Campaign 3c ships the precursor: rotation pins alone
  reorganize an entire climate (banded versus day–night). Campaign 5 should
  cash this deliberately — a culture on the locked world's terminator ring
  has a *motionless* sun: no days, no seasons, no celestial cycles at all,
  so its entire temporal religion must hang on moons, weather, and tides.
  Radical religious divergence falls out of astronomy alone, before species
  psychology exists.

- **Hydrology is People-infrastructure, not Land-refinement.** Rivers are
  banked as terrain polish (with erosion, out of Campaign 3's scope), but
  they are the skeleton of Campaign 4: settlement placement, borders,
  trade, and half of real-world toponymy hang on them. Sequencing rivers
  should be decided on Campaign 4's grounds — possibly inside it — not
  deferred as a geological nicety.

---

## Intellectual lineage

So the reasoning is recoverable when it has gone cold: benign-violation
theory (humour); Parry–Lord oral-formulaic tradition (drama as redundancy);
Berlin & Kay (colour lexicons); comparative vision science (species
perception); Stiny shape grammars and Hillier space syntax (layout);
Christopher Alexander, *A Pattern Language*, *Notes on the Synthesis of Form*,
"A City Is Not a Tree" (patterns and the semilattice criterion); Victor
Turner, *The Ritual Process*, and Arnold van Gennep, *Rites of Passage*
(liminality and sacred placement); Mary Douglas, *Purity and Danger* (queued,
for taboo generation); James Ryan and the UCSC Expressive Intelligence Studio,
*Talk of the Town* / *Bad News* (character-scale knowledge and gossip);
Solodow, *Latin Alive* (prestige as a language parameter); Milutin
Milanković, orbital forcing of climate (paleoclimate as the first history);
Denise Schmandt-Besserat, *Before Writing* (accounting tokens as writing's
ancestor); George Murdock, *Ethnographic Atlas* (the cross-cultural joint
distribution behind the social-structure axes); Walter Christaller, central
place theory (settlement condensation over a carrying-capacity field).
The map moves when the reading feeds it — which is the collaborator's role,
demonstrated.
