# Religion

**Questions it answers:** What do these people believe, and *why*?

The second question is the project's soul, and religion is the first domain
built to answer it: every belief carries a committed record of which
phenomenon it mythologizes, so the REPL's `why` command can trace faith back
to observation. Ask Gruugish's people why they revere the Unblinking Eye and
the answer is genuine provenance, not flavor text.

**Tier 0 — one belief from the top phenomenon.** Genesis looks at the
salience-ranked phenomena visible from the village and mythologizes the
first. The templating is deliberately minimal — two shapes:

- An **eternal** phenomenon (no period) becomes a changeless watcher: *"the
  Unblinking Eye is a golden sun fixed at zenith; it has never departed and
  will never blink."*
- A **periodic** phenomenon becomes a cyclic deity whose absences are mourned
  and returns feasted — the life-death-rebirth structure from the vision
  book's *Golden Bough* chapter, waiting for a sky that actually cycles.

The epithet is drawn from a seeded pool, so different worlds venerate
differently-named gods of the same sun.

**The constitutional point.** Religion consumes *only* phenomena — it imports
nothing from astronomy, climate, or anything else. Its test suite proves this
interface-blindness directly: the tests hand-construct a fictional phenomenon
(a pale wanderer with a thirty-day period), which exists in no world and
which no tier-0 provider can emit, and feed it straight to genesis. Religion
produces a correct cyclic tenet from it anyway. To be clear, the *world*
contains no moon and assumes none — the point is about the code: religion is
written against phenomena in general, so it is already correct for kinds of
sky that do not yet exist. When Campaign 2 gives the sky periods and Campaign
5 deepens the templating, the *legibly different religions from different
skies* demo — year one's exit criterion — is a composition of parts that
already work, not a new invention.

**Honest limitations of tier 0:** one belief, two templates, no relationship
to social structure, no myth *narrative* — a tenet, not a story. The tier
ladder (Campaign 5) consumes seasonal structure and culture's social facts,
generates pantheons rather than single tenets, and begins the historiography
turn: beliefs as *accounts told by someone*, not facts about the world.

**Tier 1 — the pantheon (Campaign 5).** Religion stops minting one belief
and mints a **pantheon**: one deity per phenomenon the flagship's community
observes above a salience floor of 0.25, shaped by the eternal/cyclic
templates tier 0 already trusted. The interface grows by exactly one input —
a bare `SocietySummary { strata, has_priesthood }`, mapped at the
composition root from the flagship's committed castes (Campaign 4b) — and
nothing else; religion still imports no domain, and still learns nothing
about which system produced a phenomenon. Two levers, both already-derived
facts about the society, decide the pantheon's *shape*; the sky alone still
decides its *members*.

- **Membership, no cap.** Every phenomenon at or above the floor seats a
  deity — a spinning sky's sun, its seasons, and its moons all mythologize
  independently, with no ceiling on how large a pantheon can grow. A world
  is never godless while it observes anything: if nothing clears the floor,
  the single most salient phenomenon still seats one. Phenomena below the
  floor that do clear it in a richer sky (a faint moon, the ambient air)
  become minor spirits — **esoterica**, present but easy to miss, the same
  low-salience-but-real principle climate's ambient air demonstrated at
  tier 0. Each deity's tenet still follows the eternal-vs-cyclic split
  exactly as tier 0's two templates did: an aperiodic phenomenon yields a
  changeless watcher, a periodic one a life-death-rebirth cycle of absence
  and return.
- **Verticality, from social strata.** A flagship whose emergent structure
  (Campaign 4b) reaches four or more castes — the stratified size-5 towns —
  gets a **ranked** pantheon: its single most salient deity is flagged
  `high-god` and presides over the rest. A lean camp of two or three castes
  gets a **flat** pantheon of co-equal spirits. Verticality is a known
  function of a number the census already had, so it is exactly checkable,
  not merely plausible.
- **Priesthood, from the shaman caste.** A flagship whose structure includes
  a `shaman` caste tends its pantheon as an **organized** cult, its beliefs
  held by the priesthood rather than the whole community, which instead
  gets a **folk** cult, animism shared by everyone. This is the same
  society-summary input as verticality but a genuinely separate axis — a
  lean structure can carry a shaman without reaching four strata, so an
  organized cult over a flat pantheon is a real, observed combination, not
  a contradiction (Study 004 has the numbers).
- **Two new predicates.** `high-god` (a functional flag on the presiding
  deity, present only in ranked pantheons) and `cult-form` (functional
  Text, `organized`/`folk`) join the tier-0 vocabulary
  (`is-belief`/`tenet`/`held-by`/`derived-from-phenomenon`) unchanged.

**Per species-flagship, each through its own eyes (Campaign 15).** Religion
still takes the same two inputs it always has — a salience-ranked list of
phenomena and a `SocietySummary` — but the composition root now calls it
once per species that placed a flagship, not once per world. What changed
sits entirely upstream, in [Perception](./perception.md): each species
observes at its own characteristic hour and reweights what it saw through
its own lens, so the phenomena list religion receives for a kobold warren
is genuinely different from the one it receives for the goblin village on
the same globe, without religion importing perception any more than it
ever imported astronomy. The priesthood check moved from a literal
`"shaman"` string match to each species' own shaman-rung word — a kobold
`"keeper"` presides over an organized cult exactly as a goblin `"shaman"`
does. The almanac's "The Gods" section holds one block per pantheon that
formed, and which block carries a species-qualified lead is decided by the
pantheon itself, never by its position in the list: a block names the people
who hold it whenever the world has two or more peoples to tell apart, and
falls back to the anonymous, pre-Eyes lead when it has none. That is the
same name-only-when-there-is-ambiguity convention the People section
follows, and both sections read it off one shared predicate so they cannot
drift apart. The identity contract is still cashed at the page a reader
actually sees: a legacy save carrying no `peopled-by` facts, and a
single-people world, each reproduce the pre-Eyes page byte-for-byte.

An earlier rule gave the anonymous lead to whichever block came first. It
assumed the registry-first species held that block's beliefs, and that the
registry-first species was goblin — an assumption that expired quietly when
the roster reached four peopled species, because the registry is ordered
alphabetically and bugbear sorts ahead of goblin. Block zero was never a
fact about the world, only about iteration order, and goblin's pantheon had
been rendering stripped of its attribution beside named neighbours on
roughly one seed in six by the time The Named repaired it.

**Meaning committed, voice rendered (Campaign Y2-3, The Tongues).**
Religion's own tenet fact — an English sentence, assembled once and frozen
into the ledger at genesis — is retired; the constant stays defined so
pre-Tongues saves still carry it and historiography can still recount them,
but new genesis never commits one. In its place a belief commits
**content**: a deity's generated name and epithet (`deity-name`,
`deity-epithet`, each with a roman and an IPA transcription alongside it),
and a `sentiment` — watched, mourned-and-feasted, or felt through the
ambient world — derived from the source phenomenon's venue and periodicity
the same way the eternal/cyclic split always was. Religion asks for a name
and an epithet through one trait, `DeityNamer`, without knowing what
produces them; the composition root backs it with `domains/language`'s own
`Namer`, so a deity's name and title are drawn from its own species'
phonology rather than an English word list. The almanac and the REPL render
a tenet's surface at display time, from these committed facts, through
`render_line` — `domains/language`'s permanent content→render seam — never
from a frozen sentence again. See [Language](./language.md) for the seam
itself and the voice knobs (formality, repetition, epithet density) that
carry a `Rank`-basis society's dominance honorifics and a
`Knowledge`-basis society's descriptive repetition into the telling.

**Provenance by replay: `why`/`recount`.** Every belief already carried a
committed `derived-from-phenomenon` record; Campaign 5 gives the world a
general way to read it back. `windows/historiography::recount(world,
entity)` replays any entity's committed facts, their provenance strings,
and the registry's predicate docs into a derivation sentence — for a belief,
a chain naming its phenomenon kind, its cult form, and who asserted it; for
any other entity (a settlement, a place), the same generic replay, because
`recount` is domain-agnostic and interprets no predicate specially. The
REPL's `why <id>` verb, previously wired straight to religion's own belief
lookup, now calls `recount` for any entity id — subsuming tier 0's `why`
without changing what a player already knew to type. This is
**historiography tier 0**: a flat, single-entity replay, deliberately not a
causal chain across entities. It is the seam the Year-2 event ledger and
fields-of-history will deepen, not a preview of them.

**The exit demo.** Seed 42's flagship, spinning sky, tends a pantheon headed
by **Neb the Ngobneb**, a cyclic deity whose absences are mourned
and returns feasted; pin the same seed's sky to tidally locked and the head
becomes **Neb the Nodneb**, an eternal watcher fixed forever
above the day side — and, since Campaign 20 (Firm Ground II) placed the
observer on its own ground, the pantheon shrinks around it: a locked
world's settlements all stand on the day side, the day side sees the sun
and nothing else, so the moon deities the spinning pantheon keeps never
form at all. The same globe, a different flagship under each sky (since
Campaign Y2-0's placement fix, the sky moves settlement too), yet two
societies alike enough to hand religion the same summary — so the theology
moved exactly where the sky moved and nowhere else. Since Campaign 15, the
almanac's "The Gods" section is no longer one pantheon but two: the kobold
warren standing beside each goblin village raises its own pantheon, through
its own lens, at its own hour, and its head deity is lunar rather than
solar wherever there is a moon to see (**Raxarro Rro**, marked highest, under
the spinning sky) — a divergence that tracks *species*, orthogonal to the
one above that tracks rotation. On the locked sky the warren's day-side
vantage holds no moon at all, so it too collapses to a single sun-watcher,
**Rora Ra** — domain converges on the locked sky (both peoples watch the
one body left), and today's seed 42 happens to place both flagships large
enough to raise an organized cult either way, so the species split that
survives there is voice alone, not cult form: cult form is a settlement's
own earned property (whether its role ladder grows a priestly rung), not a
species constant, and [The Branches](../chronicle/the-branches.md)'s
founder floor moved the kobold flagship to a bigger cell than the one this
page described before two more peoples joined the roster. Since Campaign
Y2-3, every one of those names is a real generated sound, drawn from its
own species' phonology rather than a shared English epithet pool — the
goblin telling formal and honorific-dense (*"...returns every 0.88 days. So
it was, so it is."*), the kobold telling repetitive (*"...comes back every 15.99 days.
That's how it's always been. That's how it's always been."*), the same
`Rank`-versus-`Knowledge` status-basis split that already shapes each
people's caste ladder now shaping how each people *tells* its myths. The
full "The Gods" sections for both peoples, under both skies, are quoted
verbatim in the gallery: [The Gods of Seed
42](../gallery/the-gods-seed-42.md). Since Campaign 27 (The Words), a
deity's name and epithet also carry a truthful **gloss** — the phenomenon
it mythologizes and the sentiment religion already derived for it,
committed as a `name-gloss` fact exactly as a settlement's does — so a
head god's name is no longer only a sound; it is a sound that means the
thing it presides over. See [Language](./language.md)'s "Glossed names"
for the mechanism and [Settlement](./settlement.md) for the parallel
settlement-name gloss.

**The model card.**

- **Drawn:** each deity's name and epithet, salted by the belief's own id,
  from the flagship species' own phonology (Campaign Y2-3;
  `domains/language`'s `Namer` — see [Language](./language.md)) rather than
  the fixed English pool tier 0 through Campaign 15 used. Religion itself
  draws nothing from the seed any longer (`stream_labels()` is empty);
  naming is entirely the `DeityNamer` implementation's business.
- **Derived:** pantheon membership from the salience floor; verticality
  from strata; cult form from priesthood presence; each tenet from its
  phenomenon's periodicity, exactly as tier 0's two templates already did.
- **Approximated (declared):** one flagship's pantheon per species, not
  comparative religion across the full scatter; celestial-and-ambient
  phenomena only — no earth gods from tectonic unrest, no sea gods from
  tides, both explicitly deferred; **static theology** — the pantheon is
  computed once at genesis and never revises as the sky, society, or a
  religious history would later change (that history is the Year-2 event
  ledger).

Laboratory: [Study 004, the Census of Faiths](../laboratory/study-004.md)
(the goblin-flagship baseline); [Study 007, the Census of
Eyes](../laboratory/study-007.md) (the two-pantheon comparison); [Study
008, the Census of Tongues](../laboratory/study-008.md) (the epithet's own
honorific and collision calibrations). Chronicle: [Campaign 5, The
Gods](../chronicle/campaign-5.md); [Campaign 15, The
Eyes](../chronicle/15-the-eyes.md); [Campaign 16, The
Tongues](../chronicle/16-the-tongues.md).

**The tier ladder ahead:** terrain and climate as phenomena sources (earth
gods from unrest, sea gods from tides); comparative religion and diffusion
across the full settlement scatter, not just each species' flagship;
per-caste patron deities; and a religious history that revises instead of
freezing at genesis, once the event ledger exists.

The distributional-twin control that this pantheon-building was ultimately
staked on — a third species carrying the goblins' exact perception vector,
scoring at chance under the blind-attribution rule and thereby proving the
rule reads structure rather than an accident of generation order — was
defined by Campaign 15 and **run** in The Meeting: 500 of 500 twin pairs
indistinguishable, the year's exit criterion cashed (see
[Study 009](../laboratory/study-009.md)).
