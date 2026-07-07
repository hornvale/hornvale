# Campaign 5: The Gods

**July 2026 · 7 commits · outcome: complete, merged — Campaign 5 (The Gods)
closes with religion at tier 1, and closes Year 1**

## What was attempted

Every domain but one had already deepened past its tier-0 shape: astronomy
drew a real sky, climate turned that sky into bands and biomes, terrain grew
a tectonic globe, and settlement and culture placed a flagship whose
subsistence and social structure emerged from the land under it. Religion
alone still minted one belief from the single top phenomenon, exactly as it
had since Campaign 1b. Campaign 5 gives that flagship a **pantheon** —
derived from the phenomena its people observe, shaped by the society they
have already grown — and gives the world a general way to account for any
of its own facts (`why <entity>`). The deliverable was always going to be
Year 1's own exit demo: the same land and the same society, under a
different sky, producing a legibly different religion.

## What landed

**A pantheon, not a belief (Campaign 5a).** `domains/religion` becomes
tier 1: for every phenomenon a community observes at or above a 0.25
salience floor — with no upper cap — genesis mints a deity, using the same
eternal-vs-cyclic templates tier 0 trusted since Campaign 1b. The interface
grows by exactly one input, a bare `SocietySummary { strata, has_priesthood
}` mapped at the composition root from the flagship's already-committed
castes (Campaign 4b); religion still imports no domain and still learns
nothing about which system produced a phenomenon. Two facts about that
society decide the pantheon's shape: a structure of four or more castes
crowns a ranked pantheon (`high-god`, a functional flag on the top-salience
deity); a `shaman` caste tends an organized cult (`cult-form`) instead of a
folk one held by the whole community. The almanac's "The Gods" section
renders the whole structure, and the exit-demo gallery pair — [The Gods of
Seed 42](../gallery/the-gods-seed-42.md) — quotes both worlds' pantheons
verbatim: the spinning sky crowns **the Wheel-Turner**, a cyclic deity
mourned in absence and feasted on return; the tidally locked twin crowns
**the Still Crown**, an eternal sun that has never departed, with no
seasonal deity at all, because a locked world has no seasons to
mythologize. Same land, same society, only the sky moved.

**Provenance by replay (Campaign 5b).** A new kernel-only window,
`windows/historiography`, adds `recount(world, entity) -> Option<String>`:
a domain-agnostic replay of any entity's committed facts, their provenance
strings, and the registry's predicate docs into a derivation sentence. It
interprets no domain-specific predicate, so a new domain's facts are
recountable the day they are committed. The REPL's `why` verb — previously
wired straight to religion's own belief lookup — now calls `recount` for
*any* entity id, subsuming religion's tier-0 `why` outright (removed
outright from `domains/religion`, fifteen lines gone) without changing what
a player already knew to type. This is historiography **tier 0**: a flat,
single-entity replay, deliberately not a causal chain across entities — the
seam the Year-2 event ledger and fields-of-history will deepen, not a
preview of them.

**The Census of Faiths, the fourth and fifth calibrations.** Four new
metrics — pantheon size, cult form, pantheon verticality, and head-deity
periodicity — join the Lab's unified registry (32 metrics now, up from 28).
Two calibrations are built in and checked on every CI build over the
500-seed `census-lands-drift` sample: verticality is ranked exactly when
the flagship's structure reaches four or more castes, and the head deity's
tenet is eternal exactly when the world is tidally locked. Run once at
author time over 10,000 worlds (`studies/census-of-faiths.study.json`), the
census confirms both hold **without a single exception** — 3,755 worlds
(37.6%) ranked, every one stratified at size 4 or 5; 457 worlds (4.6%)
crowned by an eternal head deity, and all 457 are tidally locked, the
identical count Study 002 already reported for tidal lock in this same
seed range, reached here by an entirely unrelated metric. The genuinely
unknown numbers: mean pantheon size 3.05 deities (median 3, ranging 1 to
7, with a five-world esoterica tail at size 7); 62.2% of pantheons organized
against 37.8% folk; and a cross-tabulation nobody could have predicted from
the two thresholds alone — 24.7% of the census runs an organized priesthood
over a *flat* pantheon, a shaman caste without the four strata verticality
needs, while not one ranked pantheon in the sample is ever folk. Full
analysis: [Study 004, the Census of Faiths](../laboratory/study-004.md).

**The book, promoted to match.** The religion chapter carries its own
tier-1 model card alongside the tier-0 section it grew from; the cascade
overview names all five domains at tier 1 for the first time and frames the
astronomy-to-theology cascade explicitly; the Laboratory and Gallery each
gain their capstone chapter.

## What was learned

- **A trace-protocol domain can absorb a whole new input without breaking
  its own ignorance.** Religion's tier-1 genesis reads a `SocietySummary`
  it has never seen the shape of culture behind — the same discipline that
  let it mythologize Campaign 2's moons without an edit now lets it
  mythologize Campaign 4's castes without importing `hornvale-culture`.
  Depth and blindness are not in tension; the composition root is what
  makes both possible at once.
- **A general provenance seam was cheaper built once than built per
  domain.** `recount` interprets no predicate specially — it reads the
  registry's docs and the ledger's provenance strings generically — so it
  answered `why` for a belief and a settlement on day one, and will answer
  it for the next domain's facts without modification. Building the
  narrow, well-understood tier 0 of a capability that could have sprawled
  (natural-language variation, causal chaining) kept the Year-2 event
  ledger a genuinely separate, later decision rather than something smuggled
  in early.
- **Independent thresholds produce combinations nobody authored.** Cult
  form and pantheon verticality are read from the same society summary but
  answer different questions, and the census is what turned "these two
  numbers probably move together" into a checked fact: they mostly do, but
  24.7% of worlds prove they are genuinely separate axes. This is the same
  lesson Campaign 4b's flagship-subsistence census taught with placement
  suitability — an exact function still hides findings nobody wrote down on
  purpose until an instrument counts them.
- **The five-calibration family is now a standing verification pattern, not
  a one-off trick.** belief⇔lock (Campaign 2b/L0), band-count⇔rotation
  (Campaign 3c), subsistence⇔biome (Campaign 4b), and now
  verticality⇔stratification and head-god⇔lock (Campaign 5) — five exact,
  row-checked calibrations, each a known function of a quantity the census
  already had, each asserted on every CI build against the same 500-seed
  sample. Every tier-1 domain so far has shipped with one.

## Deferred, deliberately (spec §13)

Terrain and climate stay outside religion's phenomena sources — no earth
gods from tectonic unrest, no sea gods from tidal fields — a later
religion/ecology tier's to open. Only the flagship raises a pantheon;
comparative religion and diffusion across the rest of the scatter wait for
a later social tier. Per-caste patron deities were considered and set aside
in favor of the coarser verticality-plus-priesthood coupling. Theology is
**static** — computed once at genesis, never revised — because the event
ledger, retrospective confabulation, and fields of history that would let
it *have* a history are Year-2 work; `why`/`recount`'s flat single-entity
replay is the seam they will deepen, not a preview of them. The population
stays single-species (goblin); the comparative multi-species theology the
vision book raises waits on Year 2's species-psychology substrate.
`recount`'s rendering is fixed, with no natural-language variation.

## Year 1 retrospective

Five campaigns, one thesis, proven end to end. Campaign 1 built the kernel
and the whole cascade at tier 0 simultaneously — a sun that never sets, air
that never moves, one hand-placed vale, one goblin village, one belief —
so that every later campaign would deepen something that already ran
rather than bolt on something new. Campaign 2 gave astronomy a real star
system: orbits, moons, obliquity, rotation, tidal locking, all drawn from a
seed and none of it authored. Campaign 3 spent that sky twice — a tectonic
globe from terrain, then a climate and biome map derived from the globe and
the sky together, the first domain to prove that coarse constrains fine
across two crates that import nothing from each other. Campaign 4 placed a
scatter of settlements by real suitability fields and grew one flagship's
subsistence and caste structure out of the environment it stood in, not a
fixed template. Campaign 5 gave that society gods, shaped by the sky above
it and the strata beneath it, and a way for the world to explain any of its
own facts.

**The enrichment thesis, cashed all the way to theology.** The claim the
plan made in its first month — that upstream richness produces legible
downstream difference, and that this difference would be visible without
any domain knowing about any other — is no longer a bet. The seed-42
spinning-vs-locked pair, threaded through every gallery page in this book,
demonstrates it at every layer: the same elevation field, a different biome
map (a substellar desert against latitude bands); the same biome map's
consequences, a different settlement count and a relocated flagship; the
same flagship cell, a different subsistence mode and caste ladder; and now
the same caste ladder, a different pantheon. One rotation bit, set at
genesis, propagates through five independently-testable domain crates to a
different god's name at the top of an almanac page — astronomy → climate →
biome → subsistence → social structure → theology, exactly the chain
spec §7 promised, with no step skipped and no step aware of the ones before
it.

**Five calibrations, one family.** Every tier-1 domain shipped a fact about
its own generator that the Lab could check row by row against ten thousand
worlds and assert on every CI build: belief kind against tidal lock,
circulation-band count against rotation period, subsistence mode against
biome and coast, and now pantheon verticality against social stratification
and head-deity periodicity against tidal lock. None of these are
hypotheses about the data — each is a fact about a pure function the code
already computes — and the discipline of building one alongside every
tier-1 deepening is what keeps four censuses' worth of "genuinely unknown"
numbers (habitable fraction, settlement count, structure-size mix, pantheon
size) trustworthy: an instrument that cannot lie about what it already
knows is the only kind worth asking about what it doesn't.

**What Year 2 might hold.** The deferred lists this book has been keeping
since Campaign 1b converge on a small number of standing seams, all
deliberately opened rather than closed: an **event ledger** and
**fields-of-history** to let theology, culture, and settlement *revise*
instead of freezing at genesis — the seam `recount`'s flat single-entity
replay was built to deepen, not preempt; **comparative religion and
diffusion** across a full settlement scatter, once more than the flagship
carries a society; a **species-psychology substrate** to let a second
species exist as more than a name, and with it the paternalism/hegemony
comparative-religion questions the vision book raised back at Campaign 4b;
and **language**, never built at any tier so far, the substrate every
epithet and tenet in this book has been drawn from a fixed pool rather than
generated by. None of these are scheduled — Year 2 planning is separate
work — but each is a real extension of an interface Year 1 already proved
out, not a new invention.

## Artifacts

[The Gods of Seed 42](../gallery/the-gods-seed-42.md) — the Year-1 capstone
exit-demo pair, quoted verbatim from the committed spinning and
tidally-locked almanacs. [Study 004: The Census of Faiths](../laboratory/study-004.md)
— the verticality⇔stratification and head-deity⇔lock calibrations, and the
pantheon-size/cult-form/verticality distributions, over 10,000 worlds. The
religion chapter (`book/src/domains/religion.md`) carries its tier-1 model
card; the cascade overview (`book/src/domains/overview.md`) names all five
domains at tier 1. The concept registry gains `high-god` and `cult-form`;
the REPL's `why <id>` answers for any entity, not just a belief.
