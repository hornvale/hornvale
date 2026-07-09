# Year 3 Metaplan: The World Has a Past — Design

**Date:** 2026-07-09
**Status:** Approved (brainstorming session)
**Parent spec:** `2026-07-05-hornvale-longterm-plan-design.md` (Constitution §2 governs)
**Provenance:** Year 2 closed at `9bc0b18` with the exit criterion met — one
world carrying two species that differ only in their authored parameter
vectors produced legibly different languages and religions from the same sky,
verified by a 500/500 null control and a blind-attribution metric pinned
honest at 0.875. The [Year-2 retrospective](../../retrospectives/year-2.md)
records the spine's success, the two standing process rules this metaplan
inherits, and the deepest remaining gap. This document records the Year-3
choice and sequences the campaigns behind it; each campaign after Campaign 0
opens with its own spec, and this metaplan is what they answer to.

---

## 1. The spine, and why this one

**Chosen spine: the world has a past** — physical deep time first
(paleoclimate), then social deep time (population as a field, settlement
history as an event ledger), then history made queryable (fields of history).
The world is fully realized at genesis but has no past; Year 3 gives it one.

This was the long-term-plan spec's original Year-2 order, deferred once when
Year 2 chose the culture spine; it is taken up now. Alternatives considered
and set aside (recorded so the choice isn't relitigated without new
information):

- **Known, not just true** (the epistemic layer, MAP-2): the highest ceiling
  on the map — player and scholar knowledge as a provenance-tagged second
  ledger, source criticism as a mechanic. It was deferred in Year 2 as
  "riskiest on today's thin content — not yet enough world for scholars to be
  wrong about," and that judgment is unchanged: reconstruction needs a real
  past to mis-reconstruct. Building the past is exactly what makes the
  epistemic layer buildable, so it becomes the **presumptive Year-4 spine**,
  and Year 3 is sequenced deliberately to leave it its raw material.
- **Populous & believable** (verisimilitude hardening, society across the full
  scatter): foundation work with no demo of its own. Its highest-value slice —
  giving settlement history to more than the flagship — is absorbed into
  Campaign 2 below, where population-as-a-field lands it natively; the rest is
  deferred.

The chosen spine attacks the deepest named gap (static-at-genesis) in
dependency order, and its verification story sits at the **top of the
checkability gradient**: paleoclimate is a pure function of orbital elements
(formula-checked), and the whole year's exit criterion is a divergence a Lab
metric can attribute row by row. It leans on the Lab muscle and the calibration
discipline the two prior years built.

## 2. Year-3 exit criterion

Year 1 varied the world and held the observer. Year 2 inverted it — varied the
observer, held the sky. Year 3 varies **time**:

> **Generate two worlds identical at genesis but differing only in a deep-time
> pin (their orbital forcing), and get a legibly different present — different
> glacial strata, fossil shorelines, refugia, ghost-town lineages, and
> history-derived myths — every divergence recountable through the event
> ledger to its cause in the past.**

Falsifiability teeth: a **blind-attribution metric** — a code metric, not
taste, preregistered per ADR 0016 — must attribute a history-stripped present
(a set of strata, a ghost-town roster, a myth) to the correct deep-time path
above a preregistered accuracy across 10,000 worlds. If a world with a
turbulent glacial history reads the same in the present as one with a placid
one — same shorelines, same settlement lineages, different only in a number no
one can see — the thesis fails, and is allowed to fail.

One confound must be controlled, as in Year 2. Deep-time draws come from their
own labeled streams, so the claim is statistical and needs a **null control**,
preregistered alongside the main studies: a world with **zero orbital forcing**
(circular orbit, no obliquity drift) has no Milankovitch cycling, hence no
glacial strata, no fossil shorelines from sea-level change, and no
history-driven abandonment — its present must be distributionally
indistinguishable from its own genesis snapshot, and its past must score at
chance on blind attribution (there is nothing real to recover). This
establishes that measured divergence is attributable to the deep-time path,
not to stream noise — and it is the seam the Year-4 epistemic layer will turn
into a reconstruction score.

## 3. Campaign sequence

Five campaigns, strict dependency order; every campaign ends observable and is
abortable at a week boundary.

| #   | Campaign            | Delivers                                                    | Observable ending                                          |
| --- | ------------------- | ---------------------------------------------------------- | ---------------------------------------------------------- |
| 0   | **Firm Ground II**  | time-varying orbital elements + the observer's place       | an almanac observed from a vantage point; a sky that drifts |
| 1   | **Deep Time**       | `domains/paleoclimate` — Milankovitch strata over the globe | fossil shorelines and refugia in one map                   |
| 2   | **The Ledger of Ages** | population as a field; era-ticked settlement history     | ghost towns and mother-city lineages in one almanac        |
| 3   | **Fields of History** | history as queryable fields; `why` deepens to a causal chain | `why <ghost-town>` recounts the ice age that emptied it   |
| 4   | **The Close**       | preregistered divergence studies, capstone, book close     | the deep-time capstone artifact + book close               |

Sequencing rationale: physical history precedes social history (the frontier
sequences paleoclimate ahead of population, because carrying capacity reads the
climate that glaciation moves); the event ledger precedes fields-of-history
(you cannot query a history you have not yet recorded); and the whole spine
precedes the epistemic layer (Year 4), which reconstructs a past that must
first exist. Nothing is built static-at-genesis and retrofitted with a past —
the same substrate-first discipline that carried Year 2, applied to time.

## 4. Campaign 0: Firm Ground II

Firm Ground II plays the role Year 2's Campaign 0 played — deliver the
substrate the rest of the year reads, and re-baseline exactly once so the churn
happens up front — but unlike that surgical hardening campaign it introduces
genuinely new capability, so **it opens with its own spec** rather than
proceeding straight to a plan. Two deliverables, each retiring a named deficit
the deep-time spine depends on:

1. **The sky acquires a past (SKY-1/2/4/21).** Today the star, obliquity, and
   orbital elements are drawn once at genesis and fixed for all `WorldTime`
   (`domains/astronomy`); only calendar *phase* changes. Paleoclimate has
   "almost nothing to read." This campaign makes the orbital elements
   **functions of `WorldTime`**: a drawn eccentricity (SKY-2) giving a
   tilt-independent seasonal driver, per-body phase offsets (SKY-4) retiring
   the genesis grand-alignment, and slow **obliquity and precession drift**
   (SKY-21) — the Milankovitch triad. Each new drawn quantity is permanently
   labeled (`astronomy/orbit/eccentricity`, …) and the drift laws are declared
   derived/approximated on the astronomy model card. This is where the "read
   the engine before finalizing the plan" rule first bites: the drift must
   consume the same stream draws on pinned and unpinned paths
   (pin-isolation-tested), and the existing genesis-time values become the
   `t = 0` slice of the new time-varying functions, byte-identical there.
2. **The observer acquires a place (SEQ-4/SEQ-5).** Phenomena are delivered
   position-blind: `ObserverContext.place` is an `EntityId` with no location on
   the globe, so the sky cannot be culled by horizon and generated worlds
   observe through a tier-0 climate anchored at `places[0]` and genesis day 0.
   This campaign extends `ObserverContext` with a real position (latitude and
   longitude) — a kernel seam, no producing domain edited — so the almanac is
   written from a vantage point: one hemisphere of sky, one latitude's daylight
   (SKY-8 follows cheaply). This is the seam the Year-4 epistemic layer's
   "almanac from a vantage point" needs, delivered early because deep-time
   strata are inherently *place*-bound (a fossil shoreline is somewhere).
3. **Re-baseline exactly once.** A drifting sky and a placed observer change
   almanac and census outputs; all four censuses re-run, the drift study
   regenerated, calibration rows re-asserted, every committed artifact
   refreshed once. Every later Year-3 study inherits these baselines.

Explicitly out of Firm Ground II: the celestial *bodies* deep time does not
need (wandering planets, comets, constellations — SKY-9/10/12, a later
astronomy campaign's); binary hosts (SKY-16); anything touching the paleoclimate
model itself (Campaign 1).

## 5. Campaign 1: Deep Time

**A new domain crate, `domains/paleoclimate`**, depending on `hornvale-kernel`
and nothing else. This crate boundary is a firm decision of this metaplan, not
an open question — reading `domains/climate/src/provider.rs` at design time
settled it: `GeneratedClimate` already exposes everything paleoclimate reads
(`mean_temperature_at → f64`, `habitability() → &CellMap<bool>`) as bare kernel
types, so a separate crate needs no duplication and no climate internals.
Opens with its own spec; the binding decisions:

- **Paleoclimate is climate re-run over deep time, plus strata extraction.**
  Glacial cycling is the climate temperature/habitability field evaluated at
  many past eras under time-varying orbital forcing (Campaign 0's deliverable).
  The new work is not re-deriving climate but extracting the **durable marks** a
  time-series of climate states leaves: a fossil shoreline is the maximum
  sea-transgression cell across eras, a refugium is a cell habitable through the
  cold extreme, a glacial stratum is the ice-extent envelope. These are
  paleoclimate's outputs and its committed facts; present-climate commits none.
- **The era-loop lives at the composition root** (`windows/worldgen`), not
  inside either domain: worldgen computes each era's orbital forcing from
  astronomy, asks paleoclimate for that era's sea level, calls
  `climate::generate`, and hands the snapshot fields back to paleoclimate for
  strata extraction. This orchestration seam is introduced here and **reused by
  Campaign 2's era-ticked population pass** — which is the structural reason the
  loop belongs at worldgen rather than buried in a climate tier.
- **The one open modeling question left to C1's spec** (a decision *inside*
  paleoclimate, not a crate-boundary decision) is the ice↔sea-level↔climate
  feedback: Milankovitch forcing → ice volume → sea level, resolved as a simple
  lagged/threshold one-pass model, not a coupled GCM. C1's spec settles the
  forcing formula after reading `temperature.rs`/`habitability.rs` — the
  read-the-engine rule again.
- **Consumption without dependency:** strata and shorelines are committed as
  facts (value-kind enforced per ADR 0010) so `why` can recount them, and
  downstream consumers (settlement, later religion) read paleoclimate through a
  composition-root summary — no domain-to-domain edge, the `SocietySummary`
  pattern one layer along.

**Observable ending:** one map showing a fossil shoreline and a refugium; a
world whose "the frost retreated" is a committed, queryable fact rather than a
phrase.

## 6. Campaign 2: The Ledger of Ages

Where social history lands. Opens with its own spec; the binding decisions:

- **Population as a field, settlements as its condensations** (MAP-7): a
  carrying-capacity field derived from climate, terrain, and paleoclimate's
  strata; a population-density field relaxing toward it; discrete settlements as
  condensations of that field — coarse-constrains-fine, the field as the tier-0
  prior any finer dynamics integrate to. This natively delivers the
  populous-and-believable slice Year 2 deferred: history reaches the whole
  scatter, not only the flagship.
- **History as an era-ticked pass at genesis, appended to the ledger as
  facts.** Founding, growth, fission, and abandonment become committed facts
  with a `WorldTime` — so ghost towns, mother-city lineages, and migration waves
  exist as narrative substrate the moment historiography and religion look. The
  era-loop is the one Campaign 1 placed at worldgen, now carrying settlement
  dynamics as well as climate snapshots.
- **The species-psychology substrate slots in** as per-species dispersal
  appetite and tolerance bands feeding the same dynamics — Year 2's vectors
  earning a second use, yielding niche partitioning and contact zones without a
  new vector.
- **Determinism is the sharpest constraint here** (see §9): an era-ticked
  forward pass must be byte-identical per seed+pin, so the era-tick order and
  the ledger append order become save-format contracts under ADR 0006, and the
  pin-isolation battery extends to every new deep-time pin.

**Observable ending:** one almanac carrying ghost towns and mother-city
lineages, each with a founding era and — for the abandoned — an abandonment and
its cause.

## 7. Campaign 3: Fields of History

Deliberately light on new draws; its job is to make the past **queryable** and
to cash the seam Campaign 5 built `why` to become.

- **History as typed fields over past time** (the Field abstraction extended
  from the statistical prior into the temporal one): population, ice extent, and
  settlement occupancy as functions over (space × past `WorldTime`), so a
  consumer can ask "what was here, and when" without replaying genesis.
- **`why` deepens from a flat single-entity replay to a causal chain across
  eras.** Historiography tier 0 recounts one entity's committed facts; tier 1
  follows the provenance across the event ledger — a ghost town's abandonment to
  the glacial advance that caused it, a mother city to its daughter foundations.
  This is the deepening `recount` was explicitly built to receive, not a
  preview smuggled in early.
- **The Lab gains time as a study axis** (TOOL-3): metrics sampled along
  `WorldTime` within one world — population trajectories, founding and
  abandonment rates — the instrument the divergence studies need.

**Observable ending:** `why <ghost-town>` recounts the ice age that emptied it;
a Lab study plots one world's population trajectory across eras.

## 8. Campaign 4: The Close

Deliberately light on new machinery; its job is to cash the year's check as The
Meeting did for Year 2.

- **The preregistered comparative study suite** (all hypotheses stated before
  running, ADR 0016): strata divergence × orbital forcing; ghost-town-lineage
  divergence × glacial history; the **blind-attribution metric** at its
  preregistered accuracy across 10,000 worlds; and the **null control** from
  §2 (a zero-forcing world's present is indistinguishable from its genesis
  snapshot and scores at chance on blind attribution).
- **The capstone artifact:** the successor to The Meeting's two-peoples page —
  one genesis, two histories side by side: two glacial records, two sets of
  fossil shorelines and refugia, two settlement lineages with different ghost
  towns, myths that remember different pasts, every divergence recountable by
  `why` to a deep-time cause.
- **Book close:** Year-3 chronicle; a model card for `domains/paleoclimate` and
  the extended astronomy/historiography cards (each parameter declared derived
  vs. approximated vs. drawn vs. authored — the discipline that caught the
  `communal` falsehood); freshness sweep; concept-registry review; the Year-3
  retrospective and this metaplan's assessment.

## 9. Cross-cutting

**Constitutional compliance, by constraint:** one new domain crate, kernel-only
(ADR 0002); all cross-domain flow via composition-root summaries plus
registered facts (0003); no new dependencies — the Milankovitch and ice models
hand-rolled on std (0004); all registries ordered (0005); every new drawn
quantity permanently labeled, any regenerated stream under epoch suffix (0006);
seeds never retried (0007); bounded dimensionless parameters stay bare `f64`,
coherent physical quantities (orbital elements, ice volume, sea-level change)
get newtypes (0008); no ML at runtime — any LLM help authoring forcing tables
is offline, committed, drift-checked (0009); new predicates value-kind enforced
(0010); new measurements are Rust extractors (0011); config JSON (0012); DoD
includes the book (0013); all claims preregistered (0016).

**Determinism is sharpest this year, and most catastrophic if wrong.** Every
prior year's determinism surface was a genesis snapshot; Year 3 adds a
*forward-run history*, and a history that is not byte-identical per seed+pin
corrupts every world silently. The era-tick order and the ledger append order
join the save-format contracts (0006); the pin-isolation battery covers every
new deep-time pin (a deep-time pin must consume the same draws as the unpinned
path); the era-loop at the composition root is the single place this ordering
is defined, so it is auditable in one file.

**The two standing process rules from the Year-2 retrospective are plan-review
checklist items for every campaign this year:**
1. **Read the engine before finalizing the plan.** Deep time is the most
   mechanism-heavy spine yet; when a spec's central mechanism depends on an
   engine behavior (era-tick order, a draw order, the climate provider's field
   outputs, a placement rule), read and cite that code *during plan-writing*.
   This metaplan already applied it once — the `domains/paleoclimate` boundary
   was decided by reading the climate provider, not by reasoning from prose.
2. **Treat `main` as volatile.** Verify the worktree's real base with
   `git merge-base HEAD main` before dispatching tasks that lean on recent
   infrastructure, and re-check before merge.

**Testing spine:** per-campaign property batteries (same seed+pins →
byte-identical two-history almanac; pin isolation for every new pin); the
divergence and null-control calibrations asserted row-by-row in the drift
study; censuses re-baselined exactly once (Campaign 0) and then held for the
year.

**Risks, named:**

1. **The ontology trap** — the frontier calls it sharpest here of anywhere on
   the map: every stratum, every population field, every recorded event must
   change a settlement, a myth, or a life, or it is folk science ossified into
   false precision. Mitigated by the same discipline that saved the psychology
   substrate — nothing lands that no observable consumes.
2. **Determinism drift in a forward-run history** (the year's real technical
   risk) — mitigated by the ordering-as-save-format-contract rule and the
   single-file era-loop above.
3. **Paleoclimate inputs too thin** — the SKY-1 deficit that would leave
   glacial cycling deriving from nothing; retired by Campaign 0 before Campaign
   1 reads them.
4. **Scope creep toward the epistemic layer** — reconstruction and
   wrongness-scoring are Year-4 bait. The bright line: Year 3 *builds a true
   past*; Year 4 *makes it knowable and mis-knowable*. Fields-of-history exposes
   the true past; it does not model a scholar being wrong about it.

**Explicitly deferred** (decisions, not omissions): the epistemic second ledger
and reconstruction-scoring (the presumptive Year-4 spine, MAP-2); gene-culture
coevolution (the return edge that closes the loop, comes last per SEQ-3);
contact/exchange and the full deep-time social stack (MAP-9 and downstream);
deep language and sound change (MAP-3); the celestial bodies deep time does not
need (SKY-9/10/12/16). These campaigns drain existing registry rows (MAP-6,
MAP-7, SKY-1/2/4/8/21, SEQ-4/5, TOOL-3); per-campaign specs flip those rows to
`spec'd`/`shipped` as they land.

## 10. Process

Firm Ground II opens with its own spec (it introduces new capability, unlike
Year 2's plan-only Campaign 0), and Campaigns 1–4 each open with their own spec
(book chapter first, per book-driven development), answering to this metaplan;
binding decisions recorded above carry into those specs unchanged unless
superseded with new information. Standing structural rules apply throughout: any
campaign abortable at a week boundary; no domain leads another by more than ~2
tiers; nothing exists until observable. Definition of Done for every merged
campaign includes the book (a chronicle entry and a freshness sweep) and a
one-page retrospective (decision 0020); the year closes with a Year-3
retrospective mirroring this one's parent.
