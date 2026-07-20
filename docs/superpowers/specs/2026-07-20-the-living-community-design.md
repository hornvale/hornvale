# The Living Community — Campaign 1: The Community & its History

**Status:** design (G3 review)
**Program:** The Living Community engine (campaign 1 of ~5)
**Spine:** history-first placement — the present world is the last frame of a
derived deep history.

---

## 1. The payoff

A world that lived through a couple thousand years of *actual* history. You walk
into a grassy clearing dotted with tree stumps and find a ragged goblin doll —
because this was Grubnest, a goblin village founded in the year 340 and burned in
1980 by a people who came from the coast. A day's travel away, a stone temple lies
under the roots of a later castle, under a still-later farmstead, because that hill
was the best land for miles and everyone who passed built on it. Near-identical
peoples — the four goblinoids that The Demesne could not separate on any spatial
axis — hold **distinct territories**, not because their niches differ but because
history *put them there*.

This campaign delivers that world, and the measured peoples-diversity fix comes
with it: because history is now the placer, the census diversity numbers move as a
direct consequence, not as a bolted-on correction.

## 2. Context — the program, and what The Sounding proved

The living-community engine is a **program**, decomposed like the Weather Program:

1. **The Community & its History** — *this campaign*. History-first placement:
   living settlements + standing ruins + territories, grown as the present frame of
   a derived deep history; the draft placer retired; the capacity field kept as
   substrate.
2. **The Connection Graph** (MAP-61) — routes/portals; diaspora and conflict follow
   a real transport topology instead of raw adjacency.
3. **Conflict as Criticality** (SOC-criticality) — weather-shock-driven raids that
   *emerge* (no trigger threshold); the power-law conflict-size distribution becomes
   a realism gate.
4. **The Living Present** (UNI-30) — the timeline as a lazily-derivable field; "now"
   as the intervention frontier.
5. **Personas & Institutions** (SOC-11 + projection) — role-handles flesh into named
   individuals; courts, guilds, armies.

**The Sounding** (shipped, `windows/chronicle`) was a *feasibility benchmark*, not
the engine. It proved: the macro-history bake is linear in communities × epochs;
inter-community coupling is tractable *only with a node→community index* (naive
scan ≈ Z²·¹, index ≈ Z¹·², shown byte-identically side by side); reads are sub-µs.
It also produced the tool this campaign depends on — the **workload census + a
sample-size floor** — after a review caught the first attempt reporting noise. That
lesson is load-bearing here (§9).

## 3. The spine — history-first placement

Genesis no longer *places* settlements. It **grows** them:

1. Seed an ancient world — a handful of proto-communities (per people) at founding
   sites drawn from the early-era capacity field.
2. Run a coarse forward history over the millennia. Each epoch, each community
   resolves against the **era's** carrying capacity: grow under slack, found a
   daughter into vacant favorable land, migrate toward refugia when its cell turns
   hostile, raid a neighbour under sustained pressure (displacing them), flee when
   raided/starved, collapse when it cannot.
3. The **present frame is the last epoch**: a living settlement is a community still
   alive at *t = now*; a ruin is one that died; the clearing is one that died at
   *t = now − 20 years*.

Territories and stratigraphy are not features — they are *consequences*. Favorable
sites are re-occupied across the history (ruin → resettle → ruin), stacking layers;
the capacity field therefore *predicts stratigraphy depth*, and a player learns to
read the land by its ruins.

## 4. Architecture (constitutional layering)

- **`domains/history`** (new; kernel-only) — owns the predicates (§5) and the
  **pure, local flesh derivations**: `role-handle → persona`, `(people, cause, age)
  → residue objects`, `era → tech horizon`, `occupation-record → structures`. Every
  derivation is a total function of its committed inputs + the seed; **no global
  replay**.
- **`windows/worldgen`** (composition root) — the **bake** lives here, because it
  must read terrain, paleoclimate, demography, and the capacity field, and a domain
  may depend only on the kernel. The bake emits the skeleton facts — including the
  `is-settlement` / `population` facts the settlement placer used to emit. The
  settlement domain keeps its predicates; **history becomes their sole emitter**
  (the keystone, §7).
- **`windows/*` legibility surface** — a read-only projection renders a site's
  stratigraphy and a sample of derived flesh (§10).
- The Sounding's `windows/chronicle` is a benchmark and stays separate; it is not
  imported by the engine and is retired in a later campaign.

## 5. The data model

### 5.1 The occupation record — the committed fact vector

One layer, at one site, for one span. **~12 facts, committed to the ledger** as
provenanced facts on an occupation entity. Everything else is derived flesh.

| # | Fact | Notes |
|---|------|-------|
| 1 | people / culture | `KindId` (goblin, drow, …) |
| 2 | community + lineage | the named entity; its descent |
| 3 | site | the cell; terrain/biome/capacity read from the world |
| 4 | founded (year) | |
| 5 | ended (year) | tenure = ended − founded |
| 6 | peak population | residue scale |
| 7 | tech horizon | sophistication *at that time* (era-driven, §5.3) |
| 8 | function | agrarian / mine / trade / cult / fort |
| 9 | cultural markers | deity + tongue snapshot (refs religion/language) |
| 10 | cause-of-end | famine / burned / plague / fled / migrated |
| 11 | **ended-by** ★ | which people/community did it — or "nature" (global) |
| 12 | **founded-from** ★ | the site/community the founders fled (global) |

★ The two starred facts are threads to *other* records across the whole map — the
reason the skeleton is committed rather than replayed, and the reason a pile of
ruins becomes a story. An optional 13th, `notability` (backwater ↔ seat of power),
gates whether residue is a doll or a reliquary.

### 5.2 The site and its stratigraphy

A **site** carries an ordered stack of occupation records (oldest → newest). The
present settlement at a site (if any) is its live top layer; the ruins are the dead
layers beneath. Stratigraphy depth is emergent, correlated with capacity.

### 5.3 Tech horizon

Coarse: a monotone function of world-age (the era) plus a per-people advancement
rate, clamped to a small ordinal ladder (e.g. neolithic → bronze → iron → …). It is
*committed* per occupation because a people's trajectory is globally dependent
(contact, displacement); it is not re-derivable from the ruin alone.

### 5.4 The flesh (derived, never committed)

Structures, layout, individual artifacts, the doll, inscriptions, named personas,
grave counts — all pure, local functions of the occupation record + site + seed.
This is "models author, dice roll": the ~12 facts are the authored skeleton, the
seed is the dice.

## 6. Persistence contract

- **Commit the whole history skeleton** — every occupation record and its dated
  life-events — to the ledger. Compact: order 10⁴–10⁵ facts for 2000 years (cf. the
  Sounding's 210k). The deep past is therefore **first-class and queryable** through
  the Concordance SPO/PSO index ("every ruin of the Redfang people, oldest first" is
  a query, not a replay).
- **The present is a query**, not a separate tier: the live communities are the
  occupations with no `ended` year.
- **Derive the flesh on demand**, locally, from committed facts + seed.
- This is a **genesis epoch**: settlement/peoples placement now originates in
  history, so every world's ledger changes. New save-format labels (stream-derivation
  labels for the bake draws) are declared in `domains/history::streams` and published
  into the stream manifest.

## 7. Keystone & determinism

- **Sole provider.** History is the *only* source of settlement and ruin facts. Two
  placers ⇒ two conflicting present maps ⇒ incoherent world. Retiring the draft
  placer is structural, not cosmetic.
- **Provenance.** Every emitted artifact (settlement, ruin, territory) carries a
  pointer to the skeleton event that produced it — the coherence check *and* the
  legibility surface.
- **Byte-identity.** Same seed + same pins → byte-identical history, present, and
  ruins. The bake runs at full precision; quantize only at the emit boundary.
- **Lorenz-safe.** The flesh is a pure function re-derived from the *lossless* seed +
  committed facts, never from quantized checkpoints. The bake is a deterministic
  replay, not a chaotic integrator seeded from ledger floats.

## 8. The dynamics — displacement without a floor

The bake runs over a capacity field that **varies per era** via the existing
**paleoclimate** domain (`EraClimate` habitability + `refugia`). Glacial cycles
shift habitability; peoples retreat to refugia and re-expand; overshoot on a moving
landscape drives sustained migration and displacement across the millennia — with
**no raid floor**. Conflict is the *simple* pressure-overshoot raid→flee→refound
loop; the power-law SOC-criticality dynamics are deferred to campaign 3. Refugia
prevent mass extinction; the census gate (§9) prevents inert quiescence.

## 9. Measurement, falsification, and non-goals

**The Sounding's discipline is mandatory here.** The bake must *demonstrably*
produce displacement, not be assumed to.

- **Workload census** — count grew / founded / migrated / raided / fled / collapsed /
  resettled, and report them.
- **Preregistered gates (frozen before the readout):**
  - *Displacement fired at volume* — a floor on `fled`/`resettled` over the history;
    a run below it ABORTS with a specific message (the Sounding's floor, inverted
    into a falsification metric — proof the phenomenon fired, not a forcing function).
  - *Territories separated* — the four goblinoids occupy measurably distinct regions
    (a spatial-overlap metric below a preregistered threshold); this is the
    peoples-diversity fix, measured.
  - *Stratigraphy emerged* — a non-trivial fraction of sites carry ≥ 2 layers, and
    layer depth correlates with capacity.
- **Non-goals (§9, read these before assuming scope):** NOT the connection graph
  (MAP-61 — campaign 2; this slice uses spatial adjacency). NOT emergent
  conflict/power-law raids (SOC-criticality — campaign 3; this slice uses the simple
  pressure loop). NOT the living present / player intervention (UNI-30 — campaign 4).
  NOT personas or institutions (SOC-11 — campaign 5; role-handles are minted and
  flesh is derivable, but no persona is materialized). NOT the connection-graph
  diaspora topology; displacement here is to adjacent/refugial cells.

## 10. Retiring the placer, and the legibility surface

- **Deleted:** the one-shot settlement placement step.
- **Survives:** the MAP-7 carrying-capacity field, demoted from *placer* to
  *substrate* — the fitness landscape history plays out across.
- **Quality gate (fidelity carve-out):** the emergent present settlement map must not
  regress key settlement metrics (count, size distribution, spatial spread) versus the
  draft, measured against a preregistered tolerance. If it does, we **stop and
  reassess** rather than ship a regressed walkable world to gain history.
- **Legibility (the compass's fix-legibility soul-call):** a read-only CLI/almanac
  surface renders a site's stratigraphy — each layer's occupation record in prose —
  plus a sample of derived flesh (the doll, the structures). Seeing the goblin doll is
  the acceptance test for the whole campaign.

## 11. G3 flagged items (carve-outs)

1. **Genesis epoch + census regen (lefford).** Placement now originates in history;
   every ledger changes; a census regen on the canonical Linux box is required at
   merge (decision 0063). Save-format: new stream-derivation labels; the epoch is
   deliberate.
2. **Retiring the draft placer** is a fidelity tradeoff, gated by §10's
   no-regression quality check.
3. **Timescale** (default ~2000 years; epoch granularity aligned to a coarse
   decade–century step) is a fidelity/cost knob — proposed, adjustable.

## 12. Success criteria / Definition of Done

- History-first placement is the sole provider of settlement + ruin facts;
  byte-identical across runs; the draft placer is gone and the capacity field remains
  as substrate.
- The occupation skeleton is committed and queryable; the flesh derives locally and
  deterministically.
- All three preregistered gates pass on the base world; the quality gate holds.
- The legibility surface renders a site's stratigraphy and its flesh; the goblin-doll
  acceptance test passes.
- DoD per CLAUDE.md: chronicle entry, retrospective, book freshness sweep, registry
  flips (SOC-10 → elaborated/shipped, SOC-criticality repoint, MAP-61 note),
  Confidence-Gradient re-score if a bet moved, census regen on lefford, keystone
  refreeze, full gate + artifact drift.
