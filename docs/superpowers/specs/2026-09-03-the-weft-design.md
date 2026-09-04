# The Weft — design

**Campaign:** The Weft · **Decision block:** 0686–0695 · **Ledger:**
[`2026-09-03-the-weft.md`](../ledgers/2026-09-03-the-weft.md)

The **warp** is the macro fields already running continuously through the world;
the **weft** is the fine detail that crosses them at facet resolution.

---

## 1. The gap, and what kind of thing it is

The Prospect measured the density of the inhabited surface for the first time:
**one enterable site per ~84,200 land facets**, across five seeds
(`windows/lab/tests/suite/site_density.rs`, H3). A facet at depth 13 is 1.126 km
on a side. Nathan's aim is **one thing worth finding per facet** (ledger #1 —
not per square mile, which is 2.04 facets and was the framing
`book/src/open-questions.md` published on 2026-09-03).

So the shortfall is ~**84,200×**, and it is not a tuning question. Placed
features are born on 40,962 level-6 vertices and addressed onto 402,653,184
walk facets (6 × 4¹³), so **at most one facet in 4,915 can hold a placed feature**
before a seed is built (one per `(vertex, SiteReason)`, and there are two
reasons). H2 predicted a per-facet cave
*percentage* and was falsified by ~1,070×, because a placed point process on a
41,000-point lattice cannot express one.

**§1.1 — The published figures are right; only the TARGET changed.** An
earlier draft of this section claimed `book/src/open-questions.md`'s "5.8% of
that ceiling" and "buys about 17×" did not reproduce. They reproduce exactly,
and the error was this spec's: it used a **one-kind** ceiling (1 in 9,830).
`SiteReason` has **two** variants, `Cave` and `Exotic`, and `site_facet_for`
places one facet per `(vertex, reason)` — so the ceiling is **1 in 4,915** land
facets, which gives 5.8%, 17.1× and the ~2,400× that
`windows/lab/tests/suite/site_density.rs` already states. Nothing in the book's
arithmetic is wrong.

What *is* now wrong there is the **target**, and only that. `site_density.rs`
reasons against "one per square mile" = **~0.489 sites per facet**, because a
facet is 1.126 km per side and a square mile is 2.04 facets. Nathan's aim is
**one per facet** (ledger #1). So the two target-dependent figures double and
the two target-independent ones do not move:

| quantity | published | under a 1-per-facet aim |
| --- | --- | --- |
| gap at the measurement | 41,200× | **84,200×** |
| gap at the placement ceiling | ~2,400× | **~4,915×** |
| today as a share of the ceiling | 5.8% | 5.8% (unchanged) |
| saturating the placed tier buys | 17.1× | 17.1× (unchanged) |

`site_density.rs` anticipated this exactly — "the earlier 84,200x / 4,900x
compared against a rate of 1" — and set those aside because the target was then
one per square mile. Nathan's aim restores them. **Quote `~1.19e-5` and
`~84,200` and no further digits:** the numerator is exact but the denominator is
an accept-rate estimate at ~0.78% pooled relative standard error, so the rate is
`1.19e-5 ± ~1%`.

Correcting the Gradient entry is therefore a **substitution of the target**, not
a re-derivation, and it is §10's DoD item.

## 2. The tier axis, stated correctly

**Placed** = generated at **worldgen time**, at the 41K-vertex resolution
worldgen can afford; present in the model, so it can affect and be affected by
the rest of the world. **Derived** = generated at **observation time**, at
walkable scale, around the observer — and unable to participate in
world-affecting events until derived. *Placed is Dwarf Fortress; derived is
Caves of Qud.*

This is a statement about **when a feature is generated**, never about what kind
of thing it is. Consequences the design relies on:

- **The same kind may exist at both tiers**, and caves do. A placed cave can be
  invaded and have its fall recorded; a derived hollow is somewhere a traveller
  shelters. That is not duplication — it is the reason to have both.
- A derived feature carries no history and nothing may happen *to* it, until and
  unless it is promoted. **This spec does not name the promotion (standing)
  axis**; decision 0669 deliberately left it unnamed and nothing here forces it.

Ledger #5 records the wrong criterion this replaces — a semantic one ("derived
if explained by present conditions") that sorted today's three kinds correctly
and would have generalised wrongly to every future one.

## 3. Scope — two halves

The walker survey (ledger #6) split the target:

| | already generated | walker perceives it |
| --- | --- | --- |
| ruins | yes, `is-ruin` ledger facts | **no — not at all** |
| placed exotic sites | yes, 103 at seed 42 | yes, in 5 descriptions |
| chambers | derived already | yes, minimally |
| springs, thickets, overhangs, erratics | **no** | no |

**Half A — the reader.** Give the placed tier its missing walker-facing
surface. `brief.rs` omits the ruin signature *on purpose*: "it does NOT carry
the fields no consumer reads yet… the campaign that first needs `cause` adds one
field, with no save-format consequence and no epoch." This campaign is that
campaign.

**Half B — the derived surface.** Build the tier for the kinds that do not
exist at all.

They share delivery: each ends as something the brief reports and the prose
renders. They are separable at a plan-stage boundary if the campaign must be cut
(ledger #7).

## 4. Half A — the reader

**4.1 The brief gains the ruin signature.** `cause`, `ended_by`, and the ages
already computed in `history_emit`. Additive to a type that is derived and never
serialized, so there is no save-format consequence and no epoch.

**4.2 Prose renders it.** Standing at or near a dead occupation says so. A ruin
is the strongest sign-legibility source in the world, because it is evidence
about a real past event — the one thing a derived feature structurally cannot
be (§2).

**4.3 Extent, not point.** A settlement's territory is a set of vertices, and a
ruin inherits that. **`Extent::Region` DOES NOT EXIST** — the enum has exactly
one variant, `Point`, and `site.rs` carries four separate doc corrections saying
so because four drafts asserted otherwise. An earlier draft of this section was
the fifth. **Decision rule, not a prediction:** if a ruin's territory is
recoverable from `OccupationRecord` alone, *add* the variant (an enum widening
the compiler enumerates — the one real match, `site_density.rs:237`, is
exhaustive with no `_` arm) and emit it; if it is not recoverable without new
worldgen state, emit `Point`, write the reason beside the existing corrections,
and file a registry row for the region case.

**4.4 Exotic-site variety is measured, not assumed fixed.** Seed 42's 103 placed
exotic sites carry five descriptions, 68 of them identical ("under a fungal
canopy", 66%; 74% mention a fungal canopy). **Decision rule:** if the descriptor
set is authored data, widening it is in scope; if variety is limited by a draw
that would move seed-42 goldens, it is **out** of scope for this campaign and
gets a registry row instead. Monotony is not purely a resolution problem, and
this half of it is not the derived surface's job to fix.

## 5. Half B — the derived surface

**5.1 Mechanism (ledger #2).** Two stages per kind, mirroring
`cave_process → presence_prob` one resolution down:

1. **Prevalence** `p_k(facet) ∈ [0,1]` — macro state (via `blend_at`) modulated
   by position-continuous `Fbm`.
2. **Occurrence** — a second, decorrelated position-continuous sample decides
   whether the facet carries one.

Noise is keyed on **position**, never on facet address. The Ford established the
consequence as a measurement: `channel-band-monotonicity` is "the
position-continuous-noise guard stated as a measurement", and its stated
falsification is that "address-hashed noise leaked into a band edge".

**5.2 Three parameters per kind, independent by construction.**

| parameter | controls | why separate |
| --- | --- | --- |
| **abundance** | absolute frequency over eligible facets | tunable "individually and severally" (#1 rider 4) |
| **correlation length** | how far you walk before the answer changes, **in facets** | texture vs. rhythm vs. landmark |
| **contextuality** | mixing ratio, macro state against free noise | 1 = wallpaper (macro-grounded texture), 0 = free noise — position-continuous but uncorrelated with any macro cause; the craft is between |

**Three states, not two, and an earlier draft of this spec collapsed them.**
Address-hashed noise is *spatially incoherent* — adjacent facets uncorrelated,
the thing §5.1 bans outright and The Ford's metric catches. Low contextuality is
something else: the field is still position-continuous and therefore smooth, it
is simply **uncorrelated with any macro cause**. High contextuality is the third
— the feature restates the macro field and adds nothing local. H2 forbids the
first; the erratic is deliberately the second; wallpaper is the third. An
earlier draft called the erratic "near-wallpaper", which named the wrong failure
and followed from §5.2's endpoints being written inverted.

**Nothing normalises across kinds.** Per-kind prevalences do not sum to 1. A
simplex constraint is exactly the structural cap #1 rider 3 forbids: it would
make raising enterable density necessarily lower something else. Every kind's
abundance is independently dialable to 1.0, so "most facets enterable, possibly
all" stays reachable.

**5.3 The field pack.** `blend_at` takes a `VertexMap<f64>`, but macro state is
exposed as per-vertex *accessors* (`drainage_at`, `crust_age_at`,
`boundary_distance_at`, `material_at`). Each scalar a kind reads must be
materialized once per world as a `VertexMap<f64>` and passed in — the shape
`predator_pressure_from(wc, terrain, report) -> VertexMap<f64>` already uses.

**Read the continuous causes, never the categorical label.** `Biome` is an
`enum`; a category cannot be bilinearly blended, which is *why* the biome
monotony defect exists — `blend_at` structurally cannot smooth a label.
`MaterialBuffer` is **not** categorical: five continuous `[0,1]` scalars
(`silica`, `grain`, `induration`, `carbonate`, `metamorphic_grade`), so rock is
blendable, and `carbonate` is the karst driver that physically governs springs
and caves. This rule is why derivation can beat monotony without touching the
biome system.

**5.4 The residency window (ledger #4).** Three layers: the function (pure,
recomputed at will), the residency window (materialized components near the
observer, evicted when it stops earning its keep), and the ledger (untouched).

- Backing: `Derived<K, V>` with `Validity::Pure`, keyed on
  `(seed, level, facet, kind)`. `derived.rs`: "World-derived is not a third
  class — it is `Pure` with the world's identity … folded into the key."
- Eviction is safe by construction: `Derived` carries `evict`/`evict_all`, and a
  **chaos-eviction property battery** already evicts at every legal opportunity
  to prove correctness is unaffected.
- Component storage: `ComponentStore<K, C>` (`kernel/src/component.rs`) — "the
  storage substrate an ECS component registry is built on", `BTreeMap`-backed,
  deterministic ascending-by-key iteration. **Component storage must not be
  keyed on `TypeId`**: its ordering is not build-stable and would put an
  unstable iteration order under a byte-identity guarantee.
- **No hidden cache inside a derivation.** The Terrier: "a hidden cache in a
  derivation path is how derived state stops being derived." Caller-owned,
  `&mut`-threaded, no `RefCell`/global/`OnceLock`, and a test pinning
  cache-present **byte-identical** to cache-absent — the contract
  `blend_at_cached` already states.

**5.5 Discovery is observation-scoped.** Evaluation is bounded by what is
observable, not by the world. This is not an optimisation:
`KNOW-uncertainty-is-epistemic` establishes that the world is a total function
and an observer is a partial evaluation of it, so lazy evaluation *is* what
observation means here.

**Rumors are degraded observations, never fabrications.** A rumor descends from
an actual observation worn by transmission, so its error modes are **stale**,
**vague** (precision decays per hop, giving a region not a point) and
**misattributed** — never "rolled false", which would be an aleatory mechanic
the epistemics forbid. The seam exists: `knowledge.rs` splits projection-derived
entries (re-derivable, enforced by `knowledge_is_subset`) from **heard** entries,
deliberately outside that contract because "heard ≠ true is the epistemic
point". `Knowledge` is per-agent and serialized into **session snapshots, not
the ledger**.

**5.6 The kinds.** A spanning set, chosen so each parameter is exercised
independently rather than all set to a middle value.

| kind | contextuality | correlation length | enterable | what it proves |
| --- | --- | --- | --- | --- |
| **spring / seep** | high — carbonate × drainage × elevation | long | water source | the sign case: diagnostic of what is underfoot |
| **overhang / hollow** | medium — induration × slope | short–medium | no, but **affords shelter and fire** | the affordance path end to end |
| **thicket / brake** | high — productivity × moisture | long | no, texture | attacks biome monotony directly |
| **erratic / scatter** | **low — mostly free noise** | short | no | the **negative control** |

The erratic sits deliberately at the **low-contextuality** end — mostly free
noise, uncorrelated with any macro cause — so the legibility metric can be shown
to **discriminate**. If springs and erratics score alike, the metric measures
nothing — and that is better learned from a designed control than inferred from
a uniformly good-looking number.

Overhangs use the existing affordance vocabulary: `ObjectProperty`
(`SupportsRest`, `HoldsLiquid`, `AffordsPassage`, `Encloses`, warmth), and the
STORAGE SHAPE `object_registry` already is — a `ComponentStore<K,
ObjectTraits>` — held **vessel-locally — build-state, not world-state,
nothing serialized**. "A place to get out of the rain and start a fire" is a
component bundle in a vocabulary that already exists.

**This paragraph used to say the bundle lives IN `object_registry` itself,
keyed on `KindId`. It shipped as a SEPARATE table instead
(`weft_object_registry() -> ComponentStore<WeftKind, ObjectTraits>`,
Task 8, fix round 1 / decision ledger #13), and the reason is not stylistic:
`object_registry`'s own keys are gated closed against
`hornvale_thing::THING_KINDS` by `windows/vessel/tests/suite/
kind_totality.rs`'s `every_propertied_kind_is_a_roster_row` (G-e, spec
§5.1), and `THING_KINDS` is registered into the world's `ConceptRegistry`
(`domains/thing/src/lib.rs`'s `register_concepts`), which serializes into
`world.json`. Adding `"overhang"` to `THING_KINDS` to admit it into
`object_registry` would therefore move `cli/tests/fixtures/
world-seed-42.json`'s committed bytes directly — this task's own §6
constraint forbids exactly that ("derived features are never committed
facts"). The single-registry route was never available to take, not merely
inconvenient. `weft_object_registry` reuses the SAME `offered()` query
`object_registry` is read through — one implementation, two disjoint key
populations (`KindId` vs `WeftKind`, pinned by a test:
`the_weft.rs::no_weft_kind_label_appears_in_thing_kinds`) — so this is not a
second, competing affordance mechanism.**

**5.7 A kind is three things and nothing else** — a component bundle, a
prevalence recipe, and the three scalars. Adding kind N+1 is an append that
edits nothing existing: the constitutional rule *"adding a domain must never
require editing an existing one"* at feature scale, and what makes "until I've
run out of ideas" cheap rather than a growing tax.

## 6. Determinism obligations

- **A seed-derivation label per kind** (`derived/<kind>/v1`) — a save-format
  contract. Permanent; regeneration uses an epoch suffix, never a rename. It
  must **also** be added to `cli/src/streams.rs`'s stamp roster, which is a
  separate file and a routine miss.
- **`plumb:` tags** on every authored constant (all three per-kind scalars are
  authored numbers; the check is default-deny).
- **`type-audit:` tags** on new `pub`-boundary primitives, report regenerated in
  the same commit.
- **`lexicon_guard`**: the word `cell` means a mesh vertex here; use `Vertex`
  and `Facet`. A new file gets no inventory row and may carry none.
- **Byte-identity:** `world.json` must **not** move — derived features are not
  committed facts. This is an assertion to pin with a test, not an assumption.

**Expected artifact movement — a branch table, not a prediction.** After
`make rebaseline`: if only `docs/audits/` and `docs/digest/` moved, regenerate
and commit in the same commit. If `clients/game/core/tests/fixtures/` or
`book/src/gallery/` moved, that is expected for Half A and Half B (walk-band
prose changes) — inspect the diff, confirm it is the campaign's own feature, and
rebaseline deliberately. If `cli/tests/fixtures/world-seed-42.json` moved,
**STOP**: derived features must not reach the ledger, and that byte-golden is
written only by `make rebaseline-goldens`, never by `make rebaseline`.

## 7. Preregistered measurement (decision 0016)

Frozen here, before the code that would move them.

**H1 — density.** The derived surface raises encounter density by ≥ 3 orders of
magnitude over the placed baseline. *Two numbers, separately reported, because
observation-scoped discovery makes them different questions:* **existence
density** (god's-eye: fraction of land facets carrying any derived feature) and
**encounter rate** (features met per unit travel on a seeded walk). H3 measured
the first only.

**H2 — coherence.** Derived features are spatially autocorrelated, not speckle,
measured by a standard statistic (Moran's I or Ripley's K) rather than a bespoke
one. **Paired with an anti-vacuity companion** in The Ford's shape, so a
degenerate world cannot score perfectly.

**H3 — the legibility bet, and it is allowed to fail.** A knowledgeable observer
can predict a facet's derived features from visible antecedents better than a
naive one. Operationally: mutual information between the local macro state and
the feature set, per kind. **Preregistered ordering, which is the real
prediction:** `spring > thicket > overhang > erratic`. The erratic is the
negative control and must score near zero; if it does not, the instrument is
measuring something other than legibility and the finding is about the
instrument.

**Amendment (Task 7 fix round 1, I1) — the population is land-eligible
facets, not the whole sphere.** This amendment lands AFTER Task 7's
eligibility gate (spec §5.2's own lerp floor, closed by R1: every kind's
`prevalence` now returns exactly `0.0` on ineligible ground, tested before
any macro-state read or noise draw) and BEFORE Task 9 measures H3 against
real data — a confound removed ahead of measurement, not a result rescued
after seeing one (decision 0016's forbidden shape is the reverse ordering).

**Why:** the eligibility gate makes `occurrence ⇒ land` hold with certainty
for every kind, uniformly. That shared gate is itself a macro correlation,
and on a WHOLE-SPHERE population it contributes mutual information no kind
earned by tracking any real cause — every kind earns it for free, from the
same land/ocean split R1 gave all four.

**The gate-component estimator, stated precisely (fix round 2, closing a gap
the original amendment left — this is a DIAGNOSTIC illustrating the confound,
not H3's own headline statistic, which stays "macro state × feature set" per
H3's own opening paragraph above).** Two BINARY random variables per kind,
over the WHOLE-SPHERE population (every seed-42 walk-depth facet, all 40,962
geosphere vertices, `Facet::containing(geo.position(v), geo.depth() + 7)`):
`X` = facet is land-eligible (`blend_corner_weights(weights, pack.land) >=
0.5`); `Y` = the kind's `occurs` fired at that facet. `I(X;Y)` is the
standard discrete mutual information in bits,
`Σ p(x,y) log₂(p(x,y) / (p(x)p(y)))` over the four joint cells. This is
DIFFERENT from H3's own variable pair (macro state × feature set) by
design — it isolates the ONE component (the eligibility gate alone) that a
whole-sphere population would fold into H3's real statistic, so it is
measured on its own to show the confound exists, not substituted for H3's
actual metric.

Measured (Task 7 fix round 2, this tree, seed 42, `n=40,962`,
`n_land=11,218`): **every kind's `n_land_and_occurs` equals its own
`n_occurs` exactly** (R1's guarantee, reproduced numerically, not merely
asserted) — spring `403=403`, overhang `843=843`, thicket `1,517=1,517`,
erratic `428=428`. The gate-component MI, all four kinds, ranked:

| kind | gate-component MI (bits) |
| --- | --- |
| thicket | 0.07198 |
| overhang | 0.03929 |
| erratic | 0.01974 |
| spring | **0.01857** |

**The sign case places LAST** — a stronger argument for restricting the
population than the two-kind (erratic vs. spring) comparison the original
amendment recorded: on a whole-sphere population, the confound would not
merely let the negative control outscore the sign case (erratic `0.0197` >
spring `0.0186`, the original finding), it would rank spring — H3's own
"diagnostic of what is underfoot" flagship — LAST of all four kinds on the
gate component alone, for a reason that has nothing to do with legibility.

The fix: **compute H3's mutual information over the land-eligible population
only**, so the gate is held constant rather than measured as if it were
signal. **The remedy is zero by construction, not merely small: restricted
to land-eligible facets, `X` (land-eligibility) is a CONSTANT (`true` for
every member of the population, by the restriction itself), and mutual
information between any variable and a constant is algebraically `0` — not
an approximation, not a re-measurement that happens to read near zero.** The
preregistered ordering (`spring > thicket > overhang > erratic`) and the
erratic's near-zero requirement are otherwise unchanged — only the population
the statistic is computed over is amended, and it is amended here, in the
spec, before Task 9 runs the measurement.

**H4 — no goose chases. DEFERRED WITH ITS PRODUCER, and this spec no longer
preregisters it.** The invariant is "every rumor names a region, and the region
is never empty". This campaign builds **no rumor producer** — §5.5 names rumors
as an existing *seam* (`knowledge.rs`'s heard entries), not as a deliverable —
so a test of H4 would pass by having no rumors to check, which is exactly the
vacuous-guard shape this repo has shipped five times. H4 and the producer are
deferred together and carry a registry row. Restoring H4 is the first task of
whichever campaign builds the producer.

A falsified prediction is a finding. No constant is retuned to rescue one after
unblinding without saying so in the chronicle.

## 8. Non-goals

- **The standing/promotion axis.** 0669 left it unnamed; nothing here forces it.
- **The legibility campaign** (`SURF-legibility-is-inference-from-signs`) —
  Nathan's "strongly desired followup", deferred by ledger #3. This campaign
  measures legibility; it does not reorganise itself around it.
- **A general ECS query engine.** `component.rs` names the dense-`Vec` backend
  and permutation indexes as "the query engine's work (metaplan §4.5, campaign
  4)". Out of scope.
- **Fixing biome monotony at its source.** Half B improves on it by reading
  underneath the label; it does not change the biome system.
- Two defects observed in the survey and left alone: all settlement residents
  listed as "Here:" in every chamber, and `examine`'s "You see no a …" article
  agreement.

## 9. Risks

1. **Cost.** `brief_of` runs on every `look` and every `enter`, and The Terrier
   hoisted an occupation map out of it for costing 8.7–26 ms per call. The
   window makes the per-tick tax zero when stationary and the movement tax a
   perimeter (~84 facets per step at radius 10, against ~441 held) — but the
   radius is a measurement, not a guess, and Task 0 should budget it.
2. **The metric fails to discriminate.** Mitigated by the erratic control, which
   turns that failure into a reportable finding rather than a silent pass.
3. **Two halves is a large campaign.** Separable at a plan-stage boundary.

## 10. Definition of Done

Beyond the standard DoD (chronicle entry, retrospective, freshness sweep):

- **Re-score the Confidence Gradient** (decision 0030).
  `book/src/open-questions.md` states 41,200× and ~2,400× against the
  square-mile target; the aim is 1 per facet, so they become **84,200×** and
  **~4,915×**. The 5.8% and 17.1× figures are target-independent and must NOT
  be touched (§1.1).
- Registry rows flipped and repointed;
  `SURF-legibility-is-inference-from-signs` repointed to the chronicle.
- Decisions minted inside **0686–0695** only.

## 11. Decisions expected

Numbers assigned at authoring time from the reserved block; this list is the
intent, not a reservation of specific numbers.

- The tier axis is **when a feature is generated**, and the same kind may exist
  at both tiers (records ledger #5, which corrects a criterion that would
  otherwise have governed every future kind).
- Per-kind prevalence fields are not normalised across kinds — the simplex
  constraint is a structural cap on enterable density.
- The derived surface reads continuous causes, never categorical labels.
- Rumors are degraded observations, never fabrications; the region is never
  empty.
