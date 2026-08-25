# The Granary — fine-resolution history, layer 1: seasonality

- **Status:** draft (G3 review)
- **Date:** 2026-08-23
- **Origin:** idea-registry rows `MEM-founder-key-trim` / `MEM-founder-handle-epoch`;
  brainstorm session 2026-08-23.
- **Governing documents:** the longterm-plan spec (Constitution especially);
  decision 0016 (preregistration); decision 0126 (`Fact.day` typed time);
  decision 0127 (identity vs discrimination keys).

## 1. Problem

The deep-history bake reasons in whole years. Every event it commits —
founding, raid, flight, collapse, tribute taking — is dated only by the
year-step in which its cause fired. Two consequences:

1. **History has no temporal shape.** A shortage that emerges mid-season and
   the raid it provokes are simultaneous with everything else in their year;
   "the granary emptied, and by spring they were raiding" is not expressible.
   Stocks exist (`stores`, the granary), but as year-boundary scalars: there
   is no hunger side of the year for behavior to sit on.
2. **Founder handles need a synthetic discrimination tail.** Because a raided
   founding and its same-year successor share every founding-side field at
   every ancestry depth (The Ell's measurement), `founder_handle` folds two
   post-founding fields (`ended`, `peak_population`) on top of the identity
   key. Each recomputation of those fields forces a founder-rename epoch.

The root defect is resolution, not keys. This campaign fixes the resolution;
the key fix falls out as a byproduct.

## 2. The rethink this campaign is the first slice of

Fine-resolution history, in three falsifiable layers:

1. **Seasonality** (this campaign) — production distributes over the year via
   an authored, latitude-derived harvest curve; stores integrate it; raid and
   founding checks sample sub-year phases; events carry day-grain stamps.
2. **Inter-annual memory** — weather draws make some years bad; carry-over
   stores integrate them; bad-year runs produce cross-year raid flurries
   (the Viking-age mechanism). Requires new draws → stream epoch.
3. **Full event-level resolution** — every occupation fact carries day grain;
   chronicles narrate timing; archaeology's ruin-dating inherits real ages.

**Time denomination caveat (temporary, deliberate).** Layer 1 stamps events
in f64 days-of-bake — a GRAIN change from years, not a UNIT change. The
canonical WorldTime→seconds migration (integer seconds since epoch, calendar
accessors on the type) is owned by a separate session on another machine and
is IN FLIGHT; this campaign deliberately does not introduce a second-grain
bake-local type that would die on contact with it. Float-key sentinels
(`day_key`) survive one more generation. Recorded so the migration session
finds this dependency.

## 3. Design

### 3.1 The harvest curve

Each community's food production becomes a rate over the year:

```
production(t) = annual_yield × s(latitude, biome_class, t)
```

`s` is authored per biome class: near-zero through winter, swelling through
the growing season, peaking at harvest, falling to living-off-stores. Its
**phase derives from latitude** via the existing insolation/latitude axis
(cross-link: BIO-insolation-is-latitude notes insolation == latitude today);
a cell at −lat harvests half a year out of phase with +lat. Amplitude is
authored per biome class. No new draws — fully deterministic, pure function
of `(latitude, biome_class, day)`.

### 3.2 Stores integrate the curve

The existing granary stops being a year-boundary scalar and becomes a running
stock: production accrues daily, consumption bleeds daily. This sharpens the
existing harvest/bleed split in `collect_tribute` mechanically; no new model.
Stores clamp at zero; the starvation path is exercised, not invented.

### 3.3 Sub-year event placement

Raid and founding checks move from once-per-year to N fixed sub-year phases
(v1: ~12). At each phase the EXISTING raid rule runs unchanged — raid
strength, defensibility, and the dominance margin are untouched code; only
when the rule is asked changes. An event that fires is stamped with its
crossing day; `founded`/`ended` become day-grain.

### 3.4 Determinism

Phases process in fixed order; within a phase, communities in the existing
deterministic order. Same discipline as today, more ticks of it. Stream
consumption order untouched — no new draws, so the pin-isolation contracts
are unaffected.

## 4. Preregistration (decision 0016)

**Hypothesis:** raids are not uniform over the year — they cluster on the
hunger side of the annual cycle.

**Success criteria:**

1. **Non-uniformity:** a uniformity statistic over raid day-of-year across
   seeds 0–999 exceeds the null at p < 0.001. A null result ships as a
   finding: the curve moves stores but not behavior.
2. **Phase alignment:** the modal raid phase falls in the depleted half of
   the local curve for a majority of raiding communities.
3. **Amplitude coupling:** high-amplitude biomes concentrate raids more than
   low-amplitude ones. FALLBACK (ratified): if the roster's biome spread makes
   the contrast weak, report descriptively rather than failing the campaign.

**Instrument:** `studies/the-granary.study.json`, seeds 0–999,
`BuildDepth::Settlements`, per-raid `(seed, community, day-of-year,
local-amplitude)` rows via lab CSV machinery. Studies are data, metrics are
code (decision 0011).

**Byproduct check:** founder-collision re-run over seeds 0–2999 with the tail
removed. Zero collisions → retire the discrimination tail; otherwise keep it
and report either way.

## 5. Testing

- Curve determinism: pure-function table tests per biome class; phase
  symmetry (+lat vs −lat exactly half-year apart).
- Store integration: synthetic community produces the canonical sawtooth;
  never-negative clamp; starvation path.
- Event stamping: scripted two-community scenario; stamped day lands in the
  depleted phase; `founded`/`ended` carry day grain.
- Bake run-to-run byte-identity (same-seed): determinism holds even though
  committed artifacts change.
- Founder collision: extend `windows/worldgen/tests/founder_collision.rs`.

## 6. Epoch manifest (all expected, none silent)

- `history/bake/v2` → v3 label bump (The Contour precedent: changed committed
  history without moving stream consumption order).
- Seed-42 almanacs / history fixtures regenerate; possession galleries drift.
- Founder handles rename (accepted shape, The Ell precedent).
- Census columns touching history move → census refresh ON LEFFORD at close,
  per standing rule; never locally.

## 7. Scope fences

No new draws. No changes to raid strength/margin/defensibility math. No
inter-annual signal (layer 2). Timestamps stay f64 days-of-bake (ledger #4).
No calendar accessors on kernel types (the migration session owns time's
type). No changes to the identity key's shape (`founding_key` stays
founding-side only; day grain refines what "founded" means, not which fields
identity reads).

## 8. Definition of Done

Usual ladder; chronicle entry; book freshness sweep; retrospective;
idea-registry updates: `MEM-founder-key-trim` resolved-by-byproduct or
amended; new row for layer 2 (inter-annual memory / the Viking hypothesis);
the temporary-grain caveat recorded where the migration session will find it.
