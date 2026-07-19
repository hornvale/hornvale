# The Elements — Design

**Date:** 2026-07-18
**Status:** Draft — awaiting G3 review (campaign-autopilot hard stop). **Fidelity
carve-out: the pantheon reshape is a deliberate accuracy tradeoff — see §5.**
**Campaign:** (confirm numbering at merge)
**Provenance:** The correspondence audit's inverse view surfaced that the world
emits only **9 phenomenon kinds, 8 of them from a single domain** (astronomy),
plus one tier-0 ambient gloss. The Phenomena stream is the one seam between the
modeled world and everything that observes or interprets it — religion derives
deities from it, naming glosses it, perception and cognition will read it — so a
sky-only stream means the world's *entire lived experience* is astronomical.
A planet with richly-modeled climate, terrain, and biology has inhabitants who
can observe none of it. `GeneratedClimate` holds every temperature, moisture,
and wind value and emits **nothing**; only the tier-0 `UniformClimate` stub is
wired. This campaign brings the world's conditions into the stream.

---

## 1. Goal and contract

Make the Phenomena stream carry the world's **felt conditions**, not just its
sky, and remove the architectural coupling that kept them out.

Delivered:
1. **The phenomena-source roster** — extend the `Domain` trait with a
   `phenomena_source(...) -> Option<Box<dyn PhenomenaSource>>`, iterated in
   worldgen the way `register_all` iterates `DOMAINS`, replacing the six
   hardcoded `[&dyn PhenomenaSource; 2]` fan-outs. Adding a domain's phenomena
   never again edits worldgen or another domain.
2. **The climate emitter** — `GeneratedClimate` becomes a `PhenomenaSource`,
   emitting standing conditions that vary with (place × time): heat, cold,
   dryness, wetness (→ the snow/rain concepts), prevailing wind, and
   biome-as-experienced, derived from its existing fields.
3. **The correspondence payoff** — climate's percept-gapped concepts
   (`snow`/`rain`/`ice`, the biome classes) flip from `Void::Gap` to
   `Present(PerceptKind(...))`, closing both the orphan-phenomenon and the
   unperceived-concept gaps the audit names.

**Contract:** the framework (1) is **byte-identical** — when the roster yields
exactly today's sources, every world/almanac/artifact regenerates unchanged
(a provable de-risk stage). The emitter (2) is a **fidelity change** (§5): it
adds observations, which reshapes religion and naming. No new seed draws —
phenomena derive purely from existing world-state, so **no epoch** (§6). The
tier-0 `UniformClimate` byte path is preserved instruction-for-instruction
(decision 0039). No census regen, no AWS spend.

## 2. The defect, precisely

- **The stream is sky-only.** 9 registered kinds: 8 astronomy
  (`celestial-body`, `eclipse`, `tide`, `seasonal-cycle`, `night-star`,
  `heliacal-rising/-setting`, `wandering-star`) + climate's `ambient`. Religion
  therefore grows only celestial deities; naming glosses only sky.
- **The coupling.** `windows/worldgen/src/lib.rs` builds
  `let sources: [&dyn PhenomenaSource; 2] = [&sky, &climate];` in **six**
  near-identical fan-out sites, with the two concrete types named inline and
  `climate` bound to the tier-0 `UniformClimate` stub — never the rich
  `GeneratedClimate`. There is no roster analogous to `DOMAINS`. A domain cannot
  contribute a phenomenon without editing worldgen in six places — the exact
  coupling the constitution forbids for concept registration.
- **The rich model is disconnected.** `GeneratedClimate` (`provider.rs`) has
  `temperature_at(cell, day)` (seasonal + diurnal, via The Wandering Sun and The
  Turning), `moisture_at(cell)`, `prevailing_wind`, `biome_at(cell)` — all
  sampled at place×time — and implements no `PhenomenaSource`.

## 3. The framework — the phenomena-source roster

Abstraction-lift: phenomena-sourcing is the same shape as concept-registration —
domains contributing to a shared kernel structure through the roster. So extend
the existing seam rather than invent one.

- `Domain` gains `fn phenomena_source(&self, world: &World, wc: &WorldContext)
  -> Option<Box<dyn PhenomenaSource>>` (default `None` — most domains contribute
  nothing). Signature finalized against the concrete construction sites in
  Stage 1; the shape is: given the built world, hand back this domain's live
  source, or `None`.
- Worldgen replaces the six hardcoded fan-outs with **one** helper that folds
  `DOMAINS` (plus the sky, which stays special until astronomy is migrated) into
  the `sources` list. The order is deterministic (roster order); `observe`
  already sorts by salience with a kind→description tie-break, so source order
  cannot affect output bytes.
- **De-risk:** in Stage 1 the roster yields exactly today's sources (sky +
  tier-0 ambient), so the refactor is provably byte-identical before any new
  phenomenon exists.

## 4. The climate emitter — the pilot

`GeneratedClimate` implements `PhenomenaSource`, mirroring astronomy's
`GeneratedSky` template (read `ctx.position` → cell, `ctx.time` → day; emit pure
functions of world-state; no draws). The emittable set is a **field × rate
cross-product**, and the existing `Phenomenon.period_days` carries the rate:

| field | constant (period None → Eternal) | diurnal (~1 day → cyclic) | seasonal (~1 yr → cyclic) |
|---|---|---|---|
| temperature | the standing clime (a hot / cold land) | dawn cold / afternoon heat | winter's bite / summer's scorch |
| moisture | a dry / wet land (→ `rain` when warm+wet, `snow` when cold+wet) | — | the monsoon (if a seasonal moisture term exists; else omit) |
| wind | the prevailing wind | — | — |
| biome | biome-as-experienced (desert / forest / tundra …) | — | — |

- **`Venue::Ambient`** for all (they are pervasive conditions, not sky-placed).
- **Concept binding:** each phenomenon's `kind` is a registered concept key, so
  temperature-cold binds to a `cold`/`ice` concept, wet+warm to `rain`, the
  biome phenomenon to the biome concept — closing §7's percept gaps. New
  concept/kind names are decided in Stage 2 (e.g. a `heat`/`cold` pair, or a
  single `clime` kind parameterized by description — a design sub-choice made
  against the concept registry, flagged not pre-decided).
- **Position→cell bridge:** the emitter needs `GeoCoord → CellId`; if no helper
  exists in the emitter path, Stage 2 adds one (pure, no draw).

## 5. The salience model — **the fidelity decision (G3)**

Salience decides what crosses religion's `PANTHEON_FLOOR` (0.25) and therefore
**mints deities**. Today climate's `ambient` sits at 0.15 (sub-floor, no deity);
astronomy runs 0.1–1.0. A wave of supra-floor climate phenomena would enlarge
and re-texture **every pantheon** — weather-gods alongside sky-gods, and (since
standing conditions are `Ambient` + aperiodic → `Eternal`, while diurnal/seasonal
swings are periodic → cyclic) a mix of eternal and cyclic weather deities.

**The design: salience = deviation from typical, not raw value.** A cell's
climate phenomenon is salient in proportion to how *extreme* it is — a brutal
desert or an arctic waste crosses the floor and grows a weather-god; a temperate
valley stays sub-floor and sky-only. An affine function of the field's deviation
from a temperate baseline, clamped into a per-kind band (astronomy's own
pattern). This makes the pantheon reshape **proportionate to climatic
extremity** — naturalistic and self-limiting.

**The tradeoff for you to set:**
- *Conservative* (recommended first): a high deviation threshold, so only
  genuinely harsh regions mint weather-gods. Fewer new deities; smaller
  world.json / religion / name delta. **Reversibility asymmetry:** raising
  salience later *adds* gods (roughly additive); lowering it *un-mints* gods
  (disruptive to committed worlds) — so start conservative and raise if you
  want more.
- *Generous*: a lower threshold, so most non-temperate regions grow weather
  deities. A much richer, more animist world — and a much larger, harder-to-walk-
  back artifact delta.

This is the one call I want from you at G3: **which end of that dial**, and
whether the recommended conservative-first calibration is right.

## 6. Save format, determinism, tiers

- **New phenomenon-kind labels** are a permanent save-format contract but
  **additive and safe** — no epoch — *because phenomena derive purely from
  existing world-state (no new `Stream` draws)*. Confirm in Stage 2 that no
  drawn quantity or stream-consumption order changes.
- **Tiers refine, never contradict (decision 0039):** the climate emitter
  *refines* `UniformClimate`'s tier-0 `ambient` ("there is atmosphere") by
  adding *which* atmosphere obtains here and now; the tier-0 path stays
  byte-identical (the `crust.rs` instruction-for-instruction discipline).
- **Determinism:** `phenomena()` is pure — same (world, ctx) → same output.
  `observe`'s salience sort + tie-break already guarantee order-independence, so
  the roster's source order cannot perturb bytes.

## 7. The correspondence payoff

Once climate emits these phenomena, flip the percept edges the audit flags:
climate's `snow`/`rain`/`ice` and the biome-class concepts move from
`Void::Gap("not emitted as a phenomenon yet")` to `Present(PerceptKind(...))`.
The manifest view's `Unperceived` backlog shrinks and its percept-covered count
climbs — the same two-sided close as `eclipse`/`tide`, now for weather. This is
the campaign's built-in success metric.

## 8. Staging

1. **The roster (byte-identical).** Extend the `Domain` trait; fold the six
   fan-outs into one roster-driven path yielding today's sources. Prove
   byte-identity across every committed artifact. No new phenomenon.
2. **The climate emitter (the fidelity change).** `GeneratedClimate:
   PhenomenaSource`; the position→cell bridge; the field×rate phenomena; the
   deviation-salience model at the calibration you set at G3. Regenerate; the
   delta is the intended religion/naming reshape, reviewed and re-frozen.
3. **The correspondence payoff.** Flip the climate percept edges; regenerate the
   manifest view; assert the orphan/unperceived counts move as expected.
4. **Deferred to follow-ons:** terrain standing conditions (elevation band,
   coastal proximity, biome — a second, thinner source); demography's
   standing-condition fields (strife/wilderness/refugia). Species/culture/
   religion model nothing time-observable, so they stay `NONE` until they do.

## 9. Evidence battery

- **Framework byte-identity:** Stage 1 regenerates every committed artifact
  unchanged (`git diff --exit-code`); the tier-0 world path is untouched.
- **Purity/determinism:** the climate emitter is a pure function
  (property test: same (world, ctx) → same phenomena); `observe` output is
  order-independent of source order.
- **The intended delta is bounded and explained:** Stage 2's world.json /
  religion / almanac deltas are exactly the new weather phenomena and the
  deities/names they mint — no unrelated drift. Re-freeze the keystone golden.
- **Tier refinement, not contradiction:** the tier-0 `ambient` claim still
  holds wherever the fine emitter runs (a test that the ambient gloss is
  present/refined, never removed).
- **Correspondence closure:** a test that the flipped percept edges reference
  registered kinds and that the manifest trial-balance still foots.
- **Pantheon-reshape is proportionate:** a test (or documented census probe)
  that temperate seeds gain few/no weather deities while extreme-climate seeds
  gain them — the deviation-salience model doing its job.

## 10. Success criteria

1. A domain contributes a phenomenon source via the roster with **no edit to
   worldgen or another domain**; the six fan-outs are gone.
2. Stage 1 is byte-identical; the tier-0 path is preserved.
3. Climate emits felt conditions (heat/cold/wet/wind/biome) varying with
   place×time; no new draws; no epoch.
4. Salience tracks deviation from typical at the calibration set at G3; the
   pantheon reshape is proportionate to climatic extremity.
5. Climate's `snow`/`rain`/`ice`/biome percept edges flip to `Present`; the
   audit's orphan and unperceived counts drop accordingly.

## 11. Explicitly deferred / non-goals

- **Stochastic events** (storms, eruptions, births, deaths, migrations) — the
  world has no forward simulation or event log; these are architecturally out of
  scope. All new phenomena are standing conditions or deterministic-periodic.
- **Terrain / demography emitters** — the framework enables them; they are
  follow-on campaigns (§8.4).
- **Perception/cognition consuming the new phenomena** — this campaign fills the
  stream; using it downstream (a creature *feeling* cold and acting on it) is
  the cognition wave.
- **Re-tuning astronomy's salience** — untouched; the deviation model is
  climate-local.
