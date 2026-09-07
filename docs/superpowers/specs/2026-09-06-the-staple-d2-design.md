# The Staple — D2 design brief

**Status:** G3 package — awaiting Nathan's review before planning

**Campaign:** The Staple, D2: “more than one thing flows, and some of it by
exchange”

**Metaplan:** `docs/superpowers/specs/2026-09-04-the-staple-metaplan.md`, §3,
§4 D2, §5, §6

## 1. Claim

D2 adds a voluntary local exchange channel beside the existing coercive
tribute channel. It first separates people from subsistence, then tests
whether exchange destabilizes a demography calibrated around
`ASSESS_RATE = GROWTH_RATE / 8`.

D2 is a dynamics rung. It therefore requires a preregistered Task 0 probe,
an epoch, a census re-baseline, and conversion of history-adjacent pins to
invariants before implementation is accepted.

## 2. Tree verification

The current production path is the worldgen history bake. `Community` is the
live bake state in `windows/worldgen/src/history_bake.rs:1168`; it carries
`population` and `stores`. `stores` is accumulated wealth/tribute and is
documented as never eaten. The remittance path at
`history_bake.rs:3122-3137` subtracts from subordinate `population` and adds
to patron `stores`.

`ASSESS_RATE` is `0.025` at `history_bake.rs:565-580`, coupled to
`GROWTH_RATE` as `GROWTH_RATE / 8`. `PHASES_PER_YEAR` is `12`.

The catchment machinery in `domains/demography/src/flow.rs` and `condense.rs`
is not read by production. `condense_tagged`'s caller is the Lab accessor
`demography_report_with_beta_from` in `windows/worldgen/src/lib.rs:2230-2297`.
The Lot reads committed occupation records and derives `person_years` in
`windows/lot/src/context.rs:222`; it does not own this stock. No relevant
exchange path moved under The Lot or The Culvert.

## 3. Conceptual model

The D2 resource family is typed from the beginning. D2 exercises two
non-convertible subsistence types, provisionally `Subsistence.A` and
`Subsistence.B`. They are fungible within type, not across types. `Population`
is not inventory. Existing `stores` remains non-edible wealth and is not
reinterpreted.

One subsistence unit is one person-phase of baseline subsistence. Quantities
may be fractional internally, but the unit, consumption basis, and all
calibration coefficients are explicit.

Existing total harvest/productivity becomes total subsistence production. A
deterministic terrain/climate-derived specialization coefficient splits the
unchanged total between the two types. D2 adds no random draws, second
climate model, conversion, spoilage, storage capacity, route cost, transport
loss, currency, debt, labor promise, or price discovery.

Each person-phase consumes a fixed complementary basket of both types. A
shortfall in either type cannot be substituted by the other. Unconsumed stock
carries forward losslessly.

The phase order is:

`produce → propose/clear exchange → consume → existing pressure/growth → tribute`

Exchange uses existing one-hop local reachability. Communities may propose
against projected current-phase demand; future promises are outside D2.
Opening stock first reserves the community's projected basket. Only the
remainder is offered. Clearing is simultaneous, deterministic, and pro-rata
under scarcity. Incoming stock may fund already-declared acyclic downstream
requests. An unfunded cycle cannot settle, so promises cannot manufacture
stock.

Every attempt has an explicit derived outcome: proposed, accepted, settled,
partial, refused, or impossible. Only delivered quantities affect stocks.
The full outcome trace is a study artifact; aggregate rates may be promoted
to the larger census after the rung is accepted.

The demographic adapter is baseline-preserving, monotone, and bounded: full
basket satisfaction leaves the current path unchanged, while typed shortfall
feeds the existing pressure pathway through an explicit D2 coefficient.

## 4. Task 0 probe

Run a separate paired 200-seed intervention study. For every seed, build a
control with the current path and a treatment with the D2 channel, preserving
all pre-existing draws and pins. The canonical larger census remains the
production baseline; this paired study measures the intervention.

Primary counts:

1. **Activation:** seeds with at least one settled exchange / 200. The dead
   pole is `0 / 200`.
2. **Demographic instability:** treatment seeds breaching each individually
   named, pre-existing demographic calibration bar / 200. The dead pole is
   `> 100 / 200`.

The instability verdict must use existing code bars, not a post-hoc authored
target. The exact bar set is a G3 review item because the tree currently
exposes census descriptors (`settlement-count`, `mean-population`, and
`total-population`) but no single direct population-stability band. No plan
may proceed until the bar set and its direction are named from live code.

For every seed, print—but do not use as an additional verdict—the counts of
attempted, settled, partial, refused, and impossible exchanges; stock
conservation residuals; activation; and each demographic-bar breach. Report
the full outcome denominators separately from the world denominator.

The probe must also show that a constructed control preserves the property
that the current history path is bit-identical when exchange is disabled. A
reviewer must explicitly ask whether each assertion can pass vacuously.

## 5. Cost and open ownership

Every dynamics rung costs an epoch, a census re-baseline, and conversion of
history-adjacent pins to invariants. D2 also costs a new stock accounting
surface, deterministic clearing tests, conservation tests, and paired
intervention measurement.

D2 owns the first climate-to-city link: `climate → local production → typed
stock composition → exchange access and dependence → settlement stability`.
The direct hinterland-capacity growth ceiling and comparative city/notability
remain open for D4/D5; D1's struck catchment does not own them.

The following remain deferred: obligations and debt; interest; labor,
person-related, or access claims; currency and barter instruments; conversion
rates; market quotes; negotiation; storage loss/capacity; transport and
carriers; priority institutions; and multi-commodity production beyond the
two D2 types.

