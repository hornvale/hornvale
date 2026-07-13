# 0044. Shared units live in the kernel

**Status:** Accepted (2026-07-13) · **Decider:** Nathan

In the context of decision 0008 ratifying typed quantities at API boundaries
while staying silent on *where* those newtypes live, facing a domain-layering
rule (a domain depends on the kernel and nothing else) that gives a quantity
spoken by more than one domain exactly one legal home, we decided that
**coherent physical quantities that cross domain boundaries live in the
kernel; single-domain quantities stay in their domain** — a quantity belongs
in the kernel when either (a) more than one domain speaks it, or (b) it
originates in a kernel type (e.g. `GeoCoord`'s latitude) — together with a
Stevens-scale classifier for *how* to type a quantity once it is placed, and a
boundary treatment for how it behaves crossing serialize, trace, presentation,
and evolution edges.

**Context.** The pervasive elevation/sea-level datum dodged 0008 by staying a
bare `f64` under the `waiver(elevation-convention)` type-audit tag (decision
0028), a pragmatic wart with no durable home in `docs/decisions/`. The Datum
campaign (spec
`docs/superpowers/specs/2026-07-12-the-datum-design.md`) proved the policy
end-to-end on that one family — a kernel `ReferenceElevation` newtype,
re-typed across every crate that speaks the elevation field and its sea-level
threshold — and the full doctrine this record ratifies was developed
alongside it as `docs/design/kernel-units-doctrine.md`, which this record
distills and supersedes as the citable source (the doctrine document remains
as worked rationale and the family roadmap).

**Decision.**

- **Placement.** As above: cross-domain or kernel-originating quantities live
  in `hornvale-kernel`; a quantity only one domain speaks stays there (the
  existing split already worked this way — the kernel holds `WorldTime`;
  astronomy holds its scaled views `Au`/`SolarMasses` over kernel
  Distance/Mass).
- **Proactive vocabulary, reactive surface.** The kernel unit vocabulary is
  built ahead of demand — units are a closed, knowable domain — but each
  type's richer surface (accessors, conversions) is added only when a
  consumer needs it. Proactive placement still requires a real anchor (a live
  waiver it retires, or a live wart it dedups); it is not licence for
  speculative SI completeness.
- **Promotion — classify by Stevens scale, then type.** A quantity's scale of
  measurement decides whether it is a unit at all and how it is typed:
  - **ratio** (true zero; full arithmetic) — energy, mass, distance,
    duration, absolute temperature, and every delta — a shared kernel type
    with full arithmetic.
  - **interval** (arbitrary zero; differences meaningful, ratios not) —
    elevation, Celsius, calendar date — a unit type that **carries its
    zero-convention (datum)**, promoted together with its ratio-valued delta
    (elevation's `Sub`-delta, `Celsius` → `TempAnomaly`).
  - **ordinal** and **nominal** (Mohs, Beaufort, biome, species) are **not
    units** — ordered enums or identifiers, kept out of the units library.
  - Two rules follow from the lattice: **level is chosen at birth**
    (structure only flows down the lattice — ordinal cannot be upgraded to
    interval/ratio later without re-modelling), and **interval types carry
    their datum** across every boundary they cross.
- **Derived units have no dimensional algebra.** A relation between units is a
  named-law constructor (`Speed::over(distance, duration)`), not a coupled
  exponent system — consistent with 0008's and 0004's rejection of a
  dimensional-analysis crate.
- **Boundaries — rich inside, contract at the edge.** A unit is a rich
  compute-path type that degrades to a value plus an external contract at
  each dumb boundary: quantized bytes at serialize, 8-sig-fig `f64` at the
  cross-platform edge, `Value::Number` at the trace envelope (contract lives
  on the predicate, not the envelope), a formatted string owned by the
  observer at presentation (no `impl Display` on a unit type), and a
  regenerate-from-seed at evolution (a canonical-representation change is not
  a stream-label epoch). Units are compute-only: no unit type self-serializes
  today, and a **validating unit must never derive transparent
  `Deserialize`** — it would bypass `new()` and admit the values the type
  exists to forbid.
- **Fences.** No dimensional algebra (above). Rank-1 vector/tensor quantities
  (position, velocity, gradient) are a deferred category, decomposed later
  into scalar quantities plus a direction — nothing built under this decision
  forecloses them. Ordinal/nominal scales are never units, by definition
  above.

This record supersedes 0008's silence on placement; 0008's newtype-vs-bare-f64
boundary rule stands unchanged. The `elevation-convention` waiver (decision
0028's rubric) is reclassified **temporary**: it is retired for the elevation
datum by `ReferenceElevation` (`kernel/src/units.rs`), the pilot this record
ratifies; `crust-km-convention` and any other quantity still under that waiver
retire on their own family's turn (see the doctrine's roadmap).

**Consequence.** A future coherent quantity spoken by more than one domain is
added to the kernel, not re-invented per-domain or left bare; the Stevens
classifier and the two birth/datum rules are the checklist for typing it. The
kernel unit vocabulary grows by anchored family (temperature next, then
angle — doctrine roadmap), each its own small byte-identical campaign, not one
mega-campaign. `tools/type-audit` continues to flag any un-waived bare
primitive at a pub boundary; a quantity that meets this record's placement
test and still shows bare is now a `pending` tag with this record as its
resolution path, not a permanent `waiver`.

**See also.** Decision 0008 (typed quantities at API boundaries — refined
here); decision 0028 (the bare-ok rubric — the waiver this record retires for
elevation); decision 0004 / decision 0041 (no dimensional-analysis crate;
libm for the transcendentals a unit's compute path may call);
`docs/design/kernel-units-doctrine.md` (the full doctrine, worked rationale,
and family roadmap this record ratifies and distills);
`docs/superpowers/specs/2026-07-12-the-datum-design.md` (The Datum campaign —
the elevation pilot that proved this policy end-to-end); `kernel/src/units.rs`
(`ReferenceElevation`, the first kernel unit type).
