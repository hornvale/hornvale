# Temperature

**July 2026 · outcome: merged — the temperature pair deep time proved is
promoted to the kernel's shared vocabulary, climate's last bare-number
temperature boundary is typed, and a cross-domain borrow is gone**

## What was attempted

The second family under the kernel-units doctrine (decision 0044), and the
first *promotion*. Where [The Datum](./the-datum.md) had to invent its type,
this campaign inherited one: the deep-time model had long carried a
`Celsius` newtype for absolute readings and a guarded `TempAnomaly` for
differences from the present, battle-tested through the ice-advance
machinery. But the pair lived inside one domain, and the layering rule —
a domain depends on the kernel and nothing else — meant no other domain
could speak it. Climate, which computes the world's temperature field in
the first place, emitted bare `f64` degrees at its boundary; and the
composition root papered over the gap with a borrow that should never have
existed, importing one domain's private `Celsius` to wrap another domain's
output.

The campaign moved the pair verbatim into the kernel, renamed the absolute
reading to the semantic `Temperature`, typed climate's boundary, and
deleted the borrow — under the same non-negotiable constraint as the
elevation migration: byte-identical artifacts, to the digit.

## The intensive insight

Temperature is the quantity that proves classification is two questions,
not one. On the scales of measurement it is *ratio* — a true zero exists,
and absolute readings in kelvin can meaningfully be halved or doubled. By
the elevation campaign's logic that should license full arithmetic. Yet
adding two temperatures is physically meaningless: combine two rooms at
20 °C and you do not get a room at 40 °C. Additivity tracks a different
axis — *extensive* versus *intensive*. Mass and energy are extensive: the
whole is the sum of the parts. Temperature, density, and pressure are
intensive: properties of a state, not amounts of a stuff, and no sum of
states is a state.

The type system now says exactly that. There is no
`Temperature + Temperature`. Subtracting two readings yields a
`TempAnomaly` — a genuine ratio quantity (differences of intensive values
*do* add) — and an anomaly is the only thing that can be added back to a
reading: `present + ΔT = era`. Anomalies compose with each other freely.
The full algebra of what physics permits, and nothing physics forbids, is
carried by three operator implementations; the mistake this pair was
originally invented to stop — handing an absolute reading to code
expecting a difference from present, which earlier iterations of the
deep-time model actually made — is now unrepresentable everywhere, not
just inside one crate.

## Celsius-canonical, kelvin as a view

If temperature is ratio-scaled in kelvin, why is the stored representation
Celsius? Two reasons, one hard and one subtle. The hard one is
byte-identity: every temperature the simulation has ever serialized —
almanac tables, study rows, locale JSON — is in Celsius degrees, and a
canonical-unit swap would change every one of those bytes; that is a
regenerate-and-review event, not a type migration, and this campaign
refused to be both at once. The subtle one is precision: serialized floats
quantize to eight significant digits, and a canonical unit whose typical
magnitude is ~300 spends leading digits on the constant offset that a
~15-degree Celsius reading keeps for its fraction. *Put the datum near the
data.* Kelvin exists as an accessor — a computed view for any consumer
that wants the ratio frame — while the canonical stored degree stays where
the data lives.

## The removed borrow

The quiet structural win. Before: climate derived a temperature field and
returned bare numbers; the composition root wrapped each one in the
deep-time domain's `Celsius` on the way through — a window reaching into
one domain's private vocabulary to re-validate values another domain had
already computed and vouched for. After: climate's boundary returns
`Temperature` directly, validation lives at construction — in the one
place the value is actually derived — and the composition root passes a
typed value along without opening it. The redundant wrap did not merely
move; it vanished, and the empty artifact diff is the proof it had been
redundant all along. Guarantees now flow from where values are made, and
are never re-manufactured downstream.

## The mold holds

The elevation campaign was a pilot; this one was the test of whether the
mold transfers. It does: the same origin-first sequencing, the same
compiler-enumerated conversion, the same byte-identity proof, on a
quantity with the opposite classification puzzle — elevation is interval
pretending nothing about zero, temperature is ratio that still refuses to
add. Deep time keeps its genuinely private quantities (ice volume, eustatic
sea-level change) at home; the shared pair now lives in the one place every
domain can reach it. Angle is next in the family queue, and it is *not* a
third pressing of this mold — longitude wraps, latitude clamps, and cyclic
arithmetic is new machinery, which is exactly why it waits for its own
campaign.
