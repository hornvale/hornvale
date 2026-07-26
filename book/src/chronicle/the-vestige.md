# The Vestige

**July 2026 · outcome: merged — the underworld gains its *history*: sealed
vaults, abandoned delvings, buried ruins, and dormant gate-scars, read backward
out of the settlement past and the deep-time archive, without moving a
committed byte**

## What was attempted

The Deep gave the world a stratigraphic *under*, and The Lode filled it with
inert contents — caves and ore. Both were geology: true the moment the world
was made, indifferent to anyone who ever lived. The Vestige is the first rung
where the underworld carries a *past* — not agency yet, but the residue agency
leaves behind. When a people abandons a settlement, something is left in the
ground: a warded vault they sealed, a delving they dug too far and walked away
from, a ruin the earth is slowly burying. The Vestige reads those traces out of
history the world has already narrated.

The organizing insight is that residue is not a new thing to *generate* — it is
an **integration layer**, the arc's first. Everything it needs already exists:
the settlement-abandonment history says who lived where and when they left; The
Deep's deep-time archive says how old the rock beneath them is; The Lode says
where the ore and caves that drew them sat; the Dread field says where the world
already feels wrong. A vestige is what you get when you *join* those four and
ask what the leaving left. No occupation record is duplicated, no new history is
invented — the residue is a view over the world's own memory of itself.

## What shipped

Each abandoned occupation, reconstructed from the ledger, becomes a **vestige**
carrying a kind (gate-scar, natural seal, abandoned delving, buried ruin, sealed
vault), a **seal state** that decays through *maintained → lapsing → breached*
as the maintaining civilization recedes into the past, a **valence** — whether
the site is still **venerated** (tended, remembered, a cathedral) or has been
**forgotten** (lost, a tomb no one guards) — and a **hazard** (structural,
toxic gas, pestilent, flooded, numinous, cursed). Deep pre-human crust carries
its own residue: gate-scars where something once opened, older than any people.
Where occupations stack on one cell across ages, their vestiges form a
**palimpsest** — the ground written on and overwritten.

The keystone is a **three-rate decay**, the nuclear-waste-warning problem made
mechanical: the residue itself outlasts the containment that was built to hold
it, and the containment outlasts the **warning** that told anyone what lay
inside. Warning-legibility decays fastest of the three (a short half-life on the
time since abandonment), so the oldest, most dangerous sites are precisely the
ones whose warnings have gone silent — a delver reads a breached seal with no
legible caution and no memory of what it held. Venerated sites decay slower than
forgotten ones: memory is itself a form of maintenance.

The residue surfaces as a query (`vestiges_at`), a full-color **map lens**
(`vestige-map`, colored by kind and valence), an almanac **"The Vestige"**
section, and four census metrics (residue density, forgotten fraction, dominant
hazard, mean warning-legibility). A `vestige_dread` field is derived and
**exposed** — the maximum dread the residue radiates into each cell — but left
unwired to any consumer, a hook for the metaphysics rung rather than a coupling
made now. Every output is a **pure derived read**: no new randomness beyond the
reused placement noise, no committed fact, no epoch, no changed byte. The seed-42
world serializes exactly as it did; world identity never moved.

## What it leaves reserved

The Vestige is the *derived, historical* half of the event-features rung. The
residue is inert — it records that a seal has breached, but nothing *comes
through*. The **live** half — a seal that fails during active simulation, a
delving that is dug now, a gate that opens as an event — waits for the rung
where the underworld first gains agency. The **entity** behind a numinous seal
or through a gate-scar stays behind the metaphysics gate: the `numinous` hazard
and the gate-scar are reserved hooks, named but empty. And the full
memory-and-upkeep model — who *chose* to keep tending a ward, and why one
people venerates what another forgot — is the vertical-relationship campaign's
to build; the valence ships here as a coarse age-and-abandonment proxy, honest
about being the down payment and not the arrival.
