# 0055. Sculpting is terrain epoch v3

**Status:** Accepted (2026-07-16) · **Decider:** Nathan (Sculpting campaign
close; tuning-season disposition, decision ledger #11)

In the context of Crust (terrain epoch v2) handing forward two coast-texture
bands — shoreline-development and shelf-fraction — that its craton and
lobing knobs provably could not reach, because coastline complexity and a
true depositional shelf are products of erosion and sediment transport, we
decided that **Sculpting is a full one-way regeneration of every world**
(epoch v3, per decision 0039: a contradicting generator cannot coexist as a
tier), built as a one-shot erosion/deposition correction field (**engine
A**) behind a **potential-agnostic function seam** — `carve(elevation,
drainage, induration_field, params) -> CarveDelta` reads as "a potential, a
flow network derived from it, a resistivity field," water-over-elevation
being only this campaign's instantiation — accepting that sea level and
drainage now solve **twice** (a provisional pass feeding the carve; a final
pass, through a bounded sea-trim, over the carved surface) and that the
carve's `sediment_thickness` field, not a slope proxy, now drives The
Ground's `soil_depth` and alluvium classification.

**Context.** The epoch appends exactly two new drawn streams —
`terrain/terranes` and `terrain/microcontinents` — strictly after every
existing draw, so pin-isolation holds and every pre-Sculpting save-format
contract is untouched; all other new terrain (hotspot trails, atoll chains,
discrete arcs, belt anatomy, a trench, fBm relief, wave-cut erosion, the
sediment wedge, deltas, playas, barrier islands) is derived or hash-noise,
drawing nothing new. The doubled drainage pass was the epoch's one
preregistered performance risk; it was measured (not assumed) at ~1.10×
per-world build time (+4.5 minutes at census scale) and accepted by Nathan
without a silent fidelity cut. A wiring-stage hard stop (decision ledger #4)
found that the marine freeboard constants (wedge cap, atoll rim) had been
written against the *pre-carve* sea level in a one-shot engine whose final
sea level moves after the carve fills the ocean, producing systematically
emergent shelves and atolls; the fix — a bounded second marine pass
(provisional sea level, trim wedge/atoll tops to their freeboard below it,
then the true final solve, trimmed volume booked as oceanic loss) — is
itself part of this epoch's contract, not a later patch. Two heavier engines
(B, an implicit steady-state solver; C, fixed-N geomorphic iteration) were
designed for but deliberately not built: a preregistered escalation
criterion, the **rerouted-flow fraction** over each world's twenty largest
rivers (share whose post-carve path diverges from its pre-carve path),
governs whether they are ever needed — under 0.10 median is
self-consistent (ship A alone), 0.10–0.30 flags for a human ruling on
whether B enters evaluation, over 0.30 rejects A outright.

**Consequence.** Engine A ships alone: the tuning season closed at a reroute
median of **0.0834**, comfortably under the self-consistency line, with zero
seeds ever crossing 0.30 across the whole season — B and C remain a
designed, unbuilt seam. Five of the six preregistered acceptance bands
(hypsometric-bimodality, shelf-fraction, continent-count,
largest-continent-share, plate-size-gini) land inside their Earth-anchored
ranges; shelf-fraction is the one Crust could not close (0.048 → ~0.09).
The sixth, shoreline-development, is left on an **explicitly open verdict**:
every mechanism the spec banked for it is now built and it still sits below
its floor. Per Nathan's ruling (ledger #11), no new floor is invented
unilaterally — the band is recorded outside, low, with a formal
supersession note (the floor's own anchor was partly built on
fragment-swarm coastline noise Crust deliberately removed), and **MAP-21**
(the banked rift-and-fit fake-history coda) inherits the open band as part
of its evidence baseline, mirroring Crust's own handoff of this exact pair
of bands to Sculpting one epoch prior. As with every epoch, every committed
artifact and golden fixture re-baselines in the drifting commit, and the
canonical census regenerates once, at close, on the AWS box (decision
0046) — never locally.

**See also.** `docs/superpowers/specs/2026-07-14-sculpting-design.md`;
[Sculpting chronicle](https://github.com/hornvale/hornvale/blob/main/book/src/chronicle/sculpting.md);
[The Census of Coasts III](https://github.com/hornvale/hornvale/blob/main/book/src/laboratory/census-of-coasts-iii.md);
decision [0039](0039-epochs-replace-tiers-refine.md) (epochs replace,
tiers refine); decision [0046](0046-census-regen-is-remote-only.md)
(census regeneration is AWS-only); decision
[0053](0053-ocean-fraction-is-a-target-under-supply-limited-crust.md) (the
preceding single-craton-hypsometry ruling this campaign's single-craton
shelf floor battery continued to enforce throughout tuning).
