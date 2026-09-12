# The Circuits of Seeds 42, 7 and 1234

What this page witnesses: for each seed's every cave-bearing,
non-ocean vertex, a descent plan is grown (spec §3) and eight
preregistered readouts from two specs are measured against it, then
reduced to a panel median: the Crosscut's four (its spec §4 -- loop
share, density ordering, cross-floor cycles, semilattice overlap) and
the Brattice's four (its spec §4.1-4.4 -- gate yield, detour cost,
solvability, report-only gate counts). A descent plan is never stored --
it is a pure function of `(seed, vertex, rungs, kind, character)`, so
this page is a witness, not a record: every number here is re-derived
from the seed on each regeneration.

The verdict words PASSED / FALSIFIED are frozen by each readout's own
spec, decided before this code existed, and nothing here is tuned to
reach one -- a FALSIFIED verdict is a finding the campaign publishes,
not a bug to fix by moving the threshold. A comparison that cannot be
made for a seed (no cave of some kind exists there) prints NOT
MEASURABLE rather than a vacuous PASSED.

The Crosscut's four numbers moved once in this campaign, when The
Brattice took the Crosscut's own deferred `try_extend` fix (ledger #10,
ruling C); the attribution is by revert -- with that one change
reverted the panel is byte-identical to the pre-Brattice baseline. The
gate pass and class recompute alone leave them byte-identical.

The Plat appends a ninth section per seed, the Made population: the
columns a settled underworld people cut, read off the committed ledger
(so this verb builds to Full), the heart decile, doors on Made rungs,
circulation-realm shrinkage over every plan, and report-only counts.
The eight sections above it are byte-identical to the pre-Plat panel:
the reading changes no plan byte and the Made population is new.

```text
seed 42: 874 descents

loop share: median 0.1077 (frozen floor 0.50) -> FALSIFIED
cycle membership: median 0.8442 (report only; added after Task 2 showed the entrance doorway is a bridge on ~40% of seeds)

density ordering (median anchored realms per level):
  LavaTube  WildCave 1.0000  DrowTier 2.0000
  Fracture  WildCave 2.0000  DrowTier 3.0000
  Karst     WildCave 3.0000  DrowTier 4.0000
  LavaTube < Fracture < Karst -> PASSED
  DrowTier > WildCave within kind -> PASSED
    LavaTube: DrowTier 2 > WildCave 1 -> PASSED
    Fracture: DrowTier 3 > WildCave 2 -> PASSED
    Karst: DrowTier 4 > WildCave 3 -> PASSED

cross-floor: 839/874 descents = 0.9600 (frozen floor 0.25) -> PASSED

semilattice overlap: median 0.3030 (report only)

gate yield: median 1.0000 (frozen floor 0.70; FROM realms with an admissible drawn row TO rows applied in full) -> PASSED
  skips: inadmissible 0 claimed 198 no-room 0 unsolvable 2 (report only)
detour cost: median 1.2143 over 866 gated descents (frozen floor 1.10; default body's round trip gated / ungated) -> PASSED
solvable for a body holding nothing: 874 of 874 descents (a guard; a miss is a red test, not a number)
gates on the panel's wild descents: doors 0 sumps 3169 chutes 3223; re-derived as worked (DrowTier): 872 of 874 descents carry a door (the production walk reaches a door only where the ledger says a people cut the rung — see the Made population below; The Plat, decision 0647)
return differs from outbound: 159 of 866 gated descents (report only; follows from a chute by construction)
patterns by class and span (report only):
  LongLong CrossFloor the-chute: 96
  LongLong CrossFloor two-alternative-paths: 121
  LongLong SameFloor blocked-retreat: 224
  LongLong SameFloor two-alternative-paths: 241
  LongShort CrossFloor the-chute: 37
  LongShort SameFloor dangerous-route: 68
  LongShort SameFloor hidden-shortcut: 66
  ShortLong CrossFloor the-chute: 3090
  ShortLong SameFloor blocked-retreat: 3302
  ShortLong SameFloor the-sump: 3169
  ShortShort SameFloor patrol-path: 805

the Made population: 56 occupied columns, seated {"Shallows": 45, "Undercroft": 11}; tenancy: inhabited 53 abandoned 3
heart decile <= 5: 50 of 56 = 0.8929 (frozen floor 0.6667; FROM Made levels TO the Heart's depth decile) -> PASSED
doors on Made rungs: 38 of 56 = 0.6786 (frozen floor 0.50; FROM Made rungs TO rungs with a Needs(Key) gate; all-Found the same rungs carry 0) -> PASSED
nested realms smaller than their parent: 3678 of 6279 = 0.5858 (frozen band (0.50, 0.6667); FROM nested realms on every plan TO area < parent's) -> PASSED
landings on Made levels 31 of 56; sanctums holding a thing at genesis 5 of 56 (report only)
within-level components per level, every plan: {1: 1806, 2: 1780, 3: 728, 4: 56} (report only)

seed 7: 1681 descents

loop share: median 0.0893 (frozen floor 0.50) -> FALSIFIED
cycle membership: median 0.8421 (report only; added after Task 2 showed the entrance doorway is a bridge on ~40% of seeds)

density ordering (median anchored realms per level):
  LavaTube  WildCave 1.0000  DrowTier 2.0000
  Fracture  WildCave 2.0000  DrowTier 3.0000
  Karst     WildCave 3.0000  DrowTier 4.0000
  LavaTube < Fracture < Karst -> PASSED
  DrowTier > WildCave within kind -> PASSED
    LavaTube: DrowTier 2 > WildCave 1 -> PASSED
    Fracture: DrowTier 3 > WildCave 2 -> PASSED
    Karst: DrowTier 4 > WildCave 3 -> PASSED

cross-floor: 1595/1681 descents = 0.9488 (frozen floor 0.25) -> PASSED

semilattice overlap: median 0.2794 (report only)

gate yield: median 1.0000 (frozen floor 0.70; FROM realms with an admissible drawn row TO rows applied in full) -> PASSED
  skips: inadmissible 0 claimed 390 no-room 0 unsolvable 0 (report only)
detour cost: median 1.2069 over 1673 gated descents (frozen floor 1.10; default body's round trip gated / ungated) -> PASSED
solvable for a body holding nothing: 1681 of 1681 descents (a guard; a miss is a red test, not a number)
gates on the panel's wild descents: doors 0 sumps 5753 chutes 5439; re-derived as worked (DrowTier): 1676 of 1681 descents carry a door (the production walk reaches a door only where the ledger says a people cut the rung — see the Made population below; The Plat, decision 0647)
return differs from outbound: 289 of 1673 gated descents (report only; follows from a chute by construction)
patterns by class and span (report only):
  LongLong CrossFloor the-chute: 191
  LongLong CrossFloor two-alternative-paths: 155
  LongLong SameFloor blocked-retreat: 462
  LongLong SameFloor two-alternative-paths: 424
  LongShort CrossFloor the-chute: 70
  LongShort SameFloor dangerous-route: 122
  LongShort SameFloor hidden-shortcut: 134
  ShortLong CrossFloor the-chute: 5178
  ShortLong SameFloor blocked-retreat: 6102
  ShortLong SameFloor the-sump: 5753
  ShortShort SameFloor patrol-path: 1538

the Made population: 38 occupied columns, seated {"Shallows": 22, "Undercroft": 16}; tenancy: inhabited 38 abandoned 0
heart decile <= 5: 30 of 38 = 0.7895 (frozen floor 0.6667; FROM Made levels TO the Heart's depth decile) -> PASSED
doors on Made rungs: 32 of 38 = 0.8421 (frozen floor 0.50; FROM Made rungs TO rungs with a Needs(Key) gate; all-Found the same rungs carry 0) -> PASSED
nested realms smaller than their parent: 6107 of 10544 = 0.5792 (frozen band (0.50, 0.6667); FROM nested realms on every plan TO area < parent's) -> PASSED
landings on Made levels 16 of 38; sanctums holding a thing at genesis 3 of 38 (report only)
within-level components per level, every plan: {1: 3946, 2: 3366, 3: 1021, 4: 72} (report only)

seed 1234: 1266 descents

loop share: median 0.0794 (frozen floor 0.50) -> FALSIFIED
cycle membership: median 0.8413 (report only; added after Task 2 showed the entrance doorway is a bridge on ~40% of seeds)

density ordering (median anchored realms per level):
  LavaTube  WildCave 1.0000  DrowTier 2.0000
  Fracture  WildCave 2.0000  DrowTier 3.0000
  Karst     WildCave 3.0000  DrowTier 4.0000
  LavaTube < Fracture < Karst -> PASSED
  DrowTier > WildCave within kind -> PASSED
    LavaTube: DrowTier 2 > WildCave 1 -> PASSED
    Fracture: DrowTier 3 > WildCave 2 -> PASSED
    Karst: DrowTier 4 > WildCave 3 -> PASSED

cross-floor: 1200/1266 descents = 0.9479 (frozen floor 0.25) -> PASSED

semilattice overlap: median 0.2766 (report only)

gate yield: median 1.0000 (frozen floor 0.70; FROM realms with an admissible drawn row TO rows applied in full) -> PASSED
  skips: inadmissible 0 claimed 269 no-room 0 unsolvable 0 (report only)
detour cost: median 1.2000 over 1249 gated descents (frozen floor 1.10; default body's round trip gated / ungated) -> PASSED
solvable for a body holding nothing: 1266 of 1266 descents (a guard; a miss is a red test, not a number)
gates on the panel's wild descents: doors 0 sumps 4223 chutes 4123; re-derived as worked (DrowTier): 1263 of 1266 descents carry a door (the production walk reaches a door only where the ledger says a people cut the rung — see the Made population below; The Plat, decision 0647)
return differs from outbound: 199 of 1249 gated descents (report only; follows from a chute by construction)
patterns by class and span (report only):
  LongLong CrossFloor the-chute: 119
  LongLong CrossFloor two-alternative-paths: 146
  LongLong SameFloor blocked-retreat: 353
  LongLong SameFloor two-alternative-paths: 349
  LongShort CrossFloor the-chute: 57
  LongShort SameFloor dangerous-route: 87
  LongShort SameFloor hidden-shortcut: 92
  ShortLong CrossFloor the-chute: 3947
  ShortLong SameFloor blocked-retreat: 4740
  ShortLong SameFloor the-sump: 4223
  ShortShort SameFloor patrol-path: 1078

the Made population: 61 occupied columns, seated {"Shallows": 14, "Undercroft": 47}; tenancy: inhabited 57 abandoned 4
heart decile <= 5: 42 of 61 = 0.6885 (frozen floor 0.6667; FROM Made levels TO the Heart's depth decile) -> PASSED
doors on Made rungs: 44 of 61 = 0.7213 (frozen floor 0.50; FROM Made rungs TO rungs with a Needs(Key) gate; all-Found the same rungs carry 0) -> PASSED
nested realms smaller than their parent: 4544 of 7936 = 0.5726 (frozen band (0.50, 0.6667); FROM nested realms on every plan TO area < parent's) -> PASSED
landings on Made levels 13 of 61; sanctums holding a thing at genesis 3 of 61 (report only)
within-level components per level, every plan: {1: 2941, 2: 2505, 3: 816, 4: 68} (report only)
```
