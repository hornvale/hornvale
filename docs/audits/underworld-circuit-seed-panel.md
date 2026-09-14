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
seed 42: 1079 descents

loop share: median 0.1081 (frozen floor 0.50) -> FALSIFIED
cycle membership: median 0.8507 (report only; added after Task 2 showed the entrance doorway is a bridge on ~40% of seeds)

density ordering (median anchored realms per level):
  LavaTube  WildCave 1.0000  DrowTier 2.0000
  Fracture  WildCave 2.0000  DrowTier 3.0000
  Karst     WildCave 3.0000  DrowTier 4.0000
  LavaTube < Fracture < Karst -> PASSED
  DrowTier > WildCave within kind -> PASSED
    LavaTube: DrowTier 2 > WildCave 1 -> PASSED
    Fracture: DrowTier 3 > WildCave 2 -> PASSED
    Karst: DrowTier 4 > WildCave 3 -> PASSED

cross-floor: 1049/1079 descents = 0.9722 (frozen floor 0.25) -> PASSED

semilattice overlap: median 0.3099 (report only)

gate yield: median 1.0000 (frozen floor 0.70; FROM realms with an admissible drawn row TO rows applied in full) -> PASSED
  skips: inadmissible 0 claimed 257 no-room 0 unsolvable 2 (report only)
detour cost: median 1.2143 over 1073 gated descents (frozen floor 1.10; default body's round trip gated / ungated) -> PASSED
solvable for a body holding nothing: 1079 of 1079 descents (a guard; a miss is a red test, not a number)
gates on the panel's wild descents: doors 0 sumps 3979 chutes 4262; re-derived as worked (DrowTier): 1077 of 1079 descents carry a door (the production walk reaches a door only where the ledger says a people cut the rung — see the Made population below; The Plat, decision 0647)
return differs from outbound: 198 of 1073 gated descents (report only; follows from a chute by construction)
patterns by class and span (report only):
  LongLong CrossFloor the-chute: 128
  LongLong CrossFloor two-alternative-paths: 145
  LongLong SameFloor blocked-retreat: 288
  LongLong SameFloor two-alternative-paths: 323
  LongShort CrossFloor the-chute: 57
  LongShort SameFloor dangerous-route: 91
  LongShort SameFloor hidden-shortcut: 85
  ShortLong CrossFloor the-chute: 4077
  ShortLong SameFloor blocked-retreat: 4225
  ShortLong SameFloor the-sump: 3979
  ShortShort SameFloor patrol-path: 1034

the Made population: 131 occupied columns, seated {"Shallows": 48, "Undercroft": 83}; tenancy: inhabited 124 abandoned 7
heart decile <= 5: 101 of 131 = 0.7710 (frozen floor 0.6667; FROM Made levels TO the Heart's depth decile) -> PASSED
doors on Made rungs: 100 of 131 = 0.7634 (frozen floor 0.50; FROM Made rungs TO rungs with a Needs(Key) gate; all-Found the same rungs carry 0) -> PASSED
nested realms smaller than their parent: 4908 of 8338 = 0.5886 (frozen band (0.50, 0.6667); FROM nested realms on every plan TO area < parent's) -> PASSED
landings on Made levels 30 of 131; sanctums holding a thing at genesis 14 of 131 (report only)
within-level components per level, every plan: {1: 2055, 2: 2234, 3: 999, 4: 106, 5: 1} (report only)

seed 7: 1932 descents

loop share: median 0.0909 (frozen floor 0.50) -> FALSIFIED
cycle membership: median 0.8491 (report only; added after Task 2 showed the entrance doorway is a bridge on ~40% of seeds)

density ordering (median anchored realms per level):
  LavaTube  WildCave 1.0000  DrowTier 2.0000
  Fracture  WildCave 2.0000  DrowTier 3.0000
  Karst     WildCave 3.0000  DrowTier 4.0000
  LavaTube < Fracture < Karst -> PASSED
  DrowTier > WildCave within kind -> PASSED
    LavaTube: DrowTier 2 > WildCave 1 -> PASSED
    Fracture: DrowTier 3 > WildCave 2 -> PASSED
    Karst: DrowTier 4 > WildCave 3 -> PASSED

cross-floor: 1855/1932 descents = 0.9601 (frozen floor 0.25) -> PASSED

semilattice overlap: median 0.2927 (report only)

gate yield: median 1.0000 (frozen floor 0.70; FROM realms with an admissible drawn row TO rows applied in full) -> PASSED
  skips: inadmissible 0 claimed 451 no-room 0 unsolvable 1 (report only)
detour cost: median 1.2105 over 1924 gated descents (frozen floor 1.10; default body's round trip gated / ungated) -> PASSED
solvable for a body holding nothing: 1932 of 1932 descents (a guard; a miss is a red test, not a number)
gates on the panel's wild descents: doors 0 sumps 6847 chutes 6821; re-derived as worked (DrowTier): 1928 of 1932 descents carry a door (the production walk reaches a door only where the ledger says a people cut the rung — see the Made population below; The Plat, decision 0647)
return differs from outbound: 356 of 1924 gated descents (report only; follows from a chute by construction)
patterns by class and span (report only):
  LongLong CrossFloor the-chute: 211
  LongLong CrossFloor two-alternative-paths: 211
  LongLong SameFloor blocked-retreat: 545
  LongLong SameFloor two-alternative-paths: 511
  LongShort CrossFloor the-chute: 93
  LongShort SameFloor dangerous-route: 148
  LongShort SameFloor hidden-shortcut: 161
  ShortLong CrossFloor the-chute: 6517
  ShortLong SameFloor blocked-retreat: 7303
  ShortLong SameFloor the-sump: 6847
  ShortShort SameFloor patrol-path: 1821

the Made population: 36 occupied columns, seated {"Shallows": 20, "Undercroft": 16}; tenancy: inhabited 32 abandoned 4
heart decile <= 5: 30 of 36 = 0.8333 (frozen floor 0.6667; FROM Made levels TO the Heart's depth decile) -> PASSED
doors on Made rungs: 31 of 36 = 0.8611 (frozen floor 0.50; FROM Made rungs TO rungs with a Needs(Key) gate; all-Found the same rungs carry 0) -> PASSED
nested realms smaller than their parent: 7718 of 13198 = 0.5848 (frozen band (0.50, 0.6667); FROM nested realms on every plan TO area < parent's) -> PASSED
landings on Made levels 14 of 36; sanctums holding a thing at genesis 5 of 36 (report only)
within-level components per level, every plan: {1: 4217, 2: 3944, 3: 1392, 4: 107} (report only)

seed 1234: 1632 descents

loop share: median 0.0923 (frozen floor 0.50) -> FALSIFIED
cycle membership: median 0.8431 (report only; added after Task 2 showed the entrance doorway is a bridge on ~40% of seeds)

density ordering (median anchored realms per level):
  LavaTube  WildCave 1.0000  DrowTier 2.0000
  Fracture  WildCave 2.0000  DrowTier 3.0000
  Karst     WildCave 3.0000  DrowTier 4.0000
  LavaTube < Fracture < Karst -> PASSED
  DrowTier > WildCave within kind -> PASSED
    LavaTube: DrowTier 2 > WildCave 1 -> PASSED
    Fracture: DrowTier 3 > WildCave 2 -> PASSED
    Karst: DrowTier 4 > WildCave 3 -> PASSED

cross-floor: 1572/1632 descents = 0.9632 (frozen floor 0.25) -> PASSED

semilattice overlap: median 0.3000 (report only)

gate yield: median 1.0000 (frozen floor 0.70; FROM realms with an admissible drawn row TO rows applied in full) -> PASSED
  skips: inadmissible 0 claimed 368 no-room 0 unsolvable 0 (report only)
detour cost: median 1.2069 over 1615 gated descents (frozen floor 1.10; default body's round trip gated / ungated) -> PASSED
solvable for a body holding nothing: 1632 of 1632 descents (a guard; a miss is a red test, not a number)
gates on the panel's wild descents: doors 0 sumps 5848 chutes 6011; re-derived as worked (DrowTier): 1629 of 1632 descents carry a door (the production walk reaches a door only where the ledger says a people cut the rung — see the Made population below; The Plat, decision 0647)
return differs from outbound: 260 of 1615 gated descents (report only; follows from a chute by construction)
patterns by class and span (report only):
  LongLong CrossFloor the-chute: 219
  LongLong CrossFloor two-alternative-paths: 213
  LongLong SameFloor blocked-retreat: 431
  LongLong SameFloor two-alternative-paths: 461
  LongShort CrossFloor the-chute: 86
  LongShort SameFloor dangerous-route: 145
  LongShort SameFloor hidden-shortcut: 133
  ShortLong CrossFloor the-chute: 5706
  ShortLong SameFloor blocked-retreat: 6265
  ShortLong SameFloor the-sump: 5848
  ShortShort SameFloor patrol-path: 1481

the Made population: 116 occupied columns, seated {"Shallows": 28, "Undercroft": 86, "Underdeep": 2}; tenancy: inhabited 106 abandoned 10
heart decile <= 5: 92 of 116 = 0.7931 (frozen floor 0.6667; FROM Made levels TO the Heart's depth decile) -> PASSED
doors on Made rungs: 86 of 116 = 0.7414 (frozen floor 0.50; FROM Made rungs TO rungs with a Needs(Key) gate; all-Found the same rungs carry 0) -> PASSED
nested realms smaller than their parent: 6699 of 11561 = 0.5794 (frozen band (0.50, 0.6667); FROM nested realms on every plan TO area < parent's) -> PASSED
landings on Made levels 17 of 116; sanctums holding a thing at genesis 13 of 116 (report only)
within-level components per level, every plan: {1: 3412, 2: 3301, 3: 1315, 4: 132} (report only)
```
