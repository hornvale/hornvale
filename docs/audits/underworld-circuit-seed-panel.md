# The Circuits of Seeds 42, 7 and 1234

What this page witnesses: for each seed's every cave-bearing,
non-ocean vertex, a descent plan is grown (spec §3) and the four
preregistered readouts of spec §4 are measured against it, then
reduced to a panel median. A descent plan is never stored -- it is a
pure function of `(seed, vertex, rungs, kind, character)`, so this page
is a witness, not a record: every number here is re-derived from the
seed on each regeneration.

The verdict words PASSED / FALSIFIED are frozen by spec §4, decided
before this code existed, and nothing here is tuned to reach one -- a
FALSIFIED verdict is a finding the campaign publishes, not a bug to fix
by moving the threshold. A comparison that cannot be made for a seed
(no cave of some kind exists there) prints NOT MEASURABLE rather than a
vacuous PASSED.

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
  skips: inadmissible 0 claimed 198 no-room 0 unsolvable 1 (report only)
detour cost: median 1.2143 over 866 gated descents (frozen floor 1.10; default body's round trip gated / ungated) -> PASSED
solvable for a body holding nothing: 874 of 874 descents (a guard; a miss is a red test, not a number)
gates: doors 0 sumps 3169 chutes 3224; worked descents with a door 872 of 874 (the production walk reaches none yet, spec §1)
return differs from outbound: 159 of 866 gated descents (report only; follows from a chute by construction)
patterns by class and span (report only):
  LongLong CrossFloor the-chute: 96
  LongLong CrossFloor two-alternative-paths: 121
  LongLong SameFloor blocked-retreat: 224
  LongLong SameFloor two-alternative-paths: 241
  LongShort CrossFloor the-chute: 37
  LongShort SameFloor dangerous-route: 68
  LongShort SameFloor hidden-shortcut: 66
  ShortLong CrossFloor the-chute: 3091
  ShortLong SameFloor blocked-retreat: 3302
  ShortLong SameFloor the-sump: 3169
  ShortShort SameFloor patrol-path: 805

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
gates: doors 0 sumps 5753 chutes 5439; worked descents with a door 1676 of 1681 (the production walk reaches none yet, spec §1)
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
gates: doors 0 sumps 4223 chutes 4123; worked descents with a door 1263 of 1266 (the production walk reaches none yet, spec §1)
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
```
