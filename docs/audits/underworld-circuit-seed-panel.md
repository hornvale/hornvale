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

loop share: median 0.1233 (frozen floor 0.50) -> FALSIFIED
cycle membership: median 0.8548 (report only; added after Task 2 showed the entrance doorway is a bridge on ~40% of seeds)

density ordering (median anchored realms per level):
  LavaTube  WildCave 1.0000  DrowTier 2.0000
  Fracture  WildCave 2.0000  DrowTier 3.0000
  Karst     WildCave 3.0000  DrowTier 4.0000
  LavaTube < Fracture < Karst -> PASSED
  DrowTier > WildCave within kind -> PASSED
    LavaTube: DrowTier 2 > WildCave 1 -> PASSED
    Fracture: DrowTier 3 > WildCave 2 -> PASSED
    Karst: DrowTier 4 > WildCave 3 -> PASSED

cross-floor: 841/874 descents = 0.9622 (frozen floor 0.25) -> PASSED

semilattice overlap: median 0.3061 (report only)

seed 7: 1681 descents

loop share: median 0.1111 (frozen floor 0.50) -> FALSIFIED
cycle membership: median 0.8533 (report only; added after Task 2 showed the entrance doorway is a bridge on ~40% of seeds)

density ordering (median anchored realms per level):
  LavaTube  WildCave 1.0000  DrowTier 2.0000
  Fracture  WildCave 2.0000  DrowTier 3.0000
  Karst     WildCave 3.0000  DrowTier 4.0000
  LavaTube < Fracture < Karst -> PASSED
  DrowTier > WildCave within kind -> PASSED
    LavaTube: DrowTier 2 > WildCave 1 -> PASSED
    Fracture: DrowTier 3 > WildCave 2 -> PASSED
    Karst: DrowTier 4 > WildCave 3 -> PASSED

cross-floor: 1581/1681 descents = 0.9405 (frozen floor 0.25) -> PASSED

semilattice overlap: median 0.2857 (report only)

seed 1234: 1266 descents

loop share: median 0.1042 (frozen floor 0.50) -> FALSIFIED
cycle membership: median 0.8511 (report only; added after Task 2 showed the entrance doorway is a bridge on ~40% of seeds)

density ordering (median anchored realms per level):
  LavaTube  WildCave 1.0000  DrowTier 2.0000
  Fracture  WildCave 2.0000  DrowTier 3.0000
  Karst     WildCave 3.0000  DrowTier 4.0000
  LavaTube < Fracture < Karst -> PASSED
  DrowTier > WildCave within kind -> PASSED
    LavaTube: DrowTier 2 > WildCave 1 -> PASSED
    Fracture: DrowTier 3 > WildCave 2 -> PASSED
    Karst: DrowTier 4 > WildCave 3 -> PASSED

cross-floor: 1193/1266 descents = 0.9423 (frozen floor 0.25) -> PASSED

semilattice overlap: median 0.2812 (report only)
```
