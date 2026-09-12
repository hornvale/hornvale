# The Underworld of Seeds 42, 7 and 1234

The chamber lattice as three worlds actually realize it: how many cave
systems each has, how many chambers exist beneath them, how those chambers
distribute over the delve ladder and over the rock they sit in, and then --
run by run -- the first three cave systems of each world.

A chamber is never stored. Existence and content are pure functions of an
address, so this page is a *witness*, not a record: every line is re-derived
from the seed on each regeneration, and a change to the derivation key, to
the existence gate, to a run's drawn length, to a chamber's content or
to the depth the rock grants a cave moves bytes here.

`reachable` is the count a player would actually experience: chambers
reachable from an entrance by the passage graph, whose vertical axis is
the descent sequence (a run's drawn length is its sojourn; past it, the
next band's floor 0). That sequence is the number, not a rounding error.

The `key` column is a DISPLAY FORMATTER of that run's floor 0's address,
not a derivation key (The Drift, spec amendment A.6): Task 1 deleted the
chamber existence draw, chamber_key's only production caller, so nothing in a
shipped world derives from it any more. The real derivation keys are
`levels_in_branch`'s RUN_FLOORS leg and the three per-branch legs in
`crate::character` -- this column still witnesses the address's SPELLING (the
rung name, the field order), which is what makes it worth printing.
Each run shows one glyph per floor the LATTICE admits, never per floor the
run drew: a `#` exists, a `.` sits deeper than the cave's budget
reaches, and a `_` is past that run's
own drawn length. Bounding the row by the drawn length instead is what made
an earlier version of this page unable to see either floor gate at all.

The `junctions` line counts the links between DIFFERENT cave systems that
`junctions_at` derives at each shared delve band -- derived, not drawn: it
consumes no stream leg OF ITS OWN, so a shortcut is a fact about the geology
rather than a die roll on top of an epoch. (It does travel the legs the facts
it reads already have -- a branch-count draw here, a branch character there --
and an earlier version of this page dropped that qualifier and asserted the
falsehood that it consumes no stream at all.) A link is an EDGE, counted once
rather than once per endpoint; `largest network` is the largest component
within a SINGLE band, because a junction never crosses one -- so systems
joined only at the Undercroft and systems joined only at the Nadir are two
networks, not one, and unioning them would name a component nothing can
walk.

```text
seed 42
  derivation      chamber/v3 over chamber/run-floors/v2
  lattice         4 branches per system, 5 bands, 20 levels admitted per run
  entrances       1626 drawn across 874 systems, 499 multi-entrance (chamber/entrance-count/v2)
  cave systems    874  (ocean-vertex caves skipped: 0)
  floors drawn    113799
  chambers        30537
  reachable       30537 from 1626 open entrances
  junctions       4165 links across 1575 system pairs; largest network 119 systems at one band (MAP-underworld-shortcut)
  by band         undercroft:4148  shallows:7915  deeps:14072  underdeep:3376  nadir:1026  
  by rock         regolith:1942  cover:715  basement:27880  roots:0  underneath:0  off-ladder:0
  by origin       found:30451  made:86
  past run length 0   (chambers beyond their run's drawn floors)
  past branch cnt 0   (chambers beyond their system's drawn branches)

  the first three cave systems, run by run
  (key = the floor-0 address's spelling, not a derivation key; # exists, . refused, _ past the run's drawn floors, ~ past the system's drawn branch count)

  vertex 30 — fracture cave, reach 2145.7358 m, gradient 26.007865 K/km
    30/0/undercroft/0            regolith     3 floors  ###_________________
    30/0/shallows/0              cover        5 floors  #####_______________
    30/0/deeps/0                 basement    14 floors  ##############______
    30/0/underdeep/0             basement     6 floors  ######______________
    30/0/nadir/0                 basement     5 floors  #####_______________
    30/1/undercroft/0            regolith     4 floors  ####________________
    30/1/shallows/0              -            7 floors  ~~~~~~~_____________
    30/1/deeps/0                 basement    11 floors  ###########_________
    30/1/underdeep/0             -            9 floors  ~~~~~~~~~___________
    30/1/nadir/0                 -            5 floors  ~~~~~_______________
    30/2/undercroft/0            regolith     1 floors  #___________________
    30/2/shallows/0              -            8 floors  ~~~~~~~~____________
    30/2/deeps/0                 basement    18 floors  ##################__
    30/2/underdeep/0             -            6 floors  ~~~~~~______________
    30/2/nadir/0                 -            4 floors  ~~~~________________
    30/3/undercroft/0            -            4 floors  ~~~~________________
    30/3/shallows/0              -            8 floors  ~~~~~~~~____________
    30/3/deeps/0                 basement    18 floors  ##################__
    30/3/underdeep/0             -           10 floors  ~~~~~~~~~~__________
    30/3/nadir/0                 -            5 floors  ~~~~~_______________

  vertex 111 — karst cave, reach 252.0545 m, gradient 24.69077 K/km
    111/0/undercroft/0           basement     4 floors  ####________________
    111/0/shallows/0             basement     3 floors  ###_________________
    111/0/deeps/0                -           13 floors  ............._______
    111/0/underdeep/0            -            7 floors  ......._____________
    111/0/nadir/0                -            4 floors  ....________________
    111/1/undercroft/0           basement     1 floors  #___________________
    111/1/shallows/0             -            6 floors  ~~~~~~______________
    111/1/deeps/0                -           14 floors  ~~~~~~~~~~~~~~______
    111/1/underdeep/0            -            5 floors  ~~~~~_______________
    111/1/nadir/0                -            3 floors  ~~~_________________
    111/2/undercroft/0           basement     5 floors  #####_______________
    111/2/shallows/0             -            4 floors  ~~~~________________
    111/2/deeps/0                -           16 floors  ~~~~~~~~~~~~~~~~____
    111/2/underdeep/0            -            8 floors  ~~~~~~~~____________
    111/2/nadir/0                -            3 floors  ~~~_________________
    111/3/undercroft/0           -            1 floors  ~___________________
    111/3/shallows/0             -            4 floors  ~~~~________________
    111/3/deeps/0                -           15 floors  ~~~~~~~~~~~~~~~_____
    111/3/underdeep/0            -            7 floors  ~~~~~~~_____________
    111/3/nadir/0                -            1 floors  ~___________________

  vertex 282 — karst cave, reach 483.46851 m, gradient 22.399271 K/km
    282/0/undercroft/0           basement     3 floors  ###_________________
    282/0/shallows/0             basement     6 floors  ######______________
    282/0/deeps/0                basement    13 floors  #############_______
    282/0/underdeep/0            -            8 floors  ........____________
    282/0/nadir/0                -            3 floors  ..._________________
    282/1/undercroft/0           -            2 floors  ~~__________________
    282/1/shallows/0             basement     9 floors  #########___________
    282/1/deeps/0                -           17 floors  ~~~~~~~~~~~~~~~~~___
    282/1/underdeep/0            -           10 floors  ..........__________
    282/1/nadir/0                -            5 floors  ~~~~~_______________
    282/2/undercroft/0           -            4 floors  ~~~~________________
    282/2/shallows/0             -            3 floors  ~~~_________________
    282/2/deeps/0                -           20 floors  ~~~~~~~~~~~~~~~~~~~~
    282/2/underdeep/0            -            5 floors  ....._______________
    282/2/nadir/0                -            3 floors  ~~~_________________
    282/3/undercroft/0           -            1 floors  ~___________________
    282/3/shallows/0             -            8 floors  ~~~~~~~~____________
    282/3/deeps/0                -           11 floors  ~~~~~~~~~~~_________
    282/3/underdeep/0            -            7 floors  ~~~~~~~_____________
    282/3/nadir/0                -            2 floors  ~~__________________

seed 7
  derivation      chamber/v3 over chamber/run-floors/v2
  lattice         4 branches per system, 5 bands, 20 levels admitted per run
  entrances       3177 drawn across 1681 systems, 962 multi-entrance (chamber/entrance-count/v2)
  cave systems    1681  (ocean-vertex caves skipped: 0)
  floors drawn    219277
  chambers        59227
  reachable       59227 from 3177 open entrances
  junctions       8253 links across 3007 system pairs; largest network 118 systems at one band (MAP-underworld-shortcut)
  by band         undercroft:7923  shallows:16455  deeps:20917  underdeep:10490  nadir:3442  
  by rock         regolith:4306  cover:3540  basement:51381  roots:0  underneath:0  off-ladder:0
  by origin       found:59167  made:60
  past run length 0   (chambers beyond their run's drawn floors)
  past branch cnt 0   (chambers beyond their system's drawn branches)

  the first three cave systems, run by run
  (key = the floor-0 address's spelling, not a derivation key; # exists, . refused, _ past the run's drawn floors, ~ past the system's drawn branch count)

  vertex 52 — karst cave, reach 249.24025 m, gradient 24.917192 K/km
    52/0/undercroft/0            regolith     5 floors  #####_______________
    52/0/shallows/0              cover        3 floors  ###_________________
    52/0/deeps/0                 -           19 floors  ..................._
    52/0/underdeep/0             -            5 floors  ....._______________
    52/0/nadir/0                 -            3 floors  ..._________________
    52/1/undercroft/0            regolith     4 floors  ####________________
    52/1/shallows/0              cover        6 floors  ######______________
    52/1/deeps/0                 -            6 floors  ......______________
    52/1/underdeep/0             -           10 floors  ..........__________
    52/1/nadir/0                 -            2 floors  ..__________________
    52/2/undercroft/0            regolith     4 floors  ####________________
    52/2/shallows/0              -            8 floors  ~~~~~~~~____________
    52/2/deeps/0                 -           18 floors  ..................__
    52/2/underdeep/0             -            7 floors  ~~~~~~~_____________
    52/2/nadir/0                 -            2 floors  ..__________________
    52/3/undercroft/0            -            1 floors  ~___________________
    52/3/shallows/0              -            6 floors  ~~~~~~______________
    52/3/deeps/0                 -           14 floors  ~~~~~~~~~~~~~~______
    52/3/underdeep/0             -            5 floors  ~~~~~_______________
    52/3/nadir/0                 -            2 floors  ..__________________

  vertex 70 — fracture cave, reach 2230.3734 m, gradient 24.795713 K/km
    70/0/undercroft/0            regolith     2 floors  ##__________________
    70/0/shallows/0              basement     5 floors  #####_______________
    70/0/deeps/0                 basement     8 floors  ########____________
    70/0/underdeep/0             basement     7 floors  #######_____________
    70/0/nadir/0                 basement     3 floors  ###_________________
    70/1/undercroft/0            -            3 floors  ~~~_________________
    70/1/shallows/0              -            9 floors  ~~~~~~~~~___________
    70/1/deeps/0                 basement    18 floors  ##################__
    70/1/underdeep/0             -            8 floors  ~~~~~~~~____________
    70/1/nadir/0                 -            2 floors  ~~__________________
    70/2/undercroft/0            -            3 floors  ~~~_________________
    70/2/shallows/0              -            7 floors  ~~~~~~~_____________
    70/2/deeps/0                 basement     7 floors  #######_____________
    70/2/underdeep/0             -            5 floors  ~~~~~_______________
    70/2/nadir/0                 -            4 floors  ~~~~________________
    70/3/undercroft/0            -            4 floors  ~~~~________________
    70/3/shallows/0              -            5 floors  ~~~~~_______________
    70/3/deeps/0                 basement    19 floors  ###################_
    70/3/underdeep/0             -            9 floors  ~~~~~~~~~___________
    70/3/nadir/0                 -            3 floors  ~~~_________________

  vertex 92 — karst cave, reach 1409.9429 m, gradient 26.976856 K/km
    92/0/undercroft/0            regolith     5 floors  #####_______________
    92/0/shallows/0              cover        7 floors  #######_____________
    92/0/deeps/0                 basement     5 floors  #####_______________
    92/0/underdeep/0             basement    10 floors  ##########__________
    92/0/nadir/0                 -            4 floors  ....________________
    92/1/undercroft/0            regolith     2 floors  ##__________________
    92/1/shallows/0              -            5 floors  ~~~~~_______________
    92/1/deeps/0                 -            6 floors  ~~~~~~______________
    92/1/underdeep/0             -           10 floors  ~~~~~~~~~~__________
    92/1/nadir/0                 -            5 floors  ....._______________
    92/2/undercroft/0            -            2 floors  ~~__________________
    92/2/shallows/0              -            5 floors  ~~~~~_______________
    92/2/deeps/0                 -           17 floors  ~~~~~~~~~~~~~~~~~___
    92/2/underdeep/0             -            5 floors  ~~~~~_______________
    92/2/nadir/0                 -            2 floors  ~~__________________
    92/3/undercroft/0            -            1 floors  ~___________________
    92/3/shallows/0              -            9 floors  ~~~~~~~~~___________
    92/3/deeps/0                 -           19 floors  ~~~~~~~~~~~~~~~~~~~_
    92/3/underdeep/0             -            6 floors  ~~~~~~______________
    92/3/nadir/0                 -            3 floors  ~~~_________________

seed 1234
  derivation      chamber/v3 over chamber/run-floors/v2
  lattice         4 branches per system, 5 bands, 20 levels admitted per run
  entrances       2331 drawn across 1266 systems, 733 multi-entrance (chamber/entrance-count/v2)
  cave systems    1266  (ocean-vertex caves skipped: 0)
  floors drawn    164033
  chambers        48294
  reachable       48294 from 2331 open entrances
  junctions       6937 links across 2348 system pairs; largest network 128 systems at one band (MAP-underworld-shortcut)
  by band         undercroft:5910  shallows:12104  deeps:19940  underdeep:7759  nadir:2581  
  by rock         regolith:2902  cover:2650  basement:42742  roots:0  underneath:0  off-ladder:0
  by origin       found:48135  made:159
  past run length 0   (chambers beyond their run's drawn floors)
  past branch cnt 0   (chambers beyond their system's drawn branches)

  the first three cave systems, run by run
  (key = the floor-0 address's spelling, not a derivation key; # exists, . refused, _ past the run's drawn floors, ~ past the system's drawn branch count)

  vertex 18 — fracture cave, reach 2694.0137 m, gradient 21.180804 K/km
    18/0/undercroft/0            regolith     1 floors  #___________________
    18/0/shallows/0              basement    10 floors  ##########__________
    18/0/deeps/0                 basement     8 floors  ########____________
    18/0/underdeep/0             basement     8 floors  ########____________
    18/0/nadir/0                 basement     4 floors  ####________________
    18/1/undercroft/0            regolith     4 floors  ####________________
    18/1/shallows/0              -            9 floors  ~~~~~~~~~___________
    18/1/deeps/0                 -            9 floors  ~~~~~~~~~___________
    18/1/underdeep/0             -            9 floors  ~~~~~~~~~___________
    18/1/nadir/0                 basement     1 floors  #___________________
    18/2/undercroft/0            -            3 floors  ~~~_________________
    18/2/shallows/0              -            3 floors  ~~~_________________
    18/2/deeps/0                 -            7 floors  ~~~~~~~_____________
    18/2/underdeep/0             -            6 floors  ~~~~~~______________
    18/2/nadir/0                 -            4 floors  ~~~~________________
    18/3/undercroft/0            -            4 floors  ~~~~________________
    18/3/shallows/0              -            6 floors  ~~~~~~______________
    18/3/deeps/0                 -            9 floors  ~~~~~~~~~___________
    18/3/underdeep/0             -            7 floors  ~~~~~~~_____________
    18/3/nadir/0                 -            5 floors  ~~~~~_______________

  vertex 19 — fracture cave, reach 1699.8077 m, gradient 22.357908 K/km
    19/0/undercroft/0            regolith     2 floors  ##__________________
    19/0/shallows/0              cover       10 floors  ##########__________
    19/0/deeps/0                 cover       16 floors  ################____
    19/0/underdeep/0             basement     6 floors  ######______________
    19/0/nadir/0                 -            3 floors  ..._________________
    19/1/undercroft/0            -            3 floors  ~~~_________________
    19/1/shallows/0              -           10 floors  ~~~~~~~~~~__________
    19/1/deeps/0                 -            6 floors  ~~~~~~______________
    19/1/underdeep/0             basement     7 floors  #######_____________
    19/1/nadir/0                 -            3 floors  ~~~_________________
    19/2/undercroft/0            -            5 floors  ~~~~~_______________
    19/2/shallows/0              -            4 floors  ~~~~________________
    19/2/deeps/0                 -           20 floors  ~~~~~~~~~~~~~~~~~~~~
    19/2/underdeep/0             -            6 floors  ~~~~~~______________
    19/2/nadir/0                 -            5 floors  ~~~~~_______________
    19/3/undercroft/0            -            1 floors  ~___________________
    19/3/shallows/0              -            6 floors  ~~~~~~______________
    19/3/deeps/0                 -           12 floors  ~~~~~~~~~~~~________
    19/3/underdeep/0             -            9 floors  ~~~~~~~~~___________
    19/3/nadir/0                 -            5 floors  ~~~~~_______________

  vertex 49 — fracture cave, reach 2039.1218 m, gradient 26.05835 K/km
    49/0/undercroft/0            basement     2 floors  ##__________________
    49/0/shallows/0              basement     4 floors  ####________________
    49/0/deeps/0                 basement    12 floors  ############________
    49/0/underdeep/0             basement     6 floors  ######______________
    49/0/nadir/0                 basement     2 floors  ##__________________
    49/1/undercroft/0            basement     3 floors  ###_________________
    49/1/shallows/0              -            8 floors  ~~~~~~~~____________
    49/1/deeps/0                 -           11 floors  ~~~~~~~~~~~_________
    49/1/underdeep/0             basement     9 floors  #########___________
    49/1/nadir/0                 -            5 floors  ~~~~~_______________
    49/2/undercroft/0            basement     1 floors  #___________________
    49/2/shallows/0              -            7 floors  ~~~~~~~_____________
    49/2/deeps/0                 -            7 floors  ~~~~~~~_____________
    49/2/underdeep/0             -            9 floors  ~~~~~~~~~___________
    49/2/nadir/0                 -            4 floors  ~~~~________________
    49/3/undercroft/0            -            4 floors  ~~~~________________
    49/3/shallows/0              -            5 floors  ~~~~~_______________
    49/3/deeps/0                 -           13 floors  ~~~~~~~~~~~~~_______
    49/3/underdeep/0             -           10 floors  ~~~~~~~~~~__________
    49/3/nadir/0                 -            5 floors  ~~~~~_______________
```
