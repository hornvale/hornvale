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
  entrances       2022 drawn across 1079 systems, 620 multi-entrance (chamber/entrance-count/v2)
  cave systems    1079  (ocean-vertex caves skipped: 0)
  floors drawn    140782
  chambers        27545
  reachable       27545 from 2022 open entrances
  junctions       4753 links across 2243 system pairs; largest network 150 systems at one band (MAP-underworld-shortcut)
  by band         undercroft:5234  shallows:10295  deeps:9195  underdeep:2821  nadir:0  
  by rock         regolith:2292  cover:581  basement:24672  roots:0  underneath:0  off-ladder:0
  by origin       found:27279  made:266
  past run length 0   (chambers beyond their run's drawn floors)
  past branch cnt 0   (chambers beyond their system's drawn branches)

  the first three cave systems, run by run
  (key = the floor-0 address's spelling, not a derivation key; # exists, . refused, _ past the run's drawn floors, ~ past the system's drawn branch count)

  vertex 14 — karst cave, reach 237.70004 m, gradient 23.444 K/km
    14/0/undercroft/0            regolith     1 floors  #___________________
    14/0/shallows/0              basement    10 floors  ##########__________
    14/0/deeps/0                 -           18 floors  ..................__
    14/0/underdeep/0             -            5 floors  ....._______________
    14/0/nadir/0                 -            2 floors  ..__________________
    14/1/undercroft/0            regolith     3 floors  ###_________________
    14/1/shallows/0              basement     9 floors  #########___________
    14/1/deeps/0                 -            7 floors  ......._____________
    14/1/underdeep/0             -            9 floors  ~~~~~~~~~___________
    14/1/nadir/0                 -            5 floors  ~~~~~_______________
    14/2/undercroft/0            -            3 floors  ~~~_________________
    14/2/shallows/0              -            7 floors  ~~~~~~~_____________
    14/2/deeps/0                 -            5 floors  ....._______________
    14/2/underdeep/0             -            7 floors  ~~~~~~~_____________
    14/2/nadir/0                 -            2 floors  ~~__________________
    14/3/undercroft/0            -            1 floors  ~___________________
    14/3/shallows/0              -            8 floors  ~~~~~~~~____________
    14/3/deeps/0                 -           14 floors  ~~~~~~~~~~~~~~______
    14/3/underdeep/0             -            5 floors  ~~~~~_______________
    14/3/nadir/0                 -            1 floors  ~___________________

  vertex 30 — fracture cave, reach 1484.4865 m, gradient 26.007865 K/km
    30/0/undercroft/0            regolith     3 floors  ###_________________
    30/0/shallows/0              cover        5 floors  #####_______________
    30/0/deeps/0                 basement    14 floors  ##############______
    30/0/underdeep/0             basement     6 floors  ######______________
    30/0/nadir/0                 -            5 floors  ....._______________
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

  vertex 46 — karst cave, reach 315.07616 m, gradient 24.297621 K/km
    46/0/undercroft/0            regolith     3 floors  ###_________________
    46/0/shallows/0              basement     8 floors  ########____________
    46/0/deeps/0                 -           17 floors  .................___
    46/0/underdeep/0             -            7 floors  ......._____________
    46/0/nadir/0                 -            3 floors  ..._________________
    46/1/undercroft/0            -            3 floors  ~~~_________________
    46/1/shallows/0              -            7 floors  ~~~~~~~_____________
    46/1/deeps/0                 -            5 floors  ~~~~~_______________
    46/1/underdeep/0             -            9 floors  .........___________
    46/1/nadir/0                 -            1 floors  ~___________________
    46/2/undercroft/0            -            4 floors  ~~~~________________
    46/2/shallows/0              -            3 floors  ~~~_________________
    46/2/deeps/0                 -           10 floors  ~~~~~~~~~~__________
    46/2/underdeep/0             -            6 floors  ~~~~~~______________
    46/2/nadir/0                 -            4 floors  ~~~~________________
    46/3/undercroft/0            -            5 floors  ~~~~~_______________
    46/3/shallows/0              -            4 floors  ~~~~________________
    46/3/deeps/0                 -           20 floors  ~~~~~~~~~~~~~~~~~~~~
    46/3/underdeep/0             -            8 floors  ~~~~~~~~____________
    46/3/nadir/0                 -            4 floors  ~~~~________________

seed 7
  derivation      chamber/v3 over chamber/run-floors/v2
  lattice         4 branches per system, 5 bands, 20 levels admitted per run
  entrances       3624 drawn across 1932 systems, 1101 multi-entrance (chamber/entrance-count/v2)
  cave systems    1932  (ocean-vertex caves skipped: 0)
  floors drawn    251815
  chambers        54681
  reachable       54681 from 3624 open entrances
  junctions       9110 links across 3967 system pairs; largest network 171 systems at one band (MAP-underworld-shortcut)
  by band         undercroft:9063  shallows:19372  deeps:17774  underdeep:8472  nadir:0  
  by rock         regolith:4582  cover:3227  basement:46872  roots:0  underneath:0  off-ladder:0
  by origin       found:54623  made:58
  past run length 0   (chambers beyond their run's drawn floors)
  past branch cnt 0   (chambers beyond their system's drawn branches)

  the first three cave systems, run by run
  (key = the floor-0 address's spelling, not a derivation key; # exists, . refused, _ past the run's drawn floors, ~ past the system's drawn branch count)

  vertex 52 — karst cave, reach 189.13732 m, gradient 24.917192 K/km
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

  vertex 70 — fracture cave, reach 1543.0414 m, gradient 24.795713 K/km
    70/0/undercroft/0            regolith     2 floors  ##__________________
    70/0/shallows/0              basement     5 floors  #####_______________
    70/0/deeps/0                 basement     8 floors  ########____________
    70/0/underdeep/0             basement     7 floors  #######_____________
    70/0/nadir/0                 -            3 floors  ..._________________
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

  vertex 92 — karst cave, reach 750.39796 m, gradient 26.976856 K/km
    92/0/undercroft/0            regolith     5 floors  #####_______________
    92/0/shallows/0              cover        7 floors  #######_____________
    92/0/deeps/0                 basement     5 floors  #####_______________
    92/0/underdeep/0             -           10 floors  ..........__________
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
  entrances       3088 drawn across 1632 systems, 962 multi-entrance (chamber/entrance-count/v2)
  cave systems    1632  (ocean-vertex caves skipped: 0)
  floors drawn    211274
  chambers        46932
  reachable       46932 from 3088 open entrances
  junctions       8061 links across 3490 system pairs; largest network 170 systems at one band (MAP-underworld-shortcut)
  by band         undercroft:7779  shallows:16748  deeps:16440  underdeep:5965  nadir:0  
  by rock         regolith:3513  cover:2557  basement:40862  roots:0  underneath:0  off-ladder:0
  by origin       found:46732  made:200
  past run length 0   (chambers beyond their run's drawn floors)
  past branch cnt 0   (chambers beyond their system's drawn branches)

  the first three cave systems, run by run
  (key = the floor-0 address's spelling, not a derivation key; # exists, . refused, _ past the run's drawn floors, ~ past the system's drawn branch count)

  vertex 18 — fracture cave, reach 1863.8021 m, gradient 21.180804 K/km
    18/0/undercroft/0            regolith     1 floors  #___________________
    18/0/shallows/0              basement    10 floors  ##########__________
    18/0/deeps/0                 basement     8 floors  ########____________
    18/0/underdeep/0             basement     8 floors  ########____________
    18/0/nadir/0                 -            4 floors  ....________________
    18/1/undercroft/0            regolith     4 floors  ####________________
    18/1/shallows/0              -            9 floors  ~~~~~~~~~___________
    18/1/deeps/0                 -            9 floors  ~~~~~~~~~___________
    18/1/underdeep/0             -            9 floors  ~~~~~~~~~___________
    18/1/nadir/0                 -            1 floors  .___________________
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

  vertex 19 — karst cave, reach 487.47221 m, gradient 22.357908 K/km
    19/0/undercroft/0            regolith     2 floors  ##__________________
    19/0/shallows/0              cover       10 floors  ##########__________
    19/0/deeps/0                 cover       16 floors  ################____
    19/0/underdeep/0             -            6 floors  ......______________
    19/0/nadir/0                 -            3 floors  ..._________________
    19/1/undercroft/0            -            3 floors  ~~~_________________
    19/1/shallows/0              -           10 floors  ~~~~~~~~~~__________
    19/1/deeps/0                 -            6 floors  ~~~~~~______________
    19/1/underdeep/0             -            7 floors  ......._____________
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

  vertex 22 — karst cave, reach 136.63376 m, gradient 27.201293 K/km
    22/0/undercroft/0            basement     1 floors  #___________________
    22/0/shallows/0              basement     6 floors  ######______________
    22/0/deeps/0                 -           14 floors  ..............______
    22/0/underdeep/0             -            7 floors  ......._____________
    22/0/nadir/0                 -            2 floors  ..__________________
    22/1/undercroft/0            -            3 floors  ~~~_________________
    22/1/shallows/0              basement     6 floors  ######______________
    22/1/deeps/0                 -           16 floors  ~~~~~~~~~~~~~~~~____
    22/1/underdeep/0             -           10 floors  ..........__________
    22/1/nadir/0                 -            4 floors  ~~~~________________
    22/2/undercroft/0            -            5 floors  ~~~~~_______________
    22/2/shallows/0              -            5 floors  ~~~~~_______________
    22/2/deeps/0                 -           20 floors  ~~~~~~~~~~~~~~~~~~~~
    22/2/underdeep/0             -            9 floors  ~~~~~~~~~___________
    22/2/nadir/0                 -            5 floors  ~~~~~_______________
    22/3/undercroft/0            -            5 floors  ~~~~~_______________
    22/3/shallows/0              -            9 floors  ~~~~~~~~~___________
    22/3/deeps/0                 -           13 floors  ~~~~~~~~~~~~~_______
    22/3/underdeep/0             -            5 floors  ~~~~~_______________
    22/3/nadir/0                 -            3 floors  ~~~_________________
```
