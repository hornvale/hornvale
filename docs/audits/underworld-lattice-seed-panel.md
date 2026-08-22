# The Underworld of Seeds 42, 7 and 1234

The chamber lattice as three worlds actually realize it: how many cave
systems each has, how many chambers exist beneath them, how those chambers
distribute over the delve ladder and over the rock they sit in, and then --
run by run -- the first three cave systems of each world.

A chamber is never stored. Existence and content are pure functions of an
address, so this page is a *witness*, not a record: every line is re-derived
from the seed on each regeneration, and a change to the derivation key, to
the existence draw, to a run's drawn length, to a chamber's content or
to the depth the rock grants a cave moves bytes here.

`reachable` is the count a player would actually experience: chambers
reachable from an entrance by the passage graph, which today has no
vertical connection at all -- so everything above floor 0 is cut off. That
gap is the number, not a rounding error.

The `key` column is the real derivation key of that run's floor 0 --
the string `StreamLabel::dynamic` hashes -- not a rendering of the address.
Each run shows one glyph per floor the LATTICE admits, never per floor the
run drew: a `#` exists, a `.` was refused by the existence draw (or sits
deeper than the cave's budget reaches), and a `_` is past that run's
own drawn length. Bounding the row by the drawn length instead is what made
an earlier version of this page unable to see either floor gate at all.

```text
seed 42
  derivation      chamber/v3 over chamber/run-floors/v1
  lattice         4 branches per system, 5 bands, 20 floors admitted per run, 1 entrance witnessed
  cave systems    874  (ocean-cell caves skipped: 0)
  floors drawn    113546
  chambers        14976
  reachable       1158 from 396 open entrances
  by band         undercroft:2017  shallows:4107  deeps:6682  underdeep:1655  nadir:515  
  by rock         regolith:936  cover:417  basement:13623  roots:0  underneath:0  off-ladder:0
  by origin       found:14976  made:0
  past run length 0   (chambers beyond their run's drawn floors)
  past branch cnt 0   (chambers beyond their system's drawn branches)

  the first three cave systems, run by run
  (key = the floor-0 derivation key; # exists, . refused, _ past the run's drawn floors, ~ past the system's drawn branch count)

  cell 30 — fracture cave, reach 2145.7358 m, gradient 26.007865 K/km
    30/0/0/undercroft/0          regolith     3 floors  ##._________________
    30/0/0/shallows/0            cover        3 floors  #.#_________________
    30/0/0/deeps/0               basement    12 floors  ...##.##.###________
    30/0/0/underdeep/0           basement    10 floors  ..####..#.__________
    30/0/0/nadir/0               basement     3 floors  #.._________________
    30/0/1/undercroft/0          -            5 floors  ~~~~~_______________
    30/0/1/shallows/0            -            7 floors  ~~~~~~~_____________
    30/0/1/deeps/0               -           11 floors  ~~~~~~~~~~~_________
    30/0/1/underdeep/0           -           10 floors  ~~~~~~~~~~__________
    30/0/1/nadir/0               -            5 floors  ~~~~~_______________
    30/0/2/undercroft/0          -            5 floors  ~~~~~_______________
    30/0/2/shallows/0            -            4 floors  ~~~~________________
    30/0/2/deeps/0               -           16 floors  ~~~~~~~~~~~~~~~~____
    30/0/2/underdeep/0           -            8 floors  ~~~~~~~~____________
    30/0/2/nadir/0               -            4 floors  ~~~~________________
    30/0/3/undercroft/0          -            3 floors  ~~~_________________
    30/0/3/shallows/0            -            8 floors  ~~~~~~~~____________
    30/0/3/deeps/0               -           11 floors  ~~~~~~~~~~~_________
    30/0/3/underdeep/0           -            6 floors  ~~~~~~______________
    30/0/3/nadir/0               -            2 floors  ~~__________________

  cell 111 — karst cave, reach 252.0545 m, gradient 24.69077 K/km
    111/0/0/undercroft/0         -            2 floors  ..__________________
    111/0/0/shallows/0           basement     8 floors  .###.##.____________
    111/0/0/deeps/0              -           19 floors  ..................._
    111/0/0/underdeep/0          -            8 floors  ........____________
    111/0/0/nadir/0              -            3 floors  ..._________________
    111/0/1/undercroft/0         -            4 floors  ~~~~________________
    111/0/1/shallows/0           -            8 floors  ~~~~~~~~____________
    111/0/1/deeps/0              -            5 floors  ~~~~~_______________
    111/0/1/underdeep/0          -            7 floors  ~~~~~~~_____________
    111/0/1/nadir/0              -            5 floors  ~~~~~_______________
    111/0/2/undercroft/0         -            3 floors  ~~~_________________
    111/0/2/shallows/0           -            5 floors  ~~~~~_______________
    111/0/2/deeps/0              -           12 floors  ~~~~~~~~~~~~________
    111/0/2/underdeep/0          -            6 floors  ~~~~~~______________
    111/0/2/nadir/0              -            3 floors  ~~~_________________
    111/0/3/undercroft/0         -            3 floors  ~~~_________________
    111/0/3/shallows/0           -            3 floors  ~~~_________________
    111/0/3/deeps/0              -            8 floors  ~~~~~~~~____________
    111/0/3/underdeep/0          -            8 floors  ~~~~~~~~____________
    111/0/3/nadir/0              -            3 floors  ~~~_________________

  cell 282 — karst cave, reach 483.46851 m, gradient 22.399271 K/km
    282/0/0/undercroft/0         -            1 floors  .___________________
    282/0/0/shallows/0           basement    10 floors  .###.#.#..__________
    282/0/0/deeps/0              basement    14 floors  ##..##.###.###______
    282/0/0/underdeep/0          -            6 floors  ......______________
    282/0/0/nadir/0              -            2 floors  ..__________________
    282/0/1/undercroft/0         -            4 floors  ~~~~________________
    282/0/1/shallows/0           -            4 floors  ~~~~________________
    282/0/1/deeps/0              -           16 floors  ~~~~~~~~~~~~~~~~____
    282/0/1/underdeep/0          -            5 floors  ~~~~~_______________
    282/0/1/nadir/0              -            1 floors  ~___________________
    282/0/2/undercroft/0         -            3 floors  ~~~_________________
    282/0/2/shallows/0           -           10 floors  ~~~~~~~~~~__________
    282/0/2/deeps/0              -           11 floors  ~~~~~~~~~~~_________
    282/0/2/underdeep/0          -           10 floors  ~~~~~~~~~~__________
    282/0/2/nadir/0              -            2 floors  ~~__________________
    282/0/3/undercroft/0         -            3 floors  ~~~_________________
    282/0/3/shallows/0           -            7 floors  ~~~~~~~_____________
    282/0/3/deeps/0              -           18 floors  ~~~~~~~~~~~~~~~~~~__
    282/0/3/underdeep/0          -            9 floors  ~~~~~~~~~___________
    282/0/3/nadir/0              -            4 floors  ~~~~________________

seed 7
  derivation      chamber/v3 over chamber/run-floors/v1
  lattice         4 branches per system, 5 bands, 20 floors admitted per run, 1 entrance witnessed
  cave systems    1681  (ocean-cell caves skipped: 0)
  floors drawn    218312
  chambers        29559
  reachable       2462 from 829 open entrances
  by band         undercroft:4037  shallows:8307  deeps:10124  underdeep:5293  nadir:1798  
  by rock         regolith:2159  cover:1712  basement:25688  roots:0  underneath:0  off-ladder:0
  by origin       found:29559  made:0
  past run length 0   (chambers beyond their run's drawn floors)
  past branch cnt 0   (chambers beyond their system's drawn branches)

  the first three cave systems, run by run
  (key = the floor-0 derivation key; # exists, . refused, _ past the run's drawn floors, ~ past the system's drawn branch count)

  cell 52 — karst cave, reach 249.24025 m, gradient 24.917192 K/km
    52/0/0/undercroft/0          regolith     2 floors  #.__________________
    52/0/0/shallows/0            cover        4 floors  ..#.________________
    52/0/0/deeps/0               -           10 floors  ..........__________
    52/0/0/underdeep/0           -            5 floors  ....._______________
    52/0/0/nadir/0               -            2 floors  ..__________________
    52/0/1/undercroft/0          -            4 floors  ~~~~________________
    52/0/1/shallows/0            -            7 floors  ~~~~~~~_____________
    52/0/1/deeps/0               -           18 floors  ~~~~~~~~~~~~~~~~~~__
    52/0/1/underdeep/0           -            7 floors  ~~~~~~~_____________
    52/0/1/nadir/0               -            1 floors  ~___________________
    52/0/2/undercroft/0          -            1 floors  ~___________________
    52/0/2/shallows/0            -            9 floors  ~~~~~~~~~___________
    52/0/2/deeps/0               -           16 floors  ~~~~~~~~~~~~~~~~____
    52/0/2/underdeep/0           -            8 floors  ~~~~~~~~____________
    52/0/2/nadir/0               -            2 floors  ~~__________________
    52/0/3/undercroft/0          -            2 floors  ~~__________________
    52/0/3/shallows/0            -            5 floors  ~~~~~_______________
    52/0/3/deeps/0               -            7 floors  ~~~~~~~_____________
    52/0/3/underdeep/0           -            8 floors  ~~~~~~~~____________
    52/0/3/nadir/0               -            2 floors  ~~__________________

  cell 70 — fracture cave, reach 2230.3734 m, gradient 24.795713 K/km
    70/0/0/undercroft/0          regolith     2 floors  #.__________________
    70/0/0/shallows/0            basement     3 floors  ###_________________
    70/0/0/deeps/0               basement     9 floors  #.#..####___________
    70/0/0/underdeep/0           basement     7 floors  #...###_____________
    70/0/0/nadir/0               basement     5 floors  .####_______________
    70/0/1/undercroft/0          -            1 floors  .___________________
    70/0/1/shallows/0            basement     9 floors  ...##.##.___________
    70/0/1/deeps/0               basement    11 floors  ..#.##..#.#_________
    70/0/1/underdeep/0           basement     8 floors  ..#.##.#____________
    70/0/1/nadir/0               basement     3 floors  #.#_________________
    70/0/2/undercroft/0          -            5 floors  ~~~~~_______________
    70/0/2/shallows/0            -            6 floors  ~~~~~~______________
    70/0/2/deeps/0               -           11 floors  ~~~~~~~~~~~_________
    70/0/2/underdeep/0           -            9 floors  ~~~~~~~~~___________
    70/0/2/nadir/0               -            5 floors  ~~~~~_______________
    70/0/3/undercroft/0          -            4 floors  ~~~~________________
    70/0/3/shallows/0            -            3 floors  ~~~_________________
    70/0/3/deeps/0               -           14 floors  ~~~~~~~~~~~~~~______
    70/0/3/underdeep/0           -            6 floors  ~~~~~~______________
    70/0/3/nadir/0               -            3 floors  ~~~_________________

  cell 92 — karst cave, reach 1409.9429 m, gradient 26.976856 K/km
    92/0/0/undercroft/0          regolith     3 floors  #.._________________
    92/0/0/shallows/0            cover        9 floors  ...##....___________
    92/0/0/deeps/0               basement    20 floors  ..#.##..##.######..#
    92/0/0/underdeep/0           basement     9 floors  ###.#..##___________
    92/0/0/nadir/0               -            5 floors  ....._______________
    92/0/1/undercroft/0          -            4 floors  ~~~~________________
    92/0/1/shallows/0            -            8 floors  ~~~~~~~~____________
    92/0/1/deeps/0               -           20 floors  ~~~~~~~~~~~~~~~~~~~~
    92/0/1/underdeep/0           -            7 floors  ~~~~~~~_____________
    92/0/1/nadir/0               -            4 floors  ~~~~________________
    92/0/2/undercroft/0          -            1 floors  ~___________________
    92/0/2/shallows/0            -            3 floors  ~~~_________________
    92/0/2/deeps/0               -           19 floors  ~~~~~~~~~~~~~~~~~~~_
    92/0/2/underdeep/0           -            5 floors  ~~~~~_______________
    92/0/2/nadir/0               -            5 floors  ~~~~~_______________
    92/0/3/undercroft/0          -            3 floors  ~~~_________________
    92/0/3/shallows/0            -            5 floors  ~~~~~_______________
    92/0/3/deeps/0               -           19 floors  ~~~~~~~~~~~~~~~~~~~_
    92/0/3/underdeep/0           -            8 floors  ~~~~~~~~____________
    92/0/3/nadir/0               -            5 floors  ~~~~~_______________

seed 1234
  derivation      chamber/v3 over chamber/run-floors/v1
  lattice         4 branches per system, 5 bands, 20 floors admitted per run, 1 entrance witnessed
  cave systems    1266  (ocean-cell caves skipped: 0)
  floors drawn    165367
  chambers        25165
  reachable       1905 from 641 open entrances
  by band         undercroft:3066  shallows:6222  deeps:10609  underdeep:4039  nadir:1229  
  by rock         regolith:1487  cover:1418  basement:22260  roots:0  underneath:0  off-ladder:0
  by origin       found:25165  made:0
  past run length 0   (chambers beyond their run's drawn floors)
  past branch cnt 0   (chambers beyond their system's drawn branches)

  the first three cave systems, run by run
  (key = the floor-0 derivation key; # exists, . refused, _ past the run's drawn floors, ~ past the system's drawn branch count)

  cell 18 — fracture cave, reach 2694.0137 m, gradient 21.180804 K/km
    18/0/0/undercroft/0          -            1 floors  .___________________
    18/0/0/shallows/0            basement     4 floors  .##.________________
    18/0/0/deeps/0               basement    17 floors  .....##.###.##.##___
    18/0/0/underdeep/0           basement     5 floors  .#.#._______________
    18/0/0/nadir/0               basement     4 floors  ..##________________
    18/0/1/undercroft/0          -            2 floors  ~~__________________
    18/0/1/shallows/0            -           10 floors  ~~~~~~~~~~__________
    18/0/1/deeps/0               -           10 floors  ~~~~~~~~~~__________
    18/0/1/underdeep/0           -            9 floors  ~~~~~~~~~___________
    18/0/1/nadir/0               -            3 floors  ~~~_________________
    18/0/2/undercroft/0          -            4 floors  ~~~~________________
    18/0/2/shallows/0            -            8 floors  ~~~~~~~~____________
    18/0/2/deeps/0               -            9 floors  ~~~~~~~~~___________
    18/0/2/underdeep/0           -            8 floors  ~~~~~~~~____________
    18/0/2/nadir/0               -            2 floors  ~~__________________
    18/0/3/undercroft/0          -            1 floors  ~___________________
    18/0/3/shallows/0            -            7 floors  ~~~~~~~_____________
    18/0/3/deeps/0               -            6 floors  ~~~~~~______________
    18/0/3/underdeep/0           -            7 floors  ~~~~~~~_____________
    18/0/3/nadir/0               -            1 floors  ~___________________

  cell 19 — fracture cave, reach 1699.8077 m, gradient 22.357908 K/km
    19/0/0/undercroft/0          -            1 floors  .___________________
    19/0/0/shallows/0            cover        4 floors  ##.#________________
    19/0/0/deeps/0               cover       14 floors  #...##......#.______
    19/0/0/underdeep/0           basement    10 floors  ..##.#####__________
    19/0/0/nadir/0               -            2 floors  ..__________________
    19/0/1/undercroft/0          regolith     2 floors  #.__________________
    19/0/1/shallows/0            cover        9 floors  ##.##.#.#___________
    19/0/1/deeps/0               cover       18 floors  ..##########.....#__
    19/0/1/underdeep/0           basement    10 floors  .##.#.##..__________
    19/0/1/nadir/0               -            2 floors  ..__________________
    19/0/2/undercroft/0          regolith     1 floors  #___________________
    19/0/2/shallows/0            cover        5 floors  ##..#_______________
    19/0/2/deeps/0               cover        5 floors  #####_______________
    19/0/2/underdeep/0           basement     5 floors  ..###_______________
    19/0/2/nadir/0               -            4 floors  ....________________
    19/0/3/undercroft/0          -            1 floors  .___________________
    19/0/3/shallows/0            cover        7 floors  ##...##_____________
    19/0/3/deeps/0               cover        7 floors  ##.#.##_____________
    19/0/3/underdeep/0           basement     8 floors  #.....##____________
    19/0/3/nadir/0               -            1 floors  .___________________

  cell 49 — fracture cave, reach 2039.1218 m, gradient 26.05835 K/km
    49/0/0/undercroft/0          basement     5 floors  .###._______________
    49/0/0/shallows/0            basement     9 floors  ..#.#.##.___________
    49/0/0/deeps/0               basement     8 floors  #####..#____________
    49/0/0/underdeep/0           basement    10 floors  #...###.##__________
    49/0/0/nadir/0               basement     2 floors  ##__________________
    49/0/1/undercroft/0          -            4 floors  ~~~~________________
    49/0/1/shallows/0            -            6 floors  ~~~~~~______________
    49/0/1/deeps/0               -           19 floors  ~~~~~~~~~~~~~~~~~~~_
    49/0/1/underdeep/0           -            6 floors  ~~~~~~______________
    49/0/1/nadir/0               -            4 floors  ~~~~________________
    49/0/2/undercroft/0          -            1 floors  ~___________________
    49/0/2/shallows/0            -            3 floors  ~~~_________________
    49/0/2/deeps/0               -            7 floors  ~~~~~~~_____________
    49/0/2/underdeep/0           -           10 floors  ~~~~~~~~~~__________
    49/0/2/nadir/0               -            1 floors  ~___________________
    49/0/3/undercroft/0          -            5 floors  ~~~~~_______________
    49/0/3/shallows/0            -            8 floors  ~~~~~~~~____________
    49/0/3/deeps/0               -            5 floors  ~~~~~_______________
    49/0/3/underdeep/0           -            7 floors  ~~~~~~~_____________
    49/0/3/nadir/0               -            4 floors  ~~~~________________
```
