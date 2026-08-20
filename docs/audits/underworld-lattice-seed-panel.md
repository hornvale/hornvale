# The Underworld of Seeds 42, 7 and 1234

The chamber lattice as three worlds actually realize it: how many cave
systems each has, how many chambers exist beneath them, how those chambers
distribute over the delve ladder and over the rock they sit in, and then --
run by run -- the first three cave systems of each world.

A chamber is never stored. Existence and content are pure functions of an
address, so this page is a *witness*, not a record: every line is re-derived
from the seed on each regeneration, and a change to the derivation key, to
the existence draw, to a run's drawn length or to the depth the rock grants
a cave moves bytes here.

The `key` column is the real derivation key of that run's floor 0 --
the string `StreamLabel::dynamic` hashes -- not a rendering of the address.
A `#` is a floor that exists; a `.` is one the draw refused, or one whose
band sits deeper than the cave's own budget reaches.

```text
seed 42
  derivation      chamber/v3 over chamber/run-floors/v1
  lattice         4 branches per system, 5 bands, 20 floors admitted per run
  cave systems    874
  floors drawn    113546
  chambers        37449
  by band         undercroft:5181  shallows:10354  deeps:16640  underdeep:4010  nadir:1264  
  by rock         regolith:2535  cover:972  basement:33942  roots:0  underneath:0  off-ladder:0

  the first three cave systems, run by run
  (key = the floor-0 derivation key; # = a floor that exists)

  cell 30 — fracture cave, reach 2145.7358 m, gradient 26.007865 K/km
    30/0/0/undercroft/0          regolith     3 floors  ##.
    30/0/0/shallows/0            cover        3 floors  #.#
    30/0/0/deeps/0               basement    12 floors  ...##.##.###
    30/0/0/underdeep/0           basement    10 floors  ..####..#.
    30/0/0/nadir/0               basement     3 floors  #..
    30/0/1/undercroft/0          regolith     5 floors  .##..
    30/0/1/shallows/0            cover        7 floors  #...#.#
    30/0/1/deeps/0               basement    11 floors  .#....##.##
    30/0/1/underdeep/0           basement    10 floors  ###....#..
    30/0/1/nadir/0               basement     5 floors  #..#.
    30/0/2/undercroft/0          regolith     5 floors  ....#
    30/0/2/shallows/0            cover        4 floors  ###.
    30/0/2/deeps/0               basement    16 floors  ####.....##.....
    30/0/2/underdeep/0           basement     8 floors  .#.#..##
    30/0/2/nadir/0               basement     4 floors  ...#
    30/0/3/undercroft/0          regolith     3 floors  #..
    30/0/3/shallows/0            cover        8 floors  .#..#..#
    30/0/3/deeps/0               basement    11 floors  ..###.##.#.
    30/0/3/underdeep/0           basement     6 floors  .#..#.
    30/0/3/nadir/0               basement     2 floors  .#

  cell 111 — karst cave, reach 252.0545 m, gradient 24.69077 K/km
    111/0/0/undercroft/0         -            2 floors  ..
    111/0/0/shallows/0           basement     8 floors  .###.##.
    111/0/0/deeps/0              -           19 floors  ...................
    111/0/0/underdeep/0          -            8 floors  ........
    111/0/0/nadir/0              -            3 floors  ...
    111/0/1/undercroft/0         basement     4 floors  #.#.
    111/0/1/shallows/0           basement     8 floors  #...#.##
    111/0/1/deeps/0              -            5 floors  .....
    111/0/1/underdeep/0          -            7 floors  .......
    111/0/1/nadir/0              -            5 floors  .....
    111/0/2/undercroft/0         -            3 floors  ...
    111/0/2/shallows/0           basement     5 floors  ..#..
    111/0/2/deeps/0              -           12 floors  ............
    111/0/2/underdeep/0          -            6 floors  ......
    111/0/2/nadir/0              -            3 floors  ...
    111/0/3/undercroft/0         -            3 floors  ...
    111/0/3/shallows/0           -            3 floors  ...
    111/0/3/deeps/0              -            8 floors  ........
    111/0/3/underdeep/0          -            8 floors  ........
    111/0/3/nadir/0              -            3 floors  ...

  cell 282 — karst cave, reach 483.46851 m, gradient 22.399271 K/km
    282/0/0/undercroft/0         -            1 floors  .
    282/0/0/shallows/0           basement    10 floors  .###.#.#..
    282/0/0/deeps/0              basement    14 floors  ##..##.###.###
    282/0/0/underdeep/0          -            6 floors  ......
    282/0/0/nadir/0              -            2 floors  ..
    282/0/1/undercroft/0         basement     4 floors  .#.#
    282/0/1/shallows/0           basement     4 floors  #...
    282/0/1/deeps/0              basement    16 floors  ##...##..##.#...
    282/0/1/underdeep/0          -            5 floors  .....
    282/0/1/nadir/0              -            1 floors  .
    282/0/2/undercroft/0         basement     3 floors  .##
    282/0/2/shallows/0           basement    10 floors  ...#..##.#
    282/0/2/deeps/0              basement    11 floors  #.#.##.###.
    282/0/2/underdeep/0          -           10 floors  ..........
    282/0/2/nadir/0              -            2 floors  ..
    282/0/3/undercroft/0         basement     3 floors  ##.
    282/0/3/shallows/0           basement     7 floors  ...#.#.
    282/0/3/deeps/0              basement    18 floors  ##.#####.#..#.##.#
    282/0/3/underdeep/0          -            9 floors  .........
    282/0/3/nadir/0              -            4 floors  ....

seed 7
  derivation      chamber/v3 over chamber/run-floors/v1
  lattice         4 branches per system, 5 bands, 20 floors admitted per run
  cave systems    1681
  floors drawn    218312
  chambers        73253
  by band         undercroft:10056  shallows:20567  deeps:25175  underdeep:13113  nadir:4342  
  by rock         regolith:5386  cover:4485  basement:63382  roots:0  underneath:0  off-ladder:0

  the first three cave systems, run by run
  (key = the floor-0 derivation key; # = a floor that exists)

  cell 52 — karst cave, reach 249.24025 m, gradient 24.917192 K/km
    52/0/0/undercroft/0          regolith     2 floors  #.
    52/0/0/shallows/0            cover        4 floors  ..#.
    52/0/0/deeps/0               -           10 floors  ..........
    52/0/0/underdeep/0           -            5 floors  .....
    52/0/0/nadir/0               -            2 floors  ..
    52/0/1/undercroft/0          regolith     4 floors  #.#.
    52/0/1/shallows/0            cover        7 floors  ##..#..
    52/0/1/deeps/0               -           18 floors  ..................
    52/0/1/underdeep/0           -            7 floors  .......
    52/0/1/nadir/0               -            1 floors  .
    52/0/2/undercroft/0          regolith     1 floors  #
    52/0/2/shallows/0            cover        9 floors  #.#.#####
    52/0/2/deeps/0               -           16 floors  ................
    52/0/2/underdeep/0           -            8 floors  ........
    52/0/2/nadir/0               -            2 floors  ..
    52/0/3/undercroft/0          regolith     2 floors  #.
    52/0/3/shallows/0            cover        5 floors  #.#..
    52/0/3/deeps/0               -            7 floors  .......
    52/0/3/underdeep/0           -            8 floors  ........
    52/0/3/nadir/0               -            2 floors  ..

  cell 70 — fracture cave, reach 2230.3734 m, gradient 24.795713 K/km
    70/0/0/undercroft/0          regolith     2 floors  #.
    70/0/0/shallows/0            basement     3 floors  ###
    70/0/0/deeps/0               basement     9 floors  #.#..####
    70/0/0/underdeep/0           basement     7 floors  #...###
    70/0/0/nadir/0               basement     5 floors  .####
    70/0/1/undercroft/0          -            1 floors  .
    70/0/1/shallows/0            basement     9 floors  ...##.##.
    70/0/1/deeps/0               basement    11 floors  ..#.##..#.#
    70/0/1/underdeep/0           basement     8 floors  ..#.##.#
    70/0/1/nadir/0               basement     3 floors  #.#
    70/0/2/undercroft/0          regolith     5 floors  ..#..
    70/0/2/shallows/0            basement     6 floors  #.#...
    70/0/2/deeps/0               basement    11 floors  ####...####
    70/0/2/underdeep/0           basement     9 floors  ###.###.#
    70/0/2/nadir/0               basement     5 floors  ..##.
    70/0/3/undercroft/0          regolith     4 floors  #.#.
    70/0/3/shallows/0            basement     3 floors  #..
    70/0/3/deeps/0               basement    14 floors  #.####..##..#.
    70/0/3/underdeep/0           basement     6 floors  ##....
    70/0/3/nadir/0               -            3 floors  ...

  cell 92 — karst cave, reach 1409.9429 m, gradient 26.976856 K/km
    92/0/0/undercroft/0          regolith     3 floors  #..
    92/0/0/shallows/0            cover        9 floors  ...##....
    92/0/0/deeps/0               basement    20 floors  ..#.##..##.######..#
    92/0/0/underdeep/0           basement     9 floors  ###.#..##
    92/0/0/nadir/0               -            5 floors  .....
    92/0/1/undercroft/0          regolith     4 floors  ###.
    92/0/1/shallows/0            cover        8 floors  .#..##..
    92/0/1/deeps/0               basement    20 floors  .###.#.##....#.#.##.
    92/0/1/underdeep/0           basement     7 floors  #...#..
    92/0/1/nadir/0               -            4 floors  ....
    92/0/2/undercroft/0          -            1 floors  .
    92/0/2/shallows/0            cover        3 floors  .##
    92/0/2/deeps/0               basement    19 floors  ##..####..#.....##.
    92/0/2/underdeep/0           basement     5 floors  ###.#
    92/0/2/nadir/0               -            5 floors  .....
    92/0/3/undercroft/0          regolith     3 floors  #..
    92/0/3/shallows/0            cover        5 floors  ##...
    92/0/3/deeps/0               basement    19 floors  ##...##...#####.##.
    92/0/3/underdeep/0           basement     8 floors  ###..#..
    92/0/3/nadir/0               -            5 floors  .....

seed 1234
  derivation      chamber/v3 over chamber/run-floors/v1
  lattice         4 branches per system, 5 bands, 20 floors admitted per run
  cave systems    1266
  floors drawn    165367
  chambers        62254
  by band         undercroft:7650  shallows:15286  deeps:26228  underdeep:9892  nadir:3198  
  by rock         regolith:3810  cover:3532  basement:54912  roots:0  underneath:0  off-ladder:0

  the first three cave systems, run by run
  (key = the floor-0 derivation key; # = a floor that exists)

  cell 18 — fracture cave, reach 2694.0137 m, gradient 21.180804 K/km
    18/0/0/undercroft/0          -            1 floors  .
    18/0/0/shallows/0            basement     4 floors  .##.
    18/0/0/deeps/0               basement    17 floors  .....##.###.##.##
    18/0/0/underdeep/0           basement     5 floors  .#.#.
    18/0/0/nadir/0               basement     4 floors  ..##
    18/0/1/undercroft/0          regolith     2 floors  #.
    18/0/1/shallows/0            basement    10 floors  .#..####.#
    18/0/1/deeps/0               basement    10 floors  ###.###...
    18/0/1/underdeep/0           basement     9 floors  #..#.####
    18/0/1/nadir/0               basement     3 floors  ###
    18/0/2/undercroft/0          regolith     4 floors  ####
    18/0/2/shallows/0            basement     8 floors  ##....##
    18/0/2/deeps/0               -            9 floors  .........
    18/0/2/underdeep/0           basement     8 floors  ####.##.
    18/0/2/nadir/0               basement     2 floors  #.
    18/0/3/undercroft/0          -            1 floors  .
    18/0/3/shallows/0            basement     7 floors  ##.#.#.
    18/0/3/deeps/0               basement     6 floors  .....#
    18/0/3/underdeep/0           basement     7 floors  .#.....
    18/0/3/nadir/0               -            1 floors  .

  cell 19 — fracture cave, reach 1699.8077 m, gradient 22.357908 K/km
    19/0/0/undercroft/0          -            1 floors  .
    19/0/0/shallows/0            cover        4 floors  ##.#
    19/0/0/deeps/0               cover       14 floors  #...##......#.
    19/0/0/underdeep/0           basement    10 floors  ..##.#####
    19/0/0/nadir/0               -            2 floors  ..
    19/0/1/undercroft/0          regolith     2 floors  #.
    19/0/1/shallows/0            cover        9 floors  ##.##.#.#
    19/0/1/deeps/0               cover       18 floors  ..##########.....#
    19/0/1/underdeep/0           basement    10 floors  .##.#.##..
    19/0/1/nadir/0               -            2 floors  ..
    19/0/2/undercroft/0          regolith     1 floors  #
    19/0/2/shallows/0            cover        5 floors  ##..#
    19/0/2/deeps/0               cover        5 floors  #####
    19/0/2/underdeep/0           basement     5 floors  ..###
    19/0/2/nadir/0               -            4 floors  ....
    19/0/3/undercroft/0          -            1 floors  .
    19/0/3/shallows/0            cover        7 floors  ##...##
    19/0/3/deeps/0               cover        7 floors  ##.#.##
    19/0/3/underdeep/0           basement     8 floors  #.....##
    19/0/3/nadir/0               -            1 floors  .

  cell 49 — fracture cave, reach 2039.1218 m, gradient 26.05835 K/km
    49/0/0/undercroft/0          basement     5 floors  .###.
    49/0/0/shallows/0            basement     9 floors  ..#.#.##.
    49/0/0/deeps/0               basement     8 floors  #####..#
    49/0/0/underdeep/0           basement    10 floors  #...###.##
    49/0/0/nadir/0               basement     2 floors  ##
    49/0/1/undercroft/0          basement     4 floors  #..#
    49/0/1/shallows/0            basement     6 floors  ###.#.
    49/0/1/deeps/0               basement    19 floors  #..#.###.#.#..##..#
    49/0/1/underdeep/0           basement     6 floors  #.....
    49/0/1/nadir/0               basement     4 floors  #.##
    49/0/2/undercroft/0          basement     1 floors  #
    49/0/2/shallows/0            basement     3 floors  .#.
    49/0/2/deeps/0               basement     7 floors  #.#.###
    49/0/2/underdeep/0           basement    10 floors  ###.###.##
    49/0/2/nadir/0               -            1 floors  .
    49/0/3/undercroft/0          basement     5 floors  .##..
    49/0/3/shallows/0            basement     8 floors  #.#..#..
    49/0/3/deeps/0               basement     5 floors  .####
    49/0/3/underdeep/0           basement     7 floors  ...#.#.
    49/0/3/nadir/0               basement     4 floors  ##..
```
