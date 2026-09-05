# The rest-site readout, after three surfaces entered the world

**Taken:** 2026-09-04, on the Mac, from the post-epoch Task 7 world
(`a2e5dedbe`; HEAD `479e65276` contained only later documentation repairs)
with the corrected Task 8 instrument in
`windows/lab/examples/rest_site_census.rs`.

```bash
cargo run --release -p hornvale-lab --example rest_site_census
```

This is the same requested sweep as the corrected pre-epoch checkpoint
`f352e57d6`: the same 24 seeds, 40 ticks, and request for 10 bodies per seed.
This rerun records what the two derivations actually returned: every produced
seed had 10 observed bodies, 240 total. No `offer`, hardness, surface gate, or
other Task 7 constant was retuned after unblinding.

## The headline

The three surfaces changed the measured world, and the reversal reached it.
Room-supported/non-`Bare` bouts rose from **601 of 5,096 (0.1179)** before the
epoch to **2,926 of 4,859 (0.6022)** after it: +48.42 percentage points, or
5.106 times the baseline share. Every one of the 24 seeds produced such a
bout, compared with 11 before.

That boolean is not the final production `SiteGrade`. The probe reconstructs
`room_affords_rest`, which is the `Bare`/`Afforded` room fallback;
`rest_timeline` instead emits `SiteGrade::On(kind)` for a sleep with a
same-day `SLEPT_ON` fact. The figures here therefore prove room support, not
an exact count of `Afforded` tags.

The preregistered P6 null did **not** occur: the room-supported share changed
and a live reversal was found. A different null did occur and is worth
publishing:
**rushes were composed in 34 walked rooms across 11 worlds, but no
`SLEPT_ON` fact named `rushes`.** No measured sleep selected them; this probe
does not distinguish a better co-composed choice from no sleep occurring in
those particular rooms at the relevant time.

## The sweep and the instrument check

| | |
|---|---|
| seeds | 24: `42, 7, 1234, 13, 1, 2, 3, 5, 8, 11, 17, 23, 29, 31, 37, 41, 53, 59, 61, 67, 71, 73, 79, 83` |
| ticks per seed | 40 |
| requested bodies per seed | 10 = 6 settlement-derived + 4 wild |
| **observed bodies** | **240 total; 10 for every produced seed** |
| worlds that built | 24 of 24 |
| **incomplete body derivations** | **0 of 24** |
| **runs that truncated** | **0 of 24** — every run completed all 40 ticks |

The truncation line was checked before interpreting any rate:

```text
seeds that produced a world : 24
seeds with no world         : []
bodies actually observed    : 240
INCOMPLETE body derivations : []  (must be empty)
TRUNCATED runs              : []  (must be empty)
```

## Bouts and committed surface facts

| | count | share of bouts |
|---|---:|---:|
| `RESTED` bouts | 1,263 | 0.2599 |
| `SLEPT` bouts | 3,596 | 0.7401 |
| **total bouts** | **4,859** | 1.0000 |
| room-supported / non-`Bare` | **2,926** | **0.6022** |
| room unsupported / `Bare` | **1,933** | **0.3978** |

Per seed (`bodies` and `ticks` are observed counts; all 10 and 40,
respectively):

| seed | bodies | ticks | rested | slept | supported | bare | slept-on |
|---:|---:|---:|---:|---:|---:|---:|---:|
| 42 | 10 | 40 | 51 | 186 | 109 | 128 | 80 |
| 7 | 10 | 40 | 52 | 166 | 123 | 95 | 94 |
| 1234 | 10 | 40 | 3 | 272 | 177 | 98 | 174 |
| 13 | 10 | 40 | 273 | 0 | 273 | 0 | 0 |
| 1 | 10 | 40 | 2 | 105 | 58 | 49 | 56 |
| 2 | 10 | 40 | 4 | 105 | 45 | 64 | 41 |
| 3 | 10 | 40 | 62 | 241 | 151 | 152 | 113 |
| 5 | 10 | 40 | 4 | 152 | 60 | 96 | 60 |
| 8 | 10 | 40 | 223 | 0 | 14 | 209 | 0 |
| 11 | 10 | 40 | 41 | 84 | 55 | 70 | 14 |
| 17 | 10 | 40 | 46 | 255 | 268 | 33 | 228 |
| 23 | 10 | 40 | 22 | 86 | 75 | 33 | 55 |
| 29 | 10 | 40 | 136 | 282 | 372 | 46 | 236 |
| 31 | 10 | 40 | 2 | 143 | 75 | 70 | 73 |
| 37 | 10 | 40 | 26 | 110 | 98 | 38 | 75 |
| 41 | 10 | 40 | 28 | 186 | 59 | 155 | 51 |
| 53 | 10 | 40 | 15 | 104 | 110 | 9 | 95 |
| 59 | 10 | 40 | 37 | 234 | 72 | 199 | 51 |
| 61 | 10 | 40 | 116 | 99 | 97 | 118 | 55 |
| 67 | 10 | 40 | 37 | 154 | 183 | 8 | 146 |
| 71 | 10 | 40 | 79 | 210 | 137 | 152 | 86 |
| 73 | 10 | 40 | 1 | 114 | 112 | 3 | 111 |
| 79 | 10 | 40 | 1 | 168 | 64 | 105 | 64 |
| 83 | 10 | 40 | 2 | 140 | 139 | 3 | 137 |

The 2,095 committed `SLEPT_ON` facts name:

| kind | facts |
|---|---:|
| `bed` | 311 |
| `bracken` | 463 |
| `ledge` | 1,321 |
| `rushes` | **0** |

`SLEPT_ON` is committed only for `SLEPT`, never for `RESTED`, so it is not
expected to equal the room-supported-bout total.

## P2–P6

### P2 — confirmed: a reversal exists in real composed rooms

The evidence comes from actual derived bodies standing in actual rooms during
the sweep, not from Task 4's synthetic reversal fixture. Both witness rooms
compose both `ledge` and `rushes`:

```text
seed=1234 room=FacetId(4109484416)
body=EntityId(11918482352064102400) species=gully-dwarf
ledge=1.136008901 < rushes=1.344455629

seed=1234 room=FacetId(2763773090)
body=EntityId(3792779017850388480) species=drow
ledge=1.294000000 > rushes=1.079841873
```

The same two surfaces therefore order oppositely for two bodies/species that
actually reached rooms containing them.

### P3 — confirmed: every added kind is reached

Counts are distinct walked rooms within each seed, summed as scalars across
the 24-world sweep. The rate is worlds with at least one such room out of the
24 worlds built.

| kind | distinct walked rooms | worlds reached | rate |
|---|---:|---:|---:|
| `rushes` | 34 | 11 / 24 | 0.4583 |
| `ledge` | 163 | 24 / 24 | 1.0000 |
| `bracken` | 402 | 12 / 24 | 0.5000 |

### P4 — confirmed: a live choice is consequential

An actual drow body stood in a seed-1234 room containing both a bed and a
ledge, and the grades differ:

```text
seed=1234 room=FacetId(2763773090)
body=EntityId(3792779017850388480) species=drow
bed=1.500000000 > ledge=1.294000000
```

The room's choice edge therefore has a numerical consequence for the body in
it.

### P5 — confirmed without a target: bare remains reachable

**1,933 of 4,859 bouts (0.3978) had no room support and therefore take the
`Bare` fallback.** No target was asserted.
For spatial context, the wild-and-warm quadrant remains non-affording and is
1,638 of 2,203 distinct-per-seed walked rooms (0.7435).

| quadrant | rooms | share | room supports rest somewhere |
|---|---:|---:|---|
| `built=false cold=false` | 1,638 | 0.7435 | no |
| `built=false cold=true` | 402 | 0.1825 | yes |
| `built=true cold=false` | 129 | 0.0586 | yes |
| `built=true cold=true` | 34 | 0.0154 | yes |
| total | 2,203 | | |

### P6 — the preregistered null did not occur; the observed null still ships

The room-supported share is not unchanged (`0.1179 → 0.6022`) and P2 found a
live reversal, so neither null branch named by P6 fired. The observed zero is
the `rushes` selection count: world-reachable in 11/24 worlds, selected in 0
committed sleeps. It is reported as a finding, not repaired by retuning.

## What is reconstructed

The room-support boolean reconstructs private `liveness::room_affords_rest`.
It deliberately does not join `SLEPT_ON`, so it does not reproduce
`rest_timeline`'s exact final `SiteGrade`. P2/P4 carry the other
reconstruction: the probe reproduces private `liveness::sleep_traits_of` and
`liveness::grade_of` from the public object, sleep-grade, and habitat-realm
registries, `substrate_response`, the published formula, and
`FIT_FLOOR = 0.2`. It does not widen those private production functions for a
measurement's benefit. A future private change can make either reconstruction
drift silently; the source marks both boundaries explicitly.
