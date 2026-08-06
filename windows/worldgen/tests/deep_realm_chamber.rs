//! The Deep Realm, Task 2: the chamber address lattice.
//!
//! Pure logic over `hornvale_worldgen::chamber` — no world-building fixture
//! is needed, because `chamber_exists`/`chamber_at` are pure functions of
//! `(seed, cave, addr)` and `Cave` is a small `Copy` struct callers can
//! construct directly. Not `#[ignore]`d: this battery is cheap and belongs
//! in the ordinary commit gate.
//!
//! Both tests below exist to catch the campaign's named highest-risk defect
//! (rule 1, `docs/superpowers/plans/2026-08-05-the-deep-realm.md`): an
//! address must name a PLACE, never a construction step. See each test's own
//! doc comment for which half of that rule it guards.

use hornvale_kernel::{CellId, Seed};
use hornvale_terrain::{BandKind, Cave, CaveKind};
use hornvale_worldgen::chamber::{
    ChamberAddr, SLOTS_PER_BAND, chamber_at, chamber_exists, passages_from,
};

/// The rule The Salt, 0102 and The Tolerance each learned separately:
/// generation order is never an identity. A `ChamberAddr` names a PLACE in a
/// lattice that exists before anything is generated into it, so nothing
/// about which chambers happen to exist can move another chamber's address.
///
/// Two caves differing ONLY in `deepest_band` (their measured depth budget)
/// both admit every address with `band <= 2` (`Basement`'s own rank). A
/// chamber at one of those addresses must come out byte-identical under
/// either cave — its content cannot have been renumbered by the deeper cave
/// having more chambers available to it.
#[test]
fn an_addresss_meaning_does_not_depend_on_which_other_chambers_exist() {
    let seed = Seed(90210);
    let cell = CellId(9);
    let shallow = Cave {
        kind: CaveKind::Karst,
        deepest_band: BandKind::Basement,
    };
    let deep = Cave {
        kind: CaveKind::Karst,
        deepest_band: BandKind::Roots,
    };

    // Basement's rank is 2, so bands 0..=2 (Regolith, Cover, Basement) are
    // in BOTH caves' budget; Roots's rank 3 gives `deep` a fourth band
    // `shallow` cannot reach at all. Every address checked here therefore
    // sits in the region shared by both caves' budgets.
    for band in 0..=2u8 {
        for slot in 0..SLOTS_PER_BAND {
            let addr = ChamberAddr {
                cell,
                entrance: 0,
                band,
                slot,
            };
            assert_eq!(
                chamber_exists(seed, &shallow, addr),
                chamber_exists(seed, &deep, addr),
                "existence at {addr:?} differs between a shallow and a deep cave \
                 sharing the same seed and cell"
            );
            assert_eq!(
                chamber_at(seed, &shallow, addr),
                chamber_at(seed, &deep, addr),
                "content at {addr:?} differs between a shallow and a deep cave — \
                 an address must name a PLACE, never a construction step"
            );
        }
    }
}

/// The lattice is a fixed size regardless of what any particular cave
/// realizes; occupancy within it is sparse and varies by seed. Over a cave
/// reaching `BandKind::Roots`, the address space checked here is
/// `SLOTS_PER_BAND * 4` (bands `0..=3`, `Regolith..=Roots`) — constant
/// across every seed — while the number of addresses that EXIST is
/// strictly less than that, and differs seed to seed.
#[test]
fn the_lattice_is_fixed_and_existence_is_sparse() {
    let cell = CellId(42);
    let cave = Cave {
        kind: CaveKind::Fracture,
        deepest_band: BandKind::Roots,
    };

    let mut existing_counts = Vec::new();
    for raw_seed in [1u64, 2, 3, 4, 5] {
        let seed = Seed(raw_seed);
        let mut total = 0u32;
        let mut existing = 0u32;
        for band in 0..=3u8 {
            for slot in 0..SLOTS_PER_BAND {
                total += 1;
                let addr = ChamberAddr {
                    cell,
                    entrance: 0,
                    band,
                    slot,
                };
                if chamber_exists(seed, &cave, addr) {
                    existing += 1;
                }
            }
        }
        assert_eq!(
            total,
            u32::from(SLOTS_PER_BAND) * 4,
            "the address space over a Roots-reaching cave must be a constant \
             SLOTS_PER_BAND * 4"
        );
        assert!(
            existing < total,
            "seed {raw_seed}: every address in the lattice exists ({existing}/{total}) — \
             existence must be sparse"
        );
        existing_counts.push(existing);
    }

    assert!(
        existing_counts.iter().any(|&c| c != existing_counts[0]),
        "existence count never varied across seeds: {existing_counts:?}"
    );
}

/// Spec H4. Passages are two-way — which is also why the underworld is
/// frightening: if you can go down, things can come up.
///
/// `passages_from`'s own doc comment explains WHY this holds by
/// construction (adjacency is a pure, symmetric function of the lattice
/// geometry alone). This test is the guard that the dissolution actually
/// holds in the shipped code, not merely believed to — it walks every
/// address in the probed region of the lattice, for several seeds and
/// cells, and checks both directions of every passage it finds.
#[test]
fn every_passage_is_traversable_in_both_directions() {
    let cave = Cave {
        kind: CaveKind::Fracture,
        deepest_band: BandKind::Roots,
    };

    for raw_seed in [1u64, 2, 3, 4, 5] {
        let seed = Seed(raw_seed);
        for raw_cell in [0u32, 1, 9, 42] {
            let cell = CellId(raw_cell);
            for band in 0..=3u8 {
                for slot in 0..SLOTS_PER_BAND {
                    let addr = ChamberAddr {
                        cell,
                        entrance: 0,
                        band,
                        slot,
                    };
                    for &neighbour in &passages_from(seed, &cave, addr) {
                        let back = passages_from(seed, &cave, neighbour);
                        assert!(
                            back.contains(&addr),
                            "seed {raw_seed} cell {raw_cell}: {addr:?} lists \
                             {neighbour:?} as a passage, but {neighbour:?}'s own \
                             passages do not list {addr:?} back — a one-way passage"
                        );
                    }
                }
            }
        }
    }
}

/// Step 4's connectivity guard (plan Task 3). An entrance you cannot get
/// anywhere from is not an entrance.
///
/// **"Not all of them" alone is a weak floor** — it would still pass if 999
/// of 1000 probe caves' entrances were dead ends, which is exactly the kind
/// of failure a floor-without-a-ceiling hides (a standing project lesson).
/// So this test reports the measured fraction and grounds the bar in a
/// prediction made BEFORE running it, not tuned after: `EXISTENCE_DENSITY`
/// is a coin flip (0.5) applied independently per address, and the
/// canonical entrance address (`band = 0, slot = 0`) has at most two
/// lattice neighbours (`band 0/slot 1` and `band 1/slot 0`) plus needs to
/// exist itself, so back-of-envelope under independence the reach rate is
/// well under half (`0.5 * (1 - 0.5^2) = 0.375`). **Measured over 1000
/// probe entrances (seeds 1..=100 x 10 cells): 410/1000 = 0.4100** — close
/// to that back-of-envelope prediction and comfortably nonzero, confirming
/// the lattice is not systematically disconnected from its entrances. A
/// "majority reach" bar would be an invented number the model doesn't
/// support (and the measurement bears that out — 41% is a real minority),
/// so the bar kept here is the plan's weaker one: **at least one probe
/// cave's entrance reaches a chamber, out of many probed** — strong enough
/// to fail if the lattice were broken (e.g. adjacency computing candidates
/// that never exist, or `passages_from` returning empty unconditionally),
/// but not invented past what the density model and the measurement both
/// predict.
#[test]
fn a_cave_mouth_reaches_at_least_one_chamber() {
    let cave = Cave {
        kind: CaveKind::Fracture,
        deepest_band: BandKind::Roots,
    };

    let mut reached = 0u32;
    let mut probed = 0u32;
    for raw_seed in 1u64..=100 {
        let seed = Seed(raw_seed);
        for raw_cell in 0u32..10 {
            let cell = CellId(raw_cell);
            let entrance = ChamberAddr {
                cell,
                entrance: 0,
                band: 0,
                slot: 0,
            };
            probed += 1;
            if !passages_from(seed, &cave, entrance).is_empty() {
                reached += 1;
            }
        }
    }

    let fraction = f64::from(reached) / f64::from(probed);
    println!(
        "cave-mouth connectivity: {reached}/{probed} entrances reach at least \
         one chamber ({fraction:.4})"
    );

    assert!(
        reached > 0,
        "0 of {probed} probe entrances reached any chamber — the lattice is \
         disconnected from every entrance"
    );
}
