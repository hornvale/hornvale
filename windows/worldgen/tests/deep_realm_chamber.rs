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
use hornvale_worldgen::chamber::{ChamberAddr, SLOTS_PER_BAND, chamber_at, chamber_exists};

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
