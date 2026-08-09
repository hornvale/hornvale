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

use std::collections::BTreeMap;

use hornvale_kernel::{CellId, Seed};
use hornvale_terrain::{BandKind, Cave, CaveKind};
use hornvale_worldgen::chamber::{
    ChamberAddr, ChamberOrigin, SLOTS_PER_BAND, chamber_at, chamber_exists, passages_from,
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
    let no_overrides = BTreeMap::new();
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
                chamber_at(seed, &shallow, addr, &no_overrides),
                chamber_at(seed, &deep, addr, &no_overrides),
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
/// claim: invariant(forall-seed) — lattice size is fixed, existence is
/// sparse and seed-varying (seedless sweep, audit §5: builds no world;
/// named explicitly in the task brief)
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
/// claim: invariant(forall-seed) — passage bidirectionality over a
/// hand-built lattice (seedless sweep, audit §5: builds no world)
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
/// claim: rate(forall-seed, conditional > 0.5) — cave-mouth connectivity
/// over a hand-built lattice (seedless sweep, audit §5: builds no world)
#[test]
fn a_cave_mouth_reaches_at_least_one_chamber() {
    let cave = Cave {
        kind: CaveKind::Fracture,
        deepest_band: BandKind::Roots,
    };

    let mut reached = 0u32;
    let mut probed = 0u32;
    let mut entrance_exists = 0u32;
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
            if chamber_exists(seed, &cave, entrance) {
                entrance_exists += 1;
            }
            if !passages_from(seed, &cave, entrance).is_empty() {
                reached += 1;
            }
        }
    }

    // Decomposed, because the single "reached" rate conflates two different
    // facts and only one of them is about CONNECTIVITY. `passages_from`
    // returns empty both when the entrance chamber is isolated AND when the
    // entrance address holds no chamber at all, and those mean opposite
    // things about the lattice.
    let fraction = f64::from(reached) / f64::from(probed);
    let exists_rate = f64::from(entrance_exists) / f64::from(probed);
    let conditional = if entrance_exists == 0 {
        0.0
    } else {
        f64::from(reached) / f64::from(entrance_exists)
    };
    println!(
        "cave-mouth connectivity: {reached}/{probed} probe entrances reach a \
         chamber ({fraction:.4})"
    );
    println!(
        "  decomposed: entrance chamber EXISTS in {entrance_exists}/{probed} \
         ({exists_rate:.4}); of those, {conditional:.4} reach a neighbour"
    );
    println!(
        "  NOTE: an entrance address holding NO chamber is spec §3.4 rung 0 — \
         `Sealed`, \"the void exists and is unreachable\" — not a defect. \
         Task 5's `delve` must refuse such a cave BY NAMING IT sealed."
    );

    assert!(
        reached > 0,
        "0 of {probed} probe entrances reached any chamber — the lattice is \
         disconnected from every entrance"
    );
    // The conditional rate is the one that actually measures connectivity,
    // and it has a prediction: an existing entrance has at most two lattice
    // neighbours, so under independence at EXISTENCE_DENSITY = 0.5 it should
    // reach one with probability 1 - 0.5^2 = 0.75. A collapse here would mean
    // adjacency is generating candidates that can never exist.
    assert!(
        conditional > 0.5,
        "only {conditional:.4} of EXISTING entrance chambers reach a \
         neighbour; under the lattice's own density model this should be near \
         0.75, so adjacency is likely generating unreachable candidates"
    );
}

/// The seam, per spec 3.3: a chamber's content is its own latest override
/// fact, else its address-derived default. This campaign ships no WRITER, and
/// **commits nothing** — the resolver is tested directly, so the address's
/// on-ledger form stays genuinely undecided until a campaign needs to dig.
/// (Owner's ruling, 2026-08-05: committing a fact here would fix that form as
/// a permanent key, which spec 8 flag 2 exists to defer.)
///
/// The payload is `origin`: default `Found`, override `Made` (ledger #24).
/// Also assert the two invariants that make the seam more than a lookup:
///   - no-override resolution is UNCHANGED from the pre-Task-4 derivation;
///   - `Made` is absorbing: nothing takes a chamber back to `Found`.
#[test]
fn an_override_wins_over_the_derived_default() {
    let seed = Seed(2026);
    let cave = Cave {
        kind: CaveKind::Fracture,
        deepest_band: BandKind::Roots,
    };
    let cell = CellId(4);

    // Find two addresses that both exist under this (seed, cave, cell) —
    // one to override, one to leave alone as the "unaffected" witness.
    let mut existing = Vec::new();
    for band in 0..=3u8 {
        for slot in 0..SLOTS_PER_BAND {
            let addr = ChamberAddr {
                cell,
                entrance: 0,
                band,
                slot,
            };
            if chamber_exists(seed, &cave, addr) {
                existing.push(addr);
            }
        }
    }
    assert!(
        existing.len() >= 2,
        "need at least two existing chambers under this fixture to test an \
         override against an unaffected address; found {}",
        existing.len()
    );
    let overridden_addr = existing[0];
    let other_addr = existing[1];

    let no_overrides: BTreeMap<ChamberAddr, ChamberOrigin> = BTreeMap::new();

    // Invariant: no-override resolution is UNCHANGED from the pre-Task-4
    // derivation. `chamber_at` with an empty override map must still resolve
    // `stratum` from `addr.band` alone (the only thing the old, one-fewer-
    // parameter `chamber_at` ever did), and the resolved `origin` must be the
    // address-derived default, `Found` — this campaign digs nothing.
    for &addr in &existing {
        let chamber = chamber_at(seed, &cave, addr, &no_overrides).unwrap_or_else(|| {
            panic!("{addr:?} was measured to exist but chamber_at(None) returned None")
        });
        assert_eq!(chamber.addr, addr);
        assert_eq!(
            chamber.stratum,
            hornvale_climate::Realm::UNDERDARK.strata()[addr.band as usize],
            "stratum at {addr:?} must still be the pre-Task-4 pure function of \
             addr.band alone"
        );
        assert_eq!(
            chamber.origin,
            ChamberOrigin::Found,
            "with no override recorded, {addr:?} must resolve to the \
             address-derived default, Found"
        );
    }

    // The override wins.
    let mut overrides = BTreeMap::new();
    overrides.insert(overridden_addr, ChamberOrigin::Made);
    let overridden = chamber_at(seed, &cave, overridden_addr, &overrides)
        .expect("the overridden address was measured to exist");
    assert_eq!(
        overridden.origin,
        ChamberOrigin::Made,
        "an override fact must win over the address-derived default"
    );

    // A DIFFERENT address is unaffected by an override recorded for another
    // address entirely.
    let other = chamber_at(seed, &cave, other_addr, &overrides)
        .expect("the other address was measured to exist");
    assert_eq!(
        other.origin,
        ChamberOrigin::Found,
        "an override recorded for {overridden_addr:?} must not leak onto \
         {other_addr:?}"
    );

    // `Made` is absorbing: nothing takes a chamber back to `Found`. Exercised
    // directly on the resolver so the property holds independent of the fact
    // that today's derived default is always `Found` (see `resolve_origin`'s
    // own docs for why this is tested as a standalone function).
    assert_eq!(
        hornvale_worldgen::chamber::resolve_origin(ChamberOrigin::Made, Some(ChamberOrigin::Found)),
        ChamberOrigin::Made,
        "an override of Found must not pull a Made chamber back to Found — \
         Made is absorbing"
    );
    assert_eq!(
        hornvale_worldgen::chamber::resolve_origin(ChamberOrigin::Made, None),
        ChamberOrigin::Made,
        "the absence of an override must not pull a Made chamber back to Found"
    );
}
