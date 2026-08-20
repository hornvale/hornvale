//! The Deep Realm, Task 2: the chamber address lattice.
//!
//! Pure logic over `hornvale_worldgen::chamber` — no world-building fixture
//! is needed, because `chamber_exists`/`chamber_at` are pure functions of
//! their arguments and every one of those arguments (a `Cave`, a
//! `GeothermalGradient`, a `StratigraphicColumn`) is something a caller can
//! construct directly through terrain's own entry points. Not `#[ignore]`d:
//! this battery is cheap and belongs in the ordinary commit gate.
//!
//! **The lattice's depth axis moved in The Underworld** (`chamber/v2`, spec
//! §4.1): `ChamberAddr.band` indexes the delve ladder rather than the
//! stratigraphic one, so every fixture here now carries a gradient as well as
//! a reach, and the rung a fixture reaches is asserted rather than assumed.
//!
//! Both tests below exist to catch the campaign's named highest-risk defect
//! (rule 1, `docs/superpowers/plans/2026-08-05-the-deep-realm.md`): an
//! address must name a PLACE, never a construction step. See each test's own
//! doc comment for which half of that rule it guards.

use std::collections::{BTreeMap, BTreeSet};

use hornvale_kernel::{CellId, Seed};
use hornvale_terrain::{BandKind, Cave, CaveKind, DelveRung, GeothermalGradient, rung_at_depth};
use hornvale_worldgen::chamber::{
    BRANCHES_PER_SYSTEM, ChamberAddr, ChamberOrigin, FLOORS_PER_RUN_CEILING, chamber_at,
    chamber_exists, passages_from,
};

/// The column every fixture below is built against: 401 m of cover on 35 km
/// of continental crust, which is an ordinary land column the generator
/// produces in quantity. Band tops are `[0, 1, 401, 17700.5, 35000]` m.
///
/// **Fixtures are built through `Cave::from_reach` against this column**, so
/// each one's `deepest_band` is derived from its own metre budget and the pair
/// is a state the generator could actually author (The Underworld, spec §4.0).
/// Before that they were struct literals pairing a 1 km budget with
/// `BandKind::Roots`, whose top on any real column is ~14–20 km — an
/// impossible world, and exactly the shape that lets a suite go green over a
/// broken model once something downstream starts reading the budget.
fn fixture_column() -> hornvale_terrain::StratigraphicColumn {
    hornvale_terrain::column(
        35.0,
        0.3,
        true,
        400.0,
        1.0,
        hornvale_terrain::RockClass::Sandstone,
        hornvale_terrain::Basement::Continental,
    )
}

/// The geothermal gradient every fixture below is placed under, K/km.
///
/// **24.0 is the measured median band**, not a round number: the three
/// preregistered seeds report gradient p50 at 24.419 / 25.004 / 23.082 K/km
/// (`underworld_ladder_probe.rs`). It matters because `chamber_exists` gates
/// on the DELVE ladder since `chamber/v2`, so a fixture's reach in metres is
/// only half of what decides how far down the lattice it gets; the other half
/// is the cell it is in.
fn fixture_gradient() -> GeothermalGradient {
    GeothermalGradient::new(24.0)
}

/// A budget that stops inside the cover — `deepest_band` comes out `Cover`
/// (rank 1). Near the middle of the measured lava-tube/shallow-karst range.
///
/// On the delve ladder at [`fixture_gradient`] this is ΔT = 4.8 K, the
/// `Shallows` (rank 1) — which is what `chamber_exists` gates on now.
const SHALLOW_REACH_M: f64 = 200.0;

/// A budget that cuts past the 401 m basement contact — `deepest_band` comes
/// out `Basement` (rank 2). This is the median fault-void reach the 30-world
/// readout measures, not an invented number.
///
/// **`Basement` is the deepest band any cave can reach**, because a budget
/// capped at 3 km cannot get to `Roots` (~17.7 km on this column). Fixtures
/// that used to say `Roots` say this instead.
///
/// On the delve ladder at [`fixture_gradient`] this is ΔT = 48 K, the
/// `Underdeep` (rank 3) — two rungs below `SHALLOW_REACH_M`, where the
/// stratigraphic ladder separates the same pair by only one band. That
/// widening is the re-point's whole purpose and the tests below rely on it.
const DEEP_REACH_M: f64 = 2000.0;

/// The rule The Salt, 0102 and The Tolerance each learned separately:
/// generation order is never an identity. A `ChamberAddr` names a PLACE in a
/// lattice that exists before anything is generated into it, so nothing
/// about which chambers happen to exist can move another chamber's address.
///
/// Two caves differing ONLY in their depth budget both admit every address
/// with `band <= 1` (`Cover`'s own rank). A chamber at one of those addresses
/// must come out byte-identical under either cave — its content cannot have
/// been renumbered by the deeper cave having more chambers available to it.
#[test]
fn an_addresss_meaning_does_not_depend_on_which_other_chambers_exist() {
    let seed = Seed(90210);
    let cell = CellId(9);
    let col = fixture_column();
    let shallow = Cave::from_reach(CaveKind::Karst, SHALLOW_REACH_M, &col);
    let deep = Cave::from_reach(CaveKind::Karst, DEEP_REACH_M, &col);
    assert_eq!(shallow.deepest_band, BandKind::Cover);
    assert_eq!(deep.deepest_band, BandKind::Basement);
    // The gate is the DELVE ladder since chamber/v2, so the shared region is
    // decided by the rungs, not the bands. Pinned rather than assumed: if
    // either fixture's rung moves, the loop bound below stops being the shared
    // region and this test would silently start comparing addresses only one
    // cave admits — which it would pass, vacuously.
    assert_eq!(
        rung_at_depth(shallow.depth_reach_m, fixture_gradient()),
        DelveRung::Shallows
    );
    assert_eq!(
        rung_at_depth(deep.depth_reach_m, fixture_gradient()),
        DelveRung::Underdeep
    );

    // `Shallows` is rank 1, so bands 0..=1 (Undercroft, Shallows) are in BOTH
    // caves' budget; `Underdeep`'s rank 3 gives `deep` two further rungs
    // `shallow` cannot reach at all. Every address checked here therefore sits
    // in the region shared by both caves' budgets. (The pair was originally
    // Basement/Roots over bands 0..=2 on the stratigraphic ladder, then
    // Cover/Basement when `Roots` became unreachable under a metre budget; the
    // property is identical in all three framings.)
    let no_overrides = BTreeMap::new();
    for band in 0..=1u8 {
        for branch in 0..BRANCHES_PER_SYSTEM {
            let addr = ChamberAddr {
                cell,
                entrance: 0,
                band,
                branch,
                floor: 0,
            };
            assert_eq!(
                chamber_exists(seed, &shallow, fixture_gradient(), addr),
                chamber_exists(seed, &deep, fixture_gradient(), addr),
                "existence at {addr:?} differs between a shallow and a deep cave \
                 sharing the same seed and cell"
            );
            assert_eq!(
                chamber_at(
                    seed,
                    &shallow,
                    fixture_gradient(),
                    &col,
                    addr,
                    &no_overrides
                ),
                chamber_at(seed, &deep, fixture_gradient(), &col, addr, &no_overrides),
                "content at {addr:?} differs between a shallow and a deep cave — \
                 an address must name a PLACE, never a construction step"
            );
        }
    }
}

/// The lattice is a fixed size regardless of what any particular cave
/// realizes; occupancy within it is sparse and varies by seed. Over a cave
/// reaching the `Underdeep` rung, the address space checked here is
/// `BRANCHES_PER_SYSTEM * 3` (bands `0..=2`, `Undercroft..=Deeps`) — constant
/// across every seed — while the number of addresses that EXIST is strictly
/// less than that, and differs seed to seed. The loop stops one rung short of
/// the fixture's own budget on purpose: every address it probes is in budget,
/// so a `false` from `chamber_exists` can only mean the draw refused it, never
/// that the gate did.
/// claim: invariant(forall-seed) — lattice size is fixed, existence is
/// sparse and seed-varying (seedless sweep, audit §5: builds no world;
/// named explicitly in the task brief)
#[test]
fn the_lattice_is_fixed_and_existence_is_sparse() {
    let cell = CellId(42);
    let cave = Cave::from_reach(CaveKind::Fracture, DEEP_REACH_M, &fixture_column());
    assert!(
        rung_at_depth(cave.depth_reach_m, fixture_gradient()) >= DelveRung::Deeps,
        "the fixture must reach at least the Deeps for bands 0..=2 to all be \
         in budget; otherwise the sparseness below is measuring the gate"
    );

    let mut existing_counts = Vec::new();
    for raw_seed in [1u64, 2, 3, 4, 5] {
        let seed = Seed(raw_seed);
        let mut total = 0u32;
        let mut existing = 0u32;
        for band in 0..=2u8 {
            for branch in 0..BRANCHES_PER_SYSTEM {
                total += 1;
                let addr = ChamberAddr {
                    cell,
                    entrance: 0,
                    band,
                    branch,
                    floor: 0,
                };
                if chamber_exists(seed, &cave, fixture_gradient(), addr) {
                    existing += 1;
                }
            }
        }
        assert_eq!(
            total,
            u32::from(BRANCHES_PER_SYSTEM) * 3,
            "the address space over an Underdeep-reaching cave must be a constant \
             BRANCHES_PER_SYSTEM * 3 over the bands probed"
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
    let cave = Cave::from_reach(CaveKind::Fracture, DEEP_REACH_M, &fixture_column());

    for raw_seed in [1u64, 2, 3, 4, 5] {
        let seed = Seed(raw_seed);
        for raw_cell in [0u32, 1, 9, 42] {
            let cell = CellId(raw_cell);
            for band in 0..=2u8 {
                for branch in 0..BRANCHES_PER_SYSTEM {
                    let addr = ChamberAddr {
                        cell,
                        entrance: 0,
                        band,
                        branch,
                        floor: 0,
                    };
                    for &neighbour in &passages_from(seed, &cave, fixture_gradient(), addr) {
                        let back = passages_from(seed, &cave, fixture_gradient(), neighbour);
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
/// canonical entrance address (`branch = 0, band = 0, floor = 0`) has at most
/// two lattice neighbours (`branch 1/band 0` and `branch 0/band 1`) plus needs to
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
    let cave = Cave::from_reach(CaveKind::Fracture, DEEP_REACH_M, &fixture_column());

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
                branch: 0,
                floor: 0,
            };
            probed += 1;
            if chamber_exists(seed, &cave, fixture_gradient(), entrance) {
                entrance_exists += 1;
            }
            if !passages_from(seed, &cave, fixture_gradient(), entrance).is_empty() {
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
    let col = fixture_column();
    let cave = Cave::from_reach(CaveKind::Fracture, DEEP_REACH_M, &col);
    let cell = CellId(4);

    // Find two addresses that both exist under this (seed, cave, cell) —
    // one to override, one to leave alone as the "unaffected" witness.
    let mut existing = Vec::new();
    for band in 0..=2u8 {
        for branch in 0..BRANCHES_PER_SYSTEM {
            let addr = ChamberAddr {
                cell,
                entrance: 0,
                band,
                branch,
                floor: 0,
            };
            if chamber_exists(seed, &cave, fixture_gradient(), addr) {
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
    // derivation in the half this test owns — the resolved `origin` must be
    // the address-derived default, `Found`, because this campaign digs
    // nothing. (`stratum` is NOT that pure function of `addr.band` any more:
    // since `chamber/v2` the band indexes the delve ladder, and the stratum is
    // read off the cell's own column. That is
    // `a_chamber_reports_both_its_rung_and_its_stratum`'s subject, not this
    // test's; here it is only asserted to be populated consistently.)
    for &addr in &existing {
        let chamber = chamber_at(seed, &cave, fixture_gradient(), &col, addr, &no_overrides)
            .unwrap_or_else(|| {
                panic!("{addr:?} was measured to exist but chamber_at(None) returned None")
            });
        assert_eq!(chamber.addr, addr);
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
    let overridden = chamber_at(
        seed,
        &cave,
        fixture_gradient(),
        &col,
        overridden_addr,
        &overrides,
    )
    .expect("the overridden address was measured to exist");
    assert_eq!(
        overridden.origin,
        ChamberOrigin::Made,
        "an override fact must win over the address-derived default"
    );

    // A DIFFERENT address is unaffected by an override recorded for another
    // address entirely.
    let other = chamber_at(
        seed,
        &cave,
        fixture_gradient(),
        &col,
        other_addr,
        &overrides,
    )
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

/// The two ladders are independent: a chamber says both what depth CLASS it
/// is and what ROCK it is in, and **neither answer is computable from the
/// other** (spec §4.1). Before The Underworld it was one answer wearing two
/// coats — `stratum` was `Realm::UNDERDARK.strata()[addr.band]`, i.e. the
/// address restated.
///
/// Asserted in both directions, because either alone is satisfiable by a
/// degenerate mapping:
///
/// - two chambers that **share a stratum and differ in rung** (a constant
///   `stratum` would also satisfy this, which is why the second half exists);
/// - two chambers that **share a rung and differ in stratum**, reached by
///   putting the same rung under two different gradients — the one thing a
///   function of `addr.band` alone cannot do.
#[test]
fn a_chamber_reports_both_its_rung_and_its_stratum() {
    let seed = Seed(11);
    let col = fixture_column();
    let cave = Cave::from_reach(CaveKind::Fracture, DEEP_REACH_M, &col);
    let cell = CellId(3);
    let no_overrides: BTreeMap<ChamberAddr, ChamberOrigin> = BTreeMap::new();

    // Direction 1: same stratum, different rung. On this column the basement
    // contact is at 401 m, and at 24 K/km the Shallows (2 K) begin at 83 m and
    // the Deeps (8 K) at 333 m — both still in the cover; the Underdeep (25 K)
    // at 1042 m and the Nadir (50 K) at 2083 m are both in the basement. So
    // the sweep sees two distinct rung pairs that share a stratum.
    //
    // **The sweep covers the FLOOR axis too, and that is a widening rather
    // than a rescue** (The Stope, `chamber/v3`). The lattice gained a `floor`,
    // so "every address in the fixture's budget" is 20× the set it used to be;
    // sweeping only `floor = 0` would enumerate a 1/20 slice and call it the
    // lattice. It also matters here in particular: the epoch relocated every
    // existence draw, and at `floor = 0` alone this fixture happens to realize
    // no `Shallows` chamber at all, so the `Shallows`/`Deeps` pair that shares
    // the cover is invisible in that slice. The assertion below is unchanged.
    let mut seen: Vec<(DelveRung, hornvale_climate::Stratum)> = Vec::new();
    for band in 0..=3u8 {
        for branch in 0..BRANCHES_PER_SYSTEM {
            for floor in 0..FLOORS_PER_RUN_CEILING {
                let addr = ChamberAddr {
                    cell,
                    entrance: 0,
                    band,
                    branch,
                    floor,
                };
                if let Some(chamber) =
                    chamber_at(seed, &cave, fixture_gradient(), &col, addr, &no_overrides)
                {
                    seen.push((chamber.rung, chamber.stratum));
                }
            }
        }
    }
    assert!(
        !seen.is_empty(),
        "no chamber existed anywhere in the fixture's budget — the assertions \
         below would be vacuous"
    );
    let shared_stratum_differing_rung = seen.iter().any(|&(rung_a, stratum_a)| {
        seen.iter()
            .any(|&(rung_b, stratum_b)| stratum_a == stratum_b && rung_a != rung_b)
    });
    assert!(
        shared_stratum_differing_rung,
        "no two chambers shared a stratum while differing in rung, so `rung` \
         could still be a relabelling of `stratum`: {seen:?}"
    );

    // Direction 2: same rung, different stratum — the half that a pure
    // function of `addr.band` cannot produce. The Deeps begin at 8 K, which is
    // 533 m under a cool craton (basement, contact at 401 m) and 267 m under
    // hot young crust (still cover).
    let deeps = ChamberAddr {
        cell,
        entrance: 0,
        band: 2,
        branch: 0,
        floor: 0,
    };
    let cool = GeothermalGradient::new(15.0);
    let hot = GeothermalGradient::new(30.0);
    // Both cells must admit the address at all for the comparison to mean
    // anything; `chamber_exists` is gated per-gradient, so this is not free.
    let under_cool = chamber_at(seed, &cave, cool, &col, deeps, &no_overrides);
    let under_hot = chamber_at(seed, &cave, hot, &col, deeps, &no_overrides);
    let (under_cool, under_hot) = match (under_cool, under_hot) {
        (Some(a), Some(b)) => (a, b),
        other => panic!(
            "the Deeps address must exist under both gradients for this \
             comparison to be non-vacuous; got {other:?}"
        ),
    };
    assert_eq!(
        under_cool.rung, under_hot.rung,
        "the same address must name the same rung whatever the cell"
    );
    assert_ne!(
        under_cool.stratum, under_hot.stratum,
        "the same rung under a 15 K/km and a 30 K/km cell sits at 533 m and \
         267 m, which straddle this column's 401 m basement contact — so the \
         strata must differ. They do not, which means `stratum` is not being \
         read from the cell at all."
    );
}

/// The chamber key names a DELVE rung, from an explicit table — the
/// save-format discipline `chamber_key`'s own doc states, restated at the
/// public boundary where a reader who never opens `chamber.rs` will meet it.
///
/// Asserted through the observable this test can actually reach: two addresses
/// differing ONLY in `band` must produce different chambers, and the rung each
/// reports must round-trip through the band index. A key that numbered its
/// band instead of naming it would still pass that — which is why the string
/// itself is pinned in `chamber.rs`'s own module tests, where `chamber_key` is
/// visible. This test guards the half that IS observable from outside: that
/// `addr.band` and `Chamber::rung` are the same ladder, in the same order.
#[test]
fn the_bands_index_and_the_reported_rung_are_the_same_ladder() {
    let seed = Seed(7);
    let col = fixture_column();
    let cave = Cave::from_reach(CaveKind::Fracture, DEEP_REACH_M, &col);
    let no_overrides: BTreeMap<ChamberAddr, ChamberOrigin> = BTreeMap::new();

    let mut by_band: Vec<(u8, DelveRung)> = Vec::new();
    for raw_cell in 0u32..40 {
        for band in 0..=3u8 {
            for branch in 0..BRANCHES_PER_SYSTEM {
                let addr = ChamberAddr {
                    cell: CellId(raw_cell),
                    entrance: 0,
                    band,
                    branch,
                    floor: 0,
                };
                if let Some(chamber) =
                    chamber_at(seed, &cave, fixture_gradient(), &col, addr, &no_overrides)
                {
                    by_band.push((band, chamber.rung));
                }
            }
        }
    }
    let bands_seen: BTreeSet<u8> = by_band.iter().map(|&(b, _)| b).collect();
    assert_eq!(
        bands_seen.len(),
        4,
        "the sweep must reach every band 0..=3 or the ordering check below is \
         only partly exercised; saw {bands_seen:?}"
    );
    for &(band_a, rung_a) in &by_band {
        for &(band_b, rung_b) in &by_band {
            assert_eq!(
                band_a.cmp(&band_b),
                rung_a.cmp(&rung_b),
                "band {band_a} reports {rung_a:?} and band {band_b} reports \
                 {rung_b:?} — the index and the ladder disagree on order"
            );
        }
    }
}
