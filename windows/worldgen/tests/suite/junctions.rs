//! Junctions — the underworld becomes a network (The Stope, Task 6).
//!
//! `junctions_at` derives, from terrain FACTS alone, which other cave
//! systems a chamber's system joins at a shared delve band. This realizes
//! `MAP-underworld-shortcut`: "hard to enter, easy to traverse once inside;
//! two points far apart on the surface can be close below" — false by
//! construction before this task, because one cave never connected to
//! another.
//!
//! The task's own Step 1 list, plus what review round 1 found missing:
//!
//! 1. `a_junction_is_the_derivation_rule_and_nothing_else` — THE constraint.
//!    A junction is a fact about the geology, not a die roll.
//! 2. `junctions_are_symmetric` — if A reaches B, B reaches A.
//! 3. `a_junction_never_crosses_a_band` — two systems join at a SHARED band
//!    or not at all; otherwise a junction is a vertical teleport and the
//!    depth ladder means nothing.
//! 4. `nowhere_has_no_junctions` / `the_projection_ignores_entrance_branch_
//!    and_floor` — the two halves of the address convention, which
//!    `passages_from` states and `junctions_at` did not.
//! 5. `the_character_gate_actually_binds` — the character axis is visible to
//!    the network, non-vacuously.
//!
//! # WHY TEST 1 RE-DERIVES RATHER THAN RE-INVOKES
//!
//! Its first version built a local [`hornvale_kernel::seed::Stream`],
//! advanced it a thousand times, and asserted two identical calls agreed.
//! That could not fail for the reason it named. `Stream` is a plain value
//! type and there is no global RNG in this project, so the loop influenced
//! nothing a second call could read — and a `junctions_at` that *did* draw
//! would derive a fresh seed-keyed stream per call and be exactly as
//! invariant. Review proved it: a mutation making each link a literal coin
//! flip keyed on the unordered cell pair left all three junction tests green
//! and the whole 300-test crate green with it.
//!
//! So the assertion is now an **independent re-derivation** of the
//! four-clause predicate `junctions_at`'s own doc comment states, computed
//! here from `cave_at` / `chamber_exists` / `character_at` / `bands_of` /
//! `neighbors` without calling `junctions_at`, and compared as a set. A coin
//! flip drops links the rule admits, so it goes red immediately. A stream
//! roster check would NOT have caught that mutation — it used an inline
//! `StreamLabel::dynamic`, which no `stream_labels()` roster can see.

use hornvale_kernel::{Band, Geosphere, Seed, Vertex};
use hornvale_terrain::{GeneratedTerrain, TerrainPins};
use hornvale_worldgen::chamber::{
    BRANCHES_PER_SYSTEM, ChamberAddr, LEVELS_PER_BRANCH_CEILING, chamber_exists, junctions_at,
    rung_rank,
};
use hornvale_worldgen::character::{bands_of, character_at};

/// The production mesh level and seed the panel tests share with this
/// crate's other seed-42 batteries (`gazetteer_naming.rs`'s idiom), so the
/// junction population measured here is the population of the canonical
/// globe.
const LEVEL: u32 = 6;

/// How many systems [`the_projection_ignores_level`] walks in full. A cost
/// bound, not a claim about the population — that test's own doc says why,
/// and its non-vacuity assertion fails if this is ever set so low that
/// nothing is checked.
///
/// (It cited `the_projection_ignores_branch_and_level` until Task 9's review.
/// That name was real — minted by Task 4, retired by Task 7 when `branch`
/// stopped being ignored — but a doc citing a symbol the tree no longer has
/// sends a reader looking for a test that is not there.)
const PROJECTION_SYSTEMS: usize = 40;

/// Seed-42 terrain built through the ordinary genesis path — the same
/// `hornvale_terrain::generate` -> `GeneratedTerrain::new` chain every other
/// seed-42 test in this crate uses.
fn panel_terrain() -> GeneratedTerrain {
    let geo = Geosphere::new(LEVEL);
    let outcome = hornvale_terrain::generate(Seed(42), &geo, &TerrainPins::default())
        .expect("default pins generate");
    GeneratedTerrain::new(geo, outcome)
}

/// The habitation bands, as `(rank, rung)` in rank order — **derived from the
/// delve ladder, never restated as a literal.**
///
/// This helper exists because the first version of this file walked
/// `0..5u8`, the seventh instance of this campaign's signature defect and one
/// commit family after `a910ebab3` fixed the same thing in the readout. A
/// sixth habitation rung would have compiled that loop unchanged and all
/// three junction tests would have silently stopped covering the new band:
/// green, with a shrunken corpus.
///
/// `chamber::rung_of_rank` is private, so the route from a test crate is the
/// ladder itself (`hornvale_terrain::rungs`) filtered through the lattice's
/// one explicit mapping ([`rung_rank`]) — both `pub`. `Surface` has no
/// habitation rank and drops out here exactly as it does there.
fn habitation_bands() -> Vec<(u8, Band)> {
    let mut bands: Vec<(u8, Band)> = hornvale_terrain::rungs()
        .iter()
        .filter_map(|&rung| rung_rank(rung).map(|rank| (rank, rung)))
        .collect();
    bands.sort_by_key(|&(rank, _)| rank);
    bands
}

/// Every cave-bearing cell of the panel world, in cell order.
///
/// No `is_ocean` filter: `cave_at` refuses an ocean cell as its first act, so
/// a test-side ocean guard is the same dead pairing review found inside
/// `junctions_at` (round 1, F8).
fn cave_cells(terrain: &GeneratedTerrain) -> Vec<Vertex> {
    terrain
        .geosphere()
        .vertices()
        .filter(|&cell| terrain.cave_at(cell).is_some())
        .collect()
}

/// Whether one system realizes a chamber at `(band, branch)` **and** that
/// branch's character can occupy that rung — clauses 3 and 4 of the
/// derivation rule, re-derived here from the shipped primitives rather than
/// read back out of `junctions_at`.
///
/// **Branch-scoped since The Drift's Task 7** (spec §4.6). It used to ask
/// only about the MAIN LINE, because a junction was a fact about
/// `(cell, band)` and every branch could walk sideways to branch 0 to use
/// its door. `passages_from` has no lateral rule any more, so the door
/// belongs to the branch that can actually reach it.
fn branch_admits(
    seed: Seed,
    terrain: &GeneratedTerrain,
    cell: Vertex,
    branch: u8,
    rung: Band,
) -> bool {
    let Some(cave) = terrain.cave_at(cell) else {
        return false;
    };
    let here = ChamberAddr {
        cell,
        branch,
        band: rung,
        level: 0,
    };
    if !chamber_exists(seed, &cave, terrain.geothermal_gradient_at(cell), here) {
        return false;
    }
    bands_of(character_at(seed, here)).contains(&rung)
}

/// The canonical address of one system at one `(band, branch)` — what the
/// whole panel scan asks from, and the only address shape `junctions_at`
/// projects onto.
fn canonical(cell: Vertex, band: Band, branch: u8) -> ChamberAddr {
    ChamberAddr {
        cell,
        branch,
        band,
        level: 0,
    }
}

/// Every `(asked, answered)` junction pair in the world, as ORDERED pairs —
/// both directions of every edge, deliberately.
///
/// It filtered `far.cell > addr.cell` for one commit, which made
/// `junctions_are_symmetric` blind in one direction: "`junctions_at(Y)` names
/// X but `junctions_at(X)` does not name Y", for X < Y, produced no pair from
/// either side and was never checked. Review demonstrated it with mirror
/// mutations — dropping some larger-id neighbours stayed green, dropping some
/// smaller-id ones went red. The dedup bought nothing; the scan is ~0.3 s.
fn all_junction_edges(terrain: &GeneratedTerrain) -> Vec<(ChamberAddr, ChamberAddr)> {
    let mut edges = Vec::new();
    for cell in cave_cells(terrain) {
        for &(_, rung) in &habitation_bands() {
            // Every branch, not just the main line: a junction is a fact
            // about `(cell, band, branch)` since Task 7, so a main-line-only
            // scan would leave the whole re-scoping unmeasured.
            for branch in 0..BRANCHES_PER_SYSTEM {
                let addr = canonical(cell, rung, branch);
                for far in junctions_at(Seed(42), terrain, addr) {
                    edges.push((addr, far));
                }
            }
        }
    }
    edges
}

/// THE constraint (Task 6, step 1): a junction is DERIVED, not drawn — it is
/// exactly the four-clause rule stated in `junctions_at`'s doc comment, and
/// nothing else. Re-derived independently here across the whole panel and
/// compared as a set; see this module's header for why re-invocation after
/// unrelated draws could not have asserted this.
#[test]
fn a_junction_is_the_derivation_rule_and_nothing_else() {
    let terrain = panel_terrain();
    let geo = terrain.geosphere();
    let cells = cave_cells(&terrain);
    let bands = habitation_bands();

    let mut answered = 0usize;
    for &cell in &cells {
        for &(rank, rung) in &bands {
            for branch in 0..BRANCHES_PER_SYSTEM {
                // Clauses 3 and 4 for the asking side; clause 1 is entailed
                // by `cave_at` inside `branch_admits`.
                let expected: Vec<Vertex> = if branch_admits(Seed(42), &terrain, cell, branch, rung)
                {
                    let mut near: Vec<Vertex> = geo
                        .neighbors(cell)
                        .iter()
                        .copied()
                        // Clause 2 is the geosphere's own adjacency, and
                        // clauses 1/3/4 again for the far side — at the
                        // SAME branch, which is the re-scoping.
                        .filter(|&far| branch_admits(Seed(42), &terrain, far, branch, rung))
                        .collect();
                    near.sort();
                    near
                } else {
                    Vec::new()
                };
                let shipped: Vec<Vertex> =
                    junctions_at(Seed(42), &terrain, canonical(cell, rung, branch))
                        .iter()
                        .map(|a| a.cell)
                        .collect();
                assert_eq!(
                    shipped, expected,
                    "junctions_at disagrees with the derivation rule at cell \
                     {cell:?} band {rank} branch {branch}"
                );
                answered += shipped.len();
            }
        }
    }

    // Non-vacuity: the rule must admit something on this world, or every
    // comparison above was between two empty vectors.
    assert!(
        answered > 0,
        "seed 42 produced no junctions at all — the network never formed"
    );
}

/// If A reaches B, B reaches A (Task 6, step 1). Asked from either endpoint,
/// the same relation answers — there is one junction, not two directed ones.
/// Checked over EVERY ordered pair, so a rule that dropped neighbours on one
/// side of a cell-id comparison cannot hide.
#[test]
fn junctions_are_symmetric() {
    let terrain = panel_terrain();
    let edges = all_junction_edges(&terrain);
    assert!(
        !edges.is_empty(),
        "seed 42 produced no junctions at all — symmetry holds vacuously"
    );
    for (a, b) in &edges {
        let back = junctions_at(Seed(42), &terrain, *b);
        assert!(
            back.iter().any(|c| c.cell == a.cell),
            "junction {a:?} -> {b:?} is not symmetric: asked from B, A is absent"
        );
    }
}

/// Two systems join at a SHARED band or not at all (Task 6, step 1): every
/// junction endpoint sits at exactly the band it was asked from, on the
/// neighbour system's main line, in a geographically adjacent cell. Anything
/// else would be a vertical teleport, and the depth ladder would mean
/// nothing.
#[test]
fn a_junction_never_crosses_a_band() {
    let terrain = panel_terrain();
    let geo = terrain.geosphere();
    for (a, b) in all_junction_edges(&terrain) {
        assert_eq!(b.band, a.band, "junction {a:?} -> {b:?} changed bands");
        assert_eq!(
            b.branch, a.branch,
            "junction {a:?} -> {b:?} changed branches — a junction joins branch \
             b of one system to branch b of its neighbour (The Drift, §4.6)"
        );
        assert_eq!(b.level, 0, "junction endpoint named a level");
        assert_ne!(b.cell, a.cell, "a system joined itself");
        let neighbours: Vec<Vertex> = geo.neighbors(a.cell).to_vec();
        assert!(
            neighbours.contains(&b.cell),
            "junction {a:?} -> {b:?} crossed to a non-adjacent cell"
        );
    }
}

/// A non-existent address has no junctions, exactly as it has no passages
/// (review round 1, F3). `passages_from` states and enforces this convention;
/// `junctions_at` gated only the main line, so a `branch: 99` address
/// answered with three junctions while `passages_from` answered with none —
/// a disagreement any consumer composing the two into one traversal graph
/// would have inherited.
#[test]
fn nowhere_has_no_junctions() {
    let terrain = panel_terrain();
    let edges = all_junction_edges(&terrain);
    assert!(
        !edges.is_empty(),
        "seed 42 produced no junctions at all — nothing to ask from"
    );
    let (live, _) = edges[0];
    assert!(
        !junctions_at(Seed(42), &terrain, live).is_empty(),
        "the canonical address must still answer, or this test proves nothing"
    );

    // Out of the lattice on each of the two axes the address space bounds.
    for nowhere in [
        ChamberAddr {
            branch: BRANCHES_PER_SYSTEM,
            ..live
        },
        ChamberAddr {
            level: LEVELS_PER_BRANCH_CEILING,
            ..live
        },
    ] {
        assert!(
            junctions_at(Seed(42), &terrain, nowhere).is_empty(),
            "{nowhere:?} does not exist, yet it answered with junctions"
        );
    }
}

/// The other half of the address convention: **`level` is ignored by the
/// PROJECTION** — every level of one run stands on the same far side of the
/// same doors, so any *existing* address of a run gives that run's canonical
/// (level-0) answer.
///
/// **`branch` was on this list until The Drift's Task 7 and is not any
/// more** (spec §4.6), which is why this test was renamed rather than
/// edited: its former name, `the_projection_ignores_branch_and_level`, names
/// a property that is now false, and a test whose name asserts the opposite
/// of what it checks is worse than no test. The branch half has its own test below,
/// `the_projection_is_scoped_to_the_branch`, which demands the two actually
/// disagree somewhere.
///
/// Documented since the function was written and untested until The Stope's
/// review round 1 — which is how the existence half above went unnoticed
/// underneath it.
///
/// The scan is capped at [`PROJECTION_SYSTEMS`] systems rather than run over
/// the whole panel: the address space it walks is `branches x levels` per
/// band, which is orders of magnitude more `junctions_at` calls than the
/// panel scan the other tests pay for. The cap is a cost bound, and the
/// assertion below fails if it ever bounds the sample down to nothing.
#[test]
fn the_projection_ignores_level() {
    let terrain = panel_terrain();
    let mut checked = 0usize;
    for cell in cave_cells(&terrain).into_iter().take(PROJECTION_SYSTEMS) {
        let Some(cave) = terrain.cave_at(cell) else {
            continue;
        };
        let gradient = terrain.geothermal_gradient_at(cell);
        for &(_, rung) in &habitation_bands() {
            for branch in 0..BRANCHES_PER_SYSTEM {
                let canon = canonical(cell, rung, branch);
                let expected = junctions_at(Seed(42), &terrain, canon);
                for level in 0..LEVELS_PER_BRANCH_CEILING {
                    let other = ChamberAddr { level, ..canon };
                    if other == canon || !chamber_exists(Seed(42), &cave, gradient, other) {
                        continue;
                    }
                    assert_eq!(
                        junctions_at(Seed(42), &terrain, other),
                        expected,
                        "{other:?} answered differently from its run's canonical address"
                    );
                    checked += 1;
                }
            }
        }
    }
    assert!(
        checked > 0,
        "no non-canonical address existed in the sample — the projection was never tested"
    );
}

/// **The junction relation is scoped to the BRANCH, not merely to the band**
/// (The Drift, spec §4.6) — and this test exists because the re-scoping is
/// otherwise unobservable: if every branch of a system happened to answer
/// what branch 0 answers, the change would be a no-op that
/// `the_projection_ignores_level` and the rule test would both wave through.
///
/// So it demands an actual disagreement somewhere on the panel: two existing
/// addresses of ONE system at ONE band, differing only in `branch`, whose
/// junction sets differ. An entrance is not an address axis (amendment A.3),
/// so `entrance_count` plays no part in the sweep below.
///
/// The reason a disagreement must exist is structural rather than
/// statistical: `branch_count_of` and `character_of` are both keyed on
/// `(cell, band, branch)` since Task 5, so two branches of one system at one
/// band can differ in whether the NEIGHBOUR realizes them and in whether
/// their character can occupy the rung. Either difference moves the answer.
#[test]
fn the_projection_is_scoped_to_the_branch() {
    let terrain = panel_terrain();
    let mut disagreements = 0usize;
    let mut compared = 0usize;
    for cell in cave_cells(&terrain) {
        let Some(cave) = terrain.cave_at(cell) else {
            continue;
        };
        let gradient = terrain.geothermal_gradient_at(cell);
        for &(_, rung) in &habitation_bands() {
            let base = canonical(cell, rung, 0);
            if !chamber_exists(Seed(42), &cave, gradient, base) {
                continue;
            }
            let expected = junctions_at(Seed(42), &terrain, base);
            for branch in 1..BRANCHES_PER_SYSTEM {
                let other = canonical(cell, rung, branch);
                if !chamber_exists(Seed(42), &cave, gradient, other) {
                    continue;
                }
                compared += 1;
                let answer = junctions_at(Seed(42), &terrain, other);
                // Whatever it answers, it answers about ITS OWN branch.
                for far in &answer {
                    assert_eq!(
                        far.branch, branch,
                        "{other:?} answered with an endpoint on another branch"
                    );
                }
                let same_cells: Vec<Vertex> = answer.iter().map(|a| a.cell).collect();
                let base_cells: Vec<Vertex> = expected.iter().map(|a| a.cell).collect();
                if same_cells != base_cells {
                    disagreements += 1;
                }
            }
        }
    }
    assert!(
        compared > 0,
        "no system on the panel had two existing branches at one band — the \
         comparison never ran"
    );
    assert!(
        disagreements > 0,
        "every branch of every system answered exactly what its main line \
         answered ({compared} comparisons) — the (band, branch) scoping is \
         indistinguishable from the (band) scoping it replaced"
    );
    println!("junction branch scoping: {disagreements} of {compared} branch pairs disagree");
}

/// Past the habitation ladder there is no shared rung to stand on, so there
/// are no junctions (review round 1, F11). Nothing else in the tree asks:
/// every loop over bands — this file's and the readout's alike — now derives
/// its bound from the ladder, which is correct and leaves this arm
/// unexercised. So it is exercised here, deliberately.
///
/// **`Band::Surface` is the address to probe now, not a numeral past the
/// ladder's end** (The Drift, Task 4): with `band` promoted from a bare rank
/// to a typed [`Band`], there is no value past `Nadir` left to construct —
/// every `Band` is a real rung. `Surface` is the one value the type still
/// admits that names no habitation rank, which is exactly the shape this
/// test needs.
#[test]
fn a_junction_past_the_ladder_is_empty() {
    let terrain = panel_terrain();
    let edges = all_junction_edges(&terrain);
    assert!(!edges.is_empty(), "seed 42 produced no junctions at all");
    let (live, _) = edges[0];
    assert!(
        junctions_at(
            Seed(42),
            &terrain,
            ChamberAddr {
                band: Band::Surface,
                ..live
            }
        )
        .is_empty(),
        "Band::Surface is not a habitation rung, yet it answered with junctions"
    );
}

/// The character gate BINDS (review round 1, F11): there is at least one
/// system on the panel whose main line realizes a chamber at a band, whose
/// character cannot occupy that rung, and which has a neighbour that would
/// otherwise have joined it there. Without this, widening `bands_of` — giving
/// one tier the whole ladder, say — would make the character axis invisible
/// to the network with no test red, only a number moving in an artifact.
///
/// Review measured the gate binding in 126 of 1322 cases (9.5%) on seed 42.
/// This asserts the existence of such a case, not the count: the count is a
/// property of the world and belongs in the witness, not in an assertion.
#[test]
fn the_character_gate_actually_binds() {
    let terrain = panel_terrain();
    let geo = terrain.geosphere();
    let mut refused_with_a_willing_neighbour = 0usize;
    for cell in cave_cells(&terrain) {
        let Some(cave) = terrain.cave_at(cell) else {
            continue;
        };
        let gradient = terrain.geothermal_gradient_at(cell);
        for &(_rank, rung) in &habitation_bands() {
            let main = canonical(cell, rung, 0);
            // The chamber is there...
            if !chamber_exists(Seed(42), &cave, gradient, main) {
                continue;
            }
            // ...and the character is what refuses the rung.
            if bands_of(character_at(Seed(42), main)).contains(&rung) {
                continue;
            }
            // A neighbour that would have joined but for that refusal.
            if !geo
                .neighbors(cell)
                .iter()
                .any(|&far| branch_admits(Seed(42), &terrain, far, 0, rung))
            {
                continue;
            }
            refused_with_a_willing_neighbour += 1;
            assert!(
                junctions_at(Seed(42), &terrain, main).is_empty(),
                "{main:?} was refused by the character gate, yet it answered with junctions"
            );
        }
    }
    assert!(
        refused_with_a_willing_neighbour > 0,
        "the character gate never removed a link on seed 42 — the character axis is \
         invisible to the network, and this test cannot see a widening of `bands_of`"
    );
}
