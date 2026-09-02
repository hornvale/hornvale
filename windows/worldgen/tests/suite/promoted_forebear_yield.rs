//! Task 1 (The Avowal, spec §5): the promoted-forebear yield probe — a **kill
//! criterion**. `parent-of` (spec §4.3) is only worth committing if a
//! meaningful share of promoted founders have a forebear the world *also*
//! promoted; a `kin-of`/`parent-of` predicate that almost never resolves
//! would be a name with nothing behind it, the exact failure §2 of the spec
//! documents for the trope audit itself.
//!
//! ## What "yield" means here, and why the denominator is not a free choice
//!
//! For a promoted founder `f` (a member of
//! [`hornvale_worldgen::person_promote::FounderCast::remembered`]),
//! [`hornvale_worldgen::forebear_of`] may return `None` (no entity forebear —
//! a genesis occupation, or a species with no derivable generation length) or
//! `Some((forebear_handle, kinship))`. That `forebear_handle` names *an*
//! occupation's founder, promoted or not — [`hornvale_worldgen::founder_of`]
//! is total over any occupation, promoted status plays no part in computing
//! it. Whether the forebear was **itself promoted** is a second, independent
//! question this probe answers by recomputing `founder_of` for every
//! promoted founder's own occupation and testing set membership.
//!
//! ```text
//!   yield = (promoted founders whose forebear is ALSO promoted)
//!           / (all promoted founders)
//! ```
//!
//! Roots (`forebear_of` returns `None`) and non-root-but-unpromoted-forebear
//! cases both count against this denominator — a founder either has a usable
//! `parent-of` edge or it does not, and both failure modes look the same to
//! a reader of the ledger. The other defensible denominator — excluding
//! roots, i.e. "of founders who *have* a forebear at all, how many have a
//! promoted one" — is reported alongside, labelled, but the spec's kill
//! criterion (§5: "if the median yield is under 10%, §4.3 does not ship") is
//! stated against the first denominator and only the first is load-bearing
//! here.
//!
//! ## A handle-space trap this probe had to avoid
//!
//! [`hornvale_worldgen::person_promote::Founder::handle`] and the return of
//! [`hornvale_worldgen::founder_of`] are **not the same bits** for the same
//! occupation, even though both claim to identify "this founding." `Founder`
//! is built from `founder_handle` (`domains/history/src/flesh.rs`), which
//! folds `ended`, `peak_population` and a role discriminant into the
//! founding-identity key and never touches the world seed — it exists to
//! deduplicate handles *within one promotion pass*. `founder_of`
//! (`windows/worldgen/src/descent.rs`) folds none of that in and instead XORs
//! the founding-identity key with the world seed — it exists to key a
//! *rendered name*, a disjoint purpose. Comparing `Founder::handle` against
//! `forebear_of`'s returned handle would therefore almost never match, for
//! any occupation, promoted or not — a bug that would silently report a
//! near-zero yield with no world actually lacking promoted ancestors. This
//! probe never reads `Founder::handle`; it recomputes `founder_of(world,
//! f.community)` for every promoted founder and builds the promoted-handle
//! set from that, the same identity space `forebear_of` itself returns into.
//!
//! ## A finding for Task 5, not a bug in this probe: `founder_of` collides
//!
//! Cross-checked directly (not committed here, ad hoc): at seed 42,
//! `founder_of` maps 1,212 occupations onto only 1,169 distinct `RoleHandle`
//! values — 42 colliding groups, ~3.5% of occupations sharing a handle with
//! at least one unrelated occupation. That is expected: `founder_of`
//! (`windows/worldgen/src/descent.rs`) is exactly `founding_key_from(...) ^
//! seed`, with **none** of `founder_handle`'s extra discrimination fold
//! (`ended`, `peak_population`, a role tag) — the fold `select_founders`
//! needed precisely because the identity key alone collides (The Salt's own
//! measured 8.4%/3.3%/3.6% *stem*-collision figures at seeds 42/7/1000 are
//! this same phenomenon's downstream, name-rendering symptom).
//!
//! This probe's yield reading is **the number Task 5 would actually produce
//! if it commits `parent-of` by matching `forebear_of`'s returned handle
//! against promoted founders' handles** — the only forebear-identification
//! path the public API offers, so the collision noise it carries is real,
//! not an artifact of this measurement. A direct entity-identity cross-check
//! (reading `occ-founded-from` and testing occupation-EntityId membership
//! instead of `RoleHandle` membership — bypassing `forebear_of` entirely)
//! measured seed 42 at 93/76/35/204, exactly the spec §4.3 reference — two
//! fewer `forebear_promoted` than this probe's handle-based 95/74/35/204.
//! Both readings clear the 10% kill criterion by a wide margin, so this
//! collision has **no bearing on Task 1's verdict**. It bears on Task 5's
//! design instead: if `parent-of` is committed by handle-matching, roughly
//! 1-in-100 promoted founders here could be attributed to the wrong
//! forebear, which is directly relevant to spec §4.3's requirement that
//! `parent-of` be `functional` "if and only if a founder can have at most
//! one recorded forebear" — a collision can make one handle resolve to more
//! than one promoted person. Flagged in the ledger for Task 5's implementer;
//! not fixed here, since fixing it means adding a discrimination fold to
//! `founder_of`/`forebear_of` itself, a `descent.rs` change out of Task 1's
//! scope (measurement only, no production code).
//!
//! ## The seed panel
//!
//! 25 seeds (`PANEL`): seed 42 (the committed fixture this probe's numbers
//! are checked against) plus 1..=24, spread rather than clustered so a
//! result is not an artifact of one narrow seed neighbourhood. 25 is enough
//! to read a stable median (an even split either way still leaves 12 seeds
//! on the losing side) while staying inside a `#[ignore]`d run's hand-typed
//! budget — this is a calibration probe, not a census, and does not attempt
//! `0..999` coverage.

use std::collections::BTreeMap;
use std::collections::BTreeSet;

use hornvale_astronomy::SkyPins;
use hornvale_history::flesh::RoleHandle;
use hornvale_kernel::{EntityId, Fact, Seed, Value, World, WorldTime, test_lineage};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::person_promote::{Founder, FounderCast, MEMORY_DEPTH, select_founders};
use hornvale_worldgen::{
    SettlementPins, SkyChoice, build_world, forebear_of, founder_of, occupation_records,
};

/// The seed panel this probe measures over. See the module doc's "The seed
/// panel" section for why 25 and why this spread.
const PANEL: [u64; 25] = [
    42, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
];

/// Build a world through the same path `windows/worldgen/tests/suite/
/// person_promotion.rs` uses: `build_world` with default pins. There is no
/// `BuildDepth` in this path (that ladder belongs to `build_world_to`,
/// `windows/worldgen/tests/suite/descent_graph.rs`'s helper) — `build_world`
/// already runs the promotion pass this probe reads.
fn world(seed: u64) -> World {
    build_world(
        Seed(seed),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("panel seed builds")
}

/// One world's promoted-forebear yield statistics.
#[derive(Debug, Clone)]
struct WorldYield {
    seed: u64,
    /// All promoted founders — the kill criterion's denominator.
    promoted: usize,
    /// Promoted founders whose forebear is ALSO promoted — the numerator
    /// both readings share.
    forebear_promoted: usize,
    /// Forebear exists (an entity `founder_of` names) but was not promoted.
    unpromoted_forebear: usize,
    /// `forebear_of` returned `None` — no entity forebear at all.
    roots: usize,
    /// Promoted-ancestor chain depth histogram: depth 0 is every founder
    /// counted in `unpromoted_forebear` or `roots` (its own immediate
    /// forebear is not promoted); depth N>0 is a founder reachable through
    /// exactly N consecutive promoted-forebear hops before the chain breaks.
    depths: BTreeMap<usize, usize>,
}

impl WorldYield {
    /// The kill-criterion reading: promoted-forebear founders over *all*
    /// promoted founders. Roots count against it.
    fn yield_all(&self) -> f64 {
        100.0 * self.forebear_promoted as f64 / self.promoted as f64
    }

    /// The excluding-roots reading: promoted-forebear founders over founders
    /// that have *any* entity forebear at all. **Not the kill criterion** —
    /// reported for comparison only (spec §5 / brief step 2).
    fn yield_excluding_roots(&self) -> f64 {
        let denom = self.promoted - self.roots;
        if denom == 0 {
            f64::NAN
        } else {
            100.0 * self.forebear_promoted as f64 / denom as f64
        }
    }
}

/// The core measurement: given a built `world` and its promoted [`FounderCast`],
/// walk every remembered founder's forebear chain and classify it.
///
/// `records` is passed in (rather than re-derived) so a caller building a
/// synthetic ledger by hand — the positive control below — can construct one
/// small `Vec<OccupationRecord>` set and reuse this exact function, the same
/// measurement the panel uses, rather than a parallel reimplementation that
/// could silently drift from it.
fn measure(world: &World, cast: &FounderCast) -> (usize, usize, usize, BTreeMap<usize, usize>) {
    // Recompute `founder_of` per promoted founder's OWN occupation — see the
    // module doc's handle-space trap. This is the identity space
    // `forebear_of` returns into, never `Founder::handle`.
    let promoted: Vec<(EntityId, RoleHandle)> = cast
        .remembered
        .iter()
        .map(|f: &Founder| (f.community, founder_of(world, f.community)))
        .collect();
    let promoted_handles: BTreeSet<u64> = promoted.iter().map(|(_, h)| h.0).collect();
    let community_of_handle: BTreeMap<u64, EntityId> =
        promoted.iter().map(|(c, h)| (h.0, *c)).collect();

    let mut forebear_promoted = 0usize;
    let mut unpromoted_forebear = 0usize;
    let mut roots = 0usize;
    let mut depths: BTreeMap<usize, usize> = BTreeMap::new();

    for f in &cast.remembered {
        match forebear_of(world, f.community) {
            None => {
                roots += 1;
                *depths.entry(0).or_default() += 1;
            }
            Some((forebear_handle, _kinship)) => {
                if promoted_handles.contains(&forebear_handle.0) {
                    forebear_promoted += 1;
                    let depth =
                        chain_depth(world, f.community, &promoted_handles, &community_of_handle);
                    *depths.entry(depth).or_default() += 1;
                } else {
                    unpromoted_forebear += 1;
                    *depths.entry(0).or_default() += 1;
                }
            }
        }
    }

    (forebear_promoted, unpromoted_forebear, roots, depths)
}

/// How many consecutive promoted-forebear hops `start`'s chain carries.
/// Bounded by `community_of_handle`'s size (the number of promoted
/// founders) rather than looping unboundedly — the descent tree is acyclic
/// by construction (`clan_root_of`'s doc makes the same argument), but a
/// bounded walk degrades instead of hanging if that is ever wrong.
fn chain_depth(
    world: &World,
    start: EntityId,
    promoted_handles: &BTreeSet<u64>,
    community_of_handle: &BTreeMap<u64, EntityId>,
) -> usize {
    let bound = community_of_handle.len() + 1;
    let mut depth = 0usize;
    let mut current = start;
    for _ in 0..bound {
        match forebear_of(world, current) {
            Some((h, _)) if promoted_handles.contains(&h.0) => {
                depth += 1;
                current = *community_of_handle
                    .get(&h.0)
                    .expect("handle came from promoted_handles, which is built from this same map");
            }
            _ => break,
        }
    }
    depth
}

fn measure_seed(seed: u64) -> WorldYield {
    let w = world(seed);
    let records = occupation_records(&w);
    let cast = select_founders(&records);
    let (forebear_promoted, unpromoted_forebear, roots, depths) = measure(&w, &cast);
    WorldYield {
        seed,
        promoted: cast.remembered.len(),
        forebear_promoted,
        unpromoted_forebear,
        roots,
        depths,
    }
}

/// Median of a non-empty slice of `f64`, sorted with `total_cmp` (project
/// convention for deterministic float ordering — see kernel `CLAUDE.md`).
/// The even-count case averages the two middle values.
fn median(values: &[f64]) -> f64 {
    let mut v = values.to_vec();
    v.sort_by(f64::total_cmp);
    let n = v.len();
    if n % 2 == 1 {
        v[n / 2]
    } else {
        (v[n / 2 - 1] + v[n / 2]) / 2.0
    }
}

/// claim: sanctioned-sweep(probe: measurement only, run explicitly — kill
/// criterion for spec §4.3, prints the promoted-forebear yield table over
/// [`PANEL`])
#[test]
#[ignore = "calibration: run by hand, prints the promoted-forebear yield panel"]
fn promoted_forebear_yield_panel() {
    let mut rows: Vec<WorldYield> = PANEL.iter().map(|&seed| measure_seed(seed)).collect();
    rows.sort_by_key(|r| r.seed);

    println!(
        "  seed                 promoted  fb_promoted  unpromoted_fb  roots  yield_all%  yield_excl_roots%"
    );
    for r in &rows {
        println!(
            "  {:<20} {:>8} {:>12} {:>14} {:>6} {:>10.1} {:>18.1}",
            r.seed,
            r.promoted,
            r.forebear_promoted,
            r.unpromoted_forebear,
            r.roots,
            r.yield_all(),
            r.yield_excluding_roots(),
        );
    }

    // Depth histogram, pooled across the panel (union of keys, ascending).
    let mut pooled_depths: BTreeMap<usize, usize> = BTreeMap::new();
    for r in &rows {
        for (depth, count) in &r.depths {
            *pooled_depths.entry(*depth).or_default() += count;
        }
    }
    println!("  pooled promoted-ancestor chain depth: {pooled_depths:?}");

    let yields_all: Vec<f64> = rows.iter().map(WorldYield::yield_all).collect();
    let yields_excl: Vec<f64> = rows.iter().map(WorldYield::yield_excluding_roots).collect();
    let median_all = median(&yields_all);
    let median_excl = median(&yields_excl);

    println!("  median yield (all promoted founders, the kill criterion): {median_all:.1}%");
    println!("  median yield (excluding roots, NOT the kill criterion): {median_excl:.1}%");
    println!(
        "  spec §5 kill criterion: median yield (all promoted founders) >= 10% required for §4.3 to ship"
    );

    // Sanity check on the panel itself, not on the result — a probe that
    // silently ran fewer seeds than declared would be a different lie than
    // the one this test exists to catch.
    assert_eq!(
        rows.len(),
        PANEL.len(),
        "every panel seed must report a row"
    );
}

// ---------------------------------------------------------------------------
// Positive control (spec §5 / brief step 3): a probe that can only ever
// report a high number is not evidence. This constructs a case where the
// yield MUST be near zero and confirms the same `measure` function this
// probe's panel uses reports it.
// ---------------------------------------------------------------------------

/// Commit one fact to `world`'s ledger, the same minimal helper
/// `descent_graph.rs::forebear_of_is_none_when_the_generation_length_cannot_be_derived`
/// uses to build a ledger by hand without a full world build.
fn commit(world: &mut World, subject: EntityId, predicate: &str, object: Value) {
    world
        .ledger
        .commit(
            Fact {
                subject,
                predicate: predicate.to_string(),
                object,
                place: None,
                day: Some(WorldTime::GENESIS),
                provenance: "promoted_forebear_yield positive control".to_string(),
            },
            &world.registry,
        )
        .expect("fixture facts commit cleanly");
}

/// Commit one minimal `goblin` occupation (enough for `occupation_records`
/// to reconstruct it and for `generation_length_of` to resolve — "goblin" is
/// a real roster species, per `descent_graph.rs`'s own generation-length
/// test). `mother` links `occ-founded-from`, when given.
fn commit_occupation(
    world: &mut World,
    site: u32,
    founded_day: f64,
    peak_population: u32,
    mother: Option<EntityId>,
) -> EntityId {
    let id = world
        .ledger
        .mint_entity(test_lineage(world.ledger.entity_count() as u16));
    commit(
        world,
        id,
        hornvale_history::IS_OCCUPATION,
        Value::Flag(true),
    );
    commit(
        world,
        id,
        hornvale_history::OCC_PEOPLE,
        Value::Text("goblin".to_string()),
    );
    commit(
        world,
        id,
        hornvale_history::OCC_SITE,
        Value::Number(f64::from(site)),
    );
    commit(
        world,
        id,
        hornvale_history::OCC_FOUNDED,
        Value::Number(founded_day),
    );
    commit(
        world,
        id,
        hornvale_history::OCC_PEAK,
        Value::Number(f64::from(peak_population)),
    );
    commit(
        world,
        id,
        hornvale_history::OCC_TECH,
        Value::Text("neolithic".to_string()),
    );
    commit(
        world,
        id,
        hornvale_history::OCC_FUNCTION,
        Value::Text("agrarian".to_string()),
    );
    commit(
        world,
        id,
        hornvale_history::OCC_NOTABILITY,
        Value::Text("common".to_string()),
    );
    if let Some(m) = mother {
        commit(
            world,
            id,
            hornvale_history::OCC_FOUNDED_FROM,
            Value::Entity(m),
        );
    }
    id
}

/// Confirms the instrument can report a near-zero yield: this is not a
/// negative result from a probe nobody has seen move.
///
/// Construction: one people ("goblin"), 25 occupations. Five "parent"
/// occupations carry the *lowest* peak population (1..=5) and no founder —
/// they are genesis roots themselves, irrelevant to their own yield. Twenty
/// "child" occupations carry a far higher peak population (500..=519, so
/// every child outranks every parent) and each names one of the five
/// parents as `occ-founded-from`. `MEMORY_DEPTH` (20) then promotes exactly
/// the 20 children — ranked strictly above the 5 parents on peak population,
/// the promotion order's first key — and drops all 5 parents. Every promoted
/// founder's forebear therefore exists (`forebear_of` returns `Some`) but is
/// never promoted: yield **must** be exactly 0/20 = 0%.
///
/// This is a stronger control than an all-roots panel (`forebear_of`
/// returning `None` everywhere) would be: it exercises the
/// `unpromoted_forebear` branch specifically — the same branch that does
/// almost all of the work in the real panel's 45.6% (76 of 111 non-yielding
/// founders at seed 42 are this case, not roots) — rather than only the
/// trivial "no mother at all" branch.
#[test]
fn positive_control_a_capped_people_yields_near_zero() {
    let mut w = World::new(Seed(1));
    hornvale_history::register_concepts(&mut w.registry).expect("registers cleanly");

    const PARENTS: usize = 5;
    const CHILDREN: usize = MEMORY_DEPTH;

    let mut parents = Vec::with_capacity(PARENTS);
    for p in 0..PARENTS {
        let id = commit_occupation(
            &mut w,
            /* site */ 1000 + p as u32,
            /* founded_day */ p as f64 * 1_000.0,
            /* peak_population */ (p + 1) as u32,
            /* mother */ None,
        );
        parents.push(id);
    }

    for c in 0..CHILDREN {
        commit_occupation(
            &mut w,
            /* site */ 2000 + c as u32,
            /* founded_day */ 100_000.0 + c as f64 * 1_000.0,
            /* peak_population */ 500 + c as u32,
            /* mother */ Some(parents[c % PARENTS]),
        );
    }

    let records = occupation_records(&w);
    assert_eq!(
        records.len(),
        PARENTS + CHILDREN,
        "every committed occupation must reconstruct"
    );

    let cast = select_founders(&records);
    assert_eq!(
        cast.remembered.len(),
        CHILDREN,
        "MEMORY_DEPTH caps this one people at {CHILDREN} promoted founders, \
         the 20 highest-peak-population children — the 5 low-peak parents \
         must be dropped, not promoted"
    );

    let (forebear_promoted, unpromoted_forebear, roots, depths) = measure(&w, &cast);
    println!(
        "positive control: promoted={} forebear_promoted={forebear_promoted} \
         unpromoted_forebear={unpromoted_forebear} roots={roots} depths={depths:?}",
        cast.remembered.len(),
    );

    assert_eq!(
        forebear_promoted, 0,
        "every promoted child's forebear is one of the 5 dropped parents — \
         none can be promoted, by construction"
    );
    assert_eq!(
        unpromoted_forebear, CHILDREN,
        "every promoted child has a real forebear entity that is not promoted"
    );
    assert_eq!(roots, 0, "no promoted founder in this fixture is a root");
    assert_eq!(
        depths.get(&0).copied().unwrap_or(0),
        CHILDREN,
        "every promoted founder sits at depth 0 (its own forebear is unpromoted)"
    );

    let yield_all = 100.0 * forebear_promoted as f64 / cast.remembered.len() as f64;
    assert_eq!(
        yield_all, 0.0,
        "the constructed panel's yield must be exactly 0%"
    );
}
