//! P-4 (spec §6.4): the affinity ladder is a hand-authored **metric** on the
//! biome space, and this asks whether the axes reproduce it.
//!
//! `AFFINITY_NEAR` (0.70) is documented as *"one band out in the classifier's
//! lookup table"* and `AFFINITY_MARGINAL` (0.45) as *"two bands out, **or the
//! right climate in the wrong form**"*. That second clause is the campaign's
//! own thesis, written by an author who was not decomposing anything: two
//! distinct moves — two steps along climate, or one step along *form* — land on
//! the same rung. It is an independent, already-committed statement that this
//! space has at least two dimensions with an exchange rate between them.
//!
//! So if the axes are right, a species' better-liked biomes should sit **nearer
//! its stronghold in axis space** than its worse-liked ones. That is what the
//! concordance below measures.
//!
//! **This is a report, not a gate.** A weak result means either the axes or the
//! ladder is wrong, and the campaign says which it concludes and why — it does
//! not retune axis values to improve the number, which would be fitting to the
//! check. Only the vacuity guard asserts.
//!
//! # Why this test lives at the composition root
//!
//! It reads `domains/climate` (the assignment) and `domains/species` (the
//! affinity registry). A domain may not depend on a sibling, so this cannot
//! live in either — `windows/worldgen` is the only place both are visible. That
//! is the same constraint that made `BiomeAffinity` key by *string* rather than
//! by `Biome`, which is spec §3.4's starting observation.

use hornvale_climate::axes::assignment;
use hornvale_kernel::environment_v1_basis;
use hornvale_species::biome_affinity_registry;

/// The two names where the affinity registry's vocabulary and the assignment's
/// disagree, reconciled explicitly rather than silently dropped.
///
/// This is a finding in its own right: the registry keys by `Biome::name()`
/// while the assignment keys by `Formation`, and the two sets differ on 2 of
/// the 15 names the registry actually uses. `coral-reef` is `Formation::Reef`
/// under the legacy enum's spelling; `epipelagic` is not a community at all but
/// a **stratum wearing a formation's coat** (`facets.rs` says so in as many
/// words), and it projects to open water at the surface rung.
///
/// **THE TIDEMARK (Task 3) adds five more, and every one is the same shape as
/// `epipelagic`: a STRATUM the registry names where the assignment names a
/// COMMUNITY.** Six marine peoples now carry affinity rows, and a marine
/// affinity has to key by the biome name placement resolves against
/// (`Biome::name()`), which below the shelf is a depth band. Each maps to the
/// open-water community the assignment gives that band:
///
/// - `mesopelagic` -> `twilight-water` — the band where light fails but has
///   not gone;
/// - `bathypelagic` -> `lightless-water` — the band below that, where it has;
/// - `abyssal` -> `abyssal-plain` — the seabed community of the abyssal band;
/// - `hadal-trench` -> `trench-floor` — the same for the hadal one;
/// - `hydrothermal-vent` -> `vent` — the one NON-stratum addition, and a
///   straight spelling difference: climate derives the biome from
///   `SeafloorFeature::Ridge` and the assignment names the same thing `vent`.
///
/// Mapping rather than dropping matters here for the reason the header
/// states: a dropped name silently truncates the population the concordance
/// above is computed over, and five of the fifteen would have been a third
/// of it.
const RECONCILE: &[(&str, &str)] = &[
    ("coral-reef", "reef"),
    ("epipelagic", "open-water"),
    ("mesopelagic", "twilight-water"),
    ("bathypelagic", "lightless-water"),
    ("abyssal", "abyssal-plain"),
    ("hadal-trench", "trench-floor"),
    ("hydrothermal-vent", "vent"),
];

fn assignment_name(registry_name: &str) -> &str {
    RECONCILE
        .iter()
        .find(|(from, _)| *from == registry_name)
        .map(|(_, to)| *to)
        .unwrap_or(registry_name)
}

/// Euclidean distance over the axes **both** vectors assign. Axes either side
/// declines are skipped rather than treated as zero: a genus that declines an
/// axis is silent about it, not centred on it.
fn axis_distance(a: &str, b: &str) -> Option<f64> {
    let find = |n: &str| assignment().iter().find(|x| x.name == n);
    let (x, y) = (find(a)?, find(b)?);
    let mut sum = 0.0;
    let mut shared = 0usize;
    for axis in environment_v1_basis() {
        if let (Some(p), Some(q)) = (x.vector.get(*axis), y.vector.get(*axis)) {
            sum += (p - q) * (p - q);
            shared += 1;
        }
    }
    // `sum.sqrt()`, not `kernel::math::sqrt` — `math.rs` carries the eleven
    // transcendentals routed through libm (decision 0041), and `sqrt` is
    // deliberately not one of them: IEEE-754 guarantees it exactly, so it stays
    // the intrinsic (`kernel/CLAUDE.md`).
    (shared > 0).then(|| sum.sqrt())
}

/// One species' ladder: its stronghold, and the other biomes it rates.
struct Ladder {
    stronghold: String,
    rated: Vec<(String, f64)>,
}

fn ladders() -> Vec<Ladder> {
    let registry = biome_affinity_registry();
    let mut out = Vec::new();
    for (_kind, affinity) in registry.iter() {
        let mut rated: Vec<(String, f64)> = affinity
            .by_biome
            .iter()
            .map(|(name, factor)| (assignment_name(name).to_string(), *factor))
            .collect();
        rated.sort_by(|a, b| b.1.total_cmp(&a.1).then_with(|| a.0.cmp(&b.0)));
        if let Some((top, _)) = rated.first().cloned() {
            out.push(Ladder {
                stronghold: top,
                rated,
            });
        }
    }
    out
}

#[test]
fn axis_distance_orders_the_affinity_ladder() {
    let mut concordant = 0usize;
    let mut total = 0usize;
    let mut unresolved: Vec<String> = Vec::new();

    for ladder in ladders() {
        for i in 0..ladder.rated.len() {
            for j in (i + 1)..ladder.rated.len() {
                let (near, near_f) = &ladder.rated[i];
                let (far, far_f) = &ladder.rated[j];
                if near_f <= far_f {
                    continue; // equal rungs carry no ordering claim
                }
                let (dn, df) = (
                    axis_distance(&ladder.stronghold, near),
                    axis_distance(&ladder.stronghold, far),
                );
                match (dn, df) {
                    (Some(dn), Some(df)) => {
                        total += 1;
                        if dn < df {
                            concordant += 1;
                        }
                    }
                    _ => {
                        for n in [near, far] {
                            if axis_distance(&ladder.stronghold, n).is_none()
                                && !unresolved.contains(n)
                            {
                                unresolved.push(n.clone());
                            }
                        }
                    }
                }
            }
        }
    }

    assert!(
        total > 0,
        "VACUOUS: no rated biome pair resolved to two assigned vectors. The \
         affinity registry and the assignment use different name vocabularies; \
         reconcile them in RECONCILE before reading this result as evidence."
    );
    let ratio = concordant as f64 / total as f64;
    println!(
        "P-4: {concordant}/{total} ordered pairs concordant ({:.1}%) — a better-liked \
         biome sits nearer the stronghold in axis space",
        ratio * 100.0
    );
    if !unresolved.is_empty() {
        unresolved.sort();
        println!("P-4: unresolved names (no assigned vector): {unresolved:?}");
    }
}

/// The reconciliation must stay honest: every name the registry uses has to
/// resolve to an assigned vector, or the concordance above is computed over a
/// silently truncated population.
#[test]
fn every_rated_biome_resolves_to_an_assigned_name() {
    let assigned: Vec<&str> = assignment().iter().map(|e| e.name).collect();
    let mut unresolved: Vec<String> = Vec::new();
    for (_kind, affinity) in biome_affinity_registry().iter() {
        for (name, _) in &affinity.by_biome {
            let mapped = assignment_name(name);
            if !assigned.contains(&mapped) && !unresolved.contains(&mapped.to_string()) {
                unresolved.push(mapped.to_string());
            }
        }
    }
    unresolved.sort();
    assert!(
        unresolved.is_empty(),
        "the affinity registry rates biomes the assignment cannot place: \
         {unresolved:?} — add them to RECONCILE with a stated reason, never by \
         dropping the row"
    );
}
