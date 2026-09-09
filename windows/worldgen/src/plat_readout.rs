//! The Made population (The Plat, spec §3.6 and §4): the panel section that
//! reads the committed ledger — which is why `hornvale circuit --seed`
//! builds to `Full` since this campaign — and measures, FROM the seed's
//! occupied underworld columns, what the reading and the origins did to
//! them. The Crosscut's and the Brattice's sections are untouched and
//! byte-identical; this is appended after them.
//!
//! **The cost is the point of the section, not an accident of it.** Every
//! cave-bearing, non-ocean vertex pays one [`column_origins`] call — a
//! linear scan of the committed occupations — plus one plan derivation, and
//! a Made column pays a second (all-`Found`) derivation for the report-only
//! before-figure of spec §4.2. That is what moves `hornvale circuit` from a
//! terrain-only verb to a ledger-reading one, and spec §9 asks the campaign
//! to measure it rather than to avoid it.

use std::collections::{BTreeMap, BTreeSet};

use hornvale_kernel::{Band, Seed, World};
use hornvale_terrain::GeneratedTerrain;

use crate::brattice::{Requirement, Way};
use crate::chamber::ChamberOrigin;
use crate::character::Character;
use crate::circuit::{DescentPlan, NodeId, plan_descent, plan_descent_with_origins};
use crate::delve_seating::{Tenancy, column_origins};
use crate::plat::{Reading, Role, read, role_nodes};

fn habitation_rungs() -> Vec<Band> {
    hornvale_terrain::rungs()
        .iter()
        .copied()
        .filter(|r| *r != Band::Surface)
        .collect()
}

/// The Plat's frozen verdict words against a floor (spec §4.1, §4.2).
fn verdict(value: f64, floor: f64) -> &'static str {
    if value >= floor {
        "PASSED"
    } else {
        "FALSIFIED"
    }
}

/// The same frozen words against an open band (spec §4.3: a majority, and
/// under two thirds).
fn band_verdict(value: f64, lo: f64, hi: f64) -> &'static str {
    if value > lo && value < hi {
        "PASSED"
    } else {
        "FALSIFIED"
    }
}

/// The Heart's depth decile on `level`: `(depth(Heart) − depth(Entry)) · 10 /
/// (depth(Sanctum) − depth(Entry))`, floored; `None` where the level has no
/// Heart, no Entry or no Sanctum.
///
/// A level whose Entry and Sanctum sit at the same depth has no gradient to
/// place the Heart along, so it reads decile 0 rather than dividing by zero.
/// type-audit: bare-ok(index: level), bare-ok(count: return)
fn heart_decile(plan: &DescentPlan, reading: &Reading, level: usize) -> Option<u16> {
    let entry = *role_nodes(plan, reading, level, Role::Entry).first()?;
    let sanctum = *role_nodes(plan, reading, level, Role::Sanctum).first()?;
    let heart = *role_nodes(plan, reading, level, Role::Heart).first()?;
    let d = |n: NodeId| u32::from(plan.nodes[n].depth);
    let range = d(sanctum).saturating_sub(d(entry));
    if range == 0 {
        return Some(0);
    }
    Some((d(heart).saturating_sub(d(entry)) * 10 / range) as u16)
}

/// Sum of the region areas of a realm's two paths' nodes, deduplicated —
/// the quantity spec §4.3 compares against the parent's.
/// type-audit: bare-ok(index: realm), bare-ok(count: return)
fn realm_area(plan: &DescentPlan, realm: usize) -> i64 {
    let r = &plan.realms[realm];
    let mut nodes: Vec<NodeId> = r.path_a.iter().chain(r.path_b.iter()).copied().collect();
    nodes.sort_unstable();
    nodes.dedup();
    nodes
        .iter()
        .map(|&n| {
            let rr = plan.region_of(n);
            i64::from(rr.w) * i64::from(rr.h)
        })
        .sum()
}

/// `Needs(Key)` gates whose edge lies on `level` (either endpoint's level, so
/// a cross-floor key row's gate counts for the level it gates).
/// type-audit: bare-ok(index: level), bare-ok(count: return)
fn doors_on_level(plan: &DescentPlan, level: usize) -> usize {
    plan.edges
        .iter()
        .filter(|e| {
            let on_level =
                plan.nodes[e.a].level as usize == level || plan.nodes[e.b].level as usize == level;
            on_level
                && e.gate.as_ref().is_some_and(|g| {
                    matches!(g.toward_a, Way::Needs(Requirement::Key(_)))
                        || matches!(g.toward_b, Way::Needs(Requirement::Key(_)))
                })
        })
        .count()
}

/// Connected components of `level`'s within-level passage graph (spec §4.4,
/// report only). Stairs are not an adjacency axis here: the question is how
/// many pieces one floor is in.
/// type-audit: bare-ok(index: level), bare-ok(count: return)
fn within_level_components(plan: &DescentPlan, level: usize) -> usize {
    let mut unseen: BTreeSet<NodeId> = plan.nodes_on(level).into_iter().collect();
    let mut count = 0usize;
    while let Some(&start) = unseen.iter().next() {
        count += 1;
        let mut stack = vec![start];
        unseen.remove(&start);
        while let Some(cur) = stack.pop() {
            for m in plan.neighbours(cur) {
                if plan.nodes[m].level as usize == level && unseen.remove(&m) {
                    stack.push(m);
                }
            }
        }
    }
    count
}

/// The section, one block per seed, appended by `hornvale circuit` after the
/// eight existing readouts.
///
/// **Byte-identical for a given `(world, terrain)`**, the same guarantee
/// [`crate::circuit_readout::render_circuit_panel`] makes: no wall clock, no
/// float in the compute path, and every map it renders is a `BTreeMap`, so
/// its `Debug` render is ordered.
/// type-audit: bare-ok(prose: return)
pub fn render_made_population(world: &World, terrain: &GeneratedTerrain) -> String {
    let rungs = habitation_rungs();
    let seed: Seed = world.seed;
    let mut columns = 0usize;
    let mut seated: BTreeMap<String, usize> = BTreeMap::new();
    let mut inhabited = 0usize;
    let mut abandoned = 0usize;
    let mut made_levels = 0usize;
    let mut decile_le5 = 0usize;
    let mut with_door = 0usize;
    let mut with_door_all_found = 0usize;
    let mut landings = 0usize;
    let mut sanctum_with_thing = 0usize;
    let mut nested = 0usize;
    let mut nested_smaller = 0usize;
    let mut components: BTreeMap<usize, usize> = BTreeMap::new();

    for vertex in terrain.geosphere().vertices() {
        let Some(cave) = terrain.cave_at(vertex) else {
            continue;
        };
        if terrain.is_ocean(vertex) {
            continue;
        }
        let origins = column_origins(world, terrain, vertex, &rungs);
        let origin_list: Vec<ChamberOrigin> = origins.iter().map(|o| o.0).collect();
        let plan = plan_descent_with_origins(
            seed,
            vertex,
            &rungs,
            cave.kind,
            Character::WildCave,
            &origin_list,
        );
        let reading = read(&plan);
        // Alexander 98, over EVERY plan the panel derives.
        for (i, r) in plan.realms.iter().enumerate() {
            if let Some(p) = r.parent {
                nested += 1;
                if realm_area(&plan, i) < realm_area(&plan, p) {
                    nested_smaller += 1;
                }
            }
        }
        for level in 0..rungs.len() {
            *components
                .entry(within_level_components(&plan, level))
                .or_default() += 1;
        }
        let Some(made_rung) = origin_list.iter().position(|o| *o == ChamberOrigin::Made) else {
            continue;
        };
        columns += 1;
        *seated.entry(format!("{:?}", rungs[made_rung])).or_default() += 1;
        match origins[made_rung].1 {
            Tenancy::Inhabited => inhabited += 1,
            Tenancy::Abandoned => abandoned += 1,
            Tenancy::Wild => {}
        }
        made_levels += 1;
        if heart_decile(&plan, &reading, made_rung).is_some_and(|d| d <= 5) {
            decile_le5 += 1;
        }
        if doors_on_level(&plan, made_rung) > 0 {
            with_door += 1;
        }
        // Spec §4.2's report-only before-figure: the same rung's plan as it
        // was derived before The Plat, all levels `Found`.
        let wild = plan_descent(seed, vertex, &rungs, cave.kind, Character::WildCave);
        if doors_on_level(&wild, made_rung) > 0 {
            with_door_all_found += 1;
        }
        if plan.nodes_on(made_rung).iter().any(|&n| reading.landing[n]) {
            landings += 1;
        }
        // A thing at genesis is a latent key at the Sanctum node (spec §4.4).
        if let Some(&s) = role_nodes(&plan, &reading, made_rung, Role::Sanctum).first()
            && plan.nodes[s].key.is_some()
        {
            sanctum_with_thing += 1;
        }
    }

    let ratio = |n: usize, d: usize| if d == 0 { 0.0 } else { n as f64 / d as f64 };
    let decile_share = ratio(decile_le5, made_levels);
    let door_share = ratio(with_door, made_levels);
    let shrink = ratio(nested_smaller, nested);
    let mut out = String::from("\n");
    out.push_str(&format!(
        "the Made population: {columns} occupied columns, seated {seated:?}; tenancy: inhabited {inhabited} abandoned {abandoned}\n"
    ));
    out.push_str(&format!(
        "heart decile <= 5: {decile_le5} of {made_levels} = {decile_share:.4} (frozen floor 0.6667; FROM Made levels TO the Heart's depth decile) -> {}\n",
        verdict(decile_share, 2.0 / 3.0)
    ));
    out.push_str(&format!(
        "doors on Made rungs: {with_door} of {made_levels} = {door_share:.4} (frozen floor 0.50; FROM Made rungs TO rungs with a Needs(Key) gate; all-Found the same rungs carry {with_door_all_found}) -> {}\n",
        verdict(door_share, 0.5)
    ));
    out.push_str(&format!(
        "nested realms smaller than their parent: {nested_smaller} of {nested} = {shrink:.4} (frozen band (0.50, 0.6667); FROM nested realms on every plan TO area < parent's) -> {}\n",
        band_verdict(shrink, 0.5, 2.0 / 3.0)
    ));
    out.push_str(&format!(
        "landings on Made levels {landings} of {made_levels}; sanctums holding a thing at genesis {sanctum_with_thing} of {made_levels} (report only)\n"
    ));
    out.push_str(&format!(
        "within-level components per level, every plan: {components:?} (report only)\n"
    ));
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The frozen verdict words and the FROM/TO phrasing of spec §4.1–4.3,
    /// pinned so a later edit of the prose is a review decision.
    ///
    /// Decision 0092: the sculpt is the sanctioned cost here. The section is
    /// a function of `(world, terrain)` and the committed fixture supplies
    /// only the world, so a named construction site is the only way to hold
    /// the real seed-42 historical occupied-column layer this test asserts.
    #[test]
    #[allow(clippy::disallowed_methods)]
    fn the_section_speaks_in_the_specs_frozen_words() {
        let world = crate::fixture::seed_42_world();
        let terrain = crate::terrain_of(&world).expect("seed 42 sculpts");
        let out = render_made_population(&world, &terrain);
        for needle in [
            "the Made population: 5 occupied columns",
            "heart decile <= 5:",
            "(frozen floor 0.6667; FROM Made levels TO the Heart's depth decile)",
            "doors on Made rungs:",
            "(frozen floor 0.50; FROM Made rungs TO rungs with a Needs(Key) gate; all-Found the same rungs carry 0)",
            "nested realms smaller than their parent:",
            "(frozen band (0.50, 0.6667); FROM nested realms on every plan TO area < parent's)",
            "landings on Made levels",
            "sanctums holding a thing at genesis",
            "tenancy: inhabited",
            "within-level components",
        ] {
            assert!(out.contains(needle), "missing {needle:?} in:\n{out}");
        }
        assert!(out.contains("PASSED") || out.contains("FALSIFIED"));
    }
}
