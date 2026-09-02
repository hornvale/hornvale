//! Wild herds within a window (The Roll, spec §3.2). `wild_concentrations_from`
//! answers "the world's top-k beasts"; this answers "which beasts stand within
//! call of HERE, and how many" — headcount-rendered per attractor, so a herd is
//! a herd and a lone dragon is one body.

use hornvale_demography::stack_condense::HeadcountRender;
use hornvale_kernel::{ComponentStore, KindId};
use hornvale_species::BiosphereTraits;

use crate::DemographyReport;
use crate::components::WorldComponents;

/// A mobile-beast presence at one attractor.
/// type-audit: bare-ok(identifier-text: species), pending(wave-3: position), bare-ok(index: vertex), bare-ok(count: headcount)
#[derive(Clone, Debug, PartialEq)]
pub struct WildHerd {
    /// The species label.
    pub species: String,
    /// The attractor's unit-sphere position.
    pub position: [f64; 3],
    /// The attractor vertex — the herd's identity key, with `species`.
    pub vertex: u32,
    /// How many bodies stand here: `Count(n)` is `n`, `Lone` is `1`.
    pub headcount: u32,
}

/// A mobile beast: a WILD, non-sessile, non-settling kind — `social_form`
/// is `Solitary` or `Gregarious` (not `Settled`, the peoplehood axis; not
/// `Sessile`, a rooted phototroph that is placed but never agentified).
///
/// …and not a SEA creature. The Vacancy opened the ocean to the habitat
/// model, but the walk layer this feeds is a terrestrial surface game:
/// there is no underwater locale, and every agent it mints carries a
/// freshwater thirst drive it satisfies by pathing to drinkable water. A
/// shark minted here is therefore permanently, unsatisfiably thirsty —
/// measured, not theorised: agentifying the reef shark drove the health
/// battery's null control to 0.94 thirst-caused distress and fired its
/// bug alarm.
///
/// The test is *predominantly* marine (majority uptake), not marine at
/// all, so the amphibious kind still walks: a crocodile hauls out, and
/// its 0.4 sea / 0.6 land vector is exactly the case the surface game
/// can represent. A real habitat-medium axis (MAP-11) would state this
/// properly; until then, what a creature eats is the honest proxy for
/// where it lives, which is the same reasoning the supply mask uses.
///
/// The one filter [`crate::wild_concentrations_from`] and [`wild_herds_near`]
/// share — extracted here (The Roll) so the two cannot drift.
/// type-audit: bare-ok(identifier-text: label), bare-ok(flag: return)
pub fn is_mobile_beast(biosphere: &ComponentStore<KindId, BiosphereTraits>, label: &str) -> bool {
    biosphere.get_by_label(label).is_some_and(|b| {
        let mobile = matches!(
            b.social_form,
            hornvale_species::SocialForm::Solitary | hornvale_species::SocialForm::Gregarious
        );
        let predominantly_marine = b.niche.weight(hornvale_kernel::MARINE_FORAGE) > 0.5;
        mobile && !predominantly_marine
    })
}

/// Every mobile-beast presence at every attractor `within` admits, ordered
/// by (vertex, species). `Colony` is not yet fired anywhere and contributes
/// no body.
pub fn wild_herds_near(
    wc: &WorldComponents,
    report: &DemographyReport,
    within: impl Fn([f64; 3]) -> bool,
) -> Vec<WildHerd> {
    let labels: Vec<String> = wc
        .biosphere
        .iter()
        .map(|(kind, _)| kind.0.to_string())
        .collect();
    let biosphere = hornvale_species::biosphere_registry();
    let mut out = Vec::new();
    for s in &report.stack_settlements {
        if !within(s.position) {
            continue;
        }
        for (sid, render) in &s.rendered {
            let Some(label) = labels.get(*sid as usize) else {
                continue;
            };
            if !is_mobile_beast(&biosphere, label) {
                continue;
            }
            let headcount = match render {
                HeadcountRender::Count(n) => *n,
                HeadcountRender::Lone => 1,
                HeadcountRender::Colony(_) => continue,
            };
            if headcount == 0 {
                continue;
            }
            out.push(WildHerd {
                species: label.clone(),
                position: s.position,
                vertex: s.vertex.0,
                headcount,
            });
        }
    }
    out.sort_by(|a, b| {
        a.vertex
            .cmp(&b.vertex)
            .then_with(|| a.species.cmp(&b.species))
    });
    out
}

#[cfg(test)]
// Test fixture (decision 0092): calls the sculpt/fit derivation entry
// points directly to build its own world state, once per test — the
// sanctioned test-fixture posture the weir's spec carves out. Same posture
// as `lib.rs`'s own `mod tests`, whose `wc_terrain_report` this module's
// copy is verbatim.
#[allow(clippy::disallowed_methods)]
mod tests {
    use super::*;
    use crate::components::WorldComponents;
    use crate::settlement_pins::SettlementPins;
    use crate::{SkyChoice, build_world, climate_from, demography_report_from, terrain_of};

    /// The `(wc, terrain, report)` prelude — copied verbatim from
    /// `lib.rs`'s test-only `wc_terrain_report` (`lib.rs:10834-10847`), so
    /// the two test suites build the identical fixture.
    fn wc_terrain_report(
        world: &hornvale_kernel::World,
    ) -> (
        WorldComponents,
        hornvale_terrain::GeneratedTerrain,
        hornvale_demography::DemographyReport,
    ) {
        let wc = WorldComponents::assemble().unwrap();
        let terrain = terrain_of(world).unwrap();
        let climate = climate_from(world, &terrain).unwrap();
        let report = demography_report_from(world, &wc, &terrain, &climate).unwrap();
        (wc, terrain, report)
    }

    /// A seed-42 generated-sky world with the default roster — copied
    /// verbatim from `lib.rs`'s `vigil_world`.
    fn roll_world() -> hornvale_kernel::World {
        build_world(
            hornvale_kernel::Seed(42),
            &hornvale_astronomy::SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("seed 42 builds")
    }

    /// The shared filter agrees with an INDEPENDENT re-statement of the old
    /// inline predicate (mobile, not predominantly marine) — written here
    /// rather than by calling `is_mobile_beast`, so a regression in the
    /// shared function is visible even though both
    /// `wild_concentrations_from` and `wild_herds_near` now call the same
    /// code and can never disagree WITH EACH OTHER. Two halves: (1) every
    /// species `wild_concentrations_from(wc, report, 100)` names satisfies
    /// the independent predicate, and every herd `wild_herds_near(..,
    /// |_| true)` names does too (catches OVER-inclusion); (2) for EVERY
    /// kind in `biosphere_registry()` — not only the ones seed 42 happens
    /// to place — `is_mobile_beast(&biosphere, label) == oracle(label)`
    /// (catches UNDER-inclusion: a kind the shared filter wrongly excludes
    /// never appears in `wild`/`herds` at all, so half (1) alone cannot see
    /// it).
    ///
    /// MUTATION THIS MUST FAIL AGAINST (both directions, independently):
    /// (a) drop `SocialForm::Gregarious` from `is_mobile_beast`'s
    /// `matches!` (under-inclusion — excludes dire-wolf, giant-elk, every
    /// other gregarious beast); (b) flip `!predominantly_marine` to
    /// `predominantly_marine` in `is_mobile_beast` (over-inclusion — the
    /// shared function then disagrees with this test's independent
    /// oracle).
    #[test]
    fn the_filter_is_shared() {
        let world = roll_world();
        let (wc, _terrain, report) = wc_terrain_report(&world);
        let biosphere = hornvale_species::biosphere_registry();
        // The independent oracle: NOT a call to `is_mobile_beast` — a
        // literal re-statement of what it is supposed to compute, so this
        // test can catch drift in the shared implementation rather than
        // merely proving the two callers agree with each other.
        let oracle = |label: &str| -> bool {
            biosphere.get_by_label(label).is_some_and(|b| {
                let mobile = matches!(
                    b.social_form,
                    hornvale_species::SocialForm::Solitary
                        | hornvale_species::SocialForm::Gregarious
                );
                let predominantly_marine = b.niche.weight(hornvale_kernel::MARINE_FORAGE) > 0.5;
                mobile && !predominantly_marine
            })
        };

        let wild = crate::wild_concentrations_from(&wc, &report, 100);
        assert!(!wild.is_empty(), "the wild is populated: {wild:?}");
        for (species, _pos) in &wild {
            assert!(
                oracle(species),
                "{species} named by wild_concentrations_from must satisfy the independent oracle"
            );
        }

        let herds = wild_herds_near(&wc, &report, |_| true);
        assert!(!herds.is_empty(), "the herds are populated: {herds:?}");
        for herd in &herds {
            assert!(
                oracle(&herd.species),
                "{} named by wild_herds_near must satisfy the independent oracle",
                herd.species
            );
        }

        // The symmetric half: every REGISTERED kind (not only the ones
        // seed 42 happens to place) must agree between the shared filter
        // and the independent oracle. This is the only check that can see
        // UNDER-inclusion — a kind the shared filter wrongly excludes never
        // shows up in `wild` or `herds` at all, so the two loops above
        // cannot detect its absence. Needs no world or report.
        let mut checked_kinds = 0;
        for (kind, _traits) in biosphere.iter() {
            let label = kind.0;
            assert_eq!(
                is_mobile_beast(&biosphere, label),
                oracle(label),
                "{label}: is_mobile_beast and the independent oracle disagree"
            );
            checked_kinds += 1;
        }
        assert!(checked_kinds > 0, "the biosphere registry is non-empty");
    }

    /// A herd's headcount is its attractor's rendered count, exactly:
    /// `Count(n)` -> n, `Lone` -> 1, and never more than `rendered` says
    /// (coarse constrains fine).
    ///
    /// MUTATION THIS MUST FAIL AGAINST: `Count(n) => n + 1`.
    ///
    /// claim: structural(seed: 42) — false-positive seed-loop flag; `s` binds
    /// a stack settlement, single fixed seed
    #[test]
    fn headcount_is_the_rendered_count() {
        let world = roll_world();
        let (wc, _terrain, report) = wc_terrain_report(&world);
        let labels: Vec<String> = wc
            .biosphere
            .iter()
            .map(|(kind, _)| kind.0.to_string())
            .collect();
        let biosphere = hornvale_species::biosphere_registry();

        let herds = wild_herds_near(&wc, &report, |_| true);
        assert!(!herds.is_empty());

        let mut checked = 0;
        for s in &report.stack_settlements {
            for (sid, render) in &s.rendered {
                let Some(label) = labels.get(*sid as usize) else {
                    continue;
                };
                if !is_mobile_beast(&biosphere, label) {
                    continue;
                }
                let expected = match render {
                    HeadcountRender::Count(n) => *n,
                    HeadcountRender::Lone => 1,
                    HeadcountRender::Colony(_) => continue,
                };
                if expected == 0 {
                    continue;
                }
                let found = herds
                    .iter()
                    .find(|h| h.vertex == s.vertex.0 && h.species == *label)
                    .unwrap_or_else(|| panic!("herd for {label} at vertex {} missing", s.vertex.0));
                assert_eq!(
                    found.headcount, expected,
                    "{label} at vertex {} headcount mismatch",
                    s.vertex.0
                );
                checked += 1;
            }
        }
        assert!(checked > 0, "at least one attractor was checked");
    }

    /// `within` is honoured: a predicate that admits nothing yields no herds;
    /// one that admits everything yields one entry per (attractor, mobile
    /// species) with a non-zero headcount.
    ///
    /// MUTATION THIS MUST FAIL AGAINST: ignore `within` in the loop.
    #[test]
    fn the_window_selects() {
        let world = roll_world();
        let (wc, _terrain, report) = wc_terrain_report(&world);

        let none = wild_herds_near(&wc, &report, |_| false);
        assert!(none.is_empty(), "an admit-nothing window yields no herds");

        let all = wild_herds_near(&wc, &report, |_| true);
        assert!(!all.is_empty(), "an admit-everything window yields herds");
        for herd in &all {
            assert!(herd.headcount > 0, "{herd:?} has a non-zero headcount");
        }

        // Sorted by (vertex ascending, species ascending).
        for pair in all.windows(2) {
            let (a, b) = (&pair[0], &pair[1]);
            assert!(
                (a.vertex, &a.species) <= (b.vertex, &b.species),
                "herds must be sorted by (vertex, species): {a:?} then {b:?}"
            );
        }
    }
}
