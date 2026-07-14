//! Star figures (spec §4): the sky clusters into notable groups under
//! single-link clustering over the unified bright-star catalog (notable
//! neighbors + the background starfield). This is a structural description
//! only — no mythology, no proper names — and it is keyed to one **reference
//! observer**: different species perceive different skies (different eyes,
//! different sensitivity thresholds), so the committed figure catalog is
//! explicitly the reference observer's; per-species figure catalogs are a
//! deferred registry row.

use crate::sky_position::{EquatorialCoord, ecliptic_of};
use crate::starfield::starfield;
use crate::system::StarSystem;
use hornvale_kernel::Seed;
use hornvale_kernel::math;
use std::collections::BTreeMap;

/// Great-circle separation threshold (degrees) for single-link clustering:
/// two stars closer than this join the same figure. Census-frozen (the
/// `census-of-figures` study, 1000 seeds): median 6 figures per sky, 6.4%
/// zero-figure worlds, 66.5% with at least one figure on the ecliptic.
/// type-audit: pending(wave-1)
pub const FIGURE_SEPARATION_DEG: f64 = 7.0;

/// Magnitude-class floor (inclusive) admitted into figure clustering — the
/// **reference-observer convention** (spec §4). Census-frozen alongside
/// [`FIGURE_SEPARATION_DEG`] and [`FIGURE_MIN_MEMBERS`].
/// type-audit: bare-ok(count)
pub const FIGURE_MAGNITUDE_FLOOR: u8 = 4;

/// Minimum cluster size to be recognized as a figure. Census-frozen
/// alongside [`FIGURE_SEPARATION_DEG`] and [`FIGURE_MAGNITUDE_FLOOR`].
/// type-audit: bare-ok(count)
pub const FIGURE_MIN_MEMBERS: usize = 3;

/// A cluster of bright stars forming a notable sky figure: structural only,
/// never a proper name or mythological reference.
/// type-audit: bare-ok(count: member_count), pending(wave-1: span_deg), bare-ok(count: brightest_class), bare-ok(flag: on_ecliptic)
#[derive(Debug, Clone, PartialEq)]
pub struct Figure {
    /// How many stars belong to this figure.
    pub member_count: usize,
    /// The figure's centroid: the normalized Cartesian mean of its members,
    /// converted back to equatorial coordinates.
    pub centroid: EquatorialCoord,
    /// The maximum pairwise great-circle separation between any two members
    /// of this figure, in degrees.
    pub span_deg: f64,
    /// The brightest magnitude class among the figure's members (1 = the
    /// brightest tier).
    pub brightest_class: u8,
    /// Whether the figure stands on the ecliptic — the sun-and-wanderer
    /// road: the centroid's ecliptic latitude, in the **mean-obliquity**
    /// ecliptic frame (`forcing.obliquity_mean`, not `obliquity_at(0.0)` —
    /// the two differ by the wobble term even at genesis; this convention
    /// is a deliberate choice so the frame is stable rather than tracking
    /// the instantaneous nutation), falls within `span_deg / 2 + 8°` of the
    /// ecliptic plane.
    pub on_ecliptic: bool,
}

/// One unified bright-star entry feeding the clustering pass: either a
/// notable neighbor (magnitude class 1 by definition) or a background
/// starfield entry at or brighter than [`FIGURE_MAGNITUDE_FLOOR`].
#[derive(Debug, Clone, Copy)]
struct BrightStar {
    magnitude_class: u8,
    ra_deg: f64,
    dec_deg: f64,
}

/// Great-circle separation between two equatorial positions, in degrees
/// (kernel math only, clamped against float roundoff at the poles/antipodes).
fn great_circle_sep_deg(ra1_deg: f64, dec1_deg: f64, ra2_deg: f64, dec2_deg: f64) -> f64 {
    let d1 = dec1_deg.to_radians();
    let d2 = dec2_deg.to_radians();
    let dra = (ra1_deg - ra2_deg).to_radians();
    let cos_sep = (math::sin(d1) * math::sin(d2) + math::cos(d1) * math::cos(d2) * math::cos(dra))
        .clamp(-1.0, 1.0);
    math::acos(cos_sep).to_degrees()
}

/// Union-find with path halving (iterative — no recursion depth risk) and
/// deterministic union-by-lower-index (the sort order feeding `figures` is
/// itself deterministic, so ties never depend on iteration order).
fn find(parent: &mut [usize], mut x: usize) -> usize {
    while parent[x] != x {
        parent[x] = parent[parent[x]];
        x = parent[x];
    }
    x
}

fn union(parent: &mut [usize], a: usize, b: usize) {
    let ra = find(parent, a);
    let rb = find(parent, b);
    if ra != rb {
        let (lo, hi) = if ra < rb { (ra, rb) } else { (rb, ra) };
        parent[hi] = lo;
    }
}

/// The region word for a sky position, by declination band (spec §4):
/// north of +20°, south of −20°, or the band in between where the sun and
/// wanderers travel. Shared by [`describe`] and the fact layer so both
/// report the same rule.
pub(crate) fn region_word(dec_deg: f64) -> &'static str {
    if dec_deg >= 20.0 {
        "northern sky"
    } else if dec_deg <= -20.0 {
        "southern sky"
    } else {
        "the equator's road"
    }
}

fn build_figure(stars: &[BrightStar], members: &[usize], obliquity_mean_deg: f64) -> Figure {
    let (mut x, mut y, mut z) = (0.0, 0.0, 0.0);
    for &i in members {
        let ra = stars[i].ra_deg.to_radians();
        let dec = stars[i].dec_deg.to_radians();
        x += math::cos(dec) * math::cos(ra);
        y += math::cos(dec) * math::sin(ra);
        z += math::sin(dec);
    }
    let len = (x * x + y * y + z * z).sqrt();
    // Degenerate sum (members nearly surround the sphere): fall back to the
    // brightest member's seat (the first member in the sorted unified list —
    // deterministic). Percolation at the frozen constants makes this
    // practically unreachable (mean degree ~1.8 vs ~4.5 threshold), but NaN
    // must never propagate.
    let centroid = if len < 1e-9 {
        let first = &stars[members[0]];
        EquatorialCoord {
            ra_deg: first.ra_deg,
            dec_deg: first.dec_deg,
        }
    } else {
        let (cx, cy, cz) = (x / len, y / len, z / len);
        EquatorialCoord {
            ra_deg: math::atan2(cy, cx).to_degrees().rem_euclid(360.0),
            dec_deg: math::asin(cz.clamp(-1.0, 1.0)).to_degrees(),
        }
    };

    let mut span_deg: f64 = 0.0;
    for (a_idx, &a) in members.iter().enumerate() {
        for &b in &members[a_idx + 1..] {
            let sep = great_circle_sep_deg(
                stars[a].ra_deg,
                stars[a].dec_deg,
                stars[b].ra_deg,
                stars[b].dec_deg,
            );
            if sep > span_deg {
                span_deg = sep;
            }
        }
    }

    let brightest_class = members
        .iter()
        .map(|&i| stars[i].magnitude_class)
        .min()
        .expect("a figure always has at least one member");

    let ecliptic = ecliptic_of(&centroid, obliquity_mean_deg);
    let on_ecliptic = ecliptic.lat_deg.abs() <= span_deg / 2.0 + 8.0;

    Figure {
        member_count: members.len(),
        centroid,
        span_deg,
        brightest_class,
        on_ecliptic,
    }
}

/// Cluster the unified bright-star catalog (notable neighbors, magnitude
/// class 1 by definition, plus background starfield entries at or brighter
/// than [`FIGURE_MAGNITUDE_FLOOR`]) into star figures via single-link
/// clustering on the unit sphere: two stars join the same figure iff their
/// great-circle separation is at most [`FIGURE_SEPARATION_DEG`]. Clusters
/// smaller than [`FIGURE_MIN_MEMBERS`] are discarded. Deterministic: same
/// seed and system, same output, in descending member-count order (ties
/// broken by ascending centroid right ascension).
pub fn figures(astronomy_seed: Seed, system: &StarSystem) -> Vec<Figure> {
    let mut stars: Vec<BrightStar> = system
        .neighbors
        .iter()
        .map(|n| BrightStar {
            magnitude_class: 1,
            ra_deg: n.right_ascension,
            dec_deg: n.declination,
        })
        .collect();
    stars.extend(starfield(astronomy_seed).into_iter().filter_map(|s| {
        (s.magnitude_class <= FIGURE_MAGNITUDE_FLOOR).then_some(BrightStar {
            magnitude_class: s.magnitude_class,
            ra_deg: s.ra_deg,
            dec_deg: s.dec_deg,
        })
    }));

    stars.sort_by(|a, b| {
        a.magnitude_class
            .cmp(&b.magnitude_class)
            .then_with(|| a.ra_deg.total_cmp(&b.ra_deg))
            .then_with(|| a.dec_deg.total_cmp(&b.dec_deg))
    });

    let n = stars.len();
    let mut parent: Vec<usize> = (0..n).collect();
    for i in 0..n {
        for j in (i + 1)..n {
            let sep = great_circle_sep_deg(
                stars[i].ra_deg,
                stars[i].dec_deg,
                stars[j].ra_deg,
                stars[j].dec_deg,
            );
            if sep <= FIGURE_SEPARATION_DEG {
                union(&mut parent, i, j);
            }
        }
    }

    let mut groups: BTreeMap<usize, Vec<usize>> = BTreeMap::new();
    for i in 0..n {
        let root = find(&mut parent, i);
        groups.entry(root).or_default().push(i);
    }

    let mut result: Vec<Figure> = groups
        .into_values()
        .filter(|members| members.len() >= FIGURE_MIN_MEMBERS)
        .map(|members| build_figure(&stars, &members, system.forcing.obliquity_mean))
        .collect();

    result.sort_by(|a, b| {
        b.member_count
            .cmp(&a.member_count)
            .then_with(|| a.centroid.ra_deg.total_cmp(&b.centroid.ra_deg))
    });

    result
}

const COUNT_WORDS: [&str; 10] = [
    "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve",
];

/// A small count as prose, starting at three (the clustering floor):
/// anything past twelve reads honestly as "many".
fn count_word(n: usize) -> &'static str {
    COUNT_WORDS
        .get(n.wrapping_sub(3))
        .copied()
        .unwrap_or("many")
}

/// A deterministic structural description of a figure: "a {tight|loose}
/// {chain|knot} of {count} in the {region}" — chain when the figure is long
/// and thin (`span_deg / member_count >= 3.0`), knot otherwise; tight when
/// `span_deg < 10`, loose otherwise. No digits, no proper names.
/// type-audit: bare-ok(prose: return)
pub fn describe(figure: &Figure) -> String {
    let shape = if figure.span_deg / figure.member_count as f64 >= 3.0 {
        "chain"
    } else {
        "knot"
    };
    let tightness = if figure.span_deg < 10.0 {
        "tight"
    } else {
        "loose"
    };
    let region = region_word(figure.centroid.dec_deg);
    format!(
        "a {tightness} {shape} of {} in the {region}",
        count_word(figure.member_count)
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pins::SkyPins;
    use crate::system::generate;

    #[test]
    fn figures_are_deterministic() {
        let seed = Seed(42);
        let astronomy_seed = seed.derive("astronomy");
        let outcome = generate(seed, &SkyPins::default()).unwrap();
        let a = figures(astronomy_seed, &outcome.system);
        let b = figures(astronomy_seed, &outcome.system);
        assert_eq!(a, b);
    }

    #[test]
    fn every_figure_meets_the_minimum_member_count() {
        for seed in 0..32u64 {
            let astronomy_seed = Seed(seed).derive("astronomy");
            let outcome = generate(Seed(seed), &SkyPins::default()).unwrap();
            for figure in figures(astronomy_seed, &outcome.system) {
                assert!(
                    figure.member_count >= FIGURE_MIN_MEMBERS,
                    "seed {seed}: figure has {} members, floor is {}",
                    figure.member_count,
                    FIGURE_MIN_MEMBERS
                );
            }
        }
    }

    #[test]
    fn describe_has_no_digits_and_names_a_region() {
        for seed in 0..32u64 {
            let astronomy_seed = Seed(seed).derive("astronomy");
            let outcome = generate(Seed(seed), &SkyPins::default()).unwrap();
            for figure in figures(astronomy_seed, &outcome.system) {
                let text = describe(&figure);
                assert!(
                    !text.chars().any(|c| c.is_ascii_digit()),
                    "seed {seed}: description contains a digit: {text}"
                );
                assert!(
                    text.contains("northern sky")
                        || text.contains("southern sky")
                        || text.contains("the equator's road"),
                    "seed {seed}: description names no region: {text}"
                );
            }
        }
    }

    /// The degenerate-centroid guard: members whose unit vectors sum to
    /// (near-)zero — three stars 120° apart around the equator — must fall
    /// back to the first member's position, never divide by ~0 and let NaN
    /// propagate into the centroid, region, or ecliptic answers.
    #[test]
    fn a_degenerate_member_sum_falls_back_to_the_first_members_seat() {
        let stars: Vec<BrightStar> = [0.0, 120.0, 240.0]
            .iter()
            .map(|&ra_deg| BrightStar {
                magnitude_class: 1,
                ra_deg,
                dec_deg: 0.0,
            })
            .collect();
        let figure = build_figure(&stars, &[0, 1, 2], 23.5);
        assert!(
            figure.centroid.ra_deg.is_finite() && figure.centroid.dec_deg.is_finite(),
            "degenerate centroid must never be NaN: {:?}",
            figure.centroid
        );
        assert_eq!(figure.centroid.ra_deg, 0.0);
        assert_eq!(figure.centroid.dec_deg, 0.0);
        assert!(figure.span_deg.is_finite());
        // The fallback centroid feeds the ecliptic test like any other:
        // recompute independently and agree.
        let ecliptic = ecliptic_of(&figure.centroid, 23.5);
        assert_eq!(
            figure.on_ecliptic,
            ecliptic.lat_deg.abs() <= figure.span_deg / 2.0 + 8.0
        );
    }

    /// The ecliptic-flag property, recomputed independently of `figures`'
    /// own internals: for every produced figure, `on_ecliptic` must agree
    /// with an outside recomputation via `ecliptic_of` at the system's
    /// genesis obliquity.
    #[test]
    fn on_ecliptic_matches_an_independent_recomputation_across_seeds() {
        for seed in 0..32u64 {
            let astronomy_seed = Seed(seed).derive("astronomy");
            let outcome = generate(Seed(seed), &SkyPins::default()).unwrap();
            let obliquity_mean = outcome.system.forcing.obliquity_mean;
            for figure in figures(astronomy_seed, &outcome.system) {
                let ecliptic = ecliptic_of(&figure.centroid, obliquity_mean);
                let expected = ecliptic.lat_deg.abs() <= figure.span_deg / 2.0 + 8.0;
                assert_eq!(
                    figure.on_ecliptic, expected,
                    "seed {seed}: on_ecliptic disagrees with independent recomputation"
                );
            }
        }
    }
}
