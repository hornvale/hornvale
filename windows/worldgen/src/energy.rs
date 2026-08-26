//! Named subterranean energy sources — the underworld's supply, factored into
//! the terms a chamber can actually eat (Task 4, The Sources, rung 2 of the
//! Underworld Larder). Nothing here sums them into a field yet: Task 5
//! consumes [`EnergySource::ALL`] for that, and Task 9 feeds the result to
//! species.
//!
//! # The accounting (metaplan §3.2)
//!
//! `BIO-subterranean-energy-sources` names six sources, and mapped onto
//! shipped axes they are not six independent inputs. Three read [`silica`]
//! at different bands (ultramafic/mafic/granitic); `sulphide_oxidation` reads
//! `metamorphic_grade`; `methanogenesis` reads `carbonate x porosity`;
//! `geothermal` reads the gradient. `induration` is deliberately excluded —
//! the metaplan measured `induration x metamorphic_grade` at 0.9818, so
//! admitting both would double-count one signal under two names — and
//! `grain` is unused because no source names it.
//!
//! [`silica`]: hornvale_terrain::MaterialBuffer::silica
//!
//! # The seventh term (controller Ruling P1)
//!
//! [`EnergySource::DetritalImport`] is **not one of the registry row's six**.
//! `BIO-subterranean-energy-sources` names six sources and every one is
//! lithological and chemotrophic, but `domains/climate/src/underworld.rs`'s
//! ENERGY axis note says the underworld's real supply is *detrital import
//! near the surface AND chemolithotrophy off the geothermal gradient at
//! depth* — both halves — and the row names only the half the six chemotrophic
//! terms cover. Without an import term the sum built in Task 5 would have no
//! shallow arm and could not produce the U that task measures; it would be a
//! field structurally incapable of the shape the campaign is testing for. So
//! this module ships that seventh, admittedly extra-registry, term rather
//! than silently leaving the shallow half of underworld.rs's own claim
//! unmodelled.
//!
//! Physically, `DetritalImport`'s driver is overhead drainage — what
//! `underworld.rs` calls "overhead `drainage`, what arrives"
//! (`hornvale_terrain::GeneratedTerrain::drainage_at`) — falling with depth.
//! But [`EnergySource::yield_at`] is a pure function of a material buffer, a
//! gradient, a depth and a rung's moisture; it has no `Vertex` or
//! `GeneratedTerrain` to call `drainage_at` on (nothing else in this enum
//! needs one, and adding a fifth parameter just for this term would make
//! every other source's signature carry a field it ignores). So this term
//! approximates the physical driver with the one quantity the signature
//! already carries that falls the same way drainage does: `depth_m` itself.
//! Real per-vertex drainage would refine this if a later task wires it
//! through; until then, depth is the correct proxy available at this
//! module's boundary.

use hornvale_terrain::{GeothermalGradient, MaterialBuffer};

/// Smoothstep, the third private copy in this tree (see the module-level
/// note in the task report): `kernel/src/noise.rs` and
/// `domains/terrain/src/rift.rs` both have one and both are private, and
/// `domains/alchemy`'s `clamp01` is `pub(crate)` to its own crate. A
/// smoothstep is a three-term polynomial; writing a third private copy here
/// costs less than promoting a kernel-private symbol to serve one caller in
/// `windows/`, which is a layering change this task has no mandate to make.
fn smoothstep(edge0: f64, edge1: f64, x: f64) -> f64 {
    let t = ((x - edge0) / (edge1 - edge0)).clamp(0.0, 1.0);
    t * t * (3.0 - 2.0 * t)
}

/// A smooth triangular window in `[0,1]`, `1.0` at `center` and `0.0` at
/// `center ± half_width` and beyond — the shape a silica band or any other
/// rock-class window in this module is built from. Built on [`smoothstep`]
/// rather than a bare linear tent so a band's shoulders are continuous in
/// slope, not just in value.
fn bump(x: f64, center: f64, half_width: f64) -> f64 {
    let distance = (x - center).abs();
    let closeness = (1.0 - distance / half_width).clamp(0.0, 1.0);
    smoothstep(0.0, 1.0, closeness)
}

/// Silica centre for [`EnergySource::Serpentinization`] — deep ultramafic
/// (peridotite), the low end of the felsic index.
const SERPENTINIZATION_SILICA_CENTER: f64 = 0.05;
/// Half-width of the serpentinization silica band.
const SERPENTINIZATION_SILICA_HALF_WIDTH: f64 = 0.35;
/// Silica centre for [`EnergySource::IronReduction`] — mafic (basalt/gabbro),
/// the low-mid felsic index.
const IRON_REDUCTION_SILICA_CENTER: f64 = 0.45;
/// Half-width of the iron-reduction silica band.
const IRON_REDUCTION_SILICA_HALF_WIDTH: f64 = 0.35;
/// Silica centre for [`EnergySource::Radiolysis`] — granite, the high end of
/// the felsic index, where radioactive K/U/Th are concentrated.
const RADIOLYSIS_SILICA_CENTER: f64 = 0.9;
/// Half-width of the radiolysis silica band.
const RADIOLYSIS_SILICA_HALF_WIDTH: f64 = 0.35;

/// Depth (m) at which [`EnergySource::SulphideOxidation`]'s redox front
/// peaks — the reach both the falling "oxidant from above" term and the
/// rising "sulphide from below" term share, which is what places the peak at
/// their midpoint rather than at either endpoint.
const SULPHIDE_OXIDATION_FRONT_REACH_M: f64 = 700.0;

/// Temperature rise (K) at which [`EnergySource::Geothermal`] reaches half
/// its asymptotic yield — the saturating reach in the rational form, chosen
/// so a cratonic gradient at a mid-depth rung reads a modest fraction rather
/// than the full ruler (a chamber's own heat is one of seven terms, not the
/// whole supply).
const GEOTHERMAL_REACH_K: f64 = 50.0;

/// Depth (m) at which [`EnergySource::DetritalImport`] falls to half its
/// surface value — the shallow reach of gravity/water-borne surface material
/// before it thins out with distance from the entrance.
const DETRITAL_IMPORT_REACH_M: f64 = 200.0;

/// A named subterranean energy source a chamber can draw on (spec
/// `BIO-subterranean-energy-sources`, plus [`DetritalImport`] — controller
/// Ruling P1, see the module doc). Each variant is a pure function of shipped
/// lithology, the geothermal gradient, depth and a rung's moisture; nothing
/// here sums them yet.
///
/// [`DetritalImport`]: EnergySource::DetritalImport
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EnergySource {
    /// Peridotite + water → serpentine + H2 (ultramafic rock, low silica).
    /// A water-rock reaction: yields nothing in a dry chamber.
    Serpentinization,
    /// Microbial/abiotic Fe(III) reduction in mafic rock (low-mid silica).
    IronReduction,
    /// Radioactive decay (K/U/Th, concentrated in felsic granite) splitting
    /// pore water and rock, releasing H2 and oxidants.
    Radiolysis,
    /// Pyrite and other sulphides oxidizing where descending oxidant meets
    /// ascending reduced sulphur — an intermediate-depth redox front, not a
    /// rock-class band.
    SulphideOxidation,
    /// Microbial methanogenesis in carbonate rock with enough porosity to
    /// host fluid flow and a microbial habitat. A water-rock reaction:
    /// yields nothing in a dry chamber.
    Methanogenesis,
    /// The gradient itself: deeper, hotter rock is more energy a
    /// chemolithotroph can draw on, independent of local mineralogy.
    Geothermal,
    /// Surface-sourced organic and mineral material (rockfall, percolating
    /// detritus) that thins out with distance from the entrance — the
    /// registry row's missing shallow arm (controller Ruling P1; see the
    /// module doc for why this is not one of the row's six).
    DetritalImport,
}

impl EnergySource {
    /// All seven sources — the row's six plus [`EnergySource::DetritalImport`].
    pub const ALL: [EnergySource; 7] = [
        EnergySource::Serpentinization,
        EnergySource::IronReduction,
        EnergySource::Radiolysis,
        EnergySource::SulphideOxidation,
        EnergySource::Methanogenesis,
        EnergySource::Geothermal,
        EnergySource::DetritalImport,
    ];

    /// This source's yield, `[0,1]`, given the chamber's material buffer,
    /// geothermal gradient, depth below the surface (m) and a rung's
    /// moisture (`[0,1]`, Task 3's [`Substrate::moisture`] /
    /// `chamber_moisture`).
    ///
    /// [`Substrate::moisture`]: crate::Substrate::moisture
    /// type-audit: bare-ok(diagnostic-value: depth_m), bare-ok(ratio: moisture), bare-ok(ratio: return)
    pub fn yield_at(
        &self,
        buffer: &MaterialBuffer,
        gradient: GeothermalGradient,
        depth_m: f64,
        moisture: f64,
    ) -> f64 {
        match self {
            EnergySource::Serpentinization => {
                bump(
                    buffer.silica,
                    SERPENTINIZATION_SILICA_CENTER,
                    SERPENTINIZATION_SILICA_HALF_WIDTH,
                ) * moisture
            }
            EnergySource::IronReduction => bump(
                buffer.silica,
                IRON_REDUCTION_SILICA_CENTER,
                IRON_REDUCTION_SILICA_HALF_WIDTH,
            ),
            EnergySource::Radiolysis => bump(
                buffer.silica,
                RADIOLYSIS_SILICA_CENTER,
                RADIOLYSIS_SILICA_HALF_WIDTH,
            ),
            EnergySource::SulphideOxidation => {
                let d = depth_m.max(0.0);
                let r = SULPHIDE_OXIDATION_FRONT_REACH_M;
                // 4rd/(r+d)^2: an oxidant term falling as r/(r+d) times a
                // sulphide term rising as d/(r+d). Peaks at d=r with value
                // 1.0 (AM-GM: for fixed product r*d, (r+d)^2 is minimized
                // when r=d), so this is already normalized to [0,1] before
                // scaling by metamorphic grade.
                let front = 4.0 * r * d / (r + d).powi(2);
                buffer.metamorphic_grade * front
            }
            EnergySource::Methanogenesis => buffer.carbonate * buffer.porosity * moisture,
            EnergySource::Geothermal => {
                let temp_rise_k = depth_m.max(0.0) * gradient.get() / 1000.0;
                temp_rise_k / (temp_rise_k + GEOTHERMAL_REACH_K)
            }
            EnergySource::DetritalImport => {
                let d = depth_m.max(0.0);
                DETRITAL_IMPORT_REACH_M / (DETRITAL_IMPORT_REACH_M + d)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_terrain::{Basement, MarginPolarity, SoilDepth};

    /// A `MaterialBuffer` test fixture. Only `silica`, `carbonate`,
    /// `porosity` and `metamorphic_grade` are parameters — the four this
    /// module reads. The other six fields are fixed constants chosen to be
    /// unremarkable (mid-range or the commonest variant) so a source that
    /// turns out to read one of them would silently be reading a constant,
    /// not a swept value — reported as required.
    ///
    /// Fixed: `grain = 0.5`, `induration = 0.5`, `margin =
    /// MarginPolarity::Interior`, `soil_depth = SoilDepth::new(0.0)`,
    /// `basement = Basement::Continental`, `thaumic = 0.0` (the
    /// metaphysically-inert tier's identity value, per its own doc comment).
    fn buffer(
        silica: f64,
        carbonate: f64,
        porosity: f64,
        metamorphic_grade: f64,
    ) -> MaterialBuffer {
        MaterialBuffer {
            silica,
            grain: 0.5,
            induration: 0.5,
            carbonate,
            metamorphic_grade,
            porosity,
            margin: MarginPolarity::Interior,
            soil_depth: SoilDepth::new(0.0),
            basement: Basement::Continental,
            thaumic: 0.0,
        }
    }

    #[test]
    fn every_source_is_a_ratio() {
        // Each source lands in [0,1] over the whole input domain, including the
        // degenerate corners. A source that can exceed 1 would let the sum leave
        // the ENERGY ruler, which EnvironmentVector::new rejects outright.
        for silica in [0.0, 0.25, 0.5, 0.75, 1.0] {
            for other in [0.0, 0.5, 1.0] {
                let m = buffer(silica, other, other, other);
                // NOT 0.0: `GeothermalGradient::new` carries
                // `debug_assert!(k_per_km.is_finite() && k_per_km > 0.0)`, and
                // tests run in debug. Use a small positive value at the cold end.
                for g in [1.0, 25.0, 120.0] {
                    let grad = GeothermalGradient::new(g);
                    for source in EnergySource::ALL {
                        let v = source.yield_at(&m, grad, 500.0, other);
                        assert!(
                            (0.0..=1.0).contains(&v) && v.is_finite(),
                            "{source:?} returned {v} for silica={silica} other={other} grad={g}"
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn the_three_silica_sources_peak_at_different_silica() {
        // The reduction above is honest only if the three bands are actually
        // distinct. If two peak together they are one source with two names.
        let peak = |s: EnergySource| {
            let grad = GeothermalGradient::new(25.0);
            (0..=100)
                .map(|i| i as f64 / 100.0)
                .max_by(|a, b| {
                    s.yield_at(&buffer(*a, 0.5, 0.5, 0.5), grad, 500.0, 0.5)
                        .total_cmp(&s.yield_at(&buffer(*b, 0.5, 0.5, 0.5), grad, 500.0, 0.5))
                })
                .expect("a non-empty sweep")
        };
        let serp = peak(EnergySource::Serpentinization);
        let iron = peak(EnergySource::IronReduction);
        let radio = peak(EnergySource::Radiolysis);
        assert!(
            serp < iron && iron < radio,
            "the three silica bands must be ordered ultramafic < mafic < granitic, \
             got serpentinization={serp}, iron={iron}, radiolysis={radio}"
        );
        assert!(
            (iron - serp).abs() > 0.1 && (radio - iron).abs() > 0.1,
            "two silica sources peak within 0.1 of each other — they are one \
             source under two names, and the six-source claim is not honest"
        );
    }

    #[test]
    fn a_water_rock_reaction_needs_water() {
        // Serpentinization and methanogenesis are water-rock reactions. In a dry
        // chamber they yield nothing, whatever the rock says.
        let grad = GeothermalGradient::new(25.0);
        let rich = buffer(0.1, 0.9, 0.9, 0.9);
        for source in [EnergySource::Serpentinization, EnergySource::Methanogenesis] {
            let wet = source.yield_at(&rich, grad, 500.0, 1.0);
            let dry = source.yield_at(&rich, grad, 500.0, 0.0);
            assert!(
                wet > 0.0,
                "{source:?} yields nothing even wet — check the rock inputs"
            );
            assert_eq!(
                dry, 0.0,
                "{source:?} yields {dry} in a chamber with no water"
            );
        }
    }

    #[test]
    fn sulphide_oxidation_peaks_at_intermediate_depth() {
        // It needs oxidant from above meeting sulphide from below, so it is the
        // one source that is neither rising nor falling in depth. This is what
        // makes the U's trough possible rather than imposed.
        let grad = GeothermalGradient::new(25.0);
        let m = buffer(0.5, 0.5, 0.5, 0.8);
        let at = |d: f64| EnergySource::SulphideOxidation.yield_at(&m, grad, d, 0.5);
        let depths: Vec<f64> = (0..=20).map(|i| i as f64 * 100.0).collect();
        let best = depths
            .iter()
            .copied()
            .max_by(|a, b| at(*a).total_cmp(&at(*b)))
            .expect("a non-empty sweep");
        assert!(
            best > 0.0 && best < 2000.0,
            "sulphide oxidation peaked at {best} m — an endpoint peak means it is \
             monotone, not an intermediate-depth redox front"
        );
    }
}
