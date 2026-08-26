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
//! # The seventh term (controller Ruling P1, revised in fix round 1)
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
//! **`DetritalImport` now reads a real `drainage: f64` parameter**, not a
//! depth-only approximation. Fix round 1 corrected the original ruling: the
//! first cut cited `GeneratedTerrain::drainage_at` as the physical driver but
//! specified a `yield_at` signature with no `Vertex` and no terrain to call
//! it on, so the implementation fell back to depth alone — which makes this
//! source identical at every vertex sharing a depth, contributing a pure
//! depth profile with no spatial signal. That is harmless for Task 5's U
//! (a per-depth shape), but Task 6 measures energy variation *between
//! worlds*, and a term with zero spatial variance would dilute exactly the
//! separation that task is measuring, for a reason that was an artifact of
//! this module's original signature rather than a fact about rock or water.
//! `yield_at` now takes `drainage` directly (the caller supplies
//! `GeneratedTerrain::drainage_at(vertex)`, or any value at this module's
//! test boundary), and [`EnergySource::DetritalImport`] is the only variant
//! that reads it.
//!
//! The citation for treating drainage as the shallow-arm driver is the
//! ENERGY axis's own level docs in `underworld.rs`, not just its WATER-axis
//! aside about "what arrives": `E_FED` ("a working base") is glossed as *"a
//! stream's organic load, or a modest chemical one"*, `E_RICH` explicitly
//! names *"direct detrital delivery"* as one of the two things that reach it
//! (the other being sulphide oxidation at depth — corroborating
//! [`EnergySource::SulphideOxidation`]'s own calibration as a bonus), and
//! `E_TEEMING`, the axis's richest level, is *"a whole channel's load at one
//! point."* Flow accumulation (drainage) is the shipped proxy for "is there
//! a stream here", which is exactly the quantity those three levels are
//! staged against. This module's own calibration (see
//! [`DETRITAL_IMPORT_DRAINAGE_REACH`]) anchors the half-yield point at the
//! terrain crate's own documented p90 drainage figure
//! (`hornvale_terrain::lithology::ALLUVIUM_DRAINAGE_MIN`'s doc comment: a
//! 4-seed survey found land drainage p50=3, p90=12, p95=18, p99=47, max
//! 338), so a genuinely high-flow vertex reads near `E_RICH`/`E_TEEMING` and
//! a middling one reads near `E_FED`.
//!
//! # Every chemotrophic source is now moisture-gated (fix round 1, concern 2)
//!
//! The original cut gated only [`EnergySource::Serpentinization`] and
//! [`EnergySource::Methanogenesis`] on moisture, because those were the two
//! the brief's own test named. On inspection every one of the row's six
//! mechanisms is an aqueous process — serpentinization consumes water as a
//! stoichiometric reactant; radiolysis splits porewater molecule by
//! molecule; microbial iron reduction needs an aqueous medium for electron
//! transfer; sulphide oxidation needs a connected fluid pathway to carry
//! oxidant to the front; methanogenesis is a microbial reaction that
//! consumes/produces water. And more fundamentally: this axis measures
//! energy available for **primary production** (the kernel's own words for
//! `ENERGY`) — life, not raw physical potential — and life needs water
//! categorically, regardless of which disequilibrium is feeding it. So
//! [`EnergySource::Geothermal`] is gated too: a bone-dry hot fracture has
//! thermal potential but nothing here to eat it. Each variant's doc comment
//! below states its own gate's shape and why; they are not identical (a
//! stoichiometric reactant scales with water quantity, a medium-only
//! requirement saturates at a trace, a connected-pathway requirement needs
//! more than a film) — see each variant and the corresponding arm of
//! [`EnergySource::yield_at`].
//!
//! [`EnergySource::DetritalImport`] is the one exception, deliberately: its
//! own water proxy **is** `drainage`, so gating it on `moisture` as well
//! would double-count the same physical fact under two names, the same
//! reason `induration` was excluded from the silica accounting above.
//!
//! # Nothing here was shaped to produce a U
//!
//! Every gate below is a pure function of `moisture` (or, for
//! `DetritalImport`, `drainage`) alone — none of them read `depth_m` — so
//! none of them can shift where a source peaks in depth. Verified: the
//! silica-separation and sulphide-oxidation-peaks-at-intermediate-depth
//! tests both fix moisture at `0.5`, where every new gate is already fully
//! saturated (`1.0`), so their assertions are unchanged in either value or
//! meaning by this round's edit.

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

/// A water-presence gate: `0.0` at `moisture = 0`, saturating to `1.0` by
/// `moisture = saturate_at`. Distinct from a stoichiometric `* moisture`
/// multiply — this is for a mechanism that needs water to be *present* as a
/// medium or pathway rather than one whose yield scales with water
/// *quantity*.
fn water_gate(moisture: f64, saturate_at: f64) -> f64 {
    smoothstep(0.0, saturate_at, moisture)
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
/// Moisture at which [`EnergySource::IronReduction`]'s water gate saturates:
/// a thin film is sufficient electron-transfer medium, so this saturates
/// quickly rather than tracking water quantity.
const IRON_REDUCTION_MOISTURE_SATURATE: f64 = 0.25;
/// Silica centre for [`EnergySource::Radiolysis`] — granite, the high end of
/// the felsic index, where radioactive K/U/Th are concentrated.
const RADIOLYSIS_SILICA_CENTER: f64 = 0.9;
/// Half-width of the radiolysis silica band.
const RADIOLYSIS_SILICA_HALF_WIDTH: f64 = 0.35;
/// Moisture at which [`EnergySource::Radiolysis`]'s water gate saturates:
/// radiolysis splits porewater molecule by molecule, so even trace bound
/// water suffices — the gentlest, fastest-saturating gate of the six.
const RADIOLYSIS_MOISTURE_SATURATE: f64 = 0.1;

/// Depth (m) at which [`EnergySource::SulphideOxidation`]'s redox front
/// peaks — the reach both the falling "oxidant from above" term and the
/// rising "sulphide from below" term share, which is what places the peak at
/// their midpoint rather than at either endpoint.
const SULPHIDE_OXIDATION_FRONT_REACH_M: f64 = 700.0;
/// Moisture at which [`EnergySource::SulphideOxidation`]'s water gate
/// saturates: the front needs oxidant physically carried from above to
/// sulphide below, which needs an actually-connected, flowing fluid pathway
/// rather than a static film — the firmest threshold among the
/// non-stoichiometric gates.
const SULPHIDE_OXIDATION_MOISTURE_SATURATE: f64 = 0.4;

/// Moisture at which [`EnergySource::Geothermal`]'s water gate saturates:
/// this axis measures energy available to *life*, and a bone-dry hot
/// fracture has thermal potential but nothing here to exploit it, so even
/// the gradient itself needs some water present.
const GEOTHERMAL_MOISTURE_SATURATE: f64 = 0.2;
/// Temperature rise (K) at which [`EnergySource::Geothermal`] reaches half
/// its asymptotic yield — the saturating reach in the rational form, chosen
/// so a cratonic gradient at a mid-depth rung reads a modest fraction rather
/// than the full ruler (a chamber's own heat is one of seven terms, not the
/// whole supply).
const GEOTHERMAL_REACH_K: f64 = 50.0;

/// Depth (m) at which [`EnergySource::DetritalImport`] falls to half its
/// surface value — the shallow reach of gravity/water-borne surface material
/// before it thins out with distance from the entrance.
const DETRITAL_IMPORT_DEPTH_REACH_M: f64 = 200.0;
/// Drainage at which [`EnergySource::DetritalImport`]'s drainage term
/// reaches half its asymptotic value. Anchored at the terrain crate's own
/// documented p90 drainage figure
/// (`hornvale_terrain::lithology::ALLUVIUM_DRAINAGE_MIN`'s doc comment: a
/// 4-seed survey at the canonical `Geosphere::new(6)` found land drainage
/// p50=3, p90=12, p95=18, p99=47, max 338), so a genuinely high-flow vertex
/// reads near the ENERGY axis's `E_RICH`/`E_TEEMING` levels and a middling
/// one reads near `E_FED` — see the module doc.
const DETRITAL_IMPORT_DRAINAGE_REACH: f64 = 12.0;

/// A named subterranean energy source a chamber can draw on (spec
/// `BIO-subterranean-energy-sources`, plus [`DetritalImport`] — controller
/// Ruling P1, see the module doc). Each variant is a pure function of shipped
/// lithology, the geothermal gradient, depth, a rung's moisture and overhead
/// drainage; nothing here sums them yet.
///
/// [`DetritalImport`]: EnergySource::DetritalImport
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EnergySource {
    /// Peridotite + water → serpentine + H2 (ultramafic rock, low silica).
    /// Water is a stoichiometric reactant here, so the gate is a direct
    /// `* moisture` multiply — yield tracks water quantity, not just its
    /// presence. Yields nothing in a dry chamber.
    Serpentinization,
    /// Microbial/abiotic Fe(III) reduction in mafic rock (low-mid silica).
    /// Water is only the electron-transfer medium, not a reactant, so a thin
    /// film sustains the full rate: the gate saturates quickly
    /// ([`IRON_REDUCTION_MOISTURE_SATURATE`]) rather than scaling with
    /// quantity. Still yields nothing in a dry chamber.
    IronReduction,
    /// Radioactive decay (K/U/Th, concentrated in felsic granite) splitting
    /// pore water and rock, releasing H2 and oxidants. Even trace bound
    /// porewater suffices, so this is the gentlest, fastest-saturating gate
    /// of the six ([`RADIOLYSIS_MOISTURE_SATURATE`]). Yields nothing in a
    /// dry chamber.
    Radiolysis,
    /// Pyrite and other sulphides oxidizing where descending oxidant meets
    /// ascending reduced sulphur — an intermediate-depth redox front, not a
    /// rock-class band. Needs an actually-connected, flowing fluid pathway
    /// to carry oxidant to the front, not merely a damp film, so its gate is
    /// the firmest threshold among the non-stoichiometric five
    /// ([`SULPHIDE_OXIDATION_MOISTURE_SATURATE`]). Yields nothing in a dry
    /// chamber.
    SulphideOxidation,
    /// Microbial methanogenesis in carbonate rock with enough porosity to
    /// host fluid flow and a microbial habitat. Water is consumed/produced
    /// by the reaction itself, so — like serpentinization — the gate is a
    /// direct `* moisture` multiply. Yields nothing in a dry chamber.
    Methanogenesis,
    /// The gradient itself: deeper, hotter rock is more energy a
    /// chemolithotroph can draw on, independent of local mineralogy. Gated
    /// on moisture too ([`GEOTHERMAL_MOISTURE_SATURATE`]): this axis
    /// measures energy available to *life*, and a bone-dry hot fracture has
    /// thermal potential but nothing here to exploit it.
    Geothermal,
    /// Surface-sourced organic and mineral material (rockfall, percolating
    /// detritus) that thins out with distance from the entrance — the
    /// registry row's missing shallow arm (controller Ruling P1; see the
    /// module doc for why this is not one of the row's six). Reads
    /// `drainage`, not `moisture`: drainage **is** this source's own water
    /// proxy, so gating it on moisture too would double-count the same fact
    /// under two names. Yields nothing where no drainage reaches it,
    /// whatever the depth.
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
    /// geothermal gradient, depth below the surface (m), a rung's moisture
    /// (`[0,1]`, Task 3's [`Substrate::moisture`] / `chamber_moisture`) and
    /// overhead drainage (`hornvale_terrain::GeneratedTerrain::drainage_at`
    /// at the vertex above, or any nonnegative value at this module's own
    /// test boundary). Six of seven ignore `drainage`; four of seven ignore
    /// `moisture` only in the sense of not scaling by it directly (every
    /// chemotrophic source still gates on it — see the module doc). Only
    /// [`EnergySource::DetritalImport`] reads `drainage`.
    ///
    /// [`Substrate::moisture`]: crate::Substrate::moisture
    /// type-audit: bare-ok(diagnostic-value: depth_m), bare-ok(ratio: moisture), bare-ok(diagnostic-value: drainage), bare-ok(ratio: return)
    pub fn yield_at(
        &self,
        buffer: &MaterialBuffer,
        gradient: GeothermalGradient,
        depth_m: f64,
        moisture: f64,
        drainage: f64,
    ) -> f64 {
        match self {
            EnergySource::Serpentinization => {
                bump(
                    buffer.silica,
                    SERPENTINIZATION_SILICA_CENTER,
                    SERPENTINIZATION_SILICA_HALF_WIDTH,
                ) * moisture
            }
            EnergySource::IronReduction => {
                bump(
                    buffer.silica,
                    IRON_REDUCTION_SILICA_CENTER,
                    IRON_REDUCTION_SILICA_HALF_WIDTH,
                ) * water_gate(moisture, IRON_REDUCTION_MOISTURE_SATURATE)
            }
            EnergySource::Radiolysis => {
                bump(
                    buffer.silica,
                    RADIOLYSIS_SILICA_CENTER,
                    RADIOLYSIS_SILICA_HALF_WIDTH,
                ) * water_gate(moisture, RADIOLYSIS_MOISTURE_SATURATE)
            }
            EnergySource::SulphideOxidation => {
                let d = depth_m.max(0.0);
                let r = SULPHIDE_OXIDATION_FRONT_REACH_M;
                // 4rd/(r+d)^2: an oxidant term falling as r/(r+d) times a
                // sulphide term rising as d/(r+d). Peaks at d=r with value
                // 1.0 (AM-GM: for fixed product r*d, (r+d)^2 is minimized
                // when r=d), so this is already normalized to [0,1] before
                // scaling by metamorphic grade and the water gate.
                let front = 4.0 * r * d / (r + d).powi(2);
                buffer.metamorphic_grade
                    * front
                    * water_gate(moisture, SULPHIDE_OXIDATION_MOISTURE_SATURATE)
            }
            EnergySource::Methanogenesis => buffer.carbonate * buffer.porosity * moisture,
            EnergySource::Geothermal => {
                let temp_rise_k = depth_m.max(0.0) * gradient.get() / 1000.0;
                (temp_rise_k / (temp_rise_k + GEOTHERMAL_REACH_K))
                    * water_gate(moisture, GEOTHERMAL_MOISTURE_SATURATE)
            }
            EnergySource::DetritalImport => {
                let d = depth_m.max(0.0);
                let depth_falloff =
                    DETRITAL_IMPORT_DEPTH_REACH_M / (DETRITAL_IMPORT_DEPTH_REACH_M + d);
                let dr = drainage.max(0.0);
                let drainage_term = dr / (dr + DETRITAL_IMPORT_DRAINAGE_REACH);
                depth_falloff * drainage_term
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
        // Drainage sweeps the terrain crate's own documented distribution
        // (p50=3, p90=12, max=338 — ALLUVIUM_DRAINAGE_MIN's doc comment),
        // exercising DetritalImport's new dependency at both realistic and
        // extreme magnitudes.
        for silica in [0.0, 0.25, 0.5, 0.75, 1.0] {
            for other in [0.0, 0.5, 1.0] {
                let m = buffer(silica, other, other, other);
                // NOT 0.0: `GeothermalGradient::new` carries
                // `debug_assert!(k_per_km.is_finite() && k_per_km > 0.0)`, and
                // tests run in debug. Use a small positive value at the cold end.
                for g in [1.0, 25.0, 120.0] {
                    let grad = GeothermalGradient::new(g);
                    for drainage in [0.0, 3.0, 12.0, 338.0] {
                        for source in EnergySource::ALL {
                            let v = source.yield_at(&m, grad, 500.0, other, drainage);
                            assert!(
                                (0.0..=1.0).contains(&v) && v.is_finite(),
                                "{source:?} returned {v} for silica={silica} other={other} \
                                 grad={g} drainage={drainage}"
                            );
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn the_three_silica_sources_peak_at_different_silica() {
        // The reduction above is honest only if the three bands are actually
        // distinct. If two peak together they are one source with two names.
        // moisture is fixed at 0.5, where every water gate below is already
        // saturated to 1.0 (see the module doc's "nothing here was shaped to
        // produce a U" note), so this is unaffected by fix round 1's gating.
        let peak = |s: EnergySource| {
            let grad = GeothermalGradient::new(25.0);
            (0..=100)
                .map(|i| i as f64 / 100.0)
                .max_by(|a, b| {
                    s.yield_at(&buffer(*a, 0.5, 0.5, 0.5), grad, 500.0, 0.5, 0.0)
                        .total_cmp(&s.yield_at(&buffer(*b, 0.5, 0.5, 0.5), grad, 500.0, 0.5, 0.0))
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
    fn every_chemotrophic_source_needs_water() {
        // Fix round 1, concern 2: every one of the six registry sources is an
        // aqueous process (this axis measures energy for PRIMARY PRODUCTION —
        // life — not raw physical potential), so all six yield nothing in a
        // dry chamber, whatever the rock or the gradient says. DetritalImport
        // is excluded deliberately: its own water proxy is drainage, tested
        // separately below.
        let grad = GeothermalGradient::new(25.0);
        // Each source needs a buffer/depth favorable to its OWN non-water
        // axis, or "dry == 0" would be true for the wrong reason (its rock
        // term already reading zero, not the water gate).
        let cases: [(EnergySource, MaterialBuffer, f64); 6] = [
            (
                EnergySource::Serpentinization,
                buffer(SERPENTINIZATION_SILICA_CENTER, 0.5, 0.5, 0.5),
                500.0,
            ),
            (
                EnergySource::IronReduction,
                buffer(IRON_REDUCTION_SILICA_CENTER, 0.5, 0.5, 0.5),
                500.0,
            ),
            (
                EnergySource::Radiolysis,
                buffer(RADIOLYSIS_SILICA_CENTER, 0.5, 0.5, 0.5),
                500.0,
            ),
            (
                EnergySource::SulphideOxidation,
                buffer(0.5, 0.5, 0.5, 0.8),
                SULPHIDE_OXIDATION_FRONT_REACH_M,
            ),
            (
                EnergySource::Methanogenesis,
                buffer(0.1, 0.9, 0.9, 0.9),
                500.0,
            ),
            (EnergySource::Geothermal, buffer(0.5, 0.5, 0.5, 0.5), 2000.0),
        ];
        for (source, m, depth_m) in cases {
            let wet = source.yield_at(&m, grad, depth_m, 1.0, 0.0);
            let dry = source.yield_at(&m, grad, depth_m, 0.0, 0.0);
            assert!(
                wet > 0.0,
                "{source:?} yields nothing even wet — check the non-water inputs"
            );
            assert_eq!(
                dry, 0.0,
                "{source:?} yields {dry} in a chamber with no water"
            );
        }
    }

    #[test]
    fn detrital_import_needs_drainage() {
        // Fix round 1, concern 1: DetritalImport now reads real drainage, not
        // a depth-only proxy. No overhead flow, no import — whatever the
        // depth.
        let grad = GeothermalGradient::new(25.0);
        let m = buffer(0.5, 0.5, 0.5, 0.5);
        for depth_m in [0.0, 500.0, 2000.0] {
            let flowing = EnergySource::DetritalImport.yield_at(&m, grad, depth_m, 0.5, 12.0);
            let starved = EnergySource::DetritalImport.yield_at(&m, grad, depth_m, 0.5, 0.0);
            assert!(
                flowing > 0.0,
                "DetritalImport yields nothing even with p90 drainage at depth={depth_m}"
            );
            assert_eq!(
                starved, 0.0,
                "DetritalImport yields {starved} with zero drainage at depth={depth_m}"
            );
        }
    }

    #[test]
    fn sulphide_oxidation_peaks_at_intermediate_depth() {
        // It needs oxidant from above meeting sulphide from below, so it is the
        // one source that is neither rising nor falling in depth. This is what
        // makes the U's trough possible rather than imposed. moisture is fixed
        // at 0.5, where the water gate is already saturated to 1.0, so this is
        // unaffected by fix round 1's gating.
        let grad = GeothermalGradient::new(25.0);
        let m = buffer(0.5, 0.5, 0.5, 0.8);
        let at = |d: f64| EnergySource::SulphideOxidation.yield_at(&m, grad, d, 0.5, 0.0);
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
