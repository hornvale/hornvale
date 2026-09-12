//! Named subterranean energy sources — the underworld's supply, factored into
//! the terms a chamber can actually eat (Task 4, The Sources, rung 2 of the
//! Underworld Larder). [`chemical_supply`] reduces them to the food
//! vocabulary a diet names, and [`chemical_supply_field_per_rung`] evaluates
//! that over the globe; the capacity loops in [`crate`] read it.
//!
//! # Reactions are not foods (The Trencher, spec §4.2)
//!
//! The seven members of [`EnergySource::ALL`] are **reactions**; a diet names
//! **metabolites**. The two are not in bijection and never were, so the
//! reduction is a routing table ([`EnergySource::route`]) rather than an
//! identity:
//!
//! - two reactions yield H₂ (serpentinization and radiolysis) and feed **one**
//!   axis, summed;
//! - one term is not food at all ([`EnergySource::Geothermal`] — a thermal
//!   gradient), and becomes a **modifier** on the chemical supplies;
//! - one term is not *chemical* food ([`EnergySource::DetritalImport`] —
//!   surface detritus), and routes to the existing `DETRITUS` axis.
//!
//! Each mapping is argued at its own [`SupplyRoute`] variant and at
//! [`chemical_supply`], because each is a judgment rather than a lookup.
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
//! **The U that justified it does not exist, and the term survives anyway
//! (The Trencher, Task 4, 2026-09-11).** The paragraph above is kept as the
//! record of why this term was added, but its prediction was falsified by the
//! very measurement it was added to enable: `derived_energy_is_monotone_not_a
//! _trough` measured per-rung `ENERGY` medians of 0.168609 / 0.200822 /
//! 0.265342 / 0.281421 / 0.281449 (`Undercroft` → `Nadir`, 2026-08-26) —
//! strictly non-decreasing, the opposite of a trough. So "could not produce
//! the U" was never the reason to keep it; what *is* the reason is the
//! sentence before it, that underworld.rs names detrital import as one of the
//! two real halves of underworld supply. The Trencher therefore does not
//! delete the term — it **reclassifies** it. `DetritalImport` is surface
//! organic matter, so its yield now routes to the existing
//! `hornvale_kernel::DETRITUS` axis ([`SupplyRoute::Detritus`]) instead of
//! being averaged into chemical food, and nothing here was contorted to
//! preserve a shape that measurement says is not there.
//!
//! `DetritalImport` reads a real `drainage: f64` parameter — the caller
//! supplies `GeneratedTerrain::drainage_at(vertex)` — rather than a
//! depth-only approximation (fix round 1: a depth-only form would have made
//! this source identical at every vertex sharing a depth, contributing no
//! spatial signal to Task 6's between-world energy-variation measurement).
//! The citation for treating drainage as the shallow-arm driver is the
//! ENERGY axis's own level docs in `underworld.rs`: `E_FED` ("a working
//! base") is glossed as *"a stream's organic load, or a modest chemical
//! one"*, `E_RICH` explicitly names *"direct detrital delivery"* as one of
//! the two things that reach it (the other being sulphide oxidation at
//! depth — corroborating [`EnergySource::SulphideOxidation`]'s own
//! calibration below, unprompted), and `E_TEEMING`, the axis's richest
//! level, is *"a whole channel's load at one point."* This module's own
//! calibration (see [`DETRITAL_IMPORT_DRAINAGE_REACH`]) anchors the
//! half-yield point at the terrain crate's own documented p90 drainage
//! figure.
//!
//! **`DetritalImport` is not also gated on `moisture`, and the reason is
//! narrower than an earlier draft of this doc claimed.** Zero drainage
//! already forces zero yield on its own (`dr.max(0.0) / (dr.max(0.0) +
//! reach)` is exactly `0.0` at `dr = 0`), so an additional moisture gate
//! would be a redundant control, not new information. Drainage (surface
//! flow-accumulation) and chamber moisture (`chamber_moisture`'s water-table
//! saturation) are physically distinct quantities — a fix round corrected an
//! earlier claim here that they were "the same fact under two names", which
//! overstated the identity between them; the real reason is simply that the
//! gate this source already has is sufficient.
//!
//! # Every chemotrophic source is moisture-gated (fix round 1, concern 2)
//!
//! Every one of the row's six mechanisms is an aqueous process —
//! serpentinization consumes water as a stoichiometric reactant; radiolysis
//! splits porewater molecule by molecule; microbial iron reduction needs an
//! aqueous medium for electron transfer; sulphide oxidation needs a
//! connected fluid pathway to carry oxidant to the front; methanogenesis is
//! a microbial reaction that consumes/produces water. And more
//! fundamentally: this axis measures energy available for **primary
//! production** (the kernel's own words for `ENERGY`) — life, not raw
//! physical potential — and life needs water categorically, regardless of
//! which disequilibrium is feeding it. So [`EnergySource::Geothermal`] is
//! gated too: a bone-dry hot fracture has thermal potential but nothing here
//! to eat it. Each variant's doc comment states its own gate's shape and
//! why; they are not identical (a stoichiometric reactant scales with water
//! quantity, a medium-only requirement saturates at a trace, a
//! connected-pathway requirement needs more than a film) — see each variant
//! and the corresponding arm of [`EnergySource::yield_at`].
//!
//! [`EnergySource::DetritalImport`] is the one exception, for the reason
//! given above.
//!
//! # `SulphideOxidation`'s front is in ΔT, not metres (fix round 2, Critical)
//!
//! The first cut placed the redox front's peak at a **fixed metre depth**
//! (700 m), symmetric in `depth_m` around that reach. The controller found
//! this puts the peak in `Band::Deeps` at every gradient in the legal 15–30
//! K/km range — and the frozen corpus this module already cites for its
//! ENERGY calibration says `Deeps` is the **trough**: `sump-gallery`
//! (`Deeps`) carries `E_INERT`, commented *"the corpus MINIMUM among wet
//! rows, and the trough the inversion turns on"*, while `sulphuric-hall`
//! (`Underdeep`, one rung deeper) carries `E_RICH`, commented *"sulphide
//! oxidation driven from below by the geothermal gradient — chemolithotrophy
//! proper, and richer than anything at `Deeps`"*. A metre-fixed front was
//! strong enough to fight the very trough Task 5 is about to measure.
//!
//! **This module now expresses the front in ΔT (K above the surface datum)
//! — the ladder's own native coordinate — rather than in metres, and that
//! choice is deliberate, not merely a bug fix.** `domains/terrain/src/
//! delve.rs` spaces its whole habitation ladder by heat precisely because a
//! fixed metre depth is two different rungs at two different gradients ("at
//! 1 km down a 15 K/km craton is 15 K above its datum and a 30 K/km young
//! crust is 30 K above its own"); a metre-fixed front inherits exactly that
//! instability, and the Critical is a direct instance of it. The
//! counter-argument the controller raised — the descending "oxidant from
//! above" term is a transport distance (metres) while the ascending
//! "sulphide from below" term is geothermal (ΔT), so a single-coordinate
//! peak mixes two physical quantities — is real, and is accepted rather than
//! resolved: this module treats ΔT as the ladder's own working definition of
//! "distance from the surface" (the same substitution `delve.rs` makes for
//! every other purpose in this campaign), rather than modelling oxidant
//! transport and sulphide supply as two genuinely independent coordinates,
//! which would need a real two-axis field this task has no mandate to build.
//! **What this choice buys, concretely:** the front's peak now lands in
//! `Band::Underdeep` by construction, for every legal gradient, not by
//! the luck of a metre value happening to fit — verified by
//! `sulphide_oxidation_peaks_in_underdeep_across_the_legal_gradient_range`.
//! A metre-fixed alternative was checked and rejected for this reason: with
//! the legal gradient range spanning a factor of 2 (15–30 K/km) and
//! `Underdeep` itself spanning almost exactly a factor of 2 in ΔT (25–50 K),
//! there is essentially no metre depth whose ΔT stays inside `Underdeep`
//! across the *whole* legal range — the two factors-of-2 leave no margin.
//!
//! The reach is **not a duplicated literal**: [`underdeep_delta_t_range`]
//! reads `Band::Underdeep`'s bounds live from
//! `hornvale_terrain::delta_t_range_of`, the same ladder `delve.rs` itself
//! computes rungs from. `DEEPS_TOP_K` has already moved once in that file's
//! own history (10 → 8, recorded in its doc comment); a duplicated literal
//! here would have gone stale exactly as silently as the original,
//! un-anchored 700 m did. [`EnergySource::Geothermal`]'s reach is anchored
//! the same way, to the same range's low edge — see its own doc comment for
//! why, and the IMPORTANT finding below for why it was not left as its
//! original self-referential justification.
//!
//! # Nothing here was shaped to produce a U — and that claim is scoped
//!
//! Every gate in [`EnergySource::yield_at`] is a pure function of `moisture`
//! (or, for `DetritalImport`, `drainage`) alone — none reads `depth_m` or
//! `gradient` — so no gate can shift where a source peaks in depth. That is
//! true **within this file**. It is not true once Task 5 wires these sources
//! into a field: `moisture` there is `Substrate::moisture`, itself a
//! function of depth via `chamber_moisture` (saturated below the water
//! table, drying above it), so the six moisture-gated sources inherit an
//! *indirect* depth-dependence once wired in, even though nothing in this
//! module encodes one directly. Task 5's implementer should meet that
//! deliberately rather than discover it.

use hornvale_kernel::{Band, Geosphere, VertexMap};
use hornvale_terrain::delve::rung_evaluation_depth_m;
use hornvale_terrain::{GeneratedTerrain, GeothermalGradient, MaterialBuffer, delta_t_range_of};

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

/// `Band::Underdeep`'s ΔT range (K above the surface datum), read live from
/// `delta_t_range_of` rather than duplicated as a literal — see the module
/// doc's "fix round 2" section for why a duplicated literal is exactly the
/// failure mode this replaces. Both [`EnergySource::SulphideOxidation`] and
/// [`EnergySource::Geothermal`] anchor to this same call.
///
/// `Underdeep` is never the ladder's open-ended bottom rung (`Nadir` is), so
/// its upper bound is always `Some`; the `.expect` documents that invariant
/// rather than guarding against a case this module can reach.
fn underdeep_delta_t_range() -> (f64, f64) {
    let (low, high) = delta_t_range_of(Band::Underdeep);
    (
        low,
        high.expect("Underdeep is not the ladder's open-ended bottom rung"),
    )
}

/// Silica centre for [`EnergySource::Serpentinization`] — deep ultramafic
/// (peridotite), the low end of the felsic index.
/// plumb: pending(wave-1)
const SERPENTINIZATION_SILICA_CENTER: f64 = 0.05;
/// Half-width of the serpentinization silica band.
/// plumb: pending(wave-1)
const SERPENTINIZATION_SILICA_HALF_WIDTH: f64 = 0.35;
/// Silica centre for [`EnergySource::IronReduction`] — mafic (basalt/gabbro),
/// the low-mid felsic index.
/// plumb: pending(wave-1)
const IRON_REDUCTION_SILICA_CENTER: f64 = 0.45;
/// Half-width of the iron-reduction silica band.
/// plumb: pending(wave-1)
const IRON_REDUCTION_SILICA_HALF_WIDTH: f64 = 0.35;
/// Moisture at which [`EnergySource::IronReduction`]'s water gate saturates:
/// a thin film is sufficient electron-transfer medium, so this saturates
/// quickly rather than tracking water quantity.
/// plumb: pending(wave-1)
const IRON_REDUCTION_MOISTURE_SATURATE: f64 = 0.25;
/// Silica centre for [`EnergySource::Radiolysis`] — granite, the high end of
/// the felsic index, where radioactive K/U/Th are concentrated.
/// plumb: pending(wave-1)
const RADIOLYSIS_SILICA_CENTER: f64 = 0.9;
/// Half-width of the radiolysis silica band.
/// plumb: pending(wave-1)
const RADIOLYSIS_SILICA_HALF_WIDTH: f64 = 0.35;
/// Moisture at which [`EnergySource::Radiolysis`]'s water gate saturates:
/// radiolysis splits porewater molecule by molecule, so even trace bound
/// water suffices — the gentlest, fastest-saturating gate of the six.
/// plumb: pending(wave-1)
const RADIOLYSIS_MOISTURE_SATURATE: f64 = 0.1;

/// Moisture at which [`EnergySource::SulphideOxidation`]'s water gate
/// saturates: the front needs oxidant physically carried from above to
/// sulphide below, which needs an actually-connected, flowing fluid pathway
/// rather than a static film — the firmest threshold among the
/// non-stoichiometric gates.
/// plumb: pending(wave-1)
const SULPHIDE_OXIDATION_MOISTURE_SATURATE: f64 = 0.4;

/// Moisture at which [`EnergySource::Geothermal`]'s water gate saturates:
/// this axis measures energy available to *life*, and a bone-dry hot
/// fracture has thermal potential but nothing here to exploit it, so even
/// the gradient itself needs some water present.
/// plumb: pending(wave-1)
const GEOTHERMAL_MOISTURE_SATURATE: f64 = 0.2;

/// How much a fully-realized geothermal gradient multiplies the chemical
/// metabolite supplies — the one knob of [`EnergySource::Geothermal`]'s
/// modifier form (spec §4.2, ledger #2). `1.0` means the modifier spans
/// `[1, 2]`: no thermal help at the surface datum, a doubling where the
/// gradient term saturates. See [`chemical_supply`] for why the form is
/// `1 + gain * g` and not one of the three alternatives.
/// plumb: pending(wave-1)
const GEOTHERMAL_MODIFIER_GAIN: f64 = 1.0;

/// Depth (m) at which [`EnergySource::DetritalImport`] falls to half its
/// surface value — the shallow reach of gravity/water-borne surface material
/// before it thins out with distance from the entrance.
/// plumb: pending(wave-1)
const DETRITAL_IMPORT_DEPTH_REACH_M: f64 = 200.0;
/// Drainage at which [`EnergySource::DetritalImport`]'s drainage term
/// reaches half its asymptotic value. Anchored at the terrain crate's own
/// documented p90 drainage figure
/// (`hornvale_terrain::lithology::ALLUVIUM_DRAINAGE_MIN`'s doc comment: a
/// 4-seed survey at the canonical `Geosphere::new(6)` found land drainage
/// p50=3, p90=12, p95=18, p99=47, max 338), so a genuinely high-flow vertex
/// reads near the ENERGY axis's `E_RICH`/`E_TEEMING` levels and a middling
/// one reads near `E_FED` — see the module doc.
/// plumb: pending(wave-1)
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
    /// ascending reduced sulphur — an intermediate-**ΔT** redox front (fix
    /// round 2: expressed in ΔT, not metres, so it peaks in `Band::Underdeep`
    /// at every legal gradient rather than at a metre depth that only landed
    /// there by luck — see the module doc), not a rock-class band. Needs an
    /// actually-connected, flowing fluid pathway to carry oxidant to the
    /// front, not merely a damp film, so its water gate is the firmest
    /// threshold among the non-stoichiometric five
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
    /// thermal potential but nothing here to exploit it. Its saturating
    /// reach (fix round 2: previously justified only by its own effect on
    /// the summed field, an IMPORTANT finding — see the module doc) is now
    /// anchored to [`underdeep_delta_t_range`]'s low edge: geothermal supply
    /// crosses its own half-yield point exactly where the ladder places the
    /// onset of the deep, geothermally-driven chemistry regime — the same
    /// boundary this round keys `SulphideOxidation`'s peak to. **This anchor
    /// is a modelling choice, not an independent citation** (Task 5's
    /// re-review, carried forward as its own step 3b): unlike
    /// [`DETRITAL_IMPORT_DRAINAGE_REACH`], which cites a measured p90
    /// drainage statistic, nothing independently measures where geothermal
    /// supply *should* cross half-yield — it borrows `Underdeep`'s boundary,
    /// which was built to classify habitability, not to calibrate
    /// geothermal yield. Task 5's measured per-rung energy profile (see
    /// `subterranean_energy_field_per_rung` and its probe) is what would
    /// revise it, if it disagrees.
    Geothermal,
    /// Surface-sourced organic and mineral material (rockfall, percolating
    /// detritus) that thins out with distance from the entrance — the
    /// registry row's missing shallow arm (controller Ruling P1; see the
    /// module doc for why this is not one of the row's six). Reads
    /// `drainage`, not `moisture` — see the module doc for why the earlier
    /// "double-counting" justification for that was overstated even though
    /// its conclusion holds. Yields nothing where no drainage reaches it,
    /// whatever the depth.
    DetritalImport,
}

/// Where one [`EnergySource`]'s yield lands in the food vocabulary — the
/// codomain of [`EnergySource::route`] (The Trencher, spec §4.2, ledger #2).
///
/// **Four metabolites, one correction and one non-food.** The four
/// metabolite variants name the `hornvale_kernel` axes The Trencher's Task 3
/// registered; [`SupplyRoute::Detritus`] is a *correction* rather than an
/// addition (its source was never one of the registry row's six chemical
/// mechanisms and its own doc already said so); [`SupplyRoute::Modifier`] is
/// the one route that supplies no axis at all.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SupplyRoute {
    /// Feeds `hornvale_kernel::HYDROGEN`. Two reactions route here and their
    /// yields are **summed**: two sources of the same molecule add, which is
    /// the whole of spec §4.3's argument at its smallest scale.
    Hydrogen,
    /// Feeds `hornvale_kernel::REDUCED_IRON`.
    ReducedIron,
    /// Feeds `hornvale_kernel::REDUCED_SULPHUR`.
    ReducedSulphur,
    /// Feeds `hornvale_kernel::METHANE`.
    Methane,
    /// Feeds the existing `hornvale_kernel::DETRITUS` axis — surface organic
    /// and mineral material percolating in, which is what `DETRITUS` already
    /// means at the surface. Routing it here is not a new axis and not a new
    /// meaning; it is the removal of a category error, since a pile of
    /// surface detritus was previously averaged into chemical food.
    Detritus,
    /// **Not food.** A thermal gradient is a condition that accelerates
    /// chemistry, not a substance an organism assimilates, so this route
    /// supplies no axis: its yield becomes a multiplier on the chemical
    /// metabolites instead (see [`chemical_supply`] for the form and the
    /// argument). Keeping the variant — rather than dropping the source from
    /// [`EnergySource::ALL`] — is deliberate: `dominant_source` still reports
    /// `Geothermal` as the term dominating a chamber, which is a true and
    /// useful readout even though nothing eats it.
    Modifier,
}

/// The lightless half of one point's supply vector: the `CHEMOSYNTHATE`
/// aggregate, the four metabolite axes it disaggregates into, and the
/// detrital import that routes to `DETRITUS`. Built by [`chemical_supply`],
/// consumed by the two per-vertex capacity loops in [`crate`].
///
/// **Why one struct rather than five returns.** The five numbers are derived
/// from one pass over [`EnergySource::ALL`] and are only meaningful together
/// — `chemosynthate` is the sum of the other four chemical fields, so
/// returning them separately would invite a caller to pair a fresh aggregate
/// with a stale breakdown. There is one derivation, not five that must be
/// kept in step.
/// type-audit: bare-ok(ratio: chemosynthate), bare-ok(ratio: hydrogen), bare-ok(ratio: reduced_iron), bare-ok(ratio: reduced_sulphur), bare-ok(ratio: methane), bare-ok(ratio: detritus)
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct ChemicalSupply {
    /// The `CHEMOSYNTHATE` aggregate — "chemical food", generically, for a
    /// generalist that names no metabolite. See [`chemical_supply`] for the
    /// aggregate rule and the double-counting consequence it carries.
    pub chemosynthate: f64,
    /// The `HYDROGEN` axis: serpentinization plus radiolysis.
    pub hydrogen: f64,
    /// The `REDUCED_IRON` axis: microbial/abiotic Fe(III) reduction.
    pub reduced_iron: f64,
    /// The `REDUCED_SULPHUR` axis: the sulphide-oxidation redox front.
    pub reduced_sulphur: f64,
    /// The `METHANE` axis: methanogenesis in porous carbonate.
    pub methane: f64,
    /// The `DETRITUS` contribution of [`EnergySource::DetritalImport`] — what
    /// this rung gets from *above*, **added to** (never replacing) the
    /// surface `DETRITUS` supply field the capacity loops already read.
    pub detritus: f64,
}

impl ChemicalSupply {
    /// Nothing supplied on any lightless axis — the reading for a vertex with
    /// no chamber at all, and the second argument of the capacity loops'
    /// cave-less fallback.
    ///
    /// plumb: universal(the additive identity of a supply reading rather than a tunable quantity -- every field is 0.0 by definition, and any other value would mean a vertex with no chamber supplied food)
    pub const NONE: Self = Self {
        chemosynthate: 0.0,
        hydrogen: 0.0,
        reduced_iron: 0.0,
        reduced_sulphur: 0.0,
        methane: 0.0,
        detritus: 0.0,
    };

    /// An aggregate with no breakdown: `CHEMOSYNTHATE` supplied, every
    /// metabolite axis zero.
    ///
    /// **This is the honest shape of a hydrothermal vent's surface supply**
    /// ([`crate::marine_chemosynthate_supply_field`]), and stating it as its
    /// own constructor is the point. The metabolite axes are an *underworld*
    /// disaggregation; nothing has yet measured which molecules a vent's
    /// plume actually carries, and a live peer campaign (The Tidemark) owns
    /// the marine half. A vent therefore supplies the aggregate and zero on
    /// every metabolite — not because its chemistry is undifferentiated, but
    /// because this campaign has not differentiated it.
    /// type-audit: bare-ok(ratio: chemosynthate)
    #[must_use]
    pub fn aggregate_only(chemosynthate: f64) -> Self {
        Self {
            chemosynthate,
            ..Self::NONE
        }
    }
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

    /// Where this reaction's yield goes in the food vocabulary (spec §4.2,
    /// ledger #2) — the routing table the module doc calls a judgment rather
    /// than a lookup. Total by construction: every variant routes somewhere,
    /// so adding an eighth source is a compile error here until its
    /// classification is made, which is the point.
    #[must_use]
    pub fn route(&self) -> SupplyRoute {
        match self {
            // Peridotite + water -> serpentine + H2.
            EnergySource::Serpentinization => SupplyRoute::Hydrogen,
            EnergySource::IronReduction => SupplyRoute::ReducedIron,
            // Radiolysis also yields H2. Two reactions, one molecule, one
            // axis — the spec's own worked example of why the table is not a
            // bijection.
            EnergySource::Radiolysis => SupplyRoute::Hydrogen,
            EnergySource::SulphideOxidation => SupplyRoute::ReducedSulphur,
            EnergySource::Methanogenesis => SupplyRoute::Methane,
            EnergySource::Geothermal => SupplyRoute::Modifier,
            EnergySource::DetritalImport => SupplyRoute::Detritus,
        }
    }

    /// This source's yield, `[0,1]`, given the chamber's material buffer,
    /// geothermal gradient, depth below the surface (m), a rung's moisture
    /// (`[0,1]`, Task 3's [`Substrate::moisture`] / `chamber_moisture`) and
    /// overhead drainage (`hornvale_terrain::GeneratedTerrain::drainage_at`
    /// at the vertex above, or any nonnegative value at this module's own
    /// test boundary). Only [`EnergySource::DetritalImport`] reads
    /// `drainage`; every other source gates on `moisture` in its own way
    /// (see the module doc).
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
                let delta_t = gradient.get() * depth_m.max(0.0) / 1000.0;
                let (underdeep_low, underdeep_top) = underdeep_delta_t_range();
                let r = (underdeep_low + underdeep_top) / 2.0;
                // 4r·ΔT/(r+ΔT)^2: an oxidant term falling as r/(r+ΔT) times a
                // sulphide term rising as ΔT/(r+ΔT). Peaks at ΔT=r with value
                // 1.0 (AM-GM: for fixed product r·ΔT, (r+ΔT)^2 is minimized
                // when r=ΔT), so this is already normalized to [0,1] before
                // scaling by metamorphic grade and the water gate. r is
                // Underdeep's ΔT midpoint (see the module doc, fix round 2).
                let front = 4.0 * r * delta_t / (r + delta_t).powi(2);
                buffer.metamorphic_grade
                    * front
                    * water_gate(moisture, SULPHIDE_OXIDATION_MOISTURE_SATURATE)
            }
            EnergySource::Methanogenesis => buffer.carbonate * buffer.porosity * moisture,
            EnergySource::Geothermal => {
                let temp_rise_k = depth_m.max(0.0) * gradient.get() / 1000.0;
                let (reach, _) = underdeep_delta_t_range();
                (temp_rise_k / (temp_rise_k + reach))
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

/// The whole lightless supply at one point: every named source's yield
/// ([`EnergySource::yield_at`]) routed through [`EnergySource::route`] and
/// summed **within** its metabolite, with [`EnergySource::Geothermal`]
/// applied as a multiplier rather than as food.
///
/// # Why the seven-way mean is gone, and why this sum is not the sum that
/// failed
///
/// **The old rule, preserved because it is the record of why the naive fix
/// failed.** A first cut summed all seven sources and clamped the total to
/// `[0,1]` (`EnvironmentVector::new` rejects a value outside that ruler).
/// Measured over the frozen seed set, that clamp pinned **every rung's
/// median to exactly `1.0`**: with moisture near-saturated at most chambers,
/// four to six of the seven sources (the depth/gradient/drainage-gated ones,
/// largely independent of which silica band a vertex's rock falls in) are
/// simultaneously non-trivial often enough that the raw sum blows past `1.0`
/// at the *median*, not just the tail. A clamp that fires at the median
/// erases whatever depth-shape the seven sources have, so that combination
/// rule would have been shaping the answer away, not measuring it. The mean
/// replaced it because a convex combination of seven `[0,1]` values lands in
/// `[0,1]` without ever needing to clamp.
///
/// **The mean was never wrong about saturation; it was wrong about
/// categories** (The Trencher, spec §2 and §4.2). Averaging five chemical
/// foods, one thermal gradient and one pile of surface detritus produces a
/// number whose units are nothing, and divides the answer by seven whatever
/// a chamber's chemistry actually is — a ceiling imposed by the combination
/// rule, which is what Task 5 measures.
///
/// **This sum is a narrower sum, and that is the whole reason it need not
/// saturate.** The failed sum ran across all seven sources. This one sums
/// **within a metabolite** — at most **two** reactions, and only for
/// `HYDROGEN` — and the two hydrogen reactions peak at *opposite* ends of
/// the felsic index (serpentinization at silica 0.05, radiolysis at 0.9,
/// half-width 0.35 each: disjoint bands), so even that pair is in practice
/// one term, not two. The two sources that made the old sum blow past `1.0`
/// at the median are exactly the two that leave the food vocabulary here:
/// `Geothermal` (near its own ceiling at every deep rung) and
/// `DetritalImport`. Nothing is clamped or truncated at any point.
///
/// **What is traded away, and it is MEASURED rather than hedged.** The mean
/// *guaranteed* `[0,1]`; this does not, and the overshoot is not hypothetical:
/// measured 2026-09-11 over seeds 42/7/1234 at `BuildDepth::Terrain`, the
/// per-rung medians of [`ChemicalSupply::chemosynthate`] are **1.004527 /
/// 1.316387 / 1.993109 / 2.094758 / 2.094758** (`Undercroft` → `Nadir`) —
/// projected onto the `ENERGY` ruler by [`subterranean_energy`]'s
/// `raw / (1 + raw)` transfer (below), that is **0.501 / 0.568 / 0.666 /
/// 0.677 / 0.677**, all between `E_FED` (0.5) and `E_RICH` (0.75) — so the
/// aggregate sits *above* the corpus's `E_TEEMING = 1.0` at the median of
/// every rung **as a raw magnitude**, which is exactly what a resource
/// magnitude is allowed to do; it is [`subterranean_energy`]'s job, not
/// this field's, to read that back onto the ruler. An earlier draft of this
/// paragraph said "can in principle exceed"; that was true and useless, and
/// the number replaces it.
///
/// **This is not the old saturation returning, and the distinction is the
/// measurement's own.** Each individual metabolite stays small at the median
/// (hydrogen 0.312→0.423, reduced iron 0.292→0.415, reduced sulphur
/// 0.053→0.556, methane 0.025 flat, same run), so the "at most two reactions"
/// argument above holds exactly where it was made — at the metabolite. What
/// leaves the ruler is the four-way AGGREGATE, which is arithmetic rather than
/// clamping: four terms near 0.4 sum to 1.6 however narrow each is.
///
/// **Decided (The Trencher, ledger #24), and the paragraph that used to defer
/// this to "Task 5" is corrected rather than merely updated, because the
/// deferred option it named was never actually on the table.** Rescaling the
/// `ENERGY` ruler's corpus bands cannot be expressed: the corpus is authored
/// through `hornvale_kernel::EnvironmentVector`, whose constructor rejects
/// any value outside `[0, 1]`, so the bands cannot move without breaking a
/// kernel invariant. Lowering `GEOTHERMAL_MODIFIER_GAIN` was also refused —
/// `1 + gain * g` is accepted as-is. What actually ships is neither: this
/// field (the raw sum, a resource magnitude) is left untouched, and
/// [`subterranean_energy`] — the ruler readout, not the magnitude — applies
/// its own saturating transfer on the way out. See that function's doc for
/// the mechanism and the projected medians above.
///
/// Nothing in this tree constructs an `EnvironmentVector` directly from
/// *this* raw field, so its own overshoot panics nothing; the value that
/// reaches a chamber's fit and every other ruler consumer is
/// [`subterranean_energy`]'s bounded projection, not this one.
/// `subterranean_energy_probe.rs`'s historical note that the derived field
/// reports a realized max above `1.0` describes this raw magnitude, still
/// true of it and unaffected by the ruler fix.
///
/// # `Geothermal`'s modifier form: `1 + gain * g`
///
/// A gradient is a *rate* condition — hotter rock runs the same chemistry
/// faster (Arrhenius) — so the form is multiplicative on the chemical
/// metabolites, and applies to them only. Three alternatives were considered
/// and each fails on a stated ground:
///
/// - **A bare multiply (`* g`)** deletes all shallow chemistry: `g` is near
///   zero at the surface datum, and serpentinization runs at ambient
///   temperature. That over-corrects a category error into a physical
///   falsehood.
/// - **An additive term (`+ g`)** supplies food where there is no reaction to
///   modify: a hot, chemically barren chamber would feed a hydrogen eater.
///   That is the *same* category error this campaign removes, wearing a
///   different sign. Multiplicative is exactly zero where the chemistry is
///   zero.
/// - **An unbounded multiplier** would let the modifier become the dominant
///   signal and re-impose a depth-only shape on a field whose point is
///   lithological variation. `[1, 2]` is strong but bounded.
///
/// It does **not** multiply `DetritalImport`: detritus falls in from above
/// and the rock's heat does not make more of it.
///
/// # The `CHEMOSYNTHATE` aggregate rule
///
/// [`ChemicalSupply::chemosynthate`] is the **sum of the four metabolites**.
/// `CHEMOSYNTHATE` survives as an aggregate (ledger #1) and the kernel's own
/// `HYDROGEN` doc settles what the aggregate *is*: the metabolite axes are
/// "a disaggregation of `CHEMOSYNTHATE` itself". A generalist eating chemical
/// food indiscriminately therefore receives exactly what four specialists
/// would collectively receive — which is the only rule under which the
/// aggregate and the breakdown describe one world rather than two.
///
/// **The consequence, stated because it is an authoring rule and not an
/// arithmetic error:** a niche weighting *both* `CHEMOSYNTHATE` and a
/// metabolite double-counts that metabolite. Author one or the other. The
/// alternative rules were worse: a *mean* of the four would make the
/// generalist strictly poorer than any specialist for no physical reason,
/// and a *max* would make it exactly as rich as the best specialist while
/// eating everything, which is richer than the sum only when one metabolite
/// dominates and poorer otherwise — an incoherent creature either way.
/// type-audit: bare-ok(diagnostic-value: depth_m), bare-ok(ratio: moisture), bare-ok(diagnostic-value: drainage)
pub fn chemical_supply(
    material: &MaterialBuffer,
    gradient: GeothermalGradient,
    depth_m: f64,
    moisture: f64,
    drainage: f64,
) -> ChemicalSupply {
    let mut out = ChemicalSupply::NONE;
    let mut gradient_yield = 0.0;
    // `EnergySource::ALL`'s order is the summation order, so the float
    // accumulation is fixed by a constant rather than by iteration luck.
    for source in EnergySource::ALL {
        let y = source.yield_at(material, gradient, depth_m, moisture, drainage);
        match source.route() {
            SupplyRoute::Hydrogen => out.hydrogen += y,
            SupplyRoute::ReducedIron => out.reduced_iron += y,
            SupplyRoute::ReducedSulphur => out.reduced_sulphur += y,
            SupplyRoute::Methane => out.methane += y,
            SupplyRoute::Detritus => out.detritus += y,
            SupplyRoute::Modifier => gradient_yield += y,
        }
    }
    let modifier = 1.0 + GEOTHERMAL_MODIFIER_GAIN * gradient_yield;
    out.hydrogen *= modifier;
    out.reduced_iron *= modifier;
    out.reduced_sulphur *= modifier;
    out.methane *= modifier;
    out.chemosynthate = out.hydrogen + out.reduced_iron + out.reduced_sulphur + out.methane;
    out
}

/// The `ENERGY` reading at one point: a Type-II (`raw / (1 + raw)`) transfer
/// of [`chemical_supply`]'s `CHEMOSYNTHATE` aggregate — the ruler readout,
/// not the raw resource magnitude.
///
/// Kept as a named scalar because that is what every readout consumer wants
/// (`crate::marine_chemosynthate_supply_field`, the vessel's chamber
/// conditions, and the calibration probes that compare this field against
/// `domains/climate/src/underworld.rs`'s authored `E_INERT`..`E_TEEMING`
/// ladder). It is a *projection* of [`chemical_supply`], never a second
/// derivation: change the combination rule there and every reader of this
/// moves with it.
///
/// # Why this saturates and `ChemicalSupply::chemosynthate` does not (The
/// Trencher, ledger #24, deciding Task 4's open calibration question)
///
/// `CHEMOSYNTHATE` and `ENERGY` are two different things wearing one name
/// until this function: the former is a resource *magnitude* (unbounded,
/// summed from four metabolites — see [`chemical_supply`]'s doc for why
/// their sum legitimately exceeds `1.0`), the latter is the **ruler**
/// compared against the authored corpus bands (`E_INERT`..`E_TEEMING`) that
/// live inside `hornvale_kernel::EnvironmentVector`'s `[0, 1]` contract
/// (`EnvironmentVector::new` rejects anything outside it). Rescaling the
/// bands to fit the magnitude — the option this module's doc used to defer
/// to "Task 5" — is not expressible: the corpus is authored *through* that
/// contracted type, so moving the bands breaks a kernel invariant rather
/// than a convention. Lowering `GEOTHERMAL_MODIFIER_GAIN`, or changing the
/// `CHEMOSYNTHATE` aggregate rule, were also considered and refused (ledger
/// #24) — neither is this function's job.
///
/// So the fix un-collapses the two: [`chemical_supply`]'s `chemosynthate`
/// field stays the raw sum (every other `per_axis` entry the capacity loops
/// build is an unbounded magnitude that saturates itself,
/// `supply / (1.0 + supply)`, in `crate::per_species_capacity_at`'s own
/// `score_at` — bounding it a second time here would saturate a generalist
/// TWICE while a metabolite specialist saturates once), and this function
/// applies that same saturating transfer once, on the way out, for the
/// ruler alone. It is not an arbitrary rescale or a "divide by four": it is
/// the identical form already used one call away for exactly the same
/// reason.
///
/// Projected medians (`raw / (1 + raw)` on the raw medians `chemical_supply`'s
/// doc records), `Undercroft`→`Nadir`: **0.501 / 0.568 / 0.666 / 0.677 /
/// 0.677** — all landing between `E_FED` (0.5) and `E_RICH` (0.75), so
/// ordering and contrast survive and nothing saturates flat the way the raw
/// sum did against `windows/vessel/src/underground.rs`'s `inhabitant_fit`
/// and its `energy.clamp(0.0, 1.0)`.
/// type-audit: bare-ok(diagnostic-value: depth_m), bare-ok(ratio: moisture), bare-ok(diagnostic-value: drainage), bare-ok(ratio: return)
pub fn subterranean_energy(
    material: &MaterialBuffer,
    gradient: GeothermalGradient,
    depth_m: f64,
    moisture: f64,
    drainage: f64,
) -> f64 {
    let raw = chemical_supply(material, gradient, depth_m, moisture, drainage).chemosynthate;
    raw / (1.0 + raw)
}

/// Which source contributes the most yield at one point — the scalar this
/// module retains beside [`subterranean_energy`]'s single ruler value, so
/// the seven sources' *differences* (which `BIO-subterranean-energy-sources`
/// says motivate ecology, trade, exploration and mining) survive the sum
/// rather than being discarded by it. `MARINE_FORAGE`'s own precedent,
/// applied deliberately: one axis, one calibration knob, with the
/// underlying distinction retained and retrievable beside it rather than
/// thrown away.
///
/// On an exact tie, favours the source listed **later** in
/// [`EnergySource::ALL`] — `Iterator::max_by`'s documented behaviour ("if
/// several elements are equally maximum, the last element is returned").
/// type-audit: bare-ok(diagnostic-value: depth_m), bare-ok(ratio: moisture), bare-ok(diagnostic-value: drainage)
pub fn dominant_source(
    material: &MaterialBuffer,
    gradient: GeothermalGradient,
    depth_m: f64,
    moisture: f64,
    drainage: f64,
) -> EnergySource {
    EnergySource::ALL
        .iter()
        .copied()
        .max_by(|a, b| {
            a.yield_at(material, gradient, depth_m, moisture, drainage)
                .total_cmp(&b.yield_at(material, gradient, depth_m, moisture, drainage))
        })
        .expect("EnergySource::ALL is non-empty")
}

/// [`subterranean_energy`] over every band of the ladder and every vertex of
/// the globe, indexed by `Band as usize` ([`Band::all`]'s own order:
/// `Surface` through `Nadir`, six entries) — the field the module doc's
/// opening line names as Task 5's job ("Task 5 consumes
/// [`EnergySource::ALL`] for that").
///
/// A vertex with no cave gets `[None; 6]`; a cave-bearing vertex's
/// [`Band::Surface`] slot is always `None` ([`rung_evaluation_depth_m`]
/// returns `None` there — it names no chamber, same as
/// [`crate::subterranean_substrate_field_per_rung`]'s own `Surface` slot).
///
/// **Derived exactly as [`crate::subterranean_substrate_field_per_rung`]
/// derives its own per-rung reading, because it reads that same reading
/// rather than re-deriving it**: `subterranean_per_rung` is the field
/// [`crate::subterranean_substrate_field_per_rung`] already built at this
/// call's two production sites, so this field cannot disagree with that one
/// about which rungs a chamber has, or read a different depth or moisture at
/// one it does — there is only one derivation, not two that must be kept in
/// step. Same [`rung_evaluation_depth_m`] call for the evaluation depth.
/// `drainage` is read once per vertex ([`GeneratedTerrain::drainage_at`])
/// rather than per rung, because it is [`EnergySource::DetritalImport`]'s
/// only input and does not vary with depth the way moisture does.
///
/// **This is where the module doc's "nothing here was shaped to produce a
/// U" caveat becomes concrete, not merely theoretical.** `moisture` here is
/// [`crate::Substrate::moisture`], itself depth-dependent via
/// `chamber_moisture` (saturated below the water table, drying above it) —
/// so the six moisture-gated sources inherit an *indirect* depth-dependence
/// at exactly this boundary, even though [`EnergySource::yield_at`] itself
/// never reads depth for any water gate.
/// type-audit: bare-ok(ratio: return)
pub fn subterranean_energy_field_per_rung(
    geo: &Geosphere,
    terrain: &GeneratedTerrain,
    subterranean_per_rung: &VertexMap<[Option<crate::Substrate>; 6]>,
) -> VertexMap<[Option<f64>; 6]> {
    VertexMap::from_fn(geo, |vertex| {
        let mut out = [None; 6];
        let Some(cave) = terrain.cave_at(vertex) else {
            return out;
        };
        let gradient = terrain.geothermal_gradient_at(vertex);
        let material = terrain.material_at(vertex);
        let drainage = terrain.drainage_at(vertex);
        let sub_per_rung = subterranean_per_rung.get(vertex);
        for &rung in Band::all() {
            let Some(depth_m) = rung_evaluation_depth_m(rung, gradient, cave.depth_reach_m) else {
                continue;
            };
            let Some(sub) = sub_per_rung[rung as usize] else {
                continue;
            };
            out[rung as usize] = Some(subterranean_energy(
                &material,
                gradient,
                depth_m,
                sub.moisture,
                drainage,
            ));
        }
        out
    })
}

/// [`chemical_supply`] over every band of the ladder and every vertex of the
/// globe — the field the two per-vertex capacity loops in [`crate`] read, and
/// the per-rung sibling of [`subterranean_energy_field_per_rung`].
///
/// Same shape, same gate and same derivation discipline as
/// [`subterranean_energy_field_per_rung`]: see that function's doc for why
/// `subterranean_per_rung` is read rather than re-derived, why a cave-less
/// vertex is `[None; 6]`, why the `Surface` slot is always `None`, and why
/// `drainage` is read once per vertex. The two are deliberately *parallel*
/// rather than one calling the other per rung: the capacity loops want the
/// whole [`ChemicalSupply`] and the probes want the scalar, and
/// [`subterranean_energy`] is already a projection of [`chemical_supply`], so
/// neither field can disagree with the other about any point.
/// type-audit: bare-ok(ratio: return)
pub fn chemical_supply_field_per_rung(
    geo: &Geosphere,
    terrain: &GeneratedTerrain,
    subterranean_per_rung: &VertexMap<[Option<crate::Substrate>; 6]>,
) -> VertexMap<[Option<ChemicalSupply>; 6]> {
    VertexMap::from_fn(geo, |vertex| {
        let mut out = [None; 6];
        let Some(cave) = terrain.cave_at(vertex) else {
            return out;
        };
        let gradient = terrain.geothermal_gradient_at(vertex);
        let material = terrain.material_at(vertex);
        let drainage = terrain.drainage_at(vertex);
        let sub_per_rung = subterranean_per_rung.get(vertex);
        for &rung in Band::all() {
            let Some(depth_m) = rung_evaluation_depth_m(rung, gradient, cave.depth_reach_m) else {
                continue;
            };
            let Some(sub) = sub_per_rung[rung as usize] else {
                continue;
            };
            out[rung as usize] = Some(chemical_supply(
                &material,
                gradient,
                depth_m,
                sub.moisture,
                drainage,
            ));
        }
        out
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_terrain::{Basement, MarginPolarity, SoilDepth, rung_at_delta_t};

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
        // term already reading zero, not the water gate). SulphideOxidation's
        // depth is the metre depth that reaches Underdeep's ΔT midpoint at
        // this test's 25 K/km gradient, read live rather than duplicated so
        // it cannot drift from the real peak (fix round 2).
        let sulphide_oxidation_depth_m = {
            let (low, high) = underdeep_delta_t_range();
            (low + high) / 2.0 * 1000.0 / 25.0
        };
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
                sulphide_oxidation_depth_m,
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
        // unaffected by fix round 1's gating. Fix round 2 moved the front from
        // metres to ΔT; at this test's fixed 25 K/km this grid still brackets
        // the peak (ΔT=37.5 lands exactly on depth=1500m, i=15 of the 0..=2000
        // sweep), so the interior-peak property still holds — see
        // sulphide_oxidation_peaks_in_underdeep_across_the_legal_gradient_range
        // below for the rung the peak lands in, which is the property that
        // actually broke.
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

    #[test]
    fn sulphide_oxidation_peaks_in_underdeep_across_the_legal_gradient_range() {
        // Fix round 2, Critical: the controller found the original metre-fixed
        // front peaked in Band::Deeps at every legal gradient — exactly the
        // rung the frozen corpus (sump-gallery, E_INERT) calls the trough the
        // inversion turns on, one rung shallower than where the corpus places
        // sulphide oxidation's own richness (sulphuric-hall, Underdeep,
        // E_RICH, "richer than anything at Deeps"). This pins the RUNG the
        // front peaks in — the property the corpus actually constrains and
        // the one that broke silently — not a metre value that only holds at
        // the gradients someone happened to test.
        let m = buffer(0.5, 0.5, 0.5, 0.8);
        for gradient_k_per_km in [15.0, 22.5, 30.0] {
            let grad = GeothermalGradient::new(gradient_k_per_km);
            let at = |d: f64| EnergySource::SulphideOxidation.yield_at(&m, grad, d, 0.5, 0.0);
            // 0..=3000 m: hornvale_terrain::CAVE_REACH_CEILING_M, the campaign's
            // own ceiling on how deep a cave (and so this evaluation) reaches.
            let depths: Vec<f64> = (0..=300).map(|i| i as f64 * 10.0).collect();
            let best_depth = depths
                .iter()
                .copied()
                .max_by(|a, b| at(*a).total_cmp(&at(*b)))
                .expect("a non-empty sweep");
            let delta_t = gradient_k_per_km * best_depth / 1000.0;
            let rung = rung_at_delta_t(delta_t);
            assert_eq!(
                rung,
                Band::Underdeep,
                "at {gradient_k_per_km} K/km sulphide oxidation peaked at {best_depth} m \
                 (ΔT={delta_t}), rung {rung:?} — expected Underdeep, the rung the frozen \
                 corpus (sulphuric-hall, E_RICH) names richer than Deeps (sump-gallery, \
                 E_INERT, the trough)"
            );
        }
    }

    /// claim: invariant — every reaction routes, and the two hydrogen
    /// reactions share one axis.
    ///
    /// The routing table is the campaign's judgment (spec §4.2), so it is
    /// pinned rather than left to the reader of a `match`. The load-bearing
    /// row is `Hydrogen`'s **two** members: a table that is a bijection with
    /// `EnergySource::ALL` would be the reaction vocabulary wearing a
    /// metabolite's name.
    #[test]
    fn the_routing_table_is_not_a_bijection() {
        let routed = |r: SupplyRoute| EnergySource::ALL.iter().filter(|s| s.route() == r).count();
        assert_eq!(
            routed(SupplyRoute::Hydrogen),
            2,
            "serpentinization and radiolysis both yield H2 and must share one axis"
        );
        for route in [
            SupplyRoute::ReducedIron,
            SupplyRoute::ReducedSulphur,
            SupplyRoute::Methane,
            SupplyRoute::Detritus,
            SupplyRoute::Modifier,
        ] {
            assert_eq!(routed(route), 1, "{route:?} must have exactly one source");
        }
    }

    /// claim: invariant — `chemical_supply` is the routed, per-metabolite sum
    /// of `yield_at`, to the bit.
    ///
    /// Recomputed from the public `yield_at` in the routing table's own order
    /// rather than from `chemical_supply`'s internals, so a mis-routed source
    /// or a metabolite summed with the wrong partner fails here.
    #[test]
    fn chemical_supply_sums_within_a_metabolite() {
        let m = buffer(0.5, 0.4, 0.3, 0.6);
        let g = GeothermalGradient::new(25.0);
        let (depth, moisture, drainage) = (900.0, 0.6, 12.0);
        let y = |s: EnergySource| s.yield_at(&m, g, depth, moisture, drainage);
        let modifier = 1.0 + GEOTHERMAL_MODIFIER_GAIN * y(EnergySource::Geothermal);
        let cs = chemical_supply(&m, g, depth, moisture, drainage);

        assert_eq!(
            cs.hydrogen.to_bits(),
            ((y(EnergySource::Serpentinization) + y(EnergySource::Radiolysis)) * modifier)
                .to_bits(),
            "HYDROGEN is serpentinization + radiolysis, modified"
        );
        assert_eq!(
            cs.reduced_iron.to_bits(),
            (y(EnergySource::IronReduction) * modifier).to_bits()
        );
        assert_eq!(
            cs.reduced_sulphur.to_bits(),
            (y(EnergySource::SulphideOxidation) * modifier).to_bits()
        );
        assert_eq!(
            cs.methane.to_bits(),
            (y(EnergySource::Methanogenesis) * modifier).to_bits()
        );
        // Detritus is NOT modified: surface matter falls in from above and the
        // rock's heat does not make more of it.
        assert_eq!(
            cs.detritus.to_bits(),
            y(EnergySource::DetritalImport).to_bits(),
            "DetritalImport routes to DETRITUS unmodified"
        );
        assert_eq!(
            cs.chemosynthate.to_bits(),
            (cs.hydrogen + cs.reduced_iron + cs.reduced_sulphur + cs.methane).to_bits(),
            "the CHEMOSYNTHATE aggregate is the sum of the four metabolites"
        );
        assert_eq!(
            subterranean_energy(&m, g, depth, moisture, drainage).to_bits(),
            (cs.chemosynthate / (1.0 + cs.chemosynthate)).to_bits(),
            "the ENERGY scalar must be the ruler's raw/(1+raw) projection of \
             chemical_supply's CHEMOSYNTHATE, never a second derivation (The \
             Trencher, ledger #24)"
        );
    }

    /// claim: invariant — the gradient MULTIPLIES live chemistry and never
    /// CREATES a metabolite.
    ///
    /// This is the test that discriminates `Geothermal`'s chosen modifier form
    /// (`1 + gain * g`) from the two alternatives `chemical_supply`'s doc
    /// rejects. Silica `0.5` sits outside **both** hydrogen bands
    /// (serpentinization centres at 0.05, radiolysis at 0.9, half-width 0.35
    /// each) and inside iron reduction's (centre 0.45), so this one fixture
    /// carries a dead axis and a live one at the same point: an ADDITIVE
    /// modifier would feed a hydrogen eater out of bare heat, and NO modifier
    /// would leave the iron term unmoved.
    #[test]
    fn the_gradient_multiplies_chemistry_and_never_creates_it() {
        let m = buffer(0.5, 0.0, 0.0, 0.0);
        let g = GeothermalGradient::new(25.0);
        let (depth, moisture, drainage) = (2000.0, 0.9, 0.0);
        let gradient_yield = EnergySource::Geothermal.yield_at(&m, g, depth, moisture, drainage);
        assert!(
            gradient_yield > 0.3,
            "fixture is vacuous: the gradient term reads {gradient_yield}, too small \
             to tell the three candidate forms apart"
        );
        let raw_iron = EnergySource::IronReduction.yield_at(&m, g, depth, moisture, drainage);
        assert!(
            raw_iron > 0.0,
            "fixture is vacuous: no live reaction to modify"
        );

        let cs = chemical_supply(&m, g, depth, moisture, drainage);
        assert_eq!(
            cs.hydrogen, 0.0,
            "heat must not CREATE hydrogen where neither hydrogen reaction runs"
        );
        assert!(
            cs.reduced_iron > raw_iron,
            "the gradient must multiply a live reaction: {} is not above {raw_iron}",
            cs.reduced_iron
        );
        assert_eq!(
            cs.reduced_iron.to_bits(),
            (raw_iron * (1.0 + GEOTHERMAL_MODIFIER_GAIN * gradient_yield)).to_bits()
        );
    }

    /// claim: invariant — `ChemicalSupply::aggregate_only` supplies the
    /// aggregate and nothing else.
    ///
    /// A hydrothermal vent's surface supply takes this shape, and the zeros
    /// are the assertion: this campaign disaggregates the UNDERWORLD, and
    /// silently handing a vent a metabolite breakdown it has not measured
    /// would be an invented fact.
    #[test]
    fn an_aggregate_only_reading_supplies_no_metabolite() {
        let cs = ChemicalSupply::aggregate_only(0.75);
        assert_eq!(cs.chemosynthate, 0.75);
        assert_eq!(cs.hydrogen, 0.0);
        assert_eq!(cs.reduced_iron, 0.0);
        assert_eq!(cs.reduced_sulphur, 0.0);
        assert_eq!(cs.methane, 0.0);
        assert_eq!(cs.detritus, 0.0);
    }
}
