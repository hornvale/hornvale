//! Where in the water column a marine people lives — the pelagic ladder's
//! half of the realm gate (The Tidemark, Task 2; spec §3.2 and §3.3).
//!
//! This is [`crate::subterranean_substrate_field_per_rung`]'s marine sibling,
//! and deliberately the same construct rather than a parallel one:
//! `Stratum`'s own doc already asserts that "the pelagic zones and (later)
//! the underworld's geological layers are the same construct at different
//! realms". A `Subterranean` kind is scored at every rung of
//! [`hornvale_kernel::Band::habitation`] and takes the best; a `Marine` kind
//! is scored at every band of [`Realm::WATERWORLD`]'s `strata()` and takes
//! the best. The availability mask follows from the same loop in both:
//! `1.0` where some rung (some stratum) scored, `0.0` where none did.
//!
//! # Two readings, and why both exist
//!
//! [`MarineHabitat::ambient`] is **pure** — terrain and climate only, no
//! stream and no draw — and is what the readout path
//! ([`crate::per_species_suitability_masked`]) builds for itself, hoisted
//! unconditionally and read only by a `Marine` kind, exactly as
//! `subterranean_substrate_field` is.
//!
//! [`MarineHabitat::at_instant`] is the same reading with the Waterworld
//! overlay's **vents** applied at one named instant. A vent's contribution is
//! a property of a moment — [`crate::waterworld::WaterWorld::at`] is where
//! temperature delta and chemistry are folded into a sample — so naming the
//! instant is unavoidable and is done at the call, not here. This is the
//! reading placement takes, hoisted once per bake on
//! [`crate::EraInvariantSupply`].
//!
//! Both route through one private `from_samples`, over the one column walk
//! [`crate::waterworld::marine_columns`] owns, so the two cannot disagree
//! about which strata a column has or what depth a sample sits at.
//!
//! # WHAT ACTUALLY VARIES BY BAND — read this before authoring a marine kind
//!
//! "Scored at every band and takes the best" is true, and it is a narrower
//! instrument than it sounds. Verified in the tree, not inferred, and
//! recorded in spec §3.4:
//!
//! - **`temperature_c` does not vary by band.**
//!   [`crate::waterworld::WaterFields::from_substrate`] is handed one
//!   `climate.temperature_at(sample.vertex, time)` for EVERY sample of a
//!   vertex, so the whole column is one temperature.
//! - **`chemosynthate` does not vary by band either, except at a vent's
//!   seabed sample.** The ambient value is `has_edifice ? 1.0 : 0.0`, a
//!   per-vertex terrain property, and [`WaterWorld::at`] folds a vent's
//!   chemistry in at `seabed_sample_index` alone.
//! - **`insolation` varies by band and NOTHING READS IT.** It is populated
//!   from `field.light` below; the capacity path's
//!   `tolerance_liebig_with_fixed` takes moisture and insolation from
//!   `EraInvariantTolerance`, which has no band dimension at all.
//! - **`moisture` is the constant [`MARINE_MOISTURE`].**
//!
//! So **the five strata are distinguished by depth alone**, through
//! `height_asl_m`, and four of the five depths are the global constants
//! `{0, 200, 1000, 4000, 6000}` m — identical at every ocean vertex. Only
//! the seabed band's depth is a per-vertex quantity.
//!
//! **Do not "fix" this by threading light through.** The light that reaches
//! the readout is `climate.insolation()` — a world *scalar* — attenuated by
//! `exp(-depth/1000)`. It carries no latitude, so it is a deterministic
//! function of depth and would duplicate the axis the ladder already has.
//! The gap is not that light is missing; it is that nothing in the pelagic
//! column varies per vertex except the seabed depth.
//!
//! # Which vent representation this reads
//!
//! `WaterVent`, not `hornvale_climate::Biome::HydrothermalVent`. M1 measured
//! the two at seeds 42, 7 and 3 and found them separate phenomena (the
//! intersection is 25–30% of the smaller set, against a preregistered
//! "under half"); `WaterVent` is the only one of the two carrying a
//! succession phase, which is what §4's expiring habitat needs. The biome
//! remains the coarser, ridge-derived thing the `Surface` arm already reads
//! through [`crate::marine_chemosynthate_supply_field`]; nothing here
//! replaces it.

use hornvale_climate::{GeneratedClimate, Realm, Stratum};
use hornvale_kernel::{Geosphere, SeaLevelHeight, VertexMap, WorldTime};
use hornvale_terrain::GeneratedTerrain;

use crate::Substrate;
use crate::waterworld::{
    WaterFields, WaterStocks, WaterSubstrate, WaterWorld, ambient_marine_fields,
    ambient_marine_stocks, marine_columns,
};

/// How many bands the pelagic column has — the length of
/// [`Realm::WATERWORLD`]'s `strata()`, needed as a `const` because it sizes
/// the per-vertex array below. `the_pelagic_array_matches_the_realm` pins the
/// two together, so this cannot silently drift from the realm's own roster.
/// plumb: universal(the water column's band count is a property of the realm, not of a world)
/// type-audit: bare-ok(count: PELAGIC_BANDS)
pub const PELAGIC_BANDS: usize = 5;

/// Moisture at a submerged sample.
///
/// `1.0` is a definition rather than a calibration: a sample inside an open
/// water column is saturated, and this is the same ceiling
/// `VENT_MOISTURE` cites for the open sea floor — the value every water gate
/// in [`crate::energy::EnergySource`] is built to saturate at, which a real
/// marine moisture, however finely a future model computed it, could not
/// exceed.
/// plumb: universal(saturation is a property of being underwater, not of a world)
const MARINE_MOISTURE: f64 = 1.0;

/// This stratum's index into a [`MarineHabitat`] array, or `None` for a
/// stratum that is not part of the water column (`Surface`, `Rock(_)`).
///
/// Resolved against [`Realm::WATERWORLD`]'s own `strata()` rather than
/// against a second hand-written order, so the array index and the realm's
/// roster are one fact.
/// type-audit: bare-ok(index: return)
#[must_use]
pub fn pelagic_index(stratum: Stratum) -> Option<usize> {
    Realm::WATERWORLD
        .strata()
        .iter()
        .position(|&band| band == stratum)
}

/// The marine reading of every vertex, hoisted exactly as the subterranean
/// one is: built unconditionally, read only by a `Marine` kind.
///
/// Both maps are indexed by [`pelagic_index`] — `Epipelagic` first,
/// `Hadal` last. A vertex with no water column reads `[None; 5]` /
/// `[0.0; 5]`, which is what makes the availability mask fall out of the
/// scoring loop rather than needing a second, separately-derived flag.
///
/// **Carrying a value per band is not the same as that value varying by
/// band** — see the module doc's "what actually varies" section before
/// reading a difference into these arrays. Today only `height_asl_m` does,
/// and four of its five values are global constants.
/// type-audit: bare-ok(ratio: chemosynthate)
#[derive(Clone, Debug)]
pub struct MarineHabitat {
    /// The substrate at each pelagic band, `None` where the column does not
    /// reach that band (every band below the seabed, and every band at a
    /// vertex that is not ocean).
    ///
    /// Of its four fields, only `height_asl_m` differs between the `Some`
    /// bands of one vertex: `temperature_c` is the vertex's single column
    /// temperature, `moisture` is a constant, and `insolation` is populated
    /// and read by nothing. Module doc, "what actually varies by band".
    pub substrate: VertexMap<[Option<Substrate>; PELAGIC_BANDS]>,
    /// The `CHEMOSYNTHATE` supply at each pelagic band, on the same `[0, 1]`
    /// ruler [`crate::energy::subterranean_energy`] returns — so the marine
    /// and subterranean chemotrophic supplies are comparable rather than two
    /// scales sharing one axis name. `0.0` where the column does not reach
    /// the band.
    ///
    /// **Per band in shape, per vertex in fact, with one exception.** The
    /// ambient value is `has_edifice ? 1.0 : 0.0` — a terrain property of the
    /// vertex, identical down the column — and [`WaterWorld::at`] adds a
    /// vent's chemistry at `seabed_sample_index` alone. So the only band that
    /// can differ from its neighbours is the seabed band at a vertex a live
    /// vent is lighting.
    pub chemosynthate: VertexMap<[f64; PELAGIC_BANDS]>,
}

impl MarineHabitat {
    /// The ambient marine reading: terrain and climate only, **no vents**.
    ///
    /// Pure — no stream, no draw, one map — which is what lets
    /// [`crate::per_species_suitability_masked`] hoist it unconditionally the
    /// way it hoists `subterranean_substrate_field`. A caller holding a seed
    /// wants [`MarineHabitat::at_instant`] instead; this one is the honest
    /// reading for a caller that has no overlay to read.
    #[must_use]
    pub fn ambient(
        geo: &Geosphere,
        terrain: &GeneratedTerrain,
        climate: &GeneratedClimate,
    ) -> Self {
        let substrate = marine_columns(terrain, climate);
        let fields = ambient_marine_fields(climate, &substrate);
        let stocks = ambient_marine_stocks(&substrate, &fields);
        Self::from_samples(geo, &substrate, &fields, &stocks)
    }

    /// The marine reading with `water`'s vents applied at `time`.
    ///
    /// **The instant is the caller's to name**, and it is a determinism
    /// decision rather than a detail: vent succession is a function of
    /// `WorldTime` ([`crate::waterworld::VentState`]), so two callers reading
    /// different instants would score the same world two ways. The read
    /// itself consumes no draw — [`WaterWorld::at`]'s own doc says so, and
    /// `the_marine_habitat_read_consumes_no_draw` checks it rather than
    /// trusting it.
    ///
    /// At [`WorldTime::GENESIS`] over an overlay with no vents this is
    /// bit-identical to [`MarineHabitat::ambient`]: the overlay's own ambient
    /// fields are derived at genesis, and `WaterWorld::at` recomputes the
    /// same fields before folding any vent into them.
    #[must_use]
    pub fn at_instant(
        geo: &Geosphere,
        climate: &GeneratedClimate,
        water: &WaterWorld,
        time: WorldTime,
    ) -> Self {
        let snapshot = water.at(climate, time);
        Self::from_samples(geo, &water.substrate, &snapshot.fields, &snapshot.stocks)
    }

    /// Scatter one aligned `(substrate, fields, stocks)` triple — the shape
    /// both [`WaterWorld`] and a snapshot of it carry — into the per-vertex,
    /// per-band arrays the scoring loops read.
    fn from_samples(
        geo: &Geosphere,
        substrate: &[WaterSubstrate],
        fields: &[WaterFields],
        stocks: &[WaterStocks],
    ) -> Self {
        assert_eq!(
            substrate.len(),
            fields.len(),
            "a marine substrate column and its fields must stay aligned"
        );
        assert_eq!(
            substrate.len(),
            stocks.len(),
            "a marine substrate column and its stocks must stay aligned"
        );
        let count = geo.vertex_count();
        let mut per_band_substrate = vec![[None; PELAGIC_BANDS]; count];
        let mut per_band_chemosynthate = vec![[0.0_f64; PELAGIC_BANDS]; count];
        for ((sample, field), stock) in substrate.iter().zip(fields).zip(stocks) {
            let Some(band) = pelagic_index(sample.depth_band) else {
                continue;
            };
            let vertex = sample.vertex.0 as usize;
            per_band_substrate[vertex][band] = Some(Substrate {
                temperature_c: field.temperature_c,
                moisture: MARINE_MOISTURE,
                // The pelagic light ladder — POPULATED AND READ BY NOTHING,
                // stated here so the next reader does not infer a consumer
                // from a populated field. `WaterFields::light` is the world's
                // insolation attenuated by this sample's depth, which is the
                // same quantity the surface substrate's `insolation` carries
                // at the surface, so it is the right value to put here. But
                // the capacity path takes its insolation term from
                // `EraInvariantTolerance`, which is per species per VERTEX
                // with no band dimension, and the readout path's insolation
                // is likewise the surface reading. Kept rather than dropped
                // because the field is `Substrate`'s and must hold something
                // true; see the module doc for why threading it through would
                // add no axis.
                insolation: field.light,
                // A sample in the water column sits BELOW sea level, so its
                // height on that datum is the negation of its depth. This is
                // the one place the two coordinates meet; `Substrate`'s own
                // doc already says the field is negative on ocean vertices.
                height_asl_m: SeaLevelHeight::from_metres(-field.depth_m),
            });
            per_band_chemosynthate[vertex][band] = stock.chemosynthetic_bloom;
        }
        MarineHabitat {
            substrate: VertexMap::from_fn(geo, |vertex| per_band_substrate[vertex.0 as usize]),
            chemosynthate: VertexMap::from_fn(geo, |vertex| {
                per_band_chemosynthate[vertex.0 as usize]
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_pelagic_array_matches_the_realm() {
        assert_eq!(
            PELAGIC_BANDS,
            Realm::WATERWORLD.strata().len(),
            "the per-vertex array must have one slot per band of the realm's own column"
        );
        for (index, &band) in Realm::WATERWORLD.strata().iter().enumerate() {
            assert_eq!(pelagic_index(band), Some(index));
        }
    }

    #[test]
    fn a_non_pelagic_stratum_has_no_band() {
        assert_eq!(pelagic_index(Stratum::Surface), None);
    }
}
