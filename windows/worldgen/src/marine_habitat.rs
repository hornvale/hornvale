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
/// type-audit: bare-ok(ratio: chemosynthate)
#[derive(Clone, Debug)]
pub struct MarineHabitat {
    /// The substrate at each pelagic band, `None` where the column does not
    /// reach that band (every band below the seabed, and every band at a
    /// vertex that is not ocean).
    pub substrate: VertexMap<[Option<Substrate>; PELAGIC_BANDS]>,
    /// The `CHEMOSYNTHATE` supply at each pelagic band, on the same `[0, 1]`
    /// ruler [`crate::energy::subterranean_energy`] returns — so the marine
    /// and subterranean chemotrophic supplies are comparable rather than two
    /// scales sharing one axis name. `0.0` where the column does not reach
    /// the band.
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
                // The pelagic light ladder: `WaterFields::light` is the
                // world's insolation attenuated by this sample's depth, which
                // is exactly the quantity the surface substrate's
                // `insolation` carries at the surface.
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
