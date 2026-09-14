//! Evaluated native astronomy, independent of rendering and saved world state.
use crate::SceneError;
use hornvale_astronomy::{StarSystem, StdInstant, ephemeris};
use hornvale_kernel::{
    Seed, World, WorldTime, math,
    quantize::{quantize, quantize_serde},
};
use serde::{Serialize, Serializer};

/// Evaluated observation schema; existing element schemas remain unchanged.
/// type-audit: bare-ok(identifier-text)
pub const ASTRONOMY_AT_SCHEMA: &str = "scene/astronomy-at/v1";
/// IAU exact astronomical unit, JPL https://ssd.jpl.nasa.gov/astro_par.html,
/// 149597870700 metres, expressed in kilometres.
/// plumb: universal(the IAU exact astronomical unit expressed in kilometres; a unit conversion, not a world parameter)
const KM_PER_AU: f64 = 149_597_870.7;
/// SI megametre to kilometre conversion.
/// plumb: universal(SI decimal prefixes define a megametre as one thousand kilometres)
const KM_PER_MM: f64 = 1000.0;

/// Immutable initialized astronomy; repeated queries never rebuild a world.
pub struct AstronomyContext {
    seed: Seed,
    system: StarSystem,
}
impl AstronomyContext {
    /// Reconstruct only the astronomy provider from the world's seed and pins.
    pub fn build(world: &World) -> Result<Self, SceneError> {
        let sky = hornvale_worldgen::sky_of(world).map_err(|e| SceneError::Build(e.to_string()))?;
        Ok(Self {
            seed: world.seed,
            system: sky.system().clone(),
        })
    }
}

/// A physical body with explicit absence where the source has no dimension or spin.
/// type-audit: bare-ok(identifier-text: id), bare-ok(identifier-text: kind), bare-ok(artifact: position_km), bare-ok(artifact: radius_km), bare-ok(artifact: body_to_frame)
#[derive(Debug, Clone, Serialize)]
pub struct AstronomyBody {
    /// Catalog-local ID, meaningful only within the bound world and revision.
    pub id: String,
    /// Anchor, star, moon or wanderer.
    pub kind: String,
    /// Anchor-centered native position, in kilometres.
    #[serde(serialize_with = "quantize_serde::vec_f64_field")]
    pub position_km: [f64; 3],
    /// Physical radius in kilometres, or null when unavailable.
    #[serde(serialize_with = "quantize_serde::opt_f64_field")]
    pub radius_km: Option<f64>,
    /// Three basis columns (longitude zero, longitude 90, north), or null.
    #[serde(serialize_with = "basis_field")]
    pub body_to_frame: Option<[[f64; 3]; 3]>,
}
/// One native star's evaluated unattenuated contribution.
/// type-audit: bare-ok(identifier-text: star_id), bare-ok(artifact: direction_from_anchor), bare-ok(artifact: flux_rel), bare-ok(artifact: luminosity_rel)
#[derive(Debug, Clone, Serialize)]
pub struct AstronomyLight {
    /// Catalog-local source ID.
    pub star_id: String,
    /// Unit anchor-to-star vector in the native system frame.
    #[serde(serialize_with = "quantize_serde::vec_f64_field")]
    pub direction_from_anchor: [f64; 3],
    /// Earth-relative unattenuated irradiance at the anchor.
    #[serde(serialize_with = "quantize_serde::f64_field")]
    pub flux_rel: f64,
    /// Current solar-relative luminosity from `luminosity_at`, for off-anchor light.
    #[serde(serialize_with = "quantize_serde::f64_field")]
    pub luminosity_rel: f64,
}
/// Evaluated scene. Floats retain native precision until serialization.
/// type-audit: bare-ok(identifier-text: schema), bare-ok(artifact: seed), bare-ok(artifact: ticks), bare-ok(artifact: ticks_per_std_day), bare-ok(identifier-text: frame), bare-ok(prose: models)
#[derive(Debug, Clone, Serialize)]
pub struct AstronomyAtScene {
    /// Versioned document type.
    pub schema: String,
    /// Seed, supplemented by the source envelope's full world hash.
    pub seed: u64,
    /// Exact requested instant, never converted to a JSON float.
    pub ticks: i64,
    /// Native standard-day clock scale.
    pub ticks_per_std_day: i64,
    /// Position frame and units.
    pub frame: String,
    /// Ordered source-model and validity disclosures.
    pub models: Vec<String>,
    /// Anchor, stars, moons, wanderers in native catalog order.
    pub bodies: Vec<AstronomyBody>,
    /// Primary then companion.
    pub lights: Vec<AstronomyLight>,
}

// Local nested-array emit adapter: computation remains unquantized.
fn basis_field<S: Serializer>(basis: &Option<[[f64; 3]; 3]>, s: S) -> Result<S::Ok, S::Error> {
    basis
        .map(|columns| columns.map(|column| column.map(quantize)))
        .serialize(s)
}

/// Convenience world query, building astronomy once for an offline caller.
pub fn astronomy_at_scene(world: &World, at: WorldTime) -> Result<AstronomyAtScene, SceneError> {
    astronomy_at_scene_in(&AstronomyContext::build(world)?, at)
}

/// Observe an immutable initialized system at exact native ticks.
pub fn astronomy_at_scene_in(
    ctx: &AstronomyContext,
    at: WorldTime,
) -> Result<AstronomyAtScene, SceneError> {
    // The only exact-to-continuous conversion, at the existing ephemeris boundary.
    let instant =
        StdInstant::new(at.as_std_days()).map_err(|e| SceneError::AstronomyQuery(e.to_string()))?;
    let system = &ctx.system;
    let anchor_state = hornvale_astronomy::anchor_state_at(system, instant)
        .map_err(|e| SceneError::AstronomyQuery(e.to_string()))?;
    let anchor = ephemeris::OrbitalPosition {
        x_au: anchor_state.position_au[0],
        y_au: anchor_state.position_au[1],
    };
    let radius = hornvale_astronomy::anchor_radius(system.anchor.mass)
        .map_err(|e| SceneError::AstronomyQuery(e.to_string()))?;
    let mut bodies = vec![AstronomyBody {
        id: "anchor".into(),
        kind: "anchor".into(),
        position_km: [0.0; 3],
        radius_km: Some(radius.get() * KM_PER_MM),
        body_to_frame: Some(ephemeris::anchor_body_to_frame_at(system, instant)),
    }];
    let relative = |p: ephemeris::OrbitalPosition| {
        [
            (p.x_au - anchor.x_au) * KM_PER_AU,
            (p.y_au - anchor.y_au) * KM_PER_AU,
            0.0,
        ]
    };
    for (i, p) in ephemeris::stellar_positions_at(system, instant)
        .into_iter()
        .enumerate()
    {
        bodies.push(AstronomyBody {
            id: format!("star:{i}"),
            kind: "star".into(),
            position_km: relative(p),
            radius_km: None,
            body_to_frame: None,
        });
    }
    for (i, moon) in system.moons.iter().enumerate() {
        let p = ephemeris::moon_position_at(system, i, instant).ok_or_else(|| {
            SceneError::AstronomyQuery(format!("moon:{i} has no native position: no synodic cycle"))
        })?;
        bodies.push(AstronomyBody {
            id: format!("moon:{i}"),
            kind: "moon".into(),
            position_km: p.map(|v| v * KM_PER_MM),
            radius_km: Some(hornvale_astronomy::radius_km(moon)),
            body_to_frame: None,
        });
    }
    for i in 0..system.wanderers.len() {
        let p = ephemeris::wanderer_position_at(system, i, instant).ok_or_else(|| {
            SceneError::AstronomyQuery(format!("wanderer:{i} has no native position"))
        })?;
        bodies.push(AstronomyBody {
            id: format!("wanderer:{i}"),
            kind: "wanderer".into(),
            position_km: relative(p),
            radius_km: None,
            body_to_frame: None,
        });
    }
    let lights = ephemeris::stellar_illumination_at(system, instant)
        .sources
        .into_iter()
        .map(|light| {
            let angle = light.longitude.get().to_radians();
            let star = if light.star == 0 {
                &system.star
            } else {
                &system
                    .stellar
                    .companion
                    .as_ref()
                    .expect("native companion light has a star")
                    .star
            };
            let luminosity_rel = hornvale_astronomy::luminosity_at(star, instant).get();
            if !luminosity_rel.is_finite() || luminosity_rel <= 0.0 || !light.flux_rel.is_finite() || light.flux_rel <= 0.0 {
                return Err(SceneError::AstronomyQuery(format!("star:{} native luminosity/flux is nonpositive or nonfinite at ticks {}; outside the linear brightening model's usable range", light.star, at.ticks())));
            }
            Ok(AstronomyLight {
                star_id: format!("star:{}", light.star),
                direction_from_anchor: [math::cos(angle), math::sin(angle), 0.0],
                flux_rel: light.flux_rel,
                luminosity_rel,
            })
        })
        .collect::<Result<Vec<_>, SceneError>>()?;
    Ok(AstronomyAtScene {
        schema: ASTRONOMY_AT_SCHEMA.into(), seed: ctx.seed.0, ticks: at.ticks(), ticks_per_std_day: WorldTime::TICKS_PER_STD_DAY,
        frame: "anchor-centered-system-plane/km/right-handed".into(),
        models: vec![
            "stellar-wanderer-circular-coplanar/v1: native ephemeris; wide circumprimary, close barycentric; no finite stellar disc".into(),
            "calendar-exact-equatorial/v1: Rz(pi) Rx(-obliquity) reconciles solar and native frames; spin follows native subsolar longitude, including retrograde and locked".into(),
            "moon-native-ecliptic/v1: existing synodic longitude, inclination and regressing node; no-cycle queries fail; null moon spin is unavailable, cosmetic orientation only".into(),
            "earthlike-rocky-zeng2019-linear/v1: spherical 32.5% Fe/67.5% MgSiO3 anchor, 0.5–2 Earth masses; no interior simulation".into(),
            "native-moon-density-radius/v1: physical moon radius; null stellar/wanderer radius is unavailable, point presentation only".into(),
            "native-luminosity-at/v1: linear main-sequence brightening; current solar-relative luminosity and Earth-relative inverse-square flux must be finite and positive or the query fails; no attenuation or finite-disc shadows".into(),
            "quantize-eight-significant-digits/v1: kilometres and basis quantized only at emit; exact ticks, continuous f64 ephemeris time".into(),
        ], bodies, lights,
    })
}

/// Compact ordered JSON, with native quantization only at this emit boundary.
/// type-audit: bare-ok(artifact: return)
pub fn astronomy_at_json(scene: &AstronomyAtScene) -> String {
    serde_json::to_string(scene).expect("an AstronomyAtScene always serializes")
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn astronomy_at_no_cycle_is_a_query_error() {
        let mut system = hornvale_astronomy::generate(Seed(42), &Default::default())
            .unwrap()
            .value;
        system.moons[0].period = system.anchor.year;
        let ctx = AstronomyContext {
            seed: Seed(42),
            system,
        };
        let error = astronomy_at_scene_in(&ctx, WorldTime::GENESIS).unwrap_err();
        assert!(
            matches!(error, SceneError::AstronomyQuery(ref message) if message.contains("moon:0") && message.contains("no synodic cycle"))
        );
    }
}
