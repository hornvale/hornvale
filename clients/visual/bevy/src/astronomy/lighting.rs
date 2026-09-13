//! Every body gets source-position lighting, isolated by a render layer.
use crate::{
    ViewError,
    documents::{Body, Light},
};
/// Unattenuated source flux at the observed body's location, in Earth units.
pub fn at_body(light: &Light, star: &Body, body: &Body) -> Result<([f32; 3], f32), ViewError> {
    let delta: [f64; 3] = std::array::from_fn(|i| star.position_km[i] - body.position_km[i]);
    let d2 = delta.iter().map(|x| x * x).sum::<f64>();
    if !d2.is_finite() || d2 <= 0. {
        return Err(ViewError::Range("invalid star/body separation".into()));
    }
    let direction = delta.map(|x| (x / d2.sqrt()) as f32);
    let flux = if body.id == "anchor" {
        light.flux_rel
    } else {
        light.luminosity_rel * 149_597_870.7_f64.powi(2) / d2
    };
    if !flux.is_finite() || flux <= 0. || flux > 1e12 {
        return Err(ViewError::Range(
            "source flux outside supported range".into(),
        ));
    }
    Ok((direction, flux as f32))
}

/// Lux at one astronomical unit -> isotropic lumens in render-unit distance.
/// Bevy divides point intensity by 4π and squared render distance. The 1e9-unit
/// cutoff is >=5000 times the supported moon-scene stellar distance, making its
/// smooth attenuation below 1e-12 in this pilot; it is not a stellar radius.
pub fn point_intensity(luminosity_rel: f64, km_per_unit: f64) -> Result<f32, ViewError> {
    let intensity = 127_000.0
        * luminosity_rel
        * (149_597_870.7 / km_per_unit).powi(2)
        * std::f64::consts::TAU
        * 2.0;
    if !intensity.is_finite() || intensity <= 0.0 || intensity > f64::from(f32::MAX) {
        return Err(ViewError::Range(
            "stellar intensity exceeds render range".into(),
        ));
    }
    Ok(intensity as f32)
}
