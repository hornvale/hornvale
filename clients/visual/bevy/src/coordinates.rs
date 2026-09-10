//! Supported camera envelope: origins up to 1e12 km; relative coordinates up to
//! 2e10 km. Convert only after f64 origin subtraction. Camera FOV >= .005 rad.
use crate::ViewError;
pub fn render_position(
    position_km: [f64; 3],
    origin_km: [f64; 3],
    km_per_unit: f64,
) -> Result<[f32; 3], ViewError> {
    if !km_per_unit.is_finite() || km_per_unit <= 0. {
        return Err(ViewError::Range(
            "km_per_unit must be finite and positive".into(),
        ));
    }
    if origin_km.iter().any(|x| !x.is_finite() || x.abs() > 1e12)
        || position_km
            .iter()
            .zip(origin_km)
            .any(|(p, o)| !p.is_finite() || (p - o).abs() > 2e10)
    {
        return Err(ViewError::Range(
            "outside supported camera-relative range".into(),
        ));
    }
    let p = std::array::from_fn(|i| ((position_km[i] - origin_km[i]) / km_per_unit) as f32);
    if p.iter().any(|v| !v.is_finite()) {
        return Err(ViewError::Range("position exceeds rendering range".into()));
    }
    Ok(p)
}
