//! The caloric-summer insolation index: the Milankovitch–Köppen driver of
//! glaciation, a closed-form function of the orbital elements. High-latitude
//! summer warmth rises with axial tilt and with climatic precession (e·sin ϖ);
//! low summer warmth is what lets winter snow survive and ice sheets grow.
//! Dimensionless, ~0 at the reference sky; positive = warm summers.

/// Index zero-point obliquity, degrees.
const OBLIQUITY_REF_DEG: f64 = 23.5;
/// Index sensitivity to obliquity, per degree.
const K_OBLIQUITY: f64 = 1.0;
/// Index sensitivity to climatic precession (e·sin ϖ).
const K_PRECESSION: f64 = 40.0;

/// The caloric-summer index at one moment's orbital elements. All three
/// Milankovitch elements enter: obliquity directly, eccentricity and
/// precession through the climatic-precession term `e·sin(ϖ)`.
pub fn caloric_summer_index(obliquity_deg: f64, eccentricity: f64, precession_phase: f64) -> f64 {
    (obliquity_deg - OBLIQUITY_REF_DEG) * K_OBLIQUITY
        + eccentricity * precession_phase.sin() * K_PRECESSION
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::f64::consts::FRAC_PI_2;

    #[test]
    fn reference_sky_is_near_zero() {
        // Reference obliquity, no eccentricity → index is exactly zero.
        assert_eq!(caloric_summer_index(23.5, 0.0, 0.0), 0.0);
    }

    #[test]
    fn higher_tilt_warms_summers() {
        let low = caloric_summer_index(22.0, 0.0, 0.0);
        let high = caloric_summer_index(24.5, 0.0, 0.0);
        assert!(high > low, "more tilt must raise the summer index");
    }

    #[test]
    fn precession_enters_scaled_by_eccentricity() {
        // At zero eccentricity, precession phase does nothing.
        assert_eq!(
            caloric_summer_index(23.5, 0.0, FRAC_PI_2),
            caloric_summer_index(23.5, 0.0, 0.0)
        );
        // At nonzero eccentricity, phase moves the index.
        assert!(caloric_summer_index(23.5, 0.03, FRAC_PI_2) > 0.0);
    }
}
