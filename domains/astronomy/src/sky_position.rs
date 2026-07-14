//! Celestial coordinates (campaign spec §2): where the sun and the fixed
//! stars sit on the sky's sphere at any epoch. The equinox precesses — this
//! module is `forcing::precession_at`'s first reader (SKY-stale-alignments,
//! sky half) — so every apparent position is a function of time. Star
//! ecliptic coordinates are fixed (no proper motion; declared approximation).

use hornvale_kernel::math;

/// A position on the ecliptic sphere, degrees. Fixed for all time.
/// type-audit: pending(wave-1: lon_deg), pending(wave-1: lat_deg)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct EclipticCoord {
    /// Ecliptic longitude, degrees in [0, 360), from the genesis equinox.
    pub lon_deg: f64,
    /// Ecliptic latitude, degrees in [-90, 90].
    pub lat_deg: f64,
}

/// A position on the equatorial sphere, degrees, valid at one epoch.
/// type-audit: pending(wave-1: ra_deg), pending(wave-1: dec_deg)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct EquatorialCoord {
    /// Right ascension, degrees in [0, 360).
    pub ra_deg: f64,
    /// Declination, degrees in [-90, 90].
    pub dec_deg: f64,
}

/// Convert genesis equatorial coordinates to (time-fixed) ecliptic ones,
/// through the genesis obliquity.
/// type-audit: pending(wave-1)
pub fn ecliptic_of(eq: &EquatorialCoord, obliquity_deg: f64) -> EclipticCoord {
    let (a, d) = (eq.ra_deg.to_radians(), eq.dec_deg.to_radians());
    let e = obliquity_deg.to_radians();
    let lat = math::asin(math::sin(d) * math::cos(e) - math::cos(d) * math::sin(e) * math::sin(a));
    let lon = math::atan2(
        math::sin(a) * math::cos(e) + math::tan(d) * math::sin(e),
        math::cos(a),
    );
    EclipticCoord {
        lon_deg: lon.to_degrees().rem_euclid(360.0),
        lat_deg: lat.to_degrees(),
    }
}

/// Project ecliptic coordinates onto the equator of an epoch: the apparent
/// longitude is `lon + precession_offset`, re-tilted by that epoch's
/// obliquity.
/// type-audit: pending(wave-1)
pub fn equatorial_at(
    ecl: &EclipticCoord,
    obliquity_deg: f64,
    precession_offset_deg: f64,
) -> EquatorialCoord {
    let lon = (ecl.lon_deg + precession_offset_deg).to_radians();
    let b = ecl.lat_deg.to_radians();
    let e = obliquity_deg.to_radians();
    let dec =
        math::asin(math::sin(b) * math::cos(e) + math::cos(b) * math::sin(e) * math::sin(lon));
    let ra = math::atan2(
        math::sin(lon) * math::cos(e) - math::tan(b) * math::sin(e),
        math::cos(lon),
    );
    EquatorialCoord {
        ra_deg: ra.to_degrees().rem_euclid(360.0),
        dec_deg: dec.to_degrees(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trip_through_the_ecliptic_is_identity_at_zero_precession() {
        let genesis = EquatorialCoord {
            ra_deg: 123.4,
            dec_deg: -21.7,
        };
        let ecl = ecliptic_of(&genesis, 23.5);
        let back = equatorial_at(&ecl, 23.5, 0.0);
        assert!((back.ra_deg - genesis.ra_deg).abs() < 1e-9);
        assert!((back.dec_deg - genesis.dec_deg).abs() < 1e-9);
    }

    #[test]
    fn a_full_precession_turn_returns_every_star_home() {
        let genesis = EquatorialCoord {
            ra_deg: 300.0,
            dec_deg: 55.0,
        };
        let ecl = ecliptic_of(&genesis, 20.0);
        let after = equatorial_at(&ecl, 20.0, 360.0);
        assert!((after.ra_deg - genesis.ra_deg).abs() < 1e-9);
        assert!((after.dec_deg - genesis.dec_deg).abs() < 1e-9);
    }

    #[test]
    fn precession_moves_the_pole_separation() {
        // A star near the pole drifts as the pole circles the ecliptic pole.
        let genesis = EquatorialCoord {
            ra_deg: 40.0,
            dec_deg: 88.0,
        };
        let ecl = ecliptic_of(&genesis, 23.5);
        let half_turn = equatorial_at(&ecl, 23.5, 180.0);
        let sep0 = 90.0 - genesis.dec_deg;
        let sep1 = 90.0 - half_turn.dec_deg;
        assert!(
            (sep1 - sep0).abs() > 1.0,
            "pole separation must move: {sep0} vs {sep1}"
        );
    }
}
