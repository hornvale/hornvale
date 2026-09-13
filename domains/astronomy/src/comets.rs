//! Persistent comets and their analytically derived apparitions.

use crate::ephemeris::{
    OrbitalElements, OrbitalFrame, OrbitalPosition, OrbitalValidity, anchor_orbital_state_at,
    orbital_state_at,
};
use crate::streams;
use crate::units::{Au, SolarMasses, StdDays, StdInstant};
use crate::{Anchor, StarSystem};
use hornvale_kernel::{Seed, math};

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{SkyPins, generate};

    fn system_with_comets() -> (Seed, StarSystem) {
        (0..128u64)
            .filter_map(|seed| {
                let world_seed = Seed(seed);
                let system = generate(world_seed, &SkyPins::default()).unwrap().value;
                (!system.comets.is_empty())
                    .then_some((world_seed.derive(crate::streams::ROOT), system))
            })
            .next()
            .expect("bounded seed sweep should contain a comet-bearing system")
    }

    fn geometry_fixture() -> (StarSystem, Comet) {
        let (_, mut system) = system_with_comets();
        system.anchor.orbit = Au(1.0);
        system.anchor.year = StdDays(400.0);
        system.forcing.ecc_mean = 0.0;
        system.forcing.ecc_amp = 0.0;
        system.forcing.year_phase_offset = 0.25;
        let mut comet = system.comets[0].clone();
        comet.semi_major_axis = Au(4.0);
        comet.eccentricity = 0.5;
        comet.period = StdDays(100.0);
        comet.perihelion_epoch = StdInstant(0.0);
        comet.periapsis_longitude_deg = 90.0;
        comet.ascending_node_deg = 0.0;
        comet.inclination_deg = 0.0;
        comet.baseline_activity = 1.0;
        comet.return_variation = 0.0;
        (system, comet)
    }

    #[test]
    fn comet_identities_are_deterministic_bounded_and_use_the_identity_stream() {
        let (astronomy_seed, system) = system_with_comets();
        let again = generate_comets(
            astronomy_seed,
            system.stellar.gravity_mass(&system.star),
            &system.anchor,
        );
        assert_eq!(system.comets, again);
        assert!(system.comets.len() <= MAX_COMETS);

        let mut identities = astronomy_seed
            .derive(crate::streams::COMET_IDENTITIES)
            .stream();
        let expected: Vec<_> = (0..system.comets.len())
            .map(|_| CometId(identities.next_u64()))
            .collect();
        assert_eq!(
            system
                .comets
                .iter()
                .map(|comet| comet.id)
                .collect::<Vec<_>>(),
            expected
        );
    }

    #[test]
    fn return_index_is_epoch_anchored_and_activity_varies_by_apparition() {
        let (_, system) = system_with_comets();
        let comet = &system.comets[0];
        let conditions = CometObservation::dark_clear();
        let before = comet_appearance_at(
            &system,
            comet,
            StdInstant(comet.perihelion_epoch.get() - 0.25 * comet.period.get()),
            conditions,
        )
        .unwrap();
        let at_epoch =
            comet_appearance_at(&system, comet, comet.perihelion_epoch, conditions).unwrap();
        let third = comet_appearance_at(
            &system,
            comet,
            StdInstant(comet.perihelion_epoch.get() + 2.25 * comet.period.get()),
            conditions,
        )
        .unwrap();

        assert_eq!(before.return_index, 0);
        assert_eq!(at_epoch.return_index, 0);
        assert_eq!(before.activity_multiplier, at_epoch.activity_multiplier);
        assert_eq!(third.return_index, 2);
        assert_ne!(at_epoch.activity_multiplier, third.activity_multiplier);
    }

    #[test]
    fn apparition_boundaries_choose_nearest_perihelion_with_later_midpoint_ties() {
        let (system, mut comet) = geometry_fixture();
        comet.perihelion_epoch = StdInstant(1234.0);
        comet.return_variation = 0.35;
        for (cycles, expected) in [
            (-1.5001, -2),
            (-1.5, -1),
            (-1.4999, -1),
            (-0.75, -1),
            (-0.5001, -1),
            (-0.5, 0),
            (-0.4999, 0),
            (-0.25, 0),
            (0.0, 0),
            (0.25, 0),
            (0.4999, 0),
            (0.5, 1),
            (0.5001, 1),
            (1.5, 2),
        ] {
            let instant = StdInstant(comet.perihelion_epoch.get() + cycles * comet.period.get());
            let appearance =
                comet_appearance_at(&system, &comet, instant, CometObservation::dark_clear())
                    .unwrap();
            assert_eq!(appearance.return_index, expected, "cycles={cycles}");
            let perihelion =
                StdInstant(comet.perihelion_epoch.get() + expected as f64 * comet.period.get());
            let passage =
                comet_appearance_at(&system, &comet, perihelion, CometObservation::dark_clear())
                    .unwrap();
            assert_eq!(appearance.activity_multiplier, passage.activity_multiplier);
            assert_eq!(
                Some(appearance),
                comet_appearance_at(&system, &comet, instant, CometObservation::dark_clear())
            );
        }
    }

    #[test]
    fn inclination_and_node_change_observer_geometry() {
        let (system, mut comet) = geometry_fixture();
        let planar = comet_appearance_at(
            &system,
            &comet,
            StdInstant(0.0),
            CometObservation::dark_clear(),
        )
        .unwrap();
        // Observer (0, 1, 0), perihelion (0, 2, 0).
        assert!((planar.observer_distance.get() - 1.0).abs() < 1e-12);
        comet.inclination_deg = 90.0;
        let polar = comet_appearance_at(
            &system,
            &comet,
            StdInstant(0.0),
            CometObservation::dark_clear(),
        )
        .unwrap();
        // Rotation about the x-axis puts perihelion at (0, 0, 2).
        assert!((polar.observer_distance.get() - 5.0_f64.sqrt()).abs() < 1e-12);
        assert!((polar.z_au - 2.0).abs() < 1e-12);
        assert!(polar.position.y_au.abs() < 1e-12);
        assert!((polar.solar_elongation_deg - 63.43494882292201).abs() < 1e-10);
        assert!(polar.apparent_magnitude > planar.apparent_magnitude);
        assert_eq!(polar.heliocentric_distance, planar.heliocentric_distance);
        comet.ascending_node_deg = 90.0;
        let on_node = comet_appearance_at(
            &system,
            &comet,
            StdInstant(0.0),
            CometObservation::dark_clear(),
        )
        .unwrap();
        assert!((on_node.observer_distance.get() - 1.0).abs() < 1e-12);
        assert!(on_node.z_au.abs() < 1e-12);
    }

    #[test]
    fn retrograde_orientation_reverses_motion_and_preserves_radius() {
        let (system, mut comet) = geometry_fixture();
        comet.eccentricity = 0.0;
        comet.periapsis_longitude_deg = 0.0;
        comet.inclination_deg = 180.0;
        let appearance = comet_appearance_at(
            &system,
            &comet,
            StdInstant(25.0),
            CometObservation::dark_clear(),
        )
        .unwrap();
        assert!(appearance.position.x_au.abs() < 1e-12);
        assert!((appearance.position.y_au + 4.0).abs() < 1e-12);
        assert!(appearance.z_au.abs() < 1e-12);
        assert_eq!(appearance.heliocentric_distance, Au(4.0));
    }

    #[test]
    fn tail_strength_increases_with_activity_and_decreases_with_distance() {
        let (system, mut comet) = geometry_fixture();
        let near = comet_appearance_at(
            &system,
            &comet,
            StdInstant(0.0),
            CometObservation::dark_clear(),
        )
        .unwrap();
        let far = comet_appearance_at(
            &system,
            &comet,
            StdInstant(50.0),
            CometObservation::dark_clear(),
        )
        .unwrap();
        assert!(near.tail_strength > far.tail_strength);
        comet.baseline_activity = 2.0;
        let active = comet_appearance_at(
            &system,
            &comet,
            StdInstant(0.0),
            CometObservation::dark_clear(),
        )
        .unwrap();
        assert!(active.tail_strength > near.tail_strength);
        assert!(active.apparent_magnitude < near.apparent_magnitude);
        // Observing conditions affect detection, not physical tail activity.
        let daylight = comet_appearance_at(
            &system,
            &comet,
            StdInstant(0.0),
            CometObservation {
                darkness: 0.0,
                atmospheric_suppression: 100.0,
            },
        )
        .unwrap();
        assert_eq!(daylight.tail_strength, active.tail_strength);
        assert_eq!(daylight.visibility, CometVisibility::Latent);
    }

    #[test]
    fn tail_is_finite_at_high_eccentricity_and_extreme_finite_activity() {
        let (system, mut comet) = geometry_fixture();
        comet.eccentricity = 0.999;
        for activity in [f64::MIN_POSITIVE, 1.0, f64::MAX] {
            comet.baseline_activity = activity;
            for days in [-50.0, -0.01, 0.0, 0.01, 50.0] {
                let appearance = comet_appearance_at(
                    &system,
                    &comet,
                    StdInstant(days),
                    CometObservation::dark_clear(),
                )
                .unwrap();
                assert!(appearance.tail_strength.is_finite());
                assert!((0.0..=1.0).contains(&appearance.tail_strength));
                assert!(appearance.apparent_magnitude.is_finite());
            }
        }
    }

    #[test]
    fn invalid_orientation_and_periods_are_absent() {
        let (system, comet) = geometry_fixture();
        for invalid in [f64::NAN, f64::INFINITY, f64::NEG_INFINITY] {
            let mut bad = comet.clone();
            bad.inclination_deg = invalid;
            assert!(
                comet_appearance_at(
                    &system,
                    &bad,
                    StdInstant(0.0),
                    CometObservation::dark_clear()
                )
                .is_none()
            );
            bad = comet.clone();
            bad.ascending_node_deg = invalid;
            assert!(
                comet_appearance_at(
                    &system,
                    &bad,
                    StdInstant(0.0),
                    CometObservation::dark_clear()
                )
                .is_none()
            );
        }
        for period in [0.0, -1.0, f64::NAN, f64::INFINITY] {
            let mut bad = comet.clone();
            bad.period = StdDays(period);
            assert!(
                comet_appearance_at(
                    &system,
                    &bad,
                    StdInstant(0.0),
                    CometObservation::dark_clear()
                )
                .is_none()
            );
        }
    }

    #[test]
    fn validity_is_inclusive_and_centered_on_each_perihelion_epoch() {
        let (system, mut comet) = geometry_fixture();
        for epoch in [-2.0 * COMET_VALIDITY_DAYS, 2.0 * COMET_VALIDITY_DAYS] {
            comet.perihelion_epoch = StdInstant(epoch);
            for (offset, valid) in [
                (-COMET_VALIDITY_DAYS - 1.0, false),
                (-COMET_VALIDITY_DAYS, true),
                (0.0, true),
                (COMET_VALIDITY_DAYS, true),
                (COMET_VALIDITY_DAYS + 1.0, false),
            ] {
                assert_eq!(
                    comet_appearance_at(
                        &system,
                        &comet,
                        StdInstant(epoch + offset),
                        CometObservation::dark_clear()
                    )
                    .is_some(),
                    valid,
                    "epoch={epoch}, offset={offset}"
                );
            }
            assert!(
                comet_appearance_at(
                    &system,
                    &comet,
                    StdInstant(0.0),
                    CometObservation::dark_clear()
                )
                .is_none()
            );
        }
    }

    #[test]
    fn distant_queries_preserve_identity_and_repeat_the_orbit() {
        let (_, system) = system_with_comets();
        let comet = system.comets[0].clone();
        let original = comet.clone();
        let first = comet_appearance_at(
            &system,
            &comet,
            comet.perihelion_epoch,
            CometObservation::dark_clear(),
        )
        .unwrap();
        let distant = comet_appearance_at(
            &system,
            &comet,
            StdInstant(comet.perihelion_epoch.get() + 19.0 * comet.period.get()),
            CometObservation::dark_clear(),
        )
        .unwrap();

        assert_eq!(comet, original);
        assert_eq!(first.comet_id, distant.comet_id);
        assert!(
            (first.heliocentric_distance.get() - distant.heliocentric_distance.get()).abs() < 1e-8
        );
    }

    #[test]
    fn visibility_tiers_read_brightness_geometry_darkness_and_atmosphere() {
        assert_eq!(
            visibility_tier(-3.0, 60.0, 1.0, 0.0),
            CometVisibility::GreatComet
        );
        assert_eq!(
            visibility_tier(2.0, 60.0, 1.0, 0.0),
            CometVisibility::NakedEye
        );
        assert_eq!(
            visibility_tier(7.0, 60.0, 1.0, 0.0),
            CometVisibility::Latent
        );
        assert_eq!(visibility_tier(2.0, 4.0, 1.0, 0.0), CometVisibility::Latent);
        assert_eq!(
            visibility_tier(2.0, 60.0, 0.0, 0.0),
            CometVisibility::Latent
        );
        assert_eq!(
            visibility_tier(2.0, 60.0, 1.0, 5.0),
            CometVisibility::Latent
        );
    }

    #[test]
    fn malformed_queries_are_total_and_finite() {
        assert_eq!(
            visibility_tier(f64::NAN, f64::INFINITY, f64::NAN, f64::NEG_INFINITY),
            CometVisibility::Latent
        );
        let (_, system) = system_with_comets();
        assert!(
            comet_appearance_at(
                &system,
                &system.comets[0],
                StdInstant(f64::NAN),
                CometObservation {
                    darkness: f64::NAN,
                    atmospheric_suppression: f64::INFINITY,
                },
            )
            .is_none()
        );
    }
}

/// Maximum number of persistent comets generated for one star system.
/// type-audit: bare-ok(count)
/// plumb: pending(wave-1)
pub const MAX_COMETS: usize = 5;

/// Half-width of the comet evaluator's human-history validity window.
/// plumb: pending(wave-1)
pub(crate) const COMET_VALIDITY_DAYS: f64 = 3_652_500.0;

/// Stable identity of a persistent comet.
/// type-audit: bare-ok(constructor-edge)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CometId(pub u64);

/// Immutable genesis record for a persistent comet.
/// type-audit: bare-ok(ratio: eccentricity), pending(wave-1: inclination_deg), pending(wave-1: ascending_node_deg), pending(wave-1: periapsis_longitude_deg), bare-ok(ratio: baseline_activity), bare-ok(ratio: return_variation), pending(wave-1: nucleus_radius_km), bare-ok(ratio: albedo), pending(wave-1: absolute_magnitude), bare-ok(ratio: activity_distance_exponent)
#[derive(Debug, Clone, PartialEq)]
pub struct Comet {
    /// Stable identity, independent of physical-parameter streams.
    pub id: CometId,
    /// Semi-major axis of the heliocentric orbit.
    pub semi_major_axis: Au,
    /// Orbital eccentricity in `[0, 1)`.
    pub eccentricity: f64,
    /// Inclination to the system plane in degrees.
    pub inclination_deg: f64,
    /// Longitude of ascending node in degrees.
    pub ascending_node_deg: f64,
    /// Longitude of periapsis in degrees.
    pub periapsis_longitude_deg: f64,
    /// Absolute instant of the index-zero perihelion.
    pub perihelion_epoch: StdInstant,
    /// Sidereal orbital period.
    pub period: StdDays,
    /// Mean activity for this persistent identity.
    pub baseline_activity: f64,
    /// Maximum fractional deterministic variation between returns.
    pub return_variation: f64,
    /// Effective nucleus radius in kilometres.
    pub nucleus_radius_km: f64,
    /// Geometric albedo.
    pub albedo: f64,
    /// Apparent magnitude normalization at one AU from star and observer.
    pub absolute_magnitude: f64,
    /// Heliocentric distance exponent in the activity-brightness law.
    pub activity_distance_exponent: f64,
}

impl Comet {
    pub(crate) fn orbital_elements(&self) -> OrbitalElements {
        OrbitalElements {
            frame: OrbitalFrame::SystemPlaneAu,
            epoch: self.perihelion_epoch,
            period: self.period,
            semi_major_axis: self.semi_major_axis.get(),
            eccentricity: self.eccentricity,
            mean_longitude_at_epoch_turns: self.periapsis_longitude_deg / 360.0,
            periapsis_longitude_turns: self.periapsis_longitude_deg / 360.0,
            validity: OrbitalValidity {
                from: StdInstant(self.perihelion_epoch.get() - COMET_VALIDITY_DAYS),
                until: StdInstant(self.perihelion_epoch.get() + COMET_VALIDITY_DAYS),
            },
        }
    }

    // The shared evaluator's longitude of periapsis = node + argument of
    // periapsis. Rz(node) Rx(inclination) Rz(-node) tilts both state vectors.
    pub(crate) fn orient_vector(&self, vector: [f64; 2]) -> [f64; 3] {
        let node = self.ascending_node_deg.rem_euclid(360.0).to_radians();
        let inclination = self.inclination_deg.rem_euclid(360.0).to_radians();
        let (cos_node, sin_node) = (math::cos(node), math::sin(node));
        let node_x = vector[0] * cos_node + vector[1] * sin_node;
        let node_y = -vector[0] * sin_node + vector[1] * cos_node;
        let tilted_y = node_y * math::cos(inclination);
        [
            node_x * cos_node - tilted_y * sin_node,
            node_x * sin_node + tilted_y * cos_node,
            node_y * math::sin(inclination),
        ]
    }
}

/// Observer conditions that attenuate a comet without changing it.
/// type-audit: bare-ok(ratio: darkness), pending(wave-1: atmospheric_suppression)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CometObservation {
    /// Darkness fraction, clamped to `[0, 1]` at query time.
    pub darkness: f64,
    /// Atmospheric suppression in magnitudes; negative values are treated as zero.
    pub atmospheric_suppression: f64,
}

impl CometObservation {
    /// Fully dark, unattenuated observing conditions.
    pub const fn dark_clear() -> Self {
        Self {
            darkness: 1.0,
            atmospheric_suppression: 0.0,
        }
    }
}

/// Emergent visibility of one apparition.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum CometVisibility {
    /// Physically present but not visible to the naked eye.
    Latent,
    /// Visible without optical aid.
    NakedEye,
    /// Exceptionally bright naked-eye apparition.
    GreatComet,
}

/// One immutable comet identity evaluated at an explicit instant.
/// type-audit: pending(wave-1: return_index), pending(wave-1: z_au), bare-ok(ratio: activity_multiplier), bare-ok(ratio: tail_strength), pending(wave-1: apparent_magnitude), pending(wave-1: solar_elongation_deg)
#[derive(Debug, Clone, PartialEq)]
pub struct CometReturn {
    /// Persistent identity shared by every return.
    pub comet_id: CometId,
    /// Nearest perihelion relative to the epoch; midpoint ties choose the later one.
    pub return_index: i64,
    /// Heliocentric position projected onto the system plane, after orientation.
    pub position: OrbitalPosition,
    /// Signed height above the system plane in AU.
    pub z_au: f64,
    /// Distance from the stellar center.
    pub heliocentric_distance: Au,
    /// Three-dimensional distance from the anchor observer.
    pub observer_distance: Au,
    /// Deterministic activity multiplier for this identity and return.
    pub activity_multiplier: f64,
    /// Intrinsic tail prominence in `[0, 1]`, a bounded activity/heating proxy.
    /// Equal to activity / (activity + heliocentric distance in AU squared).
    /// This is neither an angular length nor an observer visibility threshold.
    pub tail_strength: f64,
    /// Unattenuated apparent magnitude before local observing conditions.
    pub apparent_magnitude: f64,
    /// Angular separation from the stellar center in the anchor sky.
    pub solar_elongation_deg: f64,
    /// Visibility under the supplied conditions.
    pub visibility: CometVisibility,
}

/// Generate the bounded persistent comet roster from dedicated streams.
pub fn generate_comets(
    astronomy_seed: Seed,
    gravity_mass: SolarMasses,
    anchor: &Anchor,
) -> Vec<Comet> {
    let count_roll = astronomy_seed
        .derive(streams::COMET_COUNT)
        .stream()
        .range_u32(1, 100);
    let count = match count_roll {
        1..=10 => 0,
        11..=35 => 1,
        36..=65 => 2,
        66..=85 => 3,
        86..=95 => 4,
        _ => MAX_COMETS as u32,
    };

    let mut identities = astronomy_seed.derive(streams::COMET_IDENTITIES).stream();
    let mut orbits = astronomy_seed.derive(streams::COMET_ORBITS).stream();
    let mut epochs = astronomy_seed.derive(streams::COMET_EPOCHS).stream();
    let mut activities = astronomy_seed.derive(streams::COMET_ACTIVITY).stream();
    let mut visibilities = astronomy_seed.derive(streams::COMET_VISIBILITY).stream();
    let mut comets = Vec::with_capacity(count as usize);

    for _ in 0..count {
        let perihelion = anchor.orbit.get() * (0.15 + 1.25 * orbits.next_f64());
        let aphelion = anchor.orbit.get() * (4.0 + 36.0 * orbits.next_f64());
        let semi_major_axis = 0.5 * (perihelion + aphelion);
        let eccentricity = (aphelion - perihelion) / (aphelion + perihelion);
        let period = 365.25 * (semi_major_axis.powi(3) / gravity_mass.get()).sqrt();
        let inclination_deg = 180.0 * orbits.next_f64();
        let ascending_node_deg = 360.0 * orbits.next_f64();
        let periapsis_longitude_deg = 360.0 * orbits.next_f64();
        let perihelion_epoch = (epochs.next_f64() - 0.5) * period;

        comets.push(Comet {
            id: CometId(identities.next_u64()),
            semi_major_axis: Au(semi_major_axis),
            eccentricity,
            inclination_deg,
            ascending_node_deg,
            periapsis_longitude_deg,
            perihelion_epoch: StdInstant(perihelion_epoch),
            period: StdDays(period),
            baseline_activity: 0.6 + 1.2 * activities.next_f64(),
            return_variation: 0.1 + 0.35 * activities.next_f64(),
            nucleus_radius_km: 0.5 + 14.5 * visibilities.next_f64(),
            albedo: 0.02 + 0.06 * visibilities.next_f64(),
            absolute_magnitude: 3.0 + 9.0 * visibilities.next_f64(),
            activity_distance_exponent: 2.0 + 3.0 * visibilities.next_f64(),
        });
    }
    comets
}

fn return_index(comet: &Comet, instant: StdInstant) -> Option<i64> {
    let cycles = (instant.get() - comet.perihelion_epoch.get()) / comet.period.get();
    let nearest = (cycles + 0.5).floor();
    // i64::MAX rounds up to 2^63 as f64, so the upper bound is exclusive.
    if !nearest.is_finite() || nearest < i64::MIN as f64 || nearest >= i64::MAX as f64 {
        return None;
    }
    Some(nearest as i64)
}

fn activity_at_return(comet: &Comet, index: i64) -> Option<f64> {
    if !comet.baseline_activity.is_finite()
        || comet.baseline_activity <= 0.0
        || !comet.return_variation.is_finite()
        || !(0.0..=1.0).contains(&comet.return_variation)
    {
        return None;
    }
    let keyed = comet.id.0
        ^ (index as u64)
            .wrapping_mul(0x9e37_79b9_7f4a_7c15)
            .rotate_left(29);
    let roll = Seed(keyed).stream().next_f64();
    let activity = comet.baseline_activity * (1.0 + comet.return_variation * (2.0 * roll - 1.0));
    (activity.is_finite() && activity > 0.0).then_some(activity)
}

/// Evaluate a persistent comet at an explicit instant without mutating it.
pub fn comet_appearance_at(
    system: &StarSystem,
    comet: &Comet,
    instant: StdInstant,
    observation: CometObservation,
) -> Option<CometReturn> {
    if !comet.inclination_deg.is_finite() || !comet.ascending_node_deg.is_finite() {
        return None;
    }
    let state = orbital_state_at(&comet.orbital_elements(), instant)?;
    let anchor = anchor_orbital_state_at(
        system.anchor.orbit,
        system.anchor.year,
        &system.forcing,
        instant,
    )?;
    let [x_au, y_au, z_au] = comet.orient_vector(state.position);
    let position = OrbitalPosition { x_au, y_au };
    let relative = [
        position.x_au - anchor.position[0],
        position.y_au - anchor.position[1],
        z_au,
    ];
    let observer_distance =
        (relative[0] * relative[0] + relative[1] * relative[1] + relative[2] * relative[2]).sqrt();
    let star_distance =
        (anchor.position[0] * anchor.position[0] + anchor.position[1] * anchor.position[1]).sqrt();
    if !observer_distance.is_finite()
        || observer_distance <= 0.0
        || !star_distance.is_finite()
        || star_distance <= 0.0
    {
        return None;
    }

    let elongation_cos = ((relative[0] * -anchor.position[0] + relative[1] * -anchor.position[1])
        / (observer_distance * star_distance))
        .clamp(-1.0, 1.0);
    let solar_elongation_deg = math::acos(elongation_cos).to_degrees();
    let index = return_index(comet, instant)?;
    let activity_multiplier = activity_at_return(comet, index)?;
    // Saturating inverse-square heating proxy; this form avoids overflow in
    // activity + radius^2 even for finite extreme inputs.
    let scaled_distance = state.radius / activity_multiplier.sqrt();
    let tail_strength = 1.0 / (1.0 + scaled_distance * scaled_distance);
    if !comet.absolute_magnitude.is_finite()
        || !comet.activity_distance_exponent.is_finite()
        || comet.activity_distance_exponent < 0.0
    {
        return None;
    }
    let apparent_magnitude = (comet.absolute_magnitude
        + 5.0 * math::log10(observer_distance)
        + 2.5 * comet.activity_distance_exponent * math::log10(state.radius)
        - 2.5 * math::log10(activity_multiplier))
    .clamp(-50.0, 50.0);
    if !apparent_magnitude.is_finite() || !solar_elongation_deg.is_finite() {
        return None;
    }

    Some(CometReturn {
        comet_id: comet.id,
        return_index: index,
        position,
        z_au,
        heliocentric_distance: Au(state.radius),
        observer_distance: Au(observer_distance),
        activity_multiplier,
        tail_strength,
        apparent_magnitude,
        solar_elongation_deg,
        visibility: visibility_tier(
            apparent_magnitude,
            solar_elongation_deg,
            observation.darkness,
            observation.atmospheric_suppression,
        ),
    })
}

/// Classify a comet from magnitude, elongation, darkness, and atmospheric loss.
/// type-audit: pending(wave-1: apparent_magnitude), pending(wave-1: solar_elongation_deg), bare-ok(ratio: darkness), pending(wave-1: atmospheric_suppression)
pub fn visibility_tier(
    apparent_magnitude: f64,
    solar_elongation_deg: f64,
    darkness: f64,
    atmospheric_suppression: f64,
) -> CometVisibility {
    if !apparent_magnitude.is_finite()
        || !solar_elongation_deg.is_finite()
        || !darkness.is_finite()
        || !atmospheric_suppression.is_finite()
    {
        return CometVisibility::Latent;
    }
    let elongation = solar_elongation_deg.clamp(0.0, 180.0);
    let darkness_penalty = 8.0 * (1.0 - darkness.clamp(0.0, 1.0));
    let atmosphere_penalty = atmospheric_suppression.max(0.0);
    let glare_penalty = 0.4 * (20.0 - elongation).max(0.0);
    let effective_magnitude =
        apparent_magnitude + darkness_penalty + atmosphere_penalty + glare_penalty;

    if elongation >= 10.0 && effective_magnitude <= -1.0 {
        CometVisibility::GreatComet
    } else if elongation >= 8.0 && effective_magnitude <= 6.0 {
        CometVisibility::NakedEye
    } else {
        CometVisibility::Latent
    }
}
