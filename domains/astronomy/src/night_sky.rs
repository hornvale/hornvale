//! The night sky: which stars are visible to a placed observer at any epoch.
//! Every query is a derived view (spec §2) — committed nowhere, recomputed
//! on demand.

use crate::calendar::{Calendar, SkyBand};
use crate::sky_position::EquatorialCoord;
use crate::system::StarSystem;
use crate::units::StdInstant;

/// Clear-sky naked-eye ceiling, before any observer suppression.
/// type-audit: pending(wave-1)
/// plumb: pending(wave-1)
pub const NAKED_EYE_MAGNITUDE_LIMIT: f64 = 6.0;

/// Species-independent detection constraints. A caller may lower the limiting
/// magnitude for local conditions; a zenith limits candidates to its hemisphere.
/// type-audit: pending(wave-1: limiting_magnitude)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct StarObserver {
    /// Faintest admitted apparent magnitude, capped at the naked-eye ceiling.
    pub limiting_magnitude: f64,
    /// Observer zenith in the same frame as candidate positions; None means all sky.
    pub zenith: Option<EquatorialCoord>,
}

impl Default for StarObserver {
    fn default() -> Self {
        Self {
            limiting_magnitude: NAKED_EYE_MAGNITUDE_LIMIT,
            zenith: None,
        }
    }
}

impl StarObserver {
    /// Shared inclusive brightness cut and above-horizon test. Invalid inputs refuse.
    /// type-audit: pending(wave-1: apparent_magnitude), bare-ok(flag: return)
    pub fn admits(&self, apparent_magnitude: f64, position: &EquatorialCoord) -> bool {
        if !self.limiting_magnitude.is_finite()
            || !apparent_magnitude.is_finite()
            || apparent_magnitude > self.limiting_magnitude.min(NAKED_EYE_MAGNITUDE_LIMIT)
            || !valid_position(position)
        {
            return false;
        }
        self.zenith.is_none_or(|zenith| {
            if !valid_position(&zenith) {
                return false;
            }
            let dec = position.dec_deg.to_radians();
            let latitude = zenith.dec_deg.to_radians();
            let hour_angle =
                (position.ra_deg.rem_euclid(360.0) - zenith.ra_deg.rem_euclid(360.0)).to_radians();
            hornvale_kernel::math::sin(latitude) * hornvale_kernel::math::sin(dec)
                + hornvale_kernel::math::cos(latitude)
                    * hornvale_kernel::math::cos(dec)
                    * hornvale_kernel::math::cos(hour_angle)
                > 0.0
        })
    }
}

/// The activity schedule used by an observer's species.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SkyActivity {
    /// Primarily active in daylight.
    Day,
    /// Primarily active at night.
    Night,
    /// Primarily active during twilight.
    Twilight,
}

/// Species-level visual traits consumed by astronomy without depending on
/// the species domain. Worldgen adapts its species component to this value.
/// type-audit: pending(wave-1)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SkyPerception {
    /// When the observer is active.
    pub activity: SkyActivity,
    /// Night-sky acuity, from blind (0) to exceptionally sensitive (1).
    pub acuity: f64,
    /// Attention paid to the sky, from earthbound (0) to sky-attentive (1).
    pub attention: f64,
}

impl SkyPerception {
    /// The reference daytime observer.
    pub const fn diurnal() -> Self {
        Self {
            activity: SkyActivity::Day,
            acuity: 0.5,
            attention: 0.5,
        }
    }

    /// A twilight observer.
    pub const fn crepuscular() -> Self {
        Self {
            activity: SkyActivity::Twilight,
            acuity: 0.5,
            attention: 0.5,
        }
    }

    /// A reference nighttime observer.
    pub const fn nocturnal() -> Self {
        Self {
            activity: SkyActivity::Night,
            acuity: 0.5,
            attention: 0.5,
        }
    }
}

/// Location and environmental conditions for a species-specific sky query.
/// type-audit: pending(wave-1)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SpeciesSkyObserver {
    /// Observer latitude in degrees.
    pub latitude: f64,
    /// Atmospheric suppression in the range 0 (clear) to 1 (opaque).
    pub atmosphere: f64,
    /// Relative moonlight in the range 0 (new moon) to 1 (full glare).
    pub moonlight: f64,
}

impl SpeciesSkyObserver {
    /// Construct an observer with clear, moonless conditions.
    /// type-audit: pending(wave-1: latitude)
    pub const fn at_latitude(latitude: f64) -> Self {
        Self {
            latitude,
            atmosphere: 0.0,
            moonlight: 0.0,
        }
    }
}

/// A species-specific derived sky view. The candidates retain physical IDs;
/// `sky_salience` is an attention weight and carries no cultural meaning.
/// type-audit: pending(wave-1)
#[derive(Debug, Clone, PartialEq)]
pub struct SpeciesSkyObservation {
    /// Stable modeled-star candidates admitted by the physical and perceptual cuts.
    pub visible: Vec<crate::starfield::SkyStar>,
    /// The effective limiting magnitude used for this query.
    pub limiting_magnitude: f64,
    /// The observer's unmodified sky-attention scalar.
    pub sky_salience: f64,
    /// Solar light regime at the query instant, when the world has a day.
    pub band: Option<SkyBand>,
}

/// Return stable modeled-star candidates visible to a species at an epoch.
/// This is a pure derived view: no roster, cache, or simulation mutation is
/// retained, and culture/name systems are deliberately not involved.
pub fn species_sky_at(
    system: &StarSystem,
    calendar: &Calendar,
    t: StdInstant,
    observer: SpeciesSkyObserver,
    perception: SkyPerception,
) -> SpeciesSkyObservation {
    let band = calendar.sky_band(t, observer.latitude);
    let limiting_magnitude = species_limiting_magnitude(&observer, &perception, calendar, t, band);
    let active = activity_admits(perception.activity, band);
    let visible = if active {
        let zenith = EquatorialCoord {
            ra_deg: local_meridian_ra(calendar, t),
            dec_deg: observer.latitude,
        };
        catalog_stars_at(
            system,
            calendar,
            t,
            &StarObserver {
                limiting_magnitude,
                zenith: Some(zenith),
            },
        )
    } else {
        Vec::new()
    };
    SpeciesSkyObservation {
        visible,
        limiting_magnitude,
        sky_salience: perception.attention.clamp(0.0, 1.0),
        band,
    }
}

fn species_limiting_magnitude(
    observer: &SpeciesSkyObserver,
    perception: &SkyPerception,
    calendar: &Calendar,
    t: StdInstant,
    band: Option<SkyBand>,
) -> f64 {
    let night_vision = perception.acuity.clamp(0.0, 1.0);
    let sky_attention = if perception.attention.is_finite() {
        perception.attention.clamp(0.0, 1.0)
    } else {
        0.0
    };
    let atmosphere = if observer.atmosphere.is_finite() {
        observer.atmosphere.clamp(0.0, 1.0)
    } else {
        1.0
    };
    let moonlight = if observer.moonlight.is_finite() {
        observer.moonlight.clamp(0.0, 1.0)
    } else {
        1.0
    };
    let twilight_penalty = if matches!(band, Some(SkyBand::Twilight)) {
        calendar
            .solar_altitude_at(t, observer.latitude)
            .map(|altitude| {
                let depth = (-altitude / crate::calendar::TWILIGHT_DEPTH_DEG).clamp(0.0, 1.0);
                2.0 * (1.0 - depth)
            })
            .unwrap_or(2.0)
    } else {
        0.0
    };
    (2.5 + 2.0 * night_vision + 1.5 * sky_attention
        - 2.0 * atmosphere
        - moonlight
        - twilight_penalty)
        .clamp(0.0, NAKED_EYE_MAGNITUDE_LIMIT)
}

fn local_meridian_ra(calendar: &Calendar, t: StdInstant) -> f64 {
    let Some((_, fraction)) = calendar.local_day(t) else {
        return 0.0;
    };
    let direction = if calendar.is_retrograde() { -1.0 } else { 1.0 };
    (calendar.solar_equatorial(t).ra_deg + 360.0 * (fraction - 0.5) * direction).rem_euclid(360.0)
}

fn activity_admits(activity: SkyActivity, band: Option<SkyBand>) -> bool {
    match band {
        // A locked world has no solar band; retain the useful nighttime view.
        None => matches!(activity, SkyActivity::Night),
        Some(SkyBand::Day) => false,
        Some(SkyBand::Twilight) => matches!(activity, SkyActivity::Twilight),
        Some(SkyBand::Night) => matches!(activity, SkyActivity::Night),
    }
}

fn valid_position(position: &EquatorialCoord) -> bool {
    position.ra_deg.is_finite() && (-90.0..=90.0).contains(&position.dec_deg)
}

/// Convert the catalog's solar-luminosity / light-year² flux to apparent magnitude.
/// Nonpositive or nonfinite flux has no magnitude.
/// type-audit: bare-ok(ratio: brightness), pending(wave-1: return)
pub fn stellar_apparent_magnitude(brightness: f64) -> Option<f64> {
    // Solar absolute visual magnitude and ten parsecs in light-years.
    // Catalog luminosity is bolometric: using it as visual flux is the
    // declared first-slice approximation, shared by every star consumer here.
    (brightness.is_finite() && brightness > 0.0).then(|| {
        4.83 - 2.5
            * (hornvale_kernel::math::log10(brightness)
                + 2.0 * hornvale_kernel::math::log10(32.6156))
    })
}

/// Visible modeled catalog stars at an explicit epoch, ordered by stable identity.
/// The legacy [`night_sky_at`] keeps its neighbor indices for existing readers.
pub fn catalog_stars_at(
    system: &StarSystem,
    calendar: &Calendar,
    t: StdInstant,
    observer: &StarObserver,
) -> Vec<crate::starfield::SkyStar> {
    if !t.0.is_finite() {
        return Vec::new();
    }
    let mut stars: Vec<_> = system
        .neighbor_catalog
        .iter()
        .filter_map(|star| {
            let magnitude = stellar_apparent_magnitude(star.neighbor().apparent_brightness)?;
            let genesis = EquatorialCoord {
                ra_deg: star.right_ascension,
                dec_deg: star.declination,
            };
            if !valid_position(&genesis) {
                return None;
            }
            let position = calendar.star_equatorial_at(&genesis, t);
            observer
                .admits(magnitude, &position)
                .then_some(crate::starfield::SkyStar {
                    id: crate::starfield::StarId::Catalog(star.id),
                    position,
                    apparent_magnitude: magnitude,
                })
        })
        .collect();
    stars.sort_by_key(|star| star.id);
    stars
}

/// Which celestial pole a pole star is closest to.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Hemisphere {
    /// North celestial pole.
    North,
    /// South celestial pole.
    South,
}

/// A star serving as the pole star at the current epoch.
/// type-audit: bare-ok(index: neighbor), pending(wave-1: separation_deg)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PoleStar {
    /// Index into `StarSystem::neighbors`.
    pub neighbor: usize,
    /// Which pole it marks.
    pub pole: Hemisphere,
    /// How far from the pole, in degrees.
    pub separation_deg: f64,
}

/// The night sky visible to an observer at a location and moment.
/// type-audit: bare-ok(index: visible), bare-ok(index: circumpolar), bare-ok(index: never_rises), bare-ok(flag: wheels_backward), bare-ok(flag: frozen)
#[derive(Debug, Clone, PartialEq)]
pub struct NightSky {
    /// Indices of stars visible tonight (circumpolar + seasonal).
    pub visible: Vec<usize>,
    /// Indices of stars that never set at this latitude.
    pub circumpolar: Vec<usize>,
    /// Indices of stars that never rise at this latitude.
    pub never_rises: Vec<usize>,
    /// The brightest star within the pole-search radius, if any — a
    /// system-level fact (declination near a pole), identical from every
    /// latitude.
    pub pole_star: Option<PoleStar>,
    /// Whether the world spins backward (sun rises in the west).
    pub wheels_backward: bool,
    /// Whether the sky is static (locked world, no day/night cycle).
    pub frozen: bool,
}

/// How many degrees from a celestial pole a star must be to serve as a pole star.
/// type-audit: pending(wave-1)
/// plumb: pending(wave-1)
pub const POLE_STAR_MAX_SEPARATION_DEG: f64 = 10.0;

/// The unified derived view (spec §2): everything a placed observer's night
/// holds, in one query. Committed nowhere — recomputed on demand.
/// type-audit: pending(wave-1: latitude)
pub fn night_sky_at(
    system: &StarSystem,
    calendar: &Calendar,
    latitude: f64,
    t: StdInstant,
) -> NightSky {
    let frozen = calendar.day_length().is_none();
    let sun = calendar.solar_equatorial(t);
    // Pole-star selection is a system-level fact (declination near a pole):
    // a separate pass over ALL neighbors in index order (= brightness order),
    // independent of the observer's latitude.
    let mut pole_star: Option<PoleStar> = None;
    for (i, n) in system.neighbors.iter().enumerate() {
        let genesis = EquatorialCoord {
            ra_deg: n.right_ascension,
            dec_deg: n.declination,
        };
        let pos = calendar.star_equatorial_at(&genesis, t);
        let north_sep = 90.0 - pos.dec_deg;
        let south_sep = 90.0 + pos.dec_deg;
        if north_sep <= POLE_STAR_MAX_SEPARATION_DEG {
            pole_star = Some(PoleStar {
                neighbor: i,
                pole: Hemisphere::North,
                separation_deg: north_sep,
            });
            break;
        } else if south_sep <= POLE_STAR_MAX_SEPARATION_DEG {
            pole_star = Some(PoleStar {
                neighbor: i,
                pole: Hemisphere::South,
                separation_deg: south_sep,
            });
            break;
        }
    }
    let mut circumpolar = Vec::new();
    let mut never_rises = Vec::new();
    let mut visible = Vec::new();
    for (i, n) in system.neighbors.iter().enumerate() {
        let genesis = EquatorialCoord {
            ra_deg: n.right_ascension,
            dec_deg: n.declination,
        };
        let pos = calendar.star_equatorial_at(&genesis, t);
        let same_side = pos.dec_deg.signum() == latitude.signum() && latitude != 0.0;
        let mut is_circumpolar = false;
        if same_side && pos.dec_deg.abs() > 90.0 - latitude.abs() {
            circumpolar.push(i);
            is_circumpolar = true;
        } else if !same_side && pos.dec_deg.abs() > 90.0 - latitude.abs() {
            never_rises.push(i);
            continue;
        }
        if is_circumpolar {
            visible.push(i);
            continue;
        }
        if frozen {
            visible.push(i);
            continue;
        }
        // Visible tonight = transits in the dark (declared approximation).
        let day_start = t.0
            - calendar.local_day(t).map(|(_, f)| f).unwrap_or(0.0)
                * calendar.day_length().map(|d| d.0).unwrap_or(1.0);
        let transit_fraction = (0.5 + (pos.ra_deg - sun.ra_deg) / 360.0).rem_euclid(1.0);
        let transit_t = StdInstant(
            day_start + transit_fraction * calendar.day_length().map(|d| d.0).unwrap_or(1.0),
        );
        if calendar.sky_band(transit_t, latitude) != Some(SkyBand::Day) {
            visible.push(i);
        }
    }
    NightSky {
        visible,
        circumpolar,
        never_rises,
        pole_star,
        wheels_backward: calendar.is_retrograde(),
        frozen,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::calendar::calendar_of;
    use crate::pins::{MoonsPin, RotationPin, SkyPins};
    use crate::system::generate;
    use hornvale_kernel::Seed;

    #[test]
    fn stellar_magnitudes_share_a_finite_inclusive_naked_eye_cut() {
        let at_ten_parsecs = stellar_apparent_magnitude(1.0 / (32.6156 * 32.6156)).unwrap();
        assert!((at_ten_parsecs - 4.83).abs() < 1e-10);
        let position = EquatorialCoord {
            ra_deg: 0.0,
            dec_deg: 0.0,
        };
        let observer = StarObserver::default();
        assert!(observer.admits(6.0, &position));
        assert!(!observer.admits(6.000001, &position));
        for invalid in [0.0, -1.0, f64::NAN, f64::INFINITY] {
            assert!(stellar_apparent_magnitude(invalid).is_none());
        }
        assert!(!observer.admits(f64::NAN, &position));
        assert!(
            !StarObserver {
                limiting_magnitude: f64::NAN,
                ..observer
            }
            .admits(1.0, &position)
        );
    }

    #[test]
    fn species_sky_observation_distinguishes_activity_cycles() {
        let mut system = spinning_system();
        for star in &mut system.neighbor_catalog {
            star.distance = crate::units::LightYears(0.01);
        }
        let calendar = calendar_of(&system);
        let observer = SpeciesSkyObserver {
            latitude: 0.0,
            atmosphere: 0.0,
            moonlight: 0.0,
        };
        let day_span = calendar.day_length().unwrap().0;
        let sample = |wanted| {
            (0..1000)
                .map(|i| StdInstant(day_span * f64::from(i) / 1000.0))
                .find(|&t| calendar.sky_band(t, 0.0) == Some(wanted))
                .expect("the spinning test world has every sky band")
        };
        let day = sample(SkyBand::Day);
        let twilight = sample(SkyBand::Twilight);
        let night = sample(SkyBand::Night);
        let diurnal = species_sky_at(&system, &calendar, day, observer, SkyPerception::diurnal());
        let crepuscular = species_sky_at(
            &system,
            &calendar,
            twilight,
            observer,
            SkyPerception::crepuscular(),
        );
        let nocturnal = species_sky_at(
            &system,
            &calendar,
            night,
            observer,
            SkyPerception::nocturnal(),
        );
        assert!(diurnal.visible.is_empty());
        assert!(!crepuscular.visible.is_empty());
        assert!(!nocturnal.visible.is_empty());
        assert_ne!(crepuscular.band, nocturnal.band);
    }

    #[test]
    fn night_vision_changes_limiting_magnitude_monotonically_and_continuously() {
        let system = spinning_system();
        let calendar = calendar_of(&system);
        let observer = SpeciesSkyObserver::at_latitude(0.0);
        let low = species_sky_at(
            &system,
            &calendar,
            StdInstant(calendar.day_length().unwrap().0 * 0.5),
            observer,
            SkyPerception {
                activity: SkyActivity::Night,
                acuity: 0.2,
                attention: 0.5,
            },
        );
        let middle = species_sky_at(
            &system,
            &calendar,
            StdInstant(calendar.day_length().unwrap().0 * 0.5),
            observer,
            SkyPerception {
                activity: SkyActivity::Night,
                acuity: 0.5,
                attention: 0.5,
            },
        );
        let high = species_sky_at(
            &system,
            &calendar,
            StdInstant(calendar.day_length().unwrap().0 * 0.5),
            observer,
            SkyPerception {
                activity: SkyActivity::Night,
                acuity: 0.8,
                attention: 0.5,
            },
        );
        assert!(low.limiting_magnitude < middle.limiting_magnitude);
        assert!(middle.limiting_magnitude < high.limiting_magnitude);
        assert!((middle.limiting_magnitude - low.limiting_magnitude).abs() > 0.0);
        assert!((high.limiting_magnitude - middle.limiting_magnitude).abs() > 0.0);
    }

    #[test]
    fn sky_attention_and_conditions_modify_the_physical_threshold() {
        let system = spinning_system();
        let calendar = calendar_of(&system);
        let t = StdInstant(calendar.day_length().unwrap().0 * 0.5);
        let clear = SpeciesSkyObserver::at_latitude(0.0);
        let haze = SpeciesSkyObserver {
            atmosphere: 0.5,
            moonlight: 0.5,
            ..clear
        };
        let inattentive = species_sky_at(
            &system,
            &calendar,
            t,
            clear,
            SkyPerception {
                activity: SkyActivity::Night,
                acuity: 0.5,
                attention: 0.0,
            },
        );
        let attentive = species_sky_at(
            &system,
            &calendar,
            t,
            clear,
            SkyPerception {
                activity: SkyActivity::Night,
                acuity: 0.5,
                attention: 1.0,
            },
        );
        let obscured = species_sky_at(&system, &calendar, t, haze, SkyPerception::nocturnal());
        assert!(inattentive.limiting_magnitude < attentive.limiting_magnitude);
        assert!(obscured.limiting_magnitude < attentive.limiting_magnitude);
        assert_eq!(attentive.sky_salience, 1.0);
    }

    #[test]
    fn species_sky_queries_are_repeatable_and_do_not_mutate_system() {
        let system = spinning_system();
        let before = system.clone();
        let calendar = calendar_of(&system);
        let observer = SpeciesSkyObserver::at_latitude(35.0);
        let perception = SkyPerception::nocturnal();
        let first = species_sky_at(&system, &calendar, StdInstant(0.0), observer, perception);
        let second = species_sky_at(&system, &calendar, StdInstant(0.0), observer, perception);
        assert_eq!(first, second);
        assert_eq!(system, before);
    }

    #[test]
    fn local_meridian_advances_with_the_day_but_is_static_when_locked() {
        let spinning = spinning_system();
        let spinning_calendar = calendar_of(&spinning);
        let day = spinning_calendar.day_length().unwrap().0;
        assert_ne!(
            local_meridian_ra(&spinning_calendar, StdInstant(0.0)),
            local_meridian_ra(&spinning_calendar, StdInstant(day * 0.25))
        );

        let locked = locked_system();
        let locked_calendar = calendar_of(&locked);
        assert_eq!(
            local_meridian_ra(&locked_calendar, StdInstant(0.0)),
            local_meridian_ra(&locked_calendar, StdInstant(5000.0))
        );
    }

    #[test]
    fn twilight_threshold_changes_continuously_with_solar_depth() {
        let system = spinning_system();
        let calendar = calendar_of(&system);
        let day = calendar.day_length().unwrap().0;
        let near_horizon = (0..1000)
            .map(|i| StdInstant(day * f64::from(i) / 1000.0))
            .find(|&t| {
                calendar
                    .solar_altitude_at(t, 0.0)
                    .is_some_and(|alt| (-4.0..=-2.0).contains(&alt))
            })
            .expect("a sample near the horizon");
        let deep_twilight = (0..1000)
            .map(|i| StdInstant(day * f64::from(i) / 1000.0))
            .find(|&t| {
                calendar
                    .solar_altitude_at(t, 0.0)
                    .is_some_and(|alt| (-10.0..=-8.0).contains(&alt))
            })
            .expect("a sample deep in twilight");
        let observer = SpeciesSkyObserver::at_latitude(0.0);
        let perception = SkyPerception::crepuscular();
        let near = species_sky_at(&system, &calendar, near_horizon, observer, perception);
        let deep = species_sky_at(&system, &calendar, deep_twilight, observer, perception);
        assert!(
            near.limiting_magnitude < deep.limiting_magnitude,
            "near={:?} deep={:?}",
            near,
            deep
        );
        assert_ne!(near.limiting_magnitude, deep.limiting_magnitude);
    }

    #[test]
    fn modeled_sky_uses_physical_brightness_and_stable_ids_after_reordering() {
        let mut system = spinning_system();
        let cal = calendar_of(&system);
        let observer = StarObserver::default();
        for star in &mut system.neighbor_catalog {
            star.distance = crate::units::LightYears(0.01);
        }
        let bright = catalog_stars_at(&system, &cal, StdInstant(0.0), &observer);
        assert_eq!(bright.len(), system.neighbor_catalog.len());
        system.neighbor_catalog.reverse();
        assert_eq!(
            bright,
            catalog_stars_at(&system, &cal, StdInstant(0.0), &observer)
        );
        for star in &mut system.neighbor_catalog {
            star.distance = crate::units::LightYears(1e12);
        }
        assert!(catalog_stars_at(&system, &cal, StdInstant(0.0), &observer).is_empty());
    }

    fn spinning_system() -> StarSystem {
        let pins = SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            moons: Some(MoonsPin::exact(1).unwrap()),
            ..SkyPins::default()
        };
        generate(Seed(42), &pins).unwrap().value
    }

    fn locked_system() -> StarSystem {
        let pins = SkyPins {
            rotation: Some(RotationPin::Locked),
            ..SkyPins::default()
        };
        generate(Seed(42), &pins).unwrap().value
    }

    #[test]
    fn the_equatorial_observer_sees_every_star_across_a_year() {
        let system = spinning_system();
        let cal = calendar_of(&system);
        let year = cal.year_length().get();
        let mut seen: std::collections::BTreeSet<usize> = std::collections::BTreeSet::new();
        for k in 0..48 {
            let sky = night_sky_at(&system, &cal, 0.0, StdInstant(k as f64 * year / 48.0));
            assert!(sky.circumpolar.is_empty() && sky.never_rises.is_empty());
            seen.extend(sky.visible.iter().copied());
        }
        assert_eq!(
            seen.len(),
            system.neighbors.len(),
            "a year at the equator shows the whole sky"
        );
    }

    #[test]
    fn the_polar_observer_sees_one_unchanging_hemisphere() {
        let system = spinning_system();
        let cal = calendar_of(&system);
        let sky = night_sky_at(&system, &cal, 90.0, StdInstant(0.0));
        assert_eq!(
            sky.circumpolar.len() + sky.never_rises.len(),
            system.neighbors.len(),
            "at the pole every star is circumpolar or invisible"
        );
    }

    #[test]
    fn a_locked_world_has_a_frozen_sky() {
        let system = locked_system();
        let cal = calendar_of(&system);
        let a = night_sky_at(&system, &cal, 30.0, StdInstant(0.0));
        let b = night_sky_at(&system, &cal, 30.0, StdInstant(5000.0));
        assert!(a.frozen);
        assert_eq!(
            a.visible, b.visible,
            "the locked sky is a map, not a calendar"
        );
    }

    #[test]
    fn pole_star_is_latitude_independent() {
        let system = spinning_system();
        let cal = calendar_of(&system);
        let t = StdInstant(0.0);
        let south = night_sky_at(&system, &cal, -60.0, t).pole_star;
        let equator = night_sky_at(&system, &cal, 0.0, t).pole_star;
        let north = night_sky_at(&system, &cal, 60.0, t).pole_star;
        assert_eq!(south, equator, "pole star is a system fact, not a vantage");
        assert_eq!(equator, north, "pole star is a system fact, not a vantage");
    }

    #[test]
    fn wheels_backward_mirrors_retrograde_spin() {
        let system_with_spin = |spin| {
            let pins = SkyPins {
                rotation: Some(RotationPin::PeriodHours(24.0)),
                spin: Some(spin),
                ..SkyPins::default()
            };
            generate(Seed(42), &pins).unwrap().value
        };
        let retro = system_with_spin(crate::pins::SpinPin::Retrograde);
        let sky = night_sky_at(&retro, &calendar_of(&retro), 30.0, StdInstant(0.0));
        assert!(
            sky.wheels_backward,
            "retrograde spin wheels the sky backward"
        );
        let pro = system_with_spin(crate::pins::SpinPin::Prograde);
        let sky = night_sky_at(&pro, &calendar_of(&pro), 30.0, StdInstant(0.0));
        assert!(!sky.wheels_backward, "prograde spin wheels the sky forward");
    }

    /// claim: structural(seed: none — false-positive seed-loop flag; the
    /// `.any(|s| ...)` closure's `s` binds a Vec<usize> element, not a seed) —
    /// single fixed spinning_system(), sampled at 12 points across one year
    #[test]
    fn seasonal_visibility_actually_varies() {
        let system = spinning_system();
        let cal = calendar_of(&system);
        let year = cal.year_length().get();
        let skies: Vec<Vec<usize>> = (0..12)
            .map(|k| night_sky_at(&system, &cal, 30.0, StdInstant(k as f64 * year / 12.0)).visible)
            .collect();
        assert!(
            skies.iter().any(|s| *s != skies[0]),
            "winter stars must differ from summer stars"
        );
    }
}
