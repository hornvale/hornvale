//! The generated sky: a time-varying phenomena provider (tier 1/2) driven
//! entirely by a genesis outcome. Downstream systems see only `sky_at` and
//! `phenomena` — never the system or calendar directly.

use crate::anchor::Rotation;
use crate::calendar::{Calendar, calendar_of};
use crate::system::{GenesisOutcome, StarSystem};
use crate::units::StdDays;
use crate::{CELESTIAL_BODY, SkyReport};
use hornvale_kernel::{ObserverContext, PhenomenaSource, Phenomenon, Venue, WorldTime};

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pins::{MoonsPin, RotationPin, SkyPins};
    use crate::system::generate;
    use hornvale_kernel::{
        EntityId, GeoCoord, ObserverContext, PhenomenaSource, Seed, Venue, WorldTime,
    };

    fn sky(pins: SkyPins) -> GeneratedSky {
        GeneratedSky::new(generate(Seed(42), &pins).unwrap())
    }

    fn ctx(day: f64) -> ObserverContext {
        ObserverContext::at(EntityId(1), WorldTime { day })
    }

    #[test]
    fn a_spinning_sun_is_periodic_and_top_salience() {
        let s = sky(SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            ..SkyPins::default()
        });
        let seen = s.phenomena(&ctx(0.0));
        let sun = seen
            .iter()
            .find(|p| p.description.starts_with("the sun"))
            .unwrap();
        assert_eq!(sun.period_days, Some(1.0));
        assert_eq!(sun.salience, 1.0);
    }

    #[test]
    fn a_locked_sun_is_aperiodic() {
        let s = sky(SkyPins {
            rotation: Some(RotationPin::Locked),
            ..SkyPins::default()
        });
        let seen = s.phenomena(&ctx(0.0));
        let sun = seen
            .iter()
            .find(|p| p.description.contains("fixed forever"))
            .unwrap();
        assert_eq!(sun.period_days, None);
        assert_eq!(sun.salience, 1.0);
    }

    #[test]
    fn moons_and_seasons_carry_their_periods() {
        let s = sky(SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            moons: Some(MoonsPin::exact(2).unwrap()),
            ..SkyPins::default()
        });
        let seen = s.phenomena(&ctx(0.0));
        let moons: Vec<_> = seen
            .iter()
            .filter(|p| p.kind == CELESTIAL_BODY && p.description.contains("moon"))
            .collect();
        assert_eq!(moons.len(), 2);
        for m in &moons {
            assert!(m.period_days.is_some());
            assert!(m.salience < 1.0);
        }
        if s.system().anchor.obliquity.get() > 0.0 {
            assert!(seen.iter().any(|p| p.kind == SEASONAL_CYCLE));
        }
    }

    #[test]
    fn neighbors_appear_only_in_darkness_or_on_locked_worlds() {
        let spinning = sky(SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            ..SkyPins::default()
        });
        let cal = spinning.calendar();
        // Find a midday and a midnight of local day 10.
        let day_len = cal.day_length().unwrap().get();
        let noon = 10.0 * day_len + 0.5 * day_len;
        let midnight = 10.0 * day_len + 0.01 * day_len;
        let at_noon = spinning.phenomena(&ctx(noon));
        let at_night = spinning.phenomena(&ctx(midnight));
        assert!(at_noon.iter().all(|p| p.kind != NIGHT_STAR));
        assert!(at_night.iter().any(|p| p.kind == NIGHT_STAR));

        let locked = sky(SkyPins {
            rotation: Some(RotationPin::Locked),
            ..SkyPins::default()
        });
        assert!(
            locked
                .phenomena(&ctx(noon))
                .iter()
                .any(|p| p.kind == NIGHT_STAR)
        );
    }

    #[test]
    fn night_star_phenomenon_description_matches_the_neighbor_s_own_wording() {
        let s = sky(SkyPins {
            rotation: Some(RotationPin::Locked),
            ..SkyPins::default()
        });
        let seen = s.phenomena(&ctx(0.0));
        for neighbor in &s.system().neighbors {
            assert!(
                seen.iter()
                    .any(|p| p.kind == NIGHT_STAR && p.description == neighbor.night_description())
            );
        }
    }

    #[test]
    fn sky_reports_vary_with_time_and_are_deterministic() {
        let s = sky(SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            moons: Some(MoonsPin::exact(1).unwrap()),
            ..SkyPins::default()
        });
        let day_len = s.calendar().day_length().unwrap().get();
        let noon = s.sky_at(WorldTime {
            day: 10.5 * day_len,
        });
        let night = s.sky_at(WorldTime {
            day: 10.01 * day_len,
        });
        assert_ne!(noon.description, night.description);
        assert_eq!(
            s.sky_at(WorldTime {
                day: 10.5 * day_len,
            })
            .description,
            noon.description
        );
    }

    #[test]
    fn a_locked_night_side_vantage_sees_moons_and_stars_but_no_sun() {
        let s = sky(SkyPins {
            rotation: Some(RotationPin::Locked),
            moons: Some(MoonsPin::exact(1).unwrap()),
            ..SkyPins::default()
        });
        let night = ObserverContext::at_position(
            EntityId(1),
            WorldTime { day: 0.0 },
            GeoCoord {
                latitude: 10.0,
                longitude: 180.0,
            },
        );
        let ph = s.phenomena(&night);
        assert!(
            !ph.iter().any(|p| p.venue == Venue::DaySky),
            "night side must not see the sun"
        );
        assert!(
            ph.iter().any(|p| p.kind == NIGHT_STAR),
            "night side sees the stars"
        );
        assert!(
            ph.iter().any(|p| p.description.contains("moon")),
            "night side sees the moon"
        );
    }

    #[test]
    fn a_locked_day_side_vantage_sees_the_sun_and_no_night_sky() {
        let s = sky(SkyPins {
            rotation: Some(RotationPin::Locked),
            moons: Some(MoonsPin::exact(1).unwrap()),
            ..SkyPins::default()
        });
        let day = ObserverContext::at_position(
            EntityId(1),
            WorldTime { day: 0.0 },
            GeoCoord {
                latitude: 10.0,
                longitude: 0.0,
            },
        );
        let ph = s.phenomena(&day);
        assert!(
            ph.iter().any(|p| p.venue == Venue::DaySky),
            "day side sees the sun"
        );
        assert!(
            !ph.iter().any(|p| p.kind == NIGHT_STAR),
            "day side must not see the stars"
        );
        assert!(
            !ph.iter()
                .any(|p| p.kind == CELESTIAL_BODY && p.description.contains("moon")),
            "day side must not see the moon (though its tide is still felt)"
        );
    }

    #[test]
    fn a_spinning_placed_observer_sees_the_whole_sky() {
        // On a spinning world every body rises and sets, so the sky is whole
        // from any longitude at any hour — nothing is culled.
        let s = sky(SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            moons: Some(MoonsPin::exact(1).unwrap()),
            ..SkyPins::default()
        });
        let obs = ObserverContext::at_position(
            EntityId(1),
            WorldTime { day: 10.5 },
            GeoCoord {
                latitude: 40.0,
                longitude: 25.0,
            },
        );
        let ph = s.phenomena(&obs);
        assert!(ph.iter().any(|p| p.venue == Venue::DaySky), "sun present");
        assert!(
            ph.iter().any(|p| p.kind == NIGHT_STAR),
            "stars present (whole sky)"
        );
        assert!(
            ph.iter().any(|p| p.description.contains("moon")),
            "moon present"
        );
    }

    /// A degenerate moon (P_sid ≥ Y) is unreachable at genesis — the Hill
    /// cap keeps P_sid ≤ ~0.15×Y — but `sky_at` must still describe it
    /// honestly rather than panicking on `Calendar::moon_phase`'s `None`.
    /// Hand-built the same way `render.rs`'s `system_with_moon` and
    /// `calendar.rs`'s `calendar_with_moon` construct their degenerate
    /// fixtures, since genesis itself can never produce one.
    #[test]
    fn degenerate_moon_gets_an_honest_night_sky_line_instead_of_a_panic() {
        use crate::anchor::Anchor;
        use crate::moons::Moon;
        use crate::neighborhood::Neighbor;
        use crate::pins::NeighborClass;
        use crate::star::Star;
        use crate::system::{GenesisOutcome, StarSystem};
        use crate::units::{
            Au, Degrees, EarthMasses, HabitableZone, LightYears, LunarMasses, Megameters,
            SolarLuminosities, SolarMasses, StdDays,
        };

        let system = StarSystem {
            star: Star {
                mass: SolarMasses::new(1.0).unwrap(),
                luminosity: SolarLuminosities::new(1.0).unwrap(),
                class_name: "yellow dwarf".to_string(),
                habitable_zone: HabitableZone::new(Au::new(0.9).unwrap(), Au::new(1.4).unwrap())
                    .unwrap(),
            },
            anchor: Anchor {
                mass: EarthMasses::new(1.0).unwrap(),
                orbit: Au::new(1.0).unwrap(),
                year: StdDays::new(365.25).unwrap(),
                rotation: Rotation::Spinning {
                    day: StdDays::new(1.0).unwrap(),
                },
                obliquity: Degrees::new(0.0).unwrap(),
            },
            moons: vec![Moon {
                mass: LunarMasses::new(1.0).unwrap(),
                distance: Megameters::new(384.4).unwrap(),
                period: StdDays::new(400.0).unwrap(), // P_sid ≥ Y: degenerate
                angular_diameter_rel: 1.0,
                tide_rel: 1.0,
            }],
            neighbors: vec![Neighbor {
                class: NeighborClass::SunLike,
                distance: LightYears(10.0),
                apparent_brightness: 1.0,
                color: "warm yellow".to_string(),
                declination: 0.0,
                right_ascension: 0.0,
            }],
            forcing: crate::forcing::OrbitalForcing {
                obliquity_mean: 0.0,
                obliquity_amp: 0.0,
                obliquity_phase: 0.0,
                ecc_mean: 0.0,
                ecc_amp: 0.0,
                ecc_phase: 0.0,
                precession_phase: 0.0,
                year_phase_offset: 0.0,
                day_phase_offset: 0.0,
                moon_phase_offsets: vec![0.0],
            },
        };
        let sky = GeneratedSky::new(GenesisOutcome {
            system,
            notes: Vec::new(),
        });
        assert!(sky.calendar().moon_phase(StdDays(0.0), 0).is_none());
        // Local day fraction 0.0 falls outside the centered daylight
        // window, so this is night — the branch that used to `.unwrap()`.
        let report = sky.sky_at(WorldTime { day: 0.0 });
        assert!(
            report.description.contains("no phase"),
            "got {}",
            report.description
        );
    }

    /// A hand-built spinning sky with zeroed forcing (except the given
    /// obliquity), a 365.25 d year and a 1 d day — so year phase, local day
    /// and moon phase are all exact functions of `t` with no drawn offsets.
    fn bare_sky(
        obliquity: f64,
        moons: Vec<crate::moons::Moon>,
        neighbors: Vec<Neighbor>,
    ) -> GeneratedSky {
        use crate::anchor::Anchor;
        use crate::star::Star;
        use crate::units::{
            Au, Degrees, EarthMasses, HabitableZone, SolarLuminosities, SolarMasses,
        };
        let moon_phase_offsets = vec![0.0; moons.len()];
        let system = StarSystem {
            star: Star {
                mass: SolarMasses::new(1.0).unwrap(),
                luminosity: SolarLuminosities::new(1.0).unwrap(),
                class_name: "yellow dwarf".to_string(),
                habitable_zone: HabitableZone::new(Au::new(0.9).unwrap(), Au::new(1.4).unwrap())
                    .unwrap(),
            },
            anchor: Anchor {
                mass: EarthMasses::new(1.0).unwrap(),
                orbit: Au::new(1.0).unwrap(),
                year: crate::units::StdDays::new(365.25).unwrap(),
                rotation: Rotation::Spinning {
                    day: crate::units::StdDays::new(1.0).unwrap(),
                },
                obliquity: Degrees::new(obliquity).unwrap(),
            },
            moons,
            neighbors,
            forcing: crate::forcing::OrbitalForcing {
                obliquity_mean: obliquity,
                obliquity_amp: 0.0,
                obliquity_phase: 0.0,
                ecc_mean: 0.0,
                ecc_amp: 0.0,
                ecc_phase: 0.0,
                precession_phase: 0.0,
                year_phase_offset: 0.0,
                day_phase_offset: 0.0,
                moon_phase_offsets,
            },
        };
        GeneratedSky::new(GenesisOutcome {
            system,
            notes: Vec::new(),
        })
    }

    fn luna_like() -> crate::moons::Moon {
        crate::moons::Moon {
            mass: crate::units::LunarMasses::new(1.0).unwrap(),
            distance: crate::units::Megameters::new(384.4).unwrap(),
            period: crate::units::StdDays::new(27.32).unwrap(),
            angular_diameter_rel: 1.0,
            tide_rel: 1.0,
        }
    }

    fn neighbor(class: crate::pins::NeighborClass, color: &str) -> Neighbor {
        Neighbor {
            class,
            distance: crate::units::LightYears(10.0),
            apparent_brightness: 1.0,
            color: color.to_string(),
            declination: 0.0,
            right_ascension: 0.0,
        }
    }

    use crate::neighborhood::Neighbor;

    fn moon_with_period(sidereal_days: f64) -> crate::moons::Moon {
        crate::moons::Moon {
            period: crate::units::StdDays::new(sidereal_days).unwrap(),
            ..luna_like()
        }
    }

    /// SKY-5: the tide is felt, not watched — every moon raises an Ambient
    /// tide phenomenon whose period is half the moon's transit interval
    /// (the bulge is axial: two highs per pass).
    #[test]
    fn every_moon_raises_a_tide_phenomenon_with_the_transit_beat_period() {
        let s = bare_sky(
            0.0,
            vec![luna_like()],
            vec![neighbor(crate::pins::NeighborClass::SunLike, "warm yellow")],
        );
        let ph = s.phenomena(&ctx(0.0));
        let tide = ph.iter().find(|p| p.kind == TIDE).expect("tide phenomenon");
        // Day 1.0, sidereal month 27.32: the moon transits every
        // 1/(1 − 1/27.32) ≈ 1.0380 d; the felt period is half that.
        assert_eq!(tide.period_days, Some(0.52));
        assert_eq!(tide.venue, Venue::Ambient);
        assert!(tide.salience > 0.0 && tide.salience < 1.0);
    }

    /// SKY-5 × SEQ-1: a tidally locked world still feels its tides — from
    /// both hemispheres (Ambient, not culled with the night sky).
    #[test]
    fn locked_worlds_feel_the_tide_on_both_hemispheres() {
        let s = sky(SkyPins {
            rotation: Some(RotationPin::Locked),
            moons: Some(MoonsPin::exact(1).unwrap()),
            ..SkyPins::default()
        });
        assert!(s.phenomena(&ctx(0.0)).iter().any(|p| p.kind == TIDE));
        let day_side = ObserverContext::at_position(
            EntityId(1),
            WorldTime { day: 0.0 },
            GeoCoord {
                latitude: 10.0,
                longitude: 0.0,
            },
        );
        assert!(s.phenomena(&day_side).iter().any(|p| p.kind == TIDE));
    }

    /// SKY-5: with two moons the combined tide swells and slackens on the
    /// alignment beat — half the moons' mutual synodic period, since
    /// alignment and opposition both reinforce the axial bulges.
    #[test]
    fn two_moons_beat_spring_and_neap() {
        let s = bare_sky(
            0.0,
            vec![moon_with_period(13.0), moon_with_period(27.32)],
            vec![neighbor(crate::pins::NeighborClass::SunLike, "warm yellow")],
        );
        let ph = s.phenomena(&ctx(0.0));
        let beat = ph
            .iter()
            .find(|p| p.kind == TIDE && p.description.contains("spring"))
            .expect("spring/neap beat phenomenon");
        // 0.5 / (1/13 − 1/27.32) ≈ 12.40 standard days.
        assert_eq!(beat.period_days, Some(12.4));
        assert_eq!(beat.venue, Venue::Ambient);
    }

    /// SKY-5: one moon gives one tide and no beat — spring/neap needs a
    /// second tidal source.
    #[test]
    fn one_moon_worlds_have_no_spring_neap_beat() {
        let s = bare_sky(
            0.0,
            vec![luna_like()],
            vec![neighbor(crate::pins::NeighborClass::SunLike, "warm yellow")],
        );
        let ph = s.phenomena(&ctx(0.0));
        assert_eq!(ph.iter().filter(|p| p.kind == TIDE).count(), 1);
        assert!(!ph.iter().any(|p| p.description.contains("spring")));
    }

    /// SKY-14: the full window is centered on phase 0.5 (it used to start
    /// there), and the vocabulary runs crescent/quarter/gibbous, not just
    /// waxing/waning.
    #[test]
    fn moon_prose_uses_the_eight_word_vocabulary_with_a_centered_full_window() {
        let s = bare_sky(
            0.0,
            vec![luna_like()],
            vec![neighbor(crate::pins::NeighborClass::SunLike, "warm yellow")],
        );
        // Synodic month = 27.32 × 365.25 / (365.25 − 27.32) ≈ 29.53 d.
        // t = 13.1: phase ≈ 0.4436 — inside the centered full window
        // [7/16, 9/16); local-day fraction 0.1 — night.
        let full = s.sky_at(WorldTime { day: 13.1 });
        assert!(
            full.description.contains("shows its full face"),
            "got {}",
            full.description
        );
        // t = 7.1: phase ≈ 0.2404 — first quarter.
        let quarter = s.sky_at(WorldTime { day: 7.1 });
        assert!(
            quarter.description.contains("shows its first-quarter face"),
            "got {}",
            quarter.description
        );
        // t = 3.05: phase ≈ 0.1033 — waxing crescent.
        let crescent = s.sky_at(WorldTime { day: 3.05 });
        assert!(
            crescent
                .description
                .contains("shows its waxing crescent face"),
            "got {}",
            crescent.description
        );
    }

    /// SKY-14: the night sky names every star's color, counted, in order —
    /// not just `neighbors[0].color` stamped onto all of them.
    #[test]
    fn night_sky_prose_names_every_star_color_not_just_the_brightest() {
        let s = bare_sky(
            0.0,
            Vec::new(),
            vec![
                neighbor(crate::pins::NeighborClass::SunLike, "warm yellow"),
                neighbor(crate::pins::NeighborClass::RedDwarf, "dim red"),
                neighbor(crate::pins::NeighborClass::RedDwarf, "dim red"),
            ],
        );
        let night = s.sky_at(WorldTime { day: 10.1 });
        assert!(
            night
                .description
                .contains("Above, the stars keep their stations: one warm yellow, two dim red."),
            "got {}",
            night.description
        );
    }

    /// SKY-14: season prose is graded — the solstice-adjacent eighths of the
    /// year name the standstill, the rest name the direction.
    #[test]
    fn season_prose_names_the_solstice_standstills() {
        let s = bare_sky(
            23.5,
            Vec::new(),
            vec![neighbor(crate::pins::NeighborClass::SunLike, "warm yellow")],
        );
        let noon = |day: f64| s.sky_at(WorldTime { day }).description;
        // Year phase 0.25 is midsummer (daylight peaks); ±1/16 around each
        // solstice reads as a standstill.
        assert!(
            noon(91.5).contains("near their longest"),
            "got {}",
            noon(91.5)
        );
        assert!(
            noon(274.5).contains("near their shortest"),
            "got {}",
            noon(274.5)
        );
        assert!(
            noon(20.5).contains("The days are growing."),
            "got {}",
            noon(20.5)
        );
        assert!(
            noon(170.5).contains("The days are shrinking."),
            "got {}",
            noon(170.5)
        );
    }
}

/// Phenomenon kind for the annual daylight cycle.
/// type-audit: bare-ok(identifier-text)
pub const SEASONAL_CYCLE: &str = "seasonal-cycle";
/// Phenomenon kind for notable neighbor stars.
/// type-audit: bare-ok(identifier-text)
pub const NIGHT_STAR: &str = "night-star";
/// Phenomenon kind for the tides the moons raise (SKY-5).
/// type-audit: bare-ok(identifier-text)
pub const TIDE: &str = "tide";

fn round2(x: f64) -> f64 {
    (x * 100.0).round() / 100.0
}

/// The eight-word moon-phase vocabulary, one word per eighth of the synodic
/// cycle (SKY-14), indexed by [`phase_eighth`].
const PHASE_WORDS: [&str; 8] = [
    "new",
    "waxing crescent",
    "first-quarter",
    "waxing gibbous",
    "full",
    "waning gibbous",
    "last-quarter",
    "waning crescent",
];

/// Which eighth of a cycle `phase` (in [0,1)) falls in, with windows
/// *centered* on the phases they name: full spans [7/16, 9/16) around 0.5,
/// new wraps around 0 (SKY-14 — the full window used to start at 0.5
/// instead of straddling it). `render.rs`'s phase strip and orrery faces
/// sample these same bands.
pub(crate) fn phase_eighth(phase: f64) -> usize {
    ((phase * 8.0).round() as usize) % 8
}

/// The phase word for an illumination phase (0 = new, 0.5 = full).
fn phase_word(phase: f64) -> &'static str {
    PHASE_WORDS[phase_eighth(phase)]
}

/// A small count as prose; the neighborhood holds 2–5 stars, so anything
/// past five (unreachable from genesis) reads honestly as "many".
fn count_word(n: usize) -> &'static str {
    ["one", "two", "three", "four", "five"]
        .get(n.wrapping_sub(1))
        .copied()
        .unwrap_or("many")
}

/// The night-sky star line: every color represented, counted, in brightness
/// order (SKY-14) — not just the brightest star's color stamped onto all.
fn night_star_line(neighbors: &[crate::neighborhood::Neighbor]) -> String {
    let mut groups: Vec<(&str, usize)> = Vec::new();
    for n in neighbors {
        match groups.iter_mut().find(|(color, _)| *color == n.color) {
            Some((_, count)) => *count += 1,
            None => groups.push((&n.color, 1)),
        }
    }
    let listed: Vec<String> = groups
        .iter()
        .map(|(color, count)| format!("{} {}", count_word(*count), color))
        .collect();
    format!(
        "Above, the stars keep their stations: {}.",
        listed.join(", ")
    )
}

/// Graded season prose (SKY-14): the solstice-adjacent eighths of the year
/// name the standstill (daylight peaks at year phase 0.25); the rest name
/// the direction the days are moving.
fn season_words(phase: f64) -> &'static str {
    match phase_eighth(phase) {
        2 => "The days are near their longest.",
        6 => "The days are near their shortest.",
        _ if (std::f64::consts::TAU * phase).cos() > 0.0 => "The days are growing.",
        _ => "The days are shrinking.",
    }
}

/// Which size word describes a moon of this angular diameter (Luna = 1).
pub(crate) fn size_word(angular: f64) -> &'static str {
    if angular >= 1.2 {
        "vast"
    } else if angular >= 0.7 {
        "full-sized"
    } else {
        "small, distant"
    }
}

/// Capitalize the first character of `s`, leaving the rest untouched.
fn capitalize(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
    }
}

/// Tier-1/2 astronomy: the generated sky, time-varying.
#[derive(Debug, Clone, PartialEq)]
pub struct GeneratedSky {
    system: StarSystem,
    calendar: Calendar,
    notes: Vec<String>,
}

impl GeneratedSky {
    /// Wrap a genesis outcome as a live provider.
    pub fn new(outcome: GenesisOutcome) -> GeneratedSky {
        let calendar = calendar_of(&outcome.system);
        GeneratedSky {
            system: outcome.system,
            calendar,
            notes: outcome.notes,
        }
    }
    /// The underlying system.
    pub fn system(&self) -> &StarSystem {
        &self.system
    }
    /// The derived calendar.
    pub fn calendar(&self) -> &Calendar {
        &self.calendar
    }
    /// Genesis notes recorded during generation.
    /// type-audit: bare-ok(prose)
    pub fn notes(&self) -> &[String] {
        &self.notes
    }

    // Clamps negative time to 0.0; NaN also maps to 0.0 via f64::max
    // semantics — genesis-time queries never predate the world.
    fn t(&self, time: WorldTime) -> StdDays {
        StdDays(time.day.max(0.0))
    }

    /// The sky at a moment, rendered.
    pub fn sky_at(&self, time: WorldTime) -> SkyReport {
        let t = self.t(time);
        let mut bodies = vec!["the sun".to_string()];
        match &self.system.anchor.rotation {
            Rotation::Locked => {
                for i in 0..self.system.moons.len() {
                    bodies.push(format!("moon {}", i + 1));
                }
                let n = self.system.neighbors.len();
                SkyReport {
                    description: format!(
                        "A sun hangs motionless above the day side; the night side lives \
                         beneath {n} unmoving stars."
                    ),
                    bodies,
                }
            }
            Rotation::Spinning { .. } => {
                let is_day = matches!(self.calendar.is_daylight(t), Some(true));
                if is_day {
                    let mut description = format!(
                        "The sun, a {}, stands in the sky.",
                        self.system.star.class_name
                    );
                    if let Some(phase) = self.calendar.season_phase(t) {
                        description.push(' ');
                        description.push_str(season_words(phase));
                    }
                    SkyReport {
                        description,
                        bodies,
                    }
                } else {
                    for i in 0..self.system.moons.len() {
                        bodies.push(format!("moon {}", i + 1));
                    }
                    let mut parts: Vec<String> = self
                        .system
                        .moons
                        .iter()
                        .enumerate()
                        .map(|(index, moon)| match self.calendar.moon_phase(t, index) {
                            Some(phase) => capitalize(&format!(
                                "the {} moon shows its {} face.",
                                size_word(moon.angular_diameter_rel),
                                phase_word(phase)
                            )),
                            // Degenerate: P_sid ≥ Y, unreachable at genesis
                            // (the Hill cap keeps P_sid ≤ ~0.15×Y) but handled
                            // honestly rather than panicking.
                            None => capitalize(&format!(
                                "the {} moon shows no phase — its orbit outpaces the year.",
                                size_word(moon.angular_diameter_rel)
                            )),
                        })
                        .collect();
                    parts.push(night_star_line(&self.system.neighbors));
                    SkyReport {
                        description: format!("Night. {}", parts.join(" ")),
                        bodies,
                    }
                }
            }
        }
    }
}

impl PhenomenaSource for GeneratedSky {
    fn phenomena(&self, ctx: &ObserverContext) -> Vec<Phenomenon> {
        let t = self.t(ctx.time);
        let mut out = Vec::new();

        // Ever-visible hemisphere culling (SEQ-5): a placed observer sees only
        // the bodies that ever rise at their location. On a spinning world the
        // sky turns, so every body rises and sets from every longitude — the
        // whole sky is visible. On a tidally locked world the sky is fixed: the
        // substellar point sits on the prime meridian, so the day hemisphere
        // (|longitude| < 90°) sees only the sun and the night hemisphere only
        // the moons and stars. A position-blind observation (`position == None`)
        // preserves the legacy nowhere-in-particular sky, byte-for-byte: the
        // sun and moons are always present, the neighbor stars only in darkness.
        let locked = matches!(self.system.anchor.rotation, Rotation::Locked);
        let (show_sun, show_moons, show_stars) = match (locked, ctx.position) {
            (true, Some(coord)) => {
                let day_side = coord.longitude.abs() < 90.0;
                (day_side, !day_side, !day_side)
            }
            (false, Some(_)) => (true, true, true),
            (false, None) => (
                true,
                true,
                matches!(self.calendar.is_daylight(t), Some(false)),
            ),
            (true, None) => (true, true, true),
        };

        if show_sun {
            match &self.system.anchor.rotation {
                Rotation::Spinning { day } => out.push(Phenomenon {
                    kind: CELESTIAL_BODY.to_string(),
                    description: format!("the sun, a {}", self.system.star.class_name),
                    period_days: Some(round2(day.get())),
                    salience: 1.0,
                    venue: Venue::DaySky,
                }),
                Rotation::Locked => out.push(Phenomenon {
                    kind: CELESTIAL_BODY.to_string(),
                    description: "a sun fixed forever above the day side".to_string(),
                    period_days: None,
                    salience: 1.0,
                    venue: Venue::DaySky,
                }),
            }
        }

        if show_moons {
            for moon in &self.system.moons {
                let angular = moon.angular_diameter_rel;
                out.push(Phenomenon {
                    kind: CELESTIAL_BODY.to_string(),
                    description: format!("a {} moon", size_word(angular)),
                    period_days: Some(round2(moon.period.get())),
                    salience: round2(0.35 + 0.35 * angular.min(2.0) / 2.0),
                    venue: Venue::NightSky,
                });
            }
        }

        // SKY-5: tides are felt, not watched — Ambient, never culled by
        // hemisphere or daylight. The bulge is axial (two highs per pass),
        // so each moon's felt period is half its transit interval relative
        // to the surface: the local day for a spinning world, the year for
        // a locked one (which turns once per orbit).
        let surface_rotation = match self.system.anchor.rotation {
            Rotation::Spinning { day } => day.get(),
            Rotation::Locked => self.system.anchor.year.get(),
        };
        for moon in &self.system.moons {
            let rate = (1.0 / surface_rotation - 1.0 / moon.period.get()).abs();
            out.push(Phenomenon {
                kind: TIDE.to_string(),
                description: format!(
                    "the tide, rising and falling under the {} moon",
                    size_word(moon.angular_diameter_rel)
                ),
                // A moon synchronous with the surface holds a motionless
                // bulge: a tide with no period.
                period_days: (rate > 0.0).then(|| round2(0.5 / rate)),
                salience: round2(0.25 + 0.25 * moon.tide_rel.min(2.0) / 2.0),
                venue: Venue::Ambient,
            });
        }
        if self.system.moons.len() >= 2 {
            // Spring/neap: the combined tide swells when the two strongest
            // sources align OR oppose (both reinforce an axial bulge), so
            // the beat runs at half their mutual synodic period. Moons sort
            // by tide strength, ties broken by distance — deterministic.
            let mut by_tide: Vec<&crate::moons::Moon> = self.system.moons.iter().collect();
            by_tide.sort_by(|a, b| {
                b.tide_rel
                    .total_cmp(&a.tide_rel)
                    .then(a.distance.get().total_cmp(&b.distance.get()))
            });
            let (first, second) = (by_tide[0], by_tide[1]);
            let beat_rate = (1.0 / first.period.get() - 1.0 / second.period.get()).abs();
            if beat_rate > 0.0 {
                out.push(Phenomenon {
                    kind: TIDE.to_string(),
                    description: "spring and neap: the tides swell and slacken as the moons \
                                  align and part"
                        .to_string(),
                    period_days: Some(round2(0.5 / beat_rate)),
                    // The weaker source sets the modulation depth.
                    salience: round2(0.15 + 0.25 * second.tide_rel.min(2.0) / 2.0),
                    venue: Venue::Ambient,
                });
            }
        }

        let spinning = matches!(self.system.anchor.rotation, Rotation::Spinning { .. });
        if spinning && self.system.anchor.obliquity.get() > 0.0 {
            out.push(Phenomenon {
                kind: SEASONAL_CYCLE.to_string(),
                description: "the slow swelling and shrinking of daylight".to_string(),
                period_days: Some(round2(self.system.anchor.year.get())),
                salience: round2(0.5 * self.system.anchor.obliquity.get() / 35.0),
                venue: Venue::Ambient,
            });
        }

        if show_stars {
            for neighbor in &self.system.neighbors {
                out.push(Phenomenon {
                    kind: NIGHT_STAR.to_string(),
                    description: neighbor.night_description(),
                    period_days: None,
                    salience: round2(
                        (0.1 + 0.1 * (1.0 + neighbor.apparent_brightness).ln()).clamp(0.1, 0.6),
                    ),
                    venue: Venue::NightSky,
                });
            }
        }

        out
    }
}
