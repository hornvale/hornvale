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
            .filter(|p| p.description.contains("moon"))
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
            !ph.iter().any(|p| p.description.contains("moon")),
            "day side must not see the moon"
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
}

/// Phenomenon kind for the annual daylight cycle.
/// type-audit: bare-ok(identifier-text)
pub const SEASONAL_CYCLE: &str = "seasonal-cycle";
/// Phenomenon kind for notable neighbor stars.
/// type-audit: bare-ok(identifier-text)
pub const NIGHT_STAR: &str = "night-star";

fn round2(x: f64) -> f64 {
    (x * 100.0).round() / 100.0
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
                        let derivative = (std::f64::consts::TAU * phase).cos();
                        if derivative > 0.0 {
                            description.push_str(" The days are growing.");
                        } else {
                            description.push_str(" The days are shrinking.");
                        }
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
                            Some(phase) => {
                                let phase_word = if !(0.125..0.875).contains(&phase) {
                                    "new"
                                } else if phase < 0.5 {
                                    "waxing"
                                } else if phase < 0.625 {
                                    "full"
                                } else {
                                    "waning"
                                };
                                capitalize(&format!(
                                    "the {} moon shows its {} face.",
                                    size_word(moon.angular_diameter_rel),
                                    phase_word
                                ))
                            }
                            // Degenerate: P_sid ≥ Y, unreachable at genesis
                            // (the Hill cap keeps P_sid ≤ ~0.15×Y) but handled
                            // honestly rather than panicking.
                            None => capitalize(&format!(
                                "the {} moon shows no phase — its orbit outpaces the year.",
                                size_word(moon.angular_diameter_rel)
                            )),
                        })
                        .collect();
                    parts.push(format!(
                        "Above, {} stars keep their stations.",
                        self.system.neighbors[0].color
                    ));
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
