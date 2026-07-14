//! The generated sky: a time-varying phenomena provider (tier 1/2) driven
//! entirely by a genesis outcome. Downstream systems see only `sky_at` and
//! `phenomena` — never the system or calendar directly.

use crate::anchor::Rotation;
use crate::calendar::{Calendar, calendar_of};
use crate::system::{GenesisOutcome, StarSystem};
use crate::units::StdDays;
use crate::{CELESTIAL_BODY, SkyReport};
use hornvale_kernel::math;
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
                    retrograde: false,
                },
                obliquity: Degrees::new(0.0).unwrap(),
            },
            moons: vec![Moon {
                mass: LunarMasses::new(1.0).unwrap(),
                distance: Megameters::new(384.4).unwrap(),
                period: StdDays::new(400.0).unwrap(), // P_sid ≥ Y: degenerate
                angular_diameter_rel: 1.0,
                tide_rel: 1.0,
                inclination_deg: 5.14,
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
                    retrograde: false,
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
            inclination_deg: 5.14,
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

    /// SKY-6: a moon whose disc covers the sun's promises total eclipses
    /// on the node beat — Luna-and-Sol numbers reproduce Earth's rate
    /// (~19% of new moons → one solar eclipse somewhere every ~153 days).
    #[test]
    fn a_covering_moon_promises_total_eclipses_on_the_node_beat() {
        // bare_sky's star is Sol-like at 1 AU: sun angular diameter 1.0;
        // luna_like covers it (1.0 ≥ 1.0) at inclination 5.14°.
        let s = bare_sky(
            0.0,
            vec![luna_like()],
            vec![neighbor(crate::pins::NeighborClass::SunLike, "warm yellow")],
        );
        let ph = s.phenomena(&ctx(0.0));
        let eclipse = ph
            .iter()
            .find(|p| p.kind == ECLIPSE)
            .expect("eclipse phenomenon");
        assert!(
            eclipse.description.contains("devours the sun whole"),
            "got {}",
            eclipse.description
        );
        let period = eclipse.period_days.expect("eclipses recur");
        assert!(
            (150.0..157.0).contains(&period),
            "expected the Earth-like ~153 d beat, got {period}"
        );
        assert_eq!(eclipse.venue, Venue::DaySky);
        assert!(eclipse.salience < 1.0, "nothing rivals the sun");
    }

    /// SKY-6: a small, distant moon cannot cover the sun — its eclipses
    /// are rings, and they matter less.
    #[test]
    fn a_small_moon_leaves_a_burning_ring() {
        let small = crate::moons::Moon {
            angular_diameter_rel: 0.5,
            ..luna_like()
        };
        let s = bare_sky(
            0.0,
            vec![small],
            vec![neighbor(crate::pins::NeighborClass::SunLike, "warm yellow")],
        );
        let ph = s.phenomena(&ctx(0.0));
        let eclipse = ph.iter().find(|p| p.kind == ECLIPSE).expect("eclipse");
        assert!(
            eclipse.description.contains("burning ring"),
            "got {}",
            eclipse.description
        );
        assert!(eclipse.salience < 0.9, "a ring is a lesser omen");
    }

    /// SKY-6: a flat (coplanar) orbit crosses the sun at every new moon —
    /// the eclipse beat IS the synodic month.
    #[test]
    fn a_flat_orbit_eclipses_every_new_moon() {
        let flat = crate::moons::Moon {
            inclination_deg: 0.0,
            ..luna_like()
        };
        let s = bare_sky(
            0.0,
            vec![flat],
            vec![neighbor(crate::pins::NeighborClass::SunLike, "warm yellow")],
        );
        let ph = s.phenomena(&ctx(0.0));
        let eclipse = ph.iter().find(|p| p.kind == ECLIPSE).expect("eclipse");
        // Synodic month of a 27.32 d moon in a 365.25 d year ≈ 29.53 d.
        assert_eq!(eclipse.period_days, Some(29.53));
    }

    /// SKY-6 × SEQ-5: eclipses are seen where the sun is seen — a locked
    /// world's day side gets the omen, its night side never does.
    #[test]
    fn locked_day_side_sees_the_eclipse_and_night_side_does_not() {
        let s = sky(SkyPins {
            rotation: Some(RotationPin::Locked),
            moons: Some(MoonsPin::exact(1).unwrap()),
            ..SkyPins::default()
        });
        let at = |longitude: f64| {
            let obs = ObserverContext::at_position(
                EntityId(1),
                WorldTime { day: 0.0 },
                GeoCoord {
                    latitude: 10.0,
                    longitude,
                },
            );
            s.phenomena(&obs)
        };
        assert!(
            at(0.0).iter().any(|p| p.kind == ECLIPSE),
            "day side sees the eclipse"
        );
        assert!(
            !at(180.0).iter().any(|p| p.kind == ECLIPSE),
            "night side never sees the sun devoured"
        );
    }

    /// SKY-17: daylight carries the star's color — a K star's light is
    /// warm and dim, an F star's harsh and blue-edged, a G star's golden —
    /// and twilight takes the matching hue.
    #[test]
    fn daylight_and_twilight_carry_the_stars_color() {
        assert!(daylight_words("orange dwarf (K)").contains("amber"));
        assert!(daylight_words("yellow dwarf (G)").contains("golden"));
        assert!(daylight_words("yellow-white dwarf (F)").contains("blue"));
        assert!(twilight_words("orange dwarf (K)").contains("amber"));
        assert!(twilight_words("yellow dwarf (G)").contains("gold"));
        assert!(twilight_words("yellow-white dwarf (F)").contains("blue"));

        // Through the report: the bare test sky is a G — golden noon,
        // gold-glowing twilight.
        let s = bare_sky(
            0.0,
            Vec::new(),
            vec![neighbor(crate::pins::NeighborClass::SunLike, "warm yellow")],
        );
        let noon = s.sky_at(WorldTime { day: 10.5 }).description;
        assert!(noon.contains("The light is golden."), "got {noon}");
        let dusk = s.sky_at(WorldTime { day: 10.78 }).description;
        assert!(dusk.contains("The horizon glows gold."), "got {dusk}");
        let night = s.sky_at(WorldTime { day: 10.1 }).description;
        assert!(!night.contains("horizon"), "night takes no hue: {night}");
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

    /// SKY-7: the day sky tells morning from noon from evening, and the
    /// edges of night read as twilight rather than full dark.
    #[test]
    fn day_prose_tells_morning_from_noon_from_evening_and_twilight() {
        // Zero tilt: the daylight window is exactly (0.25, 0.75) of the
        // local day, so the day-part bands are hand-checkable.
        let s = bare_sky(
            0.0,
            Vec::new(),
            vec![neighbor(crate::pins::NeighborClass::SunLike, "warm yellow")],
        );
        let at = |t: f64| s.sky_at(WorldTime { day: t }).description;
        assert!(
            at(10.30).contains("climbs the morning sky"),
            "got {}",
            at(10.30)
        );
        assert!(at(10.50).contains("stands high"), "got {}", at(10.50));
        assert!(
            at(10.70).contains("sinks toward evening"),
            "got {}",
            at(10.70)
        );
        assert!(at(10.22).starts_with("Twilight."), "got {}", at(10.22));
        assert!(at(10.78).starts_with("Twilight."), "got {}", at(10.78));
        assert!(at(10.10).starts_with("Night."), "got {}", at(10.10));
    }

    /// SKY-22: a retrograde world's day sky says the sun rises in the west;
    /// an ordinary world's does not.
    #[test]
    fn retrograde_worlds_see_the_sun_rise_in_the_west() {
        let noon_sky = |spin| {
            let pins = SkyPins {
                rotation: Some(RotationPin::PeriodHours(24.0)),
                forcing: Some(crate::pins::ForcingPin::Zero),
                spin: Some(spin),
                ..SkyPins::default()
            };
            sky(pins).sky_at(WorldTime { day: 10.5 }).description
        };
        let retro = noon_sky(crate::pins::SpinPin::Retrograde);
        assert!(retro.contains("rises in the west"), "got {retro}");
        let pro = noon_sky(crate::pins::SpinPin::Prograde);
        assert!(!pro.contains("rises in the west"), "got {pro}");
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

    /// Night-sky stage 1: a placed observer on a spinning world sees a
    /// heliacal rising somewhere across a year, and every heliacal
    /// phenomenon reads below the SKY-23 top-salience invariant.
    #[test]
    fn a_placed_observer_sees_a_heliacal_rising_across_a_year() {
        let s = sky(SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            ..SkyPins::default()
        });
        let year = s.system().anchor.year.get();
        let mut found_rising = false;
        for k in 0..365 {
            let obs = ObserverContext::at_position(
                EntityId(1),
                WorldTime {
                    day: k as f64 * year / 365.0,
                },
                GeoCoord {
                    latitude: 35.0,
                    longitude: 0.0,
                },
            );
            for p in s.phenomena(&obs) {
                if p.kind == HELIACAL_RISING {
                    found_rising = true;
                }
                if p.kind == HELIACAL_RISING || p.kind == HELIACAL_SETTING {
                    assert!(p.salience < 1.0, "heliacal salience must stay subordinate");
                }
            }
        }
        assert!(
            found_rising,
            "a year at latitude 35 must show at least one heliacal rising"
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
/// Phenomenon kind for a moon crossing the face of the sun (SKY-6).
/// type-audit: bare-ok(identifier-text)
pub const ECLIPSE: &str = "eclipse";
/// Phenomenon kind for a star's heliacal rising (night-sky stage 1).
/// type-audit: bare-ok(identifier-text)
pub const HELIACAL_RISING: &str = "heliacal-rising";
/// Phenomenon kind for a star's heliacal setting (night-sky stage 1).
/// type-audit: bare-ok(identifier-text)
pub const HELIACAL_SETTING: &str = "heliacal-setting";

/// One angular-diameter unit (Sol from 1 AU ≈ Luna from Earth) in degrees
/// — the shared scale of `sun_angular_diameter_rel` and a moon's
/// `angular_diameter_rel` (declared approximation: the two units differ
/// by under 1%).
const ANGULAR_UNIT_DEG: f64 = 0.53;
/// How far (degrees of lunar ecliptic latitude) past the discs' own touch
/// an eclipse still falls somewhere on the world — the parallax allowance.
/// Calibrated so a Luna–Sol pair at 5.14° inclination eclipses at ~19% of
/// new moons (Earth's ~2.4 solar eclipses a year).
const ECLIPSE_PARALLAX_DEG: f64 = 1.0;

/// The fraction of new moons that eclipse the sun somewhere on the world
/// (SKY-6): the moon's ecliptic latitude at new moon must fall inside the
/// node threshold, and a sinusoidal latitude distribution gives
/// P = (2/π)·asin(threshold / i), saturating at 1 for a flat orbit.
fn eclipse_chance(sun_angular_rel: f64, moon_angular_rel: f64, inclination_deg: f64) -> f64 {
    let threshold =
        ANGULAR_UNIT_DEG * (sun_angular_rel + moon_angular_rel) / 2.0 + ECLIPSE_PARALLAX_DEG;
    let x = (threshold / inclination_deg.max(f64::MIN_POSITIVE)).min(1.0);
    (2.0 / std::f64::consts::PI) * math::asin(x)
}

fn round2(x: f64) -> f64 {
    (x * 100.0).round() / 100.0
}

/// How far outside the daylight window (as a fraction of the local day)
/// the dark still reads as twilight rather than night (SKY-7). The coarse,
/// place-less tier of the shared twilight concept: placed consumers read
/// [`Calendar::sky_band`] (the altitude-based [`crate::calendar::SkyBand`]
/// thresholds); this margin serves only the position-blind prose path.
const TWILIGHT_MARGIN: f64 = 0.05;

/// The quality of a star's daylight from its spectral class (SKY-17):
/// a cooler star pours warmer, dimmer light; a hotter one a harsher
/// white. Keyed on the class letter the star's name already carries
/// (`star.rs` — K, G or F on the shipped mass range).
fn daylight_words(class_name: &str) -> &'static str {
    if class_name.contains("(K)") {
        "The light is amber and heavy, warmer than it is bright."
    } else if class_name.contains("(F)") {
        "The light is hard and white, edged with blue."
    } else {
        "The light is golden."
    }
}

/// The twilight hue from the star's spectral class (SKY-17) — the same
/// key as [`daylight_words`], seen at the horizon.
fn twilight_words(class_name: &str) -> &'static str {
    if class_name.contains("(K)") {
        "The horizon smolders amber."
    } else if class_name.contains("(F)") {
        "The horizon shines pale blue-white."
    } else {
        "The horizon glows gold."
    }
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
        _ if math::cos(std::f64::consts::TAU * phase) > 0.0 => "The days are growing.",
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
            Rotation::Spinning { retrograde, .. } => {
                // SKY-7: place the moment in the sun's arc. The daylight
                // window is centered on the local day (`is_daylight`); the
                // fraction's progress through it separates morning from
                // noon from evening, and a margin either side of the
                // window reads as twilight rather than full dark.
                let fraction = self.calendar.local_day(t).map(|d| d.1).unwrap_or(0.0);
                let f = self.calendar.daylight_fraction(t).unwrap_or(0.5);
                let (dawn, dusk) = ((1.0 - f) / 2.0, (1.0 + f) / 2.0);
                let is_day = fraction > dawn && fraction < dusk;
                if is_day {
                    let progress = (fraction - dawn) / f;
                    let arc_words = if progress < 1.0 / 3.0 {
                        "climbs the morning sky"
                    } else if progress < 2.0 / 3.0 {
                        "stands high in the sky"
                    } else {
                        "sinks toward evening"
                    };
                    let mut description = format!(
                        "The sun, a {}, {}. {}",
                        self.system.star.class_name,
                        arc_words,
                        daylight_words(&self.system.star.class_name)
                    );
                    if *retrograde {
                        description.push_str(" Here it rises in the west and sets in the east.");
                    }
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
                    // SKY-7: within a twentieth of the local day of the
                    // daylight window, the dark is twilight, not night.
                    let twilight =
                        fraction > dawn - TWILIGHT_MARGIN && fraction < dusk + TWILIGHT_MARGIN;
                    // SKY-17: twilight takes the star's hue at the horizon.
                    let dark_words = if twilight {
                        format!("Twilight. {}", twilight_words(&self.system.star.class_name))
                    } else {
                        "Night.".to_string()
                    };
                    SkyReport {
                        description: format!("{} {}", dark_words, parts.join(" ")),
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
                Rotation::Spinning { day, .. } => out.push(Phenomenon {
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

            // SKY-6: eclipses — pure geometry over quantities already
            // held. Each moon with a synodic cycle crosses the sun on the
            // node beat: total if its disc covers the sun's (SKY-7 gave
            // the sun a disc), else a ring. Seen where the sun is seen.
            let sun_angular =
                crate::star::sun_angular_diameter_rel(&self.system.star, self.system.anchor.orbit);
            for (index, moon) in self.system.moons.iter().enumerate() {
                let Some(synodic) = self.calendar.synodic_month(index) else {
                    continue;
                };
                let chance =
                    eclipse_chance(sun_angular, moon.angular_diameter_rel, moon.inclination_deg);
                let total = moon.angular_diameter_rel >= sun_angular;
                let (description, salience) = if total {
                    (
                        format!(
                            "an eclipse: the {} moon devours the sun whole",
                            size_word(moon.angular_diameter_rel)
                        ),
                        0.9,
                    )
                } else {
                    (
                        format!(
                            "an eclipse: the {} moon leaves a burning ring of the sun",
                            size_word(moon.angular_diameter_rel)
                        ),
                        0.75,
                    )
                };
                out.push(Phenomenon {
                    kind: ECLIPSE.to_string(),
                    description,
                    period_days: Some(round2(synodic.get() / chance)),
                    salience,
                    venue: Venue::DaySky,
                });
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
            Rotation::Spinning { day, .. } => day.get(),
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
                        (0.1 + 0.1 * math::ln(1.0 + neighbor.apparent_brightness)).clamp(0.1, 0.6),
                    ),
                    venue: Venue::NightSky,
                });
            }
        }

        // Heliacal risings/settings (night-sky stage 1): only meaningful for
        // a placed observer on a spinning world (a locked world has no
        // horizon-crossing local day for the star to rise or set against).
        if let (true, Some(pos), Some(day_length)) =
            (spinning, ctx.position, self.calendar.day_length())
        {
            let year = self.system.anchor.year.get();
            let half_day_frac = 0.5 * day_length.get() / year;
            let phase = self.calendar.year_phase(t);
            let wrapped_dist = |a: f64, b: f64| {
                let d = (a - b).rem_euclid(1.0);
                d.min(1.0 - d)
            };
            for pair in
                crate::heliacal::heliacal_events(&self.system, &self.calendar, pos.latitude, t)
            {
                let color = &self.system.neighbors[pair.neighbor].color;
                if wrapped_dist(phase, pair.rising_frac) < half_day_frac {
                    out.push(Phenomenon {
                        kind: HELIACAL_RISING.to_string(),
                        description: format!("The {} star returns before dawn.", color),
                        period_days: Some(round2(year)),
                        salience: 0.6,
                        venue: Venue::NightSky,
                    });
                }
                if wrapped_dist(phase, pair.setting_frac) < half_day_frac {
                    out.push(Phenomenon {
                        kind: HELIACAL_SETTING.to_string(),
                        description: format!("The {} star takes its leave into the sunset.", color),
                        period_days: Some(round2(year)),
                        salience: 0.6,
                        venue: Venue::NightSky,
                    });
                }
            }
        }

        out
    }
}
