//! Which eyes a possession sees through, and the light they see by (The
//! Beholding, Task 4). Two small, independent questions: [`resolve`] answers
//! "whose eye" and [`daylight_at`] answers "under what light" — a possession
//! composes both into a coloured [`hornvale_scene::SurroundsScene`], but
//! neither knows the other exists.

use crate::Agent;
use hornvale_astronomy::{Calendar, StdDays};
use hornvale_kernel::color::{BANDS, Illuminant, Observer};
use hornvale_kernel::{World, WorldTime};

/// Whose eyes a possession's chart is coloured through.
///
/// `Default` is [`Eyes::Own`]: colour is on by default, through the
/// possessed agent's own species — the campaign's headline claim is that a
/// possession sees the world as its own kind does, not as a human narrator
/// would, so the quiet default must already do that rather than requiring an
/// opt-in.
/// type-audit: bare-ok(identifier-text: Named.0)
#[derive(Debug, Clone, PartialEq, Default)]
pub enum Eyes {
    /// See through the possessed agent's own species.
    #[default]
    Own,
    /// See through a named observer (`"standard"`, or a species' own
    /// [`hornvale_species::KindId`] label) — Task 5's `eyes <name>` verb.
    Named(String),
    /// Decline the observer step entirely: no colour, no `sight`
    /// declaration. The same posture a screen reader takes toward an image —
    /// withholding a channel is not the same as rendering it grey.
    Off,
}

/// Resolve `eyes` against `agent` to the [`Observer`] a chart should be
/// coloured through and the name [`hornvale_scene::Sight::observer`] should
/// carry. `None` for [`Eyes::Off`] (decline the observer step) and for a
/// [`Eyes::Named`] name [`hornvale_worldgen::observer::observer_named`] does
/// not recognize — generation never guesses (spec §4.6): an unknown name
/// colours nothing rather than falling back to a default eye.
/// type-audit: bare-ok(identifier-text: return)
pub fn resolve(eyes: &Eyes, agent: &Agent) -> Option<(Observer, String)> {
    match eyes {
        Eyes::Own => Some((
            hornvale_worldgen::observer::observer_for(&agent.perception),
            agent.species.clone(),
        )),
        Eyes::Named(name) => hornvale_worldgen::observer::observer_named(name)
            .map(|observer| (observer, name.clone())),
        Eyes::Off => None,
    }
}

/// A flat, colourless illuminant — every band at unit weight. The fallback
/// light for a world with no calendar (a tier-0 constant sun has no solar
/// geometry to place a real sun by), so [`daylight_at`] never needs a star
/// it cannot honestly place.
fn flat_illuminant() -> Illuminant {
    Illuminant::new([1.0; BANDS]).expect("a unit illuminant is finite and non-negative")
}

/// The light at `latitude` on `day`, and the sun altitude (degrees) that
/// produced it — returned together so a caller records the number the light
/// was actually built from, rather than re-deriving it a second time. Two
/// independent copies of one calculation is how a caption and a picture end
/// up disagreeing.
///
/// With a calendar, this is the world's own star's daylight
/// ([`hornvale_astronomy::daylight`]), reddened for the real sun altitude at
/// `latitude` and `day` ([`Calendar::solar_altitude_at`]). Without one — a
/// tier-0 constant-sun world, or a day [`StdDays`] rejects — there is no
/// solar geometry to read, so this falls back to a flat, altitude-zero light
/// rather than attenuating a real star's spectrum by a placement it cannot
/// justify.
/// type-audit: bare-ok(diagnostic-value: latitude), bare-ok(diagnostic-value: return)
pub fn daylight_at(
    world: &World,
    calendar: Option<&Calendar>,
    day: WorldTime,
    latitude: f64,
) -> (Illuminant, f64) {
    let altitude = calendar.and_then(|cal| {
        StdDays::new(day.day)
            .ok()
            .and_then(|t| cal.solar_altitude_at(t, latitude))
    });
    let base = match altitude {
        Some(_) => {
            let star = hornvale_astronomy::generate_star(
                world.seed.derive(hornvale_astronomy::streams::ROOT),
            );
            hornvale_astronomy::daylight(&star)
        }
        None => flat_illuminant(),
    };
    let altitude = altitude.unwrap_or(0.0);
    (hornvale_astronomy::at_elevation(&base, altitude), altitude)
}
