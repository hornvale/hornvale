//! The ways-on line: a dedicated, always-visible element naming which
//! directions are open from here — restored from the authoritative wire
//! channel (`sensed.room.exits` outdoors; the chamber `at`/`of` pair
//! indoors), never parsed out of prose. See `cell.rs`'s `Source::WaysOn`
//! doc for the history of why this is a *restoration*, not a new idea.
//!
//! ## Two bands, two distinct sources — deliberately not one
//!
//! Outdoors, `sensed.room.exits` really is the ways-on: filter to
//! `ExitKind::Edge` + `Direction::Compass`, exactly the filter
//! `Session::ways()` applies (`windows/vessel/src/session.rs`), which is
//! also exactly what builds the outdoor "Ways on:" prose sentence
//! (`Session::describe_here`). Both fixtures' compass output was verified
//! against that same sentence in `tests/ways.rs`.
//!
//! Indoors, `sensed.room` is *still the outdoor locale the structure stands
//! in* — every committed fixture's `sensed.room.exits` is byte-identical
//! between its walk-band and chamber-band turn, because entering a
//! structure does not move the agent's outdoor position. Filtering those
//! exits while indoors would show the country outside the walls (compass
//! letters), not the chamber the possession is actually standing in — the
//! wrong list entirely, not merely an approximation of the right one.
//!
//! The chamber band's real ways-on is fixed by construction rather than
//! named on the wire as its own field. Read from the producer
//! (`windows/vessel/src/session.rs`'s `describe_chamber_here` and
//! `further_in`, and `windows/vessel/src/structure.rs`'s own doc): a
//! structure's `links` are ALWAYS a path graph in depth order — exactly the
//! pairs `(i - 1, i)` for `chambers[i]` — so chamber `at`'s only possible
//! deeper neighbour is `at + 1`, present iff `at + 1 < of`. `out` (leave
//! the whole structure, not one chamber) is always offered; `further in` is
//! offered exactly when that neighbour exists. That is precisely
//! `plan.at + 1 < plan.of`, which is why [`Plan::at`](crate::Plan::at) and
//! [`Plan::of`](crate::Plan::of) are mirrored at all — this crate carries no
//! `hornvale-vessel` dependency (the containment rule), so this is a
//! reimplementation of a known-correct rule, not a call into one, the same
//! move `chart.rs` makes for the lattice projection.

use crate::{Cell, Compass, Direction, ExitKind, Sensed, Source, Spatial, Weight};

/// The chamber band's fixed "leave the structure" label — always offered.
const OUT: &str = "out";

/// The chamber band's fixed "go deeper" label — offered only when a deeper
/// chamber exists.
const FURTHER_IN: &str = "further in";

/// The label for one compass exit — the same transform `Session::ways()`
/// and `Session::describe_here` make when they build the outdoor "Ways on:"
/// sentence: the Rust variant name (`Ne`), uppercased (`NE`).
fn compass_label(c: Compass) -> &'static str {
    match c {
        Compass::N => "N",
        Compass::Ne => "NE",
        Compass::E => "E",
        Compass::Se => "SE",
        Compass::S => "S",
        Compass::Sw => "SW",
        Compass::W => "W",
        Compass::Nw => "NW",
    }
}

/// Compute the ways-on list for this snapshot, one label per open way, in
/// the same order the sim's own prose sentence lists them (the walk band
/// preserves `sensed.room.exits`' own order, exactly as `Session::ways()`
/// does — see this module's doc for the chamber band's fixed pair).
pub fn ways_on(sensed: &Sensed, spatial: &Spatial) -> Vec<String> {
    match spatial {
        Spatial::Walk { .. } => sensed
            .room
            .exits
            .iter()
            .filter(|e| e.kind == ExitKind::Edge)
            .filter_map(|e| match e.direction {
                Direction::Compass(c) => Some(compass_label(c).to_string()),
                _ => None,
            })
            .collect(),
        Spatial::Chamber { plan } => {
            let mut ways = vec![OUT.to_string()];
            if plan.at + 1 < plan.of {
                ways.push(FURTHER_IN.to_string());
            }
            ways
        }
    }
}

/// Draw the ways-on line into `into`, starting at `(x0, y)`: `"Ways on: "`
/// followed by [`ways_on`]'s list, comma-joined, with a trailing period —
/// the same sentence shape the sim's own prose uses, so a player who reads
/// both sees one convention rather than two (they are not the same channel,
/// only the same wording). Every glyph is attributed to [`Source::WaysOn`].
///
/// Columns past `into`'s own bounds are silently refused by
/// [`crate::Grid::set`] (the same discipline `entry.rs::write_line` and
/// `endpaper::draw` already rely on) rather than wrapped or clipped here —
/// this line is short on every fixture this campaign ships (at most a
/// handful of compass letters, or `out, further in`) and never needs to
/// wrap in the 80-column row [`crate::spread::compose`] gives it.
pub fn draw(sensed: &Sensed, spatial: &Spatial, into: &mut crate::Grid, x0: u16, y: u16) {
    let ways = ways_on(sensed, spatial);
    let line = format!("Ways on: {}.", ways.join(", "));
    for (i, ch) in line.chars().enumerate() {
        into.set(
            x0 + i as u16,
            y,
            Cell::glyph(ch, Weight::Normal, Source::WaysOn),
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Exit, Plan, PlanExtent, PlanPoint, Room};

    fn exit(direction: Direction, kind: ExitKind) -> Exit {
        Exit { direction, kind }
    }

    fn sensed_with_exits(exits: Vec<Exit>) -> Sensed {
        Sensed {
            room: Room { exits },
            sky: String::new(),
        }
    }

    fn walk() -> Spatial {
        Spatial::Walk {
            chart: crate::Chart {
                radius: 0,
                depth: 0,
                biome_legend: vec![],
                water_legend: vec![],
                relief_legend: vec![],
                cells: vec![],
                legend: vec![],
            },
        }
    }

    fn chamber(at: usize, of: usize) -> Spatial {
        Spatial::Chamber {
            plan: Plan {
                at,
                of,
                extent: PlanExtent {
                    x: 0,
                    y: 0,
                    w: 1,
                    h: 1,
                },
                palette: vec![],
                cells: vec![0],
                you: PlanPoint { x: 0, y: 0 },
                marks: vec![],
            },
        }
    }

    /// THE FILTER TEST: a mix of every `Direction`/`ExitKind` combination
    /// the wire can carry. Only the `Edge` + `Compass` pair may survive; if
    /// the filter is dropped (e.g. every exit is rendered, or `kind` is
    /// ignored), this fails by picking up `Enter`/`Exit` directions or a
    /// `Vertical`-kinded compass exit that should not exist on real data but
    /// must still be excluded defensively.
    #[test]
    fn ways_on_keeps_only_edge_compass_exits() {
        let sensed = sensed_with_exits(vec![
            exit(Direction::Compass(Compass::Ne), ExitKind::Edge),
            exit(Direction::Compass(Compass::S), ExitKind::Edge),
            exit(Direction::Exit, ExitKind::Vertical),
            exit(Direction::Enter(0), ExitKind::Vertical),
            exit(Direction::Enter(1), ExitKind::Vertical),
        ]);
        assert_eq!(ways_on(&sensed, &walk()), vec!["NE", "S"]);
    }

    /// An exit whose `kind` is `Vertical` must be excluded even when its
    /// `direction` is `Compass` — the filter is `kind == Edge`, not merely
    /// "is this a compass direction." Not a shape real `windows/locale`
    /// emits today, but the filter must not assume it can't happen.
    #[test]
    fn a_compass_direction_with_a_vertical_kind_is_excluded() {
        let sensed = sensed_with_exits(vec![exit(
            Direction::Compass(Compass::N),
            ExitKind::Vertical,
        )]);
        assert!(ways_on(&sensed, &walk()).is_empty());
    }

    /// Order is preserved, not sorted — matching `Session::ways()`'s own
    /// `.iter()....collect()`, which never reorders.
    #[test]
    fn ways_on_preserves_wire_order() {
        let sensed = sensed_with_exits(vec![
            exit(Direction::Compass(Compass::S), ExitKind::Edge),
            exit(Direction::Compass(Compass::N), ExitKind::Edge),
        ]);
        assert_eq!(ways_on(&sensed, &walk()), vec!["S", "N"]);
    }

    /// A middle chamber (a deeper neighbour exists) offers both fixed ways.
    #[test]
    fn a_middle_chamber_offers_out_and_further_in() {
        let sensed = sensed_with_exits(vec![]);
        assert_eq!(ways_on(&sensed, &chamber(0, 4)), vec!["out", "further in"]);
    }

    /// The deepest chamber (no neighbour above `at`) offers only `out` —
    /// this is what would break if the `at + 1 < of` arithmetic were
    /// flipped or off-by-one.
    #[test]
    fn the_deepest_chamber_offers_only_out() {
        let sensed = sensed_with_exits(vec![]);
        assert_eq!(ways_on(&sensed, &chamber(3, 4)), vec!["out"]);
    }

    /// The chamber band must NEVER read `sensed.room.exits` — a fixture
    /// where the outdoor locale happens to carry compass exits (as every
    /// real one does; entering a structure does not move the outdoor
    /// position) must still show the fixed out/further-in pair, not compass
    /// letters. This is the regression this module's doc calls out: reusing
    /// the walk-band filter indoors would show the wrong list entirely.
    #[test]
    fn the_chamber_band_ignores_compass_exits_on_the_shared_outdoor_room() {
        let sensed = sensed_with_exits(vec![
            exit(Direction::Compass(Compass::Ne), ExitKind::Edge),
            exit(Direction::Compass(Compass::Nw), ExitKind::Edge),
            exit(Direction::Compass(Compass::S), ExitKind::Edge),
        ]);
        assert_eq!(ways_on(&sensed, &chamber(0, 4)), vec!["out", "further in"]);
    }

    /// THE PROVENANCE ASSERTION: every glyph the ways-on line draws must be
    /// attributed to `Source::WaysOn`, not left `Unattributed` and not
    /// folded into `Source::Chrome` (the prompt's channel) or
    /// `Source::Prose` (the entry's channel) — the three ways a cell could
    /// silently lose its trace.
    #[test]
    fn draw_attributes_every_glyph_to_ways_on() {
        let sensed = sensed_with_exits(vec![exit(Direction::Compass(Compass::N), ExitKind::Edge)]);
        let mut g = crate::Grid::new(20, 1);
        draw(&sensed, &walk(), &mut g, 0, 0);
        let drawn: Vec<&Cell> = (0..20)
            .filter_map(|x| g.get(x, 0))
            .filter(|c| !c.is_blank())
            .collect();
        assert!(!drawn.is_empty(), "the line must draw something");
        for cell in drawn {
            assert_eq!(cell.source, Source::WaysOn);
        }
    }

    #[test]
    fn draw_writes_the_expected_sentence() {
        let sensed = sensed_with_exits(vec![
            exit(Direction::Compass(Compass::Ne), ExitKind::Edge),
            exit(Direction::Compass(Compass::Nw), ExitKind::Edge),
            exit(Direction::Compass(Compass::S), ExitKind::Edge),
        ]);
        let width = 40u16;
        let mut g = crate::Grid::new(width, 1);
        draw(&sensed, &walk(), &mut g, 0, 0);
        let expected_line = "Ways on: NE, NW, S.";
        let padded = format!(
            "{expected_line}{}",
            " ".repeat(width as usize - expected_line.chars().count())
        );
        assert_eq!(g.to_plain_text(), padded);
    }
}
