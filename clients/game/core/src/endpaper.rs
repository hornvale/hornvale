//! The endpaper: a thin identity strip beneath the spread.
//!
//! This is an identity strip, not a vitals bar. `Snapshot` carries no
//! player vitals at all — no hit points, stamina, hunger, or inventory —
//! by design (The Quire spec §6), so this module draws only what the
//! wire actually names: who the possession is, where it is from, and
//! when.

use crate::{Cell, SelfChannel, Weight};

/// The separator between the endpaper's three clauses.
const SEPARATOR: char = '\u{b7}';

/// Draw the endpaper strip at `origin`, one row: `me.species` of
/// `me.settlement` (population `me.population`), the observed `day`, and
/// the `turn` counter. Every datum here traces to a field `Snapshot`
/// mirrors; nothing is invented.
pub fn draw(me: &SelfChannel, day: f64, turn: u64, into: &mut crate::Grid, origin: (u16, u16)) {
    let line = format!(
        "{} of {} (pop. {}) {SEPARATOR} day {day} {SEPARATOR} turn {turn}",
        me.species, me.settlement, me.population,
    );
    for (i, ch) in line.chars().enumerate() {
        into.set(
            origin.0 + i as u16,
            origin.1,
            Cell::glyph(ch, Weight::Normal),
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn me() -> SelfChannel {
        SelfChannel {
            agent: "1".to_string(),
            species: "bugbear".to_string(),
            settlement: "Googo".to_string(),
            population: 68,
        }
    }

    #[test]
    fn draw_writes_species_settlement_population_day_and_turn() {
        let mut g = crate::Grid::new(60, 1);
        draw(&me(), 3.5, 7, &mut g, (0, 0));
        let text = g.to_plain_text();
        assert!(text.contains("bugbear"));
        assert!(text.contains("Googo"));
        assert!(text.contains("68"));
        assert!(text.contains("day 3.5"));
        assert!(text.contains("turn 7"));
    }

    #[test]
    fn draw_carries_no_vitals() {
        let mut g = crate::Grid::new(60, 1);
        draw(&me(), 0.0, 0, &mut g, (0, 0));
        let text = g.to_plain_text().to_lowercase();
        for forbidden in ["hp", "health", "stamina", "hunger", "inventory"] {
            assert!(
                !text.contains(forbidden),
                "endpaper must never invent a vital: found {forbidden:?}"
            );
        }
    }
}
