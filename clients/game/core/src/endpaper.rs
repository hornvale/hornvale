//! The endpaper: a thin identity strip beneath the spread.
//!
//! This is an identity strip, not a vitals bar. `Snapshot` carries no
//! player vitals at all — no hit points, stamina, hunger — by design, so
//! this module draws only what the wire actually names: who the
//! possession is, where it is from, and when.
//!
//! **That sentence used to end "…, or inventory — by design (The Quire
//! spec §6)", and both halves of the ending were wrong** (The Chattel,
//! Task 13, correcting this module rather than quietly deleting it).
//!
//! The CITATION was false: The Quire's spec contains the word `inventory`
//! zero times, and its §6 is about the drive-across-the-linker /
//! read-across-the-serializer seam. The rule this list obeys was never
//! written there.
//!
//! The CLASSIFICATION was false too, and it is the half that matters.
//! Hit points, stamina and hunger belong together because the sim commits
//! no such quantity — `CLIENT-vitality-folds` is explicit that wounds
//! commit and health folds, and there is no HP counter anywhere — so a
//! strip printing one would be **inventing** it. Custody is the opposite
//! kind of thing: `located-in` naming the body as its object is an
//! ordinary committed fact with an entity on each end, saved with the
//! world and read back by three verbs. It rides the wire as
//! `self.carrying` (decision 0400) and no version moved to admit it.
//!
//! This strip still draws none of it, for its own reason and not that
//! one: it is an identity strip, one row, and what a body is holding is
//! not who it is. `draw_carries_no_vitals`'s forbidden-word list keeps
//! `"inventory"` in it, and that assertion is about THIS ROW — never a
//! claim about what `Snapshot` carries.

use crate::{Cell, SelfChannel, Source, Weight};

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
            Cell::glyph(ch, Weight::Normal, Source::Identity),
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

    /// `"inventory"` stays in this list, and it asserts about THIS ROW
    /// only — the strip is an identity strip and what a body holds is not
    /// who it is. It is not a claim about the wire: since decision 0400 the
    /// snapshot carries `self.carrying`, and a future pane that draws it
    /// would be reading a committed fact rather than inventing a vital. See
    /// this module's own doc.
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
