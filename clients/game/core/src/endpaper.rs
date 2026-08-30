//! The endpaper: a thin identity strip beneath the spread.
//!
//! This is an identity strip, not a vitals bar: it draws who the
//! possession is, where it is from, and when — and nothing else, because
//! one row is what it is.
//!
//! **This module doc has been wrong twice, and both corrections are kept
//! here rather than tidied away**, because the sentence it started from is
//! the sentence a future reader will reach for. It read: *"`Snapshot`
//! carries no player vitals at all — no hit points, stamina, hunger, or
//! inventory — by design (The Quire spec §6)."*
//!
//! The CITATION was false (The Chattel, Task 13): The Quire's spec
//! contains the word `inventory` zero times, and its §6 is about the
//! drive-across-the-linker / read-across-the-serializer seam. The rule
//! this list obeys was never written in a spec at all.
//!
//! The CLASSIFICATION was false too, and the FIRST correction got its
//! reason wrong (Task 13, fix round). That correction said hit points,
//! stamina and hunger "belong together because the sim commits no such
//! quantity". Two of the three are folds over committed events, in the
//! very crate that builds this snapshot: `windows/vessel/src/liveness.rs`
//! carries `fatigue_at` ("FATIGUE == FOLD", over `rested`) and `hunger_at`
//! ("HUNGER == FOLD", folding `eaten`), with `RESTED` and `EATEN` as
//! registered predicates. Only **hit points** genuinely lack a model, by
//! decision 0070's own construction — wounds commit, vitality folds, and
//! no stored health value exists anywhere.
//!
//! So the honest statement is narrower and does not depend on a taxonomy:
//! **this strip is one row about identity, and none of these four is
//! identity.** What the wire carries is a separate question with a
//! separate answer — custody rides it as `self.carrying` (decision 0400),
//! because `located-in` naming the body as its object is an ordinary
//! committed fact with an entity on each end, saved with the world and
//! read back by three verbs; no version moved to admit it. Decision 0070
//! had already put inventory in one family with drive, belief, affect and
//! health, which is the citation 0400 owed and did not make.
//!
//! `draw_carries_no_vitals`'s forbidden-word list keeps `"inventory"` in
//! it, and that assertion is about THIS ROW — never a claim about what
//! `Snapshot` carries.

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
