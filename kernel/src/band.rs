//! The delve ladder's depth axis, shared by every domain that speaks about it
//! (spec `2026-08-23-the-drift-design.md` §3.3, §4.2).
//!
//! Before this module, one axis carried three names: `DelveRung` in
//! `domains/terrain::delve`, a *mirrored* `DelveZone` in
//! `domains/climate::underworld`, and a bare `u8` rank on
//! `windows/worldgen`'s `ChamberAddr::band`. The climate mirror cited
//! decision 0094 (*a deliberate duplicate shares its roster, never its
//! derivation*), but 0094 is written for duplicates that buy independence —
//! a lab metric re-implementing production rules so the check is not an
//! echo. Climate does not compute a band at all; it cannot **import**
//! terrain (decision 0002, the domain-layering rule). That made the mirror a
//! *forced* duplicate, filed under a decision written for deliberate ones —
//! and decision 0044 clause (a), "a quantity belongs in the kernel when more
//! than one domain speaks it," says where a forced duplicate like that
//! actually goes, now that the kernel is a legal import for both.
//!
//! **This module holds the roster and the ordering, and nothing else.** The
//! ΔT boundaries that decide which band a depth falls in, the habitable
//! ceiling, and every other derivation stay in `hornvale_terrain::delve` —
//! that is 0044's other half: the kernel holds what more than one domain
//! speaks, and each domain keeps its own meaning of it. `Band` is
//! consequently an ordinal roster rather than a Stevens-scale *unit* in
//! 0044's narrower sense — that decision keeps ordinal/nominal scales out of
//! its units library on purpose — and is placed here on the cross-domain
//! clause alone, not as an entry in that vocabulary.
//!
//! [`Band::deeper`], [`Band::shallower`], [`Band::rank`] and
//! [`Band::from_rank`] exist for `ChamberAddr::band`'s later promotion from a
//! bare `u8` rank to this type: `windows/worldgen`'s `passages_from` already
//! steps the ladder with `addr.band + 1` / `addr.band - 1`, and without these
//! helpers the tree cannot compile once that field's type changes.
//! `rank`/`from_rank` also carry the job `windows/worldgen::chamber`'s
//! `rung_rank`/`rung_of_rank` did before this module existed.

/// A rung of the delve ladder — a habitation depth *class*, never a depth in
/// metres. `domains/terrain::delve` derives which band a ΔT falls in;
/// nothing here duplicates that derivation.
///
/// Ordered shallow → deep, so a **greater** band is a **deeper** one, and
/// [`Band::Surface`] is the least. The derived `Ord` is load-bearing in two
/// ways: it makes "further down the ladder" a comparison rather than a
/// convention, and it lets a band serve as half of a `BTreeMap` key (the
/// workspace bans `HashMap`, so a map keyed by band has no other option).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Band {
    /// The overworld — above the rock column entirely, not a ΔT class. No
    /// underworld community occupies it and no habitation rank names it.
    Surface,
    /// Cellars, cave mouths, the first few tens of metres of worked rock.
    Undercroft,
    /// Shallow inhabited depth: the top of the karst and lava-tube population.
    Shallows,
    /// The ladder's broad middle — a worked or walked depth, still temperate.
    Deeps,
    /// Deep habitation, warm enough that living here is a choice with a cost.
    Underdeep,
    /// Past the habitable ceiling: hot, and the deepest class the ladder
    /// models.
    ///
    /// **Named `Sunless` until The Stope renamed it** (spec amendment B.3),
    /// and the rename is a correction rather than a redecoration: "Sunless"
    /// read as the eldritch deep — a promise this band does not make — while
    /// the band means only "past the ΔT at which the ladder stops modelling
    /// habitability". `Nadir` is astronomical vocabulary — the project's
    /// native idiom — and `hornvale_terrain::delve` measures ΔT **above the
    /// surface datum**, so "the lowest point relative to the datum" is
    /// coherent with the ladder's own coordinate. See that module's docs for
    /// how often a world's caves actually reach it, and for why a sixth band
    /// splitting this one was measured and refused.
    Nadir,
}

/// Every band, [`Band::Surface`] first, shallowest to deepest.
const ALL: [Band; 6] = [
    Band::Surface,
    Band::Undercroft,
    Band::Shallows,
    Band::Deeps,
    Band::Underdeep,
    Band::Nadir,
];

/// Every habitation band — [`ALL`] minus [`Band::Surface`] — shallowest to
/// deepest.
const HABITATION: [Band; 5] = [
    Band::Undercroft,
    Band::Shallows,
    Band::Deeps,
    Band::Underdeep,
    Band::Nadir,
];

impl Band {
    /// Every band of the ladder in order, [`Band::Surface`] first.
    ///
    /// Callers that want only the habitation bands use [`Band::habitation`];
    /// `Surface` is deliberately present here so that iterating "the ladder"
    /// never silently omits the overworld.
    pub fn all() -> &'static [Band] {
        &ALL
    }

    /// Every habitation band — [`Band::all`] minus [`Band::Surface`] —
    /// shallowest to deepest.
    pub fn habitation() -> &'static [Band] {
        &HABITATION
    }

    /// The next band down, or `None` at [`Band::Nadir`] — the bottom of the
    /// ladder.
    pub fn deeper(self) -> Option<Band> {
        match self {
            Band::Surface => Some(Band::Undercroft),
            Band::Undercroft => Some(Band::Shallows),
            Band::Shallows => Some(Band::Deeps),
            Band::Deeps => Some(Band::Underdeep),
            Band::Underdeep => Some(Band::Nadir),
            Band::Nadir => None,
        }
    }

    /// The next band up, or `None` at [`Band::Surface`] — the top of the
    /// ladder.
    pub fn shallower(self) -> Option<Band> {
        match self {
            Band::Surface => None,
            Band::Undercroft => Some(Band::Surface),
            Band::Shallows => Some(Band::Undercroft),
            Band::Deeps => Some(Band::Shallows),
            Band::Underdeep => Some(Band::Deeps),
            Band::Nadir => Some(Band::Underdeep),
        }
    }

    /// This band's zero-based rank among the **habitation** bands
    /// ([`Band::habitation`]), or `None` for [`Band::Surface`], which is not
    /// a habitation band and has no rank.
    /// type-audit: bare-ok(index: return)
    pub fn rank(self) -> Option<u8> {
        match self {
            Band::Surface => None,
            Band::Undercroft => Some(0),
            Band::Shallows => Some(1),
            Band::Deeps => Some(2),
            Band::Underdeep => Some(3),
            Band::Nadir => Some(4),
        }
    }

    /// The habitation band at a given [`Band::rank`], or `None` past the
    /// bottom of the ladder.
    /// type-audit: bare-ok(index: rank)
    pub fn from_rank(rank: u8) -> Option<Band> {
        match rank {
            0 => Some(Band::Undercroft),
            1 => Some(Band::Shallows),
            2 => Some(Band::Deeps),
            3 => Some(Band::Underdeep),
            4 => Some(Band::Nadir),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The ladder's ORDER is load-bearing: "deeper than" is a comparison, and
    /// both `hornvale_terrain::delve` and `hornvale_climate::underworld`
    /// relied on it before this roster was shared.
    #[test]
    fn the_ladder_is_ordered_surface_first_and_habitation_excludes_it() {
        let all = Band::all();
        assert_eq!(all.first(), Some(&Band::Surface));
        assert!(
            all.windows(2).all(|w| w[0] < w[1]),
            "the ladder must be ascending"
        );
        assert_eq!(all.len(), Band::habitation().len() + 1);
        assert!(!Band::habitation().contains(&Band::Surface));
    }

    /// `deeper`/`shallower` are exact inverses over the whole ladder,
    /// including `Surface`, which is a band of the ladder and not a hole in
    /// it.
    #[test]
    fn deeper_and_shallower_are_inverses_over_the_whole_ladder() {
        for &band in Band::all() {
            if let Some(down) = band.deeper() {
                assert_eq!(down.shallower(), Some(band));
            }
            if let Some(up) = band.shallower() {
                assert_eq!(up.deeper(), Some(band));
            }
        }
        assert_eq!(Band::Nadir.deeper(), None);
        assert_eq!(Band::Surface.shallower(), None);
    }

    /// `rank`/`from_rank` are exact inverses over the habitation bands, and
    /// `Surface` is outside both — no rank names it and it names no rank.
    #[test]
    fn rank_and_from_rank_round_trip_over_habitation_only() {
        for &band in Band::habitation() {
            let rank = band.rank().expect("a habitation band has a rank");
            assert_eq!(Band::from_rank(rank), Some(band));
        }
        assert_eq!(Band::Surface.rank(), None);
        assert_eq!(Band::from_rank(5), None, "the ladder ends at rank 4");
    }

    /// The derived order agrees with the ranks: a higher rank is a deeper
    /// band.
    #[test]
    fn rank_rises_with_depth() {
        let ranked: Vec<u8> = Band::habitation()
            .iter()
            .map(|b| b.rank().unwrap())
            .collect();
        assert!(ranked.windows(2).all(|w| w[0] < w[1]));
    }
}
