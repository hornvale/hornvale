//! Which vent, if any, a settled place depends on — and what that vent is
//! doing at a named instant (The Tidemark, Task 5; spec §4).
//!
//! Spec §4: "A vent entering `Failed` under an occupied vertex should
//! therefore end that occupation with `Ended::Nature` and a cause, not leave a
//! settlement silently persisting on a dead vent." This module is the index
//! that makes *under* answerable from inside [`crate::history_bake`], which
//! imports no terrain, holds no overlay, and reasons in bake years rather than
//! instants.
//!
//! # "Under" is the candidate RING, not the present position
//!
//! A vent's influence reaches exactly one vertex at a time — the one
//! [`crate::waterworld::vent_position_at`] selects — and `select_vent_position`
//! returns `None` for an `Absent` or `Failed` source. So a failed vent is
//! *nowhere*, and asking "which vertex is this failed vent under?" of the
//! present position is asking a question with no answer by construction.
//!
//! The ring is the set of vertices a vent can ever light, it is fixed for the
//! life of the world ([`crate::waterworld::WaterWorld::vent_candidate_rings`]
//! is stable source data, built once), and it is therefore the honest domain
//! of "this place depends on that vent". A community seated anywhere in the
//! ring was seated on ground the vent's chemistry reaches; when the source
//! fails, that is what has gone out from under it.
//!
//! # A vertex may sit in more than one ring, and the rule is ALL, not ANY
//!
//! Rings are built from a seabed anchor plus its marine neighbours
//! (`build_vent_candidate_ring`), so two nearby vents can overlap. A place
//! two vents can light does not go dark when one of them fails — the other is
//! still there — so [`VentTenancy::failed_at`] requires **every** hosting vent
//! to be `Failed`. `any` would have ended occupations that still had a live
//! source, which is the model saying something it does not believe.
//!
//! # The year→instant crossing lives here, once
//!
//! The bake reasons in years and the succession is a function of
//! [`WorldTime`]. Rather than letting each caller spell that crossing, it is
//! [`VentTenancy::instant_of_bake_year`]: the same
//! [`crate::history_emit::ledger_day_of_bake_year`] every other bake-year
//! stamp goes through, then [`WorldTime::from_std_days`], **which rounds to
//! the nearest tick** — named here because the kernel requires the rounding
//! rule be named at the crossing.
//!
//! # What this costs the bake, and what it does NOT
//!
//! One `BTreeMap` lookup per living community per epoch, plus exact integer
//! arithmetic per hosting vent. No globe pass, no draw, no allocation. An
//! empty tenancy — [`VentTenancy::default`], which is what every bake without
//! an overlay gets — answers `false` on the first line, so this is an exact
//! no-op for a world with no sea in it.

use std::collections::BTreeMap;

use hornvale_kernel::{Vertex, WorldTime};

use crate::waterworld::{VentState, WaterVent, WaterWorld, vent_state_at};

/// The vents each vertex depends on, and their succession, read at an instant.
///
/// Cloned off the overlay rather than borrowed, so it can ride
/// [`crate::history_bake::BakeConfig`] the way the authored species maps
/// already do. A [`WaterVent`] is four `f64`s, a `usize` and an `i64`; seed 42
/// admits a few hundred of them.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct VentTenancy {
    /// Every admitted source, in the overlay's own stable order.
    vents: Vec<WaterVent>,
    /// Vertex → the indices into `vents` whose candidate ring contains it,
    /// ascending. A vertex no vent can ever light is absent.
    hosts: BTreeMap<Vertex, Vec<usize>>,
}

impl VentTenancy {
    /// Index `water`'s vents by every vertex of their candidate rings.
    ///
    /// An overlay with no vents — including the empty one a world with no
    /// ocean produces, and the ablated one `WaterWorldConfig { enabled: false }`
    /// produces — yields an empty tenancy, which is inert.
    #[must_use]
    pub fn from_overlay(water: &WaterWorld) -> Self {
        assert_eq!(
            water.vents.len(),
            water.vent_candidate_rings.len(),
            "Waterworld vents and candidate rings must remain aligned"
        );
        let mut hosts: BTreeMap<Vertex, Vec<usize>> = BTreeMap::new();
        for (index, ring) in water.vent_candidate_rings.iter().enumerate() {
            for &vertex in ring {
                hosts.entry(vertex).or_default().push(index);
            }
        }
        VentTenancy {
            vents: water.vents.clone(),
            hosts,
        }
    }

    /// How many sources this tenancy indexes — the emptiness test a caller
    /// needs to know whether an answer of `false` means anything.
    /// type-audit: bare-ok(count: return)
    #[must_use]
    pub fn vent_count(&self) -> usize {
        self.vents.len()
    }

    /// How many vertices any vent can ever light.
    /// type-audit: bare-ok(count: return)
    #[must_use]
    pub fn hosted_vertex_count(&self) -> usize {
        self.hosts.len()
    }

    /// Whether `site` sits in some vent's candidate ring at all.
    /// type-audit: bare-ok(flag: return)
    #[must_use]
    pub fn is_hosted(&self, site: Vertex) -> bool {
        self.hosts.contains_key(&site)
    }

    /// Whether every vent that can light `site` is [`VentState::Failed`] at
    /// `time` — the condition spec §4 ends an occupation on.
    ///
    /// `false` for an unhosted vertex, which is every land vertex and every
    /// stretch of open water no source reaches.
    /// type-audit: bare-ok(flag: return)
    #[must_use]
    pub fn failed_at(&self, site: Vertex, time: WorldTime) -> bool {
        let Some(indices) = self.hosts.get(&site) else {
            return false;
        };
        indices
            .iter()
            .all(|&i| vent_state_at(&self.vents[i], time) == VentState::Failed)
    }

    /// The succession states of every vent hosting `site` at `time`, in the
    /// overlay's own vent order. Empty for an unhosted vertex.
    ///
    /// The diagnostic read: [`failed_at`](VentTenancy::failed_at) collapses
    /// this to one bit, and an observation that names an inferred phase needs
    /// the phases themselves.
    #[must_use]
    pub fn states_at(&self, site: Vertex, time: WorldTime) -> Vec<VentState> {
        self.hosts
            .get(&site)
            .map(|indices| {
                indices
                    .iter()
                    .map(|&i| vent_state_at(&self.vents[i], time))
                    .collect()
            })
            .unwrap_or_default()
    }

    /// The bake year `year` as an exact instant.
    ///
    /// Goes through [`crate::history_emit::ledger_day_of_bake_year`] — the
    /// same crossing every committed bake-year stamp takes, so a vent phase
    /// and an `occ-founded` day derive from one reading of the year — and then
    /// [`WorldTime::from_std_days`], **which ROUNDS to the nearest tick**. One
    /// tick is 0.864 s against a 100-day succession cycle, so the rounding
    /// cannot move a phase boundary that any caller could observe; it is named
    /// because the kernel requires a rounding rule be named at its crossing,
    /// not because it is in doubt.
    ///
    /// # Panics
    ///
    /// If `year` does not name a representable instant. A bake year is
    /// finite and bounded by `BakeConfig::start_year`/`end_year`, so this is
    /// the loud failure the kernel prefers to a silent clamp.
    /// type-audit: bare-ok(count: year)
    #[must_use]
    pub fn instant_of_bake_year(year: f64) -> WorldTime {
        WorldTime::from_std_days(crate::history_emit::ledger_day_of_bake_year(year))
            .unwrap_or_else(|e| panic!("bake year {year} must name a representable instant: {e:?}"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// `Vertex(u32)` — the ring members this module indexes.
    fn v(n: u32) -> Vertex {
        Vertex(n)
    }

    /// A vent whose succession is placed by its phase offset alone. At
    /// `GENESIS` the shifted tick IS the offset, so an offset inside a state's
    /// own interval puts the vent in that state: the intervals are 20 days
    /// absent, 15 nascent, 30 active, 20 weakening, 15 failed.
    fn vent(id: usize, anchor: u32, offset_days: i64) -> WaterVent {
        WaterVent {
            id,
            vertex: v(anchor),
            strength: 1.0,
            temperature_delta: 50.0,
            chemistry: 1.0,
            phase_offset_ticks: offset_days * WorldTime::TICKS_PER_STD_DAY,
        }
    }

    fn overlay(vents: Vec<WaterVent>, rings: Vec<Vec<Vertex>>) -> WaterWorld {
        WaterWorld {
            vents,
            vent_candidate_rings: rings,
            ..WaterWorld::default()
        }
    }

    #[test]
    fn an_empty_overlay_hosts_nothing_and_fails_nothing() {
        let tenancy = VentTenancy::from_overlay(&WaterWorld::default());
        assert_eq!(tenancy.vent_count(), 0);
        assert_eq!(tenancy.hosted_vertex_count(), 0);
        assert!(!tenancy.is_hosted(v(1)));
        assert!(!tenancy.failed_at(v(1), WorldTime::GENESIS));
    }

    #[test]
    fn a_failed_source_fails_every_vertex_of_its_own_ring() {
        // 90 days into the cycle is inside the failed interval (85..100).
        let water = overlay(vec![vent(0, 7, 90)], vec![vec![v(6), v(7), v(8)]]);
        let tenancy = VentTenancy::from_overlay(&water);
        assert_eq!(tenancy.hosted_vertex_count(), 3);
        for vertex in [v(6), v(7), v(8)] {
            assert!(tenancy.is_hosted(vertex));
            assert!(
                tenancy.failed_at(vertex, WorldTime::GENESIS),
                "a failed source takes its whole ring with it, not only its anchor"
            );
        }
        assert!(
            !tenancy.failed_at(v(9), WorldTime::GENESIS),
            "a vertex outside every ring is not hosted and cannot be failed"
        );
    }

    #[test]
    fn a_live_source_fails_nothing() {
        // 40 days in is inside the active interval (35..65).
        let water = overlay(vec![vent(0, 7, 40)], vec![vec![v(6), v(7)]]);
        let tenancy = VentTenancy::from_overlay(&water);
        assert_eq!(
            tenancy.states_at(v(7), WorldTime::GENESIS),
            vec![VentState::Active]
        );
        assert!(!tenancy.failed_at(v(7), WorldTime::GENESIS));
    }

    /// The `all`-not-`any` rule, in both directions. A vertex two vents can
    /// light does not go dark while one of them still burns.
    #[test]
    fn an_overlapping_vertex_needs_every_hosting_source_to_have_failed() {
        let water = overlay(
            vec![vent(0, 7, 90), vent(1, 8, 40)],
            vec![vec![v(7), v(8)], vec![v(8), v(9)]],
        );
        let tenancy = VentTenancy::from_overlay(&water);
        assert_eq!(
            tenancy.states_at(v(8), WorldTime::GENESIS),
            vec![VentState::Failed, VentState::Active],
            "vertex 8 sits in both rings and the two sources are in different phases"
        );
        assert!(
            !tenancy.failed_at(v(8), WorldTime::GENESIS),
            "one live source is enough to keep a shared vertex lit"
        );
        assert!(
            tenancy.failed_at(v(7), WorldTime::GENESIS),
            "vertex 7 is hosted only by the failed source"
        );
        assert!(
            !tenancy.failed_at(v(9), WorldTime::GENESIS),
            "vertex 9 is hosted only by the live source"
        );
    }

    /// The succession really is read at the instant it is asked about, so a
    /// bake sampling different years gets different answers off one tenancy.
    #[test]
    fn the_same_source_answers_differently_at_different_instants() {
        let water = overlay(vec![vent(0, 7, 0)], vec![vec![v(7)]]);
        let tenancy = VentTenancy::from_overlay(&water);
        let at = |days: i64| {
            tenancy.states_at(
                v(7),
                WorldTime::from_ticks(days * WorldTime::TICKS_PER_STD_DAY),
            )[0]
        };
        assert_eq!(at(0), VentState::Absent);
        assert_eq!(at(25), VentState::Nascent);
        assert_eq!(at(40), VentState::Active);
        assert_eq!(at(70), VentState::Weakening);
        assert_eq!(at(90), VentState::Failed);
        assert_eq!(at(100), VentState::Absent, "the cycle repeats");
    }

    /// The year crossing is monotone and lands where the ledger's own day
    /// stamp does — one reading of the year, not two.
    #[test]
    fn the_bake_year_crossing_agrees_with_the_ledger_day() {
        for year in [0.0, 1.0, 25.0, 137.5, 2000.0] {
            let days = crate::history_emit::ledger_day_of_bake_year(year);
            assert_eq!(
                VentTenancy::instant_of_bake_year(year),
                WorldTime::from_std_days(days).expect("a bake year is representable"),
                "the crossing must be the ledger's own day stamp, not a second derivation"
            );
        }
        assert!(
            VentTenancy::instant_of_bake_year(0.0) < VentTenancy::instant_of_bake_year(25.0),
            "later bake years must be later instants"
        );
    }
}
