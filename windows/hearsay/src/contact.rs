//! The raid seam, as a horizontal edge in the transmission graph.
//!
//! Spec §5.3. Every model before this one walked parent->child down the
//! founding tree and nothing else, so 97.67% of accounts reached exactly one
//! people and none ever reached three. `occ-ended-by` records one event both
//! parties attended; this reads it as an undirected edge.
//!
//! **Undirected is a freeze, not a discovery** (spec §5.3). Asserting that
//! news flows only one way across a raid would be authoring, which decision
//! 0021 forbids for exactly this kind of asymmetry. It is also the ceiling, so
//! a directed variant (`KNOW-directed-contact`) is a restriction measurable
//! against this campaign's numbers.

use hornvale_kernel::ledger::{EntityId, Ledger, Value};
use std::collections::BTreeMap;

/// Whether transmission may leave the founding tree.
///
/// **Deliberately no `Default` impl** — see [`crate::stance::Perpetration`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Contact {
    /// Ships today: parent->child only.
    Descent,
    /// Descent plus an undirected edge between a victim and its named attacker.
    WithRaidSeam,
}

impl Contact {
    /// Every arm, in a fixed order so a readout's columns are stable.
    pub const ALL: [Contact; 2] = [Contact::Descent, Contact::WithRaidSeam];

    /// This arm's short name, used as a readout column suffix.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn label(self) -> &'static str {
        match self {
            Contact::Descent => "descent",
            Contact::WithRaidSeam => "contact",
        }
    }
}

/// Who met whom, and on what day.
#[derive(Clone, Debug, Default)]
pub struct ContactGraph {
    /// occupation -> (peer, day of the raid that put them in contact),
    /// ascending. Undirected: every raid writes both directions.
    peers: BTreeMap<EntityId, Vec<(EntityId, f64)>>,
}

impl ContactGraph {
    /// Everyone `occ` has been in contact with, ascending, each paired with
    /// the day of the raid that established it. Empty when `occ` never met
    /// anybody.
    /// type-audit: bare-ok(count: return)
    pub fn peers_of(&self, occ: EntityId) -> &[(EntityId, f64)] {
        self.peers.get(&occ).map_or(&[], Vec::as_slice)
    }

    /// How many undirected edges this graph holds — one per raid that named
    /// an `Entity`-valued attacker, assuming no two raids share both an
    /// unordered victim/attacker pair and a day (unreachable from today's
    /// bake; `dedup` would silently collapse such a pair into one edge).
    /// type-audit: bare-ok(count: return)
    pub fn edges(&self) -> usize {
        self.peers.values().map(Vec::len).sum::<usize>() / 2
    }
}

/// Read the raid seam out of a ledger.
///
/// One undirected edge per ending carrying an `Entity`-valued `occ-ended-by`
/// and a `Number`-valued `occ-ended`. An ending missing either contributes
/// nothing: a raid with no day cannot be time-gated, and admitting it ungated
/// would smuggle spec §5.3's clock condition out through the back door.
pub fn contact_of(ledger: &Ledger) -> ContactGraph {
    let mut out = ContactGraph::default();
    for fact in ledger.find(hornvale_history::OCC_ENDED) {
        let victim = fact.subject;
        let Some(Value::Number(day)) = ledger.value_of(victim, hornvale_history::OCC_ENDED) else {
            continue;
        };
        let Some(Value::Entity(attacker)) = ledger.value_of(victim, hornvale_history::OCC_ENDED_BY)
        else {
            continue;
        };
        let (day, attacker) = (*day, *attacker);
        if attacker == victim {
            continue; // a community cannot meet itself
        }
        out.peers.entry(victim).or_default().push((attacker, day));
        out.peers.entry(attacker).or_default().push((victim, day));
    }
    // Ascending and deduplicated, so the walk is deterministic and one raid
    // is never traversed twice.
    for peers in out.peers.values_mut() {
        peers.sort_by(|a, b| a.0.cmp(&b.0).then(a.1.total_cmp(&b.1)));
        peers.dedup();
    }
    out
}
