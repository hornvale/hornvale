//! The capability schema and the derived execution schedule (metaplan §4.6).
//! A `System` declares the predicates it reads and writes; the schedule is the
//! topological order of the resulting data-dependency DAG, tie-broken by stable
//! label — a derived view over the declarations, never serialized. Genesis is
//! this schedule's first turn (tick 0); the running tick is deferred.
use std::collections::{BTreeMap, BTreeSet};

/// One system's capability declaration: a stable label plus the predicate
/// names it reads and writes. A declaration, not a behavior (the runnable
/// interface is `TickSystem`). The schedulable unit UNI-21 names.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct System {
    /// Stable id; the schedule's tie-break key (never registration position).
    pub label: &'static str,
    /// Predicate names this system reads.
    pub reads: BTreeSet<&'static str>,
    /// Predicate names this system writes.
    pub writes: BTreeSet<&'static str>,
}

impl System {
    /// Declare a system from slices (deduped into the sets).
    /// type-audit: bare-ok(identifier-text)
    pub fn new(label: &'static str, reads: &[&'static str], writes: &[&'static str]) -> System {
        System {
            label,
            reads: reads.iter().copied().collect(),
            writes: writes.iter().copied().collect(),
        }
    }
}

/// The systems' declarations — the capability schema. Build-state, never
/// serialized; the schedule and the single-writer check are pure functions of it.
#[derive(Clone, Debug, Default)]
pub struct CapabilitySchema {
    /// The declared systems.
    pub systems: Vec<System>,
}

/// Why a schedule could not be derived.
/// type-audit: bare-ok(identifier-text)
#[derive(Debug, PartialEq, Eq)]
pub enum ScheduleError {
    /// The read/write DAG has a cycle; no total order exists. Names the
    /// systems remaining when Kahn's algorithm stalled.
    Cycle {
        /// The labels caught in (or downstream of) the cycle.
        labels: Vec<&'static str>,
    },
    /// A functional predicate is written by more than one system.
    MultipleWriters {
        /// The over-written functional predicate.
        predicate: &'static str,
        /// The systems declaring it in `writes`, ascending by label.
        systems: Vec<&'static str>,
    },
}

impl CapabilitySchema {
    /// Construct from declarations.
    pub fn new(systems: Vec<System>) -> Self {
        CapabilitySchema { systems }
    }

    /// The dependency edges `writer_label -> reader_label` for every predicate a
    /// system writes and another reads. Deterministic (BTree-ordered).
    fn edges(&self) -> BTreeSet<(&'static str, &'static str)> {
        let mut edges = BTreeSet::new();
        for w in &self.systems {
            for r in &self.systems {
                if w.label == r.label {
                    continue;
                }
                if w.writes.iter().any(|p| r.reads.contains(p)) {
                    edges.insert((w.label, r.label));
                }
            }
        }
        edges
    }

    /// Topological order, ties broken by ascending label (Kahn's algorithm with
    /// a stable-label ready set). `Err(Cycle)` if no total order exists.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn schedule(&self) -> Result<Vec<&'static str>, ScheduleError> {
        let edges = self.edges();
        let mut indegree: BTreeMap<&'static str, usize> =
            self.systems.iter().map(|s| (s.label, 0usize)).collect();
        for (_, to) in &edges {
            *indegree.get_mut(to).expect("edge endpoints are systems") += 1;
        }
        let mut order = Vec::with_capacity(self.systems.len());
        loop {
            // ready = indegree 0, not yet emitted; BTreeMap iteration is label-sorted
            let next = indegree.iter().find(|&(_, &d)| d == 0).map(|(&l, _)| l);
            let Some(label) = next else { break };
            indegree.remove(&label);
            order.push(label);
            for (from, to) in &edges {
                if *from == label
                    && let Some(d) = indegree.get_mut(to)
                {
                    *d -= 1;
                }
            }
        }
        if order.len() != self.systems.len() {
            let mut labels: Vec<&'static str> = indegree.keys().copied().collect();
            labels.sort_unstable();
            return Err(ScheduleError::Cycle { labels });
        }
        Ok(order)
    }

    /// True iff `order` is a permutation of the systems' labels that respects
    /// every dependency edge (writer before reader). The keystone's checker.
    /// type-audit: bare-ok(identifier-text: order), bare-ok(flag: return)
    pub fn is_valid_order(&self, order: &[&'static str]) -> bool {
        let want: BTreeSet<&'static str> = self.systems.iter().map(|s| s.label).collect();
        let got: BTreeSet<&'static str> = order.iter().copied().collect();
        if want != got || order.len() != self.systems.len() {
            return false;
        }
        let pos: BTreeMap<&'static str, usize> =
            order.iter().enumerate().map(|(i, &l)| (l, i)).collect();
        self.edges().iter().all(|(from, to)| pos[from] < pos[to])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sys(label: &'static str, reads: &[&'static str], writes: &[&'static str]) -> System {
        System::new(label, reads, writes)
    }

    #[test]
    fn schedule_orders_a_chain() {
        // a writes P, b reads P and writes Q, c reads Q -> a,b,c
        let schema = CapabilitySchema::new(vec![
            sys("c", &["Q"], &[]),
            sys("a", &[], &["P"]),
            sys("b", &["P"], &["Q"]),
        ]);
        assert_eq!(schema.schedule().unwrap(), vec!["a", "b", "c"]);
    }

    #[test]
    fn independent_systems_break_ties_by_label() {
        // no edges: pure label order, NOT input order
        let schema = CapabilitySchema::new(vec![
            sys("terrain", &[], &["elev"]),
            sys("sky", &[], &["sun"]),
        ]);
        assert_eq!(schema.schedule().unwrap(), vec!["sky", "terrain"]);
    }

    #[test]
    fn diamond_respects_all_edges_and_ties_by_label() {
        // root -> {left, right} -> join. left/right independent -> label order.
        let schema = CapabilitySchema::new(vec![
            sys("join", &["L", "R"], &[]),
            sys("right", &["base"], &["R"]),
            sys("left", &["base"], &["L"]),
            sys("root", &[], &["base"]),
        ]);
        let order = schema.schedule().unwrap();
        assert_eq!(order, vec!["root", "left", "right", "join"]);
    }

    #[test]
    fn a_cycle_is_an_error_naming_its_systems() {
        // a reads Q writes P; b reads P writes Q -> cycle
        let schema =
            CapabilitySchema::new(vec![sys("a", &["Q"], &["P"]), sys("b", &["P"], &["Q"])]);
        match schema.schedule() {
            Err(ScheduleError::Cycle { labels }) => {
                assert!(labels.contains(&"a") && labels.contains(&"b"));
            }
            other => panic!("expected Cycle, got {other:?}"),
        }
    }

    #[test]
    fn is_valid_order_accepts_topo_sorts_and_rejects_violations() {
        let schema = CapabilitySchema::new(vec![sys("a", &[], &["P"]), sys("b", &["P"], &[])]);
        assert!(schema.is_valid_order(&["a", "b"])); // edge a->b respected
        assert!(!schema.is_valid_order(&["b", "a"])); // edge violated
        assert!(!schema.is_valid_order(&["a"])); // not a permutation
    }
}
