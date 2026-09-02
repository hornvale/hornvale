//! The Avowal's provision table (decision 0576): which of the three
//! capability homes (spec §2.1 — the ledger, the component layer, and
//! session state) is declared to serve a corpus token, and whether that
//! home actually serves it right now.
//!
//! `tropes::resolve` used to ask the `ConceptRegistry` alone whether a token
//! was present. That is blind to a capability the world genuinely has but
//! that lives anywhere but the ledger — `windows/sentiment`'s
//! `snap_judgment`, say, which is kind-keyed component data and commits no
//! fact. [`Provision`] is the fix: `resolve` now consults it instead, and it
//! can see every home once each is wired.
//!
//! This module wires the **ledger home only**. The component and session
//! homes exist in the type ([`Home::Component`], [`Home::Session`]) but are
//! **unreachable by construction** — both carry [`Unwired`], an empty enum
//! no value of which can ever exist — so a table built by this task cannot
//! accidentally score either home as serving anything. Tasks 6 and 7 wire
//! them by replacing `Unwired` with a real payload type.
//!
//! **Direction, stated per the standing rule (spec §4.1):** the table
//! asserts *declared ⊆ served*, never the reverse. A token with no row is
//! undeclared and therefore missing — the same default-deny `resolve`
//! always had. A token WITH a row is not automatically served either: its
//! home's resolver is asked, and a "no" still leaves the token missing. The
//! table can only narrow what counts as present; it can never widen it
//! beyond what a home actually serves.

use hornvale_kernel::ConceptRegistry;
// Re-exported: every public `Provision` method takes or returns a
// `Correspondent<Home, Unserved>`, so a caller needs this type too.
pub use hornvale_kernel::Correspondent;
use std::collections::BTreeMap;

/// An uninhabited placeholder payload. No value of this type can ever be
/// constructed (it has zero variants), so a [`Home`] variant that carries
/// one — [`Home::Component`], [`Home::Session`] — cannot be constructed
/// either. This is what makes the two unwired homes "exist and be
/// unreachable by construction" rather than merely undocumented: the
/// compiler enforces it, the same way `Void`'s closed variant list enforces
/// that a `Manifest` absence always names a reason.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Unwired {}

/// Why a token is absent from every home that could serve it. Closed on
/// purpose, mirroring [`hornvale_kernel::Void`]: every variant carries
/// mandatory data, so there is no way to construct a reasonless absence —
/// the compiler refuses `Unserved::NotServed()` with no argument.
///
/// ```compile_fail
/// use hornvale::provision::Unserved;
///
/// // Omits the mandatory reason — this does not compile (E0061).
/// let _u = Unserved::NotServed();
/// ```
/// type-audit: bare-ok(prose: NotServed.0)
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Unserved {
    /// Declared servable by some home, but that home's resolver answered
    /// "no" for this specific token, or no home was ever declared for it.
    NotServed(&'static str),
}

/// Which of the three capability homes (spec §2.1) is declared to serve a
/// token.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Home {
    /// The concept registry: predicates, phenomenon kinds, concepts. The
    /// only home this task's [`Provision`] rows ever name.
    Ledger,
    /// Kind-keyed build-state (`WorldComponents`) — never persisted.
    /// Unreachable by construction until Task 6 gives [`Unwired`] a real
    /// payload.
    Component(Unwired),
    /// Session state, persisted only when asked. Unreachable by
    /// construction until Task 7 gives [`Unwired`] a real payload.
    Session(Unwired),
}

/// The provision table: a declared row per namespaced token, naming which
/// home serves it or why none does.
#[derive(Clone, Debug, Default)]
pub struct Provision {
    rows: BTreeMap<String, Correspondent<Home, Unserved>>,
}

impl Provision {
    /// An empty table. Every token is undeclared, and an undeclared token
    /// is missing — default-deny needs no rows to hold.
    pub fn new() -> Self {
        Self {
            rows: BTreeMap::new(),
        }
    }

    /// Declare a row: `token` is claimed by `home`, or is recorded absent
    /// with a reason. Overwrites any existing row for the same token — the
    /// table is meant to be rebuilt fresh per resolve, never accumulated.
    pub fn declare(&mut self, token: impl Into<String>, home: Correspondent<Home, Unserved>) {
        self.rows.insert(token.into(), home);
    }

    /// Read the declared row for `token`, if any. Lets a caller see *why*
    /// a token is absent rather than only *that* it is.
    /// type-audit: bare-ok(identifier-text: token)
    pub fn row(&self, token: &str) -> Option<&Correspondent<Home, Unserved>> {
        self.rows.get(token)
    }

    /// Build the ledger home's table: one `Present(Home::Ledger)` row per
    /// predicate, phenomenon kind, and concept the registry currently
    /// holds. This reproduces exactly what the pre-Avowal `registry_tokens`
    /// scan computed, now behind the provision table's declared/served
    /// split instead of a bare set.
    pub fn from_registry(registry: &ConceptRegistry) -> Self {
        let mut table = Self::new();
        for p in registry.predicates() {
            table.declare(
                format!("predicate:{}", p.name),
                Correspondent::Present(Home::Ledger),
            );
        }
        for (kind, _doc) in registry.phenomenon_kinds() {
            table.declare(
                format!("phenomenon:{kind}"),
                Correspondent::Present(Home::Ledger),
            );
        }
        for c in registry.concepts() {
            table.declare(
                format!("concept:{}", c.name),
                Correspondent::Present(Home::Ledger),
            );
        }
        table
    }

    /// Whether `token` is served right now.
    ///
    /// `None` (undeclared) and `Absent` (declared absent) both answer
    /// `false` — default-deny. A declared `Present(Home::Ledger)` row still
    /// asks the ledger resolver ([`ledger_holds`]) rather than trusting the
    /// declaration alone, so a row naming a home that turns out not to
    /// actually serve the token is refused, not waved through: declared ⊆
    /// served, never the reverse.
    /// type-audit: bare-ok(identifier-text: token), bare-ok(flag: return)
    pub fn serves(&self, token: &str, registry: &ConceptRegistry) -> bool {
        match self.rows.get(token) {
            None => false,
            Some(Correspondent::Absent(_reason)) => false,
            Some(Correspondent::Present(Home::Ledger)) => ledger_holds(registry, token),
            // Unreachable: no `Home::Component`/`Home::Session` row can
            // exist while `Unwired` has zero variants — see the module doc.
            Some(Correspondent::Present(Home::Component(unwired))) => match *unwired {},
            Some(Correspondent::Present(Home::Session(unwired))) => match *unwired {},
        }
    }
}

/// The ledger home's resolver: does `registry` actually hold this exact
/// namespaced token? Mirrors the three namespaces the pre-Avowal
/// `registry_tokens` scan produced.
/// type-audit: bare-ok(identifier-text: token)
fn ledger_holds(registry: &ConceptRegistry, token: &str) -> bool {
    if let Some(name) = token.strip_prefix("predicate:") {
        registry.predicate(name).is_some()
    } else if let Some(name) = token.strip_prefix("phenomenon:") {
        registry.phenomenon_kind(name).is_some()
    } else if let Some(name) = token.strip_prefix("concept:") {
        registry.concept(name).is_some()
    } else {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn a_registry() -> ConceptRegistry {
        let mut r = ConceptRegistry::default();
        r.register_predicate("parent-of", false, "a parent-of relation")
            .unwrap();
        r
    }

    #[test]
    fn undeclared_token_is_not_served() {
        let table = Provision::new();
        let r = a_registry();
        assert!(!table.serves("predicate:parent-of", &r));
    }

    #[test]
    fn declared_and_actually_present_token_is_served() {
        let r = a_registry();
        let table = Provision::from_registry(&r);
        assert!(table.serves("predicate:parent-of", &r));
    }

    #[test]
    fn declared_but_unserved_token_is_refused() {
        let mut table = Provision::new();
        table.declare("predicate:ghost", Correspondent::Present(Home::Ledger));
        let r = a_registry();
        assert!(!table.serves("predicate:ghost", &r));
    }

    #[test]
    fn declared_absent_row_carries_and_returns_its_reason() {
        let mut table = Provision::new();
        table.declare(
            "predicate:affect-kind",
            Correspondent::Absent(Unserved::NotServed("component home not wired (Task 6)")),
        );
        let r = a_registry();
        assert!(!table.serves("predicate:affect-kind", &r));
        match table.row("predicate:affect-kind") {
            Some(Correspondent::Absent(Unserved::NotServed(reason))) => {
                assert_eq!(*reason, "component home not wired (Task 6)");
            }
            other => panic!("expected an Absent row carrying a reason, got {other:?}"),
        }
    }
}
