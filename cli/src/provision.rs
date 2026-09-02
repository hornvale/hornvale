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
//! Task 5 wired the **ledger home**. **Task 6 wires the component home**
//! (decision 0579): `Home::Component` now carries [`ComponentResolver`], a
//! real zero-argument resolver, instead of the uninhabited [`Unwired`]
//! placeholder Task 5 left it with — `windows/sentiment`'s per-people snap
//! judgment (`predicate:affect-kind`, `predicate:affect-intensity`) is
//! served this way. The session home ([`Home::Session`]) is still
//! **unreachable by construction** — it carries [`Unwired`], an empty enum
//! no value of which can ever exist — so a table built by this task cannot
//! accidentally score that home as serving anything. Task 7 wires it the
//! same way this task wired the component home.
//!
//! **Direction, stated per the standing rule (spec §4.1):** the table
//! asserts *declared ⊆ served*, never the reverse. A token with no row is
//! undeclared and therefore missing — the same default-deny `resolve`
//! always had. A token WITH a row is not automatically served either: its
//! home's resolver is asked, and a "no" still leaves the token missing. The
//! table can only narrow what counts as present; it can never widen it
//! beyond what a home actually serves.
//!
//! **`registry_tokens`-vs-`serves` reconciliation (decision 0579).**
//! `cli/src/tropes.rs` used to re-scan the registry directly (its own
//! `registry_tokens`, now deleted) for the Leverage fan-in table and the
//! matrix's column preamble, computing "what's held" independently of
//! `Provision::serves`. That agreed with `serves` only by coincidence, as
//! long as every served token was also a registry token — which stopped
//! being true the moment this task declared a component-home row. Both call
//! sites now read [`Provision::served_tokens`] instead, so there is exactly
//! one computation of "what's served", and `tropes.rs`'s witness-blocked
//! exclusion comment (`held.contains` implies "zero unheld bundles by
//! construction") is true again for a component-served token, not merely
//! for a ledger-served one.

use hornvale_kernel::ConceptRegistry;
// Re-exported: every public `Provision` method takes or returns a
// `Correspondent<Home, Unserved>`, so a caller needs this type too.
pub use hornvale_kernel::Correspondent;
use std::collections::{BTreeMap, BTreeSet};

/// An uninhabited placeholder payload. No value of this type can ever be
/// constructed (it has zero variants), so a [`Home`] variant that carries
/// one — [`Home::Session`], until Task 7 — cannot be constructed either.
/// This is what makes the session home "exist and be unreachable by
/// construction" rather than merely undocumented: the compiler enforces it,
/// the same way `Void`'s closed variant list enforces that a `Manifest`
/// absence always names a reason.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Unwired {}

/// The component home's resolver (decision 0579): a zero-argument predicate
/// over build-state — `KindId`-keyed data assembled fresh each run, never
/// the ledger — answering whether the token it is declared against is
/// served right now.
///
/// **A bare `fn() -> bool`, not an enum naming each known producer.** The
/// only consumer ([`Provision::serves`]) needs nothing about a producer
/// except "call it and see", and today there is exactly one producer
/// (`windows/sentiment`). An enum with one variant is an abstraction with no
/// second case to justify it; a function pointer keeps a second component
/// producer a `declare` call away rather than a match arm added here later.
/// `fn` pointers do implement `Clone`/`Debug`/`PartialEq`/`Eq` in `core`,
/// but [`Home`]'s own derive line narrows to `Clone, Debug` only — see that
/// type's doc for why (comparing this payload by address is not a
/// meaningful comparison, and `rustc` warns on it under `-D warnings`).
pub type ComponentResolver = fn() -> bool;

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
///
/// **`Clone`/`Debug` only — no `PartialEq`/`Eq`.** A derived `PartialEq`
/// would compare `Component`'s `ComponentResolver` (a `fn() -> bool`) by
/// address, which is not a meaningful comparison (the same function's
/// address can differ across codegen units, and distinct functions can
/// share one after merging) and `rustc` warns on it under
/// `-D warnings`. Nothing in this codebase ever needs `Home: PartialEq` —
/// every caller either constructs a row or pattern-matches one — so the
/// fix is to not derive a comparison nobody uses rather than paper over the
/// warning with an allow.
#[derive(Clone, Debug)]
pub enum Home {
    /// The concept registry: predicates, phenomenon kinds, concepts.
    Ledger,
    /// Kind-keyed build-state (`WorldComponents` and friends) — never
    /// persisted. Carries a real [`ComponentResolver`] as of Task 6
    /// (decision 0579).
    Component(ComponentResolver),
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

    /// Build the **full** provision table: every home wired so far —
    /// `from_registry`'s ledger rows, plus the component-home rows this task
    /// adds. This is what [`crate::tropes::resolve`] and the coverage
    /// report's Leverage/Columns sections actually consult (decision 0579);
    /// `from_registry` alone stays ledger-only for the callers (a `trope_
    /// witness.rs` fixture, `Provision`'s own tests) that want that
    /// narrower scope.
    ///
    /// `predicate:affect-kind` and `predicate:affect-intensity` are
    /// declared `Present(Home::Component(sentiment_affect_holds))` —
    /// `windows/sentiment::snap_judgment` is world-invariant, so the
    /// resolver needs no world or registry to answer. `predicate:
    /// feels-toward` is declared explicitly `Absent`, not merely left
    /// undeclared, so a reader of the row (not just the outcome) sees
    /// Nathan's grain ruling: `snap_judgment` is people-to-people
    /// (`KindId × KindId`), the corpus's `feels-toward` is person-to-person,
    /// and no person-scale producer ships this campaign
    /// (`docs/superpowers/ledgers/2026-09-01-the-avowal.md` entry #2).
    pub fn build(registry: &ConceptRegistry) -> Self {
        let mut table = Self::from_registry(registry);
        table.declare(
            format!("predicate:{}", hornvale_species::AFFECT_KIND),
            Correspondent::Present(Home::Component(sentiment_affect_holds)),
        );
        table.declare(
            format!("predicate:{}", hornvale_species::AFFECT_INTENSITY),
            Correspondent::Present(Home::Component(sentiment_affect_holds)),
        );
        table.declare(
            "predicate:feels-toward",
            Correspondent::Absent(Unserved::NotServed(
                "snap_judgment is people-to-people (KindId x KindId); feels-toward is \
                 person-to-person and has no producer at this grain (decision 0579, \
                 Nathan's grain ruling)",
            )),
        );
        table
    }

    /// Every token this table currently serves, home-blind — the single
    /// computation `tropes.rs`'s Leverage fan-in table and Columns section
    /// now share instead of each re-scanning the registry (decision 0579;
    /// see the module doc's reconciliation note). A row that is declared but
    /// not actually served (its resolver answers "no") is excluded, exactly
    /// like [`Provision::serves`].
    /// type-audit: bare-ok(identifier-text: return)
    pub fn served_tokens(&self, registry: &ConceptRegistry) -> BTreeSet<String> {
        self.rows
            .keys()
            .filter(|token| self.serves(token, registry))
            .cloned()
            .collect()
    }

    /// Whether `token` is served right now.
    ///
    /// `None` (undeclared) and `Absent` (declared absent) both answer
    /// `false` — default-deny. A declared `Present(Home::Ledger)` row still
    /// asks the ledger resolver ([`ledger_holds`]) rather than trusting the
    /// declaration alone, so a row naming a home that turns out not to
    /// actually serve the token is refused, not waved through: declared ⊆
    /// served, never the reverse. A declared `Present(Home::Component(_))`
    /// row is asked the same way — its resolver is called, not trusted.
    /// type-audit: bare-ok(identifier-text: token), bare-ok(flag: return)
    pub fn serves(&self, token: &str, registry: &ConceptRegistry) -> bool {
        match self.rows.get(token) {
            None => false,
            Some(Correspondent::Absent(_reason)) => false,
            Some(Correspondent::Present(Home::Ledger)) => ledger_holds(registry, token),
            Some(Correspondent::Present(Home::Component(resolver))) => resolver(),
            // Unreachable: no `Home::Session` row can exist while `Unwired`
            // has zero variants — see the module doc. Task 7 replaces this.
            Some(Correspondent::Present(Home::Session(unwired))) => match *unwired {},
        }
    }
}

/// The component home's resolver for `predicate:affect-kind` and
/// `predicate:affect-intensity` (decision 0579): served whenever
/// `hornvale_sentiment`'s fifteen-people catalog yields at least two
/// peoples to judge between. `snap_judgment` is total over any two
/// `PeopleTraits` — no error path — so this calls it end to end on the
/// catalog's first two peoples (deterministic `PeopleId` order) rather than
/// merely checking `!catalog().is_empty()`, which would prove the catalog
/// exists without ever exercising the judgment pipeline this token claims
/// is served.
fn sentiment_affect_holds() -> bool {
    let catalog = hornvale_sentiment::catalog();
    let mut peoples = catalog.values();
    match (peoples.next(), peoples.next()) {
        (Some(judger), Some(target)) => {
            let _: hornvale_sentiment::Judgment = hornvale_sentiment::snap_judgment(judger, target);
            true
        }
        _ => false,
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

    /// **The component home's own negative control.** The ledger home has
    /// one immediately above (`declared_but_unserved_token_is_refused`);
    /// the component home had none, and its one real resolver
    /// (`sentiment_affect_holds`) can never itself return `false` in
    /// practice — `hornvale_sentiment::catalog()` reads a hardcoded
    /// fifteen-people table, so nothing else in this crate ever exercises
    /// `Provision::serves`'s `resolver()` branch answering "no". A
    /// non-capturing closure coerces to `ComponentResolver` (`fn() ->
    /// bool`), so a synthetic always-refuses resolver costs nothing to
    /// declare here.
    #[test]
    fn declared_component_row_whose_resolver_refuses_is_not_served() {
        let mut table = Provision::new();
        table.declare(
            "predicate:never-served",
            Correspondent::Present(Home::Component(|| false)),
        );
        let r = a_registry();
        assert!(!table.serves("predicate:never-served", &r));
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
