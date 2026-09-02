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
//! Task 5 wired the **ledger home**. Task 6 wired the **component home**
//! (decision 0579): `Home::Component` carries [`ComponentResolver`], a real
//! zero-argument resolver — `windows/sentiment`'s per-people snap judgment
//! (`predicate:affect-kind`, `predicate:affect-intensity`) is served this
//! way. **Task 7 wires the session home** (decision 0580): `Home::Session`
//! now carries [`SessionResolver`] — the same shape as `ComponentResolver`,
//! since [`Provision::serves`] never has a live `Session` to hand a
//! resolver (it is built from `registry: &ConceptRegistry` alone, exactly
//! like the component home's own resolver) — instead of the uninhabited
//! `Unwired` placeholder Tasks 5 and 6 left it with. `windows/vessel::act`'s
//! derived act view (`ActHandle`, `witnessed`, `present_at`, `deed_of`,
//! `act_precedes`, `act_occurred_on`) is served this way for
//! `predicate:witnessed`, `predicate:present-at`, `predicate:deed-of`,
//! `predicate:act-precedes` and `predicate:act-occurred-on`.
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

/// The session home's resolver (decision 0580): a zero-argument predicate
/// over the derived act view's own machinery — `windows/vessel::act` —
/// answering whether the token it is declared against is served right now.
/// Exactly [`ComponentResolver`]'s shape, for the same reason: neither home
/// has a live world or session to hand its resolver, since
/// [`Provision::build`]/[`Provision::serves`] are asked with a bare
/// `&ConceptRegistry`. "Session state" names WHERE the capability would
/// live if a caller ever committed it (never the ledger, see
/// `windows/vessel::act`'s module doc), not an object this resolver reads.
pub type SessionResolver = fn() -> bool;

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
    /// Session state, persisted only when a snapshot asks (decision 0368)
    /// — never a ledger commit for the derived act view this home serves
    /// (decision 0580). Carries a real [`SessionResolver`] as of Task 7.
    Session(SessionResolver),
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
    /// `from_registry`'s ledger rows, the component-home rows Task 6 added,
    /// and the session-home rows this task adds. This is what
    /// [`crate::tropes::resolve`] and the coverage report's Leverage/Columns
    /// sections actually consult (decision 0579); `from_registry` alone
    /// stays ledger-only for the callers (a `trope_witness.rs` fixture,
    /// `Provision`'s own tests) that want that narrower scope.
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
    ///
    /// `predicate:witnessed`, `predicate:present-at`, `predicate:deed-of`,
    /// `predicate:act-precedes` and `predicate:act-occurred-on` are all
    /// declared `Present(Home::Session(session_act_view_holds))` (decision
    /// 0580) — one shared resolver, the same way both affect tokens share
    /// `sentiment_affect_holds`, since all five read the one derived act
    /// view `windows/vessel::act` provides. `predicate:history-now` (the
    /// `act-chronology` bundle's fourth token) is not declared here: it is
    /// already a registered predicate the ledger home serves (see
    /// `hornvale_history::HISTORY_NOW`), committed by the deep-history bake
    /// rather than derived over a live session.
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
        for token in [
            "predicate:witnessed",
            "predicate:present-at",
            "predicate:deed-of",
            "predicate:act-precedes",
            "predicate:act-occurred-on",
        ] {
            table.declare(
                token,
                Correspondent::Present(Home::Session(session_act_view_holds)),
            );
        }
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
    /// or `Present(Home::Session(_))` row is asked the same way — its
    /// resolver is called, not trusted.
    /// type-audit: bare-ok(identifier-text: token), bare-ok(flag: return)
    pub fn serves(&self, token: &str, registry: &ConceptRegistry) -> bool {
        match self.rows.get(token) {
            None => false,
            Some(Correspondent::Absent(_reason)) => false,
            Some(Correspondent::Present(Home::Ledger)) => ledger_holds(registry, token),
            Some(Correspondent::Present(Home::Component(resolver))) => resolver(),
            Some(Correspondent::Present(Home::Session(resolver))) => resolver(),
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

/// The session home's resolver for the acts bundle (decision 0580): proves
/// `windows/vessel::act`'s derived act view — [`ActHandle`](
/// hornvale_vessel::act::ActHandle), `witnessed`, `present_at`, `deed_of`,
/// `act_precedes`, `act_occurred_on`, `anyone_present` — end to end on
/// fixed, deterministic constituents. Mirrors [`sentiment_affect_holds`]'s
/// shape exactly: neither resolver has a live world or session to read
/// (`Provision::build`/`serves` are asked with a bare `&ConceptRegistry`
/// alone), so both prove their machinery is real the same way — by
/// exercising it, not by checking that some catalog is non-empty. Every
/// comparison below is deliberate: distinct fixed inputs must produce a
/// consistent chain of derived answers, or the machinery is not what it
/// claims to be.
fn session_act_view_holds() -> bool {
    use hornvale_kernel::{EntityId, WorldTime};
    use hornvale_vessel::act::{
        Act, act_occurred_on, act_precedes, anyone_present, deed_of, present_at, witnessed,
    };

    let actor = match EntityId::new(1) {
        Some(e) => e,
        None => return false,
    };
    let witness = match EntityId::new(2) {
        Some(e) => e,
        None => return false,
    };
    let earlier = Act {
        actor,
        deed: "founded",
        patient: None,
        day: WorldTime::from_ticks(0),
    };
    let later = Act {
        actor,
        deed: "avowed",
        patient: Some(witness),
        day: WorldTime::from_ticks(1),
    };
    let present = [witness];
    let agent_mark = hornvale_scene::Mark {
        noun: "someone".to_string(),
        kind: "agent".to_string(),
        datum: "present".to_string(),
        salience: 0,
    };

    deed_of(&earlier) == actor
        && act_occurred_on(&later) == later.day
        && act_precedes(&earlier, &later)
        && !act_precedes(&later, &earlier)
        && present_at(witness, &present)
        && !present_at(actor, &present)
        && witnessed(&later, witness, &present)
        && !witnessed(&later, actor, &present)
        && earlier.handle() != later.handle()
        && anyone_present(&[agent_mark])
        && !anyone_present(&[])
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

    /// **The session home's own negative control (Task 7), the same
    /// shape as the component home's** immediately above: `session_act_
    /// view_holds` cannot itself return `false` in practice (its inputs are
    /// fixed, deterministic constituents, not read from any world), so
    /// nothing else in this crate exercises `Provision::serves`'s
    /// `Home::Session` branch answering "no". A synthetic always-refuses
    /// resolver costs nothing to declare here, the same way it did for the
    /// component home.
    #[test]
    fn declared_session_row_whose_resolver_refuses_is_not_served() {
        let mut table = Provision::new();
        table.declare(
            "predicate:never-witnessed",
            Correspondent::Present(Home::Session(|| false)),
        );
        let r = a_registry();
        assert!(!table.serves("predicate:never-witnessed", &r));
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
