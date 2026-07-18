//! The epistemic account (C4, LANG-36): the four-filter stack a culture's
//! knowledge of the ground truth passes through, in order —
//!
//! 1. **Lexicon** — does the culture hold the concept the predicate needs?
//! 2. **Knowledge** — can the culture's capability (or instruments) reach
//!    the fact at all?
//! 3. **Ontology** — does the culture's own world-carving substitute a
//!    different category for a classification fact?
//! 4. **Valence** — what stance colors a kept, kind-naming fact?
//!
//! — plus the dial's three distance measures ([`distortion`],
//! [`distinctiveness`], [`recoverability`]) that read the resulting
//! [`Account`].
//!
//! Pure and surface-free: [`account_of`] and every measure here are total,
//! deterministic functions of their arguments — no streams, no ledger
//! writes, no rendering. This crate imports only `hornvale_kernel`, so
//! nothing here names another domain's predicates; the caller (worldgen,
//! Task 3) supplies the [`Observability`] table that encodes physical and
//! instrumental reality.
//!
//! **Derived, never stored.** An [`Account`] is not a save-format
//! artifact: nothing here is committed to the ledger or serialized.
//! Re-running [`account_of`] over the same ground facts and the same
//! [`AccountParams`] always reconstructs the same [`Account`]
//! byte-for-byte — the same determinism discipline as every other derived
//! view in this codebase (e.g. belief, `UNI-20`). A caller that wants a
//! culture's account persisted across a session must re-derive it from the
//! ledger and the params, never cache it as new committed state.

use hornvale_kernel::Value;
use std::collections::{BTreeMap, BTreeSet};

/// A committed ground-truth fact, kernel-shaped but decoupled from the
/// ledger: `subject`/`predicate` are plain identifier text (not
/// `EntityId`), so this crate never needs to resolve or mint entities —
/// the caller (worldgen, Task 3) reads the ledger and hands over this
/// flattened view.
/// type-audit: bare-ok(identifier-text: subject), bare-ok(identifier-text: predicate)
#[derive(Clone, Debug, PartialEq)]
pub struct GroundFact {
    /// The fact's subject, as identifier text (a settlement's name, a sky
    /// body's name, ...).
    pub subject: String,
    /// The fact's predicate, resolved against the concept registry
    /// upstream; here just the text key the observability table is keyed
    /// on.
    pub predicate: String,
    /// The fact's asserted value.
    pub object: Value,
}

/// Which concept a fact's predicate requires the observing culture to
/// hold, for the lexicon filter (step 1).
/// type-audit: bare-ok(identifier-text: Fixed.0)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NeededConcept {
    /// The concept named by the fact's object text (e.g. a `Value::Text`
    /// naming a classification).
    Object,
    /// The concept named by the object text's *kind*, formatted
    /// `"{object}-kind"` (e.g. the "goblin" object of an `instance-of` fact
    /// needs "goblin-kind").
    ObjectKind,
    /// A fixed concept name independent of the fact's object (e.g. every
    /// `moon-count` fact needs "moon", regardless of the count asserted).
    Fixed(&'static str),
}

/// How reachable a fact's predicate is for an observing culture, gating the
/// knowledge filter (step 2).
/// type-audit: bare-ok(ratio: SkyGraded.threshold)
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Requirement {
    /// Directly observable without instruments or inference; passes
    /// unconditionally (once held).
    Manifest,
    /// Observable once a culture's sky-observation capability reaches
    /// `threshold` (both in `[0, 1]`); below it, beyond capability.
    SkyGraded {
        /// The minimum `sky_capability` this fact requires to be kept.
        threshold: f64,
    },
    /// Requires instruments no floor culture has; always beyond capability.
    Instrumental,
    /// Requires cross-referencing another body's observations; always
    /// beyond capability at the floor.
    CrossReferential,
    /// A classification fact whose truth is stated in the ground's own
    /// terms — falls through to the ontology filter (step 3) rather than
    /// passing or failing here directly.
    Taxonomic,
}

/// One predicate's entry in the observability table: what it takes for an
/// observing culture to retain a fact asserted under this predicate.
/// type-audit: bare-ok(identifier-text: domain)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Observability {
    /// The knowledge-filter gate this predicate's facts must pass.
    pub requirement: Requirement,
    /// The domain this predicate belongs to (e.g. "sky", "peoples"), used
    /// by [`domain_distortion`] and salience ordering — never a sibling
    /// domain's crate name, just a grouping label the caller assigns.
    pub domain: &'static str,
    /// Which concept the lexicon filter requires for this predicate's
    /// facts.
    pub concept: NeededConcept,
}

/// An observing culture's felt relationship toward a fact's object, when
/// the object names a kind — the valence filter's output (step 4).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Stance {
    /// The culture's own kind.
    Ourselves,
    /// A friendly or allied kind.
    Neighbors,
    /// An opposed or hostile kind.
    Rivals,
    /// An unfamiliar kind, known only distantly.
    Strangers,
    /// No stance recorded — the default for non-kind facts and kinds
    /// absent from the stance table.
    Neutral,
}

/// How [`account_of`] orders its output entries — the salience half of the
/// ontology filter.
/// type-audit: bare-ok(flag: Salience.sky_first)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OrderPolicy {
    /// Keep the ground list's input order.
    Ground,
    /// Stably partition entries by domain: `sky_first: true` moves every
    /// `domain == "sky"` entry before the rest (each half keeping ground
    /// order); `sky_first: false` is the reverse partition. Stable
    /// partition only — never a sort.
    Salience {
        /// Whether sky-domain entries lead (`true`) or trail (`false`).
        sky_first: bool,
    },
}

/// Why a fact was lost (neither `Kept` nor `Substituted`).
/// type-audit: bare-ok(identifier-text: BeyondCapability.domain)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LossReason {
    /// The observing culture never held the required concept (the lexicon
    /// filter — or an unregistered predicate under `hold_all: false`,
    /// which fails closed).
    NoConcept,
    /// The fact's predicate requires reach the culture's knowledge filter
    /// does not grant, in the stated domain.
    BeyondCapability {
        /// The domain the fact fell short in ("sky", or "unknown" for a
        /// predicate absent from the observability table).
        domain: &'static str,
    },
}

/// What happened to one ground fact when passed through the four-filter
/// account.
/// type-audit: bare-ok(identifier-text: Substituted.truth), bare-ok(identifier-text: Substituted.theirs)
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Disposition {
    /// The fact survived every filter unchanged.
    Kept,
    /// The fact did not survive; see [`LossReason`].
    Lost(LossReason),
    /// The fact's classification was recast in the observing culture's own
    /// world-carving (the ontology filter, step 3): `truth` is the
    /// ground's own term, `theirs` the substituted term.
    Substituted {
        /// The ground truth's own term for this classification.
        truth: String,
        /// The observing culture's substituted term.
        theirs: String,
    },
}

/// One ground fact's outcome after the four-filter account, plus this
/// culture's felt stance toward the fact's object.
#[derive(Clone, Debug, PartialEq)]
pub struct AccountEntry {
    /// The ground-truth fact this entry accounts for.
    pub fact: GroundFact,
    /// What happened to the fact under this culture's account.
    pub disposition: Disposition,
    /// This culture's felt stance toward the fact's object.
    pub stance: Stance,
    /// The fact's position in the ground list [`account_of`] was called
    /// with. Never part of the public contract, never serialized — pure
    /// bookkeeping the order-distance and cross-account alignment measures
    /// re-derive every call (the LANG-36 derived-not-stored guard extends
    /// to this index too).
    original_index: usize,
}

/// A culture's complete epistemic account of the ground truth: every
/// [`GroundFact`] handed to [`account_of`], each with its [`Disposition`]
/// and [`Stance`], in the order [`AccountParams::order`] produced.
#[derive(Clone, Debug, PartialEq)]
pub struct Account {
    /// The account's entries, in output order.
    pub entries: Vec<AccountEntry>,
}

/// The knobs [`account_of`] filters a ground list through: one observing
/// culture's held vocabulary, capability, world-carving, stances, and
/// output order.
/// type-audit: bare-ok(flag: hold_all), bare-ok(identifier-text: holdings), bare-ok(identifier-text: observability), bare-ok(ratio: sky_capability), bare-ok(identifier-text: stances), bare-ok(identifier-text: world_carving)
#[derive(Clone, Debug)]
pub struct AccountParams {
    /// The null filter: when `true`, every fact is `Kept` regardless of
    /// `holdings`, `observability`, `sky_capability`, or `world_carving` —
    /// [`identity_params`] sets this.
    pub hold_all: bool,
    /// The concept names this culture holds (lexicon filter).
    pub holdings: BTreeSet<String>,
    /// The observability table, keyed by predicate. A predicate absent
    /// from this table is `Lost(BeyondCapability { domain: "unknown" })`
    /// under `hold_all: false` — fail closed.
    pub observability: BTreeMap<String, Observability>,
    /// This culture's sky-observation capability, in `[0, 1]`, gating
    /// `Requirement::SkyGraded` facts.
    pub sky_capability: f64,
    /// How `account_of` orders its output entries.
    pub order: OrderPolicy,
    /// This culture's stance toward other kinds, keyed by kind name
    /// (applies only to `ObjectKind` facts).
    pub stances: BTreeMap<String, Stance>,
    /// This culture's own name for a `Taxonomic` fact's classification, if
    /// it carves the world that way at all.
    pub world_carving: Option<String>,
}

/// The concept text a fact's predicate needs the culture to hold, per its
/// table row's [`NeededConcept`].
fn needed_concept_text(fact: &GroundFact, concept: NeededConcept) -> String {
    match concept {
        NeededConcept::Object => value_text(&fact.object),
        NeededConcept::ObjectKind => format!("{}-kind", value_text(&fact.object)),
        NeededConcept::Fixed(name) => name.to_string(),
    }
}

/// A fact's object, rendered as plain text (the lexicon/ontology filters'
/// only use of the object's value).
fn value_text(value: &Value) -> String {
    match value {
        Value::Text(s) => s.clone(),
        Value::Entity(e) => e.get().to_string(),
        Value::Number(n) => n.to_string(),
        Value::Flag(b) => b.to_string(),
    }
}

/// Steps 1-3 of the account (lexicon, knowledge, ontology) for a fact whose
/// predicate has an observability-table row. Only ever called under
/// `hold_all: false` — the caller short-circuits `hold_all: true` to
/// `Kept` before reaching this.
fn filtered_disposition(
    fact: &GroundFact,
    obs: &Observability,
    params: &AccountParams,
) -> Disposition {
    // Step 1: Lexicon. Taxonomic facts skip the held check entirely — the
    // classification as stated is beyond floor capability regardless of
    // holdings, and ontology (step 3) decides its fate instead.
    if !matches!(obs.requirement, Requirement::Taxonomic) {
        let needed = needed_concept_text(fact, obs.concept);
        if !params.holdings.contains(&needed) {
            return Disposition::Lost(LossReason::NoConcept);
        }
    }

    // Step 2: Knowledge (and, for Taxonomic, step 3: Ontology).
    match obs.requirement {
        Requirement::Manifest => Disposition::Kept,
        Requirement::SkyGraded { threshold } => {
            if params.sky_capability >= threshold {
                Disposition::Kept
            } else {
                Disposition::Lost(LossReason::BeyondCapability { domain: obs.domain })
            }
        }
        Requirement::Instrumental | Requirement::CrossReferential => {
            Disposition::Lost(LossReason::BeyondCapability { domain: obs.domain })
        }
        Requirement::Taxonomic => match &params.world_carving {
            Some(carving) => Disposition::Substituted {
                truth: value_text(&fact.object),
                theirs: carving.clone(),
            },
            None => Disposition::Lost(LossReason::NoConcept),
        },
    }
}

/// A fact's disposition under `params` — the null filter first, then the
/// table lookup (fail-closed on absence), then the filtered cascade.
fn disposition_for(fact: &GroundFact, params: &AccountParams) -> Disposition {
    if params.hold_all {
        return Disposition::Kept;
    }
    match params.observability.get(&fact.predicate) {
        None => Disposition::Lost(LossReason::BeyondCapability { domain: "unknown" }),
        Some(obs) => filtered_disposition(fact, obs, params),
    }
}

/// Step 4: Valence. Only `ObjectKind` facts carry a stance (looked up by
/// the object's kind name); everything else is `Neutral`. Read directly
/// from the observability table, independent of `hold_all` — the null
/// filter only ever governs `Disposition`, never `Stance`.
fn stance_for(fact: &GroundFact, params: &AccountParams) -> Stance {
    match params.observability.get(&fact.predicate) {
        Some(obs) if matches!(obs.concept, NeededConcept::ObjectKind) => {
            let kind = value_text(&fact.object);
            params
                .stances
                .get(&kind)
                .copied()
                .unwrap_or(Stance::Neutral)
        }
        _ => Stance::Neutral,
    }
}

/// A fact's table domain, if its predicate has a row — used by salience
/// ordering and [`domain_distortion`], independent of `hold_all`.
fn table_domain<'a>(fact: &GroundFact, params: &'a AccountParams) -> Option<&'a str> {
    params
        .observability
        .get(&fact.predicate)
        .map(|obs| obs.domain)
}

/// Apply `params.order` to a freshly filtered entry list: `Ground` keeps
/// input order; `Salience` stably partitions on `domain == "sky"`.
fn apply_order(entries: Vec<AccountEntry>, params: &AccountParams) -> Vec<AccountEntry> {
    match params.order {
        OrderPolicy::Ground => entries,
        OrderPolicy::Salience { sky_first } => {
            let (mut first, mut rest): (Vec<AccountEntry>, Vec<AccountEntry>) = entries
                .into_iter()
                .partition(|e| (table_domain(&e.fact, params) == Some("sky")) == sky_first);
            first.append(&mut rest);
            first
        }
    }
}

/// Filter `ground` through the four-filter account under `params`,
/// producing one [`AccountEntry`] per fact, in [`AccountParams::order`]
/// order. Pure and total: the same `(ground, params)` pair always produces
/// the same `Account`.
pub fn account_of(ground: &[GroundFact], params: &AccountParams) -> Account {
    let entries: Vec<AccountEntry> = ground
        .iter()
        .enumerate()
        .map(|(original_index, fact)| AccountEntry {
            fact: fact.clone(),
            disposition: disposition_for(fact, params),
            stance: stance_for(fact, params),
            original_index,
        })
        .collect();
    Account {
        entries: apply_order(entries, params),
    }
}

/// The table-free identity account params: `hold_all: true`, every other
/// table/set/map empty, `sky_capability: 1.0`, `OrderPolicy::Ground`, no
/// world-carving. Every fact handed to [`account_of`] under these params
/// comes back `Kept`, in ground order, with `Stance::Neutral` throughout
/// (the null filter — no observability row exists to carry a stance).
pub fn identity_params() -> AccountParams {
    AccountParams {
        hold_all: true,
        holdings: BTreeSet::new(),
        observability: BTreeMap::new(),
        sky_capability: 1.0,
        order: OrderPolicy::Ground,
        stances: BTreeMap::new(),
        world_carving: None,
    }
}

/// Inversions between `entries`' current order and their original ground
/// order, normalized by `n(n-1)/2` (0 for `n < 2`).
fn order_distance(entries: &[AccountEntry]) -> f64 {
    let n = entries.len();
    if n < 2 {
        return 0.0;
    }
    let mut inversions = 0usize;
    for i in 0..n {
        for j in (i + 1)..n {
            if entries[i].original_index > entries[j].original_index {
                inversions += 1;
            }
        }
    }
    inversions as f64 / ((n * (n - 1)) / 2) as f64
}

/// This account's overall distortion from the ground truth: the mean of
/// three components over `n = entries.len()` (`0.0` for an empty
/// account): the loss fraction (`Lost` + `Substituted`, over `n`), the
/// order distance (inversions between this account's order and the
/// ground/input order, over `n(n-1)/2`), and the stance fraction
/// (non-`Neutral` stances, over `n`).
/// type-audit: bare-ok(ratio: return)
pub fn distortion(account: &Account) -> f64 {
    let n = account.entries.len();
    if n == 0 {
        return 0.0;
    }
    let n_f = n as f64;
    let loss = account
        .entries
        .iter()
        .filter(|e| !matches!(e.disposition, Disposition::Kept))
        .count() as f64
        / n_f;
    let order = order_distance(&account.entries);
    let stance = account
        .entries
        .iter()
        .filter(|e| !matches!(e.stance, Stance::Neutral))
        .count() as f64
        / n_f;
    (loss + order + stance) / 3.0
}

/// This account's distortion restricted to entries whose predicate's table
/// row has `domain == d`: `(Lost + Substituted) / count`. Returns `0.0`
/// (never `NaN`) when no entry matches `d` — callers gate on the domain's
/// presence themselves.
/// type-audit: bare-ok(identifier-text: domain), bare-ok(ratio: return)
pub fn domain_distortion(account: &Account, params: &AccountParams, domain: &str) -> f64 {
    let matching: Vec<&AccountEntry> = account
        .entries
        .iter()
        .filter(|e| table_domain(&e.fact, params) == Some(domain))
        .collect();
    if matching.is_empty() {
        return 0.0;
    }
    let lost = matching
        .iter()
        .filter(|e| !matches!(e.disposition, Disposition::Kept))
        .count() as f64;
    lost / matching.len() as f64
}

/// Whether two dispositions agree, for [`distinctiveness`]: same outer
/// variant, and — when both are `Substituted` — the same `theirs` target
/// (a `Substituted` entry's `truth` is always the ground's own term, so it
/// never carries disagreement; a `Lost` entry's inner reason is likewise
/// not compared, only that both accounts lost the fact).
fn dispositions_agree(a: &Disposition, b: &Disposition) -> bool {
    match (a, b) {
        (Disposition::Kept, Disposition::Kept) => true,
        (Disposition::Lost(_), Disposition::Lost(_)) => true,
        (
            Disposition::Substituted { theirs: ta, .. },
            Disposition::Substituted { theirs: tb, .. },
        ) => ta == tb,
        _ => false,
    }
}

/// How much two accounts over the *same* ground list diverge (asserted via
/// `debug_assert_eq!` on entry-list lengths): the mean of three
/// components: the disposition-disagreement fraction (matched by each
/// entry's original ground position, comparing dispositions per
/// [`dispositions_agree`]); the order distance between the two accounts'
/// permutations (`a`'s original-index sequence read through `b`'s
/// positions, normalized inversions); and the stance-disagreement
/// fraction. `0.0` for two identical accounts.
/// type-audit: bare-ok(ratio: return)
pub fn distinctiveness(a: &Account, b: &Account) -> f64 {
    debug_assert_eq!(
        a.entries.len(),
        b.entries.len(),
        "distinctiveness compares two accounts over the same ground list"
    );
    let n = a.entries.len();
    if n == 0 {
        return 0.0;
    }
    let n_f = n as f64;

    let mut by_orig_a: Vec<Option<&AccountEntry>> = vec![None; n];
    for e in &a.entries {
        by_orig_a[e.original_index] = Some(e);
    }
    let mut by_orig_b: Vec<Option<&AccountEntry>> = vec![None; n];
    for e in &b.entries {
        by_orig_b[e.original_index] = Some(e);
    }

    let mut disposition_disagree = 0usize;
    let mut stance_disagree = 0usize;
    for k in 0..n {
        let ea = by_orig_a[k].expect("account_of assigns every original index exactly once");
        let eb = by_orig_b[k].expect("account_of assigns every original index exactly once");
        if !dispositions_agree(&ea.disposition, &eb.disposition) {
            disposition_disagree += 1;
        }
        if ea.stance != eb.stance {
            stance_disagree += 1;
        }
    }

    let mut position_in_b = vec![0usize; n];
    for (position, e) in b.entries.iter().enumerate() {
        position_in_b[e.original_index] = position;
    }
    let sequence: Vec<usize> = a
        .entries
        .iter()
        .map(|e| position_in_b[e.original_index])
        .collect();
    let mut inversions = 0usize;
    for i in 0..n {
        for j in (i + 1)..n {
            if sequence[i] > sequence[j] {
                inversions += 1;
            }
        }
    }
    let order = if n < 2 {
        0.0
    } else {
        inversions as f64 / ((n * (n - 1)) / 2) as f64
    };

    (disposition_disagree as f64 / n_f + order + stance_disagree as f64 / n_f) / 3.0
}

/// The fraction of this account's entries an outside observer could
/// recover the ground truth from: `Kept` entries always recover; a
/// `Substituted` entry recovers iff its `theirs` target is unique among
/// the account's substitutions (injectivity — two facts substituted to
/// the same target destroy the inverse for both); `Lost` entries never
/// recover. `1.0` for an empty account.
/// type-audit: bare-ok(ratio: return)
pub fn recoverability(account: &Account) -> f64 {
    let n = account.entries.len();
    if n == 0 {
        return 1.0;
    }
    let mut theirs_counts: BTreeMap<&str, usize> = BTreeMap::new();
    for e in &account.entries {
        if let Disposition::Substituted { theirs, .. } = &e.disposition {
            *theirs_counts.entry(theirs.as_str()).or_insert(0) += 1;
        }
    }
    let recovered = account
        .entries
        .iter()
        .filter(|e| match &e.disposition {
            Disposition::Kept => true,
            Disposition::Substituted { theirs, .. } => {
                theirs_counts.get(theirs.as_str()).copied().unwrap_or(0) == 1
            }
            Disposition::Lost(_) => false,
        })
        .count();
    recovered as f64 / n as f64
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fixture_ground() -> Vec<GroundFact> {
        vec![
            GroundFact {
                subject: "Vebe".to_string(),
                predicate: "is-a".to_string(),
                object: Value::Text("planet".to_string()),
            },
            GroundFact {
                subject: "Vebe".to_string(),
                predicate: "moon-count".to_string(),
                object: Value::Number(2.0),
            },
            GroundFact {
                subject: "Vebe".to_string(),
                predicate: "star-class".to_string(),
                object: Value::Text("F".to_string()),
            },
            GroundFact {
                subject: "Vebe".to_string(),
                predicate: "day-length-std".to_string(),
                object: Value::Number(1.5),
            },
            GroundFact {
                subject: "Vavako".to_string(),
                predicate: "instance-of".to_string(),
                object: Value::Text("goblin".to_string()),
            },
        ]
    }

    fn fixture_table() -> BTreeMap<String, Observability> {
        let mut table = BTreeMap::new();
        table.insert(
            "is-a".to_string(),
            Observability {
                requirement: Requirement::Taxonomic,
                domain: "sky",
                concept: NeededConcept::Object,
            },
        );
        table.insert(
            "moon-count".to_string(),
            Observability {
                requirement: Requirement::SkyGraded { threshold: 0.6 },
                domain: "sky",
                concept: NeededConcept::Fixed("moon"),
            },
        );
        table.insert(
            "star-class".to_string(),
            Observability {
                requirement: Requirement::Instrumental,
                domain: "sky",
                concept: NeededConcept::Fixed("star"),
            },
        );
        table.insert(
            "day-length-std".to_string(),
            Observability {
                requirement: Requirement::CrossReferential,
                domain: "sky",
                concept: NeededConcept::Fixed("sun"),
            },
        );
        table.insert(
            "instance-of".to_string(),
            Observability {
                requirement: Requirement::Manifest,
                domain: "peoples",
                concept: NeededConcept::ObjectKind,
            },
        );
        table
    }

    fn holdings_of(names: &[&str]) -> BTreeSet<String> {
        names.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn null_filter_keeps_everything_in_ground_order() {
        let acc = account_of(&fixture_ground(), &identity_params());
        assert!(
            acc.entries
                .iter()
                .all(|e| matches!(e.disposition, Disposition::Kept))
        );
        let predicates: Vec<&str> = acc
            .entries
            .iter()
            .map(|e| e.fact.predicate.as_str())
            .collect();
        assert_eq!(
            predicates,
            vec![
                "is-a",
                "moon-count",
                "star-class",
                "day-length-std",
                "instance-of"
            ]
        );
        assert_eq!(distortion(&acc), 0.0);
        assert_eq!(recoverability(&acc), 1.0);
    }

    #[test]
    fn sky_graded_facts_split_on_capability() {
        let mut params = AccountParams {
            hold_all: false,
            holdings: holdings_of(&["moon", "star", "sun", "earth", "goblin-kind"]),
            observability: fixture_table(),
            sky_capability: 0.5,
            order: OrderPolicy::Ground,
            stances: BTreeMap::new(),
            world_carving: Some("earth".to_string()),
        };
        let acc_low = account_of(&fixture_ground(), &params);
        let moon_low = acc_low
            .entries
            .iter()
            .find(|e| e.fact.predicate == "moon-count")
            .unwrap();
        assert_eq!(
            moon_low.disposition,
            Disposition::Lost(LossReason::BeyondCapability { domain: "sky" })
        );

        params.sky_capability = 0.9;
        let acc_high = account_of(&fixture_ground(), &params);
        let moon_high = acc_high
            .entries
            .iter()
            .find(|e| e.fact.predicate == "moon-count")
            .unwrap();
        assert_eq!(moon_high.disposition, Disposition::Kept);
    }

    #[test]
    fn taxonomic_substitutes_the_carving_or_loses() {
        let params_carved = AccountParams {
            hold_all: false,
            holdings: holdings_of(&["planet"]),
            observability: fixture_table(),
            sky_capability: 1.0,
            order: OrderPolicy::Ground,
            stances: BTreeMap::new(),
            world_carving: Some("earth".to_string()),
        };
        let acc_carved = account_of(&fixture_ground(), &params_carved);
        let is_a_carved = acc_carved
            .entries
            .iter()
            .find(|e| e.fact.predicate == "is-a")
            .unwrap();
        assert_eq!(
            is_a_carved.disposition,
            Disposition::Substituted {
                truth: "planet".to_string(),
                theirs: "earth".to_string(),
            }
        );

        let params_uncarved = AccountParams {
            world_carving: None,
            ..params_carved
        };
        let acc_uncarved = account_of(&fixture_ground(), &params_uncarved);
        let is_a_uncarved = acc_uncarved
            .entries
            .iter()
            .find(|e| e.fact.predicate == "is-a")
            .unwrap();
        assert_eq!(
            is_a_uncarved.disposition,
            Disposition::Lost(LossReason::NoConcept)
        );

        // Injectivity: two facts substituted to the SAME target both fail
        // to recover; a lone substitution recovers.
        let collision_ground = vec![
            GroundFact {
                subject: "Vebe".to_string(),
                predicate: "is-a".to_string(),
                object: Value::Text("planet".to_string()),
            },
            GroundFact {
                subject: "Luma".to_string(),
                predicate: "is-a-2".to_string(),
                object: Value::Text("moon".to_string()),
            },
        ];
        let mut collision_table = BTreeMap::new();
        collision_table.insert(
            "is-a".to_string(),
            Observability {
                requirement: Requirement::Taxonomic,
                domain: "sky",
                concept: NeededConcept::Object,
            },
        );
        collision_table.insert(
            "is-a-2".to_string(),
            Observability {
                requirement: Requirement::Taxonomic,
                domain: "sky",
                concept: NeededConcept::Object,
            },
        );
        let collision_params = AccountParams {
            hold_all: false,
            holdings: BTreeSet::new(),
            observability: collision_table,
            sky_capability: 1.0,
            order: OrderPolicy::Ground,
            stances: BTreeMap::new(),
            world_carving: Some("earth".to_string()),
        };
        let collision_acc = account_of(&collision_ground, &collision_params);
        assert!(collision_acc.entries.iter().all(|e| matches!(
            &e.disposition,
            Disposition::Substituted { theirs, .. } if theirs == "earth"
        )));
        assert_eq!(
            recoverability(&collision_acc),
            0.0,
            "both facts collide on the same substituted target — neither recovers"
        );

        // A lone substitution (only one fact maps to "earth") does recover:
        // only `is-a` is Kept-or-uniquely-substituted here (the other four
        // facts fail the lexicon filter under this test's narrow
        // `holdings`), so 1 of 5 entries recovers.
        assert_eq!(recoverability(&acc_carved), 1.0 / 5.0);
    }

    #[test]
    fn instrumental_and_cross_referential_always_lose() {
        let params = AccountParams {
            hold_all: false,
            holdings: holdings_of(&["star", "sun"]),
            observability: fixture_table(),
            sky_capability: 1.0,
            order: OrderPolicy::Ground,
            stances: BTreeMap::new(),
            world_carving: None,
        };
        let acc = account_of(&fixture_ground(), &params);
        let star = acc
            .entries
            .iter()
            .find(|e| e.fact.predicate == "star-class")
            .unwrap();
        let day = acc
            .entries
            .iter()
            .find(|e| e.fact.predicate == "day-length-std")
            .unwrap();
        assert_eq!(
            star.disposition,
            Disposition::Lost(LossReason::BeyondCapability { domain: "sky" })
        );
        assert_eq!(
            day.disposition,
            Disposition::Lost(LossReason::BeyondCapability { domain: "sky" })
        );
    }

    #[test]
    fn salience_order_partitions_sky_first_and_counts_inversions() {
        let mut params = AccountParams {
            hold_all: true,
            holdings: BTreeSet::new(),
            observability: fixture_table(),
            sky_capability: 1.0,
            order: OrderPolicy::Salience { sky_first: true },
            stances: BTreeMap::new(),
            world_carving: None,
        };
        let acc_sky_first = account_of(&fixture_ground(), &params);
        let predicates_sky_first: Vec<&str> = acc_sky_first
            .entries
            .iter()
            .map(|e| e.fact.predicate.as_str())
            .collect();
        assert_eq!(
            predicates_sky_first,
            vec![
                "is-a",
                "moon-count",
                "star-class",
                "day-length-std",
                "instance-of"
            ]
        );
        assert_eq!(
            distortion(&acc_sky_first),
            0.0,
            "the sky-first partition matches ground order exactly here"
        );

        params.order = OrderPolicy::Salience { sky_first: false };
        let acc_sky_last = account_of(&fixture_ground(), &params);
        let predicates_sky_last: Vec<&str> = acc_sky_last
            .entries
            .iter()
            .map(|e| e.fact.predicate.as_str())
            .collect();
        assert_eq!(
            predicates_sky_last,
            vec![
                "instance-of",
                "is-a",
                "moon-count",
                "star-class",
                "day-length-std"
            ]
        );
        assert!(
            distortion(&acc_sky_last) > 0.0,
            "moving instance-of first must show up as order distortion"
        );
    }

    #[test]
    fn stances_apply_only_to_object_kind_facts() {
        let mut stances = BTreeMap::new();
        stances.insert("goblin".to_string(), Stance::Rivals);
        let params = AccountParams {
            hold_all: true,
            holdings: BTreeSet::new(),
            observability: fixture_table(),
            sky_capability: 1.0,
            order: OrderPolicy::Ground,
            stances,
            world_carving: None,
        };
        let acc = account_of(&fixture_ground(), &params);
        let instance_of = acc
            .entries
            .iter()
            .find(|e| e.fact.predicate == "instance-of")
            .unwrap();
        assert_eq!(instance_of.stance, Stance::Rivals);
        for entry in acc
            .entries
            .iter()
            .filter(|e| e.fact.predicate != "instance-of")
        {
            assert_eq!(entry.stance, Stance::Neutral);
        }
        assert_eq!(distortion(&acc), (1.0 / 5.0) * (1.0 / 3.0));
    }

    #[test]
    fn strangers_stance_is_reachable() {
        let mut stances = BTreeMap::new();
        stances.insert("goblin".to_string(), Stance::Strangers);
        let params = AccountParams {
            hold_all: true,
            holdings: BTreeSet::new(),
            observability: fixture_table(),
            sky_capability: 1.0,
            order: OrderPolicy::Ground,
            stances,
            world_carving: None,
        };
        let acc = account_of(&fixture_ground(), &params);
        let instance_of = acc
            .entries
            .iter()
            .find(|e| e.fact.predicate == "instance-of")
            .unwrap();
        assert_eq!(instance_of.stance, Stance::Strangers);
    }

    #[test]
    fn domain_distortion_measures_only_the_named_domain() {
        let params = AccountParams {
            hold_all: false,
            holdings: holdings_of(&["planet", "moon", "star", "sun", "goblin-kind"]),
            observability: fixture_table(),
            sky_capability: 0.5,
            order: OrderPolicy::Ground,
            stances: BTreeMap::new(),
            world_carving: Some("earth".to_string()),
        };
        let acc = account_of(&fixture_ground(), &params);

        // The 4 sky-domain facts: is-a Substituted, moon-count Lost
        // (0.5 < the 0.6 threshold), star-class Lost (Instrumental),
        // day-length-std Lost (CrossReferential) — 4 of 4 non-Kept.
        assert_eq!(domain_distortion(&acc, &params, "sky"), 1.0);

        // The lone peoples-domain fact (instance-of) is Kept.
        assert_eq!(domain_distortion(&acc, &params, "peoples"), 0.0);

        // A domain with no matching entries returns 0.0, never NaN.
        assert_eq!(domain_distortion(&acc, &params, "nonesuch"), 0.0);
    }

    #[test]
    fn unknown_predicate_fails_closed() {
        let fact = GroundFact {
            subject: "Vebe".to_string(),
            predicate: "totally-unknown-predicate".to_string(),
            object: Value::Text("mystery".to_string()),
        };
        let params = AccountParams {
            hold_all: false,
            holdings: BTreeSet::new(),
            observability: fixture_table(),
            sky_capability: 1.0,
            order: OrderPolicy::Ground,
            stances: BTreeMap::new(),
            world_carving: None,
        };
        let acc = account_of(std::slice::from_ref(&fact), &params);
        assert_eq!(
            acc.entries[0].disposition,
            Disposition::Lost(LossReason::BeyondCapability { domain: "unknown" })
        );

        let acc_identity = account_of(&[fact], &identity_params());
        assert_eq!(acc_identity.entries[0].disposition, Disposition::Kept);
    }

    #[test]
    fn distinctiveness_is_zero_for_clones_and_positive_for_divergents() {
        let mut stances_a = BTreeMap::new();
        stances_a.insert("goblin".to_string(), Stance::Rivals);
        let params_a = AccountParams {
            hold_all: false,
            holdings: holdings_of(&["moon", "goblin-kind"]),
            observability: fixture_table(),
            sky_capability: 0.5,
            order: OrderPolicy::Ground,
            stances: stances_a,
            world_carving: None,
        };
        let acc_a1 = account_of(&fixture_ground(), &params_a);
        let acc_a2 = account_of(&fixture_ground(), &params_a);
        assert_eq!(distinctiveness(&acc_a1, &acc_a2), 0.0);

        let mut stances_b = BTreeMap::new();
        stances_b.insert("goblin".to_string(), Stance::Neutral);
        let params_b = AccountParams {
            sky_capability: 0.9,
            stances: stances_b,
            ..params_a
        };
        let acc_b = account_of(&fixture_ground(), &params_b);
        let d_ab = distinctiveness(&acc_a1, &acc_b);
        assert!(
            d_ab > 0.0,
            "differing capability (moon-count) and stance must show up as distinctiveness"
        );
        assert_eq!(
            d_ab,
            distinctiveness(&acc_b, &acc_a1),
            "distinctiveness is symmetric"
        );
    }
}
