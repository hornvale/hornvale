//! The causal-schema library and lexicalization substrate (C5, LANG-37 /
//! LANG-38): the closed set of embodied source-domains a culture's folk
//! explanations draw from, plus the closed verb tables that give the
//! agentive schema its surface voice.
//!
//! **The substrate and its two reads (LANG-38's un-collapse, the UNI-28
//! antichain lesson).** A source-domain — motion, force, container, path,
//! balance, link, verticality, kinship, flow, cycle — is a single embodied
//! structure read two deliberately separate ways: the **schema read**
//! ([`Schema`]/[`schema_table`], this module) treats a source-domain as a
//! *causal story* ("the day returns because the sun-god walks the sky");
//! the **mapping read** (LANG-38, a later task) treats the same
//! source-domain as a *descriptive projection* (duration-as-travel, with
//! no claimed causality). Collapsing the two into one struct was the
//! mistake UNI-28 named and this campaign refuses to repeat — a schema row
//! here never carries mapping-read fields, and never will.
//!
//! **Closure.** [`schema_table`] is exactly 12 authored rows (LANG-37's
//! list) and [`lexemes_for`]'s tables are exactly as closed. Adding a
//! thirteenth schema or a new verb is a deliberate library edit reviewed
//! like any other authored-table change (the `packs.rs` precedent) —
//! *never* a runtime path; nothing in this crate synthesizes a `SchemaId`
//! or a `LexemeId` that isn't one of these rows.
//!
//! **Why the selection draws at all (the anti-astrology rationale, C3's
//! copula precedent).** [`crate::grammar`] draws a tongue's word order
//! from a stream rather than deriving it from a culture's psychology
//! vector, because deriving a historically contingent surface choice from
//! a culture's traits would ship astrology as science — the trait
//! "predicts" a fact that is, in the real world, an accident of descent.
//! The same reasoning governs [`select_schema`]: *which* embodied story a
//! culture tells is nudged by its subsistence and psychology (the caller's
//! derived prior, sharpened by β), but never *determined* by them —
//! competition stays real ([`select_schema`] can and does pick against the
//! heaviest weight), and the draw is the contingency-honesty mechanism
//! that keeps a schema pick from reading as a deduction.
#![allow(clippy::module_name_repetitions)]

use crate::account::{Disposition, effective};
use hornvale_kernel::{Stream, math};

/// One of the ten embodied source-domains the schema and (future) mapping
/// reads both draw from — the closed substrate LANG-38 keeps un-collapsed
/// from its two reads.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SourceDomain {
    /// Self-propelled movement (walking, riding, rowing, stalking) — the
    /// agentive schema's home domain.
    Motion,
    /// Pushing, pulling, compelling, or resisting (Talmy's force dynamics).
    Force,
    /// Boundedness: full, empty, overflowing, contained.
    Container,
    /// A route traveled, retraced, or worn in.
    Path,
    /// A scale, tipped or level; equilibrium and its restoration.
    Balance,
    /// A bond or resemblance connecting two things (sympathetic magic's
    /// source domain).
    Link,
    /// Up/down orientation.
    Verticality,
    /// Descent and family relation.
    Kinship,
    /// A substance (liquid, mass) in motion — accumulating or draining.
    Flow,
    /// Recurrence: a thing returning to where it started.
    Cycle,
}

/// The classifier LANG-37 calls "the pass-1 hidden hub": which shape a
/// ground fact takes, derived (elsewhere) from its predicate's
/// observability row plus its value type. A schema's [`Schema::shapes`]
/// list is what a fact-shape needs to match before that schema is even a
/// candidate — the gate [`admitted`] reads.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FactShape {
    /// A periodic phenomenon with a re-derivable period (the day: `day-length-std`).
    CyclicEvent,
    /// A single scalar magnitude reported as a fact (a measured intensity;
    /// no floor caller reads this shape yet — reserved for the
    /// scalar-story schemas below).
    HighScalarState,
    /// A cardinality (the moons: `moon-count`).
    Count,
    /// A classification asserted in the ground's own terms (handled by the
    /// ontology filter, C4; the causal filter never explains a taxonomy —
    /// no schema admits this shape at the floor).
    Taxonomy,
    /// An enumerated membership list (no schema admits this shape at the
    /// floor).
    Roster,
}

/// The closed 12-row schema library (LANG-37). Every id names one embodied
/// causal story; [`schema_table`] is the only place these are constructed.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SchemaId {
    /// The compelled story: an external or overcoming force made the event happen.
    ForceDynamics,
    /// The animist story: an agent's own motion causes the event — the
    /// only schema with an [`SlotKind::Agent`] slot and a lexeme table.
    Agentive,
    /// The accumulation story: a quantity flows or drains like a substance.
    SubstanceFlow,
    /// The boundary story: a state is the fullness or overflow of a container.
    Container,
    /// The journey story: a cyclic event is a route retraced.
    PathJourney,
    /// The equilibrium story: a cyclic event is the world righting a tipped scale.
    Balance,
    /// The sympathetic-connection story: a count is explained by a bond
    /// linking the counted things — binds a [`SlotKind::Agent`] slot (C6
    /// T1: its surface frame ("… answers ⟨deity⟩.") always named a deity,
    /// so the table now says so directly, though it carries no lexeme
    /// table of its own).
    LinkSympathy,
    /// The descent story: a count is explained by kin relation — binds a
    /// [`SlotKind::Kin`] slot.
    Kinship,
    /// The debt story: a scalar state is an accrued moral balance —
    /// Lakoff's moral-accounting metaphor, a specialization of the balance
    /// image schema (it shares [`SourceDomain::Balance`] with
    /// [`SchemaId::Balance`] rather than owning a domain of its own).
    MoralAccounting,
    /// The return story: a cyclic event recurs because all things return.
    CycleReturn,
    /// The final-cause story: a scalar state reflects an entity's
    /// intrinsic tendency toward its own completed nature (an Aristotelian
    /// telos read as an entelechy-force — it shares
    /// [`SourceDomain::Force`] with [`SchemaId::ForceDynamics`]; a
    /// tendency toward a result is itself a force in Talmy's sense).
    EssenceTelos,
    /// The up/down story: a scalar state is explained by vertical position.
    Verticality,
}

/// The slot a schema's binding step must fill from the culture's own
/// furniture, or none. [`SchemaId::Agentive`] and [`SchemaId::LinkSympathy`]
/// both bind [`SlotKind::Agent`] (a deity name via `beliefs_held_by`) —
/// [`SchemaId::LinkSympathy`] joined this set at C6 T1, honestly reflecting
/// that its surface frame ("… answers ⟨deity⟩.") always named a deity even
/// though it carries no lexeme table of its own (worldgen's `agent_bearing`
/// used to special-case it explicitly; that special case is now redundant,
/// deleted at Task 2). [`SchemaId::Kinship`] binds [`SlotKind::Kin`]. Every
/// other schema carries `None`: furniture-free closed text.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SlotKind {
    /// Binds a named agent (a deity).
    Agent,
    /// Binds a kin relation.
    Kin,
    /// No slot.
    None,
}

/// One row of the closed schema library: which source-domain it reads,
/// which fact-shapes admit it, and what slot (if any) its binding step
/// must fill. Constructed only inside [`schema_table`].
///
/// `id`, `source`, `slot`, and `shapes` are all closed enum/slice types, not
/// tracked primitives — `mediation` (C6 T1) is the struct's only bare
/// primitive field, hence the only one the type-audit tag below names (its
/// predecessor tag over-tagged the enum fields; harmless while the struct
/// had no tracked primitive at all, since an all-non-primitive struct is
/// never even entered into the audit, but corrected now that `mediation`
/// makes this struct a real audited item).
/// type-audit: bare-ok(ratio: mediation)
///
/// `PartialEq` only (not `Eq`): the new `mediation: f64` field is not
/// `Eq`-eligible, same as [`crate::account::Observability`]'s `PartialEq`-
/// only derive for the same reason.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Schema {
    /// This row's schema id.
    pub id: SchemaId,
    /// The embodied source-domain this schema's causal story reads from.
    pub source: SourceDomain,
    /// Every fact-shape this schema is a candidate to explain — the gate
    /// [`admitted`] filters on. Never empty (the closure test asserts
    /// this): a schema with no admitted shape would be dead library
    /// weight.
    pub shapes: &'static [FactShape],
    /// The slot this schema's binding step must fill, or `None` for an
    /// agentless, frame-only schema.
    pub slot: SlotKind,
    /// C6 (The Doctrine): the authored, global, closed multiplier the
    /// institution's account applies to this schema's folk prior weight
    /// BEFORE β-sharpening (`doctrine prior = folk prior × mediation`) —
    /// one preregistered value per row, frozen in the plan header, never
    /// derived from a culture's own traits (the same anti-astrology
    /// posture [`select_schema`]'s module doc argues for the schema draw
    /// itself: the institution's thumb on the scale is authored doctrine,
    /// not a measured cultural fact).
    pub mediation: f64,
}

/// The closed 12-row schema library, in LANG-37's authored order
/// (force-dynamics, agentive, substance-flow, container, path-journey,
/// balance, link-sympathy, kinship, moral-accounting, cycle-return,
/// essence-telos, verticality). [`admitted`] reads this table in this
/// exact order, so the order here is itself a save-format-adjacent
/// contract for every downstream pinned assertion — changing it is a
/// deliberate library edit, not a refactor.
///
/// At the floor only two fact-shapes actually fire a schema (spec §3.3:
/// the day's `CyclicEvent` and the moons' `Count`, where kept); the other
/// six schemas below (force-dynamics, substance-flow, container,
/// moral-accounting, essence-telos, verticality) admit
/// [`FactShape::HighScalarState`] instead — their natural scalar-state
/// story — so every row still satisfies the closure test's non-empty
/// requirement without perturbing the two shapes real callers gate on.
/// `Taxonomy` and `Roster` are admitted by no row at the floor.
pub fn schema_table() -> &'static [Schema] {
    &[
        Schema {
            id: SchemaId::ForceDynamics,
            source: SourceDomain::Force,
            shapes: &[FactShape::HighScalarState],
            slot: SlotKind::None,
            mediation: 1.0,
        },
        Schema {
            id: SchemaId::Agentive,
            source: SourceDomain::Motion,
            shapes: &[FactShape::CyclicEvent, FactShape::Count],
            slot: SlotKind::Agent,
            mediation: 1.5,
        },
        Schema {
            id: SchemaId::SubstanceFlow,
            source: SourceDomain::Flow,
            shapes: &[FactShape::HighScalarState],
            slot: SlotKind::None,
            mediation: 1.0,
        },
        Schema {
            id: SchemaId::Container,
            source: SourceDomain::Container,
            shapes: &[FactShape::HighScalarState],
            slot: SlotKind::None,
            mediation: 1.0,
        },
        Schema {
            id: SchemaId::PathJourney,
            source: SourceDomain::Path,
            shapes: &[FactShape::CyclicEvent],
            slot: SlotKind::None,
            mediation: 0.6,
        },
        Schema {
            id: SchemaId::Balance,
            source: SourceDomain::Balance,
            shapes: &[FactShape::CyclicEvent],
            slot: SlotKind::None,
            mediation: 0.8,
        },
        Schema {
            id: SchemaId::LinkSympathy,
            source: SourceDomain::Link,
            shapes: &[FactShape::Count],
            // C6 T1 (The Doctrine): the C5 follow-up this campaign closes
            // out — LinkSympathy's Task-4 surface frame ("… answers
            // ⟨Deity⟩.") always named a deity even though this row read
            // `SlotKind::None`, forcing worldgen's `agent_bearing` to carry
            // a `|| schema == SchemaId::LinkSympathy` special case. Making
            // the table honest here is additive for that caller: its check
            // is `matches!(slot, Some(Agent) | Some(Kin)) ||
            // schema == LinkSympathy`, so this row now satisfying the left
            // disjunct on its own leaves the OR's result — and therefore
            // every worldgen test — unchanged. Task 2 deletes the
            // now-redundant disjunct.
            slot: SlotKind::Agent,
            mediation: 1.2,
        },
        Schema {
            id: SchemaId::Kinship,
            source: SourceDomain::Kinship,
            shapes: &[FactShape::Count],
            slot: SlotKind::Kin,
            mediation: 1.0,
        },
        Schema {
            id: SchemaId::MoralAccounting,
            source: SourceDomain::Balance,
            shapes: &[FactShape::HighScalarState],
            slot: SlotKind::None,
            mediation: 1.5,
        },
        Schema {
            id: SchemaId::CycleReturn,
            source: SourceDomain::Cycle,
            shapes: &[FactShape::CyclicEvent],
            slot: SlotKind::None,
            mediation: 0.6,
        },
        Schema {
            id: SchemaId::EssenceTelos,
            source: SourceDomain::Force,
            shapes: &[FactShape::HighScalarState],
            slot: SlotKind::None,
            mediation: 1.4,
        },
        Schema {
            id: SchemaId::Verticality,
            source: SourceDomain::Verticality,
            shapes: &[FactShape::HighScalarState],
            slot: SlotKind::None,
            mediation: 1.0,
        },
    ]
}

/// Every schema admitted for `shape`, in [`schema_table`]'s authored row
/// order (never re-sorted) — the fact-shape gate every selection draw
/// filters through before [`select_schema`] ever runs.
pub fn admitted(shape: FactShape) -> Vec<SchemaId> {
    schema_table()
        .iter()
        .filter(|row| row.shapes.contains(&shape))
        .map(|row| row.id)
        .collect()
}

/// A cyclic deity's told pace: its period's rank among the culture's own
/// cyclic beliefs (fully emic — spec §3.2 ledger #5), never an etic
/// standard-days anchor. `Brisk` for the shortest period, `Slow` for the
/// longest, `Neutral` for a sole or middle-ranked belief (no adverb
/// surfaces).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Manner {
    /// The shortest-period cyclic belief in this culture's pantheon.
    Brisk,
    /// The longest-period cyclic belief in this culture's pantheon.
    Slow,
    /// A sole or middle-ranked cyclic belief; no adverb surfaces.
    Neutral,
}

/// A closed verb/frame key — the identifier [`lexemes_for`] hands back and
/// [`select_schema`]'s caller ultimately renders. Never constructed
/// outside this module's tables; the wrapped text is always one of the
/// closed lexeme-table entries below.
/// type-audit: bare-ok(identifier-text: 0)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LexemeId(pub &'static str);

/// The subsistence-derived motion sub-frame gating which verb variant an
/// agentive explanation draws (the caller maps a culture's `Subsistence`
/// to one of these — Herding → `Mounted`, Fishing → `Rowing`,
/// Farming`/`Foraging → `Walking`; `Stalking` is a hunter/predator
/// variant of the walking frame).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SubFrame {
    /// On foot: "walks", "strides".
    Walking,
    /// Astride a mount: "rides", "drives".
    Mounted,
    /// By boat: "rows", "steers".
    Rowing,
    /// A predator's gait: "walks", "stalks".
    Stalking,
}

/// The closed lexeme table for [`SchemaId::Agentive`] (the agentive/motion
/// read) — the only schema with a lexeme table at all; see [`lexemes_for`].
fn agentive_lexemes(sub: SubFrame) -> &'static [LexemeId] {
    match sub {
        SubFrame::Walking => &[LexemeId("walks"), LexemeId("strides")],
        SubFrame::Mounted => &[LexemeId("rides"), LexemeId("drives")],
        SubFrame::Rowing => &[LexemeId("rows"), LexemeId("steers")],
        SubFrame::Stalking => &[LexemeId("walks"), LexemeId("stalks")],
    }
}

/// The closed candidate verbs for `schema` under sub-frame `sub`. Only
/// [`SchemaId::Agentive`] has a lexeme table at all — every other schema
/// (including [`SchemaId::Kinship`] and [`SchemaId::LinkSympathy`], the two
/// other `Count` explainers, both of which bind a slot but still draw no
/// verb) is frame-only at the floor: its surface is one of Task 4's closed
/// frame strings, never a drawn verb, so this always returns an empty slice
/// for them.
pub fn lexemes_for(schema: SchemaId, sub: SubFrame) -> &'static [LexemeId] {
    match schema {
        SchemaId::Agentive => agentive_lexemes(sub),
        _ => &[],
    }
}

/// The β-sharpened weighted schema draw: each admitted schema's *positive*
/// weight is raised to `weight.powf(beta)` (via
/// [`hornvale_kernel::math::powf`], the portable libm route — never the
/// inherent `f64::powf`, so the draw is bit-identical across platforms);
/// a non-positive weight sharpens to `0.0` rather than being handed to
/// `powf` — an even `beta` would otherwise flip a deliberately-excluded
/// non-positive weight back to a positive one (`(-1.0).powf(2.0) == 1.0`),
/// silently reviving a candidate the caller meant to zero out. Then
/// [`Stream::weighted_index`] draws among the sharpened weights (which
/// normalizes internally). `beta == 1.0` leaves the relative weights
/// unchanged; `beta > 1.0` sharpens toward the heaviest weight (the
/// monomania dial); `None` when `admitted` is empty or every weight is
/// non-positive.
/// type-audit: bare-ok(ratio: admitted), bare-ok(ratio: beta)
pub fn select_schema(
    admitted: &[(SchemaId, f64)],
    beta: f64,
    stream: &mut Stream,
) -> Option<SchemaId> {
    if admitted.is_empty() {
        return None;
    }
    let sharpened: Vec<f64> = admitted
        .iter()
        .map(|(_, w)| if *w > 0.0 { math::powf(*w, beta) } else { 0.0 })
        .collect();
    let index = stream.weighted_index(&sharpened)?;
    Some(admitted[index].0)
}

/// A uniform deterministic draw among `candidates` (the gate-surviving
/// lexemes for one schema/sub-frame pair) — `None` only for an empty slice
/// (an agentless schema's [`lexemes_for`] result).
pub fn select_lexeme(candidates: &'static [LexemeId], stream: &mut Stream) -> Option<LexemeId> {
    stream.pick(candidates).copied()
}

/// The four-way relationship between a culture's folk account and its
/// institution's doctrine account, for one ground fact (C6, The Doctrine —
/// spec's preregistered conflict map). [`conflict_of`] is the only
/// constructor.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ConflictState {
    /// Folk and doctrine agree: their effective dispositions match, and —
    /// when both accounts hold the fact `Explained` — the causal schema
    /// each draws matches too.
    Harmony,
    /// Folk and doctrine disagree, and the folk voice could in principle
    /// have checked: either the effective dispositions differ, or both are
    /// `Explained` under different schemas.
    Contested,
    /// Folk and doctrine disagree, but the folk voice has no way to check
    /// (the same differ-condition as [`Self::Contested`], gated by
    /// `folk_verifiable: false` instead).
    Mystery,
    /// The institution holds a fact as true (`Kept`, however it got there)
    /// that the folk voice never received at all (`Lost`) — the
    /// disclosure asymmetry the exoteric/esoteric Book editions (Task 4)
    /// exist to surface, checked before the generic differ/agree split so
    /// it wins even when doctrine's `Kept` came wrapped in `Explained`.
    RevealedClaim,
}

/// The preregistered conflict classifier (C6 T1, pure): compares one ground
/// fact's folk and doctrine [`Disposition`]s through the SAME dial-blind
/// seam every other measure in `crate::account` uses (`effective`) — this
/// function itself never matches on `Disposition::Explained` to decide the
/// effective-equality question,
/// only to read out the wrapped `schema` for the harmony-breaking tie the
/// plan header calls out ("if both Explained, schemas equal too").
/// `folk_verifiable` is the caller-derived flag (worldgen's Task 3 owns
/// translating an `Observability::requirement` into this bool via the
/// preregistered per-`Requirement` table — `Manifest` verifiable,
/// `SkyGraded{t}` verifiable iff the folk culture's own sky-capability
/// meets `t`, `Instrumental`/`CrossReferential`/`Taxonomic` never
/// verifiable); this function only ever consumes the bool, never derives
/// it, keeping the classifier ignorant of any sibling domain's fact
/// shape.
/// type-audit: bare-ok(flag: folk_verifiable)
pub fn conflict_of(
    folk: &Disposition,
    doctrine: &Disposition,
    folk_verifiable: bool,
) -> ConflictState {
    let folk_eff = effective(folk);
    let doctrine_eff = effective(doctrine);

    // RevealedClaim first: it wins regardless of whether doctrine's `Kept`
    // arrived bare or wrapped in `Explained` — either way `doctrine_eff`
    // reduces to `Kept` (effective()'s recursive unwrap), and the folk
    // voice never received the fact at all.
    if matches!(doctrine_eff, Disposition::Kept) && matches!(folk_eff, Disposition::Lost(_)) {
        return ConflictState::RevealedClaim;
    }

    let agrees = if folk_eff == doctrine_eff {
        match (folk, doctrine) {
            (
                Disposition::Explained { schema: fs, .. },
                Disposition::Explained { schema: ds, .. },
            ) => fs == ds,
            _ => true,
        }
    } else {
        false
    };

    if agrees {
        ConflictState::Harmony
    } else if folk_verifiable {
        ConflictState::Contested
    } else {
        ConflictState::Mystery
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::account::LossReason;
    use hornvale_kernel::Seed;

    /// Every `SchemaId` variant, for the closure test's exhaustiveness
    /// check (the enum itself gives no runtime reflection).
    const ALL_SCHEMA_IDS: [SchemaId; 12] = [
        SchemaId::ForceDynamics,
        SchemaId::Agentive,
        SchemaId::SubstanceFlow,
        SchemaId::Container,
        SchemaId::PathJourney,
        SchemaId::Balance,
        SchemaId::LinkSympathy,
        SchemaId::Kinship,
        SchemaId::MoralAccounting,
        SchemaId::CycleReturn,
        SchemaId::EssenceTelos,
        SchemaId::Verticality,
    ];

    #[test]
    fn the_schema_table_is_closed_and_complete() {
        let t = schema_table();
        assert_eq!(t.len(), 12, "LANG-37's closed library");
        for id in ALL_SCHEMA_IDS {
            assert_eq!(
                t.iter().filter(|row| row.id == id).count(),
                1,
                "{id:?} should appear exactly once"
            );
        }
        for row in t {
            assert!(
                !row.shapes.is_empty(),
                "{:?} must admit at least one fact-shape",
                row.id
            );
        }
    }

    #[test]
    fn admissions_match_the_preregistered_gates() {
        assert_eq!(
            admitted(FactShape::CyclicEvent),
            vec![
                SchemaId::Agentive,
                SchemaId::PathJourney,
                SchemaId::Balance,
                SchemaId::CycleReturn
            ]
        );
        assert_eq!(
            admitted(FactShape::Count),
            vec![
                SchemaId::Agentive,
                SchemaId::LinkSympathy,
                SchemaId::Kinship
            ]
        );
        // The dead-metaphor double-dip guard (LANG-38's live/dead split,
        // C5 T5): a lexicon compound like `sea` (`packs::compound_recipe`
        // — "many water") is a DEAD, lexicalized-but-inert mapping, never
        // a live productive schema. `Taxonomy` (`is-a`) and `Roster`
        // (`instance-of`) facts are concept-classification facts, not
        // day/moon phenomena; if either shape ever admitted a schema, the
        // causal-filter engine (`windows/worldgen::chorus::explain`) could
        // fire an "explanation" on a lexicon compound, collapsing the
        // live/dead split LANG-38 deliberately keeps un-collapsed. No
        // `Explained` entry may ever target a concept-classify fact — this
        // is structurally true today (no admission row in `schema_table()`
        // lists either shape), so this test pins the closure rather than
        // asserting behavior: it reddens the instant a future schema
        // addition admits `Taxonomy`/`Roster`, forcing that change to be
        // deliberate rather than an oversight.
        assert!(admitted(FactShape::Taxonomy).is_empty());
        assert!(admitted(FactShape::Roster).is_empty());
    }

    #[test]
    fn beta_sharpening_is_monotone_and_deterministic() {
        let weights = vec![
            (SchemaId::Agentive, 3.0),
            (SchemaId::PathJourney, 1.0),
            (SchemaId::Balance, 1.0),
        ];

        // Determinism: same seed, same weights, same beta -> same pick, twice.
        let pick_a = select_schema(&weights, 2.0, &mut Seed(7).stream());
        let pick_b = select_schema(&weights, 2.0, &mut Seed(7).stream());
        assert_eq!(pick_a, pick_b);
        assert!(pick_a.is_some());

        // beta == 1.0 leaves relative weights alone: the total probability
        // mass on the heaviest schema is exactly its own weight share
        // (3/5), so measure it the same way as the high-beta check below
        // for a direct before/after comparison.
        let mut wins_beta_1 = 0;
        let mut wins_beta_8 = 0;
        for i in 0..100u64 {
            let label = format!("test/schemas/beta-sweep/{i}");
            if select_schema(&weights, 1.0, &mut Seed(1).derive(&label).stream())
                == Some(SchemaId::Agentive)
            {
                wins_beta_1 += 1;
            }
            if select_schema(&weights, 8.0, &mut Seed(1).derive(&label).stream())
                == Some(SchemaId::Agentive)
            {
                wins_beta_8 += 1;
            }
        }
        assert!(
            wins_beta_8 >= 95,
            "beta=8.0 should collapse onto the top-weighted schema in >=95/100 draws, got {wins_beta_8}"
        );
        assert!(
            wins_beta_8 > wins_beta_1,
            "sharpening (beta=8.0) must win the top schema strictly more often than beta=1.0 \
             (got {wins_beta_8} vs {wins_beta_1})"
        );
    }

    #[test]
    fn selection_is_none_on_empty_admissions() {
        assert_eq!(select_schema(&[], 2.0, &mut Seed(1).stream()), None);
    }

    #[test]
    fn selection_is_none_on_all_nonpositive_weights() {
        let weights = vec![(SchemaId::Agentive, 0.0), (SchemaId::Balance, -1.0)];
        assert_eq!(select_schema(&weights, 2.0, &mut Seed(1).stream()), None);
    }

    #[test]
    fn lexeme_tables_are_closed_and_subframe_gated() {
        assert_eq!(
            lexemes_for(SchemaId::Agentive, SubFrame::Mounted)
                .iter()
                .map(|l| l.0)
                .collect::<Vec<_>>(),
            vec!["rides", "drives"]
        );
        assert_eq!(
            lexemes_for(SchemaId::Agentive, SubFrame::Walking)
                .iter()
                .map(|l| l.0)
                .collect::<Vec<_>>(),
            vec!["walks", "strides"]
        );
        assert_eq!(
            lexemes_for(SchemaId::Agentive, SubFrame::Rowing)
                .iter()
                .map(|l| l.0)
                .collect::<Vec<_>>(),
            vec!["rows", "steers"]
        );
        assert_eq!(
            lexemes_for(SchemaId::Agentive, SubFrame::Stalking)
                .iter()
                .map(|l| l.0)
                .collect::<Vec<_>>(),
            vec!["walks", "stalks"]
        );
        assert!(
            lexemes_for(SchemaId::CycleReturn, SubFrame::Walking).is_empty(),
            "agentless schemas have no verb table"
        );
        assert!(
            lexemes_for(SchemaId::Kinship, SubFrame::Walking).is_empty(),
            "kinship is frame-only too, at the floor"
        );
        assert!(
            lexemes_for(SchemaId::LinkSympathy, SubFrame::Walking).is_empty(),
            "link-sympathy is frame-only too, at the floor"
        );
    }

    #[test]
    fn select_lexeme_is_deterministic_and_none_on_empty() {
        let candidates = lexemes_for(SchemaId::Agentive, SubFrame::Mounted);
        let a = select_lexeme(candidates, &mut Seed(3).stream());
        let b = select_lexeme(candidates, &mut Seed(3).stream());
        assert_eq!(a, b);
        assert!(a.is_some());

        assert_eq!(select_lexeme(&[], &mut Seed(3).stream()), None);
    }

    /// The mediation column (C6 T1) is authored, global, and closed — one
    /// preregistered value per row, frozen in the plan header. Exact,
    /// no-tolerance equality: this is an authored-table pin, not a
    /// measurement.
    #[test]
    fn mediation_values_match_the_preregistered_column() {
        fn mediation_of(id: SchemaId) -> f64 {
            schema_table()
                .iter()
                .find(|row| row.id == id)
                .expect("every SchemaId has exactly one row")
                .mediation
        }

        assert_eq!(mediation_of(SchemaId::Agentive), 1.5);
        assert_eq!(mediation_of(SchemaId::MoralAccounting), 1.5);
        assert_eq!(mediation_of(SchemaId::EssenceTelos), 1.4);
        assert_eq!(mediation_of(SchemaId::LinkSympathy), 1.2);
        assert_eq!(mediation_of(SchemaId::Kinship), 1.0);
        assert_eq!(mediation_of(SchemaId::Container), 1.0);
        assert_eq!(mediation_of(SchemaId::ForceDynamics), 1.0);
        assert_eq!(mediation_of(SchemaId::SubstanceFlow), 1.0);
        assert_eq!(mediation_of(SchemaId::Verticality), 1.0);
        assert_eq!(mediation_of(SchemaId::Balance), 0.8);
        assert_eq!(mediation_of(SchemaId::PathJourney), 0.6);
        assert_eq!(mediation_of(SchemaId::CycleReturn), 0.6);
    }

    /// The C5 follow-up this campaign closes (C6 T1): `LinkSympathy`'s row
    /// now reads `SlotKind::Agent`, matching its always-deity-naming
    /// surface frame; `Kinship` stays `Kin`; the agentless cyclic-event
    /// schemas stay `None`.
    #[test]
    fn link_sympathy_is_agent_bearing_in_the_table() {
        fn slot_of(id: SchemaId) -> SlotKind {
            schema_table()
                .iter()
                .find(|row| row.id == id)
                .expect("every SchemaId has exactly one row")
                .slot
        }

        assert_eq!(slot_of(SchemaId::LinkSympathy), SlotKind::Agent);
        assert_eq!(slot_of(SchemaId::Kinship), SlotKind::Kin);
        assert_eq!(slot_of(SchemaId::CycleReturn), SlotKind::None);
        assert_eq!(slot_of(SchemaId::PathJourney), SlotKind::None);
        assert_eq!(slot_of(SchemaId::Balance), SlotKind::None);
    }

    /// The preregistered conflict grid (C6 T1): every named case from the
    /// plan header, checked directly against [`conflict_of`].
    #[test]
    fn conflict_states_cover_the_preregistered_grid() {
        // Harmony: identical Kept vs Kept.
        assert_eq!(
            conflict_of(&Disposition::Kept, &Disposition::Kept, true),
            ConflictState::Harmony,
            "identical Kept dispositions agree regardless of verifiability"
        );

        // Harmony: identical Explained schemas (same underlying, same
        // schema) — full agreement, not just effective equality.
        let explained_agentive_a = Disposition::Explained {
            underlying: Box::new(Disposition::Kept),
            schema: SchemaId::Agentive,
            agent: Some("Nggo".to_string()),
            lexeme: Some(LexemeId("walks")),
            manner: Manner::Neutral,
        };
        let explained_agentive_b = Disposition::Explained {
            underlying: Box::new(Disposition::Kept),
            schema: SchemaId::Agentive,
            agent: Some("Vamu".to_string()),
            lexeme: Some(LexemeId("strides")),
            manner: Manner::Brisk,
        };
        assert_eq!(
            conflict_of(&explained_agentive_a, &explained_agentive_b, true),
            ConflictState::Harmony,
            "same underlying and same schema is Harmony even with different agent/lexeme/manner"
        );

        // Contested: same underlying (both wrap Kept, so effective is
        // equal), but different schemas, and the folk voice is verifiable.
        let explained_path = Disposition::Explained {
            underlying: Box::new(Disposition::Kept),
            schema: SchemaId::PathJourney,
            agent: None,
            lexeme: None,
            manner: Manner::Neutral,
        };
        let explained_agentive = Disposition::Explained {
            underlying: Box::new(Disposition::Kept),
            schema: SchemaId::Agentive,
            agent: Some("Nggo".to_string()),
            lexeme: Some(LexemeId("walks")),
            manner: Manner::Neutral,
        };
        assert_eq!(
            conflict_of(&explained_path, &explained_agentive, true),
            ConflictState::Contested,
            "differing schemas over the same effective disposition, verifiable -> Contested"
        );

        // Mystery: the identical pair, but the folk voice cannot verify.
        assert_eq!(
            conflict_of(&explained_path, &explained_agentive, false),
            ConflictState::Mystery,
            "the same differing-schema pair, unverifiable -> Mystery"
        );

        // RevealedClaim: folk Lost vs doctrine bare Kept.
        let folk_lost = Disposition::Lost(LossReason::NoConcept);
        assert_eq!(
            conflict_of(&folk_lost, &Disposition::Kept, true),
            ConflictState::RevealedClaim,
            "doctrine holds a fact the folk voice never received"
        );
        assert_eq!(
            conflict_of(&folk_lost, &Disposition::Kept, false),
            ConflictState::RevealedClaim,
            "RevealedClaim does not depend on folk_verifiable"
        );

        // RevealedClaim: folk Lost vs doctrine Explained{underlying: Kept}
        // — the wrapped form collapses to the same effective Kept.
        let doctrine_explained_kept = Disposition::Explained {
            underlying: Box::new(Disposition::Kept),
            schema: SchemaId::MoralAccounting,
            agent: None,
            lexeme: None,
            manner: Manner::Neutral,
        };
        assert_eq!(
            conflict_of(&folk_lost, &doctrine_explained_kept, true),
            ConflictState::RevealedClaim,
            "a doctrine Explained-wrapping-Kept still reveals a claim the folk voice lacks"
        );

        // Explained-wrapping transparency: Explained{Kept} vs bare Kept,
        // same effective disposition -> Harmony, regardless of the wrapped
        // schema (only fires the schema-comparison arm when BOTH sides are
        // literally Explained).
        assert_eq!(
            conflict_of(&doctrine_explained_kept, &Disposition::Kept, true),
            ConflictState::Harmony,
            "one side bare Kept, the other Explained-wrapping-Kept: schema is irrelevant, compares effective"
        );
    }
}
