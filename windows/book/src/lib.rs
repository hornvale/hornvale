//! The Book window: render a world's committed classification facts as
//! Common sentences. Reads only the ledger; realizes via `domains/language`.
//!
//! Aggregation seam (C2 T4): `domains/language`'s `ClauseSpec.modifiers`
//! only ever joins fragments into one clause's tail ("`X`, `Y`"), and stays
//! that way — the seam between a noun-modifier ("with two moons") and a
//! trailing independent clause ("its day lasts about 1.5 standard days")
//! belongs here, at the one call site that knows a sentence is about to be
//! finished. [`fragment_for`] tags each fact's rendering as one or the
//! other; [`render_volume`] strips the realized clause's terminal period
//! and re-joins any trailing clauses with `"; "` before restoring it.
#![warn(missing_docs)]

use hornvale_astronomy::facts::{DAY_LENGTH_STD, MOON_COUNT, STAR_CLASS};
use hornvale_kernel::{EntityId, Value, World};
use hornvale_language::account::{Account, AccountEntry, AccountParams, Disposition, Stance};
use hornvale_language::clause::{
    ClauseSpec, Definiteness, Frame, Number, ParseContext, ParseError, Subject, cardinal,
    parse_common, quantity, realize_common,
};
use hornvale_language::schemas::Manner;
use hornvale_language::{
    ConflictState, Evidential, LexemeId, NounClass, SchemaId, TongueClause, TongueMorphology,
    conflict_of, realize_tongue_deep, tongue_grammar,
};
use std::collections::{BTreeMap, BTreeSet};

/// One world's volume of The Book: the seed it was rendered from plus the
/// sentences the ledger's `is-a` and `instance-of` facts realize.
/// type-audit: bare-ok(constructor-edge: seed), bare-ok(prose: lines), bare-ok(prose: tongue_lines), bare-ok(prose: tongue_gaps)
pub struct BookVolume {
    /// The seed that generated the world this volume renders.
    pub seed: u64,
    /// One Common sentence per rendered `is-a` fact (ledger commit order),
    /// then one per rendered `instance-of` fact (C2 T5: a placed peopled
    /// species' collective, "The ⟨Autonym⟩ are ⟨species⟩.").
    pub lines: Vec<String>,
    /// C3 T3, deepened by C7: one self-statement per placed people, plus
    /// (C7) one emic world-statement per placed people, both realized in
    /// its own tongue (`realize_tongue_deep` over that people's
    /// `TongueGrammar`/`TongueMorphology` and lexicon, `Evidential::Witnessed`)
    /// — "⟨autonym⟩ ⟨copula?⟩ ⟨own-kind⟩." and "⟨planet⟩ ⟨copula?⟩ ⟨their
    /// carving of "earth"⟩.", each glossed with its matching Common
    /// sentence. The self-statement law (spec §5) and the world-statement
    /// law (C7 §3.5/§4.5): autonym/own-kind and the planet/"earth" concept
    /// are all Steeped by construction, so neither ever gaps — see
    /// `every_placed_people_self_states_in_its_own_tongue` and
    /// `every_people_states_the_world_in_its_tongue`.
    pub tongue_lines: Vec<String>,
    /// C3 T3: the per-tongue coverage report — one gap line per placed
    /// people recording that its tongue cannot yet state the planet's own
    /// kind (no culture holds the `planet` concept; spec §5's gap law).
    pub tongue_gaps: Vec<String>,
    /// C4 T4: one chorus section per placed people with a committed
    /// collective — the same ground truth composed through that culture's
    /// epistemic account (`hornvale_worldgen::accounts_of`). The
    /// null-filter law (spec §4.1): an identity account's section
    /// reproduces `lines` byte-identically — see
    /// `identity_chorus_reproduces_the_gods_eye_lines`.
    pub chorus: Vec<ChorusSection>,
    /// C8 (The Diachronic Book): the Book's time axis — one
    /// [`ReckoningEpoch`] per preregistered epoch (day 0 and the
    /// hundredth year, `36_525.0` standard days; see [`reckoning_epochs`]),
    /// always the same fixed pair regardless of any `--at` lens the CLI
    /// renders separately ([`reckoning_at`]). Zero new draws/facts: pure
    /// derivation over `hornvale_worldgen::{observations_of, ladder_of}`.
    pub reckoning: Vec<ReckoningEpoch>,
}

/// One placed people's chorus section (C4 T4): its epistemic account,
/// composed into an emic paragraph (Common, in the culture's own salience
/// order) plus a sparse etic margin carrying exactly what the account's
/// filters lost or corrupted (the margin law, spec §4.3).
/// type-audit: bare-ok(identifier-text: kind), bare-ok(prose: heading), bare-ok(prose: emic), bare-ok(prose: margin)
pub struct ChorusSection {
    /// The people's kind label (e.g. `"goblin"`).
    pub kind: String,
    /// `"As the ⟨autonym⟩ tell it"` — scaffolding, not a Book corpus line.
    pub heading: String,
    /// The emic paragraph: one sentence per classification subject this
    /// culture's account keeps or substitutes, in the account's own order.
    pub emic: Vec<String>,
    /// The etic margin: one sentence per subject owning at least one
    /// `Lost`/`Substituted` entry, carrying only that lost/corrupted
    /// content (sparseness — never repeats what the emic paragraph already
    /// states).
    pub margin: Vec<String>,
    /// C6 (The Doctrine): this culture's doctrine section, when its
    /// flagship's committed `cult-form` fact gates it in (the SOC-1 gate,
    /// `hornvale_worldgen::doctrine_of`) — `None` for a folk-cult-form
    /// culture. The folk registers above (`emic`/`margin`) are
    /// byte-unchanged by this campaign regardless of this field.
    pub doctrine: Option<DoctrineSection>,
}

/// C6 (The Doctrine): one organized culture's doctrine section — the
/// institution's second account (the priesthood's own composition of the
/// SAME ground truth, run through `hornvale_worldgen::doctrine_of`'s four
/// preregistered deltas), split into four registers rather than the folk
/// section's two: `tongue_taught_line` (C7: the in-tongue taught contrast),
/// `emic` (the doctrine's own paragraph, with the `RevealedClaim`
/// substitution), `annotations` (the disclosure law's `Contested`
/// counter-lines — a separate register, never interleaved into `emic`), and
/// `margin` (the doctrine account's own etic margin, the same law as any
/// other voice). See [`doctrine_section`] for the construction.
/// type-audit: bare-ok(prose: heading), bare-ok(prose: tongue_taught_line), bare-ok(prose: emic), bare-ok(prose: annotations), bare-ok(prose: margin)
pub struct DoctrineSection {
    /// `"As the priesthood of the ⟨Autonym⟩ teach it"` — scaffolding, not
    /// a Book corpus line.
    pub heading: String,
    /// C7 T3: the doctrine's ONE in-tongue taught line — the SAME emic
    /// world-statement the Tongues section renders (same subject, same
    /// `"earth"` complement), but with `Evidential::Taught` rather than
    /// `Witnessed` — the visible morphological folk/doctrine contrast on one
    /// proposition (spec §3.5, the taught-contrast law). Glossed the same
    /// way the Tongues section glosses its lines, plus the doctrine's own
    /// "— as it is taught" marker.
    pub tongue_taught_line: String,
    /// The doctrine's own emic paragraph, in the doctrine account's own
    /// order — a `RevealedClaim` entry's construction fragment is replaced
    /// by the closed exoteric formula ([`revealed_claim_line`]) rather than
    /// rendered as an ordinary modifier.
    pub emic: Vec<String>,
    /// The disclosure law's counter-annotations (the "Galileo cell"): one
    /// line per `Contested` entry ([`counter_annotation_line`]), quoting
    /// the folk voice's own rendered explanation for that same fact.
    pub annotations: Vec<String>,
    /// The doctrine account's own etic margin — [`render_world_margin`],
    /// unmodified.
    pub margin: Vec<String>,
}

/// C8 (The Diachronic Book): one epoch's Reckoning-of-Years section — the
/// observation ledger at a fixed day `T`, read back through
/// `hornvale_worldgen::{observations_of, ladder_of}` for every placed
/// culture (see [`reckoning_epoch`]). `lines` is empty-arm-or-per-culture
/// (never both): the empty arm (`"The sky keeps no dates to number."`)
/// when the true event count at `T` is zero, else one run of lines per
/// placed culture at `Counted`+ (the folk line, then — organized cultures
/// only — the Numbered line, then — `Predictive` only — the prediction
/// line). `margin` carries the truth-margin sentence (spec §3.4) exactly
/// when some culture's held knowledge falls short of the true count —
/// empty otherwise, the same sparseness convention `ChorusSection::margin`
/// already follows.
/// type-audit: bare-ok(prose: heading), bare-ok(prose: lines), bare-ok(prose: margin)
pub struct ReckoningEpoch {
    /// `"In the first days"` / `"In the hundredth year"` for the committed
    /// pair; an ad hoc `"At day ⟨N⟩"` for the CLI's `--at` lens.
    pub heading: String,
    /// The empty arm, or one run of lines per placed culture (registry
    /// order) — see the struct doc.
    pub lines: Vec<String>,
    /// The truth-margin sentence, present iff some culture's held count
    /// falls short of the true (unwitnessed-included) count — at most one
    /// line.
    pub margin: Vec<String>,
}

/// One fact's rendering, tagged by how it joins the sentence: a noun
/// modifier folded into the main clause's tail, or an independent trailing
/// clause appended after a semicolon (see the module doc's aggregation
/// seam).
enum Fragment {
    /// Joins `ClauseSpec.modifiers`, e.g. `"with two moons"`.
    Modifier(String),
    /// Appended after the main clause, semicolon-joined, e.g. `"its day
    /// lasts about 1.5 standard days"`.
    Trailing(String),
}

/// `"a"` or `"an"`, by `word`'s first letter. Duplicated from
/// `domains/language::clause`'s private helper of the same name rather than
/// exposed from there — this predicate-specific modifier text is windows/
/// book's own construction, and the aggregation seam keeps
/// `domains/language` untouched (see the module doc).
fn indefinite_article(word: &str) -> &'static str {
    match word.chars().next().map(|c| c.to_ascii_lowercase()) {
        Some('a' | 'e' | 'i' | 'o' | 'u') => "an",
        _ => "a",
    }
}

/// An `instance-of` collective's species complement, pluralized: a naive
/// regular English plural (append `"s"`) — every peopled kind in today's
/// roster (goblin, hobgoblin, kobold, bugbear) pluralizes regularly, so no
/// irregular table exists yet. `kind` is the species `KindId` label
/// committed as the `instance-of` fact's object (e.g. `"goblin"`).
/// type-audit: bare-ok(identifier-text: kind)
fn species_label(kind: &str) -> String {
    format!("{kind}s")
}

/// Join a realized clause with its trailing independent clause(s) — the
/// aggregation seam's shared assembly tail, used by both [`render_volume`]
/// (forward) and [`rerender`] (the corpus law's re-realization check).
/// `realize_common` always terminates `line` with `'.'`: strip it, join
/// each trailing clause with `"; "`, then restore the final period. A
/// no-op when `trailing` is empty (returns `line` unchanged).
fn assemble_trailing(mut line: String, trailing: &[String]) -> String {
    if trailing.is_empty() {
        return line;
    }
    line.pop();
    for t in trailing {
        line.push_str("; ");
        line.push_str(t);
    }
    line.push('.');
    line
}

/// The construction table's authored predicate order: fragments join the
/// sentence in THIS order (the G3-approved surface — moons, then star, then
/// day length), not ledger commit order. Deterministic without sorting:
/// the array is fixed, and each lookup takes the subject's first committed
/// fact per predicate.
const CONSTRUCTION_ORDER: &[&str] = &[MOON_COUNT, STAR_CLASS, DAY_LENGTH_STD];

/// The construction table: maps a (predicate, object) pair to the fragment
/// it contributes, or `None` if this predicate has no construction yet
/// (leaving it on [`uncovered_predicates`]'s list).
fn fragment_for(predicate: &str, object: &Value) -> Option<Fragment> {
    match (predicate, object) {
        (MOON_COUNT, Value::Number(n)) => {
            let count = *n as u64;
            Some(Fragment::Modifier(format!(
                "with {} moon{}",
                cardinal(count),
                if count == 1 { "" } else { "s" }
            )))
        }
        (STAR_CLASS, Value::Text(class)) => Some(Fragment::Modifier(format!(
            "orbiting {} {class}",
            indefinite_article(class)
        ))),
        (DAY_LENGTH_STD, Value::Number(days)) => Some(Fragment::Trailing(format!(
            "its day lasts {} standard days",
            quantity(*days)
        ))),
        _ => None,
    }
}

/// Resolve the surface subject for `entity`'s clause within one volume: its
/// resolved `name` on first mention, a fixed pronoun on re-mention. `seen`
/// accumulates entities already named — share one set across a volume's
/// render loop so a later sentence about the same subject reduces to "it".
fn subject_for(entity: EntityId, name: String, seen: &mut BTreeSet<EntityId>) -> Subject {
    if seen.insert(entity) {
        Subject::Name(name)
    } else {
        Subject::Pronoun("it")
    }
}

/// Render a volume: one Common sentence per `is-a` fact, subject resolved to
/// its `name` (or a synthetic `Entity <id>` label when genuinely unnamed),
/// aggregating that subject's other facts into the sentence via the
/// construction table (`fragment_for`), in the table's authored order
/// ([`CONSTRUCTION_ORDER`]) — the sentence's surface order is an authored
/// grammar decision, not an echo of ledger commit order. Then one more
/// sentence per `instance-of` fact (C2 T5): a placed peopled species'
/// collective, named by its autonym.
pub fn render_volume(world: &World) -> BookVolume {
    let mut lines = Vec::new();
    let mut named: BTreeSet<EntityId> = BTreeSet::new();
    for fact in world.ledger.find(hornvale_kernel::world::IS_A) {
        let Value::Text(kind) = &fact.object else {
            continue;
        };
        let subject_entity = fact.subject;
        let name = world
            .ledger
            .text_of(subject_entity, hornvale_kernel::NAME)
            .map(str::to_string)
            .unwrap_or_else(|| format!("Entity {}", subject_entity.0));

        let mut modifiers = Vec::new();
        let mut trailing = Vec::new();
        for predicate in CONSTRUCTION_ORDER {
            // First committed fact per (subject, predicate) — all three
            // construction predicates are functional, so "first" is "the"
            // value; still deterministic, no sorting.
            let Some(object) = world.ledger.value_of(subject_entity, predicate) else {
                continue;
            };
            match fragment_for(predicate, object) {
                Some(Fragment::Modifier(m)) => modifiers.push(m),
                Some(Fragment::Trailing(t)) => trailing.push(t),
                None => {}
            }
        }

        let subject = subject_for(subject_entity, name, &mut named);
        let line = realize_common(&ClauseSpec {
            frame: Frame::Classify,
            subject,
            complement: kind.clone(),
            number: Number::Sg,
            definiteness: Definiteness::Indef,
            modifiers,
        });
        let line = assemble_trailing(line, &trailing);
        lines.push(line);
    }
    // (kind, autonym, common_line) per placed people — the autonym (no
    // English "The " prefix) is the tongue subject the section below
    // reuses; `common_line` is the exact rendered English sentence its
    // tongue line's gloss echoes. Keyed for lookup against
    // `hornvale_worldgen::placed_peoples`' own registry order below (the
    // two orders coincide by construction — worldgen mints each collective
    // in `placed_peoples`' order — but a map lookup stays correct even if
    // that ever changed).
    let mut people_by_kind: BTreeMap<String, (String, String)> = BTreeMap::new();
    for fact in world.ledger.find(hornvale_kernel::INSTANCE_OF) {
        // C2 T5: one collective per placed peopled species — "The
        // ⟨Autonym⟩ are ⟨species⟩." The subject carries its own leading
        // "The " (there is no per-subject determiner slot in `ClauseSpec`;
        // `definiteness` here governs only the bare-plural complement, per
        // the grammar's existing `classify_generic_plural` shape), so this
        // is the one place that article is written, never doubled.
        let Value::Text(kind) = &fact.object else {
            continue;
        };
        let subject_entity = fact.subject;
        let name = world
            .ledger
            .text_of(subject_entity, hornvale_kernel::NAME)
            .map(str::to_string)
            .unwrap_or_else(|| format!("Entity {}", subject_entity.0));
        let subject = subject_for(subject_entity, format!("The {name}"), &mut named);
        let line = realize_common(&ClauseSpec {
            frame: Frame::Classify,
            subject,
            complement: species_label(kind),
            number: Number::Pl,
            definiteness: Definiteness::Indef,
            modifiers: Vec::new(),
        });
        people_by_kind.insert(kind.clone(), (name, line.clone()));
        lines.push(line);
    }

    // C3 T3, deepened by C7: each placed people states its own kind AND
    // (C7) an emic world-statement in its own tongue — both through the
    // deep realizer (`realize_tongue_deep`, C7 T1) with that people's own
    // drawn `TongueMorphology` (C7 T2's `tongue_morphology_of`) and derived
    // noun-class readout (`noun_class_of`) — then every tongue's attempt to
    // state each probe's kind is recorded — a rendered line on success, a
    // coverage gap on failure (spec §5 — no culture holds `planet` today).
    // Iterated over `placed_peoples` (registry order, deterministic) rather
    // than the ledger scan above so the section's order matches every other
    // peoples-keyed section in the almanac/book.
    let mut tongue_lines = Vec::new();
    let mut tongue_gaps = Vec::new();
    let probes = tongue_probes(world);
    // C7 T3: the world-statement's shared subject — the planet's own
    // committed name (`hornvale_kernel::NAME` on the entity classified
    // `is-a` `"planet"`), the SAME fact the C3 planet probe's own subject
    // already reads (never re-derived via `hornvale_worldgen::world_name`,
    // which would redundantly reconstruct the whole lexicon/component
    // pipeline just to recover a fact already sitting in the ledger).
    let planet_name = planet_name_of(world);
    for (kind, _village) in hornvale_worldgen::placed_peoples(world) {
        let Some((autonym, common_line)) = people_by_kind.get(kind) else {
            continue;
        };
        let ph = hornvale_worldgen::language_of(world, kind);
        let grammar = tongue_grammar(&world.seed, kind, &ph);
        let Ok(lexicon) = hornvale_worldgen::lexicon_of(world, kind) else {
            continue;
        };
        let Ok(morph) = hornvale_worldgen::tongue_morphology_of(world, kind) else {
            continue;
        };
        let noun_class_of = |concept: &str| hornvale_worldgen::noun_class_of(world, kind, concept);

        let own_kind = format!("{kind}-kind");
        let self_statement = TongueClause {
            subject: autonym.clone(),
            complement_concept: own_kind,
            // The self-statement is a folk (self-)statement, grounded in
            // lived experience (its autonym and own-kind concept are
            // Steeped by construction) — Witnessed (C7's readout law).
            evidential: Evidential::Witnessed,
        };
        let tongue_line =
            realize_tongue_deep(&self_statement, &grammar, &morph, &noun_class_of, &lexicon)
                .unwrap_or_else(|gap| {
                    panic!(
                        "the self-statement law (spec §5) is violated for {kind}: \
                 gap on {} ({})",
                        gap.concept, gap.reason
                    )
                });
        tongue_lines.push(format!(
            "{tongue_line} (in the {kind} tongue: \"{common_line}\")"
        ));

        // C7 T3: the emic world-statement — "⟨planet⟩ [cop] ⟨their word for
        // "earth"⟩", Witnessed — the world-statement law (spec §3.5/§4.5):
        // `earth` is universal-stratum Steeped (packs.rs, ladder_rank 0,
        // never a `LexEntry::Gap`), so this never gaps for a placed people —
        // panic loudly rather than record a coverage gap, since a gap here
        // would be a violated invariant, not the C3 planet-concept probe's
        // ordinary (etic-concept) coverage gap.
        let name = planet_name.as_deref().unwrap_or_else(|| {
            panic!(
                "the world-statement law is violated: {kind} is placed but the planet has no \
                 committed name — the dominant race's \"earth\" entry is universal-stratum \
                 Steeped and the planet stage must have named the world"
            )
        });
        let world_line = world_statement(
            kind,
            name,
            Evidential::Witnessed,
            &grammar,
            &morph,
            &noun_class_of,
            &lexicon,
        );
        tongue_lines.push(format!(
            "{world_line} (in the {kind} tongue: \"{name} is the earth.\")"
        ));

        for probe in &probes {
            match probe_tongue(probe, kind, &grammar, &morph, &noun_class_of, &lexicon) {
                Ok(line) => tongue_lines.push(format!(
                    "{line} (in the {kind} tongue: \"{} is a {}.\")",
                    probe.subject, probe.concept
                )),
                Err(gap) => {
                    tongue_gaps.push(format!("{kind}: gap — {} ({})", gap.concept, gap.reason))
                }
            }
        }
    }

    BookVolume {
        seed: world.seed.0,
        lines,
        tongue_lines,
        tongue_gaps,
        chorus: chorus_sections(world),
        reckoning: reckoning_epochs(world),
    }
}

/// The autonym (committed collective `NAME`) for each placed people that
/// has one, keyed by kind label — the small ledger scan
/// [`render_volume`]'s `people_by_kind` already performs, repeated here so
/// [`chorus_sections`] stays a self-contained `fn(&World) -> _` per its
/// documented signature.
fn autonym_by_kind(world: &World) -> BTreeMap<String, String> {
    let mut autonyms = BTreeMap::new();
    for fact in world.ledger.find(hornvale_kernel::INSTANCE_OF) {
        let Value::Text(kind) = &fact.object else {
            continue;
        };
        let subject_entity = fact.subject;
        let name = world
            .ledger
            .text_of(subject_entity, hornvale_kernel::NAME)
            .map(str::to_string)
            .unwrap_or_else(|| format!("Entity {}", subject_entity.0));
        autonyms.insert(kind.clone(), name);
    }
    autonyms
}

/// C4 T4: every placed people's chorus section, in
/// `hornvale_worldgen::accounts_of` order — a people with no committed
/// collective is skipped (mirrors C3's `continue` in the tongue-lines
/// loop above). C7 T3: each organized culture's doctrine section also gets
/// its in-tongue taught line here — [`planet_name_of`] and the tongue's own
/// grammar/morphology/lexicon are (re-)derived once per doctrine-bearing
/// culture, independently of `render_volume`'s own tongue-lines loop (the
/// same "each section derives its own inputs" idiom `autonym_by_kind`
/// already follows here, rather than threading state between the two
/// unrelated `BookVolume` fields).
fn chorus_sections(world: &World) -> Vec<ChorusSection> {
    let autonyms = autonym_by_kind(world);
    let planet_name = planet_name_of(world);
    hornvale_worldgen::accounts_of(world)
        .into_iter()
        .filter_map(|voice| {
            let autonym = autonyms.get(&voice.kind)?;
            let mut section = voice_section(&voice.kind, autonym, &voice.account, world);
            section.doctrine = hornvale_worldgen::doctrine_of(world, &voice.kind).map(|dv| {
                let kind = voice.kind.as_str();
                let ph = hornvale_worldgen::language_of(world, kind);
                let grammar = tongue_grammar(&world.seed, kind, &ph);
                let lexicon = hornvale_worldgen::lexicon_of(world, kind).unwrap_or_else(|e| {
                    panic!(
                        "the taught-contrast law is violated for {kind}: lexicon derivation \
                         failed: {e:?}"
                    )
                });
                let morph =
                    hornvale_worldgen::tongue_morphology_of(world, kind).unwrap_or_else(|e| {
                        panic!(
                            "the taught-contrast law is violated for {kind}: morphology \
                             derivation failed: {e:?}"
                        )
                    });
                let noun_class_of =
                    |concept: &str| hornvale_worldgen::noun_class_of(world, kind, concept);
                let name = planet_name.as_deref().unwrap_or_else(|| {
                    panic!(
                        "the world-statement law is violated: {kind}'s doctrine is organized \
                         but the planet has no committed name"
                    )
                });
                let taught_line = world_statement(
                    kind,
                    name,
                    Evidential::Taught,
                    &grammar,
                    &morph,
                    &noun_class_of,
                    &lexicon,
                );
                let tongue_taught_line =
                    format!("{taught_line} (\"{name} is the earth — as it is taught.\")");
                doctrine_section(
                    autonym,
                    &dv,
                    &voice.params,
                    &voice.account,
                    tongue_taught_line,
                )
            });
            Some(section)
        })
        .collect()
}

// ---------------------------------------------------------------------
// C8, The Diachronic Book: the Reckoning of Years — the Book's time axis.
// Pure derivation over `hornvale_worldgen::{observations_of, ladder_of}`
// (T1); zero new draws, facts, or save-format state. See `ReckoningEpoch`'s
// doc for the per-epoch shape and the plan's Preregistered block for every
// closed string below (frozen before measurement).
// ---------------------------------------------------------------------

/// The preregistered epoch pair (plan Global Constraints): day 0 (before
/// any culture could have witnessed a darkening) and the hundredth year —
/// `36_525.0` standard days. The committed artifact always renders exactly
/// these two, regardless of the CLI's `--at` lens ([`reckoning_at`]).
const RECKONING_EPOCH_1_DAY: f64 = 0.0;
const RECKONING_EPOCH_2_DAY: f64 = 36_525.0;
const RECKONING_EPOCH_1_HEADING: &str = "In the first days";
const RECKONING_EPOCH_2_HEADING: &str = "In the hundredth year";
const RECKONING_EPOCH_1_MARGIN_PHRASE: &str = "of the first days";
const RECKONING_EPOCH_2_MARGIN_PHRASE: &str = "of the first hundred years";

/// The empty arm (spec §3.4): rendered alone, with no margin, whenever the
/// true event count at `T` is zero.
const RECKONING_EMPTY_ARM: &str = "The sky keeps no dates to number.";

/// The folk register (spec §3.4): rendered for every placed culture at
/// [`hornvale_worldgen::LadderRung::Counted`] or above — even an organized
/// cult's own priesthood shares this lived experience before its Numbered
/// line adds the institutional cardinal.
const RECKONING_FOLK_COUNTED: &str = "The sky has darkened, now and again.";

/// Every placed culture's Reckoning-of-Years read, at the two
/// preregistered epochs — the committed `BookVolume::reckoning`. Always
/// exactly two entries; never gated on world content (an eventless world
/// renders the empty arm at both).
fn reckoning_epochs(world: &World) -> Vec<ReckoningEpoch> {
    let autonyms = autonym_by_kind(world);
    [
        (
            RECKONING_EPOCH_1_HEADING,
            RECKONING_EPOCH_1_DAY,
            RECKONING_EPOCH_1_MARGIN_PHRASE,
        ),
        (
            RECKONING_EPOCH_2_HEADING,
            RECKONING_EPOCH_2_DAY,
            RECKONING_EPOCH_2_MARGIN_PHRASE,
        ),
    ]
    .into_iter()
    .map(|(heading, day, margin_phrase)| {
        let at = hornvale_astronomy::StdDays::new(day).unwrap_or_else(|e| {
            panic!("the Reckoning's preregistered epoch day {day} must be a valid StdDays: {e}")
        });
        reckoning_epoch(world, &autonyms, heading, at, margin_phrase)
    })
    .collect()
}

/// The CLI `--at <day>` lens (Task 2): a single ad hoc Reckoning epoch at
/// an arbitrary day, rendered to stdout only — never part of the
/// committed `BookVolume::reckoning`, which always uses the two
/// preregistered epochs above regardless of this function ever being
/// called (`scripts/regenerate-artifacts.sh` never passes `--at`).
pub fn reckoning_at(world: &World, at: hornvale_astronomy::StdDays) -> ReckoningEpoch {
    let day = at.get();
    let heading = format!("At day {day}");
    let margin_phrase = format!("by day {day}");
    reckoning_epoch(world, &autonym_by_kind(world), &heading, at, &margin_phrase)
}

/// The true count of eclipse events to `at` (spec §3.4): every syzygy in
/// `[0, at]`, solar AND lunar, regardless of any culture's witnessing
/// capability — the world's own physical record, as opposed to
/// [`hornvale_worldgen::observations_of`]'s per-culture WITNESSED subset.
/// The margin law compares each culture's held count against this. A
/// tier-0 constant-sun world ([`hornvale_worldgen::Sky::Constant`]) has no
/// calendar and so no eclipses ever — honestly zero, never a panic, so
/// `render_volume` stays total over every world `hornvale_worldgen` can
/// build (not just the Book's own `SkyChoice::Generated` worlds); the
/// zero short-circuits [`reckoning_epoch`] straight to the empty arm
/// before it ever calls `observations_of`/`ladder_of` (both of which
/// themselves require a Generated sky).
fn true_event_count(world: &World, at: hornvale_astronomy::StdDays) -> usize {
    let sky = hornvale_worldgen::sky_of(world)
        .unwrap_or_else(|e| panic!("the Reckoning section requires a derivable sky: {e}"));
    match sky {
        hornvale_worldgen::Sky::Generated(sky) => {
            let from =
                hornvale_astronomy::StdDays::new(0.0).expect("0.0 is always a valid StdDays");
            hornvale_astronomy::eclipse_events(sky.system(), sky.calendar(), from, at).len()
        }
        hornvale_worldgen::Sky::Constant(_) => 0,
    }
}

/// One epoch's Reckoning-of-Years section (spec §3.4), for every placed
/// culture in [`hornvale_worldgen::placed_peoples`] order: the empty arm
/// when nothing has happened by `at` (the true count is zero); otherwise,
/// per culture at [`hornvale_worldgen::LadderRung::Counted`] or above, the
/// folk line, then — organized cultures only (`Numbered`/`Predictive`) —
/// the Numbered line naming that culture's own held cardinal, then —
/// `Predictive` only, and only when the taught day falls inside the
/// prediction horizon — the prediction line. The truth margin renders
/// exactly when some culture's held knowledge falls short of the true
/// count: an organized culture falls short when its own witnessed cardinal
/// is less than the true count (it cannot witness what its sky-capability
/// gates out); any culture below `Numbered` holds no cardinal at all —
/// qualitative memory always falls short of a true count that is, by
/// construction of this branch, at least one.
fn reckoning_epoch(
    world: &World,
    autonyms: &BTreeMap<String, String>,
    heading: &str,
    at: hornvale_astronomy::StdDays,
    margin_phrase: &str,
) -> ReckoningEpoch {
    let true_count = true_event_count(world, at);
    if true_count == 0 {
        return ReckoningEpoch {
            heading: heading.to_string(),
            lines: vec![RECKONING_EMPTY_ARM.to_string()],
            margin: Vec::new(),
        };
    }

    let mut lines = Vec::new();
    let mut falls_short = false;
    for (kind, _village) in hornvale_worldgen::placed_peoples(world) {
        let Some(autonym) = autonyms.get(kind) else {
            continue;
        };
        let observations =
            hornvale_worldgen::observations_of(world, kind, at).unwrap_or_else(|e| {
                panic!(
                    "the Reckoning section requires observations_of to succeed for placed culture \
                 {kind}: {e}"
                )
            });
        let (rung, prediction) =
            hornvale_worldgen::ladder_of(world, kind, at).unwrap_or_else(|e| {
                panic!(
                    "the Reckoning section requires ladder_of to succeed for placed culture \
                     {kind}: {e}"
                )
            });

        if rung == hornvale_worldgen::LadderRung::Unknown {
            // Nothing witnessed: no line, and this culture's (zero) held
            // count trivially falls short of a true count that is >= 1
            // here.
            falls_short = true;
            continue;
        }

        let held = observations.events.len() as u64;
        lines.extend(reckoning_culture_lines(autonym, rung, held, prediction));

        let organized = matches!(
            rung,
            hornvale_worldgen::LadderRung::Numbered | hornvale_worldgen::LadderRung::Predictive
        );
        if !organized || held < true_count as u64 {
            // Folk-only/sub-threshold cultures hold no cardinal at all
            // (qualitative memory always falls short); an organized
            // culture falls short exactly when its own witnessed cardinal
            // is less than the true count (it cannot witness what its
            // sky-capability gates out).
            falls_short = true;
        }
    }

    let margin = if falls_short {
        vec![format!(
            "In truth, the darkenings {margin_phrase} number {}.",
            cardinal(true_count as u64)
        )]
    } else {
        Vec::new()
    };

    ReckoningEpoch {
        heading: heading.to_string(),
        lines,
        margin,
    }
}

/// One placed culture's Reckoning-of-Years lines at a given rung (spec
/// §3.4): nothing at [`hornvale_worldgen::LadderRung::Unknown`]; the folk
/// line alone at `Counted`; the folk line plus the Numbered line (with the
/// witnessed cardinal `held`) at `Numbered`; both of those plus — only
/// when `prediction` falls inside the teaching horizon — the prediction
/// line at `Predictive`. Pure and world-free (unlike [`reckoning_epoch`],
/// which also needs the true count to judge the margin), so the honest
/// omit-the-prediction arm (`Predictive` with `prediction: None` — T1's
/// report: unreached at seeds 1..=5, since every measured Predictive
/// culture's next event falls inside the horizon) can be driven
/// synthetically rather than only through a live world that may never
/// produce it.
fn reckoning_culture_lines(
    autonym: &str,
    rung: hornvale_worldgen::LadderRung,
    held: u64,
    prediction: Option<f64>,
) -> Vec<String> {
    if rung == hornvale_worldgen::LadderRung::Unknown {
        return Vec::new();
    }
    let mut lines = vec![RECKONING_FOLK_COUNTED.to_string()];
    if matches!(
        rung,
        hornvale_worldgen::LadderRung::Numbered | hornvale_worldgen::LadderRung::Predictive
    ) {
        lines.push(format!(
            "The priesthood of the {autonym} numbers the darkenings: {}.",
            cardinal(held)
        ));
        if rung == hornvale_worldgen::LadderRung::Predictive
            && let Some(next_day) = prediction
        {
            lines.push(format!(
                "The next darkening, it teaches, comes on day {}.",
                next_day.trunc() as u64
            ));
        }
        // `Predictive` + `None`: the taught day is beyond the priesthood's
        // teaching horizon — an honest arm; the line is simply omitted
        // rather than stating a falsehood.
    }
    lines
}

/// Read through a C5 `Explained` wrapper to what the four-filter account
/// said underneath. This module's C4 renderer doesn't know the
/// `Explained` variant yet — giving it a surface is Task 4's job — so
/// every disposition read in this file goes through this seam, keeping
/// every rendered line exactly as it was before C5 started wrapping
/// entries. Mirrors `hornvale_language::account`'s own private
/// `effective()` (recursive for the same future-proofing reason).
fn effective(d: &Disposition) -> &Disposition {
    match d {
        Disposition::Explained { underlying, .. } => effective(underlying),
        other => other,
    }
}

/// `"ourselves"`/`"neighbors"`/`"rivals"`/`"strangers"` — the stance
/// appositive's closed text table (spec §3.3); `Neutral` never reaches
/// this function (callers guard on it, since it appends nothing).
fn stance_text(stance: Stance) -> &'static str {
    match stance {
        Stance::Ourselves => "ourselves",
        Stance::Neighbors => "neighbors",
        Stance::Rivals => "rivals",
        Stance::Strangers => "strangers",
        Stance::Neutral => "",
    }
}

/// [`subject_for`]'s text-keyed analog: an [`Account`]'s entries carry only
/// resolved name text (no `EntityId` — see `GroundFact`'s doc), so a
/// chorus section's referring-expression scope tracks `seen` by that text
/// instead. `key` is the raw ground-truth name (e.g. `"Vavako"`, never
/// "The Vavako"), so a people subject's `"The "` prefix never leaks into
/// the re-mention check; `display` is the surface text used on first
/// mention.
fn subject_for_text(key: &str, display: String, seen: &mut BTreeSet<String>) -> Subject {
    if seen.insert(key.to_string()) {
        Subject::Name(display)
    } else {
        Subject::Pronoun("it")
    }
}

/// The world subject's emic clause, folding in this culture's `Kept`
/// fragment entries only: `Substituted` classification renders `theirs`
/// definite ("Vebe is the earth"); `Kept` (the identity case) renders the
/// ground truth kind indefinite, byte-matching the god's-eye line.
/// `Lost` classification never occurs at the floor (every placed culture
/// holds the universal `earth` carving — `world_carving` is always
/// `Some`), so it renders nothing; a future culture without that holding
/// would need this arm revisited.
fn render_world_clause(
    group: &[&AccountEntry],
    is_a_entry: &AccountEntry,
    seen: &mut BTreeSet<String>,
) -> Option<String> {
    let (complement, definiteness) = match effective(&is_a_entry.disposition) {
        Disposition::Kept => {
            let Value::Text(kind) = &is_a_entry.fact.object else {
                return None;
            };
            (kind.clone(), Definiteness::Indef)
        }
        Disposition::Substituted { theirs, .. } => (theirs.clone(), Definiteness::Def),
        Disposition::Lost(_) => return None,
        Disposition::Explained { .. } => unreachable!("effective() never returns Explained"),
    };

    let mut modifiers = Vec::new();
    let mut trailing = Vec::new();
    for entry in group {
        if entry.fact.predicate == hornvale_kernel::world::IS_A {
            continue;
        }
        if !matches!(effective(&entry.disposition), Disposition::Kept) {
            continue;
        }
        match fragment_for(&entry.fact.predicate, &entry.fact.object) {
            Some(Fragment::Modifier(m)) => modifiers.push(m),
            Some(Fragment::Trailing(t)) => trailing.push(t),
            None => {}
        }
    }

    let name = is_a_entry.fact.subject.clone();
    let subject = subject_for_text(&name, name.clone(), seen);
    let line = realize_common(&ClauseSpec {
        frame: Frame::Classify,
        subject,
        complement,
        number: Number::Sg,
        definiteness,
        modifiers,
    });
    Some(assemble_trailing(line, &trailing))
}

/// The world subject's etic margin (spec §4.3, the margin law): fires only
/// when this subject owns at least one `Lost`/`Substituted` entry — a
/// `Substituted` classification, or a `Lost` fragment (a fragment
/// predicate is never `Substituted`: `moon-count`/`star-class`/
/// `day-length-std` are all non-`Taxonomic` requirements). The truth-kind
/// complement reads straight off the ground fact's own object text (always
/// the ground truth, independent of disposition), so this stays correct
/// even in the never-exercised `Lost` classification case. Carries ONLY
/// the lost fragments (sparseness — a `Kept` fragment is never repeated
/// here, since the emic paragraph already states it).
///
/// **Carrier-clause assumption**: at the floor, `instance-of` is always
/// `Kept` (its `Manifest` requirement never fails once a culture holds any
/// other kind's `"{kind}-kind"` concept, which every placed culture does),
/// so no people subject ever needs a margin — every margin sentence's
/// carrier clause is this, the world subject's own classification. A
/// future culture that could lose an `instance-of` fact would need a
/// people-margin arm added here.
fn render_world_margin(group: &[&AccountEntry], is_a_entry: &AccountEntry) -> Option<String> {
    let world_lost = matches!(
        effective(&is_a_entry.disposition),
        Disposition::Substituted { .. } | Disposition::Lost(_)
    );
    let lost_fragments: Vec<&&AccountEntry> = group
        .iter()
        .filter(|entry| {
            entry.fact.predicate != hornvale_kernel::world::IS_A
                && matches!(effective(&entry.disposition), Disposition::Lost(_))
        })
        .collect();
    if !world_lost && lost_fragments.is_empty() {
        return None;
    }

    let Value::Text(truth_kind) = &is_a_entry.fact.object else {
        return None;
    };
    let mut modifiers = Vec::new();
    let mut trailing = Vec::new();
    for entry in lost_fragments {
        match fragment_for(&entry.fact.predicate, &entry.fact.object) {
            Some(Fragment::Modifier(m)) => modifiers.push(m),
            Some(Fragment::Trailing(t)) => trailing.push(t),
            None => {}
        }
    }
    let line = realize_common(&ClauseSpec {
        frame: Frame::Classify,
        subject: Subject::Name(is_a_entry.fact.subject.clone()),
        complement: truth_kind.clone(),
        number: Number::Sg,
        definiteness: Definiteness::Indef,
        modifiers,
    });
    Some(format!("In truth, {}", assemble_trailing(line, &trailing)))
}

/// Task 4 (C5): the count-aware head clause an explanation line opens
/// with — `"The day returns"` for the day; `"The moon crosses"` /
/// `"The moons cross"` for the moons, singular/plural read off the
/// `moon-count` fact's own ground value (always numeric; the moons entry is
/// only ever wrapped in [`Disposition::Explained`] while `Kept`, so this is
/// the true committed count, never a substitution). `None` for any other
/// predicate — `explain` in `windows/worldgen::chorus` only ever wraps
/// `day-length-std` and `moon-count`.
fn explanation_head(predicate: &str, object: &Value) -> Option<(String, bool)> {
    if predicate == DAY_LENGTH_STD {
        return Some(("The day returns".to_string(), false));
    }
    if predicate == MOON_COUNT {
        let Value::Number(n) = object else {
            return None;
        };
        let plural = (*n as u64) != 1;
        let head = if plural {
            "The moons cross"
        } else {
            "The moon crosses"
        };
        return Some((head.to_string(), plural));
    }
    None
}

/// Task 4's closed 6-frame surface table (the spec plan's frame table,
/// EXACT strings) — the inverse this module's `parse_explanation_body`
/// mirrors. `plural` selects the Kinship/LinkSympathy pronoun (`"they
/// are"`/`"it is"`, `"they answer"`/`"it answers"`); `agent`/`lexeme` are
/// consulted only by the three frames that carry them
/// (`Agentive`/`Kinship`/`LinkSympathy`). Returns `None` when one of those
/// frames needs a bound agent that isn't there — the "no synthetic agents,
/// ever" guard (plan Global Constraints, ledger #2) extended to rendering:
/// an unbound schema explains nothing rather than fabricating a name. Not
/// pinned by a literal example across seeds 1..=3 (only
/// `Agentive`/`CycleReturn`/`PathJourney`/`Balance` are measured to fire
/// there — `Kinship`/`LinkSympathy` simply never win the weighted schema
/// draw at the floor), even though their agent binding is fully wired
/// (`windows/worldgen::chorus::bind_agent` covers every deity-bearing
/// schema, C5 T4's review fix) — a closed table stays exhaustive regardless
/// of what today's weights happen to draw.
fn explanation_line(
    head: &str,
    plural: bool,
    schema: SchemaId,
    agent: Option<&str>,
    lexeme: Option<LexemeId>,
    manner: Manner,
) -> Option<String> {
    match schema {
        SchemaId::Agentive => {
            let deity = agent?;
            let verb = lexeme?.0;
            let manner_text = match manner {
                Manner::Brisk => ", briskly",
                Manner::Slow => ", slowly",
                Manner::Neutral => "",
            };
            Some(format!(
                "{head} because {deity} {verb} the sky{manner_text}."
            ))
        }
        SchemaId::CycleReturn => Some(format!("{head}, as all things return.")),
        SchemaId::PathJourney => Some(format!("{head} because the sky must be crossed.")),
        SchemaId::Balance => Some(format!("{head} to keep the balance.")),
        SchemaId::Kinship => {
            let deity = agent?;
            let pronoun = if plural { "they are" } else { "it is" };
            Some(format!("{head} because {pronoun} {deity}'s kin."))
        }
        SchemaId::LinkSympathy => {
            let deity = agent?;
            let pronoun = if plural { "they answer" } else { "it answers" };
            Some(format!("{head} because {pronoun} {deity}."))
        }
        // The other six schemas (ForceDynamics, SubstanceFlow, Container,
        // MoralAccounting, EssenceTelos, Verticality) admit only
        // `HighScalarState` (schemas.rs), a shape neither `explain_day` nor
        // `explain_moons` ever produces — unreachable at the floor, kept as
        // a safe exhaustive fallback rather than a panic.
        SchemaId::ForceDynamics
        | SchemaId::SubstanceFlow
        | SchemaId::Container
        | SchemaId::MoralAccounting
        | SchemaId::EssenceTelos
        | SchemaId::Verticality => None,
    }
}

/// Task 4: this subject's because-clause explanation lines — one per
/// [`Disposition::Explained`] entry in `group` (the day and/or moons),
/// in `group`'s own ground order (mirrors [`CONSTRUCTION_ORDER`]: moons
/// before day-length, since `chorus_ground`'s construction order matches
/// it). Reads `entry.disposition` directly, not through [`effective`] — the
/// explanation fields live only on the un-unwrapped `Explained` variant.
fn render_explanations(group: &[&AccountEntry]) -> Vec<String> {
    let mut lines = Vec::new();
    for entry in group {
        let Disposition::Explained {
            schema,
            agent,
            lexeme,
            manner,
            ..
        } = &entry.disposition
        else {
            continue;
        };
        let Some((head, plural)) = explanation_head(&entry.fact.predicate, &entry.fact.object)
        else {
            continue;
        };
        if let Some(line) =
            explanation_line(&head, plural, *schema, agent.as_deref(), *lexeme, *manner)
        {
            lines.push(line);
        }
    }
    lines
}

/// A people subject's emic clause: the god's-eye collective construction
/// (`species_label`, plural, indefinite), plus the stance appositive at
/// the book layer (`" — {stance}."`, replacing the terminal `.`) — absent
/// for `Neutral` (the identity case, byte-matching the god's-eye line).
/// Returns `None` if this subject's `instance-of` entry is not `Kept` (see
/// [`render_world_margin`]'s carrier-clause note: never exercised at the
/// floor).
fn render_people_clause(io_entry: &AccountEntry, seen: &mut BTreeSet<String>) -> Option<String> {
    if !matches!(effective(&io_entry.disposition), Disposition::Kept) {
        return None;
    }
    let Value::Text(kind_text) = &io_entry.fact.object else {
        return None;
    };
    let raw_name = io_entry.fact.subject.clone();
    let display = format!("The {raw_name}");
    let subject = subject_for_text(&raw_name, display, seen);
    let mut line = realize_common(&ClauseSpec {
        frame: Frame::Classify,
        subject,
        complement: species_label(kind_text),
        number: Number::Pl,
        definiteness: Definiteness::Indef,
        modifiers: Vec::new(),
    });
    if !matches!(io_entry.stance, Stance::Neutral) {
        line.pop();
        line.push_str(&format!(" — {}.", stance_text(io_entry.stance)));
    }
    Some(line)
}

/// One placed people's rendered chorus section: group `account.entries` by
/// ground-fact subject (preserving each subject's first-encountered
/// position — stable under `OrderPolicy::Salience`'s partition, since every
/// world-subject fact shares the `"sky"` domain and every people-subject
/// fact shares `"peoples"`, so each stays a contiguous block), then render
/// the world subject via [`render_world_clause`]/[`render_world_margin`]
/// and each people subject via [`render_people_clause`]. `seen` is a fresh
/// per-section scope (every account names its subjects itself — the
/// module doc's fresh-scope rule); the margin register always names its
/// (single, world) subject fresh, independent of the emic paragraph's
/// scope — it is a separate typographic register, not a continuation.
fn voice_section(kind: &str, autonym: &str, account: &Account, _world: &World) -> ChorusSection {
    let mut order: Vec<String> = Vec::new();
    let mut groups: BTreeMap<String, Vec<&AccountEntry>> = BTreeMap::new();
    for entry in &account.entries {
        let subject = entry.fact.subject.clone();
        if !groups.contains_key(&subject) {
            order.push(subject.clone());
        }
        groups.entry(subject).or_default().push(entry);
    }

    let mut emic = Vec::new();
    let mut margin = Vec::new();
    let mut seen: BTreeSet<String> = BTreeSet::new();
    for subject in &order {
        let group = &groups[subject];
        if let Some(is_a_entry) = group
            .iter()
            .find(|e| e.fact.predicate == hornvale_kernel::world::IS_A)
        {
            if let Some(line) = render_world_clause(group, is_a_entry, &mut seen) {
                emic.push(line);
            }
            // Task 4 (C5): the because-clause explanations for this
            // subject's day/moons entries, as additional emic lines —
            // appended right after the world clause, before the margin.
            emic.extend(render_explanations(group));
            if let Some(line) = render_world_margin(group, is_a_entry) {
                margin.push(line);
            }
        } else if let Some(io_entry) = group
            .iter()
            .find(|e| e.fact.predicate == hornvale_kernel::INSTANCE_OF)
            && let Some(line) = render_people_clause(io_entry, &mut seen)
        {
            emic.push(line);
        }
    }

    ChorusSection {
        kind: kind.to_string(),
        heading: format!("As the {autonym} tell it"),
        emic,
        margin,
        doctrine: None,
    }
}

/// C6 (The Doctrine), the exoteric formula (the plan's Surfaces table):
/// what a `RevealedClaim` entry's doctrine emic line asserts INSTEAD of the
/// ordinary construction fragment — the priesthood professes counted
/// knowledge of the moons without disclosing the count itself (the
/// esoteric/exoteric split, ledger #5 — the actual value is a followup
/// scope, not this artifact's). Only [`MOON_COUNT`] carries a defined
/// formula: at the floor, `sky_capability` is the only
/// [`AccountParams`] field the four deltas ever touch, and `moon-count` is
/// the only [`hornvale_language::account::Requirement::SkyGraded`]
/// predicate in the observability table (`day-length-std` is
/// `CrossReferential`, always lost regardless of capability, so it can
/// never become a doctrine-only `Kept`) — `None` for any other predicate.
fn revealed_claim_line(predicate: &str, object: &Value) -> Option<String> {
    if predicate != MOON_COUNT {
        return None;
    }
    let Value::Number(n) = object else {
        return None;
    };
    Some(if (*n as u64) == 1 {
        "The moon is counted and known to the priesthood.".to_string()
    } else {
        "The moons are counted and known to the priesthood.".to_string()
    })
}

/// [`revealed_claim_line`]'s closed inverse table — the only two strings
/// that construction direction ever emits, paired with the plurality each
/// carries.
const REVEALED_CLAIM_LINES: &[(&str, bool)] = &[
    ("The moon is counted and known to the priesthood.", false),
    ("The moons are counted and known to the priesthood.", true),
];

/// Recover a `RevealedClaim` line's plurality from its exact closed text,
/// or `None` if `line` matches neither row.
fn parse_revealed_claim(line: &str) -> Option<bool> {
    REVEALED_CLAIM_LINES
        .iter()
        .find(|(text, _)| *text == line)
        .map(|(_, plural)| *plural)
}

/// [`parse_revealed_claim`]'s inverse: the closed text for `plural`. Total
/// over `bool` (the table carries exactly one row per plurality).
fn rerender_revealed_claim(plural: bool) -> String {
    REVEALED_CLAIM_LINES
        .iter()
        .find(|(_, p)| *p == plural)
        .map(|(text, _)| (*text).to_string())
        .expect("REVEALED_CLAIM_LINES carries a row for both plurality values")
}

/// The disclosure law's counter-annotation prefix (the plan's Surfaces
/// table): `"— though the folk say ⟨folk sentence minus its terminal
/// period⟩."` — [`counter_annotation_line`] builds it,
/// [`parse_chorus_line`] strips it back off.
const COUNTER_PREFIX: &str = "— though the folk say ";

/// Build one `Contested` counter-annotation line: `folk_line` is the folk
/// voice's own rendered sentence for the same fact (always a full,
/// terminally-punctuated line — an [`explanation_line`] result in
/// practice, since a moon-count `Contested` state only ever arises when
/// BOTH accounts explain the fact under different schemas — see
/// [`doctrine_section`]'s doc). Strips `folk_line`'s own terminal period so
/// the counter-annotation's own restores exactly one, never two.
fn counter_annotation_line(folk_line: &str) -> String {
    let stripped = folk_line.strip_suffix('.').unwrap_or(folk_line);
    format!("{COUNTER_PREFIX}{stripped}.")
}

/// C6 (The Doctrine): one organized culture's doctrine section — the SAME
/// `voice_section` machinery (a fresh referring scope, [`render_world_clause`]
/// / [`render_explanations`] / [`render_world_margin`] / [`render_people_clause`]
/// all reused unmodified) run over the doctrine account instead of the folk
/// one, with two overrides driven by [`conflict_of`] on each non-`is-a`
/// entry (matched against the FOLK account's own entry for the same
/// subject/predicate, via `hornvale_worldgen::folk_verifiable` for the
/// caller-derived verifiability flag the classifier needs):
///
/// - [`ConflictState::RevealedClaim`]: this entry's ordinary construction
///   fragment is EXCLUDED from the world clause and replaced by the closed
///   exoteric formula ([`revealed_claim_line`]) as its own emic line — the
///   priesthood professes counted knowledge without disclosing the value.
///   Its because-clause explanation (if any) still renders normally: that
///   is a causal story, not the value itself, so it is not suppressed.
/// - [`ConflictState::Contested`]: rendered exactly as `voice_section`
///   would (nothing suppressed), plus a disclosure-law counter-annotation
///   ([`counter_annotation_line`]) quoting the FOLK voice's own rendered
///   explanation for that same fact — the annotations live in their own
///   register (`annotations`), never mixed into `emic` (the disclosure
///   rule is mandatory, never silent, but also never confused with the
///   doctrine's own voice).
/// - [`ConflictState::Mystery`] and [`ConflictState::Harmony`]: rendered
///   exactly as `voice_section` would, no addition at all.
///
/// The margin reuses [`render_world_margin`] UNMODIFIED — the doctrine
/// account's own etic register, the same law as any other voice.
fn doctrine_section(
    autonym: &str,
    doctrine: &hornvale_worldgen::DoctrineVoice,
    folk_params: &AccountParams,
    folk_account: &Account,
    tongue_taught_line: String,
) -> DoctrineSection {
    let mut folk_by_key: BTreeMap<(String, String), &AccountEntry> = BTreeMap::new();
    for entry in &folk_account.entries {
        folk_by_key.insert(
            (entry.fact.subject.clone(), entry.fact.predicate.clone()),
            entry,
        );
    }

    let mut order: Vec<String> = Vec::new();
    let mut groups: BTreeMap<String, Vec<&AccountEntry>> = BTreeMap::new();
    for entry in &doctrine.account.entries {
        let subject = entry.fact.subject.clone();
        if !groups.contains_key(&subject) {
            order.push(subject.clone());
        }
        groups.entry(subject).or_default().push(entry);
    }

    let mut emic = Vec::new();
    let mut annotations = Vec::new();
    let mut margin = Vec::new();
    let mut seen: BTreeSet<String> = BTreeSet::new();
    for subject in &order {
        let group = &groups[subject];
        if let Some(is_a_entry) = group
            .iter()
            .find(|e| e.fact.predicate == hornvale_kernel::world::IS_A)
        {
            let mut revealed: BTreeSet<String> = BTreeSet::new();
            for entry in group.iter() {
                if entry.fact.predicate == hornvale_kernel::world::IS_A {
                    continue;
                }
                let Some(&folk_entry) =
                    folk_by_key.get(&(subject.clone(), entry.fact.predicate.clone()))
                else {
                    continue;
                };
                let verifiable =
                    hornvale_worldgen::folk_verifiable(folk_params, &entry.fact.predicate);
                match conflict_of(&folk_entry.disposition, &entry.disposition, verifiable) {
                    ConflictState::RevealedClaim => {
                        revealed.insert(entry.fact.predicate.clone());
                    }
                    ConflictState::Contested => {
                        let lines = render_explanations(std::slice::from_ref(&folk_entry));
                        // The disclosure law is unconditional: a Contested
                        // entry that yields NO folk counter-line would let
                        // the mandatory annotation vanish silently (the
                        // same class as the non-moon RevealedClaim guard
                        // below; final-review F1). Ledger #9's parity
                        // widening makes this reachable in principle (a
                        // bare-Kept folk side has no explanation line to
                        // quote) — fail loudly so the counter-surface for
                        // that shape is authored deliberately.
                        assert!(
                            !lines.is_empty(),
                            "disclosure law: Contested entry on predicate {:?}                              produced no folk counter-annotation — author a                              counter-surface for this entry shape",
                            folk_entry.fact.predicate
                        );
                        for line in lines {
                            annotations.push(counter_annotation_line(&line));
                        }
                    }
                    ConflictState::Harmony | ConflictState::Mystery => {}
                }
            }

            let filtered: Vec<&AccountEntry> = group
                .iter()
                .filter(|e| !revealed.contains(&e.fact.predicate))
                .copied()
                .collect();
            if let Some(line) = render_world_clause(&filtered, is_a_entry, &mut seen) {
                emic.push(line);
            }
            for entry in group.iter() {
                if revealed.contains(&entry.fact.predicate) {
                    // T3 review, mandated carry-over #1 (the
                    // vanishing-realizable class): a `RevealedClaim` entry
                    // must never silently disappear from the doctrine emic
                    // just because `revealed_claim_line` carries no formula
                    // arm for its predicate — it would otherwise vanish from
                    // BOTH the emic paragraph (excluded via `filtered`
                    // above) AND the margin (this loop is the only place
                    // left that could still surface it), with no trace at
                    // all. Fail loud instead: a future predicate reaching
                    // `RevealedClaim` demands its own formula be authored
                    // here first.
                    let line = revealed_claim_line(&entry.fact.predicate, &entry.fact.object)
                        .unwrap_or_else(|| {
                            panic!(
                                "a RevealedClaim entry for predicate {:?} has no exoteric \
                                 formula authored in `revealed_claim_line` — the \
                                 vanishing-realizable class: author a formula arm for this \
                                 predicate before a doctrine section can gate it in",
                                entry.fact.predicate
                            )
                        });
                    emic.push(line);
                }
            }
            emic.extend(render_explanations(group));
            if let Some(line) = render_world_margin(group, is_a_entry) {
                margin.push(line);
            }
        } else if let Some(io_entry) = group
            .iter()
            .find(|e| e.fact.predicate == hornvale_kernel::INSTANCE_OF)
            && let Some(line) = render_people_clause(io_entry, &mut seen)
        {
            emic.push(line);
        }
    }

    DoctrineSection {
        heading: format!("As the priesthood of the {autonym} teach it"),
        tongue_taught_line,
        emic,
        annotations,
        margin,
    }
}

/// Resolve a ground-fact subject's rendered name back to the entity that
/// carries it — the same resolution `hornvale_worldgen::chorus_ground`'s
/// private `subject_name` performs, run in reverse (that helper is not
/// exported; `windows/book` cannot import it, layering runs the other way,
/// same posture as `chorus_ground`'s own doc comment). Only [`IS_A`]
/// subjects are searched — every predicate [`esoteric_lines`] can ever see
/// a `RevealedClaim` for (moon-count, star-class, day-length-std today)
/// is asserted on an `is-a`-classified subject, the same scope
/// `render_volume`'s construction table reads.
///
/// [`IS_A`]: hornvale_kernel::world::IS_A
fn entity_named(world: &World, name: &str) -> Option<EntityId> {
    world
        .ledger
        .find(hornvale_kernel::world::IS_A)
        .find_map(|fact| {
            let resolved = world
                .ledger
                .text_of(fact.subject, hornvale_kernel::NAME)
                .map(str::to_string)
                .unwrap_or_else(|| format!("Entity {}", fact.subject.0));
            (resolved == name).then_some(fact.subject)
        })
}

/// C6 T4 (the esoteric edition): the initiated lines a reader with access
/// to `reader` — the `(subject, predicate)` keys of every fact they may be
/// shown the doctrine's disclosed value for — is entitled to. For every
/// organized culture's doctrine [`ConflictState::RevealedClaim`] entry
/// whose key is in `reader`, emits `"— ⟨cardinal⟩, as the initiated
/// count."`, with the cardinal read from the LEDGER's own committed value
/// for that subject/predicate — never the account entry's cached copy —
/// so the line's number can only ever trace back to the one committed
/// truth (the mutation-verified law: `the_esoteric_law_mutation_verified`
/// drives this directly). An empty `reader` yields an empty `Vec` — the
/// committed exoteric edition discloses nothing.
///
/// More than one organized culture can independently reveal the SAME
/// ground fact (the ground truth is world-global, `chorus_ground`, run
/// through each culture's own doctrine params) — deduplicated by key, so
/// the reader sees each revealed fact's initiated line exactly once
/// regardless of how many priesthoods reveal it.
///
/// A key whose entity cannot be resolved back to an `is-a` subject, or
/// whose ledger value is not [`Value::Number`], is silently skipped: no
/// [`RevealedClaim`] predicate reaching this function today is anything
/// but a moon-count-shaped cardinal (see [`revealed_claim_line`]'s doc),
/// and this function's own reach is the reader's disclosure surface, not
/// the doctrine emic's — the vanishing-realizable panic
/// ([`doctrine_section`]) already guards the emic's own formula table.
///
/// [`RevealedClaim`]: ConflictState::RevealedClaim
/// type-audit: bare-ok(identifier-text: reader), bare-ok(prose: return)
pub fn esoteric_lines(world: &World, reader: &BTreeSet<(String, String)>) -> Vec<String> {
    let mut lines = Vec::new();
    let mut seen: BTreeSet<(String, String)> = BTreeSet::new();
    for voice in hornvale_worldgen::accounts_of(world) {
        let Some(doctrine) = hornvale_worldgen::doctrine_of(world, &voice.kind) else {
            continue;
        };
        for entry in &doctrine.account.entries {
            let predicate = &entry.fact.predicate;
            if predicate == hornvale_kernel::world::IS_A
                || predicate == hornvale_kernel::INSTANCE_OF
            {
                continue;
            }
            let key = (entry.fact.subject.clone(), predicate.clone());
            if !reader.contains(&key) || seen.contains(&key) {
                continue;
            }
            let Some(folk_entry) =
                voice.account.entries.iter().find(|e| {
                    e.fact.subject == entry.fact.subject && &e.fact.predicate == predicate
                })
            else {
                continue;
            };
            let verifiable = hornvale_worldgen::folk_verifiable(&voice.params, predicate);
            if conflict_of(&folk_entry.disposition, &entry.disposition, verifiable)
                != ConflictState::RevealedClaim
            {
                continue;
            }
            let Some(entity) = entity_named(world, &entry.fact.subject) else {
                continue;
            };
            let Some(Value::Number(n)) = world.ledger.value_of(entity, predicate) else {
                continue;
            };
            lines.push(format!(
                "— {}, as the initiated count.",
                cardinal(*n as u64)
            ));
            seen.insert(key);
        }
    }
    lines
}

/// One entry in the tongue render inventory: a concept some committed fact
/// asks every tongue to state, about a named subject. The inventory is
/// DERIVED from the ledger (C4 T1) — one probe per committed `is-a`
/// complement — so a future renderable kind auto-enters the coverage
/// report instead of waiting on a hand-list.
/// type-audit: bare-ok(identifier-text: concept), bare-ok(prose: subject)
pub struct TongueProbe {
    /// The concept the tongue is asked to state (an `is-a` complement).
    pub concept: String,
    /// The subject's surface name (the committed `name`, or the C3
    /// fallback text).
    pub subject: String,
}

/// The derived probe inventory: one probe per committed `is-a` fact,
/// ledger order.
pub fn tongue_probes(world: &World) -> Vec<TongueProbe> {
    let mut probes = Vec::new();
    for fact in world.ledger.find(hornvale_kernel::world::IS_A) {
        let Value::Text(kind) = &fact.object else {
            continue;
        };
        let subject = world
            .ledger
            .text_of(fact.subject, hornvale_kernel::NAME)
            .map(str::to_string)
            .unwrap_or_else(|| format!("Entity {}", fact.subject.0));
        probes.push(TongueProbe {
            concept: kind.clone(),
            subject,
        });
    }
    probes
}

/// Run one probe against one tongue: realize `⟨subject⟩ ⟨copula?⟩
/// ⟨concept⟩` through the deep realizer (C7) — `Ok` is a rendered line
/// (the success path C3 dropped), `Err` the recountable gap (today, always
/// the `planet` probe: no culture holds that etic concept).
fn probe_tongue(
    probe: &TongueProbe,
    _kind: &str,
    grammar: &hornvale_language::TongueGrammar,
    morph: &TongueMorphology,
    noun_class_of: &dyn Fn(&str) -> NounClass,
    lexicon: &hornvale_language::Lexicon,
) -> Result<String, hornvale_language::TongueGap> {
    realize_tongue_deep(
        &TongueClause {
            subject: probe.subject.clone(),
            complement_concept: probe.concept.clone(),
            // Every probe states a claim grounded in the same
            // lived-experience footing as the self-statement above.
            evidential: Evidential::Witnessed,
        },
        grammar,
        morph,
        noun_class_of,
        lexicon,
    )
}

/// C7 T3: the planet's own committed name (the entity classified `is-a`
/// `"planet"`; its `NAME`, committed once at genesis from the dominant
/// people's "earth" endonym — `hornvale_worldgen`'s planet stage) — the
/// SAME subject the C3 planet probe already carries as `TongueProbe::subject`
/// (this reads the identical ledger fact, not a re-derivation). `None` only
/// when the world has not reached the planet stage (no placed people at
/// all) — every call site here guards on at least one placed people first,
/// so a `None` at that point is the world-statement law's own violation,
/// not an ordinary coverage gap.
fn planet_name_of(world: &World) -> Option<String> {
    hornvale_worldgen::planet_entity(world)
        .and_then(|planet| world.ledger.text_of(planet, hornvale_kernel::NAME))
        .map(str::to_string)
}

/// C7 T3: build one tongue's emic world-statement — `TongueClause { subject:
/// planet_name, complement_concept: "earth", evidential }` — through the
/// deep realizer, using that tongue's own already-derived grammar/morphology/
/// lexicon (never re-derived here; callers pass what they already hold, the
/// same "measure once" discipline `render_volume`'s loop and
/// `chorus_sections` both follow). `earth` is universal-stratum Steeped
/// (`packs.rs`, `ladder_rank: 0` — always in the lexicon), so this must never
/// gap for a placed people; panics loudly naming the culprit rather than
/// returning a `Result`, since a gap here is an invariant violation, not a
/// coverage fact to record (contrast the C3 planet-concept probe, which
/// gaps by design).
fn world_statement(
    kind: &str,
    planet_name: &str,
    evidential: Evidential,
    grammar: &hornvale_language::TongueGrammar,
    morph: &TongueMorphology,
    noun_class_of: &dyn Fn(&str) -> NounClass,
    lexicon: &hornvale_language::Lexicon,
) -> String {
    let clause = TongueClause {
        subject: planet_name.to_string(),
        complement_concept: "earth".to_string(),
        evidential,
    };
    realize_tongue_deep(&clause, grammar, morph, noun_class_of, lexicon).unwrap_or_else(|gap| {
        panic!(
            "the world-statement law is violated for {kind}: gap on {} ({}) — \"earth\" is \
             universal-stratum Steeped and must never gap",
            gap.concept, gap.reason
        )
    })
}

/// Predicates present in the ledger that C1's grammar cannot yet render:
/// registered predicates with at least one committed fact, excluding those
/// the grammar already covers (`is-a`, plus the construction table's
/// predicates), sorted and deduped.
/// type-audit: bare-ok(identifier-text)
pub fn uncovered_predicates(world: &World) -> Vec<String> {
    let mut gaps: BTreeSet<String> = BTreeSet::new();
    for predicate in world.registry.predicates() {
        let name = predicate.name.as_str();
        let covered = name == hornvale_kernel::world::IS_A
            || name == hornvale_kernel::INSTANCE_OF
            || CONSTRUCTION_ORDER.contains(&name);
        if !covered && world.ledger.find(name).next().is_some() {
            gaps.insert(name.to_string());
        }
    }
    gaps.into_iter().collect()
}

/// The Book reads itself (The Echo, T3): invert a rendered line back into
/// the classification and fragment facts that produced it. `subject` is
/// the clause's surface text as written (a name, or the fixed re-mention
/// pronoun `"it"`) — un-prefixing a collective's leading "The " is
/// deliberately NOT this campaign's job. `kind` is the classification
/// label: the `is-a` complement as-is for a singular line, or the
/// singular species (the trailing `'s'` `species_label` appended,
/// stripped back off) for a plural `instance-of` collective — see
/// [`parse_line`]'s doc for how that distinction is recovered. `facts` are
/// the modifier/trailing fragments' (predicate, surface value) pairs, in
/// the construction table's authored order ([`CONSTRUCTION_ORDER`]).
///
/// `number` and `definiteness` are the original clause's grammatical
/// features, needed by [`rerender`] to reconstruct the exact surface
/// (copula, determiner, and whether `kind` re-pluralizes) — deliberately
/// private: they are not part of this struct's three documented fields,
/// only plumbing between [`parse_line`] and [`rerender`] inside this
/// crate.
/// type-audit: bare-ok(prose: subject), bare-ok(identifier-text: kind), bare-ok(identifier-text: facts)
pub struct ParsedLine {
    /// The clause's surface subject text (a name, or the pronoun `"it"`).
    pub subject: String,
    /// The classification label — singular, even for a plural
    /// `instance-of` collective line.
    pub kind: String,
    /// The fragment-recovered (predicate, surface value) pairs, in
    /// [`CONSTRUCTION_ORDER`].
    pub facts: Vec<(String, Value)>,
    number: Number,
    definiteness: Definiteness,
}

/// Why [`parse_line`] could not invert a rendered line. Deliberately a
/// book-local type rather than widening T2's `ParseError`: a book-level
/// failure (an unrecognized trailing fragment) is not a construction the
/// domain's clause grammar knows about — `fragment_for`/`fact_for` are
/// windows/book's own construction table (see the module doc's
/// aggregation seam), so their failure mode stays here too.
/// type-audit: bare-ok(prose: UnknownFragment.0)
#[derive(Clone, Debug, PartialEq)]
pub enum LineError {
    /// T2's clause-level parse (`parse_common`) failed.
    Clause(ParseError),
    /// The clause parsed, but a modifier or trailing fragment's text
    /// matched no entry in [`fact_for`]'s inversion table.
    UnknownFragment(String),
}

impl std::fmt::Display for LineError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LineError::Clause(e) => write!(f, "clause parse failed: {e}"),
            LineError::UnknownFragment(frag) => write!(f, "unrecognized fragment: {frag:?}"),
        }
    }
}

impl std::error::Error for LineError {}

/// `cardinal`'s inverse (a private table, not shared with
/// `domains/language::clause`'s — see this module's `indefinite_article`
/// for the established precedent of duplicating a small presentation-layer
/// table across the aggregation seam rather than widening the domain's
/// public surface for a book-only need): word (`"two"`) or digits
/// (`"13"`) to the count.
fn uncardinal(word: &str) -> Option<u64> {
    const WORDS: [&str; 13] = [
        "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
        "eleven", "twelve",
    ];
    WORDS
        .iter()
        .position(|w| *w == word)
        .map(|i| i as u64)
        .or_else(|| word.parse().ok())
}

/// The construction table run backward: recover (predicate, surface
/// value) from one fragment's text — the mirror of [`fragment_for`].
/// Returns `None` for text this table's forward direction never produces,
/// which [`parse_line`] treats as `LineError::UnknownFragment`.
fn fact_for(fragment: &str) -> Option<(String, Value)> {
    if let Some(rest) = fragment.strip_prefix("with ") {
        let count_word = rest
            .strip_suffix(" moons")
            .or_else(|| rest.strip_suffix(" moon"))?;
        return Some((
            MOON_COUNT.to_string(),
            Value::Number(uncardinal(count_word)? as f64),
        ));
    }
    if let Some(rest) = fragment.strip_prefix("orbiting ") {
        let class = rest
            .strip_prefix("an ")
            .or_else(|| rest.strip_prefix("a "))?;
        return Some((STAR_CLASS.to_string(), Value::Text(class.to_string())));
    }
    if let Some(rest) = fragment.strip_prefix("its day lasts about ") {
        let days = rest.strip_suffix(" standard days")?;
        return Some((
            DAY_LENGTH_STD.to_string(),
            Value::Number(days.parse().ok()?),
        ));
    }
    None
}

/// The closed complement set a `parse_line` call recognizes for `world`:
/// every committed `is-a` object label, plus `species_label(kind)` for
/// every committed `instance-of` object (the only source of a plural
/// complement in this campaign's grammar — see [`parse_line`]'s doc for
/// why that lets `Number::Pl` alone signal a collective on the way back),
/// plus (C4 T4) every chorus account's `Substituted` target (e.g.
/// `"earth"`) — a book-layer carving that never appears as a committed
/// `is-a` object, so a chorus emic line naming it would otherwise parse as
/// `UnknownComplement`. The closed set stays derived from the world:
/// walking `accounts_of(world)` rather than hardcoding the carving text.
pub fn parse_context(world: &World) -> ParseContext {
    let mut complements = BTreeSet::new();
    for fact in world.ledger.find(hornvale_kernel::world::IS_A) {
        if let Value::Text(kind) = &fact.object {
            complements.insert(kind.clone());
        }
    }
    for fact in world.ledger.find(hornvale_kernel::INSTANCE_OF) {
        if let Value::Text(kind) = &fact.object {
            complements.insert(species_label(kind));
        }
    }
    for voice in hornvale_worldgen::accounts_of(world) {
        for entry in &voice.account.entries {
            if let Disposition::Substituted { theirs, .. } = effective(&entry.disposition) {
                complements.insert(theirs.clone());
            }
        }
    }
    ParseContext { complements }
}

/// Invert one rendered [`render_volume`] line: split on the trailing-clause
/// seam (`"; "`), clause-parse the head via T2's `parse_common`, then
/// recover each modifier/trailing fragment's fact via [`fact_for`].
///
/// The split mirrors [`assemble_trailing`] exactly: the first segment lost
/// its own terminal `'.'` to the strip in that join (append it back,
/// unless there was no trailing clause at all — then the head is the
/// whole, already-terminated line); the LAST segment carries the final
/// `'.'` restored by that same join, which belongs to the sentence, not
/// the fragment, so it is stripped before fragment inversion. Middle
/// segments (more than one trailing clause) carry no punctuation at all.
///
/// `ParsedLine.kind` recovers the singular: this campaign's grammar (see
/// [`render_volume`]) renders `Number::Pl` for exactly one construction —
/// the `instance-of` collective, whose complement `species_label` built by
/// appending `'s'` — so a plural clause's complement minus its trailing
/// `'s'` is always the singular kind. An `is-a` line is always `Sg`, so its
/// complement is used as-is. A future `Pl` `is-a` construction would need
/// to revisit this closed-world assumption.
/// type-audit: bare-ok(prose: line)
pub fn parse_line(line: &str, ctx: &ParseContext) -> Result<ParsedLine, LineError> {
    let segments: Vec<&str> = line.split("; ").collect();
    let (head, trailing_raw) = segments
        .split_first()
        .expect("str::split always yields at least one segment");
    let clause_text = if trailing_raw.is_empty() {
        (*head).to_string()
    } else {
        format!("{head}.")
    };
    let clause = parse_common(&clause_text, ctx).map_err(LineError::Clause)?;

    let mut facts = Vec::new();
    for modifier in &clause.modifiers {
        let (predicate, value) =
            fact_for(modifier).ok_or_else(|| LineError::UnknownFragment(modifier.clone()))?;
        facts.push((predicate, value));
    }
    let last = trailing_raw.len().saturating_sub(1);
    for (i, segment) in trailing_raw.iter().enumerate() {
        let text = if i == last {
            segment.strip_suffix('.').unwrap_or(segment)
        } else {
            segment
        };
        let (predicate, value) =
            fact_for(text).ok_or_else(|| LineError::UnknownFragment(text.to_string()))?;
        facts.push((predicate, value));
    }

    let subject = match &clause.subject {
        Subject::Name(name) => name.clone(),
        Subject::Pronoun(p) => (*p).to_string(),
    };
    let kind = if clause.number == Number::Pl {
        clause
            .complement
            .strip_suffix('s')
            .map(str::to_string)
            .unwrap_or_else(|| clause.complement.clone())
    } else {
        clause.complement.clone()
    };

    Ok(ParsedLine {
        subject,
        kind,
        facts,
        number: clause.number,
        definiteness: clause.definiteness,
    })
}

/// Re-realize a [`ParsedLine`] back to its exact surface text: the corpus
/// law's other half. Regroups `parsed.facts` into modifiers/trailing via
/// [`fragment_for`] (the same construction table, forward again),
/// re-pluralizes `kind` through `species_label` for a `Pl` clause, and
/// rebuilds the clause plus any trailing clause(s) through the same
/// [`assemble_trailing`] helper `render_volume` uses — so the two never
/// drift apart into separate join logic.
/// type-audit: bare-ok(prose: return)
pub fn rerender(parsed: &ParsedLine) -> String {
    let mut modifiers = Vec::new();
    let mut trailing = Vec::new();
    for (predicate, value) in &parsed.facts {
        match fragment_for(predicate, value) {
            Some(Fragment::Modifier(m)) => modifiers.push(m),
            Some(Fragment::Trailing(t)) => trailing.push(t),
            // fact_for only ever recovers (predicate, value) pairs that
            // fragment_for's forward direction produced, so this is
            // unreachable for a ParsedLine built by parse_line.
            None => {}
        }
    }
    let complement = match parsed.number {
        Number::Pl => species_label(&parsed.kind),
        Number::Sg => parsed.kind.clone(),
    };
    let subject = match parsed.subject.as_str() {
        "it" => Subject::Pronoun("it"),
        "its" => Subject::Pronoun("its"),
        other => Subject::Name(other.to_string()),
    };
    let line = realize_common(&ClauseSpec {
        frame: Frame::Classify,
        subject,
        complement,
        number: parsed.number,
        definiteness: parsed.definiteness,
        modifiers,
    });
    assemble_trailing(line, &trailing)
}

/// The book-layer dress [`parse_chorus_line`] strips before delegating to
/// [`parse_line`], and [`rerender_chorus_line`] restores after
/// [`rerender`] — the stance appositive and the margin's `"In truth, "`
/// prefix are chorus-surface presentation, never a `domains/language`
/// construction (the module doc's aggregation seam extended one layer
/// out).
/// type-audit: bare-ok(flag: in_truth), bare-ok(identifier-text: stance)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ChorusDress {
    /// The stance appositive's closed text (`"ourselves"`, `"neighbors"`,
    /// `"rivals"`, or `"strangers"`), if this line carried one.
    pub stance: Option<&'static str>,
    /// Whether this line carried the margin register's `"In truth, "`
    /// prefix.
    pub in_truth: bool,
}

/// The four stance appositive suffixes [`parse_chorus_line`] tries, in a
/// fixed order — the exact inverse of [`render_people_clause`]'s
/// `" — {stance}."` construction.
const STANCE_SUFFIXES: &[(&str, &str)] = &[
    (" — ourselves.", "ourselves"),
    (" — neighbors.", "neighbors"),
    (" — rivals.", "rivals"),
    (" — strangers.", "strangers"),
];

/// Task 4 (C5): one rendered because-clause explanation line's recovered
/// fields — the closed-table inversion of [`explanation_line`]. `head` is
/// carried verbatim (it already encodes which predicate and plurality this
/// explanation was about, so [`rerender_explanation`] never re-derives it
/// from a ground fact it doesn't have); deity names are free tokens,
/// recovered purely by their fixed position in the closed frame — never
/// checked against a roster (mirrors [`fact_for`]'s established precedent
/// for fragment text).
/// type-audit: bare-ok(prose: head), bare-ok(prose: agent), bare-ok(flag: plural)
#[derive(Clone, Debug, PartialEq)]
pub struct ParsedExplanation {
    /// The count-aware head clause this explanation opened with (e.g.
    /// `"The day returns"`).
    pub head: String,
    /// Whether `head` was the plural (moons) form — governs the
    /// Kinship/LinkSympathy pronoun on re-realization.
    pub plural: bool,
    /// The causal schema this line's frame matched.
    pub schema: SchemaId,
    /// The bound deity/agent name, for the frames that carry one.
    pub agent: Option<String>,
    /// The bound verb lexeme, for the agentive frame.
    pub lexeme: Option<LexemeId>,
    /// The manner adverb this line's agentive frame carried, if any.
    pub manner: Manner,
}

/// One rendered chorus line's recovered shape: an ordinary classification
/// clause ([`ParsedLine`] dressed by [`ChorusDress`]), or a Task 4
/// because-clause explanation ([`ParsedExplanation`]) — a wholly different
/// frame with no clause underneath it at all, so a `ParsedLine` would have
/// nothing to hold. Deliberately no derived traits beyond what its two
/// variants (`ParsedLine`/`ChorusDress`/[`ParsedExplanation`]) already
/// support — `ParsedLine` itself derives neither `Clone` nor `Debug` nor
/// `PartialEq` (T3's original design), and no caller in this module needs
/// this enum to carry any of them either.
/// type-audit: bare-ok(flag: RevealedClaim.plural)
pub enum ChorusLine {
    /// An ordinary classification clause plus its chorus-surface dress.
    Clause(ParsedLine, ChorusDress),
    /// A because-clause explanation line (Task 4's closed frame table).
    Explanation(ParsedExplanation),
    /// C6 (The Doctrine): the `RevealedClaim` exoteric formula.
    RevealedClaim {
        /// Whether this line used the plural ("moons") surface form.
        plural: bool,
    },
    /// C6 (The Doctrine): the disclosure law's counter-annotation — wraps
    /// the recovered folk sentence it quotes, re-parsed recursively (in
    /// practice always an [`Self::Explanation`], since a moon-count
    /// `Contested` state only ever arises when both accounts explain the
    /// fact — see [`doctrine_section`]'s doc — but the recursion makes no
    /// such assumption itself).
    Counter(Box<ChorusLine>),
    /// C8 (The Diachronic Book): one Reckoning-of-Years line.
    Reckoning(ReckoningLine),
}

/// C8 (The Diachronic Book): one Reckoning-of-Years line's recovered
/// shape — pure closed-string table inversion (the plan's four new
/// sentence shapes plus the truth margin, which carries a count rather
/// than a classification clause, so it cannot reuse `ParsedLine`). Carries
/// no ground-fact recovery: the section states counts drawn from
/// `hornvale_worldgen::{observations_of, ladder_of}`, not a `chorus_ground`
/// classification, so [`emic_union_margin_covers_ground_truth`]'s ground-
/// truth walk has nothing here to check.
/// type-audit: bare-ok(prose: Numbered.autonym), bare-ok(diagnostic-value: Numbered.count), bare-ok(diagnostic-value: Prediction.day), bare-ok(prose: Margin.epoch_phrase), bare-ok(diagnostic-value: Margin.count)
#[derive(Clone, Debug, PartialEq)]
pub enum ReckoningLine {
    /// `"The sky keeps no dates to number."` — the empty arm.
    Empty,
    /// `"The sky has darkened, now and again."` — the folk register.
    FolkCounted,
    /// `"The priesthood of the ⟨autonym⟩ numbers the darkenings: ⟨count⟩."`
    Numbered {
        /// The culture's autonym, as it appeared in the line.
        autonym: String,
        /// The witnessed cardinal this culture holds.
        count: u64,
    },
    /// `"The next darkening, it teaches, comes on day ⟨day⟩."`
    Prediction {
        /// The taught next event's integer-truncated day.
        day: u64,
    },
    /// `"In truth, the darkenings ⟨epoch_phrase⟩ number ⟨count⟩."` — the
    /// truth margin.
    Margin {
        /// Which epoch's phrase this margin used (`"of the first days"` /
        /// `"of the first hundred years"` for the committed pair, or an ad
        /// hoc `--at` phrase).
        epoch_phrase: String,
        /// The true event count.
        count: u64,
    },
}

/// The closed verb-literal table [`parse_explanation_body`] matches a
/// recovered word against, to hand back the same `'static` [`LexemeId`]
/// these were minted from (a `LexemeId` wraps a `&'static str`, so a
/// runtime-parsed word can never be boxed into one directly) — duplicated
/// from `domains/language::schemas`'s own closed table, the same
/// aggregation-seam precedent [`indefinite_article`]/[`uncardinal`] set for
/// small closed tables a book-only need doesn't warrant widening the
/// domain's public surface for.
const AGENTIVE_LEXEMES: &[LexemeId] = &[
    LexemeId("walks"),
    LexemeId("strides"),
    LexemeId("rides"),
    LexemeId("drives"),
    LexemeId("rows"),
    LexemeId("steers"),
    LexemeId("stalks"),
];

/// The count-aware head clauses [`parse_explanation`] tries, paired with
/// the plurality each carries — the exact inverse of [`explanation_head`].
/// None of the three is a prefix of another (`"The day returns"`, `"The
/// moon crosses"`, `"The moons cross"` all diverge by the 9th character),
/// so trying them in any order is safe.
const EXPLANATION_HEADS: &[(&str, bool)] = &[
    ("The day returns", false),
    ("The moon crosses", false),
    ("The moons cross", true),
];

/// Invert one explanation line's body (the text after its head clause) into
/// the closed table's schema/agent/lexeme/manner — the exact inverse of
/// [`explanation_line`]'s six frames, tried in an order chosen so no two
/// frames' fixed text can be mistaken for one another (`PathJourney`'s
/// fixed string is checked before `Agentive`'s open one, since both start
/// with `" because "`).
fn parse_explanation_body(
    rest: &str,
    plural: bool,
) -> Option<(SchemaId, Option<String>, Option<LexemeId>, Manner)> {
    if rest == ", as all things return." {
        return Some((SchemaId::CycleReturn, None, None, Manner::Neutral));
    }
    if rest == " because the sky must be crossed." {
        return Some((SchemaId::PathJourney, None, None, Manner::Neutral));
    }
    if rest == " to keep the balance." {
        return Some((SchemaId::Balance, None, None, Manner::Neutral));
    }
    let kin_prefix = if plural {
        " because they are "
    } else {
        " because it is "
    };
    if let Some(mid) = rest.strip_prefix(kin_prefix) {
        let deity = mid.strip_suffix("'s kin.")?;
        if deity.is_empty() || deity.contains(' ') {
            return None;
        }
        return Some((
            SchemaId::Kinship,
            Some(deity.to_string()),
            None,
            Manner::Neutral,
        ));
    }
    let link_prefix = if plural {
        " because they answer "
    } else {
        " because it answers "
    };
    if let Some(mid) = rest.strip_prefix(link_prefix) {
        let deity = mid.strip_suffix('.')?;
        if deity.is_empty() || deity.contains(' ') {
            return None;
        }
        return Some((
            SchemaId::LinkSympathy,
            Some(deity.to_string()),
            None,
            Manner::Neutral,
        ));
    }
    let mid = rest.strip_prefix(" because ")?;
    let (core, manner) = if let Some(base) = mid.strip_suffix(", briskly.") {
        (base, Manner::Brisk)
    } else if let Some(base) = mid.strip_suffix(", slowly.") {
        (base, Manner::Slow)
    } else {
        (mid.strip_suffix('.')?, Manner::Neutral)
    };
    let core = core.strip_suffix(" the sky")?;
    let (deity, verb) = core.split_once(' ')?;
    if deity.is_empty() || verb.is_empty() || verb.contains(' ') {
        return None;
    }
    let lexeme = AGENTIVE_LEXEMES.iter().find(|l| l.0 == verb).copied()?;
    Some((
        SchemaId::Agentive,
        Some(deity.to_string()),
        Some(lexeme),
        manner,
    ))
}

/// Try to invert `line` as a Task 4 explanation line: strip one of the
/// closed heads ([`EXPLANATION_HEADS`]), then invert the remainder via
/// [`parse_explanation_body`]. `None` when `line` doesn't start with any of
/// the closed heads, or the remainder matches none of the six frames — the
/// caller ([`parse_chorus_line`]) falls through to the ordinary clause path
/// in either case.
fn parse_explanation(line: &str) -> Option<ParsedExplanation> {
    for (head, plural) in EXPLANATION_HEADS {
        if let Some(rest) = line.strip_prefix(head)
            && let Some((schema, agent, lexeme, manner)) = parse_explanation_body(rest, *plural)
        {
            return Some(ParsedExplanation {
                head: (*head).to_string(),
                plural: *plural,
                schema,
                agent,
                lexeme,
                manner,
            });
        }
    }
    None
}

/// Re-realize a [`ParsedExplanation`] back to its exact surface text: the
/// closed-table forward direction ([`explanation_line`]), which always
/// succeeds for a `ParsedExplanation` [`parse_explanation`] actually
/// produced (every frame [`parse_explanation_body`] recovers already
/// carries whatever agent/lexeme that frame requires).
fn rerender_explanation(explanation: &ParsedExplanation) -> String {
    explanation_line(
        &explanation.head,
        explanation.plural,
        explanation.schema,
        explanation.agent.as_deref(),
        explanation.lexeme,
        explanation.manner,
    )
    .expect(
        "a ParsedExplanation was only ever constructed from a line explanation_line once produced",
    )
}

/// The two committed margin phrases [`parse_reckoning_line`] recognizes —
/// neither is a prefix of the other, so trying them in either order is
/// safe. An ad hoc `--at` phrase (`reckoning_at`) is never produced by the
/// committed artifact, so this closed table need not (and does not) cover
/// it.
const RECKONING_MARGIN_PHRASES: &[&str] = &[
    RECKONING_EPOCH_1_MARGIN_PHRASE,
    RECKONING_EPOCH_2_MARGIN_PHRASE,
];

/// Invert one Reckoning-of-Years line (C8) against its closed surfaces —
/// the plan's four new sentence shapes (the empty arm, the folk line, the
/// Numbered line, the prediction line) plus the truth margin. Tried before
/// [`parse_chorus_line`]'s existing arms: none of the Reckoning surfaces
/// can collide with a copula clause, a `RevealedClaim`/counter/explanation
/// prefix, or the ordinary `"In truth, "` margin dress, so trying this
/// first is safe. `None` when `line` matches none of the closed shapes,
/// so the caller falls through to its own arms unchanged.
fn parse_reckoning_line(line: &str) -> Option<ReckoningLine> {
    if line == RECKONING_EMPTY_ARM {
        return Some(ReckoningLine::Empty);
    }
    if line == RECKONING_FOLK_COUNTED {
        return Some(ReckoningLine::FolkCounted);
    }
    if let Some(rest) = line
        .strip_prefix("The priesthood of the ")
        .and_then(|r| r.strip_suffix('.'))
    {
        let (autonym, count_word) = rest.split_once(" numbers the darkenings: ")?;
        if autonym.is_empty() {
            return None;
        }
        let count = uncardinal(count_word)?;
        return Some(ReckoningLine::Numbered {
            autonym: autonym.to_string(),
            count,
        });
    }
    if let Some(rest) = line
        .strip_prefix("The next darkening, it teaches, comes on day ")
        .and_then(|r| r.strip_suffix('.'))
    {
        let day: u64 = rest.parse().ok()?;
        return Some(ReckoningLine::Prediction { day });
    }
    for &phrase in RECKONING_MARGIN_PHRASES {
        let prefix = format!("In truth, the darkenings {phrase} number ");
        if let Some(rest) = line.strip_prefix(&prefix).and_then(|r| r.strip_suffix('.')) {
            let count = uncardinal(rest)?;
            return Some(ReckoningLine::Margin {
                epoch_phrase: phrase.to_string(),
                count,
            });
        }
    }
    None
}

/// Re-realize a [`ReckoningLine`] back to its exact surface text — the
/// closed-table forward direction, the exact inverse of
/// [`parse_reckoning_line`].
fn rerender_reckoning_line(line: &ReckoningLine) -> String {
    match line {
        ReckoningLine::Empty => RECKONING_EMPTY_ARM.to_string(),
        ReckoningLine::FolkCounted => RECKONING_FOLK_COUNTED.to_string(),
        ReckoningLine::Numbered { autonym, count } => format!(
            "The priesthood of the {autonym} numbers the darkenings: {}.",
            cardinal(*count)
        ),
        ReckoningLine::Prediction { day } => {
            format!("The next darkening, it teaches, comes on day {day}.")
        }
        ReckoningLine::Margin {
            epoch_phrase,
            count,
        } => format!(
            "In truth, the darkenings {epoch_phrase} number {}.",
            cardinal(*count)
        ),
    }
}

/// Invert one rendered chorus line (emic, annotation, or margin): try C6's
/// two closed surfaces first — the exact `RevealedClaim` formula
/// ([`parse_revealed_claim`]) and the counter-annotation prefix (stripped,
/// then the embedded folk sentence re-parsed RECURSIVELY through this same
/// function — it can be any other `ChorusLine` variant) — then Task 4's
/// explanation frames ([`parse_explanation`] — a wholly different surface
/// with no classification clause underneath it, so all three must be tried
/// before the clause path below could mis-fail on them), then fall through
/// to the ordinary clause path: strip the margin's `"In truth, "` prefix,
/// then the stance appositive suffix (restoring the clause's terminal `'.'`
/// in its place), then delegate to [`parse_line`]. Returns the recovered
/// [`ChorusLine`] — the design-freedom variant the brief allows over a bare
/// `ParsedLine`. C8 (The Diachronic Book) adds a fifth try, first: the
/// Reckoning-of-Years closed shapes ([`parse_reckoning_line`]) — none of
/// their surfaces can collide with the other four (no copula, no
/// `RevealedClaim`/counter/explanation prefix), so trying it first is safe
/// and never shadows an existing line.
/// type-audit: bare-ok(prose: line)
pub fn parse_chorus_line(line: &str, ctx: &ParseContext) -> Result<ChorusLine, LineError> {
    if let Some(reckoning) = parse_reckoning_line(line) {
        return Ok(ChorusLine::Reckoning(reckoning));
    }
    if let Some(plural) = parse_revealed_claim(line) {
        return Ok(ChorusLine::RevealedClaim { plural });
    }
    if let Some(rest) = line.strip_prefix(COUNTER_PREFIX) {
        let inner = parse_chorus_line(rest, ctx)?;
        return Ok(ChorusLine::Counter(Box::new(inner)));
    }
    if let Some(explanation) = parse_explanation(line) {
        return Ok(ChorusLine::Explanation(explanation));
    }

    let (body, in_truth) = match line.strip_prefix("In truth, ") {
        Some(rest) => (rest, true),
        None => (line, false),
    };
    let (clause_text, stance) = match STANCE_SUFFIXES
        .iter()
        .find_map(|(suffix, name)| body.strip_suffix(suffix).map(|head| (head, *name)))
    {
        Some((head, name)) => (format!("{head}."), Some(name)),
        None => (body.to_string(), None),
    };
    let parsed = parse_line(&clause_text, ctx)?;
    Ok(ChorusLine::Clause(parsed, ChorusDress { stance, in_truth }))
}

/// Re-realize a [`ChorusLine`] back to the exact chorus surface text: for a
/// [`ChorusLine::Clause`], [`rerender`] then re-append the stance
/// appositive (replacing the terminal `.`) then re-prepend `"In truth, "` —
/// the exact inverse of [`parse_chorus_line`]'s strip order; for a
/// [`ChorusLine::Explanation`], [`rerender_explanation`]; for a
/// [`ChorusLine::RevealedClaim`], [`rerender_revealed_claim`]; for a
/// [`ChorusLine::Counter`], this SAME function recursively on the wrapped
/// line, then [`counter_annotation_line`] re-wraps it.
/// type-audit: bare-ok(prose: return)
pub fn rerender_chorus_line(line: &ChorusLine) -> String {
    match line {
        ChorusLine::Clause(parsed, dress) => {
            let mut line = rerender(parsed);
            if let Some(stance) = dress.stance {
                line.pop();
                line.push_str(&format!(" — {stance}."));
            }
            if dress.in_truth {
                line = format!("In truth, {line}");
            }
            line
        }
        ChorusLine::Explanation(explanation) => rerender_explanation(explanation),
        ChorusLine::RevealedClaim { plural } => rerender_revealed_claim(*plural),
        ChorusLine::Counter(inner) => counter_annotation_line(&rerender_chorus_line(inner)),
        ChorusLine::Reckoning(reckoning) => rerender_reckoning_line(reckoning),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn constant(seed: u64) -> World {
        use hornvale_astronomy::SkyPins;
        use hornvale_terrain::TerrainPins;
        use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

        build_world(
            hornvale_kernel::Seed(seed),
            &SkyPins::default(),
            SkyChoice::Constant,
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("constant world builds")
    }

    #[test]
    fn coverage_flags_name_as_uncovered() {
        let world = constant(1);
        let gaps = uncovered_predicates(&world);
        assert!(
            gaps.contains(&"name".to_string()),
            "name has no construction yet: {:?}",
            gaps
        );
        assert!(
            !gaps.contains(&"is-a".to_string()),
            "is-a is covered: {:?}",
            gaps
        );
    }

    #[test]
    fn volume_states_the_planet_is_a_planet() {
        use hornvale_astronomy::SkyPins;
        use hornvale_terrain::TerrainPins;
        use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

        let world = build_world(
            hornvale_kernel::Seed(1),
            &SkyPins::default(),
            SkyChoice::Constant,
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("seed 1 builds");

        let vol = render_volume(&world);
        assert!(
            vol.lines.iter().any(|l| l.ends_with(" is a planet.")),
            "the volume classifies the planet: {:?}",
            vol.lines
        );
    }

    fn generated(seed: u64) -> World {
        use hornvale_astronomy::SkyPins;
        use hornvale_terrain::TerrainPins;
        use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

        build_world(
            hornvale_kernel::Seed(seed),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("generated world builds")
    }

    /// Seed 1's real, committed values (verified against the world json):
    /// star class "yellow-white dwarf (F)", two moons, day-length-std
    /// 1.5507196 std days (`quantity` truncates that to "about 1.5"). This
    /// is the exact volume `hornvale -- book` renders for seed 1 ("Vebe").
    /// Modifier order is the construction table's AUTHORED order
    /// (`CONSTRUCTION_ORDER`: moons, then star, then day length — the
    /// G3-approved surface), independent of ledger commit order.
    #[test]
    fn planet_sentence_aggregates_moons_star_and_day_length() {
        let world = generated(1);
        let vol = render_volume(&world);
        let line = vol
            .lines
            .iter()
            .find(|l| l.contains(" is a planet"))
            .expect("the planet's sentence is present");
        assert_eq!(
            line,
            "Vebe is a planet with two moons, orbiting a yellow-white dwarf (F); \
             its day lasts about 1.5 standard days."
        );
    }

    /// PROC-15 coverage: predicates the construction table now renders must
    /// drop off the uncovered list.
    #[test]
    fn aggregated_predicates_drop_off_the_uncovered_list() {
        let world = generated(1);
        let gaps = uncovered_predicates(&world);
        for predicate in ["moon-count", "star-class", "day-length-std"] {
            assert!(
                !gaps.contains(&predicate.to_string()),
                "{predicate} is now rendered, so it should be covered: {:?}",
                gaps
            );
        }
    }

    /// Vowel-initial star classes (e.g. seed 3's "orange dwarf (K)") need
    /// "an", not "a" — a real seed exposed this via `regenerate-artifacts.sh`
    /// ("Zhqea is a planet orbiting a orange dwarf (K)…").
    #[test]
    fn star_class_modifier_chooses_an_before_a_vowel() {
        let value = Value::Text("orange dwarf (K)".to_string());
        let modifier = match fragment_for(STAR_CLASS, &value) {
            Some(Fragment::Modifier(m)) => m,
            _ => panic!("expected a Modifier fragment"),
        };
        assert_eq!(modifier, "orbiting an orange dwarf (K)");
    }

    /// C2 T5: every placed peopled species' `instance-of` collective
    /// renders as "The ⟨Autonym⟩ are ⟨species⟩." — exact strings, seed 1's
    /// real committed values (verified against the world json): goblin's
    /// collective is named "Vavako", hobgoblin's "Babako".
    #[test]
    fn instance_of_collective_renders_the_autonym_are_species() {
        let world = generated(1);
        let vol = render_volume(&world);
        assert!(
            vol.lines.iter().any(|l| l == "The Vavako are goblins."),
            "goblin collective renders as the autonym: {:?}",
            vol.lines
        );
        assert!(
            vol.lines.iter().any(|l| l == "The Babako are hobgoblins."),
            "hobgoblin collective renders as the autonym: {:?}",
            vol.lines
        );
    }

    /// PROC-15 coverage: `instance-of` is now rendered, so it must drop off
    /// the uncovered list too.
    #[test]
    fn instance_of_drops_off_the_uncovered_list() {
        let world = generated(1);
        let gaps = uncovered_predicates(&world);
        assert!(
            !gaps.contains(&"instance-of".to_string()),
            "instance-of is now rendered: {:?}",
            gaps
        );
    }

    /// Referring-expression reduction: a subject already named earlier in
    /// the volume is re-mentioned with a pronoun, not its name again. No
    /// real volume exercises this today (exactly one `is-a` fact ever lands
    /// per subject — see C2 T2), so this drives the mechanism directly
    /// rather than through `render_volume`.
    #[test]
    fn subject_for_uses_a_pronoun_on_remention() {
        let entity = hornvale_kernel::EntityId::new(1).expect("1 is a valid entity id");
        let mut named = BTreeSet::new();

        assert_eq!(
            subject_for(entity, "Vebe".to_string(), &mut named),
            Subject::Name("Vebe".to_string())
        );
        assert_eq!(
            subject_for(entity, "Vebe".to_string(), &mut named),
            Subject::Pronoun("it")
        );
    }

    /// C3 T3's self-statement law (spec §5): every placed people's autonym
    /// and own-kind concept are Steeped by construction (worldgen's
    /// `exposure_of`), so every placed people's tongue self-statement
    /// renders — no gaps. C7 T3 adds a second line per placed people (the
    /// emic world-statement — `every_people_states_the_world_in_its_tongue`
    /// pins that law directly), so `tongue_lines` now carries TWO lines per
    /// placed people, not one.
    #[test]
    fn every_placed_people_self_states_in_its_own_tongue() {
        for seed in [1u64, 2, 3] {
            let world = generated(seed);
            let vol = render_volume(&world);
            let peoples = hornvale_worldgen::placed_peoples(&world);
            assert_eq!(
                vol.tongue_lines.len(),
                2 * peoples.len(),
                "seed {seed}: two tongue lines per placed people (self-statement + \
                 C7's world-statement)"
            );
            for line in &vol.tongue_lines {
                assert!(line.contains(" ("), "line carries a gloss: {line}");
            }
        }
    }

    /// C7 T3's world-statement law (spec §3.5/§4.5): every placed people
    /// renders the emic world-statement — `earth` is universal-stratum
    /// Steeped (`packs.rs`, `ladder_rank: 0`), so this never gaps, joining
    /// the self-statement law above. One world-statement tongue line per
    /// placed people, seeds 1..=3, glossed `"⟨planet⟩ is the earth."`.
    #[test]
    fn every_people_states_the_world_in_its_tongue() {
        for seed in [1u64, 2, 3] {
            let world = generated(seed);
            let vol = render_volume(&world);
            let name = planet_name_of(&world).expect(
                "the planet is named once any people is placed (seeds 1..=3 all place one)",
            );
            let peoples = hornvale_worldgen::placed_peoples(&world);
            let gloss = format!("\"{name} is the earth.\")");
            let world_lines: Vec<&String> = vol
                .tongue_lines
                .iter()
                .filter(|l| l.ends_with(&gloss))
                .collect();
            assert_eq!(
                world_lines.len(),
                peoples.len(),
                "seed {seed}: one world-statement line per placed people: {:?}",
                vol.tongue_lines
            );
        }
    }

    /// T2's measured depth landscape (frozen; `depth_landscape_measured` in
    /// `windows/worldgen/tests/deep_grammar.rs` pins the SAME numbers at the
    /// derivation layer), reduced to the one bit
    /// `the_taught_contrast_is_visible_where_deep` needs: whether
    /// `evidential_depth` is non-`None` for this (seed, species). Every row
    /// here must also be an ORGANIZED culture (a doctrine section exists) —
    /// this is a separate, settlement-demography-driven fact from
    /// `evidential_depth` (a morphology draw), so the two can move
    /// independently across an absorb.
    ///
    /// Re-pinned post-absorption (the Confluence settlement re-baseline,
    /// merge a46749f): seed 3's hobgoblin (`Shteozqae`) is no longer
    /// organized at this seed (its doctrine/priesthood section is gone from
    /// the regenerated `the-book.md` — a settlement-demography fact, not a
    /// morphology change; its `evidential_depth` measured in
    /// `deep_grammar.rs` is unchanged at `None`), so its row is dropped
    /// rather than re-measured as `false`: this test requires
    /// `section.doctrine` to be `Some` for every listed (seed, kind), and it
    /// no longer is. Seed 3's new kobold (`Jjajjjo`) is ALSO not organized
    /// at this seed (no priesthood section renders for it either), so it is
    /// not added. Both arms (`true`/`false`) remain exercised by the
    /// surviving rows.
    ///
    /// Re-pinned again post-absorption (The Rains moisture epoch, this
    /// merge): seed 2's kobold is no longer organized under the epoch's
    /// re-derived settlement layout (its chorus section still renders — it
    /// still places, and `depth_landscape_measured` still pins its
    /// morphology at `None`/`None`, unchanged, since morphology is a draw
    /// the epoch does not touch — but its doctrine/priesthood section is
    /// gone from the regenerated `the-book.md`, a settlement-demography
    /// fact). Its row is dropped rather than re-measured as `false`, for the
    /// same reason as seed 3's hobgoblin above. Both arms remain exercised
    /// by the surviving rows (`true`: seeds 1/2 hobgoblin, seed 3 goblin;
    /// `false`: seeds 1/2 goblin).
    const EVIDENTIAL_DEPTH_LANDSCAPE: &[(u64, &str, bool)] = &[
        (1, "goblin", false),   // None
        (1, "hobgoblin", true), // Particle
        (2, "goblin", false),   // None
        (2, "hobgoblin", true), // Particle
        (3, "goblin", true),    // Particle
    ];

    /// C7 T3's taught-contrast law (spec §3.5, the visible payoff): for
    /// every organized culture whose evidential depth is non-`None`, the
    /// doctrine's taught line's realized SURFACE (excluding each line's own
    /// gloss, which always differs in text) differs from the folk
    /// world-statement's surface — the morpheme contrast; for depth `None`
    /// the two surfaces are byte-identical (no marking to contrast).
    /// Per-species arms pinned against T2's frozen landscape (both arms are
    /// exercised within seeds 1..=3, per the measured table).
    #[test]
    fn the_taught_contrast_is_visible_where_deep() {
        for &(seed, kind, evidential_marks) in EVIDENTIAL_DEPTH_LANDSCAPE {
            let world = generated(seed);
            let vol = render_volume(&world);
            let name = planet_name_of(&world).expect("the planet is named at every measured seed");
            let section = vol
                .chorus
                .iter()
                .find(|s| s.kind == kind)
                .unwrap_or_else(|| panic!("seed {seed}: {kind} has a chorus section"));
            let doctrine = section.doctrine.as_ref().unwrap_or_else(|| {
                panic!("seed {seed}: {kind} is organized (T2's measured landscape)")
            });
            let folk_world_line = vol
                .tongue_lines
                .iter()
                .find(|l| l.ends_with(&format!("(in the {kind} tongue: \"{name} is the earth.\")")))
                .unwrap_or_else(|| {
                    panic!("seed {seed}: {kind}'s folk world-statement line exists")
                });
            let folk_surface = folk_world_line
                .split(" (")
                .next()
                .expect("split always yields at least one piece");
            let taught_surface = doctrine
                .tongue_taught_line
                .split(" (")
                .next()
                .expect("split always yields at least one piece");
            if evidential_marks {
                assert_ne!(
                    taught_surface, folk_surface,
                    "seed {seed} {kind}: non-None evidential depth must show the taught \
                     contrast — folk {folk_surface:?} vs taught {taught_surface:?}"
                );
            } else {
                assert_eq!(
                    taught_surface, folk_surface,
                    "seed {seed} {kind}: evidential depth None must show no marking — folk \
                     and taught surfaces must be identical"
                );
            }
        }
    }

    /// C7 T3's readout law (spec §4.1): every rendered evidential equals the
    /// shipped epistemic state — this module's PRODUCTION tongue-construction
    /// sites (everything above the `mod tests` boundary) pass only
    /// `Evidential::Witnessed` (self-statement/world-statement/probes, all
    /// Steeped-grounded) or `Evidential::Taught` (the doctrine's taught
    /// line), never `Evidential::Inferred` — checked against the LITERAL
    /// construction sites (not rendered text, which cannot distinguish
    /// evidential values that happen to share a drawn marker form; see the
    /// seed-1 hobgoblin doctrine line measured during this task, where the
    /// Taught marker coincidentally renders identically to the Inanimate
    /// class marker). The loud guard: [`tests::readout_of`], the grounding
    /// function a future because-clause tongue surface would need, panics
    /// rather than silently minting `Evidential::Inferred` for an
    /// `Explained`-grounded (inference-only) disposition — driven
    /// synthetically, since no real account entry reaches this arm at the
    /// Classify-only floor (spec §6: the because-clause stays Common-only).
    #[test]
    fn the_readout_law() {
        let source = include_str!("lib.rs");
        let production = source
            .split("#[cfg(test)]\nmod tests {\n")
            .next()
            .expect("this module's own `mod tests` boundary must exist");
        assert!(
            !production.contains("Evidential::Inferred"),
            "no production construction site in this module may ever pass \
             Evidential::Inferred — it is floor-unreachable (the readout law)"
        );
        assert!(
            production.contains("evidential: Evidential::Witnessed"),
            "the self-statement/world-statement/probe construction sites must exist"
        );
        assert!(
            production.contains("Evidential::Taught"),
            "the doctrine's taught-line construction site must exist"
        );

        /// The readout law's grounding function, driven synthetically: map
        /// an account entry's disposition to the `Evidential` a tongue
        /// clause about it would need. `Kept`/`Substituted`/`Lost` are all
        /// directly perceived classifications → `Witnessed`; `Explained` (an
        /// inference/story overlay — C5's causal filter) has no authored
        /// in-tongue surface at the Classify-only floor (spec §6: "the
        /// because-clause stays Common-only") — panics naming the gap
        /// rather than silently returning `Inferred`, so a future verbal-
        /// tongue campaign must author that surface deliberately before this
        /// function may return it.
        fn readout_of(disposition: &Disposition) -> Evidential {
            match disposition {
                Disposition::Kept | Disposition::Substituted { .. } | Disposition::Lost(_) => {
                    Evidential::Witnessed
                }
                Disposition::Explained { .. } => panic!(
                    "no in-tongue surface is authored for an Explained-grounded (inferred) \
                     tongue line yet — the Classify-only floor only ever renders Witnessed/ \
                     Taught; author a tongue surface for the because-clause (or extend the \
                     tongue grammar past Classify) before this disposition may ground \
                     Evidential::Inferred"
                ),
            }
        }

        let panicked = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            readout_of(&Disposition::Explained {
                underlying: Box::new(Disposition::Lost(
                    hornvale_language::LossReason::BeyondCapability { domain: "sky" },
                )),
                schema: SchemaId::PathJourney,
                agent: None,
                lexeme: None,
                manner: Manner::Neutral,
            })
        }))
        .is_err();
        assert!(
            panicked,
            "readout_of must panic loudly on an Explained-grounded disposition rather than \
             silently return Evidential::Inferred"
        );
    }

    /// C7 T3's shallow-identity guarantee (plan G4): for every species T2
    /// measured at depth `(None, None, _)` on both axes — seed 2's goblin
    /// and kobold are the only such (seed, species) pairs within 1..=3 (T2's
    /// frozen landscape) — that people's self-statement line is
    /// BYTE-IDENTICAL to the pre-C7 committed string (the literal from
    /// `book/src/gallery/the-book.md` before this task's regeneration).
    #[test]
    fn shallow_species_lines_are_byte_identical_to_c3() {
        let world = generated(2);
        let vol = render_volume(&world);
        assert!(
            vol.tongue_lines.contains(
                &"Maetmea Gmaapmae. (in the goblin tongue: \"The Maetmea are goblins.\")"
                    .to_string()
            ),
            "seed 2 goblin's self-statement must be byte-identical to the pre-C7 artifact: {:?}",
            vol.tongue_lines
        );
        assert!(
            vol.tongue_lines.contains(
                &"Ngkoshngta Nggasdsha. (in the kobold tongue: \"The Ngkoshngta are kobolds.\")"
                    .to_string()
            ),
            "seed 2 kobold's self-statement must be byte-identical to the pre-C7 artifact: {:?}",
            vol.tongue_lines
        );
    }

    /// C3 T3's gap law (spec §5): no culture holds `planet` — every
    /// tongue's attempt to state the planet's kind gaps.
    #[test]
    fn the_planet_sentence_gaps_in_every_tongue() {
        let world = generated(1);
        let vol = render_volume(&world);
        let peoples = hornvale_worldgen::placed_peoples(&world);
        assert_eq!(
            vol.tongue_gaps.len(),
            peoples.len(),
            "one planet gap per tongue"
        );
        for gap in &vol.tongue_gaps {
            assert!(gap.contains("planet"), "the gap names the concept: {gap}");
        }
    }

    /// Tongue lines are a pure function of the world (same reconstruction
    /// idiom as every other C3 draw): re-rendering the same seed must
    /// reproduce byte-identical lines.
    #[test]
    fn tongue_lines_are_deterministic() {
        let a = render_volume(&generated(1)).tongue_lines;
        let b = render_volume(&generated(1)).tongue_lines;
        assert_eq!(a, b);
    }

    /// The Echo T3's corpus law: for each seed volume, every rendered line
    /// parses, and re-realizing the recovered `ParsedLine` reproduces the
    /// identical line — the Book's construction table is a true bijection
    /// over the lines it actually emits, not just a one-way renderer.
    ///
    /// The same pass also stands as the standing coverage gate: it collects
    /// every predicate any parsed line's `facts` actually contributed
    /// across seeds 1..=3, and asserts [`CONSTRUCTION_ORDER`] is a subset
    /// of what the corpus exercised. A future predicate added to the
    /// construction table but never surfaced by any of these three seeds
    /// reddens this assertion, forcing a corpus extension rather than
    /// letting an unexercised construction hide behind a green gate.
    #[test]
    fn every_book_line_round_trips() {
        let mut predicates_exercised: BTreeSet<String> = BTreeSet::new();
        for seed in [1u64, 2, 3] {
            let world = generated(seed);
            let ctx = parse_context(&world);
            let volume = render_volume(&world);
            for line in &volume.lines {
                let parsed = parse_line(line, &ctx)
                    .unwrap_or_else(|e| panic!("seed {seed} line failed: {line} ({e:?})"));
                let again = rerender(&parsed);
                assert_eq!(&again, line, "seed {seed}: re-realization drifted");
                for (predicate, _) in &parsed.facts {
                    predicates_exercised.insert(predicate.clone());
                }
            }
        }
        for predicate in CONSTRUCTION_ORDER {
            assert!(
                predicates_exercised.contains(*predicate),
                "{predicate} is in CONSTRUCTION_ORDER but no line across seeds 1..=3 \
                 exercised it: {:?}",
                predicates_exercised
            );
        }
    }

    /// The exhaustive-fragment property: `fact_for` inverts `fragment_for`
    /// over the closed fragment space this construction table can ever
    /// produce — every moon count 0..=13, every star class and day length
    /// actually committed across seeds 1..=3 (the real closed space, not a
    /// hand-picked sample — see the Concordance campaign's generator-
    /// coverage lesson).
    #[test]
    fn fact_for_inverts_fragment_for_over_the_closed_space() {
        for count in 0..=13u64 {
            let value = Value::Number(count as f64);
            let text = match fragment_for(MOON_COUNT, &value) {
                Some(Fragment::Modifier(m)) => m,
                _ => panic!("expected a Modifier fragment for moon-count {count}"),
            };
            assert_eq!(
                fact_for(&text),
                Some((MOON_COUNT.to_string(), Value::Number(count as f64))),
                "moon-count {count} did not round-trip through {text:?}"
            );
        }

        for seed in [1u64, 2, 3] {
            let world = generated(seed);
            for fact in world.ledger.find(STAR_CLASS) {
                let value = fact.object.clone();
                let text = match fragment_for(STAR_CLASS, &value) {
                    Some(Fragment::Modifier(m)) => m,
                    _ => panic!("expected a Modifier fragment for {value:?}"),
                };
                assert_eq!(
                    fact_for(&text),
                    Some((STAR_CLASS.to_string(), value.clone())),
                    "star-class did not round-trip through {text:?}"
                );
            }
            for fact in world.ledger.find(DAY_LENGTH_STD) {
                let Value::Number(days) = fact.object else {
                    continue;
                };
                let value = Value::Number(days);
                let text = match fragment_for(DAY_LENGTH_STD, &value) {
                    Some(Fragment::Trailing(t)) => t,
                    _ => panic!("expected a Trailing fragment for {days}"),
                };
                let truncated = (days * 10.0).trunc() / 10.0;
                assert_eq!(
                    fact_for(&text),
                    Some((DAY_LENGTH_STD.to_string(), Value::Number(truncated))),
                    "day-length did not round-trip through {text:?}"
                );
            }
        }
    }

    /// C4 T1: the coverage report is DERIVED — the probe inventory contains
    /// one entry per committed `is-a` complement concept (today: `planet`
    /// only), so a future renderable `is-a` kind auto-enters the report.
    #[test]
    fn tongue_probes_derive_from_committed_is_a_facts() {
        let world = generated(1);
        let probes = tongue_probes(&world);
        assert_eq!(probes.len(), 1, "seed 1 commits exactly one is-a fact");
        assert_eq!(probes[0].concept, "planet");
        assert_eq!(probes[0].subject, "Vebe");
    }

    /// C4 T1: the probe's SUCCESS path lands the realized line instead of
    /// silently vanishing — driven with a synthetic lexicon that Steeps
    /// `planet`, since no real culture holds it (mutation evidence: assert
    /// the realized text, not just Ok-ness).
    #[test]
    fn probe_success_path_yields_a_line() {
        use hornvale_language::{ExposureClass, build_lexicon};
        let world = generated(1);
        let ph = hornvale_worldgen::language_of(&world, "goblin");
        let grammar = hornvale_language::tongue_grammar(&world.seed, "goblin", &ph);
        let morph = hornvale_worldgen::tongue_morphology_of(&world, "goblin")
            .expect("goblin morphology derives at seed 1");
        let noun_class_of =
            |concept: &str| hornvale_worldgen::noun_class_of(&world, "goblin", concept);
        let mut exposures = BTreeMap::new();
        exposures.insert("planet".to_string(), ExposureClass::Steeped);
        let lexicon = build_lexicon(&world.seed, "goblin", "goblin", &ph, &ph, &exposures, &[]);
        let probe = TongueProbe {
            concept: "planet".to_string(),
            subject: "Vebe".to_string(),
        };
        let line = probe_tongue(&probe, "goblin", &grammar, &morph, &noun_class_of, &lexicon)
            .expect("a Steeped concept realizes");
        assert!(
            !line.is_empty() && line.ends_with('.'),
            "a realized sentence: {line}"
        );
        assert!(line.contains("Vebe"), "the probe subject appears: {line}");
    }

    /// C4 T1: the derived report reproduces C3's exact strings on seeds
    /// 1–3 — no regression, no artifact drift from the derivation.
    #[test]
    fn derived_report_matches_the_shipped_strings() {
        let world = generated(1);
        let vol = render_volume(&world);
        assert!(
            vol.tongue_gaps
                .iter()
                .any(|g| g == "goblin: gap — planet (no entry in this lexicon)"),
            "the derived gap line is byte-identical to C3's: {:?}",
            vol.tongue_gaps
        );
    }

    /// C4 T4, the null-filter law (spec §4.1): the identity params
    /// reproduce the god's-eye volume byte-identically — the gazetteer IS
    /// the chorus's degenerate case.
    #[test]
    fn identity_chorus_reproduces_the_gods_eye_lines() {
        let world = generated(1);
        let vol = render_volume(&world);
        let ground = hornvale_worldgen::chorus_ground(&world);
        let account = hornvale_language::account::account_of(
            &ground,
            &hornvale_language::account::identity_params(),
        );
        let section = voice_section("goblin", "Vavako", &account, &world);
        assert_eq!(
            section.emic, vol.lines,
            "identity filters == the god's-eye volume"
        );
        assert!(
            section.margin.is_empty(),
            "the null filter loses nothing — no margin"
        );
    }

    /// C4 T4: seed 1's goblin section — exact derived strings (real
    /// committed values, the C2 exact-string discipline).
    #[test]
    fn goblin_section_speaks_and_margins_seed_1() {
        let vol = render_volume(&generated(1));
        let goblin = vol
            .chorus
            .iter()
            .find(|s| s.kind == "goblin")
            .expect("goblin voice");
        assert_eq!(goblin.heading, "As the Vavako tell it");
        assert!(
            goblin.emic.contains(&"Vebe is the earth.".to_string()),
            "planet substituted to the carving: {:?}",
            goblin.emic
        );
        assert!(
            goblin
                .emic
                .contains(&"The Babako are hobgoblins — neighbors.".to_string()),
            "goblin stance: {:?}",
            goblin.emic
        );
        assert!(
            goblin
                .margin
                .iter()
                .any(|m| m.starts_with("In truth, Vebe is a planet")
                    && m.contains("two moons")
                    && m.contains("yellow-white dwarf")),
            "the margin carries what the stack lost: {:?}",
            goblin.margin
        );
    }

    /// C4 T4: hobgoblin reads rivals where goblin reads neighbors (seed
    /// 1) — the chorus DIFFERS beyond vocabulary within one world.
    #[test]
    fn seed_1_voices_disagree_on_stance() {
        let vol = render_volume(&generated(1));
        let hobgoblin = vol
            .chorus
            .iter()
            .find(|s| s.kind == "hobgoblin")
            .expect("hobgoblin voice");
        assert!(
            hobgoblin
                .emic
                .contains(&"The Vavako are goblins — rivals.".to_string()),
            "hobgoblin reads goblins as rivals: {:?}",
            hobgoblin.emic
        );
    }

    /// C4 T4: kobold keeps the moons goblin loses (seed 2) — knowledge
    /// divergence surfaces: kobold's emic world line contains "with one
    /// moon", goblin's does not; goblin's margin does.
    #[test]
    fn seed_2_kobold_sees_moons_goblin_margins_them() {
        let vol = render_volume(&generated(2));
        let kobold = vol
            .chorus
            .iter()
            .find(|s| s.kind == "kobold")
            .expect("seed 2 places a kobold voice");
        let goblin = vol
            .chorus
            .iter()
            .find(|s| s.kind == "goblin")
            .expect("goblin voice");
        assert!(
            kobold.emic.iter().any(|l| l.contains("with one moon")),
            "kobold sees the moon count: {:?}",
            kobold.emic
        );
        assert!(
            goblin.emic.iter().all(|l| !l.contains("moon")),
            "goblin's emic world line never mentions moons: {:?}",
            goblin.emic
        );
        assert!(
            goblin.margin.iter().any(|m| m.contains("moon")),
            "goblin's margin carries the moons it lost: {:?}",
            goblin.margin
        );
    }

    /// C4 T4, the margin law (spec §4.3): per culture, `emic ∪ margin ⊇
    /// chorus_ground` — measured by actually parsing every emic + margin
    /// line and checking each ground-truth fact against what the parser
    /// recovered, not by a subject-name tally that never touches
    /// `chorus_ground` or `ParsedLine.facts` (a fact silently vanishing
    /// from both registers must redden this test).
    ///
    /// Recovery, per parsed line: the fragment facts in `parsed.facts`
    /// (moon-count/star-class/day-length-std, verbatim), plus the
    /// classification itself — `parsed.kind`, already recovered singular
    /// by `parse_line` regardless of the clause's number. A parsed line's
    /// facts are filed under its own subject, canonicalized by stripping a
    /// collective's leading `"The "` (the only surface dressing between a
    /// `chorus_ground` subject and its emic display name); a pronoun
    /// re-mention (`"it"`) files under the section's most recently named
    /// subject, matching how the surface actually reads.
    ///
    /// Coverage, per `GroundFact`: a fragment fact must appear verbatim in
    /// its subject's recovered `(predicate, value)` pairs, with the one
    /// documented exception that `day-length-std`'s surface value is the
    /// `quantity`-truncated number (mirrors
    /// `fact_for_inverts_fragment_for_over_the_closed_space`'s `(days *
    /// 10.0).trunc() / 10.0`); an `is-a` or `instance-of` fact must appear
    /// as a recovered kind equal to its own ground-truth text for that
    /// subject — the margin law's whole point is that the TRUTH stays
    /// recoverable even when the emic paragraph substitutes ("Vebe is the
    /// earth"), so the truth text itself (not the substitution target) is
    /// what this test requires to surface, via the margin's "In truth, ⟨
    /// name⟩ is a planet" when the emic line alone lost it.
    #[test]
    fn emic_union_margin_covers_ground_truth() {
        for seed in [1u64, 2, 3] {
            let world = generated(seed);
            let ctx = parse_context(&world);
            let ground = hornvale_worldgen::chorus_ground(&world);
            let vol = render_volume(&world);
            for section in &vol.chorus {
                let mut recovered_facts: BTreeMap<String, Vec<(String, Value)>> = BTreeMap::new();
                let mut recovered_kinds: BTreeMap<String, Vec<String>> = BTreeMap::new();
                let mut current_subject: Option<String> = None;
                for line in section.emic.iter().chain(section.margin.iter()) {
                    let chorus_line = parse_chorus_line(line, &ctx).unwrap_or_else(|e| {
                        panic!(
                            "seed {seed} {}: line failed to parse: {line} ({e:?})",
                            section.kind
                        )
                    });
                    // Task 4: an explanation line carries no NEW ground
                    // fact — it comments on a fact the clause/margin above
                    // it already surfaced (spec §4.6, "explanation is not
                    // recovery" — see `the_margin_still_carries_the_truth`).
                    // It still had to parse (the panic above already
                    // guards that); just skip fact/kind accumulation.
                    let ChorusLine::Clause(parsed, _dress) = chorus_line else {
                        continue;
                    };
                    let subject = if parsed.subject == "it" {
                        current_subject.clone().unwrap_or_else(|| {
                            panic!(
                                "seed {seed} {}: pronoun re-mention with no prior \
                                 subject: {line}",
                                section.kind
                            )
                        })
                    } else {
                        let canonical = parsed
                            .subject
                            .strip_prefix("The ")
                            .unwrap_or(&parsed.subject)
                            .to_string();
                        current_subject = Some(canonical.clone());
                        canonical
                    };
                    recovered_facts
                        .entry(subject.clone())
                        .or_default()
                        .extend(parsed.facts.iter().cloned());
                    recovered_kinds
                        .entry(subject)
                        .or_default()
                        .push(parsed.kind.clone());
                }

                for gf in &ground {
                    let facts = recovered_facts.get(&gf.subject);
                    let kinds = recovered_kinds.get(&gf.subject);
                    if gf.predicate == MOON_COUNT || gf.predicate == STAR_CLASS {
                        let ok = facts.is_some_and(|fs| {
                            fs.iter()
                                .any(|(p, v)| p == &gf.predicate && v == &gf.object)
                        });
                        assert!(
                            ok,
                            "seed {seed} {}: ground fact {}={:?} on {:?} vanished from \
                             emic+margin — recovered facts for that subject: {:?}",
                            section.kind, gf.predicate, gf.object, gf.subject, facts
                        );
                    } else if gf.predicate == DAY_LENGTH_STD {
                        let Value::Number(days) = &gf.object else {
                            panic!(
                                "seed {seed} {}: day-length-std ground fact is non-numeric: \
                                 {:?}",
                                section.kind, gf.object
                            );
                        };
                        let truncated = (days * 10.0).trunc() / 10.0;
                        let ok = facts.is_some_and(|fs| {
                            fs.iter().any(|(p, v)| {
                                p == DAY_LENGTH_STD
                                    && matches!(v, Value::Number(n) if *n == truncated)
                            })
                        });
                        assert!(
                            ok,
                            "seed {seed} {}: ground fact day-length-std={days} (surfaces as \
                             {truncated}) on {:?} vanished from emic+margin — recovered \
                             facts for that subject: {:?}",
                            section.kind, gf.subject, facts
                        );
                    } else if gf.predicate == hornvale_kernel::world::IS_A
                        || gf.predicate == hornvale_kernel::INSTANCE_OF
                    {
                        let Value::Text(truth) = &gf.object else {
                            panic!(
                                "seed {seed} {}: classification ground fact is non-text: {:?}",
                                section.kind, gf.object
                            );
                        };
                        let ok = kinds.is_some_and(|ks| ks.iter().any(|k| k == truth));
                        assert!(
                            ok,
                            "seed {seed} {}: ground truth kind {truth:?} for {:?} vanished \
                             from emic+margin — recovered kinds for that subject: {:?}",
                            section.kind, gf.subject, kinds
                        );
                    }
                }
            }
        }
    }

    /// C4 T4, the corpus law extended: every chorus emic + margin line
    /// round-trips byte-identically through `parse_chorus_line` +
    /// `rerender_chorus_line` (mirrors `every_book_line_round_trips`).
    ///
    /// Task 4 extends this SAME walk (rather than a duplicate
    /// `every_explanation_line_round_trips`, per the plan's Key Context) to
    /// also cover the new because-clause explanation lines: `parse_chorus_line`
    /// now returns a [`ChorusLine`], so a line that inverts to
    /// `ChorusLine::Explanation` round-trips through this identical
    /// assertion. `explanation_seen` additionally asserts the walk actually
    /// encountered at least one — a future regression that stopped firing
    /// explanations could otherwise hide behind a vacuously-true round-trip.
    #[test]
    fn every_chorus_line_round_trips() {
        let mut explanation_seen = 0usize;
        for seed in [1u64, 2, 3] {
            let world = generated(seed);
            let ctx = parse_context(&world);
            let vol = render_volume(&world);
            for section in &vol.chorus {
                for line in section.emic.iter().chain(section.margin.iter()) {
                    let chorus_line = parse_chorus_line(line, &ctx).unwrap_or_else(|e| {
                        panic!(
                            "seed {seed} {}: line failed to parse: {line} ({e:?})",
                            section.kind
                        )
                    });
                    if matches!(chorus_line, ChorusLine::Explanation(_)) {
                        explanation_seen += 1;
                    }
                    let again = rerender_chorus_line(&chorus_line);
                    assert_eq!(
                        &again, line,
                        "seed {seed} {}: re-realization drifted",
                        section.kind
                    );
                }
            }
        }
        assert!(
            explanation_seen > 0,
            "the walk over seeds 1..=3 should encounter at least one Task 4 explanation line"
        );
    }

    /// Task 4 (C5): every placed culture's causal-filter explanation
    /// (`Disposition::Explained` on the day and/or moons entries) renders
    /// as an additional emic line, self-consistently matching what
    /// [`explanation_line`] builds from that entry's OWN bound fields —
    /// then the seed-1 goblin line is ALSO pinned as a literal string
    /// (measured against the real committed world), the C2 exact-string
    /// discipline.
    #[test]
    fn explanation_lines_render_for_the_measured_seeds() {
        for seed in [1u64, 2, 3] {
            let world = generated(seed);
            let vol = render_volume(&world);
            for voice in hornvale_worldgen::accounts_of(&world) {
                for entry in &voice.account.entries {
                    let Disposition::Explained {
                        schema,
                        agent,
                        lexeme,
                        manner,
                        ..
                    } = &entry.disposition
                    else {
                        continue;
                    };
                    let Some((head, plural)) =
                        explanation_head(&entry.fact.predicate, &entry.fact.object)
                    else {
                        continue;
                    };
                    let Some(expected) = explanation_line(
                        &head,
                        plural,
                        *schema,
                        agent.as_deref(),
                        *lexeme,
                        *manner,
                    ) else {
                        continue;
                    };
                    let section = vol
                        .chorus
                        .iter()
                        .find(|s| s.kind == voice.kind)
                        .unwrap_or_else(|| {
                            panic!("seed {seed}: {} has no chorus section", voice.kind)
                        });
                    assert!(
                        section.emic.contains(&expected),
                        "seed {seed} {}: expected explanation line missing: {expected:?} \
                         not in {:?}",
                        voice.kind,
                        section.emic
                    );
                }
            }
        }

        // Seed 1's real, measured goblin day explanation (verified against
        // the committed world): schema PathJourney, agentless, no manner —
        // the frame table's fixed string.
        let vol = render_volume(&generated(1));
        let goblin = vol
            .chorus
            .iter()
            .find(|s| s.kind == "goblin")
            .expect("goblin voice");
        assert!(
            goblin
                .emic
                .contains(&"The day returns because the sky must be crossed.".to_string()),
            "seed 1 goblin's pinned explanation line: {:?}",
            goblin.emic
        );
    }

    /// Task 4, the null-filter law extended: the identity account (used by
    /// `identity_chorus_reproduces_the_gods_eye_lines`) never runs through
    /// `explain` (only `accounts_of` calls it), so its chorus section must
    /// carry none of the six frames' distinguishing text — and
    /// `render_volume`'s god's-eye `lines` (never touched by C5's causal
    /// filter at all — `explain` only ever runs inside `voice_section` via
    /// `accounts_of`) stay exactly as C4 shipped them.
    #[test]
    fn the_null_volume_is_untouched() {
        let world = generated(1);
        let ground = hornvale_worldgen::chorus_ground(&world);
        let account = hornvale_language::account::account_of(
            &ground,
            &hornvale_language::account::identity_params(),
        );
        let section = voice_section("goblin", "Vavako", &account, &world);
        for line in section.emic.iter().chain(section.margin.iter()) {
            assert!(
                !line.contains("because"),
                "identity chorus must carry no explanation: {line}"
            );
            assert!(
                !line.contains("as all things"),
                "identity chorus must carry no explanation: {line}"
            );
            assert!(
                !line.contains("to keep the balance"),
                "identity chorus must carry no explanation: {line}"
            );
        }

        let vol = render_volume(&world);
        assert!(
            vol.lines.iter().any(|l| l
                == "Vebe is a planet with two moons, orbiting a yellow-white dwarf (F); \
                    its day lasts about 1.5 standard days."),
            "the god's-eye planet line stays exactly as C4 shipped it: {:?}",
            vol.lines
        );
        assert!(
            vol.lines.iter().any(|l| l == "The Vavako are goblins."),
            "the god's-eye collective line stays exactly as C4 shipped it: {:?}",
            vol.lines
        );
    }

    /// Task 4, spec §4.6 ("explanation is not recovery"): seed 1's goblin
    /// day entry is BOTH explained (an additional emic line) AND still
    /// margined (the causal filter never suppresses the etic margin's own
    /// lost-fragment carrier — that's C4's job, untouched by C5).
    #[test]
    fn the_margin_still_carries_the_truth() {
        let vol = render_volume(&generated(1));
        let goblin = vol
            .chorus
            .iter()
            .find(|s| s.kind == "goblin")
            .expect("goblin voice");
        assert!(
            goblin.emic.iter().any(|l| l.starts_with("The day returns")),
            "seed 1 goblin's day explanation renders: {:?}",
            goblin.emic
        );
        assert!(
            goblin
                .margin
                .iter()
                .any(|m| m.contains("its day lasts about 1.5 standard days")),
            "explanation is not recovery (spec §4.6) — the margin still carries \
             the truth: {:?}",
            goblin.margin
        );
    }

    /// C6 T3: every seed-1 placed culture is organized (Task 2's ledger
    /// #1 measurement), so every chorus section gains a doctrine section.
    /// Goblin's exact measured surface (verified against the committed
    /// world): heading names the priesthood; the emic carries the
    /// `RevealedClaim` exoteric formula for the moons (folk capability 0.5
    /// loses `moon-count`, doctrine's boosted 0.75 clears the 0.6
    /// threshold and keeps it) and a day explanation whose bound agent is
    /// the doctrine's own measured deity, Wowako (folk's own day
    /// explanation is agentless `PathJourney`, so this is genuinely a
    /// doctrine-only causal story, not an echo of folk's).
    #[test]
    fn seed_1_doctrine_sections_render() {
        let world = generated(1);
        let vol = render_volume(&world);
        let peoples = hornvale_worldgen::placed_peoples(&world);
        assert_eq!(
            vol.chorus.iter().filter(|s| s.doctrine.is_some()).count(),
            peoples.len(),
            "every seed-1 placed culture is organized: every chorus section \
             should gain a doctrine section"
        );

        let goblin = vol
            .chorus
            .iter()
            .find(|s| s.kind == "goblin")
            .expect("goblin voice");
        let doctrine = goblin
            .doctrine
            .as_ref()
            .expect("goblin's organized flagship gates in a doctrine section at seed 1");
        assert_eq!(doctrine.heading, "As the priesthood of the Vavako teach it");
        assert!(
            doctrine
                .emic
                .contains(&"The moons are counted and known to the priesthood.".to_string()),
            "the RevealedClaim exoteric formula for the moons: {:?}",
            doctrine.emic
        );
        assert!(
            doctrine
                .emic
                .contains(&"The day returns because Wowako strides the sky, briskly.".to_string()),
            "the measured doctrine day explanation, agent Wowako: {:?}",
            doctrine.emic
        );
    }

    /// C6 T3, the disclosure law (both directions). Measured (a full sweep
    /// over seeds 1..=5, then widened to 1..=40 to double-check, via a
    /// throwaway probe before writing this test, then deleted): every
    /// organized culture's `moon-count` entry is either `RevealedClaim`
    /// (folk capability under the 0.6 threshold, doctrine's +0.25-boosted
    /// capability over it) or `Harmony` (the one measured case where folk
    /// ALSO keeps it, seed 2's kobold — same schema both sides, differing
    /// only in lexeme, which `conflict_of` ignores); every `day-length-std`
    /// entry is either `Harmony` or `Mystery` (`day-length-std` is
    /// `CrossReferential`, never folk-verifiable, so a differing schema
    /// there can only ever be `Mystery`, never `Contested`). **NO real
    /// `Contested` entry exists across seeds 1..=40** — the C5 F2 lesson's
    /// negative-result case: rather than a live sweep assertion that would
    /// vacuously and permanently pass, the disclosure law's `Contested`
    /// half is driven directly below with a synthetic folk/doctrine pair
    /// (kobold-style: folk KEEPS the moon count, but the two accounts
    /// explain it under differing schemas — Task 1's own Contested grid
    /// case, wired through the real renderer).
    #[test]
    fn the_disclosure_law_both_directions() {
        let mut mystery_seen = false;
        for seed in 1u64..=5 {
            let world = generated(seed);
            for voice in hornvale_worldgen::accounts_of(&world) {
                let Some(doctrine) = hornvale_worldgen::doctrine_of(&world, &voice.kind) else {
                    continue;
                };
                for d_entry in &doctrine.account.entries {
                    if d_entry.fact.predicate == hornvale_kernel::world::IS_A
                        || d_entry.fact.predicate == hornvale_kernel::INSTANCE_OF
                    {
                        continue;
                    }
                    let Some(f_entry) = voice.account.entries.iter().find(|e| {
                        e.fact.subject == d_entry.fact.subject
                            && e.fact.predicate == d_entry.fact.predicate
                    }) else {
                        continue;
                    };
                    let verifiable =
                        hornvale_worldgen::folk_verifiable(&voice.params, &d_entry.fact.predicate);
                    let conflict =
                        conflict_of(&f_entry.disposition, &d_entry.disposition, verifiable);
                    assert_ne!(
                        conflict,
                        ConflictState::Contested,
                        "seed {seed} {} {}: no real Contested entry exists across seeds \
                         1..=5 (measured) — a future worldgen change that introduces one \
                         should redden this assertion rather than going silently uncovered",
                        voice.kind,
                        d_entry.fact.predicate
                    );
                    if conflict == ConflictState::Mystery {
                        mystery_seen = true;
                    }
                }
            }
        }
        assert!(
            mystery_seen,
            "the sweep should find at least one real Mystery entry (day-length-std)"
        );

        // The Mystery-carries-none half, non-vacuously: since no Contested
        // entry exists (asserted above), every doctrine section's
        // annotations are empty — including every section that DOES carry
        // a Mystery entry (mystery_seen, just asserted).
        for seed in 1u64..=5 {
            let world = generated(seed);
            let vol = render_volume(&world);
            for section in &vol.chorus {
                if let Some(doctrine) = &section.doctrine {
                    assert!(
                        doctrine.annotations.is_empty(),
                        "seed {seed} {}: a Mystery entry's section must carry no \
                         annotation: {:?}",
                        section.kind,
                        doctrine.annotations
                    );
                }
            }
        }

        // The Contested half: a synthetic folk/doctrine pair over one
        // subject ("Vebe", is-a "planet" plus a moon-count of two), built
        // by running the real `account_of` (so every entry gets a real
        // `original_index`, never hand-constructed) and then mutating the
        // moon-count entry's disposition on each side — the same pattern
        // `domains/language::account`'s own `explained_is_dial_blind` test
        // uses.
        let subject = "Vebe".to_string();
        let ground = vec![
            hornvale_language::GroundFact {
                subject: subject.clone(),
                predicate: hornvale_kernel::world::IS_A.to_string(),
                object: Value::Text("planet".to_string()),
            },
            hornvale_language::GroundFact {
                subject: subject.clone(),
                predicate: MOON_COUNT.to_string(),
                object: Value::Number(2.0),
            },
        ];
        let mut observability = BTreeMap::new();
        observability.insert(
            hornvale_kernel::world::IS_A.to_string(),
            hornvale_language::Observability {
                requirement: hornvale_language::Requirement::Manifest,
                domain: "sky",
                concept: hornvale_language::NeededConcept::Object,
                shape: hornvale_language::FactShape::Taxonomy,
            },
        );
        observability.insert(
            MOON_COUNT.to_string(),
            hornvale_language::Observability {
                requirement: hornvale_language::Requirement::SkyGraded { threshold: 0.6 },
                domain: "sky",
                concept: hornvale_language::NeededConcept::Fixed("moon"),
                shape: hornvale_language::FactShape::Count,
            },
        );
        let mut holdings = BTreeSet::new();
        holdings.insert("planet".to_string());
        holdings.insert("moon".to_string());
        let params = AccountParams {
            hold_all: false,
            holdings,
            observability,
            sky_capability: 1.0,
            order: hornvale_language::OrderPolicy::Ground,
            stances: BTreeMap::new(),
            world_carving: None,
        };

        let base = hornvale_language::account_of(&ground, &params);
        assert_eq!(
            base.entries[1].disposition,
            Disposition::Kept,
            "the fixture's moon-count must start Kept (capability clears the 0.6 threshold)"
        );

        let mut folk_entries = base.entries.clone();
        folk_entries[1].disposition = Disposition::Explained {
            underlying: Box::new(Disposition::Kept),
            schema: SchemaId::PathJourney,
            agent: None,
            lexeme: None,
            manner: Manner::Neutral,
        };
        let folk_account = Account {
            entries: folk_entries,
        };

        let mut doctrine_entries = base.entries.clone();
        doctrine_entries[1].disposition = Disposition::Explained {
            underlying: Box::new(Disposition::Kept),
            schema: SchemaId::Agentive,
            agent: Some("Vamu".to_string()),
            lexeme: Some(LexemeId("walks")),
            manner: Manner::Neutral,
        };
        let doctrine_voice = hornvale_worldgen::DoctrineVoice {
            kind: "kobold".to_string(),
            params: params.clone(),
            account: Account {
                entries: doctrine_entries,
            },
        };

        // The taught line is not under test in this fixture (a synthetic
        // Contested pair over a hand-built account, not a real world) — any
        // non-empty placeholder satisfies `doctrine_section`'s signature.
        let section = doctrine_section(
            "Nggoshk",
            &doctrine_voice,
            &params,
            &folk_account,
            "placeholder — not under test here".to_string(),
        );
        assert_eq!(
            section.annotations,
            vec![
                "— though the folk say The moons cross because the sky must be crossed."
                    .to_string()
            ],
            "the disclosure law's Contested counter-annotation, synthetic pair: {:?}",
            section.annotations
        );
        assert!(
            section
                .emic
                .contains(&"Vebe is a planet with two moons.".to_string()),
            "a Contested entry renders its ordinary fragment (unlike RevealedClaim, which \
             would suppress it): {:?}",
            section.emic
        );
    }

    /// Final-review F1 (C6): the disclosure law fails LOUDLY when a
    /// Contested entry cannot produce its folk counter-annotation — the
    /// asymmetric shape ledger #9 made reachable in principle (a bare-Kept
    /// folk side has no explanation line to quote). Same fixture as the
    /// synthetic Contested pair above, minus the folk-side explanation:
    /// folk keeps the moons PLAINLY, doctrine explains them — Contested by
    /// explanatory parity, zero quotable folk lines, so the renderer must
    /// panic rather than silently drop the mandatory annotation.
    #[test]
    #[should_panic(expected = "disclosure law: Contested entry")]
    fn a_contested_entry_with_no_folk_line_panics_rather_than_vanishing() {
        let subject = "Vebe".to_string();
        let ground = vec![
            hornvale_language::GroundFact {
                subject: subject.clone(),
                predicate: hornvale_kernel::world::IS_A.to_string(),
                object: Value::Text("planet".to_string()),
            },
            hornvale_language::GroundFact {
                subject: subject.clone(),
                predicate: MOON_COUNT.to_string(),
                object: Value::Number(2.0),
            },
        ];
        let mut observability = BTreeMap::new();
        observability.insert(
            hornvale_kernel::world::IS_A.to_string(),
            hornvale_language::Observability {
                requirement: hornvale_language::Requirement::Manifest,
                domain: "sky",
                concept: hornvale_language::NeededConcept::Object,
                shape: hornvale_language::FactShape::Taxonomy,
            },
        );
        observability.insert(
            MOON_COUNT.to_string(),
            hornvale_language::Observability {
                requirement: hornvale_language::Requirement::SkyGraded { threshold: 0.6 },
                domain: "sky",
                concept: hornvale_language::NeededConcept::Fixed("moon"),
                shape: hornvale_language::FactShape::Count,
            },
        );
        let mut holdings = BTreeSet::new();
        holdings.insert("planet".to_string());
        holdings.insert("moon".to_string());
        let params = AccountParams {
            hold_all: false,
            holdings,
            observability,
            sky_capability: 1.0,
            order: hornvale_language::OrderPolicy::Ground,
            stances: BTreeMap::new(),
            world_carving: None,
        };
        let base = hornvale_language::account_of(&ground, &params);
        // Folk side: bare Kept (NO explanation) — the asymmetric Contested shape.
        let folk_account = base.clone();
        let mut doctrine_entries = base.entries.clone();
        doctrine_entries[1].disposition = Disposition::Explained {
            underlying: Box::new(Disposition::Kept),
            schema: SchemaId::Agentive,
            agent: Some("Vamu".to_string()),
            lexeme: Some(LexemeId("walks")),
            manner: Manner::Neutral,
        };
        let doctrine_voice = hornvale_worldgen::DoctrineVoice {
            kind: "kobold".to_string(),
            params: params.clone(),
            account: Account {
                entries: doctrine_entries,
            },
        };
        let _ = doctrine_section(
            "Nggoshk",
            &doctrine_voice,
            &params,
            &folk_account,
            "placeholder — not under test here".to_string(),
        );
    }

    /// C6 T3, the null-effect law: this campaign adds a NEW field
    /// (`doctrine`) to `ChorusSection` but must not perturb one byte of the
    /// pre-existing folk registers — seed 1's goblin and hobgoblin `emic`
    /// and `margin` vectors, pinned exactly (not just `contains`) against
    /// their pre-C6 committed strings.
    #[test]
    fn folk_sections_are_byte_unchanged() {
        let vol = render_volume(&generated(1));
        let goblin = vol
            .chorus
            .iter()
            .find(|s| s.kind == "goblin")
            .expect("goblin voice");
        assert_eq!(
            goblin.emic,
            vec![
                "The Vavako are goblins — ourselves.".to_string(),
                "The Babako are hobgoblins — neighbors.".to_string(),
                "Vebe is the earth.".to_string(),
                "The day returns because the sky must be crossed.".to_string(),
            ]
        );
        assert_eq!(
            goblin.margin,
            vec![
                "In truth, Vebe is a planet with two moons, orbiting a yellow-white dwarf \
                 (F); its day lasts about 1.5 standard days."
                    .to_string()
            ]
        );

        let hobgoblin = vol
            .chorus
            .iter()
            .find(|s| s.kind == "hobgoblin")
            .expect("hobgoblin voice");
        assert_eq!(
            hobgoblin.emic,
            vec![
                "The Vavako are goblins — rivals.".to_string(),
                "The Babako are hobgoblins — ourselves.".to_string(),
                "Vebe is the earth.".to_string(),
                "The day returns, as all things return.".to_string(),
            ]
        );
        assert_eq!(
            hobgoblin.margin,
            vec![
                "In truth, Vebe is a planet with two moons, orbiting a yellow-white dwarf \
                 (F); its day lasts about 1.5 standard days."
                    .to_string()
            ]
        );
    }

    /// C6 T3, the corpus law extended once more (mirrors
    /// `every_chorus_line_round_trips`): every doctrine `emic` +
    /// `annotations` + `margin` line, across seeds 1..=5, round-trips
    /// byte-identically through `parse_chorus_line` + `rerender_chorus_line`
    /// — the `RevealedClaim` formula inverts to (its plurality), and a
    /// counter-annotation inverts by stripping the fixed prefix and
    /// re-parsing the embedded folk sentence recursively (exercised
    /// directly here too, since no real seed carries one — see
    /// `the_disclosure_law_both_directions`). `revealed_claim_seen` guards
    /// against a vacuously-true walk that stopped firing `RevealedClaim`
    /// lines entirely.
    #[test]
    fn every_doctrine_line_round_trips() {
        let mut revealed_claim_seen = 0usize;
        for seed in 1u64..=5 {
            let world = generated(seed);
            let ctx = parse_context(&world);
            let vol = render_volume(&world);
            for section in &vol.chorus {
                let Some(doctrine) = &section.doctrine else {
                    continue;
                };
                for line in doctrine
                    .emic
                    .iter()
                    .chain(doctrine.annotations.iter())
                    .chain(doctrine.margin.iter())
                {
                    let chorus_line = parse_chorus_line(line, &ctx).unwrap_or_else(|e| {
                        panic!(
                            "seed {seed} {} (doctrine): line failed to parse: {line} ({e:?})",
                            section.kind
                        )
                    });
                    if matches!(chorus_line, ChorusLine::RevealedClaim { .. }) {
                        revealed_claim_seen += 1;
                    }
                    let again = rerender_chorus_line(&chorus_line);
                    assert_eq!(
                        &again, line,
                        "seed {seed} {} (doctrine): re-realization drifted",
                        section.kind
                    );
                }
            }
        }
        assert!(
            revealed_claim_seen > 0,
            "the walk over seeds 1..=5 should encounter at least one RevealedClaim line"
        );

        // The counter-annotation's own round trip, driven directly (no
        // real seed carries one — see `the_disclosure_law_both_directions`
        // for the measured absence): stripping the prefix and re-parsing
        // the embedded folk sentence recursively must invert exactly.
        let ctx = parse_context(&generated(1));
        let annotation = "— though the folk say The moons cross because the sky must be crossed.";
        let parsed = parse_chorus_line(annotation, &ctx).expect("a counter-annotation line parses");
        match &parsed {
            ChorusLine::Counter(inner) => {
                assert!(
                    matches!(**inner, ChorusLine::Explanation(_)),
                    "the embedded folk sentence should recover as an Explanation"
                );
            }
            _ => panic!("expected a ChorusLine::Counter"),
        }
        assert_eq!(rerender_chorus_line(&parsed), annotation);
    }

    /// T3 review, mandated carry-over #2: pin doctrine-margin sparseness
    /// with a real assertion (not just "the margin's law is reused
    /// unmodified" — an exact-string check that it actually behaves that
    /// way at seed 1). The moon-count fact is `Lost` on the folk side (so
    /// the folk margin carries its truth, "with two moons") but
    /// `RevealedClaim` on the doctrine side — kept, not lost, so the SAME
    /// truth must never repeat in the doctrine's own margin.
    #[test]
    fn doctrine_margin_omits_what_the_folk_margin_reveals() {
        let vol = render_volume(&generated(1));
        let goblin = vol
            .chorus
            .iter()
            .find(|s| s.kind == "goblin")
            .expect("goblin voice");
        assert!(
            goblin.margin.iter().any(|m| m.contains("with two moons")),
            "the folk margin must carry the lost moon-count truth: {:?}",
            goblin.margin
        );
        let doctrine = goblin
            .doctrine
            .as_ref()
            .expect("goblin's organized flagship gates in a doctrine section at seed 1");
        assert!(
            !doctrine.margin.iter().any(|m| m.contains("with two moons")),
            "the doctrine margin must NOT repeat the moon count: moon-count is a \
             RevealedClaim on the doctrine side (kept, not lost), so the margin's \
             sparseness law (it carries only what's lost/substituted) must exclude it: \
             {:?}",
            doctrine.margin
        );
    }

    /// T3 review, mandated carry-over #1 (the vanishing-realizable class):
    /// a future non-`MOON_COUNT` `RevealedClaim` entry must panic, loudly
    /// naming the predicate, rather than silently disappearing from both
    /// the doctrine emic and margin. Synthetic pair (predicate
    /// `STAR_CLASS`, not `MOON_COUNT`): folk `Lost` (capability 0.0, below
    /// threshold), doctrine `Kept` (capability 1.0) — a genuine
    /// `RevealedClaim`, built the same way `the_disclosure_law_both_directions`
    /// builds its synthetic Contested pair (`account_of` first, so every
    /// entry carries a real `original_index`, then mutate dispositions).
    #[test]
    #[should_panic(expected = "has no exoteric formula authored")]
    fn a_non_moon_revealed_claim_panics_rather_than_vanishing() {
        let subject = "Vebe".to_string();
        let ground = vec![
            hornvale_language::GroundFact {
                subject: subject.clone(),
                predicate: hornvale_kernel::world::IS_A.to_string(),
                object: Value::Text("planet".to_string()),
            },
            hornvale_language::GroundFact {
                subject: subject.clone(),
                predicate: STAR_CLASS.to_string(),
                object: Value::Text("yellow-white dwarf (F)".to_string()),
            },
        ];
        let mut observability = BTreeMap::new();
        observability.insert(
            hornvale_kernel::world::IS_A.to_string(),
            hornvale_language::Observability {
                requirement: hornvale_language::Requirement::Manifest,
                domain: "sky",
                concept: hornvale_language::NeededConcept::Object,
                shape: hornvale_language::FactShape::Taxonomy,
            },
        );
        observability.insert(
            STAR_CLASS.to_string(),
            hornvale_language::Observability {
                requirement: hornvale_language::Requirement::SkyGraded { threshold: 0.6 },
                domain: "sky",
                concept: hornvale_language::NeededConcept::Fixed("star"),
                shape: hornvale_language::FactShape::Taxonomy,
            },
        );
        let mut holdings = BTreeSet::new();
        holdings.insert("planet".to_string());
        holdings.insert("star".to_string());

        let folk_params = AccountParams {
            hold_all: false,
            holdings,
            observability,
            sky_capability: 0.0,
            order: hornvale_language::OrderPolicy::Ground,
            stances: BTreeMap::new(),
            world_carving: None,
        };

        let base = hornvale_language::account_of(&ground, &folk_params);
        assert_eq!(
            base.entries[1].disposition,
            Disposition::Lost(hornvale_language::LossReason::BeyondCapability { domain: "sky" }),
            "the fixture's star-class must start Lost at capability 0.0"
        );

        let folk_account = Account {
            entries: base.entries.clone(),
        };

        let mut doctrine_entries = base.entries.clone();
        doctrine_entries[1].disposition = Disposition::Kept;
        let doctrine_params = AccountParams {
            sky_capability: 1.0,
            ..folk_params.clone()
        };
        let doctrine_voice = hornvale_worldgen::DoctrineVoice {
            kind: "synthetic".to_string(),
            params: doctrine_params,
            account: Account {
                entries: doctrine_entries,
            },
        };

        // Panics: STAR_CLASS carries no `revealed_claim_line` formula arm.
        doctrine_section(
            "Nggoshk",
            &doctrine_voice,
            &folk_params,
            &folk_account,
            "placeholder — not under test here".to_string(),
        );
    }

    /// C6 T4, the esoteric law, mutation-verified: an empty reader
    /// discloses nothing (the committed exoteric edition is unaffected —
    /// its `RevealedClaim` formula still renders as usual), while a reader
    /// holding exactly `("Vebe", "moon-count")` gets exactly one initiated
    /// line, whose cardinal traces to the LEDGER's own committed value
    /// (never any other source) — proven by checking the real ledger
    /// value independently, then demonstrating that the WRONG cardinal
    /// does not match what `esoteric_lines` produced.
    #[test]
    fn the_esoteric_law_mutation_verified() {
        let world = generated(1);

        // Empty reader -> no initiated lines.
        let empty_reader: BTreeSet<(String, String)> = BTreeSet::new();
        assert!(
            esoteric_lines(&world, &empty_reader).is_empty(),
            "an empty reader must be told nothing"
        );

        // The exoteric formula is still present, untouched by the reader.
        let vol = render_volume(&world);
        let goblin = vol
            .chorus
            .iter()
            .find(|s| s.kind == "goblin")
            .expect("goblin voice");
        let doctrine = goblin
            .doctrine
            .as_ref()
            .expect("goblin's organized flagship gates in a doctrine section at seed 1");
        assert!(
            doctrine
                .emic
                .contains(&"The moons are counted and known to the priesthood.".to_string()),
            "the exoteric formula must render regardless of any reader: {:?}",
            doctrine.emic
        );

        // The initiated reader: exactly one line, for exactly this key.
        let mut reader: BTreeSet<(String, String)> = BTreeSet::new();
        reader.insert(("Vebe".to_string(), MOON_COUNT.to_string()));
        let lines = esoteric_lines(&world, &reader);
        assert_eq!(
            lines,
            vec!["— two, as the initiated count.".to_string()],
            "exactly one initiated line for the one key in the reader"
        );

        // Mutation-verify: the "two" comes from the LEDGER's own value, not
        // some other source — independently read the ledger's committed
        // moon-count for Vebe and confirm it (not a different number)
        // reproduces the line, then confirm a WRONG value's rendering
        // would NOT match what `esoteric_lines` actually produced (the
        // "verify the mechanism by asserting a WRONG expected value
        // fails" arm this test's own doc calls out).
        let vebe = entity_named(&world, "Vebe").expect("Vebe resolves to an entity");
        let ledger_value = match world.ledger.value_of(vebe, MOON_COUNT) {
            Some(Value::Number(n)) => *n as u64,
            other => panic!("Vebe's ledger moon-count must be a Value::Number: {other:?}"),
        };
        assert_eq!(
            ledger_value, 2,
            "Vebe's committed moon-count is two at seed 1"
        );
        assert_eq!(
            lines[0],
            format!("— {}, as the initiated count.", cardinal(ledger_value)),
            "the line's cardinal must equal the ledger's own value"
        );
        let wrong = format!("— {}, as the initiated count.", cardinal(ledger_value + 1));
        assert_ne!(
            lines[0], wrong,
            "a wrong cardinal must not match what esoteric_lines produced — proving \
             the assertion above is non-vacuous"
        );
    }

    /// One `BookVolume`'s complete line inventory, every register
    /// flattened: `lines`, `tongue_lines`, then each chorus section's
    /// `emic`/`margin` and (when present) `doctrine.tongue_taught_line`/
    /// `emic`/`annotations`/`margin` — used only by
    /// `initiate_edition_supersets_the_committed_artifact` to compare the
    /// committed edition against the initiated one.
    fn all_committed_lines(vol: &BookVolume) -> Vec<String> {
        let mut all = Vec::new();
        all.extend(vol.lines.iter().cloned());
        all.extend(vol.tongue_lines.iter().cloned());
        for section in &vol.chorus {
            all.extend(section.emic.iter().cloned());
            all.extend(section.margin.iter().cloned());
            if let Some(doctrine) = &section.doctrine {
                all.push(doctrine.tongue_taught_line.clone());
                all.extend(doctrine.emic.iter().cloned());
                all.extend(doctrine.annotations.iter().cloned());
                all.extend(doctrine.margin.iter().cloned());
            }
        }
        all
    }

    /// C6 T4: the omniscient-reader edition (`hornvale book --initiate`'s
    /// library-level counterpart) is the committed edition PLUS the
    /// esoteric lines, and nothing else differs — a set comparison against
    /// `render_volume`'s own committed output, with the reader built from
    /// `hornvale_worldgen::chorus_ground`'s full `(subject, predicate)` key
    /// set (the "each world's full fact-set as the reader" the CLI's
    /// `--initiate` uses).
    #[test]
    fn initiate_edition_supersets_the_committed_artifact() {
        let world = generated(1);
        let vol = render_volume(&world);
        let committed = all_committed_lines(&vol);

        let reader: BTreeSet<(String, String)> = hornvale_worldgen::chorus_ground(&world)
            .into_iter()
            .map(|f| (f.subject, f.predicate))
            .collect();
        let initiated_extra = esoteric_lines(&world, &reader);
        assert!(
            initiated_extra.contains(&"— two, as the initiated count.".to_string()),
            "the moon-count RevealedClaim's initiated line should surface under the \
             omniscient reader: {:?}",
            initiated_extra
        );

        let mut initiated = committed.clone();
        initiated.extend(initiated_extra.iter().cloned());

        let committed_set: BTreeSet<&String> = committed.iter().collect();
        let initiated_set: BTreeSet<&String> = initiated.iter().collect();
        assert!(
            committed_set.is_subset(&initiated_set),
            "every committed line must survive in the initiated edition"
        );

        let extra_set: BTreeSet<&String> =
            initiated_set.difference(&committed_set).copied().collect();
        let esoteric_set: BTreeSet<&String> = initiated_extra.iter().collect();
        assert_eq!(
            extra_set, esoteric_set,
            "nothing else differs beyond the esoteric lines"
        );
    }

    /// C8 T2, the surface task: seeds 1..=3 each render exactly the two
    /// preregistered epochs; epoch 1 (day 0) is always the empty arm
    /// (T1's `observations_at_day_zero_are_empty`: every culture is
    /// `Unknown` at day 0). Epoch 2's lines are pinned exact against the
    /// live measurement (T1's report table, re-derived here through the
    /// surface): seed 1's goblin/hobgoblin both climb to `Predictive` on
    /// an identical (shared, all-solar) recurrence class; seed 2 adds
    /// kobold, whose higher sky-capability also witnesses the lunar
    /// class; seed 3's goblin alone is organized (`Predictive`) while
    /// hobgoblin/kobold are folk-only (`Counted`, no cardinal, no
    /// prediction) — the ladder's folk cap made visible in the section
    /// itself, not just T1's unit tests.
    #[test]
    fn the_reckoning_renders_the_epoch_pair() {
        for seed in [1u64, 2, 3] {
            let vol = render_volume(&generated(seed));
            assert_eq!(
                vol.reckoning.len(),
                2,
                "seed {seed}: exactly the two preregistered epochs"
            );
            assert_eq!(vol.reckoning[0].heading, "In the first days");
            assert_eq!(
                vol.reckoning[0].lines,
                vec![RECKONING_EMPTY_ARM.to_string()],
                "seed {seed}: day 0 is the empty arm (T1: every culture is Unknown at day 0)"
            );
            assert!(
                vol.reckoning[0].margin.is_empty(),
                "seed {seed}: the empty arm carries no margin"
            );
            assert_eq!(vol.reckoning[1].heading, "In the hundredth year");
        }

        let seed1 = render_volume(&generated(1));
        assert_eq!(
            seed1.reckoning[1].lines,
            vec![
                "The sky has darkened, now and again.".to_string(),
                "The priesthood of the Vavako numbers the darkenings: 4010.".to_string(),
                "The next darkening, it teaches, comes on day 36526.".to_string(),
                "The sky has darkened, now and again.".to_string(),
                "The priesthood of the Babako numbers the darkenings: 4010.".to_string(),
                "The next darkening, it teaches, comes on day 36526.".to_string(),
            ]
        );
        assert_eq!(
            seed1.reckoning[1].margin,
            vec!["In truth, the darkenings of the first hundred years number 6472.".to_string()],
            "seed 1: both cultures hold 4010 (all-solar; neither witnesses the lunar class \
             since both are below the 0.6 threshold), which falls short of the true count \
             (6472, unwitnessed lunar events included)"
        );

        let seed2 = render_volume(&generated(2));
        assert_eq!(
            seed2.reckoning[1].lines,
            vec![
                "The sky has darkened, now and again.".to_string(),
                "The priesthood of the Maetmea numbers the darkenings: 49.".to_string(),
                "The next darkening, it teaches, comes on day 36611.".to_string(),
                "The sky has darkened, now and again.".to_string(),
                "The priesthood of the Waedwea numbers the darkenings: 49.".to_string(),
                "The next darkening, it teaches, comes on day 36611.".to_string(),
                "The sky has darkened, now and again.".to_string(),
                "The priesthood of the Ngkoshngta numbers the darkenings: 81.".to_string(),
                "The next darkening, it teaches, comes on day 36611.".to_string(),
            ]
        );
        assert_eq!(
            seed2.reckoning[1].margin,
            vec!["In truth, the darkenings of the first hundred years number 81.".to_string()],
            "seed 2: kobold holds the true count (81) exactly, but goblin/hobgoblin hold only \
             49 (they cannot witness kobold's 32 lunar events) — the margin fires from their \
             shortfall even though kobold's own count is clean"
        );

        let seed3 = render_volume(&generated(3));
        assert_eq!(
            seed3.reckoning[1].lines,
            vec![
                "The sky has darkened, now and again.".to_string(),
                "The priesthood of the Sdeozqae numbers the darkenings: 32.".to_string(),
                "The next darkening, it teaches, comes on day 36953.".to_string(),
                "The sky has darkened, now and again.".to_string(),
                "The sky has darkened, now and again.".to_string(),
            ],
            "seed 3: goblin alone is organized; hobgoblin/kobold (folk-only, no doctrine) \
             render the folk line ONLY — never a cardinal"
        );
        assert_eq!(
            seed3.reckoning[1].margin,
            vec!["In truth, the darkenings of the first hundred years number 53.".to_string()],
            "seed 3: hobgoblin/kobold hold no cardinal at all (qualitative memory), so the \
             margin fires regardless of what either witnessed"
        );
    }

    /// C8 T2, the additivity law (plan Global Constraints): this campaign's
    /// only change to `BookVolume` is the additive `reckoning` field — every
    /// pre-C8 register stays byte-identical. Pinned against seed 1's
    /// current committed literals (`book/src/gallery/the-book.md`, before
    /// this task's regeneration) — the C6/C7 idiom (mirrors
    /// `folk_sections_are_byte_unchanged`), widened here to cover every
    /// pre-C8 `BookVolume` field at seed 1, not just the chorus folk
    /// registers.
    #[test]
    fn the_additivity_law() {
        let vol = render_volume(&generated(1));

        assert_eq!(
            vol.lines,
            vec![
                "Vebe is a planet with two moons, orbiting a yellow-white dwarf (F); its day \
                 lasts about 1.5 standard days."
                    .to_string(),
                "The Vavako are goblins.".to_string(),
                "The Babako are hobgoblins.".to_string(),
            ]
        );
        assert_eq!(
            vol.tongue_lines,
            vec![
                "Saa Wowe Vavako. (in the goblin tongue: \"The Vavako are goblins.\")".to_string(),
                "Saa Wovewe Vebe. (in the goblin tongue: \"Vebe is the earth.\")".to_string(),
                "Babako Babo Be Bo. (in the hobgoblin tongue: \"The Babako are hobgoblins.\")"
                    .to_string(),
                "Vebe Vebe Be Bo. (in the hobgoblin tongue: \"Vebe is the earth.\")".to_string(),
            ]
        );
        assert_eq!(
            vol.tongue_gaps,
            vec![
                "goblin: gap — planet (no entry in this lexicon)".to_string(),
                "hobgoblin: gap — planet (no entry in this lexicon)".to_string(),
            ]
        );

        let goblin = vol
            .chorus
            .iter()
            .find(|s| s.kind == "goblin")
            .expect("goblin voice");
        assert_eq!(
            goblin.emic,
            vec![
                "The Vavako are goblins — ourselves.".to_string(),
                "The Babako are hobgoblins — neighbors.".to_string(),
                "Vebe is the earth.".to_string(),
                "The day returns because the sky must be crossed.".to_string(),
            ]
        );
        assert_eq!(
            goblin.margin,
            vec![
                "In truth, Vebe is a planet with two moons, orbiting a yellow-white dwarf \
                 (F); its day lasts about 1.5 standard days."
                    .to_string()
            ]
        );
        let goblin_doctrine = goblin.doctrine.as_ref().expect("goblin is organized");
        assert_eq!(
            goblin_doctrine.heading,
            "As the priesthood of the Vavako teach it"
        );
        assert_eq!(
            goblin_doctrine.tongue_taught_line,
            "Saa Wovewe Vebe. (\"Vebe is the earth — as it is taught.\")"
        );
        assert_eq!(
            goblin_doctrine.emic,
            vec![
                "The Vavako are goblins — ourselves.".to_string(),
                "The Babako are hobgoblins — neighbors.".to_string(),
                "Vebe is the earth.".to_string(),
                "The moons are counted and known to the priesthood.".to_string(),
                "The moons cross because Soevvae strides the sky, slowly.".to_string(),
                "The day returns because Wowako strides the sky, briskly.".to_string(),
            ]
        );
        assert!(goblin_doctrine.annotations.is_empty());
        assert_eq!(
            goblin_doctrine.margin,
            vec![
                "In truth, Vebe is a planet orbiting a yellow-white dwarf (F); its day lasts \
                 about 1.5 standard days."
                    .to_string()
            ]
        );

        let hobgoblin = vol
            .chorus
            .iter()
            .find(|s| s.kind == "hobgoblin")
            .expect("hobgoblin voice");
        assert_eq!(
            hobgoblin.emic,
            vec![
                "The Vavako are goblins — rivals.".to_string(),
                "The Babako are hobgoblins — ourselves.".to_string(),
                "Vebe is the earth.".to_string(),
                "The day returns, as all things return.".to_string(),
            ]
        );
        assert_eq!(
            hobgoblin.margin,
            vec![
                "In truth, Vebe is a planet with two moons, orbiting a yellow-white dwarf \
                 (F); its day lasts about 1.5 standard days."
                    .to_string()
            ]
        );
        let hobgoblin_doctrine = hobgoblin.doctrine.as_ref().expect("hobgoblin is organized");
        assert_eq!(
            hobgoblin_doctrine.heading,
            "As the priesthood of the Babako teach it"
        );
        assert_eq!(
            hobgoblin_doctrine.tongue_taught_line,
            "Vebe Vebe Bo Bo. (\"Vebe is the earth — as it is taught.\")"
        );
        assert_eq!(
            hobgoblin_doctrine.emic,
            vec![
                "The Vavako are goblins — rivals.".to_string(),
                "The Babako are hobgoblins — ourselves.".to_string(),
                "Vebe is the earth.".to_string(),
                "The moons are counted and known to the priesthood.".to_string(),
                "The moons cross because Kdonbem strides the sky, slowly.".to_string(),
                "The day returns because Bobako strides the sky, briskly.".to_string(),
            ]
        );
        assert!(hobgoblin_doctrine.annotations.is_empty());
        assert_eq!(
            hobgoblin_doctrine.margin,
            vec![
                "In truth, Vebe is a planet orbiting a yellow-white dwarf (F); its day lasts \
                 about 1.5 standard days."
                    .to_string()
            ]
        );
    }

    /// C8 T2, the corpus law extended once more (mirrors
    /// `every_chorus_line_round_trips`): every Reckoning line + margin,
    /// across seeds 1..=3, round-trips byte-identically through
    /// `parse_chorus_line` + `rerender_chorus_line`. The epoch-1 margin
    /// phrase (`"of the first days"`) is never produced live (epoch 1 is
    /// always the empty arm at every measured seed — the true count is
    /// always zero at day 0), so it is exercised synthetically here
    /// through the SAME public round-trip pair, rather than left as
    /// vacuous coverage.
    #[test]
    fn every_reckoning_line_round_trips() {
        let mut reckoning_seen = 0usize;
        for seed in [1u64, 2, 3] {
            let world = generated(seed);
            let ctx = parse_context(&world);
            let vol = render_volume(&world);
            for epoch in &vol.reckoning {
                for line in epoch.lines.iter().chain(epoch.margin.iter()) {
                    let chorus_line = parse_chorus_line(line, &ctx).unwrap_or_else(|e| {
                        panic!(
                            "seed {seed} {}: reckoning line failed to parse: {line} ({e:?})",
                            epoch.heading
                        )
                    });
                    assert!(
                        matches!(chorus_line, ChorusLine::Reckoning(_)),
                        "seed {seed} {}: {line:?} must invert to ChorusLine::Reckoning",
                        epoch.heading
                    );
                    reckoning_seen += 1;
                    let again = rerender_chorus_line(&chorus_line);
                    assert_eq!(
                        &again, line,
                        "seed {seed} {}: re-realization drifted",
                        epoch.heading
                    );
                }
            }
        }
        assert!(
            reckoning_seen > 0,
            "the walk over seeds 1..=3 should encounter at least one Reckoning line"
        );

        // Synthetic: the epoch-1 margin phrase is unreached live (every
        // measured seed's epoch 1 is the empty arm, which carries no
        // margin) — exercised directly through the same public
        // `parse_chorus_line`/`rerender_chorus_line` pair so a regression
        // here cannot hide behind vacuous coverage.
        let ctx = ParseContext {
            complements: BTreeSet::new(),
        };
        let synthetic = "In truth, the darkenings of the first days number three.";
        let chorus_line = parse_chorus_line(synthetic, &ctx)
            .unwrap_or_else(|e| panic!("the epoch-1 margin phrase must invert: {e:?}"));
        let ChorusLine::Reckoning(reckoning) = &chorus_line else {
            panic!("the epoch-1 margin phrase must invert to ChorusLine::Reckoning");
        };
        assert_eq!(
            *reckoning,
            ReckoningLine::Margin {
                epoch_phrase: "of the first days".to_string(),
                count: 3,
            }
        );
        assert_eq!(rerender_chorus_line(&chorus_line), synthetic);
    }

    /// C8 T2: the honest omit-the-prediction arm (`Predictive` with
    /// `prediction: None` — T1's report: unreached at seeds 1..=5, since
    /// every measured Predictive culture's next event of its most-observed
    /// class falls inside the teaching horizon). Driven synthetically
    /// against the pure `reckoning_culture_lines` helper (world-free by
    /// construction, exactly so this arm doesn't need a live world that
    /// may never produce it): the priesthood still states its count, but
    /// teaches no day rather than a falsehood.
    #[test]
    fn the_prediction_line_omits_honestly_beyond_the_teaching_horizon() {
        let lines =
            reckoning_culture_lines("Vavako", hornvale_worldgen::LadderRung::Predictive, 8, None);
        assert_eq!(
            lines,
            vec![
                "The sky has darkened, now and again.".to_string(),
                "The priesthood of the Vavako numbers the darkenings: eight.".to_string(),
            ],
            "Predictive + None: the count still renders, but no prediction line"
        );

        let lines_with_prediction = reckoning_culture_lines(
            "Vavako",
            hornvale_worldgen::LadderRung::Predictive,
            8,
            Some(9080.42957840976),
        );
        assert_eq!(
            lines_with_prediction,
            vec![
                "The sky has darkened, now and again.".to_string(),
                "The priesthood of the Vavako numbers the darkenings: eight.".to_string(),
                "The next darkening, it teaches, comes on day 9080.".to_string(),
            ],
            "Predictive + Some: the prediction line renders, integer-truncated"
        );
    }

    /// C8 T2, the margin law (spec §3.4): a culture at `Predictive`/
    /// `Numbered` whose own witnessed cardinal equals the true count
    /// contributes no shortfall (seed 2's kobold: 81 == 81), while a
    /// culture whose cardinal falls short (seed 2's goblin/hobgoblin: 49 <
    /// 81) or holds no cardinal at all (any `Counted`/`Unknown` culture)
    /// always does — both arms measured in one world, so the margin's
    /// per-culture logic is pinned exactly, not just its whole-epoch
    /// effect.
    #[test]
    fn the_margin_fires_exactly_when_knowledge_falls_short() {
        let world = generated(2);
        let at = hornvale_astronomy::StdDays::new(RECKONING_EPOCH_2_DAY).unwrap();

        assert_eq!(true_event_count(&world, at), 81);

        let kobold = hornvale_worldgen::observations_of(&world, "kobold", at).unwrap();
        assert_eq!(
            kobold.events.len(),
            81,
            "kobold's own witnessed cardinal equals the true count — no shortfall from it"
        );
        let (kobold_rung, _) = hornvale_worldgen::ladder_of(&world, "kobold", at).unwrap();
        assert_eq!(kobold_rung, hornvale_worldgen::LadderRung::Predictive);

        for kind in ["goblin", "hobgoblin"] {
            let obs = hornvale_worldgen::observations_of(&world, kind, at).unwrap();
            assert_eq!(
                obs.events.len(),
                49,
                "{kind}: witnesses only the (universally-public) solar class, missing kobold's \
                 32 lunar events — a shortfall against the true count"
            );
        }

        // The epoch's own margin fires (from goblin/hobgoblin's shortfall)
        // even though kobold's individual count is clean — the whole-
        // epoch effect of the per-culture law above.
        let vol = render_volume(&world);
        assert!(!vol.reckoning[1].margin.is_empty());

        // seed 3: hobgoblin/kobold are folk-only (Counted) — no cardinal
        // held at all, so they always fall short regardless of what they
        // witnessed, even where (as kobold does here) the witnessed set
        // happens to equal the true count.
        let seed3 = generated(3);
        let at3 = hornvale_astronomy::StdDays::new(RECKONING_EPOCH_2_DAY).unwrap();
        let (kobold3_rung, _) = hornvale_worldgen::ladder_of(&seed3, "kobold", at3).unwrap();
        assert_eq!(kobold3_rung, hornvale_worldgen::LadderRung::Counted);
        let kobold3_obs = hornvale_worldgen::observations_of(&seed3, "kobold", at3).unwrap();
        assert_eq!(
            kobold3_obs.events.len(),
            true_event_count(&seed3, at3),
            "seed 3's kobold happens to witness everything (cap 1.0) but still holds no \
             cardinal (folk-only, no doctrine) — the margin's folk arm, not the count arm"
        );
        let seed3_vol = render_volume(&seed3);
        assert!(!seed3_vol.reckoning[1].margin.is_empty());
    }
}
