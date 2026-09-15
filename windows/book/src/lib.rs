//! The Book window: render a world's committed classification facts as
//! Common sentences. Reads only the ledger; realizes via `domains/language`.
//!
//! **This window states meaning; it no longer composes English** (The
//! Interlinear). It used to own an aggregation seam: `Clause.modifiers`
//! carried pre-rendered phrases, so the choice between a noun-modifier
//! ("with two moons") and a trailing independent clause ("its day lasts
//! about 1.5 standard days") — and the `"; "` join that assembled them —
//! lived here, along with a duplicated `indefinite_article`. All of that is
//! now `domains/language`'s: [`fragment_for`] returns an `Adjunct` binding a
//! predicate to an argument, and `realize_common` decides how (and where)
//! each role surfaces. A window that had to know English article selection
//! was the symptom the interlingua removed.
#![warn(missing_docs)]

use hornvale_astronomy::facts::{DAY_LENGTH_STD, MOON_COUNT, MOON_PERIOD_RATIO, STAR_CLASS};
use hornvale_kernel::{EntityId, Value, World};
use hornvale_language::CommonVocabulary;
use hornvale_language::account::{Account, AccountEntry, AccountParams, Disposition, Stance};
use hornvale_language::clause::{
    Adjunct, Argument, Clause, Definiteness, Number, ParseContext, ParseError, Person, Polarity,
    PronounCase, Subject, Tense, cardinal, common_pronoun, common_role_surface, nominative_person,
    parse_common_with_tail, quantity, realize_common,
};
use hornvale_language::numeracy::{NumeracyRung, render_quantity_at_rung};
use hornvale_language::schemas::Manner;
use hornvale_language::{
    ConflictState, Evidential, LexemeId, NounClass, SchemaId, TongueMorphology, TongueParadigm,
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
    /// epistemic account (`hornvale_worldgen::accounts_from`). The
    /// null-filter law (spec §4.1): an identity account's section
    /// reproduces `lines` byte-identically — see
    /// `identity_chorus_reproduces_the_gods_eye_lines`.
    pub chorus: Vec<ChorusSection>,
    /// C8 (The Diachronic Book): the Book's time axis — one
    /// [`ReckoningEpoch`] per preregistered epoch (day 0 and the
    /// hundredth year, `36_525.0` standard days; see `reckoning_epochs_from`),
    /// always the same fixed pair regardless of any `--at` lens the CLI
    /// renders separately ([`reckoning_at`]). Zero new draws/facts: pure
    /// derivation over `hornvale_worldgen::{observations_from, ladder_from}`.
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
    /// `hornvale_worldgen::doctrine_from`) — `None` for a folk-cult-form
    /// culture. The folk registers above (`emic`/`margin`) are
    /// byte-unchanged by this campaign regardless of this field.
    pub doctrine: Option<DoctrineSection>,
}

/// C6 (The Doctrine): one organized culture's doctrine section — the
/// institution's second account (the priesthood's own composition of the
/// SAME ground truth, run through `hornvale_worldgen::doctrine_from`'s four
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
/// `hornvale_worldgen::{observations_from, ladder_from}` for every placed
/// culture (see [`reckoning_epoch`]). `lines` is empty-arm-or-per-culture
/// (never both): the empty arm (`"The sky keeps no dates to number."`)
/// when the true event count at `T` is zero, else one run of lines per
/// placed culture at `Counted`+ (the folk line, then — organized cultures
/// only — the Numbered line, then — `Predictive` only — the prediction
/// line). `margin` carries zero or more lines: one per placed culture with
/// a live prediction crisis (`hornvale_worldgen::crisis_from`, placed-culture
/// order), then — exactly when some culture's held knowledge falls short of
/// the true count — the world-level shortfall sentence, last.
/// type-audit: bare-ok(prose: heading), bare-ok(prose: lines), bare-ok(prose: margin)
#[derive(Debug)]
pub struct ReckoningEpoch {
    /// `"In the first days"` / `"In the hundredth year"` for the committed
    /// pair; an ad hoc `"At day ⟨N⟩"` for the CLI's `--at` lens.
    pub heading: String,
    /// The empty arm, or one run of lines per placed culture (registry
    /// order) — see the struct doc.
    pub lines: Vec<String>,
    /// `margin` carries zero or more lines: one per placed culture with a
    /// live prediction crisis (`hornvale_worldgen::crisis_from`,
    /// placed-culture order), then — exactly when some culture's held
    /// knowledge falls short of the true count — the world-level shortfall
    /// sentence, last.
    pub margin: Vec<String>,
}

/// The construction table's authored predicate order: fragments join the
/// sentence in THIS order (the G3-approved surface — moons, then star, then
/// day length), not ledger commit order. Deterministic without sorting:
/// the array is fixed, and each lookup takes the subject's first committed
/// fact per predicate.
const CONSTRUCTION_ORDER: &[&str] = &[MOON_COUNT, STAR_CLASS, DAY_LENGTH_STD];

/// The construction table: maps a (predicate, object) pair to the **adjunct**
/// it contributes, or `None` if this predicate has no construction yet
/// (leaving it on [`uncovered_predicates`]'s list).
///
/// It no longer renders. How a role surfaces moved to
/// `domains/language::clause::common_role_surface` with The Interlinear — a
/// window composing English is what forced this crate to duplicate
/// `indefinite_article`, and both are gone. The ledger's own `Value` becomes
/// the language's `Argument`; the concept id travels unresolved, because
/// resolving it is the realizing language's job.
fn fragment_for(predicate: &str, object: &Value) -> Option<Adjunct> {
    match (predicate, object) {
        (MOON_COUNT, Value::Number(n)) => Some(Adjunct {
            role: MOON_COUNT.to_string(),
            argument: Argument::Count(*n as u64),
        }),
        (STAR_CLASS, Value::Text(concept)) => Some(Adjunct {
            role: STAR_CLASS.to_string(),
            argument: Argument::Concept(concept.clone()),
        }),
        (DAY_LENGTH_STD, Value::Number(days)) => Some(Adjunct {
            role: DAY_LENGTH_STD.to_string(),
            argument: Argument::Quantity(*days),
        }),
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
        // Third person; the NUMBER comes from the clause this subject lands
        // in, which is what stops a plural people re-mentioning as a
        // singular pronoun. It held the English literal `"it"` until The
        // Inquest, which could not agree with anything.
        Subject::Pronoun(Person::Third)
    }
}

/// Render a volume: one Common sentence per `is-a` fact, subject resolved to
/// its `name` (or a synthetic `Entity <id>` label when genuinely unnamed),
/// aggregating that subject's other facts into the sentence via the
/// construction table (`fragment_for`), in the table's authored order
/// ([`CONSTRUCTION_ORDER`]) — the sentence's surface order is an authored
/// grammar decision, not an echo of ledger commit order. Then one more
/// sentence per `instance-of` fact (C2 T5): a placed peopled species'
/// collective, named by its autonym. Sculpts once (`terrain_of` +
/// `climate_from`) and delegates to [`render_volume_from`], which every
/// internal reader (`lexicon_from`, `chorus_sections_from`,
/// `reckoning_epochs_from`, …) threads instead of re-sculpting the globe per
/// call — The Shuttle (this campaign): one `render_volume` call sculpted the
/// globe ~85 times before this threading, once after. On a world whose
/// committed terrain pins fail to parse (malformed save data a built world
/// cannot actually produce), renders an empty volume — the same
/// silent-empty posture `hornvale_worldgen::accounts_from` already takes on
/// the identical failure, rather than panicking on state a normal build
/// never reaches.
// Named construction site (decision 0092): this entry wrapper sculpts/fits
// once, then delegates to `render_volume_from`.
#[allow(clippy::disallowed_methods)]
pub fn render_volume(world: &World) -> BookVolume {
    let empty = || BookVolume {
        seed: world.seed.0,
        lines: Vec::new(),
        tongue_lines: Vec::new(),
        tongue_gaps: Vec::new(),
        chorus: Vec::new(),
        reckoning: Vec::new(),
    };
    let Ok(terrain) = hornvale_worldgen::terrain_of(world) else {
        return empty();
    };
    let Ok(climate) = hornvale_worldgen::climate_from(world, &terrain) else {
        return empty();
    };
    render_volume_from(world, &terrain, &climate)
}

/// [`render_volume`]'s threaded twin: takes ALREADY-BUILT terrain/climate (a
/// caller that already sculpted the globe, e.g. for another purpose)
/// instead of re-sculpting it, and otherwise renders the identical volume —
/// see [`render_volume`] for what a volume contains.
pub fn render_volume_from(
    world: &World,
    terrain: &hornvale_terrain::GeneratedTerrain,
    climate: &hornvale_climate::GeneratedClimate,
) -> BookVolume {
    // The one vocabulary this volume renders through — assembled once per
    // world at the composition root, never per clause.
    let vocab = hornvale_worldgen::common_vocabulary(&world.registry);
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

        let mut adjuncts = Vec::new();
        for predicate in CONSTRUCTION_ORDER {
            // First committed fact per (subject, predicate) — all three
            // construction predicates are functional, so "first" is "the"
            // value; still deterministic, no sorting.
            let Some(object) = world.ledger.value_of(subject_entity, predicate) else {
                continue;
            };
            if let Some(adjunct) = fragment_for(predicate, object) {
                adjuncts.push(adjunct);
            }
        }

        let subject = subject_for(subject_entity, name, &mut named);
        let line = realize_common(
            &Clause {
                predicate: hornvale_kernel::world::IS_A.to_string(),
                subject,
                object: Argument::Concept(kind.clone()),
                number: Number::Sg,
                definiteness: Definiteness::Indef,
                // The god's-eye register states the committed record itself,
                // so it is grounded the way an observation is — Witnessed.
                // Common ignores the feature either way (spec §3.2); the
                // value is stated honestly rather than left to convenience,
                // because Task 3 hands this same clause to a tongue, which
                // does read it.
                evidential: Evidential::Witnessed,
                tense: Tense::Present,
                polarity: Polarity::Pos,
                adjuncts,
            },
            &vocab,
        );
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
        // "The " (there is no per-subject determiner slot in `Clause`;
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
        // The kind is the complement CONCEPT; `Number::Pl` is what pluralizes
        // it (the realizer's job since Task 4, not the caller's).
        let line = realize_common(
            &Clause {
                predicate: hornvale_kernel::world::IS_A.to_string(),
                subject,
                object: Argument::Concept(kind.clone()),
                number: Number::Pl,
                definiteness: Definiteness::Indef,
                // Same god's-eye register as the classification loop above.
                evidential: Evidential::Witnessed,
                tense: Tense::Present,
                polarity: Polarity::Pos,
                adjuncts: Vec::new(),
            },
            &vocab,
        );
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
        let Ok(lexicon) = hornvale_worldgen::lexicon_from(world, kind, terrain, climate) else {
            continue;
        };
        let Ok(morph) = hornvale_worldgen::tongue_morphology_of(world, kind) else {
            continue;
        };
        // The Inquest T8b: the paradigm bundle, assembled beside the
        // morphology one and from the same components, so the deep realizer
        // receives a real `TongueParadigm` rather than the `None` that made
        // tense and polarity reachable only from tests. It changes no line
        // this window renders: every clause below is present-tense and
        // positive, and both of those are the ZERO member of their axis (no
        // marker is drawn for a zero member at all). Same `else { continue }`
        // posture as the two derivations above, and it can only ever fire
        // together with the morphology one — both resolve the identical
        // (kind, family, cascade) chain.
        let Ok(paradigm) = hornvale_worldgen::tongue_paradigm_of(world, kind) else {
            continue;
        };
        // The Shuttle: compute the sky-override's animacy answer ONCE per
        // kind (the same draw `noun_class_of` used to repeat per concept)
        // and hand it to `noun_class_with_sky`, the one shared copy of the
        // animacy-coherence branch (`chorus.rs`).
        let sky_animate = hornvale_worldgen::day_schema_from(world, kind, terrain, climate)
            == Some(SchemaId::Agentive);
        let noun_class_of =
            |concept: &str| hornvale_worldgen::noun_class_with_sky(sky_animate, concept);

        let own_kind = format!("{kind}-kind");
        let self_statement = Clause {
            predicate: hornvale_kernel::world::IS_A.to_string(),
            subject: Subject::Name(autonym.clone()),
            object: Argument::Concept(own_kind),
            // Unread by either tongue realizer (spec §3.2) — a clause states
            // more than any one language surfaces.
            number: Number::Sg,
            definiteness: Definiteness::Def,
            // The self-statement is a folk (self-)statement, grounded in
            // lived experience (its autonym and own-kind concept are
            // Steeped by construction) — Witnessed (C7's readout law).
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            // No role bindings on the self-statement today — this task adds
            // the capability, not new adjunct data for existing callers.
            adjuncts: Vec::new(),
        };
        let tongue_line = realize_tongue_deep(
            &self_statement,
            &grammar,
            &morph,
            // The self-statement is present-tense and positive by
            // construction, so the paradigm's markers go unread here — it is
            // supplied because the tongue HAS one, not because this clause
            // needs it (The Inquest, spec §4.1/§4.2).
            Some(&paradigm),
            &noun_class_of,
            &lexicon,
            ph.orthography,
        )
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
            &paradigm,
            &noun_class_of,
            &lexicon,
            ph.orthography,
        );
        tongue_lines.push(format!(
            "{world_line} (in the {kind} tongue: \"{name} is the earth.\")"
        ));

        for probe in &probes {
            match probe_tongue(
                probe,
                kind,
                &grammar,
                &morph,
                &paradigm,
                &noun_class_of,
                &lexicon,
                ph.orthography,
            ) {
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
        chorus: chorus_sections_from(world, terrain, climate, &vocab),
        reckoning: reckoning_epochs_from(world, terrain, climate),
    }
}

/// The autonym (committed collective `NAME`) for each placed people that
/// has one, keyed by kind label — the small ledger scan
/// [`render_volume`]'s `people_by_kind` already performs, repeated here so
/// `chorus_sections_from` stays a self-contained `fn(&World, ..) -> _` per
/// its documented signature.
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
/// `hornvale_worldgen::accounts_from` order — a people with no committed
/// collective is skipped (mirrors C3's `continue` in the tongue-lines
/// loop above). C7 T3: each organized culture's doctrine section also gets
/// its in-tongue taught line here — [`planet_name_of`] and the tongue's own
/// grammar/morphology/lexicon are (re-)derived once per doctrine-bearing
/// culture, independently of `render_volume`'s own tongue-lines loop (the
/// same "each section derives its own inputs" idiom `autonym_by_kind`
/// already follows here, rather than threading state between the two
/// unrelated `BookVolume` fields). The Shuttle: takes ALREADY-BUILT
/// terrain/climate so every placed people's account and doctrine share the
/// one sculpt [`render_volume_from`] already paid for.
fn chorus_sections_from(
    world: &World,
    terrain: &hornvale_terrain::GeneratedTerrain,
    climate: &hornvale_climate::GeneratedClimate,
    vocab: &CommonVocabulary,
) -> Vec<ChorusSection> {
    let autonyms = autonym_by_kind(world);
    let planet_name = planet_name_of(world);
    hornvale_worldgen::accounts_from(world, terrain, climate)
        .into_iter()
        .filter_map(|voice| {
            let autonym = autonyms.get(&voice.kind)?;
            let mut section = voice_section(&voice.kind, autonym, &voice.account, world, vocab);
            section.doctrine =
                hornvale_worldgen::doctrine_from(world, &voice.kind, terrain, climate).map(|dv| {
                    let kind = voice.kind.as_str();
                    let ph = hornvale_worldgen::language_of(world, kind);
                    let grammar = tongue_grammar(&world.seed, kind, &ph);
                    let lexicon = hornvale_worldgen::lexicon_from(world, kind, terrain, climate)
                    .unwrap_or_else(|e| {
                        panic!(
                            "the taught-contrast law is violated for {kind}: lexicon derivation \
                             failed: {e:?}"
                        )
                    });
                    let morph = hornvale_worldgen::tongue_morphology_of(world, kind)
                        .unwrap_or_else(|e| {
                            panic!(
                                "the taught-contrast law is violated for {kind}: morphology \
                             derivation failed: {e:?}"
                            )
                        });
                    // The Inquest T8b: the paradigm bundle, beside the
                    // morphology one. The taught world-statement is
                    // present-tense and positive, so nothing in it reads a
                    // marker; the tongue is simply handed the paradigm it has.
                    let paradigm = hornvale_worldgen::tongue_paradigm_of(world, kind)
                        .unwrap_or_else(|e| {
                            panic!(
                                "the taught-contrast law is violated for {kind}: paradigm \
                             derivation failed: {e:?}"
                            )
                        });
                    // The Shuttle: sky_animate computed once per kind (see the
                    // matching comment in `render_volume_from`).
                    let sky_animate =
                        hornvale_worldgen::day_schema_from(world, kind, terrain, climate)
                            == Some(SchemaId::Agentive);
                    let noun_class_of = |concept: &str| {
                        hornvale_worldgen::noun_class_with_sky(sky_animate, concept)
                    };
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
                        &paradigm,
                        &noun_class_of,
                        &lexicon,
                        ph.orthography,
                    );
                    let tongue_taught_line =
                        format!("{taught_line} (\"{name} is the earth — as it is taught.\")");
                    doctrine_section(
                        autonym,
                        &dv,
                        &voice.params,
                        &voice.account,
                        tongue_taught_line,
                        vocab,
                    )
                });
            Some(section)
        })
        .collect()
}

// ---------------------------------------------------------------------
// C8, The Diachronic Book: the Reckoning of Years — the Book's time axis.
// Pure derivation over `hornvale_worldgen::{observations_from, ladder_from}`
// (T1); zero new draws, facts, or save-format state. See `ReckoningEpoch`'s
// doc for the per-epoch shape and the plan's Preregistered block for every
// closed string below (frozen before measurement).
// ---------------------------------------------------------------------

/// The preregistered epoch pair (plan Global Constraints): day 0 (before
/// any culture could have witnessed a darkening) and the hundredth year —
/// `36_525.0` standard days. The committed artifact always renders exactly
/// these two, regardless of the CLI's `--at` lens ([`reckoning_at`]).
/// plumb: pending(wave-1)
const RECKONING_EPOCH_1_DAY: f64 = 0.0;
/// plumb: pending(wave-1)
const RECKONING_EPOCH_2_DAY: f64 = 36_525.0;
const RECKONING_EPOCH_1_HEADING: &str = "In the first days";
const RECKONING_EPOCH_2_HEADING: &str = "In the hundredth year";
const RECKONING_EPOCH_1_MARGIN_PHRASE: &str = "of the first days";
const RECKONING_EPOCH_2_MARGIN_PHRASE: &str = "of the first hundred years";

/// The empty arm (spec §3.4): rendered alone, with no margin, whenever the
/// true event count at `T` is zero.
const RECKONING_EMPTY_ARM: &str = "The sky keeps no dates to number.";

/// The folk register (spec §3.4, attributed per The Book Polish): rendered
/// for every placed culture at [`hornvale_worldgen::LadderRung::Counted`] or
/// above — even an organized cult's own priesthood shares this lived
/// experience before its Numbered line adds the institutional cardinal.
/// Named per culture (`"Among the {autonym}, "`) so two folk-only cultures
/// sharing one epoch never render byte-identical lines (The Book Polish,
/// 2026-07-20: two consecutive anonymous instances of this line read as a
/// duplicate to a cold reader, not two peoples).
fn reckoning_folk_counted(autonym: &str) -> String {
    format!("Among the {autonym}, the sky has darkened, now and again.")
}

/// [`reckoning_folk_counted`]'s closed inverse: strip the fixed prefix and
/// suffix, recovering the autonym — `None` if `line` doesn't match the
/// shape at all (mirrors [`parse_reckoning_line`]'s `Numbered` arm).
fn parse_reckoning_folk_counted(line: &str) -> Option<&str> {
    line.strip_prefix("Among the ")?
        .strip_suffix(", the sky has darkened, now and again.")
}

/// Every placed culture's Reckoning-of-Years read, at the two
/// preregistered epochs — the committed `BookVolume::reckoning`. Always
/// exactly two entries; never gated on world content (an eventless world
/// renders the empty arm at both). The Shuttle: takes ALREADY-BUILT
/// terrain/climate (threaded down to [`observations_from`]/[`ladder_from`]
/// via `reckoning_epoch`) instead of re-sculpting the globe per epoch per
/// culture.
fn reckoning_epochs_from(
    world: &World,
    terrain: &hornvale_terrain::GeneratedTerrain,
    climate: &hornvale_climate::GeneratedClimate,
) -> Vec<ReckoningEpoch> {
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
        let at = hornvale_astronomy::StdInstant::new(day).unwrap_or_else(|e| {
            panic!("the Reckoning's preregistered epoch day {day} must be a valid StdDays: {e}")
        });
        reckoning_epoch(
            world,
            &autonyms,
            heading,
            at,
            margin_phrase,
            terrain,
            climate,
        )
    })
    .collect()
}

/// The single-epoch reckoning accessor: a Reckoning-of-Years epoch at any
/// arbitrary day, exposed so a caller outside this crate can read the
/// Book's own time axis. Originally the CLI's `--at <day>` lens (C8 T2:
/// rendered to stdout only, never part of the committed
/// `BookVolume::reckoning`, which always uses the two preregistered
/// epochs above regardless of this function ever being called —
/// `scripts/regenerate-artifacts.sh` never passes `--at`); The Vessel
/// Stitch T1 promotes it to the crate's public accessor and cuts the CLI
/// path over to calling it directly, so it is now this function's own
/// implementation the CLI renders rather than a parallel one — the
/// accessor law (spec §4.4), checked by
/// `reckoning_at_matches_the_fixed_pair_and_renders_arbitrary_days`. The
/// possessed session's `consult` verb (`windows/vessel`, T2) is the
/// second caller: it reads the Reckoning at the session's own day through
/// this same function. Sculpts once (`terrain_of`+`climate_from`) and
/// delegates to [`reckoning_at_from`] — a build failure on a normal world's
/// committed pins is unreachable, so this panics on it (the same posture
/// `reckoning_epoch`'s own `observations_from`/`ladder_from` calls already took
/// before The Shuttle threaded them).
// Named construction site (decision 0092): this entry wrapper sculpts/fits
// once, then delegates to `reckoning_at_from`.
#[allow(clippy::disallowed_methods)]
pub fn reckoning_at(world: &World, at: hornvale_astronomy::StdInstant) -> ReckoningEpoch {
    let terrain = hornvale_worldgen::terrain_of(world)
        .unwrap_or_else(|e| panic!("the Reckoning section requires a derivable terrain: {e}"));
    let climate = hornvale_worldgen::climate_from(world, &terrain)
        .unwrap_or_else(|e| panic!("the Reckoning section requires a derivable climate: {e}"));
    reckoning_at_from(world, at, &terrain, &climate)
}

/// [`reckoning_at`], threaded: takes ALREADY-BUILT terrain/climate instead
/// of re-sculpting the globe — the CLI/vessel callers of `reckoning_at`
/// each sculpt once per call; a caller that already holds a build (or
/// wants many `--at` lenses over one world) can share it here instead.
pub fn reckoning_at_from(
    world: &World,
    at: hornvale_astronomy::StdInstant,
    terrain: &hornvale_terrain::GeneratedTerrain,
    climate: &hornvale_climate::GeneratedClimate,
) -> ReckoningEpoch {
    let day = at.get();
    let heading = format!("At day {day}");
    let margin_phrase = format!("by day {day}");
    reckoning_epoch(
        world,
        &autonym_by_kind(world),
        &heading,
        at,
        &margin_phrase,
        terrain,
        climate,
    )
}

/// The true count of eclipse events to `at` (spec §3.4): every syzygy in
/// `[0, at]`, solar AND lunar, regardless of any culture's witnessing
/// capability — the world's own physical record, as opposed to
/// [`hornvale_worldgen::observations_from`]'s per-culture WITNESSED subset.
/// The margin law compares each culture's held count against this. Every
/// built world has the calendar and star system needed to derive the full
/// physical record.
fn true_event_count(world: &World, at: hornvale_astronomy::StdInstant) -> usize {
    let sky = hornvale_worldgen::sky_of(world)
        .unwrap_or_else(|e| panic!("the Reckoning section requires a derivable sky: {e}"));
    let from = hornvale_astronomy::StdInstant::new(0.0).expect("0.0 is always a valid StdInstant");
    hornvale_astronomy::eclipse_events(
        sky.generated().system(),
        sky.generated().calendar(),
        from,
        at,
    )
    .len()
}

/// Whether one placed culture's held knowledge falls short of the true
/// count (spec §3.4's margin rule), extracted pure and world-free (mirrors
/// [`reckoning_culture_lines`] below) so the per-culture logic is testable
/// without depending on a live world that happens to exhibit both arms:
/// a folk-only/sub-`Numbered` culture holds no cardinal at all, so it
/// always falls short regardless of `held`; an organized
/// (`Numbered`/`Predictive`) culture falls short exactly when its own
/// witnessed cardinal is less than the true count (it cannot witness what
/// its sky-capability gates out) — an organized culture whose `held`
/// equals `true_count` does not fall short on its own account.
fn culture_falls_short(rung: hornvale_worldgen::LadderRung, held: u64, true_count: u64) -> bool {
    let organized = matches!(
        rung,
        hornvale_worldgen::LadderRung::Numbered | hornvale_worldgen::LadderRung::Predictive
    );
    !organized || held < true_count
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
    at: hornvale_astronomy::StdInstant,
    margin_phrase: &str,
    terrain: &hornvale_terrain::GeneratedTerrain,
    climate: &hornvale_climate::GeneratedClimate,
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
    let mut margin = Vec::new();
    let mut falls_short = false;
    for (kind, _village) in hornvale_worldgen::placed_peoples(world) {
        let Some(autonym) = autonyms.get(kind) else {
            continue;
        };
        let observations = hornvale_worldgen::observations_from(world, kind, at, terrain, climate)
            .unwrap_or_else(|e| {
                panic!(
                    "the Reckoning section requires observations_from to succeed for placed \
                     culture {kind}: {e}"
                )
            });
        let (rung, prediction) = hornvale_worldgen::ladder_from(world, kind, at, terrain, climate)
            .unwrap_or_else(|e| {
                panic!(
                    "the Reckoning section requires ladder_from to succeed for placed culture \
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

        if culture_falls_short(rung, held, true_count as u64) {
            falls_short = true;
        }

        if rung == hornvale_worldgen::LadderRung::Predictive {
            let crisis = hornvale_worldgen::crisis_from(world, kind, at, terrain, climate)
                .unwrap_or_else(|e| {
                    panic!(
                        "the Reckoning section requires crisis_from to succeed for placed \
                         culture {kind}: {e}"
                    )
                });
            if let Some(crisis) = crisis {
                margin.push(reckoning_crisis_margin_line(autonym, crisis));
            }

            if let Some(doctrine_line) = reckoning_doctrine_line(autonym, true, crisis.is_some()) {
                lines.push(doctrine_line);
            }
        }
    }

    if falls_short {
        margin.push(format!(
            "In truth, the darkenings {margin_phrase} number {}.",
            cardinal(true_count as u64)
        ));
    }

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
    let mut lines = vec![reckoning_folk_counted(autonym)];
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

/// One placed culture's Reckoning margin line for a live prediction
/// crisis (spec Task 1's margin extension) -- pure and world-free, mirrors
/// [`reckoning_culture_lines`].
fn reckoning_crisis_margin_line(
    autonym: &str,
    crisis: hornvale_worldgen::PredictionCrisis,
) -> String {
    format!(
        "In truth, the {autonym}'s priesthood taught the darkening would come on day {}; it \
         came on day {} instead.",
        crisis.last_predicted.trunc() as u64,
        crisis.last_actual.trunc() as u64
    )
}

/// One further Reckoning line for an organized culture at `Predictive`
/// rung (spec Task 2), thematically echoing LANG-39's own "Galileo cell"
/// framing WITHOUT routing through `ConflictState`/`conflict_of` (decision
/// ledger #3) -- `None` for a folk-only culture, matching every other
/// doctrine-gated render path's existing convention. `has_doctrine` is
/// always `true` at every real call site in this file today (a
/// `Predictive`-rung culture always has a doctrine, by `ladder_from`'s own
/// gate), so the `false` arm is unreachable through any live world -- kept
/// and tested anyway (see the tests above), the same defensive posture
/// `reckoning_culture_lines`'s own `Unknown` arm already keeps.
fn reckoning_doctrine_line(autonym: &str, has_doctrine: bool, crisis_live: bool) -> Option<String> {
    if !has_doctrine {
        return None;
    }
    Some(if crisis_live {
        format!(
            "The {autonym}'s own priesthood taught wrongly, and could be shown wrong by any \
             who kept their own count."
        )
    } else {
        format!("None among the {autonym} have shown the priesthood's teaching false.")
    })
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
/// instead. `key` is the raw ground-truth name (e.g. `"Veewe"`, never
/// "The Veewe"), so a people subject's `"The "` prefix never leaks into
/// the re-mention check; `display` is the surface text used on first
/// mention.
fn subject_for_text(key: &str, display: String, seen: &mut BTreeSet<String>) -> Subject {
    if seen.insert(key.to_string()) {
        Subject::Name(display)
    } else {
        Subject::Pronoun(Person::Third)
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
    vocab: &CommonVocabulary,
) -> Option<String> {
    let (complement_concept, definiteness) = match effective(&is_a_entry.disposition) {
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

    let mut adjuncts = Vec::new();
    for entry in group {
        if entry.fact.predicate == hornvale_kernel::world::IS_A {
            continue;
        }
        if !matches!(effective(&entry.disposition), Disposition::Kept) {
            continue;
        }
        if let Some(adjunct) = fragment_for(&entry.fact.predicate, &entry.fact.object) {
            adjuncts.push(adjunct);
        }
    }

    let name = is_a_entry.fact.subject.clone();
    let subject = subject_for_text(&name, name.clone(), seen);
    Some(realize_common(
        &Clause {
            predicate: hornvale_kernel::world::IS_A.to_string(),
            subject,
            object: Argument::Concept(complement_concept),
            number: Number::Sg,
            definiteness,
            // The emic account is what this people HOLDS about the world,
            // including a classification their own world-carving recast
            // (`Disposition::Substituted`) — a carving is how a culture
            // sees, not what it was told, so the whole register is
            // Witnessed. The doctrinal register that is genuinely `Taught`
            // is the separate `doctrine_section` path, which already says
            // so at its own `world_statement` call.
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts,
        },
        vocab,
    ))
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
fn render_world_margin(
    group: &[&AccountEntry],
    is_a_entry: &AccountEntry,
    vocab: &CommonVocabulary,
) -> Option<String> {
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
    let mut adjuncts = Vec::new();
    for entry in lost_fragments {
        if let Some(adjunct) = fragment_for(&entry.fact.predicate, &entry.fact.object) {
            adjuncts.push(adjunct);
        }
    }
    let line = realize_common(
        &Clause {
            predicate: hornvale_kernel::world::IS_A.to_string(),
            subject: Subject::Name(is_a_entry.fact.subject.clone()),
            object: Argument::Concept(truth_kind.clone()),
            number: Number::Sg,
            definiteness: Definiteness::Indef,
            // The etic margin reads the ground fact's own object text, so
            // it is the record speaking: Witnessed, like the god's-eye
            // register it restores.
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts,
        },
        vocab,
    );
    Some(format!("In truth, {line}"))
}

/// Task 4 (C5): the count-aware head clause an explanation line opens
/// with — `"The day returns"` for the day; `"The moon crosses"` /
/// `"The moons cross"` for the moons, singular/plural read off the
/// `moon-count` fact's own ground value (always numeric; the moons entry is
/// only ever wrapped in [`Disposition::Explained`] while `Kept`, so this is
/// the true committed count, never a substitution); `"The moons keep their
/// measure"` for the LANG-48 period ratio (the-consonance × C5 merge —
/// `explain_moon_ratio` binds the same causal schemas the moons entry does,
/// so a `Kept` ratio the folk explain needs its own head or its because-
/// clause vanishes AND the doctrine Contested counter-annotation that
/// quotes it has nothing to say). A proportion is inherently between two
/// moons, so it is always plural (the object — the measured ratio — is not
/// inspected: the head states THAT the moons keep a measure, never the
/// value). `None` for any other predicate — `explain` in
/// `windows/worldgen::chorus` only ever wraps `day-length-std`,
/// `moon-count`, and `moon-period-ratio`.
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
    if predicate == MOON_PERIOD_RATIO {
        return Some(("The moons keep their measure".to_string(), true));
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
/// (the kind concept, plural, indefinite), plus the stance appositive at
/// the book layer (`" — {stance}."`, replacing the terminal `.`) — absent
/// for `Neutral` (the identity case, byte-matching the god's-eye line).
/// Returns `None` if this subject's `instance-of` entry is not `Kept` (see
/// [`render_world_margin`]'s carrier-clause note: never exercised at the
/// floor).
fn render_people_clause(
    io_entry: &AccountEntry,
    seen: &mut BTreeSet<String>,
    vocab: &CommonVocabulary,
) -> Option<String> {
    if !matches!(effective(&io_entry.disposition), Disposition::Kept) {
        return None;
    }
    let Value::Text(kind_text) = &io_entry.fact.object else {
        return None;
    };
    let raw_name = io_entry.fact.subject.clone();
    let display = format!("The {raw_name}");
    let subject = subject_for_text(&raw_name, display, seen);
    let mut line = realize_common(
        &Clause {
            predicate: hornvale_kernel::world::IS_A.to_string(),
            subject,
            object: Argument::Concept(kind_text.clone()),
            number: Number::Pl,
            definiteness: Definiteness::Indef,
            // A people subject's collective classification, in the same
            // emic register as `render_world_clause`.
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: Vec::new(),
        },
        vocab,
    );
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
fn voice_section(
    kind: &str,
    autonym: &str,
    account: &Account,
    _world: &World,
    vocab: &CommonVocabulary,
) -> ChorusSection {
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
            if let Some(line) = render_world_clause(group, is_a_entry, &mut seen, vocab) {
                emic.push(line);
            }
            // Task 4 (C5): the because-clause explanations for this
            // subject's day/moons entries, as additional emic lines —
            // appended right after the world clause, before the margin.
            emic.extend(render_explanations(group));
            if let Some(line) = render_world_margin(group, is_a_entry, vocab) {
                margin.push(line);
            }
        } else if let Some(io_entry) = group
            .iter()
            .find(|e| e.fact.predicate == hornvale_kernel::INSTANCE_OF)
            && let Some(line) = render_people_clause(io_entry, &mut seen, vocab)
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

/// Which closed [`revealed_claim_line`] surface a doctrine emic line used —
/// the round-trip recovery key ([`parse_revealed_claim`] reads it off the
/// exact text, [`rerender_revealed_claim`] inverts it). [`MOON_COUNT`]
/// carries a plurality (its count's own singular/plural agreement);
/// [`MOON_PERIOD_RATIO`] is a single invariant surface — a proportion
/// between two moons' cycles carries no count to agree in number with, so
/// its exoteric formula never varies. Was a bare `plural: bool` through C6
/// (the-consonance / the-living-community merge generalized it once a
/// SECOND predicate — the LANG-48 period ratio — began reaching
/// `RevealedClaim`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RevealedClaimSurface {
    /// The [`MOON_COUNT`] formula's singular surface ("The moon is
    /// counted and known to the priesthood.").
    MoonCountSingular,
    /// The [`MOON_COUNT`] formula's plural surface ("The moons are
    /// counted and known to the priesthood.").
    MoonCountPlural,
    /// The [`MOON_PERIOD_RATIO`] formula — a single invariant proportion
    /// surface, no plurality.
    MoonPeriodRatio,
}

/// C6 (The Doctrine), the exoteric formula (the plan's Surfaces table):
/// what a `RevealedClaim` entry's doctrine emic line asserts INSTEAD of the
/// ordinary construction fragment — the priesthood professes reckoned
/// knowledge (a count, or a proportion) without disclosing the value itself
/// (the esoteric/exoteric split, ledger #5 — the disclosed value lives in
/// [`esoteric_lines`], gated behind an initiated reader). Two predicates
/// carry a defined formula, both [`hornvale_language::account::Requirement::SkyGraded`]
/// (the only requirement class that can leave a fact doctrine-only `Kept`
/// while the folk lose it): [`MOON_COUNT`] (a count → singular/plural
/// surface) and [`MOON_PERIOD_RATIO`] (LANG-48, a proportion → one
/// invariant surface). `day-length-std` is `CrossReferential`, always lost
/// regardless of capability, so it can never become a doctrine-only `Kept`
/// — `None` for it and any other predicate.
fn revealed_claim_line(predicate: &str, object: &Value) -> Option<String> {
    match predicate {
        MOON_COUNT => {
            let Value::Number(n) = object else {
                return None;
            };
            Some(rerender_revealed_claim(if (*n as u64) == 1 {
                RevealedClaimSurface::MoonCountSingular
            } else {
                RevealedClaimSurface::MoonCountPlural
            }))
        }
        // The proportion never discloses its value: the exoteric surface is
        // invariant, so the object is not inspected here (the actual ratio
        // is the initiated's alone — `esoteric_lines`).
        MOON_PERIOD_RATIO => Some(rerender_revealed_claim(
            RevealedClaimSurface::MoonPeriodRatio,
        )),
        _ => None,
    }
}

/// [`revealed_claim_line`]'s closed inverse table — the only strings that
/// construction direction ever emits, paired with the [`RevealedClaimSurface`]
/// each carries. The single source of truth for every closed exoteric
/// surface (both directions read it, and `revealed_claim_line` itself emits
/// through it).
const REVEALED_CLAIM_LINES: &[(&str, RevealedClaimSurface)] = &[
    (
        "The moon is counted and known to the priesthood.",
        RevealedClaimSurface::MoonCountSingular,
    ),
    (
        "The moons are counted and known to the priesthood.",
        RevealedClaimSurface::MoonCountPlural,
    ),
    (
        "The proportion between the moons' cycles is measured and known to the priesthood.",
        RevealedClaimSurface::MoonPeriodRatio,
    ),
];

/// Recover a `RevealedClaim` line's [`RevealedClaimSurface`] from its exact
/// closed text, or `None` if `line` matches no row.
fn parse_revealed_claim(line: &str) -> Option<RevealedClaimSurface> {
    REVEALED_CLAIM_LINES
        .iter()
        .find(|(text, _)| *text == line)
        .map(|(_, surface)| *surface)
}

/// [`parse_revealed_claim`]'s inverse: the closed text for `surface`. Total
/// over [`RevealedClaimSurface`] (the table carries exactly one row per
/// surface).
fn rerender_revealed_claim(surface: RevealedClaimSurface) -> String {
    REVEALED_CLAIM_LINES
        .iter()
        .find(|(_, s)| *s == surface)
        .map(|(text, _)| (*text).to_string())
        .expect("REVEALED_CLAIM_LINES carries a row for every surface")
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
    vocab: &CommonVocabulary,
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
            if let Some(line) = render_world_clause(&filtered, is_a_entry, &mut seen, vocab) {
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
            if let Some(line) = render_world_margin(group, is_a_entry, vocab) {
                margin.push(line);
            }
        } else if let Some(io_entry) = group
            .iter()
            .find(|e| e.fact.predicate == hornvale_kernel::INSTANCE_OF)
            && let Some(line) = render_people_clause(io_entry, &mut seen, vocab)
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
/// a `RevealedClaim` for (moon-count and moon-period-ratio today, both
/// world-level sky facts) is asserted on an `is-a`-classified subject, the
/// same scope `render_volume`'s construction table reads.
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
/// whose key is in `reader`, emits one initiated line — the count's
/// cardinal (`"⟨subject⟩ has ⟨cardinal⟩ moons, as the initiated count."`)
/// or the ratio's measured proportion (`"The cycles of ⟨subject⟩'s moons
/// keep a proportion of ⟨quantity⟩ to one, as the initiated measure."`),
/// one surface per `RevealedClaim`-reachable predicate — with the value
/// read from the LEDGER's own committed fact for that subject/predicate,
/// never the account entry's cached copy, so the line's number can only
/// ever trace back to the one committed truth (the mutation-verified law:
/// `the_esoteric_law_mutation_verified` drives this directly). An empty
/// `reader` yields an empty `Vec` — the committed exoteric edition
/// discloses nothing.
///
/// More than one organized culture can independently reveal the SAME
/// ground fact (the ground truth is world-global, `chorus_ground`, run
/// through each culture's own doctrine params) — deduplicated by key, so
/// the reader sees each revealed fact's initiated line exactly once
/// regardless of how many priesthoods reveal it.
///
/// A key whose entity cannot be resolved back to an `is-a` subject, whose
/// ledger value is not [`Value::Number`], or whose predicate carries no
/// disclosure arm here, is silently skipped: every [`RevealedClaim`]
/// predicate reaching this function today is a world-level sky scalar (a
/// count or a proportion — see [`revealed_claim_line`]'s doc), and this
/// function's own reach is the reader's disclosure surface, not the
/// doctrine emic's — the vanishing-realizable panic ([`doctrine_section`])
/// already guards the emic's own formula table.
///
/// [`RevealedClaim`]: ConflictState::RevealedClaim
/// type-audit: bare-ok(identifier-text: reader), bare-ok(prose: return)
// Named construction site (decision 0092): this entry wrapper sculpts/fits
// once, then delegates to `esoteric_lines_from`.
#[allow(clippy::disallowed_methods)]
pub fn esoteric_lines(world: &World, reader: &BTreeSet<(String, String)>) -> Vec<String> {
    let Ok(terrain) = hornvale_worldgen::terrain_of(world) else {
        return Vec::new();
    };
    let Ok(climate) = hornvale_worldgen::climate_from(world, &terrain) else {
        return Vec::new();
    };
    esoteric_lines_from(world, reader, &terrain, &climate)
}

/// [`esoteric_lines`], threaded: takes ALREADY-BUILT terrain/climate instead
/// of re-sculpting the globe. On a sculpt failure, [`esoteric_lines`] mirrors
/// `hornvale_worldgen::accounts_from`'s own posture on the same failure — an
/// empty `Vec`, never a panic.
/// type-audit: bare-ok(identifier-text: reader), bare-ok(prose: return)
pub fn esoteric_lines_from(
    world: &World,
    reader: &BTreeSet<(String, String)>,
    terrain: &hornvale_terrain::GeneratedTerrain,
    climate: &hornvale_climate::GeneratedClimate,
) -> Vec<String> {
    let mut lines = Vec::new();
    let mut seen: BTreeSet<(String, String)> = BTreeSet::new();
    for voice in hornvale_worldgen::accounts_from(world, terrain, climate) {
        let Some(doctrine) = hornvale_worldgen::doctrine_from(world, &voice.kind, terrain, climate)
        else {
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
            // The Book Polish (2026-07-20): name the subject and agree in
            // number, mirroring `revealed_claim_line`'s own surface split —
            // the bare em-dash continuation this replaced read fine inside
            // the vessel's turn-by-turn flow (the player just wrote about
            // the moons) but floated with no antecedent in the CLI's static
            // `--initiate` dump, which has no surrounding context at all.
            // One disclosure surface per `RevealedClaim`-reachable predicate
            // (the-living-community merge added the LANG-48 ratio arm): a
            // count discloses its cardinal, a proportion its measured value
            // (through `quantity`, the book's own float-prose helper — the
            // exoteric formula withheld the value, this is the initiated's
            // alone). A predicate with no arm here is silently skipped: the
            // emic's own vanishing-realizable panic (`doctrine_section`)
            // already fails loud for an un-authored surface.
            let line = match predicate.as_str() {
                MOON_COUNT => {
                    let count = *n as u64;
                    let moon_word = if count == 1 { "moon" } else { "moons" };
                    format!(
                        "{} has {} {moon_word}, as the initiated count.",
                        entry.fact.subject,
                        cardinal(count)
                    )
                }
                MOON_PERIOD_RATIO => format!(
                    "The cycles of {}'s moons keep a proportion of {} to one, \
                     as the initiated measure.",
                    entry.fact.subject,
                    quantity(*n)
                ),
                _ => continue,
            };
            lines.push(line);
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
#[allow(clippy::too_many_arguments)]
fn probe_tongue(
    probe: &TongueProbe,
    _kind: &str,
    grammar: &hornvale_language::TongueGrammar,
    morph: &TongueMorphology,
    paradigm: &TongueParadigm,
    noun_class_of: &dyn Fn(&str) -> NounClass,
    lexicon: &hornvale_language::Lexicon,
    orth: hornvale_language::Orthography,
) -> Result<String, hornvale_language::TongueGap> {
    realize_tongue_deep(
        &Clause {
            predicate: hornvale_kernel::world::IS_A.to_string(),
            subject: Subject::Name(probe.subject.clone()),
            object: Argument::Concept(probe.concept.clone()),
            // Unread by either tongue realizer (spec §3.2).
            number: Number::Sg,
            definiteness: Definiteness::Def,
            // Every probe states a claim grounded in the same
            // lived-experience footing as the self-statement above.
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            // No role bindings on a C3 probe today.
            adjuncts: Vec::new(),
        },
        grammar,
        morph,
        // Every probe is present-tense and positive (spec §4.1/§4.2), so the
        // paradigm's markers go unread — it is passed because the tongue has
        // one, not because this clause asks for one.
        Some(paradigm),
        noun_class_of,
        lexicon,
        orth,
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

/// C7 T3: build one tongue's emic world-statement — a `Clause` whose
/// subject is `Subject::Name(planet_name)`, whose object is
/// `Argument::Concept("earth")`, carrying the given `evidential` — through the
/// deep realizer, using that tongue's own already-derived grammar/morphology/
/// lexicon (never re-derived here; callers pass what they already hold, the
/// same "measure once" discipline `render_volume`'s loop and
/// `chorus_sections` both follow). `earth` is universal-stratum Steeped
/// (`packs.rs`, `ladder_rank: 0` — always in the lexicon), so this must never
/// gap for a placed people; panics loudly naming the culprit rather than
/// returning a `Result`, since a gap here is an invariant violation, not a
/// coverage fact to record (contrast the C3 planet-concept probe, which
/// gaps by design).
#[allow(clippy::too_many_arguments)]
fn world_statement(
    kind: &str,
    planet_name: &str,
    evidential: Evidential,
    grammar: &hornvale_language::TongueGrammar,
    morph: &TongueMorphology,
    paradigm: &TongueParadigm,
    noun_class_of: &dyn Fn(&str) -> NounClass,
    lexicon: &hornvale_language::Lexicon,
    orth: hornvale_language::Orthography,
) -> String {
    let clause = Clause {
        predicate: hornvale_kernel::world::IS_A.to_string(),
        subject: Subject::Name(planet_name.to_string()),
        object: Argument::Concept("earth".to_string()),
        // Unread by either tongue realizer (spec §3.2).
        number: Number::Sg,
        definiteness: Definiteness::Def,
        evidential,
        tense: Tense::Present,
        polarity: Polarity::Pos,
        // No role bindings on the world-statement today.
        adjuncts: Vec::new(),
    };
    // The world-statement is present-tense and positive (spec §4.1/§4.2), so
    // the paradigm's markers go unread — it is passed because the tongue has
    // one, not because this clause asks for one.
    realize_tongue_deep(
        &clause,
        grammar,
        morph,
        Some(paradigm),
        noun_class_of,
        lexicon,
        orth,
    )
    .unwrap_or_else(|gap| {
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
    tense: Tense,
    polarity: Polarity,
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
/// `domains/language::clause`'s): word (`"two"`) or digits (`"13"`) to the
/// count.
///
/// **It survives a duplication its own precedent did not.** The doc here
/// used to cite this module's `indefinite_article` as the established case
/// for keeping a small presentation-layer table on the book side of the
/// aggregation seam. The Interlinear deleted that function, and the seam
/// with it: article selection was never a book concern, it was English
/// leaking into a window because `Clause` could not carry structure.
/// This table stays for a different and narrower reason — it runs
/// BACKWARD, and the direction is the whole argument. [`fact_for`] must
/// recognize text the program did not generate (it backs
/// `windows/vessel`'s spoken-to-heard seam), and recognition is a later
/// campaign's subject. When Common learns to recognize its own role
/// constructions, this goes with `fact_for`.
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

/// The construction table run backward: recover (predicate, surface value)
/// from one fragment's TEXT. Returns `None` for text it does not recognize,
/// which [`parse_line`] treats as `LineError::UnknownFragment`.
///
/// **No longer the mirror of [`fragment_for`], and the difference is the
/// point.** Since The Interlinear the forward direction produces an
/// `Adjunct` — structure — while this one still consumes English, so the
/// two no longer share a currency: what inverts this function's output is
/// `fragment_for` followed by `common_role_surface`. It stays an English
/// recognizer deliberately, because [`parse_line`] backs
/// `windows/vessel`'s spoken-to-heard seam, which parses text the program
/// did not generate. Recognition is a later campaign's subject (The
/// Interlinear's spec §6 freezes its coverage here).
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
        let display = rest
            .strip_prefix("an ")
            .or_else(|| rest.strip_prefix("a "))?;
        let concept = hornvale_astronomy::class_concept(display)?;
        return Some((STAR_CLASS.to_string(), Value::Text(concept.to_string())));
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

/// The public face of the private [`fact_for`], exported so
/// `cli/tests/star_class_is_a_concept.rs` can assert render and parse are
/// inverse. Do not make `fact_for` itself public — its privacy is what keeps
/// the construction table a Book concern.
/// type-audit: bare-ok(prose: fragment), bare-ok(identifier-text: return)
pub fn fact_for_public(fragment: &str) -> Option<(String, Value)> {
    fact_for(fragment)
}

/// The realized surface of the adjunct [`fragment_for`] contributes,
/// exported so `cli/tests/star_class_is_a_concept.rs` can drive its
/// round-trip assertion from the actual renderer rather than a hand-rolled
/// fragment — a hand-rolled `"orbiting a {display}"` never exercises which
/// article the realizer actually chooses, so a wrong-article regression
/// would go undetected. Since The Interlinear the article is Common's
/// choice, not this window's, so this composes the two halves:
/// `fragment_for` states the role, `common_role_surface` renders it. The
/// inline/trailing position is a language's decision the caller has no need
/// of. Do not make `fragment_for` itself public — its privacy is what keeps
/// the construction table a Book concern.
/// type-audit: bare-ok(identifier-text: predicate), bare-ok(prose: return)
pub fn fragment_for_public(
    predicate: &str,
    object: &Value,
    vocab: &CommonVocabulary,
) -> Option<String> {
    let adjunct = fragment_for(predicate, object)?;
    common_role_surface(&adjunct, vocab).map(|(_, text)| text)
}

/// Apply a listener's numeracy rung to a heard quantity fragment (LANG-44
/// spec §3.4): recover the fragment's stated surface value exactly as
/// [`fact_for`] already does, then re-render it at `listener_rung` via
/// [`hornvale_language::numeracy::render_quantity_at_rung`] — the
/// listener retains only what their own rung can express, never more
/// than the speaker actually said. Every speaker today renders at the
/// ceiling rung (`NumeracyRung::Decimals`), so this is the collapsed
/// `min(Decimals, listener_rung) == listener_rung` special case (spec
/// §3.3) — full bidirectional rung variation is a follow-up. Returns
/// `None` for a fragment [`fact_for`] itself would not recognize, or
/// whose recovered value is not a number (e.g. a star class). Reserved
/// integration seam: LANG-44's demonstrated `windows/book` entry point,
/// exercised by this crate's tests; the live `write`/`consult` wiring
/// that will call it in production is the deferred follow-up (spec §6).
/// Present in all builds so that seam is real, not test-only.
#[allow(dead_code)]
fn comprehend_quantity(fragment: &str, listener_rung: NumeracyRung) -> Option<String> {
    let (_, value) = fact_for(fragment)?;
    match value {
        Value::Number(x) => Some(render_quantity_at_rung(x, listener_rung)),
        _ => None,
    }
}

/// The closed complement set a `parse_line` call recognizes for `world`:
/// every committed `is-a` object label, plus every committed `instance-of`
/// object (the only source of a plural complement in this campaign's
/// grammar — see [`parse_line`]'s doc for why that lets `Number::Pl` alone
/// signal a collective on the way back), plus (C4 T4) every chorus account's
/// `Substituted` target (e.g.
/// `"earth"`) — a book-layer carving that never appears as a committed
/// `is-a` object, so a chorus emic line naming it would otherwise parse as
/// `UnknownComplement`. The closed set stays derived from the world:
/// walking `accounts_from(world)` rather than hardcoding the carving text.
/// Sculpts once (`terrain_of` + `climate_from`) and delegates to
/// [`parse_context_from`] on success — the vessel session's `write` verb
/// (The Shuttle) threads an already-built terrain/climate instead of
/// paying for this sculpt on every turn. On a sculpt failure, mirrors
/// `hornvale_worldgen::accounts_from`'s own posture: only the chorus-derived
/// complements are absent (an empty voice list), never the whole set — the
/// two pure-ledger loops (`is-a`, `instance-of`) still run, so a world
/// whose terrain pins fail to parse still parses every Common line it
/// would have accepted before that failure, just without the chorus
/// vocabulary.
// Named construction site (decision 0092): this entry wrapper sculpts/fits
// once, then delegates to `parse_context_with_voices`.
#[allow(clippy::disallowed_methods)]
pub fn parse_context(world: &World) -> ParseContext {
    let sculpted = hornvale_worldgen::terrain_of(world)
        .ok()
        .and_then(|terrain| {
            hornvale_worldgen::climate_from(world, &terrain)
                .ok()
                .map(|climate| (terrain, climate))
        });
    let voices = match &sculpted {
        Some((terrain, climate)) => hornvale_worldgen::accounts_from(world, terrain, climate),
        None => Vec::new(),
    };
    parse_context_with_voices(world, voices)
}

/// [`parse_context`], threaded: takes ALREADY-BUILT terrain/climate instead of
/// re-sculpting the globe. Success path only — a caller already holding a
/// terrain/climate pair has, by construction, a sculpt that succeeded, so
/// this never takes the degraded (empty-voices) arm [`parse_context`]
/// falls back to on its own internal sculpt failure.
pub fn parse_context_from(
    world: &World,
    terrain: &hornvale_terrain::GeneratedTerrain,
    climate: &hornvale_climate::GeneratedClimate,
) -> ParseContext {
    parse_context_with_voices(
        world,
        hornvale_worldgen::accounts_from(world, terrain, climate),
    )
}

/// The shared body behind [`parse_context`] and [`parse_context_from`]: the
/// two pure-ledger loops (`is-a`, `instance-of`) always run; `voices` is the
/// only part either caller varies — the real (sculpted) chorus accounts, or
/// an empty list on a sculpt failure ([`parse_context`]'s degraded arm).
fn parse_context_with_voices(
    world: &World,
    voices: Vec<hornvale_worldgen::ChorusVoice>,
) -> ParseContext {
    let mut complements = BTreeSet::new();
    for fact in world.ledger.find(hornvale_kernel::world::IS_A) {
        if let Value::Text(kind) = &fact.object {
            complements.insert(kind.clone());
        }
    }
    for fact in world.ledger.find(hornvale_kernel::INSTANCE_OF) {
        if let Value::Text(kind) = &fact.object {
            // The CONCEPT, not the plural word — `parse_common` pluralizes it
            // itself when the clause it is matching is plural.
            complements.insert(kind.clone());
        }
    }
    for voice in voices {
        for entry in &voice.account.entries {
            if let Disposition::Substituted { theirs, .. } = effective(&entry.disposition) {
                complements.insert(theirs.clone());
            }
        }
    }
    ParseContext {
        complements,
        vocabulary: hornvale_worldgen::common_vocabulary(&world.registry),
    }
}

/// Invert one rendered [`render_volume`] line: split on the trailing-clause
/// seam (`"; "`), clause-parse the head via T2's `parse_common`, then
/// recover each modifier/trailing fragment's fact via [`fact_for`].
///
/// The split mirrors the realizer's trailing join exactly: the first
/// segment lost its own terminal `'.'` to that join (append it back, unless
/// there was no trailing clause at all — then the head is the whole,
/// already-terminated line); the LAST segment carries the final `'.'`,
/// which belongs to the sentence, not the fragment, so it is stripped
/// before fragment inversion. Middle segments (more than one trailing
/// clause) carry no punctuation at all.
///
/// `ParsedLine.kind` is the recovered complement CONCEPT — always singular,
/// for a `Pl` clause as much as an `Sg` one, because `parse_common` matches
/// against each candidate concept's realized surface rather than stripping a
/// letter off the text. A future `Pl` `is-a` construction needs nothing here.
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
    // `parse_common_with_tail`, not `parse_common`: Common recognizes the
    // clause skeleton and hands back the adjunct tail as TEXT, which this
    // window's own English recognizer (`fact_for`) inverts. See that
    // function's doc for why recognition stayed here.
    let (clause, tail) = parse_common_with_tail(&clause_text, ctx).map_err(LineError::Clause)?;

    let mut facts = Vec::new();
    for modifier in &tail {
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
        // The SUBJECT slot, so the nominative — the same form the realizer
        // emitted, which is what keeps `rerender` byte-exact.
        Subject::Pronoun(person) => {
            common_pronoun(*person, clause.number, PronounCase::Nominative).to_string()
        }
        // `parse_common_with_tail` still has no clause-SUBJECT recognizer as
        // of The Mortise Task 8, which taught it to recover a clause-OBJECT
        // (`Argument::Clause`, below) but deliberately not this slot: the
        // walk's subject/verb split already commits to the EARLIEST
        // verb-group occurrence, and for a subject-embedded clause that
        // earliest occurrence is the INNER clause's own verb, not the
        // matrix one — recovering it needs the walk to try more than one
        // split candidate (a backtracking search), a different and larger
        // change than extending the give-up point the object slot already
        // had. So this arm still can never fire. Added for exhaustiveness
        // against `Subject::Clause`, the same posture the `Argument::Clause`
        // arm a few lines below now takes for its own slot.
        Subject::Clause(_) => {
            unreachable!("parse_common_with_tail never recovers a clause-embedded subject")
        }
    };
    // The clause layer already recovered the singular concept id: it matched
    // the text against each candidate id's realized surface, so the plural
    // `'s'` was undone by the same rule that added it. No suffix-stripping
    // closed-world assumption survives here.
    //
    // `parse_common`/`parse_common_with_tail` CAN now return an
    // `Argument::Clause` object (The Mortise, Task 8) — the claim in this
    // arm's message is no longer true of the function in general. It stays
    // unreachable for THIS window specifically because every line this
    // window ever hands to `parse_line` comes from its own generated
    // classification prose (`rerender`, a few lines down, always builds an
    // `is-a` clause with an `Argument::Concept` object) or a hand-written
    // test fixture in the same shape — nothing here ever constructs or
    // feeds a KNOW/THINK-shaped clause-complement sentence.
    let Argument::Concept(kind) = clause.object.clone() else {
        unreachable!(
            "parse_line only ever receives an is-a classification line, whose object is always a concept"
        )
    };

    Ok(ParsedLine {
        subject,
        kind,
        facts,
        number: clause.number,
        definiteness: clause.definiteness,
        // Recovered, not defaulted: Common has a copula construction for
        // both, so unlike `evidential` these ARE observable in the surface
        // and the rerender direction must carry them rather than assume a
        // present-tense assertion.
        tense: clause.tense,
        polarity: clause.polarity,
    })
}

/// Re-realize a [`ParsedLine`] back to its exact surface text: the corpus
/// law's other half. Turns `parsed.facts` back into adjuncts via
/// [`fragment_for`] (the same construction table, forward again) and hands
/// them to the same realizer `render_volume` uses — so the two cannot drift
/// apart into separate join logic, because there is only one join and it
/// lives in `domains/language`. Re-pluralization is not done here either:
/// `parsed.kind` is the complement CONCEPT and `parsed.number` is what
/// pluralizes it, inside the realizer.
/// type-audit: bare-ok(prose: return)
pub fn rerender(parsed: &ParsedLine, vocab: &CommonVocabulary) -> String {
    let mut adjuncts = Vec::new();
    for (predicate, value) in &parsed.facts {
        // fact_for only ever recovers (predicate, value) pairs that
        // fragment_for's forward direction produced, so a None here is
        // unreachable for a ParsedLine built by parse_line.
        if let Some(adjunct) = fragment_for(predicate, value) {
            adjuncts.push(adjunct);
        }
    }
    // One statement of the inverse, in `domains/language` — this window kept
    // its own copy of the mapping until The Inquest, and that copy is exactly
    // where the stale `"its"` arm survived a rework that had already removed
    // the fragment producing it.
    let subject = match nominative_person(&parsed.subject) {
        Some(person) => Subject::Pronoun(person),
        None => Subject::Name(parsed.subject.clone()),
    };
    realize_common(
        &Clause {
            predicate: hornvale_kernel::world::IS_A.to_string(),
            subject,
            object: Argument::Concept(parsed.kind.clone()),
            number: parsed.number,
            definiteness: parsed.definiteness,
            // A `ParsedLine` carries no evidential because Common's surface
            // carries none to recover (spec §3.2). This is the same
            // documented default `parse_common_with_tail` returns, so the
            // corpus law's two directions agree on the feature neither can
            // observe.
            evidential: Evidential::Witnessed,
            tense: parsed.tense,
            polarity: parsed.polarity,
            adjuncts,
        },
        vocab,
    )
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
pub enum ChorusLine {
    /// An ordinary classification clause plus its chorus-surface dress.
    Clause(ParsedLine, ChorusDress),
    /// A because-clause explanation line (Task 4's closed frame table).
    Explanation(ParsedExplanation),
    /// C6 (The Doctrine): the `RevealedClaim` exoteric formula.
    RevealedClaim {
        /// Which closed exoteric surface this line used (the round-trip
        /// recovery key — see [`RevealedClaimSurface`]).
        surface: RevealedClaimSurface,
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
/// `hornvale_worldgen::{observations_from, ladder_from}`, not a `chorus_ground`
/// classification, so [`emic_union_margin_covers_ground_truth`]'s ground-
/// truth walk has nothing here to check.
/// type-audit: bare-ok(prose: FolkCounted.autonym), bare-ok(prose: Numbered.autonym), bare-ok(diagnostic-value: Numbered.count), bare-ok(diagnostic-value: Prediction.day), bare-ok(prose: Margin.epoch_phrase), bare-ok(diagnostic-value: Margin.count), bare-ok(prose: Crisis.autonym), bare-ok(diagnostic-value: Crisis.taught_day), bare-ok(diagnostic-value: Crisis.actual_day), bare-ok(prose: Doctrine.autonym), bare-ok(flag: Doctrine.crisis_live)
#[derive(Clone, Debug, PartialEq)]
pub enum ReckoningLine {
    /// `"The sky keeps no dates to number."` — the empty arm.
    Empty,
    /// `"Among the ⟨autonym⟩, the sky has darkened, now and again."` — the
    /// folk register, attributed per culture (The Book Polish).
    FolkCounted {
        /// The culture's autonym, as it appeared in the line.
        autonym: String,
    },
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
    /// `"In truth, the ⟨autonym⟩'s priesthood taught the darkening would
    /// come on day ⟨taught_day⟩; it came on day ⟨actual_day⟩ instead."` —
    /// the-corrigendum T3's per-culture crisis margin line.
    Crisis {
        /// The culture's autonym, as it appeared in the line.
        autonym: String,
        /// The taught (predicted) day, as rendered (integer-truncated).
        taught_day: u64,
        /// The actual day the event occurred, as rendered
        /// (integer-truncated).
        actual_day: u64,
    },
    /// `"None among the ⟨autonym⟩ have shown the priesthood's teaching
    /// false."` (`crisis_live: false`) or `"The ⟨autonym⟩'s own priesthood
    /// taught wrongly, and could be shown wrong by any who kept their own
    /// count."` (`crisis_live: true`) — the-corrigendum T4's doctrine-voice
    /// acknowledgment, thematic-only and NOT routed through
    /// `ConflictState`/`conflict_of` (decision ledger #3).
    Doctrine {
        /// The culture's autonym, as it appeared in the line.
        autonym: String,
        /// Whether this culture carried a live prediction crisis at the
        /// time the line was rendered.
        crisis_live: bool,
    },
}

/// The closed verb-literal table [`parse_explanation_body`] matches a
/// recovered word against, to hand back the same `'static` [`LexemeId`]
/// these were minted from (a `LexemeId` wraps a `&'static str`, so a
/// runtime-parsed word can never be boxed into one directly) — duplicated
/// from `domains/language::schemas`'s own closed table, the same precedent
/// [`uncardinal`] sets for a small closed table a book-only need doesn't
/// warrant widening the domain's public surface for. (This cited
/// `indefinite_article` alongside it until The Interlinear deleted that
/// function — see [`uncardinal`] for why the two were never the same case.)
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
/// No head is a prefix of another (`"The day returns"`, `"The moon
/// crosses"`, `"The moons cross"` all diverge by the 9th character; `"The
/// moons keep their measure"` diverges from `"The moons cross"` at the
/// 10th), so trying them in any order is safe.
const EXPLANATION_HEADS: &[(&str, bool)] = &[
    ("The day returns", false),
    ("The moon crosses", false),
    ("The moons cross", true),
    ("The moons keep their measure", true),
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
/// Numbered line, the prediction line) plus the truth margin, plus (the
/// Corrigendum T3) the per-culture crisis margin line. Tried before
/// [`parse_chorus_line`]'s existing arms: none of the Reckoning surfaces
/// can collide with a copula clause, a `RevealedClaim`/counter/explanation
/// prefix, or the ordinary `"In truth, "` margin dress, so trying this
/// first is safe. `None` when `line` matches none of the closed shapes,
/// so the caller falls through to its own arms unchanged.
fn parse_reckoning_line(line: &str) -> Option<ReckoningLine> {
    if line == RECKONING_EMPTY_ARM {
        return Some(ReckoningLine::Empty);
    }
    if let Some(autonym) = parse_reckoning_folk_counted(line) {
        if autonym.is_empty() {
            return None;
        }
        return Some(ReckoningLine::FolkCounted {
            autonym: autonym.to_string(),
        });
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
    if let Some(rest) = line.strip_prefix("In truth, the ")
        && let Some((autonym, rest)) =
            rest.split_once("'s priesthood taught the darkening would come on day ")
        && !autonym.is_empty()
        && let Some((taught_word, rest)) = rest.split_once("; it came on day ")
        && let Some(actual_word) = rest.strip_suffix(" instead.")
        && let Ok(taught_day) = taught_word.parse::<u64>()
        && let Ok(actual_day) = actual_word.parse::<u64>()
    {
        return Some(ReckoningLine::Crisis {
            autonym: autonym.to_string(),
            taught_day,
            actual_day,
        });
    }
    if let Some(autonym) = line
        .strip_prefix("None among the ")
        .and_then(|r| r.strip_suffix(" have shown the priesthood's teaching false."))
    {
        if autonym.is_empty() {
            return None;
        }
        return Some(ReckoningLine::Doctrine {
            autonym: autonym.to_string(),
            crisis_live: false,
        });
    }
    if let Some(autonym) = line.strip_prefix("The ").and_then(|r| {
        r.strip_suffix(
            "'s own priesthood taught wrongly, and could be shown wrong by any who kept their \
             own count.",
        )
    }) {
        if autonym.is_empty() {
            return None;
        }
        return Some(ReckoningLine::Doctrine {
            autonym: autonym.to_string(),
            crisis_live: true,
        });
    }
    None
}

/// Re-realize a [`ReckoningLine`] back to its exact surface text — the
/// closed-table forward direction, the exact inverse of
/// [`parse_reckoning_line`].
fn rerender_reckoning_line(line: &ReckoningLine) -> String {
    match line {
        ReckoningLine::Empty => RECKONING_EMPTY_ARM.to_string(),
        ReckoningLine::FolkCounted { autonym } => reckoning_folk_counted(autonym),
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
        ReckoningLine::Crisis {
            autonym,
            taught_day,
            actual_day,
        } => format!(
            "In truth, the {autonym}'s priesthood taught the darkening would come on day \
             {taught_day}; it came on day {actual_day} instead."
        ),
        ReckoningLine::Doctrine {
            autonym,
            crisis_live,
        } => reckoning_doctrine_line(autonym, true, *crisis_live)
            .expect("has_doctrine is always true here, so this always returns Some"),
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
    if let Some(surface) = parse_revealed_claim(line) {
        return Ok(ChorusLine::RevealedClaim { surface });
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
pub fn rerender_chorus_line(line: &ChorusLine, vocab: &CommonVocabulary) -> String {
    match line {
        ChorusLine::Clause(parsed, dress) => {
            let mut line = rerender(parsed, vocab);
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
        ChorusLine::RevealedClaim { surface } => rerender_revealed_claim(*surface),
        ChorusLine::Counter(inner) => counter_annotation_line(&rerender_chorus_line(inner, vocab)),
        ChorusLine::Reckoning(reckoning) => rerender_reckoning_line(reckoning),
    }
}

#[cfg(test)]
mod tests {
    //! ## The rebase onto The Toponym's cohort ordering — the rename map
    //!
    //! The nineteen toponymic concepts sort at epoch 4 rather than in
    //! cohort 0, which reseeds every proto-root, so every lexicon-derived
    //! name in every world re-draws. Fourteen tests in this module pin such
    //! names as literals; all fourteen were re-pinned by applying exactly
    //! this map and nothing else:
    //!
    //! | seed | bugbear | gnoll | goblin | hobgoblin | kobold | planet |
    //! |---|---|---|---|---|---|---|
    //! | 1 | Xoobo→Booxo | Jakdaod→Kabjab | Veewe→Woove | Veebe→Boove | Ngongngo→Zhzho | Booko→Xoaboa |
    //! | 2 | Booqboo→Bobboo | Klalsha→Klojsho | Meepmoe→Mepmee | Weeqwoe→Webwee | Ngka→Ngkooqngto | — |
    //! | 3 | Tashoo→Doozka | Jpajjpa→Jpojjpo | Ztasoe→Sdoozka | Ztashoeg→Shtoozka | Sqaojxo→Jjojjjo | — |
    //!
    //! Deities: `Wtoevvelqa` and `Dbemdden` re-drew to the same forms and
    //! are unchanged (measured, not assumed). `Kaavoa` SPLITS: it was one
    //! form standing for two distinct beliefs, and the reseed separates
    //! them into goblin `Voovoo` and hobgoblin `Vooboo`. Eleven ordinary
    //! tongue word forms moved as well — see `the_additivity_law`, the one
    //! register whose drift is not confined to proper nouns.
    //!
    //!
    //! ## The Watershed's sonority merge (Item 0) — the second rename map
    //!
    //! Sonority sequencing orders every drawn onset and coda template by
    //! rising-to-nucleus / falling-away sonority. The draw COUNT is
    //! unchanged, so this costs no entropy — it changes only what the same
    //! draws mean, and every lexicon-derived name re-mints. The map here is
    //! far smaller than the one above, because SSP reorders only the
    //! templates that actually violate it:
    //!
    //! | what | old | new | why |
    //! |---|---|---|---|
    //! | kobold, seed 1 | Zhzho | Ngosho | `Zhzh-` is a flat sibilant cluster |
    //! | deity (goblin) | Voovoo | Voovo | equal-sonority neighbours collapse |
    //! | deity | Wtoevvelqa | Twoevave | `Wt-` falls; `Tw-` rises |
    //! | deity | Dbemdden | Daemdam | (survived the LAST reseed; not this one) |
    //! | gnoll, seed 2 | Klojsho | Kloshjo | metathesis under the reorder |
    //! | kobold, seed 2 | Ngkooqngto | Dngooqtngo | `Ngk-` rises as `Dng-` |
    //! | kobold word, seed 2 | Ngkaa | Tngaa | same onset repair, in a word |
    //! | bugbear, seed 3 | Doozka | Zooqsha | |
    //! | gnoll, seed 3 | Jpojjpo | Pjojpjo | `Jp-` falls; `Pj-` rises |
    //! | goblin, seed 3 | Sdoozka | Qzhooqsa | |
    //! | planet, seed 3 | Nxatboa | Xngatboa | |
    //! | hobgoblin, seed 3 | Shtoozka | Qzhooqsha | |
    //! | kobold, seed 3 | Jjojjjo | Jaojjao | `Jj-` geminate collapses |
    //! | kobold, seed 1 word | Ngngoq/Ngngo | Ngod/Nga | the `ngng` the commit names |
    //!
    //! The third row is the change doing exactly what it was written for:
    //! a glide-then-stop onset no language uses, reordered to the stop-then-
    //! glide one every language has.
    //! **Where the inversion proof is blind.** It verifies a BIJECTION. A
    //! many-to-one entry — one old form standing for two things, as
    //! `Kaavoa` did — collapses under the map and re-expands under its
    //! inverse, so the proof passes while the pin is wrong. That is exactly
    //! what happened here, and `the_additivity_law` caught what the proof
    //! could not. Treat the inversion as necessary, not sufficient.
    //!
    //! **Evidence this is name drift and nothing else.** Applying the
    //! INVERSE of the map above to this file reproduces the pre-rebase file
    //! byte for byte (verified by hash). The rename is therefore a bijection
    //! over proper nouns and drawn word forms; no frame, count, ordering or
    //! assertion changed. Independently, every pinned NUMBER held across the
    //! rebase: the seed-1 darkening counts 6472/4010, seed-2's 81/49,
    //! seed-3's 53/32, the taught/true days 36531/36528/36522/36337/36125,
    //! the two-moon count, the star class `yellow-white dwarf (F)` and the
    //! 1.5-standard-day day length.
    //!
    //! **A stale name in a LOOKUP KEY fails as a phantom regression, not as
    //! a diff.** `the_esoteric_law_mutation_verified` keys its reader by
    //! `(planet_name, MOON_COUNT)`; when the planet renamed, the key matched
    //! nothing and the assertion saw an empty vec — which reads as "the
    //! esoteric law stopped working" rather than "the planet was renamed".
    //! Fix the key, never the behaviour.
    //!
    //! ## The Contour epoch v2 (`history/bake/v2`) — the third rename, one entry
    //!
    //! Bumping the `BAKE` stream label (position-aware conflict changes
    //! committed history, decision 0006's epoch-suffix discipline) re-mints
    //! every draw the deep-history bake takes, including seed 1's planet
    //! name: `Xoaboa` → `Pao`. Unlike the two maps above, no OTHER proper
    //! noun in this module moved — the five peoples' own names (`Booxo`,
    //! `Kabja`, `Woove`, `Boove`, `Ngosho`) and every ordinary word form are
    //! byte-identical, verified test by test rather than assumed. Every
    //! occurrence of `Xoaboa` as a planet name in this module's test bodies
    //! was mechanically replaced with `Pao`; this doc comment and the two
    //! rename tables above it are left as the historical record they are.
    //!
    //! ## The Tense — the fourth rename, and it is the third one going back
    //!
    //! Giving capacity an era axis re-placed every settlement, and seed 1's
    //! planet name moved with the flagship culture's lexicon: `Pao` →
    //! **`Xoaboa`** — which is to say the name The Contour renamed *away* from
    //! has come back around. The bijection was verified the same way, test by
    //! test rather than assumed: the six peoples' names (`Booxo`, `Kabja`,
    //! `Woove`, `Boove`, `Ngeevnao`, `Ngosho`) are byte-identical, and so is
    //! every drawn word form in the per-tongue sentences — `Paab`, `Paokaa`,
    //! `Weveawea`, `Veabea`, `Saseo`, `Ngod`, `Ngotngo` all unmoved. Every
    //! pinned NUMBER held again: seed-2's 81/49, the taught/true day 36337,
    //! the two-moon count, `yellow-white dwarf (F)`, 1.5 standard days.
    //!
    //! **Two occurrences here contain the old name as a SUBSTRING and must not
    //! be touched** — `Paab` and `Paokaa` are gnoll word forms, not the planet.
    //! The replacement is word-boundary-anchored for that reason; a plain
    //! substring pass corrupts them silently, and they are pinned nowhere else.
    //!
    //! What did NOT move with the rename, and is a separate matter entirely:
    //! seed 2's hobgoblin and seed 4's kobold changed ORGANIZATION state under
    //! this campaign. Those are content, not naming, and are adjudicated with
    //! `the_ladder_and_prophecy_laws` (`windows/worldgen/tests/diachronic.rs`)
    //! rather than folded in here.
    //!
    //! ## The Burr (Task 4) — the fifth rename, and the widest one yet
    //!
    //! Admitting an alveolar trill as an ordinary manner (no longer gated
    //! behind the exotic-consonant capability, a decision recorded at this campaign's close) inserts extra
    //! candidate-consonant draws ahead of every species' phonology
    //! inventory, which reseeds `assign_proto_roots`'s draw for every
    //! family (`ROOT_EPOCH` v3 -> v4) — not just the planet's name. **Every
    //! one of seed 1's fifteen peoples' autonyms moved this time, plus the
    //! planet (`Xoaboa` -> `Booko`) and every drawn word form in every
    //! per-tongue sentence** — the widest rename this module has absorbed,
    //! because the reseed lands ahead of every family's draw in the
    //! stream rather than appending after an unaffected roster the way a
    //! new people joining does. The full seed-1 map: bugbear `Booxo` ->
    //! `Bao`, desert-dwarf `Tngobpngap` -> `Pngoppap`, desert-elf `Beba` ->
    //! `Bua`, drow `Bobash` -> `Boa`, gnoll `Kabja` -> `Wakdao`, goblin
    //! `Woove` -> `Qwootoqo`, gully-dwarf `Tngobknga` -> `Pngoppa`,
    //! high-elf `Tedash` -> `Doa`, hill-dwarf `Dngovgngav` -> `Bngovbav`,
    //! hobgoblin `Boove` -> `Dweowbaw`, human `Ngeevnao` -> `Naavea`,
    //! kobold `Ngosho` -> `Ngongo`, sea-elf `Petash` -> `Tua`, snow-elf
    //! `Bzhonopsho` -> `Shnotsozhmo`, wood-elf `Tetas` -> `Tensnonkun`,
    //! planet `Xoaboa` -> `Booko`. What did NOT move: which peoples are
    //! placed, which are organized, every group count (6472/4010 in the
    //! reckoning, 6/9 in the reckoning's own arithmetic elsewhere), every
    //! day-number, and every English gloss — verified test by test, same
    //! as every prior rename here.
    //!
    //! Test fixture (decision 0092): calls the sculpt/fit derivation entry
    //! points directly to build its own world state, once per test — the
    //! sanctioned test-fixture posture the weir's spec carves out.
    #![allow(clippy::disallowed_methods)]
    use super::*;
    use hornvale_language::clause::AdjunctPosition;

    /// The world's Common vocabulary, exactly as `render_volume` assembles
    /// it. Built from the composed registry rather than any particular
    /// world's — the two are the same map, and `register_all` on a bare
    /// registry is sub-millisecond.
    fn vocab() -> CommonVocabulary {
        let mut registry = hornvale_kernel::ConceptRegistry::default();
        hornvale_worldgen::register_all(&mut registry).expect("the roster registers");
        hornvale_worldgen::common_vocabulary(&registry)
    }

    /// One construction's realized surface and where Common attaches it:
    /// `fragment_for` states the role, `common_role_surface` renders it.
    /// The two halves that used to be one function here.
    fn surface_of(
        predicate: &str,
        object: &Value,
        vocab: &CommonVocabulary,
    ) -> Option<(AdjunctPosition, String)> {
        common_role_surface(&fragment_for(predicate, object)?, vocab)
    }

    /// The Interlinear: this window hands the language STRUCTURE — a
    /// predicate bound to an argument — never a rendered phrase. It is the
    /// one assertion that would redden if `fragment_for` started composing
    /// English again.
    #[test]
    fn the_book_hands_the_language_structure_not_english() {
        let a =
            fragment_for(MOON_COUNT, &Value::Number(2.0)).expect("moon-count has a construction");
        assert_eq!(a.role, MOON_COUNT);
        assert_eq!(a.argument, Argument::Count(2));
    }

    #[test]
    fn coverage_flags_name_as_uncovered() {
        let world = generated(1);
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
        use hornvale_worldgen::{SettlementPins, build_world};

        let world = build_world(
            hornvale_kernel::Seed(1),
            &SkyPins::default(),
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("seed 1 builds");

        let vol = render_volume(&world);
        // `contains(" is a planet ")`, not `ends_with(" is a planet.")`.
        // Under the constant sun the sentence stopped at the classification;
        // a generated sky commits moon-count, star-class and day-length facts
        // that the volume appends as further clauses ("... is a planet with
        // two moons, orbiting a yellow-white dwarf (F); its day lasts about
        // 1.5 standard days."), so the end anchor cannot survive.
        //
        // The TRAILING SPACE is doing the work the lost period used to: it
        // keeps " is a planetoid" (and any other suffixed noun) from
        // matching, which a bare `contains(" is a planet")` would have
        // allowed. A sibling ~30 lines below asserts the whole sentence by
        // equality, so this one only has to hold the classification claim —
        // but holding it loosely would still have been a real weakening.
        assert!(
            vol.lines.iter().any(|l| l.contains(" is a planet ")),
            "the volume classifies the planet: {:?}",
            vol.lines
        );
    }

    fn generated(seed: u64) -> World {
        use hornvale_astronomy::SkyPins;
        use hornvale_terrain::TerrainPins;
        use hornvale_worldgen::{SettlementPins, build_world};

        build_world(
            hornvale_kernel::Seed(seed),
            &SkyPins::default(),
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("generated world builds")
    }

    /// Seed 1's real, committed values (verified against the world json):
    /// star class "yellow-white dwarf (F)", two moons, day-length-std
    /// 1.5507196 std days (`quantity` truncates that to "about 1.5"). This
    /// is the exact volume `hornvale -- book` renders for seed 1 ("Booko").
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
            "Booko is a planet with two moons, orbiting a yellow-white dwarf (F); \
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
    /// ("Zhqea is a planet orbiting a orange dwarf (K)…"). The ledger holds
    /// the concept id (`"orange-dwarf"`); the article is chosen from the
    /// rendered *display*'s first letter, never the id's.
    #[test]
    fn star_class_modifier_chooses_an_before_a_vowel() {
        let value = Value::Text("orange-dwarf".to_string());
        let (position, modifier) =
            surface_of(STAR_CLASS, &value, &vocab()).expect("star-class has a construction");
        assert_eq!(position, AdjunctPosition::Inline);
        assert_eq!(modifier, "orbiting an orange dwarf (K)");
    }

    /// C2 T5: every placed peopled species' `instance-of` collective
    /// renders as "The ⟨Autonym⟩ are ⟨species⟩." — exact strings, seed 1's
    /// real committed values (verified against the merged world): goblin's
    /// collective is named "Veewe", hobgoblin's "Veebe".
    #[test]
    fn instance_of_collective_renders_the_autonym_are_species() {
        let world = generated(1);
        let vol = render_volume(&world);
        assert!(
            vol.lines.iter().any(|l| l == "The Qwootoqo are goblins."),
            "goblin collective renders as the autonym: {:?}",
            vol.lines
        );
        assert!(
            vol.lines
                .iter()
                .any(|l| l == "The Dweowbaw are hobgoblins."),
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
            Subject::Pronoun(Person::Third)
        );
    }

    /// C3 T3's self-statement law (spec §5): every placed people's autonym
    /// and own-kind concept are Steeped by construction (worldgen's
    /// `exposure_from`), so every placed people's tongue self-statement
    /// renders — no gaps. C7 T3 adds a second line per placed people (the
    /// emic world-statement — `every_people_states_the_world_in_its_tongue`
    /// pins that law directly), so `tongue_lines` now carries TWO lines per
    /// placed people, not one.
    /// claim: structural(seed: [1,2,3]) — prose rendering
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
    /// claim: structural(seed: [1,2,3]) — prose rendering
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
    /// merge a46749f): seed 3's hobgoblin (`Ztashoeg`) is no longer
    /// organized at this seed (its doctrine/priesthood section is gone from
    /// the regenerated `the-book.md` — a settlement-demography fact, not a
    /// morphology change; its `evidential_depth` measured in
    /// `deep_grammar.rs` is unchanged at `None`), so its row is dropped
    /// rather than re-measured as `false`: this test requires
    /// `section.doctrine` to be `Some` for every listed (seed, kind), and it
    /// no longer is. Seed 3's new kobold (`Sqaojxo`) is ALSO not organized
    /// at this seed (no priesthood section renders for it either), so it is
    /// not added. Both arms (`true`/`false`) remain exercised by the
    /// surviving rows.
    ///
    /// Re-pinned again post-absorption (The Rains moisture epoch): seed 2's
    /// kobold is no longer organized under the epoch's re-derived settlement
    /// layout (its chorus section still renders — it still places, and
    /// `depth_landscape_measured` still pins its morphology at `None`/`None`,
    /// unchanged, since morphology is a draw the epoch does not touch — but
    /// its doctrine/priesthood section is gone from the regenerated
    /// `the-book.md`, a settlement-demography fact). Its row is dropped rather
    /// than re-measured as `false`, for the same reason as seed 3's hobgoblin
    /// above.
    ///
    /// Re-pinned again post-absorption (The Demesne, BIO-35 Stage 1 per-axis
    /// supply, this merge): the recalibrated settlement layout flipped seed 1's
    /// hobgoblin organized→folk (its priesthood section is gone from the
    /// regenerated `the-book.md`) and seed 3's hobgoblin folk→organized (its
    /// priesthood section returns). Both are settlement-demography facts, not
    /// morphology changes — `evidential_depth` is a draw The Demesne does not
    /// touch (placement is a deterministic function of K, no new draws), so
    /// seed 1 hobgoblin's `Particle` and seed 3 hobgoblin's `None` are
    /// unchanged at the derivation layer; only which of them currently carries
    /// a doctrine moved. So seed 1's hobgoblin row is dropped (folk-only, no
    /// doctrine to contrast) and seed 3's hobgoblin row is added as `false`
    /// (organized, depth `None`).
    ///
    /// The Tense (2026-08-05) dropped the two HOBGOBLIN rows. Seed 2's and
    /// seed 3's hobgoblins are no longer organized at all — they carry no
    /// doctrine section for this test to read — because era-varying capacity
    /// shrank settlements below the emergent-caste threshold that mints a
    /// shaman (the same three peoples `diachronic.rs`'s ladder table records
    /// falling Predictive -> Counted; see its comment for the mechanism).
    /// Both arms are still exercised, which is the property this table exists
    /// for: `true` via seed 3 goblin, `false` via seeds 1 and 2 goblin. The
    /// rows are REMOVED rather than re-pinned to `false`, because "not
    /// organized" is a different state from "organized with depth None" and
    /// conflating them would let the test pass on a culture it cannot read.
    const EVIDENTIAL_DEPTH_LANDSCAPE: &[(u64, &str, bool)] = &[
        (1, "goblin", false), // None
        (2, "goblin", false), // None
        (3, "goblin", true),  // Particle
    ];

    /// C7 T3's taught-contrast law (spec §3.5, the visible payoff): for
    /// every organized culture whose evidential depth is non-`None`, the
    /// doctrine's taught line's realized SURFACE (excluding each line's own
    /// gloss, which always differs in text) differs from the folk
    /// world-statement's surface — the morpheme contrast; for depth `None`
    /// the two surfaces are byte-identical (no marking to contrast).
    /// Per-species arms pinned against T2's frozen landscape (both arms are
    /// exercised within seeds 1..=3, per the measured table).
    /// claim: structural(seed: EVIDENTIAL_DEPTH_LANDSCAPE) — prose rendering,
    /// tuple pattern `&(seed, kind, evidential_marks)` (Fix round 1, Class 1)
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

    /// The Mortise, Task 10 (spec §4.9): **this window declares clause
    /// embedding and coordination deliberately unwired**, checked rather
    /// than left as prose (the failure `LANG-in-character-acts-are-
    /// unspeakable` records: three tasks each shipped an inert concept
    /// without anyone writing down who was supposed to wire it, and nothing
    /// caught the drift).
    ///
    /// Every one of this module's PRODUCTION `Clause`-construction sites
    /// (the same `production` slice [`the_readout_law`] scans, plus all
    /// three `realize_tongue_deep` call sites) states a GOD'S-EYE OR EMIC/ETIC
    /// CLASSIFICATION fact — `predicate: hornvale_kernel::world::IS_A`,
    /// `object: Argument::Concept(...)` — read straight off a committed
    /// `is-a`/`instance-of` ledger fact. Nothing in this window's data model
    /// HOLDS a belief about another clause: `Evidential::Taught` already
    /// carries doctrine's "this is what is taught" distinction as a
    /// FEATURE on the very same is-a clause (the adjacency this task was
    /// warned about, spec §8) — a matrix `THINK`/`KNOW` wrapper around it
    /// would double-encode the same fact through two unrelated mechanisms,
    /// exactly the "caller invented to justify a capability" shape the task
    /// brief warns against, and the two stay orthogonal on purpose (no
    /// amendment to the readout law above is sanctioned by the spec).
    /// `windows/almanac`'s own `Speaker` doc records the same shape of
    /// finding independently: a phenomenon has no subject for a
    /// clause-level realizer to take at all.
    ///
    /// `KNOW`/`THINK` are not even imported into this module: their only
    /// callers today are `domains/language`'s own tests and the merchant
    /// corpus witness (`cli/tests/suite/sentence_corpus.rs`), which is a
    /// TEST-side fixture, not a production caller. `Coordination` is the
    /// same story. `Subject::Clause` appears exactly once in this file's
    /// production code, as an exhaustiveness match arm in `parse_line` that
    /// stays `unreachable!()` (documented in place) — never as a
    /// constructed value; `Argument::Clause` appears only in that same
    /// arm's neighboring doc comments, never as code.
    ///
    /// This is a finding, not an oversight: every `Clause`-construction call
    /// site in this module was read for this task, looking for where the
    /// book already says something that is genuinely one clause inside
    /// another or two clauses joined — not where one could be forced in.
    /// None does. If a future campaign gives a people or a character a
    /// belief distinct from what it perceives, or narrates two committed
    /// facts as one coordinated sentence, THIS is where that caller goes —
    /// and this test must be UPDATED, not deleted, the day that happens
    /// (the same STALE-DECL discipline `seam-guard`'s
    /// `expect(survives: …)` uses).
    #[test]
    fn the_mortise_declares_no_construction_site_embeds_or_coordinates() {
        let source = include_str!("lib.rs");
        let production = source
            .split("#[cfg(test)]\nmod tests {\n")
            .next()
            .expect("this module's own `mod tests` boundary must exist");

        assert!(
            !constructs_variant(production, "Subject::Clause"),
            "the Task 10 inertness declaration is stale: a production site now \
             constructs a clause-embedded SUBJECT — update this test's doc, \
             don't delete it"
        );
        assert!(
            !constructs_variant(production, "Argument::Clause"),
            "the Task 10 inertness declaration is stale: a production site now \
             constructs a clause-embedded OBJECT — update this test's doc, \
             don't delete it"
        );
        assert!(
            !contains_bare_identifier(production, "Coordination"),
            "the Task 10 inertness declaration is stale: a production site now \
             constructs a Coordination — update this test's doc, don't \
             delete it"
        );
        assert!(
            !production.contains("realize_common_coordination(")
                && !production.contains("realize_tongue_deep_coordination("),
            "the Task 10 inertness declaration is stale: a production site now \
             realizes a coordinated utterance — update this test's doc, \
             don't delete it"
        );
        assert!(
            !contains_bare_identifier(production, "KNOW")
                && !contains_bare_identifier(production, "THINK"),
            "the Task 10 inertness declaration is stale: a production site now \
             constructs a KNOW/THINK matrix clause — update this test's doc, \
             don't delete it"
        );
    }

    /// Whether `needle` (a bare `SCREAMING_SNAKE` identifier, e.g. `"KNOW"`)
    /// appears as a whole-word TOKEN on any non-comment line of `text`.
    ///
    /// This closes a hole Task 10's own review found (The Mortise, Task 11):
    /// the guard above used to check the literal substring `"predicate:
    /// KNOW"`, which an ordinary fully-qualified reference —
    /// `hornvale_language::packs::KNOW` — evades without evading the actual
    /// construction it names. Tokenizing on non-identifier characters and
    /// comparing whole tokens catches the qualified path form too, at the
    /// cost of also catching a token inside CODE that merely happens to be
    /// named `KNOW` — which does not exist in this module today, and is the
    /// correct failure direction for a novelty guard (a false alarm is
    /// cheap; a silent miss is the thing this test exists to prevent).
    ///
    /// A line whose trimmed start is `//` (an ordinary comment, a `///` doc
    /// comment, or a `//!` module comment) is skipped, not scanned: this
    /// function's own doc comment mentions `KNOW`/`THINK` in prose, and nothing
    /// about the scanning rule should have to keep such mentions out of the
    /// tree to stay green.
    fn contains_bare_identifier(text: &str, needle: &str) -> bool {
        text.lines().any(|line| {
            if line.trim_start().starts_with("//") {
                return false;
            }
            let mut token = String::new();
            let mut hit = false;
            for ch in line.chars() {
                if ch.is_alphanumeric() || ch == '_' {
                    token.push(ch);
                } else {
                    if token == needle {
                        hit = true;
                    }
                    token.clear();
                }
            }
            if token == needle {
                hit = true;
            }
            hit
        })
    }

    /// Whether `text` constructs the tuple-variant `path` (e.g.
    /// `"Argument::Clause"`, a `::`-joined pair of bare identifiers) as a
    /// VALUE on any non-comment line — as opposed to matching it as a
    /// PATTERN in a match arm.
    ///
    /// Closes a second hole in the guard above, found in the same review
    /// (The Mortise, the fix-wave after Task 11): the original check was
    /// the literal compound substring `"object: Argument::Clause("`, which
    /// coupled the construction to being inlined directly into a field's
    /// literal. A site that BINDS first —
    ///
    /// ```text
    /// let embedded = Argument::Clause(Box::new(inner));
    /// let self_statement = Clause { ..., object: embedded, ... };
    /// ```
    ///
    /// — never contains that substring, so it passed the old guard
    /// silently. This instead looks for `path` as a whole token (using the
    /// same non-identifier-boundary rule [`contains_bare_identifier`]
    /// uses, so `MyArgument::Clause` or `Argument::ClauseWrapper` cannot
    /// match) wherever it appears on the line, which catches the bound form
    /// too.
    ///
    /// One shape is deliberately excluded, because it is a real, documented
    /// site and not a construction: this module's own
    /// `Subject::Clause(_) => { unreachable!(...) }` exhaustiveness match
    /// arm (Task 8). A bare tuple-variant followed by `(...)` and then `=>`
    /// is unambiguously a PATTERN — an expression can never occupy that
    /// position — so a `=>` immediately after the variant's own closing
    /// paren marks a pattern and is skipped; anything else (including no
    /// trailing `(...)` at all, or a construction spanning past the end of
    /// the line) counts as a hit. Per this test suite's own stated
    /// direction, a false alarm here is cheap and a silent miss is the
    /// thing this function exists to prevent, so every ambiguous case
    /// resolves toward "hit".
    fn constructs_variant(text: &str, path: &str) -> bool {
        text.lines().any(|line| {
            if line.trim_start().starts_with("//") {
                return false;
            }
            let bytes = line.as_bytes();
            let is_ident_byte = |b: u8| (b as char).is_alphanumeric() || b == b'_';
            let mut search_from = 0usize;
            while let Some(rel) = line[search_from..].find(path) {
                let start = search_from + rel;
                let end = start + path.len();
                let boundary_before = start == 0 || !is_ident_byte(bytes[start - 1]);
                let boundary_after = end >= bytes.len() || !is_ident_byte(bytes[end]);
                if boundary_before && boundary_after {
                    let rest = line[end..].trim_start();
                    match rest.strip_prefix('(') {
                        None => return true,
                        Some(after_open) => {
                            let mut depth = 1i32;
                            let mut close = None;
                            for (i, ch) in after_open.char_indices() {
                                match ch {
                                    '(' => depth += 1,
                                    ')' => {
                                        depth -= 1;
                                        if depth == 0 {
                                            close = Some(i);
                                            break;
                                        }
                                    }
                                    _ => {}
                                }
                            }
                            match close {
                                None => return true,
                                Some(i) => {
                                    let after_close = after_open[i + 1..].trim_start();
                                    if !after_close.starts_with("=>") {
                                        return true;
                                    }
                                }
                            }
                        }
                    }
                }
                search_from = end;
            }
            false
        })
    }

    /// C7 T3's shallow-identity guarantee (plan G4): for every species T2
    /// measured at depth `(None, None, _)` on both axes — seed 2's goblin
    /// and kobold are the only such (seed, species) pairs within 1..=3 (T2's
    /// frozen landscape) — that people's self-statement line is
    /// BYTE-IDENTICAL to the pre-C7 committed string (the literal from
    /// `book/src/gallery/the-book.md` before this task's regeneration).
    ///
    /// Re-pinned at The Wearing's merge (the keystone refreeze). The
    /// campaign re-derives every lexicon-drawn name — Task 3 registers 19
    /// toponymic and quality concepts, and registration feeds the lexicon's
    /// proto-root walk — so the pre-C7 bytes cannot survive it by
    /// construction. Seed 2's goblin `Maetmea Gmaapmae` -> `Meepmoe Tmamee`
    /// and kobold `Ngkoshngta Nggasdsha` -> `Ngka Tsao`. The
    /// SHALLOW-IDENTITY property this test exists for — a
    /// `(None, None, _)`-depth species states itself in one unqualified
    /// clause — is untouched: the clause shape is identical and only the
    /// drawn proper nouns moved.
    ///
    /// Re-pinned once more at the rebase onto The Toponym's cohort
    /// ordering, which reseeds every proto-root: seed 2's goblin
    /// `Meepmoe Tmamee` -> `Mepmee Gmaamea` and kobold `Ngka Tsao` ->
    /// `Ngkooqngto Ngkaa`. Both the autonym and the drawn word form moved;
    /// the English gloss in parentheses, the one-clause shape and the
    /// `(None, None, _)` depth that selects these two peoples are all
    /// unchanged. The pre-C7 bytes remain unreachable by construction —
    /// that is a consequence of the naming work, not a weakening of this
    /// test, which still asserts the clause SHAPE exactly.
    ///
    /// Re-pinned again at F7 (The Witness, 2026-07-30): gating `Tonogenesis`
    /// on a prior merger reseeds every cascade, so `evolve`'s output moves
    /// for every root — seed 2's goblin `Mepmee Gmaamea` -> `Mepmee Maa` and
    /// kobold `Ngkooqngto Ngkaa` -> `Kooqngto Kaa`. Same story again: only
    /// the drawn word forms moved.
    ///
    /// Re-pinned again at Task 8b (The Witness, same campaign): the
    /// phonology-hosting gate in `draw_rule` reseeds every cascade once
    /// more, so kobold's self-statement moves again: `Kooqngto Kaa` ->
    /// `Nggooqngdo Nggaa`. Goblin's line is unaffected (goblin's roster was
    /// already narrowed the same way by Task 7 alone, per the golden-fixture
    /// diff this same commit re-pins). Same story once more: only the drawn
    /// word forms moved.
    ///
    /// Re-pinned a further time absorbing The Watershed's sonority merge
    /// (independently reseeding the same cascade roster): only the drawn
    /// word forms moved again, re-derived from a live run.
    ///
    /// Re-pinned again at The Burr (Task 4): admitting an alveolar trill as
    /// an ordinary manner reseeds every family's root assignment
    /// (`ROOT_EPOCH` v3 -> v4, a decision recorded at this campaign's close) — seed 2's goblin
    /// `Mepmee Maa` -> `Peerzhoe Zhagee` and kobold `Dngooqdngo Dngaa` ->
    /// `Gnga Dsao`. Same story once more: only the drawn word forms moved;
    /// the SHALLOW-IDENTITY clause shape this test exists for is untouched.
    ///
    /// Re-pinned again at The Burr (Task 5): the sonorant floor reseeds
    /// phonotactics' manner list once more — kobold `Gnga Dsao` -> `Ngao
    /// Ngaasngoo`. Goblin's `Peerzhoe Zhagee` is byte-identical, unaffected
    /// this time. Same story: only the drawn word form moved.
    ///
    /// Re-pinned once more at The Burr (Task 9): the floor becomes
    /// bundle-conditioned, and kobold's family carries `concatenative`, not
    /// `sonorant-open`, so the floor no longer applies to it: `Ngao
    /// Ngaasngoo` -> `Gnga Dsao` — the same string Task 4 left behind, not a
    /// new one. Goblin (also `concatenative`) is unaffected — its manner
    /// draw at seed 2 happened not to need the floor even when it applied.
    /// Same story: only kobold's drawn word form moved.
    #[test]
    fn shallow_species_lines_are_byte_identical_to_c3() {
        let world = generated(2);
        let vol = render_volume(&world);
        assert!(
            vol.tongue_lines.contains(
                &"Peerzhoe Zhagee. (in the goblin tongue: \"The Peerzhoe are goblins.\")"
                    .to_string()
            ),
            "seed 2 goblin's self-statement must be byte-identical to the pre-C7 artifact: {:?}",
            vol.tongue_lines
        );
        assert!(
            vol.tongue_lines.contains(
                &"Gnga Dsao. (in the kobold tongue: \"The Gnga are kobolds.\")".to_string()
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

    /// The Shuttle: a drift guard, not byte-identity evidence — `render_volume`
    /// is now literally "sculpt, then call `render_volume_from`," and this
    /// test derives `terrain`/`climate` the same way, so the two calls reduce
    /// to `f(x) == f(x)`. What it pins is that a FUTURE edit cannot fork
    /// `render_volume_from`'s body from `render_volume`'s wrapper without
    /// reddening this test. The campaign's actual byte-identity evidence is
    /// the cross-binary artifact comparison: the committed gallery artifact
    /// `book/src/gallery/the-book.md` (unchanged at this campaign's HEAD) and
    /// Task 6's pre/post-binary diffs.
    #[test]
    fn from_entry_points_equal_their_wrappers() {
        let world = generated(1);
        let terrain = hornvale_worldgen::terrain_of(&world).expect("terrain reconstructs");
        let climate = hornvale_worldgen::climate_from(&world, &terrain).expect("climate derives");
        let a = render_volume(&world);
        assert!(
            !a.lines.is_empty(),
            "seed 1 must render a non-empty volume or this drift guard is vacuous"
        );
        let b = render_volume_from(&world, &terrain, &climate);
        assert_eq!(a.lines, b.lines);
        assert_eq!(a.tongue_lines, b.tongue_lines);
        assert_eq!(a.tongue_gaps, b.tongue_gaps);
        assert_eq!(format!("{:?}", a.reckoning), format!("{:?}", b.reckoning));
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
    /// claim: structural(seed: [1,2,3]) — prose round-trip, with a non-vacuity
    /// guard (predicates_exercised)
    #[test]
    fn every_book_line_round_trips() {
        let vocab = vocab();
        let mut predicates_exercised: BTreeSet<String> = BTreeSet::new();
        for seed in [1u64, 2, 3] {
            let world = generated(seed);
            let ctx = parse_context(&world);
            let volume = render_volume(&world);
            for line in &volume.lines {
                let parsed = parse_line(line, &ctx)
                    .unwrap_or_else(|e| panic!("seed {seed} line failed: {line} ({e:?})"));
                let again = rerender(&parsed, &vocab);
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
    /// claim: structural(seed: [1,2,3]) — prose round-trip inversion, plus a
    /// closed-space sweep over moon-count 0..=13 (not a seed)
    #[test]
    fn fact_for_inverts_fragment_for_over_the_closed_space() {
        let vocab = vocab();
        for count in 0..=13u64 {
            let value = Value::Number(count as f64);
            let (position, text) = surface_of(MOON_COUNT, &value, &vocab)
                .unwrap_or_else(|| panic!("moon-count {count} must render"));
            assert_eq!(position, AdjunctPosition::Inline);
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
                let (position, text) = surface_of(STAR_CLASS, &value, &vocab)
                    .unwrap_or_else(|| panic!("{value:?} must render"));
                assert_eq!(position, AdjunctPosition::Inline);
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
                let (position, text) = surface_of(DAY_LENGTH_STD, &value, &vocab)
                    .unwrap_or_else(|| panic!("day-length {days} must render"));
                assert_eq!(position, AdjunctPosition::Trailing);
                let truncated = (days * 10.0).trunc() / 10.0;
                assert_eq!(
                    fact_for(&text),
                    Some((DAY_LENGTH_STD.to_string(), Value::Number(truncated))),
                    "day-length did not round-trip through {text:?}"
                );
            }
        }
    }

    /// Law 3 (spec §4): the fixed-speaker special case. Every speaker
    /// today renders at the ceiling rung (`Decimals`), so comprehension
    /// applied at any listener rung must agree EXACTLY with calling
    /// `render_quantity_at_rung` directly on the recovered value — proving
    /// the collapsed `min(Decimals, listener_rung) == listener_rung`
    /// claim is what the code actually does, not merely what the spec
    /// claims.
    /// claim: structural(seed: [1,2,3]) — prose/rendering agreement across
    /// numeracy rungs
    #[test]
    fn comprehend_quantity_agrees_with_the_direct_render_at_every_rung() {
        let vocab = vocab();
        for seed in [1u64, 2, 3] {
            let world = generated(seed);
            for fact in world.ledger.find(DAY_LENGTH_STD) {
                let Value::Number(days) = fact.object else {
                    continue;
                };
                let value = Value::Number(days);
                let (_, fragment) = surface_of(DAY_LENGTH_STD, &value, &vocab)
                    .unwrap_or_else(|| panic!("day-length {days} must render"));
                let truncated = (days * 10.0).trunc() / 10.0;
                for rung in [
                    NumeracyRung::Subitizing,
                    NumeracyRung::FullCounting,
                    NumeracyRung::Decimals,
                ] {
                    assert_eq!(
                        comprehend_quantity(&fragment, rung),
                        Some(render_quantity_at_rung(truncated, rung)),
                        "comprehend_quantity disagreed with the direct render at rung {rung:?} for {fragment:?}"
                    );
                }
            }
        }
    }

    /// Law 4 (spec §4): no behavior change to the existing path.
    /// `fact_for` itself, unmodified, must still recover the untruncated
    /// surface value exactly as it did before this campaign.
    #[test]
    fn fact_for_itself_is_unchanged() {
        let vocab = vocab();
        let world = generated(1);
        for fact in world.ledger.find(DAY_LENGTH_STD) {
            let Value::Number(days) = fact.object else {
                continue;
            };
            let value = Value::Number(days);
            let (_, fragment) = surface_of(DAY_LENGTH_STD, &value, &vocab)
                .unwrap_or_else(|| panic!("day-length {days} must render"));
            let truncated = (days * 10.0).trunc() / 10.0;
            assert_eq!(
                fact_for(&fragment),
                Some((DAY_LENGTH_STD.to_string(), Value::Number(truncated)))
            );
        }
    }

    /// A fragment `fact_for` does not recognize returns `None`, exactly as
    /// `fact_for` itself would.
    #[test]
    fn comprehend_quantity_returns_none_for_an_unrecognized_fragment() {
        assert_eq!(
            comprehend_quantity("this is not a real fragment", NumeracyRung::Subitizing),
            None
        );
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
        assert_eq!(probes[0].subject, "Booko");
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
        let terrain = hornvale_worldgen::terrain_of(&world).expect("terrain reconstructs");
        let climate = hornvale_worldgen::climate_from(&world, &terrain).expect("climate derives");
        let noun_class_of = |concept: &str| {
            hornvale_worldgen::noun_class_from(&world, "goblin", concept, &terrain, &climate)
        };
        let mut exposures = BTreeMap::new();
        exposures.insert("planet".to_string(), ExposureClass::Steeped);
        let lexicon = build_lexicon(
            &world.seed,
            "goblin",
            "goblin",
            &ph,
            &ph,
            &exposures,
            &[],
            hornvale_language::CascadeRegime::SETTLED,
        );
        let probe = TongueProbe {
            concept: "planet".to_string(),
            subject: "Vebe".to_string(),
        };
        let paradigm = hornvale_worldgen::tongue_paradigm_of(&world, "goblin")
            .expect("goblin paradigm derives at seed 1");
        let line = probe_tongue(
            &probe,
            "goblin",
            &grammar,
            &morph,
            &paradigm,
            &noun_class_of,
            &lexicon,
            ph.orthography,
        )
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
        let section = voice_section("goblin", "Qwootoqo", &account, &world, &vocab());
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
        assert_eq!(goblin.heading, "As the Qwootoqo tell it");
        assert!(
            goblin.emic.contains(&"Booko is the earth.".to_string()),
            "planet substituted to the carving: {:?}",
            goblin.emic
        );
        assert!(
            goblin
                .emic
                .contains(&"The Dweowbaw are hobgoblins — neighbors.".to_string()),
            "goblin stance: {:?}",
            goblin.emic
        );
        assert!(
            goblin
                .margin
                .iter()
                .any(|m| m.starts_with("In truth, Booko is a planet")
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
                .contains(&"The Qwootoqo are goblins — rivals.".to_string()),
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
    /// claim: structural(seed: [1,2,3]) — prose round-trip against ground truth
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
    /// claim: structural(seed: [1,2,3]) — prose round-trip, with a non-vacuity
    /// guard (explanation_seen)
    #[test]
    fn every_chorus_line_round_trips() {
        let vocab = vocab();
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
                    let again = rerender_chorus_line(&chorus_line, &vocab);
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
    /// claim: structural(seed: [1,2,3]) — prose rendering; own name states the
    /// shape
    #[test]
    fn explanation_lines_render_for_the_measured_seeds() {
        for seed in [1u64, 2, 3] {
            let world = generated(seed);
            let vol = render_volume(&world);
            let terrain = hornvale_worldgen::terrain_of(&world).expect("terrain reconstructs");
            let climate =
                hornvale_worldgen::climate_from(&world, &terrain).expect("climate derives");
            for voice in hornvale_worldgen::accounts_from(&world, &terrain, &climate) {
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
    /// `explain` (only `accounts_from` calls it), so its chorus section must
    /// carry none of the six frames' distinguishing text — and
    /// `render_volume`'s god's-eye `lines` (never touched by C5's causal
    /// filter at all — `explain` only ever runs inside `voice_section` via
    /// `accounts_from`) stay exactly as C4 shipped them.
    #[test]
    fn the_null_volume_is_untouched() {
        let world = generated(1);
        let ground = hornvale_worldgen::chorus_ground(&world);
        let account = hornvale_language::account::account_of(
            &ground,
            &hornvale_language::account::identity_params(),
        );
        let section = voice_section("goblin", "Qwootoqo", &account, &world, &vocab());
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
                == "Booko is a planet with two moons, orbiting a yellow-white dwarf (F); \
                    its day lasts about 1.5 standard days."),
            "the god's-eye planet line stays exactly as C4 shipped it: {:?}",
            vol.lines
        );
        assert!(
            vol.lines.iter().any(|l| l == "The Qwootoqo are goblins."),
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

    /// C6 T3: on seed 1, all placed peoples reach the organized rung and
    /// each gains a doctrine section. This is precedented genesis drift: the
    /// the-living-community merge (The Demesne per-axis supply + history-driven
    /// re-placement, crossed with The Slumber's diurnal climate) seated four
    /// peoples (bugbear/Xoobo, goblin/Veewe, hobgoblin/Veebe,
    /// kobold/Ngongngo) at seed 1, all above the organized rung — matching
    /// the regenerated `book/src/gallery/the-book.md`. The Vacancy T9 adds a
    /// fifth people (gnoll/Jakdaod), which also organizes at that same
    /// seed. The Generalist adds a sixth people (human), which also
    /// organizes at that same seed. Goblin's exact measured surface is
    /// verified against the merged world: heading names the priesthood; the
    /// emic carries the `RevealedClaim` exoteric formula for the moons (folk
    /// capability loses `moon-count`, doctrine's boosted capability clears
    /// the threshold and keeps it) and a day explanation whose bound agent
    /// is the doctrine's own measured deity, Vooboo (re-pinned at Task 8b, The
    /// Witness, as `Vooboo`; The Radiation re-places seed 1 and the same
    /// belief's deity name redrew to `Voobo` mid-campaign and back to
    /// `Vooboo` at its close, one syllable each way as the draws moved under
    /// two further placement changes — the belief, its period and the
    /// day-match are unchanged, as at every prior redraw;
    /// The Witness; folk's own day explanation is agentless `PathJourney`,
    /// so this is genuinely a doctrine-only causal story, not an echo of
    /// folk's).
    /// claim: structural(seed: 1) — false-positive extra flag; `s` binds a
    /// &ChorusSection, single fixed seed
    #[test]
    fn seed_1_doctrine_sections_render() {
        let world = generated(1);
        let vol = render_volume(&world);
        let peoples = hornvale_worldgen::placed_peoples(&world);
        let organized: Vec<&str> = vol
            .chorus
            .iter()
            .filter(|s| s.doctrine.is_some())
            .map(|s| s.kind.as_str())
            .collect();
        // The Delvers re-pin (C2c, 2026-08-07): nine peoples are placed at
        // seed 1 and EIGHT are organized. `desert-dwarf` is placed but
        // folk-only — the first time this assertion's "all placed peoples are
        // organized" reading has been false, so the two lines are now
        // deliberately different lengths rather than restating one fact
        // twice. It is corroborated independently by
        // `worldgen::diachronic::the_ladder_law`, which measures seed 1's
        // desert-dwarf at `Counted` with no prediction — a culture with no
        // organized cult cannot exceed Counted, which is the ladder's own
        // structural rule, and doctrine is exactly what it lacks.
        //
        // (It read 11 placed / 9 organized while the campaign carried five
        // dwarves; spec §11 withdrew Mountain and Duergar. No non-dwarf row
        // moved with them at this seed — the eight names below are exactly
        // the previous nine minus mountain-dwarf.)
        //
        // The Range re-pin (task 4, 2026-08-09): SEVEN organized, not eight —
        // `gnoll` joins `desert-dwarf` as placed-but-folk-only. Gnoll is the
        // campaign's first biome-affinity occupant, and its declared desert
        // preference takes its seed-1 settlement count from 61 to 13; the
        // culture is still placed (the chorus still renders "Among the Kabja"
        // in `the_reckoning_renders_the_epoch_pair`) but no longer reaches the
        // organized rung. That is the intended mechanism reaching the world,
        // not drift to be papered over — this is the deliberate re-pin, in the
        // commit that moved it.
        //
        // The Radiation re-pin (C2d, 2026-08-10): FIFTEEN placed, TWELVE
        // organized. Five of the six elves reach the organized rung at seed 1;
        // `sea-elf` joins `desert-dwarf` and `gnoll` as placed-but-folk-only.
        // Corroborated independently by `worldgen::diachronic`'s LADDER_TABLE,
        // which measures all three at `Counted` with no prediction at this
        // seed — a culture with no organized cult cannot exceed Counted.
        //
        // The Radiation SECOND re-pin (C2d task 6, 2026-08-10): ELEVEN
        // organized of the same fifteen placed. `desert-elf` joins the
        // folk-only set, making it four: desert-dwarf, desert-elf, gnoll,
        // sea-elf. This is the campaign's own later placement movement (the
        // task-3 affinity relevel and the founder-collision cut, both after
        // the re-pin above), not a new mechanism — the placed COUNT is
        // unmoved at fifteen and only the organized/folk-only boundary shifted
        // by one people. The same three corroborating readings move with it:
        // `the_reckoning_renders_the_epoch_pair` loses the Beba's four
        // priesthood lines, `reckoning_at_...` loses its one, and
        // `diachronic::the_ladder_and_prophecy_laws` reads seed 1's
        // desert-elf at `Counted` instead of `Predictive`. Four independent
        // surfaces, one fact.
        //
        // The Glasshouse re-pin (decision 0134, the terrain epoch): NINE
        // organized of the same fifteen placed. `high-elf` and `snow-elf` join
        // the folk-only set, making it six. This is a WORLD change, not a
        // placement-rule change — the craton rescale's exact solve and raised
        // clamp move every coastline, so where peoples land and how large a
        // congregation each gathers both move with them. The placed COUNT is
        // again unmoved at fifteen, which is the useful signal: the ladder's
        // organized/folk-only boundary is sensitive to terrain, its placement
        // count is not. `reckoning_at_...` loses the Tedash's and
        // Bzhonopsho's priesthood lines in the same commit — the same two
        // peoples, read through a different surface.
        //
        // The Glasshouse re-pin (Stage B Task 4, the thermostat): TEN
        // organized of the same fifteen placed. `high-elf` rejoins the
        // organized set, leaving five folk-only (desert-dwarf, desert-elf,
        // gnoll, sea-elf, snow-elf). Again a WORLD change, not a
        // placement-rule change: the damped, greenhouse-forced insolation
        // baseline (replacing the fixed 288 K blackbody one) moves settlement
        // scale, which moves which flagships clear the shaman-caste
        // threshold. The placed COUNT is again unmoved at fifteen.
        // `reckoning_at_...` gains the Tedash's priesthood line back in the
        // same commit — the same people, read through a different surface.
        // THE TRENCHER re-pin (2026-09-12, the repair pass, ledger #25/#26):
        // THIRTEEN organized of the same nineteen placed. `duergar` and
        // `kuo-toa` -- two of the four peoples the absorbed Underworld
        // Peoples work added -- join the organized set, leaving six folk-only
        // (desert-dwarf, desert-elf, gnoll, mountain-dwarf, sea-elf,
        // snow-elf). A WORLD change again, not a placement-rule change: Task
        // 4 routes `EnergySource::DetritalImport` onto the `DETRITUS` axis,
        // which was flat on all land, so subterranean rungs now carry real
        // import and underworld congregations scale differently. The placed
        // COUNT is unmoved at nineteen, the same signal every re-pin above
        // reports. Corroborated on the same two independent surfaces as
        // every previous re-pin: `diachronic::LADDER_TABLE` reads seed 1's
        // duergar and kuo-toa at `Predictive` where it read `Counted`, and
        // `the_reckoning_renders_the_epoch_pair` gains the Fovfav's and
        // Wodbog's four priesthood lines each in this same commit.
        // MERGE RE-PIN (The Trencher absorbing The Tidemark, 2026-09-15):
        // TWELVE organized, not thirteen. `duergar` and `svirfneblin` are
        // folk-only on the merged tree while `mountain-dwarf` joins the
        // organized set -- the merged world (this branch's Task 4
        // per-metabolite change plus its absorbed four underworld peoples;
        // The Tidemark's own ten new peoples and settlement-selector
        // repair; and the species metabolic-triple migration fix this
        // merge also carries) decides the cult-form threshold differently
        // from either branch alone. Re-measured directly against the
        // merged code rather than guessed at by combining the two deltas.
        assert_eq!(
            organized,
            vec![
                "bugbear",
                "drow",
                "goblin",
                "gully-dwarf",
                "high-elf",
                "hill-dwarf",
                "hobgoblin",
                "human",
                "kobold",
                "kuo-toa",
                "mountain-dwarf",
                "wood-elf",
            ],
            "seed-1: twelve of the nineteen placed peoples are organized; \
             desert-dwarf, desert-elf, duergar, gnoll, sea-elf, snow-elf and \
             svirfneblin are folk-only"
        );
        // Two campaigns re-pin this together, 15 -> 24: five of The
        // Tidemark's six marine peoples settle (merfolk is `Gregarious`) and
        // all four Underworld peoples do. MEASURED on the merged world, not
        // added from the two branches' separate pins (20 and 19).
        assert_eq!(peoples.len(), 24, "seed-1: twenty-four peoples are placed");

        let goblin = vol
            .chorus
            .iter()
            .find(|s| s.kind == "goblin")
            .expect("goblin voice");
        let doctrine = goblin
            .doctrine
            .as_ref()
            .expect("goblin's organized flagship gates in a doctrine section at seed 1");
        assert_eq!(
            doctrine.heading,
            "As the priesthood of the Qwootoqo teach it"
        );
        assert!(
            doctrine
                .emic
                .contains(&"The moons are counted and known to the priesthood.".to_string()),
            "the RevealedClaim exoteric formula for the moons: {:?}",
            doctrine.emic
        );
        // The Burr re-pin (Task 4): the trill epoch bump reseeds every
        // belief-agent draw too, not only species/settlement names — the
        // bound deity redrew from `Vooboo` to `Veewe` (coincidentally an
        // old, unrelated pre-Wearing goblin exonym; the belief, its period
        // and the day-match this test guards are unchanged).
        assert!(
            doctrine
                .emic
                .contains(&"The day returns because Veewe strides the sky, briskly.".to_string()),
            "the measured doctrine day explanation, agent Veewe: {:?}",
            doctrine.emic
        );
    }

    /// C6 T3, the disclosure law (both directions). Re-derived at the-living-
    /// community merge: the disclosure law now fires ABUNDANTLY on REAL data.
    /// The merge's history-driven re-placement seats capable folk cultures
    /// beside organized ones, so a fact both KEEP but explain under differing
    /// schemas is `Contested` — `moon-count` at many seeds (folk keeps and
    /// explains it, doctrine keeps and explains it differently), and — the
    /// LANG-48 × C6 case — `moon-period-ratio` at seed 4's kobold (folk
    /// `Explained{CycleReturn}` vs doctrine bare `Kept`). Every `Contested`
    /// entry renders exactly one counter-annotation (`"— though the folk say
    /// …"`), quoting the folk voice's own because-clause; a section with no
    /// `Contested` entry carries none. `day-length-std` is `CrossReferential`,
    /// never folk-verifiable, so a differing schema there is always `Mystery`,
    /// never `Contested`. The old measurement — "NO real Contested entry
    /// exists across seeds 1..=40" — held only before the re-placement widened
    /// which cultures are organized and capable; it is now falsified by the
    /// live sweep, which is the stronger evidence (the law firing on real
    /// worlds, not just the synthetic pair driven directly below).
    /// claim: reachability(seed: 1..=5) — own comment: "the sweep finds BOTH a
    /// real Contested and a real Mystery", with an embedded per-section
    /// invariant riding on the same builds
    #[test]
    fn the_disclosure_law_both_directions() {
        // The live half: every organized section renders exactly one
        // counter-annotation per real Contested entry, and the sweep finds
        // BOTH a real Contested and a real Mystery.
        let mut contested_seen = false;
        let mut mystery_seen = false;
        for seed in 1u64..=5 {
            let world = generated(seed);
            let vol = render_volume(&world);
            let terrain = hornvale_worldgen::terrain_of(&world).expect("terrain reconstructs");
            let climate =
                hornvale_worldgen::climate_from(&world, &terrain).expect("climate derives");
            for voice in hornvale_worldgen::accounts_from(&world, &terrain, &climate) {
                let Some(doctrine) =
                    hornvale_worldgen::doctrine_from(&world, &voice.kind, &terrain, &climate)
                else {
                    continue;
                };
                let mut contested_here = 0usize;
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
                    match conflict_of(&f_entry.disposition, &d_entry.disposition, verifiable) {
                        ConflictState::Contested => {
                            contested_here += 1;
                            contested_seen = true;
                        }
                        ConflictState::Mystery => mystery_seen = true,
                        _ => {}
                    }
                }
                let section = vol
                    .chorus
                    .iter()
                    .find(|s| s.kind == voice.kind)
                    .expect("every organized voice has a chorus section");
                let rendered = section
                    .doctrine
                    .as_ref()
                    .expect("an organized voice renders a doctrine section");
                assert_eq!(
                    rendered.annotations.len(),
                    contested_here,
                    "seed {seed} {}: the disclosure law renders exactly one counter-\
                     annotation per Contested entry: {:?}",
                    voice.kind,
                    rendered.annotations
                );
            }
        }
        assert!(
            contested_seen,
            "the merge's re-placement makes real Contested entries exist across seeds 1..=5"
        );
        assert!(
            mystery_seen,
            "the sweep should find at least one real Mystery entry (day-length-std)"
        );

        // Pin the LANG-48 × C6 case on real data: a priesthood keeps a claim
        // bare while its folk explain it, so the Contested renders a
        // counter-annotation quoting the folk's own because-clause.
        //
        // Re-pointed by The Tense (2026-08-05) from seed 4's kobold, which is
        // no longer organized. Two things were checked before re-pointing
        // rather than assumed. The general case is HEALTHY: eleven such
        // counter-annotations exist across seeds 1..=5, so `contested_seen`
        // above is not carrying this alone. But the specific FLAVOUR moved —
        // every survivor is a moon-CROSSING explanation, and the
        // moon-period-ratio variant this pin used to illustrate ("The moons
        // keep their measure …") does not occur anywhere in seeds 1..=5 now.
        // Recorded as a coverage note: the schema is unexercised in this
        // window, not known-broken.
        //
        // Re-pointed again at The Burr (Task 4): the trill epoch reseeds
        // every belief-agent draw, including bugbear's moon-crossing kin —
        // `Boko` -> `Xooka`. The general case above is unaffected.
        let vol1 = render_volume(&generated(1));
        let bugbear1 = vol1
            .chorus
            .iter()
            .find(|s| s.kind == "bugbear")
            .and_then(|s| s.doctrine.as_ref())
            .expect("seed 1 bugbear is organized");
        assert!(
            bugbear1.annotations.contains(
                &"— though the folk say The moons cross because they are Xooka's kin.".to_string()
            ),
            "the Contested renders its counter-annotation quoting the folk's own \
             because-clause: {:?}",
            bugbear1.annotations
        );

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
            &vocab(),
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
            &vocab(),
        );
    }

    /// C6 T3, the null-effect law: adding `doctrine` to `ChorusSection` must
    /// not perturb one byte of the pre-existing folk registers. Pinned exactly
    /// (not just `contains`) against seed 1's committed folk strings. Re-pinned
    /// at the-living-community merge: the history-driven re-placement now seats
    /// FOUR peoples at seed 1 (bugbear/Xoobo, goblin/Veewe, hobgoblin/Veebe,
    /// kobold/Ngongngo) and renames the planet Booko, so the folk emic now
    /// carries four peoples-lines; the C6 null-effect property (doctrine adds
    /// nothing to these two registers) is unchanged — only the merged ground
    /// truth moved. The Vacancy T9 adds a FIFTH people (gnoll/Jakdaod),
    /// re-pinned again — the null-effect property still holds, only the
    /// ground truth grew by one more peoples-line.
    ///
    /// Re-pinned once more at The Wearing's merge (the keystone refreeze).
    /// Seed 1's five peoples and its planet all re-rendered under the
    /// campaign's naming — bugbear Babako -> Xoobo, gnoll Kaabjaab ->
    /// Jakdaod, goblin Vavako -> Veewe, hobgoblin Ddenke -> Veebe, kobold
    /// Ngngoashzhoo -> Ngongngo, planet Xobo -> Booko. FIVE peoples still,
    /// same species, same stances, same sentence frames: the C6 null-effect
    /// property is untouched and only the proper nouns moved.
    ///
    /// Re-pinned once more at the rebase onto The Toponym's cohort
    /// ordering — Xoobo -> Booxo, Jakdaod -> Kabjab, Veewe -> Woove,
    /// Veebe -> Boove, Ngongngo -> Zhzho, Booko -> Xoaboa (the module
    /// header carries the full map). Still five peoples, same species, same
    /// stances (`neighbors`/`ourselves`), same sentence frames, and the
    /// closing "The day returns because the sky must be crossed." is
    /// byte-identical.
    ///
    /// The Generalist re-pin (2026-08-03): human joins the coexistence stack
    /// as a sixth people, organized alongside the other five at seed 1
    /// (matching `seed_1_doctrine_sections_render` above) — the folk emic
    /// gains one more peoples-line, "The Ngeevnao are humans — neighbors."
    /// The C6 null-effect property this test exists to guard is unchanged;
    /// only the ground truth grew by one more peoples-line, same as every
    /// prior re-pin in this test's history.
    ///
    /// The Delvers re-pin (C2c, 2026-08-07): the three dwarves join as peoples
    /// seven through nine and all three are placed at seed 1, so the folk
    /// emic gains three more peoples-lines — desert dwarf (Tngobpngap),
    /// gully dwarf (Tngobknga), hill dwarf (Dngovgngav). Note that
    /// `desert-dwarf` appears HERE even
    /// though `seed_1_doctrine_sections_render` shows it is not organized:
    /// the emic peoples-lines are about who the goblins have a NAME for, not
    /// who has a priesthood. The six pre-existing lines are BYTE-IDENTICAL,
    /// which is the C6 null-effect property this test exists to guard; only
    /// the ground truth grew, same as every prior re-pin in this history.
    ///
    /// The Radiation re-pin (C2d, 2026-08-10): the six elves join as peoples
    /// ten through fifteen and all six are placed at seed 1, so the folk emic
    /// gains six more peoples-lines — desert elf (Beba), drow (Bobash), high
    /// elf (Tedash), sea elf (Petash), snow elf (Bzhonopsho), wood elf
    /// (Tetas). **The nine pre-existing lines are BYTE-IDENTICAL**, which is
    /// the C6 null-effect property this test exists to guard; only the ground
    /// truth grew, exactly as at every prior re-pin. Note `sea-elf` appears
    /// here though `seed_1_doctrine_sections_render` shows it is not
    /// organized: the emic peoples-lines are about who the goblins have a NAME
    /// for, not who has a priesthood.
    ///
    /// The Burr re-pin (Task 5): the sonorant floor tops up every envelope
    /// short of a trill/approximant, which reseeds phonotactics' manner list
    /// for the affected species — desert dwarf, desert elf, drow, gully
    /// dwarf, high elf, hill dwarf, human, kobold, sea elf, snow elf and
    /// wood elf all re-drew their proper nouns. The C6 null-effect property
    /// this test exists to guard is unchanged; only the proper nouns moved.
    ///
    /// Re-pinned once more at The Burr (Task 9): the floor becomes
    /// bundle-conditioned, so only `sonorant-open` (elf) still carries it —
    /// every other family reverts to its pre-Task-5 draw: desert dwarf,
    /// gully dwarf and hill dwarf (`templatic`), human and kobold
    /// (`concatenative`) re-draw without a forced liquid. The elf-family
    /// lines (desert elf, drow, high elf, sea elf, snow elf, wood elf) are
    /// BYTE-IDENTICAL, since `sonorant-open` keeps the floor. Same story:
    /// only the affected proper nouns moved.
    ///
    /// Re-pinned at The Burr (Task 15): per-bundle orthography moves several
    /// dwarf and elf autonyms again, and the elf-family names additionally
    /// pick up their bundle's diacritic spelling (`š`) in place of the `sh`
    /// digraph. Every line keeps its order, template and count; only the
    /// affected proper nouns moved.
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
                "The Šzoašzo are abyssal elfs — neighbors.".to_string(),
                "The Bao are bugbears — neighbors.".to_string(),
                "The Bazap are desert dwarfs — neighbors.".to_string(),
                "The Ruašre are desert elfs — neighbors.".to_string(),
                "The Roašror are drows — neighbors.".to_string(),
                "The Dazha are duergars — neighbors.".to_string(),
                "The Wakdao are gnolls — neighbors.".to_string(),
                "The Qwootoqo are goblins — ourselves.".to_string(),
                "The Xabxat are gully dwarfs — neighbors.".to_string(),
                "The Šdoašded are high elfs — neighbors.".to_string(),
                "The Dashav are hill dwarfs — neighbors.".to_string(),
                "The Dweowbaw are hobgoblins — neighbors.".to_string(),
                "The Naavea are humans — neighbors.".to_string(),
                "The Zukzhov are kelp tenders — neighbors.".to_string(),
                "The Ngongo are kobolds — neighbors.".to_string(),
                "The Wodbog are kuo toas — neighbors.".to_string(),
                "The Dazhaf are mountain dwarfs — neighbors.".to_string(),
                "The Xaoxa are reef masons — neighbors.".to_string(),
                "The Šzuašzez are sea elfs — neighbors.".to_string(),
                "The Šroašror are snow elfs — neighbors.".to_string(),
                "The Kxsonkxzo are svirfneblins — neighbors.".to_string(),
                "The Nao are tritons — neighbors.".to_string(),
                "The Peqgop are vent commensals — neighbors.".to_string(),
                "The Sruasrer are wood elfs — neighbors.".to_string(),
                "Booko is the earth.".to_string(),
                "The day returns because the sky must be crossed.".to_string(),
            ]
        );
        assert_eq!(
            goblin.margin,
            vec![
                "In truth, Booko is a planet with two moons, orbiting a yellow-white dwarf \
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
                "The Šzoašzo are abyssal elfs — rivals.".to_string(),
                "The Bao are bugbears — rivals.".to_string(),
                "The Bazap are desert dwarfs — rivals.".to_string(),
                "The Ruašre are desert elfs — rivals.".to_string(),
                "The Roašror are drows — rivals.".to_string(),
                "The Dazha are duergars — rivals.".to_string(),
                "The Wakdao are gnolls — rivals.".to_string(),
                "The Qwootoqo are goblins — rivals.".to_string(),
                "The Xabxat are gully dwarfs — rivals.".to_string(),
                "The Šdoašded are high elfs — rivals.".to_string(),
                "The Dashav are hill dwarfs — rivals.".to_string(),
                "The Dweowbaw are hobgoblins — ourselves.".to_string(),
                "The Naavea are humans — rivals.".to_string(),
                "The Zukzhov are kelp tenders — rivals.".to_string(),
                "The Ngongo are kobolds — rivals.".to_string(),
                "The Wodbog are kuo toas — rivals.".to_string(),
                "The Dazhaf are mountain dwarfs — rivals.".to_string(),
                "The Xaoxa are reef masons — rivals.".to_string(),
                "The Šzuašzez are sea elfs — rivals.".to_string(),
                "The Šroašror are snow elfs — rivals.".to_string(),
                "The Kxsonkxzo are svirfneblins — rivals.".to_string(),
                "The Nao are tritons — rivals.".to_string(),
                "The Peqgop are vent commensals — rivals.".to_string(),
                "The Sruasrer are wood elfs — rivals.".to_string(),
                "Booko is the earth.".to_string(),
                "The day returns, as all things return.".to_string(),
            ]
        );
        assert_eq!(
            hobgoblin.margin,
            vec![
                "In truth, Booko is a planet with two moons, orbiting a yellow-white dwarf (F); its day lasts about 1.5 standard days.".to_string(),
            ]
        );
    }

    /// C6 T3, the corpus law extended once more (mirrors
    /// `every_chorus_line_round_trips`): every doctrine `emic` +
    /// `annotations` + `margin` line, across seeds 1..=5, round-trips
    /// byte-identically through `parse_chorus_line` + `rerender_chorus_line`
    /// — the `RevealedClaim` formula inverts to (its closed surface), and a
    /// counter-annotation inverts by stripping the fixed prefix and
    /// re-parsing the embedded folk sentence recursively (exercised
    /// directly here too, since no real seed carries one — see
    /// `the_disclosure_law_both_directions`). `revealed_claim_seen` guards
    /// against a vacuously-true walk that stopped firing `RevealedClaim`
    /// lines entirely.
    /// claim: structural(seed: 1..=5) — prose round-trip, with a non-vacuity
    /// guard (revealed_claim_seen)
    #[test]
    fn every_doctrine_line_round_trips() {
        let vocab = vocab();
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
                    let again = rerender_chorus_line(&chorus_line, &vocab);
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
        assert_eq!(rerender_chorus_line(&parsed, &vocab), annotation);
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
            &vocab(),
        );
    }

    /// C6 T4, the esoteric law, mutation-verified: an empty reader
    /// discloses nothing (the committed exoteric edition is unaffected —
    /// its `RevealedClaim` formula still renders as usual), while a reader
    /// holding exactly `("Booko", "moon-count")` gets exactly one initiated
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
        reader.insert(("Booko".to_string(), MOON_COUNT.to_string()));
        let lines = esoteric_lines(&world, &reader);
        assert_eq!(
            lines,
            // The Book Polish (2026-07-20): re-pinned with its subject
            // (was the bare "— two, as the initiated count."). Merge:
            // planet renamed Vebe -> Booko.
            vec!["Booko has two moons, as the initiated count.".to_string()],
            "exactly one initiated line for the one key in the reader"
        );

        // Mutation-verify: the "two" comes from the LEDGER's own value, not
        // some other source — independently read the ledger's committed
        // moon-count for Booko and confirm it (not a different number)
        // reproduces the line, then confirm a WRONG value's rendering
        // would NOT match what `esoteric_lines` actually produced (the
        // "verify the mechanism by asserting a WRONG expected value
        // fails" arm this test's own doc calls out).
        let xobo = entity_named(&world, "Booko").expect("Booko resolves to an entity");
        let ledger_value = match world.ledger.value_of(xobo, MOON_COUNT) {
            Some(Value::Number(n)) => *n as u64,
            other => panic!("Booko's ledger moon-count must be a Value::Number: {other:?}"),
        };
        assert_eq!(
            ledger_value, 2,
            "Booko's committed moon-count is two at seed 1"
        );
        let moon_word = if ledger_value == 1 { "moon" } else { "moons" };
        assert_eq!(
            lines[0],
            format!(
                "Booko has {} {moon_word}, as the initiated count.",
                cardinal(ledger_value)
            ),
            "the line's cardinal must equal the ledger's own value"
        );
        let wrong = format!(
            "Booko has {} {moon_word}, as the initiated count.",
            cardinal(ledger_value + 1)
        );
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
            // The Book Polish (2026-07-20): re-pinned with its subject.
            initiated_extra.contains(&"Booko has two moons, as the initiated count.".to_string()),
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
    /// live measurement. Re-pinned at the-living-community merge: the
    /// history-driven re-placement seated FOUR peoples at each of seeds
    /// 1..=3, ALL organized (`Predictive`), so each rendered four priesthood
    /// arms (folk line, cardinal, prediction, taught-wrongly) plus a per-
    /// culture crisis margin and the world shortfall line. The Vacancy T9
    /// adds a fifth people (gnoll/Jakdaod), also organized at every seed
    /// 1..=3, rendering a fifth priesthood arm. The Generalist adds a sixth
    /// people (human), also organized at every seed 1..=3, rendering a
    /// sixth priesthood arm — it joins the solar-only pair (goblin,
    /// hobgoblin), matching `windows/worldgen/tests/diachronic.rs::
    /// LADDER_TABLE`'s per-seed witnessed counts for human. The values below
    /// are the merged live measurement (they match
    /// `book/src/gallery/the-book.md` and `LADDER_TABLE`'s day-numbers).
    ///
    /// Re-pinned again at The Burr (Task 5): the sonorant floor reseeds
    /// phonotactics' manner list once more for every affected family, so
    /// autonyms at all three seeds moved again. The organized/folk-only
    /// partition, every witnessed count and every day-number are BYTE-
    /// IDENTICAL; only the drawn words differ.
    ///
    /// Re-pinned once more at The Burr (Task 9): the floor becomes
    /// bundle-conditioned, so only `sonorant-open` (elf) keeps it — every
    /// other affected family (desert dwarf, gully dwarf, hill dwarf, human,
    /// kobold) reverts to its pre-Task-5 autonym. Same story once more: the
    /// partition, every witnessed count and every day-number are BYTE-
    /// IDENTICAL; only the drawn words differ.
    ///
    /// Re-pinned at The Burr (Task 15): per-bundle orthography moves several
    /// dwarf and elf autonyms at all three seeds again (elf-family names
    /// additionally gain a `š`/`ṅ` diacritic spelling in place of their
    /// `sh`/`ng` digraphs). The organized/folk-only partition, every
    /// witnessed count and every day-number are BYTE-IDENTICAL; only the
    /// drawn words differ.
    /// claim: structural(seed: [1,2,3]) — prose rendering
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

        // Merge re-placement: seed 1 seats four organized peoples (Xoobo,
        // Veewe, Veebe, Ngongngo), each rendering the full priesthood
        // run. Day-numbers match LADDER_TABLE's seed-1 rows.
        //
        // The Delvers re-pin (C2c, 2026-08-07): nine peoples are placed at
        // seed 1 and EIGHT are organized — desert-dwarf (Tngobpngap) renders
        // only the "the sky has darkened" line, with no priesthood run, which
        // is `LADDER_TABLE`'s seed-1 `Counted`-with-no-prediction row showing
        // up in the prose. Re-measured wholesale from the live volume, and
        // the day-numbers still match `LADDER_TABLE`'s seed-1 rows exactly.
        // Withdrawing Mountain and Duergar (spec §11) removed their runs and
        // left every other people's autonym, count and day BYTE-IDENTICAL at
        // this seed.
        //
        // The Range re-pin (task 4, 2026-08-09): SEVEN organized. Gnoll (Kabja)
        // joins desert-dwarf (Tngobpngap) as placed-but-folk-only — its "Among
        // the Kabja" line stays and its three priesthood lines go, which is
        // precisely the shape of a culture that lost its organized cult rather
        // than its existence. The cause is this campaign's own first
        // biome-affinity row, which takes gnoll from 61 seed-1 settlements to
        // 13. Every other people's autonym, count and day is BYTE-IDENTICAL,
        // so the movement is gnoll's alone — the same shape the dwarf
        // withdrawal produced.
        //
        // The Radiation re-pin (C2d, 2026-08-10): FIFTEEN placed at seed 1 and
        // TWELVE organized. Sea-elf (Petash) joins desert-dwarf (Tngobpngap)
        // and gnoll (Kabja) as placed-but-folk-only — its "Among the Petash"
        // line renders with no priesthood run, which is `LADDER_TABLE`'s
        // seed-1 `Counted`-with-no-prediction row showing up in the prose.
        // Re-measured wholesale from the live volume at all three seeds; every
        // pre-existing people's autonym, count and day is BYTE-IDENTICAL, so
        // the movement is confined to the six new arms plus sea-elf's missing
        // priesthood run.
        //
        // The Radiation SECOND re-pin (C2d task 6, 2026-08-10): ELEVEN
        // organized of the same fifteen placed. Desert-elf (Beba) joins the
        // folk-only set — its "Among the Beba" line stays and its three
        // priesthood lines and its one crisis-margin line go, exactly the
        // shape gnoll's loss took at The Range. Every other people's autonym,
        // count and day is BYTE-IDENTICAL and the two group counts (6472
        // lunar-witnessing, 4010 solar-only) do not move at all, so the whole
        // diff is desert-elf's missing priesthood run: the reckoning
        // ARITHMETIC did not move, only who has a priesthood to report it.
        // The cause is this campaign's own later placement movement, not a new
        // mechanism.
        //
        // The Burr re-pin (Task 4): admitting an alveolar trill as an
        // ordinary manner (a decision recorded at this campaign's close) reseeds every family's root
        // assignment (`ROOT_EPOCH` v3 -> v4), so every autonym below moved.
        // The two group counts (6472 lunar-witnessing, 4010 solar-only) and
        // every day-number are BYTE-IDENTICAL — the reckoning ARITHMETIC did
        // not move, only the drawn names, and which peoples are organized
        // (ten of fifteen) is unchanged too.
        // THE TRENCHER re-pin (2026-09-12, the repair pass, ledger #25/#26):
        // THIRTEEN organized of nineteen placed. Fovfav (duergar) and Wodbog
        // (kuo-toa) each GAIN their three-line priesthood run — the exact
        // inverse of the shape gnoll's and desert-elf's losses took above,
        // and the prose face of `diachronic::LADDER_TABLE` moving seed 1's
        // duergar and kuo-toa from `Counted` to `Predictive` in this same
        // commit. Cause: Task 4 routes `EnergySource::DetritalImport` onto
        // the `DETRITUS` axis, which had been flat on all land, so the
        // underworld peoples' congregations scale differently. Every other
        // people's autonym, count and day is BYTE-IDENTICAL and the two group
        // counts (6472 lunar-witnessing, 4010 solar-only) do not move at all:
        // the reckoning ARITHMETIC did not move, only who has a priesthood to
        // report it. Six lines added, zero changed, zero removed.
        let seed1 = render_volume(&generated(1));
        assert_eq!(
            seed1.reckoning[1].lines,
            vec![
                "Among the Šzoašzo, the sky has darkened, now and again.".to_string(),
                "Among the Bao, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Bao numbers the darkenings: 6472.".to_string(),
                "The next darkening, it teaches, comes on day 36531.".to_string(),
                "The Bao's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
                "Among the Bazap, the sky has darkened, now and again.".to_string(),
                "Among the Ruašre, the sky has darkened, now and again.".to_string(),
                "Among the Roašror, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Roašror numbers the darkenings: 6472.".to_string(),
                "The next darkening, it teaches, comes on day 36531.".to_string(),
                "The Roašror's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
                "Among the Dazha, the sky has darkened, now and again.".to_string(),
                "Among the Wakdao, the sky has darkened, now and again.".to_string(),
                "Among the Qwootoqo, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Qwootoqo numbers the darkenings: 4010.".to_string(),
                "The next darkening, it teaches, comes on day 36531.".to_string(),
                "The Qwootoqo's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
                "Among the Xabxat, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Xabxat numbers the darkenings: 4010.".to_string(),
                "The next darkening, it teaches, comes on day 36531.".to_string(),
                "The Xabxat's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
                "Among the Šdoašded, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Šdoašded numbers the darkenings: 6472.".to_string(),
                "The next darkening, it teaches, comes on day 36531.".to_string(),
                "The Šdoašded's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
                "Among the Dashav, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Dashav numbers the darkenings: 4010.".to_string(),
                "The next darkening, it teaches, comes on day 36531.".to_string(),
                "The Dashav's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
                "Among the Dweowbaw, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Dweowbaw numbers the darkenings: 4010.".to_string(),
                "The next darkening, it teaches, comes on day 36531.".to_string(),
                "The Dweowbaw's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
                "Among the Naavea, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Naavea numbers the darkenings: 4010.".to_string(),
                "The next darkening, it teaches, comes on day 36531.".to_string(),
                "The Naavea's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
                "Among the Zukzhov, the sky has darkened, now and again.".to_string(),
                "Among the Ngongo, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Ngongo numbers the darkenings: 6472.".to_string(),
                "The next darkening, it teaches, comes on day 36531.".to_string(),
                "The Ngongo's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
                "Among the Wodbog, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Wodbog numbers the darkenings: 6472.".to_string(),
                "The next darkening, it teaches, comes on day 36531.".to_string(),
                "The Wodbog's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
                "Among the Dazhaf, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Dazhaf numbers the darkenings: 6472.".to_string(),
                "The next darkening, it teaches, comes on day 36531.".to_string(),
                "The Dazhaf's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
                "Among the Xaoxa, the sky has darkened, now and again.".to_string(),
                "Among the Šzuašzez, the sky has darkened, now and again.".to_string(),
                "Among the Šroašror, the sky has darkened, now and again.".to_string(),
                "Among the Kxsonkxzo, the sky has darkened, now and again.".to_string(),
                "Among the Nao, the sky has darkened, now and again.".to_string(),
                "Among the Peqgop, the sky has darkened, now and again.".to_string(),
                "Among the Sruasrer, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Sruasrer numbers the darkenings: 4010.".to_string(),
                "The next darkening, it teaches, comes on day 36531.".to_string(),
                "The Sruasrer's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
            ],
            "seed 1: thirteen organized priesthoods of nineteen placed peoples \
             (desert-dwarf, desert-elf, gnoll, mountain-dwarf, sea-elf and snow-elf are \
             folk-only — duergar and kuo-toa joined the organized set at The Trencher's \
             repair pass); the lunar-witnessing group numbers 6472, the solar-only \
             group 4010"
        );
        assert_eq!(
            seed1.reckoning[1].margin,
            vec![
                "In truth, the Bao's priesthood taught the darkening would come on day 36528; it came on day 36522 instead.".to_string(),
                "In truth, the Roašror's priesthood taught the darkening would come on day 36528; it came on day 36522 instead.".to_string(),
                "In truth, the Qwootoqo's priesthood taught the darkening would come on day 36528; it came on day 36522 instead.".to_string(),
                "In truth, the Xabxat's priesthood taught the darkening would come on day 36528; it came on day 36522 instead.".to_string(),
                "In truth, the Šdoašded's priesthood taught the darkening would come on day 36528; it came on day 36522 instead.".to_string(),
                "In truth, the Dashav's priesthood taught the darkening would come on day 36528; it came on day 36522 instead.".to_string(),
                "In truth, the Dweowbaw's priesthood taught the darkening would come on day 36528; it came on day 36522 instead.".to_string(),
                "In truth, the Naavea's priesthood taught the darkening would come on day 36528; it came on day 36522 instead.".to_string(),
                "In truth, the Ngongo's priesthood taught the darkening would come on day 36528; it came on day 36522 instead.".to_string(),
                "In truth, the Wodbog's priesthood taught the darkening would come on day 36528; it came on day 36522 instead.".to_string(),
                "In truth, the Dazhaf's priesthood taught the darkening would come on day 36528; it came on day 36522 instead.".to_string(),
                "In truth, the Sruasrer's priesthood taught the darkening would come on day 36528; it came on day 36522 instead.".to_string(),
                "In truth, the darkenings of the first hundred years number 6472.".to_string(),
            ],
            "seed 1: each organized priesthood carries a live prediction crisis, and the true \
             count (6472) exceeds the solar-only cultures' held count"
        );

        // Seed 2: six organized peoples (Booqboo, Klalsha, Meepmoe, Weeqwoe,
        // Foetjee, Ngka); the lunar-witnessing group (bugbear/Booqboo,
        // gnoll/Klalsha, kobold/Ngka) numbers 81, the solar-only trio
        // (goblin/Meepmoe, hobgoblin/Weeqwoe, human/Foetjee) 49.
        //
        // The Delvers re-pin (C2c, 2026-08-07): nine placed, EIGHT organized —
        // only desert-dwarf is folk-only here, matching `LADDER_TABLE`'s
        // seed-2 `Counted` row. Re-measured wholesale from the live volume
        // after the roster was cut to three (spec §11). The SURVIVING
        // dwarves' autonyms moved with the cut (gully-dwarf and hill-dwarf
        // are `Njanjo`/`Wanwo` here, not `Jajamjajo`/`Wawabwawo`) because
        // removing two names from accession cohort 9 re-sorts the cohort and
        // moves the remaining dwarf concepts' proto-roots; every NON-dwarf
        // autonym and count is byte-identical.
        //
        // The Range re-pin (task 4, 2026-08-09): SEVEN organized. Gnoll
        // (`Loshjo` here) loses its priesthood run and keeps its "Among the"
        // line, exactly as it does at seed 1 — the biome-affinity row is the
        // cause at both seeds, and no other people's autonym, count or day
        // moved.
        //
        // The Radiation, task 6 (C2d, 2026-08-10): FIFTEEN placed, NINE
        // organized — desert-dwarf, desert-elf, gnoll, high-elf, sea-elf and
        // snow-elf are folk-only, matching `LADDER_TABLE`'s six seed-2
        // `Counted` rows exactly. Unlike seeds 1 and 3, seed 2 moves in BOTH
        // directions: drow and hobgoblin GAIN priesthood runs here while three
        // elves lose theirs. Both witnessed counts (81 lunar-witnessing, 49
        // solar-only) and the taught day are unchanged, so nothing about the
        // reckoning arithmetic moved — only which flagships cleared the caste
        // threshold. Re-measured wholesale from the live volume.
        //
        // The Glasshouse (decision 0134, the terrain epoch): TEN organized —
        // the Xeqmoq (high-elf) GAIN a priesthood run, and nothing else at
        // this seed moves; the folk-only set drops to five. Both witnessed counts (81 and 49) and the taught day are
        // again unchanged, so once more only the caste threshold moved and not
        // the reckoning arithmetic. Seed 2 gaining one while seed 1 loses two
        // is the expected shape of a terrain epoch: new coastlines redistribute
        // where peoples settle and how large a congregation each gathers, which
        // pushes some flagships over the organized rung and others under it.
        // THE GLASSHOUSE close (`k` settled at 0.30): still FIFTEEN placed and
        // still TEN organized, and the placed roster is byte-identical — but
        // the organized set EXCHANGES one member: Kxoqboq out, Kopnop in.
        // Both witnessed counts (81 and 49) and the taught day (36337) are
        // unchanged for the fourth consecutive re-pin, so the reckoning
        // arithmetic is once again untouched and only the caste threshold
        // moved.
        //
        // An exchange rather than a net gain or loss is a shape worth naming.
        // The previous three re-pins all moved the COUNT, which made "the
        // threshold moved" easy to see; here the count is stable at ten and a
        // cardinality check would have reported no change at all, while two
        // peoples' religious lives in fact swapped. That is the argument for
        // pinning the LINES rather than the tallies.
        // The Burr re-pin (Task 4): admitting an alveolar trill as an
        // ordinary manner reseeds every family's root assignment
        // (`ROOT_EPOCH` v3 -> v4, a decision recorded at this campaign's close), so every autonym below
        // moved. The group counts (81 lunar-witnessing, 49 solar-only) and
        // every day-number are BYTE-IDENTICAL.
        //
        // The Burr re-pin (Task 5): the sonorant floor reseeds phonotactics'
        // manner list once more, so every autonym below moved again. The
        // group counts and every day-number are again BYTE-IDENTICAL.
        //
        // The Burr re-pin (Task 9): the floor becomes bundle-conditioned, so
        // only `sonorant-open` (elf) keeps it — bugbear's and kobold's
        // autonyms (both `concatenative`) revert to their pre-Task-5 draw;
        // every other tongue in this block happened to land unchanged.
        // Group counts and every day-number are again BYTE-IDENTICAL.
        //
        // The Burr re-pin (Tasks 13/15, at the absorb of main): the dwarf
        // family becomes templatic and every family gains a per-bundle
        // orthography, so any dwarf/elf autonym in this block moves once more
        // (the spelling of `ŋ`/`ʃ` for the elves, the root shape for the
        // dwarves). Group counts and day-numbers remain BYTE-IDENTICAL —
        // re-pinned from the merged run, values below are its output.
        //
        // THE UNDERWORLD (Task 8, spec §4.6's node-index re-key): still
        // FIFTEEN placed, and ELEVEN organized — the Kxoqboq gain a priesthood
        // run, and nothing else at this seed moves. Both witnessed counts (81
        // and 49) and the taught day (36337) are unchanged for the fifth
        // consecutive re-pin, so the reckoning arithmetic is untouched again
        // and only the caste threshold moved.
        //
        // **The Kxoqboq are the drow**, and this line is one of exactly two
        // surfaces that moved in that whole task. `worldgen::diachronic`'s
        // 75-row `LADDER_TABLE` moved exactly ONE row in the same run — seed 2
        // drow, `Counted` -> `Predictive`, 81 witnessed and unchanged — and a
        // culture crossing to `Predictive` is a culture gaining a priesthood.
        // Two surfaces, one fact, each re-pinned from its own scaffold and
        // neither from the other. Re-keying the deep-history node index on
        // `(cell, rung)` takes drow out of the competition for surface cells;
        // its congregation at this seed is what crossed the caste threshold.
        //
        // THE UNDERWORLD (Task 9, the genus join): back to TEN organized —
        // **the Kxoqboq lose the priesthood run they gained one task ago**,
        // and nothing else at this seed moves. Both witnessed counts (81 and
        // 49) and the taught day (36337) are unchanged for the sixth
        // consecutive re-pin. Same people, same lever, opposite direction:
        // Task 8 moved drow's competition for surface cells by re-keying the
        // node index, and Task 9 moves it again by repairing `chamber_fit`'s
        // genus join, so drow seats at a different rung in karst and fracture
        // columns and its seed-2 congregation falls back under the caste
        // threshold. The margin line goes with it — a people with no
        // priesthood teaches nothing to be wrong about.
        //
        // **The paired `diachronic` row moves back too**, and the pairing is
        // the point: `LADDER_TABLE`'s seed-2 drow row returns `Predictive` ->
        // `Counted`, 81 witnessed and unchanged, re-pinned from its own
        // scaffold rather than from this one. A single fact reaching two
        // instruments that agree without consulting each other is the check
        // this comment has been building across two campaigns.
        //
        // THE TRENCHER (Task 4, 2026-09-11): ELEVEN organized again — **the
        // Saoza gain a priesthood**, and the Saoza are the drow, for the third
        // time in three campaigns and by the same lever. Nothing else at this
        // seed moves: every other people's lines are byte-identical, and the
        // witnessed counts (81 and 49) and the taught day (36337) are
        // unchanged for the SEVENTH consecutive re-pin, so the reckoning
        // arithmetic is untouched once more and only the caste threshold
        // moved.
        //
        // **The chain, traced and measured rather than asserted** (it is not a
        // population threshold, which is the wrong guess this paragraph exists
        // to forestall — drow's flagship population went DOWN, 32 -> 29):
        //
        //   supply -> `per_species_capacity_at` (drow is the only
        //   `HabitatRealm::Subterranean` PEOPLE, and Task 4 routes
        //   `EnergySource::DetritalImport` onto the `DETRITUS` axis, on which
        //   drow carries 0.50 — the largest share of any people)
        //   -> the deep-history bake's occupation skeleton
        //   -> WHICH VERTEX is the drow's chief settlement
        //   -> that vertex's `climate.moisture_at`
        //   -> `surplus = fertility(biome_class) * moisture`
        //      (`windows/worldgen/src/lib.rs`, the culture+religion stage)
        //   -> `culture::structure`'s `env.surplus > 0.4` shaman rung
        //   -> `SocietySummary::has_priesthood`
        //   -> `religion::genesis`'s `cult-form = "organized"`
        //   -> `chorus::doctrine_from`'s SOC-1 gate
        //   -> `LadderRung::Numbered`/`Predictive` instead of `Counted`
        //   -> these three lines.
        //
        // Measured at seed 2, before and after, with everything else held:
        // the drow flagship moves from vertex **27215** (moisture 0.437847,
        // Forest, surplus **0.394063** — under the 0.4 rung, castes
        // `["delver", "matron"]`, `cult-form` `folk`) to vertex **26015**
        // (moisture 0.542484, Forest, surplus **0.488236** — over it, castes
        // `["delver", "blademaster", "dark-speaker", "matron"]`, `cult-form`
        // `organized`). `dark-speaker` is the drow's own shaman-rung word, so
        // the priesthood check fires on it exactly as goblin's `shaman` does.
        //
        // Surface peoples' flagships move at this seed too (bugbear
        // 33224 -> 21217, gnoll 40507 -> 3143, hill-dwarf 30124 -> 21438,
        // kobold 30657 -> 12951) even though SURFACE scoring is bit-identical
        // under this change — because the bake is one competition for
        // vertices across all peoples, the same mechanism Underworld Tasks 8
        // and 9 moved from the other side. None of them crosses a caste
        // threshold, which is why only the Saoza's block changes here.
        // THE TRENCHER re-pin (2026-09-12, the repair pass, ledger #25/#26):
        // THIRTEEN organized of nineteen placed at seed 2. The Saoza (drow)
        // GAIN their three-line priesthood run and their one crisis line;
        // nothing else at this seed moves, and the witnessed counts (81 and
        // 49) and the taught day (36337) are unchanged again, so the
        // reckoning arithmetic is untouched and only the caste threshold
        // moved. Corroborated by `diachronic::LADDER_TABLE` reading seed 2's
        // drow at `Predictive` where it read `Counted`.
        let seed2 = render_volume(&generated(2));
        assert_eq!(
            seed2.reckoning[1].lines,
            vec![
                "Among the Drakro, the sky has darkened, now and again.".to_string(),
                "Among the Koodzhoo, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Koodzhoo numbers the darkenings: 81.".to_string(),
                "The next darkening, it teaches, comes on day 36337.".to_string(),
                "The Koodzhoo's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
                "Among the Takaz, the sky has darkened, now and again.".to_string(),
                "Among the Boba, the sky has darkened, now and again.".to_string(),
                "Among the Saoza, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Saoza numbers the darkenings: 81.".to_string(),
                "The next darkening, it teaches, comes on day 36337.".to_string(),
                "The Saoza's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
                "Among the Pagam, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Pagam numbers the darkenings: 81.".to_string(),
                "The next darkening, it teaches, comes on day 36337.".to_string(),
                "The Pagam's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
                "Among the Ratra, the sky has darkened, now and again.".to_string(),
                "Among the Peerzhoe, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Peerzhoe numbers the darkenings: 49.".to_string(),
                "The next darkening, it teaches, comes on day 36337.".to_string(),
                "The Peerzhoe's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
                "Among the Bapa, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Bapa numbers the darkenings: 49.".to_string(),
                "The next darkening, it teaches, comes on day 36337.".to_string(),
                "The Bapa's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
                "Among the Moma, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Moma numbers the darkenings: 81.".to_string(),
                "The next darkening, it teaches, comes on day 36337.".to_string(),
                "The Moma's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
                "Among the Papan, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Papan numbers the darkenings: 49.".to_string(),
                "The next darkening, it teaches, comes on day 36337.".to_string(),
                "The Papan's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
                "Among the Geerjoe, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Geerjoe numbers the darkenings: 49.".to_string(),
                "The next darkening, it teaches, comes on day 36337.".to_string(),
                "The Geerjoe's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
                "Among the Leoglo, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Leoglo numbers the darkenings: 49.".to_string(),
                "The next darkening, it teaches, comes on day 36337.".to_string(),
                "The Leoglo's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
                "Among the Sue, the sky has darkened, now and again.".to_string(),
                "Among the Gnga, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Gnga numbers the darkenings: 81.".to_string(),
                "The next darkening, it teaches, comes on day 36337.".to_string(),
                "The Gnga's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
                "Among the Famfaam, the sky has darkened, now and again.".to_string(),
                "Among the Takan, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Takan numbers the darkenings: 81.".to_string(),
                "The next darkening, it teaches, comes on day 36337.".to_string(),
                "The Takan's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
                "Among the Qo, the sky has darkened, now and again.".to_string(),
                "Among the Muma, the sky has darkened, now and again.".to_string(),
                "Among the Nona, the sky has darkened, now and again.".to_string(),
                "Among the Kxaapxoa, the sky has darkened, now and again.".to_string(),
                "Among the Fafmex, the sky has darkened, now and again.".to_string(),
                "Among the Shaalpnge, the sky has darkened, now and again.".to_string(),
                "Among the Ṅuṅa, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Ṅuṅa numbers the darkenings: 49.".to_string(),
                "The next darkening, it teaches, comes on day 36337.".to_string(),
                "The Ṅuṅa's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
            ]
        );
        assert_eq!(
            seed2.reckoning[1].margin,
            vec![
                "In truth, the Koodzhoo's priesthood taught the darkening would come on day 35328; it came on day 35609 instead.".to_string(),
                "In truth, the Saoza's priesthood taught the darkening would come on day 35328; it came on day 35609 instead.".to_string(),
                "In truth, the Pagam's priesthood taught the darkening would come on day 35328; it came on day 35609 instead.".to_string(),
                "In truth, the Peerzhoe's priesthood taught the darkening would come on day 35328; it came on day 35609 instead.".to_string(),
                "In truth, the Bapa's priesthood taught the darkening would come on day 35328; it came on day 35609 instead.".to_string(),
                "In truth, the Moma's priesthood taught the darkening would come on day 35328; it came on day 35609 instead.".to_string(),
                "In truth, the Papan's priesthood taught the darkening would come on day 35328; it came on day 35609 instead.".to_string(),
                "In truth, the Geerjoe's priesthood taught the darkening would come on day 35328; it came on day 35609 instead.".to_string(),
                "In truth, the Leoglo's priesthood taught the darkening would come on day 35328; it came on day 35609 instead.".to_string(),
                "In truth, the Gnga's priesthood taught the darkening would come on day 35328; it came on day 35609 instead.".to_string(),
                "In truth, the Takan's priesthood taught the darkening would come on day 35328; it came on day 35609 instead.".to_string(),
                "In truth, the Ṅuṅa's priesthood taught the darkening would come on day 35328; it came on day 35609 instead.".to_string(),
                "In truth, the darkenings of the first hundred years number 81.".to_string(),
            ]
        );

        // The Delvers re-pin (C2c, 2026-08-07): nine placed, SEVEN organized —
        // desert-dwarf and kobold are folk-only at this seed, matching
        // `LADDER_TABLE`'s seed-3 `Counted` rows. Hobgoblin, folk-only under
        // both the pre-campaign and the five-dwarf rosters, GAINED a
        // priesthood when the roster was cut to three (spec §11); the caste
        // threshold is not monotone in roster size, which the ladder table's
        // own comment records. Re-measured wholesale from the live volume.
        //
        // Seed 3: six organized peoples (Tashoo, Jpajjpa, Ztasoe,
        // Ztashoeg, Shoammoem, Sqaojxo); the lunar-witnessing group
        // (bugbear/Tashoo, gnoll/Jpajjpa, kobold/Sqaojxo) numbers 53, the
        // solar-only trio (goblin/Ztasoe, hobgoblin/Ztashoeg,
        // human/Shoammoem) 32.
        //
        // Re-pinned at F7 (The Witness, 2026-07-30): gating `Tonogenesis` on
        // a prior merger reseeds every cascade — bugbear `Doozka` ->
        // `Shdoozga`, gnoll `Jpojjpo` -> `Pojjpoj`, kobold `Jjojjjo` ->
        // `Jojjjo`; goblin `Sdoozka` and hobgoblin `Shtoozka` happened to
        // land unchanged. Same story: only drawn word forms moved, the day
        // numbers and counts (53/32) are untouched.
        //
        // Re-pinned again at Task 8b (The Witness, same campaign): the
        // phonology-hosting gate in `draw_rule` reseeds every cascade once
        // more — bugbear `Shdoozga` -> `Doozka`, gnoll `Pojjpoj` ->
        // `Jpojjpoj`, goblin `Sdoozka` -> `Xofozho`, hobgoblin `Shtoozka` ->
        // `Toozka`, kobold `Jojjjo` -> `Jjojjjo`. Same story once more: only
        // drawn word forms moved, the day numbers and counts (53/32) are
        // untouched.
        //
        // Re-pinned a further time absorbing The Watershed's sonority merge
        // (independently reseeding the same cascade roster) — gnoll
        // `Jpojjpoj` -> `Pjojpjoj`, goblin `Xofozho` -> `Zhooqsa`, hobgoblin
        // `Toozka` -> `Zhooqsha`; bugbear and kobold happened to land
        // unchanged. Same story once more: only drawn word forms moved, the
        // day numbers and counts (53/32) are untouched.
        //
        // The Range re-pin (task 4, 2026-08-09): FIVE organized of nine placed,
        // and this is the one seed where the re-pin is NOT confined to gnoll.
        // Two priesthood runs go: gnoll's (`Pjojpjoj`) and **hobgoblin's**
        // (`Zhooqsha`) — the same hobgoblin the paragraph above records as
        // having GAINED a priesthood when the dwarf roster was cut. Only
        // gnoll carries a biome affinity, so hobgoblin's loss is the
        // competitive cascade: the bake contests ground across eras, and
        // suppressing one people frees cells the rest re-contest, which can
        // push a third people back across the caste threshold in either
        // direction (`windows/worldgen/tests/range_readout.rs` measures the
        // same cascade on seed 7, where gnoll's own count does not move at all
        // and bugbear triples). Every autonym, day number and count here is
        // otherwise unchanged.
        //
        // The Radiation, task 6 (C2d, 2026-08-10): FIFTEEN placed, NINE
        // organized — desert-dwarf, desert-elf, gnoll, hobgoblin, kobold and
        // sea-elf are folk-only, matching `LADDER_TABLE`'s six seed-3
        // `Counted` rows exactly. Hobgoblin appears in that list for the
        // second time in three re-pins and kobold for the first since The
        // Delvers, which is the "moves both ways" reading this file and
        // `diachronic.rs` have both carried since The Tense — the caste
        // threshold is crossed in both directions by a placement change, and
        // it is not monotone in roster size. Both counts (53/32) and the
        // taught day are unchanged again.
        // THE GLASSHOUSE close (`k` settled at 0.30): seed 3 gains a net ONE
        // organized people — Pnaaknoo drops its priesthood run while Pmaaxmoe
        // and Zhooqsha gain theirs. Both counts (53/32) and the taught day
        // (36125) are unchanged, the same as at seeds 1 and 2, so all three
        // seeds agree that this campaign moved the caste threshold and left
        // the reckoning arithmetic alone.
        //
        // Seed 2 exchanging with no net change while seed 3 nets +1 is the
        // "moves both ways, and not monotone in roster size" reading this
        // file has carried since The Tense, now observed a further time.
        // THE TRENCHER re-pin (2026-09-12, the repair pass, ledger #25/#26):
        // TEN organized of nineteen placed at seed 3, and this seed moves the
        // OTHER way — the Mo (svirfneblin) LOSE their three-line priesthood
        // run and their one crisis line, the precise shape of a culture that
        // lost its organized cult rather than its existence. Corroborated by
        // `diachronic::LADDER_TABLE` reading seed 3's svirfneblin at
        // `Counted` where it read `Predictive`. Both directions on one
        // campaign is this table's own long-standing pattern (see the
        // LADDER_TABLE header: five gains and four losses across the five
        // seeds), and is why nothing here reads as "the change made cults
        // more common".
        let seed3 = render_volume(&generated(3));
        // The Glasshouse (decision 0134, the terrain epoch): SEVEN organized,
        // the largest move of the three seeds. Drow, hobgoblin and wood-elf
        // drop to folk-only while snow-elf gains a priesthood, so the
        // folk-only set goes six -> eight. The witnessed counts (53 lunar,
        // 32 solar) and the taught day are unchanged here too: across all
        // three seeds this epoch moved only WHICH flagships clear the caste
        // threshold, never the reckoning arithmetic — which is the useful
        // reading, since the two are independent mechanisms and a terrain
        // change should touch exactly one of them.
        //
        // The Glasshouse, Stage B Task 4 (the thermostat): NINE organized.
        // Drow (Fnaaxnoo) and wood-elf (Daaxdue) both regain priesthoods,
        // matching `diachronic.rs`'s LADDER_TABLE seed-3 rows moving
        // Counted -> Predictive for exactly those two kinds. The folk-only
        // set drops eight -> six (desert-dwarf, desert-elf, gnoll, high-elf,
        // hobgoblin, sea-elf). The witnessed counts (53 lunar, 32 solar) and
        // the taught day are unchanged again.
        // The Burr re-pin (Task 4): admitting an alveolar trill as an
        // ordinary manner reseeds every family's root assignment
        // (`ROOT_EPOCH` v3 -> v4, a decision recorded at this campaign's close), so every autonym below
        // moved (one, `Zozha`, happens to redraw to itself — measured, not
        // assumed). The two group counts (53 lunar-witnessing, 32
        // solar-only), every day-number and which nine of fifteen peoples
        // are organized are BYTE-IDENTICAL.
        //
        // The Burr re-pin (Task 9): the floor becomes bundle-conditioned;
        // the one affected family here (kobold, `concatenative`) reverts to
        // its pre-Task-5 autonym, `Doba` -> `Zozha` — coincidentally the
        // same string the Task 4 re-pin above already used for a different
        // people, not a collision (each people's line is independent).
        // Every other tongue in this block, and the two group counts,
        // remain BYTE-IDENTICAL.
        assert_eq!(
            seed3.reckoning[1].lines,
            vec![
                "Among the Dofdo, the sky has darkened, now and again.".to_string(),
                "Among the Dokdoo, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Dokdoo numbers the darkenings: 53.".to_string(),
                "The next darkening, it teaches, comes on day 36125.".to_string(),
                "The Dokdoo's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
                "Among the Bafak, the sky has darkened, now and again.".to_string(),
                "Among the Rupre, the sky has darkened, now and again.".to_string(),
                "Among the Šrobro, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Šrobro numbers the darkenings: 53.".to_string(),
                "The next darkening, it teaches, comes on day 36125.".to_string(),
                "The Šrobro's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
                "Among the Gafag, the sky has darkened, now and again.".to_string(),
                "Among the Plawplaw, the sky has darkened, now and again.".to_string(),
                "Among the Rekroeg, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Rekroeg numbers the darkenings: 32.".to_string(),
                "The next darkening, it teaches, comes on day 36125.".to_string(),
                "The Rekroeg's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
                "Among the Gapag, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Gapag numbers the darkenings: 32.".to_string(),
                "The next darkening, it teaches, comes on day 36125.".to_string(),
                "The Gapag's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
                "Among the Šzopze, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Šzopze numbers the darkenings: 53.".to_string(),
                "The next darkening, it teaches, comes on day 36125.".to_string(),
                "The Šzopze's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
                "Among the Gapap, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Gapap numbers the darkenings: 32.".to_string(),
                "The next darkening, it teaches, comes on day 36125.".to_string(),
                "The Gapap's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
                "Among the Zekzoe, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Zekzoe numbers the darkenings: 32.".to_string(),
                "The next darkening, it teaches, comes on day 36125.".to_string(),
                "The Zekzoe's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
                "Among the Vaar, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Vaar numbers the darkenings: 32.".to_string(),
                "The next darkening, it teaches, comes on day 36125.".to_string(),
                "The Vaar's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
                "Among the Jojozo, the sky has darkened, now and again.".to_string(),
                "Among the Dzhaojxo, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Dzhaojxo numbers the darkenings: 53.".to_string(),
                "The next darkening, it teaches, comes on day 36125.".to_string(),
                "The Dzhaojxo's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
                "Among the Vnagva, the sky has darkened, now and again.".to_string(),
                "Among the Gava, the sky has darkened, now and again.".to_string(),
                "Among the Shango, the sky has darkened, now and again.".to_string(),
                "Among the Szubze, the sky has darkened, now and again.".to_string(),
                "Among the Šropro, the sky has darkened, now and again.".to_string(),
                "Among the Mo, the sky has darkened, now and again.".to_string(),
                "Among the Leqsla, the sky has darkened, now and again.".to_string(),
                "Among the Leolngjee, the sky has darkened, now and again.".to_string(),
                "Among the Rutre, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Rutre numbers the darkenings: 32.".to_string(),
                "The next darkening, it teaches, comes on day 36125.".to_string(),
                "The Rutre's own priesthood taught wrongly, and could be shown wrong by any who kept their own count.".to_string(),
            ],
            "seed 3: ten organized priesthoods of nineteen placed peoples (desert-dwarf, \
             desert-elf, duergar, gnoll, kuo-toa, mountain-dwarf, sea-elf, snow-elf and \
             svirfneblin are folk-only); the lunar-witnessing group numbers 53, the \
             solar-only group 32"
        );
        assert_eq!(
            seed3.reckoning[1].margin,
            vec![
                "In truth, the Dokdoo's priesthood taught the darkening would come on day 35583; it came on day 35030 instead.".to_string(),
                "In truth, the Šrobro's priesthood taught the darkening would come on day 35583; it came on day 35030 instead.".to_string(),
                "In truth, the Rekroeg's priesthood taught the darkening would come on day 35583; it came on day 35030 instead.".to_string(),
                "In truth, the Gapag's priesthood taught the darkening would come on day 35583; it came on day 35030 instead.".to_string(),
                "In truth, the Šzopze's priesthood taught the darkening would come on day 35583; it came on day 35030 instead.".to_string(),
                "In truth, the Gapap's priesthood taught the darkening would come on day 35583; it came on day 35030 instead.".to_string(),
                "In truth, the Zekzoe's priesthood taught the darkening would come on day 35583; it came on day 35030 instead.".to_string(),
                "In truth, the Vaar's priesthood taught the darkening would come on day 35583; it came on day 35030 instead.".to_string(),
                "In truth, the Dzhaojxo's priesthood taught the darkening would come on day 35583; it came on day 35030 instead.".to_string(),
                "In truth, the Rutre's priesthood taught the darkening would come on day 35583; it came on day 35030 instead.".to_string(),
                "In truth, the darkenings of the first hundred years number 53.".to_string(),
            ]
        );
    }

    /// The Vessel Stitch T1, the accessor law (spec §4.4): `reckoning_at`
    /// is the CLI `--at` path's own implementation (one function, two
    /// callers) — it must equal `reckoning_epochs`'s per-epoch output for
    /// the same day, not just resemble it. Checked against seed 1's fixed
    /// pair (day 0's empty arm, day 36525's four-line/two-margin epoch —
    /// the Corrigendum T3 added the crisis line to the margin — both
    /// pinned above in `the_reckoning_renders_the_epoch_pair`) by
    /// comparing `lines` exactly (the substantive per-culture registers —
    /// identical regardless of caller) while `heading` and the margin's
    /// leading phrase are lens-parameterized closed strings, not part of
    /// the law: the fixed pair's are the committed prose (`"In the first
    /// days"`/`"In the hundredth year"`, `"...of the first hundred
    /// years..."`), while `reckoning_at`'s are always the ad hoc `"At day
    /// ⟨N⟩"` / `"...by day ⟨N⟩..."` lens — only the CLI's `--at` stdout
    /// path ever sees them, never the committed artifact. The margin's
    /// trailing cardinal (the true count the phrase reports) IS part of
    /// the law and is pinned exact below, verified live. A third,
    /// non-preregistered day (20000) exercises the general case: some
    /// culture's ladder rung at an arbitrary day, not just the two frozen
    /// epochs.
    ///
    /// Re-pinned at The Burr (Task 9): the sonorant floor becomes
    /// bundle-conditioned, so desert dwarf, gully dwarf, hill dwarf, human
    /// and kobold (none of them `sonorant-open`) revert to their pre-
    /// Task-5 autonyms in the margin's crisis lines. The true count (6472,
    /// pinned in `the_reckoning_renders_the_epoch_pair`) and every other
    /// clause are unchanged.
    ///
    /// Re-pinned at The Burr (Task 15): per-bundle orthography moves the
    /// same margin autonyms again (elf/drow-family names additionally gain
    /// a `š` diacritic spelling). The true count (6472) and every other
    /// clause are unchanged.
    #[test]
    fn reckoning_at_matches_the_fixed_pair_and_renders_arbitrary_days() {
        let world = generated(1);
        let pair = render_volume(&world).reckoning;

        let day0 = reckoning_at(&world, hornvale_astronomy::StdInstant::new(0.0).unwrap());
        assert_eq!(
            day0.lines, pair[0].lines,
            "day 0 matches the fixed pair's empty arm"
        );
        assert_eq!(
            day0.margin, pair[0].margin,
            "the empty arm carries no margin, either way"
        );

        let day100 = reckoning_at(
            &world,
            hornvale_astronomy::StdInstant::new(RECKONING_EPOCH_2_DAY).unwrap(),
        );
        assert_eq!(
            day100.lines, pair[1].lines,
            "the hundredth year's per-culture lines match the fixed pair exactly"
        );
        // THE GLASSHOUSE re-pin (Stage B Task 4, the thermostat): the
        // Tedash's priesthood line is BACK, between Tngobknga's and
        // Dngovgngav's — the same people, read through the same surface as
        // `seed_1_doctrine_sections_render`'s high-elf rejoining the
        // organized set. Every other line is byte-identical.
        //
        // THE TRENCHER re-pin (2026-09-12, the repair pass, ledger #25/#26):
        // the Fovfav's (duergar) and the Wodbog's (kuo-toa) crisis lines are
        // NEW, after the Roašror's and the Ngongo's respectively — the same
        // two peoples, read through the same surface as
        // `seed_1_doctrine_sections_render`'s duergar and kuo-toa joining the
        // organized set, and the same two lines this campaign added to
        // `the_reckoning_renders_the_epoch_pair`'s own margin. The true count
        // (6472) and every other line are BYTE-IDENTICAL, which is the point
        // worth reading: this test's law is that `reckoning_at` equals
        // `reckoning_epochs` at the same day, and both surfaces gained
        // exactly the same two peoples.
        assert_eq!(
            day100.margin,
            vec![
                "In truth, the Bao's priesthood taught the darkening would come on day 36528; it came on day 36522 instead.".to_string(),
                "In truth, the Roašror's priesthood taught the darkening would come on day 36528; it came on day 36522 instead.".to_string(),
                "In truth, the Qwootoqo's priesthood taught the darkening would come on day 36528; it came on day 36522 instead.".to_string(),
                "In truth, the Xabxat's priesthood taught the darkening would come on day 36528; it came on day 36522 instead.".to_string(),
                "In truth, the Šdoašded's priesthood taught the darkening would come on day 36528; it came on day 36522 instead.".to_string(),
                "In truth, the Dashav's priesthood taught the darkening would come on day 36528; it came on day 36522 instead.".to_string(),
                "In truth, the Dweowbaw's priesthood taught the darkening would come on day 36528; it came on day 36522 instead.".to_string(),
                "In truth, the Naavea's priesthood taught the darkening would come on day 36528; it came on day 36522 instead.".to_string(),
                "In truth, the Ngongo's priesthood taught the darkening would come on day 36528; it came on day 36522 instead.".to_string(),
                "In truth, the Wodbog's priesthood taught the darkening would come on day 36528; it came on day 36522 instead.".to_string(),
                "In truth, the Dazhaf's priesthood taught the darkening would come on day 36528; it came on day 36522 instead.".to_string(),
                "In truth, the Sruasrer's priesthood taught the darkening would come on day 36528; it came on day 36522 instead.".to_string(),
                "In truth, the darkenings by day 36525 number 6472.".to_string(),
            ],
            "same true count (6472, pinned in the_reckoning_renders_the_epoch_pair) as the \
             fixed pair's margin, phrased through reckoning_at's own ad hoc lens; the crisis \
             lines are unaffected by the lens (they carry no epoch phrase)"
        );

        let mid = reckoning_at(
            &world,
            hornvale_astronomy::StdInstant::new(20_000.0).unwrap(),
        );
        assert!(
            !mid.heading.is_empty() && !mid.lines.is_empty(),
            "an arbitrary day renders: heading={:?} lines={:?}",
            mid.heading,
            mid.lines
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
    ///
    /// The Generalist re-pin (2026-08-03): human joins the coexistence
    /// stack as a sixth people, organized at seed 1 alongside the other
    /// five — `vol.lines`, `vol.tongue_lines`, `vol.tongue_gaps` and
    /// `goblin.emic` each gain one more human-authored entry ("The Ngeevnao
    /// are humans."/its human-tongue rendering/"human: gap — planet"/"The
    /// Ngeevnao are humans — neighbors."). The additivity property this
    /// test guards is unchanged; only the ground truth grew by one more
    /// peoples-line, same as every prior re-pin in this file's history.
    ///
    /// The Delvers re-pin (C2c, 2026-08-07): the three dwarves join as peoples
    /// seven through nine, so `vol.lines`, `vol.tongue_lines`,
    /// `vol.tongue_gaps` and `goblin.emic` each gain three entries. **Every
    /// pre-existing line is BYTE-IDENTICAL** — all twelve prior
    /// `tongue_lines` included, which is the stronger reading: three new
    /// tongues entered the cascade roster and perturbed no existing tongue's
    /// drawn word forms at all, unlike The Wearing / The Witness / The
    /// Watershed re-pins recorded below, which each moved some. The
    /// additivity property this test guards is untouched; only the ground
    /// truth grew.
    ///
    /// The Radiation re-pin (C2d, 2026-08-10): the six elves join as peoples
    /// ten through fifteen, so `vol.lines`, `vol.tongue_lines`,
    /// `vol.tongue_gaps` and both peoples' `emic` each gain six entries.
    /// **Every pre-existing line is BYTE-IDENTICAL** — all eighteen prior
    /// `tongue_lines` included, the same stronger reading The Delvers
    /// recorded: six new tongues entered the cascade roster and perturbed no
    /// existing tongue's drawn word forms at all. The additivity property this
    /// test guards is untouched; only the ground truth grew.
    ///
    /// The Burr re-pin (Task 4): admitting an alveolar trill as an ordinary
    /// manner (no longer gated behind the exotic-consonant capability,
    /// a decision recorded at this campaign's close) inserts extra candidate-consonant draws ahead of
    /// every species' phonology inventory, which reseeds `assign_proto_
    /// roots`'s draw for every family (`ROOT_EPOCH` v3 -> v4) — not only
    /// the families a trill lands in. **Unlike The Delvers and The
    /// Radiation above, no line is BYTE-IDENTICAL this time: every entry
    /// in `vol.lines` and `vol.tongue_lines` moved**, because the reseed
    /// happens ahead of every species' draw in the stream, not append-only
    /// at the roster's end the way adding a people is. The additivity
    /// property this test guards — that growing the peoples roster
    /// perturbs no other register's SHAPE — is still intact: same sixteen
    /// `vol.lines` entries, same fifteen tongues with their two-clause
    /// self-statements, same order and gloss-per-tongue pairing; only the
    /// drawn proper nouns and word forms differ.
    ///
    /// Re-pinned again at The Burr (Task 5): the sonorant floor reseeds
    /// phonotactics' manner list once more for every envelope it tops up —
    /// every proper noun in `vol.lines` and every line in `vol.tongue_lines`
    /// moved again. Same additivity property, same shape; only the drawn
    /// words differ.
    ///
    /// Re-pinned once more at The Burr (Task 9): the floor becomes
    /// bundle-conditioned, so only `sonorant-open` keeps it — desert dwarf,
    /// gully dwarf, hill dwarf, human and kobold revert to their pre-
    /// Task-5 draw wherever they appear (`vol.lines`, the tongue
    /// self-statements, and every doctrine register). Same additivity
    /// property, same shape; only the affected proper nouns and word forms
    /// moved back.
    ///
    /// Re-pinned at The Burr (Task 15): per-bundle orthography moves several
    /// dwarf and elf/drow word forms again, wherever they appear
    /// (`vol.lines`, the tongue self-statements, and every doctrine
    /// register) — the elf/drow-family forms additionally gain a `š`/`ṅ`
    /// diacritic spelling. Same additivity property, same shape; every
    /// English gloss in parentheses is byte-identical, and only the drawn
    /// proper nouns and word forms moved.
    #[test]
    fn the_additivity_law() {
        let vol = render_volume(&generated(1));

        assert_eq!(
            vol.lines,
            vec![
                "Booko is a planet with two moons, orbiting a yellow-white dwarf (F); its day lasts about 1.5 standard days.".to_string(),
                "The Šzoašzo are abyssal elfs.".to_string(),
                "The Bao are bugbears.".to_string(),
                "The Bazap are desert dwarfs.".to_string(),
                "The Ruašre are desert elfs.".to_string(),
                "The Roašror are drows.".to_string(),
                "The Dazha are duergars.".to_string(),
                "The Wakdao are gnolls.".to_string(),
                "The Qwootoqo are goblins.".to_string(),
                "The Xabxat are gully dwarfs.".to_string(),
                "The Šdoašded are high elfs.".to_string(),
                "The Dashav are hill dwarfs.".to_string(),
                "The Dweowbaw are hobgoblins.".to_string(),
                "The Naavea are humans.".to_string(),
                "The Zukzhov are kelp tenders.".to_string(),
                "The Ngongo are kobolds.".to_string(),
                "The Wodbog are kuo toas.".to_string(),
                "The Dazhaf are mountain dwarfs.".to_string(),
                "The Xaoxa are reef masons.".to_string(),
                "The Šzuašzez are sea elfs.".to_string(),
                "The Šroašror are snow elfs.".to_string(),
                "The Kxsonkxzo are svirfneblins.".to_string(),
                "The Nao are tritons.".to_string(),
                "The Peqgop are vent commensals.".to_string(),
                "The Sruasrer are wood elfs.".to_string(),
            ]
        );
        assert_eq!(
            vol.tongue_lines,
            vec![
                "Xjaṅ Šzoašzo Šzozszoo. (in the abyssal-elf tongue: \"The Šzoašzo are abyssal elfs.\")".to_string(),
                "Xjaṅ Booko Zoo. (in the abyssal-elf tongue: \"Booko is the earth.\")".to_string(),
                "Xngatboa Bao Boobao. (in the bugbear tongue: \"The Bao are bugbears.\")".to_string(),
                "Xngatboa Booko Booko. (in the bugbear tongue: \"Booko is the earth.\")".to_string(),
                "Bazap Qangaz. (in the desert-dwarf tongue: \"The Bazap are desert dwarfs.\")".to_string(),
                "Booko Shazag. (in the desert-dwarf tongue: \"Booko is the earth.\")".to_string(),
                "Ruašre Reursruureu Raunru. (in the desert-elf tongue: \"The Ruašre are desert elfs.\")".to_string(),
                "Booko Rooreu Raunru. (in the desert-elf tongue: \"Booko is the earth.\")".to_string(),
                "Roašror Rooršraar Ṅro. (in the drow tongue: \"The Roašror are drows.\")".to_string(),
                "Booko Roor Ṅro. (in the drow tongue: \"Booko is the earth.\")".to_string(),
                "Dazha Sasakga. (in the duergar tongue: \"The Dazha are duergars.\")".to_string(),
                "Booko Sazhakga. (in the duergar tongue: \"Booko is the earth.\")".to_string(),
                "Wakdao Paab Loadwaawoo. (in the gnoll tongue: \"The Wakdao are gnolls.\")".to_string(),
                "Booko Paab Laakaa. (in the gnoll tongue: \"Booko is the earth.\")".to_string(),
                "Ka Jookaojo Qwootoqo. (in the goblin tongue: \"The Qwootoqo are goblins.\")".to_string(),
                "Ka Jejoeke Booko. (in the goblin tongue: \"Booko is the earth.\")".to_string(),
                "Xabxat Bazha Xat. (in the gully-dwarf tongue: \"The Xabxat are gully dwarfs.\")".to_string(),
                "Booko Shazha Xat. (in the gully-dwarf tongue: \"Booko is the earth.\")".to_string(),
                "Šdoašded Šdeo Daddaad Dlea. (in the high-elf tongue: \"The Šdoašded are high elfs.\")".to_string(),
                "Booko Šdeo Dood Dlea. (in the high-elf tongue: \"Booko is the earth.\")".to_string(),
                "Dashav Batav Koab. (in the hill-dwarf tongue: \"The Dashav are hill dwarfs.\")".to_string(),
                "Booko Shashag Koab. (in the hill-dwarf tongue: \"Booko is the earth.\")".to_string(),
                "Dweowbaw Waawa Woa Woo. (in the hobgoblin tongue: \"The Dweowbaw are hobgoblins.\")".to_string(),
                "Booko Woeke Woa We. (in the hobgoblin tongue: \"Booko is the earth.\")".to_string(),
                "Naavea Vaozeezhoa Voosaa. (in the human tongue: \"The Naavea are humans.\")".to_string(),
                "Booko Zooveaseo Voosaa. (in the human tongue: \"Booko is the earth.\")".to_string(),
                "Tek Sod Zukzhov Deekzheukzeuv. (in the kelp-tender tongue: \"The Zukzhov are kelp tenders.\")".to_string(),
                "Tek Sod Booko Zhakzuokqoof. (in the kelp-tender tongue: \"Booko is the earth.\")".to_string(),
                "Ngongo Ngod Ngokngo. (in the kobold tongue: \"The Ngongo are kobolds.\")".to_string(),
                "Booko Ngod Sosho. (in the kobold tongue: \"Booko is the earth.\")".to_string(),
                "Wodbog Wodwod Vob. (in the kuo-toa tongue: \"The Wodbog are kuo toas.\")".to_string(),
                "Booko Wobwag Vob. (in the kuo-toa tongue: \"Booko is the earth.\")".to_string(),
                "Dazhaf Gazhapqoozh. (in the mountain-dwarf tongue: \"The Dazhaf are mountain dwarfs.\")".to_string(),
                "Booko Shazhagqaof. (in the mountain-dwarf tongue: \"Booko is the earth.\")".to_string(),
                "Zhoashoongshoo Xaoxa. (in the reef-mason tongue: \"The Xaoxa are reef masons.\")".to_string(),
                "Zhoaxo Booko. (in the reef-mason tongue: \"Booko is the earth.\")".to_string(),
                "Šzuašzez Šzazzuz. (in the sea-elf tongue: \"The Šzuašzez are sea elfs.\")".to_string(),
                "Booko Zooz. (in the sea-elf tongue: \"Booko is the earth.\")".to_string(),
                "Šroašror Trooršrar. (in the snow-elf tongue: \"The Šroašror are snow elfs.\")".to_string(),
                "Booko Roor. (in the snow-elf tongue: \"Booko is the earth.\")".to_string(),
                "Kxzhankxzankxa Kxsonkxzo. (in the svirfneblin tongue: \"The Kxsonkxzo are svirfneblins.\")".to_string(),
                "Kxankxzha Booko. (in the svirfneblin tongue: \"Booko is the earth.\")".to_string(),
                "Nao Ngeongoa Ngalzlael. (in the triton tongue: \"The Nao are tritons.\")".to_string(),
                "Booko Ngeongoa Ngoo. (in the triton tongue: \"Booko is the earth.\")".to_string(),
                "Paek Toet Peqgop Ngaat Mapngag. (in the vent-commensal tongue: \"The Peqgop are vent commensals.\")".to_string(),
                "Paek Toet Booko Goet Maqqoog. (in the vent-commensal tongue: \"Booko is the earth.\")".to_string(),
                "Sruasrer Grun Sru Rarrar. (in the wood-elf tongue: \"The Sruasrer are wood elfs.\")".to_string(),
                "Booko Grun Sru Ruur. (in the wood-elf tongue: \"Booko is the earth.\")".to_string(),
            ]
        );
        assert_eq!(
            vol.tongue_gaps,
            vec![
                "abyssal-elf: gap — planet (no entry in this lexicon)".to_string(),
                "bugbear: gap — planet (no entry in this lexicon)".to_string(),
                "desert-dwarf: gap — planet (no entry in this lexicon)".to_string(),
                "desert-elf: gap — planet (no entry in this lexicon)".to_string(),
                "drow: gap — planet (no entry in this lexicon)".to_string(),
                "duergar: gap — planet (no entry in this lexicon)".to_string(),
                "gnoll: gap — planet (no entry in this lexicon)".to_string(),
                "goblin: gap — planet (no entry in this lexicon)".to_string(),
                "gully-dwarf: gap — planet (no entry in this lexicon)".to_string(),
                "high-elf: gap — planet (no entry in this lexicon)".to_string(),
                "hill-dwarf: gap — planet (no entry in this lexicon)".to_string(),
                "hobgoblin: gap — planet (no entry in this lexicon)".to_string(),
                "human: gap — planet (no entry in this lexicon)".to_string(),
                "kelp-tender: gap — planet (no entry in this lexicon)".to_string(),
                "kobold: gap — planet (no entry in this lexicon)".to_string(),
                "kuo-toa: gap — planet (no entry in this lexicon)".to_string(),
                "mountain-dwarf: gap — planet (no entry in this lexicon)".to_string(),
                "reef-mason: gap — planet (no entry in this lexicon)".to_string(),
                "sea-elf: gap — planet (no entry in this lexicon)".to_string(),
                "snow-elf: gap — planet (no entry in this lexicon)".to_string(),
                "svirfneblin: gap — planet (no entry in this lexicon)".to_string(),
                "triton: gap — planet (no entry in this lexicon)".to_string(),
                "vent-commensal: gap — planet (no entry in this lexicon)".to_string(),
                "wood-elf: gap — planet (no entry in this lexicon)".to_string(),
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
                "The Šzoašzo are abyssal elfs — neighbors.".to_string(),
                "The Bao are bugbears — neighbors.".to_string(),
                "The Bazap are desert dwarfs — neighbors.".to_string(),
                "The Ruašre are desert elfs — neighbors.".to_string(),
                "The Roašror are drows — neighbors.".to_string(),
                "The Dazha are duergars — neighbors.".to_string(),
                "The Wakdao are gnolls — neighbors.".to_string(),
                "The Qwootoqo are goblins — ourselves.".to_string(),
                "The Xabxat are gully dwarfs — neighbors.".to_string(),
                "The Šdoašded are high elfs — neighbors.".to_string(),
                "The Dashav are hill dwarfs — neighbors.".to_string(),
                "The Dweowbaw are hobgoblins — neighbors.".to_string(),
                "The Naavea are humans — neighbors.".to_string(),
                "The Zukzhov are kelp tenders — neighbors.".to_string(),
                "The Ngongo are kobolds — neighbors.".to_string(),
                "The Wodbog are kuo toas — neighbors.".to_string(),
                "The Dazhaf are mountain dwarfs — neighbors.".to_string(),
                "The Xaoxa are reef masons — neighbors.".to_string(),
                "The Šzuašzez are sea elfs — neighbors.".to_string(),
                "The Šroašror are snow elfs — neighbors.".to_string(),
                "The Kxsonkxzo are svirfneblins — neighbors.".to_string(),
                "The Nao are tritons — neighbors.".to_string(),
                "The Peqgop are vent commensals — neighbors.".to_string(),
                "The Sruasrer are wood elfs — neighbors.".to_string(),
                "Booko is the earth.".to_string(),
                "The day returns because the sky must be crossed.".to_string(),
            ]
        );
        assert_eq!(
            goblin.margin,
            vec![
                "In truth, Booko is a planet with two moons, orbiting a yellow-white dwarf (F); its day lasts about 1.5 standard days.".to_string(),
            ]
        );
        let goblin_doctrine = goblin.doctrine.as_ref().expect("goblin is organized");
        assert_eq!(
            goblin_doctrine.heading,
            "As the priesthood of the Qwootoqo teach it"
        );
        assert_eq!(
            goblin_doctrine.tongue_taught_line,
            // The same goblin lexicon re-draw as the `tongue_lines` block
            // above: `Saa Wovewe` -> `Sa Wewoeke` -> `Ka Jejoeke` (The Burr,
            // Task 4: the trill epoch reseeds every family's root
            // assignment), gloss byte-identical throughout.
            "Ka Jejoeke Booko. (\"Booko is the earth — as it is taught.\")"
        );
        assert_eq!(
            goblin_doctrine.emic,
            vec![
                "The Šzoašzo are abyssal elfs — neighbors.".to_string(),
                "The Bao are bugbears — neighbors.".to_string(),
                "The Bazap are desert dwarfs — neighbors.".to_string(),
                "The Ruašre are desert elfs — neighbors.".to_string(),
                "The Roašror are drows — neighbors.".to_string(),
                "The Dazha are duergars — neighbors.".to_string(),
                "The Wakdao are gnolls — neighbors.".to_string(),
                "The Qwootoqo are goblins — ourselves.".to_string(),
                "The Xabxat are gully dwarfs — neighbors.".to_string(),
                "The Šdoašded are high elfs — neighbors.".to_string(),
                "The Dashav are hill dwarfs — neighbors.".to_string(),
                "The Dweowbaw are hobgoblins — neighbors.".to_string(),
                "The Naavea are humans — neighbors.".to_string(),
                "The Zukzhov are kelp tenders — neighbors.".to_string(),
                "The Ngongo are kobolds — neighbors.".to_string(),
                "The Wodbog are kuo toas — neighbors.".to_string(),
                "The Dazhaf are mountain dwarfs — neighbors.".to_string(),
                "The Xaoxa are reef masons — neighbors.".to_string(),
                "The Šzuašzez are sea elfs — neighbors.".to_string(),
                "The Šroašror are snow elfs — neighbors.".to_string(),
                "The Kxsonkxzo are svirfneblins — neighbors.".to_string(),
                "The Nao are tritons — neighbors.".to_string(),
                "The Peqgop are vent commensals — neighbors.".to_string(),
                "The Sruasrer are wood elfs — neighbors.".to_string(),
                "Booko is the earth.".to_string(),
                "The moons are counted and known to the priesthood.".to_string(),
                "The moons cross because Shmeakjeta strides the sky, slowly.".to_string(),
                "The day returns because Veewe strides the sky, briskly.".to_string(),
            ]
        );
        assert!(goblin_doctrine.annotations.is_empty());
        assert_eq!(
            goblin_doctrine.margin,
            vec![
                "In truth, Booko is a planet orbiting a yellow-white dwarf (F); its day lasts about 1.5 standard days.".to_string(),
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
                "The Šzoašzo are abyssal elfs — rivals.".to_string(),
                "The Bao are bugbears — rivals.".to_string(),
                "The Bazap are desert dwarfs — rivals.".to_string(),
                "The Ruašre are desert elfs — rivals.".to_string(),
                "The Roašror are drows — rivals.".to_string(),
                "The Dazha are duergars — rivals.".to_string(),
                "The Wakdao are gnolls — rivals.".to_string(),
                "The Qwootoqo are goblins — rivals.".to_string(),
                "The Xabxat are gully dwarfs — rivals.".to_string(),
                "The Šdoašded are high elfs — rivals.".to_string(),
                "The Dashav are hill dwarfs — rivals.".to_string(),
                "The Dweowbaw are hobgoblins — ourselves.".to_string(),
                "The Naavea are humans — rivals.".to_string(),
                "The Zukzhov are kelp tenders — rivals.".to_string(),
                "The Ngongo are kobolds — rivals.".to_string(),
                "The Wodbog are kuo toas — rivals.".to_string(),
                "The Dazhaf are mountain dwarfs — rivals.".to_string(),
                "The Xaoxa are reef masons — rivals.".to_string(),
                "The Šzuašzez are sea elfs — rivals.".to_string(),
                "The Šroašror are snow elfs — rivals.".to_string(),
                "The Kxsonkxzo are svirfneblins — rivals.".to_string(),
                "The Nao are tritons — rivals.".to_string(),
                "The Peqgop are vent commensals — rivals.".to_string(),
                "The Sruasrer are wood elfs — rivals.".to_string(),
                "Booko is the earth.".to_string(),
                "The day returns, as all things return.".to_string(),
            ]
        );
        assert_eq!(
            hobgoblin.margin,
            vec![
                "In truth, Booko is a planet with two moons, orbiting a yellow-white dwarf (F); its day lasts about 1.5 standard days.".to_string(),
            ]
        );
        // Merge re-placement: seed-1 hobgoblin (Veebe) is now organized too
        // (all four then-peoples clear the rung), matching the regenerated
        // `book/src/gallery/the-book.md`. The Vacancy T9's gnoll also
        // organizes at seed 1. Its doctrine carries the same RevealedClaim
        // moon formula the additivity law witnesses.
        let hobgoblin_doctrine = hobgoblin.doctrine.as_ref().expect("hobgoblin is organized");
        assert_eq!(
            hobgoblin_doctrine.heading,
            "As the priesthood of the Dweowbaw teach it"
        );
        assert_eq!(
            hobgoblin_doctrine.emic,
            vec![
                "The Šzoašzo are abyssal elfs — rivals.".to_string(),
                "The Bao are bugbears — rivals.".to_string(),
                "The Bazap are desert dwarfs — rivals.".to_string(),
                "The Ruašre are desert elfs — rivals.".to_string(),
                "The Roašror are drows — rivals.".to_string(),
                "The Dazha are duergars — rivals.".to_string(),
                "The Wakdao are gnolls — rivals.".to_string(),
                "The Qwootoqo are goblins — rivals.".to_string(),
                "The Xabxat are gully dwarfs — rivals.".to_string(),
                "The Šdoašded are high elfs — rivals.".to_string(),
                "The Dashav are hill dwarfs — rivals.".to_string(),
                "The Dweowbaw are hobgoblins — ourselves.".to_string(),
                "The Naavea are humans — rivals.".to_string(),
                "The Zukzhov are kelp tenders — rivals.".to_string(),
                "The Ngongo are kobolds — rivals.".to_string(),
                "The Wodbog are kuo toas — rivals.".to_string(),
                "The Dazhaf are mountain dwarfs — rivals.".to_string(),
                "The Xaoxa are reef masons — rivals.".to_string(),
                "The Šzuašzez are sea elfs — rivals.".to_string(),
                "The Šroašror are snow elfs — rivals.".to_string(),
                "The Kxsonkxzo are svirfneblins — rivals.".to_string(),
                "The Nao are tritons — rivals.".to_string(),
                "The Peqgop are vent commensals — rivals.".to_string(),
                "The Sruasrer are wood elfs — rivals.".to_string(),
                "Booko is the earth.".to_string(),
                "The moons are counted and known to the priesthood.".to_string(),
                "The moons cross because Kwoowdaw strides the sky, slowly.".to_string(),
                "The day returns because Veewe strides the sky, briskly.".to_string(),
            ]
        );
        assert!(hobgoblin_doctrine.annotations.is_empty());
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
    /// claim: structural(seed: [1,2,3]) — prose round-trip, with a non-vacuity
    /// guard (reckoning_seen)
    #[test]
    fn every_reckoning_line_round_trips() {
        let vocab = vocab();
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
                    let again = rerender_chorus_line(&chorus_line, &vocab);
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
            vocabulary: vocab.clone(),
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
        assert_eq!(rerender_chorus_line(&chorus_line, &vocab), synthetic);

        // Synthetic: The Corrigendum T4's doctrine line has a
        // `crisis_live: false` arm ("None among the ⟨autonym⟩ have shown
        // the priesthood's teaching false.") that is unreached live within
        // seeds 1..=3 -- every organized Predictive-rung culture at these
        // seeds carries a live prediction crisis (see
        // `the_reckoning_renders_the_epoch_pair`) -- so it is exercised
        // synthetically here too, through the same public round-trip pair.
        let synthetic_doctrine =
            "None among the Qwootoqo have shown the priesthood's teaching false.";
        let chorus_line = parse_chorus_line(synthetic_doctrine, &ctx)
            .unwrap_or_else(|e| panic!("the doctrine crisis_live=false line must invert: {e:?}"));
        let ChorusLine::Reckoning(reckoning) = &chorus_line else {
            panic!("the doctrine crisis_live=false line must invert to ChorusLine::Reckoning");
        };
        assert_eq!(
            *reckoning,
            ReckoningLine::Doctrine {
                autonym: "Qwootoqo".to_string(),
                crisis_live: false,
            }
        );
        assert_eq!(
            rerender_chorus_line(&chorus_line, &vocab),
            synthetic_doctrine
        );
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
        let lines = reckoning_culture_lines(
            "Qwootoqo",
            hornvale_worldgen::LadderRung::Predictive,
            8,
            None,
        );
        assert_eq!(
            lines,
            vec![
                // The Book Polish (2026-07-20): re-pinned attributed.
                "Among the Qwootoqo, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Qwootoqo numbers the darkenings: eight.".to_string(),
            ],
            "Predictive + None: the count still renders, but no prediction line"
        );

        let lines_with_prediction = reckoning_culture_lines(
            "Qwootoqo",
            hornvale_worldgen::LadderRung::Predictive,
            8,
            Some(9080.42957840976),
        );
        assert_eq!(
            lines_with_prediction,
            vec![
                "Among the Qwootoqo, the sky has darkened, now and again.".to_string(),
                "The priesthood of the Qwootoqo numbers the darkenings: eight.".to_string(),
                "The next darkening, it teaches, comes on day 9080.".to_string(),
            ],
            "Predictive + Some: the prediction line renders, integer-truncated"
        );
    }

    /// The Corrigendum T3: [`reckoning_crisis_margin_line`]'s own format,
    /// driven synthetically (world-free, mirrors the pattern above) —
    /// integer-truncated taught/actual days, same convention as the
    /// prediction line.
    #[test]
    fn the_crisis_margin_line_quotes_the_taught_and_true_days() {
        let crisis = hornvale_worldgen::PredictionCrisis {
            last_predicted: 41_200.3,
            last_actual: 40_850.9,
        };
        assert_eq!(
            reckoning_crisis_margin_line("Qwootoqo", crisis),
            "In truth, the Qwootoqo's priesthood taught the darkening would come on day 41200; \
             it came on day 40850 instead."
        );
    }

    /// The Corrigendum T4: a folk-only culture (no doctrine) never has a
    /// doctrine-voice line, regardless of `crisis_live` — mirrors every
    /// other doctrine-gated render path's `None` convention.
    #[test]
    fn the_doctrine_line_is_none_for_a_folk_only_culture() {
        assert_eq!(reckoning_doctrine_line("Qwootoqo", false, false), None);
        assert_eq!(
            reckoning_doctrine_line("Qwootoqo", false, true),
            None,
            "a folk-only culture never has a doctrine to have taught anything wrongly"
        );
    }

    /// The Corrigendum T4: [`reckoning_doctrine_line`]'s own format, driven
    /// synthetically (world-free, mirrors the pattern above) — the
    /// thematic-only doctrine-voice acknowledgment, NOT routed through
    /// `ConflictState`/`conflict_of` (decision ledger #3).
    #[test]
    fn the_doctrine_line_names_the_crisis_when_one_is_live() {
        assert_eq!(
            reckoning_doctrine_line("Qwootoqo", true, false),
            Some("None among the Qwootoqo have shown the priesthood's teaching false.".to_string())
        );
        assert_eq!(
            reckoning_doctrine_line("Qwootoqo", true, true),
            Some(
                "The Qwootoqo's own priesthood taught wrongly, and could be shown wrong by any \
                 who kept their own count."
                    .to_string()
            )
        );
    }

    /// C8 T2, the margin law (spec §3.4). Re-derived at the-living-community
    /// merge: the history-driven re-placement now seats FOUR organized
    /// (`Predictive`) peoples at seeds 2 and 3, and both margin arms are
    /// reachable LIVE. At seed 2 (true count 81) the lunar-witnessing pair
    /// (bugbear, kobold) hold the full 81 — the CLEAN arm, no shortfall — while
    /// the solar-only pair (goblin, hobgoblin) hold only 49 (capability gates
    /// out the lunar class), a shortfall that fires the margin. Seed 3 is the
    /// same shape at a smaller scale (true 53; clean pair 53, short pair 32).
    /// The per-culture predicate ([`culture_falls_short`]) is still also
    /// exercised pure and world-free (mirrors [`reckoning_culture_lines`]) so
    /// both arms stay pinned regardless of the live landscape.
    #[test]
    fn the_margin_fires_exactly_when_knowledge_falls_short() {
        // The pure predicate, both arms, driven synthetically.
        assert!(
            !culture_falls_short(hornvale_worldgen::LadderRung::Predictive, 81, 81),
            "organized (Predictive) and held == true_count: no shortfall from it"
        );
        assert!(
            !culture_falls_short(hornvale_worldgen::LadderRung::Numbered, 10, 10),
            "organized (Numbered) and held == true_count: no shortfall from it"
        );
        assert!(
            culture_falls_short(hornvale_worldgen::LadderRung::Predictive, 49, 81),
            "organized but held < true_count: a shortfall (cannot witness what capability gates out)"
        );
        assert!(
            culture_falls_short(hornvale_worldgen::LadderRung::Counted, 81, 81),
            "folk-only (Counted): no cardinal held at all, falls short regardless of held == true_count"
        );
        assert!(
            culture_falls_short(hornvale_worldgen::LadderRung::Unknown, 0, 5),
            "Unknown: no cardinal held at all, falls short"
        );

        // The live-world half: seed 2 (true 81). The lunar-witnessing pair
        // (bugbear, kobold) are organized and hold the full count — the clean
        // arm, no shortfall. The solar-only pair (goblin, hobgoblin) hold only
        // 49, a shortfall that fires the margin.
        let world = generated(2);
        let terrain = hornvale_worldgen::terrain_of(&world).expect("terrain reconstructs");
        let climate = hornvale_worldgen::climate_from(&world, &terrain).expect("climate derives");
        let at = hornvale_astronomy::StdInstant::new(RECKONING_EPOCH_2_DAY).unwrap();
        assert_eq!(true_event_count(&world, at), 81);

        for kind in ["bugbear", "kobold"] {
            let (rung, _) =
                hornvale_worldgen::ladder_from(&world, kind, at, &terrain, &climate).unwrap();
            let obs =
                hornvale_worldgen::observations_from(&world, kind, at, &terrain, &climate).unwrap();
            assert_eq!(rung, hornvale_worldgen::LadderRung::Predictive);
            assert_eq!(
                obs.events.len(),
                81,
                "{kind}: organized and witnesses every true event — the clean arm"
            );
            assert!(
                !culture_falls_short(rung, obs.events.len() as u64, 81),
                "{kind}: organized and held == true_count — no shortfall from it"
            );
        }
        for kind in ["goblin", "hobgoblin"] {
            let (rung, _) =
                hornvale_worldgen::ladder_from(&world, kind, at, &terrain, &climate).unwrap();
            let obs =
                hornvale_worldgen::observations_from(&world, kind, at, &terrain, &climate).unwrap();
            assert_eq!(
                obs.events.len(),
                49,
                "{kind}: witnesses only the (universally-public) solar class, missing the \
                 lunar events — a shortfall against the true count"
            );
            assert!(
                culture_falls_short(rung, obs.events.len() as u64, 81),
                "{kind}: organized but held < true_count — a shortfall"
            );
        }

        // The epoch's own margin fires (from the solar-only pair's shortfall).
        let vol = render_volume(&world);
        assert!(!vol.reckoning[1].margin.is_empty());

        // seed 3: the same shape at a smaller scale (true 53; the solar-only
        // pair holds 32, a shortfall that fires the margin).
        let seed3 = generated(3);
        let terrain3 = hornvale_worldgen::terrain_of(&seed3).expect("terrain reconstructs");
        let climate3 = hornvale_worldgen::climate_from(&seed3, &terrain3).expect("climate derives");
        let at3 = hornvale_astronomy::StdInstant::new(RECKONING_EPOCH_2_DAY).unwrap();
        assert_eq!(true_event_count(&seed3, at3), 53);
        for kind in ["goblin", "hobgoblin"] {
            let (rung, _) =
                hornvale_worldgen::ladder_from(&seed3, kind, at3, &terrain3, &climate3).unwrap();
            let obs = hornvale_worldgen::observations_from(&seed3, kind, at3, &terrain3, &climate3)
                .unwrap();
            assert_eq!(
                obs.events.len(),
                32,
                "{kind}: solar-only witness, a shortfall"
            );
            assert!(
                culture_falls_short(rung, obs.events.len() as u64, 53),
                "{kind}: organized but held < true_count — a shortfall"
            );
        }
        let seed3_vol = render_volume(&seed3);
        assert!(!seed3_vol.reckoning[1].margin.is_empty());
    }
}
