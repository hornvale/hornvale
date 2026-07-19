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
use hornvale_language::account::{Account, AccountEntry, Disposition, Stance};
use hornvale_language::clause::{
    ClauseSpec, Definiteness, Frame, Number, ParseContext, ParseError, Subject, cardinal,
    parse_common, quantity, realize_common,
};
use hornvale_language::{TongueClause, realize_tongue, tongue_grammar};
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
    /// C3 T3: one self-statement per placed people, realized in its own
    /// tongue (`realize_tongue` over that people's `TongueGrammar` and
    /// lexicon) — "⟨autonym⟩ ⟨copula?⟩ ⟨own-kind⟩." glossed with the
    /// matching Common line. The self-statement law (spec §5): autonym and
    /// own-kind are Steeped by construction, so this never gaps — see
    /// `every_placed_people_self_states_in_its_own_tongue`.
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

    // C3 T3: each placed people states its own kind in its own tongue, then
    // every tongue's attempt to state each probe's kind is recorded — a
    // rendered line on success, a coverage gap on failure (spec §5 — no
    // culture holds `planet` today). Iterated over `placed_peoples`
    // (registry order, deterministic) rather than the ledger scan above so
    // the section's order matches every other peoples-keyed section in the
    // almanac/book.
    let mut tongue_lines = Vec::new();
    let mut tongue_gaps = Vec::new();
    let probes = tongue_probes(world);
    for (kind, _village) in hornvale_worldgen::placed_peoples(world) {
        let Some((autonym, common_line)) = people_by_kind.get(kind) else {
            continue;
        };
        let ph = hornvale_worldgen::language_of(world, kind);
        let grammar = tongue_grammar(&world.seed, kind, &ph);
        let Ok(lexicon) = hornvale_worldgen::lexicon_of(world, kind) else {
            continue;
        };

        let own_kind = format!("{kind}-kind");
        let self_statement = TongueClause {
            subject: autonym.clone(),
            complement_concept: own_kind,
        };
        let tongue_line =
            realize_tongue(&self_statement, &grammar, &lexicon).unwrap_or_else(|gap| {
                panic!(
                    "the self-statement law (spec §5) is violated for {kind}: \
                     gap on {} ({})",
                    gap.concept, gap.reason
                )
            });
        tongue_lines.push(format!(
            "{tongue_line} (in the {kind} tongue: \"{common_line}\")"
        ));

        for probe in &probes {
            match probe_tongue(probe, kind, &grammar, &lexicon) {
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
/// loop above).
fn chorus_sections(world: &World) -> Vec<ChorusSection> {
    let autonyms = autonym_by_kind(world);
    hornvale_worldgen::accounts_of(world)
        .into_iter()
        .filter_map(|voice| {
            let autonym = autonyms.get(&voice.kind)?;
            Some(voice_section(&voice.kind, autonym, &voice.account, world))
        })
        .collect()
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
    }
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
/// ⟨concept⟩` — `Ok` is a rendered line (the success path C3 dropped),
/// `Err` the recountable gap.
fn probe_tongue(
    probe: &TongueProbe,
    _kind: &str,
    grammar: &hornvale_language::TongueGrammar,
    lexicon: &hornvale_language::Lexicon,
) -> Result<String, hornvale_language::TongueGap> {
    realize_tongue(
        &TongueClause {
            subject: probe.subject.clone(),
            complement_concept: probe.concept.clone(),
        },
        grammar,
        lexicon,
    )
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

/// Invert one rendered chorus line (emic or margin): strip the margin's
/// `"In truth, "` prefix, then the stance appositive suffix (restoring the
/// clause's terminal `'.'` in its place), then delegate to [`parse_line`].
/// Returns the recovered [`ParsedLine`] plus the [`ChorusDress`] recording
/// exactly what was stripped, so [`rerender_chorus_line`] can restore it —
/// the design-freedom variant the brief allows over a bare `ParsedLine`.
/// type-audit: bare-ok(prose: line)
pub fn parse_chorus_line(
    line: &str,
    ctx: &ParseContext,
) -> Result<(ParsedLine, ChorusDress), LineError> {
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
    Ok((parsed, ChorusDress { stance, in_truth }))
}

/// Re-realize a [`ParsedLine`] plus its [`ChorusDress`] back to the exact
/// chorus surface text: [`rerender`], then re-append the stance appositive
/// (replacing the terminal `.`), then re-prepend `"In truth, "` — the
/// exact inverse of [`parse_chorus_line`]'s strip order.
/// type-audit: bare-ok(prose: return)
pub fn rerender_chorus_line(parsed: &ParsedLine, dress: &ChorusDress) -> String {
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
    /// renders — no gaps.
    #[test]
    fn every_placed_people_self_states_in_its_own_tongue() {
        for seed in [1u64, 2, 3] {
            let world = generated(seed);
            let vol = render_volume(&world);
            let peoples = hornvale_worldgen::placed_peoples(&world);
            assert_eq!(
                vol.tongue_lines.len(),
                peoples.len(),
                "seed {seed}: one tongue line per placed people"
            );
            for line in &vol.tongue_lines {
                assert!(line.contains(" ("), "line carries a gloss: {line}");
            }
        }
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
        let mut exposures = BTreeMap::new();
        exposures.insert("planet".to_string(), ExposureClass::Steeped);
        let lexicon = build_lexicon(&world.seed, "goblin", "goblin", &ph, &ph, &exposures, &[]);
        let probe = TongueProbe {
            concept: "planet".to_string(),
            subject: "Vebe".to_string(),
        };
        let line =
            probe_tongue(&probe, "goblin", &grammar, &lexicon).expect("a Steeped concept realizes");
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
                    let (parsed, _dress) = parse_chorus_line(line, &ctx).unwrap_or_else(|e| {
                        panic!(
                            "seed {seed} {}: line failed to parse: {line} ({e:?})",
                            section.kind
                        )
                    });
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
    #[test]
    fn every_chorus_line_round_trips() {
        for seed in [1u64, 2, 3] {
            let world = generated(seed);
            let ctx = parse_context(&world);
            let vol = render_volume(&world);
            for section in &vol.chorus {
                for line in section.emic.iter().chain(section.margin.iter()) {
                    let (parsed, dress) = parse_chorus_line(line, &ctx).unwrap_or_else(|e| {
                        panic!(
                            "seed {seed} {}: line failed to parse: {line} ({e:?})",
                            section.kind
                        )
                    });
                    let again = rerender_chorus_line(&parsed, &dress);
                    assert_eq!(
                        &again, line,
                        "seed {seed} {}: re-realization drifted",
                        section.kind
                    );
                }
            }
        }
    }
}
