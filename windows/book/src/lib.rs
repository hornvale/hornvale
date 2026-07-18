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
use hornvale_language::clause::{
    ClauseSpec, Definiteness, Frame, Number, ParseContext, ParseError, Subject, cardinal,
    parse_common, quantity, realize_common,
};
use std::collections::BTreeSet;

/// One world's volume of The Book: the seed it was rendered from plus the
/// sentences the ledger's `is-a` and `instance-of` facts realize.
/// type-audit: bare-ok(constructor-edge: seed), bare-ok(prose: lines)
pub struct BookVolume {
    /// The seed that generated the world this volume renders.
    pub seed: u64,
    /// One Common sentence per rendered `is-a` fact (ledger commit order),
    /// then one per rendered `instance-of` fact (C2 T5: a placed peopled
    /// species' collective, "The ⟨Autonym⟩ are ⟨species⟩.").
    pub lines: Vec<String>,
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
        lines.push(line);
    }
    BookVolume {
        seed: world.seed.0,
        lines,
    }
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
/// why that lets `Number::Pl` alone signal a collective on the way back).
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
}
