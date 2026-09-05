//! **The interlinear**: one structure, two languages, a real world.
//!
//! Everything else in this campaign is machinery. This file is the target it
//! was built for — a real occupation out of seed 42's ledger (a people, a
//! site, a founding year, an ending), turned into ONE
//! [`hornvale_language::clause::Clause`] and realized twice: once through
//! Common (the author's register, total, infallible) and once through that
//! people's own tongue (partial, fallible). The campaign's thesis is that an
//! utterance is a fact; this is where it stops being a claim, because the
//! clause carries the same subject/predicate/object shape the occupation's
//! committed facts do and neither realizer is handed a word of the other's
//! English.
//!
//! **Why this test lives in `windows/almanac` and not `domains/language`.**
//! A window may read a domain; a domain may not reach a sibling. The clause
//! layer is in `domains/language` and the occupation facts are in
//! `domains/history`, so nothing in `domains/language` can see an
//! occupation. This window already depends on both, and carries
//! `hornvale-worldgen` as a DEV-dependency (see `Cargo.toml`), which is what
//! lets an integration test here build a real seed-42 world. The layering is
//! enforced by `cli/tests/architecture.rs`.
//!
//! **Nothing here pins a sentence.** The seed-42 occupation roster is not
//! this campaign's to freeze, and a concept-registry change in a parallel
//! campaign moves every world fixture. So each test asserts a PROPERTY, and
//! computes what it expects from the world it just read (the people's own
//! Common word, its own numerals, its own lexicon) rather than from a
//! literal.
//!
//! # Three limits this file demonstrates rather than hides
//!
//! A demonstration is only honest if it says what it does NOT show. All
//! three are visible in the pair of sentences it produces, so recording
//! them here is cheaper than letting the next reader rediscover them.
//!
//! 1. **The tongue does not mark roles at all.** Common chooses a surface
//!    per role — a preposition for the site, a trailing clause for the
//!    ending. The tongue does not: `grammar::realize_adjuncts` matches on
//!    `adjunct.argument` and **never reads `adjunct.role`**, and the deep
//!    realizer appends each resolved word as a free token after the clause.
//!    So a tongue tail of three numerals is AMBIGUOUS between the site, the
//!    founding and the ending — recoverable only from the spec's order.
//!    A per-role construction table for tongues (the tongue-side twin of
//!    `clause::common_role_surface`) does not exist, and it is recorded
//!    nowhere else. It is a different absence from the one
//!    "And Common is not yet a peer in full"
//!    (`book/src/chronicle/the-interlinear.md`) records — that was the
//!    `TongueClause`-keeps-a-`String`-subject asymmetry, and The Scarf
//!    closed it: both realizers now take the SAME `&Clause`, so this
//!    file hands one spec to each rather than projecting between two
//!    structs. This absence survived that collapse.
//! 2. **The subject takes a liberty.** The clause's subject is the PEOPLE's
//!    autonym, used as the holding's headword, because the ledger names
//!    neither the occupation nor its site (the almanac's own history page
//!    calls a site "the clearing at vertex 5585"). A subject must be either
//!    a proper name or a word of the tongue — an English noun phrase there
//!    would put English straight into the tongue rendering — and the
//!    autonym is the only proper name in reach. If a later campaign commits
//!    a name for an occupation or its site, that is the honest subject and
//!    this clause should move to it.
//! 3. **The flagship sentence exercises none of the deep realizer's
//!    marking machinery.** `realize_tongue_deep` can affix or particle-mark
//!    evidentiality and noun class, and can carry an overt copula. Whichever
//!    people this file selects, all three are DRAWN, so which paths run is a
//!    property of the world, not of this test — and at the time of writing
//!    the selected people (hobgoblin, seed 42) draws `MorphDepth::None` on
//!    both axes and no copula, so its tongue line is the bare floor
//!    assembly. The marking paths are covered by unit tests in
//!    `domains/language/src/grammar.rs`; they are not covered here, and this
//!    file must not be read as if they were. The assertions below are
//!    written to survive a draw that DOES mark — see the positive control in
//!    [`the_tongue_shares_no_word_with_common_but_the_autonym_and_the_numerals`].

use hornvale_history::{IS_OCCUPATION, OCC_ENDED, OCC_FOUNDED, OCC_PEOPLE, OCC_SITE};
use hornvale_kernel::{Seed, Value, World};
use hornvale_language::clause::{
    Adjunct, Argument, Clause, Definiteness, Number, Polarity, Subject, Tense, cardinal,
    realize_common,
};
use hornvale_language::{
    CommonVocabulary, Evidential, SchemaId, realize_tongue_deep, tongue_grammar,
};
use hornvale_worldgen::{SettlementPins, build_world};
use std::collections::BTreeSet;

/// The book's reference seed — the one every committed gallery artifact and
/// every cross-campaign fixture is drawn from.
const REFERENCE_SEED: u64 = 42;

/// A real, fully generated world at [`REFERENCE_SEED`]. `BuildDepth::Full` is
/// required and not merely convenient: occupations are committed by the
/// deep-time stage, which is the last rung.
// No `#[allow(clippy::disallowed_methods)]` here on purpose: `build_world` is
// not on `clippy.toml`'s disallowed list (only the derivation entry points
// `terrain_of`/`climate_from` are — see `tongue_of`), and that attribute IS
// the greppable sanctioned-site index, so a spurious one pollutes it.
fn generated_world() -> World {
    build_world(
        Seed(REFERENCE_SEED),
        &hornvale_astronomy::SkyPins::default(),
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .unwrap_or_else(|e| panic!("seed {REFERENCE_SEED}'s default pins must build a world: {e:?}"))
}

/// Bake year from a ledger day — the crossing `history_emit` performs on the
/// way in, undone on the way out (mirrored here rather than imported:
/// `windows/almanac` keeps its own copy of this decode, see
/// `windows/almanac/src/history.rs`'s note on the worldgen dependency cycle).
/// Foundings and endings sit on 25-year epochs, so the round trip is exact.
fn bake_year(days: f64) -> u64 {
    (days / hornvale_kernel::Years::DAYS_PER_YEAR).round() as u64
}

/// The four facts this file needs off one occupation entity, decoded.
struct Occupation {
    /// The people's canonical label, e.g. `"hobgoblin"` (`occ-people`).
    people: String,
    /// The vertex the occupation sat on (`occ-site`).
    site: u64,
    /// The bake year it was founded in (`occ-founded`).
    founded: u64,
    /// The bake year it ended in (`occ-ended`).
    ended: u64,
}

/// The first occupation in ledger order that this file can say anything
/// interesting about: it ended (so it has BOTH years), it outlived its own
/// founding epoch (so the two years differ and an assertion on each is not
/// silently the same assertion twice), it was founded after the world's
/// first year, and its people is one that actually placed (so it has a
/// tongue at all — a grammar, a lexicon and a morphology).
///
/// Ledger order, deliberately: a "pick the longest-lived" or "pick the
/// largest" selector would make this test's subject move whenever the bake's
/// numbers move, and the point here is a stable *shape*, not a stable world.
fn an_occupation_with_a_tongue(world: &World, placed: &[&str]) -> Occupation {
    world
        .ledger
        .find(IS_OCCUPATION)
        .filter_map(|fact| {
            let entity = fact.subject;
            let people = world.ledger.text_of(entity, OCC_PEOPLE)?.to_string();
            if !placed.contains(&people.as_str()) {
                return None;
            }
            let Some(Value::Number(site)) = world.ledger.value_of(entity, OCC_SITE) else {
                return None;
            };
            let Some(Value::Number(founded)) = world.ledger.value_of(entity, OCC_FOUNDED) else {
                return None;
            };
            let Some(Value::Number(ended)) = world.ledger.value_of(entity, OCC_ENDED) else {
                return None;
            };
            let (founded, ended) = (bake_year(*founded), bake_year(*ended));
            (founded > 0 && ended > founded).then_some(Occupation {
                people,
                site: *site as u64,
                founded,
                ended,
            })
        })
        .next()
        .expect(
            "seed 42's deep history holds at least one ended occupation, founded after year 0, \
             by a people that placed",
        )
}

/// A people's autonym: the `NAME` committed on its collective entity — its
/// own word for itself, and the ONLY proper name the ledger attaches to an
/// occupation's people.
///
/// Takes the FIRST `instance-of` fact naming this kind. `windows/book`'s
/// `autonym_by_kind` builds a map and so keeps the LAST; the two agree on
/// every world where a kind has exactly one collective (seed 42 does), and
/// this is deliberately not claimed to be the same read.
fn autonym_of(world: &World, people: &str) -> String {
    world
        .ledger
        .find(hornvale_kernel::INSTANCE_OF)
        .find(|fact| matches!(&fact.object, Value::Text(kind) if kind == people))
        .and_then(|fact| world.ledger.text_of(fact.subject, hornvale_kernel::NAME))
        .unwrap_or_else(|| panic!("{people} placed, so its collective carries an autonym"))
        .to_string()
}

/// The one clause this file realizes twice.
///
/// Its subject is the people's autonym — the one proper name in reach — and
/// both realizers pass a proper name through verbatim, which is precisely
/// why it is the one word the two renderings are ALLOWED to share. Its
/// complement is `"home"`, a core concept every placed people's lexicon
/// holds as a real word rather than a gap. Its four role bindings are the
/// occupation's own facts, under the occupation predicates' own ids.
///
/// **What the two languages do with those ids differs, and the difference
/// is not symmetric.** Common chooses a surface PER ROLE — `occ-site`
/// becomes a prepositional phrase, `occ-ended` a trailing clause — through
/// `clause::common_role_surface`. The tongue does not choose at all:
/// `grammar::realize_adjuncts` matches on the ARGUMENT and never reads the
/// role, and the deep realizer appends each resolved word as a free token
/// (never an affix). So the tongue's tail is three bare numerals in spec
/// order, ambiguous between the site, the founding and the ending. See this
/// module's doc, limit 1.
fn clause_for(occupation: &Occupation, autonym: &str) -> Clause {
    Clause {
        predicate: hornvale_kernel::world::IS_A.to_string(),
        subject: Subject::Name(autonym.to_string()),
        object: Argument::Concept("home".to_string()),
        number: Number::Sg,
        definiteness: Definiteness::Def,
        // The people saying where it lives and when: lived experience, so
        // Witnessed — the same reading `windows/book`'s self-statement
        // takes. Stated HERE now that `Clause` carries the feature; it
        // used to be invented out of band inside `tongue_view`.
        evidential: Evidential::Witnessed,
        // **Past, and this is the campaign's motivating defect closing.**
        // `an_occupation_with_a_tongue` selects an occupation that ENDED, so
        // the flagship used to assert a present state and then report its own
        // ending six hundred years earlier. Spec §3.3 puts the fix here rather
        // than in the realizer: tense is a RELATION to a moment outside the
        // clause, so the caller — which holds the occupation's `occ-ended`
        // fact and knows the utterance is being made now — computes the
        // relation and states it. This is that computation's first live site.
        tense: Tense::Past,
        polarity: Polarity::Pos,
        adjuncts: vec![
            Adjunct {
                role: OCC_PEOPLE.to_string(),
                argument: Argument::Concept(format!("{}-kind", occupation.people)),
            },
            Adjunct {
                role: OCC_SITE.to_string(),
                argument: Argument::Count(occupation.site),
            },
            Adjunct {
                role: OCC_FOUNDED.to_string(),
                argument: Argument::Count(occupation.founded),
            },
            Adjunct {
                role: OCC_ENDED.to_string(),
                argument: Argument::Count(occupation.ended),
            },
        ],
    }
}

/// Everything a tongue needs to speak, for one people of one world.
struct Tongue {
    grammar: hornvale_language::TongueGrammar,
    morph: hornvale_language::TongueMorphology,
    lexicon: hornvale_language::Lexicon,
    orthography: hornvale_language::Orthography,
    sky_animate: bool,
}

impl Tongue {
    /// Realize a clause in this tongue — `realize_tongue_deep`, assembled
    /// exactly as `windows/book` assembles it, so this file speaks the same
    /// tongue the book prints.
    fn say(&self, clause: &Clause) -> Result<String, hornvale_language::TongueGap> {
        let noun_class_of =
            |concept: &str| hornvale_language::noun_class_with_sky(self.sky_animate, concept);
        realize_tongue_deep(
            clause,
            &self.grammar,
            &self.morph,
            // The book prints only present-tense statements, so no tongue it
            // assembles models tense today (The Inquest, spec §4.2).
            None,
            &noun_class_of,
            &self.lexicon,
            self.orthography,
        )
    }
}

/// Assemble `people`'s tongue off a real world.
// Named construction site (decision 0092): re-derives terrain and climate off
// the built world exactly as `windows/book` does when it assembles a tongue.
#[allow(clippy::disallowed_methods)]
fn tongue_of(world: &World, people: &str) -> Tongue {
    let terrain = hornvale_worldgen::terrain_of(world).expect("a built world sculpts");
    let climate = hornvale_worldgen::climate_from(world, &terrain).expect("a sculpted world fits");
    let phonology = hornvale_worldgen::language_of(world, people);
    Tongue {
        grammar: tongue_grammar(&world.seed, people, &phonology),
        morph: hornvale_worldgen::tongue_morphology_of(world, people)
            .expect("a placed people draws a morphology"),
        lexicon: hornvale_worldgen::lexicon_from(world, people, &terrain, &climate)
            .expect("a placed people builds a lexicon"),
        orthography: phonology.orthography,
        sky_animate: hornvale_worldgen::day_schema_from(world, people, &terrain, &climate)
            == Some(SchemaId::Agentive),
    }
}

/// The peoples that actually placed on this world.
fn placed_labels(world: &World) -> Vec<&'static str> {
    hornvale_worldgen::placed_peoples(world)
        .into_iter()
        .map(|(kind, _)| kind)
        .collect()
}

/// A rendering's words, lowercased and stripped of the punctuation either
/// realizer adds (Common's commas, semicolons and terminal stop; the
/// tongue's terminal stop). Deliberately NOT a tokenizer for prose in
/// general — it is the exact inverse of the two surfaces this file
/// produces.
fn words(sentence: &str) -> BTreeSet<String> {
    sentence
        .split_whitespace()
        .map(|word| {
            word.trim_matches(|c: char| c == ',' || c == ';' || c == '.')
                .to_lowercase()
        })
        .filter(|word| !word.is_empty())
        .collect()
}

/// **Property 1** — the Common rendering carries the occupation's facts: its
/// people (by that people's own Common word), its site, and BOTH years.
///
/// Each expectation is computed from the world and from Common's own
/// renderers (`CommonVocabulary::word_for`, `cardinal`) rather than written
/// out, so this test says "the fact reached the sentence" and never "the
/// sentence is this string".
///
/// **Asserted on WHOLE TOKENS, not on substrings of the sentence**, and the
/// difference is not pedantic: a vertex and a year share digits routinely,
/// so a founding of `83` beside a site of `1837` would satisfy a
/// `contains("83")` off the VERTEX alone — deleting the `occ-founded`
/// construction entirely and leaving this test green. [`words`] is the same
/// tokenizer property 2 uses, so both properties agree on what a word is.
#[test]
fn a_real_occupation_reaches_the_common_sentence_with_its_people_site_and_both_years() {
    let world = generated_world();
    let placed = placed_labels(&world);
    let occupation = an_occupation_with_a_tongue(&world, &placed);
    let autonym = autonym_of(&world, &occupation.people);
    let vocab = CommonVocabulary::build(&world.registry).expect("Common is total on the registry");
    let common = realize_common(&clause_for(&occupation, &autonym), &vocab);
    let tokens = words(&common);

    // The people arrives PLURALIZED (`occ-people` renders the role's concept
    // through the complement slot's own plural rule), so accept either
    // number: the exact surface is pinned by `clause.rs`'s own unit test, and
    // what this test asks is only whether the fact arrived at all.
    let people_word = vocab
        .word_for(&format!("{}-kind", occupation.people))
        .to_lowercase();
    assert!(
        tokens.contains(&people_word) || tokens.contains(&format!("{people_word}s")),
        "the people ({people_word}) is not a word of {common:?}"
    );
    // A vertex is an identifier and renders as bare digits; a year is a count
    // and goes through `cardinal`, which is a WORD at or below twelve. Each
    // expectation is built with the same renderer Common used.
    assert!(
        tokens.contains(&occupation.site.to_string()),
        "the site (vertex {}) is not a word of {common:?}",
        occupation.site
    );
    assert!(
        tokens.contains(&cardinal(occupation.founded)),
        "the founding year ({}) is not a word of {common:?}",
        occupation.founded
    );
    assert!(
        tokens.contains(&cardinal(occupation.ended)),
        "the ending year ({}) is not a word of {common:?}",
        occupation.ended
    );
    assert!(
        tokens.contains(&autonym.to_lowercase()),
        "the subject ({autonym}) is not a word of {common:?}"
    );
    // Spec criterion 1, asserted on the REAL occupation rather than on a
    // synthetic clause: this holding ended (`an_occupation_with_a_tongue`
    // requires it), so the sentence that reports the ending must not also
    // assert a present state. Both halves are needed — a copula slot that
    // emitted both forms would satisfy the first alone.
    assert!(
        tokens.contains("was"),
        "the occupation ended in year {}, so Common must say `was`: {common:?}",
        occupation.ended
    );
    assert!(
        !tokens.contains("is"),
        "the occupation ended in year {}, so Common must not also say `is`: {common:?}",
        occupation.ended
    );
}

/// **Property 2, the discriminating one** — the same spec realized in the
/// people's own tongue shares no word with the Common rendering except the
/// tokens neither language lexicalizes.
///
/// **What counts as the exception, and why.** Two kinds of token, and both
/// are exceptions for the same mechanical reason: BOTH realizers pass them
/// through verbatim rather than resolving them through a vocabulary.
///
/// 1. **Proper names** — `Subject::Name` and `Argument::Name`. Here that is
///    the people's autonym: a name, not a word, and a name is the same name
///    in every language.
/// 2. **Numerals** — `Argument::Count`/`Quantity`. The site's vertex and the
///    two years reach the surface as digits on the tongue side
///    (`realize_adjuncts` renders a count with `to_string`) and, for
///    anything above twelve, as the same digits on the Common side
///    (`cardinal`). Neither is a word of either language; a tongue's own
///    numeral system is not built yet.
///
/// Everything else is a WORD, and a word shared between the two renderings
/// would mean Common had leaked into the tongue — which is exactly the
/// failure this campaign's clause layer exists to make impossible.
///
/// The check runs both ways round and carries its own positive control: the
/// tongue's lexicalized words (its word for `"home"` and its word for the
/// people) must be present in the tongue sentence and absent from the Common
/// one, so an empty intersection can never be reached by an empty rendering.
/// Mutation-checked in review: leaking Common into the tongue reds the
/// intersection, degenerating the tongue reds the control, and neutralising
/// the control turns a degenerate one-word rendering GREEN — so the control
/// is load-bearing rather than decorative.
///
/// **The two halves of that control are asserted differently, because the
/// realizer treats them differently.** The people's word rides an ADJUNCT,
/// and `grammar::realize_adjuncts` appends a resolved adjunct verbatim — it
/// is never marked, so it must appear as a whole token. The `"home"` word is
/// the COMPLEMENT, which is the one word `realize_tongue_deep` may bind a
/// marker onto: noun-class marking always targets the complement noun, and a
/// zero-copula tongue also encliticizes the evidential onto it. At
/// `MorphDepth::Affix` that produces `Qoqeba`, not `Qoqe`, so an
/// equality check here would be latently flaky at the draw weights
/// `morphology.rs` uses (`[55, 15, 30]` for noun class, `[60, 25, 15]` for
/// evidentiality — roughly a third of peoples would mark), and would fail
/// with a message that reads like truncation or a leak. Containment is the
/// right relation and is GUARANTEED, not hoped for: `morphology::affix`
/// concatenates segment lists and re-renders the whole, and
/// `naming::render_views_with` is per-segment except for `capitalize_first`
/// and a boundary apostrophe, so a marked complement always contains the
/// bare root once both sides are lowercased.
#[test]
fn the_tongue_shares_no_word_with_common_but_the_autonym_and_the_numerals() {
    let world = generated_world();
    let placed = placed_labels(&world);
    let occupation = an_occupation_with_a_tongue(&world, &placed);
    let autonym = autonym_of(&world, &occupation.people);
    let spec = clause_for(&occupation, &autonym);
    let vocab = CommonVocabulary::build(&world.registry).expect("Common is total on the registry");

    let common = realize_common(&spec, &vocab);
    let tongue_speaker = tongue_of(&world, &occupation.people);
    let tongue = tongue_speaker
        .say(&spec)
        .expect("the chosen people can say its own home");

    // Positive control: the tongue really did lexicalize two concepts, and
    // neither word is Common's. See this test's doc for why the complement
    // half is a containment and the adjunct half an equality.
    let home_word = tongue_speaker
        .lexicon
        .entry("home")
        .and_then(word_of)
        .expect("the chosen people has a word for home")
        .to_lowercase();
    let people_word = tongue_speaker
        .lexicon
        .entry(&format!("{}-kind", occupation.people))
        .and_then(word_of)
        .expect("the chosen people has a word for itself")
        .to_lowercase();
    let tongue_words = words(&tongue);
    let common_words = words(&common);

    assert!(
        tongue_words.iter().any(|token| token.contains(&home_word)),
        "the tongue's word for home ({home_word}) is in no token of {tongue:?} — either the \
         complement never reached the sentence, or `affix` no longer leaves the bare root \
         inside a marked complement (evidential depth {:?}, noun-class depth {:?})",
        tongue_speaker.morph.evidential_depth,
        tongue_speaker.morph.noun_class_depth
    );
    assert!(
        tongue_words.contains(&people_word),
        "the tongue's word for its own people ({people_word}) is not a whole token of \
         {tongue:?} — an adjunct is appended verbatim and is never marked, so this is a \
         missing adjunct, not affixation"
    );
    for word in [&home_word, &people_word] {
        assert!(
            !common_words.iter().any(|token| token.contains(word)),
            "{word} is a tongue word and leaked into the Common rendering {common:?}"
        );
    }

    let shared: Vec<&String> = tongue_words.intersection(&common_words).collect();
    let permitted =
        |word: &str| word == autonym.to_lowercase() || word.chars().all(|c| c.is_ascii_digit());
    assert!(
        shared.iter().all(|word| permitted(word)),
        "the two renderings share a WORD, not merely a name or a numeral: {shared:?}\n\
         common: {common:?}\n tongue: {tongue:?}"
    );
    assert!(
        shared.contains(&&autonym.to_lowercase()),
        "the autonym is the one name both languages should carry, but {shared:?} lacks it"
    );
}

/// **Property 3** — a tongue that lacks a bound concept refuses the WHOLE
/// clause and names the concept it could not say.
///
/// Bound on an ADJUNCT rather than on the complement, deliberately: the
/// complement's gap path already has unit coverage in
/// `domains/language/src/grammar.rs`, and gapping on a role binding is what
/// proves the role arguments really are lexicalized through the speaker's
/// own lexicon — which is the premise property 2 rests on. `"planet"` is the
/// probe because no culture on this world holds it (the same coverage gap
/// `windows/book` records).
#[test]
fn a_tongue_that_lacks_a_bound_concept_gaps_and_names_it() {
    let world = generated_world();
    let placed = placed_labels(&world);
    let occupation = an_occupation_with_a_tongue(&world, &placed);
    let autonym = autonym_of(&world, &occupation.people);
    let mut spec = clause_for(&occupation, &autonym);
    spec.adjuncts[0].argument = Argument::Concept("planet".to_string());

    let gap = tongue_of(&world, &occupation.people)
        .say(&spec)
        .expect_err("no people on this world has a word for the planet as a body");
    assert_eq!(gap.concept, "planet");
    assert!(!gap.reason.is_empty(), "a gap is recountable, never bare");
}

/// A lexicon entry's roman form, or `None` when the entry is itself a gap.
fn word_of(entry: &hornvale_language::LexEntry) -> Option<String> {
    match entry {
        hornvale_language::LexEntry::Root { views, .. }
        | hornvale_language::LexEntry::Compound { views, .. } => Some(views.roman.clone()),
        hornvale_language::LexEntry::Gap { .. } => None,
    }
}
