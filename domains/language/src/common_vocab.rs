//! Common's vocabulary. Common is the author's register, not a people's
//! tongue: it has no speakers, so it has no `Lexicon`. What it has instead is
//! a TOTAL id→word map — every registered concept has a Common word, or the
//! vocabulary refuses to be built.
//!
//! Totality is what makes the translation asymmetry a type-level fact rather
//! than a hope. Concept → Common is infallible; concept → a people's tongue
//! returns `Result<_, TongueGap>`. So a gap always means something true about
//! the world (this people has no word for the sea) and never an authoring hole
//! (nobody wrote down what `celestial-body` is called).
//!
//! This is a mechanism, not a data table: it holds no domain's concept ids.
//! Each domain exposes its own id→display pairs (e.g. astronomy's
//! `star::common_words`) and the composition root declares them into a
//! vocabulary after [`CommonVocabulary::build`] — layering forbids this crate
//! from reaching sideways to know what any other domain's ids are.

use hornvale_kernel::ConceptRegistry;
use std::collections::BTreeMap;

/// Common's TOTAL id→word map: declared exceptions first, then the naming
/// convention's own rules. `word_for` is total by construction — it returns
/// `String` unconditionally for any input, so totality does not depend on
/// how a value was built. [`CommonVocabulary::build`] validates a registry's
/// concepts against the naming convention and is how a caller should
/// construct one in practice, but `Default` (mandated by the brief, and used
/// by every test in this module) bypasses that check — it is not a second
/// validating constructor, just the ordinary empty starting point.
#[derive(Clone, Debug, Default)]
pub struct CommonVocabulary {
    declared: BTreeMap<String, String>,
}

impl CommonVocabulary {
    /// Declare the Common word for a concept the rules get wrong. Always wins
    /// over the rules; re-declaring replaces.
    /// type-audit: bare-ok(identifier-text: concept), bare-ok(identifier-text: word)
    pub fn declare(&mut self, concept: &str, word: &str) {
        self.declared.insert(concept.to_string(), word.to_string());
    }

    /// Whether this concept's word was authored rather than derived.
    /// type-audit: bare-ok(identifier-text: concept), bare-ok(flag: return)
    pub fn is_declared(&self, concept: &str) -> bool {
        self.declared.contains_key(concept)
    }

    /// The Common word for `concept`. Infallible: the naming convention
    /// resolves every id, and [`build`](Self::build) has already checked that
    /// the result reads as a word rather than a key.
    ///
    /// Resolution order: a declared entry always wins; otherwise strip a
    /// trailing `-kind` (a species tag, not part of the name — `goblin-kind`
    /// → `goblin`); otherwise replace every `'-'` with `' '` (the hyphen is an
    /// id-joiner, not part of the word — `abyssal-plain` → `abyssal plain`);
    /// an id with no hyphen is already its own word.
    /// type-audit: bare-ok(identifier-text: concept), bare-ok(prose: return)
    pub fn word_for(&self, concept: &str) -> String {
        if let Some(w) = self.declared.get(concept) {
            return w.clone();
        }
        let stem = concept.strip_suffix("-kind").unwrap_or(concept);
        stem.replace('-', " ")
    }

    /// A lint on the naming convention, not a totality check: verifies that
    /// no concept in `registry` derives to degenerate whitespace (empty, a
    /// leftover hyphen, or a stray/doubled separator) — identifier
    /// well-formedness, not word quality. `sun-like-star` → `"sun like
    /// star"` is grammatically wrong but passes this check cleanly, because
    /// it is not degenerate; fixing it is a domain's job (see
    /// `star::common_words` in `domains/astronomy`), not this function's.
    ///
    /// Starts from [`CommonVocabulary::default`] (no exceptions declared) and
    /// checks the mechanical rules alone — a domain's authored overrides
    /// (astronomy's spectral-class displays, say) are layered on afterward by
    /// the composition root via [`declare`](Self::declare), which never
    /// re-validates.
    pub fn build(registry: &ConceptRegistry) -> Result<Self, MissingCommonWords> {
        let vocab = Self::default();
        let mut bad = Vec::new();
        for concept in registry.concepts() {
            let word = vocab.word_for(&concept.name);
            // `vocab` is fresh from `Self::default()` above, so `is_declared`
            // is always false here today — this guard is forward-looking,
            // not live: if `build` ever gains a way to seed exceptions before
            // validating, a declared word must still skip this check.
            if !vocab.is_declared(&concept.name) && reads_as_a_key(&word) {
                bad.push((concept.name.clone(), word));
            }
        }
        if bad.is_empty() {
            Ok(vocab)
        } else {
            Err(MissingCommonWords { concepts: bad })
        }
    }
}

/// Whether `word` still reads as degenerate whitespace rather than a word: an
/// empty span, a leading/trailing separator, or a doubled one (a malformed id
/// like `"-leading"` or `"foo--bar"` can leave any of these; none of them read
/// as a word). These are the reachable cases — the two synthetic tests below
/// (`build_rejects_a_malformed_id`, `build_reports_every_failure_not_just_the_first`)
/// exercise them directly. The check also includes a leftover-hyphen disjunct
/// (`contains('-')`) as defense in depth against a future change to
/// [`CommonVocabulary::word_for`]'s algorithm; today it provably never fires,
/// because `word_for`'s non-declared path always ends in
/// `replace('-', " ")`, which cannot leave a hyphen behind. None of these
/// fire on the live registry today — the mechanical rules already resolve
/// all 191 concepts cleanly. A declared entry is exempt by construction —
/// [`CommonVocabulary::build`] never calls this against a declared word,
/// because an author may legitimately keep a hyphen (`sun-like star`).
fn reads_as_a_key(word: &str) -> bool {
    word.is_empty()
        || word.contains('-')
        || word.contains("  ")
        || word.starts_with(' ')
        || word.ends_with(' ')
}

/// Concepts whose derived Common word still reads as degenerate whitespace
/// rather than a word — [`CommonVocabulary::build`] refuses to construct a
/// vocabulary carrying one of these. `declare` runs on the vocabulary
/// `build` has already returned, so a failure here can only be fixed by
/// changing the mechanical rules themselves, not by declaring after the
/// fact. In practice this has never fired against the live registry.
/// type-audit: bare-ok(identifier-text: concepts)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MissingCommonWords {
    /// The concept ids that failed to resolve, paired with the word the rules
    /// produced for each.
    pub concepts: Vec<(String, String)>,
}

impl std::fmt::Display for MissingCommonWords {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} concept(s) have no Common word (the naming convention left \
             something that still reads as a key): ",
            self.concepts.len()
        )?;
        for (i, (concept, word)) in self.concepts.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{concept:?} -> {word:?}")?;
        }
        Ok(())
    }
}

impl std::error::Error for MissingCommonWords {}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{ConceptDef, ConceptKind, Correspondent, Lexicalization, Manifest, Void};

    /// An id with no hyphen is already its own word — the naming convention
    /// does the work, and declaring 98 identities would be noise.
    #[test]
    fn a_single_word_id_is_its_own_word() {
        let v = CommonVocabulary::default();
        assert_eq!(v.word_for("moon"), "moon");
    }

    /// A hyphen is an id-joiner, not part of the word. 93 of the registry's
    /// 191 concepts are hyphenated and nearly all read correctly this way.
    #[test]
    fn a_hyphen_becomes_a_space() {
        let v = CommonVocabulary::default();
        assert_eq!(v.word_for("abyssal-plain"), "abyssal plain");
        assert_eq!(v.word_for("temperate-grassland"), "temperate grassland");
    }

    /// The `-kind` suffix is a species tag, not part of the name. 28 concepts
    /// carry it; "goblin kind" is not what a reader should see.
    #[test]
    fn a_kind_suffix_is_stripped() {
        let v = CommonVocabulary::default();
        assert_eq!(v.word_for("goblin-kind"), "goblin");
        assert_eq!(v.word_for("woolly-mammoth-kind"), "woolly mammoth");
    }

    /// A declared exception always wins, for the ids the rules get wrong.
    #[test]
    fn a_declared_word_wins() {
        let mut v = CommonVocabulary::default();
        v.declare("sun-like-star", "sun-like star");
        assert_eq!(v.word_for("sun-like-star"), "sun-like star");
    }

    /// Re-declaring a concept replaces the earlier word rather than erroring
    /// or stacking — `declared` is a map, not a log.
    #[test]
    fn redeclaring_replaces() {
        let mut v = CommonVocabulary::default();
        v.declare("sun-like-star", "sunlike star");
        v.declare("sun-like-star", "sun-like star");
        assert_eq!(v.word_for("sun-like-star"), "sun-like star");
    }

    /// Insert a bare concept anchor (no manifest edges the vocabulary cares
    /// about) into a fresh registry — the shared boilerplate every test below
    /// needs to build a synthetic multi-concept registry without depending on
    /// any domain crate (layering forbids it; this is just string data).
    fn with_concept(registry: &mut ConceptRegistry, name: &str) {
        registry
            .register_manifest(Manifest {
                concept: ConceptDef {
                    name: name.to_string(),
                    domain: "test-fixture".to_string(),
                    kind: ConceptKind::Quality,
                    doc: "a fixture concept".to_string(),
                },
                lexeme: Correspondent::Present(Lexicalization::Expected),
                percept: Correspondent::Absent(Void::Gap("fixture: no phenomenon")),
                cognition: Correspondent::Absent(Void::Uncognized {
                    pending_wave: "fixture",
                }),
            })
            .unwrap();
    }

    /// Every concept id actually registered on the live world (a snapshot
    /// taken via `hornvale concepts` while writing this module — 191 concepts,
    /// 93 hyphenated, 28 of them `-kind` species tags). `domains/language` may
    /// not depend on the domains that own most of these, so the ids are
    /// reproduced here as plain string data (the same way the brief's own
    /// `a_declared_word_wins` test above uses `sun-like-star`) rather than
    /// built by calling another domain's `register_concepts`.
    const LIVE_REGISTRY_SNAPSHOT: &[&str] = &[
        "abyssal",
        "abyssal-plain",
        "alpine",
        "bait-ball",
        "barley",
        "bathypelagic",
        "black-dragon-kind",
        "blood",
        "blue",
        "blue-giant",
        "bone",
        "boreal-stand",
        "brown",
        "bugbear-kind",
        "burn",
        "carrion-crawler-kind",
        "child",
        "closed-canopy",
        "coast",
        "cold",
        "cold-upwelling",
        "coral-head",
        "coral-reef",
        "crevasse-field",
        "damp-hollow",
        "dark",
        "day",
        "desert",
        "die",
        "dire-wolf-kind",
        "drink",
        "earth",
        "eat",
        "eclipse",
        "epipelagic",
        "erg",
        "eye",
        "felsenmeer",
        "fire",
        "fire-scrub",
        "fish-shoal",
        "foot",
        "ford",
        "forest-gap",
        "frost-heave",
        "gallery-forest",
        "giant-constrictor-snake-kind",
        "giant-crocodile-kind",
        "giant-elk-kind",
        "giant-goat-kind",
        "giant-hyena-kind",
        "giant-octopus-kind",
        "giant-scorpion-kind",
        "giant-squid-kind",
        "gloom",
        "gnoll-kind",
        "goblin-kind",
        "god",
        "grass-sward",
        "great",
        "green",
        "hadal-trench",
        "hamada",
        "hand",
        "hearth",
        "heat",
        "high",
        "hill",
        "hobgoblin-kind",
        "holdfast-tangle",
        "home",
        "hydrothermal-vent",
        "ice",
        "ice-lead",
        "island",
        "kelp-canopy",
        "kelp-forest",
        "killer-whale-kind",
        "kobold-kind",
        "lake",
        "liana-forest",
        "light",
        "lightless-water",
        "little",
        "low",
        "many",
        "marine-snow",
        "marsh",
        "melt-pond",
        "mesopelagic",
        "millet",
        "moon",
        "mossy-deadfall",
        "mountain",
        "mouth",
        "move",
        "muskeg",
        "name",
        "new",
        "night",
        "nodule-field",
        "north",
        "old",
        "old-growth",
        "one",
        "open-blue",
        "orange-dwarf",
        "orange-giant",
        "otyugh-kind",
        "over",
        "owlbear-kind",
        "parent",
        "person",
        "plankton-bloom",
        "playa",
        "pressure-ridge",
        "rafted-floe",
        "rain",
        "red",
        "red-dragon-kind",
        "red-dwarf",
        "red-giant",
        "reef-rubble",
        "reef-shark-kind",
        "reg",
        "rest",
        "rhinoceros-kind",
        "rice",
        "river",
        "rust-monster-kind",
        "sargassum-drift",
        "savanna",
        "scattering-layer",
        "sclerophyll-scrub",
        "scoured-ice",
        "sea",
        "sea-ice",
        "shadow",
        "shrieker-kind",
        "shrubland",
        "sibling",
        "sleep",
        "smoker-field",
        "snow",
        "snowfield",
        "south",
        "spirit",
        "spring",
        "spur-and-groove",
        "staghorn-stand",
        "star",
        "starlit",
        "stone",
        "sun",
        "sun-like-star",
        "taiga",
        "temperate-forest",
        "temperate-grassland",
        "temperate-rainforest",
        "thorn-scrub",
        "tide",
        "treant-kind",
        "tree",
        "trench-floor",
        "trench-wall",
        "tropical-rainforest",
        "tropical-seasonal-forest",
        "tuber",
        "tubeworm-thicket",
        "tundra",
        "twig-blight-kind",
        "twilight-water",
        "two",
        "under",
        "upwelling",
        "urchin-barren",
        "valley",
        "vent-plume",
        "vine",
        "water",
        "wheat",
        "white-dragon-kind",
        "white-dwarf",
        "wind",
        "wind-scour",
        "wooded-grassland",
        "woolly-mammoth-kind",
        "xorn-kind",
        "yellow",
        "yellow-dwarf",
        "yellow-white-dwarf",
    ];

    /// Every id in the snapshot resolves without degenerating. This is a
    /// smoke test over a broad, realistic sample of ids — it is NOT the
    /// enforcement that the live registry stays total. This crate cannot
    /// build the live, fully-composed registry (layering forbids depending
    /// on the domains that own most of its concepts — see
    /// `LIVE_REGISTRY_SNAPSHOT`'s doc comment), so nothing here can actually
    /// guarantee totality over the real world. That enforcement belongs in
    /// `cli/tests/`, which can build a full world; Task 4 adds it once the
    /// composition root assembles a `CommonVocabulary` to check.
    #[test]
    fn the_registry_snapshot_resolves_completely() {
        let mut registry = ConceptRegistry::default();
        for name in LIVE_REGISTRY_SNAPSHOT {
            with_concept(&mut registry, name);
        }
        let vocab = CommonVocabulary::build(&registry)
            .expect("every registered concept must have a Common word");
        for name in LIVE_REGISTRY_SNAPSHOT {
            let w = vocab.word_for(name);
            assert!(
                !w.contains('-') || vocab.is_declared(name),
                "{name} resolved to {w:?}, which still reads as a key"
            );
        }
    }

    /// A pathological id the naming convention has never had to handle on the
    /// live registry (a leading, trailing, or doubled hyphen) is exactly what
    /// [`build`]'s validation exists to catch — proof the check is not
    /// vacuous, even though nothing in today's registry trips it.
    #[test]
    fn build_rejects_a_malformed_id() {
        let mut registry = ConceptRegistry::default();
        with_concept(&mut registry, "-leading-hyphen");
        let err = CommonVocabulary::build(&registry).unwrap_err();
        assert_eq!(
            err.concepts,
            vec![("-leading-hyphen".to_string(), " leading hyphen".to_string())]
        );
    }

    /// `build` reports every failing concept in one pass, not just the first —
    /// a caller diagnosing a naming-convention gap should not have to fix one
    /// concept and re-run to discover the next.
    #[test]
    fn build_reports_every_failure_not_just_the_first() {
        let mut registry = ConceptRegistry::default();
        with_concept(&mut registry, "-leading-hyphen");
        with_concept(&mut registry, "trailing-hyphen-");
        let err = CommonVocabulary::build(&registry).unwrap_err();
        assert_eq!(err.concepts.len(), 2);
    }

    /// An empty registry is trivially total — there is nothing to resolve.
    #[test]
    fn an_empty_registry_builds() {
        let registry = ConceptRegistry::default();
        assert!(CommonVocabulary::build(&registry).is_ok());
    }
}
