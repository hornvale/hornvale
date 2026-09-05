//! The tongue view (Task 7): one clause, realized twice — in Common and in
//! the flagship settlement's own tongue, glossed side by side.
//!
//! # Why this view matters out of proportion to its size
//!
//! The peoples phase (culture, religion, species, deep time — everything
//! [`BuildDepth::Full`] adds over [`BuildDepth::Settlements`]) is 60% of the
//! whole startup wait (decision 0359), and until this view existed nothing
//! had anything to say during it: `sky` opens complete at rung 0, `atlas`
//! draws terrain from `Terrain` on, `almanac` grows steadily — but the
//! single most expensive phase of the build had no voice at all. This view
//! is that voice: a world genuinely SPEAKING, in a tongue this campaign's
//! whole language stack drew and evolved for exactly this species.
//!
//! # Step 0 (BLOCKING): the assembly path, verified against source
//!
//! Nothing outside `domains/language` and its own tests had ever called
//! [`hornvale_language::realize_tongue`] before this view. The task brief
//! traced a five-step chain and asked that it be run, not trusted. It was
//! run — a throwaway probe built exactly this chain against seed 42 at
//! [`BuildDepth::Full`] — and it produced a real answer:
//!
//! ```text
//! flagship settlement: "Doaba"  species: "bugbear"
//! common: "the Doaba are bugbears."
//! tongue: Ok("the Doaba Doodoo Dvo.")
//! grammar.order: Sov  copula: Some("Dvo")
//! lexicon entry for bugbear-kind: Root { … views: WordViews { roman: "Doodoo", … } }
//! ```
//!
//! The tongue's object word ("Doodoo") is **byte-identical** to the
//! lexicon's own generated root for `bugbear-kind` — not a coincidence,
//! proof that [`realize_tongue`] actually lexicalized through the built
//! [`hornvale_language::Lexicon`] rather than echoing Common. The call
//! chain that worked, exactly as the brief predicted:
//!
//! 1. **The species.** [`hornvale_terrain::places`] → the first place whose
//!    [`hornvale_settlement::IS_SETTLEMENT`] is committed → the flagship —
//!    → [`hornvale_species::species_of`].
//! 2. **The lexicon.** [`hornvale_worldgen::lexicon_from`].
//! 3. **The pronouns.** [`hornvale_worldgen::tongue_morphology_of`] →
//!    `.pronouns`.
//! 4. **The grammar.** [`hornvale_worldgen::language_of`] → `Phonology`,
//!    then [`hornvale_language::tongue_grammar`] → `TongueGrammar`.
//! 5. **Common's vocabulary.** [`hornvale_worldgen::common_vocabulary`].
//! 6. **The clause.** Constructed directly — see "The clause" below.
//!
//! `language_of` is used here rather than `language_of_in` (which the
//! brief's own interfaces section names, and which `driver.rs` calls for
//! name resolution): `language_of` is `language_of_in` over the
//! canonically-assembled [`hornvale_worldgen::WorldComponents`]
//! (`windows/worldgen/src/lib.rs`'s own doc: "byte-identical … on any roster
//! whose component set is `wc`"), and taking it avoids this view needing to
//! hold a `WorldComponents` at all — the same reduction `lexicon_from` (not
//! `lexicon_from_in`) already makes for the lexicon step.
//!
//! # THE PREMISE THE BRIEF GOT WRONG: species is a `Full`-only fact
//!
//! The brief's own resolution asserted that terrain AND climate are `Some`
//! "from the **Settlements** rung on, which is exactly the rung `tongue`
//! first speaks at" — true for the two artifacts, and it does not follow for
//! species. Verified two ways, not assumed:
//!
//! - **The ladder's own doc** (`windows/worldgen/src/lib.rs`'s
//!   [`BuildDepth`]): `Settlements` is "…plus settlement placement, naming,
//!   and glosses"; `Full` is "…plus culture, religion, **species**, and deep
//!   time". Species is named only at `Full`.
//! - **The build itself, run and inspected.** The single call site of
//!   [`hornvale_species::people`] (which commits the `PEOPLED_BY` fact
//!   [`hornvale_species::species_of`] reads) sits inside `build_to`'s
//!   `"culture+religion+species"` stage (`windows/worldgen/src/lib.rs:8725`),
//!   which begins at line 8601 — strictly AFTER the `depth <= BuildDepth::
//!   Settlements` early return at line 8583. Building seed 42 directly to
//!   `BuildDepth::Settlements` and reading the flagship settlement's own
//!   [`hornvale_species::species_of`] off the resulting ledger returned
//!   `None`; building the identical world to `BuildDepth::Full` returned
//!   `Some("bugbear")`. The settlement itself (`IS_SETTLEMENT`) exists at
//!   `Settlements`; which people lives there is committed only at `Full`.
//!
//! **Consequence: [`TongueView::can_speak`] gates on [`BuildDepth::Full`],
//! not [`BuildDepth::Settlements`].** The task brief's own Step 1 sketch
//! asserted the opposite (`can_speak(BuildDepth::Settlements)` truthy) — a
//! premise this module's own probe disproved before any code was written
//! against it, per contract rule 2: a view must not claim it can speak
//! somewhere it provably cannot back that claim with a real, ledger-backed
//! fact. `Full` is also the ladder's last rung, so this view renders
//! **at most once per session** — see "No memo" below.
//!
//! # The clause: "the `<settlement>` are `<species>`s."
//!
//! [`hornvale_language::Clause`]'s own doc gives exactly this shape as its
//! worked example ("the Vavako are goblins") — a classification the world
//! can honestly assert from a settlement's committed name plus its
//! committed species, with no invented content:
//!
//! - `predicate`: [`hornvale_kernel::world::IS_A`].
//! - `subject`: [`hornvale_language::Subject::Name`] of `"the {settlement
//!   name}"` — a proper name is not translated by either realizer (both
//!   `realize_common` and `realize_tongue` pass a `Subject::Name` through
//!   verbatim), which is exactly why the probe's two lines share "the
//!   Doaba" as their opening words: that is not a bug, it is what a name
//!   crossing a language boundary unchanged looks like.
//! - `object`: [`hornvale_language::Argument::Concept`] of `"{species}-kind"`
//!   — the concept id convention every kind roster in `domains/language`
//!   uses (`domains/language/src/accession.rs`'s own kind list: `"goblin-
//!   kind"`, `"bugbear-kind"`, …; `domains/language/src/account.rs`'s
//!   `NeededConcept::ObjectKind` derives the identical `"{object}-kind"`
//!   shape from a fact's object).
//! - `number`: [`hornvale_language::Number::Pl`] ("are… goblins").
//! - `definiteness`: [`hornvale_language::Definiteness::Indef`] — the
//!   complement is a bare plural generic ("goblins", not "the goblins"),
//!   which is what `Indef` + `Pl` renders (`domains/language/src/
//!   clause.rs`'s own determiner table: `(Indef, Pl) => {}`, bare).
//! - `evidential`: [`hornvale_language::Evidential::Witnessed`] — a
//!   `PEOPLED_BY` fact is a direct ledger commitment, not doctrine or
//!   inference.
//! - `tense`: [`hornvale_language::Tense::Present`]; `polarity`:
//!   [`hornvale_language::Polarity::Pos`] — an unqualified, affirmative
//!   statement of present fact.
//!
//! # `TongueGap` is shown, never hidden
//!
//! [`hornvale_language::realize_tongue`] can fail
//! ([`hornvale_language::TongueGap`]) if this species' lexicon has no entry
//! for its own `-kind` concept — reachable in principle even though seed
//! 42's bugbears do not hit it. [`TongueView::render`] shows the gap's own
//! `concept`/`reason` rather than falling back to Common's words for the
//! tongue line: a silent substitution would misrepresent a real linguistic
//! fact (this people has no word for its own kind) as if the tongue had
//! simply said "goblins" too, which contract rule 2 forbids as surely as an
//! empty placeholder does.
//!
//! # No memo
//!
//! [`View::render`]'s own doc explains why `&mut self` exists (the atlas's
//! ~200 ms `NearestVertexIndex`) and warns it is "a memo, not a licence".
//! This view takes no licence: [`TongueView::can_speak`] is true at exactly
//! one rung ([`BuildDepth::Full`], the ladder's last), and
//! `Frame::observe`'s own doc states it re-renders every CURRENTLY speaking
//! view at every rung — which for a view that starts and ends speaking at
//! the same, final rung means exactly one call per session. Building the
//! assembly fresh on that one call is not a repeated cost to amortize; see
//! "Cost" below for the measured number. (A resize afterward does not
//! re-render at all — `Frame::resize`'s own doc: an already-drawn panel is
//! merely clipped into the new region.)
//!
//! # Cost
//!
//! Measured 2026-08-28 on the M1 Max: building the lexicon, the C7
//! morphology bundle (pronouns) and the phonology/grammar pair for seed
//! 42's flagship people (bugbear) together cost **3.3-3.4 ms** across three
//! runs — a fraction of `lexicon_from`'s own "almost all of the post-name-
//! gloss census cost" characterization, which describes its weight across a
//! whole census's many metrics, not this view's single per-session call.
//! `tests::assembling_the_tongue_is_cheap_for_one_speaker` re-measures this
//! at test time and prints it (informative, not a gate — the same posture
//! `examples/overture_clone_cost.rs` and `examples/rung_bench.rs` take).

use crate::overture::view::View;
use hornvale_game_core::{Cell, Grid, Source, Weight};
use hornvale_kernel::World;
use hornvale_kernel::world::IS_A;
use hornvale_language::{
    Argument, Clause, Definiteness, Evidential, Lexicon, Number, Polarity, Subject, Tense,
    TongueGrammar, TongueMorphology, realize_common, realize_tongue,
};
use hornvale_worldgen::{BuildDepth, RungArtifacts};

/// The flagship settlement's own name and the species peopling it — the two
/// committed facts [`clause_for`] needs, resolved together so a caller
/// never has one without the other.
struct Flagship {
    /// The settlement's committed name (e.g. `"Doaba"`).
    name: String,
    /// The settlement's committed species (e.g. `"bugbear"`), from
    /// [`hornvale_species::species_of`].
    species: String,
}

/// The world's flagship settlement and its species, if both are committed —
/// `None` before any settlement exists, and (see the module doc's premise
/// correction) `None` at any rung before [`BuildDepth::Full`] even once a
/// settlement has been placed, because `PEOPLED_BY` is not committed until
/// then. The same "first-placed settlement, in commit order" idiom
/// `flagship_of`/`flagship_vertex` use in `windows/worldgen/src/lib.rs`.
fn flagship_of(world: &World) -> Option<Flagship> {
    let place = hornvale_terrain::places(world).into_iter().find(|p| {
        world
            .ledger
            .value_of(p.id, hornvale_settlement::IS_SETTLEMENT)
            .is_some()
    })?;
    let species = hornvale_species::species_of(world, place.id)?;
    Some(Flagship {
        name: place.name,
        species,
    })
}

/// The classification clause this view realizes both ways — see the module
/// doc's "The clause" section for why each field is what it is. A pure
/// function of `flagship` alone: given the same settlement name and
/// species, always the same clause.
fn clause_for(flagship: &Flagship) -> Clause {
    Clause {
        predicate: IS_A.to_string(),
        subject: Subject::Name(format!("the {}", flagship.name)),
        object: Argument::Concept(format!("{}-kind", flagship.species)),
        number: Number::Pl,
        definiteness: Definiteness::Indef,
        evidential: Evidential::Witnessed,
        tense: Tense::Present,
        polarity: Polarity::Pos,
        adjuncts: vec![],
    }
}

/// The three built structures [`realize_tongue`] needs beyond the clause
/// itself and the tongue's own drawn grammar — assembled together because
/// all three are cheap only in combination (see the module doc's "Cost"
/// section) and none is meaningful without the other two.
struct TongueAssembly {
    /// This species' generated vocabulary.
    lexicon: Lexicon,
    /// This species' C7 morphology bundle — `.pronouns` is what
    /// [`realize_tongue`] wants; the rest rides along because
    /// [`hornvale_worldgen::tongue_morphology_of`] returns the whole bundle.
    morph: TongueMorphology,
    /// This species' drawn constituent order, copula and (where drawn)
    /// subordinator/conjunction forms.
    grammar: TongueGrammar,
}

/// Build [`TongueAssembly`] for `species`, given the terrain and climate
/// `artifacts` is already holding (never re-derived — `terrain_of`/
/// `climate_from` are banned from this crate's call sites, decision 0092).
/// `None` on any failure: [`hornvale_worldgen::lexicon_from`] and
/// [`hornvale_worldgen::tongue_morphology_of`] both return `Result`, but
/// every error variant they can produce (a malformed roster, an unknown
/// kind) should not occur for a species this build genuinely placed and
/// peopled — defensive only, like `AtlasView::render`'s own `terrain` check.
///
/// **Reads `artifacts` structurally, never by naming
/// `hornvale_terrain::GeneratedTerrain`/`hornvale_climate::GeneratedClimate`
/// explicitly.** Destructuring `RungArtifacts`'s two fields and forwarding
/// them straight into `lexicon_from` lets type inference resolve both
/// without this crate needing a `hornvale-climate` dependency at all — the
/// task brief's own finding.
fn assemble(world: &World, species: &str, artifacts: RungArtifacts<'_>) -> Option<TongueAssembly> {
    let terrain = artifacts.terrain?;
    let climate = artifacts.climate?;
    let lexicon = hornvale_worldgen::lexicon_from(world, species, terrain, climate).ok()?;
    let morph = hornvale_worldgen::tongue_morphology_of(world, species).ok()?;
    let ph = hornvale_worldgen::language_of(world, species);
    let grammar = hornvale_language::tongue_grammar(&world.seed, species, &ph);
    Some(TongueAssembly {
        lexicon,
        morph,
        grammar,
    })
}

/// `species` with its first character capitalized — the tongue line's own
/// label ("Bugbear:"), since a tongue has no other name in this campaign's
/// data model.
fn capitalized(species: &str) -> String {
    let mut chars = species.chars();
    match chars.next() {
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
        None => String::new(),
    }
}

/// Write `line` starting at column 0 of `row`, clipping at the grid's right
/// edge — the same minimal copy `sky.rs`'s `write_caption` and
/// `almanac.rs`'s `write_line` each keep privately rather than sharing.
fn write_line(grid: &mut Grid, row: u16, line: &str) {
    for (i, ch) in line.chars().enumerate() {
        let Ok(col) = u16::try_from(i) else { break };
        if col >= grid.width() {
            break;
        }
        grid.set(col, row, Cell::glyph(ch, Weight::Normal, Source::Overture));
    }
}

/// The tongue: one clause, realized in Common on the first row and in the
/// flagship people's own tongue on the second — see the module doc for the
/// clause, the `TongueGap` handling, and why this view speaks at
/// [`BuildDepth::Full`] alone.
///
/// No memo (see the module doc's "No memo" section): this view speaks at
/// exactly one rung, so `render` is called at most once per session and
/// there is nothing to amortize by keeping `self` mutable in practice.
#[derive(Debug, Default, Clone, Copy)]
pub struct TongueView;

impl TongueView {
    /// One clause realized both ways for `world`, or `None` if this world
    /// has no flagship-and-species to speak of, or the tongue realization
    /// gapped. Factored out of [`Self::render`] as the inherent helper the
    /// task brief's own non-vacuity test calls directly (`(common, tongue)
    /// = tongue_view().sample(&world).expect("a clause")`), independent of
    /// grid layout.
    ///
    /// Deliberately requires the tongue realization to SUCCEED (returns
    /// `None` on a [`hornvale_language::TongueGap`]) — this helper answers
    /// "what would a genuinely two-voiced sample look like", which a
    /// gapped clause is not; [`Self::render`] is the one place a gap is
    /// shown rather than treated as absence.
    pub fn sample(&self, world: &World, artifacts: RungArtifacts<'_>) -> Option<(String, String)> {
        let flagship = flagship_of(world)?;
        let assembly = assemble(world, &flagship.species, artifacts)?;
        let vocab = hornvale_worldgen::common_vocabulary(&world.registry);
        let clause = clause_for(&flagship);
        let common = realize_common(&clause, &vocab);
        let tongue = realize_tongue(
            &clause,
            &assembly.grammar,
            &assembly.lexicon,
            &assembly.morph.pronouns,
        )
        .ok()?;
        Some((common, tongue))
    }
}

impl View for TongueView {
    fn name(&self) -> &'static str {
        "tongue"
    }

    /// Speaks from [`BuildDepth::Full`] alone — see the module doc's
    /// premise-correction section for why `Settlements` (the brief's
    /// original guess) is not honest: a settlement exists there, but which
    /// people lives in it is not yet committed to the ledger.
    fn can_speak(&self, rung: BuildDepth) -> bool {
        rung >= BuildDepth::Full
    }

    fn render(
        &mut self,
        world: &World,
        _rung: BuildDepth,
        artifacts: RungArtifacts<'_>,
        w: u16,
        h: u16,
    ) -> Grid {
        let w = w.max(1);
        let h = h.max(1);
        let mut grid = Grid::new(w, h);

        // Defensive only, like `AtlasView::render`'s own `terrain` check:
        // the frame never calls `render` when `can_speak` returned `false`,
        // so a `None` here is unreached on the shipped path.
        let Some(flagship) = flagship_of(world) else {
            return grid;
        };
        let Some(assembly) = assemble(world, &flagship.species, artifacts) else {
            return grid;
        };

        let vocab = hornvale_worldgen::common_vocabulary(&world.registry);
        let clause = clause_for(&flagship);
        let common = realize_common(&clause, &vocab);
        write_line(&mut grid, 0, &format!("Common: {common}"));

        let label = capitalized(&flagship.species);
        match realize_tongue(
            &clause,
            &assembly.grammar,
            &assembly.lexicon,
            &assembly.morph.pronouns,
        ) {
            Ok(tongue) => write_line(&mut grid, 1, &format!("{label}: {tongue}")),
            Err(gap) => write_line(
                &mut grid,
                1,
                &format!("{label}: [no word for {}: {}]", gap.concept, gap.reason),
            ),
        }
        grid
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_astronomy::SkyPins;
    use hornvale_kernel::Seed;
    use hornvale_terrain::TerrainPins;
    use hornvale_worldgen::{
        BuildArtifacts, SettlementPins, WorldComponents, build_world_to_with_artifacts,
    };
    use std::sync::OnceLock;

    /// A real seed-42 build's artifacts at `depth`, built once per test
    /// binary — the same reasoning `atlas.rs`'s own `artifacts_at` gives.
    fn artifacts_at(depth: BuildDepth) -> &'static BuildArtifacts {
        static TERRAIN: OnceLock<BuildArtifacts> = OnceLock::new();
        static SETTLEMENTS: OnceLock<BuildArtifacts> = OnceLock::new();
        static FULL: OnceLock<BuildArtifacts> = OnceLock::new();
        let build = || {
            let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
            build_world_to_with_artifacts(
                Seed(42),
                &SkyPins::default(),
                &TerrainPins::default(),
                &SettlementPins::default(),
                &wc,
                depth,
            )
            .expect("seed 42 builds")
        };
        match depth {
            BuildDepth::Terrain => TERRAIN.get_or_init(build),
            BuildDepth::Settlements => SETTLEMENTS.get_or_init(build),
            BuildDepth::Full => FULL.get_or_init(build),
            other => panic!("no cached build for {other:?}; add one deliberately"),
        }
    }

    fn rung_artifacts(built: &BuildArtifacts) -> RungArtifacts<'_> {
        RungArtifacts {
            terrain: built.terrain.as_ref(),
            climate: built.climate.as_ref(),
        }
    }

    fn tongue_view() -> TongueView {
        TongueView
    }

    fn full_world() -> &'static BuildArtifacts {
        artifacts_at(BuildDepth::Full)
    }

    #[test]
    fn one_clause_is_realized_in_common_and_in_the_tongue() {
        // The interlinear: ONE structure, two realizations. If the two are
        // equal the tongue is not doing anything, which is the vacuity this
        // test exists to exclude.
        let built = full_world();
        let (common, tongue) = tongue_view()
            .sample(&built.world, rung_artifacts(built))
            .expect("seed 42's flagship people realizes a non-gapped clause");
        assert!(!common.is_empty() && !tongue.is_empty());
        assert_ne!(
            common, tongue,
            "the tongue realization is identical to Common; no language was applied"
        );
        // NON-VACUITY, stronger than mere inequality (two garbage strings
        // are also unequal): both realizations open on the SAME subject
        // text, because a proper name crosses a language boundary
        // unchanged (module doc, "The clause"). If the tongue's own text
        // did not even carry the shared name, `realize_tongue` would not
        // have run against the SAME clause `realize_common` did.
        let flagship = flagship_of(&built.world).expect("a flagship exists");
        let subject_text = format!("the {}", flagship.name);
        assert!(
            common.contains(&subject_text),
            "Common's own realization lost the subject: {common:?}"
        );
        assert!(
            tongue.contains(&subject_text),
            "the tongue's own realization lost the subject it should share with Common: {tongue:?}"
        );
        // And the discriminating half: the tongue's own OBJECT word must be
        // exactly the lexicon's own generated root for this species' own
        // `-kind` concept — proof the realization came from the built
        // Lexicon rather than from Common's vocabulary or a fixed string.
        let assembly = assemble(&built.world, &flagship.species, rung_artifacts(built))
            .expect("assembly succeeds for seed 42's flagship people");
        let concept = format!("{}-kind", flagship.species);
        let root_word = match assembly
            .lexicon
            .entry(&concept)
            .expect("seed 42's flagship species has an entry for its own kind")
        {
            hornvale_language::LexEntry::Root { views, .. } => views.roman.clone(),
            other => panic!("expected a Root entry for {concept}, got {other:?}"),
        };
        assert!(
            tongue.contains(&root_word),
            "the tongue text did not contain the lexicon's own root word {root_word:?} \
             for {concept}: {tongue:?}"
        );
    }

    #[test]
    fn the_tongue_is_silent_before_there_are_peoples() {
        // Corrected from the task brief's own Step 1 sketch (see the module
        // doc's premise-correction section): a settlement exists from
        // `Settlements` on, but `PEOPLED_BY` — the fact `flagship_of`
        // depends on through `hornvale_species::species_of` — is not
        // committed until `Full`. `can_speak` is a pure function of `rung`
        // alone (the trait's own contract), so this is checked
        // independently of any built world.
        assert!(!tongue_view().can_speak(BuildDepth::Terrain));
        assert!(!tongue_view().can_speak(BuildDepth::Settlements));
        assert!(tongue_view().can_speak(BuildDepth::Full));
    }

    #[test]
    fn flagship_of_finds_no_species_at_settlements_even_though_a_settlement_exists() {
        // The empirical half of the premise correction: NOT a synthetic
        // fixture, but seed 42 built for real to exactly `Settlements` and
        // read back. Proves the ledger itself, not just the source's stage
        // ordering, backs `can_speak`'s corrected rung.
        let built = artifacts_at(BuildDepth::Settlements);
        let has_settlement = hornvale_terrain::places(&built.world).into_iter().any(|p| {
            built
                .world
                .ledger
                .value_of(p.id, hornvale_settlement::IS_SETTLEMENT)
                .is_some()
        });
        assert!(
            has_settlement,
            "seed 42 should place at least one settlement by Settlements"
        );
        assert!(
            flagship_of(&built.world).is_none(),
            "a settlement's species must not be readable before Full"
        );
    }

    #[test]
    fn sample_is_none_before_full() {
        // The registry-level guard, exercised directly (bypassing
        // `can_speak`) the same way `almanac.rs`'s trap tests call a
        // component's `render` straight through: proves `sample` itself is
        // safe at an early rung, independent of the gate that (redundantly)
        // also protects it.
        let built = artifacts_at(BuildDepth::Settlements);
        assert!(
            tongue_view()
                .sample(&built.world, rung_artifacts(built))
                .is_none(),
            "sample must not fabricate a clause before a species is committed"
        );
    }

    #[test]
    fn render_shows_both_lines_at_full() {
        let built = full_world();
        let mut view = tongue_view();
        let grid = view.render(
            &built.world,
            BuildDepth::Full,
            rung_artifacts(built),
            78,
            20,
        );
        let text = grid.to_plain_text();
        assert!(
            text.contains("Common:"),
            "the tongue view drew no Common line: {text:?}"
        );
        let flagship = flagship_of(&built.world).expect("a flagship exists");
        let label = capitalized(&flagship.species);
        assert!(
            text.contains(&format!("{label}:")),
            "the tongue view drew no {label} line: {text:?}"
        );
    }

    #[test]
    fn render_is_pure_given_the_same_arguments() {
        // `View::render`'s own limit on `&mut self`: two calls with the
        // same arguments must return the same grid, even for a view that
        // keeps no memo at all.
        let built = full_world();
        let mut view = tongue_view();
        let first = view.render(
            &built.world,
            BuildDepth::Full,
            rung_artifacts(built),
            78,
            20,
        );
        let second = view.render(
            &built.world,
            BuildDepth::Full,
            rung_artifacts(built),
            78,
            20,
        );
        assert_eq!(first.to_plain_text(), second.to_plain_text());
    }

    #[test]
    fn render_before_full_is_blank_rather_than_a_placeholder() {
        // Defensive-path coverage: `render` called at a rung `can_speak`
        // never actually allows through the frame, proving the early
        // `None` returns produce an honest blank rather than a panic or a
        // fabricated line (contract rule 2, the failure mode this campaign
        // watches hardest).
        let built = artifacts_at(BuildDepth::Settlements);
        let mut view = tongue_view();
        let grid = view.render(
            &built.world,
            BuildDepth::Settlements,
            rung_artifacts(built),
            78,
            20,
        );
        let text = grid.to_plain_text();
        assert!(
            !text.contains("Common:"),
            "the tongue view spoke before a species was committed: {text:?}"
        );
    }

    #[test]
    fn assembling_the_tongue_is_cheap_for_one_speaker() {
        // INFORMATIVE, not a gate — the same posture
        // `examples/overture_clone_cost.rs` and `examples/rung_bench.rs`
        // take for a per-session cost worth a committed number. See the
        // module doc's "Cost" section for the measured range this backs.
        //
        // The timer starts AFTER `full_world()` deliberately: that call's
        // first invocation, in this binary, builds the whole world
        // (~1.7 s in `--release`, dwarfing the assembly this test means to
        // measure) through its `OnceLock::get_or_init`, and only the
        // assembly step is the cost this test is about. Also run with
        // `--release`: a debug build measures the optimizer, not the code
        // (the module doc's committed 3.3-3.4 ms figure is a `--release`
        // number; a debug run of this same test prints a much larger value
        // for the same reason `overture_clone_cost.rs`'s own doc names).
        let built = full_world();
        let flagship = flagship_of(&built.world).expect("a flagship exists");
        #[allow(clippy::disallowed_types)]
        let started = std::time::Instant::now();
        let assembly = assemble(&built.world, &flagship.species, rung_artifacts(built));
        #[allow(clippy::disallowed_types)]
        let elapsed_ms = started.elapsed().as_secs_f64() * 1000.0;
        assert!(assembly.is_some(), "assembly must succeed for seed 42");
        eprintln!(
            "tongue assembly for {}: {elapsed_ms:.3} ms",
            flagship.species
        );
    }

    #[test]
    fn a_tongue_gap_is_shown_honestly_not_hidden() {
        // Mutation-in-miniature: a clause whose object concept has no
        // lexicon entry at all must surface the gap's own text, never fall
        // back to Common's word for the same concept. Exercises
        // `realize_tongue`'s real gap path directly against a real, built
        // lexicon and grammar (no synthetic fixture), by asking for a
        // concept this species' lexicon was never built to cover.
        let built = full_world();
        let flagship = flagship_of(&built.world).expect("a flagship exists");
        let assembly = assemble(&built.world, &flagship.species, rung_artifacts(built))
            .expect("assembly succeeds for seed 42's flagship people");
        let clause = Clause {
            predicate: IS_A.to_string(),
            subject: Subject::Name(format!("the {}", flagship.name)),
            object: Argument::Concept("no-such-concept-in-any-lexicon".to_string()),
            number: Number::Pl,
            definiteness: Definiteness::Indef,
            evidential: Evidential::Witnessed,
            tense: Tense::Present,
            polarity: Polarity::Pos,
            adjuncts: vec![],
        };
        let gap = realize_tongue(
            &clause,
            &assembly.grammar,
            &assembly.lexicon,
            &assembly.morph.pronouns,
        )
        .expect_err("a concept with no lexicon entry at all must gap");
        assert_eq!(gap.concept, "no-such-concept-in-any-lexicon");
        assert!(
            !gap.reason.is_empty(),
            "a TongueGap must carry a real, recountable reason"
        );
    }
}
