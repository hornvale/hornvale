//! The wear survival funnel, made reproducible (The Witness, Task 1b).
//!
//! The Wearing published a funnel — 940 morphemes in settlement names, 611
//! clearing the frequency floor, 53 the drawn wear cascade altered, 40
//! rejected by the survival guard, 14 carrying surviving wear — measured
//! "across four sampled worlds" and never recorded which four. Nothing on
//! main reproduced it (see `.superpowers/sdd/baseline-report.md`, Figure 4),
//! so H1 (`docs/superpowers/specs/2026-07-30-the-witness-design.md` §6) had
//! no baseline to be evaluated against: a preregistered prediction whose
//! readout cannot be recomputed is a test that asserts nothing.
//!
//! # The seed list IS the instrument's identity
//!
//! [`FUNNEL_SEEDS`] is named, fixed, and committed. It is not derived, not
//! randomized, and not configurable by environment variable — changing it
//! invalidates every comparison ever made against a run of this test. Task 9
//! re-runs this exact instrument, unmodified, over the exact same four
//! seeds, and compares rung-by-rung against whatever this file prints on the
//! pre-repair tree.
//!
//! # What each rung measures, and how
//!
//! Only [`hornvale_language`]'s and [`hornvale_worldgen`]'s **public** API is
//! used — no private function in `domains/language/src/naming.rs` is called.
//! Where the real pipeline's private machinery (`Namer::worn_compound`, the
//! give-up ladder, `contains_run`, `repair_phonotactics`) cannot be reached
//! from outside the crate, this instrument recomposes the same computation
//! from public primitives that do the identical arithmetic, or — where that
//! is not possible — says so plainly rather than inventing a proxy:
//!
//! 1. **Morphemes in settlement names.** Every committed `name-gloss` fact
//!    (`hornvale_kernel::NAME_GLOSS`) is parsed back into its constituent
//!    concepts via the same segmentation [`windows/lab/src/metrics.rs`]'s
//!    `name_gloss_true`/`name_transparency` metrics use
//!    (`hornvale_worldgen::settlement_site_concepts` plus the presiding-
//!    phenomenon concepts, since the cheap `presiding: None` route is used
//!    here rather than re-deriving each settlement's observed sky). A gloss
//!    that does not parse **uniquely** against that vocabulary is skipped —
//!    known undercount, see "Known limitations" below. Each concept in a
//!    unique parse is one morpheme instance.
//! 2. **Clears the frequency floor.** The corpus is re-derived directly from
//!    the committed glosses, per species, exactly as
//!    `domains/language/src/naming.rs`'s own two-pass composition does it:
//!    "the share of this species' named settlements whose gloss contains the
//!    concept." [`WEAR_FLOOR_DUPLICATE`] is compared against it — see that
//!    constant's own doc comment for why it is a duplicate rather than an
//!    import.
//! 3. **The drawn cascade alone alters it — H1's actual quantity.** H1 is a
//!    claim about *the cascade's match rate* specifically (the idea registry
//!    names that lever outright: "the lever is the cascade's match rate, NOT
//!    the survival guard"). `Namer::sounded` is the cascade-only limb, and it
//!    is private — but its entire body is three calls to public API:
//!
//!    ```text
//!    if frequency < WEAR_FLOOR { return segments.to_vec(); }
//!    let cascade = draw_wear_cascade(&self.seed, &self.species);
//!    evolve(segments, &cascade, self.ph).modern
//!    ```
//!
//!    so this restates exactly that (`draw_wear_cascade` + `evolve`, both
//!    `pub`) against each morpheme's citation segments, rather than calling
//!    `Namer::wear` (which folds in positional reduction too, see rung 3b
//!    below). Restating is the right relationship here for the same reason
//!    the lab's own gate table restates worldgen's `Steeped` predicates
//!    (`independently_steeped_concepts`'s module note): it keeps this a
//!    second opinion on the cascade specifically, not an echo of the wider
//!    `Namer::wear`. `sounded` itself is NOT made `pub` for this — the
//!    restatement is deliberately kept in the test, at arm's length from the
//!    production code it measures.
//!
//!    **Rung 3b (informational, not partitioned further): wear (cascade +
//!    positional reduction) alters it.** The combined effect of
//!    [`hornvale_language::Namer::wear`], the one public entry point that
//!    performs the FULL wear treatment on a standalone morpheme. Reported
//!    separately, alongside rung 3, precisely so the two can be compared:
//!    the difference between them is a direct measurement of how much of
//!    the pipeline's observed wear is positional reduction rather than
//!    sound change — informative on its own, and no longer misleading now
//!    that it is not conflated with rung 3. Rungs 4 and 5 below descend
//!    from rung 3 (the cascade-only rung), not from this one.
//! 4. **Rejected by the guard, or carries surviving wear.** For every
//!    morpheme rung 3 altered, this checks whether the settlement's own
//!    committed, final `name` fact still contains the CASCADE-ALTERED
//!    form's romanization as a contiguous substring (case- and combining-
//!    diacritic-insensitive, via a local restatement of `metrics.rs`'s
//!    private `bare_surface`). This is the external, string-level analogue
//!    of the real survival guard's `contains_run` check, applied to the REAL
//!    committed output of the REAL pipeline (not a re-simulation of the
//!    give-up ladder) — the give-up ladder is private and unreachable, but
//!    its observable EFFECT (does the final name still say the worn
//!    morpheme, in one piece) is exactly what this checks. **Both counts are
//!    measured directly, per morpheme instance, by this containment check —
//!    neither is derived by subtracting the other from rung 3.** If the
//!    check finds the cascade-altered form present, the wear survived
//!    (rung 5); if not, either the real survival guard rejected it, or the
//!    real pipeline's positional reduction changed the surface further on
//!    top of the cascade change in a way this check cannot distinguish from
//!    rejection (rung 4) — an imprecision inherent to observing a private
//!    ladder's output rather than its internals, disclosed here rather than
//!    concealed.
//!
//! # Known limitations (say-so, not proxies)
//!
//! - **Undercounts rung 1.** A settlement whose gloss does not parse
//!   uniquely against the cheap (non-presiding) site vocabulary is skipped
//!   entirely, rather than counted. The real pipeline glosses every
//!   settlement it names; this instrument only counts the ones it can
//!   independently re-derive a vocabulary for.
//! - **[`WEAR_FLOOR_DUPLICATE`] is a private constant, restated.**
//!   `domains/language/src/naming.rs`'s `WEAR_FLOOR` is not `pub`. If it ever
//!   changes there without a corresponding update here, this instrument's
//!   rung 2 (and everything downstream of it) silently drifts from what the
//!   pipeline actually does — the same failure mode this campaign's keystone
//!   guards exist to close in the naming code, now confessed in its own
//!   measuring instrument instead of hidden.
//! - **Rung 4's "rejected" reading can be confounded by rung 3b's reduction
//!   on the SAME morpheme.** The containment check in point 4 above tests
//!   only whether the cascade-only form survives; it says nothing about
//!   whether a further positional reduction (rung 3b) also touched that same
//!   morpheme in the real committed name. A morpheme that is both cascade-
//!   altered and reduced can read as "rejected" here even when the real
//!   survival guard accepted its (reduced) wear — this instrument cannot
//!   tell the two apart from the committed surface alone. Disclosed rather
//!   than hidden; resolving it would require reaching the private give-up
//!   ladder itself.
//!
//! # Output
//!
//! Run with `--nocapture` so the funnel is copy-pasteable into a report:
//!
//! ```text
//! cargo nextest run -p hornvale-lab --test wear_funnel -- --nocapture
//! ```
//!
//! The assertion is deliberately a **loose floor** — non-empty and
//! monotonically narrowing rung over rung — never a tight pin. A tight
//! assertion here would freeze whatever this pipeline currently does into a
//! golden, which is the "drift checks freeze bugs" failure this campaign is
//! actively trying to change the pipeline out from under.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{CellId, Seed, Value};
use hornvale_lab::FullView;
use hornvale_language::{
    Headedness, LexEntry, Lexicon, Namer, Phonology, Segment, draw_wear_cascade, evolve,
    render_views,
};
use std::collections::{BTreeMap, BTreeSet};

/// The instrument's identity. **Named, fixed, and committed — do not derive
/// it, do not randomize it, and do not make it configurable by environment
/// variable.** Every comparison this instrument's output is ever used for
/// (Task 9's post-repair readout, and any future re-run) is a comparison
/// against a run over exactly these four seeds. Changing this list starts a
/// new instrument, not a refinement of this one — it invalidates every prior
/// reading.
const FUNNEL_SEEDS: [u64; 4] = [0, 1, 2, 3];

/// A restatement of `domains/language/src/naming.rs`'s private `WEAR_FLOOR`
/// (currently `0.25`, at that file's line ~279). Not importable — the
/// constant is not `pub`. See the module doc's "Known limitations" for the
/// drift risk this duplication carries.
const WEAR_FLOOR_DUPLICATE: f64 = 0.25;

/// A restatement of `windows/lab/src/metrics.rs`'s private
/// `PRESIDING_CONCEPTS` — the exact codomain of that module's
/// `phenomenon_concept`, i.e. every concept a settlement's presiding sky
/// phenomenon can contribute to its site vector and therefore to its gloss.
/// Needed because `hornvale_worldgen::settlement_site_concepts` is called
/// here with `presiding: None` (the cheap route `name_transparency` also
/// uses, avoiding re-deriving each settlement's own observed sky), so these
/// five must be added to the candidate vocabulary by hand or a gloss naming
/// one of them fails to parse and the settlement is silently skipped.
const PRESIDING_CONCEPTS: &[&str] = &["day", "moon", "star", "sun", "wind"];

/// Every way `gloss` reads as a `"-"`-joined sequence of `vocab` members.
/// A restatement of `windows/lab/src/metrics.rs`'s private `gloss_parses`
/// (same algorithm, same rationale: a biome concept id can itself be
/// hyphenated, e.g. `tropical-seasonal-forest`, so a gloss cannot simply be
/// `split('-')`). Returns every segmentation so the caller can require a
/// unique one.
fn gloss_parses<'a>(gloss: &str, vocab: &BTreeSet<&'a str>) -> Vec<Vec<&'a str>> {
    let mut out: Vec<Vec<&'a str>> = Vec::new();
    for word in vocab {
        if gloss == *word {
            out.push(vec![word]);
        } else if let Some(rest) = gloss
            .strip_prefix(*word)
            .and_then(|rest| rest.strip_prefix('-'))
        {
            for tail in gloss_parses(rest, vocab) {
                let mut parse = vec![*word];
                parse.extend(tail);
                out.push(parse);
            }
        }
    }
    out
}

/// A restatement of `windows/lab/src/metrics.rs`'s private `bare_surface`:
/// lowercase, and strip combining diacritics, so a citation/worn form's
/// romanization can be compared against a committed surface name
/// case-insensitively and accent-insensitively.
fn bare_surface(name: &str) -> String {
    name.to_lowercase()
        .chars()
        .filter(|c| !('\u{0300}'..='\u{036f}').contains(c))
        .collect()
}

/// The full modern-form segments `concept` resolves to in `lexicon`: a
/// root's own evolved segments, or a compound's two component roots
/// rejoined in `lexicon`'s drawn headedness order. A restatement of
/// `domains/language/src/naming.rs`'s private `concept_segments` +
/// `join_by_headedness` — both trivial, mechanical, and cited here rather
/// than reimplemented as a "concept": a `Root` entry's `derivation.modern`
/// is read directly, and a `Compound` entry is rejoined exactly as
/// `Namer::join_parts`'s doc comment describes. Returns an empty vector for
/// a `Gap` or unresolvable entry (mirroring the private original, which is
/// total for the same reason: `Namer::glossed_name` never picks a concept
/// `holds_word` has not already accepted).
fn concept_segments_ext(lexicon: &Lexicon, concept: &str) -> Vec<Segment> {
    match lexicon.entry(concept) {
        Some(LexEntry::Root { derivation, .. }) => derivation.modern.clone(),
        Some(LexEntry::Compound { modifier, head, .. }) => {
            let modifier_segs = concept_segments_ext(lexicon, modifier);
            let head_segs = concept_segments_ext(lexicon, head);
            match lexicon.headedness {
                Headedness::HeadFirst => head_segs.into_iter().chain(modifier_segs).collect(),
                Headedness::HeadLast => modifier_segs.into_iter().chain(head_segs).collect(),
            }
        }
        _ => Vec::new(),
    }
}

/// One settlement's re-derived naming data, gathered once per seed so the
/// corpus pass below never re-reads the ledger.
struct NamedSettlement {
    species: String,
    /// The unique parse of this settlement's committed gloss, in draw order.
    concepts: Vec<String>,
    /// The committed, final `name` fact — the real pipeline's actual output,
    /// used only to check whether a worn form's romanization still appears
    /// in it, never re-derived or re-simulated.
    surface: String,
}

#[test]
fn the_wear_funnel_is_reproducible() {
    let mut total_morphemes = 0usize;
    let mut clears_floor = 0usize;
    let mut cascade_alters = 0usize;
    let mut combined_alters = 0usize;
    let mut rejected_by_guard = 0usize;
    let mut carries_surviving_wear = 0usize;

    for &seed in &FUNNEL_SEEDS {
        let view = FullView::build(Seed(seed), &SkyPins::default())
            .unwrap_or_else(|e| panic!("seed {seed}: world failed to build: {e:?}"));
        let world = view.world();
        let terrain = view.terrain();
        let climate = view.climate();

        // Gather every settlement whose gloss parses uniquely against the
        // cheap (non-presiding) site vocabulary plus the presiding concepts.
        let mut settlements: Vec<NamedSettlement> = Vec::new();
        for f in world.ledger.find(hornvale_settlement::IS_SETTLEMENT) {
            let id = f.subject;
            let Some(gloss) = world.ledger.text_of(id, hornvale_kernel::NAME_GLOSS) else {
                continue;
            };
            if gloss.is_empty() {
                continue;
            }
            let Some(species) = hornvale_species::species_of(world, id) else {
                continue;
            };
            let Some(Value::Number(cell)) = world.ledger.value_of(id, hornvale_settlement::CELL_ID)
            else {
                continue;
            };
            let Some(name) = world.ledger.text_of(id, hornvale_kernel::NAME) else {
                continue;
            };
            let mut vocab: BTreeSet<&str> = hornvale_worldgen::settlement_site_concepts(
                &world.seed,
                CellId(*cell as u32),
                terrain,
                climate,
                None,
            )
            .into_iter()
            .collect();
            vocab.extend(PRESIDING_CONCEPTS.iter().copied());
            let parses = gloss_parses(gloss, &vocab);
            if parses.len() != 1 {
                // Ambiguous or unparseable against this cheap vocabulary —
                // skip rather than guess (see the module doc's known
                // undercount).
                continue;
            }
            settlements.push(NamedSettlement {
                species,
                concepts: parses[0].iter().map(|s| s.to_string()).collect(),
                surface: name.to_string(),
            });
        }

        // The corpus, re-derived directly from the committed glosses above:
        // per species, the share of its named settlements whose (unique)
        // gloss parse contains each concept — exactly the statistic
        // `domains/language/src/naming.rs`'s own two-pass composition
        // counts.
        let mut per_species_total: BTreeMap<String, usize> = BTreeMap::new();
        let mut per_species_concept_count: BTreeMap<(String, String), usize> = BTreeMap::new();
        for s in &settlements {
            *per_species_total.entry(s.species.clone()).or_insert(0) += 1;
            let distinct: BTreeSet<&String> = s.concepts.iter().collect();
            for concept in distinct {
                *per_species_concept_count
                    .entry((s.species.clone(), concept.clone()))
                    .or_insert(0) += 1;
            }
        }
        let frequency_of = |species: &str, concept: &str| -> f64 {
            let total = *per_species_total.get(species).unwrap_or(&0);
            if total == 0 {
                return 0.0;
            }
            let count = *per_species_concept_count
                .get(&(species.to_string(), concept.to_string()))
                .unwrap_or(&0);
            count as f64 / total as f64
        };

        // One lexicon and one phonology per species, built once (the real
        // pipeline's own composition-root entry points).
        let mut lexicons: BTreeMap<String, Lexicon> = BTreeMap::new();
        let mut phonologies: BTreeMap<String, Phonology> = BTreeMap::new();

        for s in &settlements {
            if !lexicons.contains_key(&s.species) {
                let lexicon = hornvale_worldgen::lexicon_from(world, &s.species, terrain, climate)
                    .unwrap_or_else(|e| panic!("seed {seed}: lexicon for {}: {e:?}", s.species));
                lexicons.insert(s.species.clone(), lexicon);
            }
            let ph = phonologies
                .entry(s.species.clone())
                .or_insert_with(|| hornvale_worldgen::language_of(world, &s.species));
            let lexicon = &lexicons[&s.species];
            let namer = Namer::new(&world.seed, &s.species, ph);

            for concept in &s.concepts {
                total_morphemes += 1;
                let frequency = frequency_of(&s.species, concept);
                if frequency < WEAR_FLOOR_DUPLICATE {
                    continue;
                }
                clears_floor += 1;

                let citation = concept_segments_ext(lexicon, concept);
                if citation.is_empty() {
                    // A concept with no lexicon entry (or a Gap) cannot be
                    // worn — not reachable in practice (see
                    // `concept_segments_ext`'s doc comment) but handled
                    // rather than assumed.
                    continue;
                }

                // Rung 3b (informational): the combined treatment, cascade
                // and positional reduction together, via the one public
                // entry point that performs both. Counted independently of
                // rung 3 below — see the module doc.
                let combined = namer.wear(&citation, frequency);
                if combined != citation {
                    combined_alters += 1;
                }

                // Rung 3 (H1's actual quantity): the drawn cascade limb
                // ALONE, restated from `Namer::sounded`'s body (private)
                // using only `draw_wear_cascade` and `evolve` (both `pub`).
                let cascade = draw_wear_cascade(&world.seed, &s.species);
                let cascade_evolved = evolve(&citation, &cascade, ph).modern;
                if cascade_evolved == citation {
                    // The cascade drew only identity-effect rules for this
                    // morpheme (or a rule that happened to be a no-op on it)
                    // — not altered, so this morpheme instance ends here.
                    continue;
                }
                cascade_alters += 1;

                // Rungs 4/5 descend from rung 3 (cascade-only), not from
                // rung 3b: both counts below are measured directly, per
                // morpheme instance, from the containment check — neither is
                // derived by subtracting the other.
                let worn_surface = bare_surface(&render_views(&cascade_evolved).roman);
                let final_surface = bare_surface(&s.surface);
                if !worn_surface.is_empty() && final_surface.contains(&worn_surface) {
                    carries_surviving_wear += 1;
                } else {
                    rejected_by_guard += 1;
                }
            }
        }
    }

    let cascade_rate = 100.0 * cascade_alters as f64 / clears_floor.max(1) as f64;
    let combined_rate = 100.0 * combined_alters as f64 / clears_floor.max(1) as f64;
    // The Wearing's own chronicle reports its (differently-seeded, and
    // possibly differently-defined) cascade-only rung at 53/611 = 8.7%.
    // This is a sanity check, not a pin: printed plainly so a divergence is
    // a stated finding rather than a smoothed-over surprise.
    const CHRONICLE_CASCADE_RATE_PCT: f64 = 8.7;
    let sanity_note = if (cascade_rate - CHRONICLE_CASCADE_RATE_PCT).abs() <= 5.0 {
        "within ~5 points of the chronicle's 8.7% — consistent with the same quantity"
    } else {
        "MORE than 5 points from the chronicle's 8.7% — a FINDING: the two rungs may still \
         differ in definition, not just in seeds; do not smooth this over before Task 9"
    };

    println!("=== THE WEAR FUNNEL (seeds {FUNNEL_SEEDS:?}) ===");
    println!("rung 1   morphemes in settlement names:                     {total_morphemes}");
    println!(
        "rung 2   clear the frequency floor (>= {WEAR_FLOOR_DUPLICATE}):             {clears_floor}"
    );
    println!(
        "rung 3   the drawn cascade ALONE alters (H1's quantity):    {cascade_alters}  ({cascade_rate:.1}% of rung 2)"
    );
    println!(
        "rung 3b  wear (cascade + positional reduction) alters:     {combined_alters}  ({combined_rate:.1}% of rung 2) [informational, NOT partitioned by rungs 4/5]"
    );
    println!(
        "rung 4   rejected by the survival guard (MEASURED directly, per morpheme — NOT rung3 minus rung5): {rejected_by_guard}"
    );
    println!(
        "rung 5   carry surviving wear (MEASURED directly, descends from rung 3): {carries_surviving_wear}"
    );
    println!(
        "sanity check vs. The Wearing's chronicle (53/611 = 8.7% cascade-only): {sanity_note}"
    );

    // A loose floor only: non-empty, and monotonically narrowing rung over
    // rung. This is an instrument, not a calibration — asserting the exact
    // counts above would freeze whatever this pipeline currently does into
    // a golden, which is exactly the failure this campaign is trying to
    // change the pipeline out from under (Task 7 has not landed yet when
    // this baseline run happens).
    assert!(
        total_morphemes > 0,
        "no settlement across FUNNEL_SEEDS {FUNNEL_SEEDS:?} produced a uniquely-parseable, \
         non-empty gloss — the funnel has nothing to measure"
    );
    assert!(
        clears_floor <= total_morphemes,
        "rung 2 ({clears_floor}) must not exceed rung 1 ({total_morphemes})"
    );
    assert!(
        cascade_alters <= clears_floor,
        "rung 3 ({cascade_alters}) must not exceed rung 2 ({clears_floor})"
    );
    assert!(
        combined_alters <= clears_floor,
        "rung 3b ({combined_alters}) must not exceed rung 2 ({clears_floor})"
    );
    assert_eq!(
        cascade_alters,
        rejected_by_guard + carries_surviving_wear,
        "every cascade-altered morpheme instance (rung 3) must land in exactly one of rung 4 \
         or rung 5"
    );
}
