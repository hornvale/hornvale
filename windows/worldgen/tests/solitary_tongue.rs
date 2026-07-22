//! The Solitary Tongue, Task 5: the campaign's payoff, asserted on a REAL
//! DERIVED WORLD (seed 42) rather than the mechanism-level unit tests Tasks
//! 1-2 already carry in `domains/language` and `windows/worldgen`'s own
//! `#[cfg(test)]` module (e.g. `cascade_regime_of_matches_the_authored_regime_map`).
//!
//! Three claims, one test each:
//!
//! - [`dragon_cascades_stay_within_the_frozen_regime_at_seed_42`] (a):
//!   FROZEN — every chromatic dragon's evolved core word draws a cascade no
//!   longer than the frozen isolate regime allows
//!   ([`hornvale_language::CascadeRegime::new`]`(0, 1)`), so its modern form
//!   traces back to the shared `"draconic"` proto through at most one sound
//!   law — the isolate is conservative.
//! - [`chromatic_dragons_diverge_less_than_the_goblinoid_family`] (b):
//!   ISOLATE < FAMILY — the three chromatics' mean inter-daughter word
//!   distance is below the goblinoid family's (goblin/hobgoblin/bugbear),
//!   at this same real world.
//! - [`peoples_lexicons_are_unchanged_from_the_pre_campaign_golden`] (c):
//!   BYTE-IDENTITY — every settled people's lexicon (goblin/hobgoblin/
//!   bugbear/kobold) is pinned to a golden captured in this commit,
//!   guarding the `Settled -> CascadeRegime::SETTLED` byte-identity Tasks
//!   1-2 already prove at the unit level, now locked at the full worldgen/
//!   seed-42 level against any future regression.
use hornvale_kernel::{Seed, World};
use hornvale_language::{CascadeRegime, LexEntry, Lexicon, Segment};
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world, lexicon_of};

/// The three chromatic dragons The Solitary Tongue gave a Draconic tongue
/// (Task 3), sharing one family (`"draconic"`) and one frozen regime.
const CHROMATIC_DRAGONS: [&str; 3] = ["white-dragon", "red-dragon", "black-dragon"];

/// The goblinoid family's three daughters (mirrors
/// `windows/lab/src/metrics.rs`'s `GOBLINOID_DAUGHTERS`) — the campaign's
/// only other multi-member speaking family, and the comparison point for
/// claim (b).
const GOBLINOID_DAUGHTERS: [&str; 3] = ["goblin", "hobgoblin", "bugbear"];

/// The book's reference seed, used throughout the campaign's own tests
/// (`cascade_regime_of_matches_the_authored_regime_map`,
/// `goblinoid_daughters_actually_diverge`) and the gallery's committed
/// artifacts.
const REFERENCE_SEED: u64 = 42;

/// A real, fully generated world at [`REFERENCE_SEED`] — settlement genesis
/// included, not the bare registry-only world `proto_goblinoid_golden.rs`
/// uses (that file only needs the seed and the concept universe; this file
/// needs actual placement so `lexicon_of`'s exposure classification reflects
/// a lived-in world, per the plan's "a real derived world" instruction).
fn generated_world(seed: u64) -> World {
    build_world(
        Seed(seed),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .unwrap_or_else(|e| panic!("seed {seed}'s default pins must build a world: {e:?}"))
}

/// The concepts `lex` holds as a bare [`LexEntry::Root`] (mirrors
/// `windows/lab/src/metrics.rs`'s `root_concepts` and
/// `windows/worldgen/src/lib.rs`'s own private test helper of the same
/// name — re-implemented here since both are private to their crates).
fn root_concepts(lex: &Lexicon) -> Vec<&str> {
    lex.entries()
        .filter(|(_, e)| matches!(e, LexEntry::Root { .. }))
        .map(|(c, _)| c)
        .collect()
}

/// A simple, defensible word distance: the count of segment positions where
/// `a` and `b` differ (index-for-index; a position present in only one of
/// the two words counts as differing). `evolve`'s rules substitute, drop, or
/// nativize segments in place and never reorder them, so positional
/// comparison is a fair proxy here — the same "count mismatched features"
/// spirit as `domains/language/src/etymology.rs`'s (private) per-segment
/// `feature_distance`, lifted to the whole-word level over the public
/// [`Segment`] equality this crate already exposes.
fn segment_distance(a: &[Segment], b: &[Segment]) -> usize {
    let n = a.len().max(b.len());
    (0..n).filter(|&i| a.get(i) != b.get(i)).count()
}

/// The mean, LENGTH-NORMALIZED pairwise [`segment_distance`] between every
/// pair of `species`, over every concept all of `species` hold as a Root
/// (mirrors `windows/lab/src/metrics.rs`'s `divergence_real`'s shared-root
/// intersection, and `windows/worldgen/src/lib.rs`'s
/// `goblinoid_daughters_actually_diverge`'s per-pair comparison of recorded
/// `derivation.modern` forms — generalized from a boolean "do they differ"
/// flag to a numeric mean distance). Normalizing each pair's raw distance by
/// its longer form's length is load-bearing: the draconic family's proto
/// words run ~8 segments long and goblinoid's run ~3-6, so a RAW (un-
/// normalized) mean favors whichever family happens to have longer words,
/// not whichever family actually diverges more per word — confirmed by
/// direct measurement (raw draconic mean 1.72 > raw goblinoid mean 1.61 at
/// seed 42, the wrong direction; length-normalized draconic mean 0.25 <
/// goblinoid mean 0.32, the campaign's actual claim).
fn mean_inter_daughter_distance(world: &World, species: &[&str; 3]) -> f64 {
    let lexes: Vec<Lexicon> = species
        .iter()
        .map(|s| lexicon_of(world, s).unwrap_or_else(|e| panic!("{s}: {e:?}")))
        .collect();
    let (first, rest) = lexes.split_first().expect("3-element array");
    let shared: Vec<&str> = root_concepts(first)
        .into_iter()
        .filter(|c| rest.iter().all(|lex| root_concepts(lex).contains(c)))
        .collect();
    assert!(
        !shared.is_empty(),
        "no concept is rooted in every one of {species:?} at seed {REFERENCE_SEED} -- \
         mean_inter_daughter_distance needs at least one shared Root to compare"
    );
    let mut total = 0.0_f64;
    let mut pairs = 0usize;
    for concept in &shared {
        for i in 0..lexes.len() {
            for j in (i + 1)..lexes.len() {
                let modern = |lex: &Lexicon| match lex.entry(concept) {
                    Some(LexEntry::Root { derivation, .. }) => derivation.modern.clone(),
                    _ => unreachable!("{concept} confirmed Root in every daughter above"),
                };
                let a = modern(&lexes[i]);
                let b = modern(&lexes[j]);
                let longer = a.len().max(b.len()).max(1) as f64;
                total += segment_distance(&a, &b) as f64 / longer;
                pairs += 1;
            }
        }
    }
    total / pairs as f64
}

/// Claim (a), FROZEN: every chromatic dragon's Root entries evolve under a
/// cascade no longer than the frozen isolate regime
/// ([`CascadeRegime::new`]`(0, 1)`) allows. This is the direct, unconfounded
/// mechanism-level signal for "near-identical to the ancestor" — NOT a raw
/// proto-vs-modern segment count, which is dominated by an orthogonal
/// effect: `evolve`'s final `nativize` pass remaps every proto segment
/// absent from the dragon's OWN drawn inventory to its nearest neighbour,
/// regardless of how many (if any) cascade rules fired, so even a
/// zero-rule word can show several differing segments (measured: white-
/// dragon draws an EMPTY cascade for all 35 of its Root concepts at seed
/// 42, yet its raw proto-vs-modern distance still averages 2.4 segments —
/// nativization noise, not drift). The rule-count bound is what
/// `CascadeRegime` actually controls and is exactly what Task 2's
/// `cascade_regime_of` computes, so this is the assertion a regressed (un-
/// frozen) regime would immediately fail: at this same seed, the four
/// SETTLED peoples draw 3-4 rules per cascade (see claim (c)'s fixture /
/// `cascade_regime_of_matches_the_authored_regime_map`), well past this
/// bound.
#[test]
fn dragon_cascades_stay_within_the_frozen_regime_at_seed_42() {
    let world = generated_world(REFERENCE_SEED);
    let frozen = CascadeRegime::new(0, 1);
    let mut any_nonempty_cascade = false;
    for dragon in CHROMATIC_DRAGONS {
        let lex = lexicon_of(&world, dragon)
            .unwrap_or_else(|e| panic!("{dragon} must carry a lexicon (Task 3): {e:?}"));
        let mut root_count = 0usize;
        for (concept, entry) in lex.entries() {
            if let LexEntry::Root { derivation, .. } = entry {
                root_count += 1;
                assert!(
                    (derivation.steps.len() as u32) <= frozen.max,
                    "{dragon}/{concept}: cascade drew {} sound-change rules, exceeding the \
                     frozen isolate regime's bound of {} rule(s) -- a solitary hoarder with no \
                     one to drift WITH must stay conservative; this fires if the dragon regime \
                     ever regresses off CascadeRegime::new(0, 1)",
                    derivation.steps.len(),
                    frozen.max
                );
                if !derivation.steps.is_empty() {
                    any_nonempty_cascade = true;
                }
            }
        }
        assert!(
            root_count > 0,
            "{dragon} minted no Root entry at seed {REFERENCE_SEED} -- claim (a) needs at \
             least one Steeped concept to examine"
        );
    }
    // Falsifiability floor: a bug that always drew the EMPTY end of the
    // regime (e.g. min/max transposed, or `range_u32` always returning its
    // low bound) would still satisfy the `<= 1` bound above but would be a
    // silently-broken regime, not a validated one. At seed 42 red-dragon and
    // black-dragon each draw the regime's other legal value (1 rule); demand
    // the frozen regime is exercised at both its allowed lengths, not just
    // vacuously always its floor.
    assert!(
        any_nonempty_cascade,
        "every chromatic dragon drew an empty cascade at seed {REFERENCE_SEED} -- \
         CascadeRegime::new(0, 1) should occasionally draw its other legal length (1 rule) too"
    );
}

/// Claim (b), ISOLATE < FAMILY: the three chromatic dragons' mean length-
/// normalized inter-daughter word distance is strictly below the goblinoid
/// family's, at seed 42 — with a real margin, not a hairline one, so the
/// assertion would fail outright if the isolate diverged as much as (or
/// more than) the settled family: measured 0.25 (draconic) vs 0.32
/// (goblinoid), asserted with a comfortable 0.05 absolute floor under that
/// margin.
#[test]
fn chromatic_dragons_diverge_less_than_the_goblinoid_family() {
    let world = generated_world(REFERENCE_SEED);
    let draconic = mean_inter_daughter_distance(&world, &CHROMATIC_DRAGONS);
    let goblinoid = mean_inter_daughter_distance(&world, &GOBLINOID_DAUGHTERS);
    assert!(
        draconic < goblinoid,
        "the frozen isolate ({draconic:.4}) must diverge LESS than the settled goblinoid \
         family ({goblinoid:.4}) at seed {REFERENCE_SEED} -- if this ever fails, the isolate \
         is no longer conservative relative to a socially-drifting family"
    );
    // A real margin, not noise: demand the gap clears an absolute floor well
    // under the measured ~0.07 gap, so a regime regression that only
    // narrows (rather than fully erases) the isolate's advantage still
    // fails loudly.
    const MIN_MARGIN: f64 = 0.05;
    assert!(
        goblinoid - draconic > MIN_MARGIN,
        "the isolate/family divergence gap ({:.4}) must clear {MIN_MARGIN} at seed \
         {REFERENCE_SEED} -- draconic={draconic:.4}, goblinoid={goblinoid:.4}",
        goblinoid - draconic
    );
}

/// Render `lex`'s full contents (every concept, in concept-id order) as a
/// stable, human-readable snapshot: one `<concept>: <kind> ...` line per
/// entry, romanization + IPA for a Root or Compound, the recountable reason
/// for a Gap. Mirrors `windows/worldgen/tests/proto_goblinoid_golden.rs`'s
/// snapshot style.
fn render_lexicon_snapshot(lex: &Lexicon) -> String {
    let mut lines = vec![
        format!("species: {}", lex.species),
        format!("headedness: {:?}", lex.headedness),
        String::new(),
    ];
    for (concept, entry) in lex.entries() {
        let line = match entry {
            LexEntry::Root { views, .. } => {
                format!("{concept}: root {} /{}/", views.roman, views.ipa)
            }
            LexEntry::Compound {
                modifier,
                head,
                views,
            } => format!(
                "{concept}: compound {modifier}+{head} -> {} /{}/",
                views.roman, views.ipa
            ),
            LexEntry::Gap { reason } => format!("{concept}: {reason}"),
        };
        lines.push(line);
    }
    lines.push(String::new());
    lines.join("\n")
}

/// The four settling peoples, in the order the golden snapshot renders
/// them.
const PEOPLES: [&str; 4] = ["goblin", "hobgoblin", "bugbear", "kobold"];

/// Claim (c), BYTE-IDENTITY: every settled people's full lexicon at seed 42
/// (a real derived, settled world) equals the golden captured in this
/// commit. Tasks 1-2 already proved the `Settled -> CascadeRegime::SETTLED`
/// byte-identity at the unit level
/// (`build_lexicon_default_regime_is_unchanged`,
/// `cascade_regime_of_matches_the_authored_regime_map`); this pins the SAME
/// claim end-to-end through the full worldgen composition root on a lived-
/// in world, so any future change that threads the wrong regime into a
/// people's lexicon (or otherwise perturbs its draws) fails loudly here
/// instead of silently.
///
/// All FOUR peoples, not just goblin: mutation-verified (temporarily
/// rerouting `Settled` through the frozen `{0,1}` regime) that goblin's own
/// rendering is, at seed 42, coincidentally insensitive to this exact
/// mutation -- every one of its SETTLED cascade's 4 rules happens to be a
/// no-op against its own drawn phonology and word shapes, so a 4-rule vs.
/// 0-1-rule cascade renders identically for goblin specifically. hobgoblin
/// and bugbear are NOT coincidentally insensitive (confirmed: the same
/// mutation changes roughly a third of their rendered root forms), so
/// including all four is what makes this guard load-bearing rather than
/// accidentally vacuous for one of the four.
///
/// A drift here on a future commit (never on this one, which captures the
/// golden) means the `Settled` byte-identity broke -- see
/// `hornvale_kernel::golden`'s module docs for the accept path
/// (REBASELINE=1), which is never appropriate for THIS specific guard
/// without first confirming the drift is deliberate and in scope.
#[test]
fn peoples_lexicons_are_unchanged_from_the_pre_campaign_golden() {
    let world = generated_world(REFERENCE_SEED);
    let mut snapshot = String::new();
    for people in PEOPLES {
        let lex = lexicon_of(&world, people).expect("every people always carries a lexicon");
        snapshot.push_str(&render_lexicon_snapshot(&lex));
        snapshot.push('\n');
    }
    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/solitary-tongue-peoples-lexicons-seed-42.txt"
        )),
        &snapshot,
        "a settled people's full seed-42 lexicon drifted from The Solitary Tongue's captured \
         golden -- every people here is Settled, so cascade_regime_of must resolve each to \
         CascadeRegime::SETTLED unchanged; a drift here means that byte-identity broke. If \
         (and only if) this drift is a deliberate, in-scope change, regenerate with \
         REBASELINE=1 and review the diff.",
    );
}
