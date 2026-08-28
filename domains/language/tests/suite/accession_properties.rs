//! The Wearing (LANG-55): later-epoch proto-roots are drawn from a reserved
//! region of the SAME-LENGTH form space, so a new concept cannot collide with
//! an established one — additivity by construction of the codomain rather than
//! by the assignment ORDER (which is what The Accession bought, at Zipf's
//! expense).
//!
//! Both tests below fix a `(seed, species)` pair whose drawn coda inventory
//! is verified (by an asserted precondition, not assumed) to be the shape
//! each test needs — mirroring `etymology::tests::test_phonology`'s own
//! documented seed-search convention. Neither uses the once-obvious "assert
//! disjointness and non-longer-ness" pair alone: `assign_proto_roots_with_epoch`'s
//! pre-existing open-addressing loop already guarantees no two concepts ever
//! share a form regardless of any coda carve, so a test built only on that
//! guarantee would pass with the carve deleted. These assert directly on the
//! one thing the carve actually controls — the drawn forms' final-coda
//! shape — instead.
use hornvale_kernel::Seed;
use hornvale_language::{
    EPOCH_COHORTS, Envelope, ExoticSeg, Segment, assign_proto_roots_with_epoch_for_test,
    draw_phonology,
};

/// A permissive envelope — every dimension maxed except tonality — so the
/// drawn phonology's coda inventory is as rich as the species/seed pair
/// allows. Mirrors `etymology::tests::test_phonology` and
/// `speakable_properties::permissive_proto`; `draw_phonology` takes an
/// `Envelope` (not just a species name), so the brief's two-argument call is
/// adapted here rather than reproduced verbatim — see the Task 2 report.
fn permissive_envelope() -> Envelope {
    Envelope {
        labiality: 1.0,
        vowel_space: 1.0,
        voicing: 1.0,
        sibilance: 1.0,
        voice_loudness: 1.0,
        tonality: 0.0,
        exotic: ExoticSeg::None,
    }
}

/// Whether `form`'s final segment is a consonant — i.e. the word ends on a
/// closed syllable, the region the carve reserves for epoch-≥1 concepts.
fn ends_closed(form: &[Segment]) -> bool {
    matches!(form.last(), Some(Segment::Consonant { .. }))
}

/// A later-epoch concept's root always ends closed, and does not grow into
/// a longer syllable tier than the epoch-0 forms drawn alongside it — but
/// only when the underlying phonology actually distinguishes open from
/// closed. `Seed(31)`/`"goblin"` is searched (by the same technique
/// `test_phonology` documents) to draw `codas: [[], [Nasal]]` — one
/// non-empty AND one empty template — asserted below as a precondition so a
/// future re-search that lands on a phonology where the two cases collapse
/// (e.g. every template closed, the vacuous case the brief's original
/// version of this test silently fell into: at `Seed(42)`/`"goblin"`,
/// `ph.codas` is `[[Stop]]`, a single always-closed template, so every form
/// ends closed whether or not the carve runs) fails loudly rather than
/// passing for the wrong reason.
#[test]
fn later_epoch_roots_end_closed_when_the_phonology_admits_both() {
    // Re-searched from Seed(13) after The Wearing's nucleus fix reseeded the
    // phonotactics draw; 13's codas collapsed to a single shape and the
    // precondition below said so. Seed 23 was then the first that satisfied
    // EVERY clause this test asserts, the length-tier one included — seed 21
    // passed the coda preconditions and then failed on length, which is why
    // the search that produced that number ran the whole body, not just the
    // preconditions.
    //
    // Re-searched AGAIN at the 2026-07-29 reversal, which withdrew
    // `ROOT_EPOCH` v4 and reseeded every draw once more. Seed 23 landed on
    // the length clause's known ~34% minority (`river` came out 10 segments
    // against an epoch-0 max of 8) — the case this test's own note says to
    // re-search rather than weaken, so that is what was done. Seed 31 is the
    // first that satisfies the whole body on the merged tree.
    let seed = Seed(31);
    let ph = draw_phonology(
        &seed,
        "goblin",
        &permissive_envelope(),
        &hornvale_language::typology::concatenative(),
    );
    assert!(
        ph.codas.iter().any(|t| t.is_empty()),
        "fixture precondition: the phonology must admit an OPEN coda \
         template, or an epoch-0 form ending open is impossible and the \
         closed/open contrast this test asserts is vacuous — re-search the \
         seed/species pair"
    );
    assert!(
        ph.codas.iter().any(|t| !t.is_empty()),
        "fixture precondition: the phonology must admit a CLOSED coda \
         template, or the carve has nothing to reserve — re-search the \
         seed/species pair"
    );

    let concepts = ["water", "stone", "fire", "hill", "river", "ford"];
    let epoch_of = |c: &str| u32::from(matches!(c, "hill" | "river" | "ford"));
    let assigned = assign_proto_roots_with_epoch_for_test(
        &seed,
        "goblinoid",
        &ph,
        &hornvale_language::typology::concatenative(),
        &concepts,
        &[],
        epoch_of,
    );

    let old: Vec<_> = ["water", "stone", "fire"]
        .iter()
        .map(|c| assigned[*c].clone())
        .collect();
    assert!(
        old.iter().any(|form| !ends_closed(form)),
        "every epoch-0 form ended closed on this phonology — the fixture \
         no longer exercises the open/closed contrast this test depends on \
         (a vacuous pass is the exact bug this test was written to catch), \
         re-search the seed/species pair"
    );

    for late in ["hill", "river", "ford"] {
        let form = &assigned[late];
        assert!(
            ends_closed(form),
            "{late} = {form:?} did not end closed — the epoch-≥1 carve is \
             not restricting the coda to the reserved region"
        );
        assert!(
            !old.contains(form),
            "{late} collided with an epoch-0 root by construction"
        );
        // A closed form can be exactly one segment longer than the SAME
        // draw would be open (the coda consonant itself); it must never be
        // a whole syllable tier longer, which is what `PROBE_BUDGET`
        // lengthening (the axis this campaign must not spend) would look
        // like against six concepts that never collide.
        //
        // **This clause holds AT THIS WITNESS SEED, not universally**, and
        // the distinction was previously left unstated. Swept over the seeds
        // in 0..2000 that satisfy every fixture precondition above, the
        // closed-coda clause holds universally, but this length clause holds
        // on only about two thirds of them. Measured before the 2026-07-29
        // reversal: 288/438 (65.8%). Re-measured on the merged tree after it,
        // by the same sweep: 270/425 (63.5%) — the same property, undisturbed,
        // which is itself evidence that the reversal changed which seeds
        // witness it and not what the carve does. The third that fail do so at
        // commits either side of The Wearing too (20/59 before, 14/38 after on
        // a narrower sweep), so it is a pre-existing property of the carve
        // against a cramped universe, not a regression: with six concepts and
        // a small form space, a reserved-region draw sometimes has to reach a
        // tier up. Read this as a witness, and re-search rather than weaken it
        // if a future reseed lands on the failing third — which is exactly
        // what the reversal did, and exactly what was done.
        let longest_old = old.iter().map(Vec::len).max().expect("non-empty");
        assert!(
            form.len() <= longest_old + 1,
            "{late} is {} segments against an epoch-0 max of {longest_old} — \
             the carve spent the LENGTH axis, which is the one axis The \
             Wearing must not spend",
            form.len()
        );
    }
}

/// Where the phonology admits no closed coda at all, the carve must
/// degrade to the identity: an epoch-≥1 concept is assigned EXACTLY the
/// form it would receive at epoch 0, not merely a same-length one.
/// `Seed(1)`/`"goblin"` is searched to draw `codas: [[], []]` — every
/// template empty, so `Namer::choose_coda_template`'s `weighty` branch can
/// never find a non-empty template to restrict to and falls through to the
/// shared, unrestricted pick (verified by reading
/// `naming.rs::choose_coda_template`; see the Task 2 report). This is the
/// "correctness before marking" clause of the carve, and — unlike a
/// coda-less phonology built by hand — this one is a genuine
/// `draw_phonology` output, so the test exercises the real fallback path
/// rather than a constructed stand-in for it.
#[test]
fn later_epoch_roots_degrade_to_epoch_zero_when_no_closed_coda_exists() {
    let seed = Seed(1);
    let ph = draw_phonology(
        &seed,
        "goblin",
        &permissive_envelope(),
        &hornvale_language::typology::concatenative(),
    );
    assert!(
        ph.codas.iter().all(|t| t.is_empty()),
        "fixture precondition: the phonology must admit NO closed coda \
         template, or this is no longer the degradation case — re-search \
         the seed/species pair"
    );

    let concepts = ["water", "stone", "fire", "hill", "river", "ford"];
    let later_epoch = |c: &str| u32::from(matches!(c, "hill" | "river" | "ford"));
    let all_epoch_zero = |_: &str| 0u32;

    let with_carve = assign_proto_roots_with_epoch_for_test(
        &seed,
        "goblinoid",
        &ph,
        &hornvale_language::typology::concatenative(),
        &concepts,
        &[],
        later_epoch,
    );
    let without_epochs = assign_proto_roots_with_epoch_for_test(
        &seed,
        "goblinoid",
        &ph,
        &hornvale_language::typology::concatenative(),
        &concepts,
        &[],
        all_epoch_zero,
    );

    assert_eq!(
        with_carve, without_epochs,
        "a coda-less phonology must assign every concept identically \
         regardless of epoch — the carve degraded to something other than \
         the identity"
    );
}

/// THE RADIATION (C2d): appending a cohort leaves every proto-root assigned
/// at an EARLIER epoch untouched, and folding the same six concepts into an
/// earlier cohort does not.
///
/// This is the campaign's save-format guard. `concept_epoch` sorts by epoch
/// first, so a concept's assignment depends only on concepts sorted at or
/// before it — appending a cohort can only ever perturb concepts at that
/// cohort's epoch or LATER, never one processed earlier. The alternative is
/// not hypothetical: before this module existed, twelve species kinds added
/// at once left ten free while `treant` moved 5 facts and `otyugh` 65, and
/// omitting the cohort entirely changes which proto-root a concept draws
/// (commit `ee4e6a00`).
///
/// **Restricted to concepts strictly BEFORE the elf cohort's epoch, and this
/// restriction is load-bearing, not cosmetic (The Confidant, Task 3).** The
/// elf cohort (epoch 10) was the newest cohort when this test was written and
/// every concept checked was therefore assigned after it in name only — the
/// check actually held over the WHOLE remaining roster by chance, because no
/// later cohort's forms happened to collide with what elf's presence changed
/// downstream. Task 3 appended epoch 12 (the six felt-state concepts) and one
/// of them, `lost`, DID collide: with elf present its probe walk lands on a
/// different form than without, at seed 6, because `lost` is processed after
/// elf and its draw depends on which forms elf already claimed. That is not a
/// violation of additivity — it is additivity working exactly as designed,
/// just for a direction this test wasn't checking. A concept processed AFTER
/// the appended cohort was never guaranteed invariant to it; only concepts
/// processed BEFORE it are. So the check below is scoped to `shipped_epoch(c)
/// < elf_epoch`, which is the claim the algorithm's own ordering actually
/// proves, permanently, no matter how many cohorts land after elf's.
///
/// **The test discriminates by construction.** Two arms differ only in the
/// epoch function: the shipped one (elf concepts at their real cohort) and a
/// mutant that reports epoch 0 for them, i.e. exactly what folding them into
/// cohort 0 would do. The shipped arm must reproduce the no-elf assignment
/// EXACTLY on every seed for every pre-elf concept; the mutant must break it
/// on at least one. Without the second clause the first is satisfiable by an
/// assignment that ignores epochs altogether. The anti-vacuity (`folded`)
/// comparison stays over the FULL roster, not just the pre-elf slice — it is
/// a contrast check ("does re-founding move ANYTHING"), not the additivity
/// guarantee, so it is not subject to the same restriction.
///
/// claim: invariant(forall-seed) — save-format contract. The additivity
/// clause is a universal over seeds (it must hold for every one, so the loop
/// is a quantifier, not a sample). The anti-vacuity clause is deliberately
/// existential over the SAME loop — one seed on which the mutant moves
/// something is enough to prove the epoch ordering reaches the assignment,
/// and demanding it on all eight would assert a stronger collision rate than
/// this campaign measures. Observed at the cohort's landing: the mutant moved
/// 1–4 pre-existing roots on each of the eight seeds (17 in total), while the
/// appended table moved none.
#[test]
fn appending_the_elf_cohort_displaces_no_existing_proto_root() {
    const ELF_CONCEPTS: [&str; 6] = [
        "desert-elf-kind",
        "drow-kind",
        "high-elf-kind",
        "sea-elf-kind",
        "snow-elf-kind",
        "wood-elf-kind",
    ];

    // The whole accessioned universe, which `cli/tests/accession.rs` pins
    // equal to the concept registry in both directions — so this is the real
    // population, not a hand-picked slice.
    let all: Vec<&'static str> = hornvale_language::EPOCH_COHORTS
        .iter()
        .flat_map(|cohort| cohort.iter().copied())
        .collect();
    assert!(
        ELF_CONCEPTS.iter().all(|c| all.contains(c)),
        "the elf cohort has not been appended yet — this test measures the \
         appended table against a synthetic no-elf control, so it cannot run \
         before the cohort exists"
    );
    let without_elves: Vec<&'static str> = all
        .iter()
        .copied()
        .filter(|c| !ELF_CONCEPTS.contains(c))
        .collect();
    assert_eq!(
        without_elves.len() + ELF_CONCEPTS.len(),
        all.len(),
        "the six elf concepts must appear exactly once each in the table"
    );

    let shipped_epoch = hornvale_language::concept_epoch;
    let folded_epoch = |c: &str| {
        if ELF_CONCEPTS.contains(&c) {
            0
        } else {
            hornvale_language::concept_epoch(c)
        }
    };

    // The provable guarantee is one-directional: a concept processed BEFORE
    // the elf cohort in the global (epoch, core, name) order can never be
    // perturbed by elf's presence, but one processed after it can be — see
    // this test's own doc for how Task 3 found that the hard way.
    let elf_epoch = shipped_epoch(ELF_CONCEPTS[0]);
    assert!(
        ELF_CONCEPTS.iter().all(|c| shipped_epoch(c) == elf_epoch),
        "the six elf concepts must share one accession epoch — they are one \
         cohort, not six"
    );
    let concepts_before_elf: Vec<&str> = without_elves
        .iter()
        .copied()
        .filter(|c| shipped_epoch(c) < elf_epoch)
        .collect();
    assert!(
        !concepts_before_elf.is_empty(),
        "no concept is assigned at an epoch before elf's — the restriction \
         below would be vacuously true; re-derive elf_epoch"
    );

    let mut folded_moved_somewhere = false;
    for raw in 1u64..=8 {
        let seed = Seed(raw);
        let ph = draw_phonology(
            &seed,
            "goblin",
            &permissive_envelope(),
            &hornvale_language::typology::concatenative(),
        );

        let control = assign_proto_roots_with_epoch_for_test(
            &seed,
            "goblinoid",
            &ph,
            &hornvale_language::typology::concatenative(),
            &without_elves,
            &[],
            shipped_epoch,
        );
        let appended = assign_proto_roots_with_epoch_for_test(
            &seed,
            "goblinoid",
            &ph,
            &hornvale_language::typology::concatenative(),
            &all,
            &[],
            shipped_epoch,
        );
        let folded = assign_proto_roots_with_epoch_for_test(
            &seed,
            "goblinoid",
            &ph,
            &hornvale_language::typology::concatenative(),
            &all,
            &[],
            folded_epoch,
        );

        for concept in &concepts_before_elf {
            assert_eq!(
                control.get(*concept),
                appended.get(*concept),
                "seed {raw}: appending the elf cohort moved `{concept}`'s \
                 proto-root, even though `{concept}` is assigned at an \
                 earlier epoch than elf. Appending must be additive BY \
                 CONSTRUCTION for every earlier-epoch concept — if this \
                 fires, an existing cohort was edited or reordered. Do not \
                 re-pin this; fix the table."
            );
        }
        for concept in &without_elves {
            if control.get(*concept) != folded.get(*concept) {
                folded_moved_somewhere = true;
            }
        }
    }

    assert!(
        folded_moved_somewhere,
        "ANTI-VACUITY: folding the six elf concepts into cohort 0 moved no \
         existing proto-root on any of eight seeds, so the additivity clause \
         above proves nothing — the epoch ordering is not reaching the \
         assignment at all. Investigate before trusting this test."
    );
}

/// The shared body of the cohort-additivity properties below: appending
/// `cohort` to the live table leaves every EARLIER-epoch proto-root exactly
/// where it was, and folding `cohort` into epoch 0 instead moves at least one
/// (the anti-vacuity control, without which the additivity clause would prove
/// nothing).
///
/// **The earlier-epoch restriction is the universally correct form, and it
/// costs a genuinely-last cohort nothing.** The guarantee
/// `assign_proto_roots_with_epoch` provides is one-directional — a concept
/// processed BEFORE `cohort` in the global (epoch, core, name) order can never
/// be perturbed by its presence, one processed after it can be. For the last
/// cohort in the table that restriction selects every other concept in the
/// roster, so the check is the full-roster one; for an interior cohort it is
/// the only true statement available. Extracted here when The Inquest appended
/// epoch 13 and the felt-state test's hand-written `EPOCH_COHORTS.last()`
/// assertion went red: without this, every future campaign that appends a
/// cohort must first demote its predecessor's test by hand.
fn assert_appending_a_cohort_is_additive(cohort: &[&str], label: &str) {
    let all: Vec<&'static str> = EPOCH_COHORTS
        .iter()
        .flat_map(|c| c.iter().copied())
        .collect();
    assert!(
        cohort.iter().all(|c| all.contains(c)),
        "the {label} cohort has not been appended yet — this test measures \
         the appended table against a synthetic control, so it cannot run \
         before the cohort exists"
    );
    let without: Vec<&'static str> = all
        .iter()
        .copied()
        .filter(|c| !cohort.contains(c))
        .collect();
    assert_eq!(
        without.len() + cohort.len(),
        all.len(),
        "each {label} concept must appear exactly once in the table"
    );

    let shipped_epoch = hornvale_language::concept_epoch;
    let folded_epoch = |c: &str| {
        if cohort.contains(&c) {
            0
        } else {
            hornvale_language::concept_epoch(c)
        }
    };

    let cohort_epoch = shipped_epoch(cohort[0]);
    assert!(
        cohort.iter().all(|c| shipped_epoch(c) == cohort_epoch),
        "the {label} concepts must share one accession epoch — they are one \
         cohort, not several"
    );
    let before: Vec<&str> = without
        .iter()
        .copied()
        .filter(|c| shipped_epoch(c) < cohort_epoch)
        .collect();
    assert!(
        !before.is_empty(),
        "no concept is assigned at an epoch before {label}'s — the \
         restriction below would be vacuously true; re-derive cohort_epoch"
    );

    let mut folded_moved_somewhere = false;
    for raw in 1u64..=8 {
        let seed = Seed(raw);
        let ph = draw_phonology(
            &seed,
            "goblin",
            &permissive_envelope(),
            &hornvale_language::typology::concatenative(),
        );

        let control = assign_proto_roots_with_epoch_for_test(
            &seed,
            "goblinoid",
            &ph,
            &hornvale_language::typology::concatenative(),
            &without,
            &[],
            shipped_epoch,
        );
        let appended = assign_proto_roots_with_epoch_for_test(
            &seed,
            "goblinoid",
            &ph,
            &hornvale_language::typology::concatenative(),
            &all,
            &[],
            shipped_epoch,
        );
        let folded = assign_proto_roots_with_epoch_for_test(
            &seed,
            "goblinoid",
            &ph,
            &hornvale_language::typology::concatenative(),
            &all,
            &[],
            folded_epoch,
        );

        for concept in &before {
            assert_eq!(
                control.get(*concept),
                appended.get(*concept),
                "seed {raw}: appending the {label} cohort moved `{concept}`'s \
                 proto-root, even though `{concept}` is assigned at an \
                 earlier epoch. Appending must be additive BY CONSTRUCTION — \
                 if this fires, an existing cohort was edited or reordered. \
                 Do not re-pin this; fix the table."
            );
            if control.get(*concept) != folded.get(*concept) {
                folded_moved_somewhere = true;
            }
        }
    }

    assert!(
        folded_moved_somewhere,
        "ANTI-VACUITY: folding the {label} cohort into cohort 0 moved no \
         existing proto-root on any of eight seeds, so the additivity clause \
         above proves nothing — the epoch ordering is not reaching the \
         assignment at all. Investigate before trusting this test."
    );
}

/// THE CONFIDANT (Task 3): appending the felt-state cohort (epoch 12, the six
/// `AffectLabel` concepts) leaves every earlier-epoch proto-root untouched.
///
/// **This test used to assert `EPOCH_COHORTS.last()` was the felt-state
/// cohort**, because epoch 12 was then genuinely last and the full-roster
/// guarantee therefore held with no before/after restriction — it was written
/// as the positive control for the carve-out the elf test needed. The Inquest
/// appended epoch 13, that assertion went red, and the record of the
/// distinction is this paragraph rather than a line of code: the shared helper
/// applies the earlier-epoch restriction unconditionally, which for a last
/// cohort degenerates to the full roster and for this one is now the only
/// true statement.
///
/// claim: invariant(forall-seed) — save-format contract, mirroring
/// `appending_the_elf_cohort_displaces_no_existing_proto_root`'s shape and
/// citing its own doc for the mechanism.
#[test]
fn appending_the_felt_state_cohort_displaces_no_existing_proto_root() {
    assert_appending_a_cohort_is_additive(
        &[
            "content",
            "eager",
            "frustrated",
            "helpless",
            "lost",
            "searching",
        ],
        "felt-state",
    );
}

/// THE INQUEST (Task 6): appending `kill` (epoch 13) displaces nothing.
///
/// **This is the campaign's byte-golden guarantee, stated as a property
/// rather than inferred from a clean `git status`.** `kill` joins
/// `packs::universal_stratum`, so it is CORE and every tongue lexicalizes it
/// — a naive insertion would have re-sorted the whole allocation and moved a
/// word in every language in the world. Epoch 13 is what makes it land
/// strictly last, and the anti-vacuity half proves that: folding `kill` into
/// cohort 0 instead — one concept, not six — does move existing roots.
///
/// It was also the last cohort as of The Inquest, and is no longer: The
/// Mortise appended `think` (epoch 14) and The Offer the five object
/// properties (epoch 15), both after it. The sanity assertion that pinned
/// `kill` as last has been deleted per its own instruction, rather than
/// reordering the table — the property above holds regardless of which
/// cohort is last, which is why the helper needed no change either.
///
/// claim: invariant(forall-seed) — save-format contract.
#[test]
fn appending_the_kill_cohort_displaces_no_existing_proto_root() {
    assert_appending_a_cohort_is_additive(&["kill"], "kill");
}

/// THE MORTISE (Task 2): appending `think` (epoch 14) displaces nothing.
///
/// Same guarantee `kill` (epoch 13) already carries, for the same reason:
/// `think` joins `packs::universal_stratum`, so it is CORE and every tongue
/// lexicalizes it — a naive insertion would have re-sorted the whole
/// allocation and moved a word in every language in the world. Epoch 14 is
/// what makes it land strictly last, after `kill`, and the anti-vacuity
/// half proves that: folding `think` into cohort 0 instead does move
/// existing roots.
///
/// claim: invariant(forall-seed) — save-format contract.
#[test]
fn appending_the_think_cohort_displaces_no_existing_proto_root() {
    assert_appending_a_cohort_is_additive(&["think"], "think");
}

/// THE OFFER (Task 9): appending the five `object_property_pack` concepts
/// (epoch 15) displaces nothing.
///
/// **Final whole-branch review minor M-e.** Every other cohort landed since
/// The Inquest wrote this pattern — elf, felt-state, kill — carries this
/// exact test, one call to the shared helper; this cohort shipped without one.
/// Task 9's own review checked the byte-golden diff by hand (five lines
/// added at the correct alphabetical positions, every pre-existing entry
/// byte-identical) but nothing PINS that as a property future campaigns
/// must also hold, the way this test does for every other cohort.
///
/// claim: invariant(forall-seed) — save-format contract, mirroring
/// `appending_the_kill_cohort_displaces_no_existing_proto_root`'s shape.
#[test]
fn appending_the_object_property_cohort_displaces_no_existing_proto_root() {
    assert_appending_a_cohort_is_additive(
        &[
            "affords-passage",
            "encloses",
            "holds-liquid",
            "radiates-heat",
            "supports-rest",
        ],
        "object-property",
    );
}
