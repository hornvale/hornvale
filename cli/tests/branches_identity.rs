//! THE structural guard for the post-Branches world (spec §6), superseding
//! `words_identity.rs`/`tongues_identity.rs`. Those keystones asserted
//! "campaign X changed only names, never structure" — an invariant true of
//! every campaign up to and including The Tongues, because each of those
//! campaigns changed how a fact was drawn, never who a settlement's people
//! were. The Branches is different by design: it adds two new peoples
//! (hobgoblin, bugbear — cognate members of the goblinoid family alongside
//! goblin, descending from a shared drawn proto-goblinoid vector) into the
//! same settlement-placement pass that already held goblin and kobold. A
//! bigger roster competing for the same cells shifts WHO wins WHICH cell
//! world-wide, so goblin's own placement — and therefore its own committed
//! names — legitimately changed too. Asserting "goblin structure/facts
//! unchanged" here would be FALSE and this file must never do it.
//!
//! What actually stayed true, and is asserted below instead:
//!
//! 1. `seed_42_is_deterministic_across_two_builds` — same seed, same pins,
//!    byte-identical world JSON.
//! 2. `kobold_lexicon_is_the_singleton_build_lexicon_call` — kobold (the
//!    one people outside the goblinoid family — the unrelated outgroup)
//!    still resolves through the direct, unmediated `build_lexicon` call
//!    with `family == species` and `proto_ph == ph`: the pre-family
//!    singleton mechanism, unperturbed by the family machinery The
//!    Branches introduced for its three goblinoid daughters.
//! 3. `kobold_phonology_is_a_pure_function_of_seed_and_envelope` —
//!    kobold's drawn phonology is a pure function of `(seed, species,
//!    envelope)` alone, exactly reproducible outside a built world and
//!    unperturbed by which OTHER species share the roster.
//! 4. `hobgoblin_and_bugbear_are_present` — the world actually contains
//!    hobgoblin and bugbear species entities: the new peoples this
//!    campaign exists to add are really there.
//! 5. `goblin_names_are_rebaselined_not_frozen` — goblin's committed
//!    settlement names are non-empty AND DIFFER from the pre-Branches
//!    fixture's (frozen at Task 2, back when only goblin and kobold
//!    competed for cells) — proof the re-derivation actually happened, not
//!    a no-op merge, and (since the founder floor, MAP-22 K=1) proof
//!    goblin still places real settlements in the shared four-people
//!    world rather than being boxed out to zero.
//! 6. `every_goblinoid_words_root_is_in_its_own_daughters_inventory` — the
//!    family-consistency guard (Task 6, mirrored here from
//!    `windows/worldgen/src/lib.rs`'s own
//!    `every_goblinoid_word_is_in_its_inventory`): every goblinoid
//!    daughter's every `Root` lexicon entry's evolved/nativized form draws
//!    only from that daughter's own phonology inventory, never a cousin's.
//! 7. `bugbear_and_kobold_are_present_in_settlement_composition`
//!    — reframed after the niche-differentiated-K cutover (windows/worldgen
//!    build_to now packs the coexistence stack competitively): bugbear and
//!    kobold no longer win any settlement's dominance (goblin and hobgoblin
//!    win them all at seed 42), so the founder-floor "every people commits
//!    at least one settlement name" guarantee this test used to check no
//!    longer holds for them — a `peopled-by`/settlement-`name` fact is
//!    committed only for a cell's DOMINANT species. What remains true, and
//!    is asserted here instead: bugbear and kobold are still PRESENT — a
//!    strictly positive density fraction in at least one settlement's
//!    composition — matching The Niche's accepted 2-way coexistence
//!    result. Recomputes the same coexistence stack genesis built
//!    (`hornvale_worldgen::demography_report`, the pure/deterministic
//!    accessor mirroring genesis's own pipeline byte-for-byte), since
//!    composition, unlike dominance, commits no ledger fact of its own.

use std::collections::{BTreeMap, BTreeSet};

use hornvale_kernel::{KindId, Seed, Value, World};
use hornvale_language::LexEntry;
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

fn default_generated_seed_42() -> World {
    generated_world_at(42)
}

/// Same pins as [`default_generated_seed_42`], parameterized by seed — the
/// readout battery walks many seeds to measure the fix's shape, not just
/// pin it at 42.
fn generated_world_at(seed: u64) -> World {
    build_world(
        Seed(seed),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .unwrap()
}

/// Every settlement `name`-fact's text among settlements peopled by
/// `species`, sorted — lets a comparison of "goblin's own committed
/// output" ignore placement/commit-order shuffling from the bigger
/// roster. Reuses `words_identity.rs`/`tongues_identity.rs`'s
/// entity-id-then-fact-lookup idiom.
fn settlement_names_of_species(world: &World, species: &str) -> Vec<String> {
    let mut names: Vec<String> = world
        .ledger
        .find(hornvale_settlement::IS_SETTLEMENT)
        .map(|f| f.subject)
        .filter(|id| hornvale_species::species_of(world, *id).as_deref() == Some(species))
        .filter_map(|id| world.ledger.text_of(id, hornvale_kernel::NAME))
        .map(str::to_string)
        .collect();
    names.sort();
    names
}

/// Every committed `species-name` fact's text, sorted — the world's
/// roster of actually-placed peoples.
fn species_names(world: &World) -> Vec<String> {
    let mut names: Vec<String> = world
        .ledger
        .find(hornvale_species::SPECIES_NAME)
        .filter_map(|f| match &f.object {
            Value::Text(t) => Some(t.clone()),
            _ => None,
        })
        .collect();
    names.sort();
    names
}

/// (1) Determinism (constitutional, beneath every campaign): same seed,
/// same pins → byte-identical world JSON.
#[test]
fn seed_42_is_deterministic_across_two_builds() {
    let a = default_generated_seed_42().to_json();
    let b = default_generated_seed_42().to_json();
    assert_eq!(a, b, "same seed + pins must yield byte-identical worlds");
}

/// (2) Singleton mechanism (kobold): `lexicon_of` for the one people
/// outside the goblinoid family must still resolve through the direct,
/// pre-family `build_lexicon` call — `family == species`, `proto_ph ==
/// ph` — end to end. Mirrors worldgen's own
/// `kobold_lexicon_mechanism_is_stable_given_fixed_exposures`
/// (`windows/worldgen/src/lib.rs`), re-checked here from outside the
/// crate as a structural guard.
#[test]
fn kobold_lexicon_is_the_singleton_build_lexicon_call() {
    let world = default_generated_seed_42();
    let kph = hornvale_worldgen::language_of(&world, "kobold");
    let kex = hornvale_worldgen::exposure_of(&world, "kobold")
        .expect("exposure_of(kobold) must succeed for a placed species");
    let wc = hornvale_worldgen::WorldComponents::assemble().expect("canonical registries");
    let kdaughters = hornvale_worldgen::family_daughters(&world, &wc, "kobold");
    let direct = hornvale_language::build_lexicon(
        &world.seed,
        "kobold",
        "kobold",
        &kph,
        &kph,
        &kex,
        &kdaughters,
        hornvale_language::CascadeRegime::SETTLED,
    );
    assert_eq!(
        hornvale_worldgen::lexicon_of(&world, "kobold").expect("lexicon_of(kobold) must succeed"),
        direct,
        "kobold — the one unrelated outgroup, a singleton family — must \
         still resolve through the direct build_lexicon call with \
         family == species and proto_ph == ph; the family machinery The \
         Branches introduced for the goblinoid daughters must never \
         perturb the one people outside it"
    );
}

/// (3) Kobold draw-path stability: `language_of` is a pure function of
/// `(seed, species, envelope)` alone — reproducible directly via
/// `draw_phonology`, unperturbed by which OTHER species now share the
/// roster (hobgoblin and bugbear joining changes nothing about how
/// kobold's own phonology is drawn).
#[test]
fn kobold_phonology_is_a_pure_function_of_seed_and_envelope() {
    let world = default_generated_seed_42();
    // `envelope_of` now takes language's own articulation type (ECS c3); the
    // kobold row of the canonical articulation registry is byte-identical to
    // the god-struct's `peopled(kobold).articulation` (proved by
    // `dissolve_equivalence`), so the drawn phonology is unchanged.
    let articulation = hornvale_language::articulation_registry();
    let kobold_articulation = articulation
        .get(&KindId("kobold"))
        .expect("kobold has an articulation row");
    let envelope = hornvale_worldgen::envelope_of(kobold_articulation);
    let direct = hornvale_language::draw_phonology(&world.seed, "kobold", &envelope);
    assert_eq!(
        hornvale_worldgen::language_of(&world, "kobold"),
        direct,
        "kobold phonology must remain a pure function of (seed, species, \
         envelope) alone, unchanged by the roster growing to four peoples"
    );
}

/// (4) New peoples present: the world actually contains hobgoblin and
/// bugbear species entities — the two peoples this campaign exists to
/// add are really there, not just theorized.
#[test]
fn hobgoblin_and_bugbear_are_present() {
    let world = default_generated_seed_42();
    let names = species_names(&world);
    assert!(
        names.iter().any(|n| n == "hobgoblin"),
        "seed 42 must place a hobgoblin species entity; got {names:?}"
    );
    assert!(
        names.iter().any(|n| n == "bugbear"),
        "seed 42 must place a bugbear species entity; got {names:?}"
    );
}

/// (5) Goblin re-baselined: goblin's committed settlement names are
/// non-empty AND DIFFER from the pre-Branches fixture's (frozen at Task
/// 2, when only goblin and kobold competed for cells) — proving both
/// that the re-derivation actually happened world-wide (not a no-op
/// merge) and that, since the founder floor (settlement's
/// founder-reservation pass, MAP-22 K=1), goblin still places real
/// settlements in the shared four-people world rather than being boxed
/// out to zero by a stronger competitor. This is the one comparison this
/// keystone makes against the OLD, two-peoples world, and the inequality
/// is deliberate: "goblin structure unchanged" would be false.
#[test]
fn goblin_names_are_rebaselined_not_frozen() {
    let fixture: World =
        serde_json::from_str(include_str!("fixtures/pre-branches-seed-42-world.json"))
            .expect("fixture parses");
    let world = default_generated_seed_42();

    let fixture_names = settlement_names_of_species(&fixture, "goblin");
    let world_names = settlement_names_of_species(&world, "goblin");

    assert!(
        !fixture_names.is_empty(),
        "the pre-Branches fixture should place at least one goblin settlement"
    );
    assert!(
        !world_names.is_empty(),
        "goblin must still place at least one settlement in the current \
         four-people world — a stronger competitor's suitability score \
         must never box a placed people out to zero"
    );
    assert_ne!(
        world_names, fixture_names,
        "goblin's committed settlement names must differ from the \
         pre-Branches fixture — adding hobgoblin and bugbear to the same \
         placement pass shifts which cells goblin wins world-wide, so its \
         own names re-derive too; byte-identity here would mean The \
         Branches never actually touched placement"
    );
}

/// (7) Coexistence-stack world invariant, reframed post-cutover: every
/// people is PRESENT (a strictly positive density fraction) in at least one
/// settlement's composition in the shared, unpinned seed-42 world, and the
/// bugbear — the one people that still never wins a settlement's dominance
/// under the niche-differentiated-K coexistence stack — is present anyway.
/// Composition commits no ledger fact of its own (only a settlement's
/// dominant species does, via `peopled-by`), so this recomputes the same
/// coexistence stack genesis built via `hornvale_worldgen::demography_report`
/// — the pure, deterministic accessor that mirrors genesis's own
/// `niche_per_species_k` → `coexist::pack` → `stack_condense::condense_stack`
/// pipeline byte-for-byte at the frozen `BETA`/`FLOOR` constants — over the
/// same peopled-only roster filter genesis's unpinned path applies.
///
/// **Premise revised by The Tumult's elevation re-datum.** This test used to
/// assert that *kobold* never wins dominance either — "The Niche's accepted
/// 2-way result" — and it held for exactly the wrong reason: the condition
/// niche's elevation axis was scored against the isostatic reference datum
/// rather than height above sea level, which put the kobold's authored
/// highland optimum above most worlds' highest land and crushed its fit
/// everywhere (see [`hornvale_species::ConditionNiche`]). With the axis
/// re-datumed the kobold is a real competitor on high ground: at seed 42 it
/// now takes 4 of 216 stack settlements (it took 0 of 105 before), and its
/// presence rises from 31 of 105 settlements to all 216. The invariant that
/// survives, and is asserted below, is the one this test was always for —
/// **never-dominant does not mean absent** — now carried by the bugbear,
/// whose stronghold axis is moisture rather than elevation and which the
/// re-datum leaves shut out of dominance exactly as before.
#[test]
fn bugbear_and_kobold_are_present_in_settlement_composition() {
    let world = default_generated_seed_42();
    // The peopled-only component set (the psyche key-set; fauna are
    // biosphere-only), matching genesis's unpinned peopled `species_set`.
    use hornvale_kernel::{ComponentStore, KindId};
    let psyche = hornvale_species::psyche_registry();
    let peopled: std::collections::BTreeSet<KindId> = psyche.ids().copied().collect();
    let biosphere: ComponentStore<KindId, hornvale_species::BiosphereTraits> =
        hornvale_species::biosphere_registry()
            .iter()
            .filter(|(k, _)| peopled.contains(k))
            .map(|(k, v)| (*k, v.clone()))
            .collect();
    let family_of: ComponentStore<KindId, &'static str> = hornvale_species::family_of()
        .iter()
        .filter(|(k, _)| peopled.contains(k))
        .map(|(k, v)| (*k, *v))
        .collect();
    let names: Vec<&'static str> = biosphere.ids().map(|k| k.0).collect();
    let wc = hornvale_worldgen::WorldComponents::from_stores(
        biosphere,
        psyche,
        hornvale_species::society_registry(),
        hornvale_species::perception_registry(),
        hornvale_language::articulation_registry(),
        hornvale_language::lexicon_registry(),
        hornvale_language::family_proto(),
        family_of,
        ComponentStore::new(),
        ComponentStore::new(),
        ComponentStore::new(),
    )
    .expect("the peopled-only component set is well-formed");
    let report = hornvale_worldgen::demography_report(&world, &wc)
        .expect("demography_report must recompute over an already-built world's committed facts");

    let tag_of = |species: &str| -> u32 {
        names
            .iter()
            .position(|n| *n == species)
            .unwrap_or_else(|| panic!("{species} must be in the peopled roster")) as u32
    };

    let dominance = |species: &str| -> usize {
        let tag = tag_of(species);
        report
            .stack_settlements
            .iter()
            .filter(|s| s.dominant == tag)
            .count()
    };

    // Present-but-not-dominant, the invariant this test exists for.
    for species in ["bugbear", "kobold"] {
        let tag = tag_of(species);
        assert!(
            report.stack_settlements.iter().any(|s| s
                .composition
                .iter()
                .any(|(id, frac)| *id == tag && *frac > 0.0)),
            "{species} must hold a strictly positive density fraction in \
             at least one settlement's composition; got none"
        );
    }
    assert_eq!(
        dominance("bugbear"),
        0,
        "bugbear must still never win a settlement's dominance at seed 42 — \
         its stronghold axis is moisture, which the elevation re-datum did \
         not touch; if this now fails, the coexistence balance shifted and \
         this test's premise needs revisiting"
    );

    // The re-datum's payoff, pinned as a band rather than an exact count so a
    // later niche re-authoring re-measures rather than merely re-pins: the
    // kobold wins high ground somewhere, but stays a minority holder behind
    // the hobgoblin. Measured at seed 42: 4 of 216 (0 of 105 before the fix).
    let total = report.stack_settlements.len();
    let kobold = dominance("kobold");
    assert!(
        kobold > 0,
        "kobold must win at least one settlement's dominance at seed 42 — the \
         whole point of the elevation re-datum is that its highland stronghold \
         became occupiable; got 0 of {total}"
    );
    assert!(
        kobold * 10 <= total,
        "kobold holds {kobold} of {total} stack settlements — a highland \
         specialist should stay a minority holder (<= 10%); a larger share \
         means the niche balance shifted and this pin needs re-measuring"
    );
}

/// (6) Family inventory-closure: every goblinoid daughter's every `Root`
/// lexicon entry's evolved/nativized form draws only from that
/// daughter's own phonology inventory — never a cousin's, and never a
/// stray proto segment that failed to nativize. Mirrors the Task 6
/// worldgen test `every_goblinoid_word_is_in_its_inventory`
/// (`windows/worldgen/src/lib.rs`), re-checked here from outside the
/// crate as a structural guard on the shipped goblinoid family
/// (goblin, hobgoblin, bugbear).
#[test]
fn every_goblinoid_words_root_is_in_its_own_daughters_inventory() {
    let world = default_generated_seed_42();
    let mut checked = 0;
    for species in ["goblin", "hobgoblin", "bugbear"] {
        let ph = hornvale_worldgen::language_of(&world, species);
        let lex = hornvale_worldgen::lexicon_of(&world, species)
            .unwrap_or_else(|e| panic!("lexicon_of({species}) failed: {e:?}"));
        for (concept, entry) in lex.entries() {
            if let LexEntry::Root { derivation, .. } = entry {
                checked += 1;
                assert!(
                    derivation.modern.iter().all(|s| ph.inventory.contains(s)),
                    "{species} concept {concept:?}: a nativized root must \
                     draw only from its own daughter's inventory, got \
                     {:?} against inventory {:?}",
                    derivation.modern,
                    ph.inventory
                );
            }
        }
    }
    assert!(
        checked > 0,
        "seed 42 should mint at least one Root lexicon entry across the \
         goblinoid family"
    );
}

/// (8) The Speakable's exit regression (spec §8.1): before the attested
/// tier, phonotactic repair collapsed every glossed word for a species to
/// one fallback syllable, so EVERY bugbear deity at seed 42 was named
/// "Bvaash" (goblin "Neb", hobgoblin "Fee") regardless of what its belief
/// glossed. Pinned by mechanism, never by exact name string: within one
/// species' pantheon, two beliefs glossing DIFFERENT things must never
/// carry the SAME deity name. Two beliefs sharing one gloss may still
/// legitimately share a name (same word, same rendering) — only
/// cross-gloss collisions are the regression this probe forbids.
#[test]
fn deities_with_distinct_glosses_carry_distinct_names_at_seed_42() {
    let world = default_generated_seed_42();
    let mut checked = 0u32;
    for species in ["bugbear", "goblin", "hobgoblin", "kobold"] {
        let Some(village) = hornvale_worldgen::flagship_of(&world, species) else {
            continue;
        };
        let beliefs = hornvale_religion::beliefs_held_by(&world, village.id);

        // gloss -> set of deity names carrying it (skip empty-gloss beliefs).
        let mut by_gloss: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
        for belief in &beliefs {
            let Some(gloss) = world.ledger.text_of(belief.id, hornvale_kernel::NAME_GLOSS) else {
                continue;
            };
            if gloss.is_empty() {
                continue;
            }
            by_gloss
                .entry(gloss.to_string())
                .or_default()
                .insert(belief.deity.clone());
        }

        let glosses: Vec<&String> = by_gloss.keys().collect();
        for i in 0..glosses.len() {
            for j in (i + 1)..glosses.len() {
                let a = glosses[i];
                let b = glosses[j];
                let inter: Vec<&String> = by_gloss[a].intersection(&by_gloss[b]).collect();
                assert!(
                    inter.is_empty(),
                    "{species}: glosses {a:?} and {b:?} share deity name(s) \
                     {inter:?} — the pre-fix all-collapse-to-one-fallback \
                     regression"
                );
                checked += 1;
            }
        }
    }
    assert!(
        checked > 0,
        "no cross-gloss pairs checked — probe is vacuous"
    );
}

/// Every reflex a lexicon word may legitimately surface as inside a
/// committed name: the citation form itself, and the two images the
/// positional reduction (The Wearing, Task 9) can leave — the word-initial
/// one, whose stressed first nucleus is spared, and the unstressed one,
/// where every nucleus falls to a single vowel.
///
/// String surgery on the rendered word, sharing no code with the reduction
/// it lets past: a romanized vowel is exactly one of `aeiou` (plus an
/// optional combining tone mark, which stays with the vowel before it), so a
/// maximal run of them is exactly one nucleus. The floor is `1` because
/// `draw_phonotactics` pushes `1` into every drawn phonology's nucleus set
/// unconditionally, so `min(nuclei)` is `1` for every language a world can
/// contain.
fn reduced_reflexes(citation: &str) -> Vec<String> {
    let shorten = |spare_first: bool| {
        let mut out = String::new();
        let mut in_run = false;
        let mut kept = 0usize;
        let mut spared = spare_first;
        for c in citation.chars() {
            let vowel = "aeiou".contains(c);
            if !vowel && !c.is_ascii_alphabetic() && in_run {
                if kept > 0 {
                    out.push(c);
                }
                continue;
            }
            if !vowel {
                if in_run {
                    spared = false;
                }
                in_run = false;
                out.push(c);
                continue;
            }
            if !in_run {
                in_run = true;
                kept = 0;
            }
            kept += 1;
            if spared || kept < 2 {
                out.push(c);
            }
        }
        out
    };
    vec![citation.to_string(), shorten(true), shorten(false)]
}

/// The Bvaash fix, pinned by mechanism not by string, reframed post-cutover.
/// The original probe checked bugbear's flagship specifically: bugbear's
/// own lexicon roots "shadow" (its perception is sharp enough to name it),
/// so a bugbear deity glossing exactly "shadow" had to carry the bugbear
/// shadow word audibly. Under the niche-differentiated-K coexistence
/// stack, bugbear never wins a settlement's dominance at seed 42 (see
/// `bugbear_and_kobold_are_present_in_settlement_composition`), so
/// `religion::genesis` — which runs only at a species' own flagship — never
/// fires for bugbear: no bugbear belief, and so no bugbear "shadow" god,
/// exists in this world at all. Worse, seed 42's actual flagshipping
/// peoples (goblin, hobgoblin) have a perceptual GAP for "shadow" (their
/// night-vision doesn't clear the luminance-rank threshold) — no species'
/// pantheon can gloss "shadow" this seed, so the concept itself is no
/// longer a live probe here.
///
/// What the Bvaash fix actually guarantees — repair never discards a
/// glossed word's identity down to a fallback syllable — is still checked,
/// generalized to whichever concept a seed's flagshipping species can
/// actually name: both goblin and hobgoblin root "gloom" (the tide belief's
/// sentiment concept, `sentiment_concept(Ambient) == "gloom"`), so this
/// asserts every flagshipping species' deity glossing exactly "gloom"
/// carries that species' own gloom word audibly in its committed name.
#[test]
fn the_gloom_gods_name_is_audibly_the_gloom_word_at_seed_42() {
    let world = default_generated_seed_42();
    let mut checked = 0;

    for species in ["goblin", "hobgoblin", "bugbear", "kobold"] {
        let Some(village) = hornvale_worldgen::flagship_of(&world, species) else {
            continue;
        };
        let lex = hornvale_worldgen::lexicon_of(&world, species)
            .unwrap_or_else(|e| panic!("lexicon_of({species}) failed: {e:?}"));
        let Some(LexEntry::Root { views, .. }) = lex.entry("gloom") else {
            // Not every species need root "gloom"; skip rather than fail —
            // this probe only exercises species that can.
            continue;
        };
        let gloom_word = views.roman.to_lowercase();

        let gloom_gods: Vec<String> = hornvale_religion::beliefs_held_by(&world, village.id)
            .into_iter()
            .filter(|belief| {
                world.ledger.text_of(belief.id, hornvale_kernel::NAME_GLOSS) == Some("gloom")
            })
            .map(|belief| belief.deity)
            .collect();
        if gloom_gods.is_empty() {
            continue;
        }
        // The gloom word or one of its reduced reflexes: The Wearing's
        // positional reduction cuts an unstressed nucleus back, so a deity
        // named `Xorro` still audibly says `xorroa`. What the Bvaash fix
        // guarantees — the word's identity is never discarded down to a
        // fallback syllable — is unchanged, and a fallback syllable is not
        // a reflex of anything.
        let reflexes = reduced_reflexes(&gloom_word);
        assert!(
            gloom_gods.iter().any(|name| {
                let lower = name.to_lowercase();
                reflexes.iter().any(|r| lower.contains(r))
            }),
            "a {species} deity glossing exactly \"gloom\" must carry the \
             {species} gloom word {gloom_word:?} (or one of its reduced \
             reflexes {reflexes:?}) audibly in its committed name; got \
             {gloom_gods:?}"
        );
        checked += 1;
    }

    assert!(
        checked > 0,
        "no species' \"gloom\" god checked at seed 42 — probe is vacuous"
    );
}

/// Readout, not a regression guard: per species over seeds 0..20, prints
/// `{seed} {species}: {distinct-deity-name-count}/{deity-count}`. (Trimmed
/// from the brief's 50 seeds to 20 — 50 live world builds ran ~14.5
/// minutes, over the brief's ~10-minute ceiling; 20 is representative and
/// fast enough to re-run.) Pre-fix, every collapsed species reads 1/N
/// regardless of N; the post-fix table (quoted in the chronicle entry) is
/// this campaign's measured evidence that distinct glosses now yield
/// distinct names in the general case, not merely at seed 42. Ignored —
/// nextest must never run it; invoke manually with `--ignored --nocapture`.
#[test]
#[ignore = "readout: chronicle evidence, run manually with --nocapture"]
fn deity_name_distinctness_readout() {
    for seed in 0..20u64 {
        let world = generated_world_at(seed);
        for species in ["bugbear", "goblin", "hobgoblin", "kobold"] {
            let Some(village) = hornvale_worldgen::flagship_of(&world, species) else {
                continue;
            };
            let beliefs = hornvale_religion::beliefs_held_by(&world, village.id);
            let total = beliefs.len();
            let distinct: BTreeSet<String> = beliefs.into_iter().map(|b| b.deity).collect();
            println!("{seed} {species}: {}/{total}", distinct.len());
        }
    }
}
