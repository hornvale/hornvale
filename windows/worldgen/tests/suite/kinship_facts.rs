//! `parent-of` and `kin-of`: kinship as a genesis fact (spec §4.3, decision
//! 0578).
//!
//! `domains/history::descent` has always computed `Kinship` between a
//! founder and their forebear; until this campaign nothing committed it.
//! `windows/worldgen::person_promote::promote` resolves, for every promoted
//! founder, whether the community it descended from was ALSO promoted —
//! through entity identity (`records[i].founded_from`, an `EntityId`), never
//! through `founder_of`'s `RoleHandle`, which collides on ~3.5% of seed 42's
//! occupations (Task 1's ledger entry #6) and would misattribute roughly 1
//! in 100 forebear edges if used to match promoted founders.
//!
//! **Review round 1 correction (`docs/superpowers/ledgers/
//! 2026-09-01-the-avowal.md` entry #13).** Two defects shipped in the first
//! round and are what this file now tests directly:
//!
//! 1. `Kinship::Ancestor(n)` collapsed to `parent-of` for every `n`, so a
//!    fact meaning "37 generations removed" read as "parent" — false under
//!    the registered `parent` concept. Fixed: `parent-of` now fires ONLY for
//!    `Ancestor(1)`; everything else (`Sibling`, `Ancestor(n)` for `n != 1`)
//!    commits `kin-of`.
//! 2. The fact was committed `(descendant, parent-of, forebear)`, which
//!    reads (registry naming rule 4, left-to-right from the subject) as "the
//!    descendant is the parent of their own ancestor" — false whenever the
//!    remove is nonzero. Fixed: both predicates now commit
//!    `(forebear, predicate, descendant)`, and both are `functional: false`
//!    to match (a forebear may found more than one daughter community).

use hornvale_astronomy::SkyPins;
use hornvale_history::descent::Kinship;
use hornvale_history::record::Founding;
use hornvale_kernel::{EntityId, Fact, Seed, Value, World};
use hornvale_person::{KIN_OF, PARENT_OF};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::person_promote::select_founders;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, WorldComponents, build_world_to, forebear_of, occupation_records,
};
use std::collections::BTreeMap;

fn seed42() -> World {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    build_world_to(
        Seed(42),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Full,
    )
    .expect("seed 42 builds")
}

/// The promoted person who founded `community`, by reading `person-founded`
/// back off the ledger — the same entity-identity route `promote` itself
/// uses, never a `RoleHandle`.
fn person_of_community(world: &World, community: EntityId) -> Option<EntityId> {
    world
        .ledger
        .find(hornvale_person::PERSON_FOUNDED)
        .find(|f| f.object == Value::Entity(community))
        .map(|f| f.subject)
}

/// Whether `world` carries a `(subject, predicate, object)` fact naming
/// `object` exactly — `predicate` is no longer functional, so this checks
/// membership across every fact for `subject`, not just the last one.
fn has_fact(world: &World, subject: EntityId, predicate: &str, object: EntityId) -> bool {
    world
        .ledger
        .facts_of(subject, predicate)
        .any(|f| f.object == Value::Entity(object))
}

/// Every `(subject, community_index)` pair this seed's cast resolves, plus
/// the entity-identity map every test below needs — factored out so each
/// test states only what makes its case distinctive.
struct Resolved {
    records: Vec<hornvale_history::record::OccupationRecord>,
    cast: Vec<hornvale_worldgen::person_promote::Founder>,
    community_to_cast: BTreeMap<EntityId, usize>,
}

fn resolve(w: &World) -> Resolved {
    let records = occupation_records(w);
    let cast = select_founders(&records).remembered;
    let community_to_cast: BTreeMap<EntityId, usize> = cast
        .iter()
        .enumerate()
        .map(|(i, f)| (f.community, i))
        .collect();
    Resolved {
        records,
        cast,
        community_to_cast,
    }
}

#[test]
fn a_direct_forebear_ancestor_1_yields_a_parent_of_fact_with_the_forebear_as_subject() {
    let w = seed42();
    let r = resolve(&w);

    let mut found = false;
    for f in &r.cast {
        let Founding::From(mother) = r.records[f.occupation].founded_from else {
            continue;
        };
        let Some(&j) = r.community_to_cast.get(&mother) else {
            continue;
        };
        if !matches!(
            forebear_of(&w, f.community),
            Some((_, Kinship::Ancestor(1)))
        ) {
            continue;
        }
        let descendant =
            person_of_community(&w, f.community).expect("a cast member is always promoted");
        let parent = person_of_community(&w, r.cast[j].community)
            .expect("the mother founder is promoted too");
        assert!(
            has_fact(&w, parent, PARENT_OF, descendant),
            "the FOREBEAR must be the subject of parent-of, naming the descendant as object"
        );
        assert!(
            !has_fact(&w, descendant, PARENT_OF, parent),
            "parent-of must not also run the other direction"
        );
        assert!(
            !has_fact(&w, parent, KIN_OF, descendant),
            "an Ancestor(1) edge must not also carry kin-of"
        );
        found = true;
        break;
    }
    assert!(
        found,
        "seed 42 has no Ancestor(1) edge between two promoted founders"
    );
}

#[test]
fn a_sibling_edge_renders_as_kin_of_never_as_parent_of() {
    let w = seed42();
    let r = resolve(&w);

    let mut found = false;
    for f in &r.cast {
        let Founding::From(mother) = r.records[f.occupation].founded_from else {
            continue;
        };
        let Some(&j) = r.community_to_cast.get(&mother) else {
            continue;
        };
        if !matches!(forebear_of(&w, f.community), Some((_, Kinship::Sibling))) {
            continue;
        }
        let descendant =
            person_of_community(&w, f.community).expect("a cast member is always promoted");
        let sibling = person_of_community(&w, r.cast[j].community)
            .expect("the mother founder is promoted too");
        assert!(
            has_fact(&w, sibling, KIN_OF, descendant),
            "a Sibling edge must commit kin-of, forebear as subject"
        );
        assert!(
            !has_fact(&w, sibling, PARENT_OF, descendant),
            "a Sibling edge must NEVER render as parent-of (spec §4.3)"
        );
        found = true;
        break;
    }
    assert!(
        found,
        "seed 42 has no promoted-sibling edge — descent_graph.rs's own test expects \
         ~13% of edges to resolve as Sibling"
    );
}

#[test]
fn an_ancestor_more_than_one_generation_removed_renders_as_kin_of_never_as_parent_of() {
    let w = seed42();
    let r = resolve(&w);

    let mut found = false;
    for f in &r.cast {
        let Founding::From(mother) = r.records[f.occupation].founded_from else {
            continue;
        };
        let Some(&j) = r.community_to_cast.get(&mother) else {
            continue;
        };
        let Some((_, Kinship::Ancestor(n))) = forebear_of(&w, f.community) else {
            continue;
        };
        if n == 1 {
            continue; // that is `a_direct_forebear_...`'s case, not this one
        }
        let descendant =
            person_of_community(&w, f.community).expect("a cast member is always promoted");
        let forebear = person_of_community(&w, r.cast[j].community)
            .expect("the mother founder is promoted too");
        assert!(
            has_fact(&w, forebear, KIN_OF, descendant),
            "Ancestor({n}) with n != 1 must commit kin-of, forebear as subject"
        );
        assert!(
            !has_fact(&w, forebear, PARENT_OF, descendant),
            "Ancestor({n}) with n != 1 must NEVER render as parent-of — only \
             Ancestor(1) is a true parent under the registered `parent` concept"
        );
        found = true;
        break;
    }
    assert!(
        found,
        "seed 42 has no multi-generation Ancestor edge between two promoted founders"
    );
}

#[test]
fn an_unpromoted_forebear_yields_no_fact_naming_the_descendant() {
    let w = seed42();
    let r = resolve(&w);

    let mut found = false;
    for f in &r.cast {
        let Founding::From(mother) = r.records[f.occupation].founded_from else {
            continue;
        };
        if r.community_to_cast.contains_key(&mother) {
            continue; // the forebear WAS promoted — not this case
        }
        let descendant =
            person_of_community(&w, f.community).expect("a cast member is always promoted");
        assert!(
            !w.ledger
                .find(PARENT_OF)
                .chain(w.ledger.find(KIN_OF))
                .any(|fact| fact.object == Value::Entity(descendant)),
            "a founder whose forebear was never promoted must appear as nobody's \
             parent-of/kin-of object — the ledger says what is remembered (spec §4.3)"
        );
        found = true;
        break;
    }
    assert!(
        found,
        "seed 42 has no promoted-founder-with-unpromoted-forebear — spec §4.3's \
         reference reads 76 at this seed"
    );
}

#[test]
fn a_root_founder_appears_as_no_ones_object() {
    let w = seed42();
    let r = resolve(&w);

    let mut found = false;
    for f in &r.cast {
        if !matches!(r.records[f.occupation].founded_from, Founding::Genesis(_)) {
            continue;
        }
        let root = person_of_community(&w, f.community).expect("a cast member is always promoted");
        assert!(
            !w.ledger
                .find(PARENT_OF)
                .chain(w.ledger.find(KIN_OF))
                .any(|fact| fact.object == Value::Entity(root)),
            "a root founder (no occ-founded-from) must appear as nobody's \
             parent-of/kin-of object"
        );
        found = true;
        break;
    }
    assert!(
        found,
        "seed 42 has no promoted root founder — spec §4.3's reference reads 35 at this seed"
    );
}

#[test]
fn a_forebear_with_more_than_one_descendant_carries_more_than_one_fact_without_contradiction() {
    // The reason PARENT_OF/KIN_OF are `functional: false`: with the forebear
    // as subject, a forebear who founded more than one daughter community
    // must be able to carry more than one fact for the SAME predicate. If
    // either predicate were still `functional: true`, the second commit
    // would fail `Ledger::check`'s contradiction guard. This world's build
    // already succeeded (`seed42()` would have returned `Err` otherwise), so
    // this test only needs to confirm the multi-fact case actually occurs on
    // seed 42 rather than being vacuously true.
    //
    // **Checked PER PREDICATE, not combined (review round 2 tightening).**
    // Counting `PARENT_OF` and `KIN_OF` facts together let a subject with one
    // fact of EACH predicate satisfy the assertion without either predicate
    // individually ever needing a second object — which would have passed
    // this test even if only `KIN_OF` (not `PARENT_OF`) actually needed
    // `functional: false` on this seed. Both are measured separately below,
    // and both are non-vacuous on seed 42 (`parent-of`: 6 subjects with 2
    // objects; `kin-of`: 5 subjects with up to 3).
    let w = seed42();
    let mut parent_of_counts: BTreeMap<EntityId, usize> = BTreeMap::new();
    for f in w.ledger.find(PARENT_OF) {
        *parent_of_counts.entry(f.subject).or_insert(0) += 1;
    }
    let mut kin_of_counts: BTreeMap<EntityId, usize> = BTreeMap::new();
    for f in w.ledger.find(KIN_OF) {
        *kin_of_counts.entry(f.subject).or_insert(0) += 1;
    }
    assert!(
        parent_of_counts.values().any(|&n| n > 1),
        "seed 42 must have at least one forebear named as the subject of more \
         than one parent-of fact on its own — otherwise parent-of's \
         functional: false is unexercised on this seed"
    );
    assert!(
        kin_of_counts.values().any(|&n| n > 1),
        "seed 42 must have at least one forebear named as the subject of more \
         than one kin-of fact on its own — otherwise kin-of's \
         functional: false is unexercised on this seed"
    );
}

/// Save-format contract (spec §4.3, step 2 item 5): resolving and committing
/// `parent-of`/`kin-of` consumes no `Stream` draw.
///
/// **Review round 1 correction (I3).** The previous version of this test
/// compared two live builds of the SAME code to each other, which is a
/// tautology under determinism: a reviewer inserted a real
/// `.derive(...).stream().next_f64()` into the kinship pass and every
/// assertion here still passed, because nothing about that insertion made
/// the (still-deterministic) code produce two DIFFERENT builds. This
/// version is renamed to say only what it actually proves — reproducibility
/// — and the real "no draw was added" evidence now lives in
/// `person_facts_match_the_current_world_baseline` below,
/// which compares against an INDEPENDENT baseline captured before this
/// campaign's code existed, not against another run of the current code.
#[test]
fn kinship_pass_is_deterministic_across_two_independent_builds() {
    let a = seed42();
    let b = seed42();
    assert_eq!(
        serde_json::to_string(&a.ledger).unwrap(),
        serde_json::to_string(&b.ledger).unwrap(),
        "seed 42 must build a byte-identical ledger twice, kinship facts included"
    );
}

/// The current-world "no Stream draw" evidence (spec §4.3 step 2 item 5, review round
/// 1 fix for I3): every `is-person`-scoped fact
/// (`is-person`/`name`/`person-born`/`person-founded`/`person-died`)
/// committed by `promote`'s FIRST pass — the only place it draws
/// (`Namer::new(&world.seed, ...).name(...)`) — is unchanged relative to the
/// committed current-world baseline in
/// `tests/fixtures/person-facts-seed-42.json`. The older
/// `pre-kinship-person-facts-seed-42.json` remains historical evidence, but
/// The Underworld changed seed 42's settlement roster enough that the current
/// world carries 1,584 person facts. Comparing the live build against the
/// current serialized baseline keeps this guard
/// meaningful without pretending those worlds are the same. A perturbed name,
/// birth day, founding day or death day still fails this test.
///
/// `name` is the only value here that is actually `Stream`-drawn; the other
/// four predicates are pure arithmetic over already-committed ledger facts.
/// All five are compared, not just `name`, because `promote`'s only draw
/// happens inside the SAME loop that computes birth/death — a stray draw
/// consumed at the wrong point could just as easily desync `birth_day`
/// (subtracted from the SAME `Namer`-adjacent computation) as the name
/// itself, and comparing only `name` would miss that.
///
/// This does not, and structurally cannot, detect a draw whose result is
/// never used for anything observable (`Stream` state is local and
/// ephemeral in this codebase — see `kernel/src/seed.rs`'s `Stream`, never
/// stored on `World` — so an inert draw leaves no trace anywhere a test
/// could read). What it proves is exactly what the reviewer's own manual
/// check established for round 1's fix: the current-world golden carries the
/// promotion facts that this test checks directly. The historical pre-task
/// golden remains available for that original comparison, but it is no longer
/// a valid identity baseline after The Murrain's world movement.
/// **The baseline's WRITER, which it did not have** (The Tidemark, Task 3).
///
/// `tests/fixtures/person-facts-seed-42.json` was hand-captured, so every
/// campaign that moved seed 42 had to reconstruct the selection rule from the
/// data before it could re-take it. The rule, recovered once and encoded here
/// so it never has to be recovered again: **every ledger fact, in ledger
/// order, whose predicate is one of the five `promote`'s first pass commits**
/// — `name` for every named entity (not only persons; a settlement's name is
/// drawn by the same `Namer` in the same pass) plus the four person-scoped
/// predicates.
///
/// `#[ignore]`d: it WRITES a committed fixture, so it must never run as part
/// of an ordinary suite. Run it by hand after confirming the world was meant
/// to move, and read the diff.
#[test]
#[ignore = "writes a committed fixture (tests/fixtures/person-facts-seed-42.json); \
            run by hand after confirming seed 42 was meant to move, never in the \
            normal test run"]
fn rewrite_the_current_world_person_baseline() {
    const PREDICATES: [&str; 5] = [
        "name",
        "is-person",
        "person-born",
        "person-founded",
        "person-died",
    ];
    let w = seed42();
    let facts: Vec<&Fact> = w
        .ledger
        .iter()
        .filter(|f| PREDICATES.contains(&f.predicate.as_str()))
        .collect();
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/person-facts-seed-42.json");
    std::fs::write(
        &path,
        serde_json::to_string_pretty(&facts).expect("facts serialize"),
    )
    .expect("the fixture is writable");
    println!("wrote {} facts to {}", facts.len(), path.display());
}

#[test]
fn person_facts_match_the_current_world_baseline() {
    let baseline: Vec<Fact> =
        serde_json::from_str(include_str!("../fixtures/person-facts-seed-42.json"))
            .expect("fixture parses as Vec<Fact>");
    // Two campaigns re-take it together: 1227 -> 1711. The Underworld
    // Peoples' four and The Tidemark's six enter the roster, nine of them
    // settle, and the deep-history bake promotes more founders and names more
    // places. MEASURED on the merged world, not added from the two branches'
    // separate takes. Regenerated by this file's own
    // `rewrite_the_current_world_person_baseline`, which is new — the fixture
    // had no writer and every campaign that moved seed 42 had to recover the
    // selection rule from the data first.
    assert_eq!(
        baseline.len(),
        1711,
        "the current-world person baseline must not drift — if this fails, \
         re-run `rewrite_the_current_world_person_baseline` (ignored, in this \
         file) after confirming the world was meant to move"
    );

    let w = seed42();
    for f in &baseline {
        let current: Vec<&Fact> = w.ledger.facts_of(f.subject, &f.predicate).collect();
        assert_eq!(
            current.len(),
            1,
            "expected exactly one {} fact for subject {:?}, found {}",
            f.predicate,
            f.subject,
            current.len()
        );
        assert_eq!(
            current[0], f,
            "a current-world person fact moved relative to the committed baseline \
             (subject {:?}, predicate {}) — a Stream draw was perturbed somewhere \
             in promote()'s first pass",
            f.subject, f.predicate
        );
    }
}

/// `place`/`day` on a `parent-of`/`kin-of` fact are the DESCENDANT's
/// community and founding day, not the forebear-subject's — `PARENT_OF`'s
/// own doc and decision 0584 say so explicitly (review round 2). Nothing
/// checked the `day` half of that until now: `make seam-guard` came back
/// with `ledger_day_of_bake_year`'s call at `person_promote.rs:433`
/// UNGUARDED — mutating that call's argument (`identity(0)`, so it returns
/// `f.founded` unconverted) left the whole suite green, meaning every
/// committed `day` on all 93 `parent-of`/`kin-of` facts could be wrong with
/// nothing to notice.
///
/// This test reads each fact's `day` back against the SAME descendant's own
/// `person-founded` day — stamped independently, in `promote`'s FIRST pass,
/// from the identical `founded_day` value before the kinship pass (second
/// pass) ever runs — rather than recomputing the expected value with the
/// function under test, which would make the assertion circular.
///
/// **Non-vacuous by construction, not just by assumption.** A test that
/// only compared against the descendant's day would still pass if the code
/// used the FOREBEAR's day instead, on any seed where every forebear and
/// its descendant happen to found on the same day — so this also confirms
/// at least one checked fact's day differs from its own forebear's founding
/// day, which is the exact case a subject/object day swap would get wrong.
#[test]
fn a_kinship_facts_day_is_the_descendants_founding_day_not_the_forebears() {
    let w = seed42();

    let founding_day_of = |person: EntityId| -> hornvale_kernel::WorldTime {
        w.ledger
            .find(hornvale_person::PERSON_FOUNDED)
            .find(|f| f.subject == person)
            .and_then(|f| f.day)
            .expect("every promoted founder carries a person-founded day")
    };

    let mut checked = 0usize;
    let mut saw_a_day_that_differs_from_the_forebears = false;
    for f in w.ledger.find(PARENT_OF).chain(w.ledger.find(KIN_OF)) {
        let Value::Entity(descendant) = f.object else {
            panic!(
                "a {} fact's object is always the descendant's EntityId",
                f.predicate
            );
        };
        let expected = founding_day_of(descendant);
        assert_eq!(
            f.day,
            Some(expected),
            "a {} fact's day must be the DESCENDANT's founding day \
             (PARENT_OF's own doc, decision 0584), not the forebear-subject's \
             — subject {:?}, object {:?}",
            f.predicate,
            f.subject,
            descendant
        );

        let forebear_day = founding_day_of(f.subject);
        if Some(forebear_day) != f.day {
            saw_a_day_that_differs_from_the_forebears = true;
        }
        checked += 1;
    }

    assert!(
        checked > 0,
        "seed 42 has no parent-of/kin-of facts to check"
    );
    assert!(
        saw_a_day_that_differs_from_the_forebears,
        "every checked fact's day equals BOTH the descendant's and the \
         forebear's founding day on this seed, so this test could not have \
         caught a subject/object day swap — it needs at least one edge where \
         the two differ"
    );
}
