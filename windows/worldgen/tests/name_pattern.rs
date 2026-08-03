//! The per-culture naming pattern, derived from society rather than authored.

use hornvale_language::anthroponym::{Author, Cite, ElementSource, GlossBasis};
use hornvale_species::{Sociality, SocietyVector, StatusBasis};
use hornvale_worldgen::name_pattern;

fn society(sociality: Sociality, status_basis: StatusBasis, radius: f64) -> SocietyVector {
    SocietyVector {
        sociality,
        status_basis,
        in_group_radius: radius,
    }
}

fn mind() -> hornvale_species::MindVector {
    // MindVector has NO `baseline()` constructor (SocietyVector does; the
    // asymmetry is real). Construct it literally — the pattern derivation
    // does not read these values today, but an honest test supplies real
    // ones rather than zeroes.
    hornvale_species::MindVector {
        threat_response: 0.5,
        deliberation_latency: 0.5,
        time_horizon: 0.5,
    }
}

#[test]
fn every_pattern_opens_with_a_given_name() {
    for soc in [Sociality::Hierarchic, Sociality::Communal] {
        for sb in [
            StatusBasis::Rank,
            StatusBasis::Knowledge,
            StatusBasis::Generosity,
        ] {
            let p = name_pattern(&mind(), &society(soc, sb, 0.5));
            assert_eq!(
                p.elements.first().map(|e| e.0),
                Some(ElementSource::Stem),
                "every culture gives a given name first"
            );
        }
    }
}

#[test]
fn descent_legitimates_in_a_hierarchic_people() {
    let p = name_pattern(
        &mind(),
        &society(Sociality::Hierarchic, StatusBasis::Rank, 0.5),
    );
    assert!(
        p.elements.iter().any(|(s, _)| matches!(
            s,
            ElementSource::Relation(Cite::Parent) | ElementSource::Relation(Cite::Clan)
        )),
        "a hierarchic people cites descent"
    );
}

#[test]
fn deeds_legitimate_in_a_communal_people() {
    let p = name_pattern(
        &mind(),
        &society(Sociality::Communal, StatusBasis::Generosity, 0.5),
    );
    assert!(
        p.elements.iter().any(|(s, _)| matches!(
            s,
            ElementSource::Deed | ElementSource::Relation(Cite::Community)
        )),
        "a communal people cites what you did, not who you came from"
    );
}

#[test]
fn knowledge_status_cites_the_mentor_not_the_parent() {
    let p = name_pattern(
        &mind(),
        &society(Sociality::Hierarchic, StatusBasis::Knowledge, 0.5),
    );
    assert!(
        p.elements
            .iter()
            .any(|(s, _)| *s == ElementSource::Relation(Cite::Mentor)),
        "where craft earns standing, the transmission lineage is the lineage"
    );
}

#[test]
fn an_insular_people_carries_fewer_elements_than_an_expansive_one() {
    let insular = name_pattern(
        &mind(),
        &society(Sociality::Hierarchic, StatusBasis::Rank, 0.0),
    );
    let expansive = name_pattern(
        &mind(),
        &society(Sociality::Hierarchic, StatusBasis::Rank, 1.0),
    );
    assert!(
        insular.elements.len() < expansive.elements.len(),
        "everyone knows everyone in an insular people; a wide 'us' needs more to disambiguate"
    );
}

#[test]
fn the_midpoint_radius_keeps_the_sociality_citation_exactly() {
    // The boundary test the length comparison above cannot make. Goblin sits
    // at `in_group_radius` exactly 0.5 — `SocietyVector::baseline`'s value
    // and the roster's most common one — so the midpoint is where a widened
    // guard (`< 0.5` becoming `<= 0.5`) would land, dropping the clan
    // citation and rewriting the pattern the chronicle publishes.
    //
    // Asserted on element CONTENT, not on a count: a count-only assertion
    // cannot tell "the clan citation was dropped" from "some other element
    // was".
    let p = name_pattern(
        &mind(),
        &society(Sociality::Hierarchic, StatusBasis::Rank, 0.5),
    );
    assert_eq!(
        p.elements,
        vec![
            (ElementSource::Stem, Author::Kin),
            (ElementSource::Relation(Cite::Parent), Author::Kin),
            (ElementSource::Relation(Cite::Clan), Author::Kin),
        ],
        "the midpoint keeps the sociality citation and adds no gloss"
    );
}

#[test]
fn the_three_radius_bands_differ_only_in_their_tail() {
    // The other two arms of the same three-way decision, pinned by content
    // so that neither boundary can be widened into its neighbour unnoticed.
    let at = |r| {
        name_pattern(
            &mind(),
            &society(Sociality::Hierarchic, StatusBasis::Rank, r),
        )
        .elements
    };
    let head = vec![
        (ElementSource::Stem, Author::Kin),
        (ElementSource::Relation(Cite::Parent), Author::Kin),
    ];
    assert_eq!(at(0.4), head, "below the midpoint drops the clan citation");

    let mut neutral = head.clone();
    neutral.push((ElementSource::Relation(Cite::Clan), Author::Kin));
    assert_eq!(at(0.5), neutral, "the midpoint keeps it");

    let mut expansive = neutral.clone();
    expansive.push((ElementSource::Gloss(GlossBasis::Bearing), Author::Outsiders));
    assert_eq!(at(0.6), expansive, "above the midpoint adds a gloss");
}

#[test]
fn the_roster_produces_at_least_three_distinct_signatures() {
    // Preregistered criterion 5.1(1). Measured here on the authored society
    // vectors rather than over seeds, so a regression is caught in the fast
    // gate rather than only in the study.
    let combos = [
        (Sociality::Hierarchic, StatusBasis::Rank),
        (Sociality::Hierarchic, StatusBasis::Knowledge),
        (Sociality::Communal, StatusBasis::Generosity),
        (Sociality::Communal, StatusBasis::Knowledge),
    ];
    let mut sigs: Vec<Vec<(ElementSource, Author)>> = combos
        .iter()
        .map(|(so, sb)| name_pattern(&mind(), &society(*so, *sb, 0.5)).elements)
        .collect();
    sigs.sort();
    sigs.dedup();
    assert!(
        sigs.len() >= 3,
        "expected >= 3 distinct signatures, got {}",
        sigs.len()
    );
}
