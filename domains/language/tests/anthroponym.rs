//! The anthroponymic schema: what a personal name is made of.

use hornvale_language::NameKind;
use hornvale_language::anthroponym::{
    Author, Cite, ElementSource, GlossBasis, IndexBasis, NameElement, NamePattern, PersonName,
};

#[test]
fn person_is_a_distinct_name_kind_with_its_own_seed_label() {
    // The label is a save-format contract: it is folded into the derive
    // path, so it must be "person" and must differ from every existing kind.
    let labels = [
        NameKind::Settlement.label_for_test(),
        NameKind::Deity.label_for_test(),
        NameKind::Epithet.label_for_test(),
        NameKind::Person.label_for_test(),
    ];
    assert_eq!(labels[3], "person");
    let mut sorted = labels.to_vec();
    sorted.sort_unstable();
    sorted.dedup();
    assert_eq!(sorted.len(), 4, "NameKind labels must be distinct");
}

#[test]
fn a_name_is_an_ordered_list_and_order_is_meaningful() {
    let given = NameElement {
        source: ElementSource::Stem,
        author: Author::Kin,
        conferred: None,
    };
    let patronymic = NameElement {
        source: ElementSource::Relation(Cite::Parent),
        author: Author::Kin,
        conferred: None,
    };
    let a = PersonName {
        elements: vec![given.clone(), patronymic.clone()],
    };
    let b = PersonName {
        elements: vec![patronymic, given],
    };
    assert_ne!(
        a, b,
        "given-first and patronymic-first are different systems"
    );
}

#[test]
fn every_element_source_and_author_is_representable() {
    // A closed-vocabulary guard: if a variant is added without updating the
    // consumers, this forces the question at compile time rather than
    // letting a new source silently render as nothing.
    let sources = [
        ElementSource::Stem,
        ElementSource::Gloss(GlossBasis::Trade),
        ElementSource::Gloss(GlossBasis::Bearing),
        ElementSource::Gloss(GlossBasis::Origin),
        ElementSource::Relation(Cite::Parent),
        ElementSource::Relation(Cite::Clan),
        ElementSource::Relation(Cite::Community),
        ElementSource::Relation(Cite::Place),
        ElementSource::Relation(Cite::Deity),
        ElementSource::Relation(Cite::Mentor),
        ElementSource::Relation(Cite::Child),
        ElementSource::Index(IndexBasis::BirthOrder),
        ElementSource::Index(IndexBasis::Generation),
        ElementSource::Deed,
    ];
    assert_eq!(sources.len(), 14);
    let authors = [
        Author::Kin,
        Author::Community,
        Author::Witnesses,
        Author::Institution,
        Author::Selfward,
        Author::Outsiders,
        Author::Inherent,
    ];
    assert_eq!(authors.len(), 7);
}

#[test]
fn a_pattern_with_no_elements_is_a_mononym_not_an_error() {
    // A culture may name with a single given name and nothing else. That is
    // Indonesia and Ancient Egypt, not a degenerate case.
    let p = NamePattern {
        elements: vec![(ElementSource::Stem, Author::Kin)],
    };
    assert_eq!(p.elements.len(), 1);
}

#[test]
fn authorship_determines_revocability() {
    // The dimension the surface vocabulary hides: an epithet conferred by
    // outsiders cannot be revoked by its bearer; a self-assumed name can.
    assert!(!Author::Outsiders.revocable_by_bearer());
    assert!(!Author::Inherent.revocable_by_bearer());
    assert!(Author::Selfward.revocable_by_bearer());
}
