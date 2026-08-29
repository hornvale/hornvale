use hornvale_game_core::register::{Population, REGISTER, binding_of};

#[test]
fn no_character_is_bound_twice() {
    // THE campaign invariant. Before The Legend, `.` meant land, floor AND
    // relief-band-2; `+` meant threshold, "everything else" AND water; `#`
    // meant wall AND settlement. Nothing caught it because each pane
    // allocated privately. This is the thing that catches it.
    let mut seen: std::collections::BTreeMap<char, &str> = std::collections::BTreeMap::new();
    for b in REGISTER {
        if let Some(prior) = seen.insert(b.glyph, b.means) {
            panic!(
                "glyph {:?} is bound twice: {:?} and {:?}",
                b.glyph, prior, b.means
            );
        }
    }
}

#[test]
fn the_subterranean_region_is_reserved_and_empty() {
    // Held for Delving campaign 2 (MAP-underworld-chart). Reserved means the
    // POPULATION exists so a claim is a one-line addition, and that nothing
    // else may quietly take those marks first.
    assert!(
        !REGISTER
            .iter()
            .any(|b| b.population == Population::Subterranean),
        "the subterranean region is reserved for Delving 2; \
         claiming it is that campaign's call, not this one's"
    );
}

#[test]
fn every_binding_is_reachable_by_lookup() {
    for b in REGISTER {
        assert_eq!(binding_of(b.glyph).map(|f| f.glyph), Some(b.glyph));
    }
    assert!(binding_of('\u{1F600}').is_none());
}
