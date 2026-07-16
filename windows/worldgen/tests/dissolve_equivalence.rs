//! Cross-crate equivalence guardrail for the SpeciesDef dissolution: the
//! component registries (possibly in different crates) reassemble the same
//! kind data the god-struct held. Lives in worldgen because only a window may
//! depend on more than one domain.
use hornvale_worldgen::components::WorldComponents;

#[test]
fn assemble_holds_every_kind_and_passes_integrity() {
    let wc = WorldComponents::assemble().expect("well-formed roster");
    // biosphere = the canonical entity set (all 16 kinds today).
    assert_eq!(wc.biosphere.len(), 16);
    // peopled cluster is coherent: psyche and perception share one key-set,
    // a subset of biosphere.
    let psy: Vec<_> = wc.psyche.ids().collect();
    let per: Vec<_> = wc.perception.ids().collect();
    assert_eq!(psy, per);
    assert_eq!(wc.psyche.len(), 4);
    for k in wc.psyche.ids() {
        assert!(
            wc.biosphere.contains(k),
            "peopled kind {k:?} lacks a biosphere row"
        );
    }
}
