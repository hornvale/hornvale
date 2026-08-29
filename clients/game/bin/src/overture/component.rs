//! The component registry (Task 6) — spec §3's *component level*, a finer
//! grain than [`super::view::View`].
//!
//! A [`View`](super::view::View) owns a whole screen; an
//! [`AlmanacComponent`] owns one line or paragraph *inside* a view. The
//! distinction earns its keep the moment a view wants to grow incrementally:
//! [`super::almanac::AlmanacView`] renders orbital facts from
//! [`BuildDepth::Astronomy`], ocean coverage from [`BuildDepth::Terrain`],
//! and a settlement count from [`BuildDepth::Settlements`] — three different
//! honest starting points inside ONE view. Without this level, the almanac
//! would need its own rung-by-rung branching, repeated and re-verified every
//! time a later campaign adds a new fact to report (the spec's own example:
//! "when a music or literature generator exists, it registers a view and the
//! frame does not change" — a component is the same promise one level down:
//! a new component registers itself and the almanac does not change).
//!
//! # Two questions, kept separate on purpose
//!
//! A component answers two independent questions, and conflating them is
//! the trap this module exists to avoid (see [`ComponentRegistry::render`]'s
//! own doc, and the task report's "the trap" section for a worked example
//! that would have passed a naive test):
//!
//! 1. **[`AlmanacComponent::needs`] — is it too early to even ask?** A rung
//!    the build has not reached yet has committed no facts a component could
//!    honestly read; asking early risks reading an ABSENT fact as a FALSE
//!    one (see [`super::almanac`]'s `NoOceansComponent` for the concrete
//!    case: [`hornvale_terrain::facts::OCEAN_FRACTION`] is a plain `Number`,
//!    committed unconditionally once terrain genesis runs — so its absence
//!    before [`BuildDepth::Terrain`] means "terrain hasn't run", never "this
//!    world has no oceans"). The registry answers this question and never
//!    calls [`AlmanacComponent::render`] when the answer is "too early".
//! 2. **[`AlmanacComponent::render`] — given that it is not too early, is
//!    there anything TRUE to say?** A component may have landed its rung and
//!    still have nothing honest to report (seed 42 is not tidally locked, so
//!    a tidal-lock component has nothing to say about it at any rung) —
//!    contract rule 2 ("show what EXISTS — never a placeholder for what does
//!    not yet", [`super::view::View`]'s own doc) applies at this level too.
//!    `render` returns [`Option::None`] rather than an empty string for
//!    exactly this reason: a caller must not have to guess whether an empty
//!    string was a real (if vacuous) answer or a "nothing to say".

use hornvale_kernel::World;
use hornvale_worldgen::{BuildDepth, RungArtifacts};

/// One line-or-paragraph-level plugin inside a component-composed
/// [`View`](super::view::View) — see the module doc for the two questions
/// this trait separates.
pub trait AlmanacComponent {
    /// The name shown in diagnostics; not rendered into the grid itself.
    fn name(&self) -> &'static str;

    /// The shallowest rung at which this component has anything it could
    /// honestly evaluate. [`ComponentRegistry::render`] never calls
    /// [`render`](AlmanacComponent::render) for a rung earlier than this.
    fn needs(&self) -> BuildDepth;

    /// This component's contribution at `rung` (already known to be at
    /// least [`needs`](AlmanacComponent::needs)), or [`None`] when it has
    /// nothing true to say about `world` right now — never a placeholder
    /// line in `None`'s place (see the module doc's second question).
    fn render(&self, world: &World, artifacts: RungArtifacts<'_>) -> Option<String>;
}

/// An ordered collection of [`AlmanacComponent`]s, each rendered only once
/// its own [`AlmanacComponent::needs`] rung has landed.
pub struct ComponentRegistry {
    /// The registered components, in the order they render.
    components: Vec<Box<dyn AlmanacComponent>>,
}

impl ComponentRegistry {
    /// Build a registry from `components`, preserving render order.
    pub fn new(components: Vec<Box<dyn AlmanacComponent>>) -> ComponentRegistry {
        ComponentRegistry { components }
    }

    /// Every component whose [`AlmanacComponent::needs`] rung is at most
    /// `rung`, filtered a second time by whether it actually has something
    /// true to say — see the module doc's "two questions" section. The
    /// first filter runs BEFORE `render` is ever called, so a component
    /// needing a later rung never sees a world it cannot honestly read: a
    /// component is skipped for being too early, never asked and told no.
    pub fn render(
        &self,
        rung: BuildDepth,
        world: &World,
        artifacts: RungArtifacts<'_>,
    ) -> Vec<String> {
        self.components
            .iter()
            .filter(|component| rung >= component.needs())
            .filter_map(|component| component.render(world, artifacts))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Seed;

    /// A component that always speaks once its rung lands — the shape of
    /// [`super::almanac`]'s unconditional paragraphs (orbit, oceans,
    /// peoples), used here to test the registry's own gating in isolation
    /// from any real fact-reading logic.
    struct AlwaysSpeaks {
        needs: BuildDepth,
        line: &'static str,
    }

    impl AlmanacComponent for AlwaysSpeaks {
        fn name(&self) -> &'static str {
            self.line
        }
        fn needs(&self) -> BuildDepth {
            self.needs
        }
        fn render(&self, _world: &World, _artifacts: RungArtifacts<'_>) -> Option<String> {
            Some(self.line.to_string())
        }
    }

    /// A component whose rung has landed but which still has nothing true
    /// to say — the shape of a "strange" fact that does not hold for this
    /// particular world (seed 42 is not tidally locked, say).
    struct NeverTrue {
        needs: BuildDepth,
    }

    impl AlmanacComponent for NeverTrue {
        fn name(&self) -> &'static str {
            "never-true"
        }
        fn needs(&self) -> BuildDepth {
            self.needs
        }
        fn render(&self, _world: &World, _artifacts: RungArtifacts<'_>) -> Option<String> {
            None
        }
    }

    /// A minimal world for tests that never read a real fact — every
    /// component above ignores its `world`/`artifacts` arguments entirely,
    /// so a synthetic empty world is honest here (contrast
    /// `almanac.rs`'s tests, which build a real seed-42 world because their
    /// components DO read real facts).
    fn empty_world() -> World {
        World::new(Seed(0))
    }

    #[test]
    fn a_component_is_skipped_until_its_rung_lands() {
        // Mirrors the task brief's own sketch (OrbitParagraph/OceansParagraph/
        // PeoplesParagraph at Astronomy/Terrain/Settlements): a registry of
        // three components, each needing a deeper rung than the last, must
        // grow its output one component at a time as the rung deepens.
        let reg = ComponentRegistry::new(vec![
            Box::new(AlwaysSpeaks {
                needs: BuildDepth::Astronomy,
                line: "orbit",
            }),
            Box::new(AlwaysSpeaks {
                needs: BuildDepth::Terrain,
                line: "oceans",
            }),
            Box::new(AlwaysSpeaks {
                needs: BuildDepth::Settlements,
                line: "peoples",
            }),
        ]);
        let world = empty_world();

        let early = reg.render(BuildDepth::Astronomy, &world, RungArtifacts::none());
        assert_eq!(
            early,
            vec!["orbit".to_string()],
            "only the orbit component can speak at rung 0"
        );

        let mid = reg.render(BuildDepth::Terrain, &world, RungArtifacts::none());
        assert_eq!(mid, vec!["orbit".to_string(), "oceans".to_string()]);

        let late = reg.render(BuildDepth::Full, &world, RungArtifacts::none());
        assert_eq!(
            late,
            vec![
                "orbit".to_string(),
                "oceans".to_string(),
                "peoples".to_string()
            ]
        );
    }

    #[test]
    fn a_landed_component_with_nothing_true_to_say_is_silently_absent() {
        // NON-VACUITY: distinguishes "skipped because too early" (the test
        // above) from "asked, and had nothing true to report" — a mix of
        // one always-speaking and one never-true component at the SAME
        // rung, so a bug that conflated `needs` gating with `render`'s own
        // `None` would still pass the test above but fail this one.
        let reg = ComponentRegistry::new(vec![
            Box::new(AlwaysSpeaks {
                needs: BuildDepth::Astronomy,
                line: "orbit",
            }),
            Box::new(NeverTrue {
                needs: BuildDepth::Astronomy,
            }),
        ]);
        let world = empty_world();
        let lines = reg.render(BuildDepth::Full, &world, RungArtifacts::none());
        assert_eq!(
            lines,
            vec!["orbit".to_string()],
            "a landed component that has nothing true to say must not appear"
        );
    }

    #[test]
    fn render_order_matches_registration_order() {
        let reg = ComponentRegistry::new(vec![
            Box::new(AlwaysSpeaks {
                needs: BuildDepth::Astronomy,
                line: "first",
            }),
            Box::new(AlwaysSpeaks {
                needs: BuildDepth::Astronomy,
                line: "second",
            }),
        ]);
        let world = empty_world();
        let lines = reg.render(BuildDepth::Astronomy, &world, RungArtifacts::none());
        assert_eq!(lines, vec!["first".to_string(), "second".to_string()]);
    }
}
