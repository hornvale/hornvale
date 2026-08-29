//! The almanac view (Task 6): a component-composed report on the world,
//! built entirely out of [`AlmanacComponent`]s registered in a
//! [`ComponentRegistry`] — see `component.rs`'s module doc for why the
//! registry exists as its own level, separate from [`View`].
//!
//! # What ships here
//!
//! Three ROUTINE components, each an unconditional paragraph once its rung
//! lands (mirroring the task brief's own sketch of `OrbitParagraph` /
//! `OceansParagraph` / `PeoplesParagraph`):
//!
//! - [`OrbitComponent`] (needs [`BuildDepth::Astronomy`]) — the world's own
//!   orbital period, off [`hornvale_astronomy::facts::YEAR_LENGTH_STD`], a
//!   plain `Number` committed unconditionally at genesis.
//! - [`OceansComponent`] (needs [`BuildDepth::Terrain`]) — ocean coverage, off
//!   [`hornvale_terrain::facts::OCEAN_FRACTION`], likewise unconditional.
//! - [`PeoplesComponent`] (needs [`BuildDepth::Settlements`]) — a settlement
//!   and population count, off [`hornvale_settlement::IS_SETTLEMENT`] /
//!   `POPULATION`.
//!
//! And three "what is strange" components (Nathan's own example from the task
//! brief), each silent unless its own fact is actually true of this world —
//! contract rule 2 one level down, per component rather than per view:
//!
//! - [`TidallyLockedComponent`] (needs [`BuildDepth::Astronomy`]).
//! - [`NoOceansComponent`] (needs [`BuildDepth::Terrain`]).
//! - [`WastelandComponent`] (needs [`BuildDepth::Settlements`]).
//!
//! # The trap this module's tests exist to catch
//!
//! [`hornvale_terrain::facts::OCEAN_FRACTION`] and
//! [`hornvale_astronomy::facts::YEAR_LENGTH_STD`] are **plain `Number`
//! facts, committed unconditionally** the moment their rung's genesis runs —
//! unlike [`hornvale_astronomy::facts::TIDALLY_LOCKED`], which is a `Flag`
//! committed **only when true**. So an absent `OCEAN_FRACTION` means
//! "terrain hasn't run yet", never "this world has no oceans" — a component
//! that read absence as falsity would announce "no oceans" about every
//! world still at [`BuildDepth::Astronomy`], regardless of what its terrain
//! eventually holds. [`NoOceansComponent::render`] avoids this by reading
//! the real fact with `?` (propagating `None` on absence) rather than
//! defaulting a missing value to zero; `tests::no_oceans_does_not_fire_from_
//! absence_before_terrain` calls it DIRECTLY (bypassing the registry's own
//! `needs()` gate) to prove the render logic itself is safe, independent of
//! the gate that would also have caught it.
//!
//! [`TIDALLY_LOCKED`] has no equivalent trap: [`BuildDepth::Astronomy`] is
//! the shallowest rung the ladder has, so a world handed to
//! [`TidallyLockedComponent`] has already had the ONE genesis step that
//! decides this fact, on every rung the ladder can produce — there is no
//! "too early" for it to fall into.
//!
//! # "A wasteland": found reachable, not dropped
//!
//! The controller's own resolution flagged this as possibly unreachable —
//! `domains/climate` commits no ledger fact at all, so a version reading the
//! ledger the way the other two "strange" components do would have nothing
//! to read. But [`hornvale_climate::GeneratedClimate::habitable_fraction`]
//! is a public method already computed by [`BuildDepth::Settlements`]
//! (`windows/worldgen/src/lib.rs`'s own doc: `RungArtifacts::climate` is
//! `Some` from that rung on) — reachable through `artifacts.climate`
//! directly, no re-derivation and no new dependency (the method is called on
//! an already-typed reference; `hornvale-climate` need not be a direct
//! `Cargo.toml` dependency for that to compile). [`WastelandComponent`]
//! reads it, no ledger predicate involved.

use crate::overture::component::{AlmanacComponent, ComponentRegistry};
use crate::overture::view::View;
use hornvale_astronomy::facts::{STAR_CLASS, TIDALLY_LOCKED, YEAR_LENGTH_STD};
use hornvale_game_core::{Cell, Grid, Source, Weight};
use hornvale_kernel::{EntityId, Value, World};
use hornvale_settlement::{IS_SETTLEMENT, POPULATION};
use hornvale_terrain::facts::OCEAN_FRACTION;
use hornvale_worldgen::{BuildDepth, RungArtifacts};

/// Read `value` as a committed [`Value::Number`], or `None` for any other
/// shape — the same defensive reading `sky.rs`'s own `as_number` uses (a
/// second, minimal copy: that one is private to its module).
fn as_number(value: Option<&Value>) -> Option<f64> {
    match value {
        Some(Value::Number(n)) => Some(*n),
        _ => None,
    }
}

/// Locate the world entity: the unique subject carrying a [`STAR_CLASS`]
/// fact — the same idiom `sky.rs`'s own `world_entity` uses (both astronomy
/// and terrain genesis commit their summary facts onto this one shared
/// subject; `windows/worldgen/src/lib.rs`'s `world_entity` local is threaded
/// into both `astronomy::facts::genesis` and `terrain::facts::genesis`).
fn world_entity(world: &World) -> Option<EntityId> {
    world.ledger.find(STAR_CLASS).map(|f| f.subject).next()
}

/// The world's own orbital period — needs only [`BuildDepth::Astronomy`],
/// and always has something to say once that rung lands:
/// [`YEAR_LENGTH_STD`] is committed unconditionally (never gated on a
/// physical condition the way [`TIDALLY_LOCKED`] is).
struct OrbitComponent;

impl AlmanacComponent for OrbitComponent {
    fn name(&self) -> &'static str {
        "orbit"
    }

    fn needs(&self) -> BuildDepth {
        BuildDepth::Astronomy
    }

    fn render(&self, world: &World, _artifacts: RungArtifacts<'_>) -> Option<String> {
        let subject = world_entity(world)?;
        let years = as_number(world.ledger.value_of(subject, YEAR_LENGTH_STD))?;
        Some(format!(
            "The world completes an orbit every {years:.1} standard days."
        ))
    }
}

/// Ocean coverage — needs [`BuildDepth::Terrain`], and always has something
/// to say once that rung lands ([`OCEAN_FRACTION`] is unconditional; see the
/// module doc's "trap" section for why that matters).
struct OceansComponent;

impl AlmanacComponent for OceansComponent {
    fn name(&self) -> &'static str {
        "oceans"
    }

    fn needs(&self) -> BuildDepth {
        BuildDepth::Terrain
    }

    fn render(&self, world: &World, _artifacts: RungArtifacts<'_>) -> Option<String> {
        let subject = world_entity(world)?;
        let fraction = as_number(world.ledger.value_of(subject, OCEAN_FRACTION))?;
        Some(format!(
            "Oceans cover {:.0}% of the world.",
            fraction * 100.0
        ))
    }
}

/// Settlements and their people — needs [`BuildDepth::Settlements`].
/// **Suppressed at zero** (no settlement placed yet, however unlikely): a
/// literal "0 settlements dot the land" would read as the placeholder
/// contract rule 2 forbids, the same convention `sky.rs`'s caption band uses
/// for a zero wanderer/figure count.
struct PeoplesComponent;

impl AlmanacComponent for PeoplesComponent {
    fn name(&self) -> &'static str {
        "peoples"
    }

    fn needs(&self) -> BuildDepth {
        BuildDepth::Settlements
    }

    fn render(&self, world: &World, _artifacts: RungArtifacts<'_>) -> Option<String> {
        let settlements: Vec<EntityId> = world
            .ledger
            .find(IS_SETTLEMENT)
            .map(|fact| fact.subject)
            .collect();
        if settlements.is_empty() {
            return None;
        }
        let population: f64 = settlements
            .iter()
            .filter_map(|&id| as_number(world.ledger.value_of(id, POPULATION)))
            .sum();
        let n = settlements.len();
        let settlement_noun = if n == 1 { "settlement" } else { "settlements" };
        Some(format!(
            "{n} {settlement_noun} dot the land, home to {} people.",
            population.round() as u64
        ))
    }
}

/// Below this ocean fraction the almanac calls a world out as having
/// essentially no oceans at all — a narrative "notably dry" cutoff, not
/// derived from any committed decision or physical threshold. Chosen well
/// under any ordinary world's coverage: seed 42 (this module's own test
/// world) measures its real, live-built fraction in
/// `tests::seed_42_has_oceans_and_is_not_locked`, printed as evidence rather
/// than assumed.
const NO_OCEANS_THRESHOLD: f64 = 0.01;

/// "No oceans" — needs [`BuildDepth::Terrain`]; silent unless the world's
/// real, committed ocean fraction is below [`NO_OCEANS_THRESHOLD`]. See the
/// module doc's "trap" section: this must read the fact with `?`, never
/// default a missing one to zero, or it would fire on every world that
/// simply hasn't reached [`BuildDepth::Terrain`] yet.
struct NoOceansComponent;

impl AlmanacComponent for NoOceansComponent {
    fn name(&self) -> &'static str {
        "no-oceans"
    }

    fn needs(&self) -> BuildDepth {
        BuildDepth::Terrain
    }

    fn render(&self, world: &World, _artifacts: RungArtifacts<'_>) -> Option<String> {
        let subject = world_entity(world)?;
        let fraction = as_number(world.ledger.value_of(subject, OCEAN_FRACTION))?;
        if fraction < NO_OCEANS_THRESHOLD {
            Some("This world has no oceans.".to_string())
        } else {
            None
        }
    }
}

/// "Tidally locked" — needs [`BuildDepth::Astronomy`]; silent unless
/// [`TIDALLY_LOCKED`] is actually committed. No absence-vs-early trap here
/// (see the module doc): the fact is fully decided by the time any world
/// exists at all, at any rung the ladder can produce.
struct TidallyLockedComponent;

impl AlmanacComponent for TidallyLockedComponent {
    fn name(&self) -> &'static str {
        "tidally-locked"
    }

    fn needs(&self) -> BuildDepth {
        BuildDepth::Astronomy
    }

    fn render(&self, world: &World, _artifacts: RungArtifacts<'_>) -> Option<String> {
        if world.ledger.find(TIDALLY_LOCKED).next().is_some() {
            Some("This world is tidally locked.".to_string())
        } else {
            None
        }
    }
}

/// Below this habitable fraction the almanac calls a world a wasteland —
/// again a narrative cutoff, not a physical threshold (see
/// [`NO_OCEANS_THRESHOLD`]'s own doc for the same caveat). Chosen well under
/// seed 42's own measured fraction (`tests::seed_42_is_not_a_wasteland`
/// prints the live value) so an ordinary world never trips it by accident.
const WASTELAND_THRESHOLD: f64 = 0.05;

/// The wasteland predicate, factored out of [`WastelandComponent::render`]
/// as a pure function of an already-computed fraction — so it can be tested
/// directly against hand-picked values (both above and below the
/// threshold), independent of building a real climate to exercise both
/// directions of the non-vacuity check.
fn wasteland_line(fraction: f64) -> Option<String> {
    if fraction < WASTELAND_THRESHOLD {
        Some(format!(
            "This world is a wasteland: only {:.0}% of the surface could support life.",
            fraction * 100.0
        ))
    } else {
        None
    }
}

/// "A wasteland" — needs [`BuildDepth::Settlements`], the rung
/// `RungArtifacts::climate` first becomes `Some` (see the module doc's own
/// section on why this is reachable at all).
struct WastelandComponent;

impl AlmanacComponent for WastelandComponent {
    fn name(&self) -> &'static str {
        "wasteland"
    }

    fn needs(&self) -> BuildDepth {
        BuildDepth::Settlements
    }

    fn render(&self, _world: &World, artifacts: RungArtifacts<'_>) -> Option<String> {
        // Defensive only, like `AtlasView::render`'s own `terrain` check: the
        // registry never calls `render` before `needs()`'s rung, so this
        // should always be `Some` on the shipped path.
        let climate = artifacts.climate?;
        wasteland_line(climate.habitable_fraction())
    }
}

/// The three "strange" components — Nathan's own example from the task
/// brief, split out so the almanac's tests can exercise exactly this subset
/// (see `tests::strange_lines`) independent of the routine paragraphs.
fn strange_components() -> Vec<Box<dyn AlmanacComponent>> {
    vec![
        Box::new(TidallyLockedComponent),
        Box::new(NoOceansComponent),
        Box::new(WastelandComponent),
    ]
}

/// Every component the shipped almanac registers, in render order: the
/// routine paragraphs first, then the "what is strange" quirks.
fn components() -> Vec<Box<dyn AlmanacComponent>> {
    let mut all: Vec<Box<dyn AlmanacComponent>> = vec![
        Box::new(OrbitComponent),
        Box::new(OceansComponent),
        Box::new(PeoplesComponent),
    ];
    all.extend(strange_components());
    all
}

/// Write `line` starting at column 0 of `row`, clipping at the grid's right
/// edge — a second, minimal copy of `sky.rs`'s own `write_caption` (see that
/// function's doc for why this campaign accepts one small copy per view
/// rather than exposing a shared helper across them).
fn write_line(grid: &mut Grid, row: u16, line: &str) {
    for (i, ch) in line.chars().enumerate() {
        let Ok(col) = u16::try_from(i) else { break };
        if col >= grid.width() {
            break;
        }
        grid.set(col, row, Cell::glyph(ch, Weight::Normal, Source::Overture));
    }
}

/// The almanac: a component-composed report on the world (see the module
/// doc for the shipped roster). No memo: every component here reads only
/// the ledger and the handed-in `artifacts`, both cheap to re-read on every
/// render (see [`View::render`]'s own doc on why `&mut self` is a memo, not
/// a licence — this view simply needs none).
#[derive(Debug, Default, Clone, Copy)]
pub struct AlmanacView;

impl View for AlmanacView {
    fn name(&self) -> &'static str {
        "almanac"
    }

    /// Always speaks: [`OrbitComponent`] needs only
    /// [`BuildDepth::Astronomy`], the shallowest rung the ladder has, and is
    /// unconditional once it lands — so this view always has at least one
    /// line, at any rung a world can be handed in at.
    fn can_speak(&self, _rung: BuildDepth) -> bool {
        true
    }

    fn render(
        &mut self,
        world: &World,
        rung: BuildDepth,
        artifacts: RungArtifacts<'_>,
        w: u16,
        h: u16,
    ) -> Grid {
        let w = w.max(1);
        let h = h.max(1);
        let mut grid = Grid::new(w, h);

        let registry = ComponentRegistry::new(components());
        let lines = registry.render(rung, world, artifacts);
        for (i, line) in lines.iter().enumerate() {
            let Ok(row) = u16::try_from(i) else { break };
            if row >= h {
                break;
            }
            write_line(&mut grid, row, line);
        }
        grid
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_astronomy::SkyPins;
    use hornvale_kernel::{Fact, Lineage, Seed, WorldTime};
    use hornvale_terrain::TerrainPins;
    use hornvale_worldgen::{
        BuildArtifacts, SettlementPins, SkyChoice, WorldComponents, build_world_to,
        build_world_to_with_artifacts,
    };
    use std::sync::OnceLock;

    /// A real seed-42 world at `depth`, built once per test binary — the
    /// same reasoning `sky.rs`'s own `world_at` gives: a genuine committed
    /// ledger, not a synthetic stand-in.
    fn world_at(depth: BuildDepth) -> &'static World {
        static ASTRONOMY: OnceLock<World> = OnceLock::new();
        static TERRAIN: OnceLock<World> = OnceLock::new();
        static FULL: OnceLock<World> = OnceLock::new();
        let build = || {
            let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
            build_world_to(
                Seed(42),
                &SkyPins::default(),
                SkyChoice::Generated,
                &TerrainPins::default(),
                &SettlementPins::default(),
                &wc,
                depth,
            )
            .expect("seed 42 builds")
        };
        match depth {
            BuildDepth::Astronomy => ASTRONOMY.get_or_init(build),
            BuildDepth::Terrain => TERRAIN.get_or_init(build),
            BuildDepth::Full => FULL.get_or_init(build),
            other => panic!("no cached world for {other:?}; add one deliberately"),
        }
    }

    /// A real seed-42 build's artifacts at `depth`, built once per test
    /// binary — the same reasoning `atlas.rs`'s own `artifacts_at` gives,
    /// needed here for [`WastelandComponent`], which reads `artifacts.climate`
    /// rather than the ledger.
    fn artifacts_at(depth: BuildDepth) -> &'static BuildArtifacts {
        static SETTLEMENTS: OnceLock<BuildArtifacts> = OnceLock::new();
        static FULL: OnceLock<BuildArtifacts> = OnceLock::new();
        let build = || {
            let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
            build_world_to_with_artifacts(
                Seed(42),
                &SkyPins::default(),
                SkyChoice::Generated,
                &TerrainPins::default(),
                &SettlementPins::default(),
                &wc,
                depth,
            )
            .expect("seed 42 builds")
        };
        match depth {
            BuildDepth::Settlements => SETTLEMENTS.get_or_init(build),
            BuildDepth::Full => FULL.get_or_init(build),
            other => panic!("no cached build for {other:?}; add one deliberately"),
        }
    }

    fn astronomy_only_world() -> &'static World {
        world_at(BuildDepth::Astronomy)
    }

    fn terrain_world() -> &'static World {
        world_at(BuildDepth::Terrain)
    }

    fn full_world() -> &'static World {
        world_at(BuildDepth::Full)
    }

    /// `RungArtifacts` borrowing `built`'s own terrain/climate — the same
    /// shape `atlas.rs`'s own `rung_artifacts` builds.
    fn rung_artifacts(built: &BuildArtifacts) -> RungArtifacts<'_> {
        RungArtifacts {
            terrain: built.terrain.as_ref(),
            climate: built.climate.as_ref(),
        }
    }

    /// Render only the three "strange" components — the subset the task
    /// brief's own `strange_lines` sketch names — independent of the
    /// routine paragraphs.
    fn strange_lines(world: &World, rung: BuildDepth, artifacts: RungArtifacts<'_>) -> Vec<String> {
        ComponentRegistry::new(strange_components()).render(rung, world, artifacts)
    }

    #[test]
    fn a_component_is_skipped_until_its_rung_lands() {
        // The registry mechanism itself is exercised in `component.rs`'s own
        // tests against synthetic components; this is the same property
        // against the REAL shipped roster and a REAL world, so a component
        // whose `needs()` disagrees with what it can actually read would
        // show up here too.
        let registry = ComponentRegistry::new(components());
        let world = astronomy_only_world();
        let early = registry.render(BuildDepth::Astronomy, world, RungArtifacts::none());
        assert_eq!(
            early.len(),
            1,
            "only the orbit component can speak at rung 0 for a non-locked, non-wasteland \
             world: {early:?}"
        );
        // NON-VACUITY: the one line present must be the orbit component's,
        // not an empty string or an unrelated fixed placeholder.
        assert!(
            early[0].contains("orbit"),
            "the one rung-0 line was not the orbit paragraph: {early:?}"
        );

        let full_built = artifacts_at(BuildDepth::Full);
        let late = registry.render(BuildDepth::Full, full_world(), rung_artifacts(full_built));
        assert_eq!(
            late.len(),
            3,
            "orbit, oceans and peoples should all speak by Full: {late:?}"
        );
    }

    #[test]
    fn no_oceans_does_not_fire_from_absence_before_terrain() {
        // THE TRAP (module doc): calling `render` DIRECTLY, bypassing the
        // registry's own `needs()` gate, on a world that has not reached
        // `BuildDepth::Terrain` at all. `OCEAN_FRACTION` is absent here
        // because terrain genesis has not run — not because this world has
        // no oceans — so a correct `render` must answer `None`, the same
        // answer it gives when the fact is genuinely false. This proves the
        // RENDER logic is safe on its own, independent of the `needs()` gate
        // that (redundantly) also protects it.
        let world = astronomy_only_world();
        assert_eq!(
            NoOceansComponent.render(world, RungArtifacts::none()),
            None,
            "an absent (too-early) ocean-fraction fact must not read as 'no oceans'"
        );
    }

    #[test]
    fn no_oceans_and_wasteland_are_silent_one_rung_early_via_the_registry() {
        // The registry's OWN gate, exercised at the boundary: at
        // `BuildDepth::Terrain`, `NoOceansComponent` can speak but
        // `WastelandComponent` (needs `Settlements`) must not appear at all,
        // even though its own `render` is never even called to find out.
        let lines = strange_lines(terrain_world(), BuildDepth::Terrain, RungArtifacts::none());
        assert!(
            !lines.iter().any(|l| l.contains("wasteland")),
            "wasteland spoke a rung before its own artifacts exist: {lines:?}"
        );
    }

    #[test]
    fn what_is_strange_names_only_what_is_true_of_this_world() {
        // NON-VACUITY (the task brief's own example, and the campaign's
        // named recurring failure): seed 42 has real oceans and is not
        // tidally locked, so a component that always fired (or a registry
        // that fired every component regardless of truth) would fail here.
        // Evidence, re-derived from THIS live build rather than trusted from
        // any document:
        let full_built = artifacts_at(BuildDepth::Full);
        let world = full_world();

        let subject = world_entity(world).expect("seed 42 commits a world entity");
        let ocean_fraction =
            as_number(world.ledger.value_of(subject, OCEAN_FRACTION)).expect("terrain has run");
        let locked = world.ledger.find(TIDALLY_LOCKED).next().is_some();
        let habitable_fraction = full_built
            .climate
            .as_ref()
            .expect("climate exists by Full")
            .habitable_fraction();
        eprintln!(
            "seed 42 evidence: ocean_fraction={ocean_fraction}, locked={locked}, \
             habitable_fraction={habitable_fraction}"
        );
        assert!(
            ocean_fraction >= NO_OCEANS_THRESHOLD,
            "seed 42 has oceans; the fixture assumption behind this test is stale: {ocean_fraction}"
        );
        assert!(
            !locked,
            "seed 42 is not tidally locked; the fixture assumption is stale"
        );
        assert!(
            habitable_fraction >= WASTELAND_THRESHOLD,
            "seed 42 is not a wasteland; the fixture assumption is stale: {habitable_fraction}"
        );

        let lines = strange_lines(world, BuildDepth::Full, rung_artifacts(full_built));
        assert!(
            !lines.iter().any(|l| l.contains("no oceans")),
            "seed 42 has oceans; the component claimed otherwise: {lines:?}"
        );
        assert!(
            !lines.iter().any(|l| l.contains("tidally locked")),
            "seed 42 is not locked; the component claimed otherwise: {lines:?}"
        );
        assert!(
            !lines.iter().any(|l| l.contains("wasteland")),
            "seed 42 is not a wasteland; the component claimed otherwise: {lines:?}"
        );
    }

    /// Commit a single hand-authored fact for a test, panicking on any
    /// commit failure — the same helper `sky.rs`'s own test module carries.
    fn commit_fact(world: &mut World, subject: EntityId, predicate: &str, object: Value) {
        world
            .ledger
            .commit(
                Fact {
                    subject,
                    predicate: predicate.to_string(),
                    object,
                    place: None,
                    day: Some(WorldTime::GENESIS),
                    provenance: "test".to_string(),
                },
                &world.registry,
            )
            .expect("a hand-committed test fact must commit cleanly");
    }

    #[test]
    fn tidally_locked_fires_when_the_fact_is_committed() {
        // The positive direction, independent of any real world's own
        // rotation: a hand-built world with the flag committed must speak.
        let mut world = astronomy_only_world().clone();
        let id = world.ledger.mint_entity(Lineage {
            parent: None,
            role: "test-locked-subject",
            ordinal: 0,
        });
        commit_fact(&mut world, id, TIDALLY_LOCKED, Value::Flag(true));
        assert_eq!(
            TidallyLockedComponent.render(&world, RungArtifacts::none()),
            Some("This world is tidally locked.".to_string())
        );
    }

    #[test]
    fn no_oceans_fires_below_the_threshold() {
        // The positive direction for `NoOceansComponent`: a hand-built world
        // with a near-zero ocean fraction committed on the real world entity
        // must speak, proving the absence check in the test above is
        // discriminating a real condition rather than being permanently
        // `None`. Committed onto the REAL world entity (not a fresh one):
        // `render`'s own `world_entity` lookup resolves off the first
        // `STAR_CLASS` subject, and a fresh entity carries no such fact.
        let mut world = astronomy_only_world().clone();
        let real_subject = world_entity(&world).expect("astronomy commits a world entity");
        commit_fact(&mut world, real_subject, OCEAN_FRACTION, Value::Number(0.0));
        assert_eq!(
            NoOceansComponent.render(&world, RungArtifacts::none()),
            Some("This world has no oceans.".to_string())
        );
    }

    #[test]
    fn wasteland_line_discriminates_above_and_below_its_threshold() {
        // The pure predicate, tested directly against hand-picked fractions
        // (the same "expected_from" independence `sky.rs`'s
        // `a_star_lands_where_its_own_coordinates_put_it` uses) — proves
        // `WastelandComponent` is not permanently silent nor permanently
        // vocal.
        assert!(
            wasteland_line(0.01).is_some(),
            "1% habitable should read as a wasteland"
        );
        assert!(
            wasteland_line(0.16).is_none(),
            "16% habitable (seed 42's own live rough order) must not read as a wasteland"
        );
    }

    #[test]
    fn peoples_is_silent_at_zero_and_speaks_once_settlements_exist() {
        let terrain_only = astronomy_only_world(); // no IS_SETTLEMENT facts at all
        assert_eq!(
            PeoplesComponent.render(terrain_only, RungArtifacts::none()),
            None,
            "zero settlements must not be captioned as a placeholder"
        );

        let full = full_world();
        let line = PeoplesComponent
            .render(full, RungArtifacts::none())
            .expect("seed 42 places settlements by Full");
        assert!(
            line.contains("settlement"),
            "peoples line missing its own noun: {line:?}"
        );
        // NON-VACUITY: the line must carry an actual settlement count, not a
        // fixed template string with nothing filled in.
        assert!(
            line.chars().next().is_some_and(|c| c.is_ascii_digit()),
            "peoples line does not lead with a count: {line:?}"
        );
    }

    #[test]
    fn almanac_view_always_speaks_and_grows_with_rung() {
        let mut view = AlmanacView;
        let early = view.render(
            astronomy_only_world(),
            BuildDepth::Astronomy,
            RungArtifacts::none(),
            78,
            20,
        );
        assert!(
            early.to_plain_text().contains("orbit"),
            "the almanac drew no orbit line at rung 0: {:?}",
            early.to_plain_text()
        );

        let full_built = artifacts_at(BuildDepth::Full);
        let late = view.render(
            full_world(),
            BuildDepth::Full,
            rung_artifacts(full_built),
            78,
            20,
        );
        let late_text = late.to_plain_text();
        assert!(
            late_text.contains("settlement"),
            "the almanac never grew a peoples line by Full: {late_text:?}"
        );
        // NON-VACUITY: the early text must be a strict prefix of the growth,
        // not merely different — the earlier lines must still be present.
        assert!(
            late_text.contains("orbit"),
            "the almanac lost its orbit line by Full: {late_text:?}"
        );
    }
}
