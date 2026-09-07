//! Everything the lot reads, assembled once per world: the occupation
//! records, the present, the species life histories, the strife field, the
//! terrain (for coordinates and hazards), the sky, and per-occupation
//! shapes and hazards. Reading the ledger is cheap; rebuilding terrain and
//! the demography stack is seconds, so a context is built once and every
//! draw borrows it.

use std::collections::BTreeMap;

use hornvale_history::record::Founding;
use hornvale_kernel::{EntityId, Value, Vertex, World};

use crate::LotError;
use crate::hazard::{Hazard, e0};
use crate::shape::{EPOCH_YEARS, Shape, population_at, shape_of};

/// One occupation, prepared for drawing.
/// type-audit: bare-ok(count: e0), bare-ok(count: births_per_year), bare-ok(count: lifespan_years), bare-ok(count: maturity_years), bare-ok(index: mother), bare-ok(index: daughters), bare-ok(identifier-text: name)
#[derive(Clone, Debug)]
pub struct Prepared {
    /// The committed record.
    pub record: hornvale_history::record::OccupationRecord,
    /// The reconstructed population shape.
    pub shape: Shape,
    /// The hazard this people faces at this site.
    pub hazard: Hazard,
    /// Life expectancy at birth under that hazard.
    pub e0: f64,
    /// Births per person-year: `1 / e0` (stationarity, spec §4.2).
    pub births_per_year: f64,
    /// The people's allometric lifespan.
    pub lifespan_years: f64,
    /// The people's age at maturity.
    pub maturity_years: f64,
    /// The mother occupation's index into `occupations`, if founded from one.
    pub mother: Option<usize>,
    /// The daughter occupations founded from this one, by index.
    pub daughters: Vec<usize>,
    /// The settlement's glossed name, if this occupation is alive (a dead
    /// occupation never carries `IS_SETTLEMENT`/`NAME_GLOSS`, so it never
    /// carries a name — spec §5's `where` slot: "name only for a living
    /// settlement").
    pub name: Option<String>,
}

/// The assembled context.
/// type-audit: bare-ok(count: start_year), bare-ok(count: present_year), bare-ok(count: epoch_years), bare-ok(index: seed), bare-ok(index: by_entity)
pub struct LotContext {
    /// The world's seed value (the hash expansion's first input).
    pub seed: u64,
    /// The bake's first year.
    pub start_year: f64,
    /// The bake's present.
    pub present_year: f64,
    /// The epoch length the curve is binned in.
    pub epoch_years: f64,
    /// Every occupation, prepared, in `occupation_records` order.
    pub occupations: Vec<Prepared>,
    /// Entity id → index into `occupations`.
    pub by_entity: BTreeMap<EntityId, usize>,
    /// Realized social people, in ledger commit order. Ordinary worlds have
    /// none; synthetic social probes opt in by committing `is-person` facts.
    pub social_people: Vec<EntityId>,
    /// The rebuilt terrain (coordinates; hazards). `pub(crate)`: nothing
    /// outside this crate reads it (checked against `windows/lab`, `cli`
    /// and `clients/lot/wasm` — the final review's item 6); every read is
    /// through [`crate::slots`] or [`LotContext::lat_lon`].
    pub(crate) terrain: hornvale_terrain::GeneratedTerrain,
    /// The composition root's registries (names, minds, societies).
    /// `pub(crate)` for the same reason as [`LotContext::terrain`].
    pub(crate) components: hornvale_worldgen::WorldComponents,
    /// Every settlement standing on each Geosphere vertex, in
    /// `all_settlements` (commit) order — the inversion of the settlements'
    /// own [`hornvale_settlement::VERTEX_ID`] facts. A vertex may hold more
    /// than one settlement (successive peoples at one site), so this is a
    /// `Vec`, never a single id; [`crate::slots`] picks the one whose people
    /// matches the lot's. Stays `pub`, unlike its four siblings here: this
    /// crate's own integration suite (`tests/suite/slots.rs`) reads it
    /// directly, and an integration test is a separate crate as far as
    /// visibility is concerned — `pub(crate)` would not compile there.
    pub settlements_by_vertex: BTreeMap<Vertex, Vec<EntityId>>,
    /// The founding tree, read once out of the ledger — what the
    /// `held-true` slot's hearsay walk needs. `pub(crate)`, see
    /// [`LotContext::terrain`].
    pub(crate) lineage: hornvale_hearsay::lineage::Lineage,
    /// The world's generated star system and its derived calendar. Since
    /// The Zenith retired the tier-0 constant sun, every world carries one.
    /// `pub(crate)`, see [`LotContext::terrain`].
    pub(crate) sky: (hornvale_astronomy::StarSystem, hornvale_astronomy::Calendar),
    /// Births per whole year, summed over every occupation and sampled at
    /// each year's midpoint (`start_year + k + 0.5`). Precomputed once here
    /// so [`crate::draw::draw`]'s unpinned birth-year pick reads a table
    /// instead of resumming every occupation's shape on every draw.
    births_by_year: Vec<f64>,
    /// The cumulative sum of `births_by_year` — the birth-year draw's CDF.
    births_cdf: Vec<f64>,
}

impl LotContext {
    /// Births per whole year, summed over every occupation, sampled at each
    /// year's midpoint. Index `k` is the year `[start_year + k, start_year +
    /// k + 1)`.
    /// type-audit: bare-ok(count: return)
    pub fn births_by_year(&self) -> &[f64] {
        &self.births_by_year
    }
    /// The cumulative sum of [`LotContext::births_by_year`] — the birth-year
    /// draw's CDF, monotone nondecreasing, same length.
    /// type-audit: bare-ok(count: return)
    pub fn births_cdf(&self) -> &[f64] {
        &self.births_cdf
    }

    /// Select the deterministic synthetic person observed by a lot index.
    /// type-audit: bare-ok(index: index)
    pub fn social_person(&self, index: u64) -> Option<EntityId> {
        self.social_people
            .get((index as usize) % self.social_people.len().max(1))
            .copied()
    }

    /// Latitude/longitude of a Geosphere vertex, in degrees — the formula
    /// `domains/terrain/src/channel.rs`'s `lat_lon` uses, applied to this
    /// context's own rebuilt terrain (no accessor on `Geosphere` gives it
    /// directly). Public because both the draw's `places` listing and the
    /// story's `where`/`sky` slots need it, and a second copy of a
    /// coordinate convention is how two renderings of one site drift apart.
    /// type-audit: bare-ok(count: return)
    pub fn lat_lon(&self, vertex: Vertex) -> (f64, f64) {
        let position = self.terrain.geosphere().position(vertex);
        let latitude = hornvale_kernel::math::asin(position[2].clamp(-1.0, 1.0)).to_degrees();
        let longitude = hornvale_kernel::math::atan2(position[1], position[0]).to_degrees();
        (latitude, longitude)
    }
}

/// Assemble the context for a world. Refuses a world with no occupations or
/// one saved before The Lot (no `occ-person-years` fact anywhere).
// Named construction site (decision 0092): the lot's own composition root —
// sculpts terrain, fits climate, and fits the coexistence stack once per
// context build, so every later draw reads the already-derived result.
#[allow(clippy::disallowed_methods)]
pub fn assemble(world: &World) -> Result<LotContext, LotError> {
    let records = hornvale_worldgen::occupation_records(world);
    if records.is_empty() {
        return Err(LotError::NoOccupations);
    }
    if world
        .ledger
        .find(hornvale_history::OCC_PERSON_YEARS)
        .next()
        .is_none()
    {
        return Err(LotError::NoPersonYears);
    }
    let wc = hornvale_worldgen::WorldComponents::assemble()
        .map_err(|e| LotError::Build(e.to_string()))?;
    let terrain =
        hornvale_worldgen::terrain_of(world).map_err(|e| LotError::Build(e.to_string()))?;
    let climate = hornvale_worldgen::climate_from(world, &terrain)
        .map_err(|e| LotError::Build(e.to_string()))?;
    let report = hornvale_worldgen::demography_report_from(world, &wc, &terrain, &climate)
        .map_err(|e| LotError::Build(e.to_string()))?;
    let present_year = hornvale_worldgen::present_year(world);

    // The bake's own config, the one `build_world` actually uses (see
    // `windows/worldgen/src/lib.rs`'s `build_world` for the construction
    // site): `start_year`/`epoch_years` come from here rather than being
    // re-derived from the records, so a world built with a different config
    // is caught rather than silently mis-binned.
    let cfg = hornvale_worldgen::BakeConfig::default_millennia();
    if (cfg.epoch_years - EPOCH_YEARS).abs() > 1e-9 {
        return Err(LotError::Build(format!(
            "the bake's epoch_years ({}) disagrees with hornvale_lot::shape::EPOCH_YEARS ({EPOCH_YEARS})",
            cfg.epoch_years
        )));
    }
    let start_year = cfg.start_year;

    let by_entity: BTreeMap<EntityId, usize> =
        records.iter().enumerate().map(|(i, r)| (r.id, i)).collect();
    let social_people = world
        .ledger
        .find(hornvale_person::IS_PERSON)
        .filter_map(|fact| match fact.object {
            Value::Flag(true)
                if world.ledger.facts_about(fact.subject).any(|candidate| {
                    candidate.predicate == hornvale_person::PERSON_SOCIAL_PROVENANCE
                }) =>
            {
                Some(fact.subject)
            }
            _ => None,
        })
        .collect();

    // Per-people life history, resolved once.
    let mut life: BTreeMap<&str, (f64, f64)> = BTreeMap::new();
    for r in &records {
        let people = r.core.people.0;
        if life.contains_key(people) {
            continue;
        }
        let bio = wc
            .biosphere
            .get_by_label(people)
            .ok_or_else(|| LotError::Build(format!("no body for people {people}")))?;
        let lh = hornvale_species::life_history(bio.mass, bio.thermal_strategy, bio.schedule);
        let lifespan = lh
            .lifespan
            .map(|y| y.get())
            .ok_or_else(|| LotError::Build(format!("people {people} has no lifespan")))?;
        // A non-positive lifespan would reach a census column as NaN (every
        // `lot-*` scaled-age metric divides by it): refuse loudly here
        // rather than letting a hazard/shape computation silently poison a
        // committed number downstream.
        if lifespan <= 0.0 {
            return Err(LotError::Build(format!(
                "people {people} has a non-positive lifespan ({lifespan})"
            )));
        }
        let maturity = lh
            .age_at_maturity
            .map(|y| y.get())
            .unwrap_or(lifespan * 0.2);
        life.insert(people, (lifespan, maturity));
    }

    let mut occupations = Vec::with_capacity(records.len());
    for r in &records {
        let (lifespan_years, maturity_years) = life[r.core.people.0];
        let strife = (*report.byproducts.strife.get(r.core.site)).clamp(0.0, 1.0);
        let hazard = Hazard {
            lifespan_years,
            strife,
        };
        let occ_e0 = e0(&hazard);
        let end = r.core.ended.unwrap_or(present_year);
        let p0 = match r.founded_from {
            Founding::Genesis(_) => hornvale_worldgen::GENESIS_POP,
            Founding::From(_) => hornvale_worldgen::DAUGHTER_POP,
        };
        let shape = shape_of(
            r.core.founded,
            end,
            r.core.peak_population,
            r.core.person_years,
            p0,
        );
        let mother = match r.founded_from {
            Founding::From(id) => by_entity.get(&id).copied(),
            Founding::Genesis(_) => None,
        };
        let name = if r.core.is_alive() {
            world
                .ledger
                .text_of(r.id, hornvale_kernel::NAME_GLOSS)
                .map(str::to_string)
        } else {
            None
        };
        occupations.push(Prepared {
            record: r.clone(),
            shape,
            hazard,
            e0: occ_e0,
            births_per_year: 1.0 / occ_e0,
            lifespan_years,
            maturity_years,
            mother,
            daughters: Vec::new(),
            name,
        });
    }
    for i in 0..occupations.len() {
        if let Some(m) = occupations[i].mother {
            occupations[m].daughters.push(i);
        }
    }

    // The births-per-year curve, precomputed once: at each whole year's
    // midpoint, the sum over every occupation of `births_per_year ×
    // population_at(shape, midpoint)` — and its running sum, the birth-year
    // draw's CDF.
    let years = (present_year - start_year).ceil().max(0.0) as usize;
    let mut births_by_year = Vec::with_capacity(years);
    let mut acc = 0.0;
    let mut births_cdf = Vec::with_capacity(years);
    for k in 0..years {
        let midpoint = start_year + k as f64 + 0.5;
        let total: f64 = occupations
            .iter()
            .map(|p| p.births_per_year * population_at(&p.shape, midpoint))
            .sum();
        births_by_year.push(total);
        acc += total;
        births_cdf.push(acc);
    }

    // The vertex→settlements inversion, read off the settlements' own
    // `hornvale_settlement::VERTEX_ID` facts. Commit order is preserved, so
    // element 0 of a shared vertex is the earliest-committed settlement
    // there.
    let mut settlements_by_vertex: BTreeMap<Vertex, Vec<EntityId>> = BTreeMap::new();
    for village in hornvale_settlement::all_settlements(world) {
        if let Some(Value::Number(n)) = world
            .ledger
            .value_of(village.id, hornvale_settlement::VERTEX_ID)
        {
            settlements_by_vertex
                .entry(Vertex(*n as u32))
                .or_default()
                .push(village.id);
        }
    }

    let lineage = hornvale_hearsay::lineage::lineage_of(&world.ledger);

    // Since The Zenith every world carries a generated sky unconditionally
    // (the tier-0 constant sun this used to guard against is retired), so
    // the `sky` slot always has a system and calendar to ask.
    let sky = match hornvale_worldgen::sky_of(world) {
        Ok(built) => (built.system().clone(), built.calendar().clone()),
        Err(e) => return Err(LotError::Build(e.to_string())),
    };

    Ok(LotContext {
        seed: world.seed.0,
        start_year,
        present_year,
        epoch_years: EPOCH_YEARS,
        occupations,
        by_entity,
        social_people,
        terrain,
        components: wc,
        settlements_by_vertex,
        lineage,
        sky,
        births_by_year,
        births_cdf,
    })
}
