//! The draw (spec §3, §4.4): pure hash expansion of (seed, index, label)
//! into a birth year, a site, and a life course under the site's hazard and
//! the community's committed fate. Nothing here draws a `Stream`.

use std::collections::BTreeMap;

use hornvale_history::record::CauseOfEnd;
use hornvale_kernel::{EntityId, Vertex};

use crate::context::{LotContext, OutbreakEvent};
use crate::endemic::{HazardBand, cause_weights_for_band};
use crate::hazard::{death_age, hazard_shares, hazard_shares_at_age, q_before};
use crate::shape::population_at;
use crate::{LotError, LotIndex, Pick};

/// A uniform in `[0, 1)` for one named choice of one lot in one world:
/// splitmix64 over `(seed, index, fnv1a(label))`, the `persona_of` idiom.
/// type-audit: bare-ok(index: seed), bare-ok(index: index), bare-ok(identifier-text: label), bare-ok(ratio: return)
pub fn uniform(seed: u64, index: u64, label: &str) -> f64 {
    let mut h: u64 = 0xcbf2_9ce4_8422_2325;
    for b in label.bytes() {
        h ^= u64::from(b);
        h = h.wrapping_mul(0x0000_0100_0000_01b3);
    }
    let mut x = seed ^ index.wrapping_mul(0x9E37_79B9_7F4A_7C15) ^ h;
    x = x.wrapping_add(0x9E37_79B9_7F4A_7C15);
    x = (x ^ (x >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
    x = (x ^ (x >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
    x ^= x >> 31;
    (x >> 11) as f64 / (1u64 << 53) as f64
}

/// How a life ended, or that it has not.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Ending {
    /// Still living at the world's present.
    Alive,
    /// Died under the continuous hazard (infant / background / senescent by age band).
    Hazard,
    /// Died in a committed outbreak that did not end the community.
    Outbreak(hornvale_kernel::KindId),
    /// Died in the community's own committed ending.
    CommunityFate(CauseOfEnd),
}

/// The named cause of a dead projected life.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DeathCause {
    /// A catalogue pathogen.
    Pathogen(hornvale_kernel::KindId),
    /// Violence within the background hazard.
    Violence,
    /// Senescence.
    Age,
    /// The record's unnamed residual.
    Unnamed,
    /// A non-plague community ending.
    Community(CauseOfEnd),
}

/// The committed or derived evidence that supplied a life's cause.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CauseProvenance {
    /// A community ending supplied the cause.
    CommunityFate {
        /// The occupation whose ending supplied the cause.
        occupation: EntityId,
        /// The committed ending cause.
        cause: CauseOfEnd,
    },
    /// A paired outbreak fact supplied the cause.
    Outbreak {
        /// The occupation struck by the outbreak.
        occupation: EntityId,
        /// The outbreak event entity.
        event: EntityId,
        /// The pathogen named by the event.
        pathogen: hornvale_kernel::KindId,
    },
    /// The continuous hazard attribution supplied the cause.
    Hazard {
        /// The occupation whose site supplied the hazard inputs.
        occupation: EntityId,
    },
}

impl DeathCause {
    /// Reader-facing cause label.
    /// type-audit: bare-ok(prose: return)
    pub fn label(&self) -> String {
        match self {
            DeathCause::Pathogen(kind) => kind.0.replace('-', " "),
            DeathCause::Violence => "violence".to_string(),
            DeathCause::Age => "age".to_string(),
            DeathCause::Unnamed => "an unnamed cause".to_string(),
            DeathCause::Community(cause) => cause_name(*cause).to_string(),
        }
    }
}

fn cause_name(cause: CauseOfEnd) -> &'static str {
    match cause {
        CauseOfEnd::Famine => "famine",
        CauseOfEnd::Burned => "burning",
        CauseOfEnd::Plague => "plague",
        CauseOfEnd::Fled => "flight",
        CauseOfEnd::Migrated => "migration",
        CauseOfEnd::Breached => "a breach",
    }
}

/// One drawn life.
/// type-audit: bare-ok(count: birth_year), bare-ok(count: death_year), bare-ok(count: age_at_death), bare-ok(flag: matured), bare-ok(index: index), bare-ok(index: occ), bare-ok(index: moved_to), bare-ok(count: moved_year)
#[derive(Clone, Debug, PartialEq)]
pub struct Life {
    /// The reader's key.
    pub index: u64,
    /// The birth occupation's index into the context.
    pub occ: usize,
    /// The birth occupation's entity.
    pub occupation: EntityId,
    /// The birth site.
    pub site: Vertex,
    /// The birth year (bake years).
    pub birth_year: f64,
    /// The death year, or the present if alive.
    pub death_year: f64,
    /// Age at death or at the present.
    pub age_at_death: f64,
    /// Whether the life reached the people's age at maturity.
    pub matured: bool,
    /// How it ended.
    pub ending: Ending,
    /// The named cause, absent only while the life is alive.
    pub cause: Option<DeathCause>,
    /// Typed evidence supplying the cause, when the life is dead.
    pub cause_provenance: Option<CauseProvenance>,
    /// The occupation whose segment supplied the ending, or the current
    /// occupation when the life remains alive.
    pub ending_occupation: EntityId,
    /// The projection boundary carried with this observation.
    pub projection: crate::projection::Projection,
    /// The occupation the life moved to when its birth community ended
    /// inside its span and a daughter took the survivors, if any.
    pub moved_to: Option<usize>,
    /// The year of that move, if any.
    pub moved_year: Option<f64>,
    /// Which pins were applied.
    pub pick: Pick,
}

/// The bake's own loss fraction in a war, breach, famine or plague ending
/// (`WAR_LOSS` = 0.3), or in an orderly departure (`1 - MIGRATE_SURVIVAL` =
/// 0.1) — spec §4.3: "`WAR_LOSS = 0.3` for `Burned`/`Breached`, `1 -
/// MIGRATE_SURVIVAL = 0.1` for `Fled`/`Migrated`, and for `Famine`/`Plague`
/// the same `WAR_LOSS`, because the bake commits no loss fraction for those
/// two endings and an authored second number would be no better founded".
fn loss_fraction(cause: CauseOfEnd) -> f64 {
    match cause {
        CauseOfEnd::Burned | CauseOfEnd::Breached | CauseOfEnd::Famine | CauseOfEnd::Plague => {
            hornvale_worldgen::WAR_LOSS
        }
        CauseOfEnd::Fled | CauseOfEnd::Migrated => 1.0 - hornvale_worldgen::MIGRATE_SURVIVAL,
    }
}

/// Births per year of one occupation at `year`.
fn births_at(ctx: &LotContext, occ: usize, year: f64) -> f64 {
    let p = &ctx.occupations[occ];
    p.births_per_year * population_at(&p.shape, year)
}

/// Whether occupation `occ` is alive (founded, not yet ended) at `year`.
fn alive_at(ctx: &LotContext, occ: usize, year: f64) -> bool {
    let r = &ctx.occupations[occ].record;
    r.core.founded <= year && r.core.ended.unwrap_or(ctx.present_year) > year
}

/// Draw one life.
pub fn draw(ctx: &LotContext, index: LotIndex, pick: &Pick) -> Result<Life, LotError> {
    let seed = ctx.seed;
    let i = index.0;
    // 1. Birth year: inverse CDF over the precomputed yearly-births curve,
    //    unless pinned.
    let birth_year = match pick.year {
        Some(y) => {
            if y < ctx.start_year || y >= ctx.present_year {
                return Err(LotError::YearOutsideSpan {
                    year: y,
                    start: ctx.start_year,
                    end: ctx.present_year,
                });
            }
            y
        }
        None => {
            let cdf = ctx.births_cdf();
            let total = cdf.last().copied().unwrap_or(0.0);
            if total <= 0.0 {
                return Err(LotError::NoOccupations);
            }
            let u = uniform(seed, i, "birth-year") * total;
            let k = cdf.iter().position(|&c| c >= u).unwrap_or(cdf.len() - 1);
            // `k` is the whole year the births-per-year curve says this
            // birth falls in; the fractional offset below places it
            // somewhere inside that year, uniformly. When exactly one
            // occupation contributed to `births_by_year[k]` and its span
            // does not cover the whole year (it opened or closed partway
            // through), this offset can land the birth OUTSIDE that
            // occupation's own tenure — the curve is binned by year, not by
            // the finer span an occupation actually held. When that
            // happens, `alive` below (step 2) comes back empty and the draw
            // returns `LotError::NoOccupations` rather than a life: rare
            // (it needs a single-contributor year AND an unlucky fractional
            // draw), and a caller sees an ordinary refused draw, the same
            // shape as every other `LotError` — never a panic or a
            // silently wrong life.
            ctx.start_year + k as f64 + uniform(seed, i, "birth-year-frac")
        }
    };
    // 2. Site: among occupations alive in that year, weighted by births.
    let alive: Vec<(usize, f64)> = ctx
        .occupations
        .iter()
        .enumerate()
        .filter(|(o, _)| alive_at(ctx, *o, birth_year))
        .map(|(o, _)| (o, births_at(ctx, o, birth_year)))
        .filter(|(_, w)| *w > 0.0)
        .collect();
    let occ = match pick.site {
        Some(v) => alive
            .iter()
            .find(|(o, _)| ctx.occupations[*o].record.core.site == v)
            .map(|(o, _)| *o)
            .ok_or(LotError::SiteNotAliveInYear {
                site: v,
                year: birth_year,
            })?,
        None => {
            let total: f64 = alive.iter().map(|(_, w)| w).sum();
            let mut u = uniform(seed, i, "site") * total;
            let mut chosen = alive
                .last()
                .map(|(o, _)| *o)
                .ok_or(LotError::NoOccupations)?;
            for (o, w) in &alive {
                if u < *w {
                    chosen = *o;
                    break;
                }
                u -= w;
            }
            chosen
        }
    };
    // 3. Life course: draw an age at death under the birth site's hazard,
    //    then splice in the community's own fate at every ending the life
    //    overlaps, following daughters until either the life ends or the
    //    trail runs out.
    let p = &ctx.occupations[occ];
    // `death_age` only tabulates survival out to `2 × lifespan_years`
    // (`survival_table`'s own range): a draw whose `u` falls below `S(2L)`
    // — the rare, long tail of a hazard this shallow — returns `2L` itself
    // rather than extrapolating past the table, so this age is capped there
    // even though the draw is otherwise continuous.
    let mut age = death_age(&p.hazard, 1.0 - uniform(seed, i, "death"));
    let mut ending = Ending::Hazard;
    let mut cause = None;
    let mut cause_provenance = None;
    let mut moved_to = None;
    let mut moved_year = None;
    let mut cur = occ;
    let mut ending_occupation = p.record.id;
    // This walk terminates because `founded_from` is acyclic BY
    // CONSTRUCTION: a daughter is always founded strictly after its mother
    // (`daughter.core.founded == end`, and `end > mother.core.founded` for
    // any occupation with a committed ending), so following `daughters`
    // forward can never revisit an occupation already on the trail. Nothing
    // in the type system proves that, though — it is a property of how the
    // bake commits records, not of this loop — so a bound is added anyway:
    // at most one hop per occupation in the whole context, which the trail
    // could visit at most once each even in the worst case.
    'course: for _ in 0..ctx.occupations.len() {
        let r = &ctx.occupations[cur].record;
        let segment_start = moved_year.unwrap_or(birth_year);
        let natural_end = birth_year + age;
        for event in ctx
            .outbreaks_by_occupation
            .get(&r.id)
            .into_iter()
            .flatten()
            .filter(|event| {
                event.year >= segment_start
                    && event.year < natural_end
                    && r.core.ended.is_none_or(|end| event.year <= end)
            })
        {
            let is_plague_ending = r.core.cause == Some(CauseOfEnd::Plague)
                && r.core
                    .ended
                    .is_some_and(|end| (end - event.year).abs() < 1e-9);
            if is_plague_ending {
                continue;
            }
            let population = population_at(&ctx.occupations[cur].shape, event.year);
            let event_draw = uniform(seed, i, &format!("outbreak-{}", event.event.get()));
            if outbreak_kills(event.deaths, population, event_draw) {
                age = event.year - birth_year;
                ending = Ending::Outbreak(event.pathogen);
                cause = Some(DeathCause::Pathogen(event.pathogen));
                ending_occupation = r.id;
                cause_provenance = Some(CauseProvenance::Outbreak {
                    occupation: r.id,
                    event: event.event,
                    pathogen: event.pathogen,
                });
                break 'course;
            }
        }
        let Some(end) = r.core.ended else { break };
        if end <= birth_year || end >= birth_year + age {
            break;
        }
        let end_cause = r.core.cause.unwrap_or(CauseOfEnd::Fled);
        if uniform(seed, i, &format!("fate-{}", r.id.0)) < loss_fraction(end_cause) {
            age = end - birth_year;
            ending = Ending::CommunityFate(end_cause);
            let (named, provenance) = community_death_cause(ctx, r.id, end, end_cause)?;
            ending_occupation = r.id;
            cause_provenance = Some(provenance);
            cause = Some(named);
            break;
        }
        // Survivors: the daughter founded on the ending year, if any (the
        // hearsay witness rule: refounded on exactly the ending day).
        let daughter = ctx.occupations[cur]
            .daughters
            .iter()
            .copied()
            .find(|&d| (ctx.occupations[d].record.core.founded - end).abs() < 1e-9);
        match daughter {
            Some(d) => {
                moved_to = Some(d);
                moved_year = Some(end);
                cur = d;
                ending_occupation = ctx.occupations[d].record.id;
            }
            None => {
                age = end - birth_year;
                ending = Ending::CommunityFate(end_cause);
                let (named, provenance) = community_death_cause(ctx, r.id, end, end_cause)?;
                ending_occupation = r.id;
                cause_provenance = Some(provenance);
                cause = Some(named);
                break;
            }
        }
    }
    let (death_year, alive_now) = if birth_year + age >= ctx.present_year {
        (ctx.present_year, true)
    } else {
        (birth_year + age, false)
    };
    if alive_now {
        ending = Ending::Alive;
        age = ctx.present_year - birth_year;
        cause = None;
        cause_provenance = None;
    } else if ending == Ending::Hazard {
        let ending = &ctx.occupations[cur];
        cause = Some(hazard_cause(
            ctx,
            ending.record.core.site,
            death_year,
            &ending.hazard,
            age,
            uniform(seed, i, "cause"),
        ));
        ending_occupation = ending.record.id;
        cause_provenance = Some(CauseProvenance::Hazard {
            occupation: ending.record.id,
        });
    }
    Ok(Life {
        index: i,
        occ,
        occupation: p.record.id,
        site: p.record.core.site,
        birth_year,
        death_year,
        age_at_death: age,
        matured: age >= p.maturity_years,
        ending,
        cause,
        cause_provenance,
        ending_occupation,
        projection: crate::projection::Projection::composite(
            crate::projection::SourceCohort {
                people: p.record.core.people,
                site: p.record.core.site,
                year: birth_year,
            },
            false,
        ),
        moved_to,
        moved_year,
        pick: *pick,
    })
}

/// Whether one outbreak draw kills a host.
/// type-audit: bare-ok(count: deaths), bare-ok(count: population), bare-ok(ratio: draw), bare-ok(flag: return)
pub fn outbreak_kills(deaths: f64, population: f64, draw: f64) -> bool {
    population > 0.0 && draw < (deaths.max(0.0) / population).clamp(0.0, 1.0)
}

fn community_death_cause(
    ctx: &LotContext,
    occupation: EntityId,
    year: f64,
    cause: CauseOfEnd,
) -> Result<(DeathCause, CauseProvenance), LotError> {
    if cause != CauseOfEnd::Plague {
        return Ok((
            DeathCause::Community(cause),
            CauseProvenance::CommunityFate { occupation, cause },
        ));
    }
    let event = ctx
        .outbreaks_by_occupation
        .get(&occupation)
        .and_then(|events| closing_outbreak(events, year))
        .ok_or_else(|| {
            LotError::Build(format!(
                "Plague ending for occupation {} has no outbreak event at year {year}",
                occupation.get()
            ))
        })?;
    Ok((
        DeathCause::Pathogen(event.pathogen),
        CauseProvenance::Outbreak {
            occupation,
            event: event.event,
            pathogen: event.pathogen,
        },
    ))
}

fn closing_outbreak(events: &[OutbreakEvent], year: f64) -> Option<&OutbreakEvent> {
    events
        .iter()
        .rfind(|event| (event.year - year).abs() < 1e-9)
}

#[cfg(test)]
mod tests {
    use super::{OutbreakEvent, closing_outbreak};
    use hornvale_kernel::{EntityId, KindId};
    use std::num::NonZeroU64;

    #[test]
    fn closing_same_year_outbreak_is_the_second_pathogen_event() {
        let events = [
            OutbreakEvent {
                event: EntityId(NonZeroU64::new(1).unwrap()),
                year: 1200.0,
                pathogen: KindId("the-pest"),
                deaths: 1.0,
            },
            OutbreakEvent {
                event: EntityId(NonZeroU64::new(2).unwrap()),
                year: 1200.0,
                pathogen: KindId("the-pox"),
                deaths: 99.0,
            },
        ];
        let closing = closing_outbreak(&events, 1200.0).expect("closing event");
        assert_eq!(closing.event.get(), 2);
        assert_eq!(closing.pathogen, KindId("the-pox"));
        assert_eq!(closing.deaths, 99.0);
    }
}

fn hazard_cause(
    ctx: &LotContext,
    site: Vertex,
    year: f64,
    hazard: &crate::hazard::Hazard,
    age: f64,
    mut draw: f64,
) -> DeathCause {
    let shares = hazard_shares_at_age(hazard, age);
    let bands = [
        (HazardBand::Infant, shares.infant),
        (HazardBand::Background, shares.background),
        (HazardBand::Senescent, shares.senescent),
    ];
    let mut weighted = Vec::<(DeathCause, f64)>::new();
    for (band, band_weight) in bands {
        for cause in cause_weights_for_band(ctx, site, year, hazard.strife, band) {
            if let Some((_, weight)) = weighted.iter_mut().find(|(named, _)| *named == cause.cause)
            {
                *weight += band_weight * cause.weight;
            } else {
                weighted.push((cause.cause, band_weight * cause.weight));
            }
        }
    }
    let fallback = weighted
        .last()
        .map(|(cause, _)| cause.clone())
        .unwrap_or(DeathCause::Unnamed);
    for (cause, weight) in weighted {
        if draw < weight {
            return cause;
        }
        draw -= weight;
    }
    fallback
}

/// The births-per-epoch curve, total and per people, and the world's
/// souls-ever total (spec §4.2, §8's H-P6).
///
/// **`souls_ever` is the binned sum, `births_by_epoch.iter().sum()`, not the
/// closed-form `Σ_o births_per_year × person_years` §4.2 states — measured
/// (seed 42) to disagree from it by `5.16e-6` relative, just over this
/// struct's own `1e-6` design target. A piecewise-linear population curve
/// sampled at year MIDPOINTS integrates exactly to the shape's own
/// `shape::integral` only over a WHOLE number of full years; an occupation
/// whose span does not land on a whole-year boundary (most of them: `founded`
/// is a bake year, not necessarily an integer) contributes a boundary-year
/// discrepancy the midpoint rule cannot close. The two only ever differ at
/// the `5e-6` level (nowhere near the `1e-3` a real accounting error would
/// show), so this field reports the number the curve ACTUALLY sums to
/// (never contradicts `births_by_epoch`) rather than a formula the curve
/// only approximates.
/// type-audit: bare-ok(count: epoch_years), bare-ok(count: start_year), bare-ok(count: present_year), bare-ok(count: births_by_epoch), bare-ok(count: souls_ever), bare-ok(count: births_by_people_by_epoch)
#[derive(Clone, Debug, PartialEq)]
pub struct Curve {
    /// The epoch length the curve is binned in (`shape::EPOCH_YEARS`).
    pub epoch_years: f64,
    /// The bake year epoch 0 opens on — the curve's own anchor, copied from
    /// [`LotContext::start_year`]. Carried on the curve rather than left to
    /// the caller because `lot/curve/v1` is rendered from a `Curve` alone
    /// (`json::curve_json`), and a series of per-epoch counts with no origin
    /// year cannot be placed on an axis at all.
    pub start_year: f64,
    /// The bake year the curve runs to — [`LotContext::present_year`],
    /// carried for the same reason as `start_year`.
    pub present_year: f64,
    /// Births per epoch, summed over every occupation.
    pub births_by_epoch: Vec<f64>,
    /// Births per epoch, summed per people (keyed by the people's label).
    pub births_by_people_by_epoch: BTreeMap<String, Vec<f64>>,
    /// How many lives the world has ever held: `births_by_epoch`'s own sum
    /// (see this struct's doc for why it is not the closed form directly).
    pub souls_ever: f64,
}

/// The births-per-epoch curve (spec §4.2's stationary births lens): the
/// precomputed yearly-births table, binned by `EPOCH_YEARS`, total and per
/// people, plus the world's souls-ever total.
pub fn curve(ctx: &LotContext) -> Curve {
    let years = ctx.births_by_year().len();
    let epoch_years = ctx.epoch_years;
    let epochs = ((years as f64) / epoch_years).ceil().max(1.0) as usize;
    let epoch_of = |k: usize| -> usize { ((k as f64) / epoch_years).floor() as usize };

    let mut births_by_epoch = vec![0.0; epochs];
    for (k, &b) in ctx.births_by_year().iter().enumerate() {
        let e = epoch_of(k).min(epochs - 1);
        births_by_epoch[e] += b;
    }

    let mut births_by_people_by_epoch: BTreeMap<String, Vec<f64>> = BTreeMap::new();
    for p in &ctx.occupations {
        let people = p.record.core.people.0.to_string();
        let entry = births_by_people_by_epoch
            .entry(people)
            .or_insert_with(|| vec![0.0; epochs]);
        for k in 0..years {
            let midpoint = ctx.start_year + k as f64 + 0.5;
            let b = p.births_per_year * population_at(&p.shape, midpoint);
            if b > 0.0 {
                let e = epoch_of(k).min(epochs - 1);
                entry[e] += b;
            }
        }
    }

    // The binned sum, not the closed form `Σ births_per_year × person_years`
    // — see this struct's doc for the measured (seed 42) discrepancy and why.
    let souls_ever: f64 = births_by_epoch.iter().sum();

    Curve {
        epoch_years,
        start_year: ctx.start_year,
        present_year: ctx.present_year,
        births_by_epoch,
        births_by_people_by_epoch,
        souls_ever,
    }
}

/// One occupation alive at a picked year, with its coordinates and living
/// name.
/// type-audit: bare-ok(index: occ), bare-ok(count: latitude), bare-ok(count: longitude), bare-ok(count: population), bare-ok(count: births_per_year), bare-ok(identifier-text: people), bare-ok(identifier-text: name)
#[derive(Clone, Debug, PartialEq)]
pub struct Place {
    /// The occupation's index into the context.
    pub occ: usize,
    /// The occupation's own entity.
    pub entity: EntityId,
    /// The Geosphere vertex it sits on.
    pub site: Vertex,
    /// The occupying people's label.
    pub people: String,
    /// The settlement's glossed name, if it carries one (living occupations
    /// only — see [`crate::context::Prepared::name`]).
    pub name: Option<String>,
    /// Latitude, degrees.
    pub latitude: f64,
    /// Longitude, degrees.
    pub longitude: f64,
    /// The reconstructed population at the asked year.
    pub population: f64,
    /// Births per person-year for this occupation.
    pub births_per_year: f64,
}

/// Every occupation alive at `year` AND contributing a positive birth
/// weight there, with its coordinates — the same "alive and contributing"
/// predicate `draw`'s own site selection uses (`alive_at` plus `births_at >
/// 0.0`), so the sites this function lists are exactly the sites `draw`
/// could choose from at `year`.
/// type-audit: bare-ok(count: year)
pub fn places(ctx: &LotContext, year: f64) -> Vec<Place> {
    ctx.occupations
        .iter()
        .enumerate()
        .filter(|(o, _)| alive_at(ctx, *o, year) && births_at(ctx, *o, year) > 0.0)
        .map(|(occ, p)| {
            let (latitude, longitude) = ctx.lat_lon(p.record.core.site);
            Place {
                occ,
                entity: p.record.id,
                site: p.record.core.site,
                people: p.record.core.people.0.to_string(),
                name: p.name.clone(),
                latitude,
                longitude,
                population: population_at(&p.shape, year),
                births_per_year: p.births_per_year,
            }
        })
        .collect()
}

/// One occupation's mortality profile: the mechanism-half numbers of spec
/// §4.3, for the exhibit and the readout.
/// type-audit: bare-ok(count: e0), bare-ok(ratio: q_maturity), bare-ok(count: maturity_years), bare-ok(count: lifespan_years), bare-ok(ratio: strife), bare-ok(ratio: infant_share), bare-ok(ratio: background_share), bare-ok(ratio: senescent_share)
#[derive(Clone, Debug, PartialEq)]
pub struct Odds {
    /// Life expectancy at birth.
    pub e0: f64,
    /// Probability of dying before the people's age at maturity.
    pub q_maturity: f64,
    /// The people's age at maturity.
    pub maturity_years: f64,
    /// The people's allometric lifespan.
    pub lifespan_years: f64,
    /// The site's strife, `[0, 1]`.
    pub strife: f64,
    /// The infant term's share of total deaths.
    pub infant_share: f64,
    /// The background term's share of total deaths.
    pub background_share: f64,
    /// The senescent term's share of total deaths.
    pub senescent_share: f64,
    /// Cause-attributed mortality shares.
    pub causes: Vec<CauseOdds>,
}

/// One cause's share of mortality.
/// type-audit: bare-ok(ratio: share)
#[derive(Clone, Debug, PartialEq)]
pub struct CauseOdds {
    /// Cause category.
    pub cause: DeathCause,
    /// Share of all mortality.
    pub share: f64,
}

/// The mortality odds for occupation `occ`.
/// type-audit: bare-ok(index: occ)
pub fn odds(ctx: &LotContext, occ: usize) -> Odds {
    odds_at(ctx, occ, ctx.present_year)
}

/// The mortality odds for occupation `occ` at `year`.
/// type-audit: bare-ok(index: occ), bare-ok(count: year)
pub fn odds_at(ctx: &LotContext, occ: usize, year: f64) -> Odds {
    let p = &ctx.occupations[occ];
    let shares = hazard_shares(&p.hazard);
    let mut causes = Vec::new();
    for (band, band_share) in [
        (HazardBand::Infant, shares.infant),
        (HazardBand::Background, shares.background),
        (HazardBand::Senescent, shares.senescent),
    ] {
        for weight in cause_weights_for_band(ctx, p.record.core.site, year, p.hazard.strife, band) {
            add_cause(&mut causes, weight.cause, band_share * weight.weight);
        }
    }

    let outbreaks = ctx
        .outbreaks_by_occupation
        .get(&p.record.id)
        .map(Vec::as_slice)
        .unwrap_or(&[]);
    let outbreak_deaths: f64 = outbreaks.iter().map(|event| event.deaths).sum();
    let outbreak_share = if p.record.core.person_years > 0.0 {
        (outbreak_deaths / p.record.core.person_years * p.e0).clamp(0.0, 1.0)
    } else {
        0.0
    };
    for cause in &mut causes {
        cause.share *= 1.0 - outbreak_share;
    }
    if outbreak_deaths > 0.0 {
        for event in outbreaks {
            add_cause(
                &mut causes,
                DeathCause::Pathogen(event.pathogen),
                outbreak_share * event.deaths / outbreak_deaths,
            );
        }
    }
    Odds {
        e0: p.e0,
        q_maturity: q_before(&p.hazard, p.maturity_years),
        maturity_years: p.maturity_years,
        lifespan_years: p.lifespan_years,
        strife: p.hazard.strife,
        infant_share: shares.infant,
        background_share: shares.background,
        senescent_share: shares.senescent,
        causes,
    }
}

fn add_cause(causes: &mut Vec<CauseOdds>, cause: DeathCause, share: f64) {
    if let Some(existing) = causes.iter_mut().find(|row| row.cause == cause) {
        existing.share += share;
    } else {
        causes.push(CauseOdds { cause, share });
    }
}
