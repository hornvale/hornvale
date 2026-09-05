//! The draw (spec §3, §4.4): pure hash expansion of (seed, index, label)
//! into a birth year, a site, and a life course under the site's hazard and
//! the community's committed fate. Nothing here draws a `Stream`.

use std::collections::BTreeMap;

use hornvale_history::record::CauseOfEnd;
use hornvale_kernel::{EntityId, Vertex};

use crate::context::LotContext;
use crate::hazard::{death_age, hazard_shares, q_before};
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
    /// Died in the community's own committed ending.
    CommunityFate(CauseOfEnd),
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
    let mut age = death_age(&p.hazard, 1.0 - uniform(seed, i, "death"));
    let mut ending = Ending::Hazard;
    let mut moved_to = None;
    let mut moved_year = None;
    let mut cur = occ;
    loop {
        let r = &ctx.occupations[cur].record;
        let Some(end) = r.core.ended else { break };
        if end <= birth_year || end >= birth_year + age {
            break;
        }
        let cause = r.core.cause.unwrap_or(CauseOfEnd::Fled);
        if uniform(seed, i, &format!("fate-{}", r.id.0)) < loss_fraction(cause) {
            age = end - birth_year;
            ending = Ending::CommunityFate(cause);
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
            }
            None => {
                age = end - birth_year;
                ending = Ending::CommunityFate(cause);
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
        moved_to,
        moved_year,
        pick: *pick,
    })
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
/// type-audit: bare-ok(count: epoch_years), bare-ok(count: births_by_epoch), bare-ok(count: souls_ever), bare-ok(count: births_by_people_by_epoch)
#[derive(Clone, Debug, PartialEq)]
pub struct Curve {
    /// The epoch length the curve is binned in (`shape::EPOCH_YEARS`).
    pub epoch_years: f64,
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
#[derive(Clone, Copy, Debug, PartialEq)]
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
}

/// The mortality odds for occupation `occ`.
/// type-audit: bare-ok(index: occ)
pub fn odds(ctx: &LotContext, occ: usize) -> Odds {
    let p = &ctx.occupations[occ];
    let shares = hazard_shares(&p.hazard);
    Odds {
        e0: p.e0,
        q_maturity: q_before(&p.hazard, p.maturity_years),
        maturity_years: p.maturity_years,
        lifespan_years: p.lifespan_years,
        strife: p.hazard.strife,
        infant_share: shares.infant,
        background_share: shares.background,
        senescent_share: shares.senescent,
    }
}
