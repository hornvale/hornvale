//! Derived endemic burden and mortality-cause attribution.

use hornvale_kernel::{KindId, Vertex};

use crate::context::LotContext;

/// Background attribution weight for the flux before density adjustment.
/// plumb: universal(The Murrain spec section 3 endemic attribution)
const FLUX_BACKGROUND: f64 = 0.35;
/// Infant attribution weight for the flux.
/// plumb: universal(The Murrain spec section 3 endemic attribution)
const FLUX_INFANT: f64 = 0.55;
/// Background attribution weight for the consumption.
/// plumb: universal(The Murrain spec section 3 endemic attribution)
const CONSUMPTION_BACKGROUND: f64 = 0.25;
/// Background attribution weight for marsh fever at unit fit.
/// plumb: universal(The Murrain spec section 3 endemic attribution)
const MARSH_BACKGROUND: f64 = 0.40;
/// Infant attribution weight for marsh fever at unit fit.
/// plumb: universal(The Murrain spec section 3 endemic attribution)
const MARSH_INFANT: f64 = 0.25;
/// Unnamed background residual.
/// plumb: universal(The Murrain spec section 3 endemic attribution)
const UNNAMED_BACKGROUND: f64 = 0.30;
/// Unnamed infant residual.
/// plumb: universal(The Murrain spec section 3 endemic attribution)
const UNNAMED_INFANT: f64 = 0.20;

/// Endemic attribution weights at one place and time.
/// type-audit: bare-ok(ratio: background_weight), bare-ok(ratio: infant_weight)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Burden {
    /// Weight among background-hazard causes.
    pub background_weight: f64,
    /// Weight among infant-hazard causes.
    pub infant_weight: f64,
}

/// The Siler term being attributed.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HazardBand {
    /// The falling early-life term.
    Infant,
    /// The constant background term.
    Background,
    /// The rising late-life term.
    Senescent,
}

/// One normalized cause weight.
/// type-audit: bare-ok(ratio: weight)
#[derive(Clone, Debug, PartialEq)]
pub struct CauseWeight {
    /// The attributed cause.
    pub cause: crate::draw::DeathCause,
    /// Its probability within this band.
    pub weight: f64,
}

/// Derive endemic burdens at a site and year.
/// type-audit: bare-ok(count: year)
pub fn endemic_burden_at(ctx: &LotContext, site: Vertex, year: f64) -> Vec<(KindId, Burden)> {
    let era = era_index(ctx, year);
    let era_start = ctx.era_substrates[era].0;
    let population = ctx.era_population.population_at(era_start, site);
    let metapopulation = ctx.metapopulation_by_era[era]
        .1
        .get(&site)
        .copied()
        .unwrap_or(0.0);
    let registry = hornvale_species::pathogen_registry();
    registry
        .iter()
        .filter_map(|(&kind, traits)| match traits.class {
            hornvale_species::PathogenClass::Environmental => Some((
                kind,
                Burden {
                    background_weight: FLUX_BACKGROUND * (0.5 + 0.5 * (population / 50.0).min(1.0)),
                    infant_weight: FLUX_INFANT,
                },
            )),
            hornvale_species::PathogenClass::Chronic => {
                let ccs = hornvale_epidemiology::critical_community_size(
                    traits.r0.expect("chronic pathogen has R0"),
                    traits
                        .infectious_years
                        .expect("chronic pathogen has an infectious period"),
                    1.0 / 30.0,
                );
                hornvale_epidemiology::persists(metapopulation, ccs).then_some((
                    kind,
                    Burden {
                        background_weight: CONSUMPTION_BACKGROUND,
                        infant_weight: 0.0,
                    },
                ))
            }
            hornvale_species::PathogenClass::Vector => {
                let fit = hornvale_worldgen::tolerance_liebig(
                    &traits.condition_niche,
                    ctx.era_substrates[era].1.get(site),
                    0.0,
                );
                Some((
                    kind,
                    Burden {
                        background_weight: MARSH_BACKGROUND * fit,
                        infant_weight: MARSH_INFANT * fit,
                    },
                ))
            }
            hornvale_species::PathogenClass::Zoonotic | hornvale_species::PathogenClass::Crowd => {
                None
            }
        })
        .collect()
}

/// Derive normalized causes within one Siler band.
/// type-audit: bare-ok(count: year), bare-ok(ratio: strife)
pub fn cause_weights_for_band(
    ctx: &LotContext,
    site: Vertex,
    year: f64,
    strife: f64,
    band: HazardBand,
) -> Vec<CauseWeight> {
    let burdens = endemic_burden_at(ctx, site, year);
    match band {
        HazardBand::Senescent => vec![CauseWeight {
            cause: crate::draw::DeathCause::Age,
            weight: 1.0,
        }],
        HazardBand::Infant => {
            let mut weights: Vec<_> = burdens
                .into_iter()
                .filter(|(_, burden)| burden.infant_weight > 0.0)
                .map(|(kind, burden)| CauseWeight {
                    cause: crate::draw::DeathCause::Pathogen(kind),
                    weight: burden.infant_weight,
                })
                .collect();
            weights.push(CauseWeight {
                cause: crate::draw::DeathCause::Unnamed,
                weight: UNNAMED_INFANT,
            });
            normalize(&mut weights);
            weights
        }
        HazardBand::Background => {
            let violence = strife.clamp(0.0, 1.0) / (1.0 + strife.clamp(0.0, 1.0));
            let mut weights: Vec<_> = burdens
                .into_iter()
                .filter(|(_, burden)| burden.background_weight > 0.0)
                .map(|(kind, burden)| CauseWeight {
                    cause: crate::draw::DeathCause::Pathogen(kind),
                    weight: burden.background_weight,
                })
                .collect();
            weights.push(CauseWeight {
                cause: crate::draw::DeathCause::Unnamed,
                weight: UNNAMED_BACKGROUND,
            });
            normalize(&mut weights);
            for weight in &mut weights {
                weight.weight *= 1.0 - violence;
            }
            weights.push(CauseWeight {
                cause: crate::draw::DeathCause::Violence,
                weight: violence,
            });
            weights
        }
    }
}

fn era_index(ctx: &LotContext, year: f64) -> usize {
    ctx.era_substrates
        .iter()
        .enumerate()
        .take_while(|(_, (era_start, _))| *era_start <= year)
        .map(|(index, _)| index)
        .last()
        .unwrap_or(0)
}

fn normalize(weights: &mut [CauseWeight]) {
    let total: f64 = weights.iter().map(|weight| weight.weight).sum();
    if total > 0.0 {
        for weight in weights {
            weight.weight /= total;
        }
    }
}
