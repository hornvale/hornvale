//! The four `lot/*/v1` payloads (spec §6.1): `lot/life/v1`, `lot/curve/v1`,
//! `lot/places/v1` and `lot/odds/v1`.
//!
//! **Quantize at emit, and only at emit** (`windows/CLAUDE.md`): every `f64`
//! leaving this module goes through `hornvale_kernel::quantize`, applied by
//! a `serialize_with` attribute on the document field rather than anywhere
//! in the compute path — the shape, the survival integral and `e₀` all run
//! at full precision and are quantized once, here.
//!
//! **Every document struct in this module is private, with private fields.**
//! Only the four `*_json` functions are public, so nothing in these
//! documents is a public primitive needing a `type-audit` verdict, and a
//! field cannot be read back by a consumer who would then depend on its
//! Rust type rather than on the JSON. Key order is declaration order
//! (`serde_json::to_string` over a struct), and the one map is a
//! `BTreeMap`, so a payload is byte-identical run to run.

use std::collections::BTreeMap;

use serde::{Serialize, Serializer};

use crate::context::LotContext;
use crate::draw::{Curve, Ending, Life, Odds, Place};
use crate::shape::Shape;
use crate::slots::{Silence, SlotValue, Source, Story};

/// Serialize a `u64` as its decimal text rather than a JSON number — the
/// `windows/vessel` snapshot's `u64_as_decimal_string`, for the same reason
/// and applied to the same family. JSON has no int64 and JavaScript's
/// `number` is an IEEE-754 double, lossy above 2^53; `EntityId` has been a
/// full-width lineage-derived draw since The Signet, and these payloads are
/// read in a browser (spec §6.4). Measured on seed 42's `lot/places/v1`: an
/// occupation entity of `10760661430244475000`, which `JSON.parse` rounds.
///
/// **Only the entity-id family takes this treatment.** `seed` and `index`
/// stay JSON numbers: both are values the caller supplied and already
/// holds, neither is a hash draw, and the exhibit's permalink carries them
/// as text anyway. The Rust fields stay `u64` in memory; this governs the
/// emit boundary alone. Private, so no `type-audit:` tag of its own.
fn u64_as_decimal_string<S: Serializer>(id: &u64, serializer: S) -> Result<S::Ok, S::Error> {
    serializer.serialize_str(&id.to_string())
}

/// The same, for an `Option<u64>` that is `None` on one of the two source
/// kinds.
fn opt_u64_as_decimal_string<S: Serializer>(
    id: &Option<u64>,
    serializer: S,
) -> Result<S::Ok, S::Error> {
    id.map(|id| id.to_string()).serialize(serializer)
}

// ---------------------------------------------------------------------
// lot/life/v1
// ---------------------------------------------------------------------

/// Which pins the reader applied, echoed so a payload is self-describing.
#[derive(Serialize)]
struct PickDoc {
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::opt_f64_field")]
    year: Option<f64>,
    site: Option<u32>,
}

/// How the life ended, split into a kind and the community's committed
/// cause where there was one.
#[derive(Serialize)]
struct EndingDoc {
    kind: &'static str,
    cause: Option<&'static str>,
}

/// Why a slot has no value — the two kinds kept distinct, because the
/// campaign's headline measurement counts them separately.
#[derive(Serialize)]
struct SilenceDoc<'a> {
    kind: &'static str,
    reason: &'a str,
}

/// One slot: its value, or `null` beside the silence that explains it, and
/// the numbers of the sources it rests on.
#[derive(Serialize)]
struct SlotDoc<'a> {
    key: &'a str,
    value: Option<&'a str>,
    silence: Option<SilenceDoc<'a>>,
    sources: Vec<usize>,
}

/// The story's silence tally — the campaign's headline number, carried on
/// every payload rather than recomputed by each consumer.
#[derive(Serialize)]
struct SilenceCounts {
    filled: usize,
    no_fact: usize,
    by_design: usize,
}

/// One entry in the flat source list. Both source kinds share one shape
/// with `null` in the fields the other kind uses: an untagged union would
/// make a consumer branch on which keys are present, and a payload whose
/// key set varies by row is harder to read, not easier.
#[derive(Serialize)]
struct SourceDoc<'a> {
    number: usize,
    kind: &'static str,
    #[serde(serialize_with = "opt_u64_as_decimal_string")]
    entity: Option<u64>,
    predicate: Option<&'a str>,
    caption: Option<&'a str>,
    function: Option<&'a str>,
    inputs: Option<&'a str>,
}

/// `lot/life/v1`.
#[derive(Serialize)]
struct LifeDoc<'a> {
    schema: &'static str,
    seed: u64,
    index: u64,
    pick: PickDoc,
    occ: usize,
    #[serde(serialize_with = "u64_as_decimal_string")]
    occupation: u64,
    site: u32,
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    birth_year: f64,
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    death_year: f64,
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    age_at_death: f64,
    matured: bool,
    ending: EndingDoc,
    moved_to: Option<usize>,
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::opt_f64_field")]
    moved_year: Option<f64>,
    shape: &'static str,
    slots: Vec<SlotDoc<'a>>,
    silences: SilenceCounts,
    sources: Vec<SourceDoc<'a>>,
}

/// One drawn life and its whole story as `lot/life/v1`.
///
/// The `sources` list is numbered in first-citation order by
/// [`Story::citations`] — the same list, from the same method, that
/// [`crate::narrate`] prints its `[n]` markers from, so a reader can carry a
/// number from the prose straight into the payload.
///
/// Needs no `World`: [`Story`] resolved every caption at tell time.
/// type-audit: bare-ok(artifact: return)
pub fn life_json(ctx: &LotContext, life: &Life, story: &Story) -> String {
    let (sources, per_slot) = story.citations();
    let mut filled = 0usize;
    let mut no_fact = 0usize;
    let mut by_design = 0usize;
    let mut slots = Vec::with_capacity(story.slots.len());
    for (at, slot) in story.slots.iter().enumerate() {
        let (value, silence) = match &slot.value {
            SlotValue::Filled(text) => {
                filled += 1;
                (Some(text.as_str()), None)
            }
            SlotValue::Silent(Silence::NoFact(reason)) => {
                no_fact += 1;
                (
                    None,
                    Some(SilenceDoc {
                        kind: "no-fact",
                        reason: reason.as_str(),
                    }),
                )
            }
            SlotValue::Silent(Silence::ByDesign(reason)) => {
                by_design += 1;
                (
                    None,
                    Some(SilenceDoc {
                        kind: "by-design",
                        reason,
                    }),
                )
            }
        };
        slots.push(SlotDoc {
            key: slot.key,
            value,
            silence,
            sources: per_slot[at].clone(),
        });
    }
    let doc = LifeDoc {
        schema: "lot/life/v1",
        seed: ctx.seed,
        index: life.index,
        pick: PickDoc {
            year: life.pick.year,
            site: life.pick.site.map(|site| site.0),
        },
        occ: life.occ,
        occupation: life.occupation.get(),
        site: life.site.0,
        birth_year: life.birth_year,
        death_year: life.death_year,
        age_at_death: life.age_at_death,
        matured: life.matured,
        ending: ending_doc(&life.ending),
        moved_to: life.moved_to,
        moved_year: life.moved_year,
        shape: shape_name(&ctx.occupations[life.occ].shape),
        slots,
        silences: SilenceCounts {
            filled,
            no_fact,
            by_design,
        },
        sources: sources
            .iter()
            .enumerate()
            .map(|(at, source)| source_doc(at + 1, source))
            .collect(),
    };
    encode(&doc)
}

/// The document form of one ending.
fn ending_doc(ending: &Ending) -> EndingDoc {
    match ending {
        Ending::Alive => EndingDoc {
            kind: "alive",
            cause: None,
        },
        Ending::Hazard => EndingDoc {
            kind: "hazard",
            cause: None,
        },
        Ending::CommunityFate(cause) => EndingDoc {
            kind: "community-fate",
            cause: Some(cause_name(*cause)),
        },
    }
}

/// The committed cause of an ending, as the payload spells it.
fn cause_name(cause: hornvale_history::record::CauseOfEnd) -> &'static str {
    use hornvale_history::record::CauseOfEnd;
    match cause {
        CauseOfEnd::Famine => "famine",
        CauseOfEnd::Burned => "burned",
        CauseOfEnd::Plague => "plague",
        CauseOfEnd::Fled => "fled",
        CauseOfEnd::Migrated => "migrated",
        CauseOfEnd::Breached => "breached",
    }
}

/// Which reconstruction the occupation's population curve used (spec §4.1).
/// The lot says which shape it interpolated with, rather than presenting one
/// curve as if the record held it.
fn shape_name(shape: &Shape) -> &'static str {
    match shape {
        Shape::RisePlateau { .. } => "rise-plateau",
        Shape::Triangle { .. } => "triangle",
        Shape::Rectangle { .. } => "rectangle",
    }
}

/// The document form of one source, at its assigned number.
fn source_doc(number: usize, source: &Source) -> SourceDoc<'_> {
    match source {
        Source::Fact {
            entity,
            predicate,
            caption,
        } => SourceDoc {
            number,
            kind: "fact",
            entity: Some(*entity),
            predicate: Some(predicate.as_str()),
            caption: Some(caption.as_str()),
            function: None,
            inputs: None,
        },
        Source::Derived { function, inputs } => SourceDoc {
            number,
            kind: "derived",
            entity: None,
            predicate: None,
            caption: None,
            function: Some(function),
            inputs: Some(inputs.as_str()),
        },
    }
}

// ---------------------------------------------------------------------
// lot/curve/v1
// ---------------------------------------------------------------------

/// `lot/curve/v1`.
#[derive(Serialize)]
struct CurveDoc<'a> {
    schema: &'static str,
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    epoch_years: f64,
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    start_year: f64,
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    present_year: f64,
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    births_by_epoch: &'a [f64],
    #[serde(serialize_with = "quantized_series_map")]
    births_by_people: &'a BTreeMap<String, Vec<f64>>,
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    souls_ever: f64,
}

/// The births-per-epoch curve as `lot/curve/v1`.
///
/// `births_by_people` is the `Curve`'s own `births_by_people_by_epoch`,
/// under the name the payload uses; every series in it is binned on the same
/// `epoch_years` as `births_by_epoch` and runs from `start_year`.
/// type-audit: bare-ok(artifact: return)
pub fn curve_json(curve: &Curve) -> String {
    encode(&CurveDoc {
        schema: "lot/curve/v1",
        epoch_years: curve.epoch_years,
        start_year: curve.start_year,
        present_year: curve.present_year,
        births_by_epoch: &curve.births_by_epoch,
        births_by_people: &curve.births_by_people_by_epoch,
        souls_ever: curve.souls_ever,
    })
}

/// Quantize every series in a per-people map on the way out. The map is a
/// `BTreeMap`, so key order is the peoples' own label order, deterministically.
fn quantized_series_map<S: Serializer>(
    series: &BTreeMap<String, Vec<f64>>,
    serializer: S,
) -> Result<S::Ok, S::Error> {
    let quantized: BTreeMap<&str, Vec<f64>> = series
        .iter()
        .map(|(people, counts)| {
            (
                people.as_str(),
                counts
                    .iter()
                    .copied()
                    .map(hornvale_kernel::quantize::quantize)
                    .collect(),
            )
        })
        .collect();
    quantized.serialize(serializer)
}

// ---------------------------------------------------------------------
// lot/places/v1
// ---------------------------------------------------------------------

/// One occupation alive at the asked year.
#[derive(Serialize)]
struct PlaceDoc<'a> {
    occ: usize,
    #[serde(serialize_with = "u64_as_decimal_string")]
    entity: u64,
    site: u32,
    people: &'a str,
    name: Option<&'a str>,
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    latitude: f64,
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    longitude: f64,
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    population: f64,
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    births_per_year: f64,
}

/// `lot/places/v1`.
#[derive(Serialize)]
struct PlacesDoc<'a> {
    schema: &'static str,
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    year: f64,
    places: Vec<PlaceDoc<'a>>,
}

/// The occupations alive at `year` as `lot/places/v1`.
///
/// **`year` is the caller's own argument, echoed back, and that is the
/// point.** `crate::draw::places` samples every `population` at one year and
/// returns places that carry none, so a consumer holding two of these
/// documents cannot tell them apart from their contents. The exhibit (spec
/// §6.4) caches a payload per year as the reader scrubs the timeline; a
/// document that can only be identified by the bookkeeping around it is one
/// mislabelled cache entry away from showing the wrong year's map under the
/// right year's heading. Pass the same `year` given to `places`.
/// type-audit: bare-ok(count: year), bare-ok(artifact: return)
pub fn places_json(year: f64, places: &[Place]) -> String {
    encode(&PlacesDoc {
        schema: "lot/places/v1",
        year,
        places: places
            .iter()
            .map(|place| PlaceDoc {
                occ: place.occ,
                entity: place.entity.get(),
                site: place.site.0,
                people: place.people.as_str(),
                name: place.name.as_deref(),
                latitude: place.latitude,
                longitude: place.longitude,
                population: place.population,
                births_per_year: place.births_per_year,
            })
            .collect(),
    })
}

// ---------------------------------------------------------------------
// lot/odds/v1
// ---------------------------------------------------------------------

/// `lot/odds/v1`.
#[derive(Serialize)]
struct OddsDoc {
    schema: &'static str,
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    e0: f64,
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    q_maturity: f64,
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    maturity_years: f64,
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    lifespan_years: f64,
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    strife: f64,
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    infant_share: f64,
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    background_share: f64,
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    senescent_share: f64,
}

/// One occupation's mortality profile as `lot/odds/v1`.
/// type-audit: bare-ok(artifact: return)
pub fn odds_json(odds: &Odds) -> String {
    encode(&OddsDoc {
        schema: "lot/odds/v1",
        e0: odds.e0,
        q_maturity: odds.q_maturity,
        maturity_years: odds.maturity_years,
        lifespan_years: odds.lifespan_years,
        strife: odds.strife,
        infant_share: odds.infant_share,
        background_share: odds.background_share,
        senescent_share: odds.senescent_share,
    })
}

/// One encoder for all four payloads: compact (never `to_string_pretty` —
/// whitespace is not a schema and a pretty payload is a larger artifact
/// with no more information), and infallible for these document types,
/// which hold no map with a non-string key and no non-finite float that
/// `quantize` has not already passed through.
fn encode<T: Serialize>(doc: &T) -> String {
    serde_json::to_string(doc).expect("a lot payload has no unserializable field")
}
