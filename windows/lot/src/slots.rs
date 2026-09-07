//! The story's slots (spec §5): every question the story asks of the
//! ledger, in a fixed order, each answered with a sourced value or an
//! honest silence.
//!
//! **The rule the whole campaign rests on: nothing here invents a value.** A
//! slot resolves to one of exactly three things —
//!
//! - [`SlotValue::Filled`] with at least one [`Source`]: either a
//!   `(entity, predicate)` fact read, captioned with that predicate's own
//!   registry doc (the `windows/historiography::recount` idiom), or a
//!   [`Source::Derived`] naming the function and its committed inputs;
//! - [`Silence::NoFact`]: the ledger was asked and had no answer. This is
//!   the measurement — the coverage readout counts it as a silence **the
//!   world has**;
//! - [`Silence::ByDesign`]: one of the four the spec excludes (§4.4),
//!   counted as a silence **the lens declined to introduce**.
//!
//! Nothing here draws a `Stream` (the one derived name goes through
//! `hornvale_language::Namer`, which is a pure function of `(seed, species,
//! kind, salt)`), commits a fact, or reads anything but the committed
//! ledger and what [`crate::context`] already rebuilt from the seed.
//!
//! [`Story`] is deliberately SELF-CONTAINED: every caption is resolved at
//! tell time, so a narrator or a JSON encoder downstream never needs the
//! `World` again.

use hornvale_astronomy::{EclipseBody, EclipseSight, StdInstant};
use hornvale_history::record::{CauseOfEnd, Ended, Founding, Function, TechHorizon};
use hornvale_kernel::{EntityId, Value, Vertex, World};

use crate::context::LotContext;
use crate::draw::{Ending, Life, uniform};
use crate::shape::population_at;

/// Where an answer came from. Every [`SlotValue::Filled`] carries at least
/// one.
/// type-audit: bare-ok(index: Fact.entity), bare-ok(identifier-text: Fact.predicate), bare-ok(prose: Fact.caption), bare-ok(identifier-text: Derived.function), bare-ok(prose: Derived.inputs)
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Source {
    /// One committed fact, read off the ledger.
    Fact {
        /// The fact's subject.
        entity: u64,
        /// The predicate read.
        predicate: String,
        /// That predicate's own registry doc — the provenance caption, not
        /// a bibliography entry. Falls back to the predicate name when the
        /// registry does not know it.
        caption: String,
    },
    /// A pure read over committed inputs — allowed by `windows/CLAUDE.md`
    /// ("a derived read over an existing field consumes nothing").
    Derived {
        /// The function that did the deriving, named as the code names it.
        function: &'static str,
        /// What it was given, in prose.
        inputs: String,
    },
}

/// Why a slot has no value.
/// type-audit: bare-ok(prose: NoFact.0), bare-ok(prose: ByDesign.0)
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Silence {
    /// The ledger was asked and had no answer. A silence the WORLD has.
    NoFact(String),
    /// Excluded by spec §4.4 — no model exists to answer it, and inventing
    /// one is the thing this instrument is built not to do. A silence the
    /// LENS declined to introduce.
    ByDesign(&'static str),
}

/// A slot's answer.
/// type-audit: bare-ok(prose: Filled.0)
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SlotValue {
    /// An answer, rendered as prose.
    Filled(String),
    /// No answer, and why.
    Silent(Silence),
}

/// One named question, its answer, and where the answer came from.
/// type-audit: bare-ok(identifier-text: key)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Slot {
    /// The question's name, as spec §5 names it.
    pub key: &'static str,
    /// The answer, or the silence.
    pub value: SlotValue,
    /// Every source the answer rests on. Empty exactly when the value is a
    /// silence with nothing to cite — a `Filled` slot with no source is a
    /// fabricated answer, and the suite fails on one.
    pub sources: Vec<Source>,
}

/// One life's whole story: every slot of spec §5, in the story's order.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Story {
    /// The slots, in the order the story tells them.
    pub slots: Vec<Slot>,
}

impl Story {
    /// The slot with this key, if the story carries one.
    /// type-audit: bare-ok(identifier-text: key)
    pub fn slot(&self, key: &str) -> Option<&Slot> {
        self.slots.iter().find(|slot| slot.key == key)
    }

    /// The story's sources, deduplicated in first-citation order, together
    /// with each slot's own 1-based citation numbers into that list — in
    /// `slots` order, one entry per slot.
    ///
    /// **There is exactly one numbering, and it lives here.** The prose's
    /// `[n]` markers ([`crate::narrate`]) and the payload's `sources` array
    /// ([`crate::json`]) are the same list because both read it from this
    /// method; a second traversal in either renderer is how a reference and
    /// its entry drift apart. It is a first-citation ordering over
    /// [`Story::slots`], which is the order the prose tells them in —
    /// `narrate::STAGES` is a partition of that same sequence into
    /// contiguous runs, pinned by a test, so the two orders cannot diverge.
    /// type-audit: bare-ok(index: return)
    pub fn citations(&self) -> (Vec<&Source>, Vec<Vec<usize>>) {
        let mut flat: Vec<&Source> = Vec::new();
        let mut per_slot: Vec<Vec<usize>> = Vec::with_capacity(self.slots.len());
        for slot in &self.slots {
            let mut numbers: Vec<usize> = Vec::with_capacity(slot.sources.len());
            for source in &slot.sources {
                let at = match flat.iter().position(|held| *held == source) {
                    Some(at) => at,
                    None => {
                        flat.push(source);
                        flat.len() - 1
                    }
                };
                if !numbers.contains(&(at + 1)) {
                    numbers.push(at + 1);
                }
            }
            per_slot.push(numbers);
        }
        (flat, per_slot)
    }
}

/// A `(value, sources)` pair, the shape every resolver returns.
type Answer = (SlotValue, Vec<Source>);

/// A filled answer with one derived source.
fn derived(text: String, function: &'static str, inputs: &str) -> Answer {
    (
        SlotValue::Filled(text),
        vec![Source::Derived {
            function,
            inputs: inputs.to_string(),
        }],
    )
}

/// A silence the world has.
fn no_fact(reason: &str) -> Answer {
    (
        SlotValue::Silent(Silence::NoFact(reason.to_string())),
        Vec::new(),
    )
}

/// A silence the lens declined to introduce.
fn by_design(reason: &'static str) -> Answer {
    (SlotValue::Silent(Silence::ByDesign(reason)), Vec::new())
}

#[derive(Clone, Copy)]
struct SocialBoundary {
    person: EntityId,
    from: f64,
    until: f64,
}

fn social_boundary(world: &World, ctx: &LotContext, life: &Life) -> Option<SocialBoundary> {
    let person = ctx.social_person_for_occupation(life.occupation)?;
    let from = hornvale_worldgen::ledger_day_of_bake_year(life.birth_year);
    let until = hornvale_worldgen::ledger_day_of_bake_year(life.death_year);
    let founded = world
        .ledger
        .facts_of(person, hornvale_person::PERSON_FOUNDED)
        .any(|fact| {
            matches!(fact.object, Value::Entity(occupation) if occupation == life.occupation)
                && fact
                    .day
                    .map(|day| day.as_std_days() < until)
                    .unwrap_or(false)
        });
    if !founded {
        return None;
    }
    let born_after_life = world
        .ledger
        .facts_of(person, hornvale_person::PERSON_BORN)
        .any(|fact| {
            fact.day
                .map(|day| day.as_std_days() >= until)
                .unwrap_or(false)
        });
    let died_before_life = world
        .ledger
        .facts_of(person, hornvale_person::PERSON_DIED)
        .any(|fact| {
            fact.day
                .map(|day| day.as_std_days() <= from)
                .unwrap_or(false)
        });
    if born_after_life || died_before_life {
        return None;
    }
    Some(SocialBoundary {
        person,
        from,
        until,
    })
}

fn object_text(value: &Value) -> String {
    match value {
        Value::Text(text) => text.clone(),
        Value::Entity(entity) => format!("entity {}", entity.get()),
        Value::Number(number) => number.to_string(),
        Value::Flag(flag) => flag.to_string(),
    }
}

fn person_axis(
    world: &World,
    ctx: &LotContext,
    life: &Life,
    predicate: &str,
    ended: &str,
) -> Answer {
    let Some(boundary) = social_boundary(world, ctx, life) else {
        return no_fact("no realized person is committed for this life");
    };
    let facts: Vec<&hornvale_kernel::Fact> = world
        .ledger
        .facts_of(boundary.person, predicate)
        .filter(|fact| active_fact_with_end(world, fact, &boundary, ended))
        .collect();
    if facts.is_empty() {
        return no_fact("no committed source answers this social question");
    }
    let text = facts
        .iter()
        .map(|fact| object_text(&fact.object))
        .collect::<Vec<_>>()
        .join(", ");
    (
        SlotValue::Filled(text),
        facts
            .iter()
            .map(|fact| cite(world, fact.subject, predicate))
            .collect(),
    )
}

fn active_fact_with_end(
    world: &World,
    fact: &hornvale_kernel::Fact,
    boundary: &SocialBoundary,
    ended: &str,
) -> bool {
    let Some(day) = fact.day.map(|day| day.as_std_days()) else {
        return false;
    };
    if day >= boundary.until {
        return false;
    }
    !world.ledger.facts_about(fact.subject).any(|candidate| {
        candidate.predicate == ended
            && candidate.object == fact.object
            && candidate
                .day
                .map(|end| end.as_std_days() <= boundary.from)
                .unwrap_or(false)
    })
}

fn relation_facts<'a>(
    world: &'a World,
    boundary: &SocialBoundary,
    predicate: &str,
    source_is_person: bool,
) -> Vec<&'a hornvale_kernel::Fact> {
    world
        .ledger
        .find(predicate)
        .filter(|fact| {
            let directed = if source_is_person {
                fact.subject == boundary.person
            } else {
                matches!(fact.object, Value::Entity(entity) if entity == boundary.person)
            };
            directed && active_fact_with_end(world, fact, boundary, &format!("{predicate}-ended"))
        })
        .collect()
}

fn siblings(world: &World, ctx: &LotContext, life: &Life) -> Answer {
    let Some(boundary) = social_boundary(world, ctx, life) else {
        return no_fact("no realized person is committed for this life");
    };
    let parents: Vec<&hornvale_kernel::Fact> = relation_facts(world, &boundary, "descent", false);
    let mut sources = parents
        .iter()
        .map(|fact| cite(world, fact.subject, "descent"))
        .collect::<Vec<_>>();
    let mut count = 0usize;
    for parent in parents.iter().map(|fact| fact.subject) {
        for fact in relation_facts(
            world,
            &SocialBoundary {
                person: parent,
                ..boundary
            },
            "descent",
            true,
        ) {
            if fact.object != Value::Entity(boundary.person) {
                count += 1;
                sources.push(cite(world, fact.subject, "descent"));
            }
        }
    }
    if count == 0 {
        return no_fact("no committed shared-descent source answers siblinghood");
    }
    (
        SlotValue::Filled(format!(
            "{count} sibling relation(s) derived from shared descent"
        )),
        sources,
    )
}

fn parental_death(world: &World, ctx: &LotContext, life: &Life) -> Answer {
    let Some(boundary) = social_boundary(world, ctx, life) else {
        return no_fact("no realized person is committed for this life");
    };
    let parents = relation_facts(world, &boundary, "descent", false);
    let mut sources = Vec::new();
    for parent in parents {
        if let Some(death) = world
            .ledger
            .facts_of(parent.subject, hornvale_person::PERSON_DIED)
            .find(|fact| {
                fact.day
                    .map(|day| {
                        day.as_std_days() >= boundary.from && day.as_std_days() < boundary.until
                    })
                    .unwrap_or(false)
            })
        {
            sources.push(cite(world, parent.subject, "descent"));
            sources.push(cite(world, death.subject, hornvale_person::PERSON_DIED));
        }
    }
    if sources.is_empty() {
        return no_fact("no committed parental-death source answers this question");
    }
    (
        SlotValue::Filled("a parental death is recorded".to_string()),
        sources,
    )
}

fn directed_relation(
    world: &World,
    ctx: &LotContext,
    life: &Life,
    predicate: &str,
    source: bool,
    empty: &str,
) -> Answer {
    let Some(boundary) = social_boundary(world, ctx, life) else {
        return no_fact("no realized person is committed for this life");
    };
    let facts = relation_facts(world, &boundary, predicate, source);
    if facts.is_empty() {
        return no_fact(empty);
    }
    (
        SlotValue::Filled(format!("{} recorded {predicate} relation(s)", facts.len())),
        facts
            .iter()
            .map(|fact| cite(world, fact.subject, predicate))
            .collect(),
    )
}

fn recognized_association(world: &World, ctx: &LotContext, life: &Life) -> Answer {
    let Some(boundary) = social_boundary(world, ctx, life) else {
        return no_fact("no realized person is committed for this life");
    };
    let associations = relation_facts(world, &boundary, "association", true);
    let recognitions = relation_facts(world, &boundary, "recognition", false);
    let pairs: Vec<(&hornvale_kernel::Fact, &hornvale_kernel::Fact)> = associations
        .iter()
        .flat_map(|association| {
            recognitions.iter().filter_map(move |recognition| {
                (association.day < recognition.day
            && matches!(recognition.object, Value::Entity(entity) if entity == boundary.person))
            .then_some((*association, *recognition))
            })
        })
        .collect();
    if pairs.is_empty() {
        return no_fact("no explicit recognized association source answers this question");
    }
    let mut sources = Vec::new();
    for (a, b) in pairs {
        sources.push(cite(world, a.subject, "association"));
        if world
            .ledger
            .facts_of(a.subject, hornvale_history::ASSOCIATION_FORM)
            .any(|fact| {
                fact.object != Value::Flag(false)
                    && active_fact_with_end(world, fact, &boundary, "association-form-ended")
            })
        {
            sources.push(cite(world, a.subject, hornvale_history::ASSOCIATION_FORM));
        }
        sources.push(cite(world, b.subject, "recognition"));
        if world
            .ledger
            .facts_of(b.subject, hornvale_history::RECOGNITION_INTERPRETATION)
            .any(|fact| {
                active_fact_with_end(world, fact, &boundary, "recognition-interpretation-ended")
            })
        {
            sources.push(cite(
                world,
                b.subject,
                hornvale_history::RECOGNITION_INTERPRETATION,
            ));
        }
    }
    (
        SlotValue::Filled("an explicitly recognized association is recorded".to_string()),
        sources,
    )
}

fn paired_relation(
    world: &World,
    ctx: &LotContext,
    life: &Life,
    first: &str,
    second: &str,
    empty: &str,
) -> Answer {
    let Some(boundary) = social_boundary(world, ctx, life) else {
        return no_fact("no realized person is committed for this life");
    };
    let first_facts = relation_facts(world, &boundary, first, true);
    let second_facts = relation_facts(world, &boundary, second, true);
    let pairs: Vec<(&hornvale_kernel::Fact, &hornvale_kernel::Fact)> = first_facts
        .iter()
        .flat_map(|a| {
            second_facts
                .iter()
                .filter_map(move |b| (a.object == b.object).then_some((*a, *b)))
        })
        .collect();
    if pairs.is_empty() {
        return no_fact(empty);
    }
    let mut sources = Vec::new();
    for (a, b) in pairs {
        sources.push(cite(world, a.subject, first));
        sources.push(cite(world, b.subject, second));
    }
    (
        SlotValue::Filled(format!("a {first}/{second} relation is recorded")),
        sources,
    )
}

fn inheritance(world: &World, ctx: &LotContext, life: &Life) -> Answer {
    let Some(boundary) = social_boundary(world, ctx, life) else {
        return no_fact("no realized person is committed for this life");
    };
    let transfers = relation_facts(world, &boundary, "transfer", false);
    let mut sources = Vec::new();
    for transfer in transfers {
        if let Some(death) = world
            .ledger
            .facts_of(transfer.subject, hornvale_person::PERSON_DIED)
            .find(|death| {
                death
                    .day
                    .map(|day| {
                        let day = day.as_std_days();
                        day >= boundary.from && day < boundary.until
                    })
                    .unwrap_or(false)
            })
        {
            sources.push(cite(world, transfer.subject, "transfer"));
            sources.push(cite(world, death.subject, hornvale_person::PERSON_DIED));
        }
    }
    if sources.is_empty() {
        return no_fact("no deceased-source transfer answers inheritance");
    }
    (
        SlotValue::Filled("an inheritance transfer is recorded".to_string()),
        sources,
    )
}

/// A residence fact is not migration by itself. The Lot recognizes migration
/// only when the same person has a closed residence interval followed by a
/// later residence at a different target within the life boundary.
fn migration(world: &World, ctx: &LotContext, life: &Life) -> Answer {
    let Some(boundary) = social_boundary(world, ctx, life) else {
        return no_fact("no realized person is committed for this life");
    };
    let residences = relation_facts(world, &boundary, "residence", true);
    let mut sources = Vec::new();
    let mut transitions = 0usize;
    for prior in &residences {
        let Some(prior_end) = world.ledger.facts_about(prior.subject).find(|fact| {
            fact.predicate == "residence-ended"
                && fact.object == prior.object
                && fact
                    .day
                    .map(|day| {
                        let day = day.as_std_days();
                        day > boundary.from && day < boundary.until
                    })
                    .unwrap_or(false)
        }) else {
            continue;
        };
        let Some(prior_end_day) = prior_end.day.map(|day| day.as_std_days()) else {
            continue;
        };
        if residences.iter().any(|later| {
            later.object != prior.object
                && later
                    .day
                    .map(|day| {
                        day.as_std_days() >= prior_end_day && day.as_std_days() < boundary.until
                    })
                    .unwrap_or(false)
        }) {
            transitions += 1;
            sources.push(cite(world, prior.subject, "residence"));
            sources.push(cite(world, prior.subject, "residence-ended"));
        }
    }
    if transitions == 0 {
        return no_fact("no committed residence transition answers migration");
    }
    (
        SlotValue::Filled(format!("{transitions} committed residence transition(s)")),
        sources,
    )
}

/// One `(entity, predicate)` citation, captioned with that predicate's own
/// registry doc — the `windows/historiography::recount` idiom, resolved
/// here so [`Story`] never needs the `World` again.
fn cite(world: &World, entity: EntityId, predicate: &str) -> Source {
    Source::Fact {
        entity: entity.get(),
        predicate: predicate.to_string(),
        caption: world
            .registry
            .predicate(predicate)
            .map(|def| def.doc.clone())
            .unwrap_or_else(|| predicate.to_string()),
    }
}

/// A year rendered the way the story says one: a whole number, since the
/// bake's own resolution is the epoch and a fractional year is an artifact
/// of the draw, not a fact about the world.
fn year(value: f64) -> String {
    format!("{}", value.round() as i64)
}

/// A year, rounded to the whole number the story displays.
fn year_i64(value: f64) -> i64 {
    value.round() as i64
}

/// An age derived from two DISPLAYED years, never from a third float
/// rounded on its own: `when`'s "born … dead … aged" triple and `sky`'s
/// "the first at age N" both name a span between two years the sentence
/// already prints, so the age must be their difference — rounding all
/// three floats independently can disagree (the gallery's seed-42 page
/// disagreed on 1 of 9 dead lives before this fix).
fn rounded_span(start: f64, end: f64) -> i64 {
    year_i64(end) - year_i64(start)
}

/// The settlement standing on `vertex` that this lot's `people` occupies,
/// falling back to the first settlement there.
///
/// A vertex may hold more than one settlement — successive peoples at one
/// site — so the people is the discriminator and commit order is only the
/// tie-break. `None` when nothing ever condensed a settlement on the
/// vertex, which is the ordinary case for a long-dead occupation.
fn settlement_on(
    ctx: &LotContext,
    world: &World,
    vertex: Vertex,
    people: &str,
) -> Option<EntityId> {
    let here = ctx.settlements_by_vertex.get(&vertex)?;
    here.iter()
        .find(|id| hornvale_species::species_of(world, **id).as_deref() == Some(people))
        .or_else(|| here.first())
        .copied()
}

/// The name of the settlement standing on `vertex`, together with the
/// citation for the `name` fact that produced it.
///
/// Returning the two as a pair is the whole point: a name reaches the
/// rendered sentence at five separate sites (the birth site, a daughter
/// site, a mother community, a remembered forebear, a tribute patron), and
/// four of them once rendered the name while citing nothing. Handing back
/// the [`Source`] with the string makes forgetting it a compile error
/// rather than a quiet false provenance.
fn named_settlement(
    ctx: &LotContext,
    world: &World,
    vertex: Vertex,
    people: &str,
) -> Option<(String, Source)> {
    let id = settlement_on(ctx, world, vertex, people)?;
    let name = world.ledger.text_of(id, hornvale_kernel::NAME)?.to_string();
    Some((name, cite(world, id, hornvale_kernel::NAME)))
}

/// The coordinates of `vertex` in degrees, with the source that produced
/// them: the settlement's own committed `latitude`/`longitude` facts where
/// both stand there, otherwise the derivation off the rebuilt Geosphere.
///
/// A committed fact beats a derivation when it exists, and the choice is
/// what fixes the citation: the previous version displayed the DERIVED
/// numbers while citing the two facts on the strength of their mere
/// existence, which named a read that never happened. Both facts are
/// required together — a half-committed pair would otherwise mix a fact
/// with a derivation inside one coordinate and cite both for both.
fn coordinates_of(
    ctx: &LotContext,
    world: &World,
    vertex: Vertex,
    settlement: Option<EntityId>,
) -> (f64, f64, Vec<Source>) {
    if let Some(id) = settlement
        && let Some(Value::Number(latitude)) =
            world.ledger.value_of(id, hornvale_settlement::LATITUDE)
        && let Some(Value::Number(longitude)) =
            world.ledger.value_of(id, hornvale_settlement::LONGITUDE)
    {
        return (
            *latitude,
            *longitude,
            vec![
                cite(world, id, hornvale_settlement::LATITUDE),
                cite(world, id, hornvale_settlement::LONGITUDE),
            ],
        );
    }
    let (latitude, longitude) = ctx.lat_lon(vertex);
    (
        latitude,
        longitude,
        vec![Source::Derived {
            function: "lot::context::LotContext::lat_lon",
            inputs: "the rebuilt Geosphere's position for the committed site".to_string(),
        }],
    )
}

/// The latitude of `vertex` in degrees, with the citation for JUST that
/// value: the settlement's own committed `latitude` fact where it stands,
/// otherwise the derivation off the rebuilt Geosphere. Unlike
/// [`coordinates_of`], this never cites a `longitude` fact — `climate` is
/// the caller, and it never displays the longitude, so citing it alongside
/// would claim a read of a value the sentence does not show.
fn latitude_of(
    ctx: &LotContext,
    world: &World,
    vertex: Vertex,
    settlement: Option<EntityId>,
) -> (f64, Source) {
    if let Some(id) = settlement
        && let Some(Value::Number(latitude)) =
            world.ledger.value_of(id, hornvale_settlement::LATITUDE)
    {
        return (*latitude, cite(world, id, hornvale_settlement::LATITUDE));
    }
    let (latitude, _longitude) = ctx.lat_lon(vertex);
    (
        latitude,
        Source::Derived {
            function: "lot::context::LotContext::lat_lon",
            inputs: "the rebuilt Geosphere's position for the committed site".to_string(),
        },
    )
}

/// The birth occupation's people, as the record names it.
fn people_of(ctx: &LotContext, life: &Life) -> &'static str {
    ctx.occupations[life.occ].record.core.people.0
}

// ---------------------------------------------------------------------
// The resolvers, one per slot, in the story's order.
// ---------------------------------------------------------------------

/// `when` — the drawn birth year and how the life sat against the present.
fn when(world: &World, ctx: &LotContext, life: &Life) -> Answer {
    let record = &ctx.occupations[life.occ].record;
    let mut sources = vec![
        cite(world, record.id, hornvale_history::OCC_FOUNDED),
        Source::Derived {
            function: "lot::draw::draw",
            inputs: "the world's births-per-year curve (§4.2) at the lot's index".to_string(),
        },
    ];
    if record.core.ended.is_some() {
        sources.push(cite(world, record.id, hornvale_history::OCC_ENDED));
    }
    if let Some(now) = world.ledger.find(hornvale_history::HISTORY_NOW).next() {
        sources.push(cite(world, now.subject, hornvale_history::HISTORY_NOW));
    }
    let text = match life.ending {
        Ending::Alive => format!(
            "born in year {}, and alive at the present, year {} — {} years old",
            year(life.birth_year),
            year(ctx.present_year),
            rounded_span(life.birth_year, ctx.present_year)
        ),
        _ => format!(
            "born in year {}, dead in year {}, aged {}",
            year(life.birth_year),
            year(life.death_year),
            rounded_span(life.birth_year, life.death_year)
        ),
    };
    (SlotValue::Filled(text), sources)
}

/// `where` — the birth site: its settlement's name, coordinates and biome
/// where one stands there; coordinates alone where none does.
///
/// Always Filled, even with an unknown biome: the vertex and its
/// coordinates are derivable for any site, so reporting the whole slot
/// silent would hide two answers the ledger genuinely has. The missing
/// half is said inside the value and cited as absent.
fn site_slot(world: &World, ctx: &LotContext, life: &Life) -> Answer {
    let record = &ctx.occupations[life.occ].record;
    let people = people_of(ctx, life);
    let mut sources = vec![cite(world, record.id, hornvale_history::OCC_SITE)];
    let settlement = settlement_on(ctx, world, life.site, people);
    let (latitude, longitude, coordinate_sources) =
        coordinates_of(ctx, world, life.site, settlement);
    sources.extend(coordinate_sources);
    let mut biome: Option<String> = None;
    let mut named: Option<String> = None;
    if let Some(id) = settlement {
        sources.push(cite(world, id, hornvale_settlement::VERTEX_ID));
        if let Some((name, source)) = named_settlement(ctx, world, life.site, people) {
            named = Some(name);
            sources.push(source);
        }
        if let Some(text) = world.ledger.text_of(id, hornvale_settlement::BIOME) {
            biome = Some(text.to_string());
            sources.push(cite(world, id, hornvale_settlement::BIOME));
        }
    }
    // No settlement, or one carrying no biome: any place entity standing on
    // the same vertex answers it instead.
    if biome.is_none() {
        for place in hornvale_terrain::places(world) {
            let on_vertex = matches!(
                world.ledger.value_of(place.id, hornvale_settlement::VERTEX_ID),
                Some(Value::Number(n)) if Vertex(*n as u32) == life.site
            );
            if on_vertex {
                biome = Some(place.biome.clone());
                sources.push(cite(world, place.id, hornvale_terrain::BIOME));
                break;
            }
        }
    }
    let ground = match &biome {
        Some(kind) => format!("a {kind} site"),
        None => {
            // The absence is a source in its own right: the caption says
            // which question was asked and came back empty.
            sources.push(Source::Derived {
                function: "lot::slots::site_slot",
                inputs: "no `biome` fact stands on this vertex — nothing in the record says \
                         what kind of country it was"
                    .to_string(),
            });
            "a site of a kind the record does not say".to_string()
        }
    };
    let place = match &named {
        Some(name) => format!("{name}, {ground}"),
        None => format!("{ground} nobody names now"),
    };
    let text = if let (Some(moved), Some(when_moved)) = (life.moved_to, life.moved_year) {
        let daughter = &ctx.occupations[moved].record;
        let daughter_settlement = settlement_on(ctx, world, daughter.core.site, people);
        let (daughter_lat, daughter_lon, daughter_coordinate_sources) =
            coordinates_of(ctx, world, daughter.core.site, daughter_settlement);
        sources.push(cite(world, daughter.id, hornvale_history::OCC_SITE));
        sources.extend(daughter_coordinate_sources);
        let onward = match named_settlement(ctx, world, daughter.core.site, people) {
            Some((name, source)) => {
                sources.push(source);
                format!("{name} ({daughter_lat:.1}°, {daughter_lon:.1}°)")
            }
            None => format!("a site at {daughter_lat:.1}°, {daughter_lon:.1}°"),
        };
        format!(
            "{place}, at {latitude:.1}°, {longitude:.1}° — and, from year {}, {onward}",
            year(when_moved)
        )
    } else {
        format!("{place}, at {latitude:.1}°, {longitude:.1}°")
    };
    (SlotValue::Filled(text), sources)
}

/// `people` — which kind the life was born to.
fn people_slot(world: &World, ctx: &LotContext, life: &Life) -> Answer {
    let record = &ctx.occupations[life.occ].record;
    (
        SlotValue::Filled(people_of(ctx, life).to_string()),
        vec![cite(world, record.id, hornvale_history::OCC_PEOPLE)],
    )
}

/// `name` — a given name drawn from the people's own phonology, salted by
/// the lot's index.
///
/// The draw is `hornvale_language::Namer::name`, a pure function of
/// `(seed, species, kind, salt)` — it consumes nothing and reseeds nothing.
/// A people with no society vector has no naming tradition to derive a
/// morphology from, and the honest answer is that no name survives.
fn name_slot(world: &World, ctx: &LotContext, life: &Life) -> Answer {
    let people = people_of(ctx, life);
    let (Some(mind), Some(society)) = (
        ctx.components.psyche.get_by_label(people),
        ctx.components.society.get_by_label(people),
    ) else {
        return no_fact("no naming tradition is derivable for this people");
    };
    let phonology = hornvale_worldgen::language_of_in(world, &ctx.components, people);
    let namer = hornvale_language::Namer::new(&world.seed, people, &phonology);
    let salt = (uniform(ctx.seed, life.index, "name") * (1u64 << 53) as f64) as u64;
    let drawn = namer.name(
        hornvale_language::NameKind::Person,
        salt,
        &hornvale_worldgen::morph_options(mind, society),
    );
    derived(
        drawn.roman,
        "language::Namer::name",
        "the people's phonology; a lot-index salt",
    )
}

/// `community-size` — how many stood there at the birth year, against the
/// community's committed peak.
fn community_size(world: &World, ctx: &LotContext, life: &Life) -> Answer {
    let prepared = &ctx.occupations[life.occ];
    let at_birth = population_at(&prepared.shape, life.birth_year).round() as i64;
    let text = format!(
        "about {at_birth} people at the birth year, in a community that reached {} at its height",
        prepared.record.core.peak_population
    );
    (
        SlotValue::Filled(text),
        vec![
            cite(world, prepared.record.id, hornvale_history::OCC_PEAK),
            cite(
                world,
                prepared.record.id,
                hornvale_history::OCC_PERSON_YEARS,
            ),
            Source::Derived {
                function: "lot::shape::population_at",
                inputs: "the committed span, peak and person-years (§4.1)".to_string(),
            },
        ],
    )
}

/// `founded-from` — the mother community and the years between, or the
/// world's own first year.
fn founded_from(world: &World, ctx: &LotContext, life: &Life) -> Answer {
    let record = &ctx.occupations[life.occ].record;
    let mut sources = vec![
        cite(world, record.id, hornvale_history::OCC_FOUNDED),
        cite(world, record.id, hornvale_history::OCC_FOUNDED_FROM),
    ];
    let text = match record.founded_from {
        Founding::Genesis(_) => format!(
            "founded at year {}, raised from nothing at its own site",
            year(record.core.founded)
        ),
        Founding::From(mother_id) => {
            let Some(&mother_index) = ctx.by_entity.get(&mother_id) else {
                return no_fact(
                    "the mother community named by `occ-founded-from` is not in this world's records",
                );
            };
            let mother = &ctx.occupations[mother_index].record;
            sources.push(cite(world, mother.id, hornvale_history::OCC_FOUNDED));
            let gap = record.core.founded - mother.core.founded;
            let named = match named_settlement(ctx, world, mother.core.site, mother.core.people.0) {
                Some((name, source)) => {
                    sources.push(source);
                    name
                }
                None => format!("an unnamed community at site {}", mother.core.site.0),
            };
            format!(
                "founded at year {} by settlers from {named}, itself {} years older",
                year(record.core.founded),
                year(gap)
            )
        }
    };
    (SlotValue::Filled(text), sources)
}

/// `founder-kinship` — how the community's founder stood to their mother
/// community's founder.
fn founder_kinship(world: &World, ctx: &LotContext, life: &Life) -> Answer {
    let record = &ctx.occupations[life.occ].record;
    let Some((_handle, kinship)) = hornvale_worldgen::forebear_of(world, record.id) else {
        return no_fact(
            "no forebear is derivable — either this community was raised at its own site, \
             or its people has no generation length to reckon a remove by",
        );
    };
    let text = match kinship {
        hornvale_history::descent::Kinship::Sibling => {
            "its founder stood in the same generation as the mother community's founder".to_string()
        }
        hornvale_history::descent::Kinship::Ancestor(remove) => format!(
            "its founder descended {remove} generation{} from the mother community's founder",
            if remove == 1 { "" } else { "s" }
        ),
    };
    (
        SlotValue::Filled(text),
        vec![
            cite(world, record.id, hornvale_history::OCC_FOUNDED_FROM),
            Source::Derived {
                function: "worldgen::descent::forebear_of",
                inputs: "the two foundings and the people's allometric generation length"
                    .to_string(),
            },
        ],
    )
}

/// `community-fate` — what became of the community the life was born to.
/// Always Filled: either a witnessed ending, or that it was still standing.
fn community_fate(world: &World, ctx: &LotContext, life: &Life) -> Answer {
    let record = &ctx.occupations[life.occ].record;
    let mut sources = vec![Source::Derived {
        function: "lot::draw::draw",
        inputs: "the community's committed fate, spliced into the life course (§4.3)".to_string(),
    }];
    if record.core.ended.is_some() {
        sources.push(cite(world, record.id, hornvale_history::OCC_ENDED));
    }
    // `OCC_CAUSE`/`OCC_ENDED_BY` are cited only inside the `CommunityFate`
    // arm below, and only when the sentence actually names what they say —
    // the final review found both pushed here unconditionally, citing a
    // committed cause/attacker even on the `Alive`/`Hazard` arms that never
    // mention either.
    let text = match &life.ending {
        Ending::CommunityFate(cause) => {
            sources.push(cite(world, record.id, hornvale_history::OCC_CAUSE));
            let mut said = format!(
                "the community {} in year {}, and the life ended with it",
                cause_phrase(*cause),
                year(life.death_year)
            );
            if let Ended::By(hand) = record.ended_by
                && let Some(&index) = ctx.by_entity.get(&hand)
            {
                sources.push(cite(world, record.id, hornvale_history::OCC_ENDED_BY));
                let attacker = &ctx.occupations[index].record;
                said.push_str(&format!(
                    " — at the hand of a {} community at site {}",
                    attacker.core.people.0, attacker.core.site.0
                ));
            }
            said
        }
        Ending::Alive => match record.core.ended {
            Some(ended) => format!(
                "the community had already ended, in year {}, before the present the life is \
                 alive at",
                year(ended)
            ),
            None => "the community was still standing at the present".to_string(),
        },
        Ending::Hazard => match record.core.ended {
            Some(ended) if ended > life.death_year => format!(
                "the community outlived the life, ending only in year {}",
                year(ended)
            ),
            Some(ended) => format!(
                "the community ended in year {}, after the life had already ended",
                year(ended)
            ),
            None => "the community was still standing when the life ended".to_string(),
        },
    };
    let text = match (life.moved_to, life.moved_year) {
        (Some(moved), Some(when_moved)) => {
            let daughter = &ctx.occupations[moved].record;
            sources.push(cite(world, daughter.id, hornvale_history::OCC_FOUNDED_FROM));
            format!(
                "{text}; the survivors refounded at site {} in year {}, and the life went with \
                 them",
                daughter.core.site.0,
                year(when_moved)
            )
        }
        _ => text,
    };
    (SlotValue::Filled(text), sources)
}

/// The committed cause of an ending, as a clause.
fn cause_phrase(cause: CauseOfEnd) -> &'static str {
    match cause {
        CauseOfEnd::Famine => "starved out",
        CauseOfEnd::Burned => "was put to the torch",
        CauseOfEnd::Plague => "was emptied by disease",
        CauseOfEnd::Fled => "fled its site",
        CauseOfEnd::Migrated => "migrated onward",
        CauseOfEnd::Breached => "broke through into something, and the delving ended there",
    }
}

/// `tech` — the community's technological horizon.
fn tech(world: &World, ctx: &LotContext, life: &Life) -> Answer {
    let record = &ctx.occupations[life.occ].record;
    let text = match record.core.tech {
        TechHorizon::Neolithic => "stone tools, and no metal",
        TechHorizon::Bronze => "bronze-working",
        TechHorizon::Iron => "iron-working",
        TechHorizon::Classical => "classical statecraft and engineering",
    };
    (
        SlotValue::Filled(text.to_string()),
        vec![cite(world, record.id, hornvale_history::OCC_TECH)],
    )
}

/// `function` — what the community was for.
fn function(world: &World, ctx: &LotContext, life: &Life) -> Answer {
    let record = &ctx.occupations[life.occ].record;
    let text = match record.core.function {
        Function::Agrarian => "farming and herding",
        Function::Mine => "extraction — ore, stone, salt",
        Function::Trade => "a waypoint on a trade route",
        Function::Cult => "a shrine seat",
        Function::Fort => "a garrisoned defensive point",
    };
    (
        SlotValue::Filled(text.to_string()),
        vec![cite(world, record.id, hornvale_history::OCC_FUNCTION)],
    )
}

/// `tongue` — the people's derived language.
///
/// `Occupation.tongue` is never committed and is always `None` (campaign
/// ledger #8), so the answer is the derived one: every speaking people has
/// a phonology drawn from its own authored articulation, and a family label
/// where it shares one with kin.
fn tongue(ctx: &LotContext, life: &Life) -> Answer {
    let people = people_of(ctx, life);
    if ctx.components.articulation.get_by_label(people).is_none() {
        return no_fact("this people has no articulation, and so no tongue to derive");
    }
    let family = ctx.components.family_of.get_by_label(people).copied();
    let text = match family {
        Some(label) if label != people => {
            format!("the {people} tongue, of the {label} family")
        }
        _ => format!("the {people} tongue"),
    };
    derived(
        text,
        "worldgen::language_of_in",
        "the people's authored articulation vector, drawn against the world's seed",
    )
}

/// `belief` — the pantheon the life's people held.
///
/// Beliefs are held by a people's FLAGSHIP settlement, not by every
/// community of that people, so this names the people's pantheon and says
/// so; it is not a claim that this particular community kept the cult.
fn belief(world: &World, ctx: &LotContext, life: &Life) -> Answer {
    let people = people_of(ctx, life);
    let Some(flagship) = hornvale_worldgen::flagship_of(world, people) else {
        return no_fact("this people has no flagship settlement, and so no committed pantheon");
    };
    let held = hornvale_religion::beliefs_held_by(world, flagship.id);
    let Some(chosen) = held
        .iter()
        .find(|belief| belief.high_god)
        .or_else(|| held.first())
    else {
        return no_fact("nothing in the record names a deity this people held");
    };
    let cult = world
        .ledger
        .text_of(chosen.id, hornvale_religion::CULT_FORM)
        .map(str::to_string);
    let mut text = format!("{} {}", chosen.deity, chosen.epithet);
    if chosen.high_god {
        text.push_str(", who presides");
    }
    text.push_str(&format!(
        " — the pantheon this people held, kept at {}",
        flagship.name
    ));
    if let Some(form) = &cult {
        text.push_str(&format!(", in {form} cult"));
    }
    let mut sources = vec![
        cite(world, chosen.id, hornvale_religion::DEITY_NAME),
        cite(world, chosen.id, hornvale_religion::DEITY_EPITHET),
        cite(world, chosen.id, hornvale_religion::HELD_BY),
    ];
    if cult.is_some() {
        sources.push(cite(world, chosen.id, hornvale_religion::CULT_FORM));
    }
    (SlotValue::Filled(text), sources)
}

/// `held-true` — what the community held to be true about its own kin's
/// endings, as hearsay derives it (never what happened: what was HELD).
fn held_true(world: &World, ctx: &LotContext, life: &Life) -> Answer {
    let record = &ctx.occupations[life.occ].record;
    // The birth occupation's own ancestry (which includes itself) is the
    // set of subjects it could hold a telling about.
    for subject in ctx.lineage.ancestry(record.id) {
        let claims = hornvale_hearsay::derive::claims_about(
            &world.ledger,
            &ctx.lineage,
            subject,
            hornvale_history::OCC_ENDED,
        );
        let Some(claim) = claims.iter().find(|claim| claim.holder == record.id) else {
            continue;
        };
        let Value::Number(day) = &claim.object else {
            continue;
        };
        let ended = hornvale_worldgen::bake_year_of_ledger_day(*day);
        let mut sources = vec![
            cite(world, subject, hornvale_history::OCC_ENDED),
            Source::Derived {
                function: "hearsay::claims_about",
                inputs: "the founding tree, walked from every witness of the ending".to_string(),
            },
        ];
        let subject_name = match ctx
            .by_entity
            .get(&subject)
            .map(|index| &ctx.occupations[*index].record)
        {
            Some(other) => {
                match named_settlement(ctx, world, other.core.site, other.core.people.0) {
                    Some((name, source)) => {
                        sources.push(source);
                        name
                    }
                    None => format!("the community at site {}", other.core.site.0),
                }
            }
            None => format!("entity {}", subject.get()),
        };
        let telling = if claim.hops == 0 {
            "first-hand".to_string()
        } else {
            format!(
                "{} telling{} removed",
                claim.hops,
                if claim.hops == 1 { "" } else { "s" }
            )
        };
        let text = format!(
            "the community held that {subject_name} came to its end in year {} — {telling}",
            year(ended)
        );
        return (SlotValue::Filled(text), sources);
    }
    no_fact(
        "this community remembers no ending — none of its forebears ended within reach of a telling",
    )
}

/// `subsistence` — how the community fed itself.
///
/// Culture facts attach to LIVING settlements, so a dead occupation has
/// none. That is the silence spec §8's H-P4 predicts, and it is a fact
/// about the ledger's reach, not about the community.
fn subsistence(world: &World, ctx: &LotContext, life: &Life) -> Answer {
    let record = &ctx.occupations[life.occ].record;
    if !record.is_alive() {
        return no_fact("culture facts attach to living settlements");
    }
    let Some(settlement) = settlement_on(ctx, world, life.site, people_of(ctx, life)) else {
        return no_fact("culture facts attach to living settlements");
    };
    let Some(mode) = world
        .ledger
        .text_of(settlement, hornvale_culture::SUBSISTENCE)
    else {
        return no_fact("no `subsistence` fact stands on this settlement");
    };
    (
        SlotValue::Filled(mode.to_string()),
        vec![cite(world, settlement, hornvale_culture::SUBSISTENCE)],
    )
}

/// `standing` — the castes the community reckoned itself in.
///
/// `has-caste` is non-functional, so every committed value is collected,
/// not just the latest.
fn standing(world: &World, ctx: &LotContext, life: &Life) -> Answer {
    let record = &ctx.occupations[life.occ].record;
    if !record.is_alive() {
        return no_fact("culture facts attach to living settlements");
    }
    let Some(settlement) = settlement_on(ctx, world, life.site, people_of(ctx, life)) else {
        return no_fact("culture facts attach to living settlements");
    };
    let castes: Vec<String> = world
        .ledger
        .facts_of(settlement, hornvale_culture::HAS_CASTE)
        .filter_map(|fact| match &fact.object {
            Value::Text(text) => Some(text.clone()),
            _ => None,
        })
        .collect();
    if castes.is_empty() {
        return no_fact("no `has-caste` fact stands on this settlement");
    }
    (
        SlotValue::Filled(castes.join(", ")),
        vec![cite(world, settlement, hornvale_culture::HAS_CASTE)],
    )
}

/// `tribute` — who the community answered to.
fn tribute(world: &World, ctx: &LotContext, life: &Life) -> Answer {
    let record = &ctx.occupations[life.occ].record;
    let Some(Value::Entity(patron)) = world
        .ledger
        .value_of(record.id, hornvale_history::PAYS_TRIBUTE_TO)
    else {
        // Names the question that came back empty, the shape every other
        // `no_fact` reason here uses ("no `subsistence` fact stands on this
        // settlement"). The reason it replaced -- "paid tribute to no one
        // the record names" -- restated the narrator's own question rather
        // than answering it, so the rendered sentence said the same thing
        // twice.
        return no_fact("no `pays-tribute-to` fact stands on this occupation");
    };
    let Some(&index) = ctx.by_entity.get(patron) else {
        return no_fact("the patron named by `pays-tribute-to` is not in this world's records");
    };
    let overlord = &ctx.occupations[index].record;
    let mut sources = vec![cite(world, record.id, hornvale_history::PAYS_TRIBUTE_TO)];
    let named = match named_settlement(ctx, world, overlord.core.site, overlord.core.people.0) {
        Some((name, source)) => {
            sources.push(source);
            name
        }
        None => format!("an unnamed community at site {}", overlord.core.site.0),
    };
    (
        SlotValue::Filled(format!(
            "paid tribute to {named}, a {} community",
            overlord.core.people.0
        )),
        sources,
    )
}

/// `dwelling` — what the community's residential core was built of.
fn dwelling(world: &World, ctx: &LotContext, life: &Life) -> Answer {
    let record = &ctx.occupations[life.occ].record;
    let seed = hornvale_history::flesh::flesh_seed_for(&world.seed, &record.core);
    let structures = hornvale_history::flesh::structures_of(record, seed);
    let residential = structures
        .iter()
        .filter(|structure| {
            matches!(
                structure,
                hornvale_history::flesh::Structure::Hut
                    | hornvale_history::flesh::Structure::Longhouse
            )
        })
        .count();
    let Some(kind) = structures.iter().find(|structure| {
        matches!(
            structure,
            hornvale_history::flesh::Structure::Hut | hornvale_history::flesh::Structure::Longhouse
        )
    }) else {
        return no_fact("the community's structures include no residential core");
    };
    let word = match kind {
        hornvale_history::flesh::Structure::Longhouse => "longhouse",
        _ => "hut",
    };
    let text = if residential == 1 {
        format!("one {word}")
    } else {
        format!("{residential} {word}s")
    };
    derived(
        text,
        "history::flesh::structures_of",
        "the committed peak population, tech horizon and function, under the \
         occupation-scoped flesh seed",
    )
}

/// `mine` — how far a working was driven, for a community that dug.
fn mine(world: &World, ctx: &LotContext, life: &Life) -> Answer {
    let record = &ctx.occupations[life.occ].record;
    if record.core.function != Function::Mine {
        return no_fact("not a mining community");
    }
    (
        SlotValue::Filled(format!(
            "a working driven {:.0} m below its own seat",
            record.core.delve_depth_m
        )),
        vec![cite(world, record.id, hornvale_history::OCC_DELVE_DEPTH)],
    )
}

/// `climate` — the site's biome and the band its latitude puts it in.
///
/// The five climate KINDS are phenomena, not facts (campaign ledger #8), so
/// nothing here reads one: the answer is the committed biome plus a band
/// derived from the vertex's own latitude.
fn climate(world: &World, ctx: &LotContext, life: &Life) -> Answer {
    let settlement = settlement_on(ctx, world, life.site, people_of(ctx, life));
    // The same latitude `where` displays, from the same chooser, so the band
    // and the number a reader sees beside it can never disagree. Only the
    // latitude source: this slot never displays a longitude, so it must
    // never cite one either.
    let (latitude, lat_source) = latitude_of(ctx, world, life.site, settlement);
    let mut sources = vec![lat_source];
    let band = if latitude.abs() < 23.5 {
        "tropical"
    } else if latitude.abs() < 66.5 {
        "temperate"
    } else {
        "polar"
    };
    sources.push(Source::Derived {
        function: "lot::slots::climate",
        inputs: "the site's own latitude, banded at 23.5° and 66.5°".to_string(),
    });
    // Bound to the settlement id here rather than read through it and
    // `expect`ed back afterwards: the biome and its citation must come from
    // one and the same entity, and a second lookup is how they drift.
    let text = match settlement.and_then(|id| {
        world
            .ledger
            .text_of(id, hornvale_settlement::BIOME)
            .map(|kind| (id, kind))
    }) {
        Some((id, kind)) => {
            let phrase = format!("{band} latitudes, and {kind} country");
            sources.push(cite(world, id, hornvale_settlement::BIOME));
            phrase
        }
        None => format!("{band} latitudes; nothing in the record names the country"),
    };
    (SlotValue::Filled(text), sources)
}

/// `sky` — the total solar eclipses visible from the site inside the life.
///
/// The window is CLAMPED to the life's own span: an unclamped one asks the
/// syzygy scan a question about all of history, and a count over all of
/// history is not something anybody saw.
fn sky(ctx: &LotContext, life: &Life) -> Answer {
    let (system, calendar) = &ctx.sky;
    let from = hornvale_worldgen::ledger_day_of_bake_year(life.birth_year);
    let until = hornvale_worldgen::ledger_day_of_bake_year(life.death_year);
    let (Ok(from), Ok(until)) = (StdInstant::new(from), StdInstant::new(until)) else {
        return no_fact("the life's span does not convert to an instant on the world's time axis");
    };
    let (latitude, longitude) = ctx.lat_lon(life.site);
    let seen: Vec<f64> = hornvale_astronomy::eclipse_events(system, calendar, from, until)
        .into_iter()
        .filter(|event| event.body == EclipseBody::Solar)
        .filter(|event| {
            hornvale_astronomy::solar_eclipse_sight(system, calendar, event, latitude, longitude)
                == EclipseSight::WholeSun
        })
        .map(|event| hornvale_worldgen::bake_year_of_ledger_day(event.day.get()))
        .collect();
    let text = match seen.first() {
        None => "the sun was never wholly taken in this life".to_string(),
        Some(first) => {
            let age = rounded_span(life.birth_year, *first).max(0);
            format!(
                "the sun was wholly taken {} time{} over this life, the first at age {}",
                seen.len(),
                if seen.len() == 1 { "" } else { "s" },
                age
            )
        }
    };
    derived(
        text,
        "astronomy::eclipse_events",
        "the world's own star system and calendar, over the life's span, at the site's \
         latitude and longitude",
    )
}

/// `ground` — what the ground did under the site inside the life.
///
/// The window is CLAMPED to the life's span, for the reason
/// `worldgen::hazard::events_in`'s own doc names: nothing there caps a
/// window, so an unclamped one iterates until it looks hung.
fn ground(ctx: &LotContext, life: &Life) -> Answer {
    let from = hornvale_worldgen::ledger_day_of_bake_year(life.birth_year);
    let until = hornvale_worldgen::ledger_day_of_bake_year(life.death_year);
    let (Ok(from), Ok(until)) = (
        hornvale_kernel::WorldTime::from_std_days(from),
        hornvale_kernel::WorldTime::from_std_days(until),
    ) else {
        return no_fact("the life's span does not convert to a tick window");
    };
    let events = hornvale_worldgen::hazard::events_in(
        hornvale_kernel::Seed(ctx.seed),
        &ctx.terrain,
        life.site,
        (from, until),
    );
    let quakes = events
        .iter()
        .filter(|event| event.kind == hornvale_worldgen::hazard::HazardEventKind::Seismic)
        .count();
    let eruptions = events
        .iter()
        .filter(|event| event.kind == hornvale_worldgen::hazard::HazardEventKind::Eruption)
        .count();
    let text = match (quakes, eruptions) {
        (0, 0) => "nothing — the ground held for the whole of this life".to_string(),
        (q, 0) => format!("{q} earthquake{}", if q == 1 { "" } else { "s" }),
        (0, e) => format!("{e} eruption{}", if e == 1 { "" } else { "s" }),
        (q, e) => format!(
            "{q} earthquake{} and {e} eruption{}",
            if q == 1 { "" } else { "s" },
            if e == 1 { "" } else { "s" }
        ),
    };
    derived(
        text,
        "worldgen::hazard::events_in",
        "the site's recurrence, over the life's span alone",
    )
}

/// `diet` — where the people got its energy, at the species level.
fn diet(ctx: &LotContext, life: &Life) -> Answer {
    let people = people_of(ctx, life);
    let Some(body) = ctx.components.biosphere.get_by_label(people) else {
        return no_fact("this people has no body in the world's roster");
    };
    let text = match body.trophic_mode {
        hornvale_species::TrophicMode::Heterotrophic => {
            "ate other living things — prey, detritus, or their remains"
        }
        hornvale_species::TrophicMode::Phototrophic => "took its energy from light",
        hornvale_species::TrophicMode::Chemotrophic => {
            "took its energy from chemical gradients in rock and water"
        }
        hornvale_species::TrophicMode::Absent => "ate nothing at all — it had no metabolism",
    };
    derived(
        text.to_string(),
        "species::BiosphereTraits::trophic_mode",
        "the people's authored trophic mode",
    )
}

/// Ask every slot of spec §5, in the story's order, of one drawn life.
///
/// Takes `world` explicitly because [`LotContext`] deliberately holds no
/// borrow of one — and resolves every caption here, so the returned
/// [`Story`] is self-contained: a narrator or a JSON encoder downstream
/// never needs the world again.
pub fn tell(world: &World, ctx: &LotContext, life: &Life) -> Story {
    let asked: Vec<(&'static str, Answer)> = vec![
        ("when", when(world, ctx, life)),
        ("where", site_slot(world, ctx, life)),
        ("people", people_slot(world, ctx, life)),
        ("name", name_slot(world, ctx, life)),
        ("community-size", community_size(world, ctx, life)),
        ("founded-from", founded_from(world, ctx, life)),
        ("founder-kinship", founder_kinship(world, ctx, life)),
        ("community-fate", community_fate(world, ctx, life)),
        ("tech", tech(world, ctx, life)),
        ("function", function(world, ctx, life)),
        ("tongue", tongue(ctx, life)),
        ("belief", belief(world, ctx, life)),
        ("held-true", held_true(world, ctx, life)),
        ("subsistence", subsistence(world, ctx, life)),
        ("standing", standing(world, ctx, life)),
        ("tribute", tribute(world, ctx, life)),
        ("dwelling", dwelling(world, ctx, life)),
        ("mine", mine(world, ctx, life)),
        ("climate", climate(world, ctx, life)),
        ("sky", sky(ctx, life)),
        ("ground", ground(ctx, life)),
        ("diet", diet(ctx, life)),
        (
            "sex",
            person_axis(
                world,
                ctx,
                life,
                hornvale_person::SEX_TRAIT,
                hornvale_person::SEX_TRAIT_ENDED,
            ),
        ),
        (
            "reproductive-role",
            person_axis(
                world,
                ctx,
                life,
                hornvale_person::REPRODUCTIVE_ROLE,
                hornvale_person::REPRODUCTIVE_ROLE_ENDED,
            ),
        ),
        (
            "gender-identity",
            person_axis(
                world,
                ctx,
                life,
                hornvale_person::GENDER_IDENTITY,
                hornvale_person::GENDER_IDENTITY_ENDED,
            ),
        ),
        (
            "gender-recognition",
            person_axis(
                world,
                ctx,
                life,
                hornvale_person::GENDER_RECOGNITION,
                hornvale_person::GENDER_RECOGNITION_ENDED,
            ),
        ),
        (
            "family",
            directed_relation(
                world,
                ctx,
                life,
                "descent",
                false,
                "no committed parent relation answers family relation",
            ),
        ),
        ("associations", recognized_association(world, ctx, life)),
        (
            "children",
            directed_relation(
                world,
                ctx,
                life,
                "descent",
                true,
                "no committed child relation answers this question",
            ),
        ),
        ("siblings", siblings(world, ctx, life)),
        (
            "descent",
            directed_relation(
                world,
                ctx,
                life,
                "descent",
                true,
                "no committed descent source answers this question",
            ),
        ),
        (
            "adoption",
            paired_relation(
                world,
                ctx,
                life,
                "care",
                "custody",
                "no committed care-and-custody source answers adoption",
            ),
        ),
        (
            "care",
            directed_relation(
                world,
                ctx,
                life,
                "care",
                true,
                "no committed directed care source answers this question",
            ),
        ),
        (
            "group-membership",
            directed_relation(
                world,
                ctx,
                life,
                "membership",
                true,
                "no committed group-membership source answers this question",
            ),
        ),
        ("migration", migration(world, ctx, life)),
        ("parental-death", parental_death(world, ctx, life)),
        ("inheritance", inheritance(world, ctx, life)),
        (
            "work",
            by_design(
                "no occupation is modelled beyond the community's own subsistence mode (spec §4.4)",
            ),
        ),
        (
            "literacy",
            by_design("nothing in this world models literacy, income or height (spec §4.4)"),
        ),
    ];
    Story {
        slots: asked
            .into_iter()
            .map(|(key, (value, sources))| Slot {
                key,
                value,
                sources,
            })
            .collect(),
    }
}
