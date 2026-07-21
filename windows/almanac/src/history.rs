//! The legibility surface: read a site's whole deep history back out of the
//! ledger and render it as prose a person can *read* — the stratigraphy of
//! occupation layers stacked on one cell (each layer's people, span, tech,
//! function, cause of end, and the ★ threads that tie one layer to the next:
//! who ended it, and where its founders came from) plus the derived flesh
//! that lies in the present-day grass (the structures the last community
//! raised, and the physical residue it left behind — a child's doll in an
//! abandoned clearing).
//!
//! Everything here is a **present-as-query**: nothing replays the deep-history
//! bake. The occupation facts (`is-occupation`, `occ-*`, `is-ruin`) are read
//! straight off the committed ledger; the flesh (`residue_of`/`structures_of`)
//! is *derived* on demand — never committed — so this window can invent as
//! much texture as it likes without touching a save-format contract.
//!
//! Determinism: the layers are ordered by founding day (`f64::total_cmp`, a
//! total order with a stable entity-id tie-break); the flesh seed is derived
//! purely from the world seed and the occupation entity id. Same world ⇒ same
//! prose, byte for byte.

use hornvale_history::flesh::{
    Durability, Residue, ResidueItem, Structure, residue_of, structures_of,
};
use hornvale_history::record::{
    CauseOfEnd, Ended, Founding, Function, Notability, OccupationRecord, TechHorizon,
};
use hornvale_kernel::{CellId, EntityId, KindId, Seed, Value, World};

/// Render a site's stratigraphy stack plus a derived flesh sample, as prose.
/// A "site" is one Geosphere cell; its stratigraphy is every occupation that
/// ever sat on it (alive or ruined), deepest/oldest layer first. If the cell
/// never held an occupation, a single line says so.
/// type-audit: bare-ok(artifact: return)
pub fn render_site(world: &World, site: CellId) -> String {
    let layers = layers_at(world, site);
    if layers.is_empty() {
        return format!(
            "The clearing at cell {}\n{}\n\nNothing ever settled here. The ground \
             keeps no memory of a people.\n",
            site.0,
            "=".repeat(23)
        );
    }

    let now = present_day(world);
    let mut out = String::new();
    let header = format!("The clearing at cell {}", site.0);
    out.push_str(&header);
    out.push('\n');
    out.push_str(&"=".repeat(header.chars().count()));
    out.push_str("\n\n");
    out.push_str(&stack_opening(&layers));
    out.push('\n');

    // A restacked site is usually one lineage refounding the same ground: the
    // same kind of settlement, founded from the same forebear, every time.
    // Saying that once here (instead of once per layer) is what keeps a
    // five-deep stack from reading like a stuck record (T7 review fix).
    let shared = SharedLineage::of(world, &layers);
    if let Some(descriptor) = &shared.descriptor {
        out.push('\n');
        out.push_str(descriptor);
        out.push('\n');
    }
    if let Some(founding) = &shared.founding {
        out.push('\n');
        out.push_str(founding);
        out.push('\n');
    }
    out.push('\n');

    for (i, layer) in layers.iter().enumerate() {
        out.push_str(&render_layer(world, layer, i, layers.len(), now, &shared));
        out.push('\n');
    }

    // The flesh sample: the last community to hold this ground is the one
    // whose remnants lie in the present-day grass.
    let last = &layers[layers.len() - 1];
    out.push_str(&render_flesh(world, last, now));
    out
}

/// Lineage-wide framing hoisted out of the per-layer paragraphs when every
/// layer in a multi-layer stack would otherwise repeat it verbatim (T7
/// review fix): the settlement descriptor (tech/function/notability/people)
/// and the founding sentence. Both are `None` for a single-layer site (there
/// is nothing to hoist) or a heterogeneous stack (each layer keeps telling
/// its own part of the story).
struct SharedLineage {
    /// "Every layer here is a classical bugbear steading, an ordinary
    /// place…" — present only when every layer shares the same tech,
    /// function, notability, and people.
    descriptor: Option<String>,
    /// The founding sentence — present only when every layer's
    /// `founded_from` thread renders identically (the whole stack traces to
    /// one forebear, or every layer independently broke new ground).
    founding: Option<String>,
}

impl SharedLineage {
    /// Compute the hoistable lines for one site's stack, or an empty
    /// (`None`, `None`) pair when there's nothing worth hoisting.
    fn of(world: &World, layers: &[Layer]) -> SharedLineage {
        if layers.len() < 2 {
            return SharedLineage {
                descriptor: None,
                founding: None,
            };
        }
        let first = &layers[0].record;
        let descriptor_uniform = layers.iter().all(|l| {
            l.record.tech == first.tech
                && l.record.function == first.function
                && l.record.notability == first.notability
                && l.people == layers[0].people
        });
        let descriptor = descriptor_uniform.then(|| {
            let tech = tech_word(first.tech);
            format!(
                "Every layer here is {article} {tech} {people} {noun}, {notability}.",
                article = article(tech),
                people = layers[0].people,
                noun = function_noun(first.function),
                notability = notability_phrase(first.notability),
            )
        });

        let founding_sentences: Vec<String> = layers
            .iter()
            .map(|l| founding_sentence(world, &l.record))
            .collect();
        let founding_uniform = founding_sentences.windows(2).all(|w| w[0] == w[1]);
        let founding = founding_uniform.then(|| {
            format!(
                "The whole lineage traces to one root: {}",
                founding_sentences[0]
            )
        });

        SharedLineage {
            descriptor,
            founding,
        }
    }
}

/// One occupation layer at a site: the ledger entity that carries it, plus the
/// reconstructed [`OccupationRecord`] the flesh derivations read.
struct Layer {
    /// The occupation entity minted for this record at emit time.
    entity: EntityId,
    /// The record rebuilt from that entity's committed facts.
    record: OccupationRecord,
    /// The people's canonical label (kept alongside the resolved `KindId` for
    /// prose; equals `record.people.0`).
    people: String,
}

/// Every occupation layer sitting on `site`, oldest founding first. Reads the
/// ledger's `is-occupation` index and keeps only those whose `occ-site`
/// matches. Ordered by `(founded, entity-id)` via `f64::total_cmp` — total and
/// deterministic.
fn layers_at(world: &World, site: CellId) -> Vec<Layer> {
    let mut layers: Vec<Layer> = world
        .ledger
        .find(hornvale_history::IS_OCCUPATION)
        .filter_map(|f| {
            let entity = f.subject;
            match world.ledger.value_of(entity, hornvale_history::OCC_SITE) {
                Some(Value::Number(cell)) if *cell as u32 == site.0 => (),
                _ => return None,
            }
            let record = record_of(world, entity)?;
            let people = record.people.0.to_string();
            Some(Layer {
                entity,
                record,
                people,
            })
        })
        .collect();
    layers.sort_by(|a, b| {
        a.record
            .founded
            .total_cmp(&b.record.founded)
            .then(a.entity.0.cmp(&b.entity.0))
    });
    layers
}

/// Reconstruct the [`OccupationRecord`] an occupation entity's committed facts
/// describe — enough of it to render prose and derive flesh. The fields flesh
/// never reads (`community`/`lineage`/`deity`/`tongue`) are filled with inert
/// placeholders (the entity's own id), since a *derived* readout has no need of
/// them. `None` if the entity is missing a load-bearing fact or names a people
/// outside the biosphere roster.
fn record_of(world: &World, entity: EntityId) -> Option<OccupationRecord> {
    let people_label = world.ledger.text_of(entity, hornvale_history::OCC_PEOPLE)?;
    let people = resolve_people(people_label)?;
    let site = CellId(number(world, entity, hornvale_history::OCC_SITE)? as u32);
    let founded = number(world, entity, hornvale_history::OCC_FOUNDED)?;
    let ended = number(world, entity, hornvale_history::OCC_ENDED);
    let peak_population = number(world, entity, hornvale_history::OCC_PEAK)? as u32;
    let tech = parse_tech(world.ledger.text_of(entity, hornvale_history::OCC_TECH)?)?;
    let function = parse_function(
        world
            .ledger
            .text_of(entity, hornvale_history::OCC_FUNCTION)?,
    )?;
    let cause = world
        .ledger
        .text_of(entity, hornvale_history::OCC_CAUSE)
        .and_then(parse_cause);
    let notability = parse_notability(
        world
            .ledger
            .text_of(entity, hornvale_history::OCC_NOTABILITY)?,
    )?;
    let ended_by = match world
        .ledger
        .value_of(entity, hornvale_history::OCC_ENDED_BY)
    {
        Some(Value::Entity(e)) => Ended::By(*e),
        _ => Ended::Nature,
    };
    let founded_from = match world
        .ledger
        .value_of(entity, hornvale_history::OCC_FOUNDED_FROM)
    {
        Some(Value::Entity(e)) => Founding::From(*e),
        Some(Value::Number(cell)) => Founding::Genesis(CellId(*cell as u32)),
        _ => Founding::Genesis(site),
    };

    Some(OccupationRecord {
        people,
        community: entity,
        lineage: entity,
        site,
        founded,
        ended,
        peak_population,
        tech,
        function,
        deity: None,
        tongue: None,
        cause,
        ended_by,
        founded_from,
        notability,
    })
}

/// A functional `Number` object read back as an `f64`.
/// type-audit: bare-ok(count: return)
fn number(world: &World, entity: EntityId, predicate: &str) -> Option<f64> {
    match world.ledger.value_of(entity, predicate) {
        Some(Value::Number(n)) => Some(*n),
        _ => None,
    }
}

/// The present frame's day: the latest founding-or-ending recorded anywhere in
/// the world's deep history. This is the moment "today" sits at — a ruin's age
/// is measured back from here. Deterministic (`f64::total_cmp` over ledger
/// numbers).
/// type-audit: bare-ok(count: return)
fn present_day(world: &World) -> f64 {
    if let Some(now) = world.ledger.find(hornvale_history::HISTORY_NOW).next()
        && let Value::Number(n) = &now.object
    {
        return *n;
    }
    // Fallback for a world with no committed `history-now` fact (a save from
    // before T8, or a synthetic Lab world that never ran the composition-root
    // bake): approximate the present as the latest committed occupation
    // event. This UNDERSTATES every ruin's age and tenure by the bake's
    // post-history stretch (the true present is `BakeConfig::end_year`, not
    // the last stochastic draw), which is exactly the gap committing
    // `history-now` fixes — see `windows/worldgen::history_emit::emit_now`.
    let founded = world.ledger.find(hornvale_history::OCC_FOUNDED);
    let ended = world.ledger.find(hornvale_history::OCC_ENDED);
    founded
        .chain(ended)
        .filter_map(|f| match &f.object {
            Value::Number(n) => Some(*n),
            _ => None,
        })
        .max_by(|a, b| a.total_cmp(b))
        .unwrap_or(0.0)
}

/// Resolve a ledger-round-tripped people label back to its canonical `'static`
/// `KindId` by matching the biosphere roster (the same content-compare idiom
/// `history_emit::resolve_people` uses — runtime text can't construct a
/// `KindId` key).
fn resolve_people(label: &str) -> Option<KindId> {
    hornvale_species::biosphere_registry()
        .iter()
        .find(|(k, _)| k.0 == label)
        .map(|(k, _)| *k)
}

fn parse_tech(label: &str) -> Option<TechHorizon> {
    Some(match label {
        "neolithic" => TechHorizon::Neolithic,
        "bronze" => TechHorizon::Bronze,
        "iron" => TechHorizon::Iron,
        "classical" => TechHorizon::Classical,
        _ => return None,
    })
}

fn parse_function(label: &str) -> Option<Function> {
    Some(match label {
        "agrarian" => Function::Agrarian,
        "mine" => Function::Mine,
        "trade" => Function::Trade,
        "cult" => Function::Cult,
        "fort" => Function::Fort,
        _ => return None,
    })
}

fn parse_cause(label: &str) -> Option<CauseOfEnd> {
    Some(match label {
        "famine" => CauseOfEnd::Famine,
        "burned" => CauseOfEnd::Burned,
        "plague" => CauseOfEnd::Plague,
        "fled" => CauseOfEnd::Fled,
        "migrated" => CauseOfEnd::Migrated,
        _ => return None,
    })
}

fn parse_notability(label: &str) -> Option<Notability> {
    Some(match label {
        "backwater" => Notability::Backwater,
        "common" => Notability::Common,
        "seat" => Notability::Seat,
        _ => return None,
    })
}

/// The one-line opening for the whole stack.
fn stack_opening(layers: &[Layer]) -> String {
    match layers.len() {
        1 => "One people, and one only, ever made a home of this ground.".to_string(),
        n => format!(
            "{} lives have passed over this ground, one settling atop the ruins of \
             the last.",
            capitalize(count_word(n as u64))
        ),
    }
}

/// Render one occupation layer as a prose paragraph. Always names this
/// layer's own peak population (T7 review fix) — the one figure that
/// distinguishes it from its neighbors even when the descriptor and founding
/// sentences are hoisted out to [`SharedLineage`] because every layer would
/// otherwise repeat them verbatim.
fn render_layer(
    world: &World,
    layer: &Layer,
    index: usize,
    total: usize,
    now: f64,
    shared: &SharedLineage,
) -> String {
    let r = &layer.record;
    let depth = depth_phrase(index, total);
    let tech = tech_word(r.tech);
    // People as an adjectival modifier reads singular ("a bugbear steading"),
    // never "a bugbears steading".
    let mut para = if shared.descriptor.is_some() {
        // The descriptor already ran once, up front; this layer just needs
        // its place in the stack and its own headcount.
        format!(
            "{depth} — at its height {}.\n",
            souls_phrase(r.peak_population)
        )
    } else {
        format!(
            "{depth} — {article} {tech} {people} {noun}, {notability}, at its height {souls}.\n",
            article = article(tech),
            people = layer.people,
            noun = function_noun(r.function),
            notability = notability_phrase(r.notability),
            souls = souls_phrase(r.peak_population),
        )
    };
    if shared.founding.is_none() {
        para.push_str("  ");
        para.push_str(&founding_sentence(world, r));
        para.push('\n');
    }
    para.push_str("  ");
    para.push_str(&span_sentence(r, now));
    para.push('\n');
    para.push_str("  ");
    para.push_str(&ending_sentence(world, r, index));
    para.push('\n');
    para
}

/// Where in the stack this layer sits, in reading order (deepest first).
fn depth_phrase(index: usize, total: usize) -> &'static str {
    if index == 0 {
        "The deepest layer"
    } else if index + 1 == total {
        "The last of them"
    } else {
        match index {
            1 => "Above it",
            2 => "Higher still",
            3 => "Above that again",
            _ => "And later",
        }
    }
}

/// The founding sentence, following the ★ `founded_from` thread: raised from
/// nothing, or settled by people fleeing a failing home.
fn founding_sentence(world: &World, r: &OccupationRecord) -> String {
    match r.founded_from {
        Founding::Genesis(_) => {
            "They raised it from nothing on open ground — the first to break this soil.".to_string()
        }
        Founding::From(e) => {
            let (who, whence, fled) = forebears(world, e);
            if fled {
                format!("Its founders fled the ice of {whence}, {who} looking for kinder ground.")
            } else {
                format!("It was settled by {who} sent out from {whence}.")
            }
        }
    }
}

/// Describe a predecessor occupation an entity id refers to: `(who, whence,
/// they-fled)`. `they-fled` is true when the predecessor ended by climate
/// (migration or flight) — the thread the brief names, "its founders fled the
/// ice of ...".
fn forebears(world: &World, e: EntityId) -> (String, String, bool) {
    let who = world
        .ledger
        .text_of(e, hornvale_history::OCC_PEOPLE)
        .map(pluralize)
        .unwrap_or_else(|| "settlers".to_string());
    let whence = match number(world, e, hornvale_history::OCC_SITE) {
        Some(cell) => format!("the clearing at cell {}", cell as u32),
        None => "a lost place".to_string(),
    };
    let fled = matches!(
        world.ledger.text_of(e, hornvale_history::OCC_CAUSE),
        Some("migrated") | Some("fled")
    );
    (who, whence, fled)
}

/// The span sentence: founded → ended, with the tenure in years, or "stands
/// yet" for a living community.
fn span_sentence(r: &OccupationRecord, now: f64) -> String {
    match r.ended {
        None => {
            let tenure = (now - r.founded).max(0.0);
            format!(
                "Founded in the year {}, it stands yet — {} years and counting.",
                year(r.founded),
                year(tenure)
            )
        }
        Some(end) => {
            let tenure = (end - r.founded).max(0.0);
            format!(
                "Founded in the year {}, it held for {} years, until the year {}.",
                year(r.founded),
                year(tenure),
                year(end)
            )
        }
    }
}

/// The ending sentence, following the ★ `ended_by` thread. The raw cause word
/// appears (so a reader — and a test — can see exactly how it fell); for a
/// long re-occupation stack the migration line is varied by `index` so the
/// stratigraphy reads as a chronicle, not a stuck record.
fn ending_sentence(world: &World, r: &OccupationRecord, index: usize) -> String {
    let Some(cause) = r.cause else {
        return "It has never ended; the people are there still.".to_string();
    };
    let by = match r.ended_by {
        Ended::By(e) => {
            let (who, _, _) = forebears(world, e);
            Some(who)
        }
        Ended::Nature => None,
    };
    match cause {
        CauseOfEnd::Burned => match by {
            Some(who) => format!("Its end came by fire — burned by {who}, and never rebuilt."),
            None => "Its end came by fire — burned, and never rebuilt.".to_string(),
        },
        CauseOfEnd::Migrated => migration_line(index),
        CauseOfEnd::Fled => match by {
            Some(who) => format!("They fled — put to flight by {who} — and did not return."),
            None => "They fled the site, and did not return.".to_string(),
        },
        CauseOfEnd::Famine => {
            "The ground failed them: famine emptied the site, one hungry season at a time."
                .to_string()
        }
        CauseOfEnd::Plague => {
            "Plague emptied it — the dead outnumbered the living, and the rest walked away."
                .to_string()
        }
    }
}

/// One of three climate-abandonment endings, cycled by layer index so a deep
/// re-occupation stack reads as a chronicle. The first variant (index 0, the
/// deepest layer of any run) always carries the raw cause word "migrated".
fn migration_line(index: usize) -> String {
    match index % 3 {
        0 => "In the end the cold drove them on: they migrated away, abandoning the \
              clearing to the ice rather than starve on it."
            .to_string(),
        1 => "Again the ice crept down the valley, and again they gathered what they \
              could carry and left the clearing to the frost."
            .to_string(),
        _ => "The seasons shortened until the ground would no longer feed them; they \
              moved on, as their forebears had before them."
            .to_string(),
    }
}

/// Render the derived flesh of the last community to hold the site: the shape
/// of the place it built, and what remains of it in the present-day grass.
fn render_flesh(world: &World, layer: &Layer, now: f64) -> String {
    let seed = flesh_seed(world, layer.entity);
    let structures = structures_of(&layer.record, seed);
    let residue = residue_of(&layer.record, now, seed);

    let mut out = String::new();
    out.push_str("In the grass today\n");
    out.push_str(&"-".repeat(18));
    out.push('\n');

    let people = pluralize(&layer.people);
    out.push_str(&format!(
        "At its height {}, the last {people} here raised {}.\n",
        souls_phrase(layer.record.peak_population),
        structure_phrase(&structures),
    ));

    if layer.record.is_alive() {
        out.push_str(
            "They are living there yet — there is no ruin to read, only smoke on the air.\n",
        );
        return out;
    }

    out.push_str(&residue_sentence(&residue));
    out.push('\n');
    out
}

/// The occupation-scoped seed the flesh derivations expand from. Derived purely
/// from the world seed and the occupation entity id — a *derived* readout, so
/// this is a rendering convention, not a save-format contract (it commits
/// nothing). Mirrors the `history/flesh` label the domain's module doc names.
fn flesh_seed(world: &World, entity: EntityId) -> Seed {
    world
        .seed
        .derive("history/flesh")
        .derive(&entity.0.get().to_string())
}

/// A prose list of the structures a community raised, folded by kind ("four
/// huts, a granary").
fn structure_phrase(structures: &[Structure]) -> String {
    // Count by kind, preserving first-seen order (small, bounded list).
    let mut kinds: Vec<(Structure, u32)> = Vec::new();
    for &s in structures {
        if let Some(entry) = kinds.iter_mut().find(|(k, _)| *k == s) {
            entry.1 += 1;
        } else {
            kinds.push((s, 1));
        }
    }
    let parts: Vec<String> = kinds
        .iter()
        .map(|(k, n)| {
            if *n == 1 {
                format!("a {}", structure_singular(*k))
            } else {
                format!("{} {}", count_word(u64::from(*n)), structure_plural(*k))
            }
        })
        .collect();
    join_prose(&parts)
}

/// The residue sentence — the archaeological impression a ruin leaves in the
/// present-day grass. Partitioned by material [`Durability`]: the perishable
/// personal effects a *young* ruin still holds (the doll — the campaign's
/// whole promise) lead when present, then the durable record that endures for
/// millennia (potsherds, foundations, worked stone, bone) reads as a site a
/// searcher can piece a story out of — never a bare item list.
fn residue_sentence(residue: &Residue) -> String {
    if residue.items.is_empty() {
        return "Of all that, nothing legible remains: the ground has taken it back.".to_string();
    }
    // Split the finds by durability: perishable personal effects vs the
    // durable/eternal archaeological record. Sort each by evocativeness and
    // fold out duplicate kinds (a doubled worked-stone scatter reads once).
    let mut perishable: Vec<ResidueItem> = residue
        .items
        .iter()
        .copied()
        .filter(|i| i.durability() == Durability::Perishable)
        .collect();
    let mut durable: Vec<ResidueItem> = residue
        .items
        .iter()
        .copied()
        .filter(|i| i.durability() != Durability::Perishable)
        .collect();
    perishable.sort_by_key(residue_rank);
    perishable.dedup();
    durable.sort_by_key(residue_rank);
    durable.dedup();

    let mut sentences: Vec<String> = Vec::new();
    if !perishable.is_empty() {
        let parts: Vec<String> = perishable.iter().map(|i| residue_phrase(*i)).collect();
        sentences.push(format!(
            "They did not take everything: in the grass a searcher finds {}.",
            join_prose(&parts)
        ));
    }
    if !durable.is_empty() {
        let parts: Vec<String> = durable.iter().map(|i| residue_phrase(*i)).collect();
        // A different lead depending on whether a perishable find already
        // opened the paragraph — so the durable record either stands alone as
        // the whole impression or reads as the deeper layer beneath the doll.
        let lead = if perishable.is_empty() {
            "The dwellings are long gone to grass, but the ground still keeps the shape of them"
        } else {
            "Older still, the durable record endures"
        };
        sentences.push(format!("{lead}: {}.", join_prose(&parts)));
    }
    sentences.join(" ")
}

/// Sort key putting the most evocative finds first (the doll above all, then
/// the sacred and inscribed, then the domestic debris that outlasts them).
/// type-audit: bare-ok(count: return)
fn residue_rank(item: &ResidueItem) -> u8 {
    match item {
        ResidueItem::Doll => 0,
        ResidueItem::Reliquary => 1,
        ResidueItem::Inscription => 2,
        ResidueItem::Bauble => 3,
        ResidueItem::Potsherd => 4,
        ResidueItem::Foundation => 5,
        ResidueItem::WorkedStone => 6,
        ResidueItem::Tool => 7,
        ResidueItem::Weapon => 8,
        ResidueItem::Bones => 9,
    }
}

fn residue_phrase(item: ResidueItem) -> String {
    match item {
        ResidueItem::Doll => "a child's rag doll, ragged but whole, where a doorway once stood",
        ResidueItem::Bauble => "a scatter of bead ornaments, their strings long rotted",
        ResidueItem::Reliquary => "a sacred vessel, buried deliberate and deep",
        ResidueItem::Inscription => "a slab of incised text, outlasting everything around it",
        ResidueItem::Tool => "a worked pot, chipped at the lip",
        ResidueItem::Weapon => "the corroded head of a blade",
        ResidueItem::Bones => "old bone, worked loose from the ground by the frost",
        ResidueItem::Potsherd => "potsherds scattered where the huts once stood",
        ResidueItem::Foundation => "the low turf-lines of the dwellings still ridging the grass",
        ResidueItem::WorkedStone => "a scatter of worked flint",
    }
    .to_string()
}

fn structure_singular(s: Structure) -> &'static str {
    match s {
        Structure::Hut => "hut",
        Structure::Longhouse => "longhouse",
        Structure::Granary => "granary",
        Structure::Shrine => "shrine",
        Structure::Temple => "temple",
        Structure::Wall => "defensive wall",
        Structure::Mineshaft => "mineshaft",
        Structure::Market => "market ground",
    }
}

fn structure_plural(s: Structure) -> &'static str {
    match s {
        Structure::Hut => "huts",
        Structure::Longhouse => "longhouses",
        Structure::Granary => "granaries",
        Structure::Shrine => "shrines",
        Structure::Temple => "temples",
        Structure::Wall => "walls",
        Structure::Mineshaft => "mineshafts",
        Structure::Market => "market grounds",
    }
}

fn tech_word(t: TechHorizon) -> &'static str {
    match t {
        TechHorizon::Neolithic => "neolithic",
        TechHorizon::Bronze => "bronze-working",
        TechHorizon::Iron => "iron-working",
        TechHorizon::Classical => "classical",
    }
}

fn function_noun(f: Function) -> &'static str {
    match f {
        Function::Agrarian => "steading",
        Function::Mine => "mining camp",
        Function::Trade => "trade waypoint",
        Function::Cult => "shrine-seat",
        Function::Fort => "garrison",
    }
}

fn notability_phrase(n: Notability) -> &'static str {
    match n {
        Notability::Backwater => "a backwater at the region's edge",
        Notability::Common => "an ordinary place, neither famed nor forgotten",
        Notability::Seat => "a regional seat of power",
    }
}

/// A peak-population phrase that reads for the small hamlets deep history
/// actually leaves behind (a remnant of one soul is not "some 1 souls").
fn souls_phrase(peak: u32) -> String {
    match peak {
        0 => "no more than a memory".to_string(),
        1 => "no more than a single household".to_string(),
        2..=9 => format!("a mere {} souls", count_word(u64::from(peak))),
        n => format!("some {} souls", count_word(u64::from(n))),
    }
}

/// The indefinite article for a word, by its leading sound ("a bronze", "an
/// iron"). Vowel-letter heuristic — adequate for this window's fixed
/// vocabulary (no "hour"/"union" edge cases arise).
fn article(word: &str) -> &'static str {
    match word.chars().next().map(|c| c.to_ascii_lowercase()) {
        Some('a' | 'e' | 'i' | 'o' | 'u') => "an",
        _ => "a",
    }
}

/// Pluralize a people label naively (`goblin` → `goblins`). Adequate for the
/// biosphere roster, which has no irregular plurals.
fn pluralize(people: &str) -> String {
    if people.ends_with('s') {
        people.to_string()
    } else {
        format!("{people}s")
    }
}

/// A small-integer count as an English word (falls back to digits past ten).
fn count_word(n: u64) -> String {
    match n {
        0 => "no".to_string(),
        1 => "one".to_string(),
        2 => "two".to_string(),
        3 => "three".to_string(),
        4 => "four".to_string(),
        5 => "five".to_string(),
        6 => "six".to_string(),
        7 => "seven".to_string(),
        8 => "eight".to_string(),
        9 => "nine".to_string(),
        10 => "ten".to_string(),
        other => other.to_string(),
    }
}

/// Render a whole-number `f64` day/year as a plain integer for prose.
/// type-audit: bare-ok(count: value)
fn year(value: f64) -> i64 {
    value.round() as i64
}

/// Join a prose list with commas and a trailing "and".
fn join_prose(parts: &[String]) -> String {
    match parts.len() {
        0 => String::new(),
        1 => parts[0].clone(),
        2 => format!("{} and {}", parts[0], parts[1]),
        _ => {
            let head = parts[..parts.len() - 1].join(", ");
            format!("{head}, and {}", parts[parts.len() - 1])
        }
    }
}

/// Capitalize the first character of a string.
fn capitalize(s: String) -> String {
    let mut chars = s.chars();
    match chars.next() {
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
        None => s,
    }
}
