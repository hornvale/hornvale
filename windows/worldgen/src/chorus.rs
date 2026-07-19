//! The Chorus (C4): derive every placed culture's [`AccountParams`] from
//! existing authored/committed state — the `voice_params` twin — plus the
//! authored observability table (spec §3.2) and the ground fact list, then
//! run each culture's account through `hornvale_language::account_of`.
//!
//! Nothing here is new draws or new save state: [`account_params_of`] reads
//! the already-built [`WorldComponents`] and the already-committed ledger;
//! [`chorus_ground`] mirrors `windows/book`'s `render_volume` fact
//! selection exactly (layering forbids importing it — `windows/book`
//! depends on `windows/worldgen`, not the reverse). See each function's own
//! doc comment for its derivation rule.
//!
//! **C5, The Explanations**, extends this file with the causal filter's
//! derivation, binding, and assembly: [`schema_prior`] and [`beta_of`]
//! derive the per-culture schema weighting (no draw); [`cyclic_beliefs_of`]
//! re-derives the period-match join to a culture's own pantheon; `explain`
//! (private, called from [`accounts_of`] after [`account_of`]) wraps
//! qualifying account entries in [`hornvale_language::Disposition::Explained`].
//! The only new seeded draws anywhere in this module are the two render-time
//! streams `explain` derives (`language/<species>/schema/<domain>/<shape>`,
//! `language/<species>/lexeme/<fact-key>`) — no epoch, no ledger change.

use crate::{BuildError, WorldComponents};
use hornvale_culture::Subsistence;
use hornvale_kernel::{Seed, World, world::IS_A};
use hornvale_language::{
    Account, AccountParams, Disposition, FactShape, GroundFact, NeededConcept, Observability,
    OrderPolicy, Requirement, SchemaId, SlotKind, Stance, SubFrame, account_of, admitted,
    lexemes_for, schema_table, schemas::Manner, select_lexeme, select_schema,
};
use hornvale_species::{ActivityCycle, PerceptionVector, PsychVector, Sociality, StatusBasis};
use std::collections::{BTreeMap, BTreeSet};

/// The authored observability table (build-state; spec §3.2): what it
/// takes for an observing culture to retain each ground predicate's facts.
/// Keyed by predicate text — `hornvale_language::account_of`'s table lookup
/// is a plain string key, matched against [`chorus_ground`]'s
/// `GroundFact::predicate` values with no registry round-trip. The three
/// astronomy predicates use their domain's own constants rather than string
/// literals; `is-a` and `instance-of` use the kernel's world-fact
/// constants.
/// type-audit: bare-ok(identifier-text)
pub fn observability_table() -> BTreeMap<String, Observability> {
    let mut table = BTreeMap::new();
    table.insert(
        IS_A.to_string(),
        Observability {
            requirement: Requirement::Taxonomic,
            domain: "sky",
            concept: NeededConcept::Object,
            shape: FactShape::Taxonomy,
        },
    );
    table.insert(
        hornvale_astronomy::facts::MOON_COUNT.to_string(),
        Observability {
            requirement: Requirement::SkyGraded { threshold: 0.6 },
            domain: "sky",
            concept: NeededConcept::Fixed("moon"),
            shape: FactShape::Count,
        },
    );
    table.insert(
        hornvale_astronomy::facts::STAR_CLASS.to_string(),
        Observability {
            requirement: Requirement::Instrumental,
            domain: "sky",
            concept: NeededConcept::Fixed("star"),
            shape: FactShape::Taxonomy,
        },
    );
    table.insert(
        hornvale_astronomy::facts::DAY_LENGTH_STD.to_string(),
        Observability {
            requirement: Requirement::CrossReferential,
            domain: "sky",
            concept: NeededConcept::Fixed("sun"),
            shape: FactShape::CyclicEvent,
        },
    );
    table.insert(
        hornvale_kernel::INSTANCE_OF.to_string(),
        Observability {
            requirement: Requirement::Manifest,
            domain: "peoples",
            concept: NeededConcept::ObjectKind,
            shape: FactShape::Roster,
        },
    );
    table
}

/// A culture's sky-observation capability, in `[0, 1]`: the mean of night
/// vision and sky attention, plus an activity-cycle bonus (`Nocturnal`
/// 0.15, `Crepuscular` 0.08, `Diurnal` 0.0), clamped to 1.0. Gates
/// [`hornvale_language::Requirement::SkyGraded`] facts in the observability
/// table above.
///
/// **TECH-1 seam**: the capability derivation is a seam — a future tech
/// ladder (instruments, recorded observation) replaces this function
/// without touching the filter contract (`Requirement`/[`Observability`]
/// stay exactly as authored; only how a culture's `sky_capability` scalar
/// is computed would change).
/// type-audit: bare-ok(ratio)
pub fn sky_capability(p: &PerceptionVector) -> f64 {
    let bonus = match p.activity {
        ActivityCycle::Nocturnal => 0.15,
        ActivityCycle::Crepuscular => 0.08,
        ActivityCycle::Diurnal => 0.0,
    };
    ((p.night_vision + p.sky_attention) / 2.0 + bonus).min(1.0)
}

/// The per-(culture, domain) schema prior (C5, preregistered plan-header
/// numbers): every schema in `admitted` starts at weight `1.0`, then
/// `subsistence`'s multiplier (Herding: kinship ×1.5, cycle-return ×1.3;
/// Farming: cycle-return ×1.5, balance ×1.3; Fishing: substance-flow ×1.5,
/// path-journey ×1.2; Foraging: agentive ×1.5, link ×1.2) and
/// `sociality`'s (Hierarchic: agentive ×1.3; Communal: balance ×1.2) apply
/// multiplicatively — a schema can gain both bonuses at once. Derived, no
/// draw; [`select_schema`] draws over the result once β has sharpened it.
/// type-audit: bare-ok(ratio: return)
pub fn schema_prior(
    subsistence: Subsistence,
    sociality: Sociality,
    admitted: &[SchemaId],
) -> Vec<(SchemaId, f64)> {
    admitted
        .iter()
        .map(|&id| {
            let mut weight = 1.0;
            match subsistence {
                Subsistence::Herding => {
                    if id == SchemaId::Kinship {
                        weight *= 1.5;
                    }
                    if id == SchemaId::CycleReturn {
                        weight *= 1.3;
                    }
                }
                Subsistence::Farming => {
                    if id == SchemaId::CycleReturn {
                        weight *= 1.5;
                    }
                    if id == SchemaId::Balance {
                        weight *= 1.3;
                    }
                }
                Subsistence::Fishing => {
                    if id == SchemaId::SubstanceFlow {
                        weight *= 1.5;
                    }
                    if id == SchemaId::PathJourney {
                        weight *= 1.2;
                    }
                }
                Subsistence::Foraging => {
                    if id == SchemaId::Agentive {
                        weight *= 1.5;
                    }
                    if id == SchemaId::LinkSympathy {
                        weight *= 1.2;
                    }
                }
            }
            match sociality {
                Sociality::Hierarchic if id == SchemaId::Agentive => weight *= 1.3,
                Sociality::Communal if id == SchemaId::Balance => weight *= 1.2,
                _ => {}
            }
            (id, weight)
        })
        .collect()
}

/// β, the monomania dial (C5, ledger #6): `status_basis` (`Knowledge` →
/// `1.0`, `Rank` → `2.0`; no floor species carries `Generosity` — the
/// midpoint `1.5` keeps the dial monotone pending a future roster entry),
/// plus `0.5` when `sociality` is `Hierarchic`. Roster (measured, plan
/// header): goblin/hobgoblin `2.5`, bugbear `2.0`, kobold `1.0`.
/// type-audit: bare-ok(ratio: return)
pub fn beta_of(psych: &PsychVector) -> f64 {
    let base = match psych.status_basis {
        StatusBasis::Knowledge => 1.0,
        StatusBasis::Rank => 2.0,
        StatusBasis::Generosity => 1.5,
    };
    let modifier = if psych.sociality == Sociality::Hierarchic {
        0.5
    } else {
        0.0
    };
    base + modifier
}

/// The period-match join (spec §3.2): `species`'s flagship's cyclic
/// beliefs (`Sentiment::Cyclic`), each re-paired with its source
/// phenomenon's `period_days` at the same list position
/// `hornvale_religion::genesis` consumed to mint it — the exact join
/// `crate::line_content_for`/`crate::rendered_pantheon_of` already use for
/// tenet rendering, reused here rather than re-derived a second way.
/// Sorted ascending by period (the manner rank: index 0 is the shortest,
/// the last index the longest). Empty when `species` placed no flagship,
/// holds no beliefs, or its phenomena can't be recomputed — callers read
/// emptiness as "no sky religion to bind to" (ledger #2).
/// type-audit: bare-ok(identifier-text: species), bare-ok(ratio: return)
pub fn cyclic_beliefs_of(world: &World, species: &str) -> Vec<(hornvale_religion::Belief, f64)> {
    let Some(flagship) = crate::flagship_of(world, species) else {
        return Vec::new();
    };
    let beliefs = hornvale_religion::beliefs_held_by(world, flagship.id);
    if beliefs.is_empty() {
        return Vec::new();
    }
    let Ok(phenomena) = crate::observed_phenomena_as(world, species) else {
        return Vec::new();
    };

    let mut cyclic: Vec<(hornvale_religion::Belief, f64)> = beliefs
        .into_iter()
        .enumerate()
        .filter(|(_, b)| b.sentiment == hornvale_religion::Sentiment::Cyclic)
        .filter_map(|(i, b)| {
            phenomena
                .get(i)
                .and_then(|p| p.period_days)
                .map(|period| (b, period))
        })
        .collect();
    cyclic.sort_by(|(_, a), (_, b)| a.total_cmp(b));
    cyclic
}

/// A cyclic belief's stream-label-safe fact-shape key (the salt leg of
/// `language/<species>/schema/<domain>/<fact-shape>`).
fn fact_shape_key(shape: FactShape) -> &'static str {
    match shape {
        FactShape::CyclicEvent => "cyclic-event",
        FactShape::HighScalarState => "high-scalar-state",
        FactShape::Count => "count",
        FactShape::Taxonomy => "taxonomy",
        FactShape::Roster => "roster",
    }
}

/// Recover the `Subsistence` enum from [`hornvale_culture::subsistence_of`]'s
/// committed name text — the reverse of [`Subsistence::name`]. `None` only
/// if the committed text isn't one of the four closed names (never
/// happens for a fact `hornvale_culture::genesis` itself committed).
fn subsistence_from_name(name: &str) -> Option<Subsistence> {
    [
        Subsistence::Farming,
        Subsistence::Herding,
        Subsistence::Fishing,
        Subsistence::Foraging,
    ]
    .into_iter()
    .find(|s| s.name() == name)
}

/// The subsistence-derived motion sub-frame (plan header): Farming →
/// Walking, Herding → Mounted, Fishing → Rowing, Foraging → Stalking.
fn sub_frame_of(s: Subsistence) -> SubFrame {
    match s {
        Subsistence::Farming => SubFrame::Walking,
        Subsistence::Herding => SubFrame::Mounted,
        Subsistence::Fishing => SubFrame::Rowing,
        Subsistence::Foraging => SubFrame::Stalking,
    }
}

/// A cyclic belief's told pace (ledger #5): rank `index` among `len`
/// cyclic beliefs sorted ascending by period — `Brisk` for the shortest,
/// `Slow` for the longest, `Neutral` for a sole or middle-ranked belief.
fn manner_of(index: usize, len: usize) -> Manner {
    if len <= 1 {
        Manner::Neutral
    } else if index == 0 {
        Manner::Brisk
    } else if index == len - 1 {
        Manner::Slow
    } else {
        Manner::Neutral
    }
}

/// `predicate`'s observability domain under `params`, or `None` if it has
/// no row — the defensive re-check [`explain`] gates on before wrapping an
/// entry, so a future observability-table edit that moves a predicate off
/// `"sky"` silently disarms the causal filter rather than mis-firing it.
fn domain_of<'a>(params: &'a AccountParams, predicate: &str) -> Option<&'a str> {
    params.observability.get(predicate).map(|o| o.domain)
}

/// Whether `schema`'s surface frame (`windows/book`'s Task 4 closed frame
/// table) needs a bound deity name at all (review carry-over, C5 T4, closed
/// out at C6 T1): all three deity-bearing frames are now
/// `hornvale_language::schema_table`'s own agent slots — `SlotKind::Agent`
/// for [`SchemaId::Agentive`] and [`SchemaId::LinkSympathy`], `SlotKind::Kin`
/// for [`SchemaId::Kinship`]. `LinkSympathy` used to read `SlotKind::None`
/// in that table even though its Task-4 frame ("…because it answers
/// ⟨Deity⟩.") always named a deity, forcing this function to carry a
/// `|| schema == SchemaId::LinkSympathy` special case; C6 T1 made the table
/// honest (`LinkSympathy` now rows in as `SlotKind::Agent`), so the slot
/// check alone now covers every deity-bearing frame and the special case is
/// deleted. This function is still the single place that decides WHETHER a
/// fired schema binds an agent; [`bind_agent`] is the only caller.
fn agent_bearing(schema: SchemaId) -> bool {
    let slot = schema_table()
        .iter()
        .find(|row| row.id == schema)
        .map(|row| row.slot);
    matches!(slot, Some(SlotKind::Agent) | Some(SlotKind::Kin))
}

/// The agent bound into a fired schema's explanation, or `None` when its
/// surface frame is agentless — see [`agent_bearing`]. `deity` is the
/// caller's already-selected binding belief's deity name (the day binds
/// the period-matched belief; the moons bind the slowest cyclic belief) —
/// this function only decides WHETHER to bind, never WHICH belief supplies
/// the name (that selection is unchanged from T3, and identical for every
/// agent-bearing schema at a given call site).
fn bind_agent(schema: SchemaId, deity: &str) -> Option<String> {
    agent_bearing(schema).then(|| deity.to_string())
}

/// The committed `day-length-std` value carried by `account`'s own ground
/// facts — present regardless of disposition (`Lost`, in this
/// predicate's case, always: `Requirement::CrossReferential` never
/// passes), since [`GroundFact::object`] is the unfiltered ground truth
/// the account's dispositions are computed FROM, never mutated by
/// [`account_of`]. `None` if no such fact exists in this account at all.
fn day_length_std_value(account: &Account) -> Option<f64> {
    account.entries.iter().find_map(|e| {
        if e.fact.predicate == hornvale_astronomy::facts::DAY_LENGTH_STD {
            match &e.fact.object {
                hornvale_kernel::Value::Number(n) => Some(*n),
                _ => None,
            }
        } else {
            None
        }
    })
}

/// The day's causal-filter binding (spec §3.2): fires on the
/// `day-length-std` entry (always `Lost` — verified by [`domain_of`]
/// staying gated `"sky"`, a defensive re-check, not an assumption) iff a
/// cyclic belief's re-derived period matches `day` within 1% relative
/// tolerance (`(p - day).abs() < 0.01 * day`). **Anonymity is
/// constitutional**: the match is against the PERIOD alone — never
/// against the source phenomenon's identity or a "sun" string — so this
/// binding cannot leak which system produced the day-length-std fact.
/// Ties bind to the first (shortest-period) match in ascending order.
/// Agent binds for every deity-bearing schema (see [`agent_bearing`]:
/// [`SchemaId::Agentive`], [`SchemaId::Kinship`], [`SchemaId::LinkSympathy`]
/// — [`bind_agent`] decides); every other schema explains agentlessly
/// (`agent: None`), per the plan header. Manner comes from the binding
/// belief's own rank among `cyclic`, independent of which schema fires.
#[allow(clippy::too_many_arguments)]
fn explain_day(
    world_seed: Seed,
    species: &str,
    account: &mut Account,
    params: &AccountParams,
    cyclic: &[(hornvale_religion::Belief, f64)],
    day: f64,
    beta: f64,
    subsistence: Subsistence,
    sociality: Sociality,
) {
    let predicate = hornvale_astronomy::facts::DAY_LENGTH_STD;
    if domain_of(params, predicate) != Some("sky") {
        return;
    }
    let Some(day_index) = account
        .entries
        .iter()
        .position(|e| e.fact.predicate == predicate)
    else {
        return;
    };
    if !matches!(account.entries[day_index].disposition, Disposition::Lost(_)) {
        return;
    }
    let Some((rank, (belief, _period))) = cyclic
        .iter()
        .enumerate()
        .find(|(_, (_, period))| (*period - day).abs() < 0.01 * day)
    else {
        return;
    };
    let manner = manner_of(rank, cyclic.len());

    let candidates = admitted(FactShape::CyclicEvent);
    let prior = schema_prior(subsistence, sociality, &candidates);
    let mut schema_stream = world_seed
        .derive("language")
        .derive(species)
        .derive("schema")
        .derive("sky")
        .derive(fact_shape_key(FactShape::CyclicEvent))
        .stream();
    let Some(schema) = select_schema(&prior, beta, &mut schema_stream) else {
        return;
    };

    let agent = bind_agent(schema, &belief.deity);
    let lexeme = if schema == SchemaId::Agentive {
        let lex_candidates = lexemes_for(SchemaId::Agentive, sub_frame_of(subsistence));
        let mut lexeme_stream = world_seed
            .derive("language")
            .derive(species)
            .derive("lexeme")
            .derive(predicate)
            .stream();
        select_lexeme(lex_candidates, &mut lexeme_stream)
    } else {
        None
    };

    let underlying = account.entries[day_index].disposition.clone();
    account.entries[day_index].disposition = Disposition::Explained {
        underlying: Box::new(underlying),
        schema,
        agent,
        lexeme,
        manner,
    };
}

/// The moons' causal-filter binding (spec §3.2): fires on a KEPT
/// `moon-count` entry iff some cyclic belief's period does NOT match
/// `day` (a slower sky-cycle exists to hang the story on). The binding
/// belief is always the SLOWEST cyclic belief (the last in `cyclic`'s
/// ascending order) — the plan header's literal rule — regardless of
/// whether that particular belief is the one whose period cleared the
/// guard. Agent binds for every deity-bearing schema (see
/// [`agent_bearing`]): [`SchemaId::Agentive`], [`SchemaId::Kinship`], and
/// [`SchemaId::LinkSympathy`] — the two `Count`-shape schemas besides
/// Agentive, both of which admit [`FactShape::Count`] and both of which
/// name a deity in their `windows/book` surface frame. Manner comes from
/// the slowest belief's own rank (`Slow`, unless it's also the sole cyclic
/// belief, which reads `Neutral`).
#[allow(clippy::too_many_arguments)]
fn explain_moons(
    world_seed: Seed,
    species: &str,
    account: &mut Account,
    params: &AccountParams,
    cyclic: &[(hornvale_religion::Belief, f64)],
    day: f64,
    beta: f64,
    subsistence: Subsistence,
    sociality: Sociality,
) {
    let predicate = hornvale_astronomy::facts::MOON_COUNT;
    if domain_of(params, predicate) != Some("sky") {
        return;
    }
    let Some(moon_index) = account
        .entries
        .iter()
        .position(|e| e.fact.predicate == predicate)
    else {
        return;
    };
    if account.entries[moon_index].disposition != Disposition::Kept {
        return;
    }
    let has_non_day_cycle = cyclic
        .iter()
        .any(|(_, period)| (*period - day).abs() >= 0.01 * day);
    if !has_non_day_cycle {
        return;
    }

    let slowest_rank = cyclic.len() - 1;
    let (belief, _period) = &cyclic[slowest_rank];
    let manner = manner_of(slowest_rank, cyclic.len());

    let candidates = admitted(FactShape::Count);
    let prior = schema_prior(subsistence, sociality, &candidates);
    let mut schema_stream = world_seed
        .derive("language")
        .derive(species)
        .derive("schema")
        .derive("sky")
        .derive(fact_shape_key(FactShape::Count))
        .stream();
    let Some(schema) = select_schema(&prior, beta, &mut schema_stream) else {
        return;
    };

    let agent = bind_agent(schema, &belief.deity);
    let lexeme = if schema == SchemaId::Agentive {
        let lex_candidates = lexemes_for(SchemaId::Agentive, sub_frame_of(subsistence));
        let mut lexeme_stream = world_seed
            .derive("language")
            .derive(species)
            .derive("lexeme")
            .derive(predicate)
            .stream();
        select_lexeme(lex_candidates, &mut lexeme_stream)
    } else {
        None
    };

    let underlying = account.entries[moon_index].disposition.clone();
    account.entries[moon_index].disposition = Disposition::Explained {
        underlying: Box::new(underlying),
        schema,
        agent,
        lexeme,
        manner,
    };
}

/// Explanation assembly (C5): wraps `account`'s day and (where kept)
/// moons entries in [`Disposition::Explained`] where a schema fires and
/// binds. Called from [`accounts_of`], after [`account_of`] — never from
/// the identity/null path ([`identity_params`](hornvale_language::identity_params)
/// never reaches this function, so the dial's pathological pole stays
/// exactly as authored).
///
/// **Unbindable guard (ledger #2):** a culture with no cyclic belief in
/// [`cyclic_beliefs_of`] explains nothing at all — no synthetic agents,
/// ever.
fn explain(world: &World, species: &str, account: &mut Account, params: &AccountParams) {
    let cyclic = cyclic_beliefs_of(world, species);
    if cyclic.is_empty() {
        return;
    }
    let Some(day) = day_length_std_value(account) else {
        return;
    };
    let Some(flagship) = crate::flagship_of(world, species) else {
        return;
    };
    let Some(subsistence) = hornvale_culture::subsistence_of(world, flagship.id)
        .as_deref()
        .and_then(subsistence_from_name)
    else {
        return;
    };
    let Ok(wc) = WorldComponents::assemble() else {
        return;
    };
    let Some(psych) = wc.psyche.get_by_label(species) else {
        return;
    };
    let beta = beta_of(psych);

    explain_day(
        world.seed,
        species,
        account,
        params,
        &cyclic,
        day,
        beta,
        subsistence,
        psych.sociality,
    );
    explain_moons(
        world.seed,
        species,
        account,
        params,
        &cyclic,
        day,
        beta,
        subsistence,
        psych.sociality,
    );
}

// ---------------------------------------------------------------------
// C6, The Doctrine: the institution's second stack.
// ---------------------------------------------------------------------
//
// Doctrine params/account are DERIVED from folk's — never authored per
// culture, never a second draw over the same furniture — by exactly the
// four preregistered deltas (plan header, ledger #2):
//   (a) capability UP:  sky_capability = min(folk + 0.25, 1.0)
//   (b) prior reweight: doctrine prior = folk prior × schema.mediation
//   (c) β UP:           doctrine β = folk β + 0.5
//   (d) agent preference: bind the high-god belief where period-compatible,
//       else fall back to folk's own binding rule.
// Selection bias, never deception: nothing here models what the doctrine
// "believes" about the folk voice, and the folk voice itself (params,
// account, and every stream it draws) is untouched by this section.

/// The four-delta capability transform (delta a, ledger #2): `folk`'s own
/// sky-capability plus a flat `0.25`, capped at `1.0` — the records/
/// calendar-priesthood mechanism (MAP-18) that lets doctrine KEEP a fact
/// folk-alone loses. Every other [`AccountParams`] field is copied from
/// `folk` verbatim (`..folk.clone()`): the selection-bias law (this
/// campaign's dial-roster sibling) requires no hidden divergence beyond
/// this one preregistered field.
pub fn doctrine_params_of(folk: &AccountParams) -> AccountParams {
    AccountParams {
        sky_capability: (folk.sky_capability + 0.25).min(1.0),
        ..folk.clone()
    }
}

/// The four-delta β transform (delta c, ledger #2): folk's own β
/// ([`beta_of`]) plus a flat `0.5` — orthodoxy is monomaniac. Not an
/// [`AccountParams`] field (β lives outside that struct for both stacks);
/// this is the doctrine analog of [`beta_of`], never folded into
/// [`doctrine_params_of`].
/// type-audit: bare-ok(ratio: return)
pub fn doctrine_beta_of(psych: &PsychVector) -> f64 {
    beta_of(psych) + 0.5
}

/// Per-fact folk-verifiability (delta-adjacent, ledger #3): whether the
/// FOLK voice — never the doctrine voice — could in principle check a
/// predicate's fact, translated from `folk`'s own observability row per the
/// preregistered table: [`Requirement::Manifest`] is always verifiable;
/// [`Requirement::SkyGraded`] is verifiable iff `folk.sky_capability` meets
/// its threshold; [`Requirement::Instrumental`], [`Requirement::CrossReferential`],
/// and [`Requirement::Taxonomic`] are never verifiable. A predicate absent
/// from `folk`'s table is not verifiable (fail closed, mirroring
/// `disposition_for`'s own posture for an unregistered predicate). This is
/// the flag [`hornvale_language::schemas::conflict_of`]'s caller-derived
/// `folk_verifiable` parameter is built from.
/// type-audit: bare-ok(identifier-text: predicate), bare-ok(flag: return)
pub fn folk_verifiable(folk: &AccountParams, predicate: &str) -> bool {
    match folk.observability.get(predicate).map(|o| o.requirement) {
        Some(Requirement::Manifest) => true,
        Some(Requirement::SkyGraded { threshold }) => folk.sky_capability >= threshold,
        Some(
            Requirement::Instrumental | Requirement::CrossReferential | Requirement::Taxonomic,
        ) => false,
        None => false,
    }
}

/// `schema`'s mediation weight (delta b's per-row multiplier), or `1.0` if
/// `schema` somehow carries no row (never happens — [`schema_table`] is
/// exhaustive over [`SchemaId`], the closure test pins this — but a stray
/// future variant fails closed to a neutral weight rather than panicking).
fn mediation_of(schema: SchemaId) -> f64 {
    schema_table()
        .iter()
        .find(|row| row.id == schema)
        .map(|row| row.mediation)
        .unwrap_or(1.0)
}

/// The doctrine's schema prior (delta b, ledger #2): [`schema_prior`]'s
/// SAME folk-derived weights, each multiplied by its schema's authored
/// [`Schema::mediation`](hornvale_language::schemas::Schema::mediation)
/// column value — applied BEFORE β-sharpening, per the plan header
/// (`doctrine prior = folk prior × mediation`). The folk prior itself
/// ([`schema_prior`]) is never mutated; this wraps it.
fn doctrine_schema_prior(
    subsistence: Subsistence,
    sociality: Sociality,
    admitted: &[SchemaId],
) -> Vec<(SchemaId, f64)> {
    schema_prior(subsistence, sociality, admitted)
        .into_iter()
        .map(|(id, weight)| (id, weight * mediation_of(id)))
        .collect()
}

/// The day's doctrine binding (delta d, ledger #2): the culture's high-god
/// belief (`high_god_id`) if it is itself a cyclic belief whose re-derived
/// period matches `day` within the same 1% relative tolerance
/// [`explain_day`] uses; otherwise folk's OWN binding rule (the first
/// ascending-period cyclic belief whose period matches `day`) — the
/// doctrine never invents a binding folk's rule wouldn't also find on its
/// own. Returns the bound belief and its rank among `cyclic` (for
/// [`manner_of`]); `None` when neither rule finds a match.
fn doctrine_day_binding(
    cyclic: &[(hornvale_religion::Belief, f64)],
    high_god_id: Option<hornvale_kernel::EntityId>,
    day: f64,
) -> Option<(usize, &hornvale_religion::Belief)> {
    if let Some(hg_id) = high_god_id
        && let Some((rank, (belief, period))) =
            cyclic.iter().enumerate().find(|(_, (b, _))| b.id == hg_id)
        && (*period - day).abs() < 0.01 * day
    {
        return Some((rank, belief));
    }
    cyclic
        .iter()
        .enumerate()
        .find(|(_, (_, period))| (*period - day).abs() < 0.01 * day)
        .map(|(rank, (belief, _))| (rank, belief))
}

/// The moons' doctrine binding (delta d, ledger #2): the culture's high-god
/// belief if it is cyclic AND its period does NOT match `day` (the moons'
/// own compatibility condition — any non-day-matched cyclic belief will
/// do); otherwise folk's OWN binding rule (the slowest cyclic belief,
/// unconditionally — see [`explain_moons`]'s doc comment for why that rule
/// ignores which belief actually cleared the has-non-day-cycle guard).
/// Returns the bound belief and its rank among `cyclic`.
fn doctrine_moons_binding(
    cyclic: &[(hornvale_religion::Belief, f64)],
    high_god_id: Option<hornvale_kernel::EntityId>,
    day: f64,
) -> Option<(usize, &hornvale_religion::Belief)> {
    if let Some(hg_id) = high_god_id
        && let Some((rank, (belief, period))) =
            cyclic.iter().enumerate().find(|(_, (b, _))| b.id == hg_id)
        && (*period - day).abs() >= 0.01 * day
    {
        return Some((rank, belief));
    }
    let slowest_rank = cyclic.len().checked_sub(1)?;
    Some((slowest_rank, &cyclic[slowest_rank].0))
}

/// The doctrine's day explanation (mirrors [`explain_day`] structurally):
/// same fire guard (a `Lost` `day-length-std` entry) and same manner rule,
/// but the doctrine's own β ([`doctrine_beta_of`]), mediation-reweighted
/// prior ([`doctrine_schema_prior`]), high-god-preferring binding
/// ([`doctrine_day_binding`]), and a SEPARATE pair of streams
/// (`doctrine-schema`/`doctrine-lexeme`) so the doctrine's selection draws
/// never share or perturb a single draw the folk pass ([`explain_day`])
/// takes.
#[allow(clippy::too_many_arguments)]
fn doctrine_explain_day(
    world_seed: Seed,
    species: &str,
    account: &mut Account,
    params: &AccountParams,
    cyclic: &[(hornvale_religion::Belief, f64)],
    day: f64,
    beta: f64,
    subsistence: Subsistence,
    sociality: Sociality,
    high_god_id: Option<hornvale_kernel::EntityId>,
) {
    let predicate = hornvale_astronomy::facts::DAY_LENGTH_STD;
    if domain_of(params, predicate) != Some("sky") {
        return;
    }
    let Some(day_index) = account
        .entries
        .iter()
        .position(|e| e.fact.predicate == predicate)
    else {
        return;
    };
    if !matches!(account.entries[day_index].disposition, Disposition::Lost(_)) {
        return;
    }
    let Some((rank, belief)) = doctrine_day_binding(cyclic, high_god_id, day) else {
        return;
    };
    let manner = manner_of(rank, cyclic.len());

    let candidates = admitted(FactShape::CyclicEvent);
    let prior = doctrine_schema_prior(subsistence, sociality, &candidates);
    let mut schema_stream = world_seed
        .derive("language")
        .derive(species)
        .derive("doctrine-schema")
        .derive("sky")
        .derive(fact_shape_key(FactShape::CyclicEvent))
        .stream();
    let Some(schema) = select_schema(&prior, beta, &mut schema_stream) else {
        return;
    };

    let agent = bind_agent(schema, &belief.deity);
    let lexeme = if schema == SchemaId::Agentive {
        let lex_candidates = lexemes_for(SchemaId::Agentive, sub_frame_of(subsistence));
        let mut lexeme_stream = world_seed
            .derive("language")
            .derive(species)
            .derive("doctrine-lexeme")
            .derive(predicate)
            .stream();
        select_lexeme(lex_candidates, &mut lexeme_stream)
    } else {
        None
    };

    let underlying = account.entries[day_index].disposition.clone();
    account.entries[day_index].disposition = Disposition::Explained {
        underlying: Box::new(underlying),
        schema,
        agent,
        lexeme,
        manner,
    };
}

/// The doctrine's moons explanation (mirrors [`explain_moons`]
/// structurally): same fire guards (a `Kept` `moon-count` entry, and some
/// non-day-matched cyclic belief must exist), but the doctrine's own β,
/// mediation-reweighted prior, high-god-preferring binding
/// ([`doctrine_moons_binding`]), and the `doctrine-schema`/`doctrine-lexeme`
/// streams. Because doctrine's [`AccountParams::sky_capability`] is
/// capability-boosted (delta a), a `moon-count` entry LOST in the folk
/// account can be `Kept` here — the mechanism behind [`ConflictState::
/// RevealedClaim`](hornvale_language::schemas::ConflictState::RevealedClaim)
/// (Task 3 renders the surface; this function only ever asks "is it Kept
/// in THIS account").
#[allow(clippy::too_many_arguments)]
fn doctrine_explain_moons(
    world_seed: Seed,
    species: &str,
    account: &mut Account,
    params: &AccountParams,
    cyclic: &[(hornvale_religion::Belief, f64)],
    day: f64,
    beta: f64,
    subsistence: Subsistence,
    sociality: Sociality,
    high_god_id: Option<hornvale_kernel::EntityId>,
) {
    let predicate = hornvale_astronomy::facts::MOON_COUNT;
    if domain_of(params, predicate) != Some("sky") {
        return;
    }
    let Some(moon_index) = account
        .entries
        .iter()
        .position(|e| e.fact.predicate == predicate)
    else {
        return;
    };
    if account.entries[moon_index].disposition != Disposition::Kept {
        return;
    }
    let has_non_day_cycle = cyclic
        .iter()
        .any(|(_, period)| (*period - day).abs() >= 0.01 * day);
    if !has_non_day_cycle {
        return;
    }

    let Some((rank, belief)) = doctrine_moons_binding(cyclic, high_god_id, day) else {
        return;
    };
    let manner = manner_of(rank, cyclic.len());

    let candidates = admitted(FactShape::Count);
    let prior = doctrine_schema_prior(subsistence, sociality, &candidates);
    let mut schema_stream = world_seed
        .derive("language")
        .derive(species)
        .derive("doctrine-schema")
        .derive("sky")
        .derive(fact_shape_key(FactShape::Count))
        .stream();
    let Some(schema) = select_schema(&prior, beta, &mut schema_stream) else {
        return;
    };

    let agent = bind_agent(schema, &belief.deity);
    let lexeme = if schema == SchemaId::Agentive {
        let lex_candidates = lexemes_for(SchemaId::Agentive, sub_frame_of(subsistence));
        let mut lexeme_stream = world_seed
            .derive("language")
            .derive(species)
            .derive("doctrine-lexeme")
            .derive(predicate)
            .stream();
        select_lexeme(lex_candidates, &mut lexeme_stream)
    } else {
        None
    };

    let underlying = account.entries[moon_index].disposition.clone();
    account.entries[moon_index].disposition = Disposition::Explained {
        underlying: Box::new(underlying),
        schema,
        agent,
        lexeme,
        manner,
    };
}

/// Doctrine explanation assembly: mirrors [`explain`] (same unbindable
/// guard, same day/flagship/subsistence/psych derivation) but calls the
/// doctrine's own day/moons passes with the doctrine β and the culture's
/// high-god belief id (its presiding deity, per
/// [`hornvale_religion::Belief::high_god`] — `None` for an unranked
/// society, which simply leaves delta d's preference inert and falls
/// through to folk's binding rule every time).
fn doctrine_explain(world: &World, species: &str, account: &mut Account, params: &AccountParams) {
    let cyclic = cyclic_beliefs_of(world, species);
    if cyclic.is_empty() {
        return;
    }
    let Some(day) = day_length_std_value(account) else {
        return;
    };
    let Some(flagship) = crate::flagship_of(world, species) else {
        return;
    };
    let Some(subsistence) = hornvale_culture::subsistence_of(world, flagship.id)
        .as_deref()
        .and_then(subsistence_from_name)
    else {
        return;
    };
    let Ok(wc) = WorldComponents::assemble() else {
        return;
    };
    let Some(psych) = wc.psyche.get_by_label(species) else {
        return;
    };
    let beta = doctrine_beta_of(psych);
    let high_god_id = hornvale_religion::beliefs_held_by(world, flagship.id)
        .into_iter()
        .find(|b| b.high_god)
        .map(|b| b.id);

    doctrine_explain_day(
        world.seed,
        species,
        account,
        params,
        &cyclic,
        day,
        beta,
        subsistence,
        psych.sociality,
        high_god_id,
    );
    doctrine_explain_moons(
        world.seed,
        species,
        account,
        params,
        &cyclic,
        day,
        beta,
        subsistence,
        psych.sociality,
        high_god_id,
    );
}

/// The institution's voice (C6): one organized culture's doctrine params
/// and the doctrine account those params produce over the SAME ground
/// truth folk's account reads ([`chorus_ground`]) — the folk ground through
/// the doctrine stack (delta a-d), never a second draw over new furniture.
/// A distinct type from [`ChorusVoice`] by construction: the dial-roster
/// law (ledger #4) requires this type never appear in [`accounts_of`]'s
/// return type, only in [`doctrines_of`]'s.
/// type-audit: bare-ok(identifier-text: kind)
#[derive(Debug)]
pub struct DoctrineVoice {
    /// The people's kind label (e.g. `"goblin"`).
    pub kind: String,
    /// The params [`doctrine_params_of`] derived for this kind.
    pub params: AccountParams,
    /// The account [`account_of`] produced from those params over
    /// [`chorus_ground`], then wrapped by [`doctrine_explain`].
    pub account: Account,
}

/// The SOC-1 gate (ledger #1): `species` gains a doctrine voice iff its
/// flagship settlement's committed `cult-form` fact is `"organized"` (never
/// `"folk"`, and never when `species` places no flagship at all). Cult-form
/// is already committed state (`hornvale_religion::genesis`) — this gate
/// spends zero new facts, concepts, or draws to ask the question.
/// type-audit: bare-ok(identifier-text: species)
pub fn doctrine_of(world: &World, species: &str) -> Option<DoctrineVoice> {
    let flagship = crate::flagship_of(world, species)?;
    let cult_form = hornvale_religion::cult_form_held_by(world, flagship.id)?;
    if cult_form != "organized" {
        return None;
    }

    let folk_params = account_params_of(world, species).ok()?;
    let params = doctrine_params_of(&folk_params);
    let ground = chorus_ground(world);
    let mut account = account_of(&ground, &params);
    doctrine_explain(world, species, &mut account, &params);

    Some(DoctrineVoice {
        kind: species.to_string(),
        params,
        account,
    })
}

/// Every organized placed people's doctrine voice, in
/// [`crate::placed_peoples`] order — the SEPARATE collection the dial-
/// roster law (ledger #4) requires: [`accounts_of`] never gains an entry
/// from this pass, and this pass never feeds the dial's roster. A placed
/// people whose flagship is `"folk"` (the SOC-1 gate's negative arm) is
/// simply absent here, same posture as [`accounts_of`]'s own
/// param-derivation-failure skip.
pub fn doctrines_of(world: &World) -> Vec<DoctrineVoice> {
    crate::placed_peoples(world)
        .into_iter()
        .filter_map(|(kind, _village)| doctrine_of(world, kind))
        .collect()
}

/// Derive `species`'s [`AccountParams`] from existing authored/committed
/// state only: no new draws, no new ledger writes, no new save-format
/// state.
///
/// **Derived, never stored** (LANG-36): re-running this over the same
/// world and species always reconstructs the same `AccountParams`
/// byte-for-byte; a caller must never cache or serialize the result — the
/// world's registry/psyche/perception components and the ledger's
/// committed facts are the only durable state.
///
/// - `holdings`: every concept [`crate::exposure_of`] classes `Steeped` or
///   `KnowsOf` (a `KnowsOf` compound is still a held word).
/// - `sky_capability`: [`sky_capability`] over the species' perception
///   vector.
/// - `order`: `Salience { sky_first: sky_attention >= 0.6 }`.
/// - `stances`: for each placed people (`k`), `Ourselves` for `species`
///   itself; otherwise, read from `species`'s OWN psychology (not `k`'s) —
///   `in_group_radius >= 0.5` → `Neighbors`, else `threat_response >= 0.5` →
///   `Rivals`, else `Strangers`.
/// - `world_carving`: `Some("earth")` iff holdings contain `"earth"` (read
///   from holdings, never hardcoded — the universal stratum sits at the
///   floor for every culture today, but this must track holdings, not
///   assume them).
/// - `hold_all: false` always; the real [`observability_table`].
///
/// type-audit: bare-ok(identifier-text: species)
pub fn account_params_of(world: &World, species: &str) -> Result<AccountParams, BuildError> {
    let wc = WorldComponents::assemble()?;
    let perception = wc.perception.get_by_label(species).ok_or_else(|| {
        BuildError::MalformedKind(format!(
            "'{species}' carries no perception component (not a peopled kind)"
        ))
    })?;
    let psyche = wc.psyche.get_by_label(species).ok_or_else(|| {
        BuildError::MalformedKind(format!(
            "'{species}' carries no psyche component (not a peopled kind)"
        ))
    })?;

    let exposure = crate::exposure_of(world, species)?;
    let holdings: BTreeSet<String> = exposure
        .iter()
        .filter(|(_, class)| {
            matches!(
                class,
                hornvale_language::ExposureClass::Steeped
                    | hornvale_language::ExposureClass::KnowsOf
            )
        })
        .map(|(concept, _)| concept.clone())
        .collect();

    let mut stances = BTreeMap::new();
    for (kind, _village) in crate::placed_peoples(world) {
        let stance = if kind == species {
            Stance::Ourselves
        } else if psyche.in_group_radius >= 0.5 {
            Stance::Neighbors
        } else if psyche.threat_response >= 0.5 {
            Stance::Rivals
        } else {
            Stance::Strangers
        };
        stances.insert(kind.to_string(), stance);
    }

    let world_carving = holdings.contains("earth").then(|| "earth".to_string());

    Ok(AccountParams {
        hold_all: false,
        holdings,
        observability: observability_table(),
        sky_capability: sky_capability(perception),
        order: OrderPolicy::Salience {
            sky_first: perception.sky_attention >= 0.6,
        },
        stances,
        world_carving,
    })
}

/// The gibberish pole (the dial's pathological end, deterministic
/// build-state, independent of any specific world): `hold_all: false`,
/// empty holdings, the real table with every requirement rewritten —
/// `Taxonomic` for the three text-valued predicates (`is-a`, `star-class`,
/// `instance-of`), `Instrumental` for the two numeric ones (`moon-count`,
/// `day-length-std`) — `sky_capability: 0.0`, `order: Salience { sky_first:
/// true }`, every peopled kind's stance `Rivals`, `world_carving:
/// Some("earth")`. Every text-valued fact substitutes to the SAME target
/// under `Taxonomic` + `world_carving: Some("earth")` — an injectivity
/// collision that drives [`hornvale_language::recoverability`] to 0.0 —
/// while both numeric facts are simply beyond capability.
pub fn pathological_params() -> AccountParams {
    let mut observability = observability_table();
    for (predicate, obs) in observability.iter_mut() {
        let text_valued = predicate == IS_A
            || predicate == hornvale_astronomy::facts::STAR_CLASS
            || predicate == hornvale_kernel::INSTANCE_OF;
        obs.requirement = if text_valued {
            Requirement::Taxonomic
        } else {
            Requirement::Instrumental
        };
    }

    let stances = hornvale_species::psyche_registry()
        .ids()
        .map(|k| (k.0.to_string(), Stance::Rivals))
        .collect();

    AccountParams {
        hold_all: false,
        holdings: BTreeSet::new(),
        observability,
        sky_capability: 0.0,
        order: OrderPolicy::Salience { sky_first: true },
        stances,
        world_carving: Some("earth".to_string()),
    }
}

/// Resolve `entity`'s ground-fact subject text: its resolved `name`, or the
/// `Entity {id}` fallback — mirrors `windows/book::render_volume`'s subject
/// resolution exactly (see the module doc's layering note on why this
/// can't just call that function).
fn subject_name(world: &World, entity: hornvale_kernel::EntityId) -> String {
    world
        .ledger
        .text_of(entity, hornvale_kernel::NAME)
        .map(str::to_string)
        .unwrap_or_else(|| format!("Entity {}", entity.0))
}

/// The world's ground truth, flattened to [`GroundFact`]s for [`account_of`]
/// — mirrors `windows/book::render_volume`'s fact selection exactly (same
/// facts, same order; Task 4's null-filter-law test pins the two against
/// each other): one `is-a` `GroundFact` per classified subject, then one
/// `GroundFact` per [`hornvale_astronomy::facts`] construction predicate
/// with a committed value on that subject (moon-count, star-class,
/// day-length-std — `render_volume`'s `CONSTRUCTION_ORDER`, copied here
/// since `windows/book` cannot be imported from `windows/worldgen`,
/// layering runs the other way), then one `GroundFact` per `instance-of`
/// fact (subject = the collective's resolved name, no "The " prefix; object
/// = the kind text).
pub fn chorus_ground(world: &World) -> Vec<GroundFact> {
    // The mirror obligation: this order must match
    // `windows/book::render_volume`'s `CONSTRUCTION_ORDER` exactly.
    const CONSTRUCTION_ORDER: [&str; 3] = [
        hornvale_astronomy::facts::MOON_COUNT,
        hornvale_astronomy::facts::STAR_CLASS,
        hornvale_astronomy::facts::DAY_LENGTH_STD,
    ];

    let mut ground = Vec::new();
    for fact in world.ledger.find(IS_A) {
        let hornvale_kernel::Value::Text(kind) = &fact.object else {
            continue;
        };
        let subject_entity = fact.subject;
        let name = subject_name(world, subject_entity);
        ground.push(GroundFact {
            subject: name.clone(),
            predicate: IS_A.to_string(),
            object: hornvale_kernel::Value::Text(kind.clone()),
        });
        for predicate in CONSTRUCTION_ORDER {
            if let Some(value) = world.ledger.value_of(subject_entity, predicate) {
                ground.push(GroundFact {
                    subject: name.clone(),
                    predicate: predicate.to_string(),
                    object: value.clone(),
                });
            }
        }
    }

    for fact in world.ledger.find(hornvale_kernel::INSTANCE_OF) {
        let hornvale_kernel::Value::Text(kind) = &fact.object else {
            continue;
        };
        let subject_entity = fact.subject;
        let name = subject_name(world, subject_entity);
        ground.push(GroundFact {
            subject: name,
            predicate: hornvale_kernel::INSTANCE_OF.to_string(),
            object: hornvale_kernel::Value::Text(kind.clone()),
        });
    }

    ground
}

/// One placed people's rendered account: its kind label, the derived
/// [`AccountParams`] that produced it, and the resulting [`Account`].
/// type-audit: bare-ok(identifier-text: kind)
#[derive(Debug)]
pub struct ChorusVoice {
    /// The people's kind label (e.g. `"goblin"`).
    pub kind: String,
    /// The params [`account_params_of`] derived for this kind.
    pub params: AccountParams,
    /// The account [`account_of`] produced from those params over
    /// [`chorus_ground`].
    pub account: Account,
}

/// Every placed people's account over the world's ground truth, in
/// [`crate::placed_peoples`] order. Mirrors `render_volume`'s posture on a
/// param-derivation failure: skip that kind rather than fail the whole
/// pass (`else { continue }`).
pub fn accounts_of(world: &World) -> Vec<ChorusVoice> {
    let ground = chorus_ground(world);
    crate::placed_peoples(world)
        .into_iter()
        .filter_map(|(kind, _village)| {
            let params = account_params_of(world, kind).ok()?;
            let mut account = account_of(&ground, &params);
            explain(world, kind, &mut account, &params);
            Some(ChorusVoice {
                kind: kind.to_string(),
                params,
                account,
            })
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_language::LossReason;

    /// The unbindable guard (ledger #2), driven directly: `explain` is
    /// private, so this lives here rather than in the integration test
    /// crate. A measured sweep over live worlds (seeds 1..=60, every
    /// placed culture) found NO real case where a placed culture's
    /// pantheon lacked a day-matched cyclic belief — the sun's own
    /// day-cycle phenomenon is salient enough to enter any non-empty
    /// pantheon at the floor, so the guard's *natural* trigger is a
    /// species with no pantheon at all (a bare `World::new`, no
    /// settlement/religion genesis ever run: `flagship_of` returns `None`,
    /// so `cyclic_beliefs_of` is empty). This exercises that exact path
    /// and asserts the day entry stays plain `Lost` — never gains an
    /// `Explained` wrapper — when there is no cyclic belief to bind to.
    #[test]
    fn unbindable_cultures_stay_plain_lost() {
        let world = World::new(Seed(1));
        assert!(
            cyclic_beliefs_of(&world, "goblin").is_empty(),
            "a bare world places no settlement, so there is no flagship to hold a pantheon"
        );

        let ground = vec![GroundFact {
            subject: "Vebe".to_string(),
            predicate: hornvale_astronomy::facts::DAY_LENGTH_STD.to_string(),
            object: hornvale_kernel::Value::Number(1.5),
        }];
        let mut params = observability_table_params();
        params.holdings.insert("sun".to_string());
        let mut account = account_of(&ground, &params);
        assert_eq!(
            account.entries[0].disposition,
            Disposition::Lost(LossReason::BeyondCapability { domain: "sky" }),
            "day-length-std is CrossReferential — always Lost before explain runs"
        );

        explain(&world, "goblin", &mut account, &params);

        assert_eq!(
            account.entries[0].disposition,
            Disposition::Lost(LossReason::BeyondCapability { domain: "sky" }),
            "no cyclic belief at all -> the guard must leave the day entry untouched"
        );
    }

    /// Review carry-over fix (C5 T4): `bind_agent` must bind a deity for
    /// every schema `windows/book`'s Task 4 frame table actually names one
    /// in — the two `Count`-shape agent-bearing schemas the day never
    /// draws (`Kinship`, `LinkSympathy`) plus `Agentive` itself. Forced
    /// directly (rather than through a live weighted draw) with both a
    /// "moons" style deity (the slowest cyclic belief) and a "day" style
    /// deity (the period-matched belief) to show `bind_agent` binds
    /// correctly regardless of which caller supplies the name — it only
    /// decides WHETHER to bind, never which belief does.
    #[test]
    fn bind_agent_covers_every_deity_bearing_schema() {
        assert_eq!(
            bind_agent(SchemaId::Agentive, "Nggo"),
            Some("Nggo".to_string()),
            "Agentive is SlotKind::Agent"
        );
        assert_eq!(
            bind_agent(SchemaId::Kinship, "Nggo"),
            Some("Nggo".to_string()),
            "Kinship is SlotKind::Kin (moons: the slowest cyclic belief's deity)"
        );
        assert_eq!(
            bind_agent(SchemaId::Kinship, "Vamu"),
            Some("Vamu".to_string()),
            "Kinship (day: the period-matched belief's deity)"
        );
        assert_eq!(
            bind_agent(SchemaId::LinkSympathy, "Nggo"),
            Some("Nggo".to_string()),
            "LinkSympathy is SlotKind::Agent in schema_table (C6 T1 made the table honest) \
             and its windows/book surface frame always names a deity"
        );
    }

    /// Every schema `schema_table()` marks agentless (`SlotKind::None`) must
    /// still bind no agent — the fix must not overshoot into synthesizing
    /// agents for schemas whose frame never names one (the "no synthetic
    /// agents, ever" constraint).
    #[test]
    fn bind_agent_stays_none_for_every_other_schema() {
        for schema in [
            SchemaId::ForceDynamics,
            SchemaId::SubstanceFlow,
            SchemaId::Container,
            SchemaId::PathJourney,
            SchemaId::Balance,
            SchemaId::MoralAccounting,
            SchemaId::CycleReturn,
            SchemaId::EssenceTelos,
            SchemaId::Verticality,
        ] {
            assert_eq!(
                bind_agent(schema, "Nggo"),
                None,
                "{schema:?} must stay agentless"
            );
        }
    }

    /// A synthetic [`hornvale_religion::Belief`] for the doctrine-binding
    /// unit tests below: `id` must be distinct per belief in a single test
    /// (it's the only field [`doctrine_day_binding`]/[`doctrine_moons_binding`]
    /// match the high-god id against).
    fn belief(id: u64, deity: &str, high_god: bool) -> hornvale_religion::Belief {
        hornvale_religion::Belief {
            id: hornvale_kernel::EntityId::new(id).expect("nonzero test id"),
            deity: deity.to_string(),
            epithet: String::new(),
            source_kind: String::new(),
            sentiment: hornvale_religion::Sentiment::Cyclic,
            high_god,
        }
    }

    /// Delta d's preference branch, driven directly (ledger #2): no live
    /// seed measured for Task 2 happened to carry BOTH a high-god belief
    /// AND a period-compatible day match (seed 1 goblin, the integration
    /// test's measured case, has no high-god belief at all), so this
    /// exercises the actual preference-over-fallback distinction with a
    /// constructed pantheon where the two rules would pick DIFFERENT
    /// beliefs if the preference weren't checked first.
    #[test]
    fn doctrine_day_binding_prefers_the_period_matched_high_god() {
        let other = belief(1, "Nggo", false);
        let hg = belief(2, "Vamu", true);
        let day = 10.0;
        // Ascending-period order (cyclic_beliefs_of's own contract): the
        // fallback rule (first period match, ascending) would find `other`
        // at rank 0 — but the high god (rank 1, also period-compatible)
        // must be preferred instead.
        let cyclic = vec![(other.clone(), 10.0), (hg.clone(), 10.05)];

        let bound = doctrine_day_binding(&cyclic, Some(hg.id), day).expect("must bind");
        assert_eq!(
            bound.1.deity, "Vamu",
            "the period-compatible high god must win"
        );
        assert_eq!(bound.0, 1, "the high god's own rank among cyclic beliefs");

        // The SAME list, fallback rule only (no high-god id): finds the
        // rank-0 match instead — proof the two rules really do disagree
        // here, and the preference function chooses the high god.
        let fallback = doctrine_day_binding(&cyclic, None, day).expect("must bind via fallback");
        assert_eq!(fallback.1.deity, "Nggo");
        assert_eq!(fallback.0, 0);
    }

    #[test]
    fn doctrine_day_binding_falls_back_when_the_high_god_is_not_period_compatible() {
        let matched = belief(1, "Nggo", false);
        let hg = belief(2, "Vamu", true);
        let day = 10.0;
        let cyclic = vec![(matched.clone(), 10.0), (hg.clone(), 500.0)];

        let bound =
            doctrine_day_binding(&cyclic, Some(hg.id), day).expect("must bind via fallback");
        assert_eq!(
            bound.1.deity, "Nggo",
            "the high god is far off-period; folk's rule governs"
        );
        assert_eq!(bound.0, 0);
    }

    #[test]
    fn doctrine_day_binding_is_none_without_any_period_match() {
        let hg = belief(1, "Vamu", true);
        let cyclic = vec![(hg.clone(), 500.0)];
        assert!(doctrine_day_binding(&cyclic, Some(hg.id), 10.0).is_none());
        assert!(doctrine_day_binding(&cyclic, None, 10.0).is_none());
    }

    #[test]
    fn doctrine_moons_binding_prefers_the_non_day_matched_high_god() {
        let slowest = belief(1, "Nggo", false);
        let hg = belief(2, "Vamu", true);
        let day = 10.0;
        // The high god (rank 0) is NOT the slowest belief (rank 1) and its
        // own period doesn't match day — the moons' compatibility
        // condition — so it must be preferred over folk's unconditional
        // slowest-belief fallback.
        let cyclic = vec![(hg.clone(), 50.0), (slowest.clone(), 900.0)];

        let bound = doctrine_moons_binding(&cyclic, Some(hg.id), day).expect("must bind");
        assert_eq!(
            bound.1.deity, "Vamu",
            "the non-day-matched high god must win"
        );
        assert_eq!(bound.0, 0);

        let fallback = doctrine_moons_binding(&cyclic, None, day).expect("must bind via fallback");
        assert_eq!(
            fallback.1.deity, "Nggo",
            "fallback rule only: the unconditional slowest belief"
        );
        assert_eq!(fallback.0, 1);
    }

    #[test]
    fn doctrine_moons_binding_falls_back_when_the_high_god_is_day_matched() {
        let hg = belief(1, "Vamu", true);
        let slowest = belief(2, "Nggo", false);
        let day = 10.0;
        let cyclic = vec![(hg.clone(), 10.0), (slowest.clone(), 900.0)];

        let bound =
            doctrine_moons_binding(&cyclic, Some(hg.id), day).expect("must bind via fallback");
        assert_eq!(
            bound.1.deity, "Nggo",
            "the high god is day-matched (not moons-compatible); folk's slowest-belief rule governs"
        );
        assert_eq!(bound.0, 1);
    }

    /// A minimal [`AccountParams`] built from the real [`observability_table`],
    /// for tests that only need the day-length-std row's gate to behave
    /// exactly as [`account_params_of`] would derive it.
    fn observability_table_params() -> AccountParams {
        AccountParams {
            hold_all: false,
            holdings: BTreeSet::new(),
            observability: observability_table(),
            sky_capability: 1.0,
            order: OrderPolicy::Ground,
            stances: BTreeMap::new(),
            world_carving: None,
        }
    }
}
