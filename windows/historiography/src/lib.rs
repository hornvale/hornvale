//! Historiography, tier 0: recount how any entity came to be, by replaying
//! its committed facts, their provenance, and the registry's predicate docs.
//! Domain-agnostic — it interprets no domain-specific predicate, so a new
//! domain's facts are recounted the day they are committed. This is the seam
//! the Year-2 event ledger and fields-of-history deepen.
#![warn(missing_docs)]

use hornvale_kernel::{EntityId, Value, World};

/// Render a value for a recount line.
fn render_value(value: &Value) -> String {
    match value {
        Value::Text(t) => t.clone(),
        Value::Number(n) => n.to_string(),
        Value::Flag(b) => b.to_string(),
        Value::Entity(e) => format!("entity {}", e.0),
    }
}

/// The predicate-namespace prefix that marks a fact as an *errand*: a
/// creature's stated reason for setting out, committed once per errand rather
/// than once per step (The Warrant, spec §4.2). Keying on the prefix keeps
/// this window domain-agnostic — `errand/water-blind` and its seven siblings
/// are registry data, and the reader-facing words are the doc
/// `register_predicate` recorded for each one, exactly as for every other
/// predicate this file renders.
/// type-audit: bare-ok(identifier-text)
pub const ERRAND_PREFIX: &str = "errand/";

/// One errand and the steps committed under it, as indices into the subject's
/// fact list.
struct Errand {
    /// Index of the `errand/*` fact itself: its object is the ORIGIN the
    /// creature set out from, its day the day it set out.
    opened: usize,
    /// Indices of the steps it covers, in commit order.
    steps: Vec<usize>,
}

/// A recount's fact list, after the errand pass — indices into that list.
enum Item {
    /// A fact belonging to no errand: rendered exactly as it always was.
    Plain(usize),
    /// An errand and the steps it covers.
    Errand(Errand),
}

/// Group a subject's facts into errands and everything else.
///
/// **The rule is structural, never domain-specific.** An `errand/`-prefixed
/// fact opens a group and stays open until the NEXT errand fact. A later fact
/// joins it only if it shares the errand's `provenance` *and* the predicate
/// the group's first step established — so a producer's run of position facts
/// is absorbed and anything else (a `drank` from another producer, a
/// hand-planted `harness-placement` position) is `Plain` and renders
/// byte-identically to what it rendered before this pass existed. Nothing here
/// names `agent-at`, or any other domain predicate.
///
/// **An interleaved fact does NOT close the open errand, and this is the one
/// place the implementation departs from the shape the task brief sketched.**
/// Closing on any other predicate was tried first and measured on seed 23 over
/// 40 waits: a creature eats or sleeps *while walking*, so a `grazed` fact
/// landing between two steps of one errand orphaned every step after it — they
/// rendered with no reason at all, which is the exact regression this campaign
/// exists to remove. Staying open is also what the producer already asserts:
/// `windows/vessel`'s own coverage test defines a step's covering errand as
/// the latest errand at or before the step's day, with no notion of an errand
/// being interrupted. The cost is that a rolled-up errand's day span may
/// enclose plain lines printed below it; the reader is told the span, so it
/// reads as "and this happened along the way".
///
/// A step committed before any errand — or by another producer — belongs to
/// no group and is never attributed to an errand it did not come from.
fn group(facts: &[&hornvale_kernel::Fact]) -> Vec<Item> {
    let mut items: Vec<Item> = Vec::new();
    let mut open: Option<usize> = None;
    for (ix, f) in facts.iter().enumerate() {
        if f.predicate.starts_with(ERRAND_PREFIX) {
            items.push(Item::Errand(Errand {
                opened: ix,
                steps: Vec::new(),
            }));
            open = Some(items.len() - 1);
            continue;
        }
        let joins = match open.map(|i| &items[i]) {
            Some(Item::Errand(e)) => {
                f.provenance == facts[e.opened].provenance
                    && e.steps
                        .first()
                        .is_none_or(|s| facts[*s].predicate == f.predicate)
            }
            _ => false,
        };
        match open {
            Some(i) if joins => {
                if let Item::Errand(e) = &mut items[i] {
                    e.steps.push(ix);
                }
            }
            _ => items.push(Item::Plain(ix)),
        }
    }
    items
}

/// The registry doc for a predicate, falling back to the predicate key — the
/// one prose source a recount renders for any predicate.
fn label(world: &World, predicate: &str) -> String {
    world
        .registry
        .predicate(predicate)
        .map(|p| p.doc.clone())
        .unwrap_or_else(|| predicate.to_string())
}

/// A fact rendered the way every recount has always rendered one: the
/// registry doc, the value, the asserting system, and the day when it carries
/// one.
fn plain_line(world: &World, f: &hornvale_kernel::Fact) -> String {
    let label = label(world, &f.predicate);
    match f.day {
        Some(day) => format!(
            "- {label}: {} (asserted by {}, day {})\n",
            render_value(&f.object),
            f.provenance,
            day.as_std_days()
        ),
        None => format!(
            "- {label}: {} (asserted by {})\n",
            render_value(&f.object),
            f.provenance
        ),
    }
}

/// The rolled-up line for one errand: named ONCE, with its origin, how many
/// steps it took, the days it spanned and where it left the creature.
fn errand_line(world: &World, facts: &[&hornvale_kernel::Fact], e: &Errand) -> String {
    let opened = facts[e.opened];
    let gloss = label(world, &opened.predicate);
    let origin = render_value(&opened.object);
    let first = e.steps.iter().find_map(|s| facts[*s].day);
    let last = e.steps.iter().rev().find_map(|s| facts[*s].day);
    let n = e.steps.len();
    if n == 0 {
        return match opened.day {
            Some(day) => format!(
                "- {gloss}: from {origin} — set out on day {}, no steps recorded\n",
                day.as_std_days()
            ),
            None => format!("- {gloss}: from {origin} — no steps recorded\n"),
        };
    }
    let ending = render_value(&facts[e.steps[n - 1]].object);
    match (first, last) {
        (Some(a), Some(b)) if n > 1 => format!(
            "- {gloss}: from {origin} — {n} steps, days {} to {}, ending at {ending}\n",
            a.as_std_days(),
            b.as_std_days()
        ),
        (Some(a), _) => format!(
            "- {gloss}: from {origin} — 1 step, day {}, ending at {ending}\n",
            a.as_std_days()
        ),
        // Every step this repo commits is dated; an undated one would be a
        // new shape, and it drops the span rather than inventing one.
        _ => {
            let steps = if n == 1 {
                "1 step".to_string()
            } else {
                format!("{n} steps")
            };
            format!("- {gloss}: from {origin} — {steps}, ending at {ending}\n")
        }
    }
}

/// The lead line of a recount: the entity's `name` fact, or a bare id.
fn lead(world: &World, entity: EntityId) -> String {
    world
        .ledger
        .text_of(entity, hornvale_kernel::NAME)
        .map(str::to_string)
        .unwrap_or_else(|| format!("entity {}", entity.0))
}

/// Recount an entity from its committed facts: a lead line naming it (by its
/// `name` fact if present), then one bullet per fact — the predicate's
/// registry doc (falling back to the predicate key), the rendered value, the
/// system that asserted it, and — when the fact carries one (`Fact.day` is a
/// typed `Option<WorldTime>`) — the sim day it was asserted on. The
/// day is what makes a recount of a *non-functional*, dated predicate (an
/// NPC's `agent-at`, one fact per position change) legible: without it, every
/// position an agent has ever held reads as an undated, unordered pile
/// ("the herder was at the river" — which time?); with it, the recount is a
/// timeline ("the herder was at the river on day 5"). `None` if nothing is
/// recorded about `entity`.
///
/// **An errand and its steps roll up into ONE line** (The Warrant, spec §5.1).
/// A walking creature commits an `errand/*` fact naming why it set out, then
/// one position fact per step under it; before this roll-up a twelve-day walk
/// rendered forty-odd consecutive lines whose only varying part was a room
/// number and a clock, every one of them carrying the identical parenthetical.
/// The roll-up names the errand once — its gloss, its origin, its step count,
/// its day span and where it ended — and [`recount_steps`] is the per-step view
/// for a reader who wants every one. The grouping is structural (see `group`):
/// a fact belonging to no errand renders exactly as it did before this pass
/// existed, so `recount` stays predicate-blind for everything else.
/// type-audit: bare-ok(artifact: return)
pub fn recount(world: &World, entity: EntityId) -> Option<String> {
    let facts: Vec<&hornvale_kernel::Fact> = world.ledger.facts_about(entity).collect();
    if facts.is_empty() {
        return None;
    }
    let mut out = format!("{}:\n", lead(world, entity));
    for item in group(&facts) {
        match item {
            Item::Plain(ix) => out.push_str(&plain_line(world, facts[ix])),
            Item::Errand(e) => out.push_str(&errand_line(world, &facts, &e)),
        }
    }
    Some(out)
}

/// The per-step recount (The Warrant, spec §5.2): one line per fact, in commit
/// order exactly as [`recount`] rendered them before the roll-up existed, but
/// every step of an errand now carries its covering errand's gloss and its
/// position WITHIN that errand — information the pre-errand ledger could not
/// express at all, because a step's provenance named a reason without knowing
/// which run of steps shared it.
///
/// An errand's own fact contributes no line of its own here: its gloss is
/// already on each step it covers. The exception is an errand with no steps at
/// all, which renders its roll-up line so that nothing is silently dropped. A
/// step with no covering errand — a hand-planted `harness-placement` position,
/// or a position committed before the first errand — renders exactly as
/// [`recount`] renders it, naming its own provenance; it is never attributed
/// to an errand it did not come from. `None` if nothing is recorded about
/// `entity`.
/// type-audit: bare-ok(artifact: return)
pub fn recount_steps(world: &World, entity: EntityId) -> Option<String> {
    let facts: Vec<&hornvale_kernel::Fact> = world.ledger.facts_about(entity).collect();
    if facts.is_empty() {
        return None;
    }
    // The covering errand of each step, by fact index: which errand fact, how
    // far into it, and how long it ran. Built from the same grouping the
    // roll-up uses, then read back in commit order so this view's line order
    // is the ledger's, not the grouping's.
    let items = group(&facts);
    let mut cover: Vec<Option<(usize, usize, usize)>> = vec![None; facts.len()];
    let mut childless: Vec<Option<usize>> = vec![None; facts.len()];
    for (slot, item) in items.iter().enumerate() {
        if let Item::Errand(e) = item {
            if e.steps.is_empty() {
                childless[e.opened] = Some(slot);
                continue;
            }
            for (i, s) in e.steps.iter().enumerate() {
                cover[*s] = Some((e.opened, i + 1, e.steps.len()));
            }
        }
    }
    let mut out = format!("{}:\n", lead(world, entity));
    for (ix, f) in facts.iter().enumerate() {
        if let Some(slot) = childless[ix] {
            if let Item::Errand(e) = &items[slot] {
                out.push_str(&errand_line(world, &facts, e));
            }
            continue;
        }
        if f.predicate.starts_with(ERRAND_PREFIX) {
            continue;
        }
        match cover[ix] {
            Some((errand, i, n)) => {
                let gloss = label(world, &facts[errand].predicate);
                let label = label(world, &f.predicate);
                let value = render_value(&f.object);
                match f.day {
                    Some(day) => out.push_str(&format!(
                        "- {label}: {value} ({gloss} — step {i} of {n}, day {})\n",
                        day.as_std_days()
                    )),
                    None => {
                        out.push_str(&format!("- {label}: {value} ({gloss} — step {i} of {n})\n"))
                    }
                }
            }
            None => out.push_str(&plain_line(world, f)),
        }
    }
    Some(out)
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::test_lineage;
    use hornvale_kernel::{Fact, Seed, WorldTime};

    fn world() -> World {
        let mut w = World::new(Seed(42));
        w.registry
            .register_predicate("is-belief", true, "subject is a belief")
            .unwrap();
        w.registry
            .register_predicate("tenet", true, "the tenet text of a belief")
            .unwrap();
        w
    }

    fn fact(subject: EntityId, predicate: &str, object: Value, provenance: &str) -> Fact {
        Fact {
            subject,
            predicate: predicate.to_string(),
            object,
            place: None,
            day: Some(WorldTime::GENESIS),
            provenance: provenance.to_string(),
        }
    }

    #[test]
    fn recount_replays_facts_with_docs_and_provenance() {
        let mut w = world();
        let e = w
            .ledger
            .mint_entity(test_lineage(w.ledger.entity_count() as u16));
        w.ledger
            .commit(
                fact(
                    e,
                    hornvale_kernel::NAME,
                    Value::Text("the Ever-Flame".to_string()),
                    "religion",
                ),
                &w.registry,
            )
            .unwrap();
        w.ledger
            .commit(
                fact(e, "is-belief", Value::Flag(true), "religion"),
                &w.registry,
            )
            .unwrap();
        w.ledger
            .commit(
                fact(
                    e,
                    "tenet",
                    Value::Text("it never blinks.".to_string()),
                    "religion",
                ),
                &w.registry,
            )
            .unwrap();
        let text = recount(&w, e).expect("entity has facts");
        assert!(text.contains("the Ever-Flame"), "lead names the entity");
        assert!(
            text.contains("the tenet text of a belief"),
            "uses the registry doc"
        );
        assert!(text.contains("it never blinks."), "renders the value");
        assert!(text.contains("religion"), "names the provenance");
    }

    #[test]
    fn recount_is_none_for_an_unknown_entity() {
        let w = world();
        assert!(recount(&w, EntityId::new(999).unwrap()).is_none());
    }

    #[test]
    fn recount_names_the_day_a_dated_fact_was_asserted() {
        // THE PROVENANCE READ (the-quickening T4): a non-functional, dated
        // predicate (like an NPC's `agent-at`) is only legible as a timeline
        // if the recount names the day, not just the value. Mutation-verify:
        // reverting the `f.day` branch above to the undated format string
        // reds this test (it would no longer contain "day 5").
        let mut w = world();
        let e = w
            .ledger
            .mint_entity(test_lineage(w.ledger.entity_count() as u16));
        w.ledger
            .commit(
                Fact {
                    day: Some(WorldTime::from_std_days(5.0).expect("finite")),
                    ..fact(
                        e,
                        "tenet",
                        Value::Text("at the river".to_string()),
                        "the-quickening",
                    )
                },
                &w.registry,
            )
            .unwrap();
        let text = recount(&w, e).expect("entity has facts");
        assert!(
            text.contains("at the river (asserted by the-quickening, day 5)"),
            "recount names the day a dated fact was asserted: {text}"
        );
    }

    #[test]
    fn recount_omits_the_day_when_a_fact_carries_none() {
        // The day suffix must be conditional, not always-on: an undated fact
        // (day: None) should read exactly as it always has, with no
        // dangling "day" text.
        let mut w = world();
        let e = w
            .ledger
            .mint_entity(test_lineage(w.ledger.entity_count() as u16));
        w.ledger
            .commit(
                Fact {
                    day: None,
                    ..fact(e, "tenet", Value::Text("undated".to_string()), "religion")
                },
                &w.registry,
            )
            .unwrap();
        let text = recount(&w, e).expect("entity has facts");
        assert!(
            text.contains("undated (asserted by religion)\n"),
            "an undated fact renders with no day suffix: {text}"
        );
        assert!(!text.contains("day"), "no dangling day text: {text}");
    }

    /// A world with the two errand predicates and the position predicate this
    /// module's errand tests use, registered exactly as a possess session
    /// registers them: the reader-facing gloss is the registry doc.
    fn errand_world() -> World {
        let mut w = world();
        w.registry
            .register_predicate(
                "errand/water-blind",
                false,
                "wandered, having found no water yet (thirst)",
            )
            .unwrap();
        w.registry
            .register_predicate("errand/home", false, "walking home (sated)")
            .unwrap();
        w.registry
            .register_predicate("agent-at", false, "an agent's position on a day")
            .unwrap();
        w.registry
            .register_predicate("drank", false, "an agent satisfied its sustenance goal")
            .unwrap();
        w
    }

    /// One dated fact at `day`.
    fn dated(subject: EntityId, predicate: &str, object: &str, provenance: &str, day: f64) -> Fact {
        Fact {
            day: Some(WorldTime::from_std_days(day).expect("finite")),
            ..fact(
                subject,
                predicate,
                Value::Text(object.to_string()),
                provenance,
            )
        }
    }

    /// One errand of `n` steps, committed as the drive tick commits it: the
    /// errand fact naming the origin, then one position fact per step, all
    /// under the same producer.
    fn commit_walk(w: &mut World, e: EntityId, key: &str, origin: u64, n: usize, from_day: f64) {
        w.ledger
            .commit(
                dated(e, key, &origin.to_string(), "vessel/liveness", from_day),
                &w.registry.clone(),
            )
            .unwrap();
        for i in 0..n {
            w.ledger
                .commit(
                    dated(
                        e,
                        "agent-at",
                        &(origin + 1 + i as u64).to_string(),
                        "vessel/liveness",
                        from_day + (i as f64) * 0.5,
                    ),
                    &w.registry.clone(),
                )
                .unwrap();
        }
    }

    /// A walker with one three-step errand.
    fn walker(w: &mut World) -> EntityId {
        let e = w
            .ledger
            .mint_entity(test_lineage(w.ledger.entity_count() as u16));
        commit_walk(w, e, "errand/water-blind", 1000, 3, 5.0);
        e
    }

    /// THE ROLL-UP (The Warrant, spec §5.1). The gloss appears ONCE for the
    /// errand, not once per step — the regression this campaign exists to
    /// close, since every step's provenance is now the identical producer
    /// token and a per-step render repeats it verbatim on every line.
    #[test]
    fn a_recount_names_each_errand_once() {
        let mut w = errand_world();
        let e = walker(&mut w);
        let text = recount(&w, e).expect("the walker has facts");
        let gloss = "wandered, having found no water yet (thirst)";
        assert_eq!(
            text.matches(gloss).count(),
            1,
            "the gloss appears once per errand, not once per step:\n{text}"
        );
        assert!(
            text.contains("3 steps"),
            "the roll-up states the step count:\n{text}"
        );
        assert!(
            text.contains("from 1000"),
            "the roll-up names the origin the errand fact carries:\n{text}"
        );
        assert!(
            text.contains("days 5 to 6"),
            "the roll-up names the day span:\n{text}"
        );
        assert!(
            text.contains("ending at 1003"),
            "the roll-up names where the errand left the creature:\n{text}"
        );
        assert!(
            !text.contains("vessel/liveness"),
            "the producer token is not reader-facing prose and must not reach \
             the rolled-up view:\n{text}"
        );
    }

    /// THE PER-STEP VIEW (spec §5.2). One line per step, each resolving its
    /// covering errand — today's information plus a position-in-errand the
    /// pre-errand ledger could not express.
    #[test]
    fn the_step_view_resolves_each_steps_covering_errand() {
        let mut w = errand_world();
        let e = walker(&mut w);
        let text = recount_steps(&w, e).expect("the walker has facts");
        assert!(text.contains("step 2 of 3"), "{text}");
        assert!(
            text.contains("wandered, having found no water yet (thirst)"),
            "{text}"
        );
        assert_eq!(
            text.matches("an agent's position on a day").count(),
            3,
            "one line per step:\n{text}"
        );
        assert!(
            !text.contains("asserted by vessel/liveness"),
            "a covered step names its errand, never the bare producer:\n{text}"
        );
    }

    /// `recount` stays predicate-blind for everything else: a fact that is
    /// neither an errand nor a step under one renders byte-identically to the
    /// line it rendered before the errand pass existed. Pinned as a LITERAL,
    /// because comparing the renderer against itself would assert nothing.
    #[test]
    fn unrelated_predicates_render_unchanged() {
        let mut w = errand_world();
        let e = w
            .ledger
            .mint_entity(test_lineage(w.ledger.entity_count() as u16));
        commit_walk(&mut w, e, "errand/water-blind", 1000, 2, 5.0);
        w.ledger
            .commit(
                dated(
                    e,
                    "drank",
                    "true",
                    "drank from the river (thirst sated)",
                    6.5,
                ),
                &w.registry.clone(),
            )
            .unwrap();
        commit_walk(&mut w, e, "errand/home", 2000, 2, 7.0);
        let expected = "- an agent satisfied its sustenance goal: true \
                        (asserted by drank from the river (thirst sated), day 6.5)\n";
        for text in [
            recount(&w, e).expect("facts"),
            recount_steps(&w, e).expect("facts"),
        ] {
            assert!(
                text.contains(expected),
                "an unrelated predicate must render exactly as it always \
                 did.\nwanted: {expected}\ngot:\n{text}"
            );
        }
    }

    /// AN INTERLEAVED FACT DOES NOT ORPHAN THE REST OF AN ERRAND, and the
    /// alternative was measured rather than argued: closing the group on any
    /// other predicate (the shape this task was briefed with) was run against
    /// seed 23 over 40 waits, where a creature grazes and sleeps *while
    /// walking*, and every step after the interleaved fact rendered with no
    /// reason at all — the exact regression the campaign exists to remove.
    #[test]
    fn a_foreign_fact_mid_errand_does_not_orphan_the_steps_after_it() {
        let mut w = errand_world();
        let e = w
            .ledger
            .mint_entity(test_lineage(w.ledger.entity_count() as u16));
        w.ledger
            .commit(
                dated(e, "errand/water-blind", "1000", "vessel/liveness", 5.0),
                &w.registry.clone(),
            )
            .unwrap();
        w.ledger
            .commit(
                dated(e, "agent-at", "1001", "vessel/liveness", 5.1),
                &w.registry.clone(),
            )
            .unwrap();
        w.ledger
            .commit(
                dated(
                    e,
                    "drank",
                    "true",
                    "drank from the river (thirst sated)",
                    5.2,
                ),
                &w.registry.clone(),
            )
            .unwrap();
        w.ledger
            .commit(
                dated(e, "agent-at", "1002", "vessel/liveness", 5.3),
                &w.registry.clone(),
            )
            .unwrap();
        let text = recount(&w, e).expect("facts");
        assert!(
            text.contains("2 steps"),
            "both steps belong to the one errand that opened before them:\n{text}"
        );
        assert!(
            !text.contains("asserted by vessel/liveness"),
            "no step is left without a reason:\n{text}"
        );
        let steps = recount_steps(&w, e).expect("facts");
        assert!(steps.contains("step 2 of 2"), "{steps}");
    }

    /// A STEP NO ERRAND COVERS RENDERS SENSIBLY, never attributed to an
    /// errand it did not come from. `place_agent` commits exactly this shape —
    /// a position with provenance `harness-placement` and no errand at all —
    /// and a recount runs over arbitrary committed history.
    #[test]
    fn an_uncovered_step_renders_as_it_always_did() {
        let mut w = errand_world();
        let e = w
            .ledger
            .mint_entity(test_lineage(w.ledger.entity_count() as u16));
        w.ledger
            .commit(
                dated(e, "agent-at", "900", "harness-placement", 1.0),
                &w.registry.clone(),
            )
            .unwrap();
        commit_walk(&mut w, e, "errand/water-blind", 1000, 2, 5.0);
        for text in [
            recount(&w, e).expect("facts"),
            recount_steps(&w, e).expect("facts"),
        ] {
            assert!(
                text.contains(
                    "- an agent's position on a day: 900 (asserted by harness-placement, day 1)\n"
                ),
                "an uncovered step keeps its own provenance line:\n{text}"
            );
            assert!(
                !text.contains("900 (wandered"),
                "an uncovered step is never attributed to a later errand:\n{text}"
            );
        }
    }

    /// AN ERRAND WITH NO STEPS IS STILL NAMED, in both views: it is a real
    /// committed fact and dropping it would lose a reason the world recorded.
    #[test]
    fn an_errand_with_no_steps_is_still_named() {
        let mut w = errand_world();
        let e = w
            .ledger
            .mint_entity(test_lineage(w.ledger.entity_count() as u16));
        commit_walk(&mut w, e, "errand/home", 2000, 0, 9.0);
        for text in [
            recount(&w, e).expect("facts"),
            recount_steps(&w, e).expect("facts"),
        ] {
            assert!(
                text.contains(
                    "- walking home (sated): from 2000 — set out on day 9, no steps recorded\n"
                ),
                "a stepless errand is named, not dropped:\n{text}"
            );
        }
    }

    /// A ONE-STEP ERRAND READS AS ONE STEP, not "days 5 to 5" — seed 23's
    /// residents alternate errands roughly every two steps, so this is the
    /// common case there rather than an edge.
    #[test]
    fn a_one_step_errand_names_a_day_not_a_span() {
        let mut w = errand_world();
        let e = w
            .ledger
            .mint_entity(test_lineage(w.ledger.entity_count() as u16));
        commit_walk(&mut w, e, "errand/home", 2000, 1, 9.0);
        let text = recount(&w, e).expect("facts");
        assert!(
            text.contains("- walking home (sated): from 2000 — 1 step, day 9, ending at 2001\n"),
            "{text}"
        );
    }

    /// Both views are deterministic, the same way `recount` always was.
    #[test]
    fn the_step_view_is_deterministic() {
        let mut w = errand_world();
        let e = walker(&mut w);
        assert_eq!(recount_steps(&w, e), recount_steps(&w, e));
    }

    #[test]
    fn recount_is_deterministic() {
        let mut w = world();
        let e = w
            .ledger
            .mint_entity(test_lineage(w.ledger.entity_count() as u16));
        w.ledger
            .commit(
                fact(e, "tenet", Value::Text("x".to_string()), "religion"),
                &w.registry,
            )
            .unwrap();
        assert_eq!(recount(&w, e), recount(&w, e));
    }
}
