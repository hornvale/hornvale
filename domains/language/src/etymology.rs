//! The etymology engine: proto-roots drawn from a species' phonology, and a
//! drawn cascade of sound-change rules that turns a proto-root into its
//! modern form. `evolve` is the Neogrammarian core — a *pure, total*
//! function: every rule applies uniformly wherever its conditioning
//! environment occurs (no rule is ever sporadic/lexical), and a rule whose
//! output segment is not in the phonology's inventory applies as identity
//! (the codomain constraint made mechanical — the cascade always lands on
//! the shipped phonology, never invents an off-menu segment).
//!
//! `proto_root` draws from the same stem machinery [`crate::naming`] uses
//! (onset/nucleus/coda templates filled by picking from the phonology's
//! inventory), over its own seed-derivation path, via
//! [`crate::naming::Namer::draw_syllables`] and
//! [`crate::naming::segments_of`] — never constructing a
//! [`crate::phoneme::Segment`] itself, the same carry-forward invariant
//! naming relies on to keep the `"?"` fallback glyph unreachable.
#![warn(missing_docs)]

use crate::naming::Namer;
use crate::phoneme::{Height, Manner, Segment, Tone};
use crate::phonology::Phonology;
use crate::streams;
use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{Seed, Stream};

/// The closed family of sound-change rules a cascade may draw from. Each is
/// a total function on a segment (or, for the two structural rules, on a
/// word-position); see [`evolve`] for how a rule composes with the
/// inventory codomain constraint.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RuleKind {
    /// A voiceless stop becomes its voiced counterpart at the same place.
    Lenition,
    /// A fricative becomes a stop at the same place and voicing.
    Fortition,
    /// A vowel's height is raised (`param` 0) or lowered (`param` 1) one
    /// step.
    VowelShift,
    /// A two-consonant word-initial (onset) cluster drops its first
    /// segment.
    ClusterSimplify,
    /// A word-final coda consonant drops.
    FinalLoss,
    /// **Tonogenesis** (spec §4): the voicing of the consonant a prior
    /// merging rule ([`RuleKind::ClusterSimplify`] or [`RuleKind::FinalLoss`])
    /// dropped is reborn as a tone on the stranded nucleus — voiced-loss →
    /// [`crate::phoneme::Tone::Low`], voiceless-loss →
    /// [`crate::phoneme::Tone::High`] (the attested direction; nasals, being
    /// voiced, fall in the Low bucket). It is a *regular conditioned* change,
    /// not a homophony patch: it fires on every syllable whose onset/coda a
    /// merger removed, collision or not. Subject to the same codomain
    /// constraint as every rule — the toned vowel takes only if it is in the
    /// phonology's inventory, so an atonal language (Neutral-only vowels) sees
    /// it as identity. Reads the derivation's own step history (the dropped
    /// voicing), never a stream draw, so it stays a pure function of
    /// `(proto, cascade, ph)`.
    Tonogenesis,
}

/// One drawn sound rule: its kind, and `param` selecting the kind's drawn
/// variant (currently only meaningful to [`RuleKind::VowelShift`], which
/// reads 0 as "raise" and 1 as "lower"; every other kind ignores `param`).
/// The rule family is this closed enum, nothing extensible — `param` is not
/// a way to add new rule shapes.
/// type-audit: pending(wave-3: param)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SoundRule {
    /// Which rule this is.
    pub kind: RuleKind,
    /// The kind's drawn variant selector (see the field's use in
    /// [`RuleKind::VowelShift`]'s docs).
    pub param: u32,
}

/// A drawn sequence of 2–4 sound rules, applied in order by [`evolve`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Cascade {
    /// The rules, in application order.
    pub rules: Vec<SoundRule>,
}

/// One daughter's evolution machinery, for the **merger-aware** family
/// proto-assignment ([`assign_proto_roots`]): its drawn cascade and its own
/// phonology. The composition root supplies one per family member so the
/// assignment can evolve a candidate proto-root through every daughter and
/// reject a candidate that would produce a *confusable* (same-domain) modern
/// collision — the cognate-safe way to drive confusable core homophony to zero.
/// Plain data (`Cascade` + `Phonology`), so this crate stays kernel-only.
#[derive(Clone, Debug, PartialEq)]
pub struct Daughter {
    /// The daughter's drawn sound-change cascade.
    pub cascade: Cascade,
    /// The daughter's own phonology (its inventory nativizes inherited sounds).
    pub phonology: Phonology,
}

/// One rule's application to a whole word during [`evolve`]: the rule
/// itself, and whether it changed anything (a rule whose conditioning
/// environment never occurred, or whose every candidate output fell outside
/// the phonology's inventory, records `changed: false`).
/// type-audit: bare-ok(flag)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct AppliedRule {
    /// The rule that was applied.
    pub rule: SoundRule,
    /// Whether this rule changed the word.
    pub changed: bool,
}

/// A committed, replayable sound-change derivation: the drawn proto-root,
/// the cascade's step-by-step record, and the resulting modern form.
/// Replayable exactly: `evolve(d.proto, cascade, ph).modern == d.modern`
/// always holds, because `evolve` is pure.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Derivation {
    /// The proto-root `evolve` started from.
    pub proto: Vec<Segment>,
    /// The cascade's rules, each with whether it changed the word.
    pub steps: Vec<AppliedRule>,
    /// The resulting modern form.
    pub modern: Vec<Segment>,
}

/// The closed, canonically ordered list of rule kinds [`draw_cascade`] draws
/// from. [`RuleKind::Tonogenesis`] is the phonology epoch's addition, appended
/// last: it is drawn like any other rule (uniformly, one `param` consumed), and
/// its effect is gated by the codomain constraint — a tone-capable language's
/// inventory admits the toned vowel and the split takes; an atonal language's
/// does not, so it applies as identity. Adding it here reseeds every cascade in
/// every world (a save-format contract): the deliberate epoch bump, carried by
/// the campaign's full artifact + calibration re-baseline.
const RULE_KINDS: [RuleKind; 6] = [
    RuleKind::Lenition,
    RuleKind::Fortition,
    RuleKind::VowelShift,
    RuleKind::ClusterSimplify,
    RuleKind::FinalLoss,
    RuleKind::Tonogenesis,
];

/// The `param` range every drawn rule consumes, regardless of kind (only
/// [`RuleKind::VowelShift`] gives it meaning) — keeping the draw uniform
/// across kinds keeps stream consumption independent of which kind is
/// picked.
const RULE_PARAM_RANGE: (u32, u32) = (0, 1);

/// The inclusive range of rules a drawn cascade contains, as a passed-in
/// value rather than a global constant — the mechanism [`draw_cascade_with_regime`]
/// draws its rule count from. A settled, socially connected people drifts at
/// the historical rate ([`CascadeRegime::SETTLED`]); a long-lived solitary
/// has no one to drift *with*, so a later regime (the composition root's
/// `cascade_regime_of`, `windows/worldgen`) narrows this toward frozen. This
/// type is language-owned and knows nothing of `SocialForm` or any other
/// species concept — the regime is always computed elsewhere and passed in,
/// keeping this crate kernel-only.
/// type-audit: bare-ok(count: min), bare-ok(count: max)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CascadeRegime {
    /// The minimum number of rules a drawn cascade may contain, inclusive.
    pub min: u32,
    /// The maximum number of rules a drawn cascade may contain, inclusive.
    pub max: u32,
}

impl CascadeRegime {
    /// The historical rate every kind that speaks today draws at: 2–4
    /// rules. This is the pre-existing `CASCADE_LEN_RANGE` constant it
    /// replaces, preserved exactly so [`draw_cascade`]'s default stays
    /// byte-identical.
    pub const SETTLED: CascadeRegime = CascadeRegime { min: 2, max: 4 };

    /// Construct a regime from an inclusive `(min, max)` rule-count range.
    /// type-audit: bare-ok(count: min), bare-ok(count: max)
    pub const fn new(min: u32, max: u32) -> CascadeRegime {
        CascadeRegime { min, max }
    }
}

/// Draw one rule: a kind, then a param, in that order.
fn draw_rule(stream: &mut Stream) -> SoundRule {
    let kind = *stream
        .pick(&RULE_KINDS)
        .expect("RULE_KINDS is a fixed non-empty array");
    let param = stream.range_u32(RULE_PARAM_RANGE.0, RULE_PARAM_RANGE.1);
    SoundRule { kind, param }
}

/// Draw a cascade for `species` at the historical `SETTLED` rate (2–4 rules).
/// Convenience wrapper over
/// [`draw_cascade_with_regime`] at [`CascadeRegime::SETTLED`] (2–4 rules) —
/// the historical rate every existing caller drew at before `CascadeRegime`
/// existed. Byte-identical to the pre-regime `draw_cascade`: the stream
/// derivation is unchanged, only the rule-count bounds now flow through a
/// regime value instead of a bare constant.
/// type-audit: bare-ok(identifier-text)
pub fn draw_cascade(seed: &Seed, species: &str) -> Cascade {
    draw_cascade_with_regime(seed, species, CascadeRegime::SETTLED)
}

/// Draw a cascade for `species` whose rule count is bounded by `regime`,
/// from
/// `seed.derive(streams::ROOT).derive(StreamLabel::dynamic(species)).derive(streams::LEXICON).derive(streams::CASCADE)`.
/// The stream derivation is identical regardless of `regime` — only the
/// `range_u32` bounds it draws the rule count from change, so
/// `draw_cascade_with_regime(seed, species, CascadeRegime::SETTLED)` is
/// exactly [`draw_cascade`]'s behavior.
/// type-audit: bare-ok(identifier-text)
pub fn draw_cascade_with_regime(seed: &Seed, species: &str, regime: CascadeRegime) -> Cascade {
    let mut stream = seed
        .derive(streams::ROOT)
        .derive(StreamLabel::dynamic(species))
        .derive(streams::LEXICON)
        .derive(streams::CASCADE)
        .stream();
    let count = stream.range_u32(regime.min, regime.max);
    let rules = (0..count).map(|_| draw_rule(&mut stream)).collect();
    Cascade { rules }
}

/// The number of syllables a drawn proto-root spans.
const PROTO_ROOT_SYLLABLE_RANGE: (u32, u32) = (1, 2);

/// Draw a proto-root for `concept` under `species`'s phonology `ph`, from
/// `seed.derive(streams::ROOT).derive(StreamLabel::dynamic(species)).derive(streams::LEXICON).derive(streams::PROTO_ROOT).derive(StreamLabel::dynamic(concept))`.
/// 1–2 syllables, filled from `ph`'s phonotactic templates by the same
/// mechanism [`crate::naming::Namer`] uses to build names.
/// type-audit: bare-ok(identifier-text)
pub fn proto_root(seed: &Seed, species: &str, concept: &str, ph: &Phonology) -> Vec<Segment> {
    let mut stream = seed
        .derive(streams::ROOT)
        .derive(StreamLabel::dynamic(species))
        .derive(streams::LEXICON)
        .derive(streams::PROTO_ROOT)
        .derive(StreamLabel::dynamic(concept))
        .stream();
    let namer = Namer::new(seed, species, ph);
    let syllables = namer.draw_syllables(
        &mut stream,
        PROTO_ROOT_SYLLABLE_RANGE.0,
        PROTO_ROOT_SYLLABLE_RANGE.1,
        false,
    );
    crate::naming::segments_of(&syllables)
}

/// The epoch suffix for the injective proto-root assignment. The Words drew
/// each root independently at `language/<family>/lexicon/root/<concept>`; the
/// homophony campaign's draw-side fix retired that for a collision-resolved
/// assignment at `.../root/v2/<concept>`; `v3` is the **merger-aware**
/// assignment — the same open-addressing draw, now also rejecting a core
/// candidate whose *evolved* form would merge with an already-placed core
/// concept's in any daughter (driving core homophony to zero). Deliberate
/// regeneration uses an epoch suffix, never a rename — the save-format
/// contract — so `v3` reseeds every root and old saves' `v2` forms are gone by
/// design, regenerated with the world.
const ROOT_EPOCH: &str = "v3";

/// Assign a distinct proto-root to every concept in `concepts` under
/// `family`'s proto-phonology `proto_ph` — the injective, collision-resolved
/// replacement for per-concept [`proto_root`] drawing (the homophony fix,
/// draw side). Deterministic open-addressing: each concept draws a candidate
/// root at epoch `root/v3`, and on collision re-draws from a probe-keyed
/// sub-stream (double hashing — colliders scatter rather than cluster), the
/// root lengthening only once a same-length probe budget is exhausted. Core
/// concepts (the authored Swadesh strata) are assigned first, so they win the
/// short forms, and a core root additionally rejects any candidate that is a
/// minimal pair of an already-placed core root (uniqueness is not enough for
/// a reader — the forms must be audibly distinct). Assignment ranges over the
/// WHOLE concept universe passed in, never a per-world subset, so a concept's
/// form never depends on which other concepts a given world exposed.
///
/// **Merger-aware (`daughters` non-empty):** a proto-root distinct at the proto
/// level can still merge with another after a daughter ages it through its own
/// cascade and nativizes it (the residual post-evolution homophony). When
/// `daughters` is supplied, a *core* candidate is additionally rejected if its
/// EVOLVED form under any daughter lands on ANY already-placed core concept's
/// evolved form in that daughter — driving core homophony to zero across the
/// whole family, cognate-safely (`evolve` is untouched, the proto stays
/// shared) and regularly (no sporadic rule; the proto is simply chosen to
/// survive the cascade distinct). Periphery mergers are tolerable and never
/// gate the assignment. An empty `daughters` reproduces the proto-only
/// assignment exactly.
/// type-audit: bare-ok(identifier-text)
pub fn assign_proto_roots(
    seed: &Seed,
    family: &str,
    proto_ph: &Phonology,
    concepts: &[&str],
    daughters: &[Daughter],
) -> std::collections::BTreeMap<String, Vec<Segment>> {
    // Core-first, then concept-id. Core concepts (the Swadesh strata) are
    // assigned before periphery so they win the short forms (length ∝
    // rarity), and the id tiebreak keeps the order stable — a concept added
    // to the universe later slots in without reshuffling the words already
    // assigned ahead of it.
    let core_rank = |concept: &&str| u8::from(!crate::packs::is_core_concept(concept));
    let mut ordered: Vec<&str> = concepts.to_vec();
    ordered.sort_by(|a, b| core_rank(a).cmp(&core_rank(b)).then_with(|| a.cmp(b)));

    let mut used: std::collections::BTreeSet<Vec<Segment>> = std::collections::BTreeSet::new();
    // Placed core roots, kept for the minimal-pair guard: a core candidate is
    // rejected not only when its form is already taken but when it is a
    // minimal pair of any core root already placed (periphery roots need only
    // be unequal — incidental near-homophony there is tolerable).
    let mut core_forms: Vec<Vec<Segment>> = Vec::new();
    // Merger-aware bookkeeping: per daughter, the set of modern forms already
    // placed for core concepts. A candidate whose evolved form hits one of these
    // in any daughter is a core merger to reject (domain-agnostic — the target
    // is zero core homophony, not merely zero same-domain confusable homophony).
    let mut placed_modern: Vec<std::collections::BTreeSet<Vec<Segment>>> =
        vec![std::collections::BTreeSet::new(); daughters.len()];
    let mut assigned: std::collections::BTreeMap<String, Vec<Segment>> =
        std::collections::BTreeMap::new();
    for concept in ordered {
        let core = crate::packs::is_core_concept(concept);
        let mut probe = 0u32;
        let form = loop {
            let candidate = draw_candidate(seed, family, concept, proto_ph, probe);
            let taken = used.contains(&candidate);
            let too_close = core
                && core_forms
                    .iter()
                    .any(|placed| is_minimal_pair(placed, &candidate));
            // Merger-aware: reject a core candidate that would evolve into ANY
            // already-placed core concept's modern form in any daughter.
            let merges = core
                && daughters.iter().enumerate().any(|(d, daughter)| {
                    let modern = evolve(&candidate, &daughter.cascade, &daughter.phonology).modern;
                    placed_modern[d].contains(&modern)
                });
            if !taken && !too_close && !merges {
                break candidate;
            }
            probe += 1;
        };
        used.insert(form.clone());
        // Record this concept's evolved form in each daughter, so later core
        // candidates are held off it (core concepts only — periphery mergers
        // are tolerable and never gate the assignment).
        if core {
            core_forms.push(form.clone());
            for (d, daughter) in daughters.iter().enumerate() {
                let modern = evolve(&form, &daughter.cascade, &daughter.phonology).modern;
                placed_modern[d].insert(modern);
            }
        }
        assigned.insert(concept.to_string(), form);
    }
    assigned
}

/// Whether `a` and `b` are a **minimal pair** — equal length, differing in
/// exactly one segment (one substitution). Such forms are formally distinct
/// yet read as near-homophones (Noa / Noe / Noo), so the assignment holds
/// core roots apart by this distance, not mere inequality. Different lengths
/// or two-plus differences are not minimal pairs.
fn is_minimal_pair(a: &[Segment], b: &[Segment]) -> bool {
    a.len() == b.len() && a.iter().zip(b).filter(|(x, y)| x != y).count() == 1
}

/// The number of same-length probes tried before the candidate root is
/// lengthened by one syllable — the open-addressing "table resize" that keeps
/// assignment terminating even in a phonology whose base space is smaller
/// than the concept universe.
const PROBE_BUDGET: u32 = 8;

/// Draw one candidate proto-root for `concept` at probe index `probe`. Probe
/// 0 draws from the base epoch path (the common, no-collision case);
/// higher probes draw from a probe-keyed sub-stream (double hashing — each
/// probe scatters independently rather than perturbing the last). Every
/// `PROBE_BUDGET` probes the candidate lengthens by one syllable, so a
/// cramped phonology still resolves all collisions.
fn draw_candidate(
    seed: &Seed,
    family: &str,
    concept: &str,
    ph: &Phonology,
    probe: u32,
) -> Vec<Segment> {
    let tier = probe / PROBE_BUDGET;
    let min = PROTO_ROOT_SYLLABLE_RANGE.0 + tier;
    let max = PROTO_ROOT_SYLLABLE_RANGE.1 + tier;
    let base = seed
        .derive(streams::ROOT)
        .derive(StreamLabel::dynamic(family))
        .derive(streams::LEXICON)
        .derive(streams::PROTO_ROOT)
        .derive(StreamLabel::dynamic(ROOT_EPOCH))
        .derive(StreamLabel::dynamic(concept));
    let mut stream = if probe == 0 {
        base.stream()
    } else {
        base.derive(streams::PROBE)
            .derive(StreamLabel::dynamic(&probe.to_string()))
            .stream()
    };
    let namer = Namer::new(seed, family, ph);
    let syllables = namer.draw_syllables(&mut stream, min, max, false);
    crate::naming::segments_of(&syllables)
}

/// Whether `seg` is a consonant (used by the two structural rules, which
/// condition on consonant-hood rather than a specific place/manner).
fn is_consonant(seg: Segment) -> bool {
    matches!(seg, Segment::Consonant { .. })
}

/// Lenition's total function on a single segment: a voiceless stop becomes
/// its voiced counterpart at the same place; anything else is identity.
fn lenition(seg: Segment) -> Segment {
    match seg {
        Segment::Consonant {
            place,
            manner: Manner::Stop,
            voiced: false,
        } => Segment::Consonant {
            place,
            manner: Manner::Stop,
            voiced: true,
        },
        other => other,
    }
}

/// Fortition's total function on a single segment: a fricative becomes a
/// stop at the same place and voicing; anything else is identity.
fn fortition(seg: Segment) -> Segment {
    match seg {
        Segment::Consonant {
            place,
            manner: Manner::Fricative,
            voiced,
        } => Segment::Consonant {
            place,
            manner: Manner::Stop,
            voiced,
        },
        other => other,
    }
}

/// VowelShift's total function on a single segment: raise (`param` 0) or
/// lower (`param` 1) a vowel's height one step. A consonant, a vowel
/// already at the extreme height for the requested direction, or an
/// unrecognized `param`, is identity.
fn vowel_shift(seg: Segment, param: u32) -> Segment {
    match seg {
        Segment::Vowel {
            height,
            backness,
            rounded,
            tone,
        } => {
            let height = match (param, height) {
                (0, Height::Low) => Height::Mid,
                (0, Height::Mid) => Height::High,
                (1, Height::High) => Height::Mid,
                (1, Height::Mid) => Height::Low,
                (_, unchanged) => unchanged,
            };
            // Tone is a separate tier: a quality shift carries the nucleus's
            // tone through unchanged (spec §3, carry-forward).
            Segment::Vowel {
                height,
                backness,
                rounded,
                tone,
            }
        }
        other => other,
    }
}

/// Apply a per-segment rule function `f` uniformly to every segment of
/// `segs`: `f`'s proposed output replaces the original only when it differs
/// AND is present in `ph.inventory` (the codomain constraint); otherwise
/// that occurrence is left unchanged. Because `f` is a pure function of the
/// segment alone, every occurrence of the same segment is decided the same
/// way — this is what makes the rule Neogrammarian-regular (never
/// sporadic).
fn apply_segment_rule(
    segs: &[Segment],
    ph: &Phonology,
    f: impl Fn(Segment) -> Segment,
) -> (Vec<Segment>, bool) {
    let mut changed = false;
    let out = segs
        .iter()
        .map(|seg| {
            let proposed = f(*seg);
            if proposed != *seg && ph.inventory.contains(&proposed) {
                changed = true;
                proposed
            } else {
                *seg
            }
        })
        .collect();
    (out, changed)
}

/// ClusterSimplify's total function on a whole word: if the word begins
/// with two consecutive consonants (a two-consonant onset — the only onset
/// position a flat segment sequence still carries once syllable boundaries
/// are gone), drop the first. Otherwise identity.
fn apply_cluster_simplify(segs: &[Segment]) -> (Vec<Segment>, bool) {
    if segs.len() >= 2 && is_consonant(segs[0]) && is_consonant(segs[1]) {
        let mut out = segs.to_vec();
        out.remove(0);
        (out, true)
    } else {
        (segs.to_vec(), false)
    }
}

/// FinalLoss's total function on a whole word: if the word's last segment
/// is a consonant (a word-final coda), drop it. Otherwise identity.
fn apply_final_loss(segs: &[Segment]) -> (Vec<Segment>, bool) {
    match segs.last() {
        Some(last) if is_consonant(*last) => {
            let mut out = segs.to_vec();
            out.pop();
            (out, true)
        }
        _ => (segs.to_vec(), false),
    }
}

/// Which nucleus a merger stranded — the tone-bearing unit tonogenesis
/// writes to. A dropped onset (`ClusterSimplify`) strands the word's first
/// vowel; a dropped coda (`FinalLoss`) strands its last.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ToneLocus {
    /// The word's first vowel (an onset was dropped before it).
    First,
    /// The word's last vowel (a coda was dropped after it).
    Last,
}

/// The tonogenetic conditioning a merger records for a later
/// [`RuleKind::Tonogenesis`] step: which nucleus was stranded, and whether
/// the dropped consonant was voiced (→ Low) or voiceless (→ High). Derived
/// entirely from the derivation's own history, never a stream draw.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct ToneConditioning {
    /// The stranded nucleus.
    locus: ToneLocus,
    /// The dropped consonant's voicing.
    dropped_voiced: bool,
}

/// The voicing of a dropped segment (always a consonant — the two structural
/// mergers only ever drop consonants; the `false` fallback is unreachable and
/// exists only to keep this total).
fn dropped_voicing(seg: Segment) -> bool {
    match seg {
        Segment::Consonant { voiced, .. } => voiced,
        Segment::Vowel { .. } => false,
    }
}

/// Tonogenesis's total function on a whole word (spec §4): given the
/// conditioning a prior merger recorded, write the corresponding tone
/// (voiced-loss → [`Tone::Low`], voiceless-loss → [`Tone::High`]) onto the
/// stranded nucleus. Applies only when a merger is pending AND the resulting
/// toned vowel is in `ph.inventory` (the codomain constraint that keeps an
/// atonal language a no-op). Pure — the pending conditioning is a function of
/// the derivation so far, not of any draw.
fn apply_tonogenesis(
    segs: &[Segment],
    pending: &Option<ToneConditioning>,
    ph: &Phonology,
) -> (Vec<Segment>, bool) {
    let Some(cond) = pending else {
        return (segs.to_vec(), false);
    };
    let tone = if cond.dropped_voiced {
        Tone::Low
    } else {
        Tone::High
    };
    let target = match cond.locus {
        ToneLocus::First => segs.iter().position(|s| matches!(s, Segment::Vowel { .. })),
        ToneLocus::Last => segs
            .iter()
            .rposition(|s| matches!(s, Segment::Vowel { .. })),
    };
    let Some(i) = target else {
        return (segs.to_vec(), false);
    };
    let Segment::Vowel {
        height,
        backness,
        rounded,
        ..
    } = segs[i]
    else {
        return (segs.to_vec(), false);
    };
    let toned = Segment::Vowel {
        height,
        backness,
        rounded,
        tone,
    };
    if toned != segs[i] && ph.inventory.contains(&toned) {
        let mut out = segs.to_vec();
        out[i] = toned;
        (out, true)
    } else {
        (segs.to_vec(), false)
    }
}

/// Dispatch one rule's application to a whole word under `ph`. `pending`
/// carries the tonogenetic conditioning a prior merger recorded (consumed
/// only by [`RuleKind::Tonogenesis`]).
fn apply_rule(
    segs: &[Segment],
    rule: &SoundRule,
    ph: &Phonology,
    pending: &Option<ToneConditioning>,
) -> (Vec<Segment>, bool) {
    match rule.kind {
        RuleKind::Lenition => apply_segment_rule(segs, ph, lenition),
        RuleKind::Fortition => apply_segment_rule(segs, ph, fortition),
        RuleKind::VowelShift => apply_segment_rule(segs, ph, |s| vowel_shift(s, rule.param)),
        RuleKind::ClusterSimplify => apply_cluster_simplify(segs),
        RuleKind::FinalLoss => apply_final_loss(segs),
        RuleKind::Tonogenesis => apply_tonogenesis(segs, pending, ph),
    }
}

/// Apply `cascade` to `proto` under `ph`, in order, recording each rule's
/// effect. Pure and total: same inputs always produce the same
/// [`Derivation`]; never panics; a rule's proposed output segment is only
/// ever adopted when it is present in `ph.inventory` (the codomain
/// constraint), otherwise that occurrence is left unchanged.
pub fn evolve(proto: &[Segment], cascade: &Cascade, ph: &Phonology) -> Derivation {
    let mut current = proto.to_vec();
    let mut steps = Vec::with_capacity(cascade.rules.len());
    // The most recent merger's tonogenetic conditioning, recorded from the
    // pre-drop word and consumed by a later `Tonogenesis` step. Threading it
    // through the derivation (never a stream draw) is what keeps tonogenesis a
    // pure, replayable function of `(proto, cascade, ph)`.
    let mut pending: Option<ToneConditioning> = None;
    for rule in &cascade.rules {
        let (next, changed) = apply_rule(&current, rule, ph, &pending);
        // Update the pending conditioning from `current` (the word *before*
        // this rule dropped anything): a fired merger records its dropped
        // consonant's voicing and stranded locus; a tonogenesis step consumes
        // whatever was pending; every other rule leaves it intact.
        match rule.kind {
            RuleKind::ClusterSimplify if changed => {
                pending = Some(ToneConditioning {
                    locus: ToneLocus::First,
                    dropped_voiced: dropped_voicing(current[0]),
                });
            }
            RuleKind::FinalLoss if changed => {
                pending = Some(ToneConditioning {
                    locus: ToneLocus::Last,
                    dropped_voiced: dropped_voicing(current[current.len() - 1]),
                });
            }
            RuleKind::Tonogenesis => pending = None,
            _ => {}
        }
        steps.push(AppliedRule {
            rule: *rule,
            changed,
        });
        current = next;
    }
    let current = nativize(&current, ph);
    Derivation {
        proto: proto.to_vec(),
        steps,
        modern: current,
    }
}

/// Merge each segment not already in `ph.inventory` to the nearest
/// same-class inventory segment (consonant→consonant, vowel→vowel) by
/// feature-mismatch count, ties broken by `Segment`'s total order. A segment
/// with no same-class neighbour in the inventory is left unchanged. Pure and
/// draw-free: this is how descent absorbs an inherited sound the daughter's
/// inventory no longer keeps (spec §2.2).
pub fn nativize(segs: &[Segment], ph: &Phonology) -> Vec<Segment> {
    segs.iter()
        .map(|&s| {
            if ph.inventory.contains(&s) {
                return s;
            }
            ph.inventory
                .iter()
                .copied()
                .filter(|c| same_class(*c, s))
                .min_by(|a, b| {
                    feature_distance(*a, s)
                        .cmp(&feature_distance(*b, s))
                        .then(a.cmp(b))
                })
                .unwrap_or(s)
        })
        .collect()
}

/// Whether two segments are both consonants or both vowels.
fn same_class(a: Segment, b: Segment) -> bool {
    matches!(
        (a, b),
        (Segment::Consonant { .. }, Segment::Consonant { .. })
            | (Segment::Vowel { .. }, Segment::Vowel { .. })
    )
}

/// Count of differing features between two same-class segments (place,
/// manner, voicing for consonants; height, backness, rounding for vowels).
/// Cross-class pairs never reach here (filtered by `same_class`).
fn feature_distance(a: Segment, b: Segment) -> u8 {
    match (a, b) {
        (
            Segment::Consonant {
                place: p1,
                manner: m1,
                voiced: v1,
            },
            Segment::Consonant {
                place: p2,
                manner: m2,
                voiced: v2,
            },
        ) => (p1 != p2) as u8 + (m1 != m2) as u8 + (v1 != v2) as u8,
        (
            Segment::Vowel {
                height: h1,
                backness: b1,
                rounded: r1,
                ..
            },
            Segment::Vowel {
                height: h2,
                backness: b2,
                rounded: r2,
                ..
            },
        ) => (h1 != h2) as u8 + (b1 != b2) as u8 + (r1 != r2) as u8,
        _ => u8::MAX,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::phonology::{Envelope, ExoticSeg, draw_phonology};

    /// A permissive envelope, drawn at a fixed seed verified (by search) to
    /// contain every consonant [`evolve`]'s tests exercise across the
    /// Lenition/Fortition/codomain paths: the six oral stops (p/b, t/d,
    /// k/g) and the three non-sibilant fricatives (f/v, x), alongside every
    /// canonical vowel (always kept under a full `vowel_space`). NOTE: the
    /// fixed seed is coupled to `draw_phonology`'s stream-consumption
    /// order — if that draw algorithm ever changes, this inventory changes
    /// and the seed must be re-searched (these tests will fail loudly, not
    /// silently).
    fn test_phonology() -> Phonology {
        draw_phonology(
            &Seed(37),
            "test",
            &Envelope {
                labiality: 1.0,
                vowel_space: 1.0,
                voicing: 1.0,
                sibilance: 1.0,
                voice_loudness: 1.0,
                tonality: 0.0,
                exotic: ExoticSeg::None,
            },
        )
    }

    fn is_voiceless_stop(seg: &Segment) -> bool {
        matches!(
            seg,
            Segment::Consonant {
                manner: Manner::Stop,
                voiced: false,
                ..
            }
        )
    }

    fn proto_with_two_voiceless_stops() -> Vec<Segment> {
        use crate::phoneme::{Backness, Place};
        vec![
            Segment::Consonant {
                place: Place::Labial,
                manner: Manner::Stop,
                voiced: false,
            }, // p
            Segment::Vowel {
                height: Height::Low,
                backness: Backness::Central,
                rounded: false,
                tone: Tone::Neutral,
            }, // a
            Segment::Consonant {
                place: Place::Velar,
                manner: Manner::Stop,
                voiced: false,
            }, // k
        ]
    }

    #[test]
    fn evolve_is_regular() {
        // Neogrammarian: every segment matching the environment changes; replaying
        // evolve on the recorded proto reproduces the recorded modern exactly.
        let ph = test_phonology();
        let cascade = Cascade {
            rules: vec![SoundRule {
                kind: RuleKind::Lenition,
                param: 0,
            }],
        };
        let d = evolve(&proto_with_two_voiceless_stops(), &cascade, &ph);
        let voiceless_left = d.modern.iter().filter(|s| is_voiceless_stop(s)).count();
        assert_eq!(
            voiceless_left, 0,
            "lenition must hit every voiceless stop or none"
        );
    }

    #[test]
    fn evolve_lands_in_inventory() {
        // Codomain: every modern segment is in the phonology's inventory.
        let ph = test_phonology();
        let cascade = draw_cascade(&Seed(5), "test");
        let proto = proto_root(&Seed(5), "test", "water", &ph);
        let d = evolve(&proto, &cascade, &ph);
        for seg in &d.modern {
            assert!(
                ph.inventory.contains(seg),
                "modern segment {seg:?} is not in the phonology's inventory"
            );
        }
    }

    #[test]
    fn derivation_replays() {
        // evolve(d.proto, cascade, ph).modern == d.modern — byte-stable replay.
        let ph = test_phonology();
        let cascade = draw_cascade(&Seed(9), "test");
        let proto = proto_root(&Seed(9), "test", "fire", &ph);
        let d = evolve(&proto, &cascade, &ph);
        let replayed = evolve(&d.proto, &cascade, &ph);
        assert_eq!(replayed.modern, d.modern);
    }

    #[test]
    fn draw_cascade_default_equals_settled_regime() {
        // draw_cascade must stay a thin wrapper over draw_cascade_with_regime
        // at CascadeRegime::SETTLED — byte-identical for every existing
        // caller, across several seeds/species.
        for (seed, species) in [
            (Seed(1), "goblin"),
            (Seed(5), "kobold"),
            (Seed(9), "hobgoblin"),
            (Seed(42), "test"),
        ] {
            assert_eq!(
                draw_cascade(&seed, species),
                draw_cascade_with_regime(&seed, species, CascadeRegime::SETTLED),
                "draw_cascade({seed:?}, {species:?}) must equal the SETTLED-regime draw"
            );
        }
    }

    #[test]
    fn proto_root_is_pure() {
        // Same (seed, species, concept) → identical segments, twice.
        let ph = test_phonology();
        let a = proto_root(&Seed(11), "test", "stone", &ph);
        let b = proto_root(&Seed(11), "test", "stone", &ph);
        assert_eq!(a, b);
        assert!(!a.is_empty());
    }

    use crate::phoneme::{Backness, Place};

    /// A deliberately cramped phonology: two stops (t, k), one nasal (n), two
    /// vowels (a, e), single-vowel nuclei, an optional nasal coda. Its base
    /// space is small enough that ~12 independent 1-2 syllable draws WOULD
    /// collide (the birthday-paradox failure the assignment fixes), while
    /// still admitting enough distinct forms to resolve them without
    /// lengthening — so injectivity here exercises collision-probing, not
    /// growth.
    fn cramped_phonology() -> Phonology {
        Phonology {
            inventory: vec![
                c(Place::Alveolar, Manner::Stop, false),  // t
                c(Place::Velar, Manner::Stop, false),     // k
                c(Place::Alveolar, Manner::Nasal, true),  // n
                v(Height::Low, Backness::Central, false), // a
                v(Height::Mid, Backness::Front, false),   // e
            ],
            onsets: vec![vec![Manner::Stop]],
            nuclei: 1,
            codas: vec![vec![Manner::Nasal], vec![]],
        }
    }

    #[test]
    fn assign_proto_roots_is_injective_where_independent_draws_would_collide() {
        // The core guarantee: over a cramped phonology where per-concept
        // draws collide, every concept still gets a DISTINCT proto-root.
        let ph = cramped_phonology();
        let concepts = [
            "c00", "c01", "c02", "c03", "c04", "c05", "c06", "c07", "c08", "c09", "c10", "c11",
        ];
        let assigned = assign_proto_roots(&Seed(1), "fam", &ph, &concepts, &[]);
        assert_eq!(assigned.len(), concepts.len(), "one form per concept");
        let forms: std::collections::BTreeSet<&Vec<Segment>> = assigned.values().collect();
        assert_eq!(
            forms.len(),
            concepts.len(),
            "assignment must be injective: a distinct form per concept"
        );
    }

    /// A phonology so tiny its base 1-2 syllable space cannot hold many
    /// concepts: one stop (t), one nasal (n), one vowel (a), optional nasal
    /// coda. Monosyllables: ta, tan (2); the base space saturates almost
    /// immediately, forcing the assignment to lengthen roots to stay
    /// injective.
    fn minuscule_phonology() -> Phonology {
        Phonology {
            inventory: vec![
                c(Place::Alveolar, Manner::Stop, false),  // t
                c(Place::Alveolar, Manner::Nasal, true),  // n
                v(Height::Low, Backness::Central, false), // a
            ],
            onsets: vec![vec![Manner::Stop]],
            nuclei: 1,
            codas: vec![vec![Manner::Nasal], vec![]],
        }
    }

    #[test]
    fn assign_proto_roots_lengthens_to_stay_injective_when_the_base_space_saturates() {
        let ph = minuscule_phonology();
        let concepts: Vec<String> = (0..16).map(|i| format!("c{i:02}")).collect();
        let refs: Vec<&str> = concepts.iter().map(|s| s.as_str()).collect();
        let assigned = assign_proto_roots(&Seed(2), "fam", &ph, &refs, &[]);
        let forms: std::collections::BTreeSet<&Vec<Segment>> = assigned.values().collect();
        assert_eq!(forms.len(), refs.len(), "still injective under saturation");
        // Non-vacuity: growth must actually have fired — a base draw is at
        // most PROTO_ROOT_SYLLABLE_RANGE.1 syllables, and in this 2-form
        // space 16 concepts cannot fit without longer roots. A rough segment
        // bound (>= 5) can only come from a 3+ syllable root here.
        let longest = assigned.values().map(|f| f.len()).max().unwrap();
        assert!(
            longest >= 5,
            "saturation must force lengthening beyond the base tier; longest was {longest} segments"
        );
    }

    #[test]
    fn is_minimal_pair_flags_exactly_one_substitution() {
        let t = c(Place::Alveolar, Manner::Stop, false);
        let n = c(Place::Alveolar, Manner::Nasal, true);
        let a = v(Height::Low, Backness::Central, false);
        let e = v(Height::Mid, Backness::Front, false);
        // ta vs te: one vowel substitution → minimal pair.
        assert!(is_minimal_pair(&[t, a], &[t, e]));
        // ta vs na: one consonant substitution → minimal pair.
        assert!(is_minimal_pair(&[t, a], &[n, a]));
        // ta vs ta: identical (zero differences) → not a minimal pair.
        assert!(!is_minimal_pair(&[t, a], &[t, a]));
        // ta vs tan: different lengths → not a minimal pair.
        assert!(!is_minimal_pair(&[t, a], &[t, a, n]));
        // ta vs ne: two substitutions → not a minimal pair.
        assert!(!is_minimal_pair(&[t, a], &[n, e]));
    }

    #[test]
    fn assign_proto_roots_places_core_concepts_first_so_they_win_the_short_forms() {
        // "water" is core (universal stratum) but sorts LAST here; the
        // "aaN" fillers are periphery and sort first. Under a saturating
        // phonology, whoever is assigned first gets the short base forms.
        // Core-first ordering must give the core concept a form no longer
        // than any periphery word — even though its id sorts last.
        let ph = minuscule_phonology();
        let mut concepts = vec!["water"];
        let fillers: Vec<String> = (0..6).map(|i| format!("aa{i}")).collect();
        concepts.extend(fillers.iter().map(|s| s.as_str()));
        let assigned = assign_proto_roots(&Seed(3), "fam", &ph, &concepts, &[]);

        let core_len = assigned["water"].len();
        let max_periph = fillers.iter().map(|f| assigned[f].len()).max().unwrap();
        assert!(
            core_len <= max_periph,
            "core 'water' ({core_len} segs) must be no longer than the longest \
             periphery word ({max_periph} segs) — core is assigned first"
        );
    }

    /// Fourteen real core concepts (universal stratum, body pack) — enough
    /// to saturate a cramped phonology and force same-length neighbours.
    fn core_concept_batch() -> Vec<&'static str> {
        vec![
            "water", "night", "day", "fire", "eat", "sleep", "die", "one", "two", "many", "hand",
            "foot", "eye", "mouth",
        ]
    }

    #[test]
    fn assign_proto_roots_holds_core_roots_apart_by_more_than_a_minimal_pair() {
        // Core vocabulary must be audibly distinct, not merely unequal: no two
        // core roots may be a minimal pair (Noa/Noe). Checked over a cramped
        // phonology that forces same-length neighbours, across several seeds.
        let ph = cramped_phonology();
        let concepts = core_concept_batch();
        for seed in 0..8u64 {
            let assigned = assign_proto_roots(&Seed(seed), "fam", &ph, &concepts, &[]);
            let forms: Vec<&Vec<Segment>> = assigned.values().collect();
            for i in 0..forms.len() {
                for j in (i + 1)..forms.len() {
                    assert!(
                        !is_minimal_pair(forms[i], forms[j]),
                        "seed {seed}: core roots {:?} and {:?} are a minimal pair",
                        forms[i],
                        forms[j]
                    );
                }
            }
        }
    }

    #[test]
    fn assign_proto_roots_is_insertion_stable_for_earlier_sorting_concepts() {
        // A concept's assignment depends only on the concepts sorted at or
        // before it (core-first, then id). So adding a later-sorting concept
        // to the universe leaves every earlier assignment byte-identical —
        // the registry can grow without reshuffling committed vocabulary
        // (the property a global minimal-perfect-hash recompute would break).
        let ph = cramped_phonology();
        let mut base = core_concept_batch();
        base.push("aa0");
        base.push("aa1");
        let before = assign_proto_roots(&Seed(4), "fam", &ph, &base, &[]);

        // "zzz-late" is non-core (sorts after every core concept) and its id
        // sorts after "aa1" — so it lands strictly last.
        let mut grown = base.clone();
        grown.push("zzz-late");
        let after = assign_proto_roots(&Seed(4), "fam", &ph, &grown, &[]);

        for concept in &base {
            assert_eq!(
                before[*concept], after[*concept],
                "{concept} must keep its form when a later-sorting concept is added"
            );
        }
    }

    #[test]
    fn assign_proto_roots_is_deterministic() {
        let ph = cramped_phonology();
        let concepts = ["water", "night", "hand", "many", "c00", "c01", "c02"];
        let a = assign_proto_roots(&Seed(7), "fam", &ph, &concepts, &[]);
        let b = assign_proto_roots(&Seed(7), "fam", &ph, &concepts, &[]);
        assert_eq!(a, b, "same inputs must yield an identical assignment");
    }

    // ---- Per-rule transformation tests: each constructs a single-rule
    // cascade directly (no drawing), feeds a hand-built proto whose
    // segments are all in `test_phonology()`'s inventory, and asserts the
    // exact expected modern sequence.

    /// A consonant literal, for hand-built protos.
    fn c(place: Place, manner: Manner, voiced: bool) -> Segment {
        Segment::Consonant {
            place,
            manner,
            voiced,
        }
    }

    /// A vowel literal (atonal), for hand-built protos.
    fn v(height: Height, backness: Backness, rounded: bool) -> Segment {
        Segment::Vowel {
            height,
            backness,
            rounded,
            tone: Tone::Neutral,
        }
    }

    /// A toned vowel literal, for hand-built protos and tone-capable test
    /// inventories exercising tonogenesis.
    fn vt(height: Height, backness: Backness, rounded: bool, tone: Tone) -> Segment {
        Segment::Vowel {
            height,
            backness,
            rounded,
            tone,
        }
    }

    /// A one-rule cascade, for targeting a single rule's semantics.
    fn one_rule(kind: RuleKind, param: u32) -> Cascade {
        Cascade {
            rules: vec![SoundRule { kind, param }],
        }
    }

    #[test]
    fn fortition_turns_fricatives_into_stops_at_same_place_and_voicing() {
        let ph = test_phonology();
        // f-a-v: both fricatives (one voiceless, one voiced) must harden.
        let proto = vec![
            c(Place::Labial, Manner::Fricative, false), // f
            v(Height::Low, Backness::Central, false),   // a
            c(Place::Labial, Manner::Fricative, true),  // v
        ];
        let d = evolve(&proto, &one_rule(RuleKind::Fortition, 0), &ph);
        assert_eq!(
            d.modern,
            vec![
                c(Place::Labial, Manner::Stop, false), // p: same place, still voiceless
                v(Height::Low, Backness::Central, false), // a: untouched
                c(Place::Labial, Manner::Stop, true),  // b: same place, still voiced
            ]
        );
        assert!(d.steps[0].changed);
    }

    #[test]
    fn vowel_shift_param_0_raises_one_step_and_high_stays_high() {
        let ph = test_phonology();
        // t-e-t-i: e (Mid) raises to i (High); i is already High — identity.
        let proto = vec![
            c(Place::Alveolar, Manner::Stop, false), // t
            v(Height::Mid, Backness::Front, false),  // e
            c(Place::Alveolar, Manner::Stop, false), // t
            v(Height::High, Backness::Front, false), // i
        ];
        let d = evolve(&proto, &one_rule(RuleKind::VowelShift, 0), &ph);
        assert_eq!(
            d.modern,
            vec![
                c(Place::Alveolar, Manner::Stop, false), // t: consonants untouched
                v(Height::High, Backness::Front, false), // i: raised from e
                c(Place::Alveolar, Manner::Stop, false), // t
                v(Height::High, Backness::Front, false), // i: High stays High
            ]
        );
        assert!(d.steps[0].changed);
    }

    #[test]
    fn vowel_shift_param_1_lowers_one_step() {
        let ph = test_phonology();
        // k-i-k-u: i (High Front) lowers to e (Mid Front); u (High Back
        // rounded) lowers to o (Mid Back rounded).
        let proto = vec![
            c(Place::Velar, Manner::Stop, false),    // k
            v(Height::High, Backness::Front, false), // i
            c(Place::Velar, Manner::Stop, false),    // k
            v(Height::High, Backness::Back, true),   // u
        ];
        let d = evolve(&proto, &one_rule(RuleKind::VowelShift, 1), &ph);
        assert_eq!(
            d.modern,
            vec![
                c(Place::Velar, Manner::Stop, false),   // k
                v(Height::Mid, Backness::Front, false), // e: lowered from i
                c(Place::Velar, Manner::Stop, false),   // k
                v(Height::Mid, Backness::Back, true),   // o: lowered from u
            ]
        );
        assert!(d.steps[0].changed);
    }

    #[test]
    fn cluster_simplify_drops_the_first_of_a_word_initial_two_consonant_onset() {
        let ph = test_phonology();
        // f-t-a: two-consonant word-initial onset drops its first segment.
        let proto = vec![
            c(Place::Labial, Manner::Fricative, false), // f
            c(Place::Alveolar, Manner::Stop, false),    // t
            v(Height::Low, Backness::Central, false),   // a
        ];
        let d = evolve(&proto, &one_rule(RuleKind::ClusterSimplify, 0), &ph);
        assert_eq!(
            d.modern,
            vec![
                c(Place::Alveolar, Manner::Stop, false), // t: now word-initial
                v(Height::Low, Backness::Central, false), // a
            ]
        );
        assert!(d.steps[0].changed);

        // t-a: a single-consonant onset is untouched.
        let simple = vec![
            c(Place::Alveolar, Manner::Stop, false),  // t
            v(Height::Low, Backness::Central, false), // a
        ];
        let d = evolve(&simple, &one_rule(RuleKind::ClusterSimplify, 0), &ph);
        assert_eq!(d.modern, simple);
        assert!(!d.steps[0].changed);
    }

    #[test]
    fn final_loss_drops_a_word_final_consonant_but_not_a_final_vowel() {
        let ph = test_phonology();
        // t-a-k: the word-final coda consonant drops.
        let proto = vec![
            c(Place::Alveolar, Manner::Stop, false),  // t
            v(Height::Low, Backness::Central, false), // a
            c(Place::Velar, Manner::Stop, false),     // k
        ];
        let d = evolve(&proto, &one_rule(RuleKind::FinalLoss, 0), &ph);
        assert_eq!(
            d.modern,
            vec![
                c(Place::Alveolar, Manner::Stop, false),  // t
                v(Height::Low, Backness::Central, false), // a: now word-final
            ]
        );
        assert!(d.steps[0].changed);

        // t-a: a word-final vowel (an open syllable) is untouched.
        let open = vec![
            c(Place::Alveolar, Manner::Stop, false),  // t
            v(Height::Low, Backness::Central, false), // a
        ];
        let d = evolve(&open, &one_rule(RuleKind::FinalLoss, 0), &ph);
        assert_eq!(d.modern, open);
        assert!(!d.steps[0].changed);
    }

    // ---- Tonogenesis (Stage 3): a lost segmental contrast reborn as pitch.

    /// A deliberately hand-built **tone-capable** phonology: the stops and
    /// nasal the tonogenesis tests need, plus the low central vowel `a` in
    /// all three relevant tones (Neutral, High, Low) IN the inventory — so a
    /// tone the rule writes lands in the codomain and actually takes. (An
    /// atonal inventory holds only the Neutral vowel; `test_phonology()` is
    /// such a case, exercised by [`tonogenesis_is_inert_in_an_atonal_language`].)
    fn tonal_phonology() -> Phonology {
        Phonology {
            inventory: vec![
                c(Place::Labial, Manner::Stop, true),     // b (voiced)
                c(Place::Labial, Manner::Stop, false),    // p (voiceless)
                c(Place::Alveolar, Manner::Stop, false),  // t
                c(Place::Alveolar, Manner::Stop, true),   // d (voiced)
                c(Place::Velar, Manner::Stop, false),     // k
                c(Place::Alveolar, Manner::Nasal, true),  // n (voiced)
                v(Height::Low, Backness::Central, false), // a  (Neutral)
                vt(Height::Low, Backness::Central, false, Tone::High), // á
                vt(Height::Low, Backness::Central, false, Tone::Low), // à
            ],
            onsets: vec![vec![Manner::Stop]],
            nuclei: 1,
            codas: vec![vec![Manner::Nasal], vec![Manner::Stop], vec![]],
        }
    }

    /// The tonogenesis cascade the tests drive: the merging rule, then the
    /// split. Tonogenesis reads what the prior merger dropped.
    fn merge_then_tonogenize(merger: RuleKind) -> Cascade {
        Cascade {
            rules: vec![
                SoundRule {
                    kind: merger,
                    param: 0,
                },
                SoundRule {
                    kind: RuleKind::Tonogenesis,
                    param: 0,
                },
            ],
        }
    }

    #[test]
    fn tonogenesis_writes_high_for_a_dropped_voiceless_onset_low_for_a_voiced_one() {
        let ph = tonal_phonology();
        // p-t-a: ClusterSimplify drops the voiceless p; the stranded first
        // nucleus takes High.
        let voiceless = vec![
            c(Place::Labial, Manner::Stop, false),    // p (voiceless)
            c(Place::Alveolar, Manner::Stop, false),  // t
            v(Height::Low, Backness::Central, false), // a
        ];
        let d = evolve(
            &voiceless,
            &merge_then_tonogenize(RuleKind::ClusterSimplify),
            &ph,
        );
        assert_eq!(
            d.modern,
            vec![
                c(Place::Alveolar, Manner::Stop, false),               // t
                vt(Height::Low, Backness::Central, false, Tone::High), // á
            ],
            "a dropped voiceless onset must raise the stranded nucleus to High"
        );
        assert!(d.steps[1].changed, "the tonogenesis step must fire");

        // b-t-a: ClusterSimplify drops the voiced b; the nucleus takes Low.
        let voiced = vec![
            c(Place::Labial, Manner::Stop, true),     // b (voiced)
            c(Place::Alveolar, Manner::Stop, false),  // t
            v(Height::Low, Backness::Central, false), // a
        ];
        let d = evolve(
            &voiced,
            &merge_then_tonogenize(RuleKind::ClusterSimplify),
            &ph,
        );
        assert_eq!(
            d.modern,
            vec![
                c(Place::Alveolar, Manner::Stop, false),              // t
                vt(Height::Low, Backness::Central, false, Tone::Low), // à
            ],
            "a dropped voiced onset must lower the stranded nucleus to Low"
        );
    }

    #[test]
    fn tonogenesis_from_final_loss_tones_the_last_nucleus() {
        let ph = tonal_phonology();
        // t-a-d: FinalLoss drops the voiced coda d; the last nucleus → Low.
        let voiced_coda = vec![
            c(Place::Alveolar, Manner::Stop, false),  // t
            v(Height::Low, Backness::Central, false), // a
            c(Place::Alveolar, Manner::Stop, true),   // d (voiced)
        ];
        let d = evolve(
            &voiced_coda,
            &merge_then_tonogenize(RuleKind::FinalLoss),
            &ph,
        );
        assert_eq!(
            d.modern,
            vec![
                c(Place::Alveolar, Manner::Stop, false),              // t
                vt(Height::Low, Backness::Central, false, Tone::Low), // à
            ],
        );

        // t-a-k: FinalLoss drops the voiceless coda k; the last nucleus → High.
        let voiceless_coda = vec![
            c(Place::Alveolar, Manner::Stop, false),  // t
            v(Height::Low, Backness::Central, false), // a
            c(Place::Velar, Manner::Stop, false),     // k (voiceless)
        ];
        let d = evolve(
            &voiceless_coda,
            &merge_then_tonogenize(RuleKind::FinalLoss),
            &ph,
        );
        assert_eq!(
            d.modern,
            vec![
                c(Place::Alveolar, Manner::Stop, false),               // t
                vt(Height::Low, Backness::Central, false, Tone::High), // á
            ],
        );
    }

    #[test]
    fn a_dropped_nasal_coda_falls_in_the_voiced_low_bucket() {
        // Nasals are voiced, so a lost nasal coda tonogenizes to Low — no
        // special case (the §Q2 decision: nasal codas join the voiced bucket).
        let ph = tonal_phonology();
        let nasal_coda = vec![
            c(Place::Alveolar, Manner::Stop, false),  // t
            v(Height::Low, Backness::Central, false), // a
            c(Place::Alveolar, Manner::Nasal, true),  // n (voiced)
        ];
        let d = evolve(
            &nasal_coda,
            &merge_then_tonogenize(RuleKind::FinalLoss),
            &ph,
        );
        assert_eq!(
            d.modern,
            vec![
                c(Place::Alveolar, Manner::Stop, false),              // t
                vt(Height::Low, Backness::Central, false, Tone::Low), // à
            ],
        );
    }

    #[test]
    fn tonogenesis_preserves_a_contrast_two_roots_would_otherwise_lose_to_a_merger() {
        // The load-bearing invariant (spec §2.2): two roots differing ONLY in
        // the onset the merger destroys stay distinct IFF they differed in the
        // tonogenetic conditioning feature. b-t-a and p-t-a both cluster-
        // simplify to t-a — a merger — but tonogenesis splits them by pitch.
        let ph = tonal_phonology();
        let voiced = vec![
            c(Place::Labial, Manner::Stop, true),     // b
            c(Place::Alveolar, Manner::Stop, false),  // t
            v(Height::Low, Backness::Central, false), // a
        ];
        let voiceless = vec![
            c(Place::Labial, Manner::Stop, false),    // p
            c(Place::Alveolar, Manner::Stop, false),  // t
            v(Height::Low, Backness::Central, false), // a
        ];

        // Without tonogenesis, the merger collapses them onto one form.
        let merge_only = one_rule(RuleKind::ClusterSimplify, 0);
        assert_eq!(
            evolve(&voiced, &merge_only, &ph).modern,
            evolve(&voiceless, &merge_only, &ph).modern,
            "test premise: the bare merger homophones the two roots"
        );

        // With tonogenesis, the lost voicing contrast survives as pitch.
        let cascade = merge_then_tonogenize(RuleKind::ClusterSimplify);
        assert_ne!(
            evolve(&voiced, &cascade, &ph).modern,
            evolve(&voiceless, &cascade, &ph).modern,
            "tonogenesis must rescue the contrast the merger destroyed"
        );
    }

    #[test]
    fn tonogenesis_is_inert_in_an_atonal_language() {
        // The codomain constraint keeps atonal species byte-identical: their
        // inventory holds only the Neutral vowel, so the toned output is
        // off-inventory and tonogenesis applies as identity. `test_phonology`
        // is atonal (every vowel Neutral).
        let ph = test_phonology();
        let proto = vec![
            c(Place::Labial, Manner::Stop, true),     // b
            c(Place::Alveolar, Manner::Stop, false),  // t
            v(Height::Low, Backness::Central, false), // a
        ];
        let with_tono = evolve(
            &proto,
            &merge_then_tonogenize(RuleKind::ClusterSimplify),
            &ph,
        );
        let without = evolve(&proto, &one_rule(RuleKind::ClusterSimplify, 0), &ph);
        assert_eq!(
            with_tono.modern, without.modern,
            "in an atonal inventory tonogenesis must be a no-op (byte-identity)"
        );
        assert!(
            !with_tono.steps[1].changed,
            "the tonogenesis step must record changed=false when inert"
        );
    }

    #[test]
    fn tonogenesis_is_pure_and_replayable() {
        // evolve is pure: replaying the recorded proto+cascade reproduces the
        // modern form exactly, tone and all.
        let ph = tonal_phonology();
        let proto = vec![
            c(Place::Labial, Manner::Stop, true),     // b
            c(Place::Alveolar, Manner::Stop, false),  // t
            v(Height::Low, Backness::Central, false), // a
        ];
        let cascade = merge_then_tonogenize(RuleKind::ClusterSimplify);
        let d = evolve(&proto, &cascade, &ph);
        let replay = evolve(&d.proto, &cascade, &ph);
        assert_eq!(d.modern, replay.modern);
    }

    #[test]
    fn tonogenesis_without_a_prior_merger_is_identity() {
        // Tonogenesis reads a prior merger's dropped feature; with none, it
        // has nothing to write and is the identity.
        let ph = tonal_phonology();
        let proto = vec![
            c(Place::Alveolar, Manner::Stop, false),  // t
            v(Height::Low, Backness::Central, false), // a
        ];
        let cascade = one_rule(RuleKind::Tonogenesis, 0);
        let d = evolve(&proto, &cascade, &ph);
        assert_eq!(d.modern, proto, "no prior merger ⇒ no tone written");
        assert!(!d.steps[0].changed);
    }

    // ---- Nativization fixtures and tests.

    /// A restrictive phonology whose inventory genuinely lacks the
    /// postalveolar sibilant ʃ (and its voiced counterpart ʒ), built by
    /// filtering [`test_phonology`]'s permissive inventory — every other
    /// consonant (the stops and non-sibilant fricatives) remains.
    fn restrictive_no_postalveolar() -> Phonology {
        let mut ph = test_phonology();
        ph.inventory.retain(|s| {
            !matches!(
                s,
                Segment::Consonant {
                    place: Place::Postalveolar,
                    ..
                }
            )
        });
        ph
    }

    /// A restrictive phonology whose inventory genuinely lacks the velar
    /// nasal ŋ, built by filtering [`test_phonology`]'s permissive
    /// inventory — every other consonant remains.
    fn restrictive_no_velar_nasal() -> Phonology {
        let mut ph = test_phonology();
        ph.inventory.retain(|s| {
            !matches!(
                s,
                Segment::Consonant {
                    place: Place::Velar,
                    manner: Manner::Nasal,
                    ..
                }
            )
        });
        ph
    }

    #[test]
    fn nativize_keeps_in_inventory_segments_untouched() {
        let ph = test_phonology(); // permissive: every segment in-inventory
        let word = proto_root(&Seed(1), "test", "water", &ph);
        assert_eq!(nativize(&word, &ph), word); // no-op when all in-inventory
    }

    #[test]
    fn nativize_merges_off_inventory_to_nearest_same_class() {
        // A restrictive inventory lacking the postalveolar sibilant ʃ; ʃ must
        // merge to the nearest same-class (consonant) segment present, never to
        // a vowel.
        let ph = restrictive_no_postalveolar();
        assert!(!ph.inventory.contains(&Segment::Consonant {
            place: Place::Postalveolar,
            manner: Manner::Sibilant,
            voiced: false,
        }));
        let sh = Segment::Consonant {
            place: Place::Postalveolar,
            manner: Manner::Sibilant,
            voiced: false,
        };
        let out = nativize(&[sh], &ph);
        assert!(ph.inventory.contains(&out[0]));
        assert!(matches!(out[0], Segment::Consonant { .. }));
    }

    #[test]
    fn evolve_output_is_subset_of_inventory_even_from_foreign_proto() {
        // proto drawn from a permissive inventory, evolved into a restrictive one
        let proto_ph = test_phonology();
        let daughter_ph = restrictive_no_postalveolar();
        let proto = proto_root(&Seed(2), "goblinoid", "water", &proto_ph);
        let cascade = draw_cascade(&Seed(2), "bugbear");
        let d = evolve(&proto, &cascade, &daughter_ph);
        assert!(d.modern.iter().all(|s| daughter_ph.inventory.contains(s)));
    }

    #[test]
    fn nativization_is_load_bearing_not_codomain_identity() {
        // NON-VACUITY GUARD. A nasal is untouched by every rule kind (Lenition
        // hits voiceless stops, Fortition fricatives, VowelShift vowels,
        // Cluster/FinalLoss only drop EDGE consonants). Put a MEDIAL nasal the
        // daughter inventory lacks into a proto word: no rule can move it, so the
        // ONLY way `modern ⊆ inventory` can hold is nativization actually firing.
        // Without this guard, the subset test above could pass via plain
        // codomain-identity and never exercise `nativize` at all.
        let daughter_ph = restrictive_no_velar_nasal(); // inventory without ŋ
        let a = Segment::Vowel {
            height: Height::Low,
            backness: Backness::Central,
            rounded: false,
            tone: Tone::Neutral,
        };
        let eng = Segment::Consonant {
            place: Place::Velar,
            manner: Manner::Nasal,
            voiced: true,
        };
        assert!(
            !daughter_ph.inventory.contains(&eng),
            "ŋ must be off-inventory for this test"
        );
        let proto = vec![a, eng, a]; // /aŋa/: the nasal is medial, untouched by all rules
        let d = evolve(&proto, &draw_cascade(&Seed(5), "bugbear"), &daughter_ph);
        assert!(d.modern.iter().all(|s| daughter_ph.inventory.contains(s)));
        assert_ne!(
            d.modern, proto,
            "nativization must have replaced the off-inventory ŋ"
        );
    }

    // ---- Merger-aware assignment: de-risk measurement (Stage 7 prep).

    /// A goblinoid `Envelope` from the shipped articulation values (species
    /// crate can't be imported here, so the numbers are transcribed).
    fn gob_env(labiality: f64, vowel: f64, voicing: f64, sib: f64, loud: f64) -> Envelope {
        Envelope {
            labiality,
            vowel_space: vowel,
            voicing,
            sibilance: sib,
            voice_loudness: loud,
            tonality: 0.0,
            exotic: ExoticSeg::None,
        }
    }

    /// The real shipped goblinoid family (proto + three daughters), rebuilt at
    /// `seed` from the transcribed envelopes.
    fn goblinoid_family(seed: u64) -> (Phonology, Vec<Daughter>) {
        let proto_ph = draw_phonology(
            &Seed(seed),
            "goblinoid",
            &gob_env(0.5, 0.5, 0.55, 0.45, 0.55),
        );
        let daughters = [
            ("goblin", gob_env(0.5, 0.5, 0.5, 0.5, 0.5)),
            ("hobgoblin", gob_env(0.5, 0.5, 0.6, 0.4, 0.8)),
            ("bugbear", gob_env(0.5, 0.4, 0.7, 0.2, 0.3)),
        ]
        .iter()
        .map(|(name, env)| Daughter {
            cascade: draw_cascade(&Seed(seed), name),
            phonology: draw_phonology(&Seed(seed), name, env),
        })
        .collect();
        (proto_ph, daughters)
    }

    /// The core concept universe (universal ∪ body ∪ kin), each with its domain.
    fn core_concepts() -> Vec<(&'static str, &'static str)> {
        crate::packs::universal_stratum()
            .iter()
            .chain(crate::packs::body_pack())
            .chain(crate::packs::kin_pack())
            .map(|e| (e.concept, crate::packs::concept_domain(e.concept).unwrap()))
            .collect()
    }

    /// `(all_core, confusable)` homophony pairs summed over the family: at each
    /// daughter, group core concepts' evolved forms and sum `C(n, 2)` over every
    /// shared form; the second figure restricts each count to same-domain
    /// members (the parsing-costly subset).
    fn core_homophony(
        assigned: &std::collections::BTreeMap<String, Vec<Segment>>,
        daughters: &[Daughter],
        concepts: &[(&'static str, &'static str)],
    ) -> (usize, usize) {
        let (mut all, mut confusable) = (0usize, 0usize);
        for daughter in daughters {
            let mut by_form: std::collections::BTreeMap<Vec<Segment>, Vec<&str>> =
                std::collections::BTreeMap::new();
            for (concept, domain) in concepts {
                let modern =
                    evolve(&assigned[*concept], &daughter.cascade, &daughter.phonology).modern;
                by_form.entry(modern).or_default().push(domain);
            }
            for domains in by_form.values() {
                let n = domains.len();
                all += n * n.saturating_sub(1) / 2;
                let mut counts: std::collections::BTreeMap<&str, usize> =
                    std::collections::BTreeMap::new();
                for d in domains {
                    *counts.entry(*d).or_insert(0) += 1;
                }
                for &c in counts.values() {
                    confusable += c * c.saturating_sub(1) / 2;
                }
            }
        }
        (all, confusable)
    }

    #[test]
    #[ignore]
    fn measure_merger_aware_core_homophony_reduction() {
        let concepts = core_concepts();
        let refs: Vec<&str> = concepts.iter().map(|(c, _)| *c).collect();
        let seeds = 0..200u64;
        let (mut base_all, mut base_conf, mut ma_all, mut ma_conf) =
            (0usize, 0usize, 0usize, 0usize);
        let (mut base_worlds, mut ma_worlds) = (0usize, 0usize);
        let (mut base_len, mut ma_len) = (0usize, 0usize);
        let n = seeds.clone().count();
        for seed in seeds {
            let (proto_ph, daughters) = goblinoid_family(seed);
            let base = assign_proto_roots(&Seed(seed), "goblinoid", &proto_ph, &refs, &[]);
            let ma = assign_proto_roots(&Seed(seed), "goblinoid", &proto_ph, &refs, &daughters);
            let (ba, bc) = core_homophony(&base, &daughters, &concepts);
            let (ma_a, ma_c) = core_homophony(&ma, &daughters, &concepts);
            base_all += ba;
            base_conf += bc;
            ma_all += ma_a;
            ma_conf += ma_c;
            base_worlds += usize::from(ba > 0);
            ma_worlds += usize::from(ma_a > 0);
            base_len += concepts.iter().map(|(c, _)| base[*c].len()).sum::<usize>();
            ma_len += concepts.iter().map(|(c, _)| ma[*c].len()).sum::<usize>();
        }
        println!(
            "MERGERAWARE (all-core) over {n} seeds, {} core concepts:",
            refs.len()
        );
        println!("  core-homophony pairs (family):  base={base_all}  merger-aware={ma_all}");
        println!("  confusable pairs (family):      base={base_conf}  merger-aware={ma_conf}");
        println!(
            "  worlds with any core pair:      base={base_worlds}/{n}  merger-aware={ma_worlds}/{n}"
        );
        println!(
            "  mean core-root length:          base={:.3}  merger-aware={:.3}  (Δ={:+.3})",
            base_len as f64 / (n * refs.len()) as f64,
            ma_len as f64 / (n * refs.len()) as f64,
            (ma_len as f64 - base_len as f64) / (n * refs.len()) as f64
        );
    }
}
