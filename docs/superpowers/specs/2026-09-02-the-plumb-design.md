# The Plumb — the ladder, and the fence one layer over

**Date:** 2026-09-02 · **Follows:** [The Wicket](2026-09-01-the-wicket-design.md)
· **Registry rows:** `MAP-the-concept-side-is-still-closed`,
`TOOL-authored-scalar-should-be-a-component` ·
**Ledger:** `docs/superpowers/ledgers/2026-09-02-the-plumb.md` ·
**Decision block:** 0586-0595

A plumb line is a fixed reference used to check whether anything else is true.
This campaign is about which fixed references should stop being fixed.

## 1. Where this sits in a four-campaign arc

The Wicket closed `MAP-one-kind-model`'s first addition and left five things
named. Nathan ruled on all five (2026-09-02); four of them are wanted, and they
do not fit in one campaign:

| | campaign | what it needs that does not exist |
|---|---|---|
| **A** | **The Plumb — this one.** The concept fence, and the ladder audit with `REST_BOUT` as its worked example | nothing; both are refactors of what The Wicket exposed |
| B | A body chooses where to sleep | creatures must *select* a rest site — a behaviour, not a data change |
| C | Kinds point at kinds | `MAP-one-kind-model` addition two |
| D | Derived per-instance | addition three |

**The ordering is not the obvious one and it is load-bearing: B precedes C and
D.** A preference is meaningless where nothing chooses. *Goblins prefer
bracken* never expresses in a world where every creature sleeps where it stands,
so building the preference before the choosing would ship a mechanism with no
observable effect — the shape decision 0398 refuses.

**One correction this spec owes.** The close of The Wicket told Nathan that
making the rest grade finer required amending decision 0069. That was
overstated. 0069 forbids serializing *fine position*; recording *which kind of
thing a body slept on* is not a position, and two beds in a room stay
indistinguishable under it. Campaign B is a behaviour question, not a
constitutional one.

## 2. What this campaign delivers

Two things, and the second subsumes what Nathan listed as a separate item.

1. **The concept fence comes down.** `domains/thing::concept_doc` is an
   exhaustive match whose missing arm panics **world genesis**, and
   `hornvale_language::EPOCH_COHORTS` is a hand-maintained list whose missing
   entry silently defaults a concept to epoch 0. Both are the shape The Wicket
   removed from `windows/vessel`, standing one layer over in two other crates.
2. **The ladder audit**, a `tools/`-level sweep on the `type-audit` model, whose
   verdicts are the five rungs of §4's ladder — with `REST_BOUT`'s conversion as
   its first finding and worked example.

## 3. The concept fence

### 3.1 `concept_doc` becomes a row

```rust
pub struct ThingTraits {
    pub display: &'static str,
    pub doc: &'static str,      // new
}
```

`concept_doc`'s single consumer is `register_concepts`
(`domains/thing/src/lib.rs:343`, `doc: concept_doc(label).to_string()`), which
already holds the row. So the function, its 17-arm match and its
`other => unreachable!` all delete outright, and **coverage becomes structural**:
`roster_and_registry_agree_in_both_directions` already guarantees every roster
label has a registry row, and a row without a `doc` will not compile.

That is the same trade The Wicket made for prose, and it is strictly better
here: prose needed a new table, this needs a field on a table that exists.

**One wrinkle the current panic message names.** A BORROWED kind (`hearth`, ceded
to `settlement` by decision 0025) never reaches `concept_doc` today. Decide at
implementation whether its row carries a doc that is never read, or the field is
`Option<&'static str>` with `BORROWED` implying `None` — read `BORROWED`'s own
handling before choosing, and state the reason. Do not infer it from this
paragraph.

### 3.2 `EPOCH_COHORTS` keeps its shape and gains a gate

`concept_epoch` (`accession.rs:614`) walks the cohorts and **returns 0 on a
miss** — the silent default that makes a forgotten cohort invisible.

The cohort list is **not** made derivable, and that refusal is the design. An
epoch is a *historical* fact — when a concept entered the world — and history is
not recomputable from the present registry. Deriving it would mean inventing it.

Instead: a **default-deny gate** asserting every registered concept appears in
exactly one cohort. Omission then fails loudly at test time instead of
defaulting silently at runtime. This is The Wicket's totality-gate pattern
applied unchanged, and like those gates it must **state the direction it
enforces** in its own doc comment.

`concept_epoch`'s `0` fallback stays as a runtime answer — the gate makes it
unreachable from the authored set, and a refusal at that depth would ripple
through every caller for a case the gate already prevents.

## 4. The ladder audit

### 4.1 The verdicts are a ladder, not a boolean

```text
  rung             example                        varies by
  universal        the tick lattice, physics      nothing
  per-world        day length, REST_BOUT          seed and pins
  per-species      sleep-debt rate, body mass     KindId
  per-people       what a people sleeps on        a kind-to-kind edge
  per-individual   this one likes a sleeping bag  derived from Lineage
```

This is `MAP-one-kind-model`'s three additions seen from the numeric side.
Making it the verdict set means **a constant tagged `per-people` is a registered
consumer for campaign C** — the audit is the arc's backlog, not merely a lint.

A tag names the rung and its reason, in the shape `type-audit` already uses:

```rust
/// plumb: universal(the lattice is a kernel constant, not a world property)
/// plumb: per-world(the day length is drawn per seed)
/// plumb: pending(wave-1)
```

### 4.2 The output is coverage, not detection

**The tool cannot predict which constants are wrong, and is not asked to.**
`FATIGUE_RISE` was found by Nathan in conversation, not by a sweep, and nothing
in a constant's syntax distinguishes a physics term from a mis-generalised
creature trait. So success is a **total, declared population** — every numeric
`const` in the audited scope carrying a verdict — and the value is that an
inherited fixedness becomes a chosen one.

That is `type-audit`'s philosophy exactly, and it is why the population size is
not the obstacle it first appears: 610 numeric consts exist workspace-wide and
269 sit in files touching a species or a body, but most are trivially
`universal` and declare themselves in one word.

### 4.3 Scope by contested-ness, and phase the backlog

Scope by **how arguable a rung is**, not by which file a constant sits in.
Blanket-declare the crates where the answer is never contested (the kernel's
lattice, terrain and astronomy physics); spend the effort on the
creature-modelling middle.

`pending(wave-N)` is a **ratified verdict class in `tools/type-audit`**, so a
phased backlog has precedent here rather than needing one invented. Wave 1 is
the contested middle; everything else may be declared in bulk or left `pending`.

**The ratchet is what matters, not the backlog.** A *new* numeric const with no
verdict fails; that is the property that stops the next `FATIGUE_RISE`. A
campaign that tagged every one of the 610 and shipped no ratchet would have
bought nothing.

### 4.4 `REST_BOUT` is the worked example

`REST_BOUT` (`liveness.rs:2398`) is `TickSpan::from_ticks(TICKS_PER_STD_DAY / 4)`
— a conscious rest lasting a fixed quarter of a *standard* day on a planet with
no standard day. Its own doc asserts a calibration — repayment must exceed
`HYSTERESIS_H` — which The Wicket's final review showed fails at any local day
longer than 1.25 standard days, roughly 30 hours, on worlds that legally admit
100.

It is a **per-world** constant, so it converts the way the fatigue rise and fall
terms already did. Doing it inside this campaign proves the ladder's second rung
is real rather than asserted, and closes a named regression.

## 5. Non-goals

- **Deriving the accession epochs.** §3.2 refuses it on principle, not on cost.
- **Campaigns B, C and D.** Named in §1 and sequenced; none is started here.
- **Tagging all 610 constants.** The ratchet is the deliverable; the backlog is
  phased and may stay `pending` indefinitely without weakening it.
- **A `component-audit` that infers rungs.** It declares; it does not guess.

## 6. Testing

- Both fence changes carry the direction they enforce in their own doc comments
  (The Wicket's decision 0556).
- Every test names the mutation it must fail against and pastes the observed red
  (decision 0353) — and, per that campaign's own finding, **precision in a
  failure message is not evidence the check performs what it says**. Run every
  mutation.
- The `concept_doc` deletion must be proved *behaviour-preserving*: the concept
  registry dump is a committed artifact, so the acceptance criterion is an empty
  diff over the declared generated paths.
- The audit's ratchet needs a positive control: a new untagged const must fail,
  demonstrated, not assumed.

## 7. Definition of done

Chronicle, retrospective, freshness sweep, Confidence Gradient re-score or an
explicit N/A, registry flips (`MAP-the-concept-side-is-still-closed` and
`TOOL-authored-scalar-should-be-a-component` both move), decision records from
block **0586-0595**, and a merge through the sluice.
