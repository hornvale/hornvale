# 0586. Every authored numeric constant declares its axis of variation

**Status:** Accepted (2026-09-02) · **Decider:** Nathan (autopilot) · **Campaign:** The Plumb

## Context

The Wicket shipped `FATIGUE_RISE` as a single authored `f64` governing how fast
every creature in the world tires. Nathan's standing note on it was general
rather than local: *"essentially everything should vary by species and might
have personal preferences as well … it's worth keeping an eye out for random
constants like this that should actually be components."*

The Wicket converted that one constant. Nothing stopped the next one. A magic
number in this codebase is not a style problem — it is a **fidelity ceiling
that nothing measures**: a value authored once and shared by every creature,
every people and every world silently asserts that the quantity does not vary,
and that assertion is invisible because it is written as an ordinary `const`.

## Decision

Every authored numeric constant in `domains/*/src` and `windows/*/src` carries a
`plumb:` tag naming the **rung** it sits on — the axis along which the value
varies:

| rung | varies by |
|---|---|
| `universal` | nothing — a physical or lattice constant |
| `per-world` | seed and pins; a property of the world, not the code |
| `per-species` | `KindId`; belongs in a species component table |
| `per-people` | what a people does; needs a kind-to-kind edge |
| `per-individual` | the individual; derived from `Lineage`, never stored |
| `pending(wave-N)` | not yet judged |

`tools/plumb` enforces this **default-deny**: an untagged constant fails
`plumb check`, which runs in `make gate-commit` and in the chamber's `gate`
phase. `docs/audits/plumb-roster.md` is the committed, drift-checked artifact.

Four properties are load-bearing:

1. **The five judged rungs require a reason; `pending` requires a wave.** A
   reasonless verdict is a parse error. This is what makes a tag an argument
   rather than a label.
2. **`pending(wave-N)` is honest and cheap, and the ratchet's value does not
   depend on the backlog being empty.** A gate that reddened on the whole
   population on day one would be ignored by everyone within a week — the same
   reasoning `tools/seam-guard` gives for its three-valued verdict and
   `type-audit` for its `pending(wave-N)` class.
3. **A `universal` verdict is per-constant and owes its reason.** Declaring a
   crate's constants `universal` in bulk because they sit in a physics crate is
   the inherited fixedness this decision exists to convert into a chosen one,
   performed at scale. Anything that cannot be reasoned about in half a line is
   `pending`.
4. **The audit reports; it does not convert.** A constant judged `per-species`
   or above is a **finding for Nathan**, published in the roster's Fidelity
   findings table with file, line, rung and reason. Fidelity is his call
   (decision 0021's shape); an audit that quietly re-tuned the world would be
   making it for him.

Identifiers and markers are out of scope — `KindId`, `ConceptKind`, `Realm`,
`Segment`, `Eyes`, `AffectLabel`, `ChannelMask`, `HabitatRealm`, `Transmission`
name a thing rather than measure one, and "does `kinds::HEARTH` vary by
species?" is a category error rather than an open question. Composite
parameter bundles are firmly **in** scope, and the distinction is not the shape
of the initializer: `MANIKIN` is a struct literal of several ratios whose own
doc already names its axis, and excluding it by shape would have dropped the
constants most worth judging.

`kernel/` and `cli/` are outside the audited roots. Whether to widen is
deferred; `plumb check kernel cli` reports 59 further constants.

## Consequences

- The next `FATIGUE_RISE` cannot land untagged.
- The first sweep declared **681** constants (686 after absorbing main):
  85 `universal`, 19 `per-species`, 2 `per-world`, 1 `per-people`,
  2 `per-individual`, 572 `pending(wave-1)`. **27 findings** reached the
  roster's Fidelity table.
- Among them, the campaign's own predecessor: `fatigue_rise_registry` gives
  every kind the identical `0.3` (only `xorn` differs, explicitly `0.0`), so
  The Wicket built the per-species **mechanism** and left the **values**
  uniform. The audit found this in the code the previous campaign had its
  hands in.
- **A verdict's reason must name an AXIS, not a PROVENANCE.** Four of the first
  90 `universal` verdicts answered "where did this number come from?"
  (*"a calibration knob, not a physical constant"*; *"a game-design
  coefficient"*) in place of "along what does it vary?" — a confident answer to
  a neighbouring question, indistinguishable from an answer to this one. The
  tell is a reason built on the word *not*, defining itself against a category
  instead of naming an axis. Nothing mechanical detects this; only reading the
  reason against the question does.
- Every branch landing after this decision pays a share of the tag cost. The
  first was this campaign's own absorption: 325 commits from main brought 17
  undeclared constants, and the ratchet caught all of them.
- Cost: the `plumb` pair (check plus roster freshness) is ~7.1 s warm, against
  ~10.9 s for the `type-audit` pair beside it.
