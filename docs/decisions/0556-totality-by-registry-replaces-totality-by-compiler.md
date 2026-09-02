# 0556. Totality by registry replaces totality by compiler, and every check names its direction

**Status:** Accepted (2026-09-01) · **Decider:** Nathan (autopilot) ·
**Relates:** [0353](0353-a-regression-test-is-specified-by-the-mutation-it-must-fail.md),
[0398](0398-a-capability-nothing-can-reach-is-not-a-capability.md),
[0557](0557-a-handle-is-a-convenience-where-a-variant-was-a-requirement.md) ·
**Ledger:** `docs/superpowers/ledgers/2026-09-01-the-wicket.md` #3, #15, #17,
#22, #23, #26, #27 · **Spec:** The Wicket §5

In the context of retiring `AnchorKind` — a closed enum whose exhaustive
matches were the only thing guaranteeing that every kind placeable in a room
had a noun, a detail line and a thing-kind row — we decided that **a closed
vocabulary's totality guarantee is replaced by a set of default-deny registry
checks, each of which states in its own doc comment which direction it
enforces**, accepting that a test refusing to pass is a weaker instrument than
a compiler refusing to build.

## Context

`AnchorKind` carried two inherited purposes and only one was load-bearing. It
was the pattern grammar's vocabulary, and it was the guarantee of totality.
Closedness serves the second and is a poor instrument for it: it buys totality
by forbidding growth, which is the cost the campaign existed to remove.

Naming the deliverable this way — *the totality mechanism is the product;
deleting the enum is the consequence* — is what decided the test strategy.
Framed as a refactor (`s/AnchorKind/KindId/`) the campaign would have shipped
the re-key with no replacement for the three exhaustive matches' guarantee.

## What was decided

**Six checks, each naming its own direction.** A gate asserting *declared ⊆
resolvable* is structurally blind to over-admission and still reads as total to
the next reader, so the direction is written at the check rather than inferred
from its name:

| | check | direction |
| --- | --- | --- |
| G-a | `every_kind_the_grammar_names_is_a_roster_row` | declared ⊆ rostered |
| G-b | `every_roster_kind_has_chamber_prose` | rostered ⊆ prosed |
| G-c | `every_chamber_prose_row_is_a_roster_kind` | prosed ⊆ rostered |
| G-d | `every_named_handle_is_a_roster_row` | named ⊆ rostered |
| G-e | `every_propertied_kind_is_a_roster_row` | propertied ⊆ rostered |
| G-f | `the_roster_is_frozen_as_an_ordered_set` | the roster itself |

G-b and G-c are deliberately a pair. The cheapest repair to a one-way check is
to delete the check, and a one-way prose gate is repaired by deleting the prose
row rather than by adding the missing one.

**G-f freezes an ORDERED SET, not a count.** A size ratchet passes any
compensating swap — a count is not a membership — so the frozen list is what
makes adding a kind a visible, deliberate edit to a committed roster. This is
the discipline `AnchorKind::ALL` bought by being generated, kept without the
closedness. For the same reason the frozen verb table sweeps `THING_KINDS`
rather than `EVERY_HANDLE`: the roster cannot go short, the handle list can
(ledger #15).

**There is deliberately no reverse check that every rostered kind is
placeable.** `cave-mouth` is a rostered kind that is a `Vertex`/`ChamberAddr`
and never an anchor, and inventing an exemption list to keep such a check green
would be a list nobody maintains. The same reasoning declined a source-text
guard against bare `KindId("…")` literals: the census found **58** occurrences
and **zero** at a production consumer site — every one is an authoring table, a
test, or a doc comment — so the guard would have shipped pre-loaded with a
two-table allow-list (ledger #22).

**A miss is a refusal, not a default.** `unwrap_or(default)` is the failure
mode a map has that a match does not, so the prose lookup refuses by naming the
kind. G-b makes the miss unreachable from the authored inventory; the refusal
is what makes the unreachable case loud if G-b is ever weakened.

## Consequences

- **The reduction is real and is the price of the openness.** The compiler
  refused to build; a test refuses to pass. Each gate is therefore specified by
  the mutation it must catch (decision 0353) rather than by its existence, and
  both directions of the prose pair were reddened by hand.
- **A static enumeration of ratchets is a floor, never a total.** The spec
  listed six; running found two more. `domains/thing::concept_doc` ends in
  `other => unreachable!`, so a roster kind with no doc arm panics **every world
  genesis** — reachable only at runtime and invisible to a grep over count
  assertions (ledger #26). Registering a kind as a concept then obligated an
  accession cohort in `hornvale_language::EPOCH_COHORTS`; without one every
  genesis defaults the kind to epoch 0 (ledger #27).
- **So the campaign's cost claim is narrower than it was drafted.** *Adding a
  kind touches no dispatcher, no enum and no match arm* is true **of the room
  grammar's path**, and the concept path still holds two closed lists of its
  own, one layer over, in two other crates. The narrower claim is the one this
  record ratifies.
- **A guard written against a spelling outlives the spelling.** Two existing
  guards (`no_verb_by_object_table_exists`,
  `no_hardcoded_anchor_kind_gates_warm`) searched source text for `AnchorKind::`
  — a literal the re-key makes impossible anywhere — so both would have read
  green in every possible tree forever. Repointed at `kinds::` (ledger #17).
  Any future source-text gate inherits this: it must be reddened against the
  vocabulary that exists **after** the change it guards.

## See also

`windows/vessel/tests/suite/kind_totality.rs` (G-a, G-b, G-c, G-e);
`domains/thing/src/lib.rs` (`THING_KINDS`, `kinds`, G-d, G-f);
`windows/vessel/src/chamber_prose.rs` (the refusal).
