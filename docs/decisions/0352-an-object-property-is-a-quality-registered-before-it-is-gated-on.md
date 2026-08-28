# 0352. An object property is a Quality, and is registered before it is gated on

**Status:** Accepted (2026-08-27) · **Decider:** Nathan (at the G3 stop) ·
**Relates:** [0257](0257-felt-states-are-concepts.md) (the exact precedent —
a window type's concept pack, kept in step by a window-side test),
[0172](0172-a-concept-with-no-possible-referent-is-an-extradiegetic-gap.md),
[0246](0246-a-renamed-concept-keeps-its-serialized-spelling-forever.md),
[0025](0025-one-concept-name-one-owner.md) ·
[The Offer](../../book/src/chronicle/the-offer.md)

In the context of introducing a five-word property vocabulary that verbs gate
on, we decided that **each property is a `ConceptKind::Quality`, registered in
an appended accession cohort before any verb gates on it**, accepting drift in
three generated language artifacts.

## Context

The honest case for registering is narrower than the one an earlier draft
made. That draft argued "register now, because doing it later costs an extra
epoch" — false, since epochs are appended and epoch 13 and epoch 14 cost the
same. What is true is that shipping a property vocabulary the world cannot
name recreates, one layer down, exactly the problem The Actants fixed for
verbs: a culture must be able to have — or lack — a word for a thing the
mechanics turn on.

The severable cut was offered explicitly at G3 (the acceptance test does not
require registration) and declined.

**`Quality` over minting a new kind, against two recent counter-precedents.**
The Actants minted `Act` and The Confidant minted `Affect` rather than
overloading `Quality`, so the standing habit pointed the other way. Both were
minted for concepts that are *categorically* new — a thing done, a thing
undergone. A property a thing HAS is what `Quality` already denotes ("an
abstract property or attribute"), so neither precedent reaches this case, and
minting `Property`/`Affordance` would create the engine-vs-world vocabulary
carve-out the one-word-per-concept rule exists to prevent.

## The rule

The five properties — `affords-passage`, `encloses`, `holds-liquid`,
`radiates-heat`, `supports-rest` — are `Quality` concepts in **epoch 14**,
appended to `EPOCH_COHORTS` and never merged into an earlier cohort, per
`domains/language/src/accession.rs`'s absolute rule. The roster is kept in step
with `ObjectProperty` by a **two-way** test in `windows/vessel`, not by an
import, because a domain cannot depend on a window.

## Consequences

- The appended cohort was verified additive line by line: the five added lines
  are exactly the five new concepts at their alphabetical positions, and every
  pre-existing `name: Root /ipa/` entry is byte-identical. Stream isolation
  held, and is now evidenced rather than assumed.
- A future property must be registered in its own appended cohort before a
  verb gates on it. Registering after the gate ships leaves a mechanic the
  world has no word for, which is the condition this record exists to prevent.
- The concept-registry drift in `book/src/reference/` and the three language
  dumps is expected and lands in the same commit as the code.
