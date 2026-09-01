# The Wicket — one kind model, not two

**Date:** 2026-09-01 · **Registry row:** `MAP-one-kind-model`
(`elaborated`, high) · **Essay:**
[frontier §the orange problem](../../../book/src/frontier/frontier.md#the-orange-problem--one-kind-model-not-two)
· **Ledger:** `docs/superpowers/ledgers/2026-09-01-the-wicket.md`

A wicket is a small gate set in front of open ground. This campaign takes one
down.

## 1. The problem, measured

Hornvale ships two kind models. A **species** is `KindId("goblin")` plus
whatever component tables happen to carry a row at that key — `biosphere_registry`,
`psyche_registry`, `perception_registry`, `family_of`, `habitat_realm`. Nothing
holds "the definition of a goblin"; a goblin is the join, and a species no table
mentions is a key with no consequences.

An **object** reaches the same shape through a fence. `affordance::object_registry()`
is a `ComponentStore<KindId, ObjectTraits>` — open, string-keyed, exactly the
species shape — and `hornvale_thing::thing_registry()` is another, already joined
into `WorldComponents`. But the vocabulary a room's grammar can name is
`AnchorKind`, a closed enum declared by the `anchor_kinds!` macro. A kind the
enum does not name cannot be placed in a room, however open the stores behind it
are.

### 1.1 The essay's numbers are wrong, and the correction is part of the work

The essay and the registry row both say "thirty-four variants, thirty-six
exhaustive match sites, seventeen files." Measured on this branch, and again at
`208efe4f1` (the essay's own commit, 2026-08-31):

```text
                                  claimed        measured today   measured at 208efe4f1
  AnchorKind variants             thirty-four    15               15
  exhaustive match sites          thirty-six      3               --
  files mentioning AnchorKind     seventeen      18 src / 22 all  --
```

The three exhaustive sites are `chamber_prose::noun`, `chamber_prose::detail`
and `affordance::thing_kind_of`. The fence is roughly 2.3x smaller than
advertised, and it was already 15 variants on the day the sentence was written —
so this is a drafting error, not drift. Both the essay and the registry row are
corrected as part of this campaign's DoD (§11).

### 1.2 What the fence actually costs

Fifteen variants, three exhaustive matches, and about 340 mentions across 18
source files — but the production predicates are almost all one shape:

```text
  interior/field.rs:70    kind != AnchorKind::Hearth        (warmth falls off from fires)
  interior/seam.rs:61,66  kind == AnchorKind::Threshold / Ground
  interior/derive.rs      kind == AnchorKind::Hearth / Threshold
  interior/pattern.rs     Pattern { kind, attach, requires } — the grammar's vocabulary
  chamber_prose.rs        noun / detail — the two exhaustive matches
  affordance.rs           thing_kind_of — the third, the adapter to KindId
```

Everything else is tests. `liveness.rs`'s 53 mentions and `session.rs`'s 34 are
test-module constructions of hand-built interiors.

## 2. What must survive

Four properties are load-bearing and the design is shaped by them.

**Totality.** Every kind that can be placed in a room has a noun (or a declared
absence of one — `Ground` is the floor, not a thing standing in it), a detail
line, and a thing-kind row. Today the compiler enforces this by exhaustiveness.
That guarantee is the point of the enum; the closedness is only the means.

**Byte-identity.** Same seed and pins, byte-identical worlds. The re-key must
not reorder an interior.

**Layering.** `kernel` → `domains/*` → `windows/*` → `cli`. A domain may not
depend on a sibling; a window may depend on a domain.

**Failing loudly.** A kind with no prose row must be a refusal with a physical
reason, never a silent default. `unwrap_or(default)` is the failure mode a map
has that a match does not.

### 2.1 The re-key is ordering-safe, verified

Three `BTree*` collections in this workspace are keyed on `AnchorKind`, and
every one is lookup-only:

```text
  pattern.rs:562   present: BTreeSet<AnchorKind>          contains / insert only
  pattern.rs:631   first_of: BTreeMap<AnchorKind, AnchorId>  get / entry only
  pattern.rs:909   counts: BTreeMap<AnchorKind, usize>     (a test)
```

Interior order comes from `selected`, a `Vec` in `INVENTORY` declaration order,
and `compose` pushes in that order. `AnchorKind::ALL` — the generated roster —
is read only by tests. So changing the key type from an enum ordered by
declaration to a `KindId` ordered lexically changes no iteration anywhere, and
the re-key alone moves no bytes. This is stated as a verified fact rather than
an expectation, and Task 2's acceptance criterion is the empty diff that
demonstrates it.

## 3. Non-goals

- **Kind-to-kind edges.** `family_of` is a single edge type flattened to a
  column; the general form is a graph over `KindId`. That is the second of
  MAP-one-kind-model's three additions and it is not in this campaign (§8
  records why the sequencing argument survived an ideonomy pass that pushed
  against it).
- **Per-instance components derived from `Lineage`.** The third addition.
- **Systems, schedulers, archetype storage.** The essay's "where the ECS
  analogy stops" is settled and this campaign does not reopen it: a scheduler
  would be a second cross-domain communication mechanism competing with the
  trace protocol, and iteration order would become an undeclared save-format
  contract.
- **Runtime-generated kinds.** `KindId(pub &'static str)` — kinds are authored
  and committed, which is the "models author, dice roll" constraint. "Kinds as
  data" here means *authored data*, never *derived at runtime*.

## 4. Design: a kind is a row

### 4.1 The roster already exists

`hornvale_thing::THING_KINDS` is a `&[&str]` roster of authored labels, and
`roster_and_registry_agree_in_both_directions` already pins it against
`thing_registry()` in both directions. It carries **16** rows: one for each of
the 15 `AnchorKind` variants, plus `cave-mouth` — a kind no variant has ever
backed, because a cave mouth is a `Vertex`/`ChamberAddr` and was never
expressible in the enum. That one row is the openness this campaign
generalises: the store behind the fence already holds a kind the fence cannot
name. The destination is built; the work is pointing the grammar at it.

(`affordance.rs`'s own doc says `key` and `cave-mouth` are both rows with no
`AnchorKind` behind them. That was true when Task 7 wrote it and stopped being
true four tasks later, when Task 11 added the `Key` variant. Verified against
the enum, not the comment.)

### 4.2 Named handles, not variants

Production predicates must not become stringly typed. `kind == KindId("hearht")`
compiles. So `domains/thing` publishes named constants beside the roster:

```rust
pub const HEARTH: KindId = KindId("hearth");
pub const THRESHOLD: KindId = KindId("threshold");
pub const GROUND: KindId = KindId("ground");
// ... one per kind that code names
```

and a test asserts every published constant's label is a `THING_KINDS` row.

**The asymmetry between a constant and a variant is the whole campaign.** A
variant is *mandatory* — a kind with no variant cannot exist. A constant is a
*convenience for code that must name the kind* — a kind with no constant is a
first-class kind that simply has no predicate written against it. Adding a kind
is a data edit: a `THING_KINDS` row, a `thing_registry` row, a prose row, and
optionally an `object_registry` row. No enum, no match arm, no macro.

### 4.3 The interior grammar re-keyed

`Anchor::kind`, `Interior::push`, `Pattern::kind`, `Pattern::requires` and
`Attach::{Beside, Within}` all take `KindId`. `anchor_kinds!`, `AnchorKind`,
`AnchorKind::ALL` and `affordance::thing_kind_of` are deleted — `thing_kind_of`
is an adapter between two vocabularies, and after this there is one.

`windows/vessel` gains a `hornvale-thing` dependency. This is layering-legal (a
window may depend on a domain; vessel already depends on eight of them) and it
lets `cli/tests/suite/anchor_thing_correspondence.rs` — which lives in `cli/`
only because `cli` was the one crate depending on both — become a vessel test or
disappear into the totality gate.

### 4.4 Prose becomes a component table

`chamber_prose::noun` and `chamber_prose::detail` become one vessel-local store:

```rust
pub struct ChamberProse {
    /// How prose names this kind, or None for a kind that is the room itself
    /// rather than a thing standing in it (the floor).
    pub noun: Option<&'static str>,
    /// The line examine gives, total where noun is not.
    pub detail: &'static str,
}

pub fn chamber_prose_registry() -> ComponentStore<KindId, ChamberProse>
```

Prose is presentation, so it stays in the window; `display` stays in
`domains/thing` where it already is. The lookup returns `Option` and callers
refuse rather than default (§5.2).

## 5. The totality gate

This is the deliverable. The compiler stops enforcing totality; a default-deny
test takes over, and it must be *better* than what it replaces, not merely
present.

### 5.1 The direction each check enforces, named in its own doc comment

A gate asserting *declared ⊆ resolvable* is structurally blind to
over-admission and still reads as total to the next reader. So each check says
which way it runs:

```text
  G-a  every KindId an INVENTORY Pattern names — kind, requires,
       Attach::Beside/Within target — is a THING_KINDS row      (declared ⊆ rostered)
  G-b  every THING_KINDS row has a chamber-prose row            (rostered ⊆ prosed)
  G-c  every chamber-prose row is a THING_KINDS row             (prosed ⊆ rostered)
  G-d  every published KindId constant is a THING_KINDS row     (named ⊆ rostered)
  G-e  every object_registry key is a THING_KINDS row           (propertied ⊆ rostered)
  G-f  THING_KINDS is frozen as an ORDERED SET, not a count
```

G-f exists because a size ratchet passes any compensating swap: a count is not a
membership. The frozen list is the ratchet, so adding a kind is a visible,
deliberate edit to a committed roster — the same discipline `AnchorKind::ALL`
bought by being generated, kept without the closedness.

There is deliberately **no** check that every rostered kind is placeable by some
pattern. `cave-mouth` is a rostered kind that is a `Vertex`/`ChamberAddr` and
never an anchor, and inventing an exemption list to keep a reverse check green
would be a list nobody maintains.

### 5.2 Refusal, not default

`compose` and the prose surfaces take a kind's prose by lookup. A miss is a
refusal naming the kind, not a fallback string. G-b makes a miss unreachable
from the authored inventory; the refusal is what makes the unreachable case
loud if G-b is ever weakened.

### 5.3 The mutations these tests must fail against

Per decision 0353, each gate is specified by the mutation it must catch. These
are named as *properties*, and the implementer finds the concrete mutation by
reading — a plan author does not know which mutation is discriminating.

- A pattern naming a kind with no roster row must redden G-a. (Today the
  compiler catches this; after the re-key nothing else does.)
- Deleting one prose row must redden G-b, and adding a prose row for a
  non-existent kind must redden G-c. Both directions, because the cheapest
  repair to a one-way check deletes the check.
- Appending a kind to `THING_KINDS` without updating the frozen ordered set must
  redden G-f; swapping two rows without changing the count must also redden it.
- Replacing a prose lookup's refusal with a default string must redden a test
  that stands a kind with no prose row in a room.

## 6. The proof: a kind the enum could not have named

Decision 0398 — *a capability nothing can reach is not a capability* — was
minted on 2026-08-29 against exactly this shape: The Chattel shipped a
container, a lock, a key and an `open`/`close` pair no session in any world
could stand in front of. An open kind vocabulary that places nothing new is the
same failure. So the campaign places one kind, and the diff that adds it is the
evidence: a `THING_KINDS` row, a `thing_registry` row, a prose row, an
`object_registry` row, and one `Pattern`. No enum edit, no match arm, no macro.

**The kind: `kneeler`.** A shrine chamber (`Role::Shrine`, drawn at chamber
index 2 of a place whose history function is `Cult`) draws exactly four
patterns: `the-ground`, `the-threshold`, `the-water-jar` (Shrine is in
`STORING_ROLES`) and `the-altar`. It affords passage, drinking and examining,
and it offers nowhere to rest.

`SupportsRest` is carried by `bed` alone, and the chain that confines a bed runs
`the-fireside-bed` requires `Hearth` → `the-fire` requires `Alcove` →
`the-alcove` declares `roles: &[Role::Hearthroom]`. So although the bed itself
declares `EVERY_ROLE`, no role but Hearthroom can produce one. A kneeler beside
the altar is a place to kneel at a rite: it earns its place by the activity it
affords, which is `anchor.rs`'s own standing rule ("an object earns a place here
by the activity it affords, never by decoration"), and it is the first place
outside a hearthroom where a body can rest.

```text
  the-kneeler   kind: kneeler   roles: [Shrine]   built: true
                attach: Beside(altar)   requires: altar
                at_locale: false        properties: [SupportsRest]
                noun: "a kneeler"
```

**Appended, never inserted, and after `the-altar`.** `INVENTORY` is a
fixed-size `[Pattern; 16]` and `draw` admits a pattern only once its `requires`
kind is already present, so the order IS the dependency order: inserting or
reordering is an epoch, and a kneeler placed before the altar it requires would
be silently dropped from every composition. The array's length becomes 17 in the
same edit.

**`at_locale: false` is the reason this is affordable.** `INVENTORY`'s own doc
states the three-part epoch rule: reordering is always an epoch; appending with
`at_locale: true` is an epoch, because a locale composition feeds `warmth_at`,
which feeds a creature's thermal drive, which is committed history; appending
with `at_locale: false` is **latent** — `selection` filters it out and the only
other consumer, `selection_for`, is read by the chamber renderer and by nothing
that commits. A chamber-band kneeler therefore changes what a player sees on
delving and commits nothing. §7 states this as a branch table rather than a
prediction.

**Alternatives discarded.** A wilderness rest object (`bracken`) would close a
larger gap — no wild room affords rest at all — but it draws in every wild
locale-band interior in every world, which is a content change of a size that
does not belong riding on a refactor. It becomes a registry row instead (§11).
A test-only kind was rejected under 0398.

**The blast radius is measured, not predicted.** The task that adds the kneeler
reports the count of shrine chambers at the three census seeds *before* it adds
the pattern, and the artifact diff *after*, under §7's branch table. If the
measurement shows shrines are unreachable at those seeds — the 0398 failure
repeating one level down, which is exactly what `needs_populous` turned out to
be — the kneeler moves to a role that is reachable, and the measurement is the
finding either way. `Role::Shrine` requires `Function::Cult` at chamber index 2,
so the count is a real question and not a formality.

## 7. Artifacts and determinism

- §2.1's verified argument says the re-key alone moves no bytes. Task 2's
  acceptance criterion is a `git diff --exit-code` over the declared generated
  paths coming back **empty** after `make rebaseline`, with the branch table
  written out rather than the expectation: *nothing moved → proceed;
  `docs/audits/` alone moved → the type-audit report drifted on a pub-boundary
  change, regenerate and commit in the same commit; anything under
  `book/src/gallery/` or a census CSV moved → STOP, the re-key is not
  behaviour-preserving and that is a finding.*
- The kneeler's artifact surface is **measured at Task 4, not predicted**, and
  the branch table is the deliverable: *nothing moved → proceed and say so;
  `clients/game/core/tests/fixtures/` moved → the chamber-band session snapshot
  picked up the new anchor, refresh and commit in the same commit; a census CSV
  or `book/src/domesday/` moved → STOP, something that commits now reads a
  chamber, `INVENTORY`'s latency rule has lapsed and the append was an
  undeclared epoch — that is a finding and the campaign's headline, not a
  rebaseline.*
- No save-format contract changes. `AnchorKind` derives no `Serialize` or
  `Deserialize` (checked on the type, not inferred from a doc comment), so no
  variant ever reaches a byte. The `threshold`/`floor`/`wall`/`furnishing`
  strings in the committed chamber fixture are `lattice::render` **plan mark**
  kinds, not `AnchorKind` values, and are unaffected by the re-key. No stream
  label changes; no seed-derivation change; the world file is untouched.
- The clients are unaffected: `clients/game/core` draws one `FURNISHING_GLYPH`
  for every furnishing kind by deliberate design (`CLIENT-glyphs-22-rejected`),
  so widening the vocabulary is invisible across the wasm ABI.

## 8. What an ideonomy pass changed

Operators: abstraction-lift → cross-domain re-instantiation. Prompts: purpose,
symmetry, materiality.

**Lifted form.** *An open commons reachable only through a closed admission
list, sized for an earlier and smaller world.* A permit regime, not a capacity
limit.

**Drug scheduling.** A controlled-substance schedule is a closed enumeration,
and a compound not on it does not legally exist; the analogue problem forced a
*structural* rule — substantial similarity — to replace the enumeration. Carried
back: without inheritance, every new kind must author every row by hand, so
kind-to-kind edges are the *rescue* for an open vocabulary, not merely the next
item on a list. This is a genuine argument for bundling addition two into this
campaign, and it loses on scale: 15 kinds do not make hand-authoring expensive,
and the essay's order-of-least-regret argument (three working mechanisms
risked for two missing ones) still governs. It is a reason to sequence edges
next, and it is recorded as such.

**Immunology.** Germline-encoded pattern recognition is closed and small;
adaptive immunity is open and enormous, and survivable only because of thymic
negative selection. Carried back: an open vocabulary is safe exactly to the
degree its default-deny gate is real — §5, and the reason it is the deliverable
rather than a supporting task.

**Faceted classification.** Ranganathan composes a subject from facets where
Dewey locates it in an enumeration, and the faceted catalogue still needs a
caption per composed class and still has to shelve the book. Carried back: the
prose row is the caption (§4.4), and an open vocabulary changes nothing
observable unless something can actually draw the new kinds — §6.

**Purpose (the overturn).** `AnchorKind` carries two inherited purposes and only
one is load-bearing: it is the grammar's vocabulary, and it is the guarantee
that every placeable kind is total in prose and thing-kind. Closedness serves
the second and is a poor instrument for it at any scale. So the campaign's
deliverable is the totality mechanism, and deleting the enum is the consequence.
This reframing decided §5.

**Materiality.** The enum conflates a region (`Ground`, `Alcove`), a graph seam
(`Threshold`) and an object (`Key`, `Altar`). Under composition that conflation
dissolves for free — a kind is a region exactly by carrying no `ObjectTraits`
row, which is already true of `screen`, `ground` and `log`. No `Role` field is
added to `Pattern`; this paragraph exists so nobody adds one later thinking it
was overlooked.

## 9. Risks

- **A stringly-typed predicate.** Mitigated by §4.2's named constants plus G-d.
  The residual risk is a `KindId("...")` literal written inline in production
  code; a grep-based check for `KindId(` outside the roster module is cheap and
  is a task decision, not a spec mandate.
- **Prose lookup cost.** Fifteen short-string comparisons per anchor rather than
  enum discriminants, on a per-chamber derivation. Expected negligible; the
  duration baseline is the instrument and a regression there is the signal.
- **The totality gate is weaker than the compiler in one direction.** The
  compiler refused to build; a test refuses to pass. That is a real reduction
  and it is the price of the openness, which is why G-a through G-f are
  specified by their mutations rather than by their existence.
- **Scope creep into edges.** §3 and §8 both name it. The registry row is
  amended at close rather than the campaign widening.
- **The one thing the compiler still catches.** `INVENTORY` is a fixed-size
  array, so appending a pattern forces its length to change and cannot be done
  silently. That protection is unaffected by this campaign and is worth naming,
  because the rest of it argues for replacing compiler guarantees with tests and
  a reader could reasonably conclude none survive.

## 10. Stages

1. **Handles and roster.** `domains/thing` publishes named `KindId` constants;
   G-d and G-f land against the existing roster. No vessel change. Green on its
   own.
2. **The re-key.** `Anchor`, `Interior`, `Pattern`, `Attach`, the four interior
   predicates, `affordance`; `anchor_kinds!` and `thing_kind_of` deleted; vessel
   gains the `hornvale-thing` dependency. G-a lands. **Acceptance: the generated
   artifacts do not move** (§7's branch table).
3. **Prose as a table.** `chamber_prose` becomes `ComponentStore<KindId,
   ChamberProse>`; G-b, G-c and the refusal land. The last exhaustive match is
   gone.
4. **The kneeler.** One kind, five data rows, one appended `Pattern`, no
   control flow. Reachability measured before, artifact diff read after under
   §7's branch table.
5. **DoD.** §11.

## 11. Definition of done

- Chronicle entry (`book/src/chronicle/the-wicket.md`) and a freshness sweep.
- **The essay and the registry row are corrected** — the thirty-four/thirty-six/
  seventeen sentence is replaced with the measured numbers and a note that it
  was wrong at authoring, not stale. This is the loud-correction discipline: a
  published measured fact that is wrong produces wrong cost estimates from
  readers acting in good faith.
- `MAP-one-kind-model` moves to reflect that addition one has shipped, with
  addition two (edges) carrying §8's inheritance argument.
- A new registry row for the wilderness-rest gap discarded in §6.
- Retrospective (`docs/retrospectives/the-wicket.md`).
- Decision records for: the totality-by-registry rule (§5, and the direction
  each check enforces), and the constant-is-a-convenience/variant-is-mandatory
  asymmetry (§4.2) — both bind future campaigns and so need records rather than
  ledger entries.
- Census refreshed once at pre-merge close on lefford; artifacts regenerated;
  merged through the sluice.
