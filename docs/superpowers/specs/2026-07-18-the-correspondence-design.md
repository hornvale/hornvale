# The Correspondence — Design

**Date:** 2026-07-18
**Status:** Draft — awaiting G3 review (campaign-autopilot hard stop)
**Campaign:** (confirm numbering at merge)
**Provenance:** Nathan's question — "we have (or are adding) concepts of wind,
ice, hot and cold, compass directions; are these also captured as concepts in
the linguistic system so that they can be expressed and understood and
conversed about by creatures, considered as factors in pathfinding?" Today the
answer is *sometimes, and nothing checks*. A modeled quantity, a registered
concept, a spoken word, an observable phenomenon, and a thinkable fact are five
separate ledgers that have drifted out of correspondence, and no test spans
them. Temperature is the indictment: an entire climate system — isotherms
([The Isotherm](2026-07-16-the-isotherm-design.md)), seasonal swing, the
Orrery's living lens — sits *modeled* with no concept, no word, no phenomenon.
It leaked across every campaign because every existing guard checks one
ledger's internal coherence, never the correspondence *between* them. This
campaign makes "modeled-but-unnamed / -unspoken / -unperceived / -unthought" an
uncompilable state rather than a lag CI surfaces after the fact.

This is the abstraction-lift of **PROC-15** (the Book as a coverage metric,
shipped C1) from one edge — fact-predicate → Common lexeme — to the full loop,
built on the **PROC-14** typestate mechanism with the **PROC-13** anti-vacuity
exhibit, as a concrete instance of the **UNI-28** View discipline.

---

## 1. Goal and contract

A modeled quantity that crosses a domain's public boundary must, *at its
registration site*, account for its manifestation across every ledger — or file
an explicit, reason-bearing typed void. The incomplete state (a concept with no
decision about its word, its percept, its cognizability) becomes
**unrepresentable**, not merely reported.

Delivered:

1. **The `Manifest` type** (kernel) — a total record a domain must supply to
   register a phenomenon-bearing concept. Each ledger edge is a field of type
   `Correspondent | Void(reason)`; the record has no `Default`, so an omitted
   edge is a compile error, not a silent absence. This is a **construction**
   proof in the PROC-13 sense — there is no representable-but-excluded state.
2. **The registration choke-point** — the concept/phenomenon registration API
   accepts a `Manifest`, not a bare `ConceptDef`. You cannot register a modeled
   phenomenon without deciding each edge.
3. **The void grammar** — a closed `Void` enum (`Unnamed`, `Gap`,
   `Imperceptible`, `Uncognized(pending(wave-N))`), each variant reason-bearing.
   Closed so that adding a void category fires an exhaustive-match obligation
   everywhere voids are consumed (the biome-tripwire flavor, kept *inside* one
   kernel type rather than spread across domains).
4. **The manifestation view** — `hornvale concepts --manifest` and a
   drift-checked book page: the folded view over all registered manifests,
   every edge shown as covered / typed-void, the void-list rendered as the
   backlog. This is the UNI-28 View; it generalizes PROC-15's coverage report
   from the render edge to all five.
5. **The first migration** — the mechanism applied to reconnect **wind** (the
   named-but-disconnected gap; no new physics) and, coordinated with the
   temperature data campaign, to name **temperature** and **compass
   directions** (the modeled-but-unnamed gap).

**Contract:** determinism is untouched. The `Manifest` is a compile-time
obligation that emits no bytes into any world, almanac, or artifact; same seed +
same pins → byte-identical worlds before and after. The registration *order* and
stream consumption are unchanged (§6). No census regen, no AWS spend, no
destructive or externally-visible action.

## 2. The five ledgers, and why they drifted

A phenomenon manifests across five ledgers, each a real surface in the codebase
today:

| Ledger | Where it lives | The edge into it |
| --- | --- | --- |
| **Compute** | domain fields / vectors (`temperature_c`, wind vectors, elevation) | — (the source) |
| **Concept** | `kernel/src/registry.rs` — `ConceptDef` / predicate / phenomenon-kind | `concept_for` |
| **Lexeme** | `domains/language/src/packs.rs` — hand-authored words | `lexeme_for` |
| **Percept** | the Phenomena stream (salience-ranked observations) | `percept_for` |
| **Cognition** | `windows/vessel` — knowledge, drives, the empty `decide()` seam | `cognize_for` |

They drifted because each is guarded *internally* and none *across*: the concept
registry checks its own conflicts/idempotency (decision 0025), the lexicon
covers registered concepts for gaps but authors words by hand, the Phenomena
stream is deliberately decoupled from its producer, and cognition touches none
of them. Two failure modes result, both live:

- **Modeled-but-unnamed** — temperature (a field, no concept/word/percept);
  compass directions (vectors used in movement, no concept/word).
- **Named-but-disconnected** — `wind` is a *language-owned* lexeme glossing an
  AMBIENT phenomenon, severed from climate's actual wind vectors. The word
  exists; it refers to nothing the climate models. (Also partial: `ice`/`snow`
  are climate concepts whose lexicalization is biome-gated and whose percept is
  absent.)

## 3. The mechanism — construction, not recognition

The naive design is one central `Phenomenon` enum with an exhaustive match at
each ledger. **It is unconstitutional and rejected:** a god-enum every domain
must edit to add a variant violates "adding a domain must never require editing
an existing one," and is exactly the recoupling UNI-28's own guard forbids ("no
god-enum of view kinds — that recouples every domain to the hub"). The
correspondence obligation must be **per-domain**, discharged at each domain's
own registration site, the way the `Biome` tripwire lives *inside* climate.

So the choke-point is the registration API, and the type does the forcing:

```rust
/// The manifestation record a domain supplies to register a modeled
/// phenomenon. No `Default`: every edge is a decision or an explicit void.
pub struct Manifest {
    /// The registered concept — the always-present anchor.
    pub concept: ConceptDef,
    /// The word a people can say, or the reasoned gap.
    pub lexeme: Correspondent<LexemeSeed, Void>,
    /// The observable Phenomenon, or why it is imperceptible.
    pub percept: Correspondent<PhenomenonKind, Void>,
    /// What a mind can hold, or a pending-wave void.
    pub cognition: Correspondent<CognitiveHandle, Void>,
}

/// A ledger edge: a real correspondent, or a first-class reasoned absence.
pub enum Correspondent<T, V> { Present(T), Absent(V) }

/// Closed: a new void category fires an exhaustive-match obligation
/// everywhere voids are consumed (the tripwire, kept inside one kernel type).
pub enum Void {
    Unnamed(&'static str),                 // no concept-side name yet
    Gap(&'static str),                     // no lexeme; recountable reason
    Imperceptible(&'static str),           // modeled but not observable
    Uncognized { pending_wave: &'static str }, // the whole unwired column, today
}
```

Registration takes the `Manifest`. There is no path that registers a modeled
phenomenon while omitting an edge, because `Manifest` has no `Default` and each
field's type admits no "unset" value — the incomplete state does not exist to be
constructed. That is what makes this a **construction** proof rather than a
recognition proof (PROC-13): non-vacuous by definition.

**Anti-vacuity exhibit (PROC-13, required).** Each edge ships a `compile_fail`
doctest that *exhibits* the forbidden state — a `Manifest` literal missing the
`lexeme` field must fail to compile — so the guard is proven to bite and cannot
rot into a vacuous witness. Mutation-check: removing a field's obligation must
flip the exhibit from red to green.

## 4. Scope — the full loop (ratified by Nathan)

The invariant reaches all five ledgers, cognition included. Cognition is the
least-built layer: `windows/vessel`'s `decide()` seam is shaped and empty, and
`_perception` is dead-wired. Therefore **most cognition edges resolve to
`Void::Uncognized { pending_wave }` at first** — and that is the yield, not a
weakness. The compiler-generated list of `Uncognized` voids *is* the build-out
roadmap for the cognitive layer, drift-checked and greppable the way
`type-audit`'s `pending(wave-N)` rows already are. The negative space becomes
the plan.

This is the deliberate difference from stopping at the lexeme edge (the PROC-15
boundary) or the percept edge: the point is to make the *unbuilt* layers file
honest, typed IOUs rather than remain invisible.

## 5. Save format, determinism, epoch — **G3 flag**

The mechanism emits no bytes and changes no world, but two surfaces are
save-format-adjacent and lead the review:

1. **The registration surface becomes a wider boundary.** Concepts already
   carry the concept-registry save-format contract (one-concept-one-owner,
   decision 0025; the drift-checked `concept-registry-generated.md`). `Manifest`
   sits at that same boundary. It adds no serialized field — the manifestation
   record is compile-time metadata, not world state — so no epoch bump is
   needed. Confirm this at review: if any edge's correspondent (e.g. a
   `LexemeSeed`) ever becomes a *drawn* value rather than authored, it would
   join the stream-consumption contract and need an epoch. Today all
   correspondents are authored/derived, not drawn.
2. **The void grammar vs. the `type-audit` verdict vocabulary.** `Void`'s
   reasons (`Unnamed`/`Gap`/`Imperceptible`/`Uncognized(pending-wave)`)
   deliberately echo `type-audit`'s `bare-ok`/`waiver`/`pending(wave-N)`.
   Decision for review: reuse the exact tokens for one enforcement vocabulary,
   or keep them distinct. Recommendation — **distinct types, shared `wave-N`
   token**, so the two backlogs join in one report without coupling the tools.

No other determinism contract (seed labels, stream order, noise/hash constants,
physics formulas) is touched.

## 6. The first migration — waves

Executed as separate campaigns after the mechanism lands (the type-audit
pattern: install enforcement, remediate in waves).

- **Wave 0 — mechanism only.** `Manifest`, `Correspondent`, `Void`, the
  registration choke-point, the `compile_fail` exhibits, the manifestation
  view. Every *existing* registered concept is migrated by wrapping its current
  state in a `Manifest` whose unbuilt edges are honest voids — a mechanical,
  behavior-preserving pass. This wave changes no world bytes.
- **Wave 1 — reconnect wind (no new physics).** Route the language-owned `wind`
  lexeme through a `Manifest` that binds it to climate's existing wind vectors
  as its `percept`, closing the named-but-disconnected gap. Pure wiring.
- **Wave 2 — name temperature & compass directions.** The modeled-but-unnamed
  gap. **Coordination flag:** the temperature/wind-field *physics* is under
  active work in a parallel data campaign
  ([The Isotherm](2026-07-16-the-isotherm-design.md) and the diurnal-field
  work). This wave adds only the *manifestation* (concept + lexeme + percept
  for temperature and for N/S/E/W); it must land **after** that data campaign
  settles to avoid churning the same surfaces. It introduces no physics and no
  new field.

Cognition edges stay `Uncognized(pending-wave)` across all waves; discharging
them is the cognitive layer's own future campaigns, which this backlog scopes.

## 7. Evidence battery

- **Construction proof holds:** `compile_fail` doctest per edge (a `Manifest`
  missing that edge does not compile); mutation-check flips each red↔green.
- **No god-enum:** an architecture test asserts no central phenomenon enum
  exists and that adding a concept in a fixture domain requires editing no other
  domain (the constitutional invariant, made a test).
- **Byte-identity:** seed-42 world / three almanacs / elevation map / registry
  and manifest dumps / lab studies are byte-identical before and after Wave 0
  (the CI drift-check already enforces this; assert it explicitly in the
  campaign gate).
- **Void accounting foots:** the manifestation view's counts (covered +
  each-void-class per ledger) equal the registered-concept count — a trial
  balance in the PROC-11 sense; a phenomenon that appears in no ledger column is
  a failure.
- **Wind reconnection:** a test that the `wind` lexeme's `percept` resolves to
  the climate wind phenomenon, not an AMBIENT stand-in.

## 8. Surfaces and the book

- `hornvale concepts --manifest` — the manifestation view (reference page,
  drift-checked, like `concepts` / `streams` today).
- A chronicle entry and a freshness sweep at merge (DoD, decision 0013).
- The Confidence Gradient: this campaign resolves the standing "is the
  vocabulary audited across systems" question implicit in PROC-15's coverage
  framing — re-score the relevant open-question chapter (PROC-5).
- Registry: flip PROC-16 `spec'd → shipped` at merge; cross-link the decision if
  the void-grammar/verdict-vocabulary call (§5.2) is ratified as one.

## 9. Success criteria

1. A modeled phenomenon cannot be registered without a `Manifest` accounting for
   all five ledgers; the incomplete state does not compile.
2. Every edge has a live `compile_fail` anti-vacuity exhibit that mutation-checks
   red↔green.
3. No central phenomenon enum; no existing domain is edited to add a concept in
   a new domain (constitutional invariant, tested).
4. Byte-identity preserved across Wave 0 (determinism contract intact).
5. The manifestation view renders the full five-ledger coverage and the typed
   void-list as a drift-checked backlog; its counts foot.
6. Wind's percept binds to climate's wind vectors.

## 10. Explicitly deferred / non-goals

- **Filling cognition.** This campaign makes cognition edges file typed voids; it
  does not build GOAP, perception wiring, or knowledge representation. Those are
  the campaigns the void-list scopes.
- **Authoring words.** The lexeme edge forces a *decision* (word or `Gap`), never
  the word's *content*; Swadesh/pack authoring stays hand-work (LANG-4).
- **Temperature/wind physics.** Owned by the parallel data campaign; this
  campaign manifests those quantities, it does not model them (§6, Wave 2
  coordination flag).
- **Retrofitting non-phenomenon concepts.** Pure abstract concepts (e.g.
  `is-a`, kin terms) that model no physical quantity carry a `Manifest` whose
  `percept`/`cognition` are honest `Imperceptible`/`Uncognized` voids; the
  invariant does not claim every concept is perceptible.
