# The Concept Registry

The registry is the negotiated vocabulary boundary between domains: the set
of **predicates** facts may use and **phenomenon kinds** observations may
carry. Registration is idempotent for identical definitions and an error for
conflicting ones, so two domains can safely require the same concept but
never quietly redefine it.

Reviewing this registry — *do these names still carve the world at its
joints?* — is a standing end-of-campaign ritual. An ontology grown one
reviewed concept at a time is the guard against the ontology trap (the
universal schema designed up front, which historically kills projects of
this kind).

Growing it is cheap, but not free of ceremony. A concept's name feeds the
language domain's proto-root assignment, which is a single ordered walk that
re-draws whenever a form is already taken — so a concept added in the middle
of that order used to displace the words assigned after it. Since
[The Accession](../chronicle/the-accession.md) every concept carries the
*generation* it joined in, and generation sorts first, so a newcomer lands
last and disturbs nothing. The obligation this creates is small and
absolute: **a newly registered concept must be added to the accession
register** (`hornvale_language::EPOCH_COHORTS`) as a new cohort, never by
editing an existing one. A concept that is registered but not accessioned
reads as generation zero and silently reopens the churn;
`cli/tests/accession.rs` holds the two rosters against each other in both
directions so that cannot happen quietly.

## Naming conventions (ratified at the Campaign 1b review)

Predicate names follow a style guide, not an ontology: the registry's
schema (the *functional* bit especially) is the source of truth for
cardinality and meaning, and names exist to read correctly. Four rules:

1. **Bare noun** (`name`, `biome`, `population`, `tenet`) — a functional
   attribute of the subject; the object is a value. Read: "the subject's
   ⟨noun⟩ is X." Each belief has exactly one `tenet`; many beliefs may share
   the same tenet text.
2. **`is-⟨kind⟩`** (`is-place`, `is-settlement`, `is-belief`) — a type flag:
   object is a true-flag, functional, marking membership in a kind.
3. **`has-⟨thing⟩`** (`has-caste`) — non-functional possession of repeatable
   things. The `has-` prefix is the "expect several" signal; its absence on
   a bare noun is the "expect one" signal.
4. **Verb/preposition phrases** (`located-in`, `held-by`,
   `derived-from-phenomenon`) — relationships, read strictly left-to-right
   from the subject: *village located-in vale*; *belief held-by village*.
   Passive forms (`-by`) mark inverse-direction relationships.
5. **Bare adjective phrases** (`tidally-locked`) — *property flags*,
   ratified at the Campaign 2 review: boolean states (not kinds), functional,
   object always a true-flag, **asserted only when true** — absence means
   false. Distinct from rule 2's `is-<kind>` membership flags.

Two riders from the Campaign 2 review:

- **Units in numeric predicate names:** the suffix `-std` means "in standard
  days" (`day-length-std`, `year-length-std`, `moon-period-std`); other units
  are named in full (`obliquity-degrees`). Names should read correctly on
  their own, but the schema is the truth — see below.
- **Rule 3 softened:** `has-<thing>` is *preferred* for repeatables, but bare
  noun phrases are acceptable where `has-` reads awkwardly
  (`notable-neighbor`, `genesis-note`, `scenario-pin`). No naming scheme
  eliminates all ambiguity; the registry's *functional* column — not the
  name — is the cardinality truth. Consequently the schema itself is slated
  to grow two columns in Campaign 3: a **value kind** (Entity/Text/Number/
  Flag, *enforced* at commit time — mistyped facts become uncommittable) and
  an informational **unit** for numeric predicates. Subject-kind constraints
  (domain/range modeling) are explicitly refused — that road leads to the
  ontology trap.

Known wart, accepted: `derived-from-phenomenon` relates a belief to a
phenomenon *kind* (text), not an entity, because phenomena are observations,
not entities. Revisit if phenomenon instances ever become first-class.
Noted for a future review: `night-star` is conceptually a sub-kind of
`celestial-body`; a kind taxonomy question will knock around Campaign 5.

The tables below are **generated** — dumped from the registry of a freshly
built world by `hornvale concepts` and verified fresh by CI. They cannot
drift from the code.

{{#include concept-registry-generated.md}}
