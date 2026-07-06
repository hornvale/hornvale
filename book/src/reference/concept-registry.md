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

Known wart, accepted: `derived-from-phenomenon` relates a belief to a
phenomenon *kind* (text), not an entity, because phenomena are observations,
not entities. Revisit if phenomenon instances ever become first-class.

The tables below are **generated** — dumped from the registry of a freshly
built world by `hornvale concepts` and verified fresh by CI. They cannot
drift from the code.

{{#include concept-registry-generated.md}}
