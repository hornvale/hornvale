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

The tables below are **generated** — dumped from the registry of a freshly
built world by `hornvale concepts` and verified fresh by CI. They cannot
drift from the code.

{{#include concept-registry-generated.md}}
