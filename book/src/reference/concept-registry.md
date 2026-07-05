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

> **Honesty note:** this page is currently maintained by hand. Making it
> *generated* — dumped from the actual registry of a freshly created world in
> CI, so it cannot drift — is a Campaign 1b deliverable.

## Predicates (as of Campaign 1a)

| Predicate | Functional | Meaning |
|-----------|-----------|---------|
| `name` | yes | Canonical name of an entity. Registered by the kernel itself; the only concept that exists before any domain does. |

*Functional* means a subject may hold at most one distinct value: a thing has
one canonical name, and the ledger rejects contradiction.

## Phenomenon kinds (as of Campaign 1a)

None registered — phenomenon kinds belong to domains, and no domains exist
yet. The integration suite registers a `celestial-body` kind in its
miniature genesis, prefiguring what astronomy will register properly in
Campaign 1b.
