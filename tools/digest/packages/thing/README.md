# Thing Digest contributor

`digest-thing` contributes checked context for `domains/thing`. It reads the
domain's public Rust APIs rather than carrying another kind list:

- `hornvale_thing::THING_KINDS` is the authored, ordered source roster.
- `hornvale_thing::thing_registry().ids()` is the component-store roster.
- `hornvale_thing::BORROWED` declares names whose concept definition belongs
  to another domain.
- `hornvale_settlement::register_concepts` followed by
  `hornvale_thing::register_concepts` builds the registry composition that the
  ownership observation checks through `ConceptRegistry::concept` and
  `ConceptRegistry::concepts`.

Thing owns each rostered concept except `hearth`, which `BORROWED` cedes to
Settlement. A missing or differently owned lender makes Thing registration
panic; the contributor catches that refusal and emits contradicted evidence
instead of claiming agreement. Reverse inclusion checks only concepts owned by
Thing, because the composed registry also contains unrelated Settlement
concepts such as `home`.

The contributor's instruction points maintainers to `domains/CLAUDE.md`, the
tracked domain guide, and to the public APIs above. Its finite checks establish
roster and concept-owner agreement only. They do not construct a world, inspect
world-generation wiring, validate save compatibility, or establish portable,
openable, lockable, placement, or other item behavior.

The Digest host invokes the binary as:

```text
digest-thing collect --repo-root <absolute-current-checkout>
```

The binary accepts only that command shape, verifies that the root and declared
`domains/thing` scope exist, validates the protocol envelope, and emits one JSON
value on stdout. It does not shell out or run world generation.
