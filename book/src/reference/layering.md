# The Layering

The constitutional dependency rule — `kernel` → `domains/*` → `windows/*` →
`cli` — is enforced by `cli/tests/architecture.rs` (decisions 0002 and
0004): a domain crate depends on the kernel and nothing else, never another
domain; windows may depend on domains and other windows because they present
them; the CLI sits on top; and only `serde`/`serde_json` cross the workspace
boundary from outside. The picture below is emitted by that same enforcer
and drift-checked against it on every test run, so what you read here is
exactly what the tests assert — the diagram cannot rot out of sync with the
graph it depicts.

{{#include layering-generated.md}}
