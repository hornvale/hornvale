# 0019. No procedural macros

**Status:** Accepted (2026-07-07) · **Decider:** Nathan

In the context of recurring boilerplate (unit newtypes, predicate
registration, stream-label declaration) and the standing temptation of
derive macros, facing the fact that a proc-macro crate means `syn` +
`quote`, we decided that **the workspace uses no procedural macros;
`macro_rules!` is the permitted escape hatch for mechanical newtype
boilerplate when a campaign adds several at once**, accepting hand-written
repetition in the meantime.

**Context.** Two forces, either sufficient alone. First, `syn`/`quote`
import roughly a hundred thousand lines of parsing machinery to save
boilerplate — against the spirit of the serde-only allowlist even as an
internal crate. Second, the obvious codegen candidates are exactly the
wrong places for magic: stream labels, predicate registrations, and typed
quantities are **save-format contracts**, and their value is that a
reviewer sees every label and every constructor in plain text. Codegen
there is where silent drift would hide.

**Consequence.** Newtype quantities (`Au`, `Mm`, `StdDays`, …) keep their
hand-rolled constructors; if the boilerplate ever gets genuinely painful,
a `macro_rules!` macro (zero deps, expansion inspectable) is the ceiling.
Proposals for derive macros are settled by this record, not reargued per
campaign.

**See also.** Decision 0004 (no new dependencies); decision 0006 (labels
are permanent contracts); decision 0008 (typed quantities); CLAUDE.md's
save-format-contract list.
