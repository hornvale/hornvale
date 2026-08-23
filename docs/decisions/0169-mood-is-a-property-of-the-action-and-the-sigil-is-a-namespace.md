# 0169. In-character is a property of the action, and `!` is a namespace

**Status:** Accepted (2026-08-22) · **Decider:** Nathan · **Reverses:** the
metaplan's §3.3, which treated the sigil as a *mood modifier* applied to a verb

In the context of giving one action suite two registers, we decided that
**a mood is carried by the action, not by the invocation — `examine` and
`!examine` are two different acts, and `!` selects a separate lookup table
rather than modifying a verb** — because a sigil that modifies a verb makes
every verb silently sigil-able, and the failure mode is an alias nobody
authored.

## Context

The metaplan's model was: parse the verb, notice the `!`, set a mood flag.
Under that model `!<anything>` is well-formed, so a verb with no meaningful
out-of-character reading still resolves — to its in-character behaviour, with
a flag set that nothing consults. The player is told a capability exists that
does not.

The model adopted instead strips the sigil *before* verb lookup and routes to
a **separate match**. An unclassified `!<verb>` therefore falls to that table's
own catch-all and is refused as an ordinary unknown verb. The separation is
load-bearing, not cosmetic: it is the only thing standing between "this form is
not implemented" and "this form silently aliases the other one."

`Action::mood()` is exhaustive by variant with **no wildcard arm**, so a new
action fails to compile until it is classified — the same discipline
`action_variants_must_all_be_rostered` applies to the roster.

## Consequences

- Group A (the operator instruments — `why`, `npcs`, `help`, `eyes`,
  `whoami`, `provoke`, `soothe`) is out-of-character **only**; its bare forms
  are retired. An operator instrument that does not look like one is how
  `Session::needs` came to be a side channel around a structural redaction.
- Group B verbs carry **both** forms, as two actions sharing one concept —
  the same way `MoveTo` and `MoveWithin` share `move`.
- **An out-of-character form must observably differ from its twin, or it must
  not ship.** Applied per verb, this rejected `!look` and `!knows` at Task 6:
  neither renderer had a gate to relax, so both would have been no-ops.
- **That rule is re-checkable, and it fired.** Task 7 gave bare `look`/`knows`
  a body-state gate, which falsified the reason for the omission, and both
  forms then shipped. The omission had been recorded *with its reason* rather
  than as a conclusion, which is what made the change detectable at all — a
  practice worth repeating wherever something is deliberately left out.
