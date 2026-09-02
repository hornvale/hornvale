# 0576. The capability provision table

**Status:** Accepted (2026-09-01) · **Decider:** Nathan · **Campaign:** The Avowal

## Context

`cli/src/tropes.rs::resolve` decided whether a dramatic situation was
stageable by testing concept-registry membership alone
(`registry_tokens`). A capability the world genuinely has can live in one
of three homes — the ledger (`EntityId`-keyed, saved), the component layer
(`KindId`-keyed, build-state, never saved), or session state (`EntityId`-
keyed, persisted only when asked) — and the registry can see only the
first. `windows/sentiment::snap_judgment` computes exactly the demands
`bundle:felt-affect` asks for and scores 0/3, because it is kind-keyed
component data and the audit reads only committed facts. Left as-is, the
instrument rewards committing what decisions 0346 and 0366 say should be
derived (spec §2.1).

## The decision

A declared table — `Provision`, never `Manifest`: `kernel/src/manifest.rs`
already defines `Manifest` for a different correspondence (a concept
carried across lexicon/perception/cognition), and reusing the word would
alias two unrelated tables — mapping each corpus token to the home that
serves it, spanning all three. `tropes::resolve` consults it instead of
`registry_tokens()` alone.

**Direction, stated per the standing rule:** the table asserts *declared ⊆
served*, never the reverse. A token with no row is undeclared, hence
missing — the same default-deny `resolve` always had. A token WITH a row is
not automatically served either: its home's resolver is asked, and a "no"
still leaves the token missing (`Provision::serves`). The table can only
narrow what counts as present; it can never widen it beyond what a home
actually serves.

**Reuse, not re-derivation.** `kernel::Correspondent<T, V>` —
`Present(payload) | Absent(reason)`, where an absence must name why — is
reused generically (`Correspondent<Home, Unserved>`); `Void` itself is not
reused, because its four variants (`Unnamed`, `Gap`, `Imperceptible`,
`Uncognized`) all name lexicon/perception/cognition reasons and none
describes "no storage home serves this token." `cli::provision::Unserved`
is a fresh, single-variant reason type (`NotServed(&'static str)`) built for
this table's own vocabulary instead.

**This decision covers the table and the ledger home only.** The component
and session homes exist in the type (`Home::Component`, `Home::Session`)
but are unreachable by construction — both carry `Unwired`, an empty enum,
so no value of either variant can exist until a later decision in this
block (spec §4.4, §4.5) replaces `Unwired` with a real payload at the call
site.

## Consequences

- `resolve`'s observable behaviour is unchanged today: `Provision::
  from_registry` declares exactly the tokens `registry_tokens` used to
  compute, checked against the same three namespaces
  (`predicate:`/`phenomenon:`/`concept:`). `docs/audits/trope-*.md` did not
  move when this landed — confirmed by running `make rebaseline` and
  diffing, and pinned going forward by
  `cli/tests/suite/provision.rs::ledger_home_still_matches_committed_reports_for_both_corpora`.
- A future home (component, session) is added by giving `Home::Component`/
  `Home::Session` a real payload and a resolver in `Provision::serves` — not
  by touching `tropes::resolve` again.
- A row claiming a home that does not actually serve a token is refused,
  not waved through — `Provision` is not a second registry a token can opt
  into by being named in it.

## See also

Spec §2.1, §4.1 (`docs/superpowers/specs/2026-09-01-the-avowal-design.md`);
`docs/superpowers/ledgers/2026-09-01-the-avowal.md` entry #7 (the full
reuse-or-re-derive reasoning); `cli/src/provision.rs`.
