# 0581. The witness is bound to its situation

**Status:** Accepted (2026-09-01) · **Decider:** Nathan · **Campaign:** The Avowal · **Supersedes:** 0577

## Context

Decision 0577 gated `Stageable` on a registered witness — a `Tableau` that
`Session::start` could actually stage — but looked that witness up by
situation id alone. Nothing about `witness_stages` ever read
`Situation::requires`, so a `Tableau` relating two goblins by `instance-of`
witnessed Supplication, Murder, or Daring Enterprise identically: whichever
id the table happened to file it under. The review that caught this named
the governing sentence spec §4.2 already states — "a committed tableau
places **its** actants and every token its requirements name resolves
through the provision table **against that staged scene**" — and found
neither half implemented: the cast bore no relation to `Situation::actants`,
and the token check ran against the registry, never against what the
tableau actually staged.

**The ruling was to strengthen the gate, not weaken the prose.** The spec is
the binding authority; loosening 0577's own claim to match the gap would
have ratcheted the campaign down to fit an implementation shortfall instead
of closing it. Both frozen corpora still score 0 Stageable (0/36
`polti-1895`, 0/409 `tvtropes-2012`), so — as with 0577 itself — this
remains the cheapest moment to raise the bar: there is no existing claim to
retrofit.

## The decision

`witness_stages` now calls a second check, `witness_binds`, before it ever
attempts to stage: **every predicate a witness's tableau states as a
relation must be among the tokens the situation's own (expanded) `requires`
list names.** A tableau relating two cast members by `instance-of` binds
only to a situation whose requirements actually ask for
`predicate:instance-of`; filed under a situation that does not, it is
refused as `"witness:unbound"` — a third sentinel alongside 0577's
`"witness:absent"` and `"witness:refused"` — even though the identical
tableau would stage without error. This is the mechanical property the
review asked for: a tableau filed under situation A is refused under
situation B whenever A and B ask for different relation predicates, so one
authored scene can no longer silently stand in for every situation it
happens to sit beside in the roster.

**What this still does not prove, stated so a reader cannot infer more than
it checks.** It does not verify that the tableau's cast fills the
situation's actant ROLES: `Situation::actants` is prose-valued (a Greimas
role name mapped to a free-text description), so there is no mechanical
role check available the way there is for a predicate token — role
assignment stays entirely unverified, exactly the limit spec §4.2 itself
states for the whole witness bar ("does not prove any world produces the
situation"). And a witness that stages NO relations at all binds vacuously
to any situation whose requirements name no predicate token at all
(`Iterator::all` over an empty iterator is `true`) — a real gap, of the same
class decision 0330 already accepted for its own narrower claim, and one
this mechanism cannot close without a role-typed corpus this project does
not have.

**The distance record decision 0330 makes mandatory, closed in the same
pass.** 0577 stored only the input (`Witnesses = BTreeMap<String,
Tableau>`), never the record `sentence_corpus.rs`'s `MERCHANT_WITNESS`
carries beside every covered entry — the realized surface (or, here, scene)
and prose stating how it differs from the corpus's own claim. `Witnesses` is
now `BTreeMap<String, WitnessEntry>`, where `WitnessEntry { tableau,
realized: String }` and `WitnessEntry::new` refuses an empty `realized`
record. A trope situation stages a SCENE rather than an utterance, so there
is no single realized surface to diff against a literal sentence the way the
merchant corpus does; the record here is free prose instead, but the field
is mandatory, so the first author of a real row cannot construct one without
writing the distance down. `render`'s Demand table prints it beside a
`Stageable` row — a rendering path wired now, even though no situation
reaches it yet.

**A false claim about `Provision`, corrected in the same pass (a defect of
the same class this decision exists to close).** 0577's own text described
`Witnesses` as threaded through `resolve` "the same way decision 0576's
`Provision` table is — supplied by the caller, never rebuilt inside." That
is backwards: `resolve` builds `Provision::from_registry(registry)` itself,
every call, from the `registry` parameter it receives. `Witnesses` is the
one that arrives pre-built by the caller; `Provision` does not. A decision
record asserting more than the code does is exactly spec's "the failure this
project documents most," and it does not become a lesser instance of that
failure for being about its own plumbing rather than about the world.

**The rendering of a witness refusal, corrected alongside.** A `Blocked`
reason of `"witness:absent"` or `"witness:unbound"` or `"witness:refused"`
is not a missing corpus token, so the Demand table no longer prints it as
one (`"blocked — missing `witness:absent`"`, which reads as a real token,
would have been actively misleading). And a witness-blocked situation is
excluded from the Leverage section's bundle-distance arithmetic
(`closest`, and the denominator behind "missing bundles ranked ... over the
N blocked situations"): having already passed the token check, it has ZERO
unheld bundles by construction, and counting it there would print "the
closest blocked situation is still missing 0 bundles" the moment one exists
— contradicting the very sentence it sits in.

## Consequences

- `resolve` and `render` both gained no NEW parameters beyond 0577's own
  addition of `world`/`witnesses` — `render` additionally now takes
  `witnesses: &Witnesses` so it can print a `Stageable` row's distance
  record.
- `tropes::witnesses()` remains empty today, for the same reason 0577 gave:
  authoring a row for a situation whose tokens do not yet resolve would cost
  a file and move no verdict. A future row must both BIND (name only
  predicates the situation's own requirements ask for) and carry a written
  `realized` record — `WitnessEntry::new` enforces the second; nothing but
  review enforces the first, since `witness_binds` is a runtime check, not a
  type-level one.
- Wiring this amendment moved **zero verdicts** on either frozen corpus —
  confirmed by regenerating both reports and the matrix and diffing against
  the previously committed artifacts: only the shared header prose
  (`WITNESS_BOUNDARY_WHAT`/`WITNESS_BOUNDARY_COMPARABILITY`, used verbatim by
  both `render` and `render_matrix` so the two committed artifacts cannot
  state the claim two different ways) changed, never a per-situation row.
- The known gaps (role assignment unverified; a no-relation witness binds
  vacuously) are stated in the shared header prose itself, not only in code
  comments and this record — a reader of the committed artifact sees the
  same limits a reader of the source does.

## See also

Spec §4.2 (`docs/superpowers/specs/2026-09-01-the-avowal-design.md`);
decision 0577 (superseded by this record); decision 0330 (the sibling
precedent on `sentences/`, both for the realization-witness discipline and
for the mandatory distance record); decision 0576 (the provision table this
gate composes with, and the source of the corrected `Provision` claim);
`docs/superpowers/ledgers/2026-09-01-the-avowal.md` entries #8 and #9;
`cli/src/tropes.rs`; `cli/tests/suite/trope_witness.rs`.
