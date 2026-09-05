# Independent checker derivation

This checker answers the four frozen `counterpart-v1` questions from the raw
`facts` dictionary. It neither imports the Charter derivation nor consumes a
candidate contribution, declared affected IDs, enrollment record, or specimen
verdict. Its finite judgments are separate from the runner's attempt validity.

## Authority and actual disclosure

The checker author was separately dispatched on `codex/counterpart-checker`.
The author read the Task 2 brief, shared interface document, and the frozen
contract supplied by the controller. The contract's SHA256 was verified as
`37ea8fa06f086000f94cf215a391b5bcbd74bef92b9502bdc02ebb0a9ad32b34`.
That input was not edited or committed by the checker author.

Production context read: `domains/CLAUDE.md`; the ownership and borrowing API
documentation in `domains/thing/src/lib.rs`; registration documentation matched
in `domains/settlement/src/lib.rs`; and registry types, storage, and registration
semantics in `kernel/src/registry.rs`. A symbol search across the Thing file also
returned matching production test comments and assertions, including its
existing ownership and mutation-test documentation. The author did not read the
complete campaign plan, Charter collector/verdict derivation, another author's
records or patches, or any candidate-produced answer. This is independent
implementation from a shared accepted contract, not a security-blind experiment.

The two-name `hearth`/`key` fixture comes literally from the shared interface.
Each changed fixture has a hand-written expected outcome; no observation from
an executable or implementation helper computes the expected answers.

## Translation into raw-fact predicates

- **Registration:** both captured outcomes must be `accepted`. Either refusal
  violates this question. A refusal is an observed subject outcome, not an
  exception in the checker or evidence that the attempt itself failed.
- **Components:** count source and component labels separately. Any duplicate
  violates the question; then require set equality, testing both missing and
  extra components. Equal counts alone cannot establish membership.
- **Borrowing:** group declarations by name without discarding multiplicity.
  Each name must occur once, belong to the source roster, and name an owner
  other than `thing`. After accepted Settlement registration, the before
  registry must have exactly one matching entry from that stated owner. A
  Thing refusal does not erase these prior observations. After Settlement
  refusal, structural declaration violations remain violations; otherwise the
  lender question is unknown because the before registry may be partial.
- **Ownership:** require both registrations to have completed before judging
  the final registry. For every source name, independently derive the expected
  owner from explicit borrowing, defaulting to `thing`; independently inspect
  its observed final owner and require exactly one matching entry. Conflicting
  declarations cannot select a winner. Reject Thing-owned names outside the
  expected Thing-owned source subset. Require every before-registry concept,
  including lender names not borrowed by Thing, to retain its single owner.
  Repeated final concept observations violate uniqueness rather than silently
  disappearing when a dictionary is built. After either refusal this question
  is unknown, even when a partial registry happens to look complete.

These questions do not inherit one another's verdicts. For example, a repeated
same-owner borrowing declaration violates borrowing while still specifying an
unambiguous expected owner for ownership. Absence of any borrowing declaration
has no lender obligations of its own; an undeclared collision is detected by
ownership if composition completes, or by registration if it refuses.

## Input boundary and limits

Validation precedes every judgment, including refusal branches. Facts must
contain exactly the seven specified fields, lists of nonempty strings or
`{name, owner}` objects as appropriate, and registration objects containing
`outcome` plus string `detail`. Raw duplicate array entries are retained as
semantic observations. Nonempty does not mean trimmed or normalized: identifiers
are compared exactly as observed.

The contract must have the specified object shapes, schema, contributor and
scope, nonempty string metadata, and exactly one occurrence of each of the four
known question IDs. Missing, additional, duplicate or unknown question IDs are
invalid; every valid evaluation returns all four results. Malformed input
raises `ValueError` rather than emitting a satisfied or unknown verdict.

`evaluate` receives already decoded dictionaries. Duplicate JSON keys cannot
be recovered at this boundary; rejecting them during raw JSON parsing belongs
to the runner. Likewise frozen byte identity, precise metadata content,
observation completeness, and whether an apparently empty array really came
from a completed observation are runner responsibilities. Empty valid arrays
are not fabricated here to recover observation faults. Nothing here expands
the frozen ownership contract to concept glosses, kind tags, simulation
behavior, or candidate correctness generally.

## Behavioral validation

The initial empty-result scaffold ran successfully as Python but produced
missing-result errors as well as assertions. That was not counted as behavioral
proof. Replacing it with a complete always-satisfied scaffold produced 74
assertion failures in 32 test methods and zero errors. The implemented checker
then passed all 32 methods, including malformed-input subcases.

An actual isolated mutation removed the final expected-versus-observed owner
comparison. Both the mutated checker and its fixture compiled as Python. The
literal one-name case (`key` observed under `settlement`, no borrowing) then
executed and failed with `AssertionError: 'satisfied' != 'violated'`. The
unmodified checker passed all 32 methods again. Detailed RED/GREEN/mutation
receipts are in the task report; no reserved challenge has been selected,
designed, or revealed by this work.
