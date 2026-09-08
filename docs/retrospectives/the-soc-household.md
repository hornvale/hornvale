# The Social Household — retrospective

*Process lessons. The chronicle carries the product story; the spec carries the
boundary; the campaign ledger carries the design rulings.*

**Stage gate:** pending refreshed close submission after the final merge from
`main`. Earlier stage gates for the implementation were green; no census was
run because the change is opt-in and does not alter generated world behavior.

## The useful boundary was a projection boundary

The campaign stayed small by making cohorts authoritative for distributions and
topologies, while treating persons, relations, lifecycle events, and groups as
seeded projections. That let the model answer The Lot's questions without
inventing a universal household class or pretending that an aggregate rate is a
biography.

The most important distinctions survived review: sex traits, reproductive role,
social gender, personal identity, institutional recognition, and transition
history are separate; relation direction and validity intervals are explicit;
and absent evidence is not a zero or a proxy inference.

## Review found boundary defects, not missing nouns

The fix rounds corrected strict time filtering, directed relation semantics,
complete provenance, explicit recognition interpretation, payload integration,
and stale generated artifacts. Each defect came from a boundary that looked
plausible when named informally but was weaker when exercised by a negative
case. The durable rule is to test the nearest absence as deliberately as the
happy path: no partner, no recognition, no active relation, no surviving
parent, and no matching lifetime.

## Follow-ups and scope dispositions

The close sweep routed all campaign follow-ups. The domain split remains the
minimal existing separation between aggregate demography and realized social
projection. Synthetic probe names remain test-only. Coordination with The
Murrain is a future interface concern, not a reason to couple the substrates.
Institutions, economy, magic, anatomy, and authored social canon for existing
species remain outside this campaign. Those dispositions are recorded in the
ledger and registry; no scratch-only follow-up remains.

## Do differently next time

Write the negative-case matrix beside the first boundary contract, not after
the first review. Keep generated payload and client drift checks in the task
that changes the producer. Finally, make the close ledger entry while each
review ruling is fresh; recovering it from ignored scratch notes is slower and
less reliable than contemporaneous recording.
