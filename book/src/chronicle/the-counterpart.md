# The Counterpart

**Draft through Task 7 evidence capture. The full12 primary and independent replays agree; the campaign has not passed G6.**

Two people can each make a sensible change and still break something when their work meets. The Counterpart tests that problem on a small, real boundary: Thing and Settlement register concept names in the same registry, and Thing borrows some names from Settlement.

The experiment asks four questions of the original source, each change alone and their combination: did registration complete, do the two Thing rosters agree, do borrowed names have the right lender, and does each name have the right owner? Separate authors wrote the changes and their descriptions. Another author derived the checker from those questions. Every question ran on every valid source, even when a proposed rule selected fewer.

## What happened when the changes met

In the safe pair, Thing adds `counterpart-token` and Settlement adds `settlement-common-room`. Each addition works alone and both work together.

In the collision pair, each owner adds `counterpart-marker`. Each change works alone. Together they trigger the registry’s refusal because Thing has not declared that name borrowed from Settlement. Ownership after the incomplete registration is reported as unknown. This is a real interaction between the two changes, observed through the existing APIs. Production registration and the earlier Charter contributor already detect the collision; the new experiment independently confirms why the combined case differs from its parts.

Renaming Settlement’s `hearth` fails differently: Thing still needs that borrowed name, so the change fails even alone. That is an ordinary dependency failure, not evidence of a joint-only interaction. A legitimate extra borrowing of `home` passes, showing that the checker does not reject every unusual arrangement.

## Did a richer agreement help?

We compared three ways to propose which questions a change might affect: declared file scopes, Cargo dependencies and the owners’ descriptions of supplied and required names.

Across eleven distinct sources, Cargo and the owner agreement proposed exactly the same questions. The agreement required additional human interpretation because its authors used different declaration conventions. It earned **no demonstrated selection advantage over Cargo on this panel**.

Declared paths needed full fallback on seven requests. That fallback caught the violations its raw proposals missed. An unknown scope therefore remained a reason to check everything. No proposed selection actually skipped a check, and satisfied checks were not assumed unnecessary.

The reserved challenge exposed the preregistered correlated miss: the frozen
checker reports an ownership violation for `key`, while the candidate reports
its own three observations as satisfied because it trusts the observed
registry owner. Path and Cargo catch it; agreement falls back from an unknown
reserved mapping and then catches it. Across the full twelve-arm panel, the
effective agreement and Cargo selections remain equal in scope and miss zero
violations, so the extra agreement earns no demonstrated selection benefit.

## What this buys

The first gain is an inspectable example of where separate work succeeds and where shared meaning requires coordination. The second is a reason to keep the machinery small: richer descriptions have to earn their cost against simpler dependency information.

The original Linux run retained all eleven sources and 44 answers, along with
exact command records and failures. The primary full12 archive and a separate
independent archive each retain all twelve arms. The frozen summarize check
accepted both; source commit/tree identities and all four outcomes compare
equal for every arm. Their small wall/observation timing differences are
reported separately from this behavioral agreement, and the independent run's
shared-cache and non-blinded disclosures remain explicit. The normal and
diagnostic Stage2 gates passed; **[PENDING: final gate, census and G6 disposition.]**

The [experiment record](https://github.com/hornvale/hornvale/blob/main/tools/digest/experiments/the-counterpart/RESULTS.md) keeps the sources, results, costs and limits. The deliberately altered sources are retained as experiment data; the simulation has not acquired these test concepts. This continues [The Charter](./the-charter.md) and [The Digest](./the-digest.md) with a bounded test of composition. General semantic independence, reusable verdicts and permission to narrow gates remain open.
