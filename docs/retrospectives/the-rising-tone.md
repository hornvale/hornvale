# The Rising Tone — retrospective

**Merged:** 2026-07-10

Process lessons from the phonology epoch (tone + tonogenesis), which began as a
homophony bug and became a suprasegmental tier. Product is the chronicle's job;
this page is about how the work went.

**The "confirm before code" gate was worth honoring, and one question inside it
was worth reopening.** The spec ended with four open design decisions to confirm
with Nathan before any Stage-3 code. Two came back confirmed as written; two
came back with a one-line instruction to run the ideonomy skill first. That
detour was not ceremony. It reframed the atonal-residual question (Q3) from "do
we accept a small tail?" into "homophony is a *parsing* cost, and a collision is
harmless exactly when the colliding meanings never compete in the same context"
— which turned into a shipped metric (`confusable-homophony-*`, the same-domain
subset of core homophony) and a ratified decision. The lesson is not "always
brainstorm"; it is that a spec's own gate can distinguish a rubber-stamp
confirmation from a question still load-bearing, and the load-bearing one repays
a structured ideation pass before code freezes the shape.

**Concentrating the reseed into a single commit kept the re-baseline single.**
Adding a drawn rule to the cascade reseeds every derivation in every world — the
expensive part of an epoch. Stage 3 was engineered to be byte-identical anyway:
the `Tone` type and the tonogenesis rule landed, but the rule was deliberately
*not* added to the drawn `RULE_KINDS`, and the tone-inventory draw was deferred
to Stage 4, so shipped worlds did not move and Stage 3 committed clean with no
re-baseline. The whole epoch drift then landed once, in Stage 4, alongside its
regeneration. Sequencing determinism-affecting changes so the ~10-minute
census + re-pin happens once rather than per stage is the difference between one
careful re-baseline and three.

**A field on a widely-constructed enum is a compiler-guided refactor, not a
sed.** Adding `tone` to `Segment::Vowel` touched roughly fifty construction and
pattern sites. Blind text substitution was unsafe because the same `rounded:
false,` line appears in both constructions (which needed `tone: Tone::Neutral`)
and `romanize`/`ipa` *patterns* (which needed `..`). The reliable path was to
let the compiler enumerate every site (`E0063` for a missing field, `E0027` for
a pattern that omits one) and to discriminate construction from pattern by the
line that follows — a comma-brace versus a fat arrow. For a determinism-critical
type change, the compiler is the exhaustive checklist; the cleverness goes into
telling the two cases apart, not into a one-liner that gets one of them wrong.

**Re-pin from the regenerated artifact, not from iterated test failures.** The
epoch moved a dozen exact calibration pins across two suites. Chasing them
through short-circuiting `assert!` messages would have meant a run per pin. Far
faster: regenerate the census CSVs once, then compute the new means and counts
directly from them with a throwaway script, and update every pin in a single
pass. A useful by-product was seeing which pins *did not* move — the
divergence-magnitude means and per-seed rates came back byte-identical, so they
were left untouched and the honest "unchanged" was recorded rather than
nervously re-pinned to a value that only looked new.

**A new capability does not need a shipped instance to be proven.** Spec §11
asks for a tone-capable species "in the roster" to exercise tone-count above
one and the capacity floor. Shipping a full tonal *people* — with culture,
settlements, religion, and a place on the globe — would have been a large,
opinionated addition that contradicts the campaign's own premise (shipped
peoples stay atonal; tone is for the future bestiary). The lighter, honest move
was a test-only roster control, exactly the pattern the null-control twin
already established: a `serpent` that lives only in a Lab test, proves the tonal
path, and never enters a generated world. This is a deliberate reading of §11 —
"the roster" as the Lab's control rosters, not the default census — and it is
flagged here as a decision a reviewer should ratify or overturn, not slipped in
silently.

**The parallel campaign re-merged cleanly because the overlap was diffed first.**
A separate Workflow Improvements campaign had advanced `main` while this epoch
ran. Rather than merge and discover conflicts, the merge began by listing which
files `main` had touched that this branch also touched — two, and neither the
heavily-rewritten `metrics.rs` nor the census fixtures. The one real conflict
(both branches extended the same re-export lines) was then trivial and expected.
Reinforcing the standing note on parallel-campaign collisions: diff the overlap
before the merge, and refreeze the shared census fixtures afterward to confirm
byte-stability — which they were.

**The freshness sweep's blast radius was, once again, the prose the drift check
cannot see.** The generated artifacts re-baselined themselves. The stale
sentence was in the Language chapter, which stated the articulation vector had
"six dimensions, no more" and warned that a seventh was "real design work
belonging to a campaign willing to weigh it" — a warning this campaign
discharged by adding the seventh. Nothing failed when that paragraph went stale;
it is exactly the class of hand-written prose the Confidence-Gradient decision
warns is uniquely easy to forget. The open-questions map itself needed no
re-score — no bet on it was moved by this epoch — and that honest no-change was
recorded rather than manufactured, which is the case that decision was written
to protect.
