# The Room Mesh — retrospective

**Completed:** 2026-07-12 (name-only designation per decision 0026; plan
`docs/superpowers/plans/2026-07-12-the-room-mesh.md`)

**Spiking the keystone first bought a fearless build.** The whole campaign
rested on one risk: the seam-crossing neighbour walk. Get the 30-edge gluing
table or the barycentric reflection wrong and every deep room's neighbours are
silently misplaced — a determinism bug with no loud symptom. That risk was
retired *before* the spec froze, in two throwaway kernel spikes validated
against a fully-built level-7 `Geosphere` oracle (all 327,680 faces, `max|Δ| =
0`): the O(1) integer walk, and adaptive depth / T-junctions across seams. The
payoff showed at Task 5 — "THE CRUX" in the progress ledger — which passed
first try, because the algorithm was already known-good and the task was only
porting it under TDD. The durable pattern: when a campaign has a single
load-bearing algorithmic unknown, spike it to an oracle *before* writing the
spec, so the spec commits to a validated shape and the build is transcription,
not discovery.

**Four ideonomy passes mostly *confirmed* the design — and that was worth
doing anyway.** Loose-ends, room-reference, ecological psychology, and
world-change were each run over the design. They did not overturn the layering;
they sharpened a handful of contracts and, more valuably, *named the non-goals*.
The convergence of four independent framings on the same boundary (P2 is the
umwelt-neutral locomotion substrate; reference, perception, and change all ride
above it) is itself the evidence the boundary is real. The lesson is that
ideonomy earns its keep even when it confirms: a spec whose §9 records exactly
what it is *not*, four ways, hands the next campaigns their handles and keeps
naming/perception/change machinery out of the kernel by construction.

**Spec self-review caught a wrong denominator before a line of code.** The
inheritance-weight denominator is `D = 3 · 2^(depth − globe_level)`, not the
`2^d` a first reading of "dyadic barycentric coordinates" suggests. The corners
of a sub-triangle are dyadic; the *centroid* averages three of them, and that is
where the factor of three enters. This was found by the spec's own worked
arithmetic during self-review, recorded openly (§7 parenthetical), and pinned by
a test asserting the numerators sum to `D`. Had it slipped through, it would have
been a silent off-by-3× in every inherited field. Worked examples in the spec —
actually computing the small case by hand — pay for themselves.

**Per-task type-audit tagging replaced a Task-8 batch, mid-campaign.** The plan
originally batched all type-audit tags into the final task. Task 2's review made
the amendment: tags are added per-task from here, matching CLAUDE.md's standing
default-deny gate, and every implementer dispatch includes the type-audit check
in its gate. By close, Task 8's "add the tags" step had degenerated to a
verify-clean sweep (it passed with no changes). Tagging-as-you-go is strictly
better: the tag is written while the primitive's meaning is fresh, the gate
stays green every commit, and the final task carries no tagging debt. The plan
should never batch a per-boundary discipline into a terminal cleanup task.

## Estimate vs reality

The plan's eight tasks ran close to estimate — the spike-first shape meant no
task hit a surprise, and the crux task passed first try. Two small findings
surfaced in review and were fixed inside their tasks without a re-review cycle
(Task 2's Invalid-path test and type-audit classes; Task 7's blend test
strengthened to pin the cell↔weight axis pairing, since a constant field is
permutation-invariant and could not have caught a swapped axis). Both were
objectively verifiable and controller-verified via diff. The endgame (this DoD
task) was routine: the manifest wiring was a two-line array edit, and the
freshness sweep found one genuinely-lagging chapter (the architecture overview's
kernel-capability list) plus one bet to re-score (coarse-constrains-fine, moved
from *no mechanism* to *mechanism shipped, composition pending*).
