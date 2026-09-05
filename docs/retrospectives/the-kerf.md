# The Kerf — retrospective

Process, not product. The product is in [the chronicle](../../book/src/chronicle/the-kerf.md), the rulings are in [the campaign ledger](../superpowers/ledgers/2026-09-04-the-kerf.md), and the three BEFORE plus three AFTER unpaired observations are in the spec's §11.

## A subtraction still needed a full measurement

Deleting a tenant made the deterministic result unusually crisp: at band 10
the removed `KnownWater` row was exactly 4,665 entries / 247,245 estimated
bytes, and the surviving Trail and LatestVisit rows did not move in three
runs. But K2 and K3 still required three readings with endpoint loads. The
advance probe clustered at 60.98–62.63 ns/fact while the belief probe varied
with a busy Mac. A subtraction is not a license to infer timing behaviour.

## A constant is an instrument with an expiry date

The two Kerf hash values, their reach control, and their sharpness control
were essential while `KnownWater` still existed. After its deletion they would
have frozen unrelated future behaviour to one old walk. The close removed the
asserted values and retained two fresh runs, all non-vacuity floors, and the
independent fold-equals-scan witnesses. The historical values remain beside
the test because provenance is useful; they no longer execute because the
comparison they represented no longer exists.

## Do not generalise a private helper from one occurrence

`rooms_at_where` states one first-visit rule for `rooms_at` and `water_at`.
That is a local repair to duplicated logic, not evidence for a repository-wide
shared-predicate law. The campaign minted decision 0756 for the proven
resident-index criterion and explicitly left 0757 unminted. A future second
site can establish whether a common abstraction has a real boundary.

## Deferred review minors

All four ledgered minors found a committed home. Task 4's two were repaired in
the close commit: `session_length_scaling.rs` now describes the two surviving
tenants and four statements, and `liveness.rs` says that the visit list's first
element supplies the first-visit instant. Task 5's two were repaired in the
final close fix: `the_kerf.rs` now describes its live two-fresh-walk guarantee
and reports both past-instant counts when that two-run floor fails.

## A ratchet can land between your last absorption and your close

The Kerf absorbed main at `0b009be30`, wrote every close artifact against that
tree, and then found main 33 commits further on when it reached G6. The Coda
had landed `docs/audits/campaign-reconciliation.tsv` and a coverage test in
that window, so the merge product was red on an obligation that did not exist
when the campaign's own definition of done was assembled: four campaign
records with no reconciliation row. Nothing was wrong with the close; the
close was simply complete against a stale main. **The lesson is about the
order of the close walk, not about diligence.** Absorbing first and writing
the artifacts second — which is what the walk already says — is what makes a
newly minted ratchet visible while there is still a commit left to fix it in.
Doing it the other way round means discovering the obligation from a red gate.

The repair was one row. But writing it surfaced something the row could not
carry: the reconciliation schema's `ledgers` column cannot be populated by any
row, because the coverage assertion compares every cited path against a
population that excludes ledgers. Zero of 1,327 committed rows cite one, while
two other assertions in the same file exist specifically to keep the column
alive. That is recorded as `PROC-reconciliation-ledgers-column-is-uncitable`
rather than fixed here: the instrument is one merge old and belongs to the
campaign that built it.

## The two conflict classes are told apart by authorship, not by directory

The close absorbed main three times, and the last one conflicted in four
generated-looking files that needed two opposite resolutions. Three —
`plumb-roster.md`, `type-audit-report.md`, `decisions-in-force.md` — are
regenerated wholly by `regenerate-artifacts.sh`, so the correct move is to
take either side as a placeholder and run `make rebaseline`; the result is the
true union, and this close verified it in both directions rather than assuming
it. The fourth, `campaign-reconciliation.tsv`, sits in the same declared
directory and looks identical from the outside, but nothing regenerates it —
resolving it that way silently drops one side's rows.

Main had learned this the hard way in the same window: `campaign/the-warp`'s
stage gate went red on 2026-09-05 because the chamber's absorb classifier
inherited `docs/audits/`'s `artifacts` author for that file and "resolved by
regeneration" a file with no regenerator. The fix was a `none(...)` override in
`docs/generated-paths.txt`. **The transferable part is the question to ask:**
not "is this path declared generated?" but "what *authors* this path?" — a
directory-level declaration answers the first and can be wrong about the
second, which is exactly how a file gets resolved by a mechanism that does not
exist.
