# The Ken — retrospective

*Process lessons. The chronicle carries the product story; the design and
decision records carry the technical contract.*

## Reproducing before spec'ing split one report into two defects

Report #12 read as "a wrong species string": *"I see a white dragon… it
says 'a black-dragon.'"* Read literally, that is the ambiguous-needle
defect (`examine dragon` silently answering with whichever match sorted
longest) — the one Nathan actually hit. Reproducing it with a staged
tableau before writing the spec surfaced a second, independent defect
nobody had reported: `presence_line` built its display noun from `species`
while `examine` matched on `label`, so the game could show a noun and then
deny it existed. Both are real and both are fixed, but only one was in the
bug report; the other would have shipped unfound as an "obvious" one-line
species-string spec had the campaign gone straight from report to fix.

## A test can go vacuous rather than red, and it reads exactly like success

Task 2 made the exits clause optional. Four existing tests consumed the
line it used to emit, two behind `.expect(...)`. `the_sky_follows_the_walker`
held `if let Some(d) = dir { … }` — once the clause vanished, `dir` was
`None`, the walker never moved, and the test still *passed*, because
weather varies with elapsed time alone and the assertion never noticed the
walk had stopped happening. The tempting repair — soften the other two
`.expect()`s to `if let` as well — would have turned all four green while
asserting nothing. The actual fix re-pointed each test at a room that still
emits the clause (a cube-corner facet, the water column), which is more
work than softening a guard and is the only version that keeps the
invariant live. A green suite is not evidence the assertion still fires;
only re-reading what a change did to each consumer is.

## `| head` on an enumeration is a silent LIMIT on a completeness claim

The spec, the plan, and the dispatch brief all named `derive_wild_npcs` as
*the* wild-label site. It is called only from `windows/lab` and tests — no
possession ever reaches it. Real gameplay derives wild creatures through
the sibling `derive_wild_herds`, which independently hardcodes the same
article defect and which the session actually calls. The controller's
pre-dispatch enumeration had run `git grep -rn 'a wild ' -- windows/vessel/src/*.rs
| head`, and the second site sorted below the cut. The plan's own verification step (`grep
'The a wild' <test output> book/src/gallery/`) passed the whole time,
because no committed artifact exercises the herd path — a green check over
the wrong population reads exactly like coverage. It surfaced only because
the Task 4 implementer went looking for a live reproduction of the defect
it had just "fixed" and could not find one on the possess path. An
enumeration that truncates its own output is not a display choice; it is an
unstated `LIMIT` on whatever completeness claim rests on it, and the fix
was widening the grep and re-deriving the claim from the full result, not
from memory of what the earlier grep had shown.

## A near-miss: the fix that satisfies the letter and not the point

Task 3's first attempt to remove the header's facet id and decimal day
shipped `[room]` / `[chamber]` — literally true to "no id, no day," and a
constant string that told a player nothing about where they stood. It was
caught by running `grep '^\[room' book/src/gallery/possession-over-time-seed-42.md
| sort -u` over a real multi-room transcript and getting one line back for
six different positions. The fix round found a token that already varies
per position for a reason the crate had already named (a facet's
`descriptor_noun`, authored "so homogeneous biome still varies room to
room") rather than inventing a new one. Removing information is not the
same task as replacing it with orientation, and the two are easy to
conflate when the letter of the requirement ("no raw facet id, no decimal
day") is satisfiable by deleting everything.

## A controller-authored spec was overturned by the work meant to implement it

Spec §4.2 asked the turn header's time phrase to "derive from the same
source as the sky line." Task 3's implementer found there is no such shared
source to derive from — the phase logic is baked directly into a
description string inside `sky_at`, with no `daypart` accessor to call.
Minting one just to satisfy the letter of the spec would have added public
API to remove a duplication that dissolves more cleanly by deletion: the
sky line one row below the header already states the time of day in the
character's own words, so the header can simply stop repeating it. The
controller ledgered the overturn rather than silently editing the spec,
which is the standing autopilot rule for exactly this case — an ideonomy
pass reversing the controller's own prior text.

## The stated scope was necessary but not sufficient, and only measurement found the gap

Task 5's brief scoped the ambiguous-noun refusal to `body_by_needle` and
its three named callers. `examine`'s outdoor arm turned out not to go
through `body_by_needle` at all — it resolves through the legend's
word-tokenised matcher instead — so the stated scope, followed exactly,
would have left the literal command from report #12 (`examine <noun>`
outdoors) unrefused. The implementer found this empirically by trying the
staged reproduction against the finished fix, not by reading the plan more
carefully; a scoped review of the named call sites alone cannot see a path
that was never named.

## A self-inflicted defect, caught and fixed inside the same task

Bare wild labels (Task 4) made a presence line read `black-dragon; 2 wild
otyugh` — bare singulars beside "N wild X" groups, an inconsistency the
campaign introduced by fixing only half of what "wild" touched. It was
recorded in the ledger and fixed one line later in the same function
(Task 5) rather than left as a new defect for a future campaign to
discover independently. A related proposal — teaching the resolver to
strip a leading "a "/"a wild " so the presence line could keep the word —
was argued, declined, and recorded rather than silently dropped: it
re-opens the displayed-string-versus-stored-label split this campaign
exists to close, in exchange for a readability gain that measured smaller
than predicted once a real multi-creature line was actually read.

## `clients/vessel/src/transcript.ts` consumed two of the strings this campaign deleted, and no workspace gate covers it

The exits-clause string and the `[room ` header prefix were both load-
bearing for the TypeScript transcript classifier that tells meta lines from
prose in the browser client. `make gate-commit` does not scan `clients/` at
all; only `make vessel-check` exercises that file, and it is listed
separately in the plan for exactly that reason. Both call sites were caught
because the plan named them up front, not because a gate would have caught
a miss — a prose change with no client-side counterpart would have been
silently wrong there until someone happened to run the browser client by
hand.

## Absorbing main brought in a structure-model rewrite that collided at exactly one function

Between this campaign's last absorption and its close, main gained The
Cruck (`Structure` now carries `roles`/`children` directly; `role_for` was
deleted) and The Lot, 54 commits total. The only real code conflict was in
`describe_chamber_here`, where this campaign's own header change and The
Cruck's fork-aware ways computation had edited adjacent lines of the same
function; git's three-way merge combined the surrounding doc comment
correctly on its own; the two computed blocks (the interior/id/ways setup)
had to be resolved by hand, and one line *outside* the marked conflict
(`role_for(at, brief)`, called only by this campaign's own commit) needed a
follow-up fix because the function it called had been deleted on main and
the merge algorithm had no way to flag a non-conflicting call to a
now-missing function. `cargo build --workspace --all-targets` caught it
immediately; a text-only read of the resolved conflict would not have.
Every generated artifact the merge conflicted on (two galleries, three
byte-golden fixtures) was resolved with an arbitrary placeholder and then
overwritten by `make rebaseline` / `make rebaseline-goldens`, never
text-merged — one of the three placeholders left a stale fixture that
failed exactly one test on the first full run, which is the expected
"regeneration, not resolution" shape working as designed.

## The reconciliation TSV's `ledgers` column stays empty, and this is not an oversight

`campaign_reconciliation_covers_every_campaign_record`'s audit population
excludes `docs/superpowers/ledgers/`, so every one of the roughly 1,300
other rows in the TSV leaves that column empty. This campaign's row does
too, for the same reason, not because its ledger has nothing worth citing.

## The Confidence Gradient

The render–command parity row moves. Task 4's finding — `presence_line`
rendering a wild group from `species` while `examine` matches `label` — is
the same "the render depicts a noun the command language denies" defect
The Handle and The Cruck had already found on five surfaces, on a sixth the
mechanized check does not reach because nothing had pointed it at that
roster. It surfaced from a player's own report, not from the ledger's
instrument. Task 5's ambiguous-needle refusal is a second, previously
undistinguished failure shape on the same row: a noun that is printed and
*accepted* can still be answered wrong, silently, which the row had not
separated from a plain denial before. Both are recorded in
`book/src/open-questions.md` as a re-score, per decision 0030 — this campaign
did not initially expect to move a Gradient bet, and would have missed that
it had without checking.
