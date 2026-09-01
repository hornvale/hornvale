# Campaign ledgers

One file per campaign: `docs/superpowers/ledgers/YYYY-MM-DD-<slug>.md`,
matching the naming convention of `specs/` and `plans/` (the third sibling
tree). Created by The Cartulary
(`docs/superpowers/specs/2026-08-30-the-cartulary-design.md`) after five
recorded losses of exactly this material to git-ignored, per-worktree
scratch that dies when the worktree is recycled.

## What belongs here

A ledger holds the things that are nobody else's derivative:

- **Rulings** — the campaign's own decisions, in `campaign-autopilot`'s
  entry format (`#N [G1|G2|G4|G5|Q] — question · decision · why (precedent
  cited) · alternatives discarded · ideonomy passes/overturns · capture
  actions`).
- **Deferred minors and parked findings** — things noticed but not acted on
  in the moment, with enough context to act on later.
- **Cross-task pre-flight rulings** — how a controller resolved a conflict
  between two tasks touching shared surface, and why.

The controller commits at each task boundary and each ruling — small,
docs-only commits that skip `gate-commit` because no Rust path is staged.
The ledger's git history becomes the campaign's decision history, readable
with `git log -p` over one file.

## What does NOT belong here, and why

**A review package is `git diff` output, regenerable by construction from
two SHAs.** Nothing is lost by not committing it: anyone can reproduce it
from the two commits it diffed. It stays in `.superpowers/sdd/<campaign>/`
— git-ignored, per-worktree scratch — and dies with the worktree, which
costs nothing because it can be regenerated.

**An implementer report is evidence *for* a ruling the ledger already
records, not the ruling itself.** The decision that matters — what was
decided and why — belongs in a ledger entry. The report that led to it is
working material, not the durable fact.

**Measured 2026-08-30, across three live campaign worktrees at Task 1:**

```
                regenerable (.diff)     durable prose
  the-chattel        3,412 K            208 K  (10 files)
  the-stile            964 K            204 K  (21 files)
  the-winze             16 K            196 K  (15 files)
```

**That n=3 read as "remarkably stable at ~200 K" and this campaign's own
later re-measurements falsified the word, not the argument.** Task 6's own
close re-measured at n=6 (range 80–360 K); this campaign's final review
measured n=13 and found a 95x spread — `the-wick` 4 K to `the-governor`
380 K, with every value in between. The mean sits close to 200 K, but
"stable" overclaimed a real, wide distribution as a narrow one. Same
correction for "up to 3.4 MB" of regenerable diffs: `the-burr` held 58.9 MB
at the same measurement. Durable prose (a ledger's kind of content) is
still on the order of hundreds of K, not megabytes, and review packages are
still the thing that varies by orders of magnitude — the argument this
tree's design rests on (don't widen it to hold reports or reviews) survives
and is if anything strengthened by the wider spread, because the outliers
are all on the regenerable side. A ledger itself measured a MEAN of
~15–19 K across those same n=6/n=13 samples, over a 12–32 K range — and
this tree's only inhabitant, this campaign's own ledger, is 41 K and still
growing, above even that range's ceiling. Read those figures as a floor,
not a size. The first draft of this sentence dropped the range and reported
the mean as the value, four lines under the paragraph correcting exactly
that move. Widening this tree to also hold reports or review packages would
drag that variance into git for every campaign, for no benefit: the thing
being committed is already reproducible from two SHAs that git already has.

**The plugin's own `progress.md` keeps its separate job and also does not
belong here.** `.superpowers/sdd/<campaign>/progress.md` is defined by the
vendored superpowers plugin (not this repository) and holds task state,
fix-round bookkeeping, and resume-after-compaction material. This
repository cannot change where the plugin writes, and a local edit to a
versioned plugin path would be overwritten by the next plugin update. That
file's purpose does not need to survive worktree recycling: a recycled
worktree means the campaign is over, and its task-completion lines are
recoverable from `git log` regardless. See spec §4a for the full reasoning
— the resolution is that the durable kinds (rulings, deferred minors,
parked findings) are written straight to a file in *this* tree, by the
controller, from the start; nothing is mirrored or copied out of the
plugin's file.

**If you are a later reader tempted to widen this tree:** don't. The
failure this design replaces was not "the ledger is too narrow" — it was
"the durable material lived somewhere that dies with the worktree." Adding
reports or review packages here reintroduces the megabyte-scale cost this
split was measured against and gives back none of the benefit, because
those artifacts are already either regenerable or evidentiary rather than
decisive.

## Format

Entries follow `campaign-autopilot`'s ledger entry format, in one of two
shapes (`campaign-autopilot`'s "The decision ledger" section has the full
rule, corrected 2026-08-30 by this campaign's own final review, finding
M2): a numbered `#N [G1|G2|G4|G5|Q] — question · decision · why (precedent
cited) · alternatives discarded · ideonomy passes / overturns · capture
actions` line for a ruling made outside any single task's own review loop
(pre-flight cross-task conflicts, a Q consultation, an ad hoc controller
call), or that task's own "Task N — complete"/"Task N — fix round" section
for a ruling that section already reviewed — same required content
(question, decision, why, alternatives, ideonomy pass), different layout,
because the task write-up already interrogated it once. **Not** "whatever
shape the controller finds clearest" for an ordinary task-boundary ruling —
this campaign's own exemplar ledger used the task-narrative shape for every
live entry it wrote and an earlier draft of this README read that as
license rather than as the second, equally-required shape.

Deferred minors and parked findings genuinely may use whatever shape is
clearest, because they are notes toward a future disposition rather than a
ruling with alternatives to record.

The freshness check (spec §5) only requires a campaign's ledger file to
exist and be non-empty, not any particular internal schema — that check
resolves a campaign's ledger **by name from its slug**, never by listing
this directory, so this README and any other file dropped here are
invisible to it by construction. The schema above is a human convention the
check does not enforce, which is why getting it right depends on this
document rather than on a test.
