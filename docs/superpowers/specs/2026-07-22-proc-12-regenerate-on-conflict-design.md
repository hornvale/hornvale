# PROC-12: regenerate-on-conflict for generated artifacts

**Working campaign name:** The Standing Offer.

**Status:** draft, awaiting G3 (spec review).

**Worktree:** `~/.config/superpowers/worktrees/hornvale/proc-12`, branch `proc-12`, off `main` @3c42db40.

## 1. Problem

This project commits generated artifacts (reference pages, a type-audit
report, chronicle indices, idea-registry rows) so they can be
drift-checked against the code that produces them (decision 0013 and the
project's own "the book may never lag merged reality" discipline). But
committing them is exactly what makes them collide: two parallel
campaign sessions each regenerate `docs/audits/type-audit-report.md` or
add a row to `book/src/SUMMARY.md`, and a later merge sees a textual
conflict on a file neither session actually meant to disagree about — the
underlying facts (a) don't conflict at all (both regenerations are
correct, just stale relative to each other) or (b) are two independent
additions that both belong.

This isn't hypothetical for this project tonight: closing PROC-18 and
PROC-19 back-to-back hit `docs/audits/type-audit-report.md` conflicts
during main-absorption on both closes, and `book/src/SUMMARY.md`
conflicts are a standing, named pattern across many prior campaigns'
closes (see decision ledger and prior campaign retrospectives). `make
preflight` already warns about this class of collision; it doesn't
remove it.

## 2. Goal

Turn the mechanical half of this collision class into a no-op: a
generated artifact's merge conflict resolves itself, either by
regenerating the artifact fresh (for files that are a complete
re-derivation from current repo state) or by keeping both sides' content
(for files that are append-only human/campaign-driven lists) — without a
human ever seeing conflict markers in these specific files.

## 3. Scope

Research (reading `scripts/regenerate-artifacts.sh`, this project's
census/keystone conventions, and this session's own carve-out policy) found
the "generated artifact" idea splits into shapes that need genuinely
different treatment, not one uniform mechanism:

### 3.1 In scope

**Tier A — append-only lists** (git's builtin `merge=union` attribute,
zero custom code):
- `book/src/SUMMARY.md`
- `book/src/frontier/idea-registry.md`

Both files grow by independent, human-authored additions (a chronicle
bullet per campaign; a registry row per idea) that never depend on each
other's content — the correct resolution of "two sessions both added a
line" is "keep both lines," which is exactly what `union` does, built
into git, with no external command and no local setup step.

**Tier B — fully re-derived documents** (a custom merge driver that
reruns the specific generator and discards both sides' text):
- `docs/audits/type-audit-report.md` (`tools/type-audit`'s own `report`
  subcommand — a separate tool outside the workspace, decisions
  0027/0028)
- `book/src/reference/concept-registry-generated.md` (`hornvale --
  concepts`)
- `book/src/reference/concept-manifest-generated.md` (`hornvale --
  concepts --manifest`)
- `book/src/reference/stream-manifest-generated.md` (`hornvale --
  streams`)
- `book/src/reference/phonology.md` (`hornvale -- phonology`)
- `book/src/reference/proto-goblinoid-generated.md` (`hornvale --
  proto`)

Each of these is produced by exactly one independent `cargo run`
invocation in `scripts/regenerate-artifacts.sh`, with no ephemeral
world-file dependency — the cheapest, lowest-risk slice of the
regenerate-artifacts pipeline, and (per decision ledger #2 in this
campaign's worktree) the exact files this project's own recent closes
hit conflicts on.

### 3.2 Explicitly out of scope (and why — this is the load-bearing section)

- **Census data** (`book/src/laboratory/generated/*/rows.csv`,
  `schema.json`, and their derived `.svg`/`.md` summaries). A census
  re-run is a full re-derivation, not an append (so Tier A's union merge
  is wrong for it — it would interleave two incompatible full sweeps'
  rows), but census regeneration is itself gated behind explicit
  authorization (`HV_CENSUS=1`, ~7 minutes locally per campaign) under
  this project's own standing policy — an automatic merge driver
  silently re-running a census mid-merge, with no human in the loop,
  would violate that policy outright. Left as a real conflict.
- **Gallery artifacts needing an ephemeral built world**
  (`book/src/reference/dictionary-generated.md`, every almanac, map,
  scene export, and possession transcript in
  `scripts/regenerate-artifacts.sh`). These need a throwaway world built
  first, and several need a hand-authored preamble re-injected around the
  generator's own output — real additional per-file driver logic beyond
  a flat "one path, one independent command" dispatch table, and none of
  them are files this project has actually hit conflicts on. Banked as a
  named follow-up (decision ledger #4), not built now — matches this
  session's own "small, right-sized campaign" discipline (see
  [The Compound Word](../../../book/src/chronicle/the-compound-word.md),
  which deferred its own general form the same way).
- **Golden/keystone test fixtures** — `cli/tests/fixtures/world-seed-42.json`,
  `cli/tests/fixtures/pre-branches-seed-42-world.json`,
  `windows/scene/tests/fixtures/tiles-seed-1-w16.json`. **This is the
  single most important exclusion in this spec.** A keystone fixture is
  *deliberately* frozen at a specific point — its entire purpose is to
  catch unintended drift between what the current code produces and what
  it produced when the fixture was last pinned (`closing-a-campaign`'s
  own documented distinction between golden pins, re-pinned in the
  drifting commit, and keystone fixtures, refrozen at merge from main's
  tip, both always by deliberate human action). Auto-regenerating one of
  these on merge conflict would silently erase the exact signal it
  exists to produce: a conflict here means two branches disagree about
  what a world looks like, and *that disagreement is the finding*, not
  noise to smooth over. These files get no `.gitattributes` entry at
  all — conflicts on them stay real conflicts, exactly as today.

## 4. Design

### 4.1 Tier A — `.gitattributes`

```gitattributes
book/src/SUMMARY.md merge=union
book/src/frontier/idea-registry.md merge=union
```

No `git config` step needed — `union` is a git builtin merge driver,
resolvable purely from `.gitattributes` on any clone. Accepted,
documented tradeoff: `union` does not guarantee ordering of the two
sides' added lines (only that neither side's content is lost) — cosmetic
only for a chronicle ToC or a registry table; no factual content is ever
discarded.

### 4.2 Tier B — a custom merge driver

**`scripts/merge-regenerate.sh`** (new file), invoked by git as
`scripts/merge-regenerate.sh %O %A %B %P` per the `gitattributes(5)`
merge-driver contract: `%A` is the temp file the driver must overwrite
with the resolved content; `%P` is the real repo-relative path being
merged, used to select which generator command to run. `%O`/`%B` (the
ancestor's and the other side's versions) are accepted but never read —
the whole point is that neither side's text is authoritative, only a
fresh regeneration is.

```bash
#!/usr/bin/env bash
# scripts/merge-regenerate.sh %O %A %B %P — a git merge driver (PROC-12).
# Regenerates a fully-re-derived artifact instead of textually merging
# it. Exits nonzero (writing nothing) on any failure.
#
# CORRECTED (found and verified empirically during Task 2, not merely
# asserted from documentation prose — see decision ledger #7): on a
# nonzero exit, git does NOT fall back to textual <<<<<<< diff3 markers
# for a path governed by a custom `merge=<driver>` attribute — that
# fallback is only git's default, undriven conflict handling. Instead
# the path is left marked UNMERGED in the index (three conflict stages —
# base/ours/theirs), with the working-tree file at its pre-merge "ours"
# content. `git status`/`git ls-files -u` surface the real conflict
# either way, and the file is never left with partial/broken driver
# output — the safety property holds, only the exact on-disk shape of
# the fallback differs from git's default undriven path.
set -euo pipefail

ours="$2"
path="$4"

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

tmp_out="$(mktemp)"
trap 'rm -f "$tmp_out"' EXIT

case "$path" in
    docs/audits/type-audit-report.md)
        cargo run --manifest-path tools/type-audit/Cargo.toml -- report > "$tmp_out"
        ;;
    book/src/reference/concept-registry-generated.md)
        cargo run -q -p hornvale -- concepts > "$tmp_out"
        ;;
    book/src/reference/concept-manifest-generated.md)
        cargo run -q -p hornvale -- concepts --manifest > "$tmp_out"
        ;;
    book/src/reference/stream-manifest-generated.md)
        cargo run -q -p hornvale -- streams > "$tmp_out"
        ;;
    book/src/reference/phonology.md)
        cargo run -q -p hornvale -- phonology > "$tmp_out"
        ;;
    book/src/reference/proto-goblinoid-generated.md)
        cargo run -q -p hornvale -- proto > "$tmp_out"
        ;;
    *)
        echo "merge-regenerate: no dispatch entry for '$path'" >&2
        exit 1
        ;;
esac

cp "$tmp_out" "$ours"
```

The dispatch happens in a temp file first, copied to `$ours` only on
success — if any generator command fails partway (most likely: the crate
doesn't currently build, because a *different*, unrelated conflict
elsewhere in the same merge hasn't been resolved yet), the script exits
nonzero before ever touching `$ours`, and git leaves the path unmerged
(three conflict stages in the index; the working-tree file at its
pre-merge "ours" content — not textual diff3 markers, which is only
git's default undriven fallback) rather than committing anything the
driver produced.

**`.gitattributes`** (same file as Tier A, additional lines):

```gitattributes
docs/audits/type-audit-report.md merge=hv-regenerate
book/src/reference/concept-registry-generated.md merge=hv-regenerate
book/src/reference/concept-manifest-generated.md merge=hv-regenerate
book/src/reference/stream-manifest-generated.md merge=hv-regenerate
book/src/reference/phonology.md merge=hv-regenerate
book/src/reference/proto-goblinoid-generated.md merge=hv-regenerate
```

**Registration** — extending the existing `make install-hooks` target
(`Makefile`, currently `git config core.hooksPath scripts/hooks`) with
one more `git config` line:

```makefile
install-hooks: ## Point git at scripts/hooks + register the regenerate-on-conflict merge driver (PROC-12)
	git config core.hooksPath scripts/hooks
	git config merge.hv-regenerate.driver 'scripts/merge-regenerate.sh %O %A %B %P'
	@echo "git hooks path set to scripts/hooks; 'make quick' now runs pre-commit."
	@echo "merge.hv-regenerate driver registered for generated-artifact conflicts."
```

**Safe-by-default for anyone who hasn't run `make install-hooks`:** if
`.gitattributes` names a merge driver (`hv-regenerate`) that has no
corresponding `git config merge.hv-regenerate.driver` entry, git falls
back to its standard 3-way text merge for that file, exactly as if the
attribute weren't set at all (`gitattributes(5)`'s documented behavior).
A fresh clone or worktree that skips the one-time setup step sees no
change in behavior — it just doesn't get the new no-op resolution until
it runs `make install-hooks`, same as `core.hooksPath` today.

## 5. Determinism and save-format impact

**None.** This campaign touches no domain, kernel, or window crate — only
`.gitattributes`, `Makefile`, and one new shell script. Tier B's driver
runs the exact same commands `scripts/regenerate-artifacts.sh` already
runs for these files; it produces no new content, only resolves *when*
that regeneration happens (at merge-conflict time instead of only at
close-time regen).

## 6. Testing

Git merge drivers aren't natively unit-testable as Rust code — this is
process tooling. Verification is a scripted integration check:

- A test script (`scripts/test-merge-regenerate.sh`, invoked manually or
  from `make quick`) that: creates a throwaway git branch, makes a
  trivial content change to one Tier B file on each of two divergent
  branches (e.g. append a stray line to `book/src/reference/phonology.md`
  on both), merges them with the driver active, and asserts (a) the
  merge succeeds with no conflict markers left in the file and (b) the
  resulting file byte-matches a fresh `cargo run -p hornvale --
  phonology` — proving the driver actually regenerated rather than
  keeping one side's stray line.
- A second case exercising the failure path: temporarily break the build
  on BOTH divergent branches (touching the same region of a source file
  so it genuinely conflicts too, not just one side — a one-sided change
  auto-merges cleanly and never actually leaves the build broken when
  the driver runs, a real bug this campaign's own Task 2 found and
  corrected, see decision ledger #6), attempt the same merge, and assert
  the driver exits nonzero and the Tier B file is left unmerged (`git
  ls-files -u` shows conflict stages) with non-empty content — not
  textual diff3 markers (decision ledger #7), and never empty or partial
  content.
- Tier A needs no custom test — `union` is git's own tested builtin;
  a smoke check (two branches each adding a different `SUMMARY.md`
  bullet, merged, both present) is enough to confirm the `.gitattributes`
  wiring itself is correct.
- `make gate` unaffected — this campaign adds no Rust code, so the
  existing gate simply needs to stay green (confirming nothing else
  regressed).

## 7. Non-goals

- Census data, gallery artifacts, and golden/keystone fixtures (§3.2) —
  explicitly excluded, not deferred-by-oversight.
- Any change to `scripts/regenerate-artifacts.sh` itself, or to what any
  of these commands produce — this campaign only changes *when* Tier B's
  six commands run (additionally, at merge-conflict time), never *what*
  they produce.
- A "regenerate everything" fallback path for files with no dispatch
  entry — the dispatch table's `*)` case exits nonzero deliberately
  (falls back to a normal conflict) rather than guessing at a command,
  so adding a new Tier B file always requires an explicit one-line
  addition to the dispatch table, never silent, accidental coverage.

## 8. Decision ledger

Full reasoning (including the ideonomy pass and the two exclusion
findings, §3.2) is recorded in this campaign's worktree at
`.superpowers/sdd/decision-ledger.md` (entries #1-#5).
