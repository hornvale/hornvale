# 0166. A generated artifact cannot be merged by regenerating it

**Status:** Accepted (2026-08-22) · **Decider:** Nathan · **Supersedes:** the
**Tier B** half of PROC-12 (`merge=hv-regenerate`); Tier A (`merge=union`) is
untouched and stays · **Relates:**
[0125](0125-github-actions-is-retired.md) (the local gates are the only gates,
so a silently wrong artifact has nothing downstream to catch it)

In the context of three separate incidents on 2026-08-20 in which
`docs/audits/type-audit-report.md` merged cleanly and wrongly, we decided that
**the six Tier B `merge=hv-regenerate` entries are deleted and the driver
retired**, because a merge driver runs *before* the merge product exists on
disk, so regeneration at that moment necessarily measures the wrong tree —
and every invocation therefore emits a confidently wrong file.

## Context

PROC-12 routed six fully-re-derived documents through a custom merge driver
(`scripts/merge-regenerate.sh`) that discards both sides' text and reruns the
generator. The premise was that neither side's committed text is
authoritative, only a fresh run of the generating command. The premise is
right. The implementation cannot deliver it, and the reason is structural
rather than a bug to be fixed in place.

The driver regenerates over the **working tree**. During a merge the working
tree is not the merge product — git's `ort` strategy has not yet written the
incoming files. `docs/audits/` sorts long before `windows/sentiment/`, so when
the type-audit tool ran, the incoming crate did not exist on disk. The tool
was not wrong about what it measured; **it measured the wrong tree.**

### It ran. This is not a story about a skipped driver

```
GIT_TRACE=1 git merge origin/main
run_command: 'scripts/merge-regenerate.sh .merge_file_… … docs/audits/type-audit-report.md'
```

12.0 s of real `cargo run`, then a clean merge, exit 0, `post-merge` hook
running normally.

### Two sessions, the same two commits, opposite answers

The merge queue on `lefford` and a campaign session merged the identical pair
of commits and each produced a clean, conflict-free, **different** result:

| merge | result |
|---|---|
| `main ← campaign/the-deed` | identifier-text 738, language 196, vessel 313, sentiment row **present** |
| `campaign/the-deed ← main` | identifier-text 739, language 201, vessel 314, sentiment row **absent** |

The output is a function of **which side is checked out**, not of the merge.
Each side silently dropped the other's contribution: one lost six primitives
across two crates, the other lost an entire crate's row — from a **default-deny
audit**, with no conflict marker. Both had been reported as separate incidents
before the mechanism connected them.

### Every invocation is wrong; the apparent successes are non-invocations

Measured in an isolated scratch repository with a sentinel driver:

```
CASE 1 — only OURS changed the path   ->  driver NOT invoked
CASE 2 — BOTH sides changed the path  ->  driver INVOKED
```

Git calls a merge driver only for a genuine three-way content merge. So:

> driver invoked ⟺ both sides changed the artifact ⟺ their sources differ in
> generator-affecting ways ⟺ the working tree is not the merge product ⟹ the
> emitted file is ours' regeneration, missing theirs.

There is no case in which the driver both matters and works. Merges where a
Tier B artifact came out correct are merges where the driver **never ran** —
one side left the path alone and git took the other. That closes the obvious
middle option: the driver cannot be repaired in place, because the information
it needs does not exist at the moment git invokes it.

### A competing fix landed while this one was being written, and it does not suffice

`929620343` ("hv-regenerate refuses to regenerate while other paths are
unmerged", via campaign/the-chroma, 2026-08-21) reached the same diagnosis
independently and shipped the fail-loudly variant: the driver recomputes the
merge from `GITHEAD_<sha>` with `merge-tree --write-tree` and exits nonzero if
any path *other than its own* comes back conflicted.

**That is careful work and it is credited here rather than its conclusion.**
Its author independently established that neither the index nor `MERGE_HEAD`
can be the signal — git performs the whole merge in memory and writes conflict
stages only after every driver has run — and verified that against real git
rather than inferring it. They also found that `merge-tree` honours
`.gitattributes`, so the probe would recurse into itself, and disabled the
driver inside it. Every one of those findings is right.

**The premise underneath is not, and no care inside the driver can reach it.**
The guard's test is *"is any other path conflicted?"*, and it treats "no" as
"the working tree is the merge product." Those are different propositions. The
working tree is stale whenever `ort` has not yet written the incoming files,
which has nothing to do with whether anything conflicted.

Reproduced with that guard's logic transcribed faithfully — `GITHEAD_<sha>`,
the `merge-tree --write-tree` recompute, the driver self-disabled inside the
probe, the same awk entry-shape filter — in an isolated scratch repository:

```
src.txt   20 lines; ours edits line 1, theirs edits line 20  -> merges CLEAN
art.md    derived from src.txt; both sides regenerated it    -> driver invoked

DRIVER: regenerating (guard passed)
Auto-merging art.md
Auto-merging src.txt
Merge made by the 'ort' strategy.

TRUTH (regenerating on the merged source):   derived: OURS + derived: THEIRS
WHAT art.md ACTUALLY HOLDS:                  derived: OURS
```

**Zero conflicted paths the whole way.** The guard passed, the driver ran, and
it dropped theirs.

That is the decisive evidence, and it is stronger than the two-direction table
above: the table shows the driver is wrong, this shows the *repair* is wrong in
the case that actually occurs. **All three of the 2026-08-20 incidents were
clean merges.** A clean merge absorbing tens of commits of source is the common
absorption shape in this repository; a merge with conflicts in the same breath
as a Tier B artifact is the rare one. `929620343` fixes the case that does not
happen here and leaves the case that always does.

### It had a test suite, and the suite had no runner

`scripts/test-merge-regenerate.sh` existed — an integration test written for
the driver in PROC-12's own Task 2. **Nothing ever invoked it.** The `outboard`
set names its eight scripts explicitly and this was not among them; PROC-12's
design document says "invoked manually or…", and manually is the only way it
ever ran. That is a large part of why the inversion survived a month.

It would not have caught this anyway, which is the more useful half: a test
that builds its own merge scenario in a scratch repository *constructs* the
working tree, so the driver reads exactly the tree the test intends. The defect
lives in the gap between the working tree and the merge product — a gap a
purpose-built fixture closes by construction and a real absorption does not.
The suite is deleted with its subject.

## Consequences

- The six Tier B entries are deleted from `.gitattributes`, along with
  `scripts/merge-regenerate.sh`, its orphaned suite
  `scripts/test-merge-regenerate.sh`, and its `git config` registration in
  `make install-hooks`. `install-hooks` keeps setting `core.hooksPath`.
- `scripts/hooks/post-merge` and `scripts/sluice-mouth.sh` both carried prose
  describing the driver as live; both are corrected. The mouth keeps its
  shape-based parse of `git merge-tree` output — the driver was one polluter of
  that stream, but the bug was trusting line *position*, and any future tool
  writing to stderr under `2>&1` would reintroduce it.
- Those six paths now take an ordinary three-way merge. That is strictly
  better: an undriven merge of an aggregate at least sometimes **conflicts**
  and forces a human to look, where the driver guaranteed a clean, plausible,
  direction-dependent answer while wearing the safeguard's name.
- **Nothing is lost.** Deleting removes zero working cases, because there were
  none — only non-invocations that never needed the driver.
- The real mechanism was already in place and is now the only one: the
  `post-merge` hook's advisory to run `make rebaseline` and diff the paths
  `docs/generated-paths.txt` declares. It is what caught all three incidents.
- `cli/tests/suite/generated_paths.rs` gains a ratchet so the entries cannot
  re-accrete, in the same shape as the test-binary ratchet.
- **The disarm is the attribute, not the config, and the config is not
  tracked.** `merge.hv-regenerate.driver` lives in `.git/config`, so every
  checkout that has run `make install-hooks` keeps a dangling registration
  pointing at a script that no longer exists. It is inert: an *attribute* is
  what triggers a driver, and the attributes are gone. Stated as sharply as it
  deserves — **after this lands, "is the driver gone?" is not a question about
  a machine at all. It is answerable only from `.gitattributes`, which is the
  thing that matters.** That makes the leftover config a curiosity rather than
  a worry, and clearing it (`git config --unset merge.hv-regenerate.driver`)
  is optional cleanup, never required for correctness.
- PROC-12's **Tier A** (`merge=union`, for append-only lists) is a different
  mechanism with a different failure mode and is **unaffected**. It uses
  git's builtin union driver, regenerates nothing, and keeps both sides'
  appended lines. Its own documented caveat — union is wrong for a *rewrite*
  — stands unchanged.

## Alternatives considered

- **Fix the driver to regenerate the merge product.** Rejected as impossible
  in place: at driver time the merge product does not exist, and a driver
  cannot force git to finish writing the other paths first.
- **Keep the driver but make it fail loudly** — shipped as `929620343` and
  measured above. It is strictly better than the original and still wrong in
  the only case we have ever observed, because it keys on conflicts and the
  defect does not need one.
- **Regenerate from the merge-tree's output tree.** The correct automation, and
  `929620343` is one step from it: it already computes that tree and discards
  the OID. Materialising it and running the generator there would measure the
  right tree. Rejected here on cost and scope — a full build in a temp checkout
  per artifact per merge — not on soundness. If someone wants it, this decision
  is supersedable and that is the mechanism working.
- **Regenerate in `post-merge`.** The cheap form of the same idea, and the one
  worth pursuing: `post-merge` runs at the one moment the tree genuinely **is**
  the merge product, so it is not structurally impossible the way a driver is.
  Deliberately not adopted inside a decision whose job is retiring something
  else; it belongs in an idea-registry row and its own change.

**On sequencing, because retiring a 30-hour-old fix deserves an argument.**
Retiring a mechanism that is wrong in the common case does not depend on
having its replacement ready. The two alternatives above are proposals for
what *should* exist; keeping a broken driver until one of them ships would
mean choosing silent wrong output over an ordinary three-way merge in the
meantime. The replacement is named here so it is not lost, which is the whole
reason to write it down rather than argue it.
