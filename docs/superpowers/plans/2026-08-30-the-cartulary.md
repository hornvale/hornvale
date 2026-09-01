# The Cartulary Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make a campaign's decision ledger a committed document, so it
survives the worktree it was written in.

**Architecture:** One new tree, `docs/superpowers/ledgers/`, holding one
markdown file per campaign — a third sibling to `specs/` and `plans/`. The
in-repo skills write the durable kinds there when they occur. The vendored
plugin's scratch ledger is untouched and keeps its own job.

**Tech Stack:** Markdown, Rust 2024 (one test in `cli/tests/suite/`), the
repo's existing append-never fixture idiom. No new dependency.

**Spec:** `docs/superpowers/specs/2026-08-30-the-cartulary-design.md`

## Global Constraints

- **No new workspace dependency.** `serde`, `serde_json`, `libm` only
  (decision 0004).
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only, enforced by
  `clippy.toml`.
- **Every check states the direction it enforces and what it is blind to**, in
  its own doc comment. House style in `docs_consistency.rs`, and this
  campaign's own subject matter.
- **`cargo fmt --all` last, then `make gate-commit`,** before every commit.
- **Supersede visibly, never edit silently.** Two committed statements become
  false in this campaign. The project's idiom — and this spec's own thesis —
  is that a record which outlives its subject produces wrong answers from
  good-faith readers, so the correction must be legible as a correction.
- **Do not touch the vendored plugin.**
  `~/.claude/plugins/cache/claude-plugins-official/superpowers/6.3.0/` is not
  in this repository. A local edit there is not committable and would be
  overwritten by the next plugin update. See spec §4a.
- **Push at every task boundary.**

## What this plan is about, applied to itself

This campaign exists because a manual copy-at-close failed five times. **The
plan therefore forbids itself the same shape:** no task may introduce a step
where durable material is copied from scratch into the committed ledger at
intervals. The committed ledger is written to directly, when the material
occurs. If a task finds itself specifying a copy, that is the defect, not the
implementation.

**Task 1 makes this campaign use its own deliverable.** From Task 2 onward the
controller writes rulings into `docs/superpowers/ledgers/2026-08-30-the-cartulary.md`
and commits them. That is the campaign dogfooding its own output, and it is
also the only way Task 5's hypothesis can be tested at all.

## File structure

| file | responsibility |
|---|---|
| `docs/superpowers/ledgers/2026-08-30-the-cartulary.md` | CREATE — this campaign's own ledger, and the tree's first inhabitant |
| `scripts/hooks/pre-commit:137-155` | MODIFY — supersede the stated rationale; the guard itself is unchanged |
| `CLAUDE.md:1119` | MODIFY — narrow the absorption-clobber warning to the path it is true of |
| `.claude/skills/campaign-autopilot/SKILL.md:190` | MODIFY — the decision ledger's location |
| `.claude/skills/closing-a-campaign/SKILL.md:32` | MODIFY — step 2's sweep, now that the durable half does not die |
| `cli/tests/fixtures/ledger-exempt-campaigns.txt` | CREATE — the append-never exemption list |
| `cli/tests/suite/docs_consistency.rs` | MODIFY — the ratchet check and its blindness |

---

### Task 1: The tree, and this campaign's own ledger in it

**Files:**
- Create: `docs/superpowers/ledgers/README.md`
- Create: `docs/superpowers/ledgers/2026-08-30-the-cartulary.md`

- [ ] **Step 1: Create the tree with a README that states what belongs here.**
  One file per campaign, named `YYYY-MM-DD-<slug>.md` to match `specs/` and
  `plans/`. The README must say what goes in a ledger and — more usefully —
  **what does not**: implementer reports, reviews and review packages stay in
  `.superpowers/` and remain scratch. State the reason (a review package is
  `git diff` output, regenerable by construction; a report is evidence for a
  ruling the ledger already records) so a later reader does not "helpfully"
  widen the tree.

- [ ] **Step 2: Create this campaign's own ledger**, seeded with the entries
  that already exist for it — they are in the spec's §1 and §4a and in the
  controller's own rulings so far. **From this point the controller writes
  rulings here, not to scratch.**

- [ ] **Step 3: Verify the path is not ignored.**

```bash
git check-ignore -v docs/superpowers/ledgers/2026-08-30-the-cartulary.md; echo "exit=$?"
```

Expected: `exit=1` and no output — nothing ignores it. **If exit=0, STOP**: a
`.gitignore` rule reaches this path and the whole design is defeated silently,
which is exactly the failure class this campaign exists to remove.

- [ ] **Step 4: Verify the pre-commit hook does NOT refuse it.** The hook
  refuses staged `.superpowers/` paths. This path is not under that directory,
  so it must pass — but confirm rather than assume, because the guard's regex
  is `(^|/)\.superpowers/` and a mistaken widening would block every commit in
  this campaign.

```bash
git add docs/superpowers/ledgers/ && git status --porcelain docs/superpowers/ledgers/
```

- [ ] **Step 5: `make gate-commit`, commit, push.**

---

### Task 2: Supersede the two false statements

**Files:**
- Modify: `scripts/hooks/pre-commit` (the comment block at 137-155)
- Modify: `CLAUDE.md:1119`

**The guard's behaviour does not change.** It keeps refusing `.superpowers/`
paths, which stays correct for the reports, reviews and up-to-3.4 MB of
regenerable diffs that remain there. Only the stated rationale narrows.

- [ ] **Step 1: Supersede the hook's rationale.** Line 145 currently reads:

```
# so committing it is always a mistake, never a judgment call.
```

That rests on "promotion at close works". Replace the claim with the
measurement: five recorded failures (spec §1), two of them *after* campaigns
added verification. Say plainly that the sentence was a judgment, that it has
been falsified, and that the guard survives its own rationale because what
remains under `.superpowers/` is regenerable or derivative.

- [ ] **Step 2: Narrow the `CLAUDE.md` warning.** Line 1119 reads:

```
is git-ignored (never force-add it: a committed ledger silently clobbers every
```

The hazard is real **and it is a property of the shared filename**, not of
committing. Every campaign writes `.superpowers/sdd/decision-ledger.md`, so two
editing it merge to one side with no conflict. (Corrected 2026-08-30, final
review finding I2: this originally named `progress.md`, which has always been
per-campaign-keyed and never carried this hazard; see decision 0493.) State
that, and that a per-campaign path has no such collision — one campaign
touches one file, ever. Point at `docs/superpowers/ledgers/`.

- [ ] **Step 3: Prove the guard still fires.** Stage a file under
  `.superpowers/` with `git add -f`, confirm the hook refuses, unstage. Paste
  the refusal. A superseded rationale must not become a weakened guard, and
  the only way to know is to make it refuse.

- [ ] **Step 4: `make gate-commit`, commit, push.**

---

### Task 3: The ratchet check

**Files:**
- Create: `cli/tests/fixtures/ledger-exempt-campaigns.txt`
- Modify: `cli/tests/suite/docs_consistency.rs`

**Interfaces:**
- Consumes: the existing `fn repo_root() -> PathBuf` in that file.
- Produces: `fn ledger_exempt_campaigns() -> BTreeSet<&'static str>`, modelled
  on the existing `fn registry_length_waivers()` at line 461.

- [ ] **Step 0 (BLOCKING): establish the population.** There are ~311 specs
  and ~293 plans and zero ledgers. **Count them yourself** — a check applied
  retroactively reddens for every campaign ever run and is deleted within a
  day. Report the real numbers before writing anything.

- [ ] **Step 1: Build the exemption list.** Every campaign slug that has a
  spec or plan today goes in `cli/tests/fixtures/ledger-exempt-campaigns.txt`,
  one per line, generated mechanically rather than typed. It is **append-never
  in the shrinking direction only**: entries may be removed when a campaign
  gains a ledger, never added — a new campaign must not be able to exempt
  itself.

- [ ] **Step 2: Write the failing test**

```rust
/// A campaign with a spec and a plan also has a ledger.
///
/// # Direction this check enforces
///
/// spec-and-plan implies ledger. It is blind to a ledger with no campaign
/// (harmless), and blind to every campaign in
/// `cli/tests/fixtures/ledger-exempt-campaigns.txt` — the ~293 that predate
/// this convention. That list may only SHRINK: a campaign gaining a ledger
/// drops out of it, and a new campaign cannot add itself.
///
/// # What it cannot see, stated because a check that does not say so reads
/// as total
///
/// It sees that a ledger file exists and is non-empty. It cannot see whether
/// the contents are honest, whether they are complete, or whether they were
/// written as the campaign ran rather than backfilled in one sitting at
/// close. Those are the properties that actually matter and none of them is
/// mechanically checkable — the same three-valued honesty `tropes check` and
/// type-audit's `waiver(...)` carry.
#[test]
fn every_campaign_with_a_spec_and_a_plan_has_a_ledger() {
    let exempt = ledger_exempt_campaigns();
    let missing: Vec<String> = campaigns_with_spec_and_plan()
        .into_iter()
        .filter(|slug| !exempt.contains(slug.as_str()))
        .filter(|slug| !ledger_exists_and_is_nonempty(slug))
        .collect();
    assert!(
        missing.is_empty(),
        "campaigns with a spec and a plan but no ledger at \
         docs/superpowers/ledgers/. A campaign's rulings, deferred minors and \
         parked findings belong in a committed file — scratch dies with the \
         worktree, which has cost this project five recorded losses:\n  {}",
        missing.join("\n  ")
    );
}
```

- [ ] **Step 3: Run to verify it fails.** Expected: FAIL — the helper
  functions do not exist.

- [ ] **Step 4: Implement** `ledger_exempt_campaigns()`,
  `campaigns_with_spec_and_plan()` and `ledger_exists_and_is_nonempty()`.
  Model the first on `registry_length_waivers()` at line 461 verbatim in shape
  — `include_str!`, `.lines()`, `.map(str::trim)`, `.filter(non-empty)`,
  `.collect()` — rather than inventing a second loading idiom.

- [ ] **Step 5: Prove it discriminates, BOTH ways.** A test that passes on
  today's tree asserts nothing:

  1. Remove this campaign's own slug from the exemption list (it should not be
     there — it has a ledger) and delete its ledger. **Expected: FAIL**, naming
     the slug. Restore.
  2. Add a fabricated slug to the exemption list that has no spec or plan.
     This is the companion direction — an exemption list that can grow
     silently is not append-never, it is just a list. The model is already in
     this file: `the_waiver_list_only_shrinks` at line 489 asserts exactly
     this for `registry-length-waivers.txt`, and refuses a waived ID that no
     row claims. **Write the equivalent, or state why the two cases differ**
     — one sentence either way, in the doc comment, not the report.
     **Expected: FAIL**, naming the fabricated slug.

  Paste both messages.

- [ ] **Step 6: `cargo fmt --all`, `make gate-commit`, commit, push.**

---

### Task 4: The in-repo skills write to the durable path

**Files:**
- Modify: `.claude/skills/campaign-autopilot/SKILL.md` (the Location line at 190)
- Modify: `.claude/skills/closing-a-campaign/SKILL.md` (step 2, line 32)
- Modify: `.claude/skills/dispatching-hornvale-subagents/SKILL.md`

**Do NOT touch the vendored plugin.** `subagent-driven-development` lives at a
versioned path outside this repository; its `progress.md` keeps its own job
(resume-after-compaction, task state) and stays scratch. See spec §4a.

- [ ] **Step 1: Re-point `campaign-autopilot`'s ledger location.** Line 190
  currently reads:

```
Location: the campaign worktree's `.superpowers/sdd/decision-ledger.md`
(scratch-in-worktree rule — never the shared main checkout).
```

It becomes `docs/superpowers/ledgers/YYYY-MM-DD-<slug>.md`, committed as
entries occur. **The "never the shared main checkout" clause still matters and
must survive in some form** — the ledger is written in the campaign's own
worktree on the campaign's own branch, not in main's checkout.

- [ ] **Step 2: Rewrite `closing-a-campaign`'s step 2.** Its current text
  assumes the ledger dies at teardown and must be swept beforehand. The
  durable half no longer dies. **But do not delete the step** — reports and
  reviews still die, and step 2's own warning about The Ell losing six of nine
  is still the reason anyone reads it. Narrow it to what remains scratch, and
  say explicitly which half is now safe and why.

- [ ] **Step 3: Add the ledger to `dispatching-hornvale-subagents`** where it
  describes what a controller carries between tasks — a one-line pointer, not
  a restatement.

- [ ] **Step 4: Check no other in-repo file still claims the ledger is
  scratch.**

```bash
grep -rn 'decision-ledger\|sdd/progress\|scratch.*ledger\|ledger.*scratch' \
  --include='*.md' .claude/ docs/ CLAUDE.md | grep -v superpowers/ledgers
```

Every hit is either correct (about the plugin's `progress.md`, which IS still
scratch) or stale. **Classify each one and say which.** This is the step that
catches the stale-prose-beside-corrected-fact defect this project keeps
paying for.

- [ ] **Step 5: `make gate-commit`, commit, push.**

---

### Task 5: H1 — does the ledger actually survive?

**Files:** none. This task produces a measurement.

- [ ] **Step 1: Read this campaign's ledger from git, not from disk.**

```bash
git show HEAD:docs/superpowers/ledgers/2026-08-30-the-cartulary.md | head -40
```

- [ ] **Step 2: Simulate the loss that motivated the campaign.** In a scratch
  clone or a second worktree of this branch — **not** by destroying the live
  worktree — confirm the ledger is present and complete when the original
  worktree's `.superpowers/` is absent. The Attestation's loss was exactly
  this: the branch survived, the scratch did not.

- [ ] **Step 3: Report H1.** Confirmed or falsified. **If falsified, STOP** —
  the campaign has no purpose and the remaining tasks should not be run.

- [ ] **Step 4: Report H2 honestly.** The spec predicts that committing the
  ledger *does* slightly change what gets written into it, and predicts that
  this is unmeasurable from inside. **Do not manufacture a measurement.**
  "Unmeasurable from here, and here is why" is the preregistered answer and is
  worth more than a fabricated one. If you *can* think of an instrument, name
  it and say what it would cost.

- [ ] **Step 5: Record both in the ledger and commit.**

---

### Task 6: Artifacts, book, chronicle, retrospective, decisions, registry

**Definition of Done (CLAUDE.md Process, decisions 0013, 0020) — not optional.**

- [ ] **Step 1: `make rebaseline`, then `git status` IMMEDIATELY**, then the
  drift check. Note the path list is two columns since The Attestation:

```bash
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$' | cut -f1)
```

**Branch table:** `docs/audits/` and `docs/digest/` moving is EXPECTED.
Anything under `book/src/domesday/`, `book/src/gallery/` or an almanac moving
→ **STOP**, that is a determinism finding.

- [ ] **Step 2: Decision records** from block **0486–0495**, per spec §10.
Derive the real list from what shipped — §10 was written before Tasks 1-5 and
the two-ledger finding (§4a) arrived after it, so at least one record's
framing will have moved.

- [ ] **Step 3: Chronicle** `book/src/chronicle/the-cartulary.md`, wired into
`book/src/SUMMARY.md`. **Lead with the measurement, not the intent:** five
recorded losses, two of them after verification was added, and what H1
returned.

- [ ] **Step 4: Retrospective** `docs/retrospectives/the-cartulary.md` plus its
index row.

- [ ] **Step 5: Registry** — a row for the deferred half: reports and reviews
still die with the worktree, carrying the measurement (~200 K prose per
campaign, of which the ledger is ~15 K).

- [ ] **Step 6: Freshness sweep + Confidence Gradient.** **Grep
`book/src/open-questions.md` before concluding no bet moved.**

- [ ] **Step 7: `make gate-commit`, commit, push.** Do NOT submit to the merge
queue — that is the controller's.
