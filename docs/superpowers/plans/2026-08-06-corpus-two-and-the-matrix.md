# Corpus Two and the Matrix Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land `tvtropes-2012` as the second column, key each report to its own corpus, and generate the matrix that ADR 0095 deferred until a second corpus existed.

**Architecture:** The corpus arrives first as inert data with its count asserted, so it is reviewable before anything reads it. Then `check` stops hardcoding one artifact path and derives it from the corpus's own `corpus` field, with Polti's artifact renamed for symmetry. Then the matrix is rendered from the same `resolve()` outputs the columns use, and gated the same way.

**Tech Stack:** Rust 2024 (workspace), `hornvale` CLI, Make + `scripts/regenerate-artifacts.sh`.

**Spec:** `docs/superpowers/specs/2026-08-06-corpus-two-and-the-matrix-design.md`. Read it before Task 1.

## Global Constraints

- **Both corpora are frozen.** Neither `tropes/polti.trope.json` nor `tropes/tvtropes-2012.trope.json` may be edited by this work. ADR 0016 as extended by 0095 makes a post-measurement change a new column, not a correction.
- **`docs/audits/` is generated and never hand-edited** (`docs/README.md:64`). Every artifact this plan produces comes from `scripts/regenerate-artifacts.sh` and is byte-checked.
- **`route`-equivalent discipline for figures:** every number this plan writes into a document comes from the renderer, not from a human transcribing a run.
- **No change to `tropes::resolve` or the bundle vocabulary.** The 20 unsatisfiable requirements `tvtropes-2012` declares are deliberate and documented in its provenance.
- **The known reading, which must not move:** `polti-1895` is **0 stageable of 36, 1 inapplicable**; `tvtropes-2012` is **0 stageable of 409, 62 inapplicable**. A moved number means a predicate moved and is a stop condition, not a rebaseline.
- **`make quick` is `fmt-check clippy type-audit type-audit-report` — it runs no tests.** Run it before every commit, and run the tests separately: `cargo test -p hornvale` for this work's targeted tests, `make gate` (~15 min, fmt + clippy + type-audit + nextest + doctests) before the branch is offered for integration. Never `--no-verify`.
- The pre-commit hook skips `make quick` when no Rust-relevant paths are staged; that is expected for docs-only commits.
- **One gating agent at a time on this machine** (`CLAUDE.md`, decision 0086/0081): `make gate` saturates ten cores on its own, and two concurrent gates cost about thirty minutes each while both look hung. Another campaign worktree, `the-deep-realm`, is live. Stagger.

---

### Task 1: The corpus arrives as data

**Files:**
- Create: `tropes/tvtropes-2012.trope.json` (copied verbatim)
- Modify: `cli/src/tropes.rs` (`mod tests`)

**Interfaces:**
- Consumes: nothing.
- Produces: the corpus file. Task 2 renders it.

**Why it is its own task.** The corpus is 266 KB of data whose provenance was established in another repository. Landing it alone means its arrival is reviewable — count, ids, parse — before any code depends on it, and a reviewer is not reading a data blob and a control-flow change in one diff.

- [ ] **Step 1: Copy the frozen artifact**

```bash
cp /Users/nathan/Projects/ndouglas/tvtropes/corpus/tvtropes-2012.trope.json \
   tropes/tvtropes-2012.trope.json
shasum -a 256 tropes/tvtropes-2012.trope.json
```

Expected: `2f77449cb20fc9763de076b5a1ec439c2368825cb37f6f055be64ec663b6da54`, which is the hash `ndouglas/tvtropes`'s `docs/corpus-emission.md` records under *Freeze*. **If it differs, stop and report** — the file is not the artifact that was frozen before measurement, and nothing downstream is worth doing until that is explained.

- [ ] **Step 2: Write the failing test**

Add to `mod tests` in `cli/src/tropes.rs`, directly after `the_live_corpus_has_thirty_six_uniquely_identified_situations`:

```rust
    /// The second corpus, asserted the same way and for the same reason.
    ///
    /// `tvtropes-2012` is frozen before measurement exactly as `polti-1895`
    /// is, and 409 is what its own provenance document declares. Deriving
    /// this number from the file would make the assertion vacuous — the
    /// point is that a change to the corpus has to come through here and be
    /// said out loud.
    #[test]
    fn the_second_corpus_has_four_hundred_and_nine_uniquely_identified_situations() {
        let corpus = load(include_str!("../../tropes/tvtropes-2012.trope.json"))
            .expect("the second corpus parses");
        let mut seen = BTreeSet::new();
        for st in &corpus.situations {
            assert!(
                seen.insert(st.id.as_str()),
                "duplicate situation id `{}` — one situation would vanish into a \
                 BTreeMap key collision and the report would understate its denominator",
                st.id
            );
        }
        assert_eq!(
            corpus.situations.len(),
            409,
            "the frozen corpus must hold exactly 409 situations; changing that \
             changes what every preregistered number was scored against"
        );
        assert_eq!(corpus.corpus, "tvtropes-2012");
    }
```

- [ ] **Step 3: Run it**

Run: `cargo test -p hornvale --bin hornvale tropes::tests::the_second_corpus 2>&1 | tail -20`

`hornvale` is a binary-only crate: it has no `src/lib.rs` and `--lib` matches no target. Do not create one to satisfy a test invocation.

Expected: PASS if Step 1 copied the right file. A parse failure means the schema diverged and is a stop condition — report it rather than editing either the corpus or `load`.

- [ ] **Step 4: Confirm the reading, without changing anything**

```bash
cargo run -q -p hornvale -- tropes --corpus tropes/tvtropes-2012.trope.json report \
  | sed -n '1,25p'
```

Expected: header naming `tvtropes-2012`, and `Stageable 0 of 409 (62 inapplicable).` **Any other numbers are a stop condition.**

- [ ] **Step 5: Commit**

```bash
make quick
git add tropes/tvtropes-2012.trope.json cli/src/tropes.rs
git commit -m "feat(tropes): land tvtropes-2012 as the second corpus

409 character tropes, frozen before measurement in ndouglas/tvtropes and
copied here verbatim so a gate does not depend on a sibling checkout. The
count is asserted for the same reason polti's 36 is: changing it has to be
a deliberate act someone comes here to perform."
```

---

### Task 2: One artifact per corpus

**Files:**
- Modify: `cli/src/main.rs` (`cmd_tropes`, the `check` arm and the usage text at ~82-85)
- Modify: `cli/src/tropes.rs` (`render`'s header line)
- Modify: `cli/tests/trope_coverage.rs`
- Modify: `scripts/regenerate-artifacts.sh:394`
- Modify: `docs/README.md:64`
- Rename: `docs/audits/trope-coverage.md` → `docs/audits/trope-coverage-polti-1895.md`
- Create: `docs/audits/trope-coverage-tvtropes-2012.md` (generated)

**Interfaces:**
- Consumes: the corpus from Task 1.
- Produces: `tropes::artifact_path(&Corpus) -> String`. Task 3 uses the same derivation.

**The wrinkle this task exists to handle.** `render` hardcodes `Regenerate with \`hornvale tropes report\`` into every artifact's header. On the second column that instruction regenerates the *first* column and silently overwrites nothing — the reader follows it and gets Polti. The header becomes corpus-derived, which changes Polti's artifact by one line. **That is an expected, reviewable diff, not drift.**

- [ ] **Step 1: Add the path derivation**

In `cli/src/tropes.rs`, beside `render`:

```rust
/// Where a corpus's committed report lives.
///
/// Derived from the corpus's own identifier rather than passed alongside it,
/// so a caller cannot pair the wrong corpus with the wrong artifact — which
/// is exactly what the previous hardcoded path did for every corpus except
/// `polti-1895`, silently and always as a failure.
pub fn artifact_path(corpus: &Corpus) -> String {
    format!("docs/audits/trope-coverage-{}.md", corpus.corpus)
}

/// The command that regenerates a report, for the header.
///
/// Takes the path the caller actually used rather than deriving one from the
/// corpus id: `polti-1895` lives in `tropes/polti.trope.json`, so a derived
/// stem would print a regenerate command naming a file that does not exist.
pub fn regenerate_command(path: &str) -> String {
    format!("hornvale tropes --corpus {path} report")
}
```

**The id-to-stem mismatch is confirmed, not hypothetical:** `tropes/polti.trope.json` carries the id `polti-1895`, and `tropes/tvtropes-2012.trope.json` carries `tvtropes-2012`. `artifact_path` may derive from the id — the artifact is named after the corpus, and both artifacts are being created by this plan. `regenerate_command` may not, which is why it takes the path.

This means `render` needs the path threaded to it. Add it as a parameter rather than storing it on `Corpus`: `Corpus` is a deserialisation of a frozen file and the path is a fact about this invocation, not about the corpus.

- [ ] **Step 2: Make the header corpus-derived**

`render` gains a `path: &str` parameter — the corpus path the caller loaded from — and its header push becomes:

```rust
    s.push_str(&format!(
        "<!-- GENERATED FILE — do not edit. Regenerate with `{}`. -->\n\n",
        regenerate_command(path)
    ));
```

Every `render` call site updates: `cmd_tropes`'s `report` and `check` arms, and any in `mod tests`. The compiler finds them.

Note this makes Polti's header read `hornvale tropes --corpus tropes/polti.trope.json report` rather than the bare `hornvale tropes report`. Both work; the explicit form is what the artifact should carry now that it is one of several.

- [ ] **Step 3: Key `check` off the corpus**

In `cli/src/main.rs`'s `cmd_tropes`, replace the `check` arm's hardcoded read:

```rust
        Some("check") => {
            let live = tropes::render(&corpus, &outcomes, &world.registry);
            let artifact = tropes::artifact_path(&corpus);
            let committed = std::fs::read_to_string(&artifact)
                .map_err(|e| format!("{artifact}: {e}"))?;
            if live == committed {
                Ok(())
            } else {
                Err(format!(
                    "trope coverage drifted for `{}`; run `make rebaseline` and review the diff",
                    corpus.corpus
                ))
            }
        }
```

Update the usage text at `cli/src/main.rs:82-85` so `--corpus`'s description no longer implies a single committed artifact.

- [ ] **Step 4: Rename and regenerate both artifacts**

```bash
git mv docs/audits/trope-coverage.md docs/audits/trope-coverage-polti-1895.md
```

Then in `scripts/regenerate-artifacts.sh`, replace line 394 with:

```bash
run -p hornvale -- tropes report > docs/audits/trope-coverage-polti-1895.md
run -p hornvale -- tropes --corpus tropes/tvtropes-2012.trope.json report \
  > docs/audits/trope-coverage-tvtropes-2012.md
```

Then `bash scripts/regenerate-artifacts.sh` and inspect:

```bash
git diff --stat docs/audits/
git diff docs/audits/trope-coverage-polti-1895.md
```

Expected: Polti's artifact differs by **exactly one line** — the header's regenerate command. `Stageable 0 of 36 (1 inapplicable).` must be unchanged. **Any other difference is a stop condition.**

- [ ] **Step 5: Make the enforcement tests per-corpus**

`cli/tests/trope_coverage.rs` has four tests. `committed_trope_coverage_matches_the_live_report` and `check_mode_agrees_with_the_committed_artifact` become per-corpus — either parameterised over a `[("polti.trope.json", "polti-1895"), ("tvtropes-2012.trope.json", "tvtropes-2012")]` slice, or duplicated once each; your call, but say which and why.

**`check_mode_fails_on_a_divergent_corpus` must keep failing for the reason it was written.** It writes a temp corpus named `divergent`; under path derivation that now resolves to `docs/audits/trope-coverage-divergent.md`, which does not exist, so `check` fails on a missing file rather than on a content mismatch. **That is a weaker test than the one it replaces.** Fix it so it still proves a content mismatch is caught — e.g. by naming the temp corpus `polti-1895` with different situations, so it resolves to a real artifact it cannot match. Confirm by running it that it fails, and say in your report which failure mode it now exercises.

- [ ] **Step 6: Update the prose reference**

`docs/README.md:64` says "the trope-coverage probe". Make it name both columns.

- [ ] **Step 7: Verify and commit**

```bash
make quick
cargo run -q -p hornvale -- tropes check
cargo run -q -p hornvale -- tropes --corpus tropes/tvtropes-2012.trope.json check
echo "both checks exited $?"
```

Both must exit 0.

```bash
git add -A
git commit -m "feat(tropes): one committed artifact per corpus

check read docs/audits/trope-coverage.md unconditionally, so any corpus
but polti compared against the wrong artifact and could only ever fail.
The path is derived from the corpus's own identifier, so a caller cannot
pair them wrongly. Polti's artifact is renamed for symmetry; its only
content change is the header naming the command that regenerates it."
```

---

### Task 3: The matrix

**Files:**
- Modify: `cli/src/tropes.rs` (add `render_matrix`)
- Modify: `cli/src/main.rs` (`matrix` mode, usage text)
- Modify: `scripts/regenerate-artifacts.sh`
- Modify: `cli/tests/trope_coverage.rs`
- Create: `docs/audits/trope-matrix.md` (generated)

**Interfaces:**
- Consumes: `artifact_path` from Task 2; `resolve` and `Corpus` unchanged.
- Produces: `tropes::render_matrix(&[(&Corpus, &BTreeMap<String, Outcome>)], &ConceptRegistry) -> String`.

**What the matrix is for.** ADR 0095: *"Corpora disagree, and the disagreement is the finding."* Neither column can show it. The document's job is to make that disagreement a generated, drift-checked artifact rather than something a reader assembles by eye — and rather than something a human writes down once and lets rot.

- [ ] **Step 1: Write the failing test**

Add to `cli/tests/trope_coverage.rs`:

```rust
/// The matrix cannot drift from the columns it summarises.
///
/// Both derive from the same `resolve()` output, so this is cheap — and it
/// closes the one gap a generated summary still has: that its figures are
/// recomputed rather than read from the reports, and could diverge if either
/// renderer changed without the other.
#[test]
fn the_matrix_agrees_with_each_committed_column() {
    let root = workspace_root();
    let matrix = std::fs::read_to_string(root.join("docs/audits/trope-matrix.md"))
        .expect("the committed matrix");
    for (corpus, stageable, inapplicable, total) in [
        ("polti-1895", 0, 1, 36),
        ("tvtropes-2012", 0, 62, 409),
    ] {
        let column = std::fs::read_to_string(
            root.join(format!("docs/audits/trope-coverage-{corpus}.md")),
        )
        .expect("the committed column");
        let headline = format!("Stageable {stageable} of {total} ({inapplicable} inapplicable).");
        assert!(
            column.contains(&headline),
            "{corpus}'s column does not carry `{headline}`"
        );
        assert!(
            matrix.contains(corpus),
            "the matrix does not name `{corpus}`"
        );
        assert!(
            matrix.contains(&total.to_string()),
            "the matrix does not carry {corpus}'s denominator {total}"
        );
    }
}
```

- [ ] **Step 2: Run it to see it fail**

Run: `cargo test -p hornvale --test trope_coverage the_matrix 2>&1 | tail -20`

Expected: FAIL — `docs/audits/trope-matrix.md` does not exist.

- [ ] **Step 3: Render the matrix**

Add `render_matrix` to `cli/src/tropes.rs`. It takes the corpora and their resolved outcomes, and emits, in this order:

1. The generated-file header, naming `hornvale tropes matrix`.
2. A statement that this measures reach against these catalogues and is not a verdict on the world — the same disclaimer the columns carry, because a matrix is more easily mistaken for a scoreboard than a single column is.
3. Per column: identifier, stageable / total, inapplicable, and a pointer to its own report.
4. **The demand table**: for every bundle either catalogue requires, its share of each catalogue, sorted by the difference between them. Shares are `situations requiring it / total situations`, computed from the corpora — not from the rendered columns, and not transcribed.
5. The bundles both rank first, and the point at which they diverge.

Sort deterministically: by descending delta, then by bundle name, so two corpora with equal deltas render stably.

Do not hand-write any figure. Everything in the document comes from the corpora and the registry.

- [ ] **Step 4: Add the mode**

In `cmd_tropes`, add `Some("matrix")`. It must load **both** corpora — the mode is not about the `--corpus` the caller passed. Read the corpus list from a constant beside `artifact_path`:

```rust
/// Every corpus this repository scores against, in render order.
///
/// A constant rather than a directory scan: which corpora are columns is a
/// deliberate act under ADR 0095, and a scan would silently add one.
pub const CORPORA: [&str; 2] = ["tropes/polti.trope.json", "tropes/tvtropes-2012.trope.json"];
```

Build the world once and resolve each corpus against it.

- [ ] **Step 5: Generate and inspect**

Add to `scripts/regenerate-artifacts.sh`, after the two column lines:

```bash
run -p hornvale -- tropes matrix > docs/audits/trope-matrix.md
```

Then `bash scripts/regenerate-artifacts.sh` and read the result. **Check these against the spec's table**, which was computed independently from the two reports:

| Bundle | `tvtropes-2012` | `polti-1895` |
|---|---|---|
| `agent-knowledge` | 53% | 19% |
| `identity-and-recognition` | 29% | 6% |
| `intent` | 25% | 50% |
| `consanguineal-kin` | 12% | 33% |
| `individual-persons` | 100% | 100% |

**If your rendered figures disagree with these, stop and report.** They were derived from the committed reports by a different route; a disagreement means one of the two derivations is wrong and that must be resolved before the document ships.

- [ ] **Step 6: Verify and commit**

```bash
make quick
cargo run -q -p hornvale -- tropes check
cargo run -q -p hornvale -- tropes --corpus tropes/tvtropes-2012.trope.json check
git status --short
```

`git status` must be clean after a regenerate — a dirty tree means the renderer is not deterministic.

```bash
git add -A
git commit -m "feat(tropes): generate the matrix ADR 0095 deferred

Both columns read 0 stageable, so the finding is not the score: polti
asks for will, blood and love; the fan taxonomy asks for knowledge,
identity and standing. They agree without exception on individual-persons
and fork immediately after. Generated rather than authored, because
docs/audits says nothing in it is hand-edited and a summary that drifts
from its own columns is worse than no summary."
```

---

## Self-Review Notes

**Spec coverage.** Corpus copied in with its count asserted → Task 1. `check` keyed off the corpus field → Task 2 Steps 1 and 3. Polti's artifact renamed with the full blast radius enumerated → Task 2 Steps 4-6. Matrix generated, not authored → Task 3. Per-corpus byte identity → Task 2 Step 5. Matrix-cannot-drift test → Task 3 Step 1. Default `report` still reproducing Polti → Task 2 Step 7's first check. Out-of-scope items (re-freezing either corpus, changing `resolve` or the vocabulary, acting on the fork, a third corpus) appear nowhere as work.

**Type consistency.** `artifact_path(&Corpus) -> String` is defined in Task 2 Step 1 and used in Task 2 Step 3. `regenerate_command(path: &str) -> String` is defined and used in Task 2 Steps 1-2 — it takes the path the caller actually used, because corpus ids do not map to file stems (see the next note). `CORPORA` is defined in Task 3 Step 4 and used by the `matrix` arm. `render_matrix` is specified in Task 3 Step 3 and called in Step 4.

**Two places the plan expects to be told it is wrong.** Task 2 Step 1 originally assumed corpus ids map to file stems. Verified before publishing: they do not — `polti.trope.json` carries `polti-1895` — so the step now states the mismatch as fact and routes `regenerate_command` through the caller's path instead. Task 3 Step 5 hands the implementer figures derived by a *different* route than the renderer will use, and instructs a stop on disagreement rather than a rebaseline — the two derivations agreeing is the evidence, and only one of them can be checked by the tests.

**The riskiest step is Task 2 Step 5.** `check_mode_fails_on_a_divergent_corpus` degrades silently under path derivation from a content check to a missing-file check, and a missing-file failure looks identical in CI. The step names the degradation and requires the implementer to say which failure mode the repaired test exercises.
