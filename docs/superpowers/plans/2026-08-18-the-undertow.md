# The Undertow Implementation Plan

> **EVERY SUBSTRATE FIGURE QUOTED BELOW IS PRE-ABSORPTION.** This branch ran to
> completion without absorbing `main`; The Underworld had moved settlement
> placement, so the substrate this plan was written against is not the one the
> campaign lands on. The plan is kept as the record of what was planned and is
> deliberately not re-derived. The re-derived figures, and the disclosure of what
> moved, are in the spec's **Erratum 0** (`docs/superpowers/specs/2026-08-18-the-undertow-design.md`).


> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make a seam crossing cost damage in proportion to how much the two
peoples are strangers, so that a community keeps its own account of a shared
history — and measure whether that breaks the pooling The Parley found.

**Architecture:** One new arm on the existing `Transmission` policy. The penalty
enters through the accumulating **width**, reusing machinery two probes already
exonerated, rather than adding an ordering-key component. Its magnitude is read
from a people-pair contact tally derived **once** at `Walk` construction.

**Tech Stack:** Rust 2024, `windows/hearsay` (a read-only window over a
committed ledger), `cargo nextest`, hand-built fixtures in `tests/common/mod.rs`.

**Spec:** `docs/superpowers/specs/2026-08-18-the-undertow-design.md` — read §3
(substrate, two probes), §5 (the derivation), §6 (the preregistration) before
Task 1. §5.4 carries Nathan's G3 ratification and governs the design.

## Global Constraints

- **Dependencies frozen:** `serde`, `serde_json`, `libm` only. Add none.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only. Float ordering
  via `total_cmp`, or `f64::to_bits` on a non-negative finite key.
- **No wall-clock time.** `Instant` is banned in tests too.
- `#![warn(missing_docs)]`; every new `pub` boundary primitive needs a
  `type-audit:` tag. Adding one drifts `docs/audits/type-audit-report.md` —
  regenerate it in the SAME commit with
  `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`.
  The redirect writes the file; running it bare regenerates nothing. Every task
  of the predecessor campaign hit this.
- **No `Default` impl on any arm enum.** `Accumulation`, `Clock`,
  `Perpetration` and `Contact` each carry an explicit "deliberately no
  `Default`" note; match it.
- **`Transmission::AS_SHIPPED` must stay behaviour-identical.** Every number the
  readout reports is a difference from it. `tests/transmission.rs` is the anchor
  — if it reddens, fix your code, never the test.
- **Never touch** `amplitude.rs`'s `gen_span` return, `accumulate.rs`'s
  `step`/`precision_at` (The Palimpsest's frozen unit erratum), or `derive.rs`'s
  `claims_about` (campaign 1's pinned baseline).
- **NO CONSTANT MAY ENCODE A PREFERENCE BETWEEN TWO PEOPLES** (spec §5.4). If a
  magnitude cannot be traced to a ledger fact it is authored, and that is the
  one thing decision 0021 actually forbids. This is not style; it is the
  campaign's licence to exist.
- `cargo fmt` last before every commit. `make gate-commit` before committing —
  but it is an **allow-list** (`docs/timings/subfloor-roster.tsv`) and will run
  **none** of your new tests, so always run yours by name too and paste it.
- Keep `docs/timings.md` rows in their own `chore(timings)` commit.
- **You may override this plan.** If a stated fact is wrong or a better test
  exists, do the better thing and **say so in your report**. What you may not do
  is quietly weaken a check.

---

### Task 1: The people-pair contact tally, derived once

**Files:**
- Modify: `windows/hearsay/src/contact.rs`
- Modify: `windows/hearsay/tests/contact.rs`

**Interfaces:**
- Produces: `ContactGraph::edges_between(&self, a: &str, b: &str) -> usize`, and
  a tally built inside `contact_of` so no caller can forget it.

**Why this is its own task:** spec §5.1 verified that today's API cannot answer
the question — `peers_of` is keyed by *occupation* and `edges()` is a bare total
(`contact.rs:55,64`). Deriving the pair count per crossing would re-scan the
graph at every step, which is the pattern `lineage.rs`'s header records The
Begat deleting from the read path and warns against reintroducing one level up.

- [ ] **Step 1: Write the failing test**

Add to `windows/hearsay/tests/contact.rs`:

```rust
/// Spec §5.1: the crossing penalty is discounted by how many raid edges lie
/// between the two peoples, so the graph must answer that directly rather
/// than making every caller re-scan it.
#[test]
fn the_graph_counts_edges_between_two_peoples() {
    // 1,2 are human; 5,6 are kobold. Two human<->kobold raids, one
    // human<->human raid, so the cross-people tally must be 2 and not 3.
    let mut led = ledger_with(&[(1, None), (2, None), (5, None), (6, None)]);
    for (occ, people) in [(1, "human"), (2, "human"), (5, "kobold"), (6, "kobold")] {
        put(
            &mut led,
            occ,
            hornvale_history::OCC_PEOPLE,
            Value::Text(people.to_string()),
        );
    }
    raid(&mut led, 1, 5, 100.0); // human <-> kobold
    raid(&mut led, 2, 6, 200.0); // human <-> kobold
    let g = contact_of(&led);

    assert_eq!(g.edges_between("human", "kobold"), 2, "both crossings count");
    assert_eq!(
        g.edges_between("kobold", "human"),
        2,
        "the pair is unordered -- (a,b) and (b,a) are one key"
    );
    assert_eq!(
        g.edges_between("human", "elf"),
        0,
        "peoples that never met count zero, and that is the EXPENSIVE case: \
         spec §5.1 divides by (1 + this), so zero means the full penalty"
    );
}

/// A same-people raid must not inflate a cross-people tally. This is the
/// discriminating case: 47.4% of endings name an attacker but only 2.33% name
/// a foreign one, so same-people raids are the overwhelming majority and a
/// tally that counted them would be dominated by noise.
#[test]
fn a_same_people_raid_does_not_count_toward_a_cross_people_pair() {
    let mut led = ledger_with(&[(1, None), (2, None), (5, None)]);
    for (occ, people) in [(1, "human"), (2, "human"), (5, "kobold")] {
        put(
            &mut led,
            occ,
            hornvale_history::OCC_PEOPLE,
            Value::Text(people.to_string()),
        );
    }
    raid(&mut led, 1, 2, 100.0); // human <-> human
    let g = contact_of(&led);

    assert_eq!(g.edges_between("human", "kobold"), 0, "no crossing happened");
    assert_eq!(g.edges_between("human", "human"), 1, "the same-people raid is still an edge");
}
```

- [ ] **Step 2: Run to verify it fails**

```
cargo test -p hornvale-hearsay --test contact 2>&1 | tail -20
```

Expected: compile failure, no method `edges_between`. **A compile-error red
proves nothing about an assertion** — Step 4 is the behavioural red.

- [ ] **Step 3: Implement**

Add a field to `ContactGraph` and populate it in `contact_of`'s existing single
pass. The pair key must be canonically ordered so `(a,b)` and `(b,a)` collapse:

```rust
    /// How many raid edges lie between each unordered pair of peoples, tallied
    /// once here rather than re-derived per crossing. Spec §5.1: the crossing
    /// penalty divides by `1 + this`, so a pair that never met pays full price.
    ///
    /// Keyed on an ORDERED tuple with the lexicographically smaller people
    /// first, so `(a,b)` and `(b,a)` are one entry — the edge is undirected and
    /// two entries would double-count it.
    between: BTreeMap<(String, String), usize>,
```

with, inside `contact_of`'s loop, after the self-raid guard:

```rust
        let people_of = |occ| match ledger.value_of(occ, hornvale_history::OCC_PEOPLE) {
            Some(Value::Text(p)) => p.clone(),
            _ => String::new(),
        };
        let (pa, pb) = (people_of(victim), people_of(attacker));
        let pair = if pa <= pb { (pa, pb) } else { (pb, pa) };
        *out.between.entry(pair).or_default() += 1;
```

and the accessor:

```rust
    /// How many raid edges lie between peoples `a` and `b`, unordered. Zero for
    /// a pair that never met — which spec §5.1 reads as the most expensive
    /// crossing, not as missing data.
    /// type-audit: bare-ok(identifier-text: a), bare-ok(identifier-text: b), bare-ok(count: return)
    pub fn edges_between(&self, a: &str, b: &str) -> usize {
        let pair = if a <= b {
            (a.to_string(), b.to_string())
        } else {
            (b.to_string(), a.to_string())
        };
        self.between.get(&pair).copied().unwrap_or(0)
    }
```

- [ ] **Step 4: Run to verify it passes, then the whole crate**

```
cargo test -p hornvale-hearsay 2>&1 | tail -20
```

| what you see | what to do |
|---|---|
| all green | Step 5 |
| a pre-existing test fails | you changed behaviour; `contact_of`'s existing outputs must not move |
| your new test passed before Step 3 | impossible unless it asserts nothing — rewrite it |

- [ ] **Step 5: Prove the canonical ordering is load-bearing**

**Demonstrate this property: with the pair-ordering swap removed (keying on
`(pa, pb)` as-given), `the_graph_counts_edges_between_two_peoples` fails.**
Assert the text you replace exists before replacing it — a `cargo fmt` rewrap
has silently made a replacement match nothing before, and the green looked
exactly like a robust implementation. Revert and paste both outputs.

- [ ] **Step 6: Check it against the real substrate**

```
cargo test -p hornvale-hearsay --test probe_contact_substrate the_raid_seam -- --ignored --nocapture --test-threads=1 > /tmp/undertow-t1.txt 2>&1; grep 'named attacker\|another people' /tmp/undertow-t1.txt
```

Then, in a temporary scratch test, print `edges_between` for the two most
common peoples on one seed and sanity-check the total across all pairs equals
`edges()`. **Report both numbers.** Remove the scratch before committing and
confirm with `git status`.

- [ ] **Step 7: Format, gate, commit**

```
cargo fmt
make gate-commit 2>&1 | tail -3
cargo test -p hornvale-hearsay --test contact 2>&1 | tail -5
git add windows/hearsay/src/contact.rs windows/hearsay/tests/contact.rs docs/audits/type-audit-report.md
git commit -m "feat(hearsay): tally raid edges per people-pair, once"
```

---

### Task 2: The crossing penalty

**Files:**
- Modify: `windows/hearsay/src/transmission.rs` (a new arm)
- Modify: `windows/hearsay/src/derive.rs`
- Modify: every `Transmission` construction site
- Test: `windows/hearsay/tests/crossing.rs` (create)

**Interfaces:**
- Consumes: `ContactGraph::edges_between` (Task 1).
- Produces: `pub enum Crossing { Free, ContactWeighted }` — same house shape as
  its siblings (derives, `pub const ALL`, `label()` with a `type-audit:` tag, no
  `Default`), and a `crossing` field on `Transmission` with
  `AS_SHIPPED.crossing == Crossing::Free`.

- [ ] **Step 1: Write the failing test**

Create `windows/hearsay/tests/crossing.rs`. Build a `Walk` exactly as
`tests/augmented_walk.rs` does. Assert three things:

1. **`Crossing::Free` is a no-op.** For the same fixture and rule, the holder
   set and every claim's `(holder, hops, precision, object)` under
   `AS_SHIPPED` are unchanged from `Contact::WithRaidSeam` today.
2. **`ContactWeighted` costs a crossing something.** On a fixture where two
   peoples have exactly ONE raid between them, a claim that crosses the seam
   arrives at a **coarser or equal** rung than the same claim under `Free`, and
   strictly coarser for at least one holder.
3. **The discount is real and is the campaign's licence.** Two fixtures
   identical except for the number of raids between the peoples (one versus
   many) must give the well-connected pair a **strictly smaller** accumulated
   width at the same holder. **This is the assertion that proves the magnitude
   is derived rather than constant** — without it, §5.4's argument is
   unevidenced and the campaign has no defence under decision 0021.

Write the fixtures into `tests/common/mod.rs` as a parameterised body with
named wrappers, the shape `two_peoples_joined_by_a_later_raid` already uses.

- [ ] **Step 2: Run to verify it fails**

```
cargo test -p hornvale-hearsay --test crossing 2>&1 | tail -20
```

- [ ] **Step 3: Implement**

`Crossing` in `transmission.rs` beside its siblings; `crossing: Crossing` on
`Transmission`; `AS_SHIPPED` gains `crossing: Crossing::Free`.

In `derive.rs`'s accumulating walk, at the point where the step's width is
computed, add — only under `Crossing::ContactWeighted`, and only when the
teller's and hearer's peoples differ:

```rust
penalty = ladder.span(Precision::FINEST) / (1.0 + edges_between(teller_people, hearer_people) as f64)
```

added to the step's `gen_span` before `rule.step`. `span(FINEST)` is the width
the accumulator already seeds itself with (`derive.rs:449`), so the penalty is
commensurate and introduces no new scale.

**Do not add a key component.** The penalty enters through `width` only.

- [ ] **Step 4: Fix every call site, then the whole crate**

```
cargo build -p hornvale-hearsay --tests 2>&1 | tail -20
cargo test -p hornvale-hearsay 2>&1 | tail -20
```

| what you see | what to do |
|---|---|
| all green, no expected value edited | Step 5 |
| a value in a pre-existing test moved | `AS_SHIPPED` is no longer shipped behaviour — fix your code |
| clippy `too_many_arguments` | move it into `Walk`, as Task 4 of the predecessor did |

- [ ] **Step 5: Prove the discount is load-bearing**

**Demonstrate this property: with the `1.0 +` denominator replaced by a
constant, the third assertion of Step 1 fails.** That assertion is the
campaign's evidence that its magnitude is derived; if a constant satisfies it,
the test is not testing what §5.4 claims. Paste the output.

- [ ] **Step 6: Format, gate, commit**

```
cargo fmt
make gate-commit 2>&1 | tail -3
cargo test -p hornvale-hearsay 2>&1 | tail -5
git add windows/hearsay/src windows/hearsay/tests docs/audits/type-audit-report.md
git commit -m "feat(hearsay): a seam crossing costs what the peoples are strangers"
```

---

### Task 3: Reproduce and disposition the non-argmin holders

**Files:** a probe or test under `windows/hearsay/tests/`, your choice of home.

Spec §3.5: **the merged walk is not always its own argmin under the seam** — 36
of 13,164 contact holders (0.27%) hold a telling with the same width bits and
the same remembered day but one hop more than an available route. Descent is
clean (0 of 310,215). It took a full route enumerator to see, because both
earlier probes reimplemented a *relaxation* and inherited the behaviour.

- [ ] **Step 1: Reproduce it against the shipped walk**

Establish the count independently. `probe_tiebreak_rules.rs` (`95cbdd33`) has
the enumerator — read it rather than writing a third one.

- [ ] **Step 2: Decide, and record the decision with its evidence**

| what you find | what to do |
|---|---|
| reproduces at ~0.27%, and Task 2's penalty leaves it | report the count under both arms; it is a real defect and belongs in the chronicle, not a fix smuggled into this campaign |
| reproduces, and the penalty changes it | report both counts — a mechanism that alters a known defect is a finding about the mechanism |
| does not reproduce | **stop and report.** Either the enumerator or the earlier probe was wrong, and which one matters more than this task |

**Do not fix it in this campaign** unless the fix is one line and provably
behaviour-preserving elsewhere — and if you believe it is, say why and let the
review decide. A correctness fix bundled into a measurement campaign confounds
the readout, which is the mistake this thread has now avoided three times.

- [ ] **Step 3: Format, gate, commit**

---

### Task 4: The preregistered readout

**Files:** create `windows/hearsay/tests/undertow_readout.rs`.

Model it on `windows/hearsay/tests/parley_readout.rs` — same 40-seed panel,
same `read_world` shape, same `#[ignore = "heavy: …"]` reason, same `claim:`
tag convention. **Read that file first**; do not invent a second harness.

- [ ] **Step 1: Write the battery**

Report, per `Crossing` arm × `Accumulation` rule, over `Contact::Descent` and
`Contact::WithRaidSeam`:

| § | quantity |
|---|---|
| §6.1 H1 | mutually-exclusive cross-people endings, as **counts with both populations named** — never a bare ratio |
| §6.2 H2 | share of cross-people holders keeping a telling that crossed no seam, split by `contact_edges` **tercile** |
| §6.3 | **BOTH LEVELS, SIDE BY SIDE:** share of holders whose held telling changed, against the change in the mutually-exclusive count |

**§6.3 is not optional formatting.** The substrate already produced a change
that rewrote 45.7% of holders and moved the aggregate by 2 events of 124. A
readout printing only the aggregate cannot tell a working mechanism from an
inert one.

**Assert only substrate controls** (spec §6.4): panel built; held claims exist;
no claim reports a rung its own ladder lacks; `AS_SHIPPED` reproduces the
pre-campaign walk; the penalty never *removes* a holder; §3's published counts
re-derive from `parley_readout.rs`'s committed constants. **Every hypothesis is
REPORTED.**

- [ ] **Step 2: Pilot, then full run**

3-seed pilot first; ≤5 s/seed keeps 40 seeds, otherwise report the measured
cost and the projection and propose a panel — **do not silently shrink it.**
Restore 40 before the full run. Capture to a file; never inline `| tail` on an
expensive run.

- [ ] **Step 3: Report each hypothesis against its §6 decision table**

State confirmed / falsified / null **with the number**. A falsified prediction
is a finding. **Do not edit `derive.rs`, `contact.rs` or the spec to rescue
one** — if you are tempted, that temptation is the finding.

- [ ] **Step 4: Format, gate, commit**

---

### Task 5: Close the campaign

- [ ] **Step 1: Chronicle** — `book/src/chronicle/the-undertow.md` + `SUMMARY.md`
  entry, at the project's deliberate altitude. It must carry both probes'
  eliminations (§3.2, §3.3), the dissociation (§3.4), the readout's real numbers
  including anything falsified, and Nathan's G3 reframing from §5.4 — that the
  goal is not an absence of prejudice but a question of whose.
- [ ] **Step 2: Retrospective** — `docs/retrospectives/the-undertow.md` +
  README index line. It must carry: that the campaign's subject was falsified
  **twice** by substrate probes before a spec existed, both times a
  controller-authored premise; that the elegant people-blind answer had already
  been measured in the `frequency` arm and had lost; and that the controller
  over-read decision 0021 and had to be corrected by Nathan.
- [ ] **Step 3: Registry** — flip `KNOW-directed-contact` to
  **`refuted (measured)`** with §3.2's numbers, prose **replaced** not appended.
  Flip whatever this campaign shipped. Leave `KNOW-teller-ladder-at-emit`,
  `KNOW-signed-amplitude`, `KNOW-lectio-difficilior`,
  `KNOW-misattribution-drift` as they are. Five columns, bare pipes escaped
  `\|`, closed-vocabulary status, Idea cell ≤600 chars, **never** append to
  `cli/tests/fixtures/registry-length-waivers.txt`.
- [ ] **Step 4: Book freshness sweep** — decision 0013. The predecessor's close
  audit found three documents stating the old model's limits as present-tense
  gaps and **missed them on the first pass**. Grep the book for chapters
  describing myth transmission, contact, pooling or the seam, and amend in the
  house "**Corrected by**" form. **Never rewrite a chronicle's history.**
- [ ] **Step 5: Regenerate and check drift**

```
make rebaseline
git diff --stat -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

| what moved | what to do |
|---|---|
| `docs/audits/` only | expected on new `pub` items; commit in the same commit |
| `docs/digest/` too | expected when registry rows move; commit it |
| `book/src/gallery/` or `book/src/domesday/` | **STOP and report** — this campaign touches no domain and no census |
| nothing at all | **suspicious.** Verify `rebaseline` ran; an empty diff needs a positive control before it means anything |

- [ ] **Step 6: Gate, commit, and hand back.** The merge is a G6 hard stop the
  controller owns. **Do not run `make sluice`.**

---

## Notes for whoever executes this

- **`make gate-commit` is an allow-list.** Every new test here is invisible to
  it until a stage gate records a duration. Always run yours by name.
- **Absorb main at each task boundary** with
  `make sluice-stage BRANCH=campaign/the-undertow REF=$(git rev-parse HEAD)`.
  It never pushes.
- **Never absorb mid-measurement** — once Task 4's full run starts, finish the
  readout first.
- **`.superpowers/sdd/` is git-ignored and dies with the worktree.** Promote
  findings before teardown.
