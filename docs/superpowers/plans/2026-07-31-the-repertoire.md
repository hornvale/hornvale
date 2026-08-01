# The Repertoire Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking. Dispatching any subagent into this repo additionally requires the `dispatching-hornvale-subagents` skill.

**Goal:** Ship `hornvale tropes check|report` — a build-time probe that scores a
frozen, provenance-stamped corpus of dramatic situations against the concept
registry, and ranks the missing capability bundles by how many situations each
would unlock.

**Architecture:** Corpus is data (`tropes/polti.trope.json`), checker is code
(`cli/src/tropes.rs`), sibling of `cli/src/concepts.rs`. Resolution is set
membership against the three registry namespaces the registry already exposes —
`predicates()`, `phenomenon_kinds()`, `concepts()`. The committed report is the
ratchet baseline, compared live, `REBASELINE=1` to accept. No seed, no world
save, no census, no new dependency.

**Tech Stack:** Rust 2024, `serde`/`serde_json` only (already in
`cli/Cargo.toml:29`), `cargo nextest`, `make gate`.

**Spec:** [`2026-07-31-the-repertoire-design.md`](../specs/2026-07-31-the-repertoire-design.md)

## Global Constraints

- **Dependency allowlist:** `serde`, `serde_json`, `libm` only (decision 0004).
  Adding any crate fails `cli/tests/architecture.rs`.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only, enforced by
  `clippy.toml` `disallowed-types`. Determinism depends on it: report output
  must be byte-stable across runs.
- **No wall-clock time.** No `SystemTime`, no dates in generated output.
- **`#![warn(missing_docs)]`** — every `pub` item, field and variant needs a
  one-line doc comment.
- **Every primitive at a `pub` boundary carries a `type-audit:` tag**
  (`bare-ok(<class>)` / `waiver(<reason>)`), or
  `cargo run --manifest-path tools/type-audit/Cargo.toml -- check` fails.
- **`cargo fmt` as the final step before every commit.** Skipped fmt is the
  most common review finding in this repo.
- **Build-state only.** Nothing in this campaign touches a seed, a stream
  label, a world save, an epoch, or a census.
- **Corpus commit lands before the first report commit** (spec §5). The git
  history is the preregistration evidence; do not squash Tasks 1 and 4.

## File Structure

| File | Responsibility |
|---|---|
| `tropes/polti.trope.json` | Create. The frozen corpus: provenance, bundle table, 36 situations. Data only. |
| `cli/src/tropes.rs` | Create. Parse the corpus, resolve against a registry, render the report. All logic. |
| `cli/src/main.rs` | Modify. One dispatch arm + one `cmd_tropes`, mirroring `cmd_concepts` at `:796`. |
| `cli/tests/trope_coverage.rs` | Create. The ratchet: live report vs the committed artifact, `REBASELINE=1` accepts. |
| `docs/audits/trope-coverage.md` | Create (generated). The committed report. |
| `scripts/regenerate-artifacts.sh` | Modify. One line beside the type-audit report at `:352`. |

---

### Task 1: Freeze the corpus and kill it early if the lattice is flat

Spec §5 P3 is the cheapest possible kill and it runs **before any code exists**.
This task is pure authorship plus one measurement.

**Files:**
- Create: `tropes/polti.trope.json`

**Interfaces:**
- Consumes: nothing.
- Produces: the corpus file, whose exact schema Task 2 parses —
  `{ corpus: String, provenance: String, bundles: BTreeMap<String, Vec<String>>,
  situations: Vec<{ id, name, actants: BTreeMap<String,String>,
  requires: Vec<String>, excluded_by: Vec<String> }> }`. Token strings are
  namespaced `predicate:<name>`, `phenomenon:<name>`, `concept:<name>`, or
  `bundle:<name>`.

- [ ] **Step 1: Dump the live vocabulary to read while decomposing**

```bash
cd "$(git rev-parse --show-toplevel)"
cargo run -q -p hornvale -- concepts > /tmp/hv-vocab.md
grep -c '^| `' /tmp/hv-vocab.md   # sanity: ~319 tokens across three sections
```

- [ ] **Step 2: Write the corpus file**

All 36 Polti situations. Every `requires` entry is either `bundle:<name>` or a
namespaced token that **exists in `/tmp/hv-vocab.md`**. A capability the world
lacks is still written as a `bundle:` whose token list contains the proposed
(non-existent) predicate name — that is how `missing` is reached. Do **not**
write prose requirements; if you cannot state a requirement as tokens, decompose
it further (spec D4).

```json
{
  "corpus": "polti-1895",
  "provenance": "Georges Polti, Les 36 situations dramatiques (1895). French dramaturgical taxonomy of European theatre. An instrument with known bias, not a standard: coverage measures reach against this catalogue only.",
  "frozen": "before first measurement, The Repertoire",
  "bundles": {
    "status-succession": ["predicate:holds-office", "predicate:succeeded-by"],
    "deception": ["predicate:asserted-falsely"],
    "lethal-violence-as-norm-violation": ["predicate:killed", "concept:murder"]
  },
  "situations": [
    {
      "id": "polti-30-ambition",
      "name": "Ambition",
      "actants": { "subject": "the ambitious one", "opponent": "the thwarter" },
      "requires": ["bundle:status-succession"],
      "excluded_by": []
    }
  ]
}
```

- [ ] **Step 3: Measure P3 — the corpus-validity kill**

```bash
python3 - <<'PY'
import json,collections
d=json.load(open('tropes/polti.trope.json'))
fan=collections.Counter()
for s in d['situations']:
    for r in s['requires']:
        if r.startswith('bundle:'): fan[r]+=1
n=len(d['situations']); med=sorted(fan.values())[len(fan)//2] if fan else 0
print(f"situations={n} bundles={len(fan)} median_fan_in={med}")
print("P3 PASS — lattice has depth" if med>=3 else "P3 FAIL — lattice is flat, STOP")
PY
```

Expected: `situations=36`, `median_fan_in >= 3`.

**If P3 fails, stop and report.** A flat lattice means no leverage signal and
the instrument is void — that is a legitimate campaign outcome (spec §5), not a
reason to re-cut the corpus until it passes. Re-cutting after seeing the number
is exactly the unblinding failure decision 0016 forbids.

- [ ] **Step 4: Commit the corpus alone**

```bash
cargo fmt
git add tropes/polti.trope.json
git commit -m "feat(the-repertoire): freeze the Polti corpus before measuring

36 situations, provenance-stamped. P3 measured at freeze: median bundle fan-in
<N>, so the lattice has depth and the leverage ranking is meaningful.

Committed alone and before any checker exists, so git history is the
preregistration evidence (spec §5, decision 0016)."
```

---

### Task 2: Parse the corpus and resolve one situation

**Files:**
- Create: `cli/src/tropes.rs`
- Modify: `cli/src/main.rs` (add `mod tropes;` beside the other `mod` lines)

**Interfaces:**
- Consumes: Task 1's JSON schema; `hornvale_kernel::ConceptRegistry` with
  `predicates() -> impl Iterator<Item = &PredicateDef>` (field `.name: String`),
  `phenomenon_kinds() -> impl Iterator<Item = (&str, &str)>`, and
  `concepts() -> impl Iterator<Item = &ConceptDef>` (field `.name: String`).
- Produces: `pub struct Corpus`, `pub fn load(json: &str) -> Result<Corpus, String>`,
  `pub enum Outcome { Stageable, Blocked(Vec<String>), Inapplicable(String) }`,
  `pub fn resolve(c: &Corpus, r: &ConceptRegistry) -> BTreeMap<String, Outcome>`
  keyed by situation `id`. Task 3 renders these; Task 4 tests them.

- [ ] **Step 1: Write the failing test**

Append to `cli/src/tropes.rs`:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    /// A requirement naming a token the registry does not hold resolves
    /// `Blocked`, never silently satisfied — the default-deny posture.
    #[test]
    fn an_unknown_token_blocks_its_situation() {
        let json = r#"{
          "corpus":"t","provenance":"t","frozen":"t",
          "bundles":{"b":["predicate:no-such-predicate"]},
          "situations":[{"id":"s1","name":"S","actants":{},
                         "requires":["bundle:b"],"excluded_by":[]}]
        }"#;
        let corpus = load(json).expect("corpus parses");
        let registry = hornvale_kernel::ConceptRegistry::default();
        let out = resolve(&corpus, &registry);
        match out.get("s1").expect("s1 resolved") {
            Outcome::Blocked(missing) => {
                assert_eq!(missing, &vec!["predicate:no-such-predicate".to_string()]);
            }
            other => panic!("expected Blocked, got {other:?}"),
        }
    }
}
```

- [ ] **Step 2: Run it and confirm it fails**

Run: `cargo test -p hornvale --bin hornvale tropes::tests::an_unknown_token_blocks_its_situation`
Expected: FAIL — `load`, `resolve`, `Outcome` do not exist.

- [ ] **Step 3: Write the minimal implementation**

Top of `cli/src/tropes.rs`:

```rust
//! The Repertoire: score a frozen corpus of dramatic situations against the
//! concept registry. Build-state only — no seed, no world save, no census.

use hornvale_kernel::ConceptRegistry;
use serde::Deserialize;
use std::collections::{BTreeMap, BTreeSet};

/// One situation as authored in the corpus.
/// type-audit: bare-ok(identifier-text: id), bare-ok(prose: name)
#[derive(Debug, Deserialize)]
pub struct Situation {
    /// Stable corpus-local identifier.
    pub id: String,
    /// Human-readable situation name.
    pub name: String,
    /// Greimas actant role → the role's description in this situation.
    pub actants: BTreeMap<String, String>,
    /// Namespaced tokens and `bundle:` references this situation needs.
    pub requires: Vec<String>,
    /// Preconditions whose absence makes this situation inapplicable.
    pub excluded_by: Vec<String>,
}

/// A frozen, provenance-stamped corpus.
/// type-audit: bare-ok(identifier-text: corpus), bare-ok(prose: provenance), bare-ok(prose: frozen)
#[derive(Debug, Deserialize)]
pub struct Corpus {
    /// Corpus identifier, e.g. `polti-1895`.
    pub corpus: String,
    /// Where this catalogue comes from and what bias it carries.
    pub provenance: String,
    /// Note recording that the freeze preceded measurement.
    pub frozen: String,
    /// Bundle name → the tokens it expands to.
    pub bundles: BTreeMap<String, Vec<String>>,
    /// The situations themselves.
    pub situations: Vec<Situation>,
}

/// How one situation resolved.
#[derive(Debug, PartialEq, Eq)]
pub enum Outcome {
    /// Every requirement satisfied.
    Stageable,
    /// One or more tokens absent, listed in corpus order.
    Blocked(Vec<String>),
    /// The world deliberately lacks a precondition; different, not deficient.
    Inapplicable(String),
}

/// Parse a corpus from JSON.
/// type-audit: bare-ok(artifact: json), bare-ok(prose: return)
pub fn load(json: &str) -> Result<Corpus, String> {
    serde_json::from_str(json).map_err(|e| format!("corpus parse: {e}"))
}

/// Every token the registry holds, namespaced.
fn registry_tokens(r: &ConceptRegistry) -> BTreeSet<String> {
    let mut t = BTreeSet::new();
    for p in r.predicates() {
        t.insert(format!("predicate:{}", p.name));
    }
    for (kind, _doc) in r.phenomenon_kinds() {
        t.insert(format!("phenomenon:{kind}"));
    }
    for c in r.concepts() {
        t.insert(format!("concept:{}", c.name));
    }
    t
}

/// Expand a requirement into concrete tokens, following one `bundle:` level.
///
/// A dangling `bundle:` reference expands to the reference itself, which no
/// registry token can ever match, so a typo blocks its situation. Returning
/// an empty list instead would make a situation whose requirements are all
/// dangling resolve `Stageable` — the exact inversion of spec D4's
/// default-deny posture.
fn expand(corpus: &Corpus, req: &str) -> Vec<String> {
    match req.strip_prefix("bundle:") {
        Some(b) => match corpus.bundles.get(b) {
            Some(tokens) => tokens.clone(),
            None => vec![req.to_string()],
        },
        None => vec![req.to_string()],
    }
}

/// Resolve every situation against a registry. Keyed by situation `id`.
pub fn resolve(corpus: &Corpus, registry: &ConceptRegistry) -> BTreeMap<String, Outcome> {
    let held = registry_tokens(registry);
    let mut out = BTreeMap::new();
    for s in &corpus.situations {
        if let Some(reason) = s.excluded_by.first() {
            out.insert(s.id.clone(), Outcome::Inapplicable(reason.clone()));
            continue;
        }
        // Deduplicated, in corpus order: two bundles may name the same
        // token, and `Blocked` is rendered verbatim into the committed
        // artifact, where a repeat would misstate the count.
        let mut missing: Vec<String> = Vec::new();
        for t in s.requires.iter().flat_map(|r| expand(corpus, r)) {
            if !held.contains(&t) && !missing.contains(&t) {
                missing.push(t);
            }
        }
        out.insert(
            s.id.clone(),
            if missing.is_empty() {
                Outcome::Stageable
            } else {
                Outcome::Blocked(missing)
            },
        );
    }
    out
}
```

Add `mod tropes;` to `cli/src/main.rs` beside the existing `mod concepts;` line.

- [ ] **Step 4: Run the test and confirm it passes**

Run: `cargo test -p hornvale --bin hornvale tropes::tests::an_unknown_token_blocks_its_situation`
Expected: PASS

- [ ] **Step 5: Add the three remaining resolution tests**

```rust
    /// A registered token satisfies, so the situation is stageable.
    #[test]
    fn a_registered_token_makes_a_situation_stageable() {
        let json = r#"{
          "corpus":"t","provenance":"t","frozen":"t","bundles":{},
          "situations":[{"id":"s1","name":"S","actants":{},
                         "requires":["predicate:known"],"excluded_by":[]}]
        }"#;
        let corpus = load(json).expect("corpus parses");
        let mut registry = hornvale_kernel::ConceptRegistry::default();
        registry
            .register_predicate("known", false, "a predicate for the test")
            .expect("registers");
        assert_eq!(resolve(&corpus, &registry).get("s1"), Some(&Outcome::Stageable));
    }

    /// `excluded_by` short-circuits: the world lacks the precondition, so the
    /// situation is inapplicable rather than blocked.
    #[test]
    fn an_excluded_situation_is_inapplicable_not_blocked() {
        let json = r#"{
          "corpus":"t","provenance":"t","frozen":"t","bundles":{},
          "situations":[{"id":"s1","name":"S","actants":{},
                         "requires":["predicate:absent"],
                         "excluded_by":["this world has no marriage"]}]
        }"#;
        let corpus = load(json).expect("corpus parses");
        let registry = hornvale_kernel::ConceptRegistry::default();
        assert_eq!(
            resolve(&corpus, &registry).get("s1"),
            Some(&Outcome::Inapplicable("this world has no marriage".to_string()))
        );
    }

    /// Resolution is deterministic: same inputs, identical output ordering.
    #[test]
    fn resolution_is_order_stable() {
        let json = r#"{
          "corpus":"t","provenance":"t","frozen":"t",
          "bundles":{"b":["predicate:x","predicate:y"]},
          "situations":[{"id":"s2","name":"B","actants":{},"requires":["bundle:b"],"excluded_by":[]},
                        {"id":"s1","name":"A","actants":{},"requires":["bundle:b"],"excluded_by":[]}]
        }"#;
        let corpus = load(json).expect("corpus parses");
        let registry = hornvale_kernel::ConceptRegistry::default();
        let a = resolve(&corpus, &registry);
        let b = resolve(&corpus, &registry);
        assert_eq!(format!("{a:?}"), format!("{b:?}"));
        assert_eq!(a.keys().collect::<Vec<_>>(), vec!["s1", "s2"]);
    }
```

- [ ] **Step 6: Run all four and confirm they pass**

Run: `cargo test -p hornvale --bin hornvale tropes::`
Expected: 4 passed.

- [ ] **Step 7: Commit**

```bash
cargo fmt
cargo clippy -p hornvale --all-targets -- -D warnings
git add cli/src/tropes.rs cli/src/main.rs
git commit -m "feat(the-repertoire): parse the corpus and resolve situations

Default-deny: an unregistered token blocks its situation. \`excluded_by\`
short-circuits to Inapplicable so a world that deliberately lacks a
precondition reads as different, not deficient (spec D4)."
```

---

### Task 3: Render the report and wire the subcommand

**Files:**
- Modify: `cli/src/tropes.rs` (add `render`)
- Modify: `cli/src/main.rs` (dispatch arm + `cmd_tropes`)

**Interfaces:**
- Consumes: `Corpus`, `Outcome`, `resolve` from Task 2.
- Produces: `pub fn render(c: &Corpus, out: &BTreeMap<String, Outcome>, registry: &ConceptRegistry) -> String`,
  emitting the four sections of spec §4 L2. Task 4 pins this output byte-for-byte.

- [ ] **Step 1: Write the failing test**

```rust
    /// The report leads with provenance and carries all four sections, so a
    /// reader cannot mistake the number for a verdict on the world.
    #[test]
    fn the_report_states_provenance_and_all_four_sections() {
        let json = r#"{
          "corpus":"t","provenance":"a catalogue with known bias","frozen":"t",
          "bundles":{},
          "situations":[{"id":"s1","name":"S","actants":{},
                         "requires":["predicate:absent"],"excluded_by":[]}]
        }"#;
        let corpus = load(json).expect("corpus parses");
        let registry = hornvale_kernel::ConceptRegistry::default();
        let out = resolve(&corpus, &registry);
        let text = render(&corpus, &out, &registry);
        assert!(text.contains("a catalogue with known bias"));
        for section in ["## Provenance", "## Demand", "## Leverage", "## Supply"] {
            assert!(text.contains(section), "missing {section}");
        }
        assert!(text.contains("GENERATED FILE"));
    }
```

- [ ] **Step 2: Run it and confirm it fails**

Run: `cargo test -p hornvale --bin hornvale tropes::tests::the_report_states_provenance_and_all_four_sections`
Expected: FAIL — `render` not found.

- [ ] **Step 3: Implement `render`**

```rust
/// Render the coverage report. Four sections, provenance first (spec §4 L2).
/// type-audit: bare-ok(prose: return)
pub fn render(
    corpus: &Corpus,
    out: &BTreeMap<String, Outcome>,
    registry: &ConceptRegistry,
) -> String {
    let mut s = String::new();
    s.push_str("<!-- GENERATED FILE — do not edit. Regenerate with `hornvale tropes report`. -->\n\n");
    s.push_str("# Trope coverage\n\n## Provenance\n\n");
    s.push_str(&format!("- **Corpus:** `{}`\n", corpus.corpus));
    s.push_str(&format!("- **Source:** {}\n", corpus.provenance));
    s.push_str(&format!("- **Frozen:** {}\n", corpus.frozen));
    s.push_str(
        "\nThis measures reach against *that* catalogue. It is not a verdict on the\nworld, and it scores **representability only** — whether an agent could plan\nor recognise a situation is not measured here.\n\n",
    );

    let stageable = out.values().filter(|o| **o == Outcome::Stageable).count();
    let inapplicable = out
        .values()
        .filter(|o| matches!(o, Outcome::Inapplicable(_)))
        .count();
    s.push_str("## Demand\n\n");
    s.push_str(&format!(
        "Stageable {stageable} of {} ({inapplicable} inapplicable).\n\n| Situation | Outcome |\n|---|---|\n",
        out.len()
    ));
    let names: BTreeMap<&str, &str> = corpus
        .situations
        .iter()
        .map(|x| (x.id.as_str(), x.name.as_str()))
        .collect();
    for (id, o) in out {
        let cell = match o {
            Outcome::Stageable => "stageable".to_string(),
            Outcome::Inapplicable(r) => format!("inapplicable — {r}"),
            Outcome::Blocked(m) => format!("blocked — missing `{}`", m.join("`, `")),
        };
        s.push_str(&format!(
            "| {} ({id}) | {cell} |\n",
            names.get(id.as_str()).copied().unwrap_or("?")
        ));
    }

    let mut fan: BTreeMap<String, Vec<String>> = BTreeMap::new();
    for st in &corpus.situations {
        if let Some(Outcome::Blocked(_)) = out.get(&st.id) {
            for r in st.requires.iter().filter(|r| r.starts_with("bundle:")) {
                fan.entry(r.clone()).or_default().push(st.id.clone());
            }
        }
    }
    let mut ranked: Vec<_> = fan.into_iter().collect();
    ranked.sort_by(|a, b| b.1.len().cmp(&a.1.len()).then(a.0.cmp(&b.0)));
    s.push_str("\n## Leverage\n\nMissing bundles by fan-in — the backlog ordering.\n\n| Bundle | Unlocks | Situations |\n|---|---|---|\n");
    for (bundle, sits) in &ranked {
        s.push_str(&format!(
            "| `{bundle}` | {} | {} |\n",
            sits.len(),
            sits.join(", ")
        ));
    }

    let required: BTreeSet<String> = corpus
        .situations
        .iter()
        .flat_map(|st| st.requires.iter().flat_map(|r| expand(corpus, r)))
        .collect();
    let orphans: Vec<String> = registry_tokens(registry)
        .into_iter()
        .filter(|t| !required.contains(t))
        .collect();
    s.push_str(&format!(
        "\n## Supply\n\n{} registered tokens no situation in this corpus requires.\nA rising demand score with a rising supply count means capability is being\nregistered that nothing uses (spec D5).\n\n",
        orphans.len()
    ));
    for t in &orphans {
        s.push_str(&format!("- `{t}`\n"));
    }
    s
}
```

- [ ] **Step 4: Run the test and confirm it passes**

Run: `cargo test -p hornvale --bin hornvale tropes::`
Expected: 5 passed.

- [ ] **Step 5: Wire the subcommand and remove the dead-code allow**

Task 2 added `#![allow(dead_code)]` at the top of `cli/src/tropes.rs` because
`cli/` is binary-only and the new `pub` surface was unused until now. Wiring the
command is what makes it used: **delete that line** and confirm clippy stays
clean. If anything is still unused after wiring, it is genuinely dead — remove
the item rather than re-adding the allow.


In `cli/src/main.rs`, add the dispatch arm immediately after the `concepts` arm
at line ~135:

```rust
        Some("tropes") => cmd_tropes(&args),
```

And the command function, modelled on `cmd_concepts` at `:796`:

```rust
/// The Repertoire: score the frozen corpus against the live registry.
/// Seed 0 as in `cmd_concepts` — the registry is identical for any seed
/// because every predicate registers up front; this exercises the fuller
/// pipeline as a smoke test.
fn cmd_tropes(args: &[String]) -> Result<(), String> {
    let path = flag_value(args, "--corpus").unwrap_or("tropes/polti.trope.json");
    let json = std::fs::read_to_string(path).map_err(|e| format!("{path}: {e}"))?;
    let corpus = tropes::load(&json)?;
    let world = world_builder::build_world(
        Seed(0),
        &SkyPins::default(),
        world_builder::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &world_builder::SettlementPins::default(),
    )
    .map_err(|e| e.to_string())?;
    let outcomes = tropes::resolve(&corpus, &world.registry);
    // Mode is positional; a leading flag means no mode was given.
    let mode = args.get(1).map(String::as_str).filter(|m| !m.starts_with("--"));
    match mode {
        Some("report") | None => {
            print!("{}", tropes::render(&corpus, &outcomes, &world.registry));
            Ok(())
        }
        Some("check") => {
            let live = tropes::render(&corpus, &outcomes, &world.registry);
            let committed = std::fs::read_to_string("docs/audits/trope-coverage.md")
                .map_err(|e| format!("docs/audits/trope-coverage.md: {e}"))?;
            if live == committed {
                Ok(())
            } else {
                Err("trope coverage drifted; run `make rebaseline` and review the diff".into())
            }
        }
        Some(other) => Err(format!("tropes: unknown mode '{other}' (report|check)")),
    }
}
```

Add `tropes` to the usage string beside `concepts`.

- [ ] **Step 6: Generate the artifact and eyeball it**

```bash
mkdir -p docs/audits
cargo run -q -p hornvale -- tropes report > docs/audits/trope-coverage.md
head -30 docs/audits/trope-coverage.md
grep -c '^| ' docs/audits/trope-coverage.md
```

Expected: provenance block first; a Demand table with 36 rows.

- [ ] **Step 7: Verify determinism**

```bash
cargo run -q -p hornvale -- tropes report > /tmp/a.md
cargo run -q -p hornvale -- tropes report > /tmp/b.md
diff /tmp/a.md /tmp/b.md && echo "byte-identical across runs"
```

Expected: no diff.

- [ ] **Step 8: Commit**

```bash
cargo fmt
cargo clippy -p hornvale --all-targets -- -D warnings
git add cli/src/tropes.rs cli/src/main.rs docs/audits/trope-coverage.md
git commit -m "feat(the-repertoire): render the coverage report, wire \`hornvale tropes\`

Four sections, provenance first. Demand and supply in ONE artifact because the
coverage number is Goodhart-gameable by registering predicates nothing uses,
and the supply scan is what catches that (spec D5)."
```

---

### Task 4: The ratchet, the gate, and regeneration

**Files:**
- Create: `cli/tests/trope_coverage.rs`
- Modify: `scripts/regenerate-artifacts.sh`

**Interfaces:**
- Consumes: `hornvale::tropes` is a binary-crate module, so the integration
  test shells out to the built binary rather than importing it — the same
  constraint every `cli/tests/*.rs` works under.
- Produces: nothing downstream.

- [ ] **Step 1: Write the failing ratchet test**

```rust
//! The Repertoire's ratchet. `docs/audits/trope-coverage.md` is a committed
//! artifact; this fails when the live report diverges from it. A per-situation
//! regression — a situation that was stageable becoming blocked — is the case
//! this exists to catch, and it means a predicate the corpus depends on was
//! removed. Regenerate deliberately with `make rebaseline` and review the diff.

use std::process::Command;

#[test]
fn committed_trope_coverage_matches_the_live_report() {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("workspace root")
        .to_path_buf();
    let out = Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args(["tropes", "report"])
        .current_dir(&root)
        .output()
        .expect("runs the binary");
    assert!(out.status.success(), "tropes report failed: {out:?}");
    let live = String::from_utf8(out.stdout).expect("utf-8");
    let path = root.join("docs/audits/trope-coverage.md");
    if std::env::var("REBASELINE").is_ok() {
        std::fs::write(&path, &live).expect("rebaselines");
        return;
    }
    let committed = std::fs::read_to_string(&path).expect("committed report exists");
    assert_eq!(
        live, committed,
        "trope coverage drifted. If deliberate: REBASELINE=1 cargo test -p hornvale \
         --test trope_coverage, then review the diff and say why in the chronicle."
    );
}
```

- [ ] **Step 2: Run it and confirm it passes against the committed artifact**

Run: `cargo test -p hornvale --test trope_coverage`
Expected: PASS (Task 3 committed a matching artifact).

- [ ] **Step 3: Disarm it to prove it discriminates**

A check only ever seen to pass is not known to be a check.

```bash
printf '\n- `predicate:deliberately-bogus`\n' >> docs/audits/trope-coverage.md
cargo test -p hornvale --test trope_coverage   # MUST fail
git checkout docs/audits/trope-coverage.md
cargo test -p hornvale --test trope_coverage   # green again
```

Expected: red, then green. Record both in the commit message.

- [ ] **Step 4: Add regeneration**

In `scripts/regenerate-artifacts.sh`, immediately after the type-audit line
(~`:352`):

```bash
run -p hornvale -- tropes report > docs/audits/trope-coverage.md
```

- [ ] **Step 5: Regenerate the artifacts and commit the drift**

```bash
make rebaseline
git status --short docs/audits/
```

Two files under `docs/audits/` legitimately change on this branch and must be
committed here:

- `docs/audits/trope-coverage.md` — the new artifact, first written by Step 4's
  regeneration line.
- `docs/audits/type-audit-report.md` — **stale since Task 2.** Every `pub`
  item added to `cli/src/tropes.rs` moves this report's counts, and `make gate`
  runs type-audit `check`, not the drift diff, so no gate on this branch has
  caught it. CLAUDE.md names omitting `docs/audits/` as a common miss.

Then prove idempotence — a second regeneration must change nothing:

```bash
make rebaseline
git diff --exit-code docs/audits/ && echo "clean — regeneration is idempotent"
```

- [ ] **Step 6: Run the full gate**

Run: `make gate`
Expected: green. This is the first whole-workspace run; budget ~15 min and do
not run it concurrently with another gate on this box.

- [ ] **Step 7: Commit**

```bash
cargo fmt
git add cli/tests/trope_coverage.rs scripts/regenerate-artifacts.sh docs/audits/
git commit -m "test(the-repertoire): ratchet the coverage artifact, wire regeneration

Disarmed to prove it discriminates: appending a bogus supply line reddens the
test, reverting greens it. REBASELINE=1 accepts a deliberate change."
```

---

### Task 5: Score the predictions and close the campaign

**Files:**
- Create: `book/src/chronicle/the-repertoire.md`
- Create: `docs/retrospectives/the-repertoire.md`
- Modify: `book/src/SUMMARY.md` (chronicle entry)
- Modify: `book/src/frontier/idea-registry.md` (status flips)

**Interfaces:**
- Consumes: the committed report from Task 3.
- Produces: nothing downstream.

- [ ] **Step 1: Score P1, P2 and P4 from the artifact**

```bash
grep -c 'stageable |' docs/audits/trope-coverage.md      # P1: expect < 6 of 36
grep -c 'inapplicable' docs/audits/trope-coverage.md     # P4: expect >= 4
sed -n '/## Leverage/,/## Supply/p' docs/audits/trope-coverage.md | head -8
                                                          # P2: top three bundles
```

- [ ] **Step 2: Write the chronicle entry**

State each of P1–P4 and whether it held. **A falsified prediction is the
headline, not an embarrassment** — several campaigns here ship the null. Do not
retune the corpus to rescue one; if you were tempted, say so.

- [ ] **Step 3: Write the retrospective**

Process lessons only; the product story is the chronicle (decision 0020).

- [ ] **Step 4: Flip the registry rows**

`NARR-trope-as-probe`, `TOOL-trope-grounding`, `TOOL-trope-prereg` and
`NARR-mechanism-leverage` are already `spec'd`; repoint **Where** at the
chronicle and flip to `shipped`. `NARR-three-columns` stays `raw` — only one
column shipped.

- [ ] **Step 5: Book freshness sweep**

Re-score the Confidence Gradient only if this moved a bet in
`book/src/open-questions.md` (decision 0030). It likely did not; say so
explicitly rather than silently skipping.

- [ ] **Step 6: Verify and commit**

```bash
cargo test -p hornvale --test docs_consistency
mdbook build book
cargo fmt
git add -A book/ docs/retrospectives/
git commit -m "docs(the-repertoire): chronicle, retrospective, registry flips"
```

- [ ] **Step 7: STOP — G6 is a hard stop**

Present the post-G3 ledger digest to Nathan. The fast-forward, the push, and
the worktree teardown are his calls, under `closing-a-campaign`. **No census**
— nothing in this campaign touches world generation.

---

## Self-Review

**Spec coverage.** §3 D1 → Task 1. D2 → Tasks 1–2. D3 (bundles) → Task 1 schema,
Task 2 `expand`, Task 3 leverage ranking. D4 (verdicts, default-deny) → Task 2
Steps 1–6. D5 (one artifact) → Task 3 Step 3. D6 (`cli/`) → Task 3 Step 5. D7
(ratchet) → Task 4. §4 L0/L1/L2 → Tasks 1/2–3/3. §5 P1–P4 → Task 1 Step 3 (P3)
and Task 5 Step 1 (P1, P2, P4). §7 verification: grounding → Task 2 Step 1;
determinism → Task 3 Step 7; ratchet-discriminates → Task 4 Step 3; artifact
freshness → Task 4 Step 5; preregistration → Task 1 Step 4 committing alone;
corpus validity → Task 1 Step 3; no world-state → no seed or ledger write
appears in any task. §8 DoD → Task 5.

**Gap found and closed:** §7's "no world-state reached / layering unchanged" had
no explicit task. It is covered by `make gate` in Task 4 Step 6, which runs
`cli/tests/architecture.rs`; noted here so a reviewer can see it was considered
rather than missed.

**Type consistency.** `load`/`resolve`/`render`/`expand`/`registry_tokens`,
`Corpus`/`Situation`/`Outcome` are named identically in Tasks 2 and 3.
A `Verdict` per-requirement enum was drafted and **cut in pre-flight**: nothing
consumes it, and an unused `pub` enum is speculative generality the review
rubric and this repo's own "avoid premature abstractions" both reject. Add it
when a column needs it. Registry methods match the live
API (`predicates()` → `.name`, `phenomenon_kinds()` → `(&str, &str)`,
`concepts()` → `.name`), verified against `kernel/src/registry.rs:175-249`.
