# The Cadastre Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Complete `technologies/asimov-1989` from 41 items to its closed
301-item population, so the corpus can compute the demand set it already
claims, and ratify the closure rule that keeps every future corpus from
repeating the truncation.

**Architecture:** The corpus is DATA and `cli/src/technologies.rs` is its
RESOLVER (decision 0011); no task writes resolver logic. Selection is two blind
rules unioned then closed (spec §4). Scoring is by hand, in batches, against a
held-out control. The authoring script is THROWAWAY — its rule and its output
counts are recorded in `provenance`, exactly as The Kiln recorded its fetches
(ledger #13 refused to commit an authoring harness and that precedent binds).

**Tech Stack:** Rust 2024, `cargo nextest`; `python3` + `curl` for the
throwaway fetch; no new dependencies (decision 0004 — the workspace allowlist
is `serde`, `serde_json`, `libm`).

**Spec:** `docs/superpowers/specs/2026-09-12-the-cadastre-design.md`
**Ledger:** `docs/superpowers/ledgers/2026-09-12-the-cadastre.md` — write
rulings there as they occur, never to `.superpowers/sdd/`.

## Global Constraints

- **No `inapplicable`, no `criterion`, no `statistic`** on any item this
  campaign authors (spec §3, §6).
- **`ordered` stays `true`**: items sit in attested-date order and every
  `presupposes` edge points backwards in the file.
- **`disclosure` marks PER-ITEM non-blindness** and is owed by every *chosen*
  item — one with no `absent` anywhere in its `presupposes` closure. **Do not
  key this on roots**; `technologies/CLAUDE.md` refuses that proxy explicitly.
  An *inherited* item carrying a disclosure is equally a finding.
- **The corpus is data.** No task adds a build-time validator, an authoring
  harness, or a generator to the repository.
- **Never `git add -A`** — this worktree may be shared. Stage explicit paths.
- Run `cargo fmt` before any commit touching Rust.

---

### Task 1: Fetch, close, and verify the population

**Files:**
- Throwaway (scratchpad, NOT committed): the fetch/close script and its cache
- Produce: `/tmp/cadastre/population.json` — the verified item set and lattice

**Interfaces:**
- Produces: `population.json`, an array of
  `{slug, title, year:int, field, built_on:[slug], led_to:[slug]}` for exactly
  the closed population. Task 2 consumes it; nothing else does.

- [ ] **Step 1: Fetch the enumeration twice, independently**

```bash
mkdir -p /tmp/cadastre
curl -sL --max-time 30 https://invention.cards/browse/ -o /tmp/cadastre/browse-1.html
sleep 5
curl -sL --max-time 30 https://invention.cards/browse/ -o /tmp/cadastre/browse-2.html
wc -c /tmp/cadastre/browse-1.html /tmp/cadastre/browse-2.html
diff /tmp/cadastre/browse-1.html /tmp/cadastre/browse-2.html && echo "FETCHES AGREE"
```

Record both byte counts and the date. **Branch table — do not predict:**
- Both fetches identical → proceed, record the byte count in `provenance`.
- **Fetches differ → STOP and report.** A corpus frozen off an unstable
  source is not frozen.

- [ ] **Step 2: Parse the enumeration — slug, title, year, field**

The browse page is server-rendered, sectioned by field, with a year span per
item:

```html
<h2>Math</h2>
<ul><li><a href="/abacus/">Abacus</a> <span class="browse-year">500 BCE</span></li>...
```

```python
import re, json
h = open('/tmp/cadastre/browse-1.html').read()
items = {}
for sec in re.finditer(r'<h2>([^<]+)</h2>\s*<ul>(.*?)</ul>', h, re.S):
    field = sec.group(1).strip()
    for m in re.finditer(
        r'href="/([a-z0-9-]+)/">([^<]*)</a>\s*<span class="browse-year">([^<]+)</span>',
        sec.group(2)):
        slug, title, yr = m.group(1), m.group(2), m.group(3)
        n = re.search(r'-?\d+', yr.replace(',', ''))
        assert n, f"no year parsed for {slug}: {yr!r}"
        v = int(n.group())
        items[slug] = {"slug": slug, "title": title, "field": field,
                       "year": -v if 'BCE' in yr else v}
print("parsed:", len(items))
json.dump(items, open('/tmp/cadastre/all-items.json', 'w'))
```

Expected shape, not an expected number: every `<li>` yields a slug, a title and
a parseable year. **Branch table:**
- Count matches The Kiln's 1,484 (after discarding the page's own `browse`
  self-link) → proceed.
- Count differs → record the difference and the new slugs **in `provenance`**,
  then proceed. The catalogue is allowed to grow; what is not allowed is
  failing to say so.

- [ ] **Step 3: Build the seed from the two blind rules**

```python
import json
items = json.load(open('/tmp/cadastre/all-items.json'))
arcs = {}
for name in ('knights', 'republic-of-letters', 'steam-diffusion'):
    # fetch https://invention.cards/story/<name>/ and take every /slug/ href,
    # discarding 'browse'
    arcs[name] = arc_slugs(name)
    print(name, len(arcs[name]))
arc_union = set().union(*arcs.values())
era = {s for s, v in items.items() if v['year'] < 1700}
seed = arc_union | era
print('arc union', len(arc_union), 'era', len(era), 'seed', len(seed))
```

Record all four counts in `provenance`. The three arc counts are a second,
independent confirmation of The Kiln's 16/10/15.

- [ ] **Step 4: Fetch every seed item page and extract its edges**

Item pages carry the relation as a labelled nav:

```html
<nav class="page-related" aria-label="Built on">
  <h2>Built on</h2>
  <ul><li><a href="/mach-number/">Mach number</a></li>...</ul>
</nav>
```

```python
def relations(html):
    out = {}
    for m in re.finditer(
        r'<nav class="page-related" aria-label="([^"]+)">(.*?)</nav>', html, re.S):
        out[m.group(1)] = re.findall(r'href="/([a-z0-9-]+)/"', m.group(2))
    return out   # keys seen in the wild: 'Built on', 'Led to', 'Stories'
```

Sleep at least 0.25 s between requests. Cache to disk so a re-run costs
nothing.

- [ ] **Step 5: Close the seed under `Built on` (BFS to fixpoint)**

```python
from collections import deque
q, seen, graph = deque(seed), set(seed), {}
while q:
    s = q.popleft()
    graph[s] = relations(get(s)).get('Built on', [])
    for t in graph[s]:
        if t not in seen:
            seen.add(t); q.append(t)
print('CLOSED:', len(graph), 'items,', sum(len(v) for v in graph.values()), 'edges')
```

Close on `Built on` **only**. `Led to` is the inverse relation and closing on
it does not terminate (spec §4.2).

- [ ] **Step 6: Gate — acyclicity**

```python
WHITE, GREY, BLACK = 0, 1, 2
col = collections.defaultdict(int); cycles = []
def dfs(u, stack):
    col[u] = GREY; stack.append(u)
    for v in graph.get(u, []):
        if v not in graph: continue
        if col[v] == GREY: cycles.append(stack[stack.index(v):] + [v])
        elif col[v] == WHITE: dfs(v, stack)
    col[u] = BLACK; stack.pop()
for n in list(graph):
    if col[n] == WHITE: dfs(n, [])
print('CYCLES:', len(cycles))
```

**Branch table:**
- 0 cycles → proceed; record the count in `provenance`.
- **Any cycle → STOP and report, naming the cycle.** `parse` rejects a cycle,
  and breaking one is a selection decision that needs a spec amendment and a
  ledger entry. **It is not an implementer's call.**

- [ ] **Step 7: Gate — `Led to` agrees with `Built on`**

Over every pair where both endpoints are in the population, check that
`a led-to b` holds exactly when `b built-on a`. Report both asymmetries
separately:

```python
B = {(a,b) for a,bs in graph.items() for b in bs if b in graph}
L = {(b,a) for a,ls in ledto.items() for b in ls if b in graph and a in graph}
print('built-on edges', len(B), 'inverted led-to', len(L))
print('led-to with no built-on:', len(L - B))
print('built-on with no led-to:', len(B - L))
```

Record all three numbers in `provenance` whatever they are. A disagreement is
**not** a stop condition — `Built on` is authoritative — but an unrecorded one
would launder the source's inconsistency into ours.

- [ ] **Step 8: Emit `population.json` and commit nothing**

```bash
python3 -c "import json;d=json.load(open('/tmp/cadastre/population.json'));print(len(d),'items')"
```

The script and its cache stay in the scratchpad. Per ledger #13, **an
authoring harness is not committed**; its rule and counts travel in
`provenance` instead.

- [ ] **Step 9: Write the ledger entry**

Append to `docs/superpowers/ledgers/2026-09-12-the-cadastre.md` a Task 1
section recording: both fetch byte counts, the parsed item count, the three arc
counts, seed size, closed size, edge count, cycle count, the three `Led to`
agreement numbers, and any branch that fired.

```bash
git add docs/superpowers/ledgers/2026-09-12-the-cadastre.md
git commit -m "docs(the-cadastre): Task 1 — the population is closed and verified"
```

---

### Task 2: Rebuild the corpus at its closed population

**Files:**
- Modify: `technologies/asimov-1989.technology.json`
- Modify: `cli/src/technologies.rs` (the `novelty_baseline` figure only)
- Modify: `cli/tests/suite/technology_corpus.rs` (two count assertions)

**Interfaces:**
- Consumes: `/tmp/cadastre/population.json` from Task 1.
- Produces: a corpus at the closed population where `technologies check
  asimov-1989` exits 0, every new item scored `absent`, and the arc items'
  substance untouched. Task 3 rescores; Task 4 discharges the sibling.

- [ ] **Step 1: Establish the RED — the current corpus cannot express the closure**

```bash
cargo run -p hornvale -- technologies check asimov-1989; echo "rc=$?"
```
Expected: `rc=0`. This is the baseline to protect — the corpus is green while
being wrong, which is the whole defect.

- [ ] **Step 2: Preserve the 41 arc items exactly, then add their real edges**

Every existing item keeps its `id`, `title`, `source` (including its arc
attribution), `verdict`, `anchor`, `statistic`, `criterion` and `contested`.
Only `presupposes`, `note` and `disclosure` may change, and only as Steps 3–4
require.

- [ ] **Step 3: Handle the chosen→inherited flip — 10 items, MEASURED**

Closure restores prerequisites that were dropped, which turns roots into
non-roots. **Ten of the 41 are roots today only because their prerequisites
were dropped, and all ten carry a `disclosure` that closure turns into a
resolver finding:**

```
inv-animal-dom          deferred  dropped: biped
inv-writing             deferred  dropped: stone-tool
inv-cart                absent    dropped: copper
inv-basic-steam-engine  absent    dropped: fire
inv-turnplow            deferred  dropped: plow, steel
inv-coal-mining         deferred  dropped: steel
inv-longbow             absent    dropped: crossbow
inv-arquebus            absent    dropped: artillery, crossbow
inv-hydrostatics        absent    dropped: geometry
inv-latent-heat         absent    dropped: heat-capacity
```

Their disclosures open *"NOT BLIND: a ROOT — no `presupposes`"*, which closure
makes **literally false**, not merely unnecessary. For each, decide by the
resolver's own test rather than by this list:

- Item is now **inherited** (something `absent` in its closure) → **delete the
  `disclosure`** and move whatever it asserted into `note`, rewritten so it no
  longer calls the item a root.
- Item is still **chosen** → keep the disclosure, but correct any sentence
  claiming it has no prerequisites.

This list is a measurement taken on 2026-09-12 and may not be exhaustive after
re-fetch; **let the resolver enumerate the real set** (Step 6), do not trust
these ten to be all of them.

- [ ] **Step 4: Add the 260 new items**

Each new item, in attested-date order, prerequisites before dependents:

```json
{
  "id": "inv-pottery",
  "title": "Pottery",
  "source": "Asimov, *Chronology of Science and Discovery*; invention.cards `/pottery/` (6,000 BCE)",
  "introduces": "pottery",
  "presupposes": ["inv-fire"],
  "verdict": "absent",
  "anchor": ""
}
```

No `disclosure`, no `note`, no `statistic`, no `criterion` at this step —
Task 3 authors verdicts and their justifications. An `absent` here is a
**placeholder that Task 3 must confirm or replace**, and the ledger says so.

- [ ] **Step 5: Rewrite `provenance` and `frozen` — the selection rule lives HERE**

Family law requires the selection rule to be stated in `provenance` and to be
**applicable by someone who has never read `history_bake.rs`**. `provenance`
must carry, in this order:

1. The source attribution (Asimov as catalogue, invention.cards as where it was
   encountered), unchanged in substance from The Kiln's.
2. **The selection rule, stated before the counts**: the three named arcs,
   union everything attested before 1700, closed under `Built on`. Say that
   1700 is a judgement about the setting's intellectual reach and NOT a
   derivation — spec §4.4 — and that `< 1700` includes Newton deliberately, so
   the corpus can report a ceiling rather than assume one.
3. Every count from Task 1: both fetch byte counts and their date, parsed
   items, the three arc counts, seed, closed size, edges, cycles, and the three
   `Led to` agreement numbers.
4. **The completion note**: that this corpus supersedes a 41-item arc-only
   predecessor whose derived demand set under-described its items, naming the
   defect and that the arc items remain individually identifiable.
5. The declared bias, carried forward and re-counted for the new population.

`frozen` must state what is frozen and what the disqualification scope is —
and, unlike The Kiln's, must **not** claim no evaluation code exists, because
it does. Say instead that SELECTION is structural (the rule admits everything
its two clauses reach, so it cannot be tuned) while VERDICTS are non-blind and
carry per-item `disclosure`.

- [ ] **Step 6: Verify edge completeness — success criterion 4**

Closure guarantees every `Built on` target is in the population, so **zero
edges may be dropped**. Check it rather than assume it:

```bash
python3 - <<'EOF'
import json, re
pop = json.load(open('/tmp/cadastre/population.json'))
src = {p['slug']: set(p['built_on']) for p in pop}
corpus = json.load(open('technologies/asimov-1989.technology.json'))
slug = {}
for i in corpus['items']:
    m = re.search(r'invention\.cards `/([a-z0-9-]+)/`', i['source'])
    assert m, f"no slug recoverable from {i['id']}'s source"
    slug[i['id']] = m.group(1)
inv = {v: k for k, v in slug.items()}
missing = []
for i in corpus['items']:
    have = set(i['presupposes'])
    want = {inv[t] for t in src[slug[i['id']]] if t in inv}
    if have != want:
        missing.append((i['id'], sorted(want - have), sorted(have - want)))
print('items whose edges disagree with the source:', len(missing))
for m in missing[:20]: print('  ', m)
EOF
```

Expected: **0**. A non-empty result means either an edge was dropped (the bug
this campaign exists to fix, reintroduced) or an edge was invented. Both are
stop-and-fix, not proceed-and-note.

- [ ] **Step 7: Move the baseline and the count assertions**

`cli/src/technologies.rs`, `novelty_baseline` — 35 is the arc corpus's figure:

```rust
pub fn novelty_baseline(corpus_id: &str) -> Option<usize> {
    match corpus_id {
        "asimov-1989" => Some(<the new absent count>),
        "henrich-2004-extended" => Some(31),
        _ => None,
    }
}
```

`cli/tests/suite/technology_corpus.rs` — both assertions currently read `41`:

```rust
assert_eq!(c.items.len(), 301);
```

Take both numbers from the corpus you built, not from this plan — this plan's
301 is a measurement taken on 2026-09-12 and Task 1 may legitimately produce a
different one.

- [ ] **Step 8: Run the resolver and let it enumerate what is left**

```bash
cargo run -p hornvale -- technologies check asimov-1989 2>&1 | tee /tmp/cadastre/check.txt
grep -c 'is CHOSEN' /tmp/cadastre/check.txt
```
Fix findings until `rc=0`. Every finding names its own repair; follow the
finding, not this plan.

- [ ] **Step 9: Run the suite and commit**

```bash
cargo fmt
cargo nextest run -p hornvale --test suite -E 'test(technology)'
make gate-commit
git add technologies/asimov-1989.technology.json cli/src/technologies.rs \
        cli/tests/suite/technology_corpus.rs
git commit -m "data(technologies): complete asimov-1989 to its closed population (Task 2)"
```

---

### Task 3: Score the new items, against a held-out control

**Files:**
- Modify: `technologies/asimov-1989.technology.json` (verdicts only)

**Interfaces:**
- Consumes: the corpus from Task 2.
- Produces: final verdicts and their anchors/disclosures. Task 4 reads the
  `registry:` citations this creates.

- [ ] **Step 1: Run the control FIRST, blind**

Dispatch a scoring agent over **the 41 arc slugs only**, giving it the item
pages and `book/src/frontier/idea-registry.md` — and **not**
`technologies/asimov-1989.technology.json`. Its brief is Step 2's procedure.
Compare its verdicts to The Kiln's committed 35 `absent` / 6 `deferred`.

**Branch table:**
- Reproduces all 6 `deferred` → record the result, proceed to Step 3.
- Misses 1–2 → record which, proceed, and state the measured recall in
  `provenance` as a declared limitation of the sweep.
- **Misses 3 or more → STOP and report** (spec §12). The method is the
  deliverable's foundation; a broken one invalidates the rest. Do not tune the
  procedure against these 6 — they are the only answer key that exists, and
  fitting to them consumes the control.

- [ ] **Step 2: The per-item scoring procedure**

For each item, in this order:

1. Read its catalogue page: title, attested date, attributed person and place,
   description.
2. Name the capability in Hornvale's own vocabulary — what would a world have
   to model for a people to *hold* this?
3. Search `book/src/frontier/idea-registry.md` for a row planning that
   capability. Search by the capability, not by the item's title: `turnplow` is
   planned by a row about ploughs, `pottery` by `TECH-2`'s pyrotechnology
   ladder. **A keyword match on the title alone was measured at 50% recall
   (ledger #4) — it is a starting point, never the search.**
4. Search `docs/decisions/` for a ratified decision *declining* the capability.
5. Assign:
   - a registry row plans it and is not `shipped` → `deferred`, anchor
     `registry:<row-id>`
   - a ratified in-force decision declines it → `refused`, anchor
     `decision:NNNN`
   - the world demonstrably models it → `present`, anchor `test:` or `path:`
   - otherwise → `absent`, anchor `""`
6. **Never `inapplicable`** (Global Constraints).
7. If the verdict turned on having read the model — and for a chosen item it
   did — write a `disclosure` saying what was known.

- [ ] **Step 3: Score the remaining items in batches**

Batches of at most 50, each a fresh agent with the Step 2 brief. A batch
reports its verdicts and the rows it cited; the controller applies them. Agents
do not edit the corpus.

- [ ] **Step 4: Reconcile and check**

```bash
cargo run -p hornvale -- technologies check asimov-1989; echo "rc=$?"
```
The `absent` baseline from Task 2 Step 5 will need updating to the final count.
`rc=0` before committing.

- [ ] **Step 5: Commit**

```bash
cargo fmt
make gate-commit
git add technologies/asimov-1989.technology.json cli/src/technologies.rs
git commit -m "data(technologies): score the completed population (Task 3)"
```

---

### Task 4: Discharge the cross-corpus obligation

**Files:**
- Modify: `technologies/henrich-2004-extended.technology.json` (`provenance` only)

- [ ] **Step 1: Establish the RED — let the resolver name the rows**

```bash
cargo run -p hornvale -- technologies check henrich-2004-extended 2>&1 \
  | grep -oE 'registry:[A-Za-z0-9-]+' | sort -u | tee /tmp/cadastre/owed.txt
wc -l /tmp/cadastre/owed.txt
```
Expected: one finding per row `asimov-1989` newly cites and henrich never
mentions. If the file is empty, Task 3 cited no new rows and this task is a
no-op — **say so in the ledger rather than inventing work**.

- [ ] **Step 2: Append the class refusal to `provenance`**

One paragraph, naming **every** row in `owed.txt` and the single demand none
discharges: `henrich-2004-extended` scores loss *trajectory*, and an
invention-side reach row cannot discharge a trajectory demand. The check is a
grep-level floor, so every row ID must appear literally.

**Change nothing else in this file.** No item, verdict, anchor, statistic or
criterion moves. That restraint is the whole defence of editing a frozen
corpus.

- [ ] **Step 3: Verify both directions and commit**

```bash
cargo run -p hornvale -- technologies check henrich-2004-extended; echo "rc=$?"
cargo run -p hornvale -- technologies check asimov-1989; echo "rc=$?"
git diff --stat technologies/henrich-2004-extended.technology.json
git add technologies/henrich-2004-extended.technology.json
git commit -m "data(technologies): rule on asimov-1989's new citations (Task 4)"
```
`git diff --stat` is the evidence that only `provenance` moved; read it before
committing.

---

### Task 5: Family law and the decision records

**Files:**
- Modify: `technologies/CLAUDE.md`
- Create: `docs/decisions/NNNN-a-corpus-is-closed-under-its-sources-dependency-relation.md`
- Create: `docs/decisions/NNNN-a-frozen-corpus-may-gain-a-ruling-that-moves-no-verdict.md`

- [ ] **Step 1: Find the next free decision numbers**

```bash
ls docs/decisions/ | sed 's/-.*//' | sort -n | tail -3
```
Take the next two. Grep first — do not assume.

- [ ] **Step 2: Write the closure decision**

The rule, stated to bind every family:

> A corpus drawn from a source that carries its own dependency relation is
> closed under that relation, in the direction that terminates. A corpus that
> cannot close states the truncation and its size in `provenance`.

Context it must carry: `presupposes` may name only in-corpus items, so the
outside edges are destroyed at authoring time and no resolver can detect the
loss; The Kiln documented the resulting under-description; the measured repair
here was 41 → 301.

- [ ] **Step 3: Write the post-freeze-ruling decision**

That a frozen corpus may gain a written ruling which moves no verdict, and may
not gain anything else. The boundary is the re-freeze prohibition, which is
about criteria and bands.

- [ ] **Step 4: Add both rules to `technologies/CLAUDE.md`**

The closure rule, and the no-`inapplicable` rule with its reason (spec §5, §6).

- [ ] **Step 5: Commit**

```bash
make gate-commit
git add technologies/CLAUDE.md docs/decisions/
git commit -m "docs(decisions): a corpus is closed under its source's dependency relation (Task 5)"
```

---

### Task 6: Regenerate, report, and close the book

**Files:**
- Modify: `docs/audits/technology-coverage-asimov-1989.md` (generated)
- Create: `book/src/chronicle/the-cadastre.md`
- Create: `docs/retrospectives/the-cadastre.md`
- Modify: `docs/audits/campaign-reconciliation.tsv`

- [ ] **Step 1: Regenerate and confirm the artifact is tracked**

```bash
make rebaseline
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$' | cut -f1) \
  || echo "DRIFT — regenerate and commit in the same commit"
```

- [ ] **Step 2: Report both series**

The report must state the `absent` **fraction** for the whole corpus and for
the 41-item arc sample separately, and whether the fraction moved (spec §9).
The raw count is not the headline.

- [ ] **Step 3: Chronicle**

It must carry: the defect (a corpus that cannot compute its own demands); the
two blind rules and why one alone was insufficient (`pottery`); the measured
sieve failure and why it disqualified the census; the `TechHorizon` correction
(it dates ruins); and the correction of The Kiln's *"the full transitive
closure… would plainly exceed 80"* — measured at 77 for the arcs' own closure.
That audit is merged and append-only, so the correction lives here and in the
new `provenance`, never by editing it.

- [ ] **Step 4: Retrospective**

Process lessons, not product. At minimum: reading a constant's definition
without reading a single CALLER produced two wrong arguments that survived
three gates (ledger #6); and a de-risking probe against existing ground truth
killed a design that had already been ratified (ledger #4) — cheaply, and only
because the ground truth already existed.

- [ ] **Step 5: Freshness sweep**

```bash
grep -rln 'asimov-1989\|technolog' book/src/ docs/ --include='*.md' | head -30
```
Re-score any Confidence Gradient bet this campaign moved
(`book/src/open-questions.md`, decision 0030).

- [ ] **Step 6: Update the reconciliation row and commit**

Flip `spec-2026-09-12-the-cadastre-design` from `active` to `shipped` with its
evidence, and add rows for the plan, chronicle and retrospective.

```bash
make gate-commit
git add book/src/chronicle/the-cadastre.md docs/retrospectives/the-cadastre.md \
        docs/audits/ docs/superpowers/
git commit -m "docs(the-cadastre): chronicle, retrospective, freshness sweep (Task 6)"
```

---

## Merge

`make sluice BRANCH=campaign/the-cadastre REF=<full-sha>` — **not prose-only**
(it touches Rust), so it pays the full phase ladder. G6 is a hard stop: present
the post-G3 ledger digest before submitting.
