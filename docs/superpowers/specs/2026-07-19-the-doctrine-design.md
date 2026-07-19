# The Doctrine (C6) — Design

**Date:** 2026-07-19
**Status:** Draft — awaiting G3 review (campaign-autopilot hard stop)
**Campaign:** C6 of the self-writing-book program
(metaplan: [program metaplan](2026-07-17-the-self-writing-book-program-metaplan-design.md) §3 C6)
**Theory:** registry row LANG-39 (doctrinal/folk conflict as generative
content), gated by SOC-1's social structure. Builds on C4's filter stack
and C5's causal filter — both shipped.

---

## 1. What this is

C5's chorus explains; C6 asks **who teaches the explanation, and what the
teaching serves**. Every culture whose flagship settlement committed an
`organized` cult-form gains a second account — the institution's — derived
from the folk account by a pure, preregistered transformation (a
**selection bias, never deception**: no theory of mind anywhere), and the
Book renders the two side by side with the conflict between them derived,
classified, and disclosed:

> **As the Vavako tell it.**
> Vebe is the earth. The day returns because the sky must be crossed.
>
> **As the priesthood of the Vavako teach it.**
> Vebe is the earth. The day returns because ⟨HighGod⟩ wills it. The
> moons are numbered and known to the priesthood.
>
> *In truth, Vebe is a planet with two moons…*

(Illustrative; constructions fixed at plan time. Note the states at work:
the day's explanation is a MYSTERY — folk cannot check it, so doctrine
speaks alone and no counter-line fires; the moon claim is a
REVEALED-CLAIM — the formula asserts knowledge without the number. The
folk counter-annotation (`— though the folk say …`) fires only in the
CONTESTED state, e.g. a sky-capable kobold folk whose kept moon-count
meets a doctrine explaining the moons differently — see §3.2/§3.3.)

The reader-relative turn ships at its honest floor: the committed Book is
the **exoteric edition** (the priesthood asserts it *knows* the count,
without the number); a reader whose knowledge-set already contains the
underlying fact is shown the value — the Book confirms what initiates
know rather than leaking it.

## 2. What exists (verified in code + measurement, 2026-07-19, main @3423331)

- `cult-form` facts are committed per settlement (`"organized"` iff the
  derived caste structure contains the shaman caste; `SocietySummary
  { strata, has_priesthood }`; `ranked = strata >= RANKED_STRATA`).
  **Measured: seeds 1–6 all-organized (the rendered volumes will show
  doctrine); seeds 7/8/10 mix in folk settlements** (the SOC-1 gate's
  negative arm exists in the wild).
- C4/C5 shipped: `AccountParams` + the four filters + `Explained`;
  `accounts_of` (the dial's roster — census-visible); the schema library
  with priors/β/period-match binding; the corpus-law-bidirectional
  chorus surface; `high_god` on `Belief`.
- The vessel holds `Knowledge` with the `absorb_common` transfer seam
  (The Echo) — the initiation source for reader-relativity. `windows/book`
  must NOT import the vessel (cycle risk): the reader parameter is a plain
  fact-key set the vessel adapts into.

## 3. Architecture

### 3.1 The doctrine stack (worldgen `chorus.rs` extension)

For a culture whose **flagship** cult-form is `organized`
(`doctrine_of(world, species) -> Option<DoctrineVoice>`):

- **Doctrine params = folk params + four preregistered deltas** (all
  derived or authored-global; nothing per-culture):
  1. **Capability up:** sky-capability +0.25 (capped 1.0) — records and
     systematic observation (the MAP-18 calendar-priesthood mechanism).
     Doctrine may therefore KEEP facts folk lose (the moon count).
  2. **Mediation reweight:** the schema library gains an authored, closed
     `mediation` column (global build-state); doctrine's prior = folk
     prior × mediation. High: agentive, moral-accounting, essence-telos
     (mediated, folk-unverifiable). Low: cycle-return, path-journey
     (self-evident). Exact values at plan time.
  3. **β up:** +0.5 (orthodoxy is monomanic).
  4. **Agent preference:** the HIGH-GOD belief when period-compatible
     with the fact's rhythm; otherwise the folk period-match. Authority
     accrues to the presiding deity by construction, not conspiracy.
- New render-time draw labels (the C5 idiom; declared in language's
  streams module, **stream-manifest reference page regenerated in the
  same task** — C5's F1 lesson):
  `language/<species>/doctrine-schema/<domain>/<fact-shape>`,
  `language/<species>/doctrine-lexeme/<fact-key>`.
- **`DoctrineVoice` is a separate collection** (`doctrines_of(world)`),
  **never a member of `accounts_of`'s roster** — see §5's dial-roster
  law.

### 3.2 The conflict map (pure, `domains/language`)

Per (fact, folk-disposition, doctrine-disposition), a four-state
classifier:

| State | Condition | Surface rule |
|---|---|---|
| `Harmony` | effective dispositions + schemas agree | doctrine line renders; no annotation |
| `Contested` | accounts differ AND folk can verify (folk capability clears the fact's observability requirement) | the doctrine paragraph MUST carry the folk counter-annotation (the disclosure rule) |
| `Mystery` | accounts differ AND folk cannot verify | doctrine speaks alone, confident — the stable cell where authority accrues |
| `RevealedClaim` | doctrine holds (Kept/Explained) what folk lost | exoteric: the knowledge-claim formula without the value; esoteric: the value (§3.4) |

Folk-verifiability is derived (folk params vs the observability row);
authority-service is the doctrine schema's mediation weight. Both axes
derived → the conflict content is derived (LANG-39's chart).

### 3.3 The surface (`windows/book`)

Per organized culture, after the folk section: a doctrine paragraph
(`As the priesthood of the ⟨autonym⟩ teach it.` heading; emic sentences
through the SAME construction machinery over the doctrine account), then
its annotations in order: folk counter-lines for `Contested` entries
(`— though the folk say ⟨folk rendering⟩.`), then the truth margin
(italic, as today, per account). Every new construction bidirectional
(the Echo gate). Folk sections are byte-unchanged (additive only).

### 3.4 The reader-relative floor

`render_volume` keeps its signature (the committed artifact = the
exoteric edition, empty reader). A new
`esoteric_lines(world, reader: &BTreeSet<(String, String)>) -> …`
(fact keyed by (subject, predicate)) yields, for each `RevealedClaim`,
the initiated line (`— two, as the initiated count.`) iff the reader-set
contains that fact. CLI: `hornvale book --initiate` prints the initiated
edition to stdout for comparison (never committed — one artifact, one
drift surface). The vessel's `Knowledge`→reader-set adapter and a session
verb are a **followup registry note**, not this campaign (wasm release
discipline is its own close).

## 4. The laws (standing tests)

1. **The SOC-1 gate law:** a culture with a folk-cult flagship has NO
   doctrine voice (measured seed if one exists at plan time, else
   synthetic society), and every organized flagship yields exactly one.
2. **The dial-roster law (census keystone):** `accounts_of` returns
   exactly the C4 folk voices — asserted structurally (no DoctrineVoice
   in the roster) AND by the 50-seed chorus-study fixture byte-identity
   check (the population proof, run at the surface task).
3. **The selection-bias law:** doctrine params differ from folk params
   ONLY by the four preregistered deltas (assert field-by-field equality
   elsewhere) — no hidden divergence, no per-culture content.
4. **The disclosure law:** every `Contested` entry's doctrine rendering
   carries its folk counter-annotation; `Mystery` entries never do (both
   directions asserted).
5. **The corpus law:** every doctrine sentence and annotation
   round-trips byte-identically.
6. **The esoteric law:** with an empty reader-set the initiated lines are
   absent and the exoteric formula is present; with the fact in the
   reader-set the value line appears and matches the ledger's committed
   value (never any other source — mutation-verified).
7. **The margin law, inherited:** each account's truth margin still
   restores ground truth; annotations never substitute for margins.

## 5. Determinism and blast radius

- **No epoch. Zero new facts, concepts, predicates.** Doctrine presence
  reads committed `cult-form`; all deltas derived/authored-global; the
  only new draws are the two doctrine label families (render-time,
  additive-safe).
- **Census untouched (carve-out not spent):** no new metrics; the
  dial-roster law + fixture byte-identity prove the six census columns
  stay byte-stable. Genesis byte-identical.
- Local artifact regen: `the-book.md` gains doctrine sections
  (additive); the stream-manifest page regenerates with the labels (same
  task); no other artifact moves.
- type-audit tags at introduction; the LinkSympathy `SlotKind` table move
  (C5 followup) lands here IF the mediation-column edit touches the same
  table rows anyway — fold it in, behavior-neutral, and delete
  `agent_bearing`'s exception.

## 6. Non-goals

- Schism, heresy-as-event, faction dynamics — C8/deferred (the map
  CLASSIFIES the unstable cell; it does not fire events).
- Tongue-rendered doctrine, evidential morphology ("taught-that" marking)
  — C7, which will read the conflict map's states as evidential sources.
- Vessel verb + wasm wiring for the esoteric unlock — followup row.
- Doctrine over settlement/terrain facts — floor stays on the sky facts
  C4/C5 already carry.
- No new census metrics, no AWS spend.

## 7. Registry and decisions

- LANG-39 → `spec'd` at spec commit; → `shipped (floor: derived second
  stack, four-state map, exoteric/esoteric editions)` at close.
- Followup rows/notes at close: vessel esoteric unlock (the game seam);
  LANG-34 (esoteric true-names) pointer updated to name this floor as its
  substrate.
- Ledger #1–#7 promoted here at G3 resolution.

## 8. Flagged for G3

1. **The census keystone (leads):** doctrine voices live OUTSIDE
   `accounts_of`; the dial's roster is now a named law with the fixture
   byte-identity proof. Zero new metrics, zero AWS spend. Confirm.
2. **Determinism:** two new render-time label families; zero facts/
   concepts; no epoch; genesis byte-identical. Confirm.
3. **The selection-bias posture:** authority-serving distortion =
   capability↑ + mediation reweight + β↑ + high-god preference — all
   global mechanism; no deception, no theory of mind, nothing authored
   per culture. The mediation column values are the campaign's one new
   authored table — your veto point if any weight reads as editorializing.
4. **The esoteric floor:** exoteric formula in the committed Book;
   initiated values only for readers already holding the fact;
   `--initiate` CLI edition uncommitted; vessel wiring deferred. Confirm
   the narrowing.
5. **Surface taste:** the priesthood heading, the folk counter-line, the
   initiated line — all corpus-law bidirectional; illustrative block in
   §1.
