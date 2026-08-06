# The Handle — every named thing answerable

**Campaign:** The Handle · **Date:** 2026-08-06 · **Status:** spec, awaiting G3
review · **Branch:** `the-handle` off main @ a3f61974

A noun is the handle you grab a thing by. The world names things it will not
then let you touch.

---

## 1. The defect, as measured

On main, in the flagship room of seed 42:

```
Tropical seasonal forest — buttressed canopy — in the lands of Goodogododaga.
The sky above: Night. The vast moon is a smear of light. …
> examine forest      You see no forest here.
> examine canopy      You see no canopy here.
> examine moon        You see no moon here.
> examine stream      You see no stream here.
```

`Session::examine` resolves by whole-string equality against a catalog whose
keys are the phrases the prose used verbatim. A player types the head noun.

**Sized mechanically.** Over that room's five catalog entries, taking every word
of every display name longer than two characters and not a stopword:

| entry | words | resolve today |
|---|---|---|
| `tropical seasonal forest` | tropical, seasonal, forest | none |
| `buttressed canopy` | buttressed, canopy | none |
| `bugbear of Goodogododaga` | bugbear, goodogododaga | goodogododaga only |
| `Goodogododaga` | — | (whole name) |
| `sky` | — | (whole name) |

**6 of 7 fail.** The one that resolves does so by coincidence: "Goodogododaga"
is separately its own entry. Nothing about the *phrase* made its words
reachable.

### 1.1 Four surfaces, one defect

- **Surface prose** — biome and regime descriptor phrases (`LOC-examine-head-noun`).
- **The sky** — one opaque entry whose datum is the whole report, so the two
  moons it names are unreachable (`LOC-sky-is-one-noun`).
- **The chart legend** — NPC marks like `bugbear of Goodogododaga`; found while
  sizing the gate, not previously filed.
- **The underworld** — `describe_underground_here` names "the rock" and a
  stratum word, and `session.rs:947` special-cases only `self.inside`, so an
  underground possession resolves against the *surface* locale's catalog.
  **Structural finding, not a live reproduction:** no cave was reachable within
  400 walked steps from the flagship and `delve_at` is crate-private, so the
  probe was inconclusive by that route. Implementation reproduces it in-crate
  before fixing it (§7).

## 2. The root cause

Every one of these surfaces **already composes its prose from named parts, and
then discards the parts.**

`windows/locale/src/grammar.rs:58-62` builds the descriptor and says so in its
own comment — *"The variety and its substrate detail are one noun phrase … the
habitat and exotic clauses are qualifiers"* — then `join(" ")`s them into a
string. Astronomy composes moon epithets inline inside `sky_at_visibility`. The
underworld composes `"The rock here is {stratum_word}"`.

So the head noun is **known at generation time and thrown away**. The catalog is
then built from the flattened string, and a player's word cannot find it.

This also settles what *not* to do. Recovering the head noun by parsing the
generated prose is the wrong direction, and a mechanical last-word rule fails on
the real data: the head of `a stream gully, shaded, in a hollow` is "hollow",
not "stream".

## 3. Design principle

**Prose is composed from declared parts; the catalog is those parts.**

The consequence worth stating plainly: *"mentioned but undeclared" stops being
possible by construction rather than by checking*, because a generator cannot
put a word in the sentence except by declaring it first. That is what makes the
gate in §6 cheap — the hard direction never needs to be enforced.

## 4. Display name and resolution words

A catalog entry gains a set of **resolution words** beside its single **display
name**.

This split is not decoration. The catalog is **not private to `examine`**:
`Focalized.nouns` flows into `SessionSnapshot.narration.nouns`
(`snapshot.rs:221`), which the browser client decodes and which has committed
fixtures. Putting aliases in that list would spray "forest, tropical, seasonal"
into a client-facing legend beside the real entry.

So: **resolution words are process-internal and never serialize.**
`NounEntry { noun, datum }` is unchanged, and `vessel/session/v1`'s *shape* does
not move. Its *content* grows — new entries for sky bodies and the underworld —
which is additive, and re-pins the three committed fixtures.

**Ambiguity resolves deterministically, with no prompt.** First match in catalog
order wins, and the existing cross-catalog precedence (prose before chart) is
untouched. This is the rule `Session::examine` already documents for the
existing collision case — *"A noun named by both grains still resolves to the
prose datum, because the prose catalog is checked, and answered from, first"* —
applied one level inward. A "which do you mean?" prompt needs pending-question
state in the verb loop and has no current demand; recorded as a follow-up, not
built.

## 5. The four surfaces

| surface | change |
|---|---|
| `windows/locale/src/grammar.rs` | `render` returns its parts beside the rendered string; the noun phrase (`variety` + `substrate_detail`) is declared, the qualifiers are not nouns |
| `windows/vessel/src/focalize.rs` | builds catalog entries from those parts rather than from the flattened descriptor |
| `domains/astronomy` | `SkyReport` gains per-body display phrases (the epithets `sky_at_visibility` composes today), so each visible body becomes its own entry — additive to a domain pub API |
| `windows/vessel/src/session.rs` | an `examine` arm for `self.underground`, mirroring `examine_chamber`; and chart-legend marks contribute resolution words |

Resolution words are derived where the derivation is safe (the significant words
of a declared part) and declared where it is not.

## 6. The gate

Two halves, and the second is the one that would have caught all four.

1. **Structural** — prose composed from declared parts (§3). Not a test; a
   property of the construction.
2. **Mechanical** — *every significant word of every catalog entry's display
   name resolves to an entry.* No curation, no parser, no new dependency. It
   fails 6-of-7 today and would have failed on `forest`, `moon`, `rock` and
   `bugbear` alike.

"Significant" means: longer than two characters, not in a small stopword list.
Both are a stated judgement rather than a discovered fact, and both live in one
place so the judgement is visible.

**The residual gap, stated.** The mechanical half enforces that *declared* nouns
are reachable. It cannot enforce that everything the prose mentions is declared
— that is what §3 closes structurally, and a generator that hand-writes a
sentence instead of composing one escapes both. A curated "words a player might
try" list was considered and rejected: it is maintained at human rate and rots
silently, whereas a rule derived from the catalog's own data cannot.

## 7. Testing

1. **Require RED, on the assertion.** Each new guard runs against unfixed code
   and is observed to fail on its assertion — not merely to fail compiling. The
   Benchmark's vacuous guard is the local precedent for why.
2. **The word-resolution gate** (§6.2) over the flagship room and at least one
   other, since one room is an anecdote.
3. **The sky's bodies resolve** — `examine moon` answers, and answers about a
   moon rather than echoing the whole sky report.
4. **The underworld reproduces before it is fixed** — an in-crate test that
   delves via the crate-private path, asserts `examine rock` currently refuses,
   then asserts it answers.
5. **The wire shape is unchanged** — `NounEntry`'s fields are exactly
   `{ noun, datum }`; the three committed fixtures re-pin with added entries and
   no removed or renamed key, verified by parsing and walking both versions.
6. **Determinism** — same seed, byte-identical world; no ledger predicate moves.

## 8. Non-goals

- **No disambiguation prompt** (§4).
- **No natural-language parsing** of generated prose, ever (§2).
- **No new verbs.** `examine` is the whole surface.
- **No epoch and no save-format change.** The wire shape is stable; only catalog
  content grows.
- **No census regeneration.** Nothing in the measurement layer reads the noun
  catalog.

## 9. Risks

1. **Widening the catalog widens what the client shows.** New entries for sky
   bodies and the underworld appear in the panes client's legend. That is the
   intent, but it is a visible product change and the fixtures will show it.
2. **The astronomy change touches a domain pub API.** Additive, but
   `SkyReport` is consumed by more than the vessel; every reader is the blast
   radius until enumerated.
3. **Stopwords and the length floor are judgement.** Set once, in one place, and
   a bad setting shows up as a noisy gate rather than a silent one.

## 10. Success criteria

- `examine forest`, `examine canopy`, `examine stream`, `examine moon` all
  answer in the flagship room; `examine bugbear` answers about the NPC.
- The word-resolution gate passes, and was observed to fail before the fix.
- `vessel/session/v1`'s shape is unchanged; its three fixtures re-pin additively.
- `make gate` green; `make vessel-check` green, because the client fixtures move
  and the commit gate does not run the client checks.
