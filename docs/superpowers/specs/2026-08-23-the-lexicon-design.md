# The Lexicon — tab completion and noun entry for the game client

**Date:** 2026-08-23
**Status:** design
**Registry rows:** CLIENT-tab-completion, CLIENT-noun-entry-for-examine
**Supersedes:** nothing. **Reserves:** verb-category filtering ("GWIM") as a followup slice.

## 1. Problem

Two defects, both found by playing rather than by tests:

1. A bare verb that needs an object burns a turn answering "Examine what?" —
   `x` is bound to `examine` and an argumentless submit costs a turn for a
   reply that carries no information (CLIENT-noun-entry-for-examine).
2. Noun matching sim-side is exact-word (`Noun::matches`), so `examine vnga`
   resolves nothing against `Vngashngatva`. A player must type whole fantasy
   names from memory. `Tab` sits reserved and bound to nothing precisely so a
   completion feature could claim it (CLIENT-tab-completion).

## 2. The split: sim emits candidates, client matches

The constitutional posture (decision 0022; The Stylus's "the client sends
text and renders the reply"; decision 0117's "the client re-derives nothing
the sim emits") decides the placement:

- **Sim-side:** the examinable vocabulary and its KIND data. The sim already
  emits `narration.nouns` (`vessel/session/v1`) — this campaign adds a
  category tag per noun, **additively** (schemas are additive-or-versioned;
  no v2 is minted for an optional field). Categories are a small closed set:
  `creature`, `place`, `thing`, with `unknown` as the honest default where
  the sim has no kind to claim.
- **Client-side:** matching. Completion is a pure function of the player's
  own buffer state — prefix, caret, keystrokes. The sim never sees a caret
  or a Tab press, and completion must be instant and turn-free, which
  forecloses a round-trip through `Session::handle` per keystroke.

This keeps both clients (terminal and web) rendering from one shared,
sim-authored vocabulary, and makes the reserved GWIM filter ("`kill <tab>`
suggests goblin, not rug or Vngashngatva") a future *sim-side emission*
change instead of a client rewrite.

**The hard constraint from The Quire stands unchanged:** the client may
*suggest*; it must never *pick*. Completion only ever fills text the player
then submits; an unknown or wrong name is answered by the sim's own prose.
Nothing reads `narration.nouns` to choose an object on the player's behalf.

## 3. Candidate sources — composed scopes

Candidates come from a **composition of scopes**, not one flat list:

```rust
trait CandidateSource { fn candidates(&self) -> Vec<Candidate>; }
```

- `Candidate { name: String, category: Category }`; `Category` mirrors the
  wire tags above.
- `Lexicon`, in `hornvale-game-core`, folds its registered scopes in order,
  first-wins dedup by name. Registered = active; there is no configuration
  surface in this campaign.
- v1 registers exactly ONE scope: `CurrentTurnNouns`, fed from the current
  turn's `narration.nouns`.
- Later scopes — session lexicon (everything ever seen), things carried,
  remembered places — each land as a NEW scope appended to the composition,
  never as edits to the completer.

## 4. Interaction

### 4.1 Tab completion (Cli focus)

`Tab` moves from `Action::None` to a new `Action::Complete`, **under
`Focus::Cli` only**. Map and Walk stay `None` — completion means nothing
without the buffer focused. The totality tests change one cell each; the
`tab_is_bound_to_nothing_in_any_focus` test narrows accordingly.

Driver flow on `Complete`:

1. Take the whitespace-delimited word ending at the caret as the prefix
   (v1 tokenization; no smarter parsing).
2. Empty prefix or no match → no-op: nothing sent, no turn, nothing drawn.
3. Unique match → replace the token, caret after it.
4. Ambiguous → extend the buffer to the common stem AND raise the hint.
   Typing any character dismisses the hint line.

### 4.2 Pluggable presentation

Cycle-vs-hint is a strategy behind one call site, not an `if` in the
driver: `TabStyle::{Hint, Cycle}`. This campaign ships `Hint` (below);
`Cycle` (first Tab extends to the longest common prefix, further Tabs cycle
full matches) is implemented and tested but unbound — no preference
mechanism exists yet, and building one here would be scope for its own
sake.

### 4.3 The hint line

One line beneath the command buffer in the entry pane, present only while
an ambiguous completion is active. Each candidate renders with the shared
stem bold and the remainder normal — `vng` → **vngashnga**tva
**vngashnga**kelm — so the differentiating character is visually explicit.
Matches beyond the pane width collapse to `… +N more`. Order is lexicon
order (current-turn nouns first): deterministic, no sort churn between
renders.

**The weight channel, refined deliberately:** this bold IS decision 0142's
weight channel carrying ATTENTION — "the part you have typed vs. the part
you still owe" — not entry-pane styling outside the vocabulary. Both
clients render the same affordance, so styling it ad hoc would fragment.
On chart surfaces the channel keeps its perishable ladder (`here` >
`sensed` > `remembered`); on completion surfaces it carries typed-vs-
suggested. The two never co-render, and this refinement is recorded here
so the web client inherits one documented meaning rather than inventing
its own.

### 4.4 Noun-entry mode

When `x` alone would submit bare `examine`, the driver instead enters a
modal state: the entry prompt changes (`examine:`), typed characters build
only the noun, `Enter` dispatches `examine <noun>` as one ordinary line,
`Esc` cancels and restores the buffer untouched (the stash-the-line
convention of The Stylus's history semantics). Completion inside the
prompt uses the same lexicon and the same `Tab` behaviour. This is a
prompt, not a picker — constraint §2 applies verbatim.

## 5. Turn-cost invariant

`Tab`, hint rendering, and entering/leaving the noun prompt cost NO turn —
only an `Enter`-dispatched submit reaches `Driver::handle`. An empty-buffer
submit already costs no turn; this campaign adds no second path that can
advance the clock.

## 6. Testing

- Engine logic (tokenization, stem computation, unique/ambiguous/none,
  first-wins fold) is pure and table-driven in `hornvale-game-core`.
- Driver state-machine tests: enter prompt → type → submit sends
  `examine <noun>`; Esc restores the buffer; ambiguous Tab extends and
  raises the hint; typing dismisses it.
- Drawing tests assert bold-boundary positions and the `+N more` collapse
  on fixed strings. No golden churn: nothing here touches committed
  artifacts, wire schemas byte-goldens, or census columns.

## 7. Out of scope / reserved

- Verb-category filtering (GWIM) — needs sim-side emission of categories
  consumed at completion time; reserved, and the reason kinds ship now.
- A user-preference mechanism for `TabStyle` — waits for any settings
  story at all.
- Scopes beyond `CurrentTurnNouns` — session lexicon, inventory, memory;
  the trait is their seam, their content is not this campaign's.
- Display-width-correct drawing — the hint char-counts like the rest of
  the pane today; it converts when CLIENT-display-width-not-char-count
  lands, all five functions together.
