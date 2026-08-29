# The Lexicon

A lexicon is the vocabulary a reader brings to a text; this campaign gives
`clients/game` one — and, crucially, makes the *sim* the author of it. Two
defects found by playing rather than by tests: `examine` with no noun
answered "Examine what?" **and cost a turn**, and noun matching was
exact-word, so `examine vnga` resolved nothing against `Vngashngatva`. A
player had to type whole invented names from memory. `Tab` sat reserved and
bound to nothing precisely so something like this could claim it.

## The split: sim emits candidates, client matches

The constitutional posture decided the placement before any code was
written. Decision 0022 (sim emits data, clients render), decision 0117 (the
client re-derives nothing the sim emits) and the Stylus's "the client sends
text and renders the reply" together draw the line:

- **The sim owns the vocabulary and its kinds.** `focalize::Noun` gained a
  coarse `NounKind` (`creature`/`place`/`thing`/`unknown`, defaulting to
  `Unknown` — an honest "no kind claimed"), and a `kind` tag rides
  `vessel/session/v2` **additively**: serde default keeps every older mirror
  loading; no v3 was minted for an optional field. This is the part a
  future verb-category filter needs, so it shipped first.
- **The client owns matching.** Completion is a pure function of the
  player's own buffer state — prefix, caret, keystrokes. The sim never sees
  a caret or a Tab press, and completion must be instant and turn-free,
  which forecloses a round-trip through `Session::handle` per keystroke.

The hard constraint from The Quire held throughout: the client may
*suggest* but never *pick*. Completion only fills text the player then
submits; an unknown or wrong name is answered by the sim's own prose.

## Scopes compose

The candidate vocabulary is not a flat list but a fold over composable
scopes (`CandidateSource`), first-wins by name. v1 registers exactly one —
the current turn's `narration.nouns` — but session lexicon, carried things,
and remembered places each land later as new scopes appended to the
composition, never as edits to the completer. That composition shape was
the design's load-bearing choice: it turned "more vocabulary later" from a
rewrite into a registration.

## Tab under the CLI

`Action::Complete` routes from `Tab` under [`Focus::Cli`] only; Map and Walk
keep it bound to nothing. An ambiguous match extends the token to the shared
stem and raises a hint line beneath the command row where each candidate's
stem is bold and the suggested remainder normal — the differentiating
character is visually explicit, so `vng[TAB]t[TAB]` walks down a match list.
Overlong lists collapse to an honest "… +N more".

**The weight channel, refined deliberately:** decision 0142 assigns weight
to attention, and "the part you have typed vs. the part you still owe" is
exactly attention. On chart surfaces the channel keeps its perishable
ladder (`here` > `sensed` > `remembered`); on completion surfaces it
carries typed-vs-suggested. The two never co-render, and the refinement is
recorded in both the spec and the drawing code so neither client invents
its own meaning.

A pluggable `TabStyle` separates presentation from mechanics: `Hint`
ships bound; `Cycle` (successive Tabs rotate full matches) is implemented
and tested but unbound, because no preference mechanism exists to bind it
to.

## Bare `x` asks instead of burning

Submitting bare `x` opens a noun prompt: the line temporarily reads
`examine …`, Enter dispatches through the ordinary submit path, Esc restores
the saved buffer verbatim, Backspace clamps at the prefix. One documented
delta from the registry sketch: the prompt is the composed line itself
(`examine bramble` visible in the buffer) rather than a separate prompt
glyph — showing exactly what will be sent beats a second surface. A review
finding worth keeping: completion state (hint, cycle rotation) originally
survived history recall, which would have let a stale rotation rewrite a
recalled line — "any other action drops it" is now enforced on caret moves
and history too, with the failure pinned by test.

## What was deliberately left out

Verb-category filtering ("GWIM": `kill <tab>` suggests creatures, not rugs)
is registered as its own row — the kinds ship now precisely so that filter
becomes a sim-side emission change instead of a client rewrite. Display-width
correctness of the hint line rides the crate-wide char-counting debt, all
five functions together. And no scope beyond the current turn's nouns ships:
the trait is their seam; their content is not this campaign's.
