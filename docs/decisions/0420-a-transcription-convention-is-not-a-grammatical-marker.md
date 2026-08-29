# 0420. A tongue that questions by intonation renders a transcription convention, and that is not a grammatical marker

**Status:** Accepted (2026-08-29) · **Decider:** Nathan (autopilot) ·
**Relates:** [0058](0058-the-book-is-a-derived-view-grammar-is-build-state.md),
[0419](0419-interrogative-force-is-an-operator-over-a-clause-never-a-field-on-it.md) ·
[The Rail](../../book/src/chronicle/the-rail.md)

In the context of a drawn interrogative strategy where the majority outcome
cross-linguistically is *no overt marker at all* — a polar question marked by
**intonation**, which a text renderer cannot show (Ultan 1978; Dryer, WALS
116) — we decided that such a tongue renders its declarative surface plus
`?`, named in the code as a **transcription convention** and explicitly not as
a grammatical marker, accepting that two tongues with genuinely different
grammars can produce the same string.

## The rule the whole class follows

Transcription can express any contrast the model **draws** and cannot
manufacture one it does not. `Phonology::orthography` is a stated *view*, and
punctuation is how writing encodes prosody, so writing `?` for a
question is exactly the kind of thing an orthography is for. What it must not
do is pretend to be morphology: a tongue that drew no interrogative particle
has no interrogative morpheme, and the `?` is the transcriber's, not the
speaker's.

The distinction is load-bearing because the alternative — inventing a particle
for the intonation-only case so every tongue has "a marker" — would author a
grammatical fact the draw did not produce, and it would be invisible in the
rendered output, which is the worst combination available.

## What ships

`TongueGrammar` gains `interrogative: Option<String>` on a new permanent
stream label (`language/<species>/grammar/interrogative`), drawn as a
one-syllable roman form by the identical route the copula and the
complementizer already use. The presence split is 40% particle / 60% none,
labelled in its own comment as **an authored-typology round split, not a
literal WALS citation** — the same honesty `ConstituentOrder`'s roll
boundaries already carry ("authored typology, approximate WALS frequencies").
The literature is cited for the *direction* (majority-is-intonation) and never
for the number.

`realize_tongue_polar_question` is a **different operator from Common's, not a
port**: Common inverts its one auxiliary because English marks a polar
question by inversion; a tongue prefixes a particle or changes nothing but the
terminal punctuation. Neither strategy moves a constituent, so — unlike
Common's — a tongue questions a lexical-verb clause exactly as readily as a
copular one. The particle is clause-initial, citing WALS 92.

The new stream label is **additive by construction** (0058): grammar is
build-state, drawn at composition time and never serialized, so the axis moves
no already-generated world's bytes. Both regeneration paths were run and zero
byte-goldens moved; only the published stream manifest gained rows.

## What this does not settle

This does **not** close the question of real alphabets and writing systems.
That is stated future work of its own, and a reading of this record as
"orthography is done" would be wrong. What is settled is narrower: where a
drawn contrast has a strong orthographic convention, the renderer may use it
and must label it as transcription.

## Consequences we accept

- **Two tongues can render the same question string for different reasons** —
  one because it drew no particle, one because it drew a particle a reader
  cannot distinguish from punctuation. Nothing in the rendered text
  distinguishes an intonation-only question from a grammar that happens to
  look like one, and nothing should: the difference is in the drawn grammar,
  which is what a reader inspects when they want it.
- **Contrasts the model does not draw stay unrenderable, and that is the more
  fixable half.** Contrastive focus has only typography and prosodic timing
  has nothing, but both are unrenderable because they are **unmodelled** —
  a different problem from this one, and a cheaper one.
