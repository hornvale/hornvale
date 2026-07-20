# The Book Polish — Design

**Date:** 2026-07-20
**Status:** Draft — awaiting G3 review (campaign-autopilot hard stop)
**Campaign:** the second post-program campaign — a readability-only
surface fix, no new mechanism. Follows up two rough edges C6's and C8's
own final reviews flagged and deferred as out-of-scope at the time.

---

## 1. What this is

Two Reckoning-of-Years surface defects, both readability-only:

**The anonymous folk-line stutter.** `reckoning_culture_lines` always
opens with the fixed unattributed string `"The sky has darkened, now and
again."`, for every culture at every rung — organized cultures get
disambiguated by their following Numbered/Predictive line, but a
folk-only culture (never organized, per the SOC-1 gate) never gets a
second line to disambiguate it. Volume 3's committed hundredth year
today reads:

> The sky has darkened, now and again.
> The priesthood of the Sdeozqae numbers the darkenings: 32.
> The next darkening, it teaches, comes on day 36953.
> The sky has darkened, now and again.
> The sky has darkened, now and again.

— two identical, anonymous lines in a row (Shteozqae and Jjajjjo, both
folk-only). A cold reader sees a duplicate, not two peoples.

**The dangling initiated line.** `esoteric_lines` renders only the bare
continuation `"— {cardinal}, as the initiated count."`, with no subject
— fine inside the vessel's turn-by-turn flow (you just wrote about the
moons), but floating and unanchored in the CLI's static `--initiate`
dump, which has no surrounding context at all.

## 2. What exists (verified, main @62f29b7d)

- `RECKONING_FOLK_COUNTED: &str` (windows/book/src/lib.rs:566) — the
  fixed string, pushed unconditionally by `reckoning_culture_lines`
  (line 765), which already receives `autonym: &str` as a parameter but
  never uses it in this string.
- `ReckoningLine::FolkCounted` — a unit variant in the parse/rerender
  round-trip enum; `parse_reckoning_line`/`rerender_reckoning_line` both
  handle it as a bare string match (no captured data).
- `esoteric_lines(world, reader) -> Vec<String>` (line 1438) — pushes
  `format!("— {}, as the initiated count.", cardinal(*n as u64))`, with
  `entry.fact.subject` available in scope but unused in the string.
  Called from `cli/src/main.rs`'s `--initiate` path and
  `windows/vessel/src/session.rs`'s `consult`; carries no corpus-law
  parse obligation (LANG-39's esoteric law only requires value-source
  correctness, never round-trip).
- `revealed_claim_line(predicate, object)` (line 1168) — the exact
  plurality-aware pattern to mirror: singular/plural closed strings keyed
  on the moon count.

## 3. Architecture

### 3.1 The folk line gains its autonym (`windows/book`)

`RECKONING_FOLK_COUNTED` (the bare string) is replaced by a format:
`"Among the {autonym}, the sky has darkened, now and again."` —
attributed for EVERY culture, at every rung, uniformly (not only when
consecutive folk cultures would otherwise collide). `ReckoningLine::
FolkCounted` gains `{ autonym: String }`; `parse_reckoning_line` strips
the `"Among the "` prefix and the fixed suffix, recovering the autonym
(mirroring the existing `Numbered` variant's own strip-prefix/strip-
suffix parse); `rerender_reckoning_line` re-assembles it. Volume 3
becomes:

> Among the Sdeozqae, the sky has darkened, now and again.
> The priesthood of the Sdeozqae numbers the darkenings: 32.
> The next darkening, it teaches, comes on day 36953.
> Among the Shteozqae, the sky has darkened, now and again.
> Among the Jjajjjo, the sky has darkened, now and again.

### 3.2 The initiated line states its subject (`windows/book`)

`esoteric_lines` renders `"{subject} has {cardinal} moon{s}, as the
initiated count."` (count-aware, mirroring `revealed_claim_line`'s own
singular/plural split) instead of the bare em-dash continuation. No
parse obligation exists for this line today (it carries none — verified,
§2) and none is added; only the exact-string test pins update.

## 4. The laws (standing tests)

1. **The attribution law:** every rendered folk-counted line names its
   own culture; no two folk-counted lines in one epoch are byte-identical
   (the stutter cannot recur even if a future world places three or more
   folk-only cultures in one epoch).
2. **The corpus law, extended:** the attributed folk line round-trips
   (parse → rerender byte-identical) for every measured seed/epoch —
   extends the existing Reckoning round-trip walk.
3. **The subject law:** every rendered initiated line names its subject
   and agrees in number with its cardinal (one moon → singular, two+ →
   plural) — measured against every seed that reaches a RevealedClaim,
   both arities exercised (seed 1's two-moon world and a one-moon world).
4. **Additivity, scoped:** every OTHER Book section (Tongues, Chorus,
   doctrine emic/margin) byte-identical; only the two touched strings'
   own lines change, and only in the ways predicted above — verified by
   regenerating the artifact and diffing against the pre-campaign commit.

## 5. Determinism and blast radius

Zero draws, zero facts, zero concepts, zero census impact (neither
string enters any lab metric). The committed artifact's Reckoning
sections and any `--initiate`/vessel-consult output change (predicted,
additive-in-information, never additive-in-lines — no new lines, existing
lines gain text); every test pinning the old bare strings re-pins in the
same commit, with provenance comments (the C7/C8 golden-pin discipline).

## 6. Non-goals

No change to the folk/doctrine attribution scheme elsewhere in the Book
(the Chorus's stance appositives, the doctrine counter-annotations —
those already name their subjects); no change to which cultures reach
which ladder rung; no new mechanism of any kind. LANG-43 (irregularity)
and LANG-44 (numeracy) are separate, larger campaigns queued next.

## 7. Registry and decisions

No registry rows — this is pure surface polish on already-shipped
mechanisms (LANG-40's Reckoning surface, LANG-39's esoteric layer). The
close's retrospective records the fix; no chronicle registry-ID citation
(the book's own ID-scoping rule).

## 8. Flagged for G3

1. **Determinism (leads, and it's the quietest kind):** two closed-string
   changes, no draws, no facts, no census impact. Confirm.
2. **Surface taste (the only real judgment call):** `"Among the
   ⟨Autonym⟩, the sky has darkened, now and again."` and `"⟨Subject⟩ has
   {cardinal} moon{s}, as the initiated count."` — your call on wording.
3. **The uniform-attribution decision (ledger #1):** every folk line
   gets named, not just consecutive duplicates — confirm you're happy
   with the more thorough (and simpler) fix over a narrower
   merge-only-when-colliding patch.
