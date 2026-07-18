# Retrospective: The Tongues

One page, process only. Product story: the chronicle entry.

## What worked

- **The self-review caught a thesis violation before execution.** The
  plan's first draft gave copula-bearing tongues an authored particle
  ("gha"). The program's constitution is zero authored surface text in
  generated languages; the fix (draw the copula's form from the tongue's
  own phonology through the proto_root pipeline) was cheaper than the
  violation and produced the campaign's best moment — *Saa* is a real
  goblin word. Plans want a thesis-check pass, not just a coverage pass.
- **An implementer corrected the plan's arithmetic.** The draft weights
  used exclusive-bounds thresholds; the kernel's `range_u32` is inclusive.
  The T1 implementer verified the helper's semantics against the kernel
  rather than transcribing, and re-derived the buckets exactly. The
  verbatim-code-plan style keeps working *because* implementers are told
  the live file is the authority.
- **Review loops stayed cheap and real**: one Important across four tasks
  (a verbatim-duplicated formatter, fixed in-crate with a `Display` impl
  the same hour), and the final fable review measured every constitutional
  claim (regenerated four reference pages into scratch to prove lexicon
  byte-identity) instead of trusting the reports.
- **Same-day cadence held for a third consecutive campaign** (C2 → Echo →
  C3, all spec→close within the session's day), with main absorbed three
  times across them and every absorption clean or one-generated-file.

## What the campaign got wrong, and what it taught

- **Two `&&`-chain self-inflicted mysteries.** A `head`-terminated pipe
  SIGPIPE'd a compound command and silently skipped the `git merge` later
  in the chain (twice); a NO-GO preflight aborted a chain before the
  progress ledger was ever written, which then read as "scratch was
  swept." Neither cost more than minutes, but both produced false
  narratives before the real one. Lesson: **state-changing commands get
  their own Bash calls; never chain them after pipes or gates.**
- **The metaplan's interface wording narrowed in delivery** — it says
  "`ClauseSpec → String` per language"; what shipped is `TongueClause`
  (subject + complement concept, Classify-only). The plan specified the
  narrowing deliberately (Common's ClauseSpec carries surface strings,
  not concepts — the tongue realizer needs concepts to lexicalize), but
  the metaplan text and the spec's §2.2 didn't say so out loud, and the
  final review had to reconstruct the reasoning. Lesson: **when a floor
  narrows an interface the program doc promises, the spec should name the
  narrowing explicitly** — reviewers shouldn't have to derive intent.
- **Coverage reporting shipped extensional, not derived**: the per-tongue
  report enumerates exactly {self-statement, planet-probe} by hand; a
  future renderable fact won't auto-enter it. Fine for a two-fact floor,
  named as C4's first structural task (derive the report from the render
  inventory; promote the planet-probe's success path so a realizable fact
  can never silently vanish).

## Follow-ups (promoted from the register)

cli `gap_text` → `GapReason::Display` cutover (one line, drift-checked
safe). `articles` tag class → `bare-ok(flag)` in the next audit wave.
C4 notes above. `views_of` dedup adjudicated a non-issue by the final
review. LANG-44 numeracy still banked (unchanged by this campaign).
