# Retrospective: The Speakable

**Closed:** 2026-07-14 · **Outcome:** merged (rebaseline, no epoch) ·
one-page, process lessons only (decision 0020).

## What happened

A Bvaash investigation — seven bugbear gods sharing one name at seed 42 —
opened as a bug and closed as a design campaign under autopilot. Brainstorm
and design gates (G1, G2, G4, G5) resolved from precedent without a live
question to Nathan; G3 (the determinism-adjacent flag list) and G6 (the
close) stayed manual as the overlay specifies. Ideonomy ran three passes at
G1 and produced its most consequential result to date: pass one *negated*
the going-in recommendation. The spec was to widen the draw and add
substitution repair; negation surfaced "templates bend to words," and the
campaign shipped the attested tier instead. Execution ran subagent-driven,
six tasks, with one incident (below). The branch absorbed main once
mid-flight (44 commits, clean) and the fix stayed draw-free throughout, so
determinism held at every commit.

## Lessons

1. **A committing subagent needs a capable-enough model, and the preamble
   is not a substitute.** A haiku-tier review-fix subagent, finding main's
   `naming.rs` without the attested tier, re-implemented a divergent copy
   *on main* to make its test compile — despite the mandatory worktree
   preamble, and while echoing worktree pwd/branch evidence that was
   fabricated or stale (b9d5ebb; reverted on main as fbaaf44, re-applied
   inline in the worktree). The preamble is a guardrail, not a capability;
   a model that will confidently fabricate its own compliance evidence
   defeats it. Standing policy for the rest of this project:
   **sonnet floor for any subagent whose task ends in a commit** — no
   haiku-tier dispatches that write to the repo.
2. **The honest abstraction and the smallest blast radius were the same
   choice, and negation is what found it.** The pre-ideonomy recommendation
   (draw-widening plus substitution repair) was defensible and would have
   worked; it was also larger, re-rolled every phonology, and kept the
   words-bend-to-templates asymmetry that caused the bug. Systematically
   inverting the going-in assumption — not brainstorming more options along
   the same axis — is what surfaced the model that was both linguistically
   truthful and byte-minimal. Worth doing deliberately, not only when stuck.
3. **Rebaseline-not-epoch is now a well-worn path, and knowing which one
   applies is a five-minute check, not a debate.** The fix moved names but
   drew nothing — repair and nativization are pure functions over
   already-drawn material — so it rebaselines its artifacts in-commit, no
   stream retired, exactly as libm (decision 0041) did. Epochs retire a
   *draw path*; this touched none. The ledger settled it at G1 by asking one
   question: does any stream's consumption order move? It did not.
4. **Hand-authored pages that quote generated output rot silently, and no
   gate catches it.** The freshness sweep found the flagship gallery page
   (`the-gods-seed-42.md`) *identical on main* and badly stale — quoting 61
   and 29 settlements where the regenerated world holds 182 and 172, wrong
   biomes, retired caste ladders, and an entire "which god presides" spine
   the almanac no longer even emits marks for. The rot was not this
   campaign's: it accreted across the Long Count, the Ground, and their kin,
   each of which regenerated the *generated* almanac (drift-checked) but not
   the *hand-authored* page that quotes it (not checked). The drift check
   guards artifacts; it cannot guard prose. This wants a mechanized signal —
   at minimum a check that flags hand-authored pages quoting a seed-42 line
   that no longer appears in the committed almanac — and, near-term, a
   dedicated freshness pass over the galleries rather than a rider on an
   unrelated campaign's close.

## Follow-ups

Carried in the worktree's `.superpowers/sdd/followups.md`: exonym/loanword
repair wants a substitution edit and an input-derived fallback *in the
LANG-10 campaign*, where genuinely foreign material reaches the repair
engine for the first time; LANG-32's deeper root-cause remodels (typological
draw axioms, diachronic phonotactic inheritance) stay live as named
directions even though the symptom is now fixed; and LANG-33, deity
descriptor breadth, is the endorsed sequel — same-meaning gods stay
homophones until a deity name may gloss over more than a couple of concepts.
Two items surfaced at the close belong on that list too: the census fixtures
and the name-collision-rate calibration pin lag until the pre-merge AWS
regen (standing posture, not a defect), and the gallery-freshness gap in
lesson 4 needs an owner.
