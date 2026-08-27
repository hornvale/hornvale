# 0308. Prior and fold are stored separately; their disagreement is the output

**Status:** Accepted (2026-08-26) · **Decider:** Nathan · **Relates:**
[0256](0256-a-hosts-testimony-is-fallible-by-construction.md) (this
campaign's own shape one level up — the gap between arbitration and
testimony, versus the gap between doctrine and conduct); [0306](0306-a-hosts-cooperation-is-derived-from-doctrine-and-conduct-never-authored.md);
[The Reticence](../../book/src/chronicle/the-reticence.md)

In the context of a host's willingness having two independent sources — what
its people believes a rider is, and what this particular rider has actually
done — we decided that **the doctrine prior and the conduct fold are kept as
two separate values through to the point of use, `stance_for(prior,
overrides)`, and never pre-summed into one disposition number**, accepting
two things to carry where one might look tidier, in exchange for keeping the
disagreement between them reachable.

## Context

Summing the two at construction — folding a people's doctrine and a rider's
conduct into a single scalar the moment a session starts — would make an
entire class of host unreachable: one whose culture still calls the rider by
its warm, ignorant word while its own patience for this particular rider has
run out. That host exists exactly because the two inputs disagree, and a
pre-summed model cannot express disagreement, only a blended average of it.

This is the same shape The Confidant's own testimony mechanism already
proved out one level up: that campaign's deliverable was the gap between
what arbitration computed and what the host said, kept as two readable
things rather than resolved into one before the player could see either.

## The rule

`stance_for(prior: Openness, overrides: u32) -> Stance` takes both as
separate arguments. `Openness` is computed once per doctrine lookup and never
adjusted by conduct; `overrides` is read fresh from
`Session::driven_overrides` at the moment of the ask. Nothing in the pipeline
between them multiplies, averages, or otherwise collapses one into the
other before this function's own four-way match does.

## Consequences

- **The measurement this campaign shipped depends on the two staying
  separate.** H4 (spec §5) asks exactly the question this separation makes
  askable — does the prior move testimony independently of the fold — and it
  could not be posed at all against a single pre-summed number.
- **A caller wanting one willingness scalar still has to compute it from
  both**, which is more calling-site work than reading a single field. The
  campaign judged that cost acceptable because the two-value form is the one
  every consumer this campaign or a later one is likely to have actually
  needs: `stance_for` itself, the per-people report, and any future study
  reading either half independently.
- **What we give up:** a single `Willingness` struct with one comparable
  scalar would be simpler to log or sort by. Nothing forecloses adding one
  later as a *derived* read over both fields; what this decision forecloses
  is deriving it *before* the disagreement is visible.

## See also

Spec §3.5; `windows/vessel/src/stance.rs`.
