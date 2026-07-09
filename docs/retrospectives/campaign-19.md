# Campaign 19 (The Star Chart) — retrospective

**Merged:** 2026-07-09

**Recurring findings.** A stale committed artifact missed by a blast-radius
enumeration has shown up before (Campaign 17's audio set-equality gate exists
because a generated binary needs an independent freshness check, not because
this exact miss had happened previously). What's new here is *where* the miss
occurred: not in the code the plan touched, but in the plan's own list of
what that code's change would ripple into. Worth watching for again — an
enumerated blast radius is a claim about completeness, and claims about
completeness are exactly the kind a fresh pair of eyes catches better than
the author who wrote the list.

**Estimate deltas.** No stage-level estimates were made for this campaign,
so there is nothing to compare against — say so rather than pad.

**Spec vs. reality.** One place where review caught what the plan's own
enumeration missed, and one place where a font table needed a second look
before it shipped:

- The spec enumerated the synodic fix's blast radius explicitly — the three
  committed almanac fixtures and the calendar's own unit tests — and that
  enumeration was accurate as far as it went. What it missed was a committed
  lab study of month counts across many seeds, `census-of-skies`, which also
  reports on the exact quantity the fix changed. The same study was
  independently absent from the CI step that regenerates every committed
  artifact and fails the build on drift — so a stale study could have sat in
  the repository, silently wrong, without either the enumeration or the
  automated gate catching it. A reviewer caught both gaps in the same pass,
  and both were closed together: the study re-baselined, and its regeneration
  command added to CI's list. Neither miss would have corrupted a world —
  the defect was already fixed at the source the study reads from — but a
  committed number that no longer matches the code that produced it is its
  own kind of drift, and this project's CI exists specifically to make that
  kind of drift loud rather than quiet.
- The planisphere's digit labels come from a small hand-drawn bitmap font —
  five digits, a handful of pixels each, written directly into the spec as
  a reference table for the implementation to copy. One digit's bitmap was
  malformed in the plan itself, before any code existed: had it shipped
  unreviewed, every future chart needing that digit would have rendered a
  misshapen numeral, and the freshness gate — which checks that the PNG is
  byte-deterministic, not that it is *correct* — would have happily kept
  regenerating the wrong glyph forever. A review pass caught the malformed
  bitmap in the plan's own table, before it ever reached generated code.

**Do differently next time.** The campaign's three deliverables were
strictly ordered — fix the synodic defect, then draw star positions, then
render the chart — specifically so the chart would never render a picture
whose underlying physics was still wrong. That ordering paid off exactly as
intended: at every commit along the way, the moon phase reported by the
almanac was already correct, so there was no window in which "the chart" and
"the sky it charts" told two different stories. The lesson worth repeating:
when a campaign both fixes a defect and builds new presentation on top of
the corrected model, sequence the defect fix first and treat the
presentation as strictly downstream — never let the two land in the same
commit, and never draw a picture of a number you haven't finished trusting.
A second, smaller lesson from this campaign's two catches: a blast-radius
enumeration and a hand-authored constant table are both claims a plan makes
about its own completeness, and both are worth a dedicated second look
before implementation starts, not just a review of the code once it's
written — the earlier a wrong table or an incomplete list is caught, the
cheaper it is to fix.
