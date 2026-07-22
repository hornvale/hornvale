# Retrospective — The Sundering (The Moving Sea)

*Living-community program, Campaign 2 slice 2. Process lessons, not product.*

## What went right

- **The preregistered gate caught the fidelity risk it was written for.** The
  first design (a static present-coastline graph, ocean an absolute block) hit
  the spec's own §7 depopulation gate on the real world — migration collapsed
  51→1. The measure-don't-narrate machinery worked exactly as designed: the
  implementer refused to re-pin a floored value, returned BLOCKED, and the
  finding went to Nathan instead of being buried. The G1 ideonomy pass had
  *predicted* this exact polarity-flip risk. The gate existing is why the
  campaign pivoted instead of shipping a broken diaspora.

- **Ideonomy found the real answer, and the substrate was already half-built.**
  Three ideonomy passes on "a time-varying sea" converged on the space×time
  principle (the graph was the one substrate frozen in time) — and the check
  that grounded it found `EraClimate.sea_level` already computed per era, down
  to −120 m. The best long-term fix was also the *cheaper* fix, because the
  world already carried the data. Running the passes on Nathan's explicit
  "what's the best long-term solution?" turned a patch into a principled build.

- **Diagnose before tuning.** When the moving sea still under-delivered (12
  migrations, not the hoped-for recovery), the discipline was to run *three*
  measurements before proposing any lever — reachability (bridges reach
  755/843 refugia), the full census (11/12 migrations ride the bridges; 6011
  of 6024 steps are peaceful growth), and the forcing constants (−42 °C, already
  severe). Each overturned a plausible hypothesis. The bottleneck turned out to
  be settlement×climate coupling + zero crowding — a *pressure* problem, C3's
  domain. Without the measurements we would have "tuned" the graph or the
  glacials, both wrong.

## What to carry forward

- **Re-scope vs floor is a real distinction, and it needs transparency.**
  Lowering a measure-don't-narrate floor is legitimate when the phenomenon
  still fires *and* the old baseline was unphysical (here: ocean-walking
  inflated 51). It is flooring-a-regression when the phenomenon went inert. The
  guard that kept us honest: every re-pin carries an in-source comment stating
  *why*, the spec records the measured outcome (§10), and the campaign's
  headline moved to the gate that fires robustly (isolation-divergence). Write
  the reasoning down at the re-pin, not at the close.

- **A finding can hand the next campaign its thesis.** The Sundering did not
  fail to produce a rich diaspora; it *located* where the volume lives
  (crowding/pressure) and handed it to C3 with the measurements attached. A
  campaign that ships a correct substrate and a sharp question for its
  successor is a good outcome, not a shortfall — but only because the question
  is backed by numbers, not narrated.

## What to do differently

- **Stage-boundary absorption was missed.** This branch's first meeting with
  main was at the close — 38 commits, two campaigns (the-eremite, PROC-19). The
  merge was clean and the gates held (the-eremite's dragons are solitary, off
  the peopled roster; PROC-19's `stream_labels!` didn't touch the bake), so no
  harm landed — but that was luck, not cadence. The CLAUDE.md stage-boundary
  absorption discipline exists precisely so a 38-commit merge is never the first
  integration. Absorb at each plan-stage boundary next time.

- **Don't background a long command with a nested `&` inside a tracked run.**
  `nohup make gate & ` inside a `run_in_background` bash returned the harness's
  "exit 0" for the `echo`, not the gate — which was still running. It cost a
  confused poll cycle. Let the harness track the long command directly.
