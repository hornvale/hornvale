# The Occlusion — retrospective

Process lessons, not product. The campaign fixed four presentation defects;
what follows is what the *doing* taught.

## The finding that only running the system could produce

Every one of this campaign's four defects was found by building the CLI and
using it — not by reading code, and not by grepping. `possess` printing
`Ways on: SE, N, SW.` and then refusing `se` is invisible in the source,
because both halves are correct in isolation: `parse_compass` accepts the
token, and the dispatch arm that would reach it is simply absent. You see it
in one second at a prompt and never in a diff.

The codebase is unusually clean by static measures — zero TODOs across 110k
lines of Rust, a default-deny type audit, a 2,319-test gate. None of that
surfaces "the almanac contradicts itself in its first sentence."

**Lesson:** for a project whose deliverable is *prose about a world*, a
periodic session of simply reading the output is a distinct and irreplaceable
form of review. Consider it a standing pre-merge step, not an ad-hoc one.

## The spec was verified and still wrong

The spec claimed phenomena are a read, not committed facts, and therefore owed
no epoch. That claim *was* checked — `SkyReport` carries no `Serialize`, which
was confirmed by reading the derive rather than assuming it. Autopilot's
verify-your-claims discipline was followed and produced a true statement.

It was the wrong statement. The determinism exposure did not run through
serialization; it ran through genesis, where `derived-from-phenomenon` is a
committed predicate. The verification answered the question that was asked
instead of the question that mattered.

**Lesson:** when a determinism claim is "X cannot affect the save format",
verifying one route is not verifying the claim. The check that would have
caught it is the cheap and total one: build the world before and after, and
`cmp`. That comparison — which the plan happened to require for other reasons
— is what actually caught it, three tasks later.

**Concrete practice:** any campaign touching a path that genesis reads should
`cmp` a seed-42 world as its *first* implementation step, not as a late test.
Cost: about ninety seconds.

## The plan's fixture step paid for the whole plan

The byte-identity fixture in Task 3 existed because the spec's self-review
flagged a gap — the spec promised a world-level test and no task had one. That
step is the only reason the pantheon loss was caught before merge. Without it
the campaign would have shipped a silent world-rewrite disguised as a prose
fix, and it would have looked entirely green: the gate passes, because the
gate pins facts *relative to the current build*, not to history.

**Lesson:** the self-review step in `writing-plans` earned its keep in a way
that is hard to see from inside it. It found one gap; that gap was the
campaign's only serious bug.

## An old test did half the work

`goblin_observation_reproduces_the_unlensed_path_bytewise`, written by an
earlier campaign, stayed red after the genesis path was fixed. That
stubbornness is what forced the principled presentation/primitive split rather
than a patch on the symptom. A test whose failure *survives* your first fix is
giving you information about the shape of the problem, not just its presence.

**Lesson:** when a fix makes one failure go away and another persist, read the
persistent one as a design critique before treating it as a second bug.

## Plan-authored code is uncompiled

Three snippets in the plan were wrong in ways only the compiler could find:
`bare-ok(threshold: ...)` is not a ratified type-audit class (the in-repo
example it was modelled on, `REGIME_FLOOR_MM`, gets away with it only by being
private); the test helpers `test_session()`/`feed()` do not exist, the real
ones being `seam_world()`/`Session::start`/`handle`; and a multi-line
`type-audit:` tag is silently malformed.

This is the fourth campaign to record this lesson, which suggests recording it
again is not the fix. The plan already said "use whatever helpers the file
defines" — and that instruction worked. The failures were all in snippets that
did *not* carry such a hedge.

**Lesson:** in a plan, either ground a snippet on a real, cited `file:line`, or
explicitly mark it as shape-not-substance. An unhedged invented identifier is
the failure mode; prose accuracy is not enough.

## Ideonomy earned a specific, nameable thing

Two passes ran. The reframe that made the campaign tractable — "the codebase
already has one occluder and never named it" — came from the cross-domain
re-instantiation (atmospheric opacity windows in radio astronomy), which also
overturned the initial binary-gating design in favour of graded attenuation
plus a floor. The homogeneity dimension in the second pass is the *only*
reason the strange-site listing carries a descriptor column, and that column
immediately exposed the 99-of-101-fungal defect.

Worth noting because the payoff was concrete rather than atmospheric: a
required column revealed a real bug within one minute of first running the
command it belonged to.

## Follow-ups

Promoted from `.superpowers/sdd/followups.md`:

- **99 of 101 exotic sites are fungal** (seed 42); 91 in cold biomes. Cause is
  diagnosed: the candidate scores in `windows/locale/src/budget.rs` are not
  commensurable — fungal scores `1.0 - unrest` (≥0.6 on any quiet land),
  geothermal scores bare `unrest` (high only near plate boundaries). Small
  change, large variety payoff.
- **Marine biomes have no prose.** `variety_pool` covers 0 of 10 marine
  biomes, and `micro_habitat` applies land clauses unconditionally, so the
  bathypelagic reads "broken terrain sun-warmed **dry** on a rise". Roughly
  70% of the planet. Its own campaign.
- **Settlement names are unpronounceable** (`Bzhobkobzhshashzhzash`).
  Unbounded length plus unrestricted onset clusters; touches language-domain
  seed derivation, so it needs epoch care.
- **"Visible bodies" is not occluded.** The almanac still prints `Visible
  bodies: the sun, moon 1, moon 2` under a flat overcast, and lists the sun at
  night. Either occlude it or rename the label.
- **Cloud-base altitude** — an observer above the deck should see a clear sky.
  Registered in the idea registry.
- **Moonlight as an occluder** — a full moon washing out faint stars, which
  astronomy could own with no weather knowledge at all. Idea registry.
- **A `map`-verb strangeness gradient**, dropped from this campaign's plan
  under the spec's own escape hatch once `map` proved to be a substantial
  chart renderer rather than a wiring job.
