# Decision ledger — The Wicket (2026-09-01)

Campaign: `campaign/the-wicket` · Registry row: `MAP-one-kind-model` ·
Spec: `docs/superpowers/specs/2026-09-01-the-wicket-design.md`

Autopilot is engaged (`campaign-autopilot`). G1/G2/G4/G5 auto-resolve and
land here; G3 and G6 are hard stops.

---

#1 [Q] — **What is the campaign named?** · Decision: **The Wicket** (a small
gate standing in front of open ground) · Why: `the-orange`, the obvious name,
is already the name of a `repertory/` scene (`repertory/the-orange.scene.json`,
founded by The Repertory, currently UNWITNESSED), and a campaign sharing a
corpus item's slug would make every future grep ambiguous — precedent is the
standing "grep the campaign name before adopting it" rule, applied here and
confirmed by `grep -ril` over `docs/`, `book/`, `repertory/` ·
Alternatives discarded: The Orangery, The Grafting, The Rootstock, The Nursery,
The Hasp (all free; none names the thing being retired as exactly) ·
ideonomy passes / overturns: 0 / 0 — naming is not a design decision and no
pass was run for it · Capture: none needed.

#2 [G1] — **Which of MAP-one-kind-model's three additions does this campaign
ship?** · Decision: **the first only** — retire the closed `AnchorKind` gate
so an object kind is a row rather than a variant; plus the totality mechanism
that replaces the compiler, plus one placed kind the enum could not have
named · Why: the frontier essay's own "order of least regret" states the
sequence (closedness → edges → per-instance derivation) and argues explicitly
against landing all three at once — *"three working mechanisms would be put at
risk to serve two missing ones, and the two are additive on the three."* That
is precedent in an `elaborated`/high registry row and its linked essay ·
Alternatives discarded: (B) bundle kind-to-kind edges into the same campaign —
the ideonomy pass found a real coupling argument for it (see below) but 15
kinds do not yet make hand-authoring expensive, so the coupling is a reason to
sequence edges NEXT, not to bundle them; (C) widen the enum or add an escape
hatch — leaves two kind models, which is the defect itself ·
ideonomy passes / overturns: 1 / 1 (framing overturned — see #3) ·
Capture: the edges/inheritance coupling is recorded in the spec's §8 and will
become a registry row amendment at close.

#3 [G1] — **What is the campaign's deliverable, stated as one thing?** ·
Decision: **registry totality replaces compile-time totality**; deleting the
enum is the consequence, not the headline · Why: the ideonomy `purpose` prompt
separated `AnchorKind`'s two inherited purposes — it is the pattern grammar's
vocabulary AND the guarantee that every placeable kind has a noun, a detail and
a thing-kind. Only the second is load-bearing, and closedness is merely a means
to it. The immunology re-instantiation (germline-encoded receptors vs. V(D)J
recombination, which is survivable only because of negative selection) says the
same thing structurally: an open vocabulary is safe exactly to the degree it has
a default-deny gate. This decides the whole test strategy, so it is the
headline · Alternatives discarded: framing the campaign as a refactor
("s/AnchorKind/KindId/"), which would have shipped the re-key with no
replacement for the three exhaustive matches' guarantee ·
ideonomy passes / overturns: 1 / 1 · Capture: spec §5 is written from this
framing.

#4 [Q] — **Does the campaign have to PLACE a new kind, or is an open
vocabulary enough?** · Decision: **it must place one** ·
Why: decision 0398 (*a capability nothing can reach is not a capability*) is
directly on point and was minted three days ago against this exact shape — The
Chattel shipped a container, a lock, a key and an `open`/`close` pair that no
session in any world could stand in front of · Alternatives discarded: prove
openness with a test-only kind (cheap, moves no artifact, and is precisely the
"capability nothing reaches" 0398 refuses) ·
ideonomy passes / overturns: 1 / 0 (the faceted-classification
re-instantiation independently reached the same place: a faceted catalogue can
compose infinitely and still has to shelve the book) ·
Capture: spec §6; the artifact cost is a G3 flagged item.

#5 [G2] — **Do the frontier essay's measured numbers survive checking?** ·
Decision: **no — they are wrong and the campaign corrects them** ·
Why: verified by command, not by reasoning. `AnchorKind` has **15** variants,
not thirty-four, and had 15 at `208efe4f1`, the essay's own commit; there are
**3** exhaustive match sites (`chamber_prose::noun`, `chamber_prose::detail`,
`affordance::thing_kind_of`), not thirty-six; "seventeen files" is 18 source
files / 22 including tests. The fence is ~2.3x smaller than both the essay and
the `MAP-one-kind-model` row advertise · Alternatives discarded: quietly using
the corrected numbers without amending the book — a published essay stating a
false measured fact produces wrong cost estimates from readers acting in good
faith, which is the failure mode CLAUDE.md's own loud-correction paragraph
exists for · ideonomy passes / overturns: 0 / 0 — a measurement, not a design
decision; no pass was run for it · Capture: the essay and the registry row are
both amended as part of the DoD sweep (spec §11).

#6 [Q] — **Is `Sleep` actually gated on `SupportsRest`?** · Decision: **no —
and three doc comments say it is** · Why: read the code rather than the
comments. `Session::sleep` (`session.rs:2536`) refuses a non-empty argument,
charges the clock, commits `rested` and sets `wake_at`; there is no bed check
and no home check. `Session::warm` (`session.rs:2624`) DOES gate, on
`offered_to_observer`, which is what a gated verb looks like here. The creature
layer already agrees with the ungated reading and says so in its own doc
(`liveness.rs:2293`, *"sleeps where it is — its proposal is always `Rest`"*),
and it already reserves a hook for a rest-QUALITY refinement. So
`SupportsRest` reaches only the advertisement layer · Alternatives discarded:
trusting `OfferedVerb::Sleep`'s doc ("gates on `SupportsRest`") and
`Action::Rest`'s doc ("precondition: at home"), which is what an earlier draft
of the spec did — it justified the `kneeler` proof kind out of them and asserted
"a shrine offers nowhere to rest", which is false ·
ideonomy passes / overturns: 0 / 0 — a measurement · Capture: the three
corrections are Task 4's, spec §6a.

#7 [Q] — **Nathan's ruling: sleep is never gated; the place GRADES it.**
Raised by Nathan directly, 2026-09-01, so this is his call and not an
auto-resolved gate. Decision: **the campaign leaves the space and does not
build the mechanism** · Why: the space is free — after this campaign a kind is
a `KindId` with open tables behind it, so a `ComponentStore<KindId, RestQuality>`
is a new table and nothing else — while building the grade would pull in a
`(species, thing)` relation, the drive's site-selection, and a census readout,
which is a campaign · Alternatives discarded: (i) build the grade now (three
subsystems, and it would put the re-key at risk for a mechanism that is additive
on it — the essay's own argument); (ii) note it and change nothing, rejected
because the three false comments actively mislead, as demonstrated by this
campaign's own first draft · ideonomy passes / overturns: 0 / 0 — Nathan's
ruling is the input, and the design question it left (what "leaving space" means
concretely) was answered from the campaign's own thesis rather than by
expansion · Capture: `PSY-rest-quality-is-a-grade-not-a-gate` and
`PSY-rest-site-is-a-tuning-indicator` written to the idea registry; spec §6a;
a decision record at close.

#8 [G1] — **Which kind proves the vocabulary is open?** · Decision: **a
`brazier` carrying `RadiatesHeat`, replacing the `kneeler`** · Why: #6 removed
the kneeler's justification — `SupportsRest` advertises and does not grant, so a
kneeler would have added a listing rather than a capability. `warm` IS enforced
(`session.rs:2624`), `hearth` is its only carrier, and a hearth is confined to a
hearthroom by `the-fire` requires `Alcove` requires `roles: &[Role::Hearthroom]`
— so no shrine, hall, smithy or storeroom can be warmed at. `Session::warm`'s
own doc names the anticipated future carrier, *"a cauldron of coals on
`AnchorKind::Vessel`, say"*, and says such a carrier would have needed a
dispatcher edit before the fix that landed with it — so the brazier arrives to
find the edit already unnecessary, which is the campaign's claim demonstrated
rather than asserted · Alternatives discarded: the kneeler (#6); a wilderness
carrier, which closes a bigger gap but redraws every outdoor interior in every
world (`MAP-wilderness-affords-no-rest`) · ideonomy passes / overturns: 1 / 1 —
the pass that overturned the kneeler was the one that asked which verbs are
enforced rather than which are advertised · Capture: spec §6.
