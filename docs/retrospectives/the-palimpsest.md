# The Palimpsest — retrospective

**Merged:** 2026-08-17 · **Program:** Myth, campaign 3 of 4

## Seven defects, all in controller text, none surviving in implementer code

The ratio held for the third campaign running: every defect that reached an
implementer originated in spec or plan prose I wrote, and none originated in
the code written against it. Two reserved-keyword and lint collisions in
sample code; a test suite in which teller and hearer were always the same
people, so the design's one load-bearing choice was pinned by nothing; a
tie-break branch that no fixture could reach; a control blind to the failure it
named; and the unit mismatch below. **Every one was caught by running
something. None was caught by reading.**

The half that is not a lament: four were caught by verification run one task
ahead of dispatch, against live source rather than against the plan, and two of
those were amended in the plan text before the next task read it — so a defect
that would have recurred twice recurred zero times.

## Three designs died to measurement, and a fourth should have

Each was proposed first and measured second, which is the wrong order and is
also the only reason they died at all.

1. **Teller-history-alone.** Killed by argument — it never consults the claim,
   so it cannot be distinguished from a per-community noise knob. Later
   confirmed numerically at 51.6% firing, near a coin flip.
2. **Distance-from-event in days.** Off scale by 25× on a single step against a
   ladder topping out at one year. Proposed, approved, and dead inside one
   measurement.
3. **A ladder of natural durations under an accumulating rule.** Transmission
   chains span 8–13 generations; the longest natural duration the world
   contains is a lifespan, at 2.2.

The standing rule this campaign adopted from those three — *measure a candidate
quantity's scale against the scale it must be commensurate with, before writing
it into a design* — is exactly the rule its own worst defect broke.

## The seventh defect is the campaign's real output

The frozen model seeds an accumulated width in **standard days**, increments it
by an amplitude that is a **dimensionless count of generations**, and compares
the sum against rung spans in **days**. The design defines the amplitude in one
unit in one section and the ladder in another unit two sections later, compares
them in a third, and never states a conversion anywhere. The implementation is
faithful to the text.

Two things about it are worth carrying:

- **It did not make the readout wrong; it made two of three rules
  untestable.** Their saturated fraction of exactly 0.0000 on all forty seeds
  is a fact about units and not about worlds — the predicted saturation was
  never put under test at all. That is a worse outcome than a falsification,
  because a falsification is information.
- **It was found only at readout**, by an implementer reading a constant it did
  not trust, after four reviews had passed over the same arithmetic.

Nathan's call was to report both: the preregistered numbers stand as measured,
and a post-hoc exploratory re-measurement is reported beside them, labelled at
four independent levels. Nothing frozen was touched, which was verified rather
than asserted — the four files the frozen readout measured are still
last-touched by their own task commits.

## A control that could not fail, inside a fix for a control that could not fail

The campaign's own theme landed on me at third order and I did not see it.

A finding said 77 hand-copied lines of the exploratory walk were executed by
nothing. I specified the remedy: on a fixture where every generation length is
1.0 standard day the conversion is the identity, so the local walk must return
a claim-for-claim identical result to the frozen one. True — and **at a
generation length of 1.0 the multiply *is* the identity, so deleting the
conversion is a no-op on that fixture and the equivalence still holds.** The
control I wrote to catch a forgotten conversion was structurally incapable of
catching one, and I wrote it as the fix for a finding about a check that could
not fail.

The implementer caught it and built two controls instead — g = 1.0 pins the
transcription, g = 50 pins the conversion — with mutation evidence in both
directions: dropping the multiply reddens the second only, dropping a path
reversal reddens the first only. That is strictly better than what I asked for.

## Two controls that earned their cost, one of them against me

- **A positive control went red against my own structural argument.** An
  earlier draft asserted that crossing social stance twice was structurally
  impossible. The probe's control asserted a maximum of 1 and measured 2 — the
  attacker can sit inside a witness's own subtree. The argument was persuasive
  and wrong, and only a control that asserted the *strong* form found out.
- **A predicted second ceiling does not exist.** Retention keeps the
  least-corrupted route per holder, which could in principle mask compounding.
  Measured: for every candidate axis the retained maximum equals the all-paths
  maximum. A predicted mechanism measured and found absent is worth the same
  as one found present, and costs a paragraph either way.

## An implementer reported a surviving mutant rather than hiding it

A tie-break arm (`<=` → `<`) left both new controls green. The implementer
reported it, argued it was an equivalent mutant, and flagged the argument as
unproven rather than presenting it as a clearance. It was later confirmed
equivalent from the lineage structure: ancestry is a unique upward walk on a
single-parent tree, so competing candidates for one descendant always come from
different witnesses with different identifiers, and exact key equality between
distinct entries cannot arise. No counterexample exists to construct.

Reporting an inconvenient survivor is the behaviour that makes mutation
evidence worth anything, and it should be said out loud when it happens.

## The contaminated freeze — how it actually went

Campaign 2 asked that this campaign's preregistration be frozen by someone who
had not read its results. Nathan was asked, and accepted the contamination
explicitly. The honest report:

**The mechanism held.** The binding consequence written into the freeze — no
campaign-2 figure may be used as a threshold anywhere — was respected
throughout, and it mattered in exactly the place it was written for. H3 came in
at +0.03 to +0.05 against the previous campaign's 0.662, and an author anchored
on that number would have been under real pressure to call the result a failure
or to go looking for a rule that recovered it. The direction-only framing,
frozen in advance, made that a reportable weakening instead of a crisis.

**It cost the instrument sharpness, and that cost is the finding.** Because no
bar could be quoted, H2 and H3 both became direction tests. So this campaign
can say its structural predictor of divergence is still positive, and cannot
say whether it is *worse* than its predecessor's in any way it has licensed
itself to assert. Contamination did not corrupt the result; it lowered the
resolution of the question, in advance, deliberately, as the price of
disclosure.

**And it aimed the campaign.** H2 exists because the one-rung ceiling was
known. That is contamination doing what contamination does — directing
attention — and it is not obviously bad. What it did not protect against is
the defect that actually cost the campaign a hypothesis, which came from my
own text and had nothing to do with what I had read.

## Process notes

- **The panel size was measured, not guessed.** A 5-seed pilot cost 12.40 s of
  test time against a 2-minute decision bar, so 40 seeds were selected; the
  full battery then measured 132.55 s and 106.63 s, and the exploratory one
  159.91 s. The decision rule was written before the pilot ran.
- **The readout is a heavy battery, not registered census metrics**, found at
  preflight. Registration is not a local act: nine studies declare every metric
  with no opt-out, so a metric added here would run on ~2,000 census worlds
  forever and restage fixtures no drift check covers. Campaign 2's readout is a
  heavy battery for the same reason.
- **A shared campaign workspace collision cost about four turns and mimicked a
  code defect** — the symptom looked like a source problem and was not. Filed
  to the board as `c1cc2209` so the next session recognises the shape rather
  than re-diagnosing it.
- **A returned commit identifier was transcribed with 39 hex characters**, one
  digit short. Verifying the object rather than echoing the string caught it
  immediately. Harmless here; it is precisely the failure that rule exists for.
- **No absorption was needed to protect the figures.** Main moved 67 commits
  during the campaign, none of them touching the kernel, any domain, the
  composition root or this window (`git diff --quiet 1e92c152 c25cf53a --
  kernel/ domains/ windows/worldgen/ windows/hearsay/` is clean) — so unlike
  campaign 2, no published figure had to be restated. That was checked, not
  assumed.

## Deferred, with homes

- **The unit fix is one multiplication**, and it belongs to campaign 4 with its
  own preregistration, frozen by someone who has not read the exploratory
  column.
- **Multiplicative now has no corrected counterpart at all.** Correctly
  labelled, its exploratory row is a third model rather than a correction, so
  the campaign ships a corrected reading for two rules and none for the third.
- **The frozen and exploratory batteries duplicate five helpers** deliberately,
  because sharing them would mean editing the file the freeze protects. If
  campaign 4 lands the conversion in the library, the two collapse into one.
- **Both readouts live only in the heavy tier**, so neither set of numbers is
  re-derived by anything that runs automatically. They are pasted into the
  chronicle rather than cited by test name for that reason.
