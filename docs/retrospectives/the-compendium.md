# The Compendium — retrospective

**Merged:** 2026-08-15

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-compendium.md): a second corpus
family scoring the program rather than the world, five verdicts, an anchor
per verdict that the resolver re-checks, and one reading whose headline is
that the first unmet item is a *refusal* rather than a hole.

## 1. This campaign built one resolver and produced four separate false-cleans in it

A false-clean is a guard reporting safety it does not have. Four of them, in
about nine hundred lines of new code whose *entire purpose* is to notice when
a citation stops being true. In the order they were found:

| the defect | why it read as safe | found by |
|---|---|---|
| a mechanism anchor matched a symbol as a **bare substring**, so a citation of `fn foo` resolved clean against `fn foobar` | there were no `present` verdicts yet, so no fixture exercised the path at all | task review |
| the novelty ratchet used `?` **inside a `for` loop**, so the always-present banner line propagated a `None` out of the whole function — the guard could never fire | the function returned a plausible type and its unit tests passed | the implementer, dogfooding its own command |
| the stale-deferral check compared a status cell by **exact equality** to `shipped`, missing `shipped (C1)`, `**shipped**` and arrow transitions — about 19% of all shipped rows | the one fixture row used to derive the rule has the bare word as its status | the controller, preparing the next task |
| a mechanism anchor could cite an **`#[ignore]`d test** — one that never runs — and the raw-text resolver called it resolved | nothing in the design had ever named `#[ignore]` as a state | by accident, while hunting stronger evidence for an unrelated item |

**None was found by a passing test suite, and no two were found by the same
mechanism.** Review caught one. The implementer caught one by using the tool
rather than testing it. The controller caught one while reading ahead. The
fourth surfaced only because somebody went looking for *better* evidence for a
verdict that was already defensible, and tripped over it.

The uncomfortable reading is the fourth column, not the first: four different
detection mechanisms, each of which found exactly one. If any one of the four
had been skipped as redundant, a false-clean would have shipped inside an
instrument whose whole claim is *the anchor is the evidence*. Redundant
detection is not redundant when each detector has a different blind spot.

A fifth, of the same family, was found in **inherited** code and deliberately
not fixed: the heavy-tier gate's ignore-reason scan matches an attribute by
raw text and false-positived on a doc comment in this campaign's new code.
Changing a gate to suit a new caller is backwards, so it is captured as a row
instead.

## 2. Three of the four are the same shape: a fragile text match wearing a different face

Substring versus token. Exact equality versus a normalized token. Present in
the text versus actually compiled and run. Every one is *the same category
error* — treating a syntactic coincidence as a semantic fact — and this
campaign's own code committed it three times before anyone said the sentence
out loud. Writing it down after the first would not obviously have prevented
the second, because each looked like a different problem at the moment it was
made. What might have: a standing question asked of every match this resolver
performs — *what is the cheapest string that satisfies this check while being
the wrong thing?* — applied once per predicate rather than once per campaign.

## 3. The status defect was mine, and its shape is specific

The dispatch that produced the exact-equality bug told the implementer to
reuse the existing registry-ID parser and *"do not invent a second rule"* —
then handed them the **status** rule as my own finding: "status is cell index
2, trim whitespace," derived by running `awk` against exactly one row. That
row's status happens to be the bare word `shipped`. One sample cannot show a
parenthetical qualifier, a bold wrapper, or a transition arrow, and it did
not.

The repository already contained the right answer. `normalize_status`, sitting
in the very drift check the ID rule was borrowed from, is documented as
reducing a status cell to its bare token by stripping emphasis, a trailing
arrow transition, and any trailing parenthetical. The fix was to reuse it
byte-for-byte, which is what the dispatch had demanded for the *other* field.

Two lessons, and the second is the transferable one:

- A rule derived from one instance is an anecdote **even when the instance is
  real**. The awk was correct. The row was correct. The generalization was
  false.
- **Telling someone else to reuse a precedent while not reusing it yourself is
  a distinct failure**, not a mild version of the first one. It arrives
  wearing the authority of the instruction it sits beside, so the implementer
  had every reason to treat the handed-over rule as equally checked. The
  dispatch would have been safer with *no* status rule in it at all.

## 4. Two controller figures were wrong; implementers derived both and corrected them

The plan asserted 75 corpus items. The real figure is **74** — my count
quietly included an unnumbered front-matter page alongside the numbered ones.
The dispatch for the surplus read asserted 19 uncited subsystems. The real
figure is **20** — one cited crate maps to a directory that was never in the
enumerated set, so five directories are cited, not six.

In both cases the implementer derived the number independently, verified it by
hand, reported the correction, and did **not** code to my figure. That is the
behaviour the process wants and it should be recorded as having happened
rather than assumed. It is also the second half of section 3's lesson: a
controller's number carries the same unearned authority a controller's rule
does, and the only thing that reliably strips it is somebody re-deriving it.

Worth noting what made re-derivation cheap: both figures were **countable from
a committed artifact**, so checking cost minutes. A controller figure that can
only be checked by re-running an expensive measurement gets believed by
default.

## 5. The novelty ratchet fired for real, on the campaign's own correction

Re-verdicting one item after review moved the `absent` count from 9 to 10, and
the ratchet went red with the message it was designed to produce. This is the
one guard in the campaign that was observed failing against a *genuine* change
rather than a synthetic fixture, and it happened because the campaign corrected
itself rather than because anyone arranged a demonstration.

Two things follow. The guard's first real firing being self-inflicted is the
best available evidence that it is pointed the right way — the deliberate
rebaseline that followed is exactly the human act it exists to force. And the
correction it caught was a verdict that had been over-generous: one `present`
in twenty-six, found by **internal inconsistency** (the verdict sat directly
above its own note conceding the chapter's real subject was absent, and its
sibling chapter read `absent`) rather than by anyone's opinion about the code.
An instrument that authors verdicts about itself will produce self-flattery;
the useful question is not whether it happens but whether the artifact carries
enough internal structure for a reader to catch it.

## 6. Two Minor findings were ruled into fix rounds, deliberately

Minor findings do not normally reopen a round. Twice they did here, and both
overrides used the same argument: this instrument's entire claim is that *the
anchor is the evidence*, so a probably-right verdict resting on a
not-quite-right citation costs more than the fix does. A file-exists-but-empty
case was promoted for the same reason — an empty in-force set would have made
every decision anchor dangle at once, reading as catastrophic instrument
failure rather than as a missing file.

The general form: **severity should be judged against what the artifact
claims, not against what the code does.** A cosmetic defect in an instrument
that publishes a number is not cosmetic.

## 7. A spec claim about enforcement had to be amended mid-campaign, not inherited

The spec said the ratchet "runs in the gate," written against a single gate
that no longer exists. The first task absorbed sixty-six commits carrying the
gate-ladder split, and under it the local commit gate runs only tests with a
recorded baseline duration — so a *newly written* test is excluded by design
and enters only after a green stage gate measures it.

Inherited unamended, a green local commit gate immediately after writing these
tests would have read as evidence the ratchet worked, when it had not run at
all. The spec gained a section stating which gate actually enforces and from
when. Nothing in the design was damaged; the claim simply had to be corrected
rather than left standing, and the amendment belongs in the spec because that
is where the next reader will look for it.

## 8. What held up

**Freezing the catalogue before authoring any verdict.** The corpus shipped
with every verdict null and its item count asserted, one task before anything
was measured. When the count turned out to be 74 rather than 75, the freeze
was the thing that made changing it a deliberate, visible act instead of a
silent adjustment.

**Refusing to mint a registry row to escape an `absent`.** Twice, an item
could have been upgraded from `absent` to `deferred` by writing a row that
said the project planned the thing. Both were declined with a written reason.
The instrument's `absent` count is its only unfalsifiable verdict and the only
one the ratchet watches; inflating the backlog to improve it would have
corrupted both.

**A fix landed in the task that owned the file, not the task that found it.**
The status defect was found while preparing the verdict-authoring task, and
was fixed by reopening the resolver task with its original implementer, whose
context was intact. Folding a resolver bugfix into the authoring task would
have muddied both diffs and put the fix outside the review that owns that
file — at the cost of one extra cycle, sequenced so two implementers never
held the same file at once.

## Follow-ups

Captured as registry rows rather than folded in:

- **`TOOL-system-probes-are-executable`** — read a verdict off a transcript
  rather than a citation, scoped to the items where a transcript would
  discriminate.
- **`TOOL-surplus-granularity-is-coarse`** — the surplus read counts crates, so
  one citation reads as full coverage.
- **`TOOL-refusal-needs-a-decision-anchor`** — four items read `absent` where
  the project's real position is a refusal recorded in a registry row rather
  than a ratified decision. Both reviewers judged `absent` correct under the
  rules and the gap real.
- **`TOOL-ignore-scanner-matches-prose`** — the inherited fifth false-match of
  section 1, and the mirror of an already-banked false-negative in the same
  scanner.
- **`TOOL-normalize-status-is-tail-blind`** — a status transition's tail is
  invisible to the shared normalizer, so `raw → shipped` would not fire the
  stale-deferral check. No such row exists today; mirroring the precedent
  exactly was the requirement, and diverging would make two consumers disagree
  about one row.
- **`TOOL-mechanism-anchor-matches-prose`** — the raw-text mechanism scan
  accepts a signature that appears in a comment or a string literal; a live
  instance already sits in the tree.
