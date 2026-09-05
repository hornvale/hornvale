# The Counterpart — testing concurrent changes against an independent obligation

Status: **G3 approved by Nathan on 2026-09-05; implementation planning authorized.**
Date: 2026-09-05. Branch: `codex/federation-next`.
Research base: `4f303d3b06d8403f79324755c6adfac9d0394a3d`.
Program: [federated development](2026-09-04-federated-development-program-metaplan-design.md).
Decision trail, Ideonomy organons, rejected branches and current-state receipts:
[Counterpart ledger](../ledgers/2026-09-05-the-counterpart.md).

## 1. Recommended outcome

Produce an independently challenged, repeatable experiment showing where two
owners can change code separately and where their shared meaning requires
coordination. Use the real **Thing–Settlement concept-registration boundary**.
The experiment compares the same frozen consumer obligations on a base, each
change alone, and their actual composition.

Its useful output is a concrete answer to “which owner needs to check what
when this boundary changes?”, with counterexamples and costs. A compact
comparison of three ways to identify affected obligations—declared context
paths, Cargo dependencies, and one explicit semantic agreement—runs in
**shadow mode**: all frozen obligations still execute on every valid arm.
No proposed selection skips an accepted test.

This is a bounded experiment with reusable fixtures and a small runner/report,
not an enactment of a federation framework. The code that ships supports the
experiment. Deliberately changed simulation sources remain disposable
specimens, retained as reconstructible inputs; they are never included in
the production campaign's source or goldens.

**Primary hypothesis:** an independently grounded consumer agreement can
expose a real interaction between separately acceptable changes, while
allowing a demonstrated noninterfering pair to proceed without a shared
semantic implementation edit.

**Comparative hypothesis:** a small owner-authored agreement identifies a
useful affected obligation that the cheaper path/build comparators miss, or
avoids unnecessary work without missing a known violation. This may be false.
Detecting interference and improving obligation selection are separate results.

Success qualifies only the named finite experiment. It neither proves
arbitrary semantic independence nor grants narrower admission.

## 2. What current evidence establishes

| Fact | Evidence and limit |
| --- | --- |
| The Charter is still current remote main | Fetch and queue read returned `4f303d3b06d8403f79324755c6adfac9d0394a3d`; the queue reports all merge phases rc=0, 1653 s. This is a timestamped research baseline, not a promise main will stay there. |
| Enrollment worked without contributor-specific host edits | [Charter evidence](../../digest/the-charter-evidence.md#sources-and-independent-enrollment) records separate adopter packages and shared Cargo.lock reconciliation: 22 external records preserved, 27 local records added. |
| Path scopes are not affected-consumer closure | Thing's [manifest](../../../tools/digest/packages/thing/Cargo.toml) declares only `domains/thing`, while [its collector](../../../tools/digest/packages/thing/src/lib.rs) calls Settlement registration. Actual `digest context domains/settlement` returned exit 1, “no contributors match scope”. This is correct under Charter's narrower contract. |
| A real shared namespace exists | `domains/thing/src/lib.rs` declares `BORROWED = &[("hearth", "settlement")]` and checks declared/undeclared ownership during registration. `domains/settlement/src/lib.rs::register_concepts` supplies the lender. No new production interface is needed merely to observe this boundary. |
| Structure and correlated agreement are insufficient | [Charter retrospective](../../retrospectives/the-charter.md) records a compiling Thing mutant and census recognition mutants that initially escaped the panel. A compiled declaration and generated prose can share the same error. |
| Stronger tools have real costs | [Recorded measurements](../../digest/the-charter-evidence.md#measurement-method-and-all-samples) include census-context selected cold requests of 11.552 s Mac / 25.712 s Linux. These measured observer costs are not census costs or a causal explanation of compilation overhead. |
| A declaration has already misdirected integration | Correction `c610c9363fd9530c68627576d240e0e44ba68b74`, already in this base, repaired hand-authored reconciliation data classified as generated. The Warp's recorded stage failure was in the merge product. Do not duplicate that landed fix. |

The Charter's 72 command samples and ten warm requests per workload/host
established bounded baselines, no SLO. Its reports identify revision plus dirty
state, without atomic snapshot or complete provenance. This experiment must
not promote those reports into reusable evidence.

The subsequent `tooling/census-study-split` census is reported green/null
by the queue. That instrumentation does not establish a cause for Charter's
1070.850 s census. Its yellow entry remains **NOT PROFILED**; Nathan owns
the investigation and authorized the deferral. No profiling work is included.

## 3. Competing approaches and why this one comes next

| Approach | Gap it would close | What remains; concrete falsifier |
| --- | --- | --- |
| **Counterpart: independent interference experiment** | Distinguishes file separation, hidden consumer dependency, and joint-only interaction; challenges declarations against behavior. | Does not solve builds, generic provenance or compatibility migration. Falsified if a valid joint-only example cannot be established, or a frozen challenger misses an independently chosen violation. |
| **Build islands** | Shrinks the census observer's actual owning API/build closure. | Semantic interference remains. Falsified if an unrelated lab edit still rebuilds the observer, production guard behavior changes, or composed preparation cost cancels the local saving. Extraction must not duplicate the guard as a shortcut. |
| **Replayable evidence first** | Retains the source, toolchain, inputs and results of a bounded observation. | Repetition can reproduce the same mistake. Falsified if an independent reader needs an unrecorded input or a material change leaves a result apparently valid. A universal dirty-tree provenance platform is a much larger claim. |
| **Compatible migration and retirement** | Tests two actual owners adopting an incompatible agreement without a flag day. | Needs a real useful amendment. Falsified by silent old-reader loss, forced simultaneous migration, or an unmigrated consumer disappearing at retirement. Inventing a production break merely to run this exercise has no demonstrated benefit. |
| **Persistent prompts, Book views and idea lifecycle** | Makes scoped checked knowledge easier to discover and maintain. | Generated clarity can amplify shared error. Falsified when an unfamiliar reader misattributes authority or a generated statement has no owned source; useful reader performance matters more than word count. |
| **Formal authority model / resource experiment** | A small model can enumerate modeled migration states; concurrent invocation tests can qualify resource isolation. | Neither establishes semantic truth. Falsifiers: a live transition absent from the model, a safe model that permits no useful progress, cancellation crossing invocation ownership, or aggregate costs overwhelming local limits. These are distinct campaigns. |

This refines the metaplan's **order**, not its constitutional constraints:
test the behavioral value of one agreement before building a general agreement
transport, evidence graph or activation scheme. The next campaign may reasonably
be build isolation if measured coordination benefit is absent here.

All alternatives remain in their existing registry homes. A row's status is
not evidence and no broader federation row becomes “shipped” by this experiment.

## 4. One boundary and one fixed question roster

The primary boundary is ownership in the ConceptRegistry produced by
Settlement registration followed by Thing registration. Scope includes:

- unique concept ownership, explicit borrowing, missing lender and wrong owner;
- roster/registration agreement in both directions;
- the negative assumption that an independently added name has no conflicting
  owner elsewhere in the composed registry.

Use public production APIs. No domain acquires a sibling-domain dependency:
development-side composition remains outboard, as in The Charter.

Freeze the finite question roster and its intended outcomes before authors
finish the specimen changes. A checker independently derives answers from the
accepted ownership contract and observable registry behavior. It may share the
roster of questions, fixture inputs and public data types; it may not call the
Charter contributor's verdict function or obtain its expected answer from the
candidate's declaration. Record every shared input/derivation so independence
can be reviewed rather than inferred from a different filename or author.

This follows decisions [0094](../../decisions/0094-a-deliberate-duplicate-shares-its-roster-never-its-derivation.md)
and [0261](../../decisions/0261-a-rule-duplicated-on-purpose-carries-a-two-way-agreement-test.md).
The question roster is a deliberate small coordination point. Its sufficiency
is still judgment, including whether an absent question hides a failure.

The production guard already rejects undeclared ownership collisions, and
Charter captures registration refusal. At the research base, the existing
`cargo test -p hornvale-thing --lib undeclared_collision_panics_instead_of_silently_ceding`
completed with one test passed. Its fixture uses a stand-in owner; this does
not qualify the proposed two-change specimen. Report existing detection,
independent corroboration and earlier obligation selection separately. The
campaign must not claim to have discovered a detector that already exists.

Each owner supplies one bounded record: owned edit scope, supplied/consumed
meaning, positive and negative assumptions, affected question IDs, known
unknowns, and source authority. These are **experiment data**, independently
authored before integration. Do not extend Digest protocol v1, add a universal
effects ontology, or make declaration text an executable authority.

The negative-name assumption is especially useful: a list of today's known
borrowed names alone cannot represent “a new name must not collide”.
The experiment must distinguish those two declarations.

## 5. The experiment and its counterexamples

For each pair use one fixed base and one fixed obligation set `O`:

```
base       -> O(base)
base + A   -> O(A)
base + B   -> O(B)
compose    -> O(A+B)
```

A and B are independently authored patches with disjoint production-source
writes. Shared lock/dependency resolution is recorded separately. If textual
composition needs a human semantic resolution, record that case as such;
do not quietly edit it until it resembles an independent composition.

The specimen population is small and preregistered by **property**, not a
guessed line substitution:

| Specimen class | Discriminating requirement |
| --- | --- |
| Safe pair | Base, both solo arms and composition satisfy the same obligations. Both edits must observably change the intended registry behavior; two no-op edits are invalid. |
| Existing dependency failure | Base passes and one solo change violates a frozen consumer obligation outside its authored path scope. Report this separately from interaction. |
| Joint-only interaction | Base and both solo changes satisfy the same obligations; the actual composition violates one. A newly claimed name colliding across independently changed owners is a candidate to investigate, not a pre-certified fixture. |
| Missing negative assumption | A bounded declaration listing known dependencies misses a genuinely new collision/claim; the independent obligation still asks the question. |
| Correlated wrong answer | A candidate declaration/check can move together while an independently grounded obligation objects. An always-satisfied declaration is not sufficient by itself: a behavioral violation must exist. |
| Correct unusual case | A legitimate explicit borrowing/ownership arrangement is accepted. Over-refusal must remain observable. |

These classes may share input specimens where the distinct property and
denominator remain explicit. No target count is a correctness proxy.

The challenger reserves at least one independently selected compiling
counterexample until owner records, comparison rules and the challenger
implementation are frozen by identity. Freeze and record its inputs before
scoring; do not retune on that challenge and
then count the corrected run as the original success. A mismatch may be a
challenger defect. Adjudicate against the accepted contract and raw behavior,
retaining the original failure and any later correction.

No blind-review or security claim follows: agents share a filesystem.
Use task-context separation and commit chronology to establish process
independence; record disclosures or accidental exposure.

**Feasibility rule:** if suitable useful non-no-op specimens cannot be
constructed on the real API without expanding production semantics or
turning the entire study into a toy, report the limitation and reopen the
design. Do not invent a passing score. Synthetic faults in real APIs demonstrate
mechanisms; they do not establish natural fault frequency or production
throughput. Any later historical replay must be labeled separately.

## 6. Compare three obligation suggestions, execute all checks

Freeze a common scoring map before any challenge is revealed: each question
ID in `O` maps to its contributor, relevant Cargo package(s), input subjects
and evidence locators. The mapping rule covers additions, deletions and the
composed state; unresolved or newly unmapped subjects produce unknown. A
selected contributor/package selects all question IDs mapped to it. This
prevents comparing contributor counts with package counts or question counts,
or changing the bridge after seeing which comparator would win.

1. **Path comparator:** reproduce the existing context scope relation over
   the changed paths. Preserve “no enrolled context” as absent/unknown,
   never “nothing needs checking”.
2. **Build comparator:** derive the relevant Cargo dependency reachability
   at each source state. State exactly how package/file membership is mapped.
   A non-Cargo input or unresolved mapping is unknown. This is a comparator,
   not a claim Cargo knows semantic or runtime inputs.
3. **Agreement comparator:** use the frozen owner records, including the
   named negative assumption, to propose affected question IDs.

Every comparator emits its proposed set, explicit unknowns, and an effective
set after fallback. Any unresolved input mapping or no-enrollment result
falls back to all of `O`; a comparator cannot earn apparent savings by declining
to answer. Report raw omissions and fallback frequency separately from effective
coverage. All members of `O` execute regardless. On the finite panel report:

- missed violating obligations, selected checks with no observed outcome
  change, and effective coverage after fallback;
- which input change reopened each obligation and whether that reason was
  available before inspecting the final behavioral result;
- when the proposed answer required manual integration analysis;
- authored/shared files and reconciliation effort;
- checking and preparation cost, separated from queue wait and author effort.

For every arm, show selected-violating, selected-satisfied,
unselected-violating and unknown separately, before and after fallback.
The cost comparison includes fallback and evaluates the same frozen mapping
for all three comparators. Any claim of unnecessary work requires a separately
frozen relevance argument; satisfaction alone is insufficient.

An unchanged outcome does not establish that a check was unnecessary, even
on this panel. It describes the observed selection cost. A build dependency
can exist without a semantic effect; a semantic input can exist outside a
build graph. An unselected passing
check is not proof it will always pass.

No general automatic selector is installed. If the agreement always requests
the same work as Cargo or all checks, record zero selection benefit. If it
misses a counterexample, record the false negative and retain broader
verification. This comparison may recommend deleting the experimental
declaration mechanism rather than extending it.

## 7. Retained inputs and repeatability

Use immutable committed source identities for base/A/B/composition. Preserve
patches or Git objects needed to reconstruct every specimen after its
temporary worktree is gone. Retain a compact machine-readable dossier with:

- exact subject, checker, comparison-rule and question-roster identities;
- per-arm source identity, dependency locks, toolchain and feature/profile
  configuration, invocation arguments, material environment and host;
- owner records, positive/negative input assumptions, excluded inputs and
  unresolved input closure;
- raw outcomes, stdout/stderr, exit status, interruption/timeout/cleanup
  status, hashes and sizes, and preparation/execution timing;
- provenance of any manual conflict resolution or correction.

Store experiment fixtures and bounded records under
`tools/digest/experiments/the-counterpart/`; link the interpreted findings
from the durable campaign ledger and eventual chronicle. This is experiment
evidence, not an input accepted by the live `context` command. Keep raw
evidence distinct from generated presentation; inventory every writer.
No new report is silently placed under a directory whose inherited generated
authority would discard hand-authored material during integration.

Another agent reconstructs and runs the accepted specimen panel from a
separate owned directory without the originating task's scratch. That
demonstrates bounded repeatability, not complete environmental closure,
atomic capture of a live dirty tree, or portable verdict reuse.

Concurrent edits during a specimen run invalidate that attempt. Use an owned
immutable-input run discipline; before/after hashes alone cannot exclude
A→B→A reads. Preserve failed attempts. Uncertain cleanup retains the owned
directory and available logs and prevents later sampling from hiding the
failure. Reuse Charter's reviewed process/diagnostic conventions where they
fit; do not copy an unrelated process supervisor.

Proposed dossier schema is development-only. It carries no simulation data
contract, epoch change or new Digest protocol version.

## 8. What is independent and what remains sequential

| Work | Placement |
| --- | --- |
| Choose the boundary, freeze meanings/questions and comparison rules | Sequential agreement under the accepted contract |
| Author A and B and their bounded owner records | Independent worktrees after that freeze |
| Derive the challenge and preserve its reserved case | Separate author/context; shared question roster, independently derived answers |
| Prepare disjoint inputs and inspect source | Concurrent where resource rules allow |
| Run the experiment | Existing local cost rules; all minutes-scale/heavy canonical work takes the ordinary serial claim |
| Construct and inspect the actual composition | Sequential integration; no silent semantic conflict repair |
| Interpret disagreements and decide which mechanism earned value | Sequential review with raw evidence |
| Admit the production campaign | Existing commit/stage/merge gates, exact tested product, census cadence and G6 remain authoritative |

Wall-clock overlap between authors is measured separately from simultaneous
execution. Separate directories establish write isolation, not machine
resource isolation. The experiment does not require simultaneous compilers
or another scheduler to call authorship concurrent.

## 9. Guarantees and response rules

**Structural within the implemented data model:** required question IDs cannot
silently disappear; incomplete/error attempts cannot render a passing
completed panel; source/checker identities and arm labels are required;
no experiment result changes admission authority. These need tests and code
review; they are proposed requirements, not achieved facts.

**Empirical:** the selected real-API specimens compile, discriminate the
named violations and safe cases, reconstruct elsewhere, and exhibit the
recorded costs. Report the exact population and observed failures.

**Judgment:** question sufficiency, oracle independence, input completeness,
usefulness of the chosen edits, interpretation of nulls, and generalization
to future campaigns. A second author or passing model does not remove these.

Attempt validity and subject outcome are separate dimensions. A completed
negative specimen can be valid evidence of a violated ownership obligation;
it is never a safe-source verdict. Captured expected production refusal is
different from an uncontrolled harness crash or missing result. The report
must preserve this distinction even when its own experiment tests pass.

| Result | Required response |
| --- | --- |
| Compile failure or unchanged mutation | Invalid specimen; not a behavioral kill or green control |
| Base already fails | Repair the experiment premise or document baseline defect; no interaction claim |
| One solo arm fails | Dependency failure; not joint-only interaction |
| Both solo arms pass and composition fails | Interaction candidate; validate raw behavior independently |
| Expected ownership refusal captured from production | Valid negative-specimen outcome; distinguish existing guard detection from independent corroboration and earlier selection |
| All arms pass | Safe observation for those inputs only |
| Challenger and implementation disagree | Preserve both; investigate authority and derivations before assigning fault |
| Missing input/check, timeout, interruption or cleanup uncertainty | Incomplete/unknown attempt; retain evidence, no successful aggregate |
| Declaration predicts no more usefully than the cheaper comparator | Comparative null; no mechanism expansion justified |
| Registry/simulation/save/epoch changes would need to ship for the assay | Outside scope; return for design amendment |
| A new candidate mechanism weakens an accepted check | Evaluate under the accepted check; candidate cannot approve itself |

No case is removed after scoring merely to improve the result. A later
revision gets a new question/specimen identity and is reported separately.

## 10. Acceptance and what could be earned later

The campaign completes its experimental obligation when it delivers a
reviewed frozen roster, independently authored specimen records, all raw
arms and failures, an independent reconstruction, and a report that separates
the primary hypothesis from the comparative result. A null or falsified
hypothesis is a valid research conclusion, provided the experiment was valid;
it is not permission to label semantic independence demonstrated.

A positive primary result needs a real safe pair and a verified joint-only
interaction under the same obligations. A positive comparative result
additionally needs demonstrated benefit over the cheaper comparator on
the reserved challenge, no missed violating obligation in the tested panel,
and a stated preparation/coordination cost. Count checks and record times;
do not preregister an arbitrary percentage improvement as a target before
a baseline exists.

Run focused outboard experiment tests and actual CLI/report exercises on
supported Mac and canonical Linux through existing gates. Challenge invalid
specimens, missing/negative inputs, stale identities, failures and
misattributed results. Verify the experiment's execution and declarations
separately. No whole-workspace intermediate Mac run, off-host census,
new performance SLO, or silent artifact rebaseline is implied.

**No admission privilege is granted by this campaign.** A later proposal
could ask to omit one named verification operation for one closed family
of changes only after supplying:

- a reviewed completeness/invalidation argument covering imports, checker
  and rule versions, toolchain/configuration/material environment, new and
  negative dependencies;
- independent counterexamples and shadow comparison against accepted
  verification on representative real changes, including failures;
- measured end-to-end benefit after preparation, review and queue costs;
- explicit unknown-scope fallback, revocation, unchanged obligations, and
  review under previously accepted authority;
- preservation of exact-tested-merge, canonical artifact and epoch rules.

Counterpart's finite evidence can inform that argument; it cannot finish it.
A simpler intervening privilege—independent preparation—still needs its
own bounded resource/build evidence and does not imply behavioral test
omission.

## 11. Capture and review boundary

The registry retains the program's existing homes:
`PROC-federated-evolution`, `PROC-evidence-selects-obligations`,
`PROC-context-provenance`, and `PROC-project-epistemology`.
Their broader work remains open. The ledger records the other alternatives,
seven Charter follow-ups, rejected prerequisites and profiling ownership.

G3 flags:

1. The strongest interaction specimens remain a hypothesis to qualify.
   Manufacturing useful-looking toy cases would defeat the campaign.
2. Independent derivation and complete input closure remain partly judgment.
   The dossier explicitly cannot justify evidence reuse.
3. The dossier is a small new development data format, outside simulation
   contracts. Digest protocol v1 and existing gates remain in force.
4. A comparative null may recommend no additional federation machinery.

Nathan approved proceeding after the plain-language explanation on 2026-09-05
("Great! Let's move forward."). Implementation planning proceeds under
campaign-autopilot; the next human review boundary is G6 before merge/close.
