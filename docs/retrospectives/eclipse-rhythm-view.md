# Eclipse Rhythm and View — retrospective

This campaign extended the shipped Eclipse Seasons core through recurrence,
observer visibility, `scene/eclipses/v3`, the native/WASM query surfaces, and
the almanac. The product account is in the
[chronicle](../../book/src/chronicle/eclipse-rhythm-view.md); the decisions
and verification evidence remain in the
[campaign ledger](../superpowers/ledgers/2026-09-10-eclipse-rhythm-and-view.md).

## The joins needed a campaign-wide review

The task reviews were clean, but the integrated review found five defects at
the boundaries between them: exeligmos was describing node slip instead of
surface closure; wrapped track endpoints lost directed sweep information;
scene enumeration used unsnapped bounds; coincidence summaries were not
reachable; and almanac wording confused event-midpoint side with
event-wide track visibility. The fix round added behavior-first regressions
and corrected the shared contracts in ledger entry #7.

The useful review question was: *can a reader observe every promise in the
specification through the same producer?* It caught issues that isolated
producer and consumer reviews could not see.

## Determinism stayed a boundary rule

The campaign added no draws, epochs, saved facts, or census columns. Existing
node facts remain the source of dated events. Time is evaluated at each event,
floating point is quantized only at emission, and native/WASM scene bytes are
identical for equal inputs. The v3 replacement was intentional because this
is pre-alpha and no compatibility consumer exists.

## Closeout findings

The canonical merge hold caught two operational facts before landing. First,
the campaign needed its own chronicle and retrospective; the existing
Eclipse Seasons documents belonged to the parent campaign. Those artifacts
are now present and wired in `book/src/SUMMARY.md` and the reconciliation
record. Second, Eclipse Seasons changes world output, so its merge must remain
behind The Planetarium's queued census. That ordering protects the census
from measuring a pre-Eclipse world and prevents a stale delivery branch.

The campaign's deferred seams are recorded in `SKY-eclipse-seasons` and the
related frontier rows: partiality, animated shadow motion, lunar shading,
standstills, transits, tidal braking, variable stars, equation-of-time work,
aurorae, and cultural consumers remain separate campaigns.
