# The Insulator

The Insulator asked whether a contributor-facing digest tool could become a
smaller build island without weakening the product it observes. The campaign
measured the existing `tools/digest` closure first, then built the smallest
boundary that could test the idea. The answer was a useful rejection.

The experiment's measurement cell made the claim concrete. It ran only frozen,
named workloads; the checkout was read-only; target and evidence directories
were the only writable roots; stdout and stderr were bounded; and incomplete
cleanup or missing provenance invalidated the record. macOS enforcement used a
constrained `sandbox-exec` profile, including only the system permissions the
real Cargo phases required. The cell's tests cover both permitted writes and
attempts to write the checkout. This turned "owned paths" from a declaration
into an enforced boundary.

The frozen Mac baseline found 54 packages and 5 workspace members. For the
publication workload, the authoritative path took 35.923 seconds cold and
0.381 seconds warm, including separately recorded preparation, build, and test
phases. The smaller Thing workload took 1.423 seconds cold and 0.362 seconds
warm. These are measurements of this tree and workload panel, not a general
build promise.

The candidate admitted `digest-protocol` and contained no copied
`windows/lab` authority. Its protocol probe completed, but produced 31 bytes;
the authoritative publication produced 7,493 bytes. Their SHA-256 identities
differed. Reproducing the authoritative output would require importing or
duplicating the publication rules owned by `windows/lab`, so the candidate was
rejected. Canonical Linux qualification was recorded as `not_run` and
`not_required`: the complete Mac output mismatch already decided the product
question. No production dependency split, selective verification, semantic
selector, or verdict cache was admitted.

The process consequence is durable. A smaller graph is now understood as a
measurement result until output identity, authority, invalidation, provenance,
and repeatable cost all agree. Negative evidence can close a hypothesis while
preserving the path for a future candidate with an explicit API that carries
the publication authority.
