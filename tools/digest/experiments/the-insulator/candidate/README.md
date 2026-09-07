# Insulator candidate boundary probe

This candidate is an experiment-only Cargo workspace outside the root
workspace. It admits `digest-protocol` and contains no `windows/lab` edge or
copied publication authority. Its binary is deliberately only a protocol
boundary probe; it does not reimplement `digest-census-publication` or call
`hornvale-lab`.

The boundary is therefore admissible as a dependency experiment. The
candidate is not admissible as a replacement for the authoritative
`digest-census-publication` workload: reproducing that workload's output
requires the publication rules owned by `windows/lab`, and importing those
rules would violate the candidate boundary. `compare_outputs` must retain
that result as an output mismatch or incomplete record rather than treating a
smaller graph as equivalent.

Provenance is recorded in the committed machine-readable `manifest.json` from
the frozen baseline commit, tree, and graph identity. The manifest also
retains the observed 31-byte output identity used by the comparison. Candidate
commands are intentionally absent from that manifest; execution continues to
use the named workload contract and the existing measurement cell.

## Measured rejection

On 2026-09-07, both commands exited successfully from the same checkout, but
their stdout identities differed:

| path | bytes | SHA-256 |
| --- | ---: | --- |
| candidate protocol probe | 31 | `188703470f44519be04f4e3f647af191ba4ae50a1e8f393477b0f3b8ccb4369d` |
| authoritative publication | 7493 | `9571178d3943687ed348ad3f0f6fed783e967329286789ad58f0a2e595063306` |

The candidate is rejected as a replacement because output identity is not
preserved. Reproducing the authoritative bytes would require importing or
duplicating the publication rules owned by `windows/lab`, so this campaign
stops here and retains the rejection rather than adding a second authority.
