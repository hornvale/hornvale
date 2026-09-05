# Digest context

Revision: `e3355f441db045f0960576ead12b0f8acc56ca7d`

Working tree: **dirty**

## Census publication boundary (`hornvale.census-publication`)

Scopes: `windows/lab/src/census\_guard.rs`, `windows/lab/src/publish.rs`, `scripts/census-canonical-host.txt`, `scripts/sluice-census.sh`

### Authored requirements

#### `hornvale.census-publication:guard-behavior`

A census study may target the committed census-golden directory suffix only on the canonical host, matched without ASCII case; unrelated studies and unrelated output directories remain permitted by this predicate.

Authorities/sources: `windows/lab/src/census\_guard.rs`, `windows/lab/src/publish.rs`, `scripts/census-canonical-host.txt`

Evidence: Checked. Expected observations: `hornvale.census-publication:exact-census-canonical-host`, `hornvale.census-publication:prefixed-census-normalized-host`, `hornvale.census-publication:exact-census-different-host`, `hornvale.census-publication:prefixed-census-different-host`, `hornvale.census-publication:exact-census-relative-suffix-different-host`, `hornvale.census-publication:unrelated-study`, `hornvale.census-publication:unrelated-output`, `hornvale.census-publication:source-host-agreement`. Result: **satisfied**.

#### `hornvale.census-publication:queued-authoring`

Request once-per-campaign census authoring through the canonical queue at a full SHA; a delivered branch whose reference artifacts moved lands only through the campaign-close merge approval.

Authorities/sources: `Makefile (sluice-census target)`, `scripts/sluice-request.sh (census request kind)`, `scripts/sluice-census.sh (branch delivery contract)`, `CLAUDE.md (census cadence and close authority)`

Evidence: Authored only (unchecked).

### Authored instructions

#### `hornvale.census-publication:interpret-guard-panel`

Treat these observations as a finite check of `require_canonical_host_for` plus current source/compiled-host agreement. The official-output cases use `Path::ends_with` semantics: an absolute checkout prefix is permitted before the full `book/src/laboratory/generated` suffix. They do not execute or establish invocation of `publish`, a census, or any queue path, and they do not identify the current machine.

Requirements: `hornvale.census-publication:guard-behavior`

Observations: `hornvale.census-publication:exact-census-canonical-host`, `hornvale.census-publication:prefixed-census-normalized-host`, `hornvale.census-publication:exact-census-different-host`, `hornvale.census-publication:prefixed-census-different-host`, `hornvale.census-publication:exact-census-relative-suffix-different-host`, `hornvale.census-publication:unrelated-study`, `hornvale.census-publication:unrelated-output`, `hornvale.census-publication:source-host-agreement`

#### `hornvale.census-publication:request-census`

After the candidate has a full commit SHA, request its once-per-campaign census with `make sluice-census BRANCH=<requester> REF=<full-sha>`. The queued job authors on the canonical host and delivers a separate census branch; if reference artifacts moved, submit that delivered branch through the ordinary merge queue at campaign close and obtain the campaign-close approval before landing it.

Requirements: `hornvale.census-publication:queued-authoring`

Observations: none

### Observations

#### `hornvale.census-publication:exact-census-canonical-host` — satisfied

Method: call the pure census guard and compare its result with an independently authored finite-case expectation

Subject: study="the-census"; output=exact committed-goldens suffix; host=canonical host

Details: Expected allowed and observed allowed. This result covers only this literal panel case and does not prove that a caller invokes the guard.

Requirements: `hornvale.census-publication:guard-behavior`

#### `hornvale.census-publication:exact-census-different-host` — satisfied

Method: call the pure census guard and compare its result with an independently authored finite-case expectation

Subject: study="the-census"; output=absolute path ending with the committed-goldens suffix; host=guaranteed different host

Details: Expected refused and observed refused. This result covers only this literal panel case and does not prove that a caller invokes the guard.

Requirements: `hornvale.census-publication:guard-behavior`

#### `hornvale.census-publication:exact-census-relative-suffix-different-host` — satisfied

Method: call the pure census guard and compare its result with an independently authored finite-case expectation

Subject: study="the-census"; output=exact relative committed-goldens suffix; host=guaranteed different host

Details: Expected refused and observed refused. This result covers only this literal panel case and does not prove that a caller invokes the guard.

Requirements: `hornvale.census-publication:guard-behavior`

#### `hornvale.census-publication:prefixed-census-different-host` — satisfied

Method: call the pure census guard and compare its result with an independently authored finite-case expectation

Subject: study="census-of-the-meeting"; output=absolute path ending with the committed-goldens suffix; host=guaranteed different host

Details: Expected refused and observed refused. This result covers only this literal panel case and does not prove that a caller invokes the guard.

Requirements: `hornvale.census-publication:guard-behavior`

#### `hornvale.census-publication:prefixed-census-normalized-host` — satisfied

Method: call the pure census guard and compare its result with an independently authored finite-case expectation

Subject: study="census-of-the-meeting"; output=absolute path ending with the committed-goldens suffix; host=ASCII-case-normalized canonical host

Details: Expected allowed and observed allowed. This result covers only this literal panel case and does not prove that a caller invokes the guard.

Requirements: `hornvale.census-publication:guard-behavior`

#### `hornvale.census-publication:source-host-agreement` — satisfied

Method: read the supplied root's host source and compare its trimmed value with hornvale-lab's compiled constant

Subject: scripts/census-canonical-host.txt and CANONICAL_CENSUS_HOST

Details: The source host and compiled host both name "lefford". This checks source/build agreement only; it does not identify the machine running the contributor.

Requirements: `hornvale.census-publication:guard-behavior`

#### `hornvale.census-publication:unrelated-output` — satisfied

Method: call the pure census guard and compare its result with an independently authored finite-case expectation

Subject: study="the-census"; output=unrelated scratch output; host=guaranteed different host

Details: Expected allowed and observed allowed. This result covers only this literal panel case and does not prove that a caller invokes the guard.

Requirements: `hornvale.census-publication:guard-behavior`

#### `hornvale.census-publication:unrelated-study` — satisfied

Method: call the pure census guard and compare its result with an independently authored finite-case expectation

Subject: study="the-chorus"; output=absolute path ending with the committed-goldens suffix; host=guaranteed different host

Details: Expected allowed and observed allowed. This result covers only this literal panel case and does not prove that a caller invokes the guard.

Requirements: `hornvale.census-publication:guard-behavior`

### Limits

- Observations cover only the named method and finite subject.
- Contributor checks are reviewed code and can themselves be wrong.
- Revision and dirty state do not establish reproducibility or an atomic source snapshot.
- This context does not authorize gate omission or approve changes to its governing rules.
