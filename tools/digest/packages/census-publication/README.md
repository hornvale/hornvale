# Census publication Digest contributor

`digest-census-publication` contributes checked context for Hornvale's pure
census-host predicate and authored guidance for the census queue. Its library
entry point is:

```rust
pub fn contribution(repo_root: &std::path::Path) -> Result<Contribution, String>
```

The executable implements the Digest contributor contract:

```text
digest-census-publication collect --repo-root <absolute-path>
```

Collection reads `scripts/census-canonical-host.txt`, compares it with the
host compiled into `hornvale_lab::census_guard::CANONICAL_CENSUS_HOST`, and
calls only the pure `require_canonical_host_for` guard. It does not build a
world, run a study or census, publish artifacts, or submit queue work.

The independently authored panel covers the exact `the-census` name, the
`census-of-` prefix, an ASCII-case-normalized canonical hostname, a guaranteed
different hostname, the exact committed-goldens suffix, an absolute path that
ends with that suffix, an unrelated study, and an unrelated output directory.
Each result is a separate required observation. The panel catches both an
always-accepting guard and an always-refusing guard, while remaining a finite
check rather than a proof over every possible input. Wrong-host refusal cases
make both `census-of-` recognition and the exact relative golden suffix
load-bearing; dedicated mutants omit each protection in turn and must
contradict the panel. In particular, observing the pure predicate does not
establish that `publish` or any other caller invokes it.

The queue requirement is deliberately authored-only. The current request
authority is:

```text
make sluice-census BRANCH=<requester> REF=<full-sha>
```

That request authors on the canonical box and delivers a separate census
branch. When census reference artifacts move, landing that branch still goes
through the ordinary merge queue and the campaign-close approval.
