# Digest context

From the repository root:

```sh
make context-prepare                # explicit dependency/build preparation
make context SCOPE=domains/thing
make context SCOPE=.               # every enrolled contributor
```

Preparation may fetch dependencies and update `tools/digest/Cargo.lock`; review
that change normally. Subsequent context commands use `--locked --offline` for
both metadata and builds. Missing dependencies or a stale lockfile fail with a
preparation instruction. No contributor is enrolled by the bootstrap itself;
no match is a nonzero result.

Manual invocation after preparation:

```sh
cargo run --quiet --manifest-path tools/digest/Cargo.toml --package digest \
  --bin digest --target-dir "$PWD/tools/digest/target" --locked --offline \
  -- context domains/thing
```

The built host can also be invoked directly from another checkout. It resolves
the caller's current directory with Git, ignoring inherited `GIT_DIR`,
`GIT_WORK_TREE`, `GIT_COMMON_DIR`, `GIT_INDEX_FILE`, `GIT_OBJECT_DIRECTORY` and
`GIT_ALTERNATE_OBJECT_DIRECTORIES`. The new command never uses its compile-time
source directory to select context. Existing `render doctor`, `render decisions`
and `render delta` retain their original source behavior.

## Enrolling a contributor

Add a package under `tools/digest/packages/`; the `packages/*` workspace pattern
includes it. Keep the protocol package dependency local:

```toml
[dependencies]
digest-protocol = { path = "../protocol" }
serde_json = "1"

[package.metadata.digest]
role = "contributor"
namespace = "hornvale.thing"
protocol = 1
binary = "digest-thing"
scopes = ["domains/thing"]
```

The declared binary must be exactly one binary target in that actual workspace
member. For example, declare `[[bin]] name = "digest-thing"` with its source
path, or use the package's default binary name. Packages without Digest metadata
are skipped. Duplicate namespaces, unsupported roles/versions, invalid scopes,
ambiguous targets and member manifests escaping `packages/` fail discovery.
Metadata contains exactly the five keys above. Namespaces start with a lowercase
ASCII letter and contain only lowercase letters, digits, dots and hyphens.
Scopes are normalized repository-relative slash paths: no empty input, absolute
paths, backslashes, `..`, duplicate or trailing separators. `.` explicitly selects
all enrolled packages. Ancestors and descendants match on path components;
`domains/thing-other` does not match `domains/thing`.

The host runs, without a shell:

```text
<exact-cargo-artifact> collect --repo-root <absolute-current-checkout>
```

Cwd is that checkout and stdin is closed. The host first asks Cargo metadata for
actual workspace members, then builds only selected contributors with explicit
manifest, package ID, binary, target directory, `--locked --offline` and JSON
compiler messages. It selects the single matching `compiler-artifact` executable,
including cached Cargo builds, and refuses paths outside this checkout's
`tools/digest/target`. Ambient target-dir/cross-target overrides cannot select a
neighbor's executable. Contributor results are always collected afresh; there is
no saved-report input, report cache or acceptance route for old evidence.

## Response contract

Emit exactly one JSON `digest_protocol::Contribution` on stdout; send diagnostics
to stderr. Exit nonzero when collection cannot be performed. An example envelope:

```json
{
  "protocol": 1,
  "namespace": "example.local",
  "display_name": "Local policy",
  "scopes": ["domains/example"],
  "requirements": [{
    "id": "example.local:rule",
    "statement": "An authored obligation.",
    "sources": ["docs/decisions/NNNN-owner.md"],
    "evidence": {"checked": {"required_observations": ["example.local:check"]}}
  }],
  "observations": [{
    "id": "example.local:check",
    "method": "Describe the finite procedure actually run.",
    "subject": "Describe its bounded subject.",
    "outcome": "satisfied",
    "details": "State what this establishes and what it does not.",
    "requirements": ["example.local:rule"]
  }],
  "instructions": [{
    "id": "example.local:editing",
    "markdown": "Read the owning source before changing this rule.",
    "requirements": ["example.local:rule"],
    "observations": ["example.local:check"]
  }]
}
```

Protocol, namespace and scope list must agree exactly with metadata. All record
IDs belong to the contributor namespace and all references resolve to the right
record kind. Unknown fields and malformed or missing records fail. Requirements
and instructions are nonempty. An authored-only requirement uses
`"evidence": "authored_only"`; it is shown as unchecked, not given a verification
badge. A checked requirement names a nonempty expected observation set. Missing
required observations are errors; required `unknown` or `contradicted` results
produce an honest report with a nonzero exit. Every explicit contradiction fails,
even when it is outside a requirement's required set. Optional `unknown` alone
is informational.

Requirements are authored policy, not inferred truth. Changing an expected
observation list does not prove the former requirement satisfied. Source references
are the authority to inspect; a passing finite check establishes only its stated
claim. Checkout revision and dirty state are context, not complete provenance or
an atomic source snapshot. This command does not replace gates, construct worlds,
run live censuses or change publication authority.

## Process and output limits

Contributors execute sequentially: stdout at most 1 MiB, stderr at most 64 KiB,
and execution at most 5 seconds. Metadata/build phases allow 16 MiB per captured
stream and 600 seconds per phase. Both pipes are concurrently drained into
bounded buffers. Overflow, deadline and cancellation fail visibly. Each process
runs in an owned Unix process group; cleanup sends termination, allows 250 ms,
then forces termination, waits for its direct child and joins the readers.
Normally exited parents cannot leave descendants holding output pipes. SIGINT
and SIGTERM request cancellation. Descendants must remain in their invocation's
process group: this is trusted repository code, not a hostile-code sandbox or
portable grandchild reaper. Context is supported on Unix; existing renderers
remain separate.

Only after every selected envelope is collected and validated does the host emit
the assembled report on stdout. Whole-phase elapsed timing and diagnostics go to
stderr; timing does not claim separate lock-wait or compilation attribution and
never enters deterministic report text. No global budget across concurrent host
invocations is established by these per-invocation limits.

Run the outboard suite with `cargo test --manifest-path tools/digest/Cargo.toml
--workspace`. CLI fixtures use real Cargo workspaces and copy Hornvale's prepared
`rust-toolchain.toml` so running outside its directory does not select an older
host default. After their minimal lockfile preparation, fixture context runs are
offline.
