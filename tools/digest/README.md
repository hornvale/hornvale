# Digest context

From the repository root:

```sh
make context-prepare                # explicit dependency/build preparation
make context SCOPE=domains/thing
make context SCOPE=windows/lab/src/publish.rs
make context SCOPE=.               # every enrolled contributor
```

Preparation may fetch dependencies and update `tools/digest/Cargo.lock`; review
that change normally. Subsequent context commands use `--locked --offline` for
both metadata and builds. Missing dependencies or a stale lockfile fail with a
preparation instruction. The Charter enrolls Thing ownership/borrowing and the
pure census-publication host predicate. A scope with no enrolled contributor
is a nonzero result. Preparation builds **all** Digest workspace members; it
is not a selected-scope latency measurement.

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
portable grandchild reaper. The supported qualification hosts are macOS and
canonical Linux (lefford).
Context uses Unix process facilities; unsupported operating systems receive a
context-specific error. Existing renderers remain separate. Qualification
results and any pending host checks are recorded in the evidence document below.

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

## Reading the two scoped reports

Thing observes roster/registry agreement and ownership through the real Thing
and Settlement registration APIs. Its finite checks do not establish worldgen
registration coverage, save compatibility or item behavior. The census adapter
calls the pure publication guard on a named case panel, including both allowed
and refused inputs, and compares the compiled canonical-host constant with its
current source file. Its official-path rule is a suffix predicate, not filesystem
containment. Neither scope constructs a world or runs a live census.

Queue instructions remain authored policy: the publication predicate does not
verify that a queue submission occurred. Read each requirement's evidence mode,
its named observations and limits, and the linked owning sources. A contradicted
or unknown required observation can produce a useful report with a nonzero exit;
malformed collection fails before emitting a completed report. Saved stdout is
an example, never an accepted input to a later context request.

To extend the tool, add a package and the metadata described above, implement
`collect --repo-root`, and test both a real satisfying case and discriminating
counterexamples through the owning APIs. Declare expected observations explicitly;
removing one does not satisfy the old obligation. Run the all-package outboard
checks and exercise the actual composed CLI. Enrollment must not require a new
contributor-specific host dispatcher or changes to simulation authority.

## Explicit cost diagnostic

After normal dependency preparation and committing the source to measure:

```sh
bash scripts/charter-measure.sh --self-test  # owned fixture/lifecycle checks only
bash scripts/charter-measure.sh            # print every measurement as JSON lines
```

The diagnostic measures committed `HEAD`, and refuses an uncommitted diagnostic
script. For each of Thing, census and combined context, it creates its own
detached worktree at that commit, preserving `rust-toolchain.toml`. It first
builds only the Digest host into the fresh `tools/digest/target`, then invokes
that host once to build/collect the selected contributor(s), followed by ten
requests of the same workload. Host compilation and the cold selected request
are separate samples. No active target cache is deleted or redirected. Dependency
and operating-system caches can be warm; “cold” means the tool target starts
absent. Requests after a failed preparation are explicitly marked unprepared.

Each sample includes the exact command, source/workload context, UTC start, load,
wall elapsed, exit status, raw `/usr/bin/time` and phase diagnostics, stdout byte
count/hash and available peak RSS. Failed stdout is retained. macOS reports RSS
in bytes; GNU Time reports KiB, normalized to bytes alongside the original units.
Neither is an aggregate simultaneous process-tree memory budget. Whole-phase
measurements do not separate compilation from lock waiting; missing timing or
memory data remain missing. The diagnostic's one-hour command safety deadline
is separate from the context operational caps above. Cleanup finishes owned
processes across the owned session, including Digest's nested process groups,
before removing their worktrees. A cleanup failure emits the failed attempt with
its command, diagnostics and retained paths before aborting further samples;
both the raw sample files and owned worktree remain for inspection. Every
unsuccessful sample retains stdout, including interruption after a zero exit.
Trusted children must not detach into separate sessions; this is not a sandbox.

This is an explicit diagnostic, not a routine gate. Canonical measurements run
only through the existing serialized queue: the controller creates an isolated
**stage-only** branch whose sole extra integration edit invokes this script from
its outboard command. That branch is never merged back. A queue submission is
not a measurement; retain the completed job log and tested merge-product SHA.
The script prints evidence and writes no committed artifact. It adds no runtime
or dependency beyond Bash, existing Python 3, Git, the prepared Rust toolchain
and the host's `/usr/bin/time`.

The [Charter evidence](../../docs/digest/the-charter-evidence.md) records commands,
all samples, actual report review, limitations and qualification status. Measured
baselines do not amend the resource caps or earn gate omission.
