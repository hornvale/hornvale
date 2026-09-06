# The Insulator: measured build boundaries

The Insulator measures whether a narrow contributor build island can reduce
invalidation or build cost while preserving the authoritative Cargo workspace
and its full correctness gates. It is an experiment. The candidate remains
outside production behavior until paired Mac and canonical Linux evidence,
byte comparisons, dependency-boundary checks, and independent review justify
integration.

## Contract

`workloads.json` is frozen before measurement. Each workload declares a full
argv and expected output paths with an explicit comparison mode. Measurements
run in immutable, owned checkouts and owned Cargo targets. A cold run owns a
fresh target; a warm run reuses only the target named by its manifest.
The recorder accepts a frozen workload ID, resolves its argv from
`workloads.json`, and rejects unknown IDs; callers cannot supply the measured
command.

Every attempt retains source, graph, toolchain, target, command, timing, exact
bounded stdout/stderr, output identities, cleanup status, and failure reason.
Preparation, compilation, test, queue, and authoring costs remain separate.
Failed preparation and incomplete cleanup are retained and fail closed; they
are never converted into a successful product result.

The recorder uses the existing Counterpart process-session cleanup pattern and
reads each stream through a hard 16 MiB retention cap. An oversized writer is
terminated as soon as a read crosses the cap; the retained attempt is marked
invalid. Capture also runs every workload inside a host filesystem sandbox:
macOS requires `/usr/bin/sandbox-exec`, while Linux requires `bwrap`. The
sandbox makes only the canonical target and evidence roots writable; the
checkout remains read-only, and capture refuses before launch when the host
mechanism is unavailable. This is
prevention at the host boundary, not authentication against a hostile kernel
or proof that a sandbox implementation is bug-free. Workload argv may contain
`${CHECKOUT}`, which is replaced with the absolute owned checkout at execution
time. Unit tests use fixture Python commands only and never invoke Cargo or a
live build.

## Commands

Run the deterministic recorder tests with:

```sh
python3 -m unittest discover -s tools/digest/experiments/the-insulator -p 'test_measure.py' -v
```

Later stages will add graph extraction, invalidation probes, candidate output
comparison, and paired qualification. Those stages must keep Cargo's full
workspace gate authoritative and must not treat a selector, smaller graph, or
candidate output as production facts.

## Limits

This experiment cannot establish that a smaller graph is always faster, that
dependency reach captures semantic influence, or that an owned clean checkout
models concurrent authoring. It cannot authorize selective verification,
portable verdict reuse, or a general federation protocol. A negative result is
still a successful qualification of the hypothesis and must be retained with
its evidence.
