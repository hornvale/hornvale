# CLAUDE.md — working in `tools/type-audit/` (and living with its verdicts)

The type-audit tool enforces the typed-quantities doctrine (decisions
0027/0028): every primitive (`f64`, `u32`, `bool`, `&str`, …) at a `pub`
boundary must carry a `type-audit:` verdict tag, or the audit fails. It is a
standalone tool **outside the workspace** (its own `Cargo.toml`), because it
may use parser libraries the workspace bans.

## Running it

```bash
# default-deny: any untagged pub-boundary primitive fails
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
# regenerate the committed report (drift-checked by `make rebaseline`, never automatically — 0125)
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
```

`make gate-commit` runs `check`; the artifact-freshness step regenerates and
drift-checks the report.

## The tag, and where it goes

A tag lives in the doc comment above the item and names each primitive:

```rust
/// type-audit: bare-ok(index: plate_of), bare-ok(ratio: return)
```

Three verdicts:
- **`bare-ok(<class>)`** — the primitive is fine bare. Ratified classes:
  `ratio`, `count`, `index`, `constructor-edge`, `envelope`,
  `identifier-text`, `prose`, `artifact`, `diagnostic-value`,
  `render-internal`, `flag`. An unknown class is an error.
- **`waiver(<reason>)`** — a deliberate exception.
- **`pending(wave-N)`** — not yet classified; scheduled.

For a struct, the tag on the struct's doc comment lists each primitive field
by name. For a function, it lists each primitive parameter (and `return`) by
name.

### On a default-deny lint, add what it DEMANDS, never what it might

A struct-level tag is required only for a primitive field that is itself at a
`pub` edge. A **private** field needs none, even on a `pub` struct — its
primitives reach the boundary only through the methods that expose them, and
those methods carry their own tags.

Two kernel types make the contrast concrete, and they sit a file apart:

- `kernel/src/fold.rs`'s `Folded<S>` has `state: S` and `position: u64`, both
  **private**. `check` passes with *method*-level tags only — `bare-ok(count:
  position)` on `resume`, `bare-ok(count: return)` on `position()`. There is
  no struct-level tag and adding one would be tagging a primitive the audit
  never asked about.
- `kernel/src/derived.rs`'s `Validity::Ledger { position }` is a field of a
  **`pub` enum variant**, so it is at the boundary and does need the
  struct-level form: `type-audit: bare-ok(count: Ledger.position)`.

The failure mode this prevents is pre-emptive tagging: on a default-deny lint
the temptation is to tag everything that looks like a primitive, which buries
the boundary the audit is trying to describe and creates extra tag positions
for the footgun below to make stale. **Run `check`, read what it demands, tag
exactly that.**

## The footgun (learned the hard way this session)

The tool tracks tag **positions**. When you **move a tagged primitive** — e.g.
lift a `plate_of: &VertexMap<u32>` param out of three functions into a new
struct field — the old functions' tags go **stale** ("stale tag position")
and the new struct field is **untagged**. Both fail `check`, and the committed
report goes stale too. This is invisible to unit tests and to a
gallery/reference/laboratory-only drift check (the report lives in
`docs/audits/`, which that subset excludes — the full check adds `docs/audits`
and `docs/digest`). **After any pub-boundary signature change: run
`type-audit check` and regenerate the report — or just run the full `make
gate`.** A commit that skipped this briefly left `main` gate-failing.

## Only primitives at `pub` edges

Newtypes (`Au`, `Mm`, `StdDays`, …) and non-primitive types (`&VertexMap<T>`
where `T` is an enum/struct) don't need tags — only bare primitives. If you
find yourself tagging a lot of bare `f64`s that form a coherent unit, the
right fix may be a newtype (decision 0008/0044), not more tags.
