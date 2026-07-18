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
# regenerate the committed report (drift-checked in CI)
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
```

`make gate` runs `check`; the artifact-freshness step regenerates and
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

## The footgun (learned the hard way this session)

The tool tracks tag **positions**. When you **move a tagged primitive** — e.g.
lift a `plate_of: &CellMap<u32>` param out of three functions into a new
struct field — the old functions' tags go **stale** ("stale tag position")
and the new struct field is **untagged**. Both fail `check`, and the committed
report goes stale too. This is invisible to unit tests and to the
gallery/reference/laboratory drift check (the report lives in `docs/audits/`,
which that subset excludes). **After any pub-boundary signature change: run
`type-audit check` and regenerate the report — or just run the full `make
gate`.** A commit that skipped this briefly left `main` gate-failing.

## Only primitives at `pub` edges

Newtypes (`Au`, `Mm`, `StdDays`, …) and non-primitive types (`&CellMap<T>`
where `T` is an enum/struct) don't need tags — only bare primitives. If you
find yourself tagging a lot of bare `f64`s that form a coherent unit, the
right fix may be a newtype (decision 0008/0044), not more tags.
