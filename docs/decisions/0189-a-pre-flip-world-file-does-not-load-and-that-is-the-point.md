# 0189. A pre-flip world file does not load, and that is the point

**Status:** Accepted (2026-08-24) · **Decider:** Nathan · **Campaign:** The
Escapement · **Supersedes:** nothing

In the context of `WorldTime` flipping from a bare `f64` day to an exact
`i64` tick count (decision 0186), facing the fact that `#[serde(transparent)]`
over a changed field type means a JSON document written before the flip now
fails to deserialize — `serde_json::from_str::<World>(old_json)` returns
`invalid type: floating point, expected i64` at the first committed fact
whose `day` is `Some` — we decided that **no float-tolerant `Deserialize` is
added to bridge the two encodings, and a pre-flip `world.json` simply does
not load.**

**What was considered and rejected.** A `Deserialize` impl that accepts
either a JSON float (interpreted as a fractional standard day, rounded to
the nearest tick on load) or a JSON integer (an exact tick count) is
straightforward to write — `serde_json::Value`'s own number representation
already distinguishes them internally. It was rejected because it would keep
the lossy encoding alive exactly where this campaign exists to remove it: a
pre-flip file loaded through such a bridge would silently round every stored
day to its nearest tick on the very first load, without the operator asking
for that rounding or knowing it happened. Worse, this campaign's own
Constitution-level finding (spec §1) is that the OLD encoding's rounding
went upward as often as down and could make a fact fail its own `day <= t`
filter on read-back — the exact defect this campaign fixes cannot be un-fixed
by a compatibility shim that re-introduces the encoding carrying it, and a
shim silently active at load time is a worse home for that defect than the
one it replaced, because nothing about loading a file signals that a
rounding pass just ran over it.

**Why this is acceptable rather than a regression.** A world is a seed plus
a ledger (Constitution §2), and every world this project has ever produced
is re-derivable byte-for-byte from its seed and pins alone. A pre-flip
`world.json` is not a unique, unrecoverable artifact — it is a cached
rendering of a computation that can be re-run in full: `hornvale new --seed
<seed> [pins...]` reproduces it exactly, now on the tick lattice from
genesis rather than migrated partway through its life. Losing the ability to
*load the old file* costs nothing that losing the ability to *regenerate the
same world* would cost, and the project has never promised the former
independently of the latter — it is the same promise the save-format epoch
discipline states for every other deliberate regeneration (decision 0033's
own epoch, the vessel/session/v2 fixtures, and this campaign's own internal
moves, spec §4).

**Why loud is the property that makes this acceptable, not merely the
observed behaviour.** `World::load`/`World::from_json`
(`kernel/src/world.rs`) return `Result`, and the CLI's own load path
(`load_world`, `cli/src/main.rs`) propagates the `serde_json::Error` through
to `main`'s top-level error handler, which prints `error: loading
<path>: <message>` and exits non-zero — the operator sees exactly what
failed and why, at the moment it fails, in the same command that failed. The
alternative failure classes were traced and ruled out, not merely assumed
absent:

- **A silent partial load** (some facts recovered, the malformed ones
  dropped) — not what `serde_json` does for a type error on a required
  field, and nothing in this tree adds tolerance for one.
- **A silent fallback to a fresh or default world** — searched for and not
  found: no call site catches `World::load`'s or `World::from_json`'s error
  and substitutes a default `World`, an `.ok()` discard, or an
  `unwrap_or_default()`, in the CLI, the browser-client wasm ABIs
  (`clients/world-wasm`, which never loads a saved file at all — it only
  ever builds fresh from a seed), or `windows/vessel`.
- **A silent value-preserving misread** (the float bytes reinterpreted as an
  integer, or truncated) — impossible in practice: `serde_json`'s `i64`
  visitor rejects a JSON number token that contains a decimal point or
  exponent outright, it does not reinterpret or truncate it.

So the one load path this tree has errors loudly, with no silent branch
found anywhere it could hide. That is the property this decision actually
rests on — not merely that old files stop loading, but that they stop
loading *audibly*, at the boundary, in the operator's own terminal.

**Consequence.** A `world.json` (or any embedded `Ledger`/`Fact` JSON)
written before this campaign's Phase B commit cannot be loaded by
`hornvale repl`, `hornvale almanac`, or any other CLI command that reads a
saved world; the fix is to regenerate it from its seed and pins. The three
committed seed-42 almanacs, the vessel/session/v2 and world-seed-42
fixtures, the lab study CSVs, the Domesday survey and the census goldens are
all regenerated in this campaign's own epoch (spec §4) rather than migrated
in place, for the identical reason: a migrated value is a value this
decision declines to trust.

**See also.** Decision 0186 (the tick-count retype this decision's epoch
follows from); decision 0188 (why the day field carries no quantization to
lose in the flip); spec §1 (the read-back defect a compatibility shim would
have kept alive); spec §4 (the save-format epoch's internal/cross-repo
split, which this record's regeneration list restates for the internal
half).
