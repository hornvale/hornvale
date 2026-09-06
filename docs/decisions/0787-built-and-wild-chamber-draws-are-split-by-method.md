# 0787. Built and wild chamber draws are split by method under two labels

**Status:** Accepted (2026-09-05) · **Decider:** Nathan (G3 / autopilot) ·
**Campaign:** The Cruck · **Relates:**
[0073](0073-epoch-granularity-is-declared.md) (a label whose draw sequence
changed needs a version),
[0084](0084-an-epoch-is-declared-only-when-a-derivation-moved.md) (an epoch
is declared only when a derivation moved),
[0786](0786-a-built-structures-chamber-graph-is-a-derived-tree.md) (the
derivation this splits)

In the context of a built structure's chambers becoming derived while a wild
structure's stay drawn, we decided that **`room/chambers/v1` is not bumped —
wild sites keep drawing under it byte for byte, and built sites draw their
facets under a new `room/chambers/built/v1`** (one draw per chamber, no count
draw), accepting that one seam now carries two permanent labels.

This is The Blocking's method split applied one band coarser: the two layout
methods each took their own stream label rather than sharing a versioned one,
and the same argument holds here. The code the two paths share — the
two-bits-per-digit `child_path` address extension and the forward collision
scan — is untouched, and the wild derivation's draw sequence is unchanged, so
bumping `room/chambers/v1` would move every cave in every world for no
reason. The alternative decision 0073 actually forbids is the third one:
keeping `v1` and changing what the built path consumes under it.

Verified before deciding rather than recalled: nothing serialized references
a chamber. Every `pack()` in the session packs a walk-band room, and
`chamber_id` feeds only the plan and the `[chamber N]` header — so a changed
built derivation orphans no committed fact.

**Accepted cost.** Two labels where a reader might expect one, forever; a
future change to the shared address extension must bump both. The manifest
gains exactly one row and no existing row changes.

**See also.** [The Cruck design](../superpowers/specs/2026-09-04-the-cruck-design.md)
§§4, 6 and G3 flag 1, and [campaign ledger](../superpowers/ledgers/2026-09-04-the-cruck.md)
#3.
