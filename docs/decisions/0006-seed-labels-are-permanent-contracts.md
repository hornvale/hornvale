# 0006. Seed-derivation labels are permanent contracts

**Status:** Accepted (2026-07-05) · **Decider:** Nathan

In the context of a hierarchical seed system where each derivation is keyed by
a string label, facing the reality that changing a label silently changes
every world derived from it, we decided that **seed-derivation labels are
permanent save-format contracts; deliberate regeneration uses an explicit
epoch suffix (`settlement/name/v2`), never a rename**, accepting that labels
accumulate rather than being tidied.

**Context.** A world's identity is its seed plus the exact sequence of labeled
draws that build it. Renaming a label corrupts every saved world that depended
on it, invisibly. Stream *consumption order* is part of the same contract — a
pin must consume the same draws as the unpinned path.

**Consequence.** Labels are declared as constants in each crate's `streams`
module and published into a generated, drift-checked stream manifest. An
identity/alias layer (mutable display names over immutable baptismal labels) is
deferred to the lazy-generation campaign, where renames would otherwise become
corrupting.

**See also.** Spec "Determinism / save-format contracts"; `CLAUDE.md`; stream
manifest reference chapter.
