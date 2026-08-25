<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab confidant`. -->

# The Confidant: felt-state reportability by people

One row per one of the fifteen `hornvale_species::society_registry()` peoples (dragons carry a `MindVector` but never settle or speak, so they sit outside this instrument — Task 4b). Three columns, spec §3.4:

- **`reportable_fraction`** — of the six reachable felt states, the share `species` has its own word for (`testify` returns `Direct`, never a `Nearest` substitute). **Capped at 50% by construction**: `MindVector` carries three `[0, 1]` scalars, one per valence-opposed pair, and a species Steeps at most one pole per pair — never both — so no people can ever hold more than 3 of the 6 words. That ceiling is an artifact of Task 4b's exposure mapping, not a finding about impoverished minds.
- **`collapse_ratio`** — the largest number of the six reachable felt states that resolve to the SAME reported word (conflation). `Absent` when `species` has no word for any felt state at all, since there is then nothing to collapse onto.
- **`misreport_distance`** — the mean circumplex distance between each reachable felt state and what `species`'s tongue actually reports for it (0 for a state with its own word). `Absent` for the same reason as `collapse_ratio`.

Computed once, at `Seed(42)` — see this function's own doc for why a single world suffices; every value here is world-invariant.

| people | reportable_fraction | collapse_ratio | misreport_distance |
|---|---|---|---|
| bugbear | 0.5 | 2 | 0.66666667 |
| desert-dwarf | 0.5 | 3 | 0.66666667 |
| desert-elf | 0.5 | 3 | 0.66666667 |
| drow | 0.5 | 4 | 1 |
| gnoll | 0.5 | 2 | 0.66666667 |
| goblin | 0 | — | — |
| gully-dwarf | 0.5 | 3 | 0.66666667 |
| high-elf | 0.5 | 4 | 1 |
| hill-dwarf | 0.5 | 4 | 1 |
| hobgoblin | 0.33333333 | 4 | 1 |
| human | 0.33333333 | 4 | 1 |
| kobold | 0.5 | 4 | 1 |
| sea-elf | 0.5 | 3 | 0.66666667 |
| snow-elf | 0.16666667 | 6 | 1.6666667 |
| wood-elf | 0.5 | 3 | 0.66666667 |
