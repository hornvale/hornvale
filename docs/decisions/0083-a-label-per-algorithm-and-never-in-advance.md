# 0083. A seed-derivation label is declared per *algorithm*, and never in advance

**Status:** Accepted (2026-07-28) · **Decider:** Nathan · **Refines:**
[0073](0073-epoch-granularity-is-declared.md),
[0072](0072-derived-geometry-is-causal.md)

In the context of the layout solver arriving with **two** interchangeable
methods behind one brief-selected seam, facing decision 0073's requirement that
a churny derivation split its labels by blast radius *before* the first bump, we
decided that **the unit of a seed-derivation label is the algorithm — one label
per method, fully qualified, declared as flat literals, and declared only for a
method that exists** — accepting that a change to something the methods *share*
must bump every one of them by hand.

**Context.** Decision 0072 made derived geometry causal, so a layout solve is a
determinism contract; 0073 fixed epoch granularity at *declaration*, because a
label's blast radius cannot be renegotiated after worlds carry it. The Blocking
is the first campaign to declare a label under both, and it declares two:
`room/layout/v1/rectilinear` (built places, BSP run inversely to *allocate*
space among known chambers) and `room/layout/v1/grown` (wild places, region
growing), selected on the brief's `built` field.

**Why the algorithm and not the subsystem.** Retuning the grower's flood order
has nothing whatever to do with the band a BSP cut may fall in. Under one shared
`room/layout/v1`, a tweak to the grower would relocate every *built* place's
floor plan in every world — an epoch charged to worlds that do not run the code
that changed. The label is a claim about which derivation produced these bytes,
and two independent derivations make two claims.

**Why a predicted method gets nothing.** The spec charts place kind against
wanted geometry and names two further methods as typed predictions — radial for
a temple, branching for a mine. Neither gets a label until it has code. A
published label is **permanent** (0073: granularity is fixed at declaration, and
a label accumulates and never decays), so pre-declaring is not cheap
future-proofing; it is minting a permanent row for an algorithm whose blast
radius nobody can yet know. The manifest would carry two rows that can never
differ from each other because neither has ever run.

**A correction worth recording, because the first argument for this was wrong.**
The initial justification offered was reoccupation: a place known first as a
cave and later as a house would flip methods, so per-method labels would keep the
two histories apart. That is false under either scheme — flipping a place's
method changes its layout because the *algorithm* changes, and a sub-label
prevents nothing there. The mechanism that actually justifies the split is epoch
blast radius, above; reoccupation only makes the coupling *observable* inside one
world's history. The record states the real reason so that a future reader
weighing a shared label does not test the wrong one and conclude the split was
unearned.

**Flat literals, not the macro's root-and-legs form.** Verified by reading the
seed macro (`kernel/src/seed.rs`) and the manifest generator
(`cli/src/streams.rs`): crates are sorted, but labels *within* a crate are
emitted in `stream_labels()` order, and the root+legs form emits its root and
legs **before** the flat block. Adopting it would have reordered the vessel's
four existing manifest rows — a diff that looks like four labels moving when
nothing moved at all. Flat literals append: the manifest diff for this campaign
is added rows and nothing displaced. The macro also documents `root:` as
*crate-wide*, which a subsystem root is not.

**Consequences.** The obligation the flat form creates is written into the
labels' own doc comment: a change to anything the two methods **share** — the
extent derivation, `Lattice`'s meaning, the cell-kind vocabulary — bumps *both*
literals, and forgetting one is a silently half-declared epoch. That obligation
is discharged by hand, and the only thing that currently catches a missed bump is
the artifact drift check. A content-addressed label — the label as a hash of the
algorithm's source, so an epoch cannot be forgotten — was considered and
rejected (a rename would churn world identity), but it names the residual hazard
honestly and is carried in the idea registry rather than dismissed.

A second consequence is cheerful: because `room/layout/v1/*` was declared *in*
this campaign and nothing on `main` draws from it, Task 4b's reification of a
wall from a boundary into a cell needed **no** bump. A label being authored is
not yet a label being versioned.

**See also.** [0073](0073-epoch-granularity-is-declared.md) (obligation 1, whose
first real exercise this is); [0072](0072-derived-geometry-is-causal.md) (why a
layout solve carries a label at all);
[0084](0084-an-epoch-is-declared-only-when-a-derivation-moved.md) (the other
half of this campaign's label reasoning — the bump that was *declined*);
[the metaplan](https://github.com/hornvale/hornvale/blob/main/docs/superpowers/specs/2026-07-25-the-rose-window-metaplan-design.md)
Amendment 2 §1b.7;
[The Blocking spec](https://github.com/hornvale/hornvale/blob/main/docs/superpowers/specs/2026-07-28-the-blocking-design.md)
§3.2 and §5.1; [The Blocking chronicle](../../book/src/chronicle/the-blocking.md).

Ratified at *The Blocking*'s merge gate.
