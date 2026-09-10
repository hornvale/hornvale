# The Census Tail

**September 2026 · outcome: closeout candidate — the census was already near
its practical floor; the test suite was not**

The campaign began with the recent timing rows, asking a narrow question: where
are the automated tests spending wall time and CPU time, and which costs are
actually worth changing? The census had already benefited from the shared
seed-sweep runner and recent readout work. Its remaining cost is dominated by
the amount of world it measures and by the canonical-box cadence, so this
campaign did not disturb the measurement instrument or refresh its goldens.

The test suite had a different tail. Several long-running tests repeated the
same independent work over seed panels. They now use the existing
`seed_sweep::map_seeds` machinery: work is parallel, rows are reassembled in
seed order, and the tests retain their byte-identity witnesses. The converted
panels include history tithe, tumult cascade, health null-control, graded pin,
brattice solvability, terminator acceptance, repertory scenes, population
readouts, founder collisions, warrant glosses, breach panels, room
classification, and hearth population.

The result is a broad reduction in the suite's long pole rather than one
special-case speedup. Representative tests that had spent tens or hundreds of
seconds now complete in single-digit or low-tens seconds, while the full
sub-floor and enforcement gates remain green. The changes are deliberately
boring: no new dependency, no output change, no altered seed order, and no
census golden movement.

Two promising edges remain outside this closeout. `resident_folds` has a
partially prepared fold-and-sweep optimization, and `radiation_readout` has a
small three-seed conversion that still needs a clean latest-main validation.
They are preserved as follow-up work rather than smuggled into the campaign's
merge candidate.
