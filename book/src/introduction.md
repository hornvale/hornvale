# Introduction

This is the project book of _Hornvale_: the living technical record of a
multiscalar world simulation observed through text. It describes the system
*as it actually exists* — its principles, its architecture, what each campaign
of work built, and what remains open.

It is deliberately not the [vision book](https://hornvale.github.io/book/),
which explains *why* this project exists and what it wants to feel like. Read
that one first if you want the soul; read this one if you want the machine.

## The altitude of this book

This book is written at a deliberate level of abstraction: technical,
mathematical, and conversational. Rust appears where it illuminates — a type
signature that *is* the design, a ten-line function worth savoring — but
never as a substitute for explanation. The test: every chapter must be fully
comprehensible without reading any code it shows, because this book is not a
code review. The code is the implementation; this book is the
*comprehension* — and keeping its principal human author fully cognizant of
the system is not a courtesy but a methodological requirement. Hornvale
claims research ambitions, and a result that no human comprehends is output,
not research.

(Interactive evaluation blocks — type a seed, watch a world generate, right
in the page — are planned once the kernel is compiled to WebAssembly. The
standard mdBook "run this code" button won't work here, since it executes on
the public Rust playground, which doesn't know Hornvale exists; our own
in-browser widgets will.)

## How this book stays honest

Documentation drifts. This book has two defenses:

1. **Generated artifacts.** Hornvale is deterministic by constitution: the
   same seed always produces the same world, byte for byte. So the Gallery's
   world documents are regenerated from pinned seeds, and they change *only
   when the system's behavior changes*. A changed artifact is a semantic
   changelog entry, legible without reading code — and CI regenerates the
   artifacts and fails if the committed copies differ, so the Gallery cannot
   silently go stale. (Generating the reference chapters from the live
   system is the remaining piece, planned for Campaign 1b.)

2. **The comprehension gate.** Every campaign closes by updating this book,
   and the update is reviewed by a human who must be able to explain the
   campaign from the book alone. If he can't, the chapter is rewritten until
   he can.

## Where things stand

**Campaigns 1a and 1b are complete.** The kernel exists (seeds, noise,
fields, the fact ledger, refinement, the trace protocol), and above it the
entire domain cascade exists at tier 0: a sun that never sets, a uniformly
mild climate, one vale, the goblin village of Gruugish, a five-rung caste
ladder, and one belief — derived, with queryable provenance, from the most
salient thing in the sky. The `hornvale` tool creates worlds, answers
questions about them interactively, and renders the almanac; an end-to-end
suite proves the whole path byte-deterministic. The world is very small, and
every part of it can explain itself, which is the entire point.

**Campaign 2 is complete: new worlds have real skies.** A generated world's
sun rises and sets on a seed-drawn day length; its moons show phases; its
night holds notable stars; its almanac has a Calendar. Worlds carry their
experimental configuration — the pins — as facts in their own ledgers, and
rebuild their skies from them at load. The campaign's exit criterion is
proven and published: the same seed's goblins worship a returning god under
a spinning sun and an unblinking one under a tidally locked sun, with
religion's code untouched — the enrichment thesis's first real data point.
Campaign 3 brings the land: terrain and climate, consuming the calendar the
sky now keeps.
