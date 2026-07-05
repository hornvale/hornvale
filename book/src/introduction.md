# Introduction

This is the project book of _Hornvale_: the living technical record of a
multiscalar world simulation observed through text. It describes the system
*as it actually exists* — its principles, its architecture, what each campaign
of work built, and what remains open.

It is deliberately not the [vision book](https://hornvale.github.io/book/),
which explains *why* this project exists and what it wants to feel like. Read
that one first if you want the soul; read this one if you want the machine.

## The altitude of this book

This book is written at a deliberate level of abstraction: technical and
mathematical, but with **no source code**. If a chapter cannot explain a
subsystem without showing Rust, the chapter has failed and gets rewritten.
The code is the implementation; this book is the *comprehension* — and keeping
its principal human author fully cognizant of the system is not a courtesy
but a methodological requirement. Hornvale claims research ambitions, and a
result that no human comprehends is output, not research.

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

**Campaign 1a is complete:** the kernel exists — seeds, noise, fields, the
fact ledger, trivial refinement, and the trace protocol's vocabulary — with
a determinism suite proving that identical seeds produce byte-identical
worlds. There are no domains yet, no REPL, no almanac; those are Campaign 1b.
The world of seed 42 currently contains one valley, one village, and one
revered phenomenon. It is very small, and it is *provably the same world
every time*, which is the entire point of starting here.
