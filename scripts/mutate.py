#!/usr/bin/env python3
"""Apply one text substitution to a source file, refusing to no-op silently.

    python3 scripts/mutate.py <file> <old> <new>
    python3 scripts/mutate.py --to <dest> <file> <old> <new>

`--to <dest>` writes the mutated text to <dest> and leaves <file> untouched.
That is the right shape for mutating something that is EXECUTED FROM A PATH
rather than imported — a git hook, a shell script under test — because the
caller then runs <dest> and has nothing to restore, removing the restore step
that is itself a documented failure mode below.

It was added because callers were already asking for it and silently not
getting it: `scripts/test-pre-push.sh` passed `--to` to a version that took
exactly three arguments, so every run failed the argument check and fell
through to a `|| sed ...` fallback. The mutation still happened, via the tool
this file exists to replace — and `sed` is precisely the tool that CANNOT
report a no-op, which is the whole reason for the assertions here.

This exists for MUTATION TESTING — neutralise a line, run the suite, and read
the red as proof that some assertion is actually holding the behaviour. The
whole value of that procedure rests on the substitution having *happened*, and
that is exactly the step this project has now got wrong twice:

  - The Axes (retrospective §3): `cargo fmt` had rewrapped a single-line
    constructor into eight lines, so a replacement of the one-line form matched
    nothing. The file was left unchanged, the suite reported `ok`, and that
    `ok` was indistinguishable from a robust implementation.
  - The Underworld, twice more during its own fix rounds.

So this script asserts two things before it writes anything, and dies loudly
on either:

  1. `old` OCCURS in the file. A pattern that matches nothing is the failure
     above; an unchanged file must never be mistaken for a surviving assertion.
  2. `old` occurs EXACTLY ONCE. A pattern that matches three call sites
     mutates three things at once, and the resulting red says nothing about
     which one an assertion caught. Narrow the pattern instead — include
     surrounding lines until it is unique.

It deliberately does NOT restore. Restoring is the caller's job and should be
`cp` from a copy taken before the first mutation, never `git checkout -- <file>`
— that reverts UNCOMMITTED work in the same file along with the mutation, and
The Axes (retrospective §4) read the resulting absence of a test as that test
having passed.

Exit status is 0 only if the file was written.
"""

import pathlib
import sys


def main(argv: list[str]) -> int:
    argv = list(argv)
    dest = None
    if len(argv) > 1 and argv[1] == "--to":
        if len(argv) < 3:
            print(__doc__, file=sys.stderr)
            print("error: --to requires a destination path", file=sys.stderr)
            return 2
        dest = pathlib.Path(argv[2])
        del argv[1:3]

    if len(argv) != 4:
        print(__doc__, file=sys.stderr)
        print("error: expected exactly three arguments", file=sys.stderr)
        return 2

    path = pathlib.Path(argv[1])
    old, new = argv[2], argv[3]

    if old == new:
        print("error: old and new are identical — this would be a no-op", file=sys.stderr)
        return 2

    try:
        source = path.read_text()
    except OSError as exc:
        print(f"error: cannot read {path}: {exc}", file=sys.stderr)
        return 2

    occurrences = source.count(old)
    if occurrences == 0:
        print(
            f"error: TARGET NOT FOUND in {path}.\n"
            f"  Nothing was written. An unchanged file is NOT a surviving assertion —\n"
            f"  the usual cause is a `cargo fmt` reflow that rewrapped the target.\n"
            f"  target: {old!r}",
            file=sys.stderr,
        )
        return 1
    if occurrences > 1:
        print(
            f"error: TARGET NOT UNIQUE in {path} ({occurrences} occurrences).\n"
            f"  Nothing was written. Mutating several sites at once makes the red\n"
            f"  unattributable; widen the pattern until it matches exactly one.\n"
            f"  target: {old!r}",
            file=sys.stderr,
        )
        return 1

    target = dest if dest is not None else path
    target.write_text(source.replace(old, new))
    if dest is not None:
        target.chmod(path.stat().st_mode)
    print(f"MUTATION APPLIED to {target}: {old!r} -> {new!r}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
