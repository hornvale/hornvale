# 0208. Completing `corner_weights`'s key retired its level guard

**Status:** Accepted (2026-08-23) · **Decider:** Nathan

In the context of migrating `RoomMeshMemo`'s `corner_weights` half onto
`Derived`, facing a `debug_assert_eq!` guard and a companion test,
`corner_weights_memo_asserts_against_geo_level_aliasing`
(`#[should_panic(expected = "RoomMeshMemo reused across two different globe
levels")]`), whose own doc comment called itself "not a full fix," we decided
to **fold `Geosphere::level()` into the memo's key and delete the guard
outright rather than strengthen it**, accepting that the `should_panic` test
had to be replaced, not merely edited, by a correctness test over two levels.

**Not "the guard was already total."** An earlier draft of this campaign's
spec concluded that, since `Geosphere::new(level: u32)` is the only
constructor and `grep -n Seed kernel/src/geosphere.rs` returns nothing, two
geospheres at the same level are byte-identical — so the guard's own claimed
gap ("two different geospheres at the SAME level would not be caught") could
never produce a wrong answer. That is true, but it is the uninteresting
finding, made only about the guard's coverage against a bug that could not
happen.

**The interesting finding: the guard existed because the key was
incomplete.** A `RoomAddr` alone did not name which `Geosphere` resolved it,
so feeding one memo from a level-2 and then a level-3 geosphere was a caller
bug no type prevented — the guard was runtime insurance against exactly that
gap, panicking rather than silently returning geometry computed at the wrong
level. Once the level joins the key (`(RoomAddr, u32)`), two levels become
two different keys: the memo holds both and answers each correctly. There is
nothing left to panic about, because the bug class the guard watched for no
longer exists. Completing the key **deletes the bug class**, rather than
proving the guard total against it.

**Capability gain, not merely a removal.** One memo may now legitimately
serve several globe levels — the cross-world sharing the census (thousands of
worlds in one process) wants — where the guard previously forced a caller to
throw the memo away and start a fresh one at every level change. This is
spec §2.3's key-completeness obligation demonstrated on the campaign's own
first tenant.

**The should_panic test was replaced, not deleted.** Dropping a
`#[should_panic]` test without replacing its assertion would be a silent loss
of coverage — the guard's absence would go untested rather than the property
that made it unnecessary going tested. `corner_weights_memo_serves_two_globe_levels_correctly`
(`kernel/src/room.rs`) replaced
`corner_weights_memo_asserts_against_geo_level_aliasing`: it feeds one memo
from a level-2 and a level-3 geosphere, as the old test did, and asserts both
return the *correct* `corner_weights` rather than requiring a panic. This is
the one exception to the migration's "if a pre-existing test needs editing,
STOP" rule (spec §2.1b), because the test's own subject — the bug it
guarded against — was retired by the same change that required editing it.

**Consequence.** `RoomMeshMemo::corner_weights_geo_level` survives as
read-path plumbing only (the first level ever inserted, for
`corner_weights_lookup`'s bare-`RoomAddr` callers and a read-side parity
check in `windows/locale`), never again as a write-path aliasing guard.
Nothing in the type or the runtime forbids a second, different level from
being inserted into the same memo — that is exactly the mixing this campaign
made legitimate.

**See also.** Spec §2.1, §2.1b, §8 item 3 (`docs/superpowers/specs/
2026-08-23-the-forebay-design.md`); decision 0206 (the `Pure`/`Ledger` split
that makes `corner_weights`'s key-completeness the load-bearing property);
decision 0207 (the store shape `corner_weights` migrated onto).
