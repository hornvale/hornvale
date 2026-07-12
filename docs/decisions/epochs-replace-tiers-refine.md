# Epochs replace, tiers refine — a contradicting generator cannot coexist as a tier

**Status:** Accepted (2026-07-11) · **Decider:** Nathan

In the context of the Crust epoch replacing the v1 "guitar-pick" terrain
generator, facing the question of whether v1 and v2 could *coexist* as
selectable tiers (the way `ConstantSun` coexists with the generated star
system), we decided: **no — a generator that contradicts its predecessor is an
epoch, not a tier, and it replaces rather than coexists.**

**The distinction.** The constitution's provider-tier doctrine (§2) permits
multiple tiers of the *same truth* to coexist: "higher fidelity refines, never
contradicts, lower — coarse constrains fine." `ConstantSun` and the generated
sky are both valid because the generated sky *refines* the constant one; every
fact the coarse tier asserts, the fine tier preserves. That is the licence for
coexistence.

Crust v2 **contradicts** v1: the same seed yields a different world — different
continents, different coastlines, different elevation at every cell. v2 does
not refine v1's continents; it overwrites them. A contradiction cannot be a
tier, because there is no "coarse truth" the two share. It is therefore an
**epoch**: a deliberate, one-way regeneration of world identity, marked by
retired stream labels (never reused, never redrawn — `plate-kind`), refrozen
fixtures, and a chronicle entry recording why.

**The load-bearing consequence: keystones frozen on pre-epoch reality retire at
the epoch.** An identity-keystone test asserts that some later campaign did not
perturb an *earlier* frozen snapshot (e.g. The Tongues / The Words asserting
that adding naming left the non-linguistic world untouched, measured against a
pre-campaign fixture). A terrain epoch deliberately changes that frozen world,
so the keystone's premise no longer holds — its evidentiary job was completed
at *its own* campaign's merge, and it retires at the epoch rather than being
mechanically "updated" (which would either make its `assert_ne!` vacuous or
force its exclusion list to swallow everything the epoch moved). Precedent:
commit `1bbc9fd` retired sibling pre-campaign fixtures at an earlier
regeneration; Crust retired the tongues/words identity keystones.

**Why not keep v1 as a fallback tier?** Because a coexisting v1 would have to be
*maintained* forever — every downstream domain (climate, placement,
paleoclimate) would carry a second code path for the guitar-pick world the
roadmap exists to abolish. The epoch discipline (retire, refreeze, chronicle)
is cheaper and honest: there is one terrain truth at a time, and its history is
the chronicle, not a live tier switch.

**Scope.** This applies again at Sculpting (terrain epoch v3) and at any future
generator change that alters — rather than refines — the worlds a prior
generator produced. Tier coexistence remains correct for genuine
fidelity levels of one truth (the sky tiers, and crust *fields* sampled at
different resolutions per `identity-computes-on-the-canonical-grid`).
