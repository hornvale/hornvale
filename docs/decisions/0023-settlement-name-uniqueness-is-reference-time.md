# 0023. Settlement-name uniqueness is a reference-time property

**Status:** Accepted (2026-07-09) · **Decider:** Nathan

In the context of Study 010's honestly-failed H4 — the measured settlement
name-collision rate (4.91%, 500-seed mean, pinned) missing its preregistered
bound (< 4.678%, twice the free-stem era's rate) after two design fixes had
already brought it down from a genuine 86% defect — facing the fact that the
bound was anchored to an era whose names *meant nothing* and therefore drew
from a flat, collision-free space, we decided that **uniqueness is a property
of a reference, not of a name: committed settlement names remain pure
per-`(seed, species, kind, salt)` draws that may collide, and any surface
that would be ambiguous (a document naming two same-named settlements)
disambiguates at render time from the entities' own site facts** — accepting
a small, measured, pinned base rate of full-name collisions as the honest
behavior of meaningful toponymy, exactly as Earth accepts its forty-one
Springfields.

**Context.** The Words gave names glossable meaning: a name compounds
site-descriptor words with a drawn stem. Meaning concentrates probability
mass — two taiga settlements *should* both want the ice word — so a bound
calibrated on meaningless stems measures the wrong thing. The constitutional
constraints (no re-draws, no shared "used" set, pin isolation by
construction; "spellings are views") already foreclose every in-name remedy
except stuffing more drawn entropy into the string, which lengthens names
without addressing the structural fact that meaning collides. The
reference-time move keeps the ledger pure and spends disambiguation only
where an actual ambiguity appears — the same lazy qualification natural
languages use ("Newcastle upon Tyne", "Springfield, Illinois").

**Consequence.** Study 010's H4 verdict stands as recorded — preregistered
bound, three measurements, failed — with this decision reframing rather than
retro-fitting it; the pinned 4.91% becomes the calibration baseline, and no
future work "fixes" the collision rate by adding entropy. A render-time
qualifier (almanac and REPL disambiguate co-occurring same-named settlements
from site facts, e.g. "Ice-Home (taiga)" / "Ice-Home of the kobolds") is a
registry vision row, buildable any time — it is a view, so it touches no
save-format contract. Collision pressure is expected to fall as a natural
by-product of world density: every future substrate that adds per-place
facts (fauna, flora, historical events, founders — LANG-5) widens the
descriptor space without touching the naming engine, and deep-time sound
change (MAP-3) will eventually differentiate collided names into distinct
fossils. The preregistration discipline (ADR 0016) is affirmed, not bent:
the claim was never tuned; the *decision* supersedes the bound in the open.

**See also.** Study 010 (`book/src/laboratory/study-010.md`); The Words spec
(`docs/superpowers/specs/2026-07-09-campaign-the-words-design.md`) §9;
decision 0016 (preregistration); registry rows LANG-4 (shipped), LANG-5
(descriptor breadth), MAP-3 (sound change).
