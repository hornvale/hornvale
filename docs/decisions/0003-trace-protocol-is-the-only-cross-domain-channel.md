# 0003. The trace protocol is the only cross-domain channel

**Status:** Accepted (2026-07-05) · **Decider:** Nathan

In the context of domains that must enrich one another (astronomy shaping
religion, climate shaping settlement) while staying decoupled
([0002](0002-domains-depend-only-on-kernel.md)), facing the question of *how*
they communicate without knowing about each other, we decided that **all
cross-domain communication flows through three kernel vocabularies — facts,
phenomena, and fields** — accepting that a consumer may never learn which
system produced a given observation.

**Context.** Religion must be able to mythologize the sky without any
dependency on astronomy's implementation; when astronomy gains moons, religion
must react without being edited. The read side (phenomena) is deliberately
anonymous and salience-ranked.

**Consequence.** Facts are an append-only, contradiction-checked
subject/predicate/object envelope. Phenomena are the universal, source-blind
read. Fields are typed functions over (space × time). This is the project's
central intellectual artifact; each vocabulary co-evolves with a book chapter.

**See also.** Spec §3.1.6 (The Trace Protocol); `CLAUDE.md` "Architecture".
