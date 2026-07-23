# 0068. The society vector gates on sociality, not sedentism

**Status:** Accepted (2026-07-23) · **Decider:** Nathan · **Refines:** [0067](0067-the-mind-society-vector-split.md)

Decision 0067 split `PsychVector` into `MindVector` and `SocietyVector` and gated
the society vector on `Settled` — `check_integrity`'s clause read *society
key-set == Settled key-set*. Nathan caught that this conflates two different
axes: **sociality** (having a society-mind — authority, status, an in-group) with
**sedentism** (forming fixed settlements). Hunter-gatherer bands, pastoralists,
and animal packs are the counterexample — intensely social, often *more*
egalitarian than settled hierarchies, yet mobile. Gating society on `Settled`
re-welds the society-mind onto settlement, the same category error the Dragons
program exists to dissolve (The Eremite decoupled *mind* from *peoplehood*; The
Cloister decoupled *individual mind* from *society mind* — and then leaned it back
onto sedentism).

**The refinement.** A society-mind is carried by every **minded** kind that lives
**socially**, not by the sedentary. `SocialForm::is_social()` (a `const fn`) is
true for `Gregarious` (packs/herds) and `Settled` (communities), false for
`Solitary` and `Sessile`. The `check_integrity` clause becomes:

> `society key-set == { k : psyche.contains(k) ∧ biosphere[k].social_form.is_social() }`

The `∧ minded` guard is load-bearing: a *mindless* social herd (a `Gregarious`
beast with no psyche) has no society-mind and carries no society vector; a *minded*
social kind must carry one; a `Solitary` minded kind (a dragon) must not.

**Determinism — byte-identical.** The only minded kinds in the current roster are
the four `Settled` peoples and the three `Solitary` dragons; there is no minded
`Gregarious` kind, so `{ minded ∧ social }` is *extensionally identical* to
`{ Settled }` today. No world, artifact, or census changes. The change is purely
to the invariant's **intension**: it stops rejecting a future minded mobile kind.
Verified: `check_integrity` accepts a minded `Gregarious` kind carrying a society
vector (a case the 0067 rule rejected), still rejects a `Solitary` kind carrying
one, and every existing test and artifact is unchanged.

**Why now, not deferred.** 0067's `society == Settled` was a determinism-critical
invariant that would have *actively rejected* the first minded nomadic people — a
`check_integrity` panic armed for exactly the Roster campaign that fills BIO-37's
**nomadic-band** grid cell. Relaxing the intension proactively is byte-identical
and disarms the trap before it is built upon.

**Scope.** This settles the *gating predicate* for the society vector. It does
**not** yet add a minded `Gregarious`/nomadic kind, nor resolve whether
`SocialForm` needs a distinct **nomadic-band** value (BIO-37's roster grid names
one, and today's `Gregarious` is animal-flavored — "an elk herd") — that is a
roster-design question for a later campaign. Settlement/demography placement
still gates on `Settled` (sedentism), correctly and separately.

**See also.** [The Cloister](../../book/src/chronicle/the-cloister.md) chronicle;
decision 0067; `domains/species/src/lib.rs` (`SocialForm::is_social`);
`windows/worldgen/src/components.rs` (`check_integrity`); BIO-37 (the sociality ×
lifespan roster grid) in the idea registry.
