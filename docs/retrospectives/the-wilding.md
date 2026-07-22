# The Wilding — retrospective

One page, process not product. Agentified the wild: `worldgen::wild_concentrations`
derives the world's beast concentrations from the coexistence stack, and
`vessel::derive_wild_npcs` mints them as NPCs appended to the peopled roster in the
possession session and the health sim — so The Quarry's predator-fear finally has a
vulnerable agent (a herbivore beast) to wake. The product work was small; the
campaign's real weight was an **upstream cleanup** it happened to uncover.

## What worked

- **The second consumer validated the abstraction for free.** `wild_concentrations`
  reused The Quarry's exact demography-fit shape (realized mass off the coexistence
  stack, encapsulated in the composition root; the vessel never touches demography).
  A pattern's second use is where you learn whether it generalized — this one did,
  with no new layering seam.

- **A new population that breaks a count is a scoping question, not a re-pin.** The
  wild NPCs walk (that is the point — The Quarry waking), so three possession tests
  asserting "the on-water settlement's NPCs never walk" broke on `agent_at_count == 0`
  → 114. Re-pinning to 114 would have *destroyed* the invariant. The right fix scoped
  those settled-population tests to a peoples-only session (a new `PossessOpts.wild_
  agents`, default on so the game/galleries keep the beasts) and added a positive test
  for the wild-motion case. When a new population trips an assertion, ask whether the
  invariant was about a *subset* before touching the number.

- **"Investigate first" caught a real judgment call hiding under mechanical re-pins.**
  A broad calibration failure looked like a uniform stale-pin sweep, but one member —
  a null control's *structural zero* (naming ⊥ pantheon structure) — was not a stale
  count; it had genuinely gone nonzero. Tracing the mechanism (the history-first
  epoch salts each people's genesis by `KindId`, so the re-keyed `goblin-twin` draws a
  slightly different baked history, and the pantheon derives from history-fed
  phenomena) showed it was a *benign, by-design* epoch consequence, not a determinism
  bug — a re-characterization, not a code fix. The diagnostic that isolated it was the
  asymmetry: head-domain stayed exactly 0 while cult-form and size drifted, pointing
  straight at "which draws route through history." A structural-zero invariant that
  breaks is never a blind re-pin — it is a fork (real regression vs epoch consequence)
  that must be resolved by mechanism.

## What the campaign taught

- **A large absorption that pulls in a census regen must be met with the FULL gate,
  not `gate-fast`.** Main was broadly red before this campaign began: The Living
  Community's census regen (commit `2c246ec1`) regenerated the drift-checked census
  fixtures but its promised "re-pin follows" never landed, so every calibration test
  that *loads* those fixtures (decision 0032) had been failing on main. The scoped
  `gate-fast` on a racing 46-commit absorption did not run the lab suite, so this
  session pushed the red main further before catching it. A census-regen commit
  silently staleifies every fixture-loading pin in the workspace; absorbing one is
  exactly the moment the full `cargo nextest run --workspace` earns its cost. This
  reinforces the standing "full gate before boundary changes" rule with a second
  trigger: **full gate after any absorption that moves a census fixture.**

- **A fast-moving main externalizes re-pin debt onto whoever next runs the full
  gate.** The Living Community deliberately deferred its census regen to the merge
  step (correctly — it belongs on the canonical box), but the *pin* follow-up was
  lost in the gap between the deferred regen and the branches that raced past it. The
  fix is the same discipline the closing skill already names: re-pin *in* the drifting
  commit, and when the drift is deferred, the deferral must carry the pins with it —
  a bare "re-pin follows" in a commit message is a debt with no owner.

## A follow-up worth promoting

The chase-back re-pinned ten calibration constants to the already-authoritative
fixture and re-characterized one structural-zero. That the whole sweep was *possible*
to do mechanically (the fixtures were correct; only the pins lagged) argues for a
cheap guard: a CI check that a census-fixture change in a commit and a calibration-pin
change are either co-present or explicitly waived — the "re-pin follows" footgun made
into a gate, so the next deferred regen cannot silently leave main red.
