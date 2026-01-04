# Hornvale Implementation Plan

This document tracks future work. Completed phases have been archived; design decisions are documented in `.claude/ARCHITECTURE.md`.

---

## Phase 11: Sensory Propagation
**Goal**: Stimuli propagate through the room graph.
**Status**: Not Started (Future)

### Planned Deliverables
- [ ] `Stimulus` component (kind, intensity, origin)
- [ ] `PropagationRule` per stimulus kind (attenuation, blockers)
- [ ] `Perceives(observer, stimulus)` derived relation
- [ ] Sound, smell, danger as stimulus kinds
- [ ] Portals as edges that can transmit/block stimuli
- [ ] Per-observer sense modifiers (enhanced hearing, etc.)

**Use Cases**:
- "You hear shouting from the west"
- "Your sword glows blue" (danger sense through portal)
- Enhanced senses extend perception range

---

## Phase 12: Advanced Content
**Goal**: Build upward as desired.
**Status**: Not Started (Future)

These are optional content layers, not kernel features:

- [ ] **Combat** — HP, attacks, damage, death as rules over components
- [ ] **Geography** — Room/area generation from templates
- [ ] **History** — Forward simulation of civilizations/events
- [ ] **Cosmos** — Stars, planets, calendars, zodiacs (the long-term vision)

Each layer is optional. The kernel doesn't require any of them.

---

## Appendix: Risk Registry

| Risk                              | Likelihood | Impact | Mitigation                                      |
| --------------------------------- | ---------- | ------ | ----------------------------------------------- |
| Performance cliff at scale        | Medium     | High   | Phase 6 benchmarks, early profiling             |
| Query explosion (deep traversals) | Medium     | Medium | Depth limits, materialized views                |
| Cache invalidation storms         | Low        | High   | Stratified derivation, epoch-based invalidation |
| DSL expressiveness gaps           | Medium     | Medium | Rust stdlib escape hatch, iterate on syntax     |
| Scope creep (cosmos before house) | High       | High   | Strict phase ordering, playable demo first      |

---

## Appendix: Definition of Done

For any phase to be marked Complete:

- [ ] All deliverables checked off
- [ ] Tests written and passing
- [ ] No clippy warnings
- [ ] Benchmarks added (if applicable)
- [ ] ARCHITECTURE.md updated with design decisions
