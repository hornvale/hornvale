//! Rule indexing for O(1) rule lookups by trigger type.
//!
//! The index is mandatory for performance - without it, rule lookup
//! becomes O(all rules) which kills performance at scale.
//!
//! ## Index Buckets
//!
//! Rules are indexed by trigger type:
//! - `by_periodic`: All rules with periodic triggers
//! - `by_before`: Before hooks by action name
//! - `by_on`: On hooks by action name
//! - `by_after`: After hooks by action name
//! - `by_derive`: Derivation rules by property name
//!
//! ## Priority Ordering
//!
//! Within each bucket, rules are ordered by priority (higher priority first).
//! This ensures deterministic evaluation order.

use im::OrdMap;

use crate::symbol::Symbol;

use super::rule::{Rule, Trigger};

/// Index for O(1) rule lookups by trigger type.
///
/// This is a mandatory optimization - the kernel owns index correctness
/// and rebuilds it when rules change.
#[derive(Debug, Clone, Default)]
pub struct RuleIndex {
    /// Indexes into rules vec for periodic rules.
    by_periodic: Vec<usize>,
    /// Indexes for Before hooks by action name.
    by_before: OrdMap<Symbol, Vec<usize>>,
    /// Indexes for On hooks by action name.
    by_on: OrdMap<Symbol, Vec<usize>>,
    /// Indexes for After hooks by action name.
    by_after: OrdMap<Symbol, Vec<usize>>,
    /// Indexes for Derive rules by property name.
    by_derive: OrdMap<Symbol, Vec<usize>>,
    /// Whether the index needs rebuilding.
    dirty: bool,
}

impl RuleIndex {
    /// Create a new empty index.
    pub fn new() -> Self {
        Self::default()
    }

    /// Check if the index needs rebuilding.
    pub fn is_dirty(&self) -> bool {
        self.dirty
    }

    /// Mark the index as needing rebuild.
    pub fn mark_dirty(&mut self) {
        self.dirty = true;
    }

    /// Rebuild the index from the given rules.
    ///
    /// This clears the existing index and rebuilds it from scratch.
    /// Rules are sorted by priority within each bucket.
    pub fn rebuild(&mut self, rules: &[Rule]) {
        self.clear();

        for (idx, rule) in rules.iter().enumerate() {
            self.insert_rule(idx, &rule.trigger);
        }

        // Sort by priority within each bucket
        self.sort_by_priority(rules);

        self.dirty = false;
    }

    /// Clear all index entries.
    fn clear(&mut self) {
        self.by_periodic.clear();
        self.by_before = OrdMap::new();
        self.by_on = OrdMap::new();
        self.by_after = OrdMap::new();
        self.by_derive = OrdMap::new();
    }

    /// Insert a rule into the appropriate bucket.
    fn insert_rule(&mut self, idx: usize, trigger: &Trigger) {
        match trigger {
            Trigger::Periodic { .. } => {
                self.by_periodic.push(idx);
            }
            Trigger::Before(action) => {
                self.by_before.entry(*action).or_default().push(idx);
            }
            Trigger::On(action) => {
                self.by_on.entry(*action).or_default().push(idx);
            }
            Trigger::After(action) => {
                self.by_after.entry(*action).or_default().push(idx);
            }
            Trigger::Derive(property) => {
                self.by_derive.entry(*property).or_default().push(idx);
            }
        }
    }

    /// Sort all buckets by rule priority (higher priority first).
    fn sort_by_priority(&mut self, rules: &[Rule]) {
        // Sort by priority (descending - higher priority first)
        let get_priority = |idx: &usize| -> i32 { -rules[*idx].priority };

        self.by_periodic.sort_by_key(get_priority);

        // im::OrdMap doesn't have values_mut, so we rebuild with sorted buckets
        self.by_before = self.sort_buckets(&self.by_before, rules);
        self.by_on = self.sort_buckets(&self.by_on, rules);
        self.by_after = self.sort_buckets(&self.by_after, rules);
        self.by_derive = self.sort_buckets(&self.by_derive, rules);
    }

    /// Sort all buckets in an OrdMap by rule priority.
    fn sort_buckets(
        &self,
        map: &OrdMap<Symbol, Vec<usize>>,
        rules: &[Rule],
    ) -> OrdMap<Symbol, Vec<usize>> {
        let get_priority = |idx: &usize| -> i32 { -rules[*idx].priority };
        let mut result = OrdMap::new();
        for (key, bucket) in map {
            let mut sorted_bucket = bucket.clone();
            sorted_bucket.sort_by_key(get_priority);
            result.insert(*key, sorted_bucket);
        }
        result
    }

    /// Get indexes for periodic rules.
    pub fn periodic(&self) -> &[usize] {
        &self.by_periodic
    }

    /// Get indexes for Before hooks for an action.
    pub fn before(&self, action: Symbol) -> &[usize] {
        self.by_before
            .get(&action)
            .map(|v| v.as_slice())
            .unwrap_or(&[])
    }

    /// Get indexes for On hooks for an action.
    pub fn on(&self, action: Symbol) -> &[usize] {
        self.by_on.get(&action).map(|v| v.as_slice()).unwrap_or(&[])
    }

    /// Get indexes for After hooks for an action.
    pub fn after(&self, action: Symbol) -> &[usize] {
        self.by_after
            .get(&action)
            .map(|v| v.as_slice())
            .unwrap_or(&[])
    }

    /// Get indexes for derivation rules for a property.
    pub fn derive(&self, property: Symbol) -> &[usize] {
        self.by_derive
            .get(&property)
            .map(|v| v.as_slice())
            .unwrap_or(&[])
    }

    /// Get all hook indexes (Before, On, After) for an action.
    pub fn all_hooks(&self, action: Symbol) -> impl Iterator<Item = usize> + '_ {
        self.before(action)
            .iter()
            .chain(self.on(action).iter())
            .chain(self.after(action).iter())
            .copied()
    }

    /// Get the total number of indexed rules.
    pub fn len(&self) -> usize {
        let hook_count: usize = self
            .by_before
            .values()
            .chain(self.by_on.values())
            .chain(self.by_after.values())
            .chain(self.by_derive.values())
            .map(|v| v.len())
            .sum();
        self.by_periodic.len() + hook_count
    }

    /// Check if the index is empty.
    pub fn is_empty(&self) -> bool {
        self.by_periodic.is_empty()
            && self.by_before.is_empty()
            && self.by_on.is_empty()
            && self.by_after.is_empty()
            && self.by_derive.is_empty()
    }

    /// Get all unique action names that have hooks.
    pub fn hook_actions(&self) -> impl Iterator<Item = Symbol> + '_ {
        self.by_before
            .keys()
            .chain(self.by_on.keys())
            .chain(self.by_after.keys())
            .copied()
            .collect::<std::collections::BTreeSet<_>>()
            .into_iter()
    }

    /// Get all unique property names that have derivation rules.
    pub fn derive_properties(&self) -> impl Iterator<Item = Symbol> + '_ {
        self.by_derive.keys().copied()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rules::{Effect, Pattern, Rule};

    fn make_periodic_rule(name: &str, interval: u64) -> Rule {
        Rule::new(
            name,
            Pattern::entity("?e"),
            Trigger::every(interval),
            Effect::no_op(),
        )
    }

    fn make_before_rule(name: &str, action: &str) -> Rule {
        Rule::new(
            name,
            Pattern::entity("?e"),
            Trigger::before(action),
            Effect::no_op(),
        )
    }

    fn make_on_rule(name: &str, action: &str) -> Rule {
        Rule::new(
            name,
            Pattern::entity("?e"),
            Trigger::on(action),
            Effect::no_op(),
        )
    }

    fn make_after_rule(name: &str, action: &str) -> Rule {
        Rule::new(
            name,
            Pattern::entity("?e"),
            Trigger::after(action),
            Effect::no_op(),
        )
    }

    fn make_derive_rule(name: &str, property: &str) -> Rule {
        Rule::new(
            name,
            Pattern::entity("?e"),
            Trigger::derive(property),
            Effect::no_op(),
        )
    }

    #[test]
    fn test_empty_index() {
        let index = RuleIndex::new();
        assert!(index.is_empty());
        assert_eq!(index.len(), 0);
        assert!(!index.is_dirty());
    }

    #[test]
    fn test_rebuild_indexes_rules() {
        let rules = vec![
            make_periodic_rule("tick-rule", 10),
            make_before_rule("before-take", "take"),
            make_on_rule("on-burn", "burn"),
            make_after_rule("after-open", "open"),
            make_derive_rule("derive-fire", "FireResistance"),
        ];

        let mut index = RuleIndex::new();
        index.rebuild(&rules);

        assert!(!index.is_dirty());
        assert_eq!(index.len(), 5);
        assert!(!index.is_empty());
    }

    #[test]
    fn test_periodic_lookup() {
        let rules = vec![
            make_periodic_rule("tick-1", 10),
            make_before_rule("not-periodic", "take"),
            make_periodic_rule("tick-2", 20),
        ];

        let mut index = RuleIndex::new();
        index.rebuild(&rules);

        let periodic = index.periodic();
        assert_eq!(periodic.len(), 2);
        assert!(periodic.contains(&0)); // tick-1
        assert!(periodic.contains(&2)); // tick-2
    }

    #[test]
    fn test_before_hook_lookup() {
        let rules = vec![
            make_before_rule("before-take-1", "take"),
            make_before_rule("before-take-2", "take"),
            make_before_rule("before-burn", "burn"),
            make_on_rule("on-take", "take"), // Not Before
        ];

        let mut index = RuleIndex::new();
        index.rebuild(&rules);

        let take_sym = Symbol::new("take");
        let burn_sym = Symbol::new("burn");
        let drop_sym = Symbol::new("drop");

        assert_eq!(index.before(take_sym).len(), 2);
        assert_eq!(index.before(burn_sym).len(), 1);
        assert_eq!(index.before(drop_sym).len(), 0);
    }

    #[test]
    fn test_on_hook_lookup() {
        let rules = vec![
            make_on_rule("on-burn-1", "burn"),
            make_on_rule("on-burn-2", "burn"),
        ];

        let mut index = RuleIndex::new();
        index.rebuild(&rules);

        let burn_sym = Symbol::new("burn");
        assert_eq!(index.on(burn_sym).len(), 2);
    }

    #[test]
    fn test_after_hook_lookup() {
        let rules = vec![make_after_rule("after-open", "open")];

        let mut index = RuleIndex::new();
        index.rebuild(&rules);

        let open_sym = Symbol::new("open");
        assert_eq!(index.after(open_sym).len(), 1);
    }

    #[test]
    fn test_derive_lookup() {
        let rules = vec![
            make_derive_rule("fire-1", "FireResistance"),
            make_derive_rule("fire-2", "FireResistance"),
            make_derive_rule("cold-1", "ColdResistance"),
        ];

        let mut index = RuleIndex::new();
        index.rebuild(&rules);

        let fire_sym = Symbol::new("FireResistance");
        let cold_sym = Symbol::new("ColdResistance");
        let poison_sym = Symbol::new("PoisonResistance");

        assert_eq!(index.derive(fire_sym).len(), 2);
        assert_eq!(index.derive(cold_sym).len(), 1);
        assert_eq!(index.derive(poison_sym).len(), 0);
    }

    #[test]
    fn test_all_hooks_for_action() {
        let rules = vec![
            make_before_rule("before-take", "take"),
            make_on_rule("on-take", "take"),
            make_after_rule("after-take", "take"),
            make_on_rule("on-burn", "burn"), // Different action
        ];

        let mut index = RuleIndex::new();
        index.rebuild(&rules);

        let take_sym = Symbol::new("take");
        let take_hooks: Vec<_> = index.all_hooks(take_sym).collect();
        assert_eq!(take_hooks.len(), 3);
    }

    #[test]
    fn test_dirty_flag() {
        let mut index = RuleIndex::new();
        assert!(!index.is_dirty());

        index.mark_dirty();
        assert!(index.is_dirty());

        index.rebuild(&[]);
        assert!(!index.is_dirty());
    }

    #[test]
    fn test_hook_actions() {
        let rules = vec![
            make_before_rule("before-take", "take"),
            make_on_rule("on-take", "take"),
            make_after_rule("after-burn", "burn"),
        ];

        let mut index = RuleIndex::new();
        index.rebuild(&rules);

        let actions: Vec<_> = index.hook_actions().collect();
        assert_eq!(actions.len(), 2);
        assert!(actions.contains(&Symbol::new("take")));
        assert!(actions.contains(&Symbol::new("burn")));
    }

    #[test]
    fn test_derive_properties() {
        let rules = vec![
            make_derive_rule("fire-1", "FireResistance"),
            make_derive_rule("cold-1", "ColdResistance"),
        ];

        let mut index = RuleIndex::new();
        index.rebuild(&rules);

        let props: Vec<_> = index.derive_properties().collect();
        assert_eq!(props.len(), 2);
    }

    #[test]
    fn test_rebuild_clears_old_index() {
        let rules1 = vec![make_periodic_rule("old-rule", 10)];
        let rules2 = vec![make_on_rule("new-rule", "burn")];

        let mut index = RuleIndex::new();
        index.rebuild(&rules1);
        assert_eq!(index.periodic().len(), 1);

        // Rebuild with different rules
        index.rebuild(&rules2);
        assert_eq!(index.periodic().len(), 0);
        assert_eq!(index.on(Symbol::new("burn")).len(), 1);
    }

    #[test]
    fn test_priority_ordering() {
        // Create rules with different priorities
        let rules = vec![
            Rule::with_priority(
                "low-priority",
                Pattern::entity("?e"),
                Trigger::on("burn"),
                Effect::no_op(),
                10,
            ),
            Rule::with_priority(
                "high-priority",
                Pattern::entity("?e"),
                Trigger::on("burn"),
                Effect::no_op(),
                100,
            ),
            Rule::with_priority(
                "medium-priority",
                Pattern::entity("?e"),
                Trigger::on("burn"),
                Effect::no_op(),
                50,
            ),
        ];

        let mut index = RuleIndex::new();
        index.rebuild(&rules);

        let burn_sym = Symbol::new("burn");
        let burn_indexes = index.on(burn_sym);

        // Should be sorted by priority (highest first)
        // Index 1 has priority 100, index 2 has priority 50, index 0 has priority 10
        assert_eq!(burn_indexes.len(), 3);
        assert_eq!(burn_indexes[0], 1); // high-priority (100)
        assert_eq!(burn_indexes[1], 2); // medium-priority (50)
        assert_eq!(burn_indexes[2], 0); // low-priority (10)
    }
}
