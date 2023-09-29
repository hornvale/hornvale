use crate::event::EventTag;

/// Rules for filtering event subscriptions.
#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Hash, Serialize)]
pub enum FilterRule {
  /// Always matches.
  Always,
  /// Never matches.
  Never,
  /// Matches the specified tag.
  MatchesTag(EventTag),
  /// Does not match the specified tag.
  DoesNotMatchTag(EventTag),
  /// Matches any of the specified tags.
  MatchesAnyOf(Vec<EventTag>),
  /// Matches all of the specified tags.
  MatchesAllOf(Vec<EventTag>),
  /// Matches none of the specified tags.
  MatchesNoneOf(Vec<EventTag>),
  /// Matches the following rules.
  MatchesRules(Vec<FilterRule>),
  /// Matches any of the following rules.
  MatchesAnyOfRules(Vec<FilterRule>),
  /// Matches all of the following rules.
  MatchesAllOfRules(Vec<FilterRule>),
  /// Matches none of the following rules.
  MatchesNoneOfRules(Vec<FilterRule>),
}

impl FilterRule {
  /// Returns `true` if the specified tag matches this rule.
  pub fn matches(&self, tag: &EventTag) -> bool {
    use FilterRule::*;
    match self {
      Always => true,
      Never => false,
      MatchesTag(rule_tag) => rule_tag == tag,
      DoesNotMatchTag(rule_tag) => rule_tag != tag,
      MatchesAnyOf(rule_tags) => rule_tags.contains(tag),
      MatchesAllOf(rule_tags) => rule_tags.iter().all(|rule_tag| rule_tag == tag),
      MatchesNoneOf(rule_tags) => rule_tags.iter().all(|rule_tag| rule_tag != tag),
      MatchesRules(rules) => rules.iter().any(|rule| rule.matches(tag)),
      MatchesAnyOfRules(rules) => rules.iter().any(|rule| rule.matches(tag)),
      MatchesAllOfRules(rules) => rules.iter().all(|rule| rule.matches(tag)),
      MatchesNoneOfRules(rules) => rules.iter().all(|rule| !rule.matches(tag)),
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_matches() {
    use FilterRule::*;
    let tag = EventTag::HasName("Test".to_string());
    assert!(Always.matches(&tag));
    assert!(!Never.matches(&tag));
    assert!(MatchesTag(tag.clone()).matches(&tag));
    assert!(!DoesNotMatchTag(tag.clone()).matches(&tag));
    assert!(MatchesAnyOf(vec![tag.clone()]).matches(&tag));
    assert!(MatchesAllOf(vec![tag.clone()]).matches(&tag));
    assert!(!MatchesNoneOf(vec![tag.clone()]).matches(&tag));
    assert!(MatchesRules(vec![MatchesTag(tag.clone())]).matches(&tag));
    assert!(MatchesAnyOfRules(vec![MatchesTag(tag.clone())]).matches(&tag));
    assert!(MatchesAllOfRules(vec![MatchesTag(tag.clone())]).matches(&tag));
    assert!(!MatchesNoneOfRules(vec![MatchesTag(tag.clone())]).matches(&tag));
  }
}
