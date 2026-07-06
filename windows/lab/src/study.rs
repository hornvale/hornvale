//! JSON study format with loud validation for batch world generation experiments.

use crate::{Metric, registry};
use hornvale_astronomy::{SkyPins, parse_pin};
use std::fs;
use std::path::Path;

/// Random number generator initialization: starting seed and count of worlds to generate.
#[derive(Debug, Clone, PartialEq, serde::Deserialize)]
pub struct Seeds {
    /// The starting seed value.
    pub from: u64,
    /// The number of worlds to generate.
    pub count: u64,
}

/// A named set of sky pins applied to all worlds in a study.
#[derive(Debug, Clone, PartialEq, serde::Deserialize)]
pub struct PinSet {
    /// The label identifying this pin set (e.g., "default", "locked").
    pub label: String,
    /// Pin specifications in "key=value" format (e.g., ["moons=3", "day-hours=24"]).
    pub pins: Vec<String>,
}

/// Metric selection: either "all" metrics or a named subset.
#[derive(Debug, Clone, PartialEq, serde::Deserialize)]
#[serde(untagged)]
pub enum MetricSelection {
    /// Analyze all metrics in the registry.
    All(String),
    /// Analyze only the named metrics.
    Named(Vec<String>),
}

/// A batch study specification: worlds, pin sets, and metrics.
#[derive(Debug, Clone, PartialEq, serde::Deserialize)]
pub struct Study {
    /// The study identifier (lowercase, digits, hyphens only; non-empty).
    pub name: String,
    /// Human-readable description.
    pub description: String,
    /// Seed configuration.
    pub seeds: Seeds,
    /// Pin sets to apply.
    pub pin_sets: Vec<PinSet>,
    /// Which metrics to extract.
    pub metrics: MetricSelection,
}

/// A loud error from study loading or validation.
#[derive(Debug, Clone, PartialEq)]
pub struct StudyError {
    /// The error message, including the offending value when applicable.
    pub message: String,
}

impl std::fmt::Display for StudyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "study: {}", self.message)
    }
}

impl std::error::Error for StudyError {}

/// Load and validate a study from a JSON file.
pub fn load_study(path: &Path) -> Result<Study, StudyError> {
    let contents = fs::read_to_string(path).map_err(|e| StudyError {
        message: format!("failed to read {}: {}", path.display(), e),
    })?;

    let study: Study = serde_json::from_str(&contents).map_err(|e| StudyError {
        message: format!("failed to parse {}: {}", path.display(), e),
    })?;

    study.validate()?;
    Ok(study)
}

impl Study {
    /// Validate the study against all rules.
    ///
    /// Rules:
    /// - name is non-empty and contains only lowercase ASCII, digits, and hyphens
    /// - seeds.count > 0
    /// - at least one pin set exists
    /// - no duplicate pin set labels
    /// - every pin parses successfully
    /// - MetricSelection::All string equals "all"
    /// - every named metric exists in registry
    pub fn validate(&self) -> Result<(), StudyError> {
        // Check name
        if self.name.is_empty() {
            return Err(StudyError {
                message: format!("name must be lowercase-kebab, got {:#?}", self.name),
            });
        }
        for c in self.name.chars() {
            if !c.is_ascii_lowercase() && !c.is_ascii_digit() && c != '-' {
                return Err(StudyError {
                    message: format!("name must be lowercase-kebab, got {:#?}", self.name),
                });
            }
        }

        // Check seeds.count
        if self.seeds.count == 0 {
            return Err(StudyError {
                message: "seeds count must be positive".to_string(),
            });
        }

        // Check at least one pin set
        if self.pin_sets.is_empty() {
            return Err(StudyError {
                message: "at least one pin set is required".to_string(),
            });
        }

        // Check for duplicate pin set labels
        let mut seen_labels = std::collections::HashSet::new();
        for pin_set in &self.pin_sets {
            if !seen_labels.insert(&pin_set.label) {
                return Err(StudyError {
                    message: format!("duplicate pin-set label {:#?}", pin_set.label),
                });
            }
        }

        // Check that every pin parses
        for pin_set in &self.pin_sets {
            for pin_str in &pin_set.pins {
                let mut test_pins = SkyPins::default();
                if let Err(reason) = parse_pin(pin_str, &mut test_pins) {
                    return Err(StudyError {
                        message: format!(
                            "invalid pin {:#?} in set {:#?}: {}",
                            pin_str, pin_set.label, reason
                        ),
                    });
                }
            }
        }

        // Check metrics
        match &self.metrics {
            MetricSelection::All(s) => {
                if s != "all" {
                    return Err(StudyError {
                        message: format!(
                            "metrics must be \"all\" or an array of metric names, got {:#?}",
                            s
                        ),
                    });
                }
            }
            MetricSelection::Named(names) => {
                let reg = registry();
                let valid_names: std::collections::HashSet<_> =
                    reg.iter().map(|m| m.name).collect();
                for name in names {
                    if !valid_names.contains(name.as_str()) {
                        return Err(StudyError {
                            message: format!("unknown metric {:#?}", name),
                        });
                    }
                }
            }
        }

        Ok(())
    }

    /// Get the selected metrics in registry order.
    pub fn selected_metrics(&self) -> Result<Vec<Metric>, StudyError> {
        let reg = registry();
        match &self.metrics {
            MetricSelection::All(_) => Ok(reg),
            MetricSelection::Named(names) => {
                let name_set: std::collections::HashSet<_> = names.iter().cloned().collect();
                Ok(reg
                    .into_iter()
                    .filter(|m| name_set.contains(m.name))
                    .collect())
            }
        }
    }

    /// Parse all pin sets and return label+SkyPins pairs.
    pub fn pin_sets_parsed(&self) -> Result<Vec<(String, SkyPins)>, StudyError> {
        let mut result = Vec::new();
        for pin_set in &self.pin_sets {
            let mut pins = SkyPins::default();
            for pin_str in &pin_set.pins {
                if let Err(reason) = parse_pin(pin_str, &mut pins) {
                    return Err(StudyError {
                        message: format!(
                            "invalid pin {:#?} in set {:#?}: {}",
                            pin_str, pin_set.label, reason
                        ),
                    });
                }
            }
            result.push((pin_set.label.clone(), pins));
        }
        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn valid_study_parses_and_validates() {
        let json = r#"{
            "name": "my-study",
            "description": "A test study",
            "seeds": { "from": 1, "count": 10 },
            "pin_sets": [
                { "label": "default", "pins": [] }
            ],
            "metrics": "all"
        }"#;

        let study: Study = serde_json::from_str(json).unwrap();
        assert_eq!(study.name, "my-study");
        assert_eq!(study.description, "A test study");
        assert_eq!(study.seeds.from, 1);
        assert_eq!(study.seeds.count, 10);
        assert_eq!(study.pin_sets.len(), 1);
        assert!(study.validate().is_ok());
    }

    #[test]
    fn name_must_be_lowercase_kebab() {
        let json = r#"{
            "name": "MyStudy",
            "description": "Test",
            "seeds": { "from": 1, "count": 10 },
            "pin_sets": [{ "label": "default", "pins": [] }],
            "metrics": "all"
        }"#;

        let study: Study = serde_json::from_str(json).unwrap();
        let err = study.validate().unwrap_err();
        assert!(err.message.contains("name must be lowercase-kebab"));
    }

    #[test]
    fn name_cannot_be_empty() {
        let json = r#"{
            "name": "",
            "description": "Test",
            "seeds": { "from": 1, "count": 10 },
            "pin_sets": [{ "label": "default", "pins": [] }],
            "metrics": "all"
        }"#;

        let study: Study = serde_json::from_str(json).unwrap();
        let err = study.validate().unwrap_err();
        assert!(err.message.contains("name must be lowercase-kebab"));
    }

    #[test]
    fn name_with_special_chars_rejected() {
        let json = r#"{
            "name": "my@study",
            "description": "Test",
            "seeds": { "from": 1, "count": 10 },
            "pin_sets": [{ "label": "default", "pins": [] }],
            "metrics": "all"
        }"#;

        let study: Study = serde_json::from_str(json).unwrap();
        let err = study.validate().unwrap_err();
        assert!(err.message.contains("name must be lowercase-kebab"));
    }

    #[test]
    fn seeds_count_must_be_positive() {
        let json = r#"{
            "name": "my-study",
            "description": "Test",
            "seeds": { "from": 1, "count": 0 },
            "pin_sets": [{ "label": "default", "pins": [] }],
            "metrics": "all"
        }"#;

        let study: Study = serde_json::from_str(json).unwrap();
        let err = study.validate().unwrap_err();
        assert!(err.message.contains("count must be positive"));
    }

    #[test]
    fn at_least_one_pin_set_required() {
        let json = r#"{
            "name": "my-study",
            "description": "Test",
            "seeds": { "from": 1, "count": 10 },
            "pin_sets": [],
            "metrics": "all"
        }"#;

        let study: Study = serde_json::from_str(json).unwrap();
        let err = study.validate().unwrap_err();
        assert!(err.message.contains("at least one pin set is required"));
    }

    #[test]
    fn duplicate_pin_set_labels_rejected() {
        let json = r#"{
            "name": "my-study",
            "description": "Test",
            "seeds": { "from": 1, "count": 10 },
            "pin_sets": [
                { "label": "set1", "pins": [] },
                { "label": "set1", "pins": [] }
            ],
            "metrics": "all"
        }"#;

        let study: Study = serde_json::from_str(json).unwrap();
        let err = study.validate().unwrap_err();
        assert!(err.message.contains("duplicate pin-set label"));
    }

    #[test]
    fn invalid_pin_rejected() {
        let json = r#"{
            "name": "my-study",
            "description": "Test",
            "seeds": { "from": 1, "count": 10 },
            "pin_sets": [
                { "label": "default", "pins": ["invalid-pin"] }
            ],
            "metrics": "all"
        }"#;

        let study: Study = serde_json::from_str(json).unwrap();
        let err = study.validate().unwrap_err();
        assert!(err.message.contains("invalid pin"));
        assert!(err.message.contains("default"));
    }

    #[test]
    fn metrics_all_must_be_literal_all() {
        let json = r#"{
            "name": "my-study",
            "description": "Test",
            "seeds": { "from": 1, "count": 10 },
            "pin_sets": [{ "label": "default", "pins": [] }],
            "metrics": "something-else"
        }"#;

        let study: Study = serde_json::from_str(json).unwrap();
        let err = study.validate().unwrap_err();
        assert!(err.message.contains("metrics must be"));
    }

    #[test]
    fn unknown_metric_rejected() {
        let json = r#"{
            "name": "my-study",
            "description": "Test",
            "seeds": { "from": 1, "count": 10 },
            "pin_sets": [{ "label": "default", "pins": [] }],
            "metrics": ["unknown-metric"]
        }"#;

        let study: Study = serde_json::from_str(json).unwrap();
        let err = study.validate().unwrap_err();
        assert!(err.message.contains("unknown metric"));
    }

    #[test]
    fn selected_metrics_all_returns_full_registry() {
        let json = r#"{
            "name": "my-study",
            "description": "Test",
            "seeds": { "from": 1, "count": 10 },
            "pin_sets": [{ "label": "default", "pins": [] }],
            "metrics": "all"
        }"#;

        let study: Study = serde_json::from_str(json).unwrap();
        let selected = study.selected_metrics().unwrap();
        let full_registry = registry();
        assert_eq!(selected.len(), full_registry.len());
    }

    #[test]
    fn selected_metrics_named_respects_registry_order() {
        let json = r#"{
            "name": "my-study",
            "description": "Test",
            "seeds": { "from": 1, "count": 10 },
            "pin_sets": [{ "label": "default", "pins": [] }],
            "metrics": ["moons-admitted", "star-class"]
        }"#;

        let study: Study = serde_json::from_str(json).unwrap();
        let selected = study.selected_metrics().unwrap();
        let full_registry = registry();

        // Find positions in registry
        let star_class_idx = full_registry
            .iter()
            .position(|m| m.name == "star-class")
            .unwrap();
        let moons_idx = full_registry
            .iter()
            .position(|m| m.name == "moons-admitted")
            .unwrap();

        // Verify returned order matches registry order
        assert_eq!(
            selected[0].name,
            if star_class_idx < moons_idx {
                "star-class"
            } else {
                "moons-admitted"
            }
        );
        assert_eq!(selected.len(), 2);
    }

    #[test]
    fn pin_sets_parsed_returns_working_skypins() {
        let json = r#"{
            "name": "my-study",
            "description": "Test",
            "seeds": { "from": 1, "count": 10 },
            "pin_sets": [
                { "label": "locked", "pins": ["rotation=locked"] }
            ],
            "metrics": "all"
        }"#;

        let study: Study = serde_json::from_str(json).unwrap();
        let parsed = study.pin_sets_parsed().unwrap();

        assert_eq!(parsed.len(), 1);
        assert_eq!(parsed[0].0, "locked");
        // Verify the pins were actually parsed and set
        assert!(parsed[0].1.rotation.is_some());
    }

    #[test]
    fn display_impl_formats_correctly() {
        let err = StudyError {
            message: "test error".to_string(),
        };
        let display_str = format!("{}", err);
        assert_eq!(display_str, "study: test error");
    }
}
