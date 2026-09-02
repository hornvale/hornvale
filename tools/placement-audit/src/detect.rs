//! Detecting pub enum/struct "shape twins" — identical member-name sets
//! duplicated across two or more crates.

use crate::extract::{ShapeKind, TypeShape};
use crate::walk::CrateTypes;

/// A group of ≥2 [`TypeShape`]s, from ≥2 distinct crates, sharing the same
/// kind and the same sorted member-name set.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TwinGroup {
    /// The matching shapes, sorted by `(crate_name, name)`.
    pub members: Vec<TypeShape>,
}

fn sort_key(t: &TypeShape) -> (&str, &str) {
    (t.crate_name.as_str(), t.name.as_str())
}

/// Find every shape twin across `crates`: pub enums/structs whose kind and
/// sorted member-name set match, appearing in ≥2 distinct crates.
///
/// Grouping is by linear accumulation (no `HashMap`, matching house style),
/// and both the groups and each group's members are sorted deterministically
/// by `(crate_name, name)` so output is byte-stable across runs.
pub fn twins(crates: &[CrateTypes]) -> Vec<TwinGroup> {
    let mut groups: Vec<(ShapeKind, Vec<String>, Vec<TypeShape>)> = Vec::new();
    for c in crates {
        for t in &c.types {
            match groups
                .iter_mut()
                .find(|(kind, members, _)| *kind == t.kind && *members == t.members)
            {
                Some((_, _, members)) => members.push(t.clone()),
                None => groups.push((t.kind, t.members.clone(), vec![t.clone()])),
            }
        }
    }

    let mut result: Vec<TwinGroup> = groups
        .into_iter()
        .filter_map(|(_, _, mut members)| {
            let mut crate_names: Vec<&str> =
                members.iter().map(|m| m.crate_name.as_str()).collect();
            crate_names.sort();
            crate_names.dedup();
            if crate_names.len() < 2 {
                return None;
            }
            members.sort_by(|a, b| sort_key(a).cmp(&sort_key(b)));
            Some(TwinGroup { members })
        })
        .collect();

    result.sort_by(|a, b| sort_key(&a.members[0]).cmp(&sort_key(&b.members[0])));
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    fn shape(crate_name: &str, name: &str, kind: ShapeKind, members: &[&str]) -> TypeShape {
        TypeShape {
            crate_name: crate_name.to_string(),
            name: name.to_string(),
            kind,
            members: members.iter().map(|m| m.to_string()).collect(),
            doc: String::new(),
            file: PathBuf::from(format!("{crate_name}/src/lib.rs")),
            line: 1,
        }
    }

    #[test]
    fn finds_a_cross_crate_twin_and_ignores_a_near_miss() {
        let crates = vec![
            CrateTypes {
                crate_name: "a".to_string(),
                types: vec![shape(
                    "a",
                    "Mood",
                    ShapeKind::Enum,
                    &["Bright", "Dim", "Level"],
                )],
            },
            CrateTypes {
                crate_name: "b".to_string(),
                types: vec![shape(
                    "b",
                    "Temper",
                    ShapeKind::Enum,
                    &["Bright", "Dim", "Level"],
                )],
            },
            CrateTypes {
                crate_name: "c".to_string(),
                types: vec![shape(
                    "c",
                    "Slope",
                    ShapeKind::Enum,
                    &["Askew", "Bright", "Level"],
                )],
            },
        ];
        let found = twins(&crates);
        assert_eq!(found.len(), 1);
        let names: Vec<&str> = found[0].members.iter().map(|t| t.name.as_str()).collect();
        assert_eq!(names, vec!["Mood", "Temper"]);
    }

    #[test]
    fn same_crate_duplicates_are_not_twins() {
        let crates = vec![CrateTypes {
            crate_name: "a".to_string(),
            types: vec![
                shape("a", "Mood", ShapeKind::Enum, &["Bright", "Dim", "Level"]),
                shape("a", "Temper", ShapeKind::Enum, &["Bright", "Dim", "Level"]),
            ],
        }];
        assert!(twins(&crates).is_empty());
    }

    #[test]
    fn different_kinds_with_the_same_member_names_are_not_twins() {
        let crates = vec![
            CrateTypes {
                crate_name: "a".to_string(),
                types: vec![shape("a", "Mood", ShapeKind::Enum, &["x", "y"])],
            },
            CrateTypes {
                crate_name: "b".to_string(),
                types: vec![shape("b", "Point", ShapeKind::Struct, &["x", "y"])],
            },
        ];
        assert!(twins(&crates).is_empty());
    }

    #[test]
    fn group_and_member_order_is_deterministic() {
        let crates = vec![
            CrateTypes {
                crate_name: "z".to_string(),
                types: vec![shape("z", "Late", ShapeKind::Enum, &["x", "y"])],
            },
            CrateTypes {
                crate_name: "a".to_string(),
                types: vec![
                    shape("a", "Early", ShapeKind::Enum, &["x", "y"]),
                    shape("a", "Solo", ShapeKind::Struct, &["p", "q"]),
                ],
            },
            CrateTypes {
                crate_name: "m".to_string(),
                types: vec![shape("m", "Solo", ShapeKind::Struct, &["p", "q"])],
            },
        ];
        let found = twins(&crates);
        assert_eq!(found.len(), 2);
        // The (x, y) enum group sorts first (crate "a" < crate "m").
        let first_names: Vec<&str> = found[0].members.iter().map(|t| t.name.as_str()).collect();
        assert_eq!(first_names, vec!["Early", "Late"]);
        let second_names: Vec<&str> = found[1].members.iter().map(|t| t.name.as_str()).collect();
        assert_eq!(second_names, vec!["Solo", "Solo"]);
    }
}
