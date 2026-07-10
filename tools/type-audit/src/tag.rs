//! Parsing the `type-audit:` verdict line inside an item's doc comment.

/// The ratified `bare-ok` classes (spec §4 / decision 0025).
pub const BARE_OK_CLASSES: &[&str] = &[
    "ratio",
    "count",
    "index",
    "constructor-edge",
    "envelope",
    "identifier-text",
    "prose",
    "artifact",
    "diagnostic-value",
    "render-internal",
    "flag",
];

/// One parsed verdict from a tag line.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Verdict {
    /// Permanently fine as a bare primitive, in the named rubric class.
    BareOk {
        /// Rubric class from [`BARE_OK_CLASSES`].
        class: String,
        /// Optional position qualifier (parameter/field name).
        position: Option<String>,
    },
    /// Should be a newtype; deliberately deferred with a stated reason.
    Waiver {
        /// Traceable reason (e.g. `decision-0014`).
        reason: String,
        /// Optional position qualifier.
        position: Option<String>,
    },
    /// Will be converted in remediation wave `wave`.
    Pending {
        /// Remediation wave number.
        wave: u32,
        /// Optional position qualifier.
        position: Option<String>,
    },
}

/// A malformed or ambiguous tag.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TagError {
    /// Human-readable reason.
    pub message: String,
}

/// Return the single `type-audit:` payload (text after the colon), or `None`.
/// Errors if more than one tag line is present.
pub fn find_tag_line(doc: &str) -> Result<Option<&str>, TagError> {
    let mut found: Option<&str> = None;
    for line in doc.lines() {
        let trimmed = line.trim();
        if let Some(rest) = trimmed.strip_prefix("type-audit:") {
            if found.is_some() {
                return Err(TagError {
                    message: "more than one type-audit line".into(),
                });
            }
            found = Some(rest.trim());
        }
    }
    Ok(found)
}

/// Parse every verdict on the item's tag line (empty when none present).
pub fn parse_tag(doc: &str) -> Result<Vec<Verdict>, TagError> {
    let Some(payload) = find_tag_line(doc)? else {
        return Ok(Vec::new());
    };
    payload
        .split(',')
        .map(|v| parse_verdict(v.trim()))
        .collect()
}

fn parse_verdict(s: &str) -> Result<Verdict, TagError> {
    let err = || TagError {
        message: format!("malformed verdict: {s:?}"),
    };
    let open = s.find('(').ok_or_else(err)?;
    if !s.ends_with(')') {
        return Err(err());
    }
    let head = &s[..open];
    let inner = &s[open + 1..s.len() - 1];
    let (arg, position) = match inner.split_once(':') {
        Some((a, p)) => (a.trim(), Some(p.trim().to_string())),
        None => (inner.trim(), None),
    };
    match head {
        "bare-ok" => {
            if !BARE_OK_CLASSES.contains(&arg) {
                return Err(TagError {
                    message: format!("unknown bare-ok class: {arg:?}"),
                });
            }
            Ok(Verdict::BareOk {
                class: arg.to_string(),
                position,
            })
        }
        "waiver" => {
            if arg.is_empty() {
                return Err(TagError {
                    message: "waiver needs a reason".into(),
                });
            }
            Ok(Verdict::Waiver {
                reason: arg.to_string(),
                position,
            })
        }
        "pending" => {
            let wave = arg
                .strip_prefix("wave-")
                .and_then(|n| n.parse::<u32>().ok())
                .ok_or_else(|| TagError {
                    message: format!("bad wave: {arg:?}"),
                })?;
            Ok(Verdict::Pending { wave, position })
        }
        other => Err(TagError {
            message: format!("unknown verdict: {other:?}"),
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_every_verdict_form() {
        assert_eq!(
            parse_tag("type-audit: bare-ok(count)").unwrap(),
            vec![Verdict::BareOk {
                class: "count".into(),
                position: None
            }]
        );
        assert_eq!(
            parse_tag("prose\ntype-audit: bare-ok(ratio: ocean_fraction), pending(wave-2: seed)")
                .unwrap(),
            vec![
                Verdict::BareOk {
                    class: "ratio".into(),
                    position: Some("ocean_fraction".into())
                },
                Verdict::Pending {
                    wave: 2,
                    position: Some("seed".into())
                },
            ]
        );
        assert_eq!(
            parse_tag("type-audit: waiver(decision-0014)").unwrap(),
            vec![Verdict::Waiver {
                reason: "decision-0014".into(),
                position: None
            }]
        );
        assert!(parse_tag("no tag here").unwrap().is_empty());
    }

    #[test]
    fn rejects_malformed_and_unknown() {
        assert!(parse_tag("type-audit: bare-ok(nonsense)").is_err()); // unknown class
        assert!(parse_tag("type-audit: frobnicate(x)").is_err()); // unknown verdict
        assert!(parse_tag("type-audit: pending(wave-x)").is_err()); // non-numeric wave
        assert!(parse_tag("type-audit: bare-ok(count)\ntype-audit: waiver(y)").is_err()); // two lines
    }

    #[test]
    fn parses_ratified_campaign27_classes() {
        for class in ["prose", "artifact", "diagnostic-value"] {
            let got = parse_tag(&format!("type-audit: bare-ok({class})")).unwrap();
            assert_eq!(
                got,
                vec![Verdict::BareOk {
                    class: class.to_string(),
                    position: None
                }]
            );
        }
    }
}
