//! Internal observation-episode manifests and their validation boundary.

use serde::de::{DeserializeSeed, Error as _, MapAccess, SeqAccess, Visitor};
use serde::{Deserialize, Deserializer, Serialize};
use std::collections::BTreeSet;
use std::fmt;
use std::path::{Path, PathBuf};

/// Editorial state of an observation package.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ObservationStatus {
    /// A candidate package exists but has not been checked.
    Draft,
    /// Terminology, evidence, and visual correspondence have been checked.
    Reviewed,
    /// Nathan approved the exact package for publication.
    Approved,
    /// Nathan published the package manually.
    Published,
}

/// Review state of an episode's evidence, independent of editorial approval.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum EvidenceStatus {
    /// The evidence record is still being assembled.
    Draft,
    /// The evidence and terminology have been checked.
    Reviewed,
}

impl<'de> Deserialize<'de> for EvidenceStatus {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        match value.as_str() {
            "draft" => Ok(Self::Draft),
            "reviewed" => Ok(Self::Reviewed),
            _ => Err(serde::de::Error::custom(format!(
                "evidence_status: unknown value '{value}'"
            ))),
        }
    }
}

/// How much implementation work an episode still requires.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CapabilityState {
    /// Existing output can support the observation.
    Existing,
    /// The simulation exists but needs an authoritative observation surface.
    NeedsObservationSurface,
    /// Authoritative data exists but needs a renderer.
    NeedsRenderer,
    /// The claimed phenomenon is not yet implemented by the simulation.
    NeedsSimulationExtension,
}

/// The dominant visual treatment of an episode.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum VisualGrammar {
    /// Maps, fields, gradients, distributions, and arrangements.
    Spatial,
    /// Timelines, intervals, population change, and cycles.
    Temporal,
    /// Typed relations such as contact, tribute, and ancestry.
    Relational,
    /// A close view of one place, being, utterance, practice, or residue.
    CloseReading,
}

impl<'de> Deserialize<'de> for VisualGrammar {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        match value.as_str() {
            "spatial" => Ok(Self::Spatial),
            "temporal" => Ok(Self::Temporal),
            "relational" => Ok(Self::Relational),
            "close_reading" => Ok(Self::CloseReading),
            _ => Err(serde::de::Error::custom(format!(
                "visual_grammar: unknown value '{value}'"
            ))),
        }
    }
}

/// A bounded world-time selection for one episode.
/// type-audit: bare-ok(diagnostic-value: start_day), bare-ok(diagnostic-value: end_day)
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct TimeWindow {
    /// First included standard day.
    pub start_day: f64,
    /// Last included standard day.
    pub end_day: f64,
}

/// The human record that permits an approved or published status.
/// type-audit: bare-ok(prose: reviewer), bare-ok(identifier-text: approved_at)
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Approval {
    /// Person who approved the exact episode package.
    pub reviewer: String,
    /// Recorded approval time, retained as authored text.
    pub approved_at: String,
}

/// One internal observation episode and its reproducibility record.
/// type-audit: bare-ok(identifier-text: id), bare-ok(prose: title), bare-ok(identifier-text: object), bare-ok(identifier-text: scale), bare-ok(identifier-text: primary_axis), bare-ok(identifier-text: phenomenon), bare-ok(prose: observation_sentence), bare-ok(identifier-text: world_revision), bare-ok(count: seed), bare-ok(count: frame_count), bare-ok(render-internal: frame_rate), bare-ok(identifier-text: source_commands), bare-ok(artifact: controlled_inputs), bare-ok(artifact: comparison_reference), bare-ok(artifact: lead_time), bare-ok(identifier-text: source_data), bare-ok(identifier-text: render_output), bare-ok(prose: caption_draft)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct EpisodeManifest {
    /// Stable episode identifier, such as `HV-001`.
    pub id: String,
    /// Public-facing episode title.
    pub title: String,
    /// Primary feature or object under observation.
    pub object: String,
    /// Explicit scale at which the object is observed.
    pub scale: String,
    /// Primary observable dimension.
    pub primary_axis: String,
    /// Realized phenomenon shown by the evidence.
    pub phenomenon: String,
    /// Dominant visual treatment.
    pub visual_grammar: VisualGrammar,
    /// Bounded statement of what the rendered evidence shows.
    pub observation_sentence: String,
    /// Revision that produced the observed world.
    pub world_revision: String,
    /// Deterministic world identity.
    pub seed: u64,
    /// Optional bounded world-time selection.
    pub time_window: Option<TimeWindow>,
    /// Number of frames in the intended episode.
    pub frame_count: u32,
    /// Frames per second in the intended episode.
    pub frame_rate: f64,
    /// Non-empty list of commands that reproduce the source evidence.
    pub source_commands: Vec<String>,
    /// State of the episode's evidence review, distinct from editorial state.
    pub evidence_status: EvidenceStatus,
    /// Current implementation readiness.
    pub capability_state: CapabilityState,
    /// Authored pins or held conditions used by the observation.
    pub controlled_inputs: serde_json::Map<String, serde_json::Value>,
    /// Internal-only research metadata, never a source for public copy.
    pub comparison_reference: Option<serde_json::Map<String, serde_json::Value>>,
    /// Optional internal scheduling metadata for capability lead time.
    pub lead_time: Option<serde_json::Map<String, serde_json::Value>>,
    /// Non-empty list naming the authoritative data surfaces used as evidence.
    pub source_data: Vec<String>,
    /// Optional repository-relative or local render output path.
    pub render_output: Option<String>,
    /// One to three advisory caption drafts.
    pub caption_draft: Vec<String>,
    /// Editorial lifecycle state of the exact video and copy package.
    pub editorial_status: ObservationStatus,
    /// Human approval record, present only for approved or published packages.
    pub approval: Option<Approval>,
}

/// A manifest could not be read, parsed, or semantically validated.
/// type-audit: bare-ok(prose: Read.reason), bare-ok(prose: Parse.reason), bare-ok(identifier-text: Invalid.field), bare-ok(prose: Invalid.reason)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ObservationError {
    /// The manifest file could not be read.
    Read {
        /// Path that could not be read.
        path: PathBuf,
        /// Operating-system error text.
        reason: String,
    },
    /// The manifest was not valid JSON or did not match the serde model.
    Parse {
        /// Path containing malformed input.
        path: PathBuf,
        /// Parser error text.
        reason: String,
    },
    /// One semantic field violated the manifest contract.
    Invalid {
        /// Field whose value was refused.
        field: &'static str,
        /// Human-readable reason for refusal.
        reason: String,
    },
}

impl fmt::Display for ObservationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Read { path, reason } => write!(f, "{}: {reason}", path.display()),
            Self::Parse { path, reason } => {
                write!(f, "{}: manifest parse: {reason}", path.display())
            }
            Self::Invalid { field, reason } => write!(f, "{field}: {reason}"),
        }
    }
}

impl std::error::Error for ObservationError {}

struct RejectDuplicateKeys;

impl<'de> DeserializeSeed<'de> for RejectDuplicateKeys {
    type Value = ();

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_any(DuplicateKeyVisitor)
    }
}

struct DuplicateKeyVisitor;

impl<'de> Visitor<'de> for DuplicateKeyVisitor {
    type Value = ();

    fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str("JSON without duplicate object keys")
    }

    fn visit_bool<E>(self, _value: bool) -> Result<Self::Value, E> {
        Ok(())
    }

    fn visit_i64<E>(self, _value: i64) -> Result<Self::Value, E> {
        Ok(())
    }

    fn visit_u64<E>(self, _value: u64) -> Result<Self::Value, E> {
        Ok(())
    }

    fn visit_f64<E>(self, _value: f64) -> Result<Self::Value, E> {
        Ok(())
    }

    fn visit_str<E>(self, _value: &str) -> Result<Self::Value, E> {
        Ok(())
    }

    fn visit_string<E>(self, _value: String) -> Result<Self::Value, E> {
        Ok(())
    }

    fn visit_none<E>(self) -> Result<Self::Value, E> {
        Ok(())
    }

    fn visit_unit<E>(self) -> Result<Self::Value, E> {
        Ok(())
    }

    fn visit_some<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        RejectDuplicateKeys.deserialize(deserializer)
    }

    fn visit_seq<A>(self, mut sequence: A) -> Result<Self::Value, A::Error>
    where
        A: SeqAccess<'de>,
    {
        while sequence.next_element_seed(RejectDuplicateKeys)?.is_some() {}
        Ok(())
    }

    fn visit_map<A>(self, mut object: A) -> Result<Self::Value, A::Error>
    where
        A: MapAccess<'de>,
    {
        let mut keys = BTreeSet::new();
        while let Some(key) = object.next_key::<String>()? {
            if !keys.insert(key.clone()) {
                return Err(A::Error::custom(format!("duplicate key `{key}`")));
            }
            object.next_value_seed(RejectDuplicateKeys)?;
        }
        Ok(())
    }
}

fn invalid(field: &'static str, reason: impl Into<String>) -> ObservationError {
    ObservationError::Invalid {
        field,
        reason: reason.into(),
    }
}

fn require_text(field: &'static str, value: &str) -> Result<(), ObservationError> {
    if value.trim().is_empty() {
        Err(invalid(field, "must not be empty"))
    } else {
        Ok(())
    }
}

fn at_path(error: ObservationError, path: &Path) -> ObservationError {
    match error {
        ObservationError::Invalid { field, reason } => ObservationError::Invalid {
            field,
            reason: format!("{}: {reason}", path.display()),
        },
        other => other,
    }
}

fn is_leap_year(year: u32) -> bool {
    year.is_multiple_of(4) && (!year.is_multiple_of(100) || year.is_multiple_of(400))
}

fn decimal(bytes: &[u8]) -> Option<u32> {
    bytes.iter().try_fold(0u32, |value, byte| {
        byte.is_ascii_digit()
            .then(|| value * 10 + u32::from(*byte - b'0'))
    })
}

fn is_utc_second_timestamp(value: &str) -> bool {
    let bytes = value.as_bytes();
    if bytes.len() != 20
        || bytes[4] != b'-'
        || bytes[7] != b'-'
        || bytes[10] != b'T'
        || bytes[13] != b':'
        || bytes[16] != b':'
        || bytes[19] != b'Z'
    {
        return false;
    }
    let Some(year) = decimal(&bytes[0..4]) else {
        return false;
    };
    let Some(month) = decimal(&bytes[5..7]) else {
        return false;
    };
    let Some(day) = decimal(&bytes[8..10]) else {
        return false;
    };
    let Some(hour) = decimal(&bytes[11..13]) else {
        return false;
    };
    let Some(minute) = decimal(&bytes[14..16]) else {
        return false;
    };
    let Some(second) = decimal(&bytes[17..19]) else {
        return false;
    };
    let days_in_month = match month {
        1 | 3 | 5 | 7 | 8 | 10 | 12 => 31,
        4 | 6 | 9 | 11 => 30,
        2 if is_leap_year(year) => 29,
        2 => 28,
        _ => return false,
    };
    (1..=days_in_month).contains(&day) && hour < 24 && minute < 60 && second < 60
}

/// Validate one parsed observation manifest without changing it.
pub fn validate_manifest(manifest: &EpisodeManifest) -> Result<(), ObservationError> {
    for (field, value) in [
        ("id", manifest.id.as_str()),
        ("title", manifest.title.as_str()),
        ("object", manifest.object.as_str()),
        ("scale", manifest.scale.as_str()),
        ("primary_axis", manifest.primary_axis.as_str()),
        ("phenomenon", manifest.phenomenon.as_str()),
        (
            "observation_sentence",
            manifest.observation_sentence.as_str(),
        ),
        ("world_revision", manifest.world_revision.as_str()),
    ] {
        require_text(field, value)?;
    }

    if manifest.frame_count == 0 {
        return Err(invalid("frame_count", "must be greater than zero"));
    }
    if !manifest.frame_rate.is_finite() || manifest.frame_rate <= 0.0 {
        return Err(invalid("frame_rate", "must be a positive finite number"));
    }
    if let Some(window) = manifest.time_window {
        if !window.start_day.is_finite() {
            return Err(invalid("time_window.start_day", "must be a finite number"));
        }
        if !window.end_day.is_finite() {
            return Err(invalid("time_window.end_day", "must be a finite number"));
        }
        if window.end_day < window.start_day {
            return Err(invalid("time_window", "end_day must not precede start_day"));
        }
    }

    if manifest.source_commands.is_empty() {
        return Err(invalid(
            "source_commands",
            "must contain at least one command",
        ));
    }
    for command in &manifest.source_commands {
        require_text("source_commands", command)?;
    }

    if manifest.source_data.is_empty() {
        return Err(invalid(
            "source_data",
            "must contain at least one authoritative data surface",
        ));
    }
    for source in &manifest.source_data {
        require_text("source_data", source)?;
    }
    if let Some(output) = &manifest.render_output {
        require_text("render_output", output)?;
    }

    if !(1..=3).contains(&manifest.caption_draft.len()) {
        return Err(invalid(
            "caption_draft",
            "must contain between one and three drafts",
        ));
    }
    for caption in &manifest.caption_draft {
        require_text("caption_draft", caption)?;
    }

    let editorially_approved = matches!(
        manifest.editorial_status,
        ObservationStatus::Approved | ObservationStatus::Published
    );
    match (editorially_approved, manifest.approval.as_ref()) {
        (true, None) => {
            return Err(invalid(
                "approval",
                "is required when editorial_status is approved or published",
            ));
        }
        (false, Some(_)) => {
            return Err(invalid(
                "approval",
                "must be absent when editorial_status is draft or reviewed",
            ));
        }
        (_, None) => {}
        (true, Some(approval)) => {
            if approval.reviewer != "Nathan" {
                return Err(invalid("approval.reviewer", "must be exactly 'Nathan'"));
            }
            if !is_utc_second_timestamp(&approval.approved_at) {
                return Err(invalid(
                    "approval.approved_at",
                    "must have the UTC second-precision shape YYYY-MM-DDTHH:MM:SSZ",
                ));
            }
        }
    }

    const DISTINCT_SOCIAL_UNITS: [&str; 4] = ["population", "settlement", "occupation", "residue"];
    if DISTINCT_SOCIAL_UNITS.contains(&manifest.object.as_str())
        && DISTINCT_SOCIAL_UNITS.contains(&manifest.scale.as_str())
        && manifest.object != manifest.scale
    {
        return Err(invalid(
            "object/scale",
            format!(
                "distinct observed units cannot be substituted: object is '{}' but scale is '{}'",
                manifest.object, manifest.scale
            ),
        ));
    }

    Ok(())
}

/// Read, parse, and validate one observation manifest.
pub fn read_manifest(path: &Path) -> Result<EpisodeManifest, ObservationError> {
    let json = std::fs::read_to_string(path).map_err(|error| ObservationError::Read {
        path: path.to_path_buf(),
        reason: error.to_string(),
    })?;
    let mut duplicate_check = serde_json::Deserializer::from_str(&json);
    RejectDuplicateKeys
        .deserialize(&mut duplicate_check)
        .map_err(|error| ObservationError::Parse {
            path: path.to_path_buf(),
            reason: error.to_string(),
        })?;
    let manifest = serde_json::from_str(&json).map_err(|error| ObservationError::Parse {
        path: path.to_path_buf(),
        reason: error.to_string(),
    })?;
    validate_manifest(&manifest).map_err(|error| at_path(error, path))?;
    Ok(manifest)
}
