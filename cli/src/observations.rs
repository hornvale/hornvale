//! Internal observation-episode manifests and their validation boundary.

use serde::{Deserialize, Serialize};
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
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

/// A bounded world-time selection for one episode.
/// type-audit: bare-ok(diagnostic-value: start_day), bare-ok(diagnostic-value: end_day)
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct TimeWindow {
    /// First included standard day.
    pub start_day: f64,
    /// Last included standard day.
    pub end_day: f64,
}

/// The human record that permits an approved or published status.
/// type-audit: bare-ok(prose: reviewer), bare-ok(identifier-text: approved_at)
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Approval {
    /// Person who approved the exact episode package.
    pub reviewer: String,
    /// Recorded approval time, retained as authored text.
    pub approved_at: String,
}

/// One internal observation episode and its reproducibility record.
/// type-audit: bare-ok(identifier-text: id), bare-ok(prose: title), bare-ok(identifier-text: object), bare-ok(identifier-text: scale), bare-ok(identifier-text: primary_axis), bare-ok(identifier-text: phenomenon), bare-ok(prose: observation_sentence), bare-ok(identifier-text: world_revision), bare-ok(count: seed), bare-ok(count: frame_count), bare-ok(render-internal: frame_rate), bare-ok(identifier-text: source_commands), bare-ok(artifact: comparison_reference), bare-ok(prose: caption_draft)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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
    /// Editorial lifecycle state.
    pub evidence_status: ObservationStatus,
    /// Human approval record, required once the package is approved.
    pub approval: Option<Approval>,
    /// Current implementation readiness.
    pub capability_state: CapabilityState,
    /// Internal-only research metadata, never a source for public copy.
    pub comparison_reference: Option<serde_json::Map<String, serde_json::Value>>,
    /// One to three advisory caption drafts.
    pub caption_draft: Vec<String>,
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

fn require_enum_value(
    document: &serde_json::Value,
    field: &'static str,
    allowed: &[&str],
) -> Result<(), ObservationError> {
    let Some(value) = document.get(field) else {
        return Ok(());
    };
    let Some(value) = value.as_str() else {
        return Err(invalid(field, "must be a string"));
    };
    if allowed.contains(&value) {
        Ok(())
    } else {
        Err(invalid(
            field,
            format!("unknown value '{value}' (expected {})", allowed.join("|")),
        ))
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

    if !(1..=3).contains(&manifest.caption_draft.len()) {
        return Err(invalid(
            "caption_draft",
            "must contain between one and three drafts",
        ));
    }
    for caption in &manifest.caption_draft {
        require_text("caption_draft", caption)?;
    }

    if matches!(
        manifest.evidence_status,
        ObservationStatus::Approved | ObservationStatus::Published
    ) {
        let approval = manifest.approval.as_ref().ok_or_else(|| {
            invalid(
                "approval",
                "is required when evidence_status is approved or published",
            )
        })?;
        require_text("approval.reviewer", &approval.reviewer)?;
        require_text("approval.approved_at", &approval.approved_at)?;
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
    let document: serde_json::Value =
        serde_json::from_str(&json).map_err(|error| ObservationError::Parse {
            path: path.to_path_buf(),
            reason: error.to_string(),
        })?;
    require_enum_value(
        &document,
        "visual_grammar",
        &["spatial", "temporal", "relational", "close_reading"],
    )
    .map_err(|error| at_path(error, path))?;
    require_enum_value(
        &document,
        "evidence_status",
        &["draft", "reviewed", "approved", "published"],
    )
    .map_err(|error| at_path(error, path))?;
    require_enum_value(
        &document,
        "capability_state",
        &[
            "existing",
            "needs_observation_surface",
            "needs_renderer",
            "needs_simulation_extension",
        ],
    )
    .map_err(|error| at_path(error, path))?;
    let manifest = serde_json::from_value(document).map_err(|error| ObservationError::Parse {
        path: path.to_path_buf(),
        reason: error.to_string(),
    })?;
    validate_manifest(&manifest).map_err(|error| at_path(error, path))?;
    Ok(manifest)
}
