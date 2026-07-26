//! The Rust-side half of the census-canonical-host guard (decision 0063).
//!
//! `scripts/census-canonical-host.sh` guards the shell entry points
//! (`scripts/census-run.sh`, the `HV_CENSUS=1` branch of
//! `scripts/regenerate-artifacts.sh`). It cannot guard `cargo run -p
//! hornvale -- lab run <study>`: that path calls straight into
//! [`crate::publish::publish`] with no shell wrapper in between, so a census
//! study run that way on any machine silently commits goldens that disagree
//! with the canonical box on ~0.1% of discrete-count metrics (values decided
//! upstream of quantize-at-emit) and then drift-check green forever. This
//! module is that missing guard, invoked from [`crate::publish::publish`]
//! itself so every caller — present or future — inherits it.
//!
//! Single source of truth for the canonical hostname: this module and the
//! shell guard both read `scripts/census-canonical-host.txt`. This side
//! bakes it in at compile time ([`include_str!`]); the shell side reads it at
//! run time. One file, two readers — never two independently hardcoded
//! copies of the hostname to drift apart.

use std::path::Path;

/// The directory holding every committed lab/census golden. Decision 0063's
/// canonical-host guard applies to writes here. Exposed so callers that want
/// to fail fast (checking before running an expensive study, not only
/// before publishing its output) can name the same directory `publish` does.
/// type-audit: bare-ok(identifier-text)
pub const CENSUS_GOLDENS_DIR: &str = "book/src/laboratory/generated";

/// The short hostname (`hostname -s`) of the one machine allowed to author
/// census goldens. Shared with `scripts/census-canonical-host.sh` via
/// `scripts/census-canonical-host.txt` — see the module doc.
/// type-audit: bare-ok(identifier-text)
pub const CANONICAL_CENSUS_HOST: &str =
    include_str!("../../../scripts/census-canonical-host.txt").trim_ascii();

/// True if `name` names a census-scale study. By convention every study
/// under `studies/` that carries decision 0063's cross-machine divergence
/// risk is either named `the-census` or prefixed `census-of-` (see
/// `studies/*.study.json`); smaller, non-census studies like `the-chorus` or
/// `branches-family` are unaffected and may run and publish anywhere.
fn is_census_study(name: &str) -> bool {
    name == "the-census" || name.starts_with("census-of-")
}

/// Refuse to publish a census study's goldens from any host but the
/// canonical one. A no-op for a non-census study, or for any output
/// directory outside the committed goldens tree — an ordinary `lab run` of a
/// scratch study must keep working everywhere.
///
/// `hostname` is threaded in (rather than read from the OS inside this
/// function) so the refusal logic stays a pure, host-independent function to
/// unit test — see [`current_hostname`] for the OS-reading half.
/// type-audit: bare-ok(identifier-text: study_name), bare-ok(identifier-text: hostname), bare-ok(prose: return)
pub fn require_canonical_host_for(
    study_name: &str,
    base_dir: &Path,
    hostname: &str,
) -> Result<(), String> {
    if !is_census_study(study_name) || !base_dir.ends_with(CENSUS_GOLDENS_DIR) {
        return Ok(());
    }
    if hostname.eq_ignore_ascii_case(CANONICAL_CENSUS_HOST) {
        return Ok(());
    }
    Err(refusal_message(study_name, hostname))
}

/// The explanatory refusal text, mirroring `scripts/census-canonical-host.sh`'s
/// message (including the cross-machine invocation) so a developer sees the
/// same guidance regardless of which entry point stopped them.
fn refusal_message(study_name: &str, hostname: &str) -> String {
    format!(
        "census: REFUSING to publish '{study_name}' from '{hostname}'.\n\
\n\
Census goldens may only be authored on '{CANONICAL_CENSUS_HOST}' (decision 0063).\n\
The boxes are not byte-identical: ~0.1% of discrete-count census metrics differ\n\
by one unit, decided upstream of quantize-at-emit, so a run here would commit\n\
values that silently disagree with the canonical ones and then drift-check\n\
green forever.\n\
\n\
Trigger the run on the canonical box instead — push your branch first, then:\n\
\n\
  ssh {CANONICAL_CENSUS_HOST} 'cd ~/Projects/hornvale && \\\n\
    HV_CENSUS_WORKTREE=canonical \\\n\
    HV_CENSUS_REF=<full-sha> \\\n\
    scripts/census-run.sh {study_name}'\n\
\n\
Pass a SHA rather than a branch name: HV_CENSUS_REF feeds 'reset --hard', which\n\
can otherwise land on a stale LOCAL branch of that name on the canonical box.\n\
Verify HEAD there matches your SHA before trusting the output.\n\
\n\
If '{CANONICAL_CENSUS_HOST}' is no longer the canonical box, change the hostname\n\
in scripts/census-canonical-host.txt — deliberately, in a reviewable commit.\n"
    )
}

/// The current machine's short hostname (`hostname -s`), matching what
/// `scripts/census-canonical-host.sh` compares. Empty — and so never
/// matching the canonical host, failing CLOSED — if the `hostname` binary is
/// unavailable or its output isn't valid UTF-8.
/// type-audit: bare-ok(identifier-text)
pub fn current_hostname() -> String {
    std::process::Command::new("hostname")
        .arg("-s")
        .output()
        .ok()
        .filter(|output| output.status.success())
        .and_then(|output| String::from_utf8(output.stdout).ok())
        .map(|s| s.trim().to_string())
        .unwrap_or_default()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn permits_non_census_study_from_any_host() {
        let result =
            require_canonical_host_for("the-chorus", Path::new(CENSUS_GOLDENS_DIR), "some-laptop");
        assert!(result.is_ok(), "non-census studies must run anywhere");
    }

    #[test]
    fn permits_census_study_outside_the_goldens_dir() {
        let result =
            require_canonical_host_for("the-census", Path::new("/tmp/scratch"), "some-laptop");
        assert!(
            result.is_ok(),
            "a census study publishing outside the goldens dir is a scratch run"
        );
    }

    #[test]
    fn permits_census_study_from_the_canonical_host() {
        let result = require_canonical_host_for(
            "the-census",
            Path::new(CENSUS_GOLDENS_DIR),
            CANONICAL_CENSUS_HOST,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn permits_census_study_from_canonical_host_case_insensitively() {
        let shouting = CANONICAL_CENSUS_HOST.to_uppercase();
        let result = require_canonical_host_for(
            "census-of-coasts",
            Path::new(CENSUS_GOLDENS_DIR),
            &shouting,
        );
        assert!(result.is_ok());
    }

    /// The unit test C1 asks for: the refusal fires for a non-matching
    /// hostname, on any machine this suite happens to run on — it never
    /// depends on being (or not being) the canonical box itself.
    #[test]
    fn refuses_census_study_from_a_non_canonical_host() {
        let result =
            require_canonical_host_for("the-census", Path::new(CENSUS_GOLDENS_DIR), "some-laptop");
        let err = result.expect_err("a non-canonical host must be refused");
        assert!(err.contains("some-laptop"), "names the offending host");
        assert!(err.contains(CANONICAL_CENSUS_HOST), "names the right box");
        assert!(err.contains("census-run.sh"), "points at the fix");
    }

    #[test]
    fn refuses_census_of_prefixed_study_from_a_non_canonical_host() {
        let result = require_canonical_host_for(
            "census-of-eyes",
            Path::new(CENSUS_GOLDENS_DIR),
            "some-laptop",
        );
        assert!(result.is_err());
    }

    #[test]
    fn canonical_host_constant_matches_the_shared_file() {
        assert_eq!(CANONICAL_CENSUS_HOST, "lefford");
    }
}
