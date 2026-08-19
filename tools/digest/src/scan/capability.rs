//! Capability facts, derived from source at render time and never stored.

use super::repo_root;

/// The externally-allowlisted crates, read from the test that ENFORCES the
/// rule rather than from any prose restatement of it. `scripts/doctor.sh` used
/// to restate it and drifted: it still omitted `libm` long after decision 0041
/// admitted it.
pub fn allowed_external() -> Vec<String> {
    let src = std::fs::read_to_string(repo_root().join("cli/tests/suite/architecture.rs"))
        .expect("architecture.rs is readable");
    let line = src
        .lines()
        .find(|l| l.contains("ALLOWED_EXTERNAL"))
        .expect("ALLOWED_EXTERNAL is declared");
    // Split on `=` first: the declaration's own type annotation (`&[&str]`)
    // also contains a `[`, so bracket-matching from the start of the whole
    // line finds that bracket instead of the value literal's.
    let (_, initializer) = line
        .split_once('=')
        .expect("ALLOWED_EXTERNAL has an initializer");
    let inner = initializer
        .split_once('[')
        .and_then(|(_, r)| r.rsplit_once(']'))
        .map(|(i, _)| i)
        .expect("ALLOWED_EXTERNAL is a slice literal");
    let mut out: Vec<String> = inner
        .split(',')
        .map(|s| s.trim().trim_matches('"').to_string())
        .filter(|s| !s.is_empty())
        .collect();
    out.sort();
    out
}

/// The layer names, in dependency order.
pub fn layers() -> Vec<String> {
    ["kernel", "domains/*", "windows/*", "cli"]
        .iter()
        .map(|s| s.to_string())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_allowlist_is_derived_from_the_enforcing_test() {
        let got = allowed_external();
        assert_eq!(
            got,
            vec![
                "libm".to_string(),
                "serde".to_string(),
                "serde_json".to_string()
            ],
            "must match ALLOWED_EXTERNAL in cli/tests/suite/architecture.rs, including libm (0041)"
        );
    }

    #[test]
    fn the_generated_map_names_libm() {
        let out = crate::render::doctor::self_map(&allowed_external(), &layers());
        assert!(
            out.contains("libm"),
            "S1: the drift doctor.sh has today must be gone"
        );
    }

    #[test]
    fn no_allowlist_is_hard_coded_in_the_generator() {
        let src = include_str!("../render/doctor.rs");
        assert!(
            !src.contains("serde_json\""),
            "S1: the renderer must not name an allowlist member literally"
        );
    }
}
