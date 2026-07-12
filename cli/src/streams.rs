//! Render the stream manifest: every seed-derivation label in the project.

use hornvale_kernel::Domain;

/// Render every registered crate's stream labels as the book's generated
/// reference page. Labels are permanent save-format contracts.
/// type-audit: bare-ok(artifact: return)
pub fn render_streams() -> String {
    let mut doc = String::new();
    doc.push_str("<!-- GENERATED FILE — do not edit. Regenerate with `hornvale streams`. -->\n\n");
    doc.push_str(
        "Labels are permanent save-format contracts; regeneration uses epoch \
         suffixes (e.g. `settlement/name/v2`), never renames.\n\n",
    );
    // DOMAINS is stored in registration order; the manifest is alphabetical.
    let mut domains: Vec<&dyn Domain> = hornvale_worldgen::DOMAINS.to_vec();
    domains.sort_by_key(|d| d.crate_name());
    for domain in domains {
        doc.push_str(&format!("### {}\n\n", domain.crate_name()));
        let labels = domain.stream_labels();
        if labels.is_empty() {
            doc.push_str("*(no seed-derivation streams)*\n\n");
            continue;
        }
        doc.push_str("| Label | Meaning |\n|---|---|\n");
        for (label, meaning) in labels {
            doc.push_str(&format!("| `{label}` | {meaning} |\n"));
        }
        doc.push('\n');
    }
    doc.push_str("### hornvale-kernel (internal)\n\n");
    doc.push_str("| Label | Meaning |\n|---|---|\n");
    doc.push_str("| `octave-{n}` | per-octave noise streams derived inside fbm (n ≥ 1) |\n");
    doc
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn manifest_lists_every_crate_and_label() {
        let doc = render_streams();
        for expected in [
            "<!-- GENERATED FILE",
            "| `settlement/name` |",
            "| `settlement/placement` |",
            "| `terrain/plate-count` |",
            "| `language/<species>/name/settlement` |",
            "octave-{n}",
            "### hornvale-paleoclimate",
            "*(no seed-derivation streams)*",
        ] {
            assert!(doc.contains(expected), "missing: {expected}");
        }
    }

    #[test]
    fn manifest_is_deterministic() {
        assert_eq!(render_streams(), render_streams());
    }

    #[test]
    fn manifest_sections_are_alphabetical_by_crate() {
        let doc = render_streams();
        // The domain section headers, in document order, must be sorted.
        let headers: Vec<&str> = doc
            .lines()
            .filter(|l| l.starts_with("### hornvale-") && !l.contains("kernel"))
            .collect();
        let mut sorted = headers.clone();
        sorted.sort_unstable();
        assert_eq!(
            headers, sorted,
            "manifest domain sections must be alphabetical"
        );
        // paleoclimate sorts between language and religion.
        let pos = |s: &str| headers.iter().position(|h| *h == s).unwrap();
        assert!(pos("### hornvale-language") < pos("### hornvale-paleoclimate"));
        assert!(pos("### hornvale-paleoclimate") < pos("### hornvale-religion"));
    }
}
