//! Render the stream manifest: every seed-derivation label in the project.

/// Render every registered crate's stream labels as the book's generated
/// reference page. Labels are permanent save-format contracts.
pub fn render_streams() -> String {
    let sources: [(&str, Vec<(&'static str, &'static str)>); 8] = [
        ("hornvale-astronomy", hornvale_astronomy::stream_labels()),
        ("hornvale-climate", hornvale_climate::stream_labels()),
        ("hornvale-culture", hornvale_culture::stream_labels()),
        ("hornvale-language", hornvale_language::stream_labels()),
        ("hornvale-religion", hornvale_religion::stream_labels()),
        ("hornvale-settlement", hornvale_settlement::stream_labels()),
        ("hornvale-species", hornvale_species::stream_labels()),
        ("hornvale-terrain", hornvale_terrain::stream_labels()),
    ];
    let mut doc = String::new();
    doc.push_str("<!-- GENERATED FILE — do not edit. Regenerate with `hornvale streams`. -->\n\n");
    doc.push_str(
        "Labels are permanent save-format contracts; regeneration uses epoch \
         suffixes (e.g. `settlement/name/v2`), never renames.\n\n",
    );
    for (crate_name, labels) in sources {
        doc.push_str(&format!("### {crate_name}\n\n"));
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
        ] {
            assert!(doc.contains(expected), "missing: {expected}");
        }
    }

    #[test]
    fn manifest_is_deterministic() {
        assert_eq!(render_streams(), render_streams());
    }
}
