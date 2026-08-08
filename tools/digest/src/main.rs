//! The digest CLI entry point.
//!
//! Only `render doctor` is implemented so far — the minimum Task 6 needed
//! to unblock `scripts/doctor.sh`'s layering/allowlist section. Task 7 builds
//! out the rest of the command surface (the `decisions` and `delta` render
//! arms, usage text, and exit-code handling); this match arm is the pattern
//! to extend.

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    if let ["render", "doctor"] = args
        .iter()
        .map(String::as_str)
        .collect::<Vec<_>>()
        .as_slice()
    {
        let allowed = digest::scan::capability::allowed_external();
        let layers = digest::scan::capability::layers();
        print!("{}", digest::render::doctor::self_map(&allowed, &layers));
    }
}
