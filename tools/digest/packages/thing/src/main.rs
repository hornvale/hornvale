use std::{
    ffi::OsString,
    io::{self, Write},
    path::PathBuf,
};

fn parse_args<I>(args: I) -> Result<PathBuf, String>
where
    I: IntoIterator<Item = OsString>,
{
    let args = args.into_iter().collect::<Vec<_>>();
    let [command, flag, root] = args.as_slice() else {
        return Err("usage: digest-thing collect --repo-root <absolute-path>".into());
    };
    if command != "collect" || flag != "--repo-root" {
        return Err("usage: digest-thing collect --repo-root <absolute-path>".into());
    }
    let root = PathBuf::from(root);
    if !root.is_absolute() {
        return Err(format!(
            "repository root must be an absolute path: {}",
            root.display()
        ));
    }
    if !root.is_dir() {
        return Err(format!(
            "repository root is not a directory: {}",
            root.display()
        ));
    }
    let scope = root.join("domains/thing");
    if !scope.is_dir() {
        return Err(format!(
            "declared scope domains/thing is absent beneath repository root {}",
            root.display()
        ));
    }
    Ok(root)
}

fn run() -> Result<(), String> {
    let _repo_root = parse_args(std::env::args_os().skip(1))?;
    let contribution = digest_thing::contribution()?;
    digest_protocol::validate(&contribution).map_err(|error| error.to_string())?;

    let stdout = io::stdout();
    let mut stdout = stdout.lock();
    serde_json::to_writer(&mut stdout, &contribution)
        .map_err(|error| format!("serialize contribution: {error}"))?;
    writeln!(stdout).map_err(|error| format!("write contribution: {error}"))?;
    Ok(())
}

fn main() {
    if let Err(error) = run() {
        eprintln!("digest-thing: {error}");
        std::process::exit(1);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{ffi::OsString, path::PathBuf};

    fn repository_root() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .ancestors()
            .nth(4)
            .expect("package is nested beneath the repository root")
            .to_path_buf()
    }

    /// Break caught: accepting an invocation other than the host's frozen
    /// `collect --repo-root <absolute>` shape would make typos ambiguous.
    #[test]
    fn parser_accepts_only_collect_with_an_absolute_repo_root() {
        let root = repository_root();
        let valid = vec![
            OsString::from("collect"),
            OsString::from("--repo-root"),
            root.as_os_str().to_owned(),
        ];
        assert_eq!(parse_args(valid).unwrap(), root);

        assert!(parse_args([OsString::from("collect")]).is_err());
        assert!(
            parse_args([
                OsString::from("collect"),
                OsString::from("--repo-root"),
                OsString::from("relative/path"),
            ])
            .is_err()
        );
    }

    /// Break caught: collecting from an absolute directory that does not
    /// contain the declared Thing scope must fail before emitting an envelope.
    #[test]
    fn parser_rejects_a_root_without_the_declared_scope() {
        let missing = repository_root().join("tools/digest/packages/thing");
        let result = parse_args([
            OsString::from("collect"),
            OsString::from("--repo-root"),
            missing.as_os_str().to_owned(),
        ]);

        assert!(result.is_err());
    }
}
