use std::{ffi::OsString, io::Write, path::PathBuf};

fn parse_args(args: &[OsString]) -> Result<PathBuf, String> {
    if args.len() != 4 || args[1] != "collect" || args[2] != "--repo-root" {
        return Err("usage: digest-census-publication collect --repo-root <absolute-path>".into());
    }
    let root = PathBuf::from(&args[3]);
    if !root.is_absolute() {
        return Err("--repo-root must be an absolute path".into());
    }
    Ok(root)
}

fn main() {
    let args: Vec<_> = std::env::args_os().collect();
    let result = parse_args(&args)
        .and_then(|root| digest_census_publication::contribution(&root))
        .and_then(|contribution| {
            serde_json::to_vec(&contribution)
                .map_err(|error| format!("serialize census contribution: {error}"))
        });
    match result {
        Ok(bytes) => {
            let mut stdout = std::io::stdout().lock();
            if let Err(error) = stdout
                .write_all(&bytes)
                .and_then(|()| stdout.write_all(b"\n"))
            {
                eprintln!("digest-census-publication: stdout: {error}");
                std::process::exit(1);
            }
        }
        Err(error) => {
            eprintln!("digest-census-publication: {error}");
            std::process::exit(1);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Checks the one accepted invocation yields its absolute root; it does
    /// not establish that the root is a Hornvale checkout.
    #[test]
    fn parses_the_contributor_collect_contract() {
        let root = std::env::current_dir().unwrap();
        let args = [
            OsString::from("digest-census-publication"),
            OsString::from("collect"),
            OsString::from("--repo-root"),
            root.clone().into_os_string(),
        ];
        assert_eq!(parse_args(&args).unwrap(), root);
    }

    /// Checks malformed, relative, and extended invocations fail closed; it
    /// does not validate repository contents.
    #[test]
    fn rejects_arguments_outside_the_contributor_contract() {
        for args in [
            vec![OsString::from("digest-census-publication")],
            vec![
                OsString::from("digest-census-publication"),
                OsString::from("collect"),
                OsString::from("--repo-root"),
                OsString::from("relative"),
            ],
            vec![
                OsString::from("digest-census-publication"),
                OsString::from("collect"),
                OsString::from("--repo-root"),
                std::env::current_dir().unwrap().into_os_string(),
                OsString::from("extra"),
            ],
        ] {
            assert!(parse_args(&args).is_err());
        }
    }
}
