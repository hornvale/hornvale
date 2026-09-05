//! Enrollment is Cargo workspace data, never a contributor-specific dispatch list.
use digest_protocol::{PROTOCOL_VERSION, normalize_scope, scope_matches};
use serde::Deserialize;
use std::{
    collections::BTreeSet,
    path::{Path, PathBuf},
};

#[derive(Deserialize)]
struct Metadata {
    workspace_root: PathBuf,
    workspace_members: Vec<String>,
    packages: Vec<Package>,
}
#[derive(Deserialize)]
struct Package {
    id: String,
    manifest_path: PathBuf,
    metadata: Option<PackageMetadata>,
    targets: Vec<Target>,
}
#[derive(Deserialize)]
struct PackageMetadata {
    digest: Option<Declaration>,
}
#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct Declaration {
    pub role: String,
    pub namespace: String,
    pub protocol: u32,
    pub binary: String,
    pub scopes: Vec<String>,
}
#[derive(Deserialize)]
struct Target {
    name: String,
    kind: Vec<String>,
}
#[derive(Debug)]
pub(super) struct Contributor {
    pub package_id: String,
    pub declaration: Declaration,
}

pub(super) fn discover(bytes: &[u8], root: &Path, scope: &str) -> Result<Vec<Contributor>, String> {
    let metadata: Metadata =
        serde_json::from_slice(bytes).map_err(|e| format!("metadata JSON: {e}"))?;
    let workspace = root.join("tools/digest");
    if canonical(&metadata.workspace_root)? != workspace || canonical(&workspace)? != workspace {
        return Err(
            "metadata workspace root does not match the current checkout (possible symlink escape)"
                .into(),
        );
    }
    let members: BTreeSet<_> = metadata.workspace_members.iter().collect();
    if members.len() != metadata.workspace_members.len() {
        return Err("ambiguous duplicate workspace member IDs".into());
    }
    let mut found = BTreeSet::new();
    let mut manifests = BTreeSet::new();
    let mut namespaces = BTreeSet::new();
    let mut selected = Vec::new();
    for package in metadata.packages {
        if !members.contains(&package.id) {
            continue;
        }
        if !found.insert(package.id.clone()) {
            return Err("ambiguous duplicate package ID".into());
        }
        let manifest = canonical(&package.manifest_path)?;
        if manifest != workspace.join("Cargo.toml")
            && !manifest.starts_with(workspace.join("packages"))
        {
            return Err(format!(
                "member manifest escape outside packages/: {}",
                manifest.display()
            ));
        }
        if !manifests.insert(manifest.clone()) {
            return Err("ambiguous duplicate member manifests".into());
        }
        let Some(declaration) = package.metadata.and_then(|metadata| metadata.digest) else {
            continue;
        };
        if !manifest.starts_with(workspace.join("packages")) {
            return Err("contributor must be a member under packages/".into());
        }
        if declaration.role != "contributor" {
            return Err(format!("unsupported digest role {:?}", declaration.role));
        }
        if declaration.protocol != PROTOCOL_VERSION {
            return Err(format!(
                "{}: unsupported protocol {}",
                declaration.namespace, declaration.protocol
            ));
        }
        let mut chars = declaration.namespace.chars();
        if !chars.next().is_some_and(|c| c.is_ascii_lowercase())
            || !chars
                .all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || matches!(c, '.' | '-'))
        {
            return Err(format!("invalid namespace {:?}", declaration.namespace));
        }
        if !namespaces.insert(declaration.namespace.clone()) {
            return Err(format!("duplicate namespace {}", declaration.namespace));
        }
        if declaration.scopes.is_empty() {
            return Err(format!("{}: scopes cannot be empty", declaration.namespace));
        }
        let mut scopes = BTreeSet::new();
        for scope in &declaration.scopes {
            let normalized = normalize_scope(scope)
                .map_err(|e| format!("{} scope: {e}", declaration.namespace))?;
            if normalized != *scope {
                return Err(format!("scope {scope:?} is not normalized"));
            }
            if !scopes.insert(scope) {
                return Err(format!("duplicate scope {scope:?}"));
            }
        }
        let matches = package
            .targets
            .iter()
            .filter(|t| t.name == declaration.binary && t.kind == ["bin"])
            .count();
        if matches != 1 {
            return Err(format!(
                "{}: expected exactly one binary target {:?}, found {matches}",
                declaration.namespace, declaration.binary
            ));
        }
        if declaration.scopes.iter().any(|s| scope_matches(scope, s)) {
            selected.push(Contributor {
                package_id: package.id,
                declaration,
            });
        }
    }
    if found.len() != members.len() {
        return Err("metadata omitted a declared workspace member".into());
    }
    selected.sort_by(|a, b| a.declaration.namespace.cmp(&b.declaration.namespace));
    if selected.is_empty() {
        return Err(format!("no contributors match scope {scope:?}"));
    }
    Ok(selected)
}
pub(super) fn canonical(path: &Path) -> Result<PathBuf, String> {
    path.canonicalize()
        .map_err(|e| format!("{}: {e}", path.display()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::{Value, json};
    fn metadata() -> (PathBuf, Value) {
        let workspace = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .canonicalize()
            .unwrap();
        let root = workspace.parent().unwrap().parent().unwrap().to_owned();
        let value = json!({"workspace_root":workspace,"workspace_members":["opaque-member"],"packages":[{"id":"opaque-member","manifest_path":workspace.join("packages/protocol/Cargo.toml"),"metadata":{"digest":{"role":"contributor","namespace":"fixture.owner","protocol":1,"binary":"collector","scopes":["domains/thing"]}},"targets":[{"name":"collector","kind":["bin"]}]}]});
        (root, value)
    }
    #[test]
    fn only_actual_members_are_enrolled_and_all_member_ids_must_exist() {
        let (root, mut value) = metadata();
        value["workspace_members"] = json!([]);
        assert!(
            discover(&serde_json::to_vec(&value).unwrap(), &root, ".")
                .unwrap_err()
                .contains("no contributors")
        );
        value["workspace_members"] = json!(["missing-id"]);
        assert!(
            discover(&serde_json::to_vec(&value).unwrap(), &root, ".")
                .unwrap_err()
                .contains("omitted")
        );
    }
    #[test]
    fn duplicate_binary_targets_are_ambiguous() {
        let (root, mut value) = metadata();
        let target = value["packages"][0]["targets"][0].clone();
        value["packages"][0]["targets"]
            .as_array_mut()
            .unwrap()
            .push(target);
        assert!(
            discover(&serde_json::to_vec(&value).unwrap(), &root, ".")
                .unwrap_err()
                .contains("found 2")
        );
    }
    #[test]
    fn incorrect_workspace_root_and_duplicate_member_ids_fail() {
        let (root, mut value) = metadata();
        value["workspace_root"] = json!(root);
        assert!(
            discover(&serde_json::to_vec(&value).unwrap(), &root, ".")
                .unwrap_err()
                .contains("workspace root")
        );
        let (_, mut value) = metadata();
        value["workspace_members"] = json!(["opaque-member", "opaque-member"]);
        assert!(
            discover(&serde_json::to_vec(&value).unwrap(), &root, ".")
                .unwrap_err()
                .contains("duplicate workspace member")
        );
    }
}
