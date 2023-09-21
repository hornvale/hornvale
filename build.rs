use convert_case::{Case, Casing};
use std::fmt::Write as _;
use std::fs;
use std::path::Path;

fn main() {
  let macros_dir = Path::new("src/_macro/macros");
  let mod_file_path = macros_dir.join("mod.rs");

  // Get all .rs files in the macros directory
  let entries = fs::read_dir(macros_dir).expect("Failed to read macros directory");
  let rs_files: Vec<_> = entries
    .filter_map(|e| {
      if let Ok(entry) = e {
        let path = entry.path();
        if path.extension()? == "rs" && path.file_stem()? != "mod" {
          return Some(path.file_stem()?.to_string_lossy().into_owned());
        }
      }
      None
    })
    .collect();

  // Generate the content for mod.rs
  let mut content = String::new();
  for file in &rs_files {
    writeln!(content, "#[macro_use]\npub mod {};", file).expect("Failed to write to content string");
  }

  // Write the content to mod.rs
  fs::write(mod_file_path, content).expect("Failed to write mod.rs");

  let components_dir = Path::new("src/ecs/component/components");
  let mod_file_path = components_dir.join("mod.rs");

  // Get all subdirectories in the components directory
  let entries = fs::read_dir(components_dir).expect("Failed to read components directory");
  let subdirs: Vec<_> = entries
    .filter_map(|e| {
      if let Ok(entry) = e {
        let path = entry.path();
        if path.is_dir() {
          return Some(path.file_name()?.to_string_lossy().into_owned());
        }
      }
      None
    })
    .collect();

  // Generate the content for mod.rs
  let mut content = String::new();
  writeln!(content, "use specs::prelude::*;\n").expect("Failed to write to content string");
  for subdir in &subdirs {
    let struct_name = subdir.to_case(Case::UpperCamel);
    writeln!(content, "pub mod {};", subdir).expect("Failed to write to content string");
    writeln!(content, "pub use {}::{};", subdir, struct_name).expect("Failed to write to content string");
  }
  writeln!(content).expect("Failed to write to content string");
  writeln!(content, "pub fn register_components(ecs: &mut World) {{").expect("Failed to write to content string");
  for subdir in &subdirs {
    let struct_name = subdir.to_case(Case::UpperCamel);
    writeln!(content, "  ecs.register::<{}>();", struct_name).expect("Failed to write to content string");
  }
  writeln!(content, "}}").expect("Failed to write to content string");

  // Write the content to mod.rs
  fs::write(mod_file_path, content).expect("Failed to write mod.rs");
}
