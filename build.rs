use convert_case::{Case, Casing};
use std::fmt::Write as _;
use std::fs;
use std::path::Path;

fn main() {
  generate_macro_mod();
  generate_component_mod();
  generate_mod_with_export("src", "action", "Action");
  generate_mod_with_export("src", "command", "Command");
  generate_mod_with_export("src", "effect", "Effect");
  generate_mod_with_export("src/ecs", "event", "Event");
  generate_mod_with_export("src/ecs", "resource", "Resource");
  generate_mod_with_export("src/ecs", "system", "System");
  generate_system_data_traits_mod();
  generate_insert_event_channels();
}

fn generate_insert_event_channels() {
  let dir = Path::new("src/ecs/event/events");
  let subdirs = get_subdirs_in_dir(dir);
  let mut content = String::new();
  writeln!(
    content,
    "// This file is generated (see build.rs). Please do not edit manually."
  )
  .expect("Failed to write to content string");
  writeln!(content, "use specs::prelude::*;").expect("Failed to write to content string");
  writeln!(content, "use specs::shrev::EventChannel;").expect("Failed to write to content string");
  writeln!(content).expect("Failed to write to content string");
  writeln!(content, "use crate::ecs::event::events::*;").expect("Failed to write to content string");
  writeln!(content).expect("Failed to write to content string");
  writeln!(content, "pub fn insert_event_channels(ecs: &mut World) {{").expect("Failed to write to content string");
  for subdir in &subdirs {
    let struct_name = format!("{}Event", subdir.to_case(Case::UpperCamel));
    writeln!(content, "  ecs.insert(EventChannel::<{}>::new());", struct_name)
      .expect("Failed to write to content string");
  }
  writeln!(content, "}}").expect("Failed to write to content string");
  fs::write(Path::new("src/ecs/event/channels/mod.rs"), content)
    .expect("Failed to write src/ecs/event/channels/mod.rs");
}

fn generate_macro_mod() {
  let dir = Path::new("src/_macro/macros");
  let files = get_files_in_dir(dir);
  let mut content = String::new();
  writeln!(
    content,
    "// This file is generated (see build.rs). Please do not edit manually."
  )
  .expect("Failed to write to content string");
  for file in &files {
    writeln!(content, "#[macro_use]\npub mod {};", file).expect("Failed to write to content string");
  }
  fs::write(dir.join("mod.rs"), content).expect("Failed to write mod.rs");
}

fn generate_component_mod() {
  let dir = Path::new("src/ecs/component/components");
  let subdirs = get_subdirs_in_dir(dir);
  let mut content = String::new();
  writeln!(
    content,
    "// This file is generated (see build.rs). Please do not edit manually."
  )
  .expect("Failed to write to content string");
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
  fs::write(dir.join("mod.rs"), content).expect("Failed to write mod.rs");
}

fn generate_mod_with_export(base_dir: &str, base: &str, suffix: &str) {
  let path = format!("{}/{}/{}s", base_dir, base, base);
  let dir = Path::new(&path);
  let subdirs = get_subdirs_in_dir(dir);
  let mut content = String::new();
  writeln!(
    content,
    "// This file is generated (see build.rs). Please do not edit manually."
  )
  .expect("Failed to write to content string");
  for subdir in &subdirs {
    let struct_name = format!("{}{}", subdir.to_case(Case::UpperCamel), suffix);
    writeln!(content, "pub mod {};", subdir).expect("Failed to write to content string");
    writeln!(
      content,
      "pub use {}::{} as {};",
      subdir,
      subdir.to_case(Case::UpperCamel),
      struct_name
    )
    .expect("Failed to write to content string");
  }
  fs::write(dir.join("mod.rs"), content).expect("Failed to write mod.rs");
}

fn generate_system_data_traits_mod() {
  let dir = Path::new("src/ecs/system/data/_trait/traits");
  let subdirs = get_subdirs_in_dir(dir);
  let mut content = String::new();
  writeln!(
    content,
    "// This file is generated (see build.rs). Please do not edit manually."
  )
  .expect("Failed to write to content string");
  for subdir in &subdirs {
    let trait_name = subdir.to_case(Case::UpperCamel);
    let trait_full_name = format!("{}Trait", trait_name);
    writeln!(content, "pub mod {};", subdir).expect("Failed to write to content string");
    writeln!(content, "pub use {}::{} as {};", subdir, trait_name, trait_full_name)
      .expect("Failed to write to content string");
  }
  fs::write(dir.join("mod.rs"), content).expect("Failed to write mod.rs");
}

fn get_files_in_dir(dir: &Path) -> Vec<String> {
  fs::read_dir(dir)
    .expect("Failed to read directory")
    .filter_map(|e| {
      if let Ok(entry) = e {
        let path = entry.path();
        if path.extension()? == "rs" && path.file_stem()? != "mod" {
          return Some(path.file_stem()?.to_string_lossy().into_owned());
        }
      }
      None
    })
    .collect()
}

fn get_subdirs_in_dir(dir: &Path) -> Vec<String> {
  fs::read_dir(dir)
    .expect("Failed to read directory")
    .filter_map(|e| {
      if let Ok(entry) = e {
        let path = entry.path();
        if path.is_dir() {
          return Some(path.file_name()?.to_string_lossy().into_owned());
        }
      }
      None
    })
    .collect()
}
