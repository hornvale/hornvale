#![allow(dead_code)]
use convert_case::{Case, Casing};
use std::fmt::Write as _;
use std::fs;
use std::path::Path;

fn main() {
  generate_macro_mod();
  generate_list_mod_with_alias("src/action", "Action");
  generate_list_mod_with_alias("src/action/_trait", "Trait");
  generate_list_mod_with_alias("src/command", "Command");
  generate_list_mod_with_alias("src/command/_trait", "Trait");
  generate_list_mod_with_export("src/command/_type");
  generate_list_mod_with_export("src/component");
  generate_list_mod_with_alias("src/effect", "Effect");
  generate_list_mod_with_alias("src/effect/_trait", "Trait");
  generate_list_mod_with_alias("src/event", "Event");
  generate_event_channel_register_mod();
  generate_list_mod_with_alias("src/entity_id", "Id");
  generate_list_mod_with_alias("src/resource", "Resource");
  generate_list_mod_with_alias("src/system", "System");
  generate_list_mod_with_alias("src/system_data/_trait", "Trait");
  generate_list_mod_with_export("src/system_data/_type");
}
//  generate_component_register_mod();

// generate_mod_with_export("event", "Event");
// generate_mod_with_export("resource", "Resource");
// generate_mod_with_export("system", "System");
//  generate_system_data_traits_mod();

fn generate_event_channel_register_mod() {
  let event_names = get_event_names();
  let mut content = String::new();
  write_mod_header(&mut content);
  writeln!(content, "use specs::prelude::*;").expect("Failed to write to content string");
  writeln!(content, "use specs::shrev::EventChannel;").expect("Failed to write to content string");
  writeln!(content).expect("Failed to write to content string");
  writeln!(content, "use crate::event::_list::*;").expect("Failed to write to content string");
  writeln!(content).expect("Failed to write to content string");
  writeln!(content, "pub fn insert_event_channels(ecs: &mut World) {{").expect("Failed to write to content string");
  for event_name in &event_names {
    let struct_name = format!("{}Event", event_name.to_case(Case::UpperCamel));
    writeln!(content, "  ecs.insert(EventChannel::<{}>::new());", struct_name)
      .expect("Failed to write to content string");
  }
  writeln!(content, "}}").expect("Failed to write to content string");
  fs::write(Path::new("src/event_channel/register/mod.rs"), content)
    .expect("Failed to write src/event_channel/register/mod.rs");
}

fn get_event_names() -> Vec<String> {
  let dir = Path::new("src/event/_list");
  let mut result = get_subdirs_in_dir(dir);
  result.sort();
  result
}

fn get_component_names() -> Vec<String> {
  let dir = Path::new("src/component/_list");
  let mut result = get_subdirs_in_dir(dir);
  result.sort();
  result
}

fn generate_component_register_mod() {
  let component_names = get_component_names();
  let mut content = String::new();
  write_mod_header(&mut content);
  writeln!(content, "use specs::prelude::*;\n").expect("Failed to write to content string");
  writeln!(content, "use crate::component::_list::*;\n").expect("Failed to write to content string");
  writeln!(content, "pub fn register_components(ecs: &mut World) {{").expect("Failed to write to content string");
  for component_name in &component_names {
    let struct_name = component_name.to_case(Case::UpperCamel);
    writeln!(content, "  ecs.register::<{}>();", struct_name).expect("Failed to write to content string");
  }
  writeln!(content, "}}").expect("Failed to write to content string");
  fs::write(Path::new("src/component/register/mod.rs"), content).expect("Failed to write mod.rs");
}

fn generate_list_mod_with_alias(base_dir: &str, suffix: &str) {
  let path = format!("{}/_list", base_dir);
  let dir = Path::new(&path);
  let mut subdirs = get_subdirs_in_dir(dir);
  subdirs.sort();
  let mut content = String::new();
  write_mod_header(&mut content);
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

fn generate_list_mod_with_export(base_dir: &str) {
  let path = format!("{}/_list", base_dir);
  let dir = Path::new(&path);
  let mut subdirs = get_subdirs_in_dir(dir);
  subdirs.sort();
  let mut content = String::new();
  write_mod_header(&mut content);
  for subdir in &subdirs {
    writeln!(content, "pub mod {};", subdir).expect("Failed to write to content string");
    writeln!(content, "pub use {}::{};", subdir, subdir.to_case(Case::UpperCamel))
      .expect("Failed to write to content string");
  }
  fs::write(dir.join("mod.rs"), content).expect("Failed to write mod.rs");
}

fn generate_system_data_traits_mod() {
  let dir = Path::new("src/system_data/_trait/traits");
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
    .unwrap_or_else(|_| panic!("Failed to read directory {:?}", dir))
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

fn generate_macro_mod() {
  let dir = Path::new("src/_macro/_list");
  let mut files = get_files_in_dir(dir);
  files.sort();
  let mut content = String::new();
  write_mod_header(&mut content);
  for file in &files {
    writeln!(content, "#[macro_use]\npub mod {};", file).expect("Failed to write to content string");
  }
  fs::write(dir.join("mod.rs"), content).expect("Failed to write mod.rs");
}

fn write_mod_header(content: &mut String) {
  writeln!(
    content,
    "// This file is generated (see build.rs). Please do not edit manually."
  )
  .expect("Failed to write to content string");
}
