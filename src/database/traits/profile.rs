use crate::database::prelude_internal::*;
use crate::profile::prelude::*;
use anyhow::Error as AnyError;
use std::path::PathBuf;

/// Trait to define methods for dealing with profiles:
/// - Load a profile from a file.
/// - Unload the profile.
/// - Save a profile to a file.
/// - Delete the profile file.
pub trait ProfileExt {
  /// Get the path to the profile file.
  fn get_profile_path(&self, profile_identifier: &ProfileIdentifier) -> PathBuf;

  /// Read a profile from a file.
  fn read_profile(&self, profile_identifier: &ProfileIdentifier) -> Result<Profile, AnyError>;

  /// Write a profile to a file.
  fn write_profile(&self, profile: &Profile) -> Result<(), AnyError>;

  /// Delete the profile file.
  fn delete_profile(&self, profile_identifier: &ProfileIdentifier) -> Result<(), AnyError>;

  /// Load a profile from a file.
  fn load_profile(&mut self, profile_identifier: &ProfileIdentifier) -> Result<(), AnyError>;

  /// Unload the profile.
  fn unload_profile(&mut self) -> Result<Option<Profile>, AnyError>;
}

impl ProfileExt for Database {
  /// Get the path to the profile file.
  fn get_profile_path(&self, profile_identifier: &ProfileIdentifier) -> PathBuf {
    let filename = FilenameFormatter::Profile(profile_identifier.clone()).filename();
    self.data_dir().join(filename)
  }

  /// Read a profile from a file.
  fn read_profile(&self, profile_identifier: &ProfileIdentifier) -> Result<Profile, AnyError> {
    let path = self.get_profile_path(profile_identifier);
    let profile = Profile::from_json(&self.read_file_contents(&path)?);
    Ok(profile)
  }

  /// Write a profile to a file.
  fn write_profile(&self, profile: &Profile) -> Result<(), AnyError> {
    let path = self.get_profile_path(&profile.identifier);
    self.write_file_contents(&path, &profile.to_json())?;
    Ok(())
  }

  /// Delete the profile file.
  fn delete_profile(&self, profile_identifier: &ProfileIdentifier) -> Result<(), AnyError> {
    let path = self.get_profile_path(profile_identifier);
    self.delete_file(&path)?;
    Ok(())
  }

  /// Load a profile from a file.
  fn load_profile(&mut self, profile_identifier: &ProfileIdentifier) -> Result<(), AnyError> {
    self.profile = Some(self.read_profile(profile_identifier)?);
    Ok(())
  }

  /// Unload the profile.
  fn unload_profile(&mut self) -> Result<Option<Profile>, AnyError> {
    Ok(self.profile.take())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::region::prelude::*;
  use crate::test_utilities::prelude::*;
  use tempfile::tempdir;

  #[test]
  fn test_get_profile_path() {
    init();
    let path = tempdir().unwrap().path().join("profile.json");
    let database = Database::at_path(&path.parent().unwrap());
    let profile_identifier = ProfileIdentifier("test".to_string());
    assert_eq!(database.get_profile_path(&profile_identifier), path);
  }

  #[test]
  fn test_read_profile() {
    init();
    let path = tempdir().unwrap().path().join("profile.json");
    let database = Database::at_path(&path.parent().unwrap());
    let profile_identifier = ProfileIdentifier("test".to_string());
    let profile = Profile {
      identifier: profile_identifier,
      name: "test".to_string(),
      region: RegionIdentifier("test".to_string()),
    };
    database.write_profile(&profile).unwrap();
    let read_profile = database.read_profile(&profile_identifier).unwrap();
    assert_eq!(profile, read_profile);
    database.delete_profile(&profile_identifier).unwrap();
    assert!(database.read_profile(&profile_identifier).is_err());
  }

  #[test]
  fn test_write_profile() {
    init();
    let path = tempdir().unwrap().path().join("profile.json");
    let database = Database::at_path(&path.parent().unwrap());
    let profile_identifier = ProfileIdentifier("test".to_string());
    let profile = Profile {
      identifier: profile_identifier,
      name: "test".to_string(),
      region: RegionIdentifier("test".to_string()),
    };
    database.write_profile(&profile).unwrap();
    let read_profile = database.read_profile(&profile_identifier).unwrap();
    assert_eq!(profile, read_profile);
    database.delete_profile(&profile_identifier).unwrap();
  }

  #[test]
  fn test_delete_profile() {
    init();
    let path = tempdir().unwrap().path().join("profile.json");
    let database = Database::at_path(&path.parent().unwrap());
    let profile_identifier = ProfileIdentifier("test".to_string());
    let profile = Profile {
      identifier: profile_identifier,
      name: "test".to_string(),
      region: RegionIdentifier("test".to_string()),
    };
    database.write_profile(&profile).unwrap();
    database.delete_profile(&profile_identifier).unwrap();
    assert!(database.read_profile(&profile_identifier).is_err());
  }

  #[test]
  fn test_load_profile() {
    init();
    let path = tempdir().unwrap().path().join("profile.json");
    let mut database = Database::at_path(&path.parent().unwrap());
    let profile_identifier = ProfileIdentifier("test".to_string());
    let profile = Profile {
      identifier: profile_identifier,
      name: "test".to_string(),
      region: RegionIdentifier("test".to_string()),
    };
    database.write_profile(&profile).unwrap();
    database.load_profile(&profile_identifier).unwrap();
    assert_eq!(database.profile, Some(profile));
    database.delete_profile(&profile_identifier).unwrap();
  }

  #[test]
  fn test_unload_profile() {
    init();
    let path = tempdir().unwrap().path().join("profile.json");
    let mut database = Database::at_path(&path.parent().unwrap());
    let profile_identifier = ProfileIdentifier("test".to_string());
    let profile = Profile {
      identifier: profile_identifier,
      name: "test".to_string(),
      region: RegionIdentifier("test".to_string()),
    };
    database.write_profile(&profile).unwrap();
    database.load_profile(&profile_identifier).unwrap();
    assert_eq!(database.profile, Some(profile));
    let unloaded_profile = database.unload_profile().unwrap();
    assert_eq!(unloaded_profile, Some(profile));
    assert_eq!(database.profile, None);
    database.delete_profile(&profile_identifier).unwrap();
  }
}
