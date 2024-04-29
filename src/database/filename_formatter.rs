use crate::profile::prelude::*;
use crate::region::prelude::*;

/// A filename formatter for items managed by the database.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum FilenameFormatter {
  /// Profile filename formatter.
  Profile(ProfileIdentifier),
  /// Region Map filename formatter.
  RegionMap,
  /// Region Adjacency Map filename formatter.
  RegionAdjacencyMap(RegionIdentifier),
}

impl FilenameFormatter {
  /// Get the filename.
  pub fn filename(&self) -> String {
    match self {
      Self::Profile(profile_identifier) => format!("profiles/{}", profile_identifier.0),
      Self::RegionMap => "region_map".to_string(),
      Self::RegionAdjacencyMap(region_identifier) => {
        format!("{}/region_adjacency_map", region_identifier)
      },
    }
  }
}
