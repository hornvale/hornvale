use std::fs::create_dir_all;
use std::path::PathBuf;

#[cfg(not(test))]
lazy_static! {
  /// This is an example for using doc comment attributes
  pub static ref LOCAL_DATA_DIR: Option<PathBuf> = {
    let mut path = dirs::data_local_dir().unwrap();
    path.push("hornvale");
    create_dir_all(&path).unwrap();
    Some(path)
  };
}

#[cfg(test)]
lazy_static! {
  /// This is an example for using doc comment attributes
  pub static ref LOCAL_DATA_DIR: Option<PathBuf> = {
    let mut path = PathBuf::from(crate::test::TEST_DATA_DIRECTORY);
    path.push("temporary/hornvale");
    create_dir_all(&path).unwrap();
    Some(path)
  };
}
