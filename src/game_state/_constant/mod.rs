use std::fs::create_dir_all;

#[cfg(not(test))]
lazy_static! {
  /// This is an example for using doc comment attributes
  pub static ref LOCAL_DATA_DIR: Option<String> = {
    let mut path = dirs::data_local_dir().unwrap();
    path.push("hornvale");
    create_dir_all(&path).unwrap();
    Some(path.to_str()?.to_string())
  };
}

#[cfg(test)]
lazy_static! {
  /// This is an example for using doc comment attributes
  pub static ref LOCAL_DATA_DIR: Option<String> = {
    let mut path = std::path::PathBuf::from(crate::test::TEST_DATA_DIRECTORY);
    path.push("temporary/hornvale");
    create_dir_all(&path).unwrap();
    Some(path.to_str()?.to_string())
  };
}
