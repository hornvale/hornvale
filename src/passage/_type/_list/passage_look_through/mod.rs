/// Whether this passage can be looked through.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub enum PassageLookThrough {
  Yes,
  No,
  IfOpen,
}