/// A type of data that can be stored in the database.
pub trait DatabaseType {
  /// The type of entity.
  type Entity;
  /// The type of identifier.
  type Identifier;
  /// The type of name.
  type Name;
  /// The type of builder.
  type Builder;
}
