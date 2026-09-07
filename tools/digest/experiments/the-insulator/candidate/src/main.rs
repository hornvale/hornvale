//! Boundary probe for The Insulator's candidate workspace.
//!
//! This binary proves that the candidate can name and exchange the shared
//! protocol without importing publication policy or implementation authority.

fn main() {
    println!("insulator-candidate protocol={}", digest_protocol::PROTOCOL_VERSION);
}
