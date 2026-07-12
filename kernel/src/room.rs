//! The Room Mesh: rooms are triangular faces of the same icosphere as
//! `Geosphere`, refined deeper and addressed by `(base face + child path)`.
//! Lazy, deterministic, and byte-identical to a fully-built `Geosphere`.
//! Identity, adjacency, and seeding are integer/rational; transcendentals live
//! only in position geometry (registry MAP-28).

/// The deepest path a `RoomId` can pack: 5 face bits + 1 sentinel + 2*29 digit
/// bits = 64. Useful room scale is ~L16-20 (an L18 room edge is ~27 m).
/// type-audit: bare-ok(count)
pub const MAX_DEPTH: usize = 29;

/// A room — a triangular face of the icosphere at refinement depth
/// `path.len()`. Keyed to the base icosahedron face (level 0), so the address
/// is independent of the world's canonical globe level.
/// type-audit: bare-ok(index: face), bare-ok(index: path)
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct RoomAddr {
    /// Which of the 20 base icosahedron faces (0..20).
    pub face: u8,
    /// Child index (0..4) at each refinement, from the base face down.
    pub path: Vec<u8>,
}

/// Packed, serialized form of a `RoomAddr` — a frozen save-format contract.
/// Layout: bits `[0,5)` = face; bits `[5,64)` = a leading-1 sentinel then 2
/// bits per digit, root digit first.
/// type-audit: bare-ok(constructor-edge)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct RoomId(pub u64);

/// Why a `RoomAddr` could not be packed.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RoomAddrError {
    /// `path.len()` exceeds `MAX_DEPTH`, so it will not fit a `u64`.
    DepthExceedsCap,
    /// A path digit was not a child index in `0..4`, or `face >= 20`.
    Invalid,
}

/// Why a `u64` is not a valid `RoomId`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RoomIdError {
    /// The face field is not a base face (`>= 20`), or the sentinel is missing.
    Malformed,
}

impl RoomAddr {
    /// Refinement depth = path length.
    /// type-audit: bare-ok(count)
    pub fn depth(&self) -> u32 {
        self.path.len() as u32
    }

    /// Pack to the `u64` `RoomId` contract. Fails past `MAX_DEPTH`.
    pub fn pack(&self) -> Result<RoomId, RoomAddrError> {
        if self.path.len() > MAX_DEPTH {
            return Err(RoomAddrError::DepthExceedsCap);
        }
        if self.face >= 20 || self.path.iter().any(|&d| d >= 4) {
            return Err(RoomAddrError::Invalid);
        }
        let mut pathword: u64 = 1; // sentinel
        for &d in &self.path {
            pathword = (pathword << 2) | u64::from(d);
        }
        Ok(RoomId((pathword << 5) | u64::from(self.face)))
    }
}

impl RoomId {
    /// Unpack to a `RoomAddr`. Validates the face and the sentinel; not total.
    pub fn unpack(&self) -> Result<RoomAddr, RoomIdError> {
        let face = (self.0 & 0x1F) as u8;
        if face >= 20 {
            return Err(RoomIdError::Malformed);
        }
        let pathword = self.0 >> 5;
        if pathword == 0 {
            return Err(RoomIdError::Malformed);
        }
        let top = 63 - pathword.leading_zeros(); // index of the sentinel bit = 2*len
        let len = (top / 2) as usize;
        let mut path = Vec::with_capacity(len);
        for i in (0..len).rev() {
            path.push(((pathword >> (2 * i)) & 0b11) as u8);
        }
        Ok(RoomAddr { face, path })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn roomid_round_trips() {
        let cases = [
            RoomAddr {
                face: 0,
                path: vec![],
            },
            RoomAddr {
                face: 19,
                path: vec![3],
            },
            RoomAddr {
                face: 7,
                path: vec![0, 1, 2, 3, 0, 1, 2],
            },
            RoomAddr {
                face: 3,
                path: vec![3; MAX_DEPTH],
            },
        ];
        for addr in cases {
            let id = addr.pack().expect("packs");
            assert_eq!(
                id.unpack().expect("unpacks"),
                addr,
                "round-trip for {addr:?}"
            );
        }
    }

    #[test]
    fn pack_rejects_over_cap() {
        let too_deep = RoomAddr {
            face: 0,
            path: vec![0; MAX_DEPTH + 1],
        };
        assert_eq!(too_deep.pack(), Err(RoomAddrError::DepthExceedsCap));
    }

    #[test]
    fn unpack_rejects_malformed() {
        assert_eq!(RoomId(20).unpack(), Err(RoomIdError::Malformed)); // face 20, no path
        assert_eq!(RoomId(0).unpack(), Err(RoomIdError::Malformed)); // face 0, pathword 0
    }

    #[test]
    fn pack_rejects_invalid() {
        assert_eq!(
            RoomAddr {
                face: 20,
                path: vec![]
            }
            .pack(),
            Err(RoomAddrError::Invalid)
        );
        assert_eq!(
            RoomAddr {
                face: 0,
                path: vec![4]
            }
            .pack(),
            Err(RoomAddrError::Invalid)
        );
    }
}
