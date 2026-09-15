//! Visual-only Level-6 cube-face address selection.
//!
//! This is deliberately a wire-address helper, not the semantic `Facet`
//! type. The renderer needs a camera-facing patch ring but must not reach
//! through the visual boundary into the simulation kernel.

const DEPTH: u32 = 6;
const FACES: [[[i64; 3]; 3]; 6] = [
    [[1, 0, 0], [0, 1, 0], [0, 0, 1]],
    [[-1, 0, 0], [0, -1, 0], [0, 0, 1]],
    [[0, 1, 0], [-1, 0, 0], [0, 0, 1]],
    [[0, -1, 0], [1, 0, 0], [0, 0, 1]],
    [[0, 0, 1], [1, 0, 0], [0, 1, 0]],
    [[0, 0, -1], [-1, 0, 0], [0, 1, 0]],
];

#[derive(Clone, Copy)]
struct Address {
    face: usize,
    x: i64,
    y: i64,
}

#[derive(Clone, Copy)]
struct Seam {
    to_face: usize,
    along_is_y: bool,
    reverse: bool,
    high: bool,
}

fn dot(a: [i64; 3], b: [f64; 3]) -> f64 {
    a[0] as f64 * b[0] + a[1] as f64 * b[1] + a[2] as f64 * b[2]
}

pub fn locate(direction: [f64; 3]) -> (usize, f64, f64) {
    let (face, _) = FACES
        .iter()
        .enumerate()
        .map(|(index, [normal, _, _])| (index, dot(*normal, direction)))
        .max_by(|left, right| left.1.total_cmp(&right.1))
        .expect("visual cube faces are nonempty");
    let [normal, u, v] = FACES[face];
    let normal_projection = dot(normal, direction);
    (
        face,
        libm::atan(dot(u, direction) / normal_projection) / std::f64::consts::FRAC_PI_4,
        libm::atan(dot(v, direction) / normal_projection) / std::f64::consts::FRAC_PI_4,
    )
}

fn pack(address: Address) -> u32 {
    let mut pathword = 1_u32;
    for bit in (0..DEPTH).rev() {
        let digit = ((((address.x as u32) >> bit) & 1) << 1) | (((address.y as u32) >> bit) & 1);
        pathword = (pathword << 2) | digit;
    }
    (pathword << 5) | address.face as u32
}

fn unpack(macro_face: u32) -> Address {
    let face = (macro_face & 0x1f) as usize;
    let pathword = macro_face >> 5;
    let mut x = 0_i64;
    let mut y = 0_i64;
    for bit in (0..DEPTH).rev() {
        let digit = (pathword >> (2 * bit)) & 0b11;
        x = (x << 1) | i64::from(digit >> 1);
        y = (y << 1) | i64::from(digit & 1);
    }
    Address { face, x, y }
}

fn seam_table() -> [[Seam; 4]; 6] {
    let mut result = [[Seam {
        to_face: 0,
        along_is_y: false,
        reverse: false,
        high: false,
    }; 4]; 6];
    for (face, slots) in result.iter_mut().enumerate() {
        let [normal, u, v] = FACES[face];
        for (side, slot) in slots.iter_mut().enumerate() {
            let (c, along, outward) = match side {
                0 => (sub(normal, u), v, neg(u)),
                1 => (add(normal, u), v, u),
                2 => (sub(normal, v), u, neg(v)),
                _ => (add(normal, v), u, v),
            };
            let to_face = FACES
                .iter()
                .position(|[candidate, _, _]| *candidate == outward)
                .expect("visual cube seam has a destination face");
            let [_, destination_u, destination_v] = FACES[to_face];
            let eu = idot(along, destination_u);
            let ev = idot(along, destination_v);
            let cu = idot(c, destination_u);
            let cv = idot(c, destination_v);
            *slot = if eu != 0 {
                Seam {
                    to_face,
                    along_is_y: false,
                    reverse: eu < 0,
                    high: cv > 0,
                }
            } else {
                Seam {
                    to_face,
                    along_is_y: true,
                    reverse: ev < 0,
                    high: cu > 0,
                }
            };
        }
    }
    result
}

fn idot(a: [i64; 3], b: [i64; 3]) -> i64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}
fn add(a: [i64; 3], b: [i64; 3]) -> [i64; 3] {
    [a[0] + b[0], a[1] + b[1], a[2] + b[2]]
}
fn sub(a: [i64; 3], b: [i64; 3]) -> [i64; 3] {
    [a[0] - b[0], a[1] - b[1], a[2] - b[2]]
}
fn neg(a: [i64; 3]) -> [i64; 3] {
    [-a[0], -a[1], -a[2]]
}

pub fn neighbors(macro_face: u32) -> Vec<u32> {
    let center = unpack(macro_face);
    let scale = 1_i64 << DEPTH;
    let seams = seam_table();
    let mut result = Vec::with_capacity(8);
    for (dx, dy) in [
        (1, 0),
        (0, 1),
        (-1, 0),
        (0, -1),
        (1, 1),
        (-1, 1),
        (-1, -1),
        (1, -1),
    ] {
        let x = center.x + dx;
        let y = center.y + dy;
        let off_x = !(0..scale).contains(&x);
        let off_y = !(0..scale).contains(&y);
        match (off_x, off_y) {
            (false, false) => result.push(pack(Address {
                face: center.face,
                x,
                y,
            })),
            (true, false) => {
                let seam = seams[center.face][if x < 0 { 0 } else { 1 }];
                let along = if seam.reverse { scale - 1 - y } else { y };
                let depth = if seam.high { scale - 1 } else { 0 };
                result.push(pack(Address {
                    face: seam.to_face,
                    x: if seam.along_is_y { depth } else { along },
                    y: if seam.along_is_y { along } else { depth },
                }));
            }
            (false, true) => {
                let seam = seams[center.face][if y < 0 { 2 } else { 3 }];
                let along = if seam.reverse { scale - 1 - x } else { x };
                let depth = if seam.high { scale - 1 } else { 0 };
                result.push(pack(Address {
                    face: seam.to_face,
                    x: if seam.along_is_y { depth } else { along },
                    y: if seam.along_is_y { along } else { depth },
                }));
            }
            (true, true) => {}
        }
    }
    result
}
