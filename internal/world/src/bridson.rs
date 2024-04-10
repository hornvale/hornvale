use rand::prelude::*;
use std::f64::consts::PI;

/// Generate points using Bridson's algorithm for Poisson Disk Sampling.
///
pub fn sample<R: Rng + ?Sized>(
  rng: &mut R,
  (start_x, start_y): (i64, i64),
  (width, height): (i64, i64),
  interval: f64,
  k: i64,
) -> Vec<(i64, i64)> {
  // Cell side length
  let cell_size = interval / 2_f64.sqrt();
  let grid_width = (width as f64 / cell_size).ceil() as i64;
  let grid_height = (height as f64 / cell_size).ceil() as i64;

  // Grid initialization
  let mut grid = vec![None; (grid_width * grid_height) as usize];
  let mut queue = Vec::new();
  let mut samples = Vec::new();

  // Helper function to get grid index
  let get_grid_index =
    |x: i64, y: i64| -> usize { (x / cell_size as i64 + y / cell_size as i64 * grid_width) as usize };

  // Helper function to check if a point is far enough from others
  let far = |x: i64, y: i64, grid: &Vec<Option<(i64, i64)>>| -> bool {
    let grid_x = x / cell_size as i64;
    let grid_y = y / cell_size as i64;
    for i in (grid_x - 2).max(0)..(grid_x + 3).min(grid_width) {
      for j in (grid_y - 2).max(0)..(grid_y + 3).min(grid_height) {
        if let Some(Some((sx, sy))) = grid.get((i + j * grid_width) as usize) {
          let dx = sx - x;
          let dy = sy - y;
          if dx * dx + dy * dy < (interval as i64) * (interval as i64) {
            return false;
          }
        }
      }
    }
    true
  };

  // First sample
  let x = start_x + width / 2;
  let y = start_y + height / 2;
  queue.push((x, y));
  samples.push((x, y));
  grid[get_grid_index(x, y)] = Some((x, y));
  let pi2 = PI * 2.0;

  // Generate other samples
  loop {
    let i = rng.gen_range(0..queue.len());
    let (parent_x, parent_y) = queue[i];

    for _ in 0..k {
      let angle = rng.gen_range(0.0..pi2);
      let r = rng.gen_range(interval..interval * 2.0);
      let x = (parent_x as f64 + r * angle.cos()).round() as i64;
      let y = (parent_y as f64 + r * angle.sin()).round() as i64;
      if 0 <= x && x < width && 0 <= y && y < height && far(x, y, &grid) {
        queue.push((x, y));
        samples.push((x, y));
        grid[get_grid_index(x, y)] = Some((x, y));
        break;
      }
    }
    if queue.is_empty() {
      break;
    }
  }
  samples
}
