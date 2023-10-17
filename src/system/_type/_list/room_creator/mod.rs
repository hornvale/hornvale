use specs::prelude::*;
use specs::shrev::EventChannel;

use crate::component::*;
use crate::event::RoomRequestEvent;
use crate::resource::SeedStringResource;

/// The `RoomCreator` system.
///
/// This system creates each chunk.
#[derive(Debug, Default)]
pub struct RoomCreator {
  /// The event reader ID.
  pub reader_id: Option<ReaderId<RoomRequestEvent>>,
}

#[derive(SystemData)]
pub struct Data<'data> {
  pub entities: Entities<'data>,
  pub seed_string_resource: Read<'data, SeedStringResource>,
  pub rr_channel: Read<'data, EventChannel<RoomRequestEvent>>,
  pub is_in_chunk: WriteStorage<'data, IsInChunkComponent>,
  pub is_a_chunk: ReadStorage<'data, IsAChunkComponent>,
  pub is_a_room: WriteStorage<'data, IsARoomComponent>,
}

impl<'data> System<'data> for RoomCreator {
  type SystemData = Data<'data>;

  /// Run the system.
  fn run(&mut self, mut data: Self::SystemData) {
    debug!("Running RoomCreator system.");
    for event in data.rr_channel.read(self.reader_id.as_mut().unwrap()) {
      debug!("Creating room.");
      let chunk_entity = (&data.entities, &data.is_a_chunk)
        .join()
        .find(|(_, _)| true)
        .map(|(entity, _)| entity)
        .unwrap();
      let room_entity = data.entities.create();
      data
        .is_in_chunk
        .insert(room_entity, IsInChunkComponent(chunk_entity))
        .unwrap();
      let room = event.room.clone();
      data.is_a_room.insert(room_entity, IsARoomComponent(room)).unwrap();
      debug!("Created room.");
    }
  }

  fn setup(&mut self, world: &mut World) {
    Self::SystemData::setup(world);
    self.reader_id = Some(world.fetch_mut::<EventChannel<RoomRequestEvent>>().register_reader());
  }
}
