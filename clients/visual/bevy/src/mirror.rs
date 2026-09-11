//! Atomic, binding-qualified snapshot acceptance. Source transport is app-owned.
use crate::{
    ViewError,
    documents::{self, Initial, Reply},
};
#[derive(Clone, bevy::prelude::Resource)]
pub struct ObservationMirror {
    initial: std::sync::Arc<Initial>,
    next: u64,
    retired_before: u64,
    generation: u64,
    pending: Option<(u64, i64)>,
    current: Option<Reply>,
}
impl ObservationMirror {
    pub fn new(json: &str) -> Result<Self, ViewError> {
        Ok(Self {
            initial: std::sync::Arc::new(documents::initial(json)?),
            next: 0,
            retired_before: 0,
            generation: 0,
            pending: None,
            current: None,
        })
    }
    pub fn request(&mut self, ticks: i64) -> Result<String, ViewError> {
        let id = self.next;
        self.next = self
            .next
            .checked_add(1)
            .ok_or_else(|| ViewError::Range("request counter overflow".into()))?;
        self.pending = Some((id, ticks));
        Ok(serde_json::json!({"schema":"visual/request/v1","binding":self.initial.binding,"request_id":id,"ticks":ticks}).to_string())
    }
    pub fn accept(&mut self, json: &str) -> Result<bool, ViewError> {
        let reply = documents::reply(json)?;
        if reply.request_id < self.retired_before {
            return Ok(false);
        }
        if reply.binding != self.initial.binding {
            return Err(ViewError::Binding(
                "reply belongs to another source binding".into(),
            ));
        }
        if reply.request_id >= self.next {
            return Err(ViewError::Document("reply was never requested".into()));
        }
        if let Some(current) = &self.current
            && reply.request_id == current.request_id
            && serde_json::to_value(&reply)? != serde_json::to_value(current)?
        {
            return Err(ViewError::Document(
                "reply conflicts with committed observation".into(),
            ));
        }
        let Some((id, ticks)) = self.pending else {
            return Ok(false);
        };
        if reply.request_id < id {
            return Ok(false);
        }
        if reply.request_id != id
            || reply.ticks != ticks
            || reply.astronomy.ticks_per_std_day != self.initial.ticks_per_std_day
        {
            return Err(ViewError::Document(
                "reply conflicts with pending observation".into(),
            ));
        }
        for m in &self.initial.moons.moons {
            if !reply
                .astronomy
                .bodies
                .iter()
                .any(|b| b.id == format!("moon:{}", m.index) && b.radius_km == Some(m.radius_km))
            {
                return Err(ViewError::Document(
                    "moon inventory/radius differs from initial document".into(),
                ));
            }
        }
        let expected_stars = if self.initial.system["stellar"]["topology"] == "single" {
            1
        } else {
            2
        };
        let expected_wanderers = self.initial.system["wanderers"]
            .as_array()
            .expect("validated inventory")
            .len();
        if !(0..expected_wanderers).all(|index| {
            reply
                .astronomy
                .bodies
                .iter()
                .any(|b| b.id == format!("wanderer:{index}") && b.kind == "wanderer")
        }) || !(0..expected_stars).all(|index| {
            reply
                .astronomy
                .bodies
                .iter()
                .any(|b| b.id == format!("star:{index}") && b.kind == "star")
        }) || reply.astronomy.seed != self.initial.tiles.seed
            || reply
                .astronomy
                .bodies
                .iter()
                .filter(|b| b.kind == "moon")
                .count()
                != self.initial.moons.moons.len()
            || reply
                .astronomy
                .bodies
                .iter()
                .filter(|b| b.kind == "star")
                .count()
                != expected_stars
            || reply
                .astronomy
                .bodies
                .iter()
                .filter(|b| b.kind == "wanderer")
                .count()
                != expected_wanderers
        {
            return Err(ViewError::Document(
                "observation differs from initial source inventory".into(),
            ));
        }
        if let Some(current) = &self.current {
            for old in &current.astronomy.bodies {
                if !reply
                    .astronomy
                    .bodies
                    .iter()
                    .any(|b| b.id == old.id && b.kind == old.kind && b.radius_km == old.radius_km)
                {
                    return Err(ViewError::Document(
                        "observation changed static body identity".into(),
                    ));
                }
            }
        }
        documents::geometry(&self.initial, &reply.astronomy, 1000.0)?;
        self.current = Some(reply);
        self.pending = None;
        Ok(true)
    }
    pub fn reset(&mut self, json: &str) -> Result<(), ViewError> {
        let next = self.next;
        let generation = self
            .generation
            .checked_add(1)
            .ok_or_else(|| ViewError::Range("reset generation overflow".into()))?;
        *self = Self::new(json)?;
        self.next = next;
        self.retired_before = next;
        self.generation = generation;
        Ok(())
    }
    pub fn generation(&self) -> u64 {
        self.generation
    }
    pub fn pending_ticks(&self) -> Option<i64> {
        self.pending.map(|(_, ticks)| ticks)
    }
    pub fn current_ticks(&self) -> Option<i64> {
        self.current.as_ref().map(|r| r.ticks)
    }
    pub fn initial(&self) -> &Initial {
        &self.initial
    }
    pub fn current(&self) -> Option<&Reply> {
        self.current.as_ref()
    }
}
