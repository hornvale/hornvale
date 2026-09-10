//! One source owner. Only serialized documents cross the app-owned boundary.
#![allow(
    clippy::disallowed_types,
    reason = "Instant measures query latency, never source or presentation time"
)]
use hornvale_visual_source::Source;
use std::{
    path::PathBuf,
    sync::{Arc, Condvar, Mutex, mpsc},
    thread::{self, JoinHandle},
    time::{Duration, Instant},
};
#[derive(Default)]
struct Slot {
    pending: Option<String>,
    completed: Option<Result<String, String>>,
    active: bool,
    closed: bool,
    queries: u64,
    coalesced: u64,
    last_query_micros: u128,
}
#[derive(Clone, Copy, Debug, Default)]
pub struct Diagnostics {
    pub queries: u64,
    pub coalesced: u64,
    pub pending: bool,
    pub active: bool,
    pub last_query_micros: u128,
}
pub struct Bridge {
    shared: Arc<(Mutex<Slot>, Condvar)>,
    worker: Option<JoinHandle<()>>,
}
impl Bridge {
    pub fn open(path: PathBuf, revision: String) -> Result<(Self, String), String> {
        Self::open_with_timeout(path, revision, None)
    }
    /// Capture startup has a deadline; timeout permanently abandons this owner.
    pub fn open_timeout(
        path: PathBuf,
        revision: String,
        timeout: Duration,
    ) -> Result<(Self, String), String> {
        Self::open_with_timeout(path, revision, Some(timeout))
    }
    fn open_with_timeout(
        path: PathBuf,
        revision: String,
        timeout: Option<Duration>,
    ) -> Result<(Self, String), String> {
        let (tx, rx) = mpsc::sync_channel(1);
        let mut bridge = Self::spawn(move || {
            let loaded = (|| {
                let mut source = Source::open(&path, &revision, "planetarium-pilot")
                    .map_err(|e| e.to_string())?;
                let initial = source.initial_document(512).map_err(|e| e.to_string())?;
                Ok::<_, String>((source, initial))
            })();
            match loaded {
                Ok((mut source, initial)) => {
                    tx.send(Ok(initial)).map_err(|e| e.to_string())?;
                    Ok(
                        Box::new(move |q: &str| source.observe(q).map_err(|e| e.to_string()))
                            as Observer,
                    )
                }
                Err(e) => {
                    let _ = tx.send(Err(e.clone()));
                    Err(e)
                }
            }
        });
        let initial = bridge.receive_initial(rx, timeout)?;
        Ok((bridge, initial))
    }
    fn receive_initial(
        &mut self,
        rx: mpsc::Receiver<Result<String, String>>,
        timeout: Option<Duration>,
    ) -> Result<String, String> {
        let result = match timeout {
            Some(timeout) => rx
                .recv_timeout(timeout)
                .map_err(|e| format!("source initialization timeout/disconnect: {e}")),
            None => rx.recv().map_err(|e| e.to_string()),
        }
        .and_then(|r| r);
        if result.is_err() {
            self.abandon();
        }
        result
    }
    /// Native Rust work cannot be forcibly interrupted. Close publication and
    /// detach only on terminal failure; the CLI then exits the process. A late
    /// worker may finish privately but cannot publish or be reused.
    fn abandon(&mut self) {
        let (lock, wake) = &*self.shared;
        let mut s = lock.lock().unwrap();
        s.closed = true;
        s.pending = None;
        s.completed = None;
        wake.notify_all();
        self.worker.take();
    }
    fn spawn<F>(initialize: F) -> Self
    where
        F: FnOnce() -> Result<Observer, String> + Send + 'static,
    {
        let shared = Arc::new((Mutex::new(Slot::default()), Condvar::new()));
        let worker_shared = shared.clone();
        let worker = thread::spawn(move || {
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                let mut observe = initialize()?;
                let (lock, wake) = &*worker_shared;
                loop {
                    let request = {
                        let mut s = lock.lock().unwrap();
                        while !s.closed && (s.pending.is_none() || s.completed.is_some()) {
                            s = wake.wait(s).unwrap();
                        }
                        if s.closed {
                            return Ok::<(), String>(());
                        }
                        s.active = true;
                        s.pending.take().unwrap()
                    };
                    let start = Instant::now();
                    let reply = observe(&request);
                    let elapsed = start.elapsed().as_micros();
                    let mut s = lock.lock().unwrap();
                    s.active = false;
                    if s.closed {
                        return Ok(());
                    }
                    s.queries += 1;
                    s.last_query_micros = elapsed;
                    s.completed = Some(reply);
                    wake.notify_all();
                }
            }));
            let (lock, wake) = &*worker_shared;
            let mut s = lock.lock().unwrap();
            s.active = false;
            if !s.closed {
                s.closed = true;
                if let Err(e) =
                    result.unwrap_or_else(|_| Err("source worker panicked/disconnected".into()))
                {
                    s.completed = Some(Err(e));
                }
            }
            wake.notify_all();
        });
        Self {
            shared,
            worker: Some(worker),
        }
    }
    pub fn submit(&mut self, request: String) -> Result<(), String> {
        let (lock, wake) = &*self.shared;
        let mut s = lock.lock().map_err(|_| "source worker state poisoned")?;
        if s.closed {
            return Err("source worker disconnected".into());
        }
        if s.pending.replace(request).is_some() {
            s.coalesced += 1;
        }
        wake.notify_all();
        Ok(())
    }
    pub fn poll(&mut self) -> Result<Option<String>, String> {
        let (lock, wake) = &*self.shared;
        let mut s = lock.lock().map_err(|_| "source worker state poisoned")?;
        let reply = s.completed.take();
        wake.notify_all();
        match reply {
            Some(r) => r.map(Some),
            None if s.closed => Err("source worker disconnected".into()),
            None => Ok(None),
        }
    }
    /// Drain earlier interactive work, then wait for this exact request. Exclusive
    /// access prevents interactive submissions from replacing an export frame.
    pub fn observe(&mut self, request: String) -> Result<String, String> {
        self.observe_until(request, None)
    }
    /// One deadline covers both draining earlier work and this exact query.
    pub fn observe_timeout(
        &mut self,
        request: String,
        timeout: Duration,
    ) -> Result<String, String> {
        let deadline = Instant::now() + timeout;
        let result = self.observe_until(request, Some(deadline));
        let result = if Instant::now() >= deadline {
            Err("source observation timeout".into())
        } else {
            result
        };
        if result.is_err() {
            self.abandon();
        }
        result
    }
    fn observe_until(
        &mut self,
        request: String,
        deadline: Option<Instant>,
    ) -> Result<String, String> {
        let (lock, wake) = &*self.shared;
        let mut s = lock.lock().map_err(|_| "source worker state poisoned")?;
        loop {
            if let Some(r) = s.completed.take() {
                wake.notify_all();
                r?;
            }
            if s.closed {
                return Err("source worker disconnected".into());
            }
            if !s.active && s.pending.is_none() {
                break;
            }
            s = wait_until(wake, s, deadline)?;
        }
        s.pending = Some(request);
        wake.notify_all();
        loop {
            if let Some(r) = s.completed.take() {
                wake.notify_all();
                return r;
            }
            if s.closed {
                return Err("source worker disconnected".into());
            }
            s = wait_until(wake, s, deadline)?;
        }
    }
    pub fn diagnostics(&self) -> Diagnostics {
        let s = self.shared.0.lock().unwrap();
        Diagnostics {
            queries: s.queries,
            coalesced: s.coalesced,
            pending: s.pending.is_some(),
            active: s.active,
            last_query_micros: s.last_query_micros,
        }
    }
}
fn wait_until<'a>(
    wake: &Condvar,
    state: std::sync::MutexGuard<'a, Slot>,
    deadline: Option<Instant>,
) -> Result<std::sync::MutexGuard<'a, Slot>, String> {
    match deadline {
        Some(deadline) => {
            let remaining = deadline
                .checked_duration_since(Instant::now())
                .ok_or("source observation timeout")?;
            let (state, result) = wake
                .wait_timeout(state, remaining)
                .map_err(|_| "source worker state poisoned")?;
            if result.timed_out() {
                return Err("source observation timeout".into());
            }
            Ok(state)
        }
        None => wake
            .wait(state)
            .map_err(|_| "source worker state poisoned".into()),
    }
}
type Observer = Box<dyn FnMut(&str) -> Result<String, String> + Send>;
impl Drop for Bridge {
    fn drop(&mut self) {
        let (lock, wake) = &*self.shared;
        {
            let mut s = lock.lock().unwrap();
            s.closed = true;
            wake.notify_all();
        }
        if let Some(w) = self.worker.take() {
            let _ = w.join();
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn capture_timeout_closes_worker_without_waiting_for_blocked_query() {
        let (release_tx, release_rx) = mpsc::channel();
        let (finished_tx, finished_rx) = mpsc::channel();
        let (started_tx, started_rx) = mpsc::channel();
        let mut b = Bridge::spawn(move || {
            Ok(Box::new(move |_| {
                started_tx.send(()).unwrap();
                release_rx.recv().unwrap();
                finished_tx.send(()).unwrap();
                Ok("late".into())
            }))
        });
        b.submit("old interactive work".into()).unwrap();
        started_rx.recv().unwrap();
        let shared = b.shared.clone();
        let result = b.observe_timeout("frame zero".into(), std::time::Duration::from_millis(10));
        assert!(result.unwrap_err().contains("timeout"));
        assert!(b.submit("next frame".into()).is_err());
        drop(b); // Must not join the still-blocked worker.
        release_tx.send(()).unwrap();
        finished_rx
            .recv_timeout(std::time::Duration::from_secs(5))
            .unwrap();
        let (lock, wake) = &*shared;
        let mut state = lock.lock().unwrap();
        while state.active {
            state = wake.wait(state).unwrap();
        }
        assert!(state.closed && state.pending.is_none() && state.completed.is_none());
    }
    #[test]
    fn capture_exact_query_deadline_survives_spurious_wakeups() {
        use std::sync::atomic::{AtomicBool, Ordering};
        let (release_tx, release_rx) = mpsc::channel();
        let (started_tx, started_rx) = mpsc::channel();
        let (result_tx, result_rx) = mpsc::channel();
        let mut b = Bridge::spawn(move || {
            Ok(Box::new(move |_| {
                started_tx.send(()).unwrap();
                release_rx.recv().unwrap();
                Ok("late reply".into())
            }))
        });
        let shared = b.shared.clone();
        let stopped = Arc::new(AtomicBool::new(false));
        let notifier_stop = stopped.clone();
        let notifier = thread::spawn(move || {
            while !notifier_stop.load(Ordering::Relaxed) {
                shared.1.notify_all();
                thread::yield_now();
            }
        });
        let caller = thread::spawn(move || {
            let result = b.observe_timeout("exact frame".into(), Duration::from_millis(100));
            result_tx.send(result).unwrap();
            drop(b); // The active native query has not been released yet.
        });
        let started = started_rx.recv_timeout(Duration::from_secs(5));
        let result = result_rx.recv_timeout(Duration::from_secs(2));
        stopped.store(true, Ordering::Relaxed);
        notifier.join().unwrap();
        // Release even on an assertion failure, so the test owns no orphan job.
        let _ = release_tx.send(());
        caller.join().unwrap();
        assert!(started.is_ok());
        assert!(result.unwrap().unwrap_err().contains("timeout"));
    }
    #[test]
    fn capture_initialization_timeout_does_not_join_blocked_initializer() {
        let (release_tx, release_rx) = mpsc::channel();
        let (tx, rx) = mpsc::sync_channel(1);
        let mut b = Bridge::spawn(move || {
            release_rx.recv().unwrap();
            let _ = tx.send(Ok("initial".into()));
            Ok(Box::new(|_| Ok("late".into())))
        });
        assert!(
            b.receive_initial(rx, Some(std::time::Duration::from_millis(10)))
                .unwrap_err()
                .contains("timeout")
        );
        drop(b);
        release_tx.send(()).unwrap();
    }
    #[test]
    fn scrub_flood_keeps_only_latest_unstarted_request() {
        let (started_tx, started_rx) = mpsc::channel();
        let (release_tx, release_rx) = mpsc::channel();
        let mut b = Bridge::spawn(move || {
            Ok(Box::new(move |q| {
                started_tx.send(q.to_owned()).unwrap();
                release_rx.recv().unwrap();
                Ok(q.to_owned())
            }))
        });
        b.submit("A".into()).unwrap();
        let first = started_rx.recv().unwrap();
        for i in 0..1000 {
            b.submit(i.to_string()).unwrap();
        }
        let queued = b.diagnostics();
        release_tx.send(()).unwrap();
        // Wait on the production condition variable: no scheduling sleeps.
        {
            let (lock, wake) = &*b.shared;
            let mut s = lock.lock().unwrap();
            while s.completed.is_none() {
                s = wake.wait(s).unwrap();
            }
        }
        let first_reply = b.poll().unwrap();
        let second = started_rx.recv().unwrap();
        release_tx.send(()).unwrap();
        {
            let (lock, wake) = &*b.shared;
            let mut s = lock.lock().unwrap();
            while s.completed.is_none() {
                s = wake.wait(s).unwrap();
            }
        }
        let reply = b.poll().unwrap();
        assert_eq!(first, "A");
        assert_eq!(first_reply, Some("A".into()));
        assert_eq!(second, "999");
        assert_eq!(reply, Some("999".into()));
        assert_eq!(b.diagnostics().queries, 2);
        assert_eq!(queued.coalesced, 999);
        assert!(queued.pending);
    }
    #[test]
    fn exact_queries_and_worker_errors_are_returned() {
        let mut b = Bridge::spawn(|| {
            Ok(Box::new(|q| {
                if q == "bad" {
                    Err("native observation failed".into())
                } else {
                    Ok(q.to_owned())
                }
            }))
        });
        for i in 0..20 {
            assert_eq!(
                b.observe_timeout(i.to_string(), Duration::from_secs(5))
                    .unwrap(),
                i.to_string()
            );
        }
        assert_eq!(
            b.observe("bad".into()).unwrap_err(),
            "native observation failed"
        );
        assert_eq!(b.diagnostics().queries, 21);
    }
    #[test]
    fn panic_surfaces_disconnect() {
        let mut b = Bridge::spawn(|| Ok(Box::new(|_| panic!("test worker disconnect"))));
        assert!(b.observe("x".into()).unwrap_err().contains("disconnected"));
    }
    #[test]
    fn app_pending_keeps_committed_ticks_and_reports_errors() {
        use crate::observation::ObservationState;
        use hornvale_bevy_view::{FilmClock, ObservationMirror, PresentationTimeline};
        let mut mirror =
            ObservationMirror::new(include_str!("../../bevy/tests/fixtures/initial.json")).unwrap();
        mirror.request(0).unwrap();
        mirror
            .accept(include_str!("../../bevy/tests/fixtures/reply.json"))
            .unwrap();
        let mut state = ObservationState {
            mirror,
            timeline: PresentationTimeline::new(FilmClock {
                start_ticks: 0,
                end_ticks: 300,
                frames: 300,
            })
            .unwrap(),
            error: None,
        };
        let mut b = Bridge::spawn(|| Ok(Box::new(|_| Err("native failure".into()))));
        state.seek(&mut b, 150).unwrap();
        assert!(state.pending());
        assert_eq!(state.mirror.current_ticks(), Some(0));
        assert_eq!(state.timeline.sample().unwrap().ticks, 150);
        {
            let (lock, wake) = &*b.shared;
            let mut s = lock.lock().unwrap();
            while s.completed.is_none() {
                s = wake.wait(s).unwrap();
            }
        }
        assert_eq!(state.poll(&mut b).unwrap_err(), "native failure");
        assert_eq!(state.error.as_deref(), Some("native failure"));
        assert_eq!(state.mirror.current_ticks(), Some(0));
    }
    #[test]
    fn exact_export_drains_interactive_work_and_keeps_its_own_reply() {
        let (started_tx, started_rx) = mpsc::channel();
        let (release_tx, release_rx) = mpsc::channel();
        let mut b = Bridge::spawn(move || {
            Ok(Box::new(move |q| {
                started_tx.send(q.to_owned()).map_err(|e| e.to_string())?;
                release_rx.recv().map_err(|e| e.to_string())?;
                Ok(q.to_owned())
            }))
        });
        b.submit("A".into()).unwrap();
        let first = started_rx.recv().unwrap();
        b.submit("B".into()).unwrap();
        let export = thread::spawn(move || {
            let reply = b.observe("EXPORT".into());
            (reply, b.diagnostics())
        });
        release_tx.send(()).unwrap();
        let second = started_rx.recv_timeout(std::time::Duration::from_secs(5));
        let _ = release_tx.send(());
        let third = started_rx.recv_timeout(std::time::Duration::from_secs(5));
        let _ = release_tx.send(());
        drop(release_tx);
        let (reply, diagnostics) = export.join().unwrap();
        assert_eq!(
            [first, second.unwrap(), third.unwrap()],
            ["A", "B", "EXPORT"]
        );
        assert_eq!(reply.unwrap(), "EXPORT");
        assert_eq!(diagnostics.queries, 3);
        assert_eq!(diagnostics.coalesced, 0);
    }
}
