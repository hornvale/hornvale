//! One native owner; only serialized Strings cross the observation channels.
use hornvale_visual_source::Source;
use std::{
    path::PathBuf,
    sync::mpsc::{self, Receiver, Sender},
    thread::{self, JoinHandle},
};
pub struct Bridge {
    requests: Option<Sender<String>>,
    replies: Receiver<Result<String, String>>,
    worker: Option<JoinHandle<()>>,
}
impl Bridge {
    pub fn open(path: PathBuf, revision: String) -> Result<(Self, String), String> {
        let (request_tx, request_rx) = mpsc::channel::<String>();
        let (reply_tx, reply_rx) = mpsc::channel();
        let worker = thread::spawn(move || {
            let mut source = match Source::open(&path, &revision, "planetarium-pilot") {
                Ok(s) => s,
                Err(e) => {
                    let _ = reply_tx.send(Err(e.to_string()));
                    return;
                }
            };
            if reply_tx
                .send(source.initial_document(512).map_err(|e| e.to_string()))
                .is_err()
            {
                return;
            }
            for request in request_rx {
                if reply_tx
                    .send(source.observe(&request).map_err(|e| e.to_string()))
                    .is_err()
                {
                    break;
                }
            }
        });
        let initial = reply_rx.recv().map_err(|e| e.to_string())??;
        Ok((
            Self {
                requests: Some(request_tx),
                replies: reply_rx,
                worker: Some(worker),
            },
            initial,
        ))
    }
    pub fn observe(&self, request: String) -> Result<String, String> {
        self.requests
            .as_ref()
            .ok_or("closed source worker")?
            .send(request)
            .map_err(|e| e.to_string())?;
        self.replies.recv().map_err(|e| e.to_string())?
    }
}
impl Drop for Bridge {
    fn drop(&mut self) {
        self.requests.take();
        if let Some(worker) = self.worker.take() {
            let _ = worker.join();
        }
    }
}
