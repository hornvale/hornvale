//! Bounded ownership of development-tool subprocesses.
use std::{
    process::{Command, ExitStatus},
    sync::{Arc, atomic::AtomicBool},
    time::Duration,
};

#[derive(Clone, Copy)]
pub(super) struct Limits {
    pub stdout: usize,
    pub stderr: usize,
    pub timeout: Duration,
    pub grace: Duration,
}
#[derive(Debug)]
pub(super) struct Output {
    pub stdout: Vec<u8>,
    pub stderr: Vec<u8>,
    pub status: ExitStatus,
    pub elapsed: Duration,
}
#[cfg(not(unix))]
pub(super) fn run(_: &mut Command, _: Limits, _: Arc<AtomicBool>) -> Result<Output, String> {
    Err("context process ownership is supported only on Unix".into())
}

#[cfg(unix)]
// Clocks bound development subprocesses; they never enter simulation or reports.
#[allow(clippy::disallowed_types)]
pub(super) fn run(
    command: &mut Command,
    limits: Limits,
    interrupted: Arc<AtomicBool>,
) -> Result<Output, String> {
    use nix::{
        sys::signal::{Signal, killpg},
        unistd::Pid,
    };
    use std::{
        os::unix::process::CommandExt, process::Stdio, sync::atomic::Ordering, thread,
        time::Instant,
    };
    if interrupted.load(Ordering::SeqCst) {
        return Err("interrupted before spawn".into());
    }
    let start = Instant::now();
    let mut child = command
        .process_group(0)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| format!("spawn: {e}"))?;
    let group = Pid::from_raw(child.id() as i32);
    let fault = Arc::new(std::sync::Mutex::new(None));
    let stdout = reader(
        child.stdout.take().expect("piped stdout"),
        limits.stdout,
        "stdout",
        fault.clone(),
    );
    let stderr = reader(
        child.stderr.take().expect("piped stderr"),
        limits.stderr,
        "stderr",
        fault.clone(),
    );
    let mut status = None;
    let mut exited_at = None;
    let mut error = None;
    loop {
        if interrupted.load(Ordering::SeqCst) {
            error = Some("interrupted".into());
        }
        if start.elapsed() >= limits.timeout {
            error = Some("timeout".into());
        }
        if let Some(reason) = fault.lock().expect("reader fault lock").clone() {
            error = Some(reason);
        }
        if error.is_some() {
            break;
        }
        if status.is_none() {
            match child.try_wait() {
                Ok(Some(value)) => {
                    status = Some(value);
                    exited_at = Some(Instant::now());
                }
                Ok(None) => {}
                Err(e) => {
                    error = Some(format!("wait: {e}"));
                    break;
                }
            }
        }
        if status.is_some() && stdout.is_finished() && stderr.is_finished() {
            break;
        }
        if exited_at.is_some_and(|at| at.elapsed() >= limits.grace) {
            error = Some("exited parent retained pipes through a descendant".into());
            break;
        }
        thread::sleep(Duration::from_millis(2));
    }
    // Always clean the owned group, including descendants of a successful parent.
    // Children are trusted to remain in this group; this is not a sandbox.
    if killpg(group, Signal::SIGTERM).is_ok() {
        let cleanup = Instant::now();
        while cleanup.elapsed() < limits.grace {
            if killpg(group, None).is_err() {
                break;
            }
            thread::sleep(Duration::from_millis(2));
        }
        if let Err(e) = killpg(group, Signal::SIGKILL)
            && e != nix::errno::Errno::ESRCH
        {
            error.get_or_insert_with(|| format!("group cleanup: {e}"));
        }
    }
    let waited = child.wait().map_err(|e| format!("reap: {e}"));
    let out = stdout
        .join()
        .map_err(|_| "stdout reader panicked".to_owned());
    let err = stderr
        .join()
        .map_err(|_| "stderr reader panicked".to_owned());
    if let Some(reason) = fault.lock().expect("reader fault lock").clone() {
        error.get_or_insert(reason);
    }
    let stdout = out?;
    let stderr = err?;
    let status = waited?;
    if let Some(reason) = error {
        return Err(format!(
            "{reason}; stderr: {}",
            String::from_utf8_lossy(&stderr)
        ));
    }
    Ok(Output {
        stdout,
        stderr,
        status,
        elapsed: start.elapsed(),
    })
}

#[cfg(unix)]
fn reader(
    mut pipe: impl std::io::Read + Send + 'static,
    limit: usize,
    name: &'static str,
    fault: Arc<std::sync::Mutex<Option<String>>>,
) -> std::thread::JoinHandle<Vec<u8>> {
    std::thread::spawn(move || {
        let mut captured = Vec::new();
        let mut chunk = [0; 8192];
        loop {
            match pipe.read(&mut chunk) {
                Ok(0) => break,
                Ok(n) => {
                    let remaining = limit - captured.len();
                    captured.extend_from_slice(&chunk[..n.min(remaining)]);
                    if n > remaining {
                        fault
                            .lock()
                            .expect("reader fault lock")
                            .get_or_insert_with(|| {
                                format!("{name} output limit ({limit} bytes) exceeded")
                            });
                    }
                }
                Err(e) if e.kind() == std::io::ErrorKind::Interrupted => continue,
                Err(e) => {
                    fault
                        .lock()
                        .expect("reader fault lock")
                        .get_or_insert_with(|| format!("{name} read: {e}"));
                    break;
                }
            }
        }
        captured
    })
}

#[cfg(all(test, unix))]
mod tests {
    use super::*;
    use std::{
        fs,
        sync::atomic::{AtomicU64, Ordering},
        thread,
    };
    static NEXT: AtomicU64 = AtomicU64::new(0);
    struct Fixture(std::path::PathBuf);
    impl Fixture {
        fn new() -> Self {
            let path = std::env::temp_dir().join(format!(
                "digest-process-{}-{}",
                std::process::id(),
                NEXT.fetch_add(1, Ordering::Relaxed)
            ));
            fs::create_dir(&path).unwrap();
            Self(path)
        }
        fn ready(&self) {
            for _ in 0..500 {
                if self.0.join("ready").exists() {
                    return;
                }
                thread::sleep(Duration::from_millis(2));
            }
            panic!("fixture failed to become ready");
        }
        fn assert_stopped(&self) {
            for name in ["parent", "grandchild"] {
                let pid = fs::read_to_string(self.0.join(name)).unwrap();
                let output = Command::new("ps")
                    .args(["-o", "stat=", "-p", pid.trim()])
                    .output()
                    .unwrap();
                let state = String::from_utf8_lossy(&output.stdout);
                assert!(
                    state.trim().is_empty() || state.trim().starts_with('Z'),
                    "{name} still running: {state}"
                );
            }
        }
    }
    impl Drop for Fixture {
        fn drop(&mut self) {
            fs::remove_dir_all(&self.0).unwrap();
        }
    }
    fn limits() -> Limits {
        Limits {
            stdout: 1024,
            stderr: 128,
            timeout: Duration::from_secs(2),
            grace: Duration::from_millis(25),
        }
    }
    fn shell(script: &str) -> Command {
        let mut cmd = Command::new("sh");
        cmd.args(["-c", script]);
        cmd
    }
    #[test]
    fn captures_both_streams_and_nonzero_status() {
        let output = run(
            &mut shell("printf hello; printf diagnosis >&2; exit 7"),
            limits(),
            Arc::new(AtomicBool::new(false)),
        )
        .unwrap();
        assert_eq!(output.stdout, b"hello");
        assert_eq!(output.stderr, b"diagnosis");
        assert_eq!(output.status.code(), Some(7));
    }
    #[test]
    fn normal_child_is_waited_and_successful() {
        assert!(
            run(
                &mut shell("printf ok"),
                limits(),
                Arc::new(AtomicBool::new(false))
            )
            .unwrap()
            .status
            .success()
        );
    }
    #[test]
    fn overflow_in_either_stream_fails() {
        for stream in ["", " >&2"] {
            let err = run(
                &mut shell(&format!(
                    "while :; do printf 01234567890123456789{stream}; done"
                )),
                limits(),
                Arc::new(AtomicBool::new(false)),
            )
            .unwrap_err();
            assert!(err.contains("limit"), "{err}");
        }
    }
    fn lifecycle(event: &str) {
        let fixture = Fixture::new();
        let interrupted = Arc::new(AtomicBool::new(false));
        let mut cmd = shell(
            "echo $$ > parent; sh -c 'echo $$ > grandchild; touch ready; while :; do sleep 1; done' & while [ ! -f release ]; do sleep 0.01; done; if [ -f exit ]; then exit 0; fi; wait",
        );
        cmd.current_dir(&fixture.0);
        let mut cap = limits();
        if event == "timeout" {
            cap.timeout = Duration::from_millis(300);
        }
        let flag = interrupted.clone();
        let task = thread::spawn(move || run(&mut cmd, cap, flag));
        fixture.ready();
        match event {
            "interrupted" => interrupted.store(true, Ordering::SeqCst),
            "retained" => {
                fs::write(fixture.0.join("exit"), "").unwrap();
                fs::write(fixture.0.join("release"), "").unwrap();
            }
            _ => {}
        }
        let err = task.join().unwrap().unwrap_err();
        assert!(err.contains(event), "{err}");
        fixture.assert_stopped();
        let pid: i32 = fs::read_to_string(fixture.0.join("parent"))
            .unwrap()
            .trim()
            .parse()
            .unwrap();
        assert_eq!(
            nix::sys::wait::waitpid(
                nix::unistd::Pid::from_raw(pid),
                Some(nix::sys::wait::WaitPidFlag::WNOHANG)
            ),
            Err(nix::errno::Errno::ECHILD)
        );
    }
    #[test]
    fn timeout_terminates_tree_and_joins_readers() {
        lifecycle("timeout");
    }
    #[test]
    fn interruption_terminates_tree_and_joins_readers() {
        lifecycle("interrupted");
    }
    #[test]
    fn exited_parent_cannot_leave_inherited_pipes() {
        lifecycle("retained");
    }
}
