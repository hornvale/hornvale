#!/usr/bin/env bash
# Explicit Charter diagnostic; never a routine gate or a census runner.
set -euo pipefail
exec python3 - "${BASH_SOURCE[0]}" "$@" <<'PY'
import json
import os
from pathlib import Path
import platform
import re
import signal
import subprocess
import sys
import tempfile
import time
import unittest

script_path = Path(sys.argv.pop(1)).resolve()


GIT_PATH_VARS = (
    "GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE",
    "GIT_OBJECT_DIRECTORY", "GIT_ALTERNATE_OBJECT_DIRECTORIES",
)
CARGO_PATH_VARS = (
    "CARGO_TARGET_DIR", "CARGO_BUILD_TARGET_DIR", "CARGO_BUILD_TARGET",
    "CARGO_ENCODED_RUSTFLAGS", "RUSTFLAGS", "RUSTC_WRAPPER",
    "RUSTC_WORKSPACE_WRAPPER",
)
interrupted = False
# A cleanup failure retains the owned directory for inspection instead of
# removing a directory that an owned process may still be using.
unsafe_cleanup = False


def request_stop(_signum, _frame):
    global interrupted
    interrupted = True


def controlled_env():
    env = os.environ.copy()
    for key in GIT_PATH_VARS + CARGO_PATH_VARS:
        env.pop(key, None)
    env["GIT_OPTIONAL_LOCKS"] = "0"
    return env


def git(root, *args):
    # Scrubbing is required even with -C: hook-inherited GIT_DIR outranks cwd.
    completed = subprocess.run(
        ["git", "-C", str(root), *args], env=controlled_env(),
        stdin=subprocess.DEVNULL, capture_output=True, text=True,
    )
    if completed.returncode:
        raise RuntimeError(f"git {args}: {completed.stderr.strip()}")
    return completed.stdout


class OwnedWorktree:
    def __init__(self, root, revision):
        self.root = root
        self.revision = revision
        self.directory = None

    def __enter__(self):
        self.directory = Path(tempfile.mkdtemp(prefix="charter-measure-"))
        self.checkout = self.directory / "checkout"
        try:
            # No sparse worktree: preserve the committed toolchain pin and all
            # contributor input files. The target directory starts absent.
            git(self.root, "worktree", "add", "--detach", str(self.checkout), self.revision)
        except BaseException:
            if self.checkout.exists():
                git(self.root, "worktree", "remove", "--force", str(self.checkout))
            self.directory.rmdir()
            raise
        return self.checkout

    def __exit__(self, _kind, _value, _traceback):
        if unsafe_cleanup:
            raise RuntimeError(f"owned process cleanup failed; retained {self.checkout}")
        git(self.root, "worktree", "remove", "--force", str(self.checkout))
        self.directory.rmdir()


def process_is_running(pid):
    result = subprocess.run(["ps", "-p", str(pid), "-o", "stat="], capture_output=True, text=True)
    return bool(result.stdout.strip()) and not result.stdout.strip().startswith("Z")


def running_group(pgid):
    result = subprocess.run(["ps", "-axo", "pid=,pgid=,stat="], capture_output=True, text=True, check=True)
    return [int(pid) for line in result.stdout.splitlines()
            for pid, group, state in [line.split()]
            if int(group) == pgid and not state.startswith("Z")]


def finish_process(process):
    global unsafe_cleanup
    # Each command starts a fresh session. Only this command's owned group is
    # signalled. Give Digest time to finish its own 250 ms contributor cleanup.
    if running_group(process.pid):
        try:
            os.killpg(process.pid, signal.SIGTERM)
        except ProcessLookupError:
            pass
        deadline = time.monotonic() + 1.0
        while running_group(process.pid) and time.monotonic() < deadline:
            time.sleep(0.02)
        if running_group(process.pid):
            try:
                os.killpg(process.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
    process.wait()
    deadline = time.monotonic() + 2.0
    while running_group(process.pid) and time.monotonic() < deadline:
        time.sleep(0.02)
    if running_group(process.pid):
        unsafe_cleanup = True
        raise RuntimeError(f"owned process group {process.pid} remains running")


def measure(command, cwd):
    import hashlib
    with tempfile.TemporaryDirectory(prefix="charter-sample-") as directory:
        directory = Path(directory)
        metrics = directory / "time.txt"
        timed = (["/usr/bin/time", "-l", "-o", str(metrics)] if platform.system() == "Darwin"
                 else ["/usr/bin/time", "-v", "-o", str(metrics)]) + command
        before = time.monotonic()
        started = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())
        load = os.getloadavg()
        timeout = False
        launch_error = None
        exit_code = None
        with (directory / "stdout").open("wb") as stdout, (directory / "stderr").open("wb") as stderr:
            process = None
            try:
                process = subprocess.Popen(timed, cwd=cwd, env=controlled_env(),
                                           stdin=subprocess.DEVNULL, stdout=stdout, stderr=stderr,
                                           start_new_session=True)
                while process.poll() is None:
                    timeout = time.monotonic() - before > 3600
                    if interrupted or timeout:
                        break
                    time.sleep(0.01)
                # Elapsed excludes harness cleanup/ps overhead on normal exit.
                elapsed = time.monotonic() - before
            except OSError as error:
                elapsed = time.monotonic() - before
                launch_error = str(error)
            finally:
                if process is not None:
                    try:
                        finish_process(process)
                    except BaseException:
                        global unsafe_cleanup
                        unsafe_cleanup = True
                        raise
                    exit_code = process.returncode
        raw = metrics.read_text(errors="replace") if metrics.exists() else ""
        if platform.system() == "Darwin":
            rss = re.search(r"(\d+)\s+maximum resident set size", raw)
            unit, factor = "bytes", 1
        else:
            rss = re.search(r"Maximum resident set size \(kbytes\):\s*(\d+)", raw)
            unit, factor = "KiB", 1024
        output = (directory / "stdout").read_bytes()
        diagnostics = (directory / "stderr").read_text(errors="replace")
        return {
            "command": command, "cwd": str(cwd), "started_utc": started,
            "load_average": load, "elapsed_seconds": elapsed,
            "exit_code": exit_code, "interrupted": interrupted,
            "harness_deadline_exceeded": timeout, "launch_error": launch_error,
            "peak_rss_raw": int(rss.group(1)) if rss else None, "peak_rss_unit": unit,
            "peak_rss_bytes": int(rss.group(1)) * factor if rss else None,
            "time_output": raw, "stderr": diagnostics,
            "context_phases": re.findall(r"^context timing: (.+)$", diagnostics, re.MULTILINE),
            "stdout_bytes": len(output), "stdout_sha256": hashlib.sha256(output).hexdigest(),
            "failure_stdout": output.decode(errors="replace") if exit_code != 0 else None,
        }


def emit(kind, **fields):
    print("charter-measure " + json.dumps({"kind": kind, **fields}, sort_keys=True), flush=True)


def successful(sample):
    return sample["exit_code"] == 0 and not sample["interrupted"] and not sample["harness_deadline_exceeded"]


def main():
    if sys.argv[1:] == ["--help"]:
        print("Usage: bash scripts/charter-measure.sh [--self-test|--help]\n"
              "Measures committed HEAD: fresh host build, selected cold request, ten warm requests\n"
              "for Thing, census publication guards, and combined context. Prints JSON lines.\n"
              "Run canonical measurements only through the ordinary serialized stage queue.")
        return 0
    if sys.argv[1:]:
        raise RuntimeError("unknown arguments; use --help")
    if platform.system() not in ("Darwin", "Linux"):
        raise RuntimeError("diagnostic supports macOS and Linux only")
    root = Path(git(Path.cwd(), "rev-parse", "--show-toplevel").strip())
    revision = git(root, "rev-parse", "HEAD").strip()
    if script_path.read_text() != git(root, "show", "HEAD:scripts/charter-measure.sh"):
        raise RuntimeError("commit the diagnostic script before sampling; executed script differs from HEAD")
    emit("environment", source=revision, root=str(root), hostname=platform.node(),
         platform=platform.platform(), python=platform.python_version(), cpus=os.cpu_count(),
         caller_tracked_status=git(root, "status", "--porcelain", "--untracked-files=no"),
         cold_definition="fresh owned tools/digest/target; dependency and OS caches may be warm",
         attribution_limit="whole phases only; compilation versus lock waiting unavailable; RSS is time's available maximum, not aggregate concurrent process-tree memory",
         removed_environment_keys=list(GIT_PATH_VARS + CARGO_PATH_VARS),
         harness_deadline_seconds=3600)
    failures = 0
    for workload, scope in (("thing", "domains/thing"), ("census", "windows/lab/src/publish.rs"), ("combined", ".")):
        if interrupted:
            return 130
        with OwnedWorktree(root, revision) as checkout:
            if interrupted:
                return 130
            target = checkout / "tools/digest/target"
            if target.exists():
                raise RuntimeError(f"cold target unexpectedly exists: {target}")
            emit("workload", workload=workload, scope=scope, revision=git(checkout, "rev-parse", "HEAD").strip(),
                 toolchain_pin=(checkout / "rust-toolchain.toml").read_text(),
                 rustc=subprocess.check_output(["rustc", "--version", "--verbose"], cwd=checkout, env=controlled_env(), text=True),
                 cargo=subprocess.check_output(["cargo", "--version"], cwd=checkout, env=controlled_env(), text=True))
            command = ["cargo", "build", "--manifest-path", str(checkout / "tools/digest/Cargo.toml"),
                       "--package", "digest", "--bin", "digest", "--target-dir", str(target), "--locked", "--offline"]
            sample = measure(command, checkout)
            emit("sample", workload=workload, phase="cold-host-build", sample=0, **sample)
            failures += not successful(sample)
            if interrupted:
                return 130
            if successful(sample):
                prepared = False
                for index in range(11):
                    sample = measure([str(target / "debug/digest"), "context", scope], checkout)
                    emit("sample", workload=workload, phase="cold-selected-request" if index == 0 else "warm-request",
                         sample=index, prepared_before_request=prepared, **sample)
                    prepared = prepared or successful(sample)
                    failures += not successful(sample)
                    if interrupted:
                        return 130
            else:
                emit("unavailable", workload=workload, reason="host build failed; cold request and ten warm requests unavailable")
            status = git(checkout, "status", "--porcelain", "--untracked-files=no")
            emit("tracked-integrity", workload=workload, status=status, unchanged=not status)
            failures += bool(status)
    emit("complete", source=revision, failures=failures)
    return 1 if failures else 0


def self_test():
    class HarnessTests(unittest.TestCase):
        def test_git_paths_cannot_redirect_owned_worktree(self):
            self.assertIn("git", globals(), "Git ownership implementation is missing")
            with tempfile.TemporaryDirectory(prefix="charter-test-") as directory:
                root = Path(directory)
                repo = root / "repo"
                repo.mkdir()
                git(repo, "init", "-q")
                git(repo, "config", "user.name", "Charter fixture")
                git(repo, "config", "user.email", "charter@example.invalid")
                (repo / "rust-toolchain.toml").write_text("fixture toolchain pin\n")
                git(repo, "add", "rust-toolchain.toml")
                git(repo, "commit", "-qm", "fixture")
                old = dict(os.environ)
                try:
                    for key in GIT_PATH_VARS:
                        os.environ[key] = str(root / "not-a-repository")
                    revision = git(repo, "rev-parse", "HEAD").strip()
                    with OwnedWorktree(repo, revision) as checkout:
                        self.assertEqual(git(checkout, "rev-parse", "HEAD").strip(), revision)
                        self.assertEqual((checkout / "rust-toolchain.toml").read_text(), "fixture toolchain pin\n")
                        self.assertFalse((checkout / "tools/digest/target").exists())
                    self.assertFalse(checkout.exists())
                    self.assertEqual(git(repo, "status", "--porcelain"), "")
                finally:
                    os.environ.clear()
                    os.environ.update(old)

        def test_failure_retains_status_and_diagnostics(self):
            self.assertIn("measure", globals(), "Measurement implementation is missing")
            result = measure([sys.executable, "-c", "import sys; print('failure details', file=sys.stderr); sys.exit(7)"], Path.cwd())
            self.assertEqual(result["exit_code"], 7)
            self.assertIn("failure details", result["stderr"])
            self.assertIsNotNone(result["peak_rss_bytes"])

        def test_interrupt_stops_owned_process(self):
            self.assertIn("measure", globals(), "Process cleanup implementation is missing")
            # SIGALRM exercises the same handler as SIGINT/SIGTERM only after
            # readiness. The TERM-resistant child also exercises forced cleanup.
            global interrupted
            with tempfile.TemporaryDirectory(prefix="charter-lifetime-") as directory:
                root = Path(directory)
                ready = root / "ready"
                child = ("import os,time,signal; from pathlib import Path; "
                         "signal.signal(signal.SIGTERM, signal.SIG_IGN); "
                         "Path('ready').write_text(str(os.getpid())); "
                         f"os.kill({os.getpid()}, signal.SIGALRM); time.sleep(30)")
                old = signal.signal(signal.SIGALRM, request_stop)
                try:
                    result = measure([sys.executable, "-c", child], root)
                    self.assertTrue(ready.exists(), "child never reached readiness")
                    self.assertTrue(result["interrupted"])
                    self.assertFalse(process_is_running(int(ready.read_text())))
                finally:
                    signal.signal(signal.SIGALRM, old)
                    interrupted = False

        def test_parent_exit_finishes_descendant_before_owned_worktree_removal(self):
            self.assertIn("OwnedWorktree", globals())
            with tempfile.TemporaryDirectory(prefix="charter-descendant-") as directory:
                repo = Path(directory)
                git(repo, "init", "-q")
                git(repo, "config", "user.name", "Charter fixture")
                git(repo, "config", "user.email", "charter@example.invalid")
                (repo / "pin").write_text("keep source")
                git(repo, "add", "pin")
                git(repo, "commit", "-qm", "fixture")
                with OwnedWorktree(repo, git(repo, "rev-parse", "HEAD").strip()) as checkout:
                    child = "import os,time; from pathlib import Path; Path('ready').write_text(str(os.getpid())); time.sleep(30)"
                    parent = ("import subprocess,sys,time; from pathlib import Path; "
                              f"subprocess.Popen([sys.executable, '-c', {child!r}]); "
                              "exec(\"while not Path('ready').exists(): time.sleep(0.01)\")")
                    result = measure([sys.executable, "-c", parent], checkout)
                    self.assertEqual(result["exit_code"], 0)
                    pid = int((checkout / "ready").read_text())
                    self.assertFalse(process_is_running(pid))
                    self.assertTrue(checkout.exists(), "checkout removed before process inspection")
                self.assertFalse(checkout.exists())

    suite = unittest.defaultTestLoader.loadTestsFromTestCase(HarnessTests)
    return 0 if unittest.TextTestRunner(verbosity=2).run(suite).wasSuccessful() else 1


if __name__ == "__main__":
    if sys.argv[1:] == ["--self-test"]:
        sys.exit(self_test())
    signal.signal(signal.SIGINT, request_stop)
    signal.signal(signal.SIGTERM, request_stop)
    try:
        sys.exit(main())
    except Exception as error:
        emit("failure", error=str(error), interrupted=interrupted)
        sys.exit(130 if interrupted else 1)
PY
