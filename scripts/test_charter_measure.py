import base64
from pathlib import Path
import sys
import unittest
import charter_measure as harness

class RetentionTests(unittest.TestCase):
    def test_success_retains_exact_binary_streams(self):
        sample = harness.measure([sys.executable, "-c", "import sys; sys.stdout.buffer.write(b'\\x00ok'); sys.stderr.write('diagnostic')"], Path.cwd(), retain_output=True)
        self.assertEqual(base64.b64decode(sample.get("stdout_base64", "")), b"\x00ok")
        self.assertEqual(base64.b64decode(sample["stderr_base64"]), b"diagnostic")
        import hashlib
        self.assertEqual(sample["stdout_bytes"], 3)
        self.assertEqual(sample["stderr_bytes"], 10)
        self.assertEqual(sample["stdout_sha256"], hashlib.sha256(b"\x00ok").hexdigest())
        self.assertEqual(sample["stderr_sha256"], hashlib.sha256(b"diagnostic").hexdigest())

    def test_fast_oversize_exit_is_failed_and_retained(self):
        sample = harness.measure([sys.executable, "-c", "print('x' * 20000)"], Path.cwd(), retain_output=True, output_limit_bytes=100)
        self.assertTrue(sample["output_limit_exceeded"])
        self.assertFalse(harness.successful(sample))
        self.assertEqual(len(base64.b64decode(sample["stdout_base64"])), 20001)

    def test_running_oversize_writer_is_stopped(self):
        sample = harness.measure([sys.executable, "-c", "import os,time; os.write(2, b'x' * 20000); time.sleep(30)"], Path.cwd(), retain_output=True, output_limit_bytes=100)
        self.assertTrue(sample["output_limit_exceeded"])
        self.assertTrue(sample["direct_child_waited"])
        self.assertFalse(harness.successful(sample))
        self.assertLess(sample["elapsed_seconds"], 10)

    def test_deadline_is_unsuccessful(self):
        sample = harness.measure([sys.executable, "-c", "import time; time.sleep(30)"], Path.cwd(), deadline_seconds=0.05)
        self.assertTrue(sample["harness_deadline_exceeded"])
        self.assertFalse(harness.successful(sample))

    def test_import_has_no_observable_process_side_effects(self):
        import subprocess
        result = subprocess.run([sys.executable, "-c", "import sys,signal; before=(sys.argv[:],signal.getsignal(signal.SIGINT),signal.getsignal(signal.SIGTERM)); import charter_measure; assert before == (sys.argv,signal.getsignal(signal.SIGINT),signal.getsignal(signal.SIGTERM))"], cwd=Path(__file__).parent, capture_output=True)
        self.assertEqual((result.returncode, result.stdout, result.stderr), (0, b"", b""))

    def test_cli_refuses_either_uncommitted_executed_file(self):
        import shutil
        import subprocess
        import tempfile
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            scripts = root / "scripts"
            scripts.mkdir()
            harness.git(root, "init", "-q")
            harness.git(root, "config", "user.name", "Counterpart fixture")
            harness.git(root, "config", "user.email", "counterpart@example.invalid")
            for name in ("charter-measure.sh", "charter_measure.py"):
                shutil.copy(Path(__file__).parent / name, scripts / name)
            harness.git(root, "add", "scripts")
            harness.git(root, "commit", "-qm", "fixture")
            for name in ("charter-measure.sh", "charter_measure.py"):
                with self.subTest(name=name):
                    path = scripts / name
                    original = path.read_text()
                    try:
                        path.write_text(original + "\n# fixture drift\n")
                        result = subprocess.run(["bash", str(scripts / "charter-measure.sh")], cwd=root, env=harness.controlled_env(), capture_output=True, text=True)
                        self.assertEqual(result.returncode, 1)
                        self.assertIn(name + " differs from HEAD", result.stdout)
                        self.assertNotIn('"kind": "environment"', result.stdout)
                    finally:
                        path.write_text(original)

if __name__ == "__main__":
    unittest.main()
