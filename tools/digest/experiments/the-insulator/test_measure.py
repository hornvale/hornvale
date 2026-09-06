import base64
import hashlib
import json
from pathlib import Path
import sys
import tempfile
import unittest

from measure import load_workloads, capture, manifest_for_attempt, validate_attempt


ROOT = Path(__file__).parent


def stream(raw=b""):
    encoded = base64.b64encode(raw).decode("ascii")
    return {
        "base64": encoded,
        "bytes": len(raw),
        "sha256": hashlib.sha256(raw).hexdigest(),
    }


def valid_attempt():
    return manifest_for_attempt(
        source={"commit": "a" * 40, "tree": "b" * 40, "merge_base": "c" * 40},
        graph={"sha256": "d" * 64, "package_count": 1},
        toolchain={"rustc": "rustc 1.80.0", "host_class": "mac"},
        target={"path": "/owned/target", "classification": "cold"},
        command=["fixture-command"],
        capture={
            "exit_code": 0,
            "deadline_s": 3600,
            "elapsed_s": 0.1,
            "cleanup": {"complete": True, "error": None},
            "interrupted": False,
            "deadline_exceeded": False,
            "output_limit_exceeded": False,
            "stdout": stream(b"ok\n"),
            "stderr": stream(),
        },
        costs={"preparation_s": 0.2, "build_s": 0.3, "test_s": 0.4},
        outputs=[{"path": "result.json", "bytes": 2, "sha256": "e" * 64}],
    )


class WorkloadTests(unittest.TestCase):
    def test_loads_valid_workload(self):
        workloads = load_workloads(ROOT / "workloads.json")
        self.assertEqual(workloads["schema"], "insulator-workloads-v1")
        self.assertTrue(workloads["workloads"])
        for workload in workloads["workloads"]:
            self.assertIsInstance(workload["command"], list)
            self.assertTrue(workload["expected_outputs"])

    def test_missing_output_declaration_is_rejected(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "workloads.json"
            value = json.loads((ROOT / "workloads.json").read_text())
            value["workloads"][0].pop("expected_outputs")
            path.write_text(json.dumps(value))
            with self.assertRaisesRegex(ValueError, "expected_outputs"):
                load_workloads(path)

    def test_non_list_command_is_rejected(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "workloads.json"
            value = json.loads((ROOT / "workloads.json").read_text())
            value["workloads"][0]["command"] = "cargo build"
            path.write_text(json.dumps(value))
            with self.assertRaisesRegex(ValueError, "command"):
                load_workloads(path)


class AttemptTests(unittest.TestCase):
    def test_valid_attempt_is_accepted(self):
        validate_attempt(valid_attempt())

    def test_incomplete_cleanup_is_rejected(self):
        attempt = valid_attempt()
        attempt["capture"]["cleanup"]["complete"] = False
        with self.assertRaisesRegex(ValueError, "cleanup"):
            validate_attempt(attempt)

    def test_stdout_over_byte_cap_is_rejected(self):
        attempt = valid_attempt()
        attempt["capture"]["stdout"] = stream(b"x" * (16 * 1024 * 1024 + 1))
        with self.assertRaisesRegex(ValueError, "stdout"):
            validate_attempt(attempt)

    def test_nonzero_completed_command_is_valid_observation(self):
        attempt = valid_attempt()
        attempt["capture"]["exit_code"] = 1
        attempt["failure"] = {"reason": "expected registration refusal", "valid_evidence": True}
        validate_attempt(attempt)

    def test_capture_retains_bounded_fixture_attempt(self):
        with tempfile.TemporaryDirectory() as directory:
            destination = Path(directory) / "attempt.json"
            result = capture(
                [sys.executable, "-c", "print('fixture')"],
                Path(directory),
                destination,
            )
            self.assertEqual(result["exit_code"], 0)
            self.assertTrue(destination.exists())
            self.assertLessEqual(result["stdout"]["bytes"], 16 * 1024 * 1024)


if __name__ == "__main__":
    unittest.main()
