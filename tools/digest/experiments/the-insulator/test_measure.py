import base64
import hashlib
import json
import platform
import shutil
from pathlib import Path
import sys
import tempfile
import unittest
from unittest import mock

from measure import (EnforcementUnavailable, load_workloads, capture, command_for_workload,
                     manifest_for_attempt, validate_attempt)


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
        target={"path": "/owned/checkout/target", "classification": "cold"},
        command=["fixture-command"],
        capture={
            "exit_code": 0,
            "cwd": "/owned/checkout",
            "deadline_s": 3600,
            "elapsed_s": 0.1,
            "ownership": {
                "checkout": "/owned/checkout",
                "target": "/owned/checkout/target",
                "evidence_root": "/owned/evidence",
                "evidence_destination": "/owned/evidence/attempt.json",
            },
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

    def test_census_workload_uses_the_collect_contract(self):
        workload = load_workloads(ROOT / "workloads.json")["workloads"][0]
        command = command_for_workload(workload, Path("/owned/checkout"))
        self.assertEqual(
            command[-3:], ["collect", "--repo-root", "/owned/checkout"],
        )

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

    def test_malformed_output_entries_are_rejected(self):
        cases = [
            [],
            [{"path": "", "bytes": 0, "sha256": "e" * 64}],
            [{"path": "../result.json", "bytes": 0, "sha256": "e" * 64}],
            [{"path": "result.json", "bytes": -1, "sha256": "e" * 64}],
            [{"path": "result.json", "bytes": True, "sha256": "e" * 64}],
            [{"path": "result.json", "bytes": 0, "sha256": "bad"}],
        ]
        for outputs in cases:
            with self.subTest(outputs=outputs):
                attempt = valid_attempt()
                attempt["outputs"] = outputs
                with self.assertRaisesRegex(ValueError, "outputs"):
                    validate_attempt(attempt)

    def test_malformed_numeric_and_status_fields_are_rejected(self):
        cases = [
            ("graph", "package_count", -1),
            ("graph", "sha256", "short"),
            ("capture", "exit_code", "0"),
            ("capture", "elapsed_s", float("nan")),
            ("capture", "deadline_s", True),
            ("capture", "interrupted", "false"),
            ("capture", "output_limit_exceeded", "false"),
            ("costs", "build_s", float("inf")),
        ]
        for section, key, value in cases:
            with self.subTest(section=section, key=key):
                attempt = valid_attempt()
                attempt[section][key] = value
                with self.assertRaises(ValueError):
                    validate_attempt(attempt)

    def test_unowned_capture_paths_are_rejected(self):
        attempt = valid_attempt()
        attempt["capture"]["ownership"]["target"] = "/shared/target"
        with self.assertRaisesRegex(ValueError, "ownership"):
            validate_attempt(attempt)

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
                owned_checkout=Path(directory),
                owned_target=Path(directory) / "target",
                owned_evidence_root=Path(directory),
            )
            self.assertEqual(result["exit_code"], 0)
            self.assertTrue(destination.exists())
            self.assertLessEqual(result["stdout"]["bytes"], 16 * 1024 * 1024)

    def test_capture_hard_stops_oversized_subprocess_output(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            destination = root / "attempt.json"
            result = capture(
                [sys.executable, "-c", "import sys; sys.stdout.write('x' * (20 * 1024 * 1024))"],
                root,
                destination,
                owned_checkout=root,
                owned_target=root / "target",
                owned_evidence_root=root,
            )
            self.assertTrue(result["output_limit_exceeded"])
            self.assertEqual(result["stdout"]["bytes"], 16 * 1024 * 1024)
            self.assertLessEqual(result["stdout"]["bytes"], 16 * 1024 * 1024)

    def test_capture_blocks_external_write(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory) / "owned"
            root.mkdir()
            external = Path(directory) / "external.txt"
            destination = root / "evidence" / "attempt.json"
            result = capture(
                [sys.executable, "-c", f"open({str(external)!r}, 'w').write('nope')"],
                root,
                destination,
                owned_checkout=root,
                owned_target=root / "target",
                owned_evidence_root=root / "evidence",
            )
            self.assertNotEqual(result["exit_code"], 0)
            self.assertFalse(external.exists())

    def test_capture_refuses_when_enforcement_is_unavailable(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            with mock.patch("measure.platform.system", return_value="Plan9"), self.assertRaisesRegex(
                EnforcementUnavailable, "no supported filesystem sandbox"
            ):
                capture(
                    [sys.executable, "-c", "print('fixture')"],
                    root,
                    root / "evidence" / "attempt.json",
                    owned_checkout=root,
                    owned_target=root / "target",
                    owned_evidence_root=root / "evidence",
                )

    @unittest.skipUnless(platform.system() == "Linux", "Linux-only enforcement prerequisite")
    def test_linux_capture_refuses_without_bwrap(self):
        if shutil.which("bwrap") is not None:
            self.skipTest("bwrap is available")
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            with self.assertRaisesRegex(EnforcementUnavailable, "bubblewrap"):
                capture(
                    [sys.executable, "-c", "print('fixture')"],
                    root,
                    root / "evidence" / "attempt.json",
                    owned_checkout=root,
                    owned_target=root / "target",
                    owned_evidence_root=root / "evidence",
                )

    def test_capture_rejects_unowned_paths(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            with self.assertRaisesRegex(ValueError, "owned"):
                capture(
                    [sys.executable, "-c", "print('fixture')"],
                    root,
                    root / "attempt.json",
                    owned_checkout=root / "other-checkout",
                    owned_target=root / "target",
                    owned_evidence_root=root,
                )

    def test_capture_rejects_unowned_evidence_destination(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            with self.assertRaisesRegex(ValueError, "evidence"):
                capture(
                    [sys.executable, "-c", "print('fixture')"],
                    root,
                    root / "attempt.json",
                    owned_checkout=root,
                    owned_target=root / "target",
                    owned_evidence_root=root / "evidence",
                )


if __name__ == "__main__":
    unittest.main()
