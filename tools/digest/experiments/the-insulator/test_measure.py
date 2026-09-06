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

import measure
from measure import (EnforcementUnavailable, load_workloads, capture, command_for_workload,
                     manifest_for_attempt, validate_attempt)


ROOT = Path(__file__).parent


def capture_fixture(command, root, destination, *, target=None, evidence_root=None):
    target = target or root / "target"
    evidence_root = evidence_root or root / "evidence"
    workload = {
        "id": "fixture",
        "command": command,
        "expected_outputs": [{"path": "result.json", "compare": "bytes"}],
    }
    with mock.patch.object(measure, "load_workloads",
                           return_value={"workloads": [workload]}):
        return capture("fixture", root, target, evidence_root, destination)


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
            root = Path(directory)
            destination = root / "evidence" / "attempt.json"
            result = capture_fixture(
                [sys.executable, "-c", "print('fixture')"],
                root,
                destination,
            )
            self.assertEqual(result["exit_code"], 0)
            self.assertTrue(destination.exists())
            self.assertLessEqual(result["stdout"]["bytes"], 16 * 1024 * 1024)

    def test_capture_hard_stops_oversized_subprocess_output(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            destination = root / "evidence" / "attempt.json"
            result = capture_fixture(
                [sys.executable, "-c", "import sys; sys.stdout.write('x' * (20 * 1024 * 1024))"],
                root,
                destination,
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
            result = capture_fixture(
                [sys.executable, "-c", f"open({str(external)!r}, 'w').write('nope')"],
                root,
                destination,
            )
            self.assertNotEqual(result["exit_code"], 0)
            self.assertFalse(external.exists())

    def test_capture_allows_target_and_evidence_writes_but_denies_checkout(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            checkout_marker = root / "source.txt"
            target = root / "target"
            evidence = root / "evidence"
            destination = evidence / "attempt.json"
            script = (
                "from pathlib import Path; "
                f"Path({str(target / 'built.txt')!r}).write_text('target'); "
                f"Path({str(evidence / 'observed.txt')!r}).write_text('evidence'); "
                f"Path({str(checkout_marker)!r}).write_text('checkout')"
            )
            result = capture_fixture([sys.executable, "-c", script], root, destination)
            self.assertNotEqual(result["exit_code"], 0)
            self.assertTrue((target / "built.txt").exists())
            self.assertTrue((evidence / "observed.txt").exists())
            self.assertFalse(checkout_marker.exists())

    def test_capture_rejects_unknown_workload_before_launch(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            with mock.patch.object(measure.subprocess, "Popen") as launch:
                with self.assertRaisesRegex(ValueError, "unknown workload id"):
                    capture("not-frozen", root, root / "target", root / "evidence",
                            root / "evidence" / "attempt.json")
                launch.assert_not_called()

    def test_capture_refuses_when_enforcement_is_unavailable(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            with mock.patch("measure.platform.system", return_value="Plan9"), self.assertRaisesRegex(
                EnforcementUnavailable, "no supported filesystem sandbox"
            ):
                with mock.patch.object(measure, "load_workloads", return_value={"workloads": [{
                    "id": "fixture", "command": [sys.executable, "-c", "print('fixture')"],
                    "expected_outputs": [{"path": "result.json", "compare": "bytes"}],
                }]}):
                    capture(
                        "fixture", root, root / "target", root / "evidence",
                        root / "evidence" / "attempt.json",
                )

    @unittest.skipUnless(platform.system() == "Linux", "Linux-only enforcement prerequisite")
    def test_linux_capture_refuses_without_bwrap(self):
        if shutil.which("bwrap") is not None:
            self.skipTest("bwrap is available")
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            with self.assertRaisesRegex(EnforcementUnavailable, "bubblewrap"):
                capture_fixture(
                    [sys.executable, "-c", "print('fixture')"], root,
                    root / "evidence" / "attempt.json",
                )

    def test_capture_rejects_unowned_paths(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            checkout = root / "checkout"
            checkout.mkdir()
            with self.assertRaisesRegex(ValueError, "owned"):
                capture_fixture([sys.executable, "-c", "print('fixture')"], checkout,
                                root / "attempt.json", target=root / "target")

    def test_capture_rejects_target_equal_to_checkout(self):
        with tempfile.TemporaryDirectory() as directory:
            checkout = Path(directory) / "checkout"
            checkout.mkdir()
            with self.assertRaisesRegex(ValueError, "target must not overlap"):
                capture_fixture(
                    [sys.executable, "-c", "print('fixture')"], checkout,
                    Path(directory) / "attempt.json", target=checkout,
                )

    def test_capture_rejects_evidence_root_equal_to_checkout(self):
        with tempfile.TemporaryDirectory() as directory:
            checkout = Path(directory) / "checkout"
            checkout.mkdir()
            with self.assertRaisesRegex(ValueError, "evidence root must not overlap"):
                capture_fixture(
                    [sys.executable, "-c", "print('fixture')"], checkout,
                    checkout / "attempt.json", evidence_root=checkout,
                )

    def test_capture_rejects_evidence_root_ancestor_of_checkout(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            checkout = root / "checkout"
            checkout.mkdir()
            with self.assertRaisesRegex(ValueError, "evidence root must not overlap"):
                capture_fixture(
                    [sys.executable, "-c", "print('fixture')"], checkout,
                    root / "attempt.json", evidence_root=root,
                )

    def test_capture_rejects_unowned_evidence_destination(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            with self.assertRaisesRegex(ValueError, "evidence"):
                capture_fixture(
                    [sys.executable, "-c", "print('fixture')"], root,
                    root / "attempt.json",
                )


if __name__ == "__main__":
    unittest.main()
