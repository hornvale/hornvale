import json
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

ROOT = Path(__file__).resolve().parent
sys.path.insert(0, str(ROOT))

from measure import (  # noqa: E402
    OUTPUT_LIMIT, _bounded_command, cargo_graph, changed_closure,
    invalidation_probes, run_baseline, sha256, summarize_baseline,
    validate_attempt,
)


def graph_fixture():
    return {
        "identity": {"format": "cargo-metadata-v1", "sha256": "a" * 64},
        "repository_root": "/repo",
        "packages": [
            {"id": "protocol", "name": "protocol", "version": "1", "manifest_path": "/repo/packages/protocol/Cargo.toml", "workspace_member": True},
            {"id": "kernel", "name": "kernel", "version": "1", "manifest_path": "/repo/kernel/Cargo.toml", "workspace_member": True},
            {"id": "lab", "name": "hornvale-lab", "version": "1", "manifest_path": "/repo/windows/lab/Cargo.toml", "workspace_member": True},
            {"id": "digest", "name": "digest", "version": "1", "manifest_path": "/repo/tools/digest/Cargo.toml", "workspace_member": True},
            {"id": "serde", "name": "serde", "version": "1", "manifest_path": "/cargo/registry/src/serde/Cargo.toml", "workspace_member": False},
        ],
        "edges": {"protocol": [], "kernel": ["protocol"], "hornvale-lab": ["kernel"], "digest": ["hornvale-lab", "serde"], "serde": []},
        "package_count": 5, "workspace_member_count": 4,
    }


def attempt(kind, *, valid=True, preparation=2.0, build=5.0, test=3.0):
    checkout, target, evidence = "/repo/checkout", "/repo/checkout/target", "/repo/evidence"
    template = ["cargo", "build", "--locked", "--offline", "--manifest-path", "tools/digest/Cargo.toml", "-p", "digest-thing"]
    capture = {
        "workload_id": "digest-thing", "workload_command_template": template, "command": template,
        "cwd": checkout, "exit_code": 0, "deadline_s": 3600, "elapsed_s": 1.0,
        "cleanup": {"complete": True, "error": None},
        "stdout": {"base64": "", "bytes": 0, "sha256": sha256(b"")},
        "stderr": {"base64": "", "bytes": 0, "sha256": sha256(b"")},
        "interrupted": False, "deadline_exceeded": False, "output_limit_exceeded": False,
        "enforcement_method": "sandbox-exec",
        "ownership": {"checkout": checkout, "target": target, "evidence_root": evidence, "evidence_destination": evidence + "/capture.json"},
    }
    record = {
        "schema": "insulator-attempt-v1",
        "source": {"commit": "a" * 40, "tree": "b" * 40, "merge_base": "c" * 40},
        "graph": {"sha256": "d" * 64, "package_count": 5, "workspace_member_count": 4},
        "toolchain": {"rustc": "rustc 1.0", "host_class": "mac"},
        "target": {"path": target, "classification": kind}, "workload_id": "digest-thing",
        "workload_command_template": template, "command": template, "capture": capture,
        "costs": {"preparation_s": preparation, "build_s": build, "test_s": test},
        "outputs": [{"path": "out.bin", "bytes": 0, "sha256": sha256(b"")}], "failure": None,
    }
    validate_attempt(record)
    if not valid:
        record["capture"]["cleanup"]["complete"] = False
    return record


class CargoGraphTests(unittest.TestCase):
    def test_canonicalizes_metadata(self):
        output = {"packages": [{"id": "z", "name": "b", "version": "1", "manifest_path": "/repo/b/Cargo.toml", "dependencies": [{"name": "a"}]}, {"id": "a", "name": "a", "version": "1", "manifest_path": "/repo/a/Cargo.toml", "dependencies": []}], "workspace_members": ["z", "a"], "resolve": {"nodes": [{"id": "z", "dependencies": ["a"]}, {"id": "a", "dependencies": []}]}}
        with tempfile.TemporaryDirectory() as directory:
            manifest = Path(directory) / "Cargo.toml"
            manifest.write_text("[workspace]\n", encoding="utf-8")
            completed = mock.Mock(returncode=0, stdout=json.dumps(output).encode(), stderr=b"")
            with mock.patch("measure._bounded_command", return_value={"returncode": 0, "stdout": completed.stdout, "stderr": b"", "deadline_exceeded": False, "output_limit_exceeded": False, "cleanup_complete": True, "cleanup_error": None, "launch_error": None}):
                result = cargo_graph(manifest, Path(directory) / "target")
        self.assertEqual(result["package_count"], 2)
        self.assertEqual(result["edges"]["b"], ["a"])

    def test_rejects_metadata_output_over_cap(self):
        with tempfile.TemporaryDirectory() as directory:
            manifest = Path(directory) / "Cargo.toml"
            manifest.write_text("[workspace]\n", encoding="utf-8")
            completed = mock.Mock(returncode=0, stdout=b"x" * (OUTPUT_LIMIT + 1), stderr=b"")
            with mock.patch("measure._bounded_command", return_value={"returncode": 0, "stdout": completed.stdout, "stderr": b"", "deadline_exceeded": False, "output_limit_exceeded": False, "cleanup_complete": True, "cleanup_error": None, "launch_error": None}):
                with self.assertRaisesRegex(ValueError, "output limit"):
                    cargo_graph(manifest, Path(directory) / "target")

    def test_rejects_metadata_timeout_with_bounded_evidence(self):
        with tempfile.TemporaryDirectory() as directory:
            manifest = Path(directory) / "Cargo.toml"
            manifest.write_text("[workspace]\n", encoding="utf-8")
            bounded = {"returncode": None, "stdout": b"partial", "stderr": b"", "deadline_exceeded": True, "output_limit_exceeded": False, "cleanup_complete": True, "cleanup_error": None, "launch_error": None}
            with mock.patch("measure._bounded_command", return_value=bounded):
                with self.assertRaisesRegex(ValueError, "deadline"):
                    cargo_graph(manifest, Path(directory) / "target")


class BoundedCommandTests(unittest.TestCase):
    def test_timeout_terminates_and_cleans_process(self):
        result = _bounded_command([sys.executable, "-c", "import time; time.sleep(10)"], Path.cwd(), timeout_s=0.05)
        self.assertTrue(result["deadline_exceeded"])
        self.assertTrue(result["cleanup_complete"])

    def test_output_cap_retains_at_most_limit(self):
        with mock.patch("measure.OUTPUT_LIMIT", 1024):
            result = _bounded_command([sys.executable, "-c", "import sys; sys.stdout.write('x' * 4096)"], Path.cwd(), timeout_s=2)
        self.assertTrue(result["output_limit_exceeded"])
        self.assertLessEqual(len(result["stdout"]), 1024)


class ClosureTests(unittest.TestCase):
    def test_traverses_reverse_dependents_from_package_dependency_edges(self):
        graph = graph_fixture()
        graph["packages"] = [
            {"id": "protocol", "name": "protocol", "manifest_path": "/repo/protocol/Cargo.toml"},
            {"id": "consumer", "name": "consumer", "manifest_path": "/repo/consumer/Cargo.toml"},
            {"id": "publication", "name": "publication", "manifest_path": "/repo/publication/Cargo.toml"},
        ]
        graph["edges"] = {
            "protocol": [],
            "consumer": ["protocol"],
            "publication": ["consumer"],
        }
        result = changed_closure(graph, ["protocol/src/lib.rs"])
        self.assertEqual(result["directly_changed"], ["protocol"])
        self.assertEqual(result["reverse_dependents"], ["consumer", "publication"])

    def test_maps_repository_paths_while_ignoring_registry_packages(self):
        result = changed_closure(graph_fixture(), ["windows/lab/src/lib.rs"])
        self.assertEqual(result["directly_changed"], ["hornvale-lab"])
        self.assertEqual(result["reverse_dependents"], ["digest"])

    def test_unrelated_path_is_empty(self):
        result = changed_closure(graph_fixture(), ["docs/README.md"])
        self.assertEqual(result["full_invalidation"], [])

    def test_includes_path_dependencies_and_uses_most_specific_root(self):
        graph = graph_fixture()
        graph["packages"].extend([
            {"id": "thing", "name": "digest-thing", "version": "1",
             "manifest_path": "/repo/tools/digest/packages/thing/Cargo.toml",
             "workspace_member": False},
            {"id": "observer", "name": "observer", "version": "1",
             "manifest_path": "/repo/tools/digest/packages/Cargo.toml",
             "workspace_member": False},
        ])
        graph["edges"]["digest-thing"] = []
        result = changed_closure(graph, ["tools/digest/packages/thing/src/lib.rs"])
        self.assertEqual(result["directly_changed"], ["digest-thing"])
        self.assertNotIn("observer", result["directly_changed"])

    def test_declares_four_deterministic_invalidation_probes(self):
        probes = invalidation_probes(graph_fixture())
        self.assertEqual(list(probes), ["protocol", "observer", "lab", "unrelated"])
        self.assertEqual(probes["protocol"]["changed_paths"], ["tools/digest/packages/protocol/src/lib.rs"])
        self.assertEqual(probes["observer"]["changed_paths"], ["tools/digest/packages/census-publication/src/main.rs"])
        self.assertEqual(probes["lab"]["changed_paths"], ["windows/lab/src/lib.rs"])
        self.assertEqual(probes["unrelated"]["closure"]["full_invalidation"], [])


class SummaryTests(unittest.TestCase):
    def test_consumes_persisted_costs_separately(self):
        result = summarize_baseline([attempt("cold"), attempt("warm")])
        self.assertEqual(result["costs"]["cold"]["build_s"], [5.0])
        self.assertEqual(result["costs"]["cold"]["preparation_s"], [2.0])
        self.assertEqual(result["costs"]["warm"]["test_s"], [3.0])

    def test_rejects_incomplete_and_invalid_numeric_attempts(self):
        result = summarize_baseline([attempt("cold"), attempt("warm"), attempt("cold", valid=False)])
        self.assertEqual(result["pair_count"], 1)
        self.assertEqual(result["excluded_attempt_count"], 1)
        with self.assertRaisesRegex(ValueError, "paired"):
            summarize_baseline([attempt("cold")])
        for value in (True, float("nan"), float("inf")):
            bad = attempt("cold")
            bad["costs"]["build_s"] = value
            with self.assertRaises(ValueError):
                summarize_baseline([bad, attempt("warm")])

    def test_excludes_malformed_attempt_before_aggregating_graph_counts(self):
        malformed = attempt("cold", valid=False)
        malformed["graph"]["package_count"] = {"would": "raise"}
        result = summarize_baseline([malformed, attempt("cold"), attempt("warm")])
        self.assertEqual(result["excluded_attempt_count"], 1)
        self.assertEqual(result["graph_counts"]["package_count"], [5])


class BaselineOrchestrationTests(unittest.TestCase):
    def _run_with_mocks(self, root, output, cold, fake_capture=None):
        (root / "tools" / "digest").mkdir(parents=True, exist_ok=True)
        fake = attempt("cold")["capture"]
        fake["cwd"] = fake["ownership"]["checkout"] = str(root)
        fake["ownership"]["target"] = str(root / "tools" / "digest" / "target")
        fake["ownership"]["evidence_root"] = str(root / "evidence")
        fake["ownership"]["evidence_destination"] = str(root / "evidence/capture.json")
        def capture_cell(workload_id, _checkout, target, evidence, destination, **_kwargs):
            value = json.loads(json.dumps(fake if fake_capture is None else fake_capture))
            value["workload_id"] = workload_id
            value["cwd"] = value["ownership"]["checkout"] = str(root)
            value["ownership"]["target"] = str(target)
            value["ownership"]["evidence_root"] = str(evidence)
            value["ownership"]["evidence_destination"] = str(destination)
            value["workload_command_template"] = next(item["command"] for item in json.loads((ROOT / "workloads.json").read_text())['workloads'] if item['id'] == workload_id)
            value["command"] = [arg.replace("${CHECKOUT}", str(root)) for arg in value["workload_command_template"]]
            return value
        with mock.patch("measure.cargo_graph", return_value=graph_fixture()), mock.patch("measure.capture", side_effect=capture_cell), mock.patch("measure._output_records", return_value=[{"path": "out.bin", "bytes": 0, "sha256": sha256(b"")}]), mock.patch("measure._source_identity", return_value={"commit": "a" * 40, "tree": "b" * 40, "merge_base": "c" * 40}), mock.patch("measure._toolchain_identity", return_value={"rustc": "rustc 1.0", "host_class": "mac"}):
            return run_baseline(root, output, "mac", cold=cold)

    def test_cold_recreates_only_owned_target_and_warm_preserves_it(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory) / "checkout"
            root.mkdir()
            target = root / "tools" / "digest" / "target"
            target.mkdir(parents=True)
            marker = target / "stale"
            marker.write_text("old", encoding="utf-8")
            self._run_with_mocks(root, Path(directory) / "cold.json", True)
            self.assertFalse(marker.exists())
            marker.write_text("warm", encoding="utf-8")
            self._run_with_mocks(root, Path(directory) / "warm.json", False)
            self.assertEqual(marker.read_text(encoding="utf-8"), "warm")

    def test_retains_incomplete_capture_without_summarizing_it(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory) / "checkout"
            root.mkdir()
            incomplete = attempt("cold")["capture"]
            incomplete["cleanup"]["complete"] = False
            dossier = self._run_with_mocks(root, Path(directory) / "failed.json", True, incomplete)
            self.assertEqual(len(dossier["attempts"]), 0)
            self.assertEqual(len(dossier["raw_attempts"]), 2)
            self.assertTrue(all(item["status"] == "invalid" for item in dossier["raw_attempts"]))

    def test_writes_validated_dossier_from_mocked_measurement_cells(self):
        with tempfile.TemporaryDirectory() as directory:
            root, output = Path(directory) / "checkout", Path(directory) / "baseline.json"
            root.mkdir()
            fake = attempt("cold")["capture"]
            fake["cwd"] = fake["ownership"]["checkout"] = str(root)
            fake["ownership"]["target"] = str(root / "target")
            fake["ownership"]["evidence_root"] = str(root / "evidence")
            fake["ownership"]["evidence_destination"] = str(root / "evidence/capture.json")
            def fake_capture(workload_id, _checkout, target, evidence, destination, **_kwargs):
                value = json.loads(json.dumps(fake))
                value["workload_id"] = workload_id
                value["cwd"] = value["ownership"]["checkout"] = str(root)
                value["ownership"]["target"] = str(target)
                value["ownership"]["evidence_root"] = str(evidence)
                value["ownership"]["evidence_destination"] = str(destination)
                value["workload_command_template"] = next(item["command"] for item in json.loads((ROOT / "workloads.json").read_text())["workloads"] if item["id"] == workload_id)
                value["command"] = [arg.replace("${CHECKOUT}", str(root)) for arg in value["workload_command_template"]]
                return value
            with mock.patch("measure.cargo_graph", return_value=graph_fixture()), mock.patch("measure.capture", side_effect=fake_capture), mock.patch("measure._output_records", return_value=[{"path": "out.bin", "bytes": 0, "sha256": sha256(b"")}]), mock.patch("measure._source_identity", return_value={"commit": "a" * 40, "tree": "b" * 40, "merge_base": "c" * 40}), mock.patch("measure._toolchain_identity", return_value={"rustc": "rustc 1.0", "host_class": "mac"}):
                dossier = run_baseline(root, output, "mac", cold=True)
            self.assertEqual(dossier["schema"], "insulator-baseline-v1")
            self.assertEqual(dossier["classification"], "cold")
            self.assertEqual(len(dossier["attempts"]), 2)
            self.assertEqual(json.loads(output.read_text())["schema"], "insulator-baseline-v1")


if __name__ == "__main__":
    unittest.main()
