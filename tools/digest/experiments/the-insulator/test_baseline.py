import json
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock


ROOT = Path(__file__).resolve().parent
sys.path.insert(0, str(ROOT))

from measure import cargo_graph, changed_closure, summarize_baseline


def graph_fixture():
    return {
        "identity": {"format": "cargo-metadata-v1", "resolve_nodes": "graph"},
        "packages": [
            {"id": "path+file:///repo#protocol@1.0.0", "name": "protocol", "version": "1.0.0", "manifest_path": "/repo/packages/protocol/Cargo.toml", "dependencies": []},
            {"id": "path+file:///repo#kernel@1.0.0", "name": "kernel", "version": "1.0.0", "manifest_path": "/repo/kernel/Cargo.toml", "dependencies": ["protocol"]},
            {"id": "path+file:///repo#lab@1.0.0", "name": "hornvale-lab", "version": "1.0.0", "manifest_path": "/repo/windows/lab/Cargo.toml", "dependencies": ["kernel"]},
            {"id": "path+file:///repo#digest@1.0.0", "name": "digest", "version": "1.0.0", "manifest_path": "/repo/tools/digest/Cargo.toml", "dependencies": ["protocol", "hornvale-lab"]},
            {"id": "path+file:///repo#unrelated@1.0.0", "name": "unrelated", "version": "1.0.0", "manifest_path": "/repo/domains/unrelated/Cargo.toml", "dependencies": []},
        ],
        "edges": {
            "protocol": ["kernel", "digest"],
            "kernel": ["hornvale-lab"],
            "hornvale-lab": ["digest"],
            "digest": [],
            "unrelated": [],
        },
    }


def attempt(kind, *, valid=True, preparation=2.0, build=5.0, test=3.0):
    record = {
        "valid": valid,
        "target": {"classification": kind},
        "timing": {
            "preparation_s": preparation,
            "build_s": build,
            "test_s": test,
        },
        "graph": {"package_count": 5, "workspace_member_count": 5},
        "cleanup": {"complete": True},
    }
    return record


class CargoGraphTests(unittest.TestCase):
    def test_runs_locked_offline_metadata_and_canonicalizes_packages(self):
        output = {
            "packages": [
                {"id": "z", "name": "b", "version": "1", "manifest_path": "/repo/b/Cargo.toml", "dependencies": [{"name": "a"}]},
                {"id": "a", "name": "a", "version": "1", "manifest_path": "/repo/a/Cargo.toml", "dependencies": []},
            ],
            "workspace_members": ["z", "a"],
            "resolve": {"nodes": [{"id": "z", "dependencies": ["a"]}, {"id": "a", "dependencies": []}]},
        }
        with tempfile.TemporaryDirectory() as directory:
            manifest = Path(directory) / "Cargo.toml"
            target = Path(directory) / "target"
            manifest.write_text("[workspace]\n", encoding="utf-8")
            completed = mock.Mock(returncode=0, stdout=json.dumps(output).encode(), stderr=b"")
            with mock.patch("measure.subprocess.run", return_value=completed) as run:
                result = cargo_graph(manifest, target)
        command = run.call_args.args[0]
        self.assertEqual(command[:4], ["cargo", "metadata", "--locked", "--offline"])
        self.assertEqual(result["package_count"], 2)
        self.assertEqual([p["name"] for p in result["packages"]], ["a", "b"])
        self.assertEqual(result["edges"]["b"], ["a"])
        self.assertEqual(result["command"]["stdout"]["bytes"], len(completed.stdout))


class ClosureTests(unittest.TestCase):
    def test_distinguishes_direct_reverse_and_full_invalidation(self):
        result = changed_closure(graph_fixture(), ["windows/lab/src/lib.rs"])
        self.assertEqual(result["directly_changed"], ["hornvale-lab"])
        self.assertEqual(result["reverse_dependents"], ["digest"])
        self.assertEqual(result["full_invalidation"], ["hornvale-lab", "digest"])

    def test_unrelated_paths_stay_out_of_closure(self):
        result = changed_closure(graph_fixture(), ["docs/README.md"])
        self.assertEqual(result["directly_changed"], [])
        self.assertEqual(result["reverse_dependents"], [])
        self.assertEqual(result["full_invalidation"], [])


class SummaryTests(unittest.TestCase):
    def test_pairs_cold_and_warm_without_combining_nested_costs(self):
        result = summarize_baseline([attempt("cold", preparation=11, build=20, test=7), attempt("warm", preparation=3, build=8, test=4)])
        self.assertEqual(result["pair_count"], 1)
        self.assertEqual(result["costs"]["cold"]["build_s"], [20.0])
        self.assertEqual(result["costs"]["warm"]["build_s"], [8.0])
        self.assertEqual(result["costs"]["cold"]["total_s"], [38.0])
        self.assertEqual(result["costs"]["warm"]["total_s"], [15.0])
        self.assertEqual(result["graph_counts"], {"package_count": [5], "workspace_member_count": [5]})

    def test_rejects_incomplete_attempts_instead_of_aggregating(self):
        with self.assertRaisesRegex(ValueError, "incomplete"):
            summarize_baseline([attempt("cold"), attempt("warm", valid=False)])

    def test_rejects_unpaired_or_duplicate_classifications(self):
        with self.assertRaisesRegex(ValueError, "paired"):
            summarize_baseline([attempt("cold")])
        with self.assertRaisesRegex(ValueError, "duplicate"):
            summarize_baseline([attempt("cold"), attempt("cold"), attempt("warm")])


if __name__ == "__main__":
    unittest.main()
