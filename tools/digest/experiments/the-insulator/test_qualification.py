import sys
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parent
sys.path.insert(0, str(ROOT))

from measure import decide, invalidation_matrix, run_paired_qualification  # noqa: E402


def attempt(source="source-a", *, cleanup=True, output="same", build=10.0,
            valid=True):
    return {
        "source": {"commit": source if len(source) == 40 else "a" * 40,
                    "tree": "b" * 40, "merge_base": "c" * 40},
        "capture": {"cleanup": {"complete": cleanup, "error": None if cleanup else "lost"}},
        "valid": valid,
        "outputs": {"publication": output},
        "costs": {"build_s": build},
    }


def qualification_fixture():
    baseline = {
        "mac": {"attempts": [attempt(build=10.0)]},
        "linux": {"attempts": [attempt(build=12.0)]},
    }
    candidate = {
        "mac": {"attempts": [attempt(build=5.0)]},
        "linux": {"attempts": [attempt(build=6.0)]},
    }
    return baseline, candidate


class PairedQualificationTests(unittest.TestCase):
    def test_requires_every_requested_host_record(self):
        baseline, candidate = qualification_fixture()
        del candidate["linux"]
        with self.assertRaisesRegex(ValueError, "linux"):
            run_paired_qualification(baseline, candidate, ["mac", "linux"])

    def test_rejects_mismatched_source_identities(self):
        baseline, candidate = qualification_fixture()
        candidate["mac"]["attempts"][0]["source"]["commit"] = "e" * 40
        with self.assertRaisesRegex(ValueError, "source"):
            run_paired_qualification(baseline, candidate, ["mac", "linux"])

    def test_rejects_incomplete_cleanup(self):
        baseline, candidate = qualification_fixture()
        candidate["mac"]["attempts"][0]["capture"]["cleanup"]["complete"] = False
        with self.assertRaisesRegex(ValueError, "cleanup"):
            run_paired_qualification(baseline, candidate, ["mac", "linux"])

    def test_returns_complete_pairs(self):
        baseline, candidate = qualification_fixture()
        result = run_paired_qualification(baseline, candidate, ["mac", "linux"])
        self.assertEqual(set(result["hosts"]), {"mac", "linux"})
        self.assertTrue(all(pair["complete"] for pair in result["pairs"].values()))


class InvalidationTests(unittest.TestCase):
    def test_maps_edits_to_rebuilt_packages_and_output(self):
        result = invalidation_matrix([
            {"edit": "unrelated", "baseline_packages": [],
             "candidate_packages": [], "output_identity": "same"},
            {"edit": "protocol", "baseline_packages": ["protocol"],
             "candidate_packages": ["protocol"], "output_identity": "same"},
        ])
        self.assertEqual(result["unrelated"]["candidate_packages"], [])
        self.assertEqual(result["protocol"]["output_identity"], "same")


class DecisionTests(unittest.TestCase):
    def test_rejects_output_mismatch(self):
        comparison = {"complete": True, "output_match": False,
                      "boundary": {"undeclared_dependencies": [], "duplicated_authority": False},
                      "performance": {"repeatable_reduction": True}}
        self.assertEqual(decide(comparison), "reject")

    def test_rejects_undeclared_dependency(self):
        comparison = {"complete": True, "output_match": True,
                      "boundary": {"undeclared_dependencies": ["windows/lab"], "duplicated_authority": False},
                      "performance": {"repeatable_reduction": True}}
        self.assertEqual(decide(comparison), "reject")

    def test_rejects_duplicated_authority(self):
        comparison = {"complete": True, "output_match": True,
                      "boundary": {"undeclared_dependencies": [], "duplicated_authority": True},
                      "performance": {"repeatable_reduction": True}}
        self.assertEqual(decide(comparison), "reject")

    def test_accepts_stable_reduction(self):
        comparison = {"complete": True, "output_match": True,
                      "boundary": {"undeclared_dependencies": [], "duplicated_authority": False},
                      "performance": {"repeatable_reduction": True}}
        self.assertEqual(decide(comparison), "admit")

    def test_rejects_noisy_non_reduction(self):
        comparison = {"complete": True, "output_match": True,
                      "boundary": {"undeclared_dependencies": [], "duplicated_authority": False},
                      "performance": {"repeatable_reduction": False}}
        self.assertEqual(decide(comparison), "reject")

    def test_negative_rejection_is_valid_evidence(self):
        comparison = {"complete": True, "evidence_valid": True, "output_match": False,
                      "boundary": {"undeclared_dependencies": [], "duplicated_authority": False},
                      "performance": {"repeatable_reduction": False}}
        self.assertEqual(decide(comparison), "reject")
        self.assertTrue(comparison["evidence_valid"])


if __name__ == "__main__":
    unittest.main()
