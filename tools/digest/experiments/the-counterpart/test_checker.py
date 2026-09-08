"""Hand-derived ownership cases; no candidate or collector output is an oracle."""

import copy
import json
from pathlib import Path
import unittest

from checker import evaluate


def example():
    return {
        "source_kinds": ["hearth", "key"],
        "component_kinds": ["hearth", "key"],
        "borrowed": [{"name": "hearth", "owner": "settlement"}],
        "before_concepts": [{"name": "hearth", "owner": "settlement"}],
        "after_concepts": [
            {"name": "hearth", "owner": "settlement"},
            {"name": "key", "owner": "thing"},
        ],
        "settlement_registration": {"outcome": "accepted", "detail": ""},
        "thing_registration": {"outcome": "accepted", "detail": ""},
    }


class CheckerTests(unittest.TestCase):
    def setUp(self):
        self.facts = example()
        self.contract = json.loads(Path(__file__).with_name("contract.json").read_text())

    def outcome(self, question):
        return evaluate(self.facts, self.contract)[question]["outcome"]

    def test_legitimate_borrowing_satisfies_all_four_questions(self):
        results = evaluate(self.facts, self.contract)
        self.assertEqual(
            {key: value["outcome"] for key, value in results.items()},
            {"registration": "satisfied", "components": "satisfied",
             "borrowing": "satisfied", "ownership": "satisfied"},
        )
        for result in results.values():
            self.assertEqual(set(result), {"outcome", "reason"})
            self.assertIsInstance(result["reason"], str)
            self.assertTrue(result["reason"])

    def test_wrong_owner_without_borrowing_is_not_satisfied(self):
        self.facts = {
            "source_kinds": ["key"], "component_kinds": ["key"], "borrowed": [],
            "before_concepts": [],
            "after_concepts": [{"name": "key", "owner": "settlement"}],
            "settlement_registration": {"outcome": "accepted", "detail": ""},
            "thing_registration": {"outcome": "accepted", "detail": ""},
        }
        self.assertEqual(self.outcome("ownership"), "violated")

    def test_undeclared_collision_violates_ownership(self):
        self.facts["borrowed"] = []
        self.assertEqual(self.outcome("ownership"), "violated")

    def test_missing_lender_violates_borrowing(self):
        self.facts["before_concepts"] = []
        self.assertEqual(self.outcome("borrowing"), "violated")

    def test_wrong_lender_violates_borrowing(self):
        self.facts["before_concepts"][0]["owner"] = "climate"
        self.assertEqual(self.outcome("borrowing"), "violated")

    def test_duplicate_borrowing_violates_borrowing(self):
        self.facts["borrowed"] *= 2
        self.assertEqual(self.outcome("borrowing"), "violated")

    def test_conflicting_borrowing_owners_cannot_be_overwritten(self):
        self.facts["borrowed"].append({"name": "hearth", "owner": "climate"})
        self.assertEqual(self.outcome("ownership"), "violated")

    def test_unused_borrowing_violates_borrowing(self):
        self.facts["borrowed"].append({"name": "home", "owner": "settlement"})
        self.facts["before_concepts"].append({"name": "home", "owner": "settlement"})
        self.assertEqual(self.outcome("borrowing"), "violated")

    def test_self_borrowing_violates_borrowing(self):
        self.facts["borrowed"][0]["owner"] = "thing"
        self.facts["before_concepts"][0]["owner"] = "thing"
        self.assertEqual(self.outcome("borrowing"), "violated")

    def test_extra_component_violates_components(self):
        self.facts["component_kinds"].append("loom")
        self.assertEqual(self.outcome("components"), "violated")

    def test_missing_component_violates_components(self):
        self.facts["component_kinds"].remove("key")
        self.assertEqual(self.outcome("components"), "violated")

    def test_duplicate_source_violates_components(self):
        self.facts["source_kinds"].append("key")
        self.assertEqual(self.outcome("components"), "violated")

    def test_duplicate_component_violates_components(self):
        self.facts["component_kinds"].append("key")
        self.assertEqual(self.outcome("components"), "violated")

    def test_extra_thing_concept_violates_ownership(self):
        self.facts["after_concepts"].append({"name": "loom", "owner": "thing"})
        self.assertEqual(self.outcome("ownership"), "violated")

    def test_missing_thing_concept_violates_ownership(self):
        self.facts["after_concepts"].pop()
        self.assertEqual(self.outcome("ownership"), "violated")

    def test_borrowed_name_must_retain_its_declared_owner(self):
        self.facts["after_concepts"][0]["owner"] = "thing"
        self.assertEqual(self.outcome("ownership"), "violated")

    def test_unborrowed_lender_concept_must_not_change_owner(self):
        self.facts["before_concepts"].append({"name": "home", "owner": "settlement"})
        self.facts["after_concepts"].append({"name": "home", "owner": "climate"})
        self.assertEqual(self.outcome("ownership"), "violated")

    def test_unborrowed_lender_concept_must_not_disappear(self):
        self.facts["before_concepts"].append({"name": "home", "owner": "settlement"})
        self.assertEqual(self.outcome("ownership"), "violated")

    def test_retained_unborrowed_lender_concept_is_allowed(self):
        for key in ("before_concepts", "after_concepts"):
            self.facts[key].append({"name": "home", "owner": "settlement"})
        self.assertEqual(self.outcome("ownership"), "satisfied")

    def test_duplicate_observed_owner_does_not_collapse_to_one(self):
        self.facts["after_concepts"].append({"name": "key", "owner": "thing"})
        self.assertEqual(self.outcome("ownership"), "violated")

    def test_ambiguous_lender_does_not_collapse_to_matching_owner(self):
        self.facts["before_concepts"].insert(0, {"name": "hearth", "owner": "climate"})
        self.assertEqual(self.outcome("borrowing"), "violated")

    def test_thing_refusal_keeps_borrowing_judgable_and_ownership_unknown(self):
        self.facts["thing_registration"] = {"outcome": "refused", "detail": "collision"}
        self.facts["after_concepts"] = []
        self.assertEqual(
            {key: row["outcome"] for key, row in evaluate(self.facts, self.contract).items()},
            {"registration": "violated", "components": "satisfied",
             "borrowing": "satisfied", "ownership": "unknown"},
        )

    def test_thing_refusal_does_not_hide_missing_lender(self):
        self.facts["thing_registration"] = {"outcome": "refused", "detail": "missing"}
        self.facts["before_concepts"] = []
        self.assertEqual(self.outcome("borrowing"), "violated")

    def test_settlement_refusal_does_not_certify_partial_lender_state(self):
        self.facts["settlement_registration"] = {"outcome": "refused", "detail": "partial"}
        self.assertEqual(
            {key: row["outcome"] for key, row in evaluate(self.facts, self.contract).items()},
            {"registration": "violated", "components": "satisfied",
             "borrowing": "unknown", "ownership": "unknown"},
        )

    def test_refusal_does_not_hide_independent_declaration_violation(self):
        self.facts["settlement_registration"]["outcome"] = "refused"
        self.facts["borrowed"] *= 2
        self.assertEqual(self.outcome("borrowing"), "violated")

    def test_array_order_has_no_semantic_role(self):
        self.facts["source_kinds"].reverse()
        self.facts["after_concepts"].reverse()
        self.assertEqual(self.outcome("ownership"), "satisfied")
        self.assertEqual(self.outcome("components"), "satisfied")

    def test_inputs_are_not_changed(self):
        original = copy.deepcopy((self.facts, self.contract))
        evaluate(self.facts, self.contract)
        self.assertEqual((self.facts, self.contract), original)

    def test_missing_fact_fields_are_invalid_even_after_refusal(self):
        for field in self.facts:
            with self.subTest(field=field):
                facts = copy.deepcopy(self.facts)
                facts["thing_registration"]["outcome"] = "refused"
                del facts[field]
                with self.assertRaises(ValueError):
                    evaluate(facts, self.contract)

    def test_invalid_fact_shapes_raise_value_error(self):
        replacements = [
            ("source_kinds", "key"), ("source_kinds", [None]),
            ("source_kinds", [""]), ("component_kinds", {}),
            ("borrowed", {}), ("borrowed", ["hearth"]),
            ("borrowed", [{"name": "hearth"}]),
            ("before_concepts", [{"name": "hearth", "owner": 7}]),
            ("after_concepts", [{"name": "key", "owner": ""}]),
            ("thing_registration", {"outcome": "unknown", "detail": ""}),
            ("thing_registration", {"outcome": "accepted"}),
            ("settlement_registration", {"outcome": "accepted", "detail": None}),
            ("settlement_registration", None),
        ]
        for field, value in replacements:
            with self.subTest(field=field, value=value):
                facts = copy.deepcopy(self.facts)
                facts[field] = value
                with self.assertRaises(ValueError):
                    evaluate(facts, self.contract)
        for facts in (None, [], {**self.facts, "candidate": {}}):
            with self.subTest(facts=facts), self.assertRaises(ValueError):
                evaluate(facts, self.contract)

    def test_missing_contract_fields_are_invalid(self):
        for field in self.contract:
            with self.subTest(field=field):
                contract = copy.deepcopy(self.contract)
                del contract[field]
                with self.assertRaises(ValueError):
                    evaluate(self.facts, contract)
        for field in self.contract["questions"][0]:
            with self.subTest(question_field=field):
                contract = copy.deepcopy(self.contract)
                del contract["questions"][0][field]
                with self.assertRaises(ValueError):
                    evaluate(self.facts, contract)

    def test_unknown_duplicate_or_missing_question_ids_are_invalid(self):
        for operation in ("unknown", "duplicate", "missing"):
            with self.subTest(operation=operation):
                contract = copy.deepcopy(self.contract)
                if operation == "unknown":
                    contract["questions"][0]["id"] = "new-question"
                elif operation == "duplicate":
                    contract["questions"].append(copy.deepcopy(contract["questions"][0]))
                else:
                    contract["questions"].pop()
                with self.assertRaises(ValueError):
                    evaluate(self.facts, contract)

    def test_invalid_contract_shapes_raise_value_error(self):
        for field, value in (
            ("schema", "other"), ("contributor", "other"), ("scope", "other"),
            ("questions", {}), ("questions", [None]), ("authorities", []),
            ("authorities", [4]),
        ):
            with self.subTest(field=field, value=value):
                contract = copy.deepcopy(self.contract)
                contract[field] = value
                with self.assertRaises(ValueError):
                    evaluate(self.facts, contract)
        for field, value in (("id", []), ("question", ""), ("packages", "wrong"),
                             ("inputs", [None]), ("sources", [])):
            with self.subTest(question_field=field, value=value):
                contract = copy.deepcopy(self.contract)
                contract["questions"][0][field] = value
                with self.assertRaises(ValueError):
                    evaluate(self.facts, contract)
        for contract in (None, [], {**self.contract, "unknown": True}):
            with self.subTest(contract=contract), self.assertRaises(ValueError):
                evaluate(self.facts, contract)


if __name__ == "__main__":
    unittest.main()
