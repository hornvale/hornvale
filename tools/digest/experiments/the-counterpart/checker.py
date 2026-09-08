"""Finite judgments over raw counterpart facts, independent of candidate output.

The runner owns raw JSON parsing (including duplicate-key rejection), frozen
file identities, and attempt validity. This module validates decoded shapes
before evaluating every frozen question. It does not run or import Rust code.
"""

from collections import Counter, defaultdict


QUESTION_IDS = ("registration", "components", "borrowing", "ownership")
FACT_FIELDS = {
    "source_kinds", "component_kinds", "borrowed", "before_concepts",
    "after_concepts", "settlement_registration", "thing_registration",
}


def _object(value, fields, path):
    if not isinstance(value, dict) or set(value) != set(fields):
        raise ValueError(f"{path} must be an object with exactly {sorted(fields)!r}")


def _text(value, path, *, allow_empty=False):
    if not isinstance(value, str) or (not allow_empty and not value.strip()):
        raise ValueError(f"{path} must be {'a' if allow_empty else 'a nonempty'} string")


def _array(value, path):
    if not isinstance(value, list):
        raise ValueError(f"{path} must be an array")


def _names(value, path, *, require_members=False):
    _array(value, path)
    if require_members and not value:
        raise ValueError(f"{path} must not be empty")
    for index, name in enumerate(value):
        _text(name, f"{path}[{index}]")


def _validate_contract(contract):
    _object(contract, {"schema", "contributor", "scope", "questions", "authorities"}, "contract")
    for field, expected in (
        ("schema", "counterpart-v1"),
        ("contributor", "hornvale.thing"),
        ("scope", "domains/thing"),
    ):
        if contract[field] != expected:
            raise ValueError(f"contract.{field} must be {expected!r}")
    _names(contract["authorities"], "contract.authorities", require_members=True)
    _array(contract["questions"], "contract.questions")
    ids = []
    for index, question in enumerate(contract["questions"]):
        path = f"contract.questions[{index}]"
        _object(question, {"id", "question", "packages", "inputs", "sources"}, path)
        _text(question["id"], f"{path}.id")
        _text(question["question"], f"{path}.question")
        for field in ("packages", "inputs", "sources"):
            _names(question[field], f"{path}.{field}", require_members=True)
        ids.append(question["id"])
    if Counter(ids) != Counter(QUESTION_IDS):
        raise ValueError("contract.questions must contain each frozen question ID exactly once")


def _validate_facts(facts):
    _object(facts, FACT_FIELDS, "facts")
    for field in ("source_kinds", "component_kinds"):
        _names(facts[field], f"facts.{field}")
    for field in ("borrowed", "before_concepts", "after_concepts"):
        _array(facts[field], f"facts.{field}")
        for index, entry in enumerate(facts[field]):
            path = f"facts.{field}[{index}]"
            _object(entry, {"name", "owner"}, path)
            _text(entry["name"], f"{path}.name")
            _text(entry["owner"], f"{path}.owner")
    for field in ("settlement_registration", "thing_registration"):
        path = f"facts.{field}"
        registration = facts[field]
        _object(registration, {"outcome", "detail"}, path)
        if registration["outcome"] not in ("accepted", "refused"):
            raise ValueError(f"{path}.outcome must be accepted or refused")
        _text(registration["detail"], f"{path}.detail", allow_empty=True)


def _owners(entries):
    """Keep multiplicity: neither ambiguous nor repeated observations disappear."""
    owners = defaultdict(list)
    for entry in entries:
        owners[entry["name"]].append(entry["owner"])
    return owners


def _result(outcome, reason):
    return {"outcome": outcome, "reason": reason}


def _judgment(problems, success):
    if problems:
        return _result("violated", "; ".join(problems))
    return _result("satisfied", success)


def evaluate(facts: dict, contract: dict) -> dict[str, dict[str, str]]:
    """Validate decoded inputs, then answer all four questions independently."""
    _validate_contract(contract)
    _validate_facts(facts)

    settlement_ok = facts["settlement_registration"]["outcome"] == "accepted"
    thing_ok = facts["thing_registration"]["outcome"] == "accepted"
    refused = [
        owner for owner, accepted in (("settlement", settlement_ok), ("thing", thing_ok))
        if not accepted
    ]
    registration = _judgment(
        [f"{owner} registration refused" for owner in refused],
        "Settlement and Thing registration both completed without refusal",
    )

    source = Counter(facts["source_kinds"])
    component = Counter(facts["component_kinds"])
    component_problems = []
    for label, counts in (("source", source), ("component", component)):
        duplicates = sorted(name for name, count in counts.items() if count > 1)
        if duplicates:
            component_problems.append(f"duplicate {label} names: {duplicates!r}")
    missing = sorted(source.keys() - component.keys())
    extra = sorted(component.keys() - source.keys())
    if missing:
        component_problems.append(f"source names missing components: {missing!r}")
    if extra:
        component_problems.append(f"components outside source roster: {extra!r}")
    components = _judgment(component_problems, "Source and component rosters agree without duplicates")

    borrowed = _owners(facts["borrowed"])
    before = _owners(facts["before_concepts"])
    after = _owners(facts["after_concepts"])
    borrowing_problems = []
    for name, owners in sorted(borrowed.items()):
        if len(owners) != 1:
            borrowing_problems.append(f"duplicate borrowing name: {name!r}")
        if name not in source:
            borrowing_problems.append(f"borrowing outside source roster: {name!r}")
        if "thing" in owners:
            borrowing_problems.append(f"self borrowing: {name!r}")
        if settlement_ok:
            for owner in sorted(set(owners)):
                if before.get(name) != [owner]:
                    borrowing_problems.append(
                        f"{name!r} requires one prior owner {owner!r}; observed {before.get(name, [])!r}"
                    )
    if borrowing_problems:
        borrowing = _judgment(borrowing_problems, "")
    elif not settlement_ok:
        borrowing = _result("unknown", "Settlement refused; lender registration is incomplete")
    else:
        borrowing = _result("satisfied", "Every borrowing is unique, used, non-self, and supplied by its lender")

    if not (settlement_ok and thing_ok):
        ownership = _result("unknown", "Registration refused; the composed registry is incomplete")
    else:
        ownership_problems = []
        for name in sorted(source):
            expected = set(borrowed.get(name, ["thing"]))
            observed = after.get(name, [])
            if len(expected) != 1 or len(observed) != 1 or set(observed) != expected:
                ownership_problems.append(
                    f"{name!r} expected owner {sorted(expected)!r}; observed {observed!r}"
                )
        expected_thing_names = {
            name for name in source if set(borrowed.get(name, ["thing"])) == {"thing"}
        }
        extra_thing_names = sorted(
            name for name, owners in after.items()
            if "thing" in owners and name not in expected_thing_names
        )
        if extra_thing_names:
            ownership_problems.append(f"extra Thing-owned names: {extra_thing_names!r}")
        for name, owners in sorted(before.items()):
            if len(owners) != 1 or after.get(name) != owners:
                ownership_problems.append(
                    f"prior concept {name!r} did not retain its single owner {owners!r}"
                )
        for name, owners in sorted(after.items()):
            if len(owners) != 1:
                ownership_problems.append(f"multiple final entries for concept {name!r}")
        ownership = _judgment(
            ownership_problems,
            "Every Thing name has its declared owner, no extra Thing names exist, and lender owners persist",
        )

    return {
        "registration": registration,
        "components": components,
        "borrowing": borrowing,
        "ownership": ownership,
    }
