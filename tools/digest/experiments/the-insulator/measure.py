"""Bounded, owned build measurements for The Insulator experiment."""

import base64
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import tempfile


HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
OUTPUT_LIMIT = 16 * 1024 * 1024
DEFAULT_TIMEOUT = 3600

_spec = importlib.util.spec_from_file_location(
    "insulator_charter_measure", ROOT / "scripts" / "charter_measure.py"
)
_measurement = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_measurement)


def sha256(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def _load_json(path: Path):
    def unique(pairs):
        result = {}
        for key, value in pairs:
            if key in result:
                raise ValueError(f"duplicate JSON key: {key}")
            result[key] = value
        return result

    return json.loads(path.read_text(encoding="utf-8"), object_pairs_hook=unique)


def load_workloads(path: Path) -> dict:
    value = _load_json(Path(path))
    if value.get("schema") != "insulator-workloads-v1":
        raise ValueError("invalid workload schema")
    workloads = value.get("workloads")
    if not isinstance(workloads, list) or not workloads:
        raise ValueError("workloads must be a non-empty list")
    identifiers = set()
    for workload in workloads:
        if not isinstance(workload, dict):
            raise ValueError("workload must be an object")
        identifier = workload.get("id")
        if not isinstance(identifier, str) or not identifier or identifier in identifiers:
            raise ValueError("workload id must be unique")
        identifiers.add(identifier)
        command = workload.get("command")
        if (not isinstance(command, list) or not command or
                any(not isinstance(arg, str) or not arg for arg in command)):
            raise ValueError(f"{identifier}: command must be a non-empty list of strings")
        outputs = workload.get("expected_outputs")
        if not isinstance(outputs, list) or not outputs:
            raise ValueError(f"{identifier}: expected_outputs is required")
        for output in outputs:
            if not isinstance(output, dict) or not isinstance(output.get("path"), str):
                raise ValueError(f"{identifier}: output path is required")
            if output.get("compare") not in {"sha256", "bytes"}:
                raise ValueError(f"{identifier}: unsupported output comparison")
    return value


def _stream(raw: bytes) -> dict:
    return {
        "base64": base64.b64encode(raw).decode("ascii"),
        "bytes": len(raw),
        "sha256": sha256(raw),
    }


def capture(command: list[str], cwd: Path, destination: Path,
            timeout_s: int = DEFAULT_TIMEOUT) -> dict:
    """Run one owned command and atomically retain its bounded result."""
    if not isinstance(command, list) or not all(isinstance(arg, str) for arg in command):
        raise ValueError("command must be a list of strings")
    destination = Path(destination)
    if destination.exists():
        raise FileExistsError(destination)
    destination.parent.mkdir(parents=True, exist_ok=True)
    result = _measurement.measure(
        command, Path(cwd), retain_output=True,
        output_limit_bytes=OUTPUT_LIMIT, deadline_seconds=timeout_s,
    )
    record = {
        "command": command,
        "cwd": str(cwd),
        "exit_code": result.get("exit_code"),
        "deadline_s": timeout_s,
        "elapsed_s": result.get("elapsed_seconds"),
        "cleanup": {
            "complete": result.get("cleanup_error") is None,
            "error": result.get("cleanup_error"),
        },
        "stdout": _stream(base64.b64decode(result["stdout_base64"])),
        "stderr": _stream(base64.b64decode(result["stderr_base64"])),
        "interrupted": result.get("interrupted"),
        "deadline_exceeded": result.get("harness_deadline_exceeded"),
        "output_limit_exceeded": result.get("output_limit_exceeded"),
        "launch_error": result.get("launch_error"),
        "retained_sample_directory": result.get("retained_sample_directory"),
    }
    with tempfile.NamedTemporaryFile(
        "w", encoding="utf-8", dir=destination.parent,
        prefix=f".{destination.name}.", delete=False,
    ) as temporary:
        json.dump(record, temporary, indent=2, sort_keys=True)
        temporary.write("\n")
        temporary_path = Path(temporary.name)
    os.replace(temporary_path, destination)
    return record


def manifest_for_attempt(*, source: dict, graph: dict, toolchain: dict,
                         target: dict, command: list[str], capture: dict,
                         costs: dict, outputs: list[dict],
                         failure: dict | None = None) -> dict:
    record = {
        "schema": "insulator-attempt-v1",
        "source": source,
        "graph": graph,
        "toolchain": toolchain,
        "target": target,
        "command": command,
        "capture": capture,
        "costs": costs,
        "outputs": outputs,
        "failure": failure,
    }
    validate_attempt(record)
    return record


def _full_sha(value) -> bool:
    return isinstance(value, str) and len(value) == 40 and all(c in "0123456789abcdef" for c in value)


def validate_attempt(record: dict) -> None:
    if not isinstance(record, dict) or record.get("schema") != "insulator-attempt-v1":
        raise ValueError("invalid attempt schema")
    source = record.get("source")
    if not isinstance(source, dict) or any(not _full_sha(source.get(key)) for key in ("commit", "tree", "merge_base")):
        raise ValueError("missing source identity")
    graph = record.get("graph")
    if not isinstance(graph, dict) or not isinstance(graph.get("sha256"), str) or not isinstance(graph.get("package_count"), int):
        raise ValueError("missing graph identity")
    toolchain = record.get("toolchain")
    if not isinstance(toolchain, dict) or not toolchain.get("rustc") or not toolchain.get("host_class"):
        raise ValueError("missing toolchain identity")
    target = record.get("target")
    if not isinstance(target, dict) or not isinstance(target.get("path"), str) or target.get("classification") not in {"cold", "warm"}:
        raise ValueError("missing target identity")
    command = record.get("command")
    if not isinstance(command, list) or not command or not all(isinstance(arg, str) for arg in command):
        raise ValueError("invalid command")
    captured = record.get("capture")
    if not isinstance(captured, dict) or not isinstance(captured.get("deadline_s"), int) or captured["deadline_s"] <= 0:
        raise ValueError("missing capture deadline")
    cleanup = captured.get("cleanup")
    if not isinstance(cleanup, dict) or cleanup.get("complete") is not True or cleanup.get("error") is not None:
        raise ValueError("incomplete cleanup")
    if captured.get("interrupted") is not False or captured.get("deadline_exceeded") is not False:
        raise ValueError("incomplete capture")
    for name in ("stdout", "stderr"):
        stream = captured.get(name)
        if not isinstance(stream, dict) or not isinstance(stream.get("base64"), str):
            raise ValueError(f"missing {name}")
        try:
            raw = base64.b64decode(stream["base64"], validate=True)
        except (ValueError, TypeError) as error:
            raise ValueError(f"invalid {name}") from error
        if len(raw) > OUTPUT_LIMIT:
            raise ValueError(f"{name} exceeds byte cap")
        if stream.get("bytes") != len(raw) or stream.get("sha256") != sha256(raw):
            raise ValueError(f"{name} hash/size mismatch")
    costs = record.get("costs")
    if not isinstance(costs, dict) or any(not isinstance(costs.get(key), (int, float)) or costs[key] < 0 for key in ("preparation_s", "build_s", "test_s")):
        raise ValueError("missing cost fields")
    outputs = record.get("outputs")
    if not isinstance(outputs, list):
        raise ValueError("missing outputs")
    if captured.get("output_limit_exceeded") is True:
        raise ValueError("capture output exceeds byte cap")
    if captured.get("exit_code") != 0:
        failure = record.get("failure")
        if not isinstance(failure, dict) or failure.get("valid_evidence") is not True or not failure.get("reason"):
            raise ValueError("nonzero command requires retained failure reason")
