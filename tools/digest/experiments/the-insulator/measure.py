"""Bounded, owned build measurements for The Insulator experiment."""

import base64
import hashlib
import importlib.util
import json
import os
import selectors
import subprocess
import time
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


def command_for_workload(workload: dict, checkout: Path) -> list[str]:
    """Expand the sole absolute-path placeholder at execution time."""
    checkout = Path(checkout)
    if not checkout.is_absolute():
        raise ValueError("workload checkout must be absolute")
    command = workload["command"]
    return [argument.replace("${CHECKOUT}", str(checkout)) for argument in command]


def _stream(raw: bytes) -> dict:
    return {
        "base64": base64.b64encode(raw).decode("ascii"),
        "bytes": len(raw),
        "sha256": sha256(raw),
    }


def _owned_path(path: Path, root: Path) -> bool:
    try:
        path.resolve().relative_to(root.resolve())
        return True
    except ValueError:
        return False


def _bounded_measure(command: list[str], cwd: Path, timeout_s: int) -> dict:
    """Run a command with a hard per-stream retention cap."""
    started = time.monotonic()
    process = None
    stdout = bytearray()
    stderr = bytearray()
    output_limit_exceeded = False
    deadline_exceeded = False
    launch_error = None
    cleanup_error = None
    try:
        process = subprocess.Popen(
            command,
            cwd=cwd,
            env=_measurement.controlled_env(),
            stdin=subprocess.DEVNULL,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            start_new_session=True,
        )
        streams = {process.stdout: stdout, process.stderr: stderr}
        selector = selectors.DefaultSelector()
        for stream in streams:
            selector.register(stream, selectors.EVENT_READ)
        while selector.get_map():
            if time.monotonic() - started > timeout_s:
                deadline_exceeded = True
                break
            for key, _ in selector.select(timeout=0.05):
                data = key.fileobj.read1(64 * 1024)
                if not data:
                    selector.unregister(key.fileobj)
                    continue
                retained = streams[key.fileobj]
                remaining = OUTPUT_LIMIT - len(retained)
                if len(data) > remaining:
                    retained.extend(data[:remaining])
                    output_limit_exceeded = True
                    break
                retained.extend(data)
            if output_limit_exceeded:
                break
            if process.poll() is not None and not selector.get_map():
                break
    except OSError as error:
        launch_error = str(error)
    finally:
        if process is not None:
            if deadline_exceeded or output_limit_exceeded:
                try:
                    _measurement.finish_process(process)
                except Exception as error:
                    cleanup_error = f"{type(error).__name__}: {error}"
            else:
                try:
                    process.wait(timeout=2.0)
                except Exception as error:
                    cleanup_error = f"{type(error).__name__}: {error}"
            for stream, retained in ((process.stdout, stdout), (process.stderr, stderr)):
                if stream is None:
                    continue
                try:
                    remainder = stream.read()
                except OSError:
                    remainder = b""
                if remainder:
                    remaining = OUTPUT_LIMIT - len(retained)
                    if len(remainder) > remaining:
                        output_limit_exceeded = True
                    retained.extend(remainder[:remaining])
                stream.close()
    return {
        "exit_code": process.returncode if process is not None else None,
        "elapsed_seconds": time.monotonic() - started,
        "cleanup_error": cleanup_error,
        "interrupted": False,
        "harness_deadline_exceeded": deadline_exceeded,
        "output_limit_exceeded": output_limit_exceeded,
        "launch_error": launch_error,
        "stdout": bytes(stdout),
        "stderr": bytes(stderr),
    }


def capture(command: list[str], cwd: Path, destination: Path, *,
            owned_checkout: Path, owned_target: Path,
            owned_evidence_root: Path,
            timeout_s: int = DEFAULT_TIMEOUT) -> dict:
    """Run one owned command and atomically retain its bounded result."""
    if not isinstance(command, list) or not all(isinstance(arg, str) for arg in command):
        raise ValueError("command must be a list of strings")
    cwd = Path(cwd)
    destination = Path(destination)
    owned_checkout = Path(owned_checkout)
    owned_target = Path(owned_target)
    owned_evidence_root = Path(owned_evidence_root)
    if not owned_checkout.is_absolute() or not owned_checkout.is_dir():
        raise ValueError("owned checkout must be an existing absolute directory")
    if cwd.resolve() != owned_checkout.resolve():
        raise ValueError("capture cwd must be the owned checkout")
    if not owned_target.is_absolute() or not _owned_path(owned_target, owned_checkout):
        raise ValueError("target must be owned by the checkout")
    if not owned_evidence_root.is_absolute() or not _owned_path(destination, owned_evidence_root):
        raise ValueError("evidence destination must be owned")
    if destination.exists():
        raise FileExistsError(destination)
    destination.parent.mkdir(parents=True, exist_ok=True)
    result = _bounded_measure(command, cwd, timeout_s)
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
        "stdout": _stream(result["stdout"]),
        "stderr": _stream(result["stderr"]),
        "interrupted": result.get("interrupted"),
        "deadline_exceeded": result.get("harness_deadline_exceeded"),
        "output_limit_exceeded": result.get("output_limit_exceeded"),
        "launch_error": result.get("launch_error"),
        "retained_sample_directory": result.get("retained_sample_directory"),
        "ownership": {
            "checkout": str(owned_checkout),
            "target": str(owned_target),
            "evidence_root": str(owned_evidence_root),
            "evidence_destination": str(destination),
        },
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


def _sha256(value) -> bool:
    return isinstance(value, str) and len(value) == 64 and all(c in "0123456789abcdef" for c in value)


def _finite_number(value, *, nonnegative=True) -> bool:
    import math
    return (isinstance(value, (int, float)) and not isinstance(value, bool)
            and math.isfinite(value) and (not nonnegative or value >= 0))


def validate_attempt(record: dict) -> None:
    if not isinstance(record, dict) or record.get("schema") != "insulator-attempt-v1":
        raise ValueError("invalid attempt schema")
    source = record.get("source")
    if not isinstance(source, dict) or any(not _full_sha(source.get(key)) for key in ("commit", "tree", "merge_base")):
        raise ValueError("missing source identity")
    graph = record.get("graph")
    if (not isinstance(graph, dict) or not _sha256(graph.get("sha256"))
            or not isinstance(graph.get("package_count"), int)
            or isinstance(graph.get("package_count"), bool) or graph["package_count"] < 0):
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
    if (not isinstance(captured, dict) or not isinstance(captured.get("deadline_s"), int)
            or isinstance(captured.get("deadline_s"), bool) or captured["deadline_s"] <= 0
            or not _finite_number(captured.get("elapsed_s"))):
        raise ValueError("missing capture deadline")
    ownership = captured.get("ownership")
    if (not isinstance(ownership, dict)
            or not all(isinstance(ownership.get(key), str) and Path(ownership[key]).is_absolute()
                       for key in ("checkout", "target", "evidence_root", "evidence_destination"))
            or not isinstance(captured.get("cwd"), str)
            or Path(captured["cwd"]).resolve() != Path(ownership["checkout"]).resolve()
            or not _owned_path(Path(ownership["target"]), Path(ownership["checkout"]))
            or not _owned_path(Path(ownership["evidence_destination"]), Path(ownership["evidence_root"]))
            or not isinstance(target.get("path"), str)
            or not Path(target["path"]).is_absolute()
            or Path(target["path"]).resolve() != Path(ownership["target"]).resolve()):
        raise ValueError("invalid ownership metadata")
    cleanup = captured.get("cleanup")
    if not isinstance(cleanup, dict) or cleanup.get("complete") is not True or cleanup.get("error") is not None:
        raise ValueError("incomplete cleanup")
    if (not isinstance(captured.get("exit_code"), int) or isinstance(captured.get("exit_code"), bool)
            or captured.get("interrupted") is not False or captured.get("deadline_exceeded") is not False
            or not isinstance(captured.get("output_limit_exceeded"), bool)):
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
    if not isinstance(costs, dict) or any(not _finite_number(costs.get(key)) for key in ("preparation_s", "build_s", "test_s")):
        raise ValueError("missing cost fields")
    outputs = record.get("outputs")
    if not isinstance(outputs, list) or not outputs:
        raise ValueError("missing outputs")
    for output in outputs:
        if not isinstance(output, dict) or not isinstance(output.get("path"), str) or not output["path"]:
            raise ValueError("invalid outputs")
        path = Path(output["path"])
        if path.is_absolute() or ".." in path.parts:
            raise ValueError("invalid outputs path")
        if (not isinstance(output.get("bytes"), int) or isinstance(output.get("bytes"), bool)
                or output["bytes"] < 0 or not _sha256(output.get("sha256"))):
            raise ValueError("invalid outputs identity")
    if captured.get("output_limit_exceeded") is True:
        raise ValueError("capture output exceeds byte cap")
    if captured.get("exit_code") != 0:
        failure = record.get("failure")
        if not isinstance(failure, dict) or failure.get("valid_evidence") is not True or not failure.get("reason"):
            raise ValueError("nonzero command requires retained failure reason")
