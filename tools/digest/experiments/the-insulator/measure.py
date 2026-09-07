"""Bounded, owned build measurements for The Insulator experiment."""

import base64
import hashlib
import importlib.util
import json
import os
import platform
import selectors
import shutil
import subprocess
import time
from pathlib import Path
import tempfile


HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
WORKLOADS_PATH = HERE / "workloads.json"
OUTPUT_LIMIT = 16 * 1024 * 1024
DEFAULT_TIMEOUT = 3600
SUPPORTED_ENFORCEMENT_METHODS = {"sandbox-exec", "bwrap"}


class EnforcementUnavailable(RuntimeError):
    """The host cannot provide the required filesystem write boundary."""

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
        phases = workload.get("phases")
        if not isinstance(phases, dict) or set(phases) != {"preparation", "test"}:
            raise ValueError(f"{identifier}: preparation and test phases are required")
        for phase_name in ("preparation", "test"):
            phase_command = phases[phase_name].get("command") if isinstance(phases[phase_name], dict) else None
            if (not isinstance(phase_command, list) or not phase_command or
                    any(not isinstance(arg, str) or not arg for arg in phase_command)):
                raise ValueError(f"{identifier}: {phase_name} command must be a non-empty list of strings")
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


def _phase_command(workload: dict, phase: str, checkout: Path) -> list[str]:
    if phase not in {"preparation", "test"}:
        raise ValueError(f"unknown workload phase: {phase}")
    return [argument.replace("${CHECKOUT}", str(checkout))
            for argument in workload["phases"][phase]["command"]]


def _workload(workload_id: str) -> dict:
    if not isinstance(workload_id, str) or not workload_id:
        raise ValueError("workload id must be a non-empty string")
    workloads = load_workloads(WORKLOADS_PATH)["workloads"]
    for workload in workloads:
        if workload["id"] == workload_id:
            return workload
    raise ValueError(f"unknown workload id: {workload_id}")


def _bounded_evidence(raw: bytes, label: str) -> dict:
    if len(raw) > OUTPUT_LIMIT:
        raise ValueError(f"cargo metadata {label} exceeds output limit")
    return _stream(raw)


def _bounded_command(command: list[str], cwd: Path, *, timeout_s: float,
                     env: dict | None = None) -> dict:
    """Run a controller command with bounded time and retained streams."""
    started = time.monotonic()
    stdout = bytearray()
    stderr = bytearray()
    process = None
    deadline_exceeded = False
    output_limit_exceeded = False
    launch_error = None
    cleanup_error = None
    try:
        process = subprocess.Popen(
            command, cwd=cwd, env=env or _measurement.controlled_env(),
            stdin=subprocess.DEVNULL, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
            start_new_session=True,
        )
        streams = {process.stdout: stdout, process.stderr: stderr}
        selector = selectors.DefaultSelector()
        for stream in streams:
            selector.register(stream, selectors.EVENT_READ)
        while selector.get_map():
            if time.monotonic() - started >= timeout_s:
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
        if output_limit_exceeded:
            deadline_exceeded = False
    except OSError as exc:
        launch_error = str(exc)
    finally:
        if process is not None:
            if deadline_exceeded or output_limit_exceeded:
                try:
                    _measurement.finish_process(process)
                except Exception as exc:  # retain the primary failure evidence
                    cleanup_error = f"{type(exc).__name__}: {exc}"
            else:
                try:
                    process.wait(timeout=2.0)
                except Exception as exc:
                    cleanup_error = f"{type(exc).__name__}: {exc}"
            for stream, retained in ((process.stdout, stdout), (process.stderr, stderr)):
                if stream is None:
                    continue
                remainder = stream.read()
                remaining = OUTPUT_LIMIT - len(retained)
                if len(remainder) > remaining:
                    output_limit_exceeded = True
                retained.extend(remainder[:remaining])
                stream.close()
    return {
        "returncode": process.returncode if process is not None else None,
        "stdout": bytes(stdout), "stderr": bytes(stderr),
        "deadline_exceeded": deadline_exceeded,
        "output_limit_exceeded": output_limit_exceeded,
        "cleanup_complete": cleanup_error is None,
        "cleanup_error": cleanup_error, "launch_error": launch_error,
    }


def cargo_graph(manifest: Path, target_dir: Path) -> dict:
    """Read a locked, offline Cargo graph and return its canonical identity."""
    manifest = Path(manifest)
    target_dir = Path(target_dir)
    if not manifest.is_absolute() or not target_dir.is_absolute():
        raise ValueError("cargo graph paths must be absolute")
    command = ["cargo", "metadata", "--locked", "--offline", "--format-version", "1", "--manifest-path", str(manifest)]
    environment = _measurement.controlled_env()
    environment["CARGO_TARGET_DIR"] = str(target_dir)
    result = _bounded_command(command, manifest.parent, timeout_s=DEFAULT_TIMEOUT, env=environment)
    stdout = _bounded_evidence(result["stdout"], "stdout")
    stderr = _bounded_evidence(result["stderr"], "stderr")
    if result["deadline_exceeded"]:
        raise ValueError("cargo metadata exceeded deadline")
    if result["output_limit_exceeded"]:
        raise ValueError("cargo metadata exceeded output limit")
    if result["launch_error"]:
        raise ValueError(f"cargo metadata could not start: {result['launch_error']}")
    if result["returncode"] != 0:
        raise ValueError(f"cargo metadata failed with exit code {result['returncode']}")
    try:
        metadata = json.loads(result["stdout"])
    except json.JSONDecodeError as exc:
        raise ValueError("cargo metadata did not emit JSON") from exc
    packages_by_id = {package["id"]: package for package in metadata.get("packages", [])}
    members = set(metadata.get("workspace_members", []))
    packages = [{
        "id": package["id"], "name": package["name"], "version": package["version"],
        "manifest_path": str(Path(package["manifest_path"]).resolve()),
        "dependencies": sorted({dependency["name"] for dependency in package.get("dependencies", [])}),
        "workspace_member": package["id"] in members,
    } for package in metadata.get("packages", [])]
    packages.sort(key=lambda package: (package["name"], package["version"], package["id"]))
    id_to_name = {identifier: package["name"] for identifier, package in packages_by_id.items()}
    edges = {}
    for node in metadata.get("resolve", {}).get("nodes", []):
        name = id_to_name.get(node.get("id"))
        if name is not None:
            edges[name] = sorted({id_to_name[dependency] for dependency in node.get("dependencies", []) if dependency in id_to_name})
    edges = {name: edges.get(name, []) for name in sorted(edges)}
    repository_root = _repository_root(manifest)
    canonical = {"packages": packages, "workspace_members": sorted(members), "edges": edges,
                 "repository_root": str(repository_root)}
    identity = sha256(json.dumps(canonical, sort_keys=True, separators=(",", ":")).encode())
    return {"identity": {"format": "cargo-metadata-v1", "sha256": identity},
            "package_count": len(packages), "workspace_member_count": len(members),
            "packages": packages, "edges": edges,
            "command": {"argv": command, "returncode": result["returncode"], "stdout": stdout, "stderr": stderr},
            "repository_root": str(repository_root)}


def _repository_root(path: Path) -> Path:
    for parent in (path.resolve().parent, *path.resolve().parents):
        if (parent / ".git").exists():
            return parent
    return path.resolve().parent


def _path_matches_package(changed: str, manifest_path: str, workspace_root: tuple[str, ...]) -> bool:
    changed_parts = Path(changed).parts
    package_parts = Path(manifest_path).parent.parts
    if not package_parts or package_parts[:len(workspace_root)] != workspace_root:
        return False
    relative_root = package_parts[len(workspace_root):]
    if changed_parts and changed_parts[0] == "/":
        changed_parts = changed_parts[1:]
    if changed_parts[:len(workspace_root)] == workspace_root:
        changed_parts = changed_parts[len(workspace_root):]
    return bool(relative_root) and changed_parts[:len(relative_root)] == relative_root


def changed_closure(graph: dict, changed_paths: list[str]) -> dict:
    """Classify directly changed packages and their reverse dependents."""
    packages = graph.get("packages")
    if not isinstance(packages, list):
        raise ValueError("graph packages are required")
    repository_root = Path(graph.get("repository_root", "/")).resolve()
    in_repository = []
    for package in packages:
        manifest = package.get("manifest_path")
        if not isinstance(manifest, str) or not isinstance(package.get("name"), str):
            raise ValueError("invalid graph package")
        package_root = Path(manifest).resolve().parent
        if _owned_path(package_root, repository_root):
            in_repository.append((package, package_root))
    names = {package["name"] for package, _ in in_repository}
    direct = set()
    for changed in changed_paths:
        changed_path = Path(changed)
        absolute = (repository_root / changed_path).resolve() if not changed_path.is_absolute() else changed_path.resolve()
        candidates = [(package, package_root) for package, package_root in in_repository
                      if _owned_path(absolute, package_root)]
        if candidates:
            package, _ = sorted(
                candidates,
                key=lambda item: (-len(item[1].parts), item[0]["name"], item[0].get("id", "")),
            )[0]
            direct.add(package["name"])
    direct = sorted(direct)
    reverse = {name: set() for name in names}
    for package, dependencies in graph.get("edges", {}).items():
        for dependency in dependencies:
            if package in names and dependency in names:
                reverse[dependency].add(package)
    dependents = set()
    frontier = list(direct)
    while frontier:
        package = frontier.pop()
        for dependent in sorted(reverse.get(package, ())):
            if dependent not in dependents:
                dependents.add(dependent)
                frontier.append(dependent)
    reverse_dependents = sorted(dependents)
    return {"directly_changed": direct, "reverse_dependents": reverse_dependents,
            "full_invalidation": direct + [name for name in reverse_dependents if name not in direct]}


def invalidation_probes(graph: dict) -> dict:
    """Return the frozen edit probes used by every baseline dossier."""
    paths = {
        "protocol": ["tools/digest/packages/protocol/src/lib.rs"],
        "observer": ["tools/digest/packages/census-publication/src/main.rs"],
        "lab": ["windows/lab/src/lib.rs"],
        "unrelated": ["book/src/frontier/idea-registry.md"],
    }
    return {
        name: {"changed_paths": changed, "closure": changed_closure(graph, changed)}
        for name, changed in paths.items()
    }


def summarize_baseline(attempts: list[dict]) -> dict:
    """Summarize one cold/warm pair for every represented frozen workload."""
    if not isinstance(attempts, list) or not attempts:
        raise ValueError("baseline attempts are required")
    by_workload = {}
    excluded = 0
    for attempt in attempts:
        try:
            validate_attempt(attempt)
        except (TypeError, ValueError):
            excluded += 1
            continue
        classification = attempt.get("target", {}).get("classification")
        workload_id = attempt.get("workload_id")
        if classification not in {"cold", "warm"} or not isinstance(workload_id, str) or not workload_id:
            raise ValueError("baseline attempts must be cold or warm")
        costs = attempt.get("costs")
        if (not isinstance(costs, dict) or
                any(not _finite_number(costs.get(field))
                    for field in ("preparation_s", "build_s", "test_s"))):
            raise ValueError("incomplete baseline attempt")
        records = by_workload.setdefault(workload_id, {})
        if classification in records:
            raise ValueError("duplicate baseline workload classification")
        records[classification] = attempt
    frozen_ids = {workload["id"] for workload in load_workloads(WORKLOADS_PATH)["workloads"]}
    if set(by_workload) != frozen_ids:
        raise ValueError("baseline must contain complete frozen workload set")
    if any(set(records) != {"cold", "warm"} for records in by_workload.values()):
        raise ValueError("baseline attempts must be paired")
    eligible = [attempt for records in by_workload.values() for attempt in records.values()]
    graph_counts = {}
    graph_identities = [attempt.get("graph", {}).get("sha256") for attempt in eligible]
    if any(not _sha256(value) for value in graph_identities) or len(set(graph_identities)) != 1:
        raise ValueError("baseline graph identity differs")
    for field in ("package_count", "workspace_member_count"):
        values = [attempt.get("graph", {}).get(field) for attempt in eligible]
        if any(not isinstance(value, int) or isinstance(value, bool) or value < 0 for value in values):
            raise ValueError("incomplete baseline graph")
        if len(set(values)) != 1:
            raise ValueError("baseline graph counts differ")
        graph_counts[field] = values[:1]
    costs = {}
    for workload_id, records in by_workload.items():
        costs[workload_id] = {}
        for classification, record in records.items():
            timing = record["costs"]
            costs[workload_id][classification] = {
                field: float(timing[field]) for field in ("preparation_s", "build_s", "test_s")
            }
            costs[workload_id][classification]["total_s"] = sum(costs[workload_id][classification].values())
    return {"pair_count": len(by_workload), "excluded_attempt_count": excluded,
            "graph_identity": graph_identities[0], "graph_counts": graph_counts, "costs": costs}


def _git_text(root: Path, *args: str) -> str:
    completed = subprocess.run(
        ["git", "-C", str(root), *args], cwd=root,
        env=_measurement.controlled_env(), stdin=subprocess.DEVNULL,
        stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=False,
    )
    if completed.returncode != 0:
        raise ValueError(f"git {' '.join(args)} failed: {completed.stderr.decode(errors='replace').strip()}")
    return completed.stdout.decode("utf-8").strip()


def _source_identity(root: Path, comparison_ref: str | None = None) -> dict:
    if comparison_ref is None:
        try:
            _git_text(root, "rev-parse", "--verify", "origin/main")
            comparison_ref = "origin/main"
        except ValueError:
            comparison_ref = "main"
    if not isinstance(comparison_ref, str) or not comparison_ref or comparison_ref.startswith("-"):
        raise ValueError("comparison ref must be a safe non-empty ref")
    return {
        "commit": _git_text(root, "rev-parse", "HEAD"),
        "tree": _git_text(root, "rev-parse", "HEAD^{tree}"),
        "merge_base": _git_text(root, "merge-base", "HEAD", comparison_ref),
        "comparison_ref": comparison_ref,
    }


def _toolchain_identity(host_class: str) -> dict:
    rustc = subprocess.run(
        ["rustc", "--version"], stdin=subprocess.DEVNULL,
        stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=False,
    )
    if rustc.returncode != 0:
        raise ValueError("rustc --version failed")
    return {"rustc": rustc.stdout.decode("utf-8").strip(), "host_class": host_class}


def _output_records(checkout: Path, workload: dict) -> list[dict]:
    records = []
    for expected in workload["expected_outputs"]:
        path = Path(expected["path"])
        if path.is_absolute() or ".." in path.parts:
            raise ValueError("workload output path must be relative and safe")
        absolute = (checkout / path).resolve()
        if not _owned_path(absolute, checkout) or not absolute.is_file():
            raise ValueError(f"expected workload output is missing: {path}")
        raw = absolute.read_bytes()
        records.append({"path": path.as_posix(), "bytes": len(raw), "sha256": sha256(raw)})
    return records


def _measure_workload_phase(workload: dict, phase: str, checkout: Path,
                            target: Path, evidence_root: Path) -> dict:
    """Measure a declared phase and retain its bounded execution evidence."""
    command = _phase_command(workload, phase, checkout)
    result = _bounded_measure(
        command, checkout, DEFAULT_TIMEOUT,
        [target, evidence_root],
    )
    return {
        "phase": phase,
        "workload_id": workload["id"],
        "workload_command_template": workload["phases"][phase]["command"],
        "command": command,
        "exit_code": result.get("exit_code"),
        "launch_error": result.get("launch_error"),
        "elapsed_s": result.get("elapsed_seconds"),
        "deadline_s": DEFAULT_TIMEOUT,
        "deadline_exceeded": result.get("harness_deadline_exceeded"),
        "output_limit_exceeded": result.get("output_limit_exceeded"),
        "cleanup": {
            "complete": result.get("cleanup_error") is None,
            "error": result.get("cleanup_error"),
        },
        "stdout": _stream(result.get("stdout", b"")),
        "stderr": _stream(result.get("stderr", b"")),
        "enforcement_method": result.get("enforcement_method"),
    }


def _phase_succeeded(record: dict) -> bool:
    return (record.get("launch_error") is None
            and record.get("exit_code") == 0
            and record.get("deadline_exceeded") is False
            and record.get("output_limit_exceeded") is False
            and record.get("cleanup", {}).get("complete") is True)


def _phase_record(value: dict | float, workload: dict, phase: str,
                  checkout: Path) -> dict:
    """Normalize legacy test doubles while keeping real phase records structured."""
    if isinstance(value, dict):
        return value
    return {
        "phase": phase, "workload_id": workload["id"],
        "workload_command_template": workload["phases"][phase]["command"],
        "command": _phase_command(workload, phase, checkout),
        "exit_code": 0, "launch_error": None, "elapsed_s": float(value),
        "deadline_s": DEFAULT_TIMEOUT, "deadline_exceeded": False,
        "output_limit_exceeded": False,
        "cleanup": {"complete": True, "error": None},
        "stdout": _stream(b""), "stderr": _stream(b""),
        "enforcement_method": "mock",
    }


def run_baseline(root: Path, output: Path, host_class: str, cold: bool) -> dict:
    """Capture every frozen workload for one cold or warm baseline class."""
    root, output = Path(root), Path(output)
    if not root.is_absolute() or not root.is_dir():
        raise ValueError("baseline root must be an existing absolute directory")
    if not output.is_absolute() or output.exists():
        raise ValueError("baseline output must be a new absolute path")
    if not isinstance(host_class, str) or not host_class:
        raise ValueError("baseline host class is required")
    classification = "cold" if cold else "warm"
    workloads = load_workloads(WORKLOADS_PATH)["workloads"]
    target = (root / "tools" / "digest" / "target").resolve()
    _prepare_baseline_target(root, target, cold)
    evidence = (output.parent / "attempts" / classification).resolve()
    graph = cargo_graph(root / "tools/digest/Cargo.toml", target)
    source = _source_identity(root)
    toolchain = _toolchain_identity(host_class)
    attempts = []
    raw_attempts = []
    for workload in workloads:
        destination = evidence / f"{workload['id']}.capture.json"
        captured = None
        try:
            preparation = _phase_record(
                _measure_workload_phase(workload, "preparation", root, target, evidence),
                workload, "preparation", root,
            )
            if not _phase_succeeded(preparation):
                raw_attempts.append({
                    "workload_id": workload["id"], "status": "invalid",
                    "phase": preparation,
                    "error": "preparation phase failed",
                })
                continue
            captured = capture(workload["id"], root, target, evidence, destination)
            raw_attempt = {"workload_id": workload["id"], "capture": captured, "status": "captured"}
            test = _phase_record(
                _measure_workload_phase(workload, "test", root, target, evidence),
                workload, "test", root,
            )
            if not _phase_succeeded(test):
                raw_attempt["status"] = "invalid"
                raw_attempt["phase"] = test
                raw_attempt["error"] = "test phase failed"
                raw_attempts.append(raw_attempt)
                continue
            costs = {"preparation_s": float(preparation["elapsed_s"]), "build_s": float(captured["elapsed_s"]), "test_s": float(test["elapsed_s"])}
            failure = None if captured.get("exit_code") == 0 else {
                "reason": "measurement command failed",
                "valid_evidence": True,
            }
            attempt = manifest_for_attempt(
                source=source,
                graph={"sha256": graph["identity"]["sha256"], "package_count": graph["package_count"], "workspace_member_count": graph["workspace_member_count"]},
                toolchain=toolchain,
                target={"path": str(target), "classification": classification},
                workload_id=workload["id"], capture=captured, costs=costs,
                outputs=_output_records(root, workload), failure=failure,
            )
            raw_attempt["status"] = "valid"
            raw_attempts.append(raw_attempt)
            attempts.append(attempt)
        except Exception as error:
            raw_attempts.append({
                "workload_id": workload["id"], "status": "invalid",
                "error": f"{type(error).__name__}: {error}"[:2048],
                "capture": captured,
            })
    dossier = {
        "schema": "insulator-baseline-v1", "classification": classification,
        "host_class": host_class, "source": source, "graph": graph,
        "attempts": attempts,
        "raw_attempts": raw_attempts,
        "invalidation_probes": invalidation_probes(graph),
    }
    output.parent.mkdir(parents=True, exist_ok=True)
    temporary = output.with_name(f".{output.name}.tmp")
    with temporary.open("x", encoding="utf-8") as stream:
        json.dump(dossier, stream, indent=2, sort_keys=True)
        stream.write("\n")
    os.replace(temporary, output)
    return dossier


def _prepare_baseline_target(root: Path, target: Path, cold: bool) -> None:
    """Establish exclusive ownership before changing the baseline target."""
    expected = (Path(root) / "tools" / "digest" / "target").resolve()
    if target != expected or target.is_symlink():
        raise ValueError("baseline target ownership cannot be established")
    if target.exists() and not target.is_dir():
        raise ValueError("baseline target must be a directory")
    if cold and target.exists():
        shutil.rmtree(target)
    target.mkdir(parents=True, exist_ok=True)


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


def _covers_checkout(path: Path, checkout: Path) -> bool:
    """Return whether a writable root includes the checkout itself."""
    return _owned_path(checkout, path)


def _sandbox_profile(roots: list[Path]) -> str:
    # Rust's Unix runtime reads sysctl state while creating its stack guard
    # page.  Without this narrow read permission sandbox-exec aborts before
    # Cargo can start, reporting EINVAL from stack_overflow.rs.
    lines = [
        "(version 1)", "(deny default)", "(allow process*)",
        "(allow file-read*)", "(allow sysctl-read)",
    ]
    lines.extend(f'(allow file-write* (subpath "{root}"))' for root in roots)
    return "\n".join(lines) + "\n"


def _enforced_command(command: list[str], cwd: Path, writable_roots: list[Path]):
    """Return a command with host-enforced writes restricted to writable_roots."""
    system = platform.system()
    if system == "Darwin":
        executable = shutil.which("sandbox-exec")
        if executable is None:
            raise EnforcementUnavailable("macOS capture requires sandbox-exec")
        profile = tempfile.NamedTemporaryFile(
            "w", encoding="utf-8", prefix="insulator-sandbox-", suffix=".sb",
            delete=False,
        )
        try:
            profile.write(_sandbox_profile(writable_roots))
            profile.close()
        except BaseException:
            profile.close()
            Path(profile.name).unlink(missing_ok=True)
            raise
        return [executable, "-f", profile.name, "--", *command], Path(profile.name), "sandbox-exec"
    if system == "Linux":
        executable = shutil.which("bwrap")
        if executable is None:
            raise EnforcementUnavailable("Linux capture requires bubblewrap (bwrap)")
        wrapped = [executable, "--die-with-parent", "--ro-bind", "/", "/"]
        wrapped.extend(["--dev", "/dev", "--proc", "/proc"])
        for root in writable_roots:
            root.mkdir(parents=True, exist_ok=True)
            wrapped.extend(["--bind", str(root), str(root)])
        wrapped.extend(["--chdir", str(cwd), "--", *command])
        return wrapped, None, "bwrap"
    raise EnforcementUnavailable(
        f"{system or 'unknown'} capture has no supported filesystem sandbox"
    )


def _bounded_measure(command: list[str], cwd: Path, timeout_s: int,
                     writable_roots: list[Path]) -> dict:
    """Run a command with a hard per-stream retention cap."""
    started = time.monotonic()
    process = None
    stdout = bytearray()
    stderr = bytearray()
    output_limit_exceeded = False
    deadline_exceeded = False
    launch_error = None
    cleanup_error = None
    profile_path = None
    enforcement_method = None
    try:
        command, profile_path, enforcement_method = _enforced_command(command, cwd, writable_roots)
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
        if profile_path is not None:
            profile_path.unlink(missing_ok=True)
    return {
        "exit_code": process.returncode if process is not None else None,
        "elapsed_seconds": time.monotonic() - started,
        "cleanup_error": cleanup_error,
        "interrupted": False,
        "harness_deadline_exceeded": deadline_exceeded,
        "output_limit_exceeded": output_limit_exceeded,
        "launch_error": launch_error,
        "enforcement_method": enforcement_method if process is not None else None,
        "stdout": bytes(stdout),
        "stderr": bytes(stderr),
    }


def capture(workload_id: str, checkout: Path, target: Path,
            evidence_root: Path, destination: Path, *,
            timeout_s: int = DEFAULT_TIMEOUT) -> dict:
    """Run one frozen workload in a bounded, owned measurement cell."""
    workload = _workload(workload_id)
    checkout = Path(checkout)
    target = Path(target)
    evidence_root = Path(evidence_root)
    destination = Path(destination)
    if not checkout.is_absolute() or not checkout.is_dir():
        raise ValueError("checkout must be an existing absolute directory")
    if not target.is_absolute():
        raise ValueError("target must be absolute")
    if not evidence_root.is_absolute():
        raise ValueError("evidence root must be absolute")
    if not destination.is_absolute():
        raise ValueError("evidence destination must be absolute")
    checkout = checkout.resolve()
    target = target.resolve()
    evidence_root = evidence_root.resolve()
    destination = destination.resolve()
    if not _owned_path(target, checkout):
        raise ValueError("target must be owned by the checkout")
    if _covers_checkout(target, checkout):
        raise ValueError("target must not overlap the checkout root")
    if _covers_checkout(evidence_root, checkout):
        raise ValueError("evidence root must not overlap the checkout root")
    if not _owned_path(destination, evidence_root):
        raise ValueError("evidence destination must be owned")
    if destination.exists():
        raise FileExistsError(destination)
    target.mkdir(parents=True, exist_ok=True)
    destination.parent.mkdir(parents=True, exist_ok=True)
    command = command_for_workload(workload, checkout)
    result = _bounded_measure(
        command, checkout, timeout_s,
        [target, evidence_root],
    )
    record = {
        "workload_id": workload_id,
        "workload_command_template": workload["command"],
        "command": command,
        "cwd": str(checkout),
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
        "enforcement_method": result.get("enforcement_method"),
        "launch_error": result.get("launch_error"),
        "retained_sample_directory": result.get("retained_sample_directory"),
        "ownership": {
            "checkout": str(checkout),
            "target": str(target),
            "evidence_root": str(evidence_root),
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
                         target: dict, workload_id: str, capture: dict,
                         costs: dict, outputs: list[dict],
                         failure: dict | None = None) -> dict:
    record = {
        "schema": "insulator-attempt-v1",
        "source": source,
        "graph": graph,
        "toolchain": toolchain,
        "target": target,
        "workload_id": workload_id,
        "workload_command_template": _workload(workload_id)["command"],
        "command": capture.get("command"),
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
    workload_id = record.get("workload_id")
    if not isinstance(workload_id, str) or not workload_id:
        raise ValueError("missing workload provenance")
    workload = _workload(workload_id)
    template = record.get("workload_command_template")
    if template != workload["command"]:
        raise ValueError("invalid workload command template")
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
            or _covers_checkout(Path(ownership["target"]), Path(ownership["checkout"]))
            or _covers_checkout(Path(ownership["evidence_root"]), Path(ownership["checkout"]))
            or not _owned_path(Path(ownership["evidence_destination"]), Path(ownership["evidence_root"]))
            or not isinstance(target.get("path"), str)
            or not Path(target["path"]).is_absolute()
            or Path(target["path"]).resolve() != Path(ownership["target"]).resolve()):
        raise ValueError("invalid ownership metadata")
    if command != command_for_workload(workload, Path(ownership["checkout"])):
        raise ValueError("command does not match workload")
    if captured.get("workload_id") != workload_id:
        raise ValueError("capture workload provenance mismatch")
    if captured.get("workload_command_template") != template:
        raise ValueError("capture workload template mismatch")
    if captured.get("enforcement_method") not in SUPPORTED_ENFORCEMENT_METHODS:
        raise ValueError("missing enforcement method")
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
