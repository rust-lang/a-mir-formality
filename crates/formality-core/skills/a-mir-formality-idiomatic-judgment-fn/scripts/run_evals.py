#!/usr/bin/env python3
"""Run Agent Skills evals with and without a skill using the Codex CLI."""

from __future__ import annotations

import argparse
import json
import shlex
import shutil
import statistics
import subprocess
import sys
import tempfile
import time
from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional, Sequence, Tuple


GRADING_SCHEMA: Dict[str, Any] = {
    "type": "object",
    "properties": {
        "assertion_results": {
            "type": "array",
            "items": {
                "type": "object",
                "properties": {
                    "text": {"type": "string"},
                    "passed": {"type": "boolean"},
                    "evidence": {"type": "string"},
                },
                "required": ["text", "passed", "evidence"],
                "additionalProperties": False,
            },
        },
        "summary": {
            "type": "object",
            "properties": {
                "passed": {"type": "integer"},
                "failed": {"type": "integer"},
                "total": {"type": "integer"},
                "pass_rate": {"type": "number"},
            },
            "required": ["passed", "failed", "total", "pass_rate"],
            "additionalProperties": False,
        },
    },
    "required": ["assertion_results", "summary"],
    "additionalProperties": False,
}


def parse_args(argv: Optional[Sequence[str]] = None) -> argparse.Namespace:
    script_path = Path(__file__).resolve()
    skill_dir = script_path.parents[1]
    parser = argparse.ArgumentParser(
        description=(
            "Run each eval in a clean Codex context with and without the skill. "
            "Results follow the iteration-N layout from agentskills.io."
        )
    )
    parser.add_argument(
        "--skill-dir",
        type=Path,
        default=skill_dir,
        help="Skill directory (default: directory containing this script).",
    )
    parser.add_argument(
        "--evals",
        type=Path,
        help="Path to evals.json (default: SKILL_DIR/evals/evals.json).",
    )
    parser.add_argument(
        "--workspace",
        type=Path,
        help=(
            "Result workspace (default: a SKILL_NAME-workspace directory under the "
            "system temporary directory, outside the source repository)."
        ),
    )
    parser.add_argument(
        "--formality-source",
        type=Path,
        help=(
            "Directory containing formality-core and formality-macros source directories "
            "for formality-core starter fixtures (default: this repository's crates directory)."
        ),
    )
    parser.add_argument(
        "--iteration",
        type=int,
        help="Iteration number. By default, choose the next unused number.",
    )
    parser.add_argument(
        "--eval-id",
        action="append",
        dest="eval_ids",
        help="Run only this eval id; repeat to select more than one.",
    )
    parser.add_argument(
        "--runs",
        type=int,
        default=1,
        help="Independent repetitions per eval and configuration (default: 1).",
    )
    parser.add_argument("--model", help="Codex model for generation runs.")
    parser.add_argument(
        "--grader-model",
        help="Codex model for grading runs (default: same as --model).",
    )
    parser.add_argument(
        "--context",
        action="append",
        default=[],
        help="Harness-only context supplied equally to both configurations; repeatable.",
    )
    parser.add_argument(
        "--grade",
        action="store_true",
        help="Grade assertions after each successful generation run.",
    )
    parser.add_argument(
        "--approval-mode",
        choices=["never", "auto"],
        default="never",
        help="Use no approvals or Codex automatic approval review (default: never).",
    )
    parser.add_argument(
        "--codex",
        default="codex",
        help="Codex CLI executable (default: codex).",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Validate inputs and print planned commands without creating results.",
    )
    parser.add_argument(
        "--fail-fast",
        action="store_true",
        help="Stop after the first failed generation or grading process.",
    )
    args = parser.parse_args(argv)
    if args.runs < 1:
        parser.error("--runs must be at least 1")
    if args.iteration is not None and args.iteration < 1:
        parser.error("--iteration must be at least 1")
    return args


def load_spec(path: Path) -> Dict[str, Any]:
    try:
        data = json.loads(path.read_text(encoding="utf-8"))
    except FileNotFoundError as error:
        raise ValueError(f"eval spec not found: {path}") from error
    except json.JSONDecodeError as error:
        raise ValueError(f"invalid JSON in {path}: {error}") from error

    if not isinstance(data, dict) or not isinstance(data.get("skill_name"), str):
        raise ValueError("eval spec must contain a string skill_name")
    evals = data.get("evals")
    if not isinstance(evals, list) or not evals:
        raise ValueError("eval spec must contain a non-empty evals array")

    seen = set()
    for index, evaluation in enumerate(evals):
        if not isinstance(evaluation, dict):
            raise ValueError(f"evals[{index}] must be an object")
        for key in ("id", "prompt", "expected_output"):
            if key not in evaluation:
                raise ValueError(f"evals[{index}] is missing {key}")
        eval_id = str(evaluation["id"])
        if eval_id in seen:
            raise ValueError(f"duplicate eval id: {eval_id}")
        seen.add(eval_id)
        if not isinstance(evaluation["prompt"], str):
            raise ValueError(f"eval {eval_id} prompt must be a string")
        if not isinstance(evaluation["expected_output"], str):
            raise ValueError(f"eval {eval_id} expected_output must be a string")
        for key in ("files", "assertions"):
            values = evaluation.get(key, [])
            if not isinstance(values, list) or not all(
                isinstance(value, str) for value in values
            ):
                raise ValueError(f"eval {eval_id} {key} must be an array of strings")
        starter = evaluation.get("starter")
        if starter not in (None, "formality-core"):
            raise ValueError(
                f"eval {eval_id} has unsupported starter {starter!r}; "
                "expected 'formality-core'"
            )
    return data


def next_iteration(workspace: Path) -> int:
    numbers = []
    if workspace.exists():
        for child in workspace.iterdir():
            if child.is_dir() and child.name.startswith("iteration-"):
                suffix = child.name[len("iteration-") :]
                if suffix.isdigit():
                    numbers.append(int(suffix))
    return max(numbers, default=0) + 1


def slug(value: Any) -> str:
    text = str(value)
    cleaned = "".join(character if character.isalnum() else "-" for character in text)
    return cleaned.strip("-") or "case"


def selected_evals(
    evaluations: Sequence[Dict[str, Any]], ids: Optional[Sequence[str]]
) -> List[Dict[str, Any]]:
    if not ids:
        return list(evaluations)
    wanted = set(ids)
    selected = [evaluation for evaluation in evaluations if str(evaluation["id"]) in wanted]
    missing = wanted - {str(evaluation["id"]) for evaluation in selected}
    if missing:
        raise ValueError(f"unknown eval id(s): {', '.join(sorted(missing))}")
    return selected


def copy_inputs(
    evaluation: Dict[str, Any], skill_dir: Path, destination: Path
) -> List[Path]:
    copied = []
    for file_name in evaluation.get("files", []):
        source = Path(file_name)
        if not source.is_absolute():
            source = skill_dir / source
            relative = Path(file_name)
            if ".." in relative.parts:
                raise ValueError(f"input path may not escape the skill directory: {file_name}")
        else:
            relative = Path(source.name)
        if not source.exists():
            raise ValueError(f"input file does not exist: {source}")
        target = destination / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        if source.is_dir():
            shutil.copytree(source, target)
        else:
            shutil.copy2(source, target)
        copied.append(target.resolve())
    return copied


def resolve_formality_source(skill_dir: Path, configured: Optional[Path]) -> Path:
    source = (configured or skill_dir.parents[1] / "crates").resolve()
    missing = [
        name
        for name in ("formality-core", "formality-macros")
        if not (source / name / "Cargo.toml").is_file()
    ]
    if missing:
        raise ValueError(
            f"formality source {source} is missing: {', '.join(missing)}; "
            "pass --formality-source with a directory containing both crates"
        )
    return source


def stage_formality_core_starter(
    evaluation: Dict[str, Any],
    source: Optional[Path],
    input_dir: Path,
    output_dir: Path,
) -> Optional[Path]:
    if evaluation.get("starter") is None:
        return None
    if source is None:
        raise ValueError("formality-core starter requested without a formality source")

    dependencies = input_dir / "dependencies"
    for crate_name in ("formality-core", "formality-macros"):
        shutil.copytree(source / crate_name, dependencies / crate_name)

    manifest = output_dir / "Cargo.toml"
    manifest.write_text(
        """[package]
name = "formality-skill-eval"
version = "0.1.0"
edition = "2021"

[dependencies]
formality-core = { path = "../inputs/dependencies/formality-core" }
""",
        encoding="utf-8",
    )
    return dependencies.resolve()


def snapshot_skill(skill_dir: Path, destination: Path) -> Path:
    destination.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(skill_dir / "SKILL.md", destination)
    return destination.resolve()


def generation_prompt(
    evaluation: Dict[str, Any],
    skill_path: Optional[Path],
    output_dir: Path,
    input_paths: Sequence[Path],
    contexts: Sequence[str],
    starter_dependencies: Optional[Path],
) -> str:
    lines = ["Execute this evaluation task in a clean context."]
    if skill_path is not None:
        lines.extend(
            [
                f"Read and follow the skill at {skill_path}.",
                "Treat the skill as instructions for how to perform the task.",
            ]
        )
    for context in contexts:
        lines.append(f"Harness context: {context}")
    if input_paths:
        lines.append("Input files:")
        lines.extend(f"- {path}" for path in input_paths)
    if starter_dependencies is not None:
        lines.extend(
            [
                f"A starter Rust crate has already been created at {output_dir}.",
                "Its Cargo.toml already configures formality-core as a local path dependency.",
                f"The staged dependency sources are at {starter_dependencies}.",
                "Use that dependency as configured; do not vendor, copy, or repoint it.",
            ]
        )
    lines.extend(
        [
            f"Create all requested artifacts under the current directory: {output_dir}",
            "Run appropriate verification before finishing.",
            "",
            "Task:",
            evaluation["prompt"],
        ]
    )
    return "\n".join(lines).rstrip() + "\n"


def codex_command(
    executable: str,
    working_dir: Path,
    final_path: Path,
    model: Optional[str],
    approval_mode: str,
    output_schema: Optional[Path] = None,
    read_only: bool = False,
) -> List[str]:
    # Approval policy is a top-level Codex option. Although `codex exec --help`
    # currently displays it, the CLI rejects it after the `exec` subcommand.
    command = [executable]
    if approval_mode == "auto" and not read_only:
        command.append("--approve-for-me")
    else:
        command.extend(["--ask-for-approval", "never"])
    command.extend(
        [
            "exec",
            "--ephemeral",
            "--ignore-user-config",
            "--ignore-rules",
            "--skip-git-repo-check",
            "--sandbox",
            "read-only" if read_only else "workspace-write",
        ]
    )
    if model:
        command.extend(["--model", model])
    if output_schema is not None:
        command.extend(["--output-schema", str(output_schema)])
    command.extend(
        [
            "--json",
            "--cd",
            str(working_dir),
            "--output-last-message",
            str(final_path),
            "-",
        ]
    )
    return command


def extract_total_tokens(transcript: Path) -> Optional[int]:
    totals: List[int] = []
    input_tokens: List[int] = []
    output_tokens: List[int] = []

    def visit(value: Any) -> None:
        if isinstance(value, dict):
            for key, child in value.items():
                if isinstance(child, int) and not isinstance(child, bool):
                    if key == "total_tokens":
                        totals.append(child)
                    elif key == "input_tokens":
                        input_tokens.append(child)
                    elif key == "output_tokens":
                        output_tokens.append(child)
                visit(child)
        elif isinstance(value, list):
            for child in value:
                visit(child)

    if not transcript.exists():
        return None
    for line in transcript.read_text(encoding="utf-8", errors="replace").splitlines():
        try:
            visit(json.loads(line))
        except json.JSONDecodeError:
            continue
    if totals:
        return max(totals)
    if input_tokens or output_tokens:
        return max(input_tokens, default=0) + max(output_tokens, default=0)
    return None


def run_process(
    command: Sequence[str], prompt: str, transcript: Path, stderr_path: Path
) -> Dict[str, Any]:
    start = time.monotonic()
    with transcript.open("w", encoding="utf-8") as stdout_file, stderr_path.open(
        "w", encoding="utf-8"
    ) as stderr_file:
        completed = subprocess.run(
            command,
            input=prompt,
            text=True,
            stdout=stdout_file,
            stderr=stderr_file,
            check=False,
        )
    duration_ms = round((time.monotonic() - start) * 1000)
    return {
        "total_tokens": extract_total_tokens(transcript),
        "duration_ms": duration_ms,
        "exit_code": completed.returncode,
    }


def write_json(path: Path, value: Any) -> None:
    path.write_text(json.dumps(value, indent=2) + "\n", encoding="utf-8")


def grading_prompt(evaluation: Dict[str, Any], output_dir: Path) -> str:
    assertions = evaluation.get("assertions", [])
    return "\n".join(
        [
            "Grade the candidate artifacts against every assertion below.",
            f"Candidate artifacts: {output_dir}",
            "Inspect the actual files and cite concrete file paths, code, test output, or",
            "other observable evidence. Do not accept claims in the candidate's final message",
            "without corroboration. Mark an assertion PASS only when the evidence supports it.",
            "For conditional assertions, explain whether the condition applies.",
            "Return one assertion_results entry for every assertion, in the same order and",
            "using the assertion text verbatim. Compute the summary from those results.",
            "",
            "Expected output:",
            evaluation["expected_output"],
            "",
            "Assertions:",
            *[f"{index}. {assertion}" for index, assertion in enumerate(assertions, 1)],
        ]
    )


def validate_grading(path: Path, assertions: Sequence[str]) -> None:
    try:
        grading = json.loads(path.read_text(encoding="utf-8"))
    except (FileNotFoundError, json.JSONDecodeError) as error:
        raise ValueError(f"invalid grading output at {path}: {error}") from error
    results = grading.get("assertion_results", [])
    if len(results) != len(assertions):
        raise ValueError(
            f"grading at {path} returned {len(results)} results for {len(assertions)} assertions"
        )
    for index, (result, assertion) in enumerate(zip(results, assertions), 1):
        if result.get("text") != assertion:
            raise ValueError(f"grading result {index} did not preserve assertion text")


def mean_and_stddev(values: Iterable[float]) -> Dict[str, Optional[float]]:
    collected = list(values)
    if not collected:
        return {"mean": None, "stddev": None}
    return {
        "mean": statistics.mean(collected),
        "stddev": statistics.pstdev(collected),
    }


def build_benchmark(records: Sequence[Dict[str, Any]]) -> Dict[str, Any]:
    summary: Dict[str, Any] = {}
    for configuration in ("with_skill", "without_skill"):
        matching = [record for record in records if record["configuration"] == configuration]
        pass_rates = [
            record["pass_rate"]
            for record in matching
            if record.get("pass_rate") is not None
        ]
        durations = [record["duration_ms"] / 1000 for record in matching]
        tokens = [record["total_tokens"] for record in matching if record["total_tokens"] is not None]
        summary[configuration] = {
            "runs": len(matching),
            "successful_runs": sum(record["exit_code"] == 0 for record in matching),
            "pass_rate": mean_and_stddev(pass_rates),
            "time_seconds": mean_and_stddev(durations),
            "tokens": mean_and_stddev(tokens),
        }

    with_skill = summary["with_skill"]
    without_skill = summary["without_skill"]

    def delta(metric: str) -> Optional[float]:
        left = with_skill[metric]["mean"]
        right = without_skill[metric]["mean"]
        if left is None or right is None:
            return None
        return left - right

    summary["delta"] = {
        "pass_rate": delta("pass_rate"),
        "time_seconds": delta("time_seconds"),
        "tokens": delta("tokens"),
    }
    return {"run_summary": summary}


def describe(command: Sequence[str], destination: Path) -> None:
    print(f"$ {shlex.join(command)}")
    print(f"  transcript: {destination}")


def main(argv: Optional[Sequence[str]] = None) -> int:
    args = parse_args(argv)
    skill_dir = args.skill_dir.resolve()
    eval_path = (args.evals or skill_dir / "evals" / "evals.json").resolve()
    spec = load_spec(eval_path)
    if spec["skill_name"] != skill_dir.name:
        raise ValueError(
            f"skill_name {spec['skill_name']!r} does not match directory {skill_dir.name!r}"
        )
    evaluations = selected_evals(spec["evals"], args.eval_ids)
    workspace = (
        args.workspace
        or Path(tempfile.gettempdir()) / f"{spec['skill_name']}-workspace"
    ).resolve()
    formality_source = None
    if any(evaluation.get("starter") == "formality-core" for evaluation in evaluations):
        formality_source = resolve_formality_source(skill_dir, args.formality_source)
    iteration_number = args.iteration or next_iteration(workspace)
    iteration_dir = workspace / f"iteration-{iteration_number}"
    if iteration_dir.exists():
        raise ValueError(f"iteration already exists: {iteration_dir}")

    plans: List[Tuple[Dict[str, Any], int, str, Path]] = []
    for evaluation in evaluations:
        for run_number in range(1, args.runs + 1):
            suffix = f"-run-{run_number}" if args.runs > 1 else ""
            eval_dir = iteration_dir / f"eval-{slug(evaluation['id'])}{suffix}"
            for configuration in ("with_skill", "without_skill"):
                plans.append((evaluation, run_number, configuration, eval_dir / configuration))

    if args.dry_run:
        for evaluation, _, configuration, config_dir in plans:
            output_dir = config_dir / "outputs"
            command = codex_command(
                args.codex,
                output_dir,
                config_dir / "final.txt",
                args.model,
                args.approval_mode,
            )
            describe(command, config_dir / "transcript.jsonl")
            print(f"  eval: {evaluation['id']} ({configuration})")
            if evaluation.get("starter") == "formality-core":
                print(f"  formality source: {formality_source}")
            if configuration == "with_skill":
                print(f"  skill snapshot: {config_dir / 'instructions' / 'SKILL.md'}")
        return 0

    iteration_dir.mkdir(parents=True)
    schema_path = iteration_dir / "grading-schema.json"
    if args.grade:
        write_json(schema_path, GRADING_SCHEMA)

    records: List[Dict[str, Any]] = []
    had_failure = False
    for evaluation, _, configuration, config_dir in plans:
        print(f"Running eval {evaluation['id']} ({configuration})", flush=True)
        output_dir = config_dir / "outputs"
        input_dir = config_dir / "inputs"
        output_dir.mkdir(parents=True)
        input_dir.mkdir(parents=True)
        input_paths = copy_inputs(evaluation, skill_dir, input_dir)
        starter_dependencies = stage_formality_core_starter(
            evaluation,
            formality_source,
            input_dir,
            output_dir,
        )
        skill_path = None
        if configuration == "with_skill":
            skill_path = snapshot_skill(
                skill_dir, config_dir / "instructions" / "SKILL.md"
            )
        prompt = generation_prompt(
            evaluation,
            skill_path,
            output_dir,
            input_paths,
            args.context,
            starter_dependencies,
        )
        (config_dir / "prompt.txt").write_text(prompt, encoding="utf-8")
        command = codex_command(
            args.codex,
            output_dir,
            config_dir / "final.txt",
            args.model,
            args.approval_mode,
        )
        describe(command, config_dir / "transcript.jsonl")
        timing = run_process(
            command,
            prompt,
            config_dir / "transcript.jsonl",
            config_dir / "stderr.log",
        )
        write_json(config_dir / "timing.json", timing)
        record = {"configuration": configuration, **timing, "pass_rate": None}
        records.append(record)
        if timing["exit_code"] != 0:
            had_failure = True
            print(f"Generation failed; see {config_dir / 'stderr.log'}", file=sys.stderr)
            if args.fail_fast:
                break
            continue

        assertions = evaluation.get("assertions", [])
        if args.grade and assertions:
            print(f"Grading eval {evaluation['id']} ({configuration})", flush=True)
            grade_prompt = grading_prompt(evaluation, output_dir)
            (config_dir / "grading-prompt.txt").write_text(
                grade_prompt, encoding="utf-8"
            )
            grading_command = codex_command(
                args.codex,
                config_dir,
                config_dir / "grading.json",
                args.grader_model or args.model,
                "never",
                output_schema=schema_path,
                read_only=True,
            )
            describe(grading_command, config_dir / "grading-transcript.jsonl")
            grade_timing = run_process(
                grading_command,
                grade_prompt,
                config_dir / "grading-transcript.jsonl",
                config_dir / "grading-stderr.log",
            )
            write_json(config_dir / "grading-timing.json", grade_timing)
            if grade_timing["exit_code"] == 0:
                try:
                    validate_grading(config_dir / "grading.json", assertions)
                    grading = json.loads(
                        (config_dir / "grading.json").read_text(encoding="utf-8")
                    )
                    record["pass_rate"] = grading["summary"]["pass_rate"]
                except ValueError as error:
                    had_failure = True
                    print(error, file=sys.stderr)
            else:
                had_failure = True
                print(
                    f"Grading failed; see {config_dir / 'grading-stderr.log'}",
                    file=sys.stderr,
                )
                if args.fail_fast:
                    break

    write_json(iteration_dir / "benchmark.json", build_benchmark(records))
    print(f"Results: {iteration_dir}")
    return 1 if had_failure else 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (OSError, ValueError) as error:
        print(f"error: {error}", file=sys.stderr)
        sys.exit(2)
