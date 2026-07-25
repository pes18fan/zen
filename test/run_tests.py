import os
import subprocess
import platform
import argparse

COL_RED = "\033[31m"
COL_GREEN = "\033[32m"
COL_YELLOW = "\033[33m"
RESET = "\033[0m"
TEXT_BOLD = "\033[1m"

LEAK_MARKERS = [
    "allocations not freed",
    "incorrect frees",
]

STRICT = False


class OS:
    @staticmethod
    def is_windows() -> bool:
        return platform.system().lower() == "windows"

    @staticmethod
    def is_mac() -> bool:
        return platform.system().lower() == "darwin"

    @staticmethod
    def is_unix() -> bool:
        return not OS.is_windows()

    @staticmethod
    def is_linux() -> bool:
        return OS.is_unix() and not OS.is_mac()


test_folder = "__tests__"
interpreter = "../bin/test/zen.exe" if OS.is_windows() else "../bin/test/zen"
tests = 0
passed = 0
failures = []
draft_paths = []

if not os.path.exists(interpreter):
    print(f"{COL_RED}ERROR:{RESET} interpreter not found in {interpreter}")
    exit(1)


def print_header() -> None:
    print(f"{COL_RED}ZEN{RESET} {COL_GREEN}TESTER{RESET}\n")


def test(folder: str) -> None:
    global tests, passed
    print(f"Now testing in directory {TEXT_BOLD}{folder}{RESET}...\n")

    for file in os.listdir(folder):
        file_path = os.path.join(folder, file)

        if file in [".", ".."]:
            continue

        if os.path.isdir(file_path):
            print("\n")
            test(file_path)
        elif file_path.endswith(".zn"):
            expect, expected_err, wants_err, is_draft = read_expected_output(
                file_path)

            if is_draft:
                continue

            print(f"Testing {TEXT_BOLD}{file_path}{RESET}: ", end="")

            output, error, status, timed_out = capture_output(
                f"{interpreter} {file_path}")

            has_leak = False
            leak_info = ""
            if STRICT:
                has_leak, leak_info = check_for_leaks(error)

            if has_leak:
                print(f"{COL_RED}FAILED{RESET} with memory leak")
                print(leak_info)
                failures.append({"path": file_path, "reason": "memory leak"})
            elif status == 0:
                if wants_err:
                    print(f"{COL_RED}FAILED{RESET} with unexpected success")
                    print(f"Expected error:\n{expected_err}")
                    print(f"Got:\n{output}")
                    failures.append(
                        {"path": file_path, "reason": "unexpected success"})
                elif multiline_output_match(output, expect):
                    print(f"{COL_GREEN}PASSED{RESET} with expected output")
                    passed += 1
                else:
                    print(f"{COL_RED}FAILED{RESET} with unexpected output")
                    print(f"Expected:\n{expect}")
                    print(f"Actual:\n{output}")
                    failures.append(
                        {"path": file_path, "reason": "unexpected output"})
            else:
                if timed_out:
                    print(f"{COL_RED}FAILED{RESET} with timeout")
                    print("Timeout expired after 2 seconds, likely infinite loop")
                    failures.append({"path": file_path, "reason": "timeout"})
                    continue

                if wants_err:
                    if expected_err in error.strip():
                        print(f"{COL_GREEN}PASSED{RESET} with expected error")
                        passed += 1
                    else:
                        print(f"{COL_RED}FAILED{RESET} with unexpected error")
                        print(f"Expected:\n{expected_err}")
                        print(f"Got:\n{error}")
                        failures.append(
                            {"path": file_path, "reason": "unexpected error"})
                else:
                    print(f"{COL_RED}FAILED{RESET} with error")
                    print(error)
                    failures.append(
                        {"path": file_path, "reason": "unexpected error"})

            tests += 1


def read_expected_output(path: str) -> tuple[str, str, bool, bool]:
    expected_output = []
    expected_error = []
    wants_error = False
    is_draft = False

    with open(path, "r") as file:
        for line in file:
            if "// DRAFT" in line.strip():
                is_draft = True
                draft_paths.append(path)
                break

            if "// expect:" in line.strip():
                expected_output.append(line.strip().split("// expect:")[1])
            elif "// ERR:" in line.strip():
                wants_error = True
                expected_error.append(line.strip().split("// ERR:")[1])

    return (
        "\n".join(expected_output).strip(),
        "\n".join(expected_error).strip(),
        wants_error,
        is_draft,
    )


def multiline_output_match(actual_output: str, expected_output: str) -> bool:
    actual_lines = actual_output.strip().splitlines()
    expected_lines = expected_output.strip().splitlines()

    return (
        len(actual_lines) == len(expected_lines) and all(a.strip() == e.strip()
                                                         for a, e in zip(
            actual_lines, expected_lines
        ))
    )


def check_for_leaks(stderr: str) -> tuple[bool, str]:
    for marker in LEAK_MARKERS:
        if marker in stderr:
            return True, stderr
    return False, ""


# return values: stdout, stderr, returncode, timeout
def capture_output(command: str) -> tuple[str, str, int, bool]:
    try:
        result = subprocess.run(
            command, shell=True, text=True, capture_output=True, timeout=2
        )
        err = result.stderr.splitlines()
        return result.stdout, "" if err == [] else err[0], result.returncode, False
    except subprocess.TimeoutExpired:
        return "", "", 1, True
    except Exception as e:
        return "", str(e), 1, False


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="zen e2e test runner")
    parser.add_argument(
        "--strict", "-s", action="store_true",
        help="enable memory leak detection (requires debug build)",
    )
    parser.add_argument(
        "--dir", "-d", type=str, default=test_folder,
        help="test directory relative to test/ (default: __tests__)",
    )
    args = parser.parse_args()
    STRICT = args.strict
    test_folder = args.dir

    print_header()

    try:
        test(test_folder)
    except KeyboardInterrupt:
        exit(0)

    print()
    print(f"Total tests run: {tests}")

    if len(draft_paths) > 0:
        print("Draft tests not run:")
        for path in draft_paths:
            print(f"\t{path}")

    print()
    if len(failures) > 0:
        print(f"{COL_RED}FAILED{RESET}: {len(failures)} tests failed.")
        print("Failed tests:")
        for failure in failures:
            print(f"\t{failure['path']} ({failure['reason']})")
    elif passed == tests:
        print(f"All tests {COL_GREEN}PASSED!{RESET} :)")
    else:
        print(f"{COL_RED}Something went wrong.{RESET}")
        print(f"{passed} tests passed, {len(failures)} failed.")
