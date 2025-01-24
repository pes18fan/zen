import os
import subprocess
import platform

COL_RED = "\033[31m"
COL_GREEN = "\033[32m"
RESET = "\033[0m"
TEXT_BOLD = "\033[1m"


class OS:
    @staticmethod
    def is_windows():
        return platform.system().lower() == "windows"

    @staticmethod
    def is_mac():
        return platform.system().lower() == "darwin"

    @staticmethod
    def is_unix():
        return not OS.is_windows()

    @staticmethod
    def is_linux():
        return OS.is_unix() and not OS.is_mac()


test_folder = "__tests__"
compiler = "../bin/test/zen.exe" if OS.is_windows() else "../bin/test/zen"
tests = 0
passed = 0
failed = 0


def print_header():
    print(f"{COL_RED}ZEN{RESET} {COL_GREEN}TESTER{RESET}\n")


def test(folder):
    global tests, passed, failed
    print(f"Now testing in directory {TEXT_BOLD}{folder}{RESET}...\n")

    for file in os.listdir(folder):
        file_path = os.path.join(folder, file)

        if file in [".", ".."]:
            continue

        if os.path.isdir(file_path):
            print("\n")
            test(file_path)
        elif file_path.endswith(".zn"):
            expect, expected_err, wants_err, is_draft = read_expected_output(file_path)

            if is_draft:
                continue

            print(f"Testing {TEXT_BOLD}{file_path}{RESET}: ", end="")

            output, error, status = capture_output(f"{compiler} {file_path}")

            if status == 0:
                if multiline_output_match(output, expect):
                    print(f"{COL_GREEN}PASSED{RESET} with expected output")
                    passed += 1
                else:
                    print(f"{COL_RED}FAILED{RESET} with unexpected output")
                    print(f"Expected:\n{expect}")
                    print(f"Actual:\n{output}")
                    failed += 1
            else:
                if wants_err:
                    if expected_err in error.strip():
                        print(f"{COL_GREEN}PASSED{RESET} with expected error")
                        passed += 1
                    else:
                        print(f"{COL_RED}FAILED{RESET} with unexpected error")
                        print(f"Expected:\n{expected_err}")
                        print(f"Got:\n{error}")
                        failed += 1
                else:
                    print(f"{COL_RED}FAILED{RESET} with error")
                    print(error)
                    failed += 1

            tests += 1


def read_expected_output(path):
    expected_output = []
    expected_error = []
    wants_error = False
    is_draft = False

    with open(path, "r") as file:
        for line in file:
            if "// DRAFT" in line.strip():
                is_draft = True
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


def multiline_output_match(actual_output, expected_output):
    actual_lines = actual_output.strip().splitlines()
    expected_lines = expected_output.strip().splitlines()

    return (
        len(actual_lines) == len(expected_lines) and all(a.strip() == e.strip()
                                                         for a, e in zip(
            actual_lines, expected_lines
        ))
    )


def capture_output(command):
    try:
        result = subprocess.run(
            command, shell=True, text=True, capture_output=True
        )
        return result.stdout, result.stderr, result.returncode
    except Exception as e:
        return "", str(e), 1


if __name__ == "__main__":
    print_header()

    try:
        test(test_folder)
    except KeyboardInterrupt:
        exit(0)

    print("\n")
    print(f"Total tests: {tests}")
    if failed > 0:
        print(f"{COL_RED}FAILED{RESET}: {failed} tests failed.")
    elif passed == tests:
        print(f"All tests {COL_GREEN}PASSED!{RESET} :)")
    else:
        print(f"{COL_RED}Something went wrong.{RESET}")
        print(f"{passed} tests passed, {failed} failed.")
