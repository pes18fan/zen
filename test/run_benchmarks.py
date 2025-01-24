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


bench_folder = "benchmark"
compiler = "../bin/rel/zen.exe" if OS.is_windows() else "../bin/rel/zen"
benchmarks = 0


def print_header():
    print(f"{COL_RED}ZEN{RESET} {COL_GREEN}BENCHMARKER{RESET}\n")


def bench(folder):
    global benchmarks
    print(f"Running benchmarks in directory {TEXT_BOLD}{folder}{RESET}...\n")

    for file in os.listdir(folder):
        file_path = os.path.join(folder, file)

        if file in [".", ".."]:
            continue

        if os.path.isdir(file_path):
            print("\n")
            bench(file_path)
        elif file_path.endswith(".zn"):
            is_draft = check_if_draft(file_path)

            if is_draft:
                continue

            print(f"Benchmarking {TEXT_BOLD}{file_path}{RESET}: ", end="")

            time, error, status = capture_output(f"{compiler} {file_path}")

            if status != 0:  # Handle errors
                print(f"{COL_RED}ERROR{RESET} while benchmarking:")
                print(error)
            else:  # Benchmarking succeeded
                print(f"Got time: {COL_GREEN}{time}{RESET}")
                benchmarks += 1


def check_if_draft(path):
    with open(path, "r") as file:
        for line in file:
            if line.strip() == "// DRAFT":
                return True
    return False


def capture_output(command):
    try:
        result = subprocess.run(
            command, shell=True, text=True, capture_output=True
        )
        output = result.stdout.strip()
        error = result.stderr.strip()
        exit_status = result.returncode

        time = output.split("\n")[-1] if output else "Unknown"
        return time, error, exit_status
    except Exception as e:
        return "Unknown", str(e), 1


if __name__ == "__main__":
    print_header()

    try:
        bench(bench_folder)
    except KeyboardInterrupt:
        print("\nBenchmark interrupted.")
        exit(0)

    print("")
    print(f"Successfully benchmarked {benchmarks} files.")
