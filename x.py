#!/usr/bin/env python
import argparse
import subprocess
import sys
import os
import shutil
import platform

OC = "odin"
ProcError = subprocess.CalledProcessError
DEBUG_FLAGS = "-debug -o:none"

OUT = "zen"
DBG_OUT = "dzen"

match platform.system():
    case "Windows":
        OUT = "zen.exe"
        DBG_OUT = "dzen.exe"
    case "Linux", "Darwin":
        pass


def create_debug_build():
    try:
        print("Compiling the debug build..")

        os.makedirs("bin/dbg", exist_ok=True)
        subprocess.run(
            f"{OC} build src/ -out:bin/dbg/{DBG_OUT} {DEBUG_FLAGS}".split(), check=True
        )
    except ProcError as e:
        print(f"Error while creating debug build: {e}", file=sys.stderr)
        exit(1)


def create_release_build():
    try:
        print("Compiling in release mode..")

        os.makedirs("bin/rel", exist_ok=True)
        os.makedirs("bin/test", exist_ok=True)
        subprocess.run(
            f"{OC} build src/ -out:bin/rel/{OUT} -o:speed".split(), check=True
        )
        shutil.copy(f"bin/rel/{OUT}", "bin/test/")
    except ProcError as e:
        print(f"Error while creating release build: {e}", file=sys.stderr)
        exit(1)


def benchmark():
    print("Starting up the benchmark runner..")
    try:
        subprocess.run(f"ruby ./run_benchmarks.rb".split(), cwd="test/", check=True)
    except FileNotFoundError:
        print("Ruby needed to run benchmarks!", file=sys.stderr)
        exit(1)
    except ProcError as e:
        print(f"Error while benchmarking: {e}", file=sys.stderr)
        exit(1)


def clean():
    shutil.rmtree("bin", ignore_errors=True)
    print("cleaned build artifacts.")


def generate_docs():
    os.makedirs("doc/", exist_ok=True)
    with open("doc/docs.txt", "w+") as doc_file:
        try:
            subprocess.run(f"{OC} doc src/".split(), stdout=doc_file, check=True)
        except ProcError as e:
            print(f"Error when generating docs: {e}", file=system)
            exit(1)

    print("docs generated at doc/docs.txt")


def test():
    print("Running unit tests:")
    try:
        subprocess.run(f"{OC} test ../../src/".split(), cwd="bin/test/", check=True)
    except ProcError as e:
        print(f"Error when running unit tests: {e}", file=sys.stderr)
        exit(1)

    print("")

    print("Running end-to-end tests:")
    try:
        subprocess.run(f"ruby ./run_tests.rb".split(), cwd="test/", check=True)
    except FileNotFoundError:
        print("Ruby needed to run the tests!", file=sys.stderr)
        exit(1)
    except ProcError as e:
        print(f"Error during e2e tests: {e}", file=sys.stderr)
        exit(1)


def run_build(args):
    try:
        subprocess.run([f"./bin/dbg/{DBG_OUT}"] + args.split(), check=True)
    except ProcError as e:
        print(f"Error running the build: {e}", file=sys.stderr)
        exit(1)


def main():
    try:
        subprocess.run(
            f"{OC} version".split(),
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            check=True,
        )
    except ProcError:
        print(f"You need Odin to build!")
        exit(1)

    parser = argparse.ArgumentParser(description="zen build system")
    subparsers = parser.add_subparsers(dest="command")

    # debug build
    dbg_parser = subparsers.add_parser("dbg", help="create a debug build")
    dbg_parser.set_defaults(func=create_debug_build)

    # release build
    rel_parser = subparsers.add_parser("rel", help="create a release build")
    rel_parser.set_defaults(func=create_release_build)

    # benchmark
    bench_parser = subparsers.add_parser("bench", help="run benchmarks")
    bench_parser.set_defaults(func=benchmark)

    # clean
    clean_parser = subparsers.add_parser("clean", help="clean build artifacts")
    clean_parser.set_defaults(func=clean)

    # doc generator
    doc_parser = subparsers.add_parser(
        "doc", help="generate documentation in the doc/ folder"
    )
    doc_parser.set_defaults(func=generate_docs)

    # Test all command
    test_parser = subparsers.add_parser("test", help="run all tests")
    test_parser.set_defaults(func=test)

    # Run program command
    run_parser = subparsers.add_parser("run", help="run the debug build")
    run_parser.add_argument(
        "--args", default="", help="optional args to pass to the program"
    )
    run_parser.set_defaults(func=lambda args: run_build(args.args))

    args = parser.parse_args()
    if not hasattr(args, "func"):
        parser.print_help()
        exit(1)

    if args.func.__code__.co_argcount > 0:
        args.func(args)
    else:
        args.func()


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        exit(0)
