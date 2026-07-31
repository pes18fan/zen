#!/usr/bin/env python3
import argparse
import subprocess
import sys
import os
import shutil
import platform

OC = "odin"
ProcError = subprocess.CalledProcessError
DEBUG_FLAGS = "-vet -vet-tabs -strict-style -vet-style -warnings-as-errors -disallow-do -debug"

# NOTE: If -o:aggressive shows weird behavior switch to -o:speed
RELEASE_FLAGS = "-vet -vet-tabs -strict-style -vet-style -warnings-as-errors -disallow-do -o:aggressive -microarch:native"
CHAOTIC_FLAGS = f"{RELEASE_FLAGS} -define:CHAOTIC=true"
TARGET = "zen"

OUT = "zen"
DBG_OUT = "dzen"

match platform.system():
    case "Windows":
        OUT = "zen.exe"
        DBG_OUT = "dzen.exe"
    case "Linux", "Darwin":
        pass


def setup_isocline() -> None:
    repo = "https://github.com/daanx/isocline.git"
    root = "isocline"
    inc = os.path.join(root, "include")
    src = os.path.join(root, "src")

    lib_name = "isocline.lib" if platform.system() == "Windows" else "libisocline.a"
    lib_path = os.path.join(root, lib_name)

    if os.path.exists(lib_path):
        return

    if not os.path.isdir(root):
        print("Downloading isocline...")
        subprocess.run(["git", "clone", "--depth",
                       "1", repo, root], check=True)

    if not (os.path.isdir(inc) and os.path.isdir(src)):
        raise RuntimeError("isocline repo is missing /include or /src")

    source = os.path.join(src, "isocline.c")

    print("Compiling isocline...")
    if platform.system() == "Windows":
        cc = shutil.which("cl")
        if cc:
            objs = []
            obj = os.path.join(root, os.path.splitext(
                os.path.basename(source))[0] + ".obj")
            subprocess.run([cc, "/nologo", "/O2", "/c",
                           source, f"/Fo{obj}"], check=True)
            objs.append(obj)
            subprocess.run(
                ["lib.exe", "/nologo", f"/OUT:{lib_path}"] + objs, check=True)
            return

        cc = shutil.which("clang") or shutil.which("gcc")
        if not cc:
            raise RuntimeError("No C compiler found for isocline build")

        objs = []
        obj = os.path.join(root, os.path.splitext(
            os.path.basename(source))[0] + ".o")
        subprocess.run([cc, "-O2", "-c", source, "-o", obj], check=True)
        objs.append(obj)

        ar = shutil.which("ar")
        if not ar:
            raise RuntimeError("No archiver found for isocline build")
        subprocess.run([ar, "rcs", lib_path] + objs, check=True)
        return

    cc = shutil.which("cc") or shutil.which("clang") or shutil.which("gcc")
    if not cc:
        raise RuntimeError("No C compiler found for isocline build")

    objs = []
    obj = os.path.join(root, os.path.splitext(
        os.path.basename(source))[0] + ".o")
    subprocess.run([cc, "-O2", "-c", source, "-o", obj], check=True)
    objs.append(obj)

    ar = shutil.which("ar")
    if not ar:
        raise RuntimeError("No archiver found for isocline build")
    subprocess.run([ar, "rcs", lib_path] + objs, check=True)
    return


def create_debug_build():
    try:
        setup_isocline()
        print("Compiling the debug build..")

        os.makedirs("bin/dbg", exist_ok=True)
        subprocess.run(
            f"{OC} build {TARGET} -out:bin/dbg/{DBG_OUT} {DEBUG_FLAGS}".split(), check=True
        )
        print("Debug build compiled!")
    except ProcError as e:
        print(f"Error while creating debug build: {e}", file=sys.stderr)
        exit(1)


def create_release_build():
    try:
        setup_isocline()
        print("Compiling in release mode..")

        os.makedirs("bin/rel", exist_ok=True)
        subprocess.run(
            f"{OC} build {TARGET} -out:bin/rel/{OUT} {RELEASE_FLAGS}".split(), check=True
        )
        print("Release build compiled!")
    except ProcError as e:
        print(f"Error while creating release build: {e}", file=sys.stderr)
        exit(1)


def create_chaotic_build():
    try:
        setup_isocline()
        print("Compiling in chaotic mode..")

        os.makedirs("bin/chaotic", exist_ok=True)
        subprocess.run(
            f"{OC} build {
                TARGET} -out:bin/chaotic/{OUT} {RELEASE_FLAGS} -define:CHAOTIC=true".split(),
            check=True,
        )
        print("Chaotic build compiled!")
    except ProcError as e:
        print(f"Error when creating chaotic build: {e}", file=sys.stderr)
        exit(1)


def test(recompile: bool, unit_only: bool = False, e2e_only: bool = False, strict: bool = False):
    if recompile:
        create_debug_build()
        os.makedirs("bin/test", exist_ok=True)
        shutil.copy(f"bin/dbg/{DBG_OUT}", f"bin/test/{OUT}")

    if not e2e_only:
        print("Running unit tests:")
        try:
            subprocess.run(f"{OC} test {TARGET}".split(), check=True)
        except ProcError as e:
            print(f"Error when running unit tests: {e}", file=sys.stderr)
            exit(1)

    if unit_only:
        return

    print("")

    print("Running e2e test suite:")
    args = [
        "python", "./run_tests.py", "-d", "__tests__/"]
    if strict:
        args.append("--strict")
    try:
        subprocess.run(args, cwd="test/", check=True)
    except ProcError as e:
        print(f"Error during e2e tests: {e}", file=sys.stderr)
        exit(1)


def benchmark(recompile: bool):
    if recompile:
        create_release_build()

    print("Starting up the benchmark runner..")
    try:
        subprocess.run("python ./run_benchmarks.py".split(),
                       cwd="test/", check=True)
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
            subprocess.run(f"{OC} doc {TARGET}".split(),
                           stdout=doc_file, check=True)
        except ProcError as e:
            print(f"Error when generating docs: {e}", file=sys.stderr)
            exit(1)

    print("docs generated at doc/docs.txt")


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
        print("You need Odin to build!")
        exit(1)

    parser = argparse.ArgumentParser(description="zen build system")
    subparsers = parser.add_subparsers(dest="command")

    # debug build
    dbg_parser = subparsers.add_parser("dbg", help="create a debug build")
    dbg_parser.set_defaults(func=create_debug_build)

    # release build
    rel_parser = subparsers.add_parser("rel", help="create a release build")
    rel_parser.set_defaults(func=create_release_build)

    # chaotic build
    chaotic_parser = subparsers.add_parser(
        "chaotic", help="create a chaotic build (a build with weird extra features)"
    )
    chaotic_parser.set_defaults(func=create_chaotic_build)

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
    test_parser.add_argument(
        "--recompile", action="store_true", help="recompile the compiler before testing"
    )
    test_parser.add_argument(
        "--strict", "-s", action="store_true",
        help="fail on memory leaks"
    )
    test_parser.add_argument(
        "--unit", "-u", action="store_true",
        help="only run unit tests"
    )
    test_parser.add_argument(
        "--e2e", "-e", action="store_true",
        help="only run end-to-end tests"
    )
    test_parser.set_defaults(
        func=lambda args: test(args.recompile, args.unit, args.e2e, args.strict))

    # benchmark
    bench_parser = subparsers.add_parser("bench", help="run benchmarks")
    bench_parser.add_argument(
        "--recompile", action="store_true", help="recompile the compiler before benchmarking"
    )
    bench_parser.set_defaults(func=lambda args: benchmark(args.recompile))

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
