package zen

import "core:fmt"
import "core:mem"
import "core:os"
import "core:path/filepath"
import "core:strings"

VERSION :: "0.0.1-beta"

/* Chaotic mode is obviously false by default */
CHAOTIC :: #config(CHAOTIC, false)

/* Config values set on start, mostly for debugging. */
Config :: struct {
	compile_only:     bool,
	dump_disassembly: bool,
	dump_tokens:      bool,
	trace_exec:       bool,
	check_leaks:      bool,
	stress_gc:        bool,
	log_gc:           bool,
	record_time:      bool,
	repl:             bool,

	/* Path to the running file. */
	__path:           string,

	/* Directory the running file is in. */
	__dirname:        string,
}

config := Config {
	compile_only     = false,
	dump_disassembly = false,
	dump_tokens      = false,
	trace_exec       = false,
	check_leaks      = false,
	stress_gc        = false,
	log_gc           = false,
	record_time      = false,
	repl             = false,
	__path           = "",
	__dirname        = "",
}

/* Fire up a REPL. */
@(private = "file")
repl :: proc(vm: ^VM) -> int {
	vm.name = "REPL"
	vm.path = "REPL"

	when CHAOTIC {
		fmt.print("Welcome to zen!")
		color_red(os.stdout, " (chaotic mode)\n")
	} else {
		fmt.println("Welcome to zen!")
	}

	fmt.println("Press 'Ctrl-D' to exit.")
	buf: [1024]byte

	for i := 1;; i += 1 {
		fmt.printf("zen:%d> ", i)
		n, err := os.read(os.stdin, buf[:])
		if err < 0 {
			fmt.eprintln("Failed to read input")
			return 74
		}

		if n == 1 {continue}
		if n <= 0 {
			fmt.println("\n")
			break
		}

		line := string(buf[:n])

		interpret(vm, vm.gc, line)
	}

	return 0
}

/* Read a file and return it as a string. */
@(private = "file")
read_file :: proc(path: string) -> (string, bool) {
	data, ok := os.read_entire_file_from_filename(path)
	if !ok {
		fmt.printf("Could not open file \"%s\". Does it exist?", path)
		return "", false
	}

	return string(data[:]), true
}

/* DO NOT USE DIRECTLY, USE ImportingModule INSTEAD */
ImportingModuleStruct :: struct {
	path:   string,
	name:   string,
	module: ^ObjModule,
}

/* A module that imports another. It may be nil, so it is set as a union. */
ImportingModule :: union {
	ImportingModuleStruct,
}

/* 
Run a file.
This is not private to the file as it is used in the VM for importing modules.
*/
run_file :: proc(vm: ^VM, path: string, importer: ImportingModule = nil) -> InterpretResult {
	source, ok := read_file(path)
	if !ok {return .INTERPRET_READ_ERROR}
	defer delete(source)

	vm.path = path

	/* Get the name of the file from the path */
	vm.name = filepath.short_stem(path)

	result := interpret(vm, vm.gc, source, importer)

	return result
}

/* Print a help string in `stream`. */
@(private = "file")
print_help :: proc(stream: os.Handle) {
	usage :: `zen <options> <path>`
	options :: `
    -h, -?, --help      Print this help message and exit
    -v, --version       Print version information and exit

    -t, --time          Record time taken to compile and run
    -C, --compile       Compile only, useful with -D
    -D, --dump          Dump disassembled bytecode
    --dump-tokens       Dump tokens from lexer and exit
    -T, --trace         Trace script execution
    -L, --log-gc        Log garbage collection
    -S, --stress-gc     Collect garbage on every allocation
    -c, --check-leaks   Report memory leaks on exit`

	color_green(stream, "zen ")
	fmt.fprintfln(stream, "%s", VERSION)
	fmt.fprintln(stream, "Compiler for the zen programming language.")
	fmt.fprintln(stream)

	color_green(stream, "Usage:")
	fmt.fprintln(stream)
	fmt.fprintln(stream, "    ", usage)
	fmt.fprintln(stream)

	color_green(stream, "Options:")
	fmt.fprintln(stream, options)
}

/* Print the version message in `stream`. */
@(private = "file")
print_version_message :: proc(stream: os.Handle) {
	color_green(stream, "zen ")
	fmt.fprintln(stream, VERSION)
	fmt.fprintln(stream, "written with <3 by pes18fan")
}

/* Parse the arguments passed to the program. */
@(private = "file")
parse_argv :: proc(vm: ^VM) -> (status: int) {
	argc := len(os.args)
	argv := os.args
	argv_i := 0
	argv_0 := argv[0]
	script := ""

	outer: for len(argv) > 1 {
		switch argv[1] {
		case "--":
			{
				argv = argv[1:]
				argc -= 1
				break outer
			}
		case "--version":
			{
				print_version_message(os.stdout)
				return 0
			}
		case "--help":
			{
				print_help(os.stdout)
				return 0
			}
		case "--compile":
			config.compile_only = true
		case "--dump":
			config.dump_disassembly = true
		case "--dump-tokens":
			config.dump_tokens = true
		case "--trace":
			config.trace_exec = true
		case "--time":
			config.record_time = true
		case "--log-gc":
			config.log_gc = true
		case "--stress-gc":
			config.stress_gc = true
		case:
			{
				if argv[1][:2] == "--" {
					fmt.eprintf("Unknown option: %s\n", argv[1])
					print_help(os.stderr)
					return 1
				} else if argv[1][0] == '-' {
					if len(argv[1]) == 1 {
						script = argv[1]
						break outer
					}
					arg := argv[1][1:]
					for c in arg {
						switch c {
						case 'v':
							print_version_message(os.stdout)
							return 0
						case '?', 'h':
							print_help(os.stdout)
							return 0
						case 'C':
							config.compile_only = true
						case 'D':
							config.dump_disassembly = true
						case 't':
							config.record_time = true
						case 'T':
							config.trace_exec = true
						case 'L':
							config.log_gc = true
						case 'S':
							config.stress_gc = true
						case:
							fmt.eprintf("Unknown option: %c", c)
							print_help(os.stderr)
							return 1
						}
					}
				} else {
					script = argv[1]
					break outer
				}
			}
		}
		argv = argv[1:]
		argc -= 1
	}

	argv_0 = argv[0]

	if script == "" {
		config.repl = true
		return repl(vm)
	} else {
		current_dir := os.get_current_directory()
		defer delete(current_dir)

		config.__path = filepath.join([]string{current_dir, script})
		config.__dirname, _ = filepath.split(config.__path)
		defer delete(config.__path)

		result := run_file(vm, script)

		switch result {
		case .INTERPRET_LEX_ERROR:
			return 65
		case .INTERPRET_COMPILE_ERROR:
			return 65
		case .INTERPRET_RUNTIME_ERROR:
			return 70
		case .INTERPRET_READ_ERROR:
			return 74
		case .INTERPRET_OK:
			return 0
		case:
			return 0
		}
	}
}

/* The entry point for the compiler. */
main :: proc() {
	status: int
	defer os.exit(status)

	/* This is to detect memory leaks. Shamelessly stolen from Odin's website lol */
	when ODIN_DEBUG {
		track: mem.Tracking_Allocator
		mem.tracking_allocator_init(&track, context.allocator)
		context.allocator = mem.tracking_allocator(&track)

		defer {
			if len(track.allocation_map) > 0 {
				fmt.eprintf("=== %v allocations not freed: ===\n", len(track.allocation_map))
				for _, entry in track.allocation_map {
					fmt.eprintf("- %v bytes @ %v\n", entry.size, entry.location)
				}
			}
			if len(track.bad_free_array) > 0 {
				fmt.eprintf("=== %v incorrect frees: ===\n", len(track.bad_free_array))
				for entry in track.bad_free_array {
					fmt.eprintf("- %p @ %v\n", entry.memory, entry.location)
				}
			}
		}
	}

	gc := init_gc()
	vm := init_VM()

	gc.mark_roots_arg = &vm
	vm.gc = &gc

	vm.gc.init_string = copy_string(vm.gc, "init")

	init_builtin_modules(&gc)
	init_natives(&gc)

	status = parse_argv(&vm)

	free_VM(&vm)
	free_gc(&gc)
}
