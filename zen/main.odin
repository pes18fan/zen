package zen

import "core:fmt"
import "core:mem"
import "core:os"
import "core:path/filepath"
import "core:strings"
import ic "isocline"

VERSION :: string(#load("../.zen_version"))

/* Chaotic mode is obviously false by default */
CHAOTIC :: #config(CHAOTIC, false)

when ODIN_DEBUG {
	/* Config values set on start, most are for debugging, but some have use in
    the actual program. */
	Config :: struct {
		compile_only:     bool,
		dump_disassembly: bool,
		dump_tokens:      bool,
		dump_ast:         bool,
		trace_exec:       bool,
		stress_gc:        bool,
		log_type:         bool,
		log_gc:           bool,
		record_time:      bool,
		repl:             bool,
	}
} else {
	Config :: struct {
		record_time: bool,
		repl:        bool,
	}
}

when ODIN_DEBUG {
	config := Config {
		compile_only     = false,
		dump_tokens      = false,
		dump_ast         = false,
		dump_disassembly = false,
		trace_exec       = false,
		stress_gc        = false,
		log_type         = false,
		log_gc           = false,
		record_time      = false,
		repl             = false,
	}
} else {
	config := Config {
		record_time = false,
		repl        = false,
	}
}

/* Fire up a REPL. */
@(private = "file")
repl :: proc(vm: ^VM) -> uint {
	vm.name = "REPL"
	vm.path = "REPL"

	when CHAOTIC {
		fmt.print("Welcome to zen!")
		fmt.println(color_red(" (chaotic mode)"))
	} else {
		fmt.println("Welcome to zen!")
	}

	fmt.println("Press 'Ctrl-D' to exit.")

	// nil means no persistent history file
	ic.ic_set_history(nil, 200)

	for i := 1;; i += 1 {
		prompt := fmt.ctprintf("zen:%d", i)
		raw := ic.ic_readline(prompt)
		if raw == nil {
			fmt.println("\n")
			break
		}

		line_str := strings.clone(string(raw))
		if len(line_str) == 0 {
			delete(line_str)
			ic.ic_free(rawptr(raw))
			continue
		}

		ic.ic_history_add(raw)

		interpret(vm, vm.gc, line_str)
		ic.ic_free(rawptr(raw))
	}

	return 0
}

/* Read a file and return it as a string. */
read_file :: proc(path: string) -> (string, bool) {
	data, err := os.read_entire_file(path, context.allocator)
	if err != nil {
		fmt.printf("Could not open file \"%s\": %s", path, os.error_string(err))
		return "", false
	}

	return string(data[:]), true
}

/* A module that imports another. A module can import nothing if it so chooses,
so ImportingModule is generally passed around as a Maybe type. */
ImportingModule :: struct {
	path:   string,
	name:   string,
	module: ^ObjModule,
}

/* 
Run a file.
This is not private to the file as it is used in the VM for importing modules.
*/
run_file :: proc(
	vm: ^VM,
	path: string,
	importer: Maybe(ImportingModule) = nil,
) -> InterpretResult {
	source, ok := read_file(path)
	if !ok {return .INTERPRET_READ_ERROR}
	// defer delete(source)

	vm.path = path

	/* Get the name of the file from the path */
	vm.name = filepath.short_stem(path)

	return interpret(vm, vm.gc, source, importer)
}

/* Print a help string in `stream`. */
@(private = "file")
print_help :: proc(stream: ^os.File) {
	usage :: `zen <options> <path>`
	when ODIN_DEBUG {
		options :: `
    -h, -?, --help      Print this help message and exit
    -v, --version       Print version information and exit

    -t, --time          Record time taken to compile and run
    -C, --compile       Compile only, useful with -D
    --dump-tokens       Dump tokens from lexer and exit
    --dump-ast          Dump the abstract syntax tree from the parser and exit
    -D, --dump          Dump disassembled bytecode
    -T, --trace         Trace script execution
    -L, --log-gc        Log garbage collection
    -S, --stress-gc     Collect garbage on every allocation
    --log-type          Log type inference`
	} else {
		options :: `
    -h, -?, --help      Print this help message and exit
    -v, --version       Print version information and exit

    -t, --time          Record time taken to compile and run`
	}

	fmt.fprint(stream, color_green("zen "))
	fmt.fprintfln(stream, "%s", VERSION)
	fmt.fprintln(stream, "Interpreter for the zen programming language.")
	fmt.fprintln(stream)

	fmt.fprint(stream, color_green("Usage:"))
	fmt.fprintln(stream)
	fmt.fprintln(stream, "    ", usage)
	fmt.fprintln(stream)

	fmt.fprint(stream, color_green("Options:"))
	fmt.fprintln(stream, options)
}

/* Print the version message in `stream`. */
@(private = "file")
print_version_message :: proc(stream: ^os.File) {
	fmt.fprint(stream, color_green("zen "))
	fmt.fprintln(stream, VERSION)
	fmt.fprintln(stream, "written with <3 by pes18fan")
}

@(private = "file")
set_debug_flag :: proc(flag: string) -> bool {
	when ODIN_DEBUG {
		switch flag {
		case "--compile":
			config.compile_only = true
		case "--dump":
			config.dump_disassembly = true
		case "--dump-tokens":
			config.dump_tokens = true
		case "--dump-ast":
			config.dump_ast = true
		case "--trace":
			config.trace_exec = true
		case "--time":
			config.record_time = true
		case "--log-type":
			config.log_type = true
		case "--log-gc":
			config.log_gc = true
		case "--stress-gc":
			config.stress_gc = true
		case:
			fmt.eprintf("Unknown option: %s\n", flag)
			print_help(os.stderr)
			return false
		}

		return true
	} else {
		fmt.eprintf("Unknown option: %s\n", flag)
		print_help(os.stderr)
		return false
	}
}

@(private = "file")
set_debug_flag_short :: proc(flag: rune) -> bool {
	when ODIN_DEBUG {
		switch flag {
		case 'C':
			config.compile_only = true
		case 'D':
			config.dump_disassembly = true
		case 'T':
			config.trace_exec = true
		case 'L':
			config.log_gc = true
		case 'S':
			config.stress_gc = true
		case:
			fmt.eprintf("Unknown option: %c\n", flag)
			print_help(os.stderr)
			return false
		}

		return true
	} else {
		fmt.eprintf("Unknown option: %c\n", flag)
		print_help(os.stderr)
		return false
	}
}

/* Parse the arguments passed to the program. */
@(private = "file")
parse_argv :: proc(vm: ^VM) -> (status: uint) {
	argc := len(os.args)
	argv := os.args
	script := ""
	args_passed := false

	outer: for len(argv) > 1 {
		switch argv[1] {
		case "--":
			{
				argv = argv[2:] /* Skip both the current arg and the "--" */
				argc -= 1
				args_passed = true
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
		case "--compile",
		     "--dump",
		     "--dump-tokens",
		     "--dump-ast",
		     "--trace",
		     "--stress-gc",
		     "--log-gc",
		     "--log-type":
			ok := set_debug_flag(argv[1])
			if !ok {return 1}
		case "--time":
			config.record_time = true
		case:
			{
				if argv[1][0] == '-' {
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
						case 'C', 'D', 'T', 'L', 'S':
							ok := set_debug_flag_short(c)
							if !ok {return 1}
						case 't':
							config.record_time = true
						case:
							fmt.eprintf("Unknown option: %c\n", c)
							print_help(os.stderr)
							return 1
						}
					}
				} else if argv[1][:2] == "--" {
					fmt.eprintf("Unknown option: %s\n", argv[1])
					print_help(os.stderr)
					return 1
				} else {
					script = argv[1]
					argv = argv[1:]
					continue outer
				}
			}
		}
		argv = argv[1:]
		argc -= 1
	}

	/* Create a ObjList for the args. Don't worry about freeing it, GC will handle it */
	args_list := new_list(vm.gc)
	for i in 0 ..< len(argv) {
		if args_passed {
			write_value_array(&args_list.items, obj_val(copy_string(vm.gc, argv[i])))
		}
	}
	vm.args = args_list

	if script == "" {
		info, stat_err := os.fstat(os.stdin, context.allocator)
		if stat_err != nil {
			fmt.eprintfln("Failed to check stdin status: %s", os.error_string(stat_err))
		}
		defer delete(info.fullpath)

		if info.type == .Named_Pipe {
			buf: [1024]byte
			n, err := os.read(os.stdin, buf[:])
			if err != nil {
				if err == .EOF {
					return 0
				}
				fmt.eprintfln("Failed to read from stdin: %s", os.error_string(err))
			}

			zen_update_path("Piped input")
			vm.name = "Piped input"
			vm.path = "Piped input"
			res := interpret(vm, vm.gc, string(buf[:n]))
			return interpret_result_exit_code(res)
		} else {
			config.repl = true
			return repl(vm)
		}
	} else {
		current_dir, wkdir_err := os.get_working_directory(context.temp_allocator)
		if wkdir_err != nil {
			fmt.eprintfln("Failed to get working directory: %s", os.error_string(wkdir_err))
			return 1
		}

		path, join_err := filepath.join([]string{current_dir, script}, context.temp_allocator)
		if join_err != nil {
			fmt.eprintfln("Failed to get file path: %s", os.error_string(join_err))
		}
		zen_update_path(path)

		dirname, _ := filepath.split(path)
		zen_update_dirname(dirname)

		result := run_file(vm, script)
		return interpret_result_exit_code(result)
	}
}

interpret_result_exit_code :: proc(result: InterpretResult) -> uint {
	switch result {
	case .INTERPRET_LEX_ERROR:
		return 65
	case .INTERPRET_PARSE_ERROR:
		return 65
	case .INTERPRET_COMPILE_ERROR:
		return 65
	case .INTERPRET_RUNTIME_ERROR:
		return 70
	case .INTERPRET_READ_ERROR:
		return 74
	case .INTERPRET_VOLUNTARY_EXIT:
		return zen_get_exit_code()
	case .INTERPRET_OK:
		return 0
	case:
		return 0
	}
}

internal_compiler_error :: proc(prefix, message: string, loc := #caller_location) -> ! {
	fmt.eprintln(color_red("Internal compiler error!"))
	fmt.eprint(prefix)
	if message != "" {
		fmt.eprintf(": %v", message)
	}
	fmt.eprintln()
	fmt.eprintfln("    in procedure %v", loc.procedure)
	fmt.eprintfln("    at position %v:%v in file %v", loc.line, loc.column, loc.file_path)
	fmt.eprintln()
	fmt.eprintln("Please report this error by opening an issue on https://github.com/pes18fan/zen")
	os.exit(1)
}

/* The entry point for the compiler. */
main :: proc() {
	context.assertion_failure_proc = internal_compiler_error

	status: int
	defer os.exit(status)

	// need to add this otherwise -vet would complain on release builds
	_ = mem.Allocator

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

	// free all temp allocator (arena) allocations (like in tprintf)
	defer free_all(context.temp_allocator)

	gc := init_gc()
	defer free_gc(&gc)
	vm := init_VM()
	defer free_VM(&vm)

	gc.mark_roots_arg = &vm
	vm.gc = &gc

	vm.gc.init_string = copy_string(vm.gc, "init")

	init_natives(&gc)

	status = int(parse_argv(&vm))
}
