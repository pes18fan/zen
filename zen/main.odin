package zen

import "core:flags"
import "core:fmt"
import "core:mem"
import "core:os"
import "core:path/filepath"
import ic "isocline"

_ :: mem

VERSION :: string(#load("../.zen_version"))

/* Chaotic mode is obviously false by default */
CHAOTIC :: #config(CHAOTIC, false)

/* Fire up a REPL. */
repl :: proc(vm: ^VM) -> int {
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
		defer ic.ic_free(rawptr(raw))

		line_str := string(raw)
		if len(line_str) == 0 {
			continue
		}

		ic.ic_history_add(raw)

		interpret(vm, vm.gc, line_str, importer = nil)
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
	defer delete(source)

	vm.path = path

	/* Get the name of the file from the path */
	vm.name = filepath.short_stem(path)

	return interpret(vm, vm.gc, source, importer)
}

run_with_opts :: proc(opt: ^Options) -> int {
	gc := init_gc()
	defer free_gc(&gc)
	vm := init_VM()
	defer free_VM(&vm)

	gc.mark_roots_arg = &vm
	vm.gc = &gc

	init_natives(&gc)

	// There are four ways to execute code via the zen binary, prioritized in
	// the following order:
	// - Via the `--exec` command line flag, where you can give a string of zen
	//      code which is directly sent to the interpreter.
	// - By providing a path to a file on disk, which is read and interpreted
	//      as zen code.
	// - By piping a string into the binary; the string is also directly sent
	//      to the interpreter as zen code.
	// - Via the REPL; it is opened by simply invoking the zen binary with no
	//      arguments.

	// First check if an `--exec` string was provided, if so directly interpret
	if opt.exec != "" {
		zen_update_path("Command-line input")
		vm.name = "Command-line input"
		vm.path = "Command-line input"
		res := interpret(&vm, vm.gc, opt.exec)
		return interpret_result_exit_code(res)
	}

	// Check if a filepath was provided; if so start the work on running it
	if opt.script != "" {
		current_dir, wkdir_err := os.get_working_directory(context.temp_allocator)
		if wkdir_err != nil {
			fmt.eprintfln("Failed to get working directory: %s", os.error_string(wkdir_err))
			return 1
		}

		path, join_err := filepath.join([]string{current_dir, opt.script}, context.temp_allocator)
		if join_err != nil {
			fmt.eprintfln("Failed to get file path: %s", os.error_string(join_err))
		}
		zen_update_path(path)

		dirname, _ := filepath.split(path)
		zen_update_dirname(dirname)

		result := run_file(&vm, opt.script)
		return interpret_result_exit_code(result)
	}

	// Now check stdin and see if something was piped in; if so directly interpret
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
		res := interpret(&vm, vm.gc, string(buf[:n]))
		return interpret_result_exit_code(res)
	}

	// None of the above cases were true: no `--exec`, no script arg, and no
	// piped input; so run the REPL
	return repl(&vm)
}

// Exit codes roughly based on the codes defined in Unix's `sysexits.h`, each
// of the ones used mean the following:
// 65 -> Input data was incorrect in some way
// 66 -> An input file did not exist or was unreadable
// 70 -> An internal software error occured
interpret_result_exit_code :: #force_inline proc(result: InterpretResult) -> int {
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
		return 66
	case .INTERPRET_VOLUNTARY_EXIT:
		return zen_get_exit_code()
	case .INTERPRET_OK:
		return 0
	}

	fmt.panicf("invalid InterpretResult value %v", result)
}

@(cold)
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

opt: Options

/* Template for command-line args. */
Options :: struct {
	script:      string `args:"pos=0" usage:"Input script, omit to use REPL instead."`,
	exec:        string `usage:"A string of zen code to directly execute."`,
	compile:     bool `usage:"Compile only, useful with -dump."`,
	dump:        bool `usage:"Dump disasembled bytecode."`,
	dump_tokens: bool `usage:"Dump tokens from lexer and exit."`,
	dump_ast:    bool `usage:"Dump the abstract syntax tree from the parser and exit."`,
	trace:       bool `usage:"Trace script execution."`,
	stress_gc:   bool `usage:"Run the garbage collector on every allocation."`,
	log_gc:      bool `usage:"Log garbage collection."`,
	log_checker: bool `usage:"Log the type checker."`,
	time:        bool `usage:"Record time taken to run various stages of the compiler."`,
	version:     bool `usage:"Print version information and exit."`,
	overflow:    [dynamic]string `usage:"Arguments to the program."`,
}

in_repl :: proc() -> bool {
	return opt.script == "" && opt.exec == ""
}

main :: proc() {
	// Parse cmdline flags first
	flags.parse_or_exit(&opt, os.args, .Unix)
	defer delete(opt.overflow)

	if opt.version {
		fmt.print(color_green("zen "))
		fmt.println(VERSION)
		fmt.println("written with <3 by pes18fan")
		return
	}

	// Setup custom panic
	context.assertion_failure_proc = internal_compiler_error

	// Turn on the tracking allocator on debug mode
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

	// Free all temp allocator (arena) allocations (like in tprintf)
	defer free_all(context.temp_allocator)

	status := run_with_opts(&opt)
	if status != 0 {os.exit(status)}
}
