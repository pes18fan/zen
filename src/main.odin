package zen

import "core:fmt"
import "core:mem"
import "core:os"
import "core:strings"

VERSION :: "zen 0.0.1"

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
}

config := Config {
	compile_only = false,
	dump_disassembly = false,
	dump_tokens = false,
	trace_exec  = false,
	check_leaks = false,
	stress_gc   = false,
	log_gc      = false,
	record_time = false,
	repl        = false,
}

/* Fire up a REPL. */
@(private = "file")
repl :: proc(vm: ^VM) -> int {
	fmt.println("Welcome to zen!")
	fmt.println("Press 'Ctrl-D' to exit.")
	buf: [1024]byte
	i := 1

	for {
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
		i += 1
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

/* Run a file. */
@(private = "file")
run_file :: proc(vm: ^VM, path: string) -> int {
	source, ok := read_file(path)
	defer delete(source)
	if !ok {return 74}
	result := interpret(vm, vm.gc, source)

	if result == .INTERPRET_LEX_ERROR do return 65
	if result == .INTERPRET_COMPILE_ERROR do return 65
	if result == .INTERPRET_RUNTIME_ERROR do return 70

	return 0
}

/* Parse the arguments passed to the program. */
@(private = "file")
parse_argv :: proc(vm: ^VM) -> (status: int) {
	argc := len(os.args)
	argv := os.args
	argv_i := 0
	argv_0 := argv[0]
	script := ""

	help_message :: `Usage: zen <options> <path>

Options:
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
	
	version_message :: VERSION + "\n" + "written with <3 by pes18fan"

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
				fmt.println(version_message)
				return 0
			}
		case "--help":
			{
				fmt.println(help_message)
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
		case "--check-leaks":
			config.check_leaks = true
		case "--log-gc":
			config.log_gc = true
		case "--stress-gc":
			config.stress_gc = true
		case:
			{
				if argv[1][:2] == "--" {
					fmt.eprintf("Unknown option: %s\n", argv[1])
					fmt.eprintln(help_message)
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
							fmt.println(version_message)
							return 0
						case '?', 'h':
							fmt.println(help_message)
							return 0
						case 'C':
							config.compile_only = true
						case 'D':
							config.dump_disassembly = true
						case 't':
							config.record_time = true
						case 'c':
							config.check_leaks = true
						case 'T':
							config.trace_exec = true
						case 'L':
							config.log_gc = true
						case 'S':
							config.stress_gc = true
						case:
							fmt.eprintf("Unknown option: %c", c)
							fmt.eprintln(help_message)
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
		return run_file(vm, script)
	}
}

main :: proc() {
	status: int
	defer os.exit(status)

	/* This is to detect memory leaks. Shamelessly stolen from Odin's website lol */
	if config.check_leaks {
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

	init_natives(&gc)

	status = parse_argv(&vm)

	free_VM(&vm)
	free_gc(&gc)
}
