package zen

import "core:fmt"
import "core:mem"
import "core:os"
import "core:strings"
import "core:time"

/* Fire up a REPL. */
@(private="file")
repl :: proc (vm: ^VM) -> int {
    fmt.println("zen REPL.")
    fmt.println("Enter '#exit' to exit.")
    buf: [1024]byte

    for {
        fmt.print("> ")
        n, err := os.read(os.stdin, buf[:])
        if err < 0 {
            fmt.eprintln("Failed to read input")
            return 74
        }

        if n == 1 { continue }
        if n <= 0 {
            fmt.println("\n")
            break
        }

        line := string(buf[:n])
        line = strings.trim_right_space(line)

        if line == "#exit" {
            break
        }

        interpret(vm, line)
    }

    return 0
}

/* Read a file and return it as a string. */
@(private="file")
read_file :: proc (path: string) -> (string, bool) {
    data, ok := os.read_entire_file_from_filename(path)
    if !ok {
        fmt.printf("Could not open file \"%s\".", path)
        return "", false
    }
    
    return string(data[:]), true
}

/* Run a file. */
@(private="file")
run_file :: proc (vm: ^VM, path: string) -> int {
    source, ok := read_file(path)
    defer delete(source)
    if !ok { return 74 }
    result := interpret(vm, source)

    if result == .INTERPRET_LEX_ERROR do return 65
    if result == .INTERPRET_COMPILE_ERROR do return 65
    if result == .INTERPRET_RUNTIME_ERROR do return 70

    return 0
}

/* Parse the arguments passed to the program. */
@(private="file")
parse_argv :: proc (vm: ^VM) -> (status: int) {
    help_message ::
`Usage: zen <option or filename>

Options:
    -h, --help      Print this help message and exit
    -v, --version   Print version information and exit`

    if len(os.args) > 2 {
        fmt.println(help_message)
        status = 64
    } else if len(os.args) == 1 {
        status = repl(vm)
    } else {
        switch os.args[1] {
            case "-h", "--help":
                fmt.println(help_message)
                status = 0
            case "-v", "--version":
                fmt.println("zen 0.0.1")
                status = 0
            case:
                status = run_file(vm, os.args[1])
        }
    }

    return status
}

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

    vm := init_VM()

    status = parse_argv(&vm)

    free_VM(&vm)
}
