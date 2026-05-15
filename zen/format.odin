package zen

import "core:fmt"
import "core:os"
import "core:terminal"

/* Print text in red in the specific `stream`. */
color_red :: proc(stream: ^os.File, text: string) {
	if !terminal.color_enabled {
		fmt.fprint(stream, text)
		return
	}

	fmt.fprint(stream, "\x1b[31m")
	fmt.fprint(stream, text)
	color_reset(stream)
}

/* Print text in green in the specific `stream`. */
color_green :: proc(stream: ^os.File, text: string) {
	if !terminal.color_enabled {
		fmt.fprint(stream, text)
		return
	}

	fmt.fprint(stream, "\x1b[32m")
	fmt.fprint(stream, text)
	color_reset(stream)
}

/* Print text in yellow in the specific `stream`. */
color_yellow :: proc(stream: ^os.File, text: string) {
	if !terminal.color_enabled {
		fmt.fprint(stream, text)
		return
	}

	fmt.fprint(stream, "\x1b[33m")
	fmt.fprint(stream, text)
	color_reset(stream)
}

/* 
Reset the text color in the terminal.
This does not need to be called manually, all the coloring functions call it in
the end.
*/
@(private = "file")
color_reset :: proc(stream: ^os.File) {
	fmt.fprint(stream, "\x1b[0m")
}
