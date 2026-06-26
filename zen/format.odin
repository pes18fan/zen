package zen

import "core:fmt"
import "core:terminal"

/* Returns text in red. */
color_red :: proc(text: string) -> string {
	if !terminal.color_enabled {
		return text
	}

	return fmt.tprintf("\x1b[31m%s\x1b[0m", text)
}

/* Returns text in green. */
color_green :: proc(text: string) -> string {
	if !terminal.color_enabled {
		return text
	}

	return fmt.tprintf("\x1b[32m%s\x1b[0m", text)
}

/* Returns text in yellow. */
color_yellow :: proc(text: string) -> string {
	if !terminal.color_enabled {
		return text
	}

	return fmt.tprintf("\x1b[33m%s\x1b[0m", text)
}

/* Returns text styled in bold. */
style_bold :: proc(text: string) -> string {
	if !terminal.color_enabled {
		return text
	}

	return fmt.tprintf("\x1b[1m%s\x1b[0m", text)
}
