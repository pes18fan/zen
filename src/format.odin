package zen

import "core:fmt"
import "core:os"
import "core:sys/windows"

color_red :: proc(text: string, stream: os.Handle) {
	when ODIN_OS == .Windows {
		switch stream {
		case os.stdout:
			windows.SetConsoleTextAttribute(
				windows.GetStdHandle(windows.STD_OUTPUT_HANDLE),
				windows.FOREGROUND_RED,
			)
		case os.stderr:
			windows.SetConsoleTextAttribute(
				windows.GetStdHandle(windows.STD_ERROR_HANDLE),
				windows.FOREGROUND_RED,
			)
		}
	} else {
		switch stream {
		case os.stdout:
			fmt.print("\x1b[31m")
		case os.stderr:
			fmt.eprint("\x1b[31m")
		}
	}

	fmt.fprint(stream, text)

	color_reset(stream)
}

/* 
Reset the text color in the terminal.
This does not need to be called manually, all the coloring functions call it in
the end.
*/
color_reset :: proc(stream: os.Handle) {
	when ODIN_OS == .Windows {
		switch stream {
		case os.stdout:
			windows.SetConsoleTextAttribute(
				windows.GetStdHandle(windows.STD_OUTPUT_HANDLE),
				windows.FOREGROUND_RED | windows.FOREGROUND_GREEN | windows.FOREGROUND_BLUE,
			)
		case os.stderr:
			windows.SetConsoleTextAttribute(
				windows.GetStdHandle(windows.STD_ERROR_HANDLE),
				windows.FOREGROUND_RED | windows.FOREGROUND_GREEN | windows.FOREGROUND_BLUE,
			)
		}
	} else {
		switch stream {
		case os.stdout:
			fmt.print("\x1b[0m")
		case os.stderr:
			fmt.eprint("\x1b[0m")
		}
	}
}
