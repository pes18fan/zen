package isocline

when ODIN_OS == .Windows {
	foreign import isocline "../../isocline/isocline.lib"
} else {
	foreign import isocline "../../isocline/libisocline.a"
}
import "core:c"

ic_highlight_env_t :: struct {}
ic_highlight_fun_t :: #type proc "c" (henv: ^ic_highlight_env_t, arg: ..rawptr)

@(default_calling_convention = "c")
foreign isocline {
	ic_readline :: proc(prompt_text: cstring) -> cstring ---
	ic_history_add :: proc(entry: cstring) ---
	ic_set_history :: proc(fname: cstring, max_entries: c.long) ---

	ic_highlight :: proc(henv: ^ic_highlight_env_t, pos: c.long, #by_ptr style: c.char) ---
	ic_set_default_highlighter :: proc(highlighter: ^ic_highlight_fun_t, arg: rawptr) ---

	ic_free :: proc(ptr: rawptr) ---
}
