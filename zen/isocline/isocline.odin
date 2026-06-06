package isocline

when ODIN_OS == .Windows {
	foreign import isocline "../../isocline/isocline.lib"
} else {
	foreign import isocline "../../isocline/libisocline.a"
}
import "core:c"

@(default_calling_convention = "c")
foreign isocline {
	ic_readline :: proc(prompt_text: cstring) -> cstring ---
	ic_history_add :: proc(entry: cstring) ---
	ic_set_history :: proc(fname: cstring, max_entries: c.long) ---

	ic_highlight :: proc(henv: rawptr, pos: c.long, count: c.long, style: cstring) ---
	ic_set_default_highlighter :: proc(highlighter: proc "c" (henv: rawptr, input: cstring, arg: rawptr), arg: rawptr) ---

	ic_set_default_completer :: proc(completer: proc "c" (cenv: rawptr, prefix: cstring), arg: rawptr) ---
	ic_add_completions :: proc(cenv: rawptr, prefix: cstring, completions: [^]cstring) -> bool ---
	ic_complete_word :: proc(cenv: rawptr, prefix: cstring, fun: proc "c" (cenv: rawptr, prefix: cstring), is_word_char: rawptr) ---

	ic_free :: proc(ptr: rawptr) ---
}
