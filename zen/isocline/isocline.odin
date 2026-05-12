package isocline

when ODIN_OS == .Windows do foreign import isocline "../../isocline/isocline.lib"
when ODIN_OS == .Linux do foreign import isocline "../../isocline/libisocline.a"
import "core:c"

@(default_calling_convention = "c")
foreign isocline {
	ic_readline :: proc(prompt_text: cstring) -> cstring ---
	ic_history_add :: proc(entry: cstring) ---
	ic_set_history :: proc(fname: cstring, max_entries: c.long) ---
	ic_free :: proc(ptr: rawptr) ---
}
